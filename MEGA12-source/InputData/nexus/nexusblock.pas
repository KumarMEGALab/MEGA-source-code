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

unit nexusblock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nexustokens, KeywordConsts;

const
  SITES_PER_SEGMENT = 10;
  SITES_PER_LINE = 80;

type
  TNexusBlockType = (nbtTaxa, nbtData, nbtCharacters, nbtTree, nbtNotes, nbtUnaligned, nbtAssumptions,
                     nbtCodons, nbtSets, nbtDistances, nbtPrivate);

  { TNexusBlock }

  TNexusBlock = class(TObject)
    private
      FEnforceUppercaseCommands: Boolean;
      procedure SetEnforceUppercaseCommands(AValue: Boolean);

    protected
      FTokens: TList; { the full token list}
      FCleanTokens: TList; { the full list minus all tokens that are part of comments}
      FBlockType: TNexusBlockType;
      FLink: String;
      FTitle: String;
      GetBlockType: TNexusBlockType;
      function GetName: String;
      procedure SetLink(AValue: String);
      procedure SetTitle(AValue: String);
      function NextOccurrence(const Start: Integer; const Pattern: String; UsingCleanList: Boolean = True):Integer; { searches for the next occurrence of Pattern, beginning at Start (exclusive). If not found, returns -1}
      procedure RaiseParseError(aToken: TNexusToken; aMsg: String);
      function ConstructQuotedLabel(const StartIndex: Integer; const EndIndex: Integer; var QuotedLabel: String; UsingCleanList: Boolean = True): Boolean;
    public
      ErrMsg: String;
      ErrLine: Integer;
      ErrCol: Integer;
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure AddToken(AToken: TNexusToken);
      procedure AddTokens(AList: TList);
      procedure UpdateCleanList; { reset FCleanTokens}
      function HasToken(TokenPattern: String; UsingCleanList: Boolean = True): Boolean; overload;
      function HasToken(const TokenPattern: String; var Index: Integer; UsingCleanList: Boolean = True): Boolean; overload;
      function GetToken(TokenPattern: String): TNexusToken;
      function TokenAt(Index: Integer; UsingCleanList: Boolean = True): TNexusToken; { typically, the clean list, which has comments removed should be used}
      function Validate: Boolean; virtual; { iterate the block's tokens and validate that Nexus format is respected}
      function Write: TStringList; virtual; abstract;
      function DebugWriteTokens: TStringList; virtual;
      function DebugWriteData: TStringList; virtual;
      function BlockTypeString: String;
      property Name: String read GetName;
      property BlockType: TNexusBlockType read FBlockType write GetBlockType;
      property Title: String read FTitle write SetTitle;
      property Link: String read FLink write SetLink;
      property EnforceUppercaseCommands: Boolean read FEnforceUppercaseCommands write SetEnforceUppercaseCommands;
  end;

  { TTaxaBlock }

  TTaxaBlock = class(TNexusBlock)
    private
      FNumTaxa: Integer;
      FTaxaLabels: TStringList;
      function ParseTaxaLabel(var Index: Integer; var aLabel: String): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function Write: TStringList; override;
      function Validate: Boolean; override;
      function ParseDimension: Boolean;
      function ParseNumTaxa(Index: Integer): Boolean;
      function ParseTaxaLabels: Boolean;
      property TaxaLabels: TStringList read FTaxaLabels;
      property NumTaxa: Integer read FNumTaxa;
  end;

  { TCharactersBlock }

  TCharactersBlock = class(TNexusBlock)
    protected
      FContainsCodingNuc: Boolean;
      FDataType: TSnTokenCode;
      FGapSymbol: Char;
      FIdenticalSymbol: Char;
      FIsInterleaved: Boolean;
      FMissingSymbol: Char;
      FNumSites: Integer;
      FHasNewTaxa: Boolean;
      FLabels, FSeqData: TStringList;
      FNumTaxa: Integer;
      function SetGapSymbol(AValue: Char): Boolean;
      function SetIdenticalSymbol(AValue: Char): Boolean;
      function SetMissingSymbol(AValue: Char): Boolean;
      function InitSeqList(Labels: TStringList; SeqData: TStringList): Boolean;

      function ParseDimension: Boolean;
      function ParseMatrixInterleaved: Boolean;
      function ParseMatrixNonInterleaved: Boolean;

      function ParseFormatCommand: Boolean;
      function ParseDataType(Index: Integer): Boolean;
      function ParseNumChars(Index: Integer): Boolean;
      function ParseNumTaxa(Index: Integer): Boolean;
      function ParseMissingSymbol(Index: Integer): Boolean;
      function ParseGapSymbol(Index: Integer): Boolean;
      function ParseMatchChar(Index: Integer): Boolean;
      function SetDataType(aStr: String): Boolean;
      function GetSourceFilename: String;
      procedure FormatSequenceBlock(const aSequence: String; var aList: TStringList);
      procedure FormatLabelsForMegaFile;
    public
      function DebugWriteData: TStringList; override;
      constructor Create;
      destructor Destroy; override;
      function Write: TStringList; override;
      function GetMegaSequenceAlignmentFileStrings: TStringList;
      function Validate: Boolean; override;
      function NumSequences: Integer;
      property NumSites: Integer read FNumSites;
      property NumTaxa: Integer read FNumTaxa;
      property DataType: TSnTokenCode read FDataType;
      property ContainsCodingNuc: Boolean read FContainsCodingNuc; { done this way instead of with TSnTokenCode for compatibility}
      property MissingSymbol: Char read FMissingSymbol;
      property GapSymbol: Char read FGapSymbol;
      property IdenticalSymbol: Char read FIdenticalSymbol;
      property IsInterleaved: Boolean read FIsInterleaved;
      property Labels: TStringList read FLabels;
      property SeqData: TStringList read FSeqData;

  end;

  { TDataBlock }

  TDataBlock = class(TCharactersBlock)
    private

    protected

    public
      destructor Destroy; override;
      function Validate: Boolean; override;
  end;

  { TTreeBlock }

  TTreeBlock = class(TNexusBlock)
  private
    FHasTranslation: Boolean;
    FLabels: TStringList;
    function GetLabels: TStringList;
    function GetNumTrees: Integer;
    function GetTranslation: TStringList;
    function ParseTranslation: Boolean;
  public
    constructor Create;
    function Write: TStringList; override;
    function DebugWriteData: TStringList; override;
    function Validate: Boolean; override;
    function GetNewickString(const Index: Integer; var NewickString: String): Boolean; { Index is zero-based}
    function GetTreeNames: TStringList;
    property NumTrees: Integer read GetNumTrees;
    property HasTranslation: Boolean read FHasTranslation;
    property Translation: TStringList read GetTranslation;
    property Labels: TStringList read GetLabels;
  end;

  { TNotesBlock }

  TNotesBlock = class(TNexusBlock)
  public
    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TAssumptionsBlock }

  TAssumptionsBlock = class(TNexusBlock)
  public
    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TUnalignedBlock }

  TUnalignedBlock = class(TNexusBlock)
  public
    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TCodonsBlock }

  TCodonsBlock = class(TNexusBlock)
  public
    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TSetsBlock }

  TSetsBlock = class(TNexusBlock)
  public
    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TDistancesBlock }

  TDistancesBlock = class(TNexusBlock)
  public
    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TPrivateBlock }

  TPrivateBlock = class(TNexusBlock)
  public

    constructor Create;
    function Write: TStringList; override;
    function Validate: Boolean; override;
  end;

  { TNexusBlockFactory }

  TNexusBlockFactory = class(TObject)
    public
      constructor Create;
      destructor Destroy; override;
      function StringToBlockType(AStr: String): TNexusBlockType;
      function CreateBlock(AType: TNexusBlockType): TNexusBlock;
  end;

implementation

uses
  MegaConsts{$IFNDEF VISUAL_BUILD}, MD_MegaMain{$ELSE}, mega_main{$ENDIF};

{ TDataBlock }

destructor TDataBlock.Destroy;
begin
  inherited Destroy;
end;

function TDataBlock.Validate: Boolean;
begin
  Result := inherited Validate;
  if Trim(ErrMsg) = 'missing CHARACTERS command' then
    ErrMsg := EmptyStr;
  if not (FTokens.Count > 7) then
  begin
    ErrMsg := 'Characters block requires at least 8 tokens';
    Exit;
  end;
  if not (TokenAt(1).Pattern = 'DATA') then
    ErrMsg := ErrMsg + LineEnding + 'missing DATA command';

  ErrMsg := Trim(ErrMsg);
  Result := (ErrMsg = EmptyStr);
end;

{ TPrivateBlock }

constructor TPrivateBlock.Create;
begin
  inherited Create;
  FBlockType := nbtPrivate;
end;

function TPrivateBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TPrivateBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TDistancesBlock }

constructor TDistancesBlock.Create;
begin
  inherited Create;
  FBlockType := nbtDistances;
end;

function TDistancesBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TDistancesBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TSetsBlock }

constructor TSetsBlock.Create;
begin
  inherited Create;
  FBlockType := nbtSets;
end;

function TSetsBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TSetsBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TCodonsBlock }

constructor TCodonsBlock.Create;
begin
  inherited Create;
  FBlockType := nbtCodons;
end;

function TCodonsBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TCodonsBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TUnalignedBlock }

constructor TUnalignedBlock.Create;
begin
  inherited Create;
  FBlockType := nbtUnaligned;
end;

function TUnalignedBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TUnalignedBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TAssumptionsBlock }

constructor TAssumptionsBlock.Create;
begin
  inherited Create;
  FBlockType := nbtAssumptions;
end;

function TAssumptionsBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TAssumptionsBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TNotesBlock }

constructor TNotesBlock.Create;
begin
  inherited Create;
  FBlockType := nbtNotes;
end;

function TNotesBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TNotesBlock.Validate: Boolean;
begin
  Result := True;
end;

{ TTreeBlock }

function TTreeBlock.GetNumTrees: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FCleanTokens.Count > 0 then
  begin
    for i := 0 to FCleanTokens.Count - 1 do
      if TNexusToken(FCleanTokens[i]).Pattern = 'TREE' then
        inc(Result);
  end;
end;

function TTreeBlock.GetLabels: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Assigned(FLabels) and (FLabels.Count > 0) then
    for i := 0 to FLabels.Count - 1 do
      Result.Add(FLabels.Names[i]);
end;

function TTreeBlock.GetTranslation: TStringList;
var
  i: Integer;
  temp: String;
begin
  Result := TStringList.Create;
  if Assigned(FLabels) and (FLabels.Count > 0) then
    for i := 0 to FLabels.Count - 1 do
    begin
      temp := FLabels.ValueFromIndex[i];
      while (temp[1] = #39) and (temp[Length(temp)] = #39) do
        temp := copy(temp, 2, Length(temp) - 2);
      temp := StringReplace(temp, ' ', '_', [rfReplaceAll]);
      Result.Add(FLabels.Names[i] + '=' + temp);
    end;
end;

function TTreeBlock.ParseTranslation: Boolean;
var
  Index: Integer;
  Start: Integer = -1;
  Finish: Integer = -1;
  NextOcc: Integer;
  aLbl, aTaxon, aComma: TNexusToken;
  LblStr: String = '';
  TaxonStr: String = '';
begin
  Result := False;
  if not HasToken('TRANSLATE', Start) then
    Exit;
  if not Assigned(FLabels) then
    FLabels := TStringList.Create
  else
    FLabels.Clear;
  Index := Start + 1;
  Finish := NextOccurrence(Start, ';', True);
  if Finish < Start then
    raise Exception.Create('Expected a semicolon to terminate the TRANSLATE command but it was not found');
  repeat
  begin
    aLbl := TokenAt(Index);
    if aLbl.NexusTokenType = nttSnglQuote then
    begin
      NextOcc := NextOccurrence(Index, #39);
      if NextOcc <= Index then
        RaiseParseError(aLbl, 'malformed label in TRANSLATE command');
      if not ConstructQuotedLabel(Index, NextOcc, LblStr, True) then
        RaiseParseError(aLbl, 'Oh no! Failed to parse quoted label in TRANSLATE command');
      Index := NextOcc;
    end
    else
      LblStr := aLbl.Pattern;
    inc(Index);
    aTaxon := TokenAt(Index);
    if aTaxon.NexusTokenType = nttSnglQuote then
    begin
      NextOcc := NextOccurrence(Index, #39);
      if NextOcc <= Index then
        RaiseParseError(aLbl, 'malformed label in TRANSLATE command');
      if not ConstructQuotedLabel(Index, NextOcc, TaxonStr, True) then
        RaiseParseError(aLbl, 'Oh no! Failed to parse quoted label in TRANSLATE command');
      Index := NextOcc;
    end
    else
      TaxonStr := aTaxon.Pattern;
    inc(Index);
    aComma := TokenAt(Index);

    if (not (aComma.NexusTokenType = nttComma)) and (Index < Finish) then
      raise Exception.Create('Expected a comma but found ' + #34 + aComma.Pattern + #34 + ' when parsing the TRANSLATE statement- row: ' + IntToStr(aComma.Line) + ' column: ' + IntToStr(aComma.Column));
    FLabels.Add(LblStr + '=' + TaxonStr);
    inc(Index);
  end;
  until Index >= Finish ;
  if Index > (Finish + 1) then
    raise Exception.Create('Expected label/taxon pairs in the TRANSLATE command but an odd number of tokens were found');
  Result := True;
end;

constructor TTreeBlock.Create;
begin
  inherited Create;
  FBlockType := nbtTree;
  FLabels := nil;
end;

function TTreeBlock.Write: TStringList;
var
  i: Integer;
  aToken: TNexusToken;
begin
  Result := TStringList.Create;
  if FTokens.Count > 0 then
    for i := 0 to FTokens.Count - 1 do
    begin
      aToken := TNexusToken(FTokens[i]);
      Result.Add(aToken.AsString);
    end;
end;

function TTreeBlock.DebugWriteData: TStringList;
var
  i: Integer;
  ANewick: String = '';
begin
  Result:=inherited DebugWriteData;
  Result.Add(Format('%-40s = %s', ['HAS_TRANSLATION', BoolToStr(HasTranslation, True)]));
  Result.Add(Format('%-40s = %s', ['NUM_TREES', IntToStr(GetNumTrees)]));
  if HasTranslation then
    for i := 0 to FLabels.Count - 1 do
    Result.Add(Format('%-40s = %s', [IntToStr(i), FLabels[i]]));
  for i := 0 to GetNumTrees - 1 do
  begin
    if GetNewickString(i, ANewick) then
      Result.Add(Format('%-40s = %s', ['NEWICK-' + IntToStr(i), ANewick]))
    else
      Result.Add(Format('%-40s = %s', ['NEWICK-' + IntToStr(i), 'FAILED TO RETRIEVE NEWICK STRING']));
  end;
end;

function TTreeBlock.Validate: Boolean;
begin
  Result := inherited Validate;
  if not (FTokens.Count > 7) then
  begin
    ErrMsg := 'Tree block requires at least 8 tokens';
    Exit;
  end;
  if not (TokenAt(1).Pattern = 'TREES') then
    ErrMsg := ErrMsg + LineEnding + 'missing TREES command';
  if not (TokenAt(2).Pattern = ';') then
    ErrMsg := ErrMsg + LineEnding + 'missing semicolon';
  if not HasToken('TREE') then
    ErrMsg := ErrMsg + LineEnding + 'missing TREE command';
  ErrMsg := Trim(ErrMsg);
  try
    FHasTranslation := ParseTranslation;
  except
    on E:Exception do
      ErrMsg := ErrMsg + LineEnding + E.Message;
  end;
  Result := (ErrMsg = EmptyStr);
end;

function TTreeBlock.GetNewickString(const Index: Integer; var NewickString: String): Boolean;
var
  i: Integer;
  TempStr: String;
  QuotedLbl: String = '';
  NextOcc: Integer;
  aToken: TNexusToken;
  TreeCount: Integer;
begin
  Result := False;
  TempStr := EmptyStr;

  if not Validate then { the call to validate will update the clean list}
    Exit;
  i := 0;
  TreeCount := 0;
  aToken := TokenAt(i);

  { first find the correct TREE command for the given index}
  while ((i < FCleanTokens.Count) and (TreeCount < (Index + 1)) {and (aToken.Pattern <> 'TREE')}) do
  begin
    aToken := TokenAt(i);
    if aToken.Pattern = 'TREE' then
      inc(TreeCount);
    inc(i);
  end;

  { move past the tree name (which may have multiple tokens if quoted)to the equals sign}
  while (aToken.NexusTokenType <> nttEquals) and (i < FCleanTokens.Count) do
  begin
    aToken := TokenAt(i);
    inc(i);
  end;

  { everthing from here to the next ';' belongs to the newick string}
  while (i < FCleanTokens.Count) and (aToken.NexusTokenType <> nttSemicolon) do
  begin
    aToken := TokenAt(i);
    if aToken.NexusTokenType = nttSnglQuote then
    begin
      NextOcc := NextOccurrence(i, #39, True);
      if NextOcc <= i then
        RaiseParseError(aToken, 'malformed quoted label in Newick string of NEXUS file');
      ConstructQuotedLabel(i, NextOcc, QuotedLbl, True);
      i := NextOcc;
      TempStr := TempStr + QuotedLbl;
    end
    else
      TempStr := TempStr + aToken.Pattern;
    inc(i);
  end;
  TempStr := Trim(TempStr);
  Result := (TempStr <> EmptyStr);
  if Result then
    NewickString := TempStr;
end;

function TTreeBlock.GetTreeNames: TStringList;
var
  aName: String;
  NextOcc: Integer;
  Index: Integer;
  aToken: TNexusToken;
begin
  Result := TStringList.Create;
  if FCleanTokens.Count > 0 then
  begin
    Index := 0;
    while Index < FCleanTokens.Count - 1 do
    begin
      if TokenAt(Index).Pattern = 'TREE' then
      begin
        AName := EmptyStr;
        inc(Index);
        if TokenAt(Index).NexusTokenType = nttSnglQuote then
        begin
          NextOcc := NextOccurrence(Index, #39, True);
          if not ConstructQuotedLabel(Index, NextOcc, aName) then
            RaiseParseError(TokenAt(Index), 'failed to parse TREE command');
        end
        else
        begin
          aToken := TokenAt(Index);
          while (aToken.NexusTokenType <> nttEquals) and (Index < FCleanTokens.Count) do
          begin
            aToken := TokenAt(Index);
            if aToken.NexusTokenType <> nttEquals then
              aName := Trim(aName + ' ' + aToken.Pattern);
            inc(Index);
          end;
        end;
        Result.Add(aName);
      end
      else
        inc(Index);
    end;
  end;
end;

{ TCharactersBlock }

function TCharactersBlock.SetGapSymbol(AValue: Char): Boolean;
const
  BadSymbols = '()[]{}/\,;:=**<>^' + #34 + #39;
begin
  Result := True;
  if Pos(AValue, BadSymbols) > 0 then
    Result := False
  else
    FGapSymbol:=AValue;
end;

function TCharactersBlock.SetIdenticalSymbol(AValue: Char): Boolean;
const
  BadSymbols = '()[]{}/\,;:=*<>^' + #34 + #39;
begin
  Result := True;
  if Pos(AValue, BadSymbols) > 0 then
    Result := False
  else
    FIdenticalSymbol:=AValue;
end;

function TCharactersBlock.SetMissingSymbol(AValue: Char): Boolean;
const
  BadSymbols = '()[]{}/\,;:=**<>^' + #34 + #39;
begin
  Result := True;
  if Pos(AValue, BadSymbols) > 0 then
    Result := False
  else
    FMissingSymbol:=AValue;
end;

function TCharactersBlock.InitSeqList(Labels: TStringList; SeqData: TStringList): Boolean;
begin
  Assert((Labels.Count > 0) and (Labels.Count = SeqData.Count));
  FLabels.Clear;
  FLabels.AddStrings(Labels);
  FSeqData.Clear;
  FSeqData.AddStrings(SeqData);
  FNumTaxa := FLabels.Count;
  Result := (FLabels.Count > 0) and (FLabels.Count = FSeqData.Count);
end;

function TCharactersBlock.ParseDimension: Boolean;
var
  Index: Integer = -1;
  aToken: TNexusToken;
  Finish: Integer;
begin
  Result := False;
  if not HasToken('DIMENSIONS', Index) then
    raise Exception.Create('missing required DIMENSIONS command');
  aToken := TokenAt(Index, True);
  Finish := NextOccurrence(Index, ';', True);
  if Finish <= Index then
    RaiseParseError(aToken, 'invalid DIMENSIONS command, did not find a terminating semicolon');
  while (Index <= Finish) do
  begin
    inc(Index);
    aToken := TokenAt(Index);
    if aToken.Pattern = 'NEWTAXA' then
      FHasNewTaxa := True
    else if aToken.Pattern = 'NCHAR' then
      ParseNumChars(Index)
    else if aToken.Pattern = 'NTAX' then
      ParseNumTaxa(Index);
  end;
  if FNumSites <= 0 then
    raise Exception.Create('failed to parse the required NCHAR command.');
  Result := True;
end;

function TCharactersBlock.ParseMatrixInterleaved: Boolean;
var
  Index: Integer = -1;
  Finish: Integer;
  NextOcc: Integer;
  TempLabels, TempSeqData: TStringList;
  LabelToken, DataToken: TNexusToken;
  aLabel: String;
  aData: String;
  AlreadyAdded: Boolean;
  column, numSpaces, i: Integer;
begin
  AlreadyAdded := False;
  TempLabels := nil;
  TempSeqData := nil;
  Result := False;
  if not HasToken('MATRIX', Index) then
    raise Exception.Create('missing required MATRIX command');
  Finish := NextOccurrence(Index, ';');
  if Finish <= Index then
    raise Exception.Create('failed to parse the MATRIX command, missing semicolon not found');

  try
    try
      TempLabels := TStringList.Create;
      TempSeqData := TStringList.Create;
      inc(Index);
      while Index < Finish do
      begin
        LabelToken := TokenAt(Index);
        if LabelToken.NexusTokenType = nttSnglQuote then { quoted FLabels may contain spaces and so consist of multiple tokens}
        begin
          aLabel := LabelToken.Pattern;
          column := LabelToken.Column;
          NextOcc := NextOccurrence(Index, #39);
          if NextOcc <= Index then
            RaiseParseError(LabelToken, 'malformed quoted label');
          while Index < NextOcc do
          begin
            inc(Index);
            LabelToken := TokenAt(Index);
            numSpaces := (LabelToken.Column - column - 1);
            if numSpaces >= 1 then
              for i := 1 to numSpaces do
                aLabel := aLabel + ' ';
            aLabel := aLabel + LabelToken.Pattern;
            column := LabelToken.Column + LabelToken.PatternLength - 1;
          end;
        end { the label should be a single token}
        else
          aLabel := LabelToken.Pattern;
        if TempLabels.IndexOf(aLabel) < 0 then { does not yet exist in the list}
        begin
          TempLabels.Add(LabelToken.Pattern);
          AlreadyAdded := False;
        end
        else
          AlreadyAdded := True;

        inc(Index);
        DataToken := TokenAt(Index);
        aData := DataToken.Pattern;
        if DataToken.Line = LabelToken.Line then
        begin
          while DataToken.Line = LabelToken.Line do  { to handle the case where users have inserted a blank space every N sites}
          begin
            inc(Index);
            DataToken := TokenAt(Index);
            if DataToken.Line = LabelToken.Line then
              aData := aData + DataToken.Pattern;
          end;
        end
        else
          inc(Index);
        if AlreadyAdded then
          TempSeqData[TempLabels.IndexOf(aLabel)] := TempSeqData[TempLabels.IndexOf(aLabel)] + aData
        else
          TempSeqData.Add(aData);
      end;
      if not (TempLabels.Count = TempSeqData.Count) then
        raise Exception.Create('malformed MATRIX command, the number of labels does not match the number of sequences');
      InitSeqList(TempLabels, TempSeqData);
    except
      on E:Exception do
        raise Exception.Create('error when parsing the MATRIX command: ' + E.Message);
    end;
  finally
    if Assigned(TempLabels) then
      TempLabels.Free;
    if Assigned(TempSeqData) then
      TempSeqData.Free;
  end;
end;

function TCharactersBlock.ParseMatrixNonInterleaved: Boolean;
var
  Index: Integer = -1;
  Finish: Integer;
  NextOcc: Integer;
  TempLabels, TempSeqData: TStringList;
  LabelToken, DataToken: TNexusToken;
  aLabel: String;
  aData: String;
  column: Integer;
  numSpaces, i: Integer;
begin
  TempLabels := nil;
  TempSeqData := nil;
  Result := False;
  if not HasToken('MATRIX', Index) then
    raise Exception.Create('missing required MATRIX command');
  Finish := NextOccurrence(Index, ';');
  if Finish <= Index then
    raise Exception.Create('failed to parse the MATRIX command, missing semicolon not found');

  try
    try
      TempLabels := TStringList.Create;
      TempSeqData := TStringList.Create;
      inc(Index);
      while Index < Finish do
      begin
        LabelToken := TokenAt(Index);
        if LabelToken.NexusTokenType = nttSnglQuote then { quoted FLabels may contain spaces and so consist of multiple tokens}
        begin
          aLabel := LabelToken.Pattern;
          column := LabelToken.Column;
          NextOcc := NextOccurrence(Index, #39);
          if NextOcc <= Index then
            RaiseParseError(LabelToken, 'malformed quoted label');
          while Index < NextOcc do
          begin
            inc(Index);
            LabelToken := TokenAt(Index);
            numSpaces := (LabelToken.Column - column - 1);
            if numSpaces >= 1 then
              for i := 1 to numSpaces do
                aLabel := aLabel + ' ';
            aLabel := aLabel + LabelToken.Pattern;
            column := LabelToken.Column + LabelToken.PatternLength - 1;
          end;
        end { the label should be a single token}
        else
          aLabel := LabelToken.Pattern;
        TempLabels.Add(aLabel);

        inc(Index);
        DataToken := TokenAt(Index);
        aData := DataToken.Pattern;
        if DataToken.Line = LabelToken.Line then
        begin
          while DataToken.Line = LabelToken.Line do  { to handle the case where users have inserted a blank space every N sites}
          begin
            inc(Index);
            DataToken := TokenAt(Index);
            if DataToken.Line = LabelToken.Line then
              aData := aData + DataToken.Pattern;
          end;
        end
        else
          inc(Index);
        TempSeqData.Add(aData);
      end;
      if not (TempLabels.Count = TempSeqData.Count) then
        raise Exception.Create('malformed MATRIX command, the number of labels does not match the number of sequences');
      InitSeqList(TempLabels, TempSeqData);
    except
      on E:Exception do
        raise Exception.Create('error when parsing the MATRIX command: ' + E.Message);
    end;
  finally
    if Assigned(TempLabels) then
      TempLabels.Free;
    if Assigned(TempSeqData) then
      TempSeqData.Free;
  end;
end;

function TCharactersBlock.ParseFormatCommand: Boolean;
var
  Index: Integer = -1;
  Finish: Integer;
  aToken: TNexusToken;
begin
  Result := False;
  if not HasToken('FORMAT', Index) then
    raise Exception.Create('[BUG] call to non-existent ParseFormatCommand');
  Finish := NextOccurrence(Index, ';', True);
  if Finish <=  Index then
    raise Exception.Create('failed to parse the FORMAT command - terminating semicolon not found');
  while Index <= Finish do
  begin
    inc(Index);
    aToken := TokenAt(Index, True);
    if FEnforceUppercaseCommands then
    begin
      if aToken.Pattern = 'DATATYPE' then
        ParseDataType(Index)
      else if (aToken.Pattern = 'MISSING') then
        ParseMissingSymbol(Index)
      else if (aToken.Pattern = 'GAP') then
        ParseGapSymbol(Index)
      else if (aToken.Pattern = 'MATCHCHAR') then
        ParseMatchChar(Index)
      else if (aToken.Pattern = 'INTERLEAVE') then
        FIsInterleaved := True
      else if (aToken.Pattern = 'TRANSPOSE') then
        RaiseParseError(aToken, 'the TRANSPOSE command is not supported');
    end
    else
    begin
      if aToken.HasSameText('DATATYPE') then
        ParseDataType(Index)
      else if aToken.HasSameText('MISSING') then
        ParseMissingSymbol(Index)
      else if aToken.HasSameText('GAP') then
        ParseGapSymbol(Index)
      else if aToken.HasSameText('MATCHCHAR') then
        ParseMatchChar(Index)
      else if aToken.HasSameText('INTERLEAVE') then
        FIsInterleaved := True
      else if aToken.HasSameText('TRANSPOSE') then
        RaiseParseError(aToken, 'the TRANSPOSE command is not supported');
    end;
  end;
  Result := True;
end;

function TCharactersBlock.ParseDataType(Index: Integer): Boolean;
var
  dtype, equalsSign, value: TNexusToken;
begin
  Result := False;
  dtype := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    RaiseParseError(dtype, 'failed to parse the DATATYPE command');
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not SetDataType(value.Pattern) then
    RaiseParseError(value, 'unsupported DATATYPE (must be one of (DNA, RNA, NUCLEOTIDE, PROTEIN, STANDARD) but found ' + #34 + value.Pattern + #34);
  Result := True;
end;

function TCharactersBlock.ParseNumChars(Index: Integer): Boolean;
var
  nChars, equalsSign, value: TNexusToken;
  nsites: Integer;
begin
  Result := False;
  nChars := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    raise Exception.Create('failed to parse the NCHARS command - row: ' + IntToStr(nChars.Line) + ' column: ' + IntToStr(nChars.Column));
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not TryStrToInt(value.Pattern, nsites) then
    RaiseParseError(value, 'Expected an intEger value but found ' + #34 + value.Pattern + #34)
  else
    FNumSites := nsites;
  Result := True;
end;

function TCharactersBlock.ParseNumTaxa(Index: Integer): Boolean;
var
  nTaxa, equalsSign, value: TNexusToken;
  ntax: Integer;
begin
  Result := False;
  nTaxa := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    RaiseParseError(nTaxa, 'failed to parse the NTAXA command');
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not TryStrToInt(value.Pattern, ntax) then
    RaiseParseError(value, 'Expected an intEger value but found ' + #34 + value.Pattern + #34)
  else
    FNumTaxa := ntax;
  Result := True;
end;

function TCharactersBlock.ParseMissingSymbol(Index: Integer): Boolean;
var
  key, equalsSign, value: TNexusToken;
begin
  Result := False;
  key := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    RaiseParseError(key, 'failed to parse the MISSING command');
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not SetMissingSymbol(value.Pattern[1]) then
    RaiseParseError(value, 'unsupported MISSING symbol - found ' + #34 + value.Pattern + #34);
  Result := True;
end;

function TCharactersBlock.ParseGapSymbol(Index: Integer): Boolean;
var
  key, equalsSign, value: TNexusToken;
begin
  Result := False;
  key := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    RaiseParseError(key, 'failed to parse the GAP command');
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not SetGapSymbol(value.Pattern[1]) then
    RaiseParseError(value, 'unsupported GAP symbol - found ' + #34 + value.Pattern + #34);
  Result := True;
end;

function TCharactersBlock.ParseMatchChar(Index: Integer): Boolean;
var
  key, equalsSign, value: TNexusToken;
begin
  Result := False;
  key := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    RaiseParseError(key, 'failed to parse the MATCHCHAR command');
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not SetIdenticalSymbol(value.Pattern[1]) then
    RaiseParseError(value, 'unsupported MATCHCHAR symbol - found ' + #34 + value.Pattern + #34);
  Result := True;
end;

function TCharactersBlock.SetDataType(aStr: String): Boolean;
begin
  Result := True;
  if Uppercase(aStr) = 'DNA' then
    FDataType := snNucleotide
  else if Uppercase(aStr) = 'RNA' then
    FDataType := snNucleotide
  else if Uppercase(aStr) = 'PROTEIN' then
    FDataType := snProtein
  else if Uppercase(aStr) = 'NUCLEOTIDE' then
    FDataType := snNucleotide
  else if Uppercase(aStr) = 'STANDARD' then
    FDataType := snNoToken
  else
    Result := False;
end;

function TCharactersBlock.GetSourceFilename: String;
begin
  {$IFNDEF VISUAL_BUILD}
  Result := ExtractFilename(D_MegaMain.DataFileName);
  {$ELSE}
  Result := MegaForm.DataFileNameOnly;
  {$ENDIF}
end;

procedure TCharactersBlock.FormatSequenceBlock(const aSequence: String; var aList: TStringList);
var
  tempStr: String;
  i: Integer;
  aNumSites: Integer;
begin
  aList.Clear;
  if Length(aSequence) > 0 then
  begin
    i := 1;
    aNumSites := 0;
    tempStr := EmptyStr;
    while i <= Length(aSequence) do
    begin
      while (aNumSites < SITES_PER_LINE) and (i <= Length(aSequence)) do
      begin
        tempStr := tempStr + aSequence[i];
        inc(i);
        inc(aNumSites);
        if (aNumSites mod SITES_PER_SEGMENT) = 0 then
          tempStr := tempStr + ' ';
      end;
      aList.Add(tempStr);
      aNumSites := 0;
      tempStr := EmptyStr;
    end;
  end;
end;

procedure TCharactersBlock.FormatLabelsForMegaFile;
var
  i: Integer;
  temp: String;
begin
  if FLabels.Count > 0 then
    for i := 0 to FLabels.Count - 1 do
    begin
      temp := FLabels[i];
      if (temp[1] = #39) and (temp[Length(temp)] = #39) then
        temp := copy(temp, 2, Length(temp) - 2);
      temp := StringReplace(temp, ' ', '_', [rfReplaceAll]);
      FLabels[i] := temp;
    end;
end;

function TCharactersBlock.DebugWriteData: TStringList;
var
  i: Integer;
begin
  Result := inherited DebugWriteData;
  Result.Add(Format('%-40s = %s', ['FContainsCodingNuc', BoolToStr(FContainsCodingNuc, True)]));
  Result.Add(Format('%-40s = %s', ['FDataType', TokenCodeString(FDataType)]));
  Result.Add(Format('%-40s = %s', ['FGapSymbol', FGapSymbol]));
  Result.Add(Format('%-40s = %s', ['FIdenticalSymbol', FIdenticalSymbol]));
  Result.Add(Format('%-40s = %s', ['FIsInterleaved', BoolToStr(FIsInterleaved, True)]));
  Result.Add(Format('%-40s = %s', ['FMissingSymbol', FMissingSymbol]));
  Result.Add(Format('%-40s = %s', ['FNumSites', IntToStr(FNumSites)]));
  Result.Add(Format('%-40s = %s', ['FHasNewTaxa', BoolToStr(FHasNewTaxa, True)]));
  if FLabels.Count > 0 then
    for i := 0 to FLabels.Count - 1 do
      Result.Add(Format('%-40s %s', [FLabels[i], FSeqData[i]]));
end;

constructor TCharactersBlock.Create;
begin
  inherited Create;
  FBlockType := nbtCharacters;
  FHasNewTaxa := False;
  FNumSites := -1;
  FNumTaxa := -1;
  FMissingSymbol := '?';
  FGapSymbol := '-';
  FIdenticalSymbol := '.';
  FIsInterleaved := False;
  FLabels := TStringList.Create;
  FSeqData := TStringList.Create;
end;

destructor TCharactersBlock.Destroy;
begin
  if Assigned(FLabels) then
    FLabels.Free;
  if Assigned(FSeqData) then
    FSeqData.Free;
  inherited Destroy;
end;

function TCharactersBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TCharactersBlock.GetMegaSequenceAlignmentFileStrings: TStringList;
var
  i: Integer;
  formattedStrings: TStringList;
begin
  formattedStrings := nil;
  Result := TStringList.Create;
  try
    formattedStrings := TStringList.Create;
    FormatLabelsForMegaFile;
    Result.Add('#mega');
    Result.Add(Format('!TITLE %s;', [Title]));
    Result.Add(EmptyStr);
    Result.Add(Format('!FORMAT DataType=%s Indel=%s Identical=%s Missing=%s;', [TokenCodeString(FDataType), FGapSymbol, FIdenticalSymbol, FMissingSymbol]));
    Result.Add(Format('!DESCRIPTION - Converted from %s NEXUS file;', [GetSourceFilename]));
    for i := 0 to FNumTaxa - 1 do
    begin
      Result.Add(EmptyStr);
      Result.Add(Format('#%s', [FLabels[i]]));
      FormatSequenceBlock(FSeqData[i], formattedStrings);
      Result.AddStrings(formattedStrings);
    end;
  finally
    if Assigned(formattedStrings) then
      formattedStrings.Free;
  end;
end;

function TCharactersBlock.Validate: Boolean;
begin
  Result := inherited Validate;
  if not (FTokens.Count > 7) then
  begin
    ErrMsg := 'Characters block requires at least 8 tokens';
    Exit;
  end;
  if not (TokenAt(1).Pattern = 'CHARACTERS') then
    ErrMsg := ErrMsg + LineEnding + 'missing CHARACTERS command';
  if not (TokenAt(2).Pattern = ';') then
    ErrMsg := ErrMsg + LineEnding + 'missing semicolon';
  if not HasToken('MATRIX') then
    ErrMsg := ErrMsg + LineEnding + 'missing MATRIX command';
  if not HasToken('DIMENSIONS') then
    ErrMsg := ErrMsg + LineEnding + 'missing DIMENSIONS command';
  if not HasToken('NCHAR') then
    ErrMsg := ErrMsg + LineEnding + 'missing NCHAR command';
  try
    ParseDimension;
    if HasToken('FORMAT') then
      ParseFormatCommand;
    if FIsInterleaved then
      ParseMatrixInterleaved
    else
      ParseMatrixNonInterleaved;
  except
    on E:Exception do
      ErrMsg := Trim(ErrMsg + LineEnding + E.Message);
  end;

  ErrMsg := Trim(ErrMsg);
  Result := (ErrMsg = EmptyStr);
end;

function TCharactersBlock.NumSequences: Integer;
begin
  Result := FSeqData.Count;
end;

{ TTaxaBlock }

function TTaxaBlock.ParseTaxaLabel(var Index: Integer; var aLabel: String): Boolean;
var
  aToken: TNexusToken;
  done: Boolean;
  column, NextOcc, numSpaces, i: Integer;
begin
  done := False;
  aToken := nil;
  Result := False;
  aLabel := EmptyStr;
  while (Index < FCleanTokens.Count) and (not done) do
  begin
    aToken := TokenAt(Index, True);
    if aToken.NexusTokenType = nttSnglQuote then
    begin
      aLabel := aToken.Pattern;
      while True do
      begin
        inc(Index);
        aToken := TokenAt(Index, True);
        aLabel := aLabel + aToken.Pattern;

        column := aToken.Column + aToken.PatternLength - 1;
        NextOcc := NextOccurrence(Index, #39);
        if NextOcc <= Index then
          RaiseParseError(aToken, 'malformed quoted label');
        while Index < NextOcc do
        begin
          inc(Index);
          aToken := TokenAt(Index);
          numSpaces := (aToken.Column - column - 1);
          if numSpaces >= 1 then
            for i := 1 to numSpaces do
              aLabel := aLabel + ' ';
          aLabel := aLabel + aToken.Pattern;
          column := aToken.Column + aToken.PatternLength - 1;
        end;

        if aToken.NexusTokenType = nttSnglQuote then
        begin
          done := True;
          inc(Index);
          break;
        end;
      end;
    end
    else
    begin
      //lineNum := aToken.Line;
      inc(Index);
      aLabel := aToken.Pattern;
      done := True;
      //while True do
      //begin
      //  inc(Index);
      //  aToken := TokenAt(Index, True);
      //  if aToken.Line = lineNum then
      //  begin
      //    aLabel := aLabel + aToken.Pattern;
      //  end
      //  else
      //  begin
      //    done := True;
      //    break;
      //  end;
      //end;
    end;
  end;
  Result := done;
end;

constructor TTaxaBlock.Create;
begin
  inherited Create;
  FTaxaLabels := TStringList.Create;
  FBlockType := nbtTaxa;
end;

destructor TTaxaBlock.Destroy;
begin
  if Assigned(FTaxaLabels) then
    FTaxaLabels.Free;
  inherited Destroy;
end;

function TTaxaBlock.Write: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TTaxaBlock.Validate: Boolean;
begin
  Result := inherited Validate;
  if not (FTokens.Count > 4) then
  begin
    ErrMsg := 'Taxa block requires at least 5 tokens';
    Exit;
  end;
  if not (TokenAt(1).Pattern = 'TAXA') then
    ErrMsg := ErrMsg + LineEnding + 'missing TAXA command';
  if not (TokenAt(2).Pattern = ';') then
    ErrMsg := ErrMsg + LineEnding + 'missing semicolon';
  if not HasToken('TAXLABELS') then
    ErrMsg := ErrMsg + LineEnding + 'missing TAXLABELS command';
  if not HasToken('DIMENSIONS') then
    ErrMsg := ErrMsg + LineEnding + 'missing DIMENSIONS command';
  if not HasToken('NTAX') then
    ErrMsg := ErrMsg + LineEnding + 'missing NTAX command';
  try
    ParseDimension;
    ParseTaxaLabels;
  except
    on E:Exception do
      ErrMsg := Trim(ErrMsg + LineEnding + E.Message);
  end;

  ErrMsg := Trim(ErrMsg);
  Result := (ErrMsg = EmptyStr);
  Result := True;
end;

function TTaxaBlock.ParseDimension: Boolean;
var
  Index: Integer = -1;
  aToken: TNexusToken;
  Finish: Integer;
begin
  Result := False;
  if not HasToken('DIMENSIONS', Index) then
    raise Exception.Create('missing required DIMENSIONS command');
  aToken := TokenAt(Index, True);
  Finish := NextOccurrence(Index, ';', True);
  if Finish <= Index then
    RaiseParseError(aToken, 'invalid DIMENSIONS command, did not find a terminating semicolon');
  while (Index <= Finish) do
  begin
    inc(Index);
    aToken := TokenAt(Index);
    if aToken.Pattern = 'NTAX' then
      if not ParseNumTaxa(Index) then
        raise Exception.Create('failed to parse the required NTAX command');
  end;
  if FNumTaxa <= 0 then
    raise Exception.Create('failed to parse the required NTAX command');
  Result := True;
end;

function TTaxaBlock.ParseNumTaxa(Index: Integer): Boolean;
var
  nTaxa, equalsSign, value: TNexusToken;
  ntax: Integer;
begin
  Result := False;
  nTaxa := TokenAt(Index, True);
  if (Index + 2) >= FCleanTokens.Count then
    RaiseParseError(nTaxa, 'failed to parse the NTAX command');
  equalsSign := TokenAt(Index + 1);
  if not (equalsSign.NexusTokenType = nttEquals) then
    RaiseParseError(equalsSign, 'expected an ' + #34 + '=' + #34 + ' character but found ' + #34 + equalsSign.Pattern + #34);
  value := TokenAt(Index + 2);
  if not TryStrToInt(value.Pattern, ntax) then
    RaiseParseError(value, 'Expected an integer value but found ' + #34 + value.Pattern + #34)
  else
    FNumTaxa := ntax;
  Result := True;
end;

function TTaxaBlock.ParseTaxaLabels: Boolean;
var
  Index: Integer = -1;
  aToken: TNexusToken;
  Finish: Integer;
  aLabel: String;
begin
  aLabel := EmptyStr;
  if not HasToken('TAXLABELS', Index) then
    raise Exception.Create('missing required TAXLABELS command');
  aToken := TokenAt(Index, True);
  Finish := NextOccurrence(Index, ';', True);
  if Finish <= Index then
    RaiseParseError(aToken, 'invalid TAXLABELS command, did not find a terminating semicolon');
  inc(Index);
  while (Index < Finish) do
  begin
    if not ParseTaxaLabel(Index, aLabel) then
      raise Exception.Create('failed to parse the NTAX command');
    FTaxaLabels.Add(aLabel);
  end;
  if FTaxaLabels.Count <> FNumTaxa then
    raise Exception.Create('failed to parse the NTAX command');
  Result := True;
end;

{ TNexusBlockFactory }

constructor TNexusBlockFactory.Create;
begin

end;

destructor TNexusBlockFactory.Destroy;
begin
  inherited Destroy;
end;

function TNexusBlockFactory.StringToBlockType(AStr: String): TNexusBlockType;
begin
  if AStr = 'TAXA' then
    Result := nbtTaxa
  else if ASTr = 'CHARACTERS' then
    Result := nbtCharacters
  else if AStr = 'DATA' then
    Result := nbtData
  else if ASTr = 'TREES' then
    Result := nbtTree
  else if ASTr = 'NOTES' then
    Result := nbtNotes
  else if ASTr = 'UNALIGNED' then
    Result := nbtUnaligned
  else if ASTr = 'ASSUMPTIONS' then
    Result := nbtAssumptions
  else if ASTr = 'CODONS' then
    Result := nbtCodons
  else if ASTr = 'SETS' then
    Result := nbtSets
  else if ASTr = 'DISTANCES' then
    Result := nbtDistances
  else
    Result := nbtPrivate;
end;

function TNexusBlockFactory.CreateBlock(AType: TNexusBlockType): TNexusBlock;
begin
  case AType of
    nbtTaxa: Result := TTaxaBlock.Create;
    nbtCharacters: Result := TCharactersBlock.Create;
    nbtData: Result := TDataBlock.Create;
    nbtTree: Result := TTreeBlock.Create;
    nbtNotes: Result := TNotesBlock.Create;
    nbtUnaligned: Result := TUnalignedBlock.Create;
    nbtAssumptions: Result := TAssumptionsBlock.Create;
    nbtCodons: Result := TCodonsBlock.Create;
    nbtSets: Result := TSetsBlock.Create;
    nbtDistances: Result := TDistancesBlock.Create;
    nbtPrivate: Result := TPrivateBlock.Create;
    else
      raise Exception.Create('Unknown Nexus command block type');
  end;
end;

{ TNexusBlock }

procedure TNexusBlock.SetEnforceUppercaseCommands(AValue: Boolean);
begin
  if FEnforceUppercaseCommands=AValue then Exit;
  FEnforceUppercaseCommands:=AValue;
end;

function TNexusBlock.GetName: String;
begin
  Result := EmptyStr;
  Assert(False, 'not implemented');
end;

procedure TNexusBlock.SetLink(AValue: String);
begin
  if FLink=AValue then Exit;
  FLink:=AValue;
end;

procedure TNexusBlock.SetTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

function TNexusBlock.NextOccurrence(const Start: Integer; const Pattern: String; UsingCleanList: Boolean = True): Integer;
var
  i: Integer;
begin
  Result := -1;
  if UsingCleanList then
  begin
    if Start < (FCleanTokens.Count - 1) then
    begin
      for i := (Start + 1) to (FCleanTokens.Count - 1) do
      begin
        if TNexusToken(FCleanTokens[i]).Pattern = Pattern then
        begin
          Result := i;
          Exit;
        end;
      end;
    end;
  end
  else
  begin
    if Start < (FTokens.Count - 1) then
    begin
      for i := (Start + 1) to (FTokens.Count - 1) do
      begin
        if TNexusToken(FTokens[i]).Pattern = Pattern then
        begin
          Result := i;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TNexusBlock.RaiseParseError(aToken: TNexusToken; aMsg: String);
var
  UpdatedMsg: String;
begin
  ErrLine := aToken.Line;
  ErrCol := aToken.Column;
  UpdatedMsg := aMsg + '(row: ' + IntToStr(ErrLine) + ', column: ' + IntToStr(ErrCol) + ')';
  raise Exception.Create(UpdatedMsg);
end;

function TNexusBlock.ConstructQuotedLabel(const StartIndex: Integer; const EndIndex: Integer; var QuotedLabel: String; UsingCleanList: Boolean): Boolean;
var
  aToken: TNexusToken;
  TempLbl: String;
  i: Integer;
  j: Integer;
begin
  Result := False;
  Assert(StartIndex < EndIndex);
  aToken := TokenAt(StartIndex, UsingCleanList);
  Assert((aToken.NexusTokenType = nttSnglQuote) and (TokenAt(EndIndex).NexusTokenType = nttSnglQuote));
  if aToken.Line <> TokenAt(EndIndex).Line then
    RaiseParseError(aToken, 'quoted lables may not be split over multiple lines');
  TempLbl := aToken.Pattern;
  j := aToken.EndColumn;
  for i := StartIndex + 1 to EndIndex do
  begin
    aToken := TokenAt(i, UsingCleanList);
    while j < aToken.Column do
    begin
      TempLbl := TempLbl + ' ';
      inc(j);
    end;
    TempLbl := TempLbl + aToken.Pattern;
    inc(j, aToken.PatternLength);
  end;
  QuotedLabel := TempLbl;
  Result := True;
end;

constructor TNexusBlock.Create;
begin
  FTokens := TList.Create;
  FCleanTokens := TList.Create;
  ErrMsg := EmptyStr;
  ErrLine := 0;
  ErrCol := 0;
  FLink := EmptyStr;
  FEnforceUppercaseCommands := False;
end;

destructor TNexusBlock.Destroy;
begin
  if Assigned(FTokens) then
  begin
    Clear;
    FTokens.Free;
  end;
  if Assigned(FCleanTokens) then
  begin
    FCleanTokens.Clear;
    FCleanTokens.Free;
  end;
  inherited Destroy;
end;

procedure TNexusBlock.Clear;
var
  i: Integer;
begin
  if Assigned(FTokens) and (FTokens.Count > 0) then
    for i := 0 to FTokens.Count - 1 do
      TNexusToken(FTokens[i]).Free;
  FTokens.Clear;
end;

procedure TNexusBlock.AddToken(AToken: TNexusToken);
begin
  FTokens.Add(AToken);
end;

procedure TNexusBlock.AddTokens(AList: TList);
var
  i: Integer;
begin
  if AList.Count > 0 then
    for i := 0 to AList.Count - 1 do
      FTokens.Add(TNexusToken(AList[i]));
end;

procedure TNexusBlock.UpdateCleanList;
type
  TCurrentState = (csParsingCommands, csInsideComment, csInsideQuote); { it is a finite state machine with 3 states}
var
  i: Integer;
  aToken: TNexusToken;
  aState: TCurrentState;
begin
  aState := csParsingCommands;
  FCleanTokens.Clear;
  if FTokens.Count > 0 then
  begin
    for i := 0 to FTokens.Count - 1 do
    begin
      aToken := TokenAt(i, False);
      case aState of
      csParsingCommands:
        begin
          case aToken.NexusTokenType of
            nttLBracket:
              begin
                aState := csInsideComment;
              end;
            nttRBracket:
              begin
                raise Exception.Create('Nexus format error. Found comment end character ' + #34 + ']' + #34 + ' outside of a comment at row: ' + IntToStr(aToken.Line) + ' col: ' + IntToStr(aToken.Column));
              end;
            nttSnglQuote:
              begin
                aState := csInsideQuote;
                FCleanTokens.Add(aToken);
              end
            else
              begin
                FCleanTokens.Add(aToken);
              end;
          end;
        end; { end csParsingCommands}
      csInsideComment:
        begin
          case aToken.NexusTokenType of
            nttLBracket:
              begin
                raise Exception.Create('Nexus format error. Found comment begin character ' + #34 + '[' + #34 + ' inside of a comment at row: ' + IntToStr(aToken.Line) + ' col: ' + IntToStr(aToken.Column) + '. Nested comments are not allowed');
              end;
            nttRBracket:
              begin
                aState := csParsingCommands;
              end;
            nttSnglQuote:
              begin
                continue;
              end
            else
              begin
                continue;
              end;
          end;
        end; { end csInsideComment}
      csInsideQuote:
        begin
          case aToken.NexusTokenType of
            nttLBracket:
              begin
                FCleanTokens.Add(aToken);
              end;
            nttRBracket:
              begin
                FCleanTokens.Add(aToken);
              end;
            nttSnglQuote:
              begin
                aState := csParsingCommands;
                FCleanTokens.Add(aToken);
              end
            else
              begin
                FCleanTokens.Add(aToken);
              end;
          end;
        end; { end inside quote}
      end;
    end;
  end;
end;

function TNexusBlock.HasToken(TokenPattern: String; UsingCleanList: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result := False;
  if UsingCleanlist then
  begin
    if FCleanTokens.Count > 0 then
    begin
      for i := 0 to FCleanTokens.Count - 1 do
      begin
        if TokenAt(i, True).Pattern = TokenPattern then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end
  else
  begin
    if FTokens.Count > 0 then
    begin
      for i := 0 to FTokens.Count - 1 do
      begin
        if TokenAt(i, False).Pattern = TokenPattern then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TNexusBlock.HasToken(const TokenPattern: String; var Index: Integer; UsingCleanList: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result := False;
  if UsingCleanList then
  begin
    if FCleanTokens.Count > 0 then
    begin
      for i := 0 to FCleanTokens.Count - 1 do
      begin
        if TokenAt(i, True).Pattern = TokenPattern then
        begin
          Result := True;
          Index := i;
          Exit;
        end;
      end;
    end;
  end
  else
  begin
    if FTokens.Count > 0 then
    begin
      for i := 0 to FTokens.Count - 1 do
      begin
        if TokenAt(i, False).Pattern = TokenPattern then
        begin
          Result := True;
          Index := i;
          Exit;
        end;
      end;
    end;
  end;
end;

function TNexusBlock.GetToken(TokenPattern: String): TNexusToken;
var
  i: Integer;
begin
  Result := nil;
  if FTokens.Count > 0 then
  begin
    for i := 0 to FTokens.Count - 1 do
    begin
      if TokenAt(i).Pattern = TokenPattern then
      begin
        Result := TokenAt(i);
        Exit;
      end;
    end;
  end;
end;

function TNexusBlock.TokenAt(Index: Integer; UsingCleanList: Boolean = True): TNexusToken;
begin
  if UsingCleanList then
  begin
    Assert((Index >= 0) and (FCleanTokens.Count > 0) and (Index < FCleanTokens.Count));
  end
  else
  begin
    Assert((Index >= 0) and (FTokens.Count > 0) and (Index < FTokens.Count));
  end;
  if UsingCleanList then
    Result := TNexusToken(FCleanTokens[Index])
  else
    Result := TNexusToken(FTokens[Index]);
end;

function TNexusBlock.Validate: Boolean;
var
  i: Integer = -1;
  NextOcc: Integer;
  aToken: TNexusToken;
  Temp: String = '';
begin
  Result := False;
  if not (FTokens.Count > 5) then
  begin
    ErrMsg := 'Nexus blocks require at least 8 tokens';
    Exit;
  end;
  UpdateCleanList;
  if HasToken('TITLE', i) and ((i + 1) < FCleanTokens.Count) then
  begin
    aToken := TokenAt(i + 1);
    if aToken.NexusTokenType = nttSnglQuote then
    begin
      NextOcc := NextOccurrence(i + 1, #39, True);
      if NextOcc <= (i + 1) then
        RaiseParseError(aToken, 'failed to parse TITLE command');
      if not ConstructQuotedLabel(i + 1, NextOcc, Temp, True) then
        RaiseParseError(aToken, 'failed to parse TITLE command');
      Title := Temp;
    end
    else
     Title := aToken.Pattern;
  end;
  if HasToken('LINK', i) and ((i + 3) < FCleanTokens.Count) then
  begin
    Link := TokenAt(i + 1).Pattern + TokenAt(i + 2).Pattern + TokenAt(i + 3).Pattern;
  end;
  if not (TokenAt(0).Pattern = 'BEGIN') then
    ErrMsg := 'missing block BEGIN command';
  if not (TokenAt(FCleanTokens.Count - 1).Pattern = ';') then
    ErrMsg := ErrMsg + LineEnding + 'missing end semicolon';
  if not (TokenAt(FCleanTokens.Count - 2).Pattern = 'END') then
    ErrMsg := ErrMsg + LineEnding + 'missing END commmand';
  ErrMsg := Trim(ErrMsg);
  Result := (ErrMsg = EmptyStr);
end;

function TNexusBlock.DebugWriteTokens: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add('*** BEGIN-BLOCK - ' + BlockTypeString);
  if FTokens.Count > 0 then
    for i := 0 to FTokens.Count - 1 do
      Result.Add(TNexusToken(FTokens[i]).AsString);
  Result.Add('*** END-BLOCK - ' + BlockTypeString);
end;

function TNexusBlock.DebugWriteData: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(EmptyStr);
  Result.Add('--------- BEGIN NEW BLOCK ----------');
  Result.Add(Format('%-40s = %s', ['TITLE', Title]));
  Result.Add(Format('%-40s = %s', ['LINK', Link]));
  //Result.Add(Format('%-40s = %s', []));
end;

function TNexusBlock.BlockTypeString: String;
begin
  case BlockType of
    nbtTaxa: Result := 'TaxaBlock';
    nbtCharacters: Result := 'CharactersBlock';
    nbtTree: Result := 'TreesBlock';
    nbtNotes: Result := 'NotesBlock';
    nbtUnaligned: Result := 'UnalignedBlock';
    nbtAssumptions: Result := 'AssumptionsBlock';
    nbtCodons: Result := 'CodonsBlock';
    nbtSets: Result := 'SetsBlock';
    nbtDistances: Result := 'DistancesBlock';
    nbtPrivate: Result := 'PrivateBlock';
    else
      Result := 'UnknownBlock';
  end;
end;


end.

