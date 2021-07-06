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

unit nexustokens;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtokenizer;

type
  TNexusTokenType = (nttBegin, nttEnd, nttNexus, nttString, nttInt, nttFloat, nttSemicolon,
                     nttTaxa, nttTitle, nttDimensions, nttNTax, nttTaxLabels, nttData, nttCharacters,
                     nttLink, nttFormat, nttDataType, nttSymbols, nttMissing, nttGap,
                     nttNChar, nttMatrix, nttTranslate, nttTree, nttTrees,
                     nttTaxLabel, nttLBracket, nttRBracket, nttEquals, nttWhitespace,
                     nttComma, nttSnglQuote, nttDblQuote, nttUnknown);

  { TNexusToken }

  TNexusToken = class(TTokenBase)
  private
    FNexusTokenType: TNexusTokenType;
    function GetIsCommandToken: Boolean;
    function GetNexusTypeString: String;
    procedure SetFNexusTokenType(AValue: TNexusTokenType);
    protected

    public
      constructor Create;
      destructor Destroy; override;

      property NexusTokenType: TNexusTokenType read FNexusTokenType write SetFNexusTokenType;
      property IsCommandToken: Boolean read GetIsCommandToken;
      property NexusTypeString: String read GetNexusTypeString;
      function AsString: String; override;
      function PatternLength: Integer;
      function HasSameText(aText: String): Boolean;
      procedure ChangeCommandToUppercase;
  end;

  function GetTokenType(AToken: TTokenBase; EnforceUppercaseCommands: Boolean): TNexusTokenType;

implementation

function GetTokenType(AToken: TTokenBase; EnforceUppercaseCommands: Boolean): TNexusTokenType;
begin
  case AToken.TokenBaseType of
    ttInt: Result := nttInt;
    ttReal: Result := nttFloat;
    ttString:
      begin
        if EnforceUppercaseCommands then
        begin
          if AToken.Pattern = 'BEGIN' then
            Result := nttBegin
          else if AToken.Pattern = 'END' then
            Result := nttEnd
          else if AToken.Pattern = 'NEXUS' then
            Result := nttNexus
          else if AToken.Pattern = 'TAXA' then
            Result := nttTaxa
          else if AToken.Pattern = 'TITLE' then
            Result := nttTitle
          else if AToken.Pattern = 'DIMENSIONS' then
            Result := nttDimensions
          else if AToken.Pattern = 'NTAX' then
            Result := nttNTax
          else if AToken.Pattern = 'TAXLABEL' then
            Result := nttTaxLabel
          else if AToken.Pattern = 'TAXLABELS' then
            Result := nttTaxLabels
          else if AToken.Pattern = 'DATA' then
            Result := nttData
          else if AToken.Pattern = 'CHARACTERS' then
            Result := nttCharacters
          else if AToken.Pattern = 'LINK' then
            Result := nttLink
          else if AToken.Pattern = 'FORMAT' then
            Result := nttFormat
          else if AToken.Pattern = 'DATATYPE' then
            Result := nttDataType
          else if AToken.Pattern = 'SYMBOLS' then
            Result := nttSymbols
          else if AToken.Pattern = 'MISSING' then
            Result := nttMissing
          else if AToken.Pattern = 'GAP' then
            Result := nttGap
          else if AToken.Pattern = 'NCHAR' then
            Result := nttNChar
          else if AToken.Pattern = 'MATRIX' then
            Result := nttMatrix
          else if AToken.Pattern = 'TRANSLATE' then
            Result := nttTranslate
          else if AToken.Pattern = 'TREE' then
            Result := nttTree
          else if AToken.Pattern = 'TREES' then
            Result := nttTrees
          else Result := nttString;
        end
        else
        begin
          if SameText(AToken.Pattern, 'BEGIN') then
            Result := nttBegin
          else if SameText(AToken.Pattern, 'END') then
            Result := nttEnd
          else if SameText(AToken.Pattern, 'NEXUS') then
            Result := nttNexus
          else if SameText(AToken.Pattern, 'TAXA') then
            Result := nttTaxa
          else if SameText(AToken.Pattern, 'TITLE') then
            Result := nttTitle
          else if SameText(AToken.Pattern, 'DIMENSIONS') then
            Result := nttDimensions
          else if SameText(AToken.Pattern, 'NTAX') then
            Result := nttNTax
          else if SameText(AToken.Pattern, 'TAXLABEL') then
            Result := nttTaxLabel
          else if SameText(AToken.Pattern, 'TAXLABELS') then
            Result := nttTaxLabels
          else if SameText(AToken.Pattern, 'DATA') then
            Result := nttData
          else if SameText(AToken.Pattern, 'CHARACTERS') then
            Result := nttCharacters
          else if SameText(AToken.Pattern, 'LINK') then
            Result := nttLink
          else if SameText(AToken.Pattern, 'FORMAT') then
            Result := nttFormat
          else if SameText(AToken.Pattern, 'DATATYPE') then
            Result := nttDataType
          else if SameText(AToken.Pattern, 'SYMBOLS') then
            Result := nttSymbols
          else if SameText(AToken.Pattern, 'MISSING') then
            Result := nttMissing
          else if SameText(AToken.Pattern, 'GAP') then
            Result := nttGap
          else if SameText(AToken.Pattern, 'NCHAR') then
            Result := nttNChar
          else if SameText(AToken.Pattern, 'MATRIX') then
            Result := nttMatrix
          else if SameText(AToken.Pattern, 'TRANSLATE') then
            Result := nttTranslate
          else if SameText(AToken.Pattern, 'TREE') then
            Result := nttTree
          else if SameText(AToken.Pattern, 'TREES') then
            Result := nttTrees
          else Result := nttString;
        end;
      end;
    ttDelim:
      begin
        if AToken.Pattern = ';' then
          Result := nttSemicolon
        else if AToken.Pattern = '=' then
          Result := nttEquals
        else if AToken.Pattern = ',' then
          Result := nttComma
        else if AToken.Pattern = #34 then
          Result := nttDblQuote
        else if AToken.Pattern = #39 then
          Result := nttSnglQuote
        else if AToken.Pattern = '[' then
          Result := nttLBracket
        else if AToken.Pattern = ']' then
          Result := nttRBracket
        else
          Result := nttWhitespace;
      end;
    ttUnknown: Result := nttUnknown;
  end;
end;

{ TNexusToken }

function TNexusToken.GetIsCommandToken: Boolean;
begin
  Assert(False, 'not implemented');
  Result := False;
end;

function TNexusToken.GetNexusTypeString: String;
begin
  case NexusTokenType of
    nttBegin: Result := 'block-begin';
    nttEnd: Result := 'block-end';
    nttNexus: Result := 'format';
    nttString: Result := 'string';
    nttInt: Result := 'integer';
    nttFloat: Result := 'double';
    nttSemicolon: Result := 'semicolon';
    nttTaxa, nttTrees, nttCharacters: Result := 'blockname';
    nttTitle, nttDimensions, nttTaxLabel, nttTaxLabels,
    nttNTax, nttLink, nttFormat, nttDataType, nttSymbols,
    nttMissing, nttGap, nttNChar, nttMatrix, nttTranslate,
    nttTree: Result := 'command';
    nttLBracket: Result := 'comment-begin';
    nttRBracket: Result := 'comment-end';
    nttEquals: Result := 'equals';
    nttWhitespace: Result := 'whitespace';
    nttComma: Result := 'comma';
    nttSnglQuote: Result := 'snglquote';
    nttDblQuote: Result := 'dblquote';
    nttUnknown: Result := 'unknown';
    else
      Result := 'unknown';
  end;
end;

procedure TNexusToken.SetFNexusTokenType(AValue: TNexusTokenType);
begin
  if FNexusTokenType=AValue then Exit;
  FNexusTokenType:=AValue;
end;

constructor TNexusToken.Create;
begin
  inherited Create;
end;

destructor TNexusToken.Destroy;
begin
  inherited Destroy;
end;

function TNexusToken.AsString: String;
begin
  Result := Format('Row: %-7d Col: %-7d BaseType: %-15s NexusType: %-15s Pattern: %s', [FLine, FColumn, TokenTypeString, NexusTypeString, FPattern]);
end;

function TNexusToken.PatternLength: Integer;
begin
  Result := Length(Pattern);
end;

function TNexusToken.HasSameText(aText: String): Boolean;
begin
  Result := SameText(aText, FPattern);
end;

procedure TNexusToken.ChangeCommandToUppercase;
begin
  case FNexusTokenType of
    nttBegin, nttEnd, nttNexus, nttTaxa, nttTitle, nttDimensions, nttTaxLabels, nttNTax,
    nttCharacters, nttLink, nttFormat, nttDataType, nttSymbols, nttMissing, nttGap,
    nttNChar, nttMatrix, nttTranslate, nttTree, nttTrees, nttTaxLabel: FPattern := Uppercase(FPattern);
  end;
end;

end.

