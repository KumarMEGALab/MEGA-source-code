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

unit StringUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, Controls, SysUtils, Graphics;

type

  { TStringCount }

  TStringCount = class(TObject)
    private
      FStringValue: AnsiString;
      FIndex: Integer;
      FCount: Integer;
      {$IFDEF DEBUG}
      function GetNumUniqueChars: Integer;
      {$ENDIF}
    public
      constructor Create(aStr: AnsiString; aIndex: Integer);
      procedure Increment;
      procedure Assign(Source: TStringCount);
      property StringValue: AnsiString read FStringValue;
      property Count: Integer read FCount write FCount;
      property Index: Integer read FIndex;
      {$IFDEF DEBUG}
      property NumUniqueChars: Integer read GetNumUniqueChars;
      {$ENDIF}
  end;

  { TSitePatternFrequency }

  TSitePatternFrequency = class(TStringCount)
    private
      {$IFDEF DEBUG}
      FFirstSiteFoundAt: Integer;
      {$ENDIF}
      function GetPatternLength: Integer;
    public
      constructor Create(aStr: String; aIndex: Integer; aFirstSiteFoundAt: Integer);
      procedure Assign(Source: TSitePatternFrequency);
      function Clone: TSitePatternFrequency;
      {$IFDEF DEBUG}
      property FirstSiteFoundAt: Integer read FFirstSiteFoundAt write FFirstSiteFoundAt;
      {$ENDIF}
      property PatternLength: Integer read GetPatternLength;
  end;

function RemoveTrailingZeros(inputStr: String): String;
function CompareSitePatternFrequency(Item1: Pointer; Item2: Pointer): Integer;
function CompareSitePatternString(Item1: Pointer; Item2: Pointer): Integer;

function AbbreviateGenusInSpeciesName(fullSpeciesName: String): String;
function LastOccurrence(Needle, Haystack: String): Integer;
function BeginsWith(TheString: String; TheSubString: String): boolean;
function Contains(HayStack: String; Needle: String): boolean;
function SplitOnSingleChar(MyFullString: String; Delimiter: Char; IgnoreWhiteSpace: boolean = false): TStringList;
function SplitOnSingleCharFaster(TheString: String; Delimiter: Char; var Tokens: TStringList; SplitOnWhiteSpace: Boolean = False): Boolean;
function SplitOnWhiteSpace(TheString: String): TStringList;
procedure SplitOnWhiteSpace2(const TheString: String; var AList: TStringList);

function SplitOnWhiteSpaceRespQuotes(TheString: String): TStringList;
function IsWhiteSpace(TheChar: String): Boolean;
function ReplaceWhiteSpace(Source: String; NewChar: Char): String;
function RemoveWhiteSpace(Source: String): String;
function NumOccurences(FullString: String; SubString: String; IgnoreCase: Boolean=True): Integer;
function GetWidthText(aText:String; aFont:TFont) : Integer;
function Mince(PathToMince: String; MaxNumChars: Integer): String;
function EscapeSpecialCharsInStringLiteral(aLiteral: String): String;
function LongestString(aList: TStringList): String;
function LongestName(aList: TStringList): String;
function LengthOfLongestName(aList: TStringList): Integer;
function LengthOfLongestString(aList: TStringList): Integer;
function RemoveLineEndings(aString: String): String;
function RemoveDuplicateStrings(aLongString: String): String;
function SanitizeForJavaScript(aString: String): String;
function JavaScriptFontPickerOptions(id: String): String;
{ Tries to format a double using the given precision and digits. If it would result in a zero,
  then switch to scientific notation}
function FormatDoubleSafe(Value: Double; Precision: Integer; Digits: Integer; IgnoreNan: Boolean=False): String;
function fds(Value: Double; Precision: Integer; Digits: Integer; IgnoreNan: Boolean=False): String;

// Allows you to do a case statement with strings. Case insensitive.
function StringToCaseSelect(Selector : String; CaseList: array of String): Integer;
function URLEncode(s: string): string;

function MinimizeWhiteSpace(htmlInnerText: WideString): WideString;
function CleanupMegaCaptionText(htmlInnerText: WideString): WideString; { a hack for Linux since we can't use THtmlViewer to get clean inner text (we get it from TChromium but all the extra white space is there)}

{
  WARNING:  this function does not respect format settings - it is intended solely for a specific purpose
  and should not be used in general. If you need something like this, you should probably roll your own
}
function ExtractPrecisionFromFloatFormatString(aFormatStr: String): Integer;
{$IFDEF FPC}
function ContainsText(HayStack: String; Needle: String): Boolean;
{$ENDIF}
function StringListNaturalCompare(List: TStringlist; Index1, Index2: Integer): Integer;
function PartitionsListStringsCompare(List: TStringList; Index1, Index2: Integer): Integer;

function ReplaceDisallowedChars(input: String): String;
function PutBackDisallowedChars(input: String): String;

function ListHasDuplicateStrings(sourceList: TStringList): Boolean;

implementation

uses
  StrUtils, Math, Forms;

function ReplaceDisallowedChars(input: String): String;
begin
  Result := StringReplace(input, ' >= ', ' gte ', [rfReplaceAll]);
  Result := StringReplace(Result, ' <= ', ' lte ', [rfReplaceAll]);
end;

function PutBackDisallowedChars(input: String): String;
begin
  Result := StringReplace(input, ' gte ', ' >= ', [rfReplaceAll]);
  Result := StringReplace(Result, ' lte ', ' <= ', [rfReplaceAll]);
end;

function ListHasDuplicateStrings(sourceList: TStringList): Boolean;
var
  i: Integer = -1;
  aList: TStringList = nil;
begin
  Result := False;
  if sourceList.Count = 0 then
    Exit;

  try
    aList := TStringList.Create;
    aList.Sorted := True;
    aList.Duplicates := dupIgnore;
    for i := 0 to sourceList.Count - 1 do
      aList.Add(sourceList[i]);
    if aList.Count < sourceList.Count then
      Result := True
    else
      Result := False;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function URLEncode(s: string): string;
var
  i: integer;
  source: PChar;
begin
  result := '';
  source := pchar(s);
  for i := 1 to length(source) do
    if not (source[i - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '~', '.', ':', '/']) then
      result := result + '%' + inttohex(ord(source[i - 1]), 2)
    else
      result := result + source[i - 1];
end;

function EscapeSpecialCharsInStringLiteral(aLiteral: String): String;
const
  Specials = '\[^$.|?*+(){}'; { THE BACKSLASH MUST BE FIRST}
var
  i: Integer;
begin
  Result := aLiteral;
  for i := 1 to Length(Specials) do
    Result := StringReplace(Result, Specials[i], '\' + Specials[i], [rfReplaceAll])
end;

function LongestString(aList: TStringList): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if aList.Count > 0 then
    for i := 0 to aList.Count - 1 do
      if Length(aList[i]) > Length(Result) then
        Result := aList[i];
end;

function LongestName(aList: TStringList): String;
var
  i: Integer;
  aName: String = '';
begin
  Result := EmptyStr;
  if aList.Count > 0 then
    for i := 0 to aList.Count - 1 do
    begin
      aName := aList.Names[i];
      if Length(aName) > Length(Result) then
        Result := aName;

    end;
end;

function LengthOfLongestName(aList: TStringList): Integer;
begin
  Result := Length(LongestName(aList));
end;

function LengthOfLongestString(aList: TStringList): Integer;
begin
  Result := Length(LongestString(aList));
end;

function RemoveTrailingZeros(inputStr: String): String;
var
  isEdited: Boolean = False;
begin
  Result := inputStr;
  while Result[Length(Result)] = '0' do
  begin
    SetLength(Result, Length(Result) - 1);
    isEdited := True;
    if isEdited and (Length(Result) > 0) and (Result[Length(Result)] = ',') then
      SetLength(Result, Length(Result) - 1);
  end;
  if isEdited and (Length(Result) > 0) and (Result[Length(Result)] = '.') then
    SetLength(Result, Length(Result) - 1);
end;

function CompareSitePatternFrequency(Item1: Pointer; Item2: Pointer): Integer;
var
  f1: TSitePatternFrequency = nil;
  f2: TSitePatternFrequency = nil;
begin
  f1 := TSitePatternFrequency(Item1);
  f2 := TSitePatternFrequency(Item2);
  Result := CompareValue(f2.Count, f1.Count);
end;

function CompareSitePatternString(Item1: Pointer; Item2: Pointer): Integer;
var
  f1: TSitePatternFrequency = nil;
  f2: TSitePatternFrequency = nil;
begin
  f1 := TSitePatternFrequency(Item1);
  f2 := TSitePatternFrequency(Item2);
  Result := CompareStr(f1.StringValue, f2.StringValue);
end;

function AbbreviateGenusInSpeciesName(fullSpeciesName: String): String;
begin
  Result := Trim(fullSpeciesName);
  if (Length(Result) > 2) and (Pos(' ', Result) > 0) then
    Result := Result[1] + '.' + Copy(Result, Pos(' ', Result), length(Result))
end;

function LastOccurrence(Needle, Haystack: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Trim(Haystack) = EmptyStr then
    Exit;
  for i := Length(Haystack) downto 0 do
  begin
    if PosEx(Needle, Haystack, i) > 0 then
    begin
      Result := PosEx(Needle, Haystack, i);
      Exit;
    end;
  end;
end;

function RemoveLineEndings(aString: String): String;
begin
  Result := StringReplace(aString, LineEnding, ' ', [rfReplaceAll]);
end;

function RemoveDuplicateStrings(aLongString: String): String;
var
  filteredStrings: TStringList = nil;
  allStrings: TStringList = nil;
  i: Integer;
begin
  try
    filteredStrings := TStringList.Create;
    allStrings := TStringList.Create;
    allStrings.Text := aLongString;
    if allStrings.Count > 0 then
      for i := 0 to allStrings.Count - 1 do
      begin
        if filteredStrings.IndexOf(allStrings[i]) < 0 then
          filteredStrings.Add(Trim(allStrings[i]));
      end;
    Result := filteredStrings.Text;
  finally
    if Assigned(filteredStrings) then
      filteredStrings.Free;
    if Assigned(allStrings) then
      allStrings.Free;
  end;
end;

function SanitizeForJavaScript(aString: String): String;
begin
  Result := StringReplace(aString, #13#10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #9, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #34, '\' + #34, [rfReplaceAll]);
  Result := StringReplace(Result, #39, '\' + #39, [rfReplaceAll]);
end;

function JavaScriptFontPickerOptions(id: String): String;
var
  i: Integer;
begin
  Result := Format('$("#%s").fontpicker({', [id]) + LineEnding;
  Result := Result + Format('%sshowOn: %sall%s,', [#9, #39, #39]) + LineEnding;
  Result := Result + Format('%ssettings: {', [#9]) + LineEnding;
  Result := Result + Format('%s%s%scharacter%s: [', [#9, #9, #39, #39]) + LineEnding;
  Result := Result + Format('%s%s%s%sunderline%s,', [#9, #9, #9, #39, #39]) + LineEnding;
  Result := Result + Format('%s%s%s%sline-through%s', [#9, #9, #9, #39, #39]) + LineEnding;
  Result := Result + Format('%s%s]', [#9, #9]) + LineEnding;
  Result := Result + Format('%s},', [#9]) + LineEnding;
  Result := Result + Format('%sfamilies: [', [#9, #9, #39, #39]) + LineEnding;
  if Screen.Fonts.Count > 0 then
    for i := 0 to Screen.Fonts.Count - 1 do
    begin
      if (Screen.Fonts[i] <> EmptyStr) and (Screen.Fonts[i][1] <> '@') then
      begin
        Result := Result + Format('%s%s%s{  name: "%s",', [#9, #9, #9, Screen.Fonts[i]]) + LineEnding;
        Result := Result + Format('%s%s%s   faces: ["%s"]', [#9, #9, #9, Screen.Fonts[i]]) + LineEnding;
        Result := Result + Format('%s%s%s}', [#9, #9, #9]);
        if i <> (Screen.Fonts.Count - 1) then
          Result := Result + ',';
        Result := Result + LineEnding;
      end;
    end;
  Result := Result + Format('%s%s]', [#9, #9]) + LineEnding;
  Result := Result + '});';
end;

function fds(Value: Double; Precision: Integer; Digits: Integer; IgnoreNan: Boolean=False): String;
begin
  Result := FormatDoubleSafe(Value, Precision, Digits, IgnoreNan);
end;

function FormatDoubleSafe(Value: Double; Precision: Integer; Digits: Integer; IgnoreNan: Boolean=False): String;
var
  TempStr: String;
  TestFloat: Double;
begin
    if IsNan(Value) then
    begin
      if not IgnoreNan then
        raise Exception.Create('A calculated value is NAN (Not a Number)')
      else
      begin
        Result := 'NAN';
        Exit;
      end;
    end;
  TempStr := Format('%' + IntToStr(Digits) + '.' + IntToStr(Precision) + 'f', [Value]);
  if TryStrToFloat(TempStr, TestFloat) then
    if (CompareValue(TestFloat, 0.0, 0.000001)= EqualsValue) then
      TempStr := Format('%' + IntToStr(Digits) + '.' + IntToStr(3) + 'e', [Value]);

  Result := TempStr;
end;

function BeginsWith(TheString: String; TheSubString: String): boolean;
begin
  Result := AnsiStartsText(TheSubString, TheString);
end;

function Contains(HayStack: String; Needle: String): boolean;
begin
  if (Pos(Needle, HayStack) > 0) then
    Result := true
  else
    Result := false;
end;

/// <summary>Split by the Delimiter to get each element. This function correctly
/// handles whitespace unlike the TStringList's delimited text function.</summary>
function SplitOnSingleChar(MyFullString: String; Delimiter: Char; IgnoreWhiteSpace: boolean = false): TStringList;
var
  currentPosition: Integer;
  tempString: String;
  MySplitStrings: TStringList;
begin
  MySplitStrings := TStringList.Create;
  currentPosition := Pos(Delimiter,MyFullString);
  while currentPosition <> 0 do
  begin
    tempString := copy(MyFullString, 1, currentPosition-1);
    delete(MyFullString,1,currentPosition+Length(Delimiter)-1);
    if (trim(tempString) <> '') or (not IgnoreWhiteSpace) then
      MySplitStrings.Add(tempString);
    currentPosition := Pos(Delimiter, MyFullString);
  end;
  if (trim(MyFullString) <> '') or (not IgnoreWhiteSpace) then
    MySplitStrings.Add(MyFullString);
  Result := MySplitStrings;
end;

// faster for the case where it is called many times because we avoid creation/destruction of the string list
// if SplitOnWhiteSpace is true, then the string will also be split on any white space characters
function SplitOnSingleCharFaster(TheString: String; Delimiter: Char; var Tokens: TStringList; SplitOnWhiteSpace: Boolean = False): Boolean;
var
  CurrentPosition: Integer;
  TempString: String;
  CleanString: String;
  i: Integer;
begin
  Tokens.Clear;
  Result := True;
  if SplitOnWhiteSpace then
    CleanString := Trim(TheString)
  else
    CleanString := TheString;

  try
    i := 1;
    CurrentPosition := 1;
    while i < Length(CleanString) do
    begin
      if (CleanString[i] = Delimiter) or (SplitOnWhiteSpace and IsWhiteSpace(CleanString[i])) then
      begin
        TempString := Trim(Copy(CleanString, CurrentPosition, i - CurrentPosition));
        Tokens.Add(TempString);
        while (i < Length(CleanString)) and ((CleanString[i] = Delimiter) or (SplitOnWhiteSpace and IsWhiteSpace(CleanString[i]))) do
          inc(i);
        CurrentPosition := i;
      end
      else
        inc(i);
    end;
    TempString := Trim(Copy(CleanString, CurrentPosition, Length(CleanString)));
    if (TempString <> EmptyStr) or (not SplitOnWhiteSpace) then
      Tokens.Add(TempString);
  Except
    on E:Exception do
    begin
      Result := False;
    end;
  end;
end;

procedure SplitOnWhiteSpace2(const TheString: String; var AList: TStringList);
var
  i: Integer;
  CleanString: String;
  Temp: String;
  Token: String;
begin
  Token := EmptyStr;
  CleanString := trim(TheString);
  if not Assigned(AList) then
    AList := TStringList.Create
  else
    AList.Clear;

  for i := 1 to Length(CleanString) do
  begin
    Temp := CleanString[i];
    if not IsWhiteSpace(Temp) then
    begin
      Token := Token + Temp;
    end
    else
    begin
      if Length(Token) > 0 then
        AList.Add(Token);
      Token := EmptyStr;
    end;
  end;
  if Length(Token) > 0 then
    AList.Add(Token);
end;

function SplitOnWhiteSpace(TheString: String): TStringList;
var
  i: Integer;
  CleanString: String;
  Temp: String;
  Token: String;
begin
  Token := EmptyStr;
  CleanString := trim(TheString);
  Result := TStringList.Create;
  for i := 1 to Length(CleanString) do
  begin
    Temp := CleanString[i];
    if not IsWhiteSpace(Temp) then
    begin
      Token := Token + Temp;
    end
    else
    begin
      if Length(Token) > 0 then
        Result.Add(Token);
      Token := EmptyStr;
    end;
  end;
  if Length(Token) > 0 then
    Result.Add(Token);
end;

function SplitOnWhiteSpaceRespQuotes(TheString: String): TStringList;
var
  Sngl: Char = #39;
  Dbl: Char = #34;
  i: Integer;
  CleanString: String = '';
  Temp: String = '';
  Token: String = '';
  insideSingleQuote: Boolean = False;
  insideDblQuote: Boolean = False;
begin
  Token := EmptyStr;
  CleanString := trim(TheString);
  Result := TStringList.Create;
  for i := 1 to Length(CleanString) do
  begin
    Temp := CleanString[i];

    if Temp = Sngl then
    begin
      insideSingleQuote := not insideSingleQuote;
      continue;
    end
    else if Temp=Dbl then
    begin
      insideDblQuote := not insideDblQuote;
      continue
    end
    else
      if insideSingleQuote or insideDblQuote then
      begin
        token := token + temp;
        continue;
      end;
    
    if isWhiteSpace(Temp) then
    begin
      if Length(Token) > 0 then
        Result.Add(Token);
      Token := EmptyStr;
    end
    else
    begin
      Token := Token + Temp;
    end;
  end;
  if Length(Token) > 0 then
    Result.Add(Token);
end;


function ReplaceWhiteSpace(Source: String; NewChar: Char): String;
var
  i: Integer;
begin
  Result := trim(Source);
  for i := 1 to Length(Result) do
    if IsWhiteSpace(Result[i]) then
      Result[i] := NewChar;
end;

function RemoveWhiteSpace(Source: String): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(Source) do
    if not IsWhiteSpace(Source[i]) then
      Result := Result + Source[i];
end;

function IsWhiteSpace(TheChar: String): Boolean;
begin
  Result := (TheChar = ' ') or
            (TheChar = #9) or
            (TheChar = #13#10) or
            (TheChar = #10) or
            (TheChar = #13) {$IFDEF FPC}or
            (TheChar = LineEnding){$ENDIF};
end;

function NumOccurences(FullString: String; SubString: String; IgnoreCase: Boolean=True): Integer;
var
  Offset: Integer;
  aFullString: String;
  aSubString: String;
begin
  if IgnoreCase then
  begin
    aFullString := LowerCase(FullString);
    aSubString := LowerCase(FullString);
  end
  else
  begin
    aFullString := FullString;
    aSubString := SubString;
  end;

  Result := 0;
  Offset := PosEx(aSubString, aFullString, 1);
  while Offset <> 0 do
  begin
    inc(Result);
    Offset := PosEx(aSubString, aFullString, Offset + length(aSubString));
  end;
end;


function GetWidthText(aText:String;aFont:TFont) : Integer;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
   Bmp.Canvas.Font := aFont;
   Result := Bmp.Canvas.TextWidth(aText);
  finally
   Bmp.Free;
  end;
end;

// Case insensitive compare
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

function MinimizeWhiteSpace(htmlInnerText: WideString): WideString;
var
  aList: TStringList = nil;
  i: Integer;
begin
  try
    aList := TStringList.Create;
    aList.Text := Trim(htmlInnerText);
    if aList.Count > 0 then
    begin
      for i := 0 to aList.Count - 1 do
        aList[i] := Trim(aList[i]);
      if aList.Count > 1 then
        for i := aList.Count - 1 downto 1 do
        begin
          if (aList[i] = EmptyStr) and (aList[i - 1] = EmptyStr) then
            aList.Delete(i);
      end;
    end;
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function CleanupMegaCaptionText(htmlInnerText: WideString): WideString;
var
  temp: WideString;
  aList: TStringList = nil;
  bList: TStringList = nil;
  i, j: Integer;
begin
  temp := MinimizeWhiteSpace(htmlInnerText);
  try
    aList := TStringList.Create;
    aList.Text := temp;
    bList := TStringList.Create;
    if aList.Count > 1 then
    begin
      temp := EmptyStr;
      bList.Add(aList[0]);
      bList.Add(EmptyStr);
      for i := 1 to aList.Count - 1 do
      begin
        if Trim(aList[i]) = EmptyStr then
          continue;
        if (Length(aList[i]) > 1) and (aList[i][1] = '1') and (aList[i][2] = '.') then
        begin
          bList.Add(Trim(temp));
          temp := EmptyStr;
          j := i;
          break;
        end
        else
          temp := temp + ' ' + aList[i];
      end;
      bList.Add(EmptyStr);
      while j < (aList.Count - 1) do
      begin
        bList.Add(Trim(aList[j]));
        inc(j);
      end;
    end
    else
      bList.Text := aList.Text;

    Result := bList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(bList) then
      bList.Free;
  end;
end;

{
  WARNING:  this function does not respect format settings - it is intended solely for a specific purpose
  and should not be used in general. If you need something like this, you should probably roll your own
}
function ExtractPrecisionFromFloatFormatString(aFormatStr: String): Integer;
var
  TempStr: String;
  TempInt: Integer;
begin
  Result := -1;
  TempInt := -1;
  TempStr := aFormatStr;
  if not (TempStr[Length(TempStr)] = 'f') then
    Exit;
  Delete(TempStr, Length(TempStr), 1); { remove the f}
  while (Length(TempStr) > 0) and (TempStr[1] <> '.') do { remove everything before the decimal separator}
    Delete(TempStr, 1, 1);
  if (not Length(TempStr) > 1) or (TempStr[1] <> '.') then
    Exit;
  TempStr := Copy(TempStr, 2, Length(TempStr));
  if TryStrToInt(TempStr, TempInt) then
    Result := TempInt;
end;

function ContainsText(HayStack: String; Needle: String): Boolean;
begin
  Result := (Pos(LowerCase(Needle), LowerCase(HayStack)) > 0);
end;

function Mince(PathToMince: String; MaxNumChars: Integer): String;
 var
   TotalLength, FLength: Integer;
 begin
   TotalLength := Length(PathToMince) ;
   if TotalLength > MaxNumChars then
   begin
    FLength := (MaxNumChars Div 2) - 2;
    Result := Copy(PathToMince, 0, fLength)
              + '...'
              + Copy(PathToMince,
                    TotalLength-fLength,
                    TotalLength) ;
   end
   else
     Result := PathToMince;
 end;

function StringListNaturalCompare(List: TStringlist; Index1, Index2: Integer): Integer;
var
  b: Boolean;
  First, Second: String;

  function ExtractNumeric(Index: Integer; var SourceStr: String): Double;
  begin
    while (Index <= Length(SourceStr)) and (SourceStr[Index] in ['0'..'9', '.']) do
      inc(Index);
    Result := StrToFloatDef(Copy(SourceStr, 1, (Index - 1)), 0);
    Delete(SourceStr, 1, (Index - 1));
  end;

begin
  Result := 0;
  First := LowerCase(List[Index1]);
  Second := LowerCase(List[Index2]);

  if (First = Second) then // many will be the same so we can return quickly
    Exit;

  if (First <> EmptyStr) and (Second <> EmptyStr) then begin
    b := False;
    while (not b) do
    begin
      if ((First[1] in ['0'..'9']) and (Second[1]  in ['0'..'9'])) then
        Result:= Sign(ExtractNumeric(1, First) - ExtractNumeric(1, Second))
      else
        Result:= Sign(Integer(First[1]) - Integer(Second[1]));
      b := (Result <> 0) or (Min(Length(First), Length(Second)) < 2);
      if not b then begin
        Delete(First,1,1);
        Delete(Second,1,1);
      end;
    end;
  end;
  if Result = 0 then
  begin
    if (Length(First) = 1) and (Length(Second) = 1) then
      Result:= Sign(Integer(First[1]) - Integer(Second[1]))
    else
      Result:= Sign(Length(First) - Length(Second));
  end;
 end;

 function PartitionsListStringsCompare(List: TStringList; Index1, Index2: Integer): Integer;
 var
   First, Second: String;
   Index: Integer;
   MaxIndex: Integer;
 begin
   Result := 0;
   First := Trim(List[Index1]);
   Second := Trim(List[Index2]);
   if First = Second then
     Exit;
   Index := 1;
   if (First = EmptyStr) or (Second = EmptyStr) then
   begin
     Result := CompareStr(First, Second);
   end
   else
   begin
     MaxIndex := Min(Length(First), Length(Second));
     while Index <= MaxIndex do
     begin
       if First[Index] <> Second[Index] then
       begin
         Result := CompareStr(Second[Index], First[Index]);
         Exit;
       end;
       inc(Index);
     end;
   end;
 end;

 { TSitePatternFrequency }

 //function TSitePatternFrequency.GetProbability: Extended;
 //begin
 //  Result := FProbability;
 //end;

function TSitePatternFrequency.GetPatternLength: Integer;
begin
  Result := Length(FStringValue);
end;

 constructor TSitePatternFrequency.Create(aStr: String; aIndex: Integer;
   aFirstSiteFoundAt: Integer);
 begin
   inherited Create(aStr, aIndex);
   {$IFDEF DEBUG}
   FFirstSiteFoundAt := aFirstSiteFoundAt;
   {$ENDIF}
 end;

 procedure TSitePatternFrequency.Assign(Source: TSitePatternFrequency);
 begin
   FStringValue := Source.StringValue;
   FCount := Source.Count;
   FIndex := Source.Index;
   {$IFDEF DEBUG}
   FFirstSiteFoundAt := Source.FirstSiteFoundAt;
   {$ENDIF}
 end;

 function TSitePatternFrequency.Clone: TSitePatternFrequency;
 begin
   Result := TSitePatternFrequency.Create(FStringValue, FIndex, -1);
   Result.Assign(Self);
 end;

 { TStringCount }

{$IFDEF DEBUG}
function TStringCount.GetNumUniqueChars: Integer;
var
  i: Integer = -1;
begin
  Result := 0;

  if Length(FStringValue) > 0 then
    for i := 1 to Length(FStringValue) do
      if FStringValue.CountChar(FStringValue[i]) = 1 then
        inc(Result);
end;
{$ENDIF}

 constructor TStringCount.Create(aStr: String; aIndex: Integer);
 begin
   FStringValue := aStr;
   FCount := 0;
   FIndex := aIndex;
 end;

  procedure TStringCount.Increment;
 begin
   inc(FCount);
 end;

  procedure TStringCount.Assign(Source: TStringCount);
  begin
    FStringValue := Source.StringValue;
    FCount := Source.Count;
    FIndex := Source.Index;
  end;

end.
