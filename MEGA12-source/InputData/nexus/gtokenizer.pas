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

unit gtokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, regexpr;

const
  MAX_BUFFER_SIZE = 1023;

type
  TDelimSet = set of char;

  
  TCharType = (ctTilde, ctBackTick, ctExclamation, ctAddress, ctPound, ctDollar,
                ctPercent, ctCaret, ctAmpersand, ctAsterisk, ctLParen, ctRightParen,
                ctUnderscore, ctMinus, ctPlus, ctEqual, ctBackSlash, ctPipe,
                ctLBracket, ctRBracket, ctLBrace, ctRBrace, ctSingleQuote, ctDblQuote,
                ctColon, ctSemiColon, ctForwardSlash, ctQuestion, ctPeriod, ctComma,
                ctGreaterThan, ctLessThan, ctTab, ctSpace, ctNewLine, ctEOF, ctReturn,
                ctAlpha, ctNumber, ctUnknown);

  TTokenBaseType = (ttInt, ttReal, ttString, ttDelim, ttUnknown);  { TODO 1 -oglen : what token types should be included? should it be variable }

  { TTokenBase }

  TTokenBase = class(TObject)
    protected
      FLine: Integer;
      FColumn: Integer;
      FPattern: String;
      FTokenBaseType: TTokenBaseType;
      function GetLineAsStr: String;
      function GetColAsStr: String;
      function GetPositionAsStr: String;
      function GetTokenTypeString: String;
    public
      constructor Create; overload;
      constructor Create(Line: Integer; Col: Integer); overload;
      procedure Assign(Other: TTokenBase);
      procedure Reset;
      function AsString: String; virtual;
      function EndColumn: Integer;
      property Line: Integer read FLine write FLine;
      property Column: Integer read FColumn write FColumn;
      property TokenBaseType: TTokenBaseType read FTokenBaseType write FTokenBaseType;
      property Pattern: String read FPattern write FPattern;
      property LineAsStr: String read GetLineAsStr;
      property ColAsStr: String read GetColAsStr;
      property PositionAsStr: String read GetPositionAsStr;
      property TokenTypeString: String read GetTokenTypeString;
  end;

  { TGTokenizer }

  { TODO 1 -oglen : need to fix the code to handle unix line endings }
  TGTokenizer = class(TObject)
    private
      FSrcFile: String;
      FFileStream: TFileStream;
      FDelims: TDelimSet;
      FBuffer: array[0..MAX_BUFFER_SIZE] of char;
      FBeginPos: Integer;  // position in the buffer where the current token begins
      FCurrPos: Integer; // our current position as we are scanning the buffer
      FEndPos: Integer; // position of the last valid character in the buffer
      FLine: Integer;
      FColumn: Integer;
      FToken: TTokenBase;
      FHasNext: Boolean;
      //FRegexInt: TRegExpr;
      //FRegexReal: TRegExpr;
      FBytesRead: Int64;
      function ReloadBuffer: Boolean;
      function EndOfFile: Boolean;
      function IsDelimiter(AChar: Char): Boolean;
      function IsWhiteSpace(AChar: Char): Boolean;
      function CheckBufferReload: Boolean;
      function InferTokenType(Pattern: String): TTokenBaseType;
      function GetProgress: Int64;
    public
      FileSize: Int64;
      constructor Create(AFileName: String; Delims: TDelimSet);
      destructor Destroy; override;
      function ReloadFile: Boolean;
      procedure NextToken(var Token: TTokenBase);
      property HasNext: Boolean read FHasNext;
      property SrcFile: String read FSrcFile write FSrcFile;
      property Progress: Int64 read GetProgress;
      property Line: Integer read FLine;
      property Column: Integer read FColumn;
  end;




implementation

uses
  Math;

{ TTokenBase }

function TTokenBase.GetLineAsStr: String;
begin
  Result := IntToStr(FLine);
end;

function TTokenBase.GetColAsStr: String;
begin
  Result := IntToStr(FColumn);
end;

function TTokenBase.GetPositionAsStr: String;
begin
  Result := '[Line=' + IntToStr(FLine) + ', Column=' + IntToStr(FColumn) + ']';
end;

function TTokenBase.GetTokenTypeString: String;
begin
  case FTokenBaseType of
    ttInt:
      Result := 'Integer';
    ttReal:
      Result := 'Real';
    ttString:
      Result := 'String';
    ttDelim:
      Result := 'Delimiter';
    ttUnknown:
      Result := 'Unknown';
    else
      Result := 'Unknown';
  end;
end;

constructor TTokenBase.Create;
begin
  FLine := -1;
  FColumn := -1;
  FPattern := EmptyStr;
  FTokenBaseType := ttUnknown;
end;

constructor TTokenBase.Create(Line: Integer; Col: Integer);
begin
  FLine := Line;
  FColumn := Col;
  FPattern := EmptyStr;
  FTokenBaseType := ttUnknown;
end;

procedure TTokenBase.Assign(Other: TTokenBase);
begin
  FLine := Other.Line;
  FColumn := Other.Column;
  FPattern := Other.Pattern;
  FTokenBaseType := Other.TokenBaseType;
end;

procedure TTokenBase.Reset;
begin
  FColumn := 0;
  FLine := 0;
  FPattern := EmptyStr;
  FTokenBaseType := ttUnknown;
end;

function TTokenBase.AsString: String;
begin
  Result := Format('Row: %-7d Col: %-7d Type: %-10s Pattern: %s', [FLine, FColumn, TokenTypeString, FPattern]);
end;

function TTokenBase.EndColumn: Integer;
begin
  Result := FColumn + Length(FPattern);
end;

{ TGTokenizer }

function TGTokenizer.ReloadBuffer: Boolean;
begin
  try
    Result := False;
    FEndPos := FFileStream.Read(FBuffer, MAX_BUFFER_SIZE) - 1;
    FBytesRead := FBytesRead + FEndPos;
    FBuffer[FEndPos + 1] := #0;
    FBeginPos := 0;
    FCurrPos := 0;

    if FEndPos >= 0 then
      Result := True;
  Except
    FHasNext := False;
    Result := False
  end;
end;

function TGTokenizer.EndOfFile: Boolean;
var
  Index: Int64;
begin
  Result := False;
  if FFileStream.Position < FFileStream.Size then
    Exit;

  Index := FCurrPos;
  while (IsWhiteSpace(FBuffer[Index]) and (FBuffer[Index] <> #0)) do
  begin
    inc(Index);
  end;

  if (FBuffer[Index] = #0) then
    Result := True;
end;

function TGTokenizer.IsDelimiter(AChar: Char): Boolean;
begin
  Result := False;
  if AChar in FDelims then
    Result := True;
end;

function TGTokenizer.IsWhiteSpace(AChar: Char): Boolean;
begin
  Result := False;
  if (AChar = #9) or
     (AChar = #13) or
     (AChar = #10) or
     (AChar = #32) then
    Result := True;
end;

function TGTokenizer.CheckBufferReload: Boolean;
var
  SeqDataStr: String;
begin
  Result := True;

  if (FEndPos < 0) then
  begin
    Result := ReloadBuffer;
    Exit;
  end
  else if FCurrPos >= (FEndPos + 1) then
  begin
    //Assert(FCurrPos <= FEndPos);
    {$IFDEF FPC}
    SetString(SeqDataStr, @FBuffer[FBeginPos], FCurrPos - FBeginPos);
    {$ELSE}
    SeqDataStr := Copy(FBuffer, FBeginPos, FCurrPos - FBeginPos);
//    SetString(SeqDataStr, @FBuffer[FBeginPos], FCurrPos - FBeginPos);
    {$ENDIF}
    FToken.Pattern := FToken.Pattern + Trim(SeqDataStr);
    Result := ReloadBuffer;
  end;
end;

// Precondition - it is already known that Pattern is not a delimiter.
function TGTokenizer.InferTokenType(Pattern: String): TTokenBaseType;
var
  TempInt: Int64;
  TempFloat: Double;
begin

  try
    if Trim(Pattern) = EmptyStr then
      Result := ttString
    else if (Pattern[1] = '.') or (Pattern[Length(Pattern)] = '.') then {trystrtofloat can be fooled}
      Result := ttString
    else if TryStrToInt64(Pattern, TempInt) then
      Result := ttInt
    else if TryStrToFloat(Pattern, TempFloat) then
      Result := ttReal
    else
      Result := ttString;
  Except
    Result := ttUnknown;
  end;
end;

function TGTokenizer.GetProgress: Int64;
begin
  Result := Min(100, Trunc(FBytesRead / FileSize * 100));
end;

constructor TGTokenizer.Create(AFileName: String; Delims: TDelimSet);
begin
  FBytesRead := 0;
  FDelims := Delims;
  FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  FileSize := FFileStream.Size;
  FSrcFile := AFileName;
  FBeginPos := 0;
  FCurrPos := 0;
  FEndPos := -1;
  FToken := TTokenBase.Create;
  FHasNext := True;
  FLine := 1;
  FColumn := 1;
  //FRegexInt := TRegExpr.Create;
  //FRegexInt.Expression := '^\d+$';
  //FRegexReal := TRegExpr.Create;
  //FRegexReal.Expression := '^([+-]?[0-9]+\.[0-9]+[eE][+-]?[0-9]{1,3})|([+-]?\.[0-9]+[eE][+-]?[0-9]{1,3})|([+-]?[0-9]+\.?[eE][+-]?[0-9]{1,3})|([+-]?[0-9]+\.[0-9]+)|([+-]?\.[0-9]+)|([+-]?[0-9]+\.?)$';
end;

destructor TGTokenizer.Destroy;
begin
  if Assigned(FFileStream) then
    FFileStream.Free;

  if Assigned(FToken) then
    FToken.Free;

  //if Assigned(FRegexInt) then
  //  FRegexInt.Free;
  //
  //if Assigned(FRegexReal) then
  //  FRegexReal.Free;

  //SetLength(FBuffer, 0); { TODO 1 -oglen : this is probably not needed }
  inherited Destroy;
end;

function TGTokenizer.ReloadFile: Boolean;
begin
  Result := False;
  try
    FFileStream.Position := 0;
    FBeginPos := 0;
    FCurrPos := 0;
    FEndPos := -1;
    FLine := 1;
    FColumn := 1;
    FHasNext := True;
    Result := True;
  except
    // just let it fail
  end;
end;

{ TODO 1 -oglen : if the file ends with whitespace, an empty token will be returned as the last token }
procedure TGTokenizer.NextToken(var Token: TTokenBase);
var
  TempStr: AnsiString;
  BufferReloadedOnDelimiter: Boolean;
begin
  try
    try
      BufferReloadedOnDelimiter := False;
      Token.Reset;
      FToken.Reset;

      // load the buffer if needed. If it fails, bail out.
        if not CheckBufferReload then
        begin
          FHasNext := False;
          Exit;
        end;

      // skip over any white space
      while IsWhiteSpace(FBuffer[FCurrPos])do
      begin
        if (FBuffer[FCurrPos] = #13) or (FBuffer[FCurrPos] = #10) then // encountered a new line
        begin
          {$IFDEF MSWINDOWS}
          if FBuffer[FCurrPos + 1] = #13 then{ TODO 1 -oglen : is this cross platform compatible? }
            inc(FCurrPos);
          {$ENDIF}
          inc(FLine);
          FColumn := 1;
        end
        else
          inc(FColumn);
        inc(FCurrPos);
        if not CheckBufferReload then
        begin
          FHasNext := False;
          Exit; // then either an error occurred or we have already reached the end of file
        end;
      end;
      FBeginPos := FCurrPos; // we start here, at the first non-whitespace character

      // now build up a token string
      FToken.Column := FColumn;
      FToken.Line := FLine;
      while True do
      begin
        // if we encounter whitespace, treat it like a delimiter
        if IsWhiteSpace(FBuffer[FCurrPos]) then
        begin
          if (FBuffer[FCurrPos] = #13) or (FBuffer[FCurrPos] = #10) then
          begin
            {$IFDEF MSWINDOWS}
            if FBuffer[FCurrPos + 1] = #13 then
              inc(FCurrPos);
            {$ENDIF}
            inc(FLine);
            FColumn := 1;
          end
          else
            inc(FColumn);
          inc(FCurrPos);
          Break;
        end;

        // if we encounter a user defined delimiter, break off the token
        if IsDelimiter(FBuffer[FCurrPos]) then
        begin
          if FBeginPos = FCurrPos then // the token will have just the delimiter
          begin
            FToken.Pattern := FBuffer[FCurrPos];
            FToken.TokenBaseType := ttDelim;
            inc(FCurrPos);
            inc(FColumn);
            FBeginPos := FCurrPos;
          end;
          Break;
        end;

        inc(FColumn);
        inc(FCurrPos);

        // check for end of buffer and eof
        if EndOfFile then
        begin
          FHasNext := False;
          Break;
        end;

        if not CheckBufferReload then
        begin
          FHasNext := False;
          Exit;
        end
        else if (FCurrPos = 0) and IsDelimiter(FBuffer[FCurrPos]) and (Trim(FToken.Pattern) <> EmptyStr) then
        begin
          BufferReloadedOnDelimiter := True;
          break;
        end;
      end; { while True do }
    Except
      FToken.TokenBaseType := ttUnknown;
    end;
  Finally
    if FToken.TokenBaseType <> ttDelim then
    begin
      if not BufferReloadedOnDelimiter then
      begin
        {$IFDEF FPC}
        SetString(TempStr, @FBuffer[FBeginPos], FCurrPos - FBeginPos);
        {$ELSE}
        TempStr := Copy(FBuffer, FBeginPos, FCurrPos - FBeginPos);
        {$ENDIF}
        FToken.Pattern := FToken.Pattern + Trim(TempStr);
      end;
    end;

    if FToken.TokenBaseType <> ttDelim then
      FToken.TokenBaseType := InferTokenType(FToken.Pattern);

    Token.Assign(FToken);
    FBeginPos := FCurrPos;
  end;
end;

end.
