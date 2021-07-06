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

unit mlexbase;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, KeywordConsts, MLongIntList;

type

  { TLexBase }

  TLexBase = class
  protected
    FStream:      TStream;
    FOrigin:      Longint;
    FBuffer:      PChar;
    FBufPtr:      PChar;
    FBufEnd:      PChar;
    FSourcePtr:   PChar;
    FSourceEnd:   PChar;
    FPrevLinePtr: PChar;   // needed to keep start of next line ptr
    FTokenPtr:    PChar;
    FStringPtr:   PChar;
    FSourceLine:  Integer; // Current line being parsed
    FSaveChar:    Char;
    FToken:       TSnTokenCode;
    FIsTokenPending: Boolean; // used by derived class to return the same Token
                              // Token once again
    LastChar:     Char;    // LastChar keeps char when token =snJustChar

    TokenLink:     Integer;      // used for assignment statements for var = option
    Keywords:      TStringList;  // read from file keywords.txt
    StringToToken: TStringList;  // read from file stringtotoken.txt
    SpecialSymbol: TSnTokenCode; // needed to specify if Nuc/Amino symbol expected

    LSrcPtr:       PChar;

    procedure ReadBuffer;
    procedure SkipBlanks;
    procedure Unwind;
    function  NextToken: TSnTokenCode;
    function  SourcePos: Longint;
    function  TokenFloat: Extended;
    function  TokenInt: Longint;
    function  TokenString: string;
    function  TokenSymbolIs(const S: string): Boolean;

    function  GetKeywordFileName: String;
    function  GetStringToTokenFileName: String;
    procedure ProcessStringToTokenList;

    procedure ReadAssignmentStatement(var CurVariable, CurOption: TSnTokenCode; var CurVariableLink: Integer);
    function  ReadSpecialStringToken(delimit: Char): TSnTokenCode;
    procedure CheckTokenSymbol(const S: string);
    function  ConvertStringToToken(const S: String): TSnTokenCode;
    function  GetTokenClass(const S: string): TSnTokenCode;

    procedure Error(Ident: Integer);
    procedure ErrorFmt(Ident: Integer; const Args: array of const);
    procedure ErrorStr(const Message: string);
    function FmtLoadStr(Ident: Integer; const Args: array of const): String;
    function  IsValidBase(x: Char): Boolean;  // local to LexBase

    property  SourceLine: Integer read FSourceLine;
    property  Token: TSnTokenCode read FToken;
    function  GetNextChar: Char;
    function  PeekNextChar: Char;

  protected
      //--------------- Important header information read in the header
    FFileName       : String;   // Name of the Input data File
    FTitle          : String;   // File Title
    FDescription    : String;   // File Description

    FMissingSym     : Char;          // Missing data symbol
    FIdenticalSym   : Char;
    FIndelSym       : Char;
      //--------------- Some general attributes
    FFoundFormat    : Boolean;       // if the format statment is found in the data file
    FDataType       : TSnTokenCode;  // any type of data
    FDataFormat     : TSnTokenCode;  // format could be anything
    FNoOfOtus       : LongInt;       // No. of Otus; or populations etc.
      //--------------- Lists of Different types of labels
    OtuLabels       : TStringList;   // A string list
    GpLabels        : TStringList;   // Group name for every OTU
    OtuGpIds        : TLongIntList;  // Indices to the Gpnames are stored here
      //------------- Display reading progress
    PercentPerAdvance  : LongInt;       // Percent adanvce upto now
    CurAdvance      : LongInt;       // Cur Adavance found
    FileSize        : LongInt;

    function  ValidateName(var S: String): Boolean;

    function  TokenFormatVariableIs(Variable: TSnTokenCode): Boolean;

    function GetTaxon(Index: Integer): String;
    function GetNoOfGps: Integer;
    function GetGpName(Index: Integer): String;

    function  GetColPosition: Integer;
    function  GetRowPosition: Integer;
    procedure SetFileName(Value: String);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure CheckDataType;  // this checkes everything;

    property FileName:     String write SetFileName;
    property Title       : String read FTitle write FTitle;
    property Description : String read FDescription write FDescription;
    property FoundFormat : Boolean read FFoundFormat;

    property DataType    : TSnTokenCode read FDataType write FDataType;
    property DataFormat  : TSnTokenCode read FDataFormat write FDataFormat;

    property MissingSym  : Char read FMissingSym write FMissingSym;
    property IdenticalSym: Char read FIdenticalSym write FIdenticalSym;
    property GapSym      : Char read FIndelSym write FIndelSym;

    property NoOfOtus    : LongInt read FNoOfOtus write FNoOfOtus;
    property Taxon[Index: Integer]: String read GetTaxon;

    property NoOfGps     : Integer read GetNoOfGps;
    property GpName[Index: Integer]  :String read GetGpName;

    property Row: Integer read GetRowPosition;
    property Col: Integer read GetColPosition;
  end;

implementation

uses
  MegaConsts,  math, Dialogs,  MegaUtils;

const
  ParseBufSize = 4096;

  constructor TLexBase.Create;
  begin
    inherited Create;

    Keywords      := nil;

    FStream         := nil;

    FFileName       := EmptyStr;
    FTitle          := 'Untitled';
    FDescription    := EmptyStr;
    FFoundFormat    := False;
    FDataType       := snNoToken;
    FDataFormat     := snNoToken;
    FIsTokenPending := False;

    FNoOfOtus       := 0;
    FMissingSym     := '?';

    OtuLabels       := nil;
    GpLabels        := nil;
    OtuGpIds        := nil;
    PercentPerAdvance  :=  0;
    CurAdvance      :=  0;
    FileSize        := -1;
    SpecialSymbol   := snNoToken;
  end;

  destructor TLexBase.Destroy;
  begin
    if FBuffer <> nil then
      FreeMem(FBuffer, ParseBufSize);
    Keywords.Free;
    StringToToken.Free;
    OtuLabels.Free;
    GpLabels.Free;
    if OtuGpIds <> nil then OtuGpIds.Destroy;
    if FStream <> nil then
      FStream.Free;
    Keywords        := nil;
    StringToToken   := nil;
    OtuLabels       := nil;
    GpLabels        := nil;
    OtuGpIds        := nil;
    inherited Destroy;
  end;

  procedure TLexBase.SetFileName(Value: String);
  begin
    try
      FFileName := Value;
      FStream := TFileStream.Create(FFileName, fmOpenRead or fmShareExclusive);
      GetMem(FBuffer, ParseBufSize);
      FBuffer[0] := #0;
      FBufPtr := FBuffer;
      FBufEnd := FBuffer + ParseBufSize;
      FSourcePtr := FBuffer;
      FSourceEnd := FBuffer;
      FTokenPtr := FBuffer;
      FPrevLinePtr := FBuffer;  // prevline ptr
      FSourceLine := 1;

      FileSize        := FStream.Size;
      PercentPerAdvance  := floor(FStream.Size/100)+1;
      CurAdvance      := 0;
    except
      raise;
    end;
  end;

  //---- Unwinds to the top of the data file
  procedure TLexBase.Unwind;
  begin
    FBuffer[0] := #0;
    FBufPtr := FBuffer;
    FBufEnd := FBuffer + ParseBufSize;
    FSourcePtr := FBuffer;
    FSourceEnd := FBuffer;
    FTokenPtr := FBuffer;
    FSourceLine := 1;
    CurAdvance  := 0;
    ReadBuffer; // and read the buffer
  end;

  function TLexBase.GetKeywordFileName: String;
  begin
    Result := GetPrivateDir+'Keywords.txt';
  end;

  function TLexBase.GetStringToTokenFileName: String;
  begin
    Result := GetPrivateDir+'StringToToken.txt';
  end;

  procedure TLexBase.ProcessStringToTokenList;
  var
    i: Integer;
  begin
    with StringToToken do
      for i := 0 to Count - 1 do
      begin
        if CompareStr(Values[Names[i]],'-1') <> 0 then
          StringToToken[i] := Names[i] + '=' + Values[Names[i]]
        else
          raise Exception.Create('String to token list is invalid');
          //StringToToken[i] := Names[i] + '=' + IntToStr(i+1); // setup the name
      end;
  end;

  //---- Reads buffer as and when needed; always called form skipblanks
  procedure TLexBase.ReadBuffer;
  var
    Count: Integer;
  begin
    Inc(FOrigin, FSourcePtr - FBuffer);
    FSourceEnd[0] := FSaveChar;
    Count := FBufPtr - FSourcePtr;
    if Count <> 0 then
      Move(FSourcePtr[0], FBuffer[0], Count);
    FBufPtr := FBuffer + Count;
    Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
    FSourcePtr := FBuffer;
    FSourceEnd := FBufPtr;
    if FSourceEnd = FBufEnd then
    begin
      FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
      if FSourceEnd = FBuffer then
        ErrorStr(SLineTooLong);
    end;
    FSaveChar := FSourceEnd[0];
    FSourceEnd[0] := #0;
  end;

  function  TLexBase.GetColPosition: Integer;
  begin
    Result :=  FSourcePtr - FPrevLinePtr;
  end;

  function  TLexBase.GetRowPosition: Integer;
  begin
    Result :=  FSourceLine -1;
  end;

  function TLexBase.GetTaxon(Index: Integer): String;
  begin
    Result := EmptyStr;
    if OtuLabels = nil then
      Exit;
    if (Index < 0) or (Index >= OtuLabels.Count) then
      ErrorStr(SListIndexError);
    Result := OtuLabels[Index];
  end;

  function TLexBase.GetNoOfGps: Integer;
  begin
    Result := 0;
    if GpLabels = nil then
      Exit;
    Result := GpLabels.Count;
  end;

  function TLexBase.GetGpName(Index: Integer): String;
  begin
    Result := EmptyStr;
    if (GpLabels = nil) or (GpLabels.Count = 0) then
      Exit;
    if (Index < 0) or (Index >= OtuLabels.Count) then
      ErrorStr(SListIndexError);
    if OtuGpIds = nil then
      Exit;
    if OtuGpIds[Index] > -1 then
      Result := GpLabels[OtuGpIds[Index]];
  end;

  //---- Reads an assignment statement completely
  procedure TLexBase.ReadAssignmentStatement(var CurVariable, CurOption: TSnTokenCode; var CurVariableLink: Integer);
  var
    TokenStr : String;
    TokenClass: TSnTokenCode;
    CurOptionLink: Integer;
  begin
    NextToken;
    TokenStr   := LowerCase(TokenString);
    TokenClass := GetTokenClass(TokenStr);
    if TokenClass <> snVariable then
       ErrorStr('An assignment statement needed.');
    CurVariable := ConvertStringToToken(TokenStr); // identify exact command type

    CurVariableLink := TokenLink;                  // Link needed for variable
    NextToken;
    CheckTokenSymbol('=');        // assignment symbol
    NextToken;
    CurOptionLink := 0;           // Get the option
    if CurVariableLink <= 0 then
      Exit;
    // otherwise an option type needed
    TokenClass := GetTokenClass(TokenString);
    if TokenClass <> snOption then
      ErrorStr('['+TokenString+'] is not a valid option for '+TokenStr+'.');
    CurOptionLink := TokenLink;
    if (CurOptionLink+CurVariableLink) <> 0 then
      ErrorStr('['+TokenString+'] is not a valid option for '+TokenStr+'.');
    CurOption := ConvertStringToToken(LowerCase(TokenString));
  end;

  //---- Checks the file for datatype information
  procedure TLexBase.CheckDataType;
  var
    CurVariable: TSnTokenCode = snNoToken;
    CurOption: TSnTokenCode = snNoToken;
    CurVariableLink: Integer = -1;
  begin
    Unwind;       //--- Unwind
    try
      if Keywords = nil then
      begin
        Keywords := TStringList.Create;
        Keywords.LoadFromFile(GetKeywordFilename); // this needs to be resource
      end;
      if StringToToken = nil then
      begin
        StringToToken := TStringList.Create;
        StringToToken.LoadFromFile(GetStringToTokenFilename); // this needs to be resource
        ProcessStringToTokenList;
      end;
    except
      On E: Exception do
      begin
        ErrorStr('MEGA is unable to find important program subdirectories.  Please re-install MEGA.');
        Exit;
      end;
    end;

    //--- Read #MEGA
    try
      NextToken; CheckTokenSymbol('#');
      NextToken; CheckTokenSymbol('MEGA');
    except
      on E: Exception do
      begin
        ErrorStr('Input data file must contain #mega.');
        Exit;
      end;
    end;

    NextToken;  FIsTokenPending := True;
    // previous version of MEGA has Title right after #mega
    if TokenSymbolIs('Title') then
    begin
      FIsTokenPending := False;
      ReadSpecialStringToken(#10);
      FTitle := TokenString;
    end;

    while True do  // read until valid commands are found
    begin
      NextToken;
      if Token <> snCommand then
      begin
        FIsTokenPending := True;
        Exit;
      end;
      NextToken;
      if TokenSymbolIs('Title') then
      begin
        ReadSpecialStringToken(']');
        FTitle := TokenString;
      end
      else if TokenSymbolIs('Description') then
      begin
        ReadSpecialStringToken(']');
        FDescription := TokenString;
      end
      else if TokenSymbolIs('Format') then
      begin
        if FFoundFormat = True then
          ErrorStr('Duplicate Format command found');
        FFoundFormat := True;
        while True do
        begin
        NextToken;  FIsTokenPending := True;

          if TokenSymbolIs(']') then
            Break;                  // format statement over
          if TokenSymbolIs('#') then
          ErrorStr('Unexpected end of Format statement or invalid # sign.');

          //--- Read statement-by-statement
          ReadAssignmentStatement(CurVariable, CurOption, CurVariableLink);
          TokenFormatVariableIs(CurVariable);           // Check for inclusion in format
          if CurVariable = snDataType then
          begin
            FDataType := CurOption;
            Exit;
          end;
        end; // end of TokenSymbolIs('Format')
      end
      else  // the command has to be associated with some data so don't worry
        Exit;
    end; // end of while
  end;

  procedure TLexBase.CheckTokenSymbol(const S: string);
  begin
    if not TokenSymbolIs(S) then
      ErrorStr(SSymbolExpected + ': ' + S);
  end;

  procedure TLexBase.Error(Ident: Integer);
  begin
    ErrorStr(LoadStr(Ident));
  end;

  procedure TLexBase.ErrorFmt(Ident: Integer; const Args: array of const);
  begin
    ErrorStr(FmtLoadStr(Ident, Args));
  end;

  procedure TLexBase.ErrorStr(const Message: string);
  begin
    raise EParserError.Create(Format('%s: %s - Line %d', [SParseError, Message, FSourceLine]));
  end;

  function TLexBase.FmtLoadStr(Ident: Integer; const Args: array of const): String;
  begin
    Result := EmptyStr;
    ErrorStr(FmtLoadStr(Ident, Args)); { is this an endless recursion?!}
  end;

  function TLexBase.GetTokenClass(const S: string): TSnTokenCode;
  var
    x: String;
  begin
    try
      x := Keywords.Values[S];
      if x = '' then
      begin
        Result := snSymbol;
        TokenLink := MaxInt;
        Exit;
      end;
      TokenLink := StrToInt(x);
      if TokenLink < 0 then
        Result := snOption
      else
        Result := snVariable;
    except
      On E: Exception do raise;
    end;
  end;

  //--- Returns the position of source where we are
  function TLexBase.SourcePos: Longint;
  begin
    Result := FOrigin + (FTokenPtr - FBuffer);
  end;

  //--- Returns a Float token
  function TLexBase.TokenFloat: Extended;
  begin
    Result := StrToFloat(TokenString);
  end;

  //--- Returns an Integer token
  function TLexBase.TokenInt: Longint;
  begin
    Result := StrToInt(TokenString);
  end;

  //--- Returns a String token
  function TLexBase.TokenString: string;
  var
    L: Integer;
  begin
    if FToken = snString then L := FStringPtr - FTokenPtr
                         else L := FSourcePtr - FTokenPtr;
    SetString(Result, FTokenPtr, L);
  end;

  //--- Compares a string to the current token string; No Exceptions are thrown
  function TLexBase.TokenSymbolIs(const S: string): Boolean;   // insensitive to case
  begin
    Result := (Token = snSymbol) and (CompareText(S, TokenString) = 0);
  end;

  function TLexBase.ConvertStringToToken(const S: String): TSnTokenCode;
  var
    x, y: String;
  begin
    Result := snNoToken;
    y := LowerCase(S);
    try
      x := StringToToken.Values[y];
      if Length(x) < 1 then
        Exit;
      Result := StringToTokenCode(x);
    except
      On E: Exception do; // kill the exception
    end;
  end;

  function TLexBase.TokenFormatVariableIs(Variable: TSnTokenCode): Boolean;
  begin
    case Variable of
      snDataType, snTreeType,
      snNOtus, snNTrees, snNSites,
      snDataFormat, snTreeFormat,
      snConfidenceName, snBLenName,
      snPValueName,
      snNodeFreqName,
      snStdErrName,
      snIndel, snMissing, snIdentical:
        Result := True;
    else
      ErrorStr('Invalid type specifier in the #Format statement.');
    end;
  end;

  //==== Validates the name; I may already have stuff for it
  function TLexBase.ValidateName(var S: String): Boolean;
  var
    L: Integer;
  begin
    Result := False;
    TrimTaxaName(S);  // trims all leading and trailing junk
    L := Length(S);
    if L = 0 then
      ErrorStr('Name is zero character long');
  end;

  function TLexBase.IsValidBase(x: Char): Boolean;
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
            'A'..'I','K'..'N', 'P'..'T', 'V'..'Z': Result := True;
          end;
      end;
  end;

  function TLexBase.GetNextChar: Char;
  begin
    Result := LSrcPtr^;
    case LSrcPtr^ of
      #0:
          begin
            FSourcePtr := LSrcPtr;
            if FStream.Size = FStream.Position then
            begin
              Result := toEOF;
              Exit;
            end;
            ReadBuffer;
            LSrcPtr := FSourcePtr;
            Result := LSrcPtr^;
          end;
      #10:
          begin
            Inc(FSourceLine);
            FPrevLinePtr := FSourcePtr;
            Inc(CurAdvance);
            Inc(LSrcPtr);
          end;
    else
      Inc(LSrcPtr);
    end;
    //if FProgressForm <> nil then
    //begin
      //if FProgressForm.ShouldStop then
      //  raise Exception.Create('User aborted process...');
      Inc(CurAdvance);
      if CurAdvance > PercentPerAdvance then
      begin
        //FProgressForm.AddProgressFn(CurAdvance div PercentPerAdvance);
        CurAdvance := CurAdvance mod PercentPerAdvance;
      end;
    //end;
  end;

  function TLexBase.PeekNextChar: Char;
  begin
    Result := LSrcPtr^;
  end;

  procedure TLexBase.SkipBlanks;
  begin
    // skip blanks; and get the next char
    while True do
      case PeekNextChar of
        #33..#255: Break;
        #0:  // next char is 0
           if GetNextChar = #0 then
             Exit;
      else
        GetNextChar;
      end;
  end;

  function TLexBase.ReadSpecialStringToken(delimit: Char): TSnTokenCode;
  var
    x: Char;
  begin
    LSrcPtr := FSourcePtr;
    skipBlanks;
    Result    := snNoToken;
    FTokenPtr := LSrcPtr;
    FStringPtr := LSrcPtr;
    while True do  // needed for multiple adjacent comments etc.
    begin
      x := PeekNextChar;
      if x = delimit then
      begin
        FStringPtr := LSrcPtr-1;
        GetNextChar;
        break;
      end;

      if x = toEOF then
        ErrorStr(SInvalidString);
      GetNextChar;
    end;
    FSourcePtr := LSrcPtr;
    Result := snString;
    FToken := Result;
  end;

  //--- Gets all kinds of Token
  function TLexBase.NextToken: TSnTokenCode;
  var
    OpenBraces: Integer;
    x: Char;
  begin
    if FIsTokenPending then
    begin
      FIsTokenPending := False;
      Exit;
    end;

    LSrcPtr := FSourcePtr;
    skipBlanks;
    Result    := snNoToken;

    while True do  // needed for multiple adjacent comments etc.
    begin
      skipBlanks;
      FTokenPtr := LSrcPtr;
      x := GetNextChar;
      case x of
        '[':
          begin
            case GetNextChar of
              '!': Result := snCommand;
              ']': GetNextChar; // comment is already over
            else
              begin
                OpenBraces:=1;
                while True do   // first do the comment (all even concatenated ones)
                begin
                  case GetNextChar of
                    '[' : Inc(OpenBraces);
                    ']' : Dec(OpenBraces);
                    toEOF: ErrorStr('Comment never ended.');
                  end;
                  if OpenBraces = 0 then
                    break;  // when it falls x is still valid
                end;
              end;
            end;
          end;
         ']', '=', '#': Result := snSymbol;
      else
        break;
      end;
      if Result <> snNoToken then
        break;
    end;

    if (Result =snNoToken) and (SpecialSymbol <> snNoToken) then
    begin
      case SpecialSymbol of
        snNucleotide, snProtein:
          begin
            LastChar := x;
            if not IsValidBase(LastChar) then Result := snInvalidBase
            else                              Result := snBase;
          end;
        snRestrictionSite, snRFLP, snRAPD:
          begin
            LastChar := x;
            if LastChar in ['+','-','1','0', FMissingSym] then
              Result := snBase
            else Result := snInvalidBase;
          end;
      end;
    end;

    //  now we can do the rest
    if (Result =snNoToken) then
    case x of
      'A'..'Z',
      'a'..'z':
         begin
           while PeekNextChar in ['A'..'Z','a'..'z','0'..'9','_','{', '}','-','.','(',')'] do
              GetNextChar;
           Result := snSymbol;
         end;
      '-', '0'..'9':  // negative values are allowed
         begin
           if x = '-' then
           begin
             LastChar := x;
             Result := snJustChar;
           end
           else
             Result := snInteger;

           while PeekNextChar in ['0'..'9'] do  //---- It is an integer
           begin
             GetNextChar;
             Result := snInteger;  // to ensure this happens
           end;

           if PeekNextChar = '.' then
           begin
             GetNextChar;
             Result := snFloat;
             while PeekNextChar in ['0'..'9'] do // building further on float if applicable
               GetNextChar;
           end;

           case PeekNextChar of  //---- Check if a symbol may be residing
             'A'..'Z', 'a'..'z':
             begin
               while PeekNextChar in ['A'..'Z','a'..'z','0'..'9','_','{', '}','-','.','(',')'] do
                 GetNextChar;
               Result := snSymbol;
             end;
           end;
         end;
      else
        begin
          LastChar := x;
          Result := snJustChar;
        end;
    end; // end of reading again
    FToken := Result;
    FSourcePtr := LSrcPtr;
  end;

end.


