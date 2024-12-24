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

unit MTokenizer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Forms, Classes, SysUtils, ComCtrls, stdctrls, buttons,
  KeywordConsts, MegaConsts, MRuntimeProgressDlg, mgeographical_info;

{ TTokenizer is based on the TParser copied from Delphi 4.02}

type
  TTUpdateOptProc = procedure(Variable, Option: AnsiString) of object;
  TTUpdateDefProc = procedure of object;

type
  TTokenizer = class(TObject)
  private
    FStream: TStream;
    FOrigin: Longint;
    FBuffer: PAnsiChar;
    FBufPtr: PAnsiChar;
    FBufEnd: PAnsiChar;
    FSourcePtr: PAnsiChar;
    FSourceLine: Integer;
    FSourceEnd: PAnsiChar;
    FTokenPtr: PAnsiChar;
    FSaveChar: AnsiChar;
    FToken: AnsiChar;
    FFloatType: AnsiChar;
    FWideStr: WideString;
    function GetGeographicalInfo(Index: Integer): TGeographicalInfo;
    procedure ClearGeographicalInfo;
    function ParseLongLabelValue(aKey: AnsiString; tempLabel: AnsiString): AnsiString;
  protected
    FGeographicalInfoList: TGeographicalInfoList;
    IsOutGroup: Boolean;

    procedure ParseNewOtuLabelCommand(const AFullLabel: PAnsiChar); // the new command format supports key value pairs (e.g. IsOutgroup=True)
    procedure ParseGroupName(ACommandStr: AnsiString; Start: Integer);
    procedure ParseSpeciesName(ACommandStr: AnsiString; Start: Integer);
    procedure ParsePopulationName(ACommandStr: AnsiString; Start: Integer);
    procedure ParseOutgroup(ACommandStr: AnsiString; Start: Integer);
  public
    FullLabel, OtuLabel, GpLabel, SpLabel, PopLabel: AnsiString;
    ContinentLablel, CountryLabel, CityLabel, YearLabel, MonthLabel, DayLabel, TimeLabel: AnsiString;
    FKeywds  : TStringList; // read in based on user's comments
    FUpdateOptProc: TTUpdateOptProc; //set this to the derived class
    FUpdateDefProc: TTUpdateDefProc;
    FNeedSymbol: AnsiChar; //--------------- this controls how long a token must be
    FForceLen: Boolean;
    FTokenLen: Integer;
    FIsTokenPending: Boolean;
    FStringToken: AnsiString;
    FDetectNewLine: Boolean;
    FFileName       : String;   // Name of the Input data File
    FTitle          : String;   // File Title
    FDescription    : String;   // File Description
    FMissingSym     : AnsiChar;          // Missing data symbol
    FIdenticalSym   : AnsiChar;
    FIndelSym       : AnsiChar;
    FFoundFormat    : Boolean;       // if the format statment is found in the data file
    FDataType       : TSnTokenCode;  // any type of data
    FDataFormat     : TSnTokenCode;  // format could be anything
    FNoOfOtus       : LongInt;       // No. of Otus; or populations etc.
    FOptions : TStringList;  // keeps all the options in a list
    FTaxa    : TStringList;
    FGps     : TStringList;
    FSps     : TStringList;
    FPops    : TStringList;

    FOutGroups: TBoolArray;
    {$IFDEF VISUAL_BUILD}
    FGauge           : TProgressBar;  // no TGauge in LCL
    {$ENDIF}
    FCaption         : TEdit;   // caption
    FStopBtn         : TSpeedButton; // to detect stop
    PercentPerAdvance: LongInt; // Percent advance per increment
    CurAdvance       : LongInt; // Cur Adavance found
    FileSize         : LongInt;
    FPrevLinePtr     : PAnsiChar;   // needed to keep start of next line ptr
    LSrcPtr          : PAnsiChar;

    constructor Create; //(Stream: TStream);  // SK: Do not Use
    destructor Destroy; override;

    procedure ReadOtuLabel;
    procedure ParseOtuLabelCommand(const AFullLabel: PAnsiChar); // Label may contain gp Info
    procedure ParseOtuLabelCommandShort(const AFullLabel: PAnsiChar); { this has the form group|species|population}
    procedure ParseOtuLabelCommandLong(const AFullLabel: PAnsiChar); { this has the form group=gpName; species=spName; population=popName;}

    function  ValidateName(var S: AnsiString): Boolean;

    procedure SetKeywords(KeywdFile: AnsiString);
    procedure SetDataFile(DataFile: AnsiString);
    procedure SetDataFromStringList(aList: TStringList);
    procedure SetRuntimeProgress(ARPin : TRuntimeProgress);
    procedure SetProgressCaption(ProgressCaption: TEdit);
    procedure SetStopButton(StopBtn: TSpeedButton);
    procedure ReadBuffer;
    procedure SkipBlanks;
    procedure SkipUntilChar(x: AnsiChar);
    procedure Unwind;
    function  PeekNextChar: AnsiChar;
    function  GetNextChar: AnsiChar;
    procedure CheckToken(T: AnsiChar);
    procedure CheckTokenSymbol(const S: AnsiString);
    procedure Error(const Ident: AnsiString);
    procedure ErrorFmt(const Ident: AnsiString; const Args: array of const);
    procedure ErrorStr(const Message: AnsiString);
	  procedure ErrorLimitationStr(const Message: AnsiString);
    function NextToken: AnsiChar;
    function SourcePos: Longint;
    function TokenComponentIdent: AnsiString;
    function TokenFloat: Extended;
    function TokenInt: Longint;
    function TokenString: AnsiString;
    function TokenWideString: WideString;
    function TokenSymbolIs(const S: AnsiString): Boolean;
    function TokenSymbolIsFloat(const S: AnsiString): Boolean;


    function GetStringToken(Delimitor: AnsiChar): AnsiChar;

    function TokenAsInt: Integer;
    function TokenAsFloat: Extended;
                   //#10 for comment, ; for string
    procedure ReadAssignmentStatement(var CurVar, OptStr: AnsiString);
    function  IsValidVariable(AVar: AnsiString): Boolean;
    function  IsValidOption(AVar, AOpt: AnsiString): Boolean;

    function GetTaxon(Index: Integer): AnsiString;
    function GetNoOfGps: Integer;
    function GetNoOfSps: Integer;
    function GetNoOfPops: Integer;
    function GetGpName(Index: Integer): AnsiString;
    function GetSpName(Index: Integer): AnsiString;
    function GetPopName(Index: Integer): AnsiString;
    function GetIsOutgroupMember(Index: Integer): Boolean;

    function  GetColPosition: Integer;
    function  GetRowPosition: Integer;

    procedure CheckDataType;  // this is needed to check for datatype before specific reading
    procedure ReadFormat;            // Reads Format statement completely

              // they need to be set by the descendant class
    property UpdateOptProc: TTUpdateOptProc read FUpdateOptProc write FUpdateOptProc;
    property UpdateDefProc: TTUpdateDefProc read FUpdateDefProc write FUpdateDefProc;
    property Title       : String read FTitle write FTitle;
    property Description : String read FDescription write FDescription;
    property FoundFormat : Boolean read FFoundFormat;
    property DataType    : TSnTokenCode read FDataType write FDataType;
    property DataFormat  : TSnTokenCode read FDataFormat write FDataFormat;
    property MissingSym  : AnsiChar read FMissingSym write FMissingSym;
    property IdenticalSym: AnsiChar read FIdenticalSym write FIdenticalSym;
    property GapSym      : AnsiChar read FIndelSym write FIndelSym;
    property NoOfOtus    : LongInt read FNoOfOtus write FNoOfOtus;
    property Taxon[Index: Integer]: AnsiString read GetTaxon;
    property NoOfGps     : Integer read GetNoOfGps;
    property GpName[Index: Integer]:AnsiString read GetGpName;
    property SpName[Index: Integer]:AnsiString read GetSpName;
    property PopName[Index: Integer]:AnsiString read GetPopName;
    property IsOutgroupMember[Index: Integer]: Boolean read GetIsOutgroupMember;
    property Row: Integer read GetRowPosition;
    property Col: Integer read GetColPosition;
    property FloatType: AnsiChar read FFloatType;
    property SourceLine: Integer read FSourceLine;
    property Token: AnsiChar read FToken;
    property FileName : AnsiString read FFileName;   // Name of the Input data File
    property GeographicalInfo[Index: Integer]: TGeographicalInfo read GetGeographicalInfo;
  end;

const
  toPound      = '#';

  toComment    = '[';
  toCommentEnd = ']';

  toCommentMega1 = '"';

  toCommand    = '!';
  toCommandEnd = ';';

  toEqual      = '=';
  toGroup      = '{';
  toGroupEnd   = '}';

  toNumber     = '0';
  toOtuName    = 'o';
  toChar       = 'c';

implementation

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF}
  TypInfo, Dialogs, MegaUtils, ErrorMessages_HC, MGlobalSettings, StrUtils;

const
  {$IFDEF WIN64}
  ParseBufSize = 200000;
  {$ELSE}
//  ParseBufSize = 32768;
  ParseBufSize = 100000;
  {$ENDIF}



{
Note on Keyword file: Options=variable format must be followed.
}

{Functions added by SK}
constructor TTokenizer.Create; // non stream create
begin
  FStream := nil; //stream is now initialized as null
  GetMem(FBuffer, ParseBufSize);
  FBuffer[0] := #0;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + ParseBufSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;
  FSourceLine := 1;

  FKeywds  := TStringList.Create;
  FOptions := TStringList.Create;
  FTaxa    := TStringList.Create;
  FGps     := TStringList.Create;
  FSps     := TStringList.Create;
  FPops     := TStringList.Create;
  FGeographicalInfoList := TGeographicalInfoList.Create;
  SetLength(FOutGroups, 0);
  {$IFDEF VISUAL_BUILD}
  FGauge   := nil;
  {$ENDIF}
  FCaption := nil;
  FStopBtn := nil;

  FFileName       := EmptyStr;
  FTitle          := 'Untitled';
  FDescription    := EmptyStr;
  FFoundFormat    := False;
  FIsTokenPending := False;
  FDetectNewLine  := False;

  FNoOfOtus       := 0;
  FMissingSym     := '?';
  PercentPerAdvance  :=  0;
  CurAdvance      :=  0;
  FileSize        := -1;
  FullLabel := EmptyStr;
  GpLabel := EmptyStr;
  SpLabel := EmptyStr;
  PopLabel := EmptyStr;
  IsOutgroup := False;
end;

procedure TTokenizer.SetKeywords(KeywdFile: AnsiString);
begin
  FKeywds.Clear;
  FKeywds.LoadFromFile(KeywdFile);
  FOptions.Add('Keywords='+KeywdFile); // set the options
end;

procedure TTokenizer.SetDataFile(DataFile: AnsiString);
begin
  if FStream <> nil then
    FStream.Free;
  FStream := nil;
  try
    FFileName := DataFile;
    FStream := TFileStream.Create(DataFile, fmOpenRead or fmShareDenyNone);
    FTokenPtr := FBuffer;
    FPrevLinePtr := FBuffer;  // prevline ptr
    FSourceLine := 1;
    FileSize := FStream.Size;
    PercentPerAdvance  := FStream.Size div 100 +1;
    CurAdvance      := 0;
    FOptions.Clear;
    FOptions.Add('Filename='+DataFile); // set the options
  except
    on E: Exception do
    begin
      if FStream <> nil then
      begin
        FStream.Free;
        FStream := nil;
      end;
      raise;
    end;
  end;
end;

procedure TTokenizer.SetDataFromStringList(aList: TStringList);
begin
  if FStream <> nil then
    FStream.Free;
  FStream := nil;
  try
    FFileName := EmptyStr;
    FStream := TMemoryStream.Create;
    aList.SaveToStream(FStream);
    FTokenPtr := FBuffer;
    FPrevLinePtr := FBuffer;
    FSourceLine := 1;
    FileSize := FStream.Size;
    PercentPerAdvance := FStream.Size div 100 +1;
    CurAdvance := 0;
    FOptions.Clear;
    FOptions.Add('Filename=none'); // set the options
    FStream.Position := 0;
  except
    on E: Exception do
    begin
      if FStream <> nil then
      begin
        FStream.Free;
        FStream := nil;
      end;
      raise;
    end;
  end;
end;

procedure TTokenizer.SetProgressCaption(ProgressCaption: TEdit);
begin
  FCaption := ProgressCaption;
  FCaption.Text := EmptyStr;
end;

procedure TTokenizer.SetStopButton(StopBtn: TSpeedButton);
begin
  FStopBtn := StopBtn;
  FStopBtn.AllowAllUp := True;
  FStopBtn.GroupIndex := High(Integer);
end;

destructor TTokenizer.Destroy;
begin
  if FBuffer <> nil then
    FreeMemAndNil(FBuffer, ParseBufSize);
  FStream.Free;
  FStream := nil;
  FreeAndNil(FKeywds);
  FreeAndNil(FTaxa);
  FreeAndNil(FGps);
  FreeAndNil(FSps);
  FreeAndNil(FPops);
  FreeAndNil(FOptions);
  SetLength(FOutgroups, 0);
  if Assigned(FGeographicalInfoList) then
  begin
    ClearGeographicalInfo;
    FGeographicalInfoList.Free;
  end;
  inherited Destroy;
end;

function TTokenizer.GetColPosition: Integer;
begin
  Result :=  FSourcePtr - FPrevLinePtr;
end;

function TTokenizer.GetRowPosition: Integer;
begin
  Result :=  FSourceLine -1;
end;

function TTokenizer.GetTaxon(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if FTaxa = nil then
    Exit;
  if (Index < 0) or (Index >= FTaxa.Count) then
    Error(SListIndexError);
  Result := FTaxa[Index];
end;

function TTokenizer.GetNoOfGps: Integer;
var
  AStrList:  TStringList;
  i: Integer;
begin
  Result := 0;
  if FGps = nil then
    Exit;

  AStrList := nil;
  try
    AStrList := TStringList.Create;
    AStrList.Sorted := True;
    AStrList.Duplicates := dupIgnore;

    for i:=0 to FGps.Count-1 do
      AStrList.Add(FGps[i]);

    Result := AStrList.Count;
  finally
    AStrList.Free;
  end;
end;

function TTokenizer.GetNoOfSps: Integer;
var
  AStrList:  TStringList;
  i: Integer;
begin
  Result := 0;
  if FSps = nil then
    Exit;

  AStrList := nil;
  try
    AStrList := TStringList.Create;
    AStrList.Sorted := True;
    AStrList.Duplicates := dupIgnore;

    for i:=0 to FSps.Count-1 do
      AStrList.Add(FSps[i]);

    Result := AStrList.Count;
  finally
    AStrList.Free;
  end;
end;

function TTokenizer.GetNoOfPops: Integer;
var
  AStrList:  TStringList;
  i: Integer;
begin
  Result := 0;
  if FPops = nil then
    Exit;

  AStrList := nil;
  try
    AStrList := TStringList.Create;
    AStrList.Sorted := True;
    AStrList.Duplicates := dupIgnore;

    for i:=0 to FPops.Count-1 do
      AStrList.Add(FPops[i]);

    Result := AStrList.Count;
  finally
    AStrList.Free;
  end;
end;


function TTokenizer.GetGpName(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if (FGps = nil) or (FGps.Count = 0) then
    Exit;
  if (Index < 0) or (Index >= FTaxa.Count) then
    Error(SListIndexError);
  Result := FGps[Index];
end;

function TTokenizer.GetIsOutgroupMember(Index: Integer): Boolean;
begin
  Result := False;
  if Index < Length(FOutgroups) then
    Result := FOutgroups[Index];
end;

function TTokenizer.GetSpName(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if (FSps = nil) or (FSps.Count = 0) then
    Exit;
  if (Index < 0) or (Index >= FTaxa.Count) then
    Error(SListIndexError);
  Result := FSps[Index];
end;

function TTokenizer.GetPopName(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if (FPops = nil) or (FPops.Count = 0) then
    Exit;
  if (Index < 0) or (Index >= FTaxa.Count) then
    Error(SListIndexError);
  Result := FPops[Index];
end;

//---- Unwinds to the top of the data file
procedure TTokenizer.Unwind;
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

//=== Checks data type before specific data type reading can begin
procedure TTokenizer.CheckDataType;
var
  AVar: AnsiString = '';
  AOpt: AnsiString = '';
begin
  Unwind;       //--- Unwind
  FForceLen := False;
  FTokenLen := -1;

  //--- Read #MEGA
  NextToken;
  CheckTokenSymbol('#');
  NextToken;
  CheckTokenSymbol('MEGA');

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
      FIsTokenPending := True;
      Break;
    end;

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
      FIsTokenPending := False;
      //
      while True do
      begin
        NextToken;
        if Token = toCommandEnd then
          Break;                  // format statement over
        if TokenSymbolIs('#') then
          ErrorStr('Unexpected end of Format statement or invalid # sign.');
        ReadAssignmentStatement(AVar,AOpt);
        if CompareText(AVar, 'datatype') = 0 then
        begin
           if      CompareText(AOpt, 'DNA')        = 0 then FDataType := snNucleotide
           else if CompareText(AOpt, 'RNA')        = 0 then FDataType := snNucleotide
           else if CompareText(AOpt, 'Nucleotide') = 0 then FDataType := snNucleotide
           else if CompareText(AOpt, 'Protein')    = 0 then FDataType := snProtein
           else if CompareText(AOpt, 'distance')       = 0 then FDataType := snDistance
           else if CompareText(AOpt, 'genotypefreq')   = 0 then FDataType := snGenotypeFreq
           else if CompareText(AOpt, 'microsatellites')= 0 then FDataType := snMicrosatellites
           else if CompareText(AOpt, 'allelefreq')     = 0 then FDataType := snAlleleFreq
           else if CompareText(AOpt, 'restrictionsite')= 0 then FDataType := snRestrictionSite
           else if CompareText(AOpt, 'rflp')           = 0 then FDataType := snRFLP
           else if CompareText(AOpt, 'rapd')           = 0 then FDataType := snRAPD
           else    Error(SInvalidProperty);
        end;
        if FDataType <> snNoToken then // leave as soon as datatype found
          Exit;
      end;
      FFoundFormat := True;
    end
    else  // the command has to be associated with some data so don't worry
    begin
      FIsTokenPending := True;
      Break;
    end;
  end; // end of while
end;

//===
//---- Reads format statement completely
procedure TTokenizer.ReadFormat;
var
  AVar: AnsiString = '';
  AOpt: AnsiString = '';
begin
  NextToken;
  if not TokenSymbolIs('Format') then
    ErrorStr('Invalid call to read format');

  if FCaption <> nil then
     FCaption.Text := 'Reading Format statement';

  while True do
  begin
    NextToken;
    if Token = toCommandEnd then
      Break;                  // format statement over
    if TokenSymbolIs('#') then
      ErrorStr('Unexpected end of Format statement or invalid # sign.');
    ReadAssignmentStatement(AVar,AOpt);
    FUpdateOptProc(AVar, AOpt);
//    FOptions.Add(AVar+'='+AOpt);
  end;
  FUpdateDefProc;
end;

//=================
procedure TTokenizer.ReadOtuLabel;
var
  ALabel: AnsiString;
begin
  FNeedSymbol := toOtuName;
  NextToken;

  // check if we again see the command.  that is not allowed
  if Token = toCommand then
    ErrorStr('Two consecutive Command statements are not allowed');

  if TokenSymbolIs('#') then
    NextToken; // otherwise error is flagged

  while Token = toComment do
  begin
    GetStringToken(toCommentEnd);
    NextToken;
  end;
  while Token = toCommentMEGA1 do
  begin
    GetStringToken(toCommentMEGA1);
    NextToken;
  end;

  OtuLabel := EmptyStr;
  GpLabel  := EmptyStr;
  SpLabel := EmptyStr;
  PopLabel := EmptyStr;
  ContinentLablel := EmptyStr;
  CountryLabel := EmptyStr;
  CityLabel := EmptyStr;
  YearLabel := EmptyStr;
  MonthLabel := EmptyStr;
  DayLabel := EmptyStr;
  TimeLabel := EmptyStr;
  FullLabel := EmptyStr;
  ALabel := TokenString;
  IsOutgroup := False;
  ParseOtuLabelCommand(PAnsiChar(ALabel));

  TrimTaxaName(OtuLabel);
  if Length(GpLabel) > 0 then
    TrimTaxaName(GpLabel);
  if Length(SpLabel) > 0 then
    TrimTaxaName(SpLabel);
  if Length(PopLabel) > 0 then
    TrimTaxaName(PopLabel);
  if Length(ContinentLablel) > 0 then
    TrimTaxaName(ContinentLablel);
  if Length(CountryLabel) > 0 then
    TrimTaxaName(CountryLabel);
  if Length(CityLabel) > 0 then
    TrimTaxaName(CityLabel);
  FNeedSymbol := #0;
end;

procedure TTokenizer.ParseGroupName(ACommandStr: AnsiString; Start: Integer);
var
  i, j: Integer;
begin
  GpLabel := EmptyStr;
  i := PosEx('.', ACommandStr, Start) + 1;
  j := PosEx('|', ACommandStr, Start);
  if j = 0 then
    j := PosEx('}', ACommandStr, Start);
  if j = 0 then
    ErrorStr('Invalid group specification in the otu label command');
  GpLabel := Trim(Copy(ACommandStr, i, j - i));
end;

function TTokenizer.GetGeographicalInfo(Index: Integer): TGeographicalInfo;
begin
  if (Index >= 0) and (Index < FGeographicalInfoList.Count) then
    Result := FGeographicalInfoList[Index]
  else
    Result := nil;
end;

procedure TTokenizer.ClearGeographicalInfo;
var
  i: Integer;
begin
  if Assigned(FGeographicalInfoList) then
  begin
    if FGeographicalInfoList.Count > 0 then
      for i := 0 to FGeographicalInfoList.Count - 1 do
        if Assigned(FGeographicalInfoList[i]) then
          FGeographicalInfoList[i].Free;
    FGeographicalInfoList.Clear;
  end;
end;

function TTokenizer.ParseLongLabelValue(aKey: AnsiString; tempLabel: AnsiString): AnsiString;
var
  front: Integer = -1;
  back: Integer = -1;
begin
  Result := EmptyStr;
  front := Pos(aKey + MEGA_COMMAND_DELIMITER, LowerCase(tempLabel));
  if front > 0 then
  begin
    Back := PosEx('|', tempLabel, Front) - 1;
    if Back <= 0 then { the pipe is not required for the last command}
      Back := Length(tempLabel);
    front := PosEx(MEGA_COMMAND_DELIMITER, tempLabel, front) + 1;
    Result := Copy(tempLabel, front, Back - front + 1);
  end;
end;

procedure TTokenizer.ParseNewOtuLabelCommand(const AFullLabel: PAnsiChar);
var
  OpenBrace: PAnsiChar;
  CloseBrace: PAnsiChar;
  TempLabel: AnsiString;
  LCase: AnsiString;
begin
  OpenBrace := StrPos(AFullLabel, '{');
  CloseBrace := StrPos(AFullLabel, '}');
  if CloseBrace = nil then
    ErrorStr('Incomplete otu label command');
  if CloseBrace < OpenBrace then
    ErrorStr('Invalid otu label command');
  if OpenBrace <= CloseBrace then
    SetString(TempLabel, OpenBrace, CloseBrace - OpenBrace + 1)
  else
    TempLabel := EmptyStr;
  TempLabel := Trim(TempLabel);
  LCase := LowerCase(TempLabel);
  if(Pos('.group', LCase) > 0) then
    ParseGroupName(TempLabel, Pos('.group', LCase) + 1);
  if(Pos('.species', LCase) > 0) then
    ParseSpeciesName(TempLabel, Pos('.species', LCase) + 1);
  if (Pos('.population', LCase) > 0) then
    ParsePopulationName(TempLabel, Pos('.population', LCase) + 1);
  if (Pos('.isoutgroup', LCase) > 0) then
    ParseOutgroup(TempLabel, Pos('.isoutgroup', LCase) + 1);
  SetString(OtuLabel, AFullLabel, OpenBrace - AFullLabel); // -1 since P points to first of gp
end;

procedure TTokenizer.ParseOtuLabelCommand(const AFullLabel: PAnsiChar); // Label may contain gp Info
var
  P: PAnsiChar;
  LBrace, RBrace: PAnsiChar;
  tempLabel: AnsiString;
begin
  P := StrPos(AFullLabel, '{');
  if P <> nil then
  begin
    LBrace := P;
    inc(LBrace);
    RBrace := StrPos(AFullLabel, '}');
    dec(RBrace);
    if LBrace <= RBrace then
    begin
      SetString(TempLabel, LBrace, RBrace - LBrace + 1);
      if Pos(MEGA_COMMAND_DELIMITER, TempLabel) > 0 then
        ParseOtuLabelCommandLong(AFullLabel)
      else
        ParseOtuLabelCommandShort(AFullLabel);
    end;
  end
  else
  begin
    SetString(OtuLabel, AFullLabel, Length(AFullLabel));
    OtuLabel := Trim(OtuLabel);
  end;
end;

procedure TTokenizer.ParseOtuLabelCommandLong(const AFullLabel: PAnsiChar);
var
  P: PAnsiChar;
  Q: PAnsiChar;
  tempLabel: AnsiString;
begin
  P := StrPos(AFullLabel, '{');
  if P <> nil then
  begin
    Q := StrPos(AFullLabel, '}');
    if Q = nil then
      ErrorStr('Incomplete OTU command statement');
    if Q < P then
      ErrorStr('Invalid OTU command statement');
    Inc(P);
    Dec(Q);
    if P <= Q then
      SetString(tempLabel, P, Q - P +1)
    else
      TempLabel := EmptyStr;
    TempLabel := Trim(TempLabel);
    GpLabel := ParseLongLabelValue('group', TempLabel);
    if SameText(GpLabel, 'outgroup') then
      IsOutGroup := True
    else
      IsOutgroup := False;

    SpLabel := ParseLongLabelValue('species', TempLabel);
    PopLabel := ParseLongLabelValue('population', TempLabel);
    ContinentLablel := ParseLongLabelValue('continent', TempLabel);
    CountryLabel := ParseLongLabelValue('country', TempLabel);
    CityLabel := ParseLongLabelValue('city', TempLabel);
    YearLabel := ParseLongLabelValue('year', TempLabel);
    MonthLabel := ParseLongLabelValue('month', TempLabel);
    DayLabel := ParseLongLabelValue('day', TempLabel);
    TimeLabel := ParseLongLabelValue('time', tempLabel);
    SetString(OtuLabel, AFullLabel, P - AFullLabel - 1); // -1 since P points to first of the definitions
    OtuLabel := Trim(OtuLabel);
  end
  else
  begin
    SetString(OtuLabel, AFullLabel, Length(AFullLabel));
    OtuLabel := Trim(OtuLabel);
  end;
end;

procedure TTokenizer.ParseOtuLabelCommandShort(const AFullLabel: PAnsiChar);
var
  P: PAnsiChar;
  Q: PAnsiChar;
  front, back: Integer;
  tempLabel: AnsiString;
begin
  P := StrPos(AFullLabel, '{');
  if P <> nil then
  begin
    Q := StrPos(AFullLabel, '}');
    if Q = nil then
      ErrorStr('Incomplete group Definition');
    if Q < P then
      ErrorStr('Invalid group definition');
    Inc(P);
    Dec(Q);
    if P <= Q then
      SetString(tempLabel, P, Q - P +1)
    else
      TempLabel := EmptyStr;
    TempLabel := Trim(TempLabel);
    GpLabel := TempLabel;
    // Check if we have any population or species values after the group name
    back := Pos('|', TempLabel);
    if back > 0 then
      GpLabel := copy(TempLabel, 1, back-1);
    if SameText(GpLabel, 'outgroup') then
      IsOutGroup := True
    else
      IsOutgroup := False;
    tempLabel := copy(tempLabel, back, length(tempLabel));

    SetString(OtuLabel, AFullLabel, P - AFullLabel -1); // -1 since P points to first of gp
    OtuLabel := Trim(OtuLabel);

    front := Pos('|', tempLabel)+1; // look for species
    if front > 1 then
    begin
      tempLabel := copy(tempLabel, front, length(tempLabel));
      back := Pos('|', tempLabel);
      if back = 0 then
        back := length(tempLabel)+1;
      SpLabel := trim(copy(tempLabel, 1, back-1));
      // update the tempLabel to remove the spLabel
      tempLabel := copy(tempLabel, back, length(tempLabel));

      // Now get the populations
      front := Pos('|', tempLabel)+1; // look for population
      if front > 1 then
      begin
        tempLabel := copy(tempLabel, front, length(tempLabel));
        back := Pos('|', tempLabel);
        if back = 0 then
          back := length(tempLabel)+1;
        PopLabel := trim(copy(tempLabel, 1, back-1));
      end;
    end;
  end
  else
  begin
    SetString(OtuLabel, AFullLabel, Length(AFullLabel));
    OtuLabel := Trim(OtuLabel);
  end;
end;

procedure TTokenizer.ParseOutgroup(ACommandStr: AnsiString; Start: Integer);
var
  i, j: Integer;
  AStr: AnsiString;
  ABool: Boolean;
begin
  IsOutgroup := False;
  i := PosEx('.', ACommandStr, Start) + 1;
  j := PosEx('|', ACommandStr, Start);
  if j = 0 then
    j := PosEx('}', ACommandStr, Start);
  if j = 0 then
    ErrorStr('Invalid outgroup specification in the otu label command');
  AStr := Trim(Copy(ACommandStr, i, j - i));
  if not TryStrToBool(AStr, ABool) then
    ErrorStr('Invalid outgroup specification in the otu label command')
  else
    IsOutgroup := ABool;
end;

procedure TTokenizer.ParsePopulationName(ACommandStr: AnsiString; Start: Integer);
var
  i, j: Integer;
begin
  PopLabel := EmptyStr;
  i := PosEx('.', ACommandStr, Start) + 1;
  j := PosEx('|', ACommandStr, Start);
  if j = 0 then
    j := PosEx('}', ACommandStr, Start);
  if j = 0 then
    ErrorStr('Invalid population specification in the otu label command');
  PopLabel := Trim(Copy(ACommandStr, i, j - i));
end;

procedure TTokenizer.ParseSpeciesName(ACommandStr: AnsiString; Start: Integer);
var
  i, j: Integer;
begin
  SpLabel := EmptyStr;
  i := PosEx('.', ACommandStr, Start) + 1;
  j := PosEx('|', ACommandStr, Start);
  if j = 0 then
    j := PosEx('}', ACommandStr, Start);
  if j = 0 then
    ErrorStr('Invalid species specification in the otu label command');
  SpLabel := Trim(Copy(ACommandStr, i, j - i));
end;

//==== Validates the name; I may already have stuff for it
function TTokenizer.ValidateName(var S: AnsiString): Boolean;
var
  L: Integer;
begin
  Result := False;
  TrimTaxaName(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    ErrorStr('Name is zero character long');
end;

// this is called if you are expected an assignment statement
procedure TTokenizer.ReadAssignmentStatement(var CurVar, OptStr: AnsiString);
begin
  // assumes that the current token is CurVar
  if Token <> toSymbol then
    Error(SSymbolExpected);
  CurVar := TokenString;

  if not IsValidVariable(CurVar) then
    raise EParserError.CreateFmt(SParseError, [CurVar+' is not a valid variable. ', FSourceLine]);

  NextToken;
  while Token = toComment do
  begin
    GetStringToken(toCommentEnd);
//    if FOptions <> nil then FOptions.Add('Comment' +'Command-'+CurVar+'='+Trim(FStringToken));
    NextToken;
  end;
  while Token = toCommentMEGA1 do
  begin
    GetStringToken(toCommentMEGA1);
//    if FOptions <> nil then FOptions.Add('Comment' +'Command-'+CurVar+'='+Trim(FStringToken));
    NextToken;
  end;

  if Token <> toEqual then
    raise EParserError.CreateFmt(SParseError, ['Equal sign expected', FSourceLine]);

  if  AnsiCompareText(Fkeywds.Values[CurVar], 'int') = 0 then
    FNeedSymbol := toNumber
  else if AnsiCompareText(Fkeywds.Values[CurVar], 'char') = 0 then
    FNeedSymbol := toChar
  else if AnsiCompareText(Fkeywds.Values[CurVar], 'name') = 0 then
    FNeedSymbol := toOtuName
  else
    FNeedSymbol := toSymbol;
  NextToken;
  while Token = toComment do
  begin
    GetStringToken(toCommentEnd);
//    if FOptions <> nil then FOptions.Add('Comment' +'Command- After '+CurVar+'='+Trim(FStringToken));
    NextToken;
  end;
  while Token = toCommentMEGA1 do
  begin
    GetStringToken(toCommentMEGA1);
//    if FOptions <> nil then FOptions.Add('Comment' +'Command- After '+CurVar+'='+Trim(FStringToken));
    NextToken;
  end;

  if Token <> toSymbol then
    Error(SSymbolExpected);
  OptStr := TokenString;
  if not IsValidOption(CurVar, OptStr) then
    raise EParserError.CreateFmt(SParseError, [OptStr+' is not a valid option for '+CurVar+'.', FSourceLine]);
  FNeedSymbol := #0;
end;

// is the variable valid
function TTokenizer.IsValidVariable(AVar: AnsiString): Boolean;
begin
  Result := FKeywds.IndexofName(AVar) >= 0;
end;

function TTokenizer.IsValidOption(AVar, AOpt: AnsiString): Boolean;
begin
  Result := True;
  try
    if  AnsiCompareText(Fkeywds.Values[AVar], 'int') = 0 then
      StrToInt(AOpt)
    else if AnsiCompareText(Fkeywds.Values[AVar], 'char') = 0 then
      Result := (Length(AOpt) = 1)
    else if AnsiCompareText(Fkeywds.Values[AVar], 'name') = 0 then
      Result := (Length(AOpt) > 0)
    else if AnsiCompareText(Fkeywds.Values[AVar], 'bool') = 0 then
      Result := (AnsiCompareText(AOpt, 'Yes') = 0) or (AnsiCompareText(AOpt, 'No') = 0)
    else
      Result := Pos(Lowercase(','+AOpt+','), Lowercase(FKeywds.Values[AVar])) > 0;
  except
    On E: Exception do
      Result := False;
  end;
end;

function TTokenizer.TokenAsInt: Integer;
begin
  if Token <> toSymbol then
    Error(SNumberExpected);
  Result := StrToInt(TokenString);
end;

function TTokenizer.TokenAsFloat: Extended;
begin
  if Token <> toSymbol then
    Error(SNumberExpected);
  Result := StrToFloat(TokenString);
end;

function TTokenizer.GetStringToken(Delimitor: AnsiChar): AnsiChar;  // get string token
var
  x: AnsiChar;
  MyStr: array of AnsiChar;
  MaxSz, i, MyLen: Integer;
  NestedCommentCount : Integer;

begin
  LSrcPtr := FSourcePtr;
//  skipBlanks;  changed in mega2b4
  MyStr := nil;

  try
    MaxSz := 1024;
    SetLength(MyStr, MaxSz);
    MyLen := 0;
    NestedCommentCount := 1;

    while True do
    begin
      x := PeekNextChar;
      if Delimitor = toCommentEnd then
      begin
        if x = toComment then
          Inc(NestedCommentCount)
        else if x = Delimitor then
        begin
          Dec(NestedCommentCount);
          if NestedCommentCount <= 0 then
          begin
            GetNextChar;
            break;
          end;
        end;
      end
      else  if x = Delimitor then  // for all other non-specific delimitors
      begin
        GetNextChar;
        break;
      end;

      if x = toEOF then
      begin
        ErrorStr('Unable to find the command terminator '''+Delimitor+'''');
      end;

      if Delimitor = ';' then  // that is, we are reading valid command
        if x in [toPound, toCommand] then
          ErrorStr('Do not use # or ! in the Description because they are reserved characters.');

      if MyLen = MaxSz then
      begin
        MaxSz := MyLen + 1024;
        SetLength(MyStr, MaxSz);
      end;
      x := GetNextChar;
//      case x of
//        #10, #13: continue;
//      end;
      MyStr[MyLen] := x;
      Inc(MyLen);
    end;
    SetLength(FStringToken, MyLen+1);
    for i:=0 to MyLen-1 do
      FStringToken[i+1] := MyStr[i];
    FStringToken[MyLen+1] := #0;
    FStringToken := Trim(FStringToken); // this trims trailing and leading gaps
  finally
    MyStr := nil;
  end;

  FSourcePtr := LSrcPtr;
  Result := Delimitor;
  FToken := Result;
end;

function TTokenizer.PeekNextChar: AnsiChar;
begin
  Result := LSrcPtr^;
end;

function TTokenizer.GetNextChar: AnsiChar;
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
  {$IFDEF VISUAL_BUILD}
  if FGauge <> nil then
  begin
    Inc(CurAdvance);
    if CurAdvance > PercentPerAdvance then
    begin
      FGauge.Position := FGauge.Position + (CurAdvance div PercentPerAdvance);
      CurAdvance := CurAdvance mod PercentPerAdvance;
      Application.ProcessMessages;
      if (FStopBtn <> nil) and FStopBtn.Down then
        raise Exception.Create('User aborted process...');
    end;
  end;
  {$ENDIF}
end;

procedure TTokenizer.CheckToken(T: AnsiChar); // Directly from DELPHI
begin
  if Token <> T then
    case T of
      AnsiChar(1):  // toSymbol
        Error(SIdentifierExpected);
      AnsiChar(2), AnsiChar(5):  // toString, toWString
        Error(SStringExpected);
      AnsiChar(3), AnsiChar(4):  // toInteger, toFloat
        Error(SNumberExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
end;

procedure TTokenizer.CheckTokenSymbol(const S: AnsiString);
begin
  if not TokenSymbolIs(S) then ErrorFmt(SSymbolExpected, [S]);
end;

procedure TTokenizer.Error(const Ident: AnsiString);
begin
  ErrorStr(Ident);
end;

procedure TTokenizer.ErrorFmt(const Ident: AnsiString; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args)); // just concatenates at present
end;

procedure TTokenizer.ErrorStr(const Message: AnsiString);
begin
  {$IFDEF VISUAL_BUILD}
  raise EParserError.CreateFmtHelp('%s (in line %d)', [Message, FSourceLine], HC_Data_File_Parsing_Error);
  {$ELSE}
  Error_nv(Message + ' (in line ' + IntToStr(FSourceLine) + ')');
  {$ENDIF}
end;

procedure TTokenizer.ErrorLimitationStr(const Message: AnsiString);
begin
  //raise EParserError.CreateFmtHelp('%s (in line %d)', [Message, FSourceLine], HC_Data_File_Parsing_Error);
  //raise EParserError.Create(Message);
  raise Exception.CreateHelp(message, -1);
end;

function TTokenizer.NextToken: AnsiChar;
var
  x: AnsiChar;
  CurTokenLen: Integer;
begin
  if FIsTokenPending then
  begin
    FIsTokenPending := False;
    Result := FToken;
    Exit;
  end;

  LSrcPtr := FSourcePtr;
  skipBlanks;
  FTokenPtr := LSrcPtr;
  Result  := #0;

  x := GetNextChar;

  case x of
    toPound, toComment, toCommentEnd, toCommentMEGA1, toCommand, toCommandEnd, toEqual, toGroup, toGroupEnd:
        Result := x;
    toEOF:
        Result := x;
    else
    begin
      case FNeedSymbol of
        #0: // unknown type
          if x in ['a'..'z', 'A'..'Z', '0'..'9', '-','.'] then // need a letter to start
          begin
            CurTokenLen := 1;
            while PeekNextChar in ['a'..'z', 'A'..'Z', '0'..'9', '-','.'] do
            begin
              if FForceLen then
                if CurTokenLen = FTokenLen then break;
              GetNextChar;
            end;
            Result := toSymbol;
          end
          else // the token is now only one char long
            Result := toSymbol;
        toSymbol:
          if x in ['a'..'z', 'A'..'Z'] then // need english letter to start
          begin
            CurTokenLen := 1;
            while PeekNextChar in ['a'..'z', 'A'..'Z', '0'..'9'] do
            begin
              if FForceLen then
                if CurTokenLen = FTokenLen then break;
              GetNextChar;
            end;
            Result := toSymbol;
          end;
       toNumber:
          if x in ['0'..'9', '.', '+', '-'] then // need
          begin
            CurTokenLen := 1;
            while PeekNextChar in ['0'..'9','.'] do
              GetNextChar;
            Result := toSymbol;
          end
          else if x = '?' then // allows for question marks in the number
            Result := toSymbol;
       toOtuName:
         begin
           if x in ValidOtuNameStartSet then
             while PeekNextChar in ValidOtuNameContinueSet + [toGroup,toGroupEnd] do
               GetNextChar;
           Result := toSymbol;
         end;
       toChar:
         Result := toSymbol;
       else
       begin
         if x in ValidOtuNameStartSet then
           while PeekNextChar in ValidOtuNameContinueSet + [toGroup,toGroupEnd] do
             GetNextChar;
         Result := toSymbol;
       end; //
      end; // case else
    end; // else begin
  end; // case end
  FToken := Result;
  FSourcePtr := LSrcPtr;
end;

procedure TTokenizer.ReadBuffer;
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then Move(FSourcePtr[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;

  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then Error(SLineTooLong);
  end;

  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

procedure TTokenizer.SkipBlanks;
begin
  // skip blanks; and get the next char
  while True do
    case PeekNextChar of
      #33..#255: Break;
      #0:  // next char is 0
         if GetNextChar = #0 then
           Exit;
      #10:  // new line
        if FDetectNewLine then Break else GetNextChar;
    else
      GetNextChar;
    end;
end;

procedure TTokenizer.SkipUntilChar(x:AnsiChar);
var
  nc: AnsiChar;
begin
  // skip blanks; and get the next char
  while True do
  begin
    nc := PeekNextChar;
    if nc = x then
    begin
      FSourcePtr := LSrcPtr;
      Exit;
    end;
    case nc of
      #0:  // next char is 0
         if GetNextChar = #0 then
           Exit;
      #10:  // new line
        if FDetectNewLine then Break else GetNextChar;
    else
        GetNextChar;
    end;
  end;
end;

function TTokenizer.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

function TTokenizer.TokenFloat: Extended;
begin
  if FFloatType <> #0 then Dec(FSourcePtr);
  Result := StrToFloat(TokenString);
  if FFloatType <> #0 then Inc(FSourcePtr);
end;

function TTokenizer.TokenInt: Longint;
begin
  Result := StrToInt(TokenString);
end;

function TTokenizer.TokenString: AnsiString;
var
  L: Integer;
begin
  L := FSourcePtr - FTokenPtr;
  SetString(Result, FTokenPtr, L);
end;

function TTokenizer.TokenWideString: WideString;
begin
  Result := FWideStr;
end;

function TTokenizer.TokenSymbolIs(const S: AnsiString): Boolean;
begin
  Result := CompareText(S, TokenString) = 0;
end;

function TTokenizer.TokenSymbolIsFloat(const S: AnsiString): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=1 to Length(S) do
  begin
    if not (S[i] in ['-','0'..'9','.']) then
      Exit;
  end;
  Result := True;
end;

function TTokenizer.TokenComponentIdent: AnsiString;
var
  P: PAnsiChar;
begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in ['A'..'Z', 'a'..'z', '_']) then
      Error(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

procedure TTokenizer.SetRuntimeProgress(ARPin: TRuntimeProgress);
begin
{$IFDEF VISUAL_BUILD}
  FGauge := ARPin.PercentGauge;
  FGauge.Min := 0;
  FGauge.Max := 100;
  FGauge.Position := 0;
{$ENDIF}
end;

end.
