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

unit mcustominifile;

{$mode objfpc}
{$H+}

interface

uses classes, sysutils, contnrs;

type
  { THashedStringList }

  THashedStringList = class(TStringList)
  private
    FValueHash: TFPHashList;
    FNameHash: TFPHashList;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override;
  end;

  TMegaIniFileKey = class
  Private
    FIdent: string;
    FValue: string;
  public
    constructor Create(const AIdent, AValue: string);
    property Ident: string read FIdent write FIdent;
    property Value: string read FValue write FValue;
  end;

  TMegaIniFileKeyList = class(TList)
  private
    function GetItem(Index: integer): TMegaIniFileKey;
    function KeyByName(const AName: string; CaseSensitive : Boolean): TMegaIniFileKey;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: integer]: TMegaIniFileKey read GetItem; default;
  end;

  TMegaIniFileSection = class
  private
    FName: string;
    FKeyList: TMegaIniFileKeyList;
  public
    Function Empty : Boolean;
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property KeyList: TMegaIniFileKeyList read FKeyList;
  end;

  TMegaIniFileSectionList = class(TList)
  private
    function GetItem(Index: integer): TMegaIniFileSection;
    function SectionByName(const AName: string; CaseSensitive : Boolean): TMegaIniFileSection;
  public
    destructor Destroy; override;
    procedure Clear;override;
    property Items[Index: integer]: TMegaIniFileSection read GetItem; default;
  end;

  TMegaCustomIniFile = class(TObject)
  Private
    FFileName: string;
    FSectionList: TMegaIniFileSectionList;
    FEscapeLineFeeds: boolean;
    FCaseSensitive : Boolean;
    FStripQuotes : Boolean;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); virtual;
    destructor Destroy; override;
    function SectionExists(const Section: string): Boolean; virtual;
    function ReadString(const Section, Ident, Default: string): string; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: String); virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); virtual;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); virtual;
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; virtual;
    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime; virtual;
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer; virtual;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteFloat(const Section, Ident: string; Value: Double); virtual;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime); virtual;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream); virtual;
    procedure ReadSection(const Section: string; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); virtual; abstract;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); virtual; abstract;
    procedure EraseSection(const Section: string); virtual; abstract;
    procedure DeleteKey(const Section, Ident: String); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: string): Boolean; virtual;
    property FileName: string read FFileName;
    property EscapeLineFeeds: boolean read FEscapeLineFeeds;
    Property CaseSensitive : Boolean Read FCaseSensitive Write FCaseSensitive;
    Property StripQuotes : Boolean Read FStripQuotes Write FStripQuotes;
  end;

  { TMegaIniFile }

  TMegaIniFile = class(TMegaCustomIniFile)
  Private
    FStream: TStream;
    FCacheUpdates: Boolean;
    FDirty : Boolean;
    procedure FillSectionList(AStrings: TStrings);
    Procedure DeleteSection(ASection : TMegaIniFileSection);
    Procedure MaybeDeleteSection(ASection : TMegaIniFileSection);
    procedure SetCacheUpdates(const AValue: Boolean);
  protected
    procedure MaybeUpdateFile;
    property Dirty : Boolean Read FDirty;
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    constructor Create(AStream: TStream; AEscapeLineFeeds : Boolean = False);
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSectionRaw(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
    property Stream: TStream read FStream;
    property CacheUpdates : Boolean read FCacheUpdates write SetCacheUpdates;
  end;

  TMegaMemIniFile = class(TMegaIniFile)
  public
    constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
    procedure Clear;
    procedure GetStrings(List: TStrings);
    procedure Rename(const AFileName: string; Reload: Boolean);
    procedure SetStrings(List: TStrings);
  end;

implementation

const
   Brackets  : array[0..1] of Char = ('[', ']');
   Separator : Char = '=';
   Comment   : Char = ';';
   LF_Escape : Char = '\';

function CharToBool(AChar: char): boolean;
begin
  Result := (Achar = '1');
end;

function BoolToChar(ABool: boolean): char;
begin
  if ABool then
    Result := '1'
  else
    Result := '0';
end;

function IsComment(const AString: string): boolean;
begin
  Result := False;
  if AString > '' then
    Result := (Copy(AString, 1, 1) = Comment);
end;

{ THashedStringList }

destructor THashedStringList.Destroy;
begin
  if Assigned(FValueHash) then
    FValueHash.Free;
  if Assigned(FNameHash) then
    FNameHash.Free;
  inherited Destroy;
end;

function THashedStringList.IndexOf(const S: String): Integer;
var
  I: Integer;
begin
  if not FValueHashValid then
    UpdateValueHash;

  I := FValueHash.FindIndexOf(S);
  if I >= 0 then
    Result := abs(Integer(FValueHash[I])-1) // casting pointers to unsigned integers may not work on Linux (compiler warning)
  else
    Result := -1;
end;

function THashedStringList.IndexOfName(const Name: String): Integer;
var
  I: Integer;
begin
  if not FNameHashValid then
    UpdateNameHash;

  I := FNameHash.FindIndexOf(Name);
  if I >= 0 then
    Result := abs(Integer(FNameHash[I])-1) // casting pointers to unsigned integers may not work on Linux (compiler warning)
  else
    Result := -1;
end;

procedure THashedStringList.Changed;
begin
  FValueHashValid := False;
  FNameHashValid := False;
  inherited Changed;
end;

constructor THashedStringList.Create;
begin
  inherited Create;
  FValueHash:= TFPHashList.Create;
  FNameHash:= TFPHashList.Create;
end;

procedure THashedStringList.UpdateValueHash;
var
  I: Integer;
begin
  if not Assigned(FValueHash) then
    FValueHash := TFPHashList.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    FValueHash.Add(Strings[I], Pointer(I+1));
  FValueHashValid := True;
end;

procedure THashedStringList.UpdateNameHash;
var
  I: Integer;
begin
  if not Assigned(FNameHash) then
    FNameHash := TFPHashList.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
    FNameHash.Add(Names[I], Pointer(I+1));
  FNameHashValid := True;
end;

{ TMegaIniFileKey }

constructor TMegaIniFileKey.Create(const AIdent, AValue: string);
begin
  FIdent := AIdent;
  FValue := AValue;
end;

{ TMegaIniFileKeyList }

function TMegaIniFileKeyList.GetItem(Index: integer): TMegaIniFileKey;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TMegaIniFileKey(inherited Items[Index]);
end;

function TMegaIniFileKeyList.KeyByName(const AName: string; CaseSensitive : Boolean): TMegaIniFileKey;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
    If CaseSensitive then
      begin
      for i := 0 to Count-1 do
        if Items[i].Ident=AName then
          begin
          Result := Items[i];
          Break;
          end;
      end
    else
      for i := 0 to Count-1 do
        if CompareText(Items[i].Ident, AName) = 0 then begin
          Result := Items[i];
          Break;
        end;
end;

destructor TMegaIniFileKeyList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMegaIniFileKeyList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

Function TMegaIniFileSection.Empty : Boolean;

Var
  I : Integer;

begin
  Result:=True;
  I:=0;
  While Result and (I<KeyList.Count)  do
    begin
    result:=IsComment(KeyList[i].Ident);
    Inc(i);
    end;
end;


{ TMegaIniFileSection }

constructor TMegaIniFileSection.Create(const AName: string);
begin
  FName := Trim(AName);
  FKeyList := TMegaIniFileKeyList.Create;
end;

destructor TMegaIniFileSection.Destroy;
begin
  FKeyList.Free;
  inherited Destroy;
end;

{ TMegaIniFileSectionList }

function TMegaIniFileSectionList.GetItem(Index: integer): TMegaIniFileSection;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TMegaIniFileSection(inherited Items[Index]);
end;

function TMegaIniFileSectionList.SectionByName(const AName: string; CaseSensitive : Boolean): TMegaIniFileSection;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
    If CaseSensitive then
      begin
      for i:=0 to Count-1 do
        if (Items[i].Name=AName) then
          begin
          Result := Items[i];
          Break;
          end;
      end
    else
      for i := 0 to Count-1 do
        if CompareText(Items[i].Name, AName) = 0 then
          begin
          Result := Items[i];
          Break;
          end;
end;

destructor TMegaIniFileSectionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMegaIniFileSectionList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

{ TMegaCustomIniFile }

constructor TMegaCustomIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);
begin
  FFileName := AFileName;
  FSectionList := TMegaIniFileSectionList.Create;
  FEscapeLineFeeds := AEscapeLineFeeds;
end;

destructor TMegaCustomIniFile.Destroy;
begin
  FSectionList.Free;
  inherited Destroy;
end;

function TMegaCustomIniFile.SectionExists(const Section: string): Boolean;

Var
  S : TMegaIniFileSection;

begin
  S:=FSectionList.SectionByName(Section,CaseSensitive);
  Result:=Assigned(S) and Not S.Empty;
end;

function TMegaCustomIniFile.ReadInteger(const Section, Ident: string; Default: Longint): Longint;
begin
  // StrToInfDef() supports hex numbers prefixed with '0x' via val()
  Result := StrToIntDef(ReadString(Section, Ident, ''), Default);
end;

procedure TMegaCustomIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TMegaCustomIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
  s: string;
begin
  Result := Default;
  s := ReadString(Section, Ident, '');
  if s > '' then
    Result := CharToBool(s[1]);
end;

procedure TMegaCustomIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  WriteString(Section, Ident, BoolToChar(Value));
end;

function TMegaCustomIniFile.ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  Result := StrToDateDef(ReadString(Section, Ident, ''),Default);
end;

function TMegaCustomIniFile.ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  Result := StrToDateTimeDef(ReadString(Section, Ident, ''),Default);
end;

function TMegaCustomIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;

begin
  Result:=StrToFloatDef(ReadString(Section, Ident, ''),Default);
end;

function TMegaCustomIniFile.ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;

begin
  Result := StrToTimeDef(ReadString(Section, Ident, ''),Default);
end;

procedure TMegaCustomIniFile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  WriteString(Section, Ident, DateToStr(Value));
end;

procedure TMegaCustomIniFile.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
  WriteString(Section, Ident, DateTimeToStr(Value));
end;

procedure TMegaCustomIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteString(Section, Ident, FloatToStr(Value));
end;

procedure TMegaCustomIniFile.WriteTime(const Section, Ident: string; Value: TDateTime);
begin
  WriteString(Section, Ident, TimeToStr(Value));
end;

function TMegaCustomIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  oSection: TMegaIniFileSection;
begin
  Result := False;
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then
    Result := (oSection.KeyList.KeyByName(Ident,CaseSensitive) <> nil);
end;

function TMegaCustomIniFile.ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
Var
  S : String;
  PB,PR : PByte;
  PC : PChar;
  H : String[3];
  i, code: Integer;
begin
  S:=ReadString(Section,Name,'');
  Setlength(H,3);
  H[1]:='$';
  Result:=Length(S) div 2;
  If Result>0 then
    begin
      GetMem(PR,Result);
      Try
        PC:=PChar(S);
        PB:=PR;
        For I:=1 to Result do
          begin
          H[2]:=PC[0];
          H[3]:=PC[1];
          Val(H,PB^,code);
          Inc(PC,2);
          Inc(PB);
          end;
        Value.WriteBuffer(PR^,Result);
      finally
        FreeMem(PR);
      end;
    end;
end;

procedure TMegaCustomIniFile.WriteBinaryStream(const Section, Name: string; Value: TStream);


Var
  M : TMemoryStream;
  S : String;
  PB : PByte;
  PC : PChar;
  H : String[2];
  i : Integer;

begin
  M:=TMemoryStream.Create;
  Try
    M.CopyFrom(Value,0);
    SetLength(S,M.Size*2);
    If (length(S)>0) then
      begin
      PB:=M.Memory;
      PC:=PChar(S);
      For I:=1 to Length(S) div 2 do
        begin
        H:=HexStr(PB^,2);
        PC[0]:=H[1];
        PC[1]:=H[2];
        Inc(PC,2);
        Inc(PB);
        end;
      end;
    WriteString(Section,Name,S);
  Finally
    M.Free;
  end;
end;

{ TMegaIniFile }

constructor TMegaIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);
var
  slLines: TStringList;
begin
  If Not (self is TMegaMemIniFile) then
    StripQuotes:=True;
  inherited Create(AFileName,AEscapeLineFeeds);
  FStream := nil;
  slLines := TStringList.Create;
  try
    if FileExists(FFileName) then
      begin
      // read the ini file values
      slLines.LoadFromFile(FFileName);
      FillSectionList(slLines);
      end
  finally
    slLines.Free;
  end;
end;

constructor TMegaIniFile.Create(AStream: TStream; AEscapeLineFeeds : Boolean = False);
var
  slLines: TStringList;
begin
  inherited Create('',AEscapeLineFeeds);
  FStream := AStream;
  slLines := TStringList.Create;
  try
    // read the ini file values
    slLines.LoadFromStream(FStream);
    FillSectionList(slLines);
  finally
    slLines.Free;
  end;
end;

destructor TMegaIniFile.destroy;
begin

  If FDirty and FCacheUpdates then
    try
      UpdateFile;
    except
      // Eat exception. Compatible to D7 behaviour, see comments to bug 19046
    end;
  inherited destroy;
end;

procedure TMegaIniFile.FillSectionList(AStrings: TStrings);
var
  i,j: integer;
  sLine, sIdent, sValue: string;
  oSection: TMegaIniFileSection;

  procedure RemoveBackslashes;
  var
    i,l: integer;
    s: string;
  begin
    AStrings.BeginUpdate;
    try
      For I:=AStrings.Count-2 downto 0 do
        begin
        S:=AStrings[i];
        L:=Length(S);
        If (I<AStrings.Count-1) and (L>0) and (S[L]=LF_Escape) then
          begin
          S:=Copy(S,1,L-1)+AStrings[I+1];
          AStrings.Delete(I+1);
          AStrings[i]:=S;
          end;
        end;
    finally
      AStrings.EndUpdate;
    end;
  end;

begin
  oSection := nil;
  FSectionList.Clear;
  if FEscapeLineFeeds then
    RemoveBackslashes;
  for i := 0 to AStrings.Count-1 do begin
    sLine := Trim(AStrings[i]);
    if sLine > '' then
      begin
      if IsComment(sLine) and (oSection = nil) then begin
        // comment at the beginning of the ini file
        oSection := TMegaIniFileSection.Create(sLine);
        FSectionList.Add(oSection);
        continue;
      end;
      if (Copy(sLine, 1, 1) = Brackets[0]) and (Copy(sLine, length(sLine), 1) = Brackets[1]) then begin
        // regular section
        oSection := TMegaIniFileSection.Create(Copy(sLine, 2, Length(sLine) - 2));
        FSectionList.Add(oSection);
      end else if oSection <> nil then begin
        if IsComment(sLine) then begin
          // comment within a section
          sIdent := sLine;
          sValue := '';
        end else begin
          // regular key
          j:=Pos(Separator, sLine);
          if j=0 then
           begin
             sIdent:='';
             sValue:=sLine
           end
          else
           begin
             sIdent:=Trim(Copy(sLine, 1,  j - 1));
             sValue:=Trim(Copy(sLine, j + 1, Length(sLine) - j));
           end;
        end;
        oSection.KeyList.Add(TMegaIniFileKey.Create(sIdent, sValue));
      end;
      end;
  end;
end;

function TMegaIniFile.ReadString(const Section, Ident, Default: string): string;
var
  oSection: TMegaIniFileSection;
  oKey: TMegaIniFileKey;
  J: integer;
begin
  Result := Default;
  oSection := FSectionList.SectionByName(Section, CaseSensitive);
  if oSection <> nil then
  begin
    oKey := oSection.KeyList.KeyByName(Ident, CaseSensitive);
    if oKey <> nil then
      if StripQuotes then
      begin
        J:=Length(oKey.Value);
        // Joost, 2-jan-2007: The check (J>1) is there for the case that
        // the value consist of a single double-quote character. (see
        // mantis bug 6555)
        if (J>1) and ((oKey.Value[1] in ['"','''']) and (oKey.Value[J]=oKey.Value[1])) then
          Result:=Copy(oKey.Value,2,J-2)
        else
          Result:=oKey.Value;
      end
      else
        Result:=oKey.Value;
  end;
end;

procedure TMegaIniFile.SetCacheUpdates(const AValue: Boolean);
begin
  if FCacheUpdates and not AValue and FDirty then
    UpdateFile;
  FCacheUpdates := AValue;
end;

procedure TMegaIniFile.WriteString(const Section, Ident, Value: String);
var
  oSection: TMegaIniFileSection = nil;
  oKey: TMegaIniFileKey = nil;
begin
  if (Section > '') and (Ident > '') then
  begin
    // update or add key
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if (oSection = nil) then
    begin
      oSection := TMegaIniFileSection.Create(Section);
      FSectionList.Add(oSection);
    end;
    with oSection.KeyList do
    begin
      oKey := KeyByName(Ident,CaseSensitive);
      if oKey <> nil then
        oKey.Value := Value
      else
        oSection.KeyList.Add(TMegaIniFileKey.Create(Ident, Value));
    end;
  end;
  MaybeUpdateFile;
end;

procedure TMegaIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  oSection: TMegaIniFileSection;
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do
        if not IsComment(Items[i].Ident) then
          Strings.Add(Items[i].Ident + Separator + Items[i].Value);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMegaIniFile.ReadSectionRaw(const Section: string; Strings: TStrings);
var
  oSection: TMegaIniFileSection;
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do
        if not IsComment(Items[i].Ident) then
         begin
           if Items[i].Ident<>'' then
            Strings.Add(Items[i].Ident + Separator +Items[i].Value)
           else
            Strings.Add(Items[i].Value);
         end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMegaIniFile.ReadSections(Strings: TStrings);
var
  i: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for i := 0 to FSectionList.Count-1 do
      if not IsComment(FSectionList[i].Name) then
        Strings.Add(FSectionList[i].Name);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMegaIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  oSection: TMegaIniFileSection;
  s: string;
  i,J: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    oSection := FSectionList.SectionByName(Section,CaseSensitive);
    if oSection <> nil then with oSection.KeyList do
      for i := 0 to Count-1 do begin
        s := Items[i].Value;
      If StripQuotes then
        begin
          J:=Length(s);
          // Joost, 2-jan-2007: The check (J>1) is there for the case that
          // the value consist of a single double-quote character. (see
          // mantis bug 6555)
          If (J>1) and ((s[1] in ['"','''']) and (s[J]=s[1])) then
             s:=Copy(s,2,J-2);
        end;
        if Items[i].Ident<>'' then
          s:=Items[i].Ident+Separator+s;
        Strings.Add(s);
      end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMegaIniFile.DeleteSection(ASection : TMegaIniFileSection);

begin
  FSectionList.Delete(FSectionList.IndexOf(ASection));
  ASection.Free;
end;

Procedure TMegaIniFile.MaybeDeleteSection(ASection : TMegaIniFileSection);

begin
  If Asection.Empty then
    DeleteSection(ASection);
end;

procedure TMegaIniFile.EraseSection(const Section: string);
var
  oSection: TMegaIniFileSection;
begin
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then begin
    { It is needed so UpdateFile doesn't find a defunct section }
    { and cause the program to crash }
    DeleteSection(OSection);
    MaybeUpdateFile;
  end;
end;

procedure TMegaIniFile.DeleteKey(const Section, Ident: String);
var
 oSection: TMegaIniFileSection;
 oKey: TMegaIniFileKey;
begin
  oSection := FSectionList.SectionByName(Section,CaseSensitive);
  if oSection <> nil then
    begin
    oKey := oSection.KeyList.KeyByName(Ident,CaseSensitive);
    if oKey <> nil then
      begin
      oSection.KeyList.Delete(oSection.KeyList.IndexOf(oKey));
      oKey.Free;
      MaybeUpdateFile;
      end;
    end;
end;

procedure TMegaIniFile.UpdateFile;
var
  slLines: TStringList = nil;
  i, j: integer;
begin
  if not Assigned(FSectionList) then
    Exit;
  slLines := TStringList.Create;
  try
    for i := 0 to FSectionList.Count-1 do
      with FSectionList[i] do begin
        if IsComment(Name) then
          // comment
          slLines.Add(Name)
        else
          // regular section
          slLines.Add(Brackets[0] + Name + Brackets[1]);
        for j := 0 to KeyList.Count-1 do
          if IsComment(KeyList[j].Ident) then
            // comment
            slLines.Add(KeyList[j].Ident)
          else
            // regular key
            slLines.Add(KeyList[j].Ident + Separator + KeyList[j].Value);
        if (i < FSectionList.Count-1) and not IsComment(Name) then
          slLines.Add('');
      end;
    if Trim(FFileName) <> '' then
      slLines.SaveToFile(FFileName)
    else if FStream <> nil then
      slLines.SaveToStream(FStream);
    FillSectionList(slLines);
    FDirty := false;
  finally
    slLines.Free;
  end;
end;

procedure TMegaIniFile.MaybeUpdateFile;
begin
  If FCacheUpdates then
    FDirty:=True
  else
    UpdateFile;
end;

{ TMegaMemIniFile }

constructor TMegaMemIniFile.Create(const AFileName: string; AEscapeLineFeeds : Boolean = False);

begin
  Inherited;
  FCacheUpdates:=True;
end;

procedure TMegaMemIniFile.Clear;
begin
  FSectionList.Clear;
end;

procedure TMegaMemIniFile.GetStrings(List: TStrings);
var
  i, j: integer;
  oSection: TMegaIniFileSection;
begin
  List.BeginUpdate;
  try
    for i := 0 to FSectionList.Count-1 do begin
      oSection := FSectionList[i];
      with oSection do begin
        if IsComment(Name) then
          List.Add(Name)
        else
          List.Add(Brackets[0] + Name + Brackets[1]);
        for j := 0 to KeyList.Count-1 do begin
          if IsComment(KeyList[j].Ident) then
            List.Add(KeyList[j].Ident)
          else
            List.Add(KeyList[j].Ident + Separator + KeyList[j].Value);
        end;
      end;
      if i < FSectionList.Count-1 then
        List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TMegaMemIniFile.Rename(const AFileName: string; Reload: Boolean);
var
  slLines: TStringList;
begin
  FFileName := AFileName;
  FStream := nil;
  if Reload then begin
    slLines := TStringList.Create;
    try
      slLines.LoadFromFile(FFileName);
      FillSectionList(slLines);
    finally
      slLines.Free;
    end;
  end;
end;

procedure TMegaMemIniFile.SetStrings(List: TStrings);
begin
  FillSectionList(List);
end;

end.
