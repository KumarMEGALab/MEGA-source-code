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

unit MFormatConvertToMega;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, FileUtil,
  SysUtils, Classes, Dialogs, IniFiles, MegaUtils, Controls;

type
  TImportFormatType = (	gm_NONE,
                        gm_ALN,
                        gm_NEXUS,
                        gm_PHYLIP,
                        gm_GCG,
                        gm_FASTA,
                        gm_PIR,
                        gm_NBRF,
                        gm_MSF,
                        gm_IG,
                        gm_XML
                        );

  // these are indexed by the above type-ids
  const FormatNames : Array [gm_NONE..gm_XML] of AnsiString = (
    	'NONE',
        '.aln      (CLUSTAL)',
        '.nexus    (PAUP, MacClade)',
        '.phylip   (PHYLIP)',
        '.gcg      (GCG format)',
        '.fasta    (FASTA format)',
        '.pir      (PIR format)',
        '.nbrf     (NBRF format)',
    	'.msf      (MSF format)',
        '.ig       (IG format)',
        '.xml      (Internet XML format)');
        LookupFormat : Array [gm_NONE..gm_XML] of AnsiString = (
    	'NONE',
        '.aln',
        '.nexus,.nex',
        '.phylip   (PHYLIP)',
        '.gcg      (GCG format)',
        '.fasta,.fas,.fst,.fsta,.fsa,.fta,.fa',
        '.pir',
        '.nbrf',
    	'.msf',
        '.ig',
        '.xml');
type

  { TFormatConvertToMega }

  TFormatConvertToMega = class
  private
    FInFileType : TImportFormatType;
    AStrList : TStringList;		// input stringlist
    BStrList : TStringList;
    FOutFile: TFileName;
    FInFile: TFileName;		// output stringlist
    I_created_AStrList, I_created_BStrList, I_created_SeqDataHash : boolean;
    function IsNotSequence(Astr : AnsiString) : boolean;
    Procedure convert_from_ig;
    Procedure convert_from_nbrf;
    Procedure convert_from_xml;
    Procedure convert_from_pir;
    procedure convert_from_gcg;
    procedure convert_from_msf;
    procedure convert_from_nexus;
    procedure ConvertFromNexus;
    Procedure convert_from_fasta;
    Procedure convert_from_phylip;
    procedure convert_from_aln;
    procedure SetInFile(const fn : TFileName);
    procedure FormatSequence(const SeqString: String; var aList: TStringList);
  public
    // for accessing sequence data outside the unit
    SeqDataHash : THashedStringList;
    CurLine : Integer;  // Current line in the source file. Used for errors.
    constructor Create( in_file : TFileName = '' ); overload;
    constructor Create( in_strlist : TStrings; in_type : TImportFormatType);  overload;
    destructor Destroy; override;
    property InFile : TFileName read FInFile write FInFile;
    property InFileType : TImportFormatType read FInFileType write FInFileType;
    property OutFile : TFileName read FOutFile write FOutFile;
    function Convert( intype : TImportFormatType = gm_NONE ) : TStringList; overload;
    function Convert( instrlist : TStrings; intype: TImportFormatType ): TStringList; overload;
    procedure UpdateFileType(loadInputData: Boolean = True);
    procedure DoLoadInputData;
  end;

  function GetInFileType( fn : TFileName ) : TImportFormatType;
  function GetInFileTypeExtended( fn : TFileName ) : TImportFormatType;
  function SplitString(const S: AnsiString; const Delim: AnsiChar; ts: TStrings): Integer;

implementation
// Will have to use Forms and MPleaseWait units from in here. Not the cleanest.

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF} StrUtils, MegaConsts, mnexusalignmentconverter;

//==================================================================
constructor TFormatConvertToMega.Create(in_file: TFileName);
//==================================================================
begin
  AStrList := TStringList.Create;
  CurLine := 0;
  I_created_AStrList := true;
  BStrList := TStringList.Create;
  // for outside access to data
  SeqDataHash := THashedStringList.Create;
  SeqDataHash.Sorted := False;
  I_created_SeqDataHash := true;
  I_created_BStrList := true;
  if (in_file <> '') then
    InFile := in_file;
end;


//==================================================================
destructor TFormatConvertToMega.Destroy;
//==================================================================
begin
   if I_created_AStrList then
   	AStrList.Free;
   if I_created_BStrList then
   	BStrList.Free;
   if I_created_SeqDataHash then
        SeqDataHash.Free;
end;

procedure TFormatConvertToMega.FormatSequence(const SeqString: String; var aList: TStringList);
var
  tempStr: String;
  i: Integer;
  numSites: Integer;
begin
  aList.Clear;
  if Length(SeqString) > 0 then
  begin
    i := 1;
    numSites := 0;
    tempStr := EmptyStr;
    while i <= Length(SeqString) do
    begin
      while (numSites < SITES_PER_LINE) and (i <= Length(SeqString)) do
      begin
        tempStr := tempStr + SeqString[i];
        inc(i);
        inc(numSites);
        if (numSites mod SITES_PER_SEGMENT) = 0 then
          tempStr := tempStr + ' ';
      end;
      aList.Add(tempStr);
      numSites := 0;
      tempStr := EmptyStr;
    end;
  end;
end;

constructor TFormatConvertToMega.Create(in_strlist: TStrings; in_type : TImportFormatType);
begin
  AStrList := TStringList.Create;
  AStrList.AddStrings(in_strlist);
  CurLine := 0;
  I_created_AStrList := true;
  BStrList := TStringList.Create;
  I_created_BStrList := true;
  // for outside access to data
  SeqDataHash := THashedStringList.Create;
  I_created_SeqDataHash := true;
  InFileType := in_type;
end;

procedure TFormatConvertToMega.UpdateFileType(loadInputData: Boolean = True);
begin
 InFileType := GetInFileTypeExtended(InFile);
 if (AStrList.Count = 0) and loadInputData then
   AStrList.LoadFromFile(InFile);
end;

procedure TFormatConvertToMega.DoLoadInputData;
begin
 if (AStrList.Count = 0) then
   AStrList.LoadFromFile(InFile);
end;

//==================================================================
function GetInFileType(fn: TFileName): TImportFormatType;
//==================================================================
var
  FExt : AnsiString;
  i : TImportFormatType;
begin
  FExt := LowerCase( ExtractFileExt( fn ) );
  Result := gm_NONE;
  for i := Low(FormatNames) to High(FormatNames) do
    if Pos(FExt, FormatNames[i]) = 1 then
    begin
      Result := TImportFormatType(i);
      exit;
    end;
end;

//==================================================================
function GetInFileTypeExtended(fn: TFileName): TImportFormatType;
//==================================================================
var
  FExt,FmtDataLine : AnsiString;
  i : TImportFormatType;
  StringTokens : TStringList = nil;
begin
  try
    StringTokens := TStringList.Create;
    FExt := LowerCase( ExtractFileExt( fn ) );
    Result := gm_NONE;
    for i := Low(LookupFormat) to High(LookupFormat) do
    begin
      StringTokens.Clear;
      FmtDataLine := LookupFormat[i];
      if Pos(',',FmtDataLine) <> 0 then
      begin
        SplitString(FmtDataLine,',',StringTokens);
        if StringTokens.IndexOf(FExt) <> - 1 then
        begin
          Result := TImportFormatType(i);
          exit;
        end;
      end
      else
      begin
        if Pos(FExt, FmtDataLine) = 1 then
        begin
          Result := TImportFormatType(i);
          exit;
        end;
      end;
    end;
  finally
    if Assigned(StringTokens) then
      StringTokens.Free;
  end;
end;


//==================================================================
function SplitString(const S: AnsiString; const Delim: AnsiChar; ts: TStrings): Integer;
//==================================================================
   procedure AddToken(const token: AnsiString);
   begin
      ts.Add(token);
      Inc(Result); // inc Result of Splitstring
   end;
var
 i:integer;
 t:AnsiString;
begin
 t:='';
 Result := 0;
 for i:=1 to Length(s) do
 begin
  if s[i]=delim then
  begin
   AddToken(t);
   t:='';
  end
  else
   t:=t+s[i];
 end;
   if (Length(t) > 0) then
   begin
    AddToken(t);
   end
   else if (Length(s) > 0) and (s[Length(s)] = delim) then
   begin
      AddToken(''); // add a empty string if there is a delimiter the end of
   end;
end;

//==================================================================
procedure TFormatConvertToMega.SetInFile(const fn : TFileName);
//==================================================================
begin
  if FileExists(fn ) then
  begin
    InFile := '';
    InFileType := GetInFileType( fn );
    if (InFileType <> gm_NONE) then
    begin
      InFile := fn;
      AStrList.Clear;
      AStrList.LoadFromFile( InFile );
    end;
  end;
end;

//==================================================================
function TFormatConvertToMega.Convert( instrlist : TStrings; intype: TImportFormatType ): TStringList;
//==================================================================
begin
  AStrList.Assign( instrlist );
  InFileType := intype;
  Result := Convert( intype );
end;

//==================================================================
function TFormatConvertToMega.Convert( intype: TImportFormatType ): TStringList;
//==================================================================
begin
  Result := nil;
  if (AStrList.Count = 0) then
    Exit;

  // write the file header
  BStrList.Clear;
  BStrList.Add( '#Mega' );
  BStrList.Add( '!Title ' + ExtractFileName(InFile) + ';');
  BStrList.Add( '' );
  try
    if (InFileType = gm_IG) then            	convert_from_ig
    else if (InFileType = gm_NBRF) then     	convert_from_nbrf
    else if (InFileType = gm_XML) then      	convert_from_xml
    else if (InFileType = gm_PIR) then      	convert_from_pir
    else if (InFileType = gm_GCG) then      	convert_from_gcg
    else if (InFileType = gm_MSF) then      	convert_from_msf
    else if (InFileType = gm_NEXUS) then    	ConvertFromNexus
    else if (InFileType = gm_FASTA) then    	convert_from_fasta
    else if (InFileType = gm_PHYLIP) then   	convert_from_phylip
    else if (InFileType = gm_ALN) then      	convert_from_aln
    else
       raise Exception.Create('Unknown data file format. Please use one of the prespecified filename extensions so that MEGA can infer the file format.');
    Result := BStrList;
  except
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Conversion failed: ' + E.Message);
      {$ELSE}
      error_nv('Format conversion failed: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TFormatConvertToMega.IsNotSequence(Astr: AnsiString): boolean;
// returns True if aStr contains chars not allowed in a DNA Sequence.
var
  i  : integer;
begin
  Result := true;
  for i := 1 to Length(aStr) do
  begin
    if not (UpCase(aStr[i]) in ['A','T','U','G','C','.','-','?',' ']) then  //Pos(aStr[i], 'acgtnACGTN.- ') = 0) then
     Exit;
  end;
  Result := false;
end;

procedure TFormatConvertToMega.convert_from_fasta;
Var
  NewStr, AStr, CurKey : AnsiString;
  i,CurSize, SeqStart : integer;

  procedure SetSeqDataHash;
  var
    CurSeq,RefSeq,HashString: AnsiString;
    CurPos,j,k: integer;
  const
    delim = '=';
  begin
    if (CurKey = '') or (CurSize = 0) then
      exit;
    CurSeq := '';
    SetLength(CurSeq, CurSize);
    CurPos := 0;
    for j := SeqStart to BStrList.Count-1 do
    begin
      RefSeq := BStrList[j];
      for k := 1 to Length(RefSeq) do
        CurSeq[k+CurPos] := RefSeq[k];
      inc(CurPos, Length(RefSeq));
      //CurSeq := CurSeq + RefSeq;
    end;
    HashString := CurKey + delim + CurSeq;
    SeqDataHash.Add(HashString);
  end;

  function StripIllegalChars(OTUName : AnsiString) : AnsiString;
  begin
    // replace pound with * for now
    while Pos('#', OTUName) > 0 do
      OTUName[Pos('#', OTUName)] := '*';
    // replace blanks with underscore
    while Pos(' ', OTUName) > 0 do
      OTUName[Pos(' ', OTUName)] := '_';
    // replace tab with underscore
    while Pos(#9, OTUName) > 0 do
      OTUName[Pos(#9, OTUName)] := '_';
    while Pos('=', OTUName) > 0 do // We MUST remove '=' else it messes up later because we split the NAME and VALUE with an = sign in the TStringList.
      OTUName[Pos('=', OTUName)] := '_';
    Result := OTUName;
  end;

begin
  CurKey  := '';
  CurSize := 0;
  for i := 0 to AStrList.Count-1 do
  begin
    Inc(CurLine);
    AStr := Trim(AStrList[i]);
    if Astr <> EmptyStr then
    begin
      if AStr[1] = '>' then // Retrieves Name of Sequence
      begin
        SetSeqDataHash;

        // Check for >P1; type formatting and delete to ;
        if Pos(';', AStr) > 0 then
        begin
          // We have a semicolon but is it
        end;
        Delete (Astr, 1, 1);
        // We need to check for illegal characters in name (replace spaces, tabs, and #)
        AStr := StripIllegalChars(AStr);
        CurKey := AStr;
        NewStr := '#';
        BStrList.Add( NewStr+Astr );

        CurSize  := 0;
        SeqStart := BStrList.Count;
      end
      else
      begin  // Retrieves Sequence Data
        BStrList.Add( Astr );
        CurSize := CurSize +Length(Astr);
{
        if (SeqDataHash.IndexOfName(CurKey) = -1) then
          SeqDataHash.Values[CurKey] := Astr
        else
          SeqDataHash.Values[CurKey] := SeqDataHash.Values[CurKey] + Astr;
}
      end;
    end;
  end;
  SetSeqDataHash;
end;

//==================================================================
procedure TFormatConvertToMega.convert_from_ig;
//==================================================================
Var
  StrLen, LstCnt, i, j, k : integer;
  NewStr, Astr, AnsiChr, CurKey : AnsiString;
  Names : array of AnsiString;
begin
  i := 0;
  j := 1;
  NewStr := '#';
  LstCnt := AStrList.Count;

  try
    while (i <= LstCnt-1) do // creates an array of sequence names
    begin
      Astr := Trim(AstrList[i]);
      if (Astr <> '')then
      begin
        if (AStr[1] = ';') then 	// comment line
        begin
          setLength(Names, j);
          Names[j-1] := AstrList[i+1];
          inc(j);
          AStrList.Delete(i);
          dec(LstCnt);
        end
        else
          inc(i);
      end
      else
        inc(i);
    end;

    LstCnt := AStrList.Count;
    for i := 0 to LstCnt-1 do		//adds '#' to Name of sequence and adds sequence
    begin									//to TempStrList
      Astr := Trim(AstrList[i]);
      StrLen := Length(AStr);
      AnsiChr := copy(Astr, StrLen, 1);
      if (Astr <> '') then
      begin
        for k := 0 to j-2 do
        begin
          if (Names[k] = AStr) then
          begin
            Astr := NewStr + Astr;
            CurKey := Astr;
          end;
        end;
        if AnsiChr = '1' then
          Delete (Astr, StrLen, 1)
        else
        begin
          BStrList.Add(Astr);
          if SeqDataHash.IndexOfName(CurKey) <> -1 then
            SeqDataHash.Values[CurKey] := SeqDataHash.Values[CurKey] + Astr;
        end;
      end;
    end;
  finally
    Names := nil;
  end;
end;

//==================================================================
procedure TFormatConvertToMega.convert_from_nbrf;
//==================================================================
Var
  LstCnt, i: integer;
  Astr: AnsiString;
begin
  // Joel Dudley: Just going to remove non-fasta elements and hand off to fasta
  // why duplicate efforts
  LstCnt := AStrList.Count;
  for i := 0 to LstCnt - 1 do
  begin
    if pos(';',AStrList[i]) > 0 then
      AStrList[i] := '>'  + Copy(AStrList[i],pos(';',AStrList[i])+1,LstCnt-1);
    if pos('*',AStrList[i]) > 0 then
    begin
      AStr := AStrList[i];
      Delete(AStr,pos('*',AStrList[i]),1);
      AStrList[i] := AStr;
    end;
  end;
  // Now a fasta formatted file
  convert_from_fasta;

{  LstCnt := AStrList.Count;

  for i := 0 to LstCnt-1 do             //removes '*' from the end of sequence
  begin
    Astr := Trim(AstrList[i]);
    NextPos := Pos('*',AStr);
    if (Astr <> '')then
    begin
      if NextPos > 1 then
    	Delete (Astr, NextPos, 1);
      if (AStr[1] = '>') Then
      begin                             //adds '#' to the name of sequence
    	Astr := AstrList[i+1];
    	NextPos := Pos(' ',AStr);
    	AStr := Copy(Astr, 1, NextPos - 1);
    	NewStr := '#';
    	Astr := NewStr + Astr;
    	BStrList.Add (AStr);
      end
      else
      begin                             //adds Sequence to TempStrList
    	Temp := Trim(UpperCase(AStr));
    	StrLen := Length(Temp);
    	Delete (Temp, 1, StrLen-5);
    	if Temp <> 'BASES' then
    	  BStrList.Add (Astr);
      end;
    end;
  end;}
end;



//==================================================================
procedure TFormatConvertToMega.convert_from_pir;
//==================================================================
var
  i, LstCnt: integer;
  AStr: AnsiString;
begin
  // Joel Dudley: Just going to remove non-fasta elements and hand off to fasta
  // why duplicate efforts
  LstCnt := AStrList.Count;
  for i := 0 to LstCnt - 1 do
  begin
    if pos(';',AStrList[i]) > 0 then
      AStrList[i] := '>'  + Copy(AStrList[i],pos(';',AStrList[i])+1,LstCnt-1);
    if pos('*',AStrList[i]) > 0 then
    begin
      AStr := AStrList[i];
      Delete(AStr,pos('*',AStrList[i]),1);
      AStrList[i] := AStr;
    end;
  end;
  // Now a fast formatted file
  convert_from_fasta;

{  NewStr := '#';
  LstCnt := AStrList.Count;

  For i := 0 to LstCnt-1 do
  begin
    AStr := Trim(AstrList[i]);
    NextPos := Pos(' ', AStr);
    Temp := UpperCase(Copy(AStr, 1, NextPos-1));
    if (Temp = 'ENTRY') then            //Retrieves the name of the sequence
    begin
      Delete(AStr, 1, NextPos);
      AStr := Trim(Astr);
      AStr := NewStr + AStr;
    end
    else                // Deletes pir file of any numbers or spaces in sequence
    begin               // As well as removes all non-sequences
      Len := Length(AStr);
      j := 1;           // added
      while j <= Length(AStr) do  // used to be for j := 1 to Len do
      begin
    	for k := 0 to 9 do
    	begin
    	  num := IntToStr(k);
    	  if(AStr[1] = num)then
    	  begin
    		NextPos := Pos(' ', AStr);
    		Delete(AStr, 1, NextPos);
    	  end;
    	end;
    	if (AStr[j] = ' ') then
    	  Delete(AStr, j, 1);
            Inc(j); // added
      end;
      AStr := Trim(AStr);
      if IsNotSequence(AStr) then
       Astr := '';
    end;
    if (Astr <> '') Then
      BStrList.Add (AStr);
  end;}
end;


//==================================================================
procedure TFormatConvertToMega.convert_from_xml;
//==================================================================
Var
  NextPos,  Proc, LstCnt, i     : integer;
  NewStr, Temp, AStr, CurKey       : AnsiString;
begin
  CurKey := EmptyStr;
  Temp := EmptyStr;
  NewStr := '#';

  LstCnt := AStrList.Count;

  for i := 0 to LstCnt-1 do     //removes '>' from text
  begin
     AStr := Trim(AStrList[i]);
     NextPos := Pos('>', AStr);
     if NextPos >= 1 Then
       Temp := Copy(AStr, 1, NextPos - 1);

     NextPos := Pos('<', Temp);          //removes '<' from text
     if NextPos >= 1 Then
       Delete(Temp, 1, NextPos);
     Temp := Trim(Temp);

     if Temp = 'name' then        //assigns value for a process
       Proc := 1
     else if Temp = 'seq-data' then
       Proc := 2
     else
       Proc := 0;

     Case Proc of                        //retrieves names of the sequence
       1 : begin
     		NextPos := Pos('>', AStr);
     		if NextPos >= 1 Then
     		  Delete(AStr, 1, NextPos);

     		NextPos := Pos('<', AStr);
     		if NextPos >= 1 Then
     		begin
     		  Astr := Copy(AStr, 1, NextPos -1);
     		  AStr := NewStr + AStr;
     		end;
     	  end;
       2 : begin                      //removes characters that don't belong in the sequence
     		NextPos := Pos('>', AStr);
     		if NextPos >= 1 Then
     		  Delete(AStr, 1, NextPos);

     		NextPos := Pos('<', AStr);
     		if NextPos >= 1 Then
     		  Astr := Copy(AStr, 1, NextPos -1);
     	  end;
       else
     	if IsNotSequence(Astr) then
     	  Astr := '';
       end;
       if (AStr <> '') Then
       begin
     	 BStrList.Add (Astr);
         if SeqDataHash.IndexOfName(CurKey) <> -1 then
            SeqDataHash.Values[CurKey] := SeqDataHash.Values[CurKey] + Astr;
       end;
     end;
end;

//==================================================================
procedure TFormatConvertToMega.convert_from_gcg;
//==================================================================
var
  NewStr, TempStr1, TempStr2, num, Temp, CurKey         : AnsiString;
  i, LstCnt, StrLen, NextPos, j                 : integer;
begin
  BStrList.Clear;
  for i := 0 to AStrList.Count-1 do
    BStrList.Add( AStrList[i] );

  LstCnt := BStrList.Count;
  NewStr := '#';

  for i := 0 to (LstCnt - 1) do         //retrieves the name of the sequence
  begin
    TempStr1 := Trim(BStrList[i]);
    if (TempStr1 <> '') Then
    begin
      Temp := Trim(UpperCase(TempStr1));
      StrLen := Length(Temp);
      Delete (Temp, 1, StrLen-12);
      Temp := UpperCase(Copy(Temp, 1, 5));
      if (Temp = 'CHECK') Then
      begin
    	if ((i-1) >= 0) then
    	begin
    	  TempStr2 := Trim(BStrList[i-1]);
    	  TempStr2 := Trim(NewStr + TempStr2);
    	  BStrList.Delete(i-1);
    	  BStrList.Insert ((i-1), TempStr2);
    	end;
    	BStrList.Delete(i);
    	BStrList.Add ('');
      end;
    end;
  end;

  for i := 0 to (LstCnt - 1) do    //removes numerical value from sequence
  begin
    TempStr1 := Trim(BStrList[i]);
    if (TempStr1 <> '') Then
    begin
      for j := 0 to 9 do
      begin
    	num := IntToStr(j);
    	if(TempStr1[1] = num)then
    	begin
    	  NextPos := Pos(' ', TempStr1);
    	  Delete(TempStr1, 1, NextPos);
    	  BStrList.Delete(i);
    	  BStrList.Insert (i, TempStr1);
          CurKey := BStrList[i-1];
          if SeqDataHash.IndexOfName(CurKey) <> -1 then
            SeqDataHash.Values[CurKey] := SeqDataHash.Values[CurKey] + TempStr1;
    	end;
      end;
    end;
  end;
end;

//==================================================================
procedure TFormatConvertToMega.convert_from_msf;
//==================================================================
var
  TempStr1, TempStr2, NewStr, CurKey : AnsiString;
  InStrList : TStringList;
  Names : Array of AnsiString;
  i, j, k, NextPos, LstCnt : integer;
begin
  k := 1;
  NewStr := '#';
  InStrList := nil;

  try
    InStrList := TStringList.Create;
    for i := 0 to AStrList.Count-1 do
      InStrList.Add( AStrList[i] );

    i := 0;
    while not (InStrlist[i] = '//') do
    begin
      TempStr1 := Trim(InStrList[i]);
      NextPos := Pos(' ', TempStr1);
      TempStr2 := Copy(TempStr1, 1, NextPos-1);

      if (TempStr2 <> 'Name:') Then   //retrieves the name of the sequence for the text
        InStrList.Delete(i)
      else
        i := i + 1;
    end;

    LstCnt := InStrList.Count;

    for i :=  0 to LstCnt -1 do           //determines the number of sequences in the text
    begin
      TempStr1 := Trim(InStrList[i]);
      NextPos := Pos(' ', TempStr1);
      TempStr2 := Copy(TempStr1, 1, NextPos-1);

      if (TempStr2 = 'Name:') Then  //Removes Name: from the each
      begin
        Delete(TempStr1, 1, NextPos);
        NextPos := Pos(' ', TempStr1);
        setLength(Names, k);
        Names[k-1] := Copy(TempStr1, 1, NextPos-1); //Holds names of all sequences
        k := k+1;
      end;
    end;

    for i := 0 to (k-2) do        //Assigns the appropiate name to a given sequence
    begin
      BStrList.Add (NewStr + Names[i]);
      CurKey := Names[i];
      for j := 0 to LstCnt - 1 do
      begin
        TempStr1 := Trim(InStrList[j]);
        NextPos := Pos(' ', TempStr1);
        TempStr2 := Copy(TempStr1, 1, NextPos-1);
        TempStr2 := Trim(TempStr2);

        if (Names[i] = TempStr2) Then
        begin
      	Delete(TempStr1, 1, NextPos);
      	TempStr1 := Trim(TempStr1);
      	BStrList.Add (TempStr1);
        if SeqDataHash.IndexOfName(CurKey) = -1 then
          SeqDataHash.Values[CurKey] := TempStr1
        else
          SeqDataHash.Values[CurKey] := SeqDataHash.Values[CurKey] + TempStr1;
        end;
      end;
      BStrList.Add (emptystr);
    end;
  finally
    begin
      if InStrList <> nil then InStrList.Free;
      Names := nil;
    end;
  end;
end;

procedure TFormatConvertToMega.convert_from_nexus;
var
  slKeyPair : TStringList;
  NoLabels, Interleave : Boolean;

  function SlClean( SL : TStringList) : TStringList;
  var
    i : Integer;
    Done : Boolean;
  begin
    // this is an ugly function
    Done := False;
    while not Done do
    for i := 0 to SL.Count - 1 do
    begin
      if i = SL.Count - 1 then
        Done := True;
      if (Trim(SL[i]) = '') or ((Pos('[',SL[i]) <> 0) and (Pos(']',SL[i]) <> 0)) then
      begin
        SL.Delete(i);
        Break;
      end;
    end;
    Result := SL;
  end;

var
  UfDataLine : AnsiString;
  i,j: integer;
  StringTokens, SubStringTokens : TStringList;
begin
  UfDataLine := AStrList.Text;
  slKeyPair := TStringList.Create;
  if Pos('nolabels',LowerCase(UfDataLine)) <> 0 then
    NoLabels := True
  else
    NoLabels := False;
  if Pos('interleave',LowerCase(UfDataLine)) <> 0 then
    Interleave := True
  else
    Interleave := False;
  // Why are we converting all carriage return and line feed characters to blank spaces?
  While Pos(#10,UfDataLine) <> 0  do
    UfDataLine[Pos(#10,UfDataLine)] := ' ';
  While Pos(#13,UfDataLine) <> 0  do
    UfDataLine[Pos(#13,UfDataLine)] := ' ';

  StringTokens := TStringList.Create;
  SplitString(UfDataLine,';',StringTokens);
  SubStringTokens := TStringList.Create;
  slKeyPair := TStringList.Create;  // Creating a string list TWICE (created on line 2 of this function).
  try
    for i := 0 to StringTokens.Count - 1 do
    begin
      if Pos('taxlabels',LowerCase(StringTokens[i])) <> 0 then
      begin
        SubStringTokens.Clear;
        SplitString(Trim(StringTokens[i]),' ',SubStringTokens);
        SubStringTokens := SlClean(SubStringTokens);
        SubStringTokens.Delete(0);  //remove the section label
      for j := 0 to SubStringTokens.Count - 1 do
      begin
        slKeyPair.Values[Trim(SubStringTokens[j])] := ' ';
      end;
    end;

    if Pos('matrix',LowerCase(StringTokens[i])) <> 0 then
    begin
      SubStringTokens.Clear;
      SplitString(Trim(StringTokens[i]),' ',SubStringTokens);
      SubStringTokens := SlClean(SubStringTokens);
      SubStringTokens.Delete(0);
      While SubStringTokens[0][1] = '[' do
      begin
        while (Pos(']',SubStringTokens[0]) = 0) and (SubStringTokens[0][1] <> ']') do
          SubStringTokens.Delete(0);
        SubStringTokens.Delete(0);
      end;

      //BStrList.Text := SubStringTokens.Text;
      for j := 0 to SubStringTokens.Count - 1 do
      begin
          if NoLabels then
          begin
            if Interleave then
            begin
              slKeyPair.Values[slKeyPair.Names[j]] := Trim(slKeyPair.Values[SubStringTokens[j-1]]) + SubStringTokens[j];  //SK FIX
            end
            else
              slKeyPair.Values[slKeyPair.Names[j]] := SubStringTokens[j];
          end
          else
          begin
            if Interleave then
            begin
              if j mod 2 <> 0 then
                slKeyPair.Values[SubStringTokens[j-1]] := Trim(slKeyPair.Values[SubStringTokens[j-1]]) + SubStringTokens[j];
            end
            else
              if j mod 2 <> 0 then
                slKeyPair.Values[SubStringTokens[j-1]] := SubStringTokens[j];
          end;
      end;
    end;
  end;

  for i := 0 to (slKeyPair.Count - 1) do
  begin
    BStrList.Add('#' + slKeyPair.Names[i]);
    BStrList.Add(slKeyPair.Values[slKeyPair.Names[i]]);
    SeqDataHash.Values[slKeyPair.Names[i]] := slKeyPair.Values[slKeyPair.Names[i]];
    BStrList.Add('');
  end;
  finally
    // Ummm, clean up some stuff you slob!
  end;
end;

procedure TFormatConvertToMega.ConvertFromNexus;
var
  converter: TNexusAlignmentConverter = nil;
begin
  try
    try
      converter := TNexusAlignmentConverter.Create;
      if not converter.ConvertNexusToMegaAlignment(FInFile, BStrList) then
        ShowMessage('Conversion failed: ' + converter.LogStrings.Text);
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(converter) then
      converter.Free
  end;
end;

/// <summary>Converts the strings of a Phylip formatted alignment file to the
/// native MEGA format. It is assumed that the Phylip format is strictly adhered
/// so that all sequences have an equal number of characters.
procedure TFormatConvertToMega.convert_from_phylip;
var
  ReadingFile: Boolean;
  TempSeq: AnsiString;
  SeqStartsAt: Integer;
  SeqPosSameLine: Boolean;
  CurStr: AnsiString;
  NumSitesStr: AnsiString;
  LengthSitesStr: Integer;
  LengthTaxaStr: Integer;
  NumTaxaStr: AnsiString;
  NumSites: Integer;
  NumTaxa: Integer;
  NumbersLine: AnsiString;
  Names : Array of AnsiString;
  Seqs : Array of AnsiString;
  i, j: integer;
  FIsInterleaved: Boolean;
  FAskedInterleaved: Boolean;
  useLongNames: Boolean;
  formattedSeqs: TStringList;

  function IsInterleaved: Boolean;
  begin
   if not FAskedInterleaved then
   begin
     FIsInterleaved := (MessageDlg('Is this file interleaved?', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
     FAskedInterleaved := True;
   end;
   result := FIsInterleaved;
  end;

  procedure ReadName;
  var
    FName: AnsiString;
    TempStr: AnsiString;
  begin
    try
      CurStr := TrimRight(AStrList[i]);
      if SeqPosSameLine then
      begin
        TempStr := Trim(CurStr);
        if (Length(TempStr) = 10 + NumSites) and (not useLongNames) then
        begin
          FName := Copy(TempStr, 1, 10); // Name is max 10 char length
          SeqStartsAt := 11;
        end
        else
        begin
          if not UseLongNames then
            MessageDlg('Please check your results carefully', mtWarning, [mbOK], 0);
          useLongNames := True;
          if TempStr[1] = #39 then
          begin
            FName := Copy(TempStr, 1, PosEx(#39, TempStr, 2));
            SeqStartsAt := PosEx(#39, TempStr, 2) + 1;
            FName := Copy(FName, 2, Length(FName) - 2);
            FName := StringReplace(FName, ' ', '_', [rfReplaceAll]);
          end
          else if TempStr[1] = #34 then
          begin
            FName := Copy(TempStr, 1, PosEx(#34, TempStr, 2));
            SeqStartsAt := PosEx(#34, TempStr, 2) + 1;
            FName := Copy(FName, 2, Length(FName) - 2);
            FName := StringReplace(FName, ' ', '_', [rfReplaceAll]);
          end
          else
          begin
            FName := Copy(TempStr, 1, Pos(' ', TempStr)-1);
            SeqStartsAt := Pos(' ', TempStr);
          end;

          while TempStr[SeqStartsAt] = ' ' do
            inc(SeqStartsAt);
        end;
      end
      else
      begin
        FName := Trim(CurStr);
        SeqStartsAt := 1;
        inc(i);
      end;
      if Trim(FName) <> EmptyStr  then
      if (j >= NumTaxa) then
      begin
        if (not SameText(FName, Names[j mod NumTaxa])) or (not IsInterleaved) then
          raise Exception.Create('Invalid name found at line ' + IntToStr(i + 1));
      end
      else
        Names[j] := FName;
    except
      on E:Exception do
        raise Exception.Create('Error in ReadName: ' + E.Message);
    end;
  end;

  procedure ReadSeq;
  begin
    try
      CurStr := TrimRight(AStrList[i]);
      TempSeq := StripSpaces(Copy(CurStr, SeqStartsAt, Length(CurStr)));
      if Length(TempSeq) < NumSites then
      begin
        // We either have a limited number of sites per line, or are interleaved.
        if not IsInterleaved then
        begin
          while Length(TempSeq) < NumSites do
          begin
            inc(i);
            TempSeq := TempSeq + StripSpaces(Copy(AStrList[i], 1, Length(AStrList[i])));
          end;
        end;
      end;
      if j >= NumTaxa then
        Seqs[j mod NumTaxa] := Seqs[j mod NumTaxa] + TempSeq
      else
        Seqs[j] := TempSeq;
      inc(i);
    except
      on E:Exception do
        raise Exception.Create('Error in ReadSeq: ' + E.Message);
    end;
  end;

begin
  try
    try
      useLongNames := False;
      FormattedSeqs := TStringList.Create;
      // Read the first line for the #taxa and the # sites.
      NumbersLine := Trim(AStrList[0]);
      LengthTaxaStr := Pos(' ', NumbersLine)-1;
      if LengthTaxaStr < 1 then
        LengthTaxaStr := Length(NumbersLine);
      NumTaxaStr := Copy(NumbersLine, 1, LengthTaxaStr);
      if (not TryStrToInt(NumTaxaStr, NumTaxa)) then
        Raise Exception.Create('Expected number of sequences, no number found.');
      NumbersLine := Trim(Copy(NumbersLine, LengthTaxaStr+1, Length(NumbersLine)));
      if NumbersLine = EmptyStr then
        Raise Exception.Create('Expected number of sites, no number found.');
      if Pos(' ', NumbersLine) <> 0 then
        LengthSitesStr := Pos(' ', NumbersLine)-1
      else
        LengthSitesStr := Length(NumbersLine);
      NumSitesStr := Copy(NumbersLine, 1, LengthSitesStr);
      if (not TryStrToInt(NumSitesStr, NumSites)) then
        Raise Exception.Create('Expected number of sites, no number found.');
      // By now we have the number of Taxa and number of Sites.  If someone has an invalid file or wrong format they also know by now.
      if Length(Copy(NumbersLine, LengthSitesStr+1, Length(NumbersLine))) > 0 then
        Raise Exception.Create('The first line should ONLY contain the number of sequences, and the number of sites.  eg. 2 150 would mean 2 sequences, 150 sites.');
       // MEGA Freaks if I give a !Format without specifying a DataType.  It will ask me yet ignore the answer.

      // Setup the arrays to hold the names and sequences.
      SetLength(Names, NumTaxa);
      SetLength(Seqs, NumTaxa);
      i := 1;
      while Trim(AStrList[i]) = EmptyStr do
      begin
        inc(i);
      end;
      CurStr := Trim(AStrList[i]);
      // This is the first line of text after the numbers.  Determine if we only have a name (the seq is next line), otherwise it's on the same line.
      SeqPosSameLine := not ((Length(CurStr) >= 1) and (Length(CurStr) <= 10));

      ReadingFile := True;
      j := 0;
      while ReadingFile do
      begin
        while Trim(AStrList[i]) = EmptyStr do
        begin
          inc(i);
        end;
        ReadName;
        ReadSeq;
        inc(j);
        if ((j >= NumTaxa) and (Length(Seqs[NumTaxa-1]) >= NumSites)) or (i >= AStrList.Count) then // G.S. 6-15-2011, added the test on i because malformed files can cause an index out of bounds error
        begin   // G.S. 6-15-2011, added a check to make sure that all sequences have the correct number of sites.
          i := 0;
          while i < numTaxa do
          begin
            if Length(Seqs[i]) <> numSites then
              Raise Exception.Create('A sequence with an incorrect number of sites was encountered. ' + IntToStr(NumSites) + ' sites were expected but ' + IntToStr(Length(Seqs[i])) + ' were found for sequence #' + IntToStr(i + 1) + '. Please validate your Phylip file.');
            inc(i);
          end;
          ReadingFile := False;
        end
        else if (j >= NumTaxa) and (not IsInterleaved) then
          raise Exception.Create('Number of taxa specified does not match the data found in this file. Maybe it is interleaved?');
      end;

      i:=0;
      while i <= NumTaxa-1 do
      begin
        BStrList.Add('#' + Names[i]);
        FormatSequence(Seqs[i], FormattedSeqs);
        BStrList.AddStrings(FormattedSeqs);
        BStrList.Add(EmptyStr);
        // G.S. 6-15-2011, added code to update SeqDataHash. This was missing so that nothing would get loaded into alignment editor after the conversion
        if SeqDataHash.IndexOfName(Names[i]) = -1 then
          SeqDataHash.Values[Names[i]] := Seqs[i]
        else
          SeqDataHash.Values[Names[i]] := SeqDataHash.Values[Names[i]] + Seqs[i];
        inc(i);
      end;
    except
      on E: Exception do
      begin
        ShowMessage('There was a problem converting your file to the MEGA format.' + #10#13 + 'Error: ' + E.Message);
      end;
    end;
  finally
    if Assigned(FormattedSeqs) then
      FormattedSeqs.Free;
  end;
end;

//==================================================================
procedure TFormatConvertToMega.convert_from_aln;
//==================================================================
var
  TempStr1, TempStr2, NewStr, CurKey: AnsiString;
  Names : Array of AnsiString;
  i, j, k, NextPos,LastChrPos : integer;
begin
  k := 1;
  NewStr := '#';
  //i := 3;  We can't make the assumption that there are two blank lines (user reported a bug where some programs save with only one line).

  try
    i := 1;  // The first line has the CLUSTAL Type Version ... information
    while Trim(AstrList[i]) = EmptyStr do
      inc(i);

    while (i < AStrList.Count) and (AstrList[i] <> EmptyStr) do
     begin
       TempStr1 := Trim(AStrList[i]);
       NextPos := Pos(' ', TempStr1);
       TempStr1 := Copy(TempStr1, 1, NextPos-1);

       if IsNotSequence(TempStr1) Then
       begin
         setLength(Names, k);
         Names[k-1] := TempStr1;  //Collects the names of the various sequence
         k := k+1;
       end;
       i := i+1;
    end;

    for i := 0 to (k-2) do    //Groups the various sequences according to the name
    begin
      BStrList.Add (NewStr + Names[i]);
      CurKey := Names[i];
      for j := 1 to AStrList.Count-1 do
      begin
        TempStr1 := Trim(AStrList[j]);
        NextPos := Pos(' ', TempStr1);
        TempStr2 := Copy(TempStr1, 1, NextPos-1);
        TempStr2 := Trim(TempStr2);

        if (Names[i] = TempStr2) Then
        begin
      	  Delete(TempStr1, 1, NextPos);
      	  TempStr1 := Trim(TempStr1);
          LastChrPos := Length(TempStr1) - 1;
          if LastChrPos = 0 then
		    continue;
          if TempStr1[LastChrPos] in ['a'..'z','A'..'Z','-'] then
          begin
      	    BStrList.Add (TempStr1);
            if (SeqDataHash.IndexOfName(CurKey) = -1) then
              SeqDataHash.Values[CurKey] := TempStr1
            else
              SeqDataHash.Values[CurKey] := SeqDataHash.Values[CurKey] + TempStr1;
          end;
        end;
      end;
      BStrList.Add (emptystr);
    end;
  finally
    Names := nil;
  end;
end;

end.
