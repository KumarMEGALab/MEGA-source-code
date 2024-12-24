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

unit MCodons;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, ExcelWrite;

const
  BaseT = 0;
  BaseC = 1;
  BaseA = 2;
  BaseG = 3;

type
  TCodonMap = AnsiChar;

  TCodons=class(TObject)
  protected
    FCodeTable: array [#0..#63] of AnsiChar;
    Mask:       array [0..2] of Integer;
    Shifts:     array [0..2] of Integer;
    MaskAll:    Integer;
  private
    FRedundancy: array [#0..#63, 0..2] of Integer;

    FValid: Boolean;         // True if code table set
    FHasRedundancy: Boolean; // True if Redundancy computed
    FHasExtRedundancy: Boolean;  // if extended redundancy is computed

    function  GenerateRedundancy: Boolean;
    function  GenerateExtRedundancy: Boolean;
    procedure SetCodeTable(Value: AnsiString);
    function  GetRedundancy(Codon: TCodonMap; Pos: Integer): Integer;
    function  GetExtendedRedundancy(Codon: TCodonMap; Pos: Integer): Integer;
  private
    function CppCompareCodonPos(x,y: TCodonMap; Pos: Integer): Integer; // 0= none; -1: Ts; +1 Tv
    function CppNoOfSynSites(Table: PAnsiChar; Codon: TCodonMap; ratio: Double): Double;
  public
    function CppRedundancyAt(Table: PAnsiChar; Codon: TCodonMap; Pos: Integer): Integer;
    function CppExtRedundancyAt(Table: PAnsiChar; Codon: TCodonMap; Pos: Integer): Integer;
  public
    constructor Create;

    function  MakeCodonBitMap(Codon: PAnsiChar): TCodonMap;
    function  MakeCodonString(BitMap: TCodonMap): AnsiString;  // !!!!!!WARNING!!!!!! Before using MakeCodonString, read the warning message above its implementation
    function  IsAmbiguousCodon(Map: TCodonMap): Boolean;
    function  BaseAt(Codon: TCodonMap; Pos: Integer): Integer;
    function  MutateTo(GetCodon, GiveCodon: TCodonMap; Pos: Integer): TCodonMap;


    function  NoOfDifferences(Map1, Map2: TCodonMap): Integer;
    function  GetNoOfSynSites(Codon: TCodonMap; ratio: Double): Double;
    function  GetAminoAcid(Codon: TCodonMap): AnsiChar;
    function  GetIntAminoAcid(Codon: Integer): AnsiChar;

    function  DiffType(x, y: TCodonMap; Pos: Integer): Integer; //0: None; 1: Ts; 2:Tv
    function  IsDifferent(x, y: TCodonMap; Pos: Integer): Boolean;
    function  IsTsDiff(x, y: TCodonMap; Pos: Integer): Boolean;
    function  IsTvDiff(x, y: TCodonMap; Pos: Integer): Boolean;

    property Valid: Boolean read FValid;
    property CodeTable: AnsiString write SetCodeTable;
    property AminoAcid[Codon: TCodonMap]: AnsiChar read GetAminoAcid;
    property IntAminoAcid[Codon: Integer]: AnsiChar read GetIntAminoAcid;
    property Redundancy[Codon: TCodonMap; Site:Integer]: Integer read GetRedundancy;
    property ExtendedRedundancy[Codon: TCodonMap; Site:Integer]: Integer read GetExtendedRedundancy;
  end;

  TCodonInfo = Class(TCodons)    // For estimating Syn and Nonsyn changes
  public
    FSynSiteTable: array [#0..#63] of Double; //exact noOfSyn sites in a codon
    FDsDnTsMat:    array [#0..#63, #0..#63] of Integer; // Ts Ds/Dn matrix
    FDsDnTvMat:    array [#0..#63, #0..#63] of Integer; // Tv Ds/Dn matrix
    FTsTvRatio:    Double;                    // Ts/tv rate ratio; by default = 1 for Nei-Gojobori
  private
    FHasSynSiteTable : Boolean;
    FHasDsDnMatrices   : Boolean;
  private
    procedure CppGetCodonDiffPathwayCounts(x,y: TCodonMap);
    procedure InitDsDnMatElts(X,Y: TCodonMap; sTs, nTs, sTv, nTv: Integer);

  private
    procedure DiffInOne(IntX, IntY: Integer; Pos: Integer);
    procedure DiffInTwo(IntX, IntY: Integer; Pos1, Pos2: Integer);
    procedure DiffInThree(IntX, IntY: Integer);
    function  TotalDsDn(x,y: TCodonMap): Integer;

    function GetSynSites(Codon: TCodonMap): Double;
    function GetNonsynSites(Codon: TCodonMap): Double;
    function GetSynDiff(Map1, Map2: TCodonMap): Double;
    function GetNonsynDiff(Map1, Map2: TCodonMap): Double;
    function GetTsSynDiff(Map1, Map2: TCodonMap): Double;
    function GetTsNonsynDiff(Map1, Map2: TCodonMap): Double;
    function GetTvSynDiff(Map1, Map2: TCodonMap): Double;
    function GetTvNonsynDiff(Map1, Map2: TCodonMap): Double;

  public
    constructor Create;
    function GenerateSynSiteTable: Boolean;  // Use TsTvRatio controls NG/Ina
    function GenerateDsDnMatrices: Boolean;  // Use TsTvRatio controls NG/Ina

    function WriteIntrinsicTables(List: TStringList; SynSiteFlag, DsDnFlag: Boolean): Boolean;
    function WriteIntrinsicTablesExcel(Xls: TExcelWrite; SynSiteFlag, DsDnFlag: Boolean): Boolean;

    // User specifies the following two things
    property CodeTable;   // from the inherited class
    property TsTvRatio: Double read FTsTvRatio write FTsTvRatio;

    // functions provided by the inherited class
    property IntAminoAcid;
    property AminoAcid;

    // User can read the following things
       // Redundancy at a site
    property Redundancy;  // comes the inherited class;
    property ExtendedRedundancy; // comes from inherited class

    property HasSynSiteTable: Boolean read FHasSynSiteTable;
    property HasDsDnMatrices: Boolean read FHasDsDnMatrices;
    
       // Number of syn or nonsyn sites
    property SynSites[Codon: TCodonMap]: Double read GetSynSites;
    property NonsynSites[Codon: TCodonMap]: Double read GetNonSynSites;

       // Number of syn or nonsyn differences
    property SynDiff[Map1, Map2: TCodonMap]: Double read GetSynDiff;
    property NonsynDiff[Map1, Map2: TCodonMap]: Double read GetNonsynDiff;

       // number of ts or tv type syn differences
    property TsSynDiff[Map1, Map2: TCodonMap]: Double read GetTsSynDiff;
    property TvSynDiff[Map1, Map2: TCodonMap]: Double read GetTvSynDiff;

       // number of ts or tv type nonsyn differences
    property TsNonsynDiff[Map1, Map2: TCodonMap]: Double read GetTsNonsynDiff;
    property TvNonsynDiff[Map1, Map2: TCodonMap]: Double read GetTvNonsynDiff;
  end;


implementation

Uses
  SysUtils, MegaUtils,
  MegaErrUtils,
  ErrorMessages_HC;

constructor TCodons.Create;
var
  i: AnsiChar;
  k: Integer;
begin
  FValid := False;
  FHasRedundancy := False;
  FHasExtRedundancy:= False;

  Shifts[0] := 4;
  Shifts[1] := 2;
  Shifts[2] := 0;

  for k:= 0 to 2 do
    Mask[k] := 3 shl Shifts[k];
  MaskAll := Mask[0] or Mask[1] or Mask[2];

  for i:=#0 to #63 do
  begin
    FCodeTable[i] := '*';
    FRedundancy[i][0] := 0;
    FRedundancy[i][1] := 0;
    FRedundancy[i][2] := 0;
  end;
end;

//--- Set up codon Table definition
procedure TCodons.SetCodeTable(Value: AnsiString);
var
  i: Integer;
begin
  FValid := False;
  if Length(Value) <> 64 then
    RaiseErrorMessage(HC_Unexpected_Error, 'Invalid code table: '+Value);
  for i:= 0 to 63 do
  begin
    if IsUnambiguousAminoAcid(Value[i+1]) then
      FCodeTable[AnsiChar(i)] := UpCase(Value[i+1])
    else
      FCodeTable[AnsiChar(i)] := '*';
  end;;
  FValid := True;
  FHasRedundancy := False;
end;

//--- No of diffs between codons
function TCodons.NoOfDifferences(Map1, Map2: TCodonMap): Integer;
var
  IntMap1, IntMap2, i, XoredProduct: Integer;
begin
  IntMap1 := Integer(Map1);
  IntMap2 := Integer(Map2);
  Result := 0;
  XoredProduct := IntMap1 xor IntMap2;    // only those which are same will be 00
  for i:=0 to 2 do
    if (XoredProduct and Mask[i]) <> 0 then
      Inc(Result);
end;

//----Convert the sets of three nucleotides to bitmapped codons
function TCodons.makeCodonBitMap(Codon: PAnsiChar): TCodonMap;
var
  IntMap, i: Integer;
begin
  IntMap := 0;
  for i:=0 to 2 do
  begin
    case UpCase(Codon[i]) of
      'A'     :  IntMap := IntMap or (BaseA shl Shifts[i]);
      'T', 'U':  IntMap := IntMap or (BaseT shl Shifts[i]);
      'C'     :  IntMap := IntMap or (BaseC shl Shifts[i]);
      'G'     :  IntMap := IntMap or (BaseG shl Shifts[i]);
    else
      IntMap := IntMap or (3 shl 6);  // adds 0x11000000 into the codon
    end;
  end;
  Result := AnsiChar(IntMap);
end;

{ !!!!!!WARNING!!!!!!WARNING!!!!!!WARNING!!!!!!WARNING
  TCodons.MakeCodonString should only be used to create
  codon strings for display purposes. When TCodons.MakeCodonBitmap
  is called, the 3 codon chars are compressed into an Integer and
  anything that is not an 'A', 'G', or 'C' is converted to 'U'.
  So if an alignment with indels was loaded, all indel chars would
  be converted to 'U' chars. You have been warned.
  !!!!!!WARNING!!!!!!WARNING!!!!!!WARNING!!!!!!WARNING
}
//----Convert a bitmap to real codon
function TCodons.MakeCodonString(BitMap: TCodonMap): AnsiString;
var
  NextNuc, IntMap, i: Integer;
  TheCodon: array[0..3] of  AnsiChar;
begin
  IntMap := Integer(BitMap);
  TheCodon[3] := #0;
  for i:=0 to 2 do
  begin
    NextNuc := (IntMap and Mask[i]) shr Shifts[i];
    case NextNuc of
      BaseA:  TheCodon[i] := 'A';
      BaseT:  TheCodon[i] := 'U';
      BaseC:  TheCodon[i] := 'C';
      BaseG:  TheCodon[i] := 'G';
      else
        TheCodon[i] := '?';
    end;
  end;
  Result := TheCodon;
end;

function  TCodons.BaseAt(Codon: TCodonMap; Pos: Integer): Integer;
var
  IntCodon: Integer;
begin
  if (Pos > 2) or (Pos < 0) or (Codon >#63) then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);

  IntCodon := Integer(Codon);
  Result := IntCodon shr Shifts[Pos];
  Result := Result and Mask[2];  //third base is the rightmost so used to mask
end;

function TCodons.MutateTo(GetCodon, GiveCodon: TCodonMap; Pos: Integer): TCodonMap;
var
  IntGetCodon, IntGiveCodon, TempCodon: Integer;
begin
  IntGetCodon  := Integer(GetCodon);
  IntGiveCodon := Integer(GiveCodon);
  TempCodon    := IntGetCodon and (not Mask[Pos]);
  TempCodon    := (TempCodon or (IntGiveCodon and Mask[Pos])) and MaskAll;
  Result := TCodonMap(TempCodon);
end;

//---Gets redundancy
function TCodons.GetRedundancy(Codon: TCodonMap; Pos: Integer): Integer;
begin
  if (not FValid) or (Pos > 2) or (Pos < 0) or (Codon >#63) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Invalid argument in TCodons.GetRedundancy');
  if not FHasRedundancy then
    GenerateRedundancy;
  Result := FRedundancy[Codon][Pos];
  if Result = 3 then
    Result := 2;
end;

//----Gets Extended redundancy
function TCodons.GetExtendedRedundancy(Codon: TCodonMap; Pos: Integer): Integer;
begin
  if (not FValid) or (Pos > 2) or (Pos < 0) or (Codon >#63) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Invalid argument in TCodons.GetExtendedRedundancy');
  if not FHasExtRedundancy then
    GenerateExtRedundancy;
  FHasExtRedundancy := True;
  Result := FRedundancy[Codon][Pos];
end;

//---Gets number of syn sites in a codon
function TCodons.GetNoOfSynSites(Codon: TCodonMap; ratio: Double): Double;
begin
  if not FValid then
    RaiseErrorMessage(HC_Unexpected_Error, 'Codon table missing in TCodons.GetNoOfSynSites');
  Result := CppNoOfSynSites(FCodeTable, Codon, ratio);
end;

function TCodons.GetAminoAcid(Codon: TCodonMap): AnsiChar;
begin
  if not FValid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  if Codon > #63 then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  Result := FCodeTable[Codon];
end;

function TCodons.GetIntAminoAcid(Codon: Integer): AnsiChar;
begin
  if not FValid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  if Codon > 63 then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  Result := FCodeTable[AnsiChar(Codon)];
end;

//---------------------------------------------------------
function TCodons.GenerateRedundancy: Boolean;
var
  Codon: AnsiChar;
  Pos: Integer;
begin
  if not FValid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  for Codon := #0 to #63 do
    for Pos:=0 to 2 do
      FRedundancy[Codon][Pos] := CppRedundancyAt(FCodeTable, TCodonMap(Codon), Pos);
  FHasRedundancy := True;
  Result := True;
end;

function TCodons.GenerateExtRedundancy: Boolean;
var
  Codon: AnsiChar;
  Pos: Integer;
begin
  if not FValid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  for Codon := #0 to #63 do
    for Pos:=0 to 2 do
      FRedundancy[Codon][Pos] := CppExtRedundancyAt(FCodeTable, TCodonMap(Codon), Pos);
  FHasRedundancy := True;
  Result := True;
end;

//--- some 'Is' functions
function TCodons.IsAmbiguousCodon(Map: TCodonMap): Boolean;
begin
  Result := (Map > #63);
end;

//--- If a site is different
function TCodons.IsDifferent(x, y: TCodonMap; Pos: Integer): Boolean;
begin
  Result := (CppCompareCodonPos(x,y,pos) <> 0);
end;

//--- Is Transitional difference at a site
function TCodons.IsTsDiff(x, y: TCodonMap; Pos: Integer): Boolean;
begin
  Result := (CppCompareCodonPos(x,y,pos) < 0);
end;

// Is Transversional difference at a site
function TCodons.IsTvDiff(x, y: TCodonMap; Pos: Integer): Boolean;
begin
  Result := (CppCompareCodonPos(x,y,pos) > 0);
end;

// Returns the difftype
function TCodons.DiffType(x,y: TCodonMap; Pos: Integer): Integer;
begin
   case CppCompareCodonPos(x,y, Pos) of
      0: Result := 0; // None
     -1: Result := 1; // Ts
      1: Result := 2; // Tv
   else
     Result := -1000;
   end;
end;

//  Computes redundancy
function TCodons.CppRedundancyAt(Table: PAnsiChar; Codon: TCodonMap; Pos: Integer): Integer;
var
  OriginalBase, NewCodon, MutateTo, IntCodon, NewBase: Integer;
begin
  Result := 0;
  if (Pos < 0) or (Pos > 2) then
    Exit;

  IntCodon := Integer(Codon);
  OriginalBase := IntCodon shr Shifts[Pos];
  OriginalBase := OriginalBase and Mask[2];  //third base is the rightmost so used to mask

  for NewBase :=0 to 3 do  // T,C,A,G
  begin
    if NewBase = OriginalBase then
      Continue;                     // do nothing, just skip it
    MutateTo := NewBase shl Shifts[Pos];
    NewCodon := ((IntCodon and (not Mask[Pos])) or MutateTo) and MaskAll;
    if Table[NewCodon] = '*' then
      Continue;  // Invalid change;
    if Table[NewCodon] = Table[IntCodon] then
      Inc(Result);
  end;
  if Result > 0 then
    Inc(Result);  // this is the redundancy
end;

function TCodons.CppExtRedundancyAt(Table: PAnsiChar; Codon: TCodonMap; Pos: Integer): Integer;
var
  TsSynFreq, TvSynFreq, TvNonsynFreq: Integer;
  OriginalBase, NewCodon, MutateTo, IntCodon, NewBase: Integer;
  function IsTransition(BaseX, BaseY: Integer): Boolean;
  begin
     if ((BaseX = BaseA) and (BaseY = BaseG)) or
        ((BaseX = BaseG) and (BaseY = BaseA)) or
        ((BaseX = BaseC) and (BaseY = BaseT)) or
        ((BaseX = BaseT) and (BaseY = BaseC)) then
     Result := True
   else
     Result := False;
  end;
begin
  Result := 0;  // every codon is at least 0 fold redundant
  if (Pos < 0) or (Pos > 2) then
    Exit;

  Result := CppRedundancyAt(Table,Codon,Pos);
  if Result <> 2 then
    Exit; // simple redundancy so we just leave

  // otherwise 2-fold site; but is it Two-fold S type or V type
  // S type is one where all ts are syn and tv are nonsyn
  // V type is one where all tv are syn and tv are nonsyn
  IntCodon := Integer(Codon);
  OriginalBase := IntCodon shr Shifts[Pos];
  OriginalBase := OriginalBase and Mask[2];  //third base is the rightmost so used to mask

  TsSynFreq := 0;
  TvSynFreq := 0;
  TvNonsynFreq := 0;

  for NewBase :=0 to 3 do  // T,C,A,G
  begin
    if NewBase = OriginalBase then
      Continue;                     // do nothing, just skip it
    MutateTo := NewBase shl Shifts[Pos];
    NewCodon := ((IntCodon and (not Mask[Pos])) or MutateTo) and MaskAll;

    // we do not enforce
    //    if IntAminoAcid[NewCodon] = '*' then  // if stop codon then ignore
    //         Exit
    // So, we treat substitution to stop codons as nonsynonymous

    if IsTransition(OriginalBase, NewBase) then
    begin
      if IntAminoAcid[NewCodon] = IntAminoAcid[IntCodon] then
        Inc(TsSynFreq);
      //else  we never use this portion anyway so commented out
      //  Inc(TsNonSynFreq);
    end
    else // it is transvesion
    begin
      if IntAminoAcid[NewCodon] <> IntAminoAcid[IntCodon] then
        Inc(TvNonSynFreq)
      else
        Inc(TvSynFreq);
    end;
  end;

  // this is conventional case
  if (TsSynFreq = 1) and (TvNonsynFreq=2) then
    Result := 2
  else if (TsSynFreq = 1) and (TvSynFreq > 0) then
    Result := 3  // more redundant than 2-fold Ts type
  else
    Result := 1;  // all other types
end;

// Results type of difference: 0= none; -1: Ts; +1 Tv
function TCodons.CppCompareCodonPos(x,y: TCodonMap; Pos: Integer): Integer;
var
  IntX, IntY: Integer;
begin
   Result := 0;  // assume no difference
   IntX := Integer(x);
   IntY := Integer(y);
   IntX := (IntX shr Shifts[Pos]) and Mask[2];  // to clear all other bits
   IntY := (IntY shr Shifts[Pos]) and Mask[2];  // to clear all other bits

   if IntX = IntY then
    Exit;

   if ((IntX = BaseA) and (IntY = BaseG)) or
      ((IntX = BaseG) and (IntY = BaseA)) or
      ((IntX = BaseC) and (IntY = BaseT)) or
      ((IntX = BaseT) and (IntY = BaseC)) then
     Result := -1  // Ts
   else
     Result := 1;  // Tv
end;

// This is how MEGA's original analysis counted the number of synonymous sites
function TCodons.CppNoOfSynSites(Table: PAnsiChar; Codon: TCodonMap; Ratio: Double): Double;
var
  OriginalBase, NewCodon, MutateTo, IntCodon, NewBase, Pos: Integer;
  s, n, TotalS, x: Double;

  function IsTransition(BaseX, BaseY: Integer): Boolean;
  begin
     if ((BaseX = BaseA) and (BaseY = BaseG)) or
        ((BaseX = BaseG) and (BaseY = BaseA)) or
        ((BaseX = BaseC) and (BaseY = BaseT)) or
        ((BaseX = BaseT) and (BaseY = BaseC)) then
     Result := True
   else
     Result := False;
  end;
begin
   IntCodon := Integer(Codon);
   Result:= 0;
   if Table[IntCodon]= '*' then
     Exit;

   TotalS := 0;
   for Pos:=0 to 2 do
   begin
     OriginalBase := (IntCodon shr Shifts[Pos]) and Mask[2];
     s := 0;  n := 0;
     for NewBase :=0 to 3 do
     begin
       if NewBase = OriginalBase then
	 Continue;                     // do nothing, just skip it
       MutateTo := NewBase shl Shifts[Pos];
       NewCodon := (IntCodon and (not Mask[Pos])) or MutateTo;
       NewCodon := NewCodon and MaskAll;  // this a new codon

       if Table[NewCodon] = '*' then
	 continue;  // Invalid change; stop codons are excluded from calculation
       x := 1; // standard adder

       // see pg 201 of Ina's paper;  This is the only addition to MEGA 1.0 algorithm
       if IsTransition(OriginalBase, NewBase) then
         x := Ratio;

       if Table[NewCodon] = Table[IntCodon] then s := s + x  // syn
       else                                      n := n + x; // nonsyn
     end;
     if s <> 0 then // Eliminates a div by 0 error.  When S and N are both 0 we get 0/0.
       TotalS := TotalS + (s/(s+n));  // So portion of each sites' syn contribution is summed
   end;
   Result := TotalS;
end;

//------ TCodonInfo class ----------------
constructor TCodonInfo.Create;
var
  i, j: AnsiChar;
begin
  inherited Create;
  FTsTvRatio       := 1;
  FHasSynSiteTable := False;
  FHasDsDnMatrices := False;
  for i:=#0 to #63 do
  begin
    FSynSiteTable[i] := 0.0;
    for j:=#0 to i do
    begin
      FDsDnTsMat[i,j] := -1;
      FDsDnTsMat[j,i] := -1;
      FDsDnTvMat[i,j] := -1;
      FDsDnTvMat[j,i] := -1;
    end;
    FDsDnTsMat[i,i] := 0;
    FDsDnTvMat[i,i] := 0;
  end;
end;

//--- Get the exact number of synonymous sites
function TCodonInfo.GetSynSites(Codon: TCodonMap): Double;
begin
  if Codon > #63 then
    RaiseErrorMessage(HC_Unexpected_Error, ClassName);
  if not FHasSynSiteTable then
    RaiseErrorMessage(HC_Unexpected_Error, 'Caller function must issue GenerateSynSiteTable command');
  Result := FSynSiteTable[Codon];
end;

//--- Get the exact number of nonsynonymous sites
function TCodonInfo.GetNonsynSites(Codon: TCodonMap): Double;
begin
  if Codon > #63 then
    RaiseErrorMessage(HC_Unexpected_Error, ClassName);
  if not FHasSynSiteTable then
    RaiseErrorMessage(HC_Unexpected_Error, ClassName);
  Result := 3.0 - FSynSiteTable[Codon];
end;

//--- Get total of DsDn entries for a pair of codons
function TCodonInfo.TotalDsDn(x,y: TCodonMap): Integer;
begin
  if (x > #63) or (y > #63) then
    RaiseErrorMessage(HC_Unexpected_Error, ClassName);
  Result := FDsDnTsMat[x,y]+FDsDnTsMat[y,x]+FDsDnTvMat[x,y]+FDsDnTvMat[y,x];
end;

//--- Get exact number of total synonymous differences
function TCodonInfo.GetSynDiff(Map1, Map2: TCodonMap): Double;
begin
  Result := 0;
  if (Map1 > #63) or (Map2 > #63) then
    Exit;
  if Map1 > Map2 then
    Result := FDsDnTsMat[Map1,Map2] + FDsDnTvMat[Map1,Map2]
  else
    Result := FDsDnTsMat[Map2,Map1] + FDsDnTvMat[Map2,Map1];
  if Result > 0 then
    Result := NoOfDifferences(Map1, Map2)*Result/TotalDsDn(Map1, Map2);
end;

//--- Get exact number of total nonsynonymous differences
function TCodonInfo.GetNonsynDiff(Map1, Map2: TCodonMap): Double;
begin
  Result := 0;
  if (Map1 > #63) or (Map2 > #63) then
    Exit;
  if Map1 > Map2 then
    Result := FDsDnTsMat[Map2,Map1] + FDsDnTvMat[Map2,Map1]
  else
    Result := FDsDnTsMat[Map1,Map2] + FDsDnTvMat[Map1,Map2];
  if Result > 0 then
    Result := NoOfDifferences(Map1, Map2)*Result/TotalDsDn(Map1, Map2);
end;

function TCodonInfo.GetTsSynDiff(Map1, Map2: TCodonMap): Double;
begin
  Result := 0;
  if (Map1 > #63) or (Map2 > #63) then
    Exit;
  if Map1 > Map2 then
    Result := FDsDnTsMat[Map1][Map2]
  else
    Result := FDsDnTsMat[Map2][Map1];
  if Result > 0 then
    Result := NoOfDifferences(Map1, Map2)*Result/TotalDsDn(Map1, Map2);
end;

function TCodonInfo.GetTsNonsynDiff(Map1, Map2: TCodonMap): Double;
begin
  Result := 0;
  if (Map1 > #63) or (Map2 > #63) then
    Exit;
  if Map1 > Map2 then
    Result := FDsDnTsMat[Map2][Map1]
  else
    Result := FDsDnTsMat[Map1][Map2];
  if Result > 0 then
    Result := NoOfDifferences(Map1, Map2)*Result/TotalDsDn(Map1, Map2);
end;

function TCodonInfo.GetTvSynDiff(Map1, Map2: TCodonMap): Double;
begin
  Result := 0;
  if (Map1 > #63) or (Map2 > #63) then
    Exit;
  if Map1 > Map2 then
    Result := FDsDnTvMat[Map1][Map2]
  else
    Result := FDsDnTvMat[Map2][Map1];
  if Result > 0 then
    Result := NoOfDifferences(Map1, Map2)*Result/TotalDsDn(Map1, Map2);
end;

function TCodonInfo.GetTvNonsynDiff(Map1, Map2: TCodonMap): Double;
begin
  Result := 0;
  if (Map1 > #63) or (Map2 > #63) then
    Exit;
  if Map1 > Map2 then
    Result := FDsDnTvMat[Map2][Map1]
  else
    Result := FDsDnTvMat[Map1][Map2];
  if Result > 0 then
    Result := NoOfDifferences(Map1, Map2)*Result/TotalDsDn(Map1, Map2);
end;

// Generates the Table of number of synonymous sites
function TCodonInfo.GenerateSynSiteTable: Boolean;
var
  ic: AnsiChar;
begin
 if not FValid then
   RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);

 for ic:=#0 to #63 do
   FSynSiteTable[ic] := GetNoOfSynSites(TCodonMap(ic), FTsTvRatio); // a/b ratio as Ina; NG=1
 FHasSynSiteTable := True;
 Result := True;
end;

function TCodonInfo.GenerateDsDnMatrices: Boolean;
var
  row, col: AnsiChar;
begin
  if not FValid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);
  for row:= #0 to #63 do
   for col:=#0 to row do
   begin
      if col = row then
        Break;
      CppGetCodonDiffPathwayCounts(TCodonMap(row), TCodonMap(col)); // auto update
   end;
  FHasDsDnMatrices := True;
  Result := True
end;

procedure TCodonInfo.InitDsDnMatElts(X,Y: TCodonMap; sTs, nTs, sTv, nTv: Integer);
begin
  if X > Y then
  begin
    FDsDnTsMat[X][Y] := sTs;  // synTs are on lower left of Ts mat
    FDsDnTvMat[X][Y] := sTv;  // synTv are on the lower left of Tv mat
    FDsDnTsMat[Y][X] := nTs;  // nonsynTs are on upper right of Ts
    FDsDnTvMat[Y][X] := nTv;  // nonsynTv are on upper right of Tv
  end
  else if Y > X then
  begin
    FDsDnTsMat[Y][X] := sTs;  //-- as above --
    FDsDnTvMat[Y][X] := sTv;
    FDsDnTsMat[X][Y] := nTs;
    FDsDnTvMat[X][Y] := nTv;
  end;
end;

procedure TCodonInfo.CppGetCodonDiffPathwayCounts(x,y: TCodonMap);
var
  IntX, IntY, XoredProduct: Integer;
  NoOfDiff, i: Integer;
  DiffPos: array [0..2] of Integer;
begin
  for i := Low(DiffPos) to High(DiffPos) do
    DiffPos[i] := 0;

  if x > y then  begin  IntX := Integer(x);  IntY := Integer(y); end
  else           begin  IntX := Integer(y);  IntY := Integer(x); end;

  if IntX = IntY then
    Exit;

  // otherwise they have differences
  NoOfDiff := 0;
  XoredProduct := IntX xor IntY;    // only those which are same will be 00

  //====we start looking at how many differences are there and where
  for i:=2 downto 0 do
  begin
    if (XoredProduct and Mask[i]) <> 0 then
    begin
      DiffPos[NoOfDiff] := i;
      Inc(NoOfDiff);
    end;
  end;

  // Now update the DsDn matrices
  case NoOfDiff of
    1: DiffInOne(IntX, IntY,DiffPos[0]);
    2: DiffInTwo(IntX, IntY, DiffPos[0], DiffPos[1]);
    3: DiffInThree(IntX, IntY);
  end;
end;

procedure TCodonInfo.DiffInOne(IntX, IntY: Integer; Pos: Integer);
var
  MyX, MyY: Integer;
  X, Y: TCodonMap;

    function IsTransition(TheX, TheY: Integer): Boolean;
    begin
      // assumes that TheX and TheY are different
      TheX := (MyX shr Shifts[Pos]) and Mask[2];  // to clear all other bits
      TheY := (MyY shr Shifts[Pos]) and Mask[2];  // to clear all other bits
      if ((TheX = BaseA) and (TheY = BaseG)) or
         ((TheX = BaseG) and (TheY = BaseA)) or
         ((TheX = BaseC) and (TheY = BaseT)) or
         ((TheX = BaseT) and (TheY = BaseC)) then
        Result := True
      else
        Result := False;
    end;
begin
  if IntX > IntY then begin  MyX := IntX; MyY := IntY end
  else                begin  MyY := IntX; MyX := IntY end;

  X := AnsiChar(MyX);
  Y := AnsiChar(MyY);
  if FDsDnTsMat[X, Y] >= 0 then
    Exit;

  InitDsDnMatElts(X,Y,0,0,0,0);

  // there is only one change:  MyX -> MyY
  if IsTransition(MyX, MyY) then  // transitional change
  begin
    if FCodeTable[X] = FCodeTable[Y] then
      FDsDnTsMat[X][Y] := 1   // syn Ts
    else
      FDsDnTsMat[Y][X] := 1;  // nonsyn Ts
  end
  else
  begin
    if FCodeTable[X] = FCodeTable[Y] then
      FDsDnTvMat[X][Y] := 1  // syn Tv
    else
      FDsDnTvMat[Y][X] := 1; // nonsyn Tv
  end;
end;

procedure TCodonInfo.DiffInTwo(IntX, IntY: Integer; Pos1, Pos2: Integer);
var
  MyX, MyY, MutateTo, NewY, OldX, OldY: Integer;
  X, Y: TCodonMap;
  sTs, nTs, sTv, nTv, i, ThePos: Integer;
  Pos: array [0..1] of Integer;
begin
  if IntX > IntY then begin  MyX := IntX; MyY := IntY end
  else                begin  MyY := IntX; MyX := IntY end;

  X := AnsiChar(MyX);
  Y := AnsiChar(MyY);
  if FDsDnTsMat[X, Y] >= 0 then
    Exit;

  OldX := MyX; OldY := MyY;
  sTs := 0;  nTs := 0;
  sTv := 0;  nTv := 0;

  Pos[0] := Pos1;
  Pos[1] := Pos2;

  for i:= 0 to 1 do
  begin
    // Generate the ith pathway by mutating in the ith relevant position
    ThePos := Pos[i];
    MutateTo := MyY and Mask[ThePos];
    NewY     := (OldX and (not Mask[ThePos])) or MutateTo;
    NewY     := NewY and MaskAll;

    // OldX -> NewY -> OldY
    if FCodeTable[AnsiChar(NewY)] <> '*' then
    begin
      // OldX -> NewY change
      if OldX < NewY then  begin X := AnsiChar(NewY); Y := AnsiChar(OldX); end
      else                 begin X := AnsiChar(OldX); Y := AnsiChar(NewY); end;
      DiffInOne(Integer(X), Integer(Y), ThePos);
      sTs := sTs + FDsDnTsMat[X][Y];
      sTv := sTv + FDsDnTvMat[X][Y];
      nTs := nTs + FDsDnTsMat[Y][X];
      nTv := nTv + FDsDnTvMat[Y][X];

      // NewY -> OldY
      if ThePos = Pos[0] then ThePos := Pos[1] else ThePos := Pos[0];
      if NewY < OldY then  begin X := AnsiChar(OldY); Y := AnsiChar(NewY); end
      else                 begin X := AnsiChar(NewY); Y := AnsiChar(OldY); end;
      DiffInOne(Integer(X), Integer(Y), ThePos);
      sTs  := sTs + FDsDnTsMat[X][Y];
      sTv  := sTv + FDsDnTvMat[X][Y];
      nTs  := nTs + FDsDnTsMat[Y][X];
      nTv  := nTv + FDsDnTvMat[Y][X];
    end;
  end;
  InitDsDnMatElts(AnsiChar(MyX), AnsiChar(MyY), sTs, nTs, sTv, nTv);
end;

//===== if differences are three
procedure TCodonInfo.DiffInThree(IntX, IntY: Integer);
var
  MyX, MyY, MutateTo, NewY, OldX, OldY: Integer;
  X, Y: TCodonMap;
  sTs, nTs, sTv, nTv, i, NoOfBottomPaths: Integer;
begin
  if IntX > IntY then begin  MyX := IntX; MyY := IntY end
  else                begin  MyY := IntX; MyX := IntY end;

  X := AnsiChar(MyX);
  Y := AnsiChar(MyY);
  if FDsDnTsMat[X, Y] >= 0 then
    Exit;

  OldX := MyX; OldY := MyY;
  sTs := 0;  nTs := 0;
  sTv := 0;  nTv := 0;

  for i:= 0 to 2 do
  begin
    // Generate the ith pathway by mutating in the ith relevant position
    MutateTo := MyY and Mask[i];
    NewY     := (OldX and (not Mask[i])) or MutateTo;
    NewY     := NewY and MaskAll;

    // OldX -> NewY -> OldY
    if FCodeTable[AnsiChar(NewY)] <> '*' then
    begin
      // NewY -> OldY  (this is the bottom part; Has Two substitutions)
      if NewY < OldY then  begin X := AnsiChar(OldY); Y := AnsiChar(NewY); end
      else                 begin X := AnsiChar(NewY); Y := AnsiChar(OldY); end;

      case i of
        0: DiffInTwo(Integer(X), Integer(Y), 1, 2);
        1: DiffInTwo(Integer(X), Integer(Y), 0, 2);
        2: DiffInTwo(Integer(X), Integer(Y), 0, 1);
      end;
      sTs  := sTs + FDsDnTsMat[X][Y];
      sTv  := sTv + FDsDnTvMat[X][Y];
      nTs  := nTs + FDsDnTsMat[Y][X];
      nTv  := nTv + FDsDnTvMat[Y][X];

      // OldX -> NewY (one substitution change in the first position)
      // if the above two paths were both valid then we should multiply their contribution
      NoOfBottomPaths := TotalDsDn(X,Y) div 2;

      if NoOfBottomPaths < 2 then
      begin
        NoOfBottomPaths := Trunc((NoOfBottomPaths / 2.0)*2.0);
      end;

      if OldX < NewY then  begin X := AnsiChar(NewY); Y := AnsiChar(OldX); end
      else                 begin X := AnsiChar(OldX); Y := AnsiChar(NewY); end;
      DiffInOne(Integer(X), Integer(Y), i);
      sTs := sTs + NoOfBottomPaths*FDsDnTsMat[X][Y];
      sTv := sTv + NoOfBottomPaths*FDsDnTvMat[X][Y];
      nTs := nTs + NoOfBottomPaths*FDsDnTsMat[Y][X];
      nTv := nTv + NoOfBottomPaths*FDsDnTvMat[Y][X];
    end;
  end;
  InitDsDnMatElts(AnsiChar(MyX), AnsiChar(MyY), sTs, nTs, sTv, nTv);
end;

function TCodonInfo.WriteIntrinsicTables(List: TStringList; SynSiteFlag, DsDnFlag: Boolean): Boolean;
var
  i, j: TCodonMap;
  Codon: AnsiString;
  TheStr, NumStr: AnsiString;
  x, Numb: Integer;
begin
  Result := False;
  if not SynSiteFlag and not DsDnFlag then
    Exit;  // nothing to write

  if not inherited Valid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);

  if SynSiteFlag then
  begin
    TheStr := 'S = No. of synonymous sites';
    List.Add(TheStr);
    TheStr := 'N = No. of nonsynonymous sites';
    List.Add(TheStr);
    TheStr := '---------------------------------';
    List.Add(TheStr);
    TheStr := '-----    No of Sites   Redundancy';
    List.Add(TheStr);
    TheStr := '-----     for codon   Pos Pos Pos';
    List.Add(TheStr);
    TheStr := 'Codon      S     N    1st 2nd 3rd';
    List.Add(TheStr);
    TheStr := '-----    ----- -----  --- --- ---';
    List.Add(TheStr);
    for i:=#0 to #63 do
    begin
      Codon := MakeCodonString(i);
      TheStr := Codon+' ('+AnsiString(FCodeTable[i])+')  '+
                FloatToStrF(SynSites[i], ffFixed, 4, 3)+' '+
                FloatToStrF(NonsynSites[i], ffFixed, 4, 3)+'   ';
      // I had to directly call CppRedundancyAt because I wanted to
      // write true redundancy (0,1,2,3, o4 4-fold)
      //
      for x:= 0 to 2 do
        TheStr := TheStr + IntToStr(CppRedundancyAt(FCodeTable,i,x)) +'   ';
      List.Add(TheStr);
    end;
    List.Add('---------------------------------');
  end;

  if DsDnFlag then
  begin
    List.Add('Number of syn (lower) and nonsyn (upper) changes between codons');
    List.Add('Note: All pathways going through a stop codon are discarded');

    // first 32
    TheStr := '         ';
    for i:=#0 to #31 do
      TheStr:= TheStr + MakeCodonString(i) + ' ';
    List.Add(TheStr);

    for i:=#0 to #63 do
    begin
      TheStr := MakeCodonString(i)+' ('+AnsiString(FCodeTable[i])+')  ';
      for j:=#0 to #31 do
      begin
        if i=j then
          TheStr := TheStr + '    '
        else if FDsDnTsMat[i,j]+FDsDnTvMat[i,j] < 0 then
          TheStr := TheStr + ' -  '
        else
        begin
          Numb := Integer(FDsDnTsMat[i,j]+FDsDnTvMat[i,j]);
          NumStr := IntToStr(Numb);
          if Length(NumStr) = 1 then  TheStr := TheStr + ' '+ NumStr+ '  '
          else                        TheStr := TheStr + NumStr + '  ';
        end;
      end;
      List.Add(TheStr);
    end;
    List.Add(' ');
    // Last 32
    TheStr := '         ';
    for i:=#32 to #63 do
      TheStr:= TheStr+MakeCodonString(i) + ' ';
    List.Add(TheStr);
    for i:=#0 to #63 do
    begin
      TheStr := MakeCodonString(i)+' ('+AnsiString(FCodeTable[i])+')  ';
      for j:=#32 to #63 do
      begin
        if i=j then
          TheStr := TheStr + '    '
        else if FDsDnTsMat[i,j]+FDsDnTvMat[i,j] < 0 then
          TheStr := TheStr + ' -  '
        else
        begin
          Numb := Integer(FDsDnTsMat[i,j]+FDsDnTvMat[i,j]);
          NumStr := IntToStr(Numb);
          if Length(NumStr) = 1 then  TheStr := TheStr + ' '+ NumStr+'  '
          else                        TheStr := TheStr + NumStr + '  ';
        end;
      end;
      List.Add(TheStr);
    end;
  end;
end;
                 
{ToDo -oKumar : test/check the XL style table writing done by Dan}
function TCodonInfo.WriteIntrinsicTablesExcel(Xls: TExcelWrite; SynSiteFlag, DsDnFlag: Boolean): Boolean;
var
  i, j: TCodonMap;
  Codon: AnsiString;
  x, Numb: Integer;
begin
  Result := False;
  if not SynSiteFlag and not DsDnFlag then
    Exit;  // nothing to write

  if not inherited Valid then
    RaiseErrorMessage(HC_Unexpected_Error, EmptyStr);

  if SynSiteFlag then
  begin

    Xls.Add('S = No. of synonymous sites');
    Xls.WriteLine(0, 'A', '', True);
    Xls.Add('N = No. of nonsynonymous sites');
    Xls.WriteLine(0, 'A', '', True);
    Xls.NewLine;
    Xls.AddBlankCell; // skip a cell
    Xls.Add('No. Sites for codon');
    Xls.AddBlankCell; // skip a cell
    Xls.Add('Redundancy');
    Xls.WriteLine();

    Xls.Add('Codon');
    Xls.Add('S');
    Xls.Add('N');
    Xls.Add('Pos 1st');
    Xls.Add('Pos 2nd');
    Xls.Add('Pos 3rd');
    Xls.WriteLine();

    for i:=#0 to #63 do
    begin
      Codon := MakeCodonString(i);
      Xls.Add(Codon+' ('+AnsiString(FCodeTable[i])+')');
      Xls.Add(SynSites[i]);
      Xls.Add(NonsynSites[i]);

      // I had to directly call CppRedundancyAt because I wanted to
      // write true redundancy (0,1,2,3, o4 4-fold)
      //
      for x:= 0 to 2 do
        Xls.Add(CppRedundancyAt(FCodeTable,i,x));
        Xls.WriteLine();
    end;
  end;

  if DsDnFlag then
  begin
    Xls.WriteLine('Number of syn (lower) and nonsyn (upper) changes between codons');

    Xls.WriteLine('Note: All pathways going through a stop codon are discarded');
    // first 32
    for i:=#0 to #31 do
      Xls.Add(MakeCodonString(i));
    Xls.WriteLine();

    for i:=#0 to #63 do
    begin
      Xls.Add(MakeCodonString(i)+' ('+AnsiString(FCodeTable[i])+')  ');
      for j:=#0 to #31 do
      begin
        if i=j then
          Xls.Add(EmptyStr)
        else if FDsDnTsMat[i,j]+FDsDnTvMat[i,j] < 0 then
          Xls.Add('-')
        else
        begin
          Numb := Integer(FDsDnTsMat[i,j]+FDsDnTvMat[i,j]);
          Xls.Add(Numb);
        end;
      end;
      Xls.WriteLine();
    end;
    Xls.WriteLine(EmptyStr);
    // Last 32
    for i:=#32 to #63 do
      Xls.Add(MakeCodonString(i));
    Xls.WriteLine();
    for i:=#0 to #63 do
    begin
      Xls.Add(MakeCodonString(i)+' ('+AnsiString(FCodeTable[i])+')  ');
      for j:=#32 to #63 do
      begin
        if i=j then
          Xls.Add(EmptyStr)
        else if FDsDnTsMat[i,j]+FDsDnTvMat[i,j] < 0 then
          Xls.Add('-')
        else
        begin
          Numb := Integer(FDsDnTsMat[i,j]+FDsDnTvMat[i,j]);
          Xls.Add(Numb);
        end;
      end;
      Xls.WriteLine();
    end;
  end;
end;
end.

