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

unit MOtuInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MegaConsts, Classes, MLongintList, ProcessInputData, mgeographical_info, mtipdatesfinder;

type

  { TOtuInfo }

  TOtuInfo = class
  private
    FId:       LongInt; // Order in the original input data file
    FIsGpUsed: Boolean;
    FName:     AnsiString;  // current Name of the otu
    FDiffFromFocalOtu: Extended;
    FRsvName:  AnsiString;  // original name of the otu
    FIsUsed:   Boolean; // If it is currently marked by user
    FIsHidden: Boolean; // If it is hidden from view but not unselected
    FData:     Pointer; // Data as READ from INPUT is kept here
    FRsvData:  Pointer; // Reserve data
    FInfo:     Pointer; // Other Data or Info pointer can be kept here
    FFreq:     LongInt; // frequency of this haplotype set by user
    FOutgroupMember: Boolean; // true if the otu is a member of the outgroup
    FGeographicalInfo: TGeographicalInfo;
    function GetCity: AnsiString;
    function GetContinent: AnsiString;
    function GetCountry: AnsiString;
    function GetDay: Byte;
    function GetGpName: AnsiString;
    function GetHasAnyGeographicalInfo: Boolean;
    function GetHasDateInfo: Boolean;
    function GetMonth: TCalendarMonth;
    function GetPopName: AnsiString;
    function GetReferenceTime: TDateTime;
    function GetSpName: AnsiString;
    function GetYear: SmallInt;
    procedure SetCity(AValue: AnsiString);
    procedure SetContinent(AValue: AnsiString);
    procedure SetCountry(AValue: AnsiString);
    procedure SetDay(AValue: Byte);
    procedure SetGpName(AValue: AnsiString);
    procedure SetMonth(AValue: TCalendarMonth);
    procedure SetPopName(AValue: AnsiString);
    procedure SetReferenceTime(AValue: TDateTime);
    procedure SetSpName(AValue: AnsiString);
    procedure SetYear(AValue: SmallInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Other: TOtuInfo);
    procedure AssignGeographicalInfo(aInfo: TGeographicalInfo);
    procedure AssignTipDate(tipDate: TTipDate);
    procedure SetGroupSource(groupSource: TGroupTagIndex);
    property HasAnyGeographicalInfo: Boolean read GetHasAnyGeographicalInfo;
    property GeographicalInfo: TGeographicalInfo read FGeographicalInfo;
    property Name:     AnsiString   read FName       write FName;
    property RsvName:  AnsiString   read FRsvName    write FRsvName;
    property GpName:   AnsiString   read GetGpName     write SetGpName;
    property SpName:   AnsiString   read GetSpName     write SetSpName;
    property PopName:   AnsiString   read GetPopName     write SetPopName;
    property Continent: AnsiString read GetContinent write SetContinent;
    property Country: AnsiString read GetCountry write SetCountry;
    property City: AnsiString read GetCity write SetCity;
    property Year: SmallInt read GetYear write SetYear;
    property Month: TCalendarMonth read GetMonth write SetMonth;
    property Day: Byte read GetDay write SetDay;
    property HasDateInfo: Boolean read GetHasDateInfo;
    property ReferenceTime: TDateTime read GetReferenceTime write SetReferenceTime;
    property IsUsed:   Boolean  read FIsUsed     write FIsUsed;
    property IsGpUsed: Boolean read FIsGpUsed write FIsGpUsed;
    property IsHidden: Boolean  read FIsHidden   write FIsHidden;
    property Id:       LongInt  read FId         write FId;
    property Data:     Pointer  read FData       write FData;
    property RsvData:  Pointer  read FRsvData    write FRsvData;
    property Info:     Pointer  read FInfo       write FInfo;
    property Freq:     LongInt read FFreq        write FFreq;
    property OutgroupMember: Boolean read FOutgroupMember write FOutgroupMember;
    property DiffFromFocalOtu: Extended read FDiffFromFocalOtu write FDiffFromFocalOtu;
  end;

  TArrayOfTOtuInfo = array[Word] of TOtuInfo;
  PArrayOfTOtuInfo = ^TArrayOfTOtuInfo;
  POtuInfo = ^TOtuInfo;

  { TAllOtuInfo }

  TAllOtuInfo = class    {------ Keeps info about all Otus ----}
  private
    FArray     : TList;
    FGroupNamesAreBackedUp: Boolean;
    FNoOfOtus  : LongInt;
    //FOwnerCount: LongInt;
    FIsDirty   : Boolean;  // tells if things have been modified

    function GetNoOfSelGroups: LongInt;
    procedure SetNoOfOtus(Value: LongInt);

    function  GetOtuInfo(Index: LongInt): TOtuInfo;
    function  GetNoOfSelOtus: LongInt;
    procedure SetOtuInfo(Index: LongInt; Item: TOtuInfo);
  public
    constructor Create;
    destructor  Destroy; override; // necessary
    //procedure   IncOwnerCount;
    //procedure   DecOwnerCount;
    function AllOtusHaveDateInfo: Boolean;
    procedure   Move(FromIndex: LongInt; ToIndex: LongInt);
    function FindById(aId: Integer): Integer;

    procedure UsedOtuInfosPhyloQ (MyUsedOtuInfos: TList; MustBeInGp: Boolean; var Sequence, TaxonName: AnsiString);
    procedure UsedOtuInfos(MyUsedOtuInfos: TList; MustBeInGp: Boolean);

    procedure   SortByInputOrder;
    procedure   SortByGroupNameAsc;
    procedure   SortByGroupNameDes;
    procedure   SortByTaxaNameAsc;
    procedure   SortByTaxaNameDes;
    procedure   SortBySpeciesNameAsc;
    procedure   SortBySpeciesNameDes;
    procedure   SortByPopulationNameAsc;
    procedure   SortByPopulationNameDes;
    procedure   SortByGroupAndTaxaName;
    procedure   SortByTaxaGpsDlg;
    procedure   SortBySimilarityToFirstOtu;

    procedure HideUnselected;
    procedure UnhideAll;

    procedure   WriteToFile(var data : TFileStream; OtuDataTypeIn : TOtuDataType);
    procedure   RestoreFromFile(data : TStream; OtuDataType: TOtuDataType; version: Integer);
    function NumActiveOutgroupMembers: Integer;
    function OutgroupIsDefined: Boolean;
    function OutgroupIsDefinedAndIsActive: Boolean;
    function ConstructSelTaxaList(AList: TLongintList): LongInt; // returns #taxa
    function GetOtuInfoByName(AName: AnsiString): TOtuInfo;
    function GetFirstUsedSequenceName: String;

    function HasGroupNames: Boolean;
    function HasSpeciesNames: Boolean;
    function HasPopulation: Boolean;
    function HasContinent: Boolean;
    function HasCountry: Boolean;
    function HasCity: Boolean;
    function HasTime: Boolean;
    function HasMonth: Boolean;
    function HasDay: Boolean;
    function HasYear: Boolean;
    procedure GetHasGeoInfoAllAtOnce(var aGp: Boolean;
                                     var aSp: Boolean;
                                     var aPop: Boolean;
                                     var aCont: Boolean;
                                     var aCountry: Boolean;
                                     var aCity: Boolean;
                                     var aTime: Boolean;
                                     var aMonth: Boolean;
                                     var aDay: Boolean;
                                     var aYear: Boolean);



    procedure CountDifferencesFromFirstVisibleSequence;
    procedure CountPairwiseDistFromFirstVisibleOtu;
    procedure AssignTipDates(tipDates: TTipDateList);
    procedure Insert(aIndex: Integer; aItem: TOtuInfo);
    procedure BackupGroupNames;
    procedure SetGroupSource(groupSource: TGroupTagIndex);
    property   IsDirty: Boolean Read FIsDirty write FIsDirty;
    //property   OwnerCount: LongInt read FOwnerCount;
    property   NoOfOtus: LongInt read FNoOfOtus write SetNoOfOtus;
    property   NoOfSelOtus: LongInt read GetNoOfSelOtus;
    property NoOfSelGroups: LongInt read GetNoOfSelGroups;
    property   Otu[Index: LongInt]: TOtuInfo read GetOtuInfo write SetOtuInfo;
    property   X[Index: LongInt]:   TOtuInfo read GetOtuInfo write SetOtuInfo; default;
    property GroupNamesAreBackedUp: Boolean read FGroupNamesAreBackedUp;
  end;

function  CountNoOfGpsInOtuInfos(OtuInfos: TList): Integer;
procedure GetGpIdForOtuInfos(OtuInfos: TList; var MyGpIds: array of Integer);
function CurrentAllOtuInfos: TAllOtuInfo;
function CompareOtuByTimestamp(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  {$IFDEF VISUAL_BUILD}Forms,{$ENDIF}
  Sysutils, ErrorMessages_HC, MegaErrUtils, MegaUtils, MVS_SeqDataExplorer,
  MVS_DistDataExplorer, MD_InputSeqData, MD_InputDistData, dateutils, math;

procedure TOtuInfo.Assign(Other: TOtuInfo);
begin
  FId := Other.FId;
  FName := Other.FName;
  FRsvName := Other.FRsvName;
  FIsUsed := Other.FIsUsed;
  FIsHidden := Other.FIsHidden;
  FData := Pointer(AnsiString(Other.FData));
  FRsvData :=  Pointer(AnsiString(Other.FRsvData));
  FInfo := Pointer(AnsiString(Other.FInfo));
  FFreq := Other.FFreq;
  FOutgroupMember := Other.FOutgroupMember;
  FGeographicalInfo.Assign(Other.FGeographicalInfo);
end;

procedure TOtuInfo.AssignGeographicalInfo(aInfo: TGeographicalInfo);
begin
  FGeographicalInfo.Assign(aInfo);
end;

procedure TOtuInfo.AssignTipDate(tipDate: TTipDate);
begin
  if not (tipDate.TaxonName = Name) then
    raise Exception.Create(Format('invalid tip date name: expected %d but got %d', [Name, tipDate.TaxonName]));
  if Assigned(FGeographicalInfo) then
  begin
    FGeographicalInfo.Year := tipDate.Year;
    FGeographicalInfo.Month := IntToMonth(tipDate.Month);
    FGeographicalInfo.Day := tipDate.Day;
  end;
end;

procedure TOtuInfo.SetGroupSource(groupSource: TGroupTagIndex);
begin
  case groupSource of
    gtiRsv: GpName := RsvName;
    gtiSpecies: GpName := SpName;
    gtiPopulation: GpName := PopName;
    gtiContinent: GpName := Continent;
    gtiCountry: GpName := Country;
    gtiCity: GpName := City;
    gtiYear: GpName := IntToStr(Year);
    gtiMonth: GpName := MonthToStr(Month);
    gtiDay: GpName := IntToStr(Day);
    gtiTime: GpName := FormatDateTime('hh:mm:ss', ReferenceTime);
    gtiNone: GpName := EmptyStr;
  end;
end;

function TOtuInfo.GetCity: AnsiString;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.City
  else
    Result := EmptyStr;
end;

function TOtuInfo.GetContinent: AnsiString;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Continent
  else
    Result := EmptyStr;
end;

function TOtuInfo.GetCountry: AnsiString;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Country
  else
    Result := EmptyStr;
end;

function TOtuInfo.GetDay: Byte;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Day
  else
    Result := 0;
end;

function TOtuInfo.GetGpName: AnsiString;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Group
  else
    Result := EmptyStr;
end;

function TOtuInfo.GetHasAnyGeographicalInfo: Boolean;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.HasAnyInfo
  else
    Result := False;
end;

function TOtuInfo.GetHasDateInfo: Boolean;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.HasDateInfo
  else
    Result := False;
end;

function TOtuInfo.GetMonth: TCalendarMonth;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Month
  else
    Result := NoMonth;
end;

function TOtuInfo.GetPopName: AnsiString;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Population
  else
    Result := EmptyStr;
end;

function TOtuInfo.GetReferenceTime: TDateTime;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.ReferenceTime
  else
    Result := MinDateTime;
end;

function TOtuInfo.GetSpName: AnsiString;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Species
  else
    Result := EmptyStr;
end;

function TOtuInfo.GetYear: SmallInt;
begin
  if Assigned(FGeographicalInfo) then
    Result := FGeographicalInfo.Year
  else
    Result := -1;
end;

procedure TOtuInfo.SetCity(AValue: AnsiString);
begin
  FGeographicalInfo.City := AValue;
end;

procedure TOtuInfo.SetContinent(AValue: AnsiString);
begin
  FGeographicalInfo.Continent := AValue;
end;

procedure TOtuInfo.SetCountry(AValue: AnsiString);
begin
  FGeographicalInfo.Country := AValue
end;

procedure TOtuInfo.SetDay(AValue: Byte);
begin
  FGeographicalInfo.Day := AValue
end;

procedure TOtuInfo.SetGpName(AValue: AnsiString);
begin
  FGeographicalInfo.Group := AValue;
end;

procedure TOtuInfo.SetMonth(AValue: TCalendarMonth);
begin
  FGeographicalInfo.Month := AValue
end;

procedure TOtuInfo.SetPopName(AValue: AnsiString);
begin
  FGeographicalInfo.Population := AValue;
end;

procedure TOtuInfo.SetReferenceTime(AValue: TDateTime);
begin
  FGeographicalInfo.ReferenceTime := AValue;
end;

procedure TOtuInfo.SetSpName(AValue: AnsiString);
begin
  FGeographicalInfo.Species := AValue;
end;

procedure TOtuInfo.SetYear(AValue: SmallInt);
begin
  FGeographicalInfo.Year := AValue
end;

constructor TOtuInfo.Create;
begin
  inherited Create;
  FName := EmptyStr;
  FRsvName := EmptyStr;
  FGeographicalInfo := TGeographicalInfo.Create;
  FId := -1;
  FIsUsed   := False;
  FData := nil;
  FRsvData := nil;
  FInfo := nil;
  FFreq := 1;  // by default it is always 1
  FOutgroupMember := False;
end;


destructor TOtuInfo.Destroy;
begin
  if Assigned(FGeographicalInfo) then
    FGeographicalInfo.Free;
  inherited;
end;

constructor TAllOtuInfo.Create;
begin
  inherited Create;
  FNoOfOtus := 0;
  FArray := nil;
  FGroupNamesAreBackedUp := False;
  FIsDirty := True;
end;

destructor TAllOtuInfo.Destroy;
var
  i: LongInt;
begin
  if FNoOfOtus > 0 then
  begin
    for i:=0 to FNoOfOtus-1 do
    begin
      if FArray[i] <> nil then TOtuInfo(FArray[i]).Free;
      FArray[i] := nil;
    end;

    FArray.Free;
    FArray := nil;
  end;
  inherited Destroy;
end;

function TAllOtuInfo.AllOtusHaveDateInfo: Boolean;
var
  i: Integer;
begin
  Result := True;
  if FNoOfOtus > 0 then
    for i := 0 to FNoOfOtus - 1 do
      if not GetOtuInfo(i).HasDateInfo then
      begin
        Result := False;
        Exit;
      end;
end;

procedure TAllOtuInfo.SetNoOfOtus(Value: LongInt);
var
  i: LongInt;
begin
  if FNoOfOtus > 0 then
    RaiseErrorMessage(HC_Unexpected_Error, 'Memory error in TAllOtuInfo.SetNoOfOtus');
  FArray := TList.Create;
  FNoOfOtus := Value;
  for i:=0 to FNoOfOtus-1 do
    FArray.Add(nil);
end;

function TAllOtuInfo.GetNoOfSelGroups: LongInt;
var
  i: Integer;
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    aList.Duplicates := dupIgnore;
    aList.Sorted := True;
    if FNoOfOtus > 0 then
      for i := 0 to FNoOfOtus - 1 do
        if GetOtuInfo(i).IsUsed and (Trim(GetOtuInfo(i).GpName) <> EmptyStr) then
          aList.Add(GetOtuInfo(i).GpName);
    Result := aList.Count;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TAllOtuInfo.GetOtuInfo(Index: LongInt): TOtuInfo;
begin
  if index >= FArray.count then
    Raise Exception.Create('Out of Bounds.');
  Result := FArray[Index];
end;

/// <summary>Sets up UsedOtuInfos</summary>
/// <note author="Glen">This is used only for the PhyloQ method; I needed a way to ignore the
/// first sequence in the alignment file and this seemed ok.</note>
procedure TAllOtuInfo.UsedOtuInfosPhyloQ (MyUsedOtuInfos: TList; MustBeInGp: Boolean; var Sequence, TaxonName: AnsiString);
var
  i: Integer;
begin
  TaxonName := TOtuInfo(X[0]).Name;
  Sequence := AnsiString(TOtuInfo(X[0]).Data);

  MyUsedOtuInfos.Clear;
  for i:=0 to NoOfOtus-1 do
    if X[i].IsUsed then
    begin
      if MustBeInGp and (Length(X[i].GpName) = 0) then
        Continue;
      MyUsedOtuInfos.Add(Pointer(X[i]));
  end;
end;


procedure TAllOtuInfo.UsedOtuInfos(MyUsedOtuInfos: TList; MustBeInGp: Boolean); // to enforce gp membership
var
  i: Integer;
begin
  MyUsedOtuInfos.Clear;
  for i:=0 to NoOfOtus-1 do
    if X[i].IsUsed then
    begin
      if not MustBeInGp then
        MyUsedOtuInfos.Add(Pointer(X[i]))
      else if Length(X[i].GpName) <> 0 then
        MyUsedOtuInfos.Add(Pointer(X[i]));
    end;
end;

function TAllOtuInfo.GetOtuInfoByName(AName: AnsiString): TOtuInfo;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to FNoOfOtus-1 do
    if SameText(TOtuInfo(FArray[i]).Name, AName) then
    begin
      Result := FArray[i];
      Exit;
    end;
end;

function TAllOtuInfo.GetFirstUsedSequenceName: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).Name <> EmptyStr then
      begin
        Result := GetOtuInfo(i).Name;
        Exit;
      end;
end;

function TAllOtuInfo.HasGroupNames: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).GpName <> EmptyStr then
        Exit(True);
end;

function TAllOtuInfo.HasSpeciesNames: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).SpName <> EmptyStr then
        Exit(True);
end;

function TAllOtuInfo.HasPopulation: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).PopName <> EmptyStr then
        Exit(True);
end;

function TAllOtuInfo.HasContinent: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).Continent <> EmptyStr then
        Exit(True);
end;

function TAllOtuInfo.HasCountry: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).Country <> EmptyStr then
        Exit(True);
end;

function TAllOtuInfo.HasCity: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).City <> EmptyStr then
        Exit(True);
end;

function TAllOtuInfo.HasTime: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).HasDateInfo then
        Exit(True);
end;

function TAllOtuInfo.HasMonth: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).Month <> NoMonth then
        Exit(True);
end;

function TAllOtuInfo.HasDay: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).Day > 0 then
        Exit(True);
end;

function TAllOtuInfo.HasYear: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      if GetOtuInfo(i).Year >= 0 then
        Exit(True);
end;

procedure TAllOtuInfo.GetHasGeoInfoAllAtOnce(var aGp: Boolean;
  var aSp: Boolean; var aPop: Boolean; var aCont: Boolean;
  var aCountry: Boolean; var aCity: Boolean; var aTime: Boolean;
  var aMonth: Boolean; var aDay: Boolean; var aYear: Boolean);
var
  i: Integer = -1;
  numTrue: Integer = 0;
begin
  aGp := False;
  aSp := False;
  aPop := False;
  aCont := False;
  aCountry := False;
  aCity := False;
  aTime := False;
  aMonth := False;
  aDay := False;
  aYear := False;

  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
    begin
      if numTrue = 10 then { exit quickly if all are set to true}
        Exit;

 { 1} if (not aGp) and (GetOtuInfo(i).GpName <> EmptyStr) then
      begin
        aGp := True;
        inc(numTrue);
      end;

 { 2} if (not aSp) and (GetOtuInfo(i).SpName <> EmptyStr) then
      begin
        aSp := True;
        inc(numTrue);
      end;

 { 3} if (not aPop) and (GetOtuInfo(i).PopName <> EmptyStr) then
      begin
        aPop := True;
        inc(numTrue);
      end;

 { 4} if (not aCont) and (GetOtuInfo(i).Continent <> EmptyStr) then
      begin
        aCont := True;
        inc(numTrue);
      end;

 { 5} if (not aCountry) and (GetOtuInfo(i).Country <> EmptyStr) then
      begin
        aCountry := True;
        inc(numTrue);
      end;

 { 6} if (not aCity) and (GetOtuInfo(i).City <> EmptyStr) then
      begin
        aCity := True;
        inc(numTrue);
      end;

 { 7} if (not aTime) and GetOtuInfo(i).HasDateInfo then
      begin
        aTime := True;
        inc(numTrue);
      end;

 { 8} if (not aMonth) and (GetOtuInfo(i).Month <> NoMonth) then
      begin
        aMonth := True;
        inc(numTrue);
      end;

 { 9} if (not aDay) and (GetOtuInfo(i).Day > 0) then
      begin
        aDay := True;
        inc(numTrue);
      end;

{ 10} if (not aYear) and (GetOtuInfo(i).Year >= 0) then
      begin
        aYear := True;
        inc(numTrue);
      end;
    end;
end;

procedure TAllOtuInfo.CountDifferencesFromFirstVisibleSequence;
var
  seq: Integer = 0;
  site: Integer = -1;
  aInfo: TOtuInfo = nil;
  firstIndex: Integer = 0;
  firstInfo: TOtuInfo = nil;
  data1: PAnsiChar = nil;
  data2: PAnsiChar = nil;
  numDiffs: Integer = 0;
begin
  if FNoOfOtus > 1 then
  begin
    while (seq < FNoOfOtus) do
    begin
      if not TOtuInfo(FArray[seq]).IsHidden then
      begin
        firstIndex := seq;
        firstInfo := TOtuInfo(FArray[seq]);
        firstInfo.DiffFromFocalOtu := 0;
        break;
      end
      else
        inc(seq);
    end;

    if VS_SeqDataExplorer.IsTranslated then
      data1 := PAnsiChar(firstInfo.RsvData)
    else
      data1 := PAnsiChar(firstInfo.Data);
    for seq := 0 to FNoOfOtus - 1 do
    begin
      if seq = firstIndex then
        continue;
      numDiffs := 0;
      aInfo := TOtuInfo(FArray[seq]);
      if VS_SeqDataExplorer.IsTranslated then
        data2 := PAnsiChar(aInfo.RsvData)
      else
        data2 := PAnsiChar(aInfo.Data);
      for site := 0 to D_InputSeqData.NoOfSites - 1 do
      begin
        if data1[site] <> data2[site] then
          inc(numDiffs);
      end;
      aInfo.DiffFromFocalOtu := numDiffs;
    end;
  end;
end;

procedure TAllOtuInfo.CountPairwiseDistFromFirstVisibleOtu;
var
  i: Integer = -1;
  aInfo: TOtuInfo = nil;
  firstIndex: Integer = 0;
  firstInfo: TOtuInfo = nil;
  aDist: Double = 0.0;
begin
  if FNoOfOtus > 1 then
  begin
    i := 0;
    while i < FNoOfOtus do
    begin
      if not TOtuInfo(FArray[i]).IsHidden then
      begin
        firstIndex := i;
        firstInfo := TOtuInfo(FArray[i]);
        firstInfo.DiffFromFocalOtu := 0;
        break;
      end
      else
        inc(i);
    end;

    for i := 0 to FNoOfOtus - 1 do
    begin
      if i = firstIndex then
        continue;
      aInfo := TOtuInfo(FArray[i]);
      aDist := D_InputDistData.Distance[i, firstIndex];
      aInfo.DiffFromFocalOtu := aDist;
    end;
  end;
end;

procedure TAllOtuInfo.AssignTipDates(tipDates: TTipDateList);
var
  i: Integer;
begin
  if FNoOfOtus > 0 then
  begin
    if not (FNoOfOtus = tipDates.Count) then
      raise Exception.Create(Format('expected %d tip dates but got %d', [FNoOfOtus, tipDates.Count]));
    for i := 0 to FNoOfOtus - 1 do
      if Assigned(tipDates[i]) then
        GetOtuInfo(i).AssignTipDate(tipDates[i]);
  end;
end;

procedure TAllOtuInfo.Insert(aIndex: Integer; aItem: TOtuInfo);
begin
  FArray.Insert(aIndex, aItem);
end;

procedure TAllOtuInfo.BackupGroupNames;
var
  i: Integer;
begin
  if FArray.Count > 0 then
  begin
    for i := 0 to FArray.Count - 1 do
      GetOtuInfo(i).RsvName := GetOtuInfo(i).GpName;
    FGroupNamesAreBackedUp := True;
  end;
end;

procedure TAllOtuInfo.SetGroupSource(groupSource: TGroupTagIndex);
var
  i: Integer;
begin
  if not GroupNamesAreBackedUp then
    BackupGroupNames;
  if NoOfOtus > 0 then
    for i := 0 to NoOfOtus - 1 do
      Otu[i].SetGroupSource(groupSource);
end;

procedure TAllOtuInfo.SetOtuInfo(Index: LongInt; Item: TOtuInfo);
begin
  FArray[Index] := Item;
end;

function  TAllOtuInfo.ConstructSelTaxaList(AList: TLongintList): LongInt;
var
  i: Longint;
begin
  Result := 0;
  if AList = nil then
    Exit;
  AList.Clear;
  for i:=0 to FNoOfOtus-1 do
     if (Otu[i].IsUsed) and (not OTU[i].IsHidden) then
       AList.Add(i);
  Result := AList.Count;
end;

function TAllOtuInfo.GetNoOfSelOtus: LongInt;
var
  i: Longint;
begin
  Result := 0;
  for i:=0 to FNoOfOtus-1 do
     if Otu[i].IsUsed then
       Inc(Result);
end;

procedure TAllOtuInfo.Move(FromIndex: LongInt; ToIndex: LongInt);
begin
  FArray.Move(FromIndex, ToIndex);
end;

function TAllOtuInfo.FindById(aId: Integer): Integer;
var
  i: Integer = -1;
begin
  Result := -1;
  if FNoOfOtus > 0 then
    for i := 0 to FNoOfOtus - 1 do
      if GetOtuInfo(i).Id = aId then
        Exit(i);
end;

function TAllOtuInfo.OutgroupIsDefined: Boolean;
var
  i: Integer;
begin
  Result := False;
  if NoOfOtus > 0 then
  begin
    for i := 0 to NoOfOtus - 1 do
    begin
      if Otu[i].OutgroupMember then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TAllOtuInfo.OutgroupIsDefinedAndIsActive: Boolean;
var
  i: Integer;
begin
  Result := False;
  if NoOfOtus > 0 then
  begin
    for i := 0 to NoOfOtus - 1 do
    begin
      if (Otu[i].OutgroupMember and Otu[i].IsUsed) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// SOrting/unsorting etc.
function CompareTaxaIds(Item1, Item2: Pointer): Integer;
begin
   Result := TOtuInfo(Item1).FId - TOtuInfo(Item2).FId;
end;

procedure TAllOtuInfo.SortByInputOrder;
begin
  FArray.Sort(CompareTaxaIds);
end;

function CompareGpNamesAsc(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item1).GpName) > 0) and (Length(TOtuInfo(Item2).GpName) > 0) then
  begin
    Result := CompareText(TOtuInfo(Item1).GpName, TOtuInfo(Item2).GpName);
    if Result = 0 then
      Result := CompareText(TOtuInfo(Item1).Name, TOtuInfo(Item2).Name);
  end
  else if (Length(TOtuInfo(Item1).GpName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item2).GpName) > 0) then
    Result := 1
  else
    Result := CompareText(TOtuInfo(Item1).Name, TOtuInfo(Item2).Name);
end;

function CompareGpNamesDes(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item2).GpName) > 0) and
     (Length(TOtuInfo(Item1).GpName) > 0) then
    Result := CompareText(TOtuInfo(Item2).GpName, TOtuInfo(Item1).GpName)
  else if (Length(TOtuInfo(Item2).GpName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item1).GpName) > 0) then
    Result := 1
end;

function CompareSpNamesAsc(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item1).SpName) > 0) and
     (Length(TOtuInfo(Item2).SpName) > 0) then
    Result := CompareText(TOtuInfo(Item1).SpName, TOtuInfo(Item2).SpName)
  else if (Length(TOtuInfo(Item1).SpName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item2).SpName) > 0) then
    Result := 1
end;

function CompareSpNamesDes(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item2).SpName) > 0) and
     (Length(TOtuInfo(Item1).SpName) > 0) then
    Result := CompareText(TOtuInfo(Item2).SpName, TOtuInfo(Item1).SpName)
  else if (Length(TOtuInfo(Item2).SpName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item1).SpName) > 0) then
    Result := 1
end;

function ComparePopNamesAsc(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item1).PopName) > 0) and
     (Length(TOtuInfo(Item2).PopName) > 0) then
    Result := CompareText(TOtuInfo(Item1).PopName, TOtuInfo(Item2).PopName)
  else if (Length(TOtuInfo(Item1).PopName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item2).PopName) > 0) then
    Result := 1
end;

function ComparePopNamesDes(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item2).PopName) > 0) and
     (Length(TOtuInfo(Item1).PopName) > 0) then
    Result := CompareText(TOtuInfo(Item2).PopName, TOtuInfo(Item1).PopName)
  else if (Length(TOtuInfo(Item2).PopName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item1).PopName) > 0) then
    Result := 1
end;

procedure TAllOtuInfo.SortByGroupNameAsc;
begin
  FArray.Sort(CompareGpNamesAsc);
end;

procedure TAllOtuInfo.SortByGroupNameDes;
begin
  FArray.Sort(CompareGpNamesDes);
end;

procedure TAllOtuInfo.SortBySpeciesNameAsc;
begin
  FArray.Sort(CompareSpNamesAsc);
end;

procedure TAllOtuInfo.SortBySpeciesNameDes;
begin
  FArray.Sort(CompareSpNamesDes);
end;

procedure TAllOtuInfo.SortByPopulationNameAsc;
begin
  FArray.Sort(ComparePopNamesAsc);
end;

procedure TAllOtuInfo.SortByPopulationNameDes;
begin
  FArray.Sort(ComparePopNamesDes);
end;



function CompareTaxaNamesAsc(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(TOtuInfo(Item1).FName, TOtuInfo(Item2).FName);
end;

function CompareTaxaNamesDec(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(TOtuInfo(Item2).FName, TOtuInfo(Item1).FName);
end;

procedure TAllOtuInfo.SortByTaxaNameAsc;
begin
  FArray.Sort(CompareTaxaNamesAsc);
end;

procedure TAllOtuInfo.SortByTaxaNameDes;
begin
  FArray.Sort(CompareTaxaNamesDec);
end;

function CompareGpAndTaxaName(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (Length(TOtuInfo(Item1).GpName) > 0) and
     (Length(TOtuInfo(Item2).GpName) > 0) then
  begin
    Result := CompareText(TOtuInfo(Item1).GpName, TOtuInfo(Item2).GpName);
    if Result = 0 then
       Result := CompareText(TOtuInfo(Item1).FName, TOtuInfo(Item2).FName);
  end
  else if (Length(TOtuInfo(Item1).GpName) > 0) then
    Result := -1
  else if (Length(TOtuInfo(Item2).GpName) > 0) then
    Result := 1
  else
    Result := CompareText(TOtuInfo(Item1).FName, TOtuInfo(Item2).FName);
end;

procedure TAllOtuInfo.SortByGroupAndTaxaName;
begin
  FArray.Sort(CompareGpAndTaxaName);
end;

function CurrentAllOtuInfos: TAllOtuInfo;
begin
  result := nil;
  if D_InputSeqData <> nil then
    result := D_InputSeqData.OtuInfos
  else if D_InputDistData <> nil then
    result := D_InputDistData.OtuInfos;
end;

function CompareByDiffFromFirstSeq(Item1: Pointer; Item2: Pointer): Integer;
var
  otu1: TOtuInfo = nil;
  otu2: TOtuInfo = nil;
begin
  otu1 := TOtuInfo(Item1);
  otu2 := TOtuInfo(Item2);
  Result := CompareValue(otu1.DiffFromFocalOtu, otu2.DiffFromFocalOtu, FP_CUTOFF);
  if Result = 0 then
    Result := CompareText(otu1.Name, otu2.Name);
end;

function CompareOtuByTimestamp(Item1: Pointer; Item2: Pointer): Integer;
var
  otu1: TOtuInfo = nil;
  otu2: TOtuInfo = nil;
begin
  otu1 := TOtuInfo(Item1);
  otu2 := TOtuInfo(Item2);
  Result := CompareValue(otu1.Year, otu2.Year);
  if Result = 0 then
    Result := CompareValue(ord(otu1.Month), ord(otu2.Month));
  if Result = 0 then
    Result := CompareValue(otu1.Day, otu2.Day);
  if Result = 0 then
    Result := CompareText(otu1.Name, otu2.Name);
end;

//=== this one uses TaxGps
function CompareTaxaGpView(Item1, Item2: Pointer): Integer;
var
  a: Integer = -1;
  b: Integer = -1;
begin
  Result := 0;
  if VS_SeqDataExplorer <> nil then
  begin
    a := VS_SeqDataExplorer.TaxaGpsDlgOrder[TOtuInfo(Item1).Id];
    b := VS_SeqDataExplorer.TaxaGpsDlgOrder[TOtuInfo(Item2).Id];
  end
  else if VS_DistDataExplorer <> nil then
  begin
    a := VS_DistDataExplorer.TaxaGpsDlgOrder[TOtuInfo(Item1).Id];
    b := VS_DistDataExplorer.TaxaGpsDlgOrder[TOtuInfo(Item2).Id];
  end;
  if a > b then        Result :=  1
  else if a < b then   Result := -1
  else                 Result :=  0;
end;

procedure TAllOtuInfo.SortByTaxaGpsDlg;
begin
  FArray.Sort(CompareTaxaGpView);
end;

procedure TAllOtuInfo.SortBySimilarityToFirstOtu;
begin
  FArray.Sort(CompareByDiffFromFirstSeq);
end;

procedure TAllOtuInfo.HideUnselected;
var
  i: Integer = 0;
begin
  if FNoOfOtus > 0 then
    for i := 0 to FNoOfOtus - 1 do
      if not GetOtuInfo(i).FIsUsed then
        GetOtuInfo(i).IsHidden := True
      else
        GetOtuInfo(i).IsHidden := False;
end;

procedure TAllOtuInfo.UnhideAll;
var
  i: Integer = 0;
begin
  if FNoOfOtus > 0 then
    for i := 0 to FNoOfOtus - 1 do
      GetOtuInfo(i).IsHidden := False;
end;

//==== General purpose functions
function CountNoOfGpsInOtuInfos(OtuInfos: TList): Integer;
var
  i,j: Integer;
  IsStrFound: Boolean;
  {$IFDEF VISUAL_BUILD}
  updateTime: TDateTime;
  {$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  updateTime := Now;
  {$ENDIF}
  Result := 0;
  if OtuInfos = nil then
    Exit;

  for i:=0 to OtuInfos.Count-1 do
  begin
    if Length(TOtuInfo(OtuInfos[i]).GpName) = 0 then
      Continue; // empty str is not worth it
    IsStrFound := False;
    for j:=i+1 to OtuInfos.Count-1 do
      if CompareStr(TOtuInfo(OtuInfos[i]).GpName,
                    TOtuInfo(OtuInfos[j]).GpName) = 0 then // same string found then break
      begin
        IsStrFound := True;
        break;
      end;
    if not IsStrFound then
      Inc(Result);
    {$IFDEF VISUAL_BUILD}
    if MillisecondsBetween(Now, updateTime) > 300 then
      Application.ProcessMessages;
    {$ENDIF}
  end;
end;

procedure GetGpIdForOtuInfos(OtuInfos: TList; var MyGpIds: array of Integer);
var
  NextId, i,j: Integer;
  {$IFDEF VISUAL_BUILD}
  updateTime: TDateTime;
  {$ENDIF}
begin
  if Length(MyGpIds) = 0 then
    Exit;
  {$IFDEF VISUAL_BUILD}
  updateTime := Now;
  {$ENDIF}
  for i:=0 to OtuInfos.Count-1 do
    MyGpIds[i] := -1;

  NextId := 0;
  for i:=0 to OtuInfos.Count-1 do
  begin
    if Length(TOtuInfo(OtuInfos[i]).GpName) = 0 then
      Exit;
    if MyGpIds[i] >= 0 then  // we already know the id
      continue;

    MyGpIds[i] := NextId;
    // now mark all the same one's below with nextId
    for j:=i+1 to OtuInfos.Count-1 do
      if CompareText(TOtuInfo(OtuInfos[i]).GpName, TOtuInfo(OtuInfos[j]).GpName) = 0 then // same string found then break
        MyGpIds[j] := NextId;
    Inc(NextId);
    {$IFDEF VISUAL_BUILD}
    if MillisecondsBetween(Now, updateTime) > 300 then
      Application.ProcessMessages;
    {$ENDIF}
  end;
end;

procedure TAllOtuInfo.WriteToFile(var data: TFileStream; OtuDataTypeIn : TOtuDataType);
var
  i, j, k : integer;
  m : Double;
  tempList, BufferString : AnsiString;
begin
      tempList := EmptyStr;
      for j:=0 to NoOfOtus-1 do
        tempList := tempList + Otu[j].Name + ',';
      SetLength(tempList, (length(tempList)-1)); //trim the last comma
      i:= length(tempList);
      data.Write(i, 4);
      data.Write(tempList[1], i);

      tempList := EmptyStr;
      for j:=0 to NoOfOtus-1 do
        tempList := tempList + Otu[j].GpName + ',';
      SetLength(tempList, (Length(tempList)-1));
      i:= length(tempList);
      data.Write(i, 4);
      data.Write(tempList[1], i);

      tempList := EmptyStr;
      for j:=0 to NoOfOtus-1 do
        tempList := tempList + Otu[j].SpName + ',';
      SetLength(tempList, (Length(tempList)-1));
      i:= length(tempList);
      data.Write(i, 4);
      data.Write(tempList[1], i);

      tempList := EmptyStr;
      for j:=0 to NoOfOtus-1 do
        tempList := tempList + Otu[j].PopName + ',';
      SetLength(tempList, (Length(tempList)-1));
      i:= length(tempList);
      data.Write(i, 4);
      data.Write(tempList[1], i);

      tempList := EmptyStr;
      for j := 0 to NoOfOtus - 1 do
      begin
        BufferString := Otu[j].GeographicalInfo.MegFileCommandString;
        if Trim(BufferString) <> EmptyStr then
          tempList := tempList + BufferString + ','
        else
          tempList := tempList + NO_GEOGRAPHICAL_INFO + ',';
      end;
        SetLength(TempList, Length(TempLIst) - 1);
        i:= length(tempList);
        data.Write(i, 4);
        data.Write(tempList[1], i);

      for j:=0 to NoOfOtus-1 do
        data.Write(Otu[j].Id, 4);

      for j:=0 to NoOfOtus-1 do
        data.Write(Otu[j].IsUsed, 1);

      for j:=0 to NoOfOtus-1 do
      begin
        case OtuDataTypeIn of
        DataDist:  //We know the OTUs hold a ditance Matrix
        begin
          for k:=0 to NoOfOtus-1 do
          begin
            m :=PArrayOfDouble(TOtuInfo(FArray.Items[j]).Data)[k];
            data.Write(m, sizeof(double));
          end;
        end;
       DataString:
          begin
            BufferString := AnsiString(TOtuInfo(FArray.Items[j]).Data);
            i := length(BufferString);
            data.Write(i, 4);
            data.Write(BufferString, i);
          end;
        end;
      end;
end;

procedure TAllOtuInfo.RestoreFromFile(data: TStream; OtuDataType: TOtuDataType; version: Integer);
var
  i: Integer = -1;
  j, k : integer;
  TaxaCSV, GroupsCSV, SpeciesCSV, PopulationCSV, geoCSV : AnsiString;
  TaxaList: TStringList = nil;
  GroupsList: TStringList = nil;
  SpeciesList: TStringList = nil;
  PopulationList: TStringList = nil;
  GeoInfoList: TStringList = nil;
begin
  try
    TaxaCSV := readStringFromFile(data);
    TaxaList := TStringList.Create;
    splitstr(TaxaCSV, ',', TaxaList);
    NoOfOtus := TaxaList.Count;

    GroupsCSV := readStringFromFile(data);
    GroupsList := TStringList.Create;
    splitstr(GroupsCSV, ',', GroupsList);

    if version >= 1001 then
    begin
      SpeciesCSV := readStringFromFile(data);
      SpeciesList := TStringList.Create;
      splitstr(SpeciesCSV, ',', SpeciesList);

      PopulationCSV := readStringFromFile(data);
      PopulationList := TStringList.Create;
      splitstr(PopulationCSV, ',', PopulationList);
    end;

    if version >= 1012 then
    begin
      GeoInfoList := TStringList.Create;
      geoCSv := readStringFromFile(data);
      splitStr(geoCSV, ',', GeoInfoList);
    end;

    for j:=0 to NoOfOtus-1 do
    begin
      Otu[j] := TOtuInfo.Create;
      Otu[j].Name := TaxaList.Strings[j];
      Otu[j].GpName := GroupsList.Strings[j];
      if (version >= 1001) and (SpeciesList.Count > j) then
        Otu[j].SpName := SpeciesList.Strings[j];
      if (version >= 1001) and (PopulationList.Count > j) then
        Otu[j].PopName := PopulationList.Strings[j];
      if (version >= 1012) and (GeoInfoList[j] <> NO_GEOGRAPHICAL_INFO) then
        Otu[j].GeographicalInfo.InitFromMegaFileCommandString(GeoInfoList[j]);
    end;

    for j:=0 to NoOfOtus-1 do
    begin
      data.read(i, 4);
      Otu[j].Id := i;
    end;

    //ASSIGN WHICH OTUs ARE USED
    for j:=0 to NoOfOtus-1 do
    begin
      i := 0;
      data.read(i, 1);
      Otu[j].IsUsed := (i = 1);
    end;

    for j:=0 to NoOfOtus-1 do
     begin
       Otu[j].Data := NewDistArray(NoOfOtus);
       for k:=0 to NoOfOtus-1 do
       begin
         case OtuDataType of
         DataDist:
           begin
               //Data.Read(TempDistMatrix[j, k], SizeOf(Double));
               Data.Read(PArrayOfDouble(Otu[j].Data)[k], sizeof(double))
           end;
         DataString:
           begin

             //TaxaCSV := String(Otu[j].Data);
             //i := length(BufferString);
             //data.Write(i, 4);
             //data.Write(BufferString, i);

           end;
         end;
       end;
     end;
  finally
    if Assigned(TaxaList) then
      TaxaList.Free;
    if Assigned(GroupsList) then
      GroupsList.Free;
    if Assigned(SpeciesList) then
      SpeciesList.Free;
    if Assigned(PopulationList) then
      PopulationList.Free;
    if Assigned(GeoInfoList) then
      GeoInfoList.Free;
  end;
end;

function TAllOtuInfo.NumActiveOutgroupMembers: Integer;
var
  i: Integer;
begin
  Result := 0;
  if NoOfOtus > 0 then
  begin
    for i := 0 to NoOfOtus - 1 do
    begin
      if (Otu[i].OutgroupMember and Otu[i].IsUsed) then
        inc(Result);
    end;
  end;
end;

end.
