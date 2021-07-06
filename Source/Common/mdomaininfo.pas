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

unit MDomainInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MegaConsts, msite_labels;

type

  { TDomainInfo }

  TDomainInfo = class    {------ KEEPS ONE GENE/DOMAIN INFO ----}
  protected
    FName:       AnsiString;  // Domain Name
    FGeneName:   AnsiString;  // if it domain then its gene name is given here
    FIsGene:     Boolean; // specifies gene status
    FIsCoding:   Boolean; // Is protein-coding?
    FCodonStart: Integer; // Codon start from site; 0 based
    FFromSite:   Integer; // First site included in this domain; 0 based
    FToSite:     Integer; // Last site included in this domain ; 0 based
    FIsUsed:     Boolean;

    function  GetNoOfSites: Integer;
    function  GetIsDomain: Boolean;
    procedure SetIsDomain(Value: Boolean);
    function  GetInfoString: AnsiString;
    function  GetSiteRangeString: AnsiString;
    function  GetFullInfoString: AnsiString;
    function  GetCodonSiteAttr(Index: Integer): TSiteAttr;
  public
    IsTemp: Boolean;
    ChildDomains: TList;
    ParentDomain: TDomainInfo;
    Constructor Create;
    destructor Destroy; override;

    procedure CopyFrom(const Value: TDomainInfo);
    procedure WriteToFile(data : TStream);
    procedure RestoreFromFile(var data : TFileStream);
    procedure Assign(data : TDomainInfo);
    procedure UpdateGeneBoundaries;
    function CheckBoxState: TMegaCheckBoxState;
    function IsChild(aInfo: TDomainInfo): Boolean;
    function IsSibling(aInfo: TDomainInfo): Boolean;
    function IsParent(aInfo: TDomainInfo): Boolean;
    property Name: AnsiString read FName write FName;
    property GeneName: AnsiString read FGeneName write FGeneName;
    property IsGene: Boolean read FIsGene write FIsGene;
    property IsDomain: Boolean read GetIsDomain write SetIsDomain;
    property IsCoding: Boolean read FIsCoding write FIsCoding;

    property IsUsed: Boolean read FIsUsed write FIsUsed;

    property NoOfSites: Integer read GetNoOfSites;
    property CodonStart: Integer read FCodonStart write FCodonStart;
    property FromSite: Integer read FFromSite write FFromSite;
    property ToSite: Integer read FToSite write FToSite;

    property CodonSiteAttr[Index: Integer]: TSiteAttr read GetCodonSiteAttr;// = meg1stSite, meg2ndSite, meg3rdSite, megNoncoding

    property InfoString: AnsiString read GetInfoString;
    property SiteRangeString: AnsiString read GetSiteRangeString;
    property FullInfoString: AnsiString read GetFullInfoString;
  end;

  TDomainInfoArray = array of TDomainInfo;
  TArrayOfTDomainInfo = array[0..MaxPointerElts-1] of TDomainInfo;
  PArrayOfTDomainInfo = ^TArrayOfTDomainInfo;

  // A list of all domain infos

  { TAllDomainInfo }

  TAllDomainInfo = class
  private
    FArray:       TList;
    function  GetDomainInfo(Index: LongInt): TDomainInfo;
    function  GetNoOfDomains: LongInt;
    procedure SetDomainInfo(Index: LongInt; Item: TDomainInfo);
    function DomainNameIsUsed(aName: String): Boolean;
  public
    constructor Create;
    destructor  Destroy; override; // necessary
    procedure   WriteToFile(data : TFileStream);
    procedure   RestoreFromFile(var data : TFileStream);
    procedure  Add(Item: TDomainInfo);
    function Remove(aInfo: TDomainInfo): Integer;
    function GetDomainByGeneName(aName: String): TDomainInfo;
    function GetGeneByGeneName(aName: String): TDomainInfo;
    function GetGeneChildDomains(geneName: String): TList;
    function NextAvailableDomainName: String;
    property   NoOfDomains: LongInt read GetNoOfDomains;
    property   X[Index: LongInt]: TDomainInfo read GetDomainInfo write SetDomainInfo; default;
  end;

  // for Domain Mark

  { TAllSiteDomainMark }

  TAllSiteDomainMark = class    {------ Keeps info about all Sites ----}
  private
    FSiteLabelOverrides: TSiteLabelArray;
    FArray:      PArrayOfTDomainInfo;
    FSiteMarks:  PAnsiChar;
    FNoOfSites:  LongInt;
    FIsDirty   : Boolean;
    FIsUsedIndependent: Boolean;
    FIndependentCount: LongInt;
    FNoOfMarkedSites   : LongInt;
    FSiteMarkTypes     : TStringList;
    FUseUnlabeledSites: Boolean;

    procedure SetNoOfSites(Value: LongInt);
    function  GetDomainMark(Index: LongInt): TDomainInfo;
    procedure SetDomainMark(Index: LongInt; Item: TDomainInfo);
    function  GetSiteMark(Index: LongInt): AnsiChar;
    procedure SetSiteMark(Index: LongInt; Item: AnsiChar);
    function  GetIndependentCount: LongInt;
    function  GetIsSiteUsed(Index: LongInt): Boolean; // consideres site label inclusion/exclusion
    function  GetIsSiteMarkTypeUsed(Item: AnsiChar): Boolean;
    procedure SetIsSiteMarkTypeUsed(Item: AnsiChar; Value: Boolean);
    function  GetSiteLabelType(Index: LongInt): AnsiChar;  //
    function  GetNoOfSiteLabelTypes: LongInt;
    function  GetNoOfDomains:LongInt;
  public
    constructor Create;
    destructor  Destroy; override; // necessary
    procedure   WriteToFile(data : TStream);
    procedure   RestoreFromFile(var data : TFileStream);
    procedure   Assign(data: TAllSiteDomainMark);
    function    IsCoding: Boolean;
    function    IsSiteUsedWithLabel(Index: LongInt; RestrictBySiteLabel: Boolean=True): Boolean;
    function    IsSiteUsedWithNoLabel(Index: LongInt): Boolean;
    function HasLabelOverride(site: LongInt): Boolean;
    function    GetNoOfIncludedDomains: LongInt;
    function SiteLabelsString: AnsiString;
    procedure   GetIncludedDomains(var IncludedDomains: TDomainInfoArray);
    procedure   SetSiteLabelsToInclude(LabelsToInclude: TStringList);
    procedure ClearAll;
    function HasLabelledSites: Boolean;
    function AddSiteLabelOverrides(const aLabel: AnsiChar; const sites: String; var msg: String): Boolean;
    procedure ClearSiteMarks;
    property    NoOfDomains:LongInt read GetNoOfDomains;
    property    NoOfSites:LongInt read FNoOfSites write SetNoOfSites;
    property    X[Index: LongInt]: TDomainInfo read GetDomainMark write SetDomainMark; default;
    property    SiteLabel[Index: LongInt]: AnsiChar read GetSiteMark write SetSiteMark;
    property    IsSiteLabelTypeUsed[Index: AnsiChar]: Boolean read GetIsSiteMarkTypeUsed write SetIsSiteMarkTypeUsed;
    property    SiteMarks:PAnsiChar read FSiteMarks write FSiteMarks;
    property    SiteLabelType[Index: LongInt]: AnsiChar read GetSiteLabelType;
    property    NoOfSiteLabelTypes: LongInt         read GetNoOfSiteLabelTypes;
    property    SiteMarkTypes:TStringList read FSiteMarkTypes write FSiteMarkTypes;
    property    NoOfLabelledSites: LongInt read FNoOfMarkedSites write FNoOfMarkedSites;
    property    IndependentCount: LongInt read GetIndependentCount;
    property    IsUsedIndependent: Boolean read FIsUsedIndependent write FIsUsedIndependent;
    property    IsSiteUsed[Index: LongInt]: Boolean read GetIsSiteUsed;
    property    IsDirty: Boolean read FIsDirty write FIsDirty;
    property    UseUnlabeledSites: Boolean read FUseUnlabeledSites write FUseUnlabeledSites;
  end;

implementation

uses
  {$IFNDEF FPC}
  ErrorMessages_HC,
  {$ENDIF}
  MegaErrUtils, SysUtils, Dialogs, MGlobalSettings, StringUtils, MegaUtils;


//======= DOMAIN MARK SYSTEM ===
constructor TAllSiteDomainMark.Create;
begin
  inherited Create;
  FNoOfSites := 0;
  FArray := nil;
  FIsDirty := False;
  FSiteMarks:= nil;
  FNoOfMarkedSites := 0;
  FSiteMarkTypes := nil;
  FIsUsedIndependent:=True;
  FIndependentCount:=-1;
  FUseUnlabeledSites := True;
end;

destructor TAllSiteDomainMark.Destroy;
var
  i: Integer;
begin
  {TODo -o DanResolve: Test to make sure tha the following do not leak memory}
  if FNoOfSites > 0 then
  begin
    FreeMemAndNil(FArray);
    FArray := nil;
    FreeMemAndNil(FSiteMarks);
    FreeAndNil(FSiteMarkTypes);
    for i := Low(FSiteLabelOverrides) to High(FSiteLabelOverrides) do
      if Assigned(FSiteLabelOverrides[i]) then
        FSiteLabelOverrides[i].Free;
  end;
  
  if FsiteMarkTypes <> nil then
    FreeAndNil(FSiteMarkTypes);
  inherited Destroy;
end;

procedure TAllSiteDomainMark.SetNoOfSites(Value: LongInt);
var
  i: LongInt;
begin
  if FNoOfSites > 0 then
    RaiseErrorMessage(-1, ClassName);
  GetMem(FSiteMarks, Value*sizeof(AnsiChar));
  GetMem(FArray, Value*sizeOf(TDomainInfo));
  FNoOfSites := Value;
  for i:=0 to FNoOfSites-1 do
    FArray[i] := nil;
  for i:=0 to FNoOfSites-1 do
    FSiteMarks[i] := ' ';
  FSiteMarkTypes := TStringList.Create;
  SetLength(FSiteLabelOverrides, Value);
  for i := Low(FSiteLabelOverrides) to High(FSiteLabelOverrides) do
    FSiteLabelOverrides[i] := nil;
end;

function  TAllSiteDomainMark.IsCoding: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to FNoOfSites-1 do
    if FArray[i] <> nil then
      if GetDomainMark(i).IsCoding then
      begin
        Result := True;
        Exit;
      end;
end;

function TAllSiteDomainMark.GetDomainMark(Index: LongInt): TDomainInfo;
begin
  Result := FArray[Index];
end;

procedure TAllSiteDomainMark.SetDomainMark(Index: LongInt; Item: TDomainInfo);
begin
  FArray[Index] := Item;
end;

function TAllSiteDomainMark.GetSiteMark(Index: LongInt): AnsiChar;
begin
  if (Index < Length(FSiteLabelOverrides)) and Assigned(FSiteLabelOverrides[Index]) then
    Result := FSiteLabelOverrides[Index].SiteLabel
  else
    begin
      if FSiteMarks[Index] = ' ' then
        Result := '_'
      else
        Result := FSiteMarks[Index];
    end;
end;

function GetIndexOf(AList: TStringList; AChar: AnsiChar): Integer;
var
  i: Integer;
begin
  If AList.Count > 0 then
    for i:=0 to AList.Count-1 do
      if Pos(AChar, AnsiString(Alist[i][1])) > 0 then
      begin
        Result := i;
        Exit;
      end;
  Result := -1;
end;

procedure TAllSiteDomainMark.SetSiteMark(Index: LongInt; Item: AnsiChar);
begin
  if FSiteMarks[Index] = Item then
    Exit;
  if Item = ' ' then
    Dec(FNoOfMarkedSites)
  else
    with FSiteMarkTypes do
    begin
      if GetIndexOf(FSiteMarkTypes, Item) < 0 then
      begin
        Add(Item);
        Objects[Count-1] := Pointer(1); //included by default
      end;
      Inc(FNoOfMarkedSites);
    end;
  FSiteMarks[Index] := Item;
end;

function  TAllSiteDomainMark.GetSiteLabelType(Index: LongInt): AnsiChar;  //
begin
  Result := AnsiChar(FSiteMarkTypes[Index][1]);  // first char
end;

function  TAllSiteDomainMark.GetNoOfSiteLabelTypes: LongInt;
begin
  Result := FSiteMarkTypes.Count;
end;

function  TAllSiteDomainMark.GetIsSiteMarkTypeUsed(Item: AnsiChar): Boolean;
var
  MyIndex: LongInt;
begin
  MyIndex := FSiteMarkTypes.IndexOf(Item);
  Result := FSiteMarkTypes.Objects[MyIndex] <> nil;
end;

procedure TAllSiteDomainMark.SetSiteLabelsToInclude(LabelsToInclude: TStringList);
var
  i: integer;
  AMark: AnsiChar;
begin
  for i:=0 to FSiteMarkTypes.Count-1 do  // this means that exclude all
    FSiteMarkTypes.Objects[i] := nil;

  if LabelsToInclude = nil then
    Exit;

  for i:=0 to LabelsToInclude.Count-1 do
  begin
    AMark := AnsiChar(LabelsToInclude[i][1]);
    SetIsSiteMarkTypeUsed(AMark, True);
  end;
end;

procedure TAllSiteDomainMark.ClearAll;
var
  i: Integer;
begin
  if NoOfSites > 0 then
    for i := 0 to NoOfSites - 1 do
      FArray[i] := nil;
end;

function TAllSiteDomainMark.HasLabelledSites: Boolean;
begin
  Result := (FNoOfMarkedSites > 0);
end;

function TAllSiteDomainMark.AddSiteLabelOverrides(const aLabel: AnsiChar; const sites: String; var msg: String): Boolean;
var
  tokens: TStringList = nil;
  i: Integer;
  lbl: TSiteLabel = nil;
  tempInt: LongInt;
  str: String;
begin
  try
    try
      if not IsValidSiteLabel(aLabel) then
        raise Exception.Create('site label must be one of A-z a-z 0-9 * + - _');
      str := Trim(sites);
      while (Length(str) > 1) and (str[Length(str)] = ',') do
        str := copy(str, 1, Length(str) - 1);
      while (Length(str) > 1) and (str[1] = ',') do
        str := copy(str, 2, Length(str));
      Result := True;
      tokens := TStringList.Create;
      if not SplitOnSingleCharFaster(str, ',', tokens, True) then
        raise Exception.Create('failed to parse site list');
      if tokens.Count > 0 then
        for i := 0 to tokens.Count - 1 do
        begin
          str := Trim(tokens[i]);
          if str <> EmptyStr then
          begin
            if TryStrToInt(str, tempInt) then
            begin
              if ((tempInt - 1) >= 0) and ((tempInt - 1) < FNoOfSites) then
              begin
                lbl := TSiteLabel.Create(tempInt - 1, aLabel);
                if Assigned(FSiteLabelOverrides[tempInt - 1]) then
                  FreeAndNil(FSiteLabelOverrides[tempInt - 1])
                else if FSiteMarks[tempInt - 1] = ' ' then
                  inc(FNoOfMarkedSites);
                FSiteLabelOverrides[tempInt - 1] := lbl;
              end
              else
                msg := msg + LineEnding + str + ' is not a valid site number';
            end
            else
            begin
              msg := msg + LineEnding + str + ' is not a valid integer';
              Result := False;
            end;
          end;
        end;
    except
      on E:Exception do
      begin
        Result := False;
        msg := E.Message;
      end;
    end;
  finally
    if Assigned(tokens) then
      tokens.Free;
  end;
end;

procedure TAllSiteDomainMark.ClearSiteMarks;
var
  i: Integer;
begin
  if FNoOfSites > 0 then
    for i := 0 to FNoOfSites - 1 do
    begin
      FSiteMarks[i] := ' ';
      if Assigned(FSiteLabelOverrides[i]) then
        FreeAndNil(FSiteLabelOverrides[i]);
    end;
end;

procedure TAllSiteDomainMark.SetIsSiteMarkTypeUsed(Item: AnsiChar; Value: Boolean);
var
  MyIndex: LongInt;
begin
  MyIndex := GetIndexOf(FSiteMarkTypes, Item);
  if Value then
    FSiteMarkTypes.Objects[MyIndex] := Pointer(1)
  else
    FSiteMarkTypes.Objects[MyIndex] := nil;
end;

procedure TAllSiteDomainMark.GetIncludedDomains(var IncludedDomains: TDomainInfoArray);
var
  i: Integer;
begin
    SetLength(IncludedDomains, 0);
    for i := 0 to NoOfSites - 1 do
    begin
      if Assigned(FArray[i]) and FArray[i].IsUsed and ((i = 0) or (FArray[i] <> FArray[i - 1])) then
      begin
        SetLength(IncludedDomains, Length(IncludedDomains) + 1);
        IncludedDomains[Length(IncludedDomains) - 1] := FArray[i];
      end;
    end;
end;

function TAllSiteDomainMark.GetIndependentCount: LongInt;
var
  i: Integer;
begin
  if (FIndependentCount < 0) or FIsDirty then
  begin
    FIndependentCount := 0;
    for i:=0 to FNoOfSites-1 do
      if FArray[i] = nil then
        Inc(FIndependentCount);
  end;
  Result := FIndependentCount;
end;

function TAllSiteDomainMark.IsSiteUsedWithNoLabel(Index: LongInt): Boolean;
begin
  Result := GetIsSiteUsed(Index) and (FSiteMarks[Index] = ' ');
end;

function TAllSiteDomainMark.HasLabelOverride(site: LongInt): Boolean;
begin
  Result := Assigned(FSiteLabelOverrides[site]);
end;

function TAllSiteDomainMark.IsSiteUsedWithLabel(Index: LongInt; RestrictBySiteLabel: Boolean=True): Boolean;
begin
  Result := GetIsSiteUsed(Index);

  if (not Result) OR (not RestrictBySiteLabel) then
    Exit;

  if FSiteMarks[Index] = ' ' then
  begin
    Result := False;
    Exit; // not a marked site
  end;

  Result := IsSiteLabelTypeUsed[FSiteMarks[Index]];
end;

function TAllSiteDomainMark.GetIsSiteUsed(Index: LongInt): Boolean;
begin
  try
    Assert(Index < NoOfSites);
    Result := False;
    if Index >= NoOfSites then
      Exit;
    if FArray[Index] = nil then
    begin
      if not FIsUsedIndependent then
        Exit;
    end
    else
    begin
      if not TDomainInfo(FArray[Index]).IsUsed then
        Exit;
    end;
    if (not FUseUnlabeledSites) and (FSiteMarks[Index] = ' ') then
      Exit;
    Result := True;
  except
    on E : Exception do
    begin
      raise Exception.Create('Tried to get DomainInfo for site: ' + IntToStr(Index));
    end;
  end;
end;

{
function TAllSiteDomainMark.GetIsSiteUsedPrimary(Index: LongInt): Boolean;
begin
  Result := False;
  if FArray[Index] = nil then
  begin
    if not FIsUsedIndependent then
      Exit;
  end
  else
  begin
    if not TDomainInfo(FArray[Index]).IsUsed then
      Exit;
  end;
  Result := True;
end;

procedure TAllSiteDomainMark.IncOwnerCount;
begin
  Inc(FOwnerCount);
end;

procedure TAllSiteDomainMark.DecOwnerCount;
begin
  Dec(FOwnerCount);
  if FOwnerCount < 0 then
    RaiseErrorMessage(HC_Unexpected_Error, 'Trying to deallocate '+ClassName+ ' too often');
end;     }

//======== All domains info ====
//--- TDomainInfo ----
constructor TDomainInfo.Create;
begin
  FName       := EmptyStr;
  FGeneName   := EmptyStr;
  FIsGene     := False;
  FIsCoding   := False;
  FCodonStart := 0;
  FFromSite   := -1;
  FToSite     := -1;
  FIsUsed     := True;
  ChildDomains := TList.Create;
  ParentDomain := nil;
  IsTemp := False;
end;

destructor TDomainInfo.Destroy;
begin
  if Assigned(ChildDomains) then { do not free the domains, they are owned by TAllDomainInfo}
    ChildDomains.Free;
  ParentDomain := nil;
  inherited;
end;

//--- Get/Set IsDomain ----
function TDomainInfo.GetIsDomain: Boolean;
begin
  Result := not FIsGene;
end;

procedure TDomainInfo.SetIsDomain(Value: Boolean);
begin
  FIsGene := not Value;
end;

//--- No of Sites
function TDomainInfo.GetNoOfSites: Integer;
begin
  if (FFromSite <> -1) and (FToSite <> -1) then
    Result := FToSite - FFromSite+1
  else
    Result := -1;
end;

//--- Gets codon attribute
function TDomainInfo.GetCodonSiteAttr(Index: Integer): TSiteAttr;
var
  Residual: integer;
begin
  Result := [meg0thBase];
  if not IsCoding then
    Exit;
  Residual := (Index - FFromSite) mod 3;

  Residual := Residual - FCodonStart;
  if Residual < 0 then
    Residual := Residual + 3;  // this correctly transforms negative numbers to positives

  case Residual of
    0:  Result := [meg1stBase];
    1:  Result := [meg2ndBase];
    2:  Result := [meg3rdBase];
  end;
{
  // now coding area
  case FCodonStart of
    0:
      case Residual of
        0:  Result := [meg1stBase];
        1:  Result := [meg2ndBase];
        2:  Result := [meg3rdBase];
      end;
    1:
      case Residual of
        0:  Result := [meg3rdBase];
        1:  Result := [meg1stBase];
        2:  Result := [meg2ndBase];
      end;
    2:
      case Residual of
        0:  Result := [meg3rdBase];
        1:  Result := [meg1stBase];
        2:  Result := [meg2ndBase];
      end;
  end;
}
{  case ((Index - (FFromSite+FCodonStart)) mod 3) of
    -2: Result := [meg3rdBase];
    -1: Result := [meg2ndBase];
     0: Result := [meg1stBase];
     1: Result := [meg2ndBase];
     2: Result := [meg3rdBase];
  end;}
end;

procedure TDomainInfo.CopyFrom(const Value: TDomainInfo);  // copies perfectly
begin
  FName       := Value.Name;
  FGeneName   := Value.GeneName;
  FIsGene     := Value.IsGene;
  FIsCoding   := Value.IsCoding;
  FCodonStart := Value.CodonStart;
  FFromSite   := Value.FromSite;
  FToSite     := Value.ToSite;
  FIsUsed     := Value.IsUsed;
end;

function TDomainInfo.GetInfoString: AnsiString;
begin
  if FIsCoding then
    Result := 'Coding; CodonStart = ' + IntToStr(FCodonStart+1) +';'
  else
    Result := EmptyStr;
end;

function TDomainInfo.GetSiteRangeString: AnsiString;
begin
  if FFromSite < 0 then
    Result := '(Undefined)'
  else
    Result := '('+IntToStr(FFromSite+1) + ' - ' + IntToStr(FToSite+1)+')';
end;

function TDomainInfo.GetFullInfoString: AnsiString;
begin
  if IsGene then
    Result := 'Gene: '+FName
  else
    Result := FName+' '+GetSiteRangeString+' '+GetInfoString;
end;

//=== All Domains system
constructor TAllDomainInfo.Create;
begin
  inherited Create;
  FArray := TList.Create;
 // FOwnerCount := 0;
end;

destructor TAllDomainInfo.Destroy;
var
  i: LongInt;
begin
  // destroy elements just in case they were not possessed by GeneDomainDlg
  for i:= 0 to FArray.Count-1 do
    if FArray[i] <> nil then TDomainInfo(FArray[i]).Free;
  FArray.free;
  FArray := nil;
  inherited Destroy;
end;

function TAllDomainInfo.GetDomainInfo(Index: LongInt): TDomainInfo;
begin
  Result := FArray[Index];
end;

procedure TAllDomainInfo.SetDomainInfo(Index: LongInt; Item: TDomainInfo);
begin
  FArray[Index] := Item;
end;

function TAllDomainInfo.DomainNameIsUsed(aName: String): Boolean;
var
  i: Integer;
  aInfo: TDomainInfo = nil;
begin
  Result := False;
  if  NoOfDomains > 0 then
    for i := 0 to NoOfDomains - 1 do
    begin
      aInfo := GetDomainInfo(i);
      if aInfo.IsDomain and SameText(aName, aInfo.Name) then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

procedure TAllDomainInfo.Add(Item: TDomainInfo);
begin
  FArray.Add(Item);
end;

function TAllDomainInfo.Remove(aInfo: TDomainInfo): Integer;
begin
  Result := FArray.Remove(aInfo);
end;

function TAllDomainInfo.GetDomainByGeneName(aName: String): TDomainInfo;
var
  i: Integer;
begin
  Result := nil;
  if FArray.Count = 0 then
    Exit;
  for i := 0 to FArray.Count - 1 do
    if (TDomainInfo(FArray[i]).GeneName = aName) and (TDomainInfo(FArray[i]).IsDomain) then
    begin
      Result := TDomainInfo(FArray[i]);
      Exit;
    end;
end;

function TAllDomainInfo.GetGeneByGeneName(aName: String): TDomainInfo;
var
  i: Integer;
begin
  Result := nil;
  if FArray.Count = 0 then
    Exit;
  for i := 0 to FArray.Count - 1 do
    if (TDomainInfo(FArray[i]).GeneName = aName) and (TDomainInfo(FArray[i]).IsGene) then
    begin
      Result := TDomainInfo(FArray[i]);
      Exit;
    end;
end;

function TAllDomainInfo.GetGeneChildDomains(geneName: String): TList;
var
  i: Integer;
  aInfo: TDomainInfo;
begin
  Result := TList.Create;
  if FArray.Count = 0 then
    Exit;
  for i := 0 to FArray.Count - 1 do
  begin
    aInfo := TDomainInfo(FArray[i]);
    if aInfo.IsDomain and (aInfo.GeneName = geneName) then
      Result.Add(aInfo);
  end;
end;

function TAllDomainInfo.NextAvailableDomainName: String;
var
  i: Integer;
begin
  Result := 'Domain-1';
  i := 1;
  while DomainNameIsUsed(Result) and (i < MaxInt) do
  begin
    inc(i);
    Result := 'Domain-' + IntToStr(i);
  end;
end;

function  TAllDomainInfo.GetNoOfDomains: LongInt;
begin
  Result := FArray.Count;
end;
      {
procedure TAllDomainInfo.IncOwnerCount;
begin
  Inc(FOwnerCount);
end;

procedure TAllDomainInfo.DecOwnerCount;
begin
  Dec(FOwnerCount);
  if FOwnerCount < 0 then
    RaiseErrorMessage(HC_Unexpected_Error, 'Trying to deallocate '+ClassName+' too often');
end;
             }
procedure TAllDomainInfo.WriteToFile(data: TFileStream);
var
  i, j : integer;
begin
   j :=0;
   while(x[j] <> nil) do
   begin
       j := j + 1;
   end;
  //i := NoOfDomains;
  //data.Write(i, 4);
  data.write(j, 4); //number of DomainInfos to be saved
  for i:=0 to j-1 do
  begin
    if x[i] <> nil then
      TDomainInfo(x[i]).WriteToFile(data);
  end;

end;

procedure TDomainInfo.WriteToFile(data: TStream);
var
  i : integer;
  BufferString : AnsiString;
begin
  i := length(Fname);
  data.write(i, 4);//How long the domain name is
  BufferString := FName;
  data.write(BufferString[1], i);// Domain Name

  i := length(FGeneName);
  data.write(i, 4); //How long the gene name is
  BufferString := FGeneName;
  if i <> 0 then
    data.write(BufferString[1], i);  // if it domain then its gene name is given here

  data.write(FIsGene, 1); // specifies gene status
  data.write(FIsCoding, 1); // Is protein-coding?
  data.write(FCodonStart, 4); // Codon start from site; 0 based
  data.write(FFromSite, 4); // First site included in this domain; 0 based
  data.write(FToSite, 4); // Last site included in this domain ; 0 based
  data.write(FIsUsed, 1);
end;

procedure TAllDomainInfo.RestoreFromFile(var data: TFileStream);
var
  i, numDomainInfos : integer;
  TempDomainInfo : TDomainInfo;
begin
  NumDomainInfos := 0;
  data.read(numDomainInfos, 4); //How many TDomainInfos were saved
  for i :=0 to numDomainInfos-1 do
  begin
    TempDomainInfo := nil;
    TempDomainInfo := TDomainInfo.Create;
    TempDomainInfo.RestoreFromFile(data);
    FArray.Add(TempDomainInfo);
    TempDomainInfo := nil;
  end;
end;

procedure TDomainInfo.RestoreFromFile(var data: TFileStream);
var
  i : integer;
  BufferString : AnsiString;
begin
  i := 0;
  //READ IN NAME FOR DOMAIN
  data.Read(i, 4); //Length of Domain's name
  setlength(BufferString, i);
  data.Read(BufferString[1], i); //Read in Domain's name
  FName := BufferString;

  BufferString := '';  //Clear the Buffer string in case the gene (read next) does not exist

  //READ IN GENE FOR DOMAIN
  data.Read(i, 4);  //How long the Gene Name is for this Domain
  if i <> 0 then //No Gene name
  begin
    SetLength(BufferString, i);
    data.Read(BufferString[1], i);
  end;
  FGeneName := BufferString;

  //IS THIS A GENE? IF NOT, THEN IT IS A DOMAIN
  data.Read(i, 1);
  FIsGene := (i = 1);

  //IS THIS CODING?
  data.Read(i, 1);
  FIsCoding := (i = 1);

  //WHAT IS THE CODON START?
  data.Read(i, 4);
  FCodonStart := i;

  //WHAT SITE DOES THIS DOMAIN START AT
  data.Read(i, 4);
  FFromSite := i;

  //WHAT SITE DOES THIS DOMAIN END AT
  data.Read(i, 4);
  FToSite := i;

  //IS THIS EVEN USED? (GENES/DOMAINS CAN BE CHECKED AND UNCHECKED)
  data.Read(i, 1);
  FIsUsed := (i = 1);
end;

procedure TAllSiteDomainMark.WriteToFile(data: TStream);
var
  i, j, s : longint;
  buffer : array of AnsiChar;
  begin
    s := NoOfDomains;

    data.write(s, 4);//How many sequences
    for j:=0 to NoOfSites-1 do
    begin
      if (FArray[j] <> nil) AND ((j = 0) OR (FArray[j] <> FArray[(j-1)])) then
      FArray[j].WriteToFile(data);
    end;

    i := length(FSiteMarks);
    data.write(i, 4); //How long FSiteMarks is
    setlength(buffer, i+1);
    StrPCopy(PAnsiChar(buffer),FSiteMarks);
    data.write(buffer[0], i);
//    ShowMessage('right after site marks ' + IntToStr(filepos(data)));
    data.write(FIsDirty, 1);
    data.write(FIsUsedIndependent, 1);
    data.write(FNoOfMarkedSites, 4);
    i := Length(FSiteMarkTypes.CommaText);
    data.write(i, 4);
    if i <> 0 then
    begin
      setlength(buffer, i+1);
      StrPCopy(PAnsiChar(buffer), PAnsiChar(FSiteMarkTypes.CommaText));
      data.write(buffer[0], i);
    end;
end;

procedure TAllSiteDomainMark.Assign(data: TAllSiteDomainMark);
var
  i, j: longint;
begin
    SetNoOfSites(data.NoOfSites);
    for j:=0 to Data.NoOfSites-1 do
      FArray[j].assign(data.FArray[j]);
    i := length(Data.FSiteMarks);
    FSiteMarks := GetMemory(i*sizeof(AnsiChar));
    StrPCopy(PAnsiChar(FSiteMarks),data.FSiteMarks);
    FIsDirty := data.FIsDirty;
    FIsUsedIndependent := data.FIsUsedIndependent;
    FNoOfMarkedSites := data.FNoOfMarkedSites;
    FSiteMarkTypes.CommaText := data.FSiteMarkTypes.CommaText;
end;

procedure TAllSiteDomainMark.RestoreFromFile(var data : TFileStream);
var
  i : longint;
begin
  for i:=0 to NoOfSites-1 do
    FArray[i].RestoreFromFile(data);
end;

function TAllSiteDomainMark.GetNoOfDomains: LongInt;
var
  s, j : integer;
begin
  s := 0;
  for j:=0 to NoOfSites-1 do
  begin
    if (FArray[j] <> nil) AND ((j = 0) OR (FArray[j] <> FArray[(j-1)])) then
      s := s + 1;
  end;
  Result := s;
end;

function TAllSiteDomainMark.GetNoOfIncludedDomains: LongInt;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to NoOfSites - 1 do
  begin
    if (i = 0) or (Assigned(FArray[i]) and FArray[i].IsUsed and (FArray[i] <> FArray[i - 1])) then
      Result := Result + 1;
  end;
end;

function TAllSiteDomainMark.SiteLabelsString: AnsiString;
var
  i: Integer;
begin
  SetLength(Result, FNoOfSites);
  if Length(Result) > 0 then
  begin
    for i := 1 to Length(Result) do
      Result[i] := ' ';
    for i := 0 to FNoOfSites - 1 do
      if SiteLabel[i] <> ' ' then
        Result[i + 1] := SiteLabel[i];
  end;
end;

procedure TDomainInfo.Assign(data: TDomainInfo);
begin
    FName := data.FName;
    FGeneName := data.FGeneName;
    FIsGene := data.FIsGene;
    FIsCoding := data.FIsCoding;
    FCodonStart := data.FCodonStart;
    FFromSite := data.FFromSite;
    FToSite := data.FToSite;
    FIsUsed := data.FIsUsed;
end;

procedure TDomainInfo.UpdateGeneBoundaries;
var
  i: Integer;
  aInfo: TDomainInfo;
begin
  if not IsGene then
    Exit;
  if ChildDomains.Count > 0 then
    for i := 0 to ChildDomains.Count - 1 do
    begin
      aInfo := TDomainInfo(ChildDomains[i]);
      if (FromSite <= aInfo.fromSite) and (ToSite >=  aInfo.ToSite) then
        continue;
      if FromSite > aInfo.FromSite then
        FromSite := aInfo.FromSite;
      if ToSite < aInfo.ToSite then
        ToSite := aInfo.ToSite;
    end;
end;

function TDomainInfo.CheckBoxState: TMegaCheckBoxState;
var
  i: Integer;
  numUsed: Integer;
begin
  Result := cbsChecked;
  if not IsUsed then
  begin
    Result := cbsUnchecked;
    Exit;
  end;
  if IsUsed and (ChildDomains.Count = 0) then
    Exit;
  if ChildDomains.Count > 0 then
  begin
    numUsed := 0;
    for i := 0 to ChildDomains.Count - 1 do
    begin
      if TDomainInfo(ChildDomains[i]).IsUsed then
      begin
        inc(numUsed);
      end;
    end;
    if numUsed = 0 then
      Result := cbsUnchecked
    else if numUsed = ChildDomains.Count then
      Result := cbsChecked
    else
      Result := cbsPartiallyChecked;
  end;
end;

function TDomainInfo.IsChild(aInfo: TDomainInfo): Boolean;
begin
  Result := (ChildDomains.IndexOf(aInfo) >= 0);
end;

function TDomainInfo.IsSibling(aInfo: TDomainInfo): Boolean;
begin
  Result := False;
  if ParentDomain <> nil then
  begin
    Result := ParentDomain.IsChild(aInfo);
  end;
end;

function TDomainInfo.IsParent(aInfo: TDomainInfo): Boolean;
begin
  Result := (aInfo = ParentDomain);
end;

end.
