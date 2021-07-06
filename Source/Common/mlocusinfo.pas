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

unit MLocusInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MegaConsts, Classes;

type
  TLocusInfo = Class
  private
    FName:        AnsiString;       // LocusName
    FAlleleNames: TStringList;  // Allele
    FIsUsed:      Boolean;      // if selected

    function  GetNoOfAlleles: Integer;
    function  GetAlleleName(Index: Integer): AnsiString;
    function  GetInfoString: AnsiString;
    function  GetHasName(Value: AnsiString): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CopyFrom(const Value: TLocusInfo);
    procedure AddAlleleName(Value: AnsiString);

    property Name: AnsiString read FName write FName;
    property IsUsed: Boolean read FIsUsed write FIsUsed;

    property NoOfAlleles: Integer read GetNoOfAlleles;
    property AlleleName[Index: Integer]: AnsiString read GetAlleleName;
    property HasAlleleName[Value: AnsiString]: Boolean read GetHasName;

    property InfoString: AnsiString read GetInfoString;
  end;

  TArrayOfTLocusInfo = array[Word] of TLocusInfo;
  PArrayOfTLocusInfo = ^TArrayOfTLocusInfo;

  // store locus pop frequencies
  TLocusPopFreqInfo = class
  public
    NoOfAlleles    : Integer;         // Number of alleles
    NoOfIndv       : Double;          // number of individuals/genes samples
    AlleleFreq     : PArrayOfDouble;  // Allele Frequencies
    GenotypeFreq   : PDistanceMatrix; // Genotype Frequencies
    HasMissingData : Boolean;         // if there are any missing freq data
    PValueStrings: TArrayOfString;
    constructor Create;
    destructor  Destroy; override;
  end;


implementation

uses
  SysUtils, MegaUtils;

//---------TLocusPopFreqInfo------
constructor TLocusPopFreqInfo.Create;
begin
  AlleleFreq   := nil;
  GenotypeFreq := nil;
end;

destructor TLocusPopFreqInfo.Destroy;
begin
  if Assigned(AlleleFreq) then
  begin
    FreeMem(AlleleFreq, NoOfAlleles*SizeOf(Double));
    AlleleFreq := nil;
  end;
  if Assigned(GenotypeFreq) then
    FreeDistMatrix(GenotypeFreq, NoOfAlleles);

  AlleleFreq   := nil;
  GenotypeFreq := nil;
  inherited Destroy;
end;

//--- TDomainInfo ----
Constructor TLocusInfo.Create;
begin
  FName       := EmptyStr;
  FIsUsed     := True;
  FAlleleNames := nil;
end;

destructor TLocusInfo.Destroy;
begin
  if FAlleleNames <> nil then
    FAlleleNames.Free;
end;

//--- No of Sites
function TLocusInfo.GetNoOfAlleles: Integer;
begin
  Result := 0;
  if FAlleleNames <> nil then
    Result := FAlleleNames.Count;
end;

procedure TLocusInfo.AddAlleleName(Value: AnsiString);
begin
  if FAlleleNames = nil then
    FAlleleNames := TStringList.Create;
  FAlleleNames.Add(Value);
end;

function TLocusInfo.GetAlleleName(Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if FAlleleNames <> nil then
    Result := FAlleleNames[Index];
end;

function  TLocusInfo.GetHasName(Value: AnsiString): Boolean;
begin
  Result := False;
  if FAlleleNames <> nil then
    Result := (FAlleleNames.IndexOf(Value) <> -1);
end;

procedure TLocusInfo.CopyFrom(const Value: TLocusInfo);  // copies perfectly
var
  i: Integer;
begin
  FName       := Value.Name;
  FIsUsed     := Value.IsUsed;
  if FAlleleNames <> nil then
    FAlleleNames := TStringList.Create
  else
    FAlleleNames.Clear;
  for i:=0 to Value.NoOfAlleles-1 do
    FAlleleNames.Add(Value.AlleleName[i]);
end;

function TLocusInfo.GetInfoString: AnsiString;
begin
  Result := 'Locus: '+FName;
  Result := Result + ' with '+ IntToStr(NoOfAlleles)+' alleles';
end;

end.
