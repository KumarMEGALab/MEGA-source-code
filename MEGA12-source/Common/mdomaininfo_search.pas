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

unit mdomaininfo_search;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mdomaininfo;

type

  { TDomainInfoSearch }

  TDomainInfoSearch = class
  private
    FCurrentMatchNumber: Integer;
    FList: TList;
    FNumMatches: Integer;
    FSearchQuery: string;
    FCurrentIndex: Integer;
    function GetDomainInfo(Index: Integer): TDomainInfo;
    function DomainNameIsMatch(const DomainInfo: TDomainInfo): Boolean;
    procedure FindNumMatches;
  public
    constructor Create(aList: TList; aQuery: String);
    destructor Destroy; override;
    procedure SetSearchQuery(const Query: string);
    function FindFirst: TDomainInfo;
    function FindNext: TDomainInfo;
    function FindPrevious: TDomainInfo;
    function IsMatch(Index: Integer): Boolean;
    function FindLast: TDomainInfo;

    property CurrentMatchNumber: Integer read FCurrentMatchNumber; { which match among matching domains is currently focused}
    property CurrentMatchRow: Integer read FCurrentIndex; { where the match is in the list of all domains, matching or not}
    property NumMatches: Integer read FNumMatches;
  end;

implementation

constructor TDomainInfoSearch.Create(aList: TList; aQuery: String);
begin
  FList := TList.Create;
  FList.AddList(aList);
  FCurrentIndex := -1; // Initialize index to -1 to indicate no current item
  FCurrentMatchNumber := -1;
  FSearchQuery := LowerCase(aQuery);
  FindNumMatches;
end;

destructor TDomainInfoSearch.Destroy;
begin
  if Assigned(FList) then
  begin
    FList.Clear;
    FList.Free;
  end;
  inherited Destroy;
end;

procedure TDomainInfoSearch.SetSearchQuery(const Query: string);
begin
  FSearchQuery := LowerCase(Query);
  FCurrentIndex := -1; // Reset current index when search query changes
end;

function TDomainInfoSearch.GetDomainInfo(Index: Integer): TDomainInfo;
begin
  Result := TDomainInfo(FList[Index]);
end;

function TDomainInfoSearch.DomainNameIsMatch(const DomainInfo: TDomainInfo): Boolean;
begin
  Result := Pos(FSearchQuery, LowerCase(DomainInfo.GeneName)) > 0;
  if not Result then
    Result := Pos(FSearchQuery, LowerCase(DomainInfo.Name)) > 0;
end;

procedure TDomainInfoSearch.FindNumMatches;
var
  i: Integer = 0;
begin
  FNumMatches := 0;
  if FList.Count > 0 then
    for i := 0 to FList.Count - 1 do
      if DomainNameIsMatch(GetDomainInfo(i)) then
        inc(FNumMatches);
end;

function TDomainInfoSearch.FindFirst: TDomainInfo;
begin
  FCurrentIndex := -1; // Start search from the beginning
  Result := FindNext; // Use FindNext to perform the actual search
  if Assigned(Result) then
    FCurrentMatchNumber := 1
  else
    FCurrentMatchNumber := 0;
end;

function TDomainInfoSearch.FindNext: TDomainInfo;
begin
  Result := nil;
  Inc(FCurrentIndex);
  while FCurrentIndex < FList.Count do
  begin
    if DomainNameIsMatch(GetDomainInfo(FCurrentIndex)) then
    begin
      Result := GetDomainInfo(FCurrentIndex);
      inc(FCurrentMatchNumber);
      Exit;
    end;
    Inc(FCurrentIndex); // Move to the next index
  end;
  FCurrentMatchNumber := 0;
  FCurrentIndex := FList.Count; // No more items to search
end;

function TDomainInfoSearch.FindPrevious: TDomainInfo;
begin
  Result := nil;
  Dec(FCurrentIndex);
  while FCurrentIndex >= 0 do
  begin
    if DomainNameIsMatch(GetDomainInfo(FCurrentIndex)) then
    begin
      Result := GetDomainInfo(FCurrentIndex);
      if FCurrentMatchNumber > 0 then
        dec(FCurrentMatchNumber)
      else
        FCurrentMatchNumber := FNumMatches;
      Exit;
    end;
    Dec(FCurrentIndex); // Move to the previous index
  end;
  FCurrentMatchNumber := 0;
  FCurrentIndex := -1; // No more items to search
end;

function TDomainInfoSearch.IsMatch(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := DomainNameIsMatch(GetDomainInfo(Index))
  else
    Result := False;
end;

function TDomainInfoSearch.FindLast: TDomainInfo;
begin
  Result := nil;
  FCurrentIndex := FList.Count; // Start search from the end
  Dec(FCurrentIndex);
  while FCurrentIndex >= 0 do
  begin
    if DomainNameIsMatch(GetDomainInfo(FCurrentIndex)) then
    begin
      Result := GetDomainInfo(FCurrentIndex);
      FCurrentMatchNumber := FNumMatches;
      Exit;
    end;
    Dec(FCurrentIndex); // Move to the previous index
  end;
  FCurrentMatchNumber := 0;
  FCurrentIndex := -1; // No more items to search
end;



end.

