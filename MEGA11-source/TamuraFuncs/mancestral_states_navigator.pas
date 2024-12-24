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

unit mancestral_states_navigator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, MegaConsts, MLTreeAnalyzer, MLTree, MComputeParsimInfo;

type
  IAncestralStatesNavigation = interface
    ['{AAF0EE8B-B833-4AC6-8028-8C8348CB6940}']
    function FindNextChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64;
    function FindPreviousChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64;
  end;

  TAbstractAncestralStatesNavigation = class abstract (TObject, IAncestralStatesNavigation)
    public
    function FindNextChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64; virtual; abstract;
    function FindPreviousChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64; virtual; abstract;
  end;

  { TMPAncestralStatesNavigation }

  TMPAncestralStatesNavigation = class(TAbstractAncestralStatesNavigation)
    protected
      FUseExtendedChars: Boolean;
      FParsimInfo: TComputeParsimInfo;
      function GetAncestralState(node, site: Int64; var state: AnsiString): Boolean;
      function NumNodes: Int64;
    public
      constructor Create(parsimInfo: TComputeParsimInfo; useExtChars: Boolean);
      destructor Destroy; override;
      function FindNextChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64; override;
      function FindPreviousChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64; override;
  end;

  { TMLAncestralStatesNavigation }

  TMLAncestralStatesNavigation = class(TAbstractAncestralStatesNavigation)
    protected
      FAncStates: TAncStateRecArray;
      FDesStates: TAncStateRecArray;
      MLAnalyzer: TMLTreeAnalyzer;
      function GetAncestralStates(node, site: Int64; states: TAncStateRecArray): Boolean;
      function NumSites: Int64;
    public
      constructor Create(analyzer: TMLTreeAnalyzer);
      destructor Destroy; override;
      function FindNextChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64; override;
      function FindPreviousChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64; override;
  end;

implementation

uses
  MegaUtils;

{ TMPAncestralStatesNavigation }

function TMPAncestralStatesNavigation.GetAncestralState(node, site: Int64; var state: AnsiString): Boolean;
var
  aStates: PArrayOfLongint;
begin
  Result := False;
  try
    aStates := AllocMem(SizeOf(LongInt)*NumNodes);
    FParsimInfo.ComputeSiteMPAncStates(site, aStates^);
    if FParsimInfo.IsNucData then
    begin
      if FUseExtendedChars then
        state := ParsimMapToNuc(AnsiChar(Chr(aStates[node])))
      else
        state := ParsimMapToNucStr(aStates[node]);
    end
    else
      state := ParsimMapToAminoStr(aStates[node]);
    Result := (state <> EmptyStr);
  finally
    FreeMemAndNil(aStates);
  end;
end;

function TMPAncestralStatesNavigation.NumNodes: Int64;
begin
  Result := 0;
  if Assigned(FParsimInfo) and (FParsimInfo.NoOfSites > 0) then
    Result := 2*FParsimInfo.NoOfSites - 2;
end;

constructor TMPAncestralStatesNavigation.Create(parsimInfo: TComputeParsimInfo; useExtChars: Boolean);
begin
  FParsimInfo := parsimInfo;
  FUseExtendedChars := useExtChars;
end;

destructor TMPAncestralStatesNavigation.Destroy;
begin
  FParsimInfo := nil;
  inherited Destroy;
end;

function TMPAncestralStatesNavigation.FindNextChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64;
var
  site: Integer;
  ancState: AnsiString = '';
  desState: AnsiString = '';
begin
  Result := -1;
  if startSite < FParsimInfo.NoOfSites then
    for site := startSite to FParsimInfo.NoOfSites - 1 do
    begin
      GetAncestralState(desNode, site, ancState);
      GetAncestralState(ancNode, site, desState);
      if desState <> ancState then
      begin
        Result := site + 1;
        Exit;
      end;
    end;
end;

function TMPAncestralStatesNavigation.FindPreviousChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64;
var
  site: Integer;
  desState: AnsiString = '';
  ancState: AnsiString = '';
begin
  Result := -1;
  if (startSite <= FParsimInfo.NoOfSites) and (startSite > 2) then
    for site := startSite - 2 downto 0 do
    begin
      GetAncestralState(desNode, site, desState);
      GetAncestralState(ancNode, site, ancState);
      if desState <> ancState then
      begin
        Result := site + 1;
        Exit;
      end;
    end;
end;

{ TMLAncestralStatesNavigation }

function TMLAncestralStatesNavigation.GetAncestralStates(node, site: Int64; states: TAncStateRecArray): Boolean;
begin
  Result := MLAnalyzer.GetAncStateProb(node, site, states);
end;

function TMLAncestralStatesNavigation.NumSites: Int64;
begin
  if Assigned(MLAnalyzer) then
    Result := MLAnalyzer.NoOfSites
  else
    Result := 0;
end;

constructor TMLAncestralStatesNavigation.Create(analyzer: TMLTreeAnalyzer);
begin
  MLAnalyzer := analyzer;
  if MLAnalyzer.Model.SeqDataType = DNA then
  begin
    SetLength(FAncStates, 4);
    SetLength(FDesStates, 4);
  end
  else
  begin
    SetLength(FDesStates, 20);
    SetLength(FAncStates, 20);
  end;
end;

destructor TMLAncestralStatesNavigation.Destroy;
begin
  MLAnalyzer := nil;
  SetLength(FDesStates, 0);
  SetLength(FAncStates, 0);
  inherited Destroy;
end;

function TMLAncestralStatesNavigation.FindNextChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64;
var
  site: Integer;
begin
  Result := -1;
  if startSite < NumSites then
    for site := startSite + 1 to NumSites do
    begin
      GetAncestralStates(desNode, site, FDesStates);
      GetAncestralStates(ancNode, site, FAncStates);
      if FDesStates[0].Name <> FAncStates[0].Name then
      begin
        Result := site;
        Exit;
      end;
    end;
end;

function TMLAncestralStatesNavigation.FindPreviousChange(desNode: Int64; ancNode: Int64; startSite: Int64): Int64;
var
  site: Integer;
begin
  Result := -1;
  if (startSite <= NumSites) and (startSite > 1) then
    for site := startSite - 1 downto 1 do
    begin
      GetAncestralStates(desNode, site, FDesStates);
      GetAncestralStates(ancNode, site, FAncStates);
      if FDesStates[0].Name <> FAncStates[0].Name then
      begin
        Result := site;
        Exit;
      end;
    end;
end;

end.

