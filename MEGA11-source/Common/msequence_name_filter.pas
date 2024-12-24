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

unit msequence_name_filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MD_Sequences;

type

  { TSequenceNameFilter }

  TSequenceNameFilter = class(TObject)
    private
      FMaxNameLength: Integer;
      FNumSeqs: Integer;
      FSequences: TSequenceList;
      function CheckSeqName(const aName: String; const startFrom: Integer): Integer;
      function CheckMEGASeqName(AName: String): Integer;
    public
      constructor Create(seqs: TSequenceList; maxLength: Integer);
      destructor Destroy; override;

      procedure FilterName(var seq: TSequence; const index: Integer);
  end;

implementation

uses
  MegaConsts;

{ TSequenceNameFilter }

function TSequenceNameFilter.CheckSeqName(const aName: AnsiString; const startFrom: Integer): Integer;
var
  i: integer;
begin
  Result := -1;
  if startFrom >= FNumSeqs then
    Exit;
  for i := startFrom to FNumSeqs - 1 do
    if aName = FSequences[i].SeqName then
    begin
      Result := i;
      Exit;
    end;
end;

function TSequenceNameFilter.CheckMEGASeqName(AName: AnsiString): integer;
var
  i: integer;
begin
  result := 0;
  if Length(AName) > 0 then
    if not (AName[1] in ValidOtuNameStartSet) then
      result := 1
    else
      for i := 2 to length(Aname) do
        if not ((AName[i]=' ') or (AName[i] in ValidOtuNameContinueSet)) then
        begin
          result := i;
          break;
        end;
  if result = 0 then
    if Length(AName) > FMaxNameLength then
      result := FMaxNameLength;
end;

constructor TSequenceNameFilter.Create(seqs: TSequenceList; maxLength: Integer);
begin
  FSequences := seqs;
  FNumSeqs := seqs.Count;
  FMaxNameLength := maxLength;
end;

destructor TSequenceNameFilter.Destroy;
begin
  inherited Destroy;
end;

procedure TSequenceNameFilter.FilterName(var seq: TSequence; const index: Integer);
var
 i: integer;
 str: String;
begin
  if seq.SeqName = '' then
  begin
    i := 0;
    repeat
      inc(i);
      str := 'Sequence '+IntToStr(i);
    until CheckSeqName(str, index + 1) = -1;
    seq.SeqName := str;
    exit;
  end;

  i := CheckMEGASeqName(seq.SeqName);
  while i > 0 do
  begin
    if i = FMaxNameLength + 1 then
      seq.SeqName := system.copy(seq.SeqName, 1, FMaxNameLength - 3) + '...'
    else
      System.Delete(seq.FSeqName, i, 1);
    i := CheckMEGASeqName(seq.SeqName);
  end;
  if CheckSeqName(seq.SeqName, index + 1) > -1 then
  begin
    i := 1;
    repeat
      inc(i);
      str := '(' + IntToStr(i) + ')';
    until CheckSeqName(system.copy(seq.SeqName, 1, FMaxNameLength - length(str)) + str, index + 1) = -1;
    seq.SeqName := system.copy(seq.SeqName, 1, FMaxNameLength - length(str)) + str;
  end;
end;

end.

