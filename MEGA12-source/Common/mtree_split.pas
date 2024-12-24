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

unit mtree_split;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MLongintList, MegaConsts;

type

  { TTreeSplit }

  TTreeSplit = class(TObject)
    private
      FAncestor: Integer;
      FClade1: TLongintList;
      FClade2: TLongintList;
      FBranchIndex: Integer;
      FIsDes1: Boolean;
      FIsDes2: Boolean;
      FIsOtu: Boolean;

    public
      constructor Create;
      destructor Destroy; override;
      procedure AddToClade1(taxonId: Integer);
      procedure AddToClade2(taxonId: Integer);
      function Matches(partitionString: String): Boolean;
      function DebugStrings: String;
      class function DebugHeaderString: String; static;
      property Ancestor: Integer read FAncestor write FAncestor;
      property BranchIndex: Integer read FBranchIndex write FBranchIndex;
      property IsDes1: Boolean read FIsDes1 write FIsDes1;
      property IsDes2: Boolean read FIsDes2 write FIsDes2;
      property IsOtu: Boolean read FIsOtu write FIsOtu;
  end;

  TTreeSplitArray = array of TTreeSplit;

implementation

{ TTreeSplit }

constructor TTreeSplit.Create;
begin
  FClade1 := TLongIntList.Create;
  FClade2 := TLongIntList.Create;
  FAncestor := -1;
  FBranchIndex := -1;
  FIsDes1 := False;
  FIsDes2 := False;
  FIsOtu := False;
end;

destructor TTreeSplit.Destroy;
begin
  if Assigned(FClade1) then
    FClade1.Free;
  if Assigned(FClade2) then
    FClade2.Free;
  inherited Destroy;
end;

procedure TTreeSplit.AddToClade1(taxonId: Integer);
begin
  FClade1.Add(taxonId);
end;

procedure TTreeSplit.AddToClade2(taxonId: Integer);
begin
  FClade2.Add(taxonId);
end;

function TTreeSplit.Matches(partitionString: String): Boolean;
var
  c1: TLongintList = nil;
  c2: TLongintList = nil;
  i: Integer;
begin
  Result := False;
  if Length(partitionString) = 0 then
    raise Exception.Create('invalid call to TTreeSplit.Matches - cannot use an empty string');
  try
    c1 := TLongintList.Create;
    c2 := TLongintList.Create;
    for i := 1 to Length(partitionString) do
      if partitionString[i] = '1' then
        c1.Add(i - 1)
      else if partitionString[i] = '0' then
        c2.Add(i - 1)
      else
        raise Exception.Create('invalid partition string - ' + partitionString);
    if (c1.Count <> FClade1.Count) and (c1.Count <> FClade2.Count) then
      Result := False
    else
      begin
        if FClade1.HasSameValues(c1) and FClade2.HasSameValues(c2) then
          Result := True
        else if FClade1.HasSameValues(c2) and FClade2.HasSameValues(c1) then
          Result := True;
      end;
  finally
    if Assigned(c1) then
      c1.Free;
    if Assigned(c2) then
      c2.Free;
  end;
end;

function TTreeSplit.DebugStrings: String;
begin
  Result := Format('%8d %8s %10d %10s %10s %s %s', [FBranchIndex, BoolToStr(IsOtu, True), FAncestor, BoolToStr(IsDes1, True), BoolToStr(IsDes2, True), 'clade1: ' + FClade1.ToCsvString, 'clade2: ' + FClade2.ToCsvString]);
end;

class function TTreeSplit.DebugHeaderString: String;
begin
  Result := Format('%8s %8s %10s %10s %10s %s', ['branch', 'isOtu', 'ancestor', 'isDes1', 'isDes2', 'data']);
end;

end.

