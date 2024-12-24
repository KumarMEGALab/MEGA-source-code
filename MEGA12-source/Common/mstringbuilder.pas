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

unit mstringbuilder;

{$mode objfpc}{$H+}

interface

uses
  MegaConsts;

const
  StringBuilderMemoryBlockLength = 4096;

type
  PStringBuilderMemoryBlock = ^TStringBuilderMemoryBlock;
  TStringBuilderMemoryBlock = record
    Data: array[0..StringBuilderMemoryBlockLength - 1] of byte;
    Count: Cardinal;
    Next: PStringBuilderMemoryBlock;
  end;

  { TMegaStringBuilder }

  TMegaStringBuilder = class
  protected
    FHead, FTail: PStringBuilderMemoryBlock;
    FTotalLength: Cardinal;
    function Min(const a, b: Cardinal): Cardinal;
  public
    property Head: PStringBuilderMemoryBlock read FHead;
    property Tail: PStringBuilderMemoryBlock read FTail;
    property TotalLength: Cardinal read FTotalLength;
    constructor Create;
    procedure Add(const aString: string);
    procedure Add(const aStrings: array of string);
    function GenerateString: string;
    procedure Clean;
    destructor Destroy; override;
  end;

  function ArrayOfExtToDelimitedStr(vals: TArrayOfExt; delimiter: String = ','): String;

implementation

uses
  sysutils;

function CreateNewBlock: PStringBuilderMemoryBlock;
begin
  New(result);
  result^.Count := 0;
  result^.Next := nil;
end;

function ArrayOfExtToDelimitedStr(vals: TArrayOfExt; delimiter: String = ','): String;
var
  b: TMegaStringBuilder = nil;
  i: Integer = -1;
begin
  Result := EmptyStr;
  if Length(vals) = 0 then
    Exit;
  try
    b := TMegaStringBuilder.Create;
    for i := Low(vals) to High(vals) do
    begin
      b.Add(FloatToStr(vals[i]));
      if i <> High(vals) then
        b.Add(delimiter);
    end;
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

{ TMegaStringBuilder }

function TMegaStringBuilder.Min(const a, b: Cardinal): Cardinal;
begin
  if
    a < b
  then
    result := a
  else
    result := b;
end;

constructor TMegaStringBuilder.Create;
begin
  FHead := nil;
  FTail := nil;
  FTotalLength := 0;
end;

procedure TMegaStringBuilder.Add(const aString: string);
var
  bytesLeftInString, bytesToWriteInCurrentBlock, bytesLeftInTailBlock: Cardinal;
  positionInString: PChar;
begin
  if aString = EmptyStr then
    Exit;
  if nil = Head then
  begin
    FHead := CreateNewBlock;
    FTail := Head;
  end;
  bytesLeftInString := Length(aString);
  Inc(FTotalLength, bytesLeftInString);

  positionInString := PChar(aString);
  while bytesLeftInString > 0 do
  begin
    Assert((StringBuilderMemoryBlockLength - Tail^.Count) >= 0, 'bad bytesLeftInTailBlock');
    bytesLeftInTailBlock := StringBuilderMemoryBlockLength - Tail^.Count;
    if bytesLeftInTailBlock = 0 then
    begin
      Tail^.Next := CreateNewBlock;
      FTail := Tail^.Next;
      bytesLeftInTailBlock := StringBuilderMemoryBlockLength;
    end;
    bytesToWriteInCurrentBlock := Min(bytesLeftInString, bytesLeftInTailBlock);
    Assert(bytesToWriteInCurrentBlock <= (StringBuilderMemoryBlockLength - Tail^.Count));
    Assert(bytesToWriteInCurrentBlock > 0);
    Move(positionInString^, Tail^.Data[Tail^.Count], bytesToWriteInCurrentBlock);
    Inc(Tail^.Count, bytesToWriteInCurrentBlock);
    Dec(bytesLeftInString, bytesToWriteInCurrentBlock);
    Inc(positionInString, bytesToWriteInCurrentBlock);
    if bytesLeftInString > 0 then
    begin
      Tail^.Next := CreateNewBlock;
      FTail := Tail^.Next;
    end;
  end;
end;

procedure TMegaStringBuilder.Add(const aStrings: array of string);
var
  i: Cardinal;
begin
  for i := 0 to Length(aStrings) - 1 do
    Add(aStrings[i]);
end;

function TMegaStringBuilder.GenerateString: string;
var
  currentBlock: PStringBuilderMemoryBlock;
  currentCount: Cardinal;
  currentResultPosition: PChar;
begin
  if Head <> nil
  then
  begin
    SetLength(result, Totallength);
    currentResultPosition := PChar(result);
    currentBlock := Head;
    while currentBlock <> nil do
    begin
      currentCount := currentBlock^.Count;
      Move(currentBlock^.Data[0], currentResultPosition^, currentCount);
      Inc(currentResultPosition, currentCount);
      currentBlock := currentBlock^.Next;
    end;
    currentResultPosition^ := #0;
  end
  else
    result := '';
end;

procedure TMegaStringBuilder.Clean;
var
  current, next: PStringBuilderMemoryBlock;
begin
  current := Head;
  while
    current <> nil
  do
  begin
    next := current^.Next;
    Dispose(current);
    current := next;
  end;
  FHead := nil;
  FTail := nil;
  FTotalLength := 0;
end;

destructor TMegaStringBuilder.Destroy;
begin
  Clean;
  inherited Destroy;
end;

end.

