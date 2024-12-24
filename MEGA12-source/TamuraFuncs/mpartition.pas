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

unit MPartition;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils;

type

  EBitsError = class(Exception);

  { TPartition }

  TPartition= class
  private
    FSize: Integer;
    FBits: Pointer;
    FFreq: Double;
    FBLen: double;
    FPartitionString: String;

    procedure Error;
    procedure SetSize(Value: Integer);
    procedure SetBits(Index: Integer; Value: Boolean);
    function GetBits(Index: Integer): Boolean;

    function GetNoOfBits:integer;

  public
    constructor Create;
    destructor Destroy; override;

    property Bits[Index : integer]: boolean read GetBits write SetBits; default;
    property Size: Integer read FSize write SetSize;
    property NoOfBits: Integer read GetNoOfBits;
    property Freq: Double read FFreq write FFreq;      // Frequency of a partition
    property BLen: double read FBLen write FBLen;      //  Average branch length

    procedure Assign(Source: TPartition);     // copies Value into itself
    function Clone: TPartition;
    procedure Add(Value: TPartition);        // does OR product
    procedure Complement;                    // Complements
    procedure Subtract(Value: TPartition);   // removes common bits
    procedure Common(Value: TPartition);     // does AND product
    procedure Difference(Value: TPartition); // does XOR product
    procedure AddFreq(Value: Double);          // increments frequency
    procedure Reset;                         // clear away the whole thing
    function  IsIdentical(Value: TPartition): Boolean; // returns false if any difference
    procedure Anchor(Index: Integer);
    function DescString(simple: Boolean): String;
    function GetPartitionString: String; { returns a string of zeros and ones where all zeros indicate sequences in one clade of partition and ones the sequences in the other clade}
  end;

implementation

uses
  TypInfo, MGlobalSettings;

const
  BitsPerInt = SizeOf(Integer) * 8;

constructor TPartition.Create;
begin
  Inherited Create;
  FFreq := 0.0;
  FPartitionString := EmptyStr;
end;

destructor TPartition.Destroy;
begin
  SetSize(0);
  inherited Destroy;
end;

procedure TPartition.Error;
begin
//  raise EBitsError.CreateRes(SBitsIndexError);
end;

procedure TPartition.SetSize(Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    if FPartitionString <> EmptyStr then
      FPartitionString := EmptyStr;
    if Value < 0 then Error;
    NewMemSize := ((Value + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    OldMemSize := ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMemAndNil(FBits, OldMemSize);
      end;
      FBits := NewMem;
    end;
    FSize := Value;
  end;
end;

procedure TPartition.SetBits(Index: Integer; Value: Boolean);
var
  i,j,k: integer;
  p: ^Integer;
begin
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
  i := Index div BitsPerInt;
  j := Index mod BitsPerInt;
  k := 1;
  k := k SHL j;
  p := FBits;
  inc(p, i);
  if value then
    p^ := p^ OR k
  else
  begin
    k := NOT k;
    p^ := p^ AND k;
  end;
end;

function TPartition.GetBits(Index: Integer): Boolean;
var
  i,j,k: integer;
  p: ^Integer;
begin
  i := Index div BitsPerInt;
  j := Index mod BitsPerInt;
  k := 1;
  k := k SHL j;
  p := FBits;
  inc(p, i);
  result := p^ and k = k;
end;


//procedure TPartition.SetBits(Index: Integer; Value: Boolean); assembler;
//asm
//
//{$IFDEF WIN64}
//{ TODO 1 -oglen -c64bit : update assembly code for 64 bit compatibility }
//        CMP     Index,[EAX].FSize
//        JAE     @@Size
//
//@@1:    MOV     EAX,[EAX].FBits
//        OR      Value,Value
//        JZ      @@2
//        BTS     [EAX],Index
//        RET
//
//@@2:    BTR     [EAX],Index
//        RET
//
//@@Size: CMP     Index,0
//        JL      TPartition.Error
//        PUSH    Self
//        PUSH    Index
//        PUSH    ECX {Value}
//        INC     Index
//        CALL    TPartition.SetSize
//        POP     ECX {Value}
//        POP     Index
//        POP     Self
//        JMP     @@1
//{$ELSE}
//        CMP     Index,[EAX].FSize
//        JAE     @@Size
//
//@@1:    MOV     EAX,[EAX].FBits
//        OR      Value,Value
//        JZ      @@2
//        BTS     [EAX],Index
//        RET
//
//@@2:    BTR     [EAX],Index
//        RET
//
//@@Size: CMP     Index,0
//        JL      TPartition.Error
//        PUSH    Self
//        PUSH    Index
//        PUSH    ECX {Value}
//        INC     Index
//        CALL    TPartition.SetSize
//        POP     ECX {Value}
//        POP     Index
//        POP     Self
//        JMP     @@1
//{$ENDIF}
//end;

//function TPartition.GetBits(Index: Integer): Boolean; assembler;
//asm
//{$IFDEF WIN64}
{ TODO 1 -oglen -c64bit : update assembly code for 64 bit compatibility }
//        CMP     Index,[EAX].FSize
//        JAE     TPartition.Error
//        MOV     EAX,[EAX].FBits
//        BT      [EAX],Index
//        SBB     EAX,EAX
//        AND     EAX,1
//{$ELSE}
//        CMP     Index,[EAX].FSize
//        JAE     TPartition.Error
//        MOV     EAX,[EAX].FBits
//        BT      [EAX],Index
//        SBB     EAX,EAX
//        AND     EAX,1
//{$ENDIF}
//end;

procedure TPartition.Reset;
var
  i: integer;
begin
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
  FFreq := 0.0;
  i := ((Size+BitsPerInt-1) div BitsPerInt)*SizeOf(Integer);
  FillChar(FBits^, i, 0);
end;

procedure TPartition.Complement;
var
  i: Integer;
  p: ^Integer;
begin
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
  p := FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    p^ := not p^;
    Inc(p);
  end;
  i := (Size mod BitsPerInt);
  if i > 0 then begin
    Dec(p);
    p^ :=  p^ and (MaxInt shr (BitsPerInt-i-1));
  end;
end;

procedure TPartition.Anchor(Index: Integer);
begin
  if (Index > (Size-1)) or (Index < 0) or (Bits[Index] = False) then
    Exit;
  Complement;
end;

function TPartition.IsIdentical(Value: TPartition): Boolean; // returns false if any difference
var
 i: Integer;
 p,q: ^Integer;
begin
  Result := False;
  p := FBits;
  q := Value.FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    if p^ <> q^ then Exit;
    Inc(p);
    Inc(q);
  end;
  Result := True;
end;

procedure TPartition.Add(Value: TPartition);        // does OR product
var
  i: Integer;
  p,q: ^Integer;
begin
  p := FBits;
  q := Value.FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    p^ := p^ or q^;
    Inc(p);
    Inc(q);
  end;
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
end;

procedure TPartition.Subtract(Value: TPartition);   // removes common bits
var
  i: Integer;
  p,q: ^Integer;
begin
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
  p := FBits;
  q := Value.FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    p^ := p^ and (not q^);
    Inc(p);
    Inc(q);
  end;
end;

procedure TPartition.Common(Value: TPartition);     // does AND product
var
  i: Integer;
  p, q: ^Integer;
begin
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
  p := FBits;
  q := Value.FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    p^ := p^ and q^;
    Inc(p);
    Inc(q);
  end;
end;

procedure TPartition.Difference(Value: TPartition); // does XOR product
var
  i: Integer;
  p, q: ^Integer;
begin
  if FPartitionString <> EmptyStr then
    FPartitionString := EmptyStr;
  p := FBits;
  q := Value.FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    p^ := p^ xor q^;
    Inc(p);
    Inc(q);
  end;
end;

procedure TPartition.AddFreq(Value: Double);
begin
  FFreq := FFreq + Value;
end;

procedure TPartition.Assign(Source: TPartition);       // copies Value into itself
var
 i: Integer;
 p, q: ^Integer;
begin
  Size := Source.Size;
  Freq := Source.Freq;
  BLen := Source.BLen;
  FPartitionString := Source.FPartitionString;
  p := FBits;
  q := Source.FBits;
  for i := 0 to ((Size+BitsPerInt-1) div BitsPerInt)-1 do begin
    p^ := q^;
    Inc(p);
    Inc(q);
  end;
end;

function TPartition.Clone: TPartition;
begin
  Result := TPartition.Create;
  Result.Assign(Self);
end;

function TPartition.GetNoOfBits:integer;
var i : integer;
begin
    Result := 0;
    for i := 0 to Size-1 do
        if Bits[i] then Inc(Result);
end;

function TPartition.DescString(simple: Boolean): String;
begin
  if simple then
  begin
    Result := Format('%s: %.0n %s', [GetPartitionString, Freq, LineEnding]);
    Exit;
  end;
  Result := Format('[split] %s', [GetPartitionString]);
  Result := Result + '[Size] ' + IntToStr(Size) + LineEnding;
  Result := Result + '[NoOfBits] ' + IntToStr(NoOfBits) + LineEnding;
  Result := Result + '[Freq] ' + FloatToStrF(Freq, ffFixed, 5, 5) + LineEnding;
  Result := Result + '[BLen] ' + FloatToStrF(BLen, ffFixed, 5, 5) + LineEnding;
end;

function TPartition.GetPartitionString: String;
var
  i: Integer;
begin
  if FPartitionString <> EmptyStr then
    Exit(FPartitionString);
  if Size = 0 then
    FPartitionString := '-'
  else
  begin
    SetLength(FPartitionString, Size);
    for i := 0 to Size - 1 do
      if Bits[i] then
        FPartitionString[i + 1] :='1'
      else
        FPartitionString[i + 1] :='0';
  end;
  Result := FPartitionString;
end;

end.

