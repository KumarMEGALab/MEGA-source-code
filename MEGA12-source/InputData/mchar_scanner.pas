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

unit mchar_scanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MAX_BUFFER_SIZE = 2000000;

type

  { TCharScanner }

  TCharScanner = class(TObject)
    private
      FFileStream: TFileStream;
      FFileName: String;
      FBuffer: array[0..MAX_BUFFER_SIZE] of AnsiChar;
      FCurrPos: Integer; // our current position as we are scanning the buffer
      FEndPos: Integer; // position of the last valid character in the buffer
      FLine: Integer;
      FColumn: Integer;
      FHasNext: Boolean;
      FBytesRead: Int64;
      FFileSize: Int64;
      procedure Initialize;
      function ReloadBuffer: Boolean;
      function CheckBufferReload: Boolean;
      function IsWhiteSpace(aChar: AnsiChar): Boolean;
    public
      constructor Create(filename: String);
      destructor Destroy; override;
      function Read(var aChar: AnsiChar): Boolean;
      function HasNext: Boolean;
      function EndOfFile: Boolean;
      function SizeInBytes: Int64;
  end;

implementation

{ TCharScanner }

procedure TCharScanner.Initialize;
begin
  FFileStream := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyNone);;
  FBytesRead := 0;
  FFileSize := FFileStream.Size;
  FCurrPos := 0;
  FEndPos := -1;
  FHasNext := True;
  FLine := 1;
  FColumn := 1;
end;

function TCharScanner.ReloadBuffer: Boolean;
begin
  try
    Result := False;
    FEndPos := FFileStream.Read(FBuffer, MAX_BUFFER_SIZE) - 1;
    FBytesRead := FBytesRead + FEndPos;
    FBuffer[FEndPos + 1] := #0;
    FCurrPos := 0;

    if FEndPos >= 0 then
      Result := True;
  Except
    FHasNext := False;
    Result := False
  end;
end;

function TCharScanner.CheckBufferReload: Boolean;
begin
  Result := True;

  if (FEndPos < 0) then
    Result := ReloadBuffer
  else if FCurrPos >= (FEndPos + 1) then
    Result := ReloadBuffer;
end;

function TCharScanner.IsWhiteSpace(aChar: AnsiChar): Boolean;
begin
  Result := False;
  if (AChar = #9) or
     (AChar = #13) or
     (AChar = #10) or
     (AChar = #32) then
    Result := True;
end;

function TCharScanner.EndOfFile: Boolean;
var
  Index: Int64;
begin
  Result := False;
  if FFileStream.Position < FFileStream.Size then
    Exit;

  Index := FCurrPos;
  while (IsWhiteSpace(FBuffer[Index]) and (FBuffer[Index] <> #0)) do
  begin
    inc(Index);
  end;

  if (FBuffer[Index] = #0) then
    Result := True;
end;

constructor TCharScanner.Create(filename: String);
begin
  FFilename := filename;
  Initialize;
end;

destructor TCharScanner.Destroy;
begin
  if Assigned(FFileStream) then
    FFileStream.Free;
  inherited Destroy;
end;

function TCharScanner.Read(var aChar: AnsiChar): Boolean;
begin
  Result := False;
  if not CheckBufferReload then
  begin
    FHasNext := False;
    Exit;
  end;
  aChar := FBuffer[FCurrPos];
  inc(FCurrPos);
  Result := True;
end;

function TCharScanner.HasNext: Boolean;
begin
  Result := FHasNext;
end;

function TCharScanner.SizeInBytes: Int64;
begin
  Result := FFileSize;
end;

end.

