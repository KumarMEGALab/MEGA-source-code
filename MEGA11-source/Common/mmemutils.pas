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

unit mmemutils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  process,
  {$ENDIF}
  Classes, SysUtils, FileUtil;

function GetAvailablePhysicalMemory(var availableMem: Int64): Boolean;

implementation

{$IFNDEF MSWINDOWS}
uses
  StringUtils;
{$ENDIF}

{$IFDEF MSWINDOWS}
function GetAvailablePhysicalMemory(var availableMem: Int64): Boolean;
var
  ms: TMemoryStatus;
begin
  ms.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(ms);
  availableMem := ms.dwAvailPhys;
  Result := True;
end;
{$ENDIF}
{$IFDEF DARWIN}
function GetAvailablePhysicalMemory(var availableMem: Int64): Boolean;
const
  READ_BYTES = 2048;
  digits = ['0', '1','2' ,'3' ,'4' , '5', '6', '7', '8', '9'];
var
  NumBytes: LongInt;
  BytesRead: LongInt;
  Process: TProcess = nil;
  MemStream: TMemoryStream = nil;
  Buffer: String;
  APath: String;
  exe: String;
  tempList: TStringList = nil;
  tokens: TStringList = nil;
  tempInt: LongInt = 0;
  i: Integer;
  tokenString: String;
  tempString: String = '';
begin
  Result := False;
  exe := FindDefaultExecutablePath('vm_stat');
  if not FileExists(exe) then
    exit;

  try
    try
      tempList := TStringList.Create;
      tokens := TStringList.Create;
      MemStream := TMemoryStream.Create;
      BytesRead := 0;
      SetLength(Buffer, READ_BYTES);
      Process := TProcess.Create(nil);
      Process.Executable := exe;
      APath := ExtractFilePath(Exe);
      Delete(APath, Length(APath), 1);
      Process.CurrentDirectory := APath;
      Process.Options := [poUsePipes, poStdErrToOutPut, poNoConsole];
      Process.Execute;
      while true do
      begin
        MemStream.SetSize(BytesRead + READ_BYTES);
        NumBytes := Process.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
        begin
          Inc(BytesRead, NumBytes);
          SetLength(Buffer, NumBytes);
          MemStream.Read(Buffer[1], NumBytes);
          tempString := tempString + Buffer;
        end
        else
          BREAK;
      end;
      tempList.Text := tempString;
      if tempList.Count > 0 then
      begin
        tokenString := tempList.Text;
        for i := 0 to tempList.Count - 1 do
        begin
          SplitOnSingleCharFaster(Trim(tempList[i]), ':', tokens, False);

          if tokens.Count > 1 then
          begin
            if SameText('Pages free', tokens[0]) then
            begin
              tokenString := Trim(tokens[1]);
              while (not (tokenString[Length(tokenString)] in digits)) and (Length(tokenString) > 0) do
                SetLength(tokenString, Length(tokenString) - 1);
              if TryStrToInt(tokenString, tempInt) then
              begin
                availableMem := tempInt * 4096;
                Result := True;
                break;
              end;
            end;
          end;
        end;
      end;
    except
      Result := False;
      // just fail
    end;
  finally
    if Assigned(Process) then
      Process.Free;
    if Assigned(MemStream) then
      MemStream.Free;
    if Assigned(tempList) then
      tempList.Free;
    if Assigned(tokens) then
      tokens.Free;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
function GetAvailablePhysicalMemory(var availableMem: Int64): Boolean;
const
  READ_BYTES = 2048;
var
  NumBytes: LongInt;
  BytesRead: LongInt;
  Process: TProcess = nil;
  MemStream: TMemoryStream = nil;
  Buffer: String;
  APath: String;
  exe: String;
  tempList: TStringList = nil;
  tokens: TStringList = nil;
  tempInt: LongInt;
  i: Integer;
  debug: String;
  tempString: String = '';
begin
  Result := False;
  exe := FindDefaultExecutablePath('free');
  if not FileExists(exe) then
    exit;

  try
    try
      tempList := TStringList.Create;
      tokens := TStringList.Create;
      MemStream := TMemoryStream.Create;
      BytesRead := 0;
      SetLength(Buffer, READ_BYTES);
      Process := TProcess.Create(nil);
      Process.Executable := exe;
      APath := ExtractFilePath(Exe);
      Delete(APath, Length(APath), 1);
      Process.CurrentDirectory := APath;
      //Process.Parameters.Add('-b');
      Process.Options := [poUsePipes, poStdErrToOutPut, poNoConsole];
      Process.Execute;
      while true do
      begin
        MemStream.SetSize(BytesRead + READ_BYTES);
        NumBytes := Process.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
        begin
          Inc(BytesRead, NumBytes);
          SetLength(Buffer, NumBytes);
          MemStream.Read(Buffer[1], NumBytes);
          tempString := tempString + Buffer;
        end
        else
          BREAK;
      end;
      tempList.Text := tempString;
      if tempList.Count > 0 then
      begin
        debug := tempList.Text;
        for i := 0 to tempList.Count - 1 do
        begin
          SplitOnWhiteSpace2(Trim(tempList[i]), tokens);
          if (tokens.Count > 0) and SameText('Mem:', tokens[0]) then
            if TryStrToInt(tokens[tokens.Count - 1], tempInt) then
            begin
              availableMem := tempInt * 1024;
              Result := True;
              break;
            end;
        end;
      end;
    except
      Result := False;
      // just fail
    end;
  finally
    if Assigned(Process) then
      Process.Free;
    if Assigned(MemStream) then
      MemStream.Free;
    if Assigned(tempList) then
      tempList.Free;
    if Assigned(tokens) then
      tokens.Free;
  end;
end;
{$ENDIF}



end.

