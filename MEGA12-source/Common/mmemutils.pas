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

unit mmemutils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,jwapsapi,{$ENDIF}
  {$IFDEF UNIX}process, Unix,{$ENDIF}
  {$IFDEF DARWIN}MacOSAll,SysCtl,{$ENDIF}
  Classes, SysUtils, FileUtil;

{$IFDEF UNIX}
type
  rusage = record
    ru_utime: timeval;     // User time used
    ru_stime: timeval;     // System time used
    ru_maxrss: clong;      // Maximum resident set size
    ru_ixrss: clong;       // Shared memory size
    ru_idrss: clong;       // Unshared data size
    ru_isrss: clong;       // Unshared stack size
    ru_minflt: clong;      // Page faults without IO
    ru_majflt: clong;      // Page faults with IO
    ru_nswap: clong;       // Number of swaps
    ru_inblock: clong;     // Block input operations
    ru_oublock: clong;     // Block output operations
    ru_msgsnd: clong;      // IPC messages sent
    ru_msgrcv: clong;      // IPC messages received
    ru_nsignals: clong;    // Signals received
    ru_nvcsw: clong;       // Voluntary context switches
    ru_nivcsw: clong;      // Involuntary context switches
  end;

const RUSAGE_SELF = 0;
{$ENDIF}

function GetPeakWorkingSetSizeMB: Double;
function GetCurrentProcessMemoryMB: Double;
function GetAvailablePhysicalMemory(var availableMem: Int64): Boolean;
{$IFDEF UNIX}
function getrusage(who: cint; var r_usage: rusage): cint; cdecl; external 'c';
{$ENDIF}
{$IFDEF MSWINDOWS}
function GetWorkingMemory(var workingMem: DWORD): Boolean;
{$ENDIF}

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

function GetWorkingMemory(var workingMem: DWORD): Boolean;
var
  ms: TMemoryStatus;
begin
  Result := False;
  try
    ms.dwLength := SizeOf(TMemoryStatus);
    GlobalMemoryStatus(ms);
    workingMem := ms.dwMemoryLoad;
    Result := True;
  except
    Result := False;
  end;
end;

{$ENDIF}
{$IFDEF DARWIN}
function GetAvailablePhysicalMemory(var availableMem: Int64): Boolean;
var
  FreeMemPageCount: Integer;
  FreeMemPageSize: Integer;
  status : Integer;
  len : size_t;
begin
  Result := False;
  len := SizeOf(Integer);

  { yes, this queries virtual instead of physical memory.  gives a more accurate
    assessment since the OS uses a lot of that memory as cache, which it quickly
    frees up as needed.

    a more relevant metric in MacOS is "memory pressure". }

  status := fpSysCtlByName('vm.page_free_count', @FreeMemPageCount, @len, Nil, 0);
  if status <> 0 then RaiseLastOSError;

  // PageSize is 4096 on Intel Mac and 16384 on Apple Silicon
  status := fpSysCtlByName('vm.pagesize', @FreeMemPageSize, @len, Nil, 0);
  if status <> 0 then RaiseLastOSError;

  availableMem := FreeMemPageCount * FreeMemPageSize;
  if (FreeMemPageSize > 0) then // a zero value means sysctl calls didn't work
    Result := True;
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

/// <summary>Get the maximum RAM used by MEGA based on the Working Set</summary>
{$IFDEF MSWINDOWS}
function GetPeakWorkingSetSizeMB: Double;
var
  psmemCounters: _PROCESS_MEMORY_COUNTERS;
  ThisApp: QWord;
  PeakSize: NativeUInt;
begin
  PeakSize := 0;

  ThisApp := GetCurrentProcessID();
  ThisApp := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, ThisApp );
  if( ThisApp = LongWord(0)) then
      exit;
  if(GetProcessMemoryInfo(ThisApp, psmemCounters, sizeof(_PROCESS_MEMORY_COUNTERS)) <> LongBool(0)) then
    PeakSize := psmemCounters.PeakWorkingSetSize
  else
    RaiseLastOSError;
  CloseHandle(ThisApp);

  Result := PeakSize / (1024 * 1024);
end;
{$ENDIF}

{$IFDEF UNIX}
function GetPeakWorkingSetSizeMB: Double;
var
  usage: rusage;
  PeakUsage: Integer;
begin
  if getrusage(RUSAGE_SELF, usage) = 0 then
  begin
    // on Unix systems, we want the MEGA process' peak resident set size
    PeakUsage := usage.ru_maxrss;
    {$IFDEF DARWIN}
    // getrusage on MacOS provides sizes in bytes
    Result := PeakUsage/1024/1024;
    {$ELSE}
    // elsewhere in Kbytes
    Result := PeakUsage/1024;
    {$ENDIF}
  end
end;
{$ENDIF}

  /// <summary>Get the current memory usage of the application</summary>
{$IFDEF MSWINDOWS}
function GetCurrentProcessMemoryMB: Double;
var
  psmemCounters: _PROCESS_MEMORY_COUNTERS;
  ThisApp: LongWord;
begin
  Result := 0;
  ThisApp := GetCurrentProcessID();
  ThisApp := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, ThisApp );
  if( ThisApp = LongWord(0)) then
      exit;
  if(GetProcessMemoryInfo(ThisApp, psmemCounters, sizeof(_PROCESS_MEMORY_COUNTERS)) <> LongBool(0)) then
    Result := psmemCounters.WorkingSetSize/1024/1024
  else
    RaiseLastOSError;
  CloseHandle(ThisApp);
end;
{$ENDIF}

{$IFDEF UNIX}
function GetCurrentProcessMemoryMB: Double;
var
  usage: rusage;
  PeakUsage: Integer;
begin
  if getrusage(RUSAGE_SELF, usage) = 0 then
  begin
    // on Unix systems, we want the MEGA process' peak resident set size
    PeakUsage := usage.ru_maxrss;
    {$IFDEF DARWIN}
    // getrusage on MacOS provides sizes in bytes
    Result := PeakUsage/1024/1024;
    {$ELSE}
    // elsewhere in Kbytes
    Result := PeakUsage/1024;
    {$ENDIF}
  end
end;
{$ENDIF}

end.

