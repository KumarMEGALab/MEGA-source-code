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

unit mtimelimit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, ExtCtrls, dateutils;

type

  { TTimeLimitThread }

  TTimeLimitThread = class(TThread)
    private
      FTimeLimit: Double;
      FMillisecondsTimeLimit: Int64;
      FStartTime: TDateTime;
      FInterval: Integer;
      function CalculateInterval: Integer; { find a suitable interval for the timer}
      function TestTimeLimitReached: Boolean;
      function HoursToMilliseconds(Hours: Double): Int64;
      procedure SetTimeLimit(AValue: Double);
      procedure DoOnTimer;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;

      property TimeLimit: Double read FTimeLimit write SetTimeLimit;
  end;

  procedure LaunchTimeLimitThread(Limit: Double);

implementation

uses
  MegaUtils_NV;

procedure LaunchTimeLimitThread(Limit: Double);
var
  LimitThread: TTimeLimitThread;
begin
  if Limit <= 0.0 then
  begin
    {$IFNDEF VISUAL_BUILD}
    warn_nv('Invalid max time limit was given and was ignored');
    {$ENDIF}
    Exit;
  end;
  LimitThread := TTimeLimitThread.Create;
  LimitThread.TimeLimit := Limit;
  LimitThread.Start;
end;

{ TTimeLimitThread }

function TTimeLimitThread.CalculateInterval: Integer;
begin
  if FTimeLimit > 1.0 then { FTimeLimit is given in hours}
    Result := 60000 { Result is given in milliseconds} {1 minute}
  else if FTimeLimit > 0.017 then {1 minute > 1 hour}
    Result := 10000 { 10 seconds}
  else
    Result := 1000; {1 second}
end;

function TTimeLimitThread.TestTimeLimitReached: Boolean;
begin
  Result := (MilliSecondsBetween(Now, FStartTime) >= FMillisecondsTimeLimit);
end;

function TTimeLimitThread.HoursToMilliseconds(Hours: Double): Int64;
var
  FractPart: Double;
  IntPart: Int64;
begin
  IntPart := trunc(Hours);
  Result := IntPart * 60 * 60 * 1000;
  FractPart := Frac(Hours) * 60 * 60 * 1000;
  Result := Result + Trunc(FractPart);
end;

procedure TTimeLimitThread.SetTimeLimit(AValue: Double);
begin
  if FTimeLimit=AValue then Exit;
  FTimeLimit:=AValue;
  FMillisecondsTimeLimit := HoursToMilliseconds(AValue);
end;

procedure TTimeLimitThread.DoOnTimer;
begin
  if TestTimeLimitReached then
  begin{$IFNDEF VISUAL_BUILD}
    Error_NV('Execution was terminated because the specified max time limit (' + Format('%.2f', [FTimeLimit]) + ' hours) was reached');
    {$ELSE}
    Assert(False, 'max time limit not implemented for mega-gui');
    {$ENDIF}
  end;
end;

constructor TTimeLimitThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FStartTime := Now;
end;

destructor TTimeLimitThread.Destroy;
begin
  inherited Destroy;
end;

procedure TTimeLimitThread.Execute;
begin
  FInterval := CalculateInterval;
  while True do
  begin
    DoOnTimer;
    Sleep(FInterval);
  end;
end;

end.

