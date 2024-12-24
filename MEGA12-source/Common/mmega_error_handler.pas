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

unit mmega_error_handler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMegaErrorHandler }

  TMegaErrorHandler = class(TObject)
    private
      FLastErrorId: LongInt;
      FLog: TStringList;
      function GetHasMessages: Boolean;
      function GetNumMessages: Integer;
    public
      destructor Destroy; override;
      procedure Initialize;
      procedure Reset;
      property Log: TStringList read FLog;
      property LastErrorId: LongInt read FLastErrorId write FLastErrorId;
      property NumMessages: Integer read GetNumMessages;
      property HasMessages: Boolean read GetHasMessages;
  end;

implementation

{ TMegaErrorHandler }

function TMegaErrorHandler.GetHasMessages: Boolean;
begin
  Result := (FLog.Count > 0);
end;

function TMegaErrorHandler.GetNumMessages: Integer;
begin
  Result := FLog.Count;
end;

destructor TMegaErrorHandler.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

procedure TMegaErrorHandler.Initialize;
begin
  FLastErrorId := -1;
  FLog := TStringList.Create;
end;

procedure TMegaErrorHandler.Reset;
begin
  FLog.Clear;
  FLastErrorId := -1;
end;

end.

