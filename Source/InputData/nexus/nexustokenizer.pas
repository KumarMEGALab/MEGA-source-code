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

unit nexustokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtokenizer, nexustokens;

type

  { TNexusTokenizer }

  TNexusTokenizer = class(TObject)
  private
    FEnforceUppercaseCommands: Boolean;
    function GetHasNext: Boolean;
    function GetProgress: Int64;
    function GetSrcFile: String;
    procedure SetEnforceUppercaseCommands(AValue: Boolean);
    procedure SetSrcFile(AValue: String);
    protected
      FDelimSet: TDelimSet;
      FBaseTokenizer: TGTokenizer;
    public
      constructor Create(AFileName: String);
      destructor Destroy; override;

      function ReloadFile: Boolean;
      procedure NextToken(var Token: TNexusToken);
      property HasNext: Boolean read GetHasNext;
      property SrcFile: String read GetSrcFile write SetSrcFile;
      property Progress: Int64 read GetProgress;
      property EnforceUppercaseCommands: Boolean read FEnforceUppercaseCommands write SetEnforceUppercaseCommands;
  end;

implementation

{ TNexusTokenizer }

function TNexusTokenizer.GetHasNext: Boolean;
begin
  Result := FBaseTokenizer.HasNext;
end;

function TNexusTokenizer.GetProgress: Int64;
begin
  Result := FBaseTokenizer.Progress;
end;

function TNexusTokenizer.GetSrcFile: String;
begin
  Result := FBaseTokenizer.SrcFile;
end;

procedure TNexusTokenizer.SetEnforceUppercaseCommands(AValue: Boolean);
begin
  if FEnforceUppercaseCommands=AValue then Exit;
  FEnforceUppercaseCommands:=AValue;
end;

procedure TNexusTokenizer.SetSrcFile(AValue: String);
begin
  FBaseTokenizer.SrcFile := AValue;
end;

constructor TNexusTokenizer.Create(AFileName: String);
begin
  FDelimSet := [';', '=', ',', ' ', #9, #34, #39, '[', ']', {$IFDEF MSWINDOWS} #13, #10{$ELSE}LineEnding{$ENDIF}];
  FBaseTokenizer := TGTokenizer.Create(AFilename, FDelimSet);
  FEnforceUppercaseCommands := False;
end;

destructor TNexusTokenizer.Destroy;
begin
  if Assigned(FBaseTokenizer) then
    FBaseTokenizer.Free;
  inherited Destroy;
end;

function TNexusTokenizer.ReloadFile: Boolean;
begin
  Result := FBaseTokenizer.ReloadFile;
end;

procedure TNexusTokenizer.NextToken(var Token: TNexusToken);
begin
  FBaseTokenizer.NextToken(TTokenBase(Token));
  Token.NexusTokenType := GetTokenType(Token, FEnforceUppercaseCommands);
  if not EnforceUppercaseCommands then
    Token.ChangeCommandToUppercase;
end;

end.

