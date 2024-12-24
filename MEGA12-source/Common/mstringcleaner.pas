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

unit MStringCleaner;

interface

uses
  Classes, SysUtils, RegExpr, StringUtils, MegaConsts;

type
  TStringCleaner = class(TObject)
    protected
      FRegex: TRegExpr;
    public
      constructor Create;
      destructor Destroy; override;
      function CleanStringList(var aList: TStringList; const aRegexStr: String; const Selection: ArrayOfInteger): Integer;
      function StripExcessTokens(var aList: TStringList; const MaxTokensToKeep: Integer; const  Selection: ArrayOfInteger): Integer;
  end;
implementation

uses
  Dialogs, Math;

{ TStringCleaner }

function TStringCleaner.CleanStringList(var aList: TStringList; const aRegexStr: String; const Selection: ArrayOfInteger): Integer;
var
  TempList: TStringList;
  TempStr: String;
  i, j: Integer;
  Index: Integer;
  //Match: TMatch;
begin
  Result := 0;
  if aList.Count = 0 then
    Exit;
  TempList := nil;

  try
    try
      TempList := TStringList.Create;
      FRegex := TRegExpr.Create;
      FRegex.Expression := aRegexStr;
      for i := 0 to Length(Selection) - 1 do { Selection specifies only those indices in aList for which we should apply filters}
      begin
        Index := Selection[i];
        TempStr := EmptyStr;
        SplitOnWhiteSpace2(aList[Index], TempList);
        if TempList.Count > 0 then
        begin
          for j := 0 to TempList.Count - 1 do
          begin
            //Match := FRegex.Match(TempList[j]);
            if FRegex.Exec(TempList[j]) then { if it matches our regex, we want to strip it out}
            begin
              inc(Result);
              continue;
            end
            else
              TempStr := TempStr + ' ' + TempList[j];
          end;
        end;
        aList[Index] := Trim(TempStr);
      end;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error occurred: ' + E.Message);
        {$ELSE}
        error_nv('An error occurred when modifying strings: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

constructor TStringCleaner.Create;
begin

end;

destructor TStringCleaner.Destroy;
begin

  inherited;
end;

function TStringCleaner.StripExcessTokens(var aList: TStringList; const MaxTokensToKeep: Integer; const  Selection: ArrayOfInteger): Integer;
var
  TempList: TStringList;
  TempStr: String;
  i, j: Integer;
  Index: Integer;
begin
  Result := 0;
  if aList.Count = 0 then
    Exit;
  TempList := nil;

  try
    try
      TempList := TStringList.Create;
      for i := 0 to Length(Selection) - 1 do { Selection specifies only those indices in aList for which we should apply filters}
      begin
        Index := Selection[i];
        TempStr := EmptyStr;
        SplitOnWhiteSpace2(aList[Index], TempList);
        if TempList.Count > 0 then
        begin
          if (TempList.Count > MaxTokensToKeep) then
            inc(Result);
          for j := 0 to Min(MaxTokensToKeep, TempList.Count) - 1 do
          begin
            TempStr := TempStr + ' ' + TempList[j];
          end;
        end;
        aList[Index] := Trim(TempStr);
      end;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error occurred: ' + E.Message);
        {$ELSE}
        error_nv('An error occurred when modifying strings: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

end.
