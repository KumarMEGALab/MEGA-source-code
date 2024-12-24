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

unit mgeographical_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, MegaConsts;

type

  { TGeographicalInfo }

  TGeographicalInfo = class(TObject)
    private
      FCity: AnsiString;
      FContinent: AnsiString;
      FCountry: AnsiString;
      FDay: Byte;
      FGroup: AnsiString;
      FMonth: TCalendarMonth;
      FPopulation: AnsiString;
      FReferenceTime: TDateTime;
      FSpecies: AnsiString;
      FYear: SmallInt;
    public
      constructor Create;
      function InitFromLabels(gp, sp, pop, cont, country, city, year, month, day, refTime: AnsiString; var msg: AnsiString): Boolean;
      function InitFromMegaFileCommandString(commandStr: AnsiString): Boolean;
      function MegFileCommandString: AnsiString;
      function LabelNeedsCommandString: Boolean;
      function HasDateInfo: Boolean;
      procedure Assign(Source: TGeographicalInfo);
      property Continent: AnsiString read FContinent write FContinent;
      property Country: AnsiString read FCountry write FCountry;
      property City: AnsiString read FCity write FCity;
      property Group: AnsiString read FGroup write FGroup;
      property Species: AnsiString read FSpecies write FSpecies;
      property Population: AnsiString read FPopulation write FPopulation;
      property Year: SmallInt read FYear write FYear;
      property Month: TCalendarMonth read FMonth write FMonth;
      property Day: Byte read FDay write FDay;
      property ReferenceTime: TDateTime read FReferenceTime write FReferenceTime;
  end;

  PGeographicalInfo = ^TGeographicalInfo;
  TGeographicalInfoList = specialize TFPGList<TGeographicalInfo>;

  function MonthToStr(m: TCalendarMonth): AnsiString;
  function StrToMonth(const s: AnsiString; var IsSuccess: Boolean): TCalendarMonth;
  function StrRepresentsMonth(aStr: AnsiString; m: TCalendarMonth): Boolean;
  function IntToMonth(aInt: Integer): TCalendarMonth;

implementation

uses
  math, MLexSeq;

function MonthToStr(m: TCalendarMonth): AnsiString;
begin
  case m of
    NoMonth: Result := EmptyStr;
    January: Result := 'January';
    February: Result := 'February';
    March: Result := 'March';
    April: Result := 'April';
    May: Result := 'May';
    June: Result := 'June';
    July: Result := 'July';
    August: Result := 'August';
    September: Result := 'September';
    October: Result := 'October';
    November: Result := 'November';
    December: Result := 'December';
  end;
end;

function StrToMonth(const s: AnsiString; var IsSuccess: Boolean): TCalendarMonth;
begin
  if StrRepresentsMonth(s, January) then
    Result := January
  else if StrRepresentsMonth(s, February) then
    Result := February
  else if StrRepresentsMonth(s, March) then
    Result := March
  else if StrRepresentsMonth(s, April) then
    Result := April
  else if StrRepresentsMonth(s, May) then
    Result := May
  else if StrRepresentsMonth(s, June) then
    Result := June
  else if StrRepresentsMonth(s, July) then
    Result := July
  else if StrRepresentsMonth(s, August) then
    Result := August
  else if StrRepresentsMonth(s, September) then
    Result := September
  else if StrRepresentsMonth(s, October) then
    Result := October
  else if StrRepresentsMonth(s, November) then
    Result := November
  else if StrRepresentsMonth(s, December) then
    Result := December
  else
    Result := NoMonth;
  IsSuccess := (Result <> noMonth);
end;

function StrRepresentsMonth(aStr: AnsiString; m: TCalendarMonth): Boolean;
var
  tempInt: Integer;
begin
  case m of
    January: Result := SameText(aStr, 'January') or SameText(aStr, 'Jan') or (TryStrToInt(aStr, tempInt) and (tempInt = 1));
    February: Result := SameText(aStr, 'February') or SameText(aStr, 'Feb') or (TryStrToInt(aStr, tempInt) and (tempInt = 2));
    March: Result := SameText(aStr, 'March') or SameText(aStr, 'Mar') or (TryStrToInt(aStr, tempInt) and (tempInt = 3));
    April: Result := SameText(aStr, 'April') or SameText(aStr, 'Apr') or (TryStrToInt(aStr, tempInt) and (tempInt = 4));
    May: Result := SameText(aStr, 'May') or SameText(aStr, 'May') or (TryStrToInt(aStr, tempInt) and (tempInt = 5));
    June: Result := SameText(aStr, 'June') or SameText(aStr, 'Jun') or (TryStrToInt(aStr, tempInt) and (tempInt = 6));
    July: Result := SameText(aStr, 'July') or SameText(aStr, 'Jul') or (TryStrToInt(aStr, tempInt) and (tempInt = 7));
    August: Result := SameText(aStr, 'August') or SameText(aStr, 'Aug') or (TryStrToInt(aStr, tempInt) and (tempInt = 8));
    September: Result := SameText(aStr, 'September') or SameText(aStr, 'Sep') or (TryStrToInt(aStr, tempInt) and (tempInt = 9));
    October: Result := SameText(aStr, 'October') or SameText(aStr, 'Oct') or (TryStrToInt(aStr, tempInt) and (tempInt = 10));
    November: Result := SameText(aStr, 'November') or SameText(aStr, 'Nov') or (TryStrToInt(aStr, tempInt) and (tempInt = 11));
    December: Result := SameText(aStr, 'December') or SameText(aStr, 'Dec') or (TryStrToInt(aStr, tempInt) and (tempInt = 12));
    else
      Result := False;
  end;
end;

function IntToMonth(aInt: Integer): TCalendarMonth;
begin
  case aInt of
    1: Result := January;
    2: Result := February;
    3: Result := March;
    4: Result := April;
    5: Result := May;
    6: Result := June;
    7: Result := July;
    8: Result := August;
    9: Result := September;
    10: Result := October;
    11: Result := November;
    12: Result := December;
    else
      Result := NoMonth;
  end;
end;

{ TGeographicalInfo }

constructor TGeographicalInfo.Create;
begin
  FContinent := EmptyStr;
  FCountry := EmptyStr;
  FCity := EmptyStr;
  FGroup := EmptyStr;
  FSpecies := EmptyStr;
  FPopulation := EmptyStr;
  FYear := -1;
  FMonth := NoMonth;
  FDay := 0;
  FReferenceTime := 0.0;
end;

function TGeographicalInfo.InitFromLabels(gp, sp, pop, cont, country, city, year, month, day, refTime: AnsiString; var msg: AnsiString): Boolean;
var
  tempInt: Integer;
  m: TCalendarMonth = NoMonth;
  isSuccess: Boolean = True;
  aTime: TDateTime;
begin
  Result := True;
  FGroup := gp;
  FSpecies := sp;
  FPopulation := pop;
  FContinent := cont;
  FCountry := country;
  FCity := city;
  if Trim(year) <> EmptyStr then
  begin
    if not TryStrToInt(year, tempInt) then
    begin
      Result := False;
      msg := Format('failed to parse year: %s', [year]);
    end
    else
      FYear := tempInt;
  end;

  if Trim(month) <> EmptyStr then
  begin
    m := StrToMonth(month, isSuccess);
    if not isSuccess then
    begin
      Result := False;
      msg := Format('failed to parse month (%s). It should be something like January, jan, 1, or 01', [month]);
    end
    else
      FMonth := m;
  end;

  if Trim(day) <> EmptyStr then
  begin
    if not TryStrToInt(day, tempInt) then
    begin
      Result := False;
      msg := Format('failed to parse day: %s', [day]);
    end
    else
      FDay := tempInt;
  end;

  if Trim(refTime) <> EmptyStr then
  begin
    try
      aTime := StrToTime(refTime);
      FReferenceTime := aTime;
    except
      on E:Exception do
      begin
        Result := False;
        msg := Format('invalid time given(%s) - should be formatted like 13:59:59 for example to represent 59 minutes and 59 seconds past 1 pm', [refTime]);
      end;
    end;
  end;
end;

function TGeographicalInfo.InitFromMegaFileCommandString(commandStr: AnsiString): Boolean;
var
  lex: TLexSeq = nil;
  msg: AnsiString = '';
begin
  Result := False;
  try
    lex := TLexSeq.Create;
    lex.ParseOtuLabelCommandLong(PAnsiChar(commandStr));
    Result := InitFromLabels(lex.GpLabel, lex.SpLabel, lex.PopLabel, lex.ContinentLablel, lex.CountryLabel, lex.CityLabel, lex.YearLabel, lex.MonthLabel, lex.DayLabel, lex.TimeLabel, msg);
  finally
    if Assigned(lex) then
      lex.Free;
  end;
end;

function TGeographicalInfo.MegFileCommandString: AnsiString;
var
  pipe: AnsiChar = #0;
begin
  Result := EmptyStr;
  if FGroup <> EmptyStr then
  begin
    Result := Format('group%s%s', [MEGA_COMMAND_DELIMITER, FGroup]);
    pipe := '|';
  end;
  if FSpecies <> EmptyStr then
  begin
    Result := Format('%s%sspecies%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, FSpecies]);
    pipe := '|';
  end;
  if FPopulation <> EmptyStr then
  begin
    Result := Format('%s%spopulation%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, FPopulation]);
    pipe := '|';
  end;
  if FContinent <> EmptyStr then
  begin
    Result := Format('%s%scontinent%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, FContinent]);
    pipe := '|';
  end;
  if FCountry <> EmptyStr then
  begin
    Result := Format('%s%scountry%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, FCountry]);
    pipe := '|';
  end;
  if FCity <> EmptyStr then
  begin
    Result := Format('%s%scity%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, FCity]);
    pipe := '|';
  end;
  if FYear >= 0 then
  begin
    Result := Trim(Format('%s%syear%s%d', [Result, pipe, MEGA_COMMAND_DELIMITER, FYear]));
    pipe := '|';
  end;
  if FMonth <> NoMonth then
  begin
    Result := Format('%s%smonth%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, MonthToStr(FMonth)]);
    pipe := '|';
  end;
  if FDay > 0 then
  begin
    Result := Format('%s%sday%s%d', [Result, pipe, MEGA_COMMAND_DELIMITER, FDay]);
    pipe := '|';
  end;
  if CompareValue(FReferenceTime, 0.0, FP_CUTOFF) > 0 then
    Result := Format('%s%stime%s%s', [Result, pipe, MEGA_COMMAND_DELIMITER, FormatDateTime('hh:mm:ss', FReferenceTime)]);
  if Trim(Result) <> EmptyStr then
    Result := Format('_{%s}', [Result])
  else
    Result := EmptyStr;
end;

function TGeographicalInfo.LabelNeedsCommandString: Boolean;
begin
  Result :=   ((FContinent <> EmptyStr) or
              (FCountry <> EmptyStr) or
              (FCity <> EmptyStr) or
              (FGroup <> EmptyStr) or
              (FSpecies <> EmptyStr) or
              (FPopulation <> EmptyStr) or
              (FYear >= 0) or
              (FMonth <> NoMonth) or
              (FDay > 0) or
              (CompareValue(FReferenceTime, 0.0, FP_CUTOFF) > 0));
end;

function TGeographicalInfo.HasDateInfo: Boolean;
begin
  Result := (FYear >= 0) and (FMonth <> NoMonth) and (FDay > 0);
end;

procedure TGeographicalInfo.Assign(Source: TGeographicalInfo);
begin
  FContinent := Source.Continent;
  FCountry := Source.Country;
  FCity := Source.City;
  FGroup := Source.Group;
  FSpecies := Source.Species;
  FPopulation := Source.Population;
  FYear := Source.Year;
  FMonth := Source.Month;
  FDay := Source.Day;
  FReferenceTime := Source.ReferenceTime;
end;

end.

