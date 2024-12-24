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

unit mtipdatesfinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MCalibrationData, nexusfile, RegExpr, MegaConsts, fgl, MegaUtils_NV,
  mruntimeprogressdlg;

const
  NUMBER_REGEX = '[\d]{1,}[.,]?[\d]{0,}';

type

  TTipDateParseRule = (tdprLast4, tdprLast2, tdprFirst4, tdprFirst2, tdprAny4, tdprAny2, tdprAny4Or2, tdprDelimiter, tdprCustom);
  TTipDatePosition = (tdpBegin, tdpEnd, tdpFirst, tdpSecond, tdpThird, tdpThirdToLast, tdpSecondToLast, tdpLast);

  { TTipDate }

  TTipDate = class(TObject)
    private
      FDateTime: TDateTime;
      FDay: Word;
      FMonth: Word;
      FTaxonName: String;
      FYear: Word;
      function GetSampleTime: Extended;
      procedure SetDay(AValue: Word);
      procedure SetMonth(AValue: Word);
      procedure SetSampleTime(aDateTime: TDateTime);
      procedure SetTaxonName(AValue: String);
      procedure SetYear(AValue: Word);
      function CleanLabel(aLabel: String): String;
    public
      constructor Create(aLabel: String; aYear: Word; aMonth: Word; aDay: Word);
      constructor CreateFromDateTime(aLabel: String; aTime: TDateTime);
      destructor Destroy; override;
      function DateTimeString: String;
      function CalibrationString: String;
      property TaxonName: String read FTaxonName write SetTaxonName;
      property SampleTime: Extended read GetSampleTime;
      property Month: Word read FMonth write SetMonth;
      property Day: Word read FDay write SetDay;
      property Year: Word read FYear write SetYear;
      property DateTime: TDateTime read FDateTime write SetSampleTime;
  end;

  TTipDateList = specialize TFPGList<TTipDate>;

  { TTipDatesFinder }

  TTipDatesFinder = class(TObject)
    private
      FRegex: TRegExpr;
      FDateFormat: String;
      FDelimiter: String;
      FDigits: Integer;
      FEarliestValidYear, FLatestValidYear: Integer;
      FIsCalendarDate: Boolean;
      FParseRule: TTipDateParseRule;
      FTipDatePosition: TTipDatePosition;
      FTipDates: TTipDateList;
      FInputDataFile: String;
      FLogStrings: TStringList;
      F4DigitStrings: TStringList;
      F2DigitStrings: TStringList;
      FUsingDelimiter: Boolean;
      procedure SetDateFormat(AValue: String);
      procedure SetDelimiter(AValue: String);
      procedure SetDigits(AValue: Integer);
      procedure SetInputDataFile(AValue: String);
      function IsDigit(aChar: Char): Boolean;
      function IsValidYear(aYear: Double): Boolean;
      procedure SetIsCalendarDate(AValue: Boolean);
      procedure SetParseRule(AValue: TTipDateParseRule);
      procedure SetTipDatePosition(AValue: TTipDatePosition);
      procedure SetUsingDelimiter(AValue: Boolean);
    protected
      FRegexMatchStrings: TStringList;
      FNexusFile: TNexusFile;
      procedure Clear;
      function ExtractYearFromLastFour(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearFromLastTwo(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearFromFirstFour(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearFromFirstTwo(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearFromLabelAny4Or2(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearFromLabel(const aLabel: String; var Year: Extended): Boolean;
      function ExtractTimeInfoFromLabel(const aLabel: String; var year: Word; var Month: Word; var Day: Word): Boolean;
      function ExtractDateTimeUsingDelimiter(const aLabel: String; var isSuccess: Boolean): TDateTime;
      function ExtractDateTimeUsingPosition(const aLabel: String; var isSuccess: Boolean): TDateTime;
      function ExtractCalendarDateUsingDelimiter(const aLabel: String; var Year: Extended): Boolean;
      function ExtractCalendarDateUsingPosition(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearUsingDelimiter(const aLabel: String; var Year: Extended): Boolean;
      function ExtractYearUsingPosition(const aLabel: String; var Year: Extended): Boolean;
      function PopulateCalibrations(var Calibrations: TCalibrations): Boolean;

      function FindFirstNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
      function FindSecondNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
      function FindThirdNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
      function FindThirdToLastNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
      function FindSecondToLastNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
      function FindLastNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
      function FindAllNumbersInLabel(aLabel: String): ArrayOfExtended;

      function GetSubstringFromDelimiter(const aLabel: String; var substr: String): Boolean;
      function BuildYearRegex(var aRegex: String): Boolean;
      function Convert2DigitDate(aDate: Extended): Extended; overload;
      function Convert2DigitDate(const aDateStr: String; var isSuccess: Boolean): Extended; overload;
      function Convert4DigitDate(const aDateStr: String; var IsSuccess: Boolean): TDateTime;
      function Convert6DigitDate(const aDateStr: String; var IsSuccess: Boolean): TDateTime;
      function Convert8DigitDate(const aDateStr: String; var IsSuccess: Boolean): TDateTime;
      function ConvertStringToDate(aDateStr: String): TDateTime;
      function FindTipDatesInNexusFile(const filename: String; var isSuccess:Boolean): TStringList;
      function FindTipDatesInMegaFile(const Filename: String; var isSuccess:Boolean): TStringList;
      function FindTipDatesInFastaFile(const Filename: String; var isSuccess:Boolean): TStringList;
      function TaxaLabelsToTipDateCalibrations(taxaLabels: TStringList; var isSuccess: Boolean): TStringList;
      function ExportActiveAlignmentToFile(const filename: String; const aFormat: TMegaExportFormat): Boolean;
      {$IFNDEF VISUAL_BUILD}
      function NonVisualExportAlignmentTofile: Boolean;
      {$ENDIF}
    public
      CheckCancel: TCheckCancelFunc;
      constructor Create;
      destructor Destroy; override;
      function FindTipDates(const Filename: String; var IsSuccess: Boolean): TStringList; overload;
      function FindTipDates(TaxaNames: TStringList; var IsSuccess: Boolean): TStringList; overload;
      function FindTipDates(TaxaNames: TStringList): Boolean; overload;
      function NumberOfTipDates: Integer;
      function GetCalibrationStrings: TStringList;
      class function SampleTimeStrToSampleTime(aStr: String): Extended;
      procedure AssignTipDatesToList(aList: TList); overload;
      procedure AssignTipDatesToList(aList: TTipDateList); overload;
      property InputDataFile: String read FInputDataFile;
      property LogStrings: TStringList read FLogStrings;
      property ParseRule: TTipDateParseRule read FParseRule write SetParseRule;
      property Digits: Integer read FDigits write SetDigits;
      property IsCalendarDate: Boolean read FIsCalendarDate write SetIsCalendarDate;
      property DateFormat: String read FDateFormat write SetDateFormat;
      property UsingDelimiter: Boolean read FUsingDelimiter write SetUsingDelimiter;
      property Delimiter: String read FDelimiter write SetDelimiter;
      property TipDatePosition: TTipDatePosition read FTipDatePosition write SetTipDatePosition;
  end;

  { TParseTipDatesThread }

  TParseTipDatesThread = class(TMEGAThread)
    private
      FIsCancelled: Boolean;
      FIsSuccess: Boolean;
      function GetLogText: String;
    protected
      FDateFormat: String;
      FLog: TStringList;
      FProgress: Integer;
      FStatus: String;
      FParser: TTipDatesFinder;
      procedure Execute; override;
      function UpdateProgress(aProgress: Integer; aStatus: String): Boolean;
      procedure DoUpdateProgress;
      function FindTipDates: Boolean;
    public
      RuntimeProgress: TRuntimeProgress;
      CheckCancel: TCheckCancelFunc;
      constructor Create(aDateFormat: String);
      destructor Destroy; override;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FIsCancelled;
      property LogText: String read GetLogText;
  end;



  function IntToDatePosition(aValue: Integer): TTipDatePosition;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Dialogs,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  dateutils, ProcessInputData, MD_InputSeqData,
  MOtuInfo, mseqexportoptions, mgeographical_info;

function IntToDatePosition(aValue: Integer): TTipDatePosition;
begin
  case aValue of
    0: Result := tdpBegin;
    1: Result := tdpEnd;
    2: Result := tdpFirst;
    3: Result := tdpSecond;
    4: Result := tdpThird;
    5: Result := tdpThirdToLast;
    6: Result := tdpSecondToLast;
    7: Result := tdpLast;
    else
      raise Exception.Create('Invalid value for date position');
  end;
end;

{ TParseTipDatesThread }

function TParseTipDatesThread.GetLogText: String;
begin
  Result := FLog.Text;
end;

procedure TParseTipDatesThread.Execute;
begin
  try
    while True do
    begin
      FIsSuccess := FindTipDates;
      Terminate;
      break;
    end;
  except
    on E:EAbort do
    begin
      FIsCancelled := True;
      FIsSuccess := False;
      FLog.Add('user cancelled');
    end;
    on E:Exception do
    begin
      FIsCancelled := False;
      FIsSuccess := False;
      FLog.Add(E.Message);
    end;
  end;
end;

function TParseTipDatesThread.UpdateProgress(aProgress: Integer; aStatus: String): Boolean;
begin
  FProgress := aProgress;
  FStatus := aStatus;
  Synchronize(@DoUpdateProgress);
  Result := FIsCancelled;
end;

procedure TParseTipDatesThread.DoUpdateProgress;
begin
  if Assigned(CheckCancel) then
    FIsCancelled := CheckCancel(FProgress, FStatus);
end;

function TParseTipDatesThread.FindTipDates: Boolean;
var
  taxaNames: TStringList = nil;
  i: Integer;
  tipDates: TTipDateList = nil;
begin
  try
    taxaNames := TStringList.Create;
    if D_InputSeqData.NoOfTaxa > 0 then
      for i := 0 to D_InputSeqData.NoOfTaxa - 1 do
        taxaNames.Add(D_InputSeqData.OtuInfos.Otu[i].Name);
    FParser := TTipDatesFinder.Create;
    FParser.DateFormat := FDateFormat;
    FParser.IsCalendarDate := True;
    FParser.ParseRule := tdprCustom;
    FParser.CheckCancel := @UpdateProgress;
    Result := FParser.FindTipDates(taxaNames);
    if not Result then
      FLog.Add(FParser.LogStrings.Text);
    tipDates := TTipDateList.Create;
    FParser.AssignTipDatesToList(tipDates);
    D_InputSeqData.OtuInfos.AssignTipDates(tipDates);
  finally
    if Assigned(taxaNames) then
      taxaNames.Free;
    if Assigned(tipDates) then
      tipDates.Free;
  end;
end;

constructor TParseTipDatesThread.Create(aDateFormat: String);
begin
  inherited Create(True);
  FDateFormat := aDateFormat;
  FreeOnTerminate := True;
  FLog := TStringList.Create;
  RuntimeProgress := nil;
end;

destructor TParseTipDatesThread.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FParser) then
    FParser.Free;
  inherited Destroy;
end;

{ TTipDate }

procedure TTipDate.SetSampleTime(aDateTime: TDateTime);
var
  aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond: Word;
begin
  FDateTime := aDateTime;
  DecodeDateTime(aDateTime, aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond);
  FYear := aYear;
  FMonth := aMonth;
  FDay := aDay;
end;

function TTipDate.GetSampleTime: Extended;
var
  d: Word;
begin
  d := DayOfTheYear(FDateTime);
  Result := FYear + d/DaysInYear(FDateTime);
end;

procedure TTipDate.SetDay(AValue: Word);
begin
  if FDay = AValue then Exit;
  FDay := AValue;
end;

procedure TTipDate.SetMonth(AValue: Word);
begin
  if FMonth = AValue then Exit;
  FMonth := AValue;
end;

procedure TTipDate.SetTaxonName(AValue: String);
begin
  if FTaxonName=AValue then Exit;
  FTaxonName:=AValue;
end;

procedure TTipDate.SetYear(AValue: Word);
begin
  if FYear = AValue then Exit;
  FYear := AValue;
end;

function TTipDate.CleanLabel(aLabel: String): String;
begin
  if (aLabel[1] = #39) and (aLabel[Length(aLabel)] = #39) then
    Result := StringReplace(aLabel, '_', ' ', [rfReplaceAll])
  else
    Result := Trim(aLabel);
end;

function TTipDate.DateTimeString: String;
begin
  Result := FormatDateTime(DEFAULT_DATE_FORMAT, FDateTime);
end;

constructor TTipDate.Create(aLabel: String; aYear: Word; aMonth: Word; aDay: Word);
begin
  FTaxonName := CleanLabel(aLabel);
  FDateTime := EncodeDateTime(aYear, aMonth, aDay, 0, 0, 0, 0);
  FYear := aYear;
  FMonth := aMonth;
  FDay := aDay;
end;

constructor TTipDate.CreateFromDateTime(aLabel: String; aTime: TDateTime);
var
  y, m, d, h, minute, s, ms: Word;
begin
  FTaxonName := CleanLabel(aLabel);
  DecodeDateTime(aTime, y, m, d, h, minute, s, ms);
  FDateTime := aTime;
  FYear := y;
  FMonth := m;
  FDay := d;
end;

destructor TTipDate.Destroy;
begin
  inherited Destroy;
end;

function TTipDate.CalibrationString: String;
var
  aName: String;
begin
  aName := #39 + FTaxonName + #39;
  Result := Format('!Taxon=%s sampleTime=%s;', [aName, FormatDateTime(DEFAULT_DATE_FORMAT, FDateTime)]);
end;

{ TTipDatesFinder }

procedure TTipDatesFinder.SetInputDataFile(AValue: String);
begin
  if FInputDataFile=AValue then Exit;
  FInputDataFile:=AValue;
  if not FileExists(FInputDataFile) then
    raise Exception.Create(Format('Input data file not found: %s', [FInputDataFile]));
end;

procedure TTipDatesFinder.SetDateFormat(AValue: String);
begin
  if FDateFormat=AValue then Exit;
  FDateFormat:=AValue;
end;

procedure TTipDatesFinder.SetDelimiter(AValue: String);
begin
  if FDelimiter=AValue then Exit;
  FDelimiter:=AValue;
end;

procedure TTipDatesFinder.SetDigits(AValue: Integer);
begin
  if FDigits=AValue then Exit;
  FDigits:=AValue;
end;

function TTipDatesFinder.IsDigit(aChar: Char): Boolean;
begin
  Result := (aChar = '0') or
            (aChar = '1') or
            (aChar = '2') or
            (aChar = '3') or
            (aChar = '4') or
            (aChar = '5') or
            (aChar = '6') or
            (aChar = '7') or
            (aChar = '8') or
            (aChar = '9');
end;

function TTipDatesFinder.IsValidYear(aYear: Double): Boolean;
begin
  Result := ((trunc(aYear) >= FEarliestValidYear) and (trunc(aYear) <= FLatestValidYear));
end;

procedure TTipDatesFinder.SetIsCalendarDate(AValue: Boolean);
begin
  if FIsCalendarDate=AValue then Exit;
  FIsCalendarDate:=AValue;
end;

procedure TTipDatesFinder.SetParseRule(AValue: TTipDateParseRule);
begin
  if FParseRule=AValue then Exit;
  FParseRule:=AValue;
end;

procedure TTipDatesFinder.SetTipDatePosition(AValue: TTipDatePosition);
begin
  if FTipDatePosition=AValue then Exit;
  FTipDatePosition:=AValue;
end;

procedure TTipDatesFinder.SetUsingDelimiter(AValue: Boolean);
begin
  if FUsingDelimiter=AValue then Exit;
  FUsingDelimiter:=AValue;
end;

procedure TTipDatesFinder.Clear;
var
  i: Integer;
begin
  if FTipDates.Count > 0 then
    for i := 0 to FTipDates.Count - 1 do
      if Assigned(FTipDates[i]) then
        FTipDates[i].Free;
  FTipDates.Clear;
end;

function TTipDatesFinder.ExtractYearFromLastFour(const aLabel: String; var Year: Extended): Boolean;
var
  tempStr: String = '';
  tempFloat: Extended;
begin
  Result := False;
  if Length(Trim(aLabel)) < 4 then
    Exit;
  tempStr := Trim(aLabel);
  tempStr := Copy(tempStr, Length(tempStr) - 3, 4);
  if TryStrToFloat(tempStr, tempFloat) then
  begin
    if IsValidYear(tempFloat) then
    begin
      Year := tempFloat;
      Result := True;
    end;
  end;
end;

function TTipDatesFinder.ExtractYearFromLastTwo(const aLabel: String; var Year: Extended): Boolean;
var
  tempStr: String = '';
  tempFloat: Extended;
begin
  Result := False;
  if Length(Trim(aLabel)) < 2 then
    Exit;
  tempStr := Trim(aLabel);
  tempStr := Copy(tempStr, Length(tempStr) - 1, 2);
  if TryStrToFloat(tempStr, tempFloat) then
  begin
    tempFloat := Convert2DigitDate(tempFloat);
    if IsValidYear(tempFloat) then
    begin
      Year := tempFloat;
      Result := True;
    end;
  end;
end;

function TTipDatesFinder.ExtractYearFromFirstFour(const aLabel: String; var Year: Extended): Boolean;
var
  tempStr: String = '';
  tempFloat: Extended;
begin
  Result := False;
  if Length(Trim(aLabel)) < 4 then
    Exit;
  tempStr := Trim(aLabel);
  tempStr := Copy(tempStr, 1, 4);
  if TryStrToFloat(tempStr, tempFloat) then
  begin
    if IsValidYear(tempFloat) then
    begin
      Year := tempFloat;
      Result := True;
    end;
  end;
end;

function TTipDatesFinder.ExtractYearFromFirstTwo(const aLabel: String; var Year: Extended): Boolean;
var
  tempStr: String = '';
  tempFloat: Extended;
begin
  Result := False;
  if Length(Trim(aLabel)) < 2 then
    Exit;
  tempStr := Trim(aLabel);
  tempStr := Copy(tempStr, 1, 2);
  if TryStrToFloat(tempStr, tempFloat) then
  begin
    tempFloat := Convert2DigitDate(tempFloat);
    if IsValidYear(tempFloat) then
    begin
      Year := tempFloat;
      Result := True;
    end;
  end;
end;

function TTipDatesFinder.ExtractYearFromLabelAny4Or2(const aLabel: String; var Year: Extended): Boolean;
var
  i, Index: Integer;
  tempStr: String = '';
  tempFloat: Extended;
  candidate: Extended = 0;
begin
  Index := 1;
  Result := False;
  F4DigitStrings.Clear;
  F2DigitStrings.Clear;
  while Index <= Length(aLabel) do
  begin
    if IsDigit(aLabel[Index]) then
      tempStr := tempStr + aLabel[Index]
    else
    begin
      if Length(tempStr) = 4 then
        F4DigitStrings.Add(tempStr)
      else if Length(tempStr) = 2 then
        F2DigitStrings.Add(tempStr);
      tempStr := EmptyStr;
    end;
    inc(Index);
  end;

  if Length(tempStr) = 4 then
    F4DigitStrings.Add(tempStr)
  else if Length(tempStr) = 2 then
    F2DigitStrings.Add(tempStr);

  if F4DigitStrings.Count > 0 then
  begin
    for i := 0 to F4DigitStrings.Count - 1 do
    begin
      if TryStrToFloat(F4DigitStrings[i], tempFloat) then
      begin
        if IsValidYear(tempFloat) and (tempFloat > candidate) then
          candidate := tempFloat;
      end;
    end;
    if IsValidYear(candidate) then
    begin
      Result := True;
      Year := candidate;
      Exit;
    end;
  end;
  if F2DigitStrings.Count > 0 then
  begin
    for i := 0 to F2DigitStrings.Count - 1 do
    begin
      if TryStrToFloat(F2DigitStrings[i], tempFloat) then
      begin
        tempFloat := Convert2DigitDate(tempFloat);
        if IsValidYear(tempFloat) and (tempFloat > candidate) then
          candidate := tempFloat;
      end
    end;
    if IsValidYear(candidate) then
    begin
      Result := True;
      Year := candidate;
      Exit;
    end;
  end;
  Result := False;
end;

function TTipDatesFinder.ExtractYearFromLabel(const aLabel: String; var Year: Extended): Boolean;
begin
  Result := False;
  case FParseRule of
    tdprLast4: Result := ExtractYearFromLastFour(aLabel, Year);
    tdprLast2: Result := ExtractYearFromLastTwo(aLabel, Year);
    tdprFirst4: ;
    tdprFirst2: ;
    tdprAny4: ;
    tdprAny2: ;
    tdprAny4Or2: Result := ExtractYearFromLabelAny4Or2(aLabel, Year);
    tdprDelimiter: ;
    tdprCustom:
      begin
        if FIsCalendarDate then
        begin
          if FUsingDelimiter then
          begin
            Result := ExtractCalendarDateUsingDelimiter(aLabel, Year);
          end
          else
          begin
            Result := ExtractCalendarDateUsingPosition(aLabel, Year);
          end;
        end
        else
        begin
          if FUsingDelimiter then
          begin
            Result := ExtractYearUsingDelimiter(aLabel, Year);
          end
          else
          begin
            Result := ExtractYearUsingPosition(aLabel, Year);
          end;
        end;
      end;
  end;
end;

function TTipDatesFinder.ExtractTimeInfoFromLabel(const aLabel: String; var year: Word; var Month: Word; var Day: Word): Boolean;
var
  temp: TDateTime;
  isSuccess: Boolean = False;
  h, m, s, ms: Word;
  tempYear: Extended = 0;
begin
  Result := False;
  if FParseRule = tdprCustom then
  begin
    if FIsCalendarDate then
    begin
      if FUsingDelimiter then
      begin
        temp := ExtractDateTimeUsingDelimiter(aLabel, isSuccess);
        if isSuccess then
          DecodeDateTime(temp, year, month, day, h, m, s, ms);
      end
      else
      begin
        temp := ExtractDateTimeUsingPosition(aLabel, isSuccess);
        if isSuccess then
          DecodeDateTime(temp, year, month, day, h, m, s, ms);
      end;
    end
    else
    begin
      if FUsingDelimiter then
      begin
        isSuccess := ExtractYearUsingDelimiter(aLabel, tempYear);
      end
      else
      begin
        isSuccess := ExtractYearUsingPosition(aLabel, tempYear);
      end;
      if isSuccess then
      begin
        year := trunc(tempYear);
        month := 1;
        day := 1;
      end;
    end;
    Result := isSuccess;
  end
  else if FParseRule = tdprAny4Or2 then
  begin
    isSuccess := ExtractYearFromLabelAny4Or2(aLabel, tempYear);
    if isSuccess then
    begin
      year := trunc(tempYear);
      month := 1;
      day := 1;
      Result := True;
    end;
  end
  else
    raise Exception.Create('not implemented');
end;

function TTipDatesFinder.ExtractDateTimeUsingDelimiter(const aLabel: String; var isSuccess: Boolean): TDateTime;
var
  reg: String = '';
  temp: String = '';
begin
  isSuccess := False;
  if BuildYearRegex(reg) then
  begin
    FRegex.Expression := reg;
    if GetSubstringFromDelimiter(aLabel, temp) then
    begin
      Assert(FRegex.Expression <> EmptyStr);
      if FRegex.Exec(temp) then
      begin
        temp := FRegex.Match[0];
        try
          Result := ConvertStringToDate(temp);
          isSuccess := True;
        except
          isSuccess := False;
        end;
      end;
    end;
  end;
end;

function TTipDatesFinder.ExtractDateTimeUsingPosition(const aLabel: String; var isSuccess: Boolean): TDateTime;
var
  temp: String = '';
  reg: String = '';

  procedure FindRegexMatches(maxMatches: Integer);
  var
    index: Integer = 0;
  begin
    repeat
      FRegexMatchStrings.Add(FRegex.Match[0]);
      inc(index);
    until (index = maxMatches) or (not FRegex.ExecPos(FRegex.MatchPos[0] + 1));
  end;

begin
  isSuccess := False;
  if BuildYearRegex(reg) then
  begin
    FRegex.Expression := reg;
    FRegexMatchStrings.Clear;
    Assert(FRegex.Expression <> EmptyStr);
    if FRegex.Exec(aLabel) then
    begin
      case FTipDatePosition of
        tdpBegin, tdpFirst:
          begin
            temp := FRegex.Match[0];
          end;
        tdpEnd, tdpLast:
          begin
            FindRegexMatches(MaxInt);
            temp := FRegexMatchStrings[FRegexMatchStrings.Count - 1];
          end;
        tdpSecond:
          begin
            FindRegexMatches(2);
            if FRegexMatchStrings.Count = 2 then
              temp := FRegexMatchStrings[1];
          end;
        tdpThird:
          begin
            FindRegexMatches(3);
            if FRegexMatchStrings.Count = 3 then
              temp := FRegexMatchStrings[2];
          end;
        tdpThirdToLast:
          begin
            FindRegexMatches(MaxInt);
            if FRegexMatchStrings.Count >= 3 then
              temp := FRegexMatchStrings[FRegexMatchStrings.Count - 3];
          end;
        tdpSecondToLast:
          begin
            FindRegexMatches(MaxInt);
            if FRegexMatchStrings.Count >= 3 then
              temp := FRegexMatchStrings[FRegexMatchStrings.Count - 2];
          end;
      end;
      try
        if temp <> EmptyStr then
        begin
          Result := ConvertStringToDate(temp);
          isSuccess := True;
        end;
      except
        isSuccess := False;
      end;
    end;
  end;
end;

function TTipDatesFinder.ExtractCalendarDateUsingDelimiter(const aLabel: String; var Year: Extended): Boolean;
var
  reg: String = '';
  temp: String = '';
  aDate: TDateTime;
begin
  Result := False;
  if BuildYearRegex(reg) then
  begin
    FRegex.Expression := reg;
    if GetSubstringFromDelimiter(aLabel, temp) then
    begin
      Assert(FRegex.Expression <> EmptyStr);
      if FRegex.Exec(temp) then
      begin
        temp := FRegex.Match[0];
        try
          aDate := ConvertStringToDate(temp);
          Year := YearOf(aDate)*1.0;
          Result := True;
        except
          Result := False;
        end;
      end;
    end;
  end;
end;

function TTipDatesFinder.ExtractCalendarDateUsingPosition(const aLabel: String; var Year: Extended): Boolean;
var
  temp: String;
  aDate: TDateTime;
  reg: String = '';
begin
  Result := False;
  if BuildYearRegex(reg) then
  begin
    FRegex.Expression := reg;
    Assert(FRegex.Expression <> EmptyStr);
    if FRegex.Exec(aLabel) then
    begin
      temp := FRegex.Match[0];
      try
        aDate := ConvertStringToDate(temp);
        Year := YearOf(aDate)*1.0;
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
end;

function TTipDatesFinder.ExtractYearUsingDelimiter(const aLabel: String; var Year: Extended): Boolean;
var
  str: String = '';
  temp: Extended = 0;
begin
  Result := False;
  if IsCalendarDate then
  begin
    Result := ExtractCalendarDateUsingDelimiter(aLabel, temp);
    if Result then
      Year := Temp;
  end
  else
  begin
    if GetSubstringFromDelimiter(aLabel, str) then
    begin
      str := Copy(Trim(str), 1, FDigits);
      if TryStrToFloat(str, temp) then
      begin
        if FDigits = 2 then
          temp := Convert2DigitDate(temp);
        Result := True;
        Year := temp;
      end;
    end;
  end;
end;

function TTipDatesFinder.ExtractYearUsingPosition(const aLabel: String; var Year: Extended): Boolean;
var
  temp: Extended = 0.0;
begin
  Result := False;
  if (FDigits = 4) then
  begin
    case FTipDatePosition of
      tdpBegin: Result := ExtractYearFromFirstFour(aLabel, Year);
      tdpEnd: Result := ExtractYearFromLastFour(aLabel, Year);
      tdpFirst:
        begin
          Result := FindFirstNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpSecond:
        begin
          Result := FindSecondNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpThird:
        begin
          Result := FindThirdNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpThirdToLast:
        begin
          Result := FindThirdToLastNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpSecondToLast:
        begin
          Result := FindSecondToLastNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpLast:
        begin
          Result := FindLastNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
    end;
  end
  else
  begin
    case FTipDatePosition of
      tdpBegin: Result := ExtractyearFromFirstTwo(aLabel, Year);
      tdpEnd: Result := ExtractYearFromLastTwo(aLabel, Year);
      tdpFirst:
        begin
          Result := FindFirstNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpSecond:
        begin
          Result := FindSecondNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpThird:
        begin
          Result := FindThirdNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpThirdToLast:
        begin
          Result := FindThirdToLastNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpSecondToLast:
        begin
          Result := FindSecondToLastNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
      tdpLast:
        begin
          Result := FindLastNumberInLabel(aLabel, temp);
          if Result then
            Year := temp;
        end;
    end;
  end;
end;

function TTipDatesFinder.PopulateCalibrations(var Calibrations: TCalibrations): Boolean;
begin
  Result := False;
  Assert(False, 'not implemented');
end;

function TTipDatesFinder.FindFirstNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
var
  str: String;
  tempFloat: Extended;
begin
  Result := False;
  FRegex.Expression := NUMBER_REGEX;
  if FRegex.Exec(aLabel) then
  begin
    str := Trim(FRegex.Match[0]);
    if Length(str) < FDigits then
    begin
      while FRegex.ExecNext do
      begin
        str := Trim(FRegex.Match[0]);
        if Length(str) >= FDigits then
          break;
      end;
    end;
    if Length(str) > FDigits then
      str := Copy(str,1,FDigits);
    if TryStrToFloat(str, tempFloat) then
    begin
      if Length(str) = 2 then
        tempFloat := Convert2DigitDate(tempFloat);
      Result := True;
      aNum := tempFloat
    end;
  end;
end;

function TTipDatesFinder.FindSecondNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
var
  str: String = '';
  tempFloat: Extended;
begin
  Result := False;
  FRegex.Expression := NUMBER_REGEX;
  if FRegex.Exec(aLabel) and FRegex.ExecNext then
  begin

    str := Trim(FRegex.Match[0]);
    if Length(str) >= FDigits then
    begin
      str := Copy(str,1,FDigits);
      if TryStrToFloat(str, tempFloat) then
      begin
        if Length(str) = 2 then
          tempFloat := Convert2DigitDate(tempFloat);
        Result := True;
        aNum := tempFloat
      end;
    end;
  end;
end;

function TTipDatesFinder.FindThirdNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
var
  str: String = '';
  tempFloat: Extended;
begin
  Result := False;
  FRegex.Expression := NUMBER_REGEX;
  if FRegex.Exec(aLabel) and FRegex.ExecNext and FRegex.ExecNext then
  begin
    str := Trim(FRegex.Match[0]);
    if Length(str) >= FDigits then
    begin
      str := Copy(str,1,FDigits);
      if TryStrToFloat(str, tempFloat) then
      begin
        if Length(str) = 2 then
          tempFloat := Convert2DigitDate(tempFloat);
        Result := True;
        aNum := tempFloat
      end;
    end;
  end;
end;

function TTipDatesFinder.FindThirdToLastNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
var
  temp: ArrayOfExtended;
begin
  Result := False;
  temp := FindAllNumbersInLabel(aLabel);
  if Length(temp) >= 3 then
  begin
    Result := True;
    aNum := temp[Length(temp) - 3];
  end;
end;

function TTipDatesFinder.FindSecondToLastNumberInLabel(aLabel: String;var aNum: Extended): Boolean;
var
  temp: ArrayOfExtended;
begin
  Result := False;
  temp := FindAllNumbersInLabel(aLabel);
  if Length(temp) >= 2 then
  begin
    Result := True;
    aNum := temp[Length(temp) - 2];
  end;
end;

function TTipDatesFinder.FindLastNumberInLabel(aLabel: String; var aNum: Extended): Boolean;
var
  tempArray: ArrayOfExtended;
begin
  Result := False;
  tempArray := FindAllNumbersInLabel(aLabel);
  if Length(tempArray) >= 1 then
  begin
    Result := True;
    aNum := tempArray[Length(tempArray)-1];
  end;
end;

function TTipDatesFinder.FindAllNumbersInLabel(aLabel: String): ArrayOfExtended;
var
  str: String = '';
  tempFloat: Extended;
begin
  FRegex.Expression := NUMBER_REGEX;
  SetLength(Result, 0);
  if FRegex.Exec(aLabel) then
  begin
    str := Trim(FRegex.Match[0]);
    if Length(str) > FDigits then
      str := Copy(str, Length(str) - FDigits + 1, FDigits);
    if TryStrToFloat(str, tempFloat) then
    begin
      if (FDigits = 2) and (Length(str) = 2) then
        tempFloat := Convert2DigitDate(tempFloat);
      SetLength(Result, 1);
      Result[0] := tempFloat;
    end;
    while FRegex.ExecNext do
    begin
      str := Trim(FRegex.Match[0]);
      if Length(str) > FDigits then
        str := Copy(str, Length(str) - FDigits + 1, FDigits);
      if TryStrToFloat(str, tempFloat) then
      begin
        if (FDigits = 2) and (Length(str) = 2) then
          tempFloat := Convert2DigitDate(tempFloat);
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := tempFloat;
      end;
    end;
  end;
end;

function TTipDatesFinder.GetSubstringFromDelimiter(const aLabel: String; var substr: String): Boolean;
var
  index: Integer;
  str: String;
begin
  Result := False;
  index := Pos(FDelimiter, aLabel);
  if index >= 1 then
  begin
    str := Copy(aLabel, index + Length(FDelimiter), Length(aLabel));
    if Length(str) > 0 then
    begin
      Result := True;
      substr := str;
    end;
  end;
end;

function TTipDatesFinder.BuildYearRegex(var aRegex: String): Boolean;
var
  temp: String;
begin
  Result := False;
  temp := LowerCase(Trim(FDateFormat));
  temp := StringReplace(temp, 'd', '[\d]', [rfReplaceAll]);
  temp := StringReplace(temp, 'm', '[\d]', [rfReplaceAll]);
  temp := StringReplace(temp, 'y', '[\d]', [rfReplaceAll]);
  temp := StringReplace(temp, 's', '[\d]', [rfReplaceAll]);
  if Length(temp) >= 3 then
  begin
    Result := True;
    aRegex := temp;
  end;
end;

function TTipDatesFinder.Convert2DigitDate(aDate: Extended): Extended;
begin
  if Trunc(aDate) <= (YearOf(Now) - 2000) then
    Result := aDate + 2000
  else
    Result := aDate + 1900;
end;

function TTipDatesFinder.Convert2DigitDate(const aDateStr: String; var isSuccess: Boolean): Extended;
var
  tempFloat: Double;
begin
  Result := 0.0;
  if TryStrToFloat(aDateStr, tempFloat) then
  begin
    isSuccess := True;
    Result := Convert2DigitDate(tempFloat);
  end
  else
    isSuccess := False;
end;

function TTipDatesFinder.Convert4DigitDate(const aDateStr: String; var IsSuccess: Boolean): TDateTime;
var
  first: SmallInt = -1;
  year, month, day: Word;
  temp: String;
  tempResult: TDateTime;
begin
  try
    first := Pos('yyyy', LowerCase(FDateFormat));
    temp := Copy(aDateStr, first, 4);
    year := StrToInt(temp);
    month := 1;
    day := 1;
    IsSuccess := TryEncodeDateTime(year, month, day, 1, 2, 0, 0, tempResult);
    if IsSuccess then
      Result := tempResult;
  except
    on E:Exception do
      IsSuccess := False;
  end;
end;

function TTipDatesFinder.Convert6DigitDate(const aDateStr: String; var IsSuccess: Boolean): TDateTime;
var
  first: SmallInt = -1;
  year, month, day: Word;
  temp: String;
  tempResult: TDateTime;
begin
  try
    first := Pos('yyyy', LowerCase(FDateFormat));
    temp := Copy(aDateStr, first, 4);
    year := StrToInt(temp);
    first := Pos('mm', LowerCase(FDateFormat));
    temp := Copy(aDateStr, first, 2);
    month := StrToInt(temp);
    day := 1;
    IsSuccess := TryEncodeDateTime(year, month, day, 1, 2, 0, 0, tempResult);
    if IsSuccess then
      Result := tempResult;
  except
    on E:Exception do
      IsSuccess := False;
  end;
end;

function TTipDatesFinder.Convert8DigitDate(const aDateStr: String; var IsSuccess: Boolean): TDateTime;
var
  first: SmallInt = -1;
  year, month, day: Word;
  temp: String;
  tempResult: TDateTime;
begin
  try
    first := Pos('yyyy', LowerCase(FDateFormat));
    temp := Copy(aDateStr, first, 4);
    year := StrToInt(temp);
    first := Pos('mm', LowerCase(FDateFormat));
    temp := Copy(aDateStr, first, 2);
    month := StrToInt(temp);
    first := Pos('dd', LowerCase(FDateFormat));
    temp := Copy(aDateStr, first, 2);
    day := StrToInt(temp);
    IsSuccess := TryEncodeDateTime(year, month, day, 1, 2, 0, 0, tempResult);
    if IsSuccess then
      Result := tempResult;
  except
    on E:Exception do
      IsSuccess := False;
  end;
end;

function TTipDatesFinder.ConvertStringToDate(aDateStr: String): TDateTime;
var
  fs: TFormatSettings;
  isSuccess: Boolean = False;
begin
  Result := MinDateTime;
  if ((Pos('-', aDateStr) <= 0) and (Pos('/', aDateStr) <= 0)) then
  begin
    case Length(FDateFormat) of
      2: Result := Convert2DigitDate(aDateStr, isSuccess);
      4: Result := Convert4DigitDate(aDateStr, isSuccess);
      6: Result := Convert6DigitDate(aDateStr, isSuccess);
      8: Result := Convert8DigitDate(aDateStr, isSuccess);
    end;
    if not isSuccess then
      raise EConvertError.Create('invalid date: ' + aDateStr);
  end
  else
  begin
    fs := DefaultFormatSettings;
    if Pos('-', aDateStr) > 0 then
      fs.DateSeparator := '-'
    else if Pos('/', aDateStr) > 0 then
      fs.DateSeparator := '/';
    fs.ShortDateFormat := FDateFormat;
    Result := StrToDate(aDateStr, fs);
  end;
end;

function TTipDatesFinder.FindTipDatesInNexusFile(const filename: String; var isSuccess: Boolean): TStringList;
var
  taxaLabels: TStringList = nil;
begin
  SetInputDataFile(Filename);
  IsSuccess := False;
  Result := TStringList.Create;
  FNexusFile.Filename := FInputDataFile;
  if not FNexusFile.Parse then
  begin
    FLogStrings.Add(FNexusFile.ErrorMsg);
    exit;
  end;
  if FNexusFile.NumCharacterBlocks = 1 then
    taxaLabels := FNexusFile.GetCharacterBlock(0).Labels
  else if FNexusFile.NumTaxaBlocks = 1 then
    taxaLabels := FNexusFile.GetTaxaBlock(0).TaxaLabels
  else
  begin
    FLogStrings.Add(Format('Expected either 1 taxa block or 1 characters block but found %d and %d blocks respectively', [FNexusFile.NumTaxaBlocks, FNexusFile.NumCharacterBlocks]));
    Exit;
  end;
  Result := TaxaLabelsToTipDateCalibrations(taxaLabels, isSuccess);
end;

function TTipDatesFinder.FindTipDatesInMegaFile(const Filename: String; var isSuccess: Boolean): TStringList;
var
  labels: TStringList = nil;
  begin
  try
    try
      if not FileExists(Filename) then
        raise Exception.Create('File not found: ' + Filename);
      if not ProcessInputMegaFile(filename, False, nil) then
        raise Exception.Create('failed to parse MEGA alignment file: ' + filename);
      labels := D_InputSeqData.GetTaxaNamesList;
      Result := TaxaLabelsToTipDateCalibrations(labels, isSuccess);
      {$IFNDEF VISUAL_BUILD}
      NonVisualExportAlignmentTofile;
      {$ENDIF}
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Error when searching for tip dates: ' + E.Message);
        {$ELSE}
        error_nv('Error when searching for tip dates', E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(labels) then
      labels.Free;
  end;
end;

function TTipDatesFinder.FindTipDatesInFastaFile(const Filename: String; var isSuccess: Boolean): TStringList;
var
  labels: TStringList = nil;
begin
  try
    try
      if not FileExists(Filename) then
        raise Exception.Create('File not found: ' + Filename);
      if not ProcessInputFastaFile(filename, False, nil) then
        raise Exception.Create('failed to parse MEGA alignment file: ' + filename);
      labels := D_InputSeqData.GetTaxaNamesList;
      Result := TaxaLabelsToTipDateCalibrations(labels, isSuccess);
      {$IFNDEF VISUAL_BUILD}
      NonVisualExportAlignmentTofile;
      {$ENDIF}
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Error when searching for tip dates: ' + E.Message);
        {$ELSE}
        error_nv('Error when searching for tip dates', E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(labels) then
      labels.Free;
  end;
end;

function TTipDatesFinder.TaxaLabelsToTipDateCalibrations(taxaLabels: TStringList; var isSuccess: Boolean): TStringList;
var
  i: Integer;
  aTipDate: TTipDate = nil;
  aLabel: String;
  aYear: Word = 0;
  aMonth: Word = 0;
  aDay: Word = 0;
  aInfo: TOtuInfo = nil;
begin
  Result := TStringList.Create;
  Assert(taxaLabels.Count = D_InputSeqData.OtuInfos.NoOfOtus);
  if taxaLabels.Count > 0 then
  begin
    IsSuccess := True;
    for i := 0 to taxaLabels.Count - 1 do
    begin
      aLabel := taxaLabels[i];
      if ExtractTimeInfoFromLabel(aLabel, aYear, aMonth, aDay) then
      begin
        aTipDate := TTipDate.Create(aLabel, aYear, aMonth, aDay);
        FTipDates.Add(aTipDate);
        Result.Add(aTipDate.CalibrationString);
        aInfo := D_InputSeqData.OtuInfos[i];
        aInfo.Year := aYear;
        aInfo.Month := IntToMonth(aMonth);
        aInfo.Day := aDay;
      end
      else
      begin
        IsSuccess := False;
        FLogStrings.Add('failed to extract data from taxon label: ' + aLabel);
      end;
    end;
  end
  else
  begin
    {$IFNDEF VISUAL_BUILD}
    error_nv('no taxa labels found');
    {$ENDIF}
  end;
end;

function TTipDatesFinder.ExportActiveAlignmentToFile(const filename: String; const aFormat: TMegaExportFormat): Boolean;
var
  options: TSeqExportOptions = nil;
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    options := TSeqExportOptions.Create;
    options.ExportFormat := aFormat;
    options.isNucData := not D_InputSeqData.IsAmino;
    if D_InputSeqData.IdenSym = #0 then
      D_InputSeqData.IdenSym := '.';
    if D_InputSeqData.GapSym = #0 then
      D_InputSeqData.GapSym := '-';
    if D_InputSeqData.MissSym = #0 then
      D_InputSeqData.MissSym := '?';
    Result := D_InputSeqData.WriteExportDataToFile(options, filename, aList, nil);
  finally
    if Assigned(options) then
      options.Free;
    if Assigned(aList) then
      aList.Free;
  end;
end;

{$IFNDEF VISUAL_BUILD}
function TTipDatesFinder.NonVisualExportAlignmentTofile: Boolean;
var
  outfile: String;
begin
  Result := False;
  if D_MegaMain.OutputFormat = ExportFasta then
  begin
    outfile := NextAvailableFilenameNV('_tip-dates.fas');
    Result := ExportActiveAlignmentToFile(outfile, mefFasta);
  end
  else
  begin
    outfile := NextAvailableFilenameNV('_tip-dates.meg');
    Result := ExportActiveAlignmentToFile(outfile, mefMega);
  end;
end;
{$ENDIF}

constructor TTipDatesFinder.Create;
begin
  FRegexMatchStrings := TStringList.Create;
  FNexusFile := TNexusFile.Create;
  FLogStrings := TStringList.Create;
  FTipDates := TTipDateList.Create;
  F4DigitStrings := TStringList.Create;
  F2DigitStrings := TStringList.Create;
  FEarliestValidYear := MaxInt*-1;
  FLatestValidYear := YearOf(Now);
  FParseRule := tdprAny4Or2;
  FRegex := TRegExpr.Create;
  FDateFormat := DEFAULT_DATE_FORMAT;
end;

destructor TTipDatesFinder.Destroy;
begin
  if Assigned(FRegexMatchStrings) then
    FRegexMatchStrings.Free;
  if Assigned(FNexusFile) then
    FNexusFile.Free;
  if Assigned(FLogStrings) then
    FLogStrings.Free;
  if Assigned(F4DigitStrings) then
    F4DigitStrings.Free;
  if Assigned(F2DigitStrings) then
    F2DigitStrings.Free;
  if Assigned(FRegex) then
    FRegex.Free;
  Clear;
  if Assigned(FTipDates) then
    FTipDates.Free;
  inherited Destroy;
end;

function TTipDatesFinder.FindTipDates(const Filename: String; var IsSuccess: Boolean): TStringList;
var
  fileExt: String;
begin
  SetInputDataFile(Filename);
  IsSuccess := False;
  fileExt := ExtractFileExt(filename);
  if Pos(fileExt, MegaExts) > 0 then
    Result := FindTipDatesInMegaFile(filename, isSuccess)
  else if Pos(fileExt, PaupExts) > 0 then
    Result := FindTipDatesInNexusFile(filename, isSuccess)
  else if Pos(fileExt, FastaExts) > 0 then
    Result := FindTipDatesInFastaFile(filename, isSuccess)
  else
    raise Exception.Create('unrecognized file format: ' + fileExt);
end;

function TTipDatesFinder.FindTipDates(TaxaNames: TStringList; var IsSuccess: Boolean): TStringList;
var
  i: Integer;
  aTipDate: TTipDate;
begin
  Result := TStringList.Create;
  if TaxaNames.Count > 0 then
  begin
    IsSuccess := FindTipDates(TaxaNames);
    if IsSuccess then
    begin
      for i := 0 to FTipDates.Count - 1 do
      begin
        aTipDate := TTipDate(FTipDates[i]);
        Result.Add(aTipDate.CalibrationString);
      end;
    end
    else
      FLogStrings.Add('failed to extract data from all taxon labels');
  end
  else
  begin
    {$IFNDEF VISUAL_BUILD}
    error_nv('no taxa labels found');
    {$ENDIF}
  end;
end;

function TTipDatesFinder.FindTipDates(TaxaNames: TStringList): Boolean;
var
  i: Integer;
  aTipDate: TTipDate;
  aLabel: String;
  aYear: Word = 0;
  aMonth: Word = 0;
  aDay: Word = 0;
  updateTime: TDateTime;
begin
  updateTime := Now;
  Result := False;
  Clear;
  if TaxaNames.Count > 0 then
  begin
    Result := True;
    for i := 0 to TaxaNames.Count - 1 do
    begin
      aLabel := TaxaNames[i];
      if ExtractTimeInfoFromLabel(aLabel, aYear, aMonth, aDay) then
      begin
        aTipDate := TTipDate.Create(aLabel, aYear, aMonth, aDay);
        FTipDates.Add(aTipDate);
      end
      else
      begin
        FTipDates.Add(nil);
        Result := False;
        FLogStrings.Add('failed to extract data from taxon label: ' + aLabel);
      end;
      if Assigned(CheckCancel) and (MilliSecondsBetween(Now, updateTime) > 300) then
        if CheckCancel(Round(i/TaxaNames.Count*100), 'Search for timestamps') then
          raise EAbort.Create('user cancelled')
        else
          updateTime := Now;
    end;
  end
  else
  begin
    {$IFNDEF VISUAL_BUILD}
    error_nv('no taxa labels found');
    {$ENDIF}
  end;
end;

function TTipDatesFinder.NumberOfTipDates: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FTipDates.Count > 0 then
    for i := 0 to FTipDates.Count - 1 do
      if Assigned(FTipDates[i]) then
        inc(Result);
end;

function TTipDatesFinder.GetCalibrationStrings: TStringList;
var
  i: Integer;
  td: TTipDate = nil;
begin
  Result := TStringList.Create;
  if FTipDates.Count > 0 then
    for i := 0 to FTipDates.Count - 1 do
    begin
      td := TTipDate(FTipDates[i]);
      if Assigned(td) then
        Result.Add(td.CalibrationString);
    end;
end;

class function TTipDatesFinder.SampleTimeStrToSampleTime(aStr: String): Extended;
var
  aTime: TDateTime;
  td: TTipDate = nil;
  aYear, aMonth, aDay, h, m, s, ms: Word;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.ShortDateFormat := DEFAULT_DATE_FORMAT;
  fs.DateSeparator := '-';
  try
    aTime := StrToDate(aStr, fs);
    DecodeDateTime(aTime, aYear, aMonth, aDay, h, m, s, ms);
    td := TTipDate.Create('temp', aYear, aMonth, aDay);
    Result := td.GetSampleTime;
  finally
    if Assigned(td) then
      td.Free;
  end;
end;

procedure TTipDatesFinder.AssignTipDatesToList(aList: TList);
var
  i: Integer;
begin
  aList.Clear;
  if Assigned(FTipDates) and (FTipDates.Count > 0) then
    for i := 0 to FTipDates.Count - 1 do
      aList.Add(FTipDates[i]);
end;

procedure TTipDatesFinder.AssignTipDatesToList(aList: TTipDateList);
var
  i: Integer;
begin
  aList.Clear;
  if Assigned(FTipDates) and (FTipDates.Count > 0) then
    for i := 0 to FTipDates.Count - 1 do
      aList.Add(FTipDates[i]);
end;

end.

