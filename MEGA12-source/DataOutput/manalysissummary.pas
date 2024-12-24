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

unit MAnalysisSummary;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, KeywordConsts, MegaConsts, MegaVerConsts,
  MUsageStatistics;

type

{ TAnalysisSummary }

 TAnalysisSummary = class(TObject)
  const
    FirstColumnWidth = 40;
    ApplicationInfoString = LineEnding + APPLICATION_INFO + LineEnding;
    TechnicalInfoString = LineEnding + TECHNICAL_INFO + LineEnding;
    GeneralInfoString = LineEnding + GENERAL_INFO + LineEnding;
    AnalysisSettingsString = LineEnding + ANALYSIS_SETTINGS + LineEnding;
    AnalysisStatsString = LineEnding + ANALYSIS_STATS + LineEnding;
  private
    FBootstrapSites: LongInt;
    FDataSourceIsExternal: Boolean;
    FMethod: String;
    FNumSitePatterns: Int64;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FNumTaxa: Integer;
    FNumSites: LongInt;
    FAnalysisName: String;
    FDataFileName: String;
    FSettingsFileName: String;
    FCommandLineString: String;
    FDataType: TSnTokenCode;
    FExecutionTime: Double; // in seconds.milliseconds
    FModelInfoList: TStringList;
    FModelInfoListWithClock: TStringList;
    FSumOfBranchLengths: Double;
    FParsimonyInfo: TStringList;
    FCalculatedValues: TStringList;
    FCalculatedValueKeys: TStringList;
    FCaptionStrings: TStringList;
    FMessages: TStringList;
    FLikelihoodWithClock: Double;
    FLikelihoodWithoutClock: Double;
    FEstimatedMemUsage: Int64;
    function GetHeader: TStringList;
    procedure SetDataType(AValue: TSnTokenCode);
    {$IFNDEF VISUAL_BUILD}{$IFDEF LINUX}
    function GetMaxHeapSize: QWord;
    {$ENDIF}{$ENDIF}
  public
    AnalysisOptions: TStringList;
    constructor Create;
    destructor Destroy; override;

    function GetDataType: String;
    function UpdateBenchmarksFile: Boolean;
    procedure WriteToFile(MyFileName: String; ExportJson: Boolean = True);
    procedure AddAnalysisInfo(AInfo: TObject);
    procedure AddParsimonyInfoString(MyKey: String; MyValue: String);
    procedure AddCalculatedValue(MyKey: String; MyValue:String);
    function RemoveCalculatedValue(MyKey: String): Boolean;
    function UpdateCalculatedValueName(MyKey: String; newKeyName: String): Boolean;
    function HasCalculatedValue(MyKey: String): Boolean;
    function CalculatedValueIndex(MyKey: String): Integer;
    procedure AddCaptionStrings(MyCaptionStrings: TStringList);
    procedure AddMessage(Msg: String);
    function ToStringList: TStringList;
    function StringsForInfoBox: TStringList;
    procedure AddOptionString(AOption: String);
    function DebugCalculatedValuesFileDump(filename: String): Boolean;
    function GetUsageDataAsJsonString(formatted: Boolean = False): String;
  published
    property DataSourceIsExternal: Boolean read FDataSourceIsExternal write FDataSourceIsExternal;
    property NumTaxa: Integer read FNumTaxa write FNumTaxa;
    property NumSites: LongInt read FNumSites write FNumSites;
    property NumSitePatterns: Int64 read FNumSitePatterns write FNumSitePatterns;
    property BootstrapSites: LongInt read FBootstrapSites write FBootstrapSites;
    property AnalysisName: String read FAnalysisName write FAnalysisName;
    property DataFileName: String read FDataFileName write FDataFileName;
    property SettingsFileName: String read FSettingsFileName write FSettingsFileName;
    property CommandLineString: String read FCommandLineString write FCommandLineString;
    property Method: String read FMethod write FMethod;
    property DataType: TSnTokenCode read FDataType write SetDataType;
    property ExecutionTime: Double read FExecutionTime write FExecutionTime;
    property ModelInfoList: TStringList read FModelInfoList write FModelInfoList;
    property ModelInfoListWithClock: TStringList read FModelInfoListWithClock write FModelInfoListWithClock;
    property SumOfBranchLengths: Double read FSumOfBranchLengths write FSumOfBranchLengths;
    property ParsimonyInfo: TStringList read FParsimonyInfo write FParsimonyInfo;
    property CalculatedValues: TStringList read FCalculatedValues write FCalculatedValues;
    property CaptionStrings: TStringList read FCaptionStrings write FCaptionStrings;
    property StartTime: TDateTime read FStartTime write FStartTime;
    property EndTime: TDateTime read FEndTime write FEndTime;
    property LikelihoodWithClock: Double read FLikelihoodWithClock write FLikelihoodWithClock;
    property LikelihoodWithOutClock: Double read FLikelihoodWithOutClock write FLikelihoodWithOutClock;
    property EstimatedMemUsage: Int64 read FEstimatedMemUsage write FEstimatedMemUsage;
end;

procedure UploadUsageData(aSummary: TAnalysisSummary);
procedure UploadUsageDataForSimpleCommand(commandName: String; dataType: TSnTokenCode; settings: TStringList = nil); { this is for calculations that have no options, such as nucleotide composition}
function GetDataTypeString(aType: TSnTokenCode): String;

implementation

uses
  {$IFDEF DEBUG}mdeveloper_console,{$ENDIF}
  {$IFDEF VISUAL_BUILD}mega_main,{$ELSE}MD_MegaMain, MegaUtils_NV, MegaErrUtils,{$ENDIF}
  MegaUtils, MTreeData, MGlobalSettings, DateUtils, StringUtils, Dialogs, MAnalysisInfo,
  mega_citation, MegaAnalysisPrefStrings, mdistpack, math, mmemutils, fpjson, MD_InputDistData,
  MD_InputSeqData;

procedure UploadUsageData(aSummary: TAnalysisSummary);
var
  json: String = '';
  reporter: TStatisticsReporter = nil;
begin
  try
     if Assigned(aSummary) then
    begin
      json := aSummary.GetUsageDataAsJsonString;
      if json <> EmptyStr then
      begin
        //{$IFDEF DEBUG}
        //WriteToDevConsole(json);
        //{$ENDIF}
        reporter := TStatisticsReporter.Create(True);
        reporter.UsageStatistics := json;
        reporter.Start;
      end;
    end;
  except
    on E: Exception do
    begin
    {$IFDEF DEBUG}
     raise E;
    {$ENDIF}
    end;
  end;
end;

procedure UploadUsageDataForSimpleCommand(commandName: String; dataType: TSnTokenCode; settings: TStringList = nil);
var
  json: TJSONObject = nil;
  technicalInfo: TJSONObject = nil;
  dataInfo: TJSONObject = nil;
  settingsInfo: TJSONObject = nil;
  usageData: String = '';
  reporter: TStatisticsReporter = nil;
  i: Integer = -1;
begin
  try
    try
      json := TJSONObject.Create;
      technicalInfo := GetTechnicalInfoJson;
      json.Add(TECHNICAL_INFO, technicalInfo);

      dataInfo := TJSONObject.Create;
      if dataType in [snNucleotide, snProtein, snCoding] then
      begin
        if not Assigned(D_InputSeqData) then
          raise Exception.Create('Developer error - invalid data type (sequence data)');
        dataInfo.Add(DATA_TYPE_STR, GetDataTypeString(dataType));
        dataInfo.Add(NUM_TAXA_STR, D_InputSeqData.NoOfTaxa);
        dataInfo.Add(NUM_SITES_STR, D_InputSeqData.NoOfSites);
      end
      else if dataType = snDistance then
      begin
        if not Assigned(D_InputDistData) then
          raise Exception.Create('Developer error - invalid data type (distance data)');
        dataInfo.Add(DATA_TYPE_STR, GetDataTypeString(dataType));
        dataInfo.Add(NUM_TAXA_STR, D_InputDistData.NoOfTaxa);
      end
      else
        raise Exception.Create('invalid data type specified for simple command: ' + GetDataTypeString(dataType));
      json.Add(GENERAL_INFO, dataInfo);

      settingsInfo := TJSONObject.Create;
      settingsInfo.Add(opsOperationType1, commandName);
      if Assigned(settings) then
        for i := 0 to settings.Count - 1 do
          settingsInfo.Add(settings.Names[i], settings.ValueFromIndex[i]);
      json.Add(ANALYSIS_SETTINGS, settingsInfo);

      {$IFDEF DEBUG}
      usageData := json.FormatJSON([foUseTabchar, foSkipWhiteSpaceOnlyLeading]);
      //WriteToDevConsole(usageData);
      {$ELSE}
      usageData := json.AsJSON;
      {$ENDIF}
      reporter := TStatisticsReporter.Create(True);
      reporter.UsageStatistics := usageData;
      reporter.Start;
    except
      on E: Exception do
      begin
      {$IFDEF DEBUG}
       raise E;
      {$ENDIF}
      end;
    end;
  finally
    if Assigned(json) then
      json.Free;
  end;
end;

function GetDataTypeString(aType: TSnTokenCode): String;
begin
  case (aType) of
    snNucleotide:
      Result := 'nucleotide (non-coding)';
    snCoding:
      Result := 'nucleotide (coding)';
    snProtein:
      Result := 'amino acid';
    snDistance:
      Result := 'distance matrix';
    snTree:
      Result := 'Newick tree';
    else
      Result := 'unknown';
  end;
end;

constructor TAnalysisSummary.Create;
begin
  FDataSourceIsExternal := False;
  FBootstrapSites := -1;
  FDataType := snNoToken;
  {$IFDEF VISUAL_BUILD}
  if Assigned(MegaForm) then { unit tests don't have MegaForm}
    FDataType := MegaForm.DataType;
  {$ENDIF}
  if Assigned(D_InputSeqData) then
  begin
    if D_InputSeqData.IsAmino then
      FDataType := snProtein
    else if D_InputSeqData.IsCoding then
      FDataType := snCoding
    else
      FDataType := snNucleotide;
  end
  else if Assigned(D_InputSeqData) then
    FDataType := snDistance;
  FModelInfoList := nil;
  FModelInfoListWithClock := nil;
  AnalysisOptions := TStringList.Create;
  FCaptionStrings := nil;
  FAnalysisName := EmptyStr;
  FDataFileName := EmptyStr;
  FSettingsFileName := EmptyStr;
  FCommandLineString := EmptyStr;
  FNumTaxa := 0;
  FNumSites := 0;
  FStartTime := Now;
  FEndTime := MinDateTime;
  FExecutionTime := 0.0;
  FSumOfBranchLengths := -1.0;
  FParsimonyInfo := nil;
  FCalculatedValues := TStringList.Create;
  FCalculatedValueKeys := TStringList.Create;
  FMessages := nil;
  FEstimatedMemUsage := -1;
end;

destructor TAnalysisSummary.Destroy;
begin
  if Assigned(FModelInfoList) then
    FModelInfoList.Free;

  if Assigned(FModelInfoListWithClock) then
    FModelInfoListWithClock.Free;

  if Assigned(AnalysisOptions) then
    AnalysisOptions.Free;

  if Assigned(FCaptionStrings) then
    FCaptionStrings.Free;

  if Assigned(FParsimonyInfo) then
    FParsimonyInfo.Free;

  if Assigned(FCalculatedValues) then
    FCalculatedValues.Free;

  if Assigned(FCalculatedValueKeys) then
    FCalculatedValueKeys.Free;

  if Assigned(FMessages) then
    FMessages.Free;

  inherited;
end;

{ TODO 1 -oglen : need to fix how we determine the data type. We can't use FDataType because it excludes snCoding }
function TAnalysisSummary.GetDataType: String;
begin
  Result := GetDataTypeString(FDataType);
end;

function TAnalysisSummary.UpdateBenchmarksFile: Boolean;
{$IFNDEF VISUAL_BUILD}
var
  aFile: Text;
  bmStr: String;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  Result := True;
  {$ELSE}
  if D_MegaMain.BenchMarksFile = EmptyStr then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  if not DirectoryExists(ExtractFileDir(D_MegaMain.BenchMarksFile)) then
    Exit;
  try
    AssignFile(aFile, D_MegaMain.BenchMarksFile);
    if not FileExists(D_MegaMain.BenchMarksFile) then
    begin
      Rewrite(aFile);
      bmStr := Format('run_time,peak_mem,num_taxa,num_sites,bootstrap_sites,analysis,stat_method,output_file', []);
      WriteLn(aFile, bmStr); { write the file header}
    end
    else
      Append(aFile);
    bmStr := Format('%.2f,%.2f,%d,%d,%d,%s,%s,%s', [SecondSpan(FEndTime, FStartTime), GetPeakWorkingSetSizeMB, NumTaxa, NumSites, BootstrapSites, AnalysisName, Method, ExtractFilename(NextAvailableFilenameNV(EmptyStr))]);
    WriteLn(aFile, bmStr);
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(D_MegaMain.BenchMarksFile);
  {$ENDIF}
end;


function TAnalysisSummary.GetHeader: TStringList;
begin
  Result := TStringList.Create;

  {$IFDEF VISUAL_BUILD}
    Result.Add(';   ' + VER_MEGA_MAJOR  + ' (Molecular Evolutionary Genetics Analysis)');
  {$ELSE}
    Result.Add(';   MEGA-CC (Molecular Evolutionary Genetics Analysis Compute Core)');
  {$ENDIF}
    Result.Add(';');
    Result.Add(';   Suggested Citation for '+ VER_MEGA_MAJOR + ':');
    Result.Add(';');
    Result.Add(';   ' + GUI_CITATION_AUTHORS + '.');
    Result.Add(';   ' + GUI_CITATION_TITLE + '.');
    Result.Add(Format(';   %s (%s) %s', [GUI_CITATION_JOURNAL, GUI_CITATION_YEAR, GUI_CITATION_PAGES]));
  {$IFNDEF VISUAL_BUILD}
    Result.Add(';');
    Result.Add(MEGA_CC_Citation('ini-file'));
  {$ENDIF}
end;

procedure TAnalysisSummary.SetDataType(AValue: TSnTokenCode);
begin
  if FDataType = AValue then Exit;
  FDataType := AValue;
end;

{$IFNDEF VISUAL_BUILD}{$IFDEF LINUX}
function TAnalysisSummary.GetMaxHeapSize: QWord;
begin
  try
    PeakMemoryCS.Acquire;
    Result := PeakMemoryUsedByMega;
  finally
    PeakMemoryCS.Release;
  end;
end;
{$ENDIF}{$ENDIF}

function TAnalysisSummary.ToStringList: TStringList;
var
  i: Integer;
  MyKey: String;
  MyValue: String;
  MyFloatString: String;
  MyExecutionTime: Double;
  Header: TStringList;
begin
  Header := nil;
  Result := nil;

  try
    {$IFNDEF VISUAL_BUILD}
    FEndTime := Now;
    {$ENDIF}
    Result := TStringList.Create;
    Header := GetHeader;
    Result.AddStrings(Header);
    if Assigned(FMessages) then
    begin
      Result.Add(LineEnding);
      Result.Add('; ****************************************************************************');
      Result.Add(';        Please see the important message(s) at the bottom of this file ');
      Result.Add('; ****************************************************************************');
    end;
    Result.Add(ApplicationInfoString);
    Result.Add(#9 + MakePrettyString(VERSION_STR, FirstColumnWidth) + '= ' + VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR_STR);
    Result.Add(#9 + MakePrettyString(BUILD_STR, FirstColumnWidth) + '= ' + VER_MEGA_BUILD);
    Result.Add(#9 + MakePrettyString(INTERFACE_STR, FirstColumnWidth) + '= ' + USER_INTERFACE);
    Result.Add(#9 + MakePrettyString(OS_STR, FirstColumnWidth) + '= ' + OPERATING_SYSTEM);
    Result.Add(GeneralInfoString);
    Result.Add(#9 + MakePrettyString(DATA_TYPE_STR, FirstColumnWidth) + '= ' + GetDataType);
    Result.Add(#9 + MakePrettyString(NUM_TAXA_STR, FirstColumnWidth) + '= ' + Format('%8.0n', [FNumTaxa*1.0]));
    if FNumSites > 0 then
      Result.Add(#9 + MakePrettyString(NUM_SITES_STR, FirstColumnWidth) + '= ' + Format('%8.0n', [FNumSites*1.0]));
    Result.Add(#9 + MakePrettyString('Data File', FirstColumnWidth) + '= ' + FDataFileName);
    {$IFNDEF VISUAL_BUILD}
    Result.Add(#9 + MakePrettyString('Settings File', FirstColumnWidth) + '= ' + FSettingsFileName);
    Result.Add(#9 + MakePrettyString('Command Line', FirstColumnWidth) + '= ' + FCommandLineString);
    {$ENDIF}

    if AnalysisOptions.Count > 0 then
    begin
      Result.Add(AnalysisSettingsString);
      for i :=0 to AnalysisOptions.Count - 1 do
      begin
        MyKey := Trim(AnalysisOptions.Names[i]);
        MyValue := Trim(AnalysisOptions.Values[AnalysisOptions.Names[i]]);
        if MyValue = 'Divider:Background' then
          Result.Add(#9 + MakePrettyString(MyKey, FirstColumnWidth))
        else
          Result.Add(#9 + MakePrettyString(MyKey, FirstColumnWidth) + '= ' + MyValue);
      end;
    end;

    Result.Add(AnalysisStatsString);
    try
      begin
        Result.Add(#9 + MakePrettyString('Start Time', FirstColumnWidth) + '= ' + DateTimeToStr(FStartTime));
        Result.Add(#9 + MakePrettyString('End Time', FirstColumnWidth) + '= ' + DateTimeToStr(FEndTime));
        MyExecutionTime := SecondSpan(FEndTime, FStartTime);
        MyFloatString := FormatDoubleSafe(MyExecutionTime, 3, 8);
        Result.Add(#9 + MakePrettyString('Execution Time', FirstColumnWidth) + '= ' + MyFloatString + ' (seconds)');
        {$IFNDEF VISUAL_BUILD}
          {$IFDEF LINUX}
	    Result.Add(#9 + MakePrettyString('Max Heap Memory Used', FirstColumnWidth) + '= ' + Format('%.3f (MB)', [GetMaxHeapSize/1024/1024]));
	  {$ELSE}
          MyFloatString := FormatDoubleSafe(GetPeakWorkingSetSizeMB, 3, 8);
          Result.Add(#9 + MakePrettyString('Peak Memory Used(Working Set)', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
	  {$ENDIF}
        {$ENDIF}

        if IsDeveloper and (FEstimatedMemUsage > 0) then
        begin
          MyFloatString := FormatDoubleSafe(FEstimatedMemUsage/1024/1024, 3, 8);
          Result.Add(#9 + MakePrettyString('Memory Required for ML', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
        end;
      end;
    except
      // don't sweat it!
    end;

    if FCalculatedValues <> nil then
    begin
      for i := 0 to FCalculatedValues.Count - 1 do
        Result.Add(#9 + FCalculatedValues[i]);
    end;

    if SumOfBranchLengths > 0 then
    begin
      MyFloatString := FormatDoubleSafe(SumOfBranchLengths, 3, 8);
      Result.Add(#9 + MakePrettyString(SUM_OF_BLENS, FirstColumnWidth) + '= ' + MyFloatString);
    end;

    if FModelInfoListWithClock <> nil then
    begin
      Result.Add('Model Information (*** With Clock ***)');
      for i := 0 to FModelInfoListWithClock.Count - 1 do
        Result.Add(#9 + FModelInfoListWithClock[i]);
      Result.Add('Model Information (*** Without Clock ***)');
    end;

    if FModelInfoList <> nil then
      for i := 0 to FModelInfoList.Count - 1 do
        Result.Add(#9 + FModelInfoList[i]);


    if FParsimonyInfo <> nil then
    begin
      for i := 0 to FParsimonyInfo.Count - 1 do
        Result.Add(#9 + FParsimonyInfo[i]);
    end;


    if FCaptionStrings <> nil then
    begin
      Result.Add(LineEnding);
      for i := 0 to FCaptionStrings.Count - 1 do
        Result.Add(FCaptionStrings[i]);
    end;

    if Assigned(FMessages) then
    begin
      Result.Add(LineEnding);
      Result.Add('; ****************************************************************************');
      Result.Add(';                 Please note the following important messages:');
      Result.Add('; ****************************************************************************');
      for i := 0 to FMessages.Count - 1 do
        Result.Add(';' + #9 + FMessages[i]);
    end;
  finally
    if Assigned(Header) then
      Header.Free;
  end;
end;

function TAnalysisSummary.StringsForInfoBox: TStringList;
var
  i: Integer;
  MyKey: String;
  MyValue: String;
  MyFloatString: String;
  MyExecutionTime: Double;
  MyPeakMem: Double;
begin
  Result := TStringList.Create;
  Result.Add('Application Information');
  Result.Add(#9 + MakePrettyString('Version', FirstColumnWidth) + '= ' + VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR_STR);
  Result.Add(#9 + MakePrettyString('Build', FirstColumnWidth) + '= ' + VER_MEGA_BUILD);
  Result.Add(#9 + MakePrettyString('User Interface', FirstColumnWidth) + '= ' + USER_INTERFACE);
  Result.Add(#9 + MakePrettyString('Operating System', FirstColumnWidth) + '= ' + OPERATING_SYSTEM);
  Result.Add('Data Information');
  if GetDataType <> 'unknown' then
    Result.Add(#9 + MakePrettyString('Data Type', FirstColumnWidth) + '= ' + GetDataType);
  Result.Add(#9 + MakePrettyString('No. of Taxa', FirstColumnWidth) + '= ' + Format('%8.0n',[FNumTaxa*1.0]));
  if FNumSites > 0 then
    Result.Add(#9 + MakePrettyString('No. of Sites', FirstColumnWidth) + '= ' + Format('%8.0n',[FNumSites*1.0]));
  if FNumSitePatterns > 0 then
    Result.Add(#9 + MakePrettyString(opsNumSitePatterns, FirstColumnWidth) + '= ' + Format('%8.0n',[FNumSitePatterns*1.0]));
  if FDataFileName <> EmptyStr then
    Result.Add(#9 + MakePrettyString('Data File', FirstColumnWidth) + '= ' + FDataFileName);
  if AnalysisOptions.Count > 0 then
  begin
    Result.Add('Analysis Settings');
    for i :=0 to AnalysisOptions.Count - 1 do
    begin
      MyKey := Trim(AnalysisOptions.Names[i]);
      MyValue := Trim(AnalysisOptions.Values[AnalysisOptions.Names[i]]);
      Result.Add(#9 + MakePrettyString(MyKey, FirstColumnWidth) + '= ' + MyValue);
    end;
  end;

  Result.Add('Analysis Statistics');
  if CompareDateTime(FEndTime,  MinDateTime) <> 0 then { for session files, FEndTime and the information below is not available}
  begin
    Result.Add(#9 + MakePrettyString('Start Time', FirstColumnWidth) + '= ' + DateTimeToStr(FStartTime));
    Result.Add(#9 + MakePrettyString('End Time', FirstColumnWidth) + '= ' + DateTimeToStr(FEndTime));
    MyExecutionTime := SecondSpan(FEndTime, FStartTime);
    MyFloatString := FormatDoubleSafe(MyExecutionTime, 3, 8);
    Result.Add(#9 + MakePrettyString('Execution Time', FirstColumnWidth) + '= ' + MyFloatString + ' (seconds)');
    {$IFNDEF VISUAL_BUILD}
      {$IFDEF LINUX}
        Result.Add(#9 + MakePrettyString('Max Heap Memory Used', FirstColumnWidth) + '= ' + Format('%.3f (MB)', [GetMaxHeapSize/1024/1024]));
      {$ELSE}
      MyPeakMem := GetPeakWorkingSetSizeMB;
      MyFloatString := FormatDoubleSafe(MyPeakMem, 3, 8);
      Result.Add(#9 + MakePrettyString('Peak Memory Used(Working Set)', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
      {$ENDIF}
    {$ENDIF}
  end;

  if IsDeveloper and (FEstimatedMemUsage > 0) then
  begin
    MyFloatString := FormatDoubleSafe(FEstimatedMemUsage/1024/1024, 3, 8);
    Result.Add(#9 + MakePrettyString('Memory Required for ML', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
  end;

  if SumOfBranchLengths > 0 then
  begin
    MyFloatString := FormatDoubleSafe(SumOfBranchLengths, 3, 8);
    Result.Add(#9 + MakePrettyString(SUM_OF_BLENS, FirstColumnWidth) + '= ' + MyFloatString);
  end;

  if FModelInfoListWithClock <> nil then
  begin
    Result.Add('Model Information (*** With Clock ***)');
    for i := 0 to FModelInfoListWithClock.Count - 1 do
      Result.Add(#9 + FModelInfoListWithClock[i]);
    Result.Add('Model Information (*** Without Clock ***)');
  end;

  if FModelInfoList <> nil then
    for i := 0 to FModelInfoList.Count - 1 do
      Result.Add(#9 + FModelInfoList[i]);


  if FParsimonyInfo <> nil then
  begin
    for i := 0 to FParsimonyInfo.Count - 1 do
      Result.Add(#9 + FParsimonyInfo[i]);
  end;

  if FCalculatedValues <> nil then
  begin
    for i := 0 to FCalculatedValues.Count - 1 do
      Result.Add(#9 + FCalculatedValues[i]);
  end;

  for i := 0 to Result.Count - 1 do
    Result[i] := Trim(Result[i]);
end;

procedure TAnalysisSummary.AddParsimonyInfoString(MyKey: String; MyValue: String);
begin
  if FParsimonyInfo = nil then
    FParsimonyInfo := TStringList.Create;
  FParsimonyInfo.Add(MakePrettyString(MyKey, FirstColumnWidth) + '= ' + MyValue);
end;

procedure TAnalysisSummary.AddCalculatedValue(MyKey: String; MyValue:String);
var
  str: String = '';
  index: Integer = -1;
begin
  if not Assigned(FCalculatedValues) then
    FCalculatedValues := TStringList.Create;
  if not Assigned(FCalculatedValueKeys) then
    FCalculatedValueKeys := TStringList.Create;
  str := MakePrettyString(PutBackDisallowedChars(MyKey), FirstColumnWidth) + '= ' + MyValue;
  index := FCalculatedValueKeys.IndexOf(MyKey);
  if index < 0 then
  begin
    FCalculatedValues.Add(str);
    FCalculatedValueKeys.Add(MyKey);
  end
  else
    FCalculatedValues[index] := str;
end;

function TAnalysisSummary.RemoveCalculatedValue(MyKey: String): Boolean;
var
  index: Integer = -1;
begin
  index := FCalculatedValueKeys.IndexOf(MyKey);
  if index >= 0 then
  begin
    Result := True;
    FCalculatedValues.Delete(index);
  end
  else
    Result := False;
end;

function TAnalysisSummary.UpdateCalculatedValueName(MyKey: String; newKeyName: String): Boolean;
var
  index: Integer = -1;
  str: String = '';
  myValue: String = '';
begin
  index := FCalculatedValueKeys.IndexOf(MyKey);
  if index >= 0 then
  begin
    Result := True;
    myValue := FCalculatedValues.ValueFromIndex[index];
    str := MakePrettyString(PutBackDisallowedChars(newKeyName), FirstColumnWidth) + '= ' + MyValue;
    FCalculatedValues[index] := str;
  end
  else
    Result := False;
end;

function TAnalysisSummary.HasCalculatedValue(MyKey: String): Boolean;
begin
  Result := FCalculatedValueKeys.IndexOf(MyKey) >= 0;
end;

function TAnalysisSummary.CalculatedValueIndex(MyKey: String): Integer;
begin
  Result := FCalculatedValueKeys.IndexOf(MyKey);
end;

procedure TAnalysisSummary.AddAnalysisInfo(AInfo: TObject);
var
  {$IFNDEF VISUAL_BUILD}
  i: Integer;
  MyKey: String = '';
  MyValue: String = '';
  aList: TStringList = nil;
  tempFloat: Double = -1;
  {$ENDIF}
  MyTreeData: TTreeData = nil;
  MyInfo: TAnalysisInfo = nil;
begin
  Assert(Assigned(aInfo), 'adding nil TAnalysisInfo');
  if not Assigned(aInfo) then
    Exit;
  FEndTime := Now;
  MyInfo := TAnalysisInfo(AInfo);
  if not (FNumTaxa > 0) then
    FNumTaxa := MyInfo.MyNoOfSeqs;
  if MyInfo.OrigNumSites > 0 then
    FNumSites := MyInfo.OrigNumSites
  else
    FNumSites := MyInfo.MyNoOfSites;

  {$IFNDEF VISUAL_BUILD}
  try
    aList := TStringList.Create;
    for i := 0 to MyInfo.MyProcessPack.TextualSettingsList.Count - 1 do
      aList.Add(ReplaceDisallowedChars(MyInfo.MyProcessPack.TextualSettingsList[i]));

    for i := 0 to aList.Count - 1 do
    begin
      MyKey := aList.Names[i];
      MyValue := aList.Values[MyKey];
      if Trim(MyValue) = EmptyStr then
        continue;

      if (MyKey = opsPercentSitesPerSample) and MyInfo.IsSubSampling then
      begin
        if MyInfo.UseAdaptivePercentageOfSites then
          MyValue := AdaptiveStr
        else
        begin
          if TryStrToFloat(MyValue, tempFloat) then
          begin
            if CompareValue(tempFloat, 1.0, FP_CUTOFF) <= 0 then
            begin
              tempFloat := tempFloat*100;
              MyValue := Format('%.0f', [tempFloat]);
            end;
          end;
        end;
      end;
      if MyInfo.IsSubSampling then
      begin
        if (MyKey = opsBootstrapSubSamples) and MyInfo.UseAdaptiveNumSamples then
          MyValue := AdaptiveStr;
        if (MyKey = opsBootstrapRepsPerSample) and MyInfo.UseAdaptiveRepsPerSample then
          MyValue := AdaptiveStr;
      end;

      if not (Pos('====', MyValue) > 0) then
      begin
        MyKey := MakePrettyString(PutBackDisallowedChars(MyKey), FirstColumnWidth);
        if AnalysisOptions.IndexOfName(MyKey) < 0 then
          AnalysisOptions.Add(MyKey + '=' + MyValue)
        else
          AnalysisOptions.Values[MyKey] := MyValue;
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
  {$ENDIF}
  if (MyInfo.MyOriTreeList <> nil) and (MyInfo.MyOriTreeList.Count > 0) then
  begin
    if MyInfo.IsSubsampling then
      MyTreeData := MyInfo.MyOriTreeList[MyInfo.MyOriTreeList.Count - 1]
    else
      MyTreeData := MyInfo.MyOriTreeList[0];
    if MyTreeData.isBLen and (not (MyInfo.MyUsrOperation in [dtdoMLModelTamer, dtdoLbsGamma, dtdoLbsTsTvBias])) then
      SumOfBranchLengths := MyTreeData.SBL;
  end;
end;

procedure TAnalysisSummary.WriteToFile(MyFileName: String; ExportJson: Boolean = True);
var
  MyList: TStringList;
  i: Integer;
  AppName: String;
begin
  FEndTime := Now;
  AppName := 'MEGA-CC';
  if not UpdateBenchmarksFile then
    AddMessage('failed to save benchmark data');
  if not RunVerbose then
  begin
    if Assigned(FMessages) then
    begin
      MyList := TStringList.Create;
      MyList.Add(AppName + ' produced the following important messages');
      for i := 0 to FMessages.Count - 1 do
      begin
        MyList.Add(IntToStr(i + 1) + '. ' + FMessages[i]);
      end;
      MyList.SaveToFile(MyFileName);
      MyList.Free;
    end;
    Exit;
  end;

  try
    MyList := ToStringList;
    MyList.SaveToFile(MyFileName);

    {$IFNDEF VISUAL_BUILD}
    if ExportUsageData and ExportJson then
    begin
      MyList.Clear;
      MyList.Add(GetUsageDataAsJsonString(True));
      MyList.SaveToFile(ChangeFileExt(MyFileName, '.json'));
    end;
    {$ENDIF}

  finally
    FreeAndNil(MyList);
  end;
end;

procedure TAnalysisSummary.AddCaptionStrings(MyCaptionStrings: TStringList);
var
  i: Integer;
begin
  if FCaptionStrings = nil then
    begin
    FCaptionStrings := TStringList.Create;
    FCaptionStrings.Add('; Note: The caption below pertains to the results generated for this analysis.');
    FCaptionStrings.Add(';');
    FCaptionStrings.Add('; Caption:');
    FCaptionStrings.Add(';');
    end;
  for i := 0 to MyCaptionStrings.Count - 1 do
    FCaptionStrings.Add('; ' + MyCaptionStrings[i]);
end;


procedure TAnalysisSummary.AddMessage(Msg: String);
begin
  if not Assigned(FMessages) then
    FMessages := TStringList.Create;

  if FMessages.IndexOf(Msg) = -1 then
    FMessages.Add(Msg);
end;

procedure TAnalysisSummary.AddOptionString(AOption: String);
var
  AList: TStringList;
begin
  AList := nil;
  try
    if (Pos('-', AOption) > 0) and
       (Pos('Neighbor-joining', AOption) <= 0) and
       (Pos('non-coding', LowerCase(AOption)) <= 0) and
       (Pos('sub-sample', LowerCase(AOption)) <= 0) and
       (Pos('tamura-nei', LowerCase(AOption)) <= 0) and
       (Pos('tree-bisection-reconnection', LowerCase(AOption)) <= 0) and
       (Pos('subtree-pruning', LowerCase(AOption)) <= 0) and
       (Pos('min-mini', LowerCase(AOption)) <= 0) and
       (Pos('max-mini', LowerCase(AOption)) <= 0) and
       (Pos('user-defined', LowerCase(AOption)) <= 0) then
      AList := SplitOnSingleChar(AOption, '-', True)
    else
      AList := SplitOnSingleChar(AOption, '=', True);
    if Assigned(AList) and (AList.Count = 2) then
      AnalysisOptions.Add(AList[0] + '=' + AList[1]);
  finally
    if Assigned(AList) then
      AList.Free;
  end;
end;

function TAnalysisSummary.DebugCalculatedValuesFileDump(filename: String): Boolean;
var
  i: Integer;
  aFile: TextFile;
  fStr: String = '';
begin
  Result := False;
  try
    fStr := Format('%%.%ds: %%s', [FirstColumnWidth]);
    AssignFile(aFile, filename);
    Rewrite(aFile);
    if FCalculatedValueKeys.Count  <> FCalculatedValues.Count then
      raise Exception.Create(Format('calculated values not updated properly. Got %d keys but %d values', [FCalculatedValueKeys.Count, FCalculatedValues.Count]));
    if FCalculatedValueKeys.Count > 0 then
      for i := 0 to FCalculatedValueKeys.Count - 1 do
        WriteLn(aFile, Format(fStr, [FCalculatedValueKeys[i], FCalculatedValues[i]]));
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TAnalysisSummary.GetUsageDataAsJsonString(formatted: Boolean = False): String;
var
  json: TJSONObject = nil;
  technicalInfo: TJSONObject = nil;
  dataInfo: TJSONObject = nil;
  settingsInfo: TJSONObject = nil;
  statsInfo: TJSONObject = nil;
  i: Integer = -1;
  MyKey: String = '';
  MyValue: String = '';
  MyExecutionTime: Double = 0;
  MyPeakMem: Double = 0;
begin
  Result := EmptyStr;
  try
    json := TJSONObject.Create;

    technicalInfo := GetTechnicalInfoJson;
    json.Add(TECHNICAL_INFO, technicalInfo);

    dataInfo := TJSONObject.Create;
    dataInfo.Add(DATA_TYPE_STR, GetDataType);
    if FDataType in [snNucleotide, snProtein, snCoding] then
    begin
      if Assigned(D_InputSeqData) and (not DataSourceIsExternal) then
      begin
        if (D_InputSeqData.OtuInfos.NoOfSelGroups > 0) then
          dataInfo.Add(NUM_GROUPS_STR, D_InputSeqData.OtuInfos.NoOfSelGroups);
        if D_InputSeqData.NoOfDomains > 1 then
          dataInfo.Add(NUM_DOMAINS_STR, D_InputSeqData.NoOfDomains);
        if D_InputSeqData.FCodeName <> EmptyStr then
          dataInfo.Add(CODE_TABLE_STR, D_InputSeqData.CodeName);
      end;
    end
    else if FDataType = snDistance then
    begin
      if (D_InputDistData.OtuInfos.NoOfSelGroups > 0) then
        dataInfo.Add(NUM_GROUPS_STR, D_InputDistData.OtuInfos.NoOfSelGroups);
    end;
    dataInfo.Add(NUM_TAXA_STR, FNumTaxa);
    if FNumSites > 0 then
      dataInfo.Add(NUM_SITES_STR, FNumSites);
    json.Add(GENERAL_INFO, dataInfo);

    settingsInfo := TJSONObject.Create;
    if AnalysisName <> EmptyStr then
      settingsInfo.Add(opsOperationType1, AnalysisName)
    else
      settingsInfo.Add(opsOperationType1, 'NOT DEFINED');
    if AnalysisOptions.Count > 0 then
    begin
      for i := 0 to AnalysisOptions.Count - 1 do
      begin
        MyKey := Trim(AnalysisOptions.Names[i]);
        if (MyKey = opsPickUserTree2) or (MyKey = opsPickStartTree2) or (MyKey = opsOperationType1) then
          continue;
        MyValue := Trim(AnalysisOptions.Values[AnalysisOptions.Names[i]]);
        if MyValue <> 'Divider:Background' then
        begin
        if settingsInfo.Find(MyKey) = nil then
          settingsInfo.Add(MyKey, MyValue);
        end;
      end;
    end;
    json.Add(ANALYSIS_SETTINGS, settingsInfo);

    statsInfo := TJSONObject.Create;
    try
      begin
        statsInfo.Add('Start Time', DateTimeToStr(FStartTime));
        if SameDate(FEndTime, MinDateTime) then
          FEndTime := Now;
        statsInfo.Add('End Time', DateTimeToStr(FEndTime));
        MyExecutionTime := SecondSpan(FEndTime, FStartTime);
        statsInfo.Add('Execution Time (seconds)', Format('%.4n', [MyExecutionTime]));
        //{$IFNDEF VISUAL_BUILD}
          {$IFDEF LINUX}
	    statsInfo.Add('Max Heap Memory Used', Format('%.3f (MB)', [GetMaxHeapSize/1024/1024]));
	  {$ELSE}
          statsInfo.Add('Peak Memory Used(Working Set - MB)', Format('%.4n', [GetPeakWorkingSetSizeMB]));
	  {$ENDIF}
        //{$ENDIF}
      end;
    except

    end;
    json.Add(ANALYSIS_STATS, statsInfo);

    {$IFDEF DEBUG}
    Result := json.FormatJSON([foUseTabchar, foSkipWhiteSpaceOnlyLeading]);
    {$ELSE}
    if formatted then
      Result := json.FormatJSON([foUseTabchar, foSkipWhiteSpaceOnlyLeading])
    else
      Result := json.AsJSON;
    {$ENDIF}
  finally
    if Assigned(json) then
      json.Free;
  end;
end;

end.
