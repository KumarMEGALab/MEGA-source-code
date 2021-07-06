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

unit MAnalysisSummary;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, KeywordConsts, MegaConsts, MegaVerConsts;

type

{ TAnalysisSummary }

 TAnalysisSummary = class
  const
    FirstColumnWidth = 40;
    ApplicationInfoString = LineEnding + '[Application Info]' + LineEnding;
    GeneralInfoString = LineEnding + '[General Info]' + LineEnding;
    AnalysisSettingsString = LineEnding + '[Analysis Settings]' + LineEnding;
    AnalysisStatsString = LineEnding + '[Analysis Statistics]' + LineEnding;
  private
    FBootstrapSites: LongInt;
    FMethod: String;
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
    FCaptionStrings: TStringList;
    FMessages: TStringList;
    FLikelihoodWithClock: Double;
    FLikelihoodWithoutClock: Double;
    FEstimatedMemUsage: Int64;
    function GetHeader: TStringList;
    {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
    function GetMaxHeapSize: QWord;
    {$ENDIF}{$ENDIF}
  public
    AnalysisOptions: TStringList;
    constructor Create;
    destructor Destroy; override;
    function GetDataType: String;
    function UpdateBenchmarksFile: Boolean;
    procedure WriteToFile(MyFileName: String);
    procedure AddAnalysisInfo(AInfo: TObject);
    procedure AddParsimonyInfoString(MyKey: String; MyValue: String);
    procedure AddCalculatedValue(MyKey: String; MyValue:String);
    procedure AddCaptionStrings(MyCaptionStrings: TStringList);
    procedure AddMessage(Msg: String);
    function ToStringList: TStringList;
    function StringsForInfoBox: TStringList;
    procedure AddOptionString(AOption: String);
  published
    property NumTaxa: Integer read FNumTaxa write FNumTaxa;
    property NumSites: LongInt read FNumSites write FNumSites;
    property BootstrapSites: LongInt read FBootstrapSites write FBootstrapSites;
    property AnalysisName: String read FAnalysisName write FAnalysisName;
    property DataFileName: String read FDataFileName write FDataFileName;
    property SettingsFileName: String read FSettingsFileName write FSettingsFileName;
    property CommandLineString: String read FCommandLineString write FCommandLineString;
    property Method: String read FMethod write FMethod;
    property DataType: TSnTokenCode read FDataType write FDataType;
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

implementation

uses
  {$IFDEF VISUAL_BUILD}mega_main,{$ELSE}MD_MegaMain, MegaUtils_NV, MegaErrUtils,{$ENDIF}
  MegaUtils, MTreeData, MGlobalSettings, DateUtils, StringUtils, Dialogs, MAnalysisInfo,
  mega_citation;

constructor TAnalysisSummary.Create;
begin
  FBootstrapSites := -1;
  FDataType := snNoToken;
  {$IFDEF VISUAL_BUILD}
  if Assigned(MegaForm) then { unit tests don't have MegaForm}
    FDataType := MegaForm.DataType;
  {$ENDIF}
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
  FExecutionTime := 0.0;
  FSumOfBranchLengths := -1.0;
  FParsimonyInfo := nil;
  FCalculatedValues := nil;
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

  if Assigned(FMessages) then
    FMessages.Free;

  inherited;
end;

{ TODO 1 -oglen : need to fix how we determine the data type. We can't use FDataType because it excludes snCoding }
function TAnalysisSummary.GetDataType: String;
begin
  case (FDataType) of
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

{$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
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
  MyPeakMem: Double;
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
    Result.Add(#9 + MakePrettyString('Version', FirstColumnWidth) + '= ' + VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR_STR);
    Result.Add(#9 + MakePrettyString('Build', FirstColumnWidth) + '= ' + VER_MEGA_BUILD);
    Result.Add(#9 + MakePrettyString('User Interface', FirstColumnWidth) + '= ' + USER_INTERFACE);
    Result.Add(#9 + MakePrettyString('Operating System', FirstColumnWidth) + '= ' + OPERATING_SYSTEM);
    Result.Add(GeneralInfoString);
    Result.Add(#9 + MakePrettyString('Data Type', FirstColumnWidth) + '= ' + GetDataType);
    Result.Add(#9 + MakePrettyString('No. of Taxa', FirstColumnWidth) + '= ' + Format('%8d',[FNumTaxa]));
    if FNumSites > 0 then
      Result.Add(#9 + MakePrettyString('No. of Sites', FirstColumnWidth) + '= ' + Format('%8d',[FNumSites]));
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
        MyPeakMem := GetPeakWorkingSetSizeMB;
        {$IFDEF MSWINDOWS}
        MyFloatString := FormatDoubleSafe(MyPeakMem, 3, 8);
        Result.Add(#9 + MakePrettyString('Peak Memory Used(Working Set)', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
        {$ELSE}
          {$IFNDEF VISUAL_BUILD}
          Result.Add(#9 + MakePrettyString('Max Heap Memory Used', FirstColumnWidth) + '= ' + Format('%.3f (MB)', [GetMaxHeapSize/1024/1024]));
          {$ELSE}
          Result.Add(#9 + MakePrettyString('Peak Memory Used(Working Set)', FirstColumnWidth) + '= ' + ' Not Available');
          {$ENDIF}
        {$ENDIF}
        if IsDeveloper and (FEstimatedMemUsage > 0) then
        begin
          MyFloatString := FormatDoubleSafe(FEstimatedMemUsage/1024/1024, 3, 8);
          Result.Add(#9 + MakePrettyString('Memory Required for ML', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
        end;
      end;
    finally
      // don't sweat it!
    end;

    if SumOfBranchLengths > 0 then
    begin
      MyFloatString := FormatDoubleSafe(SumOfBranchLengths, 3, 8);
      Result.Add(#9 + MakePrettyString('Sum of branch lengths', FirstColumnWidth) + '= ' + MyFloatString);
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
  Result.Add(#9 + MakePrettyString('Data Type', FirstColumnWidth) + '= ' + GetDataType);
  Result.Add(#9 + MakePrettyString('No. of Taxa', FirstColumnWidth) + '= ' + Format('%8d',[FNumTaxa]));
  if FNumSites > 0 then
    Result.Add(#9 + MakePrettyString('No. of Sites', FirstColumnWidth) + '= ' + Format('%8d',[FNumSites]));
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
  Result.Add(#9 + MakePrettyString('Start Time', FirstColumnWidth) + '= ' + DateTimeToStr(FStartTime));
  Result.Add(#9 + MakePrettyString('End Time', FirstColumnWidth) + '= ' + DateTimeToStr(FEndTime));
  MyExecutionTime := SecondSpan(FEndTime, FStartTime);
  MyFloatString := FormatDoubleSafe(MyExecutionTime, 3, 8);
  Result.Add(#9 + MakePrettyString('Execution Time', FirstColumnWidth) + '= ' + MyFloatString + ' (seconds)');
  MyPeakMem := GetPeakWorkingSetSizeMB;
  {$IFDEF MSWINDOWS}
  MyFloatString := FormatDoubleSafe(MyPeakMem, 3, 8);
  Result.Add(#9 + MakePrettyString('Peak Memory Used(Working Set)', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
  {$ELSE}
    {$IFNDEF VISUAL_BUILD}
    Result.Add(#9 + MakePrettyString('Max Heap Memory Used', FirstColumnWidth) + '= ' + Format('%.3f (MB)', [GetMaxHeapSize/1024/1024]));
    {$ELSE}
    Result.Add(#9 + MakePrettyString('Peak Memory Used(Working Set)', FirstColumnWidth) + '= ' + ' Not Available');
    {$ENDIF}
  {$ENDIF}
  if IsDeveloper and (FEstimatedMemUsage > 0) then
  begin
    MyFloatString := FormatDoubleSafe(FEstimatedMemUsage/1024/1024, 3, 8);
    Result.Add(#9 + MakePrettyString('Memory Required for ML', FirstColumnWidth) + '= ' + MyFloatString + ' (MB)');
  end;

  if SumOfBranchLengths > 0 then
  begin
    MyFloatString := FormatDoubleSafe(SumOfBranchLengths, 3, 8);
    Result.Add(#9 + MakePrettyString('Sum of branch lengths', FirstColumnWidth) + '= ' + MyFloatString);
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
begin
  if FCalculatedValues = nil then
    FCalculatedValues := TStringList.Create;
  FCalculatedValues.Add(MakePrettyString(MyKey, FirstColumnWidth) + '= ' + MyValue);
end;

procedure TAnalysisSummary.AddAnalysisInfo(AInfo: TObject);
var
  {$IFNDEF VISUAL_BUILD}
  i: Integer;
  MyKey: String;
  MyValue: String;
  {$ENDIF}
  MyTreeData: TTreeData;
  MyInfo: TAnalysisInfo;
begin
  FEndTime := Now;
  MyInfo := TAnalysisInfo(AInfo);
  if not (FNumTaxa > 0) then
    FNumTaxa := MyInfo.MyNoOfSeqs;
  try
    FNumSites := MyInfo.MyNoOfSites;
  except
    on E:Exception do
    {$IFDEF DEBUG}
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Error in TAnalysisSummary.AddAnalysisInfo: ' + E.Message);
      {$ELSE}
      warn_nv('Error in TAnalysisSummary.AddAnalysisInfo: ' + E.Message);
      {$ENDIF}
    {$ENDIF}
  end;
  {$IFNDEF VISUAL_BUILD}
  for i := 0 to MyInfo.MyProcessPack.TextualSettingsList.Count - 1 do
  begin
    MyKey := MyInfo.MyProcessPack.TextualSettingsList.Names[i];
    MyValue := Myinfo.MyProcessPack.TextualSettingsList.Values[MyKey];
    if not (Pos('====', MyValue) > 0) then
      AnalysisOptions.Add(MakePrettyString(MyKey, FirstColumnWidth) + '=' + MyValue);
  end;
  {$ENDIF}
  if (MyInfo.MyOriTreeList <> nil) and (MyInfo.MyOriTreeList.Count > 0) then
  begin
    MyTreeData := MyInfo.MyOriTreeList[0];
    if MyTreeData.isBLen then
      SumOfBranchLengths := MyTreeData.SBL;
  end;
end;

procedure TAnalysisSummary.WriteToFile(MyFileName: String);
var
  MyList: TStringList;
  i: Integer;
  AppName: String;
begin
  FEndTime := Now;
    {$IFDEF RELTIME_ONLY}
      {$IFDEF WIN64}
      AppName := 'RelTime-64';
      {$ELSE}
      AppName := 'RelTime-32';
      {$ENDIF}
    {$ELSE}
    AppName := 'MEGA-CC';
    {$ENDIF}
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
    if (Pos('-', AOption) > 0) and ((Pos('Neighbor-joining', AOption) <= 0) and (Pos('non-coding', LowerCase(AOption)) <= 0)) then
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

end.
