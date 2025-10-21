{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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

unit mtimetree_api;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, URIParser, fpjson,
  jsonparser, fphttpclient, mstringbuilder, mtimetree_map;

const
  BaseURL: String = 'http://timetree.temple.edu';
  NAME_SEARCH_REQUEST = 'name_search';
  TIME_SEARCH_REQUEST = 'mrca_time_search';
  NCBI_ID_SEARCH_REQUEST = 'ncbi_id_search';


type

  { TTimetreeApi }

  TTimetreeApi = class(TObject)
    private
      FLog: TStringList;
      FStringBuilder: TMegaStringBuilder;
      FClient: TFPHTTPClient;
      FURL : String;
      function ParseMrcaTimeRequest(const response: String; var timetreeMap: TTimeTreeMap): Boolean;
      function ParseNameRequest(const response: String; var timeTreeDataList: TTimeTreeTaxonDataList): Boolean;
      function ParseNameRequestObject(aJson: TJSONObject; var timeTreeData: TTimeTreeTaxonData): Boolean;
      function ParseNcbiIdRequest(const response: String; var timetreeData: TTimeTreeTaxonData): Boolean;
      function ParseStudyData(const aData: TJSONObject; var aStudy: TTimeTreeStudyData): Boolean;
      function EncodedString(source: String): String;
      procedure AppendLog(const aMsg: String);
    public
      constructor Create;
      destructor Destroy; override;

      function TimetreeTimeSearch(var timetreeMap: TTimeTreeMap; var aMsg: String): Boolean;
      function TimetreeNameSearch(const aName: String; var aMsg: String): TTimetreeTaxonDataList;
      function TimetreeNcbidSearch(const ncbiId: Integer; var aMsg: String): TTimetreeTaxonData;

  end;

  { TTimetreeApiThread }

  TTimetreeApiThread = class(TThread)
    private
      FErrorMsg: String;
      FIsSuccess: Boolean;
      FTimeTreeTaxonDataList: TTimeTreeTaxonDataList;
    protected
      FTimetreeMap: TTimeTreeMap;
      FQuery: String;
      FRequestType: String;
      procedure DoNameSearch;
      procedure DoTimeSearch;
      procedure DoNcbiIdSearch;
      procedure Execute; override;
      function CleanQuery(aQuery: String): String;
    public
      constructor Create;
      procedure InitializeForNameSearch(const query: String);
      procedure InitializeForTimeSearch(const timetreeMap: TTimeTreeMap);
      procedure InitializeForNcbiIdSearch(const ncbiId: String);

      property IsSuccess: Boolean read FIsSuccess write FIsSuccess;
      property ErrorMsg: String read FErrorMsg;
      property TimetreeMap: TTimeTreeMap read FTimetreeMap;
      property TimeTreeTaxonDataList: TTimeTreeTaxonDataList read FTimeTreeTaxonDataList;
      property Query: String read FQuery;
  end;

  function TimetreeTimeSearch(var timetreeMap: TTimeTreeMap; var aMsg: String): Boolean;
  function TimetreeNameSearch(const aName: String; var aMsg: String): TTimeTreeTaxonDataList;
  function TimetreeNcbiIdSearch(const ncbiId: Integer; var aMsg: String): TTimeTreeTaxonData;

implementation

uses
  MegaConsts, math;

function TimetreeTimeSearch(var timetreeMap: TTimeTreeMap; var aMsg: String): Boolean;
var
  api: TTimetreeApi = nil;
begin
  Result := False;
  try
    api := TTimetreeApi.Create;
    Result := api.TimetreeTimeSearch(timetreeMap, aMsg);
  finally
    if Assigned(api) then
      api.Free;
  end;
end;

function TimetreeNameSearch(const aName: String; var aMsg: String): TTimeTreeTaxonDataList;
var
  api: TTimetreeApi = nil;
begin
  try
    api := TTimetreeApi.Create;
    Result := api.TimetreeNameSearch(aName, aMsg);
  finally
    if Assigned(api) then
      api.Free;
  end;
end;

function TimetreeNcbiIdSearch(const ncbiId: Integer; var aMsg: String): TTimeTreeTaxonData;
var
  api: TTimetreeApi = nil;
begin
  try
    api := TTimetreeApi.Create;
    Result := api.TimetreeNcbidSearch(ncbiId, aMsg);
  finally
    if Assigned(api) then
      api.Free;
  end;
end;

{ TTimetreeApiThread }

procedure TTimetreeApiThread.DoNameSearch;
var
  aMsg: String = '';
begin
  FTimetreeTaxonDataList := TimetreeNameSearch(FQuery, aMsg);
  FIsSuccess := (Assigned(FTimetreeTaxonDataList)) and (FTimetreeTaxonDataList.Count > 0);
  if not FIsSuccess then
  begin
    if aMsg <> EmptyStr then
      FErrorMsg := aMsg
    else
      FErrorMsg := 'Timetree name search failed with an unknown error';
  end;
end;

procedure TTimetreeApiThread.DoTimeSearch;
var
  aMsg: String = '';
begin
  FIsSuccess := TimetreeTimeSearch(FTimetreeMap, aMsg);
  if not FIsSuccess then
  begin
    if aMsg <> EmptyStr then
      FErrorMsg := aMsg
    else
      FErrorMsg := 'Timetree mrca time searched failed with an unknown error';
  end;
end;

procedure TTimetreeApiThread.DoNcbiIdSearch;
var
  aMsg: String = '';
  ttData: TTimeTreeTaxonData = nil;
begin
  ttData := TimetreeNcbiIdSearch(StrToInt(FQuery), aMsg);
  FIsSuccess := Assigned(ttData);
  if FIsSuccess then
  begin
    FTimeTreeTaxonDataList := TTimeTreeTaxonDataList.Create;
    FTimeTreeTaxonDataList.Add(ttData);
  end
  else
  begin
    if aMsg <> EmptyStr then
      FErrorMsg := aMsg
    else
      FErrorMsg := 'Timetree name search failed with an unknown error';
  end;
end;

procedure TTimetreeApiThread.Execute;
begin
  try
    if FRequestType = NAME_SEARCH_REQUEST then
      DoNameSearch
    else if FRequestType = TIME_SEARCH_REQUEST then
      DoTimeSearch
    else if FRequestType = NCBI_ID_SEARCH_REQUEST then
      DoNcbiIdSearch
    else
      raise Exception.Create('invalid request type: ' + FRequestType);
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      FErrorMsg := E.Message;
    end;
  end;
end;

function TTimetreeApiThread.CleanQuery(aQuery: String): String;
begin
  Result := Trim(aQuery);
  Result := StringReplace(Result, '_', ' ', [rfReplaceAll]);
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

constructor TTimetreeApiThread.Create;
begin
  inherited Create(True);
  FRequestType := EmptyStr;
  FreeOnTerminate := True;
  FIsSuccess := False;
  FErrorMsg := EmptyStr;
  FTimeTreeTaxonDataList := nil;
  FTimetreeMap := nil;
end;

procedure TTimetreeApiThread.InitializeForNameSearch(const query: String);
begin
  FRequestType := NAME_SEARCH_REQUEST;
  FQuery := CleanQuery(query);
end;

procedure TTimetreeApiThread.InitializeForTimeSearch(const timetreeMap: TTimeTreeMap);
begin
  FRequestType := TIME_SEARCH_REQUEST;
  FTimetreeMap := timetreeMap;
end;

procedure TTimetreeApiThread.InitializeForNcbiIdSearch(const ncbiId: String);
begin
  FRequestType := NCBI_ID_SEARCH_REQUEST;
  FQuery := Trim(ncbiId);
end;

{ TTimetreeApi }

function TTimetreeApi.ParseMrcaTimeRequest(const response: String; var timetreeMap: TTimeTreeMap): Boolean;
var
  aJson : TJSONObject = nil;
  AData : TJSONData = nil;
  aStudy: TTimeTreeStudyData = nil;
  aArray: TJSONArray = nil;
  i: Integer = -1;
  author: String = '';
  title: String = '';
  year: Integer = 0;
  treeCount: Integer = 0;
  ageEstimate: String = '';
begin
  Result := False;
  AppendLog('parsing server MRCA response: ' + response);
  aJson := TJSONObject(GetJSON(Response));
  AData := aJson.Find('error', jtObject);
  if Assigned(AData) then
  begin
    aJson := TJSONObject(AData);
    AData := aJson.Find('message', jtString);
    raise Exception.Create('Error parsing age response: ' + AData.AsJSON);
  end;

  AData := aJson.Find('error', jtString);
  if Assigned(AData) then
    raise Exception.Create('Error parsing age response: ' + AData.AsString);

  AData := aJson.Find('precomputed_ci_low', jtNumber);
  if not Assigned(AData) then
    raise Exception.Create('Error parsing low CI');
  timetreeMap.MinTime := AData.AsFloat;

  AData := aJson.Find('precomputed_ci_high', jtNumber);
  if not Assigned(AData) then
    raise Exception.Create('Error parsing high CI');
  timetreeMap.MaxTime := AData.AsFloat;

  AData := aJson.Find('precomputed_age', jtNumber);
  if not Assigned(AData) then
    raise Exception.Create('Error parsing precomputed_age');
  timetreeMap.MedianTime := AData.AsFloat;

  AData := aJson.Find('taxon_id', jtNumber);
  if not Assigned(AData) then
    raise Exception.Create('missing mrca_id');
  timetreeMap.TimetreeMrcaId := AData.AsInteger;

  AData := aJson.Find('study_data', jtArray);
  if Assigned(AData) then
  begin
    if not (AData.JSONType = jtArray) then
      raise Exception.Create('invalid json - expected array but got something different');
    aArray := TJSONArray(AData);
    for i := 0 to aArray.Count - 1 do
    begin
      AData := TJSONObject(aArray.Items[i]);
      aStudy := TTimeTreeStudyData.Create;
      Result := ParseStudyData(TJSONObject(AData), aStudy);
      if Result and (CompareValue(aStudy.AgeEstimate, -1, FP_CUTOFF) <> 0) then
        timetreeMap.AddStudyData(aStudy)
      else
        FreeAndNil(aStudy);
    end;
  end;

  Result := True;
end;

function TTimetreeApi.ParseNameRequest(const response: String; var timeTreeDataList: TTimeTreeTaxonDataList): Boolean;
var
  i: Integer = -1;
  aArray: TJSONArray = nil;
  aJson : TJSONObject = nil;
  jsonData : TJSONData = nil;
  aTimetreeData: TTimeTreeTaxonData = nil;
begin
  Result := True;
  try
    AppendLog('parsing name request: ' + response);
    jsonData := GetJSON(response);

    case jsonData.JSONType of
      jtArray:
        begin
          aArray := TJSONArray(jsonData);
          for i := 0 to aArray.Count - 1 do
          begin
            aJson := TJSONObject(aArray.Items[i]);
            aTimetreeData := TTimeTreeTaxonData.Create;
            Result := ParseNameRequestObject(aJson, aTimetreeData);
            if not Result then
              raise Exception.Create('failure at ParseNameRequest');
            timeTreeDataList.Add(aTimetreeData);
          end;
        end;
      jtObject:
        begin
          aTimetreeData := TTimeTreeTaxonData.Create;
          Result := ParseNameRequestObject(TJSONObject(jsonData), aTimetreeData);
          if not Result then
            raise Exception.Create('failure at ParseNameRequest');
          timeTreeDataList.Add(aTimetreeData);
        end;
    end;

    Result := timeTreeDataList.Count > 0;
  finally
    if Assigned(jsonData) then
      jsonData.Free;
  end;
end;

function TTimetreeApi.ParseNameRequestObject(aJson: TJSONObject; var timeTreeData: TTimeTreeTaxonData): Boolean;
var
  jsonData : TJSONData = nil;
  tempStr : String = '';
  rankObj: TJSONObject = nil;
  thisObj: TJSONObject = nil;
begin
  Result := False;
  jsonData := aJson.Find('error', jtObject);
  if Assigned(jsonData) then
  begin
    aJson := TJSONObject(jsonData);
    jsonData := aJson.Find('message', jtString);
    tempStr := jsonData.AsJSON;
    raise Exception.Create(tempStr);
  end;

  jsonData := aJson.Find('ncbi_id', jtNumber);
  if not Assigned(jsonData) then
    raise Exception.Create('Error parsing ncbi_id from name response');
  timetreeData.NcbiId := jsonData.AsInteger;

  jsonData := aJson.Find('scientific_name', jtString);
  if not Assigned(jsonData) then
    raise Exception.Create('Error parsing scientific_name from name response');
  timetreeData.TimetreeName := jsonData.AsString;

  jsonData := aJson.Find('c_syn_name', jtString);
  if Assigned(jsonData) then
    timetreeData.Synonym := jsonData.AsString;

  jsonData := aJson.Find('taxonomic_rank', jtString);
  if not Assigned(jsonData) then
  begin
    jsonData := aJson.Find('rank', jtObject);
    if Assigned(jsonData) then
    begin
      rankObj := TJSONObject(jsonData);
      jsonData := rankObj.Find('this', jtObject);
      if Assigned(jsonData) then
      begin
        thisObj := TJSONObject(jsonData);
        if thisObj.Find('name', jtString) <> nil then
          timetreeData.TaxonomicRank := thisObj.Get('name', 'unknown');
      end;
    end;
  end
  else
    timetreeData.TaxonomicRank := jsonData.AsString;

  Result := True;
end;

function TTimetreeApi.ParseNcbiIdRequest(const response: String; var timetreeData: TTimeTreeTaxonData): Boolean;
var
  jsonData : TJSONData = nil;
  tempStr : String = '';
  rankObj: TJSONObject = nil;
  thisObj: TJSONObject = nil;
  aJson: TJSONObject = nil;
begin
  Result := False;
  AppendLog('parsing name request: ' + response);
  jsonData := GetJSON(response);
  if jsonData.JSONType <> jtObject then
    raise Exception.Create('unexpected JSONType');
  aJson := TJSONObject(jsonData);
  jsonData := aJson.Find('error', jtObject);
  if Assigned(jsonData) then
  begin
    aJson := TJSONObject(jsonData);
    jsonData := aJson.Find('message', jtString);
    tempStr := jsonData.AsJSON;
    raise Exception.Create(tempStr);
  end;

  jsonData := aJson.Find('ncbi_id', jtNumber);
  if not Assigned(jsonData) then
    raise Exception.Create('Error parsing ncbi_id from name response');
  timetreeData.NcbiId := jsonData.AsInteger;

  jsonData := aJson.Find('scientific_name', jtString);
  if not Assigned(jsonData) then
    raise Exception.Create('Error parsing scientific_name from response');
  timetreeData.TimetreeName := jsonData.AsString;

  jsonData := aJson.Find('c_syn_name', jtString);
  if Assigned(jsonData) then
    timetreeData.Synonym := jsonData.AsString;

  jsonData := aJson.Find('taxonomic_rank', jtString);
  if not Assigned(jsonData) then
  begin
    jsonData := aJson.Find('rank', jtObject);
    if Assigned(jsonData) then
    begin
      rankObj := TJSONObject(jsonData);
      jsonData := rankObj.Find('this', jtObject);
      if Assigned(jsonData) then
      begin
        thisObj := TJSONObject(jsonData);
        if thisObj.Find('name', jtString) <> nil then
          timetreeData.TaxonomicRank := thisObj.Get('name', 'unknown');
      end;
    end;
  end
  else
    timetreeData.TaxonomicRank := jsonData.AsString;

  Result := True;
end;

function TTimetreeApi.ParseStudyData(const aData: TJSONObject; var aStudy: TTimeTreeStudyData): Boolean;
var
  jsonData : TJSONData = nil;
begin
  Result := False;
  try
    jsonData := aData.Find('first_author', jtString);
    if not Assigned(jsonData) then
      raise Exception.Create('missing "first_author"');
    aStudy.FirstAuthor := jsonData.AsString;

    jsonData := aData.Find('title', jtString);
    if not Assigned(jsonData) then
      raise Exception.Create('missing "title"');
    aStudy.Title := jsonData.AsString;

    jsonData := aData.Find('pub_year', jtNumber);
    if not Assigned(jsonData) then
      raise Exception.Create('missing "pub_year"');
    aStudy.Year := jsonData.AsInteger;

    jsonData := aData.Find('tree_count', jtNumber);
    if not Assigned(jsonData) then
      raise Exception.Create('missing "tree_count"');
    aStudy.NumTrees := jsonData.AsInteger;

    jsonData := aData.Find('age_estimate', jtString);
    if not Assigned(jsonData) then
      raise Exception.Create('missing "age_estimate"');
    aStudy.AgeEstimate := StrToFloat(jsonData.AsString);
    Result := True;
  except
    on E:Exception do
    begin
      AppendLog(E.Message);
      Result := False;
    end;
  end;
end;

function TTimetreeApi.EncodedString(source: String): String;
begin
  Result := EncodeURI(ParseURI(source));
end;

procedure TTimetreeApi.AppendLog(const aMsg: String);
var
  s: String = '';
begin
  if FLog.Count = 0 then
    FLog.Add(Format('%-20s  Info', ['Time']));
  s := Format('%-20s  %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', Now), aMsg]);
  FLog.Add(s);

  //{$IFDEF DEBUG}
  //WriteToDevConsole(s);
  //{$ENDIF}
end;

constructor TTimetreeApi.Create;
begin
  FClient := TFPHTTPClient.Create(nil);
  FStringBuilder := TMegaStringBuilder.Create;
  FLog := TStringList.Create;
end;

destructor TTimetreeApi.Destroy;
begin
  if Assigned(FClient) then
    FClient.Free;
  if Assigned(FStringBuilder) then
    FStringBuilder.Free;
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

function TTimetreeApi.TimetreeTimeSearch(var timetreeMap: TTimeTreeMap; var aMsg: String): Boolean;
var
  aResponse: String = '';
  i: Integer = -1;
begin
  try
    Result := False;
    FStringBuilder.Clean;
    FStringBuilder.Add(BaseURL);
    FStringBuilder.Add('/api/mrca/id/');
    for i := 0 to timetreeMap.Count - 1 do
    begin
      FStringBuilder.Add(timetreeMap[i].NcbiIdStr);
      if i <> (timetreeMap.Count - 1) then
        FStringBuilder.Add('+');
    end;
    FStringBuilder.Add('/json');
    FURL := FStringBuilder.GenerateString;
    aResponse := FClient.Get(FURL);

    if not ParseMrcaTimeRequest(aResponse, timetreeMap) then
    begin
      aMsg := 'Failed to parse the name search response';
      timetreeMap.MedianTime := -1;
      timetreeMap.MinTime := -1;
      timetreeMap.MaxTime := -1;
      timetreeMap.TimetreeMrcaId := -1;
      Result := False;
    end
    else
      Result := True;
  except
    on E:Exception do
    begin
      Result := False;
      aMsg := E.Message;
    end;
  end;
end;

function TTimetreeApi.TimetreeNameSearch(const aName: String; var aMsg: String): TTimetreeTaxonDataList;
var
  aResponse: String = '';
begin
  try
    Result := nil;
    FStringBuilder.Clean;
    FStringBuilder.Add(BaseURL);
    FStringBuilder.Add('/api/names/');
    FStringBuilder.Add(EncodedString(aName));
    FURL := FStringBuilder.GenerateString;
    aResponse := FClient.Get(FURL);
    Result := TTimeTreeTaxonDataList.Create;
    if not ParseNameRequest(aResponse, Result) then
    begin
      aMsg := 'Failed to parse the name search response';
      Result.Free;
      Result := nil;
    end
  except
    on E:Exception do
    begin
      if Assigned(Result) then
        Result.Free;
      Result := nil;
      aMsg := E.Message;
    end;
  end;
end;

function TTimetreeApi.TimetreeNcbidSearch(const ncbiId: Integer; var aMsg: String): TTimetreeTaxonData;
var
  aResponse: String = '';
begin
  try
    Result := nil;
    FStringBuilder.Clean;
    FStringBuilder.Add(BaseURL);
    FStringBuilder.Add('/api/taxon/');
    FStringBuilder.Add(IntToStr(ncbiId));
    FURL := FStringBuilder.GenerateString;
    aResponse := FClient.Get(FURL);
    Result := TTimeTreeTaxonData.Create;
    if not ParseNcbiIdRequest(aResponse, Result) then
    begin
      aMsg := 'Failed to parse the name NCBI Id search response';
      Result.Free;
      Result := nil;
    end
  except
    on E:Exception do
    begin
      if Assigned(Result) then
        Result.Free;
      Result := nil;
      aMsg := E.Message;
    end;
  end;
end;

end.

