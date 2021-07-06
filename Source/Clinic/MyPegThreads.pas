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

unit MyPegThreads;

interface

uses
  SysUtils, Classes, syncobjs;


type

  TSearchErrorMsgCall = procedure(Msg: String; AThread: TThread) of object;

  { TGeneSearchThread }

  TGeneSearchThread = class(TThread)
    private
      FIsSuccess: Boolean;
      FMsgLog: String;
      procedure DoSearchErrorMsgCall;
    public
      SearchQuery: String;
      SearchErrorMsgCall: TSearchErrorMsgCall;
      constructor Create(CreateSuspended: Boolean);
      procedure Execute; override;
      property IsSuccess: Boolean read FIsSuccess;
      property MsgLog: String read FMsgLog;
  end;

  TMyPegDownloadErrorNotify = procedure(Msg: String; AThread: TThread) of object;

  { TMyPegThread }

  TMyPegThread = class(TThread)
    private
      function ReadFileData(aFile: String): String;
      function PushToQueue(InFile: String): Boolean;
    public
      DownloadErrorNotify: TMyPegDownloadErrorNotify;
      constructor Create(CreateSuspended: Boolean);
      procedure Execute; override;
  end;

var
  MyPegCriticalSection: TCriticalSection;
  ThreadsCancelled: Boolean; // this should only be accessed inside of MyPegCriticalSection

  // ResultsList is like a message queue that lets us avoid calling synchronize
  // to update MutationExplorer UI. The problem with calling synchronize is that
  // we can no longer call waitfor when cleaning up threads if the user cancels, or we get a deadlock
  ResultsCriticalSection: TCriticalSection;
  ResultsQueue: TList; // a list of tstringlists
  QueryFiles: TStringList;

  GeneSearchCS: TCriticalSection;
  GeneQueryResult: TList; // this should only be accessed inside of GeneSearchCS. It will get just one result, a TEvoDSearchResult
  GeneSearchCancelled: Boolean; // should also only be accessed inside of GeneSearchCS
implementation

uses
  EvoD, MegaUtils, fphttpclient;


{ TGeneSearchThread }

procedure TGeneSearchThread.DoSearchErrorMsgCall;
begin
  if Assigned(SearchErrorMsgCall) then
    SearchErrorMsgCall(FMsgLog, Self);
end;

constructor TGeneSearchThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FIsSuccess := False;
  FMsgLog := EmptyStr;
end;


procedure TGeneSearchThread.Execute;
var
  ResultFilePath: String;
  Response: TEvoDSearchResult;
  doErrMsg: Boolean = False;
begin
  try
    try
      ResultFilePath := GetTEMP + 'query.xml';
      DownloadURL(SearchQuery, ResultFilePath);
      if FileExists(ResultFilePath) then
      begin
        Response := TEvodSearchResult.Create;
        Response.ParseXML(ResultFilePath);
        try
          GeneSearchCS.Acquire;
          if not GeneSearchCancelled then
            GeneQueryResult.Add(Response)
          else
            Response.Free;  // Response will be freed by the gene search dlg unless the user cancelled
        finally
          GeneSearchCS.Release;
        end;
        FIsSuccess := True;
      end
      else
      begin
        FMsgLog := 'The server did not return a result';
        Synchronize(@DoSearchErrorMsgCall);
      end;
    except
      on E: Exception do
      begin
        FIsSuccess := False;
        FMsgLog := E.Message;
        try
          GeneSearchCS.Acquire;
          doErrMsg := (not GeneSearchCancelled);
        finally
          GeneSearchCS.Release;
        end;
        if doErrMsg then
          Synchronize(@DoSearchErrorMsgCall);
      end;
    end;
  finally
    if FileExists(ResultFilePath) then
      try
        DeleteFile(ResultFilePath);
      except
        // do nothing
      end;
  end;
end;


{ TMyPegThread }

constructor TMyPegThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TMyPegThread.Execute;
var
  CurrentFile: String;
begin
  while True do
  begin
    try
      MyPegCriticalSection.Acquire;
      if ThreadsCancelled then
        Terminate
      else if QueryFiles.Count > 0 then
      begin
        CurrentFile := QueryFiles[0];
        QueryFiles.Delete(0);
      end
      else
        CurrentFile := EmptyStr;
    finally
      MyPegCriticalSection.Release;
    end;
    if (not CheckTerminated) and (CurrentFile <> EmptyStr) then
      PushToQueue(CurrentFile)
    else
      break;
  end;
end;

function TMyPegThread.ReadFileData(aFile: String): String;
var
  aList: TStringList = nil;
begin
  Result := EmptyStr;
  if not FileExists(aFile) then
    Exit;
  aList := TStringList.Create;
  try
    aList.LoadFromFile(aFile);
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TMyPegThread.PushToQueue(InFile: String): Boolean;
var
  Response : String;
  DoStop: Boolean;
  ResultsList: TStringList = nil;
  url, data: String;
  aClient: TFPHTTPClient = nil;
begin
  DoStop := False;
  try
    MyPegCriticalSection.Acquire;
    if ThreadsCancelled then
      DoStop := True;
  finally
    MyPegCriticalSection.Release;
  end;
  if DoStop then
    Exit;

  try
    try
      if not FileExists(InFile) then
        raise Exception.Create('Cache file missing: ' + InFile);
      data := ReadFileData(InFile);
      Result := Length(data) > 0;
      url := 'http://' + MYPEG_HOST + MYPEG_PATH + 'uploader.php';
      aClient := TFPHttpClient.Create(nil);
      Response := aClient.FormPost(url, data);
      try
        ResultsCriticalSection.Acquire;
        ResultsList := TStringList.Create;
        ResultsList.Text := Response;
        ResultsQueue.Add(ResultsList);
      finally
        ResultsCriticalSection.Release;
      end;
    except
      on E: Exception do
      begin
        if Assigned(DownloadErrorNotify) then
          DownloadErrorNotify(E.Message, Self);
      end;
    end;
  finally
    if FileExists(InFile) then
    begin
      try
        DeleteFile(InFile);
      except

      end;
    end;
    if Assigned(aClient) then
      aClient.Free;
  end;
end;

end.
