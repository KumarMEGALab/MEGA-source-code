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

unit MutationDataCache;

interface

uses
  Classes, SysUtils, RegExpr;

const
  MAX_ROWS_PER_REQUEST = 1000;
  MAX_BAD_LINES_TO_SHOW = 50;

type

  TCacheUpdateEvent = function(Progress: Integer): Boolean of object;
  TCachingCompletedEvent = procedure(FL: TStringList; HasBadLines: Boolean) of object;
  TCachingCancelledEvent = procedure(FL: TStringList) of object;

  TDataCacheThread = class(TThread)
    private
      RefSeqIdRegex: TRegExpr;
      IntRegex: TRegExpr;
      AARegex: TRegExpr;
      FTokens: TStringList;
      procedure BreakIntoFiles;
      function GetFileSizeInBytes(FileName: String): Int64;
    public
      SourceFile: String;
      CacheUpdateEvent: TCacheUpdateEvent;
      CachingCompleteEvent: TCachingCompletedEvent;
      CachingCancelledEvent: TCachingCancelledEvent;
      Progress: Integer;

      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;
      function IsValidLine(Line: String): Boolean;
      function MatchesRefSeqIdRegex(Suspect: String): Boolean;
      function MatchesIntegerRegex(Suspect: String): Boolean;
  end;
implementation

uses
  Math, StringUtils, MyPegThreads {$IFDEF VISUAL_BUILD}, Dialogs{$ENDIF};

{ TDataCacheThread }

procedure TDataCacheThread.BreakIntoFiles;
var
  SourceList: TStringList;
  FileList: TStringList;
  i: Integer;
  TempDir: String;
  TempFileName: String;
  TempNumNeeded: Integer;
  NumRequestsNeeded: Integer;
  InFile: Text;
  OutFile: Text;
  TotalSize: Int64;
  TempStr: String;
  HasBadLines: Boolean;
  BadLines: TStringList;
  CurrentLine: Integer;
  DoStop: Boolean;
begin
  DoStop := False;
  FileList := nil;
  SourceList := nil;
  NumRequestsNeeded := 0;
  Progress := 0;
  TotalSize := 0;
  HasBadLines := False;
  BadLines := TStringList.Create;
  CurrentLine := 1;

  TempDir := GetTempDir(False);
  TempFileName := ChangeFileExt(ExtractFileName(SourceFile), EmptyStr);
  if not DirectoryExists(TempDir) then
  begin
    TempDir := ExtractFilePath(SourceFile);
  end;

  FileList := TStringList.Create;
  try
    try
      TotalSize := GetFileSizeInBytes(SourceFile);
      AssignFile(InFile, SourceFile);
      Reset(InFile);

      if (TotalSize mod MAX_ROWS_PER_REQUEST) = 0 then
        TempNumNeeded := TotalSize div MAX_ROWS_PER_REQUEST
      else
        TempNumNeeded := TotalSize div MAX_ROWS_PER_REQUEST + 1;
      TempNumNeeded := Max(TempNumNeeded, 1); // to guarantee no division by zero

      while True do
      begin
        try
          i := 0;
          FileList.Add(TempDir + TempFileName + '_' + IntToStr(NumRequestsNeeded) + '.txt');
          AssignFile(OutFile, TempDir + TempFileName + '_' + IntToStr(NumRequestsNeeded) + '.txt');
          Rewrite(OutFile);

          while (i < MAX_ROWS_PER_REQUEST) and (not Eof(InFile)) do
          begin
            ReadLn(InFile, TempStr);
            if IsValidLine(TempStr) then
              WriteLn(OutFile, UpperCase(TempStr))
            else
            begin
              BadLines.Values[IntToStr(CurrentLine)] := TempStr;
              HasBadLines := True;
            end;
            inc(i);
            inc(CurrentLine);
          end;
          inc(NumRequestsNeeded);
          Progress := Min(Trunc(NumRequestsNeeded / TempNumNeeded * 100), 100);
//          CacheUpdateEvent(Progress);
        finally
          CloseFile(OutFile);
        end;

        if (Eof(InFile)) or (BadLines.Count >= MAX_BAD_LINES_TO_SHOW) then
          break;

        try
          MyPegCriticalSection.Acquire;
          if ThreadsCancelled then
            DoStop := True;
        finally
          MyPegCriticalSection.Release;
        end;
        DoStop := (not CacheUpdateEvent(Progress));
        if DoStop then
          break;
      end;

      if not DoStop then
      begin
        CacheUpdateEvent(100);
        if HasBadLines then
        begin
//          CloseFile(InFile);
          CachingCompleteEvent(BadLines, True);
        end
        else
          CachingCompleteEvent(FileList, False);
      end
      else
        CachingCancelledEvent(FileList);
    except
      on E: Exception do
      begin
        BadLines.Clear;
        BadLines.Add('Error');
        BadLines.Add(E.Message);
        HasBadLines := True;
        CachingCompleteEvent(BadLines, True);
      end;
    end;
  finally
    CloseFile(InFile);
    if HasBadLines then
    begin
      for i := 0 to FileList.Count - 1 do
        if FileExists(FileList[i]) then
        begin
          try
            TempStr := FileList[i];
            SysUtils.DeleteFile(TempStr);
          Except
            on E:Exception do
            begin
              {$IFDEF DEBUG}
              {$IFDEF VISUAL_BUILD}
               ShowMessage('Error in MutationDataCache.BreakIntoFiles: ' + E.Message);
               {$ENDIF}
              {$ENDIF}
            end;
            // don't sweat it
          end;
        end;
    end;

    if Assigned(SourceList) then
      SourceList.Free;
    if Assigned(FileList) then
      FileList.Free;
    if Assigned(BadLines) then
      BadLines.Free;
  end;
end;

constructor TDataCacheThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  RefSeqIdRegex := TRegExpr.Create;
  RefSeqIdRegex.Expression := 'NP_[\d]+$';
  RefSeqIdRegex.ModifierI := True;
  IntRegex := TRegExpr.Create;
  IntRegex.Expression := '^[\d]+$';
  AARegex := TRegExpr.Create;
  AARegex.Expression := '[ACDEFGHIKLMNPQRSTVWY]';
  FTokens := TStringList.Create;
end;

destructor TDataCacheThread.Destroy;
begin
  if Assigned(RefSeqIdRegex) then
    RefSeqIdRegex.Free;
  if Assigned(IntRegex) then
    IntRegex.Free;
  if Assigned(AARegex) then
    AARegex.Free;
  if Assigned(FTokens) then
    FTokens.Free;
  inherited;
end;

procedure TDataCacheThread.Execute;
begin
  BreakIntoFiles; // any exceptions are handled in BreakIntoFiles
end;

function TDataCacheThread.GetFileSizeInBytes(FileName: String): Int64;
var
  sr : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, sr ) = 0 then
    result := sr.Size
  else
    result := -1;
 SysUtils.FindClose(sr) ;
end;

function TDataCacheThread.IsValidLine(Line: String): Boolean;
begin
  Result := False;
  try
    if SplitOnSingleCharFaster(Line, ',', FTokens, True) then
    begin
      if FTokens.Count <> 3 then
        Exit;
      if not RefSeqIdRegex.Exec(FTokens[0]) then
        Exit;
      if not IntRegex.Exec(FTokens[1]) then
        Exit;
      if (not AARegex.Exec(FTokens[2])) or (Length(Trim(FTokens[2])) <> 1) then
        Exit;
      Result := True;
    end
  except
    Result := False;
  end;
end;

function TDataCacheThread.MatchesIntegerRegex(Suspect: String): Boolean;
begin
  Result := False;
  if not IntRegex.Exec(Suspect) then
    Exit;
  Result := True;
end;

function TDataCacheThread.MatchesRefSeqIdRegex(Suspect: String): Boolean;
begin
  Result := False;
  if not RefSeqIdRegex.Exec(Suspect) then
    Exit;
  Result := True;
end;

end.
