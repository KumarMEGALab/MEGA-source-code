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

unit mseqdataexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MD_InputSeqData, MegaConsts, mseqexportoptions, ExcelWrite;

type

  { TExportSeqDataThread }

  TExportSeqDataThread = class(TThread)
    private
      FExcelFile: TExcelWrite;
      FIsCancelled: Boolean;
      FIsSuccess: Boolean;
      FMsgLog: TStringList;
      FSeqData: TD_InputSeqData;
      procedure SetSeqData(AValue: TD_InputSeqData);
    protected
      FProgress: Integer;
      FMsg: String;
      FOptions: TSeqExportOptions;
      FSaveTo: String;
      FOutList: TStringList;
      function ExportProgress(p: Integer; msg: String): Boolean;
      procedure DoExportProgress;
      function ProcessData: Boolean;
      procedure Execute; override;
    public

      CheckCancelFunc: TProgressCheckCancelFunc;

      constructor Create(aSeqData: TD_InputSeqData; aOptions: TSeqExportOptions; aSaveTo: String; aResultOutput: TStringList);
      destructor Destroy; override;
      property SeqData: TD_InputSeqData read FSeqData write SetSeqData;
      property IsSuccess: Boolean read FIsSuccess;
      property MsgLog: TStringList read FMsgLog;
      property FileDestination: String read FSaveTo;
      property ExportOptions: TSeqExportOptions read FOptions;
      property ExcelFile: TExcelWrite read FExcelFile;
  end;

implementation

{ TExportSeqDataThread }

procedure TExportSeqDataThread.SetSeqData(AValue: TD_InputSeqData);
begin
  if FSeqData=AValue then Exit;
  FSeqData:=AValue;
  FSeqData.ProgressCheckCancelFunc := @ExportProgress;
end;

function TExportSeqDataThread.ExportProgress(p: Integer; msg: String): Boolean;
begin
  FProgress := p;
  FMsg := msg;
  Synchronize(@DoExportProgress);
  Result := FIsCancelled;
end;

procedure TExportSeqDataThread.DoExportProgress;
begin
  if Assigned(CheckCancelFunc) then
    FIsCancelled := CheckCancelFunc(FProgress);
end;

function TExportSeqDataThread.ProcessData: Boolean;
begin
  try
    FOptions.IsInMainThread := False; { so ExcelWriter won't show TPleaseWait outside the context of the main thread}
    FIsSuccess := FSeqData.WriteExportDataToFile(FOptions, FSaveTo, FOutList, FExcelFile);
    Result := True;
  except
    on E:Exception do
    begin
      FMsgLog.Add(E.Message);
      FIsSuccess := False;
    end;
  end;
end;

procedure TExportSeqDataThread.Execute;
var
  i: Integer = 0;
begin
  while i = 0 do
  begin
    ProcessData;
    inc(i);
    Terminate;
    Exit;
  end;
end;

constructor TExportSeqDataThread.Create(aSeqData: TD_InputSeqData; aOptions: TSeqExportOptions; aSaveTo: String; aResultOutput: TStringList);
begin
  inherited Create(True);
  SeqData := aSeqData;
  FOptions := aOptions;
  FSaveTo := aSaveTo;
  FOutList := aResultOutput;
  FreeOnTerminate := True;
  FMsgLog := TStringList.Create;
  FIsSuccess := False;
  FIsCancelled := False;
  case FOptions.ExportFormat of
    mefExcel, mefExcelXml, mefOds, mefCsv: FExcelFile := TExcelWrite.Create(nil, 'Sequence Data');
  else
    FExcelFile := nil;
  end;
end;

destructor TExportSeqDataThread.Destroy;
begin
  if Assigned(FMsgLog) then
    FMsgLog.Free;
  if Assigned(FOptions) then
    FOptions.Free;
  inherited Destroy;
end;

end.

