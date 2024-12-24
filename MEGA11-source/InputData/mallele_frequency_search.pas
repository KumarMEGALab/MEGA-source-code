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

unit mallele_frequency_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts, MegaUtils_NV, mruntimeprogressdlg;

type

  { TAlleleFreqsThread }

  TAlleleFreqsThread = class(TMegaThread)
    private
      FIncludeReferenceSequence: Boolean;
      FFirstSeqIsReference: Boolean;
      FIsSuccess: Boolean;
      FLog: TStringList;
      FNumGroupsCreated: Integer;
      FReport: TStringList;
      FCancelled: Boolean;
      FProgress: Integer;
      FStatus: AnsiString;
      FProgressDlg: TRuntimeProgress;
      FMinCutoff: Double;
      FMaxCutoff: Double;
      function GetLogText: String;
      function ProgressCheckCancel(aProgress: Integer; aStatus: AnsiString): Boolean;
      procedure DoProgressCheckCancel;
    protected
      procedure DoExecute;
      procedure Execute; override;
    public
      constructor Create(aProgress: TRuntimeProgress; minCutoff, maxCutoff: Double; firstIsReference: Boolean; includeRefSeq: Boolean);
      destructor Destroy; override;
      property Report: TStringList read FReport;
      property ProgressDlg: TRuntimeProgress read FProgressDlg;
      property IsSuccess: Boolean read FIsSuccess;
      property LogText: String read GetLogText;
      property FirstSeqIsReference: Boolean read FFirstSeqIsReference;
      property NumGroupsCreated: Integer read FNumGroupsCreated;
  end;

implementation

uses
  MD_InputSeqData;

{ TAlleleFreqsThread }

function TAlleleFreqsThread.ProgressCheckCancel(aProgress: Integer; aStatus: AnsiString): Boolean;
begin
  FProgress := aProgress;
  FStatus := aStatus;
  Synchronize(@DoProgressCheckCancel);
  Result := FCancelled;
end;

function TAlleleFreqsThread.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text;
end;

procedure TAlleleFreqsThread.DoProgressCheckCancel;
begin
  if Assigned(FProgressDlg) then
     FCancelled := FProgressDlg.ProgressAndStatusCheckCancel(FProgress, 'Status', FStatus);
end;

procedure TAlleleFreqsThread.DoExecute;
begin
  try
    D_InputSeqData.ProgressCheckCancelFunc := @ProgressCheckCancel;
    FReport := D_InputSeqData.GetAlleleFrequenciesAsStringList(FMinCutoff, FMaxCutoff, FFirstSeqIsReference, FIncludeReferenceSequence);
    FIsSuccess := (D_InputSeqData.NumAlleleComparisonSitesUsed > 0);
    if FIsSuccess then
      FNumGroupsCreated := D_InputSeqData.GroupIdenticalSeqs(FMinCutoff, FMaxCutoff)
    else
      FLog.Add(Format('No sites with minor allele frequency in the range (%.2f%% - %.2f%%) were found', [FMinCutoff*100, FMaxCutoff*100]));
  except
    on E:Exception do
    begin
      FLog.Add(E.Message);
      FIsSuccess := False;
    end;
  end;
end;

procedure TAlleleFreqsThread.Execute;
begin
  while True do
  begin
    DoExecute;
    Terminate;
    Exit;
  end;
end;

constructor TAlleleFreqsThread.Create(aProgress: TRuntimeProgress; minCutoff, maxCutoff: Double; firstIsReference: Boolean; includeRefSeq: Boolean);
begin
  inherited Create(True);
  FLog := TStringList.Create;
  FReport := nil;
  FMinCutoff := minCutoff;
  FMaxCutoff := maxCutoff;
  FProgressDlg := aProgress;
  FCancelled := False;
  FreeOnTerminate := True;
  FIsSuccess := False;
  FFirstSeqIsReference := firstIsReference;
  FIncludeReferenceSequence := includeRefSeq;
  FNumGroupsCreated := 0;
end;

destructor TAlleleFreqsThread.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FReport) then
    FReport.Free;
  inherited Destroy;
end;

end.

