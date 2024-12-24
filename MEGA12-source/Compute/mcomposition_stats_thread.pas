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

unit mcomposition_stats_thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaUtils_NV, MD_InputSeqData, MegaConsts, mruntimeprogressdlg,
  ExcelWrite;

type

  { TStatsCompositionThread }

  TStatsCompositionThread = class abstract(TMEGAThread)
    private
      FExportType: TExportType;
      FProgress: Integer;
      FSpreadsheet: TExcelWrite;
      FStatus: AnsiString;
      FIsCancelled: Boolean;
      function GetLog: TStringList;
      function GetLogText: String;
      function GetResultList: TStringList;
    protected
      FLog: TStringList;
      FSaveLocation: AnsiString;
      FResultList: TStringList;
      procedure DoExecute; virtual; abstract;
      procedure Execute; override;
      procedure DoCheckCancel;
      function CheckCancel(progress: Integer; status: AnsiString): Boolean;
      function GetResultsTitle: String; virtual;
    public
      RuntimeProgressRef: TRuntimeProgress; {a reference to the runtime progress so it can be freed in TThread.OnTerminate. NEVER access RuntimeProgressRef from a thread (LCL is NOT thread safe), unless of course you use TThread.Synchronize to do it}
      CheckCancelFunc: TCheckCancelFunc;
      constructor Create(arp: TRuntimeProgress; aSaveLocation: AnsiString; aExportType: TExportType); {arp is just a reference to the TRuntimeProgress so it can be freed in TThread.OnTerminate}
      destructor Destroy; override;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FIsCancelled;
      property Log: TStringList read GetLog;
      property LogText: String read GetLogText;
      property SaveLocation: AnsiString read FSaveLocation;
      property ResultList: TStringList read GetResultList;
      property ExportType: TExportType read FExportType;
      property ResultsTitle: String read GetResultsTitle;
      property Spreadsheet: TExcelWrite read FSpreadsheet;
  end;

  { TNucStatsCompositionThread }

  TNucStatsCompositionThread = class(TStatsCompositionThread)
    protected
      procedure DoExecute; override;
      function GetResultsTitle: String; override;
  end;

  { TAminoAcidStatsCompositionThread }

  TAminoAcidStatsCompositionThread = class(TStatsCompositionThread)
    protected
      procedure DoExecute; override;
      function GetResultsTitle: String; override;
  end;

  { TCodonUsageCompositionThread }

  { TCodonUsageStatsThread }

  TCodonUsageStatsThread = class(TStatsCompositionThread)
    protected
      procedure DoExecute; override;
      function GetResultsTitle: String; override;
  end;

  { TStatPairFreqsThread }

  TStatPairFreqsThread = class(TStatsCompositionThread)
  private
    FIsDirectional: Boolean;
    protected
      procedure DoExecute; override;
      function GetResultsTitle: String; override;
    public
      constructor Create(arp: TRuntimeProgress; aSaveLocation: AnsiString; aExportType: TExportType; directional: Boolean);
      property IsDirectional: Boolean read FIsDirectional write FIsDirectional;
  end;


implementation

uses
  MVS_SeqDataExplorer;

{ TStatPairFreqsThread }

procedure TStatPairFreqsThread.DoExecute;
begin
  FSpreadsheet := TExcelWrite.Create(nil, 'Nuc Pair Frequencies');
  FSpreadsheet.IsXLS := True;
  FSpreadsheet.AddWorksheet('Info');
  D_InputSeqData.StatPairFreq(FResultList, FSpreadsheet, FSaveLocation, FIsDirectional);
end;

function TStatPairFreqsThread.GetResultsTitle: String;
begin
  if FIsDirectional then
    Result := 'Directional Pair Frequencies'
  else
    Result := 'Undirectional Pair Frequencies';
end;

constructor TStatPairFreqsThread.Create(arp: TRuntimeProgress;
  aSaveLocation: AnsiString; aExportType: TExportType; directional: Boolean);
begin
  inherited Create(arp, aSaveLocation, aExportType);
  FIsDirectional := directional;
end;

{ TCodonUsageStatsThread }

procedure TCodonUsageStatsThread.DoExecute;
begin
  FSpreadsheet := TExcelWrite.Create(nil, 'Codon Usage');
  D_InputSeqData.StatCodonUsage(FResultList, FSpreadsheet, FSaveLocation);
end;

function TCodonUsageStatsThread.GetResultsTitle: String;
begin
  Result := 'Codon Usage';
end;

{ TAminoAcidStatsCompositionThread }

procedure TAminoAcidStatsCompositionThread.DoExecute;
begin
  FSpreadsheet := TExcelWrite.Create(nil, 'Amino Acid Composition');
  FSpreadsheet.IsXLS := VS_SeqDataExplorer.IsXLS;
  FSpreadsheet.AddWorksheet('Info');
  D_InputSeqData.StatAminoAcidComposition(FResultList, FSpreadsheet, FSaveLocation);
end;

function TAminoAcidStatsCompositionThread.GetResultsTitle: String;
begin
  Result := 'Amino Acid Composition';
end;

{ TNucStatsCompositionThread }

procedure TNucStatsCompositionThread.DoExecute;
begin
  FSpreadsheet := TExcelWrite.Create(nil, 'Nucleotide Composition');
  FSpreadsheet.IsXLS := VS_SeqDataExplorer.IsXls;
  FSpreadsheet.AddWorksheet('Info');
  D_InputSeqData.StatNucComposition(FResultList, FSpreadsheet, FSaveLocation);
end;

function TNucStatsCompositionThread.GetResultsTitle: String;
begin
  Result := 'Nucleotide Composition';
end;

{ TStatsCompositionThread }

function TStatsCompositionThread.GetLog: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(FLog);
end;

function TStatsCompositionThread.GetLogText: String;
begin
  Result := FLog.Text;
end;

function TStatsCompositionThread.GetResultList: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(FResultList);
end;

function TStatsCompositionThread.GetResultsTitle: String;
begin
  Result := 'Composition Statistics';
end;

procedure TStatsCompositionThread.Execute;
begin
  try
    try
      if (not Assigned(D_InputSeqData)) or (D_InputSeqData.NoOfSites <= 0) then
        raise Exception.Create('composition statistics requires sequence data');
      D_InputSeqData.IsRunningInThread := True;
      while True do
      begin
        D_InputSeqData.ProgressCheckCancelFunc := @CheckCancel;
        DoExecute;
        Terminate;
        FIsSuccess := True;
        break;
      end;
    except
      on E:EAbort do
      begin
        FIsSuccess := False;
        FIsCancelled := True;
        FLog.Add(E.Message);
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    D_InputSeqData.ProgressCheckCancelFunc := nil;
    D_InputSeqData.IsRunningInThread := False;
  end;
end;

procedure TStatsCompositionThread.DoCheckCancel;
begin
  if Assigned(CheckCancelFunc) then
    FIsCancelled := CheckCancelFunc(FProgress, FStatus);
end;

function TStatsCompositionThread.CheckCancel(progress: Integer; status: AnsiString): Boolean;
begin
  FProgress := progress;
  FStatus := status;
  Synchronize(@DoCheckCancel);
  Result := FIsCancelled;
  if FIsCancelled then
    raise EAbort.Create('user cancelled');
end;

constructor TStatsCompositionThread.Create(arp: TRuntimeProgress; aSaveLocation: AnsiString; aExportType: TExportType);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FLog := TStringList.Create;
  FIsCancelled := False;
  FIsSuccess := False;
  RuntimeProgressRef := arp;
  FSaveLocation := aSaveLocation;
  FResultList := TStringList.Create;
  FExportType := aExportType;
  if Assigned(RuntimeProgressRef) then
    CheckCancelFunc := @RuntimeProgressRef.ProgressAndStatusInfoCheckCancel;
  FSpreadsheet := nil;
end;

destructor TStatsCompositionThread.Destroy;
begin
  if Assigned(FResultList) then
    FResultList.Free;
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FSpreadsheet) then
    FSpreadsheet.Free;
  inherited Destroy;
end;

end.

