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

unit mtajimaneutralitytestthread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MegaUtils_NV, Classes, SysUtils, MAnalysisInfo, MegaConsts, MLegendGenerator,
  MAnalysisSummary, ExcelWrite, mtajimatestexport;

type

  { TTajimaNeutralityTestThread }

  TTajimaNeutralityTestThread = class(TMegaThread)
    private
      FExport: TTajimaNeutralityTestExport;
      FLegendGenerator: TLegendGenerator;
      FCheckCancelFunc: TProgressCheckCancelFunc;
      FInfo: TAnalysisInfo;
      FIsSuccess: Boolean;
      FLog: String;
      FProgress: Integer;
      FCancelled: Boolean;
      FNumPairwiseComparisons: Int64;
      MyNoOfSeqs: LongInt;
      MyNoOfSites: LongInt;
      pi, pS, b1, b2, e1, e2, Theta, TajimaD, StdErrD, a1, a2: Extended;
      TotalDiffs, S: Int64;
      procedure DoTest;
      procedure DoCheckCancel;
    protected
      function AnalysisDescription: String; override;
    public
      constructor Create(aInfo: TAnalysisInfo);
      destructor Destroy; override;
      {$IFNDEF VISUAL_BUILD}
      procedure OnTestDone(aThread: TObject);
      {$ENDIF}
      procedure Execute; override;
      function GetLegend: String;
      function GetSpreadsheetExport: TTajimaNeutralityTestExport;
      procedure UpdateSummary(var summary: TAnalysisSummary);
      property IsSuccess: Boolean read FIsSuccess;
      property Log: String read FLog;
      property AnalysisInfo: TAnalysisInfo read FInfo;
      property CheckCancelFunc: TProgressCheckCancelFunc read FCheckCancelFunc write FCheckCancelFunc;
      property Cancelled: Boolean read FCancelled;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}MD_MegaMain,{$ENDIF}
  StringUtils, math, LCLIntF;

{ TTajimaNeutralityTestThread }

procedure TTajimaNeutralityTestThread.DoTest;
var
  i, j, k: Integer;
  SeqA, SeqB: PAnsiChar;
  IsVar: Boolean;
  IsNuc: Boolean;
  AllSiteDiffs: array of Integer;
begin
  try
    if Assigned(FCheckCancelFunc) then
    begin
      FProgress := 0;
      Synchronize(DoCheckCancel);
      if FCancelled then
        Exit;
    end;
    MyNoOfSeqs := FInfo.MyNoOfSeqs;
    MyNoOfSites := FInfo.MyNoOfSites;
    SetLength(AllSiteDiffs, MyNoOfSites);
    for i := 0 to Length(AllSiteDiffs) - 1 do
      AllSiteDiffs[i] := 0;
    TotalDiffs := 0;
    S := 0;
    FNumPairwiseComparisons := Floor(MyNoOfSeqs*(MyNoOfSeqs - 1)/2);
    IsNuc := not FInfo.isAminoAcid;
    for i := 1 to MyNoOfSeqs - 1 do
    begin
      SeqA := FInfo.MyMappedData[i];
      for j := 0 to i - 1 do
      begin
        SeqB := FInfo.MyMappedData[j];
        for k := 0 to MyNoOfSites - 1 do
        begin
          IsVar := True;
          if (SeqA[k] = SeqB[k]) then
            IsVar := False;
          if IsNuc then
          begin
            if (SeqA[k] = gdBaseN) or (SeqB[k] = gdBaseN) then
              IsVar := False;
          end
          else
            if (SeqA[k] = gdResiX) or (SeqB[k] = gdResiX) then
              IsVar := False;
          if IsVar then
            inc(AllSiteDiffs[k]);
        end;
      end;
      if Assigned(FCheckCancelFunc) then
      begin
        FProgress := Floor(((i*(i-1)/2)/FNumPairwiseComparisons)*100);
        Synchronize(DoCheckCancel);
        if FCancelled then
          Exit;
      end;
    end;
    for i := 0 to MyNoOfSites - 1 do
    begin
      if AllSiteDiffs[i] > 0 then
      begin
        inc(S);
        inc(TotalDiffs, AllSiteDiffs[i]);
      end;
    end;

    pS := S/MyNoOfSites;
    pi := TotalDiffs*2/MyNoOfSeqs/(MyNoOfSeqs - 1)/MyNoOfSites;

    // compute a1 and a2
    a1 := 0;
    a2 := 0;
    for i:=1 to MyNoOfSeqs - 1 do
    begin
      a1 := a1 + 1/i;
      a2 := a2 + 1/i/i;
    end;

    Theta := pS/a1;

    // Now we estimate TajimaD
    TajimaD := pi*MyNoOfSites - (S/a1);
    b1 := (MyNoOfSeqs+1)/3/(MyNoOfSeqs-1);
    b2 := 2*(MyNoOfSeqs*MyNoOfSeqs + MyNoOfSeqs + 3)/9/MyNoOfSeqs/(MyNoOfSeqs-1);
    e1 := (a1*b1 - 1)/a1/a1;
    e2 := (b2 - (MyNoOfSeqs+2)/a1/MyNoOfSeqs + a2/a1/a1)/(a1*a1 + a2);
    StdErrD := sqrt(e1*S + e2*S*(S-1));

    // We need to make sure that we are not dividing by 0 to avoid a floating point exception - Joel
    if StdErrD > 0.00000000001 then
      TajimaD := TajimaD /StdErrD;
    FIsSuccess := True;
  except
    on E: Exception do
    begin
      FIsSuccess := False;
      FLog := E.Message;
    end;
  end;
end;

procedure TTajimaNeutralityTestThread.DoCheckCancel;
begin
  if Assigned(FCheckCancelFunc) then
    FCancelled := FCheckCancelFunc(FProgress);
end;

function TTajimaNeutralityTestThread.AnalysisDescription: String;
begin
  Result := 'Tajima Neutrality Test';
end;

{$IFNDEF VISUAL_BUILD}
procedure TTajimaNeutralityTestThread.OnTestDone(aThread: TObject);
var
  SummaryFileName: String;
  MAI: TAnalysisInfo = nil;
  LegendGenerator : TLegendGenerator = nil;
begin
  MAI := AnalysisInfo;
  LegendGenerator := TLegendGenerator.Create;
  LegendGenerator.LoadTemplateFromFile('Tajima_neutrality_test.htm');
  LegendGenerator.AssignData('NoOfSeqs',IntToStr(MyNoOfSeqs));
  LegendGenerator.AssignData('NoOfSites',IntToStr(MyNoOfSites));
  if dsoUseAmino in MAI.MySubsetOpt then
    LegendGenerator.AssignData('Mode','amino acid')
  else LegendGenerator.AssignData('Mode', 'nucleotide');
  if Length(MAI.CodonPositions) > 0 then
    LegendGenerator.AssignData('IncludedPositions', MAI.CodonPositions);
  LegendGenerator.AssignData('S',IntToStr(S));
  LegendGenerator.AssignData('pS',Format('%7.6f',[pS]));
  LegendGenerator.AssignData('pi',Format('%7.6f', [pi]));
  LegendGenerator.AssignData('a1',Format('%7.6f', [a1]));
  LegendGenerator.AssignData('a2',Format('%7.6f', [a2]));
  if Theta < 0.000001 then begin
    LegendGenerator.AssignData('Theta',Format('%7.6e', [Theta]));
  end
  else
    LegendGenerator.AssignData('Theta',Format('%7.6f', [Theta]));
  LegendGenerator.AssignData('Diff',Format('%7.6e', [TajimaD]));
  LegendGenerator.AssignData('SE',Format('%7.6e', [StdErrD]));

  if StdErrD > 0.00000000001 then
    LegendGenerator.AssignData('D',Format('%7.6f', [TajimaD]))
  else
    LegendGenerator.AssignData('D','n/c');
  LegendGenerator.BindData(MAI, True);
  LegendGenerator.SaveCaptionToFile();
  D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
  D_MegaMain.AnalysisSummary.AddCalculatedValue('pS', FormatDoubleSafe(pS, 3, 8));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('pi', FormatDoubleSafe(pi, 3, 8));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('a1', FormatDoubleSafe(a1, 3, 8));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('a2', FormatDoubleSafe(a2, 3, 8));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('S', Format('%8d', [S]));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('Theta', FormatDoubleSafe(Theta, 3, 8));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('Diff', FormatDoubleSafe( TajimaD, 3, 8));
  D_MegaMain.AnalysisSummary.AddCalculatedValue('SE', FormatDoubleSafe(StdErrD, 3, 8));

  if StdErrD > 0.00000000001 then
    D_MegaMain.AnalysisSummary.AddCalculatedValue('D', FormatDoubleSafe(TajimaD, 3, 8))
  else
    D_MegaMain.AnalysisSummary.AddCalculatedValue('D', 'n/c');

  SummaryFileName := NextAvailableFilenameNV('_summary.txt');
  D_MegaMain.AnalysisSummary.WriteToFile(SummaryFileName);
  UploadUsageData(D_MegaMain.AnalysisSummary);
end;
{$ENDIF}

constructor TTajimaNeutralityTestThread.Create(aInfo: TAnalysisInfo);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FInfo := aInfo;
  FAInfo := aInfo;
  FIsSuccess := True;
  FLog := EmptyStr;
  FCheckCancelFunc := nil;
  FCancelled := False;
  FLegendGenerator := nil;
  FExport := nil;
end;

destructor TTajimaNeutralityTestThread.Destroy;
begin
  if FCancelled and Assigned(FInfo) then
    FInfo.Free;
  if Assigned(FLegendGenerator) then
    FLegendGenerator.Free;
  if Assigned(FExport) then
    FExport.Free;
  inherited Destroy;
end;

procedure TTajimaNeutralityTestThread.Execute;
begin
  StartExecute;
  DoTest;
  {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
  UpdatePeakMemUsage;
  {$ENDIF}{$ENDIF}
  EndExecute;
  UpdateAnalysisSummary;
end;

function TTajimaNeutralityTestThread.GetLegend: String;
begin
  if not Assigned(FLegendGenerator) then
  begin
    FLegendGenerator := TLegendGenerator.Create;
    FLegendGenerator.LoadTemplateFromFile('Tajima_neutrality_test.htm');
    FLegendGenerator.AssignData('NoOfSeqs',IntToStr(MyNoOfSeqs));
    FLegendGenerator.AssignData('NoOfSites',IntToStr(MyNoOfSites));
    if dsoUseAmino in FInfo.MySubsetOpt then
      FLegendGenerator.AssignData('Mode','amino acid')
    else FLegendGenerator.AssignData('Mode', 'nucleotide');
    if Length(FInfo.CodonPositions) > 0 then
      FLegendGenerator.AssignData('IncludedPositions', FInfo.CodonPositions);
    FLegendGenerator.AssignData('S',IntToStr(S));
    FLegendGenerator.AssignData('pS',Format('%7.6f',[pS]));
    FLegendGenerator.AssignData('pi',Format('%7.6f', [pi]));
    FLegendGenerator.AssignData('a1',Format('%7.6f', [a1]));
    FLegendGenerator.AssignData('a2',Format('%7.6f', [a2]));
    if Theta < 0.000001 then begin
      FLegendGenerator.AssignData('Theta',Format('%7.6e', [Theta]));
    end
    else
      FLegendGenerator.AssignData('Theta',Format('%7.6f', [Theta]));
    FLegendGenerator.AssignData('Diff',Format('%7.6e', [TajimaD]));
    FLegendGenerator.AssignData('SE',Format('%7.6e', [StdErrD]));

    if StdErrD > 0.00000000001 then
      FLegendGenerator.AssignData('D',Format('%7.6f', [TajimaD]))
    else
      FLegendGenerator.AssignData('D','n/c');
    FLegendGenerator.BindData(FInfo, True);
  end;
  Result := FLegendGenerator.GenerateLegend;
end;

function TTajimaNeutralityTestThread.GetSpreadsheetExport: TTajimaNeutralityTestExport;
var
  ex: TExcelWrite = nil;
  note: TStringList = nil;
  aRect: TRect;
begin
  try
    if not Assigned(FExport) then
    begin
      FExport := TTajimaNeutralityTestExport.Create;
      ex := FExport.SavedExcelWrite;
      ex.Add(Format('Results From Tajima%ss Neutrality Test', [#39]));
      ex.WriteLine();
      aRect := Rect(0, 0, 5, 0);
      ex.ColorCells(aRect, RGB(0, 0, 0), xlBorderBottom);
      ex.BoldCells(aRect);
      ex.Add('m');
      ex.Add('S');
      ex.Add('Ps');
      ex.Add('Θ');
      ex.Add('π');
      ex.Add('D');

      ex.WriteLine();
      ex.Add(MyNoOfSeqs);
      ex.Add(S);
      ex.Add(pS);
      ex.Add(Theta);
      ex.Add(pi);
      if StdErrD > 0.00000000001 then
        ex.Add(TajimaD)
      else
        ex.Add('n/c');
      ex.WriteLine();
      aRect := Rect(0, 1, 5, 1);
      ex.BoldItalicizeCells(aRect);
      ex.AlignCells(aRect, aCenter);
      aRect := Rect(0, 0, 5, 0);
      ex.MergeCells(aRect, aCenter, aCenter);
      note := TStringList.Create;
      GetLegend;
      FLegendGenerator.GenerateNoteAsText(note);
      ex.AddCaptionAsWorksheet(note, 'Caption');
      ex.AutoSizeColumns;
    end;
    Result := FExport;
    FExport := nil;
  finally
    if Assigned(note) then
      note.Free;
  end;
end;

procedure TTajimaNeutralityTestThread.UpdateSummary(var summary: TAnalysisSummary);
begin
  summary.AddAnalysisInfo(FInfo);
  summary.AddCalculatedValue('pS', FormatDoubleSafe(pS, 3, 8));
  summary.AddCalculatedValue('pi', FormatDoubleSafe(pi, 3, 8));
  summary.AddCalculatedValue('a1', FormatDoubleSafe(a1, 3, 8));
  summary.AddCalculatedValue('a2', FormatDoubleSafe(a2, 3, 8));
  summary.AddCalculatedValue('S', Format('%8d', [S]));
  summary.AddCalculatedValue('Theta', FormatDoubleSafe(Theta, 3, 8));
  summary.AddCalculatedValue('Diff', FormatDoubleSafe(TajimaD, 3, 8));
  summary.AddCalculatedValue('SE', FormatDoubleSafe(StdErrD, 3, 8));

  if StdErrD > 0.00000000001 then
    summary.AddCalculatedValue('D', FormatDoubleSafe(TajimaD, 3, 8))
  else
    summary.AddCalculatedValue('D', 'n/c');
end;

end.

