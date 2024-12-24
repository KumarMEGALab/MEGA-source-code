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

unit mtajimaclocktestthread;

{$mode objfpc}{$H+}

interface

uses
  MegaUtils_NV,
  Classes, SysUtils, MAnalysisInfo, MegaConsts, MAnalysisSummary, MLegendGenerator,
  mdistpack, mtajimatestexport;

type

  { TTajimaClockTestThread }

  TTajimaClockTestThread = class(TMegaThread)
    private
      FExport: TTajimaClockTestExport;
      FAnalysisInfo: TAnalysisInfo;
      FCancelled: Boolean;
      FCheckCancelFunc: TProgressCheckCancelFunc;
      FLog: String;
      FProgress: Integer;
      Subset: TDataSubsetOptions;
      ChangeType: TDistType;
      ChangesConsideredStr: String;
      MyNoOfSeqs:   LongInt;
      MyNoOfSites:  LongInt;
      MyThreeOtuInfos: TList;
      SeqA, SeqB, SeqC: PAnsiChar;
      df: Integer;
      chiSqr, chiSqrP: Extended;

      m_iji, m_ijj, m_iii, m_iij, m_ijk: Int64;
      ts_iji, ts_ijj, ts_iij: Int64;
      tv_iji, tv_ijj, tv_iij: Int64;
      procedure DoProgress;
      procedure DoTest;
      procedure SetCheckCancelFunc(AValue: TProgressCheckCancelFunc);
      {$IFNDEF VISUAL_BUILD}
      procedure OnTestDone(aThread: TObject);
      {$ENDIF}
      function TajimaTest(m1, m2: Int64): Extended;
      function IsTransition(x, y: AnsiChar): Boolean;
      function GetNoteAsList: TStringList;
    protected
      function AnalysisDescription: String; override;
    public
      constructor Create(aInfo: TAnalysisInfo; aOtuInfos: TList);
      destructor Destroy; override;
      procedure Execute; override;
      function GetSpreadsheetExport: TTajimaClockTestExport;
      procedure UpdateSummary(var s: TAnalysisSummary);
      function GetLegend: TLegendGenerator;
      function GetLegendAsText: String;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo;
      property IsSuccess: Boolean read FIsSuccess;
      property Log: String read FLog;
      property CheckCancelFunc: TProgressCheckCancelFunc read FCheckCancelFunc write SetCheckCancelFunc;
      property Cancelled: Boolean read FCancelled;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain,
  {$ELSE}
  Graphics,
  {$ENDIF}
  math, MegaUtils, MOtuInfo, StringUtils, ExcelWrite;

{ TTajimaClockTestThread }

procedure TTajimaClockTestThread.DoProgress;
begin
  if Assigned(FCheckCancelFunc) then
    FCancelled := FCheckCancelFunc(FProgress);
end;

procedure TTajimaClockTestThread.DoTest;
var
  i: Integer;
begin
  try
      FProgress := 0;
      Synchronize(@DoProgress);
      SeqA := FAnalysisInfo.MyMappedData[0];
      SeqB := FAnalysisInfo.MyMappedData[1];
      SeqC := FAnalysisInfo.MyMappedData[2];
      m_iji := 0;  m_ijj := 0;  m_iii := 0; m_iij := 0; m_ijk := 0;
      ts_iji := 0; ts_ijj := 0; ts_iij := 0;
      tv_iji :=0;  tv_ijj := 0; tv_iij := 0;
      Subset := FAnalysisInfo.MySubsetOpt;
      ChangeType := FAnalysisInfo.MyDistPack.DistComponent;
      MyNoOfSites := FAnalysisInfo.MyNoOfSites;
      MyNoOfSeqs := 3;
      if (ChangeType = gdAll) or (dsoUseAmino in subset) then
      begin
        for i:=0 to MyNoOfSites-1 do
        begin
          if      (SeqA[i] = SeqB[i]) and (SeqA[i] = SeqC[i])  then Inc(m_iii) // identical
          else if (SeqB[i] = SeqC[i]) and (SeqA[i] <> SeqC[i]) then Inc(m_ijj) // A is unique
          else if (SeqA[i] = SeqC[i]) and (SeqB[i] <> SeqC[i]) then Inc(m_iji) // B is unique
          else if (SeqA[i] = SeqB[i]) and (SeqC[i] <> SeqA[i]) then Inc(m_iij) // C is unique
          else    Inc(m_ijk);//all three different
          FProgress := Floor((i/MyNoOfSites)*100);
          Synchronize(@DoProgress);
          if FCancelled then
            Exit;
        end;

        df := 1;
        ChiSqr := TajimaTest(m_ijj, m_iji);
        if ChiSqr <0.000001 then
          ChiSqrP := 0.999999
        else
          ChiSqrP := GammaQ(0.5, 0.5*ChiSqr);

        if ChiSqrP > 0.999999999 then
          ChiSqrP := 0.999999999;
      end
      else
      begin  // come here only for nuc data
        for i := 0 to MyNoOfSites-1 do
        begin
          if      (SeqA[i] = SeqB[i]) and (SeqA[i] = SeqC[i])  then Inc(m_iii) // identical
          else if (SeqB[i] = SeqC[i]) and (SeqA[i] <> SeqC[i]) then // A is unique
          begin
            if IsTransition(SeqA[i], SeqC[i]) then Inc(ts_ijj) else Inc(tv_ijj);
          end
          else if (SeqA[i] = SeqC[i]) and (SeqB[i] <> SeqC[i]) then // B is unique
          begin
            if IsTransition(SeqB[i], SeqC[i]) then Inc(ts_iji) else Inc(tv_iji)
          end
          else if (SeqA[i] = SeqB[i]) and (SeqC[i] <> SeqA[i]) then // C is unique
          begin
            if IsTransition(SeqA[i], SeqC[i]) then Inc(ts_iij) else Inc(tv_iij);
          end
          else
            Inc(m_ijk);//all three different
          FProgress := Floor((i/MyNoOfSites)*100);
          Synchronize(@DoProgress);
          if FCancelled then
            Exit;
        end;

        ChiSqr := 0;
        df := 0;
        if (ChangeType = gdNucTsOnly) or (ChangeType = gdNucSeparateTsTv) then
        begin
          ChiSqr := ChiSqr + TajimaTest(ts_ijj, ts_iji);
          Inc(df);
        end;

        if (ChangeType = gdNucTvOnly) or (ChangeType = gdNucSeparateTsTv) then
        begin
          ChiSqr := ChiSqr + TajimaTest(tv_ijj, tv_iji);
          Inc(df);
        end;

        if ChiSqr <0.000001 then
          ChiSqrP := 0.999999
        else
          ChiSqrP := GammaQ(0.5*df, 0.5*ChiSqr);
        if ChiSqrP > 0.999999999 then
          ChiSqrP := 0.999999999;
      end;
      FProgress := 100;
      Synchronize(@DoProgress);
      FIsSuccess := True;
  except
    on E: Exception do
    begin
      FIsSuccess := False;
      FLog := E.Message;
    end;
  end;
end;

procedure TTajimaClockTestThread.SetCheckCancelFunc(AValue: TProgressCheckCancelFunc);
begin
  if FCheckCancelFunc=AValue then Exit;
  FCheckCancelFunc:=AValue;
end;

function TTajimaClockTestThread.TajimaTest(m1, m2: Int64): Extended;
begin
  if (m1+m2) = 0 then
    Result := 0
  else
    Result := ((m1-m2)*(m1-m2))/(m1+m2);
end;

function TTajimaClockTestThread.IsTransition(x, y: AnsiChar): Boolean;
begin
  Result := ((x = gdBaseA) and (y = gdBaseG)) or // AG type ts
            ((x = gdBaseG) and (y = gdBaseA)) or // GA type ts
            ((x = gdBaseT) and (y = gdBaseC)) or // TC type ts
            ((x = gdBaseC) and (y = gdBaseT));   // CT type ts
end;

function TTajimaClockTestThread.GetNoteAsList: TStringList;
var
  g: TLegendGenerator = nil;
begin
  try
    g := GetLegend;
    Result := TStringList.Create;
    g.GenerateNoteAsText(Result);
  finally
    if Assigned(g) then
      g.Free;
  end;
end;

function TTajimaClockTestThread.AnalysisDescription: String;
begin
  Result := 'Tajima Clock Test';
end;

{$IFNDEF VISUAL_BUILD}
procedure TTajimaClockTestThread.OnTestDone(aThread: TObject);
var
  LegendGenerator: TLegendGenerator = nil;
  Summary: TAnalysisSummary;
begin
  try
    try
      LegendGenerator := GetLegend;
      Summary := D_MegaMain.AnalysisSummary;
      LegendGenerator.SaveCaptionToFile();
      UpdateSummary(Summary);
      Summary.WriteToFile(ChangeFileExt(NextAvailableFilenameNV('.txt'), '_summary.txt'));
    except
      on E:Exception do
        error_nv('exception in tajima test done procedure', E);
    end;
  finally
    if Assigned(LegendGenerator) then
      LegendGenerator.Free;
  end;
end;
{$ENDIF}

constructor TTajimaClockTestThread.Create(aInfo: TAnalysisInfo; aOtuInfos: TList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FAnalysisInfo := aInfo;
  FAInfo := aInfo;
  MyThreeOtuInfos := aOtuInfos;
  FCancelled := False;
  FIsSuccess := False;
  FExport := nil;
  {$IFNDEF VISUAL_BUILD}
  OnTerminate := @OnTestDone;
  {$ENDIF}
end;

destructor TTajimaClockTestThread.Destroy;
begin
  if Assigned(FAnalysisInfo) then
    FAnalysisInfo.Free;
  if Assigned(MyThreeOtuInfos) then
    MyThreeOtuInfos.Free;
  if Assigned(FExport) then
    FExport.Free;
  inherited Destroy;
end;

procedure TTajimaClockTestThread.Execute;
begin
  StartExecute;
  DoTest;
  {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
  UpdatePeakMemUsage;
  {$ENDIF}{$ENDIF}
  EndExecute;
  UpdateAnalysisSummary;
end;

function TTajimaClockTestThread.GetSpreadsheetExport: TTajimaClockTestExport;
var
  ex: TExcelWrite = nil;
  note: TStringList;
  aRect: TRect;
begin
  try
    if not Assigned(FExport) then
    begin
      FExport := TTajimaClockTestExport.Create;
      ex := FExport.SavedExcelWrite;
      ex.Add(Format('Results from the Tajima%ss Test for 3 Sequences', [#39]));
      ex.WriteLine;
      ex.Add('Configuration');
      ex.Add('Count');
      ex.WriteLine;

      ex.Add('Identical sites in all three sequences');
      ex.Add(Format('%d', [m_iii]));
      ex.WriteLine;
      ex.Add('Divergent sites in all three sequences');
      ex.Add(Format('%d', [m_ijk]));
      ex.WriteLine;

      if ChangeType = gdNucTsOnly then
      begin
        ex.Add('Unique transitions in Sequence A');
        ex.Add(Format('%d', [ts_ijj]));
        ex.WriteLine;
        ex.Add('Unique transitions in Sequence B');
        ex.Add(Format('%d', [ts_iji]));
        ex.WriteLine;
        ex.Add('Unique transitions in Sequence C');
        ex.Add(Format('%d', [ts_iij]));
        ex.WriteLine;
      end
      else if ChangeType = gdNucTvOnly then
      begin
        ex.Add('Unique transversions in Sequence A');
        ex.Add(Format('%d', [tv_ijj]));
        ex.WriteLine;
        ex.Add('Unique transversions in Sequence B');
        ex.Add(Format('%d', [tv_iji]));
        ex.WriteLine;
        ex.Add('Unique transversions in Sequence C');
        ex.Add(Format('%d', [tv_iij]));
        ex.WriteLine;
      end
      else if ChangeType = gdNucSeparateTsTv then
      begin
        ex.Add('Unique transitions in Sequence A');
        ex.Add(Format('%d', [ts_ijj]));
        ex.WriteLine;
        ex.Add('Unique transitions in Sequence B');
        ex.Add(Format('%d', [ts_iji]));
        ex.WriteLine;
        ex.Add('Unique transitions in Sequence C');
        ex.Add(Format('%d', [ts_iij]));
        ex.WriteLine;
        ex.Add('Unique transversions in Sequence A');
        ex.Add(Format('%d', [tv_ijj]));
        ex.WriteLine;
        ex.Add('Unique transversions in Sequence B');
        ex.Add(Format('%d', [tv_iji]));
        ex.WriteLine;
        ex.Add('Unique transversions in Sequence C');
        ex.Add(Format('%d', [tv_iij]));
        ex.WriteLine;
      end
      else
      begin
        ex.Add('Unique differences in Sequence A');
        ex.Add(Format('%d', [m_ijj]));
        ex.WriteLine;
        ex.Add('Unique differences in Sequence B');
        ex.Add(Format('%d', [m_iji]));
        ex.WriteLine;
        ex.Add('Unique differences in Sequence C');
        ex.Add(Format('%d', [m_iij]));
        ex.WriteLine;
      end;
      note := GetNoteAsList;
      ex.AddCaptionAsWorksheet(note, 'Note');
      aRect := Rect(0, 0, 1, 1);
      ex.BoldCells(aRect);
      ex.AlignCells(aRect, aCenter, aCenter, 0);
      aRect := Rect(0, 0, 1, 0);
      {$IFDEF VISUAL_BUILD}
      ex.ColorCells(aRect, clBlack, xlBorderBottom, 0);
      {$ENDIF}
      ex.MergeCells(aRect, aCenter, aCenter);
      ex.AutoSizeColumns;
    end;
    Result := FExport;
    FExport := nil;
  finally
    if Assigned(note) then
      note.Free;
  end;
end;

procedure TTajimaClockTestThread.UpdateSummary(var s: TAnalysisSummary);
begin
  s.AddAnalysisInfo(FAnalysisInfo);
  s.AddCalculatedValue('Sequence A',TOtuInfo(MyThreeOtuInfos[0]).Name);
  s.AddCalculatedValue('Sequence B',TOtuInfo(MyThreeOtuInfos[1]).Name);
  s.AddCalculatedValue('Sequence C (outgroup)',TOtuInfo(MyThreeOtuInfos[2]).Name);
  s.AddCalculatedValue('Changes', ChangesConsideredStr);
  s.AddCalculatedValue('Chi Square', FormatDoubleSafe(ChiSqr, 2, 7));
  s.AddCalculatedValue('Deg Freedom',IntToStr(df));
  s.AddCalculatedValue('Chi Square P', FormatDoubleSafe(ChiSqrP, 2 , 7));
  s.AddCalculatedValue('Identical sites in all 3 sequences',IntToStr(m_iii));
  s.AddCalculatedValue('Divergent sites in all 3 sequences',IntToStr(m_ijk));
  if (ChangeType = gdAll) or (dsoUseAmino in subset) then
  begin
    s.AddCalculatedValue('Unique differences in Sequence A',IntToStr(m_ijj));
    s.AddCalculatedValue('Unique differences in Sequence B',IntToStr(m_iji));
    s.AddCalculatedValue('Unique differences in Sequence C',IntToStr(m_iij));
  end
  else
  begin
    if ChangeType = gdNucTsOnly then
    begin
      s.AddCalculatedValue('Unique transitions in Sequence A',IntToStr(ts_ijj));
      s.AddCalculatedValue('Unique transitions in Sequence B',IntToStr(ts_iji));
      s.AddCalculatedValue('Unique transitions in Sequence C',IntToStr(ts_iij));
    end
    else if ChangeType = gdNucTvOnly then
    begin
      s.AddCalculatedValue('Unique transversions in Sequence A',IntToStr(tv_ijj));
      s.AddCalculatedValue('Unique transversions in Sequence B',IntToStr(tv_iji));
      s.AddCalculatedValue('Unique transversions in Sequence C',IntToStr(tv_iij));
    end
    else if ChangeType = gdNucSeparateTsTv then
    begin
      s.AddCalculatedValue('Unique transitions in Sequence A',IntToStr(ts_ijj));
      s.AddCalculatedValue('Unique transitions in Sequence B',IntToStr(ts_iji));
      s.AddCalculatedValue('Unique transitions in Sequence C',IntToStr(ts_iij));
      s.AddCalculatedValue('Unique transversions in Sequence A',IntToStr(tv_ijj));
      s.AddCalculatedValue('Unique transversions in Sequence B',IntToStr(tv_iji));
      s.AddCalculatedValue('Unique transversions in Sequence C',IntToStr(tv_iij));
    end;
  end;
end;

function TTajimaClockTestThread.GetLegend: TLegendGenerator;
begin
  Result := TLegendGenerator.Create;
  Result.LoadTemplateFromFile('Tajima_Rate_Test_Template.htm');
  if dsoUseAmino in FAnalysisInfo.MySubsetOpt then
    Result.AssignData('Mode','amino acid')
  else
    Result.AssignData('Mode', 'nucleotide');
  if Length(FAnalysisInfo.CodonPositions) > 0 then
    Result.AssignData('IncludedPositions', FAnalysisInfo.CodonPositions);

  case ChangeType of
    gdAll:
      begin
        ChangesConsideredStr := 'All differences';
        Result.AssignData('ChangesConsidered', 'All differences');
        Result.AssignData('ChangeType', 'gdAll');
      end;
    gdNucTsOnly:
      begin
        ChangesConsideredStr := 'Only Transitions';
        Result.AssignData('ChangesConsidered', 'Only Transitions');
        Result.AssignData('ChangeType', 'gdNucTsOnly');
      end;
    gdNucTvOnly:
      begin
        ChangesConsideredStr := 'Only Transversions';
        Result.AssignData('ChangesConsidered', 'Only Transversions');
        Result.AssignData('ChangeType', 'gdNucTvOnly');
      end;
    gdNucSeparateTsTv:
      begin
        ChangesConsideredStr := 'Transitions and Transversions separated';
        Result.AssignData('ChangesConsidered', 'Transitions and Transversions separated');
        Result.AssignData('ChangeType', 'gdNucSeparateTsTv');
      end;
  else
    begin
      ChangesConsideredStr := 'All';
      Result.AssignData('ChangesConsidered', 'All');
      Result.AssignData('ChangeType', 'gdAll');
    end;
  end;

  Result.AssignData('SequenceA',TOtuInfo(MyThreeOtuInfos[0]).Name);
  Result.AssignData('SequenceB',TOtuInfo(MyThreeOtuInfos[1]).Name);
  Result.AssignData('SequenceC',TOtuInfo(MyThreeOtuInfos[2]).Name);
  Result.AssignData('NoOfSites',IntToStr(MyNoOfSites));

  Result.AssignData('m_iii',IntToStr(m_iii));
  Result.AssignData('m_ijk',IntToStr(m_ijk));
  if (ChangeType = gdAll) or (dsoUseAmino in subset) then
  begin
    Result.AssignData('m_ijj',IntToStr(m_ijj));
    Result.AssignData('m_iji',IntToStr(m_iji));
    Result.AssignData('m_iij',IntToStr(m_iij));
  end
  else
  begin
    Result.AssignData('ts_ijj',IntToStr(ts_ijj));
    Result.AssignData('ts_iji',IntToStr(ts_iji));
    Result.AssignData('ts_iij',IntToStr(ts_iij));
    Result.AssignData('tv_ijj',IntToStr(tv_ijj));
    Result.AssignData('tv_iji',IntToStr(tv_iji));
    Result.AssignData('tv_iij',IntToStr(tv_iij));
  end;

  Result.AssignData('ChiSquare',Format('%7.2f',[ChiSqr, df,ChiSqrP]));
  Result.AssignData('DegFreedom',Format('%d',[df]));
  Result.AssignData('ChiSquareP',Format('%7.5f',[ChiSqrP]));
  Result.BindData(FAnalysisInfo, True);
end;

function TTajimaClockTestThread.GetLegendAsText: String;
var
  g: TLegendGenerator = nil;
begin
  try
    g := GetLegend;
    Result := g.GenerateLegend;
  finally
    if Assigned(g) then
      g.Free;
  end;
end;

end.

