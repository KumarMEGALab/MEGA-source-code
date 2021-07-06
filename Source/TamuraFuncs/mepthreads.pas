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

unit mepthreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MLSearchThread, MAnalysisInfo, MTreeList, MTreeData,
  MTreeDataAdapter, MLTree, MLTreeAnalyzer, mreltimecomputer, megautils_nv,
  ExcelWrite, MegaConsts;

type

  { TEpThreadTerminator }

  TEpThreadTerminator = class(TObject)
    public
      procedure OnThreadDone(Thread: TObject);
  end;

  { TEpThread }

  TEpThread = class(TMegaThread)
    private
      FCheckCancelFunc: TProgressCheckCancelFunc;
      FSubTasksCheckCancel: TCheckCancelFunc;
      FWeightsNullifier: array of array of Integer;
      FWeights: array of Double;
      FNormalizedTimes: T2DExtArray;
      FSumOfWeights: Double;
      FEpValues: T2DExtArray;
      FUngappedTimes: T2DExtArray;
      FPrunedTrees: TList;
      FAnalyzedTrees: TList;
      FReltimeTrees: TList;
      FNewickStrings: TStringList;
      FSeqStrings: TStringList;
      FOtuNameLists: TList;
      FAnalysisInfo: TAnalysisInfo;
      FIsSuccess: Boolean;
      FMsgLog: TStringList;
      FTreePruner: TSimpleTreeDataAdapter;
      FReltimeComputer: TReltimeComputer;
      FPosteriorProbs: TAncStateRecArrayArrayArray;
      FNumStates: Integer;
      FProgress: Integer;
      FSubtaskProgress: Integer;
      FStatus: String;
      FSubtaskStatus: String;
      FCancelled: Boolean;
      FOrigNumTaxa: Integer;
      procedure SetAnalysisInfo(AValue: TAnalysisInfo);
      procedure ClearPrunedTrees;
      procedure ClearOtuNamesLists;
      procedure ClearReltimeTrees;
      procedure SetCheckCancelFunc(AValue: TProgressCheckCancelFunc);
      procedure SetSubTasksCheckCancel(AValue: TCheckCancelFunc);
      function WeightsNullifierToStringList: TStringList;
      function UngappedTimesToStringList: TStringList;
      function NormalizedTimesToStringList: TStringList;
      function PosteriorProbsToDebugFiles: Boolean;
      function PosteriorProbsToSpreadsheet: TExcelWrite;
      procedure NewickStringsToDebugFiles;
      procedure UngappedTimesToDebugFile;
      procedure NormalizedTimesToDebugFile;
      procedure DoCheckCancelFunc;
      procedure DoSubtaskCheckCancelFunc;
      function UpdateOverallProgress(aProgress: Integer): Boolean;
      function DebugFilename(fileExt: String): String;
    protected
      procedure OnCalcProbsFinished(aThread: TObject);
      function PruneTrees: Boolean;
      function SumOfPosteriorProbs(site: Integer; state: Integer): Double;
      procedure CalculatePosteriorProbs;
      procedure CalculateReltimes;
      procedure CalculateEpValues;
      function CalculateEpValue(site: Integer; state: Integer): Extended;
      function GetUngappedTimes: T2DExtArray;
      function GetUngappedTime(aData: TTreeData; allNames: TStringList; droppedNames: TStringList; IsDebug: Boolean=False): Extended;
      procedure CalcNormalizedTimes;
      procedure GetSeqsForNames(const names: TStringList; var Seqs: TStringList);
      procedure GetPrunedNames(const NamesBeforePruning: TStringList; const NamesAfterPruning: TStringList; var ResultList: TStringList);
      procedure GenerateOutput(var xl: TExcelWrite);
    public
      constructor Create(MAI: TAnalysisInfo);
      destructor Destroy; override;
      procedure Execute; override;
      function GenerateCaption: TStringList;
      function GetXlOutput: TExcelWrite;
      function GetTextOutput: TStringList;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
      property MsgLog: TStringList read FMsgLog;
      property IsSuccess: Boolean read FIsSuccess;
      property EPValues: T2DExtArray read FEpValues;
      property SubTasksCheckCancel: TCheckCancelFunc read FSubTasksCheckCancel write SetSubTasksCheckCancel;
      property CheckCancelFunc: TProgressCheckCancelFunc read FCheckCancelFunc write SetCheckCancelFunc;
  end;

  { TCalcPosteriorProbsThread }

  TCalcPosteriorProbsThread = class(TMegaThread)
    private
      FAnalysisInfo: TAnalysisInfo;
      FCheckCancel: TProgressCheckCancelFunc;
      FIsSuccess: Boolean;
      FMsgLog: TStringList;
      FSubtaskCheckCancel: TCheckCancelFunc;
      FTrees: TList;
      FNewickStrings: TStringList;
      FSeqStrings: TStringList;
      FOtuNamesLists: TList;
      FOrigOtuNames: TStringList;
      FPosteriorProbs: TAncStateRecArrayArrayArray;
      FIndex: Integer;
      FGroupNames: TStringList;
      procedure SetCheckCancel(AValue: TProgressCheckCancelFunc);
      procedure SetSubtaskCheckCancel(AValue: TCheckCancelFunc);
    protected
      procedure UpdateAnalysisInfo;
      function AnalyzeAllTrees: Boolean;
      function AnalyzeTree: Boolean;
      procedure AnalyzeUserTreeDone(aThread: TObject);
    public
      constructor Create(MAI: TAnalysisInfo; Newicks: TStringList; Names: TList; SeqStrings: TStringList);
      destructor Destroy; override;
      procedure Execute; override;

      property IsSuccess: Boolean read FIsSuccess;
      property MsgLog: TStringList read FMsgLog;
      property PosteriorProbs: TAncStateRecArrayArrayArray read FPosteriorProbs;
      property TreeDataList: TList read FTrees;
      property SubtaskCheckCancel: TCheckCancelFunc read FSubtaskCheckCancel write SetSubtaskCheckCancel;
      property CheckCancel: TProgressCheckCancelFunc read FCheckCancel write SetCheckCancel;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain, KeywordConsts,
  {$ELSE}
  MWriteOutputDlg, Dialogs, MEditorForm,
  {$ENDIF}
  LCLIntF, MegaUtils, math, MLegendGenerator,
  mdistpack;

{ TEpThreadTerminator }

procedure TEpThreadTerminator.OnThreadDone(Thread: TObject);
var
  t: TEpThread = nil;
  caption: TStringList = nil;
  outList: TStringList = nil;
  xlWrite: TExcelWrite = nil;
  outfile: String = 'Evolutionary_Probabilities';
  outType: TExportType;
begin
  t := TEpThread(Thread);

  try
    try
      if t.IsSuccess then
      begin
        {$IFDEF VISUAL_BUILD}
        outType := PromptUserWriteOutput(outfile, True, EXexcelXmlDisp);
        if (outType <> EXnone) and (Trim(outfile) <> EmptyStr) then
        begin
          case outType of
            EXtext, EXtextSave:
              begin
                outList := t.GetTextOutput;
                if outType = EXtextSave then
                  outList.SaveToFile(outfile)
                else
                  OpenStringList(outList, 'Evolutionary Probabilities');
              end;
            EXcsvDisp, EXcsvSave, EXexcelDisp, EXexcelSave, EXexcelXmlDisp, EXexcelXmlSave, EXodsDisp, EXodsSave:
              begin
                xlWrite := t.GetXlOutput;
                case outType of
                  EXcsvDisp, EXcsvSave: xlWrite.SaveFile(outfile, ExportCSV);
                  EXexcelDisp, EXexcelSave: xlWrite.SaveFile(outFile, ExportExcel);
                  EXexcelXmlDisp, EXexcelXmlSave: xlWrite.SaveFile(outfile, ExportExelXML);
                  EXodsDisp, EXodsSave: xlWrite.SaveFile(outfile, ExportODS);
                end;
                case outType of
                  EXcsvDisp, EXexcelDisp, EXexcelXmlDisp, EXodsDisp: OpenDocument(outfile);
                end;
              end;
          end;
        end;
        {$ELSE}
        outList := t.GetTextOutput;
        outfile := NextAvailableFilenameNV('.txt');
        outList.SaveToFile(outfile);
        caption := t.GenerateCaption;
        caption.SaveToFile(ChangeFileExt(outfile, '_caption.html'));
        D_MegaMain.AnalysisSummary.AddAnalysisInfo(t.AnalysisInfo);
        if t.AnalysisInfo.isAminoAcid then
          D_MegaMain.AnalysisSummary.DataType := snProtein
        else
          D_MegaMain.AnalysisSummary.DataType := snNucleotide;
        D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(outfile, '_summary.txt'));
        {$ENDIF}
      end
      else
        raise Exception.Create('EP calculation returned false: ' + t.MsgLog.Text);
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error occurred in the EP calculation: ' + E.Message);
        {$ELSE}
        error_nv('Oh no! An error occurred in the EP calculation: ', E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(outList) then
      outList.Free;
    if Assigned(xlWrite) then
      xlWrite.Free;
    if Assigned(caption) then
      caption.Free;
  end;
end;

{ TCalcPosteriorProbsThread }

procedure TCalcPosteriorProbsThread.SetSubtaskCheckCancel(
  AValue: TCheckCancelFunc);
begin
  if FSubtaskCheckCancel=AValue then Exit;
  FSubtaskCheckCancel:=AValue;
end;

procedure TCalcPosteriorProbsThread.SetCheckCancel(
  AValue: TProgressCheckCancelFunc);
begin
  if FCheckCancel=AValue then Exit;
  FCheckCancel:=AValue;
end;

procedure TCalcPosteriorProbsThread.UpdateAnalysisInfo;
var
  aNames: TStringList;
  i: Integer;
  seq: String;
begin
  if Assigned(FAnalysisInfo.MyOriTreeList) then
  begin
    FAnalysisInfo.MyOriTreeList.Free;
    FAnalysisInfo.MyOriTreeList := nil;
  end;
  FAnalysisInfo.MyUserNewickTree := FNewickStrings[FIndex];
  aNames := TStringList(FOtuNamesLists[FIndex]);
  FAnalysisInfo.MyOtuNames.Assign(aNames);
  if not Assigned(FAnalysisInfo.MySeqStrings) then
    FAnalysisInfo.MySeqStrings := TStringList.Create
  else
    FAnalysisInfo.MySeqStrings.Clear;
  for i := 0 to aNames.Count - 1 do
  begin
    seq := FSeqStrings[FOrigOtuNames.IndexOf(aNames[i])];
    FAnalysisInfo.MySeqStrings.Add(seq);
  end;
  FAnalysisInfo.MyNoOfSeqs := aNames.Count;
end;

function TCalcPosteriorProbsThread.AnalyzeAllTrees: Boolean;
var
  i: Integer;
begin
  Result := True;
  if FNewickStrings.Count > 0 then
  begin
    SetLength(FPosteriorProbs, FNewickStrings.Count);
    for i := 0 to FNewickStrings.Count - 1 do
    begin
      FIndex := i;
      Result := Result and AnalyzeTree;
      //if Assigned(FCheckCancel)then
      //  FCheckCancel(Round(i*100/FNewickStrings.Count));
    end;
  end
  else
    raise Exception.Create('list of pruned trees is empty');
end;

function TCalcPosteriorProbsThread.AnalyzeTree: Boolean;
var
  aThread: TMLTreeAnalyzeThread = nil;
  status: Integer;
begin
  try
    if not FAnalysisInfo.ARP.Visible then
      FAnalysisInfo.ARP.Show;
    UpdateAnalysisInfo;
    aThread := TMLTreeAnalyzeThread.Create(nil);
    aThread.GroupNames := FGroupNames;
    aThread.OptimizeBLens := true;
    aThread.OptimizeParams := true;
    aThread.ProgressDlg := FAnalysisInfo.ARP;
    {$IFDEF VISUAL_BUILD}
    if Assigned(FSubtaskCheckCancel) then
      aThread.SubTasksCheckCancel := FSubtaskCheckCancel;
    {$ENDIF}
    aThread.ShowProgress := true;
    aThread.OnTerminate := @AnalyzeUserTreeDone;
    FAnalysisInfo.ARP.Thread := aThread;
    aThread.FreeOnTerminate := False;
    aThread.Start;
    status := aThread.WaitFor;
    if status <> 0 then
      raise Exception.Create('TMLTreeAnalyzeThread returned failure code: ' + IntToStr(status));
    Result := True;
  finally
    if Assigned(aThread) then
      aThread.Free;
  end;
end;

procedure TCalcPosteriorProbsThread.AnalyzeUserTreeDone(aThread: TObject);
var
  t: TMLTreeAnalyzeThread;
  aData: TTreeData;
begin
  t := TMLTreeAnalyzeThread(aThread);
  FPosteriorProbs[FIndex] := FAnalysisInfo.MyMLAnalysisPack.GetExpectedStateProbArray(0);
  aData := TTreeData.Create(t.MLTreeAnalyzer.NoOfSeqs, True, False, False);
  t.MLTreeAnalyzer.GetTreeData(aData);
  FTrees.Add(aData);
  FAnalysisInfo.MyMLAnalysisPack.SubTaskCheckCancel := nil;
  if Assigned(FAnalysisInfo.MyMLAnalysisPack.MLTree) then
    FAnalysisInfo.MyMLAnalysisPack.MLTree.SubtaskCheckCancel := nil;
  FAnalysisInfo.MyMLAnalysisPack.Free;
  FAnalysisInfo.MySeqStrings := nil; { freed by TAnalysisInfo.MyMLAnalysisPack}
end;

constructor TCalcPosteriorProbsThread.Create(MAI: TAnalysisInfo; Newicks: TStringList; Names: TList; SeqStrings: TStringList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FAnalysisInfo := MAI;
  FTrees := TList.Create;
  FOtuNamesLists := Names;
  FOrigOtuNames := TStringList(FOtuNamesLists[0]);
  FNewickStrings := Newicks;
  FSeqStrings := SeqStrings;
  FGroupNames := TStringList.Create
end;

destructor TCalcPosteriorProbsThread.Destroy;
begin
  if Assigned(FGroupNames) then
    FGroupNames.Free;
  { self does not own FTreeDataList, FAnalysisInfo, FSeqStrings, FOtuNamesLists, or FNewickStrings}
  inherited Destroy;
end;

procedure TCalcPosteriorProbsThread.Execute;
begin
  ReturnValue := 1;
  try
    FAnalysisInfo.SetupOutgroupMembers;
    FGroupNames.Assign(FAnalysisInfo.GroupInfo.GroupNames);
    FIsSuccess := AnalyzeAllTrees;
    if not FIsSuccess then
      FMsgLog.Add('Analysis of pruned trees failed unexpectedly')
    else
      ReturnValue := 0;
    {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
    UpdatePeakMemUsage;
    {$ENDIF}{$ENDIF}
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      FMsgLog.Add('An error occurred when analyzing pruned trees: ' + E.Message);
    end;
  end;
end;

{ TEpThread }

procedure TEpThread.SetAnalysisInfo(AValue: TAnalysisInfo);
begin
  if FAnalysisInfo=AValue then Exit;
  FAnalysisInfo:=AValue;
end;

procedure TEpThread.ClearPrunedTrees;
var
  i: Integer;
begin
  if not Assigned(FPrunedTrees) then
    Exit;
  if FPrunedTrees.Count > 0 then
    for i := 0 to FPrunedTrees.Count - 1 do
      TTreeData(FPrunedTrees[i]).Free;
  FPrunedTrees.Clear;
end;

procedure TEpThread.ClearOtuNamesLists;
var
  i: Integer;
begin
  if FOtuNameLists.Count > 0 then
    for i := 0 to FOtuNameLists.Count - 1 do
      TStringList(FOtuNameLists[i]).Free;
  FOtuNameLists.Clear;
end;

procedure TEpThread.ClearReltimeTrees;
var
  i: Integer;
begin
  if FReltimeTrees.Count > 0 then
    for i := 0 to FReltimeTrees.Count - 1 do
      TTreeData(FReltimeTrees[i]).Free;
  FReltimeTrees.Clear;
end;

procedure TEpThread.SetCheckCancelFunc(AValue: TProgressCheckCancelFunc);
begin
  if FCheckCancelFunc=AValue then Exit;
  FCheckCancelFunc:=AValue;
end;

procedure TEpThread.SetSubTasksCheckCancel(AValue: TCheckCancelFunc);
begin
  if FSubTasksCheckCancel=AValue then Exit;
  FSubTasksCheckCancel:=AValue;
end;

function TEpThread.WeightsNullifierToStringList: TStringList;
var
  i,j: Integer;
  line: String;
begin
  Result := TStringList.Create;
  for i := 0 to Length(FWeightsNullifier[0]) - 1 do
  begin
    line := IntToStr(i + 1) + #9;
    for j := 0 to Length(FWeightsNullifier) - 1 do
      line := line + IntToStr(FWeightsNullifier[j][i]) + #9;
    line := Trim(line);
    Result.Add(line);
  end;
end;

function TEpThread.UngappedTimesToStringList: TStringList;
var
  step, site: Integer;
  line: String;
begin
  Result := TStringList.Create;
  line := 'site';
  for step := 0 to Length(FUngappedTimes) - 1 do
    line := line + #9 + IntToStr(step + 1);
  Result.Add(line);

  for site := 0 to Length(FUngappedTimes[0]) - 1 do
  begin
    line := IntToStr(site + 1) + #9;
    for step := 0 to Length(FUngappedTimes) - 1 do
      line := line + Format('%.6f', [FUngappedTimes[step][site]]) + #9;
    Result.Add(Trim(line));
  end;
end;

function TEpThread.NormalizedTimesToStringList: TStringList;
var
  site, step: Integer;
  line: String;
begin
  Result := TStringList.Create;
  line := 'site';
  for step := 0 to Length(FNormalizedTimes[0]) - 1 do
    line := line + ',' + IntToStr(step + 1);
  Result.Add(line);
  for site := 0 to Length(FNormalizedTimes) - 1 do
  begin
    line := IntToStr(site + 1);
    for step := 0 to Length(FNormalizedTimes[site]) - 1 do
      line := line + ',' + Format('%.6f', [FNormalizedTimes[site][step]]);
    Result.Add(line);
  end;
end;

function TEpThread.PosteriorProbsToDebugFiles: Boolean;
var
  iter, site, state: Integer;
  aList: TStringList = nil;
  line: String;
  filename: String;
begin
  {$IFDEF DEBUG}
  Result := True;
  try
    aList := TStringList.Create;
    for iter := 0 to Length(FPosteriorProbs) - 1 do
    begin
      aList.Clear;
      aList.Add('Predicted Living sequence for #1,');
      aList.Add('Site #,Probability,');
      line := ' ,';
      for state := 0 to FNumStates - 1 do
        line := line + Format('%s,', [FPosteriorProbs[iter][0][state].Name[1]]);
      aList.Add(line);
      for site := 0 to FAnalysisInfo.NoOfSites - 1 do
      begin
        line := IntToStr(site + 1) + ',';
        for state := 0 to FNumStates - 1 do
          line := line + Format('%.8f,', [FPosteriorProbs[iter][site][state].Prob]);
        aList.Add(line);
      end;
      filename := DebugFilename(Format('gene_%.3d_AS.csv', [iter]));
      filename := ExtractFileDir(filename) + PathDelim + Format('gene_%.3d_AS.csv', [iter]);
      aList.SaveToFile(filename);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
  {$ENDIF}
end;

function TEpThread.PosteriorProbsToSpreadsheet: TExcelWrite;
var
  iter, site, state: Integer;
begin
  {$IFDEF DEBUG}
  Result := TExcelWrite.Create(nil, 'iter-1');
  Result.IsXLS := True;
  for iter := 0 to Length(FPosteriorProbs) - 1 do
  begin
    if iter > 0 then
      Result.AddWorksheet(Format('iter-%d', [iter+1]));
    Result.Add('site');
    for state := 0 to FNumStates - 1 do
      Result.Add(FPosteriorProbs[iter][0][state].Name);
    Result.WriteLine(iter);
    for site := 0 to FAnalysisInfo.NoOfSites - 1 do
    begin
      Result.Add(site + 1);
      for state := 0 to FNumStates - 1 do
        Result.Add(FPosteriorProbs[iter][site][state].Prob);
      Result.WriteLine(iter);
    end;
  end;
  {$ENDIF}
end;

procedure TEpThread.NewickStringsToDebugFiles;
var
  i: Integer;
  aList: TStringList = nil;
  {$IFNDEF VISUAL_BUILD}filename: String;{$ENDIF}
begin
  try
    if FNewickStrings.Count > 0 then
    begin
      aList := TStringList.Create;
      for i := 0 to FNewickStrings.Count - 1 do
      begin
        aList.Clear;
        aList.Add(FNewickStrings[i]);
        {$IFDEF VISUAL_BUILD}
        FNewickStrings.SaveToFile(NextAvailableFilename(GetCurrentDir + Format('_debug_pruned_trees_%d.txt', [i+1])));
        {$ELSE}
        filename := NextAvailableFilenameNV(Format('_debug_pruned_trees_%d.nwk', [i+1]));
        filename := ExtractFileDir(filename) + PathDelim + Format('gene_%.3d.nwk', [i]);
        aList.SaveToFile(filename);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TEpThread.UngappedTimesToDebugFile;
var
  aList: TStringList;
  filename: String;
begin
  {$IFDEF DEBUG}
  aList := UngappedTimesToStringList;
  filename := DebugFilename('_ungapped_times.txt');
  aList.SaveToFile(filename);
  aList.Free;
  {$ENDIF}
end;

procedure TEpThread.NormalizedTimesToDebugFile;
var
  aList: TStringList;
  filename: String;
begin
  {$IFDEF DEBUG}
  aList := NormalizedTimesToStringList;
  filename := DebugFilename('_normalized_times.txt');
  aList.SaveToFile(filename);
  aList.Free;
  {$ENDIF}
end;

procedure TEpThread.DoCheckCancelFunc;
begin
  if Assigned(FCheckCancelFunc) then
  begin
    FCancelled := FCheckCancelFunc(FProgress);
    if FCancelled then
      Terminate;
  end;
end;

procedure TEpThread.DoSubtaskCheckCancelFunc;
begin
  if Assigned(FSubTasksCheckCancel) then
    FCancelled := FSubTasksCheckCancel(FSubtaskProgress, FSubtaskStatus);
  if FCancelled then
    Terminate;
end;

function TEpThread.UpdateOverallProgress(aProgress: Integer): Boolean;
begin
  FProgress := aProgress;
  Synchronize(@DoCheckCancelFunc);
  Result := FCancelled;
end;

function TEpThread.DebugFilename(fileExt: String): String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := GetCurrentDir + fileExt;
  Result := NextAvailableFilename(Result);
  {$ELSE}
  Result := NextAvailableFilenameNV(fileExt);
  {$ENDIF}
end;

procedure TEpThread.OnCalcProbsFinished(aThread: TObject);
var
  t: TCalcPosteriorProbsThread;
begin
  t := TCalcPosteriorProbsThread(aThread);
  if not t.IsSuccess then
    raise Exception.Create(t.MsgLog.Text);
  FAnalyzedTrees := t.TreeDataList;
  FPosteriorProbs := t.PosteriorProbs;
  FAnalysisInfo.MyMLAnalysisPack := nil;
  FProgress := 90;
  FStatus := 'Calculating EP Values';
  Synchronize(@DoCheckCancelFunc);
end;

procedure TEpThread.Execute;
var
  aData: TTreeData;
begin
  ReturnValue := 1;
  try
    FIsSuccess := PruneTrees;
    if FIsSuccess then
    begin
      FNewickStrings.Delete(FNewickStrings.Count - 1);
      aData := TTreeData(FPrunedTrees[FPrunedTrees.Count - 1]);
      aData.Free;
      FPrunedTrees.Delete(FPrunedTrees.Count - 1);
      CalculatePosteriorProbs;
      CalculateReltimes;

      if FIsSuccess then
      begin
        FUngappedTimes := GetUngappedTimes;
        CalcNormalizedTimes;
        CalculateEpValues;
        ReturnValue := 0;
      end;
      FAnalysisInfo.MyNoOfSeqs := FOrigNumTaxa;
      FAnalysisInfo.ARP.Hide;
    end
    else
      raise Exception.Create('failed to prune tree');
  except
    on E:Exception do
    begin
      FMsgLog.Add('An error occurred while calculating EP values: ' + E.Message);
      {$IFNDEF VISUAL_BUILD}
      FMsgLog.SaveToFile(NextAvailableFilenameNV('.txt'));
      {$ENDIF}
      FIsSuccess := False;
    end;
  end;
end;

function TEpThread.GetXlOutput: TExcelWrite;
begin
  Result := TExcelWrite.Create(nil, 'Evolutionary Probabilities');
  Result.IsXLS := True;
  GenerateOutput(Result);
end;

function TEpThread.PruneTrees: Boolean;
begin
  Result := False;
  Assert(Assigned(FAnalysisInfo) and Assigned(FAnalysisInfo.MyOriTreeList) and FAnalysisInfo.MyOriTreeList.IsRooted);
  if not FAnalysisInfo.MyOriTreeList.isRooted then
    raise Exception.Create('EP calculation requires a rooted tree but the input tree is not rooted');
  FTreePruner.SetTreeData(FAnalysisInfo.MyOriTreeList[0], True, FAnalysisInfo.MyOriTreeList.OTUNameList);
  FPrunedTrees := FTreePruner.GetPrunedEpTreeList(FNewickStrings, FOtuNameLists);
  Result := (FPrunedTrees.Count > 0);
end;

function TEpThread.SumOfPosteriorProbs(site: Integer; state: Integer): Double;
var
  iter: Integer;
begin
  Result := 0.0;
  for iter := 0 to FPrunedTrees.Count - 1 do
    Result := Result + FPosteriorProbs[iter][site][state].Prob;
end;

procedure TEpThread.CalculatePosteriorProbs;
var
  aThread: TCalcPosteriorProbsThread = nil;
  status: Integer;
begin
  try
    aThread := TCalcPosteriorProbsThread.Create(AnalysisInfo, FNewickStrings, FOtuNameLists, FSeqStrings);
    aThread.OnTerminate := @OnCalcProbsFinished;
    aThread.SubtaskCheckCancel := SubTasksCheckCancel;
    aThread.CheckCancel := @UpdateOverallProgress;
    aThread.FreeOnTerminate := False;
    aThread.Start;
    status := aThread.WaitFor;
    if status <> 0 then
      raise Exception.Create('TCalcPosteriorProbsThread returned failure status: ' + IntToStr(status));
  finally
    if Assigned(aThread) then
      aThread.Free;
  end;
end;

procedure TEpThread.CalculateReltimes;
var
  i: Integer;
  aData: TTreeData;
  reltimeTree: TTreeData;
begin
  SetLength(FWeights, FAnalyzedTrees.Count);
  FSumOfWeights := 0.0;
  if FAnalyzedTrees.Count > 0 then
    for i := 0 to FAnalyzedTrees.Count - 1 do
    begin
      aData := TTreeData(FAnalyzedTrees[i]);
      reltimeTree := FReltimeComputer.ComputeRelTimeBLens(aData, AnalysisInfo.MaxRateRatio);
      if not Assigned(reltimeTree) then
        raise Exception.Create(FReltimeComputer.Log.Text);
      FReltimeTrees.Add(reltimeTree);
      FWeights[i] := reltimeTree.SBL;
      FSumOfWeights := FSumOfWeights + FWeights[i];
    end;
end;

procedure TEpThread.CalculateEpValues;
var
  numStates: Integer;
  state, site: Integer;
begin
  if FAnalysisInfo.isAminoAcid then
    numStates := 20
  else
    numStates := 4;
  SetLength(FEpValues, numStates);

  for state := 0 to Length(FEpValues) - 1 do
  begin
    SetLength(FEpValues[state], AnalysisInfo.NoOfSites);
    for site := 0 to AnalysisInfo.NoOfSites - 1 do
      FEpValues[state][site] := 0.0;
  end;
  for site := 0 to AnalysisInfo.NoOfSites - 1 do
    for state := 0 to numStates - 1 do
      FEpValues[state][site] := CalculateEpValue(site, state);
end;

function TEpThread.CalculateEpValue(site: Integer; state: Integer): Extended;
var
  iter: Integer;
  w: Extended;
begin
  Result := 0.0;
  w := 0.0;
  for iter := 0 to FPrunedTrees.Count - 1 do
  begin
    w := w + FNormalizedTimes[site][iter];
    Result := Result + FPosteriorProbs[iter][site][state].Prob*FNormalizedTimes[site][iter];
  end;
  if w > 0.0 then
    Result := Result/w;
end;

function TEpThread.GetUngappedTimes: T2DExtArray;
var
  step, site, seq: Integer;
  allNames: TStringList;
  droppedNames: TStringList = nil;
  aData: TTreeData;
  aSeqs: TStringList = nil;
  prevNames: String;
begin
  SetLength(Result, FReltimeTrees.Count);
  for step := 0 to Length(Result) - 1 do
  begin
    SetLength(Result[step], Length(FSeqStrings[0]));
    for site := 0 to Length(Result[step]) - 1 do
      Result[step][site] := 0.0;
  end;
  aSeqs := TStringList.Create;
  droppedNames := TStringList.Create;

  FSubtaskStatus := 'Getting ungapped times';
  for step := 0 to FReltimeTrees.Count - 1 do
  begin
    if Assigned(FSubTasksCheckCancel) then
    begin
      FSubtaskProgress := Round(step/FReltimeTrees.Count*100);
      Synchronize(@DoSubtaskCheckCancelFunc);
    end;
    aData := TTreeData(FReltimeTrees[step]);
    allNames := TStringList(FOtuNameLists[step]);
    aSeqs.Clear;
    droppedNames.Clear;
    prevNames := EmptyStr;

    GetSeqsForNames(allNames, aSeqs);
    for site := 0 to Length(Result[step]) - 1 do
    begin
      droppedNames.Clear;
      for seq := 0 to aSeqs.Count - 1 do
      begin
        if aSeqs[seq][site+1] = '-' then
          droppedNames.Add(allNames[seq]);
      end;

      if droppedNames.Count = 0 then
        Result[step][site] := aData.SBL
      else
      begin
        if (droppedNames.Text = prevNames) and (prevNames <> EmptyStr) then
          Result[step][site] := Result[step][site - 1]
        else
        begin
          prevNames := droppedNames.Text;
          Result[step][site] := GetUngappedTime(aData, allNames, droppedNames);
        end;
      end;
    end;
  end;
end;

function TEpThread.GetUngappedTime(aData: TTreeData; allNames: TStringList;
  droppedNames: TStringList; IsDebug: Boolean): Extended;
var
  adapter: TSimpleTreeDataAdapter = nil;
begin
  if (allNames.Count - DroppedNames.Count) < 2 then
  begin
    Result := 0.0;
    Exit;
  end;

  try
    Result := aData.SBL;
    if droppedNames.Count > 0 then
    begin
      adapter := TSimpleTreeDataAdapter.Create;
      adapter.SetTreeData(aData, True, allNames);
      adapter.RemoveLeafNodes(droppedNames);
      Result := adapter.SumOfBlens;
    end;
  finally
    if Assigned(adapter) then
      adapter.Free;
  end;
end;

procedure TEpThread.CalcNormalizedTimes;
var
  site, step: Integer;
  tempSum: Extended;
begin
  SetLength(FNormalizedTimes, Length(FSeqStrings[0]));
  for site := 0 to Length(FNormalizedTimes) - 1 do
  begin
    SetLength(FNormalizedTimes[site], Length(FUngappedTimes));
    tempSum := 0.0;
    for step := 0 to Length(FNormalizedTimes[site]) - 1 do
    begin
      FNormalizedTimes[site][step] := FUngappedTimes[step][site];
      tempSum := tempSum + FNormalizedTimes[site][step];
    end;
    if CompareValue(tempSum, 0.0, 0.0000001) > 0 then
      for step := 0 to Length(FNormalizedTimes[site]) - 1 do
        FNormalizedTimes[site][step] := FNormalizedTimes[site][step]/tempSum;
  end;
end;

procedure TEpThread.GetSeqsForNames(const names: TStringList; var Seqs: TStringList);
var
  i, index: Integer;
  oriNames: TStringList;
begin
  Seqs.Clear;
  oriNames := TStringList(FOtuNameLists[0]);
  if names.Count > 0 then
    for i := 0 to names.Count - 1 do
    begin
      index := oriNames.IndexOf(names[i]);
      Seqs.Add(FSeqStrings[index]);
    end;
end;

procedure TEpThread.GetPrunedNames(const NamesBeforePruning: TStringList; const NamesAfterPruning: TStringList; var ResultList: TStringList);
var
  i: Integer;
  index: Integer;
begin
  ResultList.Assign(NamesBeforePruning);
  if NamesAfterPruning.Count > 0 then
    for i := NamesAfterPruning.Count - 1 downto 0 do
    begin
      index := ResultList.IndexOf(NamesAfterPruning[i]);
      ResultList.Delete(index);
    end;
end;

function TEpThread.GetTextOutput: TStringList;
var
  site, state: Integer;
  line: String;
  caption: TStringList = nil;
begin
  try
    Result := TStringList.Create;
    line := 'site' + #9;
    for state := 0 to FNumStates - 1 do
    begin
      line := line + FPosteriorProbs[0][0][state].Name[1];
      if state < FNumStates - 1 then
        line := line + '         ' + #9;
    end;
    Result.Add(line);
    for site := 0 to FAnalysisInfo.NoOfSites - 1 do
    begin
      line := Format('%d' + #9, [site + 1]);
      for state := 0 to FNumStates - 1 do
      begin
        line := line + Format('%.8f', [FEpValues[state][site]]);
        if state < FNumStates - 1 then
          line := line + #9;
      end;
      Result.Add(line);
    end;
    Result.Add(EmptyStr);
    {$IFDEF VISUAL_BUILD}
    caption := GenerateCaption;
    Result.AddStrings(caption);
    {$ENDIF}
  finally
    if Assigned(caption) then
      caption.Free;
  end;
end;

procedure TEpThread.GenerateOutput(var xl: TExcelWrite);
var
  site, state: Integer;
  caption: TStringList;
begin
  try
    xl.Add('site');
    for state := 0 to FNumStates - 1 do
      xl.Add(FPosteriorProbs[0][0][state].Name[1]);
    xl.WriteLine();

    for site := 0 to FAnalysisInfo.NoOfSites - 1 do
    begin
      xl.Add(site + 1);
      for state := 0 to FNumStates - 1 do
        xl.Add(FEpValues[state][site]);
      xl.WriteLine();
    end;
    caption := GenerateCaption;
    xl.AddCaptionAsWorksheet(caption);
  finally
    if Assigned(caption) then
      caption.Free;
  end;
end;

function TEpThread.GenerateCaption: TStringList;
var
  lg: TLegendGenerator = nil;
begin
  try
    try
      lg := TLegendGenerator.Create;
      lg.LoadTemplateFromFile('evolutionary_probabilities.htm');
      lg.BindData(FAnalysisInfo);
      lg.BindData(FAnalysisInfo.MyDistPack);
      lg.BindData(FAnalysisInfo.MyTreePack);
      lg.AssignData('NoOfSeqs', IntToStr(FAnalysisInfo.NoOfSeqs));
      lg.AssignData('TaxonName', FAnalysisInfo.MyOtuNames[0]);
      lg.AssignData('NoOfSites', IntToStr(FAnalysisInfo.NoOfSites));

      if FAnalysisInfo.MyDistPack.DoesContain(gdGamma) then
        lg.AssignData('NoOfGCats', IntToStr(FAnalysisInfo.NoOfGammaCat))
      else
        lg.AssignData('GammaPara', 'N/A');
      if not FAnalysisInfo.MyDistPack.DoesContain(gdInvar) then
        lg.AssignData('PropOfInvariant', 'N/A');
      Result := TStringList.Create;
      {$IFDEF VISUAL_BUILD}
      lg.GenerateLegendAsText(Result);
      {$ELSE}
      Result.Text := lg.GenerateLegend;
      {$ENDIF}
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
         ShowMessage('Oh no! Failed to generate caption: ' + E.Message);
      {$ELSE}
         Warn_nv('Failed to generate caption: ' + E.Message);
      {$ENDIF}
    end;
  finally
    if Assigned(lg) then
      lg.Free;
  end;
end;

constructor TEpThread.Create(MAI: TAnalysisInfo);
begin
  inherited Create(True);
  FCancelled := False;
  FProgress := 0;
  FStatus := EmptyStr;
  FreeOnTerminate := True;
  FTreePruner := TSimpleTreeDataAdapter.Create;
  AnalysisInfo := MAI;
  FMsgLog := TStringList.Create;
  FPrunedTrees := TList.Create;
  FNewickStrings := TStringList.Create;
  FOtuNameLists := TList.Create;
  FSeqStrings := TStringList.Create;
  FSeqStrings.Assign(MAI.MySeqStrings);
  FReltimeComputer := TReltimeComputer.Create;
  FReltimeTrees := TList.Create;
  FOrigNumTaxa := MAI.MyOriTreeList.NoOfOTUs;
  if FAnalysisInfo.isAminoAcid then
    FNumStates := 20
  else
    FNumStates := 4;
end;

destructor TEpThread.Destroy;
begin
  if Assigned(FAnalysisInfo) and Assigned(FAnalysisInfo.ARP) then
  begin
    //FAnalysisInfo.ARP.Free; { TODO 1 -oglen -cbugs : FAnalysisInfo.ARP needs to be freed but doing so is leading to a weird crash bug }
    FAnalysisInfo.ARP := nil;
  end;
  if Assigned(FAnalysisInfo) then
    FAnalysisInfo.Free;
  if Assigned(FTreePruner) then
    FTreePruner.Free;
  if Assigned(FMsgLog) then
    FMsgLog.Free;
  if Assigned(FPrunedTrees) then
  begin
    ClearPrunedTrees;
    FPrunedTrees.Free;
  end;
  if Assigned(FOtuNameLists) then
  begin
    ClearOtuNamesLists;
    FOtuNameLists.Free;
  end;
  if Assigned(FSeqStrings) then
    FSeqStrings.Free;
  if Assigned(FNewickStrings) then
    FNewickStrings.Free;
  if Assigned(FReltimeComputer) then
    FReltimeComputer.Free;
  if Assigned(FReltimeTrees) then
  begin
    ClearReltimeTrees;
    FReltimeTrees.Free;
  end;
  inherited Destroy;
end;

end.

