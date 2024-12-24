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

unit ShowTrees;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Dialogs, MAnalysisInfo, MegaConsts, Forms, MLTree, Classes, MTreeData,
  MTreeList, mcorrelationtest, AppLinker;

procedure ShowDistTree(MAI : TAnalysisInfo);
procedure ShowDistBootTree(MAI: TAnalysisInfo);
procedure ShowParsimTree(MAI : TAnalysisInfo);
procedure ShowParsimBootTree(MAI: TAnalysisInfo);
procedure ShowMLTree(MAI : TAnalysisInfo);
function ShowMLBootTree(MAI: TAnalysisInfo): Boolean;
procedure ShowModelTestResults(MAI :TAnalysisInfo; aType: TExportType = EXnone; aLocation: String = '');
procedure ShowClockTestResults(MAI :TAnalysisInfo);
procedure ShowCorrTestMLResult(MAI: TAnalysisInfo; aResult: TCorrelationTest);
procedure ShowCorrTestBLenResult(MAI: TAnalysisInfo; aResult: TCorrelationTest);
procedure ShowRelTimeBLenResults(MAI: TAnalysisInfo);
procedure ShowRelTimeMLResults(MAI: TAnalysisInfo; corrTest: TCorrelationTest = nil);
procedure ShowRelTimeLSResults(MAI: TAnalysisInfo);
procedure ShowPatternResults(MAI :TAnalysisInfo);
procedure ShowSiteRateResults(MAI :TAnalysisInfo);
procedure ShowGeneDupsTree(AnalysisInfo: TAnalysisInfo);
function ShowTreeFromFile(Value: String=''):TForm;
function ShowTreeFromTreeList(AList: TTreeList; filename: String): TObject;
{$IFDEF VISUAL_BUILD}
function DoSessionTest(t: TObject): Boolean;
{$ENDIF}

implementation

uses
  LCLIntf, LCLType,
  {$IFDEF VISUAL_BUILD}
  Mega_Main, MTreeViewForm,
  {$ELSE}
  MegaUtils,  MProcessPack,
  MegaUtils_NV, mancestralstatesexporter, mancestralstatesexportheader, MD_MegaMain,{$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ENDIF}
  SysUtils, MegaErrUtils, Graphics, MegaVerConsts, MLegendGenerator,
  MTreeDataAdapter, mtree_display_setup;


procedure ShowDistTree(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowDistTree(MAI);
    finally
      if Assigned(setup) then
        setup.Free;
    end;
end;

procedure ShowDistBootTree(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowDistBootTree(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowParsimTree(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowParsimTree(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowParsimBootTree(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowParsimBootTree(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

{$IFDEF VISUAL_BUILD}
function DoSessionTest(t: TObject): Boolean;
var
  filename: String;
  te: TTreeViewForm;
  aMsg: String = '';
begin
  try
    te := TTreeViewForm(t);
    filename := GetTempFileName;
    filename := ChangeFileExt(filename, '.mtsx');
    Result := te.SaveSession(filename, aMsg);
    if not Result then
      raise Exception.Create('tree session (SAVE) file test failed');
    Result := te.RetrieveSession(filename);
    if not Result then
      raise Exception.Create('tree session (LOAD) file test failed');
  finally
    if FileExists(filename) then
      DeleteFile(filename);
    te.Close;
    Halt(0);
  end;
end;
{$ENDIF}

procedure ShowMLTree(MAI : TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowMLTree(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

function ShowMLBootTree(MAI: TAnalysisInfo): Boolean;
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    Result := setup.ShowMLBootTree(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowCorrTestMLResult(MAI: TAnalysisInfo; aResult: TCorrelationTest);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowCorrTestMLResult(MAI, aResult);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowCorrTestBLenResult(MAI: TAnalysisInfo; aResult: TCorrelationTest);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowCorrTestBLenResult(MAI, aResult);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowRelTimeBLenResults(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowRelTimeBLenResults(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowRelTimeMLResults(MAI: TAnalysisInfo; corrTest: TCorrelationTest = nil);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowRelTimeMLResults(MAI, corrTest);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowRelTimeLSResults(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowRelTimeLSResults(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowClockTestResults(MAI: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowClockTestResults(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure ShowSiteRateResults(MAI :TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowSiteRateResults(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;


procedure ShowGeneDupsTree(AnalysisInfo: TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
{$IFDEF VISUAL_BUILD}
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowGeneDupsTree(AnalysisInfo);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
  {$ENDIF}
end;


function ShowTreeFromFile(Value: String=''):TForm;
{$IFDEF VISUAL_BUILD}
var
  TreeExpl : TTreeViewForm;
  {$ENDIF}
begin
{$IFDEF VISUAL_BUILD}
  Result := nil;
  TreeExpl := nil;
  try try
    TreeExpl := TTreeViewForm.Create(Application);
    // Disable the caption expert in this case (change to use IndexOf or tie to action)
    {$IFNDEF CALTEST}
    TreeExpl.MainMenu.Items[0].Items[5].Enabled := False;
    with TreeExpl do
    begin
      if Length(Value) = 0 then
      begin
       if not FileOpenDialog.Execute then exit;
       Value := FileOpenDialog.FileName;
      end;

      if Pos(lowercase(ExtractFileExt(Value)), MtsExts) > 0 then
      begin
        if TreeExpl.RetrieveSession(Value) then
        begin
          TreeExpl.Show;
          TreeExpl.RefreshCaption;
        end;
      end
      else
      begin
        if not TreeExpl.ImportNewickStandard(Value) then
        begin
          MessageDlg('MEGA encountered a problem importing your newick file, it may not be a valid newick standard file.', mtError, [mbOK], 0);
          exit;
        end;
        TreeExpl.Show;
      end;
      {$IFDEF MYPEG_ONLY}
      Caption := 'Tree Explorer ('+ Value + ')';
      {$ELSE}
      Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Explorer ('+ ExtractFileName(Value) + ')';
      {$ENDIF}
    end;
    {$ENDIF CALTEST}
    Result := TreeExpl;
    {$IFDEF DARWIN}TreeExpl.Tree.Refresh;{$ENDIF}
    TreeExpl := nil;
  except
    On E: Exception do
    begin
      if IsSessionTest then
        raise Exception.Create(E.Message)
      else
        ShowErrorMessage(E);
    end;
  end;
  finally
    if TreeExpl <> nil then
      TreeExpl.Free;
  end;
  {$ENDIF}
end;

procedure ShowPatternResults(MAI :TAnalysisInfo);
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowPatternResults(MAI);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

function SortModelsByName(item1: pointer; item2: pointer): integer;
begin
  result := ANSICompareText(TModelInfo(item1^).ModelName, TModelInfo(item2^).ModelName);
end;


procedure ShowModelTestResults(MAI: TAnalysisInfo; aType: TExportType = EXnone; aLocation: String = '');
var
  setup: TTreeDisplaySetup = nil;
begin
  try
    setup := TTreeDisplaySetup.Create;
    setup.ShowModelTestResults(MAI, aType, aLocation);
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

function ShowTreeFromTreeList(AList: TTreeList; filename: String): TObject;
{$IFDEF VISUAL_BUILD}
var
  TreeExpl : TTreeViewForm;
  {$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  TreeExpl := nil;
  try
    try
      TreeExpl := TTreeViewForm.Create(Application);
      Result := TreeExpl;
        TreeExpl.Show;
        if not TreeExpl.ImportTreeList(AList, True, filename) then
          MessageDlg('MEGA encountered a problem importing your newick file, it may not be a valid newick standard file.', mtError, [mbOK], 0)
        else
        begin
          {$IFDEF DARWIN}TreeExpl.Tree.Refresh;{$ENDIF}
          TreeExpl := nil;
        end;
    except
      On E: Exception do
        ShowMessage('Oh no! An error occurred when loading the newick tree: ' + E.Message);
    end;
  finally
    if TreeExpl <> nil then
      TreeExpl.Free;
  end;
  {$ENDIF}
end;

end;

end.

