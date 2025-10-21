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

unit mtimetree_search_frame;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, ActnList, StdCtrls,
  Dialogs, Buttons, mtimetree_map, MegaConsts, graphics, Grids, Menus, Types;

type

  { TTimetreeSearchFrame }

  TTimetreeSearchFrame = class(TFrame)
    ApplyCalibrationAction: TAction;
    CalibrateNodeBtn: TSpeedButton;
    SearchButton: TSpeedButton;
    StudiesBtn: TSpeedButton;
    StayOnTopCheckbox: TCheckBox;
    CloseBtn: TSpeedButton;
    ConfidenceIntervalEdit: TEdit;
    FixedTimeRadioBtn: TRadioButton;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MaxTimeRadioBtn: TRadioButton;
    MedianTimeEdit: TEdit;
    MenuItem1: TMenuItem;
    MinTimeRadioBtn: TRadioButton;
    MrcaSearchDrawGrid: TDrawGrid;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressTimer: TTimer;
    TimeSearchAction: TAction;
    ActionList1: TActionList;
    TimeSearchFeedbackLabel: TLabel;
    UniformDistRadioBtn: TRadioButton;
    procedure ApplyCalibrationActionExecute(Sender: TObject);
    procedure CalibrateNodeBtnClick(Sender: TObject);
    procedure CalibrateNodeBtnMouseEnter(Sender: TObject);
    procedure CalibrateNodeBtnMouseLeave(Sender: TObject);
    procedure CalibrateNodeBtnPaint(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure CloseBtnMouseEnter(Sender: TObject);
    procedure CloseBtnMouseLeave(Sender: TObject);
    procedure CloseBtnPaint(Sender: TObject);
    procedure MrcaSearchDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchButtonMouseEnter(Sender: TObject);
    procedure SearchButtonMouseLeave(Sender: TObject);
    procedure SearchButtonPaint(Sender: TObject);
    procedure StayOnTopCheckboxChange(Sender: TObject);
    procedure StudiesBtnClick(Sender: TObject);
    procedure StudiesBtnMouseEnter(Sender: TObject);
    procedure StudiesBtnMouseLeave(Sender: TObject);
    procedure StudiesBtnPaint(Sender: TObject);
    procedure TimeSearchActionExecute(Sender: TObject);
    procedure CalibrationTypeRadioBtnChange(Sender: TObject);

  private
    FCalibrateNodeButtonCaption: String;
    FIdleColor: TColor;
    FHoverColor: TColor;
    FGridHighlightColor: TColor;
    FActiveFontColor: TColor;
    FIgnoreEvents: Boolean;
    FCurrentTimeSearchResult: TTimeTreeMap;
    FAllTimeSearchResults: array of TTimeTreeMap;
    procedure UpdateProgressBar(aProgress: Integer);
    function SelectedCalibrationCategory: TCalibrationCategory;
    procedure TimeSearchThreadDone(aThread: TObject);
    procedure LaunchTimeSearchThread;
    procedure EnableTimeTools(value: Boolean);
    procedure AddTimeSearchResult(aResult: TTimeTreeMap);
    procedure ClearRadioButtons;
    procedure UpdateStudiesButton;
  public
    ProgressBar: TProgressBar;
    UpdateTimetreeMapProc: TUpdateTimetreeMapProc;
    CloseNotify: TNotifyEvent;
    NodeCalibratedNotify: TNotifyEvent;
    procedure Initialize(numNodes: Integer);
    function HasTimetreeMap(treeBoxIndex: Integer): Boolean;
    procedure SetTimetreeMap(treeBoxIndex: Integer); overload;
    procedure SetTimetreeMap(aMap: TTimeTreeMap); overload;
    procedure ClearTimetreeMap;
    function DebugTimeSearchResults: TStringList;
  end;

implementation

uses
  mtimetree_api, mstringbuilder, mtimetree_studies_export;

{$R *.lfm}

{ TTimetreeSearchFrame }

procedure TTimetreeSearchFrame.MrcaSearchDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  str: String = '';
  x: Integer = -1;
  y: Integer = -1;
  ttData: TTimeTreeTaxonData = nil;
  ts: TTextStyle;
begin
  if Assigned(FCurrentTimeSearchResult) and (aRow > FCurrentTimeSearchResult.Count) then
    Exit;
  with MrcaSearchDrawGrid.Canvas do
  begin
    ts := Canvas.TextStyle;
    ts.Layout := tlCenter;
    if aRow = 0 then
      ts.Alignment := taCenter
    else
      ts.Alignment := taLeftJustify;
    Font.Color := clBlack;
    Brush.Style := bsClear;

    x := aRect.Left + 2;
    y := aRect.Top + 2;
    if aRow = 0 then
    begin
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(aRect);

      if aCol = 0 then
        str := 'Scientific Name'
      else if aCol = 1 then
        str := 'Synonym'
      else if aCol = 2 then
        str := 'Taxonomic Rank'
      else if aCol = 3 then
        str := 'NCBI ID';
      Brush.Style := bsClear;
      TextRect(aRect, x, y, str, ts);
    end
    else
    begin
      if gdSelected in aState then
      begin
        Brush.Color := FGridHighlightColor;
        Font.Color := FHoverColor;
      end
      else
      begin
        Brush.Color := clWhite;
        Font.Color := clBlack;
      end;
      Brush.Style := bsSolid;
      FillRect(aRect);
      if Assigned(FCurrentTimeSearchResult) then
      begin
        ttData := FCurrentTimeSearchResult[aRow - 1];
        if aCol = 0 then
          str := ttData.TimetreeName
        else if aCol = 1 then
          str := ttData.Synonym
        else if aCol = 2 then
          str := ttData.TaxonomicRank
        else if aCol = 3 then
          str := Format('%d', [ttData.NcbiId]);
        Brush.Style := bsClear;
        TextRect(aRect, x, y, str, ts);
      end;
    end;
  end;
end;

procedure TTimetreeSearchFrame.ProgressTimerTimer(Sender: TObject);
begin
  if Assigned(ProgressBar) then
    UpdateProgressBar((ProgressBar.Position + ProgressBar.Step) mod ProgressBar.Max);
end;

procedure TTimetreeSearchFrame.SearchButtonClick(Sender: TObject);
begin
  TimeSearchActionExecute(Sender);
end;

procedure TTimetreeSearchFrame.SearchButtonMouseEnter(Sender: TObject);
begin
  SearchButton.Invalidate;
end;

procedure TTimetreeSearchFrame.SearchButtonMouseLeave(Sender: TObject);
begin
  SearchButton.Invalidate
end;

procedure TTimetreeSearchFrame.SearchButtonPaint(Sender: TObject);
var
  w: Integer = -1;
  h: Integer = -1;
begin
  with SearchButton.Canvas do
  begin
    if SearchButton.MouseInClient then
      Brush.Color := FHoverColor
    else
      Brush.Color := FIdleColor;
    Brush.Style := bsSolid;
    FillRect(ClientRect);
    Font.Color := clWhite;
    Font.Style := [fsBold];
    w := TextWidth('Get Time Estimate');
    h := TextHeight('Get Time Estimate');
    TextOut((SearchButton.Width - w) div 2, (SearchButton.Height - h) div 2, 'Get Time Estimate');
  end;
end;

procedure TTimetreeSearchFrame.StayOnTopCheckboxChange(Sender: TObject);
begin
  with Parent as TForm do
  begin
    if StayOnTopCheckbox.Checked then
      FormStyle := fsStayOnTop
    else
      FormStyle := fsNormal;
  end;
end;

procedure TTimetreeSearchFrame.StudiesBtnClick(Sender: TObject);
begin
  if Assigned(FCurrentTimeSearchResult) then
    ExportTimetreeStudies(FCurrentTimeSearchResult);
end;

procedure TTimetreeSearchFrame.StudiesBtnMouseEnter(Sender: TObject);
begin
  StudiesBtn.Invalidate;
end;

procedure TTimetreeSearchFrame.StudiesBtnMouseLeave(Sender: TObject);
begin
  StudiesBtn.Invalidate;
end;

procedure TTimetreeSearchFrame.StudiesBtnPaint(Sender: TObject);
var
  w: Integer = -1;
  h: Integer = -1;
  aText: String = '';
begin
  with StudiesBtn.Canvas do
  begin
    if StudiesBtn.MouseInClient then
      Brush.Color := FHoverColor
    else
      Brush.Color := FIdleColor;
    Brush.Style := bsSolid;
    FillRect(ClientRect);
    Font.Color := clWhite;
    Font.Style := [fsBold];
    aText := StudiesBtn.Caption;
    w := TextWidth(aText);
    h := TextHeight(aText);
    TextOut((StudiesBtn.Width - w) div 2, (StudiesBtn.Height - h) div 2, aText);
  end;
end;

procedure TTimetreeSearchFrame.TimeSearchActionExecute(Sender: TObject);
begin
  try
    UpdateProgressBar(0);
    ProgressTimer.Enabled := True;
    LaunchTimeSearchThread;
  except
    on E:Exception do
    begin
      TimeSearchFeedbackLabel.Caption := Format('The time search failed with the following message: %s', [E.Message]);
      ProgressTimer.Enabled := False;
      UpdateProgressBar(0);
    end;
  end;
end;

procedure TTimetreeSearchFrame.CalibrationTypeRadioBtnChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;

  try
    FIgnoreEvents := True;
    if Sender <> UniformDistRadioBtn then
      UniformDistRadioBtn.Checked := False
    else
    begin
      MinTimeRadioBtn.Checked := False;
      MaxTimeRadioBtn.Checked := False;
      FixedTimeRadioBtn.Checked := False;
    end;

    if Sender = UniformDistRadioBtn then
      ApplyCalibrationAction.Caption := 'Calibrate Target Node (Uniform Distribution)'
    else if Sender = MinTimeRadioBtn then
      ApplyCalibrationAction.Caption := 'Calibrate Target Node (Min Time Only)'
    else if Sender = MaxTimeRadioBtn then
      ApplyCalibrationAction.Caption := 'Calibrate Target Node (Max Time Only)'
    else if Sender = FixedTimeRadioBtn then
      ApplyCalibrationAction.Caption := 'Calibrate Target Node (Fixed Time)'
    else
      ShowMessage('Application error - missing handler for calibration type');
    FCalibrateNodeButtonCaption := ApplyCalibrationAction.Caption;
    CalibrateNodeBtn.Invalidate;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TTimetreeSearchFrame.UpdateProgressBar(aProgress: Integer);
begin
  if Assigned(ProgressBar) then
    ProgressBar.Position := aProgress;
end;

function TTimetreeSearchFrame.SelectedCalibrationCategory: TCalibrationCategory;
begin
  if UniformDistRadioBtn.Checked then
    Result := ccDensityUniform
  else if MinTimeRadioBtn.Checked then
    Result := ccMinTimeOnly
  else if MaxTimeRadioBtn.Checked then
    Result := ccMaxTimeOnly
  else if FixedTimeRadioBtn.Checked then
    Result := ccFixedTime
  else
    Result := ccNone;
end;

procedure TTimetreeSearchFrame.TimeSearchThreadDone(aThread: TObject);
var
  t: TTimetreeApiThread = nil;
begin
  ProgressTimer.Enabled := False;
  UpdateProgressBar(0);
  t := TTimetreeApiThread(aThread);
  if t.IsSuccess then
  begin
    AddTimeSearchResult(t.TimetreeMap);
    FCurrentTimeSearchResult := FAllTimeSearchResults[t.TimetreeMap.TreeBoxNodeId];
    Assert(FCurrentTimeSearchResult is TTimeTreeMap);
    UpdateStudiesButton;
    StudiesBtn.Left := SearchButton.Left + SearchButton.Width + 10;
    EnableTimeTools(True);
    TimeSearchFeedbackLabel.Caption := 'The following time estimate was found:';
    MedianTimeEdit.Text := Format('%.1f', [FCurrentTimeSearchResult.MedianTime]);
    ConfidenceIntervalEdit.Text := Format('%.1f - %.1f', [FCurrentTimeSearchResult.MinTime, FCurrentTimeSearchResult.MaxTime]);
  end
  else
  begin
    MedianTimeEdit.Text := 'N/A';
    ConfidenceIntervalEdit.Text := 'N/A';
    EnableTimeTools(False);
    TimeSearchFeedbackLabel.Caption := EmptyStr;
    if t.ErrorMsg <> EmptyStr then
      TimeSearchFeedbackLabel.Caption := Format('The time search failed with the following message: %s', [t.ErrorMsg])
    else
      TimeSearchFeedbackLabel.Caption := 'The time search failed with an unknown error';
    StudiesBtn.Caption := 'no studies found';
    StudiesBtn.Enabled := False;
  end;
end;

procedure TTimetreeSearchFrame.LaunchTimeSearchThread;
var
  t: TTimetreeApiThread = nil;
begin
  EnableTimeTools(False);
  t := TTimetreeApiThread.Create;
  t.InitializeForTimeSearch(FCurrentTimeSearchResult);
  t.OnTerminate := @TimeSearchThreadDone;
  t.Start;
end;

procedure TTimetreeSearchFrame.EnableTimeTools(value: Boolean);
begin
  MedianTimeEdit.Enabled := value;
  ConfidenceIntervalEdit.Enabled := value;
  ApplyCalibrationAction.Enabled := value;
  CalibrateNodeBtn.Enabled := value;
  UniformDistRadioBtn.Enabled := value;
  MinTimeRadioBtn.Enabled := value;
  MaxTimeRadioBtn.Enabled := value;
  FixedTimeRadioBtn.Enabled := value;
  if value = False then
    TimeSearchFeedbackLabel.Caption := EmptyStr
  else
    TimeSearchFeedbackLabel.Caption := 'The following time estimate was found:';
  Invalidate;
end;

procedure TTimetreeSearchFrame.AddTimeSearchResult(aResult: TTimeTreeMap);
var
  ttMap: TTimeTreeMap = nil;
begin
  Assert(aResult is TTimeTreeMap);
  Assert(aResult.TreeBoxNodeId > 0);
  if Assigned(FAllTimeSearchResults[aResult.TreeBoxNodeId]) then
  begin
    if FAllTimeSearchResults[aResult.TreeBoxNodeId] <> aResult then
      FAllTimeSearchResults[aResult.TreeBoxNodeId].Assign(aResult);
  end
  else
  begin
    ttMap := aResult.Clone;
    FAllTimeSearchResults[aResult.TreeBoxNodeId] := ttMap;
  end;
end;

procedure TTimetreeSearchFrame.ClearRadioButtons;
begin
  UniformDistRadioBtn.Checked := False;
  MinTimeRadioBtn.Checked := False;
  MaxTimeRadioBtn.Checked := False;
  FixedTimeRadioBtn.Checked := False;
  UniformDistRadioBtn.Checked := False;
end;

procedure TTimetreeSearchFrame.UpdateStudiesButton;
begin
  if FCurrentTimeSearchResult.NumStudies = 0 then
  begin
    StudiesBtn.Caption := 'no studies found';
    StudiesBtn.Enabled := False;
  end
  else if FCurrentTimeSearchResult.NumStudies = 1 then
  begin
    StudiesBtn.Caption := 'Details for 1 study';
    StudiesBtn.Enabled := True;
  end
  else
  begin
    StudiesBtn.Caption := Format('Details for %.0n studies', [FCurrentTimeSearchResult.NumStudies*1.0]);
    StudiesBtn.Enabled := True;
  end;
end;

procedure TTimetreeSearchFrame.Initialize(numNodes: Integer);
var
  i: Integer = -1;
begin
  FIgnoreEvents := False;
  UpdateProgressBar(0);
  TimeSearchFeedbackLabel.Caption := EmptyStr;
  FActiveFontColor := RGB(31, 156, 157);
  FIdleColor := $00b1a247;
  FHoverColor := $008c8225;
  FGridHighlightColor := $00f6f4e4;
  TimeSearchFeedbackLabel.Font.Color := $00003ac2;
  MrcaSearchDrawGrid.RowCount := 1;
  MrcaSearchDrawGrid.DefaultRowHeight;
  if Length(FAllTimeSearchResults) = 0 then
  begin
    SetLength(FAllTimeSearchResults, numNodes);
    for i := 0 to numNodes - 1 do
      FAllTimeSearchResults[i] := nil;
  end;
  FCalibrateNodeButtonCaption := ApplyCalibrationAction.Caption;
end;

function TTimetreeSearchFrame.HasTimetreeMap(treeBoxIndex: Integer): Boolean;
begin
  Result := (FAllTimeSearchResults[treeBoxIndex] <> nil);
end;

procedure TTimetreeSearchFrame.SetTimetreeMap(treeBoxIndex: Integer);
begin
  FCurrentTimeSearchResult := FAllTimeSearchResults[treeBoxIndex];
  Assert(Assigned(FCurrentTimeSearchResult));
  Assert(FCurrentTimeSearchResult is TTimeTreeMap);
  try
    FIgnoreEvents := True;
    if FCurrentTimeSearchResult.MedianTime > 0 then
    begin
      EnableTimeTools(True);
      MedianTimeEdit.Text := Format('%.1f', [FCurrentTimeSearchResult.MedianTime]);
      ConfidenceIntervalEdit.Text := Format('%.1f - %.1f', [FCurrentTimeSearchResult.MinTime, FCurrentTimeSearchResult.MaxTime]);
    end
    else
    begin
      MedianTimeEdit.Text := EmptyStr;
      ConfidenceIntervalEdit.Text := EmptyStr;
      EnableTimeTools(False);
    end;
    MrcaSearchDrawGrid.RowCount := FCurrentTimeSearchResult.Count + 1;
    ClearRadioButtons;
    case FCurrentTimeSearchResult.CalibrationType of
      ccDensityUniform:
        begin
          UniformDistRadioBtn.Checked := True;
          ApplyCalibrationAction.Caption := 'Calibrate Target Node (Uniform Distribution)';
        end;
      ccMinTimeOnly:
        begin
          MinTimeRadioBtn.Checked := True;
          ApplyCalibrationAction.Caption := 'Calibrate Target Node (Min Time Only)';
        end;
      ccMaxTimeOnly:
        begin
          MaxTimeRadioBtn.Checked := True;
          ApplyCalibrationAction.Caption := 'Calibrate Target Node (Max Time Only)';
        end;
      ccFixedTime:
        begin
          FixedTimeRadioBtn.Checked := True;
          ApplyCalibrationAction.Caption := 'Calibrate Target Node (Fixed Time)';
        end;
      ccNone:
        begin
          UniformDistRadioBtn.Checked := True;
          ApplyCalibrationAction.Caption := 'Calibrate Target Node (Uniform Distribution)';
        end;
    end;
    if FCurrentTimeSearchResult.MedianTime > 0 then
      TimeSearchFeedbackLabel.Caption := 'The following time estimate was found:';
    MrcaSearchDrawGrid.Invalidate;
    FCalibrateNodeButtonCaption := ApplyCalibrationAction.Caption;
    CalibrateNodeBtn.Invalidate;
    SearchButton.Enabled := True;
    UpdateStudiesButton;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TTimetreeSearchFrame.SetTimetreeMap(aMap: TTimeTreeMap);
var
  ttMap: TTimeTreeMap = nil;
begin
  if Assigned(FAllTimeSearchResults[aMap.TreeBoxNodeId]) then
  begin
    if FAllTimeSearchResults[aMap.TreeBoxNodeId] <> aMap then
    begin
      ttMap := FAllTimeSearchResults[aMap.TreeBoxNodeId];
      Assert(ttMap is TTimeTreeMap);
      FAllTimeSearchResults[aMap.TreeBoxNodeId].Assign(aMap);
    end;
    MrcaSearchDrawGrid.RowCount := FAllTimeSearchResults[aMap.TreeBoxNodeId].Count + 1;
  end
  else
  begin
    ttMap := aMap.Clone;
    FAllTimeSearchResults[aMap.TreeBoxNodeId] := ttMap;
    SetTimeTreeMap(aMap.TreeBoxNodeId);
  end;
  SearchButton.Enabled := True;
end;

procedure TTimetreeSearchFrame.ClearTimetreeMap;
begin
  FCurrentTimeSearchResult := nil;
  MrcaSearchDrawGrid.RowCount := 1;
  MrcaSearchDrawGrid.DefaultRowHeight;
  MedianTimeEdit.Text := EmptyStr;
  ConfidenceIntervalEdit.Text := EmptyStr;
  TimeSearchFeedbackLabel.Caption := 'No data available for the selected node.';
  SearchButton.Enabled := False;
end;

function TTimetreeSearchFrame.DebugTimeSearchResults: TStringList;
var
  i: Integer = -1;
  j: Integer = -1;
  b: TMegaStringBuilder = nil;
  aList: TStringList = nil;
begin
  try
    b := TMegaStringBuilder.Create;
    b.Add(TTimeTreeMap.DebugStringHeader);
    b.Add(LineEnding);
    for i := Low(FAllTimeSearchResults) to High(FAllTimeSearchResults) do
      if Assigned(FAllTimeSearchResults[i]) then
      begin
        try
          aList := FAllTimeSearchResults[i].DebugStrings;
          for j := 0 to aList.Count - 1 do
            b.Add(aList[j]);
          b.Add(LineEnding)
        finally
          if Assigned(aList) then
            aList.Free;
        end;;
      end;
    Result := TStringList.Create;
    Result.Add(b.GenerateString);
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

procedure TTimetreeSearchFrame.CalibrateNodeBtnClick(Sender: TObject);
begin
  ApplyCalibrationActionExecute(Sender);
end;

procedure TTimetreeSearchFrame.CalibrateNodeBtnMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchFrame.CalibrateNodeBtnMouseLeave(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchFrame.CalibrateNodeBtnPaint(Sender: TObject);
var
  w: Integer = -1;
  h: Integer = -1;
begin
  with CalibrateNodeBtn.Canvas do
  begin
    if CalibrateNodeBtn.MouseInClient then
      Brush.Color := FHoverColor
    else
      Brush.Color := FIdleColor;
    FillRect(ClientRect);
    Font.Color := clWhite;
    Font.Style := [fsBold];

    w := TextWidth(FCalibrateNodeButtonCaption);
    h := TextHeight(FCalibrateNodeButtonCaption);
    TextOut((CalibrateNodeBtn.Width - w) div 2, (CalibrateNodeBtn.Height - h) div 2, FCalibrateNodeButtonCaption);
  end;
end;

procedure TTimetreeSearchFrame.CloseBtnClick(Sender: TObject);
begin
  if Assigned(CloseNotify) then
    CloseNotify(Self);
end;

procedure TTimetreeSearchFrame.CloseBtnMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchFrame.CloseBtnMouseLeave(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchFrame.CloseBtnPaint(Sender: TObject);
var
  w: Integer = -1;
  h: Integer = -1;
begin
  with CloseBtn.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := $00dddddd;
    CloseBtn.Canvas.FillRect(ClientRect);
    if CloseBtn.MouseInClient then
      Font.Color := FHoverColor
    else
      Font.Color := clGrayText;
    Font.Style := [fsBold];
    w := TextWidth('Close');
    h := TextHeight('Close');
    TextOut((CloseBtn.Width - w) div 2, (CloseBtn.Height - h) div 2, 'Close');
  end;
end;

procedure TTimetreeSearchFrame.ApplyCalibrationActionExecute(Sender: TObject);
begin
  if not Assigned(UpdateTimetreeMapProc) then
    raise Exception.Create('Application error: missing UpdateTimetreeMapProc');
  if (not Assigned(FCurrentTimeSearchResult)) or (FCurrentTimeSearchResult.MedianTime < 0) then
  begin
    ShowMessage('No time data to apply. Hint: right-click a node and select "Fetch Time from Timetree"');
    Exit;
  end;
  FCurrentTimeSearchResult.CalibrationType := SelectedCalibrationCategory;
  UpdateTimetreeMapProc(FCurrentTimeSearchResult);
  if Assigned(NodeCalibratedNotify) then
    NodeCalibratedNotify(Self);
end;

end.

