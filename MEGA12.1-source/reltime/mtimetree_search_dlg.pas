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

unit mtimetree_search_dlg;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  LCLIntf, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, ActnList, ComCtrls, Menus, Buttons, mtimetree_map,
  MegaConsts, MegaVerConsts, Types;

type

  { TTimetreeSearchDlg }

  TTimetreeSearchDlg = class(TForm)
    KeepOnTopCheckbox: TCheckBox;
    NameSearchAction: TAction;
    ApplyNameAction: TAction;
    ActionList2: TActionList;
    MenuItem1: TMenuItem;
    CancelAction: TAction;
    Label1: TLabel;
    Label2: TLabel;
    NameSearchFeedbackLabel: TLabel;
    PopupMenu1: TPopupMenu;
    ProgressBar: TProgressBar;
    SaveAction: TAction;
    ActionList1: TActionList;
    NameSearchDrawGrid: TDrawGrid;
    NameSearchEdit: TEdit;
    Panel1: TPanel;
    CloseButton: TSpeedButton;
    ApplyResultButton: TSpeedButton;
    SearchButton: TSpeedButton;
    TaxaNamesComboBox: TComboBox;
    ProgressTimer: TTimer;
    procedure ApplyNameActionExecute(Sender: TObject);
    procedure ApplyResultButtonClick(Sender: TObject);
    procedure ApplyResultButtonMouseEnter(Sender: TObject);
    procedure ApplyResultButtonMouseLeave(Sender: TObject);
    procedure ApplyResultButtonPaint(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure FormContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KeepOnTopCheckboxChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure NameSearchActionExecute(Sender: TObject);
    procedure NameSearchBtnClick(Sender: TObject);
    procedure NameSearchDrawGridAfterSelection(Sender: TObject; aCol,
      aRow: Integer);
    procedure NameSearchDrawGridContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure NameSearchDrawGridDblClick(Sender: TObject);
    procedure NameSearchDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CloseButtonMouseEnter(Sender: TObject);
    procedure CloseButtonMouseLeave(Sender: TObject);
    procedure CloseButtonPaint(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchButtonMouseEnter(Sender: TObject);
    procedure SearchButtonMouseLeave(Sender: TObject);
    procedure SearchButtonPaint(Sender: TObject);
    procedure TaxaNamesComboBoxChange(Sender: TObject);
  private
    FApplyButtonCaption: String;
    FIdleColor: TColor;
    FHoverColor: TColor;
    FGridHighlightColor: TColor;
    FCurrentQuery: String;
    FNameQueryStrings: TStringList;
    FActiveFontColor: TColor;
    FIgnoreEvents: Boolean;
    FCurrentTaxonSearchResult: TTimeTreeTaxonDataList;
    FAllTaxaSearchResults: array of TTimeTreeTaxonDataList;
    procedure DisableApplyResultsButton;
    procedure UnselectCurrentNameSearchResults;
    procedure UpdateProgressBar(aProgress: Integer);
    procedure ClearCurrentSearchResult;
    procedure NameSearchThreadDone(aThread: TObject);
    procedure NcbiIdSearchThreadDone(aThread: TObject);
    procedure LaunchNameSearchThread(query: String);
    procedure LaunchNcbiSearchThread(ncbiId: Integer);
    function NameSearchResultFormatStr: String;
    procedure UpdateNameSearchLabel(queryStr: String; numResults: Integer);
    procedure SetApplyNameActionCaption(ttName: String; otuName: String);
  public
    UpdateTimetreeDataProc: TUpdateTimetreeDataProc;
    procedure Initialize(numNodes: Integer);
    procedure SetTaxaNames(names: TStringList; index: Integer);
    procedure SetFocusedTaxon(otuIndex: Integer);
    procedure NcbiIdSearch(ncbiId: Integer; taxonIndex: Integer);
    procedure ShowAtPosition(aTop: Integer; aLeft: Integer);
    function DebugNameSearchResults: TStringList;
  end;

var
  TimetreeSearchDlg: TTimetreeSearchDlg;

implementation

{$R *.lfm}

uses
  mtimetree_api, mstringbuilder;

{ TTimetreeSearchDlg }

procedure TTimetreeSearchDlg.KeepOnTopCheckboxChange(Sender: TObject);
begin
  if KeepOnTopCheckbox.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TTimetreeSearchDlg.MenuItem1Click(Sender: TObject);
begin
  ApplyNameActionExecute(Sender);
end;

procedure TTimetreeSearchDlg.NameSearchActionExecute(Sender: TObject);
var
  tempInt: Integer = -1;
begin
  try
    ClearCurrentSearchResult;
    NameSearchFeedbackLabel.Caption := EmptyStr;

    if TaxaNamesComboBox.ItemIndex < 0 then
    begin
      NameSearchFeedbackLabel.Caption := 'Please select a target sequence from the drop down list.';
      Exit;
    end;

    if Trim(NameSearchEdit.Text) = EmptyStr then
    begin
      NameSearchFeedbackLabel.Caption := 'Please specify a search term.';
      Exit;
    end;

    UpdateProgressBar(0);
    ProgressTimer.Enabled := True;
    FCurrentQuery := NameSearchEdit.Text;
    if Trim(FCurrentQuery) <> EmptyStr then
      FNameQueryStrings[TaxaNamesComboBox.ItemIndex] := FCurrentQuery;
    if TryStrToInt(FCurrentQuery, tempint) then
      LaunchNcbiSearchThread(tempInt)
    else
      LaunchNameSearchThread(FCurrentQuery);
    Invalidate;
  except
    on E:Exception do
    begin
      NameSearchFeedbackLabel.Caption := 'An error has been encountered: ' + E.Message;
      ProgressTimer.Enabled := False;
      UpdateProgressBar(0);
    end;
  end;
end;

procedure TTimetreeSearchDlg.NameSearchBtnClick(Sender: TObject);
begin
  NameSearchActionExecute(Sender);
end;

procedure TTimetreeSearchDlg.NameSearchDrawGridAfterSelection(Sender: TObject; aCol, aRow: Integer);
var
  ttData: TTimeTreeTaxonData = nil;
  aName: String = '';
begin
  if NameSearchDrawGrid.Row >= 1 then
  begin
    Assert(Assigned(FCurrentTaxonSearchResult[NameSearchDrawGrid.Row - 1]));
    ttData := FCurrentTaxonSearchResult[NameSearchDrawGrid.Row - 1];

    if ttData.UserQuery <> EmptyStr then
      NameSearchEdit.Text := ttData.UserQuery
    else if ttData.Synonym <> EmptyStr then
      NameSearchEdit.Text := ttData.Synonym
    else
      NameSearchEdit.Text := ttData.OtuName;
    aName := ttData.TimetreeName;
    SetApplyNameActionCaption(aName, FCurrentQuery);
  end;
end;

procedure TTimetreeSearchDlg.NameSearchDrawGridContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  screenPos: TPoint;
  gridCoords: TPoint;
begin
  screenPos := NameSearchDrawGrid.ClientToScreen(MousePos);
  gridCoords := NameSearchDrawGrid.MouseToCell(MousePos);
  if gridCoords.Y > 0 then
  begin
    NameSearchDrawGrid.Row := gridCoords.Y;
    PopupMenu1.PopUp(screenPos.X, screenPos.Y);
  end;
  Handled := True
end;

procedure TTimetreeSearchDlg.NameSearchDrawGridDblClick(Sender: TObject);
begin
  if NameSearchDrawGrid.Row >= 1 then
    ApplyNameActionExecute(Sender);
end;

procedure TTimetreeSearchDlg.NameSearchDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  str: String = '';
  x: Integer = -1;
  y: Integer = -1;
  ttData: TTimeTreeTaxonData = nil;
  ts: TTextStyle;
begin
  if Assigned(FCurrentTaxonSearchResult) and (aRow > FCurrentTaxonSearchResult.Count) then
    Exit;
  with NameSearchDrawGrid.Canvas do
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
      if Assigned(FCurrentTaxonSearchResult) then
      begin
        ttData := FCurrentTaxonSearchResult[aRow - 1];
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

procedure TTimetreeSearchDlg.ProgressTimerTimer(Sender: TObject);
begin
  UpdateProgressBar((ProgressBar.Position + ProgressBar.Step) mod ProgressBar.Max);
end;

procedure TTimetreeSearchDlg.SaveActionExecute(Sender: TObject);
begin
  if NameSearchDrawGrid.Row > 0 then
    ApplyNameActionExecute(Sender)
  else
    NameSearchFeedbackLabel.Caption := 'Please select a search result to apply';
end;

procedure TTimetreeSearchDlg.CloseButtonClick(Sender: TObject);
begin
  Hide;
end;

procedure TTimetreeSearchDlg.CloseButtonMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchDlg.CloseButtonMouseLeave(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchDlg.CloseButtonPaint(Sender: TObject);
var
  w: Integer = -1;
  h: Integer = -1;
begin
  with CloseButton.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := $00dddddd;
    CloseButton.Canvas.FillRect(ClientRect);

    if CloseButton.MouseInClient then
      Font.Color := FHoverColor
    else
      Font.Color := clGrayText;
    Font.Style := [fsBold];
    w := TextWidth('Close');
    h := TextHeight('Close');
    TextOut((CloseButton.Width - w) div 2, (CloseButton.Height - h) div 2, 'Close');
  end;
end;

procedure TTimetreeSearchDlg.SearchButtonClick(Sender: TObject);
begin
  NameSearchActionExecute(Sender);
end;

procedure TTimetreeSearchDlg.SearchButtonMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchDlg.SearchButtonMouseLeave(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchDlg.SearchButtonPaint(Sender: TObject);
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
    FillRect(ClientRect);
    Font.Color := clWhite;
    Font.Style := [fsBold];
    w := TextWidth('Search');
    h := TextHeight('Search');
    TextOut((SearchButton.Width - w) div 2, (SearchButton.Height - h) div 2, 'Search');
  end;
end;

procedure TTimetreeSearchDlg.TaxaNamesComboBoxChange(Sender: TObject);
var
  i: Integer = -1;
  aRow: Integer = 1;
  ttData: TTimeTreeTaxonData = nil;
begin
  if FIgnoreEvents then Exit;
  ClearCurrentSearchResult;
  if TaxaNamesComboBox.ItemIndex >= 0 then
  begin
    NameSearchEdit.Text := FNameQueryStrings[TaxaNamesComboBox.ItemIndex];
    FCurrentTaxonSearchResult := FAllTaxaSearchResults[TaxaNamesComboBox.ItemIndex];
    if Assigned(FCurrentTaxonSearchResult) then
    begin
      NameSearchDrawGrid.RowCount := (FCurrentTaxonSearchResult.Count + 1);
      for i := 0 to FCurrentTaxonSearchResult.Count - 1 do
      begin
        if FCurrentTaxonSearchResult[i].IsSelected then
        begin
          ttData := FCurrentTaxonSearchResult[i];
          if ttData.UserQuery <> EmptyStr then
            NameSearchEdit.Text := ttData.UserQuery
          else if ttData.Synonym <> EmptyStr then
            NameSearchEdit.Text := ttData.Synonym
          else
            NameSearchEdit.Text := ttData.OtuName;
          aRow := i + 1;
          break;
        end;
      end;
      NameSearchDrawGrid.Row := aRow;
      SetApplyNameActionCaption(FCurrentTaxonSearchResult[aRow - 1].TimetreeName, FCurrentTaxonSearchResult[aRow - 1].OtuName);
    end;
  end
  else
  begin
    NameSearchDrawGrid.RowCount := 1;
    DisableApplyResultsButton;
  end;
  NameSearchDrawGrid.Invalidate;
end;

procedure TTimetreeSearchDlg.DisableApplyResultsButton;
begin
  ApplyResultButton.Caption := 'No Results to Apply';
  ApplyResultButton.Enabled := False;
  ApplyResultButton.Invalidate;
end;

procedure TTimetreeSearchDlg.UnselectCurrentNameSearchResults;
var
  i: Integer = -1;
begin
  if Assigned(FCurrentTaxonSearchResult) and (FCurrentTaxonSearchResult.Count > 0) then
    for i := 0 to FCurrentTaxonSearchResult.Count - 1 do
      FCurrentTaxonSearchResult[i].IsSelected := False;
end;

procedure TTimetreeSearchDlg.UpdateProgressBar(aProgress: Integer);
begin
  if Assigned(ProgressBar) then
    ProgressBar.Position := aProgress;
end;

procedure TTimetreeSearchDlg.ClearCurrentSearchResult;
begin
  try
    FIgnoreEvents := True;
    NameSearchDrawGrid.RowCount := 1;
    DisableApplyResultsButton;
    NameSearchFeedbackLabel.Caption := EmptyStr;
    FApplyButtonCaption := SEARCH_BTN_DEFAULT_CAPTION;
    FCurrentTaxonSearchResult := nil;
  finally
    FIgnoreEvents := False;
  end;
  Invalidate;
end;

procedure TTimetreeSearchDlg.NameSearchThreadDone(aThread: TObject);
var
  t: TTimetreeApiThread = nil;
  i: Integer = -1;
begin
  ProgressTimer.Enabled := False;
  UpdateProgressBar(0);
  t := TTimetreeApiThread(aThread);
  if t.IsSuccess then
  begin
    FCurrentTaxonSearchResult := t.TimeTreeTaxonDataList;
    FAllTaxaSearchResults[TaxaNamesComboBox.ItemIndex] := t.TimeTreeTaxonDataList;
    UpdateNameSearchLabel(t.Query, FCurrentTaxonSearchResult.Count);
    NameSearchDrawGrid.RowCount := FCurrentTaxonSearchResult.Count + 1;
    for i := 0 to FCurrentTaxonSearchResult.Count - 1 do
    begin
      FCurrentTaxonSearchResult[i].OtuIndex := TaxaNamesComboBox.ItemIndex;
      FCurrentTaxonSearchResult[i].OtuName := TaxaNamesComboBox.Items[TaxaNamesComboBox.ItemIndex]
    end;
    if FCurrentTaxonSearchResult.Count = 1 then
      NameSearchDrawGrid.Row := 1;
    SetApplyNameActionCaption(FCurrentTaxonSearchResult[0].TimetreeName, TaxaNamesComboBox.Items[TaxaNamesComboBox.ItemIndex]);
  end
  else
  begin
    if t.ErrorMsg <> EmptyStr then
      NameSearchFeedbackLabel.Caption := Format('The search for "%s" failed with the following message: %s', [t.Query, t.ErrorMsg])
    else
      UpdateNameSearchLabel(t.Query, 0);
  end;
  Invalidate;
end;

procedure TTimetreeSearchDlg.NcbiIdSearchThreadDone(aThread: TObject);
var
  t: TTimetreeApiThread = nil;
begin
  ProgressTimer.Enabled := False;
  UpdateProgressBar(0);
  t := TTimetreeApiThread(aThread);
  if t.IsSuccess then
  begin
    FCurrentTaxonSearchResult := t.TimeTreeTaxonDataList;
    FAllTaxaSearchResults[TaxaNamesComboBox.ItemIndex] := t.TimeTreeTaxonDataList;
    if FCurrentTaxonSearchResult.Count > 1 then
    begin
      NameSearchFeedbackLabel.Caption := 'The search for "%s" returned an invalid response';
      exit;
    end
    else
      UpdateNameSearchLabel(t.Query, 1);
    NameSearchDrawGrid.RowCount := 2;
    FCurrentTaxonSearchResult[0].OtuIndex := TaxaNamesComboBox.ItemIndex;
    FCurrentTaxonSearchResult[0].OtuName := TaxaNamesComboBox.Items[TaxaNamesComboBox.ItemIndex];
    NameSearchEdit.Text := FCurrentTaxonSearchResult[0].Synonym;
    NameSearchDrawGrid.Row := 1;
    SetApplyNameActionCaption(FCurrentTaxonSearchResult[0].TimetreeName, FCurrentTaxonSearchResult[0].OtuName);
  end
  else
  begin
    if t.ErrorMsg <> EmptyStr then
      NameSearchFeedbackLabel.Caption := Format('The search for "%s" failed with the following message: %s', [t.Query, t.ErrorMsg])
    else
      UpdateNameSearchLabel(t.Query, 0);
  end;
  if not Visible then
    Show;
  Invalidate;
end;

procedure TTimetreeSearchDlg.LaunchNameSearchThread(query: String);
var
  t: TTimetreeApiThread = nil;
begin
  t := TTimetreeApiThread.Create;
  t.InitializeForNameSearch(query);
  t.OnTerminate := @NameSearchThreadDone;
  t.Start;
end;

procedure TTimetreeSearchDlg.LaunchNcbiSearchThread(ncbiId: Integer);
var
  t: TTimetreeApiThread = nil;
begin
  t := TTimetreeApiThread.Create;
  t.InitializeForNcbiIdSearch(IntToStr(ncbiId));
  t.OnTerminate := @NcbiIdSearchThreadDone;
  t.Start;
end;

function TTimetreeSearchDlg.NameSearchResultFormatStr: String;
var
  ttData: TTimeTreeTaxonData = nil;
  i: Integer = -1;
  longestNameLen: Integer = 20;
  longestRankLen: Integer = 7;
begin
  Result := EmptyStr;
  if Assigned(FCurrentTaxonSearchResult) then
  begin
    if FCurrentTaxonSearchResult.Count > 0 then
      for i := 0 to FCurrentTaxonSearchResult.Count - 1 do
      begin
        ttData := FCurrentTaxonSearchResult[i];
        if Length(ttData.TimetreeName) > longestNameLen then
          longestNameLen := Length(ttData.TimetreeName);
        if Length(ttData.TaxonomicRank) > longestRankLen then
          longestRankLen := Length(ttData.TaxonomicRank);
      end;
  end;

  Result := Format('%%-%ds  rank = %%-%ds NCBI ID = %%d', [longestNameLen, longestRankLen]);
end;

procedure TTimetreeSearchDlg.UpdateNameSearchLabel(queryStr: String; numResults: Integer);
var
  temp: String = '';
begin
  if numResults = 0 then
    temp := Format('The search for "%s" returned no results.', [queryStr])
  else if numResults = 1 then
    temp := Format('The search for "%s" returned 1 result.', [queryStr])
  else
    temp := Format('The search for "%s" returned %d results. Please select the preferred result to apply.', [queryStr, numResults]);
  NameSearchFeedbackLabel.Caption := temp;
end;

procedure TTimetreeSearchDlg.SetApplyNameActionCaption(ttName: String; otuName: String);
begin
  FApplyButtonCaption := Format('Apply "%s" to "%s"', [ttName, otuName]);
  ApplyResultButton.Enabled := True;
  ApplyResultButton.Invalidate;
end;

procedure TTimetreeSearchDlg.Initialize(numNodes: Integer);
begin
  FIgnoreEvents := False;
  if not Assigned(FNameQueryStrings) then
    FNameQueryStrings := TStringList.Create;
  FCurrentTaxonSearchResult := nil;
  NameSearchFeedbackLabel.Caption := 'Select a "Target Sequence" and "Search" by name or ID';
  UpdateProgressBar(0);
  NameSearchDrawGrid.RowCount := 1;
  FActiveFontColor := RGB(31, 156, 157);
  FIdleColor := $00b1a247; // #47a2b1;
  FHoverColor := $008c8225; // #25828c
  FGridHighlightColor := $00f6f4e4; // #e4f4f6;
  NameSearchFeedbackLabel.Font.Color := $00003ac2;
  DisableApplyResultsButton;
end;

procedure TTimetreeSearchDlg.SetTaxaNames(names: TStringList; index: Integer);
var
  i: Integer = -1;
begin
  try
    FIgnoreEvents := True;
    TaxaNamesComboBox.Clear;
    TaxaNamesComboBox.Items.AddStrings(names);
    TaxaNamesComboBox.ItemIndex := index;
    FNameQueryStrings.AddStrings(names);
    NameSearchEdit.Text := FNameQueryStrings[index];
    SetLength(FAllTaxaSearchResults, names.Count);
    for i := 0 to names.Count - 1 do
      FAllTaxaSearchResults[i] := nil;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TTimetreeSearchDlg.SetFocusedTaxon(otuIndex: Integer);
var
  ttData: TTimeTreeTaxonData = nil;
  i: Integer = -1;
  aRow: Integer = 1;
begin
  try
    FIgnoreEvents := True;
    TaxaNamesComboBox.ItemIndex := otuIndex;
    FCurrentTaxonSearchResult := FAllTaxaSearchResults[otuIndex];
    if Assigned(FCurrentTaxonSearchResult) then
    begin
      ttData := FCurrentTaxonSearchResult[0];
      if FCurrentTaxonSearchResult.Count > 1 then
        for i := 1 to FCurrentTaxonSearchResult.Count - 1 do
          if FCurrentTaxonSearchResult[i].IsSelected then
          begin
            aRow := i + 1;
            ttData := FCurrentTaxonSearchResult[i];
            break;
          end;
      if ttData.UserQuery <> EmptyStr then
        NameSearchEdit.Text := ttData.UserQuery
      else if ttData.Synonym <> EmptyStr then
        NameSearchEdit.Text := ttData.Synonym
      else
        NameSearchEdit.Text := ttData.OtuName;

      NameSearchDrawGrid.RowCount := FCurrentTaxonSearchResult.Count + 1;
      SetApplyNameActionCaption(ttData.TimetreeName, ttData.OtuName);
      UpdateNameSearchLabel(ttData.SearchString, FCurrentTaxonSearchResult.Count);
    end
    else
    begin
      NameSearchEdit.Text := TaxaNamesComboBox.Items[TaxaNamesComboBox.ItemIndex];
      NameSearchDrawGrid.RowCount := 1;
      DisableApplyResultsButton;
      NameSearchFeedbackLabel.Caption := EmptyStr;
    end;
    NameSearchDrawGrid.Row := aRow;
    NameSearchDrawGrid.Invalidate;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TTimetreeSearchDlg.NcbiIdSearch(ncbiId: Integer; taxonIndex: Integer);
begin
  try
    ClearCurrentSearchResult;
    NameSearchEdit.Text := IntToStr(ncbiId);
    NameSearchFeedbackLabel.Caption := EmptyStr;
    NameSearchDrawGrid.RowCount := 1;
    DisableApplyResultsButton;
    try
      FIgnoreEvents := True;
      TaxaNamesComboBox.ItemIndex := taxonIndex;
    finally
      FIgnoreEvents := False;
    end;
    UpdateProgressBar(0);
    ProgressTimer.Enabled := True;
    FCurrentQuery := TaxaNamesComboBox.Items[taxonIndex];
    LaunchNcbiSearchThread(ncbiId);
    Invalidate;
  except
    on E:Exception do
    begin
      NameSearchFeedbackLabel.Caption := 'An error has been encountered: ' + E.Message;
      ProgressTimer.Enabled := False;
      UpdateProgressBar(0);
    end;
  end;
end;

procedure TTimetreeSearchDlg.ShowAtPosition(aTop: Integer; aLeft: Integer);
begin
  Top := aTop;
  Left := aLeft;
  Show;
end;

function TTimetreeSearchDlg.DebugNameSearchResults: TStringList;
var
  b: TMegaStringBuilder = nil;
  i: Integer = -1;
  j: Integer = -1;
begin
  try
    b := TMegaStringBuilder.Create;
    b.Add(TTimeTreeTaxonData.DebugStringHeader);
    b.Add(LineEnding);
    for i := Low(FAllTaxaSearchResults) to High(FAllTaxaSearchResults) do
      if Assigned(FAllTaxaSearchResults[i]) then
      begin
        for j := 0 to FAllTaxaSearchResults[i].Count - 1 do
        begin
          b.Add(LineEnding);
          b.Add(FAllTaxaSearchResults[i][j].DebugString);
        end;
      end;
    Result := TStringList.Create;
    Result.add(b.GenerateString);
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

procedure TTimetreeSearchDlg.CancelActionExecute(Sender: TObject);
begin
  Hide;
end;

procedure TTimetreeSearchDlg.ApplyNameActionExecute(Sender: TObject);
var
  ttData: TTimeTreeTaxonData = nil;
begin
  if not Assigned(UpdateTimetreeDataProc) then
    raise Exception.Create('Application error: missing UpdateTimetreeDataProc');
  if NameSearchDrawGrid.Row < 1 then
  begin
    if NameSearchDrawGrid.RowCount = 1 then
      ShowMessage('Please execute a new search to find a taxon name to apply')
    else
      ShowMessage('Please select a taxon name search result to apply.');
    Exit;
  end;
  UpdateNameSearchLabel(FCurrentQuery, FCurrentTaxonSearchResult.Count);
  UnselectCurrentNameSearchResults;
  ttData := FCurrentTaxonSearchResult[NameSearchDrawGrid.Row - 1];
  ttData.IsSelected := True;
  UpdateTimetreeDataProc(ttData);
  if FormStyle <> fsStayOnTop then
    Hide;
end;

procedure TTimetreeSearchDlg.ApplyResultButtonClick(Sender: TObject);
begin
  ApplyNameActionExecute(Sender);
end;

procedure TTimetreeSearchDlg.ApplyResultButtonMouseEnter(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchDlg.ApplyResultButtonMouseLeave(Sender: TObject);
begin
  (Sender as TSpeedButton).Invalidate;
end;

procedure TTimetreeSearchDlg.ApplyResultButtonPaint(Sender: TObject);
var
  w: Integer = -1;
  h: Integer = -1;
begin
  with ApplyResultButton.Canvas do
  begin
    if ApplyResultButton.MouseInClient then
      Brush.Color := FHoverColor
    else
      Brush.Color := FIdleColor;
    FillRect(ClientRect);
    if ApplyResultButton.Enabled then
      Font.Color := clWhite
    else
      Font.Color := clGrayText;
    Font.Style := [fsBold];

    w := TextWidth(FApplyButtonCaption);
    h := TextHeight(FApplyButtonCaption);
    TextOut((ApplyResultButton.Width - w) div 2, (ApplyResultButton.Height - h) div 2, FApplyButtonCaption);
  end;
end;

procedure TTimetreeSearchDlg.FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  screenPos: TPoint;
  gridCoords: TPoint;
begin
  screenPos := NameSearchDrawGrid.ClientToScreen(MousePos);
  gridCoords := NameSearchDrawGrid.MouseToCell(MousePos);
  if gridCoords.Y > 0 then
  begin
    NameSearchDrawGrid.Row := gridCoords.Y;
    PopupMenu1.PopUp(screenPos.X, screenPos.Y);
  end;
  Handled := True
end;

procedure TTimetreeSearchDlg.FormCreate(Sender: TObject);
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': ' + 'NCBI ID Search';
  FApplyButtonCaption := SEARCH_BTN_DEFAULT_CAPTION;
end;

procedure TTimetreeSearchDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(FNameQueryStrings) then
    FNameQueryStrings.Free;
end;

end.

