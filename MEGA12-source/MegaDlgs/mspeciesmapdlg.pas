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

unit MSpeciesMapDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, ImgList, ComCtrls,
  ExtCtrls, Menus, StdCtrls, Grids, Buttons, ClipBrd, IniPropStorage,
  MegaConsts, Math, MCleanSpeciesNamesDlg, MStringCleaner, mimageform;

type

  TSpeciesMapDlgCallback = procedure(IsCancelled: Boolean) of object;

  TSearchResult = class(TObject)
    private

    protected

    public
      Column: Integer;
      Row: Integer;
      constructor Create(ACol: Integer; ARow: Integer);
      destructor Destroy; override;
  end;

  { TSpeciesMapDlg }

  TSpeciesMapDlg = class(TForm)
    SaveAction2: TAction;
    CancelAction: TAction;
    ActionList2: TActionList;
    IniPropStorage1: TIniPropStorage;
    Panel2: TPanel;
    SearchUpBtn: TBitBtn;
    SearchDownBtn: TBitBtn;
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ActionList1: TActionList;
    ImportDataAction: TAction;
    SaveAction: TAction;
    ExportDataAction: TAction;
    DisplayHelpAction: TAction;
    SearchUpAction: TAction;
    SearchDownAction: TAction;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    F1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    Import1: TMenuItem;
    Export1: TMenuItem;
    Save1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    DrawGrid: TDrawGrid;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    SearchEdit: TEdit;
    CopyAction: TAction;
    PasteAction: TAction;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    SelectAllAction: TAction;
    ToolButton7: TToolButton;
    HelpContentsAction: TAction;
    Contents1: TMenuItem;
    StripNumericTokensAction: TAction;
    CopySelectedToRightAction: TAction;
    CopyAllToRightAction: TAction;
    CopyTaxaNamestoSpecies1: TMenuItem;
    AllTaxa1: TMenuItem;
    SelectedTaxa1: TMenuItem;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    CleanSpeciesNamesAction: TAction;
    ToolButton10: TToolButton;
    CleanSpeciesNames1: TMenuItem;
    procedure FormActivate(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure ImportDataActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure DisplayHelpActionExecute(Sender: TObject);
    procedure SearchUpActionExecute(Sender: TObject);
    procedure SearchDownActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DrawGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure ExportDataActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGridGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure DrawGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure SearchEditEnter(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure SearchEditClick(Sender: TObject);
    procedure HelpContentsActionExecute(Sender: TObject);
    procedure CopySelectedToRightActionExecute(Sender: TObject);
    procedure CopyAllToRightActionExecute(Sender: TObject);
    procedure CleanSpeciesNamesActionExecute(Sender: TObject);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

  private
    FCleaner: TStringCleaner;
    FCleanSpNamesDlg: TCleanSpNamesDlg;
    FSearchResults: TList;
    FSearchTerm: String;
    FSearchResultIndex: Integer;
    FMapList: TStringList;
    TaxaNames: TStringList;
    SpeciesNames: TStringList;
    FTempSpeciesNames: TStringList;
    procedure InitMainMenu;
    procedure UpdateSelectedIndices;
    function CellText(ACol: Integer; ARow: Integer): String;
    procedure BuildSearchResultsList;
    procedure ClearSearchResults;
    procedure GoToNextSearchResult;
    procedure GoToPreviousSearchResult;
    procedure DrawEdges(ARect: TRect; DoLeft: Boolean; DoTop: Boolean; DoRight: Boolean; DoBottom: Boolean; AColor: TColor);
    function ValidateForm: Boolean;
    function BuildMapList: TStringList;
    procedure SortByColumn(AColumn: Integer);
    procedure ApplySpeciesNamesFilters(Sender: TObject);
    procedure RevertSpeciesNamesFilters(Sender: TObject);
    procedure FinishedWithSpeciesNamesFilters(Sender: TObject);
  protected
    FSelectedIndices: ArrayOfInteger;
    FSpeciesNamesModified: Boolean;
    function HasSpeciesNames: Boolean;
    function AskSelectAll: Boolean;
  public
    WizardCallback: TSpeciesMapDlgCallback;
    { Public declarations }
    procedure LoadTaxaNames(ANamesList: TStringList);
    property MapList: TStringList read BuildMapList;
  end;

var
  SpeciesMapDlg: TSpeciesMapDlg;

implementation

{$R *.lfm}

uses
  mhelpfiles, mhelpkeywords, MFileUtils, MegaUtils, Mega_Main,
  StringUtils, mshortcutshelper, ContextHelp_HC;

procedure TSpeciesMapDlg.ApplySpeciesNamesFilters(Sender: TObject);
var
  EscapedString: String;
  NumTokensStripped: Integer;
begin
  UpdateSelectedIndices;
  if Length(FSelectedIndices) = 0 then
    Exit;
  if FCleanSpNamesDlg.IsMaxTokensFilter then
  begin
    NumTokensStripped := FCleaner.StripExcessTokens(SpeciesNames, FCleanSpNamesDlg.MaxTokens, FSelectedIndices);
    if NumTokensStripped > 0 then
      FSpeciesNamesModified := True;
  end;
  if FCleanSpNamesDlg.IsNumberFilter then
  begin
    NumTokensStripped := FCleaner.CleanStringList(SpeciesNames, '\d', FSelectedIndices);
    if NumTokensStripped > 0 then
      FSpeciesNamesModified := True;
  end;
  if FCleanSpNamesDlg.IsNonAlphaNumericFilter then
  begin
    NumTokensStripped := FCleaner.CleanStringList(SpeciesNames, '\W', FSelectedIndices);
    if NumTokensStripped > 0 then
      FSpeciesNamesModified := True;
  end;
  if FCleanSpNamesDlg.IsTextFilter then
  begin
    EscapedString := EscapeSpecialCharsInStringLiteral(FCleanSpNamesDlg.TextString);
    NumTokensStripped := FCleaner.CleanStringList(SpeciesNames, EscapedString, FSelectedIndices);
    if NumTokensStripped > 0 then
      FSpeciesNamesModified := True;
  end;
  if FCleanSpNamesDlg.IsRegexFilter then
  begin
    NumTokensStripped := FCleaner.CleanStringList(SpeciesNames, FCleanSpNamesDlg.RegexString, FSelectedIndices);
    if NumTokensStripped > 0 then
      FSpeciesNamesModified := True;
  end;
  DrawGrid.Invalidate;
end;

function TSpeciesMapDlg.AskSelectAll: Boolean;
var
  aMsgDlg: TForm;
  dlgButton: TButton;
  i, Response: Integer;
  CaptionIndex: Integer;
  aCaptions: array[0..1] of String;
//  aWidth: Integer;
begin
  Result := False;
  if DrawGrid.Selection.Bottom <= 0 then { no rows with taxa names are selected}
  begin
    Response := MessageDlg('Nothing is selected. Edit all species names?', mtConfirmation, [mbYes, mbCancel], 0);
    case Response of
      mrYes:
        begin
          SelectAllActionExecute(Self);
          Result := True;
        end;
      mrCancel:
        begin
          Result := False;
          Exit;
        end;
    end;
  end
  else if ((DrawGrid.Selection.Top > 0) or (DrawGrid.Selection.Bottom < (DrawGrid.RowCount - 1))) then
  begin
    CaptionIndex := 0;
    aCaptions[0] := 'Selected';
    aCaptions[1] := 'All';
    aMsgDlg := CreateMessageDialog('Edit species names only for selected rows?', mtConfirmation, mbYesNo);
    for i := 0 to aMsgDlg.ComponentCount - 1 do
    begin
     { If the object is of type TButton, then }
      if (aMsgDlg.Components[i] is TButton) then
      begin
        dlgButton := TButton(aMsgDlg.Components[i]);
        if CaptionIndex > High(aCaptions) then Break;
        { Give a new caption from our Captions array}
        dlgButton.Caption := aCaptions[CaptionIndex];
//        aWidth := self.Canvas.TextWidth(aCaptions[CaptionIndex]);
        dlgButton.Width := 90;
        Inc(CaptionIndex);
      end;
    end;
    Response := aMsgDlg.ShowModal;
    case Response of
      mrYes:
        begin
          { just leave the selection the way it is}
          Result := True;
        end;
      mrNo:
        begin
          SelectAllActionExecute(Self);
          Result := True;
        end;
      mrCancel:
        begin
          Result := False;
        end;
    end;
  end
  else
    Result := True;
end;

function TSpeciesMapDlg.BuildMapList: TStringList;
var
  i: Integer;
begin
  if not Assigned(FMapList) then
    FMapList := TStringList.Create;
  FMapList.Clear;
  for i := 0 to TaxaNames.Count - 1 do
    FMapList.Add(TaxaNames[i] + '=' + SpeciesNames[i]);
  Result := FMapList;
end;

procedure TSpeciesMapDlg.BuildSearchResultsList;
var
  i: Integer;
  AResult: TSearchResult;
begin
  FSearchResultIndex := -1;
  if TaxaNames.Count = 0 then
    Exit;
  FSearchTerm := LowerCase(Trim(SearchEdit.Text));
  if FSearchTerm = EmptyStr then
    Exit;
  ClearSearchResults;
  for i := 0 to TaxaNames.Count - 1 do
    if Pos(FSearchTerm, LowerCase(TaxaNames[i])) > 0 then
    begin
      AResult := TSearchResult.Create(0, i + 1);
      FSearchResults.Add(AResult);
    end;

  if SpeciesNames.Count > 0 then
    for i := 0 to SpeciesNames.Count - 1 do
      if Pos(FSearchTerm, LowerCase(SpeciesNames[i])) > 0 then
      begin
        AResult := TSearchResult.Create(1, i + 1);
        FSearchResults.Add(AResult);
      end;
  if FSearchResults.Count = 0 then
    ShowMessage('The search term ' + #39 + Trim(SearchEdit.Text) + #39 + ' was not found');
end;

procedure TSpeciesMapDlg.CancelBtnClick(Sender: TObject);
begin
//  Self.Hide;
  ModalResult := mrCancel;
//  WizardCallback(True);
end;

function TSpeciesMapDlg.CellText(ACol: Integer; ARow: Integer): String;
begin
  Result := EmptyStr;

  if ARow = 0 then { handle the header row}
  begin
    if ACol = 0 then
      Result := 'Taxon'
    else if ACol = 1 then

      Result := 'Species';
  end
  else
  begin
    if ARow > (TaxaNames.Count - 1) then
      Exit;
    if ACol = 0 then
      Result := TaxaNames[ARow - 1]
    else if ACol = 1 then
      Result := SpeciesNames[ARow - 1]
  end;
end;

procedure TSpeciesMapDlg.CleanSpeciesNamesActionExecute(Sender: TObject);
begin
  if not AskSelectAll then
    Exit;
  FTempSpeciesNames.Assign(SpeciesNames); { in case the user wants to cancel changes, we can revert}
  FCleanSpNamesDlg.Left := Left - 20;
  FCleanSpNamesDlg.Top := Top + 20;
  FCleanSpNamesDlg.Show;
end;

procedure TSpeciesMapDlg.ClearSearchResults;
var
  i: Integer;
begin
  if FSearchResults.Count > 0 then
    for i := 0 to FSearchResults.Count - 1 do
      if Assigned(FSearchResults[i]) then
        TSearchResult(FSearchResults[i]).Free;
  FSearchResults.Clear;
end;

procedure TSpeciesMapDlg.CopyActionExecute(Sender: TObject);
var
  i, j: Integer;
  ARect: TGridRect;
  AText: String;
begin
  AText := EmptyStr;
  ARect := DrawGrid.Selection;
  if (ARect.Left < 0) or (ARect.Top < 0) then
    Exit;

  for i  := ARect.Top to ARect.Bottom do
  begin
    for j := ARect.Left to ARect.Right do
    begin
      AText := AText + CellText(j, i) + #9;
    end;
    AText := Trim(AText) + LineEnding;
  end;
  AText := Trim(AText);
  try
    Clipboard.AsText := AText;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when copying data to the clipboard: ' + E.Message);
  end;
end;

procedure TSpeciesMapDlg.CopyAllToRightActionExecute(Sender: TObject);
var
  Response: Integer;
begin
  if TaxaNames.Count = 0 then
    Exit;
  if HasSpeciesNames then
  begin
    Response := MessageDlg('One or more species names will be overwritten. Is this ok?', mtConfirmation, mbYesNo, 0);
    if Response <> mrYes then
      Exit;
  end;
  SpeciesNames.Clear;
  SpeciesNames.AddStrings(TaxaNames);
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.DisplayHelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TSpeciesMapDlg.ExportDataActionExecute(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  Response: Integer;
  TempList: TStringList;
begin
  SaveDlg := TSaveDialog.Create(Self);
  SaveDlg.InitialDir := GetCurrentDir;
  SaveDlg.Title := 'Please specify a file to save to';

  if SaveDlg.Execute then
  begin
    if FileExists(SaveDlg.FileName) then
    begin
      Response := MessageDlg('That file already exists. Are you sure you want to over write it?', mtConfirmation, mbYesNo, 0);
      if Response = mrNo then
      begin
        SaveDlg.Free;
        Exit;
      end;
    end;

    try
      try
        TempList := BuildMapList;
        TempList.SaveToFile(SaveDlg.FileName);
      except
        on E: Exception do
          ShowMessage('Oh no! An error occurred: ' + E.Message);
      end;
    finally
      if Assigned(TempList) then
        TempList.Free;
      if Assigned(SaveDlg) then
        SaveDlg.Free;
    end;

  end
  else
  begin
    if Assigned(SaveDlg) then
      SaveDlg.Free;
    Exit;
  end;
end;

procedure TSpeciesMapDlg.FinishedWithSpeciesNamesFilters(Sender: TObject);
var
  Response: Integer;
begin
  if FSpeciesNamesModified then
  begin
    Response := MessageDlg('One or more species names have been modified. Save these changes?', mtConfirmation, mbYesNo, 0);
    if Response <> mrYes then
      RevertSpeciesNamesFilters(Self)
    else
      FTempSpeciesNames.Assign(SpeciesNames);
  end;
  FCleanSpNamesDlg.Hide;
end;

procedure TSpeciesMapDlg.SearchUpActionExecute(Sender: TObject);
begin
  if (Trim(SearchEdit.Text) = EmptyStr) then
    Exit;
  if FSearchTerm <> LowerCase(Trim(SearchEdit.Text)) then
    BuildSearchResultsList;
  if FSearchResults.Count > 0 then
    GoToPreviousSearchResult;
end;

procedure TSpeciesMapDlg.SelectAllActionExecute(Sender: TObject);
var
  ARect: TGridRect;
begin
  ARect.Left := 0;
  ARect.Right := DrawGrid.ColCount  - 1;
  ARect.Top := 0;
  if TaxaNames.Count = 0 then
    ARect.Bottom := 0
  else
    ARect.Bottom := DrawGrid.RowCount - 1;
  DrawGrid.Selection := ARect;
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.SearchDownActionExecute(Sender: TObject);
begin
  if (Trim(SearchEdit.Text) = EmptyStr) then
    Exit;
  if FSearchTerm <> LowerCase(Trim(SearchEdit.Text)) then
    BuildSearchResultsList;
  if FSearchResults.Count > 0 then
    GoToNextSearchResult;
end;

procedure TSpeciesMapDlg.SearchEditClick(Sender: TObject);
begin
  if SearchEdit.Text <> EmptyStr then
  begin
    SearchEdit.SelStart := 1;
    SearchEdit.SelLength := Length(SearchEdit.Text);
  end;
end;

procedure TSpeciesMapDlg.SearchEditEnter(Sender: TObject);
begin
  SearchEdit.Font.Color := clBlack;
  SearchEdit.Font.Style := [];
  SearchDownBtn.Default := True;
  SearchEdit.SelStart := 1;
  SearchEdit.SelLength := Length(SearchEdit.Text);
end;

procedure TSpeciesMapDlg.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  SetLength(FSelectedIndices, 0);
  FSearchResults := TList.Create;
  FSearchTerm := EmptyStr;
  FSearchResultIndex := -1;
  SpeciesNames := TStringList.Create;
  TaxaNames := TStringList.Create;
  FTempSpeciesNames := TStringList.Create;

  Left:=(Screen.Width-Width)  div 2;
  Top:=(Screen.Height-Height) div 2;
  DrawGrid.Row := 1;
  DrawGrid.Col := 1;
  FCleanSpNamesDlg := TCleanSpNamesDlg.Create(Self);
  FCleanSpNamesDlg.ApplyChangesNotify := ApplySpeciesNamesFilters;
  FCleanSpNamesDlg.RevertChangesNotify := RevertSpeciesNamesFilters;
  FCleanSpNamesDlg.DoneNotify := FinishedWithSpeciesNamesFilters;
  FCleaner := TStringCleaner.Create;
  InitMainMenu;
  ImageForm.UpdateImgList(Self);
  HelpContext := HC_GENEDUPS_WIZARD;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TSpeciesMapDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(TaxaNames) then
    TaxaNames.Free;
  if Assigned(FMapList) then
    FMapList.Free;
  if Assigned(FCleanSpNamesDlg) then
  begin
    FCleanSpNamesDlg.DoneNotify := nil;
    FreeAndNil(FCleanSpNamesDlg);
  end;
  if Assigned(FTempSpeciesNames) then
    FTempSpeciesNames.Free;
  if Assigned(FCleaner) then
    FCleaner.Free;
end;

procedure TSpeciesMapDlg.FormResize(Sender: TObject);
var
  Col: Integer;
  ColWidth: Integer;
  LineWidths: Integer;
  Delta: Integer;
begin
  if Panel2.Width > ToolBar2.Width then
    ToolBar2.Left := Round((Panel2.Width - ToolBar2.Width)/2);
  DrawGrid.Width := ClientWidth - 12;
  LineWidths := DrawGrid.GridLineWidth * (DrawGrid.ColCount - 1);
  ColWidth := (DrawGrid.ClientWidth div DrawGrid.ColCount) - DrawGrid.GridLineWidth;
  Delta := (DrawGrid.ClientWidth - (ColWidth * DrawGrid.ColCount)) - LineWidths;
  for Col := 0 to DrawGrid.ColCount - 1 do
  begin
    DrawGrid.ColWidths[Col] := colWidth;
    if Col = DrawGrid.ColCount - 1 then
      DrawGrid.ColWidths[Col] := DrawGrid.ColWidths[Col] + Delta;
  end;
end;

procedure TSpeciesMapDlg.FormShow(Sender: TObject);
begin
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.GoToNextSearchResult;
var
  AResult: TSearchResult;
begin
  if FSearchResults.Count = 1 then
    FSearchResultIndex := 0
  else if FSearchResultIndex = -1 then { handle the case for a new search}
    FSearchResultIndex := 0
  else if FSearchResultIndex = (FSearchResults.Count - 1) then { handle the case for having already focused on the last result}
    FSearchResultIndex := 0
  else
    inc(FSearchResultIndex);
  AResult := TSearchResult(FSearchResults[FSearchResultIndex]);
  DrawGrid.Col := AResult.Column;
  DrawGrid.Row := AResult.Row;
  DrawGrid.Invalidate; { force repaint of visible area}
end;

procedure TSpeciesMapDlg.GoToPreviousSearchResult;
var
  AResult: TSearchResult;
begin
  if FSearchResults.Count = 1 then
    FSearchResultIndex := 0
  else if FSearchResultIndex = -1 then { handle the case for a new search}
    FSearchResultIndex := (FSearchResults.Count - 1)
  else if FSearchResultIndex = 0 then { handle the case for having already focused on the last result}
    FSearchResultIndex := (FSearchResults.Count - 1)
  else
    dec(FSearchResultIndex);
  AResult := TSearchResult(FSearchResults[FSearchResultIndex]);
  DrawGrid.Col := AResult.Column;
  DrawGrid.Row := AResult.Row;
  DrawGrid.Invalidate; { force repaint of visible area}
end;

function TSpeciesMapDlg.HasSpeciesNames: Boolean;
var
  i: Integer;
begin
  Result := False;
  if SpeciesNames.Count = 0 then
    Exit;
  for i := 0 to SpeciesNames.Count - 1 do
    if Trim(SpeciesNames[i]) <> EmptyStr then
    begin
      Result := True;
      break;
    end;
end;

procedure TSpeciesMapDlg.HelpContentsActionExecute(Sender: TObject);
var
  helpTopic: String;
begin
  try
    helpTopic := MapHelpContextToKeyword(HelpContext);
    if Trim(helpTopic) <> EmptyStr then
      ShowContextSensitiveHelp(helpTopic)
    else
      ShowContextSensitiveHelp(HelpKeyword);
  except
    on E:Exception do
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TSpeciesMapDlg.ImportDataActionExecute(Sender: TObject);
var
  InputFile: AnsiString;
  OpenDlg: TOpenDialog;
  TempList: TStringList;
  i: Integer;
  Index: Integer;
  TempStr: String;
  ExtraNames: TStringList;
begin
  ExtraNames := nil;
  TempList := nil;
  OpenDlg := TOpenDialog.Create(Self);
  OpenDlg.InitialDir := GetCurrentDir;
  OpenDlg.Title := 'Please select a taxa to species mapping file';
  OpenDlg.Options := OpenDlg.Options + [ofFileMustExist];
  if OpenDlg.Execute then
  begin
    InputFile := OpenDlg.FileName;
    try
      try
        ExtraNames := TStringList.Create;
        TempList := TStringList.Create;
        TempList.CaseSensitive := False;
        TempList.LoadFromFile(InputFile);
        if TempList.Count > 0 then
        begin
          for i := 0 to TempList.Count - 1 do
          begin
//            TempStr := AnsiStrings.StringReplace(TempList.Names[i], '_', ' ', [rfReplaceAll]);
            TempStr := TempList.Names[i];
            TrimTaxaName2(TempStr);
            Index := TaxaNames.IndexOf(Trim(TempStr));
            if Index >= 0 then
              SpeciesNames[Index] := Trim(TempList.ValueFromIndex[i])
            else
              ExtraNames.Add(TempStr);
          end;
        end
        else
          raise Exception.Create('no key/value pairs found');
        SwitchDirectory(InputFile);
        if ExtraNames.Count > 0 then
        begin
          TempStr := EmptyStr;
          i := 0;
          while (i < ExtraNames.Count) and (i < 5) do
          begin
            TempStr := '   ' + TempStr + ExtraNames[i] + #13#10;
            inc(i);
          end;
          TempStr := 'Some imported names were not found in the active data: ' + #13#10 + TempStr;
          if ExtraNames.Count > 5 then
            TempStr := TempStr + 'Only the first 5 anomolies are shown';
          ShowMessage(TempStr);
        end;
      except
        on E: Exception do
          ShowMessage('Oh no! An error occurred: ' + E.Message);
      end;
    finally
      DrawGrid.Invalidate;
      if Assigned(TempList) then
        TempList.Free;
      if Assigned(OpenDlg) then
        OpenDlg.Free;
      if Assigned(ExtraNames) then
        ExtraNames.Free;
    end;
  end;
end;

procedure TSpeciesMapDlg.FormActivate(Sender: TObject);
begin
  ToolBar2.Images := ImageForm.GetDialogButtonImageList;
  ToolBar2.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar2.ImagesWidth := ToolBar2.ButtonWidth;
  if Panel2.Width > ToolBar2.Width then
    ToolBar2.Left := Round((Panel2.Width - ToolBar2.Width)/2);
  Constraints.MinWidth := ToolBar2.Width + 20;
end;

function TSpeciesMapDlg.FormHelp(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
begin
  Result := True;
  CallHelp := False;
  HelpContentsActionExecute(nil);
end;

procedure TSpeciesMapDlg.LoadTaxaNames(ANamesList: TStringList);
var
  i: Integer;
begin
  TaxaNames.Clear;
  SpeciesNames.Clear;
  TaxaNames.AddStrings(ANamesList);
  if TaxaNames.Count > 0 then
  begin
    DrawGrid.RowCount := TaxaNames.Count + 1;
    for i := 0 to TaxaNames.Count - 1 do
      SpeciesNames.Add(EmptyStr);
  end;
  with DrawGrid do
    ColWidths[1] := ClientWidth - ColWidths[0] - 2 * GridLineWidth;
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.CopySelectedToRightActionExecute(Sender: TObject);
var
  i: Integer;
  ARect: TGridRect;
  NumOverwrites: Integer;
  Response: Integer;
begin
  NumOverwrites := 0;
  ARect := DrawGrid.Selection;

  if (ARect.Left < 0) or (ARect.Top < 0) or ((ARect.Top = 0) and (ARect.Bottom = 0)) then
    Exit;
  for i := Max(1, ARect.Top) to ARect.Bottom do
  begin
    if ((i - 1) < SpeciesNames.Count) and (Trim(SpeciesNames[i - 1]) <> EmptyStr) then
      inc(NumOverwrites);
  end;
  if NumOverwrites > 0 then
  begin
    Response := MessageDlg(IntToStr(NumOverwrites) + ' species name(s) will be overwritten. Is this ok?', mtConfirmation, mbYesNo, 0);
    if Response <> mrYes then
      Exit;
  end;
  for i  := Max(1, ARect.Top) to ARect.Bottom do
  begin
    while (i-1) >= SpeciesNames.Count do
      SpeciesNames.Append(EmptyStr);
      SpeciesNames[i - 1] := TaxaNames[i - 1];
  end;
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.PasteActionExecute(Sender: TObject);
var
  ARect: TGridRect;
  AText: String;
  i, j: Integer;
  AList: TStringList;
begin
  AList := nil;
  ARect := DrawGrid.Selection;
  if not Clipboard.HasFormat(CF_TEXT) then
    Exit;
  if (ARect.Top < 0) or (ARect.Left < 0) then
    Exit;
  if ARect.Left = 0 then
  begin
    ShowMessage('Taxa names cannot be edited here');
    Exit;
  end;
  AText := Clipboard.AsText;
  if Trim(AText) = EmptyStr then
    Exit;
  if Pos(LineEnding, AText) > 0 then
  begin
    AList := TStringList.Create;
    try
      AList.Text := AText;
      i := ARect.Top; { loop over rows in the grid}
      j := 0; { loop over strings in the text to be pasted}
      while ((i - 1) < SpeciesNames.Count) and (j < AList.Count) do { if the user is pasting too much data, truncate it}
      begin
        SpeciesNames[i - 1] := AList[j];
        inc(j);
        inc(i);
      end;
    finally
      if Assigned(AList) then
        AList.Free;
    end;
  end
  else
  begin
    for i := ARect.Top to ARect.Bottom do
      SpeciesNames[i - 1] := AText;
  end;
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.RevertSpeciesNamesFilters(Sender: TObject);
begin
  SpeciesNames.Assign(FTempSpeciesNames);
  FSpeciesNamesModified := False;
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.SaveActionExecute(Sender: TObject);
begin
  if not ValidateForm then
    Exit;
  ModalResult := mrOk;
end;

procedure TSpeciesMapDlg.SortByColumn(AColumn: Integer);
begin
  { TODO 1 -oglen -cgenedup : implement sortbycolumn for mspeciesmapdlg }
end;


procedure TSpeciesMapDlg.DrawEdges(ARect: TRect; DoLeft: Boolean;
  DoTop: Boolean; DoRight: Boolean; DoBottom: Boolean; AColor: TColor);
begin
  with DrawGrid.Canvas do
  begin
    Pen.Color := AColor;
    if DoLeft then
    begin
      MoveTo(aRect.Left, aRect.Top);
      LineTo(aRect.Left, aRect.Bottom);
    end;
    if DoTop then
    begin
      MoveTo(aRect.Left - 1, aRect.Top);
      LineTo(aRect.Right + 1, aRect.Top);
    end;
    if DoRight then
    begin
      MoveTo(aRect.Right - 1, aRect.Top);
      LineTo(aRect.Right - 1, aRect.Bottom);
    end;
    if DoBottom then
    begin
      MoveTo(aRect.Left - 1, aRect.Bottom - 1);
      LineTo(aRect.Right + 1, aRect.Bottom - 1);
    end;
  end;
end;

procedure TSpeciesMapDlg.DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  AText: String;
  x, y: Integer;
  ARect: TGridRect;
begin
  if not Visible then
    Exit;
  with DrawGrid.Canvas do
  begin
    if ARow = 0 then { handle the header row}
    begin
      if ACol = 0 then
        AText := 'Taxon Name'
      else
        AText := 'Species Name';
      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;
      FillRect(Rect);
      DrawEdges(Rect, False, False, False, True, clGrayText);
      Font.Style := [fsBold, fsUnderline];
      Font.Color := clGrayText;
      Brush.Style := bsClear;
      x := Rect.Left + ((DrawGrid.ColWidths[aCol] - TextWidth(AText)) div 2);
      y := Rect.Top + 2;
      TextOut(x, y, AText);
    end
    else { not the header row}
    begin
      if ACol = 0 then
        Font.Color := clGrayText
      else
        Font.Color := clBlack;
      Font.Style := [];
      if gdSelected in State then
      begin
        ARect := DrawGrid.Selection;

        if ACol = 0 then
          Brush.Color := clBtnFace
        else
          Brush.Color := RGB(186, 230, 235);
        Brush.Style := bsSolid;
        FillRect(Rect);
        if ARect.Top = aRow then
          DrawEdges(Rect, False, True, False, False, clGray);
        if ARect.Bottom = aRow then
          DrawEdges(Rect, False, False, False, True, clGray);
        if ARect.Left = aCol then
          DrawEdges(Rect, True, False, False, False, clGray);
        if ARect.Right = aCol then
          DrawEdges(Rect, False, False, True, False, clGray);
      end
      else { not selected}
      begin
        Brush.Color := clWhite;
        Brush.Style := bsSolid;
        FillRect(Rect);
        if DrawGrid.Row = (aRow + 1) then
          DrawEdges(Rect, False, False, True, True, clSilver)
        else
          DrawEdges(Rect, False, False, True, True, clSilver);
      end;
      if  ARow = DrawGrid.Row then
        if ACol = 0 then
          DrawEdges(Rect, True, False, False, False, clGreen);
      x := Rect.Left + 2;
      y := Rect.Top + 2;
      Brush.Style := bsclear;
      if ACol = 0 then
        AText := TaxaNames[ARow - 1]
      else
      begin
        AText := SpeciesNames[ARow - 1];
      end;
      if AText <> EmptyStr then
        TextOut(x, y, AText);
    end;
  end;
end;

procedure TSpeciesMapDlg.DrawGridGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  if (ACol = 0) or (ARow = 0) then
    Exit;
  if (ARow <= SpeciesNames.Count) then
    Value := SpeciesNames[ARow - 1]
end;

procedure TSpeciesMapDlg.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    DrawGrid.Invalidate
  end;
end;

procedure TSpeciesMapDlg.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem
  {$ENDIF}
end;

procedure TSpeciesMapDlg.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.DrawGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if not Visible then Exit;
  if (ACol = 0) or (ARow = 0) then
  begin
    DrawGrid.Options := DrawGrid.Options - [goEditing]
  end
  else
    DrawGrid.Options := DrawGrid.Options + [goEditing];
  CanSelect := True;
  DrawGrid.Invalidate;
end;

procedure TSpeciesMapDlg.DrawGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if (ACol = 0) or (ARow = 0) or (not Visible) then
    Exit;
  if ARow <= SpeciesNames.Count then
    SpeciesNames[ARow - 1] := Value;
end;

procedure TSpeciesMapDlg.UpdateSelectedIndices;
var
  aRect: TGridRect;
  i: Integer;
  Index: Integer;
  Response: Integer;
begin
  aRect := DrawGrid.Selection;
  if (aRect.Top >= 0) and (aRect.Bottom > 0) then
  begin
    Index := Max(1, aRect.Top) - 1;
    SetLength(FSelectedIndices, aRect.Bottom - Max(1, aRect.Top) + 1);
    for i := 0 to Length(FSelectedIndices) - 1 do
    begin
      FSelectedIndices[i] := Index;
      inc(Index);
    end;
  end
  else
  begin
    Response := MessageDlg('Nothing selected. Apply filter(s) to all rows?', mtConfirmation, mbYesNo, 0);
    if Response <> mrYes then
    begin
      SetLength(FSelectedIndices, 0);
    end
    else
    begin
      aRect.Top := 1;
      aRect.Bottom := (DrawGrid.RowCount - 1);
      aRect.Left := 1;
      aRect.Right := 1;
      DrawGrid.Selection := aRect;
      UpdateSelectedIndices;
    end;
  end;
end;

function TSpeciesMapDlg.ValidateForm: Boolean;
var
  i: Integer;
begin
  Assert(TaxaNames.Count = SpeciesNames.Count);
  Result := False;
  for i := 0 to TaxaNames.Count - 1 do
  begin
    if Trim(SpeciesNames[i]) = EmptyStr then
    begin
      DrawGrid.Col := 1;
      DrawGrid.Row := i + 1;
      DrawGrid.SetFocus;
      ShowMessage('All taxa require a species name');
      Exit;
    end;
  end;
  Result := True;
end;

{ TSearchResult }

constructor TSearchResult.Create(ACol: Integer; ARow: Integer);
begin
  inherited Create;
  Row := ARow;
  Column := ACol;
end;

destructor TSearchResult.Destroy;
begin
  inherited;
end;

end.
