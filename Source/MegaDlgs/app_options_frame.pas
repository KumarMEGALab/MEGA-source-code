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

unit app_options_frame;

{$mode objfpc}{$H+}

interface
{$IFDEF VISUAL_BUILD}
uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, Spin, Types, Graphics,
  EditBtn, CheckLst, MegaConsts, manalysissettings, fpjson, jsonparser,
  KeywordConsts, MProcessPack;

const
  MyDefaultBgColor     : TColor = $00f7f8f8;
  MyActiveColor        : TColor = DEFAULT_ACTIVE_BG_COLOR;  // for background
  MyHighlightBgColor   : TColor = $00e7e7e7;
  MyNotApplicableColor : TColor = clSilver;  // for font
  MySeparatorColor     : TColor = clBlue;  // for fill
  MyUncheckedColor     : TColor = 15921906;
  DNAStr               : String = 'DNA';
  cDNAStr              : String = 'cDNA';
  ProteinStr           : String = 'Protein';
  MyEditableCellColor        : TColor = clWhite;  // for background
  UNCHECKMARK_IMAGE_INDEX = 0;
  CHECKMARK_IMAGE_INDEX = 1;
  GRAY_CHECKMARK_IMAGE_INDEX = 2;

type

  { TOptionsGridFrame }

  TOptionsGridFrame = class(TFrame)
    CheckListBox: TCheckListBox;
    CheckmarkList: TImageList;
    FileNameEdit: TFileNameEdit;
    FPickListComboBx: TComboBox;
    OptionsGrid: TDrawGrid;
    FloatSpinEdit: TFloatSpinEdit;
    IntegerSpinEdit: TSpinEdit;
    StringEdit: TEdit;
    procedure CheckListBoxClickCheck(Sender: TObject);
    procedure FileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEditExit(Sender: TObject);
    procedure FloatSpinEditChange(Sender: TObject);
    procedure FloatSpinEditExit(Sender: TObject);
    procedure FPickListComboBxChange(Sender: TObject);
    procedure IntegerSpinEditChange(Sender: TObject);
    procedure IntegerSpinEditExit(Sender: TObject);
    procedure OptionsGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure OptionsGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure OptionsGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OptionsGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure OptionsGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure StringEditChange(Sender: TObject);
    procedure StringEditExit(Sender: TObject);
  private
    FEditorRow: Integer;
    FDataType: TSnTokenCode;
    FHasAdvancedSettings: Boolean;
    FHelpName: String;
    FHelpUrl: String;
    FShowAdvancedSettings: Boolean;
    FInitialized: Boolean;
    FSettings: TAnalysisSettingsList;
    FUseNotSelectedString: Boolean;
    FVisibleSettings: TAnalysisSettingsList;
    procedure DrawRectEdges(ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
    procedure SetDataType(AValue: TSnTokenCode);
    procedure CheckHasAdvancedSettings;
    procedure SetHelpName(AValue: String);
    procedure SetHelpUrl(AValue: String);
    procedure SetUseNotSelectedString(AValue: Boolean);
    procedure UpdateVisibleSettings;
    procedure ToggleAdvancedOptions(showAdvanced: Boolean);
  public
    ValidateSettingsFunc: TValidateSettingsFunc;
    OnAppSettingsLoaded: TAppSettingsLoadedProc;
    cmdargs: String;
    GridLineColor: TColor;
    Checkmark: TBitmap;
    Uncheckmark: TBitmap;
    GrayCheckMark: TBitmap;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function SettingIsEditable(aRow: Integer): Boolean;
    function HasSetting(AName: String): Boolean;
    function GetSetting(AName: String): TAnalysisSetting;
    procedure ClearSettings;
    function LoadJson(Filename: String; AType: TSnTokenCode): Boolean;
    function GetJson: TJSONObject;
    function ComputeCmdargs: String; // Creates the actual string of command line arguments from all the FSettings in the dlg.
    procedure GetCmdArgs(var Options: TStringList);
    procedure GetShortCmdArgs(var Options: TStringList);
    procedure GetHumanReadableArgs(var options: TStringList);
    function ValidateSettings(var msg: String): Boolean;
    function SelectedGeneticCode: String;
    function GetHeaderHeight: Integer;
    function GetCellColor(ARow: Integer): TColor;
    procedure ResizeColumns;
    procedure AdjustColWidths;
    function SelectionWidth: Integer;
    procedure Initialize(aParent: TWinControl);
    procedure DoResize(Sender: TObject);
    function GetMaoStrings(var AList: TStringList; const ProcessPack: TProcessPack): Boolean;
    function SumColumnWidths: Integer;
    property Initialized: Boolean read FInitialized write FInitialized;
    property DataType: TSnTokenCode read FDataType write SetDataType;
    property UseNotSelectedString: Boolean read FUseNotSelectedString write SetUseNotSelectedString;
    property HelpUrl: String read FHelpUrl write SetHelpUrl;
    property HelpName: String read FHelpName write SetHelpName;
  end;
{$ENDIF}
implementation
{$IFDEF VISUAL_BUILD}

{$R *.lfm}

uses
  MegaAnalysisPrefStrings, math,LCLType, LCL, LCLIntf, MegaUtils, typinfo,
  MSelectGeneticCodeDlg, manalysisprefdlg, MD_InputSeqData, Dialogs, StringUtils;

{ TOptionsGridFrame }

procedure TOptionsGridFrame.OptionsGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  RowValue: String;
  AText: String;
  x, y: Integer;
  ASetting: TAnalysisSetting;
  aTextStyle: TTextStyle;
begin
  if (not Visible) or (not FInitialized) or (not Assigned(Parent)) then Exit;
  with OptionsGrid.Canvas do
  begin
    aTextStyle := TextStyle;
    aTextStyle.Layout := tlCenter;
    aTextStyle.Alignment := taCenter;
    Font.Style := [];
    Pen.Color := GridLineColor;
    if ARow = 0 then { handle the header row}
    begin
      Brush.Color := $00aaaaaa;
      Font.Color := clBlack;
      FillRect(aRect);
      if aCol = 0 then
        RowValue := 'Option'
      else if aCol = 2 then
        RowValue := 'Setting'
      else
        RowValue := EmptyStr;
      y := aRect.Top + 2;
      x := aRect.Left + 2;
      Brush.Style := bsClear;
      Font.Style := [];
      TextRect(aRect, x, y, RowValue, aTextStyle);
      Pen.Color := GridLineColor;
      DrawRectEdges(aRect, aCol = 0, False, False, True);
    end
    else if FHasAdvancedSettings and (ARow = (OptionsGrid.RowCount-1)) then { the row to draw the Advanced Settings checkbox}
    begin
      Brush.Color := GetCellColor(aRow);
      FillRect(aRect);
      if aCol = 0 then
      begin
        x := aRect.Left + 2;
        y := aRect.Top + 2;
        Font.Color := clBlue;
        aTextStyle.Alignment := taRightJustify;
        TextRect(aRect, x, y, 'Show Advanced Settings ', aTextStyle);
      end
      else if aCol = 1 then
      begin
        x := aRect.left + 2;
        Y := aRect.Top + (aRect.Bottom - aRect.Top - CheckMark.Height) div 2;
        if FShowAdvancedSettings then
          Draw(x, y, Checkmark)
        else
          Draw(x, y, Uncheckmark);
      end;
      DrawRectEdges(aRect, (aCol = 0) and (aCol <> 3), False, aCol <> 2, True);
    end
    else
    begin
      ASetting := FVisibleSettings[aRow - 1];
      RowValue :=  ASetting.StringValue;
      if FUseNotSelectedString and (RowValue = NotApplicableStr) then
        RowValue := NotSelectedStr;
      if Length(RowValue) > 40 then
        RowValue := Mince(RowValue, 40);
      if ACol = 0 then
      begin
        if (RowValue = opsOperationType1) or (RowValue = opsPlus) then
        begin
          Font.Color := DEFAULT_ACTIVE_BG_COLOR;
          Font.Style := [fsBold];
        end;
        if (RowValue <> opsOperationType1) and (RowValue <> opsPlus) then
          Brush.Color := MyDefaultBgColor
        else
          Brush.Color := GetCellColor(aRow);
        FillRect(aRect);
        if ASetting.IsAdvancedSetting and (aCol = 0) then
          Font.Style := Font.Style + [fsItalic];
      end
      else
      begin
        Brush.Color := GetCellColor(aRow);
        FillRect(aRect);
        if not ASetting.IsApplicable then
        begin
          Font.Color := MyNotApplicableColor;
        end
        else if RowValue = opsPlus then
        begin
          RowValue := EmptyStr;
        end
        else
        begin  // this is important
          Font.Color := clBlack;
          Font.Style := [fsItalic];
        end;
      end;
      Pen.Color := GridLineColor;

      if (aRow <> 0) and (aRow <> (OptionsGrid.RowCount - 1)) then
      begin
        Pen.Color := GridLineColor;
        DrawRectEdges(aRect, aCol = 0, False, (aCol <> 2) and (ASetting.StringValue <> opsPlus), True);
      end;

      if ACol = 0 then
      begin
        AText := ASetting.DisplayName;
        x := aRect.Left + 2;
      end
      else if aCol = 1 then
      begin
        x := aRect.Left + 2;
        y := aRect.Top + Round((aRect.Bottom - aRect.Top - Checkmark.Height)/2);
        if ASetting.IndentLevel > 0 then
        begin
          if ASetting.IsOptional then
          begin
            if ASetting.IsChecked then
              Draw(x, y, Checkmark)
            else
              Draw(x, y, Uncheckmark);
          end
          else
            Draw(x, y, GrayCheckMark);
        end;
      end
      else
      begin
        AText := RowValue;
        x := aRect.Left + 2;
      end;
      if (aCol = 2) then
      begin
        aTextStyle.Alignment := taLeftJustify;
      end
      else
      begin
        aTextStyle.Alignment := taRightJustify;
        if RowValue = opsPlus then
          aText := UpperCase(AText);
        aText := aText + '  ';
      end;
      y := aRect.Top + 2;
      Brush.Style := bsClear;
      if (ACol = 0) and (ASetting.IndentLevel = 2) then
        Font.Style := [fsItalic];
      if (aCol <> 1) and (aCol <> 3) then
        TextRect(aRect, x, y, AText, aTextStyle);
    end;
  end;
end;

procedure TOptionsGridFrame.OptionsGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
  if aRow <= 0 then
    Exit;
  if (aRow - 1) < FVisibleSettings.Count then
    if FVisibleSettings[aRow - 1].Hint <> EmptyStr then
      HintText := FVisibleSettings[aRow - 1].Hint;
end;

procedure TOptionsGridFrame.FloatSpinEditExit(Sender: TObject);
var
  ASetting: TFloatAnalysisSetting;
begin
  if (OptionsGrid.Row - 1) >= FVisibleSettings.Count then
    Exit;
  ASetting := TFloatAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
  ASetting.Value := FloatSpinEdit.Value;
end;

procedure TOptionsGridFrame.FloatSpinEditChange(Sender: TObject);
var
  ASetting: TFloatAnalysisSetting;
begin
  if (OptionsGrid.Row - 1) >= FVisibleSettings.Count then
    Exit;
  if FVisibleSettings[OptionsGrid.Row -1] is TFloatAnalysisSetting then
  begin
    ASetting := TFloatAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
    ASetting.Value := FloatSpinEdit.Value;
  end;
end;

procedure TOptionsGridFrame.FileNameEditAcceptFileName(Sender: TObject; var Value: String);
var
  aSetting: TStringAnalysisSetting;
begin
  if not FileExists(Value) then
  begin
    ShowMessage('The specified input file was not found');
    Exit;
  end;
  aSetting := TStringAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
  aSetting.Value := Value;
  aSetting.IsApplicable := True;
  aSetting.IsReadOnly := True;
end;

procedure TOptionsGridFrame.CheckListBoxClickCheck(Sender: TObject);
var
  ASetting: TMultiPickListSetting = nil;
  i: Integer;
begin
  if CheckListBox.Count = 0 then
    Exit;
  if FVisibleSettings[OptionsGrid.Row - 1] is TMultiPickListSetting then
  begin
    ASetting := TMultiPickListSetting(FVisibleSettings[OptionsGrid.Row - 1]);
    if CheckListBox.Count > 0 then
      for i := 0 to CheckListBox.Count - 1 do
        ASetting.Checked[i] := CheckListBox.Checked[i];
    {$IFDEF UNIX}
    OptionsGrid.Col := 0;
    {$ENDIF}
    OptionsGrid.Invalidate;
  end;
end;

procedure TOptionsGridFrame.FileNameEditExit(Sender: TObject);
var
  aSetting: TStringAnalysisSetting;
begin
  if OptionsGrid.Row <> FEditorRow then { bug fix for macOS where this procedure fires out of order and at weird times}
    Exit;
  if FileExists(FileNameEdit.Text) then
  begin
    aSetting := TStringAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
    aSetting.Value := FileNameEdit.Text;
  end;
end;

procedure TOptionsGridFrame.FPickListComboBxChange(Sender: TObject);
var
  ASetting: TPickListAnalysisSetting;
begin
  if FPicklistComboBx.ItemIndex < 0 then
    Exit;
  if FVisibleSettings[OptionsGrid.Row - 1] is TPickListAnalysisSetting then
  begin
    ASetting := TPickListAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
    ASetting.ItemIndex := FPickListComboBx.ItemIndex;
    {$IFDEF UNIX}
    OptionsGrid.Col := 0; { for some reason, on Linux, FPickListComboBox won't surrender focus and the user cannot select another editable row}
    {$ENDIF}
    OptionsGrid.Invalidate;
  end;
end;

procedure TOptionsGridFrame.IntegerSpinEditChange(Sender: TObject);
var
  ASetting: TIntegerAnalysisSetting;
begin
  if (OptionsGrid.Row <= 0) or ((OptionsGrid.Row - 1) >= FVisibleSettings.Count) then
    Exit;
  if FVisibleSettings[OptionsGrid.Row - 1] is TIntegerAnalysisSetting then
  begin
    ASetting := TIntegerAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
    ASetting.Value := IntegerSpinEdit.Value;
  end;
end;

procedure TOptionsGridFrame.IntegerSpinEditExit(Sender: TObject);
var
  ASetting: TIntegerAnalysisSetting;
begin
  if (OptionsGrid.Row <= 0) or ((OptionsGrid.Row - 1) >= FVisibleSettings.Count) then
    Exit;
  ASetting := TIntegerAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
  ASetting.Value := IntegerSpinEdit.Value;
end;

procedure TOptionsGridFrame.OptionsGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
  cellCoords: TRect;
  aSetting: TAnalysisSetting = nil;
  canSelect: Boolean = True;
begin
  with OptionsGrid do
  begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol = 1) and (ARow <> 0) and (ARow <= FVisibleSettings.Count) then
    begin
      aSetting := FVisibleSettings[ARow - 1];
      if aSetting.IsOptional then
        aSetting.IsChecked := not aSetting.IsChecked;
      if aSetting is TStringAnalysisSetting then
        OptionsGridSelectCell(Sender, 2, aRow, canSelect);
    end;
    cellCoords := CellRect(ACol, ARow);
    if FHasAdvancedSettings and (ARow = (OptionsGrid.RowCount - 1)) then
    begin
      if (ACol = 1) and (Button = mbLeft) then
      begin
        if abs(X - cellCoords.Right) <= Checkmark.Width*2 then
          ToggleAdvancedOptions(not FShowAdvancedSettings);
      end;
    end;
    Invalidate;
  end;
end;

procedure TOptionsGridFrame.OptionsGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  OptionsGrid.Invalidate;
end;

procedure TOptionsGridFrame.OptionsGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
var
  ASetting: TAnalysisSetting = nil;
  PlistStrings: TStringList = nil;
  aRect: TRect;
  i: Integer;
begin
  if (aCol = 0) or (aCol = 1) or (aCol = 3) or (aRow = 0) then { disallow editing of read-only cells}
  begin
    Editor := nil;
    Exit;
  end;

  if FHasAdvancedSettings and (aRow = (OptionsGrid.RowCount - 1)) then
  begin
    Editor := nil;
    Exit;
  end;

  ASetting := FVisibleSettings[aRow - 1];
  if ASetting.IsReadOnly or (ASetting.IsOptional and (not ASetting.IsChecked)) then
  begin
    Editor := nil;
    Exit;
  end;

  case ASetting.SettingType of
  stInteger:
    begin
      with (ASetting as TIntegerAnalysisSetting) do
      begin
        IntegerSpinEdit.MinValue := Min;
        IntegerSpinEdit.MaxValue := Max;
        IntegerSpinEdit.Value := Value;
        IntegerSpinEdit.Increment := Increment;
        IntegerSpinEdit.BoundsRect := OptionsGrid.CellRect(aCol, aRow);
        Editor := IntegerSpinEdit;
      end;
    end;
  stFloat:
    begin
      with (ASetting as TFloatAnalysisSetting) do
      begin
        FloatSpinEdit.MinValue := Min;
        FloatSpinEdit.MaxValue := Max;
        FloatSpinEdit.Value := Value;
        FloatSpinEdit.Increment := Increment;
        FloatSpinEdit.BoundsRect := OptionsGrid.CellRect(aCol, aRow);
        Editor := FloatSpinEdit;
      end;
    end;
  stString:
    begin
      with(ASetting as TStringAnalysisSetting) do
      begin
        if (ASetting.Name = 'Partition File') or (ASetting.Name = 'Cluster File') then
        begin
          with (ASetting as TStringAnalysisSetting) do
          begin
            if (Value <> EmptyStr) and (FileExists(Value)) then
              FileNameEdit.FileName := Value;
            FileNameEdit.BoundsRect := OptionsGrid.CellRect(aCol, aRow);
            Editor := FileNameEdit;
          end;
        end
        else
        begin
          StringEdit.BoundsRect := OptionsGrid.CellRect(aCol, aRow);
          StringEdit.Text := Value;
          Editor := StringEdit;
        end;
      end;
    end;
  stPicklist:
    begin
      with (ASetting as TPickListAnalysisSetting) do
      begin
        FPickListComboBx.Items.Clear;
        PlistStrings := PickListStrings;
        FPickListComboBx.Items.AddStrings(PlistStrings);
        PlistStrings.Free;
        FPickListComboBx.BoundsRect := OptionsGrid.CellRect(aCol,aRow);
        FPickListComboBx.ItemIndex := ItemIndex;
        FPickListComboBx.Hint := Description;
        Editor := FPickListComboBx;
      end;
    end;
  stMultiPickList:
    begin
      with ASetting as TMultiPickListSetting do
      begin
        CheckListBox.Clear;
        plistStrings := PickListStrings;
        CheckListBox.Items.AddStrings(plistStrings);
        if plistStrings.Count > 0 then
          for i := 0 to plistStrings.Count - 1 do
            CheckListBox.Checked[i] := Checked[i];
        PlistStrings.Free;
        aRect := OptionsGrid.CellRect(aCol, aRow);
        CheckListBox.Top := aRect.Top;
        CheckListBox.Left := aRect.Left;
        CheckListBox.Width := aRect.Right - aRect.Left;
        CheckListBox.ItemIndex := ItemIndex;
        CheckListBox.Hint := Description;
        CheckListBox.Height := CheckListBox.ItemHeight*CheckListBox.Count;
        Editor := CheckListBox;
      end;
    end;
  end;
  FEditorRow := aRow;
end;

procedure TOptionsGridFrame.StringEditChange(Sender: TObject);
var
  ASetting: TStringAnalysisSetting;
begin
  if (OptionsGrid.Row - 1) >= FVisibleSettings.Count then
    Exit;
  ASetting := TStringAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
  ASetting.Value := StringEdit.Text
end;

procedure TOptionsGridFrame.StringEditExit(Sender: TObject);
var
  ASetting: TStringAnalysisSetting;
begin
  if (OptionsGrid.Row - 1) >= FVisibleSettings.Count then
    Exit;
  ASetting := TStringAnalysisSetting(FVisibleSettings[OptionsGrid.Row - 1]);
  ASetting.Value := StringEdit.Text
end;

function TOptionsGridFrame.SettingIsEditable(aRow: Integer): Boolean;
var
  aSetting: TAnalysisSetting;
begin
  Result := False;
  if (aRow > 0) and (aRow < (FVisibleSettings.Count - 1)) then
  begin
    ASetting := FVisibleSettings[aRow - 1];
    if Assigned(ASetting) then
      Result := ((not ASetting.IsReadOnly) and aSetting.IsApplicable);
  end;
end;

function TOptionsGridFrame.HasSetting(AName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FSettings.Count = 0 then
    Exit;
  for i := 0 to FSettings.Count - 1 do
  begin
    if AName = TAnalysisSetting(FSettings[i]).Name then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TOptionsGridFrame.GetSetting(AName: String): TAnalysisSetting;
var
  i: Integer;
begin
  Result := nil;
  if FSettings.Count = 0 then
    Exit;
  for i := 0 to FSettings.Count - 1 do
  begin
    if AName = TAnalysisSetting(FSettings[i]).Name then
    begin
      Result := TAnalysisSetting(FSettings[i]);
      Exit;
    end;
  end;
end;

procedure TOptionsGridFrame.ClearSettings;
var
  i: Integer;
begin
  if Assigned(FSettings) then
  begin
    if FSettings.Count > 0 then
      for i := 0 to FSettings.Count - 1 do
        TAnalysisSetting(FSettings[i]).Free;
    FSettings.Clear;
  end;
  if Assigned(FVisibleSettings) then
    FVisibleSettings.Clear;
  IntegerSpinEdit.Value := 0;
  FloatSpinEdit.Value := 0.0;
end;

function TOptionsGridFrame.LoadJson(Filename: String; AType: TSnTokenCode): Boolean;
var
  AParser: TJSONParser;
  AJson: TJSONObject;
  AData: TJSONData;
  AList: TStringList;
  i: Integer;
  NumSettings: Integer;
  ASetting: TAnalysisSetting;
begin
  try
    try
      DataType := AType;
      ClearSettings; { we might be just resetting to the default values}
      if not FileExists(Filename) then
        raise Exception.Create('Missing JSON file: ' + Filename);
      AList := TStringList.Create;
      AList.LoadFromFile(Filename);
      AParser := TJSONParser.Create(AList.Text, []);
      AData := AParser.Parse;

      if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
        raise Exception.Create('Failed to parse JSON file');
      AJson := TJSONObject(AData);

      AData := AJson.Find('Count', jtNumber);
      if not Assigned(AData) then
        raise Exception.Create('failed to parse JSON settings');
      NumSettings := AData.Value;

      if NumSettings > 0 then
      begin
        for i := 0 to NumSettings - 1 do
        begin
          AData := AJson.Find('FSettings-' + IntToStr(i), jtObject);
          if not Assigned(AData) then
            raise Exception.Create('failed to parse JSON settings');
          ASetting := GetSettingFromJson(AData.AsJSON);
          if not Assigned(ASetting) then
            raise Exception.Create('Failed to load analysis setting from JSON');
          if (ASetting.LongCmdFlag = 'input.infile_') then
             ASetting.SetValueFromStr(D_InputSeqData.SourceFileName);
          if (ASetting.LongCmdFlag = 'input.starting_tree_') then
             ASetting.SetValueFromStr(AnalysisPrefDlg.TreePack.TreeFile);
          FSettings.Add(ASetting);
        end;
      end;
      if Assigned(OnAppSettingsLoaded) then
        OnAppSettingsLoaded(FSettings);
      CheckHasAdvancedSettings;
      UpdateVisibleSettings;
      if FHasAdvancedSettings then
        OptionsGrid.RowCount := FVisibleSettings.Count + 2
      else
        OptionsGrid.RowCount := FVisibleSettings.Count + 1;
      DoResize(Self);
      OptionsGrid.Invalidate;
      Result := True;
    except
      on E:Exception do
        ShowMessage('Oh no! Loading settings from JSON failed with the following message: ' + E.Message);
    end;
  finally
    if Assigned(AList) then
      AList.Free;
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(AJson) then
      AJson.Free;
  end;
end;

function TOptionsGridFrame.GetJson: TJSONObject;
var
  i: Integer;
begin
  Assert(Assigned(FSettings));
  Result := TJSONObject.Create;
  Result.Add('Count', FSettings.Count);
  if FSettings.Count > 0 then
  begin
    for i := 0 to FSettings.Count - 1 do
    begin
      Result.Add('FSettings-' + IntToStr(i), TAnalysisSetting(FSettings[i]).GetJson);
    end;
  end;
end;

function TOptionsGridFrame.ComputeCmdargs: String;
var
  i: Integer;
  ASetting: TAnalysisSetting;
begin
  cmdargs := EmptyStr;
  for i := 0 to FSettings.Count - 1 do
  begin
    ASetting := TAnalysisSetting(FSettings[i]);
    cmdargs := cmdargs + ASetting.LongCmdFlag + ' ' + ASetting.StringValue + ' ';
  end;
  cmdargs := Trim(cmdargs);
  Result := cmdargs;
end;

procedure TOptionsGridFrame.GetCmdArgs(var Options: TStringList);
var
  i: Integer;
  ASetting: TAnalysisSetting =  nil;
begin
  for i := 0 to FSettings.Count - 1 do
  begin
    ASetting := TAnalysisSetting(FSettings[i]);
    if (not ASetting.IsApplicable) or ASetting.IsReadOnly then
      continue;
    if (ASetting is TMultiPickListSetting) and (Trim(TMultiPickListSetting(ASetting).GetLongCmdValue) <> EmptyStr) then
    begin
      Options.Add(ASetting.ShortCmdFlag + '=' + TMultiPickListSetting(ASetting).GetLongCmdValue);
    end
    else if (ASetting is TPickListAnalysisSetting) and (Trim(TPickListAnalysisSetting(ASetting).GetLongCmdValue) <> EmptyStr) then
      Options.Add(ASetting.LongCmdFlag + '=' + TPickListAnalysisSetting(ASetting).GetLongCmdValue)
    else
      if not (Trim(ASetting.StringValue) = EmptyStr) then
         Options.Add(ASetting.LongCmdFlag + '=' + ASetting.StringValue);
  end;
end;

procedure TOptionsGridFrame.GetShortCmdArgs(var Options: TStringList);
var
  i: Integer;
  ASetting: TAnalysisSetting = nil;
begin
  for i := 0 to FSettings.Count - 1 do
  begin
    ASetting := TAnalysisSetting(FSettings[i]);
    if (not ASetting.IsApplicable) or (ASetting.StringValue = opsPlus) then
      continue;
    if ASetting.IsOptional and (not ASetting.IsChecked) then
      continue;
    if (ASetting is TPickListAnalysisSetting) then
    begin
      if (Trim(TPickListAnalysisSetting(ASetting).GetLongCmdValue) <> EmptyStr) then
        Options.Add(ASetting.ShortCmdFlag + '=' + TPickListAnalysisSetting(ASetting).GetLongCmdValue);
    end
    else
      if not(Trim(ASetting.StringValue) = EmptyStr) then
         Options.Add(ASetting.ShortCmdFlag + '=' + ASetting.StringValue);
  end;
end;

procedure TOptionsGridFrame.GetHumanReadableArgs(var options: TStringList);
var
  i: Integer;
  aSetting: TAnalysisSetting =  nil;
begin
  for i := 0 to FSettings.Count - 1 do
  begin
    aSetting := TAnalysisSetting(FSettings[i]);
    if (not ASetting.IsApplicable) or ASetting.IsReadOnly then
      continue;
    options.Add(aSetting.DisplayName + '=' + aSetting.StringValue);
  end;
end;

function TOptionsGridFrame.ValidateSettings(var msg: String): Boolean;
var
  selectedOptions: TStringList = nil;
begin
  Result := True;
  if Assigned(ValidateSettingsFunc) then
  begin
    try
      selectedOptions := TStringList.Create;
      GetShortCmdArgs(selectedOptions);
      Result := ValidateSettingsFunc(selectedOptions, msg);
    finally
      if Assigned(selectedOptions) then
        selectedOptions.Free;
    end;
  end;
end;

function TOptionsGridFrame.SelectedGeneticCode: String;
var
  TableName: String;
  GenCodeDlg: TSelectGeneticCodeDlg = nil;
begin
  Result := EmptyStr;

  if HasSetting(opsGeneticCode2) and GetSetting(opsGeneticCode2).IsApplicable then
  begin
    try
      TableName := GetSetting(opsGeneticCode2).StringValue;
      GenCodeDlg := TSelectGeneticCodeDlg.Create(nil);
      GenCodeDlg.CodeTableName := TableName;
      Result := GenCodeDlg.CodeTable;
    finally
      if Assigned(GenCodeDlg) then
        GenCodeDlg.Free;
    end;
  end
  else if HasSetting(opsGeneticCode2) then
    Result := NotApplicableStr;
end;

procedure TOptionsGridFrame.DrawRectEdges(ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
begin
  if not Visible then Exit;
  with OptionsGrid.Canvas do
  begin
    if DoLeft then
      Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
    if DoTop then
      Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
    if DoRight then
      Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
    if DoBottom then
      Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  end;
end;

procedure TOptionsGridFrame.SetDataType(AValue: TSnTokenCode);
begin
  if FDataType = AValue then Exit;
  FDataType := AValue;
end;

procedure TOptionsGridFrame.CheckHasAdvancedSettings;
var
  i: Integer;
begin
  FHasAdvancedSettings := False;
  if Assigned(FSettings) and (FSettings.Count > 0) then
    for i := 0 to FSettings.Count - 1 do
      if FSettings[i].IsAdvancedSetting then
      begin
        FHasAdvancedSettings := True;
        Exit;
      end;
end;

procedure TOptionsGridFrame.SetHelpName(AValue: String);
begin
  if FHelpName = AValue then Exit;
  FHelpName := AValue;
end;

procedure TOptionsGridFrame.SetHelpUrl(AValue: String);
begin
  if FHelpUrl = AValue then Exit;
  FHelpUrl := AValue;
end;

procedure TOptionsGridFrame.SetUseNotSelectedString(AValue: Boolean);
begin
  if FUseNotSelectedString = AValue then Exit;
  FUseNotSelectedString := AValue;
  OptionsGrid.Invalidate;
end;

procedure TOptionsGridFrame.UpdateVisibleSettings;
var
  i: Integer;
begin
  FVisibleSettings.Clear;
  if FSettings.Count > 0 then
    for i := 0 to FSettings.Count - 1 do
    begin
      if FSettings[i].IsVisible then
      begin
        if not FSettings[i].IsAdvancedSetting then
          FVisibleSettings.Add(FSettings[i])
        else if FShowAdvancedSettings then
          FVisibleSettings.Add(FSettings[i]);
      end;
    end;
end;

procedure TOptionsGridFrame.ToggleAdvancedOptions(showAdvanced: Boolean);
var
  i: Integer;

begin
  FShowAdvancedSettings := showAdvanced;
  FVisibleSettings.Clear;
  for i := 0 to FSettings.Count - 1 do
  begin
    if FShowAdvancedSettings then
      FVisibleSettings.Add(FSettings[i])
    else if not FSettings[i].IsAdvancedSetting then
      FVisibleSettings.Add(FSettings[i]);
  end;
  if FHasAdvancedSettings then
    OptionsGrid.RowCount := FVisibleSettings.Count + 2
  else
    OptionsGrid.RowCount := FVisibleSettings.Count + 1;
  OptionsGrid.Invalidate;
end;

function TOptionsGridFrame.GetHeaderHeight: Integer;
begin
  Result := 26;
  { TODO 2 -oglen -cmegaxplatform : update GetHeaderHeight to calculate it dynamically for linux and mac }
  {$IFDEF MSWINDOWS}
  Result := GetSystemMetrics(SM_CYCAPTION);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := 26;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := 26;
  {$ENDIF}
end;

function TOptionsGridFrame.GetCellColor(ARow: Integer): TColor;
var
  ASetting: TAnalysisSetting = nil;
begin
  Assert(aRow <> 0);
  if FHasAdvancedSettings and (ARow = (OptionsGrid.RowCount - 1)) then
  begin
    Result := MyDefaultBgColor;
    Exit;
  end;
  ASetting := FVisibleSettings[aRow - 1];
  if ASetting.StringValue = opsPlus then
    Result := MyHighlightBgColor
  else if not ASetting.IsReadOnly then
    Result := MyEditableCellColor
  else
    Result := MyDefaultBgColor;
end;

procedure TOptionsGridFrame.ResizeColumns;
const
  Margin = 10;
var
  i: Integer;
  MaxWidth: Integer;
  AWidth: Integer;
begin
  { do the first column}
  MaxWidth := OptionsGrid.Canvas.TextWidth('Option');
  OptionsGrid.Canvas.Font.Style := [fsBold];
  for i := 0 to FVisibleSettings.Count - 1 do
  begin
    AWidth := OptionsGrid.Canvas.TextWidth(UpperCase(FVisibleSettings[i].DisplayName) + ' ');
    if AWidth > MaxWidth then
      MaxWidth := AWidth;
  end;
  OptionsGrid.ColWidths[0] := Max(100, MaxWidth + Margin);

  { check mark column}
  OptionsGrid.ColWidths[1] := 16;

  { now the third column}
  OptionsGrid.Canvas.Font.Style := [];
  MaxWidth := OptionsGrid.Canvas.TextWidth('Setting');

  for i := 0 to FVisibleSettings.Count - 1 do
  begin

    AWidth := OptionsGrid.Canvas.TextWidth(FVisibleSettings[i].StringValue);
    if AWidth > MaxWidth then
      MaxWidth := AWidth;
  end;
  OptionsGrid.ColWidths[2] := Max(200, MaxWidth + Margin);
  OptionsGrid.ColWidths[3] := 2;
end;

procedure TOptionsGridFrame.AdjustColWidths;
var
  SumWidths: Integer;
  WidthDiff: Integer;
begin
  SumWidths := (OptionsGrid.ColWidths[0] + OptionsGrid.ColWidths[1] + OptionsGrid.ColWidths[3]);
  WidthDiff := (OptionsGrid.Width - SumWidths);
  OptionsGrid.ColWidths[2] := Max(50, WidthDiff);
end;

function TOptionsGridFrame.SelectionWidth: Integer;
var
  ARect: TRect;
begin
  ARect := OptionsGrid.CellRect(1, OptionsGrid.TopRow);
  result := (ARect.Right + 1) - ARect.Left;
end;

constructor TOptionsGridFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditorRow := -1;
  FInitialized := False;
  GridLineColor := $00c5c5c5;
  FDataType := snNoToken;
  CheckMark   := TBitMap.Create;
  UnCheckMark := TBitMap.Create;
  GrayCheckMark := TBitMap.Create;
  CheckmarkList.GetBitmap(CHECKMARK_IMAGE_INDEX, CheckMark);
  CheckmarkList.GetBitmap(UNCHECKMARK_IMAGE_INDEX, UnCheckMark);
  CheckmarkList.GetBitmap(GRAY_CHECKMARK_IMAGE_INDEX, GrayCheckMark);
  FSettings := TAnalysisSettingsList.Create;
  FVisibleSettings := TAnalysisSettingsList.Create;
  FHasAdvancedSettings := False;
  FShowAdvancedSettings := False;
  FUseNotSelectedString := False;
end;

destructor TOptionsGridFrame.Destroy;
begin
  ClearSettings;
  if Assigned(FSettings) then
    FSettings.Free;
  if Assigned(FVisibleSettings) then
    FVisibleSettings.Free;
  if Assigned(CheckMark) then
    CheckMark.Free;
  if Assigned(UnCheckMark) then
    UnCheckMark.Free;
  if Assigned(GrayCheckMark) then
    GrayCheckMark.Free;
  inherited Destroy;
end;

procedure TOptionsGridFrame.Initialize(aParent: TWinControl);
begin
  Parent := aParent;
  Align := alClient;
  OptionsGrid.Align := alClient;
  if Assigned(FVisibleSettings) then
  begin
    if FHasAdvancedSettings then
      OptionsGrid.RowCount := FVisibleSettings.Count + 2
    else
      OptionsGrid.RowCount := FVisibleSettings.Count + 1
  end
  else
    OptionsGrid.RowCount := 1;
  FInitialized := True;
end;

procedure TOptionsGridFrame.DoResize(Sender: TObject);
begin
  AdjustColWidths;
  if IntegerSpinEdit.Visible then
    IntegerSpinEdit.Width := SelectionWidth;
  if FloatSpinEdit.Visible then
    FloatSpinEdit.Width := SelectionWidth;
  if FPicklistComboBx.Visible then
    FPicklistComboBx.Width := SelectionWidth;
  if StringEdit.Visible then
    StringEdit.Width := SelectionWidth;
  OptionsGrid.Invalidate;
end;

function TOptionsGridFrame.GetMaoStrings(var AList: TStringList; const ProcessPack: TProcessPack): Boolean;
var
  FormatLen: Integer;
  FLStr: String;
  i: Integer;
  ASetting: TAnalysisSetting;
begin
  FormatLen := LengthOfLongestNameString(FSettings);
  FLStr := '%-' + IntToStr(FormatLen) + 's = %-' + IntToStr(FormatLen) + 's';
  AList.Clear;
  AddMaoFileHeader(AList, FLStr);
  AList.Add('[ DataSettings ]');
  if (DataType = snCoding) or (DataType = snCodingDna) then
    AList.Add(Format(FLStr, [DataTypeStr, GetEnumName(TypeInfo(TSnTokenCode), Integer(snNucleotide))])) { for backwards compatibility, megacc doesn't recognize snCoding}
  else
    AList.Add(Format(FLStr, [DataTypeStr, GetEnumName(TypeInfo(TSnTokenCode), Integer(DataType))]));
  if DataType = snNucleotide then
    AList.Add(Format(FLStr, [ContainsCodingNucStr, BoolToStr(False, true)]))
  else if (DataType = snCoding) or (DataType = snCodingDna) then
    AList.Add(Format(FLStr, [ContainsCodingNucStr, BoolToStr(True, true)]));

  AList.Add(Format(FLStr, [MissingBaseSymbolStr, DefaultMissingSymbol]));
  AList.Add(Format(FLStr, [IdenticalBaseSymbolStr, DefaultIdenticalSymbol]));
  AList.Add(Format(FLStr, [GapSymbolStr, DefaultGapSymbol]));

  AList.Add('[ ProcessTypes ]');
  for i := 0 to ProcessPack.Count - 1 do
    AList.Add(Format(FLStr, [GetEnumName(TypeInfo(TProcessType), integer(ProcessPack.ProcessTypes[i])), 'true']));

  AList.Add('[ AnalysisSettings ]');
  for i := 0 to FSettings.Count - 1 do
  begin
    ASetting := TAnalysisSetting(FSettings[i]);
    AList.Add(Format(FLStr, [ASetting.Name, ASetting.StringValue]));
  end;
  if HasSetting(opsGeneticCode2) then
    AList.Add(Format(FLStr, ['GeneticCodeTable', SelectedGeneticCode]));
  Result := True;
end;

function TOptionsGridFrame.SumColumnWidths: Integer;
var
  i: Integer;
begin
  Result := 0;
  if OptionsGrid.ColCount > 0 then
    for i := 0 to OptionsGrid.ColCount - 1 do
      Result += OptionsGrid.ColWidths[i];
end;
{$ENDIF}
end.

