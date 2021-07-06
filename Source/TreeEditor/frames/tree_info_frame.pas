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

unit tree_info_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Grids,
  frame_utils, Types, Graphics, ActnList;

const
  TIP_NAME_PLACEHOLDER = 'TIP_NAME=';
  TIP_NAME = 'TIP_NAME';

type

  { TTreeInfoFrame }

  TTreeInfoFrame = class(TFrame, ITreeToolbarFrame)
    CopyAction: TAction;
    SelectAllAction: TAction;
    ActionList1: TActionList;
    AnalysisSummaryGrid: TDrawGrid;
    AncestralRatesGrid: TDrawGrid;
    AncestralStatesGrid: TDrawGrid;
    BranchInfoGrid: TDrawGrid;
    TreeInfoGrid: TDrawGrid;
    GeneralInfoGrid: TDrawGrid;
    Image1: TImage;
    Label1: TLabel;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    GeneralInfoTab: TTabSheet;
    AncRateTab: TTabSheet;
    AnalysisSummaryTab: TTabSheet;
    TreeInfoTab: TTabSheet;
    BranchInfoTab: TTabSheet;
    AncStateTab: TTabSheet;
    procedure CopyActionExecute(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PageControlChange(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
  private
    FFixedCellColor: TColor;
    FIsClosing: Boolean;
    FPanel2Height: Integer;
    FGeneralInfoStrings: TStringList;
    FAnalysisInfoStrings: TStringList;
    FTreeInfoStrings: TStringList;
    FAncestralStatesStrings: TStringList;
    FAncestralRatesStrings: TStringList;
    function GetCellText(aCol, aRow: Integer): String;
    function GetStringsForActiveTab: TStringList;
    function GetGridForActiveTab: TDrawGrid;
    procedure ApplyUpdates;
    procedure CleanUpGeneralInfoStrings;
  public
    BranchInfoStrings: TStringList;
    function GetSelectedText: String;
    procedure UpdateColumnWidths;
    procedure Initialize;
    procedure Clear;
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);

    procedure DisplayGeneralInfo(Info: TStringList);
    procedure DisplayAnalysisInfo(info: TStringList);
    procedure DisplayTreeInfo(types, info: array of String; MaxIndex: Integer);
    procedure DisplayBranchInfo(types, info: array of String; MaxIndex: Integer; tipNames: TStringList);
    procedure DisplayAncState(types, info: array of String; MaxIndex: Integer);
    procedure DisplayAncRate(types, info: array of String; MaxIndex: Integer);
    property IsClosing: Boolean read FIsClosing write FIsClosing;
  end;

implementation

uses
  Dialogs, MegaConsts, Clipbrd;

{$R *.lfm}

{ TTreeInfoFrame }

procedure TTreeInfoFrame.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  s: TStringList = nil;
  g: TDrawGrid = nil;
  aText: String = '';
  x, y: Integer;
  ts: TTextStyle;
begin
  if FIsClosing then
    Exit;
  s := GetStringsForActiveTab;
  g := GetGridForActiveTab;
  if not Assigned(g) then
  begin
    ShowMessage('Application Error: missing display grid');
    Exit;
  end;
  if not g.Visible then
    Exit;
  aText := GetCellText(aCol, aRow);
  ts := g.Canvas.TextStyle;
  ts.Layout := tlCenter;
  if aRow = 0 then
    ts.Alignment := taCenter
  else
    ts.Alignment := taLeftJustify;

  with g.Canvas do
  begin
    Font.Color := clBlack;
    if aRow = 0 then
    begin
      Font.Color := DEFAULT_ACTIVE_BG_COLOR;
      Brush.Color := DEFAULT_ACTIVE_BG_COLOR2;
    end
    else
    begin
      if aCol = 0 then
      begin
        if (s.Count >= aRow) and (Trim(s.Names[aRow - 1]) = EmptyStr) then
        begin
          Font.Color := DEFAULT_ACTIVE_BG_COLOR;
          ts.Alignment := taCenter;
        end;
        Brush.Color := FFixedCellColor;
      end
      else
        Brush.Color := clWhite;
    end;
    Brush.Style := bsSolid;
    if gdSelected in aState then
    begin
      Brush.Color := clHighlight;
      Font.Color := clWhite;
    end;
    FillRect(aRect);
    x := aRect.Left + 4;
    y := aRect.Top + 4;
    Brush.Style := bsClear;
    if (aCol = 0) or (aRow = 0) then
      Font.Style := [fsBold]
    else
      Font.Style := [];
    if aText <> TIP_NAME then
      TextRect(aRect, x, y, Trim(aText), ts);
  end;
end;

procedure TTreeInfoFrame.CopyActionExecute(Sender: TObject);
begin
  Clipboard.AsText := GetSelectedText;
end;

procedure TTreeInfoFrame.PageControlChange(Sender: TObject);
begin
  ApplyUpdates;
end;

procedure TTreeInfoFrame.SelectAllActionExecute(Sender: TObject);
var
  g: TDrawGrid = nil;
begin
  g := GetGridForActiveTab;
  if Assigned(g) then
  begin
    g.Selection := Rect(0, 0, g.ColCount, g.RowCount);
    g.Invalidate;
  end;
end;

function TTreeInfoFrame.GetSelectedText: String;
var
  g: TDrawGrid = nil;
  i, j: Integer;
begin
  Result := EmptyStr;
  g := GetGridForActiveTab;
  with g do
  begin
    if (Selection.Top >= 0) and (Selection.Left >= 0) then
    begin
      for i := Selection.Top to Selection.Bottom do
        for j := Selection.Left to Selection.Right do
        begin
          Result := Result + #9 + GetCellText(j, i);
          if j = Selection.Right then
            Result := Trim(Result) + LineEnding;
        end;
      Result := Trim(Result);
    end;
  end;
end;

function TTreeInfoFrame.GetCellText(aCol, aRow: Integer): String;
var
  s: TStringList = nil;
begin
  s := GetStringsForActiveTab;

  if aRow = 0 then
  begin
    if aCol = 0 then
      Result := 'Name'
    else
      Result := 'Value';
  end
  else
  begin
    if aCol = 0 then
    begin
      if s.Count >= aRow then
      begin
        if Trim(s.Names[aRow - 1]) <> EmptyStr then
          Result := s.Names[aRow - 1]
        else
          Result := s[aRow - 1];
      end;
    end
    else
    begin
      if s.Count >= aRow then
      begin
        if Trim(s.Names[aRow - 1]) <> EmptyStr then
          Result := s.ValueFromIndex[aRow - 1]
        else
          Result := LineEnding;
      end;
    end;
    Result := StringReplace(Result, EQUALS_SIGN_PLACEHOLDER, '=', [rfReplaceAll]);
  end;
end;

function TTreeInfoFrame.GetStringsForActiveTab: TStringList;
begin
  if PageControl.ActivePage = GeneralInfoTab then
    Result := FGeneralInfoStrings
  else if PageControl.ActivePage = TreeInfoTab then
    Result := FTreeInfoStrings
  else if PageControl.ActivePage = BranchInfoTab then
    Result := BranchInfoStrings
  else if PageControl.ActivePage = AncStateTab then
    Result := FAncestralStatesStrings
  else if PageControl.ActivePage = AncRateTab then
    Result := FAncestralRatesStrings
  else if PageControl.ActivePage = AnalysisSummaryTab then
    Result := FAnalysisInfoStrings
  else
    Result := nil;
end;

function TTreeInfoFrame.GetGridForActiveTab: TDrawGrid;
begin
  if PageControl.ActivePage = GeneralInfoTab then
    Result := GeneralInfoGrid
  else if PageControl.ActivePage = TreeInfoTab then
    Result := TreeInfoGrid
  else if PageControl.ActivePage = BranchInfoTab then
    Result := BranchInfoGrid
  else if PageControl.ActivePage = AncStateTab then
    Result := AncestralStatesGrid
  else if PageControl.ActivePage = AncRateTab then
    Result := AncestralRatesGrid
  else if PageControl.ActivePage = AnalysisSummaryTab then
    Result := AnalysisSummaryGrid
  else
    Result := nil;
end;

procedure TTreeInfoFrame.UpdateColumnWidths;
var
  s: TStringList = nil;
  g: TDrawGrid = nil;
  i: Integer;
  str: String;
begin
  if FIsClosing then
    Exit;
  s := GetStringsForActiveTab;
  g := GetGridForActiveTab;
  if (not Assigned(g)) or (not Assigned(s)) then
    Exit;

  if s.Count > 0 then
  begin
    str := s.Names[0];
    if s.Count > 1 then
      for i := s.Count - 1 downto 1 do
        if Length(s.Names[i]) > Length(str) then
          str := s.Names[i];
    g.ColWidths[0] := g.Canvas.TextWidth(str + '      ');
    g.ColWidths[1] := g.Width - g.ColWidths[0];
  end;
  g.Invalidate;
end;

procedure TTreeInfoFrame.ApplyUpdates;
var
  g: TDrawGrid = nil;
begin
  if FIsClosing then
    Exit;
  UpdateColumnWidths;
  g := GetGridForActiveTab;
  g.Invalidate;
end;

procedure TTreeInfoFrame.CleanUpGeneralInfoStrings;
var
  i: Integer;
  s: String;
begin
  if FGeneralInfoStrings.Count > 0 then
  begin
    for i := FGeneralInfoStrings.Count - 1 downto 0 do
    begin
      FGeneralInfoStrings[i] := Trim(FGeneralInfoStrings[i]);
      s := FGeneralInfoStrings[i];
      if Pos('===', s) >= 1 then
        FGeneralInfoStrings[i] := Trim(FGeneralInfoStrings.Names[i]);
    end;
    for i := FGeneralInfoStrings.Count - 1 downto 0 do
      if FGeneralInfoStrings.IndexOf(FGeneralInfoStrings[i]) <> i then
        FGeneralInfoStrings.Delete(i);
  end;
end;

procedure TTreeInfoFrame.Initialize;
begin
  FGeneralInfoStrings := TStringList.Create;
  FAnalysisInfoStrings := TStringList.Create;
  FTreeInfoStrings := TStringList.Create;
  BranchInfoStrings := TStringList.Create;
  FAncestralStatesStrings := TStringList.Create;
  FAncestralRatesStrings := TStringList.Create;
  FFixedCellColor := $00f7f8f8;
  FIsClosing := False;
end;

procedure TTreeInfoFrame.Clear;
begin
  FGeneralInfoStrings.Free;
  FAnalysisInfoStrings.Free;
  FTreeInfoStrings.Free;
  BranchInfoStrings.Free;
  FAncestralStatesStrings.Free;
  FAncestralRatesStrings.Free;
end;

function TTreeInfoFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TTreeInfoFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TTreeInfoFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TTreeInfoFrame.ToggleCollapseBodyPanel;
var
  g: TDrawGrid = nil;
begin
  if FIsClosing then
    Exit;
  if Panel2.Visible then FPanel2Height := Panel2.Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
  begin
    Height := Panel1.Height + FPanel2Height;
    g := GetGridForActiveTab;
    if Assigned(g) then
      g.Invalidate;
  end
  else
    Height := Panel1.Height;
end;

procedure TTreeInfoFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TTreeInfoFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TTreeInfoFrame.DisplayGeneralInfo(Info: TStringList);
begin
  if FIsClosing then
    Exit;
  FGeneralInfoStrings.Assign(Info);
  CleanUpGeneralInfoStrings;
  GeneralInfoGrid.RowCount := FGeneralInfoStrings.Count + 1;
  PageControl.ActivePage := GeneralInfoTab;
  ApplyUpdates;
end;

procedure TTreeInfoFrame.DisplayAnalysisInfo(info: TStringList);
begin
  if FIsClosing then
    Exit;
  FAnalysisInfoStrings.Assign(info);
  AnalysisSummaryGrid.RowCount := info.Count + 1;
  ApplyUpdates;
end;

procedure TTreeInfoFrame.DisplayTreeInfo(types, info: array of String; MaxIndex: Integer);
var
  i: Integer;
begin
  if FIsClosing then
    Exit;
  FTreeInfoStrings.Clear;
  if MaxIndex > 0 then
    for i := 0 to MaxIndex do
      FTreeInfoStrings.Add(Format('%s=%s', [types[i], info[i]]));
  TreeInfoGrid.RowCount := MaxIndex + 2;
  ApplyUpdates;
end;

procedure TTreeInfoFrame.DisplayBranchInfo(types, info: array of String; MaxIndex: Integer; tipNames: TStringList);
var
  i: Integer;
begin
  if FIsClosing then
    Exit;
  BranchInfoStrings.Clear;
  if MaxIndex > 0 then
    for i := 0 to MaxIndex do
      BranchInfoStrings.Add(Format('%s=%s', [types[i], info[i]]));
  if tipNames.Count > 0 then
  begin
    BranchInfoStrings.Add('Taxa in clade:');
    for i := 0 to tipNames.Count - 1 do
      BranchInfoStrings.Add(TIP_NAME_PLACEHOLDER + tipNames[i]);
    BranchInfoGrid.RowCount := MaxIndex + 2 + tipNames.Count + 1;
  end
  else
    BranchInfoGrid.RowCount := MaxIndex + 2;

  ApplyUpdates;
end;

procedure TTreeInfoFrame.DisplayAncState(types, info: array of String; MaxIndex: Integer);
var
  i: Integer;
  index: Integer;
begin
  if FIsClosing then
    Exit;
  FAncestralStatesStrings.Clear;
  if MaxIndex > 0 then
    for i := 0 to MaxIndex do
      FAncestralStatesStrings.Add(Format('%s=%s', [types[i], info[i]]));
  index := FAncestralStatesStrings.Count - 1;
  while index >= 0 do
  begin
    if FAncestralStatesStrings[index] = '=' then
    begin
      FAncestralStatesStrings.Delete(index);
      dec(index);
    end
    else
      break;
  end;
  AncestralStatesGrid.RowCount := FAncestralStatesStrings.Count + 1;
  ApplyUpdates;
end;

procedure TTreeInfoFrame.DisplayAncRate(types, info: array of String; MaxIndex: Integer);
var
  i: Integer;
  index: Integer;
begin
  if FIsClosing then
    Exit;
  FAncestralRatesStrings.Clear;
  if MaxIndex > 0 then
    for i := 0 to MaxIndex do
      FAncestralRatesStrings.Add(Format('%s=%s', [types[i], info[i]]));
  index := FAncestralRatesStrings.Count - 1;
  while index >= 0 do
  begin
    if FAncestralRatesStrings[index] = '=' then
    begin
      FAncestralRatesStrings.Delete(index);
      dec(index);
    end
    else
      break;
  end;
  AncestralRatesGrid.RowCount := FAncestralRatesStrings.Count + 1;
  ApplyUpdates;
end;

end.

