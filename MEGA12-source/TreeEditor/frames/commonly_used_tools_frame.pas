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

unit commonly_used_tools_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  ActnList, Spin, Dialogs, frame_utils, Types;

type

  { TCommonlyUsedToolsFrame }

  TCommonlyUsedToolsFrame = class(TFrame, ITreeToolbarFrame)
    BranchLengthsChbox: TCheckBox;
    CaptionChbox: TCheckBox;
    FontNameLabel: TLabel;
    Panel7: TPanel;
    Label4: TLabel;
    HorizAutoSizeLabel: TLabel;
    FontSizeSpinEdit: TUpDown;
    TaxonSeparationLabel: TLabel;
    TreeWidthLabel: TLabel;
    TreeWidthUpDown: TUpDown;
    TaxonSeparationUpDown: TUpDown;
    VertAutoSizeLabel: TLabel;
    TopologyOnlyLabel: TLabel;
    SearchResultsLabel: TLabel;
    NoneRadioButton: TRadioButton;
    ShowAncestorsChbox: TCheckBox;
    AncStatesSiteLabel: TLabel;
    Panel4: TPanel;
    ScaleBarChbox: TCheckBox;
    AncestorsSiteSpinEdit: TSpinEdit;
    TipImagesChbox: TCheckBox;
    GroupNamesChbox: TCheckBox;
    FindNextAction: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    Label1: TLabel;
    Panel5: TPanel;
    BootstrapsRadioBtn: TRadioButton;
    Panel6: TPanel;
    SiteCoverageRadioBtn: TRadioButton;
    NodeIdsRadioBtn: TRadioButton;
    SearchTipNamesEdit: TLabeledEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ToolBar1: TToolBar;
    FindNextButton: TToolButton;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    procedure FindNextActionExecute(Sender: TObject);
    procedure FontNameLabelClick(Sender: TObject);
    procedure FontSizeSpinEditMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SearchTipNamesEditChange(Sender: TObject);
    procedure SearchTipNamesEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FNumSearchHits: Integer;
    FPrevQuery: String;
    FSearchResultsFocusedIndex: Integer;
    function GetIsShowNodeIds: Boolean;
    function GetIsShowSiteCoverage: Boolean;
    function GetIsShowStats: Boolean;
    procedure SetIsShowNodeIds(AValue: Boolean);
    procedure SetIsShowSiteCoverage(AValue: Boolean);
    procedure SetIsShowStats(AValue: Boolean);
  public
    SearchNamesNotify: TNotifyEvent;
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
    procedure AdjustHeight(toolbarsTotalHeight: Integer);
    procedure SetSearchResultsInfo(numResults: Integer; focusedResultIndex: Integer);
    function GetSelectedStatsDisplayType: String;
    procedure SetSelectedStatsDisplayType(t: String);
    function IsMyRadioButton(rb: TRadioButton): Boolean;

    property PrevQuery: AnsiString read FPrevQuery write FPrevQuery;
    property NumSearchHits: Integer read FNumSearchHits;
    property SearchResultsFocusedIndex: Integer read FSearchResultsFocusedIndex;
    property IsShowStats: Boolean read GetIsShowStats write SetIsShowStats;
    property IsShowSiteCoverage: Boolean read GetIsShowSiteCoverage write SetIsShowSiteCoverage;
    property IsShowNodeIds: Boolean read GetIsShowNodeIds write SetIsShowNodeIds;
  end;

implementation

{$R *.lfm}

uses
  MegaConsts, LCLType;

{ TCommonlyUsedToolsFrame }

procedure TCommonlyUsedToolsFrame.FindNextActionExecute(Sender: TObject);
begin
  if Assigned(SearchNamesNotify) then
    SearchNamesNotify(Sender);
end;

procedure TCommonlyUsedToolsFrame.FontNameLabelClick(Sender: TObject);
begin
  ToolButton1.Click;
end;

procedure TCommonlyUsedToolsFrame.FontSizeSpinEditMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TCommonlyUsedToolsFrame.SearchTipNamesEditChange(Sender: TObject);
begin
  if SearchResultsLabel.Caption <> EmptyStr then
    SearchResultsLabel.Caption := EmptyStr;
  if SearchTipNamesEdit.Text = EmptyStr then
  begin
    PrevQuery := EmptyStr;
    FindNextActionExecute(Sender);
  end;
end;

procedure TCommonlyUsedToolsFrame.SearchTipNamesEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FindNextButton.Click;
end;

function TCommonlyUsedToolsFrame.GetIsShowSiteCoverage: Boolean;
begin
  Result := SiteCoverageRadioBtn.Checked;
end;

function TCommonlyUsedToolsFrame.GetIsShowNodeIds: Boolean;
begin
  Result := NodeIdsRadioBtn.Checked;
end;

function TCommonlyUsedToolsFrame.GetIsShowStats: Boolean;
begin
  Result := BootstrapsRadioBtn.Checked;
end;

procedure TCommonlyUsedToolsFrame.SetIsShowNodeIds(AValue: Boolean);
begin
  NodeIdsRadioBtn.Checked := AValue;
end;

procedure TCommonlyUsedToolsFrame.SetIsShowSiteCoverage(AValue: Boolean);
begin
  SiteCoverageRadioBtn.Checked := AValue;
end;

procedure TCommonlyUsedToolsFrame.SetIsShowStats(AValue: Boolean);
begin
  BootstrapsRadioBtn.Checked := AValue;
end;

function TCommonlyUsedToolsFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TCommonlyUsedToolsFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TCommonlyUsedToolsFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TCommonlyUsedToolsFrame.ToggleCollapseBodyPanel;
begin
  { leave this frame expanded at all times per Sudhir}
end;

procedure TCommonlyUsedToolsFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TCommonlyUsedToolsFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TCommonlyUsedToolsFrame.AdjustHeight(toolbarsTotalHeight: Integer);
begin
  Panel2.Height := toolbarsTotalHeight + Panel3.Height + Panel4.Height + Panel5.Height + Panel6.Height + Panel7.Height + 20;
  ClientHeight := Panel1.Height + Panel2.Height;
end;

procedure TCommonlyUsedToolsFrame.SetSearchResultsInfo(numResults: Integer; focusedResultIndex: Integer);
begin
  FNumSearchHits := numResults;
  FSearchResultsFocusedIndex := focusedResultIndex;
  if FNumSearchHits > 0 then
  begin
    if FNumSearchHits = 1 then
      SearchResultsLabel.Caption := '1 match found'
    else
      SearchResultsLabel.Caption := Format('Focused on #%.0n of %.0n matches found', [FSearchResultsFocusedIndex*1.0, FNumSearchHits*1.0]);
  end
  else
  begin
    SearchResultsLabel.Caption := EmptyStr;
    PrevQuery := EmptyStr;
  end;
end;

function TCommonlyUsedToolsFrame.GetSelectedStatsDisplayType: String;
begin
  if BootstrapsRadioBtn.Checked then
    Result := STATS_DISPLAY_FREQUENCY
  else if SiteCoverageRadioBtn.Checked then
    Result := STATS_DISPLAY_SITE_COVERAGE
  else if NodeIdsRadioBtn.Checked then
    Result := STATS_DISPLAY_NODE_IDS
  else
    Result := STATS_DISPLAY_NOTHING;
end;

procedure TCommonlyUsedToolsFrame.SetSelectedStatsDisplayType(t: String);
begin
  if t.Equals(STATS_DISPLAY_FREQUENCY) or t.Equals(STATS_DISPLAY_RANGE) then
    BootstrapsRadioBtn.Checked := True
  else if t.Equals(STATS_DISPLAY_SITE_COVERAGE) then
    SiteCoverageRadioBtn.Checked := True
  else if t.Equals(STATS_DISPLAY_NODE_IDS) then
    NodeIdsRadioBtn.Checked := True
  else
    NoneRadioButton.Checked := True;
end;

function TCommonlyUsedToolsFrame.IsMyRadioButton(rb: TRadioButton): Boolean;
begin
  Result := ((rb = BootstrapsRadioBtn) or (rb = NodeIdsRadioBtn) or (rb = SiteCoverageRadioBtn) or (rb = NoneRadioButton));
end;

end.

