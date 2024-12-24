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

unit node_stats_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, Dialogs,
  frame_utils;

type

  { TNodeStatsFrame }

  TNodeStatsFrame = class(TFrame, ITreeToolbarFrame)
    AutoNameNodesBtn: TButton;
    ClearNodeLabelsBtn: TButton;
    FontDialog1: TFontDialog;
    Label7: TLabel;
    LabelFontBtn: TButton;
    DisplayNodeLabelsChBox: TCheckBox;
    ShowStatsCheckBx: TCheckBox;
    StatTypeComboBox: TComboBox;
    StatsFontBtn: TButton;
    StatsPlacementComboBox: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StatsFontSizeSpinEdit: TSpinEdit;
    StatsHorizDistanceSpinEdit: TSpinEdit;
    StatsVertDistanceSpinEdit: TSpinEdit;
    procedure LabelFontBtnClick(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1MouseEnter(Sender: TObject);
  private
    FWasAutoCollapsed: Boolean;
    FPanel2Height: Integer;
    function GetStatsDisplayTypes: TStringList;
    procedure SetStatsDisplayTypes(AValue: TStringList);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    NodeFontChangedNotify: TNotifyEvent;
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
    function HasStatType(aStatType: String): Boolean;
    procedure AddStatDisplayType(aType: String);
    procedure RemoveStatsDisplayType(aType: String);
    procedure SetCurrentStatDisplayType(aType: String);
    function GetCurrentStatDisplayType: String;
    procedure SetDisplayNodeNames(aVal: Boolean);
    property  StatsDisplayTypes: TStringList read GetStatsDisplayTypes write SetStatsDisplayTypes;
  end;

implementation

uses
  Graphics, MegaConsts;

{$R *.lfm}

{ TNodeStatsFrame }

procedure TNodeStatsFrame.Panel1MouseEnter(Sender: TObject);
begin
  Panel1.Color := HOVER_PANEL_COLOR;
  Panel1.Font.Color := HOVER_PANEL_FONT;
end;

function TNodeStatsFrame.GetStatsDisplayTypes: TStringList;
var
  i: Integer = -1;
begin
  Result := TStringList.Create;
  if StatTypeComboBox.Items.Count > 0 then
    for i := 0 to StatTypeComboBox.Items.Count - 1 do
      Result.Add(StatTypeComboBox.Items[i]);
end;

procedure TNodeStatsFrame.SetStatsDisplayTypes(AValue: TStringList);
var
  i: Integer = -1;
begin
  StatTypeComboBox.Items.Clear;
  if AValue.Count > 0 then
  for i := 0 to AValue.Count - 1 do
    StatTypeComboBox.Items.Add(AValue[i]);
end;

procedure TNodeStatsFrame.SetEnabled(Value: Boolean);
begin
  if (not Value) and Panel2.Visible then
  begin
    ToggleCollapseBodyPanel;
    FWasAutoCollapsed := True;
  end
  else if Value and (not Panel2.Visible) and FWasAutoCollapsed then
  begin
    ToggleCollapseBodyPanel;
    FWasAutoCollapsed := False;
  end;

  inherited SetEnabled(Value);
end;

function TNodeStatsFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TNodeStatsFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TNodeStatsFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TNodeStatsFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TNodeStatsFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
  FWasAutoCollapsed := False;
end;

procedure TNodeStatsFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

function TNodeStatsFrame.HasStatType(aStatType: String): Boolean;
begin
  Result := (StatTypeComboBox.Items.IndexOf(aStatType) >= 0);
end;

procedure TNodeStatsFrame.AddStatDisplayType(aType: String);
begin
  if StatTypeComboBox.Items.IndexOf(aType) < 0 then
    StatTypeComboBox.Items.Add(aType);
end;

procedure TNodeStatsFrame.RemoveStatsDisplayType(aType: String);
var
  index: Integer = -1;
  currentType: String = '';
begin
  index := StatTypeComboBox.Items.IndexOf(aType);
  if index >= 0 then
  begin
    if StatTypeComboBox.ItemIndex >= 0 then
      currentType := StatTypeComboBox.Items[StatTypeComboBox.ItemIndex];
    StatTypeComboBox.Items.Delete(index);
    if StatTypeComboBox.Items.Count > 0 then
    begin
      if currentType = aType then
        StatTypeComboBox.ItemIndex := 0
      else
        StatTypeComboBox.ItemIndex := StatTypeComboBox.Items.IndexOf(currentType);
    end;
  end;
end;

procedure TNodeStatsFrame.SetCurrentStatDisplayType(aType: String);
var
  index: Integer = -1;
begin
  index := StatTypeComboBox.Items.IndexOf(aType);
  if index >= 0 then
  begin
    if StatTypeComboBox.ItemIndex <> index then
      StatTypeComboBox.ItemIndex := index;
  end;
  if aType = STATS_DISPLAY_NOTHING then
    ShowStatsCheckBx.Checked := False
  else if (aType = STATS_DISPLAY_SITE_COVERAGE) or (aType = STATS_DISPLAY_NODE_IDS) then
    ShowStatsCheckBx.Checked := True;
end;

function TNodeStatsFrame.GetCurrentStatDisplayType: String;
begin
  Result := EmptyStr;
  if (StatTypeComboBox.Items.Count > 0) and (StatTypeComboBox.ItemIndex >= 0) then
    Result := StatTypeComboBox.Items[StatTypeComboBox.ItemIndex];
end;

procedure TNodeStatsFrame.SetDisplayNodeNames(aVal: Boolean);
begin
  if DisplayNodeLabelsChBox.Checked <> aVal then
    DisplayNodeLabelsChBox.Checked := aVal;
end;

procedure TNodeStatsFrame.Panel1Exit(Sender: TObject);
begin
  Panel1.Color := clDefault;
  Panel1.Font.Color := clBlack;
end;

procedure TNodeStatsFrame.LabelFontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute and Assigned(NodeFontChangedNotify) then
    NodeFontChangedNotify(Sender);
end;

end.

