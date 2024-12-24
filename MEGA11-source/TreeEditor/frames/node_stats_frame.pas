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

unit node_stats_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, frame_utils;

type

  { TNodeStatsFrame }

  TNodeStatsFrame = class(TFrame, ITreeToolbarFrame)
    BootstrapFrequencyBtn: TRadioButton;
    DataCoverageBtn: TRadioButton;
    NodeIdsBtn: TRadioButton;
    StatsFontBtn: TButton;
    ShowStatsCheckBx: TCheckBox;
    StatsHideIfLowerThanCheckBx: TCheckBox;
    StatsPlacementComboBox: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StatsFontSizeSpinEdit: TSpinEdit;
    StatsHorizDistanceSpinEdit: TSpinEdit;
    StatsVertDistanceSpinEdit: TSpinEdit;
    StatsHideIfLowerThanSpinEdit: TSpinEdit;
    procedure RadioBtnChange(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1MouseEnter(Sender: TObject);
  private
    FPanel2Height: Integer;
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
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
end;

procedure TNodeStatsFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TNodeStatsFrame.Panel1Exit(Sender: TObject);
begin
  Panel1.Color := clDefault;
  Panel1.Font.Color := clBlack;
end;

procedure TNodeStatsFrame.RadioBtnChange(Sender: TObject);
begin
  StatsHideIfLowerThanCheckBx.Enabled := BootstrapFrequencyBtn.Checked;
  StatsHideIfLowerThanSpinEdit.Enabled := BootstrapFrequencyBtn.Checked;
end;

end.

