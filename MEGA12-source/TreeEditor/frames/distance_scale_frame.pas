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

unit distance_scale_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, ComCtrls,
  frame_utils;

type

  { TDistanceScaleFrame }

  TDistanceScaleFrame = class(TFrame, ITreeToolbarFrame)
    CaptionEdit: TEdit;
    DistanceScaleCheckBx: TCheckBox;
    DistScaleFontBtn: TButton;
    DistScaleFontSizeSpinEdit: TSpinEdit;
    DistScaleLengthSpinEdit: TFloatSpinEdit;
    DistScaleTickIntervalSpinEdit: TSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    DistScalePanel: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LineWidthComboBx: TComboBox;
    TimeMajorTickSpinEdit: TFloatSpinEdit;
    TimescaleCaptionEdit: TEdit;
    TimescaleCheckBx: TCheckBox;
    TimescaleFontBtn: TButton;
    TimescaleFontSizeSpinEdit: TSpinEdit;
    TimeScaleLineWidthComboBox: TComboBox;
    TimescaleMinorTickSpinEdit: TFloatSpinEdit;
    TimescalePanel: TPanel;
    Label6: TLabel;
    Image1: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    FWasAutoCollapsed: Boolean;
    FPanel2Height: Integer;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;

    procedure OpenDistScaleTab;
    procedure OpenTimeScaleTab;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
  end;

implementation

{$R *.lfm}

{ TDistanceScaleFrame }

procedure TDistanceScaleFrame.SetEnabled(Value: Boolean);
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

function TDistanceScaleFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TDistanceScaleFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TDistanceScaleFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TDistanceScaleFrame.OpenDistScaleTab;
begin
  TimescalePanel.Visible := False;
  DistScalePanel.Visible := True;
  DistScalePanel.Align := alClient;
end;

procedure TDistanceScaleFrame.OpenTimeScaleTab;
begin
  DistScalePanel.Visible := False;
  TimeScalePanel.Visible := True;
  TimeScalePanel.Align := alClient;
end;

procedure TDistanceScaleFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TDistanceScaleFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
  FWasAutoCollapsed := False;
end;

procedure TDistanceScaleFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

end.

