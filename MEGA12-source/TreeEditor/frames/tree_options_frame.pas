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

unit tree_options_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Spin, frame_utils, Types,
  MTreeBox;

type

  { TTreeOptionsFrame }

  TTreeOptionsFrame = class(TFrame, ITreeToolbarFrame)
    RadiationHorizNamesChBox: TCheckBox;
    HorizNamesChBox: TCheckBox;
    CircularPage: TTabSheet;
    Image1: TImage;
    RectTreeImage: TImage;
    CircleTreeImage: TImage;
    RadiationTreeImage: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    BlensFactorLabel: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    RadialPage: TTabSheet;
    RectangularPage: TTabSheet;
    RadiationBranchLengthSpinEdit: TSpinEdit;
    RadiationStartAngleSpinEdit: TSpinEdit;
    StartAngleSpinEdit: TSpinEdit;
    RadiusSpinEdit: TSpinEdit;
    CenterHoleSpinEdit: TSpinEdit;
    TaxonSeparationSpinEdit: TSpinEdit;
    TaxonSeparationTrackBar: TTrackBar;
    TreeWidthTrackBar: TTrackBar;
    TreeWidthSpinEdit: TSpinEdit;
    procedure Panel1Click(Sender: TObject);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure TaxonSeparationSpinEditChange(Sender: TObject);
    procedure TreeWidthSpinEditChange(Sender: TObject);
  private
    FWasAutoCollapsed: Boolean;
    FPanel2Height: Integer;
  public
    DisableEventNotifications: Boolean;
    procedure SetStretchImages(aValue: Boolean);
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
    procedure SetTaxonSeparation(aValue: Integer);
    procedure SetTreeWidth(aValue: Integer); overload;
    procedure SetTreeWidth(aTree: TTreeBox); overload;
    procedure SetBranchLength(aValue: Integer; aMaxValue: Integer; aCaption: String);
    procedure SetHorizTaxaNames(aValue: Boolean);
    procedure SetStartAngle(aValue: Integer);
    procedure SetCenterMargin(aValue: Integer);
    procedure SetRadius(aValue: Integer);
  end;

implementation

{$R *.lfm}

uses
  MegaConsts, Graphics;

{ TTreeOptionsFrame }

procedure TTreeOptionsFrame.Panel1MouseEnter(Sender: TObject);
begin
  Panel1.Color := HOVER_PANEL_COLOR;
  Panel1.Font.Color := HOVER_PANEL_FONT;
  Invalidate;
end;

procedure TTreeOptionsFrame.Panel1Click(Sender: TObject);
begin
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := Panel1.Height + Panel2.Height
  else
    Height := Panel1.Height;
end;

procedure TTreeOptionsFrame.Panel1MouseLeave(Sender: TObject);
begin
  Panel1.Color := clDefault;
  Invalidate;
end;

procedure TTreeOptionsFrame.TaxonSeparationSpinEditChange(Sender: TObject);
begin
  if DisableEventNotifications then Exit;
  TaxonSeparationTrackBar.Position := TaxonSeparationSpinEdit.Value;
end;

procedure TTreeOptionsFrame.TreeWidthSpinEditChange(Sender: TObject);
begin
  if DisableEventNotifications then Exit;
  TreeWidthTrackBar.Position := TreeWidthSpinEdit.Value
end;

procedure TTreeOptionsFrame.SetStretchImages(aValue: Boolean);
begin
  RectTreeImage.Stretch := aValue;
  RectTreeImage.Proportional := aValue;
  CircleTreeImage.Stretch := aValue;
  CircleTreeImage.Proportional := aValue;
  RadiationTreeImage.Stretch := aValue;
  RadiationTreeImage.Proportional := aValue;
end;

function TTreeOptionsFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TTreeOptionsFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TTreeOptionsFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TTreeOptionsFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TTreeOptionsFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
  FWasAutoCollapsed := False;
end;

procedure TTreeOptionsFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TTreeOptionsFrame.SetTaxonSeparation(aValue: Integer);
begin
  try
    DisableEventNotifications := True;
    if TaxonSeparationSpinEdit.Value <> aValue then
      TaxonSeparationSpinEdit.Value := aValue;
    if TaxonSeparationTrackBar.Position <> aValue then
      TaxonSeparationTrackBar.Position := aValue;
  finally
    DisableEventNotifications := False;
  end;
end;

procedure TTreeOptionsFrame.SetTreeWidth(aValue: Integer);
begin
  try
    DisableEventNotifications := True;
    if TreeWidthSpinEdit.Value <> aValue then
      TreeWidthSpinEdit.Value := aValue;
    if TreeWidthTrackBar.Position <> aValue then
      TreeWidthTrackBar.Position := aValue;
  finally
    DisableEventNotifications := False;
  end;
end;

procedure TTreeOptionsFrame.SetTreeWidth(aTree: TTreeBox);
var
  w: Integer = -1;
begin
  if not Assigned(aTree) then
    Exit;

  try
    DisableEventNotifications := True;
    w := aTree.TreeWidth;
    if TreeWidthSpinEdit.Value <> w then
      TreeWidthSpinEdit.Value := w;
    if TreeWidthTrackBar.Position <> w then
      TreeWidthTrackBar.Position := w;
  finally
    DisableEventNotifications := False;
  end;
end;

procedure TTreeOptionsFrame.SetBranchLength(aValue: Integer; aMaxValue: Integer; aCaption: String);
begin
  try
    DisableEventNotifications := True;

    BlensFactorLabel.Caption := aCaption;
    RadiationBranchLengthSpinEdit.MaxValue := aMaxValue;

    if RadiationBranchLengthSpinEdit.Value <> aValue then
      RadiationBranchLengthSpinEdit.Value := aValue;
  finally
    DisableEventNotifications := False;
  end;
end;

procedure TTreeOptionsFrame.SetHorizTaxaNames(aValue: Boolean);
begin
  try
    DisableEventNotifications := True;
    HorizNamesChBox.Checked := aValue;
    RadiationHorizNamesChBox.Checked := aValue;
  finally
    DisableEventNotifications := False;
  end;
end;

procedure TTreeOptionsFrame.SetStartAngle(aValue: Integer);
begin
  try
    DisableEventNotifications := True;
    StartAngleSpinEdit.Value := aValue;
    RadiationStartAngleSpinEdit.Value := aValue;
  finally
    DisableEventNotifications := False
  end;
end;

procedure TTreeOptionsFrame.SetCenterMargin(aValue: Integer);
begin
  try
    DisableEventNotifications := True;
    CenterHoleSpinEdit.Value := aValue;
  finally
    DisableEventNotifications := False;
  end;
end;

procedure TTreeOptionsFrame.SetRadius(aValue: Integer);
begin
  try
    DisableEventNotifications := True;
    RadiusSpinEdit.Value := aValue;
  finally
    DisableEventNotifications := False;
  end;
end;

end.

