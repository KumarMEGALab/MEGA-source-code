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

unit ancestors_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin, frame_utils,
  MegaConsts;

const
  MOST_PROBABLE_INDEX = 0;
  CURRENT_SITE_INDEX = 1;
  ALL_SITE_INDEX = 2;
  CHANGES_LIST_INDEX = 3;
  DETAILED_EXPORT_INDEX = 4;


type

  { TAncestorsFrame }

  TAncestorsFrame = class(TFrame, ITreeToolbarFrame)
    AncExtendedCharsCheckBx: TCheckBox;
    AncExportComboBx: TComboBox;
    AncFontBtn: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    AncShowAllRadioBtn: TRadioButton;
    AncMostProbableRadioBtn: TRadioButton;
    AncHideAmbiguousRadioBtn: TRadioButton;
    AncShowNoneRadioBtn: TRadioButton;
    AncExportSpeedBtn: TSpeedButton;
    AncSiteNumSpinEdit: TSpinEdit;
    AncStateFontSizeSpinEdit: TSpinEdit;
    PrevChangeBtn: TSpeedButton;
    NextChangeBtn: TSpeedButton;
  private
    FPanel2Height: Integer;
  public
    procedure SetRadioButtonsEnabled(aValue: Boolean; isLikelihood: Boolean);
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
    function AncStatesDisplaySelection: Integer;
    procedure UpdateExportOptions(isParsimony: Boolean);
    function GetSelectedExportOption: TAncestralStatesExportType;
  end;

implementation

{$R *.lfm}

{ TAncestorsFrame }

procedure TAncestorsFrame.SetRadioButtonsEnabled(aValue: Boolean; isLikelihood: Boolean);
begin
  AncShowAllRadioBtn.Enabled := aValue;
  AncMostProbableRadioBtn.Enabled := (aValue and isLikelihood);
  AncExtendedCharsCheckBx.Visible := (aValue and (not isLikelihood));
  AncHideAmbiguousRadioBtn.Enabled := aValue;
  AncShowNoneRadioBtn.Enabled := aValue;
end;

function TAncestorsFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TAncestorsFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TAncestorsFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TAncestorsFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TAncestorsFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TAncestorsFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

function TAncestorsFrame.AncStatesDisplaySelection: Integer;
begin
  if AncShowAllRadioBtn.Checked then
    Result := 0
  else if AncMostProbableRadioBtn.Checked then
    Result := 1
  else if AncHideAmbiguousRadioBtn.Checked then
    Result := 2
  else if AncShowNoneRadioBtn.Checked then
    Result := 3
  else
    Result := -1;
end;

procedure TAncestorsFrame.UpdateExportOptions(isParsimony: Boolean);
begin
  AncExportComboBx.Items.Clear;
  if not IsParsimony then
    AncExportComboBx.Items.Add(MOST_PROBABLE);
  AncExportComboBx.Items.Add(CURRENT_SITE_ANCESTORS);
  if isParsimony then
    AncExportComboBx.Items.Add(ALL_SITE_ANCESTORS);
  AncExportComboBx.Items.Add(CHANGES_LIST);
  AncExportComboBx.Items.Add(DETAILED_TEXT_EXPORT);
  AncExportComboBx.ItemIndex := 0;
end;

function TAncestorsFrame.GetSelectedExportOption: TAncestralStatesExportType;
var
  aSelection: String;
begin
  aSelection := AncExportComboBx.Text;
  if aSelection = MOST_PROBABLE then
    Result := asExportMostProbable
  else if aSelection = CURRENT_SITE_ANCESTORS then
    Result := asExportCurrentSite
  else if aSelection = ALL_SITE_ANCESTORS then
    Result := asExportAllSites
  else if aSelection = CHANGES_LIST then
    Result := asExportChangesList
  else if aSelection = DETAILED_TEXT_EXPORT then
    Result := asExportDetails
  else
    raise Exception.Create('Application Error: missing ancestral states export type');
end;

end.

