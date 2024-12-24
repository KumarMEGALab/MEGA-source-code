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

unit ancestors_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin, frame_utils,
  MegaConsts, MLongintList;

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
    AncSiteNumSpinEdit: TSpinEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NextChangeBtn: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    AncShowAllRadioBtn: TRadioButton;
    AncMostProbableRadioBtn: TRadioButton;
    AncHideAmbiguousRadioBtn: TRadioButton;
    AncShowNoneRadioBtn: TRadioButton;
    AncExportSpeedBtn: TSpeedButton;
    AncStateFontSizeSpinEdit: TSpinEdit;
    PrevChangeBtn: TSpeedButton;
    procedure AncSiteNumSpinEditClick(Sender: TObject);
    procedure AncSiteNumSpinEditEditingDone(Sender: TObject);
    procedure AncSiteNumSpinEditEnter(Sender: TObject);
    procedure AncSiteNumSpinEditExit(Sender: TObject);
    procedure AncSiteNumSpinEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PanelOnClick(Sender: TObject);
  private
    FCurrentSiteIndex: Integer;
    FIncludedSites: TLongIntList;
    FLockedForSiteNumberEditing: Boolean;
    FWasAutoCollapsed: Boolean;
    FPanel2Height: Integer;
    function PointInSpinEditBounds(p: TPoint): Boolean;
    function PointInPrevButton(p: TPoint): Boolean;
    function PointInNextButton(p: TPoint): Boolean;
    procedure SetLockedForSiteNumberEditing(AValue: Boolean);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure SetIncludedSites(includedSites: TIntArray);
    procedure SetSiteByIndex(aIndex: Integer);
    function AutoAdjustSiteIndex(requestedValue: Integer): Integer;
    property LockedForSiteNumberEditing: Boolean read FLockedForSiteNumberEditing write SetLockedForSiteNumberEditing;
    property CurrentSiteIndex: Integer read FCurrentSiteIndex;
  end;

implementation

uses
  mdeveloper_console,
  mimageform, MegaUtils;

{$R *.lfm}

{ TAncestorsFrame }

procedure TAncestorsFrame.PanelOnClick(Sender: TObject);
var
  clickPos: TPoint;
begin
  clickPos := Panel1.ScreenToClient(Mouse.CursorPos);
  if AncShowNoneRadioBtn.Checked then
  begin
    if PointInSpinEditBounds(clickPos) or PointInPrevButton(clickPos) or PointInNextButton(clickPos) then
    begin
      if not Panel2.Visible then
        ToggleCollapseBodyPanel;
      AncShowAllRadioBtn.Checked := True;
      Exit;
    end;
  end;

  if not (PointInSpinEditBounds(clickPos) or PointInPrevButton(clickPos) or PointInNextButton(clickPos)) then
    ToggleCollapseBodyPanel;
  if GetBodyPanel.Visible then
    ImageForm.GetSmallArrowIcons.GetBitmap(EXPANDED_ARROW_INDEX, GetExpandCollapseImage.Picture.Bitmap)
  else
    ImageForm.GetSmallArrowIcons.GetBitmap(COLLAPSED_ARROW_INDEX, GetExpandCollapseImage.Picture.Bitmap);
  Invalidate;
  if Assigned(Parent) then
    Parent.Invalidate;
end;

procedure TAncestorsFrame.AncSiteNumSpinEditEnter(Sender: TObject);
begin
  WriteToDevConsole('spin edit enter');
  //FLockedForSiteNumberEditing := True;
end;

procedure TAncestorsFrame.AncSiteNumSpinEditEditingDone(Sender: TObject);
begin
  WriteToDevConsole(Format('spin edit editingDone - site %d', [AncSiteNumSpinEdit.Value]));
  if Assigned(AncSiteNumSpinEdit.OnChange )then
    AncSiteNumSpinEdit.OnChange(Sender);
end;

procedure TAncestorsFrame.AncSiteNumSpinEditClick(Sender: TObject);
begin
  WriteToDevConsole('spin edit click');
end;

procedure TAncestorsFrame.AncSiteNumSpinEditExit(Sender: TObject);
begin
  WriteToDevConsole('spin edit exit');
  //FLockedForSiteNumberEditing := False;
end;

procedure TAncestorsFrame.AncSiteNumSpinEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  WriteToDevConsole('spin edit key down');
end;

function TAncestorsFrame.PointInSpinEditBounds(p: TPoint): Boolean;
var
  r: TRect;
begin
  r := AncSiteNumSpinEdit.BoundsRect;
  Result := PointInRect(p, r);
end;

function TAncestorsFrame.PointInPrevButton(p: TPoint): Boolean;
var
  r: TRect;
begin
  r := PrevChangeBtn.BoundsRect;
  Result := PointInRect(p, r);
end;

function TAncestorsFrame.PointInNextButton(p: TPoint): Boolean;
var
  r: TRect;
begin
  r := NextChangeBtn.BoundsRect;
  Result := PointInRect(p, r);
end;

procedure TAncestorsFrame.SetLockedForSiteNumberEditing(AValue: Boolean);
begin
  if FLockedForSiteNumberEditing = AValue then Exit;
  FLockedForSiteNumberEditing := AValue;
end;

procedure TAncestorsFrame.SetEnabled(Value: Boolean);
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

constructor TAncestorsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIncludedSites := TLongIntList.Create;
  FLockedForSiteNumberEditing := False;
  FCurrentSiteIndex := 0;
end;

destructor TAncestorsFrame.Destroy;
begin
  if Assigned(FIncludedSites) then
    FIncludedSites.Free;
  inherited Destroy;
end;

procedure TAncestorsFrame.SetRadioButtonsEnabled(aValue: Boolean; isLikelihood: Boolean);
begin
  AncShowAllRadioBtn.Enabled := aValue;
  AncMostProbableRadioBtn.Enabled := (aValue and isLikelihood);
  AncExtendedCharsCheckBx.Enabled := (aValue and (not isLikelihood));
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
  FWasAutoCollapsed := False;
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
  if not IsParsimony then
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

procedure TAncestorsFrame.SetIncludedSites(includedSites: TIntArray);
var
  i: Integer = -1;
begin
  if not Assigned(FIncludedSites) then
    FIncludedSites := TLongIntList.Create
  else
    FIncludedSites.Clear;

  if Length(includedSites) > 0 then
  begin
    for i := Low(includedSites) to High(includedSites) do
      FIncludedSites.Add(includedSites[i]);
    AncSiteNumSpinEdit.MinValue := FIncludedSites[0];
    AncSiteNumSpinEdit.MaxValue := FIncludedSites[FIncludedSites.Count - 1];
  end;
end;

procedure TAncestorsFrame.SetSiteByIndex(aIndex: Integer);
begin
  if FIncludedSites.Count = 0 then
    Exit;
  if aIndex >= FIncludedSites.Count then
    raise Exception.Create(Format('invalid site index. Got %d but max is %d', [aIndex, FIncludedSites.Count]));
  if aIndex < 0 then
    raise Exception.Create(Format('invalid site index. Got %d but min is 0', [aIndex]));

  try
    FLockedForSiteNumberEditing := True;
    AncSiteNumSpinEdit.Value := FIncludedSites[aIndex];
    FCurrentSiteIndex := aIndex;
  finally
    FLockedForSiteNumberEditing := False;
  end;
end;

function TAncestorsFrame.AutoAdjustSiteIndex(requestedValue: Integer): Integer;
var
  previousValue: Integer = -1;
  newValue: Integer = -1;
  newIndex: Integer = -1;
begin
  previousValue := FIncludedSites[FCurrentSiteIndex];
  if requestedValue = previousValue then
    Exit(FCurrentSiteIndex);

  newIndex := FIncludedSites.Find(requestedValue);
  if newIndex >= 0 then
  begin
    SetSiteByIndex(newIndex);
    Exit(FCurrentSiteIndex);
  end;

  if requestedValue > previousValue then
  begin
    if FCurrentSiteIndex < (FIncludedSites.Count - 1) then
      SetSiteByIndex(FCurrentSiteIndex + 1);
  end
  else
  begin
    if FCurrentSiteIndex > 0 then
      SetSiteByIndex(FCurrentSiteIndex - 1);
  end;
  Result := FCurrentSiteIndex;
end;

end.

