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

unit MNewickExportDlg;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, StdCtrls,
  MNewickExportOptions, ExtCtrls, ComCtrls, mshortcutshelper;

type

  { TNewickExportOptionsForm }

  TNewickExportOptionsForm = class(TForm)
    ActionList1: TActionList;
    BootstrapValuesCheckbox: TCheckBox;
    BranchLengthsCheckbox: TCheckBox;
    BootstrapStdDevCheckbox: TCheckBox;
    QuotedNodeLabelsCheckbox: TCheckBox;
    GeneDuplicationsCheckbox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NodeLabelsCheckbox: TCheckBox;
    Panel1: TPanel;
    GeneralOptionsHeader: TPanel;
    TimesPanel: TPanel;
    TimetreeOptionsHeader: TPanel;
    Panel5: TPanel;
    GeneTreeOptionsHeader: TPanel;
    Panel7: TPanel;
    RelativeTimesBtn: TRadioButton;
    AbsoluteTimesBtn: TRadioButton;
    SaveAction: TAction;
    CancelAction: TAction;
    SpeciationsCheckbox: TCheckBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NodeLabelsCheckboxChange(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
  private
    FExportOptions: TNewickExportOptions;
    function GetExportOptions: TNewickExportOptions;
    function DivergenceTimesSelected: Boolean;
    function ReltimesSelected: Boolean;
    { Private declarations }
  public
    procedure SelectEnabledCheckboxes(Options: TNewickExportOptions);
    property ExportOptions: TNewickExportOptions read GetExportOptions;
    { Public declarations }
  end;

var
  NewickExportOptionsForm: TNewickExportOptionsForm;

implementation

{$R *.lfm}

uses
  mimageform;

procedure TNewickExportOptionsForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TNewickExportOptionsForm.DivergenceTimesSelected: Boolean;
begin
  Result := (AbsoluteTimesBtn.Enabled and AbsoluteTimesBtn.Checked);
end;

function TNewickExportOptionsForm.GetExportOptions: TNewickExportOptions;
begin
  FExportOptions.BranchLengths := BranchLengthsCheckbox.Checked;
  FExportOptions.BootstrapVals := BootstrapValuesCheckbox.Checked;
  FExportOptions.BootstrapStdDev := BootstrapStdDevCheckbox.Checked;
  FExportOptions.NodeLabels := NodeLabelsCheckbox.Checked;
  FExportOptions.GeneDuplications := GeneDuplicationsCheckbox.Checked;
  FExportOptions.SpeciationEvents := SpeciationsCheckbox.Checked;
  FExportOptions.DivergenceTimes := DivergenceTimesSelected;
  FExportOptions.Reltimes := ReltimesSelected;
  FExportOptions.UseQuotesForLabels := QuotedNodeLabelsCheckbox.Checked;
  Result := FExportOptions;
end;

function TNewickExportOptionsForm.ReltimesSelected: Boolean;
begin
  Result := (RelativeTimesBtn.Enabled and RelativeTimesBtn.Checked);
end;

procedure TNewickExportOptionsForm.SaveActionExecute(Sender: TObject);
begin
  ModalResult := mrYes;
end;

procedure TNewickExportOptionsForm.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  GeneralOptionsHeader.Color := $00b1a216;
  TimetreeOptionsHeader.Color := $00b1a216;
  GeneTreeOptionsHeader.Color := $00b1a216;
end;

procedure TNewickExportOptionsForm.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TNewickExportOptionsForm.NodeLabelsCheckboxChange(Sender: TObject);
begin
  QuotedNodeLabelsCheckbox.Enabled := NodeLabelsCheckbox.Checked;
end;

procedure TNewickExportOptionsForm.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TNewickExportOptionsForm.SelectEnabledCheckboxes(Options: TNewickExportOptions);
begin
  BranchLengthsCheckbox.Checked := Options.BranchLengths;
  BranchLengthsCheckbox.Enabled := BranchLengthsCheckbox.Checked;
  BootstrapValuesCheckbox.Checked  := Options.BootstrapVals;
  BootstrapValuesCheckbox.Enabled := BootstrapValuesCheckbox.Checked;
  BootstrapStdDevCheckbox.Checked := Options.BootstrapStdDev;
  BootstrapStdDevCheckbox.Enabled := BootstrapStdDevCheckbox.Checked;
  NodeLabelsCheckbox.Checked := Options.NodeLabels;
  NodeLabelsCheckbox.Enabled := NodeLabelsCheckbox.Checked;
  QuotedNodeLabelsCheckbox.Enabled := NodeLabelsCheckbox.Enabled;
  QuotedNodeLabelsCheckbox.Checked := Options.UseQuotesForLabels;
  GeneDuplicationsCheckbox.Checked := Options.GeneDuplications;
  GeneDuplicationsCheckbox.Enabled := GeneDuplicationsCheckbox.Checked;
  SpeciationsCheckbox.Checked := Options.SpeciationEvents;
  SpeciationsCheckbox.Enabled := SpeciationsCheckbox.Checked;
  RelativeTimesBtn.Enabled := Options.Reltimes;
  AbsoluteTimesBtn.Enabled := Options.Reltimes and Options.DivergenceTimes;
  if Options.Reltimes then
  begin
    if Options.DivergenceTimes then
      AbsoluteTimesBtn.Checked := True
    else
      RelativeTimesBtn.Checked := True;
  end
  else
    TimetreeOptionsHeader.Color := clSilver;
  if (not BranchLengthsCheckbox.Enabled) and (not BootstrapValuesCheckbox.Enabled) and (not NodeLabelsCheckbox.Enabled) then
    GeneralOptionsHeader.Color := clSilver;
  if (not GeneDuplicationsCheckbox.Enabled) and (not SpeciationsCheckbox.Enabled) then
    GeneTreeOptionsHeader.Color := clSilver;
end;

end.
