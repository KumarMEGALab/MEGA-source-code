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

unit MNewickExportDlg;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, StdCtrls,
  MNewickExportOptions, ExtCtrls, mshortcutshelper;

type

  { TNewickExportOptionsForm }

  TNewickExportOptionsForm = class(TForm)
    Button2: TButton;
    ActionList1: TActionList;
    SaveAction: TAction;
    CancelButton: TButton;
    CancelAction: TAction;
    TimetreeGroupBox: TRadioGroup;
    GroupBox1: TGroupBox;
    BranchLengthsCheckbox: TCheckBox;
    BootstrapValuesCheckbox: TCheckBox;
    NodeLabelsCheckbox: TCheckBox;
    GroupBox2: TGroupBox;
    GeneDuplicationsCheckbox: TCheckBox;
    SpeciationsCheckbox: TCheckBox;
    procedure FormCreate(Sender: TObject);
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


procedure TNewickExportOptionsForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TNewickExportOptionsForm.DivergenceTimesSelected: Boolean;
begin
  Result := (TimeTreeGroupBox.Enabled and (TimetreeGroupBox.ItemIndex = 1));
end;

function TNewickExportOptionsForm.GetExportOptions: TNewickExportOptions;
begin
  FExportOptions.BranchLengths := BranchLengthsCheckbox.Checked;
  FExportOptions.BootstrapVals := BootstrapValuesCheckbox.Checked;
  FExportOptions.NodeLabels := NodeLabelsCheckbox.Checked;
  FExportOptions.GeneDuplications := GeneDuplicationsCheckbox.Checked;
  FExportOptions.SpeciationEvents := SpeciationsCheckbox.Checked;
  FExportOptions.DivergenceTimes := DivergenceTimesSelected;
  FExportOptions.Reltimes := ReltimesSelected;
  Result := FExportOptions;
end;

function TNewickExportOptionsForm.ReltimesSelected: Boolean;
begin
  Result := (TimeTreeGroupBox.Enabled and (TimetreeGroupBox.ItemIndex = 0));
end;

procedure TNewickExportOptionsForm.SaveActionExecute(Sender: TObject);
begin
  ModalResult := mrYes;
end;

procedure TNewickExportOptionsForm.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
end;

procedure TNewickExportOptionsForm.SelectEnabledCheckboxes(Options: TNewickExportOptions);
begin
  BranchLengthsCheckbox.Checked := Options.BranchLengths;
  BranchLengthsCheckbox.Enabled := BranchLengthsCheckbox.Checked;
  BootstrapValuesCheckbox.Checked  := Options.BootstrapVals;
  BootstrapValuesCheckbox.Enabled := BootstrapValuesCheckbox.Checked;
  NodeLabelsCheckbox.Checked := Options.NodeLabels;
  NodeLabelsCheckbox.Enabled := NodeLabelsCheckbox.Checked;
  GeneDuplicationsCheckbox.Checked := Options.GeneDuplications;
  GeneDuplicationsCheckbox.Enabled := GeneDuplicationsCheckbox.Checked;
  SpeciationsCheckbox.Checked := Options.SpeciationEvents;
  SpeciationsCheckbox.Enabled := SpeciationsCheckbox.Checked;
  TimetreeGroupBox.Enabled := Options.Reltimes;
  if TimetreeGroupBox.Enabled then
  begin
    if Options.DivergenceTimes then
      TimetreeGroupBox.ItemIndex := 1
    else
    begin
      TimetreeGroupBox.ItemIndex := 0;
      TRadioButton(TimetreeGroupBox.Controls[1]).Enabled := False;
    end;
  end;
end;

end.
