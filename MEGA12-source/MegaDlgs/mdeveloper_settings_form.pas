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

unit mdeveloper_settings_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ComCtrls, ActnList;

type

  { TDeveloperSettingsDlg }

  TDeveloperSettingsDlg = class(TForm)
    IQTreeComboBox: TComboBox;
    EnableSubsamplingComboBx: TComboBox;
    Label2: TLabel;
    Label7: TLabel;
    UseMeanBclCBox: TCheckBox;
    DisplayLogsComboBx: TComboBox;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    MyEslComboBx: TComboBox;
    KeepOnTopChbox: TCheckBox;
    CloseAction: TAction;
    ActionList1: TActionList;
    IsDeveloperComboBx: TComboBox;
    GlenDevModeComboBx: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure CloseActionExecute(Sender: TObject);
    procedure EnableSubsamplingComboBxChange(Sender: TObject);
    procedure DisplayLogsComboBxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GlenDevModeComboBxChange(Sender: TObject);
    procedure IQTreeComboBoxChange(Sender: TObject);
    procedure IsDeveloperComboBxChange(Sender: TObject);
    procedure KeepOnTopChboxChange(Sender: TObject);
    procedure MyEslComboBxChange(Sender: TObject);
    procedure UseMeanBclCBoxChange(Sender: TObject);
  private
    FIgnoreEvents: Boolean;
  public

  end;

var
  DeveloperSettingsDlg: TDeveloperSettingsDlg;

implementation

{$R *.lfm}

uses
  MegaConsts, mimageform, mega_main;

{ TDeveloperSettingsDlg }

procedure TDeveloperSettingsDlg.FormActivate(Sender: TObject);
begin
  try
    FIgnoreEvents := True;
    if IsDeveloper then
      IsDeveloperComboBx.ItemIndex := 1
    else
      IsDeveloperComboBx.ItemIndex := 0;
    if ShowLogFiles then
      DisplayLogsComboBx.ItemIndex := 1
    else
      DisplayLogsComboBx.ItemIndex := 0;
    ToolBar1.Images := ImageForm.GetDialogButtonImageList;
    ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
    ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
    if ClientWidth > ToolBar1.Width then
      ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
    UseMeanBclCBox.Checked := UseLbsMeanForCutoffs;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TDeveloperSettingsDlg.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TDeveloperSettingsDlg.GlenDevModeComboBxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
end;

procedure TDeveloperSettingsDlg.IQTreeComboBoxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
end;

procedure TDeveloperSettingsDlg.IsDeveloperComboBxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
  if IsDeveloperComboBx.ItemIndex = 1 then
    IsDeveloper := True
  else
    IsDeveloper := False;
  MegaForm.DeveloperLabel.Visible := IsDeveloper;
  MegaForm.SetIsDeveloperBtn.Visible := IsDeveloper;
  MegaForm.Invalidate;
end;

procedure TDeveloperSettingsDlg.KeepOnTopChboxChange(Sender: TObject);
begin
  if KeepOnTopChbox.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TDeveloperSettingsDlg.MyEslComboBxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
  if MyEslComboBx.ItemIndex = MyEslComboBx.Items.IndexOf('True') then
  begin
    MegaForm.MyEslAction.Enabled := True;
    MegaForm.MyEslAction.Visible := True;
    DrPhyloEnabled := True;
  end
  else
  begin
    MegaForm.MyEslAction.Enabled := False;
    MegaForm.MyEslAction.Visible := False;
    DrPhyloEnabled := False;
  end;
end;

procedure TDeveloperSettingsDlg.UseMeanBclCBoxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
  UseLbsMeanForCutoffs := UseMeanBclCBox.Checked;
end;

procedure TDeveloperSettingsDlg.CloseActionExecute(Sender: TObject);
begin
  Hide;
end;

procedure TDeveloperSettingsDlg.EnableSubsamplingComboBxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
  if EnableSubsamplingComboBx.ItemIndex = EnableSubsamplingComboBx.Items.IndexOf('True') then
    EnableSubsamplingAnalyses := True
  else
    EnableSubsamplingAnalyses := False;
end;

procedure TDeveloperSettingsDlg.DisplayLogsComboBxChange(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
  if DisplayLogsComboBx.ItemIndex = DisplayLogsComboBx.Items.IndexOf('True') then
    ShowLogFiles := True
  else
    ShowLogFiles := False;
end;

end.

