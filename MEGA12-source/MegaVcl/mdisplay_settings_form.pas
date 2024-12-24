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

unit mdisplay_settings_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, ActnList;

type

  { TDisplaySettingsForm }

  TDisplaySettingsForm = class(TForm)
    CloseAction: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ProportionalCbox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ScaledIconsCbox: TCheckBox;
    SpinEdit1: TSpinEdit;
    StretchedIconsCbox: TCheckBox;
    DefaultImagesBtn: TRadioButton;
    GroupBox1: TGroupBox;
    LargeImagesBtn: TRadioButton;
    procedure CloseActionExecute(Sender: TObject);
    procedure DefaultImagesBtnChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LargeImagesBtnChange(Sender: TObject);
    procedure ProportionalCboxChange(Sender: TObject);
    procedure ScaledIconsCboxChange(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure StretchedIconsCboxChange(Sender: TObject);
  private
    FRadioButtonUpdating: Boolean;
  public

  end;

var
  DisplaySettingsForm: TDisplaySettingsForm;

implementation

{$R *.lfm}

uses
  mega_main, MegaVerConsts;

{ TDisplaySettingsForm }

procedure TDisplaySettingsForm.LargeImagesBtnChange(Sender: TObject);
begin
  if FRadioButtonUpdating then Exit;
  try
    FRadioButtonUpdating := True;
    MegaForm.UseLargeIconSize := LargeImagesBtn.Checked;
  finally
    FRadioButtonUpdating := False;
  end;
end;

procedure TDisplaySettingsForm.ProportionalCboxChange(Sender: TObject);
begin
  MegaForm.ProportionalIcons := ProportionalCbox.Checked;
end;

procedure TDisplaySettingsForm.ScaledIconsCboxChange(Sender: TObject);
begin
  MegaForm.ScaledIcons := ScaledIconsCbox.Checked;
end;

procedure TDisplaySettingsForm.SpinEdit1Change(Sender: TObject);
begin
  MegaForm.ButtonWidth := SpinEdit1.Value;
end;

procedure TDisplaySettingsForm.StretchedIconsCboxChange(Sender: TObject);
begin
  MegaForm.StretchIcons := StretchedIconsCbox.Checked;
end;

procedure TDisplaySettingsForm.DefaultImagesBtnChange(Sender: TObject);
begin
  if FRadioButtonUpdating then Exit;
  try
    FRadioButtonUpdating := True;
    MegaForm.UseLargeIconSize := (not DefaultImagesBtn.Checked);
  finally
    FRadioButtonUpdating := False;
  end;
end;

procedure TDisplaySettingsForm.CloseActionExecute(Sender: TObject);
begin
  Hide;
end;

procedure TDisplaySettingsForm.FormCreate(Sender: TObject);
begin
  FRadioButtonUpdating := False;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Display Settings';
end;

end.

