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

unit msequence_identity_dlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin;

type

  { TSequenceIdentityOptionsDlg }

  TSequenceIdentityOptionsDlg = class(TForm)
    CancelBtn: TImage;
    IncludeReferenceCheckbx: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    ErrorMsg: TLabel;
    MinCutoffEdit: TFloatSpinEdit;
    MaxCutoffEdit: TFloatSpinEdit;
    ImageList1: TImageList;
    OkBtn: TImage;
    Label1: TLabel;
    UseFirstSeqBtn: TRadioButton;
    AutoFindMajorAllele: TRadioButton;
    procedure AlleleFreqCutoffEditChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
  private
    procedure ShowErrorMessage;
  public
    function FirstSequenceIsReference: Boolean;
    function MinCutoffValue: Double;
    function MaxCutoffValue: Double;
    function UseRefSeqInFrequencyCalculation: Boolean;
  end;

var
  SequenceIdentityOptionsDlg: TSequenceIdentityOptionsDlg;

implementation

uses
  math, MegaConsts;

{$R *.lfm}

{ TSequenceIdentityOptionsDlg }

procedure TSequenceIdentityOptionsDlg.OkBtnMouseEnter(Sender: TObject);
begin
  ImageList1.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TSequenceIdentityOptionsDlg.CancelBtnMouseEnter(Sender: TObject);
begin
  ImageList1.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TSequenceIdentityOptionsDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSequenceIdentityOptionsDlg.AlleleFreqCutoffEditChange(Sender: TObject);
begin
  if ErrorMsg.Visible then
    ErrorMsg.Visible := False;
end;

procedure TSequenceIdentityOptionsDlg.CancelBtnMouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TSequenceIdentityOptionsDlg.FormShow(Sender: TObject);
begin
  ErrorMsg.Visible := False;
end;

procedure TSequenceIdentityOptionsDlg.OkBtnClick(Sender: TObject);
begin
  if CompareValue(MinCutoffValue, MaxCutoffValue, FP_CUTOFF) > 0 then
  begin
    ShowErrorMessage;
    Exit;
  end;
  ModalResult := mrOK;
end;

procedure TSequenceIdentityOptionsDlg.OkBtnMouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TSequenceIdentityOptionsDlg.ShowErrorMessage;
begin
  ErrorMsg.Caption := 'Min cutoff value cannot be greater than the max cutoff value';
  ErrorMsg.Visible := True;
end;

function TSequenceIdentityOptionsDlg.FirstSequenceIsReference: Boolean;
begin
  Result := UseFirstSeqBtn.Checked;
end;

function TSequenceIdentityOptionsDlg.MinCutoffValue: Double;
begin
  Result := MinCutoffEdit.Value;
end;

function TSequenceIdentityOptionsDlg.MaxCutoffValue: Double;
begin
  Result := MaxCutoffEdit.Value;
end;

function TSequenceIdentityOptionsDlg.UseRefSeqInFrequencyCalculation: Boolean;
begin
  Result := IncludeReferenceCheckbx.Checked;
end;

end.

