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

unit mcalibrateclockdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IniPropStorage, MegaConsts, mimageform, MegaPrivateFiles, MegaUtils;

type


  { TCalibrateClockDlg }

  TCalibrateClockDlg = class(TForm)
    CalibrateByRateBox: TGroupBox;
    CalibrateByTimeBox: TGroupBox;
    IniPropStorage1: TIniPropStorage;
    Label3: TLabel;
    TimeUnitEdit: TEdit;
    FocusedNodeLabel: TLabel;
    EvoRateEdit: TEdit;
    DivTimeEdit: TEdit;
    HelpBtn: TImage;
    CancelBtn: TImage;
    BtnImages: TImageList;
    HelpBtnImages: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TImage;
    Panel1: TPanel;
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure HelpBtnClick(Sender: TObject);
    procedure InitImages;
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
  private
    FCalibrateClockMode: TCalibrateClockMode;
    FDivTime: Double;
    FEvoRate: Double;
    FFocusedNode: Integer;
    procedure SetCalibrateClockMode(AValue: TCalibrateClockMode);
    procedure SetDivTime(AValue: Double);
    procedure SetEvoRate(AValue: Double);
    procedure SetFocusedNode(AValue: Integer);
    function ValidateForm: Boolean;
  public
    property CalibrateClockMode: TCalibrateClockMode read FCalibrateClockMode write SetCalibrateClockMode;
    property DivTime: Double read FDivTime write SetDivTime;
    property EvoRate: Double read FEvoRate write SetEvoRate;
    property FocusedNode: Integer read FFocusedNode write SetFocusedNode;
  end;

implementation

uses
  TreeExplorer_HC, mhelpfiles;

{$R *.lfm}

{ TCalibrateClockDlg }

procedure TCalibrateClockDlg.OkBtnMouseEnter(Sender: TObject);
begin
  BtnImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TCalibrateClockDlg.CancelBtnMouseEnter(Sender: TObject);
begin
  BtnImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TCalibrateClockDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TCalibrateClockDlg.CancelBtnMouseLeave(Sender: TObject);
begin
  BtnImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TCalibrateClockDlg.FormCreate(Sender: TObject);
begin
  FDivTime := -1.0;
  FEvoRate := -1.0;
  SetCalibrateClockMode(ccmEvoRate);
  InitImages;
  HelpContext := HC_CALIBRATE_MOLECULAR_CLOCK;
  HelpKeyword := 'Calibrate_Timetree_with_Molecular_Clock.htm';
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

function TCalibrateClockDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result := True;
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TCalibrateClockDlg.HelpBtnClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TCalibrateClockDlg.InitImages;
begin
  {$IFDEF DARWIN}
  HelpBtn.Proportional:=True;
  CancelBtn.Proportional:=True;
  OkBtn.Proportional:=True;
  {$ENDIF}
end;

procedure TCalibrateClockDlg.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TCalibrateClockDlg.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TCalibrateClockDlg.OkBtnClick(Sender: TObject);
begin
  if ValidateForm then
  begin
    case FCalibrateClockMode of
      ccmEvoRate: FEvoRate := StrToFloat(Trim(EvoRateEdit.Text));
      ccmDivTime: FDivTime := StrToFloat(Trim(DivTimeEdit.Text));
    end;
    ModalResult := mrOk;
  end;
end;

procedure TCalibrateClockDlg.OkBtnMouseLeave(Sender: TObject);
begin
  BtnImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TCalibrateClockDlg.SetCalibrateClockMode(AValue: TCalibrateClockMode);
begin
  FCalibrateClockMode:=AValue;
  case FCalibrateClockMode of
    ccmEvoRate:
      begin
        CalibrateByRateBox.Enabled := True;
        EvoRateEdit.Enabled := True;
        CalibrateByTimeBox.Enabled := False;
        DivTimeEdit.Enabled := False;
        TimeUnitEdit.Enabled := False;
      end;
    ccmDivTime:
      begin
        CalibrateByRateBox.Enabled := False;
        EvoRateEdit.Enabled := False;
        CalibrateByTimeBox.Enabled := True;
        DivTimeEdit.Enabled := True;
        TimeUnitEdit.Enabled := True;
      end;
  end;
end;

procedure TCalibrateClockDlg.SetDivTime(AValue: Double);
begin
  if FDivTime=AValue then Exit;
  FDivTime:=AValue;
end;

procedure TCalibrateClockDlg.SetEvoRate(AValue: Double);
begin
  if FEvoRate=AValue then Exit;
  FEvoRate:=AValue;
end;

procedure TCalibrateClockDlg.SetFocusedNode(AValue: Integer);
begin
  if FFocusedNode=AValue then Exit;
  FFocusedNode:=AValue;
  if FFocusedNode >= 1 then
    FocusedNodeLabel.Caption := Format('For node #%d', [FFocusedNode])
  else
    FocusedNodeLabel.Caption := EmptyStr;
end;

function TCalibrateClockDlg.ValidateForm: Boolean;
var
  tempDbl: Double;
begin
  Result := False;
  case FCalibrateClockMode of
    ccmEvoRate:
      begin
        if Trim(EvoRateEdit.Text) = EmptyStr then
        begin
          ShowMessage('Please provide a value for the evolutionary rate');
          EvoRateEdit.SetFocus;
          Exit;
        end;
        if not TryStrToFloat(Trim(EvoRateEdit.Text), tempDbl) then
        begin
          ShowMessage('Please provide a valid evolutionary rate');
          EvoRateEdit.SelectAll;
          EvoRateEdit.SetFocus;
          Exit;
        end
        else if (tempDbl <= 0) then
        begin
          ShowMessage('Evolutionary rate must be greater than zero');
          if EvoRateEdit.Enabled then
          begin
            EvoRateEdit.SelectAll;
            EvoRateEdit.SetFocus;
          end;
          Exit;
        end;
        FEvoRate := tempDbl;
      end;
    ccmDivTime:
      begin
        if Trim(DivTimeEdit.Text) = EmptyStr then
        begin
          ShowMessage('Please provide a value for the divergence time');
          DivTimeEdit.SetFocus;
          Exit;
        end;
        if not TryStrToFloat(Trim(DivTimeEdit.Text), tempDbl) then
        begin
          ShowMessage('Please provide a valid divergence time');
          DivTimeEdit.SelectAll;
          DivTimeEdit.SetFocus;
          Exit;
        end
        else if (tempDbl <= 0) then
        begin
          ShowMessage('Divergence time must be greater than zero');
          DivTimeEdit.SelectAll;
          DivTimeEdit.SetFocus;
          Exit;
        end;
        FDivTime := tempDbl;
      end;
  end;
  Result := True;
end;

end.

