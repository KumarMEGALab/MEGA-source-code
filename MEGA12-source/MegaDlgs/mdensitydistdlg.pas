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

unit mdensitydistdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Spin, ActnList, IniPropStorage, Menus,
  MegaConsts, mcalibrationdensity, MCalibrationData, MegaUtils,
  MegaPrivateFiles;

const
  FLOAT_DELTA = 0.000001;

type

  { TCalibrationParamsDlg }

  TCalibrationParamsDlg = class(TForm)
    ExpDistRateEdit: TFloatSpinEdit;
    ExpDistTimeEdit: TFloatSpinEdit;
    ExponentialDistConstraintsLabel: TLabel;
    FixedRateEdit: TFloatSpinEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LognormalDistConstraintsLabel: TLabel;
    LogNormalMeanSpinEdit: TFloatSpinEdit;
    LogNormalOffsetSpinEdit: TFloatSpinEdit;
    LogNormalStdDevSpinEdit: TFloatSpinEdit;
    NecessaryDummyMenu: TMainMenu;
    NormalDistConstraintsLabel: TLabel;
    NormalDistMeanEdit: TFloatSpinEdit;
    NormalDistStdDevEdit: TFloatSpinEdit;
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    DistributionChart: TChart;
    AreaSeries: TAreaSeries;
    BarSeries: TBarSeries;
    DebugAction: TAction;
    ActionList1: TActionList;
    IniPropStorage1: TIniPropStorage;
    Label11: TLabel;
    Label12: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    SingleTimeEdit: TFloatSpinEdit;
    SingleTimeLabel: TLabel;
    SingleTimeTab: TTabSheet;
    RateTab: TTabSheet;
    LogNormalDistTab: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    UniformDistConstraintsLabel: TLabel;
    UniformDistLbl: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    NormalDistTab: TTabSheet;
    UniformDistMaxTimeEdit: TFloatSpinEdit;
    UniformDistMinTimeEdit: TFloatSpinEdit;
    UniformDistTab: TTabSheet;
    ExponentialDistTab: TTabSheet;
    procedure CancelBtnClick(Sender: TObject);
    procedure DebugActionExecute(Sender: TObject);
    procedure DevBtnClick(Sender: TObject);
    procedure ExpDistRateEditChange(Sender: TObject);
    procedure ExpDistTimeEditChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure LogNormalMeanSpinEditChange(Sender: TObject);
    procedure LogNormalOffsetSpinEditChange(Sender: TObject);
    procedure LogNormalStdDevSpinEditChange(Sender: TObject);
    procedure NormalDistMeanEditChange(Sender: TObject);
    procedure NormalDistStdDevEditChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SingleTimeEditChange(Sender: TObject);
    procedure UniformDistMaxTimeEditChange(Sender: TObject);
    procedure UniformDistMinTimeEditChange(Sender: TObject);
  private
    FIsUpdating: Boolean;
    FCalibration: TCalibrationTime;
    FCalibrationCategory: TCalibrationCategory;
    FDensity: TCalibrationDensity;
    FCalibrationDensityDistribution: TCalibrationDensityDistribution;
    procedure SetParamsFromDensity(aDensity: TCalibrationDensity);
    procedure SetCalibrationCategory(AValue: TCalibrationCategory);
    procedure SetUniformDistMax(AValue: Double);
    procedure SetUniformDistMin(AValue: Double);
    procedure UpdateAreaSeries;
    procedure UpdateAreaSeriesNormal;
    procedure UpdateAreaSeriesLogNormal;
    procedure UpdateAreaSeriesExponential;
    procedure UpdateAreaSeriesUniform;
    procedure SetCalibrationDensityDistribution(AValue: TCalibrationDensityDistribution);
    procedure SetExpDistRate(AValue: Double);
    procedure SetExpDistTime(AValue: Double);
    procedure SetNormalDistMean(AValue: Double);
    procedure SetNormalDistStdDev(AValue: Double);
    function ValidateForm: Boolean;
    procedure HideTabs;
  public
    procedure Initialize(aCalib: TCalibrationTime);
    property CalibrationDensityDistribution: TCalibrationDensityDistribution read FCalibrationDensityDistribution write SetCalibrationDensityDistribution;
    property CalibrationCategory: TCalibrationCategory read FCalibrationCategory write SetCalibrationCategory;
  end;

implementation

{$R *.lfm}

uses
  math, StringUtils, spe, MegaVerConsts, mimageform, mhelpkeywords, mhelpfiles, ContextHelp_HC;

{ TCalibrationParamsDlg }

procedure TCalibrationParamsDlg.OkBtnClick(Sender: TObject);
begin
  if ValidateForm then
    ModalResult := mrOk;
end;

procedure TCalibrationParamsDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TCalibrationParamsDlg.DebugActionExecute(Sender: TObject);
var
  msg: String = '';
begin
  {$IFDEF DEBUG}
  msg := msg + Format('visible: %s', [BoolToStr(PageControl1.Visible, True)]) + LineEnding;
  msg := msg + Format('top: %d', [PageControl1.Top]) + LineEnding;
  msg := msg + Format('left: %d', [PageControl1.Left]) + LineEnding;
  msg := msg + Format('width: %d', [PageControl1.Width]) + LineEnding;
  msg := msg + Format('height: %d', [PageControl1.Height]) + LineEnding;
  msg := msg + Format('active tab: %s', [PageControl1.ActivePage.Caption]) + LineEnding;
  msg := msg + Format('me visible: %s', [BoolToStr(NormalDistMeanEdit.Visible, True)]);
  ShowMessage(Trim(msg));
  {$ENDIF}
end;

procedure TCalibrationParamsDlg.DevBtnClick(Sender: TObject);
var
  aDensity: TCalibrationDensity;
begin
  ShowMessage(Format('isVis: %s, editVis: %s', [BoolToStr(RateTab.Visible, True), BoolToStr(FixedRateEdit.Visible, True)]));
  Exit;
  if Assigned(FDensity) then
    FreeAndNil(FDensity);
  case PageControl1.ActivePageIndex of
    2:
      begin
        aDensity := TCalibrationDensity.CreateExponentialDistUsingLambda(30, 0.25);
        CalibrationDensityDistribution := cddExponential;
        SetParamsFromDensity(aDensity);
      end;
    1:
      begin
        aDensity := TCalibrationDensity.CreateUniformDist(8, 20);
        CalibrationDensityDistribution := cddUniform;
        SetParamsFromDensity(aDensity);
      end;
    0:
      begin
        aDensity := TCalibrationDensity.CreateNormalDist(8, 1);
        CalibrationDensityDistribution := cddNormal;
        SetParamsFromDensity(aDensity);
      end;
  end;
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.ExpDistRateEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.ExpDistTimeEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TCalibrationParamsDlg.FormCreate(Sender: TObject);
begin
  FIsUpdating := False;
  HideTabs;
  NormalDistMeanEdit.MinValue := 0;
  NormalDistMeanEdit.MaxValue := MaxDouble;
  NormalDistMeanEdit.Increment := 1;
  NormalDistMeanEdit.Value := 1;

  NormalDistStdDevEdit.MinValue := 0;
  NormalDistStdDevEdit.MaxValue := MaxDouble;
  NormalDistStdDevEdit.DecimalPlaces := 3;
  NormalDistStdDevEdit.Increment := 0.1;
  NormalDistStdDevEdit.Value := 0.25;

  ExpDistTimeEdit.MinValue := 0;
  ExpDistTimeEdit.MaxValue := MaxDouble;
  ExpDistTimeEdit.Value := 1;
  ExpDistTimeEdit.Increment := 1;

  ExpDistRateEdit.DecimalPlaces := 3;
  ExpDistRateEdit.Value := 0.25;
  ExpDistRateEdit.Increment := 0.1;

  UniformDistMinTimeEdit.MinValue := 0;
  UniformDistMinTimeEdit.MaxValue := MaxDouble;
  UniformDistMinTimeEdit.Value := 1;
  UniformDistMinTimeEdit.Increment := 1;

  UniformDistMaxTimeEdit.MinValue := 0;
  UniformDistMaxTimeEdit.MaxValue := MaxDouble;
  UniformDistMaxTimeEdit.Value := 1;
  UniformDistMaxTimeEdit.Increment := 1;

  SingleTimeEdit.MinValue := 0;
  SingleTimeEdit.MaxValue := MaxDouble;
  SingleTimeEdit.Increment := 1;
  SingleTimeEdit.Value := 1;

  FixedRateEdit.MinValue := 0.00000000000001;
  FixedRateEdit.MaxValue := MaxDouble;
  FixedRateEdit.Increment := 0.01;
  FixedRateEdit.Value := 0.1;
  FixedRateEdit.DecimalPlaces := 4;

  FDensity := nil;
  //{$IFDEF DEBUG}
  // DevBtn.Visible := True;
  //{$ENDIF}
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  UniformDistConstraintsLabel.Font.Color := HOVER_PANEL_COLOR;
  UniformDistConstraintsLabel.Font.Style := [fsBold];
  NormalDistConstraintsLabel.Font.Color := HOVER_PANEL_COLOR;
  NormalDistConstraintsLabel.Font.Style := [fsBold];
  LognormalDistConstraintsLabel.Font.Color := HOVER_PANEL_COLOR;
  LognormalDistConstraintsLabel.Font.Style := [fsBold];
  ExponentialDistConstraintsLabel.Font.Color := HOVER_PANEL_COLOR;
  ExponentialDistConstraintsLabel.Font.Style := [fsBold];
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Calibration Parameters';
  HelpContext := HC_TIMETREE_WIZARD;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TCalibrationParamsDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(FDensity) then
    FDensity.Free;
end;

function TCalibrationParamsDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
begin
  try
    Result := True;
    CallHelp := False;
    ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
  except
    on E:Exception do
      ShowMessage('Application error: failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TCalibrationParamsDlg.FormResize(Sender: TObject);
begin
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TCalibrationParamsDlg.FormShow(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.HelpActionExecute(Sender: TObject);
begin
  {$IFDEF DARWIN}
  if not (Screen.ActiveForm = Self) then Exit;
  {$ENDIF}
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TCalibrationParamsDlg.LogNormalMeanSpinEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.LogNormalOffsetSpinEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.LogNormalStdDevSpinEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.NormalDistMeanEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.NormalDistStdDevEditChange(Sender: TObject);
begin
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.PageControl1Change(Sender: TObject);
begin
  if not Visible then
    Exit;
  Invalidate;
end;

procedure TCalibrationParamsDlg.SingleTimeEditChange(Sender: TObject);
begin
  case CalibrationCategory of
    ccMinTimeOnly:
      begin

      end;
    ccMaxTimeOnly:
      begin

      end;
    ccMinMaxTime:
      begin
        raise Exception.Create('not implemented');
      end;
    ccFixedTime:
      begin

      end;
  end;
end;

procedure TCalibrationParamsDlg.UniformDistMaxTimeEditChange(Sender: TObject);
begin
  if not FIsUpdating then
    UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.UniformDistMinTimeEditChange(Sender: TObject);
begin
  try
    FIsUpdating := True;
    if UniformDistMinTimeEdit.Value > UniformDistMaxTimeEdit.Value then
      UniformDistMaxTimeEdit.Value := UniformDistMinTimeEdit.Value;
  finally
    FIsUpdating := False;
  end;
  UpdateAreaSeries;
end;

procedure TCalibrationParamsDlg.UpdateAreaSeries;
begin
  if Assigned(FDensity) then
    FreeAndNil(FDensity);
  AreaSeries.Clear;
  BarSeries.Clear;
  if PageControl1.ActivePage = NormalDistTab then
    UpdateAreaSeriesNormal
  else if PageControl1.ActivePage = ExponentialDistTab then
    UpdateAreaSeriesExponential
  else if PageControl1.ActivePage = LogNormalDistTab then
    UpdateAreaSeriesLogNormal
  else
    UpdateAreaSeriesUniform;
  DistributionChart.Invalidate;
  Invalidate;
end;

procedure TCalibrationParamsDlg.SetCalibrationCategory(AValue: TCalibrationCategory);
begin
  FCalibrationCategory:=AValue;
  case AValue of
    ccDensityNormal:
      begin
        DistributionChart.Visible := True;
        PageControl1.ActivePage := NormalDistTab;
      end;
    ccDensityLogNormal:
      begin
        DistributionChart.Visible := True;
        PageControl1.ActivePage := LogNormalDistTab;
      end;
    ccDensityExponential:
      begin
        DistributionChart.Visible := True;
        PageControl1.ActivePage := ExponentialDistTab;
      end;
    ccDensityUniform:
      begin
        DistributionChart.Visible := True;
        PageControl1.ActivePage := UniformDistTab;
        UniformDistLbl.Caption := 'Uniform Distribution';
      end;
    ccMinTimeOnly:
      begin
        DistributionChart.Visible := False;
        SingleTimeLabel.Caption := 'Min Time';
        PageControl1.ActivePage := SingleTimeTab;
        SingleTimeTab.Caption := 'Min Time';
      end;
    ccMaxTimeOnly:
      begin
        DistributionChart.Visible := False;
        SingleTimeLabel.Caption := 'Max Time';
        PageControl1.ActivePage := SingleTimeTab;
        SingleTimeTab.Caption := 'Max Time';
      end;
    ccMinMaxTime:
      begin
        PageControl1.ActivePage := UniformDistTab;
        DistributionChart.Visible := False;
        UniformDistLbl.Caption := 'Min and Max Times';
      end;
    ccFixedTime:
      begin
        DistributionChart.Visible := False;
        SingleTimeLabel.Caption := 'Fixed Time';
        PageControl1.ActivePage := SingleTimeTab;
        SingleTimeTab.Caption := 'Fixed Time';
      end;
    ccFixedRate:
      begin
        DistributionChart.Visible := False;
        PageControl1.ActivePage := RateTab;
      end;
    else
      raise Exception.Create('invalid TCalibrationCategory');
    end;
  if DistributionChart.Visible then
    ClientHeight := PageControl1.Height + PageControl1.TabHeight + DistributionChart.Height + Panel1.height
  else
    ClientHeight := PageControl1.Height + PageControl1.TabHeight + Panel1.height;

  //case AValue of
  //  ccMinTimeOnly, ccMaxTimeOnly, ccFixedTime: ClientWidth := (2*SingleTimeEdit.Left + CancelBtn.Width + OkBtn.Width);
  //  ccFixedRate: ClientWidth := (2*SingleTimeEdit.Left + CancelBtn.Width + OkBtn.Width*2);
  //  else
  //    ClientWidth := 3*UniformDistMinTimeEdit.Left + 3*UniformDistMinTimeEdit.Width;
  //end;
end;

procedure TCalibrationParamsDlg.SetUniformDistMax(AValue: Double);
begin
  UniformDistMaxTimeEdit.Value := AValue;
end;

procedure TCalibrationParamsDlg.SetUniformDistMin(AValue: Double);
begin
  UniformDistMinTimeEdit.Value := AValue;
end;

procedure TCalibrationParamsDlg.UpdateAreaSeriesNormal;
var
  i: Integer;
  pdf: TFloatPointArray;
begin
  AreaSeries.ConnectType := ctLine;
  FDensity := TCalibrationDensity.CreateNormalDist(NormalDistMeanEdit.Value, NormalDistStdDevEdit.Value);
  NormalDistConstraintsLabel.Caption := Format('95%% CI: %.2f - %.2f', [FDensity.MinTime, FDensity.MaxTime]);
  pdf := FDensity.XYValsForAreaChart;
  if Length(pdf) = 0 then
    Exit;
  for i := Low(pdf) to High(pdf) do
  begin
    if pdf[i].X >= 0 then
      AreaSeries.AddXY(pdf[i].X, pdf[i].Y);
  end;
  if FDensity.MinTime > 0 then
    BarSeries.AddXY(FDensity.MinTime, FDensity.NormalPdf(FDensity.MinTime));
  if FDensity.MaxTime > 0 then
    BarSeries.AddXY(FDensity.MaxTime, FDensity.NormalPdf(FDensity.MaxTime));
  for i := Low(pdf) to High(pdf) do
    pdf[i].Free;
  SetLength(pdf, 0);
end;

procedure TCalibrationParamsDlg.UpdateAreaSeriesLogNormal;
var
  i: Integer;
  pdf: TFloatPointArray;
begin
  AreaSeries.ConnectType := ctLine;
  FDensity := TCalibrationDensity.CreateLogNormalDist(LogNormalOffsetSpinEdit.Value, LogNormalMeanSpinEdit.Value, LogNormalStdDevSpinEdit.Value);
  LogNormalDistConstraintsLabel.Caption := Format('95%% CI: %.2f - %.2f', [FDensity.MinTime, FDensity.MaxTime]);
  pdf := FDensity.XYValsForAreaChart;
  if Length(pdf) = 0 then
    Exit;
  for i := Low(pdf) to High(pdf) do
  begin
    if pdf[i].X >= 0 then
      AreaSeries.AddXY(pdf[i].X, pdf[i].Y);
  end;
  if FDensity.MinTime > 0 then
    BarSeries.AddXY(FDensity.MinTime, FDensity.LogNormalPdf(FDensity.MinTime - FDensity.Time));
  if FDensity.MaxTime > 0 then
    BarSeries.AddXY(FDensity.MaxTime, FDensity.LogNormalPdf(FDensity.MaxTime - FDensity.Time));
  for i := Low(pdf) to High(pdf) do
    pdf[i].Free;
  SetLength(pdf, 0);
end;

procedure TCalibrationParamsDlg.UpdateAreaSeriesExponential;
var
  i: Integer;
  pdf: TFloatPointArray;
begin
  AreaSeries.ConnectType := ctLine;
  FDensity := TCalibrationDensity.CreateExponentialDistUsingLambda(ExpDistTimeEdit.Value, ExpDistRateEdit.Value);
  ExponentialDistConstraintsLabel.Caption := Format('95%% CI: %.2f - %.2f', [FDensity.MinTime, FDensity.MaxTime]);
  pdf := FDensity.XYValsForAreaChart;
  if Length(pdf) = 0 then
    Exit;
  for i := Low(pdf) to High(pdf) do
    AreaSeries.AddXY(pdf[i].X, pdf[i].Y);
  BarSeries.AddXY(FDensity.MinTime, FDensity.ExponentialPdf(FDensity.MinTime));
  for i := Low(pdf) to High(pdf) do
    pdf[i].Free;
  SetLength(pdf, 0);
end;

procedure TCalibrationParamsDlg.UpdateAreaSeriesUniform;
var
  i: Integer;
  pdf: TFloatPointArray;
begin
  AreaSeries.ConnectType := ctStepXY;
  FDensity := TCalibrationDensity.CreateUniformDist(UniformDistMinTimeEdit.Value, UniformDistMaxTimeEdit.Value);
  UniformDistConstraintsLabel.Caption := Format('95%% CI: %.2f - %.2f', [FDensity.MinTime, FDensity.MaxTime]);
  pdf := FDensity.XYValsForAreaChart;
  if Length(pdf) = 0 then
    Exit;
  for i := Low(pdf) to High(pdf) do
    AreaSeries.AddXY(pdf[i].X, pdf[i].Y);
  //BarSeries.AddXY(FDensity.MinTime, FDensity.UniformPdf(FDensity.MinTime));
  //BarSeries.AddXY(FDensity.MaxTime, FDensity.UniformPdf(FDensity.MaxTime));
  for i := Low(pdf) to High(pdf) do
    pdf[i].Free;
  SetLength(pdf, 0);
end;

procedure TCalibrationParamsDlg.SetCalibrationDensityDistribution( AValue: TCalibrationDensityDistribution);
begin
  FCalibrationDensityDistribution:=AValue;
  case AValue of
    cddNormal:
      begin
        PageControl1.ActivePage := NormalDistTab;
      end;
    cddLogNormal:
      begin
        PageControl1.ActivePage := LogNormalDistTab;
      end;
    cddExponential:
      begin
        PageControl1.ActivePage := ExponentialDistTab;
      end;
    cddUniform:
      begin
        PageControl1.ActivePage := UniformDistTab;
      end;
    else
      raise Exception.Create('Application Error: no handler for calibration density distribution');
  end;
end;

procedure TCalibrationParamsDlg.SetExpDistRate(AValue: Double);
begin
  ExpDistRateEdit.Value := AValue;
end;

procedure TCalibrationParamsDlg.SetExpDistTime(AValue: Double);
begin
  ExpDistTimeEdit.Value := AValue;
end;

procedure TCalibrationParamsDlg.SetNormalDistMean(AValue: Double);
begin
  NormalDistMeanEdit.Value := aValue
end;

procedure TCalibrationParamsDlg.SetNormalDistStdDev(AValue: Double);
begin
  NormalDistStdDevEdit.Value := AValue;
end;

function TCalibrationParamsDlg.ValidateForm: Boolean;
var
  temp: Double;
begin
  Result := False;

  case FCalibration.CalibrationCategory of
    ccDensityNormal:
      begin
        if (Trim(NormalDistMeanEdit.Text) = EmptyStr) or (not TryStrToFloat(NormalDistMeanEdit.Text, temp)) then
        begin
          ShowMessage('Please provide a valid numeric value for the distribution mean');
          {$IFNDEF DARWIN}
          NormalDistMeanEdit.SelectAll;
          NormalDistMeanEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        if (Trim(NormalDistStdDevEdit.Text) = EmptyStr) or (not TryStrToFloat(NormalDistStdDevEdit.Text, temp)) or (CompareValue(temp, 0.0, FLOAT_DELTA) <= 0) then
        begin
          ShowMessage('Please provide a valid numeric value ( > 0) for the distribution standard deviation');
          {$IFNDEF DARWIN}
          NormalDistStdDevEdit.SelectAll;
          NormalDistStdDevEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        FCalibration.SetCalibrationDensity(TCalibrationDensity.CreateNormalDist(NormalDistMeanEdit.Value, NormalDistStdDevEdit.Value));
      end;
    ccDensityLogNormal:
      begin
        if (Trim(LogNormalOffsetSpinEdit.Text) = EmptyStr) or (not TryStrToFloat(LogNormalOffsetSpinEdit.Text, temp)) then
        begin
          ShowMessage('Please provide a valid numeric value for the distribution offset(time)');
          {$IFNDEF DARWIN}
          LogNormalOffsetSpinEdit.SelectAll;
          LogNormalOffsetSpinEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        if (Trim(LogNormalMeanSpinEdit.Text) = EmptyStr) or (not TryStrToFloat(LogNormalMeanSpinEdit.Text, temp)) then
        begin
          ShowMessage('Please provide a valid numeric value for the distribution mean');
          {$IFNDEF DARWIN}
          LogNormalMeanSpinEdit.SelectAll;
          LogNormalMeanSpinEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        if (Trim(LogNormalStdDevSpinEdit.Text) = EmptyStr) or (not TryStrToFloat(LogNormalStdDevSpinEdit.Text, temp)) or (CompareValue(temp, 0.0, FLOAT_DELTA) <= 0) then
        begin
          ShowMessage('Please provide a valid numeric value ( > 0) for the distribution standard deviation');
          {$IFNDEF DARWIN}
          LogNormalStdDevSpinEdit.SelectAll;
          LogNormalStdDevSpinEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        FCalibration.SetCalibrationDensity(TCalibrationDensity.CreateLogNormalDist(LogNormalOffsetSpinEdit.Value, LogNormalMeanSpinEdit.Value, LogNormalStdDevSpinEdit.Value));
      end;
    ccDensityExponential:
      begin
        if (Trim(ExpDistRateEdit.Text) = EmptyStr) or (not TryStrToFloat(ExpDistRateEdit.Text, temp)) or (CompareValue(temp, 0.0, FLOAT_DELTA) <= 0) then
        begin
          ShowMessage('Please provide a valid numeric value ( > 0) for the rate parameter');
          {$IFNDEF DARWIN}
          ExpDistRateEdit.SelectAll;
          ExpDistRateEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        if (Trim(ExpDistTimeEdit.Text) = EmptyStr) or (not TryStrToFloat(ExpDistTimeEdit.Text, temp)) or (CompareValue(temp, 0.0, FLOAT_DELTA) <= 0) then
        begin
          ShowMessage('Please provide a valid numeric value ( > 0) for the time parameter');
          {$IFNDEF DARWIN}
          ExpDistTimeEdit.SelectAll;
          ExpDistTimeEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        FCalibration.SetCalibrationDensity(TCalibrationDensity.CreateExponentialDistUsingLambda(ExpDistTimeEdit.Value, ExpDistRateEdit.Value));
      end;
    ccDensityUniform:
      begin
        if (Trim(UniformDistMinTimeEdit.Text) = EmptyStr) or (not TryStrToFloat(UniformDistMinTimeEdit.Text, temp)) then
        begin
          ShowMessage('Please provide a valid numeric value for the distribution min time');
          {$IFNDEF DARWIN}
          UniformDistMinTimeEdit.SelectAll;
          UniformDistMinTimeEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        if (Trim(UniformDistMaxTimeEdit.Text) = EmptyStr) or (not TryStrToFloat(UniformDistMaxTimeEdit.Text, temp)) or (CompareValue(temp, 0.0, FLOAT_DELTA) <= 0) then
        begin
          ShowMessage('Please provide a valid numeric value ( > 0) for the distribution max time');
          {$IFNDEF DARWIN}
          UniformDistMaxTimeEdit.SelectAll;
          UniformDistMaxTimeEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        if UniformDistMinTimeEdit.Value > UniformDistMaxTimeEdit.Value then
        begin
          ShowMessage('The distribution min time cannot be greater than the distribution max time');
          {$IFNDEF DARWIN}
          UniformDistMaxTimeEdit.SelectAll;
          UniformDistMaxTimeEdit.SetFocus;
          {$ENDIF}
          Exit;
        end;
        FCalibration.SetCalibrationDensity(TCalibrationDensity.CreateUniformDist(UniformDistMinTimeEdit.Value, UniformDistMaxTimeEdit.Value));
      end;
    ccMinTimeOnly:
      begin
        FCalibration.MinTime := SingleTimeEdit.Value;
        FCalibration.MaxTime := -1;
      end;
    ccMaxTimeOnly:
      begin
        FCalibration.MinTime := -1;
        FCalibration.MaxTime := SingleTimeEdit.Value;
      end;
    ccMinMaxTime:
      begin
        FCalibration.MinTime := UniformDistMinTimeEdit.Value;
        FCalibration.MaxTime := UniformDistMaxTimeEdit.Value;
      end;
    ccFixedTime:
      begin
        FCalibration.MinTime := SingleTimeEdit.Value;
        FCalibration.MaxTime := FCalibration.MinTime;
      end;
    ccFixedRate:
      begin
        if CompareValue(FixedRateEdit.Value, 0.0, FP_CUTOFF) < 0 then
        begin
          ShowMessage('Fixed evolutionary rate must be greater than zero');
          {$IFNDEF DARWIN}
          FixedRateEdit.SelectAll;
          FixedRateEdit.SetFocus;
          {$ENDIF}
          Exit;
        end
        else
          FCalibration.MinTime := FixedRateEdit.Value;
      end
    else
      raise Exception.Create('invalid calibration type');
  end;
  Result := True;
end;

procedure TCalibrationParamsDlg.HideTabs;
begin
  PageControl1.ShowTabs := False;
end;

procedure TCalibrationParamsDlg.SetParamsFromDensity(aDensity: TCalibrationDensity);
begin
  if not Assigned(aDensity) then
    Exit;
  case aDensity.CalibrationDensityDistribution of
    cddNormal:
      begin
        PageControl1.ActivePage := NormalDistTab;
        NormalDistMeanEdit.Text := fds(aDensity.Mean, 2, 8);
        NormalDistStdDevEdit.Text := fds(aDensity.StdDev, 2, 8);
      end;
    cddLogNormal:
      begin
        PageControl1.ActivePage := LogNormalDistTab;
        LogNormalMeanSpinEdit.Text := fds(aDensity.Mean, 2, 8);
        LogNormalStdDevSpinEdit.Text := fds(aDensity.StdDev, 2, 8);
        LogNormalOffsetSpinEdit.Text := fds(aDensity.Time, 2, 8);
      end;
    cddExponential:
      begin
        PageControl1.ActivePage := ExponentialDistTab;
        ExpDistRateEdit.Value := aDensity.Lambda;
        ExpDistTimeEdit.Value := aDensity.Time;
      end;
    cddUniform:
      begin
        PageControl1.ActivePage := UniformDistTab;
        UniformDistMinTimeEdit.Value := aDensity.MinTime;
        UniformDistMaxTimeEdit.Value := aDensity.MaxTime;
      end;
    cddNone: ;
  end;
end;

procedure TCalibrationParamsDlg.Initialize(aCalib: TCalibrationTime);
begin
  FCalibration := aCalib;
  SetCalibrationCategory(aCalib.CalibrationCategory);
  case aCalib.CalibrationCategory of
    ccDensityNormal, ccDensityLogNormal, ccDensityExponential, ccDensityUniform:
      begin
        SetParamsFromDensity(aCalib.GetCalibrationDensity);
      end;
    ccMinTimeOnly:
      begin
        PageControl1.ActivePage := SingleTimeTab;
        if aCalib.MinTime > 0 then
          SingleTimeEdit.Value := aCalib.MinTime;
      end;
    ccMaxTimeOnly:
      begin
        PageControl1.ActivePage := SingleTimeTab;
        if aCalib.MaxTime > 0 then
          SingleTimeEdit.Value := aCalib.MaxTime;
      end;
    ccMinMaxTime:
      begin
        PageControl1.ActivePage := UniformDistTab;
        UniformDistMinTimeEdit.Value := aCalib.MinTime;
        UniformDistMaxTimeEdit.Value := aCalib.MaxTime;
      end;
    ccFixedTime:
      begin
        PageControl1.ActivePage := SingleTimeTab;
        if aCalib.MinTime > 0 then
          SingleTimeEdit.Value := aCalib.MinTime;
      end;
    ccFixedRate:
      begin
        PageControl1.ActivePage := RateTab;
        RateTab.Visible := True;
      end;
    ccNone: raise Exception.Create('invalid calibration category');
  end;
end;

end.

