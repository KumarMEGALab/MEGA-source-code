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

unit AppOptionsDlg;

{$mode objfpc}{$H+}

interface
{$IFDEF VISUAL_BUILD}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Grids, ActnList,
  fpjson, jsonparser, KeywordConsts, MProcessPack, MegaConsts, mimageform,
  app_options_frame;

type

  TRestoreDefaultsCallback = function(AType: TSnTokenCode): Boolean of object;

  { TAppOptions }

  TAppOptions = class(TForm)
    CancelBtnImg: TImage;
    ComputeAction: TAction;
    HelpBtnImg: TImage;
    ImageList1 :TImageList;
    ImageList2: TImageList;
    ButtonsPanel: TPanel;
    OkBtnImg: TImage;
    Panel1: TPanel;
    ResetBtnImg: TImage;
    RestoreDefaultsAction: TAction;
    CancelAction: TAction;
    SaveSettingsAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    procedure CancelActionExecute(Sender: TObject);
    procedure CancelBtnImgMouseEnter(Sender :TObject);
    procedure CancelBtnImgMouseLeave(Sender :TObject);
    procedure CancelBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComputeActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure HelpBtnImgMouseEnter(Sender :TObject);
    procedure HelpBtnImgMouseLeave(Sender :TObject);
    procedure HelpBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OkBtnImgMouseEnter(Sender :TObject);
    procedure OkBtnImgMouseLeave(Sender :TObject);
    procedure OkBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure ResetBtnImgMouseEnter(Sender :TObject);
    procedure ResetBtnImgMouseLeave(Sender :TObject);
    procedure ResetBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RestoreDefaultsActionExecute(Sender: TObject);
    procedure SaveSettingsActionExecute(Sender: TObject);
  private
    OptionsGrid: TDrawGrid;
    FDataType: TSnTokenCode;
    FIsSettingChecked: Boolean;
    FIsSettingOptional: Boolean;
    procedure InitImgButtons;
    procedure PositionButtons;
    procedure PositionButtonsPanel;
    procedure SetDataType(AValue: TSnTokenCode);
    procedure ClearSettings;
  public
    OptionsFrame: TOptionsGridFrame;
    RestoreDefaultsCallback: TRestoreDefaultsCallback;
    isDNA: Boolean;
    iscDNA: Boolean;
    cmdargs: String;
    function SelectedGeneticCode: String;
    function GetMaoStrings(var AList: TStringList; const ProcessPack: TProcessPack): Boolean;
    procedure SizeFormToFitOptions;
    function LoadJson(Filename: String; AType: TSnTokenCode): Boolean;
    function GetJson: TJSONObject;
    procedure ComputeCmdargs;
    procedure GetCmdArgs(var Options: TStringList);
    procedure GetShortCmdArgs(var Options: TStringList);
    procedure SetIsChecked(AValue: Boolean);
    procedure SetIsOptional(AValue: Boolean);
    property IsSettingChecked: Boolean read FIsSettingChecked write SetIsChecked;
    property IsSettingOptional: Boolean read FIsSettingOptional write SetIsOptional;
    property DataType: TSnTokenCode read FDataType write SetDataType;
  end;

var
  AppOptions: TAppOptions;
{$ENDIF}
implementation
{$IFDEF VISUAL_BUILD}
{$R *.lfm}

uses
  MegaAnalysisPrefStrings, math,LCLType, LCL, LCLIntf,
  MegaUtils, typinfo, mhelpfiles, mhelpkeywords,
  manalysisprefdlg, mshortcutshelper;

{ TAppOptions }

procedure TAppOptions.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TAppOptions.ResetBtnImgMouseEnter(Sender :TObject);
begin
  ImageList2.GetBitmap(3, ResetBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.ResetBtnImgMouseLeave(Sender :TObject);
begin
  ImageList2.GetBitmap(2, ResetBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.ResetBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF LINUX}
  RestoreDefaultsActionExecute(Sender);
  {$ENDIF}
end;

procedure TAppOptions.RestoreDefaultsActionExecute(Sender: TObject);
begin
  Assert(Assigned(RestoreDefaultsCallback));
  ModalResult := mrRetry;
  RestoreDefaultsCallback(FDataType);
end;

procedure TAppOptions.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TAppOptions.CancelBtnImgMouseEnter(Sender :TObject);
begin
  ImageList1.GetBitmap(3, CancelBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.CancelBtnImgMouseLeave(Sender :TObject);
begin
  ImageList1.GetBitmap(2, CancelBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.CancelBtnImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF LINUX}
  CancelActionExecute(Sender);
  {$ENDIF}
end;

procedure TAppOptions.ComputeActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TAppOptions.FormActivate(Sender: TObject);
begin
  if isPrototyper then
    ImageList1.GetBitmap(4, OkBtnImg.Picture.Bitmap)
  else
    ImageList1.GetBitmap(0, OkBtnImg.Picture.Bitmap);
  PositionButtons;
  if not OptionsFrame.Initialized then
    OptionsFrame.Initialized := True;
  SizeFormToFitOptions;
end;

procedure TAppOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  OptionsFrame.Initialized := False; { to exit out of any late calls to OnDrawCell}
end;

procedure TAppOptions.FormResize(Sender: TObject);
begin
  try
    BeginFormUpdate;
    if Assigned(OptionsFrame) then
      OptionsFrame.DoResize(Sender);
    PositionButtonsPanel;
  finally
    EndFormUpdate;
  end;
end;

procedure TAppOptions.HelpBtnImgMouseEnter(Sender :TObject);
begin
  ImageList2.GetBitmap(1, HelpBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.HelpBtnImgMouseLeave(Sender :TObject);
begin
  ImageList2.GetBitmap(0, HelpBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.HelpBtnImgMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF LINUX}
  HelpActionExecute(Sender);
  {$ENDIF}
end;

procedure TAppOptions.OkBtnImgMouseEnter(Sender :TObject);
begin
  if IsPrototyper then
    ImageList1.GetBitmap(5, OkBtnImg.Picture.Bitmap)
  else
    ImageList1.GetBitmap(1, OkBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.OkBtnImgMouseLeave(Sender :TObject);
begin
  if isPrototyper then
    ImageList1.GetBitmap(4, OkBtnImg.Picture.Bitmap)
  else
    ImageList1.GetBitmap(0, OkBtnImg.Picture.Bitmap);
end;

procedure TAppOptions.OkBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF LINUX}
  ComputeActionExecute(Sender);
  {$ENDIF}
end;

procedure TAppOptions.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  FDataType := snNoToken;
  RestoreDefaultsCallback := nil;
  InitImgButtons;
  Constraints.MinWidth := Width;

  OptionsFrame := TOptionsGridFrame.Create(Self);
  OptionsFrame.Initialize(Self);
  OptionsGrid := OptionsFrame.OptionsGrid;
  OptionsGrid.Color := MyDefaultBgColor;
  OptionsGrid.ColWidths[1] := 0; {hide checkboxes used for AppLinker system}
  if OptionsGrid.CanSetFocus then
    OptionsGrid.SetFocus;
  ImageForm.UpdateImgList(Self);
end;

procedure TAppOptions.FormDestroy(Sender: TObject);
begin

end;

procedure TAppOptions.SaveSettingsActionExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TAppOptions.InitImgButtons;
begin
  ImageList1.GetBitmap(0, OkBtnImg.Picture.Bitmap);
  ImageList1.GetBitmap(2, CancelBtnImg.Picture.Bitmap);
  ImageList2.GetBitmap(2, ResetBtnImg.Picture.Bitmap);
  ImageList2.GetBitmap(0, HelpBtnImg.Picture.Bitmap);
  {$IFDEF DARWIN}
  HelpBtnImg.Proportional:=True;
  ResetBtnImg.Proportional:=True;
  CancelBtnImg.Proportional:=True;
  OkBtnImg.Proportional:=True;
  {$ENDIF}
end;

procedure TAppOptions.SizeFormToFitOptions;
var
  HeaderHeight: Integer;
begin
  try
    BeginFormUpdate;
    OptionsFrame.ResizeColumns;
    HeaderHeight := OptionsFrame.GetHeaderHeight;
    Height := ((OptionsGrid.DefaultRowHeight * OptionsGrid.RowCount) + Panel1.Height + HeaderHeight);
    Width := Max(Constraints.MinWidth, (OptionsGrid.ColWidths[0] + OptionsGrid.ColWidths[1] + OptionsGrid.ColWidths[2] + 8));
    OptionsFrame.DoResize(nil);
  finally
    EndFormUpdate;
  end;
end;

procedure TAppOptions.PositionButtons;
const
  BTN_SPACING = 8;
begin
  HelpBtnImg.Left := BTN_SPACING;
  ResetBtnImg.Left := HelpBtnImg.Left + HelpBtnImg.Width + BTN_SPACING;
  CancelBtnImg.Left := ResetBtnImg.Left + ResetBtnImg.Width + BTN_SPACING;
  OkBtnImg.Left := CancelBtnImg.Left + CancelBtnImg.Width + BTN_SPACING;
  ButtonsPanel.Width := (HelpBtnImg.Width + ResetBtnImg.Width + CancelBtnImg.Width + OkBtnImg.Width + BTN_SPACING*5);
  Constraints.MinWidth := ButtonsPanel.Width;
end;

procedure TAppOptions.PositionButtonsPanel;
begin
  if ButtonsPanel.Width < ClientWidth then
    ButtonsPanel.Left := Round((ClientWidth - ButtonsPanel.Width)/2)
  else
    ButtonsPanel.Left := 0;
end;

procedure TAppOptions.SetDataType(AValue: TSnTokenCode);
begin
  OptionsFrame.DataType := AValue;
  if FDataType=AValue then Exit;
  FDataType:=AValue;
end;

procedure TAppOptions.ClearSettings;
begin
  OptionsFrame.ClearSettings;
end;

function TAppOptions.GetMaoStrings(var AList: TStringList; const ProcessPack: TProcessPack): Boolean;
begin
  Result := OptionsFrame.GetMaoStrings(AList, ProcessPack);
end;

function TAppOptions.LoadJson(Filename: String; AType: TSnTokenCode): Boolean;
begin
  Result := OptionsFrame.LoadJson(Filename, AType);
end;

function TAppOptions.GetJson: TJSONObject;
begin
  Result := OptionsFrame.GetJson;
end;

procedure TAppOptions.ComputeCmdargs;
begin
  cmdargs := OptionsFrame.ComputeCmdargs;
end;

procedure TAppOptions.GetCmdArgs(var Options: TStringList);
begin
  OptionsFrame.GetCmdArgs(Options);
end;

procedure TAppOptions.GetShortCmdArgs(var Options: TStringList);
begin
  OptionsFrame.GetShortCmdArgs(Options);
end;

function TAppOptions.SelectedGeneticCode: String;
begin
  Result := OptionsFrame.SelectedGeneticCode;
end;

procedure TAppOptions.SetIsChecked(AValue: Boolean);
begin
  if FIsSettingChecked = AValue then
    Exit;
  FIsSettingChecked:=AValue;
end;

procedure TAppOptions.SetIsOptional(AValue: Boolean);
begin
  if FIsSettingOptional = AValue then
    Exit;
  FIsSettingOptional:=AValue;
end;
{$ENDIF}
end.

