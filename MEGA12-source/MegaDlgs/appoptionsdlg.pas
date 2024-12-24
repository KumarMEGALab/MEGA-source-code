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

unit AppOptionsDlg;

{$mode objfpc}{$H+}

interface
{$IFDEF VISUAL_BUILD}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Grids, ActnList, ComCtrls,
  fpjson, jsonparser, KeywordConsts, MProcessPack, MegaConsts, mimageform,
  app_options_frame;

type

  TRestoreDefaultsCallback = function(AType: TSnTokenCode): Boolean of object;

  { TAppOptions }

  TAppOptions = class(TForm)
    ComputeAction: TAction;
    ButtonsPanel: TPanel;
    Panel1: TPanel;
    RestoreDefaultsAction: TAction;
    CancelAction: TAction;
    SaveSettingsAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure CancelActionExecute(Sender: TObject);
    procedure CancelBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComputeActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure HelpBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OkBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure ResetBtnImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RestoreDefaultsActionExecute(Sender: TObject);
    procedure SaveSettingsActionExecute(Sender: TObject);
  private
    OptionsGrid: TDrawGrid;
    FDataType: TSnTokenCode;
    FIsSettingChecked: Boolean;
    FIsSettingOptional: Boolean;
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
    function GetJsonString: String;
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
  typinfo, mhelpfiles, mhelpkeywords,
  manalysisprefdlg, mshortcutshelper;

{ TAppOptions }

procedure TAppOptions.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
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
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if isPrototyper then
    ComputeAction.ImageIndex := 6
  else
    ComputeAction.ImageIndex := 5;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if ButtonsPanel.Width > ToolBar1.Width then
    ToolBar1.Left := Round((ButtonsPanel.Width - ToolBar1.Width)/2);
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
    if ClientWidth > ToolBar1.Width then
      ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
  finally
    EndFormUpdate;
  end;
end;

procedure TAppOptions.HelpBtnImgMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF LINUX}
  HelpActionExecute(Sender);
  {$ENDIF}
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
  Constraints.MinWidth := Width;

  OptionsFrame := TOptionsGridFrame.Create(Self);
  OptionsFrame.Initialize(Self);
  OptionsGrid := OptionsFrame.OptionsGrid;
  OptionsGrid.Color := MyDefaultBgColor;
  OptionsGrid.ColWidths[1] := 0; {hide checkboxes used for AppLinker system}
  if OptionsGrid.CanSetFocus then
    OptionsGrid.SetFocus;
end;

procedure TAppOptions.FormDestroy(Sender: TObject);
begin

end;

procedure TAppOptions.SaveSettingsActionExecute(Sender: TObject);
begin
  ModalResult := mrOK;
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

function TAppOptions.GetJsonString: String;
var
  j: TJSONObject = nil;
begin
  Result := EmptyStr;
  try
    j := GetJson;
    if Assigned(j) then
      Result := j.AsJSON;
  finally
    if Assigned(j) then
      j.Free;
  end;
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

