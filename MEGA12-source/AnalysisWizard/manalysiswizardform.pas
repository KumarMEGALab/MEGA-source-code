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

unit MAnalysisWizardForm;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, ActnList, ImgList, MAnalysisWizard, MegaConsts, CentralDialogBox_HC,
  mimageform;

type
  TLaunchAnalysisCallback = procedure of object; // to tell TTreeViewForm that the analysis is ready to be launched
  TCancelAnalysisCallback = procedure of object; // to tell TTreeViewForm that the user cancelled

  TLabelArray = array of TLabel;
  TPanelArray = array of TPanel;
  TActionArray = array of TAction;
  TButtonArray = array of TBitBtn;
  TImageArray = array of TImage;

  { TAnalysisWizardForm }

  TAnalysisWizardForm = class(TForm)
    DeveloperAction: TAction;
    Image9: TImage;
    DefaultButtons2: TImageList;
    DisabledButtons2: TImageList;
    HoverButtons2: TImageList;
    HelpButtons2: TImageList;
    SkipButtons2: TImageList;
    RunServerIQTreeLabel1: TLabel;
    ServerIQTreeAction: TAction;
    LocalIQTreeAction: TAction;
    LocalIQTreeLabel: TLabel;
    ServerIQTreeLabel: TLabel;
    SkipTreeAction: TAction;
    SkipTreeBtn: TImage;
    LaunchBtn: TImage;
    Check: TImage;
    Check1: TImage;
    Check4: TImage;
    AnalysisOptionsBtn: TImage;
    CancelLabel: TLabel;
    HoverButtons: TImageList;
    DisabledButtons: TImageList;
    Image1: TImage;
    Image2: TImage;
    Image6: TImage;
    HelpLabel: TLabel;
    ImageList1: TImageList;
    ResetLabel: TLabel;
    SkipButtons: TImageList;
    LoadTreeBtn: TImage;
    LoadSeqDataBtn: TImage;
    HelpButtons: TImageList;
    HelpAction: TAction;
    HeaderPanel: TPanel;
    HeaderLabel: TLabel;
    LoadTreePanel: TPanel;
    AnalysisSettingsPanel: TPanel;
    LoadSeqDataPanel: TPanel;
    LaunchPanel: TPanel;
    StatusBar1: TStatusBar;
    LoadTreeLabel: TLabel;
    LoadSeqDataLabel: TLabel;
    ChooseSettingsLabel: TLabel;
    DefaultButtons: TImageList;
    ActionList1: TActionList;
    LoadTreeAction: TAction;
    LoadSeqDataAction: TAction;
    AnalysisPreferencesAction: TAction;
    LaunchAnalysisAction: TAction;
    SkipAction: TAction;
    QuitAction: TAction;
    LoadDataHelpBtn: TImage;
    LoadTreeHelpBtn: TImage;
    SettingsHelpBtn: TImage;
    ResetAction: TAction;
    procedure AnalysisOptionsBtnMouseEnter(Sender: TObject);
    procedure AnalysisOptionsBtnMouseLeave(Sender: TObject);
    procedure CancelLabelMouseEnter(Sender: TObject);
    procedure CancelLabelMouseLeave(Sender: TObject);
    procedure DeveloperActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure HelpActionExecute(Sender: TObject);
    procedure HelpLabelMouseEnter(Sender: TObject);
    procedure HelpLabelMouseLeave(Sender: TObject);
    procedure LaunchBtnMouseEnter(Sender: TObject);
    procedure LaunchBtnMouseLeave(Sender: TObject);
    procedure LoadDataHelpBtnClick(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure LoadSeqDataBtnMouseEnter(Sender: TObject);
    procedure LoadSeqDataBtnMouseLeave(Sender: TObject);
    procedure LoadTreeActionExecute(Sender: TObject);
    procedure LoadSeqDataActionExecute(Sender: TObject);
    procedure LoadTreeBtnMouseEnter(Sender: TObject);
    procedure LoadTreeBtnMouseLeave(Sender: TObject);
    procedure LocalIQTreeActionExecute(Sender: TObject);
    procedure ResetLabelMouseEnter(Sender: TObject);
    procedure ResetLabelMouseLeave(Sender: TObject);
    procedure ServerIQTreeActionExecute(Sender: TObject);
    procedure SkipTreeActionExecute(Sender: TObject);
    procedure SkipTreeBtnMouseEnter(Sender: TObject);
    procedure SkipTreeBtnMouseLeave(Sender: TObject);
    procedure AnalysisPreferencesActionExecute(Sender: TObject);
    procedure LaunchAnalysisActionExecute(Sender: TObject);
    procedure SkipActionExecute(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ResetActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FProcessStep: TTtProcessStep;
    FLabels: TLabelArray;
    FPanels: TPanelArray;
    FActions: TActionArray;
    FButtons: TImageArray;
    FCurrentStepColor: TColor;
    FDefaultColor: TColor;
    FDefaultFontColor: TColor;
    FActiveFontColor: TColor;
    procedure InitLabelArray;
    procedure InitPanelArray;
    procedure InitActionArray;
    procedure InitButtonArray;
    procedure AdvanceToStep;
    procedure UnHighlightAllPanels;
    procedure HighlightAllPanels;
    procedure InitForm;
    procedure InitFontColors;
    function WizardMode: TAnalysisWizardMode;
    function AnalysisType: TAnalysisWizardAnalysis;
    procedure SetProcessStep(const Value: TTtProcessStep);
    procedure ApplyUpdatesForAnalysisType;
    procedure UpdateForHyphy; { this is here just for demo purposes. If we want to go to a wizard-style system for external applications in the future, then we should create a new wizard system}
    procedure InitImages;
    function GetHelpButtons: TImageList;
    function GetSkipButtons: TImageList;
    function GetDefaultButtons: TImageList;
    function GetDisabledButtons: TImageList;
    function GetHoverButtons: TImageList;
    { Private declarations }
  public
    LaunchAnalysisCallback: TLaunchAnalysisCallback;
    CancelAnalysisCallback: TCancelAnalysisCallback;
    Wizard: TAnalysisWizard;
    procedure UpdateView(Step: TTtProcessStep);
    { Public declarations }
  end;

var
  AnalysisWizardForm: TAnalysisWizardForm;

implementation

uses
  Mega_Main, ContextHelp_HC, mhelpfiles, mhelpkeywords, uMegaBrowser, math,
  MegaVerConsts;

{$R *.lfm}

procedure TAnalysisWizardForm.AdvanceToStep;
begin
  UnHighlightAllPanels;
  case FProcessStep of
    ttpsLoadTree:
      begin
        ImageList1.GetBitmap(0, Check.Picture.Bitmap);
        LoadTreeLabel.Font.Color := FActiveFontColor;
        LoadTreeAction.Enabled := True;
        GetDefaultButtons.GetBitmap(0, LoadTreeBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to load a tree file';
        if LoadSeqDataAction.Enabled then
        begin
          GetDefaultButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
          LoadSeqDataLabel.Font.Color := FDefaultFontColor;
        end
        else
        begin
          GetDisabledButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
          LoadSeqDataLabel.Font.Color := FDefaultFontColor;
        end;
        if (AnalysisType = ttaHyPhy) then
          SkipAction.Enabled := True;
      end;
    ttpsLoadSeqData:
      begin
        LoadSeqDataLabel.Font.Color := FActiveFontColor;
        LoadSeqDataAction.Enabled := True;
        GetDefaultButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to load a sequence data file';
      end;
    ttpsDoSettings:
      begin
        if (AnalysisType = ttaHyPhy) and (not MegaForm.HasCodonData) then
        begin
          ShowMessage('Codon selection analysis requires protein-coding sequence data');
          MegaForm.CloseDataActionExecute(Self);
          ResetActionExecute(Self);
          Exit;
        end;
        ChooseSettingsLabel.Font.Color := FActiveFontColor;
        AnalysisPreferencesAction.Enabled := True;
        GetDefaultButtons.GetBitmap(4, AnalysisOptionsBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to select analysis settings';
      end;
    ttpsLaunchAnalysis:
      begin
        ImageList1.GetBitmap(0, Check4.Picture.Bitmap);
        LaunchAnalysisAction.Enabled := True;
        GetDefaultButtons.GetBitmap(5, LaunchBtn.Picture.Bitmap);
        Self.BringToFront;
      end;
  end;
end;

procedure TAnalysisWizardForm.AnalysisPreferencesActionExecute(Sender: TObject);
begin
  if not AnalysisPreferencesAction.Enabled then
    Exit;
  try
    AnalysisPreferencesAction.Enabled := False;
    Wizard.GetAnalysisPreferences;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;


function TAnalysisWizardForm.AnalysisType: TAnalysisWizardAnalysis;
begin
  if Assigned(Wizard) then
    Result := Wizard.AnalysisType
end;

procedure TAnalysisWizardForm.ApplyUpdatesForAnalysisType;
begin
  case AnalysisType of
    ttaHyPhy: UpdateForHyphy;
  end;
end;

procedure TAnalysisWizardForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  { only let the user close the window via the QuitAction, so we can notify listeners that the user has cancelled}
end;

procedure TAnalysisWizardForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  HeaderLabel.Font.Color := clGrayText;
  {$IFNDEF DEBUG}
  //DeveloperAction.Enabled := False;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
end;

procedure TAnalysisWizardForm.FormShow(Sender: TObject);
begin
  InitForm;
  ResetAction.Visible := (Wizard.LaunchedFrom <> ttsTreeExplorer);
end;

procedure TAnalysisWizardForm.InitLabelArray;
begin
  SetLength(FLabels, 4);
  FLabels[0] := LoadSeqDataLabel;
  FLabels[1] := LoadTreeLabel;
  FLabels[2] := ChooseSettingsLabel;
end;

procedure TAnalysisWizardForm.InitImages;
begin
  GetDisabledButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
  GetDisabledButtons.GetBitmap(0, LoadTreeBtn.Picture.Bitmap);
  GetDisabledButtons.GetBitmap(4, AnalysisOptionsBtn.Picture.Bitmap);
  GetDisabledButtons.GetBitmap(5, LaunchBtn.Picture.Bitmap);
  GetSkipButtons.GetBitmap(2, SkipTreeBtn.Picture.Bitmap);
  GetHelpButtons.GetBitmap(0, LoadDataHelpBtn.Picture.Bitmap);
  GetHelpButtons.GetBitmap(0, LoadTreeHelpBtn.Picture.Bitmap);
  GetHelpButtons.GetBitmap(0, SettingsHelpBtn.Picture.Bitmap);
end;

function TAnalysisWizardForm.GetHelpButtons: TImageList;
var
  scaleFactor: Double = -1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) > 0 then
    Result := HelpButtons2
  else
    Result := HelpButtons;
end;

function TAnalysisWizardForm.GetSkipButtons: TImageList;
var
  scaleFactor: Double = -1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) > 0 then
    Result := SkipButtons2
  else
    Result := SkipButtons;
end;

function TAnalysisWizardForm.GetDefaultButtons: TImageList;
var
  scaleFactor: Double = -1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) > 0 then
    Result := DefaultButtons2
  else
    Result := DefaultButtons;
end;

function TAnalysisWizardForm.GetDisabledButtons: TImageList;
var
  scaleFactor: Double = -1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) > 0 then
    Result := DisabledButtons2
  else
    Result := DisabledButtons;
end;

function TAnalysisWizardForm.GetHoverButtons: TImageList;
var
  scaleFactor: Double = -1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) > 0 then
    Result := HoverButtons2
  else
    Result := HoverButtons;
end;

procedure TAnalysisWizardForm.UpdateForHyphy;
var
  reduceBy: Integer;
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Codon-by-Codon Selection (Datamonkey)';
  HeaderLabel.Caption := 'Codon-by-Codon Selection (Datamonkey)';
  HeaderLabel.Height:=HeaderLabel.Canvas.TextHeight('Codon-by-Codon Selection (Datamonkey)');
  LoadDataHelpBtn.Hint := 'The MEGA or Fasta formatted sequence alignment file to infer dN and dS substitution rates';
  SettingsHelpBtn.Hint := 'You can set preferences for substitution model to be used, distribution of evolutionary rates,  treatment of missing data, etc...';
  reduceBy := LoadTreePanel.Height;
  LoadTreePanel.Visible := False;
  LoadTreePanel.Enabled := False;
  ClientHeight := ClientHeight - reduceBy;
  AdvanceToStep;
end;


procedure TAnalysisWizardForm.UpdateView(Step: TTtProcessStep);
begin
  SetProcessStep(Step);
end;

function TAnalysisWizardForm.WizardMode: TAnalysisWizardMode;
begin
  if Assigned(Wizard) then
    Result := Wizard.WizardMode
  else
    Result := awmSeqData; { be default}
end;

procedure TAnalysisWizardForm.InitActionArray;
begin
  SetLength(FActions, 6);
  FActions[0] := LoadTreeAction;
  FActions[1] := LoadSeqDataAction;
  FActions[2] := AnalysisPreferencesAction;
  FActions[3] := SkipAction;
  FActions[4] := LaunchAnalysisAction;
  FActions[5] := SkipTreeAction;
end;

procedure TAnalysisWizardForm.InitButtonArray;
begin
  SetLength(FButtons, 5);
  FButtons[0] := LoadTreeBtn;
  FButtons[1] := LoadSeqDataBtn;
  FButtons[2] := AnalysisOptionsBtn;
  FButtons[3] := LaunchBtn;
  FButtons[4] := SkipTreeBtn;
end;

procedure TAnalysisWizardForm.InitForm;
begin
  if (AnalysisType = ttaHyPhy) then
     HelpContext := HC_CodonOmega_Analysis_Options
  else
     HelpContext := HC_Index;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  InitLabelArray;
  InitPanelArray;
  InitActionArray;
  InitButtonArray;
  FDefaultColor := clWhite;
  FDefaultFontColor := RGB($c4, $c4, $c4);
  FCurrentStepColor := FDefaultColor;
  FActiveFontColor := RGB(31, 156, 157);
  HeaderPanel.Color := FDefaultColor;
  InitFontColors;
  UnHighlightAllPanels;
  InitImages;
  SkipTreeBtn.Enabled:=False;
  case WizardMode of
    awmSeqData:
      begin
        if not MegaForm.HasSequenceData then
          FProcessStep := ttpsLoadSeqData
        else if (MegaForm.HasSequenceData) and (AnalysisType = ttaHyPhy) then
             FProcessStep := ttpsDoSettings
        else
          FProcessStep := ttpsLoadTree;
      end;
  end;
  ApplyUpdatesForAnalysisType;
  AdvanceToStep;
end;

procedure TAnalysisWizardForm.InitFontColors;
begin
  Font.Color := FDefaultFontColor;
end;

procedure TAnalysisWizardForm.InitPanelArray;
var
  i: Integer;
begin
  SetLength(FPanels, 4);
  FPanels[0] := LoadTreePanel;
  FPanels[1] := LoadSeqDataPanel;
  FPanels[2] := AnalysisSettingsPanel;
  FPanels[3] := LaunchPanel;
  for i := 0 to Length(FPanels) -1 do
  begin
    FPanels[i].ControlStyle := FPanels[i].ControlStyle - [csParentBackground] + [csOpaque];
  end;
end;

procedure TAnalysisWizardForm.LaunchAnalysisActionExecute(Sender: TObject);
begin
  if not LaunchAnalysisAction.Enabled then
    Exit;
  try
    if Wizard.LaunchedFrom = ttsTreeExplorer then
    begin
      Assert(Assigned(LaunchAnalysisCallback));
      LaunchAnalysisCallback;
    end
    else
    begin
      MegaForm.RemoveWindowFromTray(Self);
      LaunchAnalysisAction.Enabled := False;
      Self.Hide;
      ModalResult := mrOk;
      Wizard.LaunchAnalysis;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TAnalysisWizardForm.LoadSeqDataActionExecute(Sender: TObject);
begin
  if not LoadSeqDataAction.Enabled then
    Exit;
  try
    LoadSeqDataAction.Enabled := False;
    GetDisabledButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
    LoadSeqDataLabel.Font.Color := FDefaultFontColor;
    Wizard.LoadSeqData;
    ImageList1.GetBitmap(0, Check.Picture.Bitmap);
  Except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TAnalysisWizardForm.LoadTreeBtnMouseEnter(Sender: TObject);
begin
  if not LoadTreeAction.Enabled then
    Exit;
  GetHoverButtons.GetBitmap(0, LoadTreeBtn.Picture.Bitmap);
  LoadTreeBtn.Invalidate;
end;

procedure TAnalysisWizardForm.LoadTreeBtnMouseLeave(Sender: TObject);
begin
  if not LoadTreeAction.Enabled then
    Exit;
  GetDefaultButtons.GetBitmap(0, LoadTreeBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.LocalIQTreeActionExecute(Sender: TObject);
begin
end;

procedure TAnalysisWizardForm.ResetLabelMouseEnter(Sender: TObject);
begin
  ResetLabel.Font.Color := FActiveFontColor;
  ResetLabel.Invalidate;
end;

procedure TAnalysisWizardForm.ResetLabelMouseLeave(Sender: TObject);
begin
  ResetLabel.Font.Color := clGrayText;
  ResetLabel.Invalidate;
end;

procedure TAnalysisWizardForm.ServerIQTreeActionExecute(Sender: TObject);
begin
end;

procedure TAnalysisWizardForm.SkipTreeActionExecute(Sender: TObject);
begin
    if not SkipTreeAction.Enabled then
    Exit;
    if Wizard.LaunchedFrom = ttsMegaMain then
      FProcessStep := ttpsDoSettings
    else if IsDeveloper then
      FProcessStep := ttpsDoSettings
    else
    begin
      FProcessStep := ttpsLaunchAnalysis;
    end;
    Wizard.SkipTreeActionActivated := True;
  AdvanceToStep;
end;

procedure TAnalysisWizardForm.SkipTreeBtnMouseEnter(Sender: TObject);
begin
  GetSkipButtons.GetBitmap(1, SkipTreeBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.SkipTreeBtnMouseLeave(Sender: TObject);
begin
  GetSkipButtons.GetBitmap(0, SkipTreeBtn.Picture.Bitmap)
end;

procedure TAnalysisWizardForm.LoadTreeActionExecute(Sender: TObject);
begin
  if not LoadTreeAction.Enabled then
    Exit;
  try
    LoadTreeAction.Enabled := False;
    GetDisabledButtons.GetBitmap(0, LoadTreeBtn.Picture.Bitmap);
    LoadTreeLabel.Font.Color := FDefaultFontColor;
    Wizard.LoadTree;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TAnalysisWizardForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  try
    Result := True;
    helpTopic := MapHelpContextToKeyword(HelpContext);
    CallHelp := False;
    if Trim(helpTopic) <> EmptyStr then
      ShowContextSensitiveHelp(helpTopic)
    else
      ShowContextSensitiveHelp(HelpKeyword);
  except
    on E:Exception do
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TAnalysisWizardForm.CancelLabelMouseEnter(Sender: TObject);
begin
  CancelLabel.Font.Color := FActiveFontColor;
  CancelLabel.Invalidate;
end;

procedure TAnalysisWizardForm.AnalysisOptionsBtnMouseEnter(Sender: TObject);
begin
  if not AnalysisPreferencesAction.Enabled then
    Exit;
  GetHoverButtons.GetBitmap(4, AnalysisOptionsBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.AnalysisOptionsBtnMouseLeave(Sender: TObject);
begin
  if not AnalysisPreferencesAction.Enabled then
    Exit;
  GetDefaultButtons.GetBitmap(4, AnalysisOptionsBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.CancelLabelMouseLeave(Sender: TObject);
begin
  CancelLabel.Font.Color := clGrayText;
  CancelLabel.Invalidate;
end;

procedure TAnalysisWizardForm.DeveloperActionExecute(Sender: TObject);
begin

end;

procedure TAnalysisWizardForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TAnalysisWizardForm.HelpActionExecute(Sender: TObject);
var
  browser: TMegaBrowserFrm = nil;
begin
    ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TAnalysisWizardForm.HelpLabelMouseEnter(Sender: TObject);
begin
  HelpLabel.Font.Color := FActiveFontColor;
  HelpLabel.Invalidate;
end;

procedure TAnalysisWizardForm.HelpLabelMouseLeave(Sender: TObject);
begin
  HelpLabel.Font.Color := clGrayText;
end;

procedure TAnalysisWizardForm.LaunchBtnMouseEnter(Sender: TObject);
begin
  if not LaunchAnalysisAction.Enabled then
    Exit;
  GetHoverButtons.GetBitmap(5, LaunchBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.LaunchBtnMouseLeave(Sender: TObject);
begin
  if not LaunchAnalysisAction.Enabled then
    Exit;
  GetDefaultButtons.GetBitmap(5, LaunchBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.LoadDataHelpBtnClick(Sender: TObject);
var
  cursorPos: TPoint;
  aPos: TPoint;
begin
  aPos := Point(1, 1);
  CursorPos := LoadDataHelpBtn.ClientToScreen(aPos);
  Application.ActivateHint(CursorPos);
end;

procedure TAnalysisWizardForm.HelpBtnMouseEnter(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  GetHelpButtons.GetBitmap(1, aImage.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.HelpBtnMouseLeave(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  GetHelpButtons.GetBitmap(0, aImage.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.LoadSeqDataBtnMouseEnter(Sender: TObject);
begin
  if not LoadSeqDataAction.Enabled then
    Exit;
  GetHoverButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.LoadSeqDataBtnMouseLeave(Sender: TObject);
begin
  if not LoadSeqDataAction.Enabled then
    Exit;
  GetDefaultButtons.GetBitmap(0, LoadSeqDataBtn.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.QuitActionExecute(Sender: TObject);
begin
  try
    if Wizard.LaunchedFrom = ttsTreeExplorer then
    begin
      Assert(Assigned(CancelAnalysisCallback));
      CancelAnalysisCallback;
      if not MegaForm.HasSequenceData then
        MegaForm.CloseDataBtn.Hide;
      Close;
    end
    else
    begin
      MegaForm.RemoveWindowFromTray(Self);
      ModalResult := mrCancel;
      if not MegaForm.HasSequenceData then
        MegaForm.CloseDataBtn.Hide;
      Hide;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TAnalysisWizardForm.ResetActionExecute(Sender: TObject);
begin
  if WizardMode = awmSeqData then
    MegaForm.PromptCloseCurrentlyActive;
  if (MegaForm.HasSequenceData) and (AnalysisType = ttaHyPhy) then
    UpdateView(ttpsDoSettings);
  if MegaForm.HasActiveData then
  begin
    Wizard.CheckResetUserTree;
    UpdateView(ttpsLoadTree)
  end
  else
  begin
    if Assigned(Wizard) then
      Wizard.Reset;
    UpdateView(ttpsLoadSeqData);
  end;

  ImageList1.GetBitmap(1, Check.Picture.Bitmap);
  ImageList1.GetBitmap(1, Check1.Picture.Bitmap);
  ImageList1.GetBitmap(1, Check4.Picture.Bitmap);
end;

procedure TAnalysisWizardForm.SetProcessStep(const Value: TTtProcessStep);
begin
  if Value = ttpsCancelAnalysis then
  begin
    QuitActionExecute(Self);
    Exit;
  end;

  FProcessStep := Value;
  AdvanceToStep;
end;

procedure TAnalysisWizardForm.SkipActionExecute(Sender: TObject);
begin
  if not SkipAction.Enabled then
    Exit;
  if AnalysisType = ttaHyPhy then
  begin
    FProcessStep := ttpsDoSettings;
  end
  else
  begin
    if Wizard.LaunchedFrom = ttsMegaMain then
      FProcessStep := ttpsDoSettings
    else if IsDeveloper then
      FProcessStep := ttpsDoSettings
    else
    begin
      FProcessStep := ttpsLaunchAnalysis;
    end;
  end;
  AdvanceToStep;
end;

procedure TAnalysisWizardForm.UnHighlightAllPanels;
var
  i: Integer;
begin
  for i := 0 to Length(FPanels) - 1 do
  begin
    FPanels[i].Color := FDefaultColor;
    FPanels[i].Font.Color := FDefaultFontColor;
    FPanels[i].BevelOuter := bvNone;
    FPanels[i].BevelInner := bvNone;
  end;

  for i := 0 to Length(FActions) - 1 do
    FActions[i].Enabled := False;

  for i := 0 to Length(FLabels) - 1 do
    FLabels[i].Font.Color := FDefaultFontColor;
  InitImages;
end;

procedure TAnalysisWizardForm.HighlightAllPanels;
var
  i: Integer;
begin
  for i := 0 to Length(FPanels) - 1 do
  begin
    FPanels[i].Color := FDefaultColor;
    FPanels[i].Font.Color := FDefaultFontColor;
    FPanels[i].BevelOuter := bvNone;
    FPanels[i].BevelInner := bvNone;
  end;

  for i := 0 to Length(FActions) - 1 do
    FActions[i].Enabled := True;

  for i := 0 to Length(FLabels) - 1 do
    FLabels[i].Font.Color := FDefaultFontColor;
end;

end.
