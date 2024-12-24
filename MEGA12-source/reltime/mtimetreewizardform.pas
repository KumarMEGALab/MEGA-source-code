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

unit MTimeTreeWizardForm;

interface

uses
  LCLIntf, LCLType,SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, ActnList, ImgList, Menus, MTimeTreeWizard, MegaConsts,
  mimageform;

const
  COMPONENTS_SPACING = 10;

type
  TLaunchAnalysisCallback = procedure of object; // to tell TTreeViewForm that the analysis is ready to be launched
  TCancelAnalysisCallback = procedure of object; // to tell TTreeViewForm that the user cancelled
  TCancelCalibrationsCallback = procedure of object; // to tell TTreeViewForm that the user decided to skip calibrations but still continue

  TLabelArray = array of TLabel;
  TPanelArray = array of TPanel;
  TActionArray = array of TAction;
  TButtonArray = array of TBitBtn;
  TImageArray = array of TImage;

  { TTimeTreeWizardForm }

  TTimeTreeWizardForm = class(TForm)
    Check: TImage;
    Check1: TImage;
    Check2: TImage;
    Check3: TImage;
    Check4: TImage;
    DeveloperAction: TAction;
    AnalysisOptionsBtn: TImage;
    CancelLabel: TLabel;
    HoverButtons: TImageList;
    DisabledButtons: TImageList;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image6: TImage;
    HelpLabel: TLabel;
    CheckBoxImages: TImageList;
    HelpButtons2: TImageList;
    DefaultButtons2: TImageList;
    HoverButtons2: TImageList;
    DisabledButtons2: TImageList;
    NecessaryDummyMenu: TMainMenu;
    SkipButtons2: TImageList;
    ResetLabel: TLabel;
    SkipButtons: TImageList;
    LaunchBtn: TImage;
    SkipBtn: TImage;
    SpecifyCalibrationsBtn: TImage;
    SpecifyOutgroupBtn: TImage;
    LoadTreeBtn: TImage;
    LoadSeqDataBtn: TImage;
    HelpButtons: TImageList;
    HelpAction: TAction;
    HeaderPanel: TPanel;
    HeaderLabel: TLabel;
    LoadTreePanel: TPanel;
    CalibrationsPanel: TPanel;
    AnalysisSettingsPanel: TPanel;
    LoadSeqDataPanel: TPanel;
    LaunchPanel: TPanel;
    OutgroupPanel: TPanel;
    SpecifyOutgroupTEBtn: TImage;
    StatusBar1: TStatusBar;
    LoadTreeLabel: TLabel;
    LoadSeqDataLabel: TLabel;
    SpecifyOutgroupLabel: TLabel;
    SpecifyCalibrationsLabel: TLabel;
    ChooseSettingsLabel: TLabel;
    DefaultButtons: TImageList;
    ActionList1: TActionList;
    LoadTreeAction: TAction;
    LoadSeqDataAction: TAction;
    SpecifyOutgroupTaxaAction: TAction;
    SpecifyCalibrationsAction: TAction;
    AnalysisPreferencesAction: TAction;
    LaunchAnalysisAction: TAction;
    SkipAction: TAction;
    QuitAction: TAction;
    LoadDataHelpBtn: TImage;
    LoadTreeHelpBtn: TImage;
    SpecifyOutgroupHelpBtn: TImage;
    SpecifyCalibrationsHelpBtn: TImage;
    SettingsHelpBtn: TImage;
    SpecifyOutgroupTEAction: TAction;
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
    procedure ResetLabelMouseEnter(Sender: TObject);
    procedure ResetLabelMouseLeave(Sender: TObject);
    procedure SkipBtnMouseEnter(Sender: TObject);
    procedure SkipBtnMouseLeave(Sender: TObject);
    procedure SpecifyCalibrationsBtnMouseEnter(Sender: TObject);
    procedure SpecifyCalibrationsBtnMouseLeave(Sender: TObject);
    procedure SpecifyOutgroupBtnMouseEnter(Sender: TObject);
    procedure SpecifyOutgroupBtnMouseLeave(Sender: TObject);
    procedure SpecifyOutgroupTaxaActionExecute(Sender: TObject);
    procedure SpecifyCalibrationsActionExecute(Sender: TObject);
    procedure AnalysisPreferencesActionExecute(Sender: TObject);
    procedure LaunchAnalysisActionExecute(Sender: TObject);
    procedure SkipActionExecute(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpecifyOutgroupTEActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ResetActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpecifyOutgroupTEBtnMouseEnter(Sender: TObject);
    procedure SpecifyOutgroupTEBtnMouseLeave(Sender: TObject);
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
    FCalibrationsSkipped: Boolean;
    procedure InitLabelArray;
    procedure InitPanelArray;
    procedure InitActionArray;
    procedure InitButtonArray;
    procedure AdvanceToStep;
    procedure UnHighlightAllPanels;
    procedure HighlightAllPanels;
    procedure InitForm;
    procedure InitFontColors;
    function WizardMode: TTimeTreeWizardMode;
    function AnalysisType: TTimeTreeAnalysis;
    procedure SetProcessStep(const Value: TTtProcessStep);
    function CanSetOutgroup: Boolean;
    procedure ApplyUpdatesForWizardMode;
    procedure ApplyUpdatesForAnalysisType;
    procedure UpdateForSeqDataMode;
    procedure UpdateForBlensMode;
    procedure UpdateForReltime;
    procedure UpdateForCorrtest;
    procedure UpdateForEp;
    procedure UpdateForLbsInference;
    procedure UpdateForLbsUserTreeTiming;
    procedure UpdateForHyphy; { this is here just for demo purposes. If we want to go to a wizard-style system for external applications in the future, then we should create a new wizard system}
    procedure InitImages;
    procedure HideTreeExplorerOutgroupButton;
    function GetDisabledImages: TImageList;
    function GetDefaultImages: TImageList;
    function GetHoverImages: TImageList;
    function GetHelpImages: TImageList;
    function GetSkipImages: TImageList;
    { Private declarations }
  public
    LaunchAnalysisCallback: TLaunchAnalysisCallback;
    CancelAnalysisCallback: TCancelAnalysisCallback;
    CancelCalibrationsCallback: TCancelCalibrationsCallback;
    Wizard: TTimeTreeWizard;
    procedure UpdateView(Step: TTtProcessStep);
    { Public declarations }
  end;

var
  TimeTreeWizardForm: TTimeTreeWizardForm;

implementation

uses
  Mega_Main, ContextHelp_HC, mhelpfiles, mhelpkeywords, mshortcutshelper, MCalibrationData,
  MegaVerConsts, mdistpack;

{$R *.lfm}

procedure TTimeTreeWizardForm.AdvanceToStep;
begin
  UnHighlightAllPanels;
  case FProcessStep of
    ttpsLoadTree:
      begin
        CheckBoxImages.GetBitmap(0, Check.Picture.Bitmap);
        LoadTreeLabel.Font.Color := FActiveFontColor;
        LoadTreeAction.Enabled := True;
        GetDefaultImages.GetBitmap(1, LoadTreeBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to load a tree file';
        if LoadSeqDataAction.Enabled then
        begin
          GetDefaultImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
          LoadSeqDataLabel.Font.Color := FDefaultFontColor;
        end;
      end;
    ttpsLoadSeqData:
      begin
        LoadSeqDataLabel.Font.Color := FActiveFontColor;
        LoadSeqDataAction.Enabled := True;
        GetDefaultImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
        CheckBoxImages.GetBitmap(1, Check.Picture.Bitmap);
        Check.Invalidate;
        StatusBar1.SimpleText := 'Ready to load a sequence data file';
      end;
    ttpsDoOutgroup:
      begin
        CheckBoxImages.GetBitmap(0, Check1.Picture.Bitmap);
        SpecifyOutgroupLabel.Font.Color := FActiveFontColor;
        SpecifyOutgroupTaxaAction.Enabled := True;
        SpecifyOutgroupTEAction.Enabled := True;
        GetDefaultImages.GetBitmap(4, SpecifyOutgroupTEBtn.Picture.Bitmap);
        GetDefaultImages.GetBitmap(5, SpecifyOutgroupBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to select the outgroup taxa';
      end;
    ttpsDoCalibrations:
      begin
        CheckBoxImages.GetBitmap(0, Check2.Picture.Bitmap);
        SpecifyCalibrationsLabel.Font.Color := FActiveFontColor;
        SpecifyCalibrationsAction.Enabled := True;
        GetDefaultImages.GetBitmap(0, SpecifyCalibrationsBtn.Picture.Bitmap);
        SpecifyOutgroupTaxaAction.Enabled := CanSetOutgroup;
        SpecifyOutgroupTEAction.Enabled := CanSetOutgroup;
        SkipAction.Enabled := True;
        GetSkipImages.GetBitmap(0, SkipBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to specify divergence time calibration constraints';
        //{$IFDEF DEBUG}
        //if IsGlenDevMode then
        //  SpecifyCalibrationsActionExecute(nil);
        //{$ENDIF}
      end;
    ttpsDoSettings:
      begin
        if (AnalysisType = ttaEP) or (AnalysisType = ttaCorrtest) then
          CheckBoxImages.GetBitmap(0, Check2.Picture.Bitmap)
        else
          CheckBoxImages.GetBitmap(0, Check3.Picture.Bitmap);
        ChooseSettingsLabel.Font.Color := FActiveFontColor;
        SpecifyOutgroupTaxaAction.Enabled := CanSetOutgroup;
        SpecifyOutgroupTEAction.Enabled := CanSetOutgroup;
        AnalysisPreferencesAction.Enabled := True;
        GetDefaultImages.GetBitmap(6, AnalysisOptionsBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to select analysis settings';
      end;
    ttpsLaunchAnalysis:
      begin
        CheckBoxImages.GetBitmap(0, Check4.Picture.Bitmap);
        SpecifyOutgroupTaxaAction.Enabled := CanSetOutgroup;
        SpecifyOutgroupTEAction.Enabled := CanSetOutgroup;
        LaunchAnalysisAction.Enabled := True;
        GetDefaultImages.GetBitmap(2, LaunchBtn.Picture.Bitmap);
        Self.BringToFront;
        StatusBar1.SimpleText := 'Ready to launch analysis';
      end;
  end;
  {$IFDEF DARWIN}
  if Visible then
  begin
    BringToFront;
    SetFocus;
    if Cursor <> crDefault then
      Cursor := crDefault;
  end;
  {$ENDIF}
end;

procedure TTimeTreeWizardForm.AnalysisPreferencesActionExecute(Sender: TObject);
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

function TTimeTreeWizardForm.AnalysisType: TTimeTreeAnalysis;
begin
  if Assigned(Wizard) then
    Result := Wizard.AnalysisType
  else
    Result := ttaReltime; { default}
end;

procedure TTimeTreeWizardForm.ApplyUpdatesForAnalysisType;
begin
  case AnalysisType of
    ttaLbsInference: UpdateForLbsInference;
    ttaLbsTreeTiming: UpdateForLbsUserTreeTiming;
    ttaReltime: UpdateForReltime;
    ttaCorrtest: UpdateForCorrtest;
    ttaEP: UpdateForEp;
  end;
end;

procedure TTimeTreeWizardForm.ApplyUpdatesForWizardMode;
begin
  case WizardMode of
    ttwmSeqData: UpdateForSeqDataMode;
    ttwmBlens: UpdateForBlensMode;
  end;
end;

procedure TTimeTreeWizardForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  { only let the user close the window via the QuitAction, so we can notify listeners that the user has cancelled}
//  if Sender = QuitAction then
//    CanClose := True
//  else
//    CanClose := False;
end;

procedure TTimeTreeWizardForm.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  DoubleBuffered := True;
  HeaderLabel.Font.Color := clGrayText;
  StatusBar1.Font.Size := 8;
  {$IFNDEF DEBUG}
  DeveloperAction.Enabled := False;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Timetree Wizard';
end;

procedure TTimeTreeWizardForm.FormShow(Sender: TObject);
begin
  InitForm;
  ResetAction.Visible := (Wizard.LaunchedFrom <> ttsTreeExplorer);
end;

procedure TTimeTreeWizardForm.SpecifyOutgroupTEBtnMouseEnter(Sender: TObject);
begin
  if not SpecifyOutgroupTEAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(4, SpecifyOutgroupTEBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.SpecifyOutgroupTEBtnMouseLeave(Sender: TObject);
begin
  if not SpecifyOutgroupTEAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(4, SpecifyOutgroupTEBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.InitLabelArray;
begin
  SetLength(FLabels, 5);
  FLabels[0] := LoadSeqDataLabel;
  FLabels[1] := LoadTreeLabel;
  FLabels[2] := SpecifyOutgroupLabel;
  FLabels[3] := SpecifyCalibrationsLabel;
  FLabels[4] := ChooseSettingsLabel;
end;

procedure TTimeTreeWizardForm.UpdateForBlensMode;
var
  tempHeight: Integer;
begin
  tempHeight := LaunchPanel.Height;
  LoadSeqDataPanel.Visible := False;
  LoadSeqDataAction.Enabled := False;
  case AnalysisType of
    ttaReltime, ttaCorrtest:
      begin
        ChooseSettingsLabel.Caption := 'Set Max Rate Ratio';
        SettingsHelpBtn.Hint := 'Constrains the maximum relative rate ratio among lineages';
      end;
  end;
  ClientHeight := (HeaderPanel.Height + LoadTreePanel.Height + OutgroupPanel.Height + CalibrationsPanel.Height + AnalysisSettingsPanel.Height + tempHeight + 20)
end;

procedure TTimeTreeWizardForm.UpdateForCorrtest;
var
  reduceBy: Integer;
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Rate Correlation Test';
  HeaderLabel.Caption := 'Rate Correlation Test Wizard';
  LoadDataHelpBtn.Hint := 'The MEGA or Fasta formatted sequence alignment file which was used to infer the phylogeny to be tested';
  LoadTreeHelpBtn.Hint := 'The Newick formatted file which contains the phylogeny to be tested';

  reduceBy := CalibrationsPanel.Height;
  CalibrationsPanel.Visible := False;
  ClientHeight := ClientHeight - reduceBy;
  HelpContext := HC_CORRTEST;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TTimeTreeWizardForm.UpdateForEp;
var
  reduceBy: Integer;
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Evolutionary Probabilities';
  HeaderLabel.Caption := 'EP Calculation Wizard';
  LoadDataHelpBtn.Hint := 'The MEGA or Fasta formatted sequence alignment file which was used to infer the phylogeny to be tested';
  LoadTreeHelpBtn.Hint := 'The Newick formatted file which contains the phylogeny to be tested';
  reduceBy := CalibrationsPanel.Height;
  CalibrationsPanel.Visible := False;
  ClientHeight := ClientHeight - reduceBy;
  HelpContext := HC_EVOLUTIONARY_PROBABILITY;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);

  LaunchPanel.Align := alNone;
  AnalysisSettingsPanel.Align := alNone;
  OutgroupPanel.Align := alNone;
  AnalysisSettingsPanel.Align := alTop;
  AnalysisSettingsPanel.Top := LoadTreePanel.Top + LoadTreePanel.Height + 1;
  OutgroupPanel.Align := alTop;
  OutgroupPanel.Top := AnalysisSettingsPanel.Top + AnalysisSettingsPanel.Height + 1;
  LaunchPanel.Align := alClient;
end;

procedure TTimeTreeWizardForm.UpdateForLbsInference;
var
  reduceBy: Integer;
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': LBS Tree Search';
  HeaderLabel.Caption := 'Little Bootstraps Wizard';
  LoadDataHelpBtn.Hint := 'The MEGA or Fasta formatted sequence alignment file which will be used to infer the phylogeny to be tested';
  reduceBy := LoadTreePanel.Height;
  LoadTreePanel.Visible := False;
  HideTreeExplorerOutgroupButton;
  ClientHeight := ClientHeight - reduceBy;
  HelpContext := HC_Maximum_Likelihood_ML_;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TTimeTreeWizardForm.UpdateForLbsUserTreeTiming;
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': LBS Timing';
  HeaderLabel.Caption := 'Little Bootstraps Wizard';
  LoadDataHelpBtn.Hint := 'The MEGA or Fasta formatted sequence alignment file which will be used to infer the phylogeny to be tested';
  LoadTreeHelpBtn.Hint := 'The Newick formatted file which contains the phylogeny to be analyzed';
  HideTreeExplorerOutgroupButton;
  HelpContext := HC_Maximum_Likelihood_ML_;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TTimeTreeWizardForm.InitImages;
begin
  GetDisabledImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
  GetDisabledImages.GetBitmap(1, LoadTreeBtn.Picture.Bitmap);
  GetDisabledImages.GetBitmap(4, SpecifyOutgroupTEBtn.Picture.Bitmap);
  GetDisabledImages.GetBitmap(5, SpecifyOutgroupBtn.Picture.Bitmap);
  GetDisabledImages.GetBitmap(0, SpecifyCalibrationsBtn.Picture.Bitmap);
  GetDisabledImages.GetBitmap(6, AnalysisOptionsBtn.Picture.Bitmap);
  GetDisabledImages.GetBitmap(2, LaunchBtn.Picture.Bitmap);
  GetSkipImages.GetBitmap(2, SkipBtn.Picture.Bitmap);
  GetHelpImages.GetBitmap(0, LoadDataHelpBtn.Picture.Bitmap);
  GetHelpImages.GetBitmap(0, LoadTreeHelpBtn.Picture.Bitmap);
  GetHelpImages.GetBitmap(0, SpecifyOutgroupHelpBtn.Picture.Bitmap);
  GetHelpImages.GetBitmap(0, SpecifyCalibrationsHelpBtn.Picture.Bitmap);
  GetHelpImages.GetBitmap(0, SettingsHelpBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.HideTreeExplorerOutgroupButton;
begin
  SpecifyOutgroupTEBtn.Visible := False;
  SpecifyOutgroupBtn.Left := LoadSeqDataBtn.Left;
  SpecifyOutgroupHelpBtn.Left := LoadDataHelpBtn.Left;
  Check2.Left := SpecifyOutgroupBtn.Left + SpecifyOutgroupBtn.Width + COMPONENTS_SPACING;
  SpecifyOutgroupHelpBtn.Hint := 'Little Bootstrap timing requires that one or more be designated as representing the outgroup';
end;

function TTimeTreeWizardForm.GetDisabledImages: TImageList;
begin
  if MegaForm.UseLargeIconSize then
    Result := DisabledButtons2
  else
    Result := DisabledButtons;
end;

function TTimeTreeWizardForm.GetDefaultImages: TImageList;
begin
  if MegaForm.UseLargeIconSize then
    Result := DefaultButtons2
  else
    Result := DefaultButtons;
end;

function TTimeTreeWizardForm.GetHoverImages: TImageList;
begin
  if MegaForm.UseLargeIconSize then
    Result := HoverButtons2
  else
    Result := HoverButtons;
end;

function TTimeTreeWizardForm.GetHelpImages: TImageList;
begin
  if MegaForm.UseLargeIconSize then
    Result := HelpButtons2
  else
    Result := HelpButtons;
end;

function TTimeTreeWizardForm.GetSkipImages: TImageList;
begin
  if MegaForm.UseLargeIconSize then
    Result := SkipButtons2
  else
    Result := SkipButtons;
end;

procedure TTimeTreeWizardForm.UpdateForHyphy;
var
  reduceBy: Integer;
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Datamonkey';
  HeaderLabel.Caption := 'Codon-by-Codon Selection (Datamonkey)';
  HeaderLabel.Height:=HeaderLabel.Canvas.TextHeight('Codon-by-Codon Selection (Datamonkey)');
  LoadDataHelpBtn.Hint := 'The MEGA or Fasta formatted sequence alignment file to infer dN and dS substitution rates';
  reduceBy := CalibrationsPanel.Height + OutgroupPanel.Height + LoadTreePanel.Height;
  CalibrationsPanel.Visible := False;
  OutgroupPanel.Visible := False;
  LoadTreePanel.Visible := False;
  LoadTreePanel.Enabled:=False;
  ClientHeight := ClientHeight - reduceBy;
  AdvanceToStep;
end;

procedure TTimeTreeWizardForm.UpdateForReltime;
begin
  if Assigned(Wizard) then
  begin
    if Wizard.CalibrationType = ctLeafNode then
    begin
      Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Tip Dating';
      HeaderLabel.Caption := 'Tip Dating Wizard';
      SpecifyCalibrationsLabel.Caption := 'Add Tip Dates';
    end
    else if Wizard.CalibrationType = ctInternalNode then
    begin
      Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Divergence Timing';
      HeaderLabel.Caption := 'Timetree Wizard';
      SpecifyCalibrationsLabel.Caption := 'Calibrate Nodes';
    end;
  end;
end;

procedure TTimeTreeWizardForm.UpdateForSeqDataMode;
begin
  if (AnalysisType = ttaCorrtest) or (AnalysisType = ttaEP) then
  begin
    ChooseSettingsLabel.Caption := 'Choose Analysis Settings';
  end;
end;

procedure TTimeTreeWizardForm.UpdateView(Step: TTtProcessStep);
begin
  SetProcessStep(Step);
end;

function TTimeTreeWizardForm.WizardMode: TTimeTreeWizardMode;
begin
  if Assigned(Wizard) then
    Result := Wizard.WizardMode
  else
    Result := ttwmSeqData; { be default}
end;

procedure TTimeTreeWizardForm.InitActionArray;
begin
  SetLength(FActions, 8);
  FActions[0] := LoadTreeAction;
  FActions[1] := SpecifyOutgroupTaxaAction;
  FActions[2] := LoadSeqDataAction;
  FActions[3] := SpecifyCalibrationsAction;
  FActions[4] := AnalysisPreferencesAction;
  FActions[5] := SkipAction;
  FActions[6] := LaunchAnalysisAction;
  FActions[7] := SpecifyOutgroupTEAction;
end;

procedure TTimeTreeWizardForm.InitButtonArray;
begin
  SetLength(FButtons, 8);
  FButtons[0] := LoadTreeBtn;
  FButtons[1] := LoadSeqDataBtn;
  FButtons[2] := SpecifyOutgroupBtn;
  FButtons[3] := SpecifyCalibrationsBtn;
  FButtons[4] := SkipBtn;
  FButtons[5] := AnalysisOptionsBtn;
  FButtons[6] := LaunchBtn;
  FButtons[7] := SpecifyOutgroupTEBtn;
end;

procedure TTimeTreeWizardForm.InitForm;
begin
  HelpContext := HC_TIMETREE_WIZARD;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  FCalibrationsSkipped := False;
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
  ApplyUpdatesForWizardMode;
  case WizardMode of
    ttwmSeqData:
      begin
        if not MegaForm.HasSequenceData then
          FProcessStep := ttpsLoadSeqData
        else
        begin
          if Wizard.UsrOperation = dtdoLbsInference then
            FProcessStep := ttpsDoOutgroup
          else
            FProcessStep := ttpsLoadTree;
        end;
      end;
    ttwmBlens:
      begin
        FProcessStep := ttpsLoadTree;
      end;
  end;
  ApplyUpdatesForAnalysisType;
  AdvanceToStep;
  if MegaForm.UseLargeIconSize then
  begin

  end;
end;

procedure TTimeTreeWizardForm.InitFontColors;
begin
  Font.Color := FDefaultFontColor;
end;

procedure TTimeTreeWizardForm.InitPanelArray;
var
  i: Integer;
begin
  SetLength(FPanels, 6);
  FPanels[0] := LoadTreePanel;
  FPanels[1] := LoadSeqDataPanel;
  FPanels[2] := OutgroupPanel;
  FPanels[3] := CalibrationsPanel;
  FPanels[4] := AnalysisSettingsPanel;
  FPanels[5] := LaunchPanel;
  for i := 0 to Length(FPanels) -1 do
  begin
    FPanels[i].ControlStyle := FPanels[i].ControlStyle - [csParentBackground] + [csOpaque];
  end;
end;

procedure TTimeTreeWizardForm.LaunchAnalysisActionExecute(Sender: TObject);
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

procedure TTimeTreeWizardForm.LoadSeqDataActionExecute(Sender: TObject);
begin
  if not LoadSeqDataAction.Enabled then
    Exit;
  try
    LoadSeqDataAction.Enabled := False;
    GetDisabledImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
    LoadSeqDataLabel.Font.Color := FDefaultFontColor;
    Wizard.LoadSeqData;
  Except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTimeTreeWizardForm.LoadTreeBtnMouseEnter(Sender: TObject);
begin
  if not LoadTreeAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(1, LoadTreeBtn.Picture.Bitmap);
  LoadTreeBtn.Invalidate;
end;

procedure TTimeTreeWizardForm.LoadTreeBtnMouseLeave(Sender: TObject);
begin
  if not LoadTreeAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(1, LoadTreeBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.ResetLabelMouseEnter(Sender: TObject);
begin
  ResetLabel.Font.Color := FActiveFontColor;
  ResetLabel.Invalidate;
end;

procedure TTimeTreeWizardForm.ResetLabelMouseLeave(Sender: TObject);
begin
  ResetLabel.Font.Color := clGrayText;
  ResetLabel.Invalidate;
end;

procedure TTimeTreeWizardForm.SkipBtnMouseEnter(Sender: TObject);
begin
  if not SpecifyCalibrationsAction.Enabled then
    Exit;
  GetSkipImages.GetBitmap(1, SkipBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.SkipBtnMouseLeave(Sender: TObject);
begin
  if not SpecifyCalibrationsAction.Enabled then
    Exit;
  GetSkipImages.GetBitmap(0, SkipBtn.Picture.Bitmap)
end;

procedure TTimeTreeWizardForm.SpecifyCalibrationsBtnMouseEnter(Sender: TObject);
begin
  if not SpecifyCalibrationsAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(0, SpecifyCalibrationsBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.SpecifyCalibrationsBtnMouseLeave(Sender: TObject);
begin
  if not SpecifyCalibrationsAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(0, SpecifyCalibrationsBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.SpecifyOutgroupBtnMouseEnter(Sender: TObject);
begin
  if not SpecifyOutgroupTaxaAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(5, SpecifyOutgroupBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.SpecifyOutgroupBtnMouseLeave(Sender: TObject);
begin
  if not SpecifyOutgroupTaxaAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(5, SpecifyOutgroupBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.LoadTreeActionExecute(Sender: TObject);
begin
  if not LoadTreeAction.Enabled then
    Exit;
  try
    LoadTreeAction.Enabled := False;
    GetDisabledImages.GetBitmap(1, LoadTreeBtn.Picture.Bitmap);
    LoadTreeLabel.Font.Color := FDefaultFontColor;
    Wizard.LoadTree;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TTimeTreeWizardForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TTimeTreeWizardForm.CancelLabelMouseEnter(Sender: TObject);
begin
  CancelLabel.Font.Color := FActiveFontColor;
  CancelLabel.Invalidate;
end;

procedure TTimeTreeWizardForm.AnalysisOptionsBtnMouseEnter(Sender: TObject);
begin
  if not AnalysisPreferencesAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(6, AnalysisOptionsBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.AnalysisOptionsBtnMouseLeave(Sender: TObject);
begin
  if not AnalysisPreferencesAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(6, AnalysisOptionsBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.CancelLabelMouseLeave(Sender: TObject);
begin
  CancelLabel.Font.Color := clGrayText;
  CancelLabel.Invalidate;
end;

procedure TTimeTreeWizardForm.DeveloperActionExecute(Sender: TObject);
begin
  HighlightAllPanels;
  //case FProcessStep of
  //  ttpsLoadTree:
      begin
        LoadTreeLabel.Font.Color := FActiveFontColor;
        LoadTreeAction.Enabled := True;
        GetDefaultImages.GetBitmap(1, LoadTreeBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to load a tree file';
        if LoadSeqDataAction.Enabled then
        begin
          GetDefaultImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
          LoadSeqDataLabel.Font.Color := FDefaultFontColor;
        end;
      end;
    //ttpsLoadSeqData:
      begin
        LoadSeqDataLabel.Font.Color := FActiveFontColor;
        LoadSeqDataAction.Enabled := True;
        GetDefaultImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to load a sequence data file';
      end;
    //ttpsDoOutgroup:
      begin
        SpecifyOutgroupLabel.Font.Color := FActiveFontColor;
        SpecifyOutgroupTaxaAction.Enabled := True;
        SpecifyOutgroupTEAction.Enabled := True;
        GetDefaultImages.GetBitmap(4, SpecifyOutgroupTEBtn.Picture.Bitmap);
        GetDefaultImages.GetBitmap(5, SpecifyOutgroupBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to select the outgroup taxa';
      end;
    //ttpsDoCalibrations:
      begin
        SpecifyCalibrationsLabel.Font.Color := FActiveFontColor;
        SpecifyCalibrationsAction.Enabled := True;
        GetDefaultImages.GetBitmap(0, SpecifyCalibrationsBtn.Picture.Bitmap);
        SpecifyOutgroupTaxaAction.Enabled := CanSetOutgroup;
        SpecifyOutgroupTEAction.Enabled := CanSetOutgroup;
        SkipAction.Enabled := True;
        GetSkipImages.GetBitmap(0, SkipBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to specify divergence time calibration constraints';
      end;
    //ttpsDoSettings:
      begin
        ChooseSettingsLabel.Font.Color := FActiveFontColor;
        SpecifyOutgroupTaxaAction.Enabled := CanSetOutgroup;
        SpecifyOutgroupTEAction.Enabled := CanSetOutgroup;
        AnalysisPreferencesAction.Enabled := True;
        GetDefaultImages.GetBitmap(6, AnalysisOptionsBtn.Picture.Bitmap);
        StatusBar1.SimpleText := 'Ready to select analysis settings';
      end;
    //ttpsLaunchAnalysis:
      begin
        SpecifyOutgroupTaxaAction.Enabled := CanSetOutgroup;
        SpecifyOutgroupTEAction.Enabled := CanSetOutgroup;
        LaunchAnalysisAction.Enabled := True;
        GetDefaultImages.GetBitmap(2, LaunchBtn.Picture.Bitmap);
        Self.BringToFront;
      end;
      StatusBar1.SimpleText := 'Ready';
  //end;
end;

procedure TTimeTreeWizardForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not MegaForm.HasSequenceData) and (MegaForm.CloseDataBtn.Visible) then
    MegaForm.CloseDataBtn.Hide;
  CloseAction := caFree;
end;

procedure TTimeTreeWizardForm.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TTimeTreeWizardForm.HelpLabelMouseEnter(Sender: TObject);
begin
  HelpLabel.Font.Color := FActiveFontColor;
  HelpLabel.Invalidate;
end;

procedure TTimeTreeWizardForm.HelpLabelMouseLeave(Sender: TObject);
begin
  HelpLabel.Font.Color := clGrayText;
end;

procedure TTimeTreeWizardForm.LaunchBtnMouseEnter(Sender: TObject);
begin
  if not LaunchAnalysisAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(2, LaunchBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.LaunchBtnMouseLeave(Sender: TObject);
begin
  if not LaunchAnalysisAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(2, LaunchBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.LoadDataHelpBtnClick(Sender: TObject);
var
  cursorPos: TPoint;
  aPos: TPoint;
begin
  aPos := Point(1, 1);
  CursorPos := LoadDataHelpBtn.ClientToScreen(aPos);
  Application.ActivateHint(CursorPos);
end;

procedure TTimeTreeWizardForm.HelpBtnMouseEnter(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  GetHelpImages.GetBitmap(1, aImage.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.HelpBtnMouseLeave(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  GetHelpImages.GetBitmap(0, aImage.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.LoadSeqDataBtnMouseEnter(Sender: TObject);
begin
  if not LoadSeqDataAction.Enabled then
    Exit;
  GetHoverImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.LoadSeqDataBtnMouseLeave(Sender: TObject);
begin
  if not LoadSeqDataAction.Enabled then
    Exit;
  GetDefaultImages.GetBitmap(1, LoadSeqDataBtn.Picture.Bitmap);
end;

procedure TTimeTreeWizardForm.QuitActionExecute(Sender: TObject);
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

procedure TTimeTreeWizardForm.ResetActionExecute(Sender: TObject);
begin
  if WizardMode = ttwmSeqData then
    MegaForm.PromptCloseCurrentlyActive;
  if MegaForm.HasActiveData or (WizardMode = ttwmBlens) then
  begin
    UpdateView(ttpsLoadTree);
    if Assigned(Wizard) then
      Wizard.Reset(ttpsLoadTree);
  end
  else
  begin
    UpdateView(ttpsLoadSeqData);
    if Assigned(Wizard) then
      Wizard.Reset(ttpsLoadSeqData);
  end;
  CheckBoxImages.GetBitmap(1, Check.Picture.Bitmap);
  CheckBoxImages.GetBitmap(1, Check1.Picture.Bitmap);
  CheckBoxImages.GetBitmap(1, Check2.Picture.Bitmap);
  CheckBoxImages.GetBitmap(1, Check3.Picture.Bitmap);
  CheckBoxImages.GetBitmap(1, Check4.Picture.Bitmap);
  Invalidate;
end;

procedure TTimeTreeWizardForm.SetProcessStep(const Value: TTtProcessStep);
begin
  if Value = ttpsCancelAnalysis then
  begin
    QuitActionExecute(Self);
    Exit;
  end;

  if Value = ttpsDisableOutgroup then
  begin
    OutgroupPanel.Enabled := False;
    Exit;
  end;

  FProcessStep := Value;
  AdvanceToStep;
end;

function TTimeTreeWizardForm.CanSetOutgroup: Boolean;
begin
  Result := True;
  if (Wizard.LaunchedFrom = ttsTreeExplorer) and (Wizard.OutgroupIsDefined) then
    Result := False;
end;

procedure TTimeTreeWizardForm.SkipActionExecute(Sender: TObject);
begin
  if not SkipAction.Enabled then
    Exit;
  begin
    FCalibrationsSkipped := True;
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

procedure TTimeTreeWizardForm.SpecifyCalibrationsActionExecute(Sender: TObject);
begin
  if not SpecifyCalibrationsAction.Enabled then
    Exit;
  try
    SpecifyCalibrationsAction.Enabled := False;
    SpecifyOutgroupTaxaAction.Enabled := False;
    SpecifyOutgroupTEAction.Enabled := False;
    Wizard.GetCalibrations;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTimeTreeWizardForm.SpecifyOutgroupTaxaActionExecute(Sender: TObject);
begin
  if not SpecifyOutgroupTaxaAction.Enabled then
    Exit;
  try
    SpecifyOutgroupTaxaAction.Enabled := False;
    SpecifyOutgroupTEAction.Enabled := False;
    if Wizard.WizardMode = ttwmSeqData then
      Wizard.SetOutgroupViaTaxaGpsDlg
    else
      Wizard.SetOutgroupViaSelectOutgroupDlg;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTimeTreeWizardForm.SpecifyOutgroupTEActionExecute(Sender: TObject);
begin
  if not SpecifyOutgroupTEAction.Enabled then
    Exit;
  try
    SpecifyOutgroupTaxaAction.Enabled := False;
    SpecifyOutgroupTEAction.Enabled := False;
    Wizard.SetOutgroupViaTE;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTimeTreeWizardForm.UnHighlightAllPanels;
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

procedure TTimeTreeWizardForm.HighlightAllPanels;
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
  //InitImages;
end;

end.
