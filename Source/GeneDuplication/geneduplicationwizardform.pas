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

unit GeneDuplicationWizardForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, ImgList, ActnList,
  ExtCtrls, StdCtrls, Buttons, StdActns, MGeneDuplicationWizard, MAnalysisInfo,
  mimageform;

type
  TGDProcessStep = (gdpsLoadGeneTree, gdpsLoadSpeciesTree, gdpsMapSpecies, gdpsRootGeneTree, gdpsRootSpeciesTree, gdpsLaunchAnalysis);
  TPanelArray = array of TPanel;
  TActionArray = array of TAction;
  TButtonArray = array of TBitBtn;
  TImageArray = array of TImage;
  TLabelArray = array of TLabel;
  TCancelledCallback = procedure of object;

  { TGDWizardForm }

  TGDWizardForm = class(TForm)
    HelpAction: TAction;
    HeaderPanel: TPanel;
    Image1: TImage;
    HoverButtons: TImageList;
    DisabledButtons: TImageList;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    HelpButtons: TImageList;
    SkipButtons: TImageList;
    LaunchAnalysisBtn: TImage;
    SkipRootSpTreeBtn: TImage;
    RootSpeciesTreeBtn: TImage;
    SkipRootGeneTreeBtn: TImage;
    RootGeneTreeBtn: TImage;
    SkipSpTreeBtn: TImage;
    LoadSpeciesTreeBtn: TImage;
    MapSpeciesNamesBtn: TImage;
    LoadGeneTreeBtn: TImage;
    SkipImages: TImageList;
    LoadGeneTreePanel: TPanel;
    MapSpeciesNamesPanel: TPanel;
    ActionList1: TActionList;
    DefaultButtons: TImageList;
    LoadSpeciesTreePanel: TPanel;
    LaunchAnalysisPanel: TPanel;
    LoadSpeciesTreeLabel: TLabel;
    MapSpeciesNamesLabel: TLabel;
    LoadGeneTreeLabel: TLabel;
    RootGeneTreePanel: TPanel;
    RootGeneTreeLabel: TLabel;
    HelpContents1: THelpContents;
    LoadGeneTreeAction: TAction;
    LoadSpeciesTreeAction: TAction;
    MapSpeciesAction: TAction;
    InferGeneDupsAction: TAction;
    RootGeneTreeAction: TAction;
    RootSpeciesTreeAction: TAction;
    RootSpeciesTreePanel: TPanel;
    RootSpeciesTreeLabel: TLabel;
    HeaderLabel: TLabel;
    CancelLabel: TStaticText;
    HelpLabel: TStaticText;
    Step1Img: TImage;
    Step2Img: TImage;
    Step3Img: TImage;
    Step4Img: TImage;
    Step5Img: TImage;
    CancelAction: TAction;
    procedure CancelLabelMouseEnter(Sender: TObject);
    procedure CancelLabelMouseLeave(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure HelpActionExecute(Sender: TObject);
    procedure HelpLabelMouseEnter(Sender: TObject);
    procedure HelpLabelMouseLeave(Sender: TObject);
    procedure LaunchAnalysisBtnMouseEnter(Sender: TObject);
    procedure LaunchAnalysisBtnMouseLeave(Sender: TObject);
    procedure LoadGeneTreeBtnMouseEnter(Sender: TObject);
    procedure LoadGeneTreeBtnMouseLeave(Sender: TObject);
    procedure LoadSpeciesTreeActionExecute(Sender: TObject);
    procedure InferGeneDupsActionExecute(Sender: TObject);
    procedure LoadSpeciesTreeBtnMouseEnter(Sender: TObject);
    procedure LoadSpeciesTreeBtnMouseLeave(Sender: TObject);
    procedure MapSpeciesNamesBtnMouseEnter(Sender: TObject);
    procedure MapSpeciesNamesBtnMouseLeave(Sender: TObject);
    procedure RootGeneTreeActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadGeneTreeActionExecute(Sender: TObject);
    procedure MapSpeciesActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RootGeneTreeBtnMouseEnter(Sender: TObject);
    procedure RootGeneTreeBtnMouseLeave(Sender: TObject);
    procedure RootSpeciesTreeBtnMouseEnter(Sender: TObject);
    procedure RootSpeciesTreeBtnMouseLeave(Sender: TObject);
    procedure SkipSpTreeBtnClick(Sender: TObject);
    procedure SkipRootGeneTreeBtnClick(Sender: TObject);
    procedure RootSpeciesTreeActionExecute(Sender: TObject);
    procedure SkipRootSpTreeBtnClick(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SkipSpTreeBtnMouseEnter(Sender: TObject);
    procedure SkipSpTreeBtnMouseLeave(Sender: TObject);
    procedure Step1ImgMouseEnter(Sender: TObject);
    procedure Step1ImgMouseLeave(Sender: TObject);
  private
    FWizard: TGeneDuplicationsWizard;
    FProcessStep: TGDProcessStep;
    FPanels: TPanelArray;
    FLabels: TLabelArray;
    FActions: TActionArray;
    FButtons: TImageArray;
    FCurrentStepFontColor: TColor;
    FDefaultFontColor: TColor;
    procedure InitFontColors;
    procedure InitPanelArray;
    procedure InitLabelArray;
    procedure InitActionArray;
    procedure InitButtonArray;
    procedure InitImages;
    procedure AdvanceToStep;
    procedure UnHighlightAllPanels;
    procedure InitForm;
    procedure SetProcessStep(const Value: TGDProcessStep);
  public
    CancelledCallback: TCancelledCallback;
    procedure InitFromTreeExplorer(AInfo: TAnalysisInfo; TreeExplorer: TObject); { for the case where the wizard is launched from the TreeExplorer}
    property ProcessStep: TGDProcessStep read FProcessStep write SetProcessStep;
    property Wizard: TGeneDuplicationsWizard read FWizard write FWizard;
  end;

var
  GDWizardForm: TGDWizardForm;

implementation

uses
  MProcessPack, ContextHelp_HC, mhelpfiles, mhelpkeywords;

{$R *.lfm}

procedure TGDWizardForm.AdvanceToStep;
begin
  try
    BeginFormUpdate;
    UnHighlightAllPanels;
    case FProcessStep of
      gdpsLoadGeneTree:
        begin
          LoadGeneTreeLabel.Font.Color := FCurrentStepFontColor;
          LoadGeneTreeAction.Enabled := True;
          DefaultButtons.GetBitmap(0, LoadGeneTreeBtn.Picture.Bitmap);
        end;
      gdpsLoadSpeciesTree:
        begin
          LoadSpeciesTreeLabel.Font.Color := FCurrentStepFontColor;
          LoadSpeciesTreeAction.Enabled := True;
          SkipSpTreeBtn.Enabled := True;
          DefaultButtons.GetBitmap(0, LoadSpeciesTreeBtn.Picture.Bitmap);
          SkipButtons.GetBitmap(0, SkipSpTreeBtn.Picture.Bitmap);
        end;
      gdpsMapSpecies:
        begin
          MapSpeciesNamesLabel.Font.Color := FCurrentStepFontColor;
          MapSpeciesAction.Enabled := True;
          DefaultButtons.GetBitmap(1, MapSpeciesNamesBtn.Picture.Bitmap);
        end;
      gdpsRootGeneTree:
        begin
          RootGeneTreeLabel.Font.Color := FCurrentStepFontColor;
          RootGeneTreeAction.Enabled := True;
          SkipRootGeneTreeBtn.Enabled := True;
          DefaultButtons.GetBitmap(2, RootGeneTreeBtn.Picture.Bitmap);
          SkipButtons.GetBitmap(0, SkipRootGeneTreeBtn.Picture.Bitmap);
        end;
      gdpsRootSpeciesTree:
        begin
          RootSpeciesTreeLabel.Font.Color := FCurrentStepFontColor;
          RootSpeciesTreeAction.Enabled := True;
          SkipRootSpTreeBtn.Enabled := True;
          DefaultButtons.GetBitmap(2, RootSpeciesTreeBtn.Picture.Bitmap);
          SkipButtons.GetBitmap(0, SkipRootSpTreeBtn.Picture.Bitmap);
        end;
      gdpsLaunchAnalysis:
        begin
          InferGeneDupsAction.Enabled := True;
          DefaultButtons.GetBitmap(3, LaunchAnalysisBtn.Picture.Bitmap);
        end;
    end;
    Invalidate;
  finally
    EndFormUpdate;
  end;
end;

procedure TGDWizardForm.CancelActionExecute(Sender: TObject);
begin
  if Assigned(CancelledCallback) then
    CancelledCallback
  else
    ModalResult := mrAbort;
end;

procedure TGDWizardForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TGDWizardForm.SkipSpTreeBtnMouseEnter(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  SkipButtons.GetBitmap(1, aImage.Picture.Bitmap);
end;

procedure TGDWizardForm.SkipSpTreeBtnMouseLeave(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  if not aImage.Enabled then
    Exit;
  SkipButtons.GetBitmap(0, aImage.Picture.Bitmap);
end;

procedure TGDWizardForm.Step1ImgMouseEnter(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  HelpButtons.GetBitmap(1, aImage.Picture.Bitmap);
end;

procedure TGDWizardForm.Step1ImgMouseLeave(Sender: TObject);
var
  aImage: TImage;
begin
  if not (Sender is TImage) then
    Exit;
  aImage := TImage(Sender);
  HelpButtons.GetBitmap(0, aImage.Picture.Bitmap);
end;

procedure TGDWizardForm.InitFontColors;
begin
  Font.Color := FDefaultFontColor;
end;

procedure TGDWizardForm.FormCreate(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  CancelledCallback := nil;
  FWizard := TGeneDuplicationsWizard.Create;
  FWizard.Owner := Self;
  ProcessPack := TProcessPack.Create;
  ProcessPack.AddProcessType(ppInferGeneDups);
  FWizard.ProcessPack := ProcessPack;
  InitForm;
  ImageForm.UpdateImgList(Self);
end;

procedure TGDWizardForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FWizard) then
    FWizard.Free;
end;

procedure TGDWizardForm.RootGeneTreeBtnMouseEnter(Sender: TObject);
begin
  if RootGeneTreeAction.Enabled then
    HoverButtons.GetBitmap(2, RootGeneTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.RootGeneTreeBtnMouseLeave(Sender: TObject);
begin
  if RootGeneTreeAction.Enabled then
    DefaultButtons.GetBitmap(2, RootGeneTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.RootSpeciesTreeBtnMouseEnter(Sender: TObject);
begin
  if RootSpeciesTreeAction.Enabled then
    HoverButtons.GetBitmap(2, RootSpeciesTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.RootSpeciesTreeBtnMouseLeave(Sender: TObject);
begin
  if RootSpeciesTreeAction.Enabled then
    HoverButtons.GetBitmap(2, RootSpeciesTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.InferGeneDupsActionExecute(Sender: TObject);
begin
  if not InferGeneDupsAction.Enabled then
    Exit;
  try
    Self.Visible := False;
    FWizard.LaunchGeneDuplicationThread;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred: ' + E.Message);
  end;
end;

procedure TGDWizardForm.LoadSpeciesTreeBtnMouseEnter(Sender: TObject);
begin
  if LoadSpeciesTreeAction.Enabled then
    HoverButtons.GetBitmap(0, LoadSpeciesTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.LoadSpeciesTreeBtnMouseLeave(Sender: TObject);
begin
  if LoadSpeciesTreeAction.Enabled then
    DefaultButtons.GetBitmap(0, LoadSpeciesTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.MapSpeciesNamesBtnMouseEnter(Sender: TObject);
begin
  if MapSpeciesAction.Enabled then
    HoverButtons.GetBitmap(1, MapSpeciesNameSBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.MapSpeciesNamesBtnMouseLeave(Sender: TObject);
begin
  if MapSpeciesAction.Enabled then
    DefaultButtons.GetBitmap(1, MapSpeciesNameSBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.InitActionArray;
begin
  SetLength(FActions, 6);
  FActions[0] := LoadGeneTreeAction;
  FActions[1] := MapSpeciesAction;
  FActions[2] := LoadSpeciesTreeAction;
  FActions[3] := RootGeneTreeAction;
  FActions[4] := RootSpeciesTreeAction;
  FActions[5] := InferGeneDupsAction;
end;

procedure TGDWizardForm.InitButtonArray;
begin
  SetLength(FButtons, 8);
  FButtons[0] := LoadGeneTreeBtn;
  FButtons[1] := LoadSpeciesTreeBtn;
  FButtons[2] := MapSpeciesNamesBtn;
  FButtons[3] := RootGeneTreeBtn;
  FButtons[4] := LaunchAnalysisBtn;
  FButtons[5] := SkipSpTreeBtn;
  FButtons[6] := SkipRootGeneTreeBtn;
  FButtons[7] := RootSpeciesTreeBtn;
end;

procedure TGDWizardForm.InitImages;
begin
  try
    BeginFormUpdate;
    DisabledButtons.GetBitmap(0, LoadGeneTreeBtn.Picture.Bitmap);
    DisabledButtons.GetBitmap(1, MapSpeciesNamesBtn.Picture.Bitmap);
    DisabledButtons.GetBitmap(0, LoadSpeciesTreeBtn.Picture.Bitmap);
    DisabledButtons.GetBitmap(2, RootGeneTreeBtn.Picture.Bitmap);
    DisabledButtons.GetBitmap(2, RootSpeciesTreeBtn.Picture.Bitmap);
    DisabledButtons.GetBitmap(3, LaunchAnalysisBtn.Picture.Bitmap);
    SkipButtons.GetBitmap(2, SkipRootGeneTreeBtn.Picture.Bitmap);
    SkipButtons.GetBitmap(2, SkipRootSpTreeBtn.Picture.Bitmap);
    SkipButtons.GetBitmap(2, SkipSpTreeBtn.Picture.Bitmap);
    Invalidate;
  finally
    EndFormUpdate;
  end;
end;


procedure TGDWizardForm.InitForm;
begin
  HelpContext := HC_GENEDUPS_WIZARD;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  InitPanelArray;
  InitLabelArray;
  InitActionArray;
  InitButtonArray;
  InitImages;
  FDefaultFontColor := RGB($c4, $c4, $c4);
  FCurrentStepFontColor := RGB(31, 156, 157);
  InitFontColors;
  UnHighlightAllPanels;
  FProcessStep := gdpsLoadGeneTree;
  AdvanceToStep;
end;

procedure TGDWizardForm.InitPanelArray;
var
  i: Integer;
begin
  SetLength(FPanels, 6);
  FPanels[0] := LoadGeneTreePanel;
  FPanels[1] := LoadSpeciesTreePanel;
  FPanels[2] := MapSpeciesNamesPanel;
  FPanels[3] := RootGeneTreePanel;
  FPanels[4] := LaunchAnalysisPanel;
  FPanels[5] := RootSpeciesTreePanel;
  for i := 0 to Length(FPanels) -1 do
    FPanels[i].ParentColor := False;
end;

procedure TGDWizardForm.InitLabelArray;
begin
  SetLength(FLabels, 5);
  FLabels[0] := LoadGeneTreeLabel;
  FLabels[1] := MapSpeciesNamesLabel;
  FLabels[2] := LoadSpeciesTreeLabel;
  FLabels[3] := RootGeneTreeLabel;
  FLabels[4] := RootSpeciesTreeLabel;
end;

procedure TGDWizardForm.LoadGeneTreeActionExecute(Sender: TObject);
begin
  if not LoadGeneTreeAction.Enabled then
    Exit;
  try
    LoadGeneTreeAction.Enabled := False;
    if FWizard.LoadGeneTree then
    begin
      FProcessStep := gdpsMapSpecies;
      AdvanceToStep;
    end;
  finally
    if FProcessStep = gdpsLoadGeneTree  then
      LoadGeneTreeAction.Enabled := True;
  end;
end;

procedure TGDWizardForm.LoadSpeciesTreeActionExecute(Sender: TObject);
begin
  if not LoadSpeciesTreeAction.Enabled then
    Exit;
  try
    LoadSpeciesTreeAction.Enabled := False;
    if FWizard.LoadSpeciesTree then
    begin
      if not FWizard.GeneTreeIsRooted then
        FProcessStep := gdpsRootGeneTree
      else if (FWizard.UsingSpeciesTree and ( not FWizard.SpeciesTreeIsRooted)) then
        FProcessStep := gdpsRootSpeciesTree
      else
        FProcessStep := gdpsLaunchAnalysis;
      AdvanceToStep;
    end;
  finally
    if FProcessStep = gdpsLoadSpeciesTree then
      LoadSpeciesTreeAction.Enabled := True;
  end;
end;

procedure TGDWizardForm.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TGDWizardForm.HelpLabelMouseEnter(Sender: TObject);
begin
  HelpLabel.Font.Color := FCurrentStepFontColor;
end;

procedure TGDWizardForm.HelpLabelMouseLeave(Sender: TObject);
begin
  HelpLabel.Font.Color := clGrayText;
end;

procedure TGDWizardForm.LaunchAnalysisBtnMouseEnter(Sender: TObject);
begin
  if InferGeneDupsAction.Enabled then
    HoverButtons.GetBitmap(3, LaunchAnalysisBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.LaunchAnalysisBtnMouseLeave(Sender: TObject);
begin
  if InferGeneDupsAction.Enabled then
    DefaultButtons.GetBitmap(3, LaunchAnalysisBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.LoadGeneTreeBtnMouseEnter(Sender: TObject);
begin
  if LoadGeneTreeAction.Enabled then
    HoverButtons.GetBitmap(0, LoadGeneTreeBtn.Picture.Bitmap);
end;

procedure TGDWizardForm.LoadGeneTreeBtnMouseLeave(Sender: TObject);
begin
  if LoadGeneTreeAction.Enabled then
    DefaultButtons.GetBitmap(0, LoadGeneTreeBtn.Picture.Bitmap);
end;

function TGDWizardForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TGDWizardForm.CancelLabelMouseEnter(Sender: TObject);
begin
  CancelLabel.Font.Color := FCurrentStepFontColor;
end;

procedure TGDWizardForm.CancelLabelMouseLeave(Sender: TObject);
begin
  CancelLabel.Font.Color := clGrayText;
end;

procedure TGDWizardForm.MapSpeciesActionExecute(Sender: TObject);
begin
  if not MapSpeciesAction.Enabled then
    Exit;
  try
    MapSpeciesAction.Enabled := False;
    if FWizard.MapSpeciesNames then
    begin
      FProcessStep := gdpsLoadSpeciesTree;
      AdvanceToStep;
    end;
  finally
    if FProcessStep = gdpsMapSpecies then
      MapSpeciesAction.Enabled := True;
  end;
end;

procedure TGDWizardForm.RootGeneTreeActionExecute(Sender: TObject);
begin
  if not RootGeneTreeAction.Enabled then
    Exit;
  try
    RootGeneTreeAction.Enabled := False;
    if FWizard.RootATree(True) then
    begin
      if FWizard.UsingSpeciesTree and (not FWizard.SpeciesTreeIsRooted) then
        FProcessStep := gdpsRootSpeciesTree
      else
        FProcessStep := gdpsLaunchAnalysis;
      AdvanceToStep;
    end;
  finally
    if FProcessStep = gdpsRootGeneTree then
      RootGeneTreeAction.Enabled := True;
  end;
end;

procedure TGDWizardForm.RootSpeciesTreeActionExecute(Sender: TObject);
begin
  if not RootSpeciesTreeAction.Enabled then
    Exit;
  try
    RootSpeciesTreeAction.Enabled := False;
    if FWizard.RootATree(False) then
    begin
      FProcessStep := gdpsLaunchAnalysis;
      AdvanceToStep;
    end;
  finally
    if FProcessStep = gdpsRootSpeciesTree then
      RootSpeciesTreeAction.Enabled := True;
  end;
end;

procedure TGDWizardForm.SetProcessStep(const Value: TGDProcessStep);
begin
  FProcessStep := Value;
  AdvanceToStep;
end;

procedure TGDWizardForm.SkipRootGeneTreeBtnClick(Sender: TObject);
begin
  if not RootGeneTreeAction.Enabled then
    Exit;
  FWizard.GeneTreeIsRooted := False;
  if FWizard.UsingSpeciesTree and (not FWizard.SpeciesTreeIsRooted) then
    FProcessStep := gdpsRootSpeciesTree
  else
    FProcessStep := gdpsLaunchAnalysis;
  AdvanceToStep;
end;

procedure TGDWizardForm.SkipRootSpTreeBtnClick(Sender: TObject);
begin
  if not RootSpeciesTreeAction.Enabled then
    Exit;
  FWizard.SpeciesTreeIsRooted := False;
  FProcessStep := gdpsLaunchAnalysis;
  AdvanceToStep;
end;

procedure TGDWizardForm.SkipSpTreeBtnClick(Sender: TObject);
begin
  if not LoadSpeciesTreeAction.Enabled then
    Exit;
  FWizard.UsingSpeciesTree := False;
  if not FWizard.GeneTreeIsRooted then
    FProcessStep := gdpsRootGeneTree
  else
    FProcessStep := gdpsLaunchAnalysis;
  AdvanceToStep;
end;

procedure TGDWizardForm.UnHighlightAllPanels;
var
  i: Integer;
begin
  try
    BeginFormUpdate;
    for i := 0 to Length(FLabels) - 1 do
      FLabels[i].Font.Color := FDefaultFontColor;

    for i := 0 to Length(FActions) - 1 do
      FActions[i].Enabled := False;
    SkipRootSpTreeBtn.Enabled := False;
    SkipRootGeneTreeBtn.Enabled := False;
    SkipSpTreeBtn.Enabled := False;
    InitImages;
    Invalidate;
  finally
    EndFormUpdate;
  end;
end;

procedure TGDWizardForm.InitFromTreeExplorer(AInfo: TAnalysisInfo; TreeExplorer: TObject);
begin
  FWizard.InitFromTreeExplorer(AInfo, TreeExplorer);
end;


end.
