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

unit ClustalOptionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, StdCtrls, Spin , ActnList, MProcessPack, MClustalW;

type

  { TClustalSetupDlg }

  TClustalSetupDlg = class(TForm)
    ResetAction: TAction;
    DivergentCutoffNEdit: TSpinEdit;
    DNAGapExtendNEdit: TFloatSpinEdit;
    ToolButton4: TToolButton;
    TransitionWeightNEdit: TFloatSpinEdit;
    DNAGapOpenNEdit: TFloatSpinEdit;
    DNAMatrixCBx: TComboBox;
    DNAPWGapExtendNEdit: TFloatSpinEdit;
    DNAPWGapOpenNEdit: TFloatSpinEdit;
    KeepGapsCBx: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MatrixSettingsPanel: TPanel;
    OkAction : TAction ;
    CancelAction : TAction ;
    HelpAction : TAction ;
    ActionList1 : TActionList ;
    OpenGuideTreeDlg: TOpenDialog;
    DnaAlignmentPanel: TPanel;
    DnaAlignmentHeader: TPanel;
    AlignmentControlsPanel: TPanel;
    ProteinAlignmentPanel: TPanel;
    Panel3: TPanel;
    DnaMatrixHeader: TPanel;
    ProteinAlignmentHeader: TPanel;
    ProteinMatrixHeader: TPanel;
    ProteinGapExtendNEdit: TFloatSpinEdit;
    ProteinGapOpenNEdit: TFloatSpinEdit;
    ProteinPWGapExtendNEdit: TFloatSpinEdit;
    ProteinPWGapOpenNEdit: TFloatSpinEdit;
    ResidueSpecificCBx: TComboBox;
    HidrophilicCBx: TComboBox;
    EndGapSeparationCBx: TComboBox;
    GeneticCodeCombo: TComboBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    GeneticCodeLabel: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    ProteinMatrixCBx: TComboBox;
    ProteinMatrixScrollBox: TScrollBox;
    DnaMatrixScrollBox: TScrollBox;
    DNATabSheet: TTabSheet;
    ProteinTabSheet: TTabSheet;
    GapSeparationDistNEdit: TSpinEdit;
    SelectedGuideTreeEdit: TEdit;
    SelectGuideTreeBtn: TButton;
    ToolBar1 : TToolBar ;
    ToolButton1 : TToolButton ;
    ToolButton2 : TToolButton ;
    ToolButton3 : TToolButton ;
    PageControl1: TPageControl;
    BottomPanel: TPanel;
    UseNegativeCBx: TComboBox;
    procedure CancelActionExecute (Sender : TObject );
    procedure DnaMatrixScrollBoxClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GeneticCodeComboDropDown(Sender: TObject);
    procedure HelpActionExecute (Sender : TObject );
    procedure OkActionExecute (Sender : TObject );
    procedure ResetActionExecute(Sender: TObject);
    procedure SelectGuideTreeBtnClick(Sender: TObject);
  private
    FcDNA: Boolean;
    PrBoolean :Boolean;
    { private declarations }
  public
    ClustalW: TClustalW;
    procedure UpdateGeneticCode;
    function constructProcessPack: TProcessPack;
    property IsUsingcDNA: Boolean read FcDNA write FcDNA;
    function spinRowMax(rowname:String) :double;
    function spinRowMin(rowname:String) :double;
    function SelectedGeneticCodeName: String;
    procedure SetIsDna(aValue: Boolean);
    { public declarations }
  end;

var
  ClustalSetupDlg: TClustalSetupDlg;

implementation

uses
  MAlignEditMainForm, MegaUtils, StringUtils, mhelpfiles, mhelpkeywords, AlnBuilder_HC,
  mimageform, math, MegaConsts;

{$R *.lfm}

{ TClustalSetupDlg }

procedure TClustalSetupDlg.GeneticCodeComboDropDown(Sender: TObject);
begin
  AlignEditMainForm.ActionGeneticCode.Execute;
  UpdateGeneticCode;
end;

procedure TClustalSetupDlg.HelpActionExecute (Sender : TObject );
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TClustalSetupDlg.CancelActionExecute (Sender : TObject );
begin
  ModalResult := mrCancel;
end;

procedure TClustalSetupDlg.DnaMatrixScrollBoxClick(Sender: TObject);
begin

end;

procedure TClustalSetupDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
  PageControl1.ShowTabs := False;
  DnaAlignmentHeader.Color := $00b1a216;
  DnaMatrixHeader.Color := $00b1a216;
  ProteinAlignmentHeader.Color := $00b1a216;
  ProteinMatrixHeader.Color := $00b1a216;
end;

procedure TClustalSetupDlg.FormCreate(Sender: TObject);
begin
  HelpContext := HC_CLUSTALW;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TClustalSetupDlg.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TClustalSetupDlg.OkActionExecute (Sender : TObject );
begin
  ModalResult := mrOK
end;

procedure TClustalSetupDlg.ResetActionExecute(Sender: TObject);
begin
  ClustalW.SetDefaults;
  with ClustalW do
  begin
    IsUsingcDNA := ClustalW.RequestUntranslate;
    DNAMatrixCBx.ItemIndex     := integer(DNAMatrix);
    ProteinMatrixCBx.ItemIndex := integer(ProteinMatrix);

    DNAPWGapOpenNEdit.Value   := DNAPWGapOpenPenalty;
    DNAPWGapExtendNEdit.Value := DNAPWGapExtendPenalty;
    DNAGapOpenNEdit.Value     := DNAGapOpenPenalty;
    DNAGapExtendNEdit.Value   := DNAGapExtendPenalty;
    ProteinPWGapOpenNEdit.Value   := ProteinPWGapOpenPenalty;
    ProteinPWGapExtendNEdit.Value := ProteinPWGapExtendPenalty;
    ProteinGapOpenNEdit.Value     := ProteinGapOpenPenalty;
    ProteinGapExtendNEdit.Value   := ProteinGapExtendPenalty;

    TransitionWeightNEdit.Value := TransitionWeight;

    DivergentCutoffNEdit.Value := DivergentCutoff;
    if UseNegativeMatrix then
      UseNegativeCBx.ItemIndex   := 0
    else
      UseNegativeCBx.ItemIndex   := 1;

    if ResidueSpecificPenalty then
      ResidueSpecificCBx.ItemIndex := 0
    else
      ResidueSpecificCBx.ItemIndex := 1;

    if HydrophilicPenalty then
      HidrophilicCBx.ItemIndex := 0
    else
      HidrophilicCBx.ItemIndex := 1;

    if EndGapSeparation then
      EndGapSeparationCBx.ItemIndex := 0
    else
      EndGapSeparationCBx.ItemIndex := 1;

    GapSeparationDistNEdit.Value := GapSeparationDistance;

    KeepGapsCBx.Checked := not ResetGaps;
  end;
end;

procedure TClustalSetupDlg.SelectGuideTreeBtnClick(Sender: TObject);
begin
  OpenGuideTreeDlg.FileName := SelectedGuideTreeEdit.Text;
  if OpenGuideTreeDlg.Execute then
  begin
    SelectedGuideTreeEdit.Text := OpenGuideTreeDlg.FileName;
  end
end;

procedure TClustalSetupDlg.UpdateGeneticCode;
begin
  if AlignEditMainForm.CurAlignGrid.CodeTableName = EmptyStr then
    AlignEditMainForm.CurAlignGrid.CodeTableName := 'Standard';
  GeneticCodeCombo.Items.Text := AlignEditMainForm.CurGeneticCodeName;
  GeneticCodeCombo.ItemIndex := 0;
end;

function TClustalSetupDlg.constructProcessPack: TProcessPack;
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;

  ProcessPack := TProcessPack.Create;

  ProcessPack.AddProcessType(ppAlign);
  ProcessPack.AddProcessType(ppClustalW);
  {$IFDEF ONLINE}
  UpdateClustalFromJSON;
  {$ENDIF}
  case PageControl1.ActivePageIndex of
    0:
    begin
      ProcessPack.AddKeyValuePairSetting('DNAPWGapOpeningPenalty', FloatToStr(DNAPWGapOpenNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('DNAPWGapExtensionPenalty', FloatToStr(DNAPWGapExtendNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('DNAMAGapOpeningPenalty', FloatToStr(DNAGapOpenNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('DNAMAGapExtensionPenalty', FloatToStr(DNAGapExtendNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('DNAWeightMatrix', DNAMatrixCBx.Text);
      ProcessPack.AddKeyValuePairSetting('TransitionWeightNEdit', FloatToStr(TransitionWeightNEdit.Value));
    end;
    1:
    begin
      ProcessPack.AddKeyValuePairSetting('ProteinPWGapOpeningPenalty', FloatToStr(ProteinPWGapOpenNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('ProteinPWGapExtensionPenalty', FloatToStr(ProteinPWGapExtendNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('ProteinMAGapOpeningPenalty', FloatToStr(ProteinGapOpenNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('ProteinMAGapExtensionPenalty', FloatToStr(ProteinGapExtendNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('ProteinWeightMatrix', ProteinMatrixCBx.Text);
      ProcessPack.AddKeyValuePairSetting('Residue-specificPenalties', ResidueSpecificCBx.Text);
      ProcessPack.AddKeyValuePairSetting('HydrophillicPenalties', HidrophilicCbx.Text);
      ProcessPack.AddKeyValuePairSetting('GapSeperationDistance', FloatToStr(GapSeparationDistNEdit.Value));
      ProcessPack.AddKeyValuePairSetting('EndGapSeperation', EndGapSeparationCBx.Text);
      if IsUsingcDNA or PrBoolean then
        ProcessPack.AddKeyValuePairSetting('GeneticCodeTable', AlignEditMainForm.CurGeneticCode);
      ProcessPack.containsCodingNuc := isUsingcDNA;
    end;
  end;
  ProcessPack.AddKeyValuePairSetting('UseNegativeMatrix', UseNegativeCBx.Text);
  ProcessPack.AddKeyValuePairSetting('DelayDivergentCutoff', FloatToStr(DivergentCutoffNEdit.Value));
  ProcessPack.AddKeyValuePairSetting('KeepPredefinedGaps', BoolToStr(KeepGapsCBx.checked));
  ProcessPack.AddKeyValuePairSetting('GuideTree', SelectedGuideTreeEdit.Text);
  result := ProcessPack;
end;

function TClustalSetupDlg.spinRowMax(rowname: String): double;
begin
  case StringToCaseSelect(rowName, ['Gap Opening Penalty 1', 'Gap Extension Penalty 1', '', 'Gap Opening Penalty 2', 'Gap Extension Penalty 2', 'Gap Separation Distance', 'Delay Divergence Cutoff(%)', 'Transition Weight' ]) of
  0: result := 100;
  1: result := 100;
  2: result := 100;
  3: result := 100;
  4: result := 100;
  5: result := 100;
  6: result := 100;
  7: result := 1;
  end;
end;

function TClustalSetupDlg.spinRowMin(rowname: String): double;
begin
  case StringToCaseSelect(rowName, ['Gap Opening Penalty 1', 'Gap Extension Penalty 1', '', 'Gap Opening Penalty 2', 'Gap Extension Penalty 2', 'Gap Separation Distance', 'Delay Divergence Cutoff(%)', 'Transition Weight' ]) of
  0: result := 0;
  1: result := 0;
  2: result := 0;
  3: result := 0;
  4: result := 0;
  5: result := 0;
  6: result := 0;
  7: result := 0;
  end;
end;

function TClustalSetupDlg.SelectedGeneticCodeName: String;
begin
  Result := GeneticCodeCombo.Items[GeneticCodeCombo.ItemIndex];
end;

procedure TClustalSetupDlg.SetIsDna(aValue: Boolean);
var
  MARGIN: Integer = 10;
  scalingFactor: Double = 0;
begin
  scalingFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scalingFactor, 1.25, FP_CUTOFF) > 0 then
    MARGIN := 20;
  if aValue then
  begin
    MatrixSettingsPanel.Parent := DnaMatrixScrollBox;
    MatrixSettingsPanel.Left := 0;
    MatrixSettingsPanel.Top := TransitionWeightNEdit.Top + TransitionWeightNEdit.Height + MARGIN;
    PageControl1.ActivePageIndex := 0;
    ClientHeight := BottomPanel.Height + MatrixSettingsPanel.Height + DnaAlignmentPanel.Height + SelectGuideTreeBtn.Top + SelectGuideTreeBtn.Height + MARGIN*2;
  end
  else
  begin
    MatrixSettingsPanel.Parent := ProteinMatrixScrollBox;
    MatrixSettingsPanel.Left := 0;
    if GeneticCodeCombo.Visible then
    begin
      MatrixSettingsPanel.Top := GeneticCodeCombo.Top + GeneticCodeCombo.Height + MARGIN;
      ClientHeight := BottomPanel.Height + MatrixSettingsPanel.Height + ProteinAlignmentPanel.Height + GeneticCodeCombo.Top + GeneticCodeCombo.Height + MARGIN*2;
    end
    else
    begin
      MatrixSettingsPanel.Top := EndGapSeparationCBx.Top + EndGapSeparationCBx.Height + MARGIN;
      ClientHeight := BottomPanel.Height + MatrixSettingsPanel.Height + ProteinAlignmentPanel.Height + EndGapSeparationCBx.Top + EndGapSeparationCBx.Height + MARGIN*2;
    end;
    PageControl1.ActivePageIndex := 1;
  end;
end;

end.

