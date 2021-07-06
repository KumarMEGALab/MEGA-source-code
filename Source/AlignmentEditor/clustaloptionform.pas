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

unit ClustalOptionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, StdCtrls, Spin, MProcessPack;

type

  { TClustalSetupDlg }

  TClustalSetupDlg = class(TForm)
    OpenGuideTreeDlg: TOpenDialog;
    ResidueSpecificCBx: TComboBox;
    HidrophilicCBx: TComboBox;
    EndGapSeparationCBx: TComboBox;
    GeneticCodeCombo: TComboBox;
    DNAMatrixCBx: TComboBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    GeneticCodeLabel: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ProteinGapExtendNEdit: TFloatSpinEdit;
    ProteinGapOpenNEdit: TFloatSpinEdit;
    ProteinMatrixCBx: TComboBox;
    ProteinPWGapExtendNEdit: TFloatSpinEdit;
    ProteinPWGapOpenNEdit: TFloatSpinEdit;
    DNAPWGBx: TGroupBox;
    DNAMultipleGBx: TGroupBox;
    DNAPWGapOpenNEdit: TFloatSpinEdit;
    DNAPWGapExtendNEdit: TFloatSpinEdit;
    DNAGapOpenNEdit: TFloatSpinEdit;
    DNAGapExtendNEdit: TFloatSpinEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    ProteinPWGrpBx: TGroupBox;
    ProteinMultipleGroupBx: TGroupBox;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SelectGuideTreeBtn: TButton;
    SelectedGuideTreeEdit: TEdit;
    KeepGapsCBx: TCheckBox;
    DNATabSheet: TTabSheet;
    ProteinTabSheet: TTabSheet;
    GapSeparationDistNEdit: TSpinEdit;
    TransitionWeightNEdit: TFloatSpinEdit;
    UseNegativeCBx: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    DivergentCutoffNEdit: TSpinEdit;
    procedure GeneticCodeComboDropDown(Sender: TObject);
    procedure SelectGuideTreeBtnClick(Sender: TObject);
  private
    FcDNA: Boolean;
    PrBoolean :Boolean;
    { private declarations }
  public
    procedure UpdateGeneticCode;
    function constructProcessPack: TProcessPack;
    property IsUsingcDNA: Boolean read FcDNA write FcDNA;
    function spinRowMax(rowname:String) :double;
    function spinRowMin(rowname:String) :double;
    function SelectedGeneticCodeName: String;
    { public declarations }
  end;

var
  ClustalSetupDlg: TClustalSetupDlg;

implementation

uses
  MAlignEditMainForm, MegaUtils, StringUtils;

{$R *.lfm}

{ TClustalSetupDlg }

procedure TClustalSetupDlg.GeneticCodeComboDropDown(Sender: TObject);
begin
  AlignEditMainForm.ActionGeneticCode.Execute;
  UpdateGeneticCode;
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

end.

