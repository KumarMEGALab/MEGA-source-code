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

unit mesl_options_dlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  Menus, ExtCtrls, Spin, EditBtn, StdCtrls, IniPropStorage, zipper, esl_linker,
  MAnalysisSummary;

const
  LEASTR = 'leastr';
  LOGISTIC = 'logistic';
  OL_LEASTR = 'ol_leastr';
  OL_LOGISTIC = 'ol_logistic';
  ESL_HELP_PAGE = 'https://github.com/kumarlabgit/MyESL';

type

  { TEslOptionsDlg }

  TEslOptionsDlg = class(TForm)
    AutoNameLengthLabel: TLabel;
    AutoNameLengthSpinEdit: TSpinEdit;
    AutoNameNodesCbox: TCheckBox;
    BilevelSparsityCheckbox: TCheckBox;
    DrpDnaCheckBox: TCheckBox;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    DrpProteinCheckBox: TCheckBox;
    DrpFastaDirectoryEdit: TDirectoryEdit;
    Label14: TLabel;
    Label16: TLabel;
    Label26: TLabel;
    LambdaMinSpinEdit: TFloatSpinEdit;
    Lambda2MinSpinEdit: TFloatSpinEdit;
    LambdaMaxSpinEdit: TFloatSpinEdit;
    Lambda2MaxSpinEdit: TFloatSpinEdit;
    LambdaStepSpinEdit: TFloatSpinEdit;
    Lambda2StepSpinEdit: TFloatSpinEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    NewickLabel: TLabel;
    OrLabel: TLabel;
    FixedSparsityParamsCheckbox: TRadioButton;
    IntervalSparsityParamsCheckbox: TRadioButton;
    Panel3: TPanel;
    UseFilesInDirectoryBtn: TRadioButton;
    NumSitesPerPartitionSpinEdit: TSpinEdit;
    UseGenesDomainsBtn: TRadioButton;
    PartitionActiveDataBtn: TRadioButton;
    ResponseFileEdit: TFileNameEdit;
    ResponseFileLabel: TLabel;
    CrossValidationSpinEdit: TSpinEdit;
    DrPhyloTab: TTabSheet;
    DrpNumTaxaSpinEdit: TSpinEdit;
    DrpNumGroupsSpinEdit: TSpinEdit;
    TreeFileEdit: TFileNameEdit;
    TreeFileLabel: TLabel;
    UnbalancedCheckbox: TCheckBox;
    CrossValidationCheckbox: TCheckBox;
    CladeSizeLowerBoundSEdit: TSpinEdit;
    CladeSizeLowerCbox: TCheckBox;
    CladeSizeUpperBoundSEdit: TSpinEdit;
    CladeSizeUpperCbox: TCheckBox;
    DownsampleBalanceCheckBox: TCheckBox;
    EmailEdit: TEdit;
    EnsembleCoverageCheckbox: TCheckBox;
    EnsembleCoverageSpinEdit: TSpinEdit;
    EnsemblePartsCheckbox: TCheckBox;
    EnsemblePartsSpinEdit: TSpinEdit;
    GenePenaltiesFilenameEdit: TFileNameEdit;
    Label1: TLabel;
    Label15: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    HiddenItemsPanel: TPanel;
    NodeListFileEdit: TFileNameEdit;
    SampleBalanceChBox: TCheckBox;
    SlepOptionsFilenameEdit: TFileNameEdit;
    DevAction: TAction;
    GeneDisplayCutoffFloatSpinEdit: TFloatSpinEdit;
    Label19: TLabel;
    Label20: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    GeneDisplayLimitSpinEdit: TSpinEdit;
    SmartSamplingCheckbox: TCheckBox;
    Label17: TLabel;
    Label4: TLabel;
    ProgressMemo: TMemo;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    SparsifyCheckbox: TCheckBox;
    Splitter1: TSplitter;
    Timer1: TTimer;
    IniPropStorage1: TIniPropStorage;
    Label9: TLabel;
    SmartSamplingSpinEdit: TSpinEdit;
    MethodCBox: TComboBox;
    FastaDirectoryEdit: TDirectoryEdit;
    LambdaSpinEdit: TFloatSpinEdit;
    Lambda2SpinEdit: TFloatSpinEdit;
    HelpAction: TAction;
    ImageList1: TImageList;
    Label3: TLabel;
    Panel1: TPanel;
    ResetDefaultsAction: TAction;
    LaunchAction: TAction;
    CloseAction: TAction;
    ActionList1: TActionList;
    PageControl1: TPageControl;
    TreeBasedTab: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    UpsampleBalanceCheckBox: TCheckBox;
    procedure CladeSizeLowerCboxChange(Sender: TObject);
    procedure CladeSizeUpperCboxChange(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure DevActionExecute(Sender: TObject);
    procedure DrpFastaDirectoryEditAcceptDirectory(Sender: TObject;
      var Value: String);
    procedure DrpFastaDirectoryEditChange(Sender: TObject);
    procedure EnsembleCoverageCheckboxChange(Sender: TObject);
    procedure EnsemblePartsCheckboxChange(Sender: TObject);
    procedure FixedSparsityParamsCheckboxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure LaunchActionExecute(Sender: TObject);
    procedure MethodCBoxChange(Sender: TObject);
    procedure NumSitesPerPartitionSpinEditChange(Sender: TObject);
    procedure NumSitesPerPartitionSpinEditEditingDone(Sender: TObject);
    procedure ResetDefaultsActionExecute(Sender: TObject);
    procedure SmartSamplingCheckboxChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UseGenesDomainsBtnChange(Sender: TObject);
  private
    FAlignmentLength: Integer;
    FActiveDataHasDomains: Boolean;
    FCladeSize: Integer;
    FIgnoreEvents: Boolean;
    FArchiveBaseDirectory: String;
    FFastaDirectoryName: String;
    FIsDrPhylo: Boolean;
    FNewickString: String;
    FNumTaxaInTargetClade: Integer;
    FNumTaxaInUserTree: Integer;
    FProgressInterval: Integer;
    FNumFiles: Integer;
    FNumZipped: Integer;
    FTargetNodeLabel: String;
    FZipper: TZipper;
    FArchiveFile: String;
    procedure UpdateAnalysisSummary(s: TAnalysisSummary);
    function GetDrPhyloCommandLineOptions: TStringList;
    function GetEslCommandLineOptions: TStringList;
    procedure SetActiveDataHasDomains(AValue: Boolean);
    procedure SetAlignmentLength(AValue: Integer);
    procedure SetIsDrPhylo(AValue: Boolean);
    procedure SetNumTaxaInTargetClade(AValue: Integer);
    function TempNewickFile: String;
    procedure UpdateFastaDirectoryName(aPath: String);
    procedure UpdateProgress(aMsg: String);
    function GetEmailAddress: String;
    function GetAlignmentListFileName: String;
    function GetInternalArchiveFileName(filename: String): String;
    procedure TreeBasedParamStrings(const aNewickFile: String; var aList: TStringList);
    procedure ResponseBasedParamStrings(var aList: TStringList);
    procedure GlobalParamStrings(var aList: TStringList);
    function ValidateTreeBasedOptions: Boolean;
    procedure LaunchEslAnalysis(isTreeBased: Boolean);
    procedure LaunchDrPhylo;
    procedure GenerateArchive(isTreeBased: Boolean; fastaFiles: TStringList);
    function ValidateResponseBasedOptions: Boolean;
    procedure OnZipProgress(Sender: TObject; const Pct: Double);
    procedure OnEndZipFile(Sender : TObject; Const Ratio : Double);
  public
    procedure SetDataType(aType: String);
    procedure SetActiveAlignmentEnabled(aValue: Boolean);
    procedure SetNewickString(AValue: String; numTaxa: Integer = 0);
    property NewickString: String read FNewickString;
    property EslCommandLineOptions: TStringList read GetEslCommandLineOptions;
    property DrPhyloCommandLineOptions: TStringList read GetDrPhyloCommandLineOptions;
    property TargetNodeLabel: String read FTargetNodeLabel write FTargetNodeLabel;
    property IsDrPhylo: Boolean read FIsDrPhylo write SetIsDrPhylo;
    property NumTaxaInTargetClade: Integer read FNumTaxaInTargetClade write SetNumTaxaInTargetClade;
    property ActiveDataHasDomains: Boolean read FActiveDataHasDomains write SetActiveDataHasDomains;
    property AlignmentLength: Integer write SetAlignmentLength;
    property CladeSize: Integer read FCladeSize write FCladeSize;
    property NumTaxaInUserTree: Integer read FNumTaxaInUserTree write FNumTaxaInUserTree;
  end;

var
  EslOptionsDlg: TEslOptionsDlg;

implementation

{$R *.lfm}

uses
  LazFileUtils, FileUtil, math, uMegaBrowser, mega_main,
  MEditorForm, MegaVerConsts, mimageform, MegaConsts, mruntimeprogressdlg,
  MAnalysisInfo, MProcessPack, MD_InputSeqData;

{ TEslOptionsDlg }

procedure TEslOptionsDlg.HelpActionExecute(Sender: TObject);
var
  aBrowser: TMegaBrowserFrm = nil;
begin
  aBrowser := CreateNewChromiumBrowserWindow(bmBrowser, MegaForm, False);
  aBrowser.GoToUrl(ESL_HELP_PAGE, 'ESL');
  aBrowser.Show;
end;

procedure TEslOptionsDlg.CloseActionExecute(Sender: TObject);
begin
  EslOptionsDlg := nil;
  Close;
end;

procedure TEslOptionsDlg.DevActionExecute(Sender: TObject);
var
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    TreeBasedParamStrings(EmptyStr, aList);
    GlobalParamStrings(aList);
    OpenStringList(aList, 'Tree-based params');
  finally
    if Assigned(aList) then
      aList.Free;
  end;

  try
    aList := TStringList.Create;
    ResponseBasedParamStrings(aList);
    GlobalParamStrings(aList);
    OpenStringList(aList, 'Response-based params');
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TEslOptionsDlg.DrpFastaDirectoryEditAcceptDirectory(Sender: TObject; var Value: String);
begin
  if not UseFilesInDirectoryBtn.Checked then
    UseFilesInDirectoryBtn.Checked := True;
end;

procedure TEslOptionsDlg.DrpFastaDirectoryEditChange(Sender: TObject);
begin
  if not UseFilesInDirectoryBtn.Checked then
    UseFilesInDirectoryBtn.Checked := True;
end;

procedure TEslOptionsDlg.EnsembleCoverageCheckboxChange(Sender: TObject);
begin
  EnsembleCoverageSpinEdit.Enabled := EnsembleCoverageCheckbox.Checked;
end;

procedure TEslOptionsDlg.EnsemblePartsCheckboxChange(Sender: TObject);
begin
  EnsemblePartsSpinEdit.Enabled := EnsemblePartsCheckbox.Checked;
end;

procedure TEslOptionsDlg.FixedSparsityParamsCheckboxChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if FixedSparsityParamsCheckbox.Checked then
    begin
      LambdaMinSpinEdit.Enabled := False;
      LambdaMaxSpinEdit.Enabled := False;
      LambdaStepSpinEdit.Enabled := False;
      Lambda2MinSpinEdit.Enabled := False;
      Lambda2MaxSpinEdit.Enabled := False;
      Lambda2StepSpinEdit.Enabled := False;

      LambdaSpinEdit.Enabled := True;
      Lambda2SpinEdit.Enabled := True;
    end
    else
    begin
      LambdaMinSpinEdit.Enabled := True;
      LambdaMaxSpinEdit.Enabled := True;
      LambdaStepSpinEdit.Enabled := True;
      Lambda2MinSpinEdit.Enabled := True;
      Lambda2MaxSpinEdit.Enabled := True;
      Lambda2StepSpinEdit.Enabled := True;

      LambdaSpinEdit.Enabled := False;
      Lambda2SpinEdit.Enabled := False;
    end;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TEslOptionsDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TEslOptionsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  EslOptionsDlg := nil;
  CloseAction := caFree;
end;

procedure TEslOptionsDlg.CladeSizeUpperCboxChange(Sender: TObject);
begin
  CladeSizeUpperBoundSEdit.Enabled := CladeSizeUpperCbox.Checked;
end;

procedure TEslOptionsDlg.CladeSizeLowerCboxChange(Sender: TObject);
begin
  CladeSizeLowerBoundSEdit.Enabled := CladeSizeLowerCbox.Checked;
end;

procedure TEslOptionsDlg.FormCreate(Sender: TObject);
begin
  FNumTaxaInUserTree := 0;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': DrPhylo Settings';
  FActiveDataHasDomains := False;
  FAlignmentLength := 0;
  FIgnoreEvents := False;
  MethodCBox.Items.Clear;
  MethodCBox.Items.Add(LOGISTIC);
  MethodCBox.Items.Add(LEASTR);
  FCladeSize := 0;
  //MethodCBox.Items.Add(OL_LEASTR);
  //MethodCBox.Items.Add(OL_LOGISTIC);
  {$IFDEF DEBUG}
  DrpFastaDirectoryEdit.Directory := 'C:\Users\gstecher\Documents\lazarusProjects\MEGA-cef4Delphi\Private\DrPhylo\debug_test_input\alignment_files';
  //TreeFileEdit.FileName := 'C:\Users\gstecher\Documents\iGEM\MEGA\testing\esl-tree\angiosperm_20spec.nwk';
  //ResponseFileEdit.FileName := 'C:\Users\gstecher\Documents\iGEM\MEGA\testing\esl-response\angiosperm_20spec_pred.txt';
  //NodeListFileEdit.FileName := 'C:\Users\gstecher\Documents\iGEM\MEGA\testing\esl-tree\node_list.txt';
  //GenePenaltiesFilenameEdit.FileName := 'C:\Users\gstecher\Documents\iGEM\MEGA\testing\esl-tree\gene_penalties.txt';
  //SlepOptionsFilenameEdit.FileName := 'C:\Users\gstecher\Documents\iGEM\MEGA\testing\esl-tree\slep_options.txt';
  EmailEdit.Text := 'tuf81013@temple.edu';
  {$ENDIF}
  FArchiveBaseDirectory := 'esl_input_data' + PathDelim;
end;

procedure TEslOptionsDlg.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TEslOptionsDlg.FormShow(Sender: TObject);
begin
  if FIsDrPhylo then
  begin
    ClientWidth := DrpFastaDirectoryEdit.Left + DrpFastaDirectoryEdit.Width + 60;
    ClientHeight := DrpNumGroupsSpinEdit.Top + DrpNumGroupsSpinEdit.Height + 60;
  end
  else
  begin
    ClientWidth := Lambda2StepSpinEdit.Left + Lambda2StepSpinEdit.Width*3;
    ClientHeight := CrossValidationCheckbox.Top + CrossValidationCheckbox.Height + 60;
  end;
end;

procedure TEslOptionsDlg.LaunchActionExecute(Sender: TObject);
begin
  if FIsDrPhylo then
  begin
    if not (DrpDnaCheckBox.Checked or DrpProteinCheckBox.Checked) then
    begin
      ShowMessage('At least one character type for encoding must be selected');
      Exit;
    end;
    LaunchDrPhylo;
  end
  else
  begin
    raise Exception.Create('missing process handler for ESL analysis');
    { the correct thing to do here is call LaunchEslAnalysis but updates
      would be needed as the latest development effort is for DrPhylo only. It is
      possible in the future that ESL will be supported but it is not
      likely and so that code has not been maintained.
    }
    LaunchEslAnalysis(FileExists(TreeFileEdit.FileName));
  end;
end;

procedure TEslOptionsDlg.MethodCBoxChange(Sender: TObject);
begin
 if MethodCBox.ItemIndex = MethodCBox.Items.IndexOf(LOGISTIC) then
   SampleBalanceChBox.Enabled := True
 else
   SampleBalanceChBox.Enabled := False;
end;

procedure TEslOptionsDlg.NumSitesPerPartitionSpinEditChange(Sender: TObject);
begin
  if not PartitionActiveDataBtn.Checked then
    PartitionActiveDataBtn.Checked := True;
end;

procedure TEslOptionsDlg.NumSitesPerPartitionSpinEditEditingDone(Sender: TObject);
var
  maxGroups: Integer = -1;
  remainder: Double = 0;
begin
  if NumSitesPerPartitionSpinEdit.Value > 0 then
  begin
    maxGroups := trunc(FAlignmentLength/NumSitesPerPartitionSpinEdit.Value);
    remainder := frac(FAlignmentLength/NumSitesPerPartitionSpinEdit.Value);
    if CompareValue(remainder, 0, FP_CUTOFF) > 0 then
      inc(maxGroups);
    if DrpNumGroupsSpinEdit.Value > maxGroups then
      DrpNumGroupsSpinEdit.Value := maxGroups;
    DrpNumGroupsSpinEdit.MaxValue := maxGroups;
  end;
end;

procedure TEslOptionsDlg.ResetDefaultsActionExecute(Sender: TObject);
begin
  LambdaSpinEdit.Value := 0.1;
  Lambda2SpinEdit.Value := 0.1;
  MethodCBox.ItemIndex := 0;
  AutoNameNodesCbox.Checked := False;
  AutoNameLengthSpinEdit.Value := 5;
  CladeSizeLowerBoundSEdit.Value := 0;
  CladeSizeUpperBoundSEdit.Value := 8;
  CladeSizeLowerCbox.Checked := False;
  CladeSizeUpperCbox.Checked := False;
  SmartSamplingCheckbox.Checked := False;
  SampleBalanceChBox.Checked := False;
  EnsemblePartsCheckbox.Checked := False;
  EnsembleCoverageCheckbox.Checked := True;
  EnsembleCoverageSpinEdit.Value := 5;
  GeneDisplayLimitSpinEdit.Value := 100;
  GeneDisplayCutoffFloatSpinEdit.Value := 0.0;
  SparsifyCheckbox.Checked := False;
  SampleBalanceChBox.Checked := False;
  SampleBalanceChBox.Enabled := False;
  UpsampleBalanceCheckBox.Checked := False;
  DownsampleBalanceCheckBox.Checked := False;
end;

procedure TEslOptionsDlg.SmartSamplingCheckboxChange(Sender: TObject);
begin
  SmartSamplingSpinEdit.Enabled := SmartSamplingCheckbox.Checked;
end;

procedure TEslOptionsDlg.Timer1Timer(Sender: TObject);
begin
  if ProgressBar1.Position < 100 then
    ProgressBar1.Position := ProgressBar1.Position + 1
  else
    ProgressBar1.Position := 0;
  ProgressBar1.Invalidate;
end;

procedure TEslOptionsDlg.UseGenesDomainsBtnChange(Sender: TObject);
var
  maxDomains: Integer = -1;
begin
  if UseGenesDomainsBtn.Checked then
  begin
    if Assigned(D_InputSeqData) and Assigned(D_InputSeqData.AllDomainInfo) then
      begin
        maxDomains := D_InputSeqData.AllDomainInfo.NoOfDomains;
        if maxDomains > 1 then
        begin
          if DrpNumGroupsSpinEdit.Value > maxDomains then
            DrpNumGroupsSpinEdit.Value := maxDomains;
        end;
      end;
  end;
end;

procedure TEslOptionsDlg.UpdateAnalysisSummary(s: TAnalysisSummary);
begin
  if UseGenesDomainsBtn.Checked then
    s.AnalysisOptions.Add('Data Segmented By=genes_and_domains')
  else if PartitionActiveDataBtn.Checked then
  begin
    s.AnalysisOptions.Add('Data Segmented By=fixed size');
    s.AnalysisOptions.Add(Format('No. of Sites Per Partition=%d', [NumSitesPerPartitionSpinEdit.Value]))
  end
  else if UseFilesInDirectoryBtn.Checked then
    s.AnalysisOptions.Add('Data Segmented By=fasta_files');
  s.AnalysisOptions.Add(Format('Encode Nucleotide Chars=%s', [BoolToStr(DrpDnaCheckBox.Checked, True)]));
  s.AnalysisOptions.Add(Format('Encode Protein Chars=%s', [BoolToStr(DrpProteinCheckBox.Checked, True)]));
  s.AnalysisOptions.Add(Format('No. of Rows=%d', [DrpNumTaxaSpinEdit.Value]));
  s.AnalysisOptions.Add(Format('No. of Columns=%d', [DrpNumGroupsSpinEdit.Value]));
  s.AnalysisOptions.Add(Format('Target Clade Size=%d', [NumTaxaInTargetClade]));
  s.AnalysisOptions.Add(Format('No. of Taxa in Tree=%d', [NumTaxaInUserTree]));
end;

function TEslOptionsDlg.TempNewickFile: String;
var
  aFile: TextFile;
begin
  Result := GetTempFileName(EmptyStr, 'mega_newick_');
  Result := ChangeFileExt(Result, '.nwk');
  try
    AssignFile(aFile, Result);
    Rewrite(aFile);
    WriteLn(aFile, FNewickString);
  finally
    CloseFile(aFile);
  end;
end;

function TEslOptionsDlg.GetEslCommandLineOptions: TStringList;
begin
  Result := TStringList.Create;
  if FixedSparsityParamsCheckbox.Checked then
  begin
    Result.Add(Format('--lambda1=%.8f', [LambdaSpinEdit.Value]));
    Result.Add(Format('--lambda2=%.8f', [Lambda2SpinEdit.Value]));
  end
  else
  begin
    Result.Add(Format('--grid_z=%.8f,%.8f,%.8f', [LambdaMinSpinEdit.Value, LambdaMaxSpinEdit.Value, LambdaStepSpinEdit.Value]));
    Result.Add(Format('--grid_y=%.8f,%.8f,%.8f', [Lambda2MinSpinEdit.Value, Lambda2MaxSpinEdit.Value, Lambda2StepSpinEdit.Value]));
  end;

  Result.Add(Format('--method=%s', [LowerCase(MethodCBox.Items[MethodCBox.ItemIndex])]));

  if SmartSamplingCheckbox.Checked then
    Result.Add(Format('--smart_sampling=%d', [SmartSamplingSpinEdit.Value]));

  if CrossValidationCheckbox.Checked then
    Result.Add(Format('--xval=%d', [CrossValidationSpinEdit.Value]));

  Result.Add('--m_grid= ');
  //if UnbalancedCheckbox.Checked then
  //  Result.Add('');

  //Result.Add(Format('gene_display_limit=%d', [GeneDisplayLimitSpinEdit.Value]));
  //if CompareValue(GeneDisplayCutoffFloatSpinEdit.Value, 0.0, FP_CUTOFF) > 0 then
  //  Result.Add(Format('gene_display_cutoff=%.8f', [GeneDisplayCutoffFloatSpinEdit.Value]));
end;

procedure TEslOptionsDlg.SetActiveDataHasDomains(AValue: Boolean);
begin
  if FActiveDataHasDomains = AValue then Exit;
  FActiveDataHasDomains := AValue;
  if not FActiveDataHasDomains then
  begin
    if UseGenesDomainsBtn.Checked then
      PartitionActiveDataBtn.Checked := True;
    UseGenesDomainsBtn.Enabled := False;
  end
  else
  begin
    UseGenesDomainsBtn.Enabled := True;
    UseGenesDomainsBtn.Checked := True;
  end;
end;

procedure TEslOptionsDlg.SetAlignmentLength(AValue: Integer);
begin
  FAlignmentLength := AValue;
  if NumSitesPerPartitionSpinEdit.Value > AValue then
    NumSitesPerPartitionSpinEdit.Value := AValue div 3;
  NumSitesPerPartitionSpinEdit.MaxValue := AValue - 1;
end;

procedure TEslOptionsDlg.SetIsDrPhylo(AValue: Boolean);
begin
  if FIsDrPhylo = AValue then Exit;
  FIsDrPhylo := AValue;
  if IsDrPhylo then
    PageControl1.ActivePage := DrPhyloTab;
end;

procedure TEslOptionsDlg.SetNumTaxaInTargetClade(AValue: Integer);
begin
  if FNumTaxaInTargetClade = AValue then Exit;
  FNumTaxaInTargetClade := AValue;
  DrpNumTaxaSpinEdit.Value := AValue;
  DrpNumTaxaSpinEdit.MaxValue := AValue;
end;

function TEslOptionsDlg.GetDrPhyloCommandLineOptions: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('--DrPhylo= ');
  if DrpDnaCheckBox.Checked and DrpProteinCheckBox.Checked then
    Result.Add('--data_type=molecular')
  else if DrpDnaCheckBox.Checked then
    Result.Add('--data_type=nucleotide')
  else if DrpProteinCheckBox.Checked then
    Result.Add('--data_type=protein')
  else
    raise Exception.Create('At least one character type for encoding must be selected');
  Result.Add(Format('--m_grid=%d,%d', [DrpNumTaxaSpinEdit.Value, DrpNumGroupsSpinEdit.Value]));
  Result.Add('--subsets=0'); { giving MyESL a value here less than 1 so that MyESL will automatically determine the number of partitions to use}
end;

procedure TEslOptionsDlg.UpdateFastaDirectoryName(aPath: String);
var
  p: String = '';
begin
  p := aPath;
  if p[Length(p)] <> PathDelim then
    p += PathDelim;
  FFastaDirectoryName := ExtractFileDir(p);
end;

procedure TEslOptionsDlg.SetNewickString(AValue: String; numTaxa: Integer = 0);
begin
  FNewickString := Trim(AValue);
  if FNewickString = EmptyStr then
  begin
    TreeFileEdit.Enabled := True;
    TreeFileLabel.Enabled := True;
    TreeFileEdit.Visible := True;
    TreeFileLabel.Visible := True;
    ResponseFileLabel.Visible := True;
    ResponseFileEdit.Enabled := True;
    ResponseFileEdit.Visible := True;
    OrLabel.Visible := True;
  end
  else
  begin
    NewickLabel.Caption := Format('Using a phylogeny with %d taxa', [numTaxa]);
    NewickLabel.Visible := True;
    TreeFileEdit.Enabled := False;
    TreeFileEdit.Visible := False;
    TreeFileLabel.Visible := False;
    ResponseFileLabel.Visible := False;
    ResponseFileEdit.Enabled := False;
    ResponseFileEdit.Visible := False;
    OrLabel.Visible := False;
    UpdateProgress(Format('added newick string with %d taxa:', [numTaxa]));
    UpdateProgress(#9 + FNewickString);
  end;
end;

procedure TEslOptionsDlg.UpdateProgress(aMsg: String);
begin
  if ProgressMemo.Lines.Count = 0 then
    ProgressMemo.Lines.Add(Format('%-20s %s', ['Time', 'Info']));
  ProgressMemo.Lines.Add(Format('%-20s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', Now), aMsg]));
end;

function TEslOptionsDlg.GetEmailAddress: String;
begin
  Result := EmailEdit.Text;
end;


function TEslOptionsDlg.GetAlignmentListFileName: String;
begin
  Result := 'alignment_files.txt'
end;

function TEslOptionsDlg.GetInternalArchiveFileName(filename: String): String;
begin
  Result := FFastaDirectoryName + PathDelim + ExtractFileName(filename);
end;

procedure TEslOptionsDlg.TreeBasedParamStrings(const aNewickFile: String; var aList: TStringList);
begin
  if FNewickString <> EmptyStr then
    aList.Add(Format('tree%s%s', [#9, ESL_NEWICK]))
  else if aNewickFile <> EmptyStr then
    aList.add(Format('tree%s%s', [#9, ExtractFileName(aNewickFile)]))
  else
    aList.Add(Format('tree%s%s', [#9, ExtractFileName(TreeFileEdit.FileName)]));

  if FileExists(NodeListFileEdit.Filename) then
    aList.Add(Format('nodelist%s%s', [#9, ExtractFileName(NodeListFileEdit.Filename)]));

  //if AutoNameNodesCbox.Checked then
  //begin
    //aList.Add('auto_name_nodes');
    //aList.Add(Format('auto_name_length%s%d', [#9, AutoNameLengthSpinEdit.Value]));
  //end;

  //if CladeSizeLowerCbox.Checked then
  //  aList.Add(Format('cladesize_cutoff_lower%s%d', [#9, CladeSizeLowerBoundSEdit.Value]));
  //if CladeSizeUpperCbox.Checked then
  //  aList.Add(Format('cladesize_cutoff_upper%s%d', [#9, CladeSizeUpperBoundSEdit.Value]));
end;

procedure TEslOptionsDlg.ResponseBasedParamStrings(var aList: TStringList);
begin
  aList.Add(Format('response%s%s', [#9, ExtractFileName(ResponseFileEdit.FileName)]));
end;

procedure TEslOptionsDlg.GlobalParamStrings(var aList: TStringList);
begin
  if FixedSparsityParamsCheckbox.Checked then
  begin
    aList.Add(Format('lambda1%s%.2f', [#9, LambdaSpinEdit.Value]));
    aList.Add(Format('lambda2%s%.2f', [#9, Lambda2SpinEdit.Value]));
  end
  else
  begin
    aList.Add(Format('grid_z %.8f,%.8f,%.8f', [LambdaMinSpinEdit.Value, LambdaMaxSpinEdit.Value, LambdaStepSpinEdit.Value]));
    aList.Add(Format('grid_y %.8f,%.8f,%.8f', [Lambda2MinSpinEdit.Value, Lambda2MaxSpinEdit.Value, Lambda2StepSpinEdit.Value]));
  end;

  aList.Add(Format('aln_list%s%s', [#9, GetAlignmentListFileName]));

  if FileExists(GenePenaltiesFilenameEdit.Filename) then
    aList.Add(Format('gene_penalties%s%s', [#9, ExtractFileName(GenePenaltiesFilenameEdit.Filename)]));

  if FileExists(SlepOptionsFilenameEdit.Filename) then
    aList.Add(Format('slep_opts%s%s', [#9, ExtractFileName(SlepOptionsFilenameEdit.Filename)]));

  aList.Add(Format('method%s%s', [#9, LowerCase(MethodCBox.Items[MethodCBox.ItemIndex])]));
  if SmartSamplingCheckbox.Checked then
    aList.Add(Format('smart_sampling%s%d', [#9, SmartSamplingSpinEdit.Value]));
  //if EnsemblePartsCheckbox.Checked then
  //  aList.Add(Format('ensemble_parts%s%d', [#9, EnsemblePartsSpinEdit.Value]));
  //if EnsembleCoverageCheckbox.Checked then
  //  aList.Add(Format('ensemble_coverage%s%d', [#9, EnsembleCoverageSpinEdit.Value]));
  aList.Add(Format('gene_display_limit%s%d', [#9, GeneDisplayLimitSpinEdit.Value]));
  if CompareValue(GeneDisplayCutoffFloatSpinEdit.Value, 0.0, FP_CUTOFF) > 0 then
    aList.Add(Format('gene_display_cutoff%s%.8f', [#9, GeneDisplayCutoffFloatSpinEdit.Value]));
  //if SparsifyCheckbox.Checked then
  //  aList.Add('sparsify');
  //if SampleBalanceChBox.Checked and SampleBalanceChBox.Enabled then
  //  aList.Add('slep_sample_balance');
  //if UpsampleBalanceCheckBox.Checked then
  //  aList.Add('upsample_balance');
  //if DownsampleBalanceCheckBox.Checked then
  //  aList.Add('downsample_balance');
end;

function TEslOptionsDlg.ValidateTreeBasedOptions: Boolean;
begin
  Result := False;
  if FIsDrPhylo then
  begin
    if UseFilesInDirectoryBtn.Checked then
    begin
      if not DirectoryExists(DrpFastaDirectoryEdit.Directory) then
      begin
        ShowMessage('Please specify a valid directory that contains the FASTA alignment files to process');
        DrpFastaDirectoryEdit.SetFocus;
        Exit;
      end;
    end;
  end
  else
  begin
    if not DirectoryExists(FastaDirectoryEdit.Directory) then
    begin
      ShowMessage('Please specify a valid directory that contains the FASTA alignment files to process');
      FastaDirectoryEdit.SetFocus;
      Exit;
    end;

    if TreeFileEdit.Enabled and (not FileExists(TreeFileEdit.FileName)) then
    begin
      ShowMessage('Please specify a Newick tree file');
      TreeFileEdit.SetFocus;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TEslOptionsDlg.LaunchEslAnalysis(isTreeBased: Boolean);
var
  fastaFiles: TStringList = nil;
begin
  try
    fastaFiles := FindAllFiles(FastaDirectoryEdit.Directory);
    UpdateFastaDirectoryName(FastaDirectoryEdit.Directory);
    if fastaFiles.Count = 0 then
      raise Exception.Create('no alignment files were found');

    GenerateArchive(isTreeBased, fastaFiles);
    ProgressBar1.Position := 0;
    Invalidate;
  finally
    if Assigned(fastaFiles) then
      fastaFiles.Free;
  end;
end;

procedure TEslOptionsDlg.LaunchDrPhylo;
var
  fastaFiles: TStringList = nil;
  t: TEslLinkerThread = nil;
  ARP: TRuntimeProgress = nil;
  MAI: TAnalysisInfo = nil;
  aOptions: TStringList = nil;
begin
  try
    if FIsDrPhylo then
    begin
      if UseFilesInDirectoryBtn.Checked then
      begin
        fastaFiles := FindAllFiles(DrpFastaDirectoryEdit.Directory);
        UpdateFastaDirectoryName(DrpFastaDirectoryEdit.Directory);
        if fastaFiles.Count = 0 then
          raise Exception.Create('no alignment files were found');
      end;
    end
    else
    begin
      fastaFiles := FindAllFiles(FastaDirectoryEdit.Directory);
      UpdateFastaDirectoryName(FastaDirectoryEdit.Directory);
      if fastaFiles.Count = 0 then
        raise Exception.Create('no alignment files were found');
    end;

    if UseGenesDomainsBtn.Checked then
      t := TEslLinkerThread.CreateForDomainData(ALIGNMENT_DOMAINS, D_InputSeqData)
    else if PartitionActiveDataBtn.Checked then
      t := TEslLinkerThread.CreateForSegmentedData(ALIGNMENT_SEGMENTS, D_InputSeqData, NumSitesPerPartitionSpinEdit.Value)
    else if UseFilesInDirectoryBtn.Checked then
      t := TEslLinkerThread.CreateForFastaFiles(FASTA_FILES, FFastaDirectoryName)
    else
        raise Exception.Create('missing DrPhylo data source option handler');
    t.CladeSize := FCladeSize;
    t.EslLink.NumGridRows := DrpNumTaxaSpinEdit.Value;
    t.EslLink.NumGridColumns := DrpNumGroupsSpinEdit.Value;
    if FIsDrPhylo then
      aOptions := DrPhyloCommandLineOptions
    else
      aOptions := EslCommandLineOptions;
    t.UpdateCommandLineOptions(aOptions);
    t.NewickString := FNewickString;
    t.TargetNodeName := FTargetNodeLabel;
    t.FreeOnTerminate := True;
    if FIsDrPhylo then
      t.OnTerminate := MegaForm.DrPhyloThreadDone
    else
      t.OnTerminate := MegaForm.EslThreadDone;

    ARP := TRuntimeProgress.Create(Application);
    MAI := TAnalysisInfo.Create;
    MAI.MyProcessPack := TProcessPack.Create;
    MAI.MyProcessPack.AddProcessType(ppDrPhylo);
    MAI.ARP := ARP;
    MAI.ARP.HasCmdLineOutput := True;
    ARP.ApplinkerProgressPlaceholder := EmptyStr;
    ARP.Refresh;
    ARP := nil;
    t.EslLink.MAI := MAI;
    t.UpdateProgressProc := MAI.ARP.UpdatePercentProgress;
    t.UpdateStatusProc := MAI.ARP.UpdateRunStatusInfo;
    t.SetNewLineProc(MAI.ARP.AddCommandLine);
    t.NewLineCheckCancelFunc := MAI.ARP.AddCommandLineCheckCancel;
    t.ShowAnalysisOptionsProc := MAI.ARP.AddAnalysisOptions;
    UpdateAnalysisSummary(MAI.AnalysisSummary);
    {$IFDEF VISUAL_BUILD}
    if not FileExists(GetEslExePath) then
      ShowMessage('The DrPhylo binaries need to be unzipped for first-time use which may take some time.');
    MAI.ARP.StdOutLabelCaption := 'Executing DrPhylo...';
    MAI.ARP.UseTimer := True;
    MAI.ARP.TimerInterval := 250;
    MAI.ARP.SetKeepOnTop(True);
    MAI.ARP.Show;
    MAI.ARP.DisplayCommandLineTab;
    MAI.ARP.Height := Max(MAI.ARP.Height, 400);
    MAI.ARP.Width := Max(MAI.ARP.Width, 600);
    MAI.ARP.Thread := t;
    {$ENDIF}
    t.Start;
  finally
    if Assigned(fastaFiles) then
      fastaFiles.Free;
    if Assigned(aOptions) then
      aOptions.Free;
  end;
  EslOptionsDlg := nil;
  Close;
end;

procedure TEslOptionsDlg.GenerateArchive(isTreeBased: Boolean; fastaFiles: TStringList);
var
  paramsFilename: String = '';
  aNewickFile: String = '';
  aParams: TStringList = nil;
  i: Integer = -1;
  fileList: TStringList = nil;
  fileListFilename: String = '';
begin
  try
    fileList := TStringList.Create;
    aParams := TStringList.Create;
    FNumFiles := fastaFiles.Count;
    FNumZipped := 0;
    FProgressInterval := max(1, FNumFiles div 100);
    for i := 0 to fastaFiles.Count - 1 do
    begin
      FZipper.Entries.AddFileEntry(fastaFiles[i], FArchiveBaseDirectory + GetInternalArchiveFileName(fastaFiles[i]));
      fileList.Add(GetInternalArchiveFileName(fastaFiles[i]));
    end;

    paramsFilename := GetTempFileName(EmptyStr, 'mega_esl_parameters.txt');
    if paramsFilename = EmptyStr then
      raise Exception.Create('unable to create temp file');
    if isTreeBased then
    begin
      if FNewickString <> EmptyStr then
      begin
        aNewickFile := TempNewickFile;
        TreeBasedParamStrings(aNewickFile, aParams);
      end
      else
        TreeBasedParamStrings(EmptyStr, aParams);
    end
    else
      ResponseBasedParamStrings(aParams);
    GlobalParamStrings(aParams);
    aParams.SaveToFile(paramsFilename);
    FZipper.Entries.AddFileEntry(paramsFilename, FArchiveBaseDirectory + 'parameters.txt');

    if isTreeBased and FileExists(NodeListFileEdit.FileName) then
      FZipper.Entries.AddFileEntry(NodeListFileEdit.FileName, FArchiveBaseDirectory + ExtractFilename(NodeListFileEdit.FileName));

    if FileExists(GenePenaltiesFilenameEdit.FileName) then
      FZipper.Entries.AddFileEntry(GenePenaltiesFilenameEdit.FileName, FArchiveBaseDirectory + ExtractFilename(GenePenaltiesFilenameEdit.FileName));

    if FileExists(SlepOptionsFilenameEdit.FileName) then
      FZipper.Entries.AddFileEntry(SlepOptionsFilenameEdit.FileName, FArchiveBaseDirectory + ExtractFilename(SlepOptionsFilenameEdit.FileName));

    UpdateProgress('generated parameters file with the following settings: ');
    for i := 0 to aParams.Count - 1 do
      UpdateProgress(Format('%s%s%s', [#9, #9, aParams[i]]));
    fileListFilename := GetTempFileName(EmptyStr, 'mega_esl_files.txt');
    fileList.SaveToFile(fileListFilename);
    FZipper.Entries.AddFileEntry(fileListFilename, FArchiveBaseDirectory + GetAlignmentListFileName);
    UpdateProgress(Format('found %.0n alignment files for submission', [fileList.Count*1.0]));

    if isTreeBased then
    begin
      if TreeFileEdit.Enabled then
        FZipper.Entries.AddFileEntry(TreeFileEdit.FileName, FArchiveBaseDirectory + ExtractFileName(TreeFileEdit.FileName))
      else if FNewickString <> EmptyStr then
      begin
        FZipper.Entries.AddFileEntry(aNewickFile, FArchiveBaseDirectory + ExtractFileName(aNewickFile));
      end
      else
        raise Exception.Create('Application error - newick tree file not resolved');
    end
    else
      FZipper.Entries.AddFileEntry(ResponseFileEdit.FileName, FArchiveBaseDirectory + ExtractFileName(ResponseFileEdit.FileName));
    UpdateProgress('generating zip archive (Please Wait...)');
    FZipper.OnEndFile := OnEndZipFile;
    FZipper.ZipAllFiles;
    UpdateProgress('zip archive generated');
  finally
    if Assigned(aParams) then
      aParams.Free;
    if Assigned(fileList) then
      fileList.Free;
    if FileExists(paramsFilename) then
      try
        DeleteFile(paramsFilename);
      except

      end;
    if isTreeBased and FileExists(aNewickFile) then
      try
        DeleteFile(aNewickFile);
      except

      end;
    if FileExists(fileListFilename) then
      try
        DeleteFile(fileListFilename);
      except

      end;
  end;
end;

function TEslOptionsDlg.ValidateResponseBasedOptions: Boolean;
begin
  Result := False;
  if not DirectoryExists(FastaDirectoryEdit.Directory) then
  begin
    ShowMessage('Please specify a valid directory that contains the FASTA alignment files to process');
    FastaDirectoryEdit.SetFocus;
    Exit;
  end;

  if not FileExists(ResponseFileEdit.FileName) then
  begin
    ShowMessage('Please specify a response file');
    ResponseFileEdit.SetFocus;
    Exit;
  end;
  Result := True;
end;

procedure TEslOptionsDlg.OnZipProgress(Sender: TObject; const Pct: Double);
begin
  ProgressBar1.Position := Min(100, Round(pct));
  ProgressBar1.Invalidate;
  ProgressMemo.Lines.Add(Format('Progress %.2f%%', [pct]));
  Application.ProcessMessages;
end;

procedure TEslOptionsDlg.OnEndZipFile(Sender: TObject; const Ratio: Double);
var
  aProgress: Integer = -1;
begin
  inc(FNumZipped);
  if (FNumZipped mod FProgressInterval) = 0 then
  begin
    aProgress := min(100, Round(FNumZipped/FNumFiles*100));
    ProgressBar1.Position := aProgress;
  end;
end;

procedure TEslOptionsDlg.SetDataType(aType: String);
begin
  if aType = DNAStr then
  begin
    DrpDnaCheckBox.Checked := True;
    DrpProteinCheckBox.Checked := False;
  end
  else if aType = ProteinStr then
  begin
    DrpDnaCheckBox.Checked := False;
    DrpProteinCheckBox.Checked := True;
  end
  else
  begin
    DrpDnaCheckBox.Checked := False;
    DrpProteinCheckBox.Checked := False;
  end;
end;

procedure TEslOptionsDlg.SetActiveAlignmentEnabled(aValue: Boolean);
begin
  if not aValue then
  begin
    UseFilesInDirectoryBtn.Checked := True;
    UseGenesDomainsBtn.Enabled := False;
    PartitionActiveDataBtn.Enabled := False;
    NumSitesPerPartitionSpinEdit.Enabled := False;
  end
  else
  begin
    UseGenesDomainsBtn.Enabled := True;
    PartitionActiveDataBtn.Enabled := True;
    NumSitesPerPartitionSpinEdit.Enabled := True;
  end;
end;

end.

