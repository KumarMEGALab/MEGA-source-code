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

unit genedomainpropertiesdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, IniPropStorage, MDomainInfo, msitepickform,
  mimageform, MegaPrivateFiles, MegaUtils;

type

  { TGenesDomainsPropertiesEditor }

  TGenesDomainsPropertiesEditor = class(TForm)
    HelpImages: TImageList;
    HelpBtn: TImage;
    CancelBtn: TImage;
    ButtonImages: TImageList;
    GeneSiteToBtn: TButton;
    DomainSiteFromBtn: TButton;
    DomainSiteToBtn: TButton;
    DomainFromSiteEdit: TEdit;
    DomainToSiteEdit: TEdit;
    GeneToSiteEdit: TEdit;
    GeneFromSiteEdit: TEdit;
    GeneIsCodingCheckBox: TCheckBox;
    GeneCodonStartComboBox: TComboBox;
    DomainCodonStartComboBox: TComboBox;
    GeneIsUsedCheckbox: TCheckBox;
    DomainIsUsedCheckBox: TCheckBox;
    DomainIsCodingCheckBox: TCheckBox;
    GeneNameEdit: TLabeledEdit;
    IniPropStorage1: TIniPropStorage;
    OkBtn: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DomainNameEdit: TLabeledEdit;
    DomainCodonStartLabel: TLabel;
    GeneCodonStartLabel: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    GeneSiteFromBtn: TSpeedButton;
    GenePropertiesTab: TTabSheet;
    DomainPropertiesTab: TTabSheet;
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure DomainIsCodingCheckBoxChange(Sender: TObject);
    procedure GeneIsCodingCheckBoxChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure DomainSiteFromBtnClick(Sender: TObject);
    procedure DomainSiteToBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GeneSiteFromBtnClick(Sender: TObject);
    procedure GeneSiteToBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure InitImages;
  private
    FHasDna: Boolean;
    FIsEditingGene: Boolean;
    FMaxSiteIndex: Integer;
    FDomainInfo: TDomainInfo;
    FCurDomainInfo: TDomainInfo;
    procedure SetHasDna(AValue: Boolean);
    procedure SetIsEditingGene(AValue: Boolean);
    procedure SetMaxSiteIndex(AValue: Integer);
    function ValidateForm: Boolean;
  public
    SitePicker: TSitePickForm;
    procedure InitFromDomainInfo(aInfo: TDomainInfo);
    procedure GetDomainInfoProperties(var aInfo: TDomainInfo);
    property IsEditingGene: Boolean read FIsEditingGene write SetIsEditingGene;
    property HasDna: Boolean read FHasDna write SetHasDna;
    property MaxSiteIndex: Integer read FMaxSiteIndex write SetMaxSiteIndex;
    { public declarations }
  end;

var
  GenesDomainsPropertiesEditor: TGenesDomainsPropertiesEditor;

implementation

uses
  MD_InputSeqData, ContextHelp_HC, mhelpfiles, mhelpkeywords;

{$R *.lfm}

{ TGenesDomainsPropertiesEditor }

procedure TGenesDomainsPropertiesEditor.FormCreate(Sender: TObject);
begin
  FIsEditingGene := True;
  FHasDna := True;
  GenePropertiesTab.TabVisible := True;
  DomainPropertiesTab.TabVisible := False;
  FMaxSiteIndex := 1;
  SitePicker := TSitePickForm.Create(Self);
  SitePicker.FNoOfTaxa := D_InputSeqData.NoOfTaxa;
  SitePicker.FNoOfSites := D_InputSeqData.FNoOfSites;
  SitePicker.FDomainMarks := D_InputSeqData.FDomainMarks;
  SitePicker.FOtuInfos    := D_InputSeqData.OtuInfos;
  SitePicker.CurDomainInfo:= nil;
  SitePicker.Initialize;
  FDomainInfo := TDomainInfo.Create;
  HelpContext := HC_Genes_Domains_Dialog;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  InitImages;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  Application.ProcessMessages;
end;

procedure TGenesDomainsPropertiesEditor.InitImages;
begin
  {$IFDEF DARWIN}
  HelpBtn.Proportional:=True;
  CancelBtn.Proportional:=True;
  OkBtn.Proportional:=True;
  {$ENDIF}
end;

procedure TGenesDomainsPropertiesEditor.DomainSiteToBtnClick(Sender: TObject);
begin
  SitePicker.CurDomainInfo := FCurDomainInfo;
  if Trim(DomainFromSiteEdit.Text) = EmptyStr then
  begin
    ShowMessage('Please identify a domain start site first');
    Exit;
  end;
  SitePicker.Caption := 'End site for ' + FDomainInfo.Name;
  SitePicker.IsPickingStart := True;
  if FDomainInfo.ToSite >= 0 then
    SitePicker.GoToSite(FDomainInfo.ToSite)
  else if FDomainInfo.FromSite >= 0 then
    SitePicker.GoToSite(FDomainInfo.FromSite)
  else SitePicker.GoToSite(0);

  if SitePicker.ShowModal = mrOK then
  begin
    DomainToSiteEdit.Text := IntToStr(SitePicker.GetSitePicked + 1);
    FDomainInfo.ToSite := SitePicker.GetSitePicked;
  end;
end;

procedure TGenesDomainsPropertiesEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult = mrOk) and (not ValidateForm) then
    CanClose := False
  else
    CanClose := True;
end;

procedure TGenesDomainsPropertiesEditor.DomainSiteFromBtnClick(Sender: TObject);
begin
  SitePicker.CurDomainInfo := FCurDomainInfo;
  SitePicker.Caption := 'Start site for '+ FDomainInfo.Name;
  SitePicker.IsPickingStart := True;
  if FDomainInfo.FromSite >= 0 then
    SitePicker.GoToSite(FDomainInfo.FromSite);
  if SitePicker.ShowModal = mrOK then
  begin
    DomainFromSiteEdit.Text := IntToStr(SitePicker.GetSitePicked + 1);
    FDomainInfo.FromSite := SitePicker.GetSitePicked;
  end;
end;

procedure TGenesDomainsPropertiesEditor.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TGenesDomainsPropertiesEditor.DomainIsCodingCheckBoxChange(
  Sender: TObject);
begin
  DomainCodonStartComboBox.Enabled := DomainIsCodingCheckBox.Checked;
end;

procedure TGenesDomainsPropertiesEditor.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TGenesDomainsPropertiesEditor.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TGenesDomainsPropertiesEditor.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TGenesDomainsPropertiesEditor.GeneIsCodingCheckBoxChange(
  Sender: TObject);
begin
  GeneCodonStartComboBox.Enabled := GeneIsCodingCheckBox.Checked;
end;

procedure TGenesDomainsPropertiesEditor.HelpBtnClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TGenesDomainsPropertiesEditor.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TGenesDomainsPropertiesEditor.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TGenesDomainsPropertiesEditor.FormDestroy(Sender: TObject);
begin
  if Assigned(SitePicker) then
    SitePicker.Free;
  if Assigned(FDomainInfo) then
    FDomainInfo.Free;
end;

procedure TGenesDomainsPropertiesEditor.GeneSiteFromBtnClick(Sender: TObject);
begin
  SitePicker.CurDomainInfo := FCurDomainInfo;
  SitePicker.Caption := 'Start site for '+ FDomainInfo.GeneName;
  SitePicker.IsPickingStart := True;
  if FDomainInfo.FromSite >= 0 then
    SitePicker.GoToSite(FDomainInfo.FromSite);
  if SitePicker.ShowModal = mrOK then
  begin
    GeneFromSiteEdit.Text := IntToStr(SitePicker.GetSitePicked + 1);
    FDomainInfo.FromSite := SitePicker.GetSitePicked;
  end;
end;

procedure TGenesDomainsPropertiesEditor.GeneSiteToBtnClick(Sender: TObject);
begin
  SitePicker.CurDomainInfo := FCurDomainInfo;
  if Trim(GeneFromSiteEdit.Text) = EmptyStr then
  begin
    ShowMessage('Please identify a gene start site first');
    Exit;
  end;
  SitePicker.Caption := 'End site for ' + FDomainInfo.GeneName;
  SitePicker.IsPickingStart := False;
  if FDomainInfo.ToSite >= 0 then
    SitePicker.GoToSite(FDomainInfo.ToSite)
  else if FDomainInfo.FromSite >= 0 then
    SitePicker.GoToSite(FDomainInfo.FromSite)
  else SitePicker.GoToSite(0);

  if SitePicker.ShowModal = mrOK then
  begin
    GeneToSiteEdit.Text := IntToStr(SitePicker.GetSitePicked + 1);
    FDomainInfo.ToSite := SitePicker.GetSitePicked;
  end;
end;

procedure TGenesDomainsPropertiesEditor.OkBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TGenesDomainsPropertiesEditor.OkBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TGenesDomainsPropertiesEditor.SetIsEditingGene(AValue: Boolean);
begin
  FIsEditingGene:=AValue;
  if FIsEditingGene then
    PageControl1.ActivePage := GenePropertiesTab
  else
    PageControl1.ActivePage := DomainPropertiesTab;
  GenePropertiesTab.TabVisible := FIsEditingGene;
  DomainPropertiesTab.TabVisible := (not FIsEditingGene);
end;

procedure TGenesDomainsPropertiesEditor.SetHasDna(AValue: Boolean);
begin
  if FHasDna=AValue then Exit;
  FHasDna:=AValue;
  DomainIsCodingCheckBox.Enabled := FHasDna;
  DomainCodonStartComboBox.Enabled := (FHasDna and DomainIsCodingCheckBox.Checked);
  DomainCodonStartLabel.Enabled := FHasDna;
  GeneCodonStartComboBox.Enabled := (FHasDna and GeneIsCodingCheckBox.Checked);
  GeneIsCodingCheckBox.Enabled := FHasDna;
  GeneCodonStartLabel.Enabled := FHasDna;
end;

procedure TGenesDomainsPropertiesEditor.SetMaxSiteIndex(AValue: Integer);
begin
  if FMaxSiteIndex=AValue then Exit;
  FMaxSiteIndex:=AValue;
  //GeneFromSpinEdit.MaxValue := AValue;
  //GeneToSpinEdit.Value := AValue;
  //DomainFromSpinEdit.Value := AValue;
  //DomainToSpinEdit.Value := AValue;
end;

function TGenesDomainsPropertiesEditor.ValidateForm: Boolean;
var
  startSite, endSite: Integer;
begin
  Result := False;
  if IsEditingGene then
  begin
    if (Trim(GeneFromSiteEdit.Text) = EmptyStr) or (not TryStrToInt(GeneFromSiteEdit.Text, startSite)) then
    begin
      ShowMessage('A valid integer is required for the gene start site');
      GeneFromSiteEdit.SetFocus;
      Exit;
    end;
    if (Trim(GeneToSiteEdit.Text) = EmptyStr) or (not TryStrToInt(GeneToSiteEdit.Text, endSite)) then
    begin
      ShowMessage('A valid integer is required for the gene end site');
      GeneToSiteEdit.SetFocus;
      Exit;
    end;
    if (startSite < 1) or (startSite > endSite) then
    begin
      ShowMessage('Invalid gene start site');
      GeneFromSiteEdit.SetFocus;
      Exit;
    end;
    if (endSite < 1) then
    begin
      ShowMessage('Invalid gene end site');
      GeneToSiteEdit.SetFocus;
      Exit;
    end;
    if Trim(GeneNameEdit.Text) = EmptyStr then
    begin
      ShowMessage('Gene name is required');
      GeneNameEdit.SetFocus;
      Exit;
    end;

  end
  else
  begin
    if (Trim(DomainFromSiteEdit.Text) = EmptyStr) or (not TryStrToInt(DomainFromSiteEdit.Text, startSite)) then
    begin
      ShowMessage('A valid integer is required for the domain start site');
      DomainFromSiteEdit.SetFocus;
      Exit;
    end;
    if (Trim(DomainToSiteEdit.Text) = EmptyStr) or (not TryStrToInt(DomainToSiteEdit.Text, endSite)) then
    begin
      ShowMessage('A valid integer is required for the domain end site');
      DomainToSiteEdit.SetFocus;
      Exit;
    end;
    if Trim(DomainNameEdit.Text) = EmptyStr then
    begin
      ShowMessage('Domain name is required');
      DomainNameEdit.SetFocus;
      Exit;
    end;
    if (startSite < 1) or (startSite > endSite) then
    begin
      ShowMessage('Invalid domain start site');
      DomainFromSiteEdit.SetFocus;
      Exit;
    end;
    if (endSite < 1) then
    begin
      ShowMessage('Invalid domain end site');
      DomainToSiteEdit.SetFocus;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TGenesDomainsPropertiesEditor.InitFromDomainInfo(aInfo: TDomainInfo);
begin
  try
    BeginFormUpdate;
    FDomainInfo.Assign(aInfo);
    FCurDomainInfo := aInfo;
    IsEditingGene := aInfo.IsGene;
    HasDna := D_InputSeqData.IsNuc;
    if IsEditingGene then
    begin
      GeneNameEdit.Text := aInfo.GeneName;
      if aInfo.FromSite >= 0 then
        GeneFromSiteEdit.Text := IntToStr(aInfo.FromSite + 1)
      else
        GeneFromSiteEdit.Text := EmptyStr;
      if aInfo.ToSite >= 0 then
        GeneToSiteEdit.Text := IntToStr(aInfo.ToSite + 1)
      else
        GeneToSiteEdit.Text := EmptyStr;
      GeneIsUsedCheckbox.Checked := aInfo.IsUsed;
      GeneIsCodingCheckBox.Checked := aInfo.IsCoding;
      GeneCodonStartComboBox.ItemIndex := aInfo.CodonStart;
    end
    else
    begin
      DomainNameEdit.Text := aInfo.Name;
      if aInfo.FromSite >= 0 then
        DomainFromSiteEdit.Text := IntToStr(aInfo.FromSite + 1)
      else
        DomainFromSiteEdit.Text := EmptyStr;
      if aInfo.ToSite >= 0 then
        DomainToSiteEdit.Text := IntToStr(aInfo.ToSite + 1)
      else
        DomainToSiteEdit.Text := EmptyStr;
      DomainIsUsedCheckBox.Checked := aInfo.IsUsed;
      DomainIsCodingCheckBox.Checked := aInfo.IsCoding;
      Assert((aInfo.CodonStart >= 0) and (aInfo.CodonStart <= 2));
      DomainCodonStartComboBox.ItemIndex := aInfo.CodonStart;
    end;
  finally
    EndFormUpdate;
  end;
end;

procedure TGenesDomainsPropertiesEditor.GetDomainInfoProperties(var aInfo: TDomainInfo);
begin
  if IsEditingGene then
  begin
    aInfo.GeneName := GeneNameEdit.Text;
    aInfo.FromSite := StrToInt(GeneFromSiteEdit.Text) - 1;
    aInfo.ToSite := StrToInt(GeneToSiteEdit.Text) - 1;
    aInfo.IsUsed := GeneIsUsedCheckbox.Checked;
    if GeneIsCodingCheckBox.Enabled then
      aInfo.IsCoding := GeneIsCodingCheckBox.Checked;
    if GeneCodonStartComboBox.Enabled then
      aInfo.CodonStart := GeneCodonStartComboBox.ItemIndex;
  end
  else
  begin
    aInfo.Name := DomainNameEdit.Text;
    aInfo.FromSite := StrToInt(DomainFromSiteEdit.Text) - 1;
    aInfo.ToSite := StrToInt(DomainToSiteEdit.Text) - 1;
    aInfo.IsUsed := DomainIsUsedCheckBox.Checked;
    if DomainIsCodingCheckBox.Enabled then
      aInfo.IsCoding := DomainIsCodingCheckBox.Checked;
    if DomainCodonStartComboBox.Enabled then
      aInfo.CodonStart:= DomainCodonStartComboBox.ItemIndex;
  end;
end;

end.

