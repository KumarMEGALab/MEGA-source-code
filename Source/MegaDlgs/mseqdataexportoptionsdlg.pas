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

unit mseqdataexportoptionsdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons, mseqexportoptions, mimageform;

type

  { TSeqDataExportOptionsDlg }

  TSeqDataExportOptionsDlg = class(TForm)
    HelpBtn: TImage;
    CancelBtn: TImage;
    ButtonImages: TImageList;
    HelpBtnImages: TImageList;
    OkBtn: TImage;
    MissingAndGapDataCBx: TComboBox;
    ExportSitesCBx: TComboBox;
    InterleavedChkBx: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Pos1stChkBx: TCheckBox;
    Pos3rdChkBx: TCheckBox;
    Pos2ndChkBx: TCheckBox;
    NoncodingChkBx: TCheckBox;
    FormatCBx: TComboBox;
    GroupBox1: TGroupBox;
    Panel2: TPanel;
    SiteNumCBx: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    SitesPerLineSE: TSpinEdit;
    TitleEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    DescriptionEdit: TMemo;
    Panel1: TPanel;
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormatCBxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure InterleavedChkBxChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure SiteNumCBxChange(Sender: TObject);
  private
    FIsNucData    : Boolean;
    FIsChooseBases: Boolean;

    FHasHighlightedSites: Boolean;
    FHasDomains         : Boolean;

    function  GetFileTitle: String;
    procedure SetFileTitle(const Value: String);
    function  GetFileDescription: String;
    procedure SetFileDescription(const Value: String);

    function  GetSitesPerLine:Integer;
    procedure EnableCodPosSiteGpBx(Value: Boolean);
    { private declarations }
  public
    function IsMEGAFormat: Boolean;
    function IsPAUP3Format: Boolean;
    function IsPAUP4Format: Boolean;
    function IsPhylipFormat: Boolean;
    function IsEXCELFormat: Boolean;
    function IsCSVFormat: Boolean;
    function IsFastaFormat: Boolean;
    procedure ForceExcelFormat;
    procedure ForceCSVFormat;
    procedure ForceMEGAFormat;
    function IsInterleaved: Boolean;
    function IsRemove1stBase: Boolean;
    function IsRemove2ndBase: Boolean;
    function IsRemove3rdBase: Boolean;
    function IsRemove0thBase: Boolean;
    function IsRemoveMissData:Boolean;
    function IsRemoveGapData: Boolean;
    function IsExportAllSites:           Boolean;
    function IsExportUnHighlightedSites: Boolean;
    function IsExportHighlightedSites:   Boolean;
    function IsWriteSiteNumAtEnd: Boolean;
    function IsWriteSiteNumAtTop: Boolean;
    function IsCodonByCodonWriting: Boolean;
    function ExportOptions: TSeqExportOptions;
    procedure ForceSeqMax;

    property SitesPerLine: Integer  read GetSitesPerLine;
    property FileTitle: String read GetFileTitle write SetFileTitle;
    property FileDescription: String read GetFileDescription write SetFileDescription;
    property IsNucData: Boolean read FIsNucData write FIsNucData;
    property IsChooseBases: Boolean read FIsChooseBases write FIsChooseBases;
    property HasDomains: Boolean write FHasDomains;
    property HasHighlightedSites: Boolean write FHasHighlightedSites;
    { public declarations }
  end;



implementation

uses
  DataExplorerHelp_HC, MegaConsts, mhelpfiles, mhelpkeywords;

{$R *.lfm}

{ TSeqDataExportOptionsDlg }

procedure TSeqDataExportOptionsDlg.FormatCBxChange(Sender: TObject);
begin
  if FHasDomains then
    InterleavedChkBx.Checked := True;
  if FormatCBx.ItemIndex = 6 then // Item 6 = Fasta
    InterleavedChkBx.Checked := False; // FASTA DOESN'T ALLOW INTERLEAVED
  InterleavedChkBx.Enabled   := (not FHasDomains) and (FormatCBx.ItemIndex <> 6);
  ForceSeqMax;
end;

procedure TSeqDataExportOptionsDlg.FormCreate(Sender: TObject);
begin
  HelpContext := HC_Export_Data_in_Sequence_Data_Explorer;
  {$IFDEF DARWIN}
  HelpBtn.Proportional:=True;
  CancelBtn.Proportional:=True;
  OkBtn.Proportional:=True;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
end;

procedure TSeqDataExportOptionsDlg.HelpBtnClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TSeqDataExportOptionsDlg.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TSeqDataExportOptionsDlg.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TSeqDataExportOptionsDlg.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TSeqDataExportOptionsDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSeqDataExportOptionsDlg.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TSeqDataExportOptionsDlg.InterleavedChkBxChange(Sender: TObject);
begin
  if FHasDomains then
    InterleavedChkBx.Checked := True;
  if (not InterleavedChkBx.Checked) and (SiteNumCBx.ItemIndex = 1) then
    SiteNumCBx.ItemIndex := 2;
end;

procedure TSeqDataExportOptionsDlg.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSeqDataExportOptionsDlg.OkBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TSeqDataExportOptionsDlg.OkBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TSeqDataExportOptionsDlg.SiteNumCBxChange(Sender: TObject);
begin
  if (SiteNumCBx.ItemIndex = 1) and (not InterleavedChkBx.Checked) then
  begin
    ShowMessage('You need to select interleaved output if you want to write the site numbers on top.');
    SiteNumCBx.ItemIndex := 2;
  end;
end;

function TSeqDataExportOptionsDlg.GetFileTitle: String;
begin
  Result := Trim(TitleEdit.Text);
end;

procedure TSeqDataExportOptionsDlg.SetFileTitle(const Value: String);
begin
  TitleEdit.Text := Trim(Value);
end;

function TSeqDataExportOptionsDlg.GetFileDescription: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  with DescriptionEdit do
    for i:=0 to Lines.Count-1 do
    begin
      Lines[i] := TrimRight(Lines[i]);
      if length(Lines[i]) > 0 then
        Result := Result + ' '+Lines[i];
    end;
end;

procedure TSeqDataExportOptionsDlg.SetFileDescription(const Value: String);
begin
  DescriptionEdit.Lines.Clear;
  DescriptionEdit.Lines.Add(Trim(Value));
end;

function TSeqDataExportOptionsDlg.GetSitesPerLine: Integer;
begin
  Result := Trunc(SitesPerLineSE.Value);
end;

procedure TSeqDataExportOptionsDlg.EnableCodPosSiteGpBx(Value: Boolean);
begin
  Pos1stChkBx.Enabled := Value;
  Pos2ndChkBx.Enabled := Value;
  Pos3rdChkBx.Enabled := Value;
  NoncodingChkBx.Enabled := Value;
end;

procedure TSeqDataExportOptionsDlg.ForceMEGAFormat;
begin
  FormatCBx.ItemIndex := 0;
end;

function TSeqDataExportOptionsDlg.IsMEGAFormat: Boolean;
begin
  Result := FormatCBx.ItemIndex = 0;
end;
function TSeqDataExportOptionsDlg.IsPAUP4Format: Boolean;
begin
  Result := FormatCBx.ItemIndex = 1;
end;
function TSeqDataExportOptionsDlg.IsPAUP3Format: Boolean;
begin
  Result := FormatCBx.ItemIndex = 2;
end;
function TSeqDataExportOptionsDlg.IsPhylipFormat: Boolean;
begin
  Result := FormatCBx.ItemIndex = 3;
end;

procedure TSeqDataExportOptionsDlg.ForceExcelFormat;
begin
  FormatCBx.ItemIndex := 4;
end;

function TSeqDataExportOptionsDlg.IsEXCELFormat: Boolean;
begin
  Result := FormatCBx.ItemIndex = 4;
end;

procedure TSeqDataExportOptionsDlg.ForceCSVFormat;
begin
  FormatCBx.ItemIndex := 5;
end;
function TSeqDataExportOptionsDlg.IsCSVFormat: Boolean;
begin
  Result := FormatCBx.ItemIndex = 5;
end;

function TSeqDataExportOptionsDlg.IsFastaFormat: Boolean;
begin
  Result := FormatCBx.ItemIndex = 6;
end;

function TSeqDataExportOptionsDlg.IsInterleaved: Boolean;
begin
  Result := InterleavedChkBx.Checked;
end;

function TSeqDataExportOptionsDlg.IsWriteSiteNumAtEnd: Boolean;
begin
  Result := SiteNumCBx.ItemIndex = 2;
end;
function TSeqDataExportOptionsDlg.IsWriteSiteNumAtTop: Boolean;
begin
  Result := SiteNumCBx.ItemIndex = 1;
end;

function TSeqDataExportOptionsDlg.IsRemove1stBase: Boolean;
begin
  Result := False;
  if FIsChooseBases then
    Result := not Pos1stChkBx.Checked;
end;

function TSeqDataExportOptionsDlg.IsRemove2ndBase: Boolean;
begin
  Result := False;
  if FIsChooseBases then
    Result := not Pos2ndChkBx.Checked;
end;

function TSeqDataExportOptionsDlg.IsRemove3rdBase: Boolean;
begin
  Result := False;
  if FIsChooseBases then
    Result := not Pos3rdChkBx.Checked;
end;

function TSeqDataExportOptionsDlg.IsRemove0thBase: Boolean;
begin
  Result := False;
  if FIsChooseBases then
    Result := not NoncodingChkBx.Checked;
end;

function TSeqDataExportOptionsDlg.IsRemoveMissData:Boolean;
begin
//0: Include sites with missing data and gaps
//1: Exclude sites with missing data and gaps
//2: Exclude sites with missing data
//3: Exclude sites with alignment gaps
  case MissingAndGapDataCBx.ItemIndex of
    0,3: Result := False;
  else
    Result := True;
  end;
end;

function TSeqDataExportOptionsDlg.IsRemoveGapData: Boolean;
begin
//0: Include sites with missing data and gaps
//1: Exclude sites with missing data and gaps
//2: Exclude sites with missing data
//3: Exclude sites with alignment gaps
  case MissingAndGapDataCBx.ItemIndex of
      0,2: Result := False;
  else
    Result := True;
  end;
end;

function TSeqDataExportOptionsDlg.IsExportAllSites: Boolean;
begin
  Result := (not FHasHighlightedSites) or (ExportSitesCBx.ItemIndex=0);
end;

function TSeqDataExportOptionsDlg.IsExportUnHighlightedSites: Boolean;
begin
  Result := (not FHasHighlightedSites) or (ExportSitesCBx.ItemIndex=2);
end;

function TSeqDataExportOptionsDlg.IsExportHighlightedSites: Boolean;
begin
  Result := (not FHasHighlightedSites) or (ExportSitesCBx.ItemIndex=1);
end;

function TSeqDataExportOptionsDlg.IsCodonByCodonWriting: Boolean;
begin
  Result := FIsNucData and FIsChooseBases;
  if Result then
    Result := Result and (not IsRemove1stBase) and
                         (not IsRemove2ndBase) and
                         (not IsRemove3rdBase);
  if Result and FHasHighlightedSites then
    Result := Result and IsExportAllSites;
end;

function TSeqDataExportOptionsDlg.ExportOptions: TSeqExportOptions;
begin
   Result := TSeqExportOptions.Create;
   Result.isNucData := IsNucData;
   Result.isChooseBases := IsChooseBases;
   Result.IsRemove1stBase := IsRemove1stBase;
   Result.IsRemove2ndBase := IsRemove2ndBase;
   Result.IsRemove3rdBase := IsRemove3rdBase;
   Result.IsRemove0thBase := IsRemove0thBase;
   Result.IsRemoveMissData := IsRemoveMissData;
   Result.IsRemoveGapData := IsRemoveGapData;
   Result.IsWriteSiteNumAtEnd := IsWriteSiteNumAtEnd;
   Result.IsWriteSiteNumAtTop := IsWriteSiteNumAtTop;
   Result.IsInterleaved := IsInterleaved;
   Result.IsExportHighlightedSites := IsExportHighlightedSites;
   Result.IsExportUnHighlightedSites := IsExportUnHighlightedSites;
   Result.IsPAUP4Format := IsPAUP4Format;
   Result.IsPAUP3Format := IsPAUP3Format;
   Result.IsMEGAFormat := IsMEGAFormat;
   Result.IsFASTAFormat := IsFASTAFormat;
   Result.IsPhylipFormat := IsPhylipFormat;
   Result.IsEXCELFormat := IsEXCELFormat;
   Result.IsCSVFormat := IsCSVFormat;
   Result.IsCodonByCodonWriting := IsCodonByCodonWriting;
   Result.SitesPerLine := SitesPerLine;
   Result.FileTitle := FileTitle;
   Result.FileDescription := FileDescription;
   Result.IsStandardNames := false;
   case FormatCBx.ItemIndex of
       0: Result.ExportFormat := mefMega;
       1: Result.ExportFormat := mefPaup4;
       2: Result.ExportFormat := mefPaup3;
       3: Result.ExportFormat := mefPhylip;
       4: Result.ExportFormat := mefExcel;
       5: Result.ExportFormat := mefCsv;
       6: Result.ExportFormat := mefFasta;
   end;
end;

procedure TSeqDataExportOptionsDlg.ForceSeqMax;
begin
  if (FormatCBx.ItemIndex = 4) then // // BUGFIX: No reason CSV should be lmiited to only 250 sites per line. CSV was ItemIndex 5.
  begin
    InterleavedChkBx.Checked := true;
    InterleavedChkBx.Enabled := false;
    SitesPerLineSE.MaxValue := 250;
    if SitesPerLineSE.Value > 250 then SitesPerLineSE.Value := 250;
  end
  else if (FormatCBx.ItemIndex = 6) then
  begin
    SitesPerLineSe.MaxValue := 50000000;
  end
  else
  begin
    SitesPerLineSe.MaxValue := 50000000;
    InterleavedChkBx.Enabled := true;
  end;
end;

end.

