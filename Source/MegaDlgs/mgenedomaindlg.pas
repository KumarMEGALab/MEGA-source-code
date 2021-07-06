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

unit MGeneDomainDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, Grids, MDomainInfo, msitepickform, mgridcolumnresizer,
  genedomainpropertiesdlg, LCLType, LCLIntf, ActnList, Menus, StdCtrls,
  MegaConsts, mimageform;

const
  GENE_NAME_INDEX = 0;
  DOMAIN_NAME_INDEX = 1;
  FROM_INDEX = 2;
  TO_INDEX = 3;
  SITES_INDEX = 4;
  IS_CODING_INDEX = 5;
  CODON_START_INDEX = 6;

type

  { TGeneDomainDlg }

  TGeneDomainDlg = class(TForm)
    AutoHighlightSitesBtn: TButton;
    AutoHighlightComboBox: TComboBox;
    DeleteAction: TAction;
    AddDomainToGeneAction: TAction;
    CheckboxImages: TImageList;
    HelpBtnImages: TImageList;
    HelpBtn: TImage;
    Label1: TLabel;
    OkBtn: TImage;
    OkBtnImages: TImageList;
    Panel2: TPanel;
    SiteNumEdit: TEdit;
    InsertDomainAction: TAction;
    InsertGeneAction: TAction;
    ActionAddDomain: TAction;
    ActionAddGene: TAction;
    ActionList1: TActionList;
    GenesDomainsDrawGrid: TDrawGrid;
    GridImages: TImageList;
    EditBtnImage: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    DomainsPopupMenu: TPopupMenu;
    GenesPopupMenu: TPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    GridPopupMenu: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    DataPreviewPanel: TPanel;
    SeqGrid: TDrawGrid;
    SiteLabelGrid: TDrawGrid;
    PageControl1: TPageControl;
    Panel1: TPanel;
    GeneDomainNameSText: TStaticText;
    GTreeTabSheet: TTabSheet;
    SiteLabelsTabSheet: TTabSheet;
    GenomeTrackBar: TTrackBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionAddDomainExecute(Sender: TObject);
    procedure ActionAddGeneExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure AddDomainToGeneActionExecute(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenesDomainsDrawGridClick(Sender: TObject);
    procedure GenesDomainsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GenesDomainsDrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GenesDomainsDrawGridMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GenesDomainsDrawGridSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure GenomeTrackBarChange(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure InsertDomainActionExecute(Sender: TObject);
    procedure InsertGeneActionExecute(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SeqGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SeqGridTopleftChanged(Sender: TObject);
    procedure SiteLabelGridClick(Sender: TObject);
    procedure SiteLabelGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SiteLabelGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SiteLabelGridKeyPress(Sender: TObject; var Key: char);
    procedure SiteLabelGridTopleftChanged(Sender: TObject);
  private
    FHintLabel: TStaticText;
    FIndependentsCheckState: TMegaCheckBoxState;
    FColumnSizer: TGridColumnResizer;
    FEditBtnImg: TBitmap;
    FGeneCheckedImg: TBitmap;
    FGeneUncheckedImg: TBitmap;
    FGenePartialImg: TBitmap;
    FDomainCheckedImg: TBitmap;
    FDomainUncheckedImg: TBitmap;
    FDomainPartialImg: TBitmap;
    FCheckedImg: TBitmap;
    FUncheckedImg: TBitmap;
    FPartialCheckedImg: TBitmap;
    FGenesAndDomains: TList;
    FNumIndependentSites: Integer;
    IsEstablishingNewSite: Boolean;
    CurFocusRow: Integer;
    CurFocusCol: Integer;
    CurInfo:  TDomainInfo;
    procedure InitHintLabel;
    function NumGenesAndDomainsUsed: Integer;
    procedure UpdateMarksForDomainInfo(aInfo: TDomainInfo);
    function GeneNameIsUsed(geneName: String): Boolean;
    function DomainNameIsUsed(domainName:  String): Boolean;
    procedure ClearDomainInfo(aInfo: TDomainInfo);
    procedure AddDomainMarks(aInfo: TDomainInfo);
    procedure DrawRectEdges(ACanvas: TCanvas; ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
    procedure InitGridImages;
    procedure ClearGridImages;
    function GenesDomainsGridHeaderStr(aCol: Integer): String;
    function AddGtDomain(Info: TDomainInfo): TDomainInfo;
    function AddGtGene(Info: TDomainInfo):TDomainInfo;
    function GetGeneByName(geneName: String): TDomainInfo;
    procedure DeleteDomain(index: Integer; updateIndySiteCounts: Boolean);
    procedure UpdateIndependentSitesCount;
    procedure EstablishNewCurrentSite(NextSite: Integer);
    function HasCodingDna: Boolean;
    function GetLongestStrings: TStringList;
    function UpdateGeneCoordinates(aInfo: TDomainInfo; fromSite: Integer; toSite: Integer): Boolean;
  public
    IsInit: Boolean;
    FSitePicker: TSitePickForm;
    GenesDomainsPropsEditor: TGenesDomainsPropertiesEditor;
    procedure Initialize;
    function NextAvailableGeneName: String;
    function NextAvailableDomainName: String;
    procedure ShowHintLabel;
    procedure HideHintLabel;
    procedure RemoveInfo(aInfo: TDomainInfo);
    procedure AddInfo(aInfo: TDomainInfo);
    property NumIndependentSites: Integer read FNumIndependentSites;
  end;

var
  GeneDomainDlg: TGeneDomainDlg;

implementation

uses
  MD_InputSeqData, MegaVerConsts, MegaUtils, math, mhelpfiles, mhelpkeywords,
  ContextHelp_HC, MPleaseWait, MV_SeqDataExplorer;

{$R *.lfm}

{ TGeneDomainDlg }

procedure TGeneDomainDlg.FormCreate(Sender: TObject);
begin
  IsInit := False;
  HelpContext := HC_Genes_Domains_Dialog;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  FGenesAndDomains := TList.Create;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Genes/Domain Organization';
  InitGridImages;
  FColumnSizer := TGridColumnResizer.Create;
  FIndependentsCheckState := cbsChecked;
  DoubleBuffered := True;
  ImageForm.UpdateImgList(Self);
  PageControl1.ActivePage := GTreeTabSheet;
  InitHintLabel;
end;

procedure TGeneDomainDlg.GenesDomainsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aStr: String;
  x, y: Integer;
  ts: TTextStyle;
  aInfo: TDomainInfo = nil;
  aImg: TBitmap = nil;
  i: Integer;
begin
  if not Visible then Exit;
  ts := GenesDomainsDrawGrid.Canvas.TextStyle;
  ts.Layout := tlCenter;
  with GenesDomainsDrawGrid.Canvas do
  begin
    if aRow = 0 then
    begin
      aStr := GenesDomainsGridHeaderStr(aCol);
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      x := aRect.Left + 2;
      y := aRect.Top + 1;
      FillRect(aRect);
      ts.Alignment := taCenter;
      if aStr <> EmptyStr then
        TextRect(aRect, x, y, aStr, ts);
      DrawRectEdges(GenesDomainsDrawGrid.Canvas, aRect, aCol = 0, True, True, True);
    end
    else if aRow = (GenesDomainsDrawGrid.RowCount - 1) then
    begin
      if gdSelected in aState then
      begin
        Brush.Color := clHighlight;
        Brush.Style := bsSolid;
        FillRect(aRect);
        DrawRectEdges(GenesDomainsDrawGrid.Canvas, aRect, aCol = 0, True, False, True);
      end
      else
      begin
        Brush.Color := clWhite;
        FillRect(aRect);
      end;
      case aCol of
        GENE_NAME_INDEX:
          begin
            if D_InputSeqData.FDomainMarks.IsUsedIndependent then
              aImg := FDomainCheckedImg
            else
              aImg := FDomainUncheckedImg;
            x := aRect.Left + 1;
            y := aRect.Top + (aRect.Bottom - aRect.Top - aImg.Height) div 2;
            Draw(x, y, aImg);

            aStr := 'Independents';
            ts.Alignment := taLeftJustify;
            x := aRect.Left + aImg.Width;
            y := aRect.Top + 1;
            TextRect(aRect, x, y, aStr, ts);
          end;
        SITES_INDEX:
          begin
            aStr := IntToStr(FNumIndependentSites);
            ts.Alignment := taRightJustify;
            x := aRect.Right - 2;
            y := aRect.Top + 1;
            TextRect(aRect, x, y, aStr, ts);
          end;
        IS_CODING_INDEX:
          begin
            if D_InputSeqData.IsNuc then
            begin
              case FIndependentsCheckState of
                cbsChecked: aImg := FCheckedImg;
                cbsUnchecked: aImg := FUncheckedImg;
                cbsPartiallyChecked: aImg := FPartialCheckedImg;
              end;
              x := aRect.Left + 1;
              y := aRect.Top + (aRect.Bottom - aRect.Top - aImg.Height) div 2;
              Draw(x, y, aImg);
            end;
          end;
      end;
    end
    else
    begin
      if (aRow > FGenesAndDomains.Count) then
        Exit;
      if gdSelected in aState then
      begin
        Brush.Color := clHighlight;
        Brush.Style := bsSolid;
        FillRect(aRect);
        DrawRectEdges(GenesDomainsDrawGrid.Canvas, aRect, aCol = 0, True, False, True);
      end
      else
      begin
        Brush.Color := clWhite;
        FillRect(aRect);
      end;
      aInfo := TDomainInfo(FGenesAndDomains[aRow - 1]);
      case aCol of
        GENE_NAME_INDEX:
          begin
            if aInfo.IsGene or (aInfo.ParentDomain = nil) then
            begin
              if aInfo.IsGene then
              begin
                aStr := aInfo.GeneName;
                case aInfo.CheckBoxState of
                  cbsChecked: aImg := FGeneCheckedImg;
                  cbsUnchecked: aImg := FGeneUncheckedImg;
                  cbsPartiallyChecked: aImg := FGenePartialImg;
                end;
              end
              else
              begin
                aStr := aInfo.Name;
                case aInfo.CheckBoxState of
                cbsChecked: aImg := FDomainCheckedImg;
                cbsUnchecked: aImg := FDomainUncheckedImg;
                cbsPartiallyChecked: aImg := FDomainPartialImg;
                end;
              end;
              x := aRect.Left + 1;
              y := aRect.Top + (aRect.Bottom - aRect.Top - aImg.Height) div 2;
              Draw(x, y, aImg);
              ts.Alignment := taLeftJustify;
              x := aRect.Left + aImg.Width;
              y := aRect.Top + 1;
              TextRect(aRect, x, y, aStr, ts);
            end;
          end;
        DOMAIN_NAME_INDEX:
          begin
            if aInfo.IsDomain and (aInfo.ParentDomain <> nil) then
            begin
              case aInfo.CheckBoxState of
              cbsChecked: aImg := FDomainCheckedImg;
              cbsUnchecked: aImg := FDomainUncheckedImg;
              cbsPartiallyChecked: aImg := FDomainPartialImg;
              end;
              x := aRect.Left + 1;
              y := aRect.Top + (aRect.Bottom - aRect.Top - aImg.Height) div 2;
              Draw(x, y, aImg);

              aStr := aInfo.Name;
              ts.Alignment := taLeftJustify;
              x := aRect.Left + aImg.Width;
              y := aRect.Top + 1;
              TextRect(aRect, x, y, aStr, ts);
            end;
          end;
        FROM_INDEX:
          begin
            if aInfo.FromSite >= 0 then
              aStr := IntToStr(aInfo.FromSite + 1)
            else
              aStr := '?';
            ts.Alignment := taRightJustify;
            x := aRect.Right - 2;
            y := aRect.Top + 1;
            TextRect(aRect, x, y, aStr, ts);
          end;
        TO_INDEX:
          begin
            if aInfo.ToSite >= 0 then
              aStr := IntToStr(aInfo.ToSite + 1)
            else
              aStr := '?';
            ts.Alignment := taRightJustify;
            x := aRect.Right - 2;
            y := aRect.Top + 1;
            TextRect(aRect, x, y, aStr, ts);
          end;
        SITES_INDEX:
          begin
            if aInfo.NoOfSites >= 0 then
              aStr := IntToStr(aInfo.NoOfSites)
            else
              aStr := '0';
            ts.Alignment := taRightJustify;
            x := aRect.Right - 2;
            y := aRect.Top + 1;
            TextRect(aRect, x, y, aStr, ts);
          end;
        IS_CODING_INDEX:
          begin
            if D_InputSeqData.IsNuc then
            begin
              if aInfo.ChildDomains.Count = 0 then
              begin
                if aInfo.IsCoding then
                  aImg := FCheckedImg
                else
                  aImg := FUncheckedImg;
              end
              else
              begin
                if not aInfo.IsCoding then
                  aImg := FUncheckedImg
                else
                begin
                  aImg := FCheckedImg;
                  for i := 0 to aInfo.ChildDomains.Count - 1 do
                    if not TDomainInfo(aInfo.ChildDomains[i]).IsCoding then
                    begin
                      aImg := FPartialCheckedImg;
                      break;
                    end;
                end;
              end;
              x := aRect.Left + 1;
              y := aRect.Top + (aRect.Bottom - aRect.Top - aImg.Height) div 2;
              Draw(x, y, aImg);
            end
            else
            begin
              if aInfo.IsDomain then
              begin
                aImg := FEditBtnImg;
                x := aRect.Left + Round(aImg.Width / 3);
                y := aRect.Top;
                Draw(x, y, aImg);
              end;
            end;
          end;
        CODON_START_INDEX:
          begin
            if not D_InputSeqData.IsNuc then
              Exit;
            if aInfo.IsCoding and (not aInfo.IsGene) then
            begin
              case aInfo.CodonStart of
                0: aStr := '1st Site';
                1: aStr := '2nd Site';
                2: aStr := '3rd Site';
              end;
              ts.Alignment := taCenter;
              x := aRect.Left + 2;
              y := aRect.Top + 1;
              TextRect(aRect, x, y, aStr, ts);
            end;
          end;
        CODON_START_INDEX + 1:
          begin
            aImg := FEditBtnImg;
            x := aRect.Left + Round(aImg.Width / 3);
            y := aRect.Top + 1;
            Draw(x, y, aImg);
          end;
      end;
    end;
  end;
end;

procedure TGeneDomainDlg.DeleteActionExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
  Index, mResponse: Integer;
  aMsg: String;
begin
  Index := GenesDomainsDrawGrid.Selection.Top - 1;
  if (Index < 0) or (Index >= FGenesAndDomains.Count) then
    Exit;
  aInfo := TDomainInfo(FGenesAndDomains[Index]);
  if aInfo.IsGene then
    aMsg := 'Are you sure you want to delete the selected gene (' + AInfo.GeneName + ')?'
  else
    aMsg := 'Are you sure you want to delete the selected domain (' + AInfo.Name + ')?';
  mResponse := MessageDlg('Confirm Deletion', aMsg, mtConfirmation, mbYesNo, 0);
  if mResponse = mrYes then
    DeleteDomain(Index, True);
end;

procedure TGeneDomainDlg.FormActivate(Sender: TObject);
begin
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (NumGenesAndDomainsUsed = 0) and (not (FIndependentsCheckState = cbsChecked)) then
  begin
    ShowMessage('Cannot unselect all data. Please resolve this issue');
    CanClose := False;
    Exit;
  end;
  CanClose := True;
end;

function TGeneDomainDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TGeneDomainDlg.FormResize(Sender: TObject);
begin
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.FormShow(Sender: TObject);
begin
  Initialize;
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.GenesDomainsDrawGridClick(Sender: TObject);
begin
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.AddDomainToGeneActionExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
  newInfo: TDomainInfo = nil;
  index: Integer;
  mResponse: Integer;
begin
  index := GenesDomainsDrawGrid.Selection.Top - 1;
  if (index < 0) or (index >= FGenesAndDomains.Count) then
    Exit;
  aInfo := TDomainInfo(FGenesAndDomains[index]);
  if not aInfo.IsGene then
  begin
    ShowMessage('To add a domain to a gene, please select a gene first');
    Exit;
  end;
  try
    newInfo := TDomainInfo.Create;
    newInfo.Name := NextAvailableDomainName;
    newInfo.GeneName := aInfo.GeneName;
    newInfo.IsDomain := True;
    newInfo.ParentDomain := aInfo;
    newInfo.IsCoding := aInfo.IsCoding;
    GenesDomainsPropsEditor.InitFromDomainInfo(newInfo);
    mResponse := GenesDomainsPropsEditor.ShowModal;
    if mResponse = mrOk then
    begin
      aInfo.ChildDomains.Add(newInfo);
      UpdateMarksForDomainInfo(newInfo);
      if Index = (FGenesAndDomains.Count - 1) then
        FGenesAndDomains.Add(newInfo)
      else
      begin
        FGenesAndDomains.Insert(index + TDomainInfo(FGenesAndDomains[index]).ChildDomains.Count, newInfo);
      end;
      AddDomainMarks(newInfo);
      newInfo := nil;
      GenesDomainsDrawGrid.RowCount := (FGenesAndDomains.Count + 2);
      GenesDomainsDrawGrid.Invalidate;
    end;
  finally
    if Assigned(newInfo) then
      newInfo.Free;
  end;
end;

procedure TGeneDomainDlg.BitBtn2Click(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TGeneDomainDlg.ActionAddGeneExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
begin
  if FNumIndependentSites = 0 then
  begin
    ShowMessage('Cannot add a new gene because there are no independent sites');
    Exit;
  end;
  aInfo := TDomainInfo.Create;
  aInfo.GeneName := NextAvailableGeneName;
  aInfo.IsUsed := True;
  aInfo.IsGene := True;
  aInfo.IsCoding := HasCodingDna;
  FGenesAndDomains.Add(aInfo);
  GenesDomainsDrawGrid.RowCount := (FGenesAndDomains.Count + 2);
  GenesDomainsDrawGrid.Invalidate;
  D_InputSeqData.FDomainMarks.IsDirty  := True;
end;

procedure TGeneDomainDlg.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
var
  aBool: Boolean;
  selectedRow: Integer;
begin
  selectedRow := GenesDomainsDrawGrid.Selection.Top;
  aBool := (selectedRow <> (GenesDomainsDrawGrid.RowCount - 1));
  ActionAddGene.Enabled := True;
  InsertGeneAction.Enabled := True;
  ActionAddDomain.Enabled := True;
  InsertDomainAction.Enabled := True;
  AddDomainToGeneAction.Enabled := (aBool and TDomainInfo(FGenesAndDomains[selectedRow - 1]).IsGene);
  DeleteAction.Enabled := aBool and (FGenesAndDomains.Count > 0);
  Handled := True;
end;

procedure TGeneDomainDlg.ActionAddDomainExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
begin
  if FNumIndependentSites = 0 then
  begin
    ShowMessage('Cannot add a new domain because there are no independent sites');
    Exit;
  end;
  aInfo := TDomainInfo.Create;
  aInfo.Name := NextAvailableDomainName;
  aInfo.IsUsed := True;
  aInfo.IsDomain := True;
  aInfo.IsCoding := HasCodingDna;
  FGenesAndDomains.Add(aInfo);
  GenesDomainsDrawGrid.RowCount := (FGenesAndDomains.Count + 2);
  GenesDomainsDrawGrid.Invalidate;
  D_InputSeqData.FDomainMarks.IsDirty  := True;
end;

procedure TGeneDomainDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(FGenesAndDomains) then
    FreeAndNil(FGenesAndDomains);
  if Assigned(FColumnSizer) then
    FColumnSizer.Free;
  ClearGridImages;
end;

procedure TGeneDomainDlg.GenesDomainsDrawGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Sender <> GenesDomainsDrawGrid then
    Exit;
  case Key of
    VK_DELETE:
      if DeleteAction.Enabled then
        DeleteActionExecute(Sender);
  end;
end;

procedure TGeneDomainDlg.GenesDomainsDrawGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aCol: Integer = -1;
  aRow: Integer = -1;
  aInfo: TDomainInfo;
  aRect: TRect;
  myX: Integer;
  i: Integer;
  aName: String;
begin
  if not Visible then Exit;
  GenesDomainsDrawGrid.MouseToCell(x, y, aCol, aRow);
  aRect := GenesDomainsDrawGrid.CellRect(aCol, aRow);
  if aRow < 1 then
    Exit;
  if aRow = (GenesDomainsDrawGrid.RowCount - 1) then
  begin
    myX := X - aRect.Left;
    if (myX >= 0) and (myX <= FGeneCheckedImg.Width) and (aCol = GENE_NAME_INDEX) then
    begin
      D_InputSeqData.FDomainMarks.IsUsedIndependent :=  (not D_InputSeqData.FDomainMarks.IsUsedIndependent);
      GenesDomainsDrawGrid.InvalidateRow(aRow);
    end;
    if (myX >= 0) and (myX <= FGeneCheckedImg.Width) and (aCol = IS_CODING_INDEX) and D_InputSeqData.IsNuc then
    begin
      case FIndependentsCheckState of
        cbsChecked: FIndependentsCheckState := cbsUnchecked;
        cbsUnchecked, cbsPartiallyChecked: FIndependentsCheckState := cbsChecked;
      end;
      GenesDomainsDrawGrid.InvalidateRow(aRow);
    end;
    Exit;
  end;
  if FGenesAndDomains.Count = 0 then
    Exit;
  aInfo := TDomainInfo(FGenesAndDomains[aRow - 1]);
  case aCol of
    GENE_NAME_INDEX:
      begin
        if aInfo.IsGene or (aInfo.ParentDomain = nil) then
        begin
          myX := X - aRect.Left;
          if (myX >= 0) and (myX <= FGeneCheckedImg.Width) then
          begin
            aInfo.IsUsed := (not aInfo.IsUsed);
            GenesDomainsDrawGrid.InvalidateRow(aRow);
            if aInfo.ChildDomains.Count > 0 then
              for i := 0 to aInfo.ChildDomains.Count - 1 do
              begin
                TDomainInfo(aInfo.ChildDomains[i]).IsUsed := aInfo.IsUsed;
                GenesDomainsDrawGrid.InvalidateRow(aRow + i + 1);
              end;
            if Assigned(aInfo.ParentDomain) and (aRow > 0) then
              GenesDomainsDrawGrid.InvalidateRow(aRow - 1);
            D_InputSeqData.FDomainMarks.IsDirty  := True;
          end;
        end;
      end;
    DOMAIN_NAME_INDEX:
      begin
        if aInfo.IsDomain and (aInfo.ParentDomain <> nil) then
        begin
          myX := X - aRect.Left;
          if (myX >= 0) and (myX <= FDomainCheckedImg.Width) then
          begin
            aInfo.IsUsed := (not aInfo.IsUsed);
            GenesDomainsDrawGrid.InvalidateRow(aRow);
            if aInfo.ChildDomains.Count > 0 then
              for i := 0 to aInfo.ChildDomains.Count - 1 do
              begin
                TDomainInfo(aInfo.ChildDomains[i]).IsUsed := aInfo.IsUsed;
                GenesDomainsDrawGrid.InvalidateRow(aRow + i + 1);
              end;
            if Assigned(aInfo.ParentDomain) and (aRow > 0) then
              GenesDomainsDrawGrid.InvalidateRow(aRow - 1);
            D_InputSeqData.FDomainMarks.IsDirty  := True;
          end;
        end;
      end;
    IS_CODING_INDEX:
      begin
        if not D_InputSeqData.IsNuc then
        begin
          myX := X - aRect.Left;
          if (myX >= 0) and (myX <= FEditBtnImg.Width) then
          begin
            if aInfo.IsGene then
              Exit;
            GenesDomainsPropsEditor.IsEditingGene := aInfo.IsGene;
            GenesDomainsPropsEditor.InitFromDomainInfo(aInfo);
            if GenesDomainsPropsEditor.ShowModal = mrOk then
            begin
              UpdateMarksForDomainInfo(aInfo);
              GenesDomainsDrawGrid.InvalidateRow(aRow);
              if aInfo.ChildDomains.Count > 0 then
                for i := 0 to aInfo.ChildDomains.Count - 1 do
                begin
                  TDomainInfo(aInfo.ChildDomains[i]).IsUsed := aInfo.IsUsed;
                  GenesDomainsDrawGrid.InvalidateRow(aRow + i + 1);
                end;
              if Assigned(aInfo.ParentDomain) and (aRow > 0) then
                GenesDomainsDrawGrid.InvalidateRow(aRow - 1);
            end;
          end;
        end;
      end;
    CODON_START_INDEX + 1:
      begin
        myX := X - aRect.Left;
        if (myX >= 0) and (myX <= FEditBtnImg.Width) then
        begin
          if aInfo.IsGene then
          begin
            aName := aInfo.GeneName;
            if InputQuery('Edit Gene Name', 'Gene Name', aName) then
            begin
              aInfo.GeneName := aName;
              GenesDomainsDrawGrid.InvalidateRow(aRow);
            end;
            Exit;
          end;
          GenesDomainsPropsEditor.IsEditingGene := aInfo.IsGene;
          GenesDomainsPropsEditor.InitFromDomainInfo(aInfo);
          if GenesDomainsPropsEditor.ShowModal = mrOk then
          begin
            UpdateMarksForDomainInfo(aInfo);
            GenesDomainsDrawGrid.InvalidateRow(aRow);
            if aInfo.ChildDomains.Count > 0 then
              for i := 0 to aInfo.ChildDomains.Count - 1 do
              begin
                TDomainInfo(aInfo.ChildDomains[i]).IsUsed := aInfo.IsUsed;
                GenesDomainsDrawGrid.InvalidateRow(aRow + i + 1);
              end;
            if Assigned(aInfo.ParentDomain) and (aRow > 0) then
              GenesDomainsDrawGrid.InvalidateRow(aRow - 1);
          end;
        end;
      end;
  end;
end;

procedure TGeneDomainDlg.GenesDomainsDrawGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.GenomeTrackBarChange(Sender: TObject);
begin
  EstablishNewCurrentSite(GenomeTrackBar.Position);
end;

procedure TGeneDomainDlg.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TGeneDomainDlg.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TGeneDomainDlg.InsertDomainActionExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
  aRow: Integer;
begin
  if FNumIndependentSites = 0 then
  begin
    ShowMessage('Cannot add a new domain because there are no independent sites');
    Exit;
  end;
  aInfo := TDomainInfo.Create;
  aInfo.Name := NextAvailableDomainName;
  aInfo.IsDomain := True;
  aInfo.IsCoding := HasCodingDna;

  aRow := GenesDomainsDrawGrid.Selection.Top - 1;
  while (aRow > 0) and (TDomainInfo(FGenesAndDomains[aRow]).ParentDomain <> nil) do
    dec(aRow);
  FGenesAndDomains.Insert(aRow, aInfo);
  GenesDomainsDrawGrid.RowCount := (FGenesAndDomains.Count + 2);
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.InsertGeneActionExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
  aRow: Integer;
begin
  if FNumIndependentSites = 0 then
  begin
    ShowMessage('Cannot add a new gene because there are no independent sites');
    Exit;
  end;
  aInfo := TDomainInfo.Create;
  aInfo.GeneName := NextAvailableGeneName;
  aInfo.IsGene := True;
  aInfo.IsCoding := HasCodingDna;

  aRow := GenesDomainsDrawGrid.Selection.Top - 1;
  while (aRow > 0) and (TDomainInfo(FGenesAndDomains[aRow]).ParentDomain <> nil) do
    dec(aRow);
  FGenesAndDomains.Insert(aRow, aInfo);
  GenesDomainsDrawGrid.RowCount := (FGenesAndDomains.Count + 2);
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TGeneDomainDlg.OkBtnMouseEnter(Sender: TObject);
begin
  OkBtnImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TGeneDomainDlg.OkBtnMouseLeave(Sender: TObject);
begin
  OkBtnImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TGeneDomainDlg.PageControl1Change(Sender: TObject);
var
  selectedRow: Integer;
  selectedInfo: TDomainInfo;
begin
  if not Visible then Exit;
  HideHintLabel;
  if (Pagecontrol1.ActivePage = SiteLabelsTabSheet) then
  begin
    selectedRow := GenesDomainsDrawGrid.Selection.Top - 1;
    if (selectedRow >= 0) and (selectedRow < FGenesAndDomains.Count) then
      selectedInfo := TDomainInfo(FGenesAndDomains[selectedRow])
    else
      selectedInfo := nil;
    if (selectedInfo <> nil) and (selectedInfo.FromSite >= 0) then
        EstablishNewCurrentSite(selectedInfo.FromSite);
    if SeqGrid.Col < 0 then
      EstablishNewCurrentSite(0);
  end
  else if PageControl1.ActivePage = GTreeTabSheet then
    GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.SeqGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  PrevBrushColor, PrevPenColor : TColor;
  DispChar: AnsiChar;
  MakeYellowBackground: Boolean;
  ts: TTextStyle;
begin
  if not Visible then Exit;
  ts := SiteLabelGrid.Canvas.TextStyle;
  with SeqGrid, Canvas do
  begin
    if (ACol >= D_InputSeqData.FNoOfSites) or (ARow >= D_InputSeqData.NoOfTaxa) then // inc last row
      Exit;
    PrevBrushColor := Brush.Color;
    PrevPenColor   := Pen.Color;

    DispChar := PAnsiChar(D_InputSeqData.FOtuInfos[ARow].Data)[ACol];  // all data is displayed as such

    // we mark all parts of a focused gene or a domain by a yellow background
    MakeYellowBackground := False;
    if (D_InputSeqData.FDomainMarks[ACol] <> nil) and (CurInfo <> nil) then
    begin
      if CurInfo.IsGene then
        MakeYellowBackground := (D_InputSeqData.FDomainMarks[ACol].GeneName = CurInfo.Name)  // this is cool
      else if CurInfo = D_InputSeqData.FDomainMarks[ACol] then
        MakeYellowBackground := True;
    end;

    if MakeYellowBackground then
      Brush.Color := clYellow;

    // now we choose colors
    if (gdSelected in aState) then
    begin
      Brush.Color := clNavy;
      if MakeYellowBackground then Font.Color := clYellow
      else                         Font.Color := clWhite;
    end;

    FillRect(aRect);
    ts.Alignment := taCenter;
    with aRect do
      TextRect(aRect, (aRect.Left + aRect.Right) div 2, Top+2, DispChar, ts);

    // writing the coding frame
    with aRect do
    if (D_InputSeqData.FDomainMarks[ACol] <> nil) then
    begin
      if meg1stBase in D_InputSeqData.FDomainMarks[ACol].CodonSiteAttr[ACol] then
      begin
        Pen.Color   := clGray;
        PolyLine([Point(aRect.Right, aRect.Bottom-2), Point(aRect.Left+2,  aRect.Bottom - 2),
                  Point(aRect.Left+2,  aRect.Top+2),  Point(aRect.Right, aRect.Top+2)]);
      end
      else if meg2ndBase in D_InputSeqData.FDomainMarks[ACol].CodonSiteAttr[ACol] then
      begin
        Pen.Color   := clGray;
        MoveTo(aRect.Left, aRect.Bottom-2); LineTo(aRect.Right, aRect.Bottom-2);
        MoveTo(aRect.Left, aRect.Top+2);    LineTo(aRect.Right, aRect.Top+2);
      end
      else if meg3rdBase in D_InputSeqData.FDomainMarks[ACol].CodonSiteAttr[ACol] then
      begin
        Pen.Color   := clGray;
        PolyLine([Point(aRect.Left,    aRect.Bottom-2), Point(aRect.Right-2, aRect.Bottom-2),
                  Point(aRect.Right-2, aRect.Top+2),    Point(aRect.Left,  aRect.Top+2)]);
      end;
    end;

    Brush.Color := PrevBrushColor;
    Pen.Color   := PrevPenColor;
    if gdFocused in aState then
      DrawFocusRect(aRect);

    if gdSelected in aState then
    begin
      if ACol <> CurFocusCol then
        EstablishNewCurrentSite(ACol);
      CurFocusCol := ACol;
      if CurFocusRow <> ARow then
        CurFocusRow := ARow;
    end;
  end;
end;

procedure TGeneDomainDlg.SeqGridTopleftChanged(Sender: TObject);
begin
  if not Visible then Exit;
  if SeqGrid.Focused then
    if SiteLabelGrid.LeftCol <> SeqGrid.LeftCol then
      SitelabelGrid.LeftCol := SeqGrid.LeftCol;
end;

procedure TGeneDomainDlg.SiteLabelGridClick(Sender: TObject);
begin
  if SiteLabelGrid.Col <> SeqGrid.Col then
    SeqGrid.Col := SitelabelGrid.Col;
end;

procedure TGeneDomainDlg.SiteLabelGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  PrevBrushColor, PrevPenColor : TColor;
  DispChar: AnsiChar;
  ts: TTextStyle;
begin
  if not Visible then Exit;
  ts := SiteLabelGrid.Canvas.TextStyle;
  with SiteLabelGrid, Canvas do
  begin
    PrevBrushColor := Brush.Color;
    PrevPenColor   := Pen.Color;

    DispChar := D_InputSeqData.FDomainMarks.SiteLabel[ACol];

    // now we choose colors
    if (gdSelected in aState) and (gdFocused in aState) then
    begin
      Brush.Color := clNavy;
      Font.Color := clWhite;
    end
    else
    begin
      Brush.Color := clWhite;
      Font.Color := clBlack;
    end;

    FillRect(aRect);
    ts.Alignment := taCenter;
    ts.Layout := tlCenter;
    with aRect do
      TextRect(aRect, (aRect.Left + aRect.Right) div 2, aRect.Top+1, DispChar, ts);

    Brush.Color := PrevBrushColor;
    Pen.Color   := PrevPenColor;
  end;
end;

procedure TGeneDomainDlg.SiteLabelGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Akey: Char;
begin
  with SiteLabelGrid do
    case Key of
      VK_DELETE:
      begin
          AKey := ' ';
          SiteLabelGridKeyPress(Sender, AKey);
      end;
    end;
end;

procedure TGeneDomainDlg.SiteLabelGridKeyPress(Sender: TObject; var Key: char);
begin
  with SitelabelGrid do
  begin
    if IsValidSiteLabel(key) or (key = ' ') then
    begin
        if key = '_' then
          key := ' ';
        if D_InputSeqData.FDomainMarks.SiteLabel[Selection.Left] <> AnsiChar(key) then
        begin
          D_InputSeqData.FDomainMarks.SiteLabel[Selection.Left] := AnsiChar(key);
          SiteLabelGridDrawCell(self, Selection.Left, Selection.Top, CellRect(Selection.Left, Selection.Top), [gdSelected, gdFocused]);
        end;
        key := #0;
      end;
    end;
end;

procedure TGeneDomainDlg.SiteLabelGridTopleftChanged(Sender: TObject);
begin
  if SiteLabelGrid.Focused then
    if SiteLabelGrid.LeftCol <> SeqGrid.LeftCol then
      SeqGrid.LeftCol := SitelabelGrid.LeftCol;
end;

procedure TGeneDomainDlg.InitHintLabel;
begin
  FHintLabel := TStaticText.Create(Self);
  FHintLabel.Parent := SeqGrid;
  FHintLabel.Anchors := [akBottom, akRight];
  FHintLabel.Caption := 'You can auto-label sites by selecting an attribute type from the drop down list below and clicking the "Apply" button.';
  FHintLabel.Align := alBottom;
  FHintLabel.Visible := False;
end;

function TGeneDomainDlg.NumGenesAndDomainsUsed: Integer;
var
  i: Integer;
begin
  Result := 0;
  if not Assigned(FGenesAndDomains) then
    Exit;
  if FGenesAndDomains.Count > 0 then
    for i := 0 to FGenesAndDomains.Count - 1 do
      if TDomainInfo(FGenesAndDomains[i]).IsUsed then
        inc(Result);
end;

procedure TGeneDomainDlg.UpdateMarksForDomainInfo(aInfo: TDomainInfo);
var
  parentInfo, childInfo: TDomainInfo;
  i, j: Integer;
  isCoding: Boolean;
begin
  if aInfo.FromSite >= 0 then
    for i := aInfo.FromSite to aInfo.ToSite do
      D_InputSeqData.FDomainMarks[i] := nil;
  GenesDomainsPropsEditor.GetDomainInfoProperties(aInfo);

  parentInfo := aInfo.ParentDomain;
  if Assigned(parentInfo) then
  begin
    UpdateGeneCoordinates(parentInfo, aInfo.FromSite, aInfo.ToSite);
    if (parentInfo.ChildDomains.Count > 0) and D_InputSeqData.IsNuc then
    begin
      isCoding := False;
      for i := 0 to parentInfo.ChildDomains.Count - 1 do
        if TDomainInfo(parentInfo.ChildDomains[i]).IsCoding then
        begin
          isCoding := True;
          break;
        end;
      parentInfo.IsCoding := isCoding;
      GenesDomainsDrawGrid.InvalidateRow(FGenesAndDomains.IndexOf(parentInfo) + 1);
    end;
  end;

  for i := aInfo.FromSite to aInfo.ToSite do
    D_InputSeqData.FDomainMarks[i] := aInfo;
  if aInfo.ChildDomains.Count > 0 then
  begin
    for i := 0 to aInfo.ChildDomains.Count - 1 do
    begin
      childInfo := TDomainInfo(aInfo.ChildDomains[i]);
      for j := childInfo.FromSite to childInfo.ToSite do
        D_InputSeqData.FDomainMarks[j] := childInfo;
    end;
  end;
  UpdateIndependentSitesCount;
  D_InputSeqData.FDomainMarks.IsDirty  := True;
end;

function TGeneDomainDlg.GeneNameIsUsed(geneName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FGenesAndDomains.Count > 0 then
    for i := 0 to FGenesAndDomains.Count - 1 do
      if TDomainInfo(FGenesAndDomains[i]).IsGene and SameText(geneName, TDomainInfo(FGenesAndDomains[i]).GeneName) then
      begin
        Result := True;
        Exit;
      end;
end;

function TGeneDomainDlg.DomainNameIsUsed(domainName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FGenesAndDomains.Count > 0 then
    for i := 0 to FGenesAndDomains.Count - 1 do
      if TDomainInfo(FGenesAndDomains[i]).IsDomain and SameText(domainName, TDomainInfo(FGenesAndDomains[i]).Name) then
      begin
        Result := True;
        Exit;
      end;
end;

function TGeneDomainDlg.NextAvailableGeneName: String;
var
  i: Integer;
begin
  Result := 'Gene-1';
  i := 1;
  while GeneNameIsUsed(Result) and (i < MaxInt) do
  begin
    inc(i);
    Result := 'Gene-' + IntToStr(i);
  end;
end;

function TGeneDomainDlg.NextAvailableDomainName: String;
var
  i: Integer;
begin
  Result := 'Domain-1';
  i := 1;
  while DomainNameIsUsed(Result) and (i < MaxInt) do
  begin
    inc(i);
    Result := 'Domain-' + IntToStr(i);
  end;
end;

procedure TGeneDomainDlg.ShowHintLabel;
begin
  FHintLabel.Visible := True;
  FHintLabel.BringToFront;
end;

procedure TGeneDomainDlg.HideHintLabel;
begin
  FHintLabel.Visible := False;
end;

procedure TGeneDomainDlg.RemoveInfo(aInfo: TDomainInfo);
begin
  try
    BeginFormUpdate;
    FGenesAndDomains.Remove(aInfo);
    UpdateIndependentSitesCount;
    GenesDomainsDrawGrid.RowCount := FGenesAndDomains.Count + 2;
    GenesDomainsDrawGrid.Invalidate;
  finally
    EndFormUpdate;
  end;
end;

procedure TGeneDomainDlg.AddInfo(aInfo: TDomainInfo);
var
  index: Integer;
begin
  if Assigned(aInfo.ParentDomain) then
  begin
    index := FGenesAndDomains.IndexOf(aInfo.ParentDomain);
    if index >= 0 then
    begin
      if index = (FGenesAndDomains.Count - 1) then
        FGenesAndDomains.Add(aInfo)
      else
        FGenesAndDomains.Insert(index + TDomainInfo(FGenesAndDomains[index]).ChildDomains.Count, aInfo);
    end
    else
      raise Exception.Create('gene domain dialog missing parent domain info');
  end
  else
    FGenesAndDomains.Add(aInfo);
  UpdateIndependentSitesCount;
  GenesDomainsDrawGrid.RowCount := FGenesAndDomains.Count + 2;
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.DeleteDomain(index: Integer; updateIndySiteCounts: Boolean);
var
  aInfo: TDomainInfo = nil;
  childInfo: TDomainInfo = nil;
  i: Integer;
begin
  if (index < 0) or (index >= FGenesAndDomains.Count) then
    Exit;
  aInfo := TDomainInfo(FGenesAndDomains[index]);
  if aInfo.ChildDomains.Count > 0 then
    for i := 0 to aInfo.ChildDomains.Count - 1 do
    begin
      childInfo := TDomainInfo(aInfo.ChildDomains[i]);
      ClearDomainInfo(childInfo);
      ChildInfo.Free;
    end;
  ClearDomainInfo(aInfo);
  aInfo.Free;
  D_InputSeqData.FDomainMarks.IsDirty := True;
  if updateIndySiteCounts then
    UpdateIndependentSitesCount;
  GenesDomainsDrawGrid.Invalidate;
end;

procedure TGeneDomainDlg.ClearDomainInfo(aInfo: TDomainInfo);
var
  i: Integer;
begin
  if aInfo.NoOfSites > 0 then
    for i := aInfo.FromSite to aInfo.ToSite do
      D_InputSeqData.FDomainMarks[i] := nil;
  D_InputSeqData.AllDomainInfo.Remove(aInfo);
  FGenesAndDomains.Remove(aInfo);
  GenesDomainsDrawGrid.RowCount := FGenesAndDomains.Count + 2;
end;

procedure TGeneDomainDlg.AddDomainMarks(aInfo: TDomainInfo);
var
  i: Integer;
begin
  if aInfo.NoOfSites > 0 then
    for i := aInfo.FromSite to aInfo.ToSite do
      D_InputSeqData.FDomainMarks[i] := aInfo;
end;

procedure TGeneDomainDlg.DrawRectEdges(ACanvas: TCanvas; ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clSilver;
  if DoLeft then
    ACanvas.Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
  if DoTop then
    ACanvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
  if DoRight then
    ACanvas.Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
  if DoBottom then
    ACanvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
end;

procedure TGeneDomainDlg.InitGridImages;
begin
  FEditBtnImg := TBitmap.Create;
  FGeneCheckedImg := TBitmap.Create;
  FGeneUncheckedImg := TBitmap.Create;
  FGenePartialImg := TBitmap.Create;
  FDomainCheckedImg := TBitmap.Create;
  FDomainUncheckedImg := TBitmap.Create;
  FDomainPartialImg := TBitmap.Create;
  FCheckedImg := TBitmap.Create;
  FUncheckedImg := TBitmap.Create;
  FPartialCheckedImg := TBitmap.Create;
  EditBtnImage.GetBitmap(0, FEditBtnImg);
  GridImages.GetBitmap(0, FDomainCheckedImg);
  GridImages.GetBitmap(1, FDomainPartialImg);
  GridImages.GetBitmap(2, FDomainUncheckedImg);
  GridImages.GetBitmap(3, FGeneCheckedImg);
  GridImages.GetBitmap(4, FGenePartialImg);
  GridImages.GetBitmap(5, FGeneUncheckedImg);
  CheckboxImages.GetBitmap(0, FCheckedImg);
  CheckboxImages.GetBitmap(1, FUncheckedImg);
  CheckboxImages.GetBitmap(2, FPartialCheckedImg);
  {$IFDEF DARWIN}
  HelpBtn.Proportional:=True;
  OkBtn.Proportional:=True;
  {$ENDIF}
end;

procedure TGeneDomainDlg.ClearGridImages;
begin
  FEditBtnImg.Free;
  FGeneCheckedImg.Free;
  FGeneUncheckedImg.Free;
  FGenePartialImg.Free;
  FDomainCheckedImg.Free;
  FDomainUncheckedImg.Free;
  FDomainPartialImg.Free;
  FCheckedImg.Free;
  FUncheckedImg.Free;
  FPartialCheckedImg.Free;
end;

function TGeneDomainDlg.GenesDomainsGridHeaderStr(aCol: Integer): String;
begin
  Result := EmptyStr;
  case aCol of
    0: Result := 'Level 1';
    1: Result := 'Level 2';
    2: Result := 'From';
    3: Result := 'To';
    4: Result := '#Sites';
    5:
      begin
        if D_InputSeqData.IsNuc then
          Result := 'Is Coding';
      end;
    6:
      begin
        if D_InputSeqData.IsNuc then
          Result := 'Codon Start';
      end;
    7: Result := EmptyStr;
  end;
end;

function TGeneDomainDlg.AddGtDomain(Info: TDomainInfo): TDomainInfo;
begin
  try
    D_InputSeqData.FDomainMarks.IsDirty := True;
    Info.IsDomain := True;
    if Trim(Info.GeneName) = EmptyStr then  // Free floating domains are added at the root level
      FGenesAndDomains.Add(Info)
    else
    begin
      Result := AddGtGene(Info);
      FGenesAndDomains.Add(Info);
    end;
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when initializing domains: ' + E.Message);
  end;
end;

function TGeneDomainDlg.AddGtGene(Info: TDomainInfo): TDomainInfo;
begin
  Result := nil;
  try
    if Trim(Info.GeneName) = EmptyStr then
      Exit;
    Result := GetGeneByName(Info.GeneName);
    if Result <> nil then
      Exit;

    D_InputSeqData.FDomainMarks.IsDirty := True;
    Result := TDomainInfo.Create;
    Result.GeneName := Info.GeneName;
    Result.FromSite := Info.FromSite;
    Result.ToSite := Info.ToSite;
    Result.IsGene := True;
    Result.IsCoding := Info.IsCoding;
    Result.CodonStart := Info.CodonStart;
    Info.ParentDomain := Result;
    Result.ChildDomains.Add(Info);
    FGenesAndDomains.Add(Result);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when setting up domains: ' + E.Message);
  end;
end;

function TGeneDomainDlg.GetGeneByName(geneName: String): TDomainInfo;
var
  i: Integer;
begin
  Result := nil;
  if FGenesAndDomains.Count > 0 then
    for i := 0 to FGenesAndDomains.Count - 1 do
    begin
      if TDomainInfo(FGenesAndDomains[i]).GeneName = geneName then
      begin
        Result := TDomainInfo(FGenesAndDomains[i]);
        Exit;
      end;
    end;
end;

procedure TGeneDomainDlg.UpdateIndependentSitesCount;
var
  i: Integer;
begin
  FNumIndependentSites := 0;
  for i := 0 to D_InputSeqData.FNoOfSites - 1 do
    if D_InputSeqData.FDomainMarks[i] = nil then
      Inc(FNumIndependentSites);
  GenesDomainsDrawGrid.InvalidateRow(GenesDomainsDrawGrid.RowCount - 1);
end;

procedure TGeneDomainDlg.EstablishNewCurrentSite(NextSite: Integer);
begin
  if IsEstablishingNewSite then
    exit;
  IsEstablishingNewSite:= True;
  GenomeTrackBar.Position := NextSite;
  with SeqGrid do
  begin
    if (NextSite < LeftCol) or (NextSite >= (LeftCol + VisibleColCount)) then
      LeftCol  := NextSite;
    Col := NextSite;
    Invalidate;
  end;

  with SiteLabelGrid do
  begin
    if (NextSite < LeftCol) or (NextSite >= (LeftCol + VisibleColCount)) then
      LeftCol  := NextSite;
    Col := NextSite;
    Invalidate;
  end;

  SiteNumEdit.Text := 'Site #'+IntToStr(NextSite+1);

  if D_InputSeqData.FDomainMarks[NextSite] = nil then
    GeneDomainNameSText.Caption := EmptyStr
  else
    with D_InputSeqData.FDomainMarks[NextSite] do
    begin
      if Length(GeneName) > 0 then
        GeneDomainNameSText.Caption := GeneName + '('+Name+')'
      else
        GeneDomainNameSText.Caption := Name;
    end;
  IsEstablishingNewSite:= False;
end;

function TGeneDomainDlg.HasCodingDna: Boolean;
begin
  Result := (D_InputSeqData.FIsCoding and D_InputSeqData.FIsNuc);
end;

function TGeneDomainDlg.GetLongestStrings: TStringList;
var
  i: Integer;
  aInfo: TDomainInfo;
begin
  Result := TStringList.Create;
  Result.Add('Gene/Domain Name');
  Result.Add('Domain Name');
  Result.Add('From');
  Result.Add('To');
  Result.Add('#Sites');
  if D_InputSeqData.IsNuc then
  begin
    Result.Add('Is Coding');
    Result.Add('Codon Start');
  end;
  Result.Add('---EDITBTN---');
  Result.Add(' ');

  if FGenesAndDomains.Count > 0 then
    for i := 0 to FGenesAndDomains.Count - 1 do
    begin
      aInfo := TDomainInfo(FGenesAndDomains[i]);
      if Length(aInfo.GeneName) > Length(Result[GENE_NAME_INDEX]) then
        Result[GENE_NAME_INDEX] := aInfo.GeneName;
      if aInfo.IsDomain and (Length(aInfo.Name) > Length(Result[DOMAIN_NAME_INDEX])) then
        Result[DOMAIN_NAME_INDEX] := aInfo.Name;
      if Length(IntToStr(aInfo.FromSite)) > Length(Result[FROM_INDEX]) then
        Result[FROM_INDEX] := IntToStr(aInfo.FromSite);
      if Length(IntToStr(aInfo.ToSite)) > Length(Result[TO_INDEX]) then
        Result[TO_INDEX] := IntToStr(aInfo.ToSite);
    end;
end;

function TGeneDomainDlg.UpdateGeneCoordinates(aInfo: TDomainInfo; fromSite: Integer; toSite: Integer): Boolean;
var
  i, j: Integer;
  childInfo: TDomainInfo;
begin
  Result := True;
  if aInfo.FromSite >= 0 then
    for i := aInfo.FromSite to aInfo.ToSite do
      D_InputSeqData.FDomainMarks[i] := nil;
  aInfo.FromSite := fromSite;
  aInfo.ToSite := toSite;
  if aInfo.ChildDomains.Count > 0 then
    for i := 0 to aInfo.ChildDomains.Count - 1 do
    begin
      childInfo := TDomainInfo(aInfo.ChildDomains[i]);
      aInfo.FromSite := Min(aInfo.FromSite, childInfo.FromSite);
      aInfo.ToSite := Max(aInfo.ToSite, childInfo.ToSite);
    end;
  Result := (aInfo.FromSite <= aInfo.ToSite);
  if Result then
  begin
    for i := aInfo.FromSite to aInfo.ToSite do
      D_InputSeqData.FDomainMarks[i] := aInfo;
    if aInfo.ChildDomains.Count > 0 then
      for i := 0 to aInfo.ChildDomains.Count - 1 do
      begin
        childInfo := TDomainInfo(aInfo.ChildDomains[i]);
        for j := childInfo.FromSite to childInfo.ToSite do
          D_InputSeqData.FDomainMarks[j] := childInfo;
      end;
  end;
end;

procedure TGeneDomainDlg.Initialize;
var
  i: integer;
  longestStrings: TStringList = nil;
  pw: TPleaseWait = nil;
begin
  if IsInit then
    Exit;
  try
    pw := TPleaseWait.Create(Self);
    pw.Caption := 'Please Wait...';
    pw.Action := 'Initializing genes and domains';
    pw.SetToMarqueeMode;
    pw.Show;
    if not IsInit then
      V_SeqDataExplorer.InitGeneDomainsPropertyEditor;
    IsInit := True;
    if D_InputSeqData.AllDomainInfo.NoOfDomains > 0 then
      for i:=0 to D_InputSeqData.AllDomainInfo.NoOfDomains-1 do
      begin
        AddGtDomain(D_InputSeqData.AllDomainInfo[i]);
        D_InputSeqData.AllDomainInfo[i] := nil;
        Application.ProcessMessages;
      end;
    GenesDomainsDrawGrid.RowCount := FGenesAndDomains.Count + 2;
    if D_InputSeqData.IsNuc then
      GenesDomainsDrawGrid.ColCount := 9
    else
      GenesDomainsDrawGrid.ColCount := 7;
    longestStrings := GetLongestStrings;
    FColumnSizer.ResizeGridColumns(GenesDomainsDrawGrid, longestStrings);

    GenomeTrackBar.Min := 0;
    GenomeTrackBar.Max := D_InputSeqData.FNoOfSites - 1;
    with SeqGrid do
    begin
      DefaultColWidth  := Canvas.TextWidth('W') + 4;
      DefaultRowHeight := Canvas.TextHeight(' ') + 2;
      RowCount := 1;
      ColCount := D_InputSeqData.FNoOfSites;
      FixedRows := 0;
      FixedCols := 0;
    end;

    with SiteLabelGrid do
    begin
      DefaultColWidth  := Canvas.TextWidth('W') + 4;
      DefaultRowHeight := Canvas.TextHeight(' ') + 2;
      RowCount := 1;
      ColCount := D_InputSeqData.FNoOfSites;
      FixedRows := 0;
      FixedCols := 0;
    end;
    EstablishNewCurrentSite(0);
    if FGenesAndDomains.Count > 0 then
      for i := 0 to FGenesAndDomains.Count - 1 do
        if TDomainInfo(FGenesAndDomains[i]).IsGene then
          TDomainInfo(FGenesAndDomains[i]).UpdateGeneBoundaries;

    UpdateIndependentSitesCount;
  finally
    if Assigned(longestStrings) then
      longestStrings.Free;
    if Assigned(pw) then
      pw.Free;
  end;
end;

end.

