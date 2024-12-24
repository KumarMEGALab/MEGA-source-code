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

unit MWriteOutputDlg;

{$mode objfpc}{$H+}

interface

uses
  LCLIntF, LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, EditBtn, IniPropStorage, ActnList,
  ComCtrls, MegaConsts, mimageform;

const
  IS_USE_ALL_SITES = 0;
  IS_USE_HIGHLIGHTED_SITES = 1;
  IS_USE_UNHIGHLIGHTED_SITES = 2;

type

  { TWriteOutputDlg }

  TWriteOutputDlg = class(TForm)
    OkAction: TAction;
    HelpAction: TAction;
    CancelAction: TAction;
    ActionList1: TActionList;
    ChooseSitesLabel: TLabel;

    directoryLbl: TLabel;
    DisplayOnlyBtn: TRadioButton;
    FilenamePanel: TPanel;
    FilePickBtn: TSpeedButton;
    IniPropStorage1: TIniPropStorage;
    outputFilenameLbl: TLabel;
    Panel4: TPanel;
    SaveDlg: TSaveDialog;
    SaveToDiskRadioBtn: TRadioButton;
    selDirectory: TEdit;
    {$IFDEF DARWIN}
    SelectOutputCBx: TRadioGroup;
    chosenSitesCBx: TRadioGroup;
    {$ELSE}
    SelectOutputCBx: TComboBox;
    chosenSitesCBx: TComboBox;
    {$ENDIF}
    OutputFormatLbl: TLabel;
    FormatAndFilePanel: TPanel;
    ExportExcelDisabled: TLabel;
    selFilename: TEdit;
    SitesPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    {$IFDEF DARWIN}
    procedure InitOutputRadioGroup;
    procedure InitSitesRadioGroup;
    procedure OutputRadioGroupSelectionChanged(Sender: TObject);
    procedure SitesRadioGroupSelectionChanged(Sender: TObject);
    {$ELSE}
    procedure InitOutputComboBox;
    procedure InitSitesComboBox;
    {$ENDIF}
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure chosenSitesCBxChange(Sender: TObject);
    procedure DisplayOnlyBtnClick(Sender: TObject);
    procedure FilenamePanelPaint(Sender: TObject);
    procedure FilePickBtnClick(Sender: TObject);
    procedure FormatAndFilePanelClick(Sender: TObject);
    procedure FormatAndFilePanelPaint(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SaveToDiskRadioBtnClick(Sender: TObject);
    procedure SelectOutputCBxChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure selDirectoryChange(Sender: TObject);
    procedure selFilenameChange(Sender: TObject);
    procedure ExportExcelDisabledClick(Sender: TObject);
    procedure EnableDisableFilenameOptions;
    procedure SitesPanelPaint(Sender: TObject);
    procedure UpdateFileExtension;
  private
    FSelectedFormat: String;
    FSitesOption: Integer;
    FHideSites: Boolean;
    FAllowedExportTypes: TExportTypeSet;
    procedure DrawPanelBorder(aPanel: TPanel);
    procedure InitForm;
    function GetSelectedOutputFormat: String;
    function GetExportFormat: TExportType;
    function GetOutputFile: String;

    function GetIsUseAllSites: Boolean;
    function GetIsUseHighlightedSites: Boolean;
    function GetIsUseUnHighlightedSites: Boolean;
    function HighlightInvalidDirectory: Boolean;
    function HighlightInvalidFileName: Boolean;
    procedure SetExportFormat(AValue: TExportType);
    procedure ShowErrorsInFileParameters;
    function IndexOfChoice(Choice: TExportType): integer;
  public
    procedure UpdateSaveOptions;
    procedure SetFileName(Filename: String);
    procedure Disallow(ToDisable : TExportType);
    procedure Allow(toAllow: TExportType);
    function ValidFileName(FileName: String): Boolean;
    property EXoutputFormat: TExportType read GetExportFormat write SetExportFormat;
    property OutputFile:           String read GetOutputFile;
    property SelectedOutputFormat: String read GetSelectedOutputFormat;

    property IsUseAllSites: Boolean read GetIsUseAllSites;
    property IsUseHighlightedSites: Boolean read GetIsUseHighlightedSites;
    property IsUseUnHighlightedSites: Boolean read GetIsUseUnHighlightedSites;
    property HideSitesPanel: Boolean read FHideSites write FHideSites; // Called before show to hide the sites panel for that single show.
  end;

function PromptUserWriteOutput(var SaveLocation: String; MyHideSitesPanel: Boolean = True; DefaultFormat: TExportType=EXnone): TExportType;

const
  AttentionNeededColor = TColor($FFE5EB);

var
  WriteOutputDlg: TWriteOutputDlg;

implementation

{$R *.lfm}

{ TWriteOutputDlg }
uses
  ExcelWrite, MegaVerConsts, LazFileUtils, DataExplorerHelp_HC, mhelpfiles, mhelpkeywords;

function PromptUserWriteOutput(var SaveLocation: String; MyHideSitesPanel: Boolean = True; DefaultFormat: TExportType=EXnone): TExportType;
begin
  Result := EXnone;
  WriteOutputDlg.HideSitesPanel := MyHideSitesPanel;
  if Trim(SaveLocation) <> EmptyStr then
  begin
    WriteOutputDlg.selFilename.Text := ChangeFileExt(SaveLocation, '.txt');
    WriteOutputDlg.selDirectory.Text := GetCurrentDirUTF8 + PathDelim;
  end;

  if DefaultFormat <> ExNone then
    WriteOutputDlg.EXoutputFormat := DefaultFormat;
  if WriteOutputDlg.ShowModal <> mrOK then
    Exit;
  Result := WriteOutputDlg.EXOutputFormat;
  case Result of
    EXtext, EXFasta: ;
    EXtextSave, EXcsvSave, EXexcelSave, EXexcelXmlSave, EXodsSave, EXfastaSave: SaveLocation := WriteOutputDlg.OutputFile;
    EXcsvDisp, EXexcelDisp, EXexcelXmlDisp, EXodsDisp: SaveLocation := ExcelWrite.GetSaveLocation(Result);
    EXInvalid, EXnone: Raise Exception.Create('Prompt user for output format returned EXNone, rather than a valid result type.');
  end;
end;

procedure TWriteOutputDlg.FilePickBtnClick(Sender: TObject);
begin
  with SaveDlg do
  begin
    case GetExportFormat of
      EXtext, EXtextSave: DefaultExt := 'txt';
      EXcsvDisp, EXcsvSave: DefaultExt := 'csv';
      EXexcelDisp, EXexcelSave: DefaultExt := 'xls';
      EXexcelXmlDisp, EXexcelXmlSave: DefaultExt := 'xlsx';
      EXodsDisp, EXodsSave: DefaultExt := 'ods';
      EXfasta, EXfastaSave: DefaultExt := 'fas';
    end;
    if (Trim(selFileName.Text) = EmptyStr) and (Trim(selDirectory.Text) = EmptyStr) then
    begin

      FileName := 'Result.' + DefaultExt;
    end
    else
    begin
      FileName := Trim(selFileName.Text);
      if DirectoryExists(selDirectory.Text) then
        SaveDlg.FileName := ExtractFilePath(selDirectory.Text) + FileName;
      if ExtractFileExt(FileName) <> EmptyStr then
        SaveDlg.Filter := ExtractFileExt(FileName) + ' files (*' + ExtractFileExt(FileName) + ')|*' + ExtractFileExt(FileName) + '|All files (*.*)|*.*';
    end;
    InitialDir := GetCurrentDirUTF8;
    if Execute then
    begin
      selDirectory.text := ExtractFilePath(SaveDlg.FileName);
      selFilename.Text := ExtractFileName(SaveDlg.FileName);
    end;
  end;
end;

procedure TWriteOutputDlg.FormatAndFilePanelClick(Sender: TObject);
begin

end;

procedure TWriteOutputDlg.FormatAndFilePanelPaint(Sender: TObject);
begin
  if not Visible then
    Exit;
  DrawPanelBorder(FormatAndFilePanel);
end;

procedure TWriteOutputDlg.DisplayOnlyBtnClick(Sender: TObject);
begin
  EnableDisableFilenameOptions;
end;

procedure TWriteOutputDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if Panel4.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel4.Width - ToolBar1.Width)/2);
end;

{$IFNDEF DARWIN}
procedure TWriteOutputDlg.InitOutputComboBox;
begin
  SelectOutputCBx := TComboBox.Create(Self);
  SelectOutputCBx.Parent := FormatAndFilePanel;
  SelectOutputCBx.OnChange := @SelectOutputCBxChange;
  SelectOutputCBx.BorderStyle := bsSingle;
  SelectOutputCBx.Height := 27;
  SelectOutputCBx.ItemHeight := 19;
  SelectOutputCBx.Left := 20;
  SelectOutputCBx.Top := 29;
  SelectOutputCBx.Width := 405;
  SelectOutputCBx.Style := csDropDownList;
end;



procedure TWriteOutputDlg.InitSitesComboBox;
begin
  chosenSitesCBx := TComboBox.Create(Self);
  chosenSitesCBx.Parent := SitesPanel;
  chosenSitesCBx.Height := 27;
  chosenSitesCBx.ItemHeight := 19;
  chosenSitesCBx.Left := 20;
  chosenSitesCBx.Top := 40;
  chosenSitesCBx.Style := csOwnerDrawFixed;
  chosenSitesCBx.Text := 'Use all sites';
  chosenSitesCBx.Width := 405;
  chosenSitesCBx.OnChange := @chosenSitesCBxChange;
  chosenSitesCBx.Items.Add('Use all sites');
  chosenSitesCBx.Items.Add('Use only highlighted sites');
  chosenSitesCBx.Items.Add('Use only UNhighlighted sites');
  chosenSitesCBx.ItemIndex := 0;
end;
{$ELSE}

procedure TWriteOutputDlg.InitOutputRadioGroup;
begin
  SelectOutputCBx := TRadioGroup.Create(Self);
  SelectOutputCBx.Parent := FormatAndFilePanel;
  SelectOutputCBx.OnSelectionChanged := @OutputRadioGroupSelectionChanged;
  SelectOutputCBx.Left := 20;
  SelectOutputCBx.Top := 29;
  SelectOutputCBx.Width := 405;
end;

procedure TWriteOutputDlg.InitSitesRadioGroup;
begin
  chosenSitesCBx := TRadioGroup.Create(Self);
  chosenSitesCBx.Parent := SitesPanel;
  chosenSitesCBx.Left := 20;
  chosenSitesCBx.Top := 35;
  chosenSitesCBx.Width := 405;
  chosenSitesCBx.OnSelectionChanged := @SitesRadioGroupSelectionChanged;
  chosenSitesCBx.Items.Add('Use all sites');
  chosenSitesCBx.Items.Add('Use only highlighted sites');
  chosenSitesCBx.Items.Add('Use only UNhighlighted sites');
  chosenSitesCBx.ItemIndex := 0;
end;

procedure TWriteOutputDlg.OutputRadioGroupSelectionChanged(Sender: TObject);
begin
  EnableDisableFilenameOptions;
  if selFileName.Text <> 'please provide a name' then
  begin
    if Trim(selFileName.Text) = EmptyStr then
      selFileName.Text := 'Result';
    if DisplayOnlyBtn.Checked then
      FileNamePanel.Color := Color
    else
    begin
      UpdateFileExtension;
      FilenamePanel.Color := Color;
      FilenamePanel.Invalidate;
    end;
    ShowErrorsInFileParameters;
  end;
  if SelectOutputCBx.ItemIndex >= 0 then
    FSelectedFormat := SelectOutputCBx.Items[SelectOutputCBx.ItemIndex]
  else
    FSelectedFormat := EmptyStr;
end;

procedure TWriteOutputDlg.SitesRadioGroupSelectionChanged(Sender: TObject);
begin
  FSitesOption := chosenSitesCBx.ItemIndex;
end;

{$ENDIF}

procedure TWriteOutputDlg.FormResize(Sender: TObject);
begin
  if Panel4.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel4.Width - ToolBar1.Width)/2);
end;


procedure TWriteOutputDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWriteOutputDlg.chosenSitesCBxChange(Sender: TObject);
begin
  FSitesOption := chosenSitesCBx.ItemIndex;
end;

procedure TWriteOutputDlg.FilenamePanelPaint(Sender: TObject);
begin
  if not Visible then
    Exit;
  DrawPanelBorder(FilenamePanel);
end;

procedure TWriteOutputDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  FullFileName: String;
begin
  Case ModalResult of
    mrOK:
    begin
      if FilenamePanel.Enabled then
      begin
        FullFileName := selDirectory.Text + selFileName.Text;
        if not DirectoryExists(ExtractFileDir(FullFileName)) then
        begin
          MessageDlg('The directory you have specified is invalid.'+ LineEnding +'Please check the spelling or specify another directory.', mtWarning, [mbOK], 0);
          CanClose := false;
        end
        else
        begin
          if ExtractFileName(FullFileName) <> '' then
          begin
            if FileExists(FullFileName) then
              if (MessageDlg('A file by this name already exists.'+ LineEnding +'Would you like to overwrite it?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
                CanClose := true // want to overwrite
              else
                CanClose := false; // do not want to overwrite
          end
          else
            CanClose := false;
        end;
      end;
    end;
    mrCancel, mrNone: CanClose := true;
  end;
  if CanClose then
    UpdateSaveOptions;
end;

function TWriteOutputDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  try
    Result := True;
    helpTopic := MapHelpContextToKeyword(HelpContext);
    CallHelp := False;
    ShowContextSensitiveHelp(helpTopic)
  except
    on E:Exception do
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

function TWriteOutputDlg.GetIsUseAllSites: Boolean;
begin
  Result := (FSitesOption = IS_USE_ALL_SITES);
end;

function TWriteOutputDlg.GetIsUseHighlightedSites: Boolean;
begin
  Result := (FSitesOption = IS_USE_HIGHLIGHTED_SITES);
end;

function TWriteOutputDlg.GetIsUseUnHighlightedSites: Boolean;
begin
  Result := (FSitesOption = IS_USE_UNHIGHLIGHTED_SITES);
end;

function TWriteOutputDlg.GetOutputFile: String;
begin
  Result := ExtractFilePath(selDirectory.Text) + selFileName.Text; // Have ExtractFilePath on the Directory edit because it will add a '/' if needed.
end;

procedure TWriteOutputDlg.FormShow(Sender: TObject);
begin
  SitesPanel.Visible := (not FHideSites);
  FHideSites := false;
  if not HasExcel then  //If the user does not have excel don't offer to display it for them
    Disallow(EXexcelDisp);
  if SelectOutputCBx.Items.Count < 5 then
    SelectOutputCBx.ItemIndex := 0;
  SelectOutputCBxChange(nil); // force the check to see if we should prompt for a filename (in case of Save, vs. Display).
  ClientHeight := Panel4.Height + FilenamePanel.Height + FormatAndFilePanel.Height;
  if SitesPanel.Visible then
    ClientHeight := ClientHeight + chosenSitesCBx.Height + ChooseSitesLabel.Height + 30
end;

procedure TWriteOutputDlg.HelpBtnClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext))
end;

procedure TWriteOutputDlg.SaveToDiskRadioBtnClick(Sender: TObject);
begin
  EnableDisableFilenameOptions;
end;

procedure TWriteOutputDlg.SelectOutputCBxChange(Sender: TObject);
begin
  EnableDisableFilenameOptions;
  if SelectOutputCBx.ItemIndex >= 0 then
    FSelectedFormat := SelectOutputCBx.Items[SelectOutputCBx.ItemIndex]
  else
    FSelectedFormat := EmptyStr;
  if selFileName.Text <> 'please provide a name' then
  begin
    if Trim(selFileName.Text) = EmptyStr then
      selFileName.Text := 'Result';
    if DisplayOnlyBtn.Checked then
      FileNamePanel.Color := Color
    else
    begin
      UpdateFileExtension;
      FilenamePanel.Color := Color;
      FilenamePanel.Invalidate;
    end;
    ShowErrorsInFileParameters;
  end;

end;

function TWriteOutputDlg.GetExportFormat: TExportType;
var
  aFormatStr: String;
begin
  aFormatStr := GetSelectedOutputFormat;
  if aFormatStr = XLSX_FORMAT_STR then
    Result := EXexcelXmlSave
  else if aFormatStr = XLS_FORMAT_STR then
    Result := EXexcelSave
  else if aFormatStr = ODS_FORMAT_STR then
    Result := EXodsSave
  else if aFormatStr = CSV_FORMAT_STR then
    Result := EXcsvSave
  else if aFormatStr = TEXT_FORMAT_STR then
    Result := EXtextSave
  else if aFormatStr = FASTA_FORMAT_STR then
    Result := EXfastaSave
  else
    Result := EXnone;
  if DisplayOnlyBtn.Checked then
    case Result of
      EXtextSave: Result := EXtext;
      EXcsvSave: Result := EXcsvDisp;
      EXexcelSave: Result := EXexcelDisp;
      EXexcelXmlSave: Result := EXexcelXmlDisp;
      EXodsSave: Result := EXodsDisp;
      EXfastaSave: Result := EXFasta;
    end;
end;

procedure TWriteOutputDlg.OKBtnClick(Sender: TObject);
var
  aType: TExportType;
begin
  if FileNamePanel.Enabled then
  begin
    If Trim(selFilename.Text) = EmptyStr then
    begin
      ShowMessage('Please provide a valid filename for writing the results');
      ModalResult := mrNone;
      Exit;
    end;
    If (Trim(selDirectory.Text) = EmptyStr) or (not DirectoryExists(Trim(selDirectory.Text))) then
    begin
      ShowMessage('Please provide a valid directory for writing the results');
      ModalResult := mrNone;
      Exit;
    end;
    ModalResult := mrOk;
  end
  else
    ModalResult := mrOk;
  {$IFDEF MSWINDOWS}
  aType := GetExportFormat;
  if ((aType = EXexcelXmlDisp) or (aType = EXexcelDisp)) and (not ExcelWrite.hasExcel) then
  begin
    ShowMessage('Microsoft Excel appears to be not installed on this machine.  Please select another option.');
    ModalResult := mrNone;
  end
  else
    ModalResult := mrOk;
  {$ENDIF}
  selFilename.Text := Trim(selFilename.Text);
  selDirectory.Text := Trim(selDirectory.Text);
end;

procedure TWriteOutputDlg.FormCreate(Sender: TObject);
begin
  {$IFDEF DARWIN}
  InitOutputRadioGroup;
  InitSitesRadioGroup;
  {$ELSE}
  InitOutputComboBox;
  InitSitesComboBox;
  {$ENDIF}
  FAllowedExportTypes := [EXtext, EXtextSave, EXcsvDisp, EXcsvSave, EXexcelDisp, EXexcelSave, EXexcelXmlDisp, EXexcelXmlSave, EXodsDisp, EXodsSave];
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Select Output Format';
  UpdateSaveOptions;
  SelectOutputCBx.ItemIndex := (SelectOutputCBx.Items.Count - 1); { text output by default}
  HelpKeyword := 'Display_Results_in_XL_CSV_Text.htm';
  HelpContext := HC_Display_Results_in_XL_CSV_Text;
  InitForm;
end;

procedure TWriteOutputDlg.selDirectoryChange(Sender: TObject);
begin
  ShowErrorsInFileParameters;
end;

procedure TWriteOutputDlg.selFilenameChange(Sender: TObject);
begin
  ShowErrorsInFileParameters;
end;

function TWriteOutputDlg.ValidFileName(FileName: String): Boolean;
var
  i: Integer;

const
  InvalidChars = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
begin
  result := false;
  for i:=1 to Length(FileName) do // check if there are invalid characters in the name.
    if FileName[i] in InvalidChars then
      exit;
  if CompareStr(ExtractFileName(FileName), ExtractFileExt(FileName)) = 0 then // make sure there is actually a filename and not just an extension.
    exit;
  if (Length(ExtractFileExt(FileName)) < 2) or (Length(ExtractFileName(FileName)) < 3) then // check that a file extension exists.
    exit;
  result := true;
end;

function TWriteOutputDlg.HighlightInvalidDirectory: Boolean;
begin
  if (Trim(selDirectory.Text) = EmptyStr) or DirectoryExists(Trim(selDirectory.Text)) then
  begin
    selDirectory.Font.Color := clBlack;
    Result := False;
  end
  else
  begin
    selDirectory.Font.Color := clRed;
    Result := true;
  end;
end;

function TWriteOutputDlg.HighlightInvalidFileName: Boolean;
begin
  if ValidFileName(Trim(selFileName.Text)) then
  begin
    selFileName.Font.Color := clBlack;
    result := false;
  end
  else
  begin
    selFileName.Font.Color := clRed;
    result := true;
  end;
end;

procedure TWriteOutputDlg.SetExportFormat(AValue: TExportType);
var
  index: Integer = -1;
begin
  index := IndexOfChoice(AValue);
  if index < 0 then
    Exit;
  UpdateSaveOptions;
  SelectOutputCBx.ItemIndex := index;
  case AValue of
    EXtextSave, EXcsvSave, EXexcelSave, EXexcelXmlSave, EXodsSave, EXFastaSave: SaveToDiskRadioBtn.Checked := True;
    EXcsvDisp, EXexcelDisp, EXexcelXmlDisp, EXodsDisp: DisplayOnlyBtn.Checked := True;
  end;
end;

procedure TWriteOutputDlg.SetFileName(Filename: String);
begin
  selFilename.Text := Filename;
end;

procedure TWriteOutputDlg.ShowErrorsInFileParameters;
begin
  if not FilenamePanel.Enabled then
    Exit;
  if HighlightInvalidDirectory or HighlightInvalidFileName then
    FileNamePanel.Color := AttentionNeededColor;
end;

procedure TWriteOutputDlg.Disallow(ToDisable : TExportType);
begin
  case ToDisable of
    EXtext, EXtextSave:
      begin
        Exclude(FAllowedExportTypes, EXtext);
        Exclude(FAllowedExportTypes, EXtextSave);
      end;
    EXcsvDisp, EXcsvSave:
      begin
        Exclude(FAllowedExportTypes, EXcsvDisp);
        Exclude(FAllowedExportTypes, EXcsvSave);
      end;
    EXexcelDisp, EXexcelSave:
      begin
        Exclude(FAllowedExportTypes, EXexcelDisp);
        Exclude(FAllowedExportTypes, EXexcelSave);
      end;
    EXexcelXmlDisp, EXexcelXmlSave:
      begin
        Exclude(FAllowedExportTypes, EXexcelXmlDisp);
        Exclude(FAllowedExportTypes, EXexcelXmlSave);
      end;
    EXodsDisp, EXodsSave:
      begin
        Exclude(FAllowedExportTypes, EXodsDisp);
        Exclude(FAllowedExportTypes, EXodsSave);
      end;
    EXfasta, EXfastaSave:
      begin
        Exclude(FAllowedExportTypes, EXfasta);
        Exclude(FAllowedExportTypes, EXfastaSave);
      end;
  end;
  if (IndexOfChoice(ToDisable) >= 0) and (IndexOfChoice(ToDisable) < SelectOutputCBx.Items.Count) then
    SelectOutputCBx.Items.Delete(IndexOfChoice(ToDisable));
  while SelectOutputCBx.ItemIndex >= SelectOutputCBx.Items.Count do
    SelectOutputCBx.ItemIndex := SelectOutputCBx.ItemIndex - 1;
end;

procedure TWriteOutputDlg.Allow(toAllow: TExportType);
begin
  case toAllow of
    EXtext, EXtextSave:
      begin
        Include(FAllowedExportTypes, EXtext);
        Include(FAllowedExportTypes, EXtextSave);
      end;
    EXcsvDisp, EXcsvSave:
      begin
        Include(FAllowedExportTypes, EXcsvDisp);
        Include(FAllowedExportTypes, EXcsvSave);
      end;
    EXexcelDisp, EXexcelSave:
      begin
        Include(FAllowedExportTypes, EXexcelDisp);
        Include(FAllowedExportTypes, EXexcelSave);
      end;
    EXexcelXmlDisp, EXexcelXmlSave:
      begin
        Include(FAllowedExportTypes, EXexcelXmlDisp);
        Include(FAllowedExportTypes, EXexcelXmlSave);
      end;
    EXfasta, EXFastaSave:
      begin
        Include(FAllowedExportTypes, EXfasta);
        Include(FAllowedExportTypes, EXFastaSave);
      end;
    EXodsDisp, EXodsSave:
      begin
        Include(FAllowedExportTypes, EXodsDisp);
        Include(FAllowedExportTypes, EXodsSave);
      end;
  end;

  UpdateSaveOptions;
end;

function TWriteOutputDlg.IndexOfChoice(Choice: TExportType): integer;
begin
  Result := -1;

  case Choice of
    EXInvalid, EXnone: raise Exception.Create('invalid export type');
    EXtext, EXtextSave: Result := SelectOutputCBx.Items.IndexOf(TEXT_FORMAT_STR);
    EXcsvSave, EXcsvDisp: Result := SelectOutputCBx.Items.IndexOf(CSV_FORMAT_STR);
    EXexcelSave, EXexcelDisp: Result := SelectOutputCBx.Items.IndexOf(XLS_FORMAT_STR);
    EXexcelXmlSave, EXexcelXmlDisp: Result := SelectOutputCBx.Items.IndexOf(XLSX_FORMAT_STR);
    EXodsSave, EXodsDisp: Result := SelectOutputCBx.Items.IndexOf(ODS_FORMAT_STR);
    EXfasta, EXFastaSave: Result := SelectOutputCBx.Items.IndexOf(FASTA_FORMAT_STR);
  end;
end;

procedure TWriteOutputDlg.ExportExcelDisabledClick(Sender: TObject);
begin
  ShowMessage(
    'Excel Export Disabled:' + #10#13 +
      'Excel Export is disabled because you either have a sequence with more than 32,760 sites or more than 250 nodes.' + #10#13 +
      'Versions of Microsoft Excel before Excel 2007 have a limit of 256 columns and at most 32,727 characters per cell.' + #10#13 +
      'MEGA exports excel files in the Excel 97-2003 .xls format which is compatable with all versions of excel after Excel 97.'
    );
end;

procedure TWriteOutputDlg.EnableDisableFilenameOptions;
begin
  selFileName.Enabled    := SaveToDiskRadioBtn.Checked;
  selDirectory.Enabled   := selFileName.Enabled;
  outputFileNameLbl.Enabled := selFileName.Enabled;
  directoryLbl.Enabled := selFileName.Enabled;
  FilePickBtn.Enabled := selFileName.Enabled;
  FilenamePanel.Enabled  := selFileName.Enabled;
  if selFilename.Enabled then
    UpdateFileExtension;
end;

procedure TWriteOutputDlg.SitesPanelPaint(Sender: TObject);
begin
  if not Visible then
    Exit;
  DrawPanelBorder(SitesPanel);
end;

procedure TWriteOutputDlg.UpdateFileExtension;
begin
  case GetExportFormat of
    EXtextSave: selFileName.Text := ChangeFileExt(selFileName.Text, '.txt');
    EXcsvSave: selFileName.Text := ChangeFileExt(selFileName.Text, '.csv');
    EXexcelSave: selFileName.Text := ChangeFileExt(selFileName.Text, '.xls');
    EXexcelXmlSave: selFileName.Text := ChangeFileExt(selFileName.Text, '.xlsx');
    EXodsSave: selFileName.Text := ChangeFileExt(selFileName.Text, '.ods');
    EXfastaSave: selFileName.Text := ChangeFileExt(selFileName.Text, '.fas');
  end;
end;

procedure TWriteOutputDlg.DrawPanelBorder(aPanel: TPanel);
var
  aMargin: Integer = 4;
begin
  if not Visible then
    Exit;
  with aPanel.Canvas do
  begin
    pen.Color := RGB($8b, $aa, $c6);
    Line(aMargin, aMargin, aPanel.Width - aMargin, aMargin);
    Line(aMargin, aMargin, aMargin, aPanel.Height - aMargin);
    Line(aPanel.Width - aMargin, aMargin, aPanel.Width - aMargin, aPanel.Height - aMargin);
    Line(aMargin, aPanel.Height - aMargin, aPanel.Width - aMargin, aPanel.Height - aMargin);
  end;
end;

procedure TWriteOutputDlg.InitForm;
var
  aFontName: String = 'default';
begin
  {$IFDEF MSWINDOWS}
  if Screen.Fonts.IndexOf('Open Sans') >= 0 then
    aFontName := 'Open Sans';
  {$ELSE}
  if Screen.Fonts.IndexOf('OpenSymbol') >= 0 then
    aFontName := 'OpenSymbol';
  {$ENDIF}
  Font.Name := aFontName;
  Color := $00f7f8f8;
  FilenamePanel.Color := Color;
  ChooseSitesLabel.Font.Style := [fsBold];
  ChooseSitesLabel.Font.Color := clGrayText;
  ChooseSitesLabel.Font.Size := 8;
  ChooseSitesLabel.Font.Name := aFontName;
  outputFilenameLbl.Font.Style := [fsBold];
  outputFilenameLbl.Font.Color := clGrayText;
  outputFilenameLbl.Font.Size := 8;
  outputFilenameLbl.Font.Name := aFontName;
  directoryLbl.Font.Style := [fsBold];
  directoryLbl.Font.Color := clGrayText;
  directoryLbl.Font.Size := 8;
  directoryLbl.Font.Name := aFontName;
  OutputFormatLbl.Font.Style := [fsBold];
  OutputFormatLbl.Font.Color := clGrayText;
  OutputFormatLbl.Font.Size := 8;
  OutputFormatLbl.Font.Name := aFontName;
  SelectOutputCBx.Font.Color := clBlack;
  FSitesOption := chosenSitesCBx.ItemIndex;
end;

procedure TWriteOutputDlg.UpdateSaveOptions;
var
  curOption: String;
  {$IFDEF DARWIN}
   delta: Integer;
  {$ENDIF}
begin
  curOption := SelectedOutputFormat;
  SelectOutputCBx.Items.Clear;
  if EXexcelXmlDisp in FAllowedExportTypes then
    SelectOutputCBx.Items.Add(XLSX_FORMAT_STR);
  if EXexcelDisp in FAllowedExportTypes then
    SelectOutputCBx.Items.Add(XLS_FORMAT_STR);
  if EXodsDisp in FAllowedExportTypes then
    SelectOutputCBx.Items.Add(ODS_FORMAT_STR);
  if EXcsvDisp in FAllowedExportTypes then
    SelectOutputCBx.Items.Add(CSV_FORMAT_STR);
  if EXtext in FAllowedExportTypes then
    SelectOutputCBx.Items.Add(TEXT_FORMAT_STR);
  if EXfasta in FAllowedExportTypes then
    SelectOutputCBx.Items.Add(FASTA_FORMAT_STR);
  SelectOutputCBx.ItemIndex := SelectOutputCBx.Items.IndexOf(curOption);
  if SelectOutputCBx.ItemIndex = -1 then
    SelectOutputCBx.ItemIndex := 0;

  FSelectedFormat := SelectOutputCBx.Items[SelectOutputCBx.ItemIndex];
  {$IFDEF DARWIN}
  SelectOutputCBx.Height := SelectOutputCBx.Rows*(FormatAndFilePanel.Canvas.TextHeight('STUFF') + 5) + 20;
  delta := FormatAndFilePanel.Height;
  FormatAndFilePanel.Height := SelectOutputCBx.Height + OutputFormatLbl.Height + SaveToDiskRadioBtn.Height + 30;
  delta := FormatAndFilePanel.Height - delta;
  if delta > 0 then
    ClientHeight := ClientHeight + delta;
  {$ENDIF}
end;

function TWriteOutputDlg.GetSelectedOutputFormat: String;
begin
  Result := FSelectedFormat;
end;

end.

