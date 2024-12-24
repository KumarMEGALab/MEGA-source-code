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

unit MutationDetailView;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ActnList, ComCtrls, ExtCtrls, Menus, Grids, IniPropStorage, EvoD,
  MMultiStageProgress, MegaUtils, mgridcolumnresizer, mimageform;

const


  NMID_COL_INDEX = 2; // this is the column index in the mutation explorer which displays NM IDs
  FOC_SITE_COL_INDEX = 24; // column index in mutation explorer for the AA position
  PEPTIDE_COL_INDEX = 1;  // column index in mutation explorer for the peptide id


type

  { TMutationDetailViewForm }

  TMutationDetailViewForm = class(TForm)
    DetailsDrawGrid: TDrawGrid;
    GetAlignmentAction: TAction;
    AncParsimonyAction: TAction;
    AncMLAction: TAction;
    ActionList1: TActionList;
    IniPropStorage1: TIniPropStorage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    AlignButton: TToolButton;
    AncestorsButton: TToolButton;
    ToolButton3: TToolButton;
    procedure AncMLActionExecute(Sender: TObject);
    procedure AncParsimonyActionExecute(Sender: TObject);
    procedure DetailsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DetailsDrawGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetAlignmentActionExecute(Sender: TObject);
  private
    FGridColumnSizer: TGridColumnResizer;
    FGridColor1: TColor;
    FGridColor2: TColor;
    FGridSelectedColor: TColor;
    FIsInitialized: Boolean;
    FActiveNmID: String;
    FPeptideId: String;
    FSelectionHasChanged: Boolean; // so we don't keep querying the server for the same alignment if the user wants to run multiple analyses
    FMSProgress: TMultiStageProgressForm;
    FColumnsList: TStringList;
    function GetDrawGridString(FieldIndex: Integer): String;
    function GetDrawGridFloatString(aValue: Extended): String;
    function GetDrawGridIntString(aValue: Integer): String;
    procedure LoadColumnsList;
    function DownloadAlignmentFile(Url: String; TargetFile: String): Boolean;
    function LaunchSearchQuery(Query: String): TEvodSearchResult;
    procedure UpdateColumnSizes;
    procedure UpdateClientHeight;
    procedure DrawRectEdges(aRect: TRect; aColor: TColor; doLeft, doTop, doRight, doBottom: Boolean);
    function IsHeaderRow(aRow: Integer): Boolean;
    { private declarations }
  public
    { Public declarations }
    FocusedAASite: Integer;
    Precision: Integer;
    DidActivateData: Boolean;
    MutationDiagnosis: TMutationDiagnosis;
    procedure CreateDetailView;
    procedure UpdateDetailView(aDiagnosis: TMutationDiagnosis);
    procedure RefreshView;
    procedure SetActionsEnabled(AEnabled: Boolean);
    function ActivateAlignment(ShowSDE: Boolean = True): Boolean; overload;// downloads a 46 species alignment from the EvoD server and opens the alignment in the Sequence Data Explorer
    function ActivateAlignment(MRnaID: String; PeptideId: String): Boolean; overload;
  end;

var
  MutationDetailViewForm: TMutationDetailViewForm;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MV_SeqDataExplorer, mega_main,
  {$ENDIF}
  ProcessInputData, Math, MegaPrivateFiles;

{$R *.lfm}

{ TMutationDetailViewForm }

procedure TMutationDetailViewForm.AncMLActionExecute(Sender: TObject);
begin
  try
    ActivateAlignment(False);
    AncMLAction.Enabled := False; // simplifies our life if the user can only do one at a time
    AncParsimonyAction.Enabled := False;
    MegaForm.AncestralSeqsMLActionExecute(AncMLAction);
  except
    on E: Exception do
    begin
      ShowMessage('MEGA error occurred when inferring ancestral sequences: ' + E.Message);
      AncMLAction.Enabled := True;
      AncParsimonyAction.Enabled := True;
    end;
  end;
end;

procedure TMutationDetailViewForm.AncParsimonyActionExecute(Sender: TObject);
begin
  try
    ActivateAlignment(False);
    AncParsimonyAction.Enabled := False;
    AncMLAction.Enabled := False;
    MegaForm.AncestralSeqsMPActionExecute(AncParsimonyAction);
  except
    on E: Exception do
    begin
      ShowMessage('Oh no! An error occurred when inferring ancestral sequences: ' + E.Message);
      AncMLAction.Enabled := True;
      AncParsimonyAction.Enabled := True;
    end;
  end;
end;

procedure TMutationDetailViewForm.DetailsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  x, y: Integer;
  aText: String;
  aTextStyle: TTextStyle;
  padding: String;
begin
  if not Visible then Exit;
  padding := '  ';
  try
    BeginFormUpdate;
    aTextStyle := DetailsDrawGrid.Canvas.TextStyle;
    aTextStyle.SystemFont := False;
    aTextStyle.Layout := tlCenter;
    with DetailsDrawGrid.Canvas do
    begin
      x := aRect.Left + 2;
      y := aRect.Top + 2;
      Font.Style := [];
      Font.Color := clBlack;
      if (DetailsDrawGrid.Selection.Top = aRow) and (DetailsDrawGrid.Selection.Left <> 0) then
      begin
        Brush.Color := FGridSelectedColor;
        Font.Color := clWhite;
      end
      else if IsHeaderRow(aRow) then
        Brush.Color := FGridColor2
      else
        Brush.Color := FGridColor1;
      if aRow = 0 then
      begin
        Brush.Color := clBtnFace;
        FillRect(aRect);
        if aCol = 1 then
          aText := 'Property'
        else if aCol = 2 then
          aText := 'Value'
        else
          aText := EmptyStr;
        aTextStyle.Alignment := taCenter;
        TextRect(aRect, x, y, aText, aTextStyle);
        DrawRectEdges(aRect, clBlack, (aCol <> (DetailsDrawGrid.ColCount - 1)) and (aCol <> 1), True, (aCol = (DetailsDrawGrid.ColCount - 1)) or (aCol = 0), True);
      end
      else if aCol = 0 then
      begin
        Font.Color := clMaroon;
        aTextStyle.Alignment := taCenter;
        Brush.Color := clBtnFace;
        Brush.Style := bsSolid;
        FillRect(aRect);
        if (DetailsDrawGrid.Selection.Top = aRow) and (aRow > 0) then
        begin
          Font.Style := [fsBold];
          aText := '>';
          TextRect(aRect, x, y, aText, aTextStyle);
        end;
        DrawRectEdges(aRect, clBlack, True, aRow <> 1, True, aRow = (DetailsDrawGrid.RowCount - 1));
      end
      else if aCol = 1 then
      begin
        FillRect(aRect);
        aText := FColumnsList[aRow - 1];
        aTextStyle.Alignment := taLeftJustify;
        TextRect(aRect, x, y, aText, aTextStyle);
        DrawRectEdges(aRect, clSilver, False, aRow <> 1, False, aRow = (DetailsDrawGrid.RowCount - 1));
      end
      else if aCol = 2 then
      begin
        FillRect(aRect);
        if Assigned(MutationDiagnosis) then
        begin
          aTextStyle.Alignment := taRightJustify;
          aText := GetDrawGridString(aRow) + padding;
          TextRect(aRect, x, y, aText, aTextStyle);
        end;
        DrawRectEdges(aRect, clSilver, True, aRow <> 1, False, aRow = (DetailsDrawGrid.RowCount - 1));
      end
      else if aCol = 3 then
      begin
        FillRect(aRect);
        DrawRectEdges(aRect, clSilver, False, aRow <> 1, True, aRow = (DetailsDrawGrid.RowCount - 1));
      end;
    end;
  finally
    EndFormUpdate;
  end;
end;

procedure TMutationDetailViewForm.DetailsDrawGridSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin
  DetailsDrawGrid.Invalidate;
end;

procedure TMutationDetailViewForm.FormCreate(Sender: TObject);
{$IFDEF MYPEG_ONLY}
var
  IconFile: String;
{$ENDIF}
begin
  {$IFDEF MYPEG_ONLY}
  IconFile := GetPrivateFile(mfMyPegIconFile);
  if FileExists(IconFile) then
    try
      Self.Icon.LoadFromFile(IconFile);
    except
     // don't sweat the small stuff
     {$IFDEF DEBUG}
      on E: Exception do
        ShowMessage('Error in MutationDetailView.FormCreate: ' + E.Message);
     {$ENDIF}
    end;
  {$ENDIF}
  Constraints.MinWidth := AlignButton.Width + ToolButton3.Width + AncestorsButton.Width + 20;
  FGridColumnSizer := TGridColumnResizer.Create;
  Self.Caption := 'Detail View';
  FGridColor1 := RGB($e9, $e9, $e9);
  FGridColor2 := RGB($a0, $c7, $d2);
  FGridSelectedColor := RGB($17, $85, $a6);

  DidActivateData := False;
  FocusedAASite := -1;
  FColumnsList := TStringList.Create;
  FSelectionHasChanged := True;
  FActiveNMiD := EmptyStr;
  LoadColumnsList;
  FIsInitialized := False;
  DetailsDrawGrid.Color := FGridColor1;
  CreateDetailView;
  FMSProgress := TMultiStageProgressForm.Create(Self);
  PopupMenu1.OwnerDraw := True;
  PopupMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  PopupMenu1.OnDrawItem := MegaForm.DrawMenuItem;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TMutationDetailViewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FColumnsList) then
    FColumnsList.Free;
  if Assigned(FGridColumnSizer) then
    FGridColumnSizer.Free;
end;

procedure TMutationDetailViewForm.FormResize(Sender: TObject);
var
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    aList.Add('>');
    aList.Add('  Polyphen-2 Balanced ');
    aList.Add('Likely Deleterious');
    aList.Add(' ');
    FGridColumnSizer.ResizeGridColumns(DetailsDrawGrid, aList);
    DetailsDrawGrid.Invalidate;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TMutationDetailViewForm.FormShow(Sender: TObject);
begin
  if not FIsInitialized then
    CreateDetailView;
end;

procedure TMutationDetailViewForm.GetAlignmentActionExecute(Sender: TObject);
begin
  try
    if Assigned(V_SeqDataExplorer) and (not FSelectionHasChanged) then
      V_SeqDataExplorer.Show
    else
      ActivateAlignment(True);
  except
    on E: Exception do
      ShowMessage('MEGA error when retrieving the species alignment: ' + E.Message);
  end;
end;

function TMutationDetailViewForm.GetDrawGridString(FieldIndex: Integer): String;
begin
  try
    case FieldIndex of
      1: Result := EmptyStr; { Mutations}
      2: Result := Format('%s', [MutationDiagnosis.Rsid]);
      3: Result := Format('%s', [MutationDiagnosis.PeptideId]);
      4: Result := Format('%s', [MutationDiagnosis.Mrnaid]);
      5: Result := Format('%s', [MutationDiagnosis.Wildallele]);
      6: Result := Format('%s', [MutationDiagnosis.Mutantallele]);
      7: Result := EmptyStr; { Predictions}
      8: Result := Format('%s', [MutationDiagnosis.Conspred]);
      9: Result := Format('%s', [MutationDiagnosis.Evodpred]);
      10: Result := Format('%.3e', [MutationDiagnosis.Evodpval]);
      11: Result := Format('%s', [MutationDiagnosis.Polyphenpred]);
      12: Result := Format('%s', [MutationDiagnosis.Polyphenpred_Balanced]);
      13: Result := Format('%s', [MutationDiagnosis.Siftpred]);
      14: Result := Format('%s', [MutationDiagnosis.Siftpred_Balanced]);
      15: Result := EmptyStr; { Impact}
      16: Result := GetDrawGridFloatString(MutationDiagnosis.Evodscore);
      17: Result := GetDrawGridFloatString(MutationDiagnosis.Polyphenscore);
      18: Result := GetDrawGridFloatString(MutationDiagnosis.Siftscore);
      19: Result := GetDrawGridFloatString(MutationDiagnosis.Granthamdist);
      20: Result := GetDrawGridFloatString(MutationDiagnosis.Blosum62);
      21: Result := EmptyStr; { Evolutionary Features}
      22: Result := GetDrawGridFloatString(MutationDiagnosis.Ratevertebrate);
      23: Result := GetDrawGridFloatString(MutationDiagnosis.Timefitchvertebrate);
      24: Result := GetDrawGridFloatString(MutationDiagnosis.Timemutvertebrate);
      25: Result := EmptyStr; { Coordinate Info}
      26: Result := Format('%s', [MutationDiagnosis.Chromosome]);
      27: Result := GetDrawGridIntString(MutationDiagnosis.Chromosomepos);
      28: Result := Format('%s', [MutationDiagnosis.Strand]);
      29: Result := GetDrawGridIntString(MutationDiagnosis.Nucpos);
      30: Result := GetDrawGridIntString(MutationDiagnosis.Aapos);
      31: Result := Format('%s', [MutationDiagnosis.Wildnuc]);
      32: Result := Format('%s', [MutationDiagnosis.Mutantnuc]);
      else
        Result := EmptyStr;
    end;
  except
    Result := 'NA';
  end;
end;

function TMutationDetailViewForm.GetDrawGridFloatString(aValue: Extended): String;
var
  formatStr: String;
begin
  formatStr := '%.' + IntToStr(Precision) + 'f';
  if CompareValue(aValue, BAD_NUMBER, 0.000001) = 0 then
    Result := 'NA'
  else
    Result := Format(formatStr, [aValue]);
end;

function TMutationDetailViewForm.GetDrawGridIntString(aValue: Integer): String;
begin
  if aValue = BAD_NUMBER then
    Result := 'NA'
  else
    Result := IntToStr(aValue);
end;

procedure TMutationDetailViewForm.LoadColumnsList;
begin
  FColumnsList.Add('Mutations');
  FColumnsList.Add('  rsID');
  FColumnsList.Add('  Peptide ID');
  FColumnsList.Add('  mRNA Accession');
  FColumnsList.Add('  Reference (AA)');
  FColumnsList.Add('  Mutant (AA)');

  FColumnsList.Add('Predictions');
  FColumnsList.Add(' Consensus');
  FColumnsList.Add(' EvoD');
  FColumnsList.Add(' EvoD P-value');
  FColumnsList.Add(' PolyPhen-2');
  FColumnsList.Add(' PolyPhen-2 Balanced');
  FColumnsList.Add(' SIFT');
  FColumnsList.Add(' SIFT Balanced');

  FColumnsList.Add('Impact');
  FColumnsList.Add('  EvoD');
  FColumnsList.Add('  PolyPhen-2');
  FColumnsList.Add('  SIFT');
  FColumnsList.Add('  Grantham Distance');
  FColumnsList.Add('  Blosum 62');

  FColumnsList.Add('Evolutionary Features');
  FColumnsList.Add('  Substitution Rate');
  FColumnsList.Add('  Position Time Span');
  FColumnsList.Add('  Mutation Time Span');

  FColumnsList.Add('Coordinate Info');
  FColumnsList.Add('  Chromosome');
  FColumnsList.Add('  Chromosome Position');
  FColumnsList.Add('  Strand');
  FColumnsList.Add('  Nucleotide Position');
  FColumnsList.Add('  AA Position');
  FColumnsList.Add('  Wild Nucleotide');
  FColumnsList.Add('  Mutant Nucleotide');
end;

function TMutationDetailViewForm.DownloadAlignmentFile(Url: String; TargetFile: String): Boolean;
var
  filename: String;
begin
  Result := False;

  try
    filename := downloadURL(Url, TargetFile);
    if FileExists(filename) then
      Result := True;
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when downloading the alignment file: ' + E.Message);
  end;
end;

function TMutationDetailViewForm.LaunchSearchQuery(Query: String): TEvodSearchResult;
var
  Url: String;
  TempFile: String;
begin
  Result := nil;
  try
    try
      //FMSProgress.SetToProgressMode;
      //FMSProgress.HideCurrentOpProgress;
      //FMSProgress.StartProgress('Requesting sequence alignment...', EmptyStr);
      //FMSProgress.Show;
      Url := 'http://' + MYPEG_HOST + MYPEG_PATH + 'searchForSeq.php?nmid=' + Query;
      TempFile := GetTEMP + '\' + 'query.xml';
      //Downloader.OnDownloadProgress := FMSProgress.DownloadUrlProgressNotify;
      downloadURL(Url, TempFile);
      if FileExists(TempFile) then
      begin
        Result := TEvodSearchResult.Create;
        Result.parseXML(TempFile);
      end
      else
        Result := nil;
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
         ShowMessage('Error in MutationDetailView.LaunchSearchQuery: ' + E.Message);
        {$ENDIF}
        Result := nil;
      end;
    end;
  finally
    try
      if FileExists(TempFile) then
        DeleteFile(TempFile);
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
         ShowMessage('Error in MutationDetailView.LaunchSearchQuery: ' + E.Message);
        {$ENDIF}
      end;
      // don't sweat it
    end;
  end;
end;

procedure TMutationDetailViewForm.UpdateColumnSizes;
var
  aList: TStringList = nil;
  i, aWidth: Integer;
begin
  try
    BeginFormUpdate;
    aList := TStringList.Create;
    aList.Add('>');
    aList.Add('  Polyphen-2 Balanced ');
    aList.Add('Likely Deleterious');
    aList.Add(' ');
    FGridColumnSizer.ResizeGridColumns(DetailsDrawGrid, aList);
    DetailsDrawGrid.ColWidths[DetailsDrawGrid.ColCount - 1] := 4;
    aWidth := 0;
    for i := 0 to DetailsDrawGrid.ColCount - 1 do
      aWidth := aWidth + DetailsDrawGrid.ColWidths[i];
    ClientWidth := aWidth + 4;
    DetailsDrawGrid.Invalidate;
  finally
    EndFormUpdate;
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TMutationDetailViewForm.UpdateClientHeight;
begin
  ClientHeight := ToolBar1.Height + DetailsDrawGrid.DefaultRowHeight*DetailsDrawGrid.RowCount + 4;
end;

procedure TMutationDetailViewForm.DrawRectEdges(aRect: TRect; aColor: TColor; doLeft, doTop, doRight, doBottom: Boolean);
begin
  with DetailsDrawGrid.Canvas do
  begin
    Pen.Color := aColor;
    Pen.Style := psSolid;
    Pen.Width := 1;
    if DoLeft then
      Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
    if DoTop then
      Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
    if DoRight then
      Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
    if DoBottom then
      Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  end;
end;

function TMutationDetailViewForm.IsHeaderRow(aRow: Integer): Boolean;
begin
  case aRow of
    1, 7, 15, 21, 25: Result := True;
    else
      Result := False;
  end;
end;

procedure TMutationDetailViewForm.CreateDetailView;
begin
  try
    BeginFormUpdate;
    DetailsDrawGrid.Align := alClient;
    DetailsDrawGrid.RowCount := FColumnsList.Count + 1; { +1 for the header}
    DetailsDrawGrid.Invalidate;
    FIsInitialized := True;
    UpdateColumnSizes;
    UpdateClientHeight;
  finally
    EndFormUpdate;
  end;
end;

procedure TMutationDetailViewForm.UpdateDetailView(aDiagnosis: TMutationDiagnosis);
begin
  try
    BeginFormUpdate;
    FSelectionHasChanged := True;
    MutationDiagnosis := aDiagnosis;
    if Assigned(MutationDiagnosis) then
    begin
      FActiveNmID := MutationDiagnosis.Mrnaid;
      FocusedAASite := MutationDiagnosis.Aapos;
      FPeptideId := MutationDiagnosis.Peptideid;
    end;
    //UpdateColumnSizes;
    DetailsDrawGrid.Invalidate;
  finally
    EndFormUpdate;
  end;
end;

procedure TMutationDetailViewForm.RefreshView;
begin
  try
    BeginFormUpdate;
    DetailsDrawGrid.Invalidate;
  finally
    EndFormUpdate;
  end;
end;

procedure TMutationDetailViewForm.SetActionsEnabled(AEnabled: Boolean);
begin
  AlignButton.Enabled := Enabled;
  AncestorsButton.Enabled := Enabled;
  AncMLAction.Enabled := Enabled;
end;

function TMutationDetailViewForm.ActivateAlignment(ShowSDE: Boolean): Boolean;
var
  Url: String = '';
  TempFile: String = '';
  Nmid: String = '';
  Query: TEvodSearchResult = nil;
begin
  Result := False;
  if (not FSelectionHasChanged) and (Assigned(V_SeqDataExplorer) and (V_SeqDataExplorer.EvoDPeptideId = FPeptideId)) then // no need to retrieve the same alignment
  begin
    V_SeqDataExplorer.Show;
    Exit;
  end;

  try
    {$IFDEF VISUAL_BUILD}
    MegaForm.PromptCloseCurrentlyActive;
    if not MegaForm.HasActiveData then
    begin
      Query := LaunchSearchQuery(FActiveNmID);
      if Assigned(Query) then
      begin
        Nmid := Query.GetResult(0).MRnaID; // just to be sure it is the one that the server prefers
        Url := 'http://' + MYPEG_HOST + SPECIES_ALIGNMENTS_PATH + Nmid + '.meg';
        TempFile := getTemp + '\' + Nmid + '.meg';
        try
          if not DownloadAlignmentFile(Url, TempFile) then
            raise Exception.Create('Alignment download failed. nmid=' + Nmid) // gets handled upstream
          else
          begin
            FMSProgress.StopProgress;
            FMSProgress.Hide;
          end;
        except
           on E:Exception do
             raise Exception.Create(E.Message); { in case an exception was raised inside of DownloadAlignmentFile}
        end;

        DoOpenDataFile(TempFile);
        V_SeqDataExplorer.evodNmid := Nmid;
        V_SeqDataExplorer.Caption := 'Sequence Data Explorer: ' + FPeptideId + '/' + Nmid;
        if ShowSDE then
          V_SeqDataExplorer.Show
        else
          V_SeqDataExplorer.Hide; // hide the sde when doing phylogeny construction or ancestral sequence inference
        V_SeqDataExplorer.SetDiagnoseSite(FocusedAASite);
        V_SeqDataExplorer.EvoDPeptideId := FPeptideId;
        DidActivateData := True;
      end
      else
      begin
        ShowMessage('Oh no! Failed to download the alignment file. Are you connected to the internet?');
      end;
    end;
    {$ENDIF}
    FSelectionHasChanged := False;
    Result := True;
  finally
    if Assigned(Query) then
      Query.Free;
    try
      if FileExists(TempFile) then
        DeleteFile(TempFile);
    except
      // don't sweat the small stuff
      {$IFDEF DEBUG}
      on E: Exception do
        ShowMessage('Failed to cleanup temp file: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TMutationDetailViewForm.ActivateAlignment(MRnaID: String; PeptideId: String): Boolean;
var
  Url: String;
  TempFile: String;
begin
  Result := False;
  try
    Url := 'http://' + MYPEG_HOST + SPECIES_ALIGNMENTS_PATH + MRnaId + '.meg';
    TempFile := getTemp + '\' + MRnaID + '.meg';

    if not DownloadAlignmentFile(Url, TempFile) then
      raise Exception.Create('Alignment download failed. nmid=' + MRnaId) // gets handled upstream
    else
    begin
      FMSProgress.StopProgress;
      FMSProgress.Hide;
    end;
    {$IFDEF VISUAL_BUILD}
    MegaForm.PromptCloseCurrentlyActive;
    if not MegaForm.HasActiveData then // They might have clicked NO, in which case we just don't want to procede.
    begin
      MegaForm.OpenFile(TempFile, 'meg', OActivate);
      V_SeqDataExplorer.evodNmid := MRnaID;
      V_SeqDataExplorer.EvoDPeptideId := PeptideId;
      V_SeqDataExplorer.Caption := 'Sequence Data Explorer: ' + PeptideId + '/' + MRnaID;
      V_SeqDataExplorer.Show;
      V_SeqDataExplorer.SetDiagnoseSite(1);
      DidActivateData := True;
    end;
    {$ENDIF}
    FSelectionHasChanged := False;
    Result := True;
  finally
    try
      if FileExists(TempFile) then
        DeleteFile(TempFile);
    except
      // don't sweat the small stuff
      {$IFDEF DEBUG}
      on E: Exception do
        ShowMessage('Failed to cleanup temp file: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

end.

