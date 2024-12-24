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

unit MSelectGeneticCodeDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, CheckLst, Spin, ActnList, ComCtrls, MegaConsts, ExcelWrite,
  MD_InputSeqData, MEditorForm;

type

  { TSelectGeneticCodeDlg }

  TSelectGeneticCodeDlg = class(TForm)
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    NamesLBx: TCheckListBox;
    CodesLBx: TListBox;
    AddSBtn: TSpeedButton;
    DeleteSBtn: TSpeedButton;
    EditSBtn: TSpeedButton;
    ScrollBox1: TScrollBox;
    CodeSelSEdit: TSpinEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ViewSBtn: TSpeedButton;
    ComputeSBtn: TSpeedButton;
    TopPanel: TPanel;
    BottomPanel: TPanel;
    procedure AddSBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ComputeSBtnClick(Sender: TObject);
    procedure DeleteSBtnClick(Sender: TObject);
    procedure EditSBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NamesLBxClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ViewSBtnClick(Sender: TObject);
  private
    FCurrent: Integer;
    BaseName: array[0..3] of Char;  // names of bases
    procedure SetCheckedBox(index: Integer);
    function GetCodeTable: String;
    function GetCodeTableName: String;
    procedure SetCodeTableName(AValue: String);
    procedure ViewSelectedCode(ExportType: TExportType; SaveLoc: AnsiString);
    procedure StatsSelectedCode(ExportType: TExportType; SaveLoc: AnsiString);
  public
    function GetGeneticCodeNames: TStringList;
    procedure ExportGeneticCodeStatistics(ExportTypeStr: String; SaveLocation: String);
    procedure ExportGeneticCodeTable(ExportTypeStr: String; SaveLocation: String);
    property CodeTableName: String read GetCodeTableName write SetCodeTableName;
    property CodeTable:     String read GetCodeTable;
  end;

var
  SelectGeneticCodeDlg: TSelectGeneticCodeDlg;

implementation

uses
  MegaUtils, MCodons, MegaVerConsts, ContextHelp_HC, mhelpfiles, mhelpkeywords,
  mdefine_genetic_code_dlg, MWriteOutputDlg, mimageform;

{$R *.lfm}

{ TSelectGeneticCodeDlg }

procedure TSelectGeneticCodeDlg.FormCreate(Sender: TObject);
var
  i: Integer;
  textHeight: Integer;
begin
  inherited;

  BaseName[0] := 'U';
  BaseName[1] := 'C';
  BaseName[2] := 'A';
  BaseName[3] := 'G';
  NamesLBx.Items.Clear;
  if NamesLBx.Items.Count = 0 then
  begin
    for i:=0 to GetNoOfDefaultCodeTables-1 do
    begin
      NamesLBx.Items.Add(GetDefaultCodeTableName(i));
      CodesLBx.Items.Add(GetDefaultCodeTable(i));
    end;
  end;
  HelpContext := HC_Select_Genetic_Code_Table_Dialog;
  FCurrent := 0;
  NamesLBx.ItemIndex := FCurrent;
  NamesLBx.Checked[FCurrent] := True;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Select Genetic Code';
  textHeight := CodesLBx.Canvas.TextHeight('Standard');
  NamesLBx.Height := (TextHeight * GetNoOfDefaultCodeTables) + 30;
  CodesLBx.Width := 0;
  CodesLBx.Height := 0;
end;

procedure TSelectGeneticCodeDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSelectGeneticCodeDlg.AddSBtnClick(Sender: TObject);
var
  NewCodeTableDlg: TDefineNewGeneticCodeDlg = nil;
begin
  try try
    NewCodeTableDlg := TDefineNewGeneticCodeDlg.Create(Self);
    NewCodeTableDlg.TableName              := 'Enter Genetic Code Name';
    NewCodeTableDlg.ReadOnlyTableName      := False;

    if NamesLBx.ItemIndex < 0 then
      NamesLBx.ItemIndex := Trunc(CodeSelSEdit.Value);

    with CodesLBx do
      NewCodeTableDlg.CodeTable := Items[NamesLBx.ItemIndex];

    with NewCodeTableDlg do
      while True do
      begin
        if ShowModal = mrOK then
        begin
           if NamesLBx.Items.IndexOf(TableName) >= 0 then
            MessageDlg('Error: Duplicate name - '+Name+' . Try again!', mtError, [mbOK],0)
          else
          begin
            NamesLBx.Items.Add(TableName);
            CodesLBx.Items.Add(CodeTable);
            NamesLBx.ItemIndex := NamesLBx.Items.Count-1;
            Break;
          end;
        end
      else
        Break;
      end;
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;
  finally
    if Assigned(NewCodeTableDlg) then
      NewCodeTableDlg.Free;
  end;
end;

procedure TSelectGeneticCodeDlg.ComputeSBtnClick(Sender: TObject);
var
  Format: TExportType;
  SaveLoc: String = '';
begin
  Format := PromptUserWriteOutput(SaveLoc);
  if Format = EXnone then
    exit;
  StatsSelectedCode(Format, SaveLoc);
end;

procedure TSelectGeneticCodeDlg.DeleteSBtnClick(Sender: TObject);
var
  CurIndex, i : Integer;
begin
  if NamesLBx.Items.Count = 1 then
  begin
    ShowMessage('Sorry, you cannot delete all code tables.');
    Exit;
  end;

  if NamesLBx.ItemIndex < 0 then
      NamesLBx.ItemIndex := Trunc(CodeSelSEdit.Value);

  with NamesLBx do
   if Checked[ItemIndex] then
    begin
      ShowMessage('Select another code table before deleting this one.');
      Exit;
    end;

  // delete the table from the display list
  CurIndex := NamesLBx.ItemIndex;

  NamesLBx.Items.Delete(CurIndex);
  CodesLBx.Items.Delete(CurIndex);

  // get the correct index of CodeSelSEdit.Value
  for i:=0 to NamesLBx.Count-1 do
    if NamesLBx.Checked[i] then
    begin
      CodeSelSEdit.Value := i;
      break;
    end;
end;

procedure TSelectGeneticCodeDlg.EditSBtnClick(Sender: TObject);
var
  CurIndex : Integer;
  NewCodeTableDlg: TDefineNewGeneticCodeDlg = nil;
  CodeName: AnsiString;
begin
  CurIndex := NamesLBx.ItemIndex;
  if NamesLBx.ItemIndex < 0 then
    NamesLBx.ItemIndex := Trunc(CodeSelSEdit.Value);
  CurIndex := NamesLBx.ItemIndex;

  try try
    NewCodeTableDlg := TDefineNewGeneticCodeDlg.Create(Self);
    NewCodeTableDlg.ReadOnlyTableName := False;
    CodeName := NamesLBx.Items[CurIndex];
    NewCodeTableDlg.TableName := CodeName;

    with CodesLBx do
      NewCodeTableDlg.CodeTable := Items[CurIndex];

    with NewCodeTableDlg do
      while True do
      begin
        if ShowModal = mrOK then
        begin
          if (CompareText(NamesLBx.Items[CurIndex], TableName) = 0) or
             (NamesLBx.Items.IndexOf(Name) < 0) then
          begin
            NamesLBx.Items[CurIndex] := TableName;
            CodesLBx.Items[CurIndex] := CodeTable;
            NamesLBx.ItemIndex := CurIndex;
            Break;
          end
          else
            MessageDlg('Error: Duplicate name - '+Name+' . Try again!', mtError, [mbOK],0);
        end
        else Break;
      end;
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;
  finally
    if Assigned(NewCodeTableDlg) then
      NewCodeTableDlg.Free;
  end;
end;

procedure TSelectGeneticCodeDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if BottomPanel.Width > ToolBar1.Width then
    ToolBar1.Left := Round((BottomPanel.Width - ToolBar1.Width)/2);
  Constraints.MinWidth := ToolBar1.Width + 20;
end;

procedure TSelectGeneticCodeDlg.FormResize(Sender: TObject);
begin
  if BottomPanel.Width > ToolBar1.Width then
    ToolBar1.Left := Round((BottomPanel.Width - ToolBar1.Width)/2);
end;

procedure TSelectGeneticCodeDlg.NamesLBxClickCheck(Sender: TObject);
var
  current: Integer = -1;
begin
  current := NamesLBx.ItemIndex;
  if current < 0 then
    Exit;
  SetCheckedBox(current);
end;

function TSelectGeneticCodeDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TSelectGeneticCodeDlg.FormShow(Sender: TObject);
begin

end;

procedure TSelectGeneticCodeDlg.HelpBtnClick(Sender: TObject);
begin
  try
    ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when initializing the help browser: ' + E.Message);
  end;
end;

procedure TSelectGeneticCodeDlg.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSelectGeneticCodeDlg.ViewSBtnClick(Sender: TObject);
var
  Format: TExportType;
  SaveLoc: String = '';
begin
  Format := PromptUserWriteOutput(SaveLoc);
  if Format = EXnone then
    exit;
  ViewSelectedCode(Format, SaveLoc);
end;

procedure TSelectGeneticCodeDlg.SetCheckedBox(index: Integer);
var
  i: Integer;
begin
  FCurrent := index;
  if NamesLBx.Items.Count > 0 then
    for i := 0 to NamesLBx.Items.Count - 1 do
      if NamesLBx.Checked[i] and (i <> FCurrent) then
        NamesLBx.Checked[i] := False;
  if not NamesLBx.Checked[FCurrent] then
    NamesLBx.Checked[FCurrent] := True;
  NamesLBx.ItemIndex := FCurrent;
end;

function TSelectGeneticCodeDlg.GetCodeTable: String;
begin
  Result := CodesLBx.Items[NamesLBx.ItemIndex];
end;

function TSelectGeneticCodeDlg.GetCodeTableName: String;
begin
  Result := NamesLBx.Items[FCurrent];
end;

procedure TSelectGeneticCodeDlg.SetCodeTableName(AValue: String);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  with NamesLBx do
    for i:=0 to Items.Count-1 do
    begin
      if CompareText(OtuNameToMegaStr(Items[i]), OtuNameToMegaStr(AValue)) = 0 then
      begin
        Found := True;
        SetCheckedBox(i);
        break;
      end;
    end;
  if not Found then
    raise Exception.Create('Code Table : '+AValue+' was not found.');
end;

procedure TSelectGeneticCodeDlg.ViewSelectedCode(ExportType: TExportType; SaveLoc: AnsiString);
var
  Ch : AnsiChar;
  i, j, k, Index: Integer;
  CodeName, TheTable, AStr: AnsiString;
  SaveLocation: String;
  TheLines: TStringList = nil;
  CodonName: array[0..4] of AnsiChar;
  ViewXls: TExcelWrite = nil;
  CSVOut: TStringList = nil;
begin
  if NamesLBx.ItemIndex < 0 then
    NamesLBx.ItemIndex := Trunc(CodeSelSEdit.Value);
  CodeName := NamesLBx.Items[NamesLBx.ItemIndex];

  try
    TheLines := TStringList.Create;
    ViewXls := TExcelWrite.Create(Self, 'Code Table '+ CodeName);
    ViewXls.IsXLS := True;
    AStr := 'Code Table: '+ CodeName;
    ViewXls.WriteLine(AStr);
    TheTable := CodesLBx.Items[NamesLBx.ItemIndex];
    TheLines.Add(AStr);
    CodonName[3] := #0;
    TheLines.Add(' +----------+----------+----------+----------+');
    for i := 0 to 3  do
    begin
      CodonName[0] := BaseName[i];
      for j := 0 to 3 do
      begin
        AStr := ' | ';
        CodonName[2] := BaseName[j];
        for k := 0 to 3 do
        begin
          CodonName[1] := BaseName[k];
          Index := i*16+ k*4 + j; // due to display purposes
          Ch := TheTable[Index+1];  // 0,1,2,3; 16,17,18,19; ..
          AStr := AStr + CodonName+'  ' + GetThreeLetterCode(Ch)+' | ';
          ViewXls.Add(CodonName+'  ' + GetThreeLetterCode(Ch));
        end;
        TheLines.Add(AStr);
        ViewXls.WriteLine();
      end;
      TheLines.Add(' +----------+----------+----------+----------+');
    end;
  except
    On E:Exception do
    begin
      ShowMessage(E.Message);
      if TheLines <> nil then
        TheLines.Free;
      TheLines := nil;
      Exit;
    end;
  end;

  try try
    if ExportIsWorkbookDisplay(ExportType) then
    begin
      SaveLocation := ExcelWrite.GetSaveLocation(ExportType);
      ViewXls.SaveFile(SaveLocation, ExcelExportToFileType(ExportType));
      RunAProgram(SaveLocation);
    end
    else if ExportIsWorkbookSave(ExportType) then
      ViewXls.SaveFile(SaveLoc, ExcelExportToFileType(ExportType))
    else if ExportType = EXcsvSave then
    begin
      CSVOut := ViewXls.GetCsvList;
      CSVOut.SaveToFile(SaveLoc)
    end
    else if ExportType = EXtextSave then
      TheLines.SavetoFile(SaveLoc)
    else
     OpenStringList(TheLines, 'Code Table: '+ CodeName);
  except
    On E: Exception do
      ShowMessage(E.message);
  end;
  finally
    if Assigned(CSVOut) then
      CSVOut.Free;
    if Assigned(TheLines) then
      TheLines.Free;
    if Assigned(ViewXls) then
      ViewXls.Free;
  end;
end;

procedure TSelectGeneticCodeDlg.StatsSelectedCode(ExportType: TExportType; SaveLoc: AnsiString);
var
  MyStrList : TStringList = nil;
  MyCodonInfo: TCodonInfo = nil;
  SaveLocation : String;
  StatXls: TExcelWrite = nil;
  CSVOut: TStringList = nil;
begin
  if NamesLBx.ItemIndex < 0 then
    NamesLBx.ItemIndex := Trunc(CodeSelSEdit.Value);

  //Just call CodonInfo and do the computation
  try try
    //
    StatXls := TExcelWrite.Create(self, 'Syn Nonsyn sites');
    StatXls.IsXLS := True;
    //StatXls.AddWorksheet('Info');
    MyCodonInfo := TCodonInfo.Create;
    MyCodonInfo.TsTvRatio := 1; // Future release will change it

    // Code Table is obtained here
    MyCodonInfo.CodeTable := CodesLBx.Items[NamesLBx.ItemIndex];

    MyCodonInfo.GenerateSynSiteTable;

    MyStrList := TStringList.Create;

    with NamesLBx do
    begin
      MyStrList.Add('Code Table: '+Items[ItemIndex]);

      StatXls.Add('Code Table: ' + Items[ItemIndex]);
      StatXls.WriteLine(0, 'A', '', True);
      MyStrList.Add('Method:     '+'Nei-Gojobori (1986) methodology');

      StatXls.Add('Method:  Nei-Gojobori (1986) methodology');
      StatXls.WriteLine(0, 'A', '', True);
    end;
    // add code table name
    MyCodonInfo.WriteIntrinsicTables(MyStrList, True, False); // syn, DsDn
    MyCodonInfo.WriteIntrinsicTablesExcel(StatXls, True, False);
    // replace with the rest
    if Assigned(MyCodonInfo) then
      MyCodonInfo.Free;
    MyCodonInfo := nil;
    StatXls.MergeCells(Rect(1, 5, 2, 5), aCenter, aCenter);
    StatXls.MergeCells(Rect(3, 5, 5, 5), aCenter, aCenter);

    if ExportIsWorkbookDisplay(ExportType) then
    begin
      SaveLocation := ExcelWrite.GetSaveLocation(ExportType);
      StatXls.SaveFile(SaveLocation, ExcelExportToFileType(ExportType));
      RunAProgram(SaveLocation);
    end
    else if ExportIsWorkbookSave(ExportType) then
      StatXls.SaveFile(SaveLoc, ExcelExportToFileType(ExportType))
    else if ExportType = EXcsvSave then
    begin
      CSVOut := StatXls.GetCsvList;
      CSVOut.SaveToFile(SaveLoc)
    end
    else if ExportType = EXtextSave then
      MyStrList.SaveToFile(SaveLoc)
    else
      OpenStringList(MyStrList, 'Syn/Nonsyn sites');
  except
    On E: Exception do ShowMessage(E.Message);
  end
  finally
    if Assigned(CSVOut) then
      CSVOut.FRee;
    if StatXls <> nil     then
      StatXls.Free;
    if MyStrList <> nil   then
      MyStrList.Free;
    if MyCodonInfo <> nil then
      MyCodonInfo.Free;
  end;
end;

function TSelectGeneticCodeDlg.GetGeneticCodeNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  try
    if NamesLBx.Items.Count > 0 then
      for i := 0 to NamesLBx.Items.Count - 1 do
        Result.Add(NamesLBx.Items[i]);
  except
    result := nil;
  end;
end;

procedure TSelectGeneticCodeDlg.ExportGeneticCodeStatistics(ExportTypeStr: String; SaveLocation: String);
var
  MyStrList : TStringList;
  MyCodonInfo: TCodonInfo;
  StatXls: TExcelWrite;
  ExportType: TExportType;
begin
  {$IFDEF VISUAL_BUILD}
  StatXls := nil;
  MyCodonInfo := nil;
  MyStrList   := nil;
  if (ExportTypeStr = 'EXexcelDisp') then
     ExportType := EXexcelDisp
  else if (ExportTypeStr = 'EXexcelSave') then
     ExportType := EXexcelSave
  else if (ExportTypeStr = 'EXcsvSave') then
     ExportType := EXcsvSave
  else if (ExportTypeStr = 'EXtext') then
     ExportType := EXtext
  else
     ExportType := EXnone;
  try try
    StatXls := TExcelWrite.Create(self, 'Syn Nonsyn sites');
    StatXls.IsXLS := (ExportType = EXexcelDisp) or (ExportType = EXexcelSave) or (ExportType = EXcsvSave);
    MyCodonInfo := TCodonInfo.Create;
    MyCodonInfo.TsTvRatio := 1;
    MyCodonInfo.CodeTable := D_InputSeqData.CodeTable;
    MyCodonInfo.GenerateSynSiteTable;
    MyStrList := TStringList.Create;
    begin
      MyStrList. Add('Code Table: '+ D_InputSeqData.CodeName);
      StatXls.Add('Code Table: ' + D_InputSeqData.CodeName);
      StatXls.WriteLine(0, 'A', '', True);
      MyStrList.Add('Method:     '+'Nei-Gojobori (1986) methodolgy');

      StatXls.Add('Method:  Nei-Gojobori (1986) methodology');
      StatXls.WriteLine(0, 'A', '', True);
    end;
    MyCodonInfo.WriteIntrinsicTables(MyStrList, True, False);
    MyCodonInfo.WriteIntrinsicTablesExcel(StatXls, True, False);
    if Assigned(MyCodonInfo) then
      MyCodonInfo.Free;
    MyCodonInfo := nil;
    StatXls.MergeCells(Rect(1, 5, 2, 5), aCenter, aCenter);
    StatXls.MergeCells(Rect(3, 5, 5, 5), aCenter, aCenter);
    SaveLocation := ExcelWrite.GetSaveLocation;
    if ExportType = EXexcelDisp then
    begin
      StatXls.SaveFile(SaveLocation, ExportExcel, False);
      RunAProgram(SaveLocation);
    end
    else if ExportType = EXexcelSave then
      StatXls.SaveFile(SaveLocation, ExportExcel, False)
    else if ExportType = EXcsvSave then
      StatXls.SaveFile(SaveLocation, ExportCSV, False)
    else
      OpenStringList(MyStrList, 'Syn/Nonsyn sites');
  except
    On E: Exception do ShowMessage(E.Message);
  end
  finally
    if StatXls <> nil     then StatXls.Free;
    if MyStrList <> nil   then MyStrList.Free;
    if MyCodonInfo <> nil then MyCodonInfo.Free;
  end;
  {$ENDIF}
end;

procedure TSelectGeneticCodeDlg.ExportGeneticCodeTable(ExportTypeStr: String; SaveLocation: String);
var
  Ch : AnsiChar;
  i, j, k, Index: Integer;
  AStr: AnsiString;
  TheLines: TStringList;
  CodonName: array[0..4] of AnsiChar;
  ViewXls: TExcelWrite;
  ExportType: TExportType;
begin
  {$IFDEF VISUAL_BUILD}
  TheLines := nil;
  ViewXls := nil;
  BaseName[0] := 'U';
  BaseName[1] := 'C';
  BaseName[2] := 'A';
  BaseName[3] := 'G';
  try
    TheLines := TStringList.Create;
    ViewXls := TExcelWrite.Create(Self, 'Code Table '+ D_InputSeqData.CodeName);
    if (ExportTypeStr = 'EXexcelDisp') then
       ExportType := EXexcelDisp
    else if (ExportTypeStr = 'EXexcelSave') then
       ExportType := EXexcelSave
    else if (ExportTypeStr = 'EXcsvSave') then
       ExportType := EXcsvSave
    else if (ExportTypeStr = 'EXtext') then
       ExportType := EXtext
    else
       ExportType := EXnone;
    ViewXls.IsXLS := (ExportType = EXexcelDisp) or (ExportType = EXexcelSave) or (ExportType = EXcsvSave);
    AStr := 'Code Table: '+ D_InputSeqData.CodeName;
    ViewXls.WriteLine(AStr);
    TheLines.Add(AStr);
    CodonName[3] := #0;
    TheLines.Add(' +----------+----------+----------+----------+');
    for i := 0 to 3  do
    begin
      CodonName[0] := BaseName[i];
      for j := 0 to 3 do
      begin
        AStr := ' | ';
        CodonName[2] := BaseName[j];
        for k := 0 to 3 do
        begin
          CodonName[1] := BaseName[k];
          Index := i*16+ k*4 + j;
          Ch := D_InputSeqData.CodeTable[Index+1];
          AStr := AStr + CodonName+'  ' + GetThreeLetterCode(Ch)+' | ';
          ViewXls.Add(CodonName+'  ' + GetThreeLetterCode(Ch));
        end;
        TheLines.Add(AStr);
        ViewXls.WriteLine();
      end;
      TheLines.Add(' +----------+----------+----------+----------+');
    end;
  except
    On E:Exception do
    begin
      ShowMessage(E.Message);
      if TheLines <> nil then
        TheLines.Free;
      TheLines := nil;
      Exit;
    end;
  end;
  try try
    SaveLocation := ExcelWrite.GetSaveLocation;
    if ExportType = EXexcelDisp then
    begin
      ViewXls.SaveFile(SaveLocation, ExportExcel, False);
      RunAProgram(SaveLocation);
    end
    else if ExportType = EXexcelSave then
      ViewXls.SaveFile(SaveLocation, ExportExcel, False)
    else if ExportType = EXcsvSave then
      ViewXls.SaveFile(SaveLocation, ExportCSV, False)
    else
     OpenStringList(TheLines, 'Code Table: '+ D_InputSeqData.CodeName);
  except
    On E: Exception do
      ShowMessage(E.message);
  end;
  finally
    TheLines.Free;
    ViewXls.Free;
  end;
  {$ENDIF}
end;

end.

