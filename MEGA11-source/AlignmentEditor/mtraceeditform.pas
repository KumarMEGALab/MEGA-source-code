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

unit MTraceEditForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
{$IFDEF VISUAL_BUILD}
uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  LCLIntF, LCLType, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, MTraceEdit, Math, Menus, ComCtrls, ActnList,
  ImgList, Clipbrd, Printers, StdCtrls, IniPropStorage, MegaUtils,
  PrintersDlgs, mimageform, MegaPrivateFiles, miniformstream;

type

  { TTraceEditForm }

  TTraceEditForm = class(TForm)
    IniPropStorage1: TIniPropStorage;
    MenuItem1: TMenuItem;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    StatusBar1: TStatusBar;
    ActionList1: TActionList;
    ActionUndo: TAction;
    ActionFind: TAction;
    ActionFindNext: TAction;
    ActionFindPrev: TAction;
    ActionNextN: TAction;
    ActionMaskLeft: TAction;
    ActionMaskRight: TAction;
    ActionClearMask: TAction;
    ActionCopyFASTA: TAction;
    ActionCopyText: TAction;
    ActionOpen: TAction;
    ActionSave: TAction;
    ActionSend: TAction;
    ActionExit: TAction;
    ActionReverse: TAction;
    ActionFont: TAction;
    Panel3: TPanel;
    Panel2: TPanel;
    VertTrackBar: TTrackBar;
    FontDlg: TFontDialog;
    ActionCopyImage: TAction;
    ActionThickLine: TAction;
    ActionThickerLine: TAction;
    ActionThinLine: TAction;
    ActionPrint: TAction;
    ActionPrintSetup: TAction;
    PrintSetupDlg: TPrinterSetupDialog;
    MainMenu1: TMainMenu;
    Data1: TMenuItem;
    OpenFile1: TMenuItem;
    N1: TMenuItem;
    SaveFile1: TMenuItem;
    AddtoAlignPad1: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    N4: TMenuItem;
    Copy1: TMenuItem;
    FASTformat1: TMenuItem;
    PlainText1: TMenuItem;
    N5: TMenuItem;
    raceImageinWindow1: TMenuItem;
    MaskUpstream1: TMenuItem;
    MaskDownstream1: TMenuItem;
    N6: TMenuItem;
    ReverseComplement1: TMenuItem;
    Search1: TMenuItem;
    Find1: TMenuItem;
    FindNext1: TMenuItem;
    FindNext2: TMenuItem;
    N7: TMenuItem;
    NextN1: TMenuItem;
    Display1: TMenuItem;
    Font1: TMenuItem;
    LineWidth1: TMenuItem;
    N1pixel1: TMenuItem;
    N2pixels1: TMenuItem;
    N3pixels1: TMenuItem;
    Bevel1: TBevel;
    ActionOpenNewWindow: TAction;
    ActionOpenNewWindow1: TMenuItem;
    ActionPaste: TAction;
    ActionFindInFile: TAction;
    ActionBLAST: TAction;
    N2: TMenuItem;
    FindinFile1: TMenuItem;
    ActionBLAST1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    Help1: TMenuItem;
    HorzTrackBar: TTrackBar;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    ExportFASTAFile1: TMenuItem;
    ActionExport: TAction;
    ExportDlg: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPrevExecute(Sender: TObject);
    procedure ActionNextNExecute(Sender: TObject);
    procedure ActionMaskRightExecute(Sender: TObject);
    procedure ActionMaskLeftExecute(Sender: TObject);
    procedure ActionClearMaskExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSendExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionCopyFASTAExecute(Sender: TObject);
    procedure ActionCopyTextExecute(Sender: TObject);
    procedure ActionReverseExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure IniPropStorage1RestoreProperties(Sender: TObject);
    procedure IniPropStorage1SaveProperties(Sender: TObject);
    procedure VertTrackBarChange(Sender: TObject);
    procedure HorzTrackBarChange(Sender: TObject);
    procedure TraceEditMoved(Sender: TObject);
    procedure ActionCopyImageExecute(Sender: TObject);
    procedure ActionThickLineExecute(Sender: TObject);
    procedure ActionThinLineExecute(Sender: TObject);
    procedure ActionThickerLineExecute(Sender: TObject);
    procedure ActionPrintSetupExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionOpenNewWindowExecute(Sender: TObject);
    procedure ActionFindInFileExecute(Sender: TObject);
    procedure ActionBLASTExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
  private
    procedure InitMainMenu;
    procedure InitTraceEdit;
    procedure ScrollToCursor;
    procedure CopyBitmapToClipboard;

    procedure SetCaption;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    TraceEdit: TTraceEdit;
    SearchStr: String;
  end;

  function CreateNewTraceEditorWindow: TTraceEditForm;
  function OpenFileInNewTraceEditorWindow(filename: String): TTraceEditForm;
{$ENDIF}
implementation
{$IFDEF VISUAL_BUILD}
{$R *.lfm}

uses
  MAlignEditMainForm, MWebBrowser, MAlignGrid, mhelpfiles, mhelpkeywords,
  MMegaWindowInfo, AlnBuilder_HC, MegaVerConsts, mega_main,
  MD_Sequences, LCLProc, mshortcutshelper;

{$IFDEF MSWINDOWS} VAR Mutex: THandle; {$ENDIF}

function CheckPrevInst(title: AnsiString): boolean;
var
  PrevWnd: HWnd;
  a : array [0..1023] of AnsiChar;
  i : integer;
begin
  result := false;
  {$IFDEF MSWINDOWS}
    Mutex := CreateMutex(NIL, False, 'SingleInstanceProgramHMutex2'); // conflicts when opening ABI files from double click because it has the same name as the main program and it hasn't releaseed the mutex yet.  Without this change mega terminates
    {wait if other instance still initializing; quit if too long}
    IF WaitForSingleObject(Mutex, 10000) = WAIT_TIMEOUT THEN Halt;
  {$ELSE}
    //IF HPREVINST = 0 then Exit;
  {$ENDIF}

  for i := 0 to 1023 do
    a[i] := #0;

  {$IFDEF MSWINDOWS}
  PrevWnd := FindWindowA('TTraceEditForm', StrPCopy(a, title));
  if PrevWnd <> 0 then
  begin
    if IsIconic(PrevWnd) then
      ShowWindow(PrevWnd, SW_SHOWNORMAL)
    else
     {$IFDEF MSWINDOWS}
       SetForegroundWindow(PrevWnd);
     {$ELSE}
       BringWindowToTop(PrevWnd);
     {$ENDIF}
      result := true;
    end;
  {$ENDIF}
end;

function GetCaption(filename: String): String;
begin
  result := 'TraceEditor ('+filename+')'
end;

function CreateNewTraceEditorWindow: TTraceEditForm;
begin
  Result := TTraceEditForm.Create(Application);
  Result.ActionOpenExecute(nil);
  if Result.TraceEdit.Empty then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function OpenFileInNewTraceEditorWindow(filename: String): TTraceEditForm;
begin
  result := nil;
  if (filename = '') or not FileExists(filename) then
  begin
    result := CreateNewTraceEditorWindow;
    exit;
  end;
  if CheckPrevInst(GetCaption(filename)) then
    exit;

  Result := TTraceEditForm.Create(Application);
  if Result.TraceEdit.LoadFromFile(filename) then
    Result.SetCaption
  else
    ShowMessage('Invalid file format. '''+ExtractFileName(filename)+''' file could not be opened.');

  if Result.TraceEdit.Empty then
  begin
    Result.Free;
    Result := nil;
  end;
end;

procedure TTraceEditForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TTraceEditForm.SetCaption;
begin
  if TraceEdit.Empty or (TraceEdit.Data.FileName = '') then exit;
  Caption := GetCaption(TraceEdit.Data.FileName);
  
  OpenDlg.InitialDir := ExtractFileDir(TraceEdit.Data.FileName);
end;

procedure TTraceEditForm.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  InitTraceEdit;
  HelpContext := HC_Trace_Data_File_Viewer_Editor;
  Data1.HelpContext := HC_Trace_Data_File_Viewer_Editor;
  Edit1.HelpContext := HC_Trace_Data_File_Viewer_Editor;
  Search1.HelpContext := HC_Trace_Data_File_Viewer_Editor;
  Display1.HelpContext := HC_Trace_Data_File_Viewer_Editor;
  AddWindowToTray(self);
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Trace Data Viewer/Editor';
  {$IFNDEF DARWIN}
  InitMainMenu;
  {$ELSE}
  ActionFindNext.ShortCut := TextToShortCut('CTRL+G');
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TTraceEditForm.FormDestroy(Sender: TObject);
begin
  RemoveWindowFromTray(Self);
  if Assigned(TraceEdit) then
    FreeAndNil(TraceEdit);
end;

procedure TTraceEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  check: word;
begin
  Action := caFree;
  if TraceEdit.Modified then
  begin
    check := MessageDlg('Closing Trace Editor. Do you wish to save the data to file?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if check = mrYes then
      ActionSaveExecute(Sender)
    else if check = mrCancel then
      Action := caNone;
  end;
end;

procedure TTraceEditForm.ActionUpdate(Sender: TObject);
begin
  ActionOpen.Enabled  := true;
  ActionOpenNewWindow.Enabled := true;
  ActionSave.Enabled  := TraceEdit.Data <> nil;
  ActionExport.Enabled  := TraceEdit.Data <> nil;
  ActionSend.Enabled  := TraceEdit.Data <> nil;
  ActionPrint.Enabled := TraceEdit.Data <> nil;
  ActionExit.Enabled  := true;

  
  ActionUndo.Enabled := TraceEdit.UndoList.Count > 0;
  ActionCopyFASTA.Enabled := TraceEdit.Data <> nil;
  ActionCopyText.Enabled := TraceEdit.Data <> nil;
  ActionCopyImage.Enabled := TraceEdit.Data <> nil;
  ActionMaskLeft.Enabled := (TraceEdit.SelStart >= 0);
  ActionMaskRight.Enabled := (TraceEdit.SelStart >= 0);
  ActionClearMask.Enabled := (TraceEdit.Data.LeftClip > 0) or (TraceEdit.Data.RightClip < TraceEdit.Data.NoOfBases-1);
  ActionReverse.Enabled := TraceEdit.Data <> nil;

  ActionFind.Enabled := TraceEdit.Data <> nil;
  ActionFindNext.Enabled := ActionFind.Enabled and (SearchStr <> '');
  ActionFindPrev.Enabled := ActionFind.Enabled and (SearchStr <> '');
  ActionNextN.Enabled := TraceEdit.Data <> nil;

  ActionFindInFile.Enabled := TraceEdit.SelLength > 1;
  ActionBLAST.Enabled      := TraceEdit.Data <> nil;

  ActionFont.Enabled := TraceEdit.Data <> nil;
  ActionThinLine.Enabled := TraceEdit.Data <> nil;
  ActionThickLine.Enabled := TraceEdit.Data <> nil;
  ActionThickerLine.Enabled := TraceEdit.Data <> nil;
  ActionThinLine.Checked := TraceEdit.LineWidth = 1;
  ActionThickLine.Checked := TraceEdit.LineWidth = 2;
  ActionThickerLine.Checked := TraceEdit.LineWidth = 3;

  HorzTrackBar.Enabled := TraceEdit.Data <> nil;
  VertTrackBar.Enabled := TraceEdit.Data <> nil;
  HorzTrackBar.Position := trunc(TraceEdit.HorzScale*4);
  VertTrackBar.Position := 11 -trunc(TraceEdit.VertScale*2);
end;

procedure TTraceEditForm.ActionThinLineExecute(Sender: TObject);
begin
  if TraceEdit.LineWidth = 1 then exit;
  TraceEdit.LineWidth := 1;
  TraceEdit.Refresh;
end;

procedure TTraceEditForm.ActionThickLineExecute(Sender: TObject);
begin
  if TraceEdit.LineWidth = 2 then exit;
  TraceEdit.LineWidth := 2;
  TraceEdit.Refresh;
end;

procedure TTraceEditForm.ActionThickerLineExecute(Sender: TObject);
begin
  if TraceEdit.LineWidth = 3 then exit;
  TraceEdit.LineWidth := 3;
  TraceEdit.Refresh;
end;

procedure TTraceEditForm.ActionUndoExecute(Sender: TObject);
begin
  TraceEdit.Undo;
  ScrollToCursor;
end;

procedure TTraceEditForm.ScrollToCursor;
var
  pos, target: integer;
begin
  if TraceEdit.SelStart >= 0 then
    target := trunc(TraceEdit.Data.Peak[TraceEdit.SelStart+TraceEdit.SelLength-1]*TraceEdit.HorzScale)
  else if TraceEdit.Cursor >= 0 then
    target := TraceEdit.Cursor
  else
    target := 0;

  pos := TraceEdit.HorzPosition;
  if (target > pos) and (target < (pos +TraceEdit.ClientWidth)) then exit;

  pos := target -(TraceEdit.ClientWidth div 2);
  TraceEdit.HorzPosition := pos;
end;


procedure TTraceEditForm.CopyBitmapToClipboard;
var
  Bitmap: TBitMap = nil;
  aRect: TRect;
begin
  Clipboard.Clear;
  try
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(TraceEdit.ClientWidth, TraceEdit.ClientHeight);
    aRect := Rect(0, 0, Bitmap.Width, Bitmap.Height);
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(aRect);
    TraceEdit.DrawTraceData(bitmap.Canvas, TraceEdit.HorzPosition, TraceEdit.HorzPosition +TraceEdit.ClientWidth, 0, 0);
    Clipboard.Assign(Bitmap);
  finally
    if Assigned(Bitmap) then
      Bitmap.Free;
  end;
end;

procedure TTraceEditForm.ActionCopyImageExecute(Sender: TObject);
begin
  CopyBitmapToClipboard;
end;

procedure TTraceEditForm.ActionFindInFileExecute(Sender: TObject);
var
  NewTraceEditForm: TTraceEditForm;
begin
  NewTraceEditForm := CreateNewTraceEditorWindow;
  if Assigned(NewTraceEditForm) then
  begin
    NewTraceEditForm.Left := Left;
    NewTraceEditForm.Top  := Top+Height;
    NewTraceEditForm.Show;
    NewTraceEditForm.SearchStr := TraceEdit.SelectedSequence;
    NewTraceEditForm.ActionFindExecute(nil);
  end;
end;

procedure TTraceEditForm.ActionFindExecute(Sender: TObject);
begin
  if TraceEdit.SelLength > 1 then
    SearchStr := TraceEdit.SelectedSequence;
  if not InputQuery('Find', 'Enter the sequence to search', SearchStr) then exit;
  if TraceEdit.Find(SearchStr) then
    ScrollToCursor
  else
    ShowMessage('The query sequence was not found.');
end;

procedure TTraceEditForm.ActionFindNextExecute(Sender: TObject);
begin
  if TraceEdit.FindNext(SearchStr) then
    ScrollToCursor
  else
    ShowMessage('The query sequence was not found.');
end;

procedure TTraceEditForm.ActionFindPrevExecute(Sender: TObject);
begin
  if TraceEdit.FindPrev(SearchStr) then
    ScrollToCursor
  else
    ShowMessage('The query sequence was not found.');
end;

procedure TTraceEditForm.ActionNextNExecute(Sender: TObject);
begin
  TraceEdit.FindNext('N');
  ScrollToCursor;
end;

procedure TTraceEditForm.ActionMaskLeftExecute(Sender: TObject);
begin
  TraceEdit.MaskLeft;
end;

procedure TTraceEditForm.ActionMaskRightExecute(Sender: TObject);
begin
  TraceEdit.MaskRight;
end;

procedure TTraceEditForm.ActionClearMaskExecute(Sender: TObject);
begin
  TraceEdit.ClearMask;
end;

procedure TTraceEditForm.ActionOpenExecute(Sender: TObject);
var
  i: integer;
  tmpForm: TTraceEditForm;
begin
  OpenDlg.Options := [ofHideReadOnly,ofEnableSizing,ofAllowMultiSelect, ofFileMustExist ];
  OpenDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(OpenDlg.InitialDir);
  if not OpenDlg.Execute then exit;

  if OpenDlg.Files.Count > 0 then
    for i := 0 to OpenDlg.Files.Count-1 do
    begin
      if CheckPrevInst(GetCaption(OpenDlg.Files[i])) then
        continue;

      if i = OpenDlg.Files.Count-1 then
        tmpForm := self
      else
        tmpForm := TTraceEditForm.Create(Application);

      if tmpForm.TraceEdit.LoadFromFile(OpenDlg.Files[i]) then
      begin
        tmpForm.SetCaption;
        tmpForm.WindowState := wsNormal;
        tmpForm.Show;
      end
      else
      begin
        ShowMessage('Invalid file format. '''+ExtractFileName(OpenDlg.Files[i])+''' file could not be opened.');
        if tmpForm <> self then
          tmpForm.Free;
      end;
    end;
{
  if CheckPrevInst(GetCaption(OpenDlg.FileName)) then
    exit;

  if not TraceEdit.LoadFromFile(OpenDlg.FileName) then
  begin
    ShowMessage('Invalid file format. '''+ExtractFileName(OpenDlg.FileName)+''' file could not be opened.');
    exit;
  end;
  SetCaption;
}
end;

procedure TTraceEditForm.ActionOpenNewWindowExecute(Sender: TObject);
var
  TraceEditForm: TTraceEditForm;
begin
  TraceEditForm := CreateNewTraceEditorWindow;
  if TraceEditForm <> nil then
    TraceEditForm.Show;
end;

procedure TTraceEditForm.ActionSaveExecute(Sender: TObject);
begin
  if SaveDlg.FileName = '' then
    if SaveDlg.InitialDir = '' then
      SaveDlg.FileName := ExtractFilePath(TraceEdit.Data.FileName) +TraceEdit.Data.Name +'.scf'
    else
      SaveDlg.FileName := SaveDlg.InitialDir +'\' +TraceEdit.Data.Name +'.scf';
  SaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDlg.InitialDir);
  if not SaveDlg.Execute then exit;

  TraceEdit.SaveToFile(SaveDlg.FileName);

  SaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFilePath(SaveDlg.FileName));
  Caption := 'TraceEdit ('+SaveDlg.FileName+')';
end;

procedure TTraceEditForm.ActionExportExecute(Sender: TObject);
begin
  if ExportDlg.FileName = '' then
    if ExportDlg.InitialDir = '' then
      ExportDlg.FileName := ExtractFilePath(TraceEdit.Data.FileName) +TraceEdit.Data.Name +'.fas'
    else
      ExportDlg.FileName := ExportDlg.InitialDir +'\' +TraceEdit.Data.Name +'.fas';
  ExportDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExportDlg.InitialDir);
  if not ExportDlg.Execute then exit;

  TraceEdit.ExportToFASTAFile(ExportDlg.FileName);

  ExportDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFilePath(ExportDlg.FileName));
end;

procedure TTraceEditForm.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  {$ENDIF}
end;

procedure TTraceEditForm.InitTraceEdit;
begin
  TraceEdit := TTraceEdit.Create(Self);
  TraceEdit.Parent := Self;
  with TraceEdit do
  begin
    Left := 0;
    Top := 33;
    Width := 885;
    Height := 237;
    EditEnabled := True;
    HorzScale := 1.000000000000000000;
    VertScale := 1.000000000000000000;
    OnMoved := TraceEditMoved;
    MaskColor := clSilver;
    Align := alClient;
    Color := clWhite;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -15;
    Font.Name := 'Arial';
    Font.Style := [];
    LineWidth := 1;
    ParentColor := False;
    ParentFont := False;
    TabOrder := 3;
    TabStop := True;
  end;
end;

procedure TTraceEditForm.ActionSendExecute(Sender: TObject);
var
  seq: TSequence;
begin
  seq := TSequence.Create;
  seq.SeqName := TraceEdit.Data.Name;
  seq.SeqData := TraceEdit.Data.UnmaskedBases;
  seq.FileName:= TraceEdit.Data.FileName;

  FindAlignmentEditorWindow(false);
  with AlignEditMainForm do
  begin
    if PageControl1.ActivePageIndex = 1 then
      ActionTranslateExecute(nil);
    if (not AlignGrid1.IsDNA) and
       (MessageDlg('You are inserting DNA data in protein data.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel) then
    begin
      seq.Free;
      exit;
    end;
    try
      AlignGrid1.SkipHeaderUpdates := True;
      AlignGrid1.AppendSequence(seq);
    finally
      AlignGrid1.SkipHeaderUpdates := False;
    end;
    InitForm;
    WindowState := wsNormal;
  end;
  SetFocus;
  MessageDlg('Sequence data added to AlnExplorer successfully.', mtInformation, [mbOK], 0);
end;

procedure TTraceEditForm.ActionPrintExecute(Sender: TObject);
var
  aRect: TRect;
  n,i,dx,dpi,d,dh, ph,pw,w,h,x0,x,x1,b,hh : integer;
  r: double;
  aBitmap: TBitmap = nil;
begin
  {$IFDEF DARWIN}
    ShowMessage('Printing not yet implemented for macOS. Please use copy/paste to print from a document.');
  {$ENDIF}
  {$IFNDEF DARWIN}
  if not PrintSetupDlg.Execute then
    Exit;
  {$ENDIF}
  try
    try
      Printer.BeginDoc;
      aBitmap := TBitmap.Create;
      hh := Abs(TraceEdit.Font.Height);

      //dpi := trunc(GetDeviceCaps(Printer.Handle, HORZRES)/GetDeviceCaps(Printer.Handle, HORZSIZE)*25);
      dpi := trunc(Printer.PageWidth/Printer.XDPI)*2;
      //r :=  GetDeviceCaps(Printer.Handle, HORZRES)/GetDeviceCaps(Printer.Handle, HORZSIZE)
      //     /(GetDeviceCaps(Canvas.Handle, HORZRES)/GetDeviceCaps(Canvas.Handle, HORZSIZE));
      r := dpi/(GetDeviceCaps(TraceEdit.Canvas.Handle, HORZRES)/GetDeviceCaps(TraceEdit.Canvas.Handle, HORZSIZE));
      d := TraceEdit.Pitch;
      dh := Abs(TraceEdit.Font.Height);
      h := TraceEdit.Height + dh;
      if TraceEdit.Data.LeftClip > 0 then
        x0 := (TraceEdit.Data.Peak[TraceEdit.Data.LeftClip]+TraceEdit.Data.Peak[TraceEdit.Data.LeftClip-1]) div 2
      else
        x0 := 0;
      if TraceEdit.Data.RightClip <= TraceEdit.Data.NoOfBases then
        x1 := (TraceEdit.Data.Peak[TraceEdit.Data.RightClip-1]+TraceEdit.Data.Peak[TraceEdit.Data.RightClip-2]) div 2
      else
        x1 := TraceEdit.Data.Width;
      w := x1-x0;

      //ph := Printer.PageHeight -dpi -trunc(Abs(Font.Height)*r);
      //pw := Printer.PageWidth -dpi;
      ph := Printer.PageHeight - Printer.YDPI -trunc(Abs(Font.Height)*r);
      pw := Printer.PageWidth - Printer.XDPI;

      if pw*ph < trunc(w*h*r*r) then
      begin
        n := ceil(sqrt((w*ph)/(pw*h)));
        dx := trunc(pw/ph*h*n);
      end
      else
      begin
        dx := trunc(pw/r);
        n := ceil(w/dx);
        if dx > w then dx := w;
        pw := trunc(dx*r);
        ph := trunc(n*h*r);
      end;
      pw := pw +trunc(2*d*r);

      aBitmap.Width := dx +2*d + dh;
      aBitmap.Height  := n*h;
      aRect := Rect(0, 0, aBitmap.Width, aBitmap.Height);
      aBitmap.Canvas.Brush.Color := clWhite;
      aBitmap.Canvas.FillRect(aRect);
      for i := 0 to n-1 do
      begin
        b := TraceEdit.PosToSite(x0+dx);
        if b = TraceEdit.Data.NoOfBases-1 then
          x := trunc((3*TraceEdit.Data.Peak[b]-TraceEdit.Data.Peak[b-1])*TraceEdit.HorzScale/2)
        else
          x := trunc((TraceEdit.Data.Peak[b] +TraceEdit.Data.Peak[b+1])*TraceEdit.HorzScale/2);
        if x >= x1 then
          x := x1-1;
        TraceEdit.DrawTraceData(aBitmap.Canvas, x0, x, d*2, i*h+dh*2);
        x0 := x;
      end;

      aRect.Left := dpi div 2;
      if trunc(hh*r) > (dpi div 2) then
        aRect.Top := trunc(hh*r) +trunc(abs(Font.Height)*r)
      else
        aRect.Top := (dpi div 2) +trunc(abs(Font.Height)*r);
      aRect.Right := aRect.Left +pw;
      aRect.Bottom := aRect.Top +ph;

      d := abs(TraceEdit.Font.Height);
      aBitmap.Canvas.Font.Color := clBlack;
      aBitmap.Canvas.TextOut(d*4, 0, 'Sample: ' + TraceEdit.Data.Name);
      aBitmap.Canvas.TextOut(aBitmap.Canvas.TextWidth('Sample: '+ TraceEdit.Data.Name) + d*8, 0, 'File: ' + TraceEdit.Data.FileName);
      Printer.Canvas.StretchDraw(aRect, aBitmap);
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when printing: ' + E.Message);
    end;
  finally
    Printer.EndDoc;
    if Assigned(aBitmap) then
      aBitmap.Free;
  end;
end;

procedure TTraceEditForm.ActionPrintSetupExecute(Sender: TObject);
begin
  PrintSetupDlg.Execute;
end;

procedure TTraceEditForm.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TTraceEditForm.ActionCopyFASTAExecute(Sender: TObject);
begin
  TraceEdit.CopySequenceToClipboard;
end;

procedure TTraceEditForm.ActionCopyTextExecute(Sender: TObject);
begin
  if TraceEdit.SelLength > 1 then
    TraceEdit.CopySelectionToClipboard
  else
    TraceEdit.CopyBasesToClipboard;
end;

procedure TTraceEditForm.ActionReverseExecute(Sender: TObject);
begin
  TraceEdit.ReverseComplement;
end;

procedure TTraceEditForm.ActionFontExecute(Sender: TObject);
begin
  FontDlg.Font.Assign(TraceEdit.Font);
  if not FontDlg.Execute then exit;

  TraceEdit.Font.Assign(FontDlg.Font);
  TraceEdit.Refresh;
end;

function TTraceEditForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TTraceEditForm.IniPropStorage1RestoreProperties(Sender: TObject);
begin
  if FileExists(GetPrivateFile(sfTraceEditForm, False)) then
    TIniFormStream.IniPropStorageRestoreProperties(Self, GetPrivateFile(sfTraceEditForm));
end;

procedure TTraceEditForm.IniPropStorage1SaveProperties(Sender: TObject);
begin
  TIniFormStream.IniPropStorageSaveProperties(Self, GetPrivateFile(sfTraceEditForm, False));
end;

procedure TTraceEditForm.VertTrackBarChange(Sender: TObject);
begin
  if VertTrackBar.Position = round(11 -TraceEdit.VertScale*2) then exit;

  TraceEdit.VertScale := (11 - VertTrackBar.Position)*0.5;
  TraceEdit.Refresh;
end;

procedure TTraceEditForm.HorzTrackBarChange(Sender: TObject);
var
  pos: integer;
begin
  if HorzTrackBar.Position = round(TraceEdit.HorzScale*4) then exit;

  pos := TraceEdit.HorzPosition +(TraceEdit.ClientWidth div 2);
  pos := trunc(pos*HorzTrackBar.Position*0.25/TraceEdit.HorzScale) -(TraceEdit.ClientWidth div 2);

  TraceEdit.HorzScale := HorzTrackBar.Position*0.25;

  if pos <> HorzTrackBar.Position then
    TraceEdit.HorzPosition := pos
  else
    TraceEdit.Refresh;
end;

procedure TTraceEditForm.TraceEditMoved(Sender: TObject);
begin
  if (TraceEdit.SelStart >= 0) or (TraceEdit.Cursor >= 0) then
    ScrollToCursor;
end;

procedure TTraceEditForm.ActionBLASTExecute(Sender: TObject);
var
  ChromiumBrowser: TChromiumBrowser;
begin
    ChromiumBrowser := CreateNewChromiumBrowserWindow(bmBrowser);

  if TraceEdit.SelLength > 1 then
    ChromiumBrowser.BLAST(TraceEdit.SelectedSequence, true)
  else
    ChromiumBrowser.BLAST(TraceEdit.Data.UnmaskedBases, true);
end;

procedure TTraceEditForm.Help1Click(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TTraceEditForm.FormShow(Sender: TObject);
begin
  if (Left > Screen.Width-200) or (Top+Height > Screen.Height) then
  begin
    Left := 0;
    Top  := 0;
  end
  else
  begin
    Left := Left +20;
    Top  := Top +20;
  end;
  AddWindowToTray(self);
end;
{$ENDIF}
end.

