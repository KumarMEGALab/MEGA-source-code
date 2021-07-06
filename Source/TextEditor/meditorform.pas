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

unit MEditorForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{
This module was first designed by Sudhir Kumar for DcMemos
Then it was implemented and debugged by David Schwartz
Then it was modified and redesigned extensively for appearance and layout by Sudhir Kumar
Kumar also added drap-drop etc stuff into it and copy to clipboard utilities
It was again reworked by David Schwartz
Then, DcMemo was replaced with SynEdit by Glen Stecher and Dan Peterson

To Do:
Add state-saver system to Dlgs
Fix ConvertDlg so that the same item does not appear multiple times in the drop-down.
Added Drag-and-drop of 1 or more files: see code in OnActivate Event
Add HelpContexts

	modified: 3/20/01	by: David Schwartz
	* corrected a problem where the long file names weren't tracking with the tabsheets
	  by adding Set/GetLongTabSheetCaption() and changing references to a TStringList
	  to one of these functions.
	modified: 3/22/01	by: David Schwartz
	* added public function: procedure ConvertFileToMegaFormat; this allows
		an external program to instantiate the form and initiate a conversion.
		If the form already exists and a file is open, the conversion dialog
		will open just as if the convert button were clicked.
	* changed behavior of Convert_btnClick so that if nothing is open, it
		now opens the conversion dialog anyway; if the input file spec
		is blank, the dialog executes an OpenFile dialog.
	* function OpenStringList() wasn't completed; it should work now.
	* changed new_page() so it works properly with only a StringList supplied.
	* when you click the exit button, or File|Exit menu item, it now iteratively
		closes each open window, then exits the program.
  modified June 2012 by: Glen Stecher, Dan Peterson
  * replaced DcMemo with SynEdit
}


interface
uses
  {$IFDEF VISUAL_BUILD}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, stdctrls, Menus, Buttons, ImgList, ActnList,
  MFormatConvertToMega, MegaVerConsts, ExtCtrls,
  SynEdit, SynMemo, MInnerForm, MegaUtils, SynEditHighlighter,
  SynEditSearch,  SynEditMiscClasses, PrintersDlgs,
  mimageform, MegaPrivateFiles,
  {$ENDIF}
  LCLIntf, LCLType, IniPropStorage;

const
  ACTION_LIST_UPDATE_FREQUENCY = 500;

var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromStart: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbReplaceAll: boolean;

  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

type
  AddMeToList = procedure (ASubmenu: AnsiString; AEntry: AnsiString; ATag: Pointer) of object;

  {$IFDEF VISUAL_BUILD}
type

  { TEditorForm }

  TEditorForm = class(TForm)
    IniPropStorage1: TIniPropStorage;
    MacCloseBtn: TBitBtn;
    CloseBtnImages: TImageList;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FileCloseItem: TMenuItem;
    N4: TMenuItem;
    FileConvertItem: TMenuItem;
    FileExit: TMenuItem;
    EditMenu: TMenuItem;
    PrintDialog1: TPrintDialog;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    SearchFindItem: TMenuItem;
    SearchRelaceItem: TMenuItem;
    N2: TMenuItem;
    SearchGotoLineItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FontDialog1: TFontDialog;
    DisplaySetFontItem: TMenuItem;
    DisplayShowLineNumItem: TMenuItem;
    StandardToolBar: TToolBar;
    ConvertButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    PrintButton: TToolButton;
    ToolButton8: TToolButton;
    CutButton: TToolButton;
    CopyButton: TToolButton;
    PasteButton: TToolButton;
    UndoButton: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    FindBtn: TToolButton;
    ToolButton12: TToolButton;
    FontBtn: TToolButton;
    ReplaceBtn: TToolButton;
    ToolButton14: TToolButton;
    ToolButton13: TToolButton;
    ActionList1: TActionList;
    NewAction: TAction;
    OpenAction: TAction;
    SaveAction: TAction;
    SaveAsAction: TAction;
    PrintAction: TAction;
    ExitAction: TAction;
    ConvertAction: TAction;
    CutAction: TAction;
    CopyAction: TAction;
    PasteAction: TAction;
    FindAction: TAction;
    ReplaceAction: TAction;
    SetFontAction: TAction;
    GoToLineAction: TAction;
    FileNewItem: TMenuItem;
    SearchMenu: TMenuItem;
    UndoAction: TAction;
    RedoAction: TAction;
    EditUndoItem: TMenuItem;
    EditRedoItem: TMenuItem;
    CloseFileAction: TAction;
    N5: TMenuItem;
    N7: TMenuItem;
    N6: TMenuItem;
    FileDeleteItem: TMenuItem;
    DeleteBlockAction: TAction;
    SelectAllAction: TAction;
    ShowLineNumAction: TAction;
    EditSelectAllItem: TMenuItem;
    FileReopenItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    N11: TMenuItem;
    CloseFile1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GotoLine1: TMenuItem;
    N9: TMenuItem;
    N12: TMenuItem;
    ConverttoMEGAFormat1: TMenuItem;
    Print1: TMenuItem;
    DisplayMenu: TMenuItem;
    DisplayWordWrapItem: TMenuItem;
    N3: TMenuItem;
    CopyWMFToClipAction: TAction;
    CopyBMPToClipAction: TAction;
    CopyEMFToClipboardItem: TMenuItem;
    CopyBitmaptoClipboardItem: TMenuItem;
    CopyWinMetafiletoClipboardItem: TMenuItem;
    N10: TMenuItem;
    CopyEMFToClipAction: TAction;
    CopyScreenShotSubmenu: TMenuItem;
    CopyScreenshotToClipboard1: TMenuItem;
    EnhancedMetafileFormat1: TMenuItem;
    WindowsMetafileFormat1: TMenuItem;
    BitmapFormat1: TMenuItem;
    WordWrapAction: TAction;
    UtilitiesMenu: TMenuItem;
    MergeLinesItem: TMenuItem;
    CleanSequenceItem: TMenuItem;
    Formatby3Item: TMenuItem;
    Formatby10Item: TMenuItem;
    N13: TMenuItem;
    FormatSeqSubmenu: TMenuItem;
    ReverseCompItem: TMenuItem;
    N15: TMenuItem;
    N14: TMenuItem;
    PageControl1: TPageControl;
    //SynEditSearch: TSynEditSearch;
    //SynEditRegexSearch: TSynEditRegexSearch;
    CloseBtn: TBitBtn;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure NewActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure PrintActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure ConvertActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure SetFontActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure ReplaceActionExecute(Sender: TObject);
    procedure GoToLineActionExecute(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure CloseFileActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionList1Update(aAction: TBasicAction; var Handled: Boolean);
    procedure ShowLineNumActionExecute(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure DeleteBlockActionExecute(Sender: TObject);
    procedure MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure MemoStateChange(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    //procedure FileReopenMRUListClick(Sender: TObject; const ItemText: String; var Action: TOvcMRUClickAction);
    procedure FormActivate(Sender: TObject);
    procedure CopyWMFToClipActionExecute(Sender: TObject);
    procedure CopyBMPToClipActionExecute(Sender: TObject);
    procedure CopyEMFToClipActionExecute(Sender: TObject);
    procedure WordWrapActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MergeLinesItemClick(Sender: TObject);
    procedure CleanSequenceItemClick(Sender: TObject);
    procedure Formatby3ItemClick(Sender: TObject);
    procedure Formatby10ItemClick(Sender: TObject);
    procedure ReverseCompItemClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FileDropNotify(aFiles: TStrings);
  private
    fSearchFromStart: boolean;
    FOpenWithNewFile: Boolean;
    FFormIsClosing: Boolean;
    FLastUpdateTime: TDateTime;
    procedure InitMainMenu;
    procedure AssignContextSensitiveHelp;
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    function GetActiveMemo : TSynMemo;
    function FindOpenFilePage(fn: TFileName): integer;
    function IsFileOpen( fn : TFileName ) : boolean;
    procedure UnExpectedSearchException(Sender: TObject ;const Error : AnsiString);
    function CreateNewPage: TTabSheet;
    function SheetToForm(sheet: TTabSheet): TInnerForm;
    function GetActiveInnerForm: TInnerForm;
    procedure CloseSheet;
    procedure UpdateActionList;
    function GetNumOpenFiles: Integer;
  public
    function CloseFile(filename: String): Boolean;
    property NumOpenFiles: Integer read GetNumOpenFiles;
    property OpenWithNewFile: Boolean read FOpenWithNewFile write FOpenWithNewFile;
  end;

  function OpenFileAndFocus(fileName: String; Row: Integer; Column: Integer ): TEditorForm;
  function CloseTextEditorFile(filename: String): Boolean;
  function  OpenStringList( sl : TStrings; ACaption: AnsiString; FocusSelf: Boolean = false) : TEditorForm;
  procedure ConvertFileToMegaFormat;
  function  FindTextEditorWindow(OpenNewPage: Boolean=False): TEditorForm;
  function  IsFileOpen( fn : TFileName ) : boolean;
  function  FindOpenFilePage(fn: TFileName): integer;

var
  EditorForm: TEditorForm;
{$ENDIF}
implementation

{$R *.lfm}

uses
  {$IFDEF VISUAL_BUILD}
  {$IFDEF DARWIN}mshortcutshelper,{$ENDIF}
  mega_main, MFormatConvertOptDlg, MMegaWindowInfo, TextEditorHelp_HC,
  MegaConsts, MegaMainPreferences, OutputUtils, textsearchdialog, DlgReplaceText,
  DlgConfirmReplace, SynEditTypes, Math, mhelpkeywords, mhelpfiles,
  {$ENDIF}
  dateutils;



resourcestring
  STextNotFound = 'Text not found';

const
  TEMP_FILE_FLAG = 'NO_FILE';

type
  TTeOptions = set of (teMergeLines, teRemoveSpaces, teInsertSpacesBy3, teInsertSpacesBy10, teReverseComplement);
  {$IFDEF VISUAL_BUILD}
function FindTextEditorWindow(OpenNewPage: Boolean=False) : TEditorForm;
begin
  if (EditorForm = nil) then
    EditorForm := TEditorForm.Create(MegaForm);
  EditorForm.OpenWithNewFile := OpenNewPage;
  EditorForm.Show;
  Result := EditorForm;

end;


// returns -1 if fn isn't open, or the page# in the PageControl if it is.
// If it's open multiple times, then the first one is returned.
// This does a match on the full file name, not just the base name.
function FindOpenFilePage(fn: TFileName): integer;
begin
  Result := FindTextEditorWindow.FindOpenFilePage( fn );
end;

// returns TRUE if the file is open.
// This does a match on the full file name, not just the base name.
function IsFileOpen( fn : TFileName ) : boolean;
begin
  result := FindTextEditorWindow.IsFileOpen( fn );
end;

procedure ConvertFileToMegaFormat;
begin
  with FindTextEditorWindow do
  begin
    Show;
    ConvertActionExecute(nil);
  end;
end;

function CloseTextEditorFile(filename: String): Boolean;
var
  editor: TEditorForm = nil;
begin
  Result := False;
  editor := FindTextEditorWindow;
  if (trim(fileName) = EmptyStr) or (not FileExists(fileName)) or (not Assigned(editor)) then
    Exit;
  Result := editor.CloseFile(filename);
  if editor.PageControl1.PageCount = 0 then
    editor.Close;
end;

function OpenStringList( sl : TStrings; aCaption: AnsiString; FocusSelf: Boolean = false) : TEditorForm;
var
  InnerForm: TInnerForm  = nil;
  StringList: TStringList = nil;
begin
  if not Assigned(sl) then
    raise Exception.Create('Application error: trying to open nil TStrings');
  Result := FindTextEditorWindow;
  Result.CreateNewPage;
  InnerForm := Result.GetActiveInnerForm;
  StringList := TStringList.Create;
  StringList.Assign(sl);
  InnerForm.SetLines(StringList);
  if trim(aCaption)[1] = '*' then
  begin
    TTabSheet(InnerForm.Parent).Caption := Copy(aCaption, 2, Length(aCaption));
    InnerForm.Caption := Copy(aCaption, 2, Length(aCaption));
  end
  else
  begin
    InnerForm.Caption := aCaption;
    TTabSheet(InnerForm.Parent).Caption := aCaption;
  end;
  InnerForm.SynMemo1.Modified := True;
  Result.UpdateActionList;
  InnerForm.Memo.CaretX := 0;
  InnerForm.Memo.CaretY := 0;
  FreeAndNil(StringList);
  if FocusSelf then
    Result.SetFocus;
end;

function OpenNew( aCaption : AnsiString ) : TEditorForm;
var
  sl : TStringList;
  InnerForm: TInnerForm;
begin
  Result := FindTextEditorWindow;
  Result.CreateNewPage;
  sl := TStringList.Create;
  InnerForm := Result.GetActiveInnerForm;
  InnerForm.SetLines(sl);
  InnerForm.Caption := aCaption;
  TTabSheet(InnerForm.Parent).Caption := aCaption;
  sl.Free;
  Result.UpdateActionList;
end;


///  <summary>Opens the given text file and places the caret at the position
///  indicated by row and column</summary>
function OpenFileAndFocus(FileName: String; Row: Integer; Column: Integer ): TEditorForm;
var
  InnerForm: TInnerForm;
  Memo: TSynMemo;
  NumLines: Integer;
  NumColumns: Integer;
  tab: Integer;
begin
  Result := FindTextEditorWindow;
  if ((trim(FileName) = EmptyStr) or (not FileExists(FileName))) then
    Exit;
  if Result.IsFileOpen(Filename) then
  begin
    tab := Result.FindOpenFilePage(Filename);
    if tab >= 0 then
      Result.PageControl1.ActivePageIndex := tab;
    Exit;
  end;
  Result.CreateNewPage;
  InnerForm := Result.GetActiveInnerForm;
  Memo := InnerForm.Memo;

  try
    InnerForm.OpenFile(FileName);
    //Result.FileReopenMRUList.Add(FileName);
    NumLines := Memo.Lines.Count;
    NumColumns := Memo.Width;

    if Row < 0 then
      Row := 0
    else if Row > NumLines - 1 then
      Row := NumLines - 1;

    if Column < 1 then
      Column := 1
    else if Column > NumColumns then
      Column := NumColumns;

    Memo.CaretX := max(Column, 1);
    Memo.CaretY := Row;
  Except on E:Exception do
    begin
      if E.ClassType = EOutOfMemory then
        MessageDlg('The file you are trying to open is too large for MEGAs text editor to handle.', mtError, [mbOK], 0)
      else
      begin
        MessageDlg('There was a problem opening the text file.'+LineEnding+''+LineEnding+'Details: ', mtError, [mbOK], 0);
      end;
    end;
  end;
  InnerForm.FileName := FileName;
  TTabSheet(InnerForm.Parent).Caption := InnerForm.Caption;
  Result.UpdateActionList;
  Result.BringToFront;
end;


{ TEditorForm }

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  {$IFDEF DARWIN}
  UpdateShortcutsForMacOs(ActionList1);
  CloseBtn.Width := 30;
  CloseBtn.Height := 30;
  CloseBtn.Left := CloseBtn.Left - 20;
  MacCloseBtn.Visible := False;
  {$ELSE}
  MacCloseBtn.Visible := False;
  {$ENDIF}
  WordWrapAction.Visible := False; { hidden because synedit does not yet support it}
  FFormIsClosing := False;
  FLastUpdateTime := Now;
  FOpenWithNewFile := False;
  AddWindowToTray(Self);

  AssignContextSensitiveHelp;

  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Text File Editor and Format Converter';

  // Display Actions
  SetFontAction.Enabled       := False;
  ShowLineNumAction.Enabled   := False;
  WordWrapAction.Enabled      := False;

  // Edit Actions
  CopyBMPToClipAction.Visible := False;
  CopyWMFToClipAction.Visible := False;
  CopyEMFToClipAction.Visible := False;
  CopyAction.Enabled          := False;
  CutAction.Enabled           := False;
  PasteAction.Enabled         := False;
  UndoAction.Enabled          := False;
  RedoAction.Enabled          := False;
  DeleteBlockAction.Enabled   := False;
  SelectAllAction.Enabled     := False;

  SaveAction.Enabled          := False;
  SaveAsAction.Enabled        := False;
  CloseFileAction.Enabled     := False;

  FindAction.Enabled          := False;
  ReplaceAction.Enabled       := False;
  GoToLineAction.Enabled      := False;

  CopyScreenShotSubmenu.Enabled := False;
  CopyScreenShotSubmenu.Visible := False;
  FormatSeqSubmenu.Enabled      := False;
  ReverseCompItem.Enabled       := False;
  {$IFNDEF DARWIN}
  InitMainMenu;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  {$IFDEF VISUAL_BUILD}
  RemoveWindowFromTray(Self);
  {$ENDIF}
end;

function TEditorForm.IsFileOpen(fn: TFileName): boolean;
begin
  Result := (FindOpenFilePage(fn) >= 0);
end;

/// <summary>Searches through the list of tabs for a tab whose source file name
/// matches fn. If no match is found, returns -1.</summary>
function TEditorForm.FindOpenFilePage(fn: TFileName): integer;
var
  InnerForm: TInnerForm;
  i: Integer;
begin
  Result := -1;
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    InnerForm := SheetToForm(PageControl1.Pages[i]);
    if trim(InnerForm.FileName) = trim(fn) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TEditorForm.GetNumOpenFiles: Integer;
var
  i: integer;
  Memo: TSynMemo;
begin
  Result := 0;
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    Memo := SheetToForm(PageControl1.Pages[i]).Memo;
    if Memo.Modified then
    begin
      inc(Result);
    end;
  end;
end;

function TEditorForm.CloseFile(filename: String): Boolean;
var
  tab: Integer;
  innerForm: TInnerForm = nil;
  target: TTabSheet = nil;
begin
  Result := False;
  if IsFileOpen(filename) then
  begin
    tab := FindOpenFilePage(Filename);
    if tab >= 0 then
    begin
      target := PageControl1.Pages[tab];
      innerForm := SheetToForm(target);
      innerForm.CloseFile;
      innerForm.Close;
      PageControl1.Pages[tab].Free;
      Result := True;
    end;
  end;
end;

procedure TEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
  checked, aborted: boolean;
  questionRes: String;
  InnerForm: TInnerForm;
  aNumOpenFiles: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  FFormIsClosing := True;
  aNumOpenFiles := GetNumOpenFiles;

  aborted := false;
  if aNumOpenFiles > 1 then
  begin
    if not TryToGetPreferencesMainMenu(UserPref_AlwaysCloseTextEditorStr, checked, QuestionRes) then
      checked := false;
    if not checked then
    begin
      if MessageDlgCheck(Self, caption, 'Closing Editor', UserPref_AlwaysCloseTextEditorStr, checked, mtWarning, [mbOK, mbAbort], 0) = mrAbort then
        aborted := true;
      UpdatePreferencesMainMenu(UserPref_AlwaysCloseTextEditorStr, checked, BoolToStr(True));
    end
    else
    begin
      UpdatePreferencesMainMenu(UserPref_AlwaysCloseTextEditorStr, checked, BoolToStr(False));
    end;
    if not aborted then
    begin
      for i := PageControl1.PageCount-1 downto 0 do
      begin
        PageControl1.ActivePageIndex := i;
        InnerForm := GetActiveInnerForm;
        CloseFileActionExecute(nil);
      end;
    end;
  end
  else
    if NumOpenFiles = 1 then // if there's just a single file open we don't need to show the warning.
      CloseFileActionExecute(nil);

  aNumOpenFiles := GetNumOpenFiles;
  if (aNumOpenFiles > 0) then
  begin
    CloseAction := caNone;
    ActionList1.State := asNormal;
  end
  else
  begin
    {$IFDEF DARWIN}Hide;{$ENDIF}
    CloseAction := caFree;
    EditorForm := nil; // <- important to set missing status itself
  end;
  {$ENDIF}
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ActionList1.State := asSuspended;
  CanClose := True;
end;

procedure TEditorForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
  aList: TStringList = nil;
begin
  try
    try
      if Length(FileNames) > 0 then
      begin
        aList := TStringList.Create;
        for i := 0 to Length(Filenames) - 1 do
        begin
          if FileExists(FileNames[i]) then
          begin
            aList.LoadFromFile(Filenames[i]);
            OpenStringList(aList, ExtractFilename(FileNames[i]), True);
            aList.Clear;
          end;
        end;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TEditorForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  try
    Result := True;
    helpTopic := MapHelpContextToKeyword(Data);
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


procedure UpdateStatusBar( StatusBar:TStatusBar; Memo: TSynMemo);
var
  Row: Integer;
  Col: Integer;
begin

  If StatusBar.Panels.Count > 0 then
  begin
    Col := Memo.CaretX;
    Row := Memo.CaretY;
    StatusBar.Panels[0].Text := IntToStr(Row) + ' : ' + IntToStr(Col);
  end;

  if (StatusBar.Panels.Count > 1) then
    if (Memo.Modified) then
      StatusBar.Panels[1].Text := 'modified'
    else
      StatusBar.Panels[1].Text := EmptyStr;

  if (StatusBar.Panels.Count > 2) then
  begin
    if Memo.InsertMode then
      StatusBar.Panels[2].Text := 'Insert'
    else
      StatusBar.Panels[2].Text := 'Overwrite';
  end;
end;

procedure TEditorForm.MemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Memo: TSynMemo;
  InnerForm: TInnerForm;
begin
  if not Visible then Exit;
  InnerForm := GetActiveInnerForm;
  if not Assigned(InnerForm) then Exit;
  Memo := InnerForm.Memo;

  if (Memo <> nil) then
  begin
    if Memo.Modified then
      (InnerForm.Parent as TTabSheet).Caption := '*' + GetActiveInnerForm.Caption
    else
      (InnerForm.Parent as TTabSheet).Caption := GetActiveInnerForm.Caption;
    UpdateStatusBar(StatusBar1, Memo);
    UndoAction.Enabled := Memo.CanUndo;
    RedoAction.Enabled := Memo.CanRedo;
    CutAction.Enabled := Memo.SelAvail;
    CopyAction.Enabled := Memo.SelAvail;
    PasteAction.Enabled := Memo.CanPaste;
    FormatSeqSubmenu.Enabled := Memo.SelAvail;
    ReverseCompItem.Enabled := Memo.SelAvail;
  end;
end;

procedure TEditorForm.MemoStateChange(Sender: TObject);
var
  Memo: TSynMemo;
  InnerForm: TInnerForm;
begin
  InnerForm := GetActiveInnerForm;
  Memo := InnerForm.Memo;


  if (Memo <> nil) then
  begin
    if Memo.Modified then
      (InnerForm.Parent as TTabSheet).Caption := '*' + GetActiveInnerForm.Caption
    else
      (InnerForm.Parent as TTabSheet).Caption := GetActiveInnerForm.Caption;
    UpdateStatusBar(StatusBar1, Memo);
    UndoAction.Enabled := Memo.CanUndo;
    RedoAction.Enabled := Memo.CanRedo;
    CutAction.Enabled := Memo.SelAvail;
    CopyAction.Enabled := Memo.SelAvail;
    PasteAction.Enabled := Memo.CanPaste;
    FormatSeqSubmenu.Enabled := Memo.SelAvail;
    ReverseCompItem.Enabled := Memo.SelAvail;
  end;
end;

function TEditorForm.GetActiveMemo : TSynMemo;
begin
  Result := GetActiveInnerForm.Memo;
end;


//=============
//=============  ACTIONS ARE HERE
//=============

function TEditorForm.CreateNewPage: TTabSheet;
var
  InnerForm: TInnerForm;
begin
  Result := TTabSheet.Create(PageControl1);
  Result.PageControl := PageControl1;
  InnerForm := TInnerForm.Create(Result);
  InnerForm.FilesDropNotify := FileDropNotify;
  InnerForm.Parent := Result;
  InnerForm.Align := alClient;
  InnerForm.Visible := TRUE;
  InnerForm.Memo.OnStatusChange := MemoStatusChange;
  InnerForm.Memo.OnChange := MemoStateChange;
  InnerForm.Memo.Lines.Clear;
  InnerForm.Caption := 'Untitled';
  Result.Caption := 'Untitled';
  InnerForm.Memo.Modified := True;
  PageControl1.ActivePage := Result;
  InnerForm.SetFocus;
end;


procedure TEditorForm.NewActionExecute(Sender: TObject);
begin
  CreateNewPage;
end;

procedure TEditorForm.OpenActionExecute(Sender: TObject);
var
  Sheet: TTabSheet;
  InnerForm: TInnerForm;
begin
  if not DirectoryExists(OpenDialog1.InitialDir) then
    OpenDialog1.InitialDir := GetHomeDirectory;
  if OpenDialog1.Execute then
  begin
    OpenDialog1.InitialDir := ExtractFileDir(OpenDialog1.Filename);
    Sheet := CreateNewPage; // create a new tab for the data
    InnerForm := SheetToForm(sheet);
    try
      InnerForm.OpenFile(OpenDialog1.FileName);
      InnerForm.FileName := OpenDialog1.FileName;
      //FileReopenMRUList.Add(OpenDialog1.FileName);
    Except on E:Exception do
      begin
        if E.ClassType = EOutOfMemory then
          MessageDlg('The file you are trying to open is too large for MEGAs text editor to handle.', mtError, [mbOK], 0)
        else
          MessageDlg('There was a problem opening the text file.'+LineEnding+''+LineEnding+'Details: ', mtError, [mbOK], 0);
      end;
    end;
    Sheet.Caption := ExtractFileName(InnerForm.FileName);
  end;
end;

function TEditorForm.GetActiveInnerForm: TInnerForm;
var
  Sheet: TTabSheet;
begin
  Result := nil;
  if PageControl1.PageCount = 0 then
    Exit;
  Sheet := PageControl1.ActivePage;
  Result := SheetToForm(Sheet);
end;


procedure TEditorForm.SaveActionExecute(Sender: TObject);
var
  InnerForm: TInnerForm;
  Sheet: TTabSheet;
  SaveLocation, suggestedName: String;
begin
  InnerForm := GetActiveInnerForm;
  Sheet := TTabSheet(InnerForm.Parent);
  if InnerForm.FileName = EmptyStr then
  begin
    try
      // Use the suggested name as the default filename (otherwise it's balank).
      suggestedName := TTabSheet(InnerForm.Parent).Caption;
      if pos('*', suggestedName) = 1 then
        delete(suggestedName, 1, 1);
      SaveLocation := OutputUtils.AskUserForFileSaveLocation(suggestedName);
      if SaveLocation = EmptyStr then
        Exit;
      InnerForm.SaveFile(SaveLocation);
      InnerForm.FileName := SaveLocation;
      Sheet.Caption := ExtractFileName(SaveLocation);
    Except on E: Exception do
      MessageDlg('There was a problem saving the text file.  We suggest that you try saving to a different location, using Save As.'+LineEnding+''+LineEnding+'Details: ', mtError, [mbOK], 0);
    end;
  end
  else
  begin
    InnerForm.SaveFile(InnerForm.FileName);
    Sheet.Caption := InnerForm.Caption;
  end;
end;

procedure TEditorForm.SaveAsActionExecute(Sender: TObject);
var
  InnerForm: TInnerForm;
  SaveLocation: String;
  Sheet: TTabSheet;
begin
  InnerForm := GetActiveInnerForm;
  SaveLocation := OutputUtils.AskUserForFileSaveLocation;
  try
    if SaveLocation = EmptyStr then
      Exit;
    InnerForm.SaveFile(SaveLocation);
    InnerForm.FileName := SaveLocation;
    Sheet := TTabSheet(InnerForm.Parent);
    Sheet.Caption := ExtractFileName(SaveLocation);
  Except on E: Exception do
    MessageDlg('There was a problem saving the text file.  We suggest that you try saving to a different location, using Save As.'+LineEnding+''+LineEnding+'Details: ', mtError, [mbOK], 0);
  end;
end;


procedure TEditorForm.PrintActionExecute(Sender: TObject);
var
  InnerForm: TInnerForm;
begin
  {$IFDEF DARWIN}
    ShowMessage('Printing not yet implemented for macOS. Please use copy/paste to print from a document.');
	Exit;
  {$ENDIF}
  InnerForm := GetActiveInnerForm;
  InnerForm.PrintFile;
end;

procedure TEditorForm.ExitActionExecute(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to PageControl1.PageCount-1 do
    CloseSheet;
  Close;
end;

procedure TEditorForm.CutActionExecute(Sender: TObject);
var
  InnerForm: TInnerForm;
begin
  InnerForm := GetActiveInnerForm;
  InnerForm.Memo.CutToClipboard;
end;

procedure TEditorForm.CopyActionExecute(Sender: TObject);
var
  InnerForm: TInnerForm;
begin
  InnerForm := GetActiveInnerForm;
  if InnerForm <> nil then
    InnerForm.Memo.CopyToClipboard;
end;

procedure TEditorForm.PasteActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo <> nil then
    Memo.PasteFromClipboard;
end;

procedure TEditorForm.SetFontActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo <> nil then
  begin
    FontDialog1.Font := Memo.Font;
    if FontDialog1.Execute then
      Memo.Font := TFont(FontDialog1.Font);
  end;
end;

procedure TEditorForm.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
var
  Options: TSynSearchOptions;
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;

  if AReplace then
    Options := [ssoPrompt, ssoReplace]
  else
    Options := [];

  if ABackwards then
    Include(Options, ssoBackwards);

  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);

  if fSearchFromStart then
    Include(Options, ssoEntireScope);

  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);

  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);

  if gbReplaceAll then
    Include(Options, ssoReplaceAll);


  try
    ShowMessage(gsSearchText);
    if Memo.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
    begin
      Beep;
      Statusbar1.Panels[2].Text := STextNotFound;
      if ssoBackwards in Options then
        Memo.BlockEnd := Memo.BlockBegin
      else
        Memo.BlockBegin := Memo.BlockEnd;
      Memo.CaretXY := Memo.BlockBegin;
    end;
  Except on E:Exception do
     StatusBar1.Panels[2].Text := 'Search failed with unknown error.';
  end;


  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TEditorForm.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TTextSearchDialog;
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;

  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  dlg.Memo := Memo;
  with dlg do
  try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromStart := gbSearchFromStart;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then
    begin
      // if something is selected search for that text
      if Memo.SelAvail and (Memo.BlockBegin.Y = Memo.BlockEnd.Y)
      then
        SearchText := Memo.SelText
      else
        SearchText := Memo.GetWordAtRowCol(Memo.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do
    begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
      ReplaceAll := gbReplaceAll;
    end;
    SearchWholeWords := gbSearchWholeWords;
    Show;
  Except on E:Exception do
    if dlg.visible then
      dlg.Close;
  end;
end;


procedure TEditorForm.FindActionExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(FALSE);
end;


procedure TEditorForm.ReplaceActionExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(True);
end;

procedure TEditorForm.GoToLineActionExecute(Sender: TObject);
var
  StringValue: String;
  IntValue: Integer;
  MaxValue: Integer;
  Memo: TSynMemo;
  InnerForm: TInnerForm;
begin
  InnerForm := GetActiveInnerForm;
  if Assigned(InnerForm) then
  begin
    Memo := InnerForm.Memo;
    if not Assigned(Memo) then
      Exit;
    MaxValue := Memo.Lines.Count;
    StringValue := InputBox('Go To Line', 'Please enter a new line number', '1');
    StringValue := trim(StringValue);
    if StringValue = EmptyStr then
      Exit;  // do nothing since the user didn't give us a value

    try
      IntValue := StrToInt(StringValue);
      if IntValue > MaxValue then
        Memo.CaretY := MaxValue
      else if IntValue < 1 then
        Memo.CaretY := 1
      else
        Memo.CaretY := IntValue;
    Except
      on E:Exception do
        ShowMessage('Invalid value: ' + StringValue);
    end;
  end;
end;


procedure TEditorForm.UndoActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo <> nil then
  begin
    if Memo.CanUndo then
      Memo.Undo;
    UndoAction.Enabled := Memo.CanUndo;
  end;
end;

procedure TEditorForm.RedoActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo <> nil then
  begin
    if Memo.CanRedo then
      Memo.Redo;
    RedoAction.Enabled := Memo.CanRedo;
  end;
end;

procedure TEditorForm.CloseBtnClick(Sender: TObject);
begin
  CloseFileActionExecute(CloseBtn);
end;

procedure TEditorForm.CloseFileActionExecute(Sender: TObject);
begin
  CloseSheet;
  if not FFormIsClosing then
    UpdateActionList;
end;

procedure TEditorForm.CloseSheet;
var
  InnerForm: TInnerForm;
  DoSave: Integer;
  NewFileName: String;
begin
  InnerForm := GetActiveInnerForm;
  if not Assigned(InnerForm) then
    Exit;
  if InnerForm.Memo.Modified then
  begin
    DoSave := MessageDlg('Save changes to ''' + InnerForm.Caption + '''?',mtConfirmation, mbYesNoCancel, 0);
    if DoSave = mrCancel then
      Exit
    else if DoSave = mrNo then
    begin
      InnerForm.SynMemo1.Modified := False;
    end
    else
    begin
      if not FileExists(InnerForm.FileName) then
      begin
        NewFileName := AskUserForFileSaveLocation;
        if NewFileName = EmptyStr then
          Exit;
        InnerForm.FileName := NewFileName;
      end;
    end;
  end;

  InnerForm.CloseFile;
  InnerForm.Close;
  PageControl1.ActivePage.Free;
end;

//======
procedure TEditorForm.ConvertActionExecute(Sender: TObject);
var
  i : integer;
  InnerForm: TInnerForm;
  TempInnerForm: TInnerForm;
  FileName : string;
  outstrlist : TStrings;
  in_type : TImportFormatType;
  AConvertOptDlg : TFormatConvertOptDlg;
  AConverter     : TFormatConvertToMega;
  ASaveDlg       : TSaveDialog;
  Response: Integer;
begin
  AConvertOptDlg := nil;
  AConverter := nil;
  InnerForm := nil;

  FileName := EmptyStr;
  if (PageControl1.PageCount > 0) then
    FileName := GetActiveInnerForm.FileName;

  try
    AConvertOptDlg := TFormatConvertOptDlg.Create(Self);
    with AConvertOptDlg do
    begin
      // re-initialize the SrcFile_cbo combo box
      // If we get back something that isn't in this list, then we'll assume
      // it's another file we haven't opened yet, so we need to open it
      // if it exists.
      if Length(FileName) > 0 then ImportFileName := FileName;
      if ShowModal <> mrOK then
        Exit;
      FileName      := ImportFileName;
      in_type := ImportFileType;
    end;

    if in_Type = gm_None then
    begin
      ShowMessage( 'Unknown input file type to import.' );
      Exit;
    end;

    // now we check if this file is already open
    for i := 0 to PageControl1.PageCount - 1 do
    begin
      TempInnerForm := SheetToForm(PageControl1.Pages[i]);
      if FileName = TempInnerForm.FileName then
      begin
        InnerForm := TempInnerForm;
        Break;
      end;
    end;

    if InnerForm = nil then // no opened file found;so we've got a new file here; open it.
    begin
      if FileExists(FileName) then
      begin
        OpenFileAndFocus(FileName, 0, 0);
        InnerForm := GetActiveInnerForm;
      end
      else
      begin
        ShowMessage( 'File [' + FileName + '] doesn''t exist!' );
        Exit;
      end;
    end;

    //Time to start converting
    AConverter := TFormatConvertToMega.Create(InnerForm.Memo.Lines, in_type);
    with AConverter do
    begin
      InFile := FileName;
      OutStrList := AConverter.Convert;
      if (Outstrlist <> nil) then
      begin
        ASaveDlg := TSaveDialog.Create(nil);
        ASaveDlg.FileName := ChangeFileExt(ExtractFileName(FileName), '.meg');
        try
        ASaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ASaveDlg.InitialDir);
        if ASaveDlg.Execute then
        begin
          if FileExists(ASaveDlg.FileName) then
            begin
              Response := MessageDlg('Overwrite File?', 'The specified file already exists. Do you want to overwrite it?', mtConfirmation, mbYesNo, 0);
              if Response = mrYes then
                begin
                  OutStrList.SaveToFile(ASaveDlg.FileName);
                  OpenFileAndFocus(ASaveDlg.FileName, 0, 0);
                end;
            end
          else
            begin
              OutStrList.SaveToFile(ASaveDlg.FileName);
              OpenFileAndFocus(ASaveDlg.FileName, 0, 0);
            end;
        end
        else
        begin
          InnerForm := SheetToForm(CreateNewPage);
          InnerForm.Memo.Lines.Assign(OutStrList);
        end;
        finally
          ASaveDlg.Free;
        end;

        ShowMessage('File conversion complete.  Please note that a data file may not be '+
                      'in the exact format that MEGA expects for importing. '+
                      'Therefore, you must check the converted results carefully on the '+
                      'screen before proceeding further.');
      end
      else
        // Need to get the line number where the error occured and move cursor if possible - Joel
        if AConverter.CurLine > 0 then
          ShowMessage('Error: Conversion failed at line (' + IntToStr(AConverter.CurLine) + ')')
        else
    	  ShowMessage( 'Sorry.  The file conversion failed!' );
    end;
  finally
    if AConvertOptDlg <> nil then
      AConvertOptDlg.Free;
    if AConverter <> nil then
      AConverter.Free;
    InnerForm := nil;
    TempInnerForm := nil;
  end;
end;

procedure TEditorForm.UpdateActionList;
var
  HasFiles: Boolean;
  Memo: TSynMemo;
  InnerForm: TInnerForm;
begin
  if FFormIsClosing then
    Exit;

  if MilliSecondsBetween(Now, FLastUpdateTime) < ACTION_LIST_UPDATE_FREQUENCY then
    Exit;
  FLastUpdateTime := Now;
  try
  HasFiles := PageControl1.PageCount > 0;
  PageControl1.Visible := HasFiles;
  InnerForm := GetActiveInnerForm;
  if InnerForm = nil then
  begin
    // Edit Actions
    CopyAction.Enabled            := False;
    CutAction.Enabled             := False;
    PasteAction.Enabled           := False;
    UndoAction.Enabled            := False;
    RedoAction.Enabled            := False;
    DeleteBlockAction.Enabled     := False;
    SelectAllAction.Enabled       := False;

    SaveAction.Enabled            := False;
    SaveAsAction.Enabled          := False;
    PrintAction.Enabled           := False;
    CloseFileAction.Enabled       := False;

    FindAction.Enabled            := False;
    ReplaceAction.Enabled         := False;
    GoToLineAction.Enabled        := False;

    FormatSeqSubmenu.Enabled      := False;
    ReverseCompItem.Enabled       := False;
    ShowLineNumAction.Enabled     := False;
    WordWrapAction.Enabled        := False;
    SetFontAction.Enabled         := False;
    CloseBtn.Enabled              := False;
    Exit;
  end;
  Memo := InnerForm.Memo;

  // Display Actions
  SetFontAction.Enabled     := HasFiles;
  ShowLineNumAction.Enabled := HasFiles;

  DisplayShowLineNumItem.Checked := Memo.Gutter.Visible;
  WordWrapAction.Enabled    := False; {HasFiles; disabled because synedit does not yet support it}
  //DisplayWordWrapItem.Checked := Memo.WordWrap;

  // Edit Actions
  CopyAction.Enabled            := Memo.SelAvail;
  CutAction.Enabled             := Memo.SelAvail;
  PasteAction.Enabled           := Memo.CanPaste;
  UndoAction.Enabled            := Memo.CanUndo;
  RedoAction.Enabled            := Memo.CanRedo;
  DeleteBlockAction.Enabled     := Memo.SelAvail;
  SelectAllAction.Enabled       := True;

  SaveAction.Enabled            := Memo.Modified;
  SaveAsAction.Enabled          := True;
  PrintAction.Enabled           := HasFiles;
  CloseFileAction.Enabled       := HasFiles;
  CloseBtn.Enabled              := HasFiles;

  FindAction.Enabled            := HasFiles;
  ReplaceAction.Enabled         := HasFiles;
  GoToLineAction.Enabled        := HasFiles;

//  CopyScreenShotSubmenu.Visible := HasFiles;
  FormatSeqSubmenu.Enabled      := Memo.SelAvail;
  ReverseCompItem.Enabled       := Memo.SelAvail;
  except
    on E:Exception do
      ShowMessage('An error occurred while updating the Text Editor action list: ' + E.Message);
  end;
end;

procedure TEditorForm.ActionList1Update(aAction: TBasicAction; var Handled: Boolean);
begin
  Handled := True;
  if FFormIsClosing then
    Exit;
  try
    UpdateActionList;
  except
    // A bad way to handle this but on Linux this is firing after we begin closing the form and SynEdit is getting an access violation
  end;
end;

function TEditorForm.SheetToForm(sheet: TTabSheet): TInnerForm;
begin
  Result := nil;
  if (not Assigned(sheet)) or (sheet.ControlCount = 0) then Exit;
  if sheet.Controls[0].ClassType = TInnerForm then
      result := TInnerForm(sheet.Controls[0])
  else
    raise Exception.Create('Internal error with the MEGA Text Editor. Please report this to the software authors.');
end;

procedure TEditorForm.ShowLineNumActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  with DisplayShowLineNumItem do
    Checked := not Checked;
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;

  with Memo do
  begin
    if DisplayShowLineNumItem.Checked then
    begin
      Gutter.Visible := True;
      //Gutter.Width := 34;
    end
    else
    begin
      Gutter.Visible := False;
      //Gutter.Width := 10;
    end;
  end;
end;

procedure TEditorForm.SelectAllActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;

  Memo.SelectAll;
end;

procedure TEditorForm.DeleteBlockActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  Memo.ClearSelection;
end;

procedure TEditorForm.TabControl1Change(Sender: TObject);
begin
  // not sure why this is here
end;

procedure TEditorForm.FileDropNotify(aFiles: TStrings);
var
  i: Integer;
  Sheet: TTabSheet;
  InnerForm: TInnerForm;
begin
  if aFiles.Count = 0 then
    Exit;
  try
    for i := 0 to aFiles.Count - 1 do
      if FileExists(aFiles[i]) then
      begin
        Sheet := CreateNewPage; // create a new tab for the data
        InnerForm := SheetToForm(Sheet);
        try
          InnerForm.OpenFile(aFiles[i]);
          InnerForm.FileName := aFiles[i];
          //FileReopenMRUList.Add(aFiles[i]);
        Except on E:Exception do
          begin
            if E.ClassType = EOutOfMemory then
              MessageDlg('The file you are trying to open is too large for MEGAs text editor to handle.', mtError, [mbOK], 0)
            else
              MessageDlg('There was a problem opening the text file.'+LineEnding+''+LineEnding+'Details: ', mtError, [mbOK], 0);
          end;
        end;
        Sheet.Caption := ExtractFileName(InnerForm.FileName);
      end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TEditorForm.InitMainMenu;
begin
  {$IFDEF VISUAL_BUILD}
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem
  {$ENDIF}
  {$ENDIF}
end;

procedure TEditorForm.AssignContextSensitiveHelp;
begin
  HelpContext := HC_Text_Editor;

  // File menu
  FileNewItem.HelpContext := HC_TE_New;
  FileOpenItem.HelpContext := HC_TE_Open;
  FileSaveItem.HelpContext := HC_TE_Save;
  FileSaveAsItem.HelpContext := HC_TE_Save_As;
  FileCloseItem.HelpContext  := HC_Close_File_in_Text_Editor;
  FileConvertItem.HelpContext := HC_Convert_to_Mega_Format_in_Text_Editor;
  FileExit.HelpContext := HC_TE_Exit;
  FilePrintItem.HelpContext := HC_TE_Print;

  // EditMenu
  EditCutItem.HelpContext := HC_TE_Cut;
  EditCopyItem.HelpContext  := HC_TE_Copy;
  EditPasteItem.HelpContext := HC_TE_Paste;
  EditUndoItem.HelpContext := HC_TE_Undo;
  FileDeleteItem.HelpContext :=  HC_Delete_in_Text_Editor;
  EditSelectAllItem.HelpContext :=  HC_Select_All_in_Text_Editor;

  //SearchMenu
  SearchFindItem.HelpContext :=  HC_TE_Find;
  SearchRelaceItem.HelpContext := HC_TE_Replace;
  SearchGotoLineItem.HelpContext :=  HC_Go_to_Line;

  // DisplayMenu: TMenuItem;
  DisplaySetFontItem.HelpContext := HC_TE_Font;
  DisplayShowLineNumItem.HelpContext :=  HC_Show_Line_Numbers_in_Text_Editor;
  DisplayWordWrapItem.HelpContext :=  HC_Word_Wrap_in_Text_Editor;
  CopyBitmaptoClipboardItem.HelpContext := HC_Copy_Screenshot_to_Clipboard;
  CopyWinMetafiletoClipboardItem.HelpContext := HC_Copy_Screenshot_to_Clipboard;
  CopyEMFtoClipboardItem.HelpContext := HC_Copy_Screenshot_to_Clipboard;
  CopyScreenShotSubmenu.HelpContext := HC_Copy_Screenshot_to_Clipboard;

  // UtilitiesMenu: TMenuItem;
  MergeLinesItem.HelpContext := HC_Format_Selected_Sequence;
  CleanSequenceItem.HelpContext := HC_Format_Selected_Sequence;
  Formatby3Item.HelpContext := HC_Format_Selected_Sequence;
  Formatby10Item.HelpContext := HC_Format_Selected_Sequence;
  FormatSeqSubmenu.HelpContext :=  HC_Format_Selected_Sequence;
  ReverseCompItem.HelpContext := HC_Reverse_Complement;
end;

//procedure TEditorForm.FileReopenMRUListClick(Sender: TObject; const ItemText: String; var Action: TOvcMRUClickAction);
//var
//  Sheet: TTabSheet;
//  InnerForm: TInnerForm;
//  i: Integer;
//begin
//
//  for i := 0 to PageControl1.PageCount - 1 do
//  begin
//    InnerForm := sheetToForm(PageControl1.Pages[i]);
//    if InnerForm.FileName = ItemText then
//    begin
//      PageControl1.ActivePageIndex := i;
//      Exit;
//    end;
//  end;
//
//  if FileExists(ItemText) then
//  begin
//    Sheet := CreateNewPage; // create a new tab for the data
//    InnerForm := SheetToForm(Sheet);
//    try
//      InnerForm.OpenFile(ItemText);
//      InnerForm.FileName := ItemText;
//    Except on E:Exception do
//      begin
//        if E.ClassType = EOutOfMemory then
//          MessageDlg('The file you are trying to open is too large for MEGAs text editor to handle.', mtError, [mbOK], 0)
//        else
//          MessageDlg('There was a problem opening the text file.'+LineEnding+''+LineEnding+'Details: ', mtError, [mbOK], 0);
//      end;
//    end;
//    Sheet.Caption := ExtractFileName(ItemText);
//  end
//  else
//    FileReopenMRUList.Remove(ItemText);
//end;

procedure TEditorForm.FormActivate(Sender: TObject);
begin
  if not Assigned(EditorForm) then
    Exit;
  AddWindowToTray(Self);
  if (EditorForm.PageControl1.PageCount = 0) and (OpenWithNewFile) then
    EditorForm.NewActionExecute(nil);
end;

procedure TEditorForm.CopyWMFToClipActionExecute(Sender: TObject);
begin
  if PageControl1.Visible = False then Exit;
  //SynEdit1.CopyGraphicToClipboard(gfBitmap);
end;

procedure TEditorForm.CopyBMPToClipActionExecute(Sender: TObject);
begin
  if PageControl1.Visible = False then Exit;
  //SynEdit1.CopyGraphicToClipboard(gfMetaFile);
end;

procedure TEditorForm.CopyEMFToClipActionExecute(Sender: TObject);
begin
  if PageControl1.Visible = False then Exit;
  //SynEdit1.CopyGraphicToClipboard(gfEnhancedMetaFile);
end;

procedure TEditorForm.WordWrapActionExecute(Sender: TObject);
var
  Memo: TSynMemo;
begin
  if not PageControl1.Visible then Exit;
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  ShowMessage('Sorry, word wrap is not implemented as the current version of SynEdit does not support it');
  Exit;
  DisplayWordWrapItem.Checked := not DisplayWordWrapItem.Checked;
  //Memo.WordWrap := DisplayWordWrapItem.Checked;
end;

function DoTextOperation(AStr: AnsiString; Options: TTeOptions): AnsiString;
var
  ch : AnsiChar;
  i, j, Offset, len : integer;
begin
  i := 1;
  j := 1;
  AStr := Trim(AStr);
  len := Length(AStr);
  Result := EmptyStr;
  if len = 0 then
    Exit;
  SetLength(Result, len+1); // just to make sure that there are no problems
  repeat
    ch := AStr[j];
    if teMergeLines in Options then
    begin
      if not ((ch = #10) or (ch = #13)) then
      begin
        Result[i] := ch;
        Inc(i);
      end;
    end
    else if (teRemoveSpaces in Options) or
            (teInsertSpacesBy3 in Options) or
            (teInsertSpacesby10 in Options) then
    begin
      if not ((ch in ['0'..'9',',' ,' ']) or (ch = '\t'))then
      begin
        Result[i] := ch;
        Inc(i);
      end;
    end
    else if teReverseComplement in Options then
    begin
      if ch in ['A','a','T','t','C','c','G','g','U','u'] then
        case ch of
          'A': ch := 'T';  'a': ch := 't';
          'T': ch := 'A';  't': ch := 'a';
          'C': ch := 'G';  'c': ch := 'g';
          'G': ch := 'C';  'g': ch := 'c';
          'U': ch := 'A';  //'a': ch := 't';
        end;
      Result[len-j+1] := ch;
      Inc(i);
    end;
    Inc(j);
  until (j > len);

  // reverse #10#13
  if teReverseComplement in Options then
  begin
    for j:= 1 to length(Result)-1 do
    begin
      if (Result[j] = #10) and (Result[j+1] = #13) then
      begin
        Result[j]   := #13;
        Result[j+1] := #10;
      end;
    end;
  end;

  AStr := Copy(Result, 1, i-1);
  if not ((teInsertSpacesBy3 in Options) or (teInsertSpacesby10 in Options)) then
  begin
    Result := AStr;
    Exit;
  end;
  // otherwise fall here

  AStr := Trim(AStr);
  len := Length(AStr);
  Result := EmptyStr;
  if len = 0 then
    Exit;
  SetLength(Result, 2*len); // just to make sure that there are no problems

  i := 1;
  j := 1;
  OffSet := 1;
  repeat
    ch := AStr[j];
    Result[i] := ch;
    Inc(i);
    if not (ch in [#10,#13]) then
    begin
      if (teInsertSpacesBy3 in Options) then
      begin
        if (Offset mod 3) = 0 then
        begin
          Result[i] := ' ';
          Inc(i);
        end
      end
      else if (teInsertSpacesby10 in Options) then
      begin
        if (Offset mod 10) = 0 then
        begin
          Result[i] := ' ';
          Inc(i);
        end
      end;
      Inc(Offset);
    end;
    Inc(j);
  until (j > len);
  AStr := Copy(Result, 1, i-1);
  Result := AStr;
end;

procedure TEditorForm.MergeLinesItemClick(Sender: TObject);
var
  Memo: TSynMemo;
  MergedText: String;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  MergedText := DoTextOperation(Memo.SelText, [teMergeLines]);
  Memo.ClearSelection;
  Memo.SelText := MergedText;
end;

procedure TEditorForm.CleanSequenceItemClick(Sender: TObject);
var
  Memo: TSynMemo;
  MergedText: String;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  MergedText := DoTextOperation(Memo.SelText, [teRemoveSpaces]);
  Memo.ClearSelection;
  Memo.SelText := MergedText;
end;

procedure TEditorForm.Formatby3ItemClick(Sender: TObject);
var
  Memo: TSynMemo;
  MergedText: String;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  MergedText := DoTextOperation(Memo.SelText, [teRemoveSpaces, teInsertSpacesBy3]);
  Memo.ClearSelection;
  Memo.SelText := MergedText;
end;

procedure TEditorForm.Formatby10ItemClick(Sender: TObject);
var
  Memo: TSynMemo;
  MergedText: String;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  MergedText := DoTextOperation(Memo.SelText, [teRemoveSpaces, teInsertSpacesBy10]);
  Memo.ClearSelection;
  Memo.SelText := MergedText;
end;

procedure TEditorForm.ReverseCompItemClick(Sender: TObject);
var
  Memo: TSynMemo;
  MergedText: String;
begin
  Memo := GetActiveInnerForm.Memo;
  if Memo = nil then
    Exit;
  MergedText := DoTextOperation(Memo.SelText, [teReverseComplement]);
  Memo.ClearSelection;
  Memo.SelText := MergedText;
end;

procedure TEditorForm.UnExpectedSearchException(Sender : TObject ; const Error : AnsiString);
begin
  ShowMessage('There was an error conducting your search.  If you have "Selected Text" chosen as the scope make sure there something is selected.');
end;
{$ENDIF}

end.
