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

unit uMegaBrowser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

//{$I cef.inc}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF UNIX}LMessages, {$ENDIF}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Menus, SyncObjs,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls,
  ActnList, Buttons, attabs, uCEFChromium, uCEFInterfaces,
  uCEFTypes, uCEFConstants, uCEFWinControl, uCEFChromiumCore,
  MegaConsts, MBrowserImportController, uMegaBrowserTab, mbrowserutils,
  MMatrixExport,MegaUtils, MD_Sequences, umegarenderer, htmloptionsdlg, mimageform,
  uMegaBrowserPanel;

const
  DEFAULT_TAB_CAPTION                      = 'New Tab';
  HOMEPAGE_URL                             = 'http://www.ncbi.nlm.nih.gov';

type
  TBrowserMode = (bmBrowser, bmCaption, bmResults, bmHelpBrowser, bmDefaultBrowser, bmCitation, bmFileViewer);
  TDTVisitStatus = (dvsIdle, dvsGettingDocNodeID, dvsQueryingSelector, dvsSettingAttributeValue);

  { TMegaBrowserFrm }
  TMegaBrowserFrm = class(TForm)
    actCopy: TAction;
    actExportExcel: TAction;
    actExportCSV: TAction;
    actCopyToClipboard: TAction;
    actCloseTab: TAction;
    actDisplayHelpContents: TAction;
    actCut: TAction;
    actFindText: TAction;
    actDevToolsExecute: TAction;
    actExit: TAction;
    MenuItem2: TMenuItem;
    SaveImageToFileAction: TAction;
    ActionAnalysisSummary: TAction;
    actTabRight: TAction;
    actTabLeft: TAction;
    BrowserInfoAction: TAction;
    actLoadHTML: TAction;
    actSaveToTextFile: TAction;
    AddToAlignment: TAction;
    actMegaWebsite: TAction;
    actNewWindow: TAction;
    actNewTab: TAction;
    actPrint: TAction;
    actSelectAll: TAction;
    actPaste: TAction;
    actZoomReset: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    CaptionImage: TImage;
    DeveloperAction: TAction;
    actStop: TAction;
    actNext: TAction;
    actPrev: TAction;
    actReload: TAction;
    ActionList1: TActionList;
    edAddress: TEdit;
    FindDialog1: TFindDialog;
    ImageList1: TImageList;
    ImageList2: TImageList;
    MainMenu1: TMainMenu;
    Flog: TMemo;
    MegaBrowserPanel1: TMegaBrowserPanel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    ImagePanel: TPanel;
    ImageScrollBox: TScrollBox;
    ImageSplitter: TSplitter;
    WindowsMenuItem: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    ReloadMenu: TMenuItem;
    ContentsMenu: TMenuItem;
    MegaWebsiteMenu: TMenuItem;
    ExportJsonMenu: TMenuItem;
    LoadHTMLMenu: TMenuItem;
    AddJSONtoHTMLMenu: TMenuItem;
    NewTabMenu: TMenuItem;
    NewWindowMenu: TMenuItem;
    CloseTabMenu: TMenuItem;
    AddToAlignmentMenu: TMenuItem;
    CutMenu: TMenuItem;
    CopyMenu: TMenuItem;
    PasteMenu: TMenuItem;
    SaveDialog: TSaveDialog;
    SelectAllMenu: TMenuItem;
    SpeedButton1: TSpeedButton;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToggleToolBar: TMenuItem;
    StatusBar1: TMenuItem;
    N4: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    AddToAlignmentSpacer: TToolButton;
    ExportToolsSpacer: TToolButton;
    ToolButton9: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    printBtn: TToolButton;
    ZoomInMenu: TMenuItem;
    ZoomOutMenu: TMenuItem;
    ZoomResetMenu: TMenuItem;
    N3: TMenuItem;
    FindTextMenu: TMenuItem;
    ExportOptionsSpacer: TMenuItem;
    ExitMenu: TMenuItem;
    TabToolsItemSpacer: TMenuItem;
    EditMenuItem: TMenuItem;
    MenuItem3: TMenuItem;
    NavigateMenu: TMenuItem;
    HelpItem: TMenuItem;
    DeveloperMenu: TMenuItem;
    ForwardMenu: TMenuItem;
    BackMenu: TMenuItem;
    StopLoadMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;

    procedure ActionAnalysisSummaryExecute(Sender: TObject);
    procedure actTabLeftExecute(Sender: TObject);
    procedure actTabRightExecute(Sender: TObject);
    procedure BrowserInfoActionExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SaveImageToFileActionExecute(Sender: TObject);
    procedure TabsTabChanged(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean); 
    procedure FormDestroy(Sender: TObject);

    procedure actCloseTabExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDevToolsExecuteExecute(Sender: TObject);
    procedure actDisplayHelpContentsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportCsvExecute(Sender: TObject);
    procedure actExportExcelExecute(Sender: TObject);
    procedure actFindTextExecute(Sender: TObject);
    procedure actMegaWebsiteExecute(Sender: TObject);
    procedure actNewTabExecute(Sender: TObject);
    procedure actNewWindowExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actNextUpdate(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actPrevUpdate(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actReloadExecute(Sender: TObject);
    procedure actReloadUpdate(Sender: TObject);
    procedure actSaveToTextFileExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actZoomResetExecute(Sender: TObject);
    procedure AddToAlignmentExecute(Sender: TObject);
    procedure AddToAlignmentUpdate(Sender: TObject);
    procedure actLoadHTMLExecute(Sender: TObject);
    {$IFDEF VISUAL_BUILD}
    procedure crmBeforeDownload(Sender: TObject; const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure DeveloperActionExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure ProcessJSMessage(const Browser: ICefBrowser;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure ShowJSMessageDialog(jsMessage: String);
    procedure crmProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    {$ENDIF}
    procedure edAddressClick(Sender: TObject);
    procedure edAddressKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure CopyLinkToClipboard1Click(Sender: TObject);

    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;

    procedure MainMenu1DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure MainMenu1MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure Search1Click(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure ToggleToolbarClick(Sender: TObject);
    procedure OpenLinkInNewTab1Click(Sender: TObject);
    procedure OpenLinkInNewWindow1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    {$IFDEF UNIX}

    {$ENDIF}
    procedure SendCompMessage(aMsg : cardinal; aData : PtrInt = 0);

  private
    FCaptionTitle: String;
    FAnalysisSummary: TStringList;
    FButtonHint: String;
    FDestinationUrl: String;
    FBlastBaseUrl: String;
    FPendingUrl : String;
    {$IFDEF VISUAL_BUILD}
    FImportController : TBrowserImportController;
    FMatrixExport: TMatrixExport;
    {$ENDIF}
    FBrowserMode: TBrowserMode;
    FLoading: Boolean;
    FHtml: String;
    FLinkUrl: String;
    timeframe : integer;
    UserDataUpdated: boolean;
    blastSeqsBtnClicked: boolean;
    {$IFDEF VISUAL_BUILD}
    procedure InitBrowserSettings(var Settings: TCefBrowserSettings);
    procedure InitWindowInfo(var info: TCefWindowInfo);
    {$ENDIF}
    procedure PromptForCDSsToUse;
    procedure PromptForSeqNameOptions(aSeq: TSequence);
    procedure ProcessCDSsOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessSeqNameOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure EnableLinkMenus(AreEnabled: Boolean);
    procedure LoadLinkData;
    procedure SetAnalysisSummary(AValue: TStringList);
    procedure UpdateLinkData;
    procedure UpdateUserLinkData;
    procedure SetShowCaptionTools(Value: Boolean);
    procedure SetShowDataExportTools(Value: Boolean);
    procedure SetShowNavigation(Value: Boolean);
    function FetchSeqsFromBlast: boolean;
    procedure InitialTab(aUrl: ustring = '');
    procedure HideBrowserSpecificTools;
    procedure SetWindowSize;
    { private declarations }
  public
    DoNotAddToWindowTray: Boolean;
    FileBrowserFilename: String;
    QUERYURL: string;
    BLASTnURL: string;
    BLASTpURL: string;
    BLASTSequence: string;
    OnCloseNotify: TNotifyEvent;
    {$IFDEF VISUAL_BUILD}
    procedure SetMatrixExport(MatrixExport: TMatrixExport); { setup for exporting some kind of results to excel or csv}
    {$ENDIF}
    procedure DisplayImage(imageFile: String);
    procedure SetTargetDimensions(aClientWidth: Integer; aClientHeight: Integer);
    procedure GoToUrl(Url: String; tabName: String = '');
    procedure QueryGene(isDNA : Boolean);
    procedure BLAST(sequence: string; isDNA: boolean);
    procedure ShowGene(gi: string;  isDNA: boolean);
    procedure LoadSequencesFromHtml(HtmlStrings: TStringList);
    procedure LoadHtmlFromString(Html: String);
    procedure RunInCaptionMode;
    procedure RunInExcelExportMode;
    procedure RunInWebBrowserMode;
    procedure RunInDefaultWebBrowserMode;
    procedure RunInHelpMode;
    procedure RunInFileViewerMode;
    procedure LoadPlaceholder(destinationUrl: String);
    function activeTab: TBrowserTab;
    function activeTabChromium: TChromium;
    function activeBrowser: ICefBrowser;
    property Chromium: TChromium read activeTabChromium;
    property BrowserMode: TBrowserMode read FBrowserMode write FBrowserMode;
    property ButtonHint: String read FButtonHint write FButtonHint;
    property AnalysisSummary: TStringList read FAnalysisSummary write SetAnalysisSummary;
    property CaptionTitle: String read FCaptionTitle write FCaptionTitle;
    procedure TitleChange(Sender: TObject; const title: ustring);
    procedure URLChange(Sender: TObject; const url: ustring);
    procedure StatusChange(Sender: TObject; const status: ustring);
    procedure ButtonStateChange(Sender: TObject);
    procedure NewTab(aUrl: ustring = '');// overload;
    procedure Log(aMsg: String);
    procedure HandleChromiumContextMenu(Sender: TObject; const commandId: PtrInt);
    procedure HandleChromiumKeypress(Sender: TObject; event: PCefKeyEvent; osEvent: TCefEventHandle);
    procedure CloseFile(fileUrl: String);
    function FileIsOpen(filename: String): Boolean;

  protected
    FLoadCount :  Int8;
    FClosingTab : boolean;
    FCanClose : boolean;
    FCritSection : TCriticalSection;
    FStatus      : TDTVisitStatus;
    FErrorText   : string;
    FActiveTab  : TBrowserTab;

    function  GetBrowserTabCount : integer;
    property  BrowserTabCount : integer    read GetBrowserTabCount;

    procedure NotifyMoveOrResizeStarted;

    procedure BrowserInitializedMsg(Data: PtrInt);
    procedure OnTabClose(Sender: TObject; const TabCount: integer);

    {$IFDEF UNIX}
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    {$ENDIF}

    function  CloseAllTabs : boolean;

    {$IFDEF WINDOWS}
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure WMQueryEndSession(var aMessage: TWMQueryEndSession); message WM_QUERYENDSESSION;
    procedure WMDpiChanged(var Message: TMessage); message WM_DPICHANGED;
    {$ENDIF}
    function  QuerySelector(aNodeID : integer; const aSelector : string) : integer;
    function  SetAttributeValue(aNodeID : integer; const aName, aValue : string) : integer;
  end;


function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; HideWindow: Boolean = False) : TMegaBrowserFrm; overload;
function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; Owner: TComponent; HideWindow: Boolean = False): TMegaBrowserFrm; overload;
function GetHelpBrowser(helpFile: String): TMegaBrowserFrm;
function ShowFileInBrowser(aFile: String; tabCaption: String): TMegaBrowserFrm;
procedure CloseBrowserFile(aFile: String);
function FileIsOpenInFileBrowser(filename: String): Boolean;

var
  FMain: TMegaBrowserFrm = nil;
  MegaBrowserFrm: TMegaBrowserFrm;
  HelpBrowser: TMegaBrowserFrm;
  FileBrowser: TMegaBrowserFrm;
  FillBlastFormCheck: boolean;
  tryNCBIDownload: boolean;
  visitFetchSeqsFromHTMLFinished: Boolean;

implementation

{$R *.lfm}

uses
    {$IFDEF VISUAL_BUILD}
    mega_main, MegaPrivateFiles, MPleaseWait,
    uCEFProcessMessage, uCEFMiscFunctions, uCEFDictionaryValue,
    MWriteOutputDlg, MEditorForm, MAlignEditMainForm,
    {$ENDIF}
    dateutils, Clipbrd, Printers, MegaVerConsts,
    gettext, Math,
    MMegaWindowInfo, AlnBuilder_HC, mhelpfiles, mhelpkeywords,  LCLProc,
    mshortcutshelper, StringUtils, uCEFApplicationCore;


procedure TMegaBrowserFrm.FormCreate(Sender: TObject);
begin
  FButtonHint := 'Web Browser';
  DoNotAddToWindowTray := False;
  ImagePanel.Visible := False;
  {$IFDEF DARWIN}
  MegaBrowserPanel1.Tabs.Font := Screen.SystemFont;
  edAddress.Font := Screen.SystemFont;
  SpeedButton1.Font := Screen.SystemFont;
  UpdateShortcutsForMacOs(ActionList1);
  {$ENDIF}
  FAnalysisSummary := nil;
  ActionAnalysisSummary.Visible := False;
  ActionAnalysisSummary.Enabled := False;
  FCaptionTitle := EmptyStr;
  FLoadCount := 0;
  FCanClose := False;
  FStatus := dvsIdle;
  FCritSection := TCriticalSection.Create;
  ImageForm.UpdateImgList(Self);
  FImportController := nil;
  MegaBrowserPanel1.OnTabClose := OnTabClose;
  {$IFNDEF DEBUG}
  DeveloperAction.Enabled := False;
  DeveloperAction.Visible := False;
  DeveloperMenu.Enabled := False;
  DeveloperMenu.Visible := False;
  {$ELSE}
  ShowInTaskBar := stAlways;
  {$ENDIF}
  SetWindowSize;
end;

function TMegaBrowserFrm.CloseAllTabs : boolean;
var
  i : integer;
begin
  Result := False;
  i      := pred(MegaBrowserPanel1.TabCount);

  while (i >= 0) do
    begin
      MegaBrowserPanel1.Tabs.DeleteTab(i, true, false);
      Result := True;
      dec(i);
    end;
  FCanClose := True;
end;

procedure TMegaBrowserFrm.actExitExecute(Sender: TObject);
begin
  FileBrowserFilename := EmptyStr;
  Self.Close;
end;

// this function is used only for the menu option that closes the active tab
procedure TMegaBrowserFrm.actCloseTabExecute(Sender: TObject);
begin
  MegaBrowserPanel1.CloseActiveTab;
end;

procedure TMegaBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // this code is from the TabbedBrowser2 demo
  CanClose := FCanClose;
  if (CloseAllTabs) then
    begin
      FCanClose := True;
      CanClose  := True;
      {$IFDEF WINDOWS}
      PostMessage(Handle, WM_CLOSE, 0, 0);
      {$ENDIF}
    end;
end;

procedure TMegaBrowserFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(OnCloseNotify) then
    OnCloseNotify(Self);

  if Self = FileBrowser then
    FileBrowser := nil
  else if Self = HelpBrowser then
    HelpBrowser := nil;

  case BrowserMode of
    bmCitation, bmResults, bmDefaultBrowser: CloseAction := caFree;
    bmBrowser, bmHelpBrowser, bmFileViewer: CloseAction := caFree;
    bmCaption: CloseAction := caHide;
  end;

  RemoveWindowFromTray(Self);

  if FMain = Self then
    FMain := nil;
end;

procedure TMegaBrowserFrm.HandleChromiumContextMenu(Sender: TObject; const commandId: PtrInt);
begin
  if (commandId <> 0) then
    begin;
      // there should be a more elegant way of passing the URL
      // but this gets the job done
      FLinkUrl := StatusBar.Panels[0].Text;
       case commandId of
        CLIENT_ID_TAB_COPY:       OpenLinkInNewTab1Click(Self);
        CLIENT_ID_WINDOW_COPY:    OpenLinkInNewWindow1Click(Self);
        CLIENT_ID_CLIPBOARD_COPY: CopyLinkToClipboard1Click(Self);
        end;
    end;
end;

function GetExceptionDump(E:Exception): String;
var
  I: Integer;
  Frames: PPointer;
begin
  Result := #9 + 'Stacktrace:' + LineEnding;
  if E <> nil then
  begin
    Result := Result + #9#9 + 'Exception class: ' + E.ClassName + LineEnding;
    Result := Result + #9#9 + 'Message: ' + E.Message + LineEnding;
  end;
  Result := Result + #9#9 + 'Address: ' + BackTraceStrFunc(ExceptAddr) + LineEnding;

  Frames := ExceptFrames;
  if ExceptFrameCount > 0 then
  begin
    Result := Result + #9#9 + 'Frames:' + LineEnding;
    for I := 0 to ExceptFrameCount - 1 do
      Result := Result + LineEnding + #9#9#9 + BackTraceStrFunc(Frames[I]) + LineEnding;
  end
  else
    Result := Result + #9#9 + 'Frames: none';
  Result := TrimRight(Result);
end;

procedure FetchSeqsFromBlastRes(const doc: ICefDomDocument);
begin
    MegaBrowserFrm.activeBrowser.MainFrame.ExecuteJavaScript(
      'document.forms["overview0"].dwGenBank.click();document.forms["overview0"].dw_cont.click()','https://www.google.com/', 0);
end;

procedure CopyAllToClipboardCallback(const doc: ICefDomDocument);
var
  OutString: String;
begin
  OutString := doc.Body.ElementInnerText;
  ClipBoard.SetTextBuf(PChar(OutString));
end;

procedure PrintCallback(const doc: ICefDomDocument);
var
  i, x, y: Integer;
  PageText: TStringList;
begin
  PageText := nil;
  try
    try
      PageText := TStringList.Create;
      PageText.Text := doc.Body.ElementInnerText;
      Printer.BeginDoc;
      X := 200;
      Y := 200;
      for i := 0 to PageText.Count - 1 do
      begin
        if (not Printer.Aborted) then
        begin
          Printer.Canvas.TextOut(X, Y, PageText[i]);
          Y := Y + 80;
          if (Y > (Printer.PageHeight - 300)) then
          begin
            Y := 200;
            Printer.NewPage;
            Sleep(500);  // to give the user time to abort!
          end;
        end;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! Printing failed because of an error: ' + E.Message);
    end;
  finally
    if Assigned(PageText) then
      PageText.Free;
    if (not Printer.Aborted) then
      Printer.EndDoc;
  end;
end;

procedure SaveCaptionToTextCallback(const doc: ICefDomDocument);
var
  OutText : TStringList = nil;
  SaveDialog: TSaveDialog = nil;
begin
  OutText := TStringList.Create;
  OutText.Text := doc.Body.ElementInnerText;
  try
    SaveDialog := TSaveDialog.Create(nil);
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog.InitialDir);
    if SaveDialog.Execute and (SaveDialog.FileName <> EmptyStr) then
      OutText.SaveToFile(SaveDialog.FileName);
  finally
    if Assigned(OutText) then
	   OutText.Free;
    if Assigned(SaveDialog) then
	   SaveDialog.Free;
  end;
end;

procedure FetchSeqsFromHTML(const doc: ICefDomDocument);
var
  PleaseWait : TPleaseWait;
  MyInnerText : TStringList;
  ImportController: TBrowserImportController;
  IsValidData: boolean;
  msg: ICefProcessMessage;
begin
  try
    try
      PleaseWait := TPleaseWait.Create(MegaBrowserFrm);
      PleaseWait.Caption     := 'Add to AlnExplorer';
      PleaseWait.Action      := 'Exporting sequences to AlnExplorer...';
      PleaseWait.PercentDone := 0;
      PleaseWait.Show;
      Application.ProcessMessages;
      MyInnerText := TStringList.Create;
      MyInnerText.Text := (doc.Body.ElementInnerText);
      ImportController := TBrowserImportController.Create;
      IsValidData := ImportController.ValidateData(MyInnerText);
      if IsValidData then
      begin
        // Create a message and send it to the main thread
        msg := TCefProcessMessageRef.New('visitdomproc-fetchseqsfromhtml-export');
        msg.ArgumentList.SetString(0, doc.Body.ElementInnerText);
        MegaBrowserFrm.activeTabChromium.SendProcessMessage(PID_BROWSER, msg);

        //ImportController.ExportToAlignEditor(MyInnerText);
      end
      else
      begin
        if ImportController.ValidateMessyFastaData(MyInnerText) then
        begin
          MessageDlg('You are attempting to import sequences which look like fasta sequences, but contain other invalid data.'+ LineEnding+''+ LineEnding+'If you are using the NCBI "FASTA" display option, then please choose "FASTA (text)" instead.', mtWarning, [mbOK], 0);
          // After we find out that this is NOT valid we should exit imediately.
        end
        else
          tryNCBIDownload := true;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    visitFetchSeqsFromHTMLFinished := true;

    // Create a message and send it to the main thread
    msg := TCefProcessMessageRef.New('visitdomproc-fetchseqsfromhtml-complete');
    msg.ArgumentList.SetBool(0, tryNCBIDownload);
    msg.ArgumentList.SetBool(1, visitFetchSeqsFromHTMLFinished);
    MegaBrowserFrm.activeTabChromium.SendProcessMessage(PID_BROWSER, msg);

    if assigned(MyInnerText) then
     MyInnerText.Free;
    if Assigned(ImportController) then
      ImportController.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
  end;
end;
function getpath(const n: ICefDomNode): string;
begin
  Result := '<' + n.Name + '>';
  if (n.Parent <> nil) then
    Result := getpath(n.Parent) + Result;
end;

procedure domvisitorcallback(const doc: ICefDomDocument);
begin
  if FillBlastFormCheck and Assigned(doc.GetElementById('seq')) then
  begin
    FillBlastFormCheck := false;
    MegaBrowserFrm.activeBrowser.MainFrame.ExecuteJavaScript(
      'document.getElementById(''seq'').value='''+MegaBrowserFrm.BlastSequence+''';','https://www.google.com/', 0);
  end;
end;

function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; HideWindow: Boolean = False): TMegaBrowserFrm;
begin
  Result := CreateNewChromiumBrowserWindow(Mode, MegaForm, HideWindow);
end;

function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; Owner: TComponent; HideWindow: Boolean = False): TMegaBrowserFrm;
  function LaunchNewBrowserTab: TMegaBrowserFrm;
  begin
    if not Assigned(MegaBrowserFrm) then
        MegaBrowserFrm := TMegaBrowserFrm.Create(Application)
     else
        MegaBrowserFrm.NewTab();
     Result := MegaBrowserFrm;
  end;

var
  NewWindow: TMegaBrowserFrm;
begin
  if Mode = bmBrowser then
     NewWindow := LaunchNewBrowserTab
  else
  begin
    NewWindow := TMegaBrowserFrm.Create(Owner);
  end;

  case Mode of
    bmBrowser:
      NewWindow.RunInWebBrowserMode;
    bmCaption, bmCitation:
      NewWindow.RunInCaptionMode;
    bmResults:
      NewWindow.RunInExcelExportMode;
    bmDefaultBrowser:
      NewWindow.RunInDefaultWebBrowserMode;
    else
      Assert(False); // then somebody must have added a new TBrowserMode but didn't add it here
  end;
  NewWindow.BrowserMode := Mode;

  if not HideWindow then
  begin
    {$IFDEF VISUAL_BUILD}
    AddWindowToTray(NewWindow);
    {$ENDIF}
    newWindow.Show;
    newWindow.Invalidate;
    newWindow.Update;
    {$IFDEF DARWIN}Application.ProcessMessages;{$ENDIF}
  end;
  Result := newWindow;
end;

function GetHelpBrowser(helpFile: String): TMegaBrowserFrm;
var
  cleanUrl: String;
begin
  try
    if not Assigned(HelpBrowser) then
    begin
      HelpBrowser := TMegaBrowserFrm.Create(Application);
      HelpBrowser.RunInHelpMode;
    end;
    Result := HelpBrowser;
    cleanUrl := 'file:///' + helpFile;
    {$IFDEF MSWINDOWS}
    cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
    {$ENDIF}
    if not Result.Visible then
      Result.Show
    else
      Result.BringToFront;
    if not Assigned(Result.activeTabChromium) then
       Result.InitialTab(cleanUrl)
    else
       Result.activeTabChromium.LoadURL(cleanUrl);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when initializing the help browser: ' + E.Message);
  end;
end;

function ShowFileInBrowser(aFile: String; tabCaption: String): TMegaBrowserFrm;
var
  cleanUrl: String = '';
begin
  {$IFNDEF NO_BROWSER}
  try
    if not Assigned(FileBrowser) then
    begin
      FileBrowser := TMegaBrowserFrm.Create(Application);
      FileBrowser.RunInFileViewerMode;
      FileBrowser.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': File Browser';
    end;
    FileBrowser.FileBrowserFilename := aFile;
    Result := FileBrowser;
    cleanUrl := 'file:///' + aFile;
    {$IFDEF MSWINDOWS}
    cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
    {$ENDIF}

    AddWindowToTray(Result);
    if not Result.Visible then
      Result.Show
    else
      Result.BringToFront;
    if not Assigned(Result.activeTabChromium) then
      Result.InitialTab(cleanUrl)
    else
      Result.activeTabChromium.LoadURL(cleanUrl);
    Result.Height := 800;
  except
    on E:Exception do
      ShowMessage('Application error when initializing the help browser to display a file: ' + E.Message);
  end;
  {$ENDIF}
end;

procedure CloseBrowserFile(aFile: String);
begin
  if Assigned(FileBrowser) and (aFile = FileBrowser.FileBrowserFilename) then
    FileBrowser.Close;
end;

function FileIsOpenInFileBrowser(filename: String): Boolean;
begin
  Result := False;
  if Assigned(FileBrowser) then
    Result := filename = FileBrowser.FileBrowserFilename;
end;

procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
begin
  commandLine.AppendSwitchWithValue('proxy-auto-detect', 'true');
end;

{ MegaBrowserFrm }

procedure TMegaBrowserFrm.actExportExcelExecute(Sender: TObject);
var
  ExportType: TExportType;
  SaveLocation: String;
  Response: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  SaveLocation := MegaForm.DataFileNameOnly + '_' + FMatrixExport.ExportProcedureString;
  WriteOutputDlg.Disallow(EXtext);
  if Sender = actExportCsv then
    ExportType := PromptUserWriteOutput(SaveLocation, True, EXcsvDisp)
  else
    ExportType := PromptUserWriteOutput(SaveLocation, True, EXexcelDisp);
  if ExportType = EXnone then
    Exit;
  if FileExists(SaveLocation) then
  begin
    Response := MessageDlg('Overwrite File?', 'The specified file already exists. Do you want to overwrite it?', mtConfirmation, mbYesNo, 0);
    if Response = mrNo then
      Exit;
  end;
  try
    if Assigned(FMatrixExport) then
      FMatrixExport.DoResultsExport(ExportType, SaveLocation);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred: ' + E.Message);
  end;
  {$ENDIF}
end;

function TMegaBrowserFrm.activeTab: TBrowserTab;
begin
  Result :=  MegaBrowserPanel1.ActiveTab;
end;

function TMegaBrowserFrm.activeTabChromium: TChromium;
begin
  Result := MegaBrowserPanel1.ActiveTabChromium;
end;

function TMegaBrowserFrm.activeBrowser: ICefBrowser;
begin
  Result := nil;
  if (MegaBrowserPanel1.TabCount > 0) then
      Result := MegaBrowserPanel1.ActiveTab.ChromiumComponent.Browser;
end;

procedure TMegaBrowserFrm.actFindTextExecute(Sender: TObject);
begin
  FindDialog1.Execute();
end;

procedure TMegaBrowserFrm.actMegaWebsiteExecute(Sender: TObject);
begin
  Self.activeTabChromium.LoadURL(WEBSITE_URL);
end;

procedure TMegaBrowserFrm.actNewTabExecute(Sender: TObject);
begin
  NewTab();
end;

procedure TMegaBrowserFrm.actNewWindowExecute(Sender: TObject);
var
  newWindow: TMegaBrowserFrm;
begin
  newWindow := CreateNewChromiumBrowserWindow(bmDefaultBrowser, MegaForm);
  newWindow.Left := Self.Left + 20;
  newWindow.Top := Self.Top + 20;
  newWindow.Show;
  newWindow.InitialTab('https://www.ncbi.nlm.nih.gov');
end;

procedure TMegaBrowserFrm.actNextExecute(Sender: TObject);
begin
  activeTabChromium.GoForward;
end;

procedure TMegaBrowserFrm.actNextUpdate(Sender: TObject);
begin
  if (not Assigned(activeTabChromium)) then
    Exit;

  if activeTabChromium.CanGoForward then
    actNext.Enabled := True
  else
    actNext.Enabled := False;
end;

procedure TMegaBrowserFrm.actPasteExecute(Sender: TObject);
begin
  if edAddress.Focused then
    edAddress.PasteFromClipboard
  else if activeTabChromium.FrameIsFocused then
       activeTabChromium.ClipboardPaste;
end;

procedure TMegaBrowserFrm.actPrevExecute(Sender: TObject);
begin
  actPrev.Enabled := false;
  activeTabChromium.GoBack;
  if (activeTabChromium.CanGoBack) then
    actPrev.Enabled := true;
end;

procedure TMegaBrowserFrm.actPrevUpdate(Sender: TObject);
begin
  if (not Assigned(activeTabChromium)) then
    Exit;

  if (activeTabChromium.CanGoBack) then
    actPrev.Enabled := True
  else
    actPrev.Enabled := False;
end;

procedure TMegaBrowserFrm.actReloadExecute(Sender: TObject);
begin
  if Assigned(activeTabChromium) then
      activeTabChromium.Reload;
end;

procedure TMegaBrowserFrm.actReloadUpdate(Sender: TObject);
begin
  actReload.Enabled := (Assigned(activeTabChromium) and (activeBrowser <> nil) and (not FLoading));
end;

procedure TMegaBrowserFrm.actStopExecute(Sender: TObject);
begin
  if Assigned(activeTabChromium) then
  activeTabChromium.StopLoad;
end;

procedure TMegaBrowserFrm.actZoomInExecute(Sender: TObject);
begin
  if Assigned(activeTabChromium) then
     activeTabChromium.IncZoomStep;
end;

procedure TMegaBrowserFrm.actZoomOutExecute(Sender: TObject);

begin
   if Assigned(activeTabChromium) then
      activeTabChromium.DecZoomStep;
end;

procedure TMegaBrowserFrm.actZoomResetExecute(Sender: TObject);
begin
  if Assigned(activeTabChromium) then
     activeTabChromium.ResetZoomLevel;
end;

procedure TMegaBrowserFrm.AddToAlignmentExecute(Sender: TObject);
var
  AChr: TChromium = nil;
begin
  FindAlignmentEditorWindow(False);
  visitFetchSeqsFromHTMLFinished := False;
  tryNCBIDownload := False;
  AChr := activeTabChromium;
  if (not Assigned(AChr)) then
    Exit;
  try
    AChr.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_FETCHSEQSFROMHTML));
    Log('visitdomproc-fetchseqsfromhtml message sent');
  except
  on E: Exception do
    Log('failed to process Add To Alignment command');
  end;
end;

procedure TMegaBrowserFrm.AddToAlignmentUpdate(Sender: TObject);
begin
  AddToAlignment.Enabled := (not FLoading);
end;

procedure TMegaBrowserFrm.actSelectAllExecute(Sender: TObject);
begin
  if edAddress.Focused then
    edAddress.SelectAll
  else
    activeTabChromium.SelectAll;
end;

procedure TMegaBrowserFrm.actLoadHTMLExecute(Sender: TObject);
var
  filename : string;
  fileURL  : string;
begin
  if OpenDialog1.Execute then
     begin
        filename := OpenDialog1.Filename;
        {$IFDEF UNIX}fileURL  := 'file:///' + filename;{$ENDIF}
        {$IFDEF WINDOWS}fileURL  := filename;{$ENDIF}
        NewTab(fileURL);
        edAddress.Text := fileURL;
     end;
end;

procedure TMegaBrowserFrm.actPrintExecute(Sender: TObject);
var
  jsMessage: String;
begin
  if activeTabChromium <> nil then
  begin
    //{$IFDEF DARWIN}
     //jsMessage := 'Printing not yet implemented for macOS. Please use copy/paste to print from a document.';
     //ShowJSMessageDialog(jsMessage);
     //Exit;
    //{$ENDIF}
    activeTabChromium.Print;
  end;
end;

procedure TMegaBrowserFrm.actSaveToTextFileExecute(Sender: TObject);
var
  aStr: String;
  aList: TStringList = nil;
  message: ICefProcessMessage;
begin
  try
    if activeTabChromium = nil then
      Exit;
    if (FBrowserMode = bmCaption) or (FBrowserMode = bmResults) or (FBrowserMode = bmCitation) then
    begin
      try
        try
          activeTabChromium.SelectAll;
          activeTabChromium.ClipboardCopy;
          SaveDialog.Filter := 'Text Files|*.txt|All Files|*.*';
          SaveDialog.DefaultExt := 'txt';
          SaveDialog.InitialDir := GetCurrentDir;
          if FBrowserMode = bmCaption then
            SaveDialog.Filename := 'Caption.txt'
          else
          begin
            if Trim(FButtonHint) <> EmptyStr then
              SaveDialog.Filename := ChangeFileExt(FButtonHint, '.txt')
            else
              SaveDialog.Filename := 'Results.txt';
          end;
          if SaveDialog.Execute and (SaveDialog.FileName <> '') then
          begin
            {$IFNDEF DARWIN}
            aStr := Clipboard.AsText;
            aList := TStringList.Create;
            aList.Text := aStr;
            aList.SaveToFile(SaveDialog.FileName);
            OpenFileAndFocus(SaveDialog.FileName, 0, 0);
            {$ELSE}
            aStr := SaveDialog.Filename;
            message := TCefProcessMessageRef.New(VISITDOMPROC_MACSAVECAPTIONTOTEXT);
            message.ArgumentList.SetString(0, aStr);
            activeTabChromium.SendProcessMessage(PID_RENDERER, message);
            {$ENDIF}
          end;
        except
          on E:Exception do
            ShowMessage('Oh no! An error occurred when saving the caption text to a file: ' + E.Message);
        end;
      finally
        if Assigned(aList) then
          aList.Free;
      end;
    end
    else
      activeTabChromium.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_SAVECAPTIONTOTEXT));
  except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaBrowserFrm.crmBeforeDownload(Sender: TObject; const Browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
var
  downloadLoc: String;
begin
  downloadLoc := EmptyStr;
  Log('crmBeforeDownload');
  if (downloadItem.mimeType = 'application/octet-stream') and (pos('sequence.gb', suggestedName) = 1) and (blastSeqsBtnClicked) then
  begin
    downloadLoc := GetTempDir + 'seqdump.txt';
  end
  else
  begin
    // TODO append default folder
    downloadLoc := suggestedName;
  end;
  try
    if downloadLoc <> EmptyStr then
    begin
      blastSeqsBtnClicked := false;
      callback.Cont(downloadLoc, True);
    end;
  Except on E: EfCreateError do
    ShowMessage('There was a problem downloading the file.  You may want to try again.');
  end;
  blastSeqsBtnClicked := false;
end;

procedure TMegaBrowserFrm.DeveloperActionExecute(Sender: TObject);
var
  aList: TStringList = nil;
  jsMessage: String;
begin
  {$IFDEF VISUAL_BUILD}{$IFDEF DEBUG}
  aList := TStringList.Create;
  aList.LoadFromFile('C:\Users\gstecher\mydata\Documents\lazarusProjects\MEGA-Lazarus\UnitTests\Web\Genbank_result_multiple_CDSs.txt');
  if Assigned(FImportController) then
    FreeAndNil(FImportController);
  FImportController := TBrowserImportController.Create;

  if not FImportController.ValidateData(aList) then
    ShowMessage('failed to validate genbank data');
  FImportController.RawSequenceData := aList;
  if FImportController.WebDataFormat = wsdfGenbank then
  begin
    if FImportController.HasCDSs then
    begin
      PromptForCDSsToUse;
    end;
  end
  else if FImportController.WebDataFormat = wsdfFasta then
  begin
    if FImportController.ValidateData(aList) then
      FImportController.ExportFastaToAlignEditor(aList)
    else
    begin
      jsMessage := 'Unable to find sequence data in the web document. '+
                  'Please make sure that you display the sequence data '+
                  'in FASTA/GenBank format. '+
                  'If you are using NCBI Blast, check the sequences you want to download first.';
      ShowJSMessageDialog(jsMessage);
    end;
  end;
  if Assigned(aList) then
    aList.Free;
  {$ENDIF}{$ENDIF}
end;

procedure TMegaBrowserFrm.FindDialog1Find(Sender: TObject);
var
  aString: String = '';
  code: ustring;
  aCaseSensitive : String = 'false';
  aBackwards : String = 'true';
  aWrapAround : String = 'false';
  aSearchInFrames : String = 'true';
  aWholeWord: String = 'false';
begin
  if Assigned(activeTabChromium) then
  begin
    if frMatchCase in FindDialog1.Options then
      aCaseSensitive := 'true';
    if not (frDown in FindDialog1.Options) then
      aBackwards := 'false';
    if frEntireScope in FindDialog1.Options then
      aWrapAround := 'true';
    if frWholeWord in FindDialog1.Options then
      aWholeWord := 'true';
    FindDialog1.Options := FindDialog1.Options +  [frFindNext];
    with FindDialog1 do
    begin

      aString := Format('window.find("%s","%s","%s","%s","%s")', [FindText, aCaseSensitive, aBackwards, aWrapAround, aWholeWord, aSearchInFrames]);
      code := ustring(aString);
      activeTabChromium.ExecuteJavaScript(code, 'about:blank',0);
    end;
    activeTabChromium.Invalidate;
  end;
end;

procedure TMegaBrowserFrm.ProcessJSMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
begin
  if HtmlOptionsDialog.Visible then
    HtmlOptionsDialog.Hide;
end;

procedure TMegaBrowserFrm.ShowJSMessageDialog(jsMessage: String);
var
  js: String;
begin
  js := EmptyStr;
  HtmlOptionsDialog.HelpAction.Visible := False;
  HtmlOptionsDialog.Show;
  HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessJSMessage;
  HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_JS_MESSAGE_DIALOG;
  js := js + Format('$("#%s").append("%s");', [JS_MESSAGE, jsMessage]);
  HtmlOptionsDialog.LoadOptionsFile(wofJSMessageDialog, js, 'Message', 390, 200, HC_Edit_Menu_in_Alignment_Explorer);
end;

procedure TMegaBrowserFrm.TitleChange(Sender: TObject; const title: ustring);
var
  selectBrowserTab: TBrowserTab = nil;
begin
  Log('TitleChange: ' + title);

  selectBrowserTab := activeTab;
  if not Assigned(selectBrowserTab) then
    Exit;

  if (Sender = selectBrowserTab) then
  begin
    if FBrowserMode = bmBrowser then
    begin
      Caption := 'MEGA Web Browser: ' + title;
      // The title change could come from another tab.  Make sure we find the tab relative to the Sender object.
      MegaBrowserPanel1.ActiveTab.TabData.TabCaption := title;
    end
    else if FBrowserMode = bmFileViewer then
    begin
      Caption := VER_MEGA_WIN_CAPTION_PREFIX +  ': File Viewer';
      MegaBrowserPanel1.ActiveTab.TabData.TabCaption := title;
    end
    else if FBrowserMode = bmHelpBrowser then
    begin
      Caption := 'MEGA Help';
      MegaBrowserPanel1.ActiveTab.TabData.TabCaption := title;
    end
    else if FBrowserMode = bmDefaultBrowser then
    begin
       Caption := 'MEGA Web Browser';
       MegaBrowserPanel1.ActiveTab.TabData.TabCaption := title;
    end
    else if (FBrowserMode = bmCaption) then
    begin
       FCaptionTitle := title;
       Caption := 'MEGA Caption Expert: ' + title;
       MegaBrowserPanel1.ActiveTab.TabData.TabCaption := 'Citation';
    end
    else if (FBrowserMode = bmResults) then
    begin
       Caption := 'Results';
       MegaBrowserPanel1.ActiveTab.TabData.TabCaption := 'Results';
    end
    else if FBrowserMode = bmCitation then
      Caption := 'MEGA Citation'
    else
        MegaBrowserPanel1.ActiveTab.TabData.TabCaption := title;
  end;
end;

procedure TMegaBrowserFrm.URLChange(Sender: TObject; const url: ustring);
var
  selectBrowserTab: TBrowserTab = nil;
begin
  Log('URLChange: ' + url);

  selectBrowserTab := activeTab;
  if not Assigned(selectBrowserTab) then
    Exit;

  if (Sender = self) or (Sender = selectBrowserTab.BrowserFrame) then
    edAddress.Text := UTF8Encode(url);
end;

procedure TMegaBrowserFrm.StatusChange(Sender: TObject; const status: ustring);
var
  selectBrowserTab: TBrowserTab = nil;
begin
  Log('StatusChange: ' + status);

  selectBrowserTab := activeTab;
  if not Assigned(selectBrowserTab) then
    Exit;

  if (Sender = self) or (Sender = selectBrowserTab.BrowserFrame) then
    StatusBar.Panels[0].Text := status;
end;

procedure TMegaBrowserFrm.ButtonStateChange(Sender: TObject);
var
  selectBrowserTab: TBrowserTab = nil;
  BrowserIsLoading: Boolean;
  BrowserCanGoBack: Boolean;
  BrowserCanGoForward: Boolean;
begin
  selectBrowserTab := activeTab;
  if not Assigned(selectBrowserTab) then
    Exit;

  BrowserIsLoading := selectBrowserTab.BrowserFrame.BrowserIsLoading;
  BrowserCanGoBack := selectBrowserTab.BrowserFrame.BrowserCanGoBack;
  BrowserCanGoForward := selectBrowserTab.BrowserFrame.BrowserCanGoForward;

  if (Sender = self) or (Sender = selectBrowserTab.BrowserFrame) then
  begin
       actPrev.Enabled    := BrowserCanGoBack;
       actNext.Enabled := BrowserCanGoForward;

  if BrowserIsLoading then
    begin
      actReload.Enabled := False;
      actStop.Enabled   := True;
    end
   else
    begin
      actReload.Enabled := True;
      actStop.Enabled   := False;
    end;
  end;
end;

procedure TMegaBrowserFrm.edAddressClick(Sender: TObject);
begin
  if not edAddress.Focused then
    edAddress.SelectAll;
  edAddress.SetFocus;
end;

procedure TMegaBrowserFrm.edAddressKeyPress(Sender: TObject; var Key: char);
var
  url: String;
begin
  if (Key = #13) or (Key = #10) or (Key = LineEnding) then
  begin
    if Assigned(activeTabChromium) and Assigned(activeTabChromium.Browser) then
    begin
      url := Trim(edAddress.Text);
      activeTabChromium.LoadURL(url);
    end;
  end;
end;

procedure TMegaBrowserFrm.FormActivate(Sender: TObject);
begin
  FMain := Self;
  if Assigned(activeTabChromium) then
    activeTabChromium.Invalidate;
  MegaForm.UpdateMenuForPinupItems(WindowsMenuItem, Self);
end;

procedure TMegaBrowserFrm.CopyLinkToClipboard1Click(Sender: TObject);
begin
  ClipBoard.AsText := FLinkUrl;
  actCopyExecute(Sender);
end;

procedure TMegaBrowserFrm.FormDestroy(Sender: TObject);
begin
  if MegaBrowserFrm = Self then
    MegaBrowserFrm := nil;
  if BrowserMode = bmHelpBrowser then
    HelpBrowser := nil;
  if Assigned(FAnalysisSummary) then
    FAnalysisSummary.Free;
  if Assigned(FImportController) then
    FImportController.Free;
  if Assigned(FMatrixExport) then
    FMatrixExport.Free;
  FreeAndNil(FCritSection);
end;

function TMegaBrowserFrm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  if FBrowserMode = bmHelpBrowser then
    Exit;

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

// in the CEF4Delphi demo, FormShow is unique to Windows implementation
// todomike make sure this is utilized correctly in Linux
procedure TMegaBrowserFrm.FormShow(Sender: TObject);
begin
  if DoNotAddToWindowTray then
    Exit;
  Caption:= 'Initializing Browser Resources. Please Wait...';
  AddWindowToTray(Self);
end;

procedure TMegaBrowserFrm.MainMenu1DrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin
  MegaForm.DrawMenuItem(Sender, ACanvas, ARect, AState);
end;

procedure TMegaBrowserFrm.MainMenu1MeasureItem(Sender: TObject;
  ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MegaForm.MeasureMenuItem(Sender, ACanvas, AWidth, AHeight);
end;

procedure TMegaBrowserFrm.Search1Click(Sender: TObject);
begin
  activeBrowser.MainFrame.LoadUrl('https://www.google.com/');
end;

procedure TMegaBrowserFrm.StatusBar1Click(Sender: TObject);
begin
  StatusBar1.Checked := (not StatusBar1.Checked);
  StatusBar.Visible := StatusBar1.Checked;
end;

procedure TMegaBrowserFrm.ToggleToolbarClick(Sender: TObject);
begin
  Self.ToggleToolBar.Checked := (not Self.ToggleToolBar.Checked);
  Toolbar1.Visible := ToggleToolBar.Checked;
end;

procedure TMegaBrowserFrm.OpenLinkInNewTab1Click(Sender: TObject);
var
  url: String;
begin
  try
    url := Trim(FLinkUrl);
    NewTab(url);
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred during navigation: ' + E.Message);
  end;
end;

procedure TMegaBrowserFrm.OpenLinkInNewWindow1Click(Sender: TObject);
var
  ABrowser: TMegaBrowserFrm;
  url: String;
begin
  {$IFDEF VISUAL_BUILD}
  try
    ABrowser := CreateNewChromiumBrowserWindow(bmDefaultBrowser, MegaForm);
    url := Trim(FLinkUrl);
    ABrowser.GoToUrl(url);
    ABrowser.Show;
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred during navigation: ' + E.Message);
  end;
  {$ENDIF}
end;

procedure TMegaBrowserFrm.Timer1Timer(Sender: TObject);
begin
  case timeframe of
    0: StatusBar.Panels[0].Text := 'Loading';
    1: StatusBar.Panels[0].Text := 'Loading.';
    2: StatusBar.Panels[0].Text := 'Loading..';
    3: StatusBar.Panels[0].Text := 'Loading...';
  end;
  timeframe := (timeframe + 1) mod 3;

end;

procedure TMegaBrowserFrm.BrowserInfoActionExecute(Sender: TObject);
var
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    aList.Add(Format('Chrome version:          %s', [GlobalCEFApp.ChromeVersion]));
    aList.Add(Format('Chrome major ver:        %d', [GlobalCEFApp.ChromeMajorVer]));
    aList.Add(Format('Chrome minor ver:        %d', [GlobalCEFApp.ChromeMinorVer]));
    aList.Add(Format('Chrome release:          %d', [GlobalCEFApp.ChromeRelease]));
    aList.Add(Format('Chrome build:            %d', [GlobalCEFApp.ChromeBuild]));
    aList.Add(Format('libcef version:          %s', [GlobalCEFApp.LibCefVersion]));
    aList.Add(Format('Child process count:     %d', [GlobalCEFApp.ChildProcessesCount]));
    aList.Add(Format('Used memory(MB):         %.0n', [GlobalCEFApp.UsedMemory/1024/1024*1.0]));
    aList.Add(Format('Total memory(MB):        %.0n', [GlobalCEFApp.TotalSystemMemory/1024/1024*1.0]));
    aList.Add(Format('Available memory(MB):    %.0n', [GlobalCEFApp.AvailableSystemMemory/1024/1024*1.0]));
    aList.Add(Format('Memory load(MB):         %.0n', [GlobalCEFApp.SystemMemoryLoad/1024/1024*1.0]));
    aList.Add(Format('JavaScript enabled:      %s', [BoolToStr(not GlobalCEFApp.DisableJavascript, True)]));
    aList.Add(Format('Multiprocess:            %s', [BoolToStr(not GlobalCEFApp.SingleProcess, True)]));
    aList.Add(Format('Enable media stream:     %s', [BoolToStr(GlobalCEFApp.EnableMediaStream, True)]));
    aList.Add(Format('Enable speech input:     %s', [BoolToStr(GlobalCEFApp.EnableSpeechInput, True)]));
    aList.Add(Format('Enable GPU:              %s', [BoolToStr(GlobalCEFApp.EnableGPU, True)]));
    aList.Add(Format('Chrome runtime:          %s', [BoolToStr(GlobalCEFApp.ChromeRuntime, True)]));
    aList.Add(Format('Multithreaded loop       %s', [BoolToStr(GlobalCEFApp.MultiThreadedMessageLoop, True)]));
    aList.Add(Format('Windowless rendering     %s', [BoolToStr(GlobalCEFApp.WindowlessRenderingEnabled, True)]));
    aList.Add(Format('Scale factor:            %.2f', [GlobalCEFApp.DeviceScaleFactor]));
    {$IFDEF MSWINDOWS}
    aList.Add(Format('High DPI support:        %s', [BoolToStr(GlobalCEFApp.EnableHighDPISupport, True)]));
    {$ENDIF}

    {$IFDEF DARWIN}
    aList.Add(Format('Bundle path:             %s', [GlobalCEFApp.MainBundlePath]));
    {$ENDIF}
    aList.Add(Format('Command args enabled:    %s', [BoolToStr(not GlobalCEFApp.CommandLineArgsDisabled, True)]));

    aList.Add(Format('Default encoding         %s', [GlobalCEFApp.DefaultEncoding]));
    aList.Add(Format('User agent               %s', [GlobalCEFApp.UserAgent]));
    //aList.Add(Format('User agent product       %s', [GlobalCEFApp.UserAgentProduct]));
    aList.Add(Format('Locale                   %s', [GlobalCEFApp.Locale]));
    aList.Add(Format('Log file                 %s', [GlobalCEFApp.LogFile]));
    aList.Add(Format('Enabled features         %s', [GlobalCEFApp.EnableFeatures]));
    aList.Add(Format('Disable features         %s', [GlobalCEFApp.DisableFeatures]));
    aList.Add(Format('Enabled Blink features   %s', [GlobalCEFApp.EnableBlinkFeatures]));
    aList.Add(Format('Disable Blink features   %s', [GlobalCEFApp.DisableBlinkFeatures]));
    aList.Add(Format('Blink settings           %s', [GlobalCEFApp.BlinkSettings]));
    aList.Add(Format('Force field trials       %s', [GlobalCEFApp.ForceFieldTrials]));
    aList.Add(Format('Force field trial params %s', [GlobalCEFApp.ForceFieldTrialParams]));

    aList.Add(Format('Fast unload              %s', [BoolToStr(GlobalCEFApp.FastUnload, True)]));
    aList.Add(Format('Disable safe browsing    %s', [BoolToStr(GlobalCEFApp.DisableSafeBrowsing, True)]));
    aList.Add(Format('Mute audio               %s', [BoolToStr(GlobalCEFApp.MuteAudio, True)]));
    aList.Add(Format('Site per process         %s', [BoolToStr(GlobalCEFApp.SitePerProcess, True)]));
    aList.Add(Format('Disable web security     %s', [BoolToStr(GlobalCEFApp.DisableWebSecurity, True)]));
    aList.Add(Format('Disable PDF extension    %s', [BoolToStr(GlobalCEFApp.DisablePDFExtension, True)]));
    aList.Add(Format('Disable site iso trials  %s', [BoolToStr(GlobalCEFApp.DisableSiteIsolationTrials, True)]));
    aList.Add(Format('Disable Chrome login     %s', [BoolToStr(GlobalCEFApp.DisableChromeLoginPrompt, True)]));
    aList.Add(Format('Disable extensions       %s', [BoolToStr(GlobalCEFApp.DisableExtensions, True)]));
    aList.Add(Format('Disable bkgrd networking %s', [BoolToStr(GlobalCEFApp.DisableBackgroundNetworking, True)]));
    aList.Add(Format('Metrics recording only   %s', [BoolToStr(GlobalCEFApp.MetricsRecordingOnly, True)]));
    aList.Add(Format('Allow access from files  %s', [BoolToStr(GlobalCEFApp.AllowFileAccessFromFiles, True)]));
    aList.Add(Format('Allow insecure content   %s', [BoolToStr(GlobalCEFApp.AllowRunningInsecureContent, True)]));
    aList.Add(Format('Enable print preview     %s', [BoolToStr(GlobalCEFApp.EnablePrintPreview, True)]));
    aList.Add(Format('Disable JavaScript       %s', [BoolToStr(GlobalCEFApp.DisableJavascript, True)]));
    aList.Add(Format('Disable JS close windows %s', [BoolToStr(GlobalCEFApp.DisableJavascriptCloseWindows, True)]));
    aList.Add(Format('Disable JS access ClpBrd %s', [BoolToStr(GlobalCEFApp.DisableJavascriptAccessClipboard, True)]));
    aList.Add(Format('Disable JS DOM paste     %s', [BoolToStr(GlobalCEFApp.DisableJavascriptDomPaste, True)]));
    aList.Add(Format('Allow access file url    %s', [BoolToStr(GlobalCEFApp.AllowUniversalAccessFromFileUrls, True)]));
    aList.Add(Format('Disable image loading    %s', [BoolToStr(GlobalCEFApp.DisableImageLoading, True)]));
    aList.Add(Format('Image shring to fit      %s', [BoolToStr(GlobalCEFApp.ImageShrinkStandaloneToFit, True)]));
    aList.Add(Format('Disable text area resize %s', [BoolToStr(GlobalCEFApp.DisableTextAreaResize, True)]));
    aList.Add(Format('Disable tab to links     %s', [BoolToStr(GlobalCEFApp.DisableTabToLinks, True)]));
    aList.Add(Format('Enable profanity filter  %s', [BoolToStr(GlobalCEFApp.EnableProfanityFilter, True)]));
    aList.Add(Format('Disable spell checking   %s', [BoolToStr(GlobalCEFApp.DisableSpellChecking, True)]));
    aList.Add(Format('Disable canvas reading   %s', [BoolToStr(GlobalCEFApp.DisableReadingFromCanvas, True)]));
    aList.Add(Format('Hyperlink auditing       %s', [BoolToStr(GlobalCEFApp.HyperlinkAuditing, True)]));
    aList.Add(Format('Disable info timeout     %s', [BoolToStr(GlobalCEFApp.DisableNewBrowserInfoTimeout, True)]));
    aList.Add(Format('Disable zygote           %s', [BoolToStr(GlobalCEFApp.DisableZygote, True)]));
    aList.Add(Format('Use mock keychain        %s', [BoolToStr(GlobalCEFApp.UseMockKeyChain, True)]));
    //aList.Add(Format('Disable test request handling %s', [BoolToStr(GlobalCEFApp.DisableRequestHandlingForTesting, True)]));
    //aList.Add(Format('Disable popup blocking   %s', [BoolToStr(GlobalCEFApp.DisablePopupBlocking, True)]));
    //aList.Add(Format('Disable cache navigation %s', [BoolToStr(GlobalCEFApp.DisableBackForwardCache, True)]));
    //aList.Add(Format('Disable component update %s', [BoolToStr(GlobalCEFApp.DisableComponentUpdate, True)]));
    //aList.Add(Format('Kiosk printing           %s', [BoolToStr(GlobalCEFApp.KioskPrinting, True)]));
    case GlobalCEFApp.AutoplayPolicy of
      appDefault: aList.Add(Format('Autoplay policy          %s', ['default']));
      appDocumentUserActivationRequired: aList.Add(Format('Autoplay policy          %s', ['user activation required']));
      appNoUserGestureRequired: aList.Add(Format('Autoplay policy          %s', ['NO user gesture required']));
      appUserGestureRequired: aList.Add(Format('Autoplay policy          %s', ['user gesture required']));
    end;

    case GlobalCEFApp.SmoothScrolling of
      STATE_DEFAULT: aList.Add(Format('Smooth scrolling          %s', ['default']));
      STATE_ENABLED: aList.Add(Format('Smooth scrolling          %s', ['enabled']));
      STATE_DISABLED: aList.Add(Format('Smooth scrolling          %s', ['disabled']));
    end;

    OpenStringList(aList, 'Browser Properties', True);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TMegaBrowserFrm.FormResize(Sender: TObject);
begin
  {$IFDEF DEBUG}
  StatusBar.Panels[0].Width := 100;
  StatusBar.Panels[1].Width := StatusBar.ClientWidth - StatusBar.Panels[0].Width - 2;
  StatusBar.Panels[1].Text := Format('ClientWidth = %d, ClientHeight = %d', [ClientWidth, ClientHeight]);
  {$ENDIF}
end;

procedure TMegaBrowserFrm.SaveImageToFileActionExecute(Sender: TObject);
begin
  try
    SaveDialog.Filter := 'PNG Image|*.png';
    if SaveDialog.Execute then
    begin
      CaptionImage.Picture.SaveToFile(SaveDialog.FileName);
    end;
  except
    on E:Exception do
      ShowMessage('Application error: ' + E.Message);
  end;
end;

procedure TMegaBrowserFrm.ActionAnalysisSummaryExecute(Sender: TObject);
begin
  if Assigned(FAnalysisSummary) and (FAnalysisSummary.Count > 0) then
    OpenStringList(AnalysisSummary, 'Analysis Summary', True)
  else
    ShowMessage('Cannot display the analysis summary because it does not exist in the browser');
end;

procedure TMegaBrowserFrm.actTabLeftExecute(Sender: TObject);
var
  NextIndex: integer;
begin
  NextIndex := (MegaBrowserPanel1.Tabs.TabIndex - 1);
  NextIndex := (NextIndex + MegaBrowserPanel1.TabCount) mod MegaBrowserPanel1.TabCount;
  MegaBrowserPanel1.Tabs.TabIndex := NextIndex;
end;

procedure TMegaBrowserFrm.actTabRightExecute(Sender: TObject);
var
  NextIndex: integer;
begin
  NextIndex := (MegaBrowserPanel1.Tabs.TabIndex + 1);
  NextIndex := NextIndex mod (MegaBrowserPanel1.TabCount);
  MegaBrowserPanel1.Tabs.TabIndex := NextIndex;
end;

// Read the keypresses intercepted by the Chromium component
// and execute the corresponding action if the shortcut keys match
procedure TMegaBrowserFrm.HandleChromiumKeypress(Sender: TObject;
  event: PCefKeyEvent; osEvent: TCefEventHandle);
var
  aShortCut: TShortCut = $0;
  scCtrl: boolean;
  scShift: boolean;
  scAlt: boolean;
  scMeta: boolean;

begin
     if (KEYEVENT_KEYUP = event.kind) then
     begin
       // Shift key
       if ($2 and event.modifiers) = $2 then
           aShortCut := aShortCut or $2000;

       // Control key
       if ($4 and event.modifiers) = $4 then
           aShortCut := aShortCut or $4000;

       // Alt key
       if ($8 and event.modifiers) = $8 then
           aShortCut := aShortCut or $8000;

       // Character
       aShortCut := aShortCut or event.windows_key_code;

       if (aShortCut <> $0) then
       begin
         if (aShortCut = actCloseTab.ShortCut) then actCloseTab.Execute;
         if (aShortCut = actTabLeft.ShortCut) then actTabLeft.Execute;
         if (aShortCut = actTabRight.ShortCut) then actTabRight.Execute;
         if (aShortCut = actSaveToTextFile.ShortCut) then actSaveToTextFile.Execute;
         if (aShortCut = actFindText.ShortCut) then actFindText.Execute;
         if (aShortCut = actNewTab.ShortCut) then actNewTab.Execute;
       end;
     end;
end;

procedure TMegaBrowserFrm.CloseFile(fileUrl: String);
var
  i : integer;
  aCaption: String = '';
begin
  aCaption := ExtractFileName(fileUrl);

  for i := MegaBrowserPanel1.TabCount - 1 downto 0 do
  begin
    if MegaBrowserPanel1.TabAtIndex(i).Caption = aCaption then
    begin
      MegaBrowserPanel1.Tabs.DeleteTab(i, True, False);
      Exit;
    end;
  end;
end;

function TMegaBrowserFrm.FileIsOpen(filename: String): Boolean;
var
  i : integer;
  aCaption: String = '';
begin
  Result := False;
  if MegaBrowserPanel1.TabCount = 0 then
    Exit;
  aCaption := ExtractFileName(filename);
  for i := MegaBrowserPanel1.TabCount - 1 downto 0 do
    if MegaBrowserPanel1.TabAtIndex(i).Caption = aCaption then
      Exit(True);
end;

procedure TMegaBrowserFrm.TabsTabChanged(Sender: TObject);
var
  url: String;
  title: String;
  status: String;
  selectedBrowserTab: TBrowserTab;
begin
  selectedBrowserTab := activeTab;

  if Assigned(selectedBrowserTab) and (selectedBrowserTab <> nil) then
  begin
    if Assigned(selectedBrowserTab.ChromiumComponent) then
    begin
      url := selectedBrowserTab.ChromiumComponent.DocumentURL;
      title := selectedBrowserTab.TabData.TabCaption;

      TitleChange(self, title);
      URLChange(self, url);
      StatusChange(self, status);
      ButtonStateChange(self);
    end;
  end;

end;

procedure TMegaBrowserFrm.InitBrowserSettings(var Settings: TCefBrowserSettings);
var
  aCrm: TChromium;
begin
  aCrm := MegaBrowserFrm.activeTabChromium;
  FillChar(Settings, SizeOf(settings), 0);

  Settings.size := SizeOf(TCefBrowserSettings);
  settings.standard_font_family := CefString(aCrm.FontOptions.StandardFontFamily);
  settings.fixed_font_family := CefString(aCrm.FontOptions.FixedFontFamily);
  settings.serif_font_family := CefString(aCrm.FontOptions.SerifFontFamily);
  settings.sans_serif_font_family := CefString(aCrm.FontOptions.SansSerifFontFamily);
  settings.cursive_font_family := CefString(aCrm.FontOptions.CursiveFontFamily);
  settings.fantasy_font_family := CefString(aCrm.FontOptions.FantasyFontFamily);
  settings.default_font_size := aCrm.FontOptions.DefaultFontSize;
  settings.default_fixed_font_size := aCrm.FontOptions.DefaultFixedFontSize;
  settings.minimum_font_size := aCrm.FontOptions.MinimumFontSize;
  settings.minimum_logical_font_size := aCrm.FontOptions.MinimumLogicalFontSize;
end;

procedure TMegaBrowserFrm.InitWindowInfo(var info: TCefWindowInfo);
var
  rect: TRect;
  aParentForm: TCustomForm;
begin
  FillChar(info, SizeOf(info), 0);
  {$IFDEF WINDOWS}
    rect := GetClientRect;

    info.style := WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
    info.bounds.x := rect.Left;
    info.bounds.y := rect.Top;
    info.bounds.width := rect.Right - rect.Left;
    info.bounds.height := rect.Bottom - rect.Top;
    info.window_name := CefString('Dev Tools');
  {$ENDIF}
  {$IFDEF LINUX}
    aParentForm := GetParentForm(Self);

    //{$IFDEF LCLGTK2}
    //  gtk_widget_realize(PGtkWidget(fParentForm.Handle));
    //  info.parent_window := gdk_window_xwindow(PGtkWidget(fParentForm.Handle)^.window);
    //{$ENDIF}
    //{$IFDEF LCLQT}
    //  info.parent_window := QWidget_winId(TQtWidget(fParentForm.Handle).Widget);
    //{$ENDIF}

    info.bounds.x := Left;
    info.bounds.y := Top;
    info.bounds.width := Width;
    info.bounds.height := Height;
  {$ENDIF}
  {$IFDEF LCLCOCOA}
    rect := GetClientRect;

    info.parent_view := TCefWindowHandle(Handle);
    info.bounds.x := rect.Left;
    info.bounds.y := rect.Top;
    info.bounds.width := rect.Right - rect.Left;
    info.bounds.height := rect.Bottom - rect.Top;
  {$ENDIF}
end;

procedure TMegaBrowserFrm.ProcessCDSsOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: ustring;
  i, j : Integer;
  index: Integer = 0;
  tempCDSs: TBoolArrayArray;
  aSeq: TSequence = nil;
begin
  try
    try
      HtmlOptionsDialog.Hide;
      temp := message.ArgumentList.GetString(0);
      if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
        raise Exception.Create(temp);

      SetLength(tempCDSs, FImportController.NumValidGenbankResults);
      temp := message.ArgumentList.GetString(CDS_IMPORT_ONLY_CDS_INDEX);
      if temp = 'true' then
      begin
        FImportController.ImportOnlyCDSs := True;
        temp := message.ArgumentList.GetString(CDS_IMPORT_ALL_DATA_INDEX);
        Assert(temp = 'false', 'invalid radio checkbox vals');
        for i := 0 to FImportController.NumValidGenbankResults - 1 do
        begin
          temp := message.ArgumentList.GetString(i + 2);
          Assert(Length(temp) = FImportController.NumCDSs(i), Format('got %d CDSs but expected %d', [Length(temp), FImportController.NumCDSs(i)]));
          setLength(tempCDSs[i], FImportController.NumCDSs(i));
          if Length(temp) > 0 then
          begin
            for j := 1 to Length(temp) do
              begin
                if temp[j] = 'T' then
                  tempCDSs[i][j - 1] := True
                else
                  tempCDSs[i][j - 1] := False;
              end;
          end;
        end;
        FImportController.UpdateIncludedCDSs(tempCDSs);
      end
      else
      begin
        FImportController.ImportOnlyCDSs := False;
      end;
      aSeq := FImportController.GetFirstGenbankSequence;
      PromptForSeqNameOptions(aSeq);
    except
      on E:Exception do
      begin
        ShowJSMessageDialog('An error occurred when processing CDSs options: ' + E.Message);
      end;
    end;
  finally
    if Assigned(aSeq) then
      aSeq.Free;
  end;
end;

procedure TMegaBrowserFrm.ProcessSeqNameOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
begin
  try
    FImportController.BuildSequenceListFromGenBank;
    FImportController.ProcessSeqNameOptionsMessage(Browser, message, Result);
  except
    on E:Exception do
      ShowJSMessageDialog('An error occurred when processing sequence name options: ' + E.Message);
  end;
end;

procedure TMegaBrowserFrm.PromptForCDSsToUse;
var
  js: String = '';
  temp: String = '';
  i, j: Integer;
  c: TCDSArrayArray;
  id: String = '';
begin
  try
  begin
    c := FImportController.GetCDSs;
    HtmlOptionsDialog.Show;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessCDSsOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_SELECT_CDS_TO_IMPORT;
    for i := Low(c) to High(c) do
    begin
      if Length(c[i]) = 0 then
        continue;
      temp := Format('<div id=%sgp%d%s><h4 style=%sfont-weight:bold;%s>%s</h4>', [#39, i, #39, #39, #39, c[i][0].accession]);
      for j := Low(c[i]) to High(c[i]) do
      begin
        id := c[i][0].accession + '_' + c[i][j].gene;
        temp := temp + Format('<input type=%scheckbox%s name=%s%s%s value=%s%s%s id=%s%s%s ischecked=%strue%s checked />', [#39, #39, #39, id, #39, #39, id, #39, #39, id, #39, #39, #39]);
        temp := temp + Format('<label for=%s%s%s class=%sfont_weight_normal%s> %s <span style=%sfont-size:80%%%s>sites (%d..%d)</span></label><br><br>', [#39, id, #39, #39, #39, c[i][j].gene, #39, #39, c[i][j].join[0].x, c[i][j].join[0].y]);
      end;
      temp := temp + '</div>';
      js := js + Format('$("#%s").append("%s");', ['cds_fieldset', temp]) + LineEnding;
    end;
    HtmlOptionsDialog.LoadOptionsFile(wofSelectCDSsToImport, js, 'Features to Import', 400, 500, HC_Edit_Menu_in_Alignment_Explorer);
  end;
  except
    On E:Exception do
      ShowJSMessageDialog('An error occurred when setting CDSs options: ' + E.Message);
 end;
end;

procedure TMegaBrowserFrm.PromptForSeqNameOptions(aSeq: TSequence);
var
  js: String;
begin
  with aSeq do
    try
      begin
        js := EmptyStr;
        HtmlOptionsDialog.Show;
        HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessSeqNameOptionsMessage;
        HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_SEQNAME_OPTIONS;
        js := js + Format('$("#%s").select2({minimumResultsForSearch: Infinity, width: 340, data: [{id:"empty",text:""}, {id:"species",text:"%s"}, {id:"subspecies",text:"%s"}, {id:"strain",text:"%s"}, {id:"host",text:"%s"}, {id:"senotype",text:"%s"}, {id:"gene",text:"%s"}, {id:"allele",text:"%s"}, {id:"uids",text:"%s"}]});', [SEQNAME_FIRST,
        (aSeq.Species), (aSeq.Subspecies), (aSeq.Strain), (aSeq.Host), (aSeq.Senotype), (aSeq.Gene), (aSeq.Allele), (aSeq.AccessionNum)]) + LineEnding;
        js := js + Format('$("#%s").select2({minimumResultsForSearch: Infinity, width: 340, data: [{id:"empty",text:""}, {id:"species",text:"%s"}, {id:"subspecies",text:"%s"}, {id:"strain",text:"%s"}, {id:"host",text:"%s"}, {id:"senotype",text:"%s"}, {id:"gene",text:"%s"}, {id:"allele",text:"%s"}, {id:"uids",text:"%s"}]});', [SEQNAME_SECOND,
        (aSeq.Species), (aSeq.Subspecies), (aSeq.Strain), (aSeq.Host), (aSeq.Senotype), (aSeq.Gene), (aSeq.Allele), (aSeq.AccessionNum)]) + LineEnding;
        js := js + Format('$("#%s").select2({minimumResultsForSearch: Infinity, width: 340, data: [{id:"empty",text:""}, {id:"species",text:"%s"}, {id:"subspecies",text:"%s"}, {id:"strain",text:"%s"}, {id:"host",text:"%s"}, {id:"senotype",text:"%s"}, {id:"gene",text:"%s"}, {id:"allele",text:"%s"}, {id:"uids",text:"%s"}]});', [SEQNAME_THIRD,
        (aSeq.Species), (aSeq.Subspecies), (aSeq.Strain), (aSeq.Host), (aSeq.Senotype), (aSeq.Gene), (aSeq.Allele), (aSeq.AccessionNum)]) + LineEnding;
        js := js + Format('$("#%s").select2({minimumResultsForSearch: Infinity, width: 340, data: [{id:"empty",text:""}, {id:"species",text:"%s"}, {id:"subspecies",text:"%s"}, {id:"strain",text:"%s"}, {id:"host",text:"%s"}, {id:"senotype",text:"%s"}, {id:"gene",text:"%s"}, {id:"allele",text:"%s"}, {id:"uids",text:"%s"}]});', [SEQNAME_FOURTH,
        (aSeq.Species), (aSeq.Subspecies), (aSeq.Strain), (aSeq.Host), (aSeq.Senotype), (aSeq.Gene), (aSeq.Allele), (aSeq.AccessionNum)]) + LineEnding;
        js := js + Format('$("#%s").prop("checked", %s);', [SEQNAME_USE_INITIAL, 'false']) + LineEnding;
        js := js + Format('$("#%s").attr("ischecked", "%s");', [SEQNAME_USE_INITIAL, 'false']) + LineEnding;
        js := js + Format('$("#%s").val("%s");', [SEQNAME_FULL_INFO, (aSeq.SeqName)]) + LineEnding;
        js := js + Format('$("#%s").val("%s");', [SEQNAME_SEQ_LABEL, (aSeq.SeqName)]) + LineEnding;
        js := js + Format('$("#%s").val("%s");', [SEQNAME_FULL_NAME, aSeq.Species]) + LineEnding;
        js := js + Format('$("#%s").val("%s");', [SEQNAME_ABBR_NAME, AbbreviateGenusInSpeciesName(aSeq.Species)]) + LineEnding;

        js := js + Format('$("#%s").val("%s")', ['subspecies', aSeq.Subspecies]) + LineEnding;
        js := js + Format('$("#%s").val("%s")', ['strain', aSeq.Strain]) + LineEnding;
        js := js + Format('$("#%s").val("%s")', ['host', aSeq.Host]) + LineEnding;
        js := js + Format('$("#%s").val("%s")', ['senotype', aSeq.Senotype]) + LineEnding;
        js := js + Format('$("#%s").val("%s")', ['gene', aSeq.Gene]) + LineEnding;
        js := js + Format('$("#%s").val("%s")', ['allele', aSeq.Allele]) + LineEnding;
        js := js + Format('$("#%s").val("%s")', ['uids', aSeq.AccessionNum]) + LineEnding;
        HtmlOptionsDialog.LoadOptionsFile(wofSeqNameOptions, js, 'Input Sequence Label', 380, 500, HC_Edit_Menu_in_Alignment_Explorer);

      end;
      except
        On E:Exception do
          ShowJSMessageDialog('An error occurred when processing sequence name options: ' + E.Message);
      end;
end;

procedure TMegaBrowserFrm.Log(aMsg: String);
begin
  {$IFDEF DEBUG}
  FLog.Lines.Insert(0, aMsg);
  {$ENDIF}
end;

procedure TMegaBrowserFrm.actExportCsvExecute(Sender: TObject);
begin
  actExportExcelExecute(Sender);
end;

procedure TMegaBrowserFrm.actCopyToClipboardExecute(Sender: TObject);
begin
  if activeTabChromium = nil then
    Exit;

  if (FBrowserMode = bmCaption) or (FBrowserMode = bmResults) then
  begin
    activeTabChromium.SelectAll;
    activeTabChromium.ClipboardCopy;
    if Trim(FHtml) <> EmptyStr then
      activeTabChromium.LoadURL(CefGetDataURI(FHtml,'text/html'));
  end
  else
    activeTabChromium.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_COPYALLTOCLIPBOARD));
end;

procedure TMegaBrowserFrm.actCutExecute(Sender: TObject);
begin
  if edAddress.Focused then
    edAddress.CutToClipboard
  else if activeTabChromium.FrameIsFocused then
       activeTabChromium.ClipboardCut;
end;

procedure TMegaBrowserFrm.actDevToolsExecuteExecute(Sender: TObject);
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
begin
  InitBrowserSettings(Settings);
  InitWindowInfo(info);
  //activeBrowser.Host.ShowDevTools(info, nil, settings, nil);
end;

procedure TMegaBrowserFrm.actDisplayHelpContentsExecute(Sender: TObject);
begin
  {$IFDEF VISUAL_BUILD}
  if (FBrowserMode = bmBrowser) or (FBrowserMode = bmDefaultBrowser) then
  begin
    ShowContextSensitiveHelp(MapHelpContextToKeyword(HC_Web_Browser));
  end
  else
    MegaForm.DisplayHelpContentsActionExecute(nil);
  {$ENDIF}
end;

procedure TMegaBrowserFrm.actCopyExecute(Sender: TObject);
begin
  if edAddress.Focused then
    edAddress.CopyToClipboard
  else if activeTabChromium.FrameIsFocused then
    activeTabChromium.ClipboardCopy;
end;

procedure TMegaBrowserFrm.EnableLinkMenus(AreEnabled: Boolean);
begin
  if AreEnabled then
  begin
  //  Openlinkinnewtab1.Enabled := True;
  //  Openlinkinnewwindow1.Enabled := True;
  //  Copylinktoclipboard1.Enabled := True;
  //end
  //else
  //begin
  //  Openlinkinnewtab1.Enabled := False;
  //  Openlinkinnewwindow1.Enabled := False;
  //  Copylinktoclipboard1.Enabled := False;
  end;
end;

procedure TMegaBrowserFrm.LoadLinkData;
var
  linklist: TStringList = nil;
  filename: string;
  i: integer;
  j: integer;
begin
  FBlastBaseURL := 'https://blast.ncbi.nlm.nih.gov';
  BLASTnURL := FBlastBaseUrl + '/Blast.cgi?PROGRAM=blastn&PAGE_TYPE=BlastSearch&LINK_LOC=blasthome';
  BLASTpURL := FBlastBaseUrl + '/Blast.cgi?PROGRAM=blastp&PAGE_TYPE=BlastSearch&LINK_LOC=blasthome';
  QUERYURL := 'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi';

  Exit;

  filename := GetPrivateFile(mfFixedLinksFile);
  if FileExists(filename) then
  begin
    linklist := TStringList.Create;
    linklist.LoadFromFile(filename);
    if linklist.Count > 0 then
    begin
      //if linklist.Values['Search'] <> '' then
      //  Links1.Hint := LinkList.Values['Search'];
      if linklist.Values['BLASTn'] <> '' then
        BLASTnURL := LinkList.Values['BLASTn']
      else
        BLASTnURL := FBlastBaseUrl + '/Blast.cgi?CMD=Web&LAYOUT=OneWindows&AUTO_FORMAT=Fullauto&PAGE=Nucleotides&NCBI_GI=yes&FILTER=L&HITLIST_SIZE=100&SHOW_OVERVIEW=yes&AUTO_FORMAT=yes&SHOW_LINKOUT=yes';
      if linklist.Values['BLASTp'] <> '' then
        BLASTpURL := LinkList.Values['BLASTp']
      else
        BLASTpURL := FBlastBaseUrl + '/Blast.cgi?CMD=Web&LAYOUT=OneWindows&AUTO_FORMAT=Fullauto&PAGE=Proteins&NCBI_GI=yes&HITLIST_SIZE=100&COMPOSITION_BASED_STATISTICS=yes&SHOW_OVERVIEW=yes&AUTO_FORMAT=yes&CDD_SEARCH=yes&FILTER=L&SHOW_LINKOUT=yes';
      if linklist.Values['QUERY'] <> '' then
        QUERYURL := LinkList.Values['QUERY']
      else
        QUERYURL := 'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi';
    end;
    if (linklist.Values['Search'] = '') or (linklist.Values['BLASTn'] = '') or (linklist.Values['BLASTp'] = '') or (linklist.Values['QUERY'] = '') then
      UpdateLinkData;
    linklist.Free;
  end
  else
  begin
    BLASTnURL := FBlastBaseUrl + '/Blast.cgi?CMD=Web&LAYOUT=OneWindows&AUTO_FORMAT=Fullauto&PAGE=Nucleotides&NCBI_GI=yes&FILTER=L&HITLIST_SIZE=100&SHOW_OVERVIEW=yes&AUTO_FORMAT=yes&SHOW_LINKOUT=yes';
    BLASTpURL := FBlastBaseUrl + '/Blast.cgi?CMD=Web&LAYOUT=OneWindows&AUTO_FORMAT=Fullauto&PAGE=Proteins&NCBI_GI=yes&HITLIST_SIZE=100&COMPOSITION_BASED_STATISTICS=yes&SHOW_OVERVIEW=yes&AUTO_FORMAT=yes&CDD_SEARCH=yes&FILTER=L&SHOW_LINKOUT=yes';
    QUERYURL := 'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi';
    UpdateLinkData;
  end;

  //filename := GetPrivateFile(mfUserLinksFile);
  //if FileExists(filename) then
  //begin
  //  linklist := TStringList.Create;
  //  linklist.LoadFromFile(filename);
  //  if linklist.Count > 0 then
  //    for i := linklist.Count-1 downto 0 do
  //      with TToolButton.Create(Links) do
  //      begin
  //        Parent     := Links;
  //        AutoSize   := true;
  //        Caption    := LinkList.Names[i];
  //        Hint       := LinkList.Values[LinkList.Names[i]];
  //        ImageIndex := 6;
  //
  //        OnClick    := LinkButtonClick;
  //      end;
  //  linklist.Free;
  //end;
  //if Links.ButtonCount = 0 then
  //begin
  //  with TToolButton.Create(Links) do
  //  begin
  //    Parent     := Links;
  //    AutoSize   := true;
  //    Caption    := 'MEGA';
  //    Hint       := WEBSITE_URL;
  //    ImageIndex := 6;
  //    OnClick    := LinkButtonClick;
  //  end;
  //  UpdateUserLinkData;
  //end;
  UserDataUpdated := false;
end;

procedure TMegaBrowserFrm.SetAnalysisSummary(AValue: TStringList);
begin
  if Assigned(FAnalysisSummary) then
    FreeAndNil(FAnalysisSummary);
  FAnalysisSummary := AValue;
  ActionAnalysisSummary.Visible := Assigned(FAnalysisSummary);
  ActionAnalysisSummary.Enabled := ActionAnalysisSummary.Visible;
end;

procedure TMegaBrowserFrm.UpdateLinkData;
var
  linklist: TStringList = nil;
begin
  //try
  //  linklist := TStringList.Create;
  //  linklist.Add('Search='+Links1.Hint);
  //  linklist.Add('BLASTn='+BLASTnURL);
  //  linklist.Add('BLASTp='+BLASTpURL);
  //  linklist.Add('QUERY='+QUERYURL);
  //  linklist.SaveToFile(GetPrivateFile(mfFixedLinksFile));
  //finally
  //  if Assigned(linkList) then
  //    linkList.Free;
  //end;
end;

procedure TMegaBrowserFrm.UpdateUserLinkData;
var
  linklist: TStringList;
  i: integer;
begin
  //linklist := TStringList.Create;
  //if Links.ButtonCount > 0 then
  //  for i := 0 to Links.ButtonCount-1 do
  //    linklist.Add(Links.Buttons[i].Caption+'='+Links.Buttons[i].Hint);
  //linklist.SaveToFile(GetPrivateFile(mfUserLinksFile));
  //
  //linklist.Free;
  UserDataUpdated := false;
end;

procedure TMegaBrowserFrm.SetShowCaptionTools(Value: Boolean);
begin
  actSaveToTextFile.Visible := Value;
  actSaveToTextFile.Enabled := Value;
  actCopyToClipboard.Visible := Value;
  actCopyToClipboard.Enabled := Value;
  actPrint.Visible := Value;
  actPrint.Enabled := Value;
end;

procedure TMegaBrowserFrm.SetShowDataExportTools(Value: Boolean);
begin
  actExportExcel.Visible := Value;
  actExportExcel.Enabled := Value;
  actExportCsv.Visible := Value;
  actExportCsv.Enabled := Value;
end;

procedure TMegaBrowserFrm.SetShowNavigation(Value: Boolean);
begin
  AddToAlignment.Visible := Value;
  actPrev.Visible := Value;
  actPrev.Enabled := False;
  actReload.Visible := Value;
  actNext.Visible := Value;
  actNext.Enabled := False;
  edAddress.Visible := Value;
  NavigateMenu.Visible := Value;
  NavigateMenu.Enabled := Value;
end;

function TMegaBrowserFrm.FetchSeqsFromBlast: boolean;
var
  filename: String;
  callAt: TDateTime;
  {$IFDEF VISUAL_BUILD}
  PleaseWait: TPleaseWait = nil;
  {$ENDIF}
  seqDumpList: TStringList = nil;
  IsValidData, accessable: Boolean;
  jsMessage: String;
  aSeq: TSequence = nil;
begin
  {$IFDEF VISUAL_BUILD}
  result := false;
  filename := getTemp + '\seqdump.txt';

  try
    if FileExists(filename) then
      deleteFile(filename);
  except
    filename := NextAvailableFilename(GetTemp + '\seqdump.txt');
  end;

  // Get current time.  If download doesn't start in < 15 seconds, we error out.
  callAt := now;

  blastSeqsBtnClicked := true;
  SpeedButton1.Enabled := false; // Disable while downloading.
  PleaseWait := TPleaseWait.Create(Self);

  try
    PleaseWait.Caption     := 'Add to AlnExplorer';
    PleaseWait.Action      := 'Searching for selected sequences...';
    PleaseWait.PercentDone := 0;
    PleaseWait.Show;
    PleaseWait.PercentDone := 1;
    Application.ProcessMessages;

    if activeBrowser = nil then
      Exit;
    activeBrowser.MainFrame.VisitDomProc(@FetchSeqsFromBlastRes);

    PleaseWait.PercentDone := 10;
    Application.ProcessMessages;

    // Wait for the javascript to initiate the download.
    while (not FileExists(filename)) and (SecondsBetween(Now, callAt) < 15) do
      application.ProcessMessages;

    PleaseWait.PercentDone := 20;
    Application.ProcessMessages;

    // Get current time.  If download doesn't finish in < 20 seconds, we error out.
    callAt := now;

    if FileExists(filename) then
    begin
      seqDumpList := TStringList.Create;
      // Try to access the file.  Can't access while being written
      accessable := false;
      while not accessable do
      begin
        try
          if SecondsBetween(Now, callAt) > 20 then
          begin
            jsMessage := ('An error occured when MEGA attempted to open the sequences file.' +#10#13 + 'Please try again, or download and import manually.');
            ShowJSMessageDialog(jsMessage);
            exit;
          end;
          case secondsBetween(now, callAt) mod 4 of
            0: PleaseWait.Action := 'Downloading selected sequences... -';
            1: PleaseWait.Action := 'Downloading selected sequences... /';
            2: PleaseWait.Action := 'Downloading selected sequences... -';
            3: PleaseWait.Action := 'Downloading selected sequences... \';
          end;

          Application.processMessages;
          seqDumpList.LoadFromFile(filename);
        Except
          on E: Exception do
          begin
            {$IFDEF DEBUG}
            ShowMessage('Error in ChromiumBroser.FetchSeqsFromBlast: ' + E.Message);
            {$ENDIF}
            continue;
          end;
        end;
        accessable := true;
      end;
      PleaseWait.PercentDone := 50;
      Application.ProcessMessages;
      try
        if Assigned(FImportController) then
          FreeAndNil(FImportController);
        FImportController := TBrowserImportController.Create;
        IsValidData := FImportController.ValidateData(seqDumpList);
        PleaseWait.PercentDone := 70;
        Application.ProcessMessages;
        if IsValidData then
        begin
          PleaseWait.Action := 'Importing to Alignment Editor...';
          Application.processMessages;
          if FImportController.WebDataFormat = wsdfFasta then
            FImportController.ExportFastaToAlignEditor(seqDumpList)
          else
          begin
            FImportController.RawSequenceData := seqDumpList;
            if FImportController.HasCDSs then
              PromptForCDSsToUse
            else
            begin
              aSeq := FImportController.GetFirstGenbankSequence;
              PromptForSeqNameOptions(aSeq);
            end;
          end;
          result := true;
        end
        else
        begin
          {ShowMessage('Didn''t find any valid sequences. '+
                        'Please make sure that you have checked some taxa to download. ');}
        end;
        PleaseWait.PercentDone := 99;
        Application.ProcessMessages;
      finally
        if Assigned(seqDumpList) then
          seqDumpList.Free;
      end;
    end;
  finally
    if Assigned(aSeq) then
      aSeq.Free;
    if Assigned(PleaseWait) then
      FreeAndNil(PleaseWait);
    SpeedButton1.Enabled := true;
  end;
  {$ENDIF VISUAL_BUILD}
end;



function TMegaBrowserFrm.GetBrowserTabCount : integer;
var
  i : integer;
begin
  Result := 0;
  i := pred(MegaBrowserPanel1.TabCount);

  while (i > 0) do
    begin
      // Only count the fully initialized browser tabs and not the one waiting to be used.
      if MegaBrowserPanel1.TabAtIndex(i).Initialized then
        inc(Result);
      dec(i);
    end;
end;

procedure TMegaBrowserFrm.InitialTab(aUrl: ustring = '');
var
newBrowserTab: TBrowserTab = nil;
url: ustring = MEGA_WEBSITE_URL;
begin
  if Trim(aUrl) <> EmptyStr then
    url := aUrl;

  newBrowserTab := MegaBrowserPanel1.CreateNewTab(url);

  URLChange(self, url);
  TitleChange(self, newBrowserTab.TabData.TabCaption);

  Invalidate;
end;

procedure TMegaBrowserFrm.NewTab(aUrl: ustring = '');
var
  url: ustring = MEGA_WEBSITE_URL;
  newBrowserTab: TBrowserTab = nil;
begin
  if Trim(aUrl) <> EmptyStr then
    url := aUrl;
  newBrowserTab := MegaBrowserPanel1.CreateNewTab(url);
  URLChange(self, url);
  TitleChange(self, newBrowserTab.TabData.TabCaption);
  Invalidate;
end;

procedure TMegaBrowserFrm.HideBrowserSpecificTools;
begin
  actNewTab.Enabled := False;
  actNewTab.Visible := False;
  actCloseTab.Enabled := False;
  actCloseTab.Visible := False;
  TabToolsItemSpacer.Visible := False;
  TabToolsItemSpacer.Enabled := False;
end;

{$IFDEF VISUAL_BUILD}
procedure TMegaBrowserFrm.SetMatrixExport(MatrixExport: TMatrixExport);
begin
  FMatrixExport := MatrixExport;
end;

procedure TMegaBrowserFrm.DisplayImage(imageFile: String);
begin
  CaptionImage.Picture.LoadFromFile(imageFile);
  CaptionImage.Width := CaptionImage.Picture.Width;
  CaptionImage.Height := CaptionImage.Picture.Height;
  ImagePanel.Height := Min(CaptionImage.Height + 20, 500);
  ImagePanel.Visible := True;
  ImageSplitter.Visible := True;
  SaveImageToFileAction.Visible := True;
end;

procedure TMegaBrowserFrm.SetTargetDimensions(aClientWidth: Integer; aClientHeight: Integer);
var
  scalingFactor: Double = 1;
begin
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;
  ClientWidth := Round(aClientWidth*scalingFactor);
  ClientHeight := Round(aClientHeight*scalingFactor);
  Invalidate;
end;

{$ENDIF}

procedure TMegaBrowserFrm.GoToUrl(Url: String; tabName: String = '');
begin
  if Assigned(activeTabChromium) then
  begin
    Self.activeTabChromium.StopLoad;
    Self.activeTabChromium.LoadURL(Url);
    if tabName <> EmptyStr then
      MegaBrowserPanel1.ActiveTab.TabData.TabCaption := tabName;
  end
  else
  begin
    Self.NewTab(Url);
    if tabName <> EmptyStr then
      MegaBrowserPanel1.ActiveTab.TabData.TabCaption := tabName;
  end;
end;

procedure TMegaBrowserFrm.QueryGene(isDNA: Boolean);
var
  aUrl: String;
begin
  QUERYURL := 'https://www.ncbi.nlm.nih.gov/';
  if isDNA then
    aUrl := QUERYURL+'nucleotide'
  else
    aUrl := QUERYURL+'protein';
  LoadPlaceholder(aUrl);
end;

procedure TMegaBrowserFrm.BLAST(sequence: string; isDNA: boolean);
var
  aUrl: String;
begin
  LoadLinkData;

  BLASTSequence := sequence;
  FillBlastFormCheck := true;
  if isDNA then
    aUrl := BLASTnURL + '&QUERY=' + sequence
  else
    aUrl := BLASTpURL + '&QUERY=' + sequence;
  LoadPlaceholder(aUrl);
end;

procedure TMegaBrowserFrm.ShowGene(gi: string; isDNA: boolean);
var
  aUrl: String;
begin
  if isDNA then
    aUrl := QUERYURL +'?cmd=Retrieve&db=' + 'nucleotide&list_uids='+gi+'&dopt=GenBank'
  else
    aUrl := QUERYURL+'?cmd=Retrieve&db=protein&list_uids='+gi+'&dopt=GenPept';
  LoadPlaceholder(aUrl);
end;

procedure TMegaBrowserFrm.LoadSequencesFromHtml(HtmlStrings: TStringList);
var
  {$IFDEF VISUAL_BUILD}
  PleaseWait : TPleaseWait = nil;
  {$ENDIF}
  MyInnerText : TStringList = nil;
  IsValidData: boolean;
  jsMessage: String;
  aSeq: TSequence = nil;
begin
  {$IFDEF VISUAL_BUILD}
  try
    PleaseWait := TPleaseWait.Create(MegaBrowserFrm);
    PleaseWait.Caption     := 'Add to AlnExplorer';
    PleaseWait.Action      := 'Exporting sequences to AlnExplorer...';
    PleaseWait.PercentDone := 0;
    PleaseWait.Show;
    Application.ProcessMessages;
    MyInnerText := HtmlStrings;
    if Assigned(FImportController) then
      FreeAndNil(FImportController);
    FImportController := TBrowserImportController.Create;
    IsValidData := FImportController.ValidateData(MyInnerText);
    if IsValidData then
    begin
      if FImportController.WebDataFormat = wsdfFasta then
        FImportController.ExportFastaToAlignEditor(MyInnerText)
      else
      begin
        FImportController.RawSequenceData := MyInnerText;
        if FImportController.HasCDSs then
          PromptForCDSsToUse
        else
        begin
          aSeq := FImportController.GetFirstGenbankSequence;
          PromptForSeqNameOptions(aSeq);
        end;
      end;
    end
    else
    begin
      jsMessage := ('Unable to find sequence data in the web document. '+
                    'Please make sure that you display the sequence data '+
                    'in FASTA(default) or GenBank format. '+
                    'If you are using NCBI, try "Send to Text" before fetching data.');
      ShowJSMessageDialog(jsMessage);
      exit;
    end;
  finally
   if assigned(MyInnerText) then
     MyInnerText.Free;
   if Assigned(aSeq) then
     aSeq.Free;
   if Assigned(PleaseWait) then
     PleaseWait.Free;
  end;
  {$ENDIF VISUAL_BUILD}
end;

procedure TMegaBrowserFrm.LoadHtmlFromString(Html: String);
begin
  try
    FHtml := Html;
    if not assigned(activeTabChromium) then
    begin
       InitialTab(CefGetDataURI(FHtml,'text/html'));
    end
    else
    begin
        FDestinationUrl := CefGetDataURI(FHtml,'text/html');
        activeTabChromium.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_PLACEHOLDERLOADED));
    end;
    {$IFDEF DEBUG}
    StatusBar.Visible := True;
    StatusBar.Panels[0].Text := 'Html Loaded'
    {$ENDIF}
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaBrowserFrm.RunInCaptionMode;
begin
  FBrowserMode := bmCaption;
  MegaBrowserPanel1.Tabs.Visible := False;
  SetShowNavigation(False);
  SetShowCaptionTools(True);
  SetShowDataExportTools(False);
  SetWindowSize;
  actNewWindow.Visible := False;
  actNewWindow.Enabled := False;
  actMegaWebsite.Visible := False;
  actMegaWebsite.Enabled := False;
  ToolButton1.Visible := False;
  actCut.Visible := False;
  actCut.Enabled := False;
  actPaste.Visible := False;
  actPaste.Enabled := False;
  StatusBar.Visible := False;
  HideBrowserSpecificTools;
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := False;
end;

procedure TMegaBrowserFrm.RunInExcelExportMode;
begin
  FBrowserMode := bmResults;
  MegaBrowserPanel1.Tabs.Visible := False;
  SetShowNavigation(False);
  SetShowCaptionTools(True);
  SetShowDataExportTools(True);
  SetWindowSize;
  actNewWindow.Visible := False;
  actNewWindow.Enabled := False;
  actMegaWebSite.Visible := False;
  actMegaWebSite.Enabled := False;
  ToolButton1.Visible := False;
  actCut.Visible := False;
  actCut.Enabled := False;
  actPaste.Visible := False;
  actPaste.Enabled := False;
  StatusBar.Visible := False;
  ExportToolsSpacer.Visible := True;
  ExportOptionsSpacer.Visible := True;
  AddToAlignmentSpacer.Visible := False;
  HideBrowserSpecificTools;
end;

procedure TMegaBrowserFrm.RunInWebBrowserMode;
begin
  FBrowserMode := bmBrowser;
  MegaBrowserPanel1.Tabs.Visible := True;
  SetShowNavigation(True);
  SetShowCaptionTools(False);
  SetShowDataExportTools(False);

  StatusBar.Visible := True;
  actNewWindow.Visible := True;
  actNewWindow.Enabled := True;
  actMegaWebSite.Visible := True;
  actMegaWebSite.Enabled := True;
  ToolButton1.Visible := True;
  NavigateMenu.Visible := True;
  NavigateMenu.Enabled := True;
  actFindText.Visible := True;
  actFindText.Enabled := True;
  actCut.Visible := True;
  actCut.Enabled := True;
  actPaste.Visible := True;
  actPaste.Enabled := True;
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := True;
end;

procedure TMegaBrowserFrm.RunInDefaultWebBrowserMode;
begin
  RunInWebBrowserMode;

  FBrowserMode := bmDefaultBrowser;
  AddToAlignmentSpacer.Visible := False;
  AddToAlignment.Visible := False;
  ContentsMenu.Enabled := False;
  ExportJsonMenu.Enabled := False;
  LoadHTMLMenu.Enabled := False;
  AddJSONtoHTMLMenu.Enabled := False;
end;

procedure TMegaBrowserFrm.RunInHelpMode;
begin
  FBrowserMode := bmHelpBrowser;
  MegaBrowserPanel1.Tabs.Visible := False;
  SetShowNavigation(True);
  SetShowCaptionTools(False);
  SetShowDataExportTools(False);
  SetWindowSize;
  actNewWindow.Visible := False;
  actNewWindow.Enabled := False;
  actMegaWebsite.Visible := False;
  actMegaWebsite.Enabled := False;
  actCut.Visible := False;
  actCut.Enabled := False;
  actPaste.Visible := False;
  actPaste.Enabled := False;
  StatusBar.Visible := True;
  ToggleToolbar.Visible := False;
  {$IFNDEF DEBUG}
  StatusBar.Visible := False;
  {$ENDIF}
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := False;
  Caption := VER_MEGA_MAJOR + ' Help';
  if Assigned(MegaBrowserPanel1.ActiveTab) then
    MegaBrowserPanel1.ActiveTab.TabData.TabCaption := 'MEGA Help';
  AddToAlignment.Visible := False;
  AddToAlignment.Enabled := False;
  edAddress.Width := Toolbar1.Width - ToolButton1.Width - ToolButton2.Width - ToolButton3.Width - 20;
  HelpItem.Visible := False;
  HelpItem.Enabled := False;
  HideBrowserSpecificTools;
end;

procedure TMegaBrowserFrm.RunInFileViewerMode;
begin
  FBrowserMode := bmFileViewer;
  MegaBrowserPanel1.Tabs.Visible := False;
  SetShowNavigation(True);
  SetShowCaptionTools(False);
  SetShowDataExportTools(False);
  SetWindowSize;
  actNewWindow.Visible := False;
  actNewWindow.Enabled := False;
  actMegaWebsite.Visible := False;
  actMegaWebsite.Enabled := False;
  actCut.Visible := False;
  actCut.Enabled := False;
  actPaste.Visible := False;
  actPaste.Enabled := False;
  StatusBar.Visible := True;
  ToggleToolbar.Visible := False;
  ActionAnalysisSummary.Visible := False;
  ActionAnalysisSummary.Enabled := False;
  actFindText.Visible := False;
  actFindText.Enabled := False;
  actSelectAll.Visible := False;
  actSelectAll.Enabled := False;
  {$IFNDEF DEBUG}
  StatusBar.Visible := False;
  {$ENDIF}
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := False;
  Caption := VER_MEGA_MAJOR + ' File Viewer';
  if Assigned(MegaBrowserPanel1.ActiveTab) then
    MegaBrowserPanel1.ActiveTab.TabData.TabCaption := 'File Viewer';
  AddToAlignment.Visible := False;
  AddToAlignment.Enabled := False;
  edAddress.Width := Toolbar1.Width - ToolButton1.Width - ToolButton2.Width - ToolButton3.Width - 20;
  edAddress.Enabled := False;
  HelpItem.Visible := False;
  HelpItem.Enabled := False;
  HideBrowserSpecificTools;
end;

procedure TMegaBrowserFrm.SetWindowSize;
begin
  Width := Round(Screen.Width*0.5);
  Height := Round(Screen.Height*0.7);
end;

procedure TMegaBrowserFrm.LoadPlaceholder(destinationUrl: String);
var
  aChrm: TChromium;
begin
  if not Assigned(activeTabChromium) then
     InitialTab(destinationUrl)
  else
  begin
    FDestinationUrl := destinationUrl;
    activeTabChromium.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_PLACEHOLDERLOADED));
  end;
  //LoadHtmlFromString(HtmlPlaceholderString);
  //FDestinationUrl := destinationUrl;
  //////use the message loop to load destination url
  //aChrm := activeTabChromium;
  //if Assigned(aChrm) then
  //try
  //   aChrm.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_PLACEHOLDERLOADED));
  //   Log('visitdomproc-fetchseqsfromhtml message sent')
  //except
  //  on E:Exception do
  //  Log('failed to process Add To Alignment command');
  //end;
end;

//mwebbrowser.pas ends here

procedure TMegaBrowserFrm.crmProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser;const frame:ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  i, x, y: Integer;
  temp: ustring;
  OutString: WideString;
  aList : TStringList = nil;
  jsMessage: String;
  aSeq: TSequence = nil;
begin
  Log('crmProcessMessageReceived');
  try
    if (message.Name = VISITDOMPROC_FETCHSEQSFROMHTML_COMPLETE) then
    begin
      tryNCBIDownload := message.ArgumentList.GetBool(0);
      if tryNCBIDownload then
        if not FetchSeqsFromBlast then
        begin
          jsMessage := 'Unable to find sequence data in the web document. '+
                        'Please make sure that you display the sequence data '+
                        'in FASTA/GenBank format. '+
                        'If you are using NCBI Blast, check the sequences you want to download first.';
          ShowJSMessageDialog(jsMessage);
        end;
    end
    else if (message.Name = VISITDOMPROC_FETCHSEQSFROMHTML_EXPORT) then
    begin
      aList := TStringList.Create;
      aList.Text := Trim(message.ArgumentList.GetString(0));
      if Assigned(FImportController) then
        FreeAndNil(FImportController);
      FImportController := TBrowserImportController.Create;
      if FImportController.ValidateData(aList) then
      begin
        if FImportController.WebDataFormat = wsdfFasta then
          FImportController.ExportFastaToAlignEditor(aList)
        else
        begin
          FImportController.RawSequenceData := aList;
          if FImportController.HasCDSs then
            PromptForCDSsToUse
          else
          begin
            aSeq := FImportController.GetFirstGenbankSequence;
            PromptForSeqNameOptions(aSeq);
          end;
        end;
      end
      else
      begin
        jsMessage := 'Unable to find sequence data in the web document. '+
                    'Please make sure that you display the sequence data '+
                    'in FASTA/GenBank format. '+
                    'If you are using NCBI Blast, check the sequences you want to download first.';
        ShowJSMessageDialog(jsMessage);
      end;
    end
    else if message.Name = VISITDOMPROC_COPYALLTOCLIPBOARD_COMPLETE then
    begin
      temp := Trim(message.ArgumentList.GetString(0));
      if (FBrowserMode = bmCaption) or (FBrowserMode = bmResults) then
        OutString := CleanupMegaCaptionText(temp)
      else
        OutString := MinimizeWhiteSpace(temp);
      ClipBoard.AsText := OutString;
      //ClipBoard.SetTextBuf(PChar(OutString));
    end
    else if message.Name = VISITDOMPROC_SAVECAPTIONTOTEXT_COMPLETE then
    begin
      aList := TStringList.Create;
      if (FBrowserMode = bmCaption) or (FBrowserMode = bmResults) then
        aList.Text := CleanupMegaCaptionText(message.ArgumentList.GetString(0))
      else
        aList.Text := MinimizeWhiteSpace(message.ArgumentList.GetString(0));
      try
        SaveDialog.Filter := 'Text Files|*.txt|All Files|*.*';
        SaveDialog.DefaultExt := 'txt';
        SaveDialog.InitialDir := GetCurrentDir;
        SaveDialog.Filename := 'Caption.txt';
        if SaveDialog.Execute and (SaveDialog.FileName <> '') then
          aList.SaveToFile(SaveDialog.FileName);
      except
        on E:Exception do
          ShowMessage('Oh no! An error occurred when saving the caption text to a file: ' + E.Message);
      end;
    end
    else if message.Name = VISITDOMPROC_MACSAVECAPTIONTOTEXT_COMPLETE then
    begin
      temp := message.ArgumentList.GetString(0);
      if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
        raise Exception.Create(temp);
      aList := TStringList.Create;
      aList.Text := ClipBoard.AsText;
      try
        aList.SaveToFile(temp);
        if FileExists(temp) then
          OpenFileAndFocus(temp, 0, 0);
      except
        on E:Exception do
          ShowMessage('Oh no! An error occurred when saving the caption text to a file: ' + E.Message);
      end;
    end
    else if message.Name = VISITDOMPROC_PRINTCAPTION_COMPLETE then
    begin
      try
        try
          aList := TStringList.Create;
          aList.Text := Trim(message.ArgumentList.GetString(0));
          Printer.BeginDoc;
          X := 200;
          Y := 200;
          for i := 0 to aList.Count - 1 do
          begin
            if (not Printer.Aborted) then
            begin
              Printer.Canvas.TextOut(X, Y, aList[i]);
              Y := Y + 80;
              if (Y > (Printer.PageHeight - 300)) then
              begin
                Y := 200;
                Printer.NewPage;
                Sleep(500);  // to give the user time to abort!
              end;
            end;
          end;
        except
          on E:Exception do
            ShowMessage('Oh no! Printing failed because of an error: ' + E.Message);
        end;
      finally
        if (not Printer.Aborted) then
          Printer.EndDoc;
      end;
    end
    else if message.Name = VISITDOMPROC_DOMVISITORCALLBACK_COMPLETE then
    begin

    end
    else if message.Name = VISITDOMPROC_PLACEHOLDERLOADED_COMPLETE then
    begin
      edAddress.Text := FDestinationUrl;
      edAddress.Invalidate;
      activeTabChromium.LoadURL(FDestinationUrl);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(aSeq) then
      aSeq.Free;
  end;
end;

procedure TMegaBrowserFrm.SendCompMessage(aMsg : cardinal; aData: PtrInt);
begin
  case aMsg of
    CEF_INITIALIZED : Application.QueueAsyncCall(BrowserInitializedMsg, aData);
  end;
end;

procedure TMegaBrowserFrm.BrowserInitializedMsg(Data: PtrInt);
begin
  // for MEGA browser, nothing needs be done here
end;

procedure TMegaBrowserFrm.OnTabClose(Sender: TObject; const TabCount: integer);
begin
  if (TabCount = 1) then
     Hide;
end;

{$IFDEF UNIX}
procedure TMegaBrowserFrm.WMMove(var Message: TLMMove);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

procedure TMegaBrowserFrm.WMSize(var Message: TLMSize);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

procedure TMegaBrowserFrm.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;
{$ENDIF}

procedure TMegaBrowserFrm.NotifyMoveOrResizeStarted;
var
  i : integer;
begin
  i := 0;
  while (i < MegaBrowserPanel1.TabCount) do
    begin
      MegaBrowserPanel1.TabAtIndex(i).NotifyMoveOrResizeStarted;
      inc(i);
    end;
end;

{$IFDEF WINDOWS}
procedure TMegaBrowserFrm.WMMove(var aMessage : TWMMove);
var
  i : integer = 0;
begin
  inherited;

  while (i < MegaBrowserPanel1.TabCount) do
    begin
      MegaBrowserPanel1.TabAtIndex(i).NotifyMoveOrResizeStarted;
      inc(i);
    end;
end;

procedure TMegaBrowserFrm.WMMoving(var aMessage : TMessage);
var
  i : integer = 0;
begin
  inherited;

  while (i < MegaBrowserPanel1.TabCount) do
    begin
      MegaBrowserPanel1.TabAtIndex(i).NotifyMoveOrResizeStarted;
      inc(i);
    end;
end;

procedure TMegaBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMegaBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

procedure TMegaBrowserFrm.WMQueryEndSession(var aMessage: TWMQueryEndSession);
begin
  // We return False (0) to close the browser correctly while we can.
  // This is not what Microsoft recommends doing when an application receives
  // WM_QUERYENDSESSION but at least we avoid TApplication calling HALT when
  // it receives WM_ENDSESSION.
  // The CEF subprocesses may receive WM_QUERYENDSESSION and WM_ENDSESSION
  // before the main process and they may crash before closing the main form.
  aMessage.Result := 0;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TMegaBrowserFrm.WMDpiChanged(var Message: TMessage);
begin
  inherited;

  if (GlobalCEFApp <> nil) then
    GlobalCEFApp.UpdateDeviceScaleFactor;

  MegaBrowserPanel1.UpdateDPI;
end;
{$ENDIF}


// https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelector
function TMegaBrowserFrm.QuerySelector(aNodeID : integer; const aSelector : string) : integer;
var
  TempParams : ICefDictionaryValue;
begin
  Result := 0;

  try
    if (length(aSelector) > 0) then
      begin
        TempParams := TCefDictionaryValueRef.New;
        TempParams.SetInt('nodeId', aNodeID);
        TempParams.SetString('selector', aSelector);
        Result := activeTabChromium.ExecuteDevToolsMethod(0, 'DOM.querySelector', TempParams);
      end;
  finally
    TempParams := nil;
  end;
end;

// https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-setAttributeValue
function TMegaBrowserFrm.SetAttributeValue(aNodeID : integer; const aName, aValue : string) : integer;
var
  TempParams : ICefDictionaryValue;
begin
  Result := 0;
  try
    if (aNodeID <> 0) then
      begin
        TempParams := TCefDictionaryValueRef.New;
        TempParams.SetInt('nodeId', aNodeID);
        TempParams.SetString('name', aName);
        TempParams.SetString('value', aValue);
        Result := activeTabChromium.ExecuteDevToolsMethod(0, 'DOM.setAttributeValue', TempParams);
      end;
  finally
    TempParams := nil;
  end;
end;

end.
