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

unit MWebBrowser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, StdCtrls, Buttons, ExtCtrls, {$IFDEF VISUAL_BUILD}MegaUtils,
  MMatrixExport, MegaConsts, cef3lcl, cef3types, cef3intf, cef3gui, cef3own, htmloptionsdlg,
  MBrowserImportController,{$ENDIF} Types, LCLType, mimageform, MD_Sequences;

{$IFDEF VISUAL_BUILD}
const
  CLIENT_ID_TAB_COPY       = MENU_ID_USER_FIRST + 0;
  CLIENT_ID_WINDOW_COPY    = MENU_ID_USER_FIRST + 1;
  CLIENT_ID_CLIPBOARD_COPY = MENU_ID_USER_FIRST + 2;
{$ENDIF}

type
  TBrowserMode = (bmBrowser, bmCaption, bmResults, bmHelpBrowser, bmDefaultBrowser);

  { TTabComponents }

  TTabComponents = class(TObject)
    private
      {$IFNDEF NO_BROWSER}
      FChromium: TChromium;
      FDevTools: TChromium;
      {$ENDIF}
      FSplitter: TSplitter;
      FTabIndex: Integer;
      function GetBrowser: ICefBrowser;
    public
      {$IFNDEF NO_BROWSER}
      constructor Create(aIndex: Integer; aChr, aDev: TChromium; aSpl: TSplitter);
      {$ENDIF}
      destructor Destroy; override;
      property TabIndex: Integer read FTabIndex;
      {$IFNDEF NO_BROWSER}
      property Chromium: TChromium read FChromium;
      property DevTools: TChromium read FDevTools;
      {$ENDIF}
      property Splitter: TSplitter read FSplitter;
      property Browser: ICefBrowser read GetBrowser;
  end;

  { TChromiumBrowser }

  TChromiumBrowser = class(TForm)
    actExportExcel: TAction;
    actExportCsv: TAction;
    actCopyToClipboard: TAction;
    actCloseTab: TAction;
    actDisplayHelpContents: TAction;
    actCut: TAction;
    actCopy: TAction;
    actFindText: TAction;
    actDevToolsExecute: TAction;
    actExit: TAction;
    DeveloperAction: TAction;
    actStop: TAction;
    actZoomReset: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    actSelectAll: TAction;
    actPaste: TAction;
    actMegaWebsite: TAction;
    actNewWindow: TAction;
    actNewTab: TAction;
    actPrint: TAction;
    actSaveToTextFile: TAction;
    AddToAlignment: TAction;
    actReload: TAction;
    actNext: TAction;
    ActionList1: TActionList;
    actPrev: TAction;
    {$IFNDEF NO_BROWSER}
    crm: TChromium;
    debug: TChromium;
    {$ENDIF}
    devToolsBtn: TButton;
    edAddress: TEdit;
    FindDialog1: TFindDialog;
    MainMenu1: TMainMenu;
    FLog: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    ExportOptionsSpacer: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem4: TMenuItem;
    Search1: TMenuItem;
    printBtn: TToolButton;
    Website1: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    ToggleToolbar: TMenuItem;
    MenuItem3: TMenuItem;
    StatusBar1: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    TabToolsItemSpacer: TMenuItem;
    NavigateMenu: TMenuItem;
    HelpItem: TMenuItem;
    PageControl1: TPageControl;
    CloseTabPopup: TPopupMenu;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    AddToAlignmentSpacer: TToolButton;
    ExportToolsSpacer: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton9: TToolButton;
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
    {$IFDEF VISUAL_BUILD}
    procedure crmAddressChange(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; const url: ustring);
    procedure crmBeforeContextMenu(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; const params: ICefContextMenuParams;
      const model: ICefMenuModel);
    procedure crmBeforeDownload(Sender: TObject; const Browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure crmBeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure crmContextMenuCommand(Sender: TObject;
      const Browser: ICefBrowser; const Frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags; out Result: Boolean);
    procedure crmLoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; httpStatusCode: Integer);
    procedure crmLoadError(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
    procedure crmLoadStart(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; transitionType: TCefTransitionType);
    procedure DeveloperActionExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure ProcessJSMessage(const Browser: ICefBrowser;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure ShowJSMessageDialog(jsMessage: String);
    procedure crmProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure crmStatusMessage(Sender: TObject; const Browser: ICefBrowser;
      const value: ustring);
    procedure crmTitleChange(Sender: TObject; const Browser: ICefBrowser;
      const title: ustring);
    {$ENDIF}
    procedure edAddressClick(Sender: TObject);
    procedure edAddressKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure CopyLinkToClipboard1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure MainMenu1DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure MainMenu1MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure PageControl1ChangingTab(Sender: TObject; var AllowChange: Boolean
      );
    procedure PageControl1CloseTabClicked(Sender: TObject);
    procedure PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PageControl1ChangeTab(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure ToggleToolbarClick(Sender: TObject);
    procedure OpenLinkInNewTab1Click(Sender: TObject);
    procedure OpenLinkInNewWindow1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FPopupBrowser: ICefBrowser;
    FPopupUrl: ustring;
    FIsHandlingPopupRequest: Boolean;
    FDestinationUrl: String;
    FBlastBaseUrl: String;
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
    FTabComponents: TList;
    {$IFDEF VISUAL_BUILD}
    procedure InitBrowserSettings(var Settings: TCefBrowserSettings);
    procedure InitWindowInfo(var info: TCefWindowInfo);
    {$ENDIF}
    procedure PromptForCDSsToUse;
    procedure PromptForSeqNameOptions(aSeq: TSequence);
    procedure ProcessCDSsOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessSeqNameOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ClearTabComponents;
    procedure Log(aMsg: String);
    procedure EnableLinkMenus(AreEnabled: Boolean);
    procedure LoadLinkData;
    procedure UpdateLinkData;
    procedure UpdateUserLinkData;
    procedure SetShowCaptionTools(Value: Boolean);
    procedure SetShowDataExportTools(Value: Boolean);
    procedure SetShowNavigation(Value: Boolean);
    function FetchSeqsFromBlast: boolean;
    procedure NewTab(aUrl: String = '');// overload;
    procedure LoadPopupUrl(url: ustring);
    procedure HideTabs;
    procedure HideBrowserSpecificTools;
    { private declarations }
  public
    QUERYURL: string;
    BLASTnURL: string;
    BLASTpURL: string;
    BLASTSequence: string;
    OnCloseNotify: TNotifyEvent;
    {$IFDEF VISUAL_BUILD}
    procedure SetMatrixExport(MatrixExport: TMatrixExport); { setup for exporting some kind of results to excel or csv}
    {$ENDIF}
    procedure GoToUrl(Url: String);
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
    {$IFNDEF NO_BROWSER}
    procedure LoadPlaceholder(destinationUrl: String);
    function activeTabChromium: TChromium;
    function activeBrowser: ICefBrowser;
    function activeTabDebugChromium: TChromium;
    function activeTabSplitter: TSplitter;
    property Chromium: TChromium read activeTabChromium;
    {$ENDIF}
    property BrowserMode: TBrowserMode read FBrowserMode write FBrowserMode;
    function GetBrowserImage: TBitmap;
  end;

  { TCefNewTabTask }

  TCefNewTabTask = class(TCefTaskOwn)
  protected
    fTargetUrl: ustring;
    procedure Execute; override;
  public
    constructor Create(targetURL: ustring); reintroduce;
  end;

function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; HideWindow: Boolean = False) : TChromiumBrowser; overload;
function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; Owner: TComponent; HideWindow: Boolean = False): TChromiumBrowser; overload;
function GetHelpBrowser(helpFile: String): TChromiumBrowser;
function ShowFileInBrowser(aFile: String; tabCaption: String): TChromiumBrowser;

{$IFNDEF NO_BROWSER}
procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
procedure FetchSeqsFromBlastRes(const doc: ICefDomDocument);
procedure CopyAllToClipboardCallback(const doc: ICefDomDocument);
procedure PrintCallback(const doc: ICefDomDocument);
procedure SaveCaptionToTextCallback(const doc: ICefDomDocument);
procedure FetchSeqsFromHTML(const doc: ICefDomDocument);
function getpath(const n: ICefDomNode): string;
procedure domvisitorcallback(const doc: ICefDomDocument);
{$ENDIF}

var
  FMain: TChromiumBrowser = nil;
  ChromiumBrowser: TChromiumBrowser;
  HelpBrowser: TChromiumBrowser;
  FileBrowser: TChromiumBrowser;
  FillBlastFormCheck: boolean;
  tryNCBIDownload: boolean;
  visitFetchSeqsFromHTMLFinished: Boolean;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  mega_main, MegaPrivateFiles, MPleaseWait, cef3ref,  cef3lib,
  cef3api, MWriteOutputDlg, MEditorForm, mshortcutshelper,
  {$ENDIF}
  dateutils, Clipbrd, Printers, MegaVerConsts,
  gettext, mrenderprocesshandler, mbrowserutils,
  MMegaWindowInfo, AlnBuilder_HC, mhelpfiles, mhelpkeywords,  LCLProc,
  StringUtils;

{$IFNDEF NO_BROWSER}

procedure FetchSeqsFromBlastRes(const doc: ICefDomDocument);
begin
    ChromiumBrowser.activeBrowser.MainFrame.ExecuteJavaScript(
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
  OutText : TStringList;
  SaveDialog: TSaveDialog;
begin
  OutText := TStringList.Create;
  OutText.Text := doc.Body.ElementInnerText;
  try
    SaveDialog := TSaveDialog.Create(nil);
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog.InitialDir);
    if SaveDialog.Execute and (SaveDialog.FileName <> '') then
      OutText.SaveToFile(SaveDialog.FileName);
  finally
    FreeAndNil(OutText);
    FreeAndNil(SaveDialog);
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
      PleaseWait := TPleaseWait.Create(ChromiumBrowser);
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
        ChromiumBrowser.activeBrowser.SendProcessMessage(PID_BROWSER, msg);

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
    ChromiumBrowser.activeBrowser.SendProcessMessage(PID_BROWSER, msg);

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
    ChromiumBrowser.activeBrowser.MainFrame.ExecuteJavaScript(
      'document.getElementById(''seq'').value='''+ChromiumBrowser.BlastSequence+''';','https://www.google.com/', 0);
  end;
end;

{$ENDIF}

function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; HideWindow: Boolean = False): TChromiumBrowser;
begin
  Result := TChromiumBrowser.Create(MegaForm);
  Result.BrowserMode := Mode;
  case Mode of
    bmBrowser:
      Result.RunInWebBrowserMode;
    bmCaption:
      Result.RunInCaptionMode;
    bmResults:
      Result.RunInExcelExportMode;
    bmDefaultBrowser:
       Result.RunInDefaultWebBrowserMode;
    else
      Assert(False); // then somebody must have added a new TBrowserMode but didn't add it here
  end;

  if not HideWindow then
  begin
    {$IFDEF VISUAL_BUILD}
    AddWindowToTray(Result);
    {$ENDIF}
    Result.Show;
  end;
end;

function CreateNewChromiumBrowserWindow(Mode: TBrowserMode; Owner: TComponent; HideWindow: Boolean = False): TChromiumBrowser;
var
  NewWindow: TChromiumBrowser;
begin
  NewWindow := TChromiumBrowser.Create(Owner);
  NewWindow.BrowserMode := Mode;
  case Mode of
    bmBrowser:
      NewWindow.RunInWebBrowserMode;
    bmCaption:
      NewWindow.RunInCaptionMode;
    bmResults:
      NewWindow.RunInExcelExportMode;
    bmDefaultBrowser:
      NewWindow.RunInDefaultWebBrowserMode;
    else
      Assert(False); // then somebody must have added a new TBrowserMode but didn't add it here
  end;

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

function GetHelpBrowser(helpFile: String): TChromiumBrowser;
var
  cleanUrl: String;
begin
  {$IFNDEF NO_BROWSER}
  try
    if not Assigned(HelpBrowser) then
    begin
      HelpBrowser := TChromiumBrowser.Create(Application);
      HelpBrowser.RunInHelpMode;
    end;
    Result := HelpBrowser;
    cleanUrl := 'file:///' + helpFile;
    {$IFDEF MSWINDOWS}
    cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
    {$ENDIF}

    AddWindowToTray(Result);
    Result.Show;
    if Result.activeBrowser <> nil then
      Result.activeTabChromium.Load(cleanUrl);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when initializing the help browser: ' + E.Message);
  end;
  {$ENDIF}
end;

function ShowFileInBrowser(aFile: String; tabCaption: String): TChromiumBrowser;
var
  cleanUrl: String;
begin
  {$IFNDEF NO_BROWSER}
  try
    if not Assigned(FileBrowser) then
    begin
      FileBrowser := TChromiumBrowser.Create(Application);
      FileBrowser.RunInHelpMode;
    end;
    Result := FileBrowser;
    Result.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Browser';
    cleanUrl := 'file:///' + aFile;
    {$IFDEF MSWINDOWS}
    cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
    {$ENDIF}

    AddWindowToTray(Result);
    Result.Show;
    if Result.activeBrowser <> nil then
    begin
      Result.activeTabChromium.Load(cleanUrl);
      Result.PageControl1.ActivePage.Caption := tabCaption;
      Result.Height := 800;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when initializing the help browser: ' + E.Message);
  end;
  {$ENDIF}
end;

procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
begin
  commandLine.AppendSwitchWithValue('proxy-auto-detect', 'true');
end;

{$R *.lfm}

{ TCefNewTabTask }

procedure TCefNewTabTask.Execute;
begin
Assert(CefCurrentlyOn(TID_UI));
if Assigned(FMain) then
  FMain.NewTab(UTF8Encode(fTargetUrl));
end;

constructor TCefNewTabTask.Create(targetURL: ustring);
begin
  inherited Create;
  fTargetURL := targetURL;
end;

{ TTabComponents }
{$IFNDEF NO_BROWSER}

function TTabComponents.GetBrowser: ICefBrowser;
begin
  if Assigned(Chromium) then
    Result := Chromium.Browser;
end;

constructor TTabComponents.Create(aIndex: Integer; aChr, aDev: TChromium; aSpl: TSplitter);
begin
  FTabIndex := aIndex;
  FChromium := aChr;
  FDevTools := aDev;
  FSplitter := aSpl;
end;
{$ENDIF}
destructor TTabComponents.Destroy;
begin
  inherited Destroy;
end;

{ TChromiumBrowser }

procedure TChromiumBrowser.actExportExcelExecute(Sender: TObject);
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

procedure TChromiumBrowser.actFindTextExecute(Sender: TObject);
begin
  FindDialog1.Options :=  FindDialog1.Options + [frDisableWholeWord];
  FindDialog1.Execute;
end;

procedure TChromiumBrowser.actMegaWebsiteExecute(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  Self.activeBrowser.MainFrame.LoadUrl(WEBSITE_URL);
  {$ENDIF}
end;

procedure TChromiumBrowser.actNewTabExecute(Sender: TObject);
begin
  NewTab;
end;

procedure TChromiumBrowser.actNewWindowExecute(Sender: TObject);
var
  newWindow: TChromiumBrowser;
begin
  newWindow := TChromiumBrowser.Create(Owner);
  newWindow.Left := Self.Left + 20;
  newWindow.Top := Self.Top + 20;
  newWindow.Show;
end;

procedure TChromiumBrowser.actNextExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if Assigned(activeTabChromium) and (activeBrowser <> nil) then
    activeBrowser.GoForward;
{$ENDIF}
end;

procedure TChromiumBrowser.actNextUpdate(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  if (not Assigned(activeTabChromium)) or (activeBrowser = nil) then
    Exit;

  if activeBrowser.CanGoForward then
    actNext.Enabled := True
  else
    actNext.Enabled := False;
  {$ENDIF}
end;

procedure TChromiumBrowser.actPasteExecute(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  if edAddress.Focused then
    edAddress.PasteFromClipboard
  else if activeBrowser.MainFrame.IsFocused then
    activeBrowser.MainFrame.Paste
  {$ENDIF}
end;

procedure TChromiumBrowser.actPrevExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  actPrev.Enabled := false;
  activeBrowser.GoBack;
  if activeBrowser.CanGoBack then
    actPrev.Enabled := true;
{$ENDIF}
end;

procedure TChromiumBrowser.actPrevUpdate(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  if (not Assigned(activeTabChromium)) or (activeTabChromium.Browser = nil) then
    Exit;

  if activeBrowser.CanGoBack then
    actPrev.Enabled := True
  else
    actPrev.Enabled := False;
  {$ENDIF}
end;

procedure TChromiumBrowser.actPrintExecute(Sender: TObject);
{$IFDEF DARWIN}
var
  jsMessage: String;
{$ENDIF}
begin
  {$IFNDEF NO_BROWSER}
  if activeTabChromium <> nil then
  begin
    try
    {$IFDEF DARWIN}
     jsMessage := 'Printing not yet implemented for macOS. Please use copy/paste to print from a document.';
     ShowJSMessageDialog(jsMessage);
     Exit;
    {$ENDIF}
      Printer.BeginDoc;
      activeTabChromium.PaintTo(Printer.Canvas, 0, 0);
    finally
      Printer.EndDoc;
    end;
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.actReloadExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if activeBrowser <> nil then
    if FLoading then
      activeBrowser.StopLoad
    else
      activeBrowser.Reload;
{$ENDIF}
end;

procedure TChromiumBrowser.actReloadUpdate(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  actReload.Enabled := (Assigned(activeTabChromium) and (activeBrowser <> nil) and (not FLoading));
  {$ENDIF}
end;

procedure TChromiumBrowser.actSaveToTextFileExecute(Sender: TObject);
var
  aStr: String;
  aList: TStringList = nil;
  {$IFDEF DARWIN}message: ICefProcessMessage;{$ENDIF}
begin
  try
    if activeTabChromium = nil then
      Exit;
    if (FBrowserMode = bmCaption) or (FBrowserMode = bmResults) then
    begin
      try
        try
          activeTabChromium.Browser.MainFrame.SelectAll;
          activeTabChromium.Browser.MainFrame.Copy;
          SaveDialog.Filter := 'Text Files|*.txt|All Files|*.*';
          SaveDialog.DefaultExt := 'txt';
          SaveDialog.InitialDir := GetCurrentDir;
          if FBrowserMode = bmCaption then
            SaveDialog.Filename := 'Caption.txt'
          else
            SaveDialog.Filename := 'Results.txt';
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
            activeTabChromium.Browser.SendProcessMessage(PID_RENDERER, message);
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
      activeBrowser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_SAVECAPTIONTOTEXT));
  except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TChromiumBrowser.actSelectAllExecute(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  if edAddress.Focused then
    edAddress.SelectAll
  else
    activeBrowser.MainFrame.SelectAll;
  {$ENDIF}
end;

procedure TChromiumBrowser.actStopExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  activeBrowser.StopLoad;
{$ENDIF}
end;

procedure TChromiumBrowser.actZoomInExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if activeBrowser <> nil then
    activeBrowser.Host.ZoomLevel := activeBrowser.Host.ZoomLevel + 0.5;
{$ENDIF}
end;

procedure TChromiumBrowser.actZoomOutExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if activeBrowser <> nil then
    activeBrowser.Host.ZoomLevel := activeBrowser.Host.ZoomLevel - 0.5;
{$ENDIF}
end;

procedure TChromiumBrowser.actZoomResetExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if activeBrowser <> nil then
    activeBrowser.Host.ZoomLevel := 0;
{$ENDIF}
end;

procedure TChromiumBrowser.AddToAlignmentExecute(Sender: TObject);
{$IFNDEF NO_BROWSER}
var
  browser: TChromium;
{$ENDIF}
begin
{$IFNDEF NO_BROWSER}
  visitFetchSeqsFromHTMLFinished := False;
  tryNCBIDownload := False;
  browser := activeTabChromium;
  if (not Assigned(browser)) or (not Assigned(browser.Browser)) then
    Exit;

  // When the VisitDomProc is complete in the render process, a message will be sent back to this process.
  // See crmProcessMessageReceived
  if browser.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_FETCHSEQSFROMHTML)) then
    Log('visitdomproc-fetchseqsfromhtml message sent')
  else
    Log('failed to process Add To Alignment command');
{$ENDIF}
end;

procedure TChromiumBrowser.AddToAlignmentUpdate(Sender: TObject);
begin
  AddToAlignment.Enabled := (not FLoading);
end;

procedure TChromiumBrowser.crmAddressChange(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const url: ustring);
begin
{$IFNDEF NO_BROWSER}
  if (browser.Host.WindowHandle = activeTabChromium.Browser.Host.WindowHandle) and ((frame = nil) or (frame.IsMain)) then
    edAddress.Text := UTF8Encode(url);
{$ENDIF}
end;

procedure TChromiumBrowser.crmBeforeContextMenu(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  Assert(CefCurrentlyOn(TID_UI));
  model.Clear;
  FLinkUrl := StatusBar.SimpleText;
  If ([CM_TYPEFLAG_PAGE, CM_TYPEFLAG_FRAME] * params.GetTypeFlags) <> [] then
  begin
    // Add seperator if the menu already contains items
    If model.GetCount > 0 then model.AddSeparator;

    model.AddItem(CLIENT_ID_TAB_COPY, '&Open Link in New Tab');
    model.AddItem(CLIENT_ID_WINDOW_COPY, '&Open Link in New Window');
    model.AddItem(CLIENT_ID_CLIPBOARD_COPY, '&Copy Link to Clipboard');
  end;
end;

procedure TChromiumBrowser.crmBeforeDownload(Sender: TObject; const Browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
var
  downloadLoc: String;
begin
  downloadLoc := EmptyStr;
{$IFNDEF NO_BROWSER}
  Log('crmBeforeDownload');
  if (downloadItem.mimeType = 'application/octet-stream') and (pos('sequence.gb', suggestedName) = 1) and (blastSeqsBtnClicked) then
  begin
    downloadLoc := GetTempDir + 'seqdump.txt';
  end
  else
  begin
    //SaveDialog.FileName := suggestedName;
    //if SaveDialog.Execute then
    //  downloadLoc := SaveDialog.FileName;
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
{$ENDIF}
end;

procedure TChromiumBrowser.crmBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; var popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean; out
  Result: Boolean);
begin
  if Trim(targetFrameName) <>  '' then { this is a workaround for cases where a form post is opened in a new tab (named frame) but post data is lost for a popup window}
  begin
    CefPostTask(TID_UI, TCefNewTabTask.Create(HTML_PLACEHOLDER_URL));
    Result := False; { allows the popup to be created so we can capture the final url on crmLoadEnd}
    FIsHandlingPopupRequest := True;

    { now, hide the popup window. It would be nice to just anchor the popup to our new tab but
      it is not possible without major refactoring of fpcef3, so we will just capture the
      finalized url with request data in crmLoadEnd }
    windowInfo.x := 10000;
    windowInfo.y := 10000;
    windowInfo.width := 1;
    windowInfo.height := 1;
  end
  else
  begin
    CefPostTask(TID_UI, TCefNewTabTask.Create(targetUrl));
    Result := True; { setting Result := True prevents a popup from being created and we just load the url into our new tab}
  end;
end;

procedure TChromiumBrowser.crmContextMenuCommand(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; out Result: Boolean);
begin
  Assert(CefCurrentlyOn(TID_UI));

  Result := True;

  case commandId of
    CLIENT_ID_TAB_COPY:       OpenLinkInNewTab1Click(Self);
    CLIENT_ID_WINDOW_COPY:    OpenLinkInNewWindow1Click(Self);
    CLIENT_ID_CLIPBOARD_COPY: CopyLinkToClipboard1Click(Self);
  else
    Result := False;
  end;
end;

procedure TChromiumBrowser.crmLoadEnd(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer);
begin
{$IFNDEF NO_BROWSER}
  Log('crmLoadEnd');
  try
    timer1.enabled := false;
    if (browser <> nil) and Assigned(activeBrowser) and (browser.Host.WindowHandle = activeBrowser.Host.WindowHandle) and ((frame = nil) or (frame.IsMain)) then
    begin
      FLoading := False;
      StatusBar.SimpleText := 'Done';
    end;
    if Browser.IsPopup and FIsHandlingPopupRequest and Frame.IsMain then
    begin
      FPopupUrl := Browser.MainFrame.Url;
      FPopupBrowser := Browser;
      LoadPopupUrl(FPopupUrl);
      if FPopupBrowser <> nil then
      begin
        if FPopupBrowser.IsLoading then
          FPopupBrowser.StopLoad;
        FPopupBrowser.GetHost.CloseBrowser(True);
        FPopupBrowser := nil;
      end;
      FIsHandlingPopupRequest := False;
    end;
  except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
{$ENDIF}
end;

procedure TChromiumBrowser.crmLoadError(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
var
  errorMsg : string;
begin
  {$IFNDEF NO_BROWSER}
  Log('crmLoadError: ' + IntToStr(Ord(errorCode)));
  if errorCode = ERR_ABORTED then
  begin
    // We manually call LoadURL, we can safely ignore Aborted Errors.
    Exit;
  end
  else if errorCode = ERR_CACHE_MISS then
  begin
    errorMsg := '<h1>How Embarrassing!</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + ' Cache Miss</p>' +
                '<p>This is most likely due to a page requiring a form submit.  Try pressing F5 or navigating to this page as you would normally.</p>';
  end
  else if errorCode = ERR_NAME_NOT_RESOLVED then
  begin
    errorMsg := '<h1>Oops!</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + ' Name Not Resolved</p>' +
                '<p>This URL could not be resolved.</p>';
  end
  else
  begin
    errorMsg := '<h1>How Embarrassing!</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + '</p>' +
                '<p>' + errorText + '</p>';
  end;
  browser.MainFrame.LoadString(errorMsg, failedUrl);
  {$ENDIF}
end;

procedure TChromiumBrowser.crmLoadStart(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; transitionType: TCefTransitionType);
begin
{$IFNDEF NO_BROWSER}
  Log('crmLoadStart');
  Timer1.Enabled := true;
  if Assigned(browser) and Assigned(activeBrowser) and (browser.Host.WindowHandle = activeBrowser.Host.WindowHandle) and ((frame = nil) or (frame.IsMain)) then
  begin
    FLoading := True;
    StatusBar.SimpleText := 'Begin Loading';
  end;
{$ENDIF}
end;

procedure TChromiumBrowser.DeveloperActionExecute(Sender: TObject);
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

procedure TChromiumBrowser.FindDialog1Find(Sender: TObject);
var
  aString: String;
  code: ustring;
  aCaseSensitive : integer;
  aBackwards : integer;
  aWrapAround : integer;
  aSearchInFrames : integer;
begin
{$IFNDEF NO_BROWSER}
  if Assigned(activeTabChromium) and (activeBrowser <> nil) then
  begin
    aCaseSensitive := 0;
    aBackwards := 1;
    aWrapAround := 1;
    aSearchInFrames := 1;
    with FindDialog1 do
    begin
      aString := FindText;
      if frDown in Options then
        aBackwards := 0;
      if frMatchCase in Options then
        aCaseSensitive := 1;
      code := 'window.find(''' + aString + ''',' + IntToStr(aCaseSensitive)
             + ',' + IntToStr(aBackwards) + ',' + IntToStr(aWrapAround)
             + ',' + IntToStr(aSearchInFrames) +')';
      activeTabChromium.Browser.MainFrame.ExecuteJavaScript(code, 'about:blank', 0);
    end;
    activeTabChromium.Invalidate;
  end;
{$ENDIF}
end;

procedure TChromiumBrowser.ProcessJSMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
begin
  if HtmlOptionsDialog.Visible then
    HtmlOptionsDialog.Hide;
end;

procedure TChromiumBrowser.ShowJSMessageDialog(jsMessage: String);
var
  js: String;
begin
  js := EmptyStr;
  HtmlOptionsDialog.HelpButtonImg.Visible:=False;
  HtmlOptionsDialog.Show;
  HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessJSMessage;
  HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_JS_MESSAGE_DIALOG;
  js := js + Format('$("#%s").append("%s");', [JS_MESSAGE, jsMessage]);
  HtmlOptionsDialog.LoadOptionsFile(wofJSMessageDialog, js, 'Message', 390, 200, HC_Edit_Menu_in_Alignment_Explorer);
end;

procedure TChromiumBrowser.crmProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  i, x, y: Integer;
  temp: ustring;
  OutString: WideString;
  aList : TStringList = nil;
  jsMessage: String;
  aSeq: TSequence = nil;
begin
  {$IFNDEF NO_BROWSER}
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
      activeTabChromium.Load(FDestinationUrl);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(aSeq) then
      aSeq.Free;
  end;
  {$ENDIF NO_BROWSER}
end;

procedure TChromiumBrowser.crmStatusMessage(Sender: TObject;
  const Browser: ICefBrowser; const value: ustring);
begin
  {$IFNDEF NO_BROWSER}
  Log('crmStatusMessage: ' + value);
  StatusBar.SimpleText := value;
  {$ENDIF}
end;

procedure TChromiumBrowser.crmTitleChange(Sender: TObject; const Browser: ICefBrowser; const title: ustring);
{$IFNDEF NO_BROWSER}
var
  acrm: TChromium;
{$ENDIF}
begin
  {$IFNDEF NO_BROWSER}
  Log('crmTitleChange: ' + title);
  acrm := activeTabChromium;
  if not Assigned(acrm) then
    Exit;
  if browser.Host.WindowHandle = acrm.Browser.Host.WindowHandle then
  begin
    if FBrowserMode = bmBrowser then
    begin
      Caption := 'MEGA Web Browser: ' + title;
      // The title change could come from another tab.  Make sure we find the tab relative to the Sender object.
      PageControl1.ActivePage.Caption := title;
    end
    else if FBrowserMode = bmHelpBrowser then
      Caption := 'MEGA Help'
    else if FBrowserMode = bmDefaultBrowser then
       Caption := 'MEGA Web Browser'
    else
      Caption := 'MEGA Caption Expert: ' + title;
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.edAddressClick(Sender: TObject);
begin
  edAddress.SelectAll;
end;

procedure TChromiumBrowser.edAddressKeyPress(Sender: TObject; var Key: char);
var
  url: String;
begin
{$IFNDEF NO_BROWSER}
  if (Key = #13) or (Key = #10) or (Key = LineEnding) then
  begin
    if Assigned(activeTabChromium) and Assigned(activeTabChromium.Browser) then
    begin
      url := Trim(edAddress.Text);
      activeTabChromium.Load(url);
    end;
  end;
{$ENDIF}
end;

procedure TChromiumBrowser.FormActivate(Sender: TObject);
begin
  FMain := Self;
  if Assigned(activeTabChromium) then
    activeTabChromium.Invalidate;
end;

procedure TChromiumBrowser.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFNDEF NO_BROWSER}
  if Assigned(OnCloseNotify) then
    OnCloseNotify(Self);
  if Assigned(crm) and Assigned(crm.Browser) then
    crm.Browser.Host.CloseBrowser(true);
  CloseAction := caFree;
  if FMain = Self then
    FMain := nil;
  {$ENDIF}
end;

procedure TChromiumBrowser.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if BrowserMode = bmHelpBrowser then
  begin
    CanClose := False;
    Self.Hide;
    Exit;
  end;
  CanClose := True;
end;

procedure TChromiumBrowser.FormCreate(Sender: TObject);
begin
  // *** IF ON LINUX YOU RUN INTO AN ISSUE OF TCHROMIUM JUMPING
  //     OFF THE WINDOW AND DISPLAYING IN A NEW ONE THE CHANGE
  //     SHOWN BELOW MAY BE NEEDED. IT IS NEEDED IN FPCEF3 3.2743
  //     AND IS SUPPOSED TO BE FIXED IN LATER VERSIONS BUT THAT
  //     IS NOT VERIFIED
  // ***

  //diff --git a/Component/cef3lcl.pas b/Component/cef3lcl.pas
  //@@ -687,6 +687,7 @@ begin
  //       fParentForm := GetParentForm(Self);
  //
  //       {$IFDEF LCLGTK2}
  //+        gtk_widget_realize(PGtkWidget(fParentForm.Handle));
  //         info.parent_window := gdk_window_xwindow(PGtkWidget(fParentForm.Handle)^.window);
  //       {$ENDIF}
  //       {$IFDEF LCLQT}



  {$IFNDEF NO_BROWSER}
  {$IFNDEF DEBUG}
  DeveloperAction.Enabled := False;
  DeveloperAction.Visible := False;
  {$ENDIF}
  FIsHandlingPopupRequest := False;
  UpdateShortcutsForMacOs(ActionList1);
  FImportController := nil;
  FTabComponents := TList.Create;
  FTabComponents.Add(TTabComponents.Create(0, crm, debug, Splitter1));
  OnCloseNotify := nil;
  FBlastBaseUrl := 'http://blast.ncbi.nlm.nih.gov';
  {$IFDEF VISUAL_BUILD}
  FMatrixExport := nil;
    {$IFDEF DARWIN}
    SpeedButton1.Caption := SpeedButton1.Caption + '  ';
    SpeedButton1.AutoSize := True;
    {$ENDIF}
  {$ENDIF}
  EnableLinkMenus(False);
  FLoading := False;
  ChromiumBrowser := Self;
  LoadLinkData;
  FillBlastFormCheck := false;
  //Links1.Visible := false;// disabled for now, it has problems and is not used often
  HelpContext :=  HC_Web_Browser;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  {$IFDEF VISUAL_BUILD}
  if IsDeveloper then
  begin
    DevToolsBtn.Visible := True;
    DevToolsBtn.Enabled := True;
  end
  else
  begin
    DevToolsBtn.Visible := False;
    DevToolsBtn.Enabled := False;
  end;
  {$ENDIF}
  {$ENDIF}
  actZoomResetExecute(Sender);
  crm.AnchorAsAlign(alClient, 0);
  crm.BringToFront;
  ImageForm.UpdateImgList(Self);
end;

procedure TChromiumBrowser.CopyLinkToClipboard1Click(Sender: TObject);
begin
  ClipBoard.AsText := FLinkUrl;
  actCopyExecute(Sender);
end;

procedure TChromiumBrowser.FormDestroy(Sender: TObject);
begin
  if Assigned(FTabComponents) then
  begin
    ClearTabComponents;
    FTabComponents.Free;
  end;
  if Assigned(FImportController) then
    FImportController.Free;
  if Assigned(FMatrixExport) then
    FMatrixExport.Free;
end;

function TChromiumBrowser.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TChromiumBrowser.FormShow(Sender: TObject);
{$IFDEF DARWIN}
var
  tc: TTabComponents;
  url: String;
{$ENDIF}
begin
  AddWindowToTray(Self);
  {$IFDEF DARWIN}
  tc := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]);
  if Assigned(tc) then
  begin
    tc.Chromium.Show;
    url := tc.Chromium.Browser.MainFrame.GetUrl;
    edAddress.Text := url;
    tc.Chromium.SetFocus;
    tc.Chromium.Invalidate;
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.MainMenu1DrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin
  MegaForm.DrawMenuItem(Sender, ACanvas, ARect, AState);
end;

procedure TChromiumBrowser.MainMenu1MeasureItem(Sender: TObject;
  ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MegaForm.MeasureMenuItem(Sender, ACanvas, AWidth, AHeight);
end;

procedure TChromiumBrowser.PageControl1CloseTabClicked(Sender: TObject);
begin
  actCloseTabExecute(Sender);
end;

procedure TChromiumBrowser.PageControl1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  screenCoords: TPoint;
begin
  screenCoords := PageControl1.ClientToScreen(MousePos);
  CloseTabPopup.Popup(screenCoords.X, screenCoords.Y);
  Handled := True;
end;

procedure TChromiumBrowser.PageControl1ChangingTab(Sender: TObject; var AllowChange: Boolean);
{$IFDEF UNIX}
var
  aChrm: TChromium = nil;
{$ENDIF}
begin
  AllowChange := True;
  {$IFDEF UNIX}
  aChrm := activeTabChromium;
  if Assigned(aChrm) then
    aChrm.Hide;
  {$ENDIF}
end;

procedure TChromiumBrowser.PageControl1ChangeTab(Sender: TObject);
{$IFDEF UNIX}
var
  url: String;
  tc : TTabComponents;
{$ENDIF}
begin
  {$IFDEF UNIX}
  tc := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]);
  if Assigned(tc) then
  begin
    tc.Chromium.Show;
    url := tc.Chromium.Browser.MainFrame.GetUrl;
    edAddress.Text := url;
    tc.Chromium.SetFocus;
    tc.Chromium.Invalidate;
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.Search1Click(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  activeBrowser.MainFrame.LoadUrl('https://www.google.com/');
{$ENDIF}
end;

procedure TChromiumBrowser.StatusBar1Click(Sender: TObject);
begin
  Statusbar1.Checked := (not Statusbar1.Checked);
  StatusBar.Visible := StatusBar1.Checked;
end;

procedure TChromiumBrowser.ToggleToolbarClick(Sender: TObject);
begin
  Self.ToggleToolBar.Checked := (not Self.ToggleToolBar.Checked);
  Toolbar1.Visible := ToggleToolBar.Checked;
end;

procedure TChromiumBrowser.OpenLinkInNewTab1Click(Sender: TObject);
var
  url: String;
begin
  {$IFNDEF NO_BROWSER}
  try
    url := Trim(FLinkUrl);
    NewTab();
    activeBrowser.MainFrame.LoadUrl(url);
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred during navigation: ' + E.Message);
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.OpenLinkInNewWindow1Click(Sender: TObject);
var
  ABrowser: TChromiumBrowser;
  url: String;
begin
  {$IFDEF VISUAL_BUILD}
  try
    ABrowser := CreateNewChromiumBrowserWindow(bmBrowser, MegaForm);
    url := Trim(FLinkUrl);
    ABrowser.GoToUrl(url);
    ABrowser.Show;
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred during navigation: ' + E.Message);
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.Timer1Timer(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  case timeframe of
    0: StatusBar.SimpleText := 'Loading';
    1: StatusBar.SimpleText := 'Loading.';
    2: StatusBar.SimpleText := 'Loading..';
    3: StatusBar.SimpleText := 'Loading...';
  end;
  timeframe := (timeframe + 1) mod 3;
{$ENDIF}
end;

procedure TChromiumBrowser.InitBrowserSettings(var Settings: TCefBrowserSettings);
var
  aCrm: TChromium;
begin
  aCrm := ChromiumBrowser.activeTabChromium;
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

procedure TChromiumBrowser.InitWindowInfo(var info: TCefWindowInfo);
var
  rect: TRect;
 {$IFDEF LINUX}aParentForm: TCustomForm;{$ENDIF}
begin
  FillChar(info, SizeOf(info), 0);
  {$IFDEF WINDOWS}
    rect := GetClientRect;

    info.style := WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
    info.x := rect.Left;
    info.y := rect.Top;
    info.width := rect.Right - rect.Left;
    info.height := rect.Bottom - rect.Top;
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

    info.x := Left;
    info.y := Top;
    info.width := Width;
    info.height := Height;
  {$ENDIF}
  {$IFDEF LCLCOCOA}
    rect := GetClientRect;

    info.parent_view := TCefWindowHandle(Handle);
    info.x := rect.Left;
    info.y := rect.Top;
    info.width := rect.Right - rect.Left;
    info.height := rect.Bottom - rect.Top;
  {$ENDIF}
end;

procedure TChromiumBrowser.ProcessCDSsOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: ustring;
  i, j : Integer;
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

procedure TChromiumBrowser.ProcessSeqNameOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
begin
  try
    FImportController.BuildSequenceListFromGenBank;
    FImportController.ProcessSeqNameOptionsMessage(Browser, message, Result);
  except
    on E:Exception do
      ShowJSMessageDialog('An error occurred when processing sequence name options: ' + E.Message);
  end;
end;

procedure TChromiumBrowser.PromptForCDSsToUse;
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

procedure TChromiumBrowser.PromptForSeqNameOptions(aSeq: TSequence);
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

procedure TChromiumBrowser.ClearTabComponents;
var
  i: Integer;
begin
  if FTabComponents.Count > 0 then
    for i := 0 to FTabComponents.Count - 1 do
      TTabComponents(FTabComponents[i]).Free;
end;

procedure TChromiumBrowser.Log(aMsg: String);
begin
  {$IFDEF DEBUG}
  FLog.Lines.Insert(0, aMsg);
  {$ENDIF}
end;

procedure TChromiumBrowser.actExportCsvExecute(Sender: TObject);
begin
  actExportExcelExecute(Sender);
end;

procedure TChromiumBrowser.actCopyToClipboardExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if activeTabChromium = nil then
    Exit;

  if (FBrowserMode = bmCaption) or (FBrowserMode = bmResults) then
  begin
    activeBrowser.MainFrame.SelectAll;
    activeBrowser.MainFrame.Copy;
    if Trim(FHtml) <> EmptyStr then
      activeBrowser.MainFrame.LoadString(FHtml, 'http://localhost/');
  end
  else
    activeBrowser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_COPYALLTOCLIPBOARD));
{$ENDIF}
end;

procedure TChromiumBrowser.actCutExecute(Sender: TObject);
begin
{$IFNDEF NO_BROWSER}
  if edAddress.Focused then
    edAddress.CutToClipboard
  else if activeBrowser.MainFrame.IsFocused then
    activeBrowser.MainFrame.Cut;
{$ENDIF}
end;

procedure TChromiumBrowser.actDevToolsExecuteExecute(Sender: TObject);
var
  info: TCefWindowInfo;
  settings: TCefBrowserSettings;
begin
{$IFNDEF NO_BROWSER}
  InitBrowserSettings(Settings);
  InitWindowInfo(info);
  Panel1.Visible := True;
  ChromiumBrowser.activeTabDebugChromium.Visible := true;
  ChromiumBrowser.activeTabSplitter.Visible := true;
  activeBrowser.Host.ShowDevTools(info, nil, settings, nil);
{$ENDIF}
end;

procedure TChromiumBrowser.actDisplayHelpContentsExecute(Sender: TObject);
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

procedure TChromiumBrowser.actExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TChromiumBrowser.actCloseTabExecute(Sender: TObject);
var
  tc: TTabComponents;
begin
  CloseTabPopup.Close;
  if PageControl1.PageCount > 1 then
  begin
    tc := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]);
    if Assigned(tc) then
      tc.Free;
    FTabComponents.Delete(PageControl1.ActivePageIndex);
    PageControl1.ActivePage.Free;
  end
  else
    actExitExecute(Sender);
end;

procedure TChromiumBrowser.actCopyExecute(Sender: TObject);
begin
  {$IFNDEF NO_BROWSER}
  if edAddress.Focused then
    edAddress.CopyToClipboard
  else if activeBrowser.MainFrame.IsFocused  then
    activeBrowser.MainFrame.Copy;
  {$ENDIF}
end;
{$IFNDEF NO_BROWSER}
function TChromiumBrowser.activeTabChromium: TChromium;
begin
  result := nil;
  if (PageControl1.PageCount > 0) and (PageControl1.ActivePageIndex < FTabComponents.Count) then
    Result := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]).Chromium;
end;

function TChromiumBrowser.activeBrowser: ICefBrowser;
begin
  Result := nil;
  if (PageControl1.PageCount > 0) and (PageControl1.ActivePageIndex < FTabComponents.Count) then
    Result := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]).Browser;
end;

{$ENDIF}
procedure TChromiumBrowser.EnableLinkMenus(AreEnabled: Boolean);
begin
  //if AreEnabled then
  //begin
  //  Openlinkinnewtab1.Enabled := True;
  //  Openlinkinnewwindow1.Enabled := True;
  //  Copylinktoclipboard1.Enabled := True;
  //end
  //else
  //begin
  //  Openlinkinnewtab1.Enabled := False;
  //  Openlinkinnewwindow1.Enabled := False;
  //  Copylinktoclipboard1.Enabled := False;
  //end;
end;

procedure TChromiumBrowser.LoadLinkData;
var
  linklist: TStringList;
  filename: string;
  //i: integer;
  //j: integer;
begin
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

procedure TChromiumBrowser.UpdateLinkData;
//var
//  linklist: TStringList = nil;
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

procedure TChromiumBrowser.UpdateUserLinkData;
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

procedure TChromiumBrowser.SetShowCaptionTools(Value: Boolean);
begin
  actSaveToTextFile.Visible := Value;
  actSaveToTextFile.Enabled := Value;
  actCopyToClipboard.Visible := Value;
  actCopyToClipboard.Enabled := Value;
  actPrint.Visible := Value;
  actPrint.Enabled := Value;
end;

procedure TChromiumBrowser.SetShowDataExportTools(Value: Boolean);
begin
  actExportExcel.Visible := Value;
  actExportExcel.Enabled := Value;
  actExportCsv.Visible := Value;
  actExportCsv.Enabled := Value;
end;

procedure TChromiumBrowser.SetShowNavigation(Value: Boolean);
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

function TChromiumBrowser.FetchSeqsFromBlast: boolean;
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
  {$IFNDEF NO_BROWSER}
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
  {$ENDIF NO_BROWSER}
  {$ENDIF VISUAL_BUILD}
end;

procedure TChromiumBrowser.NewTab(aUrl: String = '');
var
  tabSheet: TTabSheet = nil;
  url: String = 'https://www.megasoftware.net';
  {$IFNDEF NO_BROWSER}
  chrome: TChromium = nil;
  debug:TChromium = nil;
  splitter:TSplitter = nil;
  {$ENDIF}
begin
  if Trim(aUrl) <> EmptyStr then
    url := aUrl;
  {$IFNDEF NO_BROWSER}
  if (PageControl1.CanFocus) then
  begin
    tabSheet := PageControl1.AddTabSheet;
    tabSheet.Caption := 'New Tab';
    splitter := TSplitter.Create(tabSheet);
    debug := TChromium.Create(tabSheet);
    chrome := TChromium.Create(tabSheet);
    FTabComponents.Add(TTabComponents.Create(FTabComponents.Count, chrome, debug, splitter));
    chrome.OnAddressChange := crmAddressChange;
    chrome.OnBeforeDownload := crmBeforeDownload;
    chrome.OnLoadEnd := crmLoadEnd;
    chrome.OnLoadError := crmLoadError;
    chrome.OnLoadStart := crmLoadStart;
    chrome.OnStatusMessage := crmStatusMessage;
    chrome.OnTitleChange := crmTitleChange;
    chrome.OnBeforeContextMenu := crmBeforeContextMenu;
    chrome.OnContextMenuCommand:= crmContextMenuCommand;
    chrome.OnProcessMessageReceived := crmProcessMessageReceived;
    chrome.OnBeforePopup := crmBeforePopup;
    splitter.Parent := tabSheet;
    chrome.Parent := tabSheet;
    debug.Parent := tabSheet;
    chrome.left := 0;
    chrome.top := 0;
    splitter.Visible := false;
    splitter.Align := alBottom;
    debug.Visible := false;
    debug.Align := alBottom;
    edAddress.Text := url;
    PageControl1.ActivePage := tabSheet;
    chrome.Visible := true;
    chrome.Enabled := true;
    chrome.AnchorAsAlign(alClient, 0);
    if url = HTML_PLACEHOLDER_URL then
      chrome.Browser.MainFrame.LoadString(HtmlPlaceholderString, 'http://localhost/')
    else
      chrome.Load(url);
    chrome.BringToFront;
    chrome.Invalidate;
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.LoadPopupUrl(url: ustring);
var
  {$IFNDEF NO_BROWSER}
  chrome: TChromium = nil;
  {$ENDIF}
begin
  {$IFNDEF NO_BROWSER}
  if (PageControl1.CanFocus) then
  begin
    chrome := activeTabChromium;
    chrome.Load(url);
    chrome.Invalidate;
    FPopupUrl := '';
  end;
  {$ENDIF}
end;

procedure TChromiumBrowser.HideTabs;
var
  i: Integer;
begin
  if PageControl1.PageCount > 0 then
  begin
    for i := 0 to PageControl1.PageCount - 1 do
      PageControl1.Pages[i].TabVisible := False;
    PageControl1.ActivePageIndex := 0;
  end;
end;

procedure TChromiumBrowser.HideBrowserSpecificTools;
begin
  actNewTab.Enabled := False;
  actNewTab.Visible := False;
  actCloseTab.Enabled := False;
  actCloseTab.Visible := False;
  TabToolsItemSpacer.Visible := False;
  TabToolsItemSpacer.Enabled := False;
end;

{$IFNDEF NO_BROWSER}
function TChromiumBrowser.activeTabDebugChromium: TChromium;
begin
  result := nil;
  if PageControl1.PageCount > 0 then
    Result := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]).DevTools;
end;

function TChromiumBrowser.activeTabSplitter: TSplitter;
begin
  result := nil;
  if PageControl1.PageCount > 0 then
    Result := TTabComponents(FTabComponents[PageControl1.ActivePageIndex]).Splitter;
end;
{$ENDIF}
{$IFDEF VISUAL_BUILD}
procedure TChromiumBrowser.SetMatrixExport(MatrixExport: TMatrixExport);
begin
  FMatrixExport := MatrixExport;
end;
{$ENDIF}
procedure TChromiumBrowser.GoToUrl(Url: String);
begin
  {$IFNDEF NO_BROWSER}
    Self.activeBrowser.StopLoad;
    Self.activeBrowser.MainFrame.LoadUrl(Url);
  {$ENDIF}
end;

procedure TChromiumBrowser.QueryGene(isDNA: Boolean);
var
  aUrl: String;
begin
  {$IFNDEF NO_BROWSER}
  if isDNA then
    aUrl := QUERYURL+'?db=nucleotide'
  else
    aUrl := QUERYURL+'?db=protein';
  LoadPlaceholder(aUrl);
  {$ENDIF}
end;

procedure TChromiumBrowser.BLAST(sequence: string; isDNA: boolean);
var
  aUrl: String;
begin
{$IFNDEF NO_BROWSER}
  BLASTSequence := sequence;
  FillBlastFormCheck := true;
  if isDNA then
    aUrl := BLASTnURL + '&QUERY=' + sequence
  else
    aUrl := BLASTpURL + '&QUERY=' + sequence;
  LoadPlaceholder(aUrl);
{$ENDIF}
end;

procedure TChromiumBrowser.ShowGene(gi: string; isDNA: boolean);
var
  aUrl: String;
begin
  {$IFNDEF NO_BROWSER}
  if isDNA then
    aUrl := QUERYURL +'?cmd=Retrieve&db=' + 'nucleotide&list_uids='+gi+'&dopt=GenBank'
  else
    aUrl := QUERYURL+'?cmd=Retrieve&db=protein&list_uids='+gi+'&dopt=GenPept';
  LoadPlaceholder(aUrl);
  {$ENDIF}
end;

procedure TChromiumBrowser.LoadSequencesFromHtml(HtmlStrings: TStringList);
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
  {$IFNDEF NO_BROWSER}
  try
    PleaseWait := TPleaseWait.Create(ChromiumBrowser);
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
  {$ENDIF NO_BROWSER}
  {$ENDIF VISUAL_BUILD}
end;


procedure TChromiumBrowser.LoadHtmlFromString(Html: String);
begin
  try
    FHtml := Html;
    {$IFNDEF NO_BROWSER}
    activeBrowser.MainFrame.LoadString(Html, 'http://localhost/');
    {$IFDEF DEBUG}
    StatusBar.Visible := True;
    StatusBar.SimpleText := 'Html Loaded'
    {$ENDIF}
    {$ENDIF}
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TChromiumBrowser.RunInCaptionMode;
begin
  FBrowserMode := bmCaption;
  SetShowNavigation(False);
  SetShowCaptionTools(True);
  SetShowDataExportTools(False);
  Height := 500;
  Width := 700;
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
  PageControl1.ActivePage.Caption := 'Citation';
  HideBrowserSpecificTools;
end;

procedure TChromiumBrowser.RunInExcelExportMode;
begin
  FBrowserMode := bmResults;
  SetShowNavigation(False);
  SetShowCaptionTools(True);
  SetShowDataExportTools(True);
  Height := 500;
  Width := 700;
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
  PageControl1.ActivePage.Caption := 'Results';
  HideBrowserSpecificTools;
end;

procedure TChromiumBrowser.RunInWebBrowserMode;
begin
  FBrowserMode := bmBrowser;
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
  actcopy.Visible := True;
  actcopy.Enabled := True;
  actCut.Visible := True;
  actCut.Enabled := True;
  actPaste.Visible := True;
  actPaste.Enabled := True;
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := True;
end;

procedure TChromiumBrowser.RunInDefaultWebBrowserMode;
begin
  FBrowserMode := bmDefaultBrowser;
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
  actcopy.Visible := True;
  actcopy.Enabled := True;
  actCut.Visible := True;
  actCut.Enabled := True;
  actPaste.Visible := True;
  actPaste.Enabled := True;
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := False;
  AddToAlignment.Visible := False;
  actSaveToTextFile.Visible := False;
  actExportExcel.Visible := False;
  MenuItem11.Enabled := False;
  MenuItem13.Enabled := False;
  MenuItem14.Enabled := False;
  MenuItem15.Enabled := False;
end;

procedure TChromiumBrowser.RunInHelpMode;
begin
  FBrowserMode := bmHelpBrowser;
  SetShowNavigation(True);
  SetShowCaptionTools(False);
  SetShowDataExportTools(False);
  Height := 500;
  Width := 1000;
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
  StatusBar1.Visible := False;
  ExportToolsSpacer.Visible := False;
  ExportOptionsSpacer.Visible := False;
  AddToAlignmentSpacer.Visible := False;
  Caption := VER_MEGA_MAJOR + ' Help';
  PageControl1.ActivePage.Caption := 'MEGA Help';
  AddToAlignment.Visible := False;
  AddToAlignment.Enabled := False;
  edAddress.Width := Toolbar1.Width - ToolButton1.Width - ToolButton2.Width - ToolButton3.Width - 20;
  StatusBar.Visible := False;
  HelpItem.Visible := False;
  HelpItem.Enabled := False;
  HideBrowserSpecificTools;
end;

procedure TChromiumBrowser.LoadPlaceholder(destinationUrl: String);
var
  aChrm: TChromium;
begin
  LoadHtmlFromString(HtmlPlaceholderString);
  FDestinationUrl := destinationUrl;
  //use the message loop to load destination url
  aChrm := activeTabChromium;
  if Assigned(aChrm) then
    if aChrm.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_PLACEHOLDERLOADED)) then
      Log('visitdomproc-fetchseqsfromhtml message sent')
    else
      Log('failed to process Add To Alignment command');
end;

function TChromiumBrowser.GetBrowserImage: TBitmap;
var
  Temp: TBitmap;
  Cnv: TControlCanvas;
begin
{$IFNDEF NO_BROWSER}
  Temp := nil;
  Result := nil;
  try
    Temp := TBitmap.Create;
    Cnv := TControlCanvas.Create;
    Cnv.Control := self;
    Temp.Width := self.Width;
    Temp.Height:= self.Height;
    Temp.Canvas.Copyrect(Temp.Canvas.ClipRect, Cnv, Cnv.ClipRect);
    Cnv.Free;

    {if not CefGetBitmap(activeBrowser, PET_VIEW, Temp) then
    begin
      Result := nil;
      FreeAndNil(Temp);
    end
    else
    begin }
      Result := Temp;
   { end;}
  Except
    on E: Exception do
    begin
      Result := nil;
      {$IFDEF DEBUG}
       ShowMessage('Error in ChromiumBrowser.GetBrowserImage: ' + E.Message);
      {$ENDIF}
    end;
  end;
{$ENDIF}
end;

end.

