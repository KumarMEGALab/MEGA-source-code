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

unit htmloptionsdlg;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLtype, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, messages,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList, mhelpfiles, mhelpkeywords,
  Walk_Through_MEGA, strutils, MPleaseWaitDlg, mimageform, uCEFChromium,
  uCEFInterfaces, uCEFTypes, LMessages,
  uCEFConstants, uCEFChromiumCore, uCEFApplication,
  uCEFLinkedWindowParent, uCEFChromiumEvents;

type
  TProcessMessageReceivedProc = procedure(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean) of object;

  {$IFDEF VISUAL_BUILD}
  { THtmlOptionsDialog }

  THtmlOptionsDialog = class(TForm)
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    CEFWindowParent1: TCEFLinkedWindowParent;
    Chromium1: TChromium;
    DebugAction: TAction;
    ButtonsPanel: TPanel;
    ExecuteJavascript: TAction;
    ImageList2: TImageList;
    BottomPanel: TPanel;
    SaveDialog: TSaveDialog;
    OpenFolder: TSelectDirectoryDialog;
    ShowDevToolsAction: TAction;
    OpenFileAction: TAction;
    OptionsDlgStatusBar: TStatusBar;
    ToggleDevToolsAction: TAction;
    ActionList1: TActionList;
    OpenDialog: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure CancelButtonImgClick(Sender: TObject);
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure CEFWindowParent1Enter(Sender: TObject);
    procedure CEFWindowParent1Exit(Sender: TObject);
    procedure Chromium1BeforeDownload(Sender: TObject;
      const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1FileDialog(Sender: TObject; const Browser: ICefBrowser;
      mode: Cardinal; const title, defaultFilePath: ustring;
      const acceptFilters: TStrings;
      const callback: ICefFileDialogCallback; var Result: Boolean);
    procedure Chromium1LoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1LoadError(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
    procedure Chromium1LoadingStateChange(Sender: TObject;
      const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure DebugActionExecute(Sender: TObject);
    procedure ExecuteJavascriptExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function FormHelp (Command :Word ; Data :PtrInt ; var CallHelp :Boolean
      ):Boolean ;
    procedure FormHide(Sender: TObject);
    procedure FormResize (Sender :TObject );
    procedure FormShow(Sender: TObject);
    procedure HelpButtonImgClick (Sender :TObject );
    procedure HelpButtonImgDblClick (Sender :TObject );
    procedure OkButtonImgClick(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure ShowDevToolsActionExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToggleDevToolsActionExecute(Sender: TObject);
    procedure VisitDomBtnClick(Sender: TObject);
    procedure ShowBrowserMessage(aMessage: string);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser;const Frame: ICefFrame; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1StatusMessage(Sender: TObject;
      const Browser: ICefBrowser; const value: ustring);
    {$IFDEF UNIX}
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserSetFocusMsg(Data: PtrInt);
    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserShowMessageDialogMsg(Data: PtrInt);
    procedure SendCompMessage(aMsg : cardinal; Data: PtrInt = 0);
    {$ENDIF}
    {$IFDEF WINDOWS}
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    {$ENDIF}
  private
    FOptionsFile: String;
    FOkClicked: Boolean;
    FJavascript: TStringList;
    FFilename: String;
    FTempFiles: TStringList;
    FObserverCancelCallbacks: TList;
    FObserverOkCallbacks: TList;
    FPleaseWait: TPleaseWaitDlg;
    procedure PositionButtonsPanel;
    procedure CleanUpTempFiles;
    procedure ProcessFormValidationMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ClearObservers;
    procedure InitBrowserSettings(var Settings: TCefBrowserSettings);
    procedure InitWindowInfo(var info: TCefWindowInfo);
    {$IFDEF LINUX}
    procedure SendCEFKeyEvent(const aCefEvent : TCefKeyEvent);
    {$ENDIF}
  protected
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.
  public
    ProcessMessageReceivedProc: TProcessMessageReceivedProc;
    DomProcedureName: String;
    CancelCallback: TNotifyEvent;
    FirstTime : Boolean;
    AddToAlign: Boolean;
    procedure AddObserver(aCancelCallback: TNotifyEvent; aOkCallback: TNotifyEvent); { because TChromium works asynchronously, we let interested parties register as observers to be notified of events}
    procedure LoadOptionsFile(aFile: String; aCaption: String; aWidth: Integer; aHeight: Integer; aHelpContext: THelpContext = HC_First_Time_User); overload;
    procedure LoadOptionsFile(aFile: String; javascript: String; aCaption: String; aWidth: Integer; aHeight: Integer; aHelpContext: THelpContext = HC_First_Time_User); overload;
    procedure LoadPlaceholder;
    procedure ReInitializeForm;
  end;

var
  HtmlOptionsDialog: THtmlOptionsDialog;
  UploadFileName: String;
  DownloadFileName: String;
  {$ENDIF}

implementation
  {$IFDEF VISUAL_BUILD}
{$R *.lfm}

uses
  mbrowserutils, MegaUtils,
  LazFileUtils, Math, MegaConsts, mshortcutshelper,
  {$IFDEF LINUX}gdk2, gtk2, glib2, uCEFLinuxFunctions,{$ENDIF}
  uCEFProcessMessage, uCEFMiscFunctions, umegarenderer;

{$IFDEF LINUX}
function GTKKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer) : GBoolean; cdecl;
var
  TempCefEvent : TCefKeyEvent;
begin
  GdkEventKeyToCEFKeyEvent(Event, TempCefEvent);

  if (Event^._type = GDK_KEY_PRESS) then
    begin
      TempCefEvent.kind := KEYEVENT_RAWKEYDOWN;
      HtmlOptionsDialog.SendCEFKeyEvent(TempCefEvent);
      TempCefEvent.kind := KEYEVENT_CHAR;
      HtmlOptionsDialog.SendCEFKeyEvent(TempCefEvent);
    end
   else
    begin
      TempCefEvent.kind := KEYEVENT_KEYUP;
      HtmlOptionsDialog.SendCEFKeyEvent(TempCefEvent);
    end;

  Result := True;
end;

procedure ConnectKeyPressReleaseEvents(const aWidget : PGtkWidget);
begin
  g_signal_connect(aWidget, 'key-press-event',   TGTKSignalFunc(@GTKKeyPress), nil);
  g_signal_connect(aWidget, 'key-release-event', TGTKSignalFunc(@GTKKeyPress), nil);
end;
{$ENDIF}

{ THtmlOptionsDialog }

procedure THtmlOptionsDialog.BrowserDestroyMsg(var aMessage : TMessage);
begin
  FreeAndNil(CEFWindowParent1);
end;

{$IFDEF LINUX}
procedure THtmlOptionsDialog.SendCEFKeyEvent(const aCefEvent : TCefKeyEvent);
begin
  Chromium1.SendKeyEvent(@aCefEvent);
end;
{$ENDIF}

procedure THtmlOptionsDialog.CEFWindowParent1Enter(Sender: TObject);
begin
   If not(csDesigning in ComponentState) then Chromium1.SetFocus(True);
end;

procedure THtmlOptionsDialog.CEFWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure THtmlOptionsDialog.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // The main browser is being destroyed
  if (Chromium1.BrowserId = 0) then
    begin
      FCanClose := True;
      {$IFDEF WINDOWS}
      PostMessage(Handle, WM_CLOSE, 0, 0);
      {$ELSE}
	  { todomike - see if we can use SendCompMessage for Windows as well}
      SendCompMessage(CEF_BEFORECLOSE);
      {$ENDIF}
    end;
end;

procedure THtmlOptionsDialog.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  if (browser <> nil) and
     (Chromium1.BrowserId = browser.Identifier) and
     (CEFWindowParent1 <> nil) then
    begin
      CEFWindowParent1.DestroyChildWindow;
      {$IFDEF WINDOWS}
      PostMessage(Handle, CEF_DESTROY, 0, 0);
	  {$ENDIF}
	  aAction := cbaDelay;      
    end;
end;

procedure THtmlOptionsDialog.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  {$IFDEF UNIX}SendCompMessage(CEF_SETFOCUS);{$ENDIF}
end;

procedure THtmlOptionsDialog.CleanUpTempFiles;
var
  i: Integer;
  aFile: String;
begin
  if FTempFiles.Count > 0 then
    for i := 0 to FTempFiles.Count - 1 do
    begin
      aFile := FTempFiles[i];
      if FileExists(aFile) then
      begin
        try
          DeleteFile(aFile);
        except

        end;
      end;
    end;
end;

procedure THtmlOptionsDialog.ProcessFormValidationMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: String;
  isValid: Boolean;
begin
  try
    temp := message.ArgumentList.GetString(0);
    if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
      raise Exception.Create(temp);
    temp := message.ArgumentList.GetString(IS_VALIDATED_INDEX);
    isValid := StrToBool(temp);
    if not isValid then
      Exit;
    FOkClicked := True;
    //if not Chromium1.Browser.Mainframe.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(DomProcedureName)) then
    //  ShowBrowserMessage('failed to send message to renderer');
    try
      Chromium1.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(DomProcedureName));
    except
      on E:Exception do
      begin
        ShowBrowserMessage('failed to send message to renderer');
      end
    end;
  except
    on E:Exception do
    begin
      ShowBrowserMessage('Application Error: An error has occurred: ' + E.Message);
      Hide;
    end
  end;
end;

procedure THtmlOptionsDialog.ClearObservers;
var
  p: PNotifyEvent;
  i: Integer;
begin
  if FObserverOkCallbacks.Count > 0 then
    for i := 0 to FObserverOkCallbacks.Count - 1 do
    begin
      p := PNotifyEvent(FObserverOkCallbacks[i]);
      Dispose(p);
    end;
  FObserverOkCallbacks.Clear;

  if FObserverCancelCallbacks.Count > 0 then
    for i := 0 to FObserverCancelCallbacks.Count - 1 do
    begin
      p := PNotifyEvent(FObserverCancelCallbacks[i]);
      Dispose(p);
    end;
  FObserverCancelCallbacks.Clear;
end;

procedure THtmlOptionsDialog.InitBrowserSettings(var Settings: TCefBrowserSettings);
begin
  FillChar(Settings, SizeOf(settings), 0);
  Settings.size := SizeOf(TCefBrowserSettings);
  settings.standard_font_family := CefString(Chromium1.FontOptions.StandardFontFamily);
  settings.fixed_font_family := CefString(Chromium1.FontOptions.FixedFontFamily);
  settings.serif_font_family := CefString(Chromium1.FontOptions.SerifFontFamily);
  settings.sans_serif_font_family := CefString(Chromium1.FontOptions.SansSerifFontFamily);
  settings.cursive_font_family := CefString(Chromium1.FontOptions.CursiveFontFamily);
  settings.fantasy_font_family := CefString(Chromium1.FontOptions.FantasyFontFamily);
  settings.default_font_size := Chromium1.FontOptions.DefaultFontSize;
  settings.default_fixed_font_size := Chromium1.FontOptions.DefaultFixedFontSize;
  settings.minimum_font_size := Chromium1.FontOptions.MinimumFontSize;
  settings.minimum_logical_font_size := Chromium1.FontOptions.MinimumLogicalFontSize;
end;

procedure THtmlOptionsDialog.InitWindowInfo(var info: TCefWindowInfo);
var
  rect: TRect;
  cefrect: TCefRect;
  {$IFDEF LINUX}aParentForm: TCustomForm;{$ENDIF}
begin
  FillChar(info, SizeOf(info), 0);
  {$IFDEF WINDOWS}
    rect := GetClientRect;

    info.style := WS_VISIBLE or WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_TABSTOP;
    cefrect.x := rect.Left;
    cefrect.y := rect.Top;
    cefrect.width := rect.Right - rect.Left;
    cefrect.height := rect.Bottom - rect.Top;
    info.bounds := cefrect;
    info.window_name := CefString('Dev Tools');
  {$ENDIF}
  {$IFDEF LINUX}
    aParentForm := GetParentForm(Self);
    cefrect.x := Left;
    cefrect.y := Top;
    cefrect.width := Width;
    cefrect.height := Height;
    info.bounds := cefrect;
  {$ENDIF}
  {$IFDEF DARWIN}
    rect := GetClientRect;
    info.parent_view := TCefWindowHandle(Handle);
    cefrect.x := rect.Left;
    cefrect.y := rect.Top;
    cefrect.width := rect.Right - rect.Left;
    cefrect.height := rect.Bottom - rect.Top;
    info.bounds := cefrect;
  {$ENDIF}
end;

procedure THtmlOptionsDialog.AddObserver(aCancelCallback: TNotifyEvent; aOkCallback: TNotifyEvent);
var
  p: PNotifyEvent;
begin
  New(p);
  p^ := aCancelCallback;
  FObserverCancelCallbacks.Add(p);
  New(p);
  p^ := aOkCallback;
  FObserverOkCallbacks.Add(p);
end;

procedure THtmlOptionsDialog.LoadOptionsFile(aFile: String; aCaption: String; aWidth: Integer; aHeight: Integer; aHelpContext: THelpContext = HC_First_Time_User);
var
  htmlFile: String;
  cleanUrl: String;
  scaleFactor: Double = 1.0;
begin
  if PixelsPerInch > DesignTimePPI then
    scaleFactor := PixelsPerInch/DesignTimePPI;
  UploadFileName := EmptyStr;
  DownloadFileName := EmptyStr;
  {$IFNDEF MSWINDOWS}
  ReInitializeForm;
  {$ENDIF}
  OkAction.Visible := True;
  HelpAction.Visible := True;
  if not Visible then
    Show;

  HelpContext := aHelpContext;
  FFilename := ExtractFilenameOnly(aFile);
  ClientWidth := Round(aWidth*scaleFactor);
  ClientHeight := Round(aHeight*scaleFactor);
  PositionButtonsPanel;
  Caption := aCaption;
  htmlFile := GetPrivateFile(aFile, False);
  if not FileExists(htmlFile) then
  begin
    ShowMessage(Format('Application Error - an options settings file (%s) is missing from your computer. Please restart MEGA to try and resolve this issue', [aFile]));
    Exit;
  end;
  cleanUrl := 'file:///' + htmlFile;
  {$IFDEF MSWINDOWS}
  cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
  {$ENDIF}
  cleanUrl := StringReplace(cleanUrl, 'file:////', 'file:///', [rfReplaceAll]);
  FOptionsFile := cleanUrl;
  Chromium1.DefaultUrl:= cleanUrl;
  Chromium1.CreateBrowser(CEFWindowParent1, '');
  Chromium1.LoadURL(cleanUrl);
end;

procedure THtmlOptionsDialog.LoadOptionsFile(aFile: String; javascript: String; aCaption: String; aWidth: Integer; aHeight: Integer; aHelpContext: THelpContext);
var
  htmlFile, tempFile: String;
  html: String;
  aList: TStringList = nil;
  cleanUrl: String;
  scaleFactor: Double = 1.0;
begin
  if PixelsPerInch > DesignTimePPI then
    scaleFactor := PixelsPerInch/DesignTimePPI;
  {$IFNDEF MSWINDOWS}
  ReInitializeForm;
  {$ENDIF}
  OkAction.Visible := True;
  HelpAction.Visible := True;
  if not Visible then
    Show;
  HelpContext := aHelpContext;
  FFilename := ExtractFilenameOnly(aFile);
  ClientWidth := Round(aWidth*scaleFactor);
  ClientHeight := Round(aHeight*scaleFactor);
  PositionButtonsPanel;
  Caption := aCaption;
  htmlFile := GetPrivateFile(aFile, False);

  if not FileExists(htmlFile) then
    raise Exception.Create(Format('Oh no! An options settings file (%s) is missing from your computer. Please restart MEGA to try and resolve this issue', [aFile]));

  try
    try
      BeginFormUpdate;
      aList := TStringList.Create;
      aList.LoadFromFile(htmlFile);
      html := aList.Text;
      html := StringReplace(html, MEGA_JS_TEMPLATE_PLACEHOLDER, javascript, [rfIgnoreCase]);
      tempFile := NextAvailableFilename(htmlFile);
      FTempFiles.Add(tempFile);
      aList.Text := html;
      aList.SaveToFile(tempFile);
      while (not FileExists(tempFile)) or (not FileIsReadable(tempFile)) do
        Sleep(0);

      cleanUrl := 'file:///' + tempFile;
      {$IFDEF MSWINDOWS}
      cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
      {$ENDIF}
      cleanUrl := StringReplace(cleanUrl, 'file:////', 'file:///', [rfReplaceAll]);
      FOptionsFile := cleanUrl;
      Chromium1.DefaultUrl:= cleanUrl;
      Chromium1.CreateBrowser(CEFWindowParent1, '');
      Chromium1.LoadURL(cleanUrl);
    except
      on E:Exception do
        ShowMessage('Oh no! Failed to load html options dialog: ' + E.Message);
    end;
  finally
    EndFormUpdate;
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure THtmlOptionsDialog.LoadPlaceholder;
begin
  Chromium1.LoadString(HtmlPlaceholderString, 'http://localhost/');
end;

procedure THtmlOptionsDialog.ReInitializeForm;
begin
  Exit;
  if Assigned(Chromium1) then
    FreeAndNil(Chromium1);
  Chromium1 := TChromium.Create(Self);
  //Chromium1.Parent := Self;
  //Chromium1.Visible := True;
  //Chromium1.Align := alClient;
  Chromium1.OnLoadEnd := @Chromium1LoadEnd;
  Chromium1.OnLoadError := @Chromium1LoadError;
  Chromium1.OnLoadingStateChange := @Chromium1LoadingStateChange;
  Chromium1.OnProcessMessageReceived := @Chromium1ProcessMessageReceived;
  Chromium1.OnStatusMessage := @Chromium1StatusMessage;
  Chromium1.OnFileDialog:= @Chromium1FileDialog;
  Chromium1.OnBeforeDownload := @Chromium1BeforeDownload;
  Chromium1.DefaultUrl := 'about:blank' ;
  FirstTime := True;
end;

procedure THtmlOptionsDialog.VisitDomBtnClick(Sender: TObject);
begin
  //if not Chromium1.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_SEQDATAEXPORTOPTIONS)) then
  //  ShowMessage('failed to send message to renderer');
  try
     Chromium1.Browser.Mainframe.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_SEQDATAEXPORTOPTIONS));
  except
    on E:Exception do
    ShowMessage('failed to send message to renderer');
  end
end;

procedure THtmlOptionsDialog.ToggleDevToolsActionExecute(Sender: TObject);
begin
  //BottomPanel.Visible := (not BottomPanel.Visible);
end;

procedure THtmlOptionsDialog.FormShow(Sender: TObject);
begin
  FOkClicked := False;
  OptionsDlgStatusBar.SimpleText := 'Ready';
  FJavaScript.Clear;
  FirstTime := True;
end;

procedure THtmlOptionsDialog.HelpButtonImgClick (Sender :TObject );
begin
  {$IFDEF DEBUG}
  DebugActionExecute(Sender);
  Exit;
  {$ENDIF}
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure THtmlOptionsDialog .HelpButtonImgDblClick (Sender :TObject );
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure THtmlOptionsDialog.OkButtonImgClick(Sender: TObject);
var
  js: String;
begin
  js := '$("form").submit()';
  Chromium1.Browser.MainFrame.ExecuteJavaScript(js, '', 0);
  if Trim(DomProcedureName) = EmptyStr then
    raise Exception.Create('DOM procedure not set. This is a bug in the MEGA software');
  try
     Chromium1.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_CHECK_FORM_VALIDATED));
  except
    on E:Exception do
      ShowMessage('Browser error - failed to send message to renderer process: ' + E.Message);
  end
end;

procedure THtmlOptionsDialog.OpenFileActionExecute(Sender: TObject);
var
  filename: String;
begin
  {$IFNDEF DEBUG}
  Exit; // it is only for developers
  {$ENDIF}
  OpenDialog.Filter := 'HTML Files|*.html|All Files|*.*';
  if not OpenDialog.Execute then
    Exit;
  filename := OpenDialog.FileName;
  if not FileExists(filename) then
  begin
    ShowMessage('Oh no! the specified file was not found: ' + filename);
    Exit;
  end;
  Chromium1.DefaultUrl:= filename;
  Chromium1.CreateBrowser(CEFWindowParent1, '');
  Chromium1.LoadURL(filename);
end;

procedure THtmlOptionsDialog.ShowDevToolsActionExecute(Sender: TObject);
var
  info: TCefWindowInfo;
  Settings: TCefBrowserSettings;
  Pinfo: PCefWindowInfo;
  Psettings: PCefBrowserSettings;
begin
  if not IsDeveloper then
    Exit;
  InitBrowserSettings(Settings);
  InitWindowInfo(info);
  Chromium1.Browser.Host.ShowDevTools(Pinfo, nil, Psettings, nil);
end;

procedure THtmlOptionsDialog.Timer1Timer(Sender: TObject);
begin
  FPleaseWait.CancelBtn.Hide;
  FPleaseWait.UseProgressTimer(False);
  FPleaseWait.SetToMarqueeMode;
   if (FPleaseWait.ProgressBar1.Position=100) then
     begin
        FPleaseWait.Timer1.Enabled:=False;
        FPleaseWait.Close;
        FreeAndNil(FPleaseWait);
        HtmlOptionsDialog.Show;
    end
    else
      begin
        HtmlOptionsDialog.Hide;
        FPleaseWait.Show;
        FPleaseWait.BringToFront;
        FPleaseWait.ProgressBar1.StepIt;
        FPleaseWait.Timer1.Enabled := true;
      end
  end;

procedure THtmlOptionsDialog.FormCreate(Sender: TObject);
begin
  AddToAlign := False;
  FCanClose := False;
  FClosing  := False;
  UpdateShortcutsForMacOs(ActionList1);
  FirstTime := True;
  {$IFNDEF DEBUG}
  OpenFileAction.Enabled := False;
  {$ENDIF}
  FOkClicked := False;
  DomProcedureName := EmptyStr;
  FJavascript := TStringList.Create;
  HelpContext := HC_First_Time_User;
  CancelCallback := nil;
  FTempFiles := TStringList.Create;
  FormStyle := fsStayOnTop;
  ButtonsPanel.Color := RGB($f7, $f8, $f8);
  Constraints.MinWidth := Width;
  FObserverOkCallbacks := TList.Create;
  FObserverCancelCallbacks := TList.Create;
  SetActiveWindow(Self.Handle);
  ImageForm.UpdateImgList(Self);

  {$IFDEF LINUX}
  ConnectKeyPressReleaseEvents(PGtkWidget(Self.Handle));
  {$ENDIF}
end;

procedure THtmlOptionsDialog.FormDestroy(Sender: TObject);
begin
  if Assigned(FJavascript) then
    FJavaScript.Free;
  CleanUpTempFiles;
  if Assigned(FTempFiles) then
    FTempFiles.Free;
  if Assigned(FObserverOkCallbacks) then
    FObserverOkCallbacks.Free;
  if Assigned(FObserverCancelCallbacks) then
    FObserverCancelCallbacks.Free;
end;

function THtmlOptionsDialog.FormHelp (Command :Word ; Data :PtrInt ; var CallHelp :Boolean ):Boolean ;
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

procedure THtmlOptionsDialog.FormHide(Sender: TObject);
begin
  LoadPlaceholder;
end;

procedure THtmlOptionsDialog.FormResize(Sender :TObject );
begin
  PositionButtonsPanel;
  FirstTime := True;
  //{$IFDEF DEBUG}
  //OptionsDlgStatusBar.SimpleText := Format('w-%d, h-%d : w2-%d, h2-%d, visible: %s, top-%d, left-%d', [ClientWidth, ClientHeight, Chromium1.Width, Chromium1.Height, BoolToStr(Chromium1.Visible, True), Chromium1.Top, Chromium1.Left]);
  //{$ENDIF}
end;

procedure THtmlOptionsDialog.CancelButtonImgClick(Sender: TObject);
var
  i: Integer;
  aNotify: TNotifyEvent;
begin
  Self.Hide;
  if Assigned(CancelCallback) then
  begin
    CancelCallback(Sender);
    CancelCallback := nil; { for one-time use}
  end;
  if FObserverCancelCallbacks.Count > 0 then
    for i := 0 to FObserverCancelCallbacks.Count - 1 do
    begin
      aNotify := TNotifyEvent(FObserverCancelCallbacks[i]^);
      aNotify(Sender);
    end;
  ClearObservers;
end;

procedure THtmlOptionsDialog.Chromium1BeforeDownload(Sender: TObject;
  const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
begin
// Show "Save As" dialog, download to default temp directory
  callback.Cont(Concat(GetUserDir(), SaveDialog.FileName), True);
end;

procedure THtmlOptionsDialog.Chromium1FileDialog(Sender: TObject;
  const browser: ICefBrowser; mode: Cardinal; const title,
  defaultFilePath: ustring; const acceptFilters: TStrings;
  const callback: ICefFileDialogCallback; var Result: Boolean);
Var
  ModeType: TCefFileDialogMode;
  Success: Boolean = False;
  Files: TStringList = nil;
  InitialDir: ustring;
  LCLDialog: TOpenDialog = nil;


  function GetDescriptionFromMimeType(const mimeType: String): String;
  Const
    WildCardMimeTypes: array[0..3] of array[0..1] of String = (
      ('audio', 'Audio Files'),
      ('image', 'Image Files'),
      ('text', 'Text Files'),
      ('video', 'Video Files'));
  Var
    i: Integer;
  begin
    Result := '';

    For i := 0 to High(WildCardMimeTypes) do
    begin
      If AnsiCompareText(mimeType, WildCardMimeTypes[i][0] + '/*') = 0 then
      begin
        Result := WildCardMimeTypes[i][1];
        Break;
      end;
    end;
  end;

  procedure AddFilters(includeAll: Boolean);
  Var
    hasFilter: Boolean = False;
    Filter, Line, Descr: String;
    Ext: TStringList = nil;
    sepPos: SizeInt;
    i, k: Integer;
  begin
    try
      try
        Filter := '';
        Ext := TStringList.Create;
        Ext.Delimiter := ';';
        Ext.Add('.bmp');
        Ext.Add('.jpg');
        Ext.Add('.jpeg');
        For i := 0 to AcceptFilters.Count - 1 do
        begin
          Line := AcceptFilters[i];

          If Line = '' then Continue;

          sepPos := Pos('|', Line);
          If  sepPos <> 0 then
          begin
            // treat as a filter of the form "Filter Name|.ext1;.ext2;.ext3"

            Descr := Copy(Line, 1, sepPos - 1);
            Line := StringReplace(Copy(Line, sepPos + 1, Length(Line) - sepPos), '.', '*.', [rfReplaceAll]);
          end
          Else
          begin
            Ext.Clear;
            Descr := '';

            If AnsiStartsStr('.', Line) then
            begin
              // treat as an extension beginning with the '.' character
              Ext.Add('*' + Line);
            end
            Else
            begin
              // convert mime type to one or more extensions
              Descr := GetDescriptionFromMimeType(Line);
              CefGetExtensionsForMimeType(Line, Ext);

              For k := 0 to Ext.Count - 1 do Ext[k] := '*.' + Ext[k];
            end;

            If Ext.Count = 0 then Continue;

            // combine extensions, reuse Line
            Line := Ext.DelimitedText;
          end;

          If Descr = '' then Descr := Line
          {$IFDEF LCLGTK2}
            Else Descr := Descr + ' (' + Line + ')'
          {$ENDIF};

          If Length(Filter) > 0 then Filter := Filter + '|';
          Filter := Filter + Descr + '|' + Line;

          hasFilter := True;
        end;

        // if there are filters, add *.* filter
        If includeAll and hasFilter then
          Filter := Filter + '|All Files' + {$IFDEF LCLGTK2}' (*.*)' +{$ENDIF} '|*.*';

        LCLDialog.Filter := Filter;

        If hasFilter then LCLDialog.FilterIndex := 0;
      except
        on E:Exception do
           ShowBrowserMessage('Oh no! An error has occurred: ' + E.Message);
      end;
      finally
        if Assigned(Ext) then
           Ext.Free;
      end;
  end;

begin
  try
    // Remove modifier flags
    //ModeType := TCefFileDialogMode(LongWord(Mode) and LongWord(FILE_DIALOG_TYPE_MASK));
    ModeType := TCefFileDialogMode(LongWord(Mode));
    case ModeType of
      FILE_DIALOG_OPEN,
      FILE_DIALOG_OPEN_MULTIPLE: LCLDialog := HtmlOptionsDialog.OpenDialog;
      FILE_DIALOG_OPEN_FOLDER: LCLDialog := HtmlOptionsDialog.OpenFolder;
      FILE_DIALOG_SAVE: LCLDialog := HtmlOptionsDialog.SaveDialog;
      else
        raise Exception.Create('Unimplemented dialog type.');
    end;

    if ModeType = FILE_DIALOG_OPEN_MULTIPLE then
      LCLDialog.Options := LCLDialog.Options + [ofAllowMultiSelect];
    if ModeType = FILE_DIALOG_SAVE then
    begin
      //if Boolean(LongWord(Mode) and LongWord(FILE_DIALOG_OVERWRITEPROMPT_FLAG)) then
      //  LCLDialog.Options := LCLDialog.Options + [ofOverwritePrompt];
      if defaultFilePath <> '' then
      begin
        InitialDir := ExtractFileDir(defaultFilePath);
        if DirectoryExists(InitialDir) then
          LCLDialog.InitialDir := InitialDir
        else
          LCLDialog.InitialDir := GetUserDir;
        LCLDialog.FileName := ExtractFileName(defaultFilePath);
      end;
    end;

    //if Boolean(LongWord(Mode) and LongWord(FILE_DIALOG_HIDEREADONLY_FLAG)) then
    //  LCLDialog.Options := LCLDialog.Options + [ofHideReadOnly];
    AddFilters(False);
    Success := HtmlOptionsDialog.OpenDialog.Execute;
    if Success then
    begin
      Files := TStringList.Create;
      if ModeType = FILE_DIALOG_OPEN_MULTIPLE then
         Files.AddStrings(LCLDialog.Files)
      else
        Files.Add(LCLDialog.FileName);
      HtmlOptionsDialog.SaveDialog.FileName := LCLDialog.FileName;
      UploadFileName := LCLDialog.Filename;
      //Callback.Cont(HtmlOptionsDialog.SaveDialog.FilterIndex, Files);
      Callback.Cont(Files);
      FreeAndNil(Files);
    end
    else
      Callback.Cancel;
    Result := True;
  except
    on E:Exception do
       ShowBrowserMessage('Application Error: ' + E.Message);
  end;
end;

procedure THtmlOptionsDialog.Chromium1LoadEnd(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; httpStatusCode: Integer);
begin
  OptionsDlgStatusBar.SimpleText := 'Idle';
  {$IFDEF MSWINDOWS}
  if Screen.PixelsPerInch > 96 then
  begin
    if CompareValue(Chromium1.Browser.Host.ZoomLevel, 0, 0.01) = 0 then
      Chromium1.Browser.Host.ZoomLevel := Chromium1.Browser.Host.ZoomLevel - 1.15;
  end;
  {$ENDIF}
  Chromium1.Invalidate;
end;

procedure THtmlOptionsDialog.Chromium1LoadError(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame; errorCode: TCefErrorCode;
  const errorText, failedUrl: ustring);
begin
  OptionsDlgStatusBar.SimpleText := 'Error: ' + errorText;
  {$IFDEF DEBUG}
  ShowBrowserMessage('Error: ' + errorText);
  {$ENDIF}
end;

procedure THtmlOptionsDialog.Chromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if isLoading then
    OptionsDlgStatusBar.SimpleText := 'Loading'
  else
    OptionsDlgStatusBar.SimpleText := 'Done Loading';
end;

procedure THtmlOptionsDialog.DebugActionExecute(Sender: TObject);
begin
  {$IFDEF DEBUG}
  Chromium1.DefaultUrl:= FOptionsFile;
  Chromium1.CreateBrowser(CEFWindowParent1, '');
  Chromium1.LoadURL(FOptionsFile);
  //ShowBrowserMessage('Chromium visible = ' + BoolToStr(Chromium1.Visible, True));
  {$ENDIF}
end;

procedure THtmlOptionsDialog.ExecuteJavascriptExecute(Sender: TObject);
var
  js: String;
begin
  js := '$("form").submit()';
  Chromium1.Browser.MainFrame.ExecuteJavaScript(js, '', 0);

end;

procedure THtmlOptionsDialog.FormActivate(Sender: TObject);
begin
  if FirstTime then
  begin
    ToolBar1.Images := ImageForm.GetDialogButtonImageList;
    ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
    ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
    Constraints.MinWidth := ToolBar1.Width + 20;
    if ClientWidth > ToolBar1.Width then
      ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
    FirstTime := False;
    if (AddToAlign=True) and (HtmlOptionsDialog.Showing=True) then
    begin
      FPleaseWait := TPleaseWaitDlg.Create(nil);
      HtmlOptionsDialog.Hide;
      FPleaseWait.Timer1.Interval:=100;
      FPleaseWait.Timer1.OnTimer := @HtmlOptionsDialog.Timer1Timer;
    end;
    AddToAlign := False;
  end;
end;

procedure THtmlOptionsDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not FOkClicked) and Assigned(CancelCallback) then
    CancelCallback(Self);
end;

procedure THtmlOptionsDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {$IFDEF DARWIN}
  if not FOkClicked then
    CanClose := False;
  {$ENDIF}
  CanClose := FCanClose;
    if not(FClosing) then
      begin
        FClosing := True;
        Chromium1.CloseBrowser(True);
      end;
end;

procedure THtmlOptionsDialog.Chromium1ProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser;const Frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  i: Integer;
  aNotify: TNotifyEvent;
  p: PNotifyEvent;
begin
  if message.Name = VISITDOMPROC_CHECK_FORM_VALIDATED_COMPLETE then
  begin
    ProcessFormValidationMessage(Browser, message, Result);
    Exit;
  end;
  if Assigned(ProcessMessageReceivedProc) then
    ProcessMessageReceivedProc(Browser, message, Result);
  if FObserverOkCallbacks.Count > 0 then
  begin
    for i := 0 to FObserverOkCallbacks.Count - 1 do
    begin
      p := PNotifyEvent(FObserverOkCallbacks[i]);
      aNotify := p^;
      aNotify(Sender);
    end;
  end;
  ClearObservers;
end;

procedure THtmlOptionsDialog.Chromium1StatusMessage(Sender: TObject;
  const Browser: ICefBrowser; const value: ustring);
begin
  if Trim(value) <> EmptyStr then
    OptionsDlgStatusBar.SimpleText := value;
end;

procedure THtmlOptionsDialog.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  {$IFDEF UNIX}SendCompMessage(CEF_AFTERCREATED); {$ENDIF}
  {$IFDEF WINDOWS}PostMessage(Handle, CEF_AFTERCREATED, 0, 0);{$ENDIF}
end;

procedure THtmlOptionsDialog.PositionButtonsPanel;
begin
  if ButtonsPanel.Width > ToolBar1.Width then
    ToolBar1.Left := Round((ButtonsPanel.Width - ToolBar1.Width)/2);
  ButtonsPanel.Invalidate;
end;

{$IFDEF UNIX}
procedure THtmlOptionsDialog.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure THtmlOptionsDialog.BrowserSetFocusMsg(Data: PtrInt);
begin
  CEFWindowParent1.SetFocus;
end;

procedure THtmlOptionsDialog.BrowserCreatedMsg(Data: PtrInt);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure THtmlOptionsDialog.BrowserShowMessageDialogMsg(Data: PtrInt);
var
  PointerToText: PString;
begin
  if (Data <> 0) then
  begin
    PointerToText := PString(Data);
    ShowMessage(PointerToText^);
  end;
end;

procedure THtmlOptionsDialog.SendCompMessage(aMsg : cardinal; Data: PtrInt = 0);
begin
  case aMsg of
    CEF_AFTERCREATED : Application.QueueAsyncCall(@BrowserCreatedMsg, 0);
    CEF_BEFORECLOSE  : Application.QueueAsyncCall(@BrowserCloseFormMsg, 0);
    CEF_SETFOCUS     : Application.QueueAsyncCall(@BrowserSetFocusMsg, 0);
    CEF_SHOWMESSAGE  : Application.QueueAsyncCall(@BrowserShowMessageDialogMsg, Data);
  end;
end;
{$ENDIF}

procedure THtmlOptionsDialog.ShowBrowserMessage(aMessage: string);
begin
  // Calling ShowMessage directly under Linux will cause a crash
  // because Chromium is not allowed to invoke the GUI directly
  {$IFDEF UNIX}
  SendCompMessage(CEF_SHOWMESSAGE, IntPtr(@aMessage));
  {$ELSE}
  ShowMessage(aMessage);
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure THtmlOptionsDialog.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
end;
{$ENDIF}

{$ENDIF}
end.

