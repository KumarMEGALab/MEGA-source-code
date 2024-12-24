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

unit htmloptionsdlg;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLtype, Classes, SysUtils, FileUtil, cef3lcl, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ActnList, cef3types, cef3intf, cef3gui, mhelpfiles,
  mhelpkeywords, Walk_Through_MEGA, cef3lib, strutils, MPleaseWaitDlg, mimageform;

type
  TProcessMessageReceivedProc = procedure(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean) of object;

  {$IFDEF VISUAL_BUILD}
  { THtmlOptionsDialog }

  THtmlOptionsDialog = class(TForm)
    DebugAction: TAction;
    ButtonsPanel: TPanel;
    CancelButtonImg: TImage;
    ExecuteJavascript: TAction;
    HelpButtonImg: TImage;
    ImageList2: TImageList;
    BottomPanel: TPanel;
    OkButtonImg: TImage;
    SaveDialog: TSaveDialog;
    OpenFolder: TSelectDirectoryDialog;
    ShowDevToolsAction: TAction;
    OpenFileAction: TAction;
    ImageList1: TImageList;
    OptionsDlgStatusBar: TStatusBar;
    ToggleDevToolsAction: TAction;
    ActionList1: TActionList;
    Chromium1: TChromium;
    OpenDialog: TOpenDialog;
    procedure CancelButtonImgClick(Sender: TObject);
    procedure CancelButtonImgMouseEnter(Sender: TObject);
    procedure CancelButtonImgMouseLeave(Sender: TObject);
    procedure Chromium1BeforeDownload(Sender: TObject;
      const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure Chromium1FileDialog(Sender: TObject; const Browser: ICefBrowser;
      mode: TCefFileDialogMode; const title, defaultFileName: ustring;
      acceptFilters: TStrings; selectedAcceptFilter: Integer;
      const callback: ICefFileDialogCallback; out Result: Boolean);
    procedure Chromium1LoadEnd(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1LoadError(Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
    procedure Chromium1LoadingStateChange(Sender: TObject;
      const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
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
    procedure HelpButtonImgMouseEnter (Sender :TObject );
    procedure HelpButtonImgMouseLeave (Sender :TObject );
    procedure OkButtonImgClick(Sender: TObject);
    procedure OkButtonImgMouseEnter(Sender: TObject);
    procedure OkButtonImgMouseLeave(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure ShowDevToolsActionExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToggleDevToolsActionExecute(Sender: TObject);
    procedure VisitDomBtnClick(Sender: TObject);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1StatusMessage(Sender: TObject;
      const Browser: ICefBrowser; const value: ustring);
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
    {$IFDEF VISUAL_BUILD}
    procedure InitBrowserSettings(var Settings: TCefBrowserSettings);
    procedure InitWindowInfo(var info: TCefWindowInfo);
    {$ENDIF}
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
  mrenderprocesshandler, cef3ref, mbrowserutils, MegaUtils,
  LazFileUtils, Math, MegaConsts, mshortcutshelper;

{ THtmlOptionsDialog }

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
    if not Chromium1.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(DomProcedureName)) then
      ShowMessage('failed to send message to renderer');
  except
    on E:Exception do
    begin
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
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
begin
  UploadFileName := EmptyStr;
  DownloadFileName := EmptyStr;
  {$IFNDEF MSWINDOWS}
  ReInitializeForm;
  {$ENDIF}
  OkButtonImg.Visible := True;
  HelpButtonImg.Visible := True;
  if not Visible then
    Show;
  if not Chromium1.Visible then
    Chromium1.Visible := True;
  HelpContext := aHelpContext;
  FFilename := ExtractFilenameOnly(aFile);
  ClientWidth := aWidth;
  ClientHeight := aHeight;
  PositionButtonsPanel;
  Caption := aCaption;
  htmlFile := GetPrivateFile(aFile, False);
  if not FileExists(htmlFile) then
  begin
    ShowMessage(Format('Oh no! An options settings file (%s) is missing from your computer. Please restart MEGA to try and resolve this issue', [aFile]));
    Exit;
  end;
  cleanUrl := 'file:///' + htmlFile;
  {$IFDEF MSWINDOWS}
  cleanUrl := StringReplace(cleanUrl, '\', '/', [rfReplaceAll]);
  {$ENDIF}
  cleanUrl := StringReplace(cleanUrl, 'file:////', 'file:///', [rfReplaceAll]);
  FOptionsFile := cleanUrl;
  Chromium1.Load(cleanUrl);
end;

procedure THtmlOptionsDialog.LoadOptionsFile(aFile: String; javascript: String; aCaption: String; aWidth: Integer; aHeight: Integer; aHelpContext: THelpContext);
var
  htmlFile, tempFile: String;
  html: String;
  aList: TStringList = nil;
  cleanUrl: String;
begin
  {$IFNDEF MSWINDOWS}
  ReInitializeForm;
  {$ENDIF}
  OkButtonImg.Visible := True;
  HelpButtonImg.Visible := True;
  if not Visible then
    Show;
  if not Chromium1.Visible then
    Chromium1.Visible := True;
  HelpContext := aHelpContext;
  FFilename := ExtractFilenameOnly(aFile);
  ClientWidth := aWidth;
  ClientHeight := aHeight;
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
      Chromium1.Load(cleanUrl);
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
  Chromium1.Browser.MainFrame.LoadString(HtmlPlaceholderString, 'http://localhost/');
end;

procedure THtmlOptionsDialog.ReInitializeForm;
begin
  Exit;
  if Assigned(Chromium1) then
    FreeAndNil(Chromium1);
  Chromium1 := TChromium.Create(Self);
  Chromium1.Parent := Self;
  Chromium1.Visible := True;
  Chromium1.Align := alClient;
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
  if not Chromium1.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_SEQDATAEXPORTOPTIONS)) then
    ShowMessage('failed to send message to renderer');
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

procedure THtmlOptionsDialog .HelpButtonImgMouseEnter (Sender :TObject );
begin
  ImageList2.GetBitmap(1, HelpButtonImg.Picture.Bitmap);
end;

procedure THtmlOptionsDialog .HelpButtonImgMouseLeave (Sender :TObject );
begin
  ImageList2.GetBitmap(0, HelpButtonImg.Picture.Bitmap);
end;

procedure THtmlOptionsDialog.OkButtonImgClick(Sender: TObject);
var
  js: String;
begin
  js := '$("form").submit()';
  Chromium1.Browser.MainFrame.ExecuteJavaScript(js, '', 0);
  if Trim(DomProcedureName) = EmptyStr then
    raise Exception.Create('DOM procedure not set. This is a bug in the MEGA software');
  if not Chromium1.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_CHECK_FORM_VALIDATED)) then
    ShowMessage('Failed to send message to renderer process');
end;

procedure THtmlOptionsDialog.OkButtonImgMouseEnter(Sender: TObject);
begin
  ImageList1.GetBitmap(1, OkButtonImg.Picture.Bitmap);
end;

procedure THtmlOptionsDialog.OkButtonImgMouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(0, OkButtonImg.Picture.Bitmap);
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
  Chromium1.Load(filename);
end;

procedure THtmlOptionsDialog.ShowDevToolsActionExecute(Sender: TObject);
var
  info: TCefWindowInfo;
  Settings: TCefBrowserSettings;
begin
{$IFNDEF NO_BROWSER}
  if not IsDeveloper then
    Exit;
  InitBrowserSettings(Settings);
  InitWindowInfo(info);
  Chromium1.Browser.Host.ShowDevTools(info, nil, settings, nil);
{$ENDIF}
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
      end
  end;

procedure THtmlOptionsDialog.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  FirstTime := True;
  OpenFileAction.Enabled := False;
  FOkClicked := False;
  ImageList1.GetBitmap(0, OkButtonImg.Picture.Bitmap);
  ImageList1.GetBitmap(2, CancelButtonImg.Picture.Bitmap);
  ImageList2.GetBitmap(0, HelpButtonImg.Picture.Bitmap);
  {$IFDEF DARWIN}
  OkButtonImg.Proportional:=True;
  CancelButtonImg.Proportional:=True;
  HelpButtonImg.Proportional:=True;
  {$ENDIF}
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

procedure THtmlOptionsDialog.CancelButtonImgMouseEnter(Sender: TObject);
begin
  ImageList1.GetBitmap(3, CancelButtonImg.Picture.Bitmap);
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

procedure THtmlOptionsDialog.CancelButtonImgMouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(2, CancelButtonImg.Picture.Bitmap);
end;

procedure THtmlOptionsDialog.Chromium1BeforeDownload(Sender: TObject;
  const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
begin
// Show "Save As" dialog, download to default temp directory
  callback.Cont(Concat(GetUserDir(), SaveDialog.FileName), True);
end;

procedure THtmlOptionsDialog.Chromium1FileDialog(Sender: TObject;
  const Browser: ICefBrowser; mode: TCefFileDialogMode; const title,
  defaultFileName: ustring; acceptFilters: TStrings;
  selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback; out
  Result: Boolean);
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

        If hasFilter then LCLDialog.FilterIndex := SelectedAcceptFilter;
      except
        on E:Exception do
           ShowMessage('Oh no! An error has occurred: ' + E.Message);
      end;
      finally
        if Assigned(Ext) then
           Ext.Free;
      end;
  end;

begin
  try
    // Remove modifier flags
    ModeType := TCefFileDialogMode(LongWord(Mode) and LongWord(FILE_DIALOG_TYPE_MASK));

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
      if Boolean(LongWord(Mode) and LongWord(FILE_DIALOG_OVERWRITEPROMPT_FLAG)) then
        LCLDialog.Options := LCLDialog.Options + [ofOverwritePrompt];
      if DefaultFileName <> '' then
      begin
        InitialDir := ExtractFileDir(DefaultFileName);
        if DirectoryExists(InitialDir) then
          LCLDialog.InitialDir := InitialDir
        else
          LCLDialog.InitialDir := GetUserDir;
        LCLDialog.FileName := ExtractFileName(DefaultFileName);
      end;
    end;

    if Boolean(LongWord(Mode) and LongWord(FILE_DIALOG_HIDEREADONLY_FLAG)) then
      LCLDialog.Options := LCLDialog.Options + [ofHideReadOnly];
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
      Callback.Cont(HtmlOptionsDialog.SaveDialog.FilterIndex, Files);
      FreeAndNil(Files);
    end
    else
      Callback.Cancel;
    Result := True;
  except
    on E:Exception do
       ShowMessage('Application Error: ' + E.Message);
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
  ShowMessage('Error: ' + errorText);
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
  Chromium1.Load(FOptionsFile);
  //Chromium1.BringToFront;
  //
  //Chromium1.Browser.Stopload;
  //Chromium1.Browser.Reload;
  ShowMessage('Chromium visible = ' + BoolToStr(Chromium1.Visible, True));
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
  //if FirstTime then
  //begin
  //  FirstTime := False;
  //  if (AddToAlign=True) and (HtmlOptionsDialog.Showing=True) then
  //  begin
  //    FPleaseWait := TPleaseWaitDlg.Create(nil);
  //    HtmlOptionsDialog.Hide;
  //    FPleaseWait.Timer1.Interval:=100;
  //    FPleaseWait.Timer1.OnTimer := @HtmlOptionsDialog.Timer1Timer;
  //  end;
  //  AddToAlign := False;
  //end;
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
end;

procedure THtmlOptionsDialog.Chromium1ProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
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

procedure THtmlOptionsDialog.PositionButtonsPanel;
var
  space: Integer;
begin
  space := BottomPanel.Width - ButtonsPanel.Width;
  if space > 0 then
    ButtonsPanel.Left := Round(space / 2);
  ButtonsPanel.Invalidate;
end;
{$ENDIF}

end.

