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

unit uMegaBrowserFrame;

//{$I cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, SyncObjs, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  uCEFWinControl, uCEFWindowParent, uCEFChromiumCore, uCEFChromium, uCEFTypes,
  uCEFInterfaces, uCEFConstants, uCEFLinkedWindowParent, uCEFChromiumEvents,
  uCEFMiscFunctions, uCEFApplication, mbrowserutils;

type
  TBrowserTitleEvent = procedure(Sender: TObject; const aTitle : ustring) of object;
  TBrowserURLEvent = procedure(Sender: TObject; const aURL : ustring) of object;
  TBrowserStatusEvent = procedure(Sender: TObject; const aStatus : ustring) of object;
  TBrowserMessageEvent = procedure(Sender: TObject; const Browser: ICefBrowser; const frame:ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean) of object;
  TLogEvent = procedure(log : string) of object;
  TBrowserContextMenuEvent = procedure(Sender: TObject; const Data: PtrInt) of object;
  TBrowserStateEvent = procedure(Sender: TObject) of object;
  TPreKeyEvent = procedure(Sender: TObject; event: PCefKeyEvent; osEvent: TCefEventHandle) of object;

  TBrowserMessageData = record Sender: TObject; Browser: ICefBrowser;  frame:ICefFrame; sourceProcess: TCefProcessId; message: ICefProcessMessage; end;
  PBrowserMessageData = ^TBrowserMessageData;
  TBrowserAddressData = record Sender: TObject; URL: ustring; end;
  PBrowserAddressData = ^TBrowserAddressData;

  TBrowserNewTabEvent = procedure(url: ustring) of object;


{ TBrowserFrame }

  TBrowserFrame = class(TFrame)
    Chromium1: TChromium;
    CEFWindowParent1: TCEFLinkedWindowParent;

    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
    procedure Chromium1DownloadUpdated(Sender: TObject;
      const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
    procedure Chromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure Chromium1StatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser;  const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1PreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
    procedure Chromium1KeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);

    protected
      FBrowserCS            : TCriticalSection;
      FClosing              : boolean;   // Indicates that this frame is destroying the browser
      FHomepage             : ustring;
      FOnBrowserDestroyed   : TNotifyEvent;
      FOnBrowserTitleChange : TBrowserTitleEvent;
      FOnBrowserURLChange   : TBrowserURLEvent;
      FOnBrowserStatusChange: TBrowserStatusEvent;
      FOnBrowserStateChange : TBrowserStateEvent;
      FOnProcessMsgReceived : TBrowserMessageEvent;
      FOnContextMenuMsg     : TBrowserContextMenuEvent;
      FOnBrowserNewTab      : TBrowserNewTabEvent;
      FBrowserAddress       : ustring;
      FBrowserIsLoading     : boolean;
      FBrowserCanGoBack     : boolean;
      FBrowserCanGoForward  : boolean;
      FBrowserStatusText    : ustring;
      FBrowserTitle         : ustring;
      FBrowserPendingHTML   : ustring;
      FOnPreKeyEvent        : TPreKeyEvent;
      FLogAction            : TLogEvent;
      FContext              : ICefRequestContext;
      FExtraInfo            : ICefDictionaryValue;
      FCacheIsClearing      : Boolean;

      // Windows-specific function
      function  GetInitialized : boolean;

      procedure SetBrowserAddress(const aValue : ustring);
      procedure SetBrowserIsLoading(aValue : boolean);
      procedure SetBrowserCanGoBack(aValue : boolean);
      procedure SetBrowserCanGoForward(aValue : boolean);
      procedure SetBrowserStatusText(const aValue : ustring);
      procedure SetBrowserTitle(const aValue : ustring);

      function  GetBrowserAddress : ustring;
      function  GetBrowserIsLoading : boolean;
      function  GetBrowserCanGoBack : boolean;
      function  GetBrowserCanGoForward : boolean;
      function  GetBrowserStatusText : ustring;
      function  GetBrowserTitle : ustring;

      function  GetChromiumComponent : TChromium;
      procedure Log(aMsg: String);

      procedure BrowserCreatedMsg(Data: PtrInt);  // CEF_AFTERCREATED
      procedure BrowserUpdateTitleMsg(Data: PtrInt);            // CEF_UPDATETITLE or CEF_UPDATECAPTION
      procedure BrowserUpdateAddressMsg(Data: PtrInt);           // CEF_UPDATEADDRESS;
      procedure BrowserUpdateStateMsg(Data: PtrInt);     // CEF_UPDATELOADINGSTATE or CEF_UPDATESTATE
      procedure BrowserUpdateStatusTextMsg(Data: PtrInt);       // CEF_UPDATESTATUSTEXT
      procedure BrowserSetFocusMsg(Data: PtrInt);          // CEF_SETFOCUS
      procedure BrowserContextMenuMsg(Data: PtrInt);       // CEF_CONTEXTMENUCMD
      procedure BrowserForwardProcessMsg(Data: PtrInt);    // CEF_RECVPROCESSMSG
      procedure BrowserLoadErrorMsg(Data: PtrInt);    // CEF_LOADERROR
      procedure BrowserNewTabMsg(Data: PtrInt);  // CEF_CREATENEXTTAB
      procedure SendCompMessage(aMsg : cardinal; Data: PtrInt = 0);

    public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateAllHandles;
      procedure   CreateBrowser;
      procedure   CloseBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      function    CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;

      property  Initialized              : boolean               read GetInitialized;
      property  Closing                  : boolean               read FClosing;
      property  Homepage                 : ustring               read FHomepage              write FHomepage;
      property  OnBrowserDestroyed       : TNotifyEvent          read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property  OnBrowserTitleChange     : TBrowserTitleEvent    read FOnBrowserTitleChange  write FOnBrowserTitleChange;
      property  OnBrowserURLChange       : TBrowserURLEvent      read FOnBrowserURLChange    write FOnBrowserURLChange;
      property  OnBrowserStatusChange    : TBrowserStatusEvent   read FOnBrowserStatusChange write FOnBrowserStatusChange;
      property  OnBrowserStateChange     : TBrowserStateEvent    read FOnBrowserStateChange  write FOnBrowserStateChange;
      property  OnPreKeyEvent            : TPreKeyEvent          read FOnPreKeyEvent         write FOnPreKeyEvent;
      property  OnBrowserNewTab          : TBrowserNewTabEvent   read FOnBrowserNewTab       write FOnBrowserNewTab;
      property  ChromiumComponent        : TChromium             read GetChromiumComponent;
      property  OnProcessMessageReceived : TBrowserMessageEvent  read FOnProcessMsgReceived  write FOnProcessMsgReceived;
      property  LogAction                : TLogEvent             read FLogAction             write FLogAction;
      property  OnContextMenuMsg         : TBrowserContextMenuEvent  read FOnContextMenuMsg  write FOnContextMenuMsg;

      property  BrowserAddress       : ustring             read GetBrowserAddress      write SetBrowserAddress;
      property  BrowserIsLoading     : boolean             read GetBrowserIsLoading    write SetBrowserIsLoading;
      property  BrowserCanGoBack     : boolean             read GetBrowserCanGoBack    write SetBrowserCanGoBack;
      property  BrowserCanGoForward  : boolean             read GetBrowserCanGoForward write SetBrowserCanGoForward;
      property  BrowserStatusText    : ustring              read GetBrowserStatusText   write SetBrowserStatusText;
      property  BrowserTitle         : ustring              read GetBrowserTitle        write SetBrowserTitle;
  end;

implementation

{$R *.lfm}

constructor TBrowserFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FClosing               := False;
  FHomepage              := '';
  FBrowserAddress        := '';
  FBrowserIsLoading      := False;
  FBrowserCanGoBack      := False;
  FBrowserCanGoForward   := False;
  FBrowserStatusText     := '';
  FBrowserTitle          := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;
  FBrowserCS             := TCriticalSection.Create;
  FCacheIsClearing       := False;
end;

destructor TBrowserFrame.Destroy;
begin
  FBrowserCS.Free;
  inherited Destroy;
end;

// Windows-specific function
procedure TBrowserFrame.CreateAllHandles;
begin
  {$IFDEF WINDOWS}
  CreateHandle;

  CEFWindowParent1.CreateHandle;
  {$ENDIF}
end;

// Windows-specific function
function TBrowserFrame.GetInitialized : boolean;
begin
  Result := Chromium1.Initialized;
end;

procedure TBrowserFrame.SetBrowserAddress(const aValue : ustring);
begin
  FBrowserCS.Acquire;
  FBrowserAddress := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserIsLoading(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserIsLoading := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserCanGoBack(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserCanGoBack := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserCanGoForward(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserCanGoForward := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserStatusText(const aValue : ustring);
begin
  FBrowserCS.Acquire;
  FBrowserStatusText := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserTitle(const aValue : ustring);
begin
  FBrowserCS.Acquire;
  FBrowserTitle := aValue;
  FBrowserCS.Release;
end;
// synonymous with GetPendingAddress in Windows demo
function TBrowserFrame.GetBrowserAddress : ustring;
begin
  FBrowserCS.Acquire;
  Result := FBrowserAddress;
  FBrowserCS.Release;
end;

// synonymous with GetPendingIsLoading in Windows demo
function TBrowserFrame.GetBrowserIsLoading : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserIsLoading;
  FBrowserCS.Release;
end;
// synonymous with GetPendingCanGoBack in Windows demo
function TBrowserFrame.GetBrowserCanGoBack : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserCanGoBack;
  FBrowserCS.Release;
end;
// synonymous with GetPendingCanGoForward in Windows demo
function TBrowserFrame.GetBrowserCanGoForward : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserCanGoForward;
  FBrowserCS.Release;
end;
// synonymous with GetPendingStatus in Windows demo
function TBrowserFrame.GetBrowserStatusText : ustring;
begin
  FBrowserCS.Acquire;
  Result := FBrowserStatusText;
  FBrowserCS.Release;
end;
// synonymous with GetPendingTitle in Windows demo
function TBrowserFrame.GetBrowserTitle : ustring;
begin
  FBrowserCS.Acquire;
  Result := FBrowserTitle;
  FBrowserCS.Release;
end;
// this is newly added for interfacing with MEGA components
function TBrowserFrame.GetChromiumComponent : TChromium;
begin
  Result := Chromium1;
end;

procedure TBrowserFrame.NotifyMoveOrResizeStarted;
begin
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TBrowserFrame.CreateBrowser;
begin
  Chromium1.DefaultURL := FHomepage;
  {$IFDEF UNIX}
  Chromium1.CreateBrowser(CEFWindowParent1.Handle, CEFWindowParent1.BoundsRect);
  {$ENDIF}
  {$IFDEF WINDOWS}
  Chromium1.CreateBrowser(CEFWindowParent1);
  {$ENDIF}
end;

procedure TBrowserFrame.CloseBrowser;
begin
  if not(FClosing) then
    begin
      FClosing              := True;
      {$IFNDEF DARWIN}
      // NOTE: Chromium1.CloseBrowser triggers a FormCloseQuery in Cocoa
      // so closing one tab will unintentionally close all of them.
      Chromium1.CloseBrowser(True);

      if GlobalCEFApp.ChromeRuntime then
        CEFWindowParent1.Free;
      {$ENDIF}
    end;
end;

// ShowBrowser is Windows-specific
procedure TBrowserFrame.ShowBrowser;
begin
  Chromium1.WasHidden(False);
  Chromium1.SetFocus(True);
  Chromium1.AudioMuted := False;
end;

// HideBrowser is Windows-specific
procedure TBrowserFrame.HideBrowser;
begin
  Chromium1.SetFocus(False);
  Chromium1.WasHidden(True);
  Chromium1.AudioMuted := True;
end;
procedure TBrowserFrame.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TBrowserFrame.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure TBrowserFrame.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SetFocus(True);
end;

// Chromium1GotFocus is Linux-specific
procedure TBrowserFrame.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  {$IFDEF UNIX}SendCompMessage(CEF_SETFOCUS);{$ENDIF}
end;

procedure TBrowserFrame.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  BrowserAddress := url;
  FCacheIsClearing := False;
  SendCompMessage(CEF_UPDATEADDRESS);
end;

procedure TBrowserFrame.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then FOnBrowserDestroyed(self);
end;

procedure TBrowserFrame.Chromium1PreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;
  isKeyboardShortcut := False;
  if assigned(FOnPreKeyEvent) then FOnPreKeyEvent(self, event, osEvent);

end;

procedure TBrowserFrame.Chromium1KeyEvent(Sender: TObject;
      const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; out Result: Boolean);
begin
  Result := False;
end;

// on Linux, GUI changes cannot be executed from the CEF thread
// so any MEGA browser functions that do so must be called async
procedure TBrowserFrame.Chromium1ProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser;
  const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  MessageToForward: PBrowserMessageData;
begin
   if assigned(FOnProcessMsgReceived) then
     begin
          New(MessageToForward);
          MessageToForward^.Sender := Sender;
          MessageToForward^.Browser := Browser;
          MessageToForward^.frame := frame;
          MessageToForward^.sourceProcess := sourceProcess;
          MessageToForward^.message := message;

          SendCompMessage(CEF_RECVPROCESSMSG, PtrInt(MessageToForward));
    end;
end;
procedure TBrowserFrame.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
var
  URLData: PBrowserAddressData;
begin
  Result := True;

  New(URLData);
  URLData^.Url := targetUrl;
  SendCompMessage(CEF_CREATENEXTTAB, PtrInt(URLData));
end;

procedure TBrowserFrame.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  {$IFDEF WINDOWS}
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
  {$ENDIF}
  {$IFDEF LINUX}
  aAction := cbaClose;
  {$ENDIF}
end;

procedure TBrowserFrame.Chromium1DownloadUpdated(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
var
  URLData: PBrowserAddressData;
begin
  if downloadItem.IsComplete then
  begin
    New(URLData);
    URLData^.Url := {$IFDEF UNIX}'file:///' +{$ENDIF}downloadItem.FullPath;
    SendCompMessage(CEF_CREATENEXTTAB, PtrInt(URLData));
  end;
end;

procedure TBrowserFrame.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
var
  errorMsg : string;
begin
  Log('ChromiumLoadError: ' + IntToStr(Ord(errorCode)));
  if (errorCode = ERR_ABORTED) or
     (frame = nil) or
     not(frame.IsValid) or
     not(frame.IsMain) then
    exit
  else if errorCode = ERR_CACHE_MISS then
  begin
    errorMsg := '<h1>Browser Load Error</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + ' Cache Miss</p>' +
                '<p>This is most likely due to a page requiring a form submit.  Try pressing F5 or navigating to this page as you would normally.</p>';
  end
  else if errorCode = ERR_NAME_NOT_RESOLVED then
  begin
    errorMsg := '<h1>Browser Load Error</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + ' Name Not Resolved</p>' +
                '<p>This URL could not be resolved.</p>';
  end
  else if errorCode = ERR_TOO_MANY_REDIRECTS then
  begin
    if not FCacheIsClearing then
    begin
      // Previously cached pages may become corrupted
      // Possibly due to not loading fully before the user navigates away
      // Big problem on NCBI as corrupt cached pages often form a redirect loop
      // If we encounter one, we attempt to load a fresh copy of the page before erroring
      FCacheIsClearing := True;
      browser.ReloadIgnoreCache;
      Exit;
    end
    else
    begin
      FCacheIsClearing := False;
      errorMsg := '<h1>Browser Load Error</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + ' Too Many Redirects</p>' +
                '<p>Failed URL: ' + failedUrl + '</p>' +
                '<p>MEGA was unable to resolve the issue by automatically clearing the page cache.</p>' +
                '<p>This suggests a problem with the host site.</p>';
    end;
  end
  else
  begin
    errorMsg := '<h1>Browser Load Error</h1><br>' +
                '<p>Error: ' + intToStr(Ord(errorCode)) + '</p>' +
                '<p>' + errorText + '</p>';
  end;

  FBrowserCS.Acquire;
  FBrowserPendingHTML := errorMsg;
  FBrowserCS.Release;

  SendCompMessage(CEF_LOADERROR);
end;

procedure TBrowserFrame.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  BrowserIsLoading    := isLoading;
  BrowserCanGoBack    := canGoBack;
  BrowserCanGoForward := canGoForward;

  SendCompMessage(CEF_UPDATESTATE);
end;

procedure TBrowserFrame.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  out Result: Boolean);
begin
  // no special handling needed for MEGA Browser
  Result := True;
end;

procedure TBrowserFrame.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  BrowserStatusText := value;

  SendCompMessage(CEF_UPDATESTATUSTEXT);
end;

procedure TBrowserFrame.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if (length(title) > 0) then
    BrowserTitle := title
   else
    BrowserTitle := Chromium1.DocumentURL;

  SendCompMessage(CEF_UPDATETITLE);
end;

// CreateClientHandler is Windows-specific
function TBrowserFrame.CreateClientHandler(var   windowInfo        : TCefWindowInfo;
                                           var   client            : ICefClient;
                                           const targetFrameName   : string;
                                           const popupFeatures     : TCefPopupFeatures) : boolean;
var
  TempRect : TRect;
begin
  if CEFWindowParent1.HandleAllocated and
     Chromium1.CreateClientHandler(client, False) then
    begin
      Result   := True;
      TempRect := CEFWindowParent1.ClientRect;

      WindowInfoAsChild(windowInfo, CEFWindowParent1.Handle, TempRect, '');
    end
   else
    Result := False;
end;
procedure TBrowserFrame.BrowserLoadErrorMsg(Data: PtrInt);
var
  TempHTML : ustring;
begin
  FBrowserCS.Acquire;
  TempHTML            := FBrowserPendingHTML;
  FBrowserPendingHTML := '';
  FBrowserCS.Release;

  if (length(TempHTML) > 0) then
    Chromium1.LoadString(TempHTML);
end;

procedure TBrowserFrame.BrowserNewTabMsg(Data: PtrInt);
var
  URLData: TBrowserAddressData;
begin
  URLData := PBrowserAddressData(Data)^;
  if Assigned(FOnBrowserNewTab) then
    FOnBrowserNewTab(URLData.URL);
end;

// SendCompMessage was unique to Linux CEF4Delphi demos
// but this approach works equally well for Windows
procedure TBrowserFrame.SendCompMessage(aMsg : cardinal; Data: PtrInt);
begin
  case aMsg of
    CEF_SETFOCUS           : Application.QueueAsyncCall(@BrowserSetFocusMsg, 0);
    CEF_LOADERROR          : Application.QueueAsyncCall(@BrowserLoadErrorMsg, 0);
    CEF_AFTERCREATED       : Application.QueueAsyncCall(@BrowserCreatedMsg, 0);
    CEF_UPDATEADDRESS      : Application.QueueAsyncCall(@BrowserUpdateAddressMsg, 0);
    CEF_UPDATESTATE        : Application.QueueAsyncCall(@BrowserUpdateStateMsg, 0);
    CEF_UPDATESTATUSTEXT   : Application.QueueAsyncCall(@BrowserUpdateStatusTextMsg, 0);
    CEF_UPDATETITLE        : Application.QueueAsyncCall(@BrowserUpdateTitleMsg, 0);
    CEF_RECVPROCESSMSG     : Application.QueueAsyncCall(@BrowserForwardProcessMsg, Data);
    CEF_CONTEXTMENUCMD     : Application.QueueAsyncCall(@BrowserContextMenuMsg, Data);
    CEF_CREATENEXTTAB      : Application.QueueAsyncCall(@BrowserNewTabMsg, Data);
    // Do not use CEF_DESTROY to control destruction of tabs
    // CEF_DESTROY            : Application.QueueAsyncCall(@BrowserDestroyMsg, 0);
  end;
end;

procedure TBrowserFrame.BrowserCreatedMsg(Data: PtrInt);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure TBrowserFrame.BrowserUpdateTitleMsg(Data: PtrInt);
begin
  if assigned(FOnBrowserTitleChange) then
    FOnBrowserTitleChange(self, BrowserTitle);
end;

procedure TBrowserFrame.BrowserUpdateAddressMsg(Data: PtrInt);
begin
  // pass a message to the main MEGA browser window

  if assigned(FOnBrowserURLChange) then
    FOnBrowserURLChange(self, BrowserAddress);
end;

procedure TBrowserFrame.BrowserUpdateStateMsg(Data: PtrInt);
begin
  // pass a message to the main MEGA browser window handling button changes

  if assigned(FOnBrowserStateChange) then
    FOnBrowserStateChange(self);
end;

procedure TBrowserFrame.BrowserUpdateStatusTextMsg(Data: PtrInt);
begin
  if assigned(FOnBrowserStatusChange) then
    FOnBrowserStatusChange(self, BrowserStatusText);
end;

procedure TBrowserFrame.BrowserForwardProcessMsg(Data: PtrInt);
var
  MessageToForward: TBrowserMessageData;
  Result: boolean;
begin
  MessageToForward := PBrowserMessageData(Data)^;
  if assigned(FOnProcessMsgReceived) then
   begin
        FOnProcessMsgReceived(MessageToForward.Sender,
        MessageToForward.Browser,
        MessageToForward.frame,
        MessageToForward.sourceProcess,
        MessageToForward.message,
        Result);
   end;
end;

procedure TBrowserFrame.BrowserContextMenuMsg(Data: PtrInt);
begin
  // pass a message to the main MEGA browser indicating what command was selected
  if assigned(FOnContextMenuMsg) then
    FOnContextMenuMsg(self, Data);
end;

procedure TBrowserFrame.BrowserSetFocusMsg(Data: PtrInt);
begin
  if (Self.Parent.Visible) then
     CEFWindowParent1.SetFocus;
end;

procedure TBrowserFrame.Log(aMsg: String);
begin
  {$IFDEF DEBUG}
   if assigned(FLogAction) then
    FLogAction(aMsg);
  {$ENDIF}
end;

procedure TBrowserFrame.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
begin
  Result := False;
  if Chromium1.IsSameBrowser(browser) then
   begin
        SendCompMessage(CEF_CONTEXTMENUCMD, PtrInt(commandId));
   end;
end;

procedure TBrowserFrame.Chromium1BeforeContextMenu(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if Chromium1.IsSameBrowser(browser) then
    begin
      model.Clear;
      If model.GetCount > 0 then model.AddSeparator;
      model.AddItem(CLIENT_ID_TAB_COPY, '&Open Link in New Tab');
      model.AddItem(CLIENT_ID_WINDOW_COPY, '&Open Link in New Window');
      model.AddItem(CLIENT_ID_CLIPBOARD_COPY, '&Copy Link to Clipboard');
    end;

end;



end.

