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

unit uMegaBrowserTab;

{$mode objfpc}{$H+}

//{$I cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Classes, Messages, ComCtrls, Controls,
  Forms, SysUtils, uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFChromium,
  uMegaBrowserFrame, mbrowserutils, attabs;

{ TBrowserTab }

type
  TBrowserTab = class(TObject)
    protected
      FBrowserFrame : TBrowserFrame;
      FTabID        : cardinal;
      FParent       : TWinControl;
      FTabData      : TATTabData;
      FReadyToFree  : boolean;

      function    GetParentForm : TCustomForm;
      function    GetInitialized : boolean;
      function    GetClosing : boolean;
      function    GetChromiumComponent : TChromium;
      function    GetCaption : string;
      procedure   SetCaption(aCaption : string);

      procedure   SendFormMessage(aMsg : cardinal; aData : PtrInt = 0);
      function    PostFormMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;

      procedure   BrowserFrame_OnBrowserDestroyed(Sender: TObject);
      procedure   BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : ustring);

      property    ParentForm : TCustomForm  read GetParentForm;

    public
      constructor Create(aTabID : cardinal; aParent : TWinControl);
      procedure   AssignToTab(aTabData: TATTabData; const aCaption : string);
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser(const aHomepage : ustring);
      procedure   CloseBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      {$IFDEF WINDOWS}
      function    CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
      {$ENDIF}

      property    TabID             : cardinal   read FTabID;
      property    Closing           : boolean    read GetClosing;
      property    Initialized       : boolean    read GetInitialized;
      property    ChromiumComponent : TChromium    read GetChromiumComponent;
      property    BrowserFrame      : TBrowserFrame read FBrowserFrame;
      property    TabData           : TATTabData    read FTabData write FTabData;
      property    Caption           : string        read GetCaption write SetCaption;
      property    ReadyToFree       : boolean       read FReadyToFree;
  end;

implementation

uses
  uMegaBrowser;

constructor TBrowserTab.Create(aTabID : cardinal;  aParent : TWinControl);
begin
    FBrowserFrame := nil;
    FTabID        := aTabID;
    FParent       := aParent;
    FReadyToFree  := false;
end;

procedure TBrowserTab.AssignToTab(aTabData: TATTabData; const aCaption : string);
begin
    FTabData      := aTabData;
    FTabData.TabCaption    := aCaption;
    FTabData.TabObject     := self;
end;

function TBrowserTab.GetParentForm : TCustomForm;
var
  TempParent : TWinControl;
begin
  TempParent := FParent;

  while (TempParent <> nil) and not(TempParent is TCustomForm) do
    TempParent := TempParent.Parent;

  if (TempParent <> nil) and (TempParent is TCustomForm) then
    Result := TCustomForm(TempParent)
   else
    Result := nil;
end;
// this is newly added for interfacing with MEGA components
function TBrowserTab.GetChromiumComponent : TChromium;
begin
  Result := nil;
  if (GetInitialized) then
    Result := FBrowserFrame.Chromium1;
end;
// Windows-specific function
function TBrowserTab.GetInitialized : boolean;
begin
  Result := (FBrowserFrame <> nil) and
            FBrowserFrame.Initialized;
end;
// Windows-specific function
function TBrowserTab.GetClosing : boolean;
begin
  Result := (FBrowserFrame <> nil) and
            FBrowserFrame.Closing;
end;

function TBrowserTab.GetCaption : string;
begin
  Result := FTabData.TabCaption;
end;

procedure TBrowserTab.SetCaption(aCaption : string);
begin
  FTabData.TabCaption := aCaption;
end;

// Windows-specific function for sending messages to the main browser form
// kept for archival purposes
function TBrowserTab.PostFormMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempForm : TCustomForm;
begin
  TempForm := ParentForm;
  Result   := (TempForm <> nil) and
              TempForm.HandleAllocated and
              PostMessage(TempForm.Handle, aMsg, aWParam, aLParam);
end;

// A cross-platform function for sending messages to the main browser form
procedure TBrowserTab.SendFormMessage(aMsg : cardinal; aData : PtrInt);
var
  TempForm : TCustomForm;
begin
  TempForm := ParentForm;
  if (TempForm <> nil) and (TempForm is TCustomForm) then
    TMegaBrowserFrm(TempForm).SendCompMessage(aMsg, aData);
end;

procedure TBrowserTab.NotifyMoveOrResizeStarted;
begin
  FBrowserFrame.NotifyMoveOrResizeStarted;
end;

procedure TBrowserTab.CreateBrowser(const aHomepage : ustring);
begin
  if (FBrowserFrame = nil) then
    begin
      FBrowserFrame                               := TBrowserFrame.Create(FParent);
      FBrowserFrame.Parent                        := FParent;
      FBrowserFrame.Align                         := alClient;
      FBrowserFrame.Visible                       := True;
      FBrowserFrame.OnBrowserDestroyed   := @BrowserFrame_OnBrowserDestroyed;
      FBrowserFrame.OnBrowserTitleChange := @BrowserFrame_OnBrowserTitleChange;
      FBrowserFrame.OnProcessMessageReceived:= @TMegaBrowserFrm(ParentForm).crmProcessMessageReceived;
      FBrowserFrame.OnContextMenuMsg := @TMegaBrowserFrm(ParentForm).HandleChromiumContextMenu;
      FBrowserFrame.ChromiumComponent.OnBeforeDownload := @TMegaBrowserFrm(ParentForm).crmBeforeDownload;
      FBrowserFrame.OnBrowserNewTab := @TMegaBrowserFrm(ParentForm).NewTab;
      FBrowserFrame.OnBrowserStatusChange := @TMegaBrowserFrm(ParentForm).StatusChange;
      FBrowserFrame.OnBrowserURLChange    := @TMegaBrowserFrm(ParentForm).URLChange;
      FBrowserFrame.OnBrowserStateChange  := @TMegaBrowserFrm(ParentForm).ButtonStateChange;
      FBrowserFrame.OnPreKeyEvent         := @TMegaBrowserFrm(ParentForm).HandleChromiumKeypress;
      {$IFDEF DEBUG}
      FBrowserFrame.LogAction := @TMegaBrowserFrm(ParentForm).Log;
      {$ENDIF}
      FBrowserFrame.Name                 := 'BrowserFrame' + intToStr(TabID);
      {$IFDEF WINDOWS}
      FBrowserFrame.CreateAllHandles;
      {$ENDIF}
    end;

  FBrowserFrame.Homepage := aHomepage;
  if (FBrowserFrame <> nil) then FBrowserFrame.CreateBrowser;
end;

procedure TBrowserTab.CloseBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.CloseBrowser;
  FReadyToFree := true;
end;
// Windows-specific function
procedure TBrowserTab.ShowBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.ShowBrowser;
end;
// Windows-specific function
procedure TBrowserTab.HideBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.HideBrowser;
end;

procedure TBrowserTab.BrowserFrame_OnBrowserDestroyed(Sender: TObject);
begin
  // This event is executed in a CEF thread so we have to send a message to
  // destroy the tab in the main application thread.
  // we've decided the browser should not be sending messages to destroy the tab
  // the browser sends the command to destroy the browser, then destroys the tab
  //SendFormMessage(CEF_DESTROYTAB, PtrInt(TabID));
end;

procedure TBrowserTab.BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : ustring);
begin
  SetCaption(aTitle);
  TMegaBrowserFrm(GetParentForm).TitleChange(self, aTitle);
end;

{$IFDEF WINDOWS}
// Windows-specific function
function TBrowserTab.CreateClientHandler(var   windowInfo        : TCefWindowInfo;
                                         var   client            : ICefClient;
                                         const targetFrameName   : string;
                                         const popupFeatures     : TCefPopupFeatures) : boolean;
begin
  Result := (FBrowserFrame <> nil) and
            FBrowserFrame.CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures);
end;
{$ENDIF}


end.
