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

unit uMegaBrowserPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, attabs, uCEFChromium,
  uMegaBrowserTab, uCEFInterfaces, uCEFTypes, MegaConsts;

type
  TTabCloseEvent = procedure(Sender: TObject; const TabCount: integer) of object;

  { TMegaBrowserPanel }

  TMegaBrowserPanel = class(TFrame)
    BrowserPanel: TPanel;
    Tabs: TATTabs;

    procedure ATTabs1TabChanged(Sender: TObject);
    procedure ATTabs1TabChangeQuery(Sender: TObject; ANewTabIndex: integer;
      var ACanChange: boolean);
    procedure TabsTabClose(Sender: TObject; ATabIndex: integer; var ACanClose,
      ACanContinue: boolean);
    procedure TabsTabGetCloseAction(Sender: TObject;
      var AAction: TATTabActionOnClose);
    procedure TabsTabMove(Sender: TObject; AIndexFrom, AIndexTo: integer);
    procedure TabsTabPlusClick(Sender: TObject);
  private
    procedure UpdateTabDPI(aTab: TATTabData);
    function GetTabBrowser(aTab: TBrowserTab) : TChromium;
    function GetBrowserFromTabData(aTabData: TATTabData) : TChromium;

  protected
     FLastTabID  : cardinal; // Used by NextTabID to generate unique tab IDs
     FActiveTab: TBrowserTab;
     FActiveTabIndex: integer;
     FTabCloseEvent: TTabCloseEvent;
     function  GetNextTabID : cardinal;
     property  NextTabID : cardinal   read GetNextTabID;
     function GetParentForm : TCustomForm;

  public
      property ActiveTab: TBrowserTab read FActiveTab;
      property ActiveTabIndex: integer read FActiveTabIndex;
      function ActiveTabChromium: TChromium;
      function ActiveBrowser : ICefBrowser;
      property Chromium: TChromium read ActiveTabChromium;
      function TabCount: integer;
      function TabAtIndex(index: integer): TBrowserTab;
      function CreateNewTab(aUrl: ustring = '') : TBrowserTab;
      procedure FreeTab(aTab : TBrowserTab);
      procedure UpdateDPI;
      property OnTabClose: TTabCloseEvent read FTabCloseEvent write FTabCloseEvent;
      procedure CloseActiveTab;

    procedure Register;
  end;

implementation

{$R *.lfm}

{ TMegaBrowserPanel }

procedure TMegaBrowserPanel.Register;
begin
  RegisterComponents('MEGA Browser Panel', [TMegaBrowserPanel]);
end;

function TMegaBrowserPanel.GetNextTabID : cardinal;
begin
  inc(FLastTabID);
  Result := FLastTabID;
end;

procedure TMegaBrowserPanel.ATTabs1TabChanged(Sender: TObject);
begin

end;
function TMegaBrowserPanel.GetParentForm : TCustomForm;
var
  TempParent : TWinControl;
begin
  TempParent := Parent;

  while (TempParent <> nil) and not(TempParent is TCustomForm) do
    TempParent := TempParent.Parent;

  if (TempParent <> nil) and (TempParent is TCustomForm) then
    Result := TCustomForm(TempParent)
   else
    Result := nil;
end;

procedure TMegaBrowserPanel.FreeTab(aTab : TBrowserTab);
begin
  if Assigned(aTab) then
    begin
      aTab.BrowserFrame.Hide;
      aTab.CloseBrowser;

      // can't actually free the tab at this point
      // it's still referenced by TabChangeQuery
      // CloseBrowser marks the tab as ready to be freed later
    end;
end;

procedure TMegaBrowserPanel.CloseActiveTab;
begin
  if Assigned(ActiveTab) then
    begin
      Tabs.DeleteTab(ActiveTabIndex, true, false);
    end;
end;

procedure TMegaBrowserPanel.ATTabs1TabChangeQuery(Sender: TObject;
  ANewTabIndex: integer; var ACanChange: boolean);
var
     newTab: TBrowserTab;
begin
     if Assigned(ActiveTab) then
      begin
        if (not ActiveTab.ReadyToFree) then
           ActiveTab.BrowserFrame.Hide
        else
           ActiveTab.Free;
      end;

    newTab := TBrowserTab(Tabs.GetTabData(ANewTabIndex).TabObject);

    if Assigned(newTab) then
    begin
      FActiveTab := newTab;
      FActiveTabIndex := ANewTabIndex;
      newTab.BrowserFrame.Show;
    end;
end;

procedure TMegaBrowserPanel.TabsTabClose(Sender: TObject; ATabIndex: integer;
  var ACanClose, ACanContinue: boolean);
var
     selectedTab: TBrowserTab;
begin
     selectedTab := TabAtIndex(ATabIndex);
     FreeTab(selectedTab);

     // only non-active tabs can be safely freed at this point
     if (selectedTab <> FActiveTab) then
        selectedTab.Free;

     if (FTabCloseEvent <> nil) then
          FTabCloseEvent(Sender, Tabs.TabCount);
end;

procedure TMegaBrowserPanel.TabsTabGetCloseAction(Sender: TObject;
  var AAction: TATTabActionOnClose);
begin
  AAction := aocRecent;
end;

procedure TMegaBrowserPanel.TabsTabMove(Sender: TObject; AIndexFrom,
  AIndexTo: integer);
begin
     if (AIndexFrom = FActiveTabIndex) then
        FActiveTabIndex := AIndexTo;
end;

procedure TMegaBrowserPanel.TabsTabPlusClick(Sender: TObject);
begin
  CreateNewTab;
end;

function TMegaBrowserPanel.TabAtIndex(index: integer) : TBrowserTab;
var
  TabObject: TObject = nil;
  TabData: TATTabData;
begin
    Result := nil;
    if (Tabs.TabCount > 0) and (index >= 0) and Assigned(Tabs.GetTabData(index))
    and Assigned(Tabs.GetTabData(index).TabObject) then
      begin
           TabData := Tabs.GetTabData(index);
           TabObject := TabData.TabObject;
           try
           if (TabObject <> nil) and (TabObject is TBrowserTab) then
              Result := TBrowserTab(TabObject);

           finally
           end;
      end;
end;

function TMegaBrowserPanel.ActiveTabChromium : TChromium;
var
  aTab: TBrowserTab = nil;
begin
    Result := nil;
    aTab := ActiveTab;
    if (aTab <> nil) then
      Result := GetTabBrowser(aTab);
end;

function TMegaBrowserPanel.ActiveBrowser : ICefBrowser;
var
  aTab: TBrowserTab = nil;
begin
    Result := nil;
    aTab := ActiveTab;
    if (aTab <> nil) then
      Result := GetTabBrowser(aTab).Browser;
end;

function TMegaBrowserPanel.TabCount : integer;
var
  i : integer;
  aBrowserTab : TBrowserTab = nil;
begin
  Result := 0;
  i      := pred(Tabs.TabCount);

  while (i >= 0) do
    begin
      // Only count the fully initialized browser tabs and not the one waiting to be used.

      aBrowserTab := TabAtIndex(i);
      if (aBrowserTab <> nil) and (aBrowserTab.Initialized) then
        inc(Result);

      dec(i);
    end;
end;


function TMegaBrowserPanel.CreateNewTab(aUrl: ustring = '') : TBrowserTab;
var
newBrowserTab: TBrowserTab;
newTATab: TATTabData;
url: ustring = MEGA_WEBSITE_URL;
begin
     if Trim(aUrl) <> EmptyStr then
        url := aUrl;

     newBrowserTab := TBrowserTab.Create(NextTabID, BrowserPanel);
     newBrowserTab.CreateBrowser(url);
     newTATab :=  Tabs.AddTab(-1, '');
     newBrowserTab.AssignToTab(newTATab, 'New Tab');
     Tabs.TabIndex := Tabs.TabCount - 1;
     Result := newBrowserTab;
end;

procedure TMegaBrowserPanel.UpdateDPI;
var
   aTab: TCollectionItem = nil;
begin
     for aTab in Tabs.Tabs do
         UpdateTabDPI(TATTabData(aTab));
end;

function TMegaBrowserPanel.GetBrowserFromTabData(aTabData: TATTabData) : TChromium;
begin
  Result := nil;
  try
    Result := TBrowserTab(aTabData.TabObject).ChromiumComponent;
  finally
  end;
end;

function TMegaBrowserPanel.GetTabBrowser(aTab: TBrowserTab) : TChromium;
begin
  Result := GetBrowserFromTabData(aTab.TabData);
end;

procedure TMegaBrowserPanel.UpdateTabDPI(aTab: TATTabData);
var
   aBrowser: TChromium = nil;
begin
     aBrowser := GetBrowserFromTabData(aTab);
     if (aBrowser <> nil) then
    begin
      aBrowser.NotifyScreenInfoChanged;
      aBrowser.WasResized;
    end;
end;

end.
