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

unit mesl_menu_builder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esl_linker, MegaConsts,
  Menus;

type

  { TEslResultsMenuBuilder }

  TEslResultsMenuBuilder = class(TObject)
  public
    function BuildResultsPopupMenu(aLink: TEslLinker; menuOwner: TComponent; imageIndex: Integer; onClick: TNotifyEvent): TPopupMenu;
    function BuildMenuItemsList(aLink: TEslLinker; itemsOwner: TComponent; imageIndex: Integer; onClick: TNotifyEvent): TMenuItemsList;
  end;

implementation

uses
  applinker_result_file;

{ TEslResultsMenuBuilder }

function TEslResultsMenuBuilder.BuildResultsPopupMenu(aLink: TEslLinker; menuOwner: TComponent; imageIndex: Integer; onClick: TNotifyEvent): TPopupMenu;
var
  menuItems: TMenuItemsList = nil;
  i: Integer;
  aMenu: TMenuItem = nil;
begin
  try
    menuItems := BuildMenuItemsList(aLink, menuOwner, imageIndex, onClick);
    Result := TPopupMenu.Create(menuOwner);
    if menuItems.Count > 0 then
      for i := 0 to menuItems.Count - 1 do
        Result.Items.Add(menuItems[i]);
    aMenu := TMenuItem.Create(menuOwner);
    aMenu.Caption := CLOSE_RESULTS_FILES;
    aMenu.Hint := 'Close the results display for this analysis';
    aMenu.OnClick := onClick;
    aMenu.ImageIndex := imageIndex;
    Result.Items.Add(aMenu);
    menuItems.Clear;
  finally
    if Assigned(menuItems) then
      menuItems.Free;
  end;
end;

function TEslResultsMenuBuilder.BuildMenuItemsList(aLink: TEslLinker; itemsOwner: TComponent; imageIndex: Integer; onClick: TNotifyEvent): TMenuItemsList;
var
  aMenu: TMenuItem = nil;
  aList: TList = nil;
  f: TAppLinkerResultsFile = nil;
  i: Integer = -1;
begin
  Result := TMenuItemsList.Create;
  aList := aLink.ResultsFiles;
  for i := 0 to aList.Count - 1 do
  begin
    f := TAppLinkerResultsFile(aList[i]);
    aMenu := TMenuItem.Create(itemsOwner);
    aMenu.Caption := f.DisplayName;
    aMenu.Hint := f.ToolTip;
    aMenu.OnClick := onClick;
    aMenu.ImageIndex := imageIndex;
    Result.Add(aMenu);
  end;

  aMenu := TMenuItem.Create(itemsOwner);
  aMenu.Caption := SAVE_RESULTS_FILES;
  aMenu.OnClick := onClick;
  aMenu.ImageIndex := imageIndex;
  Result.Add(aMenu);
end;

end.

