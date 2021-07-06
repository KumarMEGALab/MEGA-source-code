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

unit mimageform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus;

const
  COLLAPSED_ARROW_INDEX = 1;
  EXPANDED_ARROW_INDEX = 0;
  RIGHT_ARROW_INDEX = 1;
  LEFT_ARROW_INDEX = 2;

type

  { TImageForm }

  TImageForm = class(TForm)
    DefaultIcons: TImageList;
    DisabledIcons: TImageList;
    SmallArrowIcons: TImageList;
  private

  public
    procedure UpdateImgList(aForm: TForm);

  end;

var
  ImageForm: TImageForm;

implementation

uses
  ActnList;

{$R *.lfm}

{ TImageForm }

procedure TImageForm.UpdateImgList(aForm: TForm);
var
  i: Integer;
begin
  for i:=0 to aForm.ComponentCount-1 do
  begin
    if aForm.Components[i].ClassName='TToolBar' then
    begin
      if (TToolBar(aForm.Components[i]).Images<>nil) then
      begin
        if (TToolBar(aForm.Components[i]).Name='TrayToolbar2') or (TToolBar(aForm.Components[i]).Name='PinupsToolbar') then
          continue
        else
        begin
          TToolBar(aForm.Components[i]).Images := DefaultIcons;
          TToolBar(aForm.Components[i]).DisabledImages := DisabledIcons;
        end;
      end;
    end;

    if aForm.Components[i].ClassName='TMainMenu' then
    begin
      if (TMainMenu(aForm.Components[i]).Images <> nil) then
        TMainMenu(aForm.Components[i]).Images := DefaultIcons;
    end;

    if aForm.Components[i].ClassName='TPopupMenu' then
    begin
      if (TPopupMenu(aForm.Components[i]).Images<>nil) then
        TPopupMenu(aForm.Components[i]).Images := DefaultIcons;
    end;

    if aForm.Components[i].ClassName = 'TActionList' then
    begin
      if TActionList(aForm.Components[i]).Images <> nil then
        TActionList(aForm.Components[i]).Images := DefaultIcons;
    end;
  end;
end;

end.

