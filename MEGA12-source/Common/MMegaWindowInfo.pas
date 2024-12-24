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

unit MMegaWindowInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Forms, Dialogs;

type
  TMegaTrayItem = Class
    IsCreating: Boolean;
    Form: TForm;
  end;

procedure AddWindowToTray(Frm: TForm);
procedure RemoveWindowFromTray(Frm: TForm);

implementation

uses
  {$IFDEF FPC}
  LCLIntF, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, MegaConsts;

procedure AddWindowToTray(Frm: TForm);
var
  IsDone: Boolean;
  AFormItem: TMegaTrayItem;
begin
  if not Assigned(Application.OnIdle) then
    Exit;
  IsDone := False;
  AFormItem := TMegaTrayItem.Create;
  try
    AFormItem.IsCreating := True;
    AFormItem.Form := Frm;
    Application.OnIdle(AFormItem, IsDone);
  finally
    AFormItem.Free;  
  end;
end;

procedure RemoveWindowFromTray(Frm: TForm);
var
  IsDone: Boolean;
  AFormItem: TMegaTrayItem;
begin
  if @Application.OnIdle = nil then
    Exit;
  IsDone := False;
  AFormItem := TMegaTrayItem.Create;
  try
    AFormItem.IsCreating := False;
    AFormItem.Form := Frm;
    Application.OnIdle(AFormItem, IsDone);
  finally
    AFormItem.Free;
  end;
end;

end.
