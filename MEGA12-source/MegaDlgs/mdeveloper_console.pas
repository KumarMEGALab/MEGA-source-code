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

unit mdeveloper_console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TDeveloperConsole }

  TDeveloperConsole = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private

  public
    function NumLines: Integer;
  end;

  procedure WriteToDevConsole(aMsg: String);

var
  DeveloperConsole: TDeveloperConsole;

implementation

uses
  MegaConsts;

procedure WriteToDevConsole(aMsg: String);
begin
  {$IFNDEF DEBUG}
  if not IsDeveloper then
    Exit;
  {$ENDIF}

  {$IFDEF VISUAL_BUILD}
  if not Assigned(DeveloperConsole) then
    DeveloperConsole := TDeveloperConsole.Create(Application);
  if aMsg = LineEnding then
    DeveloperConsole.Memo1.Lines.Add(aMsg)
  else
    DeveloperConsole.Memo1.Lines.Add(Format('%8.0n %s', [DeveloperConsole.NumLines*1.0, aMsg]));
  if not DeveloperConsole.Visible then
    DeveloperConsole.Show;
  {$ENDIF}
end;

{$R *.lfm}

{ TDeveloperConsole }

procedure TDeveloperConsole.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

function TDeveloperConsole.NumLines: Integer;
begin
  Result := Memo1.Lines.Count;
end;

procedure TDeveloperConsole.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TDeveloperConsole.Button2Click(Sender: TObject);
begin
  Hide;
end;

end.

