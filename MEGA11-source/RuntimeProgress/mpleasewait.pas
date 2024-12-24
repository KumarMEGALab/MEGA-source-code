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

unit MPleaseWait;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
 {$IFDEF VISUAL_BUILD}
 MPleaseWaitDlg, Dialogs,
 {$ENDIF}
 SysUtils, Classes, MegaConsts;

type

  { TPleaseWait }

  TPleaseWait = class(TObject)
  private
    progress : Integer;
    CurrentAction : AnsiString;
    Canceled : boolean;
    ShowCancel : Boolean;
    CurrentCaption : AnsiString;
    {$IFDEF VISUAL_BUILD}
    PleaseWaitDlg : TPleaseWaitDlg;
    {$ENDIF}
    function GetVisible: Boolean;
    procedure SetAction(Value: AnsiString);
    procedure SetPercentDone(Value: Integer);
    procedure SetCaption(Value : AnsiString);
    function GetIsCancel: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    procedure show;
    procedure hide;
    function GetCurrentAction: AnsiString;
    function GetPercentDone: Integer;
    Constructor Create(Owner : TComponent);
    Destructor Destroy; override;
  public
    procedure SetShowCancel(Value: Boolean=True);
    procedure UseProgressTimer(Value: Boolean=True);
    procedure SetToMarqueeMode;
    property IsCancel: Boolean read GetIsCancel;

    property Action: AnsiString read CurrentAction write SetAction;
    property PercentDone: Integer write SetPercentDone;
    property Caption: AnsiString read CurrentCaption write SetCaption;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

implementation

uses
 Forms;

constructor TPleaseWait.Create(Owner: TComponent);
begin
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg := TPleaseWaitDlg.Create(Owner);
  {$ENDIF}
end;

destructor TPleaseWait.Destroy;
begin
  {$IFDEF VISUAL_BUILD}
  try
    if Assigned(PleaseWaitDlg) then
      FreeAndNil(PleaseWaitDlg);
  Except on E: Exception do
    {$IFDEF DEBUG}
     ShowMessage('Error in TPleaseWait: ' + E.Message);
    {$ENDIF}
    // If we have an error while closing this dialog it's not too important, rather don't show an error message.
  end;
  {$ENDIF}
  inherited;
end;

procedure TPleaseWait.show;
begin
  {$IFDEF VISUAL_BUILD}
  if not isPrototyper then
    PleaseWaitDlg.Show;
  {$ENDIF}
  {$IFNDEF DARWIN}Application.ProcessMessages;{$ENDIF}   // PleaseWaitDlg is often invisible without this. -KT
end;

procedure TPleaseWait.hide;
begin
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg.Hide;
  {$ENDIF}
  {$IFNDEF DARWIN}Application.ProcessMessages;{$ENDIF}
end;


procedure TPleaseWait.SetCaption(Value : AnsiString);
begin
  CurrentCaption := Value;
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg.Caption := CurrentCaption;
  {$ENDIF}
end;

procedure TPleaseWait.SetAction(Value: AnsiString);
begin
  CurrentAction := Value;
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg.MyAction := Value;
  {$ENDIF}
end;

function TPleaseWait.GetVisible: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := Assigned(PleaseWaitDlg) and PleaseWaitDlg.Visible;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TPleaseWait.SetPercentDone(Value: Integer);
begin
  Progress := Value;
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg.PercentDone := Value;
  {$ENDIF}
end;

function TPleaseWait.GetPercentDone:Integer;
begin
  Result := Progress;
end;

function TPleaseWait.GetCurrentAction:AnsiString;
begin
 Result := CurrentAction;
end;

function TPleaseWait.GetIsCancel: Boolean;
begin
  Result := Canceled;
  {$IFDEF VISUAL_BUILD}
  Result := PleaseWaitDlg.IsCancel;
  {$ENDIF}
end;

procedure TPleaseWait.SetVisible(AValue: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
    if Assigned(PleaseWaitDlg) then
      PleaseWaitDlg.Visible := AValue;
  {$ENDIF}
end;

procedure TPleaseWait.SetShowCancel(Value: Boolean);
begin
  ShowCancel := value;
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg.SetShowCancel(value);
  {$ENDIF}
end;

procedure TPleaseWait.SetToMarqueeMode;
begin
  {$IFDEF VISUAL_BUILD}
  PleaseWaitDlg.SetToMarqueeMode;
  {$ENDIF}
end;

procedure TPleaseWait.UseProgressTimer(Value: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(PleaseWaitDlg) then
    PleaseWaitDlg.UseProgressTimer(value);
  {$ENDIF}
end;

end.
