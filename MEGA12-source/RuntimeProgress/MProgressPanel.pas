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

unit MProgressPanel;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, {$IFDEF VISUAL_BUILD}Controls, Forms, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,{$ENDIF} MegaConsts;

type
  TProgressPanel = class(TObject)
  private
      FCancelled: Boolean;
    protected
      {$IFDEF VISUAL_BUILD}
      FGauge: TProgressBar;
      FLabel: TLabel;
      {$ENDIF}
      FLastUpdateTime: TDateTime;
    public
      CheckCancelFunc: TCheckCancelFunc;

      constructor Create({$IFDEF VISUAL_BUILD}aGauge: TProgressBar; aLabel: TLabel{$ENDIF});
      destructor Destroy; override;

      function CheckCancel(Progress: Integer; Status: AnsiString): Boolean;

      property Cancelled: Boolean read FCancelled write FCancelled;
  end;
implementation

uses
  math, {$IFDEF VISUAL_BUILD}Graphics,{$ENDIF} DateUtils;

{ TProgressPanel }

function TProgressPanel.CheckCancel(Progress: Integer; Status: AnsiString): Boolean;
begin
  if FCancelled then
  begin
    Result := True;
    Exit;
  end;

  { sometimes we need to check if the user cancelled when we don't really know where progress is precisely}
  if Progress = CHECK_CANCEL_NO_PROG_VAL then
  begin
    Result := FCancelled;
    Exit;
  end;
  Result := False;
  if (MillisecondsBetween(Now, FLastUpdateTime) < SUBTASK_PROG_UPDATE_INTERVAL) and (Progress <> 100) {$IFDEF VISUAL_BUILD}and (FLabel.Caption = Status){$ENDIF} then
    Exit;
  {$IFDEF VISUAL_BUILD}
  FGauge.Position := Min(100, Progress);
  if Trim(Status) <> EmptyStr then
  begin
    FLabel.Caption := Status;
    FLabel.Refresh;
  end;
  FGauge.Refresh;
  if FGauge.Position = 100 then
  begin
    FGauge.Position := 0;
    FLabel.Caption := EmptyStr;
  end;
  {$ENDIF}
  Result := FCancelled;
  FLastUpdateTime := Now;
end;

constructor TProgressPanel.Create({$IFDEF VISUAL_BUILD}aGauge: TprogressBar; aLabel: TLabel{$ENDIF});
begin
  FLastUpdateTime := Now - 1000;
  FCancelled := False;
  {$IFDEF VISUAL_BUILD}
  FGauge := aGauge;
  FLabel := aLabel;
  {$ENDIF}
  CheckCancelFunc := Self.CheckCancel;
end;

destructor TProgressPanel.Destroy;
begin
  inherited;
end;

end.
