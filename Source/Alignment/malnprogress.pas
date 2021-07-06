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

unit MAlnProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Buttons, mimageform;

type

  { TClustalWProgressDlg }

  TClustalWProgressDlg = class(TForm)
    CancelBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    TimeLabel: TLabel;
    PairwiseProgressBar: TProgressBar;
    MultipleProgressBar: TProgressBar;
    Timer1: TTimer;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FIsClosing: Boolean;
    FStartTime: TDateTime;
    function RuntimeString: String;
  public
    { Public declarations }
    {$IFDEF VISUAL_BUILD}
    Thread : TThread;
    {$ELSE}
	Thread: TMEGAThread;
    {$ENDIF}
  end;

var
  ClustalWProgressDlg: TClustalWProgressDlg;

implementation

uses
  dateutils;

{$R *.lfm}

{ TClustalWProgressDlg }

procedure TClustalWProgressDlg.FormCreate(Sender: TObject);
begin
  FIsClosing := False;
  PairwiseProgressBar.BarShowText := True;
  MultipleProgressBar.BarShowtext := True;
  ImageForm.UpdateImgList(Self);
  FormStyle := fsStayOnTop;
end;

procedure TClustalWProgressDlg.FormShow(Sender: TObject);
begin
  FStartTime := Now;
  Timer1.Enabled := True;
end;

procedure TClustalWProgressDlg.Timer1Timer(Sender: TObject);
begin
  if FIsClosing then
    Exit;
  TimeLabel.Caption := RuntimeString;
end;

function TClustalWProgressDlg.RuntimeString: String;
begin
  Result := Format('ALIGNMENT BY CLUSTALW (%.2d:%.2d:%.2d)', [HoursBetween(Now, FStartTime), MinutesBetween(Now, FStartTime) mod 60, SecondsBetween(Now, FStartTime) mod 60]);
end;

procedure TClustalWProgressDlg.CancelBtnClick(Sender: TObject);
begin
  Timer1.Enabled := False;
  Thread.Terminate;
end;

procedure TClustalWProgressDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FIsClosing := True;
  Timer1.Enabled := False;
end;

end.

