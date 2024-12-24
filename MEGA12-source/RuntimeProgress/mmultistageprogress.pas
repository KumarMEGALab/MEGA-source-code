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

unit MMultiStageProgress;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

  LCLIntF, LCLType, SysUtils, Variants, Classes,Graphics,
  Controls, Forms, Dialogs, Grids, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, syncobjs;

type

  {
    @author: Glen Stecher
    A progress dialog that can display overall progress as well as progress for
    sub-tasks. The dialog can also run single-stage mode as well as run in marquee
    mode for the cases when progress cannot be calculated.

    Notes:
      1) The owner should always call StartProgress and StopProgress to enable/disable
         the timers.
      2) If the user clicks cancel, the dialog does not go away, it is the responsibility
         of the owner to either hide or free the dialog. The owner can check the IsCancelled
         variable directly or check the return value of different update functions. The
         update functions will return false if the user has cancelled.
      3) To run in single-stage mode, call HideCurrentOpProgress. This will hide the
         lower group box and resize the form appropriately.  To switch back to multi-stage
         mode, either call SetToProgressMode or free and recreate the form.
      4) If you don't have a way to cancel an operation (for instance, when writing an
         Excel file using TXLSFile) you should disable the CancelButton.
      5) The synchronization of the overall progress and progress of subtasks can be done
         like this:

           a) Estimate the proportion of time each subtask will require. For example, a calculation
              has 3 sub-tasks; the first takes 25% of the calculation time, the second takes
              50%, and the third 25%.
           b) When updating progress for the first task, the CurrentOpProgress should
              be set to the real progress for that sub-task. The OverallProgress should be
              set to Trunc(CurrentOpProgress * .25).
           c) When updating progress for the second task, the CurrentOpProgress should be
              set to the real progress for the sub-task and the OverallProgress souuld be set
              to Trunc(25 + CurrentOpProgress * .5).
           d) For the last sub-task, CurrentOpProgress is the real progress for that task
              and OverallProgress should be Trun(25 + 50 + CurrentOpProgress * .25).
  }

  { TMultiStageProgressForm }

  TMultiStageProgressForm = class(TForm)
    CancelButton: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    OverallProgressPanel: TPanel;
    OverallStatusPanel: TPanel;
    OverallProgressBar: TProgressBar;
    OverallProgressGrid: TStringGrid;
    CurrentOpProgressPanel: TPanel;
    CurrentOpStatusPanel: TPanel;
    CurrentOpProgressBar: TProgressBar;
    CurrentOpProgressGrid: TStringGrid;
    OverallProgressTimer: TTimer;
    MarqueeTimer: TTimer;
    OverallProgressLabel: TLabel;
    CurrentOpProgressLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OverallProgressTimerTimer(Sender: TObject); // for updating the run time
    procedure CancelButtonClick(Sender: TObject);
    procedure MarqueeTimerTimer(Sender: TObject); // for the case where progress cannot be calculated, we just run in marquee mode
    procedure OverallProgressGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure CurrentOpProgressGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
  private
    FLastUpdateTime: TDateTime;
    FIsMarqueeMode: Boolean;
    FMarqueeProgress: Integer;
    FStartTime: TDateTime;
    FRunTime: TDateTime;
    FNoSelectRect: TGridRect; // a hack so no tgrid cells will be highlighted when focus is moved elsewhere
  public
    CancelNotify: TNotifyEvent;
    IsCancelled: Boolean; // it can be checked without updating progress, for instance in marquee mode
    procedure StartProgress(OverallStatus: String; CurrentOpStatus: String); // resets everything
    procedure StopProgress; // stops the timers so compute cycles are not wasted. Use this when you want to hide the dlg instead of freeing it
    procedure ExcelProgressNotify(ProgressRate: Double); // set TXLSFile.OnProgress := ExcelProgressNotify and you can get the actual progress when writing Excel files

    { when using this form for showing progress of saving a graphics to a file, use the two below}
    procedure InitForSaveGraphicProgress;

    { Set TDownloadURL.OnDownloadProgress := DownloadUrlProgressNotify and you can
      get the actual progress when using TDownloadURL. Also, this enables the
      user to cancel the download. If the user clicks cancel, then TDownloadURL
      gets the message when it fires the on progress event. ***NOTE: you should
      call HideCurrentOpProgress when using this as only the overall progress gets
      updated.}
    //procedure DownloadUrlProgressNotify(Sender: TDownLoadURL;
    //                                    Progress: Cardinal;
    //                                    ProgressMax: Cardinal;
    //                                    StatusCode: TURLDownloadStatus;
    //                                    StatusText: String; var Cancel: Boolean);
    procedure HideCurrentOpProgress; // this allows to run in single stage mode (then you only update the overall progress)
    procedure HideCancelButton;
    procedure SetToMarqueeMode; // will run the form without progress updates, progress will auto-increment
    procedure SetToProgressMode; // in this mode, you must provide the progress updates
    function UpdateOverallProgress(Progress: Integer): Boolean; // returns false if user has cancelled
    function UpdateCurrentOpProgress(Progress: Integer): Boolean; // returns false if user has cancelled
    function UpdateCurrentOpStatus(Status: String): Boolean; // returns false if user has cancelled
    function SetOverallStatus(Status: String): Boolean;
  end;

  TOperationDoneCallback = procedure of object;
  TUserCancelledCallback = procedure of object;

  TMarqueeProgressThread = class(TThread)
    private
      FProgress: TMultiStageProgressForm;
      FStatusMsg: String;

      procedure SetStatusMsg(const Value: String);
      procedure DoTimer;
      procedure FreeProgress;
      procedure HideProgress;
      procedure ShowProgress;
      procedure InitCritSection;
    public


      OperationDoneCallback: TOperationDoneCallback;
      UserCancelledCallback: TUserCancelledCallback;
      constructor Create(AOwner: TComponent; CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;

      property StatusMsg: String read FStatusMsg write SetStatusMsg;
      property ProgressForm: TMultiStageProgressForm read FProgress write FProgress;
  end;
var
  MultiStageProgressForm: TMultiStageProgressForm;
  MarqueeProgressCriticalSection: TCriticalSection;
  TaskCompleted: Boolean; // this should only be accessed inside of MarqueeCriticalSection

implementation

uses
  Math, MegaUtils, dateUtils;

{$R *.lfm}

procedure TMultiStageProgressForm.CancelButtonClick(Sender: TObject);
begin
  IsCancelled := True;
  CancelButton.Enabled := False;
  OverallProgressGrid.Cells[1, 1] := 'Cancelling, please wait...';
  if Assigned(CancelNotify) then
    CancelNotify(CancelButton);
end;

procedure TMultiStageProgressForm.CurrentOpProgressGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if not Visible then Exit;
  CurrentOpProgressGrid.Selection := FNoSelectRect; // so the string grids won't highlight a cell when focus moves to another dialog
  if gdFixed in State then //if is fixed use the clBtnFace color
    Canvas.Brush.Color := clBtnFace
  else
    Canvas.Brush.Color := clWindow;

  Canvas.FillRect(Rect);
  Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, CurrentOpProgressGrid.Cells[ACol, ARow]);
end;

//procedure TMultiStageProgressForm.DownloadUrlProgressNotify(Sender: TDownLoadURL;
//                                                            Progress: Cardinal;
//                                                            ProgressMax: Cardinal;
//                                                            StatusCode: TURLDownloadStatus;
//                                                            StatusText: String;
//                                                            var Cancel: Boolean);
//var
//  AProgress: Integer;
//begin
//  if not (StatusCode = dsDownloadingData) then
//    Exit;
//
//  AProgress := Trunc(Progress/ProgressMax * 100);
//  OverallProgressBar.Position := Min(100, AProgress);
//  OverallProgressLabel.Caption := IntToStr(Min(100, AProgress)) + '%';
//  Cancel := IsCancelled;
//  OverallProgressGrid.Cells[1,1] := StatusText;
//end;

{ This is for getting progress from TXLSFile. To use it, set
  TXLSFile.OnProgress := ProgressForm.ExcelProgressNotify.
  Then, TXLSFile will update the progress. Be sure to set CancelButton.Enabled := False
  because there is no way to stop TXLSFile once it is writing a file. }
procedure TMultiStageProgressForm.ExcelProgressNotify(ProgressRate: Double);
begin
  CurrentOpProgressBar.Position := Min(100, Trunc(ProgressRate * 100));
end;

procedure TMultiStageProgressForm.FormCreate(Sender: TObject);
{$IFDEF MYPEG_ONLY}
var
  IconFile: String;
{$ENDIF}
begin
  CancelNotify := nil;
  FIsMarqueeMode := False;
  FStartTime := Time;
  IsCancelled := False;
  with OverallProgressGrid do
  begin
    ColWidths[1] := ClientWidth - ColWidths[0] - 2 * GridLineWidth;
    Cells[0,0] := 'Run Time';
    Cells[0,1] := 'Status';
    Cells[1,0] := Format('%.2d:%.2d:%.2d', [0, 0, 0]);
    Cells[1,1] := 'Idle';
    Invalidate;
  end;

  With CurrentOpProgressGrid do
  begin
    ColWidths[1] := ClientWidth - ColWidths[0] - 2 * GridLineWidth;
    Cells[0,0] := 'Status';
    Cells[1,0] := 'Idle';
    Invalidate;
  end;

  {$IFDEF MYPEG_ONLY}
    IconFile := GetPrivateFile(mfMyPegIconFile);
    if FileExists(IconFile) then
      Self.Icon.LoadFromFile(IconFile);
  {$ENDIF}

  FNoSelectRect.Top := -1;
  FNoSelectRect.Bottom := -1;
  FNoSelectRect.Left := -1;
  FNoSelectRect.Right := -1;
end;

procedure TMultiStageProgressForm.FormDestroy(Sender: TObject);
begin
  if MarqueeTimer.Enabled then
    MarqueeTimer.Enabled := False;
  if OverallProgressTimer.Enabled then
    OverallProgressTimer.Enabled := False;
end;

procedure TMultiStageProgressForm.HideCancelButton;
begin
  Height := Height - CancelButton.Height;
  CancelButton.Visible := False;
end;

procedure TMultiStageProgressForm.HideCurrentOpProgress;
var
  Delta: Integer;
begin
  if GroupBox2.Visible then
  begin
    Delta := GroupBox2.Height;
    Self.Height := Self.Height - Delta;
    GroupBox2.Visible := False;
    GroupBox1.Caption := EmptyStr;
    CancelButton.Top := (GroupBox1.Top + GroupBox1.Height + 2);
    OverallProgressGrid.Invalidate;
  end;
end;

procedure TMultiStageProgressForm.InitForSaveGraphicProgress;
begin
  StartProgress('Saving image file', 'preparing image');
  GroupBox2.Visible := False;
  SetToProgressMode;
end;

procedure TMultiStageProgressForm.MarqueeTimerTimer(Sender: TObject);
var
  CurrentTime: TDateTime;
  TimeStr: String;
  Hour: Word;
  Minute: Word;
  Second: Word;
  Millisecond: Word;
begin
  CurrentTime := Time;
//  if MillisecondsBetween(CurrentTime, FLastUpdateTime) < 300 then
//    Exit;
  inc(FMarqueeProgress, 1);
  if FMarqueeProgress >= 100 then
    FMarqueeProgress := 0;
  OverallProgressBar.Position := FMarqueeProgress;
  FLastUpdateTime := CurrentTime;
  FRunTime := CurrentTime - FStartTime;
  DecodeTime(FRunTime, Hour, Minute, Second, Millisecond);
  TimeStr := Format('%.2d:%.2d:%.2d', [Hour, Minute, Second]);
  OverallProgressGrid.Cells[1, 0] := TimeStr;
  OverallProgressGrid.Invalidate;
  OverallProgressBar.Invalidate;
end;

procedure TMultiStageProgressForm.OverallProgressGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if not Visible then Exit;
  OverallProgressGrid.Selection := FNoSelectRect;
  if ACol = 0 then
    Canvas.Brush.Color := clBtnFace
  else
    Canvas.Brush.Color := clWindow;

  Canvas.FillRect(Rect);
  Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, OverallProgressGrid.Cells[ACol, ARow]);
end;

procedure TMultiStageProgressForm.OverallProgressTimerTimer(Sender: TObject);
var
  CurrentTime: TDateTime;
  TimeStr: String;
  Hour: Word;
  Minute: Word;
  Second: Word;
  Millisecond: Word;
begin
  if FIsMarqueeMode then
    Exit;
  CurrentTime := Time;
  FRunTime := CurrentTime - FStartTime;
  DecodeTime(FRunTime, Hour, Minute, Second, Millisecond);
  TimeStr := Format('%.2d:%.2d:%.2d', [Hour, Minute, Second]);
  OverallProgressGrid.Cells[1, 0] := TimeStr;
  OverallProgressGrid.Invalidate;
end;

function TMultiStageProgressForm.SetOverallStatus(Status: String): Boolean;
begin
  Result := not IsCancelled;
  if not IsCancelled then
  begin
    OverallProgressGrid.Cells[1,1] := Status;
    OverallProgressGrid.Invalidate;
  end;
end;

procedure TMultiStageProgressForm.SetToMarqueeMode;
begin
  FMarqueeProgress := 0;
  FIsMarqueeMode := True;
  OverallProgressLabel.Caption := EmptyStr;
  HideCurrentOpProgress;
  OverallProgressBar.Style := pbstNormal;
  OverallProgressBar.Max := 100;
  OverallProgressBar.Min := 0;
//  OverallProgressBar.Step := 5;
  OverallProgressBar.Position := 0;
  OverallProgressBar.Smooth := True;
  MarqueeTimer.Enabled := True;
  OverallProgressTimer.Enabled := False;
  FLastUpdateTime := Time;
  OverallProgressBar.Enabled := True;
end;

procedure TMultiStageProgressForm.SetToProgressMode;
var
  Delta: Integer;
begin
  FIsMarqueeMode := False;
  GroupBox1.Caption := 'Overall';
  if not GroupBox2.Visible then
  begin
    Delta := GroupBox2.Height;
    CancelButton.Top := CancelButton.Top + Delta;
    Self.Height := Self.Height + Delta;
    GroupBox2.Caption := 'Current Operation';
    GroupBox2.Visible := True;
  end;
  OverallProgressBar.Style := pbstNormal;
  OverallProgressLabel.Caption := '0%';
  OverallProgressTimer.Enabled := True;
  MarqueeTimer.Enabled := False;
end;

procedure TMultiStageProgressForm.StartProgress(OverallStatus: String; CurrentOpStatus: String);
begin
  OverallProgressTimer.Enabled := True;
  IsCancelled := False;
  CancelButton.Enabled := True;
  FStartTime := Time;
  OverallProgressBar.Position := 0;
  CurrentOpProgressBar.Position := 0;
  OverallProgressGrid.Cells[1, 1] := OverallStatus;
  CurrentOpProgressGrid.Cells[1, 0] := CurrentOpStatus;
  if GroupBox2.Visible then
    CurrentOpProgressGrid.Invalidate;
  OverallProgressGrid.Invalidate;
end;

procedure TMultiStageProgressForm.StopProgress;
begin
  MarqueeTimer.Enabled := False;
  OverallProgressTimer.Enabled := False;
  IsCancelled := False;
  CancelButton.Enabled := True;
  FStartTime := Time;
  OverallProgressBar.Position := 0;
  CurrentOpProgressBar.Position := 0;
  OverallProgressGrid.Cells[1, 1] := 'Idle';
  CurrentOpProgressGrid.Cells[1, 0] := 'Idle';
  if GroupBox2.Visible then
    CurrentOpProgressGrid.Invalidate;
  OverallProgressGrid.Invalidate;
end;

function TMultiStageProgressForm.UpdateCurrentOpProgress(Progress: Integer): Boolean;
var
  AProgress: Integer;
begin
  Result := not IsCancelled;
  if not IsCancelled then
  begin
    AProgress := Min(Progress, 100);
    CurrentOpProgressBar.Position := AProgress;
    CurrentOpProgressLabel.Caption := IntToStr(AProgress) + '%';
  end;
end;

function TMultiStageProgressForm.UpdateCurrentOpStatus(Status: String): Boolean;
begin
  Result := not IsCancelled;
  if not IsCancelled then
  begin
    CurrentOpProgressGrid.Cells[1,0] := Status;
    CurrentOpProgressBar.Position := 0;
    CurrentOpProgressGrid.Invalidate;
  end;
end;

function TMultiStageProgressForm.UpdateOverallProgress(Progress: Integer): Boolean;
var
  AProgress: Integer;
begin
  Result := not IsCancelled;
  if not IsCancelled then
  begin
    AProgress := Min(Progress, 100);
    OverallProgressBar.Position := AProgress;
    OverallProgressLabel.Caption := IntToStr(AProgress) + '%';
    OverallProgressBar.Invalidate;
    OverallProgressLabel.Invalidate;
  end;
end;

{ TMarqueeProgressThread }

constructor TMarqueeProgressThread.Create(AOwner: TComponent; CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
//  FProgress := TMultiStageProgressForm.Create(nil);
  Synchronize(InitCritSection);
end;

destructor TMarqueeProgressThread.Destroy;
begin
  MarqueeProgressCriticalSection.Free;
  Synchronize(HideProgress);
  inherited;
end;

procedure TMarqueeProgressThread.DoTimer;
begin
  try
    MarqueeProgressCriticalSection.Acquire;
    if TaskCompleted then
    begin
      Terminate;
    end;
  finally
    MarqueeProgressCriticalSection.Release;
  end;
end;

procedure TMarqueeProgressThread.Execute;
begin
  Synchronize(ShowProgress);
  while not Terminated do
  begin
    Synchronize(DoTimer);
    Sleep(200);
  end;
end;

procedure TMarqueeProgressThread.FreeProgress;
begin
  if Assigned(FProgress) then
  begin
    FProgress.Hide;
    FProgress.Free;
  end;
end;

procedure TMarqueeProgressThread.HideProgress;
begin
  if Assigned(FProgress) then
    FProgress.Hide;
end;

procedure TMarqueeProgressThread.InitCritSection;
begin
  MarqueeProgressCriticalSection := TCriticalSection.Create;
  try
    MarqueeProgressCriticalSection.Acquire;
    TaskCompleted := False;
  finally
    MarqueeProgressCriticalSection.Release;
  end;
end;

procedure TMarqueeProgressThread.SetStatusMsg(const Value: String);
begin
  FStatusMsg := Value;
  FProgress.SetOverallStatus(FStatusMsg);
end;

procedure TMarqueeProgressThread.ShowProgress;
begin
  FProgress.UpdateCurrentOpStatus(FStatusMsg);
//  FProgress.SetToMarqueeMode;
//  FProgress.HideCancelButton;
//  FProgress.Show;
//  FProgress.BringToFront;
end;

end.
