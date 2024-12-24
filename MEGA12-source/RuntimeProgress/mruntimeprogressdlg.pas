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

unit mruntimeprogressdlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
  Grids,
  {$ENDIF}
  mworkflow_interfaces, LCLIntF, LCLType, dateutils,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, ExtCtrls, StdCtrls, Menus, MProgressPanel,
  mcancellable, syncobjs;

const
  CL_TAB = 2;
  HOFFSET = 30;
  VSPACING = 12;

{
 *******************************!!! WARNING !!!*********************************
   Under no circumstances should we ever directly create an instance of
   TRuntimeProgressDlg. Instead, we instantiate TRuntimeProgress which will
   create and manage an instance of TRuntimeProgressDlg as needed (any time
   the TRuntimeProgressDlg is needed, TRuntimeProgress will automatically
   delegate responsibilities to it).
 ***********************!!! YOU HAVE BEEN WARNED !!!****************************
}


type


  TUpdateRunStatusInfoEvent = procedure (AType, AInfo: String) of object;

  TRuntimeProgress = class;
  {$IFDEF VISUAL_BUILD}

  { TRuntimeProgressDlg }

  TRuntimeProgressDlg = class(TForm)
    AnalysisOptionsDisplay: TDrawGrid;
    AnalysisOptionsPanel: TPanel;
    CmdLineOut: TMemo;
    StdOutLabel: TLabel;
    StopBitBtn: TBitBtn;
    KeepOnTopChBox: TCheckBox;
    GaugeRzLabel: TLabel;
    ImageList1: TImageList;
    Copy2: TPopupMenu;
    Copy1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    RunStatusPanel: TPanel;
    PauseBitBtn: TBitBtn;
    PercentGauge: TProgressBar;
    ProgressScrollBox: TScrollBox;
    RunStatusStrGrid: TStringGrid;
    ScrollWithStatusUpdates1: TMenuItem;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    GaugePanel: TPanel;
    ProgressBar1: TProgressBar;
    StopBtn: TSpeedButton;
    StatusOptionsPage: TTabSheet;
    CommandLinePage: TTabSheet;
    StatusUpdateTimer: TTimer;
    RuntimeTimer: TTimer;
    AnalysisOptionsPage: TTabSheet;
    ShowDetailsToggle: TToggleBox;
    procedure AnalysisOptionsDisplayClick(Sender: TObject);
    procedure AnalysisOptionsDisplayDrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure AnalysisOptionsDisplaySelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KeepOnTopChBoxChange(Sender: TObject);
    procedure RuntimeTimerTimer(Sender: TObject);
    procedure ShowAnalysisOptsItemClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PauseBitBtnClick(Sender: TObject);
    procedure StatusUpdateTimerTimer(Sender: TObject);
  private
    FApplinkerProgressPlaceholder: String;
    FCancellables: TList;
    FNeedsTrayIcon: Boolean;
    FTrayIconAdded: Boolean;
    FStartTime: TDateTime;
    FOptionsPanelHeight: Integer;
    FNextGaugeTopCoord: Integer;
    FRuntimeProgress: TRuntimeProgress; // TRuntimeProgress runs the show and will automatically delegate to the TRuntimeProgressDlg when needed
    FLongestCmdLineEntry : Integer;
    FDataFileName       : String;
    FDataTitle          : String;
    FRunStatusStrList   :  TStringList;
    FAnalysisOptStrList : TStringList;
    FAnalysisOptionStrLen: Integer;
    FIsMarqueeMode: Boolean;
    FProgressPanels: TList;
    FUseTimer: Boolean;
    FOptionsDisplayStrings: TStringList;
    procedure NotifyCalculationCancelled;
    procedure AdjustColumnWidths;
    function GetTextForSettingsDisplayGrid(const aRow: Integer; const aCol: Integer; var isSectionHeader: Boolean): String;
    function RuntimeString: String;
    procedure SetApplinkerProgressPlaceholder(AValue: String);
    procedure UpdateBounds;
    function GetPercentProgress: Integer;
    function GetThread: TThread;
    function GetTimeInterval: Integer;
    procedure SetMarqueeMode(const Value: Boolean);
    procedure SetTimerInterval(AValue: Integer);
    procedure SetUseTimer(AValue: Boolean);
    procedure UpdateAnalysisOptsRichEdit;
    procedure UpdateRunStatusGridFixedColWidth(newStr: String);
    procedure AjdustWindowSize;
  public
    CheckAbortCriticalSection: TCriticalSection;
    LastUpdateTime: TDateTime;
    ThreadRunning      : Boolean;
    HasCmdLineOutput   : Boolean;
    FMAI : Pointer;// used by kumar to make TRuntimeDlg generic
    procedure RegisterCancellable(c: TCancellable);
    procedure UnregisterCancellable(c: TCancellable);
    procedure UpdateDataInfo(numTaxa, numSites, numGroups, numThreads: Integer);
    procedure SetFixedColWidthForStr(aString: String);
    procedure setRunStatusVisibility(AVisible: boolean);
    procedure ThreadDone(Sender: TObject); // for assignment to FThread OnTerminate
    procedure ThreadCancel(Sender: TObject);
    procedure ProcessMessages; // simply calls processmessages on the application
    // Call the following for updating progress from FThread.
    // Since, all Updates fns use VCL component, they must be called
    // in the synchronize method in the FThread to avoiding multi-thread conflicts. See VisualSwap for an
    procedure HideAnalysisOptions;
    procedure ShowAnalysisOptions;
    procedure UpdatePercentProgress(AValue: Integer);
    procedure UpdateRunStatusInfo(AType: AnsiString; AInfo: AnsiString);
    procedure ClearContents;
    procedure ClearAllButStartTime;
    function  GetRunStatusInfoIndex(AType: AnsiString): Integer;
    procedure UpdateRunStatusInfoByIndex(AType: Integer; AInfo: AnsiString);
    procedure AddCommandLine(NewLine: AnsiString);
    procedure DisplayError(ErrorStr: AnsiString);
    procedure DisplayOutputInEditor;
    function DoesContainCmdStatus(Status: AnsiString): Boolean;
    function AddSubTaskPanel(Status: AnsiString; Progress: Integer): TProgressPanel;
    function AddSubTaskPanelNoLabel(Progress: Integer): TProgressPanel;
    function GetHint: String;
    procedure RemoveRunStatusInfo(aNames: TStringList);
    property PercentProgress : Integer read GetPercentProgress;
    property Thread: TThread read GetThread;
    property RuntimeProgress: TRuntimeProgress read FRuntimeProgress write FRuntimeProgress;
    property AnalysisOptStrList: TStringList read FAnalysisOptStrList write FAnalysisOptStrList;
    property RunStatusStrList: TStringList read FRunStatusStrList write FRunStatusStrList;
    property DataFileName: String read FDataFileName write FDataFilename;
    property DataTitle: String read FDataTitle write FDataTitle;
    property IsMarqueeMode: Boolean read FIsMarqueeMode write SetMarqueeMode;
    property UseTimer: Boolean read FUseTimer write SetUseTimer;
    property TimerInterval: Integer read GetTimeInterval write SetTimerInterval;
    property NeedsTrayIcon: Boolean read FNeedsTrayIcon write FNeedsTrayIcon;
    property ApplinkerProgressPlaceholder: String read FApplinkerProgressPlaceholder write SetApplinkerProgressPlaceholder;
  end;
 {$ENDIF}

  { TRuntimeProgress }

  TRuntimeProgress = class(TObject)
   private
    FAutoAdvanceProgress: Boolean;
    FMarqueeTimer: TTimer;
    FMarqueeStrings: TStringList;
    FMarqueeCharIndex: Integer;
    FMarqueeProgressChars: String;
    FIsMarqueeMode: Boolean;
    FDataFileName      : String;
    FDataTitle         : String;
    FNeedsTrayIcon: Boolean;
    FProgress          : Integer;
    FStatus            : String;
    FLastRefreshTime: TDateTime;
    FThread: TThread; //This is the client thread associated with this TRuntimeProgress
    FRunStatusStrList: TStringList;
    FAnalysisOptStrList: TStringList;
    FAnalysisOptionStrLen: Integer;
    {$IFDEF VISUAL_BUILD}
    FRuntimeProgressDlg : TRuntimeProgressDlg;
    {$ENDIF}
    function GetApplinkerProgressPlaceholder: String;
    function GetHeight: Integer;
    function GetStdOutLabelCaption: String;
    function GetTimerInterval: Integer;
    function GetWidth: Integer;
    function LongestRunStatusString: String;
    function GetRepsCompletedForBootstrapThread(aThread: TThread): Integer;
    procedure SetApplinkerProgressPlaceholder(AValue: String);
    procedure SetAutoAdvanceProgress(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetNeedsTrayIcon(AValue: Boolean);
    procedure SetStdOutLabelCaption(AValue: String);
    procedure SetTimer(Value: TNotifyEvent);
    procedure SetThread(const Value: TThread);
    procedure SetDataFileName(const Value: String);
    procedure SetDataTitle(const Value: String);
    procedure SetTimerInterval(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure WriteProgressAndStatus(aType: String; aInfo: String);
    procedure SetMarqueeMode(const Value: Boolean);
    procedure MarqueeTimer(Sender: TObject);
    procedure InitMarqueeStrings;
    procedure UpdateDataInfo;
    {$IFNDEF VISUAL_BUILD}
    function ProgressString: String;
    {$ENDIF}
  public
    FMAI: Pointer;
    DisableTimerOnFirstUpdate: Boolean;
    constructor Create(Owner : TComponent=nil);
    destructor Destroy; override;
    procedure RegisterCancellable(c: TCancellable);
    procedure UnregisterCancellable(c: TCancellable);
    function GetUserCancelled: Boolean;
    procedure SetFixedColWidthForStr(AString: String);
    procedure SetProgress(progress : integer);
    function ProgressCheckCancel(Progress: Integer): Boolean;
    function ProgressAndStatusCheckCancel(Progress: Integer; AType: String; AInfo: String): Boolean;
    function ProgressAndStatusInfoCheckCancel(Progress: Integer; AInfo: String): Boolean;
    procedure ClearCommandLine;
    procedure SetTimerStatus(TimerOn: Boolean);
    function HasStatusInfo(AType: String): Boolean;
    procedure AddRunStatusInfo(AType: String; AInfo: String);
    procedure RemoveRunStatusInfo(AType: String); overload;
    procedure RemoveRunStatusInfo(aNames: TStringList); overload;
    procedure RemoveAllRunStatusInfo;
    procedure Show;
    procedure SetKeepOnTop(aValue: Boolean);
    procedure AddAnalysisOptions(AType, AInfo: String; showOptions: Boolean = True); overload;
    procedure AddAnalysisOptions(aOptions: TStringList; showOptions: Boolean = True); overload;

    procedure WriteAnalysisOptionsToStdOut();
    procedure AddCommandLine(NewLine: String);
    function AddCommandLineCheckCancel(NewLine: String): Boolean;
    procedure DisplayOutputInEditor;
    procedure DisplayCommandLineTab;
    function DoesContainCmdStatus(Status: String): Boolean;
    procedure setVisible(FVisible: Boolean);
    function GetIsVisible: Boolean;
    procedure Refresh();
    procedure SendToPipe(msg: String);
    procedure ProcessMessages;
    procedure RePaint;
    procedure SetHasCmdLineOutput(Cmd : Boolean);
    {$IFDEF VISUAL_BUILD}
    function VisualPercentGauge: TProgressBar;
    function VisualStopBtn: TSpeedButton;
    procedure SetStopButtonDown(Value: Boolean);
    {$ELSE}
    procedure UpdateStdOut;
    {$ENDIF}
    function GetThread: TThread;
    procedure Close;
    procedure Hide;
    procedure SetCaption(Caption : String);
    procedure HideAnalysisOptions;
    procedure ShowAnalysisOptions;
    procedure ThreadDone(Sender: TObject);
    procedure UpdateRunStatusInfo(AType, AInfo: String);

    procedure UpdatePercentProgress(Progressin: Integer);
    function AddSubTask(Status: AnsiString; Progress: Integer): TProgressPanel;
    function AddSubTaskNoLabel(Progress: Integer): TProgressPanel; deprecated; { needs updated for high dpi systems before it can be used}
    property DataFileName: String read FDataFileName write SetDataFileName;
    property DataTitle:    String read FDataTitle    write SetDataTitle;
    property AnalysisOptStrList: TStringList read FAnalysisOptStrList;
    property Progress: integer read FProgress write SetProgress;
    property UseTimer: boolean write SetTimerStatus;
    property TimerInterval: Integer read GetTimerInterval write SetTimerInterval;
    property OnTimer: TNotifyEvent write SetTimer;
    property Caption: String write SetCaption;
    property Visible: Boolean read GetIsVisible write SetVisible;
    property HasCmdLineOutput: Boolean write SetHasCmdLineOutput;
    property Thread: TThread read GetThread write SetThread;
    property IsMarqueeMode: Boolean read FIsMarqueeMode write SetMarqueeMode;
    {$IFDEF VISUAL_BUILD}
    property PercentGauge: TProgressBar read VisualPercentGauge;
    property StopBtn: TSpeedButton read VisualStopBtn;
    {$ENDIF}
    property NeedsTrayIcon: Boolean read FNeedsTrayIcon write SetNeedsTrayIcon;
    property UserCancelled: Boolean read GetUserCancelled;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
    property AutoAdvanceProgress: Boolean read FAutoAdvanceProgress write SetAutoAdvanceProgress;
    property ApplinkerProgressPlaceholder: String read GetApplinkerProgressPlaceholder write SetApplinkerProgressPlaceholder;
    property StdOutLabelCaption: String read GetStdOutLabelCaption write SetStdOutLabelCaption;
  end;

    procedure ShowRunStatusInfoStatic(AType, AInfo: String);
    procedure ShowProgressStatic(ProgressIn: Integer);
    procedure ShowProgressIncrementStatic;
 {$IFDEF VISUAL_BUILD}
var
  RuntimeProgressDlg: TRuntimeProgressDlg;
 {$ENDIF}

implementation

{$R *.lfm}

uses
  {$IFDEF DEBUG}mdeveloper_console,{$ENDIF}
  {$IFDEF VISUAL_BUILD}
  Mega_Main,  MEditorForm, mimageform, MTreeSearchThread, MegaVerConsts,
  MLSearchThread, mreltimethreads,  MAnalysisInfo, Applinker,
  {$ELSE}
  MD_MegaMain, StringUtils, mmega_std_out, Video,
  {$ENDIF}
  Math, MPTree, MegaConsts, MegaAnalysisPrefStrings, megautils;

{$IFDEF VISUAL_BUILD}

procedure TRuntimeProgressDlg.FormCreate(Sender: TObject);
begin
  StdOutLabel.Caption := EmptyStr;
  FCancellables := TList.Create;
  FNeedsTrayIcon := True;
  FTrayIconAdded := False;
  FProgressPanels := TList.Create;
  FOptionsDisplayStrings := TStringList.Create;
  FRunStatusStrList := nil;
  FAnalysisOptStrList:= nil;
  PercentGauge.Position := 0;

  StopBtn.AllowAllUp := True;
  StopBtn.GroupIndex := 1;
  FDataFileName := EmptyStr;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Progress';
  FNextGaugeTopCoord := PercentGauge.Top + PercentGauge.Height + 4;
  FOptionsPanelHeight := AnalysisOptionsPanel.Height;
  ImageForm.UpdateImgList(Self);
  FStartTime := MinDateTime;
  CheckAbortCriticalSection := TCriticalSection.Create;
  FormStyle := fsStayOnTop;
end;

procedure TRuntimeProgressDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  RuntimeTimer.Enabled := False;
  StatusUpdateTimer.Enabled := False;
  CloseAction := caFree;
  if FTrayIconAdded then
  begin
    MegaForm.RemoveWindowFromTray(Self);
    FTrayIconAdded := False;
  end;
end;

procedure TRuntimeProgressDlg.AnalysisOptionsDisplayDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aTxt: String;
  x, y: Integer;
  ts: TTextStyle;
  isSectionHeader: Boolean = False;
begin
  if not AnalysisOptionsDisplay.Visible then Exit;
  if (AnalysisOptionsDisplay.RowCount = 0) or (AnalysisOptionsDisplay.ColCount = 0) then Exit;
  with AnalysisOptionsDisplay.Canvas do
  begin
    ts := TextStyle;
    if aCol = 0 then
      Brush.Color := AnalysisOptionsDisplay.FixedColor
    else
      Brush.Color := clWhite;
    Brush.Style := bsSolid;
    FillRect(aRect);
    x := aRect.Left + 2;
    y := aRect.Top + 2;
    aTxt := GetTextForSettingsDisplayGrid(aRow, aCol, isSectionHeader);
    Brush.Style := bsClear;
    if isSectionHeader then
    begin
      ts.Alignment := taCenter;
      Font.Style := [fsBold];
      Font.Color := DEFAULT_ACTIVE_BG_COLOR;
    end
    else
    begin
      ts.Alignment := taLeftJustify;
      Font.Style := [];
      Font.Color := RunStatusStrGrid.Font.Color;
    end;
    if Trim(aTxt) <> EmptyStr then
      TextRect(aRect, x, y, aTxt, ts);
  end;
end;

procedure TRuntimeProgressDlg.AnalysisOptionsDisplayClick(Sender: TObject);
begin

end;

procedure TRuntimeProgressDlg.AnalysisOptionsDisplaySelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin

end;

procedure TRuntimeProgressDlg.FormActivate(Sender: TObject);
var
  scaleFactor: Double = 1;
begin
  if PixelsPerInch > DesignTimePPI then
    scaleFactor := PixelsPerInch/DesignTimePPI;
  FNextGaugeTopCoord := PercentGauge.Top + PercentGauge.Height + Round(12*scaleFactor);
end;

procedure TRuntimeProgressDlg.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FOptionsDisplayStrings) then
    FOptionsDisplayStrings.Free;
  if StatusUpdateTimer.Enabled then
    StatusUpdateTimer.Enabled := False;
  // we don't want to free anything owned by TRuntimeProgress
  FRuntimeProgress := nil;
  FRunStatusStrList := nil;
  FAnalysisOptStrList := nil;

  if Assigned(FProgressPanels) then
  begin
    if FProgressPanels.Count > 0 then
      for i := 0 to FProgressPanels.Count - 1 do
        TProgressPanel(FProgressPanels[i]).Free;
    FProgressPanels.Free;
  end;
  if Assigned(FCancellables) then
    FCancellables.Free;
  if Assigned(CheckAbortCriticalSection) then
    CheckAbortCriticalSection.Free;
end;

procedure TRuntimeProgressDlg.FormShow(Sender: TObject);
begin
  if CompareValue(FStartTime, MinDateTime, FP_CUTOFF) = 0 then
    FStartTime := Now;
  StatusUpdateTimer.Enabled := UseTimer;
  PageControl1.Pages[CL_TAB].TabVisible := HasCmdLineOutput;
  PageControl1.ActivePageIndex := 0;
  UpdateAnalysisOptsRichEdit;

  if RunStatusStrGrid.RowCount < 2 then
    setRunStatusVisibility(true);
  UpdateBounds;
  Invalidate;
  if FNeedsTrayIcon and (not FTrayIconAdded) then
  begin
    MegaForm.AddWindowToTray(Self, False);
    FTrayIconAdded := True;
  end;
end;

procedure TRuntimeProgressDlg.KeepOnTopChBoxChange(Sender: TObject);
begin
 if KeepOnTopChBox.Checked then
   FormStyle := fsStayOnTop
 else
   FormStyle := fsNormal;
end;

procedure TRuntimeProgressDlg.RuntimeTimerTimer(Sender: TObject);
var
  str: String = '';
begin
  if not Assigned(FRunStatusStrList) then
    Exit;
  if not Assigned(RunStatusStrGrid) then
    Exit;

  str := RuntimeString;
  if FRunStatusStrList.IndexOfName(OPERATION_RUN_TIME) < 0 then
  begin
    RunStatusStrGrid.RowCount := RunStatusStrGrid.RowCount + 1;
    FRunStatusStrList.Add(OPERATION_RUN_TIME + '=' + str);
    AjdustWindowSize;
  end
  else
    UpdateRunStatusInfo(OPERATION_RUN_TIME, str);

  if PageControl1.ActivePage = StatusOptionsPage then
    RunStatusStrGrid.Invalidate;

  if (FApplinkerProgressPlaceholder <> EmptyStr) and (PageControl1.ActivePage = CommandLinePage) then
  begin
    if FApplinkerProgressPlaceholder.EndsWith('...') then
      FApplinkerProgressPlaceholder := Copy(FApplinkerProgressPlaceholder, 1, Length(FApplinkerProgressPlaceholder) - 3)
    else
      FApplinkerProgressPlaceholder := FApplinkerProgressPlaceholder + '.';
    CmdLineOut.Lines[CmdLineOut.Lines.Count - 1] := Format('%s - %s', [str, FApplinkerProgressPlaceholder]);
    Invalidate;
  end;
end;

procedure TRuntimeProgressDlg.ThreadDone(Sender : TObject);
begin
  if fsModal in FormState then
    ModalResult := mrOK;
end;

procedure TRuntimeProgressDlg.ThreadCancel(Sender : TObject);
begin
  if fsModal in FormState then
    ModalResult := mrCancel;
end;

procedure TRuntimeProgressDlg.StopBtnClick(Sender: TObject);
var
  CurrentAnalysisClassName: String;
  RepsCompleted: Integer;
  Response: Integer;
  i: Integer;
  aThread: TThread = nil;
begin

  if FRuntimeProgress.GetThread = nil then
  begin
    StopBtn.Down := True;
    Hide;
    ProcessMessages;
    Exit;
  end;
  StatusBar1.Canvas.Font.Color := clRed;
  StatusBar1.SimpleText := 'Please wait while calculation threads terminate...';
  StopBitBtn.Enabled := False;

  {$IFDEF MSWINDOWS}
  { TODO 2 -oglen -cXE2 : the thread is not being suspended. TThread.Suspend is deprecated (unsafe)
                          but TThread does not provide a way to suspend. Instead, you have to
                          incorporate the logic into your TThread descendent class. }
  if Assigned(FRuntimeProgress.Thread) then
    FRuntimeProgress.GetThread.Suspend;
  {$ENDIF}
  CurrentAnalysisClassName := FRuntimeProgress.GetThread.ClassName;
  if SameText(CurrentAnalysisClassName, 'TBootstrapMPTreeSearchThread') then
  begin
    RepsCompleted := (FRuntimeProgress.GetThread as TBootstrapMPTreeSearchThread).RepsCompleted;
    if RepsCompleted >= MIN_NUM_BOOT_REPS then
    begin
      Response := MessageDlg(IntToStr(RepsCompleted) + ' reps completed. Use only completed reps for bootstrap tree construction?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Response = mrCancel then
      begin
        {$IFDEF MSWINDOWS}
        FRuntimeProgress.GetThread.Resume;
        {$ENDIF}
        StopBitBtn.Enabled := True;
        StopBtn.Down := False;
        Exit;
      end
      else if Response = mrYes then
      begin
        (FRuntimeProgress.GetThread as TBootstrapMPTreeSearchThread).PartiallyCompleted := True;
        TAnalysisInfo(FRuntimeProgress.FMAI).MyTreePack.BootReps := (FRuntimeProgress.GetThread as TBootstrapMPTreeSearchThread).RepsCompleted;
      end;
    end
    else
    begin
      if MessageDlg('Do you want to abort the process?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      begin
        {$IFDEF MSWINDOWS}
        FRuntimeProgress.GetThread.Resume;
        {$ENDIF}
        StopBitBtn.Enabled := True;
        StopBtn.Down := False;
        Exit;
      end;
    end;
  end
  else
  begin
    if MessageDlg('Do you want to abort the process?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    begin
      {$IFDEF MSWINDOWS}
      FRuntimeProgress.GetThread.Resume;
      {$ENDIF}
      StopBitBtn.Enabled := True;
      StopBtn.Down := False;
      Exit;
    end;
  end;
  {$IFDEF MSWINDOWS}
  FRuntimeProgress.GetThread.Resume;  // This is important KT
  {$ENDIF}

  aThread := FRuntimeProgress.GetThread;

  {$IFDEF VISUAL_BUILD}
  if aThread is TAppLinkThread then
    TAppLinkThread(aThread).TerminateAppLinkProcess;
  {$ENDIF}

  FRuntimeProgress.GetThread.Terminate;
  if FProgressPanels.Count > 0 then
    for i := 0 to FProgressPanels.Count - 1 do
      TProgressPanel(FProgressPanels[i]).Cancelled := True;


  if (aThread is TBootstrapMLThread) or
     (aThread is TMLTreeAnalyzeThread) or
     (aThread is TMLClockTestThread) or
     (aThread is TModelTestThread) or
     (aThread is TRelTimeMLThread) or
     (aThread is TMLTreeSearchThread) then
  begin
    (aThread as TMLTreeThread).Canceled := True;
  end;
  NotifyCalculationCancelled;
  Hide;
end;

procedure TRuntimeProgressDlg.UpdateRunStatusInfo(AType: AnsiString; AInfo: AnsiString);
var
  i: Integer;
begin
  if (AType = 'Status') and (Trim(AInfo) = EmptyStr) then
  begin
    Assert(False, 'empty status info');
  end;
  i := FRunStatusStrList.IndexOfName(AType);
  if i < 0 then
    Exit;
  try
    if RunStatusStrGrid.RowCount < FRunStatusStrList.Count then
    begin
      RunStatusStrGrid.RowCount := FRunStatusStrList.Count;
      if (PageControl1.ActivePage = StatusOptionsPage) then
        AjdustWindowSize;
    end;
    if RunStatusStrGrid.Cells[0, i] <> AType then
    begin
      RunStatusStrGrid.Cells[0,i] := AType;
      UpdateRunStatusGridFixedColWidth(AType);
    end;
    if RunStatusStrGrid.Cells[1,i] <> AInfo then
      RunStatusStrGrid.Cells[1,i] := AInfo;
    if MillisecondsBetween(Now, LastUpdateTime) > 1000 then
      LastUpdateTime := Now;
    RunStatusStrGrid.Invalidate;
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
        ShowMessage('Error in UpdateRunStatusInfo: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function  TRuntimeProgressDlg.GetRunStatusInfoIndex(AType: AnsiString): Integer;
begin
  Result := FRunStatusStrList.IndexOf(AType);
end;

function TRuntimeProgressDlg.GetThread: TThread;
begin
  Result := FRuntimeProgress.GetThread;
end;

function TRuntimeProgressDlg.GetTimeInterval: Integer;
begin
  Result := StatusUpdateTimer.Interval;
end;

procedure TRuntimeProgressDlg.UpdateRunStatusInfoByIndex(AType: Integer; AInfo: AnsiString);
begin
  with RunStatusStrGrid do
    if (AType >= 0) and (AType < RowCount) then
    begin
      Cells[1,AType] := AInfo;
      UpdateRunStatusGridFixedColWidth(Cells[0,AType]);
      Invalidate;
    end;
end;

procedure TRuntimeProgressDlg.UpdatePercentProgress(AValue: Integer);
begin
  PercentGauge.Position := AValue;
  PercentGauge.Invalidate;
end;

function TRuntimeProgressDlg.AddSubTaskPanel(Status: AnsiString; Progress: Integer): TProgressPanel;
var
  aProgPanel: TProgressPanel;
  aGauge: TProgressBar;
  aLabel: TLabel;
begin
  aGauge := TProgressBar.Create(Self);
  aGauge.Smooth := False;
  aGauge.Style := pbstNormal;
  aGauge.BarShowText := True;
  aGauge.Max := 100;
  aGauge.Step := 1;
  aGauge.Position := Progress;
  aGauge.Height := 14;
  aLabel := TLabel.Create(Self);
  aLabel.Caption := Status;
  aProgPanel := TProgressPanel.Create(aGauge, aLabel);
  FProgressPanels.Add(aProgPanel);
  Panel2.Height := Panel2.Height + aGauge.Height + VSPACING + aLabel.Height;
  aGauge.Parent := Panel2;
  aLabel.Parent := Panel2;
  aLabel.Top := FNextGaugeTopCoord;
  aLabel.Left := PercentGauge.Left + HOFFSET;
  aGauge.Top := aLabel.Top + aLabel.Height + VSPACING;
  aGauge.Left := aLabel.Left;
  aGauge.Width := PercentGauge.Width - HOFFSET;
  FNextGaugeTopCoord := aGauge.Top + aGauge.Height + VSPACING;
  Result := aProgPanel;
  GaugePanel.Height := Min(GaugePanel.Height + aGauge.Height + aLabel.Height + VSPACING, 350);
  if FProgressPanels.Count = 1 then
    GaugePanel.Height := GaugePanel.Height + 4;
  Refresh;
  Application.ProcessMessages;
end;

function TRuntimeProgressDlg.AddSubTaskPanelNoLabel(Progress: Integer): TProgressPanel;
var
  aProgPanel: TProgressPanel;
  aGauge: TProgressBar;
  aLabel: TLabel;
begin
  aLabel := nil;
  aGauge := TProgressBar.Create(Self);
  aGauge.Position := Progress;
  aGauge.Height := 14;
  if FProgressPanels.Count = 0 then
  begin
    aLabel := TLabel.Create(Self);
    aLabel.Caption := 'Subtask Progress:';
    aProgPanel := TProgressPanel.Create(aGauge, aLabel);
  end
  else
    aProgPanel := TProgressPanel.Create(aGauge, nil);
  FProgressPanels.Add(aProgPanel);

  aGauge.Parent := Panel2;
  if Assigned(aLabel) then
  begin
    aLabel.Parent := Panel2;
    aLabel.Top := FNextGaugeTopCoord;
    aLabel.Left := PercentGauge.Left + HOFFSET;
    aGauge.Top := aLabel.Top + aLabel.Height + VSPACING;
    Panel2.Height := Panel2.Height + aGauge.Height + aLabel.Height + VSPACING;
  end
  else
  begin
    aGauge.Top := FNextGaugeTopCoord;
    Panel2.Height := Panel2.Height + aGauge.Height + VSPACING;
  end;

  FNextGaugeTopCoord := aGauge.Top + VSPACING;
  aGauge.Left := PercentGauge.Left + HOFFSET;
  aGauge.Width := PercentGauge.Width - HOFFSET;
  Result := aProgPanel;
  if Assigned(aLabel) then
    GaugePanel.Height := Min(GaugePanel.Height + aGauge.Height + aLabel.Height + VSPACING, 350)
  else
    GaugePanel.Height := Min(GaugePanel.Height + aGauge.Height + VSPACING, 350);
  if FProgressPanels.Count = 1 then
    GaugePanel.Height := GaugePanel.Height + VSPACING;
  Refresh;
  Application.ProcessMessages;
end;

function TRuntimeProgressDlg.GetHint: String;
begin
  Result := Format('%d%% ', [PercentGauge.Position]);
  if FAnalysisOptStrList.IndexOfName(opsPhylo2) >= 0 then
    Result += FAnalysisOptStrList.Values[opsPhylo2];
end;

procedure TRuntimeProgressDlg.RemoveRunStatusInfo(aNames: TStringList);
var
  i: Integer = -1;
  index: Integer;
  numRemoved: Integer = 0;
begin
  if FRunStatusStrList.Count > 0 then
    for i := 0 to aNames.Count - 1 do
    begin
      index := FRunStatusStrList.IndexOfName(aNames[i]);
      if index >= 0 then
      begin
        FRunStatusStrList.Delete(index);
        inc(numRemoved);
      end;
    end;
  if numRemoved > 0 then
  begin
    RunStatusStrGrid.RowCount := Max(1, RunStatusStrGrid.RowCount - numRemoved);
    if FRunStatusStrList.Count > 0 then
      for i := 0 to FRunStatusStrList.Count - 1 do
      begin
        RunStatusStrGrid.Cells[0,i] := FRunStatusStrList.Names[i];
        RunStatusStrGrid.Cells[1, i] := FRunStatusStrList.ValueFromIndex[i];
      end;
    RunStatusStrGrid.Invalidate;
  end;
end;

procedure TRuntimeProgressDlg.HideAnalysisOptions;
begin
  PageControl1.Visible := False;
  ShowDetailsToggle.Checked := False;
  AjdustWindowSize;
  Invalidate;
end;

procedure TRuntimeProgressDlg.ShowAnalysisOptions;
begin
  PageControl1.Visible := True;
  AnalysisOptionsPage.TabVisible := (AnalysisOptionsDisplay.RowCount > 0);
  ShowDetailsToggle.Checked := True;
  UpdateAnalysisOptsRichEdit;
  AjdustWindowSize;
  Invalidate;
end;

procedure TRuntimeProgressDlg.ShowAnalysisOptsItemClick(Sender: TObject);
begin
  PageControl1.Visible := (not PageControl1.Visible);
  AjdustWindowSize;
  Invalidate;
end;

procedure TRuntimeProgressDlg.FormResize(Sender: TObject);
begin
  AdjustColumnWidths;
  if AnalysisOptionsPanel.Visible then
    FOptionsPanelHeight := AnalysisOptionsPanel.Height;
end;

procedure TRuntimeProgressDlg.AddCommandLine(NewLine: AnsiString);
begin
  if CmdLineOut <> nil then
    CmdLineOut.Append(NewLine);
end;

procedure TRuntimeProgressDlg.ClearAllButStartTime;
begin
  RunStatusStrGrid.RowCount := 1;
end;

procedure TRuntimeProgressDlg.ClearContents;
var
  i: Integer;
begin
  try
    for i := 0 to RunStatusStrGrid.ColCount - 1 do
      RunStatusStrGrid.Cols[i].Clear;
    RunStatusStrGrid.RowCount := 1;
  except
    // its not worth killing an analysis
  end;
end;

procedure TRuntimeProgressDlg.Copy1Click(Sender: TObject);
begin
  CmdLineOut.SelectAll;
  CmdLineOut.CopyToClipboard;
  CmdLineOut.ClearSelection;
end;

procedure TRuntimeProgressDlg.DisplayOutputInEditor;
begin
  OpenStringList(CmdLineOut.Lines, 'Command Line Output');
end;

procedure TRuntimeProgressDlg.FormHide(Sender: TObject);
begin
  if StatusUpdateTimer.Enabled then
    StatusUpdateTimer.Enabled := False;
  CmdLineOut.Clear;
  if FTrayIconAdded then
  begin
    MegaForm.RemoveWindowFromTray(Self);
    FTrayIconAdded := False;
  end;
end;

procedure TRuntimeProgressDlg.PauseBitBtnClick(Sender: TObject);
begin
  PauseBitBtn.Glyph := nil;
  if PauseBitBtn.Caption = '&Pause' then
    PauseBitBtn.Caption := '&Resume'
  else
    PauseBitBtn.Caption := '&Pause';
  Application.ProcessMessages;
end;

function TRuntimeProgressDlg.DoesContainCmdStatus(Status: AnsiString): Boolean;
begin
  Result := (CmdLineOut.Lines.IndexOf(Status) >= 0);
end;

function TRuntimeProgressDlg.GetPercentProgress: Integer;
begin
  result := PercentGauge.Position;
end;

procedure TRuntimeProgressDlg.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TRuntimeProgressDlg.SetMarqueeMode(const Value: Boolean);
begin
  if FIsMarqueeMode = Value then
    Exit;
  FIsMarqueeMode := Value;
  if FIsMarqueeMode then
  begin
    PercentGauge.Style := pbstMarquee;
    StatusUpdateTimer.OnTimer := StatusUpdateTimerTimer;
    StatusUpdateTimer.Enabled := True;
  end
  else
  begin
    ClearAllButStartTime;
    StatusUpdateTimer.OnTimer := StatusUpdateTimerTimer;
    StatusUpdateTimer.Enabled := False;
    PercentGauge.Style := pbstNormal;
    RunStatusStrGrid.Invalidate;
  end;
end;

procedure TRuntimeProgressDlg.SetTimerInterval(AValue: Integer);
begin
  StatusUpdateTimer.Interval := AValue;
end;

procedure TRuntimeProgressDlg.SetUseTimer(AValue: Boolean);
begin
  if FUseTimer=AValue then Exit;
  FUseTimer:=AValue;
  StatusUpdateTimer.Enabled := AValue;
end;

procedure TRuntimeProgressDlg.UpdateAnalysisOptsRichEdit;
var
  line: Integer;
  TempStr: String;
  tokens: TStringList = nil;
  token: String;
  key: String;
begin
  FAnalysisOptionStrLen := 15;
  try
    tokens := TStringList.Create;
    if FAnalysisOptStrList.Count > 0 then
    begin
      for line := 0 to FAnalysisOptStrList.Count-1 do
      begin
        token := FAnalysisOptStrList[line];
        if (Pos('[fsBold]', token) > 0) then
        begin
          TempStr := Copy(token, 0, Pos('[fsBold]', token) - 1);
          if FOptionsDisplayStrings.IndexOf(Trim(TempStr)) < 0 then
            FOptionsDisplayStrings.Add(Trim(TempStr));
        end
        else if (Pos('====', token) > 0) then
        begin
          TempStr := Copy(token, 0, Pos('=', token) - 1);
          if FOptionsDisplayStrings.IndexOf(Trim(TempStr)) < 0 then
            FOptionsDisplayStrings.Add(Trim(TempStr));
        end
        else
        begin
          SplitStr(token, '=', tokens, true);
          if tokens.Count = 2 then
            TempStr := Format('%-' + IntToStr(FAnalysisOptionStrLen) + 's = %s', [Trim(tokens[0]), tokens[1]])
          else
            TempStr := Trim(token);
          if FOptionsDisplayStrings.IndexOf(Trim(TempStr)) < 0 then
            FOptionsDisplayStrings.Add(Trim(TempStr));
        end;
        AnalysisOptionsDisplay.RowCount := FOptionsDisplayStrings.Count;
        AnalysisOptionsDisplay.Invalidate;
      end;

      if FOptionsDisplayStrings.Count > 0 then
      begin
        for line := 0 to FOptionsDisplayStrings.Count - 1 do
        begin
          token := FOptionsDisplayStrings[line];
          if (Pos('[fsBold]', token) > 0) or (Pos('====', token) > 0) or (Pos('analysis', LowerCase(token)) > 0) then
            continue;
          SplitStr(token, '=', tokens, true);
          key := tokens[0];
          if Length(Trim(key)) > FAnalysisOptionStrLen then
            FAnalysisOptionStrLen := Length(Trim(key));
        end;

        AnalysisOptionsDisplay.ColWidths[0] := AnalysisOptionsDisplay.Canvas.TextWidth('n')*(FAnalysisOptionStrLen + 1);
        AnalysisOptionsDisplay.ColWidths[1] := AnalysisOptionsDisplay.Width - AnalysisOptionsDisplay.ColWidths[0];
        RunStatusStrGrid.ColWidths[0] := AnalysisOptionsDisplay.ColWidths[0];
        RunStatusStrGrid.ColWidths[1] := RunStatusStrGrid.Width - RunStatusStrGrid.ColWidths[0];
      end;
    end;
  finally
    if Assigned(tokens) then
      tokens.Free;
  end;
end;

procedure TRuntimeProgressDlg.UpdateRunStatusGridFixedColWidth(newStr: String);
var
  aStr: String = '';
  row: Integer;
begin
  if RunStatusStrGrid.RowCount > 0 then
    for row := 0 to RunStatusStrGrid.RowCount - 1 do
    begin
      if Length(RunStatusStrGrid.Cells[0, row]) > Length(newStr) then
        aStr := RunStatusStrGrid.Cells[0, row];
    end;
  try
    BeginFormUpdate;
    if aStr = EmptyStr then
      RunStatusStrGrid.ColWidths[0] := RunStatusStrGrid.Canvas.TextWidth(newStr) + 10;
    AdjustColumnWidths;
  finally
    EndFormUpdate;
    RunStatusStrGrid.Invalidate;
  end;
end;

procedure TRuntimeProgressDlg.AjdustWindowSize;
var
  h: Integer = 0;
begin
  h := GaugePanel.Height + StatusBar1.Height;
  if PageControl1.Visible then
  begin
    h := h + 20;
    if PageControl1.ActivePage = StatusOptionsPage then
      h += (RunStatusStrGrid.RowCount + 1)*(RunStatusStrGrid.DefaultRowHeight + RunStatusStrGrid.GridLineWidth)
    else if PageControl1.ActivePage = AnalysisOptionsPage then
      h += (AnalysisOptionsDisplay.RowCount + 1)*(AnalysisOptionsDisplay.DefaultRowHeight * AnalysisOptionsDisplay.GridLineWidth)
    else if PageControl1.ActivePage = CommandLinePage then
      h += CmdLineOut.Height;
  end;
  ClientHeight := h + 2;
end;

procedure TRuntimeProgressDlg.RegisterCancellable(c: TCancellable);
begin
  try
    CheckAbortCriticalSection.Acquire;
    if FCancellables.IndexOf(c) < 0 then
      FCancellables.Add(c);
  finally
    CheckAbortCriticalSection.Release;
  end;
end;

procedure TRuntimeProgressDlg.UnregisterCancellable(c: TCancellable);
begin
  try
    CheckAbortCriticalSection.Acquire;
    if FCancellables.IndexOf(c) >= 0 then
      FCancellables.Remove(c);
  finally
    CheckAbortCriticalSection.Release;
  end;
end;

procedure TRuntimeProgressDlg.UpdateDataInfo(numTaxa, numSites, numGroups, numThreads: Integer);
begin
  if numThreads > 0 then
    if FOptionsDisplayStrings.IndexOfName('No. of Threads') >= 0 then
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Threads')] := Format('No. of Threads=%d', [numThreads])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Threads=%d', [numThreads]));

  if numGroups > 0 then
    if FOptionsDisplayStrings.IndexOfName('No. of Groups') >= 0 then
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Groups')] := Format('No. of Groups=%.0n', [numGroups*1.0])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Groups=%.0n', [numGroups*1.0]));

  if numSites > 0 then
    if FOptionsDisplayStrings.IndexOfName('No. of Sites') >= 0 then
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Sites')] := Format('No. of Sites=%.0n', [numSites*1.0])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Sites=%.0n', [numSites*1.0]));

  if numTaxa > 0 then
    if FOptionsDisplayStrings.IndexOfName('No. of Taxa') >= 0 then
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Taxa')] := Format('No. of Taxa=%.0n', [numTaxa*1.0])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Taxa=%.0n', [numTaxa*1.0]));
end;

procedure TRuntimeProgressDlg.SetFixedColWidthForStr(aString: String);
var
  aWidth: Integer;
begin
  try
    BeginFormUpdate;
    aWidth := RunStatusStrGrid.Canvas.TextWidth(aString);
    RunStatusStrGrid.ColWidths[0] := aWidth + 10;
  finally
    EndFormUpdate;
  end;
end;

procedure TRuntimeProgressDlg.setRunStatusVisibility(AVisible: boolean);
begin
  RunStatusPanel.Visible := AVisible;
  AjdustWindowSize;
end;

procedure TRuntimeProgressDlg.DisplayError(ErrorStr: AnsiString);
begin
  UpdateRunStatusInfo('!WARNING!', ErrorStr);
end;

procedure TRuntimeProgressDlg.StatusUpdateTimerTimer(Sender: TObject);
begin
  if PercentGauge.Position >= 96 then
    PercentGauge.Position := 0;
  PercentGauge.Position := PercentGauge.Position + 1;
end;

procedure TRuntimeProgressDlg.NotifyCalculationCancelled;
var
  i: Integer = -1;
  aCancellable: TCancellable = nil;
begin
  try
    CheckAbortCriticalSection.Acquire;
    if Assigned(FCancellables) and (FCancellables.Count > 0) then
      for i := FCancellables.Count - 1 downto 0 do
      begin
        aCancellable := TCancellable(FCancellables[i]);
        aCancellable.SetCancelled(True);
        UnregisterCancellable(aCancellable);
      end;
  finally
    CheckAbortCriticalSection.Release;
  end;
end;

procedure TRuntimeProgressDlg.AdjustColumnWidths;
begin
  RunStatusStrGrid.ColWidths[1] := RunStatusStrGrid.Width - RunStatusStrGrid.ColWidths[0];
  AnalysisOptionsDisplay.ColWidths[1] := AnalysisOptionsDisplay.Width - AnalysisOptionsDisplay.ColWidths[0];
end;

function TRuntimeProgressDlg.GetTextForSettingsDisplayGrid(const aRow: Integer; const aCol: Integer; var isSectionHeader: Boolean): String;
var
  temp: String;
begin
  Result := EmptyStr;
  if (not Assigned(FOptionsDisplayStrings)) or (aRow >= FOptionsDisplayStrings.Count) then Exit;
  isSectionHeader := False;
  temp := FOptionsDisplayStrings[aRow];
  if aCol = 0 then
  begin
    if Pos('=', temp) > 0 then
      Result := Trim(FOptionsDisplayStrings.Names[aRow])
    else
    begin
      Result := Trim(FOptionsDisplayStrings[aRow]);
      isSectionHeader := True;
    end;
  end
  else
  begin
    if Pos('=', temp) > 0 then
      Result := Trim(FOptionsDisplayStrings.ValueFromIndex[aRow])
    else
      Result := EmptyStr;
  end;
end;

function TRuntimeProgressDlg.RuntimeString: String;
begin
  Result := Format('%.2d:%.2d:%.2d', [HoursBetween(Now, FStartTime), MinutesBetween(Now, FStartTime) mod 60, SecondsBetween(Now, FStartTime) mod 60]);
end;

procedure TRuntimeProgressDlg.SetApplinkerProgressPlaceholder(AValue: String);
begin
  if FApplinkerProgressPlaceholder = AValue then Exit;
  FApplinkerProgressPlaceholder := AValue;
end;

procedure TRuntimeProgressDlg.UpdateBounds;
begin
  {$IFDEF DARWIN}
  ClientWidth := PercentGauge.Left + PercentGauge.Width + 20;
  {$ENDIF}
end;

{$ENDIF}

{ TRuntimeProgress }

constructor TRuntimeProgress.Create(Owner: TComponent = nil);
{$IFDEF VISUAL_BUILD}
var
  ANow: TDateTime;
  ADate: AnsiString;
  ATime: AnsiString;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD} { TTimer does not work in console applications because there is no message loop}
  FMarqueeTimer := TTimer.Create(nil);
  FMarqueeTimer.Enabled := False;
  FMarqueeTimer.OnTimer := MarqueeTimer;
  FMarqueeTimer.Interval := 200;
  {$ELSE}
  ProgressNameColumnWidth := 50;
  {$ENDIF}
  FNeedsTrayIcon := True;
  FAutoAdvanceProgress := True;
  DisableTimerOnFirstUpdate := False;
  InitMarqueeStrings;
  FMarqueeProgressChars := 'Computing';
  FMarqueeCharIndex := 0;
  FIsMarqueeMode := False;
  FAnalysisOptionStrLen := 0;
  FMAI := nil;
  FThread := nil;
  FRunStatusStrList := TStringList.Create;
  FAnalysisOptStrList := TStringList.Create;
  FAnalysisOptStrList.Add('Using the following analysis options:');
  FProgress := 0;
  FDataFileName := EmptyStr;
  FDataTitle := EmptyStr;
  FStatus := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg := TRuntimeProgressDlg.Create(Owner);
  FRuntimeProgressDlg.AnalysisOptStrList := FAnalysisOptStrList;
  FRuntimeProgressDlg.RunStatusStrList := FRunStatusStrList;
  FRuntimeProgressDlg.RuntimeProgress := Self;
  {$IFNDEF BUILD_WEBTOP_LIST}
  ANow := Now;
  ADate := DateToStr(ANow);
  ATime := TimeToStr(ANow);
  ADate := ADate + ' ' + ATime;
  AddRunStatusInfo('Start time', ADate);
  {$ENDIF}
  {$ENDIF}
end;

destructor TRuntimeProgress.Destroy;
begin

  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FreeAndNil(FRuntimeProgressDlg);
  {$ELSE}
  if not UseFormattedConsoleOutput then
    WriteLn(EmptyStr);
  {$ENDIF}
  if Assigned(FRunStatusStrList) then
    FreeAndNil(FRunStatusStrList);
  if Assigned(FAnalysisOptStrList) then
    FreeAndNil(FAnalysisOptStrList);
  FMAI := nil;
  if Assigned(FMarqueeStrings) then
    FreeAndNil(FMarqueeStrings);
  if Assigned(FMarqueeTimer) then
  begin
    FMarqueeTimer.Enabled := False;
    FreeAndNil(FMarqueeTimer);
  end;
  inherited;
end;

procedure TRuntimeProgress.RegisterCancellable(c: TCancellable);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.RegisterCancellable(c);
  {$ENDIF}
end;

procedure TRuntimeProgress.UnregisterCancellable(c: TCancellable);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.UnregisterCancellable(c);
  {$ENDIF}
end;

procedure TRuntimeProgress.AddRunStatusInfo(AType: String; AInfo: String);
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;
  {$ENDIF}
  if (AType = 'Status') and (Trim(AInfo) = EmptyStr) then
  begin
    Assert(False, 'empty status info');
  end;

  if FRunStatusStrList.IndexOfName(AType) < 0 then
  begin
    FRunStatusStrList.Values[AType] := AInfo;
    {$IFNDEF VISUAL_BUILD}
    if sameText(AType, 'Log Likelihood') then
    begin
      // do nothing
    end
    else if SameText(AType, 'status') then
    begin
      if not SameText(FStatus, AInfo) then
      begin
        if FAutoAdvanceProgress then
          FProgress := 1;
        FStatus := trim(AInfo);
        WriteProgressAndStatus(AType, AInfo);
      end;
    end
    else
      WriteProgressAndStatus(AType, AInfo);
    {$ELSE}
    try
      FRuntimeProgressDlg.UpdateRunStatusInfo(AType, AInfo);
      SetFixedColWidthForStr(AType);
    except
      {$IFDEF DEBUG}
      on E: Exception do
        ShowMessage('Developer warning; error at TRuntimeProgress.AddRunStatusInfo: ' + E.Message);
      {$ENDIF}
    end;
    {$ENDIF}
  end
  else
    UpdateRunStatusInfo(AType, AInfo);
  UpdateDataInfo;
end;

function TRuntimeProgress.AddSubTask(Status: AnsiString; Progress: Integer): TProgressPanel;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FRuntimeProgressDlg.AddSubTaskPanel(Status, Progress);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

function TRuntimeProgress.AddSubTaskNoLabel(Progress: Integer): TProgressPanel;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FRuntimeProgressDlg.AddSubTaskPanelNoLabel(Progress);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetProgress(progress: integer);
{$IFNDEF VISUAL_BUILD}
var
  progString: String;
  index: Integer;
{$ENDIF}
begin
  if (FProgress = Progress) or (FProgress < 0) then
    Exit;
  {$IFDEF VISUAL_BUILD}
  if (Progress <> 0) and (Progress <> 100) and (MillisecondsBetween(Now, FLastRefreshTime) < PROG_UPDATE_INTERVAL) then
    Exit;
  if (FProgress <> 100) and (Progress < FProgress) and (MillisecondsBetween(Now, FLastRefreshTime) < 1000) then
    Exit; { to help with flickering problems}
  {$ENDIF}
  FProgress := Min(100, Progress);
  FLastRefreshTime := Now;

  {$IFDEF VISUAL_BUILD}
  try
    if Assigned(FRuntimeProgressDlg) and Assigned(FRuntimeProgressDlg.PercentGauge) then
    begin
      FRuntimeProgressDlg.PercentGauge.Position := FProgress;
      FRuntimeProgressDlg.PercentGauge.Invalidate;
      Refresh;
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do
      ShowMessage('Developer warning; an error occurred at TRuntimeProgress.SetProgress: ' + E.Message);
    {$ENDIF}
  end;
  {$ELSE}
  if D_MegaMain.RunSilent then
    exit;
  progString := ProgressString;
  FRunStatusStrList.Values[CURRENT_PROGRESS] := progString;
  if Assigned(MegaStdOut) then
  begin
    MegaStdOut.Values[CURRENT_PROGRESS] := progString;
    index := MegaStdOut.IndexOfName(CURRENT_PROGRESS);
    try
      if UseFormattedConsoleOutput then
        LockScreenUpdate;
      DoTextOut(STATUS_MARGIN, index + 1, CURRENT_PROGRESS, progString);
    finally
      if UseFormattedConsoleOutput then
      begin
        UnlockScreenUpdate;
        UpdateScreen(False);
      end;
    end;
  end;
  {$ENDIF}
end;

function TRuntimeProgress.ProgressCheckCancel(Progress: Integer): Boolean;
begin
  SetProgress(Progress);
  Result := UserCancelled;
end;

function TRuntimeProgress.ProgressAndStatusCheckCancel(Progress: Integer; AType: String; AInfo: String): Boolean;
begin
  if (AType = 'Status') and (Trim(AInfo) = EmptyStr) then
  begin
    Assert(False, 'empty status info');
  end;
  Result := UserCancelled;
  if (FProgress = Progress) or (FProgress < 0) then
    Exit;
  {$IFDEF VISUAL_BUILD}
  if not AType.Contains('Thread') then
  begin
    if (Progress <> 0) and (Progress <> 100) and (MillisecondsBetween(Now, FLastRefreshTime) < PROG_UPDATE_INTERVAL) then
      Exit;
    if (FProgress <> 100) and (Progress < FProgress) and (MillisecondsBetween(Now, FLastRefreshTime) < 1000) then
      Exit; { to help with flickering problems}
  end;
  {$ENDIF}
  FProgress := Min(100, Progress);

  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) and Assigned(FRuntimeProgressDlg.PercentGauge) then
  begin
    FRuntimeProgressDlg.PercentGauge.Position := FProgress;
    FRuntimeProgressDlg.PercentGauge.Invalidate;
  end;
 {$ELSE}
  if D_MegaMain.RunSilent then
    exit;
  SetProgress(Progress);
 {$ENDIF}


  if FRunStatusStrList.IndexOf(AType) < 0 then
  begin
    FRunStatusStrList.Values[AType] := AInfo;
    {$IFDEF VISUAL_BUILD}
    FRuntimeProgressDlg.UpdateRunStatusInfo(AType, AInfo);
    SetFixedColWidthForStr(AType);
    Refresh;
    {$ELSE}
    if SameText(AType, 'status') then
    begin
      if not SameText(FStatus, AInfo) then
      begin
        if FAutoAdvanceProgress then
          FProgress := 1;
        FStatus := trim(AInfo);
        WriteProgressAndStatus(AType, AInfo);
      end;
    end
    else
      WriteProgressAndStatus(AType, AInfo);
    {$ENDIF}
  end;
end;

function TRuntimeProgress.ProgressAndStatusInfoCheckCancel(Progress: Integer; AInfo: String): Boolean;
begin
   if Trim(AInfo) = EmptyStr then
  begin
    Assert(False, 'empty status info');
  end;
  Result := ProgressAndStatusCheckCancel(Progress, 'Status', AInfo);
end;

{$IFDEF VISUAL_BUILD}
procedure TRuntimeProgress.SetStopButtonDown(Value: Boolean);
begin
  FRuntimeProgressDlg.StopBtn.Down := Value;
end;
{$ENDIF}

procedure TRuntimeProgress.SetTimerStatus(TimerOn: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.UseTimer := TimerOn;
  {$ENDIF}
end;

function TRuntimeProgress.HasStatusInfo(AType: String): Boolean;
begin
  Result := (FRunStatusStrList.IndexOf(AType) >= 0);
end;

procedure TRuntimeProgress.Show;
begin
  FLastRefreshTime := (Now - 1000);
  UpdateDataInfo;
end;

procedure TRuntimeProgress.SetKeepOnTop(aValue: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
   if aValue then
     FRuntimeProgressDlg.KeepOnTopChBox.Checked := True
   else
     FRuntimeProgressDlg.KeepOnTopChBox.Checked := False;
  {$ENDIF}
end;

procedure TRuntimeProgress.ThreadDone(Sender: TObject);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.ThreadDone(Sender);
  {$ENDIF}
end;

procedure TRuntimeProgress.UpdateRunStatusInfo(AType, AInfo: String);
begin
   if (AType = 'Status') and (Trim(AInfo) = EmptyStr) then
   begin
     Assert(False, 'empty status info');
   end;
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;
  {$ENDIF}
  if SameText(LowerCase(AType), 'no. of seqs') or SameText(LowerCase(AType), 'no. of sites') or SameText(LowerCase(AType), 'no. of taxa') or (AInfo = SKIP_STATUS_UPDATE) then
    Exit;
  if DisableTimerOnFirstUpdate then
    UseTimer := False;
  if FRunStatusStrList.Values[AType] = AInfo then // then status has not changed so don't waste resources
    exit;

  if FRunStatusStrList.IndexOfName(AType) = -1 then  // then it doesn't exist so add it
  begin
    AddRunStatusInfo(AType, AInfo);
    Exit;
  end;

  FRunStatusStrList.Values[Atype] := AInfo;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.UpdateRunStatusInfo(AType, AInfo);
  if (MillisecondsBetween(Now, FLastRefreshTime) > PROG_UPDATE_INTERVAL) then
  begin
    FLastRefreshTime := Now;
  end;
  {$ELSE}
  if SameText(AType, 'status') then
  begin
    if not SameText(FStatus, AInfo) then
    begin
      if FAutoAdvanceProgress then
        FProgress := 0;
      FStatus := trim(AInfo);
      WriteProgressAndStatus(AType, AInfo);
    end;
  end
  else
    WriteProgressAndStatus(AType, AInfo);
  {$ENDIF}
end;

procedure TRuntimeProgress.HideAnalysisOptions;
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.HideAnalysisOptions;
  {$ENDIF}
end;

procedure TRuntimeProgress.ShowAnalysisOptions;
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.ShowAnalysisOptions;
  {$ENDIF}
end;

procedure TRuntimeProgress.AddAnalysisOptions(AType, AInfo: String; showOptions: Boolean = True);
begin
  {$IFDEF VISUAL_BUILD}
  UpdateRunStatusInfo(AType, AInfo);
  {$ELSE}
  if FAnalysisOptStrList.Values[AType] <> AInfo then
  begin
    FAnalysisOptStrList.Values[AType] := AInfo;
    if showOptions then
    begin
      if UseFormattedConsoleOutput then
        UpdateStdOut
      else
        DoTextOut(0, 0, AType, AInfo);
    end;
  end;
  {$ENDIF}
end;

procedure TRuntimeProgress.AddAnalysisOptions(aOptions: TStringList; showOptions: Boolean = True);
var
  i: Integer;
  key, value: String;
begin
  if aOptions.Count > 0 then
  begin
    for i := 0 to aOptions.Count - 1 do
    begin
      key := aOptions.Names[i];
      value := aOptions.ValueFromIndex[i];
      if Trim(Value) <> EmptyStr then
        FAnalysisOptStrList.Values[key] := value
      else
        FAnalysisOptStrList.Add(aOptions[i]);
      {$IFNDEF VISUAL_BUILD}
      if not UseFormattedConsoleOutput then
        if Trim(Value) <> EmptyStr then
          DoTextOut(0, 0, key, value)
        else
          DoTextOut(0, 0, aOptions[i]);
      {$ENDIF}
    end;
    if showOptions then
      ShowAnalysisOptions
    else
    begin
      {$IFDEF VISUAL_BUILD}
      FRuntimeProgressDlg.UpdateAnalysisOptsRichEdit;
      {$ENDIF}
    end;
  end;
  {$IFNDEF VISUAL_BUILD}
  if UseFormattedConsoleOutput then
    UpdateStdOut;
  {$ENDIF}
end;

procedure TRuntimeProgress.WriteAnalysisOptionsToStdOut();
{$IFNDEF VISUAL_BUILD}
var
  i: Integer;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;
  if AnalysisOptStrList.Count > 0 then
    for i := AnalysisOptStrList.Count - 1 downto 0 do
      if ansipos('====', AnalysisOptStrList[i]) > 0 then
        AnalysisOptStrList.Delete(i);
  if AnalysisOptStrList.Count = 0 then
    Exit;
  MegaStdOut.Values['Start time'] := Format('%s %s', [DateToStr(Now), TimeToStr(Now)]);
  if not UseFormattedConsoleOutput then
    UpdateStdOut;
  {$ENDIF}
end;

procedure TRuntimeProgress.WriteProgressAndStatus(aType: String; aInfo: String);
{$IFNDEF VISUAL_BUILD}
var
  temp: String;
  index: Integer;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  try
    if UseFormattedConsoleOutput then
      LockScreenUpdate;
    if MegaStdOut.Values[aType] <> aInfo then
    begin
      MegaStdOut.Values[aType] := aInfo;
      index := MegaStdOut.IndexOfName(aType);
      DoTextOut(STATUS_MARGIN, index + 1, aType, aInfo);
    end;

    if FProgress > 0 then
    begin
      temp := ProgressString;
      if MegaStdOut.Values[CURRENT_PROGRESS] <> temp then
      begin
        MegaStdOut.Values[CURRENT_PROGRESS] := temp;
        FRunStatusStrList.Values[CURRENT_PROGRESS] := temp;
        index := MegaStdOut.IndexOfName(CURRENT_PROGRESS);
        DoTextOut(STATUS_MARGIN, index + 1, CURRENT_PROGRESS, temp);
      end;
    end;
  finally
    if UseFormattedConsoleOutput then
    begin
      UnlockScreenUpdate;
      UpdateScreen(False);
    end;
  end;
  {$ENDIF}
end;

procedure TRuntimeProgress.AddCommandLine(NewLine: String);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.AddCommandLine(NewLine);
  {$ENDIF}
end;

function TRuntimeProgress.AddCommandLineCheckCancel(NewLine: String): Boolean;
begin
  AddCommandLine(NewLine);
  Result := UserCancelled;
end;

procedure TRuntimeProgress.DisplayOutputInEditor;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.DisplayOutputInEditor;
  {$ENDIF}
end;

procedure TRuntimeProgress.DisplayCommandLineTab;
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.PageControl1.ActivePage := FRuntimeProgressDlg.CommandLinePage;
  FRuntimeProgressDlg.Invalidate;
  {$ENDIF}
end;

procedure TRuntimeProgress.Close;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Close;
  {$ENDIF}
end;

procedure TRuntimeProgress.Hide;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Hide;
  {$ENDIF}
end;

function TRuntimeProgress.DoesContainCmdStatus(Status: String): Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    Result := FRuntimeProgressDlg.DoesContainCmdStatus(Status);
  {$ENDIF}
end;

function TRuntimeProgress.GetThread: TThread;
begin
  Result := FThread;
end;

function TRuntimeProgress.GetUserCancelled: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if FRunTimeProgressDlg.StopBtn.Down then
    Result := True;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetTimerInterval(AValue: Integer);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.TimerInterval := AValue;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetWidth(AValue: Integer);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Width := AValue;
  {$ENDIF}
end;

function TRuntimeProgress.LongestRunStatusString: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if FRunStatusStrList.Count > 0 then
    for i := 0 to FRunStatusStrList.Count - 1 do
      if Length(FRunStatusStrList.Names[i]) > Length(Result) then
        Result := FRunStatusStrList.Names[i];
end;

function TRuntimeProgress.GetTimerInterval: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FRuntimeProgressDlg.TimerInterval;
  {$ELSE}
  Result := -1;
  {$ENDIF}
end;

function TRuntimeProgress.GetWidth: Integer;
begin
  Result := 0;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    Result := FRuntimeProgressDlg.Width;
  {$ENDIF}
end;

function TRuntimeProgress.GetHeight: Integer;
begin
  Result := 0;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    Result := FRuntimeProgressDlg.Height;
  {$ENDIF}
end;

function TRuntimeProgress.GetStdOutLabelCaption: String;
begin
{$IFDEF VISUAL_BUILD}
  if Assigned(RuntimeProgressDlg) then
    Result := RuntimeProgressDlg.StdOutLabel.Caption;
{$ENDIF}
end;

function TRuntimeProgress.GetApplinkerProgressPlaceholder: String;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(RuntimeProgressDlg) then
    Result := RuntimeProgressDlg.ApplinkerProgressPlaceholder
  else
    Result := EmptyStr;
  {$ENDIF}
end;

function TRuntimeProgress.GetRepsCompletedForBootstrapThread(aThread: TThread): Integer;
begin
  if aThread.ClassNameIs('TBootstrapMPTreeSearchThread') then
    Result := (aThread as TBootstrapMPTreeSearchThread).RepsCompleted
  else
    raise Exception.Create('Invalid call to GetRepsCompletedForBootstrapThread. Only supported for parsimony. This is a bug');
end;

procedure TRuntimeProgress.SetApplinkerProgressPlaceholder(AValue: String);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.ApplinkerProgressPlaceholder := AValue;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetAutoAdvanceProgress(AValue: Boolean);
begin
  if FAutoAdvanceProgress = AValue then Exit;
  FAutoAdvanceProgress := AValue;
end;

procedure TRuntimeProgress.SetHeight(AValue: Integer);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Height := AValue;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetNeedsTrayIcon(AValue: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
  if FNeedsTrayIcon = AValue then Exit;
  FNeedsTrayIcon := AValue;
  if Assigned(FRuntimeProgressDlg) then
    FRunTimeProgressDlg.NeedsTrayIcon := FNeedsTrayIcon;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetStdOutLabelCaption(AValue: String);
begin
  {$IFDEF VISUAL_BUILD}
    if Assigned(FRuntimeProgressDlg) then
      FRuntimeProgressDlg.StdOutLabel.Caption := AValue;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetTimer(Value: TNotifyEvent);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.StatusUpdateTimer.OnTimer := Value;
  {$ENDIF}
end;

procedure TRuntimeProgress.ClearCommandLine;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.CmdLineOut.Lines.Clear;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetCaption(Caption: String);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Caption := Caption;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetDataFileName(const Value: String);
begin
  FDataFileName := Value;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.DataFileName := Value;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetDataTitle(const Value: String);
begin
  FDataTitle := Value;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.DataTitle := Value;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetThread(const Value: TThread);
begin
  FThread := Value;
end;

{$IFDEF VISUAL_BUILD}
function TRuntimeProgress.VisualPercentGauge: TProgressBar;
begin
  Result := FRuntimeProgressDlg.PercentGauge;
end;

function TRuntimeProgress.VisualStopBtn: TSpeedButton;
begin
  Result := FRuntimeProgressDlg.StopBtn;
end;
{$ENDIF}

procedure TRuntimeProgress.InitMarqueeStrings;
begin
  FMarqueeStrings := TStringList.Create;
  FMarqueeStrings.Add('-');
  FMarqueeStrings.Add('--');
  FMarqueeStrings.Add('---');
  FMarqueeStrings.Add('----');
  FMarqueeStrings.Add('-----');
  FMarqueeStrings.Add('------');
  FMarqueeStrings.Add('-------');
  FMarqueeStrings.Add('--------');
  FMarqueeStrings.Add('---------');
  FMarqueeStrings.Add('----------');
  FMarqueeStrings.Add('-----------');
  FMarqueeStrings.Add('------------');
  FMarqueeStrings.Add('-------------');
  FMarqueeStrings.Add('--------------');
  FMarqueeStrings.Add('---------------');
end;

procedure TRuntimeProgress.UpdateDataInfo;
{$IFDEF VISUAL_BUILD}
var
  aInfo: TAnalysisInfo = nil;
  numTaxa, numSites, numGroups, numThreads: Integer;
{$ENDIF}
begin
 {$IFDEF VISUAL_BUILD}
  if Assigned(FMAI) then
  begin
    aInfo := TAnalysisInfo(FMAI);
    if Assigned(aInfo.MyUsedOtuInfos) then
      numTaxa := aInfo.MyUsedOtuInfos.Count
    else
      numTaxa := -1;
    if aInfo.NoOfSites > 0 then
      numSites := aInfo.NoOfSites
    else
      numSites := -1;
    if aInfo.NoOfGps > 0 then
      numGroups := aInfo.NoOfGps
    else
      numGroups := -1;
    if aInfo.MyNumThreadsToUse > 0 then
      numThreads := aInfo.MyNumThreadsToUse
    else
      numThreads := -1;
    FRuntimeProgressDlg.UpdateDataInfo(numTaxa, numSites, numGroups, numThreads);
  end;
{$ENDIF}
end;

{$IFNDEF VISUAL_BUILD}

procedure TRuntimeProgress.UpdateStdOut;
var
  i: Integer;
  temp: String;
  aName: String;
  aVal: String;
  numOptionStrings: Integer = 0;
begin
  ClearMegaStdOut;
  if Assigned(FAnalysisOptStrList) and (FAnalysisOptStrList.Count > 0) then
  begin
    for i := 0 to FAnalysisOptStrList.Count - 1 do
    begin
      temp := FAnalysisOptStrList[i];
      if (not Contains(temp, '====')) and (not Contains(temp, NotApplicableStr)) and (MegaStdOut.IndexOf(temp) < 0) then
          MegaStdOut.Add(temp)
    end;
    numOptionStrings := MegaStdOut.Count;
  end;

  if Assigned(FRunStatusStrList) and (FRunStatusStrList.Count > 0) then
  begin
    for i := 0 to FRunStatusStrList.Count - 1 do
    begin
      temp := FRunStatusStrList[i];
      aName := FRunStatusStrList.Names[i];
      aVal := FRunStatusStrList.ValueFromIndex[i];
      if MegaStdOut.IndexOfName(aName) >= 0 then
        MegaStdOut.Values[aName] := aVal
      else if temp = aVal then
      begin
        if MegaStdOut.IndexOf(temp) < 0 then
          MegaStdOut.Add(temp);
      end
      else if aName <> EmptyStr then
      begin
        if MegaStdOut.Values[aName] <> aVal then
          MegaStdOut.Values[aName] := aVal;
      end;
    end;
  end;
  ProgressNameColumnWidth := LengthOfLongestName(MegaStdOut) + 2;
  try
    if UseFormattedConsoleOutput then
      ClearScreen
    else
      WriteLn(EmptyStr);
    if UseFormattedConsoleOutput then
      LockScreenUpdate;
    if MegaStdOut.Count > 0 then
      for i := 0 to MegaStdOut.Count - 1 do
      begin
        if i < 2 then
          DoTextOut(1, i + 1, MegaStdOut[i], LightCyan)
        else if (i = 2) or (i = numOptionStrings) then
          DoTextOut(1, i + 1, MegaStdOut[i], Yellow)
        else
          DoTextOut(STATUS_MARGIN, i + 1, MegaStdOut.Names[i], MegaStdOut.ValueFromIndex[i]);
      end;
  finally
    if UseFormattedConsoleOutput then
    begin
      UnlockScreenUpdate;
      UpdateScreen(False);
    end;
  end;
end;

function TRuntimeProgress.ProgressString: String;
var
  numChars: Integer;
  index: Integer = 0;
begin
  Result := Format('%d%% ', [FProgress]);
  if not UseFormattedConsoleOutput then
    Exit;
  numChars := Round(FProgress/100*25);
  index := Length(Result) + 1;
  SetLength(Result, Length(Result) + 25);
  if numChars > 0 then
  begin
    while index <= numChars do
    begin
      Result[index] := '-';
      inc(index);
    end;
  end;

  while index <= Length(Result) do
  begin
    Result[index] := ' ';
    inc(index);
  end;
end;

{$ENDIF}

function TRuntimeProgress.GetIsVisible: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  Result := FRuntimeProgressDlg.Visible;
  {$ENDIF}
end;

procedure TRuntimeProgress.MarqueeTimer(Sender: TObject);
var
  ProgString: String;
begin
  if FMarqueeCharIndex = (FMarqueeStrings.Count - 1) then
    FMarqueeCharIndex := 0
  else
    inc(FMarqueeCharIndex);
  ProgString := FMarqueeStrings[FMarqueeCharIndex];
  UpdateRunStatusInfo('Please wait', ProgString);
end;

procedure TRuntimeProgress.Refresh();
begin
  if (MillisecondsBetween(Now, FLastRefreshTime) < PROG_UPDATE_INTERVAL) then
    Exit;
  FLastRefreshTime := Now;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Refresh;
  {$ENDIF}
end;

procedure TRuntimeProgress.RemoveRunStatusInfo(AType: String);
{$IFDEF VISUAL_BUILD}
var
  i: Integer;
{$ENDIF}
begin
  if FRunStatusStrList.IndexOfName(AType) < 0 then
    Exit;
  FRunStatusStrList.Delete(FRunStatusStrList.IndexOfName(AType));
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.ClearContents;
  if FRunStatusStrList.Count > 0 then
    for i := 0 to FRunStatusStrList.Count - 1 do
      FRuntimeProgressDlg.UpdateRunStatusInfo(FRunStatusStrList.Names[i], FRunStatusStrList.ValueFromIndex[i]);
  {$ENDIF}
end;

procedure TRuntimeProgress.RemoveRunStatusInfo(aNames: TStringList);
var
  i: Integer = -1;
  index: Integer = -1;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.RemoveRunStatusInfo(aNames)
  {$ELSE}
  if FRunStatusStrList.Count > 0 then
  begin
    for i := 0 to aNames.Count - 1 do
    begin
      index := FRunStatusStrList.IndexOfName(aNames[i]);
      if index >= 0 then
        FRunStatusStrList.Delete(index);
    end;
  end;
  {$ENDIF}
end;

procedure TRuntimeProgress.RemoveAllRunStatusInfo;
begin
  FRunStatusStrList.Clear;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.ClearContents;
  {$ENDIF}
end;

procedure TRuntimeProgress.ProcessMessages;
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.ProcessMessages;
  {$ENDIF}
end;

procedure TRuntimeProgress.RePaint;
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.Repaint;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetHasCmdLineOutput(Cmd: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.HasCmdLineOutput := Cmd;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetMarqueeMode(const Value: Boolean);
begin
  FIsMarqueeMode := Value;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.IsMarqueeMode := FIsMarqueeMode;
  {$ENDIF}
end;

procedure TRuntimeProgress.SendToPipe(msg: String);
begin
  //SendToWebtop(Msg);
end;

procedure TRuntimeProgress.UpdatePercentProgress(Progressin: Integer);
begin
  FProgress := ProgressIn;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.UpdatePercentProgress(ProgressIn);
  {$ELSE}
  SetProgress(Progressin);
  {$ENDIF}
end;

procedure TRuntimeProgress.setVisible(FVisible: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.Visible := True;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetFixedColWidthForStr(AString: String);
{$IFDEF VISUAL_BUILD}
var
  longestStr: String;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  try
    longestStr := LongestRunStatusString;
    if Length(aString) > Length(longestStr) then
      FRuntimeProgressDlg.SetFixedColWidthForStr(AString);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when setting a fixed column width: ' + E.Message);
  end;
  {$ENDIF}
end;

procedure ShowRunStatusInfoStatic(AType, AInfo: String);
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;

  if SameText(AType, 'status') then
    DoTextOut(1, NextProgressLine, AInfo, LightGreen)
  else
    DoTextOut(1, NextProgressLine, MakePrettyString('     ' + AType, 25) + ' : ' + AInfo, LightGreen);
  {$ENDIF}
end;

procedure ShowProgressStatic(ProgressIn: Integer);
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    exit;

  DoTextOut(1, NextProgressLine, '     ' + IntToStr(ProgressIn) + '%', LightGreen);
  {$ENDIF}
end;

procedure ShowProgressIncrementStatic;
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;
  if StaticProgressPosition < ScreenWidth then
    inc(StaticProgressPosition)
  else
    StaticProgressPosition := 1;
  DoTextOut(StaticProgressPosition, NextProgressLine, '.');
  {$ENDIF}
end;

end.

