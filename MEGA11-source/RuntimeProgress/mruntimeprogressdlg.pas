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

unit mruntimeprogressdlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  LCLIntF, LCLType, dateutils,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  MegaUtils, Buttons, ExtCtrls, StdCtrls, Menus, Grids,
  MProgressPanel, Types;

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
    AnalysisOptLabelPanel: TPanel;
    DetailsMenuBtn: TBitBtn;
    GaugeRzLabel: TLabel;
    ImageList1: TImageList;
    Copy2: TPopupMenu;
    Copy1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    AnalysisOptionsPanel: TPanel;
    RunStatusLabelPanel: TPanel;
    RunStatusPanel: TPanel;
    PauseBitBtn: TBitBtn;
    PercentGauge: TProgressBar;
    ProgressScrollBox: TScrollBox;
    RunStatusStrGrid: TStringGrid;
    ScrollWithStatusUpdates1: TMenuItem;
    ShowAnalysisOptsItem: TMenuItem;
    ShowRunStatusItem: TMenuItem;
    PanelRzSplit: TSplitter;
    StdOut: TListBox;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    GaugePanel: TPanel;
    DetailsPopupMenu: TPopupMenu;
    ProgressBar1: TProgressBar;
    StopBitBtn: TBitBtn;
    StopBtn: TSpeedButton;
    StatusOptionsPage: TTabSheet;
    CommandLinePage: TTabSheet;
    StatusUpdateTimer: TTimer;
    RuntimeTimer: TTimer;
    procedure AnalysisOptionsDisplayClick(Sender: TObject);
    procedure AnalysisOptionsDisplayDrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure AnalysisOptionsDisplaySelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RuntimeTimerTimer(Sender: TObject);
    procedure ShowRunStatusItemClick(Sender: TObject);
    procedure ShowAnalysisOptsItemClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure StdOutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormHide(Sender: TObject);
    procedure PauseBitBtnClick(Sender: TObject);
    procedure DetailsMenuBtnClick(Sender: TObject);
    procedure StatusUpdateTimerTimer(Sender: TObject);
  private
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
    procedure AdjustColumnWidths;
    function GetTextForSettingsDisplayGrid(const aRow: Integer; const aCol: Integer; var isSectionHeader: Boolean): String;
    function RuntimeString: String;
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
    LastUpdateTime: TDateTime;
    ThreadRunning      : Boolean;
    HasCmdLineOutput   : Boolean;
    FMAI : Pointer;// used by kumar to make TRuntimeDlg generic
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
    procedure BringRunStatusInfoInView(AType: AnsiString);
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
  end;
 {$ENDIF}

  TRuntimeProgress = class(TObject)
   private
    FMarqueeTimer: TTimer;
    FMarqueeStrings: TStringList;
    FMarqueeCharIndex: Integer;
    FMarqueeProgressChars: String;
    FIsMarqueeMode: Boolean;
    FLastLineLength     : Integer; // the length of the last line written to the console. So we can make sure to overwrite it completely.
    FDataFileName      : String;
    FDataTitle         : String;
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
    function GetHeight: Integer;
    function GetTimerInterval: Integer;
    function GetWidth: Integer;
    function LongestRunStatusString: String;
    function GetRepsCompletedForBootstrapThread(aThread: TThread): Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetTimer(Value: TNotifyEvent);
    procedure SetThread(const Value: TThread);
    procedure SetDataFileName(const Value: String);
    procedure SetDataTitle(const Value: String);
    procedure SetTimerInterval(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure WriteProgressAndStatus(AInfo: String='');
    procedure SetMarqueeMode(const Value: Boolean);
    procedure MarqueeTimer(Sender: TObject);
    procedure InitMarqueeStrings;
    procedure UpdateDataInfo;
  public
    FMAI: Pointer;
    DisableTimerOnFirstUpdate: Boolean;
    constructor Create(Owner : TComponent=nil);
    destructor Destroy(); override;
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
    procedure RemoveRunStatusInfo(AType: String);
    procedure Show;
    procedure SetKeepOnTop(aValue: Boolean);
    procedure AddAnalysisOptions(AType, AInfo: String); overload;
    procedure AddAnalysisOptions(aOptions: TStringList; showOptions: Boolean = True); overload;

    procedure WriteAnalysisOptionsToStdOut();
    procedure AddCommandLine(NewLine: String);
    function AddCommandLineCheckCancel(NewLine: String): Boolean;
    procedure BringRunStatusInfoInView(AType: String);
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
    {$ENDIF}
    function GetThread: TThread;
    procedure Hide;
    procedure SetCaption(Caption : String);
    procedure HideAnalysisOptions;
    procedure ShowAnalysisOptions;
    procedure ThreadDone(Sender: TObject);
    procedure UpdateRunStatusInfo(AType, AInfo: String);

    procedure UpdatePercentProgress(Progressin: Integer);
    function AddSubTask(Status: AnsiString; Progress: Integer): TProgressPanel;
    function AddSubTaskNoLabel(Progress: Integer): TProgressPanel;
    property DataFileName: String read FDataFileName write SetDataFileName;
    property DataTitle:    String read FDataTitle    write SetDataTitle;
    property AnalysisOptStrList: TStringList read FAnalysisOptStrList;
    property RunStatusStrList: TStringList read FRunStatusStrList;
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
    property UserCancelled: Boolean read GetUserCancelled;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
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
  {$IFDEF VISUAL_BUILD}
  Mega_Main,  MEditorForm, mimageform,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  Math, MTreeSearchThread, MegaVerConsts, MPTree,
  MegaConsts, MAnalysisInfo, MLSearchThread, mreltimethreads, MegaAnalysisPrefStrings;

{$IFDEF VISUAL_BUILD}

procedure TRuntimeProgressDlg.FormCreate(Sender: TObject);
begin
  FProgressPanels := TList.Create;
  FOptionsDisplayStrings := TStringList.Create;
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    AnalysisOptionsDisplay.Font.Name := 'Courier';
    {$ELSE}
    AnalysisOptionsDisplay.Font.Name := 'Monospace';
    {$ENDIF}
  {$ENDIF}
  FRunStatusStrList := nil;
  FAnalysisOptStrList:= nil;
  PercentGauge.Position := 0;

  StopBtn.AllowAllUp := True;
  StopBtn.GroupIndex := 1;
  FDataFileName := EmptyStr;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Progress';
  FNextGaugeTopCoord := Panel2.Height;
  FOptionsPanelHeight := AnalysisOptionsPanel.Height;
  ImageForm.UpdateImgList(Self);
  FStartTime := MinDateTime;
  ShowAnalysisOptsItemClick(Sender);
end;

procedure TRuntimeProgressDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  RuntimeTimer.Enabled := False;
  StatusUpdateTimer.Enabled := False;
  CloseAction := caHide;
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
      Font.Color := clBlack;
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
end;

procedure TRuntimeProgressDlg.FormShow(Sender: TObject);
begin
  if CompareValue(FStartTime, MinDateTime, FP_CUTOFF) = 0 then
    FStartTime := Now;
  StatusUpdateTimer.Enabled := UseTimer;
  PageControl1.Pages[1].TabVisible := HasCmdLineOutput;
  PageControl1.ActivePageIndex := 0;
  UpdateAnalysisOptsRichEdit;

  if PanelRzSplit.Visible and (not ShowAnalysisOptsItem.Checked) then
    ShowAnalysisOptsItem.Checked := True;

  if FAnalysisOptStrList.Count < 1 then
    if ShowAnalysisOptsItem.Checked then
      ShowAnalysisOptsItemClick(Sender);
  if RunStatusStrGrid.RowCount < 2 then
    if ShowRunStatusItem.Checked then
      setRunStatusVisibility(true);
  UpdateBounds;
  Invalidate;
end;

procedure TRuntimeProgressDlg.RuntimeTimerTimer(Sender: TObject);
begin
  if not Assigned(FRunStatusStrList) then
    Exit;
  if not Assigned(RunStatusStrGrid) then
    Exit;
  if FRunStatusStrList.IndexOfName('Operation Run Time') < 0 then
  begin
    RunStatusStrGrid.RowCount := RunStatusStrGrid.RowCount + 1;
    FRunStatusStrList.Add('Operation Run Time=' + RuntimeString);
    AjdustWindowSize;
  end
  else
    UpdateRunStatusInfo('Operation Run Time', RuntimeString);
  RunStatusStrGrid.Invalidate;
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
  aThread: TThread;
begin

  if FRuntimeProgress.GetThread = nil then
  begin
    StopBtn.Down := True;
    Hide;
    ProcessMessages;
    Exit;
  end;

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
  FRuntimeProgress.GetThread.Terminate;
  if FProgressPanels.Count > 0 then
    for i := 0 to FProgressPanels.Count - 1 do
      TProgressPanel(FProgressPanels[i]).Cancelled := True;

  aThread := FRuntimeProgress.GetThread;
  if (aThread is TBootstrapMLThread) or
     (aThread is TMLTreeAnalyzeThread) or
     (aThread is TMLClockTestThread) or
     (aThread is TModelTestThread) or
     (aThread is TRelTimeMLThread) or
     (aThread is TMLTreeSearchThread) then
  begin
    (aThread as TMLTreeThread).Canceled := True;
  end;

  Close;
end;

procedure TRuntimeProgressDlg.UpdateRunStatusInfo(AType: AnsiString; AInfo: AnsiString);
var
  i: Integer;
begin
  i := FRunStatusStrList.IndexOfName(AType);
  if i < 0 then
    Exit;
  try
    if RunStatusStrGrid.RowCount < FRunStatusStrList.Count then
    begin
      RunStatusStrGrid.RowCount := FRunStatusStrList.Count;
      AjdustWindowSize;
    end;
    RunStatusStrGrid.Cells[0,i] := AType;
    RunStatusStrGrid.Cells[1,i] := AInfo;
    if MillisecondsBetween(Now, LastUpdateTime) > 1000 then
    begin
      UpdateRunStatusGridFixedColWidth(AType);
      LastUpdateTime := Now;
      RunStatusStrGrid.Invalidate;
    end;
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
const
  OFFSET = 30;
var
  aProgPanel: TProgressPanel;
  aGauge: TProgressBar;
  aLabel: TLabel;
begin
  aGauge := TProgressBar.Create(Self);
  aGauge.Position := Progress;
  aGauge.Height := 14;
  aLabel := TLabel.Create(Self);
  aLabel.Caption := Status;
  aProgPanel := TProgressPanel.Create(aGauge, aLabel);
  FProgressPanels.Add(aProgPanel);

  aGauge.Parent := ProgressScrollBox;
  aLabel.Parent := ProgressScrollBox;
  aLabel.Top := FNextGaugeTopCoord;
  aLabel.Left := PercentGauge.Left + OFFSET;
  aGauge.Top := aLabel.Top + 18;
  aGauge.Left := aLabel.Left;
  aGauge.Width := PercentGauge.Width - OFFSET;
  FNextGaugeTopCoord := FNextGaugeTopCoord + 32;

  Result := aProgPanel;
  GaugePanel.Height := Min(GaugePanel.Height + 36, 350);
  if FProgressPanels.Count = 1 then
    GaugePanel.Height := GaugePanel.Height + 4;
  Refresh;
  Application.ProcessMessages;
end;

function TRuntimeProgressDlg.AddSubTaskPanelNoLabel(Progress: Integer): TProgressPanel;
const
  OFFSET = 30;
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

  aGauge.Parent := ProgressScrollBox;
  if Assigned(aLabel) then
  begin
    aLabel.Parent := ProgressScrollBox;
    aLabel.Top := FNextGaugeTopCoord;
    aLabel.Left := PercentGauge.Left + OFFSET;
    aGauge.Top := aLabel.Top + 18;
    FNextGaugeTopCoord := FNextGaugeTopCoord + 32;
  end
  else
  begin
    aGauge.Top := FNextGaugeTopCoord + 4;
    FNextGaugeTopCoord := FNextGaugeTopCoord + 18;
  end;
  aGauge.Left := PercentGauge.Left + OFFSET;
  aGauge.Width := PercentGauge.Width - OFFSET;
  Result := aProgPanel;
  if Assigned(aLabel) then
    GaugePanel.Height := Min(GaugePanel.Height + 36, 350)
  else
    GaugePanel.Height := Min(GaugePanel.Height + 18, 350);
  if FProgressPanels.Count = 1 then
    GaugePanel.Height := GaugePanel.Height + 4;
  Refresh;
  Application.ProcessMessages;
end;

procedure TRuntimeProgressDlg.BringRunStatusInfoInView(AType: AnsiString);
var
  i: Integer;
begin
  i := FRunStatusStrList.IndexOf(AType);
  if i < 0 then
    Exit;

  if not ShowRunStatusItem.Checked then
    ShowRunStatusItemClick(ShowRunStatusItem);

  with RunStatusStrGrid do
    if (i < TopRow) or (i > (TopRow + VisibleRowCount)) then
       TopRow := i;
end;

procedure TRuntimeProgressDlg.HideAnalysisOptions;
begin
  ShowAnalysisOptsItem.Checked := True; // so it can disabled in the next fn
  ShowAnalysisOptsItemClick(ShowAnalysisOptsItem);
end;

procedure TRuntimeProgressDlg.ShowAnalysisOptions;
begin
  ShowAnalysisOptsItem.Checked := False; // so it can disabled in the next fn
  ShowAnalysisOptsItemClick(ShowAnalysisOptsItem);
  UpdateAnalysisOptsRichEdit;
end;


procedure TRuntimeProgressDlg.ShowRunStatusItemClick(Sender: TObject);
begin
  ShowRunStatusItem.OnClick := nil;
  setRunStatusVisibility(not ShowRunStatusItem.Checked);
  ShowRunStatusItem.OnClick := ShowRunStatusItemClick;
end;

procedure TRuntimeProgressDlg.ShowAnalysisOptsItemClick(Sender: TObject);
begin
  ShowAnalysisOptsItem.Checked := not ShowAnalysisOptsItem.Checked;

  if (not ShowAnalysisOptsItem.Checked) and (not ShowRunStatusItem.Checked) then
    PageControl1.Visible := False
  else
  begin
    PageControl1.Visible := True;
    if ShowRunStatusItem.Checked then
      RunStatusPanel.Visible := True
    else
      RunStatusPanel.Visible := False;
    if ShowAnalysisOptsItem.checked then
    begin
      PanelRzSplit.Visible := True;
      AnalysisOptionsPanel.Visible := True;
    end
    else
    begin
      AnalysisOptionsPanel.Visible := False;
      PanelRzSplit.Visible := False;
    end;
  end;
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
  if StdOut <> nil then
  begin
    StdOut.Items.Insert(0, NewLine);
    StdOut.TopIndex := 1;
  end;
  FLongestCmdLineEntry := 0;
  if (Stdout.Canvas.TextWidth(StdOut.Items[StdOut.Count-1]) > FLongestCmdLineEntry) then
  begin
    FLongestCmdLineEntry := Stdout.Canvas.TextWidth(StdOut.Items[StdOut.Count-1]);
    Stdout.ScrollWidth := FLongestCmdLineEntry; //Set the ListBox's scroll width to the largest item's text width
  end;
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
var
  cursorPos: TPoint;
  selectedint: Integer;
  SelectedRows: TMemo;
  i: Integer;
begin
  cursorPos := TPoint.Create(0, 0);
  SelectedRows := TMemo.Create(nil);
  SelectedRows.Visible := false;
  SelectedRows.Parent := Self;
  for i:=0 to StdOut.Items.Count-1 do
  begin
    if StdOut.Selected[i] then
      SelectedRows.Lines.Add(StdOut.Items.Strings[i]);
  end;

  if SelectedRows.Lines.Count = 0 then
  begin
    getCursorPos(cursorPos);
    cursorPos := StdOut.ScreenToClient(cursorPos);
    selectedint := StdOut.ItemAtPos(cursorPos, true);
    if selectedint >= 0 then
      SelectedRows.Lines.Add(StdOut.Items.Strings[i])
  end;
  SelectedRows.SelectAll;
  SelectedRows.CopyToClipboard;

  SelectedRows.Free;
end;

procedure TRuntimeProgressDlg.StdOutMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cursorPos: TPoint;
  selectedint: Integer;
  i: Integer;
begin
  cursorPos := TPoint.Create(0, 0);
  if Button = mbRight then
  begin
    getCursorPos(cursorPos);
    cursorPos := StdOut.ScreenToClient(cursorPos);
    selectedint := StdOut.ItemAtPos(cursorPos, true);
    if (selectedint >= 0) and (StdOut.Selected[selectedint] = false) then
    begin
      for i:=0 to StdOut.Items.Count-1 do
        StdOut.Selected[i] := false;
      StdOut.Selected[selectedint] := true;
    end;
  end;
end;

procedure TRuntimeProgressDlg.DisplayOutputInEditor;
begin
  OpenStringList(StdOut.Items, 'Command Line Output');
end;

procedure TRuntimeProgressDlg.FormHide(Sender: TObject);
begin
  if StatusUpdateTimer.Enabled then
    StatusUpdateTimer.Enabled := False;
  StdOut.Clear;
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
  Result := (StdOut.Items.IndexOf(Status) >= 0);
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
  if PageControl1.ActivePage = StatusOptionsPage then
  begin
    h := GaugePanel.Height + StatusBar1.Height;
    if PageControl1.Visible then
    begin
      h := h + PageControl1.TabHeight;
      if RunStatusPanel.Visible then
      begin
        RunStatusPanel.Height := RunStatusLabelPanel.Height + (RunStatusStrGrid.RowCount + 1)*(RunStatusStrGrid.DefaultRowHeight + RunStatusStrGrid.GridLineWidth);
        h := h + RunStatusPanel.Height + 2;
        PanelRzSplit.Top := RunStatusPanel.Height;
      end;
      if AnalysisOptionsPanel.Visible then
        h := h + FOptionsPanelHeight + PanelRzSplit.Height;
    end;
    ClientHeight := h + 2;
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
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Groups')] := Format('No. of Groups=%d', [numGroups])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Groups=%d', [numGroups]));

  if numSites > 0 then
    if FOptionsDisplayStrings.IndexOfName('No. of Sites') >= 0 then
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Sites')] := Format('No. of Sites=%d', [numSites])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Sites=%d', [numSites]));

  if numTaxa > 0 then
    if FOptionsDisplayStrings.IndexOfName('No. of Taxa') >= 0 then
      FOptionsDisplayStrings[FOptionsDisplayStrings.IndexOfName('No. of Taxa')] := Format('No. of Taxa=%d', [numTaxa])
    else
      FOptionsDisplayStrings.Insert(0, Format('No. of Taxa=%d', [numTaxa]));
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
  ShowRunStatusItem.Checked := AVisible;
  RunStatusPanel.Visible := AVisible;
  AjdustWindowSize;
end;

procedure TRuntimeProgressDlg.DisplayError(ErrorStr: AnsiString);
begin
  UpdateRunStatusInfo('!WARNING!', ErrorStr);
end;

procedure TRuntimeProgressDlg.DetailsMenuBtnClick(Sender: TObject);
var
  lowerLeft : TPoint;
begin
  lowerLeft := Point(DetailsMenuBtn.Left, DetailsMenuBtn.Top + DetailsMenuBtn.Height + Panel2.Height);
  lowerLeft := ClientToScreen(lowerLeft);
  DetailsPopupMenu.Popup(lowerLeft.X, lowerLeft.Y);
end;

procedure TRuntimeProgressDlg.StatusUpdateTimerTimer(Sender: TObject);
begin
  if PercentGauge.Position >= 96 then
    PercentGauge.Position := 0;
  PercentGauge.Position := PercentGauge.Position + 1;
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

procedure TRuntimeProgressDlg.UpdateBounds;
begin
  {$IFDEF DARWIN}
  ClientWidth := PercentGauge.Left + PercentGauge.Width + 20;
  {$ENDIF}
end;

{$ENDIF}

{ TRuntimeProgress }

constructor TRuntimeProgress.Create(Owner: TComponent = nil);
var
  ANow: TDateTime;
  ADate: AnsiString;
  ATime: AnsiString;
begin
  {$IFDEF VISUAL_BUILD} { TTimer does not work in console applications because there is no message loop}
  FMarqueeTimer := TTimer.Create(nil);
  FMarqueeTimer.Enabled := False;
  FMarqueeTimer.OnTimer := MarqueeTimer;
  FMarqueeTimer.Interval := 200;
  {$ENDIF}
  DisableTimerOnFirstUpdate := False;
  InitMarqueeStrings;
  FMarqueeProgressChars := 'Computing';
  FMarqueeCharIndex := 0;
  FIsMarqueeMode := False;
  FLastLineLength := 0;
  FAnalysisOptionStrLen := 0;
  FMAI := nil;
  FThread := nil;
  FRunStatusStrList := TStringList.Create;
  FAnalysisOptStrList := TStringList.Create;
  FProgress := 0;
  FDataFileName := EmptyStr;
  FDataTitle := EmptyStr;
  FStatus := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg := TRuntimeProgressDlg.Create(Owner);
  FRuntimeProgressDlg.AnalysisOptStrList := FAnalysisOptStrList;
  FRuntimeProgressDlg.RunStatusStrList := FRunStatusStrList;
  FRuntimeProgressDlg.RuntimeProgress := Self;
  {$ENDIF}
  {$IFNDEF BUILD_WEBTOP_LIST}
  ANow := Now;
  ADate := DateToStr(ANow);
  ATime := TimeToStr(ANow);
  ADate := ADate + ' ' + ATime;
  AddRunStatusInfo('Start time', ADate);
  {$ENDIF}
end;

destructor TRuntimeProgress.Destroy();
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FreeAndNil(FRuntimeProgressDlg);
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

procedure TRuntimeProgress.AddRunStatusInfo(AType: String; AInfo: String);
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;
  {$ENDIF}

  if RunStatusStrList.IndexOf(AType) < 0 then
  begin
    RunStatusStrList.Values[AType] := AInfo;
    {$IFNDEF VISUAL_BUILD}
    if sameText(AType, 'Log Likelihood') then
    begin
      // do nothing
    end
    else if SameText(AType, 'status') then
    begin
      if not SameText(FStatus, AInfo) then
      begin
        FProgress := 1;
        FStatus := trim(AInfo);
        WriteProgressAndStatus;
      end;
    end
    else
      WriteProgressAndStatus(AInfo);
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
  end;
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
begin
  if (FProgress = Progress) or (FProgress < 0) then
    Exit;
  if (Progress <> 0) and (Progress <> 100) and (MillisecondsBetween(Now, FLastRefreshTime) < PROG_UPDATE_INTERVAL) then
    Exit;
  if (FProgress <> 100) and (Progress < FProgress) and (MillisecondsBetween(Now, FLastRefreshTime) < 1000) then
    Exit; { to help with flickering problems}
  FProgress := Min(100, Progress);
  FLastRefreshTime := Now;

  {$IFDEF VISUAL_BUILD}
  try
    if Assigned(FRuntimeProgressDlg) and Assigned(FRuntimeProgressDlg.PercentGauge) then
    begin
      if (FProgress < FRuntimeProgressDlg.PercentGauge.Position) and (FProgress > 5) then
      begin
        FRuntimeProgressDlg.PercentGauge.Position := FProgress;
        FRuntimeProgressDlg.PercentGauge.Invalidate;
        Refresh;
      end
      else
      begin
        FRuntimeProgressDlg.PercentGauge.Position := FProgress;
        FRuntimeProgressDlg.PercentGauge.Invalidate;
        Refresh;
      end;
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
  WriteProgressAndStatus;
  {$ENDIF}
end;

function TRuntimeProgress.ProgressCheckCancel(Progress: Integer): Boolean;
begin
  SetProgress(Progress);
  Result := UserCancelled;
end;

function TRuntimeProgress.ProgressAndStatusCheckCancel(Progress: Integer; AType: String; AInfo: String): Boolean;
begin
  Result := UserCancelled;
  if (FProgress = Progress) or (FProgress < 0) then
    Exit;
  if (Progress <> 0) and (Progress <> 100) and (MillisecondsBetween(Now, FLastRefreshTime) < PROG_UPDATE_INTERVAL) then
    Exit;
  if (FProgress <> 100) and (Progress < FProgress) and (MillisecondsBetween(Now, FLastRefreshTime) < 1000) then
    Exit; { to help with flickering problems}
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
  WriteProgressAndStatus;
 {$ENDIF}


  if RunStatusStrList.IndexOf(AType) < 0 then
  begin
    RunStatusStrList.Values[AType] := AInfo;
    {$IFDEF VISUAL_BUILD}
    FRuntimeProgressDlg.UpdateRunStatusInfo(AType, AInfo);
    SetFixedColWidthForStr(AType);
    {$ELSE}
    if sameText(AType, 'Log Likelihood') then
    begin
      // do nothing
    end
    else if SameText(AType, 'status') then
    begin
      if not SameText(FStatus, AInfo) then
      begin
        FProgress := 1;
        FStatus := trim(AInfo);
        WriteProgressAndStatus;
      end;
    end
    else
      WriteProgressAndStatus(AInfo);
    {$ENDIF}
  end;
 Refresh;
end;

function TRuntimeProgress.ProgressAndStatusInfoCheckCancel(Progress: Integer; AInfo: String): Boolean;
begin
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
  Result := (RunStatusStrList.IndexOf(AType) >= 0);
end;

procedure TRuntimeProgress.Show;
begin
  FLastRefreshTime := (Now - 1000);
  UpdateDataInfo;
  {$IFDEF VISUAL_BUILD}
   FRuntimeProgressDlg.Show;
   FRuntimeProgressDlg.BringToFront;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetKeepOnTop(aValue: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
   if aValue then
     FRuntimeProgressDlg.FormStyle := fsStayOnTop
   else
     FRuntimeProgressDlg.FormStyle := fsNormal;
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
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;

  if (AType = 'Parsimony Trees') or (AType = 'Tree Length') then
    Exit;
  {$ENDIF}
  if SameText(LowerCase(AType), 'no. of seqs') or SameText(LowerCase(AType), 'no. of sites') or SameText(LowerCase(AType), 'no. of taxa') then
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
  if sameText(AType, 'Log Likelihood') then
  begin
    // do nothing
  end
  else if SameText(AType, 'status') then
  begin
    if not SameText(FStatus, AInfo) then
    begin
      FProgress := 0;
      FStatus := trim(AInfo);
      WriteProgressAndStatus;
    end;
  end
  else
    WriteProgressAndStatus(AInfo);
  {$ENDIF}
end;

procedure TRuntimeProgress.BringRunStatusInfoInView(AType: String);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.BringRunStatusInfoInView(AType);
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

procedure TRuntimeProgress.AddAnalysisOptions(AType, AInfo: String);
begin
  FAnalysisOptStrList.Add(AType + ' = ' + AInfo);
  UpdateRunStatusInfo(AType, AInfo);
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
        FAnalysisOptStrList.Add(key + '=' + value)
      else
        FAnalysisOptStrList.Add(aOptions[i]);
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
end;

procedure TRuntimeProgress.WriteAnalysisOptionsToStdOut();
var
  i: Integer;
  Pieces: TStringList = nil;
  FirstColumnWidth: Integer;
begin
  try
    try
      {$IFNDEF VISUAL_BUILD}
      if D_MegaMain.RunSilent then
        Exit;
      {$ENDIF}
      Pieces := TStringList.Create;
      FirstColumnWidth := 0;
      for i := 0 to FAnalysisOptStrList.Count - 1 do
      begin
        SplitStr(AnalysisOptStrList[i], ':', Pieces, true);
        if  (Pieces.Count = 2) and ( not (ansiPos('====', Pieces[1]) > 0)) then
          if Length(Pieces[0]) > FirstColumnWidth then
            FirstColumnWidth := Length(Pieces[0]);
      end;

      inc(FirstColumnWidth, 3);
      {$IFNDEF VISUAL_BUILD}
      WriteLn(#13 + 'Using the following analysis options:');
      for i := 0 to AnalysisOptStrList.Count - 1 do
      begin
        SplitStr(AnalysisOptStrList[i], ':', Pieces, true);
        if  (Pieces.Count = 2) and ( not (ansiPos('====', Pieces[1]) > 0)) then
          WriteLn(Format('  %-' + IntToStr(FirstColumnWidth) + 's %s', [Pieces[0], Pieces[1]]));
      end;
      {$IFNDEF BUILD_WEBTOP_LIST}
      WriteLn('Start time: ' + DateToStr(Now)+' '+TimeToStr(Now));
      {$ENDIF}
      WriteLn('Executing analysis:' + LineEnding);
      {$ENDIF}
    Except
      // don't sweat it!
    end;
  finally
    if Assigned(Pieces) then
      Pieces.Free;
  end;
end;

procedure TRuntimeProgress.WriteProgressAndStatus(AInfo: String = '');
{$IFNDEF VISUAL_BUILD}
var
  OutStr: String;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  OutStr := FStatus + ' ' + AInfo;
  if FLastLineLength >= Length(Trim(OutStr)) then
      Write(#13 + Format('  %3d%%  %-' + IntToStr(FLastLineLength + 1) + 's', [FProgress, FStatus + ' ' + AInfo]))
    else
      Write(#13 + Format('  %3d%%  %-s', [FProgress, FStatus + ' ' + AInfo]));
  if FLastLineLength < Length(FStatus + AInfo + #9) then
    FLastLineLength := Length(Trim(FStatus + ' ' + AInfo));
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
  if FRunStatusStrList.Count > 0 then;
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

function TRuntimeProgress.GetRepsCompletedForBootstrapThread(aThread: TThread): Integer;
begin
  if aThread.ClassNameIs('TBootstrapMPTreeSearchThread') then
    Result := (aThread as TBootstrapMPTreeSearchThread).RepsCompleted
  else
    raise Exception.Create('Invalid call to GetRepsCompletedForBootstrapThread. Only supported for parsimony. This is a bug');
end;

procedure TRuntimeProgress.SetHeight(AValue: Integer);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FRuntimeProgressDlg) then
    FRuntimeProgressDlg.Height := AValue;
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
    FRuntimeProgressDlg.StdOut.Clear;
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
var
  aInfo: TAnalysisInfo = nil;
  numTaxa, numSites, numGroups, numThreads: Integer;
begin
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
    {$IFDEF VISUAL_BUILD}
    FRuntimeProgressDlg.UpdateDataInfo(numTaxa, numSites, numGroups, numThreads);
    {$ENDIF}
  end;
end;

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
var
  i: Integer;
begin
  if RunStatusStrList.IndexOfName(AType) < 0 then
    Exit;
  RunStatusStrList.Delete(RunStatusStrList.IndexOfName(AType));
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.ClearContents;
  if RunStatusStrList.Count > 0 then
    for i := 0 to RunStatusStrList.Count - 1 do
      FRuntimeProgressDlg.UpdateRunStatusInfo(RunStatusStrList.Names[i], RunStatusStrList.ValueFromIndex[i]);
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
  {$ENDIF}
end;

procedure TRuntimeProgress.setVisible(FVisible: Boolean);
begin
  {$IFDEF VISUAL_BUILD}
  FRuntimeProgressDlg.Visible := True;
  {$ENDIF}
end;

procedure TRuntimeProgress.SetFixedColWidthForStr(AString: String);
var
  longestStr: String;
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
    WriteLn(AInfo)
  else
    WriteLn(MakePrettyString('     ' + AType, 25) + ' : ' + AInfo);
  {$ENDIF}
end;

procedure ShowProgressStatic(ProgressIn: Integer);
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    exit;

  WriteLn('     ' + IntToStr(ProgressIn) + '%');
  {$ENDIF}
end;

procedure ShowProgressIncrementStatic;
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.RunSilent then
    Exit;

  Write('.');
  {$ENDIF}
end;

end.

