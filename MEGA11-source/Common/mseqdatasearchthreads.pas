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

unit MSeqDataSearchThreads;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, StdCtrls, ExtCtrls, SysUtils,
  MegaConsts, RegExpr, MD_InputSeqData, MegaUtils_NV;

type
  TMotifSearchThread = class(TMEGAThread)
  protected
    NewPercent: integer;
    PrevPercent: Integer;
    FCanceled: boolean;
    Motif: AnsiString;
    MotifExpanded: AnsiString;
    RowToStart: Integer;
    ColToStart: Integer;
    StopAfterFirst: Boolean;
    NoOfTaxa: Integer;
    currRes: TSearchResult;
    AProgressIndex: Integer;
    searchingForNext: Boolean;
  protected
    procedure Execute; override;
    function ExpandMotif(Motif: AnsiString): AnsiString;
    procedure AddSearchResult;
    procedure ReportNextLoc;
    procedure MotifProgressUpdate;
    procedure MotifSearchDone;
  public
    constructor Create(AMotif: AnsiString; ARowToStart, AColToStart: Integer; AStopAfterFirst: Boolean; ProgressIndex: Integer; IsSearchingForNext: Boolean);
  end;

  { TCountMotifsThread }

  TCountMotifsThread = class(TMEGAThread)
    private
      FNumMatchesFound: Int64;
      FIsSuccess: Boolean;
      FMotif: String;
      function GetLogText: String;
    protected
      FLog: TStringList;
      function DoCount: Boolean;
      procedure Execute; override;
    public
      constructor Create(aMotif: String);
      destructor Destroy; override;
      property Motif: String read FMotif;
      property IsSuccess: Boolean read FIsSuccess;
      property LogText: String read GetLogText;
      property NumMatchesFound: Int64 read FNumMatchesFound;
  end;

{ TODO 5 -oDan -cNeeds to be improved in the future : Ran out of time to make the Epitope search thread based.  Sudhir has asked me to move on to another project.  When time is available make this thread based, it could create a speedup relative to the # of cores the user's cpu has.}


implementation

uses
  MegaUtils;

{ TCountMotifsThread }

function TCountMotifsThread.GetLogText: String;
begin
  Result := FLog.Text;
end;

function TCountMotifsThread.DoCount: Boolean;
var
  seq: Integer = 0;
  site: Integer = 1;
  ambiguities: Integer = 0;
  {$IFDEF DEBUG}
  debug: TStringList = nil;
  temp: String;
  {$ENDIF}
begin
  Result := False;
  try
    FNumMatchesFound := 0;
    for seq := 0 to D_InputSeqData.NoOfTaxa - 1 do
      for site := 0 to D_InputSeqData.NoOfSites - Length(FMotif) do
        if D_InputSeqData.CheckKMer(FMotif, seq, site, ambiguities, False) then
          inc(FNumMatchesFound);
    FIsSuccess := True;
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      {$IFDEF DEBUG}
      debug := DumpExceptionCallStack(E);
      if Assigned(debug) then
      begin
        temp := debug.Text;
        FLog.Add(debug.Text);
        debug.Free;
      end;
      {$ENDIF}
      FLog.Add(E.Message);
    end;
  end;
  Result := FIsSuccess;
end;

procedure TCountMotifsThread.Execute;
begin
  while True do
  begin
    FIsSuccess := DoCount;
    Terminate;
    Exit;
  end;
end;

constructor TCountMotifsThread.Create(aMotif: String);
begin
  inherited Create(True);
  FMotif := UpperCase(aMotif);
  FLog := TStringList.Create;
  FreeOnTerminate := True;
end;

destructor TCountMotifsThread.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

function TMotifSearchThread.ExpandMotif(Motif: AnsiString): AnsiString;
var
  i: Integer;
begin
  i:=1;
  while i < Length(Motif) do
  begin
    Motif := Copy(Motif, 0, i) + '-*' + Copy(Motif, i+1, Length(Motif));
    i := i + 3;
  end;
  if D_InputSeqData.IsAmino then
  begin
    Motif := ReplaceRegExpr('[X]', Motif, '.', false);
    result := Motif;
  end
  else
  begin
    Motif := ReplaceRegExpr('[T|U]', Motif, '[T|U]', False); //  'T': result := (box[i] = 'T') or (box[i] = 'U');  && 'U': result := (box[i] = 'U') or (box[i] = 'T');
    Motif := ReplaceRegExpr('R', Motif, '[R|A|G]', False); //   'R': result := (box[i] = 'R') or (box[i] = 'A') or (box[i] = 'G');
    Motif := ReplaceRegExpr('Y', Motif, '[Y|T|C|U]', False); //  'Y': result := (box[i] = 'Y') or (box[i] = 'T') or (box[i] = 'C') or (box[i] = 'U');
    Motif := ReplaceRegExpr('M', Motif, '[A|C]', False); //  'M': result := (box[i] = 'M') or (box[i] = 'A') or (box[i] = 'C');
    Motif := ReplaceRegExpr('K', Motif, '[K|T|G|U]', False); //  'K': result := (box[i] = 'K') or (box[i] = 'T') or (box[i] = 'G') or (box[i] = 'U');
    Motif := ReplaceRegExpr('S', Motif, '[S|C|G]', False); //  'S': result := (box[i] = 'S') or (box[i] = 'C') or (box[i] = 'G');
    Motif := ReplaceRegExpr('W', Motif, '[W|A|T|U]', False); //  'W': result := (box[i] = 'W') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U');
    Motif := ReplaceRegExpr('B', Motif, '[B|T|U|C|G|Y|K|S]', False); //  'B': result := (box[i] = 'B') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'C') or (box[i] = 'G') or (box[i] = 'Y') or (box[i] = 'K') or (box[i] = 'S');
    Motif := ReplaceRegExpr('V', Motif, '[V|A|C|G|R|M|S]', False); //  'V': result := (box[i] = 'V') or (box[i] = 'A') or (box[i] = 'C') or (box[i] = 'G')                   or (box[i] = 'R') or (box[i] = 'M') or (box[i] = 'S');
    Motif := ReplaceRegExpr('D', Motif, '[D|A|T|U|G|R|K|W]', False); // 'D': result := (box[i] = 'D') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'G') or (box[i] = 'R') or (box[i] = 'K') or (box[i] = 'W');
    Motif := ReplaceRegExpr('H', Motif, '[H|A|T|U|C|Y|M|W]', False); //  'H': result := (box[i] = 'H') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'C') or (box[i] = 'Y') or (box[i] = 'M') or (box[i] = 'W');
    Motif := ReplaceRegExpr('N', Motif, '.', False); // 'N': result := true;
    result := Motif;
  end;
end;

constructor TMotifSearchThread.Create(AMotif: AnsiString; ARowToStart, AColToStart: Integer; AStopAfterFirst: Boolean; ProgressIndex: Integer; IsSearchingForNext: Boolean);
begin
  inherited Create(True);  // start the thread in suspend mode
  Motif := AMotif;
  RowToStart := ARowToStart;
  ColToStart := AColToStart;
  StopAfterFirst := AStopAfterFirst;
  FreeOnTerminate := True; // cleans up it's own allocated stuff rather tna relying on the main mega thread.
  NoOfTaxa := D_InputSeqData.NoOfTaxa;
  AProgressIndex := ProgressIndex;
  searchingForNext := IsSearchingForNext;
end;

procedure TMotifSearchThread.Execute;
var
  CurSequence: AnsiString;
  Regex: TRegExpr;
  i, l: Integer;
  TerminateFlag, skipByNotUsedSite: Boolean;
begin
  Regex := TRegExpr.Create;
  MotifExpanded := ExpandMotif(Motif);
  TerminateFlag := false;
  try
    i:=RowToStart; // setup for while loop
    if RowToStart < 0 then
      RowToStart := 0;
    if ColToStart < 0 then
      ColToStart := 0;
    while (i <> RowToStart-1) and (not TerminateFlag) do
    begin

      if ((i mod NoOfTaxa) < RowToStart) then
      begin
        // already looped back
        newPercent := (((NoOfTaxa-RowToStart) + (i mod NoOfTaxa))*100 div NoOfTaxa);

        if prevPercent <> newPercent then
          Synchronize(MotifProgressUpdate);

        prevPercent := newPercent;
      end
      else
      begin
        // hasn't yet reached the bottom
        newPercent := ((i - RowToStart)*100 div NoOfTaxa);

        if prevPercent <= (newPercent-10) then
          Synchronize(MotifProgressUpdate);
      end;

      if not D_InputSeqData.SeqUsed[i] then // skip unused sequences.
      begin
        i := (i mod NoOfTaxa) + 1;
        Continue;
      end;
      CurSequence := UpperCase(D_InputSeqData.Sequence[i]);
      Regex.InputString := Copy(CurSequence, ColToStart+1, Length(CurSequence));
      Regex.Expression := MotifExpanded;
      if Regex.Exec(Regex.InputString) then
      REPEAT
        if D_InputSeqData.SearchTreadCanceled then
        begin
          Synchronize(D_InputSeqData.ClearSearchResults);
          TerminateFlag := true;
          Terminate;
        end
        else
        begin
          // Check that none of the result is in a domain which is unchecked.  If it is then throw out this result!  // DP Verified.
          skipByNotUsedSite := false;
          for l:=Regex.MatchPos[0]-1 + ColToStart to Regex.MatchPos[0]-1 + ColToStart + Regex.MatchLen[0] do
            if not D_InputSeqData.FDomainMarks.IsSiteUsed[l] then
              skipByNotUsedSite := true;

          if not skipByNotUsedSite then
          begin
            currRes := TSearchResult.Create();
            currRes.Top := i;
            currRes.Left := Regex.MatchPos[0]-1 + ColToStart;
            currRes.Right := currRes.Left + Regex.MatchLen[0];
            currRes.SearchStr := Motif;
            currRes.UserInput := Motif;
            currRes.MatchStr := Regex.Match[Regex.SubExprMatchCount];
            if searchingForNext then
              Synchronize(ReportNextLoc)
            else
              Synchronize(AddSearchResult);
            if StopAfterFirst then
            begin
              Terminate; // we have found a match and only want the first result
              TerminateFlag := true;
            end;
          end;
        end;
      UNTIL (not Regex.ExecNext) or (TerminateFlag); // repeat until there is no posative match for this motif and this line.

      ColToStart := 0; // After the first line start searching at the front again.      if not back then
      i := (i mod NoOfTaxa) + 1; // This allows us to loop around if we reach the end and there is no match.
      if (RowToStart = 0) and (i = NoOfTaxa) then
        i := -1;
    end;

  finally
    FreeAndNil(Regex);
    Synchronize(MotifSearchDone);
  end;

end;

procedure TMotifSearchThread.AddSearchResult;
begin
  D_InputSeqData.AddSearchResult(currRes);
end;

procedure TMotifSearchThread.MotifProgressUpdate;
begin
  D_InputSeqData.MotifProgressUpdate(AProgressIndex, newPercent);
end;

procedure TMotifSearchThread.MotifSearchDone;
begin
  D_InputSeqData.MotifProgressUpdate(AProgressIndex, 100);
  D_InputSeqData.CheckRemainingSearchThreads(searchingFOrNext);
end;

procedure TMotifSearchThread.ReportNextLoc;
begin
  D_InputSeqData.ReportNextLoc(currRes);
end;

end.
