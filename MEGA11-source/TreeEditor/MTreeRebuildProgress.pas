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

unit MTreeRebuildProgress;

interface

uses
  Classes, MegaConsts;

type

  TRebuildTreeProgress = class(TObject)
    private
      FPhase: TTreeRebuildPhase;
      FBaseProgress: Integer; { for a given phase, its starting progress}
      FPhaseProgress: Integer;
      FOverallProgress: Integer;
      function PhaseLength(APhase: TTreeRebuildPhase): Integer;
      procedure SetPhase(const Value: TTreeRebuildPhase);
      procedure SetPhaseProgress(const Value: Integer);
      procedure SetOverallProgress(const Value: Integer);

    protected

    public
      constructor Create;
      destructor Destroy; override;
      function PhaseToString(APhase: TTreeRebuildPhase): String;
      procedure UpdatePhaseProgress(const PhaseProg: Integer; var OverallProg: Integer); overload;
      procedure UpdatePhaseProgress(const APhase: TTreeRebuildPhase; const PhaseProg: Integer; var OverallProg: Integer); overload;
      procedure Reset;
      property OverallProgress: Integer read FOverallProgress write SetOverallProgress;
      property Phase: TTreeRebuildPhase read FPhase write SetPhase;
      property PhaseProgress: Integer read FPhaseProgress write SetPhaseProgress;
  end;

implementation

{ TRebuildTreeProgress }

constructor TRebuildTreeProgress.Create;
begin
  Phase := rpIdle;
end;

destructor TRebuildTreeProgress.Destroy;
begin
  inherited;
end;

function TRebuildTreeProgress.PhaseLength(APhase: TTreeRebuildPhase): Integer;
begin
  case APhase of
    rpIdle:              Result := 0;
    rpSetPosition:       Result := 2;
    rpSetSize:           Result := 2;
    rpCreateMetafile:    Result := 3;
    rpDrawBranches:      Result := 40;
    rpDrawMarkers:       Result := 1;
    rpDrawNodeIds:       Result := 5;
    rpDrawDivTimes:      Result := 5;
    rpDrawStats:         Result := 5;
    rpDrawDataCoverage:  Result := 5;
    rpDrawBranchLengths: Result := 5;
    rpDrawCharState:     Result := 5;
    rpDrawScale:         Result := 2 ;
    rpPaint:             Result := 20;
  end;
end;

function TRebuildTreeProgress.PhaseToString(APhase: TTreeRebuildPhase): String;
begin
  case APhase of
    rpIdle: Result := 'Idle';
    rpSetPosition: Result := 'Calculating position...';
    rpSetSize: Result := 'Calculating size...';
    rpCreateMetafile: Result := 'Creating metafile...';
    rpDrawBranches: Result := 'Drawing branches...';
    rpDrawMarkers: Result := 'Drawing markers...';
    rpDrawNodeIds: Result := 'Drawing node IDs...';
    rpDrawDivTimes: Result := 'Drawing divergence times...';
    rpDrawStats: Result := 'Drawing bootstrap values...';
    rpDrawDataCoverage: Result := 'Drawing data coverage...';
    rpDrawBranchLengths: Result := 'Drawing branch lengths...';
    rpDrawCharState: Result := 'Drawing ancestral states...';
    rpDrawScale: Result := 'Drawing scale...';
    rpPaint: Result := 'Repainting the canvas...';
  end;
end;

procedure TRebuildTreeProgress.Reset;
begin
  FPhase := rpSetPosition;
  FPhaseProgress := 0;
  FOverallProgress := 0;
end;

procedure TRebuildTreeProgress.SetOverallProgress(const Value: Integer);
begin
  if FOverallProgress <> Value then
    FOverallProgress := Value;
end;

procedure TRebuildTreeProgress.SetPhase(const Value: TTreeRebuildPhase);
begin
  if FPhase = Value then
    Exit;
  FPhase := Value;
  case FPhase of
    rpIdle:
      begin
        FOverallProgress := 0;
        FPhaseProgress := 0;
      end;
    rpSetPosition:
      begin
        FOverallProgress := 1;
        FPhaseProgress := 0;
      end;
    rpSetSize: { 2%}
      begin

        FOverallProgress := 2;
        FPhaseProgress := 0;
      end;
    rpCreateMetafile: { 3%}
      begin
        FOverallProgress := 4;
        FPhaseProgress := 0;
      end;
    rpDrawBranches: { 40%}
      begin
        FOverallProgress := 7;
        FPhaseProgress := 0;
      end;
    rpDrawMarkers: { 1%}
      begin
        FOverallProgress := 47;
        FPhaseProgress := 0;
      end;
    rpDrawNodeIds: { 5%}
      begin
        FOverallProgress := 48;
        FPhaseProgress := 0;
      end;
    rpDrawDivTimes: { 5%}
      begin
        FOverallProgress := 53;
        FPhaseProgress := 0;
      end;
    rpDrawStats: { 5%}
      begin
        FOverallProgress := 58;
        FPhaseProgress := 0;
      end;
    rpDrawDataCoverage: { 5%}
      begin
        FOverallProgress := 63;
        FPhaseProgress := 0;
      end;
    rpDrawBranchLengths: { 5%}
      begin
        FOverallProgress := 68;
        FPhaseProgress := 0;
      end;
    rpDrawCharState: { 5%}
      begin
        FOverallProgress := 73;
        FPhaseProgress := 0;
      end;
    rpDrawScale: { 1%}
      begin
        FOverallProgress := 78;
        FPhaseProgress := 0;
      end;
    rpPaint: { 20%}
      begin
        FOverallProgress := 79;
        FPhaseProgress := 0;
      end;
  end;
  FBaseProgress := FOverallProgress;
end;

procedure TRebuildTreeProgress.SetPhaseProgress(const Value: Integer);
begin
  if FPhaseProgress = Value then
    Exit;
  FPhaseProgress := Value;
  FOverallProgress := FBaseProgress + Round(PhaseLength(FPhase) * Value / 100);
end;

procedure TRebuildTreeProgress.UpdatePhaseProgress(const PhaseProg: Integer; var OverallProg: Integer);
begin
  SetPhaseProgress(PhaseProg);
  OverallProg := FOverallProgress;
end;

procedure TRebuildTreeProgress.UpdatePhaseProgress(const APhase: TTreeRebuildPhase; const PhaseProg: Integer; var OverallProg: Integer);
begin
  SetPhase(APhase);
  SetPhaseProgress(PhaseProg);
  OverallProg := FOverallProgress;
end;

end.
