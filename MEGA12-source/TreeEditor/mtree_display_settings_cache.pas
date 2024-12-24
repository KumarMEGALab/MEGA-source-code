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

unit mtree_display_settings_cache;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTreeDisplaySettingsCache }

  TTreeDisplaySettingsCache = class(TObject)
    private
      FShowBlens: Boolean;
      FShowCharState: Boolean;
      FShowCompressedClusterSize: Boolean;
      FShowDivTimes: Boolean;
      FShowGeneDupMarkers: Boolean;
      FShowHeightErrBar: Boolean;
      FShowImages: Boolean;
      FShowNodeIds: Boolean;
      FShowNodeLabels: Boolean;
      FShowSamplingTimeScale: Boolean;
      FShowScale: Boolean;
      FShowSpeciationMarkers: Boolean;
      FShowStats: Boolean;
      FShowStatsRange: Boolean;
      FShowDataCoverage: Boolean;
      FShowTimeScale: Boolean;
    public
      constructor Create;

      property ShowBlens: Boolean read FShowBlens write FShowBlens;
      property ShowStats: Boolean read FShowStats write FShowStats;
      property ShowStatsRange: Boolean read FShowStatsRange write FShowStatsRange;
      property ShowCharState: Boolean read FShowCharState write FShowCharState;
      property ShowDivTimes: Boolean read FShowDivTimes write FShowDivTimes;
      property ShowNodIds: Boolean read FShowNodeIds write FShowNodeIds;
      property ShowDataCoverage: Boolean read FShowDataCoverage write FShowDataCoverage;
      property ShowNodeLabels: Boolean read FShowNodeLabels write FShowNodeLabels;

      property ShowGeneDupMarkers: Boolean read FShowGeneDupMarkers write FShowGeneDupMarkers;
      property ShowSpeciationMarkers: Boolean read FShowSpeciationMarkers write FShowSpeciationMarkers;
      property ShowImages: Boolean read FShowImages write FShowImages;
      property ShowScale : Boolean read FShowScale write FShowScale;
      property ShowTimeScale : Boolean read FShowTimeScale write FShowTimeScale;
      property ShowSamplingTimeScale: Boolean read FShowSamplingTimeScale write FShowSamplingTimeScale;
      property ShowCompressedClusterSize: Boolean read FShowCompressedClusterSize write FShowCompressedClusterSize;
      property ShowHeightErrBar: Boolean read FShowHeightErrBar write FShowHeightErrBar;
  end;

implementation

{ TTreeDisplaySettingsCache }

constructor TTreeDisplaySettingsCache.Create;
begin
  FShowBlens := False;
  FShowCharState := False;
  FShowDivTimes := False;
  FShowNodeIds := False;
  FShowStats := False;
  FShowStatsRange := False;
  FShowDataCoverage := False;
  FShowNodeLabels := False;
  FShowGeneDupMarkers := False;
  FShowSpeciationMarkers := False;
  FShowImages := False;
  FShowTimeScale := False;
  FShowSamplingTimeScale := False;
  FShowCompressedClusterSize := False;
  FShowHeightErrBar := False;
end;

end.

