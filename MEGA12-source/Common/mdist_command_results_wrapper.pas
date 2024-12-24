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

unit mdist_command_results_wrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mdistpack, MegaConsts, MDisplayMatrixDlg;

type

  { TDistCommandResultsData }

  TDistCommandResultsData = class(TObject)
    public
      MyD: PDistanceMatrix;
      MyV: PDistanceMatrix;
      MyMeanGpD: ArrayOfDouble;
      MyMeanGpV: ArrayOfDouble; // within gp means
      MyMeanD: Double;
      MyMeanV: Double;
      constructor Create;
  end;

  { TDistCommandDataWrapper }

  { TDistCommandResultsWrapper }

  TDistCommandResultsWrapper = class(TObject)
    private
      FOwnsResultsData: Boolean;
    public
      UsrOperation: TDistTreeDlgOption;
      DispDlg: TD_DisplayMatrix;
      DispDlgVS: TVS_DisplayMatrix;
      {$IFDEF VISUAL_BUILD}
      DispDlgV: TDisplayMatrixDlg;
      {$ENDIF}
      MatrixExportStrings: TStringList;
      constructor Create(aDispDlg: TD_DisplayMatrix; aDispDlgVS: TVS_DisplayMatrix{$IFDEF VISUAL_BUILD}; aDispDlgV: TDisplayMatrixDlg{$ENDIF});
      destructor Destroy; override;
      property OwnsResultsData: Boolean read FOwnsResultsData write FOwnsResultsData;
  end;

implementation

{ TDistCommandResultsData }

constructor TDistCommandResultsData.Create;
begin
  MyD := nil;
  MyV := nil;
  MyMeanGpD := nil;
  MyMeanGpV := nil;
  MyMeanD := -1;
  MyMeanV := -1;
end;

{ TDistCommandResultsWrapper }

constructor TDistCommandResultsWrapper.Create(aDispDlg: TD_DisplayMatrix; aDispDlgVS: TVS_DisplayMatrix{$IFDEF VISUAL_BUILD}; aDispDlgV: TDisplayMatrixDlg{$ENDIF});
begin
  FOwnsResultsData := True;
  DispDlg := aDispDlg;
  DispDlgVS := aDispDlgVS;
  {$IFDEF VISUAL_BUILD}
  DispDlgV := aDispDlgV;
  {$ENDIF}
  MatrixExportStrings := nil;
end;

destructor TDistCommandResultsWrapper.Destroy;
begin
  if FOwnsResultsData then
  begin
    {$IFDEF VISUAL_BUILD}
    if Assigned(DispDlgV) then
      DispDlgV.Close;
    {$ELSE}
    if Assigned(DispDlg) then
      DispDlg.Free;
    if Assigned(DispDlgVS) then
      DispDlgVS.Free;
    {$ENDIF}
  end;
  if Assigned(MatrixExportStrings) then
    MatrixExportStrings.Free;
  inherited Destroy;
end;

end.

