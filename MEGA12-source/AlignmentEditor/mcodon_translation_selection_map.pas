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

unit mcodon_translation_selection_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mstringbuilder;

type

  { TCodonTranslationSelectionMap }

  TCodonTranslationSelectionMap = class(TObject)


    private

      FCodingDnaSelection: TRect;
      FAminoAcidSelection: TRect;
      FMaxDnaSites: Int64;
      FNumSeqs: Integer;
      procedure SetMaxDnaSites(AValue: Int64);
      procedure SetNumSeqs(AValue: Integer);
      procedure Validate;
    public
      constructor Create(aNumSeqs: Integer; aMaxDnaSites: Integer);

      procedure SetCodingDnaSelection(aRect: TRect);
      procedure SetAminoAcidSelection(aRect: TRect);

      procedure SanityTestsDna(aRect: TRect; var aList: TStringList);
      procedure SanityTestsAA(aRect: TRect; var aList: TStringList);
      property NumSeqs: Integer read FNumSeqs write SetNumSeqs;
      property MaxDnaSites: Int64 read FMaxDnaSites write SetMaxDnaSites;
      property CurrentDnaSelection: TRect read FCodingDnaSelection;
      property CurrentAASelection: TRect read FAminoAcidSelection;
  end;

  function DoSanityTestsForCodonTranslationSelectionMap: TStringList;

implementation

function DoSanityTestsForCodonTranslationSelectionMap: TStringList;
var
  numSeqs: Integer = 11;
  numSites: Int64 = 50;
  aRect: TRect;
  map: TCodonTranslationSelectionMap = nil;
begin
  try
    Result := TStringList.Create;
    map := TCodonTranslationSelectionMap.Create(numSeqs, numSites);

    aRect.Top := 1;
    aRect.Bottom := map.NumSeqs;
    aRect.Left := 1;
    aRect.Right := 1;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := 11;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := 12;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := 13;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := numSites;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := numSites - 1;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := numSites - 2;
    map.SanityTestsDna(aRect, Result);

    aRect.Right := numSites - 3;
    map.SanityTestsDna(aRect, Result);

    aRect.Left := numSites - 3;
    map.SanityTestsDna(aRect, Result);

    aRect.Left := numSites - 2;
    aRect.Right := numSites;
    map.SanityTestsDna(aRect, Result);

    aRect.Left := numSites - 1;
    map.SanityTestsDna(aRect, Result);

    aRect.Left := numSites;
    map.SanityTestsDna(aRect, Result);

    aRect.Left := numSites;
    aRect.Right := numSites;
    map.SanityTestsDna(aRect, Result);

    aRect.Left := 1;
    aRect.Right := 1;
    map.SanityTestsAA(aRect, Result);

    aRect.Right := 11;
    map.SanityTestsAA(aRect, Result);

    aRect.Right := 12;
    map.SanityTestsAA(aRect, Result);

    aRect.Right := 13;
    map.SanityTestsAA(aRect, Result);

    numSites := ((numSites - 1) div 3) + 1;
    map.MaxDnaSites := numSites*3;
    Result.Add(EmptyStr);
    Result.Add(Format('*** changed num sites to %d for AA tests ***', [numSites]));
    Result.Add(EmptyStr);

    aRect.Right := numSites;
    map.SanityTestsAA(aRect, Result);

    aRect.Right := numSites - 1;
    map.SanityTestsAA(aRect, Result);

    aRect.Right := numSites - 2;
    map.SanityTestsAA(aRect, Result);

    aRect.Right := numSites - 3;
    map.SanityTestsAA(aRect, Result);

    aRect.Left := numSites - 3;
    map.SanityTestsAA(aRect, Result);

    aRect.Left := numSites - 2;
    aRect.Right := numSites;
    map.SanityTestsAA(aRect, Result);

    aRect.Left := numSites - 1;
    map.SanityTestsAA(aRect, Result);

    aRect.Left := numSites;
    map.SanityTestsAA(aRect, Result);

    aRect.Left := numSites;
    aRect.Right := numSites;
    map.SanityTestsAA(aRect, Result);

  finally
    if Assigned(map) then
      map.Free;
  end;
end;

{ TCodonTranslationSelectionMap }

procedure TCodonTranslationSelectionMap.SetMaxDnaSites(AValue: Int64);
begin
  if FMaxDnaSites = AValue then Exit;
  FMaxDnaSites := AValue;
end;

procedure TCodonTranslationSelectionMap.SetNumSeqs(AValue: Integer);
begin
  if FNumSeqs = AValue then Exit;
  FNumSeqs := AValue;
end;

procedure TCodonTranslationSelectionMap.Validate;
begin
  if FCodingDnaSelection.Left < 1 then
    raise Exception.Create(Format('DnaSelection.Left=%d but the min value is 1', [FCodingDnaSelection.Left]));

  if FCodingDnaSelection.Right > FMaxDnaSites then
    raise Exception.Create(Format('DnaSelection.Right=%d but the max value is %d', [FCodingDnaSelection.Right, FMaxDnaSites]));

  if FCodingDnaSelection.Left > FCodingDnaSelection.Right then
    raise Exception.Create(Format('DnaSelection.Left=%d is greater than DnaSelection.Right=%d', [FCodingDnaSelection.Left, FCodingDnaSelection.Right]));

  if FCodingDnaSelection.Top > FCodingDnaSelection.Bottom then
    raise Exception.Create(Format('DnaSelection.Left=%d is greater than DnaSelection.Right=%d', [FCodingDnaSelection.Top, FCodingDnaSelection.Bottom]));

  if FAminoAcidSelection.Left < 1 then
    raise Exception.Create(Format('AASelection.Left=%d but the min value is 1', [FAminoAcidSelection.Left]));

  if FAminoAcidSelection.Right > (((FMaxDnaSites - 1) div 3) + 1) then
    raise Exception.Create(Format('AASelection.Right=%d but the max value is %d', [FAminoAcidSelection.Right, ((FMaxDnaSites - 1) div 3) + 1]));

  if FAminoAcidSelection.Left > FAminoAcidSelection.Right then
    raise Exception.Create(Format('DnaSelection.Left=%d is greater than DnaSelection.Right=%d', [FAminoAcidSelection.Left, FAminoAcidSelection.Right]));

  if FAminoAcidSelection.Top > FAminoAcidSelection.Bottom then
    raise Exception.Create(Format('DnaSelection.Left=%d is greater than DnaSelection.Right=%d', [FAminoAcidSelection.Top, FAminoAcidSelection.Bottom]));

  if FCodingDnaSelection.Right > FMaxDnaSites then
    raise Exception.Create(Format('max DNA sites = %d but DnaSelection.Right = %d', [FMaxDnaSites, FCodingDnaSelection.Right]));

end;

constructor TCodonTranslationSelectionMap.Create(aNumSeqs: Integer; aMaxDnaSites: Integer);
begin
  FNumSeqs := aNumSeqs;
  FMaxDnaSites := aMaxDnaSites;
end;

procedure TCodonTranslationSelectionMap.SetCodingDnaSelection(aRect: TRect);
begin
  if aRect.Right > FMaxDnaSites then
    FMaxDnaSites := aRect.Right;
  FCodingDnaSelection.Top := aRect.Top;
  FCodingDnaSelection.Bottom := aRect.Bottom;
  FCodingDnaSelection.Left := aRect.Left;
  FCodingDnaSelection.Right := aRect.Right;

  FAminoAcidSelection.Top := aRect.Top;
  FAminoAcidSelection.Bottom := aRect.Bottom;
  FAminoAcidSelection.Left := ((FCodingDnaSelection.Left - 1) div 3) + 1;
  FAminoAcidSelection.Right := ((FCodingDnaSelection.Right - 1) div 3) + 1;
  Validate;
end;

procedure TCodonTranslationSelectionMap.SetAminoAcidSelection(aRect: TRect);
begin
  if aRect.Right*3 > FMaxDnaSites then
    FMaxDnaSites := aRect.Right*3;
  FAminoAcidSelection.Top := aRect.Top;
  FAminoAcidSelection.Bottom := aRect.Bottom;
  FAminoAcidSelection.Left := aRect.Left;
  FAminoAcidSelection.Right := aRect.Right;

  FCodingDnaSelection.Top := aRect.Top;
  FCodingDnaSelection.Bottom := aRect.Bottom;
  FCodingDnaSelection.Left := ((FAminoAcidSelection.Left - 1)*3) + 1;
  FCodingDnaSelection.Right := ((FAminoAcidSelection.Right - 1)*3) + 3;
  Validate;
end;

procedure TCodonTranslationSelectionMap.SanityTestsDna(aRect: TRect; var aList: TStringList);
begin
  SetCodingDnaSelection(aRect);
  if aList.Count > 0 then
    aList.Add(EmptyStr);
  aList.Add(Format('Set DNA selection from site %d to site %d', [aRect.Left, aRect.Right]));
  aList.Add(Format('AA selection is from site %d to %d', [FAminoAcidSelection.Left, FAminoAcidSelection.Right]));
end;

procedure TCodonTranslationSelectionMap.SanityTestsAA(aRect: TRect; var aList: TStringList);
begin
  SetAminoAcidSelection(aRect);
  if aList.Count > 0 then
    aList.Add(EmptyStr);
  aList.Add(Format('Set AA selection from site %d to site %d', [aRect.Left, aRect.Right]));
  aList.Add(Format('DNA selection is from site %d to %d', [FCodingDnaSelection.Left, FCodingDnaSelection.Right]));
end;

end.

