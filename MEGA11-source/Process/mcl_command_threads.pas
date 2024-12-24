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

unit mcl_command_threads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mdist_command_threads;

type

  { TMclCommandThread }

  TMclCommandThread = class(TDistCommandThread)
  private
    function GetFigureLegend: TStringList;
    protected
      FFigureLegend: TStringList;
      procedure AllocateDistComputer; override;
      procedure UpdateMatricesForExactTest; override;
      procedure GenerateFigureLegend;
      function DoExecute: Boolean; override;
    public

      destructor Destroy; override;
      property FigureLegend: TStringList read GetFigureLegend;
  end;

implementation

uses
  MLegendGenerator, mdistpack, MSeqDistBase, MNucDist, math, MegaConsts;

{ TMclCommandThread }

function TMclCommandThread.GetFigureLegend: TStringList;
begin
  Result := TStringList.Create;
  if Assigned(FFigureLegend) then
    Result.AddStrings(FFigureLegend);
end;

procedure TMclCommandThread.AllocateDistComputer;
begin
  if MAI.MyDistPack.DoesContain(gdAmino) then
    raise Exception.Create('MCL computation not available for amino acid data');
  inherited AllocateDistComputer;
end;

procedure TMclCommandThread.UpdateMatricesForExactTest;
begin
  { a stub because it is not applicable for MCL}
  { NOTE: violates the Open-Closed principle as this modifies the parent class.
          In a better design, we would extend the parent class without modification.
          This would not be hard to fix - just add a base parent class that does
          everything else except this and make this an abstract procedure in the
          base class.
  }
end;

procedure TMclCommandThread.GenerateFigureLegend;
var
  CaptionExpert : TLegendGenerator = nil;
  a1, a2, b: Double;
  R, { G_To_T,} Qsum : Double;
begin
  try
    Assert((UsrOperation = dtdoMCLComputePattern) or (UsrOperation = dtdoMCLTsTvBias), 'invalid user operatiion specified for MCL command');
    CaptionExpert := TLegendGenerator.Create;
    CaptionExpert.LoadTemplateFromFile('MCL_substitution_pattern.htm');
    if  UsrOperation = dtdoMCLComputePattern then
      CaptionExpert.AssignData('Title','MCL Substitution Matrix')
    else if UsrOperation = dtdoMCLTsTvBias then
      CaptionExpert.AssignData('Title', 'MCL Transition/Transversion Bias');
    with DistComputer as TNucDist do begin
      //G_To_T := 1; //MCL_Freq_T;  note that it assumes that everything is divided by beta
      CaptionExpert.AssignData('k1',FloatToStr(RoundTo(MCL_kappa1,-3)));
      CaptionExpert.AssignData('k2',FloatToStr(RoundTo(MCL_kappa2,-3)));
      CaptionExpert.AssignData('FreqA', FloatToStrF(RoundTo(MCL_Freq_A,-4)*100, ffFixed, 12, 2)+'%');
      CaptionExpert.AssignData('FreqT', FloatToStrF(RoundTo(MCL_Freq_T,-4)*100, ffFixed, 12, 2)+'%');
      CaptionExpert.AssignData('FreqC', FloatToStrF(RoundTo(MCL_Freq_C,-4)*100, ffFixed, 12, 2)+'%');
      CaptionExpert.AssignData('FreqG', FloatToStrF(RoundTo(MCL_Freq_G,-4)*100, ffFixed, 12, 2)+'%');

      a1 := MCL_kappa1;
      a2 := MCL_kappa2;
      b  := 1;
      Qsum := 0.0;
      Qsum := Qsum +        0        + (b *MCL_Freq_T) + (b *MCL_Freq_C) + (a1*MCL_Freq_G);
      Qsum := Qsum + (b *MCL_Freq_A) +         0       + (a2*MCL_Freq_C) + (b *MCL_Freq_G);
      Qsum := Qsum + (b *MCL_Freq_A) + (a2*MCL_Freq_T) +        0        + (b *MCL_Freq_G);
      Qsum := Qsum + (a1*MCL_Freq_A) + (b *MCL_Freq_T) + (b *MCL_Freq_C) +      0;
      if CompareValue(Qsum, 0.0, FP_CUTOFF) = 0 then
        Raise Exception.Create('Calculation error! Qsum == 0.  ProcessTestCmds in ProcessMCLCommands');

      CaptionExpert.AssignData('Qat',FloatToStr(RoundTo(((b *MCL_Freq_T)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qac',FloatToStr(RoundTo(((b *MCL_Freq_C)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qag',FloatToStr(RoundTo(((a1*MCL_Freq_G)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qaa','-'); //FloatToStr(RoundTo(-1*(MCL_Freq_T + MCL_Freq_C + (MCL_kappa1 * MCL_Freq_G)),-3)));

      CaptionExpert.AssignData('Qta',FloatToStr(RoundTo(((b *MCL_Freq_A)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qtc',FloatToStr(RoundTo(((a2*MCL_Freq_C)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qtg',FloatToStr(RoundTo(((b *MCL_Freq_G)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qtt','-');//FloatToStr(RoundTo(-1*(MCL_Freq_A + MCL_Freq_G + (MCL_kappa2 * MCL_Freq_C)),-3)));

      CaptionExpert.AssignData('Qca',FloatToStr(RoundTo(((b *MCL_Freq_A)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qct',FloatToStr(RoundTo(((a2*MCL_Freq_T)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qcg',FloatToStr(RoundTo(((b *MCL_Freq_G)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qcc','-'); //FloatToStr(RoundTo(-1*(MCL_Freq_A + MCL_Freq_G + (MCL_kappa2 * MCL_Freq_T)),-3)));

      CaptionExpert.AssignData('Qga',FloatToStr(RoundTo(((a1*MCL_Freq_A)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qgt',FloatToStr(RoundTo(((b *MCL_Freq_T)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qgc',FloatToStr(RoundTo(((b *MCL_Freq_C)/Qsum * 100),-2)));
      CaptionExpert.AssignData('Qgg','-'); //FloatToStr(RoundTo(-1*(MCL_Freq_C + MCL_Freq_T + (MCL_kappa1 * MCL_Freq_A)),-3)));

      R := ((MCL_kappa2 * MCL_Freq_C * MCL_Freq_T) + (MCL_kappa1 * MCL_Freq_G * MCL_Freq_A)) /
              ((MCL_Freq_A + MCL_Freq_G) * (MCL_Freq_T + MCL_Freq_C));
      CaptionExpert.AssignData('R',FloatToStr(RoundTo(R,-3)));
    end; // end with DistComputer
    if UsrOperation =  dtdoMCLTsTvBias then
      CaptionExpert.AssignData('MCLType', 'TsTvBias')
    else if UsrOperation = dtdoMCLComputePattern then
      CaptionExpert.AssignData('MCLType', 'Pattern');

    CaptionExpert.BindData(MAI.MyDistPack, True);
    CaptionExpert.BindData(MAI, True);
    FFigureLegend := TStringList.Create;
    FFigureLegend.Text := CaptionExpert.GenerateLegend;
    {$IFNDEF VISUAL_BUILD}
    CaptionExpert.SaveCaptionToFile;
    {$ENDIF}
  finally
    if Assigned(CaptionExpert) then
      CaptionExpert.Free;
  end;
end;

function TMclCommandThread.DoExecute: Boolean;
begin
  try
    Result := inherited DoExecute;
    GenerateFigureLegend;
  finally
    if Assigned(ARP) then
    begin
      if Assigned(MAI) then
        MAI.ARP := nil;
      ARP.Hide;
    end;
  end;
end;

destructor TMclCommandThread.Destroy;
begin
  inherited Destroy;
end;

end.

