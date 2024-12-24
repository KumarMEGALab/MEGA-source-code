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

unit MSubstitutionModelUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MAnalysisInfo, MLTree, MegaConsts;

function CreateSubstitutionModel(MAI: TAnalysisInfo; aCheckCancel: TCheckCancelFunc = nil): TGammaRateVariationModel;

implementation

uses
  ErrorMessages_HC, MDistPack, MLModels, MTreePack, MegaErrUtils;

function CreateSubstitutionModel(MAI: TAnalysisInfo; aCheckCancel: TCheckCancelFunc = nil): TGammaRateVariationModel;
var
  origCheckCancel: TCheckCancelFunc;
begin
  Result := nil;
  // setup model
  with MAI.MyDistPack do
  begin
    MAI.MyNoOfGammaCat := 1;
    if doesContain(gdGamma) then
      MAI.MyNoOfGammaCat := NoOfGammaCategories;

    case DistModel of
      gdJukesCantor : Result := TJCModel.Create      (0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
      gdKimura2para : Result := TK2Model.Create      (0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
      gdTamura      : Result := TT3Model.Create      (0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
      gdHKY         : Result := THKYModel.Create     (0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
      gdTamuraNei   : Result := TTN93Model.Create    (0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
      gdREV         : Result := TGTRModel.Create     (0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
      gdJones       : Result := TJTTModel.Create     (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdJonesPi     : Result := TJTTModel.Create     (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdDayhoff     : Result := TDayhoffModel.Create (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdDayhoffPi   : Result := TDayhoffModel.Create (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdMtRev24     : Result := TmtREV24Model.Create (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdMtRev24Pi   : Result := TmtREV24Model.Create (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdCpRev       : Result := TcpREVModel.Create   (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdCpRevPi     : Result := TcpREVModel.Create   (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdRtRev       : Result := TrtREVModel.Create   (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdRtRevPi     : Result := TrtREVModel.Create   (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdWAG         : Result := TWAGModel.Create     (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdWAGPi       : Result := TWAGModel.Create     (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdLG          : Result := TLGModel.Create      (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdLGPi        : Result := TLGModel.Create      (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
      gdPoisson     : Result := TPoissonModel.Create (0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
      gdEqualInput  : Result := TPoissonModel.Create (0, doesContain(gdInvar), True, MAI.MyNoOfGammaCat);
    else
      begin
        if MAI.MyTreePack.DoesContain(ttModelTest) then // just put in a fake Model
        begin
           if MAI.MyDistPack.DoesContain(gdOneNuc) then
           begin
             Result := TTN93Model.Create(0, doesContain(gdInvar), MAI.MyNoOfGammaCat);
             TTN93Model(Result).SetParamsFromSeqs(MAI.MySeqStrings);
           end
           else
             Result := TJTTModel.Create(0, doesContain(gdInvar), False, MAI.MyNoOfGammaCat);
        end
        else
          RaiseErrorMessage(HC_Unexpected_Error, 'Programming bug, as no applicable model is transmitted');
      end
    end;
    if Assigned(aCheckCancel) then
    begin
      origCheckCancel := Result.CheckCancel;
      Result.CheckCancel := aCheckCancel;
    end;
    Result.SetParamsFromSeqs(MAI.MySeqStrings);
    if Assigned(aCheckCancel) then
      Result.CheckCancel := origCheckCancel;
    Result.NoOfThreads := MAI.MyNumThreadsToUse; // GS - because the user sets the value
  end;
end;

end.
