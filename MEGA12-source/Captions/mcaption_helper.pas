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

unit mcaption_helper;

{$mode ObjFPC}{$H+}

interface

uses
 {$IFDEF VISUAL_BUILD}mtreeinfoform, mtreebox, MTreeViewForm,{$ENDIF}
  Classes, SysUtils, MLTreeAnalyzer, MAnalysisInfo, MLegendGenerator, mltree,
  AppLinker;

type

  { TCaptionHelper }

  TCaptionHelper = class(TObject)
    private
      FAncStatesDispSetting: String;
      {$IFDEF VISUAL_BUILD}
      FTree: TTreeBox;
      FInfoDisplay: TTreeInfoForm;
      FTreeViewer: TTreeViewForm;
      {$ENDIF}
      FAnalysisInfo: TAnalysisInfo;
      FMLTreeAnalyzer: TMLTreeAnalyzer;
      FMolecularClockTest: Double;
      FShowDivergenceTimes: Boolean;
      FShowHeightErrBars: Boolean;
      FUsingExtended_IUPAC_Codes: Boolean;

      procedure AddDataNeededForMegacc(var FigureGenerator: TLegendGenerator);
    public
      RI: Double;
      CI: Double;
      RCI: Double;
      iRI: Double;
      iCI: Double;
      iRCI: Double;
      constructor Create;

      procedure UpdateFigureGenerator(var FigureGenerator: TLegendGenerator);

      property MLAnalyzer: TMLTreeAnalyzer read FMLTreeAnalyzer write FMLTreeAnalyzer;
      property MAI: TAnalysisInfo read FAnalysisInfo write FAnalysisInfo;
      property ShowDivergenceTimes: Boolean read FShowDivergenceTimes write FShowDivergenceTimes;
      property ShowHeightErrBar: Boolean read FShowHeightErrBars write FShowHeightErrBars;
      property MolecularClockTest: Double read FMolecularClockTest write FMolecularClockTest;
      property AncStatesDispSetting: String read FAncStatesDispSetting write FAncStatesDispSetting;
      property UsingExtended_IUPAC_Codes: Boolean read FUsingExtended_IUPAC_Codes write FUsingExtended_IUPAC_Codes;
      {$IFDEF VISUAL_BUILD}
      property Tree: TTreeBox read FTree write FTree;
      property InfoDisplay: TTreeInfoForm read FInfoDisplay write FInfoDisplay;
      property TreeViewer: TTreeViewForm read FTreeViewer write FTreeViewer;
      {$ENDIF}
  end;

implementation

uses
  mdistpack, MegaConsts, mreltime_caption, ml_tree_caption, mtreepack,
  MegaUtils, mdefault_tree_caption, mancestral_states_caption, mp_tree_caption;

{ TCaptionHelper }

procedure TCaptionHelper.AddDataNeededForMegacc(var FigureGenerator: TLegendGenerator);
begin
  FigureGenerator.AssignData('SiteIndexForCaption', '1');
end;

constructor TCaptionHelper.Create;
begin
  RI := -1;
  CI := -1;
  RCI := -1;
  iRI := -1;
  iCI := -1;
  iRCI := -1;
  FShowHeightErrBars := False;
  FShowDivergenceTimes := False;
  FMLTreeAnalyzer := nil;
  FAnalysisInfo := nil;
  FMolecularClockTest := -1;
  FUsingExtended_IUPAC_Codes := False;
  FAncStatesDispSetting := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  FTree := nil;
  FInfoDisplay := nil;
  {$ENDIF}
end;

procedure TCaptionHelper.UpdateFigureGenerator(var FigureGenerator: TLegendGenerator);
var
  ModelInfo : TModelInfo = nil;
begin
  try
    try
      if (MLAnalyzer <> nil) and (not MAI.MyDistPack.DoesContain(gdInvar)) then
        FigureGenerator.AssignData('PropOfInvariant', 'N/A');
      if (MLAnalyzer <> nil) and (not MAI.MyDistPack.DoesContain(gdGamma)) then
        FigureGenerator.AssignData('GammaPara', 'N/A');

      {$IFDEF VISUAL_BUILD}
      if FTree.isAncState then
        FigureGenerator.AssignData('AncestralStatesCaption', GenerateAncestralStatesCaption(MAI, FAncStatesDispSetting, FUsingExtended_IUPAC_Codes, FTree));
      if FTree.TreeBoxType = tttReltimeTree then
      begin
        FigureGenerator.ClearCitations;
        if FigureGenerator.HasData('DefaultTreeCaption') then
          FigureGenerator.AssignData('DefaultTreeCaption', GenerateReltimeCaption(MAI, FTree, MLAnalyzer))
        else if FigureGenerator.HasData('MLTreeCaption') then
          FigureGenerator.AssignData('MLTreeCaption', GenerateReltimeCaption(MAI, FTree, MLAnalyzer))
        else
          FigureGenerator.AssignData('ReltimeCaption', GenerateReltimeCaption(MAI, FTree, MLAnalyzer));
      end
      else if Assigned(MLAnalyzer) then
      begin
        FigureGenerator.ClearCitations;
        FigureGenerator.AssignData('MLTreeCaption', GenerateMLTreeCaption(MAI, MLAnalyzer, FTree))
      end
      else if Assigned(MAI) and (MAI.MyUsrOperation in [dtdoLbsAnalyzeTree, dtdoLbsInference, dtdoLbsTiming]) then
      begin
        FigureGenerator.ClearCitations;
        FigureGenerator.AssignData('MLTreeCaption', GenerateMLTreeCaption(MAI, nil, Tree))
      end
      else if Assigned(MAI) and (MAI.MyUsrOperation in [dtdoMPComputeUserTreeBLens, dtdoMPTree]) then
      begin
        FigureGenerator.ClearCitations;
        FigureGenerator.AssignData('MpTreeCaption', GenerateMpTreeCaption(MAI, TreeViewer))
      end
      else if Assigned(TreeViewer) and Assigned(FTree) and Assigned(MAI) then
      begin
        FigureGenerator.ClearCitations;
        FigureGenerator.AssignData('DefaultTreeCaption', GenerateDefaultTreeCaption(MAI, TreeViewer, FTree));
      end;

      if (FTree.TreeBoxType = tttReltimeTree) and MAI.IsSubsampling then
      begin
        FigureGenerator.AssignData('IsBlensTimingMethod', 'True');
        if MAI.MyUsrOperation = dtdoLbsInference then
          FigureGenerator.AssignData('InferTree', 'True')
        else
          FigureGenerator.AssignData('InferTree', 'False');
      end;
      {$ELSE}
      if MAI.MyUsrOperation in [dtdoMLInferAncSeq, dtdoMLInferAncSeqMyPeg, dtdoMPInferAncSeq, dtdoMPInferAncSeqMyPeg, dtdoMPPredictLivingSeq] then
        FigureGenerator.AssignData('AncestralStatesCaption', GenerateAncestralStatesCaption(MAI, SHOW_ALL, True))
      else if MAI.MyUsrOperation in [dtdoRelTimeML, dtdoRelTimeBLens, dtdoRelTimeLS, dtdoLbsTiming, dtdoRtdtBlens, dtdoRtdtLS, dtdoRtdtML] then
      begin
        FigureGenerator.AssignData('RunMode', 'commandLine');
        FigureGenerator.AssignData('ReltimeCaption', GenerateReltimeCaption(MAI,{$IFDEF VISUAL_BUILD} nil,{$ENDIF} MLAnalyzer));
      end
      else if Assigned(MLAnalyzer) then
        FigureGenerator.AssignData('MLTreeCaption', GenerateMLTreeCaption(MAI, MLAnalyzer {$IFDEF VISUAL_BUILD}, nil{$ENDIF}))
      else if Assigned(MAI) and (MAI.MyUsrOperation = dtdoLbsAnalyzeTree) then
        FigureGenerator.AssignData('MLTreeCaption', GenerateMLTreeCaption(MAI, nil {$IFDEF VISUAL_BUILD}, nil{$ENDIF}))
      else if Assigned(MAI) and (MAI.MyUsrOperation in [dtdoMPComputeUserTreeBLens, dtdoMPTree]) then
        FigureGenerator.AssignData('MpTreeCaption', GenerateMpTreeCaptionNonVisual(MAI, RI, CI, RCI, IRI, ICI, IRCI))
      else
        FigureGenerator.AssignData('DefaultTreeCaption', GenerateDefaultTreeCaption(MAI{$IFDEF VISUAL_BUILD}, TreeViewer, FTree{$ENDIF}));
      {$ENDIF}

      if Assigned(MLAnalyzer) then
      begin
        FigureGenerator.AssignData('LogLikelihoodCE', Format('%.2n', [MLAnalyzer.LogLikelihood]));
        ModelInfo := TModelInfo.Create;    //Used for models of protein names
        MLAnalyzer.GetModelInfo(ModelInfo); //for whatever reason, this doesn't work even though they are accessed with properties declared published
        FigureGenerator.AssignData('FullModelName', ModelInfo.FullName);   //Binding ModelInfo with published properties doesn't seem to work

        if MAI.MyDistPack.DoesContain(gdInvar) then
          FigureGenerator.AssignData('PropOfInvariant', Format('%.2f', [ModelInfo.Invar*100]));

        if MAI.MyDistPack.DoesContain(gdGamma) then
        begin
          FigureGenerator.AssignData('GammaPara', Format('%.4f', [ModelInfo.Gamma]));
          FigureGenerator.AssignData('NoOfGCats', IntToStr(ModelInfo.NoOfRates));
        end;

        if MAI.MyTreePack.DoesContain(ttClock) or MAI.MyTreePack.DoesContain(ttClockTest) then
          FigureGenerator.AssignData('MolecularClockTest', Format('%.4f', [MolecularClockTest]));

        if MAI.ClockType = ctLocal then
        begin
          if MAI.MyUsrOperation in [dtdoRelTimeML, dtdoRtdtML] then
            FigureGenerator.AssignData('Operation', 'dtdoRelTimeML')
          else if MAI.MyUsrOperation in [dtdoRelTimeBLens, dtdoRtdtBlens] then
            FigureGenerator.AssignData('Operation', 'dtdoRelTimeBLens')
          else if MAI.MyUsrOperation in [dtdoRelTimeLS, dtdoRtdtLS] then
            FigureGenerator.AssignData('Operation', 'dtdoRelTimeLS')
          else if MAI.MyUsrOperation = dtdoGeneDupInference then
            FigureGenerator.AssignData('Operation', 'dtdoGeneDupInference');

          if MAI.ClockLevel = clOneStdErr then
            FigureGenerator.AssignData('ClockLevel', IntToStr(1))
          else if MAI.ClockLevel = clTwoStdErr then
            FigureGenerator.AssignData('ClockLevel', IntToStr(2))
          else if MAI.ClockLevel = clThreeStdErr then
            FigureGenerator.AssignData('ClockLevel', IntToStr(3))
          else
            FigureGenerator.AssignData('ClockLevel', 'Unknown');
          if MAI.MergeRates then
            FigureGenerator.AssignData('MergeRates', 'True')
          else
            FigureGenerator.AssignData('MergeRates', 'False');

          if ShowDivergenceTimes then
            FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True')
          else
            FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'False');
          if ShowHeightErrBar then
            FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'True')
          else
            FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'False');
        end;
      end
      else
      begin
        if IsReltimeBLensAnalysis(MAI.MyUsrOperation) then
          FigureGenerator.AssignData('Operation', 'dtdoRelTimeBLens')
        else if (MAI.MyUsrOperation = dtdoRelTimeLS) or (MAI.MyUsrOperation = dtdoRtdtLS) then
        begin
          FigureGenerator.AssignData('GammaPara', 'N/A');
          FigureGenerator.AssignData('Operation', 'dtdoRelTimeLS');
        end
        else if MAI.MyUsrOperation = dtdoOLSComputeUserTreeBLens then
          FigureGenerator.AssignData('Operation', 'dtdoOLSComputeUserTreeBLens')
        else if MAI.MyUsrOperation = dtdoMPComputeUserTreeBLens then
          FigureGenerator.AssignData('Operation', 'dtdoMPComputeUserTreeBLens');

        if (MAI.MyDistPack <> nil) then
        begin
          if MAI.MyDistPack.DistModel = gdNoOfDiff  then
            FigureGenerator.AssignData('FullModelName', 'number of differences')
          else if MAI.MyDistPack.DistModel = gdPropDist  then
            FigureGenerator.AssignData('FullModelName', 'p-distance');
        end;
      end;

      if (MAI.MyTreePack <> nil) and (MAI.MyTreePack.DoesContain(ttML)) then
      begin
        if MAI.MyTreePack.DoesContain(ttInferTree) then
        begin
          if MAI.isAminoAcid then
          begin
            FigureGenerator.AssignData('InitialMlTreeModel', 'the Poisson model');
            FigureGenerator.AssignData('DataType', 'protein');
          end
          else
          begin
            FigureGenerator.AssignData('InitialMlTreeModel', 'the Jukes-Cantor model');
            FigureGenerator.AssignData('InitialTreeModelCitation', 'Jukes_and_Cantor_1969');
            FigureGenerator.AssignData('DataType', 'nucleotide');
          end;
        end
        else
          FigureGenerator.AssignData('AnalyzeUserTree', 'True');
      end;
      if MAI.MyTreePack.DoesContain(ttUserTree) and (not MAI.MyTreePack.DoesContain(ttInferTree)) then
        FigureGenerator.AssignData('AnalyzeUserTree', 'True');

      {$IFDEF VISUAL_BUILD}
      FigureGenerator.BindData(InfoDisplay);
      if Assigned(FTree) then
        FigureGenerator.BindData(FTree);
      {$ENDIF}

      FigureGenerator.BindData(MAI);
      if Assigned(MAI.MyTreePack) and (not IsReltimeBLensAnalysis(MAI.MyUsrOperation)) then
        FigureGenerator.BindData(MAI.MyTreePack);
      if Assigned(MAI.MyDistPack) then
        FigureGenerator.BindData(MAI.MyDistPack);
      {$IFNDEF VISUAL_BUILD}
      AddDataNeededForMegacc(FigureGenerator);
      {$ENDIF}
    except
      on E:Exception do
      begin
        if IsSessionTest then
          Halt(MEGA_ERROR_EXIT_CODE)
        else
          raise E;
      end;
    end;
  finally
    if Assigned(ModelInfo) then
      ModelInfo.Free;
  end;
end;

end.

