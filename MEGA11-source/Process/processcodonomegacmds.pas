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

unit ProcessCodonOmegaCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  FileUtil,
  Classes,
  MAnalysisInfo,
  MDistPack,
  MegaConsts,
  MProcessPack,
  MegaAnalysisPrefStrings
  {$IFDEF VISUAL_BUILD}
  , manalysisprefdlg
  {$ENDIF}
  ;
var
  SaveLocation: AnsiString;
  {$IFDEF VISUAL_BUILD}
  SLACUrl: String;
  {$ENDIF}

procedure ProcessCodonOmegaCommand(UsrOperation: TDistTreeDlgOption; AOption: String; ProcessPack : TProcessPack);
procedure ExecuteCodonOmegaCommand(MAI: TAnalysisInfo);
function TaxaNamesReplace(Source: String; TaxaNameList: TStringList): String;

function GetExportType: TExportType;

implementation

uses
  LCLIntf, LCLType, MLegendGenerator, SysUtils,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  MegaUtils, MegaErrUtils, StringUtils, fphttpclient,
  {$IFDEF VISUAL_BUILD}
  Mega_Main, MWriteOutputDlg,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  KeywordConsts, MNucDist, MD_InputSeqData, MRuntimeProgressDlg, StrUtils,
  mseqexportoptions, Menus;

function TaxaNamesReplace(Source: String; TaxaNameList: TStringList): String;
var
  k: Integer;
  OffSet: Integer;
  NameTerminators: Set of Char;
begin
  NameTerminators := [',', ':', '(', ')', ';', ' '];
  Result := LowerCase(trim(Source));
  for k := 0 to TaxaNameList.Count - 1 do
  begin
    if NumOccurences(Result, TaxaNameList[k], True) > 1 then
    begin
      Offset := PosEx(TaxaNameList[k], Result, 1);
      if Result[Offset + Length(TaxaNameList[k])] in NameTerminators then
      begin
        Result := Copy(Result, 1, Offset - 1) + 'TX' + IntToStr(k) + Copy(Result, Offset + Length(TaxaNameList[k]), Length(Result));
      end;
      while Offset <> 0 do
      begin
        Offset := PosEx(TaxaNameList[k], Result, Offset + length(TaxaNameList[k]));
        if Result[Offset + Length(TaxaNameList[k])] in NameTerminators then
        begin
          Result := Copy(Result, 1, Offset - 1) + 'TX' + IntToStr(k) + Copy(Result, Offset + Length(TaxaNameList[k]), Length(Result));
        end;
      end;
    end
    else
    begin
      Result := StringReplace(Result, TaxaNameList[k], 'TX' + IntToStr(k), [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
end; // end TaxaNamesReplace

procedure ExecuteCodonOmegaCommand(MAI: TAnalysisInfo);
var
  TempDir, NexOut: String;
  NexusList: TStringList = nil;
  FileNameOffset: String;
  StartsForIncludedCodons: TIntArray;
  aClient: TFPHTTPClient = nil;
  PostData: TStringList = nil;
  GeneticCodeIndex: Integer;
  ResponseStream: TStringStream = nil;
  S : String;
  index: Integer;
  TempString: String;
begin
 try
    MAI.ARP.WriteAnalysisOptionsToStdOut;
    MAI.ARP.AddRunStatusInfo('Status', 'Preparing data');
    MAI.ARP.Show;
    MAI.MyMappedData := TList.Create;
    D_InputSeqData.ARP := MAI.ARP;
    StartsForIncludedCodons := D_InputSeqData.PrepareDataForCodonOmegaAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MAI.MyUsedOtuInfos,
                           MAI.MyNoOfSeqs, MAI.MyNoOfSites, MAI.MyLabelsUsed, MAI.SiteCoverage);
     {$IFNDEF VISUAL_BUILD}
     D_MegaMain.AnalysisSummary.NumTaxa := MAI.MyNoOfSeqs;
     {$ELSE}
     MAI.AnalysisSummary.NumTaxa := MAI.MyNoOfSeqs;
     {$ENDIF}
    if MAI.NoOfSites <= 0 then
    begin
      ShowMessage('All Sites were removed, try partial deletion rather than complete.');
      Exit;
    end;
    if MAI.MyNoOfSeqs < 3 then
    begin
      ShowMessage('At least three sequences are needed for calculating codon-by-codon selection.');
      Exit;
    end;
    TempDir := GetTempDir(False);
    FileNameOffset := NextAvailableFileNumber(TempDir + 'HyPhyNexus.nex');
    NexOut := TempDir + 'HyPhyNexus' + FileNameOffset + '.nex';
    NexusList := TStringList.Create;
    D_InputSeqData.WriteCodonMapsToNexus(NexusList, MAI.MyMappedData, MAI.NoOfSites, 'DNA');
    NexusList.SaveToFile(NexOut);
    NexusList.Free;
    {$IFDEF VISUAL_BUILD}
    if AnalysisPrefDlg.FPicklistComboBx.ItemIndex < 0 then
       GeneticCodeIndex := 0
    else
        GeneticCodeIndex := AnalysisPrefDlg.FPickListComboBx.ItemIndex;
    {$ENDIF}
    if not FileExists(NexOut) then
      raise Exception.Create('Cache file missing: ' + NexOut);
    PostData:= TStringList.Create;
    PostData.Clear;
    aClient := TFPHTTPClient.Create(nil);
    ResponseStream := TStringStream.Create('');
    PostData.Add(NexOut);
    PostData.Add('gencodeid='+IntToStr(GeneticCodeIndex));
    try
      aClient.FileFormPost(DatamonkeyUrl, PostData, 'files', NexOut, ResponseStream);
    except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
   end;
    S := ResponseStream.DataString;
    index := S.IndexOf('upload_redirect_path')+28;
    TempString := EmptyStr;
    while index < ResponseStream.DataString.Length-2 do
    begin
      TempString := TempString + S.Chars[index];
      inc(index);
    end;
      {$IFDEF VISUAL_BUILD}
      SLACUrl := DatamonkeyUrl+TempString;
      MegaForm.SLACUrl:=SLACUrl;
      MegaForm.SLACJobID:=TempString;
      MegaForm.OpenSLACWindow;
      MegaForm.UpdateJobQueueList(TempString);
      {$ENDIF}
      MAI.ARP.Hide;
  finally
    if FileExists(NexOut) then
    begin
      try
        DeleteFile(NexOut);
      except

      end;
    end;
    if Assigned(aClient) then
      aClient.Free;
    if Assigned(PostData) then
      PostData.Free;
    if Assigned(MAI) then
    begin
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
    SetLength(StartsForIncludedCodons, 0);
  end;
end;

function GetExportType: TExportType;
begin
  {$IFDEF VISUAL_BUILD}
  Result := PromptUserWriteOutput(SaveLocation, True);
  {$ELSE}
  Result := EXcsvSave;
  {$ENDIF}
end;

procedure ProcessCodonOmegaCommand(UsrOperation: TDistTreeDlgOption; AOption: String; ProcessPack : TProcessPack);
var
  MAI: TAnalysisInfo = nil;
  ARP : TRuntimeProgress = nil;
  {$IFNDEF VISUAL_BUILD}
  i: Integer;
  {$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  D_MegaMain.AnalysisSummary.DataType := snCoding;
  for i := 0 to ProcessPack.TextualSettingsList.Count - 1 do
    D_MegaMain.AnalysisSummary.AnalysisOptions.Add(ProcessPack.TextualSettingsList[i]);
  {$ENDIF}

  try
    MAI := TAnalysisInfo.Create;
    ARP := TRuntimeProgress.Create(Application);
    ARP.HasCmdLineOutput := True;
    {$IFDEF VISUAL_BUILD}
    ARP.DataFileName  :=  MegaForm.DataTitle;
    ARP.DataTitle     :=  MegaForm.DataTitle;
    MAI.DataFilename := MegaForm.DataFilename;
    {$ELSE}
    ARP.DataFileName  :=  D_MegaMain.DataFileName;
    ARP.DataTitle     :=  D_MegaMain.DataTitle;
    MAI.DataFilename := D_MegaMain.DataFilename;
    {$ENDIF}

    MAI.AnalysisSummary.DataType := snCoding;
    MAI.MyProcessPack := ProcessPack;
    MAI.InitialUsrOperation := UsrOperation;
    MAI.ARP := ARP;
    ARP := nil;
    if not MAI.GetAnalysisOptions(UsrOperation) then   // used to be GetTreeOptions
      Exit;
    ExecuteCodonOmegaCommand(MAI);
  except
    on E: Exception do
      ShowErrorMessage(E);
  end;
end;


end.
