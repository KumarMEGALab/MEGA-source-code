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

unit MClustalW;

interface

uses
  {$IFDEF VISUAL_BUILD}
  MAlnProgress,
  {$ENDIF}
  Classes, Controls, Dialogs, SysUtils, MegaConsts, MTreeData, MAlnThread,
  fpjson, jsonparser, syncobjs;

type
  TDNAMatrix = MAlnThread.TDNAMatrix;
  TProteinMatrix = MAlnThread.TProteinMatrix;

  { TCustomClustalW }

  TCustomClustalW = class(TComponent)
  private
    FAutocDNA: Boolean;
    FDNAMatrix: TDNAMatrix;
    FProteinMatrix: TProteinMatrix;

    FDNAPWGapOpenPenalty: double;
    FDNAPWGapExtendPenalty: double;
    FDNAGapOpenPenalty: double;
    FDNAGapExtendPenalty: double;
    FProteinPWGapOpenPenalty: double;
    FProteinPWGapExtendPenalty: double;
    FProteinGapOpenPenalty: double;
    FProteinGapExtendPenalty: double;

    FDivergentCutoff: integer;
    FTransitionWeight: double;
    FUseNegativeMatrix: boolean;

    FResidueSpecificPenalty: boolean;
    FHydrophilicPenalty: boolean;
    FEndGapSeparation: boolean;
    FGapSeparationDistance: integer;

    FIsDNA: boolean;
    FResetGaps: boolean;
    FUnTranslate: boolean;

    FSequences: TStringList;
    FSeqNames:  TStringList;
    XSites: AnsiString;

    ClustalWThread: TClustalWThread;
    {$IFDEF VISUAL_BUILD}
    ProgressDlg: TClustalWProgressDlg;
    {$ENDIF}
    FOnTerminate: TNotifyEvent;
    FDone: boolean;
    FGuideTreeFile: AnsiString;

    procedure SetSequences(sl: TStringList);
    procedure SetSeqNames(sl: TStringList);
    function GetShowProgress: boolean;
    procedure SetShowProgress(value: boolean);
    procedure OnClustalWTerminate(Sender: TObject);
    procedure GetSessionProperties(var json: TJSONObject);
    procedure SetSessionProperties(const json: TJsonObject);
  public
    function GetSettingsAsJson: TJSONObject;
    procedure SetDefaults;
    property AutocDNA: Boolean read FAutocDNA write FAutocDNA;
    property DNAMatrix: TDNAMatrix read FDNAMatrix write FDNAMatrix;
    property ProteinMatrix: TProteinMatrix read FProteinMatrix write FProteinMatrix;
    property DNAPWGapOpenPenalty: double read FDNAPWGapOpenPenalty write FDNAPWGapOpenPenalty;
    property DNAPWGapExtendPenalty: double read FDNAPWGapExtendPenalty write FDNAPWGapExtendPenalty;
    property DNAGapOpenPenalty: double read FDNAGapOpenPenalty write FDNAGapOpenPenalty;
    property DNAGapExtendPenalty: double read FDNAGapExtendPenalty write FDNAGapExtendPenalty;
    property ProteinPWGapOpenPenalty: double read FProteinPWGapOpenPenalty write FProteinPWGapOpenPenalty;
    property ProteinPWGapExtendPenalty: double read FProteinPWGapExtendPenalty write FProteinPWGapExtendPenalty;
    property ProteinGapOpenPenalty: double read FProteinGapOpenPenalty write FProteinGapOpenPenalty;
    property ProteinGapExtendPenalty: double read FProteinGapExtendPenalty write FProteinGapExtendPenalty;
    property DivergentCutoff: integer read FDivergentCutoff write FDivergentCutoff;
    property TransitionWeight: double read FTransitionWeight write FTransitionWeight;
    property UseNegativeMatrix: boolean read FUseNegativeMatrix write FUseNegativeMatrix;
    property ResidueSpecificPenalty: boolean read FResidueSpecificPenalty write FResidueSpecificPenalty;
    property HydrophilicPenalty: boolean read FHydrophilicPenalty write FHydrophilicPenalty;
    property EndGapSeparation: boolean read FEndGapSeparation write FEndGapSeparation;
    property GapSeparationDistance: integer read FGapSeparationDistance write FGapSeparationDistance;
    property ResetGaps: boolean read FResetGaps write FResetGaps;

    property Sequences: TStringList read FSequences write SetSequences;
    property SeqNames: TStringList read FSeqNames write SetSeqNames;
    property IsDNA: boolean read FIsDNA write FIsDNA;
    property RequestUntranslate: boolean read FUnTranslate write FUnTranslate;

    property ShowProgress: boolean read GetShowProgress write SetShowProgress;
    property Done: boolean read FDone;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;

    property GuideTreeFile: AnsiString read FGuideTreeFile write FGuideTreeFile;

    procedure Align;
    procedure CheckGuideTree;

    function SaveSessionProperties(filename: String): Boolean;
    function LoadSessionProperties(filename: String): Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

  TClustalW = class(TCustomClustalW)
  private
  public
  published
    property DNAMatrix;
    property ProteinMatrix;
    property DNAPWGapOpenPenalty;
    property DNAPWGapExtendPenalty;
    property DNAGapOpenPenalty;
    property DNAGapExtendPenalty;
    property ProteinPWGapOpenPenalty;
    property ProteinPWGapExtendPenalty;
    property ProteinGapOpenPenalty;
    property ProteinGapExtendPenalty;
    property DivergentCutoff;
    property TransitionWeight;
    property UseNegativeMatrix;
    property ResidueSpecificPenalty;
    property HydrophilicPenalty;
    property EndGapSeparation;
    property GapSeparationDistance;
    property ResetGaps;
    property ShowProgress;
    property OnTerminate;
    property GuideTreeFile;
    property RequestUntranslate;
  end;

function ProteinMatrixToString(aMatrix: TProteinMatrix): String;
function StringToProteinMatrix(aStr: String): TProteinMatrix;
function DnaMatrixToString(aMatrix: TDNAMatrix): String;
function StringToDnaMatrix(aStr: String): TDNAMatrix;
function BoolToOnOffString(aBool: Boolean): String;
function OnOffStringToBool(aStr: String): Boolean;

procedure Register;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MTreeInputForm, MegaPrivateFiles,
  {$ELSE}
  MTreeList, MOtuInfo,
  {$ENDIF}
  MegaUtils, MegaAnalysisPrefStrings;

const
  DNABases : set of char = ['A','T','C','G','U','R','Y','M','K','S','W','B','V','D','H'];
  AABases  : set of char = ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y'];

function ProteinMatrixToString(aMatrix: TProteinMatrix): String;
begin
  case aMatrix of
    blosum: Result := 'BLOSUM';
    pam: Result := 'PAM';
    gonnet: Result := 'Gonnet';
    identity: Result := 'Identity';
  end;
end;

function StringToProteinMatrix(aStr: String): TProteinMatrix;
begin
  if SameText(aStr, 'blosum') then
    Result := blosum
  else if SameText(aStr, 'pam') then
    Result := pam
  else if SameText(aStr, 'gonnet') then
    Result := gonnet
  else
    Result := identity;
end;

function DnaMatrixToString(aMatrix: TDNAMatrix): String;
begin
  case aMatrix of
    iub: Result := 'IUB';
    clustalw: Result := 'ClustalW (1,6)';
  end;
end;

function StringToDnaMatrix(aStr: String): TDNAMatrix;
begin
  if SameText(aStr, 'IUB') then
    Result := iub
  else
    Result := clustalw;
end;

function BoolToOnOffString(aBool: Boolean): String;
begin
  if aBool then
    Result := 'ON'
  else
    Result := 'OFF';
end;

function OnOffStringToBool(aStr: String): Boolean;
begin
  if SameText(aStr, 'ON') then
    Result := True
  else
    Result := False;
end;

procedure Register;
begin
  RegisterComponents('Tamura Tools', [TClustalW]);
end;

constructor TCustomClustalW.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF VISUAL_BUILD}
  AlignmentSessionPropsCI := TCriticalSection.Create;
  {$ENDIF}
  FSequences := TStringList.Create;
  FSeqNames  := TStringList.Create;
  SetDefaults;
  Self.SetSubComponent(True);
end;

destructor TCustomClustalW.Destroy;
var
  filename: String;
begin
  if Assigned(FSequences) then
    FSequences.Free;
  if Assigned(FSeqNames) then
    FSeqNames.Free;

  if ClustalWThread <> nil then
    ClustalWThread.Free;
  {$IFDEF VISUAL_BUILD}
  if ProgressDlg <> nil then
    ProgressDlg.Free;

  try
    filename := GetPrivateOutputFile(sfClustalOptions);
    SaveSessionProperties(filename);
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
       ShowMessage('Error when saving clustal session properties: ' + E.Message);
      {$ENDIF}
    end;
  end;
  if Assigned(AlignmentSessionPropsCI) then
    AlignmentSessionPropsCI.Free;
  {$ENDIF}

  inherited;
end;

procedure TCustomClustalW.SetSequences(sl: TStringList);
var
  i: integer;
begin
  if sl.Count = 0 then exit;

  FSequences.Clear;
  for i := 0 to sl.Count-1 do
    FSequences.Add(sl[i]);
end;

procedure TCustomClustalW.SetSeqNames(sl: TStringList);
var
  i: integer;
begin
  if sl.Count = 0 then exit;

  FSeqNames.Clear;
  for i := 0 to sl.Count-1 do
    FSeqNames.Add(sl[i]);
end;

function TCustomClustalW.GetShowProgress: boolean;
begin
  Result := True;
{$IFDEF VISUAL_BUILD}
  result := ProgressDlg <> nil;
  {$ENDIF}
end;

procedure TCustomClustalW.SetShowProgress(value: boolean);
begin
  {$IFDEF VISUAL_BUILD}
  if value = ShowProgress then exit;
  if value then
    ProgressDlg := TClustalWProgressDlg.Create(Self)
  else
  begin
    ProgressDlg.Free;
    ProgressDlg := nil;
  end;
  {$ENDIF}
end;

procedure TCustomClustalW.Align;
var
  i,j: integer;
  str: AnsiString;
begin
  if Sequences = nil then exit;

  try
    if ResetGaps then
      for i := 0 to Sequences.Count-1 do  // strip out all gaps already in the alignment.
      begin
        str := Sequences[i];
        while Pos('-', str) > 0 do
          Delete(str, Pos('-', str), 1);
        Sequences[i] := str;
      end;

    XSites := '';  // sites substituted with X (ambig, unknown, etc)
    for i := 0 to Sequences.Count-1 do
    begin
      str := Sequences[i];
      for j := 1 to Length(str) do
        if IsDNA then
        begin
          if not (upcase(str[j]) in DNABases) then
          begin
            XSites := XSites +str[j];
            str[j] := 'N';
          end
        end
        else if not (upcase(str[j]) in AABases) then // Substitute any non AA bases with X.
        begin
          XSites := XSites +str[j];
          str[j] := 'X';
        end;
      Sequences[i] := str;
    end;

    if ClustalWThread <> nil then ClustalWThread.Free;
    ClustalWThread := TClustalWThread.Create;

    ClustalWThread.DNAMatrix                 := DNAMatrix;
    ClustalWThread.ProteinMatrix             := ProteinMatrix;
    ClustalWThread.DNAPWGapOpenPenalty       := DNAPWGapOpenPenalty;
    ClustalWThread.DNAPWGapExtendPenalty     := DNAPWGapExtendPenalty;
    ClustalWThread.DNAGapOpenPenalty         := DNAGapOpenPenalty;
    ClustalWThread.DNAGapExtendPenalty       := DNAGapExtendPenalty;
    ClustalWThread.ProteinPWGapOpenPenalty   := ProteinPWGapOpenPenalty;
    ClustalWThread.ProteinPWGapExtendPenalty := ProteinPWGapExtendPenalty;
    ClustalWThread.ProteinGapOpenPenalty     := ProteinGapOpenPenalty;
    ClustalWThread.ProteinGapExtendPenalty   := ProteinGapExtendPenalty;
    ClustalWThread.DivergentCutoff           := DivergentCutoff;
    ClustalWThread.TransitionWeight          := TransitionWeight;
    ClustalWThread.UseNegativeMatrix         := UseNegativeMatrix;
    ClustalWThread.ResidueSpecificPenalty    := ResidueSpecificPenalty;
    ClustalWThread.HydrophilicPenalty        := HydrophilicPenalty;
    ClustalWThread.EndGapSeparation          := EndGapSeparation;
    ClustalWThread.GapSeparationDistance     := GapSeparationDistance;

    ClustalWThread.SeqList  := Sequences;
    ClustalWThread.SeqNames := SeqNames;
    {$IFDEF VISUAL_BUILD}
    ClustalWThread.ProgressDlg := ProgressDlg;
    {$ENDIF}
    ClustalWThread.OnTerminate := {$IFDEF FPC}@{$ENDIF}OnClustalWTerminate;
    ClustalWThread.IsDNA := IsDNA;

    ClustalWThread.CheckGuideTree := {$IFDEF FPC}@{$ENDIF}CheckGuideTree;

    ClustalWThread.Start;
    {$IFNDEF VISUAL_BUILD}
    ClustalWThread.WaitFor;
    {$ENDIF}
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred: ' + E.Message);
  end;
end;

procedure TCustomClustalW.CheckGuideTree;
var
  {$IFDEF VISUAL_BUILD}
  ATreeDlg: TTreeInputForm = nil;
  {$ELSE}
  tempList: TStringList = nil;
  AExpNameStrList: TStringList = nil;
  i: integer;
  ANamesList: TStringList = nil;
  AInputTree: TTreeList = nil;
  MyUsedOtuInfos : TList = nil;
  IsPerfectTree: Boolean;
  {$ENDIF}
  tree: TTreeData = nil;
begin
  {$IFDEF UNIX}
  raise Exception.Create('clustal guide tree not supported on *nix systems');
  {$ENDIF}
  ClustalWThread.Suspend;

  try
    if FGuideTreeFile = EmptyStr then Exit;

    tree := TTreeData.Create(Sequences.Count, true, false, false);

    if not IsPerfectTreeFile(FGuideTreeFile) then
    begin
	  {$IFDEF VISUAL_BUILD}
      MessageDlg('The contents of the tree file do not conform to the Newick Format standard.'+ LineEnding +'MEGA will not be able to use this tree file.'+#13+#10+'ClustalW Alignment will continue without a user specified tree.', mtError, [mbOK], 0);
  	{$ELSE}
	    Raise Exception.Create('The contents of the tree file do not conform to the Newick Format standard, MEGA can not continue until this is fixed');
	  {$ENDIF}
    end
    else
    begin
      {$IFDEF VISUAL_BUILD}
      ATreeDlg := TTreeInputForm.Create(nil);
      ATreeDlg.SetActiveDataNames(SeqNames);
      ATreeDlg.SetTreeFromFile(FGuideTreeFile);
      ATreeDlg.InitializeActiveDataAndTreeNameMatches;
      if ATreeDlg.DoesTreeHaveAllActiveNames then
      begin
        ATreeDlg.RebuildTreeNames;
        ATreeDlg.PruneUnwantedTreeNodesAndNames;
        ATreeDlg.GetTreeData(tree);
        ClustalWThread.SetTreeData(tree);
      end
      else
      begin
        if (MessageDlg('The sequence names in the Alignment Editor do not match the names in the Guide Tree you have supplied.'+ LineEnding +'Would you like to match the Alignment Names to the Guide Tree?  This is necessary to use the guide tree.', mtError, [mbYes, mbNo], 0) = mrYes) then
        begin
          if ATreeDlg.ShowModal = mrOK then
          begin
            ATreeDlg.RebuildTreeNames;
            ATreeDlg.PruneUnwantedTreeNodesAndNames;
            ATreedlg.GetTreeData(tree);
            ClustalWThread.SetTreeData(Tree);
          end;
        end;
      end;
    {$ELSE}
      try
        AInputTree := TTreeList.Create;
        ANamesList := TStringList.Create;
	    	// needed to create the expected names list and fill it with the OTU names from the MEGA Activated file.
        AExpNameStrList := TStringList.Create;
	      for i:=0 to MyUsedOtuInfos.Count-1 do
          with TOtuInfo(MyUsedOtuInfos[i]) do
          begin
            AExpNameStrList.Add(Name);
          end;
		    AInputTree.ImportFromNewickFile(FGuideTreeFile, nil);
        for i:= 0 to AInputTree.NoOfOTUs-1 do
          ANamesList.Add(AInputTree.OTUName[i]);
        tempList :=  RegexReplaceAStringList(ANamesList, '[^A-Za-z0-9]', '');
        ANamesList.Free;
        ANamesList := tempList;
        tempList := RegexReplaceAStringList(AExpNameStrList, '[^A-Za-z0-9]', '');
        AExpNameStrList.Free;
        AExpNameStrList := tempList;
        for i := 0 to AExpNameStrList.Count-1 do  // enforcing strict naming and size conventions (any changes rejects the file)
          if ANamesList.IndexOf(AExpNameStrList.Strings[i]) < 0 then
          begin
            Raise Exception.Create('The list of taxa in the guide tree file is ' +
              'different from the list of taxa in the sequence file, this must ' +
              'be corrected to complete this analysis with a guide tree.');
          end;
      finally
        FreeAndNil(ANamesList);
        FreeAndNil(AExpNameStrList);
        FreeAndNil(AInputTree);
      end;
	  {$ENDIF}
	end;
  finally
    ClustalWThread.Start;
    FreeAndNil(Tree);
  end;  
end;

function TCustomClustalW.SaveSessionProperties(filename: String): Boolean;
var
  json: TJsonObject = nil;
  str: String = '';
  aFile: TextFile;
begin
  try
    try
      AlignmentSessionPropsCI.Acquire;
      AssignFile(aFile, filename);
      Rewrite(aFile);
      json := TJsonObject.Create;
      GetSessionProperties(json);
      str := json.AsJSON;
      WriteLn(aFile, str);
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
        ShowMessage('Exception when saving TClustalW session properties - ' + E.Message + ' ' + filename);
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(aFile);
    AlignmentSessionPropsCI.Release;
    if Assigned(json) then
      json.Free;
  end;
end;

function TCustomClustalW.LoadSessionProperties(filename: String): Boolean;
var
  json: TJsonObject = nil;
  jparser: TJSONParser  = nil;
  aData: TJSONData = nil;
  aList: TStringList = nil;
begin
  Result := False;
  if not FileExists(filename) then
    Exit;

  try
    try
      AlignmentSessionPropsCI.Acquire;
      aList := TStringList.Create;
      aList.LoadFromFile(filename);
      if Trim(aList.Text) <> EmptyStr then
      begin
        jparser := TJsonParser.Create(aList.Text, []);
        aData := jparser.Parse;
        if not Assigned(aData) then
          raise Exception.Create('invalid json data');
        json := TJsonObject(aData);
        SetSessionProperties(json);
        Result := True;
      end;
    except
      on E:Exception do
      begin
        Result := False;
        {$IFDEF DEBUG}
        ShowMessage('exception when loading TClustalW session properties: ' + E.Message + ' ' + filename);
        {$ENDIF}
      end;
    end;
  finally
    AlignmentSessionPropsCI.Release;
    if Assigned(aList) then
      aList.Free;
    if Assigned(jparser) then
      jparser.Free;
  end;
end;


procedure TCustomClustalW.OnClustalWTerminate(Sender: TObject);
var
  i,j: integer;
  str: AnsiString;
begin
  FDone := false;
  {$IFDEF VISUAL_BUILD}
  if ShowProgress then
    ProgressDlg.Hide;
  {$ENDIF}

  if not ClustalWThread.Canceled then
  begin
    if XSites <> '' then
      for i := 0 to Sequences.Count-1 do
      begin
        str := Sequences[i];
        for j := 1 to Length(str) do
          if (IsDNA and (str[j] = 'N')) or ((not IsDNA) and (str[j] = 'X')) then
          begin
            str[j] := XSites[1];
            Delete(XSites, 1, 1);
          end;
        Sequences[i] := str;
      end;

    FDone := true;
  end;
  if Assigned(OnTerminate) then
    OnTerminate(Sender);
end;

procedure TCustomClustalW.GetSessionProperties(var json: TJSONObject);
begin
  json.Add('DNAMatrix', Integer(DNAMatrix));
  json.Add('ProteinMatrix', Integer(ProteinMatrix));
  json.Add('DNAPWGapOpenPenalty', DNAPWGapOpenPenalty);
  json.Add('DNAPWGapExtendPenalty', DNAPWGapExtendPenalty);
  json.Add('DNAGapOpenPenalty', DNAGapOpenPenalty);
  json.Add('DNAGapExtendPenalty', DNAGapExtendPenalty);
  json.Add('ProteinPWGapOpenPenalty', ProteinPWGapOpenPenalty);
  json.Add('ProteinPWGapExtendPenalty', ProteinPWGapExtendPenalty);
  json.Add('ProteinGapOpenPenalty', ProteinGapOpenPenalty);
  json.Add('ProteinGapExtendPenalty', ProteinGapExtendPenalty);
  json.Add('DivergentCutoff', DivergentCutoff);
  json.Add('TransitionWeight', TransitionWeight);
  json.Add('UseNegativeMatrix', UseNegativeMatrix);
  json.Add('ResidueSpecificPenalty', ResidueSpecificPenalty);
  json.Add('HydrophilicPenalty', HydrophilicPenalty);
  json.Add('EndGapSeparation', EndGapSeparation);
  json.Add('GapSeparationDistance', GapSeparationDistance);
  json.Add('ResetGaps', ResetGaps);
end;

procedure TCustomClustalW.SetSessionProperties(const json: TJsonObject);
var
  data: TJsonData = nil;
begin
  data := json.Find('DNAMatrix', jtNumber);
  if Assigned(data) then
    DNAMatrix := TDNAMatrix(data.AsInteger);
  data := json.Find('ProteinMatrix', jtNumber);
  if Assigned(data) then
    ProteinMatrix := TProteinMatrix(data.AsInteger);
  data := json.Find('DNAPWGapOpenPenalty', jtNumber);
  if Assigned(data) then
    DNAPWGapOpenPenalty := data.AsFloat;
  data := json.Find('DNAPWGapExtendPenalty', jtNumber);
  if Assigned(data) then
    DNAPWGapExtendPenalty := data.AsFloat;
  data := json.Find('DNAGapOpenPenalty', jtNumber);
  if Assigned(data) then
    DNAGapOpenPenalty := data.AsFloat;
  data := json.Find('DNAGapExtendPenalty', jtNumber);
  if Assigned(data) then
    DNAGapExtendPenalty := data.AsFloat;
  data := json.Find('ProteinPWGapOpenPenalty', jtNumber);
  if Assigned(data) then
    ProteinPWGapOpenPenalty := data.AsFloat;
  data := json.Find('ProteinPWGapExtendPenalty', jtNumber);
  if Assigned(data) then
    ProteinPWGapExtendPenalty := data.AsFloat;
  data := json.Find('ProteinGapOpenPenalty', jtNumber);
  if Assigned(data) then
    ProteinGapOpenPenalty := data.AsFloat;
  data := json.Find('ProteinGapExtendPenalty', jtNumber);
  if Assigned(data) then
    ProteinGapExtendPenalty := data.AsFloat;
  data := json.Find('DivergentCutoff', jtNumber);
  if Assigned(data) then
    DivergentCutoff := data.AsInteger;
  data := json.Find('TransitionWeight', jtNumber);
  if Assigned(data) then
    TransitionWeight := data.AsFloat;
  data := json.Find('UseNegativeMatrix', jtBoolean);
  if Assigned(data) then
    UseNegativeMatrix := data.AsBoolean;
  data := json.Find('ResidueSpecificPenalty', jtBoolean);
  if Assigned(data) then
    ResidueSpecificPenalty := data.AsBoolean;
  data := json.Find('HydrophilicPenalty', jtBoolean);
  if Assigned(data) then
    HydrophilicPenalty := data.AsBoolean;
  data := json.Find('EndGapSeparation', jtBoolean);
  if Assigned(data) then
    EndGapSeparation := data.AsBoolean;
  data := json.Find('GapSeparationDistance', jtNumber);
  if Assigned(data) then
    GapSeparationDistance := data.AsInteger;
  data := json.Find('ResetGaps', jtBoolean);
  if Assigned(data) then
    ResetGaps := data.AsBoolean;
end;

function TCustomClustalW.GetSettingsAsJson: TJSONObject;
var
  tempStr: String = '';

  function DnaMatrixStr: String;
  begin
    case DNAMatrix of
      iub: Result := 'iub';
      clustalw: Result := 'clustalw';
    end;
  end;

  function ProteinMatrixStr: String;
  begin
    case ProteinMatrix of
      blosum: Result := 'blosum';
      pam: Result := 'pam';
      gonnet: Result := 'gonnet';
      identity: Result := 'identity';
    end;
  end;

begin
  Result := TJSONObject.Create;
  Result.Add('Analysis', 'Sequence Alignment');
  Result.Add(opsPhylo2, 'ClustalW');

  if IsDNA then
  begin
    Result.Add('DNAMatrix', DnaMatrixStr);
    Result.Add('DNAPWGapOpenPenalty', DNAPWGapOpenPenalty);
    Result.Add('DNAPWGapExtendPenalty', DNAPWGapExtendPenalty);
    Result.Add('DNAGapOpenPenalty', DNAGapOpenPenalty);
    Result.Add('DNAGapExtendPenalty', DNAGapExtendPenalty);
  end
  else
  begin
    Result.Add('ProteinMatrix', ProteinMatrixStr);
    Result.Add('ProteinPWGapOpenPenalty', ProteinPWGapOpenPenalty);
    Result.Add('ProteinPWGapExtendPenalty', ProteinPWGapExtendPenalty);
    Result.Add('ProteinGapOpenPenalty', ProteinGapOpenPenalty);
    Result.Add('ProteinGapExtendPenalty', ProteinGapExtendPenalty);
  end;

  Result.Add('DivergentCutoff', DivergentCutoff);
  Result.Add('TransitionWeight', TransitionWeight);
  Result.Add('UseNegativeMatrix', BoolToStr(UseNegativeMatrix, True));
  Result.Add('ResidueSpecificPenalty', BoolToStr(ResidueSpecificPenalty, True));
  Result.Add('HydrophilicPenalty', BoolToStr(HydrophilicPenalty, True));
  Result.Add('EndGapSeparation', BoolToStr(EndGapSeparation, True));
  Result.Add('GapSeparationDistance', GapSeparationDistance);
  Result.Add('ResetGaps', BoolToStr(ResetGaps, True));
end;

procedure TCustomClustalW.SetDefaults;
begin
  DNAMatrix                  := iub;
  ProteinMatrix              := gonnet;

  DNAPWGapOpenPenalty        := 15.0;
  DNAPWGapExtendPenalty      := 6.66;
  DNAGapOpenPenalty          := 15.0;
  DNAGapExtendPenalty        := 6.66;
  ProteinPWGapOpenPenalty    := 10.0;
  ProteinPWGapExtendPenalty  := 0.1;
  ProteinGapOpenPenalty      := 10.0;
  ProteinGapExtendPenalty    := 0.2;

  DivergentCutoff            := 30;
  TransitionWeight           := 0.5;
  UseNegativeMatrix          := false;

  ResidueSpecificPenalty     := true;
  HydrophilicPenalty         := true;
  EndGapSeparation           := false;
  GapSeparationDistance      := 4;

  ResetGaps                  := true;
  ShowProgress               := true;
end;


end.

