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

unit MAlnThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF VISUAL_BUILD}
 MAlnProgress,
{$ENDIF}
 MegaUtils_NV, Classes, Controls, Dialogs, MegaConsts, MTreeData, matrices;

type
  treeptr = ^stree;
  stree = record   //* phylogenetic tree structure */
     left   : treeptr;
     right  : treeptr;
     parent : treeptr;
     dist   : double;
     leaf   : boolean;
     order  : integer;
//     name   : string;
  end;            //  stree, *treeptr;

  ArrayOftreeptr = array[0..MaxPointerElts-1] of treeptr;
  PArrayOftreeptr = ^ArrayOftreeptr;

  TMatMenuEntry = record
     title  : AnsiString;
     text   : AnsiString;   // string : string;
  end;

  TMatMenu = record
     noptions : integer;
     opt : array [0..9] of TMatMenuEntry;
  end;

  TSeriesMat = record
     llimit  : integer;
     ulimit  : integer;
     matptr  : PArrayOfInt;
     aa_xref : PArrayOfInt;
  end;

  TUserMatSeries = record
     nmat : integer;
     mat  : array[0..9] of TSeriesMat;
  end;

  TDNAMatrix = (iub, clustalw);
  TProteinMatrix = (blosum, pam, gonnet, identity);

  TIntMatrix= array [0..MaxPointerElts-1] of PArrayOfInt;
  PIntMatrix= ^TIntMatrix;

  TMatrix = array[0..31,0..31] of integer;  // 2D array of NUMRES x NUMRES

  TAlignMode = (amMultiple, amProfile, amSequence);

  TClustalWThread = class(TMEGAThread) 
  private
    tmat : PDistanceMatrix;

    gap_open, gap_extend : double;
    pw_go_penalty, pw_ge_penalty : double;

    seqlen_array : PArrayOfInt;

    max_aln_length : integer;
    usermat, pw_usermat : TMatrix;
    userdnamat, pw_userdnamat : TMatrix;
    def_aa_xref, aa_xref, pw_aa_xref : array[0..32] of integer;
    def_dna_xref, dna_xref, pw_dna_xref : array[0..32] of integer;
    nseqs, nsets : integer;
    output_index : PArrayOfInt;

    sets: PIntMatrix;
    seq_weight : PArrayOfInt;
    max_aa : integer;
    gap_pos1 : integer;
    gap_pos2 : integer;
    mat_avscore : integer;
    profile_no : integer;
    profile1_nseqs : integer;

    distance_tree : Boolean;

    seq_array : PArrayOfPChar;

    gap_penalty_mask1, gap_penalty_mask2 : PAnsiChar;
    sec_struct_mask1, sec_struct_mask2 : PAnsiChar;

    lptr : PArrayOftreeptr;
    olptr : PArrayOftreeptr;
    nptr : PArrayOftreeptr;
    ptrs : PArrayOftreeptr;


{
    multiple alignment parameters
}
    dna_gap_open : double;
    dna_gap_extend : double;
    prot_gap_open : double;
    prot_gap_extend : double;
// sint		profile_type = PROFILE;
    gap_dist : integer;
    output_order  : integer;
    divergence_cutoff : integer;
    matnum : integer;
    mtrxname : AnsiString;
    dnamatnum : integer;
    dnamtrxname : AnsiString;
    no_weights : boolean;
    neg_matrix : boolean;
    no_hyd_penalties : boolean;
    no_pref_penalties : boolean;
    no_var_penalties : boolean;
    use_endgaps : boolean;
    endgappenalties : boolean;
    reset_alignments_new : boolean;
    reset_alignments_all : boolean;
    output_struct_penalties : integer;
    struct_penalties1 : boolean;
    struct_penalties2 : boolean;
    use_ss1 : boolean;
    use_ss2 : boolean;

//    helix_penalty : integer;
//    strand_penalty : integer;
//    loop_penalty : integer;
//    helix_end_minus : integer;
//    helix_end_plus : integer;
//    strand_end_minus : integer;
//    strand_end_plus : integer;
//    helix_end_penalty : integer;
//    strand_end_penalty : integer;

{
    pairwise alignment parameters
}
    dna_pw_go_penalty : double;
    dna_pw_ge_penalty : double;
    prot_pw_go_penalty : double;
    prot_pw_ge_penalty : double;
    pw_matnum : integer;
    pw_mtrxname : AnsiString;
    pw_dnamatnum : integer;
    pw_dnamtrxname : AnsiString;

//    quick_pairalign : boolean;
    transition_weight : double;

{
    quick pairwise alignment parameters
}

    debug: integer;
    dnaflag : boolean;
    FScore: integer;
    FAlignMode: TAlignMode;

    FSeqNames: TStringList;
    FSeqArray: TStringList;
    FTree : TTreeData;
    FisRootedTree : boolean;

    Err: boolean;
    ErrMessage: AnsiString;

    procedure init_matrix;
    function get_matrix(matptr, xref: PArrayOfInt; var matrix : TMatrix; neg_flag : Boolean; scale: integer):integer;
    procedure SetSeqArray(seqs : TStringList);
    procedure SetSeqNames(names : TStringList);
    procedure SetTree(tree : TTreeData);

    function pairalign(istart, iend, jstart, jend: integer):integer;
    function malign(istart: integer):integer;
    function prfalign(group, aligned : PArrayOfInt):integer;
    function calc_weight(leaf: integer):integer;
    procedure aln_score;
    procedure calc_seq_weights(first_seq, last_seq : integer; sweight : PArrayOfInt);
    procedure calc_gap_coeff(alignment: PArrayOfPChar; gaps: PArrayOfInt; profile: PIntMatrix; struct_penalties: boolean;
                             gap_penalty_mask: PAnsiChar; first_seq, last_seq, prf_length, gapcoef, lencoef: integer);
    procedure calc_prf1(profile: PIntMatrix; alignment: PArrayOfPChar; gaps: PArrayOfInt;
                        var matrix: TMatrix; seq_weight: PArrayOfInt; prf_length, first_seq, last_seq: integer);
    procedure calc_prf2(profile: PIntMatrix; alignment: PArrayOfPChar; seq_weight: PArrayOfInt;
                        prf_length, first_seq, last_seq: integer);

    function GetDNAMatrix:TDNAMatrix;
    procedure SetDNAMatrix(m: TDNAMatrix);
    function GetProteinMatrix:TProteinMatrix;
    procedure SetProteinMatrix(m: TProteinMatrix);
    function GetHydroPenalty:boolean;
    procedure SetHydroPenalty(value: boolean);
    function GetResPenalty:boolean;
    procedure SetResPenalty(value: boolean);

    function GetUseUserTree: boolean;
  protected
  {$IFDEF VISUAL_BUILD} 
    FProgressDlg: TClustalWProgressDlg;
    {$ENDIF}	
    ProgressUnit, ProgressNum: integer;
    FCanceled: boolean;

    {$IFDEF VISUAL_BUILD}	
    procedure SetProgressDlg(dlg: TClustalWProgressDlg);
    {$ENDIF}	
    function GetShowProgress: boolean;
    procedure InitProgressDlg;
    procedure DoOnPairwiseProgress; virtual;
    procedure DoOnMultipleProgress; virtual;
    procedure Execute; override;
    procedure OnPairwiseProgress;
    procedure OnMultipleProgress;

    function Align:boolean;

    function DoMultipleAlignment: boolean;


    procedure DoOnError;
  public
    Time1, Time2: TDateTime;
    CheckGuideTree: TThreadMethod;

    property Canceled: boolean read FCanceled;

    {$IFDEF VISUAL_BUILD}	
    property ProgressDlg: TClustalWProgressDlg read FProgressDlg write SetProgressDlg;
    {$ENDIF}	
    property ShowProgress: boolean read GetShowProgress;

    property SeqList : TStringList read FSeqArray write SetSeqArray;
    property SeqNames : TStringList read FSeqNames write SetSeqNames;
    property IsDNA : boolean read dnaflag write dnaflag;
    property UseUserTree: boolean read GetUseUserTree;
    property Tree : TTreeData read FTree write SetTree;
    property isRootedTree : boolean read FisRootedTree write FisRootedTree;
    property Score: integer read FScore;

    property AlignMode: TAlignMode read FAlignMode write FAlignMode;
    property NoOfProfile1Seqs: integer read profile1_nseqs write profile1_nseqs;

    property DNAMatrix: TDNAMatrix read GetDNAMatrix write SetDNAMatrix;
    property ProteinMatrix: TProteinMatrix read GetProteinMatrix write SetProteinMatrix;

    property DNAPWGapOpenPenalty: double read dna_pw_go_penalty write dna_pw_go_penalty;
    property DNAPWGapExtendPenalty: double read dna_pw_ge_penalty write dna_pw_ge_penalty;
    property DNAGapOpenPenalty: double read dna_gap_open write dna_gap_open;
    property DNAGapExtendPenalty: double read dna_gap_extend write dna_gap_extend;
    property ProteinPWGapOpenPenalty: double read prot_pw_go_penalty write prot_pw_go_penalty;
    property ProteinPWGapExtendPenalty: double read prot_pw_ge_penalty write prot_pw_ge_penalty;
    property ProteinGapOpenPenalty: double read prot_gap_open write prot_gap_open;
    property ProteinGapExtendPenalty: double read prot_gap_extend write prot_gap_extend;

    property DivergentCutoff: integer read divergence_cutoff write divergence_cutoff;
    property TransitionWeight: double read transition_weight write transition_weight;
    property UseNegativeMatrix: boolean read neg_matrix write neg_matrix;

    property ResidueSpecificPenalty: boolean read GetResPenalty write SetResPenalty;
    property HydrophilicPenalty: boolean read GetHydroPenalty write SetHydroPenalty;
    property EndGapSeparation: boolean read use_endgaps write use_endgaps;
    property GapSeparationDistance: integer read gap_dist write gap_dist;


    procedure GetTreeData(treedata: TTreeData);
    procedure SetTreeData(treedata: TTreeData);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}mruntimeprogressdlg,{$ENDIF}
  Math, SysUtils, MTreeEstFunc,MGlobalSettings;

const
  INT_SCALE_FACTOR : integer = 100;  // Scaling factor to convert float to integer for profile scores */
  NUMRES : integer = 32;		// max size of comparison matrix */
  //MAXMAT : integer = 10;
  GAPCOL : integer = 32;
  LENCOL : integer = 33;
  //ALIGNED: integer =  1;
  INPUT  : integer =  0;

const
  amino_acid_codes : AnsiString = 'ABCDEFGHIKLMNPQRSTUVWXYZ-';
  hyd_residues : AnsiString = 'GPSNDQEKR';
  pr : array[0..19] of AnsiChar = ('A', 'C', 'D', 'E', 'F', 'G', 'H', 'K', 'I', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'Y', 'W');
  pas_op : array[0..19] of integer = (87, 87,104, 69, 80,139,100,104, 68, 79, 71,137,126, 93,128,124,111, 75,100, 77);
  //pas_op2: array[0..19] of integer = (88, 57,111, 98, 75,126, 95, 97, 70, 90, 60,122,110,107, 91,125,124, 81,106, 88);
  //pal_op : array[0..19] of integer = (84, 69,128, 78, 88,176, 53, 95, 55, 49, 52,148,147,100, 91,129,105, 51,128, 88);

function TClustalWThread.GetDNAMatrix:TDNAMatrix;
begin
  if dnamtrxname = 'iub' then
    result := iub
  else
    result := clustalw;
end;

procedure TClustalWThread.SetDNAMatrix(m: TDNAMatrix);
begin
  if m = iub then
    dnamtrxname := 'iub'
  else if m = clustalw then
    dnamtrxname := 'clustalw';
  pw_dnamtrxname := dnamtrxname;
end;

function TClustalWThread.GetProteinMatrix:TProteinMatrix;
begin
  if mtrxname = 'blosum' then
    result := blosum
  else if mtrxname = 'pam' then
    result := pam
  else if mtrxname = 'gonnet' then
    result := gonnet
  else
    result := identity;
end;

procedure TClustalWThread.SetProteinMatrix(m: TProteinMatrix);
begin
  if m = blosum then
    mtrxname := 'blosum'
  else if m = pam then
    mtrxname := 'pam'
  else if m = gonnet then
    mtrxname := 'gonnet'
  else if m = identity then
    mtrxname := 'id';
  pw_mtrxname := mtrxname;
end;

function TClustalWThread.GetResPenalty:boolean;
begin
  result := not no_pref_penalties;
end;

procedure TClustalWThread.SetResPenalty(value: boolean);
begin
  no_pref_penalties := not value;
end;

function TClustalWThread.GetHydroPenalty:boolean;
begin
  result := not no_hyd_penalties;
end;

procedure TClustalWThread.SetHydroPenalty(value: boolean);
begin
  no_hyd_penalties := not value;
end;

function Max(a,b: integer):integer;
begin
   if a > b then
      result := a
   else
      result := b;
end;

function Min(a,b: integer):integer;
begin
   if a < b then
      result := a
   else
      result := b;
end;

constructor TClustalWThread.Create;
begin
  inherited Create(true);

  {$IFDEF VISUAL_BUILD}
  FProgressDlg := nil;
  {$ENDIF}
  FreeOnTerminate := false;
  FCanceled := false;
  FAlignMode := amMultiple;

  profile_no := 0;
  profile1_nseqs := 0;
  dna_gap_open := 15.0;
  dna_gap_extend := 6.66;
  prot_gap_open := 10.0;
  prot_gap_extend := 0.2;
// sint		profile_type = PROFILE;
  gap_dist := 4;
  output_order  := 1;
  divergence_cutoff := 30;
  matnum := 3;
  mtrxname := 'gonnet';
  dnamatnum := 1;
  dnamtrxname := 'iub';
  no_weights := FALSE;
  neg_matrix := FALSE;
  no_hyd_penalties := FALSE;
  no_pref_penalties := FALSE;
  no_var_penalties := true;
  use_endgaps := FALSE;
  endgappenalties := FALSE;
  reset_alignments_new := FALSE;
  reset_alignments_all := FALSE;
  output_struct_penalties := 0;
  struct_penalties1 := false;
  struct_penalties2 := false;
  use_ss1 := TRUE;
  use_ss2 := TRUE;
//  helix_penalty := 4;
//  strand_penalty := 4;
//  loop_penalty := 1;
//  helix_end_minus := 3;
//  helix_end_plus := 0;
//  strand_end_minus := 1;
//  strand_end_plus := 1;
//  helix_end_penalty := 2;
//  strand_end_penalty := 2;

  transition_weight := 0.5;

{
  pairwise alignment parameters
}
  dna_pw_go_penalty := 15.0;
  dna_pw_ge_penalty := 6.66;
  prot_pw_go_penalty := 10.0;
  prot_pw_ge_penalty := 0.1;
  pw_matnum := 3;
  pw_mtrxname := 'gonnet';
  pw_dnamatnum := 1;
  pw_dnamtrxname := 'iub';

  FSeqArray := nil;
  FTree := nil;
  FisRootedTree := false;
  FScore := 0;

  seq_array := nil;
  seqlen_array := nil;
  seq_weight := nil;
  output_index := nil;
  tmat := nil;
  nptr := nil;
  ptrs := nil;
  lptr := nil;
  olptr := nil;
  sets := nil;

  nseqs := 0;
  ProgressNum := 1;

  init_matrix;
end;

destructor TClustalWThread.Destroy;
var
  i : integer;
begin
  if FTree <> nil then
    FTree.Free;

  if seq_array <> nil then
  begin
    for i := 1 to nseqs do
      FreeMemAndNil(seq_array[i]);
    FreeMemAndNil(seq_array);
  end;
  if seqlen_array <> nil then
    FreeMemAndNil(seqlen_array);
  if seq_weight <> nil then
    FreeMemAndNil(seq_weight);
  if output_index <> nil then
    FreeMemAndNil(output_index);
  if tmat <> nil then
  begin
    for i := 0 to nseqs-1 do
      FreeMemAndNil(tmat[i]);
    FreeMemAndNil(tmat);
  end;

  inherited;
end;

procedure TClustalWThread.Execute;
begin

  ReturnValue := 1;
  if Terminated then
  begin
    FCanceled := true;
    Exit;
  end;
  try
    try
      if Align then
        ReturnValue := 0
      else
        FCanceled := true;
    except
     on E:Exception do
     begin
       FCanceled := true;
       {$IFDEF DEBUG}
         {$IFDEF VISUAL_BUILD}
           ShowMessage('Error in ClustalThread.Execute: ' + E.Message);
         {$ELSE}
           error_nv('Error in ClustalThread.Execute: ' + E.Message);
         {$ENDIF}
       {$ENDIF}
     end;
    end;
  finally
    if Terminated then
      FCanceled := true;
  end;
end;
{$IFDEF VISUAL_BUILD}
procedure TClustalWThread.SetProgressDlg(dlg: TClustalWProgressDlg);
begin
  FProgressDlg := dlg;
  if FProgressDlg <> nil then
    FProgressDlg.Thread := Self;
end;
{$ENDIF}


function TClustalWThread.GetShowProgress: boolean;
begin
  Result := True;
{$IFDEF VISUAL_BUILD}
  result := FProgressDlg <> nil;
{$ENDIF}
end;

procedure TClustalWThread.InitProgressDlg;
begin
  {$IFDEF VISUAL_BUILD}
  if FProgressDlg = nil then exit;
  {$ENDIF}
  if nseqs < 10 then
    ProgressNum  := 80 div (nseqs*(nseqs-1) div 2)
  else
    ProgressNum  := 1;
  {$IFDEF VISUAL_BUILD}
  if nseqs < 10 then
    FProgressDlg.PairwiseProgressBar.Max := (2*ProgressNum +round(power(2,trunc(ln(ProgressNum)/ln(2)))))*nseqs*(nseqs-1) div 2
  else
    FProgressDlg.PairwiseProgressBar.Max := ProgressNum*(nseqs*(nseqs-1) div 2)*3;

  if nseqs-1 > 15 then
    FProgressDlg.MultipleProgressBar.Max := (nseqs-1)*16
  else
    FProgressDlg.MultipleProgressBar.Max := (15 div (nseqs-1))*16*(nseqs-1);

  FProgressDlg.PairwiseProgressBar.Position := 0;
  FProgressDlg.MultipleProgressBar.Position := 0;
  FProgressDlg.Show;
  {$ENDIF}
end;

procedure TClustalWThread.DoOnPairwiseProgress;
begin
{$IFDEF VISUAL_BUILD}
  if FProgressDlg.PairwiseProgressBar.Position < FProgressDlg.PairwiseProgressBar.Max then
    FProgressDlg.PairwiseProgressBar.StepIt;
{$ELSE}
  ShowProgressIncrementStatic;
{$ENDIF}
end;

procedure TClustalWThread.DoOnMultipleProgress;
begin
{$IFDEF VISUAL_BUILD}
  if FProgressDlg.MultipleProgressBar.Position < FProgressDlg.MultipleProgressBar.Max then
    FProgressDlg.MultipleProgressBar.StepIt;
{$ELSE}
  ShowProgressIncrementStatic;
{$ENDIF}
end;

procedure TClustalWThread.OnPairwiseProgress;
begin
//FPairwiseProgress := Progress;
  if ShowProgress then
    Synchronize(DoOnPairwiseProgress);
end;

procedure TClustalWThread.OnMultipleProgress;
begin
//FMultipleProgress := Progress;
  if ShowProgress then
    Synchronize(DoOnMultipleProgress);
end;

procedure TClustalWThread.DoOnError;
begin
  {$IFDEF VISUAL_BUILD} 
  if MessageDlg(ErrMessage, mtError, [mbIgnore,mbAbort], 0) = mrIgnore then
    Err := false;
  {$ELSE}
  Err := False;
  warn_nv('Error occured curing ClustalW alignment: ' + ErrMessage);
  {$ENDIF} 
end;

procedure TClustalWThread.init_matrix;
var
   c1,c2 : AnsiChar;
   i, j, maxres : integer;
begin
   max_aa := Length(amino_acid_codes)-2;
   gap_pos1 := NUMRES-2;          //* code for gaps inserted by clustalw */
   gap_pos2 := NUMRES-1;          //* code for gaps already in alignment */

{
   set up cross-reference for default matrices hard-coded in matrices.h
}
   for i :=0 to NUMRES-1 do def_aa_xref[i] := -1;
   for i :=0 to NUMRES-1 do def_dna_xref[i] := -1;

   maxres := 0;
   for i := 0 to Length(amino_acid_order)-1 do
   begin
      c1 := amino_acid_order[i+1];
      for j := 0 to Length(amino_acid_codes)-1 do
      begin
         c2 := amino_acid_codes[j+1];
         if c1 = c2 then
         begin
            def_aa_xref[i] := j;
            maxres := maxres +1;
            break;
         end;
      end;

      if (def_aa_xref[i] = -1) and (amino_acid_order[i] <> '*') then
//          error("residue %c in matrices.h is not recognised", amino_acid_order[i]);
   end;

   maxres := 0;
   for i := 0 to Length(nucleic_acid_order)-1 do
   begin
      c1 := nucleic_acid_order[i+1];
      for j :=0 to Length(amino_acid_codes)-1 do
      begin
         c2 := amino_acid_codes[j+1];
         if c1 = c2 then
         begin
            def_dna_xref[i] := j;
            maxres := maxres +1;
            break;
         end;
      end;
//      if (def_dna_xref[i] = -1) and (nucleic_acid_order[i] <> '*') then
//         error("nucleic acid %c in matrices.h is not recognised", nucleic_acid_order[i]);
   end;
end;

function TClustalWThread.get_matrix(matptr, xref: PArrayOfInt; var matrix : TMatrix; neg_flag : Boolean; scale: integer):integer;
var
   gg_score, gr_score, i, j, k, ix, ti, tj, maxres, av1,av2,av3,min, max : integer;
begin
   gg_score := 1;
   gr_score := 0;
   k := 0;
   ix := 0;
{
   default - set all scores to 0
}
   for i :=0 to max_aa do
      for j := 0 to max_aa do
          matrix[i][j] := 0;

   ix := 0;
   maxres := 0;
   for i :=0 to max_aa do
   begin
      ti := xref[i];
      for j := 0 to i do
      begin
         tj := xref[j]; 
         if (ti <> -1) and (tj <> -1) then
         begin
            k := matptr[ix];
            if ti = tj then
            begin
               matrix[ti][ti] := k * scale;
               maxres := maxres +1;
            end
            else
            begin
               matrix[ti][tj] := k * scale;
               matrix[tj][ti] := k * scale;
            end;
            ix := ix +1;
         end;
      end;
   end;

   maxres := maxres -1;

   av1 := 0;
   av2 := 0;
   av3 := 0;
   for i := 0 to max_aa do
      for j :=0 to i do
      begin
         av1 :=  av1 +matrix[i][j];
         if i = j then
            av2 := av2 +matrix[i][j]
         else
            av3 := av3 +matrix[i][j];
      end;

   av1 := av1 div ((maxres*maxres) div 2);
   av2 := av2 div maxres;
   av3 := av3 div ((maxres*maxres-maxres) div 2);
   mat_avscore := -av3;

   min := matrix[0][0];
   max := matrix[0][0];
   for i := 0 to max_aa do
      for j := 1 to i do
      begin
        if matrix[i][j] < min then
           min := matrix[i][j];
        if matrix[i][j] > max then
           max := matrix[i][j];
      end;

// if (debug>1) fprintf(stdout,"average mismatch score %d\n",(pint)av3);
// if (debug>1) fprintf(stdout,"average match score %d\n",(pint)av2);
// if (debug>1) fprintf(stdout,"average score %d\n",(pint)av1);

{
   if requested, make a positive matrix - add -(lowest score) to every entry
}
   if neg_flag = FALSE then
   begin

// if (debug>1) fprintf(stdout,"min %d max %d\n",(pint)min,(pint)max);

      if min < 0 then
         for i :=0 to max_aa do
         begin
            ti := xref[i];
            if ti <> -1 then
               for j :=0 to max_aa do
               begin
                  tj := xref[j];
{
                if (tj <> -1) matrix[ti][tj] -= (2*av3);
}
                  if tj <> -1 then matrix[ti][tj] := matrix[ti][tj] -min;
               end;
         end;
{
       gr_score := av3;
       gg_score := -av3;
}

   end;

   for i := 0 to gap_pos1-1 do
   begin
      matrix[i][gap_pos1] := gr_score;
      matrix[gap_pos1][i] := gr_score;
      matrix[i][gap_pos2] := gr_score;
      matrix[gap_pos2][i] := gr_score;
   end;
   matrix[gap_pos1][gap_pos1] := gg_score;
   matrix[gap_pos2][gap_pos2] := gg_score;
   matrix[gap_pos2][gap_pos1] := gg_score;
   matrix[gap_pos1][gap_pos2] := gg_score;

   maxres := maxres +2;

   result := maxres;
end;

function TClustalWThread.GetUseUserTree: boolean;
begin
  result := assigned(FTree);
end;

procedure TClustalWThread.SetTree(tree : TTreeData);
begin
  if tree.NoOfOTUs <> nseqs then
    Exit;

  if FTree = nil then
    FTree := TTreeData.Create(tree.NoOfOTUs, tree.isBLen, tree.isSE, tree.isStats);

  FTree.Assign(tree);
end;

procedure TClustalWThread.GetTreeData(treedata: TTreeData);
begin
  if treedata.NoOfOTUs <> nseqs then
    Exit;
  if FTree = nil then
    Exit;

  treedata.Assign(FTree);
end;

procedure TClustalWThread.SetTreeData(treedata: TTreeData);
begin
  if treedata.NoOfOTUs <> nseqs then
    Exit;

  if FTree = nil then
    FTree := TTreeData.Create(treedata.NoOfOTUs, treedata.isBLen, treedata.isSE, treedata.isStats);

  FTree.Assign(treedata);
end;

procedure TClustalWThread.SetSeqNames(names : TStringList);
var
  i: integer;
begin
  if names.Count = 0 then exit;
  if FSeqNames = nil then
    FSeqNames := TStringList.Create;

  FSeqNames.Clear;
  for i := 0 to names.Count-1 do
    FSeqNames.Add(names[i]);
end;

procedure TClustalWThread.SetSeqArray(seqs : TStringList);

   function res_index(t: AnsiString; c: AnsiChar):integer;
   var
      i: integer;
   begin
      result := -1;
      for i := 1 to Length(t) do
         if UpCase(t[i]) = UpCase(c) then
            result := i-1;
   end;

   procedure p_encode(seq: PAnsiChar; l: integer);
   var
     i: integer;
   begin

//* code seq as ints .. use gap_pos2 for gap */
//*	static char *aacids="CSTPAGNDEQHRKMILVFYW";*/
	
      for i := 1 to l do
         if seq[i] = '-' then
            seq[i] := AnsiChar(Chr(gap_pos2))
         else
	    seq[i] := AnsiChar(Chr(res_index(amino_acid_codes, seq[i])));
      seq[l+1] := AnsiChar(-3);
   end;

   procedure n_encode(seq: PAnsiChar; l: integer);
   var
     i: integer;
   begin

//* code seq as ints .. use gap_pos2 for gap */
//*	static char *nucs="ACGTU";	*/

      for i := 1 to l do
         if seq[i] = '-' then       	  //* if a gap character -> code = gap_pos2 */
	    seq[i] := AnsiChar(Chr(gap_pos2))     //* this is the code for a gap in */
	 else                             //* the input files */
	    seq[i] := AnsiChar(Chr(res_index(amino_acid_codes,seq[i])));
      seq[l+1] := AnsiChar(-3);
   end;

    function check_dnaflag(seq: PAnsiChar; slen: integer):boolean;
//* check if DNA or Protein
//   The decision is based on counting all A,C,G,T,U or N.
//   If >= 85% of all characters (except -) are as above => DNA  */
    var
       i, c, nresidues, nbases : integer;
//       ratio: double;
       dna_codes: AnsiString;
    begin
	dna_codes := 'ACGTUN';

	nresidues := 0;
        nbases := 0;
	for i := 1 to slen do
           if seq[i] <> '-' then
           begin
	      nresidues := nresidues +1;
              c := res_index(dna_codes, seq[i]);
	      if c >= 0 then
		 nbases := nbases +1;
           end;

	if (nbases = 0) or (nresidues = 0) then
        begin
           result := false;
           exit;
        end;
//	ratio := nbases/nresidues;
//* DES 	fprintf(stdout,"\n nbases = %d, nresidues = %d, ratio = %f\n",
//		(pint)nbases,(pint)nresidues,(pint)ratio); */
{
	if ratio >= 0.85 then
           result := true
	else
           result := false;
}
        if nbases = nresidues then
          result := true
        else
          result := false;

   end;

var
  i,j: integer;
  //debug: Integer;
begin
  try
    if nseqs > 0 then
    begin
      if seq_array <> nil then
      begin
        for i := 1 to nseqs do
          FreeMemAndNil(seq_array[i]);
        FreeMemAndNil(seq_array);
      end;
      FreeMemAndNil(seqlen_array);
      FreeMemAndNil(seq_weight);
      FreeMemAndNil(output_index);
      if tmat <> nil then
      begin
        for i := 0 to nseqs-1 do
          FreeMemAndNil(tmat[i]);
        FreeMemAndNil(tmat);
      end;
      FSeqArray := nil;
    end;

    if seqs.count < 2 then Exit;

    nseqs := seqs.count;
    GetMem(seq_weight, sizeof(integer)*(nseqs+2));
    GetMem(seq_array, sizeof(PAnsiChar)*(nseqs+2));
    for i := 0 to nseqs-1 do
      seq_array[i] := nil;
    GetMem(seqlen_array, sizeof(integer)*(nseqs+2));
    GetMem(output_index,  sizeof(integer)*(nseqs+2));
    GetMem(tmat, sizeof(Pointer)*(nseqs+2));
    for i := 0 to nseqs-1 do
    begin
      //tmat[i] := nil;
      GetMem(tmat[i], sizeof(double)*(nseqs+2));
    end;
    //GetMem(seq_weight, sizeof(integer)*(nseqs+2));
    max_aln_length := 0;
    for i := 1 to nseqs do
      GetMem(seq_array[i], sizeof(AnsiChar)*(Length(seqs[i-1])+2));

    for i := 0 to nseqs-1 do
    begin
      seq_array[i+1,0] := #0;
      for j := 1 to Length(seqs[i]) do
        seq_array[i+1,j] := AnsiString(seqs[i])[j];
      seq_array[i+1, Length(seqs[i])+1] := #0;

      seqlen_array[i+1] := Length(seqs[i]);
      if max_aln_length < seqlen_array[i+1] then
        max_aln_length := seqlen_array[i+1];
    end;

    FSeqArray := seqs;

  //  if not dnaflag then
    if dnaflag then
      dnaflag := check_dnaflag(seq_array[1], Length(seqs[0])); //* check DNA/Prot */

    for i := 0 to nseqs-1 do
      if dnaflag then
        n_encode(seq_array[i+1], Length(seqs[i]))  //* encode the sequence*/
      else				     //* as ints  */
        p_encode(seq_array[i+1], Length(seqs[i]));

    max_aln_length := max_aln_length *2;
  except
    on E:Exception do
    begin
      {$IFNDEF VISUAL_BUILD}
      error_nv('error in TClustalThread.SetSeqArray: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TClustalWThread.Align: boolean;
begin
  result := DoMultipleAlignment;
end;

function TClustalWThread.DoMultipleAlignment: boolean;
var
   i,j,len : integer;
   tmpSeq: AnsiString;
begin
//        info("Start of Pairwise alignments");
//        info("Aligning...");

  if ShowProgress then
    Synchronize(InitProgressDlg);

  result := false;

  if dnaflag then
  begin
    gap_open      := dna_gap_open;
    gap_extend    := dna_gap_extend;
    pw_go_penalty := dna_pw_go_penalty;
    pw_ge_penalty := dna_pw_ge_penalty;
//    ktup          := dna_ktup;
//    window        := dna_window;
//    signif        := dna_signif;
//    wind_gap      := dna_wind_gap;
  end
  else
  begin
    gap_open      := prot_gap_open;
    gap_extend    := prot_gap_extend;
    pw_go_penalty := prot_pw_go_penalty;
    pw_ge_penalty := prot_pw_ge_penalty;
//    ktup          := prot_ktup;
//    window        := prot_window;
//    signif        := prot_signif;
//    wind_gap      := prot_wind_gap;
  end;

  {$IFNDEF UNIX} { On *NIX systems it is an error to pause a thread from within another thread}
  if assigned(CheckGuideTree) then
    Synchronize(CheckGuideTree);
  {$ENDIF}
  if not UseUserTree then
  begin
    Time1 := Time;
    i := pairalign(0, nseqs, 0, nseqs);
    Time1 := Time-Time1;
    if i <= 0 then
    begin
      if i = -1 then
        {$IFDEF VISUAL_BUILD} 
        ShowMessage('Internal error occurred in Clustal. Alignment aborted.'); 
        {$ELSE}
        error_nv('Internal error occurred in Clustal. Alignment aborted.');
        {$ENDIF} 
      exit;
    end;

    FTree := TTreeData.Create(nseqs, true, false, false);
    if nseqs = 2 then
    begin
      FTree.NodeArray[0].des1 := 0;
      FTree.NodeArray[0].des2 := 1;
      FTree.BLen[0] := tmat[1,0]/2;
      FTree.BLen[1] := tmat[1,0]/2;
    end
    else
      MakeNJTree(FTree, tmat, false);
  end;

  Time2 := Time;
  i := malign(0);
  Time2 := Time-Time2;
  if i > 0 then
    result := true
  else
  begin
    if i = -1 then
    {$IFDEF VISUAL_BUILD} 
      ShowMessage('Internal error occurred in Clustal. Alignment aborted.');
      {$ELSE}
      error_nv('Internal error occurred in Clustal. Alignment aborted.');
      {$ENDIF} 
    result := false;
  end;

  if result then
  begin
    len := 0;
    for i := 1 to nseqs do
      if len < seqlen_array[i] then
         len := seqlen_array[i];
    for i := 1 to nseqs do
    begin
      setlength(tmpSeq, len);
      for j := 1 to len do
      	if (integer(seq_array[i][j])= -3) or (integer(seq_array[i][j]) = 253) then
          begin
            setlength(tmpSeq, j);
	          break;
          end
        else if (integer(seq_array[i][j]) < 0) or (integer(seq_array[i][j]) > max_aa) then
          tmpSeq[j] := '-'
        else
	        tmpSeq[j] := amino_acid_codes[integer(seq_array[i][j])+1];
        FseqArray[i-1] := tmpSeq;
    end;
  end;

end;

function TClustalWThread.pairalign(istart, iend, jstart, jend: integer):integer;
var
   int_scale : integer;
   mm_score : double;
   print_ptr, last_print : integer;
   displ : PArrayOfInt;
   HH, DD, RR, SS : PArrayOfInt;
   g, gh : integer;
   seq1, seq2 : integer;
   matrix : TMatrix;
   maxscore : integer;
   sb1, sb2, se1, se2 : integer;

   procedure add(v: integer);
   begin
      if last_print < 0 then
      begin
         displ[print_ptr-1] := v;
         displ[print_ptr] := last_print;
         print_ptr := print_ptr +1;
      end
      else
      begin
         last_print := v ;
         displ[print_ptr] := v;
         print_ptr := print_ptr +1;
      end;
   end;

   function calc_score(iat, jat, v1, v2: integer):integer;
   var ipos,jpos: integer;
   begin
      ipos := v1 + iat;
      jpos := v2 + jat;

      result := matrix[integer(seq_array[seq1][ipos])][integer(seq_array[seq2][jpos])]
   end;

   function tracepath(tsb1, tsb2: integer):double;
   var
      c1,c2,i1,i2,i,k,pos,to_do,count: integer;
   begin
      to_do := print_ptr-1;
      i1 := tsb1;
      i2 := tsb2;

      pos := 0;
      count := 0;
      for i :=1 to to_do do
      begin

   //    if (debug>1) fprintf(stdout,'%d ',(pint)displ[i]);

         if displ[i] = 0 then
         begin
            c1 := integer(seq_array[seq1][i1]);
            c2 := integer(seq_array[seq2][i2]);
            if (c1 <> gap_pos1) and (c1 <> gap_pos2) and (c1 = c2) then
               count := count +1;
            i1 := i1 +1;
            i2 := i2 +1;
            pos := pos +1;
         end
         else
         begin
            k := displ[i];
            if k > 0 then
            begin
               i2 := i2 +k;
               pos := pos +k;
            end
            else
            begin
               i1 := i1 -k;
               pos := pos -k;
            end
         end;

         if Terminated then break;

      end;
      result := 100.0 * count;
   end;

   procedure forward_pass(ia, ib : PAnsiChar; n, m: integer);
   var
      i,j : integer;
      f,h,p,t : integer;
   begin
      maxscore := 0;
      se1 := 0;
      se2 := 0;
      for i := 0 to m do
      begin
         HH[i] := 0;
         DD[i] := -g;
      end;

      for i := 1 to n do
      begin
         h := 0;
         p := 0;
         f := -g;

         for j := 1 to m do
         begin
            f := f - gh;
            t := h - g - gh;
            if f < t then f := t;

            DD[j] := DD[j] - gh;
            t := HH[j] - g - gh;
            if DD[j] < t then DD[j] := t;

            h := p + matrix[integer(ia[i])][integer(ib[j])];
            if h < f then h := f;
            if h < DD[j] then h := DD[j];
            if h < 0 then h := 0;

            p := HH[j];
            HH[j] := h;

            if h > maxscore then
            begin
                 maxscore := h;
                 se1 := i;
                 se2 := j;
            end
         end;

         if i mod ProgressUnit = 0 then
           OnPairwiseProgress;

         if Terminated then break;

      end;
      if not Terminated and (n < ProgressNum) then
        for i := n+1 to ProgressNum do
          OnPairwiseProgress;
   end;

   procedure reverse_pass(ia, ib: PAnsiChar);
   var
      i,j,f,h,p,t,cost : integer;
   begin
      cost := 0;
      sb1 := 1;
      sb2 := 1;
      for i := se2 downto 1 do
      begin
         HH[i] := -1;
         DD[i] := -1;
      end;

      for i := se1 downto 1 do
      begin
         h := -1;
         f := -1;
         if i = se1 then
            p := 0
         else
            p := -1;

         for j := se2 downto 1 do
         begin
            f := f - gh;
            t := h - g - gh;
            if f < t then f := t;

            DD[j] := DD[j] - gh;
            t := HH[j] - g - gh;
            if DD[j] < t then DD[j] := t;

            h := p + matrix[integer(ia[i])][integer(ib[j])];
            if h < f then h := f;
            if h < DD[j] then h := DD[j];

            p := HH[j];
            HH[j] := h;

            if h > cost then
            begin
               cost := h;
               sb1 := i;
               sb2 := j;
               if cost >= maxscore then break;
            end;
         end;
         if cost >= maxscore then
           break;

         if i mod ProgressUnit = 0 then
           OnPairwiseProgress;

         if Terminated then break;

      end;
      if not Terminated and (se1 < ProgressNum) then
        for i := se1+1 to ProgressNum do
          OnPairwiseProgress;
   end;

   procedure del(k: integer);
   begin
      if last_print < 0 then
      begin
         displ[print_ptr-1] := displ[print_ptr-1] - k;
         last_print := displ[print_ptr-1];
      end
      else
      begin
         displ[print_ptr] := -k;
         last_print := -k;
         print_ptr := print_ptr +1;
      end;
   end;

   function diff(A, B, M, N, tb, te: integer):integer;
   var
      f, h, e, s, t : integer;        // static

      function tbgap(k: integer):integer;
      begin
        if k <= 0 then
          result := 0
        else
          result := tb +gh*k;
      end;

      function tegap(k: integer):integer;
      begin
        if k <= 0 then
          result := 0
        else
          result := te +gh*k;
      end;

      function local_diff(A, B, M, N, tb, te: integer; depth: integer):integer;
      var
         midi,midj,i,j,midh,gtype :integer;
      begin
        try
          if Terminated then exit;

          if N <= 0 then
          begin
             if M > 0 then
             begin
                del(M);
             end;

             result := -tbgap(M);
             exit;
          end;

          if M <= 1 then
          begin
             if M <= 0 then
             begin
                add(N);
                result := -tbgap(N);
                exit;
             end;

             midh := -(tb+gh) - tegap(N);
             h := -(te+gh) - tbgap(N);
             if h > midh then
                midh := h;

             midj := 0;
             for j := 1 to N do
             begin
                h := calc_score(1,j,A,B) - tegap(N-j) - tbgap(j-1);

                if h > midh then
                begin
                   midh := h;
                   midj := j;
                end
             end;

             if midj = 0 then
             begin
                del(1);
                add(N);
             end
             else
             begin
                if midj > 1 then
                   add(midj-1);
                last_print := 0;
                displ[print_ptr] := 0;
                print_ptr := print_ptr +1;
                if midj < N then
                   add(N-midj);
             end;
             result := midh;
             exit;
          end;

      //* Divide: Find optimum midpoint (midi,midj) of cost midh */

          midi := M div 2;
          HH[0] := 0;
          t := -tb;
          for j := 1 to N do
          begin
             t := t -gh;
             HH[j] := t;
             DD[j] := t -g;
          end;

          t := -tb;
          for i := 1 to midi do
          begin
             s := HH[0];
             t := t -gh;
             h := t;
             HH[0] := h;
             f := t -g;
             for j := 1 to N do
             begin
                h := h-g-gh;
                f := f-gh;
                if h > f then f := h;
                h := HH[j]-g-gh;
                e := DD[j]-gh;
                if h > e then e := h;
                h := s + calc_score(i,j,A,B);
                if f > h then h := f;
                if e > h then h := e;

                s := HH[j];
                HH[j] := h;
                DD[j] := e;
             end;
          end;

          DD[0] := HH[0];

          RR[N] := 0;
          t := -te;
          for j := N-1 downto 0 do
          begin
             t := t -gh;
             RR[j] := t;
             SS[j] := t -g;
          end;

          t := -te;
          for i := M-1 downto midi do
          begin
             s := RR[N];
             t := t -gh;
             h := t;
             RR[N] := h;
             f := t -g;

             for j := N-1 downto 0 do
             begin
                h := h-g-gh;
                f := f-gh;
                if h > f then f := h;
                h := RR[j]-g-gh;
                e := SS[j]-gh;
                if h > e then e := h;
                h := s + calc_score(i+1,j+1,A,B);
                if f > h then h := f;
                if e > h then h := e;

                s := RR[j];
                RR[j] := h;
                SS[j] := e;
             end
          end;

          SS[N] := RR[N];

          midh := HH[0]+RR[0];
          midj := 0;
          gtype := 1;
          for j := 0 to N do
          begin
             h := HH[j] + RR[j];
             if h >= midh then
                if (h > midh) or ((HH[j] <> DD[j]) and (RR[j] = SS[j])) then
                begin
                   midh := h;
                   midj := j;
                end;
          end;

          for j := N downto 0 do
          begin
             h := DD[j] + SS[j] + g;
             if h > midh then
             begin
                midh := h;
                midj := j;
                gtype := 2;
             end;
          end;

      //* Conquer recursively around midpoint  */

          if gtype = 1 then   //* Type 1 gaps  */
          begin
             local_diff(A,B,midi,midj,tb,g, depth+1);

             local_diff(A+midi,B+midj,M-midi,N-midj,g,te, depth+1);
          end
          else
          begin
             local_diff(A,B,midi-1,midj,tb,0, depth+1);

             del(2);
             local_diff(A+midi+1,B+midj,M-midi-1,N-midj,0,te, depth+1);
          end;

          result := midh;       //* Return the score of the best alignment */
        finally
          if depth = ProgressUnit then
            OnPairwiseProgress;
        end;
      end;

   begin
      result := local_diff(A, B, M, N, tb, te, 0);
   end;

var
   matptr : PArrayOfInt;
   mat_xref : PArrayOfInt;
   gscale, ghscale : double;
   si,sj,i,j,n,m,len1,len2,c : integer;
begin
  for i := Low(matrix) to High(matrix) do
    for j := Low(matrix[i]) to High(matrix[i]) do
      matrix[i,j] := 0;
  try
    displ := nil;
    HH    := nil;
    DD    := nil;
    RR    := nil;
    SS    := nil;
    {$IFNDEF VISUAL_BUILD}
    ShowRunStatusInfoStatic('status', 'Executing pairwise alignment');
   {$ENDIF}
  try
    GetMem(displ, (2*max_aln_length+1)*sizeof(integer));
    GetMem(HH, max_aln_length*sizeof(integer));
    GetMem(DD, max_aln_length*sizeof(integer));
    GetMem(RR, max_aln_length*sizeof(integer));
    GetMem(SS, max_aln_length*sizeof(integer));

    int_scale := 100;

    gscale := 1.0;
    ghscale := 1.0;

    if dnaflag then
    begin
 //     if debug>1 then fprintf(stdout,"matrix %s\n",pw_dnamtrxname);

       if pw_dnamtrxname = 'iub' then
       begin
          matptr := addr(swgapdnamt);
          mat_xref := addr(def_dna_xref);
       end
       else if pw_dnamtrxname = 'clustalw' then
       begin
          matptr := addr(clustalvdnamt);
          mat_xref := addr(def_dna_xref);
          gscale := 0.6667;
          ghscale := 0.751;
       end
       else
       begin
          matptr := addr(pw_userdnamat);
          mat_xref := addr(pw_dna_xref);
       end;
       if get_matrix(matptr, mat_xref, matrix, TRUE, int_scale) = 0 then
       begin
          result := -1;
          exit;
       end;

       matrix[0][4] := Trunc(transition_weight*matrix[0][0]);
       matrix[4][0] := Trunc(transition_weight*matrix[0][0]);
       matrix[2][11] := Trunc(transition_weight*matrix[0][0]);
       matrix[11][2] := Trunc(transition_weight*matrix[0][0]);
       matrix[2][12] := Trunc(transition_weight*matrix[0][0]);
       matrix[12][2] := Trunc(transition_weight*matrix[0][0]);
    end
    else
    begin
 //     if (debug>1) fprintf(stdout,'matrix %s\n',pw_mtrxname);
       if pw_mtrxname = 'blosum' then
       begin
          matptr := Addr(blosum30mt);
          mat_xref := Addr(def_aa_xref);
       end
       else if pw_mtrxname = 'pam' then
       begin
          matptr := Addr(pam350mt);
          mat_xref := Addr(def_aa_xref);
       end
       else if pw_mtrxname = 'gonnet' then
       begin
          matptr := Addr(gon250mt);
          int_scale := int_scale div 10;
          mat_xref := Addr(def_aa_xref);
       end
       else if pw_mtrxname = 'id' then
       begin
          matptr := Addr(idmat);
          mat_xref := Addr(def_aa_xref);
       end
       else
       begin
          matptr := Addr(pw_usermat);
          mat_xref := Addr(pw_aa_xref);
       end;

       if get_matrix(matptr, mat_xref, matrix, TRUE, int_scale) = 0 then
       begin
          result := -1;
          exit;
       end;
    end;
 ////

 ////
    for si := Max(0,istart) to Min(nseqs, iend)-1 do
    begin
       n := seqlen_array[si+1];
       len1 := 0;
       for i := 1 to n do
       begin
          c := integer(seq_array[si+1][i]);
          if (c <> gap_pos1) and (c <> gap_pos2) then
             len1 := len1 +1;
       end;

       for sj := Max(si, jstart)+1 to Min(nseqs,jend)-1 do
       begin
          m := seqlen_array[sj+1];
          if (n = 0) or (m = 0) then
          begin
            tmat[si][sj] := 1.0;
 	   tmat[sj][si] := 1.0;
 	   continue;
 	 end;
 	 len2 := 0;
 	 for i := 1 to m do
          begin
 	    c := integer(seq_array[sj+1][i]);
 	    if (c <> gap_pos1) and (c <> gap_pos2) then
                len2 := len2 +1;
 	 end;

          if dnaflag then
          begin
             g := Trunc(2*pw_go_penalty*int_scale*gscale);
             gh := Trunc(pw_ge_penalty*int_scale*ghscale);
          end
          else begin
             if mat_avscore <= 0 then
                g := Trunc(2*(pw_go_penalty + ln(MIN(n,m)))*int_scale)
             else
                g := Trunc(2*mat_avscore*(pw_go_penalty +ln((MIN(n,m))))*gscale);
             gh := Trunc(pw_ge_penalty*int_scale);
          end;

 //      if (debug>1) fprintf(stdout,'go %d ge %d\n',(pint)g,(pint)gh);


 //////////////////////
 // align the sequences
 //////////////////////

          seq1 := si+1;
          seq2 := sj+1;

  //        OnPairwiseProgress;

          ProgressUnit := n div ProgressNum;
          if ProgressUnit = 0 then
            ProgressUnit := 1;
          forward_pass(seq_array[seq1], seq_array[seq2], n, m);
          if Terminated then Break;
 //         OnPairwiseProgress;

          ProgressUnit := se1 div ProgressNum;
          if ProgressUnit = 0 then
            ProgressUnit := 1;
          reverse_pass(seq_array[seq1], seq_array[seq2]);
          if Terminated then Break;
 //         OnPairwiseProgress;

          last_print := 0;
 	 print_ptr := 1;

 // use Myers and Miller to align two sequences


          if nseqs < 10 then
            ProgressUnit := trunc(ln(ProgressNum)/ln(2))
          else
            ProgressUnit := 0;
          maxscore := diff(sb1-1, sb2-1, se1-sb1+1, se2-sb2+1, 0, 0);
          if Terminated then Break;
 //         OnPairwiseProgress;

 // calculate percentage residue identity

          mm_score := tracepath(sb1,sb2);

 	 if (len1 = 0) or (len2 = 0) then
             mm_score := 0
 	 else
 	    mm_score := mm_score/Min(len1,len2);

          tmat[si][sj] := (100.0 - mm_score)/100.0;
          tmat[sj][si] := (100.0 - mm_score)/100.0;

          if  debug > 1 then
          begin
 {
             fprintf(stdout,'Sequences (%d:%d) Aligned. Score: %d CompScore:  %d\n',
                             (pint)si+1,(pint)sj+1,
                             (pint)mm_score,
                             (pint)maxscore/(MIN(len1,len2)*100));
 }
          end
          else
          begin
 //            info('Sequences (%d:%d) Aligned. Score:  %d', (pint)si+1,(pint)sj+1, (pint)mm_score);
          end;

          if Terminated then Break;
       end;
    end;
  finally
    FreeMemAndNil(displ);
    FreeMemAndNil(HH);
    FreeMemAndNil(DD);
    FreeMemAndNil(RR);
    FreeMemAndNil(SS);
  end;
  if Terminated then
    result := 0
  else
    result := 1;
    {$IFDEF VISUAL_BUILD}
  if not Terminated then
    while FProgressDlg.PairwiseProgressBar.Position < FProgressDlg.PairwiseProgressBar.Max do
      OnPairwiseProgress;
      {$ENDIF}
  except
    on E:Exception do
    begin
       {$IFNDEF VISUAL_BUILD}
       error_nv('error in TClustalWThread.pairalign: ' + E.Message);
       {$ENDIF}
    end;
  end;
end;

function TClustalWThread.calc_weight(leaf: integer):integer;
var
  p: treeptr;
  weight: double;
begin
  weight := 0.0;

  p := olptr[leaf];
  while (p.parent <> nil) do
  begin
    weight := weight +p.dist / p.order;
    p := p.parent;
  end;

  weight := weight*100.0;

  result := Trunc(weight);
end;

procedure TClustalWThread.calc_seq_weights(first_seq, last_seq : integer; sweight : PArrayOfInt);
var
   i, nseqs, temp, sum : integer;
   weight : PArrayOfInt;
begin
   Weight := nil;
{
   If there are more than three sequences....
}
   nseqs := last_seq -first_seq;
   if (nseqs >= 2) and (distance_tree = TRUE) and (no_weights = FALSE) then
   begin
{
  Calculate sequence weights based on Phylip tree.
}
      GetMem(weight, (last_seq+1)*sizeof(integer));

      for i := first_seq to last_seq-1 do
         weight[i] := calc_weight(i);

{
  Normalise the weights, such that the sum of the weights := INT_SCALE_FACTOR
}

      sum := 0;
      for i := first_seq to last_seq-1 do
         sum := sum +weight[i];

      if sum = 0 then
      begin
         for i := first_seq to last_seq-1 do
            weight[i] := 1;
         sum := last_seq;
      end;

      for i := first_seq to last_seq-1 do
      begin
         sweight[i] := trunc((weight[i] * INT_SCALE_FACTOR)/sum);
         if sweight[i] < 1 then sweight[i] := 1;
      end;

      FreeMemAndNil(weight);

   end
   else
   begin
{
  Otherwise, use identity weights.
}
      temp := trunc(INT_SCALE_FACTOR / nseqs);
      for i:= first_seq to last_seq-1 do
         sweight[i] := temp;
   end;
end;

function TClustalWThread.malign(istart: integer):integer; //* full progressive alignment*/
const
  LEFT  : integer = 1;
  RIGHT : integer = 2;
var
  seq_tree,root : treeptr;
  groups : PArrayOfInt;
  ntotal : integer;

   procedure mark_group1(p: treeptr; groups: PArrayOfInt; n: integer);
   var
     i: integer;
   begin
     for i := 0 to n-1 do
       if olptr[i] = p then
         groups[i] := 1
       else
         groups[i] := 0;
   end;

   procedure mark_group2(p: treeptr; groups: PArrayOfInt; n: integer);
   var
     i: integer;
   begin
     for i := 0 to n-1 do
       if olptr[i] = p then
         groups[i] := 2
       else if groups[i] <> 0 then
         groups[i] := 1;
   end;

   procedure save_set(n: integer; groups: PArrayOfInt);
   var
     i: integer;
   begin
     for i := 0 to n-1 do
       sets[nsets+1][i+1] := groups[i];
     nsets := nsets +1;
   end;

   procedure group_seqs(p: treeptr; next_groups: PArrayOfInt; nseqs: integer);
   var
     i: integer;
     tmp_groups: PArrayOfInt;
   begin
     tmp_groups := nil;
     GetMem(tmp_groups, (nseqs+1)*sizeof(integer));
     for i := 0 to nseqs-1 do
       tmp_groups[i] := 0;

     if p.left <> nil then
     begin
       if p.left.leaf = false then
       begin
         group_seqs(p.left, next_groups, nseqs);
         for i := 0 to nseqs-1 do
           if next_groups[i] <> 0 then tmp_groups[i] := 1;
       end
       else
         mark_group1(p.left, tmp_groups, nseqs);
     end;

     if p.right <> nil then
     begin
       if p.right.leaf = false then
       begin
         group_seqs(p.right, next_groups, nseqs);
         for i := 0 to nseqs-1 do
           if next_groups[i] <> 0 then tmp_groups[i] := 2;
       end
       else
         mark_group2(p.right, tmp_groups, nseqs);

       save_set(nseqs, tmp_groups);
     end;

     for i := 0 to nseqs-1 do
       next_groups[i] := tmp_groups[i];

     FreeMemAndNil(tmp_groups);
   end;

   procedure create_sets(first_seq, last_seq : integer);
   var
     i, j, nseqs : integer;
   begin
     nsets := 0;
     nseqs := last_seq-first_seq;
     if nseqs >= 2 then
     begin
   {
     If there are more than three sequences....
   }
       GetMem(groups, (nseqs+1)*sizeof(integer));
       group_seqs(root, groups, nseqs);
       FreeMemAndNil(groups);
     end
     else
     begin
       GetMem(groups, (nseqs+1)*sizeof(integer));
       for i := 0 to nseqs-2 do
       begin
         for j := 0 to nseqs-1 do
           if j <= i then
             groups[j] := 1
           else if j = i+1 then
             groups[j] := 2
           else
             groups[j] := 0;
         save_set(nseqs, groups);
       end;
       FreeMemAndNil(groups);
     end;
   end;

   function avail():treeptr;
   var
      p : treeptr;
   begin
      GetMem(p, sizeof(stree));
      p.left := nil;
      p.right := nil;
      p.parent := nil;
      p.dist := 0.0;
      p.leaf := false;
      p.order := 0;
//      p.name := '';
      result := p;
   end;

   procedure set_info(p, parent: treeptr; pleaf: boolean; pname: AnsiString; pdist: double);
   begin
      p.parent := parent;
      p.leaf := pleaf;
      p.dist := pdist;
      p.order := 0;
//      p.name := pname;
      if p.leaf = true then
      begin
         p.left := nil;
         p.right := nil;
      end;
   end;

   function calc_root_mean(root: treeptr; var maxdist: double):double;
   var
     dist, lsum, rsum, lmean, rmean, diff: double;
     p: treeptr;
     nl, nr, direction, i: integer;
   begin
      lsum := 0.0;
      rsum := 0.0;
   {
      for each leaf, determine whether the leaf is left or right of the root.
   }
      dist := 0;
      maxdist := 0;
      nl := 0;
      nr := 0;
      for i := 0 to nseqs-1 do
      begin
        p := lptr[i];
        dist := 0.0;
        while (p.parent <> root) do
        begin
          dist := dist +p.dist;
          p := p.parent;
        end;
        if p = root.left then
          direction := LEFT
        else
          direction := RIGHT;
        dist := dist +p.dist;

        if direction = LEFT then
        begin
          lsum := lsum +dist;
          nl := nl +1;
        end
        else
        begin
          rsum := rsum +dist;
          nr := nr +1;
        end;
        if dist > maxdist then
          maxdist := dist;
      end;

      lmean := lsum / nl;
      rmean := rsum / nr;

      diff := lmean - rmean;
      result := diff;
   end;

   function calc_mean(nptr: treeptr; var maxdist: double; nseqs: integer):double;
   var
     dist, lsum, rsum, lmean, rmean, diff: double;
     p: treeptr;
     path2root : PArrayOftreeptr;
     dist2node : PArrayOfDouble;
     depth, i, j, n, nl, nr, direction : integer;
     found : boolean;
   begin
     dist2node := nil;
     path2root := nil;
     lsum := 0.0;
     rsum := 0.0;
     depth := 0;
     n := 0;

     GetMem(path2root, nseqs*sizeof(treeptr));
     GetMem(dist2node, nseqs*sizeof(double));
   {
      determine all nodes between the selected node and the root;
   }
     depth := 0;
     maxdist := 0;
     dist := 0;
     nl := 0;
     nr := 0;
     p := nptr;
     while (p <> nil) do
     begin
       path2root[depth] := p;
       dist := dist +p.dist;
       dist2node[depth] := dist;
       p := p.parent;
       depth := depth +1;
     end;

   {
      *nl := *nr := 0;
      for each leaf, determine whether the leaf is left or right of the node.
      (RIGHT := descendant, LEFT := not descendant)
   }
     for i := 0 to nseqs-1 do
     begin
       p := lptr[i];
       if p = nptr then
       begin
         direction := RIGHT;
         dist := 0.0;
       end
       else
       begin
         direction := LEFT;
         dist := 0.0;
   {
      find the common ancestor.
   }
         found := FALSE;
         n := 0;
         while (found = FALSE) and (p.parent <> nil) do
         begin
           for j :=0 to depth-1 do
             if p.parent = path2root[j] then
             begin
               found := TRUE;
               n := j;
             end;
           dist := dist +p.dist;
           p := p.parent;
         end;
         if p = nptr then
           direction := RIGHT;
       end;

       if direction = LEFT then
       begin
         lsum := lsum +dist;
         lsum := lsum +dist2node[n-1];
         nl := nl +1;
       end
       else
       begin
         rsum := rsum +dist;
         nr := nr +1;
       end;

       if dist > maxdist then
         maxdist := dist;
     end;

     FreeMemAndNil(dist2node);
     FreeMemAndNil(path2root);

     lmean := lsum / nl;
     rmean := rsum / nr;

     diff := lmean - rmean;
     result := diff;
   end;

   function insert_root(p: treeptr; diff: double):treeptr;
   var
     newp, prev, q, t : treeptr;
     dist, prevdist,td : double;
   begin
     prev := nil;
     q := nil;
     t := nil;
     
     newp := avail();

     t := p.parent;
     prevdist := t.dist;

     p.parent := newp;

     dist := p.dist;

     p.dist := diff / 2;
     if p.dist < 0.0 then
       p.dist := 0.0;
     if p.dist > dist then
       p.dist := dist;

     t.dist := dist - p.dist;

     newp.left := t;
     newp.right := p;
     newp.parent := nil;
     newp.dist := 0.0;
     newp.leaf := false;

     if t.left = p then
       t.left := t.parent
     else
       t.right := t.parent;

     prev := t;
     q := t.parent;

     t.parent := newp;

     while (q <> nil) do
     begin
       if q.left = prev then
       begin
         q.left := q.parent;
         q.parent := prev;
         td := q.dist;
         q.dist := prevdist;
         prevdist := td;
         prev := q;
         q := q.left;
       end
       else
       begin
         q.right := q.parent;
         q.parent := prev;
         td := q.dist;
         q.dist := prevdist;
         prevdist := td;
         prev := q;
         q := q.right;
       end;
     end;

   {
     remove the old root node
   }
     q := prev;
     if q.left = nil then
     begin
       dist := q.dist;
       q := q.right;
       q.dist := q.dist +dist;
       q.parent := prev.parent;
       if prev.parent.left = prev then
         prev.parent.left := q
       else
         prev.parent.right := q;
       prev.right := nil;
     end
     else
     begin
       dist := q.dist;
       q := q.left;
       q.dist := q.dist +dist;
       q.parent := prev.parent;
       if prev.parent.left = prev then
         prev.parent.left := q
       else
         prev.parent.right := q;
       prev.left := nil;
     end;
     FreeMemAndNil(prev);
     result := newp;
   end;

   function reroot(ptree: treeptr; nseqs: integer):treeptr;
   var
     p, rootnode, rootptr: treeptr;
     diff, mindiff, mindepth: double;
     maxdist: Double = 0;
     i: integer;
     first: boolean;
   begin
     mindiff := 0.0;
     mindepth := 1.0;
     first := TRUE;

   {
     find the difference between the means of leaf->node
     distances on the left and on the right of each node
   }
     rootptr := ptree;
     for i := 0 to  ntotal-1 do
     begin
       p := ptrs[i];
       if p.parent = nil then
          diff := calc_root_mean(p, maxdist)
       else
          diff := calc_mean(p, maxdist, nseqs);

       if (diff = 0) or ((diff > 0) and (diff < 2 * p.dist)) then
       begin
         if (maxdist < mindepth) or (first = TRUE) then
         begin
           first := FALSE;
           rootptr := p;
           mindepth := maxdist;
           mindiff := diff;
         end;
       end;

     end;

   {
     insert a new node as the ancestor of the node which produces the shallowest
     tree.
   }
     if rootptr = ptree then
     begin
       mindiff := rootptr.left.dist + rootptr.right.dist;
       rootptr := rootptr.right;
     end;
     rootnode := insert_root(rootptr, mindiff);

     diff := calc_root_mean(rootnode, maxdist);

     result := rootnode;
   end;

   procedure order_nodes;
   var
     i : integer;
     p : treeptr;
   begin
     for i := 0 to nseqs-1 do
     begin
       p := lptr[i];
       while p <> nil do
       begin
         p.order := p.order +1;
         p := p.parent;
       end;
     end;
   end;

   procedure parse_tree;
   var
     i : integer;
   begin
     distance_tree := TRUE;

     GetMem(nptr, (2*nseqs-1)*sizeof(treeptr));
     GetMem(ptrs, (2*nseqs-1)*sizeof(treeptr));
     GetMem(lptr, nseqs*sizeof(treeptr));
     GetMem(olptr, nseqs*sizeof(treeptr));

     ntotal := 2*nseqs -1;
     for i := 0 to ntotal-1 do
       ptrs[i] := avail;

     for i := 0 to nseqs-2 do
     begin
       ptrs[i+nseqs].left := ptrs[FTree.Node[i].des1];
       ptrs[FTree.Node[i].des1].parent := ptrs[i+nseqs];
       if FTree.IsBLen then
         ptrs[FTree.Node[i].des1].dist := FTree.BLen[FTree.Node[i].des1]
       else
         ptrs[FTree.Node[i].des1].dist := 0.0;
       if FTree.Node[i].des1 < nseqs then
       begin
         ptrs[FTree.Node[i].des1].leaf := true;
         ptrs[FTree.Node[i].des1].left := nil;
         ptrs[FTree.Node[i].des1].right := nil;
       end
       else
         ptrs[FTree.Node[i].des1].leaf := false;

       ptrs[i+nseqs].right := ptrs[FTree.Node[i].des2];
       ptrs[FTree.Node[i].des2].parent := ptrs[i+nseqs];
       if FTree.IsBLen then
         ptrs[FTree.Node[i].des2].dist := FTree.BLen[FTree.Node[i].des2]
       else
         ptrs[FTree.Node[i].des2].dist := 0.0;
       if FTree.Node[i].des2 < nseqs then
       begin
         ptrs[FTree.Node[i].des2].leaf := true;
         ptrs[FTree.Node[i].des2].left := nil;
         ptrs[FTree.Node[i].des2].right := nil;
       end
       else
         ptrs[FTree.Node[i].des2].leaf := false;
     end;
     for i := 0 to 2*nseqs-2 do
     begin
       nptr[i] := ptrs[i];
       if ptrs[i].parent = nil then
         seq_tree := ptrs[i];
     end;
     for i := 0 to nseqs-1 do
     begin
       lptr[i] := ptrs[i];
       olptr[i] := ptrs[i];
     end;

     if nseqs = 2 then
       root := seq_tree
     else
       root := reroot(seq_tree, nseqs);

     order_nodes;
   end;

   function calc_similarities(nseqs : integer):integer;
   var
     depth, i,j, k, n: integer;
     found : boolean;
     nerrs: integer;
     seq1,seq2 : PArrayOfInt;
     p : treeptr;
     path2root : PArrayOftreeptr;
     dist : double;
     dist2node, bad_dist : PArrayOfDouble;
     dmat : PDistanceMatrix;
//   char err_mess[1024],err1[MAXLINE],reply[MAXLINE];
   begin
     seq1      := nil;
     seq2      := nil;
     p         := nil;
     path2root := nil;
     dist2node := nil;
     bad_dist  := nil;
     dmat      := nil;

     depth := 0;

     GetMem(path2root, nseqs*sizeof(treeptr));
     GetMem(dist2node, nseqs*sizeof(double));
     GetMem(dmat, nseqs*sizeof(pointer));
     for i := 0 to nseqs-1 do
     begin
       GetMem(dmat[i], nseqs*sizeof(double));
       // do NOT set dmat[i] := nil here.  That breaks Clustal!
     end;
     GetMem(seq1, nseqs*sizeof(integer));
     GetMem(seq2, nseqs*sizeof(integer));
     GetMem(bad_dist, nseqs*sizeof(double));


     result := 0;
     if nseqs >= 2 then
     begin
       result := 1;
   {
     for each leaf, determine all nodes between the leaf and the root;
   }
       for i := 0 to nseqs-1 do
       begin
         depth := 0;
         dist := 0;
         p := olptr[i];
         while (p <> nil) do
         begin
           path2root[depth] := p;
           dist := dist +p.dist;
           dist2node[depth] := dist;
           p := p.parent;
           depth := depth +1;
         end;

   {
     for each pair....
   }
         for j := 0 to i-1 do
         begin
           p := olptr[j];
           dist := 0.0;
   {
     find the common ancestor.
   }
           found := FALSE;
           n := 0;
           while ((found = FALSE) and (p.parent <> nil)) do
           begin
             for k := 0 to depth-1 do
               if p.parent = path2root[k] then
               begin
                 found := TRUE;
                 n := k;
               end;
             dist := dist +p.dist;
             p := p.parent;
           end;

           dmat[i][j] := dist + dist2node[n-1];
         end;
       end;

       nerrs := 0;
       for i :=0 to nseqs-1 do
       begin
         dmat[i][i] := 0.0;
         for j := 0 to i-1 do
         begin
           if dmat[i][j] < 0.01 then
             dmat[i][j] := 0.01;
           if dmat[i][j] > 1.0 then
           begin
             if dmat[i][j] > 1.1 then
             begin
               if nerrs < nseqs then
               begin
                 seq1[nerrs] := i;
                 seq2[nerrs] := j;
                 bad_dist[nerrs] := dmat[i][j];
               end;
               nerrs := nerrs +1;
             end;
             dmat[i][j] := 1.0;
           end;
         end;
       end;
       if nerrs > 0 then
       begin
//         if MessageDlg('Some sequences are too divergent to be aligned.', mtError, [mbIgnore,mbAbort], 0) = mrAbort then
//           result := 0;

         Err := true;
         ErrMessage := 'Some sequences are too divergent to be aligned.';
         Synchronize(DoOnError);
         if Err then
           result := 0;

{
//            strcpy(err_mess,"The following sequences are too divergent to be aligned:\n");
           for i:=0;i<nerrs and i<5;i++)
           begin
           	sprintf(err1,"           %s and %s (distance %1.3f)\n",
           	                        names[seq1[i]+1],
     	   				names[seq2[i]+1],bad_dist[i]);
           	strcat(err_mess,err1);
           end;
     	  strcat(err_mess,"(All distances should be between 0.0 and 1.0)\n");
     	  strcat(err_mess,"This may not be fatal but you have been warnednot \n");
            strcat(err_mess,"SUGGESTION: Remove one or more problem sequences and try again");
            if interactive then
            	    (*reply):=prompt_for_yes_no(err_mess,"Continue ");
            else (*reply) := 'y';
            if (*reply <> 'y') and (*reply <> 'Y'))
                   return((sint)0);
}
       end;
     end
     else
       for i := 0 to nseqs-1 do
         for j :=0 to i-1 do
           dmat[i][j] := tmat[i][j];

     FreeMemAndNil(path2root);
     FreeMemAndNil(dist2node);
     for i :=0 to nseqs-1 do
     begin
       tmat[i][i] := 0.0;
       for j :=0 to i-1 do
       begin
          tmat[i][j] := 100.0 - dmat[i][j]*100.0;
          tmat[j][i] := tmat[i][j];
       end;
     end;

     if dmat <> nil then
     begin
       for i := 0 to nseqs-1 do
         FreeMemAndNil(dmat[i]);
       FreeMemAndNil(dmat);
     end;
     FreeMemAndNil(seq1);
     FreeMemAndNil(seq2);
     FreeMemAndNil(bad_dist);
   end;

   procedure clear_tree_nodes(p : treeptr);
   begin
     if p = nil then
       p := root;
     if p.left <> nil then
     begin
       clear_tree_nodes(p.left);
     end;
     if p.right <> nil then
     begin
       clear_tree_nodes(p.right);
     end;
     p.left := nil;
     p.right := nil;
     FreeMemAndNil(p);
   end;

   procedure clear_tree(p : treeptr);
   begin
     clear_tree_nodes(p);

     FreeMemAndNil(nptr);
     FreeMemAndNil(ptrs);
     FreeMemAndNil(lptr);
     FreeMemAndNil(olptr);
   end;

var
   aligned, group, maxid, tree_weight : PArrayOfInt;
   ix,max,sum,i,j,s,iseq,status,entries : integer;
begin
//   OnMultipleProgress;
  {$IFNDEF VISUAL_BUILD}
  ShowRunStatusInfoStatic('status', #13#10 + 'Executing multiple sequence alignment');
  {$ENDIF}
   groups      := nil;
   group       := nil;
   aligned     := nil;
   maxid       := nil;
   tree_weight := nil;


   if nseqs > 120 then
      ProgressUnit := 0
   else
      ProgressUnit := trunc(ln(240 div (nseqs-1))/ln(2));

   ntotal := 0;
   i := 0;
   j := 0;
   s := 0;
   iseq := 0;

//   info("Start of Multiple Alignment");

//* get the phylogenetic tree from *.ph */

   if nseqs >= 2 then
     parse_tree;
{
   begin
      status := read_tree(phylip_name, 0, nseqs);
      if status = 0 then begin
         result := 0;
         exit;
      end;
   end;
}
//* calculate sequence weights according to branch lengths of the tree -
//  weights in global variable seq_weight normalised to sum to 100 */

   calc_seq_weights(0, nseqs, seq_weight);

//* recalculate tmat matrix as percent similarity matrix */

   status := calc_similarities(nseqs);
   if status = 0 then
   begin
      result := 0;
      exit;
   end;

//* for each sequence, find the most closely related sequence */

   GetMem(maxid, (nseqs+1)*sizeof(integer));
   for i :=1 to nseqs do
   begin
      maxid[i] := -1;
      for j := 1 to nseqs do
         if (j <> i) and (maxid[i] < tmat[i-1][j-1]) then maxid[i] := trunc(tmat[i-1][j-1]);
   end;

//* group the sequences according to their relative divergence */

   try
      if istart = 0 then
      try
         GetMem(sets, (nseqs+1)*sizeof(pointer));
         for i :=0 to nseqs do
            GetMem(sets[i], (nseqs+1)*sizeof(integer));

         create_sets(0,nseqs);
//    info("There are %d groups",(pint)nsets);

//* clear the memory used for the phylogenetic tree */

         if nseqs >= 2 then clear_tree(nil);

//* start the multiple alignments.........  */

//    info("Aligning...");

//* first pass, align closely related sequences first.... */

         ix := 0;
         GetMem(aligned, (nseqs+1)*sizeof(integer));
         for i :=0 to nseqs do
            aligned[i] := 0;
         for s := 1 to nsets do
         begin
         {$IFNDEF VISUAL_BUILD}
          ShowProgressIncrementStatic;
         {$ENDIF}
            entries := 0;
            for i:=1 to nseqs do
            begin
               if (sets[s][i] <> 0) and (maxid[i] > divergence_cutoff) then
               begin
                  entries := entries +1;
                  if aligned[i] = 0 then
                  begin
                     if output_order = INPUT then
                     begin
                        ix := ix +1;
                        output_index[i] := i;
                     end
                     else
                     begin
                        ix := ix +1;
                        output_index[ix] := i;
                     end;
                     aligned[i] := 1;
                  end;
               end;
            end;

            if entries > 0 then
               FScore := prfalign(sets[s], aligned)
            else
               FScore := 0;

//* negative score means fatal error... exit now!  */

            if Terminated then break;

            if score < 0 then
            begin
               result := -1;
               exit;
            end;
//         if (entries > 0) and (score > 0) then
//            info("Group %d: Sequences:%4d      Score:%d", (pint)s,(pint)entries,(pint)score);
//         else
//          info("Group %d:                     Delayed", (pint)s);

         end;

      finally
         if sets <> nil then
         begin
           for i := 0 to nseqs do
              FreeMemAndNil(sets[i]);
           FreeMemAndNil(sets);
         end;
      end
      else
      begin
//* clear the memory used for the phylogenetic tree */

         if nseqs >= 2 then
            clear_tree(nil);

         GetMem(aligned, (nseqs+1)*sizeof(integer));
         ix := 0;
         for i := 1 to istart+1 do
         begin
            aligned[i] := 1;
            ix := ix +1;
            output_index[i] := i;
         end;
         for i := istart+2 to nseqs do
            aligned[i] := 0;
      end;

      if Terminated then exit;

//* second pass - align remaining, more divergent sequences..... */

//* if not all sequences were aligned, for each unaligned sequence,
//  find it's closest pair amongst the aligned sequences.  */

      GetMem(group, (nseqs+1)*sizeof(integer));
      GetMem(tree_weight, (nseqs)*sizeof(integer));
      for i := 0 to nseqs-1 do
      	 tree_weight[i] := seq_weight[i];

//* if we haven't aligned any sequences, in the first pass - align the
//  two most closely related sequences now */

      if ix = 0 then
      begin
         max := -1;
         iseq := 0;
         for i := 1 to nseqs do
         begin
            for j := i+1 to nseqs do
            begin
               if max < Trunc(tmat[i-1][j-1]) then
      	    begin
                  max := Trunc(tmat[i-1][j-1]);
                  iseq := i;
               end;
            end;
         end;
         aligned[iseq] := 1;
         if output_order = INPUT then
         begin
            ix := ix +1;
            output_index[iseq] := iseq;
         end
         else
         begin
            ix := ix +1;
            output_index[ix] := iseq;
         end;
      end;

      while ix < nseqs do
      begin
         for i :=1 to nseqs do
         begin
            if aligned[i] = 0 then
            begin
               maxid[i] := -1;
               for j := 1 to nseqs do
                  if (maxid[i] < Trunc(tmat[i-1][j-1])) and (aligned[j] <> 0) then
                     maxid[i] := Trunc(tmat[i-1][j-1]);
            end;
         end;
//* find the most closely related sequence to those already aligned */

         max := -1;
         iseq := 0;
         for i:=1 to nseqs do
         begin
            if (aligned[i] = 0) and (maxid[i] > max) then
            begin
               max := maxid[i];
               iseq := i;
            end;
         end;

//* align this sequence to the existing alignment */
//* weight sequences with percent identity with profile*/
//* OR...., multiply sequence weights from tree by percent identity with new sequence */

         if no_weights = FALSE then
         begin
            for j:=0 to nseqs-1 do
               if aligned[j+1] <> 0 then
                  seq_weight[j] := Trunc(tree_weight[j]*tmat[j][iseq-1]);

//  Normalise the weights, such that the sum of the weights = INT_SCALE_FACTOR

            sum := 0;
            for j :=0 to nseqs-1 do
               if aligned[j+1] <> 0 then
                  sum := sum +seq_weight[j];
            if sum = 0 then
            begin
               for j :=0 to nseqs-1 do
                  seq_weight[j] := 1;
               sum := nseqs;
            end;
            for j := 0 to nseqs -1 do
               if aligned[j+1] <> 0 then
               begin
                  seq_weight[j] := Trunc((seq_weight[j]*INT_SCALE_FACTOR) / sum);
                  if seq_weight[j] < 1 then
                    seq_weight[j] := 1;
               end;
         end;

         entries := 0;
         for j := 1 to nseqs do
            if aligned[j] <> 0 then
            begin
               group[j] := 1;
               entries := entries +1;
            end
            else if iseq = j then
            begin
               group[j] := 2;
               entries := entries +1;
            end;
         aligned[iseq] := 1;

         FScore := prfalign(group, aligned);
//      info("Sequence:%d     Score:%d",(pint)iseq,(pint)score);

         if Terminated then break;

         if output_order = INPUT then
         begin
            ix := ix +1;
            output_index[iseq] := iseq;
         end
         else
         begin
            ix := ix +1;
            output_index[ix] := iseq;
         end;
      end;

      if Terminated then exit;

      aln_score();

//* make the rest (output stuff) into routine clustal_out in file amenu.c */

   finally
      if Terminated then
        result := 0
      else
        result := nseqs;

      if assigned(group) then
         FreeMemAndNil(group);
      if assigned(aligned) then
         FreeMemAndNil(aligned);
      if assigned(maxid) then
         FreeMemAndNil(maxid);
      if assigned(tree_weight) then
         FreeMemAndNil(tree_weight);
   end;
end;


procedure TClustalWThread.aln_score;

   function count_gaps(s1, s2, l: integer):integer;
   var
     i, g, qq, rr: integer;
     Q, R: PArrayOfInt;
   begin
     Q := nil;
     R := nil;
     GetMem(Q, (l+2)*sizeof(integer));
     GetMem(R, (l+2)*sizeof(integer));

     Q[0] := 0;
     R[0] := 0;
     g := 0;

     for i := 1 to l-1 do
     begin
       if integer(seq_array[s1][i]) > max_aa then
         qq := 1
       else
         qq := 0;
       if integer(seq_array[s2][i]) > max_aa then
         rr := 1
       else
         rr := 0;

       if ((Q[i-1] <= R[i-1]) and (qq <> 0) and (1-rr <> 0)) or
          ((Q[i-1] >= R[i-1]) and (1-qq <> 0) and (rr <> 0)) then
           g := g +1;
       if qq <> 0 then
         Q[i] := Q[i-1]+1
       else
         Q[i] := 0;

       if rr <> 0 then
         R[i] := R[i-1]+1
       else
         R[i] := 0;
     end;

     FreeMemAndNil(Q);
     FreeMemAndNil(R);

     result := g;
   end;

var
  mat_xref, matptr: PArrayOfInt;
  maxres,s1,s2,c1,c2,ngaps,i,j,l1,l2: integer;
  matrix: TMatrix;
  score: double;
begin
  for i := Low(matrix) to High(matrix) do
    for j := Low(matrix[i]) to High(matrix[i]) do
      matrix[i,j] := 0;
{ calculate an overall score for the alignment by summing the
scores for each pairwise alignment }

  matptr := Addr(blosum45mt);
  mat_xref := Addr(def_aa_xref);
  maxres := get_matrix(matptr, mat_xref, matrix, TRUE, 100);
  if maxres = 0 then
  begin
    {
       fprintf(stdout,"Error: matrix blosum30 not found\n");
       return;
    }
  end;

  score := 0;
  for s1 := 1 to nseqs do
  begin
    for s2 := 1 to s1-1 do
    begin
      l1 := seqlen_array[s1];
      l2 := seqlen_array[s2];
      i := 1;
      while (i<l1) and (i<l2) do
      begin
        c1 := integer(seq_array[s1][i]);
        c2 := integer(seq_array[s2][i]);
        if (c1>=0) and (c1<=max_aa) and (c2>=0) and (c2<=max_aa) then
            score := score +matrix[c1][c2];
        i := i +1;
      end;

        ngaps := count_gaps(s1, s2, l1);

        score := score -Trunc(100*gap_open*ngaps);

    end;
  end;

  FScore := trunc(score/100);

//  info("Alignment Score %d", (pint)score);

end;

function TClustalWThread.prfalign(group, aligned : PArrayOfInt):integer;
var
   print_ptr,last_print : integer;
   displ : PArrayOfInt;

   alignment: PArrayOfPChar;
   aln_len, aln_weight : PArrayOfInt;
   aln_path1, aln_path2 : PAnsiChar;
   alignment_len : integer = -1;
   profile1, profile2 : PIntMatrix;
   HH, DD, RR, SS, gS : PArrayOfInt;
   matrix: TMatrix;
   nseqs1, nseqs2 : integer;
   prf_length1, prf_length2 : integer;
   gaps : PArrayOfInt;
   gapcoef1,gapcoef2 : integer;
   lencoef1,lencoef2 : integer;
   switch_profiles : boolean;

   procedure padd(k : integer);
   begin
      if last_print < 0 then
      begin
         displ[print_ptr-1] := k;
         displ[print_ptr] := last_print;
         print_ptr := print_ptr +1;
      end
      else
      begin
         displ[print_ptr] := k;
         last_print := displ[print_ptr];
         print_ptr := print_ptr +1;      // last_print = displ[print_ptr++] = k;
      end;
   end;

   procedure pdel(k : integer);
   begin
      if last_print < 0 then
      begin
         displ[print_ptr-1] := displ[print_ptr-1] -k;
         last_print := displ[print_ptr-1];
      end
      else
      begin
         displ[print_ptr] := -(k);
         last_print := displ[print_ptr];
         print_ptr := print_ptr +1;      // last_print = displ[print_ptr++] = -(k);
      end;
   end;

   //* calculate the score for a gap of length k, at residues A[i] and B[j]  */

   function gap_penalty1(i, j, k : integer):integer;
   var
      ix, gp, g, h : integer;
   begin
      h := 0;

      if k <= 0 then
      begin
         result := 0;
         exit;
      end;
      if (not endgappenalties) and ((i = 0) or (i = prf_length1)) then
      begin
         result := 0;
         exit;
      end;

      g := profile2[j][GAPCOL] + profile1[i][GAPCOL];
      for ix := 0 to Min(k, prf_length2-j)-1 do
         h := profile2[ix+j][LENCOL];

      gp := g + h * k;
      result := gp;
   end;

   //* calculate the score for a gap of length k, at residues A[i] and B[j]  */

   function gap_penalty2(i, j, k: integer):integer;
   var
      ix, gp, g, h : integer;
   begin
      h := 0;

      if k <= 0 then
      begin
         result := 0;
         exit;
      end;
      if (not endgappenalties) and ((j = 0) or (j = prf_length2)) then
      begin
         result := 0;
         exit;
      end;

      g := profile1[i][GAPCOL] + profile2[j][GAPCOL];
      for ix := 0 to Min(k, prf_length1-i)-1 do
         h := profile1[ix+i][LENCOL];

      gp := g + h * k;
      result := gp;
   end;

   function prfscore(n, m: integer):integer;
   var
      ix, score: integer;
   begin
      score := 0;
      for ix := 0 to max_aa do
         score := score +(profile1[n][ix] * profile2[m][ix]);
      score := score +(profile1[n][gap_pos1] * profile2[m][gap_pos1]);
      score := score +(profile1[n][gap_pos2] * profile2[m][gap_pos2]);
      result := score div 10;
   end;

   procedure palign;
   begin
      last_print := 0;
      displ[print_ptr] := 0;
      print_ptr := print_ptr +1;
   end;

   //* calculate the score for opening a gap at residues A[i] and B[j]       */

   function open_penalty1(i, j: integer):integer;
   var
      g : integer;
   begin
      if (not endgappenalties) and ((i = 0) or (i = prf_length1)) then
      begin
         result := 0;
         exit;
      end;

      g := profile2[j][GAPCOL] + profile1[i][GAPCOL];
      result := g;
   end;

   //* calculate the score for extending an existing gap at A[i] and B[j]    */

   function ext_penalty1(i, j: integer):integer;
   var
      h : integer;
   begin
      if (not endgappenalties) and ((i = 0) or (i = prf_length1)) then
      begin
         result := 0;
         exit;
      end;

      h := profile2[j][LENCOL];
      result := h;
   end;

   //* calculate the score for opening a gap at residues A[i] and B[j]       */

   function open_penalty2(i, j: integer):integer;
   var
      g : integer;
   begin
      if (not endgappenalties) and ((j = 0) or (j = prf_length2)) then
      begin
         result := 0;
         exit;
      end;

      g := profile1[i][GAPCOL] + profile2[j][GAPCOL];
      result := g;
   end;

   //* calculate the score for extending an existing gap at A[i] and B[j]    */

   function ext_penalty2(i, j: integer):integer;
   var
      h : integer;
   begin
      if (not endgappenalties) and ((j = 0) or (j = prf_length2)) then
      begin
         result := 0;
         exit;
      end;

      h := profile1[i][LENCOL];
      result := h;
   end;

   function pdiff(A, B, M, N, go1, go2, depth: integer):integer;
   var
      midi,midj,midh,gtype : integer;
      t, tl, g, h : integer;
      i,j: integer;
      k, f, e, s : integer;
   begin
      if Terminated then exit;

      try
      //* Boundary cases: M <= 1 or N == 0 */
      {
      if debug>2) fprintf(stdout,"A %d B %d M %d N %d midi %d go1 %d go2 %d\n", (pint)A,(pint)B,(pint)M,(pint)N,(pint)M/2,(pint)go1,(pint)go2);
      }
      //* if sequence B is empty....   *//

         if N <= 0 then
         begin

      //* if sequence A is not empty....                                        */

            if M > 0 then
            begin

      //* delete residues A[1] to A[M]                                          */

                pdel(M);
            end;
            result := -gap_penalty1(A,B,M);
            exit;
         end;

      //* if sequence A is empty....                                            */

         if M <= 1 then
         begin
            if M <= 0 then
            begin

      //* insert residues B[1] to B[N]                                          */

                padd(N);
                result := -gap_penalty2(A,B,N);
                exit;
            end;

      //* if sequence A has just one residue....                                */

            if go1 = 0 then
               midh :=  -gap_penalty1(A+1,B+1,N)
            else
               midh :=  -gap_penalty2(A+1,B,1)-gap_penalty1(A+1,B+1,N);
            midj := 0;
            for j := 1 to N do
            begin
               k := -gap_penalty1(A,B+1,j-1) + prfscore(A+1,B+j) -gap_penalty1(A+1,B+j+1,N-j);
               if k > midh then
               begin
                  midh := k;
                  midj := j;
               end;
            end;

            if midj = 0 then
            begin
               padd(N);
               pdel(1);
            end
            else
            begin
               if midj > 1 then padd(midj-1);
               palign();
               if midj < N then padd(N-midj);
            end;
            result := midh;
            exit;
         end;


     //* Divide sequence A in half: midi */

         midi := M div 2;

         if depth = 0 then
           ProgressUnit := midi div (4*ProgressNum)
         else if depth = 1 then
           ProgressUnit := midi div ProgressNum;
         if ProgressUnit = 0 then
           ProgressUnit := 1;

      //* In a forward phase, calculate all HH[j] and HH[j] */

         HH[0] := 0;
         t := -open_penalty1(A,B+1);
         tl := -ext_penalty1(A,B+1);
         for j := 1 to N do
         begin
            t := t+tl;
            HH[j] := t;
            DD[j] := t-open_penalty2(A+1,B+j);
         end;

         if go1 = 0 then
            t := 0
         else
            t := -open_penalty2(A+1,B);
         tl := -ext_penalty2(A+1,B);
         for i := 1 to midi do
         begin
            s := HH[0];
            t := t+tl;
            k := t;
            HH[0] := k;
            f := t-open_penalty1(A+i,B+1);

            for j := 1 to N do
            begin
               g := open_penalty1(A+i,B+j);
               h := ext_penalty1(A+i,B+j);
               k := k-g-h;
               f := f-h;
               if k > f then f := k;
               g := open_penalty2(A+i,B+j);
               h := ext_penalty2(A+i,B+j);

               k := HH[j]-g-h;
               e := DD[j]-h;
               if k > e then
                  e := k;
               k := s + prfscore(A+i, B+j);
               if f > k then k := f;
               if e > k then k := e;

               s := HH[j];
               HH[j] := k;
               DD[j] := e;
            end;

            if depth <= 1 then
              if i mod ProgressUnit = 0 then
                OnMultipleProgress;

            if Terminated then exit;
         end;

         DD[0]:=HH[0];

      //* In a reverse phase, calculate all RR[j] and SS[j] */

         RR[N]:=0;
         tl := 0;
         for j:= N-1 downto 0 do
         begin
            g := -open_penalty1(A+M,B+j+1);
            tl := tl -ext_penalty1(A+M,B+j+1);
            RR[j] := g+tl;
            SS[j] := RR[j] -open_penalty2(A+M,B+j);
            gS[j] := open_penalty2(A+M,B+j);
         end;

         tl := 0;
         for i := M-1 downto midi do
         begin
            s := RR[N];
            if go2 = 0 then
               g := 0
            else
               g := -open_penalty2(A+i+1,B+N);
            tl := tl -ext_penalty2(A+i+1,B+N);
            RR[N] := g+tl;
            k := g+tl;
            t := open_penalty1(A+i,B+N);
            f := RR[N]-t;

            for j := N-1 downto 0 do
            begin
               g := open_penalty1(A+i,B+j+1);
               h := ext_penalty1(A+i,B+j+1);
               k := k-g-h;
               f := f-h-g+t;
               if k > f then f := k;
               t := g;
               g := open_penalty2(A+i+1,B+j);
               h := ext_penalty2(A+i+1,B+j);
               k := RR[j]-g-h;
               if i = M-1 then
                  e := SS[j]-h
               else
               begin
      	       e := SS[j]-h-g +open_penalty2(A+i+2,B+j);
                  gS[j] := g;
               end;
               if k > e then e := k;
               k := s + prfscore(A+i+1, B+j+1);
               if f > k then k := f;
               if e > k then k := e;

               s := RR[j];
               RR[j] := k;
               SS[j] := e;
            end;

            if depth <= 1 then
              if i mod ProgressUnit = 0 then
                OnMultipleProgress;

            if Terminated then exit;
         end;
         SS[N] := RR[N];
         gS[N] := open_penalty2(A+midi+1,B+N);

      //* find midj, such that HH[j]+RR[j] or DD[j]+SS[j]+gap is the maximum */

         midh := HH[0]+RR[0];
         midj := 0;
         gtype := 1;
         for j := 0 to N do
         begin
            k := HH[j] + RR[j];
            if k >= midh then
               if (k > midh) or ((HH[j] <> DD[j]) and (RR[j] = SS[j])) then
               begin
                  midh := k;
                  midj := j;
               end;
         end;

         for j := N downto 0 do
         begin
            k := DD[j] + SS[j] + gS[j];
            if k > midh then
            begin
               midh := k;
               midj := j;
               gtype := 2;
            end;
         end;

      //* Conquer recursively around midpoint                                   */

         if gtype = 1 then          //* Type 1 gaps  */
         begin
      //    if debug>2 then fprintf(stdout,"Type 1,1: midj %d\n",(pint)midj);

            pdiff(A,B,midi,midj,go1,1, depth+1);

      //    if debug > 2 then fprintf(stdout,"Type 1,2: midj %d\n",(pint)midj);

            pdiff(A+midi,B+midj,M-midi,N-midj,1,go2, depth+1);
         end
         else
         begin
      //    if debug>2) fprintf(stdout,"Type 2,1: midj %d\n",(pint)midj);
            pdiff(A,B,midi-1,midj,go1, 0, depth+1);
            pdel(2);

      //    if debug>2) fprintf(stdout,"Type 2,2: midj %d\n",(pint)midj);

            pdiff(A+midi+1,B+midj,M-midi-1,N-midj,0,go2, depth+1);
         end;
         result := midh;       //* Return the score of the best alignment */
      finally
        if (depth > 1) and (depth <= 5) then
          case nseqs of
            2:   OnMultipleProgress;
            3:   if depth < 5 then OnMultipleProgress;
            4:   if (depth = 4) or (depth = 2) then OnMultipleProgress;
            5,6: if depth < 4 then OnMultipleProgress;
            7,8: if depth = 3 then OnMultipleProgress;
          else
            if depth = 2 then OnMultipleProgress;
         end;
      end;
   end;

   procedure ptracepath(var alen: integer);
   var
      i,j,k,pos,to_do : integer;
   begin
      pos := 0;

      to_do := print_ptr-1;

      for i := 1 to to_do do
      begin
   //    if debug>1) fprintf(stdout,"%d ",(pint)displ[i]);
         if displ[i] = 0 then
         begin
            aln_path1[pos] := #2;
            aln_path2[pos] := #2;
            pos := pos +1;
         end
         else
         begin
            k := displ[i];
            if k > 0 then
            begin
               for j := 0 to k-1 do
               begin
                  aln_path2[pos+j] := #2;
                  aln_path1[pos+j] := #1;
               end;
               pos := pos +k;
            end
            else
            begin
               if displ[i] < 0 then
                  k := -displ[i]
               else
                  k := displ[i];     //  k := (displ[i]<0) ? displ[i] * -1 : displ[i];
               for j := 0 to k-1 do
               begin
                  aln_path1[pos+j] := #2;
                  aln_path2[pos+j] := #1;
               end;
               pos := pos +k;
            end;
         end;
      end;
   // if debug > 1 then fprintf(stdout,"\n");

      alen := pos;

   end;

   function add_ggaps_mask(mask: PAnsiChar; len: integer; path1, path2: PAnsiChar):PAnsiChar;
   var
      i,ix : integer;
      ta : PAnsiChar;
   begin
      ta := nil;
      GetMem(ta, (len+1)*sizeof(AnsiChar) );

      ix := 0;
      if switch_profiles = FALSE then
      begin
         for i := 0 to len-1 do
            if path1[i] = #2 then
            begin
               ta[i] := mask[ix];
               ix := ix +1;
            end
            else if path1[i] = #1 then
               ta[i] := AnsiChar(Chr(gap_pos1));
      end
      else
      begin
         for i := 0 to len-1 do
            if path2[i] = #2 then
            begin
               ta[i] := mask[ix];
               ix := ix +1;
            end
            else if path2[i] = #1 then
               ta[i] := AnsiChar(Chr(gap_pos1));
      end;
      ReallocMem(mask,(len+2)*sizeof (AnsiChar));
      for i := 0 to len-1 do
         mask[i] := ta[i];
      mask[len] := AnsiChar(Chr(0)); // '\0';

      FreeMemAndNil(ta);

      result := mask;
   end;

   procedure add_ggaps;
   var
      i,j,ix,len : integer;
      ta: PAnsiChar;
   begin
      ta := nil;
      GetMem(ta, (alignment_len+1)*sizeof (AnsiChar) );

      for j := 0 to nseqs1-1 do
      begin
         ix := 0;
         for i := 0 to alignment_len-1 do
         begin
            if aln_path1[i] = #2 then
            begin
               if ix < aln_len[j] then
                  ta[i] := alignment[j][ix]
               else
                  ta[i] := #127;
               ix := ix +1;
            end
            else if aln_path1[i] = #1 then
            begin
   {
      insertion in first alignment...
   }
               ta[i] := AnsiChar(Chr(gap_pos1));
            end
            else
            begin
               raise Exception.create('Error in aln_path');
            end;
         end;
         ta[alignment_len] := #127;

         len := alignment_len;
         ReallocMem(alignment[j], (len+2)*sizeof (AnsiChar));
         for i := 0 to len-1 do
            alignment[j][i] := ta[i];
         alignment[j][len] := #127;
         aln_len[j] := len;
      end;

      for j := nseqs1 to nseqs1+nseqs2-1 do
      begin
         ix := 0;
         for i := 0 to alignment_len-1 do
         begin
            if aln_path2[i] = #2 then
            begin
               if ix < aln_len[j] then
                  ta[i] := alignment[j][ix]
               else
                  ta[i] := #127;
               ix := ix +1;
            end
            else if aln_path2[i] = #1 then
            begin
   {
      insertion in second alignment...
   }
               ta[i] := AnsiChar(Chr(gap_pos1));
            end
            else
            begin
               raise Exception.create('Error in aln_path');
            end;
         end;
         ta[alignment_len] := #127;

         len := alignment_len;
         ReallocMem(alignment[j], (len+2)*sizeof(AnsiChar) );
         for i := 0 to len-1 do
            alignment[j][i] := ta[i];
         alignment[j][len] := #127;
         aln_len[j] := len;
      end;

      FreeMemAndNil(ta);

      if struct_penalties1 <> false then
         gap_penalty_mask1 := add_ggaps_mask(gap_penalty_mask1,alignment_len,aln_path1,aln_path2);
      if struct_penalties1 = true then
         sec_struct_mask1 := add_ggaps_mask(sec_struct_mask1,alignment_len,aln_path1,aln_path2);

      if struct_penalties2 <> false then
         gap_penalty_mask2 := add_ggaps_mask(gap_penalty_mask2,alignment_len,aln_path2,aln_path1);
      if struct_penalties2 = true then
         sec_struct_mask2 := add_ggaps_mask(sec_struct_mask2,alignment_len,aln_path2,aln_path1);
   {
   if debug > 0 then
   begin
     char c;
     extern char *amino_acid_codes;

      for i:=0;i<nseqs1+nseqs2;i do
        begin
         for j:=0;j<alignment_len;j do
          begin
           if alignment[i][j] == 127) break;
           else if (alignment[i][j] == gap_pos1) or (alignment[i][j] == gap_pos2))  c := '-';
           else c := amino_acid_codes[alignment[i][j]];
           fprintf(stdout,"%c", c);
          end;
         fprintf(stdout,"\n\n");
        end;
   end;
   }
   end;

var
   negative : boolean;
   c, i, j, count : integer;
   NumSeq, len, len1, len2, iss, minlen : integer;
   se1, se2, sb1, sb2 : integer;
   maxres, int_scale, score : integer;
   matptr, mat_xref : PArrayOfInt;
   scale, logmin, logdiff, pcid : double;
begin
   for i := Low(matrix) to High(matrix) do
     for j := Low(matrix[i]) to High(matrix[i]) do
       matrix[i,j] := 0;
   alignment := nil;
   aln_len := nil;
   aln_weight := nil;
   matptr := nil;
   mat_xref := nil;
   HH := nil;
   DD := nil;
   RR := nil;
   SS := nil;
   gS := nil;

   count := 0;

   GetMem(alignment, nseqs*sizeof(PAnsiChar));
   GetMem(aln_len, nseqs*sizeof(integer));
   GetMem(aln_weight, nseqs*sizeof(integer));

   for i := 0 to nseqs-1 do
      if aligned[i+1] = 0 then group[i+1] := 0;

   nseqs1 := 0;
   nseqs2 := 0;
   for i := 0 to nseqs-1 do
      if group[i+1] = 1 then
         nseqs1 := nseqs1 +1
      else if group[i+1] = 2 then
         nseqs2 := nseqs2 +1;

   if (nseqs1 = 0) or (nseqs2 = 0) then
   begin
      result := 0;
      exit;
   end;

   if nseqs2 > nseqs1 then
   begin
      switch_profiles := TRUE;
      for i := 0 to nseqs-1 do
         if group[i+1] = 1 then
            group[i+1] := 2
         else if group[i+1] = 2 then
            group[i+1] := 1;
   end
   else
      switch_profiles := FALSE;

   int_scale := 100;

//   calculate the mean of the sequence pc identities between the two groups

      count := 0;
      pcid := 0.0;
      negative := neg_matrix;
      for i := 0 to nseqs-1 do
      begin
         if group[i+1] = 1 then
            for j := 0 to nseqs-1 do
               if group[j+1] = 2 then
               begin
                  count := count +1;
                  pcid := pcid +tmat[i][j];
               end;
      end;

   pcid := Trunc(pcid/count);

//  Make the first profile.

   prf_length1 := 0;
   for i := 0 to nseqs-1 do
       if (group[i+1] = 1) then
	   if seqlen_array[i+1] > prf_length1 then
               prf_length1 := seqlen_array[i+1];

   nseqs1 := 0;

   for i := 0 to nseqs-1 do
   begin
      if group[i+1] = 1 then
      begin
         len := seqlen_array[i+1];
         GetMem(alignment[nseqs1], (len+2)*sizeof(AnsiChar) );
         for j := 0 to len-1 do
            alignment[nseqs1][j] := seq_array[i+1][j+1];
	 for j := len to prf_length1-1 do
	     alignment[nseqs1][j+1] := AnsiChar(Char(gap_pos1));
         alignment[nseqs1][prf_length1+1] := #127;
         aln_len[nseqs1] := prf_length1;
         aln_weight[nseqs1] := seq_weight[i];
         nseqs1 := nseqs1 +1;
      end;
   end;

{
  Make the second profile.
}
   prf_length2 := 0;
   for i := 0 to nseqs-1 do
       if group[i+1] = 2 then
	   if seqlen_array[i+1] > prf_length2 then
               prf_length2 := seqlen_array[i+1];

   nseqs2 := 0;
//   if debug > 0 then fprintf(stdout,"sequences profile 2:\n");
   for i := 0 to nseqs-1 do
   begin
      if group[i+1] = 2 then
      begin
{
if debug>0)
begin
extern char **names;
fprintf(stdout,"%s\n",names[i+1]);
end;
}
         len := seqlen_array[i+1];
         GetMem(alignment[nseqs1+nseqs2], (len+2)*sizeof (AnsiChar) );
         for j := 0 to len-1 do
            alignment[nseqs1+nseqs2][j] := seq_array[i+1][j+1];
	 for j := len  to prf_length2-1 do
	   alignment[nseqs1+nseqs2][j+1] := AnsiChar(Chr(gap_pos1));
         alignment[nseqs1+nseqs2][prf_length2+1] := #127;
         aln_len[nseqs1+nseqs2] := prf_length2;
         aln_weight[nseqs1+nseqs2] := seq_weight[i];
         nseqs2 := nseqs2 +1;
      end;
   end;

   max_aln_length := prf_length1 + prf_length2 + 2;

{
   calculate real length of profiles - removing end gaps!
}
   len1 := 0;
   for i := 0 to nseqs1-1 do
   begin
      iss := 0;
      for j := 0 to Min(aln_len[i], prf_length1)-1 do
      begin
         c := integer(alignment[i][j]);
         if (c <>gap_pos1) and (c <> gap_pos2) then
            iss := iss +1;
      end;
      len1 := len1 +iss;
   end;
   len1 := Trunc(len1/nseqs1);

   len2 := 0;
   for i := nseqs1 to (nseqs2+nseqs1)-1 do
   begin
      iss := 0;
      for j := 0 to Min(aln_len[i],prf_length2)-1 do
      begin
         c := integer(alignment[i][j]);
         if (c <> gap_pos1) and (c <> gap_pos2) then
            iss := iss +1;
      end;
      len2 := len2 +iss;
   end;
   len2 := Trunc(len2/nseqs2);

// if(debug>0) fprintf(stdout,"pcid %3.1f scale %3.1f\n",pcid,scale);


   if dnaflag then
   begin
      scale := 1.0;
      if dnamtrxname = 'iub' then
      begin
         matptr := Addr(swgapdnamt);
         mat_xref := Addr(def_dna_xref);
      end
      else if dnamtrxname = 'clustalw' then
      begin
         matptr := Addr(clustalvdnamt);
         mat_xref := Addr(def_dna_xref);
         scale := 0.66;
      end
      else
      begin
         matptr := Addr(userdnamat);
         mat_xref := Addr(dna_xref);
      end;
      maxres := get_matrix(matptr, mat_xref, matrix, neg_matrix, int_scale);
      if maxres = 0 then
      begin
         result := -1;
         exit;
      end;

//* fix suggested by Chanan Rubin at Compugen */

      matrix[mat_xref[0]][mat_xref[4]] := Trunc(transition_weight*matrix[0][0]);
      matrix[mat_xref[4]][mat_xref[0]] := Trunc(transition_weight*matrix[0][0]);
      matrix[mat_xref[2]][mat_xref[11]]:= Trunc(transition_weight*matrix[0][0]);
      matrix[mat_xref[11]][mat_xref[2]]:= Trunc(transition_weight*matrix[0][0]);
      matrix[mat_xref[2]][mat_xref[12]]:= Trunc(transition_weight*matrix[0][0]);
      matrix[mat_xref[12]][mat_xref[2]]:= Trunc(transition_weight*matrix[0][0]);

      gapcoef1 := Trunc(100.0*gap_open*scale);
      gapcoef2 := gapcoef1;
      lencoef1 := Trunc(100.0*gap_extend*scale);
      lencoef2 := lencoef1;
   end
   else
   begin
      if (len1 = 0) or (len2 = 0) then
      begin
  	 logmin :=  1.0;
  	 logdiff := 1.0;
      end
      else
      begin
  	 minlen := MIN(len1,len2);
 	 logmin := 1.0/log10(minlen);
 	 if len2 < len1 then
    	    logdiff := 1.0 +0.5*log10(len2/len1)
  	 else if len1 < len2 then
  	    logdiff := 1.0 +0.5*log10(len1/len2)
  	 else
            logdiff := 1.0;
	 if logdiff < 0.9 then
            logdiff := 0.9;
      end;
{
if(debug>0) fprintf(stdout,"%d %d logmin %f   logdiff %f\n", (pint)len1,(pint)len2, logmin,logdiff);
}
      scale := 0.75;
      if mtrxname = 'blosum' then
      begin
         scale := 0.75;
         if negative or (distance_tree = FALSE) then
            matptr := Addr(blosum40mt)
         else if pcid > 80.0 then
            matptr := Addr(blosum80mt)
         else if pcid > 60.0 then
            matptr := Addr(blosum62mt2)
         else if pcid > 40.0 then
            matptr := Addr(blosum45mt)
         else if pcid > 30.0 then
         begin
            scale  := 0.5;
            matptr := Addr(blosum45mt);
         end
         else if pcid > 20.0 then
         begin
            scale  := 0.6;
            matptr := Addr(blosum45mt);
         end
         else
         begin
            scale  := 0.6;
            matptr := Addr(blosum30mt);
         end;
         mat_xref := Addr(def_aa_xref);

      end
      else if mtrxname = 'pam' then
      begin
         scale := 0.75;
         if negative or (distance_tree = FALSE) then
            matptr := Addr(pam120mt)
         else if pcid > 80.0 then
             matptr := Addr(pam20mt)
         else if pcid > 60.0 then
             matptr := Addr(pam60mt)
         else if pcid > 40.0 then
            matptr := Addr(pam120mt)
         else
            matptr := Addr(pam350mt);
         mat_xref := Addr(def_aa_xref);
      end
      else if mtrxname = 'gonnet' then
      begin
	 scale := scale/2.0;
         if negative or distance_tree = FALSE then
            matptr := Addr(gon250mt)
         else if pcid > 35.0 then
         begin
            matptr := Addr(gon80mt);
	    scale := scale/2.0;
         end
         else if pcid > 25.0 then
         begin
            if minlen < 100 then
               matptr := Addr(gon250mt)
            else
               matptr := Addr(gon120mt);
         end
         else
         begin
            if minlen < 100 then
               matptr := Addr(gon350mt)
	    else
               matptr := Addr(gon160mt);
         end;
         mat_xref := Addr(def_aa_xref);
         int_scale := int_scale div 10;
      end
      else if mtrxname = 'id' then
      begin
         matptr := Addr(idmat);
         mat_xref := Addr(def_aa_xref);
      end
{
      else if(user_series)
      begin
           matptr=NULL;
	   found=FALSE;
	   for(i=0;i<matseries.nmat;i++)
		if(pcid>=matseries.mat[i].llimit && pcid<=matseries.mat[i].ulimit)
		begin
			j=i;
			found=TRUE;
			break;
		end
	   if(found==FALSE)
	   begin
		if(!error_given)
		warning(
"\nSeries matrix not found for sequence percent identity = %d.\n"
"(Using first matrix in series as a default.)\n"
"This alignment may not be optimal!\n"
"SUGGESTION: Check your matrix series input file and try again.",(int)pcid);
		error_given=TRUE;
		j=0;
	   end

           matptr = matseries.mat[j].matptr;
           mat_xref = matseries.mat[j].aa_xref;
/* this gives a scale of 0.5 for pcid=llimit and 1.0 for pcid=ulimit */
           scale=0.5+(pcid-matseries.mat[j].llimit)/((matseries.mat[j].ulimit-matseries.mat[j].llimit)*2.0);
if (debug>0) fprintf(stdout,"pcid %d  matrix %d\n",(pint)pcid,(pint)j+1);
        }
      else
      begin
         matptr := Addr(usermat);
         mat_xref := Addr(aa_xref);
      end;
      maxres := get_matrix(matptr, mat_xref, matrix, negative, int_scale);
      if maxres = 0 then
      begin
//           fprintf(stdout,"Error: matrix %s not found\n", mtrxname);
         result := -1;
         exit;
      end;

      if negative then
      begin
         gapcoef1 := Trunc(100.0 * gap_open);
         gapcoef2 := Trunc(100.0 * gap_open);
         lencoef1 := Trunc(100.0 * gap_extend);
         lencoef2 := Trunc(100.0 * gap_extend);
      end
      else
      begin
         if mat_avscore <= 0 then
         begin
            gapcoef1 := Trunc(100.0 * (gap_open + logmin));
            gapcoef2 := gapcoef1;
         end
         else
         begin
            gapcoef1 := Trunc(scale * mat_avscore * (gap_open*logdiff/logmin));
            gapcoef2 := gapcoef1;
         end;
         lencoef1 := Trunc(100.0 * gap_extend);
         lencoef2 := lencoef1;
      end;
   end;
{
if debug>0)
begin
fprintf(stdout,"matavscore %d\n",mat_avscore);
fprintf(stdout,"Gap Open1 %d  Gap Open2 %d  Gap Extend1 %d   Gap Extend2 %d\n",
   (pint)gapcoef1,(pint)gapcoef2, (pint)lencoef1,(pint)lencoef2);
fprintf(stdout,"Matrix  %s\n", mtrxname);
end;
}

   GetMem(profile1, (prf_length1+2)*sizeof(pointer) );
   for i := 0 to prf_length1+1 do
      GetMem(profile1[i], (LENCOL+2)*sizeof(integer) );

   GetMem(profile2, (prf_length2+2)*sizeof(pointer) );
   for i := 0 to prf_length2+1 do
       GetMem(profile2[i], (LENCOL+2)*sizeof(integer) );

{
  calculate the Gap Coefficients.
}
   GetMem(gaps, (max_aln_length+1)*sizeof(integer) );

   if switch_profiles = FALSE then
      calc_gap_coeff(alignment, gaps, profile1, (struct_penalties1 and use_ss1), gap_penalty_mask1, 0, nseqs1, prf_length1, gapcoef1, lencoef1)
   else
      calc_gap_coeff(alignment, gaps, profile1, (struct_penalties2 and use_ss2), gap_penalty_mask2, 0, nseqs1, prf_length1, gapcoef1, lencoef1);
{
  calculate the profile matrix.
}
   calc_prf1(profile1, alignment, gaps, matrix, aln_weight, prf_length1, 0, nseqs1);

{
if debug > 4
begin
extern char *amino_acid_codes;
  for j:=0;j<=max_aa;j do
    fprintf(stdout,"%c    ", amino_acid_codes[j]);
 fprintf(stdout,"\n");
  for i:=0;i<prf_length1;i do
   begin
    for j:=0;j<=max_aa;j do
      fprintf(stdout,"%d ", (pint)profile1[i+1][j]);
    fprintf(stdout,"%d ", (pint)profile1[i+1][gap_pos1]);
    fprintf(stdout,"%d ", (pint)profile1[i+1][gap_pos2]);
    fprintf(stdout,"%d %d\n",(pint)profile1[i+1][GAPCOL],(pint)profile1[i+1][LENCOL]);
   end;
end;
}
{
  calculate the Gap Coefficients.
}

   if switch_profiles = FALSE then
      calc_gap_coeff(alignment, gaps, profile2, (struct_penalties2 and use_ss2), gap_penalty_mask2, nseqs1, nseqs1+nseqs2, prf_length2, gapcoef2, lencoef2)
   else
      calc_gap_coeff(alignment, gaps, profile2, (struct_penalties1 and use_ss1), gap_penalty_mask1, nseqs1, nseqs1+nseqs2, prf_length2, gapcoef2, lencoef2);
{
  calculate the profile matrix.
}
   calc_prf2(profile2, alignment, aln_weight, prf_length2, nseqs1, nseqs1+nseqs2);

   FreeMemAndNil(aln_weight);

{
if debug>4)
begin
extern char *amino_acid_codes;
  for j:=0;j<=max_aa;j do
    fprintf(stdout,"%c    ", amino_acid_codes[j]);
 fprintf(stdout,"\n");
  for i:=0;i<prf_length2;i do
   begin
    for j:=0;j<=max_aa;j do
      fprintf(stdout,"%d ", (pint)profile2[i+1][j]);
    fprintf(stdout,"%d ", (pint)profile2[i+1][gap_pos1]);
    fprintf(stdout,"%d ", (pint)profile2[i+1][gap_pos2]);
    fprintf(stdout,"%d %d\n",(pint)profile2[i+1][GAPCOL],(pint)profile2[i+1][LENCOL]);
   end;
end;
}

   GetMem(aln_path1, (max_aln_length+1)*sizeof(AnsiChar) );
   GetMem(aln_path2, (max_aln_length+1)*sizeof(AnsiChar) );

{
   align the profiles
}
// use Myers and Miller to align two sequences */

   last_print := 0;
   print_ptr := 1;

   sb1 := 0;
   sb2 := 0;
   se1 := prf_length1;
   se2 := prf_length2;

   GetMem(HH, (max_aln_length+1)*sizeof(integer) );
   GetMem(DD, (max_aln_length+1)*sizeof(integer) );
   GetMem(RR, (max_aln_length+1)*sizeof(integer) );
   GetMem(SS, (max_aln_length+1)*sizeof(integer) );
   GetMem(gS, (max_aln_length+1)*sizeof(integer) );
   GetMem(displ, (max_aln_length+1)*sizeof (integer) );


   if nseqs-1 > 15 then
     ProgressNum := 1
   else
     ProgressNum := Trunc(15/(nseqs-1));

   score := pdiff(sb1, sb2, se1-sb1, se2-sb2, profile1[0][GAPCOL], profile1[prf_length1][GAPCOL], 0);

   FreeMemAndNil(HH);
   FreeMemAndNil(DD);
   FreeMemAndNil(RR);
   FreeMemAndNil(SS);
   FreeMemAndNil(gS);

   ptracepath(alignment_len);

   FreeMemAndNil(displ);

   add_ggaps();

   if profile1 <> nil then
   begin
     for i := 0 to prf_length1+1 do
        FreeMemAndNil(profile1[i]);
     FreeMemAndNil(profile1);
   end;
   if profile2 <> nil then
   begin
     for i := 0 to prf_length2+1 do
        FreeMemAndNil(profile2[i]);
     FreeMemAndNil(profile2);
   end;
   prf_length1 := alignment_len;

   FreeMemAndNil(aln_path1);
   FreeMemAndNil(aln_path2);

   NumSeq := 0;
   for j := 0 to nseqs-1 do
      if group[j+1]  = 1 then
      begin
         seqlen_array[j+1] := prf_length1;
         ReallocMem(seq_array[j+1], (prf_length1+2)*sizeof(AnsiChar));
         for i := 0 to prf_length1-1 do
            seq_array[j+1][i+1] := alignment[NumSeq][i];
         NumSeq := NumSeq +1;
      end;
   for j := 0 to nseqs-1 do
      if group[j+1]  = 2 then
      begin
         seqlen_array[j+1] := prf_length1;
         ReallocMem(seq_array[j+1], (prf_length1+2)*sizeof(AnsiChar));
         for i := 0 to prf_length1-1 do
            seq_array[j+1][i+1] := alignment[NumSeq][i];
         NumSeq := NumSeq +1;
      end;

   if alignment <> nil then
   begin
     for i := 0 to nseqs1+nseqs2-1 do
        FreeMemAndNil(alignment[i]);
     FreeMemAndNil(alignment);
   end;
   FreeMemAndNil(aln_len);
   FreeMemAndNil(gaps);

   result := score div 100;
end;

procedure TClustalWThread.calc_gap_coeff(alignment: PArrayOfPChar; gaps: PArrayOfInt; profile: PIntMatrix;
                                   struct_penalties: boolean; gap_penalty_mask: PAnsiChar;
                                   first_seq, last_seq, prf_length, gapcoef, lencoef: integer);
var
  reduced_gap : double;
  nhyd_pen,nvar_pen,npref_pen : boolean; //* local copies of ho_hyd_penalties, no_pref_penalties */
  gdist : integer;              //* local copy of gap_dist */
  vll, vwindow : integer;

   procedure calc_v_penalties(aln : PArrayOfPChar; n, fs, ls: integer; weight: PArrayOfInt);
   var
      vlut : array[0..25,0..25] of integer;
      ix1,ix2,i,j,t: integer;
   begin
     for i := 0 to 25 do
       for j := 0 to 25 do
         if (i = j) then
           vlut[i,j] := 1
         else
           vlut[i,j] := 0;
     for i := 0 to n-1 do
     begin
        weight[i] := 0;
	t := 0;
	for j := i-vwindow to i+vwindow-1 do
	begin
	   if (j >= 0) and  (j<n) then
           begin
              ix1 := integer(aln[fs][j]);
              ix2 := integer(aln[fs+1][j]);
              if (ix1 < 0) or (ix1 > max_aa) or (ix2< 0) or (ix2> max_aa) then
                 continue;
              weight[i] := weight[i] +vlut[Ord(amino_acid_codes[ix1+1])-Ord('A')][Ord(amino_acid_codes[ix2+1])-Ord('A')];
	      t := t +1;
	   end;
	end;
//* now we have a weight -t < w < t */
        weight[i] := weight[i] +t;
        if t > 0 then
          weight[i] := Trunc((weight[i]*100)/(2*t))
        else
          weight[i] := 100;
//* now we have a weight vll < w < 100 */
        if weight[i] < vll then
          weight[i] := vll;
     end;
   end;

   procedure calc_p_penalties(aln: PArrayOfPChar; n, fs, ls: integer; weight: PArrayOfInt);
   var
     ix,i,j,k,numseq: integer;
   begin
    numseq := ls - fs;
     for i := 0 to n-1 do
     begin
       weight[i] := 0;
       for k := fs to ls-1 do
       begin
         for j := 0 to 19 do                     //  correctd  from "for j := 0 to 21 do"
         begin
           ix := integer(aln[k][i]);
           if (ix < 0) or (ix > max_aa) then continue;
           if amino_acid_codes[ix+1] = pr[j] then
           begin
             weight[i] := weight[i] +(180-pas_op[j]);
             break;
           end;
         end;
       end;
       weight[i] := weight[i] div numseq;
     end;
   end;

   procedure calc_h_penalties(aln: PArrayOfPChar; n, fs, ls: integer; weight: PArrayOfInt);
   var
     ix,nh,j,k,i,e,s: integer;
     hyd: PArrayOfInt;
     scale: double;
   begin
   {
      weight[] is the length of the hydrophilic run of residues.
   }

     GetMem(hyd, (n+2)*sizeof(integer));
     nh := Length(hyd_residues);
     for i := 0 to n-1 do
        weight[i] := 0;

     for k := fs to ls-1 do
     begin
        for i := 0 to n-1 do
        begin
            hyd[i] := 0;
            for j := 0 to nh-1 do
            begin
               ix := integer(aln[k][i]);
               if (ix < 0) or (ix > max_aa) then continue;
               if amino_acid_codes[ix+1] = hyd_residues[j+1] then
               begin
                  hyd[i] := 1;
                  break;
               end;
            end;
        end;
        i := 0;
        while i < n do
        begin
           if hyd[i] = 0 then
              i := i +1
           else
           begin
              s := i;
              while (hyd[i] <> 0) and (i<n) do
                 i := i +1;
              e := i;
              if (e-s) > 3 then
                 for j := s to e-1 do
                    weight[j] := weight[j] +100;
           end;
        end;
     end;

     scale := ls - fs;
     for i := 0 to n-1 do
        weight[i] := Trunc(weight[i]/scale);

     FreeMemAndNil(hyd);
{
   if debug > 1 then
   begin
     for(i:=0;i<n;i++) fprintf(stdout,"%d ", (pint)weight[i]);
     fprintf(stdout,"\n");
   end;
}
   end;

   function local_penalty(penalty, n : integer; pweight, hweight, vweight: PArrayOfInt):integer;
   var
     h: boolean;
     gw: double;
   begin
     h := FALSE;

     if dnaflag then
     begin
       result := 1;
       exit;
     end;

     gw := 1.0;
     if nvar_pen = FALSE then
        gw := gw*vweight[n]/100.0;

     if nhyd_pen = FALSE then
     begin
        if hweight[n] > 0 then
        begin
          gw := gw *0.5;
          h := TRUE;
        end;
     end;
     if (npref_pen = FALSE) and (h=FALSE) then
     begin
        gw := gw*(pweight[n]/100.0);
     end;

     gw := gw *penalty;
     result := Trunc(gw);

   end;

   function percentid(s1, s2: PAnsiChar; length: integer):double;
   var
     i,count,total: integer;
     score: double;
   begin
     count := 0;
     total := 0;
     for i := 0 to length-1 do
     begin
       if (integer(s1[i]) >= 0) and (integer(s1[i]) < max_aa) then
       begin
         total := total +1;
         if s1[i] = s2[i] then
           count := count +1;
       end;
       if (integer(s1[i]) = -3) or (integer(s2[i]) = -3) then break;
     end;

     if total = 0 then
       score := 0
     else
       score := 100.0 * count / total;
     result := score;
   end;

var
  c,i,j,iss,ie,numseq,val,pcid: integer;
  gap_pos, p_weight, h_weight, v_weight : PArrayOfInt;
  scale: double;
begin
   reduced_gap := 1.0;
   vll := 50;
   vwindow := 5;

   numseq := last_seq - first_seq;
   if numseq = 2 then
     pcid := Trunc(percentid(alignment[first_seq],alignment[first_seq+1],prf_length))
   else
     pcid := 0;

   for j :=0 to prf_length-1 do
     gaps[j] := 0;
{
   Check for a gap penalty mask
}
   if struct_penalties <> false then
   begin
     nvar_pen := TRUE;
     nhyd_pen := TRUE;
     npref_pen := TRUE;
     gdist := 0;
   end
   else if (no_var_penalties = FALSE) and (pcid > 60) then
   begin
//   if(debug>0) fprintf(stderr,"Using variable zones to set gap penalties (pcid = %d)\n",pcid);
     nvar_pen := FALSE;
     nhyd_pen := TRUE;
     npref_pen := TRUE;
     gdist := 0;
   end
   else
   begin
     nvar_pen := TRUE;
     nhyd_pen := no_hyd_penalties;
     npref_pen := no_pref_penalties;
     gdist := gap_dist;
   end;

   for i := first_seq to last_seq-1 do
   begin
{
   Include end gaps as gaps ?
}
     iss := 0;
     ie := prf_length;
     if (use_endgaps = FALSE) and (endgappenalties = FALSE) then
     begin
       for j :=0 to prf_length-1 do
       begin
         c := integer(alignment[i][j]);
         if (c < 0) or (c > max_aa) then
            iss := iss +1
         else
            break;
       end;
       for j := prf_length-1 downto 0 do
       begin
         c := integer(alignment[i][j]);
         if (c < 0) or (c > max_aa) then
            ie := ie -1
         else
            break;
       end;
     end;

     for j := iss to ie-1 do
       if (integer(alignment[i][j]) < 0) or (integer(alignment[i][j]) > max_aa) then
         gaps[j] := gaps[j] +1;
   end;

   if (not dnaflag) and (nvar_pen = FALSE) then
   begin
     GetMem(v_weight, (prf_length+2) * sizeof(integer) );
     calc_v_penalties(alignment, prf_length, first_seq, last_seq, v_weight);
   end;

   if (not dnaflag) and (npref_pen = FALSE) then
   begin
      GetMem(p_weight, (prf_length+2)*sizeof(integer) );
      calc_p_penalties(alignment, prf_length, first_seq, last_seq, p_weight);
   end;

   if (not dnaflag) and (nhyd_pen = FALSE) then
   begin
      GetMem(h_weight, (prf_length+2)*sizeof (integer) );
      calc_h_penalties(alignment, prf_length, first_seq, last_seq, h_weight);
   end;

   GetMem(gap_pos, (prf_length+2)*sizeof (integer) );
{
    mark the residues close to an existing gap (set gaps[i] := -ve)
}
   if dnaflag or (gdist <= 0) then
   begin
     for i := 0 to prf_length-1 do
       gap_pos[i] := gaps[i];
   end
   else
   begin
     i:=0;
     while (i < prf_length) do
     begin
        if gaps[i] <= 0 then
        begin
           gap_pos[i] := gaps[i];
           i := i +1;
        end
        else
        begin
          for j := -gdist+1 to -1 do // (j = -gdist+1; j<0; j++)
          begin
           if (i+j >= 0) and ((i+j) < prf_length) and
              ((gaps[i+j] = 0) or (gaps[i+j] < j)) then
              gap_pos[i+j] := j;
          end;
          while (gaps[i] > 0) do
          begin
            if i>= prf_length then break;
            gap_pos[i] := gaps[i];
            i := i +1;
          end;
          for j := 0 to gdist-1 do
          begin
           if gaps[i+j] > 0 then break;
           if (i+j >= 0) and ((i+j) < prf_length) and
              ((gaps[i+j] = 0) or (gaps[i+j] < -j)) then
             gap_pos[i+j] := -j-1;
          end;
          i := i + j;
        end;
     end;
   end;
{
if debug > 1 then
begin
fprintf(stdout,"gap open %d gap ext %d\n",(pint)gapcoef,(pint)lencoef);
fprintf(stdout,"gaps:\n");
  for(i:=0;i<prf_length;i++) fprintf(stdout,"%d ", (pint)gaps[i]);
  fprintf(stdout,"\n");
fprintf(stdout,"gap_pos:\n");
  for(i:=0;i<prf_length;i++) fprintf(stdout,"%d ", (pint)gap_pos[i]);
  fprintf(stdout,"\n");
end;
}
   for j := 0 to prf_length-1 do
   begin
      if gap_pos[j] <= 0 then
      begin
{
    apply residue-specific and hydrophilic gap penalties.
}
	 if not dnaflag then
         begin
            profile[j+1][GAPCOL] := local_penalty(gapcoef, j, p_weight, h_weight, v_weight);
            profile[j+1][LENCOL] := lencoef;
	 end
	 else begin
            profile[j+1][GAPCOL] := gapcoef;
            profile[j+1][LENCOL] := lencoef;
	 end;

{
    increase gap penalty near to existing gaps.
}
         if gap_pos[j] < 0 then
            profile[j+1][GAPCOL] := Trunc(profile[j+1][GAPCOL]*(2.0+2.0*(gdist+gap_pos[j])/gdist));
      end
      else
      begin
         scale := (numseq-gaps[j])/numseq* reduced_gap;
         profile[j+1][GAPCOL] := Trunc(scale*gapcoef);
         profile[j+1][LENCOL] := Trunc(0.5 * lencoef);
      end;
{
    apply the gap penalty mask
}
      if struct_penalties <> false then
      begin
        val := Ord(gap_penalty_mask[j]) -Ord('0');
        if (val > 0) and (val < 10) then
        begin
          profile[j+1][GAPCOL] := profile[j+1][GAPCOL] *val;
          profile[j+1][LENCOL] := profile[j+1][LENCOL] *val;
        end;
      end;
{
   make sure no penalty is zero - even for all-gap positions
}
        if profile[j+1][GAPCOL] <= 0 then
           profile[j+1][GAPCOL] := 1;
        if profile[j+1][LENCOL] <= 0then
           profile[j+1][LENCOL] := 1;
     end;

//* set the penalties at the beginning and end of the profile */
     if endgappenalties = TRUE then
     begin
        profile[0][GAPCOL] := gapcoef;
        profile[0][LENCOL] := lencoef;
     end
     else
     begin
        profile[0][GAPCOL] := 0;
        profile[0][LENCOL] := 0;
        profile[prf_length][GAPCOL] := 0;
        profile[prf_length][LENCOL] := 0;
     end;
{
if debug>1 then
begin
  fprintf(stdout,"Opening penalties:\n");
  for(i:=0;i<=prf_length;i++) fprintf(stdout," %d:%d ",i, (pint)profile[i][GAPCOL]);
  fprintf(stdout,"\n");
end;
if debug>1 then
begin
  fprintf(stdout,"Extension penalties:\n");
  for(i:=0;i<=prf_length;i++) fprintf(stdout,"%d:%d ",i, (pint)profile[i][LENCOL]);
  fprintf(stdout,"\n");
end;
}
   if (not dnaflag) and (nvar_pen = FALSE) then
      FreeMemAndNil(v_weight);

   if (not dnaflag) and (npref_pen = FALSE) then
      FreeMemAndNil(p_weight);

   if (not dnaflag) and (nhyd_pen = FALSE) then
      FreeMemAndNil(h_weight);

   FreeMemAndNil(gap_pos);
end;              

procedure TClustalWThread.calc_prf1(profile: PIntMatrix; alignment: PArrayOfPChar; gaps: PArrayOfInt;
                              var matrix: TMatrix; seq_weight: PArrayOfInt; prf_length, first_seq, last_seq: integer);
var
  weighting: PIntMatrix;
  sum2, d, i, res, numseq, r, pos, f: integer;
  scale: double;
begin
  GetMem(weighting, (NUMRES+2)*sizeof(pointer) );
  for i := 0 to NUMRES+1 do
    GetMem(weighting[i], (prf_length+2)*sizeof(integer) );

  numseq := last_seq-first_seq;

  sum2 := 0;
  for i := first_seq to last_seq-1 do
    sum2 := sum2 +seq_weight[i];

  for r := 0 to prf_length-1 do
  begin
     for d := 0 to max_aa do
     begin
        weighting[d][r] := 0;
        for i := first_seq to last_seq-1 do
           if d = integer(alignment[i][r]) then
              weighting[d][r] := weighting[d][r] +seq_weight[i];
     end;
     weighting[gap_pos1][r] := 0;
     for i := first_seq to last_seq-1 do
        if gap_pos1 = integer(alignment[i][r]) then
           weighting[gap_pos1][r] := weighting[gap_pos1][r] +seq_weight[i];
     weighting[gap_pos2][r] := 0;
     for i := first_seq to last_seq-1 do
        if gap_pos2 = integer(alignment[i][r]) then
           weighting[gap_pos2][r] := weighting[gap_pos2][r] +seq_weight[i];
  end;

  for pos :=0 to prf_length-1 do
  begin
    if gaps[pos] = numseq then
    begin
       for res := 0 to max_aa do
         profile[pos+1][res] := matrix[res][gap_pos1];
       profile[pos+1][gap_pos1] := matrix[gap_pos1][gap_pos1];
       profile[pos+1][gap_pos2] := matrix[gap_pos2][gap_pos1];
    end
    else
    begin
       scale := (numseq-gaps[pos]) / numseq;
       for res :=0 to max_aa do
       begin
         f := 0;
         for d := 0 to max_aa do
            f := f +(weighting[d][pos] * matrix[d][res]);
         f := f +(weighting[gap_pos1][pos] * matrix[gap_pos1][res]);
         f := f +(weighting[gap_pos2][pos] * matrix[gap_pos2][res]);
         profile[pos+1][res] := Trunc((f / sum2)*scale);
       end;
       f := 0;
       for d := 0 to max_aa do
         f := f +(weighting[d][pos] * matrix[d][gap_pos1]);
       f := f +(weighting[gap_pos1][pos] * matrix[gap_pos1][gap_pos1]);
       f := f +(weighting[gap_pos2][pos] * matrix[gap_pos2][gap_pos1]);
       profile[pos+1][gap_pos1] := Trunc((f / sum2)*scale);
       f := 0;
       for d := 0 to max_aa do
         f := f +(weighting[d][pos] * matrix[d][gap_pos2]);
       f := f +(weighting[gap_pos1][pos] * matrix[gap_pos1][gap_pos2]);
       f := f +(weighting[gap_pos2][pos] * matrix[gap_pos2][gap_pos2]);
       profile[pos+1][gap_pos2] := Trunc((f / sum2)*scale);
    end;
  end;

  if weighting <> nil then
  begin
    for i := 0 to NUMRES+1 do
      FreeMemAndNil(weighting[i]);
    FreeMemAndNil(weighting);
  end;
end;

procedure TClustalWThread.calc_prf2(profile: PIntMatrix; alignment: PArrayOfPChar; seq_weight: PArrayOfInt;
                              prf_length, first_seq, last_seq: integer);
var
  sum1, sum2, i, d, r: integer;
begin
  for r := 0 to prf_length-1 do
  begin
{
   calculate sum2 := number of residues found in this column
}
    sum2 := 0;
    for i := first_seq to last_seq-1 do
      sum2 := sum2 +seq_weight[i];

{
   only include matrix comparison scores for those residue types found in this
   column
}
    if sum2 = 0 then
    begin
      for d :=0 to max_aa do
        profile[r+1][d] := 0;
      profile[r+1][gap_pos1] := 0;
      profile[r+1][gap_pos2] := 0;
    end
    else
    begin
      for d := 0 to max_aa do
      begin
        sum1 := 0;
        for i := first_seq to last_seq-1 do
          if d = integer(alignment[i][r]) then
            sum1 := sum1 +seq_weight[i];
        profile[r+1][d] := Trunc(10 * sum1 / sum2);
      end;
      sum1 := 0;
      for i := first_seq to last_seq-1 do
        if gap_pos1 = integer(alignment[i][r]) then
          sum1 := sum1 +seq_weight[i];
      profile[r+1][gap_pos1] := Trunc(10 * sum1 / sum2);
      sum1 := 0;
      for i := first_seq to last_seq-1 do
        if gap_pos2 = integer(alignment[i][r]) then
          sum1 := sum1 +seq_weight[i];
      profile[r+1][gap_pos2] := Trunc(10 * sum1 / sum2);
    end;
  end;
end;



end.
