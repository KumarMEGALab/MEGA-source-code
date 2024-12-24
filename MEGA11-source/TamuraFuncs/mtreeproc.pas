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

unit MTreeProc;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, MTreeBox, Math, MSimpleTreeNode;

procedure SwapNode(VAR p1,p2 : TpNode);
function SibNode(p : TpNode):TpNode;
procedure SetMaxLength(root : TpNode);
procedure SetClusterSize(root : TpNode);
procedure SetDepth(root : TpNode; Threshold : integer);
procedure SortBranchByFigure(p : TpNode);
procedure SortBranchByOrder(p : TpNode);

procedure SortBySortValues(p: TpNode);
procedure SortBySortStrings(p: TpNode);

procedure SortBranchByYear(p: TpNode);
procedure SortBranchByContinent(p: TpNode);
procedure SortBranchByGroup(p: TpNode);

procedure SetSortValuesByContinent(p: TpNode);
procedure SetsortValuesByGroup(p: TpNode);
procedure SetSortValuesByYear(p: TpNode);
procedure SetClusterGroupNames(p: TpNode);
procedure SetNodeFlagFromOTU(p : TpNode);
procedure SetNodeFlagFromRoot(p : TpNode);
function SearchCommonAncestor(root : TpNode):TpNode;
function FindCommonAncestorOfFlaggedTaxa(root: TpNode): TpNode;
procedure ChangeRoot(root, newposition : TpNode; midpoint : boolean; PreserveBlens: Boolean=False); { PreserveBlens is for Caltester to prevent modifying blens of the user-provided timetree}
function GetEarliestAndLatestDates(const p: TpNode; var earliestDate: String; var latestDate: String): Boolean; overload;
function GetEarliestAndLatestDates(const p: TpNode; var earliestDate: Double; var latestDate: Double): Boolean; overload;
procedure MarkTempOutgroup(outgroupRoot: TpNode; ingroupRoot: TpNode);
procedure GetTipDistancesFromAncestor(const ancestor: TpNode; var tipNodes: TList);
procedure SwapNodeSimple(var First, Second: TSimpleTreeNode);
function SibNodeSimple(P: TSimpleTreeNode): TSimpleTreeNode;
procedure ChangeRootSimple(var Root: TSimpleTreeNode; var NewPosition: TSimpleTreeNode);

implementation

uses
  MOtuInfo, dateutils, MegaConsts;

procedure SwapNode(VAR p1,p2 : TpNode);
var p : TpNode;
begin
    p := p1;
    p1 := p2;
    p2 := p;
end;

function SibNode(p : TpNode):TpNode;
begin
    if p.anc = nil then
        Result := nil
    else if p = p.anc.des1 then
        Result := p.anc.des2
    else if p = p.anc.des2 then
        Result := p.anc.des1
    else
        Result := nil;
end;

procedure SetMaxLength(root : TpNode);
var q : TpNode;

    procedure SetMaxLen2(p : TpNode);
    begin
        if p.OTU then
            p.branch.maxlen2 := p.branch.length
        else begin
            SetMaxLen2(p.des1);
            SetMaxLen2(p.des2);
            if p.des1.branch.maxlen2 > p.des2.branch.maxlen2 then
                p.branch.maxlen2 := p.des1.branch.maxlen2 + p.branch.length
            else
                p.branch.maxlen2 := p.des2.branch.maxlen2 + p.branch.length;
        end;
    end; { SetMaxLen2 }

    procedure SetMaxLen1(p : TpNode);
    begin
        q := SibNode(p);
        if p.anc.branch.maxlen1 > q.branch.maxlen2 then
            p.branch.maxlen1 := p.anc.branch.maxlen1 +p.branch.length
        else
            p.branch.maxlen1 := q.branch.maxlen2 +p.branch.length;
        if not p^.OTU then begin
            SetMaxLen1(p^.des1);
            SetMaxLen1(p^.des2);
        end;
    end; { SetMaxLen1 }

begin
    with root^ do begin
        SetMaxLen2(des1);
        SetMaxLen2(des2);
        des1.branch.maxlen1 := des2.branch.maxlen2 + des1.branch.length;
        des2.branch.maxlen1 := des1.branch.maxlen2 + des2.branch.length;
        if not des1.OTU then begin
            SetMaxLen1(des1.des1);
            SetMaxLen1(des1.des2);
        end;
        if not des2.OTU then begin
            SetMaxLen1(des2.des1);
            SetMaxLen1(des2.des2);
        end;
    end;
end; { SetMaxLength }

procedure SetDepth(root : TpNode; Threshold : integer);

    procedure SetDepthFromOTU(p : TpNode; Threshold : integer);
    var d1,d2 : integer;
    begin
        with p^ do begin
            if OTU or compressed then
                depth := 0
            else begin
                SetDepthFromOTU(des1, Threshold);
                SetDepthFromOTU(des2, Threshold);
                if (des1.OTU or des1.compressed) or (des1.branch.stat2 >= Threshold) then
                    d1 := des1.depth +1
                else
                    d1 := des1.depth;
                if (des2.OTU or des2.compressed) or (des2.branch.stat2 >= Threshold) then
                    d2 := des2.depth +1
                else
                    d2 := des2.depth;
                if d1 > d2 then
                    depth := d1
                else
                    depth := d2;
            end;
        end;
    end;

    procedure SetDepthFromRoot(p : TpNode; Threshold : integer);
    begin
        with p^ do begin
            if OTU then Exit;
            if not((anc = root) and
                   (anc.des1.OTU or anc.des2.OTU or anc.des1.compressed or anc.des2.compressed)) then
                if branch.stat2 < Threshold then
                    depth := anc^.depth;
            SetDepthFromRoot(des1, Threshold);
            SetDepthFromRoot(des2, Threshold);
        end;
    end;

begin
    SetDepthFromOTU(root, Threshold);
    with root^ do
    begin
        if des1.OTU or des2.OTU or des1.compressed or des2.compressed then
            if des1.depth > des2.depth then
          if des1.branch.stat2 >= Threshold then
                depth := des1.depth +1
            else
            depth := des1.depth
        else if des2.branch.stat2 >= Threshold then
          depth := des2.depth +1
        else
          depth := des2.depth;

        SetDepthFromRoot(des1, Threshold);
        SetDepthFromRoot(des2, Threshold);
    end;
end;

procedure SetClusterSize(root : TpNode);
begin
    with root^ do
        if not OTU then begin
            SetClusterSize(des1);
            SetClusterSize(des2);
            size := des1^.size + des2^.size;
            if des1^.minOTU < des2^.minOTU then
                minOTU := des1^.minOTU
            else
                minOTU := des2^.minOTU;
        end;
end; { SetClusterSize }

procedure SortBySortValues(p: TpNode);
begin
  if p^.OTU then Exit;
  if p^.des1^.sortValue > p^.des2^.sortValue then
    SwapNode(p^.des1,p^.des2);
  SortBySortValues(p^.des1);
  SortBySortValues(p^.des2);
end;

procedure SortBySortStrings(p: TpNode);
begin
  if p^.OTU then Exit;
  if CompareStr(p^.des1^.sortString, p^.des2^.sortString) > 0 then
    SwapNode(p^.des1,p^.des2);
  SortBySortStrings(p^.des1);
  SortBySortStrings(p^.des2);
end;

procedure SortBranchByYear(p: TpNode);
begin
  if p.anc = nil then
    SetSortValuesByYear(p);
  SortBySortValues(p);
end;

procedure SortBranchByContinent(p: TpNode);
begin
  if p.anc = nil then
    SetSortValuesByContinent(p);
  SortBySortStrings(p);
end;

procedure SortBranchByGroup(p: TpNode);
begin
  if p.anc = nil then
    SetsortValuesByGroup(p);
  SortBySortStrings(p);
end;

procedure SetSortValuesByContinent(p: TpNode);
var
  aInfo: TOtuInfo = nil;
begin
  if Assigned(p.des1) then
    SetSortValuesByContinent(p.des1);
  if Assigned(p.des2) then
    SetSortValuesByContinent(p.des2);
  if p.OTU then
  begin
    aInfo := TOtuInfo(p.otuInfo);
    p.sortString := aInfo.GeographicalInfo.Continent;
  end
  else
  begin
    if CompareStr(p.des1.sortString, p.des2.sortString) <= 0 then
      p.sortString := p.des1.sortString
    else
      p.sortString := p.des2.sortString;
  end;
end;

procedure SetsortValuesByGroup(p: TpNode);
var
  aInfo: TOtuInfo = nil;
begin
  if Assigned(p.des1) then
    SetSortValuesByGroup(p.des1);
  if Assigned(p.des2) then
    SetSortValuesByGroup(p.des2);
  if p.OTU then
  begin
    aInfo := TOtuInfo(p.otuInfo);
    p.sortString := aInfo.GeographicalInfo.Group;
  end
  else
  begin
    if CompareStr(p.des1.sortString, p.des2.sortString) <= 0 then
      p.sortString := p.des1.sortString
    else
      p.sortString := p.des2.sortString;
  end;
end;

procedure SetSortValuesByYear(p: TpNode);
var
  aInfo: TOtuInfo = nil;
begin
  if Assigned(p.des1) then
    SetSortValuesByYear(p.des1);
  if Assigned(p.des2) then
    SetSortValuesByYear(p.des2);
  if p.OTU then
  begin
    aInfo := TOtuInfo(p.otuInfo);
    p.sortValue := aInfo.GeographicalInfo.Year
  end
  else
    p.sortValue := max(p.des1.sortValue, p.des2.sortValue);
end;

procedure SetClusterGroupNames(p: TpNode);
begin
  if Assigned(p.des1) then
    SetClusterGroupNames(p.des1);
  if Assigned(p.des2) then
    SetClusterGroupNames(p.des2);
  if p.OTU and Assigned(p.otuInfo) then
    p.sortString := TOtuInfo(p.otuInfo).GpName
  else if Assigned(p.des1) and Assigned(p.des2) and (p.des1.sortString = p.des2.sortString) then
    p.sortString := p.des1.sortString;
end;

procedure SetNodeFlagFromOTU(p : TpNode);
begin
    if p.OTU then Exit;
    SetNodeFlagFromOTU(p.des1);
    SetNodeFlagFromOTU(p.des2);
    if p.des1.flag or p.des2.flag then
        p.flag := true
    else
        p.flag := false;
end;

procedure SetNodeFlagFromRoot(p : TpNode);
begin
    if p.OTU or (p.des1.flag and p.des2.flag) then Exit;
    if p.des1.flag or p.des2.flag then begin
        p.flag := false;
        if p.des1.flag then
            SetNodeFlagFromRoot(p.des1);
        if p.des2.flag then
            SetNodeFlagFromRoot(p.des2);
    end;
end;



function SearchCommonAncestor(root : TpNode):TpNode;
var
  maxsize : integer;

  procedure FindAncestor(p : TpNode; f : boolean);
  begin
      if p.flag = f then begin
          if p.size > maxsize then begin
              Result := p;
              maxsize := p.size;
          end
      end
      else if not p.OTU then begin
          FindAncestor(p.des1, f);
          FindAncestor(p.des2, f);
      end;
  end;

begin
    SetNodeFlagFromOTU(root);
    if not root.flag then begin
        Result := root;
        Exit;
    end;
    SetNodeFlagFromRoot(root);
    maxsize := 0;
    if root.flag then
        FindAncestor(root, false)
    else begin
        FindAncestor(root, true);
        if Result.size > (root.size div 2) then begin
            maxsize := root.size -Result.size;
            FindAncestor(Result, false);
        end;
    end;
end;

function FindCommonAncestorOfFlaggedTaxa(root: TpNode): TpNode;

  function FindAncestor(n: TpNode): TpNode;
  begin
    Result := nil;
    if n.flag then
    begin
      if Assigned(n.anc) and (not n.anc.flag) then
      begin
        Result := n;
        Exit;
      end
      else
        if not Assigned(n.anc) then
        begin
          Result := n;
          Exit;
        end;
    end
    else
    begin
      if Assigned(n.des1) then
        Result := FindAncestor(n.des1);
      if (Result = nil) and Assigned(n.des2) then
        Result := FindAncestor(n.des2);
    end;
  end;

begin
  SetNodeFlagFromOTU(root);
  SetNodeFlagFromRoot(root);
  Result := FindAncestor(root);
end;

procedure ChangeRoot(root, newposition : TpNode; midpoint : boolean; PreserveBlens: Boolean=False);
var a, p, d, q : TpNode;
    b0,b1,b2 : TBranch;
    rootmode: integer;

    procedure SwapMaxlen(var b : TBranch);
    var len : double;
    begin
        len := b.maxlen1;
        b.maxlen1 := b.maxlen2;
        b.maxlen2 := len;
    end;

begin
    if newposition = root then Exit;
    if newposition^.anc = root then
    begin
        if midpoint then
            if (newposition = root.des1) and (root.des1.branch.maxlen2 < root.des2.branch.maxlen2) then
                newposition := root.des2
            else if (newposition = root.des2) and (root.des2.branch.maxlen2 < root.des1.branch.maxlen2) then
                newposition := root.des1;
        with newposition^ do
        begin
            if midpoint then
                if ABS(branch.maxlen1 -branch.maxlen2) <= branch.length then
                begin
                    b0.length := (branch.maxlen1 +branch.maxlen2-branch.length)/2;
                    b1.length := branch.maxlen2 - b0.length;
                    b2.length := branch.maxlen1 - b0.length;
                end
                else
                begin
                    b1.length := 0.0;
                    b2.length := branch.length;
                end
            else
            begin
                b1.length := branch.length*1/10;
                b2.length := branch.length*9/10;
            end;
            if (newposition = root.des1) and (not PreserveBlens) then
            begin
                root.des2.branch.length  := root.des2.branch.length  +b1.length;
                root.des2.branch.maxlen2 := root.des2.branch.maxlen2 +b1.length;
                branch.length := b2.length;
                branch.maxlen2 := branch.maxlen2 -b1.length;
            end
            else if (newposition = root.des2) and (not PreserveBlens) then
            begin
                root.des1.branch.length  := root.des1.branch.length  +b1.length;
                root.des1.branch.maxlen2 := root.des1.branch.maxlen2 +b1.length;
                branch.length  := b2.length;
                branch.maxlen2 := branch.maxlen2 -b1.length;
            end;
        end;
        Exit;
    end;

    p := newposition;
    while p.anc <> root do
        p := p.anc;

    if p = p.anc.des1 then
        rootmode := 1
    else
        rootmode := 2;
    p := newposition;
    if rootmode = 1 then
    begin
      while p.anc <> root do
      begin
        if p = p.anc.des1 then
          SwapNode(p.anc.des1, p.anc.des2);
        p := p.anc;
      end;
    end
    else
    begin
      while p.anc <> root do
      begin
        if p = p.anc.des2 then
          SwapNode(p.anc.des1, p.anc.des2);
        p := p.anc;
      end;
    end;

    q := SibNode(p);
    b0 := q.branch;
    b0.maxlen2 := p.branch.maxlen1;
    b0.length := root.des1.branch.length +root.des2.branch.length;
    b0.SE     := root.des1.branch.SE +root.des2.branch.SE;
    b0.stats  := max(root.des1.branch.stats, root.des2.branch.stats);
    root.des1.branch.stats := b0.stats;
    root.des2.branch.stats := b0.stats;
    b0.stat2  := max(root.des1.branch.stat2, root.des2.branch.stat2);
    root.des1.branch.stat2 := b0.stat2;
    root.des2.branch.stat2 := b0.stat2;

    d := newposition;
    p := d.anc;
    a := p.anc;
    b2 := d.branch;
    while p <> root do
    begin
        b1 := p.branch;
        p.branch := b2;
        SwapMaxlen(p.branch);
        b2 := b1;
        p.anc := d;
        if d = p.des1 then
            p.des1 := a
        else
            p.des2 := a;
        d := p;
        p := a;
        a := a^.anc;
    end;
    if d = p.des1 then
    begin
        p.des2.anc := d;
        p.des2.branch := b0;
        if p = d.des1 then
            d.des1 := p.des2
        else
            d.des2 := p.des2;
    end
    else
    begin
        p.des1.anc := d;
        p.des1.branch := b0;
        if p = d.des1 then
            d.des1 := p.des1
        else
            d.des2 := p.des1;
    end;

    p := newposition.anc;
    p.anc := root;
    newposition.anc := root;
    if rootmode = 1 then
    begin
        root.des1 := newposition;
        root.des2 := p;
    end
    else
    begin
        root.des1 := p;
        root.des2 := newposition;
    end;
    b0 := newposition.branch;
    b1 := b0;
    b2 := b0;
    if ABS(b0.maxlen1 -b0.maxlen2) <= b0.length then
    begin
        b1.maxlen1 := (b0.maxlen1+b0.maxlen2-b0.length)/2;
        b1.length := b0.maxlen2 - b1.maxlen1;
        b2.length := b0.maxlen1 - b1.maxlen1;
    end
    else
    begin
        b1.length := b0.length*b0.maxlen2/(b0.maxlen1 +b0.maxlen2);
        b2.length := b0.length*b0.maxlen1/(b0.maxlen1 +b0.maxlen2);
    end;

    b1.maxlen1 := b0.maxlen2;
    b1.maxlen2 := b0.maxlen1 -b2.length;
    b2.maxlen1 := b0.maxlen1;
    b2.maxlen2 := b0.maxlen2 -b1.length;

    p.branch := b1;
    newposition.branch := b2;
end;

function GetEarliestAndLatestDates(const p: TpNode; var earliestDate: String; var latestDate: String): Boolean;
var
  aEarliest, aLatest: TDateTime;
begin
  aEarliest := MaxDateTime;
  aLatest := MinDateTime;
  Result := GetEarliestAndLatestDates(p, aEarliest, aLatest);
  if Result then
  begin
    latestDate := FormatDateTime(DefaultFormatSettings.ShortDateFormat, aLatest);
    earliestDate := FormatDateTime(DefaultFormatSettings.ShortDateFormat, aEarliest);
  end;
end;

function GetEarliestAndLatestDates(const p: TpNode; var earliestDate: Double; var latestDate: Double): Boolean;
var
  aEarliest, aLatest: TDateTime;
  temp: TDateTime;
  aInfo: TOtuInfo = nil;

  procedure ProcessNode(n: TpNode);
  begin
    if Assigned(n.des1) then
      ProcessNode(n.des1);
    if Assigned(n.des2) then
      ProcessNode(n.des2);
    if n.Otu then
    begin
      if Assigned(n.otuInfo) then
      begin
        aInfo := TOtuInfo(n.otuInfo);
        if aInfo.HasDateInfo then
        begin
          if TryEncodeDateTime(aInfo.Year, ord(aInfo.Month), aInfo.Day, 0, 0, 0, 0, temp) then
          begin
            if CompareDate(temp, aLatest) > 0 then
              aLatest := temp;
            if CompareDate(temp, aEarliest) < 0 then
              aEarliest := temp;
          end;
        end;
      end;
    end;
  end;

begin
  Result := False;
  aEarliest := MaxDateTime;
  aLatest := MinDateTime;
  ProcessNode(p);
  Result := (CompareDate(aEarliest, MaxDateTime) < 0) and (CompareDate(aLatest, MinDateTime) > 0);
  if Result then
  begin
    earliestDate := aEarliest;
    latestDate := aLatest;
  end;
end;

procedure MarkTempOutgroup(outgroupRoot: TpNode; ingroupRoot: TpNode);

  procedure MarkNode(n: TpNode; isOutgroup: Boolean);
  begin
    n.tempOutgroup := isOutgroup;
    if Assigned(n.des1) then
      MarkNode(n.des1, isOutgroup);
    if Assigned(n.des2) then
      MarkNode(n.des2, isOutgroup);
  end;

begin
  MarkNode(outgroupRoot, True);
  MarkNode(ingroupRoot, False);
end;

procedure SortBranchByFigure(p : TpNode);
begin
    if p = nil then Exit;
    if p^.OTU then Exit;
    if p^.anc = nil then begin
        if p^.des1^.size < p^.des2^.size then
            SwapNode(p^.des1, p^.des2);
    end
    else
        if p = p^.anc^.des1 then begin
            if p^.des1^.size < p^.des2^.size then
                SwapNode(p^.des1, p^.des2);
        end
        else if p = p^.anc^.des2 then begin
            if p^.des1^.size > p^.des2^.size then
                 SwapNode(p^.des1, p^.des2);
        end;
    if p^.des1^.size = p^.des2^.size then
        if p^.des1^.minOTU > p^.des2^.minOTU then
            SwapNode(p^.des1, p^.des2);
    SortBranchByFigure(p^.des1);
    SortBranchByFigure(p^.des2);

end;

procedure SortBranchByOrder(p : TpNode);
begin
    if p^.OTU then Exit;
    if p^.des1^.minOTU > p^.des2^.minOTU then
        SwapNode(p^.des1,p^.des2);
    SortBranchByOrder(p^.des1);
    SortBranchByOrder(p^.des2);
end;

procedure ChangeRootSimple(var Root: TSimpleTreeNode; var NewPosition: TSimpleTreeNode);
var
  a, p, d: TSimpleTreeNode;
  rootmode: integer;

begin
  if newposition = root then Exit;
  if newposition.Ancestor = root then
    Exit;

  { find if newposition is in des1 lineage or des2 lineage}
  p := newposition;
  while p.Ancestor <> root do
      p := p.Ancestor;
  if p = p.ancestor.des1 then
      rootmode := 1
  else
      rootmode := 2;

  { swap siblings in the lineage that newposition is in}
  p := newposition;
  if rootmode = 1 then
  begin
    while p.ancestor <> root do
    begin
      if p = p.ancestor.des1 then
        SwapNodeSimple(p.ancestor.des1, p.ancestor.des2);
      p := p.ancestor;
    end;
  end
  else
  begin
    while p.ancestor <> root do
    begin
      if p = p.ancestor.des2 then
        SwapNodeSimple(p.ancestor.des1, p.ancestor.des2);
      p := p.ancestor;
    end;
  end;

  d := newposition;
  p := d.ancestor;
  a := p.ancestor;
  while p <> root do
  begin
      p.ancestor := d;
      if d = p.des1 then
          p.des1 := a
      else
          p.des2 := a;
      d := p;
      p := a;
      a := a.ancestor;
  end;
  if d = p.des1 then
  begin
      p.des2.ancestor := d;
      if p = d.des1 then
          d.des1 := p.des2
      else
          d.des2 := p.des2;
  end
  else
  begin
      p.des1.ancestor := d;
      if p = d.des1 then
          d.des1 := p.des1
      else
          d.des2 := p.des1;
  end;

  p := newposition.ancestor;
  p.ancestor := root;
  newposition.ancestor := root;
  if rootmode = 1 then
  begin
      root.des1 := newposition;
      root.des2 := p;
  end
  else
  begin
      root.des1 := p;
      root.des2 := newposition;
  end;
end;

function SibNodeSimple(P: TSimpleTreeNode): TSimpleTreeNode;
begin
  if not Assigned(p.Ancestor)  then
    Result := nil
  else if P = P.Ancestor.Des1 then
    Result := P.Ancestor.Des2
  else if P = P.Ancestor.Des2 then
    Result := P.Ancestor.Des1
  else
    Result := nil;
end;

procedure GetTipDistancesFromAncestor(const ancestor: TpNode; var tipNodes: TList);

  procedure GetDistance(n: TpNode);
  begin
    if Assigned(n.des1) then
    begin
      n.des1.tempValue := n.tempValue + n.des1.branch.length;
      GetDistance(n.des1);
    end;
    if Assigned(n.des2) then
    begin
      n.des2.tempValue := n.tempValue + n.des2.branch.length;
      GetDistance(n.des2);
    end;
    if n.OTU then
      tipNodes.Add(n);
  end;

begin
  tipNodes.Clear;
  ancestor.tempValue := 0.0;
  GetDistance(ancestor);
end;

procedure SwapNodeSimple(var First, Second: TSimpleTreeNode);
var
  Temp: TSimpleTreeNode;
begin
  Temp := First;
  First := Second;
  Second := Temp;
end;
end.

