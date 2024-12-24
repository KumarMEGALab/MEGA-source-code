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

unit mreltimetreenode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TRelTimeTreeNode }

  TRelTimeTreeNode = class(TObject)
    private
      Fanc   : TRelTimeTreeNode;
      Fdes1  : TRelTimeTreeNode;
      Fdes2  : TRelTimeTreeNode;

      {
        Please do not use private variables of type 'extended' directly but
        rather use their public properties so that in their 'Setvalue' procedures
        we can detect invalid values. The problem we have is that floating point
        division by zero does not raise an exception, even though it is an error.
        From the FreePascal and Delphi documentation for the Math unit:
             const NaN = 0.0 / 0.0;
             const Infinity =  1.0 / 0.0;
        So floating point division by zero returns Nan or Infinity and the problem
        is not detected. But in the 'Setvalue' procedures, we have a check for these
        so an exception is raised and we can fix the problem code
      }
      Fblen   : extended;
      FRate   : extended;
      FHeight : extended;
      Fvb     : extended;
      Fvr     : extended;
      Fmaxh   : extended;
      Fminh   : extended;
      Fh0     : extended;
      Fha0    : extended;
      Fvh     : extended;
      Fvh0    : extended;
      Fvh1    : extended;
      Fvh2    : extended;
      Frtd0   : extended;
      Frtd1   : extended;
      FTime   : extended;
      FDataCoverage: Double; { the proportion of sites for which at least one taxon from each descendent lineage has informative data}
      procedure SetBlen(AValue: Extended);
      procedure SetDataCoverage(AValue: Double);
      procedure SetH0(AValue: extended);
      procedure SetHa0(AValue: extended);
      procedure SetHeight(AValue: Extended);
      procedure SetMaxh(AValue: extended);
      procedure SetMinh(AValue: extended);
      procedure SetRate(AValue: Extended);
      procedure SetRtd0(AValue: extended);
      procedure SetRtd1(AValue: extended);
      procedure SetVb(AValue: extended);
      procedure SetVh(AValue: extended);
      procedure SetVh0(AValue: extended);
      procedure SetVh1(AValue: extended);
      procedure SetVh2(AValue: extended);
      procedure SetVr(AValue: extended);
    public
      index  : integer;
      depth  : integer;
      size   : integer;
      flag  : boolean;
      done  : boolean;
      anchored: boolean;
      fixed : boolean;
      IsOutgroupMember: Boolean;
      des1Index, des2Index: Integer; { these are only used for reading session files}
      h1      : extended;
      h2      : extended;
      constructor Create(AIndex: Integer);
      destructor Destroy; override;
      function DebugString(comments: String=''): String;
      function DebugStringOneLine: String;
      class function DebugHeaderStringOneLine: String; static;
      function OTU: boolean;
      procedure WriteToFile(var aFile: File);
      procedure ReadFromFile(var aFile: File);
      function CorrTestRate: Extended;
      function BlenIsZero: Boolean;
      procedure Assign(Source: TReltimeTreeNode);
      property blen: Extended read Fblen write SetBlen;
      property rate: Extended read FRate write SetRate;
      property height: Extended read FHeight write SetHeight;
      property vb : extended read FVb write SetVb;
      property vr : extended read FVr write SetVr;
      property maxh : extended read FMaxh write SetMaxh;
      property minh : extended read FMinh write SetMinh;
      property h0 : extended read FH0 write SetH0;
      property ha0 : extended read FHa0 write SetHa0;
      property vh : extended read FVh write SetVh;
      property vh0 : extended read FVh0 write SetVh0;
      property vh1 : extended read FVh1 write SetVh1;
      property vh2 : extended read FVh2 write SetVh2;
      property rtd0: extended read Frtd0 write SetRtd0;
      property rtd1: extended read Frtd1 write SetRtd1;
      property Time: extended read FTime write FTime;
      property anc: TRelTimeTreeNode read fanc write fanc;
      property des1: TRelTimeTreeNode read fdes1 write fdes1;
      property des2: TRelTimeTreeNode read fdes2 write fdes2;
      property DataCoverage: Double read FDataCoverage write SetDataCoverage;
  end;
  TRelTimeTreeNodeArray = array of TRelTimeTreeNode;

implementation

uses
  math, MegaConsts;

{ TReltimeTreeNode }

procedure TRelTimeTreeNode.Assign(Source: TReltimeTreeNode);
begin
  index  := source.index;
  anc    := source.anc;
  des1   := source.des1;
  des2   := source.des2;
  size   := source.size;
  depth  := source.depth;
  blen   := source.blen;
  vb     := source.vb;
  rate   := source.rate;
  vr     := source.vr;
  height := source.height;
  maxh   := source.maxh;
  minh   := source.minh;
  h0     := source.h0;
  h1     := source.h1;
  h2     := source.h2;
  ha0    := source.ha0;
  vh     := source.vh;
  vh0    := source.vh0;
  vh1    := source.vh1;
  vh2    := source.vh2;
  rtd0   := source.rtd0;
  rtd1   := source.rtd1;
  Time   := source.Time;
  flag     := source.flag;
  done     := source.done;
  anchored := source.anchored;
  fixed    := source.fixed;
  DataCoverage := source.DataCoverage;
end;

procedure TRelTimeTreeNode.SetRate(AValue: Extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('rate for node ' + IntToStr(Index) + ' is not a valid number (probably division by zero)');
  if FRate=AValue then Exit;
  FRate:=AValue;
end;

procedure TRelTimeTreeNode.SetRtd0(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('rtd0 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if Frtd0=AValue then Exit;
  Frtd0:=AValue;
end;

procedure TRelTimeTreeNode.SetRtd1(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('rtd1 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if Frtd1=AValue then Exit;
  Frtd1:=AValue;
end;

procedure TRelTimeTreeNode.SetVb(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('vb for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FVb=AValue then Exit;
  FVb:=AValue;
end;

procedure TRelTimeTreeNode.SetVh(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('Vh for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FVh=AValue then Exit;
  FVh:=AValue;
end;

procedure TRelTimeTreeNode.SetVh0(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('vh0 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FVh0=AValue then Exit;
  FVh0:=AValue;
end;

procedure TRelTimeTreeNode.SetVh1(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('vh1 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FVh1=AValue then Exit;
  FVh1:=AValue;
end;

procedure TRelTimeTreeNode.SetVh2(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('vh2 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FVh2=AValue then Exit;
  FVh2:=AValue;
end;

procedure TRelTimeTreeNode.SetVr(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('vr for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FVr=AValue then Exit;
  FVr:=AValue;
end;

procedure TRelTimeTreeNode.WriteToFile(var aFile: File);
var
  desIndex: Integer;
begin
  BlockWrite(aFile, FBlen, SizeOf(Extended));
  BlockWrite(aFile, FRate, SizeOf(Extended));
  BlockWrite(aFile, FHeight, SizeOf(Extended));
  BlockWrite(aFile, Fvb, SizeOf(Extended));
  BlockWrite(aFile, Fvr, SizeOf(Extended));
  BlockWrite(aFile, Fmaxh, SizeOf(Extended));
  BlockWrite(aFile, Fminh, SizeOf(Extended));
  BlockWrite(aFile, Fh0, SizeOf(Extended));
  BlockWrite(aFile, Fha0, SizeOf(Extended));
  BlockWrite(aFile, Fvh, SizeOf(Extended));
  BlockWrite(aFile, Fvh0, SizeOf(Extended));
  BlockWrite(aFile, Fvh1, SizeOf(Extended));
  BlockWrite(aFile, Fvh2, SizeOf(Extended));
  BlockWrite(aFile, Frtd0, SizeOf(Extended));
  BlockWrite(aFile, Frtd1, SizeOf(Extended));
  BlockWrite(aFile, FTime, SizeOf(Extended));
  BlockWrite(aFile, FDataCoverage, SizeOf(Double));
  BlockWrite(aFile, h1, SizeOf(Extended));
  BlockWrite(aFile, h2, SizeOf(Extended));
  BlockWrite(aFile, index, SizeOf(Integer));
  BlockWrite(aFile, depth, SizeOf(Integer));
  BlockWrite(aFile, size, SizeOf(Integer));
  BlockWrite(aFile, flag, SizeOf(Boolean));
  BlockWrite(aFile, done, SizeOf(Boolean));
  BlockWrite(aFile, anchored, SizeOf(Boolean));
  BlockWrite(aFile, fixed, SizeOf(Boolean));
  BlockWrite(aFile, IsOutgroupMember, SizeOf(Boolean));
  if Assigned(des1) then
    desIndex := des1.Index
  else
    desIndex := -1;
  BlockWrite(aFile, desIndex, SizeOf(Integer));
  if Assigned(des2) then
    desIndex := des2.Index
  else
    desIndex := -1;
  BlockWrite(aFile, desIndex, SizeOf(Integer));
end;

procedure TRelTimeTreeNode.SetHeight(AValue: Extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('height for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TRelTimeTreeNode.SetH0(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('h0 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FH0=AValue then Exit;
  FH0:=AValue;
end;

procedure TRelTimeTreeNode.SetBlen(AValue: Extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('blen for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if Fblen=AValue then Exit;
  Fblen:=AValue;
end;

procedure TRelTimeTreeNode.SetDataCoverage(AValue: Double);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('DataCoverage for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FDataCoverage=AValue then Exit;
  FDataCoverage:=AValue;
end;

procedure TRelTimeTreeNode.SetHa0(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('ha0 for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FHa0=AValue then Exit;
  FHa0:=AValue;
end;

procedure TRelTimeTreeNode.SetMaxh(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('maxh for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FMaxh=AValue then Exit;
  FMaxh:=AValue;
end;

procedure TRelTimeTreeNode.SetMinh(AValue: extended);
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    raise Exception.Create('minh for node ' + IntToStr(Index) + ' is not a valid value (probably division by zero)');
  if FMinh=AValue then Exit;
  FMinh:=AValue;
end;

function TRelTimeTreeNode.OTU: boolean;
begin
  result := (fdes1 = nil) or (fdes2 = nil);
end;

procedure TRelTimeTreeNode.ReadFromFile(var aFile: File);
begin
  BlockRead(aFile, FBlen, SizeOf(Extended));
  BlockRead(aFile, FRate, SizeOf(Extended));
  BlockRead(aFile, FHeight, SizeOf(Extended));
  BlockRead(aFile, Fvb, SizeOf(Extended));
  BlockRead(aFile, Fvr, SizeOf(Extended));
  BlockRead(aFile, Fmaxh, SizeOf(Extended));
  BlockRead(aFile, Fminh, SizeOf(Extended));
  BlockRead(aFile, Fh0, SizeOf(Extended));
  BlockRead(aFile, Fha0, SizeOf(Extended));
  BlockRead(aFile, Fvh, SizeOf(Extended));
  BlockRead(aFile, Fvh0, SizeOf(Extended));
  BlockRead(aFile, Fvh1, SizeOf(Extended));
  BlockRead(aFile, Fvh2, SizeOf(Extended));
  BlockRead(aFile, Frtd0, SizeOf(Extended));
  BlockRead(aFile, Frtd1, SizeOf(Extended));
  BlockRead(aFile, FTime, SizeOf(Extended));
  BlockRead(aFile, FDataCoverage, SizeOf(Double));
  BlockRead(aFile, h1, SizeOf(Extended));
  BlockRead(aFile, h2, SizeOf(Extended));
  BlockRead(aFile, index, SizeOf(Integer));
  BlockRead(aFile, depth, SizeOf(Integer));
  BlockRead(aFile, size, SizeOf(Integer));
  BlockRead(aFile, flag, SizeOf(Boolean));
  BlockRead(aFile, done, SizeOf(Boolean));
  BlockRead(aFile, anchored, SizeOf(Boolean));
  BlockRead(aFile, fixed, SizeOf(Boolean));
  BlockRead(aFile, IsOutgroupMember, SizeOf(Boolean));
  BlockRead(aFile, des1Index, SizeOf(Integer));
  BlockRead(aFile, des2Index, SizeOf(Integer));
end;

function TRelTimeTreeNode.CorrTestRate: Extended;
var
  timeElapsed: Extended;
begin

  if CompareValue(blen, 0.0, FP_CUTOFF) = 0 then
    Result := 0.0
  else if Assigned(anc) then
  begin
    timeElapsed := (anc.height - height);
    if CompareValue(timeElapsed, 0.0, FP_CUTOFF) = 0 then
      Result := 0.0
    else
      Result := blen/timeElapsed;
  end
  else
    Result := rate;
end;

function TRelTimeTreeNode.BlenIsZero: Boolean;
begin
  Result := (CompareValue(blen, 0.0, FP_CUTOFF) = 0);
end;

constructor TRelTimeTreeNode.Create(AIndex: Integer);
begin
  inherited Create;

  index := AIndex;
  rate  := 1;
  done := False;
  IsOutgroupMember := False;
end;

destructor TRelTimeTreeNode.Destroy;
begin
  inherited;
end;

function TRelTimeTreeNode.DebugString(comments: String): String;
var
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    aList.Add(Format('index    = %d', [index]));
    aList.Add(Format('depth    = %d', [depth]));
    aList.Add(Format('size     = %d', [size]));
    aList.Add(Format('flag     = %s', [BoolToStr(flag, True)]));
    aList.Add(Format('done     = %s', [BoolToStr(done, True)]));
    aList.Add(Format('anchored = %s', [BoolToStr(anchored, True)]));
    aList.Add(Format('fixed    = %s', [BoolToStr(fixed, True)]));
    aList.Add(Format('outgroup = %s', [BoolToStr(IsOutgroupMember, True)]));
    aList.Add(Format('anc      = %d', [Fanc.index]));
    aList.Add(Format('des1     = %d', [Fdes1.index]));
    aList.Add(Format('des2     = %d', [Fdes2.index]));
    aList.Add(Format('blen     = %.4f', [Fblen]));
    aList.Add(Format('rate     = %.4f', [FRate]));
    aList.Add(Format('height   = %.4f', [FHeight]));
    aList.Add(Format('vb       = %.4f', [Fvb]));
    aList.Add(Format('vr       = %.4f', [Fvr]));
    aList.Add(Format('maxh     = %.4f', [Fmaxh]));
    aList.Add(Format('minh     = %.4f', [Fminh]));
    aList.Add(Format('h0       = %.4f', [Fh0]));
    aList.Add(Format('ha0      = %.4f', [Fha0]));
    aList.Add(Format('vh       = %.4f', [Fvh]));
    aList.Add(Format('vh0      = %.4f', [Fvh0]));
    aList.Add(Format('vh1      = %.4f', [Fvh1]));
    aList.Add(Format('vh2      = %.4f', [Fvh2]));
    aList.Add(Format('rtd0     = %.4f', [Frtd0]));
    aList.Add(Format('rtd1     = %.4f', [Frtd1]));
    aList.Add(Format('time     = %.4f', [FTime]));
    aList.Add(Format('coverage = %.4f', [FDataCoverage]));
    aList.Add(Format('h1       = %.4f', [h1]));
    aList.Add(Format('h2       = %.4f', [h2]));
    if Trim(comments) <> EmptyStr then
    begin
      aList.Add(EmptyStr);
      aList.Add('Comments:');
      aList.Add(comments);
    end;
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TRelTimeTreeNode.DebugStringOneLine: String;
var
  ancIndex, d1Index, d2Index: Integer;
begin
  if Self.OTU then
  begin
    d1Index := -1;
    d2Index := -1;
  end
  else
  begin
    d1Index := des1.index;
    d2Index := des2.index;
  end;

  if Assigned(anc) then
    ancIndex := anc.index
  else
    ancIndex := -1;
  Result := Format('%d,%d,%d,%s,%s,%s,%s,%s,%d,%d,%d,%.3e,%.4f,%.3e,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%.3e', [index,depth,size,BoolToStr(flag, True),BoolToStr(done, True),BoolToStr(anchored, True),BoolToStr(fixed, True),BoolToStr(IsOutgroupMember, True),ancIndex,d1Index,d2Index,Fblen,FRate,FHeight,Fmaxh,Fminh,FTime,FDataCoverage,Frtd0,Frtd1,FH0]);
end;

class function TRelTimeTreeNode.DebugHeaderStringOneLine: String; static;
begin
  Result := 'index,depth,size,flag,done,anchored,fixed,outgroup,anc,des1,des2,blen,rate,height,maxh,minh,time,coverage,rtd0,rtd1,h0';
end;

end.

