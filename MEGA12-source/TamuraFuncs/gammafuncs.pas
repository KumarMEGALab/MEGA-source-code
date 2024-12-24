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

unit GammaFuncs;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Math;

procedure GenerateDiscreteGammaRates(gamma: extended; n: integer; var rate: array of extended);
function FitGammaRates(n: integer; var rate: array of extended): extended;   // return best fit gamma value;
procedure InitGammaX;
procedure InitGammaY1;
procedure InitGammaY2;
procedure InitGammaY3;
procedure InitGammaY4;
procedure InitGammaY5;
procedure InitGammaY6;
procedure InitGammaY7;
procedure InitGammaY8;
procedure InitGammaY9;
procedure InitGammaY10;
procedure InitGammaY11;
procedure InitGammaY12;
procedure InitGammaY13;
procedure InitGammaY14;
procedure InitGammaY15;
procedure InitGammaY16;
procedure InitGammaY17;
procedure InitGammaY18;
procedure InitGammaY19;
procedure InitGammaY20;
procedure InitGammaY21;
procedure InitGammaY22;
procedure InitGammaY23;
procedure InitGammaY24;
procedure InitGammaY25;
procedure InitGammaY26;
procedure InitGammaY27;
procedure InitGammaY28;
procedure InitGammaY29;
procedure InitGammaY30;
procedure InitGammaY31;
procedure InitGammaY32;
procedure InitGammaY33;
procedure InitGammaY34;
procedure InitGammaY35;
procedure InitGammaY36;
procedure InitGammaY37;
procedure InitGammaY38;
procedure InitGammaY39;
procedure InitGammaY40;
procedure InitGammaY41;
procedure InitGammaY42;
procedure InitGammaY43;
procedure InitGammaY44;
procedure InitGammaY45;
procedure InitGammaY46;
procedure InitGammaY47;
procedure InitGammaY48;
procedure InitGammaY49;
procedure InitGammaY50;
procedure InitGammaY51;
procedure InitGammaY52;
procedure InitGammaY53;
procedure InitGammaY54;
procedure InitGammaY55;
procedure InitGammaY56;
procedure InitGammaY57;
procedure InitGammaY58;
procedure InitGammaY59;
procedure InitGammaY60;
procedure InitGammaY61;
procedure InitGammaY62;
procedure InitGammaY63;
procedure InitGammaY64;
procedure InitGammaY65;
procedure InitGammaY66;
procedure InitGammaY67;
procedure InitGammaY68;
procedure InitGammaY69;
procedure InitGammaY70;
procedure InitGammaY71;
procedure InitGammaY72;
procedure InitGammaY73;
procedure InitGammaY74;
procedure InitGammaY75;
procedure InitGammaY76;
procedure InitGammaY77;
procedure InitGammaY78;
procedure InitGammaY79;
procedure InitGammaY80;
procedure InitGammaY81;
procedure InitGammaY82;
procedure InitGammaY83;
procedure InitGammaY84;
procedure InitGammaY85;
procedure InitGammaY86;
procedure InitGammaY87;
procedure InitGammaY88;
procedure InitGammaY89;
procedure InitGammaY90;
procedure InitGammaY91;
procedure InitGammaY92;
procedure InitGammaY93;
procedure InitGammaY94;
procedure InitGammaY95;
procedure InitGammaY96;
procedure InitGammaY97;
procedure InitGammaY98;
procedure InitGammaY99;
procedure InitGammaY100;
//procedure InitGammaY101;
procedure InitGammaY;

implementation

var
  GammaX: array [0..99] of extended;
  GammaY: array [0..99,0..99] of extended;
  GammaTable: array [0..99,0..99] of extended;

procedure MakeGammaRate(gamma: extended; var r: array of extended);

  function f(value: extended): extended;
  begin
    result := (value*value -1)*value;
  end;

var
  i,j0,j1,k: integer;
  t,h,p,v: extended;
begin
  v := ln(gamma);

  for i := 0 to 99 do
  begin
    p := GammaX[99] -GammaX[0];
    while v > GammaX[99] do
      v := v -p;
    while v < GammaX[0] do
      v := v +p;
    j0 := 0;
    j1 := 99;
    while j0 < j1 do
    begin
      k := (j0+j1) div 2;
      if GammaX[k] < v then j0 := k+1 else j1 := k;
    end;
    if j0 > 0 then j0 := j0-1;
    h := GammaX[j0+1] -GammaX[j0];
    t := (v -GammaX[j0])/h;

    r[i] := t*GammaY[i,j0+1] +(1 -t)*GammaY[i,j0] +h*h*(f(t)*GammaTable[i,j0+1] +f(1-t)*GammaTable[i,j0]);
  end;
end;

procedure InitializeGammaTable;
var
  h,d: array of extended;
  t,u: extended;
  i,j: integer;
begin
  setlength(h,100);
  setlength(d,100);

  for i := 0 to 99 do
  begin
    for j := 0 to 98 do
      h[j] := GammaX[j+1] -GammaX[j];
    for j := 1 to 98 do
      d[j] := 2*(GammaX[j+1]-GammaX[j-1]);
    u := (GammaY[i,1] -GammaY[i,0])/h[0];
    for j := 1 to 98 do
    begin
      t := (GammaY[i,j+1] -GammaY[i,j])/h[j];
      GammaTable[i,j] := t -u;
      u := t;
    end;
    GammaTable[i,0]  := 0;
    GammaTable[i,99] := 0;
    GammaTable[i,1]  := GammaTable[i,1] -h[0]*GammaTable[i,0];
    GammaTable[i,98] := GammaTable[i,98] -h[98]*GammaTable[i,99];
    for j := 1 to 97 do
    begin
      t := h[j]/d[j];
      GammaTable[i,j+1] := GammaTable[i,j+1] -GammaTable[i,j]*t;
      d[j+1] := d[j+1] -h[j]*t;
    end;
    for j := 98 downto 1 do
      GammaTable[i,j] := (GammaTable[i,j] -h[j]*GammaTable[i,j+1])/d[j];
  end;

  setlength(h,0);
  setlength(d,0);
end;

procedure GenerateDiscreteGammaRates(gamma: extended; n: integer; var rate: array of extended);
var
  r: array [0..99] of extended;
  i,j: integer;
  d1,d2: extended;
begin
  for i := Low(r) to High(r) do
    r[i] := 0;
  MakeGammaRate(gamma, r);
  j := 0;
  for i := 0 to n-1 do
  begin
    d1 := i/n;
    d2 := (i+1)/n;
    rate[i] := 0;
    while (j/100 <= d1) and (j < 99) do
      inc(j);
    rate[i] := rate[i] +(j/100 -d1)*r[j-1];

    while ((j+1)/100 < d2) and (j < 99) do
    begin
      rate[i] := rate[i] +r[j]/100;
      inc(j);
    end;

    rate[i] := rate[i] +(d2-j/100)*r[j];

    rate[i] := rate[i]*n;
  end;
end;

function FitGammaRates(n: integer; var rate: array of extended): extended;
var
  d1da,d2da,d3da: array [0..99,0..99] of extended;
  y: array [0..99,0..99] of extended;
  delta: extended;

  procedure MakeSpline(var x,y,b,c,d: array of extended);
  var
    a,h,l,m,z: array of extended;
    i: integer;
  begin
    setlength(a,100);
    setlength(h,100);
    setlength(l,100);
    setlength(m,100);
    setlength(z,100);

    for i := 0 to 98 do
      h[i] := x[i+1] -x[i];
    for i := 1 to 98 do
      a[i] := 3/h[i]*(y[i+1]-y[i]) -3/h[i-1]*(y[i]-y[i-1]);
    l[0] := 1;
    m[0] := 0;
    z[0] := 0;
    for i := 1 to 98 do
    begin
      l[i] := 2*(x[i+1] -x[i-1]) -h[i-1]*m[i-1];
      m[i] := h[i]/l[i];
      z[i] := (a[i] -h[i-1]*z[i-1])/l[i];
    end;
    l[99] := 1;
    z[99] := 0;
    c[99] := 0;
    for i := 98 downto 0 do
    begin
      c[i] := z[i] -m[i]*c[i+1];
      b[i] := (y[i+1] -y[i])/h[i] -h[i]*(c[i+1] +2*c[i])/3;
      d[i] := (c[i+1] -c[i])/3/h[i];
    end;

    setlength(a,0);
    setlength(h,0);
    setlength(l,0);
    setlength(m,0);
    setlength(z,0);
  end;

  function Interpolation(ln_gamma: extended; i: integer): extended;
  var
    j: integer;
    delta: extended;
  begin
    j := 99;
    while ln_gamma < GammaX[j] do
      dec(j);
    delta := ln_gamma -GammaX[j];

    result := y[i,j] +(d1da[i,j] +(d2da[i,j] +d3da[i,j]*delta)*delta)*delta;
    if result < 0 then result := 0;
  end;

  function ecr(i,j: integer): extended;
  begin
    result := (y[i,j] +(d1da[i,j] +(d2da[i,j] +d3da[i,j]*delta)*delta)*delta)*n;

    if result < 0 then
      result := 0;
  end;

  function d1(i,j: integer): extended;
  begin
    result := (d1da[i,j] +(2*d2da[i,j] +3*d3da[i,j]*delta)*delta)*n;
  end;

  function d2(i,j: integer): extended;
  begin
    result := (2*d2da[i,j] +6*d3da[i,j]*delta)*n;
  end;

  function R(j: integer): extended;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to n-1 do
      result := result +(ecr(i,j)-rate[i])*(ecr(i,j)-rate[i]);
  end;

  function dR(j: integer): extended;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to n-1 do
      result := result +(ecr(i,j)-rate[i])*d1(i,j);
  end;

  function ddR(j: integer): extended;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to n-1 do
      result := result +(d1(i,j)*d1(i,j) +d2(i,j)*(ecr(i,j)-rate[i]));
  end;

  function R2(j: integer): extended;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to n-1 do
      if ecr(i,j) > 0 then
        result := result +(ecr(i,j)-rate[i])*(ecr(i,j)-rate[i]);
  end;

var
  d,dd,mind,r0,r1: extended;
  i,j,k,rep,jj: integer;

  p1,p2: extended;
begin
  for i := 0 to 99 do
    for j := 0 to 99 do
    begin
      d1da[i][j] := 0.0;
      d2da[i][j] := 0.0;
      d3da[i][j] := 0.0;
    end;
  for j := 0 to 99 do
  begin
    i := 0;
    for k := 0 to n-1 do
    begin
      p1 := k/n;
      p2 := (k+1)/n;
      y[k,j] := 0;
      while (i/100 <= p1) and (i < 99) do
        inc(i);
      y[k,j] := y[k,j] +(i/100 -p1)*GammaY[i-1,j];

      while ((i+1)/100 < p2) and (i < 99) do
      begin
        y[k,j] := y[k,j] +GammaY[i,j]/100;
        inc(i);
      end;

      y[k,j] := y[k,j] +(p2-i/100)*GammaY[i,j];
    end;
  end;

  for i := 0 to n-1 do
    MakeSpline(GammaX, y[i], d1da[i], d2da[i], d3da[i]);

  r0 := 0;
  for k := 0 to n-1 do
    r0 := r0 +(y[k,99]*n-rate[k])*(y[k,99]*n-rate[k]);
  j  := 99;
  jj := 99;
  repeat
    dec(j,1);
    r1 := 0;
    for k := 0 to n-1 do
      r1 := r1 +(y[k,j]*n-rate[k])*(y[k,j]*n-rate[k]);
    if r1 < r0 then
    begin
      r0 := r1;
      jj := j;
    end;
  until j = 2;
  j := jj;

{
  if result < 0.04 then
    result := exp(GammaX[j])
  else
  begin
    maxg := sqrt(sqrt(sqrt(2)))*result;
    ming := result/sqrt(sqrt(sqrt(2)));
    result := exp(GammaX[j]);
    if result > maxg then
      result := maxg
    else if result < ming then
      result := ming;
  end;
}

  result := exp(GammaX[j]);
  delta := 0;
  d := R(j);
  if d < 0.0001 then
    mind := 0.000000000001
  else
    mind := power(10, floor(log10(d))-8);
  rep := 0;
  repeat
    if abs(ddR(j)) > 0.000000000001 then
      if (ln(result) -dR(j)/ddR(j)) > GammaX[99] then
        result := exp(GammaX[99])
      else if (ln(result) -dR(j)/ddR(j)) < GammaX[0] then
        result := exp(GammaX[0])
      else
        result := exp(ln(result) -dR(j)/ddR(j));
    if (j < 99) and (result > exp(GammaX[j+1])) then
    begin
      if j < 97 then
        inc(j);
      result := exp(GammaX[j]);
    end
    else if (j > 0) and (result < exp(GammaX[j-1])) then
    begin
      if j > 2 then
        dec(j);
      result := exp(GammaX[j]);
    end;
    delta := (ln(result) -GammaX[j]);

    dd := d -R(j);
    d  := d -dd;
    inc(rep);
  until (abs(dd) < mind) or (rep = 100);

  if result < 0.05 then
    result := 0.05
  else if result > 200 then
    result := 200;
end;

procedure InitGammaX;
begin
  GammaX[0]  := -3.218875824868; GammaX[1]  := -3.132232427298; GammaX[2]  := -3.045589029728; GammaX[3]  := -2.958945632158; GammaX[4]  := -2.872302234588; GammaX[5]  := -2.785658837018; GammaX[6]  := -2.699015439448; GammaX[7]  := -2.612372041878; GammaX[8]  := -2.525728644308; GammaX[9]  := -2.439085246738;
  GammaX[10] := -2.352441849168; GammaX[11] := -2.265798451598; GammaX[12] := -2.179155054028; GammaX[13] := -2.092511656458; GammaX[14] := -2.005868258888; GammaX[15] := -1.919224861318; GammaX[16] := -1.832581463748; GammaX[17] := -1.745938066178; GammaX[18] := -1.659294668608; GammaX[19] := -1.572651271038;
  GammaX[20] := -1.486007873468; GammaX[21] := -1.399364475898; GammaX[22] := -1.312721078328; GammaX[23] := -1.226077680758; GammaX[24] := -1.139434283188; GammaX[25] := -1.052790885618; GammaX[26] := -0.966147488048; GammaX[27] := -0.879504090478; GammaX[28] := -0.792860692908; GammaX[29] := -0.706217295338;
  GammaX[30] := -0.619573897768; GammaX[31] := -0.532930500198; GammaX[32] := -0.446287102628; GammaX[33] := -0.359643705058; GammaX[34] := -0.273000307488; GammaX[35] := -0.186356909918; GammaX[36] := -0.099713512348; GammaX[37] := -0.013070114778; GammaX[38] :=  0.073573282792; GammaX[39] :=  0.160216680362;
  GammaX[40] :=  0.246860077932; GammaX[41] :=  0.333503475502; GammaX[42] :=  0.420146873072; GammaX[43] :=  0.506790270642; GammaX[44] :=  0.593433668211; GammaX[45] :=  0.680077065781; GammaX[46] :=  0.766720463351; GammaX[47] :=  0.853363860921; GammaX[48] :=  0.940007258491; GammaX[49] :=  1.026650656061;
  GammaX[50] :=  1.113294053631; GammaX[51] :=  1.199937451201; GammaX[52] :=  1.286580848771; GammaX[53] :=  1.373224246341; GammaX[54] :=  1.459867643911; GammaX[55] :=  1.546511041481; GammaX[56] :=  1.633154439051; GammaX[57] :=  1.719797836621; GammaX[58] :=  1.806441234191; GammaX[59] :=  1.893084631761;
  GammaX[60] :=  1.979728029331; GammaX[61] :=  2.066371426901; GammaX[62] :=  2.153014824471; GammaX[63] :=  2.239658222041; GammaX[64] :=  2.326301619611; GammaX[65] :=  2.412945017181; GammaX[66] :=  2.499588414751; GammaX[67] :=  2.586231812321; GammaX[68] :=  2.672875209891; GammaX[69] :=  2.759518607461;
  GammaX[70] :=  2.846162005031; GammaX[71] :=  2.932805402601; GammaX[72] :=  3.019448800171; GammaX[73] :=  3.106092197741; GammaX[74] :=  3.192735595311; GammaX[75] :=  3.279378992881; GammaX[76] :=  3.366022390451; GammaX[77] :=  3.452665788021; GammaX[78] :=  3.539309185591; GammaX[79] :=  3.625952583161;
  GammaX[80] :=  3.712595980731; GammaX[81] :=  3.799239378301; GammaX[82] :=  3.885882775871; GammaX[83] :=  3.972526173441; GammaX[84] :=  4.059169571011; GammaX[85] :=  4.145812968581; GammaX[86] :=  4.232456366151; GammaX[87] :=  4.319099763721; GammaX[88] :=  4.405743161291; GammaX[89] :=  4.492386558861;
  GammaX[90] :=  4.579029956431; GammaX[91] :=  4.665673354001; GammaX[92] :=  4.752316751571; GammaX[93] :=  4.838960149141; GammaX[94] :=  4.925603546711; GammaX[95] :=  5.012246944281; GammaX[96] :=  5.098890341851; GammaX[97] :=  5.185533739421; GammaX[98] :=  5.272177136991; GammaX[99] :=  5.358820534561;
end;

procedure InitGammaY;
begin
  InitGammaY1;
  InitGammaY2;
  InitGammaY3;
  InitGammaY4;
  InitGammaY5;
  InitGammaY6;
  InitGammaY7;
  InitGammaY8;
  InitGammaY9;
  InitGammaY10;
  InitGammaY11;
  InitGammaY12;
  InitGammaY13;
  InitGammaY14;
  InitGammaY15;
  InitGammaY16;
  InitGammaY17;
  InitGammaY18;
  InitGammaY19;
  InitGammaY20;
  InitGammaY21;
  InitGammaY22;
  InitGammaY23;
  InitGammaY24;
  InitGammaY25;
  InitGammaY26;
  InitGammaY27;
  InitGammaY28;
  InitGammaY29;
  InitGammaY30;
  InitGammaY31;
  InitGammaY32;
  InitGammaY33;
  InitGammaY34;
  InitGammaY35;
  InitGammaY36;
  InitGammaY37;
  InitGammaY38;
  InitGammaY39;
  InitGammaY40;
  InitGammaY41;
  InitGammaY42;
  InitGammaY43;
  InitGammaY44;
  InitGammaY45;
  InitGammaY46;
  InitGammaY47;
  InitGammaY48;
  InitGammaY49;
  InitGammaY50;
  InitGammaY51;
  InitGammaY52;
  InitGammaY53;
  InitGammaY54;
  InitGammaY55;
  InitGammaY56;
  InitGammaY57;
  InitGammaY58;
  InitGammaY59;
  InitGammaY60;
  InitGammaY61;
  InitGammaY62;
  InitGammaY63;
  InitGammaY64;
  InitGammaY65;
  InitGammaY66;
  InitGammaY67;
  InitGammaY68;
  InitGammaY69;
  InitGammaY70;
  InitGammaY71;
  InitGammaY72;
  InitGammaY73;
  InitGammaY74;
  InitGammaY75;
  InitGammaY76;
  InitGammaY77;
  InitGammaY78;
  InitGammaY79;
  InitGammaY80;
  InitGammaY81;
  InitGammaY82;
  InitGammaY83;
  InitGammaY84;
  InitGammaY85;
  InitGammaY86;
  InitGammaY87;
  InitGammaY88;
  InitGammaY89;
  InitGammaY90;
  InitGammaY91;
  InitGammaY92;
  InitGammaY93;
  InitGammaY94;
  InitGammaY95;
  InitGammaY96;
  InitGammaY97;
  InitGammaY98;
  InitGammaY99;
  InitGammaY100;
  //InitGammaY101;
end;

procedure InitGammaY1;
begin
  GammaY[0,0] := 3.62422E-50; GammaY[1,0] := 2.43218E-42; GammaY[2,0] := 6.14177E-38; GammaY[3,0] := 8.16717E-35; GammaY[4,0] := 2.16837E-32; GammaY[5,0] := 2.08236E-30; GammaY[6,0] := 9.92674E-29; GammaY[7,0] := 2.83559E-27; GammaY[8,0] := 5.47748E-26; GammaY[9,0] := 7.76881E-25;
  GammaY[10,0] := 8.57832E-24; GammaY[11,0] := 7.70009E-23; GammaY[12,0] := 5.80628E-22; GammaY[13,0] := 3.77319E-21; GammaY[14,0] := 2.15649E-20; GammaY[15,0] := 1.10188E-19; GammaY[16,0] := 5.10166E-19; GammaY[17,0] := 2.16433E-18; GammaY[18,0] := 8.49266E-18; GammaY[19,0] := 3.10684E-17;
  GammaY[20,0] := 1.06684E-16; GammaY[21,0] := 3.45881E-16; GammaY[22,0] := 1.06417E-15; GammaY[23,0] := 3.12085E-15; GammaY[24,0] := 8.75812E-15; GammaY[25,0] := 2.36003E-14; GammaY[26,0] := 6.12523E-14; GammaY[27,0] := 1.53535E-13; GammaY[28,0] := 3.7259E-13; GammaY[29,0] := 8.77298E-13;
  GammaY[30,0] := 2.00823E-12; GammaY[31,0] := 4.47723E-12; GammaY[32,0] := 9.73731E-12; GammaY[33,0] := 2.06893E-11; GammaY[34,0] := 4.3005E-11; GammaY[35,0] := 8.75577E-11; GammaY[36,0] := 1.7481E-10; GammaY[37,0] := 3.426E-10; GammaY[38,0] := 6.5975E-10; GammaY[39,0] := 1.24947E-09;
  GammaY[40,0] := 2.32911E-09; GammaY[41,0] := 4.27662E-09; GammaY[42,0] := 7.74048E-09; GammaY[43,0] := 1.38191E-08; GammaY[44,0] := 2.43503E-08; GammaY[45,0] := 4.23733E-08; GammaY[46,0] := 7.28582E-08; GammaY[47,0] := 1.23846E-07; GammaY[48,0] := 2.08214E-07; GammaY[49,0] := 3.46381E-07;
  GammaY[50,0] := 5.70425E-07; GammaY[51,0] := 9.3028E-07; GammaY[52,0] := 1.50301E-06; GammaY[53,0] := 2.40654E-06; GammaY[54,0] := 3.81993E-06; GammaY[55,0] := 6.01288E-06; GammaY[56,0] := 9.38869E-06; GammaY[57,0] := 1.4546E-05; GammaY[58,0] := 2.23674E-05; GammaY[59,0] := 3.41453E-05;
  GammaY[60,0] := 5.176E-05; GammaY[61,0] := 7.79301E-05; GammaY[62,0] := 0.000116563; GammaY[63,0] := 0.000173239; GammaY[64,0] := 0.000255889; GammaY[65,0] := 0.000375715; GammaY[66,0] := 0.000548462; GammaY[67,0] := 0.000796141; GammaY[68,0] := 0.001149374; GammaY[69,0] := 0.001650554;
  GammaY[70,0] := 0.002358097; GammaY[71,0] := 0.003352137; GammaY[72,0] := 0.004742114; GammaY[73,0] := 0.006676866; GammaY[74,0] := 0.009357994; GammaY[75,0] := 0.013057502; GammaY[76,0] := 0.018141037; GammaY[77,0] := 0.025098415; GammaY[78,0] := 0.034583673; GammaY[79,0] := 0.047467552;
  GammaY[80,0] := 0.064906306; GammaY[81,0] := 0.088431997; GammaY[82,0] := 0.120071343; GammaY[83,0] := 0.162502883; GammaY[84,0] := 0.219266279; GammaY[85,0] := 0.295043857; GammaY[86,0] := 0.39604442; GammaY[87,0] := 0.530535713; GammaY[88,0] := 0.709599675; GammaY[89,0] := 0.948233568;
  GammaY[90,0] := 1.267010288; GammaY[91,0] := 1.694685951; GammaY[92,0] := 2.272502531; GammaY[93,0] := 3.061731028; GammaY[94,0] := 4.157954154; GammaY[95,0] := 5.720883236; GammaY[96,0] := 8.046166969; GammaY[97,0] := 11.77952164; GammaY[98,0] := 18.87167807; GammaY[99,0] := 39.42310254;
end;
procedure InitGammaY2;
begin
  GammaY[0,1] := 4.70573E-46; GammaY[1,1] := 7.49549E-39; GammaY[2,1] := 8.16112E-35; GammaY[3,1] := 5.97773E-32; GammaY[4,1] := 1.00049E-29; GammaY[5,1] := 6.59855E-28; GammaY[6,1] := 2.29145E-26; GammaY[7,1] := 4.97694E-25; GammaY[8,1] := 7.55102E-24; GammaY[9,1] := 8.62815E-23;
  GammaY[10,1] := 7.83395E-22; GammaY[11,1] := 5.88021E-21; GammaY[12,1] := 3.76041E-20; GammaY[13,1] := 2.09748E-19; GammaY[14,1] := 1.03964E-18; GammaY[15,1] := 4.64874E-18; GammaY[16,1] := 1.89857E-17; GammaY[17,1] := 7.15508E-17; GammaY[18,1] := 2.50983E-16; GammaY[19,1] := 8.25444E-16;
  GammaY[20,1] := 2.5613E-15; GammaY[21,1] := 7.53881E-15; GammaY[22,1] := 2.11467E-14; GammaY[23,1] := 5.67613E-14; GammaY[24,1] := 1.46316E-13; GammaY[25,1] := 3.63357E-13; GammaY[26,1] := 8.71766E-13; GammaY[27,1] := 2.02572E-12; GammaY[28,1] := 4.56924E-12; GammaY[29,1] := 1.00247E-11;
  GammaY[30,1] := 2.14315E-11; GammaY[31,1] := 4.47197E-11; GammaY[32,1] := 9.12138E-11; GammaY[33,1] := 1.82108E-10; GammaY[34,1] := 3.56324E-10; GammaY[35,1] := 6.84069E-10; GammaY[36,1] := 1.28988E-09; GammaY[37,1] := 2.39117E-09; GammaY[38,1] := 4.36181E-09; GammaY[39,1] := 7.83566E-09;
  GammaY[40,1] := 1.38728E-08; GammaY[41,1] := 2.42238E-08; GammaY[42,1] := 4.17434E-08; GammaY[43,1] := 7.10344E-08; GammaY[44,1] := 1.19435E-07; GammaY[45,1] := 1.9852E-07; GammaY[46,1] := 3.26365E-07; GammaY[47,1] := 5.30922E-07; GammaY[48,1] := 8.5502E-07; GammaY[49,1] := 1.36369E-06;
  GammaY[50,1] := 2.15486E-06; GammaY[51,1] := 3.37475E-06; GammaY[52,1] := 5.24001E-06; GammaY[53,1] := 8.0692E-06; GammaY[54,1] := 1.23273E-05; GammaY[55,1] := 1.86885E-05; GammaY[56,1] := 2.81231E-05; GammaY[57,1] := 4.20192E-05; GammaY[58,1] := 6.23499E-05; GammaY[59,1] := 9.19026E-05;
  GammaY[60,1] := 0.000134592; GammaY[61,1] := 0.000195887; GammaY[62,1] := 0.000283379; GammaY[63,1] := 0.000407562; GammaY[64,1] := 0.000582855; GammaY[65,1] := 0.00082898; GammaY[66,1] := 0.001172784; GammaY[67,1] := 0.001650633; GammaY[68,1] := 0.00231158; GammaY[69,1] := 0.003221494;
  GammaY[70,1] := 0.004468454; GammaY[71,1] := 0.006169769; GammaY[72,1] := 0.008481062; GammaY[73,1] := 0.011608021; GammaY[74,1] := 0.015821535; GammaY[75,1] := 0.021477165; GammaY[76,1] := 0.029040142; GammaY[77,1] := 0.039117424; GammaY[78,1] := 0.052498806; GammaY[79,1] := 0.070209655;
  GammaY[80,1] := 0.09357871; GammaY[81,1] := 0.124325514; GammaY[82,1] := 0.16467374; GammaY[83,1] := 0.217499172; GammaY[84,1] := 0.286524781; GammaY[85,1] := 0.376581214; GammaY[86,1] := 0.493960246; GammaY[87,1] := 0.646904005; GammaY[88,1] := 0.846298719; GammaY[89,1] := 1.106687381;
  GammaY[90,1] := 1.447799743; GammaY[91,1] := 1.89696053; GammaY[92,1] := 2.493070783; GammaY[93,1] := 3.293606145; GammaY[94,1] := 4.387821482; GammaY[95,1] := 5.924447208; GammaY[96,1] := 8.178228183; GammaY[97,1] := 11.74814247; GammaY[98,1] := 18.44050195; GammaY[99,1] := 37.56242844;
end;
procedure InitGammaY3;
begin
  GammaY[0,2] := 2.76522E-42; GammaY[1,2] := 1.17797E-35; GammaY[2,2] := 5.93041E-32; GammaY[3,2] := 2.51499E-29; GammaY[4,2] := 2.75906E-27; GammaY[5,2] := 1.29026E-25; GammaY[6,2] := 3.35298E-24; GammaY[7,2] := 5.66693E-23; GammaY[8,2] := 6.89121E-22; GammaY[9,2] := 6.45899E-21;
  GammaY[10,2] := 4.90094E-20; GammaY[11,2] := 3.12185E-19; GammaY[12,2] := 1.71622E-18; GammaY[13,2] := 8.32002E-18; GammaY[14,2] := 3.61839E-17; GammaY[15,2] := 1.43143E-16; GammaY[16,2] := 5.20989E-16; GammaY[17,2] := 1.76114E-15; GammaY[18,2] := 5.57331E-15; GammaY[19,2] := 1.66229E-14;
  GammaY[20,2] := 4.69972E-14; GammaY[21,2] := 1.26579E-13; GammaY[22,2] := 3.26172E-13; GammaY[23,2] := 8.07149E-13; GammaY[24,2] := 1.9245E-12; GammaY[25,2] := 4.43409E-12; GammaY[26,2] := 9.89771E-12; GammaY[27,2] := 2.14541E-11; GammaY[28,2] := 4.52509E-11; GammaY[29,2] := 9.30442E-11;
  GammaY[30,2] := 1.8682E-10; GammaY[31,2] := 3.66848E-10; GammaY[32,2] := 7.05462E-10; GammaY[33,2] := 1.33024E-09; GammaY[34,2] := 2.46235E-09; GammaY[35,2] := 4.47908E-09; GammaY[36,2] := 8.01423E-09; GammaY[37,2] := 1.41173E-08; GammaY[38,2] := 2.45028E-08; GammaY[39,2] := 4.19351E-08;
  GammaY[40,2] := 7.08174E-08; GammaY[41,2] := 1.18082E-07; GammaY[42,2] := 1.94522E-07; GammaY[43,2] := 3.16766E-07; GammaY[44,2] := 5.10175E-07; GammaY[45,2] := 8.1306E-07; GammaY[46,2] := 1.28276E-06; GammaY[47,2] := 2.00435E-06; GammaY[48,2] := 3.10299E-06; GammaY[49,2] := 4.76134E-06;
  GammaY[50,2] := 7.24392E-06; GammaY[51,2] := 1.09309E-05; GammaY[52,2] := 1.63651E-05; GammaY[53,2] := 2.43155E-05; GammaY[54,2] := 3.58653E-05; GammaY[55,2] := 5.25299E-05; GammaY[56,2] := 7.64167E-05; GammaY[57,2] := 0.000110439; GammaY[58,2] := 0.000158602; GammaY[59,2] := 0.00022638;
  GammaY[60,2] := 0.000321218; GammaY[61,2] := 0.000453186; GammaY[62,2] := 0.000635844; GammaY[63,2] := 0.000887354; GammaY[64,2] := 0.001231938; GammaY[65,2] := 0.001701753; GammaY[66,2] := 0.002339303; GammaY[67,2] := 0.003200539; GammaY[68,2] := 0.004358813; GammaY[69,2] := 0.005909916;
  GammaY[70,2] := 0.007978483; GammaY[71,2] := 0.010726087; GammaY[72,2] := 0.014361482; GammaY[73,2] := 0.019153499; GammaY[74,2] := 0.025447291; GammaY[75,2] := 0.033684738; GammaY[76,2] := 0.044430103; GammaY[77,2] := 0.058402266; GammaY[78,2] := 0.076515304; GammaY[79,2] := 0.099929679;
  GammaY[80,2] := 0.130117072; GammaY[81,2] := 0.168942939; GammaY[82,2] := 0.218772424; GammaY[83,2] := 0.282607538; GammaY[84,2] := 0.364267012; GammaY[85,2] := 0.468625662; GammaY[86,2] := 0.601938748; GammaY[87,2] := 0.772291111; GammaY[88,2] := 0.990234995; GammaY[89,2] := 1.269723023;
  GammaY[90,2] := 1.629520913; GammaY[91,2] := 2.095435397; GammaY[92,2] := 2.704002669; GammaY[93,2] := 3.508977003; GammaY[94,2] := 4.593576121; GammaY[95,2] := 6.096160141; GammaY[96,2] := 8.271881794; GammaY[97,2] := 11.67618191; GammaY[98,2] := 17.98126345; GammaY[99,2] := 35.76307892;
end;
procedure InitGammaY4;
begin
  GammaY[0,3] := 7.8545E-39; GammaY[1,3] := 9.98378E-33; GammaY[2,3] := 2.47795E-29; GammaY[3,3] := 6.36991E-27; GammaY[4,3] := 4.74767E-25; GammaY[5,3] := 1.62106E-23; GammaY[6,3] := 3.23097E-22; GammaY[7,3] := 4.33998E-21; GammaY[8,3] := 4.30886E-20; GammaY[9,3] := 3.36759E-19;
  GammaY[10,3] := 2.16725E-18; GammaY[11,3] := 1.18744E-17; GammaY[12,3] := 5.68152E-17; GammaY[13,3] := 2.42148E-16; GammaY[14,3] := 9.33933E-16; GammaY[15,3] := 3.30151E-15; GammaY[16,3] := 1.08099E-14; GammaY[17,3] := 3.30688E-14; GammaY[18,3] := 9.5209E-14; GammaY[19,3] := 2.59591E-13;
  GammaY[20,3] := 6.73833E-13; GammaY[21,3] := 1.67281E-12; GammaY[22,3] := 3.98742E-12; GammaY[23,3] := 9.15778E-12; GammaY[24,3] := 2.03263E-11; GammaY[25,3] := 4.37178E-11; GammaY[26,3] := 9.13325E-11; GammaY[27,3] := 1.85728E-10; GammaY[28,3] := 3.68333E-10; GammaY[29,3] := 7.13598E-10;
  GammaY[30,3] := 1.35264E-09; GammaY[31,3] := 2.51208E-09; GammaY[32,3] := 4.5767E-09; GammaY[33,3] := 8.18917E-09; GammaY[34,3] := 1.44062E-08; GammaY[35,3] := 2.49403E-08; GammaY[36,3] := 4.25282E-08; GammaY[37,3] := 7.1487E-08; GammaY[38,3] := 1.18544E-07; GammaY[39,3] := 1.94057E-07;
  GammaY[40,3] := 3.13804E-07; GammaY[41,3] := 5.0156E-07; GammaY[42,3] := 7.92798E-07; GammaY[43,3] := 1.23994E-06; GammaY[44,3] := 1.91974E-06; GammaY[45,3] := 2.94364E-06; GammaY[46,3] := 4.47207E-06; GammaY[47,3] := 6.73416E-06; GammaY[48,3] := 1.00547E-05; GammaY[49,3] := 1.48907E-05;
  GammaY[50,3] := 2.18807E-05; GammaY[51,3] := 3.19112E-05; GammaY[52,3] := 4.62043E-05; GammaY[53,3] := 6.64353E-05; GammaY[54,3] := 9.48863E-05; GammaY[55,3] := 0.000134649; GammaY[56,3] := 0.000189887; GammaY[57,3] := 0.00026618; GammaY[58,3] := 0.000370967; GammaY[59,3] := 0.000514113;
  GammaY[60,3] := 0.000708642; GammaY[61,3] := 0.000971666; GammaY[62,3] := 0.001325573; GammaY[63,3] := 0.00179952; GammaY[64,3] := 0.002431327; GammaY[65,3] := 0.003269856; GammaY[66,3] := 0.004377985; GammaY[67,3] := 0.00583633; GammaY[68,3] := 0.007747883; GammaY[69,3] := 0.010243766;
  GammaY[70,3] := 0.013490374; GammaY[71,3] := 0.017698203; GammaY[72,3] := 0.023132764; GammaY[73,3] := 0.030128039; GammaY[74,3] := 0.039103095; GammaY[75,3] := 0.050582556; GammaY[76,3] := 0.065221897; GammaY[77,3] := 0.083838729; GammaY[78,3] := 0.107451618; GammaY[79,3] := 0.137328467;
  GammaY[80,3] := 0.175047166; GammaY[81,3] := 0.222572202; GammaY[82,3] := 0.282352363; GammaY[83,3] := 0.357446786; GammaY[84,3] := 0.451689915; GammaY[85,3] := 0.569910936; GammaY[86,3] := 0.718231447; GammaY[87,3] := 0.904478331; GammaY[88,3] := 1.138771375; GammaY[89,3] := 1.434384702;
  GammaY[90,3] := 1.809053631; GammaY[91,3] := 2.287038588; GammaY[92,3] := 2.902552786; GammaY[93,3] := 3.705749005; GammaY[94,3] := 4.774116538; GammaY[95,3] := 6.236284278; GammaY[96,3] := 8.329093266; GammaY[97,3] := 11.56745771; GammaY[98,3] := 17.49911776; GammaY[99,3] := 34.02618146;
end;
procedure InitGammaY5;
begin
  GammaY[0,4] := 1.14555E-35; GammaY[1,4] := 4.80347E-30; GammaY[2,4] := 6.23405E-27; GammaY[3,4] := 1.0133E-24; GammaY[4,4] := 5.30312E-23; GammaY[5,4] := 1.35803E-21; GammaY[6,4] := 2.12314E-20; GammaY[7,4] := 2.31065E-19; GammaY[8,4] := 1.9048E-18; GammaY[9,4] := 1.26009E-17;
  GammaY[10,4] := 6.97156E-17; GammaY[11,4] := 3.32621E-16; GammaY[12,4] := 1.40092E-15; GammaY[13,4] := 5.30461E-15; GammaY[14,4] := 1.83223E-14; GammaY[15,4] := 5.84117E-14; GammaY[16,4] := 1.73541E-13; GammaY[17,4] := 4.84363E-13; GammaY[18,4] := 1.27857E-12; GammaY[19,4] := 3.21025E-12;
  GammaY[20,4] := 7.70428E-12; GammaY[21,4] := 1.77471E-11; GammaY[22,4] := 3.93828E-11; GammaY[23,4] := 8.44598E-11; GammaY[24,4] := 1.75538E-10; GammaY[25,4] := 3.54438E-10; GammaY[26,4] := 6.96795E-10; GammaY[27,4] := 1.33633E-09; GammaY[28,4] := 2.50451E-09; GammaY[29,4] := 4.59422E-09;
  GammaY[30,4] := 8.26027E-09; GammaY[31,4] := 1.45756E-08; GammaY[32,4] := 2.52702E-08; GammaY[33,4] := 4.30927E-08; GammaY[34,4] := 7.23481E-08; GammaY[35,4] := 1.19691E-07; GammaY[36,4] := 1.95282E-07; GammaY[37,4] := 3.1445E-07; GammaY[38,4] := 5.00063E-07; GammaY[39,4] := 7.85885E-07;
  GammaY[40,4] := 1.22127E-06; GammaY[41,4] := 1.87765E-06; GammaY[42,4] := 2.85752E-06; GammaY[43,4] := 4.30669E-06; GammaY[44,4] := 6.4308E-06; GammaY[45,4] := 9.5177E-06; GammaY[46,4] := 1.39673E-05; GammaY[47,4] := 2.03311E-05; GammaY[48,4] := 2.93647E-05; GammaY[49,4] := 4.20961E-05;
  GammaY[50,4] := 5.99156E-05; GammaY[51,4] := 8.46919E-05; GammaY[52,4] := 0.000118922; GammaY[53,4] := 0.000165924; GammaY[54,4] := 0.000230084; GammaY[55,4] := 0.000317168; GammaY[56,4] := 0.00043472; GammaY[57,4] := 0.000592564; GammaY[58,4] := 0.000803431; GammaY[59,4] := 0.001083746;
  GammaY[60,4] := 0.001454613; GammaY[61,4] := 0.00194303; GammaY[62,4] := 0.002583399; GammaY[63,4] := 0.003419392; GammaY[64,4] := 0.004506234; GammaY[65,4] := 0.005913519; GammaY[66,4] := 0.007728645; GammaY[67,4] := 0.010061009; GammaY[68,4] := 0.013047114; GammaY[69,4] := 0.016856786;
  GammaY[70,4] := 0.021700707; GammaY[71,4] := 0.027839569; GammaY[72,4] := 0.035595151; GammaY[73,4] := 0.04536377; GammaY[74,4] := 0.057632583; GammaY[75,4] := 0.072999414; GammaY[76,4] := 0.092196899; GammaY[77,4] := 0.116122027; GammaY[78,4] := 0.145872431; GammaY[79,4] := 0.182791268;
  GammaY[80,4] := 0.228523147; GammaY[81,4] := 0.285084497; GammaY[82,4] := 0.354953093; GammaY[83,4] := 0.441183487; GammaY[84,4] := 0.547558125; GammaY[85,4] := 0.678788701; GammaY[86,4] := 0.840789813; GammaY[87,4] := 1.041059419; GammaY[88,4] := 1.289221464; GammaY[89,4] := 1.597822838;
  GammaY[90,4] := 1.983544095; GammaY[91,4] := 2.469120819; GammaY[92,4] := 3.086492451; GammaY[93,4] := 3.882395622; GammaY[94,4] := 4.928931381; GammaY[95,4] := 6.345589591; GammaY[96,4] := 8.35213953; GammaY[97,4] := 11.42580327; GammaY[98,4] := 16.99886403; GammaY[99,4] := 32.35248184;
end;
procedure InitGammaY6;
begin
  GammaY[0,5] := 9.06714E-33; GammaY[1,5] := 1.37514E-27; GammaY[2,5] := 9.85083E-25; GammaY[3,5] := 1.0526E-22; GammaY[4,5] := 3.98707E-21; GammaY[5,5] := 7.8478E-20; GammaY[6,5] := 9.82315E-19; GammaY[7,5] := 8.81505E-18; GammaY[8,5] := 6.127E-17; GammaY[9,5] := 3.47797E-16;
  GammaY[10,5] := 1.67477E-15; GammaY[11,5] := 7.03695E-15; GammaY[12,5] := 2.63612E-14; GammaY[13,5] := 8.95376E-14; GammaY[14,5] := 2.79461E-13; GammaY[15,5] := 8.10246E-13; GammaY[16,5] := 2.20168E-12; GammaY[17,5] := 5.64859E-12; GammaY[18,5] := 1.37678E-11; GammaY[19,5] := 3.20485E-11;
  GammaY[20,5] := 7.15678E-11; GammaY[21,5] := 1.53912E-10; GammaY[22,5] := 3.19839E-10; GammaY[23,5] := 6.44112E-10; GammaY[24,5] := 1.26031E-09; GammaY[25,5] := 2.40141E-09; GammaY[26,5] := 4.46478E-09; GammaY[27,5] := 8.11439E-09; GammaY[28,5] := 1.44388E-08; GammaY[29,5] := 2.51912E-08;
  GammaY[30,5] := 4.31493E-08; GammaY[31,5] := 7.26467E-08; GammaY[32,5] := 1.20348E-07; GammaY[33,5] := 1.96364E-07; GammaY[34,5] := 3.15843E-07; GammaY[35,5] := 5.01208E-07; GammaY[36,5] := 7.85283E-07; GammaY[37,5] := 1.2156E-06; GammaY[38,5] := 1.86032E-06; GammaY[39,5] := 2.81624E-06;
  GammaY[40,5] := 4.21958E-06; GammaY[41,5] := 6.26044E-06; GammaY[42,5] := 9.20192E-06; GammaY[43,5] := 1.34053E-05; GammaY[44,5] := 1.93632E-05; GammaY[45,5] := 2.77421E-05; GammaY[46,5] := 3.94385E-05; GammaY[47,5] := 5.56495E-05; GammaY[48,5] := 7.79644E-05; GammaY[49,5] := 0.000108481;
  GammaY[50,5] := 0.000149951; GammaY[51,5] := 0.000205967; GammaY[52,5] := 0.000281191; GammaY[53,5] := 0.000381647; GammaY[54,5] := 0.000515078; GammaY[55,5] := 0.000691391; GammaY[56,5] := 0.000923205; GammaY[57,5] := 0.001226525; GammaY[58,5] := 0.001621567; GammaY[59,5] := 0.00213376;
  GammaY[60,5] := 0.002794972; GammaY[61,5] := 0.003644992; GammaY[62,5] := 0.004733318; GammaY[63,5] := 0.006121321; GammaY[64,5] := 0.007884842; GammaY[65,5] := 0.010117306; GammaY[66,5] := 0.012933464; GammaY[67,5] := 0.016473855; GammaY[68,5] := 0.02091014; GammaY[69,5] := 0.026451475;
  GammaY[70,5] := 0.0333521; GammaY[71,5] := 0.041920412; GammaY[72,5] := 0.052529781; GammaY[73,5] := 0.065631498; GammaY[74,5] := 0.081770288; GammaY[75,5] := 0.101602969; GammaY[76,5] := 0.125920976; GammaY[77,5] := 0.155677717; GammaY[78,5] := 0.192021987; GammaY[79,5] := 0.236339146;
  GammaY[80,5] := 0.290302301; GammaY[81,5] := 0.355936657; GammaY[82,5] := 0.43570141; GammaY[83,5] := 0.532595473; GammaY[84,5] := 0.65029616; GammaY[85,5] := 0.793344364; GammaY[86,5] := 0.967396826; GammaY[87,5] := 1.179577575; GammaY[88,5] := 1.438980068; GammaY[89,5] := 1.757405637;
  GammaY[90,5] := 2.150486304; GammaY[91,5] := 2.639466955; GammaY[92,5] := 3.254128505; GammaY[93,5] := 4.037975022; GammaY[94,5] := 5.058032169; GammaY[95,5] := 6.425257377; GammaY[96,5] := 8.343515772; GammaY[97,5] := 11.25499918; GammaY[98,5] := 16.48493272; GammaY[99,5] := 30.74233697;
end;
procedure InitGammaY7;
begin
  GammaY[0,6] := 4.09768E-30; GammaY[1,6] := 2.44571E-25; GammaY[2,6] := 1.01637E-22; GammaY[3,6] := 7.39981E-21; GammaY[4,6] := 2.0858E-19; GammaY[5,6] := 3.22709E-18; GammaY[6,6] := 3.29492E-17; GammaY[7,6] := 2.47728E-16; GammaY[8,6] := 1.47225E-15; GammaY[9,6] := 7.26124E-15;
  GammaY[10,6] := 3.07781E-14; GammaY[11,6] := 1.15069E-13; GammaY[12,6] := 3.87069E-13; GammaY[13,6] := 1.18976E-12; GammaY[14,6] := 3.38331E-12; GammaY[15,6] := 8.99018E-12; GammaY[16,6] := 2.2506E-11; GammaY[17,6] := 5.34423E-11; GammaY[18,6] := 1.21062E-10; GammaY[19,6] := 2.62884E-10;
  GammaY[20,6] := 5.4948E-10; GammaY[21,6] := 1.10946E-09; GammaY[22,6] := 2.17063E-09; GammaY[23,6] := 4.12613E-09; GammaY[24,6] := 7.63848E-09; GammaY[25,6] := 1.38002E-08; GammaY[26,6] := 2.43768E-08; GammaY[27,6] := 4.21697E-08; GammaY[28,6] := 7.15476E-08; GammaY[29,6] := 1.19216E-07;
  GammaY[30,6] := 1.95316E-07; GammaY[31,6] := 3.14971E-07; GammaY[32,6] := 5.00451E-07; GammaY[33,6] := 7.84143E-07; GammaY[34,6] := 1.21262E-06; GammaY[35,6] := 1.85215E-06; GammaY[36,6] := 2.79604E-06; GammaY[37,6] := 4.17446E-06; GammaY[38,6] := 6.16734E-06; GammaY[39,6] := 9.02129E-06;
  GammaY[40,6] := 1.30716E-05; GammaY[41,6] := 1.87704E-05; GammaY[42,6] := 2.67234E-05; GammaY[43,6] := 3.7736E-05; GammaY[44,6] := 5.2872E-05; GammaY[45,6] := 7.35279E-05; GammaY[46,6] := 0.000101525; GammaY[47,6] := 0.000139227; GammaY[48,6] := 0.000189682; GammaY[49,6] := 0.000256801;
  GammaY[50,6] := 0.000345575; GammaY[51,6] := 0.000462345; GammaY[52,6] := 0.00061513; GammaY[53,6] := 0.00081402; GammaY[54,6] := 0.001071659; GammaY[55,6] := 0.001403828; GammaY[56,6] := 0.001830142; GammaY[57,6] := 0.002374887; GammaY[58,6] := 0.00306802; GammaY[59,6] := 0.003946368;
  GammaY[60,6] := 0.005055043; GammaY[61,6] := 0.006449128; GammaY[62,6] := 0.00819567; GammaY[63,6] := 0.010376033; GammaY[64,6] := 0.013088679; GammaY[65,6] := 0.016452432; GammaY[66,6] := 0.020610334; GammaY[67,6] := 0.02573416; GammaY[68,6] := 0.032029739; GammaY[69,6] := 0.039743208;
  GammaY[70,6] := 0.049168364; GammaY[71,6] := 0.060655334; GammaY[72,6] := 0.074620816; GammaY[73,6] := 0.0915602; GammaY[74,6] := 0.11206199; GammaY[75,6] := 0.136825023; GammaY[76,6] := 0.166679171; GammaY[77,6] := 0.20261037; GammaY[78,6] := 0.245791163; GammaY[79,6] := 0.297618282;
  GammaY[80,6] := 0.359759397; GammaY[81,6] := 0.434211958; GammaY[82,6] := 0.523378219; GammaY[83,6] := 0.630162311; GammaY[84,6] := 0.758097873; GammaY[85,6] := 0.911518857; GammaY[86,6] := 1.095792681; GammaY[87,6] := 1.317645568; GammaY[88,6] := 1.585627951; GammaY[89,6] := 1.910799451;
  GammaY[90,6] := 2.307777161; GammaY[91,6] := 2.796363492; GammaY[92,6] := 3.404292984; GammaY[93,6] := 4.172040243; GammaY[94,6] := 5.161873009; GammaY[95,6] := 6.47678965; GammaY[96,6] := 8.305854956; GammaY[97,6] := 11.05872183; GammaY[98,6] := 15.96138531; GammaY[99,6] := 29.19571274;
end;
procedure InitGammaY8;
begin
  GammaY[0,7] := 1.10777E-27; GammaY[1,7] := 2.81141E-23; GammaY[2,7] := 7.09536E-21; GammaY[3,7] := 3.63792E-19; GammaY[4,7] := 7.82719E-18; GammaY[5,7] := 9.71502E-17; GammaY[6,7] := 8.22959E-16; GammaY[7,7] := 5.25994E-15; GammaY[8,7] := 2.70719E-14; GammaY[9,7] := 1.17342E-13;
  GammaY[10,7] := 4.42356E-13; GammaY[11,7] := 1.48555E-12; GammaY[12,7] := 4.52638E-12; GammaY[13,7] := 1.26933E-11; GammaY[14,7] := 3.31365E-11; GammaY[15,7] := 8.12726E-11; GammaY[16,7] := 1.88698E-10; GammaY[17,7] := 4.17345E-10; GammaY[18,7] := 8.83919E-10; GammaY[19,7] := 1.80075E-09;
  GammaY[20,7] := 3.54216E-09; GammaY[21,7] := 6.7496E-09; GammaY[22,7] := 1.24944E-08; GammaY[23,7] := 2.25246E-08; GammaY[24,7] := 3.96318E-08; GammaY[25,7] := 6.81882E-08; GammaY[26,7] := 1.14919E-07; GammaY[27,7] := 1.89997E-07; GammaY[28,7] := 3.08577E-07; GammaY[29,7] := 4.92912E-07;
  GammaY[30,7] := 7.7525E-07; GammaY[31,7] := 1.20174E-06; GammaY[32,7] := 1.83765E-06; GammaY[33,7] := 2.77433E-06; GammaY[34,7] := 4.13828E-06; GammaY[35,7] := 6.10302E-06; GammaY[36,7] := 8.90439E-06; GammaY[37,7] := 1.28603E-05; GammaY[38,7] := 1.83955E-05; GammaY[39,7] := 2.60738E-05;
  GammaY[40,7] := 3.66373E-05; GammaY[41,7] := 5.10567E-05; GammaY[42,7] := 7.05932E-05; GammaY[43,7] := 9.6875E-05; GammaY[44,7] := 0.000131992; GammaY[45,7] := 0.00017861; GammaY[46,7] := 0.000240114; GammaY[47,7] := 0.000320778; GammaY[48,7] := 0.000425969; GammaY[49,7] := 0.0005624;
  GammaY[50,7] := 0.000738425; GammaY[51,7] := 0.000964396; GammaY[52,7] := 0.001253089; GammaY[53,7] := 0.001620205; GammaY[54,7] := 0.002084966; GammaY[55,7] := 0.002670819; GammaY[56,7] := 0.003406262; GammaY[57,7] := 0.004325814; GammaY[58,7] := 0.00547115; GammaY[59,7] := 0.006892431;
  GammaY[60,7] := 0.008649843; GammaY[61,7] := 0.010815405; GammaY[62,7] := 0.013475058; GammaY[63,7] := 0.016731091; GammaY[64,7] := 0.020704961; GammaY[65,7] := 0.025540558; GammaY[66,7] := 0.031407989; GammaY[67,7] := 0.038507975; GammaY[68,7] := 0.047076944; GammaY[69,7] := 0.057392963;
  GammaY[70,7] := 0.069782644; GammaY[71,7] := 0.084629216; GammaY[72,7] := 0.10238199; GammaY[73,7] := 0.123567508; GammaY[74,7] := 0.148802743; GammaY[75,7] := 0.17881082; GammaY[76,7] := 0.214439884; GammaY[77,7] := 0.256685912; GammaY[78,7] := 0.306720549; GammaY[79,7] := 0.365925422;
  GammaY[80,7] := 0.435934904; GammaY[81,7] := 0.51869005; GammaY[82,7] := 0.616507547; GammaY[83,7] := 0.732169133; GammaY[84,7] := 0.869039414; GammaY[85,7] := 1.031223838; GammaY[86,7] := 1.223784634; GammaY[87,7] := 1.453042471; GammaY[88,7] := 1.727008289; GammaY[89,7] := 2.05602511;
  GammaY[90,7] := 2.453716591; GammaY[91,7] := 2.938529738; GammaY[92,7] := 3.536280955; GammaY[93,7] := 4.284585955; GammaY[94,7] := 5.241276401; GammaY[95,7] := 6.501934253; GammaY[96,7] := 8.241860664; GammaY[97,7] := 10.84050136; GammaY[98,7] := 15.43191042; GammaY[99,7] := 27.71230189;
end;
procedure InitGammaY9;
begin
  GammaY[0,8] := 1.86966E-25; GammaY[1,8] := 2.16604E-21; GammaY[2,8] := 3.46304E-19; GammaY[3,8] := 1.2889E-17; GammaY[4,8] := 2.16644E-16; GammaY[5,8] := 2.19756E-15; GammaY[6,8] := 1.56847E-14; GammaY[7,8] := 8.6359E-14; GammaY[8,8] := 3.89441E-13; GammaY[9,8] := 1.49904E-12;
  GammaY[10,8] := 5.07385E-12; GammaY[11,8] := 1.54389E-11; GammaY[12,8] := 4.29531E-11; GammaY[13,8] := 1.10714E-10; GammaY[14,8] := 2.67178E-10; GammaY[15,8] := 6.08807E-10; GammaY[16,8] := 1.31904E-09; GammaY[17,8] := 2.73305E-09; GammaY[18,8] := 5.44187E-09; GammaY[19,8] := 1.04554E-08;
  GammaY[20,8] := 1.9451E-08; GammaY[21,8] := 3.5145E-08; GammaY[22,8] := 6.18354E-08; GammaY[23,8] := 1.06182E-07; GammaY[24,8] := 1.7831E-07; GammaY[25,8] := 2.93342E-07; GammaY[26,8] := 4.73506E-07; GammaY[27,8] := 7.50986E-07; GammaY[28,8] := 1.17175E-06; GammaY[29,8] := 1.80061E-06;
  GammaY[30,8] := 2.72786E-06; GammaY[31,8] := 4.07793E-06; GammaY[32,8] := 6.02046E-06; GammaY[33,8] := 8.78453E-06; GammaY[34,8] := 1.26767E-05; GammaY[35,8] := 1.81034E-05; GammaY[36,8] := 2.55998E-05; GammaY[37,8] := 3.58642E-05; GammaY[38,8] := 4.98021E-05; GammaY[39,8] := 6.85792E-05;
  GammaY[40,8] := 9.36862E-05; GammaY[41,8] := 0.000127018; GammaY[42,8] := 0.000170969; GammaY[43,8] := 0.000228549; GammaY[44,8] := 0.000303518; GammaY[45,8] := 0.000400552; GammaY[46,8] := 0.000525441; GammaY[47,8] := 0.000685311; GammaY[48,8] := 0.000888905; GammaY[49,8] := 0.001146899;
  GammaY[50,8] := 0.001472275; GammaY[51,8] := 0.00188076; GammaY[52,8] := 0.002391335; GammaY[53,8] := 0.003026828; GammaY[54,8] := 0.003814597; GammaY[55,8] := 0.004787325; GammaY[56,8] := 0.005983931; GammaY[57,8] := 0.007450626; GammaY[58,8] := 0.009242119; GammaY[59,8] := 0.011423005;
  GammaY[60,8] := 0.014069361; GammaY[61,8] := 0.017270561; GammaY[62,8] := 0.021131368; GammaY[63,8] := 0.025774321; GammaY[64,8] := 0.031342464; GammaY[65,8] := 0.03800248; GammaY[66,8] := 0.045948284; GammaY[67,8] := 0.05540514; GammaY[68,8] := 0.066634414; GammaY[69,8] := 0.079939049;
  GammaY[70,8] := 0.095669918; GammaY[71,8] := 0.114233201; GammaY[72,8] := 0.136099023; GammaY[73,8] := 0.161811587; GammaY[74,8] := 0.192001184; GammaY[75,8] := 0.227398481; GammaY[76,8] := 0.268851688; GammaY[77,8] := 0.317347353; GammaY[78,8] := 0.374035784; GammaY[79,8] := 0.440262468;
  GammaY[80,8] := 0.51760732; GammaY[81,8] := 0.607934313; GammaY[82,8] := 0.713455038; GammaY[83,8] := 0.836811383; GammaY[84,8] := 0.981184566; GammaY[85,8] := 1.1504415; GammaY[86,8] := 1.349335154; GammaY[87,8] := 1.583784525; GammaY[88,8] := 1.861280769; GammaY[89,8] := 2.191461719;
  GammaY[90,8] := 2.587020685; GammaY[91,8] := 3.065135495; GammaY[92,8] := 3.649805146; GammaY[93,8] := 4.375979852; GammaY[94,8] := 5.297354319; GammaY[95,8] := 6.502607614; GammaY[96,8] := 8.154249579; GammaY[97,8] := 10.60369384; GammaY[98,8] := 14.89983794; GammaY[99,8] := 26.29151032;
end;
procedure InitGammaY10;
begin
  GammaY[0,9] := 2.04882E-23; GammaY[1,9] := 1.15639E-19; GammaY[2,9] := 1.21772E-17; GammaY[3,9] := 3.3829E-16; GammaY[4,9] := 4.53697E-15; GammaY[5,9] := 3.82498E-14; GammaY[6,9] := 2.33271E-13; GammaY[7,9] := 1.11988E-12; GammaY[8,9] := 4.47231E-12; GammaY[9,9] := 1.54348E-11;
  GammaY[10,9] := 4.73151E-11; GammaY[11,9] := 1.31493E-10; GammaY[12,9] := 3.36504E-10; GammaY[13,9] := 8.02693E-10; GammaY[14,9] := 1.80214E-09; GammaY[15,9] := 3.83805E-09; GammaY[16,9] := 7.80365E-09; GammaY[17,9] := 1.52286E-08; GammaY[18,9] := 2.86507E-08; GammaY[19,9] := 5.21628E-08;
  GammaY[20,9] := 9.2201E-08; GammaY[21,9] := 1.58658E-07; GammaY[22,9] := 2.66431E-07; GammaY[23,9] := 4.37532E-07; GammaY[24,9] := 7.03944E-07; GammaY[25,9] := 1.1114E-06; GammaY[26,9] := 1.72438E-06; GammaY[27,9] := 2.63255E-06; GammaY[28,9] := 3.95914E-06; GammaY[29,9] := 5.87151E-06;
  GammaY[30,9] := 8.59462E-06; GammaY[31,9] := 1.24278E-05; GammaY[32,9] := 1.77656E-05; GammaY[33,9] := 2.51238E-05; GammaY[34,9] := 3.51708E-05; GammaY[35,9] := 4.87668E-05; GammaY[36,9] := 6.70097E-05; GammaY[37,9] := 9.12927E-05; GammaY[38,9] := 0.000123371; GammaY[39,9] := 0.000165442;
  GammaY[40,9] := 0.000220245; GammaY[41,9] := 0.000291169; GammaY[42,9] := 0.000382389; GammaY[43,9] := 0.000499025; GammaY[44,9] := 0.000647321; GammaY[45,9] := 0.000834861; GammaY[46,9] := 0.001070813; GammaY[47,9] := 0.00136622; GammaY[48,9] := 0.001734328; GammaY[49,9] := 0.002190962;
  GammaY[50,9] := 0.00275497; GammaY[51,9] := 0.003448712; GammaY[52,9] := 0.004298636; GammaY[53,9] := 0.005335925; GammaY[54,9] := 0.006597232; GammaY[55,9] := 0.008125522; GammaY[56,9] := 0.009971022; GammaY[57,9] := 0.012192291; GammaY[58,9] := 0.014857448; GammaY[59,9] := 0.01804554;
  GammaY[60,9] := 0.021848107; GammaY[61,9] := 0.026370941; GammaY[62,9] := 0.031736079; GammaY[63,9] := 0.03808406; GammaY[64,9] := 0.045576488; GammaY[65,9] := 0.054398937; GammaY[66,9] := 0.064764262; GammaY[67,9] := 0.076916383; GammaY[68,9] := 0.091134613; GammaY[69,9] := 0.107738647;
  GammaY[70,9] := 0.127094321; GammaY[71,9] := 0.1496203; GammaY[72,9] := 0.175795911; GammaY[73,9] := 0.206170331; GammaY[74,9] := 0.241373493; GammaY[75,9] := 0.282129091; GammaY[76,9] := 0.329270242; GammaY[77,9] := 0.383758498; GammaY[78,9] := 0.446707166; GammaY[79,9] := 0.519410186;
  GammaY[80,9] := 0.603378287; GammaY[81,9] := 0.700384847; GammaY[82,9] := 0.812524703; GammaY[83,9] := 0.942290684; GammaY[84,9] := 1.092674764; GammaY[85,9] := 1.26730394; GammaY[86,9] := 1.470626232; GammaY[87,9] := 1.708175658; GammaY[88,9] := 1.986929447; GammaY[89,9] := 2.31586672;
  GammaY[90,9] := 2.706811853; GammaY[91,9] := 3.175736734; GammaY[92,9] := 3.744945687; GammaY[93,9] := 4.446900239; GammaY[94,9] := 5.331442574; GammaY[95,9] := 6.480833134; GammaY[96,9] := 8.045699654; GammaY[97,9] := 10.35145373; GammaY[98,9] := 14.36814516; GammaY[99,9] := 24.93250123;
end;
procedure InitGammaY11;
begin
  GammaY[0,10] := 1.51112E-21; GammaY[1,10] := 4.4108E-18; GammaY[2,10] := 3.17125E-16; GammaY[3,10] := 6.74564E-15; GammaY[4,10] := 7.35834E-14; GammaY[5,10] := 5.23544E-13; GammaY[6,10] := 2.7633E-12; GammaY[7,10] := 1.16954E-11; GammaY[8,10] := 4.1768E-11; GammaY[9,10] := 1.30382E-10;
  GammaY[10,10] := 3.64888E-10; GammaY[11,10] := 9.32967E-10; GammaY[12,10] := 2.21105E-09; GammaY[13,10] := 4.91178E-09; GammaY[14,10] := 1.03198E-08; GammaY[15,10] := 2.06549E-08; GammaY[16,10] := 3.96159E-08; GammaY[17,10] := 7.31701E-08; GammaY[18,10] := 1.30677E-07; GammaY[19,10] := 2.2645E-07;
  GammaY[20,10] := 3.81893E-07; GammaY[21,10] := 6.28368E-07; GammaY[22,10] := 1.01099E-06; GammaY[23,10] := 1.5936E-06; GammaY[24,10] := 2.46513E-06; GammaY[25,10] := 3.74782E-06; GammaY[26,10] := 5.60746E-06; GammaY[27,10] := 8.26635E-06; GammaY[28,10] := 1.20192E-05; GammaY[29,10] := 1.7253E-05;
  GammaY[30,10] := 2.44707E-05; GammaY[31,10] := 3.43207E-05; GammaY[32,10] := 4.76315E-05; GammaY[33,10] := 6.54543E-05; GammaY[34,10] := 8.91125E-05; GammaY[35,10] := 0.000120261; GammaY[36,10] := 0.000160956; GammaY[37,10] := 0.000213738; GammaY[38,10] := 0.000281725; GammaY[39,10] := 0.000368726;
  GammaY[40,10] := 0.000479368; GammaY[41,10] := 0.000619247; GammaY[42,10] := 0.000795095; GammaY[43,10] := 0.001014982; GammaY[44,10] := 0.001288533; GammaY[45,10] := 0.001627187; GammaY[46,10] := 0.002044488; GammaY[47,10] := 0.002556409; GammaY[48,10] := 0.003181729; GammaY[49,10] := 0.003942447;
  GammaY[50,10] := 0.004864259; GammaY[51,10] := 0.005977084; GammaY[52,10] := 0.007315663; GammaY[53,10] := 0.008920226; GammaY[54,10] := 0.01083724; GammaY[55,10] := 0.013120243; GammaY[56,10] := 0.015830786; GammaY[57,10] := 0.019039471; GammaY[58,10] := 0.022827127; GammaY[59,10] := 0.027286115;
  GammaY[60,10] := 0.032521794; GammaY[61,10] := 0.038654163; GammaY[62,10] := 0.0458197; GammaY[63,10] := 0.05417344; GammaY[64,10] := 0.063891308; GammaY[65,10] := 0.075172766; GammaY[66,10] := 0.08824381; GammaY[67,10] := 0.103360391; GammaY[68,10] := 0.120812328; GammaY[69,10] := 0.140927808;
  GammaY[70,10] := 0.164078591; GammaY[71,10] := 0.190686071; GammaY[72,10] := 0.221228362; GammaY[73,10] := 0.256248663; GammaY[74,10] := 0.29636519; GammaY[75,10] := 0.34228306; GammaY[76,10] := 0.39480865; GammaY[77,10] := 0.45486706; GammaY[78,10] := 0.523523595; GammaY[79,10] := 0.602010409;
  GammaY[80,10] := 0.691759988; GammaY[81,10] := 0.794447588; GammaY[82,10] := 0.912045757; GammaY[83,10] := 1.04689541; GammaY[84,10] := 1.201799737; GammaY[85,10] := 1.380150432; GammaY[86,10] := 1.58610526; GammaY[87,10] := 1.824814906; GammaY[88,10] := 2.102779119; GammaY[89,10] := 2.428365737;
  GammaY[90,10] := 2.812568414; GammaY[91,10] := 3.270234349; GammaY[92,10] := 3.822086544; GammaY[93,10] := 4.498269352; GammaY[94,10] := 5.345039058; GammaY[95,10] := 6.438688922; GammaY[96,10] := 7.918816859; GammaY[97,10] := 10.08672452; GammaY[98,10] := 13.8394768; GammaY[99,10] := 23.63422651;
end;
procedure InitGammaY12;
begin
  GammaY[0,11] := 7.75352E-20; GammaY[1,11] := 1.23623E-16; GammaY[2,11] := 6.27337E-15; GammaY[3,11] := 1.0458E-13; GammaY[4,11] := 9.44153E-13; GammaY[5,11] := 5.74849E-12; GammaY[6,11] := 2.65663E-11; GammaY[7,11] := 1.00133E-10; GammaY[8,11] := 3.22674E-10; GammaY[9,11] := 9.18423E-10;
  GammaY[10,11] := 2.36379E-09; GammaY[11,11] := 5.59811E-09; GammaY[12,11] := 1.23629E-08; GammaY[13,11] := 2.57247E-08; GammaY[14,11] := 5.08529E-08; GammaY[15,11] := 9.61395E-08; GammaY[16,11] := 1.74773E-07; GammaY[17,11] := 3.06898E-07; GammaY[18,11] := 5.22518E-07; GammaY[19,11] := 8.6533E-07;
  GammaY[20,11] := 1.39772E-06; GammaY[21,11] := 2.20718E-06; GammaY[22,11] := 3.41437E-06; GammaY[23,11] := 5.18336E-06; GammaY[24,11] := 7.73413E-06; GammaY[25,11] := 1.13581E-05; GammaY[26,11] := 1.64369E-05; GammaY[27,11] := 2.34652E-05; GammaY[28,11] := 3.30777E-05; GammaY[29,11] := 4.6082E-05;
  GammaY[30,11] := 6.34964E-05; GammaY[31,11] := 8.65956E-05; GammaY[32,11] := 0.000116963; GammaY[33,11] := 0.000156553; GammaY[34,11] := 0.000207761; GammaY[35,11] := 0.000273507; GammaY[36,11] := 0.000357328; GammaY[37,11] := 0.000463487; GammaY[38,11] := 0.000597099; GammaY[39,11] := 0.000764263;
  GammaY[40,11] := 0.000972228; GammaY[41,11] := 0.001229569; GammaY[42,11] := 0.001546386; GammaY[43,11] := 0.001934532; GammaY[44,11] := 0.002407865; GammaY[45,11] := 0.002982531; GammaY[46,11] := 0.003677278; GammaY[47,11] := 0.004513808; GammaY[48,11] := 0.005517168; GammaY[49,11] := 0.006716178;
  GammaY[50,11] := 0.008143916; GammaY[51,11] := 0.009838246; GammaY[52,11] := 0.011842408; GammaY[53,11] := 0.014205665; GammaY[54,11] := 0.016984028; GammaY[55,11] := 0.020241043; GammaY[56,11] := 0.024048679; GammaY[57,11] := 0.028488301; GammaY[58,11] := 0.033651749; GammaY[59,11] := 0.03964254;
  GammaY[60,11] := 0.046577203; GammaY[61,11] := 0.05458677; GammaY[62,11] := 0.06381844; GammaY[63,11] := 0.074437448; GammaY[64,11] := 0.086629165; GammaY[65,11] := 0.10060148; GammaY[66,11] := 0.116587491; GammaY[67,11] := 0.134848584; GammaY[68,11] := 0.155677961; GammaY[69,11] := 0.179404704;
  GammaY[70,11] := 0.206398489; GammaY[71,11] := 0.237075082; GammaY[72,11] := 0.271902805; GammaY[73,11] := 0.311410171; GammaY[74,11] := 0.356194982; GammaY[75,11] := 0.406935256; GammaY[76,11] := 0.464402442; GammaY[77,11] := 0.529477537; GammaY[78,11] := 0.603170974; GammaY[79,11] := 0.686647308;
  GammaY[80,11] := 0.781256215; GammaY[81,11] := 0.888571922; GammaY[82,11] := 1.01044386; GammaY[83,11] := 1.149062681; GammaY[84,11] := 1.307047543; GammaY[85,11] := 1.487567738; GammaY[86,11] := 1.694489793; GammaY[87,11] := 1.932611842; GammaY[88,11] := 2.207993217; GammaY[89,11] := 2.528411791;
  GammaY[90,11] := 2.904089418; GammaY[91,11] := 3.348824285; GammaY[92,11] := 3.881864726; GammaY[93,11] := 4.53119815; GammaY[94,11] := 5.339747587; GammaY[95,11] := 6.378263457; GammaY[96,11] := 7.776105165; GammaY[97,11] := 9.812227301; GammaY[98,11] := 13.31615757; GammaY[99,11] := 22.39545786;
end;
procedure InitGammaY13;
begin
  GammaY[0,12] := 2.85283E-18; GammaY[1,12] := 2.6125E-15; GammaY[2,12] := 9.6482E-14; GammaY[3,12] := 1.28749E-12; GammaY[4,12] := 9.77259E-12; GammaY[5,12] := 5.15628E-11; GammaY[6,12] := 2.10879E-10; GammaY[7,12] := 7.1443E-10; GammaY[8,12] := 2.09445E-09; GammaY[9,12] := 5.47608E-09;
  GammaY[10,12] := 1.30494E-08; GammaY[11,12] := 2.88028E-08; GammaY[12,12] := 5.96131E-08; GammaY[13,12] := 1.16808E-07; GammaY[14,12] := 2.18335E-07; GammaY[15,12] := 3.91708E-07; GammaY[16,12] := 6.779E-07; GammaY[17,12] := 1.13641E-06; GammaY[18,12] := 1.85176E-06; GammaY[19,12] := 2.94164E-06;
  GammaY[20,12] := 4.56707E-06; GammaY[21,12] := 6.94488E-06; GammaY[22,12] := 1.03629E-05; GammaY[23,12] := 1.51983E-05; GammaY[24,12] := 2.19394E-05; GammaY[25,12] := 3.12116E-05; GammaY[26,12] := 4.38078E-05; GammaY[27,12] := 6.07243E-05; GammaY[28,12] := 8.32018E-05; GammaY[29,12] := 0.000112774;
  GammaY[30,12] := 0.000151322; GammaY[31,12] := 0.000201136; GammaY[32,12] := 0.000264992; GammaY[33,12] := 0.000346227; GammaY[34,12] := 0.000448833; GammaY[35,12] := 0.000577563; GammaY[36,12] := 0.000738044; GammaY[37,12] := 0.000936909; GammaY[38,12] := 0.001181938; GammaY[39,12] := 0.001482226;
  GammaY[40,12] := 0.001848354; GammaY[41,12] := 0.002292591; GammaY[42,12] := 0.002829112; GammaY[43,12] := 0.003474239; GammaY[44,12] := 0.004246705; GammaY[45,12] := 0.005167943; GammaY[46,12] := 0.006262412; GammaY[47,12] := 0.007557943; GammaY[48,12] := 0.009086122; GammaY[49,12] := 0.010882714;
  GammaY[50,12] := 0.012988122; GammaY[51,12] := 0.015447888; GammaY[52,12] := 0.01831325; GammaY[53,12] := 0.021641743; GammaY[54,12] := 0.02549786; GammaY[55,12] := 0.029953786; GammaY[56,12] := 0.03509019; GammaY[57,12] := 0.04099711; GammaY[58,12] := 0.047774928; GammaY[59,12] := 0.055535441;
  GammaY[60,12] := 0.064403062; GammaY[61,12] := 0.074516148; GammaY[62,12] := 0.086028487; GammaY[63,12] := 0.099110973; GammaY[64,12] := 0.113953484; GammaY[65,12] := 0.130767016; GammaY[66,12] := 0.14978611; GammaY[67,12] := 0.171271624; GammaY[68,12] := 0.195513925; GammaY[69,12] := 0.222836583;
  GammaY[70,12] := 0.253600656; GammaY[71,12] := 0.288209725; GammaY[72,12] := 0.327115807; GammaY[73,12] := 0.370826382; GammaY[74,12] := 0.41991277; GammaY[75,12] := 0.475020219; GammaY[76,12] := 0.536880143; GammaY[77,12] := 0.606325066; GammaY[78,12] := 0.684307028; GammaY[79,12] := 0.771920536;
  GammaY[80,12] := 0.87043138; GammaY[81,12] := 0.981313243; GammaY[82,12] := 1.106299037; GammaY[83,12] := 1.247429637; GammaY[84,12] := 1.407143036; GammaY[85,12] := 1.588392893; GammaY[86,12] := 1.794784861; GammaY[87,12] := 2.030787803; GammaY[88,12] := 2.302033077; GammaY[89,12] := 2.615754119;
  GammaY[90,12] := 2.981451498; GammaY[91,12] := 3.411945383; GammaY[92,12] := 3.925113196; GammaY[93,12] := 4.546934272; GammaY[94,12] := 5.317233775; GammaY[95,12] := 6.301615117; GammaY[96,12] := 7.619940688; GammaY[97,12] := 9.530457786; GammaY[98,12] := 12.80021455; GammaY[99,12] := 21.21480309;
end;
procedure InitGammaY14;
begin
  GammaY[0,13] := 7.73995E-17; GammaY[1,13] := 4.26265E-14; GammaY[2,13] := 1.17845E-12; GammaY[3,13] := 1.28317E-11; GammaY[4,13] := 8.30643E-11; GammaY[5,13] := 3.84186E-10; GammaY[6,13] := 1.40403E-09; GammaY[7,13] := 4.31192E-09; GammaY[8,13] := 1.15874E-08; GammaY[9,13] := 2.80197E-08;
  GammaY[10,13] := 6.22058E-08; GammaY[11,13] := 1.28694E-07; GammaY[12,13] := 2.50946E-07; GammaY[13,13] := 4.65297E-07; GammaY[14,13] := 8.26138E-07; GammaY[15,13] := 1.41254E-06; GammaY[16,13] := 2.33659E-06; GammaY[17,13] := 3.75366E-06; GammaY[18,13] := 5.87503E-06; GammaY[19,13] := 8.983E-06;
  GammaY[20,13] := 1.3449E-05; GammaY[21,13] := 1.9755E-05; GammaY[22,13] := 2.85185E-05; GammaY[23,13] := 4.05219E-05; GammaY[24,13] := 5.67457E-05; GammaY[25,13] := 7.84077E-05; GammaY[26,13] := 0.000107007; GammaY[27,13] := 0.000144373; GammaY[28,13] := 0.000192723; GammaY[29,13] := 0.000254728;
  GammaY[30,13] := 0.000333577; GammaY[31,13] := 0.000433064; GammaY[32,13] := 0.000557673; GammaY[33,13] := 0.000712674; GammaY[34,13] := 0.000904233; GammaY[35,13] := 0.001139532; GammaY[36,13] := 0.001426897; GammaY[37,13] := 0.001775944; GammaY[38,13] := 0.002197732; GammaY[39,13] := 0.002704936;
  GammaY[40,13] := 0.003312035; GammaY[41,13] := 0.004035513; GammaY[42,13] := 0.004894078; GammaY[43,13] := 0.005908903; GammaY[44,13] := 0.007103884; GammaY[45,13] := 0.008505925; GammaY[46,13] := 0.010145238; GammaY[47,13] := 0.012055675; GammaY[48,13] := 0.014275089; GammaY[49,13] := 0.016845716;
  GammaY[50,13] := 0.019814602; GammaY[51,13] := 0.023234059; GammaY[52,13] := 0.027162161; GammaY[53,13] := 0.031663286; GammaY[54,13] := 0.036808712; GammaY[55,13] := 0.04267726; GammaY[56,13] := 0.049356006; GammaY[57,13] := 0.056941064; GammaY[58,13] := 0.065538447; GammaY[59,13] := 0.075265023;
  GammaY[60,13] := 0.086249576; GammaY[61,13] := 0.098633993; GammaY[62,13] := 0.112574589; GammaY[63,13] := 0.128243607; GammaY[64,13] := 0.145830908; GammaY[65,13] := 0.16554589; GammaY[66,13] := 0.187619689; GammaY[67,13] := 0.212307694; GammaY[68,13] := 0.239892457; GammaY[69,13] := 0.270687072;
  GammaY[70,13] := 0.30503911; GammaY[71,13] := 0.343335241; GammaY[72,13] := 0.386006702; GammaY[73,13] := 0.433535769; GammaY[74,13] := 0.486463545; GammaY[75,13] := 0.545399297; GammaY[76,13] := 0.611031804; GammaY[77,13] := 0.684143248; GammaY[78,13] := 0.765626343; GammaY[79,13] := 0.856505657;
  GammaY[80,13] := 0.957968586; GammaY[81,13] := 1.071386636; GammaY[82,13] := 1.198368369; GammaY[83,13] := 1.340824247; GammaY[84,13] := 1.501029071; GammaY[85,13] := 1.681728735; GammaY[86,13] := 1.886277843; GammaY[87,13] := 2.118833741; GammaY[88,13] := 2.384633865; GammaY[89,13] := 2.690401683;
  GammaY[90,13] := 3.044966126; GammaY[91,13] := 3.460236397; GammaY[92,13] := 3.952814226; GammaY[93,13] := 4.546814675; GammaY[94,13] := 5.27918336; GammaY[95,13] := 6.210746682; GammaY[96,13] := 7.452560271; GammaY[97,13] := 9.243689501; GammaY[98,13] := 12.29339515; GammaY[99,13] := 20.09077933;
end;
procedure InitGammaY15;
begin
  GammaY[0,14] := 1.58858E-15; GammaY[1,14] := 5.4882E-13; GammaY[2,14] := 1.16561E-11; GammaY[3,14] := 1.05374E-10; GammaY[4,14] := 5.89292E-10; GammaY[5,14] := 2.41435E-09; GammaY[6,14] := 7.9551E-09; GammaY[7,14] := 2.23198E-08; GammaY[8,14] := 5.5364E-08; GammaY[9,14] := 1.24596E-07;
  GammaY[10,14] := 2.59177E-07; GammaY[11,14] := 5.05225E-07; GammaY[12,14] := 9.32652E-07; GammaY[13,14] := 1.64377E-06; GammaY[14,14] := 2.78391E-06; GammaY[15,14] := 4.55429E-06; GammaY[16,14] := 7.22746E-06; GammaY[17,14] := 1.11655E-05; GammaY[18,14] := 1.68413E-05; GammaY[19,14] := 2.48632E-05;
  GammaY[20,14] := 3.60035E-05; GammaY[21,14] := 5.12307E-05; GammaY[22,14] := 7.17459E-05; GammaY[23,14] := 9.90242E-05; GammaY[24,14] := 0.000134861; GammaY[25,14] := 0.000181423; GammaY[26,14] := 0.000241306; GammaY[27,14] := 0.000317597; GammaY[28,14] := 0.000413943; GammaY[29,14] := 0.000534631;
  GammaY[30,14] := 0.000684664; GammaY[31,14] := 0.000869859; GammaY[32,14] := 0.001096942; GammaY[33,14] := 0.001373653; GammaY[34,14] := 0.001708868; GammaY[35,14] := 0.002112716; GammaY[36,14] := 0.002596722; GammaY[37,14] := 0.003173945; GammaY[38,14] := 0.00385914; GammaY[39,14] := 0.004668924;
  GammaY[40,14] := 0.005621955; GammaY[41,14] := 0.006739126; GammaY[42,14] := 0.008043776; GammaY[43,14] := 0.009561908; GammaY[44,14] := 0.01132243; GammaY[45,14] := 0.013357415; GammaY[46,14] := 0.015702371; GammaY[47,14] := 0.018396542; GammaY[48,14] := 0.021483224; GammaY[49,14] := 0.025010113;
  GammaY[50,14] := 0.029029674; GammaY[51,14] := 0.033599546; GammaY[52,14] := 0.038782977; GammaY[53,14] := 0.044649303; GammaY[54,14] := 0.051274465; GammaY[55,14] := 0.05874158; GammaY[56,14] := 0.067141566; GammaY[57,14] := 0.076573829; GammaY[58,14] := 0.087147029; GammaY[59,14] := 0.098979928;
  GammaY[60,14] := 0.112202333; GammaY[61,14] := 0.126956162; GammaY[62,14] := 0.143396636; GammaY[63,14] := 0.161693625; GammaY[64,14] := 0.182033185; GammaY[65,14] := 0.204619305; GammaY[66,14] := 0.229675912; GammaY[67,14] := 0.257449182; GammaY[68,14] := 0.288210208; GammaY[69,14] := 0.322258116;
  GammaY[70,14] := 0.359923701; GammaY[71,14] := 0.401573698; GammaY[72,14] := 0.447615861; GammaY[73,14] := 0.498504977; GammaY[74,14] := 0.55475007; GammaY[75,14] := 0.616923108; GammaY[76,14] := 0.685669559; GammaY[77,14] := 0.761721313; GammaY[78,14] := 0.845912618; GammaY[79,14] := 0.939199937;
  GammaY[80,14] := 1.042690408; GammaY[81,14] := 1.157662854; GammaY[82,14] := 1.285615672; GammaY[83,14] := 1.428327425; GammaY[84,14] := 1.587915656; GammaY[85,14] := 1.76693495; GammaY[86,14] := 1.968505705; GammaY[87,14] := 2.196494402; GammaY[88,14] := 2.455769956; GammaY[89,14] := 2.752581185;
  GammaY[90,14] := 3.095132677; GammaY[91,14] := 3.494486759; GammaY[92,14] := 3.966057931; GammaY[93,14] := 4.532228648; GammaY[94,14] := 5.227272075; GammaY[95,14] := 6.10758163; GammaY[96,14] := 7.276048479; GammaY[97,14] := 8.953974415; GammaY[98,14] := 11.79718692; GammaY[99,14] := 19.0217858;
end;
procedure InitGammaY16;
begin
  GammaY[0,15] := 2.52535E-14; GammaY[1,15] := 5.68861E-12; GammaY[2,15] := 9.50408E-11; GammaY[3,15] := 7.24573E-10; GammaY[4,15] := 3.54185E-09; GammaY[5,15] := 1.29776E-08; GammaY[6,15] := 3.88687E-08; GammaY[7,15] := 1.00348E-07; GammaY[8,15] := 2.31231E-07; GammaY[9,15] := 4.87111E-07;
  GammaY[10,15] := 9.54389E-07; GammaY[11,15] := 1.76143E-06; GammaY[12,15] := 3.09205E-06; GammaY[13,15] := 5.20158E-06; GammaY[14,15] := 8.43559E-06; GammaY[15,15] := 1.32516E-05; GammaY[16,15] := 2.02439E-05; GammaY[17,15] := 3.01716E-05; GammaY[18,15] := 4.39902E-05; GammaY[19,15] := 6.28872E-05;
  GammaY[20,15] := 8.83205E-05; GammaY[21,15] := 0.000122063; GammaY[22,15] := 0.000166247; GammaY[23,15] := 0.000223419; GammaY[24,15] := 0.000296597; GammaY[25,15] := 0.000389327; GammaY[26,15] := 0.000505753; GammaY[27,15] := 0.000650686; GammaY[28,15] := 0.000829683; GammaY[29,15] := 0.001049128;
  GammaY[30,15] := 0.001316318; GammaY[31,15] := 0.00163956; GammaY[32,15] := 0.002028272; GammaY[33,15] := 0.002493084; GammaY[34,15] := 0.003045959; GammaY[35,15] := 0.00370031; GammaY[36,15] := 0.004471127; GammaY[37,15] := 0.005375116; GammaY[38,15] := 0.006430846; GammaY[39,15] := 0.007658895;
  GammaY[40,15] := 0.009082022; GammaY[41,15] := 0.010725336; GammaY[42,15] := 0.012616479; GammaY[43,15] := 0.014785828; GammaY[44,15] := 0.017266702; GammaY[45,15] := 0.020095582; GammaY[46,15] := 0.023312359; GammaY[47,15] := 0.026960585; GammaY[48,15] := 0.031087752; GammaY[49,15] := 0.035745588;
  GammaY[50,15] := 0.040990382; GammaY[51,15] := 0.046883331; GammaY[52,15] := 0.053490925; GammaY[53,15] := 0.060885354; GammaY[54,15] := 0.06914497; GammaY[55,15] := 0.078354786; GammaY[56,15] := 0.088607024; GammaY[57,15] := 0.100001733; GammaY[58,15] := 0.112647463; GammaY[59,15] := 0.126662032;
  GammaY[60,15] := 0.142173373; GammaY[61,15] := 0.159320496; GammaY[62,15] := 0.178254572; GammaY[63,15] := 0.199140165; GammaY[64,15] := 0.222156635; GammaY[65,15] := 0.247499745; GammaY[66,15] := 0.275383508; GammaY[67,15] := 0.306042318; GammaY[68,15] := 0.339733429; GammaY[69,15] := 0.376739827;
  GammaY[70,15] := 0.417373626; GammaY[71,15] := 0.461980038; GammaY[72,15] := 0.510942069; GammaY[73,15] := 0.564686142; GammaY[74,15] := 0.623688804; GammaY[75,15] := 0.688484827; GammaY[76,15] := 0.759677042; GammaY[77,15] := 0.837948373; GammaY[78,15] := 0.924076656; GammaY[79,15] := 1.018956386;
  GammaY[80,15] := 1.123612106; GammaY[81,15] := 1.239234755; GammaY[82,15] := 1.367225587; GammaY[83,15] := 1.50923604; GammaY[84,15] := 1.667236112; GammaY[85,15] := 1.843601941; GammaY[86,15] := 2.041235788; GammaY[87,15] := 2.263732364; GammaY[88,15] := 2.515617467; GammaY[89,15] := 2.802699378;
  GammaY[90,15] := 3.132600853; GammaY[91,15] := 3.51559694; GammaY[92,15] := 3.966000194; GammaY[93,15] := 4.504582631; GammaY[94,15] := 5.163137623; GammaY[95,15] := 5.9939463; GammaY[96,15] := 7.092332298; GammaY[97,15] := 8.663153597; GammaY[98,15] := 11.31283961; GammaY[99,15] := 18.00610257;
end;
procedure InitGammaY17;
begin
  GammaY[0,16] := 3.17746E-13; GammaY[1,16] := 4.83504E-11; GammaY[2,16] := 6.49315E-10; GammaY[3,16] := 4.23378E-09; GammaY[4,16] := 1.82828E-08; GammaY[5,16] := 6.04373E-08; GammaY[6,16] := 1.6578E-07; GammaY[7,16] := 3.96436E-07; GammaY[8,16] := 8.53621E-07; GammaY[9,16] := 1.69223E-06;
  GammaY[10,16] := 3.13806E-06; GammaY[11,16] := 5.5078E-06; GammaY[12,16] := 9.23178E-06; GammaY[13,16] := 1.48796E-05; GammaY[14,16] := 2.31887E-05; GammaY[15,16] := 3.50961E-05; GammaY[16,16] := 5.17729E-05; GammaY[17,16] := 7.46619E-05; GammaY[18,16] := 0.000105519; GammaY[19,16] := 0.000146458;
  GammaY[20,16] := 0.000199995; GammaY[21,16] := 0.000269103; GammaY[22,16] := 0.000357267; GammaY[23,16] := 0.000468534; GammaY[24,16] := 0.000607586; GammaY[25,16] := 0.000779793; GammaY[26,16] := 0.00099129; GammaY[27,16] := 0.001249047; GammaY[28,16] := 0.001560942; GammaY[29,16] := 0.001935848;
  GammaY[30,16] := 0.002383708; GammaY[31,16] := 0.002915633; GammaY[32,16] := 0.00354399; GammaY[33,16] := 0.004282498; GammaY[34,16] := 0.005146335; GammaY[35,16] := 0.006152244; GammaY[36,16] := 0.007318647; GammaY[37,16] := 0.008665761; GammaY[38,16] := 0.010215726; GammaY[39,16] := 0.011992741;
  GammaY[40,16] := 0.014023195; GammaY[41,16] := 0.016335823; GammaY[42,16] := 0.018961861; GammaY[43,16] := 0.021935209; GammaY[44,16] := 0.025292616; GammaY[45,16] := 0.029073862; GammaY[46,16] := 0.033321971; GammaY[47,16] := 0.038083421; GammaY[48,16] := 0.043408389; GammaY[49,16] := 0.049351003;
  GammaY[50,16] := 0.055969618; GammaY[51,16] := 0.063327128; GammaY[52,16] := 0.07149129; GammaY[53,16] := 0.080535092; GammaY[54,16] := 0.090537156; GammaY[55,16] := 0.101582183; GammaY[56,16] := 0.113761446; GammaY[57,16] := 0.12717334; GammaY[58,16] := 0.141923999; GammaY[59,16] := 0.158127986;
  GammaY[60,16] := 0.175909071; GammaY[61,16] := 0.19540111; GammaY[62,16] := 0.216749041; GammaY[63,16] := 0.240110023; GammaY[64,16] := 0.26565473; GammaY[65,16] := 0.293568842; GammaY[66,16] := 0.324054755; GammaY[67,16] := 0.357333565; GammaY[68,16] := 0.393647375; GammaY[69,16] := 0.433261978;
  GammaY[70,16] := 0.476469998; GammaY[71,16] := 0.5235946; GammaY[72,16] := 0.574993882; GammaY[73,16] := 0.631066099; GammaY[74,16] := 0.692255922; GammaY[75,16] := 0.759061988; GammaY[76,16] := 0.832046061; GammaY[77,16] := 0.911844212; GammaY[78,16] := 0.999183604; GammaY[79,16] := 1.094890766;
  GammaY[80,16] := 1.199919485; GammaY[81,16] := 1.31538399; GammaY[82,16] := 1.442584655; GammaY[83,16] := 1.583057693; GammaY[84,16] := 1.738635773; GammaY[85,16] := 1.91152876; GammaY[86,16] := 2.104434775; GammaY[87,16] := 2.320695945; GammaY[88,16] := 2.564519976; GammaY[89,16] := 2.841304811;
  GammaY[90,16] := 3.15813203; GammaY[91,16] := 3.524545471; GammaY[92,16] := 3.953833722; GammaY[93,16] := 4.465274833; GammaY[94,16] := 5.088359901; GammaY[95,16] := 5.87155678; GammaY[96,16] := 6.903178442; GammaY[97,16] := 8.372862807; GammaY[98,16] := 10.84138175; GammaY[99,16] := 17.04198486;
end;
procedure InitGammaY18;
begin
  GammaY[0,17] := 3.22803E-12; GammaY[1,17] := 3.42729E-10; GammaY[2,17] := 3.7726E-09; GammaY[3,17] := 2.13067E-08; GammaY[4,17] := 8.20736E-08; GammaY[5,17] := 2.46746E-07; GammaY[6,17] := 6.2417E-07; GammaY[7,17] := 1.39097E-06; GammaY[8,17] := 2.81397E-06; GammaY[9,17] := 5.27532E-06;
  GammaY[10,17] := 9.30014E-06; GammaY[11,17] := 1.55868E-05; GammaY[12,17] := 2.50394E-05; GammaY[13,17] := 3.8803E-05; GammaY[14,17] := 5.83005E-05; GammaY[15,17] := 8.52727E-05; GammaY[16,17] := 0.00012182; GammaY[17,17] := 0.000170445; GammaY[18,17] := 0.000234101; GammaY[19,17] := 0.00031624;
  GammaY[20,17] := 0.000420859; GammaY[21,17] := 0.000552559; GammaY[22,17] := 0.000716593; GammaY[23,17] := 0.000918929; GammaY[24,17] := 0.001166304; GammaY[25,17] := 0.001466289; GammaY[26,17] := 0.001827347; GammaY[27,17] := 0.002258905; GammaY[28,17] := 0.002771418; GammaY[29,17] := 0.003376441;
  GammaY[30,17] := 0.004086701; GammaY[31,17] := 0.004916175; GammaY[32,17] := 0.005880168; GammaY[33,17] := 0.006995393; GammaY[34,17] := 0.008280061; GammaY[35,17] := 0.009753968; GammaY[36,17] := 0.011438589; GammaY[37,17] := 0.013357175; GammaY[38,17] := 0.01553486; GammaY[39,17] := 0.017998765;
  GammaY[40,17] := 0.02077812; GammaY[41,17] := 0.023904378; GammaY[42,17] := 0.027411354; GammaY[43,17] := 0.031335358; GammaY[44,17] := 0.035715348; GammaY[45,17] := 0.040593089; GammaY[46,17] := 0.046013325; GammaY[47,17] := 0.052023971; GammaY[48,17] := 0.058676314; GammaY[49,17] := 0.066025236;
  GammaY[50,17] := 0.074129461; GammaY[51,17] := 0.083051819; GammaY[52,17] := 0.092859546; GammaY[53,17] := 0.103624604; GammaY[54,17] := 0.115424052; GammaY[55,17] := 0.128340441; GammaY[56,17] := 0.142462267; GammaY[57,17] := 0.157884477; GammaY[58,17] := 0.174709028; GammaY[59,17] := 0.193045531;
  GammaY[60,17] := 0.213011962; GammaY[61,17] := 0.234735482; GammaY[62,17] := 0.25835336; GammaY[63,17] := 0.284014033; GammaY[64,17] := 0.311878306; GammaY[65,17] := 0.342120744; GammaY[66,17] := 0.374931274; GammaY[67,17] := 0.410517034; GammaY[68,17] := 0.449104511; GammaY[69,17] := 0.490942059;
  GammaY[70,17] := 0.536302828; GammaY[71,17] := 0.585488231; GammaY[72,17] := 0.638832031; GammaY[73,17] := 0.696705222; GammaY[74,17] := 0.759521864; GammaY[75,17] := 0.827746092; GammaY[76,17] := 0.901900716; GammaY[77,17] := 0.982580226; GammaY[78,17] := 1.070455796; GammaY[79,17] := 1.166296448;
  GammaY[80,17] := 1.270994773; GammaY[81,17] := 1.38558449; GammaY[82,17] := 1.511275447; GammaY[83,17] := 1.649496232; GammaY[84,17] := 1.80195138; GammaY[85,17] := 1.970696002; GammaY[86,17] := 2.158238706; GammaY[87,17] := 2.367687428; GammaY[88,17] := 2.602953515; GammaY[89,17] := 2.86905289;
  GammaY[90,17] := 3.172565341; GammaY[91,17] := 3.52235549; GammaY[92,17] := 3.930760757; GammaY[93,17] := 4.415671803; GammaY[94,17] := 5.004444798; GammaY[95,17] := 5.742013282; GammaY[96,17] := 6.710195679; GammaY[97,17] := 8.084545472; GammaY[98,17] := 10.38364313; GammaY[99,17] := 16.12762459;
end;
procedure InitGammaY19;
begin
  GammaY[0,18] := 2.6968E-11; GammaY[1,18] := 2.05775E-09; GammaY[2,18] := 1.88955E-08; GammaY[3,18] := 9.34967E-08; GammaY[4,18] := 3.24112E-07; GammaY[5,18] := 8.9276E-07; GammaY[6,18] := 2.09595E-06; GammaY[7,18] := 4.37727E-06; GammaY[8,18] := 8.36155E-06; GammaY[9,18] := 1.48904E-05;
  GammaY[10,18] := 2.5059E-05; GammaY[11,18] := 4.02543E-05; GammaY[12,18] := 6.21934E-05; GammaY[13,18] := 9.29643E-05; GammaY[14,18] := 0.000135066; GammaY[15,18] := 0.000191452; GammaY[16,18] := 0.000265569; GammaY[17,18] := 0.000361406; GammaY[18,18] := 0.000483534; GammaY[19,18] := 0.000637154;
  GammaY[20,18] := 0.000828139; GammaY[21,18] := 0.001063087; GammaY[22,18] := 0.001349362; GammaY[23,18] := 0.001695149; GammaY[24,18] := 0.002109496; GammaY[25,18] := 0.002602374; GammaY[26,18] := 0.003184718; GammaY[27,18] := 0.003868492; GammaY[28,18] := 0.004666731; GammaY[29,18] := 0.005593609;
  GammaY[30,18] := 0.006664487; GammaY[31,18] := 0.007895979; GammaY[32,18] := 0.00930601; GammaY[33,18] := 0.010913885; GammaY[34,18] := 0.012740353; GammaY[35,18] := 0.014807676; GammaY[36,18] := 0.01713971; GammaY[37,18] := 0.019761976; GammaY[38,18] := 0.022701746; GammaY[39,18] := 0.025988134;
  GammaY[40,18] := 0.029652185; GammaY[41,18] := 0.03372698; GammaY[42,18] := 0.038247745; GammaY[43,18] := 0.043251964; GammaY[44,18] := 0.048779511; GammaY[45,18] := 0.054872785; GammaY[46,18] := 0.061576864; GammaY[47,18] := 0.068939665; GammaY[48,18] := 0.077012129; GammaY[49,18] := 0.085848416;
  GammaY[50,18] := 0.095506128; GammaY[51,18] := 0.106046548; GammaY[52,18] := 0.117534912; GammaY[53,18] := 0.130040704; GammaY[54,18] := 0.143637991; GammaY[55,18] := 0.158405797; GammaY[56,18] := 0.174428512; GammaY[57,18] := 0.191796366; GammaY[58,18] := 0.210605954; GammaY[59,18] := 0.230960822;
  GammaY[60,18] := 0.252972146; GammaY[61,18] := 0.276759487; GammaY[62,18] := 0.302451658; GammaY[63,18] := 0.330187711; GammaY[64,18] := 0.360118071; GammaY[65,18] := 0.392405823; GammaY[66,18] := 0.427228212; GammaY[67,18] := 0.464778375; GammaY[68,18] := 0.505267344; GammaY[69,18] := 0.54892639;
  GammaY[70,18] := 0.596009765; GammaY[71,18] := 0.646797933; GammaY[72,18] := 0.701601389; GammaY[73,18] := 0.760765185; GammaY[74,18] := 0.824674427; GammaY[75,18] := 0.893760802; GammaY[76,18] := 0.968512838; GammaY[77,18] := 1.049478503; GammaY[78,18] := 1.137282236; GammaY[79,18] := 1.232644438;
  GammaY[80,18] := 1.336392531; GammaY[81,18] := 1.449488061; GammaY[82,18] := 1.573056359; GammaY[83,18] := 1.708426219; GammaY[84,18] := 1.857182728; GammaY[85,18] := 2.021237489; GammaY[86,18] := 2.202924199; GammaY[87,18] := 2.405131352; GammaY[88,18] := 2.631494585; GammaY[89,18] := 2.886676497;
  GammaY[90,18] := 3.176790449; GammaY[91,18] := 3.510069355; GammaY[92,18] := 3.897968341; GammaY[93,18] := 4.357091365; GammaY[94,18] := 4.912813987; GammaY[95,18] := 5.60679387; GammaY[96,18] := 6.514835562; GammaY[97,18] := 7.799462745; GammaY[98,18] := 9.940272161; GammaY[99,18] := 15.26119062;
end;
procedure InitGammaY20;
begin
  GammaY[0,19] := 1.88422E-10; GammaY[1,19] := 1.06144E-08; GammaY[2,19] := 8.26012E-08; GammaY[3,19] := 3.61796E-07; GammaY[4,19] := 1.13788E-06; GammaY[5,19] := 2.89122E-06; GammaY[6,19] := 6.33686E-06; GammaY[7,19] := 1.24666E-05; GammaY[8,19] := 2.25898E-05; GammaY[9,19] := 3.83728E-05;
  GammaY[10,19] := 6.18782E-05; GammaY[11,19] := 9.56018E-05; GammaY[12,19] := 0.00014251; GammaY[13,19] := 0.000206079; GammaY[14,19] := 0.000290325; GammaY[15,19] := 0.000399845; GammaY[16,19] := 0.000539853; GammaY[17,19] := 0.000716209; GammaY[18,19] := 0.00093546; GammaY[19,19] := 0.001204873;
  GammaY[20,19] := 0.001532466; GammaY[21,19] := 0.00192705; GammaY[22,19] := 0.002398259; GammaY[23,19] := 0.002956586; GammaY[24,19] := 0.003613419; GammaY[25,19] := 0.00438108; GammaY[26,19] := 0.005272859; GammaY[27,19] := 0.006303054; GammaY[28,19] := 0.007487008; GammaY[29,19] := 0.008841154;
  GammaY[30,19] := 0.010383052; GammaY[31,19] := 0.012131437; GammaY[32,19] := 0.014106263; GammaY[33,19] := 0.016328753; GammaY[34,19] := 0.01882145; GammaY[35,19] := 0.021608273; GammaY[36,19] := 0.024714572; GammaY[37,19] := 0.028167197; GammaY[38,19] := 0.03199456; GammaY[39,19] := 0.036226711;
  GammaY[40,19] := 0.040895416; GammaY[41,19] := 0.046034244; GammaY[42,19] := 0.051678659; GammaY[43,19] := 0.057866124; GammaY[44,19] := 0.064636213; GammaY[45,19] := 0.072030733; GammaY[46,19] := 0.08009386; GammaY[47,19] := 0.08887229; GammaY[48,19] := 0.0984154; GammaY[49,19] := 0.108775434;
  GammaY[50,19] := 0.120007702; GammaY[51,19] := 0.132170807; GammaY[52,19] := 0.145326896; GammaY[53,19] := 0.159541932; GammaY[54,19] := 0.174886012; GammaY[55,19] := 0.19143371; GammaY[56,19] := 0.209264468; GammaY[57,19] := 0.228463035; GammaY[58,19] := 0.249119958; GammaY[59,19] := 0.271332134;
  GammaY[60,19] := 0.295203452; GammaY[61,19] := 0.320845493; GammaY[62,19] := 0.348378346; GammaY[63,19] := 0.377931533; GammaY[64,19] := 0.409645066; GammaY[65,19] := 0.443670663; GammaY[66,19] := 0.480173145; GammaY[67,19] := 0.519332056; GammaY[68,19] := 0.561343536; GammaY[69,19] := 0.606422512;
  GammaY[70,19] := 0.654805259; GammaY[71,19] := 0.706752392; GammaY[72,19] := 0.762552482; GammaY[73,19] := 0.822526269; GammaY[74,19] := 0.887031708; GammaY[75,19] := 0.956470113; GammaY[76,19] := 1.031295652; GammaY[77,19] := 1.112017892; GammaY[78,19] := 1.199217637; GammaY[79,19] := 1.293565184;
  GammaY[80,19] := 1.39583018; GammaY[81,19] := 1.506906311; GammaY[82,19] := 1.627840948; GammaY[83,19] := 1.759870722; GammaY[84,19] := 1.90446952; GammaY[85,19] := 2.063415065; GammaY[86,19] := 2.238877684; GammaY[87,19] := 2.433543765; GammaY[88,19] := 2.650793179; GammaY[89,19] := 2.894959124;
  GammaY[90,19] := 3.171721836; GammaY[91,19] := 3.48872839; GammaY[92,19] := 3.856614468; GammaY[93,19] := 4.290790362; GammaY[94,19] := 4.814795902; GammaY[95,19] := 5.467252912; GammaY[96,19] := 6.318398317; GammaY[97,19] := 7.518704064; GammaY[98,19] := 9.511753498; GammaY[99,19] := 14.44083625;
end;
procedure InitGammaY21;
begin
  GammaY[0,20] := 1.11817E-09; GammaY[1,20] := 4.76549E-08; GammaY[2,20] := 3.18735E-07; GammaY[3,20] := 1.24741E-06; GammaY[4,20] := 3.58597E-06; GammaY[5,20] := 8.45793E-06; GammaY[6,20] := 1.74003E-05; GammaY[7,20] := 3.24005E-05; GammaY[8,20] := 5.59293E-05; GammaY[9,20] := 9.09713E-05;
  GammaY[10,20] := 0.000141053; GammaY[11,20] := 0.000210267; GammaY[12,20] := 0.000303301; GammaY[13,20] := 0.000425458; GammaY[14,20] := 0.000582677; GammaY[15,20] := 0.000781562; GammaY[16,20] := 0.001029395; GammaY[17,20] := 0.001334161; GammaY[18,20] := 0.00170457; GammaY[19,20] := 0.002150072;
  GammaY[20,20] := 0.002680882; GammaY[21,20] := 0.003308; GammaY[22,20] := 0.004043228; GammaY[23,20] := 0.004899196; GammaY[24,20] := 0.005889382; GammaY[25,20] := 0.007028132; GammaY[26,20] := 0.008330688; GammaY[27,20] := 0.009813212; GammaY[28,20] := 0.01149281; GammaY[29,20] := 0.013387563;
  GammaY[30,20] := 0.015516555; GammaY[31,20] := 0.017899906; GammaY[32,20] := 0.020558809; GammaY[33,20] := 0.023515562; GammaY[34,20] := 0.026793616; GammaY[35,20] := 0.030417616; GammaY[36,20] := 0.034413447; GammaY[37,20] := 0.038808293; GammaY[38,20] := 0.043630691; GammaY[39,20] := 0.048910597;
  GammaY[40,20] := 0.054679454; GammaY[41,20] := 0.060970273; GammaY[42,20] := 0.067817713; GammaY[43,20] := 0.075258179; GammaY[44,20] := 0.083329923; GammaY[45,20] := 0.092073158; GammaY[46,20] := 0.101530186; GammaY[47,20] := 0.111745536; GammaY[48,20] := 0.122766118; GammaY[49,20] := 0.134641397;
  GammaY[50,20] := 0.147423579; GammaY[51,20] := 0.161167827; GammaY[52,20] := 0.175932496; GammaY[53,20] := 0.19177939; GammaY[54,20] := 0.208774063; GammaY[55,20] := 0.226986142; GammaY[56,20] := 0.246489695; GammaY[57,20] := 0.267363639; GammaY[58,20] := 0.289692216; GammaY[59,20] := 0.313565507;
  GammaY[60,20] := 0.339080022; GammaY[61,20] := 0.366339373; GammaY[62,20] := 0.395455034; GammaY[63,20] := 0.42654721; GammaY[64,20] := 0.459745827; GammaY[65,20] := 0.495191668; GammaY[66,20] := 0.533037684; GammaY[67,20] := 0.573450506; GammaY[68,20] := 0.616612195; GammaY[69,20] := 0.662722273;
  GammaY[70,20] := 0.712000153; GammaY[71,20] := 0.764687932; GammaY[72,20] := 0.821053686; GammaY[73,20] := 0.881395454; GammaY[74,20] := 0.94604596; GammaY[75,20] := 1.015380105; GammaY[76,20] := 1.089816342; GammaY[77,20] := 1.169828974; GammaY[78,20] := 1.255962103; GammaY[79,20] := 1.348836858;
  GammaY[80,20] := 1.449169253; GammaY[81,20] := 1.557791255; GammaY[82,20] := 1.675677379; GammaY[83,20] := 1.803977622; GammaY[84,20] := 1.944063433; GammaY[85,20] := 2.097588763; GammaY[86,20] := 2.26657193; GammaY[87,20] := 2.453511078; GammaY[88,20] := 2.661548241; GammaY[89,20] := 2.894711315;
  GammaY[90,20] := 3.158278696; GammaY[91,20] := 3.459353101; GammaY[92,20] := 3.807810721; GammaY[93,20] := 4.217954292; GammaY[94,20] := 4.711621439; GammaY[95,20] := 5.324622804; GammaY[96,20] := 6.122041178; GammaY[97,20] := 7.243202621; GammaY[98,20] := 9.09842599; GammaY[99,20] := 13.66469198;
end;
procedure InitGammaY22;
begin
  GammaY[0,21] := 5.71684E-09; GammaY[1,21] := 1.88444E-07; GammaY[2,21] := 1.09691E-06; GammaY[3,21] := 3.86856E-06; GammaY[4,21] := 1.02351E-05; GammaY[5,21] := 2.2539E-05; GammaY[6,21] := 4.37423E-05; GammaY[7,21] := 7.74331E-05; GammaY[8,21] := 0.000127831; GammaY[9,21] := 0.000199795;
  GammaY[10,21] := 0.000298822; GammaY[11,21] := 0.000431058; GammaY[12,21] := 0.0006033; GammaY[13,21] := 0.000822997; GammaY[14,21] := 0.00109826; GammaY[15,21] := 0.001437862; GammaY[16,21] := 0.001851246; GammaY[17,21] := 0.002348528; GammaY[18,21] := 0.002940506; GammaY[19,21] := 0.003638662;
  GammaY[20,21] := 0.004455174; GammaY[21,21] := 0.005402922; GammaY[22,21] := 0.006495496; GammaY[23,21] := 0.007747209; GammaY[24,21] := 0.009173107; GammaY[25,21] := 0.010788983; GammaY[26,21] := 0.012611393; GammaY[27,21] := 0.014657669; GammaY[28,21] := 0.016945942; GammaY[29,21] := 0.019495159;
  GammaY[30,21] := 0.022325111; GammaY[31,21] := 0.025456455; GammaY[32,21] := 0.028910744; GammaY[33,21] := 0.03271046; GammaY[34,21] := 0.036879053; GammaY[35,21] := 0.041440975; GammaY[36,21] := 0.046421727; GammaY[37,21] := 0.051847909; GammaY[38,21] := 0.057747273; GammaY[39,21] := 0.064148783;
  GammaY[40,21] := 0.071082683; GammaY[41,21] := 0.078580566; GammaY[42,21] := 0.086675459; GammaY[43,21] := 0.09540191; GammaY[44,21] := 0.104796087; GammaY[45,21] := 0.114895889; GammaY[46,21] := 0.125741063; GammaY[47,21] := 0.137373338; GammaY[48,21] := 0.149836576; GammaY[49,21] := 0.163176932;
  GammaY[50,21] := 0.177443035; GammaY[51,21] := 0.192686192; GammaY[52,21] := 0.208960607; GammaY[53,21] := 0.226323637; GammaY[54,21] := 0.24483606; GammaY[55,21] := 0.264562394; GammaY[56,21] := 0.285571242; GammaY[57,21] := 0.307935675; GammaY[58,21] := 0.331733674; GammaY[59,21] := 0.357048623;
  GammaY[60,21] := 0.383969859; GammaY[61,21] := 0.412593306; GammaY[62,21] := 0.443022188; GammaY[63,21] := 0.47536784; GammaY[64,21] := 0.509750635; GammaY[65,21] := 0.546301051; GammaY[66,21] := 0.585160885; GammaY[67,21] := 0.626484685; GammaY[68,21] := 0.670441381; GammaY[69,21] := 0.717216172;
  GammaY[70,21] := 0.767012779; GammaY[71,21] := 0.820056058; GammaY[72,21] := 0.8765951; GammaY[73,21] := 0.936906918; GammaY[74,21] := 1.001302402; GammaY[75,21] := 1.070126921; GammaY[76,21] := 1.143769947; GammaY[77,21] := 1.22267574; GammaY[78,21] := 1.307348719; GammaY[79,21] := 1.398367315;
  GammaY[80,21] := 1.496397843; GammaY[81,21] := 1.602214305; GammaY[82,21] := 1.716723332; GammaY[83,21] := 1.840995302; GammaY[84,21] := 1.976306544; GammaY[85,21] := 2.124195299; GammaY[86,21] := 2.286539267; GammaY[87,21] := 2.465661786; GammaY[88,21] := 2.66448459; GammaY[89,21] := 2.886751848;
  GammaY[90,21] := 3.137367845; GammaY[91,21] := 3.422931254; GammaY[92,21] := 3.752613438; GammaY[93,21] := 4.139690114; GammaY[94,21] := 4.604421484; GammaY[95,21] := 5.18001452; GammaY[96,21] := 5.92678203; GammaY[97,21] := 6.973744218; GammaY[98,21] := 8.700497871; GammaY[99,21] := 12.93091605;
end;
procedure InitGammaY23;
begin
  GammaY[0,22] := 2.55113E-08; GammaY[1,22] := 6.63475E-07; GammaY[2,22] := 3.39873E-06; GammaY[3,22] := 1.0886E-05; GammaY[4,22] := 2.66748E-05; GammaY[5,22] := 5.51387E-05; GammaY[6,22] := 0.000101416; GammaY[7,22] := 0.000171364; GammaY[8,22] := 0.000271532; GammaY[9,22] := 0.000409126;
  GammaY[10,22] := 0.000591993; GammaY[11,22] := 0.000828599; GammaY[12,22] := 0.001128016; GammaY[13,22] := 0.001499909; GammaY[14,22] := 0.001954523; GammaY[15,22] := 0.002502678; GammaY[16,22] := 0.003155758; GammaY[17,22] := 0.003925712; GammaY[18,22] := 0.004825046; GammaY[19,22] := 0.005866823;
  GammaY[20,22] := 0.007064664; GammaY[21,22] := 0.008432752; GammaY[22,22] := 0.009985831; GammaY[23,22] := 0.011739217; GammaY[24,22] := 0.013708802; GammaY[25,22] := 0.015911069; GammaY[26,22] := 0.018363098; GammaY[27,22] := 0.021082584; GammaY[28,22] := 0.024087854; GammaY[29,22] := 0.027397887;
  GammaY[30,22] := 0.031032332; GammaY[31,22] := 0.035011538; GammaY[32,22] := 0.039356582; GammaY[33,22] := 0.044089299; GammaY[34,22] := 0.049232316; GammaY[35,22] := 0.054809096; GammaY[36,22] := 0.060843978; GammaY[37,22] := 0.067362231; GammaY[38,22] := 0.074390097; GammaY[39,22] := 0.081954863;
  GammaY[40,22] := 0.090084917; GammaY[41,22] := 0.098809823; GammaY[42,22] := 0.1081604; GammaY[43,22] := 0.11816881; GammaY[44,22] := 0.128868652; GammaY[45,22] := 0.140295069; GammaY[46,22] := 0.152484862; GammaY[47,22] := 0.165476623; GammaY[48,22] := 0.179310873; GammaY[49,22] := 0.194030217;
  GammaY[50,22] := 0.209679523; GammaY[51,22] := 0.226306104; GammaY[52,22] := 0.243959943; GammaY[53,22] := 0.26269392; GammaY[54,22] := 0.282564077; GammaY[55,22] := 0.30362991; GammaY[56,22] := 0.325954697; GammaY[57,22] := 0.349605868; GammaY[58,22] := 0.374655407; GammaY[59,22] := 0.401180322;
  GammaY[60,22] := 0.429263165; GammaY[61,22] := 0.458992618; GammaY[62,22] := 0.490464164; GammaY[63,22] := 0.523780848; GammaY[64,22] := 0.559054132; GammaY[65,22] := 0.596404918; GammaY[66,22] := 0.635964676; GammaY[67,22] := 0.677876747; GammaY[68,22] := 0.722297889; GammaY[69,22] := 0.769400051;
  GammaY[70,22] := 0.819372455; GammaY[71,22] := 0.872424037; GammaY[72,22] := 0.928786335; GammaY[73,22] := 0.988718279; GammaY[74,22] := 1.052506261; GammaY[75,22] := 1.120471502; GammaY[76,22] := 1.192979153; GammaY[77,22] := 1.270441584; GammaY[78,22] := 1.353328276; GammaY[79,22] := 1.442177201;
  GammaY[80,22] := 1.537608819; GammaY[81,22] := 1.640343818; GammaY[82,22] := 1.751225879; GammaY[83,22] := 1.871250995; GammaY[84,22] := 2.001607181; GammaY[85,22] := 2.143726018; GammaY[86,22] := 2.299354145; GammaY[87,22] := 2.470652557; GammaY[88,22] := 2.660338227; GammaY[89,22] := 2.871891663;
  GammaY[90,22] := 3.109870989; GammaY[91,22] := 3.380407168; GammaY[92,22] := 3.692016839; GammaY[93,22] := 4.057023917; GammaY[94,22] := 4.494227706; GammaY[95,22] := 5.034423551; GammaY[96,22] := 5.73351068; GammaY[97,22] := 6.710981197; GammaY[98,22] := 8.318060809; GammaY[99,22] := 12.23766816;
end;
procedure InitGammaY24;
begin
  GammaY[0,23] := 1.00559E-07; GammaY[1,23] := 2.10049E-06; GammaY[2,23] := 9.56339E-06; GammaY[3,23] := 2.8018E-05; GammaY[4,23] := 6.39594E-05; GammaY[5,23] := 0.000124715; GammaY[6,23] := 0.000218325; GammaY[7,23] := 0.000353457; GammaY[8,23] := 0.00053935; GammaY[9,23] := 0.00078577;
  GammaY[10,23] := 0.001102972; GammaY[11,23] := 0.001501677; GammaY[12,23] := 0.001993043; GammaY[13,23] := 0.002588655; GammaY[14,23] := 0.003300505; GammaY[15,23] := 0.004140986; GammaY[16,23] := 0.005122881; GammaY[17,23] := 0.006259359; GammaY[18,23] := 0.007563972; GammaY[19,23] := 0.009050656;
  GammaY[20,23] := 0.010733729; GammaY[21,23] := 0.012627902; GammaY[22,23] := 0.014748278; GammaY[23,23] := 0.017110363; GammaY[24,23] := 0.019730079; GammaY[25,23] := 0.022623771; GammaY[26,23] := 0.025808228; GammaY[27,23] := 0.029300698; GammaY[28,23] := 0.033118905; GammaY[29,23] := 0.037281076;
  GammaY[30,23] := 0.041805962; GammaY[31,23] := 0.046712869; GammaY[32,23] := 0.052021687; GammaY[33,23] := 0.057752927; GammaY[34,23] := 0.063927753; GammaY[35,23] := 0.070568029; GammaY[36,23] := 0.077696363; GammaY[37,23] := 0.085336154; GammaY[38,23] := 0.093511651; GammaY[39,23] := 0.102248007;
  GammaY[40,23] := 0.111571349; GammaY[41,23] := 0.121508847; GammaY[42,23] := 0.132088794; GammaY[43,23] := 0.143340685; GammaY[44,23] := 0.155295321; GammaY[45,23] := 0.1679849; GammaY[46,23] := 0.181443138; GammaY[47,23] := 0.195705389; GammaY[48,23] := 0.210808776; GammaY[49,23] := 0.226792351;
  GammaY[50,23] := 0.243697253; GammaY[51,23] := 0.261566888; GammaY[52,23] := 0.280447134; GammaY[53,23] := 0.300386564; GammaY[54,23] := 0.321436691; GammaY[55,23] := 0.343652247; GammaY[56,23] := 0.367091489; GammaY[57,23] := 0.391816545; GammaY[58,23] := 0.417893796; GammaY[59,23] := 0.445394311;
  GammaY[60,23] := 0.474394337; GammaY[61,23] := 0.50497584; GammaY[62,23] := 0.537227151; GammaY[63,23] := 0.571243666; GammaY[64,23] := 0.607128644; GammaY[65,23] := 0.644994152; GammaY[66,23] := 0.684962126; GammaY[67,23] := 0.727165598; GammaY[68,23] := 0.771750127; GammaY[69,23] := 0.818875454;
  GammaY[70,23] := 0.86871744; GammaY[71,23] := 0.921470343; GammaY[72,23] := 0.977350705; GammaY[73,23] := 1.036597062; GammaY[74,23] := 1.099475759; GammaY[75,23] := 1.16628832; GammaY[76,23] := 1.237373415; GammaY[77,23] := 1.313114969; GammaY[78,23] := 1.393950326; GammaY[79,23] := 1.480380199;
  GammaY[80,23] := 1.572982085; GammaY[81,23] := 1.672427218; GammaY[82,23] := 1.779501079; GammaY[83,23] := 1.895131094; GammaY[84,23] := 2.020422848; GammaY[85,23] := 2.156708422; GammaY[86,23] := 2.305613458; GammaY[87,23] := 2.469148009; GammaY[88,23] := 2.649838704; GammaY[89,23] := 2.850921836;
  GammaY[90,23] := 3.076634264; GammaY[91,23] := 3.332672336; GammaY[92,23] := 3.626946331; GammaY[93,23] := 3.970897941; GammaY[94,23] := 4.381972605; GammaY[95,23] := 4.888732914; GammaY[96,23] := 5.542995001; GammaY[97,23] := 6.455443652; GammaY[98,23] := 7.951106078; GammaY[99,23] := 11.58313153;
end;
procedure InitGammaY25;
begin
  GammaY[0,24] := 3.53949E-07; GammaY[1,24] := 6.03352E-06; GammaY[2,24] := 2.4632E-05; GammaY[3,24] := 6.64436E-05; GammaY[4,24] := 0.000142072; GammaY[5,24] := 0.000262526; GammaY[6,24] := 0.000439142; GammaY[7,24] := 0.000683527; GammaY[8,24] := 0.001007527; GammaY[9,24] := 0.001423207;
  GammaY[10,24] := 0.001942831; GammaY[11,24] := 0.002578851; GammaY[12,24] := 0.003343906; GammaY[13,24] := 0.00425081; GammaY[14,24] := 0.005312557; GammaY[15,24] := 0.006542319; GammaY[16,24] := 0.007953452; GammaY[17,24] := 0.009559499; GammaY[18,24] := 0.011374197; GammaY[19,24] := 0.013411488;
  GammaY[20,24] := 0.015685529; GammaY[21,24] := 0.018210701; GammaY[22,24] := 0.021001628; GammaY[23,24] := 0.024073189; GammaY[24,24] := 0.027440535; GammaY[25,24] := 0.03111911; GammaY[26,24] := 0.035124673; GammaY[27,24] := 0.039473318; GammaY[28,24] := 0.044181498; GammaY[29,24] := 0.049266056;
  GammaY[30,24] := 0.054744254; GammaY[31,24] := 0.060633799; GammaY[32,24] := 0.066952885; GammaY[33,24] := 0.073720226; GammaY[34,24] := 0.080955094; GammaY[35,24] := 0.088677369; GammaY[36,24] := 0.096907579; GammaY[37,24] := 0.105666956; GammaY[38,24] := 0.114977487; GammaY[39,24] := 0.124861977;
  GammaY[40,24] := 0.13534411; GammaY[41,24] := 0.146448522; GammaY[42,24] := 0.158200876; GammaY[43,24] := 0.170627945; GammaY[44,24] := 0.183757701; GammaY[45,24] := 0.197619413; GammaY[46,24] := 0.212243762; GammaY[47,24] := 0.227662945; GammaY[48,24] := 0.243910817; GammaY[49,24] := 0.261023025;
  GammaY[50,24] := 0.279037165; GammaY[51,24] := 0.297992958; GammaY[52,24] := 0.317932433; GammaY[53,24] := 0.338900143; GammaY[54,24] := 0.360943393; GammaY[55,24] := 0.384112504; GammaY[56,24] := 0.408461095; GammaY[57,24] := 0.434046413; GammaY[58,24] := 0.460929678; GammaY[59,24] := 0.489176515;
  GammaY[60,24] := 0.518857392; GammaY[61,24] := 0.55004813; GammaY[62,24] := 0.582830501; GammaY[63,24] := 0.617292882; GammaY[64,24] := 0.653531017; GammaY[65,24] := 0.691648879; GammaY[66,24] := 0.731759668; GammaY[67,24] := 0.773986953; GammaY[68,24] := 0.818466007; GammaY[69,24] := 0.865345346;
  GammaY[70,24] := 0.91478851; GammaY[71,24] := 0.966976254; GammaY[72,24] := 1.0221101; GammaY[73,24] := 1.080412006; GammaY[74,24] := 1.142130192; GammaY[75,24] := 1.207545324; GammaY[76,24] := 1.27697301; GammaY[77,24] := 1.350770857; GammaY[78,24] := 1.429345617; GammaY[79,24] := 1.513164065;
  GammaY[80,24] := 1.602764285; GammaY[81,24] := 1.698771015; GammaY[82,24] := 1.80191616; GammaY[83,24] := 1.913063043; GammaY[84,24] := 2.033240747; GammaY[85,24] := 2.163689613; GammaY[86,24] := 2.305922079; GammaY[87,24] := 2.461808517; GammaY[88,24] := 2.633699288; GammaY[89,24] := 2.824603573;
  GammaY[90,24] := 3.038460138; GammaY[91,24] := 3.280562072; GammaY[92,24] := 3.55825745; GammaY[93,24] := 3.882170766; GammaY[94,24] := 4.268493246; GammaY[95,24] := 4.743720805; GammaY[96,24] := 5.355892187; GammaY[97,24] := 6.207550976; GammaY[98,24] := 7.599535362; GammaY[99,24] := 10.96552532;
end;
procedure InitGammaY26;
begin
  GammaY[0,25] := 1.12356E-06; GammaY[1,25] := 1.58535E-05; GammaY[2,25] := 5.84972E-05; GammaY[3,25] := 0.00014617; GammaY[4,25] := 0.000294225; GammaY[5,25] := 0.000517411; GammaY[6,25] := 0.000830044; GammaY[7,25] := 0.001246107; GammaY[8,25] := 0.001779323; GammaY[9,25] := 0.0024432;
  GammaY[10,25] := 0.003251082; GammaY[11,25] := 0.004216174; GammaY[12,25] := 0.00535158; GammaY[13,25] := 0.006670326; GammaY[14,25] := 0.008185387; GammaY[15,25] := 0.009909708; GammaY[16,25] := 0.011856233; GammaY[17,25] := 0.014037919; GammaY[18,25] := 0.016467767; GammaY[19,25] := 0.019158837;
  GammaY[20,25] := 0.022124275; GammaY[21,25] := 0.025377335; GammaY[22,25] := 0.028931401; GammaY[23,25] := 0.032800013; GammaY[24,25] := 0.03699689; GammaY[25,25] := 0.041535958; GammaY[26,25] := 0.046431375; GammaY[27,25] := 0.051697561; GammaY[28,25] := 0.057349226; GammaY[29,25] := 0.063401401;
  GammaY[30,25] := 0.069869473; GammaY[31,25] := 0.076769216; GammaY[32,25] := 0.084116832; GammaY[33,25] := 0.091928986; GammaY[34,25] := 0.100222848; GammaY[35,25] := 0.109016142; GammaY[36,25] := 0.118327184; GammaY[37,25] := 0.128174941; GammaY[38,25] := 0.138579082; GammaY[39,25] := 0.149560034;
  GammaY[40,25] := 0.161139048; GammaY[41,25] := 0.173338264; GammaY[42,25] := 0.186180787; GammaY[43,25] := 0.199690766; GammaY[44,25] := 0.213893473; GammaY[45,25] := 0.228815407; GammaY[46,25] := 0.244484389; GammaY[47,25] := 0.260929674; GammaY[48,25] := 0.278182073; GammaY[49,25] := 0.296274089;
  GammaY[50,25] := 0.31524006; GammaY[51,25] := 0.335116319; GammaY[52,25] := 0.355941377; GammaY[53,25] := 0.377756115; GammaY[54,25] := 0.400604005; GammaY[55,25] := 0.424531348; GammaY[56,25] := 0.449587541; GammaY[57,25] := 0.475825393; GammaY[58,25] := 0.503301457; GammaY[59,25] := 0.532076389;
  GammaY[60,25] := 0.562215394; GammaY[61,25] := 0.5937887; GammaY[62,25] := 0.626872105; GammaY[63,25] := 0.661547592; GammaY[64,25] := 0.697904039; GammaY[65,25] := 0.736038021; GammaY[66,25] := 0.776054741; GammaY[67,25] := 0.81806908; GammaY[68,25] := 0.862206879; GammaY[69,25] := 0.908606359;
  GammaY[70,25] := 0.957419767; GammaY[71,25] := 1.008816431; GammaY[72,25] := 1.06298181; GammaY[73,25] := 1.12012285; GammaY[74,25] := 1.180472806; GammaY[75,25] := 1.244292333; GammaY[76,25] := 1.311875323; GammaY[77,25] := 1.383554811; GammaY[78,25] := 1.459710165; GammaY[79,25] := 1.54077592;
  GammaY[80,25] := 1.627253607; GammaY[81,25] := 1.719725419; GammaY[82,25] := 1.818872895; GammaY[83,25] := 1.925500093; GammaY[84,25] := 2.040565272; GammaY[85,25] := 2.165223166; GammaY[86,25] := 2.300881528; GammaY[87,25] := 2.449280845; GammaY[88,25] := 2.612607674; GammaY[89,25] := 2.793660655;
  GammaY[90,25] := 2.996101636; GammaY[91,25] := 3.224850122; GammaY[92,25] := 3.486733362; GammaY[93,25] := 3.791618226; GammaY[94,25] := 4.154534407; GammaY[95,25] := 4.60006493; GammaY[96,25] := 5.17275456; GammaY[97,25] := 5.967622613; GammaY[98,25] := 7.263173624; GammaY[99,25] := 10.38309513;
end;
procedure InitGammaY27;
begin
  GammaY[0,26] := 3.24548E-06; GammaY[1,26] := 3.83902E-05; GammaY[2,26] := 0.000128949; GammaY[3,26] := 0.000300164; GammaY[4,26] := 0.000571441; GammaY[5,26] := 0.000960117; GammaY[6,26] := 0.001482139; GammaY[7,26] := 0.002152432; GammaY[8,26] := 0.002985139; GammaY[9,26] := 0.003993781;
  GammaY[10,26] := 0.005191384; GammaY[11,26] := 0.006590569; GammaY[12,26] := 0.008203631; GammaY[13,26] := 0.010042602; GammaY[14,26] := 0.012119304; GammaY[15,26] := 0.014445397; GammaY[16,26] := 0.017032425; GammaY[17,26] := 0.019891849; GammaY[18,26] := 0.02303509; GammaY[19,26] := 0.026473555;
  GammaY[20,26] := 0.030218677; GammaY[21,26] := 0.034281943; GammaY[22,26] := 0.038674921; GammaY[23,26] := 0.043409298; GammaY[24,26] := 0.048496902; GammaY[25,26] := 0.053949738; GammaY[26,26] := 0.059780014; GammaY[27,26] := 0.066000176; GammaY[28,26] := 0.072622935; GammaY[29,26] := 0.079661303;
  GammaY[30,26] := 0.087128628; GammaY[31,26] := 0.095038622; GammaY[32,26] := 0.103405408; GammaY[33,26] := 0.112243549; GammaY[34,26] := 0.121568095; GammaY[35,26] := 0.131394623; GammaY[36,26] := 0.141739283; GammaY[37,26] := 0.152618847; GammaY[38,26] := 0.164050758; GammaY[39,26] := 0.17605319;
  GammaY[40,26] := 0.188645107; GammaY[41,26] := 0.201846319; GammaY[42,26] := 0.21567756; GammaY[43,26] := 0.23016056; GammaY[44,26] := 0.245318122; GammaY[45,26] := 0.261174213; GammaY[46,26] := 0.27775406; GammaY[47,26] := 0.295084251; GammaY[48,26] := 0.313192849; GammaY[49,26] := 0.332109522;
  GammaY[50,26] := 0.351865669; GammaY[51,26] := 0.372494582; GammaY[52,26] := 0.394031605; GammaY[53,26] := 0.416514311; GammaY[54,26] := 0.439982725; GammaY[55,26] := 0.46447954; GammaY[56,26] := 0.490050356; GammaY[57,26] := 0.516743974; GammaY[58,26] := 0.54461271; GammaY[59,26] := 0.573712742;
  GammaY[60,26] := 0.604104512; GammaY[61,26] := 0.635853171; GammaY[62,26] := 0.669029088; GammaY[63,26] := 0.703708427; GammaY[64,26] := 0.739973804; GammaY[65,26] := 0.777915016; GammaY[66,26] := 0.817629964; GammaY[67,26] := 0.859225615; GammaY[68,26] := 0.902819122; GammaY[69,26] := 0.948539212;
  GammaY[70,26] := 0.99652858; GammaY[71,26] := 1.046943264; GammaY[72,26] := 1.099956204; GammaY[73,26] := 1.155761867; GammaY[74,26] := 1.214577086; GammaY[75,26] := 1.276644652; GammaY[76,26] := 1.342238358; GammaY[77,26] := 1.411668823; GammaY[78,26] := 1.48528968; GammaY[79,26] := 1.563506115;
  GammaY[80,26] := 1.646785348; GammaY[81,26] := 1.735669894; GammaY[82,26] := 1.830794574; GammaY[83,26] := 1.932908589; GammaY[84,26] := 2.042904613; GammaY[85,26] := 2.161857722; GammaY[86,26] := 2.291078478; GammaY[87,26] := 2.432186796; GammaY[88,26] := 2.587217204; GammaY[89,26] := 2.758772988;
  GammaY[90,26] := 2.950259307; GammaY[91,26] := 3.166249235; GammaY[92,26] := 3.4130869; GammaY[93,26] := 3.699937252; GammaY[94,26] := 4.040754549; GammaY[95,26] := 4.458351429; GammaY[96,26] := 4.99404109; GammaY[97,26] := 5.735889043; GammaY[98,26] := 6.941780197; GammaY[99,26] := 9.834132943;
end;
procedure InitGammaY28;
begin
  GammaY[0,27] := 8.60029E-06; GammaY[1,27] := 8.62584E-05; GammaY[2,27] := 0.000265471; GammaY[3,27] := 0.000578699; GammaY[4,27] := 0.001046493; GammaY[5,27] := 0.001686038; GammaY[6,27] := 0.002512399; GammaY[7,27] := 0.003539171; GammaY[8,27] := 0.004778866; GammaY[9,27] := 0.006243174;
  GammaY[10,27] := 0.00794315; GammaY[11,27] := 0.009889346; GammaY[12,27] := 0.012091922; GammaY[13,27] := 0.014560732; GammaY[14,27] := 0.017305393; GammaY[15,27] := 0.020335347; GammaY[16,27] := 0.023659915; GammaY[17,27] := 0.027288338; GammaY[18,27] := 0.031229823; GammaY[19,27] := 0.035493582;
  GammaY[20,27] := 0.040088861; GammaY[21,27] := 0.04502498; GammaY[22,27] := 0.050311361; GammaY[23,27] := 0.05595756; GammaY[24,27] := 0.061973296; GammaY[25,27] := 0.068368483; GammaY[26,27] := 0.075153257; GammaY[27,27] := 0.082338006; GammaY[28,27] := 0.089933401; GammaY[29,27] := 0.097950428;
  GammaY[30,27] := 0.106400419; GammaY[31,27] := 0.115295083; GammaY[32,27] := 0.124646541; GammaY[33,27] := 0.134467365; GammaY[34,27] := 0.144770612; GammaY[35,27] := 0.155569864; GammaY[36,27] := 0.166879273; GammaY[37,27] := 0.178713605; GammaY[38,27] := 0.191088282; GammaY[39,27] := 0.204019442;
  GammaY[40,27] := 0.217523989; GammaY[41,27] := 0.231619653; GammaY[42,27] := 0.246325051; GammaY[43,27] := 0.261659762; GammaY[44,27] := 0.277644397; GammaY[45,27] := 0.294300681; GammaY[46,27] := 0.311651541; GammaY[47,27] := 0.329721206; GammaY[48,27] := 0.348535307; GammaY[49,27] := 0.368120994;
  GammaY[50,27] := 0.388507068; GammaY[51,27] := 0.40972412; GammaY[52,27] := 0.431804677; GammaY[53,27] := 0.454783379; GammaY[54,27] := 0.478697169; GammaY[55,27] := 0.503585503; GammaY[56,27] := 0.529490579; GammaY[57,27] := 0.556457605; GammaY[58,27] := 0.584535088; GammaY[59,27] := 0.613775165;
  GammaY[60,27] := 0.644233967; GammaY[61,27] := 0.675972046; GammaY[62,27] := 0.709054825; GammaY[63,27] := 0.743553178; GammaY[64,27] := 0.779544021; GammaY[65,27] := 0.817110986; GammaY[66,27] := 0.856345261; GammaY[67,27] := 0.897346507; GammaY[68,27] := 0.940223932; GammaY[69,27] := 0.985098394;
  GammaY[70,27] := 1.03210122; GammaY[71,27] := 1.081377801; GammaY[72,27] := 1.133091311; GammaY[73,27] := 1.187422374; GammaY[74,27] := 1.244572882; GammaY[75,27] := 1.304769614; GammaY[76,27] := 1.368267903; GammaY[77,27] := 1.435357075; GammaY[78,27] := 1.506367053; GammaY[79,27] := 1.581675447;
  GammaY[80,27] := 1.661717985; GammaY[81,27] := 1.747000517; GammaY[82,27] := 1.838114492; GammaY[83,27] := 1.935757996; GammaY[84,27] := 2.040762626; GammaY[85,27] := 2.154129678; GammaY[86,27] := 2.277079313; GammaY[87,27] := 2.411119679; GammaY[88,27] := 2.558144384; GammaY[89,27] := 2.720575307;
  GammaY[90,27] := 2.901579365; GammaY[91,27] := 3.105408739; GammaY[92,27] := 3.337959795; GammaY[93,27] := 3.60774691; GammaY[94,27] := 3.927728568; GammaY[95,27] := 4.319079165; GammaY[96,27] := 4.820123351; GammaY[97,27] := 5.512500216; GammaY[98,27] := 6.635057991; GammaY[99,27] := 9.316972354;
end;
procedure InitGammaY29;
begin
  GammaY[0,28] := 2.10606E-05; GammaY[1,28] := 0.000180951; GammaY[2,28] := 0.000513322; GammaY[3,28] := 0.001053033; GammaY[4,28] := 0.001816126; GammaY[5,28] := 0.002815283; GammaY[6,28] := 0.00406123; GammaY[7,28] := 0.005563411; GammaY[8,28] := 0.007330387; GammaY[9,28] := 0.009370082;
  GammaY[10,28] := 0.011689962; GammaY[11,28] := 0.014297157; GammaY[12,28] := 0.017198558; GammaY[13,28] := 0.020400893; GammaY[14,28] := 0.023910785; GammaY[15,28] := 0.027734807; GammaY[16,28] := 0.031879521; GammaY[17,28] := 0.03635152; GammaY[18,28] := 0.04115746; GammaY[19,28] := 0.046304089;
  GammaY[20,28] := 0.051798278; GammaY[21,28] := 0.057647049; GammaY[22,28] := 0.063857597; GammaY[23,28] := 0.070437318; GammaY[24,28] := 0.07739383; GammaY[25,28] := 0.084735004; GammaY[26,28] := 0.092468979; GammaY[27,28] := 0.100604196; GammaY[28,28] := 0.109149418; GammaY[29,28] := 0.118113758;
  GammaY[30,28] := 0.127506704; GammaY[31,28] := 0.137338152; GammaY[32,28] := 0.147618429; GammaY[33,28] := 0.158358333; GammaY[34,28] := 0.169569159; GammaY[35,28] := 0.181262734; GammaY[36,28] := 0.193451461; GammaY[37,28] := 0.206148354; GammaY[38,28] := 0.219367082; GammaY[39,28] := 0.233122014;
  GammaY[40,28] := 0.247428272; GammaY[41,28] := 0.262301784; GammaY[42,28] := 0.277759339; GammaY[43,28] := 0.293818653; GammaY[44,28] := 0.310498437; GammaY[45,28] := 0.327818472; GammaY[46,28] := 0.345799686; GammaY[47,28] := 0.364464253; GammaY[48,28] := 0.383835686; GammaY[49,28] := 0.403938936;
  GammaY[50,28] := 0.42480052; GammaY[51,28] := 0.446448646; GammaY[52,28] := 0.468913358; GammaY[53,28] := 0.492226695; GammaY[54,28] := 0.516422864; GammaY[55,28] := 0.541538439; GammaY[56,28] := 0.567612576; GammaY[57,28] := 0.594687259; GammaY[58,28] := 0.62280757; GammaY[59,28] := 0.652021985;
  GammaY[60,28] := 0.682382748; GammaY[61,28] := 0.713946245; GammaY[62,28] := 0.746773422; GammaY[63,28] := 0.780930314; GammaY[64,28] := 0.816488607; GammaY[65,28] := 0.853526291; GammaY[66,28] := 0.892128412; GammaY[67,28] := 0.932387935; GammaY[68,28] := 0.974406746; GammaY[69,28] := 1.018297468;
  GammaY[70,28] := 1.064182818; GammaY[71,28] := 1.112198608; GammaY[72,28] := 1.16249693; GammaY[73,28] := 1.215246337; GammaY[74,28] := 1.270635222; GammaY[75,28] := 1.328875027; GammaY[76,28] := 1.390204139; GammaY[77,28] := 1.454892628; GammaY[78,28] := 1.52324808; GammaY[79,28] := 1.595622675;
  GammaY[80,28] := 1.672422733; GammaY[81,28] := 1.754119898; GammaY[82,28] := 1.841265509; GammaY[83,28] := 1.934510025; GammaY[84,28] := 2.034628466; GammaY[85,28] := 2.14255378; GammaY[86,28] := 2.259422383; GammaY[87,28] := 2.386638221; GammaY[88,28] := 2.525963941; GammaY[89,28] := 2.679653594;
  GammaY[90,28] := 2.850653143; GammaY[91,28] := 3.042916963; GammaY[92,28] := 3.261926467; GammaY[93,28] := 3.515593544; GammaY[94,28] := 3.815954925; GammaY[95,28] := 4.182668915; GammaY[96,28] := 4.651295969; GammaY[97,28] := 5.297536587; GammaY[98,28] := 6.342663609; GammaY[99,28] := 8.829999756;
end;
procedure InitGammaY30;
begin
  GammaY[0,29] := 4.79744E-05; GammaY[1,29] := 0.000356418; GammaY[2,29] := 0.000937137; GammaY[3,29] := 0.001817413; GammaY[4,29] := 0.003000577; GammaY[5,29] := 0.004489403; GammaY[6,29] := 0.006286436; GammaY[7,29] := 0.008394144; GammaY[8,29] := 0.010814997; GammaY[9,29] := 0.013551529;
  GammaY[10,29] := 0.016606368; GammaY[11,29] := 0.019982264; GammaY[12,29] := 0.023682116; GammaY[13,29] := 0.027708981; GammaY[14,29] := 0.032066097; GammaY[15,29] := 0.036756895; GammaY[16,29] := 0.041785008; GammaY[17,29] := 0.047154287; GammaY[18,29] := 0.052868814; GammaY[19,29] := 0.058932909;
  GammaY[20,29] := 0.06535115; GammaY[21,29] := 0.072128377; GammaY[22,29] := 0.079269712; GammaY[23,29] := 0.086780568; GammaY[24,29] := 0.094666669; GammaY[25,29] := 0.102934056; GammaY[26,29] := 0.111589113; GammaY[27,29] := 0.120638578; GammaY[28,29] := 0.130089562; GammaY[29,29] := 0.139949571;
  GammaY[30,29] := 0.150226525; GammaY[31,29] := 0.16092878; GammaY[32,29] := 0.172065153; GammaY[33,29] := 0.183644946; GammaY[34,29] := 0.195677977; GammaY[35,29] := 0.208174608; GammaY[36,29] := 0.221145776; GammaY[37,29] := 0.234603033; GammaY[38,29] := 0.248558578; GammaY[39,29] := 0.263025301;
  GammaY[40,29] := 0.278016827; GammaY[41,29] := 0.293547567; GammaY[42,29] := 0.309632766; GammaY[43,29] := 0.32628856; GammaY[44,29] := 0.343532047; GammaY[45,29] := 0.361381352; GammaY[46,29] := 0.379855692; GammaY[47,29] := 0.398975469; GammaY[48,29] := 0.418762357; GammaY[49,29] := 0.439239401;
  GammaY[50,29] := 0.460431124; GammaY[51,29] := 0.482363651; GammaY[52,29] := 0.505064838; GammaY[53,29] := 0.528564422; GammaY[54,29] := 0.552894185; GammaY[55,29] := 0.578088132; GammaY[56,29] := 0.604182687; GammaY[57,29] := 0.631216946; GammaY[58,29] := 0.659232915; GammaY[59,29] := 0.688275776;
  GammaY[60,29] := 0.718394236; GammaY[61,29] := 0.749640875; GammaY[62,29] := 0.782072563; GammaY[63,29] := 0.815750926; GammaY[64,29] := 0.850742873; GammaY[65,29] := 0.887121213; GammaY[66,29] := 0.924965348; GammaY[67,29] := 0.964362081; GammaY[68,29] := 1.005407106; GammaY[69,29] := 1.048204396;
  GammaY[70,29] := 1.092868645; GammaY[71,29] := 1.139527861; GammaY[72,29] := 1.188323396; GammaY[73,29] := 1.239412472; GammaY[74,29] := 1.292971112; GammaY[75,29] := 1.349196843; GammaY[76,29] := 1.408312026; GammaY[77,29] := 1.470568702; GammaY[78,29] := 1.53625373; GammaY[79,29] := 1.605695971;
  GammaY[80,29] := 1.67927445; GammaY[81,29] := 1.757428588; GammaY[82,29] := 1.840672749; GammaY[83,29] := 1.929613603; GammaY[84,29] := 2.024973087; GammaY[85,29] := 2.127620503; GammaY[86,29] := 2.238614782; GammaY[87,29] := 2.359263336; GammaY[88,29] := 2.491207297; GammaY[89,29] := 2.636544706;
  GammaY[90,29] := 2.798016768; GammaY[91,29] := 2.979301779; GammaY[92,29] := 3.185496493; GammaY[93,29] := 3.423955081; GammaY[94,29] := 3.70586066; GammaY[95,29] := 4.049468245; GammaY[96,29] := 4.487782439; GammaY[97,29] := 5.091015771; GammaY[98,29] := 6.064214442; GammaY[99,29] := 8.371648332;
end;
procedure InitGammaY31;
begin
  GammaY[0,30] := 0.00010226; GammaY[1,30] := 0.000662607; GammaY[2,30] := 0.00162312; GammaY[3,30] := 0.002988499; GammaY[4,30] := 0.004739872; GammaY[5,30] := 0.00686473; GammaY[6,30] := 0.009354086; GammaY[7,30] := 0.012201248; GammaY[8,30] := 0.015401156; GammaY[9,30] := 0.01895;
  GammaY[10,30] := 0.022844969; GammaY[11,30] := 0.027084079; GammaY[12,30] := 0.031666058; GammaY[13,30] := 0.036590257; GammaY[14,30] := 0.041856589; GammaY[15,30] := 0.047465476; GammaY[16,30] := 0.053417819; GammaY[17,30] := 0.059714966; GammaY[18,30] := 0.066358692; GammaY[19,30] := 0.073351187;
  GammaY[20,30] := 0.080695042; GammaY[21,30] := 0.088393243; GammaY[22,30] := 0.096449169; GammaY[23,30] := 0.10486659; GammaY[24,30] := 0.113649667; GammaY[25,30] := 0.122802959; GammaY[26,30] := 0.132331426; GammaY[27,30] := 0.142240442; GammaY[28,30] := 0.1525358; GammaY[29,30] := 0.163223724;
  GammaY[30,30] := 0.174310887; GammaY[31,30] := 0.185804426; GammaY[32,30] := 0.197711958; GammaY[33,30] := 0.2100416; GammaY[34,30] := 0.222801995; GammaY[35,30] := 0.236002333; GammaY[36,30] := 0.249652381; GammaY[37,30] := 0.263762512; GammaY[38,30] := 0.278343739; GammaY[39,30] := 0.29340775;
  GammaY[40,30] := 0.308966943; GammaY[41,30] := 0.325034482; GammaY[42,30] := 0.341624339; GammaY[43,30] := 0.358751336; GammaY[44,30] := 0.376431215; GammaY[45,30] := 0.394680694; GammaY[46,30] := 0.413517542; GammaY[47,30] := 0.432960645; GammaY[48,30] := 0.453030097; GammaY[49,30] := 0.47374729;
  GammaY[50,30] := 0.495135012; GammaY[51,30] := 0.517217562; GammaY[52,30] := 0.54002087; GammaY[53,30] := 0.563572629; GammaY[54,30] := 0.587902466; GammaY[55,30] := 0.613042104; GammaY[56,30] := 0.639025528; GammaY[57,30] := 0.665889223; GammaY[58,30] := 0.693672398; GammaY[59,30] := 0.722417257;
  GammaY[60,30] := 0.752169295; GammaY[61,30] := 0.782977639; GammaY[62,30] := 0.814895425; GammaY[63,30] := 0.84798024; GammaY[64,30] := 0.882294616; GammaY[65,30] := 0.917906571; GammaY[66,30] := 0.954890327; GammaY[67,30] := 0.993327547; GammaY[68,30] := 1.033306607; GammaY[69,30] := 1.074924702;
  GammaY[70,30] := 1.118289894; GammaY[71,30] := 1.16352137; GammaY[72,30] := 1.210751332; GammaY[73,30] := 1.26012665; GammaY[74,30] := 1.311811453; GammaY[75,30] := 1.365990199; GammaY[76,30] := 1.422870789; GammaY[77,30] := 1.482688255; GammaY[78,30] := 1.545710624; GammaY[79,30] := 1.612244815;
  GammaY[80,30] := 1.682644077; GammaY[81,30] := 1.757318729; GammaY[82,30] := 1.836748406; GammaY[83,30] := 1.921498669; GammaY[84,30] := 2.01224294; GammaY[85,30] := 2.10979108; GammaY[86,30] := 2.21512979; GammaY[87,30] := 2.329477265; GammaY[88,30] := 2.45436176; GammaY[89,30] := 2.591736471;
  GammaY[90,30] := 2.744152967; GammaY[91,30] := 2.915033503; GammaY[92,30] := 3.109118461; GammaY[93,30] := 3.333245334; GammaY[94,30] := 3.597806589; GammaY[95,30] := 3.919759363; GammaY[96,30] := 4.32974437; GammaY[97,30] := 4.892902215; GammaY[98,30] := 5.799297464; GammaY[99,30] := 7.940407258;
end;
procedure InitGammaY32;
begin
  GammaY[0,31] := 0.000205062; GammaY[1,31] := 0.001168223; GammaY[2,31] := 0.002678974; GammaY[3,31] := 0.004701732; GammaY[4,31] := 0.007186964; GammaY[5,31] := 0.010103009; GammaY[6,31] := 0.013427447; GammaY[7,31] := 0.017143523; GammaY[8,31] := 0.021238312; GammaY[9,31] := 0.025701666;
  GammaY[10,31] := 0.030525543; GammaY[11,31] := 0.035703566; GammaY[12,31] := 0.041230714; GammaY[13,31] := 0.047103101; GammaY[14,31] := 0.053317807; GammaY[15,31] := 0.059872762; GammaY[16,31] := 0.066766644; GammaY[17,31] := 0.073998806; GammaY[18,31] := 0.081569222; GammaY[19,31] := 0.089478438;
  GammaY[20,31] := 0.097727539; GammaY[21,31] := 0.10631812; GammaY[22,31] := 0.115252265; GammaY[23,31] := 0.124532532; GammaY[24,31] := 0.134161942; GammaY[25,31] := 0.144143972; GammaY[26,31] := 0.154482551; GammaY[27,31] := 0.165182058; GammaY[28,31] := 0.176247328; GammaY[29,31] := 0.187683655;
  GammaY[30,31] := 0.1994968; GammaY[31,31] := 0.211693002; GammaY[32,31] := 0.224278991; GammaY[33,31] := 0.237262002; GammaY[34,31] := 0.250649792; GammaY[35,31] := 0.264450665; GammaY[36,31] := 0.278673488; GammaY[37,31] := 0.293327724; GammaY[38,31] := 0.308423459; GammaY[39,31] := 0.323971428;
  GammaY[40,31] := 0.339983058; GammaY[41,31] := 0.356470502; GammaY[42,31] := 0.373446686; GammaY[43,31] := 0.390925354; GammaY[44,31] := 0.408921122; GammaY[45,31] := 0.427449532; GammaY[46,31] := 0.44652712; GammaY[47,31] := 0.466171483; GammaY[48,31] := 0.486401355; GammaY[49,31] := 0.507236695;
  GammaY[50,31] := 0.52869877; GammaY[51,31] := 0.550810281; GammaY[52,31] := 0.573595465; GammaY[53,31] := 0.597080209; GammaY[54,31] := 0.62129221; GammaY[55,31] := 0.646261128; GammaY[56,31] := 0.672018757; GammaY[57,31] := 0.698599229; GammaY[58,31] := 0.726039228; GammaY[59,31] := 0.754378243;
  GammaY[60,31] := 0.78365884; GammaY[61,31] := 0.813926987; GammaY[62,31] := 0.845232384; GammaY[63,31] := 0.877628923; GammaY[64,31] := 0.911175121; GammaY[65,31] := 0.945934621; GammaY[66,31] := 0.981977295; GammaY[67,31] := 1.019378579; GammaY[68,31] := 1.05822103; GammaY[69,31] := 1.098596587;
  GammaY[70,31] := 1.140606074; GammaY[71,31] := 1.184360527; GammaY[72,31] := 1.229983502; GammaY[73,31] := 1.277612521; GammaY[74,31] := 1.327400823; GammaY[75,31] := 1.379520739; GammaY[76,31] := 1.434166341; GammaY[77,31] := 1.491557369; GammaY[78,31] := 1.551944102; GammaY[79,31] := 1.615612683;
  GammaY[80,31] := 1.682893188; GammaY[81,31] := 1.754168402; GammaY[82,31] := 1.829885782; GammaY[83,31] := 1.910572869; GammaY[84,31] := 1.996857517; GammaY[85,31] := 2.089494993; GammaY[86,31] := 2.189404843; GammaY[87,31] := 2.297722181; GammaY[88,31] := 2.415870658; GammaY[89,31] := 2.545669221;
  GammaY[90,31] := 2.689493348; GammaY[91,31] := 2.850528035; GammaY[92,31] := 3.033183476; GammaY[93,31] := 3.243818353; GammaY[94,31] := 3.492093631; GammaY[95,31] := 3.793765078; GammaY[96,31] := 4.177287245; GammaY[97,31] := 4.703113618; GammaY[98,31] := 5.54747523; GammaY[99,31] := 7.534820025;
end;
procedure InitGammaY33;
begin
  GammaY[0,32] := 0.000388737; GammaY[1,32] := 0.001961921; GammaY[2,32] := 0.004231054; GammaY[3,32] := 0.007104766; GammaY[4,32] := 0.010498477; GammaY[5,32] := 0.014360558; GammaY[6,32] := 0.018655535; GammaY[7,32] := 0.02335743; GammaY[8,32] := 0.02844647; GammaY[9,32] := 0.033907231;
  GammaY[10,32] := 0.039727497; GammaY[11,32] := 0.045897513; GammaY[12,32] := 0.052409475; GammaY[13,32] := 0.059257168; GammaY[14,32] := 0.066435699; GammaY[15,32] := 0.073941299; GammaY[16,32] := 0.081771171; GammaY[17,32] := 0.089923377; GammaY[18,32] := 0.098396737; GammaY[19,32] := 0.107190769;
  GammaY[20,32] := 0.116305622; GammaY[21,32] := 0.125742037; GammaY[22,32] := 0.135501307; GammaY[23,32] := 0.145585251; GammaY[24,32] := 0.155996196; GammaY[25,32] := 0.166736954; GammaY[26,32] := 0.177810818; GammaY[27,32] := 0.189221549; GammaY[28,32] := 0.200973379; GammaY[29,32] := 0.213071003;
  GammaY[30,32] := 0.225519592; GammaY[31,32] := 0.23832479; GammaY[32,32] := 0.251492727; GammaY[33,32] := 0.265030029; GammaY[34,32] := 0.278943835; GammaY[35,32] := 0.293241816; GammaY[36,32] := 0.307932183; GammaY[37,32] := 0.323023722; GammaY[38,32] := 0.338525814; GammaY[39,32] := 0.354448463;
  GammaY[40,32] := 0.370802332; GammaY[41,32] := 0.387598773; GammaY[42,32] := 0.404849872; GammaY[43,32] := 0.422568487; GammaY[44,32] := 0.440768302; GammaY[45,32] := 0.459463875; GammaY[46,32] := 0.4786707; GammaY[47,32] := 0.49840526; GammaY[48,32] := 0.518685125; GammaY[49,32] := 0.539529012;
  GammaY[50,32] := 0.560956866; GammaY[51,32] := 0.582989969; GammaY[52,32] := 0.605651042; GammaY[53,32] := 0.628964365; GammaY[54,32] := 0.65295591; GammaY[55,32] := 0.677653485; GammaY[56,32] := 0.703086901; GammaY[57,32] := 0.729288153; GammaY[58,32] := 0.756291628; GammaY[59,32] := 0.784134319;
  GammaY[60,32] := 0.812856126; GammaY[61,32] := 0.842500132; GammaY[62,32] := 0.873112903; GammaY[63,32] := 0.904744906; GammaY[64,32] := 0.937450927; GammaY[65,32] := 0.97129057; GammaY[66,32] := 1.006329137; GammaY[67,32] := 1.042637357; GammaY[68,32] := 1.080292736; GammaY[69,32] := 1.119380934;
  GammaY[70,32] := 1.159996259; GammaY[71,32] := 1.202242819; GammaY[72,32] := 1.246236037; GammaY[73,32] := 1.292104493; GammaY[74,32] := 1.339991434; GammaY[75,32] := 1.390057926; GammaY[76,32] := 1.442485312; GammaY[77,32] := 1.497478888; GammaY[78,32] := 1.555272308; GammaY[79,32] := 1.616133017;
  GammaY[80,32] := 1.680369078; GammaY[81,32] := 1.748337681; GammaY[82,32] := 1.820456634; GammaY[83,32] := 1.897218024; GammaY[84,32] := 1.979207513; GammaY[85,32] := 2.067129427; GammaY[86,32] := 2.161841086; GammaY[87,32] := 2.264400489; GammaY[88,32] := 2.376134219; GammaY[89,32] := 2.498736691;
  GammaY[90,32] := 2.634419984; GammaY[91,32] := 2.786149345; GammaY[92,32] := 2.958028897; GammaY[93,32] := 3.155973897; GammaY[94,32] := 3.38896787; GammaY[95,32] := 3.671654896; GammaY[96,32] := 4.030467697; GammaY[97,32] := 4.521527532; GammaY[98,32] := 5.308291671; GammaY[99,32] := 7.1534888;
end;
procedure InitGammaY34;
begin
  GammaY[0,33] := 0.000699745; GammaY[1,33] := 0.0031513; GammaY[2,33] := 0.006418742; GammaY[3,33] := 0.010348677; GammaY[4,33] := 0.01482429; GammaY[5,33] := 0.019777466; GammaY[6,33] := 0.02516285; GammaY[7,33] := 0.030948006; GammaY[8,33] := 0.0371087; GammaY[9,33] := 0.0436263;
  GammaY[10,33] := 0.050486213; GammaY[11,33] := 0.057676882; GammaY[12,33] := 0.065189108; GammaY[13,33] := 0.073015578; GammaY[14,33] := 0.08115052; GammaY[15,33] := 0.08958945; GammaY[16,33] := 0.09832898; GammaY[17,33] := 0.107366672; GammaY[18,33] := 0.116700925; GammaY[19,33] := 0.126330879;
  GammaY[20,33] := 0.136256353; GammaY[21,33] := 0.14647778; GammaY[22,33] := 0.156996171; GammaY[23,33] := 0.167813072; GammaY[24,33] := 0.178930544; GammaY[25,33] := 0.190351135; GammaY[26,33] := 0.202077873; GammaY[27,33] := 0.214114245; GammaY[28,33] := 0.226464202; GammaY[29,33] := 0.239132144;
  GammaY[30,33] := 0.25212293; GammaY[31,33] := 0.265441878; GammaY[32,33] := 0.279094773; GammaY[33,33] := 0.293087873; GammaY[34,33] := 0.30742792; GammaY[35,33] := 0.322122163; GammaY[36,33] := 0.337178366; GammaY[37,33] := 0.352604834; GammaY[38,33] := 0.368410434; GammaY[39,33] := 0.384604621;
  GammaY[40,33] := 0.401197467; GammaY[41,33] := 0.418199697; GammaY[42,33] := 0.435622721; GammaY[43,33] := 0.453478672; GammaY[44,33] := 0.471780463; GammaY[45,33] := 0.49054183; GammaY[46,33] := 0.509777377; GammaY[47,33] := 0.529502647; GammaY[48,33] := 0.549734186; GammaY[49,33] := 0.570489617;
  GammaY[50,33] := 0.59178772; GammaY[51,33] := 0.613648522; GammaY[52,33] := 0.636093398; GammaY[53,33] := 0.659145182; GammaY[54,33] := 0.68282829; GammaY[55,33] := 0.707168856; GammaY[56,33] := 0.732194872; GammaY[57,33] := 0.757936397; GammaY[58,33] := 0.78442573; GammaY[59,33] := 0.811697598;
  GammaY[60,33] := 0.839789431; GammaY[61,33] := 0.868741633; GammaY[62,33] := 0.898597885; GammaY[63,33] := 0.929405505; GammaY[64,33] := 0.961215849; GammaY[65,33] := 0.994085056; GammaY[66,33] := 1.02807376; GammaY[67,33] := 1.063248088; GammaY[68,33] := 1.099681272; GammaY[69,33] := 1.137453355;
  GammaY[70,33] := 1.176652497; GammaY[71,33] := 1.217376119; GammaY[72,33] := 1.259732218; GammaY[73,33] := 1.303840946; GammaY[74,33] := 1.349836493; GammaY[75,33] := 1.397869341; GammaY[76,33] := 1.448109004; GammaY[77,33] := 1.500747342; GammaY[78,33] := 1.556002571; GammaY[79,33] := 1.614124734;
  GammaY[80,33] := 1.675401438; GammaY[81,33] := 1.740166313; GammaY[82,33] := 1.808809178; GammaY[83,33] := 1.88178931; GammaY[84,33] := 1.959653358; GammaY[85,33] := 2.043058251; GammaY[86,33] := 2.132803531; GammaY[87,33] := 2.229875622; GammaY[88,33] := 2.335510935; GammaY[89,33] := 2.451288547;
  GammaY[90,33] := 2.579269086; GammaY[91,33] := 2.722213728; GammaY[92,33] := 2.883943128; GammaY[93,33] := 3.069961721; GammaY[94,33] := 3.288625924; GammaY[95,33] := 3.553550954; GammaY[96,33] := 3.889299438; GammaY[97,33] := 4.347988418; GammaY[98,33] := 5.081278354; GammaY[99,33] := 6.795067826;
end;
procedure InitGammaY35;
begin
  GammaY[0,34] := 0.001200866; GammaY[1,34] := 0.004859421; GammaY[2,34] := 0.009386611; GammaY[3,34] := 0.014578114; GammaY[4,34] := 0.020297394; GammaY[5,34] := 0.026468264; GammaY[6,34] := 0.03304154; GammaY[7,34] := 0.039982904; GammaY[8,34] := 0.047267231; GammaY[9,34] := 0.054875549;
  GammaY[10,34] := 0.062793236; GammaY[11,34] := 0.071008888; GammaY[12,34] := 0.07951356; GammaY[13,34] := 0.088300248; GammaY[14,34] := 0.097363514; GammaY[15,34] := 0.106699214; GammaY[16,34] := 0.116304293; GammaY[17,34] := 0.126176629; GammaY[18,34] := 0.136314912; GammaY[19,34] := 0.146718557;
  GammaY[20,34] := 0.157387621; GammaY[21,34] := 0.168322753; GammaY[22,34] := 0.179525147; GammaY[23,34] := 0.190996502; GammaY[24,34] := 0.202738999; GammaY[25,34] := 0.21475528; GammaY[26,34] := 0.227048425; GammaY[27,34] := 0.239621952; GammaY[28,34] := 0.252479805; GammaY[29,34] := 0.265626347;
  GammaY[30,34] := 0.279066365; GammaY[31,34] := 0.292805071; GammaY[32,34] := 0.306848111; GammaY[33,34] := 0.321201568; GammaY[34,34] := 0.335871978; GammaY[35,34] := 0.350866342; GammaY[36,34] := 0.366192143; GammaY[37,34] := 0.381857362; GammaY[38,34] := 0.397870506; GammaY[39,34] := 0.414240625;
  GammaY[40,34] := 0.430977342; GammaY[41,34] := 0.448090889; GammaY[42,34] := 0.465592143; GammaY[43,34] := 0.483492646; GammaY[44,34] := 0.501804666; GammaY[45,34] := 0.520541235; GammaY[46,34] := 0.539716201; GammaY[47,34] := 0.559344286; GammaY[48,34] := 0.579441147; GammaY[49,34] := 0.600023445;
  GammaY[50,34] := 0.621108922; GammaY[51,34] := 0.642716485; GammaY[52,34] := 0.6648663; GammaY[53,34] := 0.687579879; GammaY[54,34] := 0.710880238; GammaY[55,34] := 0.734791996; GammaY[56,34] := 0.759341504; GammaY[57,34] := 0.78455703; GammaY[58,34] := 0.810468932; GammaY[59,34] := 0.837109858;
  GammaY[60,34] := 0.864514975; GammaY[61,34] := 0.892722224; GammaY[62,34] := 0.921772614; GammaY[63,34] := 0.95171055; GammaY[64,34] := 0.982584185; GammaY[65,34] := 1.01444624; GammaY[66,34] := 1.04735343; GammaY[67,34] := 1.081367775; GammaY[68,34] := 1.11655804; GammaY[69,34] := 1.152999091;
  GammaY[70,34] := 1.190773495; GammaY[71,34] := 1.229972455; GammaY[72,34] := 1.270697064; GammaY[73,34] := 1.31305975; GammaY[74,34] := 1.357185958; GammaY[75,34] := 1.403216643; GammaY[76,34] := 1.451310209; GammaY[77,34] := 1.501645945; GammaY[78,34] := 1.554428089; GammaY[79,34] := 1.609890008;
  GammaY[80,34] := 1.668300513; GammaY[81,34] := 1.729971205; GammaY[82,34] := 1.795266021; GammaY[83,34] := 1.864614016; GammaY[84,34] := 1.938525135; GammaY[85,34] := 2.017612389; GammaY[86,34] := 2.102621839; GammaY[87,34] := 2.194473382; GammaY[88,34] := 2.294319838; GammaY[89,34] := 2.403633104;
  GammaY[90,34] := 2.524333349; GammaY[91,34] := 2.658992644; GammaY[92,34] := 2.811169016; GammaY[93,34] := 2.985986377; GammaY[94,34] := 3.191220105; GammaY[95,34] := 3.439533435; GammaY[96,34] := 3.75375908; GammaY[97,34] := 4.182312508; GammaY[98,34] := 4.865958659; GammaY[99,34] := 6.458269652;
end;
procedure InitGammaY36;
begin
  GammaY[0,35] := 0.001972153; GammaY[1,35] := 0.007219036; GammaY[2,35] := 0.013275382; GammaY[3,35] := 0.019921743; GammaY[4,35] := 0.027025325; GammaY[5,35] := 0.034515126; GammaY[6,35] := 0.042346719; GammaY[7,35] := 0.050489904; GammaY[8,35] := 0.058923092; GammaY[9,35] := 0.067630348;
  GammaY[10,35] := 0.076599676; GammaY[11,35] := 0.085821957; GammaY[12,35] := 0.095290243; GammaY[13,35] := 0.104999285; GammaY[14,35] := 0.114945194; GammaY[15,35] := 0.125125195; GammaY[16,35] := 0.135537448; GammaY[17,35] := 0.14618091; GammaY[18,35] := 0.157055232; GammaY[19,35] := 0.168160677;
  GammaY[20,35] := 0.179498061; GammaY[21,35] := 0.191068701; GammaY[22,35] := 0.202874377; GammaY[23,35] := 0.214917304; GammaY[24,35] := 0.227200113; GammaY[25,35] := 0.239725831; GammaY[26,35] := 0.252497866; GammaY[27,35] := 0.265520005; GammaY[28,35] := 0.278796408; GammaY[29,35] := 0.292331607;
  GammaY[30,35] := 0.306130508; GammaY[31,35] := 0.320198394; GammaY[32,35] := 0.334540935; GammaY[33,35] := 0.349164196; GammaY[34,35] := 0.364074647; GammaY[35,35] := 0.37927918; GammaY[36,35] := 0.39478512; GammaY[37,35] := 0.410600254; GammaY[38,35] := 0.426732846; GammaY[39,35] := 0.443191656;
  GammaY[40,35] := 0.459985973; GammaY[41,35] := 0.477125642; GammaY[42,35] := 0.4946211; GammaY[43,35] := 0.512483406; GammaY[44,35] := 0.530724287; GammaY[45,35] := 0.549356178; GammaY[46,35] := 0.56839227; GammaY[47,35] := 0.587846566; GammaY[48,35] := 0.607733939; GammaY[49,35] := 0.628070195;
  GammaY[50,35] := 0.648872133; GammaY[51,35] := 0.670157659; GammaY[52,35] := 0.691945853; GammaY[53,35] := 0.714257045; GammaY[54,35] := 0.737112952; GammaY[55,35] := 0.760536789; GammaY[56,35] := 0.784553406; GammaY[57,35] := 0.809189431; GammaY[58,35] := 0.834473447; GammaY[59,35] := 0.860436175;
  GammaY[60,35] := 0.887110688; GammaY[61,35] := 0.914532633; GammaY[62,35] := 0.942740553; GammaY[63,35] := 0.971776172; GammaY[64,35] := 1.001684893; GammaY[65,35] := 1.032515733; GammaY[66,35] := 1.064321952; GammaY[67,35] := 1.097162266; GammaY[68,35] := 1.131100677; GammaY[69,35] := 1.166207357;
  GammaY[70,35] := 1.202559861; GammaY[71,35] := 1.240243514; GammaY[72,35] := 1.279352885; GammaY[73,35] := 1.319993422; GammaY[74,35] := 1.36228252; GammaY[75,35] := 1.406351794; GammaY[76,35] := 1.452349706; GammaY[77,35] := 1.500443913; GammaY[78,35] := 1.550825129; GammaY[79,35] := 1.603711773;
  GammaY[80,35] := 1.659354953; GammaY[81,35] := 1.718045747; GammaY[82,35] := 1.780124297; GammaY[83,35] := 1.845991185; GammaY[84,35] := 1.916122546; GammaY[85,35] := 1.991090605; GammaY[86,35] := 2.071591568; GammaY[87,35] := 2.158483667; GammaY[88,35] := 2.252842144; GammaY[89,35] := 2.356039382;
  GammaY[90,35] := 2.469865412; GammaY[91,35] := 2.596717041; GammaY[92,35] := 2.739908532; GammaY[93,35] := 2.904211457; GammaY[94,35] := 3.096863293; GammaY[95,35] := 3.329645741; GammaY[96,35] := 3.623791342; GammaY[97,35] := 4.024293328; GammaY[98,35] := 4.661852074; GammaY[99,35] := 6.141861145;
end;
procedure InitGammaY37;
begin
  GammaY[0,36] := 0.00311012; GammaY[1,36] := 0.010365158; GammaY[2,36] := 0.018212874; GammaY[3,36] := 0.026484166; GammaY[4,36] := 0.035084094; GammaY[5,36] := 0.043964142; GammaY[6,36] := 0.053095068; GammaY[7,36] := 0.06245769; GammaY[8,36] := 0.072038814; GammaY[9,36] := 0.081829115;
  GammaY[10,36] := 0.091821943; GammaY[11,36] := 0.102012579; GammaY[12,36] := 0.11239776; GammaY[13,36] := 0.122975359; GammaY[14,36] := 0.133744157; GammaY[15,36] := 0.144703682; GammaY[16,36] := 0.155854094; GammaY[17,36] := 0.167196097; GammaY[18,36] := 0.17873087; GammaY[19,36] := 0.190460022;
  GammaY[20,36] := 0.202385549; GammaY[21,36] := 0.214509811; GammaY[22,36] := 0.226835508; GammaY[23,36] := 0.239365661; GammaY[24,36] := 0.252103609; GammaY[25,36] := 0.265052992; GammaY[26,36] := 0.278217755; GammaY[27,36] := 0.291602146; GammaY[28,36] := 0.305210711; GammaY[29,36] := 0.319048306;
  GammaY[30,36] := 0.333120098; GammaY[31,36] := 0.347431576; GammaY[32,36] := 0.361988557; GammaY[33,36] := 0.376797195; GammaY[34,36] := 0.391864007; GammaY[35,36] := 0.40719588; GammaY[36,36] := 0.422800082; GammaY[37,36] := 0.438684292; GammaY[38,36] := 0.454856616; GammaY[39,36] := 0.471325611;
  GammaY[40,36] := 0.488100313; GammaY[41,36] := 0.505190265; GammaY[42,36] := 0.522605548; GammaY[43,36] := 0.540356815; GammaY[44,36] := 0.558455332; GammaY[45,36] := 0.576913017; GammaY[46,36] := 0.595742477; GammaY[47,36] := 0.614957086; GammaY[48,36] := 0.634571022; GammaY[49,36] := 0.654599317;
  GammaY[50,36] := 0.675057941; GammaY[51,36] := 0.695963874; GammaY[52,36] := 0.717335186; GammaY[53,36] := 0.73919113; GammaY[54,36] := 0.761552241; GammaY[55,36] := 0.784440452; GammaY[56,36] := 0.80787922; GammaY[57,36] := 0.831893663; GammaY[58,36] := 0.856510702; GammaY[59,36] := 0.881759287;
  GammaY[60,36] := 0.907670569; GammaY[61,36] := 0.934278088; GammaY[62,36] := 0.961618076; GammaY[63,36] := 0.989729954; GammaY[64,36] := 1.018655943; GammaY[65,36] := 1.048441983; GammaY[66,36] := 1.079138698; GammaY[67,36] := 1.110800848; GammaY[68,36] := 1.143488385; GammaY[69,36] := 1.177267297;
  GammaY[70,36] := 1.212210168; GammaY[71,36] := 1.248396962; GammaY[72,36] := 1.285916279; GammaY[73,36] := 1.324866867; GammaY[74,36] := 1.365358639; GammaY[75,36] := 1.407514791; GammaY[76,36] := 1.451474179; GammaY[77,36] := 1.497393822; GammaY[78,36] := 1.545452088; GammaY[79,36] := 1.595852985;
  GammaY[80,36] := 1.648831452; GammaY[81,36] := 1.704659694; GammaY[82,36] := 1.763655379; GammaY[83,36] := 1.826192612; GammaY[84,36] := 1.892716261; GammaY[85,36] := 1.963760707; GammaY[86,36] := 2.039975786; GammaY[87,36] := 2.122162489; GammaY[88,36] := 2.211324062; GammaY[89,36] := 2.308740595;
  GammaY[90,36] := 2.416081273; GammaY[91,36] := 2.535580387; GammaY[92,36] := 2.670326438; GammaY[93,36] := 2.824764285; GammaY[94,36] := 3.005633311; GammaY[95,36] := 3.223899288; GammaY[96,36] := 3.499313915; GammaY[97,36] := 3.873706185; GammaY[98,36] := 4.468478085; GammaY[99,36] := 5.844662524;
end;
procedure InitGammaY38;
begin
  GammaY[0,37] := 0.004724949; GammaY[1,37] := 0.014426908; GammaY[2,37] := 0.024306094; GammaY[3,37] := 0.034340182; GammaY[4,37] := 0.044515037; GammaY[5,37] := 0.05482467; GammaY[6,37] := 0.065266405; GammaY[7,37] := 0.075839286; GammaY[8,37] := 0.08654339; GammaY[9,37] := 0.097379482;
  GammaY[10,37] := 0.10834882; GammaY[11,37] := 0.119453036; GammaY[12,37] := 0.130694068; GammaY[13,37] := 0.142074109; GammaY[14,37] := 0.153595577; GammaY[15,37] := 0.165261092; GammaY[16,37] := 0.177073466; GammaY[17,37] := 0.189035689; GammaY[18,37] := 0.20115093; GammaY[19,37] := 0.213422529;
  GammaY[20,37] := 0.225854; GammaY[21,37] := 0.23844903; GammaY[22,37] := 0.251211486; GammaY[23,37] := 0.264145414; GammaY[24,37] := 0.27725505; GammaY[25,37] := 0.290544823; GammaY[26,37] := 0.304019362; GammaY[27,37] := 0.317683508; GammaY[28,37] := 0.331542319; GammaY[29,37] := 0.345601083;
  GammaY[30,37] := 0.359865331; GammaY[31,37] := 0.37434085; GammaY[32,37] := 0.389033689; GammaY[33,37] := 0.403950181; GammaY[34,37] := 0.419096958; GammaY[35,37] := 0.434480967; GammaY[36,37] := 0.45010949; GammaY[37,37] := 0.465990164; GammaY[38,37] := 0.482131004; GammaY[39,37] := 0.498540425;
  GammaY[40,37] := 0.51522727; GammaY[41,37] := 0.532200839; GammaY[42,37] := 0.54947092; GammaY[43,37] := 0.567047811; GammaY[44,37] := 0.584942383; GammaY[45,37] := 0.603166109; GammaY[46,37] := 0.621731088; GammaY[47,37] := 0.640650114; GammaY[48,37] := 0.659936727; GammaY[49,37] := 0.679605267;
  GammaY[50,37] := 0.699670938; GammaY[51,37] := 0.72014988; GammaY[52,37] := 0.741059247; GammaY[53,37] := 0.76241729; GammaY[54,37] := 0.784243457; GammaY[55,37] := 0.80655848; GammaY[56,37] := 0.829384536; GammaY[57,37] := 0.85274536; GammaY[58,37] := 0.876666355; GammaY[59,37] := 0.901174801;
  GammaY[60,37] := 0.926300031; GammaY[61,37] := 0.952073636; GammaY[62,37] := 0.978529709; GammaY[63,37] := 1.005705226; GammaY[64,37] := 1.033639991; GammaY[65,37] := 1.062377262; GammaY[66,37] := 1.09196445; GammaY[67,37] := 1.122452982; GammaY[68,37] := 1.153899132; GammaY[69,37] := 1.186364783;
  GammaY[70,37] := 1.219917932; GammaY[71,37] := 1.254633692; GammaY[72,37] := 1.290595091; GammaY[73,37] := 1.327894422; GammaY[74,37] := 1.3666348; GammaY[75,37] := 1.406931659; GammaY[76,37] := 1.448914898; GammaY[77,37] := 1.492731462; GammaY[78,37] := 1.538548263; GammaY[79,37] := 1.58655615;
  GammaY[80,37] := 1.63697483; GammaY[81,37] := 1.69005875; GammaY[82,37] := 1.746104938; GammaY[83,37] := 1.805463127; GammaY[84,37] := 1.868548832; GammaY[85,37] := 1.935861168; GammaY[86,37] := 2.008007006; GammaY[87,37] := 2.085734226; GammaY[88,37] := 2.16997927; GammaY[89,37] := 2.261936542;
  GammaY[90,37] := 2.363163105; GammaY[91,37] := 2.475742543; GammaY[92,37] := 2.602554056; GammaY[93,37] := 2.747739771; GammaY[94,37] := 2.917577553; GammaY[95,37] := 3.122277786; GammaY[96,37] := 3.380221903; GammaY[97,37] := 3.730312366; GammaY[98,37] := 4.285359261; GammaY[99,37] := 5.565547672;
end;
procedure InitGammaY39;
begin
  GammaY[0,38] := 0.006935834; GammaY[1,38] := 0.019519665; GammaY[2,38] := 0.031635314; GammaY[3,38] := 0.043531768; GammaY[4,38] := 0.055324536; GammaY[5,38] := 0.067071352; GammaY[6,38] := 0.078807545; GammaY[7,38] := 0.090557328; GammaY[8,38] := 0.102338598; GammaY[9,38] := 0.114165345;
  GammaY[10,38] := 0.126048988; GammaY[11,38] := 0.137999182; GammaY[12,38] := 0.150024329; GammaY[13,38] := 0.162131925; GammaY[14,38] := 0.174328797; GammaY[15,38] := 0.186621277; GammaY[16,38] := 0.199015329; GammaY[17,38] := 0.211516641; GammaY[18,38] := 0.224130708; GammaY[19,38] := 0.236862886;
  GammaY[20,38] := 0.249718441; GammaY[21,38] := 0.262702593; GammaY[22,38] := 0.275820546; GammaY[23,38] := 0.289077522; GammaY[24,38] := 0.302478782; GammaY[25,38] := 0.316029655; GammaY[26,38] := 0.329735551; GammaY[27,38] := 0.343601996; GammaY[28,38] := 0.357634639; GammaY[29,38] := 0.371839273;
  GammaY[30,38] := 0.386221855; GammaY[31,38] := 0.400788525; GammaY[32,38] := 0.415545621; GammaY[33,38] := 0.430499705; GammaY[34,38] := 0.445657573; GammaY[35,38] := 0.461026284; GammaY[36,38] := 0.476613174; GammaY[37,38] := 0.492425883; GammaY[38,38] := 0.508472376; GammaY[39,38] := 0.524760963;
  GammaY[40,38] := 0.541300342; GammaY[41,38] := 0.558099621; GammaY[42,38] := 0.575168329; GammaY[43,38] := 0.592516474; GammaY[44,38] := 0.610154567; GammaY[45,38] := 0.628093668; GammaY[46,38] := 0.646345419; GammaY[47,38] := 0.664922098; GammaY[48,38] := 0.683836669; GammaY[49,38] := 0.70310283;
  GammaY[50,38] := 0.722735082; GammaY[51,38] := 0.742748792; GammaY[52,38] := 0.76316025; GammaY[53,38] := 0.783986793; GammaY[54,38] := 0.805246876; GammaY[55,38] := 0.826960133; GammaY[56,38] := 0.849147535; GammaY[57,38] := 0.871831496; GammaY[58,38] := 0.895036018; GammaY[59,38] := 0.918786839;
  GammaY[60,38] := 0.943111614; GammaY[61,38] := 0.968040109; GammaY[62,38] := 0.993604267; GammaY[63,38] := 1.019838948; GammaY[64,38] := 1.046781794; GammaY[65,38] := 1.074473487; GammaY[66,38] := 1.102958457; GammaY[67,38] := 1.132284989; GammaY[68,38] := 1.162505982; GammaY[69,38] := 1.193679377;
  GammaY[70,38] := 1.225869041; GammaY[71,38] := 1.25914542; GammaY[72,38] := 1.293586355; GammaY[73,38] := 1.329278412; GammaY[74,38] := 1.366318081; GammaY[75,38] := 1.404813562; GammaY[76,38] := 1.444886564; GammaY[77,38] := 1.486674545; GammaY[78,38] := 1.5303338; GammaY[79,38] := 1.576042926;
  GammaY[80,38] := 1.624007472; GammaY[81,38] := 1.674465489; GammaY[82,38] := 1.727694669; GammaY[83,38] := 1.78402193; GammaY[84,38] := 1.843835651; GammaY[85,38] := 1.90760233; GammaY[86,38] := 1.975888907; GammaY[87,38] := 2.049393816; GammaY[88,38] := 2.128991434; GammaY[89,38] := 2.215796695;
  GammaY[90,38] := 2.311262568; GammaY[91,38] := 2.417332918; GammaY[92,38] := 2.536692937; GammaY[93,38] := 2.673204083; GammaY[94,38] := 2.832716628; GammaY[95,38] := 3.024741376; GammaY[96,38] := 3.266391563; GammaY[97,38] := 3.593862711; GammaY[98,38] := 4.11202408; GammaY[99,38] := 5.303446881;
end;
procedure InitGammaY40;
begin
  GammaY[0,39] := 0.009864915; GammaY[1,39] := 0.025738436; GammaY[2,39] := 0.040250593; GammaY[3,39] := 0.054067725; GammaY[4,39] := 0.067486164; GammaY[5,39] := 0.08064814; GammaY[6,39] := 0.093637666; GammaY[7,39] := 0.106510353; GammaY[8,39] := 0.119305858; GammaY[9,39] := 0.132054031;
  GammaY[10,39] := 0.14477829; GammaY[11,39] := 0.157497633; GammaY[12,39] := 0.170227909; GammaY[13,39] := 0.182982654; GammaY[14,39] := 0.195773674; GammaY[15,39] := 0.208611453; GammaY[16,39] := 0.221505455; GammaY[17,39] := 0.234464347; GammaY[18,39] := 0.247496173; GammaY[19,39] := 0.260608489;
  GammaY[20,39] := 0.273808464; GammaY[21,39] := 0.287102975; GammaY[22,39] := 0.300498667; GammaY[23,39] := 0.31400202; GammaY[24,39] := 0.327619399; GammaY[25,39] := 0.341357095; GammaY[26,39] := 0.355221361; GammaY[27,39] := 0.369218449; GammaY[28,39] := 0.383354637; GammaY[29,39] := 0.397636262;
  GammaY[30,39] := 0.412069742; GammaY[31,39] := 0.426661605; GammaY[32,39] := 0.441418507; GammaY[33,39] := 0.456347263; GammaY[34,39] := 0.471454864; GammaY[35,39] := 0.486748497; GammaY[36,39] := 0.502235585; GammaY[37,39] := 0.517923802; GammaY[38,39] := 0.533821087; GammaY[39,39] := 0.549935684;
  GammaY[40,39] := 0.566276165; GammaY[41,39] := 0.582851459; GammaY[42,39] := 0.599670884; GammaY[43,39] := 0.616744179; GammaY[44,39] := 0.634081538; GammaY[45,39] := 0.651693648; GammaY[46,39] := 0.669591734; GammaY[47,39] := 0.687787597; GammaY[48,39] := 0.706293654; GammaY[49,39] := 0.725123025;
  GammaY[50,39] := 0.744289568; GammaY[51,39] := 0.763807922; GammaY[52,39] := 0.783693607; GammaY[53,39] := 0.803963088; GammaY[54,39] := 0.824633864; GammaY[55,39] := 0.845724558; GammaY[56,39] := 0.867255026; GammaY[57,39] := 0.88924647; GammaY[58,39] := 0.911721569; GammaY[59,39] := 0.934704626;
  GammaY[60,39] := 0.958221703; GammaY[61,39] := 0.982300882; GammaY[62,39] := 1.006972293; GammaY[63,39] := 1.032268733; GammaY[64,39] := 1.058225684; GammaY[65,39] := 1.084881266; GammaY[66,39] := 1.112277254; GammaY[67,39] := 1.140459169; GammaY[68,39] := 1.169476617; GammaY[69,39] := 1.199384059;
  GammaY[70,39] := 1.23024131; GammaY[71,39] := 1.262114473; GammaY[72,39] := 1.295076687; GammaY[73,39] := 1.329209049; GammaY[74,39] := 1.364602062; GammaY[75,39] := 1.401357003; GammaY[76,39] := 1.439587906; GammaY[77,39] := 1.479423632; GammaY[78,39] := 1.521010469; GammaY[79,39] := 1.564515658;
  GammaY[80,39] := 1.61013147; GammaY[81,39] := 1.658080656; GammaY[82,39] := 1.708623101; GammaY[83,39] := 1.762064494; GammaY[84,39] := 1.818768024; GammaY[85,39] := 1.87916972; GammaY[86,39] := 1.943799342; GammaY[87,39] := 2.01330956; GammaY[88,39] := 2.088517155; GammaY[89,39] := 2.170463236;
  GammaY[90,39] := 2.260504268; GammaY[91,39] := 2.360454341; GammaY[92,39] := 2.472818669; GammaY[93,39] := 2.601198676; GammaY[94,39] := 2.751048417; GammaY[95,39] := 2.931230628; GammaY[96,39] := 3.157684452; GammaY[97,39] := 3.464101102; GammaY[98,39] := 3.948009317; GammaY[99,39] := 5.057317355;
end;
procedure InitGammaY41;
begin
  GammaY[0,40] := 0.013630508; GammaY[1,40] := 0.033153083; GammaY[2,40] := 0.050170776; GammaY[3,40] := 0.065925594; GammaY[4,40] := 0.080944629; GammaY[5,40] := 0.095473572; GammaY[6,40] := 0.109654412; GammaY[7,40] := 0.123579348; GammaY[8,40] := 0.137312959; GammaY[9,40] := 0.150902993;
  GammaY[10,40] := 0.164386241; GammaY[11,40] := 0.177792001; GammaY[12,40] := 0.191144247; GammaY[13,40] := 0.204463052; GammaY[14,40] := 0.217765567; GammaY[15,40] := 0.231066701; GammaY[16,40] := 0.244379629; GammaY[17,40] := 0.257716153; GammaY[18,40] := 0.27108699; GammaY[19,40] := 0.284501988;
  GammaY[20,40] := 0.297970292; GammaY[21,40] := 0.31150049; GammaY[22,40] := 0.32510072; GammaY[23,40] := 0.338778755; GammaY[24,40] := 0.352542089; GammaY[25,40] := 0.366397995; GammaY[26,40] := 0.380353584; GammaY[27,40] := 0.394415851; GammaY[28,40] := 0.40859172; GammaY[29,40] := 0.422888081;
  GammaY[30,40] := 0.437311823; GammaY[31,40] := 0.451869869; GammaY[32,40] := 0.466569199; GammaY[33,40] := 0.481416892; GammaY[34,40] := 0.49642015; GammaY[35,40] := 0.511586308; GammaY[36,40] := 0.526922877; GammaY[37,40] := 0.542437568; GammaY[38,40] := 0.558138315; GammaY[39,40] := 0.574033306;
  GammaY[40,40] := 0.59013101; GammaY[41,40] := 0.606440205; GammaY[42,40] := 0.62297001; GammaY[43,40] := 0.639729919; GammaY[44,40] := 0.656729823; GammaY[45,40] := 0.673980079; GammaY[46,40] := 0.691491526; GammaY[47,40] := 0.709275519; GammaY[48,40] := 0.72734399; GammaY[49,40] := 0.745709501;
  GammaY[50,40] := 0.764385289; GammaY[51,40] := 0.783385333; GammaY[52,40] := 0.802724418; GammaY[53,40] := 0.822418206; GammaY[54,40] := 0.842483319; GammaY[55,40] := 0.862937423; GammaY[56,40] := 0.883799313; GammaY[57,40] := 0.90508907; GammaY[58,40] := 0.926828159; GammaY[59,40] := 0.949039524;
  GammaY[60,40] := 0.971747792; GammaY[61,40] := 0.994979321; GammaY[62,40] := 1.018762744; GammaY[63,40] := 1.043128872; GammaY[64,40] := 1.068110932; GammaY[65,40] := 1.093745072; GammaY[66,40] := 1.120070431; GammaY[67,40] := 1.147129741; GammaY[68,40] := 1.17496963; GammaY[69,40] := 1.20364132;
  GammaY[70,40] := 1.233201088; GammaY[71,40] := 1.263710848; GammaY[72,40] := 1.295239169; GammaY[73,40] := 1.327862189; GammaY[74,40] := 1.361664725; GammaY[75,40] := 1.396741831; GammaY[76,40] := 1.433200367; GammaY[77,40] := 1.471160968; GammaY[78,40] := 1.510760726; GammaY[79,40] := 1.552156216;
  GammaY[80,40] := 1.59552756; GammaY[81,40] := 1.641083258; GammaY[82,40] := 1.689066414; GammaY[83,40] := 1.739763093; GammaY[84,40] := 1.79351302; GammaY[85,40] := 1.850723915; GammaY[86,40] := 1.911891152; GammaY[87,40] := 1.977624779; GammaY[88,40] := 2.048687962; GammaY[89,40] := 2.126053354;
  GammaY[90,40] := 2.210987816; GammaY[91,40] := 2.30518509; GammaY[92,40] := 2.410983432; GammaY[93,40] := 2.531743075; GammaY[94,40] := 2.672550855; GammaY[95,40] := 2.841669179; GammaY[96,40] := 3.053950012; GammaY[97,40] := 3.340767044; GammaY[98,40] := 3.792861905; GammaY[99,40] := 4.826195143;
end;
procedure InitGammaY42;
begin
  GammaY[0,41] := 0.018340434; GammaY[1,41] := 0.041805697; GammaY[2,41] := 0.061384679; GammaY[3,41] := 0.07905526; GammaY[4,41] := 0.095620803; GammaY[5,41] := 0.111446596; GammaY[6,41] := 0.126740072; GammaY[7,41] := 0.141633999; GammaY[8,41] := 0.156220176; GammaY[9,41] := 0.170565665;
  GammaY[10,41] := 0.184721533; GammaY[11,41] := 0.198727971; GammaY[12,41] := 0.212617475; GammaY[13,41] := 0.226416925; GammaY[14,41] := 0.240148996; GammaY[15,41] := 0.25383315; GammaY[16,41] := 0.267486353; GammaY[17,41] := 0.281123594; GammaY[18,41] := 0.294758298; GammaY[19,41] := 0.308402625;
  GammaY[20,41] := 0.322067707; GammaY[21,41] := 0.335763843; GammaY[22,41] := 0.349500653; GammaY[23,41] := 0.363287199; GammaY[24,41] := 0.377132094; GammaY[25,41] := 0.391043585; GammaY[26,41] := 0.405029628; GammaY[27,41] := 0.419097953; GammaY[28,41] := 0.433256111; GammaY[29,41] := 0.447511538;
  GammaY[30,41] := 0.461871592; GammaY[31,41] := 0.476343579; GammaY[32,41] := 0.490934805; GammaY[33,41] := 0.505652607; GammaY[34,41] := 0.520504379; GammaY[35,41] := 0.535497605; GammaY[36,41] := 0.550639893; GammaY[37,41] := 0.565938996; GammaY[38,41] := 0.581402846; GammaY[39,41] := 0.59703958;
  GammaY[40,41] := 0.612857572; GammaY[41,41] := 0.628865449; GammaY[42,41] := 0.645072157; GammaY[43,41] := 0.661486968; GammaY[44,41] := 0.678119502; GammaY[45,41] := 0.694979788; GammaY[46,41] := 0.712078292; GammaY[47,41] := 0.72942596; GammaY[48,41] := 0.747034262; GammaY[49,41] := 0.764915241;
  GammaY[50,41] := 0.783081566; GammaY[51,41] := 0.801546589; GammaY[52,41] := 0.820324403; GammaY[53,41] := 0.839429901; GammaY[54,41] := 0.858878893; GammaY[55,41] := 0.878688171; GammaY[56,41] := 0.898875557; GammaY[57,41] := 0.919460055; GammaY[58,41] := 0.940461958; GammaY[59,41] := 0.961902968;
  GammaY[60,41] := 0.983806346; GammaY[61,41] := 1.006196946; GammaY[62,41] := 1.029101791; GammaY[63,41] := 1.052550001; GammaY[64,41] := 1.076572784; GammaY[65,41] := 1.101204049; GammaY[66,41] := 1.126480763; GammaY[67,41] := 1.15244317; GammaY[68,41] := 1.179135092; GammaY[69,41] := 1.206604596;
  GammaY[70,41] := 1.234904492; GammaY[71,41] := 1.264092932; GammaY[72,41] := 1.294234337; GammaY[73,41] := 1.325400201; GammaY[74,41] := 1.357670081; GammaY[75,41] := 1.391133067; GammaY[76,41] := 1.425889309; GammaY[77,41] := 1.462051891; GammaY[78,41] := 1.499749333; GammaY[79,41] := 1.539128413;
  GammaY[80,41] := 1.580357739; GammaY[81,41] := 1.623632511; GammaY[82,41] := 1.669180379; GammaY[83,41] := 1.717269057; GammaY[84,41] := 1.768216494; GammaY[85,41] := 1.822404204; GammaY[86,41] := 1.880295427; GammaY[87,41] := 1.942460576; GammaY[88,41] := 2.009613135; GammaY[89,41] := 2.082662141;
  GammaY[90,41] := 2.162791236; GammaY[91,41] := 2.25158252; GammaY[92,41] := 2.351219339; GammaY[93,41] := 2.464838181; GammaY[94,41] := 2.59718547; GammaY[95,41] := 2.755967315; GammaY[96,41] := 2.955028743; GammaY[97,41] := 3.223598312; GammaY[98,41] := 3.646140641; GammaY[99,41] := 4.609155241;
end;
procedure InitGammaY43;
begin
  GammaY[0,42] := 0.024086186; GammaY[1,42] := 0.051710081; GammaY[2,42] := 0.073853976; GammaY[3,42] := 0.09338359; GammaY[4,42] := 0.111417213; GammaY[5,42] := 0.128452366; GammaY[6,42] := 0.144767374; GammaY[7,42] := 0.160538266; GammaY[8,42] := 0.175885511; GammaY[9,42] := 0.19089629;
  GammaY[10,42] := 0.205636405; GammaY[11,42] := 0.2201572; GammaY[12,42] := 0.234499835; GammaY[13,42] := 0.248698065; GammaY[14,42] := 0.262780112; GammaY[15,42] := 0.276769985; GammaY[16,42] := 0.290688418; GammaY[17,42] := 0.304553559; GammaY[18,42] := 0.318381497; GammaY[19,42] := 0.332186658;
  GammaY[20,42] := 0.345982117; GammaY[21,42] := 0.359779842; GammaY[22,42] := 0.373590894; GammaY[23,42] := 0.387425584; GammaY[24,42] := 0.401293604; GammaY[25,42] := 0.415204134; GammaY[26,42] := 0.429165947; GammaY[27,42] := 0.443187479; GammaY[28,42] := 0.457276886; GammaY[29,42] := 0.47144212;
  GammaY[30,42] := 0.485690973; GammaY[31,42] := 0.500031124; GammaY[32,42] := 0.514470183; GammaY[33,42] := 0.529015729; GammaY[34,42] := 0.543675345; GammaY[35,42] := 0.558456652; GammaY[36,42] := 0.573367342; GammaY[37,42] := 0.5884152; GammaY[38,42] := 0.603608159; GammaY[39,42] := 0.618954315;
  GammaY[40,42] := 0.634461942; GammaY[41,42] := 0.650139544; GammaY[42,42] := 0.665995882; GammaY[43,42] := 0.68204; GammaY[44,42] := 0.698281269; GammaY[45,42] := 0.714729412; GammaY[46,42] := 0.731394549; GammaY[47,42] := 0.748287235; GammaY[48,42] := 0.765418502; GammaY[49,42] := 0.782799892;
  GammaY[50,42] := 0.800443545; GammaY[51,42] := 0.818362237; GammaY[52,42] := 0.836569403; GammaY[53,42] := 0.855079244; GammaY[54,42] := 0.873906785; GammaY[55,42] := 0.893067957; GammaY[56,42] := 0.912579682; GammaY[57,42] := 0.932459971; GammaY[58,42] := 0.95272803; GammaY[59,42] := 0.973404387;
  GammaY[60,42] := 0.994510931; GammaY[61,42] := 1.016071288; GammaY[62,42] := 1.038110971; GammaY[63,42] := 1.060657283; GammaY[64,42] := 1.0837397; GammaY[65,42] := 1.107390216; GammaY[66,42] := 1.131643563; GammaY[67,42] := 1.156537505; GammaY[68,42] := 1.182113374; GammaY[69,42] := 1.208416403;
  GammaY[70,42] := 1.235496162; GammaY[71,42] := 1.263407322; GammaY[72,42] := 1.292210327; GammaY[73,42] := 1.321972248; GammaY[74,42] := 1.352767761; GammaY[75,42] := 1.384680498; GammaY[76,42] := 1.417804432; GammaY[77,42] := 1.452245599; GammaY[78,42] := 1.488124436; GammaY[79,42] := 1.525578472;
  GammaY[80,42] := 1.564765744; GammaY[81,42] := 1.605869083; GammaY[82,42] := 1.649101743; GammaY[83,42] := 1.694714466; GammaY[84,42] := 1.743004822; GammaY[85,42] := 1.794329844; GammaY[86,42] := 1.849123034; GammaY[87,42] := 1.907917997; GammaY[88,42] := 1.971381987; GammaY[89,42] := 2.040364974;
  GammaY[90,42] := 2.115973389; GammaY[91,42] := 2.199685576; GammaY[92,42] := 2.293541108; GammaY[93,42] := 2.400468991; GammaY[94,42] := 2.52489993; GammaY[95,42] := 2.674024432; GammaY[96,42] := 2.86075467; GammaY[97,42] := 3.112333075; GammaY[98,42] := 3.507417482; GammaY[99,42] := 4.405323476;
end;
procedure InitGammaY44;
begin
  GammaY[0,43] := 0.030938488; GammaY[1,43] := 0.062853059; GammaY[2,43] := 0.087517194; GammaY[3,43] := 0.108819521; GammaY[4,43] := 0.128223461; GammaY[5,43] := 0.146367604; GammaY[6,43] := 0.163604564; GammaY[7,43] := 0.180155086; GammaY[8,43] := 0.196168942; GammaY[9,43] := 0.211753684;
  GammaY[10,43] := 0.226989919; GammaY[11,43] := 0.24194011; GammaY[12,43] := 0.256654006; GammaY[13,43] := 0.271172134; GammaY[14,43] := 0.285528158; GammaY[15,43] := 0.29975052; GammaY[16,43] := 0.31386361; GammaY[17,43] := 0.327888631; GammaY[18,43] := 0.34184424; GammaY[19,43] := 0.35574704;
  GammaY[20,43] := 0.369611963; GammaY[21,43] := 0.383452568; GammaY[22,43] := 0.397281285; GammaY[23,43] := 0.411109609; GammaY[24,43] := 0.424948254; GammaY[25,43] := 0.438807285; GammaY[26,43] := 0.452696236; GammaY[27,43] := 0.466624193; GammaY[28,43] := 0.480599884; GammaY[29,43] := 0.494631739;
  GammaY[30,43] := 0.508727959; GammaY[31,43] := 0.522896561; GammaY[32,43] := 0.537145435; GammaY[33,43] := 0.551482372; GammaY[34,43] := 0.565915131; GammaY[35,43] := 0.580451459; GammaY[36,43] := 0.595099117; GammaY[37,43] := 0.609865927; GammaY[38,43] := 0.624759803; GammaY[39,43] := 0.639788779;
  GammaY[40,43] := 0.654961043; GammaY[41,43] := 0.670284966; GammaY[42,43] := 0.685769135; GammaY[43,43] := 0.701422386; GammaY[44,43] := 0.717253838; GammaY[45,43] := 0.733272913; GammaY[46,43] := 0.749489408; GammaY[47,43] := 0.765913518; GammaY[48,43] := 0.782555849; GammaY[49,43] := 0.799427495;
  GammaY[50,43] := 0.816540078; GammaY[51,43] := 0.8339058; GammaY[52,43] := 0.851537496; GammaY[53,43] := 0.8694487; GammaY[54,43] := 0.88765371; GammaY[55,43] := 0.906167666; GammaY[56,43] := 0.925006608; GammaY[57,43] := 0.944187626; GammaY[58,43] := 0.963728931; GammaY[59,43] := 0.98364993;
  GammaY[60,43] := 1.003971344; GammaY[61,43] := 1.024715517; GammaY[62,43] := 1.045906404; GammaY[63,43] := 1.067569783; GammaY[64,43] := 1.089733548; GammaY[65,43] := 1.112427788; GammaY[66,43] := 1.135685216; GammaY[67,43] := 1.15954142; GammaY[68,43] := 1.184035234; GammaY[69,43] := 1.209209119;
  GammaY[70,43] := 1.235109767; GammaY[71,43] := 1.261788595; GammaY[72,43] := 1.289302342; GammaY[73,43] := 1.317713995; GammaY[74,43] := 1.347093704; GammaY[75,43] := 1.37751992; GammaY[76,43] := 1.409080752; GammaY[77,43] := 1.441875754; GammaY[78,43] := 1.476017914; GammaY[79,43] := 1.511636142;
  GammaY[80,43] := 1.548878582; GammaY[81,43] := 1.587916613; GammaY[82,43] := 1.62895001; GammaY[83,43] := 1.672213631; GammaY[84,43] := 1.717986169; GammaY[85,43] := 1.766601954; GammaY[86,43] := 1.818466816; GammaY[87,43] := 1.874080107; GammaY[88,43] := 1.934066161; GammaY[89,43] := 1.999220006;
  GammaY[90,43] := 2.070576288; GammaY[91,43] := 2.149517089; GammaY[92,43] := 2.237948525; GammaY[93,43] := 2.338607065; GammaY[94,43] := 2.455630605; GammaY[95,43] := 2.595731529; GammaY[96,43] := 2.770957537; GammaY[97,43] := 3.006711712; GammaY[98,43] := 3.376278561; GammaY[99,43] := 4.213874156;
end;
procedure InitGammaY45;
begin
  GammaY[0,44] := 0.038944543; GammaY[1,44] := 0.075197159; GammaY[2,44] := 0.102294289; GammaY[3,44] := 0.12525912; GammaY[4,44] := 0.145921212; GammaY[5,44] := 0.165065274; GammaY[6,44] := 0.183119649; GammaY[7,44] := 0.200350115; GammaY[8,44] := 0.216935663; GammaY[9,44] := 0.233003994;
  GammaY[10,44] := 0.248650243; GammaY[11,44] := 0.263947729; GammaY[12,44] := 0.278954526; GammaY[13,44] := 0.293717695; GammaY[14,44] := 0.308276125; GammaY[15,44] := 0.322662495; GammaY[16,44] := 0.336904683; GammaY[17,44] := 0.351026785; GammaY[18,44] := 0.365049882; GammaY[19,44] := 0.378992632;
  GammaY[20,44] := 0.392871711; GammaY[21,44] := 0.406702168; GammaY[22,44] := 0.420497708; GammaY[23,44] := 0.434270924; GammaY[24,44] := 0.448033475; GammaY[25,44] := 0.461796245; GammaY[26,44] := 0.475569465; GammaY[27,44] := 0.489362828; GammaY[28,44] := 0.503185575; GammaY[29,44] := 0.517046568;
  GammaY[30,44] := 0.530954379; GammaY[31,44] := 0.544917337; GammaY[32,44] := 0.558943573; GammaY[33,44] := 0.573041082; GammaY[34,44] := 0.587217762; GammaY[35,44] := 0.601481453; GammaY[36,44] := 0.615839979; GammaY[37,44] := 0.630301179; GammaY[38,44] := 0.644872943; GammaY[39,44] := 0.659563245;
  GammaY[40,44] := 0.674380174; GammaY[41,44] := 0.689331957; GammaY[42,44] := 0.704427018; GammaY[43,44] := 0.719673998; GammaY[44,44] := 0.735081763; GammaY[45,44] := 0.750659466; GammaY[46,44] := 0.76641658; GammaY[47,44] := 0.78236293; GammaY[48,44] := 0.798508733; GammaY[49,44] := 0.814864646;
  GammaY[50,44] := 0.831441809; GammaY[51,44] := 0.848251892; GammaY[52,44] := 0.865307153; GammaY[53,44] := 0.882620475; GammaY[54,44] := 0.900205485; GammaY[55,44] := 0.918076598; GammaY[56,44] := 0.936249046; GammaY[57,44] := 0.954739025; GammaY[58,44] := 0.973563772; GammaY[59,44] := 0.992741621;
  GammaY[60,44] := 1.012292233; GammaY[61,44] := 1.032236762; GammaY[62,44] := 1.052597812; GammaY[63,44] := 1.073399671; GammaY[64,44] := 1.094668604; GammaY[65,44] := 1.116433024; GammaY[66,44] := 1.138723746; GammaY[67,44] := 1.161574283; GammaY[68,44] := 1.185021188; GammaY[69,44] := 1.209104408;
  GammaY[70,44] := 1.233867857; GammaY[71,44] := 1.259359869; GammaY[72,44] := 1.285633772; GammaY[73,44] := 1.312748744; GammaY[74,44] := 1.340770671; GammaY[75,44] := 1.369773217; GammaY[76,44] := 1.399839125; GammaY[77,44] := 1.431061786; GammaY[78,44] := 1.46354715; GammaY[79,44] := 1.497416207;
  GammaY[80,44] := 1.532807913; GammaY[81,44] := 1.569882916; GammaY[82,44] := 1.608828479; GammaY[83,44] := 1.6498647; GammaY[84,44] := 1.693252721; GammaY[85,44] := 1.739305689; GammaY[86,44] := 1.788403637; GammaY[87,44] := 1.841014133; GammaY[88,44] := 1.897721563; GammaY[89,44] := 1.959269943;
  GammaY[90,44] := 2.026627256; GammaY[91,44] := 2.10108612; GammaY[92,44] := 2.184428764; GammaY[93,44] := 2.279212898; GammaY[94,44] := 2.389304696; GammaY[95,44] := 2.520973162; GammaY[96,44] := 2.685464686; GammaY[97,44] := 2.906478393; GammaY[98,44] := 3.252325002; GammaY[99,44] := 4.034029648;
end;
procedure InitGammaY46;
begin
  GammaY[0,45] := 0.048127012; GammaY[1,45] := 0.088684206; GammaY[2,45] := 0.11809132; GammaY[3,45] := 0.142590275; GammaY[4,45] := 0.164388546; GammaY[5,45] := 0.184418448; GammaY[6,45] := 0.203183741; GammaY[7,45] := 0.220994563; GammaY[8,45] := 0.238058431; GammaY[9,45] := 0.25452257;
  GammaY[10,45] := 0.270496104; GammaY[11,45] := 0.286062732; GammaY[12,45] := 0.301288457; GammaY[13,45] := 0.316226533; GammaY[14,45] := 0.330920775; GammaY[15,45] := 0.345407842; GammaY[16,45] := 0.359718874; GammaY[17,45] := 0.373880669; GammaY[18,45] := 0.387916565; GammaY[19,45] := 0.401847114;
  GammaY[20,45] := 0.415690601; GammaY[21,45] := 0.429463444; GammaY[22,45] := 0.443180523; GammaY[23,45] := 0.456855429; GammaY[24,45] := 0.470500686; GammaY[25,45] := 0.48412791; GammaY[26,45] := 0.497747972; GammaY[27,45] := 0.51137111; GammaY[28,45] := 0.52500702; GammaY[29,45] := 0.538664962;
  GammaY[30,45] := 0.552353826; GammaY[31,45] := 0.566082203; GammaY[32,45] := 0.579858442; GammaY[33,45] := 0.593690701; GammaY[34,45] := 0.607587; GammaY[35,45] := 0.621555256; GammaY[36,45] := 0.635603333; GammaY[37,45] := 0.64973906; GammaY[38,45] := 0.663970299; GammaY[39,45] := 0.678304964;
  GammaY[40,45] := 0.692751032; GammaY[41,45] := 0.707316603; GammaY[42,45] := 0.722009922; GammaY[43,45] := 0.736839415; GammaY[44,45] := 0.751813719; GammaY[45,45] := 0.766941715; GammaY[46,45] := 0.78223257; GammaY[47,45] := 0.797695764; GammaY[48,45] := 0.813341137; GammaY[49,45] := 0.829178907;
  GammaY[50,45] := 0.845219769; GammaY[51,45] := 0.861474906; GammaY[52,45] := 0.877956014; GammaY[53,45] := 0.894675401; GammaY[54,45] := 0.911646029; GammaY[55,45] := 0.928881588; GammaY[56,45] := 0.946396565; GammaY[57,45] := 0.964206327; GammaY[58,45] := 0.982327213; GammaY[59,45] := 1.00077656;
  GammaY[60,45] := 1.019573034; GammaY[61,45] := 1.038736613; GammaY[62,45] := 1.058288586; GammaY[63,45] := 1.078251932; GammaY[64,45] := 1.098651432; GammaY[65,45] := 1.119513878; GammaY[66,45] := 1.140868313; GammaY[67,45] := 1.162746308; GammaY[68,45] := 1.185182278; GammaY[69,45] := 1.208213857;
  GammaY[70,45] := 1.231882298; GammaY[71,45] := 1.256233059; GammaY[72,45] := 1.281316337; GammaY[73,45] := 1.307187726; GammaY[74,45] := 1.333909135; GammaY[75,45] := 1.361549761; GammaY[76,45] := 1.390187307; GammaY[77,45] := 1.419909455; GammaY[78,45] := 1.450815679; GammaY[79,45] := 1.483019487;
  GammaY[80,45] := 1.51665122; GammaY[81,45] := 1.551861594; GammaY[82,45] := 1.588826216; GammaY[83,45] := 1.62775142; GammaY[84,45] := 1.668882009; GammaY[85,45] := 1.712511458; GammaY[86,45] := 1.758995804; GammaY[87,45] := 1.808773015; GammaY[88,45] := 1.862390363; GammaY[89,45] := 1.920544396;
  GammaY[90,45] := 1.984141105; GammaY[91,45] := 2.054390028; GammaY[92,45] := 2.132958305; GammaY[93,45] := 2.222237689; GammaY[94,45] := 2.325842174; GammaY[95,45] := 2.449629421; GammaY[96,45] := 2.604102781; GammaY[97,45] := 2.811382297; GammaY[98,45] := 3.135173367; GammaY[99,45] := 3.865058185;
end;
procedure InitGammaY47;
begin
  GammaY[0,46] := 0.058484541; GammaY[1,46] := 0.103239351; GammaY[2,46] := 0.134804892; GammaY[3,46] := 0.160696809; GammaY[4,46] := 0.183503546; GammaY[5,46] := 0.204303346; GammaY[6,46] := 0.223673565; GammaY[7,46] := 0.241967203; GammaY[8,46] := 0.259419116; GammaY[9,46] := 0.276195105;
  GammaY[10,46] := 0.292417527; GammaY[11,46] := 0.308179851; GammaY[12,46] := 0.323555499; GammaY[13,46] := 0.338603496; GammaY[14,46] := 0.353372232; GammaY[15,46] := 0.367902051; GammaY[16,46] := 0.382227091; GammaY[17,46] := 0.396376627; GammaY[18,46] := 0.410376059; GammaY[19,46] := 0.424247671;
  GammaY[20,46] := 0.438011209; GammaY[21,46] := 0.45168433; GammaY[22,46] := 0.465282963; GammaY[23,46] := 0.478821607; GammaY[24,46] := 0.492313559; GammaY[25,46] := 0.505771096; GammaY[26,46] := 0.519205648; GammaY[27,46] := 0.532627923; GammaY[28,46] := 0.546048029; GammaY[29,46] := 0.55947556;
  GammaY[30,46] := 0.572919688; GammaY[31,46] := 0.586389228; GammaY[32,46] := 0.599892709; GammaY[33,46] := 0.613438414; GammaY[34,46] := 0.627034462; GammaY[35,46] := 0.640688834; GammaY[36,46] := 0.654409404; GammaY[37,46] := 0.668203998; GammaY[38,46] := 0.68208042; GammaY[39,46] := 0.696046495;
  GammaY[40,46] := 0.710110098; GammaY[41,46] := 0.724279184; GammaY[42,46] := 0.738561829; GammaY[43,46] := 0.752966254; GammaY[44,46] := 0.767500862; GammaY[45,46] := 0.782174255; GammaY[46,46] := 0.796995313; GammaY[47,46] := 0.8119732; GammaY[48,46] := 0.82711738; GammaY[49,46] := 0.842437687;
  GammaY[50,46] := 0.857944362; GammaY[51,46] := 0.873648096; GammaY[52,46] := 0.889560077; GammaY[53,46] := 0.905692046; GammaY[54,46] := 0.922056355; GammaY[55,46] := 0.938666026; GammaY[56,46] := 0.955534803; GammaY[57,46] := 0.972677291; GammaY[58,46] := 0.990109006; GammaY[59,46] := 1.00784646;
  GammaY[60,46] := 1.025907206; GammaY[61,46] := 1.044310036; GammaY[62,46] := 1.063075183; GammaY[63,46] := 1.082224352; GammaY[64,46] := 1.101780931; GammaY[65,46] := 1.121770194; GammaY[66,46] := 1.142219526; GammaY[67,46] := 1.163158676; GammaY[68,46] := 1.184620066; GammaY[69,46] := 1.206639154;
  GammaY[70,46] := 1.229254765; GammaY[71,46] := 1.252509618; GammaY[72,46] := 1.276450934; GammaY[73,46] := 1.301131014; GammaY[74,46] := 1.326608051; GammaY[75,46] := 1.352947074; GammaY[76,46] := 1.380221083; GammaY[77,46] := 1.408512434; GammaY[78,46] := 1.437914537; GammaY[79,46] := 1.468533952;
  GammaY[80,46] := 1.500493017; GammaY[81,46] := 1.533933176; GammaY[82,46] := 1.569019158; GammaY[83,46] := 1.605944496; GammaY[84,46] := 1.644938755; GammaY[85,46] := 1.686277063; GammaY[86,46] := 1.730293164; GammaY[87,46] := 1.77739748; GammaY[88,46] := 1.828102743; GammaY[89,46] := 1.883061373;
  GammaY[90,46] := 1.943121744; GammaY[91,46] := 2.009416153; GammaY[92,46] := 2.083504819; GammaY[93,46] := 2.167625341; GammaY[94,46] := 2.265157567; GammaY[95,46] := 2.381577439; GammaY[96,46] := 2.526699074; GammaY[97,46] := 2.721178697; GammaY[98,46] := 3.024456092; GammaY[99,46] := 3.706272124;
end;
procedure InitGammaY48;
begin
  GammaY[0,47] := 0.069993552; GammaY[1,47] := 0.118775177; GammaY[2,47] := 0.152326143; GammaY[3,47] := 0.179461918; GammaY[4,47] := 0.203147145; GammaY[5,47] := 0.224601617; GammaY[6,47] := 0.244473229; GammaY[7,47] := 0.26315568; GammaY[8,47] := 0.280909592; GammaY[9,47] := 0.297918162;
  GammaY[10,47] := 0.314316062; GammaY[11,47] := 0.330205811; GammaY[12,47] := 0.345667676; GammaY[13,47] := 0.360765978; GammaY[14,47] := 0.375553284; GammaY[15,47] := 0.39007329; GammaY[16,47] := 0.404362859; GammaY[17,47] := 0.418453505; GammaY[18,47] := 0.432372483; GammaY[19,47] := 0.446143631;
  GammaY[20,47] := 0.459788003; GammaY[21,47] := 0.473324359; GammaY[22,47] := 0.48676957; GammaY[23,47] := 0.500138928; GammaY[24,47] := 0.513446402; GammaY[25,47] := 0.526704849; GammaY[26,47] := 0.539926188; GammaY[27,47] := 0.553121541; GammaY[28,47] := 0.56630136; GammaY[29,47] := 0.57947552;
  GammaY[30,47] := 0.59265343; GammaY[31,47] := 0.6058441; GammaY[32,47] := 0.619056194; GammaY[33,47] := 0.632298109; GammaY[34,47] := 0.64557802; GammaY[35,47] := 0.658903935; GammaY[36,47] := 0.672283731; GammaY[37,47] := 0.685725201; GammaY[38,47] := 0.699236091; GammaY[39,47] := 0.712824134;
  GammaY[40,47] := 0.726497072; GammaY[41,47] := 0.740262726; GammaY[42,47] := 0.754129006; GammaY[43,47] := 0.76810392; GammaY[44,47] := 0.782195643; GammaY[45,47] := 0.796412531; GammaY[46,47] := 0.810763163; GammaY[47,47] := 0.825256371; GammaY[48,47] := 0.839901277; GammaY[49,47] := 0.854707333;
  GammaY[50,47] := 0.869684361; GammaY[51,47] := 0.884842595; GammaY[52,47] := 0.900192707; GammaY[53,47] := 0.915745919; GammaY[54,47] := 0.93151402; GammaY[55,47] := 0.94750939; GammaY[56,47] := 0.963745113; GammaY[57,47] := 0.980235041; GammaY[58,47] := 0.996993872; GammaY[59,47] := 1.014037256;
  GammaY[60,47] := 1.031381823; GammaY[61,47] := 1.049045367; GammaY[62,47] := 1.067047007; GammaY[63,47] := 1.08540725; GammaY[64,47] := 1.10414818; GammaY[65,47] := 1.12329365; GammaY[66,47] := 1.142869486; GammaY[67,47] := 1.16290374; GammaY[68,47] := 1.183426961; GammaY[69,47] := 1.204472527;
  GammaY[70,47] := 1.226077045; GammaY[71,47] := 1.248280725; GammaY[72,47] := 1.271127948; GammaY[73,47] := 1.29466794; GammaY[74,47] := 1.318955425; GammaY[75,47] := 1.344051539; GammaY[76,47] := 1.370024894; GammaY[77,47] := 1.396952876; GammaY[78,47] := 1.424923236; GammaY[79,47] := 1.454036056;
  GammaY[80,47] := 1.48440621; GammaY[81,47] := 1.516166466; GammaY[82,47] := 1.54947147; GammaY[83,47] := 1.584502826; GammaY[84,47] := 1.621475866; GammaY[85,47] := 1.660648668; GammaY[86,47] := 1.702334201; GammaY[87,47] := 1.746917248; GammaY[88,47] := 1.794878397; GammaY[89,47] := 1.846829018;
  GammaY[90,47] := 1.903563896; GammaY[91,47] := 1.966143513; GammaY[92,47] := 2.036028692; GammaY[93,47] := 2.115313839; GammaY[94,47] := 2.207161341; GammaY[95,47] := 2.316692725; GammaY[96,47] := 2.453082596; GammaY[97,47] := 2.635629737; GammaY[98,47] := 2.919821556; GammaY[99,47] := 3.55703228;
end;
procedure InitGammaY49;
begin
  GammaY[0,48] := 0.082610905; GammaY[1,48] := 0.135195588; GammaY[2,48] := 0.170544146; GammaY[3,48] := 0.198770917; GammaY[4,48] := 0.223205263; GammaY[5,48] := 0.245201941; GammaY[6,48] := 0.265475351; GammaY[7,48] := 0.284457237; GammaY[8,48] := 0.302432127; GammaY[9,48] := 0.31959926;
  GammaY[10,48] := 0.336104611; GammaY[11,48] := 0.352058951; GammaY[12,48] := 0.367548759; GammaY[13,48] := 0.382643148; GammaY[14,48] := 0.397398449; GammaY[15,48] := 0.411861363; GammaY[16,48] := 0.426071189; GammaY[17,48] := 0.440061436; GammaY[18,48] := 0.453861008; GammaY[19,48] := 0.467495113;
  GammaY[20,48] := 0.480985951; GammaY[21,48] := 0.494353253; GammaY[22,48] := 0.507614704; GammaY[23,48] := 0.520786289; GammaY[24,48] := 0.533882563; GammaY[25,48] := 0.546916871; GammaY[26,48] := 0.55990155; GammaY[27,48] := 0.57284808; GammaY[28,48] := 0.585767194; GammaY[29,48] := 0.598669012;
  GammaY[30,48] := 0.611563125; GammaY[31,48] := 0.624458685; GammaY[32,48] := 0.637364467; GammaY[33,48] := 0.65028894; GammaY[34,48] := 0.663240321; GammaY[35,48] := 0.676226625; GammaY[36,48] := 0.689255698; GammaY[37,48] := 0.702335294; GammaY[38,48] := 0.715473092; GammaY[39,48] := 0.728676716;
  GammaY[40,48] := 0.741953794; GammaY[41,48] := 0.755311985; GammaY[42,48] := 0.76875901; GammaY[43,48] := 0.782302688; GammaY[44,48] := 0.795950961; GammaY[45,48] := 0.809711935; GammaY[46,48] := 0.823593905; GammaY[47,48] := 0.837605379; GammaY[48,48] := 0.851755153; GammaY[49,48] := 0.866052324;
  GammaY[50,48] := 0.880506301; GammaY[51,48] := 0.895126882; GammaY[52,48] := 0.909924291; GammaY[53,48] := 0.924909222; GammaY[54,48] := 0.940092895; GammaY[55,48] := 0.955487109; GammaY[56,48] := 0.971104307; GammaY[57,48] := 0.986957646; GammaY[58,48] := 1.003061066; GammaY[59,48] := 1.019429378;
  GammaY[60,48] := 1.03607837; GammaY[61,48] := 1.053024909; GammaY[62,48] := 1.070287048; GammaY[63,48] := 1.087884167; GammaY[64,48] := 1.105837129; GammaY[65,48] := 1.124168451; GammaY[66,48] := 1.142902505; GammaY[67,48] := 1.162065766; GammaY[68,48] := 1.181687011; GammaY[69,48] := 1.201797675;
  GammaY[70,48] := 1.222432243; GammaY[71,48] := 1.2436286; GammaY[72,48] := 1.26542855; GammaY[73,48] := 1.287878389; GammaY[74,48] := 1.311029602; GammaY[75,48] := 1.334939689; GammaY[76,48] := 1.359673165; GammaY[77,48] := 1.385302777; GammaY[78,48] := 1.411910994; GammaY[79,48] := 1.439591855;
  GammaY[80,48] := 1.468453286; GammaY[81,48] := 1.498619965; GammaY[82,48] := 1.530237072; GammaY[83,48] := 1.563475129; GammaY[84,48] := 1.598536256; GammaY[85,48] := 1.635662612; GammaY[86,48] := 1.675147812; GammaY[87,48] := 1.717352768; GammaY[88,48] := 1.762728147; GammaY[89,48] := 1.811847112;
  GammaY[90,48] := 1.865454594; GammaY[91,48] := 1.924544281; GammaY[92,48] := 1.990484596; GammaY[93,48] := 2.06523681; GammaY[94,48] := 2.15176128; GammaY[95,48] := 2.254850423; GammaY[96,48] := 2.383085204; GammaY[97,48] := 2.554505143; GammaY[98,48] := 2.820934186; GammaY[99,48] := 3.416733007;
end;
procedure InitGammaY50;
begin
  GammaY[0,49] := 0.096277083; GammaY[1,49] := 0.152399311; GammaY[2,49] := 0.189348699; GammaY[3,49] := 0.218513343; GammaY[4,49] := 0.243570319; GammaY[5,49] := 0.266001048; GammaY[6,49] := 0.286581683; GammaY[7,49] := 0.305778997; GammaY[8,49] := 0.323899381; GammaY[9,49] := 0.34115665;
  GammaY[10,49] := 0.357706983; GammaY[11,49] := 0.373668578; GammaY[12,49] := 0.389133473; GammaY[13,49] := 0.404175049; GammaY[14,49] := 0.418852981; GammaY[15,49] := 0.433216626; GammaY[16,49] := 0.447307424; GammaY[17,49] := 0.461160629; GammaY[18,49] := 0.474806583; GammaY[19,49] := 0.488271687;
  GammaY[20,49] := 0.501579133; GammaY[21,49] := 0.51474948; GammaY[22,49] := 0.527801119; GammaY[23,49] := 0.540750625; GammaY[24,49] := 0.553613045; GammaY[25,49] := 0.566402142; GammaY[26,49] := 0.579130599; GammaY[27,49] := 0.591810171; GammaY[28,49] := 0.604451832; GammaY[29,49] := 0.617065889;
  GammaY[30,49] := 0.629662079; GammaY[31,49] := 0.642249661; GammaY[32,49] := 0.654837476; GammaY[33,49] := 0.667434044; GammaY[34,49] := 0.680047604; GammaY[35,49] := 0.692686148; GammaY[36,49] := 0.705357496; GammaY[37,49] := 0.71806933; GammaY[38,49] := 0.730829229; GammaY[39,49] := 0.743644717;
  GammaY[40,49] := 0.756523286; GammaY[41,49] := 0.76947244; GammaY[42,49] := 0.78249972; GammaY[43,49] := 0.795612728; GammaY[44,49] := 0.808819194; GammaY[45,49] := 0.822126984; GammaY[46,49] := 0.835544111; GammaY[47,49] := 0.849078796; GammaY[48,49] := 0.862739497; GammaY[49,49] := 0.876534943;
  GammaY[50,49] := 0.890474169; GammaY[51,49] := 0.90456656; GammaY[52,49] := 0.91882189; GammaY[53,49] := 0.933250369; GammaY[54,49] := 0.947862671; GammaY[55,49] := 0.962670049; GammaY[56,49] := 0.977684362; GammaY[57,49] := 0.992918091; GammaY[58,49] := 1.008384489; GammaY[59,49] := 1.024097582;
  GammaY[60,49] := 1.04007231; GammaY[61,49] := 1.05632466; GammaY[62,49] := 1.072871714; GammaY[63,49] := 1.089731796; GammaY[64,49] := 1.106924619; GammaY[65,49] := 1.124471452; GammaY[66,49] := 1.142395298; GammaY[67,49] := 1.160721118; GammaY[68,49] := 1.179476074; GammaY[69,49] := 1.198689819;
  GammaY[70,49] := 1.218394828; GammaY[71,49] := 1.238626791; GammaY[72,49] := 1.259425076; GammaY[73,49] := 1.280833283; GammaY[74,49] := 1.302899851; GammaY[75,49] := 1.32567887; GammaY[76,49] := 1.349231044; GammaY[77,49] := 1.373624787; GammaY[78,49] := 1.398937639; GammaY[79,49] := 1.425257998;
  GammaY[80,49] := 1.452687278; GammaY[81,49] := 1.481342643; GammaY[82,49] := 1.511360489; GammaY[83,49] := 1.542900951; GammaY[84,49] := 1.576153824; GammaY[85,49] := 1.611346444; GammaY[86,49] := 1.648754397; GammaY[87,49] := 1.688716358; GammaY[88,49] := 1.731655138; GammaY[89,49] := 1.778108323;
  GammaY[90,49] := 1.828774434; GammaY[91,49] := 1.884585024; GammaY[92,49] := 1.946822552; GammaY[93,49] := 2.017324534; GammaY[94,49] := 2.098863548; GammaY[95,49] := 2.195926227; GammaY[96,49] := 2.316542299; GammaY[97,49] := 2.477582653; GammaY[98,49] := 2.727474268; GammaY[99,49] := 3.284809312;
end;
procedure InitGammaY51;
begin
  GammaY[0,50] := 0.110919575; GammaY[1,50] := 0.170282912; GammaY[2,50] := 0.208632524; GammaY[3,50] := 0.238584474; GammaY[4,50] := 0.264142219; GammaY[5,50] := 0.286904289; GammaY[6,50] := 0.307703339; GammaY[7,50] := 0.327037924; GammaY[8,50] := 0.345234125; GammaY[9,50] := 0.362518811;
  GammaY[10,50] := 0.379057247; GammaY[11,50] := 0.394974206; GammaY[12,50] := 0.410366635; GammaY[13,50] := 0.425311652; GammaY[14,50] := 0.439871847; GammaY[15,50] := 0.454098889; GammaY[16,50] := 0.468036071; GammaY[17,50] := 0.481720149; GammaY[18,50] := 0.495182695; GammaY[19,50] := 0.508451128;
  GammaY[20,50] := 0.521549489; GammaY[21,50] := 0.53449904; GammaY[22,50] := 0.547318753; GammaY[23,50] := 0.560025687; GammaY[24,50] := 0.572635301; GammaY[25,50] := 0.585161697; GammaY[26,50] := 0.597617833; GammaY[27,50] := 0.610015694; GammaY[28,50] := 0.622366425; GammaY[29,50] := 0.634680479;
  GammaY[30,50] := 0.646967708; GammaY[31,50] := 0.659237434; GammaY[32,50] := 0.671498552; GammaY[33,50] := 0.683759589; GammaY[34,50] := 0.696028765; GammaY[35,50] := 0.708314046; GammaY[36,50] := 0.720623196; GammaY[37,50] := 0.732963816; GammaY[38,50] := 0.745343389; GammaY[39,50] := 0.757769301;
  GammaY[40,50] := 0.770248918; GammaY[41,50] := 0.782789593; GammaY[42,50] := 0.795398677; GammaY[43,50] := 0.808083582; GammaY[44,50] := 0.820851804; GammaY[45,50] := 0.833710951; GammaY[46,50] := 0.846668779; GammaY[47,50] := 0.859733219; GammaY[48,50] := 0.872912415; GammaY[49,50] := 0.886214736;
  GammaY[50,50] := 0.899648863; GammaY[51,50] := 0.9132238; GammaY[52,50] := 0.926948879; GammaY[53,50] := 0.94083385; GammaY[54,50] := 0.954888913; GammaY[55,50] := 0.969124769; GammaY[56,50] := 0.98355268; GammaY[57,50] := 0.998184485; GammaY[58,50] := 1.013032787; GammaY[59,50] := 1.028110952;
  GammaY[60,50] := 1.043433123; GammaY[61,50] := 1.059014416; GammaY[62,50] := 1.074871; GammaY[63,50] := 1.091020206; GammaY[64,50] := 1.107480672; GammaY[65,50] := 1.124272488; GammaY[66,50] := 1.141417379; GammaY[67,50] := 1.158938905; GammaY[68,50] := 1.176862692; GammaY[69,50] := 1.195216705;
  GammaY[70,50] := 1.21403156; GammaY[71,50] := 1.233340893; GammaY[72,50] := 1.253181792; GammaY[73,50] := 1.273595308; GammaY[74,50] := 1.294627066; GammaY[75,50] := 1.316327983; GammaY[76,50] := 1.338755183; GammaY[77,50] := 1.361973041; GammaY[78,50] := 1.386054485; GammaY[79,50] := 1.411082643;
  GammaY[80,50] := 1.437152868; GammaY[81,50] := 1.464375295; GammaY[82,50] := 1.492878109; GammaY[83,50] := 1.522811779; GammaY[84,50] := 1.554354598; GammaY[85,50] := 1.587720088; GammaY[86,50] := 1.623167031; GammaY[87,50] := 1.661013379; GammaY[88,50] := 1.701655977; GammaY[89,50] := 1.745599299;
  GammaY[90,50] := 1.793498667; GammaY[91,50] := 1.84622778; GammaY[92,50] := 1.90498912; GammaY[93,50] := 1.971505078; GammaY[94,50] := 2.048373622; GammaY[95,50] := 2.139797201; GammaY[96,50] := 2.253293467; GammaY[97,50] := 2.404648354; GammaY[98,50] := 2.639137735; GammaY[99,50] := 3.160733458;
end;
procedure InitGammaY52;
begin
  GammaY[0,51] := 0.126456206; GammaY[1,51] := 0.18874329; GammaY[2,51] := 0.228292903; GammaY[3,51] := 0.25888635; GammaY[4,51] := 0.284828924; GammaY[5,51] := 0.307825854; GammaY[6,51] := 0.328760726; GammaY[7,51] := 0.348160506; GammaY[8,51] := 0.366368751; GammaY[9,51] := 0.383623838;
  GammaY[10,51] := 0.400098999; GammaY[11,51] := 0.415924734; GammaY[12,51] := 0.431202234; GammaY[13,51] := 0.446011849; GammaY[14,51] := 0.460418668; GammaY[15,51] := 0.474476327; GammaY[16,51] := 0.488229694; GammaY[17,51] := 0.501716802; GammaY[18,51] := 0.514970256; GammaY[19,51] := 0.528018319;
  GammaY[20,51] := 0.540885726; GammaY[21,51] := 0.553594321; GammaY[22,51] := 0.566163551; GammaY[23,51] := 0.578610873; GammaY[24,51] := 0.59095206; GammaY[25,51] := 0.603201487; GammaY[26,51] := 0.615372332; GammaY[27,51] := 0.627476745; GammaY[28,51] := 0.639526011; GammaY[29,51] := 0.651530674;
  GammaY[30,51] := 0.663500642; GammaY[31,51] := 0.675445286; GammaY[32,51] := 0.687373511; GammaY[33,51] := 0.699293834; GammaY[34,51] := 0.711214432; GammaY[35,51] := 0.723143228; GammaY[36,51] := 0.735087921; GammaY[37,51] := 0.747056012; GammaY[38,51] := 0.759054873; GammaY[39,51] := 0.771091776;
  GammaY[40,51] := 0.783173929; GammaY[41,51] := 0.795308506; GammaY[42,51] := 0.807502687; GammaY[43,51] := 0.819763683; GammaY[44,51] := 0.832098767; GammaY[45,51] := 0.844515292; GammaY[46,51] := 0.857020765; GammaY[47,51] := 0.869622852; GammaY[48,51] := 0.882329377; GammaY[49,51] := 0.895148399;
  GammaY[50,51] := 0.908088232; GammaY[51,51] := 0.921157484; GammaY[52,51] := 0.934365093; GammaY[53,51] := 0.947720371; GammaY[54,51] := 0.961233045; GammaY[55,51] := 0.974913289; GammaY[56,51] := 0.988771836; GammaY[57,51] := 1.002819977; GammaY[58,51] := 1.017069652; GammaY[59,51] := 1.03153353;
  GammaY[60,51] := 1.046225032; GammaY[61,51] := 1.061158481; GammaY[62,51] := 1.076349184; GammaY[63,51] := 1.091813541; GammaY[64,51] := 1.107569175; GammaY[65,51] := 1.123635075; GammaY[66,51] := 1.140031765; GammaY[67,51] := 1.15678149; GammaY[68,51] := 1.173908436; GammaY[69,51] := 1.191438988;
  GammaY[70,51] := 1.20940201; GammaY[71,51] := 1.227829225; GammaY[72,51] := 1.246755599; GammaY[73,51] := 1.2662198; GammaY[74,51] := 1.286264804; GammaY[75,51] := 1.306938569; GammaY[76,51] := 1.328294865; GammaY[77,51] := 1.350394278; GammaY[78,51] := 1.373305443; GammaY[79,51] := 1.39710657;
  GammaY[80,51] := 1.421887351; GammaY[81,51] := 1.447751356; GammaY[82,51] := 1.474819113; GammaY[83,51] := 1.503232064; GammaY[84,51] := 1.533157779; GammaY[85,51] := 1.564796892; GammaY[86,51] := 1.598392517; GammaY[87,51] := 1.63424329; GammaY[88,51] := 1.672721858; GammaY[89,51] := 1.71430184;
  GammaY[90,51] := 1.759598324; GammaY[91,51] := 1.809431136; GammaY[92,51] := 1.864928312; GammaY[93,51] := 1.927705113; GammaY[94,51] := 2.000197097; GammaY[95,51] := 2.086342467; GammaY[96,51] := 2.193182982; GammaY[97,51] := 2.335496903; GammaY[98,51] := 2.555635871; GammaY[99,51] := 3.044008115;
end;
procedure InitGammaY53;
begin
  GammaY[0,52] := 0.142798243; GammaY[1,52] := 0.207679658; GammaY[2,52] := 0.248232854; GammaY[3,52] := 0.2793284; GammaY[4,52] := 0.30554668; GammaY[5,52] := 0.328688691; GammaY[6,52] := 0.349683243; GammaY[7,52] := 0.369082304; GammaY[8,52] := 0.38724469; GammaY[9,52] := 0.404418748;
  GammaY[10,52] := 0.420784568; GammaY[11,52] := 0.436477554; GammaY[12,52] := 0.451602504; GammaY[13,52] := 0.466242499; GammaY[14,52] := 0.480464732; GammaY[15,52] := 0.494324486; GammaY[16,52] := 0.507867932; GammaY[17,52] := 0.521134142; GammaY[18,52] := 0.534156571; GammaY[19,52] := 0.54696417;
  GammaY[20,52] := 0.55958223; GammaY[21,52] := 0.572033055; GammaY[22,52] := 0.584336478; GammaY[23,52] := 0.596510254; GammaY[24,52] := 0.60857041; GammaY[25,52] := 0.620531514; GammaY[26,52] := 0.632406893; GammaY[27,52] := 0.644208818; GammaY[28,52] := 0.655948661; GammaY[29,52] := 0.667637021;
  GammaY[30,52] := 0.67928383; GammaY[31,52] := 0.69089847; GammaY[32,52] := 0.702489844; GammaY[33,52] := 0.714066425; GammaY[34,52] := 0.725636349; GammaY[35,52] := 0.737207458; GammaY[36,52] := 0.748787354; GammaY[37,52] := 0.760383443; GammaY[38,52] := 0.772002977; GammaY[39,52] := 0.783653091;
  GammaY[40,52] := 0.795340828; GammaY[41,52] := 0.807073204; GammaY[42,52] := 0.818857227; GammaY[43,52] := 0.830699896; GammaY[44,52] := 0.842608265; GammaY[45,52] := 0.85458947; GammaY[46,52] := 0.866650748; GammaY[47,52] := 0.878799475; GammaY[48,52] := 0.891043196; GammaY[49,52] := 0.903389652;
  GammaY[50,52] := 0.915846821; GammaY[51,52] := 0.928422924; GammaY[52,52] := 0.941126528; GammaY[53,52] := 0.953966546; GammaY[54,52] := 0.966952241; GammaY[55,52] := 0.980093329; GammaY[56,52] := 0.993399983; GammaY[57,52] := 1.006882964; GammaY[58,52] := 1.020553633; GammaY[59,52] := 1.034423974;
  GammaY[60,52] := 1.048506736; GammaY[61,52] := 1.062815494; GammaY[62,52] := 1.077364745; GammaY[63,52] := 1.092170013; GammaY[64,52] := 1.107247969; GammaY[65,52] := 1.122616566; GammaY[66,52] := 1.138295198; GammaY[67,52] := 1.15430488; GammaY[68,52] := 1.170668437; GammaY[69,52] := 1.187410784;
  GammaY[70,52] := 1.204559174; GammaY[71,52] := 1.222143503; GammaY[72,52] := 1.240196732; GammaY[73,52] := 1.25875532; GammaY[74,52] := 1.277859765; GammaY[75,52] := 1.297555249; GammaY[76,52] := 1.317892416; GammaY[77,52] := 1.338928315; GammaY[78,52] := 1.360727564; GammaY[79,52] := 1.383363776;
  GammaY[80,52] := 1.406921351; GammaY[81,52] := 1.431497736; GammaY[82,52] := 1.4572063; GammaY[83,52] := 1.484180066; GammaY[84,52] := 1.512576594; GammaY[85,52] := 1.542584494; GammaY[86,52] := 1.574432264; GammaY[87,52] := 1.608400527; GammaY[88,52] := 1.644839378; GammaY[89,52] := 1.684193667;
  GammaY[90,52] := 1.727040984; GammaY[91,52] := 1.774150981; GammaY[92,52] := 1.826582366; GammaY[93,52] := 1.885850669; GammaY[94,52] := 1.954240308; GammaY[95,52] := 2.035443695; GammaY[96,52] := 2.136060142; GammaY[97,52] := 2.269931605; GammaY[98,52] := 2.476694886; GammaY[99,52] := 2.934171205;
end;
procedure InitGammaY54;
begin
  GammaY[0,53] := 0.159853162; GammaY[1,53] := 0.226995069; GammaY[2,53] := 0.2683619; GammaY[3,53] := 0.299827747; GammaY[4,53] := 0.326219988; GammaY[5,53] := 0.349424254; GammaY[6,53] := 0.370408859; GammaY[7,53] := 0.389747384; GammaY[8,53] := 0.407811722; GammaY[9,53] := 0.424858697;
  GammaY[10,53] := 0.441074187; GammaY[11,53] := 0.456597702; GammaY[12,53] := 0.471537044; GammaY[13,53] := 0.485977541; GammaY[14,53] := 0.499988104; GammaY[15,53] := 0.513625352; GammaY[16,53] := 0.526936504; GammaY[17,53] := 0.539961469; GammaY[18,53] := 0.552734375; GammaY[19,53] := 0.565284706;
  GammaY[20,53] := 0.577638192; GammaY[21,53] := 0.589817483; GammaY[22,53] := 0.601842684; GammaY[23,53] := 0.613731778; GammaY[24,53] := 0.625500969; GammaY[25,53] := 0.637164957; GammaY[26,53] := 0.648737161; GammaY[27,53] := 0.660229928; GammaY[28,53] := 0.671654682; GammaY[29,53] := 0.683022033;
  GammaY[30,53] := 0.69434192; GammaY[31,53] := 0.705623697; GammaY[32,53] := 0.716876217; GammaY[33,53] := 0.728107907; GammaY[34,53] := 0.73932683; GammaY[35,53] := 0.750540742; GammaY[36,53] := 0.761757131; GammaY[37,53] := 0.772983299; GammaY[38,53] := 0.784226379; GammaY[39,53] := 0.795493354;
  GammaY[40,53] := 0.806791122; GammaY[41,53] := 0.818126521; GammaY[42,53] := 0.829506357; GammaY[43,53] := 0.840937441; GammaY[44,53] := 0.852426614; GammaY[45,53] := 0.863980777; GammaY[46,53] := 0.875606903; GammaY[47,53] := 0.887312117; GammaY[48,53] := 0.899103694; GammaY[49,53] := 0.910989059;
  GammaY[50,53] := 0.922975862; GammaY[51,53] := 0.935071999; GammaY[52,53] := 0.947285648; GammaY[53,53] := 0.959625303; GammaY[54,53] := 0.97209982; GammaY[55,53] := 0.984718458; GammaY[56,53] := 0.997490888; GammaY[57,53] := 1.010427355; GammaY[58,53] := 1.023538668; GammaY[59,53] := 1.036836197;
  GammaY[60,53] := 1.050332034; GammaY[61,53] := 1.064039052; GammaY[62,53] := 1.077970984; GammaY[63,53] := 1.092142529; GammaY[64,53] := 1.106569462; GammaY[65,53] := 1.121268765; GammaY[66,53] := 1.136258773; GammaY[67,53] := 1.151559341; GammaY[68,53] := 1.167192041; GammaY[69,53] := 1.183180386;
  GammaY[70,53] := 1.199550088; GammaY[71,53] := 1.21632937; GammaY[72,53] := 1.233549319; GammaY[73,53] := 1.251244312; GammaY[74,53] := 1.269452533; GammaY[75,53] := 1.288216572; GammaY[76,53] := 1.307584143; GammaY[77,53] := 1.327608988; GammaY[78,53] := 1.348351965; GammaY[79,53] := 1.369882392;
  GammaY[80,53] := 1.392279728; GammaY[81,53] := 1.415635692; GammaY[82,53] := 1.440056973; GammaY[83,53] := 1.465668728; GammaY[84,53] := 1.492619172; GammaY[85,53] := 1.521085692; GammaY[86,53] := 1.551283147; GammaY[87,53] := 1.583475356; GammaY[88,53] := 1.617991379; GammaY[89,53] := 1.65524924;
  GammaY[90,53] := 1.695791569; GammaY[91,53] := 1.740341264; GammaY[92,53] := 1.789892442; GammaY[93,53] := 1.845867751; GammaY[94,53] := 1.910410905; GammaY[95,53] := 1.986985584; GammaY[96,53] := 2.081779564; GammaY[97,53] := 2.207764444; GammaY[98,53] := 2.402055486; GammaY[99,53] := 2.83078892;
end;
procedure InitGammaY55;
begin
  GammaY[0,54] := 0.177527023; GammaY[1,54] := 0.246597518; GammaY[2,54] := 0.288596521; GammaY[3,54] := 0.320309251; GammaY[4,54] := 0.346781402; GammaY[5,54] := 0.369972122; GammaY[6,54] := 0.390883582; GammaY[7,54] := 0.41010768; GammaY[8,54] := 0.428027279; GammaY[9,54] := 0.444906251;
  GammaY[10,54] := 0.460935229; GammaY[11,54] := 0.476257063; GammaY[12,54] := 0.490981981; GammaY[13,54] := 0.505197105; GammaY[14,54] := 0.518972708; GammaY[15,54] := 0.53236646; GammaY[16,54] := 0.545426393; GammaY[17,54] := 0.55819305; GammaY[18,54] := 0.570701058; GammaY[19,54] := 0.582980301;
  GammaY[20,54] := 0.595056822; GammaY[21,54] := 0.606953512; GammaY[22,54] := 0.618690672; GammaY[23,54] := 0.630286438; GammaY[24,54] := 0.641757118; GammaY[25,54] := 0.653117487; GammaY[26,54] := 0.664381023; GammaY[27,54] := 0.675560091; GammaY[28,54] := 0.686666108; GammaY[29,54] := 0.69770968;
  GammaY[30,54] := 0.708700711; GammaY[31,54] := 0.719648499; GammaY[32,54] := 0.730561843; GammaY[33,54] := 0.741449103; GammaY[34,54] := 0.752318246; GammaY[35,54] := 0.763176926; GammaY[36,54] := 0.774032533; GammaY[37,54] := 0.784892232; GammaY[38,54] := 0.795763009; GammaY[39,54] := 0.806651707;
  GammaY[40,54] := 0.817565064; GammaY[41,54] := 0.828509731; GammaY[42,54] := 0.839492341; GammaY[43,54] := 0.850519518; GammaY[44,54] := 0.861597878; GammaY[45,54] := 0.872734097; GammaY[46,54] := 0.883934923; GammaY[47,54] := 0.895207207; GammaY[48,54] := 0.906557936; GammaY[49,54] := 0.917994253;
  GammaY[50,54] := 0.929523498; GammaY[51,54] := 0.941153214; GammaY[52,54] := 0.952891238; GammaY[53,54] := 0.964745706; GammaY[54,54] := 0.976725052; GammaY[55,54] := 0.9888381; GammaY[56,54] := 1.001094084; GammaY[57,54] := 1.01350273; GammaY[58,54] := 1.026074287; GammaY[59,54] := 1.038819575;
  GammaY[60,54] := 1.051750072; GammaY[61,54] := 1.064877983; GammaY[62,54] := 1.078216325; GammaY[63,54] := 1.091779028; GammaY[64,54] := 1.105581015; GammaY[65,54] := 1.119638348; GammaY[66,54] := 1.133968374; GammaY[67,54] := 1.148589863; GammaY[68,54] := 1.163523197; GammaY[69,54] := 1.178790586;
  GammaY[70,54] := 1.19441631; GammaY[71,54] := 1.210427006; GammaY[72,54] := 1.226852012; GammaY[73,54] := 1.243723761; GammaY[74,54] := 1.261078261; GammaY[75,54] := 1.278955663; GammaY[76,54] := 1.297400951; GammaY[77,54] := 1.316464774; GammaY[78,54] := 1.336204476; GammaY[79,54] := 1.356685355;
  GammaY[80,54] := 1.37798225; GammaY[81,54] := 1.400181529; GammaY[82,54] := 1.423383637; GammaY[83,54] := 1.447706383; GammaY[84,54] := 1.473289253; GammaY[85,54] := 1.500299156; GammaY[86,54] := 1.528938217; GammaY[87,54] := 1.559454571; GammaY[88,54] := 1.59215765; GammaY[89,54] := 1.627440448;
  GammaY[90,54] := 1.665813002; GammaY[91,54] := 1.707954614; GammaY[92,54] := 1.754799198; GammaY[93,54] := 1.807682866; GammaY[94,54] := 1.868618275; GammaY[95,54] := 1.940856162; GammaY[96,54] := 2.030201352; GammaY[97,54] := 2.148816019; GammaY[98,54] := 2.33147238; GammaY[99,54] := 2.73345551;
end;
procedure InitGammaY56;
begin
  GammaY[0,55] := 0.195726439; GammaY[1,55] := 0.2664007; GammaY[2,55] := 0.308860333; GammaY[3,55] := 0.340705387; GammaY[4,55] := 0.367171191; GammaY[5,55] := 0.390279498; GammaY[6,55] := 0.411060867; GammaY[7,55] := 0.430122352; GammaY[8,55] := 0.447855763; GammaY[9,55] := 0.464530643;
  GammaY[10,55] := 0.48034141; GammaY[11,55] := 0.495433546; GammaY[12,55] := 0.509919158; GammaY[13,55] := 0.523886746; GammaY[14,55] := 0.537407605; GammaY[15,55] := 0.550540175; GammaY[16,55] := 0.563333082; GammaY[17,55] := 0.575827325; GammaY[18,55] := 0.58805787; GammaY[19,55] := 0.600054876;
  GammaY[20,55] := 0.611844596; GammaY[21,55] := 0.623450075; GammaY[22,55] := 0.634891718; GammaY[23,55] := 0.646187734; GammaY[24,55] := 0.65735448; GammaY[25,55] := 0.668406759; GammaY[26,55] := 0.679358049; GammaY[27,55] := 0.69022069; GammaY[28,55] := 0.701006077; GammaY[29,55] := 0.711724776;
  GammaY[30,55] := 0.722386627; GammaY[31,55] := 0.733000865; GammaY[32,55] := 0.7435762; GammaY[33,55] := 0.754120889; GammaY[34,55] := 0.764642804; GammaY[35,55] := 0.775149486; GammaY[36,55] := 0.785648188; GammaY[37,55] := 0.796145953; GammaY[38,55] := 0.806649634; GammaY[39,55] := 0.817165912;
  GammaY[40,55] := 0.827701361; GammaY[41,55] := 0.838262471; GammaY[42,55] := 0.848855677; GammaY[43,55] := 0.859487395; GammaY[44,55] := 0.870164042; GammaY[45,55] := 0.880892071; GammaY[46,55] := 0.891677979; GammaY[47,55] := 0.902528381; GammaY[48,55] := 0.913450011; GammaY[49,55] := 0.924449719;
  GammaY[50,55] := 0.935534544; GammaY[51,55] := 0.94671173; GammaY[52,55] := 0.957988759; GammaY[53,55] := 0.969373386; GammaY[54,55] := 0.980873675; GammaY[55,55] := 0.992498041; GammaY[56,55] := 1.004255259; GammaY[57,55] := 1.016154598; GammaY[58,55] := 1.028205824; GammaY[59,55] := 1.040419196;
  GammaY[60,55] := 1.052805611; GammaY[61,55] := 1.065376648; GammaY[62,55] := 1.078144647; GammaY[63,55] := 1.091122804; GammaY[64,55] := 1.104325254; GammaY[65,55] := 1.117767199; GammaY[66,55] := 1.131465049; GammaY[67,55] := 1.145436554; GammaY[68,55] := 1.159700981; GammaY[69,55] := 1.174279316;
  GammaY[70,55] := 1.189194494; GammaY[71,55] := 1.204471668; GammaY[72,55] := 1.220138533; GammaY[73,55] := 1.236225699; GammaY[74,55] := 1.252767139; GammaY[75,55] := 1.269800726; GammaY[76,55] := 1.287368881; GammaY[77,55] := 1.30551936; GammaY[78,55] := 1.324306216; GammaY[79,55] := 1.343790989;
  GammaY[80,55] := 1.364044196; GammaY[81,55] := 1.385147202; GammaY[82,55] := 1.407194615; GammaY[83,55] := 1.430297376; GammaY[84,55] := 1.454586815; GammaY[85,55] := 1.480220052; GammaY[86,55] := 1.507387318; GammaY[87,55] := 1.536322102; GammaY[88,55] := 1.567315513; GammaY[89,55] := 1.600737198;
  GammaY[90,55] := 1.637066773; GammaY[91,55] := 1.676942874; GammaY[92,55] := 1.721243283; GammaY[93,55] := 1.771223443; GammaY[94,55] := 1.828773898; GammaY[95,55] := 1.896947041; GammaY[96,55] := 1.981191185; GammaY[97,55] := 2.092915408; GammaY[98,55] := 2.264713743; GammaY[99,55] := 2.641791593;
end;
procedure InitGammaY57;
begin
  GammaY[0,56] := 0.21436016; GammaY[1,56] := 0.286324486; GammaY[2,56] := 0.329084097; GammaY[3,56] := 0.360955975; GammaY[4,56] := 0.387336901; GammaY[5,56] := 0.410300689; GammaY[6,56] := 0.430901039; GammaY[7,56] := 0.449757136; GammaY[8,56] := 0.467267828; GammaY[9,56] := 0.483707049;
  GammaY[10,56] := 0.4992721; GammaY[11,56] := 0.514110413; GammaY[12,56] := 0.528335466; GammaY[13,56] := 0.542036741; GammaY[14,56] := 0.555286242; GammaY[15,56] := 0.568142937; GammaY[16,56] := 0.580655845; GammaY[17,56] := 0.592866243; GammaY[18,56] := 0.604809315; GammaY[19,56] := 0.616515363;
  GammaY[20,56] := 0.628010737; GammaY[21,56] := 0.639318553; GammaY[22,56] := 0.650459248; GammaY[23,56] := 0.661451045; GammaY[24,56] := 0.672310304; GammaY[25,56] := 0.683051796; GammaY[26,56] := 0.693688959; GammaY[27,56] := 0.704234093; GammaY[28,56] := 0.714698519; GammaY[29,56] := 0.725092723;
  GammaY[30,56] := 0.735426468; GammaY[31,56] := 0.745708898; GammaY[32,56] := 0.75594861; GammaY[33,56] := 0.766153762; GammaY[34,56] := 0.776332119; GammaY[35,56] := 0.78649109; GammaY[36,56] := 0.796637808; GammaY[37,56] := 0.806779163; GammaY[38,56] := 0.816921851; GammaY[39,56] := 0.827072404;
  GammaY[40,56] := 0.83723723; GammaY[41,56] := 0.847422629; GammaY[42,56] := 0.857634865; GammaY[43,56] := 0.86788017; GammaY[44,56] := 0.878164744; GammaY[45,56] := 0.888494824; GammaY[46,56] := 0.898876691; GammaY[47,56] := 0.909316705; GammaY[48,56] := 0.919821329; GammaY[49,56] := 0.930397154;
  GammaY[50,56] := 0.94105093; GammaY[51,56] := 0.951789577; GammaY[52,56] := 0.962620268; GammaY[53,56] := 0.973550434; GammaY[54,56] := 0.984587755; GammaY[55,56] := 0.99574024; GammaY[56,56] := 1.007016284; GammaY[57,56] := 1.018424705; GammaY[58,56] := 1.029974755; GammaY[59,56] := 1.041676202;
  GammaY[60,56] := 1.053539392; GammaY[61,56] := 1.065575313; GammaY[62,56] := 1.077795666; GammaY[63,56] := 1.090212953; GammaY[64,56] := 1.10284057; GammaY[65,56] := 1.115692916; GammaY[66,56] := 1.128785516; GammaY[67,56] := 1.142135159; GammaY[68,56] := 1.155760067; GammaY[69,56] := 1.169680075;
  GammaY[70,56] := 1.183916856; GammaY[71,56] := 1.198494173; GammaY[72,56] := 1.213438179; GammaY[73,56] := 1.228777774; GammaY[74,56] := 1.244545024; GammaY[75,56] := 1.26077567; GammaY[76,56] := 1.277509732; GammaY[77,56] := 1.294792256; GammaY[78,56] := 1.312674214; GammaY[79,56] := 1.33121363;
  GammaY[80,56] := 1.350476976; GammaY[81,56] := 1.370540938; GammaY[82,56] := 1.391494663; GammaY[83,56] := 1.413442673; GammaY[84,56] := 1.436508672; GammaY[85,56] := 1.460840631; GammaY[86,56] := 1.48661767; GammaY[87,56] := 1.514059596; GammaY[88,56] := 1.543440403; GammaY[89,56] := 1.575107935;
  GammaY[90,56] := 1.60951342; GammaY[91,56] := 1.647257548; GammaY[92,56] := 1.689165732; GammaY[93,56] := 1.736418187; GammaY[94,56] := 1.790791616; GammaY[95,56] := 1.85515359; GammaY[96,56] := 1.934620365; GammaY[97,56] := 2.039900005; GammaY[98,56] := 2.201560678; GammaY[99,56] := 2.555441898;
end;
procedure InitGammaY58;
begin
  GammaY[0,57] := 0.233340292; GammaY[1,57] := 0.306295157; GammaY[2,57] := 0.349205552; GammaY[3,57] := 0.381007826; GammaY[4,57] := 0.407232901; GammaY[5,57] := 0.429996554; GammaY[6,57] := 0.450370659; GammaY[7,57] := 0.468983687; GammaY[8,57] := 0.486239752; GammaY[9,57] := 0.502415966;
  GammaY[10,57] := 0.517711653; GammaY[11,57] := 0.532275581; GammaY[12,57] := 0.546222131; GammaY[13,57] := 0.559641409; GammaY[14,57] := 0.57260585; GammaY[15,57] := 0.58517471; GammaY[16,57] := 0.597397204; GammaY[17,57] := 0.609314745; GammaY[18,57] := 0.620962589; GammaY[19,57] := 0.632371084;
  GammaY[20,57] := 0.643566597; GammaY[21,57] := 0.654572228; GammaY[22,57] := 0.665408391; GammaY[23,57] := 0.676093259; GammaY[24,57] := 0.686643127; GammaY[25,57] := 0.697072705; GammaY[26,57] := 0.707395352; GammaY[27,57] := 0.717623276; GammaY[28,57] := 0.727767712; GammaY[29,57] := 0.737839057;
  GammaY[30,57] := 0.747846962; GammaY[31,57] := 0.757800459; GammaY[32,57] := 0.767708041; GammaY[33,57] := 0.777577734; GammaY[34,57] := 0.787417166; GammaY[35,57] := 0.797233618; GammaY[36,57] := 0.807034085; GammaY[37,57] := 0.816825296; GammaY[38,57] := 0.826613803; GammaY[39,57] := 0.836405995;
  GammaY[40,57] := 0.846208098; GammaY[41,57] := 0.856026252; GammaY[42,57] := 0.865866525; GammaY[43,57] := 0.875734942; GammaY[44,57] := 0.885637514; GammaY[45,57] := 0.895580263; GammaY[46,57] := 0.905569232; GammaY[47,57] := 0.915610559; GammaY[48,57] := 0.925710475; GammaY[49,57] := 0.935875295;
  GammaY[50,57] := 0.946111494; GammaY[51,57] := 0.956425717; GammaY[52,57] := 0.966824813; GammaY[53,57] := 0.977315864; GammaY[54,57] := 0.987906218; GammaY[55,57] := 0.998603494; GammaY[56,57] := 1.009415711; GammaY[57,57] := 1.020351276; GammaY[58,57] := 1.03141897; GammaY[59,57] := 1.042628081;
  GammaY[60,57] := 1.053988437; GammaY[61,57] := 1.065510466; GammaY[62,57] := 1.07720527; GammaY[63,57] := 1.0890847; GammaY[64,57] := 1.101161451; GammaY[65,57] := 1.113449161; GammaY[66,57] := 1.125962527; GammaY[67,57] := 1.138717437; GammaY[68,57] := 1.151731128; GammaY[69,57] := 1.165022354;
  GammaY[70,57] := 1.178611607; GammaY[71,57] := 1.192521344; GammaY[72,57] := 1.206776272; GammaY[73,57] := 1.221403686; GammaY[74,57] := 1.236433866; GammaY[75,57] := 1.251900551; GammaY[76,57] := 1.267841514; GammaY[77,57] := 1.28429926; GammaY[78,57] := 1.301321878; GammaY[79,57] := 1.3189641;
  GammaY[80,57] := 1.337288611; GammaY[81,57] := 1.356367716; GammaY[82,57] := 1.376285456; GammaY[83,57] := 1.397140346; GammaY[84,57] := 1.419048959; GammaY[85,57] := 1.442150712; GammaY[86,57] := 1.466614338; GammaY[87,57] := 1.492646857; GammaY[88,57] := 1.520506274; GammaY[89,57] := 1.550520067;
  GammaY[90,57] := 1.583112948; GammaY[91,57] := 1.618850182; GammaY[92,57] := 1.658508308; GammaY[93,57] := 1.703197353; GammaY[94,57] := 1.754587845; GammaY[95,57] := 1.815375048; GammaY[96,57] := 1.890365795; GammaY[97,57] := 1.989615306; GammaY[98,57] := 2.141806649; GammaY[99,57] := 2.474073262;
end;
procedure InitGammaY59;
begin
  GammaY[0,58] := 0.252583212; GammaY[1,58] := 0.326245468; GammaY[2,58] := 0.36916917; GammaY[3,58] := 0.400814331; GammaY[4,58] := 0.426819854; GammaY[5,58] := 0.449333931; GammaY[6,58] := 0.469441954; GammaY[7,58] := 0.487779018; GammaY[8,58] := 0.504752821; GammaY[9,58] := 0.52064255;
  GammaY[10,58] := 0.535648779; GammaY[11,58] := 0.549921039; GammaY[12,58] := 0.563574183; GammaY[13,58] := 0.576698597; GammaY[14,58] := 0.589366884; GammaY[15,58] := 0.601638382; GammaY[16,58] := 0.613562337; GammaY[17,58] := 0.625180159; GammaY[18,58] := 0.636527069; GammaY[19,58] := 0.647633353;
  GammaY[20,58] := 0.658525303; GammaY[21,58] := 0.66922594; GammaY[22,58] := 0.679755588; GammaY[23,58] := 0.690132314; GammaY[24,58] := 0.700372318; GammaY[25,58] := 0.710490211; GammaY[26,58] := 0.720499237; GammaY[27,58] := 0.730411495; GammaY[28,58] := 0.7402381; GammaY[29,58] := 0.749989316;
  GammaY[30,58] := 0.759674679; GammaY[31,58] := 0.769303095; GammaY[32,58] := 0.778882913; GammaY[33,58] := 0.788422035; GammaY[34,58] := 0.797927962; GammaY[35,58] := 0.807407825; GammaY[36,58] := 0.816868469; GammaY[37,58] := 0.826316487; GammaY[38,58] := 0.835758265; GammaY[39,58] := 0.845200011;
  GammaY[40,58] := 0.854647798; GammaY[41,58] := 0.864107574; GammaY[42,58] := 0.873585237; GammaY[43,58] := 0.883086637; GammaY[44,58] := 0.892617573; GammaY[45,58] := 0.902183861; GammaY[46,58] := 0.911791343; GammaY[47,58] := 0.921445914; GammaY[48,58] := 0.93115355; GammaY[49,58] := 0.940920328;
  GammaY[50,58] := 0.950752438; GammaY[51,58] := 0.960656261; GammaY[52,58] := 0.970638369; GammaY[53,58] := 0.980705513; GammaY[54,58] := 0.990864709; GammaY[55,58] := 1.001123238; GammaY[56,58] := 1.01148873; GammaY[57,58] := 1.021969177; GammaY[58,58] := 1.032572944; GammaY[59,58] := 1.043308862;
  GammaY[60,58] := 1.054186268; GammaY[61,58] := 1.065215063; GammaY[62,58] := 1.076405782; GammaY[63,58] := 1.087769664; GammaY[64,58] := 1.099318745; GammaY[65,58] := 1.111065942; GammaY[66,58] := 1.123025174; GammaY[67,58] := 1.135211483; GammaY[68,58] := 1.14764118; GammaY[69,58] := 1.160332009;
  GammaY[70,58] := 1.173303345; GammaY[71,58] := 1.186576417; GammaY[72,58] := 1.200174578; GammaY[73,58] := 1.214123618; GammaY[74,58] := 1.228452141; GammaY[75,58] := 1.243192011; GammaY[76,58] := 1.258378892; GammaY[77,58] := 1.274052911; GammaY[78,58] := 1.290259454; GammaY[79,58] := 1.307050166;
  GammaY[80,58] := 1.324484191; GammaY[81,58] := 1.34262973; GammaY[82,58] := 1.361566045; GammaY[83,58] := 1.381386027; GammaY[84,58] := 1.402199581; GammaY[85,58] := 1.424138119; GammaY[86,58] := 1.44736066; GammaY[87,58] := 1.47206227; GammaY[88,58] := 1.498486008; GammaY[89,58] := 1.526940316;
  GammaY[90,58] := 1.557825137; GammaY[91,58] := 1.591672648; GammaY[92,58] := 1.62921375; GammaY[93,58] := 1.671492953; GammaY[94,58] := 1.720081714; GammaY[95,58] := 1.777514579; GammaY[96,58] := 1.848309913; GammaY[97,58] := 1.941914658; GammaY[98,58] := 2.08525692; GammaY[99,58] := 2.397375231;
end;
procedure InitGammaY60;
begin
  GammaY[0,59] := 0.272010196; GammaY[1,59] := 0.34611456; GammaY[2,59] := 0.388925814; GammaY[3,59] := 0.420335003; GammaY[4,59] := 0.446064223; GammaY[5,59] := 0.468285127; GammaY[6,59] := 0.488092256; GammaY[7,59] := 0.506124877; GammaY[8,59] := 0.522792749; GammaY[9,59] := 0.538376103;
  GammaY[10,59] := 0.553076033; GammaY[11,59] := 0.567042318; GammaY[12,59] := 0.580389879; GammaY[13,59] := 0.593209092; GammaY[14,59] := 0.605572496; GammaY[15,59] := 0.617539321; GammaY[16,59] := 0.629158687; GammaY[17,59] := 0.640471861; GammaY[18,59] := 0.651513927; GammaY[19,59] := 0.662315019;
  GammaY[20,59] := 0.672901288; GammaY[21,59] := 0.683295619; GammaY[22,59] := 0.693518184; GammaY[23,59] := 0.703586917; GammaY[24,59] := 0.713517869; GammaY[25,59] := 0.7233255; GammaY[26,59] := 0.733022921; GammaY[27,59] := 0.742622083; GammaY[28,59] := 0.752133966; GammaY[29,59] := 0.761568707;
  GammaY[30,59] := 0.770935693; GammaY[31,59] := 0.780243687; GammaY[32,59] := 0.789500908; GammaY[33,59] := 0.798715105; GammaY[34,59] := 0.807893619; GammaY[35,59] := 0.817043443; GammaY[36,59] := 0.826171256; GammaY[37,59] := 0.835283508; GammaY[38,59] := 0.844386434; GammaY[39,59] := 0.853486072;
  GammaY[40,59] := 0.862588322; GammaY[41,59] := 0.871698974; GammaY[42,59] := 0.880823734; GammaY[43,59] := 0.889968249; GammaY[44,59] := 0.899138137; GammaY[45,59] := 0.908338993; GammaY[46,59] := 0.917576462; GammaY[47,59] := 0.926856236; GammaY[48,59] := 0.936184045; GammaY[49,59] := 0.945565723;
  GammaY[50,59] := 0.955007224; GammaY[51,59] := 0.964514647; GammaY[52,59] := 0.974094259; GammaY[53,59] := 0.983752531; GammaY[54,59] := 0.993496137; GammaY[55,59] := 1.003332049; GammaY[56,59] := 1.013267557; GammaY[57,59] := 1.023310245; GammaY[58,59] := 1.03346808; GammaY[59,59] := 1.04374946;
  GammaY[60,59] := 1.054163257; GammaY[61,59] := 1.064718875; GammaY[62,59] := 1.075426313; GammaY[63,59] := 1.086296234; GammaY[64,59] := 1.097340047; GammaY[65,59] := 1.108569999; GammaY[66,59] := 1.119999275; GammaY[67,59] := 1.13164212; GammaY[68,59] := 1.143513973; GammaY[69,59] := 1.155631626;
  GammaY[70,59] := 1.168013407; GammaY[71,59] := 1.180679395; GammaY[72,59] := 1.193651669; GammaY[73,59] := 1.206954607; GammaY[74,59] := 1.220615238; GammaY[75,59] := 1.234663668; GammaY[76,59] := 1.249133585; GammaY[77,59] := 1.264062883; GammaY[78,59] := 1.279494417; GammaY[79,59] := 1.295476941;
  GammaY[80,59] := 1.312066276; GammaY[81,59] := 1.329326784; GammaY[82,59] := 1.347333248; GammaY[83,59] := 1.366173297; GammaY[84,59] := 1.38595059; GammaY[85,59] := 1.40678905; GammaY[86,59] := 1.42883861; GammaY[87,59] := 1.45228315; GammaY[88,59] := 1.47735175; GammaY[89,59] := 1.504335059;
  GammaY[90,59] := 1.533609866; GammaY[91,59] := 1.565677425; GammaY[92,59] := 1.601226006; GammaY[93,59] := 1.641238933; GammaY[94,59] := 1.687195174; GammaY[95,59] := 1.741479298; GammaY[96,59] := 1.808340604; GammaY[97,59] := 1.896659001; GammaY[98,59] := 2.031727978; GammaY[99,59] := 2.325056986;
end;
procedure InitGammaY61;
begin
  GammaY[0,60] := 0.291547841; GammaY[1,60] := 0.365847782; GammaY[2,60] := 0.408432362; GammaY[3,60] := 0.439535027; GammaY[4,60] := 0.464937764; GammaY[5,60] := 0.486827348; GammaY[6,60] := 0.506303462; GammaY[7,60] := 0.52400727; GammaY[8,60] := 0.540349174; GammaY[9,60] := 0.555609515;
  GammaY[10,60] := 0.569989262; GammaY[11,60] := 0.583637963; GammaY[12,60] := 0.596670264; GammaY[13,60] := 0.60917625; GammaY[14,60] := 0.621228163; GammaY[15,60] := 0.632884959; GammaY[16,60] := 0.644195496; GammaY[17,60] := 0.655200799; GammaY[18,60] := 0.665935712; GammaY[19,60] := 0.676430152;
  GammaY[20,60] := 0.686710054; GammaY[21,60] := 0.696798092; GammaY[22,60] := 0.706714245; GammaY[23,60] := 0.716476269; GammaY[24,60] := 0.726100044; GammaY[25,60] := 0.735599854; GammaY[26,60] := 0.744988642; GammaY[27,60] := 0.754278207; GammaY[28,60] := 0.763479362; GammaY[29,60] := 0.772602078;
  GammaY[30,60] := 0.781655593; GammaY[31,60] := 0.790648508; GammaY[32,60] := 0.799588896; GammaY[33,60] := 0.808484366; GammaY[34,60] := 0.817342094; GammaY[35,60] := 0.826168919; GammaY[36,60] := 0.834971376; GammaY[37,60] := 0.843755746; GammaY[38,60] := 0.85252809; GammaY[39,60] := 0.861294291;
  GammaY[40,60] := 0.870060066; GammaY[41,60] := 0.878831044; GammaY[42,60] := 0.887612766; GammaY[43,60] := 0.896410685; GammaY[44,60] := 0.905230226; GammaY[45,60] := 0.914076805; GammaY[46,60] := 0.922955847; GammaY[47,60] := 0.931872815; GammaY[48,60] := 0.940833228; GammaY[49,60] := 0.949842668;
  GammaY[50,60] := 0.958906861; GammaY[51,60] := 0.968031666; GammaY[52,60] := 0.977223066; GammaY[53,60] := 0.986487243; GammaY[54,60] := 0.995830576; GammaY[55,60] := 1.00525972; GammaY[56,60] := 1.014781616; GammaY[57,60] := 1.024403485; GammaY[58,60] := 1.034132915; GammaY[59,60] := 1.043977893;
  GammaY[60,60] := 1.053946854; GammaY[61,60] := 1.064048731; GammaY[62,60] := 1.074293017; GammaY[63,60] := 1.084689831; GammaY[64,60] := 1.095249994; GammaY[65,60] := 1.105985117; GammaY[66,60] := 1.116907695; GammaY[67,60] := 1.128031223; GammaY[68,60] := 1.139370321; GammaY[69,60] := 1.150940885;
  GammaY[70,60] := 1.162760261; GammaY[71,60] := 1.174847443; GammaY[72,60] := 1.187223316; GammaY[73,60] := 1.199910931; GammaY[74,60] := 1.21293584; GammaY[75,60] := 1.226326499; GammaY[76,60] := 1.240114741; GammaY[77,60] := 1.254336367; GammaY[78,60] := 1.269031857; GammaY[79,60] := 1.284247254;
  GammaY[80,60] := 1.300035267; GammaY[81,60] := 1.316456658; GammaY[82,60] := 1.333582015; GammaY[83,60] := 1.351494037; GammaY[84,60] := 1.370290535; GammaY[85,60] := 1.390088421; GammaY[86,60] := 1.411029118; GammaY[87,60] := 1.433286042; GammaY[88,60] := 1.457075196; GammaY[89,60] := 1.48267058;
  GammaY[90,60] := 1.510427331; GammaY[91,60] := 1.540817796; GammaY[92,60] := 1.574490398; GammaY[93,60] := 1.612371293; GammaY[94,60] := 1.655853063; GammaY[95,60] := 1.707180253; GammaY[96,60] := 1.770351071; GammaY[97,60] := 1.85371658; GammaY[98,60] := 1.981046984; GammaY[99,60] := 2.256846699;
end;
procedure InitGammaY62;
begin
  GammaY[0,61] := 0.311128291; GammaY[1,61] := 0.385396429; GammaY[2,61] := 0.427651314; GammaY[3,61] := 0.458384792; GammaY[4,61] := 0.483417038; GammaY[5,61] := 0.504942251; GammaY[6,61] := 0.524061548; GammaY[7,61] := 0.541415924; GammaY[8,61] := 0.557415151; GammaY[9,61] := 0.572338832;
  GammaY[10,61] := 0.586387212; GammaY[11,61] := 0.599709166; GammaY[12,61] := 0.612418749; GammaY[13,61] := 0.624605514; GammaY[14,61] := 0.636341222; GammaY[15,61] := 0.647684405; GammaY[16,61] := 0.658683528; GammaY[17,61] := 0.669379261; GammaY[18,61] := 0.679806124; GammaY[19,61] := 0.689993748;
  GammaY[20,61] := 0.699967803; GammaY[21,61] := 0.709750708; GammaY[22,61] := 0.719362214; GammaY[23,61] := 0.72881985; GammaY[24,61] := 0.73813928; GammaY[25,61] := 0.747334597; GammaY[26,61] := 0.756418545; GammaY[27,61] := 0.765402747; GammaY[28,61] := 0.774297851; GammaY[29,61] := 0.783113647;
  GammaY[30,61] := 0.791859208; GammaY[31,61] := 0.800542979; GammaY[32,61] := 0.809172866; GammaY[33,61] := 0.817756303; GammaY[34,61] := 0.826300306; GammaY[35,61] := 0.834811564; GammaY[36,61] := 0.843296464; GammaY[37,61] := 0.851761113; GammaY[38,61] := 0.860211413; GammaY[39,61] := 0.86865308;
  GammaY[40,61] := 0.877091678; GammaY[41,61] := 0.885532653; GammaY[42,61] := 0.893981354; GammaY[43,61] := 0.902443051; GammaY[44,61] := 0.910922995; GammaY[45,61] := 0.919426426; GammaY[46,61] := 0.927958557; GammaY[47,61] := 0.936524643; GammaY[48,61] := 0.945129987; GammaY[49,61] := 0.953779969;
  GammaY[50,61] := 0.96248006; GammaY[51,61] := 0.971235857; GammaY[52,61] := 0.980053076; GammaY[53,61] := 0.988937645; GammaY[54,61] := 0.997895685; GammaY[55,61] := 1.006933523; GammaY[56,61] := 1.016057771; GammaY[57,61] := 1.025275311; GammaY[58,61] := 1.034593368; GammaY[59,61] := 1.044019544;
  GammaY[60,61] := 1.053561855; GammaY[61,61] := 1.06322879; GammaY[62,61] := 1.073029366; GammaY[63,61] := 1.082973188; GammaY[64,61] := 1.093070523; GammaY[65,61] := 1.103332382; GammaY[66,61] := 1.11377061; GammaY[67,61] := 1.124397995; GammaY[68,61] := 1.135228386; GammaY[69,61] := 1.146276838;
  GammaY[70,61] := 1.157559771; GammaY[71,61] := 1.169095162; GammaY[72,61] := 1.180902771; GammaY[73,61] := 1.193004404; GammaY[74,61] := 1.205424227; GammaY[75,61] := 1.218189143; GammaY[76,61] := 1.231329248; GammaY[77,61] := 1.244878377; GammaY[78,61] := 1.258874781; GammaY[79,61] := 1.273361963;
  GammaY[80,61] := 1.288389711; GammaY[81,61] := 1.304015413; GammaY[82,61] := 1.320305725; GammaY[83,61] := 1.337338726; GammaY[84,61] := 1.355206749; GammaY[85,61] := 1.374020136; GammaY[86,61] := 1.393912344; GammaY[87,61] := 1.415046988; GammaY[88,61] := 1.43762783; GammaY[89,61] := 1.461913292;
  GammaY[90,61] := 1.488238253; GammaY[91,61] := 1.517048018; GammaY[92,61] := 1.548953752; GammaY[93,61] := 1.58482817; GammaY[94,61] := 1.625983129; GammaY[95,61] := 1.674532379; GammaY[96,61] := 1.734239692; GammaY[97,61] := 1.812962655; GammaY[98,61] := 1.933051214; GammaY[99,61] := 2.192489852;
end;
procedure InitGammaY63;
begin
  GammaY[0,62] := 0.33068931; GammaY[1,62] := 0.404717442; GammaY[2,62] := 0.446550366; GammaY[3,62] := 0.47685944; GammaY[4,62] := 0.501482945; GammaY[5,62] := 0.522615436; GammaY[6,62] := 0.541356105; GammaY[7,62] := 0.558343892; GammaY[8,62] := 0.573986768; GammaY[9,62] := 0.588562828;
  GammaY[10,62] := 0.602271066; GammaY[11,62] := 0.615259319; GammaY[12,62] := 0.627640749; GammaY[13,62] := 0.639504148; GammaY[14,62] := 0.650920625; GammaY[15,62] := 0.66194815; GammaY[16,62] := 0.672634698; GammaY[17,62] := 0.683020492; GammaY[18,62] := 0.693139667; GammaY[19,62] := 0.703021493;
  GammaY[20,62] := 0.71269131; GammaY[21,62] := 0.72217124; GammaY[22,62] := 0.73148077; GammaY[23,62] := 0.740637184; GammaY[24,62] := 0.749655905; GammaY[25,62] := 0.758550802; GammaY[26,62] := 0.767334422; GammaY[27,62] := 0.776018179; GammaY[28,62] := 0.784612522; GammaY[29,62] := 0.793127055;
  GammaY[30,62] := 0.80157068; GammaY[31,62] := 0.809951682; GammaY[32,62] := 0.818277787; GammaY[33,62] := 0.826556263; GammaY[34,62] := 0.834793975; GammaY[35,62] := 0.842997436; GammaY[36,62] := 0.851172862; GammaY[37,62] := 0.859326208; GammaY[38,62] := 0.867463196; GammaY[39,62] := 0.875589389;
  GammaY[40,62] := 0.8837102; GammaY[41,62] := 0.891830891; GammaY[42,62] := 0.89995664; GammaY[43,62] := 0.908092552; GammaY[44,62] := 0.916243688; GammaY[45,62] := 0.924415083; GammaY[46,62] := 0.93261177; GammaY[47,62] := 0.940838783; GammaY[48,62] := 0.949101237; GammaY[49,62] := 0.957404312;
  GammaY[50,62] := 0.965753239; GammaY[51,62] := 0.974153376; GammaY[52,62] := 0.982610216; GammaY[53,62] := 0.991129408; GammaY[54,62] := 0.999716762; GammaY[55,62] := 1.008378347; GammaY[56,62] := 1.017120483; GammaY[57,62] := 1.025949716; GammaY[58,62] := 1.034872925; GammaY[59,62] := 1.043897343;
  GammaY[60,62] := 1.053030595; GammaY[61,62] := 1.06228075; GammaY[62,62] := 1.071656372; GammaY[63,62] := 1.081166582; GammaY[64,62] := 1.090821123; GammaY[65,62] := 1.100630442; GammaY[66,62] := 1.110605771; GammaY[67,62] := 1.120759232; GammaY[68,62] := 1.13110395; GammaY[69,62] := 1.141654184;
  GammaY[70,62] := 1.152425486; GammaY[71,62] := 1.163434876; GammaY[72,62] := 1.174701057; GammaY[73,62] := 1.186244664; GammaY[74,62] := 1.198088559; GammaY[75,62] := 1.210258192; GammaY[76,62] := 1.222782021; GammaY[77,62] := 1.235692038; GammaY[78,62] := 1.249024405; GammaY[79,62] := 1.262820237;
  GammaY[80,62] := 1.277126584; GammaY[81,62] := 1.291997669; GammaY[82,62] := 1.30749646; GammaY[83,62] := 1.323696707; GammaY[84,62] := 1.340685604; GammaY[85,62] := 1.358567339; GammaY[86,62] := 1.377467904; GammaY[87,62] := 1.397541735; GammaY[88,62] := 1.418981129; GammaY[89,62] := 1.442029921;
  GammaY[90,62] := 1.467004027; GammaY[91,62] := 1.494323449; GammaY[92,62] := 1.524564492; GammaY[93,62] := 1.558549892; GammaY[94,62] := 1.597516034; GammaY[95,62] := 1.643454431; GammaY[96,62] := 1.699909861; GammaY[97,62] := 1.7742792; GammaY[98,62] := 1.887587525; GammaY[99,62] := 2.131748724;
end;
procedure InitGammaY64;
begin
  GammaY[0,63] := 0.350174255; GammaY[1,63] := 0.42377306; GammaY[2,63] := 0.465102001; GammaY[3,63] := 0.494938431; GammaY[4,63] := 0.519120289; GammaY[5,63] := 0.539836056; GammaY[6,63] := 0.558179928; GammaY[7,63] := 0.574787095; GammaY[8,63] := 0.590062693; GammaY[9,63] := 0.604282625;
  GammaY[10,63] := 0.617644154; GammaY[11,63] := 0.630293711; GammaY[12,63] := 0.642343325; GammaY[13,63] := 0.653880844; GammaY[14,63] := 0.664976576; GammaY[15,63] := 0.675687801; GammaY[16,63] := 0.686061894; GammaY[17,63] := 0.696138555; GammaY[18,63] := 0.705951466; GammaY[19,63] := 0.715529496;
  GammaY[20,63] := 0.724897612; GammaY[21,63] := 0.734077611; GammaY[22,63] := 0.743088669; GammaY[23,63] := 0.751947783; GammaY[24,63] := 0.760670115; GammaY[25,63] := 0.7692693; GammaY[26,63] := 0.777757666; GammaY[27,63] := 0.786146408; GammaY[28,63] := 0.794445771; GammaY[29,63] := 0.802665174;
  GammaY[30,63] := 0.810813326; GammaY[31,63] := 0.818898321; GammaY[32,63] := 0.826927718; GammaY[33,63] := 0.834908601; GammaY[34,63] := 0.842847677; GammaY[35,63] := 0.850751309; GammaY[36,63] := 0.858625536; GammaY[37,63] := 0.866476149; GammaY[38,63] := 0.874308721; GammaY[39,63] := 0.88212864;
  GammaY[40,63] := 0.889941139; GammaY[41,63] := 0.897751309; GammaY[42,63] := 0.90556417; GammaY[43,63] := 0.913384672; GammaY[44,63] := 0.921217682; GammaY[45,63] := 0.929068054; GammaY[46,63] := 0.936940635; GammaY[47,63] := 0.944840284; GammaY[48,63] := 0.952771899; GammaY[49,63] := 0.960740415;
  GammaY[50,63] := 0.968750878; GammaY[51,63] := 0.976808438; GammaY[52,63] := 0.984918335; GammaY[53,63] := 0.993085972; GammaY[54,63] := 1.001316902; GammaY[55,63] := 1.009616916; GammaY[56,63] := 1.017992033; GammaY[57,63] := 1.026448497; GammaY[58,63] := 1.034992861; GammaY[59,63] := 1.043632008;
  GammaY[60,63] := 1.052373192; GammaY[61,63] := 1.061224084; GammaY[62,63] := 1.070192822; GammaY[63,63] := 1.079288069; GammaY[64,63] := 1.088519074; GammaY[65,63] := 1.09789575; GammaY[66,63] := 1.107428753; GammaY[67,63] := 1.117129578; GammaY[68,63] := 1.127010664; GammaY[69,63] := 1.137085525;
  GammaY[70,63] := 1.147368894; GammaY[71,63] := 1.157876891; GammaY[72,63] := 1.168627225; GammaY[73,63] := 1.17963943; GammaY[74,63] := 1.190935143; GammaY[75,63] := 1.202538445; GammaY[76,63] := 1.21447626; GammaY[77,63] := 1.226778847; GammaY[78,63] := 1.239480404; GammaY[79,63] := 1.252619807;
  GammaY[80,63] := 1.266241537; GammaY[81,63] := 1.280396845; GammaY[82,63] := 1.295145241; GammaY[83,63] := 1.310556409; GammaY[84,63] := 1.32671273; GammaY[85,63] := 1.343712622; GammaY[86,63] := 1.361675075; GammaY[87,63] := 1.380745927; GammaY[88,63] := 1.401106728; GammaY[89,63] := 1.422987653;
  GammaY[90,63] := 1.446686857; GammaY[91,63] := 1.472600651; GammaY[92,63] := 1.501272712; GammaY[93,63] := 1.533479008; GammaY[94,63] := 1.57038533; GammaY[95,63] := 1.613868893; GammaY[96,63] := 1.667269805; GammaY[97,63] := 1.737554603; GammaY[98,63] := 1.844511831; GammaY[99,63] := 2.074401221;
end;
procedure InitGammaY65;
begin
  GammaY[0,64] := 0.369531949; GammaY[1,64] := 0.442530476; GammaY[2,64] := 0.483283083; GammaY[3,64] := 0.512605126; GammaY[4,64] := 0.536317369; GammaY[5,64] := 0.556596378; GammaY[6,64] := 0.574528621; GammaY[7,64] := 0.590744006; GammaY[8,64] := 0.605643887; GammaY[9,64] := 0.619501369;
  GammaY[10,64] := 0.632511563; GammaY[11,64] := 0.644819208; GammaY[12,64] := 0.656534962; GammaY[13,64] := 0.667745539; GammaY[14,64] := 0.67852032; GammaY[15,64] := 0.688915798; GammaY[16,64] := 0.698978665; GammaY[17,64] := 0.708748041; GammaY[18,64] := 0.718257087; GammaY[19,64] := 0.727534209;
  GammaY[20,64] := 0.736603965; GammaY[21,64] := 0.745487791; GammaY[22,64] := 0.754204542; GammaY[23,64] := 0.762770904; GammaY[24,64] := 0.77120177; GammaY[25,64] := 0.779510508; GammaY[26,64] := 0.787709199; GammaY[27,64] := 0.795808807; GammaY[28,64] := 0.803819372; GammaY[29,64] := 0.811750122;
  GammaY[30,64] := 0.819609562; GammaY[31,64] := 0.8274056; GammaY[32,64] := 0.835145615; GammaY[33,64] := 0.842836531; GammaY[34,64] := 0.850484873; GammaY[35,64] := 0.85809681; GammaY[36,64] := 0.865678237; GammaY[37,64] := 0.873234795; GammaY[38,64] := 0.880771884; GammaY[39,64] := 0.888294727;
  GammaY[40,64] := 0.895808395; GammaY[41,64] := 0.903317829; GammaY[42,64] := 0.91082787; GammaY[43,64] := 0.918343282; GammaY[44,64] := 0.925868756; GammaY[45,64] := 0.933408983; GammaY[46,64] := 0.940968645; GammaY[47,64] := 0.948552402; GammaY[48,64] := 0.956164956; GammaY[49,64] := 0.963811061;
  GammaY[50,64] := 0.97149554; GammaY[51,64] := 0.97922331; GammaY[52,64] := 0.986999377; GammaY[53,64] := 0.994828923; GammaY[54,64] := 1.002717283; GammaY[55,64] := 1.010669959; GammaY[56,64] := 1.01869269; GammaY[57,64] := 1.026791431; GammaY[58,64] := 1.034972425; GammaY[59,64] := 1.043242225;
  GammaY[60,64] := 1.051607731; GammaY[61,64] := 1.06007624; GammaY[62,64] := 1.068655485; GammaY[63,64] := 1.077353696; GammaY[64,64] := 1.086179659; GammaY[65,64] := 1.095142782; GammaY[66,64] := 1.104253175; GammaY[67,64] := 1.113521741; GammaY[68,64] := 1.122960277; GammaY[69,64] := 1.132581594;
  GammaY[70,64] := 1.142399652; GammaY[71,64] := 1.152429725; GammaY[72,64] := 1.162688587; GammaY[73,64] := 1.173194735; GammaY[74,64] := 1.183968655; GammaY[75,64] := 1.195033141; GammaY[76,64] := 1.206413675; GammaY[77,64] := 1.21813889; GammaY[78,64] := 1.230241137; GammaY[79,64] := 1.242757189;
  GammaY[80,64] := 1.255729113; GammaY[81,64] := 1.269205372; GammaY[82,64] := 1.283242228; GammaY[83,64] := 1.297905555; GammaY[84,64] := 1.313273213; GammaY[85,64] := 1.329438207; GammaY[86,64] := 1.346512965; GammaY[87,64] := 1.364635257; GammaY[88,64] := 1.383976566; GammaY[89,64] := 1.404754259;
  GammaY[90,64] := 1.427249851; GammaY[91,64] := 1.451837462; GammaY[92,64] := 1.479030216; GammaY[93,64] := 1.509560295; GammaY[94,64] := 1.544527421; GammaY[95,64] := 1.585701887; GammaY[96,64] := 1.636232413; GammaY[97,64] := 1.702683366; GammaY[98,64] := 1.803688602; GammaY[99,64] := 2.020239377;
end;
procedure InitGammaY66;
begin
  GammaY[0,65] := 0.388716493; GammaY[1,65] := 0.46096146; GammaY[2,65] := 0.501074468; GammaY[3,65] := 0.529846405; GammaY[4,65] := 0.553065599; GammaY[5,65] := 0.572891466; GammaY[6,65] := 0.590400262; GammaY[7,65] := 0.606215267; GammaY[8,65] := 0.620733241; GammaY[9,65] := 0.634223937;
  GammaY[10,65] := 0.64687993; GammaY[11,65] := 0.658843999; GammaY[12,65] := 0.670225246; GammaY[13,65] := 0.681109113; GammaY[14,65] := 0.691563925; GammaY[15,65] := 0.701645286; GammaY[16,65] := 0.711399132; GammaY[17,65] := 0.72086394; GammaY[18,65] := 0.730072317; GammaY[19,65] := 0.739052164;
  GammaY[20,65] := 0.747827609; GammaY[21,65] := 0.756419688; GammaY[22,65] := 0.764846883; GammaY[23,65] := 0.773125571; GammaY[24,65] := 0.781270357; GammaY[25,65] := 0.789294331; GammaY[26,65] := 0.79720932; GammaY[27,65] := 0.805026062; GammaY[28,65] := 0.812754368; GammaY[29,65] := 0.820403244;
  GammaY[30,65] := 0.827980995; GammaY[31,65] := 0.835495347; GammaY[32,65] := 0.842953507; GammaY[33,65] := 0.85036221; GammaY[34,65] := 0.857727811; GammaY[35,65] := 0.865056321; GammaY[36,65] := 0.872353461; GammaY[37,65] := 0.879624695; GammaY[38,65] := 0.886875257; GammaY[39,65] := 0.894110223;
  GammaY[40,65] := 0.901334514; GammaY[41,65] := 0.908552899; GammaY[42,65] := 0.915770054; GammaY[43,65] := 0.922990578; GammaY[44,65] := 0.930219012; GammaY[45,65] := 0.937459859; GammaY[46,65] := 0.944717592; GammaY[47,65] := 0.951996719; GammaY[48,65] := 0.959301773; GammaY[49,65] := 0.966637302;
  GammaY[50,65] := 0.974007928; GammaY[51,65] := 0.981418361; GammaY[52,65] := 0.988873412; GammaY[53,65] := 0.996377991; GammaY[54,65] := 1.003937209; GammaY[55,65] := 1.011556357; GammaY[56,65] := 1.01924087; GammaY[57,65] := 1.026996439; GammaY[58,65] := 1.034829011; GammaY[59,65] := 1.042744825;
  GammaY[60,65] := 1.050750448; GammaY[61,65] := 1.058852819; GammaY[62,65] := 1.067059294; GammaY[63,65] := 1.075377691; GammaY[64,65] := 1.083816356; GammaY[65,65] := 1.092384223; GammaY[66,65] := 1.101090886; GammaY[67,65] := 1.109946693; GammaY[68,65] := 1.118962832; GammaY[69,65] := 1.12815145;
  GammaY[70,65] := 1.137525784; GammaY[71,65] := 1.147100307; GammaY[72,65] := 1.156890914; GammaY[73,65] := 1.166915127; GammaY[74,65] := 1.177192351; GammaY[75,65] := 1.187744167; GammaY[76,65] := 1.198594701; GammaY[77,65] := 1.209771055; GammaY[78,65] := 1.221303847; GammaY[79,65] := 1.233227874;
  GammaY[80,65] := 1.245582937; GammaY[81,65] := 1.258414876; GammaY[82,65] := 1.271776906; GammaY[83,65] := 1.285731325; GammaY[84,65] := 1.300351747; GammaY[85,65] := 1.315726098; GammaY[86,65] := 1.331960649; GammaY[87,65] := 1.349185602; GammaY[88,65] := 1.367562996; GammaY[89,65] := 1.387298191;
  GammaY[90,65] := 1.408657105; GammaY[91,65] := 1.431993057; GammaY[92,65] := 1.457790543; GammaY[93,65] := 1.486740738; GammaY[94,65] := 1.519881503; GammaY[95,65] := 1.558883049; GammaY[96,65] := 1.606715044; GammaY[97,65] := 1.669565806; GammaY[98,65] := 1.764990374; GammaY[99,65] := 1.969069024;
end;
procedure InitGammaY67;
begin
  GammaY[0,66] := 0.407687029; GammaY[1,66] := 0.479042016; GammaY[2,66] := 0.518460629; GammaY[3,66] := 0.546652295; GammaY[4,66] := 0.569359161; GammaY[5,66] := 0.588718795; GammaY[6,66] := 0.605795078; GammaY[7,66] := 0.621203442; GammaY[8,66] := 0.635335316; GammaY[9,66] := 0.648456638;
  GammaY[10,66] := 0.660757132; GammaY[11,66] := 0.672377377; GammaY[12,66] := 0.683424724; GammaY[13,66] := 0.693983224; GammaY[14,66] := 0.704120054; GammaY[15,66] := 0.71388985; GammaY[16,66] := 0.723337741; GammaY[17,66] := 0.732501501; GammaY[18,66] := 0.741413112; GammaY[19,66] := 0.75009996;
  GammaY[20,66] := 0.758585711; GammaY[21,66] := 0.766890978; GammaY[22,66] := 0.775033874; GammaY[23,66] := 0.783030433; GammaY[24,66] := 0.79089494; GammaY[25,66] := 0.798640198; GammaY[26,66] := 0.806277778; GammaY[27,66] := 0.813818187; GammaY[28,66] := 0.821270997; GammaY[29,66] := 0.828644998;
  GammaY[30,66] := 0.835948304; GammaY[31,66] := 0.843188436; GammaY[32,66] := 0.850372393; GammaY[33,66] := 0.85750675; GammaY[34,66] := 0.864597698; GammaY[35,66] := 0.871651073; GammaY[36,66] := 0.878672427; GammaY[37,66] := 0.885667064; GammaY[38,66] := 0.892640071; GammaY[39,66] := 0.899596352;
  GammaY[40,66] := 0.906540642; GammaY[41,66] := 0.913477573; GammaY[42,66] := 0.920411678; GammaY[43,66] := 0.927347381; GammaY[44,66] := 0.934289059; GammaY[45,66] := 0.941241049; GammaY[46,66] := 0.948207671; GammaY[47,66] := 0.955193244; GammaY[48,66] := 0.962202086; GammaY[49,66] := 0.96923859;
  GammaY[50,66] := 0.976307206; GammaY[51,66] := 0.983412427; GammaY[52,66] := 0.990558858; GammaY[53,66] := 0.9977512; GammaY[54,66] := 1.004994332; GammaY[55,66] := 1.0122933; GammaY[56,66] := 1.019653296; GammaY[57,66] := 1.027079744; GammaY[58,66] := 1.034578312; GammaY[59,66] := 1.042154942;
  GammaY[60,66] := 1.049815887; GammaY[61,66] := 1.057567748; GammaY[62,66] := 1.065417519; GammaY[63,66] := 1.073372631; GammaY[64,66] := 1.081441015; GammaY[65,66] := 1.089631154; GammaY[66,66] := 1.097952161; GammaY[67,66] := 1.106413856; GammaY[68,66] := 1.115026853; GammaY[69,66] := 1.123802673;
  GammaY[70,66] := 1.13275387; GammaY[71,66] := 1.141894167; GammaY[72,66] := 1.151238628; GammaY[73,66] := 1.160803859; GammaY[74,66] := 1.170608243; GammaY[75,66] := 1.180672226; GammaY[76,66] := 1.191018654; GammaY[77,66] := 1.201673191; GammaY[78,66] := 1.212664827; GammaY[79,66] := 1.224026499;
  GammaY[80,66] := 1.235795876; GammaY[81,66] := 1.248016341; GammaY[82,66] := 1.260738242; GammaY[83,66] := 1.274020511; GammaY[84,66] := 1.287932785; GammaY[85,66] := 1.302558216; GammaY[86,66] := 1.317997294; GammaY[87,66] := 1.334373124; GammaY[88,66] := 1.351838881; GammaY[89,66] := 1.370588661;
  GammaY[90,66] := 1.390873752; GammaY[91,66] := 1.413027971; GammaY[92,66] := 1.437508978; GammaY[93,66] := 1.46496952; GammaY[94,66] := 1.496389502; GammaY[95,66] := 1.533345418; GammaY[96,66] := 1.578639336; GammaY[97,66] := 1.638107769; GammaY[98,66] := 1.728297286; GammaY[99,66] := 1.92070855;
end;
procedure InitGammaY68;
begin
  GammaY[0,67] := 0.42640747; GammaY[1,67] := 0.496752016; GammaY[2,67] := 0.535429303; GammaY[3,67] := 0.563015634; GammaY[4,67] := 0.58519468; GammaY[5,67] := 0.604078004; GammaY[6,67] := 0.620715162; GammaY[7,67] := 0.635712712; GammaY[8,67] := 0.649456107; GammaY[9,67] := 0.662207038;
  GammaY[10,67] := 0.674152102; GammaY[11,67] := 0.685429484; GammaY[12,67] := 0.696144652; GammaY[13,67] := 0.706380139; GammaY[14,67] := 0.716201869; GammaY[15,67] := 0.725663458; GammaY[16,67] := 0.734809174; GammaY[17,67] := 0.743676042; GammaY[18,67] := 0.752295411; GammaY[19,67] := 0.760694104;
  GammaY[20,67] := 0.768895276; GammaY[21,67] := 0.776919117; GammaY[22,67] := 0.784783358; GammaY[23,67] := 0.792503669; GammaY[24,67] := 0.800094018; GammaY[25,67] := 0.807566922; GammaY[26,67] := 0.814933675; GammaY[27,67] := 0.822204511; GammaY[28,67] := 0.829388785; GammaY[29,67] := 0.83649508;
  GammaY[30,67] := 0.843531292; GammaY[31,67] := 0.850504746; GammaY[32,67] := 0.857422265; GammaY[33,67] := 0.864290234; GammaY[34,67] := 0.871114646; GammaY[35,67] := 0.877901189; GammaY[36,67] := 0.884655262; GammaY[37,67] := 0.891381997; GammaY[38,67] := 0.898086324; GammaY[39,67] := 0.904772989;
  GammaY[40,67] := 0.911446585; GammaY[41,67] := 0.918111577; GammaY[42,67] := 0.92477231; GammaY[43,67] := 0.931433078; GammaY[44,67] := 0.938098112; GammaY[45,67] := 0.944771577; GammaY[46,67] := 0.951457627; GammaY[47,67] := 0.958160412; GammaY[48,67] := 0.964884097; GammaY[49,67] := 0.971632881;
  GammaY[50,67] := 0.978410991; GammaY[51,67] := 0.985222764; GammaY[52,67] := 0.992072627; GammaY[53,67] := 0.998965059; GammaY[54,67] := 1.005904719; GammaY[55,67] := 1.012896425; GammaY[56,67] := 1.019945136; GammaY[57,67] := 1.027056021; GammaY[58,67] := 1.034234485; GammaY[59,67] := 1.041486186;
  GammaY[60,67] := 1.048817079; GammaY[61,67] := 1.056233444; GammaY[62,67] := 1.063741933; GammaY[63,67] := 1.071349615; GammaY[64,67] := 1.079064024; GammaY[65,67] := 1.086893221; GammaY[66,67] := 1.094845859; GammaY[67,67] := 1.10293126; GammaY[68,67] := 1.111159502; GammaY[69,67] := 1.119541516;
  GammaY[70,67] := 1.128089209; GammaY[71,67] := 1.136815596; GammaY[72,67] := 1.14573496; GammaY[73,67] := 1.154863043; GammaY[74,67] := 1.16421727; GammaY[75,67] := 1.173817012; GammaY[76,67] := 1.183683912; GammaY[77,67] := 1.193842284; GammaY[78,67] := 1.204319582; GammaY[79,67] := 1.215146997;
  GammaY[80,67] := 1.226360195; GammaY[81,67] := 1.238000244; GammaY[82,67] := 1.250114799; GammaY[83,67] := 1.262759635; GammaY[84,67] := 1.276000643; GammaY[85,67] := 1.289916498; GammaY[86,67] := 1.304602261; GammaY[87,67] := 1.320174367; GammaY[88,67] := 1.336777671; GammaY[89,67] := 1.354595697;
  GammaY[90,67] := 1.373866007; GammaY[91,67] := 1.394904125; GammaY[92,67] := 1.418142541; GammaY[93,67] := 1.44419797; GammaY[94,67] := 1.473996; GammaY[95,67] := 1.509025315; GammaY[96,67] := 1.551931021; GammaY[97,67] := 1.608220341; GammaY[98,67] := 1.69349664; GammaY[99,67] := 1.874988125;
end;
procedure InitGammaY69;
begin
  GammaY[0,68] := 0.444846218; GammaY[1,68] := 0.514074873; GammaY[2,68] := 0.551971175; GammaY[3,68] := 0.578931773; GammaY[4,68] := 0.600570938; GammaY[5,68] := 0.618970578; GammaY[6,68] := 0.635164234; GammaY[7,68] := 0.649748652; GammaY[8,68] := 0.663102774; GammaY[9,68] := 0.675483701;
  GammaY[10,68] := 0.687074641; GammaY[11,68] := 0.698011206; GammaY[12,68] := 0.708396863; GammaY[13,68] := 0.71831253; GammaY[14,68] := 0.72782283; GammaY[15,68] := 0.736980285; GammaY[16,68] := 0.745828233; GammaY[17,68] := 0.754402935; GammaY[18,68] := 0.762735078; GammaY[19,68] := 0.770850893;
  GammaY[20,68] := 0.778773034; GammaY[21,68] := 0.78652123; GammaY[22,68] := 0.79411279; GammaY[23,68] := 0.801563035; GammaY[24,68] := 0.808885609; GammaY[25,68] := 0.816092724; GammaY[26,68] := 0.823195395; GammaY[27,68] := 0.830203611; GammaY[28,68] := 0.837126481; GammaY[29,68] := 0.843972344;
  GammaY[30,68] := 0.850748904; GammaY[31,68] := 0.8574633; GammaY[32,68] := 0.864122155; GammaY[33,68] := 0.870731676; GammaY[34,68] := 0.877297693; GammaY[35,68] := 0.883825716; GammaY[36,68] := 0.890320956; GammaY[37,68] := 0.896788407; GammaY[38,68] := 0.903232854; GammaY[39,68] := 0.909658878;
  GammaY[40,68] := 0.916070919; GammaY[41,68] := 0.92247329; GammaY[42,68] := 0.928870199; GammaY[43,68] := 0.935265772; GammaY[44,68] := 0.941664056; GammaY[45,68] := 0.948069083; GammaY[46,68] := 0.954484868; GammaY[47,68] := 0.960915384; GammaY[48,68] := 0.967364628; GammaY[49,68] := 0.973836629;
  GammaY[50,68] := 0.980335458; GammaY[51,68] := 0.986865232; GammaY[52,68] := 0.993430185; GammaY[53,68] := 1.00003464; GammaY[54,68] := 1.006683031; GammaY[55,68] := 1.013379956; GammaY[56,68] := 1.020130147; GammaY[57,68] := 1.026938538; GammaY[58,68] := 1.033810279; GammaY[59,68] := 1.040750763;
  GammaY[60,68] := 1.04776566; GammaY[61,68] := 1.054860947; GammaY[62,68] := 1.062042952; GammaY[63,68] := 1.069318398; GammaY[64,68] := 1.07669445; GammaY[65,68] := 1.084178768; GammaY[66,68] := 1.091779569; GammaY[67,68] := 1.099505708; GammaY[68,68] := 1.107366752; GammaY[69,68] := 1.115373077;
  GammaY[70,68] := 1.12353598; GammaY[71,68] := 1.131867809; GammaY[72,68] := 1.140382111; GammaY[73,68] := 1.149093814; GammaY[74,68] := 1.158019435; GammaY[75,68] := 1.167177338; GammaY[76,68] := 1.176588039; GammaY[77,68] := 1.186274574; GammaY[78,68] := 1.196262954; GammaY[79,68] := 1.206582726;
  GammaY[80,68] := 1.217267671; GammaY[81,68] := 1.228356681; GammaY[82,68] := 1.239894877; GammaY[83,68] := 1.251935066; GammaY[84,68] := 1.264539621; GammaY[85,68] := 1.277783004; GammaY[86,68] := 1.291755178; GammaY[87,68] := 1.306566313; GammaY[88,68] := 1.32235345; GammaY[89,68] := 1.339290188;
  GammaY[90,68] := 1.357601198; GammaY[91,68] := 1.377584834; GammaY[92,68] := 1.39964997; GammaY[93,68] := 1.424379522; GammaY[94,68] := 1.452648143; GammaY[95,68] := 1.485862204; GammaY[96,68] := 1.526519734; GammaY[97,68] := 1.579819576; GammaY[98,68] := 1.660482467; GammaY[99,68] := 1.831748985;
end;
procedure InitGammaY70;
begin
  GammaY[0,69] := 0.462975863; GammaY[1,69] := 0.530997217; GammaY[2,69] := 0.56807955; GammaY[3,69] := 0.594398267; GammaY[4,69] := 0.615488612; GammaY[5,69] := 0.633399625; GammaY[6,69] := 0.649147384; GammaY[7,69] := 0.663318034; GammaY[8,69] := 0.676283513; GammaY[9,69] := 0.688296031;
  GammaY[10,69] := 0.699535224; GammaY[11,69] := 0.710133977; GammaY[12,69] := 0.720193629; GammaY[13,69] := 0.729793421; GammaY[14,69] := 0.738996607; GammaY[15,69] := 0.747854573; GammaY[16,69] := 0.756409714; GammaY[17,69] := 0.764697467; GammaY[18,69] := 0.772747826; GammaY[19,69] := 0.780586433;
  GammaY[20,69] := 0.788235409; GammaY[21,69] := 0.795714015; GammaY[22,69] := 0.803039156; GammaY[23,69] := 0.810225763; GammaY[24,69] := 0.817287143; GammaY[25,69] := 0.824235214; GammaY[26,69] := 0.831080705; GammaY[27,69] := 0.837833345; GammaY[28,69] := 0.844502003; GammaY[29,69] := 0.851094804;
  GammaY[30,69] := 0.857619222; GammaY[31,69] := 0.864082194; GammaY[32,69] := 0.870490176; GammaY[33,69] := 0.876849183; GammaY[34,69] := 0.883164876; GammaY[35,69] := 0.889442595; GammaY[36,69] := 0.895687404; GammaY[37,69] := 0.901904115; GammaY[38,69] := 0.908097357; GammaY[39,69] := 0.914271583;
  GammaY[40,69] := 0.920431071; GammaY[41,69] := 0.926579987; GammaY[42,69] := 0.932722388; GammaY[43,69] := 0.938862253; GammaY[44,69] := 0.945003495; GammaY[45,69] := 0.951149964; GammaY[46,69] := 0.957305516; GammaY[47,69] := 0.963474001; GammaY[48,69] := 0.969659241; GammaY[49,69] := 0.9758651;
  GammaY[50,69] := 0.982095481; GammaY[51,69] := 0.988354346; GammaY[52,69] := 0.99464571; GammaY[53,69] := 1.000973708; GammaY[54,69] := 1.007342612; GammaY[55,69] := 1.013756794; GammaY[56,69] := 1.020220772; GammaY[57,69] := 1.026739254; GammaY[58,69] := 1.033317151; GammaY[59,69] := 1.039959601;
  GammaY[60,69] := 1.046672003; GammaY[61,69] := 1.053460051; GammaY[62,69] := 1.060329766; GammaY[63,69] := 1.067287542; GammaY[64,69] := 1.07434019; GammaY[65,69] := 1.081494992; GammaY[66,69] := 1.088759759; GammaY[67,69] := 1.0961429; GammaY[68,69] := 1.103653501; GammaY[69,69] := 1.111301412;
  GammaY[70,69] := 1.119097359; GammaY[71,69] := 1.12705306; GammaY[72,69] := 1.135181368; GammaY[73,69] := 1.14349644; GammaY[74,69] := 1.152013944; GammaY[75,69] := 1.160751291; GammaY[76,69] := 1.169727932; GammaY[77,69] := 1.178965702; GammaY[78,69] := 1.188489255; GammaY[79,69] := 1.198326589;
  GammaY[80,69] := 1.20850971; GammaY[81,69] := 1.219075464; GammaY[82,69] := 1.230066592; GammaY[83,69] := 1.241533104; GammaY[84,69] := 1.253534068; GammaY[85,69] := 1.266139985; GammaY[86,69] := 1.279436019; GammaY[87,69] := 1.293526456; GammaY[88,69] := 1.308540996; GammaY[89,69] := 1.32464391;
  GammaY[90,69] := 1.342047766; GammaY[91,69] := 1.361034791; GammaY[92,69] := 1.381991693; GammaY[93,69] := 1.405469663; GammaY[94,69] := 1.432295564; GammaY[95,69] := 1.46379857; GammaY[96,69] := 1.502338829; GammaY[97,69] := 1.552826234; GammaY[98,69] := 1.629155139; GammaY[99,69] := 1.790842719;
end;
procedure InitGammaY71;
begin
  GammaY[0,70] := 0.480772885; GammaY[1,70] := 0.547508576; GammaY[2,70] := 0.583750087; GammaY[3,70] := 0.609414633; GammaY[4,70] := 0.629950019; GammaY[5,70] := 0.647369655; GammaY[6,70] := 0.662670886; GammaY[7,70] := 0.676428593; GammaY[8,70] := 0.689007323; GammaY[9,70] := 0.700654123;
  GammaY[10,70] := 0.711544869; GammaY[11,70] := 0.721809621; GammaY[12,70] := 0.731547515; GammaY[13,70] := 0.740836026; GammaY[14,70] := 0.749736987; GammaY[15,70] := 0.758300623; GammaY[16,70] := 0.766568343; GammaY[17,70] := 0.774574762; GammaY[18,70] := 0.782349154; GammaY[19,70] := 0.789916528;
  GammaY[20,70] := 0.797298478; GammaY[21,70] := 0.804513801; GammaY[22,70] := 0.811578968; GammaY[23,70] := 0.818508542; GammaY[24,70] := 0.825315479; GammaY[25,70] := 0.832011369; GammaY[26,70] := 0.838606671; GammaY[27,70] := 0.845110864; GammaY[28,70] := 0.851532565; GammaY[29,70] := 0.857879675;
  GammaY[30,70] := 0.864159467; GammaY[31,70] := 0.87037867; GammaY[32,70] := 0.876543529; GammaY[33,70] := 0.882659899; GammaY[34,70] := 0.888733282; GammaY[35,70] := 0.894768842; GammaY[36,70] := 0.900771484; GammaY[37,70] := 0.906745878; GammaY[38,70] := 0.912696489; GammaY[39,70] := 0.918627595;
  GammaY[40,70] := 0.92454335; GammaY[41,70] := 0.930447789; GammaY[42,70] := 0.936344812; GammaY[43,70] := 0.942238253; GammaY[44,70] := 0.948131878; GammaY[45,70] := 0.95402941; GammaY[46,70] := 0.959934523; GammaY[47,70] := 0.965850914; GammaY[48,70] := 0.971782283; GammaY[49,70] := 0.977732322;
  GammaY[50,70] := 0.98370477; GammaY[51,70] := 0.989703421; GammaY[52,70] := 0.995732139; GammaY[53,70] := 1.001794838; GammaY[54,70] := 1.007895604; GammaY[55,70] := 1.01403865; GammaY[56,70] := 1.020228271; GammaY[57,70] := 1.026468958; GammaY[58,70] := 1.032765392; GammaY[59,70] := 1.039122476;
  GammaY[60,70] := 1.04554535; GammaY[61,70] := 1.05203943; GammaY[62,70] := 1.058610453; GammaY[63,70] := 1.065264502; GammaY[64,70] := 1.072008055; GammaY[65,70] := 1.078848036; GammaY[66,70] := 1.085791868; GammaY[67,70] := 1.092847543; GammaY[68,70] := 1.100023692; GammaY[69,70] := 1.10732967;
  GammaY[70,70] := 1.11477566; GammaY[71,70] := 1.122372783; GammaY[72,70] := 1.130133239; GammaY[73,70] := 1.138070462; GammaY[74,70] := 1.146199313; GammaY[75,70] := 1.154536311; GammaY[76,70] := 1.163099904; GammaY[77,70] := 1.171910794; GammaY[78,70] := 1.180992354; GammaY[79,70] := 1.190371126;
  GammaY[80,70] := 1.200077446; GammaY[81,70] := 1.21014623; GammaY[82,70] := 1.220617979; GammaY[83,70] := 1.231540074; GammaY[84,70] := 1.242968468; GammaY[85,70] := 1.254969939; GammaY[86,70] := 1.267625146; GammaY[87,70] := 1.281032836; GammaY[88,70] := 1.295315805; GammaY[89,70] := 1.310629556;
  GammaY[90,70] := 1.327175283; GammaY[91,70] := 1.345220069; GammaY[92,70] := 1.365129795; GammaY[93,70] := 1.387425869; GammaY[94,70] := 1.412890292; GammaY[95,70] := 1.442779792; GammaY[96,70] := 1.479325196; GammaY[97,70] := 1.527165515; GammaY[98,70] := 1.599420973; GammaY[99,70] := 1.752130387;
end;
procedure InitGammaY72;
begin
  GammaY[0,71] := 0.498217348; GammaY[1,71] := 0.563601113; GammaY[2,71] := 0.598980527; GammaY[3,71] := 0.623982098; GammaY[4,71] := 0.643958906; GammaY[5,71] := 0.660886367; GammaY[6,71] := 0.675742022; GammaY[7,71] := 0.689088917; GammaY[8,71] := 0.701283879; GammaY[9,71] := 0.712568593;
  GammaY[10,71] := 0.723115026; GammaY[11,71] := 0.733050298; GammaY[12,71] := 0.742471285; GammaY[13,71] := 0.751453656; GammaY[14,71] := 0.760057772; GammaY[15,71] := 0.768332656; GammaY[16,71] := 0.776318724; GammaY[17,71] := 0.784049737; GammaY[18,71] := 0.791554241; GammaY[19,71] := 0.798856629;
  GammaY[20,71] := 0.805977928; GammaY[21,71] := 0.812936449; GammaY[22,71] := 0.819748253; GammaY[23,71] := 0.826427511; GammaY[24,71] := 0.832986834; GammaY[25,71] := 0.839437508; GammaY[26,71] := 0.845789689; GammaY[27,71] := 0.852052591; GammaY[28,71] := 0.858234607; GammaY[29,71] := 0.864343403;
  GammaY[30,71] := 0.870386043; GammaY[31,71] := 0.876369058; GammaY[32,71] := 0.882298517; GammaY[33,71] := 0.888180075; GammaY[34,71] := 0.89401906; GammaY[35,71] := 0.8998205; GammaY[36,71] := 0.905589129; GammaY[37,71] := 0.911329465; GammaY[38,71] := 0.917045824; GammaY[39,71] := 0.92274234;
  GammaY[40,71] := 0.928423023; GammaY[41,71] := 0.934091763; GammaY[42,71] := 0.939752322; GammaY[43,71] := 0.945408392; GammaY[44,71] := 0.951063599; GammaY[45,71] := 0.956721523; GammaY[46,71] := 0.962385697; GammaY[47,71] := 0.96805967; GammaY[48,71] := 0.973746995; GammaY[49,71] := 0.979451209;
  GammaY[50,71] := 0.985175899; GammaY[51,71] := 0.990924698; GammaY[52,71] := 0.996701303; GammaY[53,71] := 1.002509457; GammaY[54,71] := 1.008353067; GammaY[55,71] := 1.014236159; GammaY[56,71] := 1.020162833; GammaY[57,71] := 1.026137372; GammaY[58,71] := 1.032164244; GammaY[59,71] := 1.038248116;
  GammaY[60,71] := 1.044393891; GammaY[61,71] := 1.050606731; GammaY[62,71] := 1.056892093; GammaY[63,71] := 1.063255766; GammaY[64,71] := 1.069703914; GammaY[65,71] := 1.076243124; GammaY[66,71] := 1.082880451; GammaY[67,71] := 1.089623487; GammaY[68,71] := 1.096480436; GammaY[69,71] := 1.103460187;
  GammaY[70,71] := 1.11057241; GammaY[71,71] := 1.117827665; GammaY[72,71] := 1.125237534; GammaY[73,71] := 1.132814768; GammaY[74,71] := 1.140573473; GammaY[75,71] := 1.148529319; GammaY[76,71] := 1.156699805; GammaY[77,71] := 1.165104571; GammaY[78,71] := 1.173765783; GammaY[79,71] := 1.182708609;
  GammaY[80,71] := 1.191961811; GammaY[81,71] := 1.201558495; GammaY[82,71] := 1.211537048; GammaY[83,71] := 1.221942375; GammaY[84,71] := 1.232827498; GammaY[85,71] := 1.244255693; GammaY[86,71] := 1.256303372; GammaY[87,71] := 1.269064075; GammaY[88,71] := 1.282654107; GammaY[89,71] := 1.297220733;
  GammaY[90,71] := 1.312954446; GammaY[91,71] := 1.330108089; GammaY[92,71] := 1.349027979; GammaY[93,71] := 1.370207541; GammaY[94,71] := 1.394386643; GammaY[95,71] := 1.422754008; GammaY[96,71] := 1.457419089; GammaY[97,71] := 1.502766825; GammaY[98,71] := 1.571191878; GammaY[99,71] := 1.71548236;
end;
procedure InitGammaY73;
begin
  GammaY[0,72] := 0.515292629; GammaY[1,72] := 0.579269336; GammaY[2,72] := 0.613770458; GammaY[3,72] := 0.638103399; GammaY[4,72] := 0.657520266; GammaY[5,72] := 0.673956474; GammaY[6,72] := 0.688368899; GammaY[7,72] := 0.701308267; GammaY[8,72] := 0.713123406; GammaY[9,72] := 0.724050474;
  GammaY[10,72] := 0.734257429; GammaY[11,72] := 0.743868365; GammaY[12,72] := 0.752977825; GammaY[13,72] := 0.761659646; GammaY[14,72] := 0.769972701; GammaY[15,72] := 0.777964772; GammaY[16,72] := 0.785675257; GammaY[17,72] := 0.793137065; GammaY[18,72] := 0.800378001; GammaY[19,72] := 0.807421819;
  GammaY[20,72] := 0.814289003; GammaY[21,72] := 0.820997358; GammaY[22,72] := 0.827562512; GammaY[23,72] := 0.833998267; GammaY[24,72] := 0.840316878; GammaY[25,72] := 0.846529319; GammaY[26,72] := 0.852645467; GammaY[27,72] := 0.858674255; GammaY[28,72] := 0.864623832; GammaY[29,72] := 0.870501661;
  GammaY[30,72] := 0.876314585; GammaY[31,72] := 0.882068937; GammaY[32,72] := 0.887770603; GammaY[33,72] := 0.893425062; GammaY[34,72] := 0.899037475; GammaY[35,72] := 0.904612704; GammaY[36,72] := 0.910155333; GammaY[37,72] := 0.915669727; GammaY[38,72] := 0.92116006; GammaY[39,72] := 0.926630334;
  GammaY[40,72] := 0.932084393; GammaY[41,72] := 0.937525989; GammaY[42,72] := 0.942958773; GammaY[43,72] := 0.948386287; GammaY[44,72] := 0.953812021; GammaY[45,72] := 0.959239417; GammaY[46,72] := 0.964671888; GammaY[47,72] := 0.970112809; GammaY[48,72] := 0.975565589; GammaY[49,72] := 0.981033654;
  GammaY[50,72] := 0.986520423; GammaY[51,72] := 0.992029377; GammaY[52,72] := 0.997564026; GammaY[53,72] := 1.003128008; GammaY[54,72] := 1.008725053; GammaY[55,72] := 1.014358955; GammaY[56,72] := 1.020033654; GammaY[57,72] := 1.025753239; GammaY[58,72] := 1.031521971; GammaY[59,72] := 1.037344306;
  GammaY[60,72] := 1.043224912; GammaY[61,72] := 1.049168705; GammaY[62,72] := 1.055180884; GammaY[63,72] := 1.061266958; GammaY[64,72] := 1.067432792; GammaY[65,72] := 1.073684649; GammaY[66,72] := 1.080029243; GammaY[67,72] := 1.086473793; GammaY[68,72] := 1.093026094; GammaY[69,72] := 1.099694592;
  GammaY[70,72] := 1.106488474; GammaY[71,72] := 1.113417774; GammaY[72,72] := 1.120493485; GammaY[73,72] := 1.127727712; GammaY[74,72] := 1.135133849; GammaY[75,72] := 1.142726769; GammaY[76,72] := 1.150523077; GammaY[77,72] := 1.158541409; GammaY[78,72] := 1.166802794; GammaY[79,72] := 1.175331107;
  GammaY[80,72] := 1.184153627; GammaY[81,72] := 1.193301743; GammaY[82,72] := 1.202811859; GammaY[83,72] := 1.212726545; GammaY[84,72] := 1.223096069; GammaY[85,72] := 1.233980407; GammaY[86,72] := 1.245451974; GammaY[87,72] := 1.257599409; GammaY[88,72] := 1.270532908; GammaY[89,72] := 1.284391977;
  GammaY[90,72] := 1.299357055; GammaY[91,72] := 1.315667598; GammaY[92,72] := 1.33365152; GammaY[93,72] := 1.353775944; GammaY[94,72] := 1.376741154; GammaY[95,72] := 1.403671988; GammaY[96,72] := 1.436563949; GammaY[97,72] := 1.479563536; GammaY[98,72] := 1.544385013; GammaY[99,72] := 1.68077696;
end;
procedure InitGammaY74;
begin
  GammaY[0,73] := 0.531985117; GammaY[1,73] := 0.594509861; GammaY[2,73] := 0.628121098; GammaY[3,73] := 0.651782567; GammaY[4,73] := 0.670640138; GammaY[5,73] := 0.686587566; GammaY[6,73] := 0.700560313; GammaY[7,73] := 0.713096442; GammaY[8,73] := 0.724536548; GammaY[9,73] := 0.735111119;
  GammaY[10,73] := 0.744984022; GammaY[11,73] := 0.754276276; GammaY[12,73] := 0.763080049; GammaY[13,73] := 0.771467302; GammaY[14,73] := 0.779495397; GammaY[15,73] := 0.787210883; GammaY[16,73] := 0.794652111; GammaY[17,73] := 0.801851119; GammaY[18,73] := 0.80883498; GammaY[19,73] := 0.815626801;
  GammaY[20,73] := 0.822246508; GammaY[21,73] := 0.828711418; GammaY[22,73] := 0.835036723; GammaY[23,73] := 0.841235836; GammaY[24,73] := 0.847320667; GammaY[25,73] := 0.853301875; GammaY[26,73] := 0.85918905; GammaY[27,73] := 0.864990866; GammaY[28,73] := 0.87071523; GammaY[29,73] := 0.876369378;
  GammaY[30,73] := 0.881959947; GammaY[31,73] := 0.887493077; GammaY[32,73] := 0.892974467; GammaY[33,73] := 0.898409437; GammaY[34,73] := 0.903802958; GammaY[35,73] := 0.909159734; GammaY[36,73] := 0.914484218; GammaY[37,73] := 0.919780621; GammaY[38,73] := 0.925052969; GammaY[39,73] := 0.930305128;
  GammaY[40,73] := 0.935540807; GammaY[41,73] := 0.94076362; GammaY[42,73] := 0.945977086; GammaY[43,73] := 0.951184615; GammaY[44,73] := 0.956389565; GammaY[45,73] := 0.961595245; GammaY[46,73] := 0.966804934; GammaY[47,73] := 0.972021873; GammaY[48,73] := 0.977249334; GammaY[49,73] := 0.982490601;
  GammaY[50,73] := 0.987748951; GammaY[51,73] := 0.993027717; GammaY[52,73] := 0.998330261; GammaY[53,73] := 1.003660058; GammaY[54,73] := 1.009020679; GammaY[55,73] := 1.014415746; GammaY[56,73] := 1.019849023; GammaY[57,73] := 1.025324414; GammaY[58,73] := 1.030845983; GammaY[59,73] := 1.036417976;
  GammaY[60,73] := 1.042044843; GammaY[61,73] := 1.047731272; GammaY[62,73] := 1.053482212; GammaY[63,73] := 1.059302905; GammaY[64,73] := 1.065198939; GammaY[65,73] := 1.071176274; GammaY[66,73] := 1.077241293; GammaY[67,73] := 1.083400862; GammaY[68,73] := 1.089662391; GammaY[69,73] := 1.096033907;
  GammaY[70,73] := 1.102524142; GammaY[71,73] := 1.109142625; GammaY[72,73] := 1.115899802; GammaY[73,73] := 1.122807173; GammaY[74,73] := 1.12987745; GammaY[75,73] := 1.13712476; GammaY[76,73] := 1.144564861; GammaY[77,73] := 1.152215438; GammaY[78,73] := 1.160096451; GammaY[79,73] := 1.168230555;
  GammaY[80,73] := 1.176643629; GammaY[81,73] := 1.185365452; GammaY[82,73] := 1.194430553; GammaY[83,73] := 1.203879307; GammaY[84,73] := 1.21375938; GammaY[85,73] := 1.224127635; GammaY[86,73] := 1.235052731; GammaY[87,73] := 1.246618694; GammaY[88,73] := 1.258929965; GammaY[89,73] := 1.272118751;
  GammaY[90,73] := 1.286356024; GammaY[91,73] := 1.30186864; GammaY[92,73] := 1.318967212; GammaY[93,73] := 1.338094135; GammaY[94,73] := 1.35991247; GammaY[95,73] := 1.38548702; GammaY[96,73] := 1.416706242; GammaY[97,73] := 1.45749276; GammaY[98,73] := 1.518922458; GammaY[99,73] := 1.647900478;
end;
procedure InitGammaY75;
begin
  GammaY[0,74] := 0.548283955; GammaY[1,74] := 0.609321186; GammaY[2,74] := 0.642035093; GammaY[3,74] := 0.665024768; GammaY[4,74] := 0.68332546; GammaY[5,74] := 0.698787926; GammaY[6,74] := 0.712325635; GammaY[7,74] := 0.724463673; GammaY[8,74] := 0.735534253; GammaY[9,74] := 0.74576209;
  GammaY[10,74] := 0.755306887; GammaY[11,74] := 0.764286552; GammaY[12,74] := 0.772790841; GammaY[13,74] := 0.78088982; GammaY[14,74] := 0.788639333; GammaY[15,74] := 0.796084676; GammaY[16,74] := 0.803263166; GammaY[17,74] := 0.81020595; GammaY[18,74] := 0.816939353; GammaY[19,74] := 0.823485853;
  GammaY[20,74] := 0.829864811; GammaY[21,74] := 0.83609306; GammaY[22,74] := 0.842185342; GammaY[23,74] := 0.848154684; GammaY[24,74] := 0.854012665; GammaY[25,74] := 0.859769621; GammaY[26,74] := 0.86543486; GammaY[27,74] := 0.871016804; GammaY[28,74] := 0.876523102; GammaY[29,74] := 0.881960768;
  GammaY[30,74] := 0.887336252; GammaY[31,74] := 0.892655491; GammaY[32,74] := 0.897924005; GammaY[33,74] := 0.90314694; GammaY[34,74] := 0.908329107; GammaY[35,74] := 0.913475053; GammaY[36,74] := 0.918589083; GammaY[37,74] := 0.923675262; GammaY[38,74] := 0.928737479; GammaY[39,74] := 0.933779463;
  GammaY[40,74] := 0.938804789; GammaY[41,74] := 0.943816943; GammaY[42,74] := 0.948819312; GammaY[43,74] := 0.953815179; GammaY[44,74] := 0.958807774; GammaY[45,74] := 0.963800279; GammaY[46,74] := 0.968795842; GammaY[47,74] := 0.973797575; GammaY[48,74] := 0.978808617; GammaY[49,74] := 0.98383212;
  GammaY[50,74] := 0.988871222; GammaY[51,74] := 0.993929115; GammaY[52,74] := 0.999009011; GammaY[53,74] := 1.004114241; GammaY[54,74] := 1.009248223; GammaY[55,74] := 1.014414418; GammaY[56,74] := 1.019616416; GammaY[57,74] := 1.024857938; GammaY[58,74] := 1.030142867; GammaY[59,74] := 1.035475253;
  GammaY[60,74] := 1.04085934; GammaY[61,74] := 1.046299593; GammaY[62,74] := 1.051800727; GammaY[63,74] := 1.057367738; GammaY[64,74] := 1.063005941; GammaY[65,74] := 1.068721007; GammaY[66,74] := 1.074519011; GammaY[67,74] := 1.080406483; GammaY[68,74] := 1.086390468; GammaY[69,74] := 1.092478602;
  GammaY[70,74] := 1.098679186; GammaY[71,74] := 1.105001272; GammaY[72,74] := 1.111454788; GammaY[73,74] := 1.118050656; GammaY[74,74] := 1.124800952; GammaY[75,74] := 1.131719089; GammaY[76,74] := 1.138820033; GammaY[77,74] := 1.146120579; GammaY[78,74] := 1.153639669; GammaY[79,74] := 1.16139881;
  GammaY[80,74] := 1.169422561; GammaY[81,74] := 1.177739176; GammaY[82,74] := 1.186381419; GammaY[83,74] := 1.195387597; GammaY[84,74] := 1.204802925; GammaY[85,74] := 1.214681337; GammaY[86,74] := 1.225087948; GammaY[87,74] := 1.23610243; GammaY[88,74] := 1.247823809; GammaY[89,74] := 1.260377421;
  GammaY[90,74] := 1.273925329; GammaY[91,74] := 1.288682525; GammaY[92,74] := 1.30494334; GammaY[93,74] := 1.323126897; GammaY[94,74] := 1.343861259; GammaY[95,74] := 1.36815478; GammaY[96,74] := 1.397795301; GammaY[97,74] := 1.436495145; GammaY[98,74] := 1.494730921; GammaY[99,74] := 1.616746478;
end;
procedure InitGammaY76;
begin
  GammaY[0,75] := 0.564180792; GammaY[1,75] := 0.623703473; GammaY[2,75] := 0.655516325; GammaY[3,75] := 0.677836125; GammaY[4,75] := 0.695583938; GammaY[5,75] := 0.71056645; GammaY[6,75] := 0.723674685; GammaY[7,75] := 0.735420532; GammaY[8,75] := 0.746127714; GammaY[9,75] := 0.756015088;
  GammaY[10,75] := 0.765238147; GammaY[11,75] := 0.773911668; GammaY[12,75] := 0.782122982; GammaY[13,75] := 0.789940245; GammaY[14,75] := 0.79741777; GammaY[15,75] := 0.804599595; GammaY[16,75] := 0.811521995; GammaY[17,75] := 0.818215238; GammaY[18,75] := 0.824704903; GammaY[19,75] := 0.831012825;
  GammaY[20,75] := 0.837157804; GammaY[21,75] := 0.843156184; GammaY[22,75] := 0.849022276; GammaY[23,75] := 0.854768721; GammaY[24,75] := 0.860406752; GammaY[25,75] := 0.865946395; GammaY[26,75] := 0.871396676; GammaY[27,75] := 0.876765758; GammaY[28,75] := 0.882061053; GammaY[29,75] := 0.887289356;
  GammaY[30,75] := 0.892456913; GammaY[31,75] := 0.897569472; GammaY[32,75] := 0.902632375; GammaY[33,75] := 0.907650599; GammaY[34,75] := 0.912628793; GammaY[35,75] := 0.917571355; GammaY[36,75] := 0.922482443; GammaY[37,75] := 0.92736598; GammaY[38,75] := 0.93222572; GammaY[39,75] := 0.93706526;
  GammaY[40,75] := 0.941888045; GammaY[41,75] := 0.946697433; GammaY[42,75] := 0.951496688; GammaY[43,75] := 0.956288966; GammaY[44,75] := 0.961077376; GammaY[45,75] := 0.965864974; GammaY[46,75] := 0.97065477; GammaY[47,75] := 0.975449781; GammaY[48,75] := 0.98025302; GammaY[49,75] := 0.985067477;
  GammaY[50,75] := 0.989896176; GammaY[51,75] := 0.994742173; GammaY[52,75] := 0.999608536; GammaY[53,75] := 1.004498457; GammaY[54,75] := 1.009415212; GammaY[55,75] := 1.014362095; GammaY[56,75] := 1.019342542; GammaY[57,75] := 1.02436011; GammaY[58,75] := 1.029418499; GammaY[59,75] := 1.034521571;
  GammaY[60,75] := 1.039673374; GammaY[61,75] := 1.044878168; GammaY[62,75] := 1.05014044; GammaY[63,75] := 1.055464946; GammaY[64,75] := 1.06085675; GammaY[65,75] := 1.066321251; GammaY[66,75] := 1.071864231; GammaY[67,75] := 1.077491902; GammaY[68,75] := 1.083210969; GammaY[69,75] := 1.089028688;
  GammaY[70,75] := 1.09495295; GammaY[71,75] := 1.100992364; GammaY[72,75] := 1.10715636; GammaY[73,75] := 1.113455323; GammaY[74,75] := 1.11990073; GammaY[75,75] := 1.126505317; GammaY[76,75] := 1.1332833; GammaY[77,75] := 1.140250629; GammaY[78,75] := 1.147425292; GammaY[79,75] := 1.154827704;
  GammaY[80,75] := 1.162481185; GammaY[81,75] := 1.170412557; GammaY[82,75] := 1.178652916; GammaY[83,75] := 1.187238613; GammaY[84,75] := 1.196212548; GammaY[85,75] := 1.2056259; GammaY[86,75] := 1.215540439; GammaY[87,75] := 1.226031742; GammaY[88,75] := 1.237193731; GammaY[89,75] := 1.249145264;
  GammaY[90,75] := 1.262040004; GammaY[91,75] := 1.276081782; GammaY[92,75] := 1.291549599; GammaY[93,75] := 1.308840673; GammaY[94,75] := 1.328550125; GammaY[95,75] := 1.351633215; GammaY[96,75] := 1.379783176; GammaY[97,75] := 1.416514661; GammaY[98,75] := 1.471741437; GammaY[99,75] := 1.587215467;
end;
procedure InitGammaY77;
begin
  GammaY[0,76] := 0.57966953; GammaY[1,76] := 0.63765834; GammaY[2,76] := 0.668569754; GammaY[3,76] := 0.690223596; GammaY[4,76] := 0.707423907; GammaY[5,76] := 0.721932491; GammaY[6,76] := 0.734617621; GammaY[7,76] := 0.745977829; GammaY[8,76] := 0.756328253; GammaY[9,76] := 0.765881864;
  GammaY[10,76] := 0.774789922; GammaY[11,76] := 0.783164044; GammaY[12,76] := 0.791089139; GammaY[13,76] := 0.798631442; GammaY[14,76] := 0.805843726; GammaY[15,76] := 0.812768789; GammaY[16,76] := 0.819441855; GammaY[17,76] := 0.825892319; GammaY[18,76] := 0.832145012; GammaY[19,76] := 0.838221128;
  GammaY[20,76] := 0.844138911; GammaY[21,76] := 0.849914219; GammaY[22,76] := 0.855560931; GammaY[23,76] := 0.861091308; GammaY[24,76] := 0.866516238; GammaY[25,76] := 0.871845443; GammaY[26,76] := 0.877087665; GammaY[27,76] := 0.882250814; GammaY[28,76] := 0.887342067; GammaY[29,76] := 0.892368004;
  GammaY[30,76] := 0.89733467; GammaY[31,76] := 0.902247626; GammaY[32,76] := 0.907112037; GammaY[33,76] := 0.911932705; GammaY[34,76] := 0.916714146; GammaY[35,76] := 0.921460606; GammaY[36,76] := 0.926176079; GammaY[37,76] := 0.930864362; GammaY[38,76] := 0.935529077; GammaY[39,76] := 0.940173678;
  GammaY[40,76] := 0.94480151; GammaY[41,76] := 0.949415809; GammaY[42,76] := 0.95401969; GammaY[43,76] := 0.958616205; GammaY[44,76] := 0.963208339; GammaY[45,76] := 0.967799032; GammaY[46,76] := 0.972391171; GammaY[47,76] := 0.976987654; GammaY[48,76] := 0.981591374; GammaY[49,76] := 0.986205195;
  GammaY[50,76] := 0.990832016; GammaY[51,76] := 0.995474746; GammaY[52,76] := 1.000136352; GammaY[53,76] := 1.004819892; GammaY[54,76] := 1.009528463; GammaY[55,76] := 1.014265226; GammaY[56,76] := 1.019033469; GammaY[57,76] := 1.023836592; GammaY[58,76] := 1.028678122; GammaY[59,76] := 1.033561742;
  GammaY[60,76] := 1.038491313; GammaY[61,76] := 1.043470893; GammaY[62,76] := 1.048504761; GammaY[63,76] := 1.053597453; GammaY[64,76] := 1.058753787; GammaY[65,76] := 1.063978903; GammaY[66,76] := 1.069278308; GammaY[67,76] := 1.074657912; GammaY[68,76] := 1.080124087; GammaY[69,76] := 1.085683742;
  GammaY[70,76] := 1.09134438; GammaY[71,76] := 1.097114188; GammaY[72,76] := 1.103002134; GammaY[73,76] := 1.109018084; GammaY[74,76] := 1.115172943; GammaY[75,76] := 1.121478817; GammaY[76,76] := 1.12794921; GammaY[77,76] := 1.134599276; GammaY[78,76] := 1.141446107; GammaY[79,76] := 1.148509086;
  GammaY[80,76] := 1.155810358; GammaY[81,76] := 1.163375391; GammaY[82,76] := 1.171233707; GammaY[83,76] := 1.179419815; GammaY[84,76] := 1.18797444; GammaY[85,76] := 1.196946154; GammaY[86,76] := 1.206393574; GammaY[87,76] := 1.21638841; GammaY[88,76] := 1.227019775; GammaY[89,76] := 1.238400423;
  GammaY[90,76] := 1.250676107; GammaY[91,76] := 1.264040135; GammaY[92,76] := 1.278757062; GammaY[93,76] := 1.295203485; GammaY[94,76] := 1.313943517; GammaY[95,76] := 1.335882438; GammaY[96,76] := 1.362624479; GammaY[97,76] := 1.397498419; GammaY[98,76] := 1.449889113; GammaY[99,76] := 1.559214068;
end;
procedure InitGammaY78;
begin
  GammaY[0,77] := 0.594746108; GammaY[1,77] := 0.651188712; GammaY[2,77] := 0.681201279; GammaY[3,77] := 0.702194817; GammaY[4,77] := 0.718854209; GammaY[5,77] := 0.732895797; GammaY[6,77] := 0.745164877; GammaY[7,77] := 0.756146538; GammaY[8,77] := 0.766147288; GammaY[9,77] := 0.775374192;
  GammaY[10,77] := 0.783974267; GammaY[11,77] := 0.792055969; GammaY[12,77] := 0.799701781; GammaY[13,77] := 0.806976028; GammaY[14,77] := 0.813929953; GammaY[15,77] := 0.820605099; GammaY[16,77] := 0.827035654; GammaY[17,77] := 0.83325014; GammaY[18,77] := 0.839272648; GammaY[19,77] := 0.845123734;
  GammaY[20,77] := 0.85082109; GammaY[21,77] := 0.856380091; GammaY[22,77] := 0.861814189; GammaY[23,77] := 0.867135268; GammaY[24,77] := 0.872353877; GammaY[25,77] := 0.877479433; GammaY[26,77] := 0.882520402; GammaY[27,77] := 0.887484432; GammaY[28,77] := 0.892378489; GammaY[29,77] := 0.89720894;
  GammaY[30,77] := 0.901981614; GammaY[31,77] := 0.906701898; GammaY[32,77] := 0.911374783; GammaY[33,77] := 0.916004911; GammaY[34,77] := 0.920596642; GammaY[35,77] := 0.92515408; GammaY[36,77] := 0.929681077; GammaY[37,77] := 0.934181297; GammaY[38,77] := 0.938658232; GammaY[39,77] := 0.943115209;
  GammaY[40,77] := 0.947555454; GammaY[41,77] := 0.951982081; GammaY[42,77] := 0.956398091; GammaY[43,77] := 0.960806415; GammaY[44,77] := 0.965209926; GammaY[45,77] := 0.96961143; GammaY[46,77] := 0.974013732; GammaY[47,77] := 0.978419614; GammaY[48,77] := 0.982831821; GammaY[49,77] := 0.987253114;
  GammaY[50,77] := 0.991686252; GammaY[51,77] := 0.996134052; GammaY[52,77] := 1.000599356; GammaY[53,77] := 1.005085059; GammaY[54,77] := 1.009594138; GammaY[55,77] := 1.014129619; GammaY[56,77] := 1.018694636; GammaY[57,77] := 1.023292434; GammaY[58,77] := 1.027926382; GammaY[59,77] := 1.032599997;
  GammaY[60,77] := 1.037316956; GammaY[61,77] := 1.042081132; GammaY[62,77] := 1.04689661; GammaY[63,77] := 1.051767705; GammaY[64,77] := 1.056699007; GammaY[65,77] := 1.061695412; GammaY[66,77] := 1.066762159; GammaY[67,77] := 1.071904876; GammaY[68,77] := 1.077129633; GammaY[69,77] := 1.082442998;
  GammaY[70,77] := 1.087852112; GammaY[71,77] := 1.093364757; GammaY[72,77] := 1.098989456; GammaY[73,77] := 1.104735597; GammaY[74,77] := 1.110613544; GammaY[75,77] := 1.116634804; GammaY[76,77] := 1.122812214; GammaY[77,77] := 1.129160171; GammaY[78,77] := 1.135694909; GammaY[79,77] := 1.142434846;
  GammaY[80,77] := 1.149401019; GammaY[81,77] := 1.156617622; GammaY[82,77] := 1.164112687; GammaY[83,77] := 1.171918984; GammaY[84,77] := 1.18007518; GammaY[85,77] := 1.188627388; GammaY[86,77] := 1.197631253; GammaY[87,77] := 1.207154847; GammaY[88,77] := 1.217282737; GammaY[89,77] := 1.228121918;
  GammaY[90,77] := 1.239810688; GammaY[91,77] := 1.25253244; GammaY[92,77] := 1.266538118; GammaY[93,77] := 1.282184882; GammaY[94,77] := 1.30000764; GammaY[95,77] := 1.320864616; GammaY[96,77] := 1.346276259; GammaY[97,77] := 1.379396482; GammaY[98,77] := 1.429112873; GammaY[99,77] := 1.532654825;
end;
procedure InitGammaY79;
begin
  GammaY[0,78] := 0.609408293; GammaY[1,78] := 0.664298641; GammaY[2,78] := 0.693417585; GammaY[3,78] := 0.713758015; GammaY[4,78] := 0.729884115; GammaY[5,78] := 0.7434664; GammaY[6,78] := 0.755327071; GammaY[7,78] := 0.765937732; GammaY[8,78] := 0.775596252; GammaY[9,78] := 0.784503792;
  GammaY[10,78] := 0.792803132; GammaY[11,78] := 0.800599576; GammaY[12,78] := 0.807973188; GammaY[13,78] := 0.814986394; GammaY[14,78] := 0.821688911; GammaY[15,78] := 0.82812104; GammaY[16,78] := 0.834315938; GammaY[17,78] := 0.84030126; GammaY[18,78] := 0.846100363; GammaY[19,78] := 0.851733171;
  GammaY[20,78] := 0.857216831; GammaY[21,78] := 0.862566238; GammaY[22,78] := 0.867794425; GammaY[23,78] := 0.872912899; GammaY[24,78] := 0.877931879; GammaY[25,78] := 0.882860477; GammaY[26,78] := 0.88770689; GammaY[27,78] := 0.892478516; GammaY[28,78] := 0.897182095; GammaY[29,78] := 0.901823785;
  GammaY[30,78] := 0.906409222; GammaY[31,78] := 0.91094361; GammaY[32,78] := 0.915431763; GammaY[33,78] := 0.919878184; GammaY[34,78] := 0.924287084; GammaY[35,78] := 0.928662404; GammaY[36,78] := 0.93300787; GammaY[37,78] := 0.937327015; GammaY[38,78] := 0.941623193; GammaY[39,78] := 0.945899633;
  GammaY[40,78] := 0.950159444; GammaY[41,78] := 0.954405601; GammaY[42,78] := 0.958640999; GammaY[43,78] := 0.962868447; GammaY[44,78] := 0.967090715; GammaY[45,78] := 0.97131053; GammaY[46,78] := 0.975530558; GammaY[47,78] := 0.979753456; GammaY[48,78] := 0.983981868; GammaY[49,78] := 0.988218425;
  GammaY[50,78] := 0.9924658; GammaY[51,78] := 0.996726696; GammaY[52,78] := 1.001003792; GammaY[53,78] := 1.00529988; GammaY[54,78] := 1.009617817; GammaY[55,78] := 1.013960492; GammaY[56,78] := 1.018330892; GammaY[57,78] := 1.022732111; GammaY[58,78] := 1.027167373; GammaY[59,78] := 1.031640033;
  GammaY[60,78] := 1.036153602; GammaY[61,78] := 1.040711771; GammaY[62,78] := 1.045318428; GammaY[63,78] := 1.049977691; GammaY[64,78] := 1.054693929; GammaY[65,78] := 1.059471809; GammaY[66,78] := 1.064316327; GammaY[67,78] := 1.069232835; GammaY[68,78] := 1.074227109; GammaY[69,78] := 1.079305401;
  GammaY[70,78] := 1.084474502; GammaY[71,78] := 1.089741819; GammaY[72,78] := 1.095115463; GammaY[73,78] := 1.100604356; GammaY[74,78] := 1.106218357; GammaY[75,78] := 1.111968403; GammaY[76,78] := 1.117866694; GammaY[77,78] := 1.123926917; GammaY[78,78] := 1.130164502; GammaY[79,78] := 1.136596953;
  GammaY[80,78] := 1.143244256; GammaY[81,78] := 1.150129393; GammaY[82,78] := 1.157278995; GammaY[83,78] := 1.164724193; GammaY[84,78] := 1.172501716; GammaY[85,78] := 1.18065535; GammaY[86,78] := 1.189237935; GammaY[87,78] := 1.1983141; GammaY[88,78] := 1.207964134; GammaY[89,78] := 1.218289606;
  GammaY[90,78] := 1.229421773; GammaY[91,78] := 1.241534673; GammaY[92,78] := 1.254866421; GammaY[93,78] := 1.269755862; GammaY[94,78] := 1.286710375; GammaY[95,78] := 1.306543855; GammaY[96,78] := 1.330697861; GammaY[97,78] := 1.3621617; GammaY[98,78] := 1.409355207; GammaY[99,78] := 1.507455919;
end;
procedure InitGammaY80;
begin
  GammaY[0,79] := 0.623655486; GammaY[1,79] := 0.676993156; GammaY[2,79] := 0.705226032; GammaY[3,79] := 0.724921885; GammaY[4,79] := 0.740523221; GammaY[5,79] := 0.753654547; GammaY[6,79] := 0.765114937; GammaY[7,79] := 0.775362521; GammaY[8,79] := 0.784686548; GammaY[9,79] := 0.793282294;
  GammaY[10,79] := 0.801288325; GammaY[11,79] := 0.808806806; GammaY[12,79] := 0.815915395; GammaY[13,79] := 0.822674643; GammaY[14,79] := 0.829132756; GammaY[15,79] := 0.835328789; GammaY[16,79] := 0.841294878; GammaY[17,79] := 0.847057834; GammaY[18,79] := 0.852640288; GammaY[19,79] := 0.858061527;
  GammaY[20,79] := 0.863338163; GammaY[21,79] := 0.868484612; GammaY[22,79] := 0.873513507; GammaY[23,79] := 0.878435987; GammaY[24,79] := 0.883261926; GammaY[25,79] := 0.888000148; GammaY[26,79] := 0.892658572; GammaY[27,79] := 0.89724437; GammaY[28,79] := 0.901764061; GammaY[29,79] := 0.90622358;
  GammaY[30,79] := 0.910628379; GammaY[31,79] := 0.914983478; GammaY[32,79] := 0.919293546; GammaY[33,79] := 0.923562929; GammaY[34,79] := 0.927795675; GammaY[35,79] := 0.931995592; GammaY[36,79] := 0.936166276; GammaY[37,79] := 0.94031112; GammaY[38,79] := 0.944433378; GammaY[39,79] := 0.948536165;
  GammaY[40,79] := 0.952622448; GammaY[41,79] := 0.956695102; GammaY[42,79] := 0.9607569; GammaY[43,79] := 0.964810565; GammaY[44,79] := 0.968858762; GammaY[45,79] := 0.972904087; GammaY[46,79] := 0.97694911; GammaY[47,79] := 0.980996379; GammaY[48,79] := 0.985048415; GammaY[49,79] := 0.98910777;
  GammaY[50,79] := 0.993177008; GammaY[51,79] := 0.997258687; GammaY[52,79] := 1.001355376; GammaY[53,79] := 1.005469755; GammaY[54,79] := 1.009604566; GammaY[55,79] := 1.013762555; GammaY[56,79] := 1.017946584; GammaY[57,79] := 1.022159613; GammaY[58,79] := 1.026404712; GammaY[59,79] := 1.030685089;
  GammaY[60,79] := 1.03500409; GammaY[61,79] := 1.039365227; GammaY[62,79] := 1.043772217; GammaY[63,79] := 1.048228983; GammaY[64,79] := 1.052739693; GammaY[65,79] := 1.057308789; GammaY[66,79] := 1.061941024; GammaY[67,79] := 1.066641501; GammaY[68,79] := 1.071415714; GammaY[69,79] := 1.076269619;
  GammaY[70,79] := 1.081209684; GammaY[71,79] := 1.086242951; GammaY[72,79] := 1.091377137; GammaY[73,79] := 1.096620729; GammaY[74,79] := 1.101983102; GammaY[75,79] := 1.107474659; GammaY[76,79] := 1.113107003; GammaY[77,79] := 1.118893149; GammaY[78,79] := 1.12484776; GammaY[79,79] := 1.130987471;
  GammaY[80,79] := 1.137331281; GammaY[81,79] := 1.143901032; GammaY[82,79] := 1.150722032; GammaY[83,79] := 1.157823856; GammaY[84,79] := 1.165241392; GammaY[85,79] := 1.173016242; GammaY[86,79] := 1.181198601; GammaY[87,79] := 1.189849845; GammaY[88,79] := 1.19904622; GammaY[89,79] := 1.208884169;
  GammaY[90,79] := 1.219488315; GammaY[91,79] := 1.231023871; GammaY[92,79] := 1.243716844; GammaY[93,79] := 1.257888809; GammaY[94,79] := 1.274021196; GammaY[95,79] := 1.292886117; GammaY[96,79] := 1.315850798; GammaY[97,79] := 1.345749539; GammaY[98,79] := 1.390561964; GammaY[99,79] := 1.483540561;
end;
procedure InitGammaY81;
begin
  GammaY[0,80] := 0.637488538; GammaY[1,80] := 0.689278137; GammaY[2,80] := 0.716634543; GammaY[3,80] := 0.735695506; GammaY[4,80] := 0.750781373; GammaY[5,80] := 0.763470628; GammaY[6,80] := 0.77453927; GammaY[7,80] := 0.784432004; GammaY[8,80] := 0.793429506; GammaY[9,80] := 0.801721203;
  GammaY[10,80] := 0.809441475; GammaY[11,80] := 0.816689382; GammaY[12,80] := 0.823540198; GammaY[13,80] := 0.830052608; GammaY[14,80] := 0.836273322; GammaY[15,80] := 0.842240176; GammaY[16,80] := 0.847984294; GammaY[17,80] := 0.853531646; GammaY[18,80] := 0.858904142; GammaY[19,80] := 0.864120455;
  GammaY[20,80] := 0.869196655; GammaY[21,80] := 0.874146711; GammaY[22,80] := 0.878982845; GammaY[23,80] := 0.88371582; GammaY[24,80] := 0.888355193; GammaY[25,80] := 0.892909487; GammaY[26,80] := 0.897386376; GammaY[27,80] := 0.901792795; GammaY[28,80] := 0.906135026; GammaY[29,80] := 0.91041881;
  GammaY[30,80] := 0.914649405; GammaY[31,80] := 0.918831676; GammaY[32,80] := 0.922970132; GammaY[33,80] := 0.927068949; GammaY[34,80] := 0.931132041; GammaY[35,80] := 0.935163069; GammaY[36,80] := 0.93916551; GammaY[37,80] := 0.943142657; GammaY[38,80] := 0.947097625; GammaY[39,80] := 0.951033401;
  GammaY[40,80] := 0.954952853; GammaY[41,80] := 0.958858731; GammaY[42,80] := 0.962753728; GammaY[43,80] := 0.966640465; GammaY[44,80] := 0.970521478; GammaY[45,80] := 0.974399269; GammaY[46,80] := 0.978276292; GammaY[47,80] := 0.982155004; GammaY[48,80] := 0.986037854; GammaY[49,80] := 0.989927261;
  GammaY[50,80] := 0.993825667; GammaY[51,80] := 0.997735517; GammaY[52,80] := 1.001659299; GammaY[53,80] := 1.005599581; GammaY[54,80] := 1.009558957; GammaY[55,80] := 1.01354006; GammaY[56,80] := 1.017545631; GammaY[57,80] := 1.021578502; GammaY[58,80] := 1.0256416; GammaY[59,80] := 1.029737982;
  GammaY[60,80] := 1.033870845; GammaY[61,80] := 1.038043545; GammaY[62,80] := 1.042259619; GammaY[63,80] := 1.04652281; GammaY[64,80] := 1.050837093; GammaY[65,80] := 1.055206697; GammaY[66,80] := 1.059636145; GammaY[67,80] := 1.064130302; GammaY[68,80] := 1.068694405; GammaY[69,80] := 1.07333412;
  GammaY[70,80] := 1.078055599; GammaY[71,80] := 1.082865549; GammaY[72,80] := 1.087771311; GammaY[73,80] := 1.092780964; GammaY[74,80] := 1.09790343; GammaY[75,80] := 1.1031486; GammaY[76,80] := 1.108527512; GammaY[77,80] := 1.114052536; GammaY[78,80] := 1.119737622; GammaY[79,80] := 1.125598588;
  GammaY[80,80] := 1.131653495; GammaY[81,80] := 1.137923104; GammaY[82,80] := 1.144431465; GammaY[83,80] := 1.151206691; GammaY[84,80] := 1.15828194; GammaY[85,80] := 1.16569674; GammaY[86,80] := 1.173498774; GammaY[87,80] := 1.181746361; GammaY[88,80] := 1.190511932; GammaY[89,80] := 1.199887085;
  GammaY[90,80] := 1.209990175; GammaY[91,80] := 1.22097809; GammaY[92,80] := 1.23306542; GammaY[93,80] := 1.246557436; GammaY[94,80] := 1.261911092; GammaY[95,80] := 1.279859106; GammaY[96,80] := 1.301698638; GammaY[97,80] := 1.330117939; GammaY[98,80] := 1.372682143; GammaY[99,80] := 1.460836767;
end;
procedure InitGammaY82;
begin
  GammaY[0,81] := 0.650909584; GammaY[1,81] := 0.701160191; GammaY[2,81] := 0.727651502; GammaY[3,81] := 0.746088256; GammaY[4,81] := 0.760668596; GammaY[5,81] := 0.772925121; GammaY[6,81] := 0.783610874; GammaY[7,81] := 0.793157225; GammaY[8,81] := 0.801836343; GammaY[9,81] := 0.80983186;
  GammaY[10,81] := 0.817274015; GammaY[11,81] := 0.82425879; GammaY[12,81] := 0.830859106; GammaY[13,81] := 0.837131804; GammaY[14,81] := 0.843122116; GammaY[15,81] := 0.848866668; GammaY[16,81] := 0.854395605; GammaY[17,81] := 0.85973406; GammaY[18,81] := 0.864903226; GammaY[19,81] := 0.869921167;
  GammaY[20,81] := 0.874803446; GammaY[21,81] := 0.879563572; GammaY[22,81] := 0.884213354; GammaY[23,81] := 0.888763204; GammaY[24,81] := 0.893222352; GammaY[25,81] := 0.897599053; GammaY[26,81] := 0.901900725; GammaY[27,81] := 0.90613405; GammaY[28,81] := 0.910305105; GammaY[29,81] := 0.914419424;
  GammaY[30,81] := 0.918482102; GammaY[31,81] := 0.922497838; GammaY[32,81] := 0.926470963; GammaY[33,81] := 0.930405515; GammaY[34,81] := 0.934305255; GammaY[35,81] := 0.938173737; GammaY[36,81] := 0.942014311; GammaY[37,81] := 0.945830132; GammaY[38,81] := 0.949624207; GammaY[39,81] := 0.953399399;
  GammaY[40,81] := 0.95715848; GammaY[41,81] := 0.960904122; GammaY[42,81] := 0.964638891; GammaY[43,81] := 0.968365292; GammaY[44,81] := 0.972085759; GammaY[45,81] := 0.975802709; GammaY[46,81] := 0.979518523; GammaY[47,81] := 0.983235536; GammaY[48,81] := 0.98695608; GammaY[49,81] := 0.990682472;
  GammaY[50,81] := 0.994417064; GammaY[51,81] := 0.998162211; GammaY[52,81] := 1.0019203; GammaY[53,81] := 1.005693775; GammaY[54,81] := 1.009485098; GammaY[55,81] := 1.013296808; GammaY[56,81] := 1.017131518; GammaY[57,81] := 1.020991928; GammaY[58,81] := 1.02488084; GammaY[59,81] := 1.028801167;
  GammaY[60,81] := 1.032755965; GammaY[61,81] := 1.036748442; GammaY[62,81] := 1.040781966; GammaY[63,81] := 1.044860104; GammaY[64,81] := 1.048986643; GammaY[65,81] := 1.053165617; GammaY[66,81] := 1.057401341; GammaY[67,81] := 1.061698448; GammaY[68,81] := 1.066061921; GammaY[69,81] := 1.070497152;
  GammaY[70,81] := 1.075010003; GammaY[71,81] := 1.07960686; GammaY[72,81] := 1.084294713; GammaY[73,81] := 1.089081247; GammaY[74,81] := 1.093974949; GammaY[75,81] := 1.09898523; GammaY[76,81] := 1.104122594; GammaY[77,81] := 1.109398813; GammaY[78,81] := 1.11482715; GammaY[79,81] := 1.120422648;
  GammaY[80,81] := 1.12620248; GammaY[81,81] := 1.132186389; GammaY[82,81] := 1.138397248; GammaY[83,81] := 1.144861774; GammaY[84,81] := 1.151611483; GammaY[85,81] := 1.158683951; GammaY[86,81] := 1.166124498; GammaY[87,81] := 1.17398854; GammaY[88,81] := 1.182344895; GammaY[89,81] := 1.1912806;
  GammaY[90,81] := 1.200908091; GammaY[91,81] := 1.21137639; GammaY[92,81] := 1.222889301; GammaY[93,81] := 1.235736717; GammaY[94,81] := 1.250352497; GammaY[95,81] := 1.26743219; GammaY[96,81] := 1.288206882; GammaY[97,81] := 1.315227159; GammaY[98,81] := 1.355667685; GammaY[99,81] := 1.439276998;
end;
procedure InitGammaY83;
begin
  GammaY[0,82] := 0.66392189; GammaY[1,82] := 0.712646529; GammaY[2,82] := 0.738285674; GammaY[3,82] := 0.756109742; GammaY[4,82] := 0.770195033; GammaY[5,82] := 0.782028538; GammaY[6,82] := 0.792340517; GammaY[7,82] := 0.801549133; GammaY[8,82] := 0.80991813; GammaY[9,82] := 0.817625421;
  GammaY[10,82] := 0.824797152; GammaY[11,82] := 0.831526253; GammaY[12,82] := 0.837883341; GammaY[13,82] := 0.843923437; GammaY[14,82] := 0.849690301; GammaY[15,82] := 0.855219393; GammaY[16,82] := 0.860539877; GammaY[17,82] := 0.865676057; GammaY[18,82] := 0.870648427; GammaY[19,82] := 0.875474472;
  GammaY[20,82] := 0.880169238; GammaY[21,82] := 0.884745774; GammaY[22,82] := 0.8892155; GammaY[23,82] := 0.89358847; GammaY[24,82] := 0.897873619; GammaY[25,82] := 0.902078924; GammaY[26,82] := 0.906211534; GammaY[27,82] := 0.9102779; GammaY[28,82] := 0.914283898; GammaY[29,82] := 0.918234888;
  GammaY[30,82] := 0.922135773; GammaY[31,82] := 0.925991075; GammaY[32,82] := 0.92980497; GammaY[33,82] := 0.933581362; GammaY[34,82] := 0.9373239; GammaY[35,82] := 0.941035988; GammaY[36,82] := 0.944720844; GammaY[37,82] := 0.948381503; GammaY[38,82] := 0.952020873; GammaY[39,82] := 0.955641732;
  GammaY[40,82] := 0.959246727; GammaY[41,82] := 0.962838415; GammaY[42,82] := 0.96641926; GammaY[43,82] := 0.969991681; GammaY[44,82] := 0.973558044; GammaY[45,82] := 0.977120644; GammaY[46,82] := 0.980681754; GammaY[47,82] := 0.984243611; GammaY[48,82] := 0.987808465; GammaY[49,82] := 0.991378564;
  GammaY[50,82] := 0.994956138; GammaY[51,82] := 0.998543409; GammaY[52,82] := 1.002142693; GammaY[53,82] := 1.005756343; GammaY[54,82] := 1.009386699; GammaY[55,82] := 1.013036188; GammaY[56,82] := 1.016707311; GammaY[57,82] := 1.020402644; GammaY[58,82] := 1.024124857; GammaY[59,82] := 1.027876741;
  GammaY[60,82] := 1.03166121; GammaY[61,82] := 1.035481319; GammaY[62,82] := 1.039340286; GammaY[63,82] := 1.043241509; GammaY[64,82] := 1.047188603; GammaY[65,82] := 1.05118542; GammaY[66,82] := 1.055236063; GammaY[67,82] := 1.059344946; GammaY[68,82] := 1.06351682; GammaY[69,82] := 1.067756824;
  GammaY[70,82] := 1.072070536; GammaY[71,82] := 1.07646404; GammaY[72,82] := 1.080943988; GammaY[73,82] := 1.085517689; GammaY[74,82] := 1.090193227; GammaY[75,82] := 1.094979562; GammaY[76,82] := 1.099886685; GammaY[77,82] := 1.104925793; GammaY[78,82] := 1.110109504; GammaY[79,82] := 1.115452133;
  GammaY[80,82] := 1.120970019; GammaY[81,82] := 1.126681933; GammaY[82,82] := 1.132609623; GammaY[83,82] := 1.138778502; GammaY[84,82] := 1.145218541; GammaY[85,82] := 1.151965455; GammaY[86,82] := 1.159062326; GammaY[87,82] := 1.166561835; GammaY[88,82] := 1.174529393; GammaY[89,82] := 1.183047713;
  GammaY[90,82] := 1.192223635; GammaY[91,82] := 1.202198757; GammaY[92,82] := 1.213166703; GammaY[93,82] := 1.225402835; GammaY[94,82] := 1.239319212; GammaY[95,82] := 1.255576308; GammaY[96,82] := 1.275342869; GammaY[97,82] := 1.301039646; GammaY[98,82] := 1.339473299; GammaY[99,82] := 1.418797966;
end;
procedure InitGammaY84;
begin
  GammaY[0,83] := 0.676529721; GammaY[1,83] := 0.723744892; GammaY[2,83] := 0.74854612; GammaY[3,83] := 0.76576973; GammaY[4,83] := 0.779370893; GammaY[5,83] := 0.790791371; GammaY[6,83] := 0.80073889; GammaY[7,83] := 0.809618554; GammaY[8,83] := 0.817685783; GammaY[9,83] := 0.825112846;
  GammaY[10,83] := 0.832021848; GammaY[11,83] := 0.838502738; GammaY[12,83] := 0.844623847; GammaY[13,83] := 0.850438398; GammaY[14,83] := 0.855988723; GammaY[15,83] := 0.861309118; GammaY[16,83] := 0.866427788; GammaY[17,83] := 0.871368222; GammaY[18,83] := 0.876150246; GammaY[19,83] := 0.880790761;
  GammaY[20,83] := 0.885304296; GammaY[21,83] := 0.889703459; GammaY[22,83] := 0.893999289; GammaY[23,83] := 0.898201516; GammaY[24,83] := 0.902318754; GammaY[25,83] := 0.90635869; GammaY[26,83] := 0.910328242; GammaY[27,83] := 0.914233655; GammaY[28,83] := 0.918080583; GammaY[29,83] := 0.921874192;
  GammaY[30,83] := 0.925619208; GammaY[31,83] := 0.929320002; GammaY[32,83] := 0.932980621; GammaY[33,83] := 0.936604811; GammaY[34,83] := 0.940196078; GammaY[35,83] := 0.9437577; GammaY[36,83] := 0.947292787; GammaY[37,83] := 0.950804283; GammaY[38,83] := 0.954294968; GammaY[39,83] := 0.957767505;
  GammaY[40,83] := 0.961224435; GammaY[41,83] := 0.964668229; GammaY[42,83] := 0.96810128; GammaY[43,83] := 0.971525892; GammaY[44,83] := 0.974944323; GammaY[45,83] := 0.978358773; GammaY[46,83] := 0.981771438; GammaY[47,83] := 0.985184489; GammaY[48,83] := 0.98860006; GammaY[49,83] := 0.992020277;
  GammaY[50,83] := 0.995447301; GammaY[51,83] := 0.998883286; GammaY[52,83] := 1.002330424; GammaY[53,83] := 1.005790954; GammaY[54,83] := 1.009267128; GammaY[55,83] := 1.012761272; GammaY[56,83] := 1.016275768; GammaY[57,83] := 1.019813079; GammaY[58,83] := 1.023375763; GammaY[59,83] := 1.026966479;
  GammaY[60,83] := 1.030588011; GammaY[61,83] := 1.034243272; GammaY[62,83] := 1.037935326; GammaY[63,83] := 1.041667425; GammaY[64,83] := 1.045443012; GammaY[65,83] := 1.049265753; GammaY[66,83] := 1.053139566; GammaY[67,83] := 1.057068651; GammaY[68,83] := 1.061057543; GammaY[69,83] := 1.065111139;
  GammaY[70,83] := 1.06923475; GammaY[71,83] := 1.073434164; GammaY[72,83] := 1.077715721; GammaY[73,83] := 1.082086383; GammaY[74,83] := 1.086553846; GammaY[75,83] := 1.091126634; GammaY[76,83] := 1.095814253; GammaY[77,83] := 1.100627363; GammaY[78,83] := 1.105577976; GammaY[79,83] := 1.110679707;
  GammaY[80,83] := 1.115948095; GammaY[81,83] := 1.121401006; GammaY[82,83] := 1.127059131; GammaY[83,83] := 1.132946636; GammaY[84,83] := 1.139092024; GammaY[85,83] := 1.145529262; GammaY[86,83] := 1.152299324; GammaY[87,83] := 1.15945229; GammaY[88,83] := 1.167050347; GammaY[89,83] := 1.175172126;
  GammaY[90,83] := 1.183919184; GammaY[91,83] := 1.193426095; GammaY[92,83] := 1.203876857; GammaY[93,83] := 1.21553312; GammaY[94,83] := 1.228786343; GammaY[95,83] := 1.244263887; GammaY[96,83] := 1.263075657; GammaY[97,83] := 1.287519905; GammaY[98,83] := 1.324056287; GammaY[99,83] := 1.399340148;
end;
procedure InitGammaY85;
begin
  GammaY[0,84] := 0.688738201; GammaY[1,84] := 0.734463436; GammaY[2,84] := 0.758442129; GammaY[3,84] := 0.77507809; GammaY[4,84] := 0.788206396; GammaY[5,84] := 0.799224078; GammaY[6,84] := 0.808816589; GammaY[7,84] := 0.817376153; GammaY[8,84] := 0.825150008; GammaY[9,84] := 0.832304858;
  GammaY[10,84] := 0.838958828; GammaY[11,84] := 0.845198929; GammaY[12,84] := 0.851091251; GammaY[13,84] := 0.856687248; GammaY[14,84] := 0.862027862; GammaY[15,84] := 0.867146247; GammaY[16,84] := 0.872069643; GammaY[17,84] := 0.87682077; GammaY[18,84] := 0.881418788; GammaY[19,84] := 0.88588001;
  GammaY[20,84] := 0.890218472; GammaY[21,84] := 0.894446363; GammaY[22,84] := 0.898574348; GammaY[23,84] := 0.902611806; GammaY[24,84] := 0.906567052; GammaY[25,84] := 0.910447522; GammaY[26,84] := 0.914259893; GammaY[27,84] := 0.918010177; GammaY[28,84] := 0.921703826; GammaY[29,84] := 0.925345832;
  GammaY[30,84] := 0.92894077; GammaY[31,84] := 0.932492839; GammaY[32,84] := 0.936005931; GammaY[33,84] := 0.939483653; GammaY[34,84] := 0.942929392; GammaY[35,84] := 0.946346329; GammaY[36,84] := 0.949737437; GammaY[37,84] := 0.953105531; GammaY[38,84] := 0.956453304; GammaY[39,84] := 0.959783328;
  GammaY[40,84] := 0.963098051; GammaY[41,84] := 0.966399837; GammaY[42,84] := 0.969690962; GammaY[43,84] := 0.97297366; GammaY[44,84] := 0.976250112; GammaY[45,84] := 0.979522433; GammaY[46,84] := 0.982792717; GammaY[47,84] := 0.98606302; GammaY[48,84] := 0.989335412; GammaY[49,84] := 0.992611959;
  GammaY[50,84] := 0.995894705; GammaY[51,84] := 0.999185692; GammaY[52,84] := 1.002487037; GammaY[53,84] := 1.005800896; GammaY[54,84] := 1.00912941; GammaY[55,84] := 1.012474799; GammaY[56,84] := 1.015839346; GammaY[57,84] := 1.0192254; GammaY[58,84] := 1.022635409; GammaY[59,84] := 1.026071917;
  GammaY[60,84] := 1.029537574; GammaY[61,84] := 1.033035163; GammaY[62,84] := 1.036567612; GammaY[63,84] := 1.040138025; GammaY[64,84] := 1.043749677; GammaY[65,84] := 1.047406061; GammaY[66,84] := 1.051110919; GammaY[67,84] := 1.054868257; GammaY[68,84] := 1.058682392; GammaY[69,84] := 1.062557987;
  GammaY[70,84] := 1.066500102; GammaY[71,84] := 1.070514257; GammaY[72,84] := 1.074606491; GammaY[73,84] := 1.078783434; GammaY[74,84] := 1.083052406; GammaY[75,84] := 1.087421529; GammaY[76,84] := 1.091899856; GammaY[77,84] := 1.096497534; GammaY[78,84] := 1.101225991; GammaY[79,84] := 1.106098182;
  GammaY[80,84] := 1.111128902; GammaY[81,84] := 1.116335147; GammaY[82,84] := 1.121736602; GammaY[83,84] := 1.127356258; GammaY[84,84] := 1.133221233; GammaY[85,84] := 1.139363842; GammaY[86,84] := 1.145823049; GammaY[87,84] := 1.152646488; GammaY[88,84] := 1.159893301; GammaY[89,84] := 1.16763825;
  GammaY[90,84] := 1.175977892; GammaY[91,84] := 1.185040167; GammaY[92,84] := 1.194999973; GammaY[93,84] := 1.206105992; GammaY[94,84] := 1.218730233; GammaY[95,84] := 1.233468769; GammaY[96,84] := 1.251375943; GammaY[97,84] := 1.274634384; GammaY[98,84] := 1.30937639; GammaY[99,84] := 1.380847638;
end;
procedure InitGammaY86;
begin
  GammaY[0,85] := 0.70055321; GammaY[1,85] := 0.744810675; GammaY[2,85] := 0.767983157; GammaY[3,85] := 0.784044761; GammaY[4,85] := 0.796711752; GammaY[5,85] := 0.807337017; GammaY[6,85] := 0.81658406; GammaY[7,85] := 0.824832434; GammaY[8,85] := 0.832321321; GammaY[9,85] := 0.839211939;
  GammaY[10,85] := 0.845618529; GammaY[11,85] := 0.851625212; GammaY[12,85] := 0.857295865; GammaY[13,85] := 0.862680237; GammaY[14,85] := 0.867817884; GammaY[15,85] := 0.872740825; GammaY[16,85] := 0.877475396; GammaY[17,85] := 0.882043544; GammaY[18,85] := 0.886463754; GammaY[19,85] := 0.890751794;
  GammaY[20,85] := 0.89492123; GammaY[21,85] := 0.898983819; GammaY[22,85] := 0.902949839; GammaY[23,85] := 0.906828356; GammaY[24,85] := 0.910627411; GammaY[25,85] := 0.914354167; GammaY[26,85] := 0.918015046; GammaY[27,85] := 0.921615863; GammaY[28,85] := 0.925161894; GammaY[29,85] := 0.928657937;
  GammaY[30,85] := 0.932108384; GammaY[31,85] := 0.935517299; GammaY[32,85] := 0.93888844; GammaY[33,85] := 0.942225287; GammaY[34,85] := 0.945531092; GammaY[35,85] := 0.948808896; GammaY[36,85] := 0.952061582; GammaY[37,85] := 0.955291878; GammaY[38,85] := 0.958502357; GammaY[39,85] := 0.961695473;
  GammaY[40,85] := 0.964873597; GammaY[41,85] := 0.968039013; GammaY[42,85] := 0.97119391; GammaY[43,85] := 0.974340423; GammaY[44,85] := 0.977480622; GammaY[45,85] := 0.980616559; GammaY[46,85] := 0.983750255; GammaY[47,85] := 0.986883685; GammaY[48,85] := 0.99001882; GammaY[49,85] := 0.993157614;
  GammaY[50,85] := 0.996302049; GammaY[51,85] := 0.999454093; GammaY[52,85] := 1.002615764; GammaY[53,85] := 1.005789125; GammaY[54,85] := 1.008976228; GammaY[55,85] := 1.012179188; GammaY[56,85] := 1.015400183; GammaY[57,85] := 1.018641469; GammaY[58,85] := 1.021905379; GammaY[59,85] := 1.02519434;
  GammaY[60,85] := 1.02851088; GammaY[61,85] := 1.031857663; GammaY[62,85] := 1.035237489; GammaY[63,85] := 1.038653306; GammaY[64,85] := 1.042108247; GammaY[65,85] := 1.045605646; GammaY[66,85] := 1.049149065; GammaY[67,85] := 1.052742328; GammaY[68,85] := 1.056389545; GammaY[69,85] := 1.060095157;
  GammaY[70,85] := 1.063863992; GammaY[71,85] := 1.067701307; GammaY[72,85] := 1.071612851; GammaY[73,85] := 1.075604938; GammaY[74,85] := 1.079684546; GammaY[75,85] := 1.083859414; GammaY[76,85] := 1.088138157; GammaY[77,85] := 1.092530432; GammaY[78,85] := 1.097047122; GammaY[79,85] := 1.101700569;
  GammaY[80,85] := 1.106504844; GammaY[81,85] := 1.111476122; GammaY[82,85] := 1.116633141; GammaY[83,85] := 1.121997782; GammaY[84,85] := 1.127595847; GammaY[85,85] := 1.133458076; GammaY[86,85] := 1.139621545; GammaY[87,85] := 1.146131571; GammaY[88,85] := 1.153044404; GammaY[89,85] := 1.160431158;
  GammaY[90,85] := 1.168383661; GammaY[91,85] := 1.177023567; GammaY[92,85] := 1.18651718; GammaY[93,85] := 1.197100916; GammaY[94,85] := 1.209128393; GammaY[95,85] := 1.223166124; GammaY[96,85] := 1.240215963; GammaY[97,85] := 1.262351349; GammaY[98,85] := 1.295395624; GammaY[99,85] := 1.363268151;
end;
procedure InitGammaY87;
begin
  GammaY[0,86] := 0.711981261; GammaY[1,86] := 0.754795389; GammaY[2,86] := 0.777178772; GammaY[3,86] := 0.792679677; GammaY[4,86] := 0.804897094; GammaY[5,86] := 0.815140431; GammaY[6,86] := 0.824051607; GammaY[7,86] := 0.831997704; GammaY[8,86] := 0.839209991; GammaY[9,86] := 0.84584433;
  GammaY[10,86] := 0.852011134; GammaY[11,86] := 0.857791691; GammaY[12,86] := 0.863247727; GammaY[13,86] := 0.868427287; GammaY[14,86] := 0.873368587; GammaY[15,86] := 0.878102565; GammaY[16,86] := 0.882654642; GammaY[17,86] := 0.887045995; GammaY[18,86] := 0.89129449; GammaY[19,86] := 0.895415348;
  GammaY[20,86] := 0.899421643; GammaY[21,86] := 0.903324734; GammaY[22,86] := 0.907134553; GammaY[23,86] := 0.910859835; GammaY[24,86] := 0.914508321; GammaY[25,86] := 0.918086933; GammaY[26,86] := 0.921601883; GammaY[27,86] := 0.925058761; GammaY[28,86] := 0.928462639; GammaY[29,86] := 0.931818155;
  GammaY[30,86] := 0.935129562; GammaY[31,86] := 0.938400757; GammaY[32,86] := 0.941635346; GammaY[33,86] := 0.944836692; GammaY[34,86] := 0.948007941; GammaY[35,86] := 0.951152019; GammaY[36,86] := 0.954271693; GammaY[37,86] := 0.957369566; GammaY[38,86] := 0.960448134; GammaY[39,86] := 0.963509775;
  GammaY[40,86] := 0.966556752; GammaY[41,86] := 0.969591237; GammaY[42,86] := 0.972615352; GammaY[43,86] := 0.975631159; GammaY[44,86] := 0.978640648; GammaY[45,86] := 0.981645778; GammaY[46,86] := 0.984648461; GammaY[47,86] := 0.987650614; GammaY[48,86] := 0.990654141; GammaY[49,86] := 0.993660916;
  GammaY[50,86] := 0.996672811; GammaY[51,86] := 0.999691709; GammaY[52,86] := 1.002719569; GammaY[53,86] := 1.005758355; GammaY[54,86] := 1.008810021; GammaY[55,86] := 1.011876597; GammaY[56,86] := 1.01496017; GammaY[57,86] := 1.01806289; GammaY[58,86] := 1.021186991; GammaY[59,86] := 1.024334785;
  GammaY[60,86] := 1.027508688; GammaY[61,86] := 1.030711241; GammaY[62,86] := 1.033945114; GammaY[63,86] := 1.037213125; GammaY[64,86] := 1.040518258; GammaY[65,86] := 1.0438637; GammaY[66,86] := 1.04725285; GammaY[67,86] := 1.050689348; GammaY[68,86] := 1.054177114; GammaY[69,86] := 1.057720386;
  GammaY[70,86] := 1.061323765; GammaY[71,86] := 1.064992256; GammaY[72,86] := 1.068731332; GammaY[73,86] := 1.072547018; GammaY[74,86] := 1.076445957; GammaY[75,86] := 1.080435516; GammaY[76,86] := 1.0845239; GammaY[77,86] := 1.088720313; GammaY[78,86] := 1.093035119; GammaY[79,86] := 1.097480062;
  GammaY[80,86] := 1.102068544; GammaY[81,86] := 1.106815968; GammaY[82,86] := 1.111740163; GammaY[83,86] := 1.116861955; GammaY[84,86] := 1.122205903; GammaY[85,86] := 1.127801265; GammaY[86,86] := 1.133683318; GammaY[87,86] := 1.139895182; GammaY[88,86] := 1.146490383; GammaY[89,86] := 1.15353657;
  GammaY[90,86] := 1.1611211; GammaY[91,86] := 1.169359688; GammaY[92,86] := 1.178410493; GammaY[93,86] := 1.18849834; GammaY[94,86] := 1.199959451; GammaY[95,86] := 1.213332392; GammaY[96,86] := 1.229569412; GammaY[97,86] := 1.25064079; GammaY[98,86] := 1.282078153; GammaY[99,86] := 1.346552253;
end;

procedure InitGammaY88;
begin
  GammaY[0,87] := 0.723029415; GammaY[1,87] := 0.764426565; GammaY[2,87] := 0.786038596; GammaY[3,87] := 0.80099275; GammaY[4,87] := 0.812772482; GammaY[5,87] := 0.822644435; GammaY[6,87] := 0.831229339; GammaY[7,87] := 0.838882046; GammaY[8,87] := 0.845826069; GammaY[9,87] := 0.852212011;
  GammaY[10,87] := 0.85814654; GammaY[11,87] := 0.863708179; GammaY[12,87] := 0.868956528; GammaY[13,87] := 0.873937994; GammaY[14,87] := 0.878689478; GammaY[15,87] := 0.88324083; GammaY[16,87] := 0.8876166; GammaY[17,87] := 0.891837239; GammaY[18,87] := 0.89591997; GammaY[19,87] := 0.899879481;
  GammaY[20,87] := 0.903728405; GammaY[21,87] := 0.907477688; GammaY[22,87] := 0.911136896; GammaY[23,87] := 0.914714466; GammaY[24,87] := 0.918217881; GammaY[25,87] := 0.921653798; GammaY[26,87] := 0.925028191; GammaY[27,87] := 0.928346464; GammaY[28,87] := 0.931613521; GammaY[29,87] := 0.934833818;
  GammaY[30,87] := 0.938011431; GammaY[31,87] := 0.941150134; GammaY[32,87] := 0.944253416; GammaY[33,87] := 0.947324501; GammaY[34,87] := 0.950366394; GammaY[35,87] := 0.953381933; GammaY[36,87] := 0.956373788; GammaY[37,87] := 0.959344466; GammaY[38,87] := 0.962296346; GammaY[39,87] := 0.965231714;
  GammaY[40,87] := 0.968152764; GammaY[41,87] := 0.971061587; GammaY[42,87] := 0.973960198; GammaY[43,87] := 0.976850578; GammaY[44,87] := 0.979734657; GammaY[45,87] := 0.982614304; GammaY[46,87] := 0.985491369; GammaY[47,87] := 0.988367663; GammaY[48,87] := 0.991245011; GammaY[49,87] := 0.994125233;
  GammaY[50,87] := 0.997010126; GammaY[51,87] := 0.999901474; GammaY[52,87] := 1.002801142; GammaY[53,87] := 1.005711026; GammaY[54,87] := 1.008632995; GammaY[55,87] := 1.011568984; GammaY[56,87] := 1.014520994; GammaY[57,87] := 1.017491087; GammaY[58,87] := 1.020481385; GammaY[59,87] := 1.023494102;
  GammaY[60,87] := 1.026531551; GammaY[61,87] := 1.029596156; GammaY[62,87] := 1.032690461; GammaY[63,87] := 1.035817151; GammaY[64,87] := 1.038979082; GammaY[65,87] := 1.04217929; GammaY[66,87] := 1.045421015; GammaY[67,87] := 1.048707727; GammaY[68,87] := 1.052043169; GammaY[69,87] := 1.055431386;
  GammaY[70,87] := 1.058876755; GammaY[71,87] := 1.06238405; GammaY[72,87] := 1.065958492; GammaY[73,87] := 1.06960582; GammaY[74,87] := 1.073332359; GammaY[75,87] := 1.077145125; GammaY[76,87] := 1.081051941; GammaY[77,87] := 1.085061569; GammaY[78,87] := 1.089183876; GammaY[79,87] := 1.093430053;
  GammaY[80,87] := 1.097812866; GammaY[81,87] := 1.102346974; GammaY[82,87] := 1.107049357; GammaY[83,87] := 1.111939847; GammaY[84,87] := 1.117041817; GammaY[85,87] := 1.122383115; GammaY[86,87] := 1.127997325; GammaY[87,87] := 1.133925483; GammaY[88,87] := 1.140218523; GammaY[89,87] := 1.146940826;
  GammaY[90,87] := 1.154175513; GammaY[91,87] := 1.162032674; GammaY[92,87] := 1.170662773; GammaY[93,87] := 1.180279655; GammaY[94,87] := 1.191203089; GammaY[95,87] := 1.203945207; GammaY[96,87] := 1.219411352; GammaY[97,87] := 1.239474314; GammaY[98,87] := 1.269390151; GammaY[99,87] := 1.330653628;
end;
procedure InitGammaY89;
begin
  GammaY[0,88] := 0.73370519; GammaY[1,88] := 0.773713355; GammaY[2,88] := 0.794572292; GammaY[3,88] := 0.808993835; GammaY[4,88] := 0.820347842; GammaY[5,88] := 0.82985898; GammaY[6,88] := 0.838127183; GammaY[7,88] := 0.845495337; GammaY[8,88] := 0.852179355; GammaY[9,88] := 0.858324696;
  GammaY[10,88] := 0.864034369; GammaY[11,88] := 0.869384177; GammaY[12,88] := 0.874431669; GammaY[13,88] := 0.879221638; GammaY[14,88] := 0.88378969; GammaY[15,88] := 0.888164648; GammaY[16,88] := 0.892370183; GammaY[17,88] := 0.896426028; GammaY[18,88] := 0.900348814; GammaY[19,88] := 0.904152698;
  GammaY[20,88] := 0.90784985; GammaY[21,88] := 0.911450839; GammaY[22,88] := 0.914964903; GammaY[23,88] := 0.918400158; GammaY[24,88] := 0.921763814; GammaY[25,88] := 0.925062303; GammaY[26,88] := 0.928301385; GammaY[27,88] := 0.931486251; GammaY[28,88] := 0.934621626; GammaY[29,88] := 0.937711815;
  GammaY[30,88] := 0.940760751; GammaY[31,88] := 0.943772047; GammaY[32,88] := 0.946749063; GammaY[33,88] := 0.949694919; GammaY[34,88] := 0.95261251; GammaY[35,88] := 0.955504547; GammaY[36,88] := 0.958373598; GammaY[37,88] := 0.961222094; GammaY[38,88] := 0.964052321; GammaY[39,88] := 0.96686646;
  GammaY[40,88] := 0.969666617; GammaY[41,88] := 0.972454821; GammaY[42,88] := 0.975233008; GammaY[43,88] := 0.97800306; GammaY[44,88] := 0.980766828; GammaY[45,88] := 0.983526127; GammaY[46,88] := 0.986282721; GammaY[47,88] := 0.989038347; GammaY[48,88] := 0.991794755; GammaY[49,88] := 0.994553687;
  GammaY[50,88] := 0.997316866; GammaY[51,88] := 1.000085995; GammaY[52,88] := 1.002862866; GammaY[53,88] := 1.005649303; GammaY[54,88] := 1.008447081; GammaY[55,88] := 1.011258051; GammaY[56,88] := 1.014084128; GammaY[57,88] := 1.016927276; GammaY[58,88] := 1.019789531; GammaY[59,88] := 1.022673003;
  GammaY[60,88] := 1.025579905; GammaY[61,88] := 1.028512555; GammaY[62,88] := 1.031473378; GammaY[63,88] := 1.034464944; GammaY[64,88] := 1.037489977; GammaY[65,88] := 1.040551366; GammaY[66,88] := 1.0436522; GammaY[67,88] := 1.046795797; GammaY[68,88] := 1.049985723; GammaY[69,88] := 1.05322583;
  GammaY[70,88] := 1.056520294; GammaY[71,88] := 1.059873668; GammaY[72,88] := 1.063290934; GammaY[73,88] := 1.066777553; GammaY[74,88] := 1.070339557; GammaY[75,88] := 1.073983637; GammaY[76,88] := 1.077717241; GammaY[77,88] := 1.08154871; GammaY[78,88] := 1.085487453; GammaY[79,88] := 1.089544125;
  GammaY[80,88] := 1.093730879; GammaY[81,88] := 1.098061686; GammaY[82,88] := 1.102552718; GammaY[83,88] := 1.107222853; GammaY[84,88] := 1.112094351; GammaY[85,88] := 1.117193732; GammaY[86,88] := 1.122552961; GammaY[87,88] := 1.128211102; GammaY[88,88] := 1.134216643; GammaY[89,88] := 1.140630859;
  GammaY[90,88] := 1.147532857; GammaY[91,88] := 1.155027412; GammaY[92,88] := 1.16325769; GammaY[93,88] := 1.172427151; GammaY[94,88] := 1.182839991; GammaY[95,88] := 1.194983332; GammaY[96,88] := 1.209718152; GammaY[97,88] := 1.228825054; GammaY[98,88] := 1.257299679; GammaY[99,88] := 1.315528748;
end;
procedure InitGammaY90;
begin
  GammaY[0,89] := 0.744016487; GammaY[1,89] := 0.782664999; GammaY[2,89] := 0.802789486; GammaY[3,89] := 0.816692702; GammaY[4,89] := 0.827632971; GammaY[5,89] := 0.836793827; GammaY[6,89] := 0.844754856; GammaY[7,89] := 0.851847214; GammaY[8,89] := 0.858279395; GammaY[9,89] := 0.864191826;
  GammaY[10,89] := 0.869683938; GammaY[11,89] := 0.874828904; GammaY[12,89] := 0.879682244; GammaY[13,89] := 0.884287181; GammaY[14,89] := 0.88867807; GammaY[15,89] := 0.892882708; GammaY[16,89] := 0.896923944; GammaY[17,89] := 0.900820792; GammaY[18,89] := 0.904589291; GammaY[19,89] := 0.908243102;
  GammaY[20,89] := 0.911793961; GammaY[21,89] := 0.915252043; GammaY[22,89] := 0.918626253; GammaY[23,89] := 0.921924432; GammaY[24,89] := 0.925153511; GammaY[25,89] := 0.928319686; GammaY[26,89] := 0.93142852; GammaY[27,89] := 0.934485016; GammaY[28,89] := 0.937493708; GammaY[29,89] := 0.940458744;
  GammaY[30,89] := 0.943383924; GammaY[31,89] := 0.94627273; GammaY[32,89] := 0.949128378; GammaY[33,89] := 0.951953871; GammaY[34,89] := 0.954752011; GammaY[35,89] := 0.957525407; GammaY[36,89] := 0.960276511; GammaY[37,89] := 0.96300766; GammaY[38,89] := 0.96572107; GammaY[39,89] := 0.968418839;
  GammaY[40,89] := 0.97110297; GammaY[41,89] := 0.97377541; GammaY[42,89] := 0.976438042; GammaY[43,89] := 0.979092671; GammaY[44,89] := 0.981741053; GammaY[45,89] := 0.984384928; GammaY[46,89] := 0.98702601; GammaY[47,89] := 0.989665967; GammaY[48,89] := 0.992306452; GammaY[49,89] := 0.994949134;
  GammaY[50,89] := 0.997595686; GammaY[51,89] := 1.000247726; GammaY[52,89] := 1.002906971; GammaY[53,89] := 1.005575166; GammaY[54,89] := 1.008254011; GammaY[55,89] := 1.010945284; GammaY[56,89] := 1.013650806; GammaY[57,89] := 1.016372462; GammaY[58,89] := 1.019112191; GammaY[59,89] := 1.021872009;
  GammaY[60,89] := 1.024654031; GammaY[61,89] := 1.027460465; GammaY[62,89] := 1.03029363; GammaY[63,89] := 1.033155982; GammaY[64,89] := 1.036050122; GammaY[65,89] := 1.038978808; GammaY[66,89] := 1.041944989; GammaY[67,89] := 1.044951831; GammaY[68,89] := 1.048002729; GammaY[69,89] := 1.051101354;
  GammaY[70,89] := 1.054251695; GammaY[71,89] := 1.05745809; GammaY[72,89] := 1.060725285; GammaY[73,89] := 1.064058487; GammaY[74,89] := 1.067463448; GammaY[75,89] := 1.070946547; GammaY[76,89] := 1.074514879; GammaY[77,89] := 1.078176399; GammaY[78,89] := 1.081940067; GammaY[79,89] := 1.085816033;
  GammaY[80,89] := 1.089815874; GammaY[81,89] := 1.093952898; GammaY[82,89] := 1.098242505; GammaY[83,89] := 1.102702684; GammaY[84,89] := 1.107354635; GammaY[85,89] := 1.112223608; GammaY[86,89] := 1.11734005; GammaY[87,89] := 1.122741152; GammaY[88,89] := 1.128473078; GammaY[89,89] := 1.134594165;
  GammaY[90,89] := 1.141179717; GammaY[91,89] := 1.148329464; GammaY[92,89] := 1.156179671; GammaY[93,89] := 1.164923962; GammaY[94,89] := 1.174851792; GammaY[95,89] := 1.186426608; GammaY[96,89] := 1.200467401; GammaY[97,89] := 1.218667577; GammaY[98,89] := 1.245776573; GammaY[99,89] := 1.301136553;
end;
procedure InitGammaY91;
begin
  GammaY[0,90] := 0.753971509; GammaY[1,90] := 0.791290802; GammaY[2,90] := 0.810699752; GammaY[3,90] := 0.824098987; GammaY[4,90] := 0.834637506; GammaY[5,90] := 0.843458569; GammaY[6,90] := 0.851121854; GammaY[7,90] := 0.857947078; GammaY[8,90] := 0.864135478; GammaY[9,90] := 0.869822576;
  GammaY[10,90] := 0.875104311; GammaY[11,90] := 0.880051275; GammaY[12,90] := 0.884717038; GammaY[13,90] := 0.889143278; GammaY[14,90] := 0.893363128; GammaY[15,90] := 0.897403397; GammaY[16,90] := 0.90128611; GammaY[17,90] := 0.905029615; GammaY[18,90] := 0.908649356; GammaY[19,90] := 0.912158503;
  GammaY[20,90] := 0.915568379; GammaY[21,90] := 0.918888789; GammaY[22,90] := 0.922128303; GammaY[23,90] := 0.925294475; GammaY[24,90] := 0.928394002; GammaY[25,90] := 0.931432835; GammaY[26,90] := 0.934416329; GammaY[27,90] := 0.937349317; GammaY[28,90] := 0.940236167; GammaY[29,90] := 0.943080856;
  GammaY[30,90] := 0.945887043; GammaY[31,90] := 0.948658094; GammaY[32,90] := 0.951397097; GammaY[33,90] := 0.954106941; GammaY[34,90] := 0.956790322; GammaY[35,90] := 0.959449749; GammaY[36,90] := 0.96208758; GammaY[37,90] := 0.964706061; GammaY[38,90] := 0.967307324; GammaY[39,90] := 0.969893382;
  GammaY[40,90] := 0.972466162; GammaY[41,90] := 0.975027534; GammaY[42,90] := 0.977579304; GammaY[43,90] := 0.980123205; GammaY[44,90] := 0.982660924; GammaY[45,90] := 0.985194129; GammaY[46,90] := 0.987724464; GammaY[47,90] := 0.990253529; GammaY[48,90] := 0.992782906; GammaY[49,90] := 0.995314197;
  GammaY[50,90] := 0.997849003; GammaY[51,90] := 1.000388881; GammaY[52,90] := 1.002935465; GammaY[53,90] := 1.005490421; GammaY[54,90] := 1.008055384; GammaY[55,90] := 1.010632046; GammaY[56,90] := 1.013222159; GammaY[57,90] := 1.015827522; GammaY[58,90] := 1.018449989; GammaY[59,90] := 1.02109149;
  GammaY[60,90] := 1.023754045; GammaY[61,90] := 1.026439758; GammaY[62,90] := 1.029150842; GammaY[63,90] := 1.031889643; GammaY[64,90] := 1.03465864; GammaY[65,90] := 1.037460466; GammaY[66,90] := 1.04029794; GammaY[67,90] := 1.043174083; GammaY[68,90] := 1.046092134; GammaY[69,90] := 1.049055597;
  GammaY[70,90] := 1.052068276; GammaY[71,90] := 1.055134304; GammaY[72,90] := 1.058258201; GammaY[73,90] := 1.06144494; GammaY[74,90] := 1.064700002; GammaY[75,90] := 1.068029459; GammaY[76,90] := 1.071440084; GammaY[77,90] := 1.074939459; GammaY[78,90] := 1.078536118; GammaY[79,90] := 1.082239736;
  GammaY[80,90] := 1.08606135; GammaY[81,90] := 1.090013633; GammaY[82,90] := 1.094111255; GammaY[83,90] := 1.098371354; GammaY[84,90] := 1.102814121; GammaY[85,90] := 1.107463613; GammaY[86,90] := 1.112348829; GammaY[87,90] := 1.11750519; GammaY[88,90] := 1.122976661; GammaY[89,90] := 1.12881879;
  GammaY[90,90] := 1.135103275; GammaY[91,90] := 1.141925065; GammaY[92,90] := 1.149413883; GammaY[93,90] := 1.15775404; GammaY[94,90] := 1.16722103; GammaY[95,90] := 1.178255882; GammaY[96,90] := 1.191637843; GammaY[97,90] := 1.2089778; GammaY[98,90] := 1.234792328; GammaY[99,90] := 1.287438455;
end;
procedure InitGammaY92;
begin
  GammaY[0,91] := 0.763578701; GammaY[1,91] := 0.799600087; GammaY[2,91] := 0.818312606; GammaY[3,91] := 0.831222219; GammaY[4,91] := 0.841370906; GammaY[5,91] := 0.849862565; GammaY[6,91] := 0.857237448; GammaY[7,91] := 0.863804081; GammaY[8,91] := 0.869756634; GammaY[9,91] := 0.875225844;
  GammaY[10,91] := 0.880304249; GammaY[11,91] := 0.885059928; GammaY[12,91] := 0.889544537; GammaY[13,91] := 0.893798269; GammaY[14,91] := 0.897853064; GammaY[15,91] := 0.901734771; GammaY[16,91] := 0.905464607; GammaY[17,91] := 0.909060258; GammaY[18,91] := 0.912536625; GammaY[19,91] := 0.91590638;
  GammaY[20,91] := 0.919180433; GammaY[21,91] := 0.922368244; GammaY[22,91] := 0.925478061; GammaY[23,91] := 0.928517163; GammaY[24,91] := 0.931491997; GammaY[25,91] := 0.934408296; GammaY[26,91] := 0.937271216; GammaY[27,91] := 0.940085411; GammaY[28,91] := 0.942855085; GammaY[29,91] := 0.945584068;
  GammaY[30,91] := 0.948275878; GammaY[31,91] := 0.950933758; GammaY[32,91] := 0.953560676; GammaY[33,91] := 0.956159413; GammaY[34,91] := 0.95873256; GammaY[35,91] := 0.961282532; GammaY[36,91] := 0.963811596; GammaY[37,91] := 0.966321909; GammaY[38,91] := 0.968815521; GammaY[39,91] := 0.971294367;
  GammaY[40,91] := 0.973760296; GammaY[41,91] := 0.976215104; GammaY[42,91] := 0.978660526; GammaY[43,91] := 0.981098226; GammaY[44,91] := 0.983529821; GammaY[45,91] := 0.985956912; GammaY[46,91] := 0.988381076; GammaY[47,91] := 0.990803835; GammaY[48,91] := 0.993226728; GammaY[49,91] := 0.995651289;
  GammaY[50,91] := 0.998079033; GammaY[51,91] := 1.000511448; GammaY[52,91] := 1.002950112; GammaY[53,91] := 1.005396632; GammaY[54,91] := 1.007852556; GammaY[55,91] := 1.010319503; GammaY[56,91] := 1.012799147; GammaY[57,91] := 1.015293206; GammaY[58,91] := 1.017803452; GammaY[59,91] := 1.020331735;
  GammaY[60,91] := 1.022879987; GammaY[61,91] := 1.025450215; GammaY[62,91] := 1.028044537; GammaY[63,91] := 1.030665193; GammaY[64,91] := 1.033314544; GammaY[65,91] := 1.035995103; GammaY[66,91] := 1.038709561; GammaY[67,91] := 1.041460796; GammaY[68,91] := 1.044251901; GammaY[69,91] := 1.047086222;
  GammaY[70,91] := 1.04996739; GammaY[71,91] := 1.052899344; GammaY[72,91] := 1.055886399; GammaY[73,91] := 1.058933296; GammaY[74,91] := 1.062045256; GammaY[75,91] := 1.06522807; GammaY[76,91] := 1.068488193; GammaY[77,91] := 1.071832848; GammaY[78,91] := 1.075270176; GammaY[79,91] := 1.078809402;
  GammaY[80,91] := 1.082461038; GammaY[81,91] := 1.086237167; GammaY[82,91] := 1.090151767; GammaY[83,91] := 1.094221161; GammaY[84,91] := 1.098464592; GammaY[85,91] := 1.102904976; GammaY[86,91] := 1.107569937; GammaY[87,91] := 1.112493224; GammaY[88,91] := 1.11771671; GammaY[89,91] := 1.123293302;
  GammaY[90,91] := 1.129291285; GammaY[91,91] := 1.135801072; GammaY[92,91] := 1.142946185; GammaY[93,91] := 1.150902103; GammaY[94,91] := 1.159931099; GammaY[95,91] := 1.170452968; GammaY[96,91] := 1.183209322; GammaY[97,91] := 1.199732923; GammaY[98,91] := 1.224320012; GammaY[99,91] := 1.274398127;
end;
procedure InitGammaY93;
begin
  GammaY[0,92] := 0.772846693; GammaY[1,92] := 0.807602146; GammaY[2,92] := 0.825637432; GammaY[3,92] := 0.838071743; GammaY[4,92] := 0.847842439; GammaY[5,92] := 0.856014989; GammaY[6,92] := 0.863110673; GammaY[7,92] := 0.869427123; GammaY[8,92] := 0.875151626; GammaY[9,92] := 0.880410252;
  GammaY[10,92] := 0.885292231; GammaY[11,92] := 0.889863196; GammaY[12,92] := 0.894172942; GammaY[13,92] := 0.898260215; GammaY[14,92] := 0.902155792; GammaY[15,92] := 0.905884578; GammaY[16,92] := 0.909467032; GammaY[17,92] := 0.912920187; GammaY[18,92] := 0.916258401; GammaY[19,92] := 0.91949388;
  GammaY[20,92] := 0.922637134; GammaY[21,92] := 0.925697274; GammaY[22,92] := 0.928682244; GammaY[23,92] := 0.931599052; GammaY[24,92] := 0.934453908; GammaY[25,92] := 0.937252337; GammaY[26,92] := 0.939999289; GammaY[27,92] := 0.942699244; GammaY[28,92] := 0.945356261; GammaY[29,92] := 0.94797402;
  GammaY[30,92] := 0.950555907; GammaY[31,92] := 0.953105039; GammaY[32,92] := 0.955624274; GammaY[33,92] := 0.958116284; GammaY[34,92] := 0.960583564; GammaY[35,92] := 0.963028433; GammaY[36,92] := 0.965453069; GammaY[37,92] := 0.967859549; GammaY[38,92] := 0.970249839; GammaY[39,92] := 0.972625801;
  GammaY[40,92] := 0.974989208; GammaY[41,92] := 0.977341787; GammaY[42,92] := 0.979685203; GammaY[43,92] := 0.982021041; GammaY[44,92] := 0.984350873; GammaY[45,92] := 0.986676238; GammaY[46,92] := 0.988998625; GammaY[47,92] := 0.991319502; GammaY[48,92] := 0.993640347; GammaY[49,92] := 0.995962631;
  GammaY[50,92] := 0.998287789; GammaY[51,92] := 1.000617272; GammaY[52,92] := 1.002952584; GammaY[53,92] := 1.00529524; GammaY[54,92] := 1.007646747; GammaY[55,92] := 1.010008653; GammaY[56,92] := 1.01238255; GammaY[57,92] := 1.014770087; GammaY[58,92] := 1.017172956; GammaY[59,92] := 1.019592916;
  GammaY[60,92] := 1.022031813; GammaY[61,92] := 1.024491566; GammaY[62,92] := 1.026974205; GammaY[63,92] := 1.029481868; GammaY[64,92] := 1.032016809; GammaY[65,92] := 1.03458143; GammaY[66,92] := 1.0371783; GammaY[67,92] := 1.039810161; GammaY[68,92] := 1.042479961; GammaY[69,92] := 1.045190898;
  GammaY[70,92] := 1.047946424; GammaY[71,92] := 1.050750308; GammaY[72,92] := 1.053606671; GammaY[73,92] := 1.056520024; GammaY[74,92] := 1.059495351; GammaY[75,92] := 1.06253818; GammaY[76,92] := 1.065654658; GammaY[77,92] := 1.06885167; GammaY[78,92] := 1.07213698; GammaY[79,92] := 1.075519375;
  GammaY[80,92] := 1.079008881; GammaY[81,92] := 1.082617017; GammaY[82,92] := 1.086357104; GammaY[83,92] := 1.090244704; GammaY[84,92] := 1.094298148; GammaY[85,92] := 1.098539271; GammaY[86,92] := 1.102994395; GammaY[87,92] := 1.107695672; GammaY[88,92] := 1.112682998; GammaY[89,92] := 1.118006776;
  GammaY[90,92] := 1.123732054; GammaY[91,92] := 1.129944947; GammaY[92,92] := 1.136763093; GammaY[93,92] := 1.144353601; GammaY[94,92] := 1.152966207; GammaY[95,92] := 1.16300059; GammaY[96,92] := 1.175162707; GammaY[97,92] := 1.190911338; GammaY[98,92] := 1.214334159; GammaY[99,92] := 1.261981344;
end;
procedure InitGammaY94;
begin
  GammaY[0,93] := 0.781784258; GammaY[1,93] := 0.815306244; GammaY[2,93] := 0.832683514; GammaY[3,93] := 0.844656759; GammaY[4,93] := 0.854061187; GammaY[5,93] := 0.861924774; GammaY[6,93] := 0.868750323; GammaY[7,93] := 0.874824865; GammaY[8,93] := 0.880328966; GammaY[9,93] := 0.885384166;
  GammaY[10,93] := 0.890076472; GammaY[11,93] := 0.894469142; GammaY[12,93] := 0.898610163; GammaY[13,93] := 0.902536864; GammaY[14,93] := 0.906278907; GammaY[15,93] := 0.909860283; GammaY[16,93] := 0.913300704; GammaY[17,93] := 0.916616568; GammaY[18,93] := 0.919821701; GammaY[19,93] := 0.922927872;
  GammaY[20,93] := 0.925945194; GammaY[21,93] := 0.928882442; GammaY[22,93] := 0.931747264; GammaY[23,93] := 0.934546406; GammaY[24,93] := 0.937285847; GammaY[25,93] := 0.939970897; GammaY[26,93] := 0.942606335; GammaY[27,93] := 0.945196471; GammaY[28,93] := 0.9477452; GammaY[29,93] := 0.950256066;
  GammaY[30,93] := 0.952732327; GammaY[31,93] := 0.955176982; GammaY[32,93] := 0.957592778; GammaY[33,93] := 0.959982287; GammaY[34,93] := 0.962347906; GammaY[35,93] := 0.964691864; GammaY[36,93] := 0.967016255; GammaY[37,93] := 0.969323074; GammaY[38,93] := 0.971614212; GammaY[39,93] := 0.973891444;
  GammaY[40,93] := 0.976156495; GammaY[41,93] := 0.97841102; GammaY[42,93] := 0.980656602; GammaY[43,93] := 0.982894769; GammaY[44,93] := 0.985127029; GammaY[45,93] := 0.987354859; GammaY[46,93] := 0.989579677; GammaY[47,93] := 0.99180291; GammaY[48,93] := 0.994025975; GammaY[49,93] := 0.996250248;
  GammaY[50,93] := 0.998477138; GammaY[51,93] := 1.000708032; GammaY[52,93] := 1.002944354; GammaY[53,93] := 1.005187562; GammaY[54,93] := 1.00743909; GammaY[55,93] := 1.00970042; GammaY[56,93] := 1.011973084; GammaY[57,93] := 1.014258656; GammaY[58,93] := 1.016558753; GammaY[59,93] := 1.018875062;
  GammaY[60,93] := 1.021209345; GammaY[61,93] := 1.023563434; GammaY[62,93] := 1.025939259; GammaY[63,93] := 1.028338863; GammaY[64,93] := 1.0307644; GammaY[65,93] := 1.033218168; GammaY[66,93] := 1.035702621; GammaY[67,93] := 1.038220373; GammaY[68,93] := 1.040774244; GammaY[69,93] := 1.043367283;
  GammaY[70,93] := 1.046002783; GammaY[71,93] := 1.048684333; GammaY[72,93] := 1.051415863; GammaY[73,93] := 1.05420168; GammaY[74,93] := 1.05704654; GammaY[75,93] := 1.059955716; GammaY[76,93] := 1.062935069; GammaY[77,93] := 1.065991171; GammaY[78,93] := 1.069131418; GammaY[79,93] := 1.072364184;
  GammaY[80,93] := 1.075699033; GammaY[81,93] := 1.079146937; GammaY[82,93] := 1.082720604; GammaY[83,93] := 1.086434866; GammaY[84,93] := 1.090307195; GammaY[85,93] := 1.094358406; GammaY[86,93] := 1.098613579; GammaY[87,93] := 1.103103352; GammaY[88,93] := 1.10786574; GammaY[89,93] := 1.11294877;
  GammaY[90,93] := 1.118414417; GammaY[91,93] := 1.124344726; GammaY[92,93] := 1.130851755; GammaY[93,93] := 1.138094671; GammaY[94,93] := 1.146311327; GammaY[95,93] := 1.155882329; GammaY[96,93] := 1.167479847; GammaY[97,93] := 1.182492574; GammaY[98,93] := 1.204810684; GammaY[99,93] := 1.250155721;
end;
procedure InitGammaY95;
begin
  GammaY[0,94] := 0.790400244; GammaY[1,94] := 0.822721544; GammaY[2,94] := 0.839459979; GammaY[3,94] := 0.850986279; GammaY[4,94] := 0.860036009; GammaY[5,94] := 0.86760063; GammaY[6,94] := 0.874164959; GammaY[7,94] := 0.880005699; GammaY[8,94] := 0.88529689; GammaY[9,94] := 0.890155664;
  GammaY[10,94] := 0.894664896; GammaY[11,94] := 0.898885537; GammaY[12,94] := 0.902863816; GammaY[13,94] := 0.906635692; GammaY[14,94] := 0.910229737; GammaY[15,94] := 0.913669056; GammaY[16,94] := 0.916972634; GammaY[17,94] := 0.920156259; GammaY[18,94] := 0.923233247; GammaY[19,94] := 0.926214927;
  GammaY[20,94] := 0.929111038; GammaY[21,94] := 0.931930024; GammaY[22,94] := 0.934679248; GammaY[23,94] := 0.937365203; GammaY[24,94] := 0.939993644; GammaY[25,94] := 0.942569679; GammaY[26,94] := 0.945097909; GammaY[27,94] := 0.947582479; GammaY[28,94] := 0.950027128; GammaY[29,94] := 0.952435281;
  GammaY[30,94] := 0.954810071; GammaY[31,94] := 0.957154361; GammaY[32,94] := 0.959470813; GammaY[33,94] := 0.9617619; GammaY[34,94] := 0.964029911; GammaY[35,94] := 0.966276989; GammaY[36,94] := 0.968505161; GammaY[37,94] := 0.970716344; GammaY[38,94] := 0.972912331; GammaY[39,94] := 0.975094849;
  GammaY[40,94] := 0.977265557; GammaY[41,94] := 0.979426027; GammaY[42,94] := 0.981577774; GammaY[43,94] := 0.983722285; GammaY[44,94] := 0.985861009; GammaY[45,94] := 0.98799533; GammaY[46,94] := 0.990126638; GammaY[47,94] := 0.992256302; GammaY[48,94] := 0.994385649; GammaY[49,94] := 0.996516028;
  GammaY[50,94] := 0.998648789; GammaY[51,94] := 1.000785227; GammaY[52,94] := 1.002926728; GammaY[53,94] := 1.005074702; GammaY[54,94] := 1.007230512; GammaY[55,94] := 1.009395573; GammaY[56,94] := 1.01157134; GammaY[57,94] := 1.013759321; GammaY[58,94] := 1.015961067; GammaY[59,94] := 1.018178187;
  GammaY[60,94] := 1.020412364; GammaY[61,94] := 1.02266536; GammaY[62,94] := 1.024939016; GammaY[63,94] := 1.027235282; GammaY[64,94] := 1.029556216; GammaY[65,94] := 1.031904002; GammaY[66,94] := 1.034280984; GammaY[67,94] := 1.03668966; GammaY[68,94] := 1.039132726; GammaY[69,94] := 1.04161309;
  GammaY[70,94] := 1.044133897; GammaY[71,94] := 1.046698577; GammaY[72,94] := 1.04931087; GammaY[73,94] := 1.051974886; GammaY[74,94] := 1.054695163; GammaY[75,94] := 1.057476723; GammaY[76,94] := 1.060325165; GammaY[77,94] := 1.063246757; GammaY[78,94] := 1.066248551; GammaY[79,94] := 1.069338541;
  GammaY[80,94] := 1.072525835; GammaY[81,94] := 1.075820898; GammaY[82,94] := 1.07923584; GammaY[83,94] := 1.082784806; GammaY[84,94] := 1.086484457; GammaY[85,94] := 1.090354629; GammaY[86,94] := 1.094419233; GammaY[87,94] := 1.09870747; GammaY[88,94] := 1.103255568; GammaY[89,94] := 1.108109295;
  GammaY[90,94] := 1.113327711; GammaY[91,94] := 1.118988997; GammaY[92,94] := 1.12519992; GammaY[93,94] := 1.132112121; GammaY[94,94] := 1.139952167; GammaY[95,94] := 1.149082585; GammaY[96,94] := 1.160143517; GammaY[97,94] := 1.174457228; GammaY[98,94] := 1.195726804; GammaY[99,94] := 1.238890984;
end;
procedure InitGammaY96;
begin
  GammaY[0,95] := 0.798703557; GammaY[1,95] := 0.829857131; GammaY[2,95] := 0.845975796; GammaY[3,95] := 0.857069109; GammaY[4,95] := 0.865775552; GammaY[5,95] := 0.873051041; GammaY[6,95] := 0.879362894; GammaY[7,95] := 0.884977776; GammaY[8,95] := 0.890063384; GammaY[9,95] := 0.894732571;
  GammaY[10,95] := 0.89906517; GammaY[11,95] := 0.90311989; GammaY[12,95] := 0.906941254; GammaY[13,95] := 0.910563895; GammaY[14,95] := 0.914015324; GammaY[15,95] := 0.917317788; GammaY[16,95] := 0.92048956; GammaY[17,95] := 0.923545852; GammaY[18,95] := 0.926499481; GammaY[19,95] := 0.929361341;
  GammaY[20,95] := 0.932140811; GammaY[21,95] := 0.934846019; GammaY[22,95] := 0.93748405; GammaY[23,95] := 0.940061151; GammaY[24,95] := 0.942582855; GammaY[25,95] := 0.945054087; GammaY[26,95] := 0.947479273; GammaY[27,95] := 0.949862383; GammaY[28,95] := 0.952207032; GammaY[29,95] := 0.954516512;
  GammaY[30,95] := 0.956793825; GammaY[31,95] := 0.959041728; GammaY[32,95] := 0.961262781; GammaY[33,95] := 0.963459363; GammaY[34,95] := 0.965633664; GammaY[35,95] := 0.967787758; GammaY[36,95] := 0.969923595; GammaY[37,95] := 0.972042992; GammaY[38,95] := 0.974147692; GammaY[39,95] := 0.976239359;
  GammaY[40,95] := 0.978319556; GammaY[41,95] := 0.980389811; GammaY[42,95] := 0.982451595; GammaY[43,95] := 0.984506318; GammaY[44,95] := 0.98655535; GammaY[45,95] := 0.988600046; GammaY[46,95] := 0.990641741; GammaY[47,95] := 0.992681716; GammaY[48,95] := 0.994721273; GammaY[49,95] := 0.996761705;
  GammaY[50,95] := 0.998804274; GammaY[51,95] := 1.000850249; GammaY[52,95] := 1.002900958; GammaY[53,95] := 1.004957725; GammaY[54,95] := 1.007021867; GammaY[55,95] := 1.009094751; GammaY[56,95] := 1.011177763; GammaY[57,95] := 1.013272346; GammaY[58,95] := 1.015379977; GammaY[59,95] := 1.017502192;
  GammaY[60,95] := 1.019640602; GammaY[61,95] := 1.021796883; GammaY[62,95] := 1.023972803; GammaY[63,95] := 1.026170227; GammaY[64,95] := 1.028391117; GammaY[65,95] := 1.03063757; GammaY[66,95] := 1.032911811; GammaY[67,95] := 1.035216226; GammaY[68,95] := 1.037553389; GammaY[69,95] := 1.03992607;
  GammaY[70,95] := 1.042337278; GammaY[71,95] := 1.044790288; GammaY[72,95] := 1.047288667; GammaY[73,95] := 1.049836345; GammaY[74,95] := 1.052437641; GammaY[75,95] := 1.055097346; GammaY[76,95] := 1.057820803; GammaY[77,95] := 1.060613986; GammaY[78,95] := 1.063483626; GammaY[79,95] := 1.066437348;
  GammaY[80,95] := 1.06948384; GammaY[81,95] := 1.072633084; GammaY[82,95] := 1.075896628; GammaY[83,95] := 1.079287949; GammaY[84,95] := 1.082822934; GammaY[85,95] := 1.086520501; GammaY[86,95] := 1.090403445; GammaY[87,95] := 1.094499607; GammaY[88,95] := 1.098843519; GammaY[89,95] := 1.103478804;
  GammaY[90,95] := 1.108461751; GammaY[91,95] := 1.11386687; GammaY[92,95] := 1.119795914; GammaY[93,95] := 1.126393374; GammaY[94,95] := 1.133875123; GammaY[95,95] := 1.142586529; GammaY[96,95] := 1.153137357; GammaY[97,95] := 1.166786905; GammaY[98,95] := 1.187060958; GammaY[99,95] := 1.228158373;
end;
procedure InitGammaY97;
begin
  GammaY[0,96] := 0.806703107; GammaY[1,96] := 0.836721966; GammaY[2,96] := 0.852239769; GammaY[3,96] := 0.862913871; GammaY[4,96] := 0.87128825; GammaY[5,96] := 0.878284255; GammaY[6,96] := 0.884352198; GammaY[7,96] := 0.889748991; GammaY[8,96] := 0.894636176; GammaY[9,96] := 0.899122448;
  GammaY[10,96] := 0.903284691; GammaY[11,96] := 0.907179438; GammaY[12,96] := 0.910849559; GammaY[13,96] := 0.914328398; GammaY[14,96] := 0.917642439; GammaY[15,96] := 0.920813099; GammaY[16,96] := 0.923857966; GammaY[17,96] := 0.926791681; GammaY[18,96] := 0.929626572; GammaY[19,96] := 0.932373137;
  GammaY[20,96] := 0.935040398; GammaY[21,96] := 0.937636161; GammaY[22,96] := 0.940167258; GammaY[23,96] := 0.942639701; GammaY[24,96] := 0.945058799; GammaY[25,96] := 0.947429296; GammaY[26,96] := 0.949755449; GammaY[27,96] := 0.952041075; GammaY[28,96] := 0.954289651; GammaY[29,96] := 0.956504336;
  GammaY[30,96] := 0.958688021; GammaY[31,96] := 0.960843372; GammaY[32,96] := 0.962972837; GammaY[33,96] := 0.965078685; GammaY[34,96] := 0.967163043; GammaY[35,96] := 0.969227905; GammaY[36,96] := 0.97127512; GammaY[37,96] := 0.973306456; GammaY[38,96] := 0.975323589; GammaY[39,96] := 0.977328092;
  GammaY[40,96] := 0.979321488; GammaY[41,96] := 0.981305245; GammaY[42,96] := 0.98328075; GammaY[43,96] := 0.98524937; GammaY[44,96] := 0.987212439; GammaY[45,96] := 0.98917123; GammaY[46,96] := 0.991127021; GammaY[47,96] := 0.993081069; GammaY[48,96] := 0.995034593; GammaY[49,96] := 0.996988832;
  GammaY[50,96] := 0.998945023; GammaY[51,96] := 1.000904351; GammaY[52,96] := 1.00286809; GammaY[53,96] := 1.004837538; GammaY[54,96] := 1.006813931; GammaY[55,96] := 1.008798567; GammaY[56,96] := 1.010792781; GammaY[57,96] := 1.012797951; GammaY[58,96] := 1.014815503; GammaY[59,96] := 1.016846903;
  GammaY[60,96] := 1.018893688; GammaY[61,96] := 1.020957459; GammaY[62,96] := 1.023039897; GammaY[63,96] := 1.025142784; GammaY[64,96] := 1.027267996; GammaY[65,96] := 1.029417536; GammaY[66,96] := 1.031593538; GammaY[67,96] := 1.03379828; GammaY[68,96] := 1.036034217; GammaY[69,96] := 1.038303993;
  GammaY[70,96] := 1.040610472; GammaY[71,96] := 1.04295678; GammaY[72,96] := 1.045346326; GammaY[73,96] := 1.047782857; GammaY[74,96] := 1.050270504; GammaY[75,96] := 1.05281384; GammaY[76,96] := 1.055417955; GammaY[77,96] := 1.058088545; GammaY[78,96] := 1.060832035; GammaY[79,96] := 1.063655693;
  GammaY[80,96] := 1.066567811; GammaY[81,96] := 1.069577912; GammaY[82,96] := 1.072697011; GammaY[83,96] := 1.075937959; GammaY[84,96] := 1.079315904; GammaY[85,96] := 1.082848883; GammaY[86,96] := 1.086558634; GammaY[87,96] := 1.090471705; GammaY[88,96] := 1.094621021; GammaY[89,96] := 1.099048169;
  GammaY[90,96] := 1.103806805; GammaY[91,96] := 1.108967953; GammaY[92,96] := 1.114628597; GammaY[93,96] := 1.120926457; GammaY[94,96] := 1.128067249; GammaY[95,96] := 1.136380058; GammaY[96,96] := 1.146445833; GammaY[97,96] := 1.159464153; GammaY[98,96] := 1.178792733; GammaY[99,96] := 1.217930827;
end;
procedure InitGammaY98;
begin
  GammaY[0,97] := 0.814407787; GammaY[1,97] := 0.843324861; GammaY[2,97] := 0.858260513; GammaY[3,97] := 0.868528972; GammaY[4,97] := 0.87658231; GammaY[5,97] := 0.883308285; GammaY[6,97] := 0.889140696; GammaY[7,97] := 0.894326991; GammaY[8,97] := 0.899022738; GammaY[9,97] := 0.903332594;
  GammaY[10,97] := 0.907330592; GammaY[11,97] := 0.911071153; GammaY[12,97] := 0.914595544; GammaY[13,97] := 0.917935859; GammaY[14,97] := 0.921117589; GammaY[15,97] := 0.924161344; GammaY[16,97] := 0.927084051; GammaY[17,97] := 0.929899801; GammaY[18,97] := 0.932620449; GammaY[19,97] := 0.935256098;
  GammaY[20,97] := 0.937815422; GammaY[21,97] := 0.940305942; GammaY[22,97] := 0.942734229; GammaY[23,97] := 0.94510605; GammaY[24,97] := 0.947426526; GammaY[25,97] := 0.949700216; GammaY[26,97] := 0.951931206; GammaY[27,97] := 0.954123184; GammaY[28,97] := 0.956279475; GammaY[29,97] := 0.958403123;
  GammaY[30,97] := 0.960496913; GammaY[31,97] := 0.962563387; GammaY[32,97] := 0.96460491; GammaY[33,97] := 0.966623677; GammaY[34,97] := 0.968621712; GammaY[35,97] := 0.970600928; GammaY[36,97] := 0.972563125; GammaY[37,97] := 0.974509977; GammaY[38,97] := 0.976443094; GammaY[39,97] := 0.978364009;
  GammaY[40,97] := 0.980274161; GammaY[41,97] := 0.982174958; GammaY[42,97] := 0.984067754; GammaY[43,97] := 0.985953839; GammaY[44,97] := 0.987834489; GammaY[45,97] := 0.989710953; GammaY[46,97] := 0.991584427; GammaY[47,97] := 0.993456119; GammaY[48,97] := 0.995327222; GammaY[49,97] := 0.997198896;
  GammaY[50,97] := 0.999072301; GammaY[51,97] := 1.000948646; GammaY[52,97] := 1.002829132; GammaY[53,97] := 1.004714944; GammaY[54,97] := 1.006607309; GammaY[55,97] := 1.008507466; GammaY[56,97] := 1.010416695; GammaY[57,97] := 1.012336305; GammaY[58,97] := 1.014267649; GammaY[59,97] := 1.016212136;
  GammaY[60,97] := 1.018171237; GammaY[61,97] := 1.020146492; GammaY[62,97] := 1.022139504; GammaY[63,97] := 1.024151975; GammaY[64,97] := 1.026185696; GammaY[65,97] := 1.028242573; GammaY[66,97] := 1.030324641; GammaY[67,97] := 1.032434084; GammaY[68,97] := 1.034573248; GammaY[69,97] := 1.036744655;
  GammaY[70,97] := 1.038951048; GammaY[71,97] := 1.041195401; GammaY[72,97] := 1.043480967; GammaY[73,97] := 1.045811324; GammaY[74,97] := 1.048190408; GammaY[75,97] := 1.050622588; GammaY[76,97] := 1.053112727; GammaY[77,97] := 1.055666263; GammaY[78,97] := 1.058289317; GammaY[79,97] := 1.060988824;
  GammaY[80,97] := 1.063772691; GammaY[81,97] := 1.066650005; GammaY[82,97] := 1.069631275; GammaY[83,97] := 1.072728763; GammaY[84,97] := 1.075956914; GammaY[85,97] := 1.079332928; GammaY[86,97] := 1.082877534; GammaY[87,97] := 1.086616049; GammaY[88,97] := 1.090579874; GammaY[89,97] := 1.094808664;
  GammaY[90,97] := 1.099353578; GammaY[91,97] := 1.104282324; GammaY[92,97] := 1.10968736; GammaY[93,97] := 1.115699964; GammaY[94,97] := 1.122516222; GammaY[95,97] := 1.130449767; GammaY[96,97] := 1.140054196; GammaY[97,97] := 1.15247243; GammaY[98,97] := 1.170902802; GammaY[99,97] := 1.208182952;
end;
procedure InitGammaY99;
begin
  GammaY[0,98] := 0.821826436; GammaY[1,98] := 0.84967449; GammaY[2,98] := 0.864046458; GammaY[3,98] := 0.873922612; GammaY[4,98] := 0.881665712; GammaY[5,98] := 0.888130908; GammaY[6,98] := 0.893735973; GammaY[7,98] := 0.898719176; GammaY[8,98] := 0.903230291; GammaY[9,98] := 0.907370067;
  GammaY[10,98] := 0.911209767; GammaY[11,98] := 0.914801766; GammaY[12,98] := 0.918185782; GammaY[13,98] := 0.921392696; GammaY[14,98] := 0.924447039; GammaY[15,98] := 0.927368639; GammaY[16,98] := 0.930173784; GammaY[17,98] := 0.932876023; GammaY[18,98] := 0.93548677; GammaY[19,98] := 0.938015739;
  GammaY[20,98] := 0.940471274; GammaY[21,98] := 0.942860612; GammaY[22,98] := 0.945190057; GammaY[23,98] := 0.947465171; GammaY[24,98] := 0.949690873; GammaY[25,98] := 0.951871546; GammaY[26,98] := 0.954011125; GammaY[27,98] := 0.956113137; GammaY[28,98] := 0.958180796; GammaY[29,98] := 0.960217029;
  GammaY[30,98] := 0.962224492; GammaY[31,98] := 0.964205648; GammaY[32,98] := 0.966162763; GammaY[33,98] := 0.968097938; GammaY[34,98] := 0.970013139; GammaY[35,98] := 0.971910183; GammaY[36,98] := 0.973790797; GammaY[37,98] := 0.975656612; GammaY[38,98] := 0.97750915; GammaY[39,98] := 0.979349883;
  GammaY[40,98] := 0.981180215; GammaY[41,98] := 0.983001474; GammaY[42,98] := 0.98481496; GammaY[43,98] := 0.986621932; GammaY[44,98] := 0.988423592; GammaY[45,98] := 0.990221134; GammaY[46,98] := 0.992015734; GammaY[47,98] := 0.993808522; GammaY[48,98] := 0.99560064; GammaY[49,98] := 0.997393229;
  GammaY[50,98] := 0.999187365; GammaY[51,98] := 1.000984214; GammaY[52,98] := 1.002784956; GammaY[53,98] := 1.004590701; GammaY[54,98] := 1.006402613; GammaY[55,98] := 1.008221884; GammaY[56,98] := 1.010049741; GammaY[57,98] := 1.011887446; GammaY[58,98] := 1.013736295; GammaY[59,98] := 1.015597628;
  GammaY[60,98] := 1.017472842; GammaY[61,98] := 1.01936341; GammaY[62,98] := 1.021270867; GammaY[63,98] := 1.023196839; GammaY[64,98] := 1.025143047; GammaY[65,98] := 1.027111313; GammaY[66,98] := 1.029103575; GammaY[67,98] := 1.031121911; GammaY[68,98] := 1.033168562; GammaY[69,98] := 1.035245939;
  GammaY[70,98] := 1.037356659; GammaY[71,98] := 1.039503572; GammaY[72,98] := 1.041689784; GammaY[73,98] := 1.043918701; GammaY[74,98] := 1.046194081; GammaY[75,98] := 1.048520091; GammaY[76,98] := 1.050901373; GammaY[77,98] := 1.053343121; GammaY[78,98] := 1.055851175; GammaY[79,98] := 1.058432157;
  GammaY[80,98] := 1.061093608; GammaY[81,98] := 1.063844191; GammaY[82,98] := 1.066693931; GammaY[83,98] := 1.069654529; GammaY[84,98] := 1.072739771; GammaY[85,98] := 1.075966066; GammaY[86,98] := 1.079353179; GammaY[87,98] := 1.082925253; GammaY[88,98] := 1.086712238; GammaY[89,98] := 1.090751956;
  GammaY[90,98] := 1.095093194; GammaY[91,98] := 1.099800516; GammaY[92,98] := 1.104962072; GammaY[93,98] := 1.110703028; GammaY[94,98] := 1.11721032; GammaY[95,98] := 1.124782908; GammaY[96,98] := 1.133948428; GammaY[97,98] := 1.145796031; GammaY[98,98] := 1.163372857; GammaY[99,98] := 1.198890279;
end;
procedure InitGammaY100;
begin
  GammaY[0,99] := 0.828967824; GammaY[1,99] := 0.855779361; GammaY[2,99] := 0.869605833; GammaY[3,99] := 0.879102769; GammaY[4,99] := 0.88654621; GammaY[5,99] := 0.892759667; GammaY[6,99] := 0.898145374; GammaY[7,99] := 0.902932698; GammaY[8,99] := 0.907265808; GammaY[9,99] := 0.911241668;
  GammaY[10,99] := 0.914928843; GammaY[11,99] := 0.918377744; GammaY[12,99] := 0.921626579; GammaY[13,99] := 0.924705062; GammaY[14,99] := 0.927636789; GammaY[15,99] := 0.930440838; GammaY[16,99] := 0.933132871; GammaY[17,99] := 0.935725923; GammaY[18,99] := 0.938230972; GammaY[19,99] := 0.940657357;
  GammaY[20,99] := 0.943013105; GammaY[21,99] := 0.945305169; GammaY[22,99] := 0.947539626; GammaY[23,99] := 0.949721808; GammaY[24,99] := 0.951856448; GammaY[25,99] := 0.953947761; GammaY[26,99] := 0.955999523; GammaY[27,99] := 0.958015143; GammaY[28,99] := 0.959997691; GammaY[29,99] := 0.961949978;
  GammaY[30,99] := 0.963874571; GammaY[31,99] := 0.965773829; GammaY[32,99] := 0.967649936; GammaY[33,99] := 0.969504899; GammaY[34,99] := 0.971340603; GammaY[35,99] := 0.973158814; GammaY[36,99] := 0.974961171; GammaY[37,99] := 0.976749239; GammaY[38,99] := 0.978524502; GammaY[39,99] := 0.980288348;
  GammaY[40,99] := 0.982042127; GammaY[41,99] := 0.983787135; GammaY[42,99] := 0.985524595; GammaY[43,99] := 0.987255717; GammaY[44,99] := 0.988981666; GammaY[45,99] := 0.990703583; GammaY[46,99] := 0.992422592; GammaY[47,99] := 0.994139777; GammaY[48,99] := 0.995856234; GammaY[49,99] := 0.997573053;
  GammaY[50,99] := 0.999291269; GammaY[51,99] := 1.00101199; GammaY[52,99] := 1.002736345; GammaY[53,99] := 1.004465409; GammaY[54,99] := 1.006200295; GammaY[55,99] := 1.007942147; GammaY[56,99] := 1.009692128; GammaY[57,99] := 1.011451437; GammaY[58,99] := 1.013221315; GammaY[59,99] := 1.015003055;
  GammaY[60,99] := 1.016798001; GammaY[61,99] := 1.018607553; GammaY[62,99] := 1.020433177; GammaY[63,99] := 1.02227642; GammaY[64,99] := 1.024138921; GammaY[65,99] := 1.026022429; GammaY[66,99] := 1.027928803; GammaY[67,99] := 1.029860026; GammaY[68,99] := 1.031818238; GammaY[69,99] := 1.033805739;
  GammaY[70,99] := 1.035825023; GammaY[71,99] := 1.037878807; GammaY[72,99] := 1.039970062; GammaY[73,99] := 1.042102047; GammaY[74,99] := 1.044278352; GammaY[75,99] := 1.046502948; GammaY[76,99] := 1.04878026; GammaY[77,99] := 1.051115243; GammaY[78,99] := 1.053513479; GammaY[79,99] := 1.055981286;
  GammaY[80,99] := 1.058525866; GammaY[81,99] := 1.06115548; GammaY[82,99] := 1.063879691; GammaY[83,99] := 1.066709663; GammaY[84,99] := 1.069658549; GammaY[85,99] := 1.072742006; GammaY[86,99] := 1.075978894; GammaY[87,99] := 1.079392241; GammaY[88,99] := 1.083010609; GammaY[89,99] := 1.086870077;
  GammaY[90,99] := 1.091017179; GammaY[91,99] := 1.095513492; GammaY[92,99] := 1.100443081; GammaY[93,99] := 1.105925292; GammaY[94,99] := 1.112138369; GammaY[95,99] := 1.119367348; GammaY[96,99] := 1.128115207; GammaY[97,99] := 1.139420051; GammaY[98,99] := 1.156185549; GammaY[99,99] := 1.190030319;
end;

{
////////////
// Negative Binomial version
/////////////
var
  NBX: array [0..60] of double;
  NBY: array [0..60,0..60] of double;
  d1da,d2da,d3da: array [0..60,0..60] of double;

procedure MakeSpline(var x,y,b,c,d: array of double);
var
  a,h,l,m,z: array of double;
  i: integer;
begin
  setlength(a,61);
  setlength(h,61);
  setlength(l,61);
  setlength(m,61);
  setlength(z,61);

  for i := 0 to 59 do
    h[i] := x[i+1] -x[i];
  for i := 1 to 59 do
    a[i] := 3/h[i]*(y[i+1]-y[i]) -3/h[i-1]*(y[i]-y[i-1]);
  l[0] := 1;
  m[0] := 0;
  z[0] := 0;
  for i := 1 to 59 do
  begin
    l[i] := 2*(x[i+1] -x[i-1]) -h[i-1]*m[i-1];
    m[i] := h[i]/l[i];
    z[i] := (a[i] -h[i-1]*z[i-1])/l[i];
  end;
  l[60] := 1;
  z[60] := 0;
  c[60] := 0;
  for i := 59 downto 0 do
  begin
    c[i] := z[i] -m[i]*c[i+1];
    b[i] := (y[i+1] -y[i])/h[i] -h[i]*(c[i+1] +2*c[i])/3;
    d[i] := (c[i+1] -c[i])/3/h[i];
  end;

  setlength(a,0);
  setlength(h,0);
  setlength(l,0);
  setlength(m,0);
  setlength(z,0);
end;

function Interpolation(ln_gamma: double; i: integer): double;
var
  j: integer;
  delta: double;
begin
  j := 60;
  while ln_gamma < NBX[j] do
    dec(j);
  delta := ln_gamma -NBX[j];

  result := NBY[i,j] +(d1da[i,j] +(d2da[i,j] +d3da[i,j]*delta)*delta)*delta;
  if result < 0 then result := 0;
end;

procedure InitializeNegativeBinomialTable(m: double; n, nc:integer);
var
  P: array[0..60] of double;
  d,f: array of double;
  i,j,k: integer;
begin
  if m < n/100 then
    m := n/100;

  setlength(d,n+1);
  setlength(f,n+1);

  NBX[0] := 0.01;
  for i := 1 to 60 do
    NBX[i] := NBX[i-1]*sqrt(sqrt(2));
  for i := 0 to 60 do
    P[i] := m/NBX[i];

  for i := 0 to 60 do
  begin
    d[0] := power((1+P[i]), -NBX[i]);
    for k := 0 to n-1 do
      d[k+1] := (NBX[i]+k)/(k+1)*(P[i]/(P[i]+1))*d[k];

    f[0] := d[0];
    for k := 1 to n-1 do
      f[k] := f[k-1]+d[k];

    k := 0;
    for j := 0 to nc-2 do
    begin
      NBY[j,i] := 0;
      repeat
        if k > 0 then
          if j = 0 then
            NBY[j,i] := NBY[j,i] +(min(f[k],(j+1)/nc) -f[k-1])*k
          else
            NBY[j,i] := NBY[j,i] +(min(f[k],(j+1)/nc) -max(f[k-1],j/nc))*k;
        if f[k] < (j+1)/nc then
        begin
          inc(k);
          if f[k] >= (j+1)/nc then
            if j = 0 then
              NBY[j,i] := NBY[j,i] +((j+1)/nc -f[k-1])*k
            else
              NBY[j,i] := NBY[j,i] +((j+1)/nc -max(f[k-1],j/nc))*k;

        end;
      until (f[k] >= (j+1)/nc) or (k = n);
    end;

    NBY[nc-1,i] := m;
    for j := 0 to nc-2 do
      NBY[nc-1,i] := NBY[nc-1,i] -NBY[j,i];

    for j := 0 to nc-1 do
      NBY[j,i] := NBY[j,i]/m;
  end;
  setlength(d,0);
  setlength(f,0);

  for i := 0 to 60 do
    NBX[i] := ln(NBX[i]);
  for i := 0 to nc-1 do
    MakeSpline(NBX,NBY[i],d1da[i],d2da[i],d3da[i]);
end;

procedure DiscreteNegativeBinomial(a: double; n: integer; var c: array of extended);
var
  i: integer;
begin
  for i := 0 to n-1 do
    c[i] := Interpolation(ln(a), i)*n;
end;

procedure TGammaRateVariationModel.ComputeInitialGamma;
var
  delta: double;

  function ecr(i,j: integer): double;
  begin
    result := (NBY[i,j] +(d1da[i,j] +(d2da[i,j] +d3da[i,j]*delta)*delta)*delta)*NoOfRates;

    if result < 0 then
      result := 0;
  end;

  function d1(i,j: integer): double;
  begin
    result := (d1da[i,j] +(2*d2da[i,j] +3*d3da[i,j]*delta)*delta)*NoOfRates;
  end;

  function d2(i,j: integer): double;
  begin
    result := (2*d2da[i,j] +6*d3da[i,j]*delta)*NoOfRates;
  end;

  function R(j: integer): double;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to NoOfRates-1 do
      result := result +(ecr(i,j)-FRate[i])*(ecr(i,j)-FRate[i]);
  end;

  function dR(j: integer): double;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to NoOfRates-1 do
      result := result +(ecr(i,j)-FRate[i])*d1(i,j);
  end;

  function ddR(j: integer): double;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to NoOfRates-1 do
      result := result +(d1(i,j)*d1(i,j) +d2(i,j)*(ecr(i,j)-FRate[i]));
  end;

  function R2(j: integer): double;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to NoOfRates-1 do
      if ecr(i,j) > 0 then
        result := result +(ecr(i,j)-FRate[i])*(ecr(i,j)-FRate[i]);
  end;

var
  d,dd,mind,r0,r1,ming,maxg: double;
  i,j,k,rep: integer;
begin

  r0 := 0;
  for k := 0 to NoOfRates-1 do
    r0 := r0 +(NBY[k,60]*NoOfRates-FRate[k])*(NBY[k,60]*NoOfRates-FRate[k]);
  i  := 60;
  j := 60;
  repeat
    dec(i,1);
    r1 := 0;
    for k := 0 to NoOfRates-1 do
      r1 := r1 +(NBY[k,i]*NoOfRates-FRate[k])*(NBY[k,i]*NoOfRates-FRate[k]);
    if r1 < r0 then
    begin
      r0 := r1;
      j := i;
    end;
  until i = 2;

  if FGamma < 0.01 then
    FGamma := exp(NBX[j])
  else
  begin
    maxg := sqrt(sqrt(2))*FGamma;
    ming := FGamma/sqrt(sqrt(2));
    FGamma := exp(NBX[j]);
    if FGamma > maxg then
      FGamma := maxg
    else if FGamma < ming then
      FGamma := ming;
  end;

  delta := 0;
  d := R(j);
  if d < 0.0001 then
    mind := 0.000000000001
  else
    mind := power(10, floor(log10(d))-8);
  rep := 0;
  repeat
    if abs(ddR(j)) > 0.000000000001 then
      if (ln(FGamma) -dR(j)/ddR(j)) > NBX[60] then
        FGamma := exp(NBX[60])
      else
        FGamma := exp(ln(FGamma) -dR(j)/ddR(j));
    if (j < 60) and (FGamma > exp(NBX[j+1])) then
    begin
      if j < 56 then
        inc(j);
      FGamma := exp(NBX[j]);
    end
    else if (j > 0) and (FGamma < exp(NBX[j-1])) then
    begin
      if j > 4 then
        dec(j);
      FGamma := exp(NBX[j]);
    end;
    delta := (ln(FGamma) -NBX[j]);

    dd := d -R(j);
    d  := d -dd;
    inc(rep);
  until (abs(dd) < mind) or (rep = 100);

  if FGamma < 0.05 then
    FGamma := 0.05;

end;
}

initialization
  //GammaX[0]  := -3.218875824868; GammaX[1]  := -3.132232427298; GammaX[2]  := -3.045589029728; GammaX[3]  := -2.958945632158; GammaX[4]  := -2.872302234588; GammaX[5]  := -2.785658837018; GammaX[6]  := -2.699015439448; GammaX[7]  := -2.612372041878; GammaX[8]  := -2.525728644308; GammaX[9]  := -2.439085246738;
  //GammaX[10] := -2.352441849168; GammaX[11] := -2.265798451598; GammaX[12] := -2.179155054028; GammaX[13] := -2.092511656458; GammaX[14] := -2.005868258888; GammaX[15] := -1.919224861318; GammaX[16] := -1.832581463748; GammaX[17] := -1.745938066178; GammaX[18] := -1.659294668608; GammaX[19] := -1.572651271038;
  //GammaX[20] := -1.486007873468; GammaX[21] := -1.399364475898; GammaX[22] := -1.312721078328; GammaX[23] := -1.226077680758; GammaX[24] := -1.139434283188; GammaX[25] := -1.052790885618; GammaX[26] := -0.966147488048; GammaX[27] := -0.879504090478; GammaX[28] := -0.792860692908; GammaX[29] := -0.706217295338;
  //GammaX[30] := -0.619573897768; GammaX[31] := -0.532930500198; GammaX[32] := -0.446287102628; GammaX[33] := -0.359643705058; GammaX[34] := -0.273000307488; GammaX[35] := -0.186356909918; GammaX[36] := -0.099713512348; GammaX[37] := -0.013070114778; GammaX[38] :=  0.073573282792; GammaX[39] :=  0.160216680362;
  //GammaX[40] :=  0.246860077932; GammaX[41] :=  0.333503475502; GammaX[42] :=  0.420146873072; GammaX[43] :=  0.506790270642; GammaX[44] :=  0.593433668211; GammaX[45] :=  0.680077065781; GammaX[46] :=  0.766720463351; GammaX[47] :=  0.853363860921; GammaX[48] :=  0.940007258491; GammaX[49] :=  1.026650656061;
  //GammaX[50] :=  1.113294053631; GammaX[51] :=  1.199937451201; GammaX[52] :=  1.286580848771; GammaX[53] :=  1.373224246341; GammaX[54] :=  1.459867643911; GammaX[55] :=  1.546511041481; GammaX[56] :=  1.633154439051; GammaX[57] :=  1.719797836621; GammaX[58] :=  1.806441234191; GammaX[59] :=  1.893084631761;
  //GammaX[60] :=  1.979728029331; GammaX[61] :=  2.066371426901; GammaX[62] :=  2.153014824471; GammaX[63] :=  2.239658222041; GammaX[64] :=  2.326301619611; GammaX[65] :=  2.412945017181; GammaX[66] :=  2.499588414751; GammaX[67] :=  2.586231812321; GammaX[68] :=  2.672875209891; GammaX[69] :=  2.759518607461;
  //GammaX[70] :=  2.846162005031; GammaX[71] :=  2.932805402601; GammaX[72] :=  3.019448800171; GammaX[73] :=  3.106092197741; GammaX[74] :=  3.192735595311; GammaX[75] :=  3.279378992881; GammaX[76] :=  3.366022390451; GammaX[77] :=  3.452665788021; GammaX[78] :=  3.539309185591; GammaX[79] :=  3.625952583161;
  //GammaX[80] :=  3.712595980731; GammaX[81] :=  3.799239378301; GammaX[82] :=  3.885882775871; GammaX[83] :=  3.972526173441; GammaX[84] :=  4.059169571011; GammaX[85] :=  4.145812968581; GammaX[86] :=  4.232456366151; GammaX[87] :=  4.319099763721; GammaX[88] :=  4.405743161291; GammaX[89] :=  4.492386558861;
  //GammaX[90] :=  4.579029956431; GammaX[91] :=  4.665673354001; GammaX[92] :=  4.752316751571; GammaX[93] :=  4.838960149141; GammaX[94] :=  4.925603546711; GammaX[95] :=  5.012246944281; GammaX[96] :=  5.098890341851; GammaX[97] :=  5.185533739421; GammaX[98] :=  5.272177136991; GammaX[99] :=  5.358820534561;

  //GammaY[0,0] := 3.62422E-50; GammaY[1,0] := 2.43218E-42; GammaY[2,0] := 6.14177E-38; GammaY[3,0] := 8.16717E-35; GammaY[4,0] := 2.16837E-32; GammaY[5,0] := 2.08236E-30; GammaY[6,0] := 9.92674E-29; GammaY[7,0] := 2.83559E-27; GammaY[8,0] := 5.47748E-26; GammaY[9,0] := 7.76881E-25;
  //GammaY[10,0] := 8.57832E-24; GammaY[11,0] := 7.70009E-23; GammaY[12,0] := 5.80628E-22; GammaY[13,0] := 3.77319E-21; GammaY[14,0] := 2.15649E-20; GammaY[15,0] := 1.10188E-19; GammaY[16,0] := 5.10166E-19; GammaY[17,0] := 2.16433E-18; GammaY[18,0] := 8.49266E-18; GammaY[19,0] := 3.10684E-17;
  //GammaY[20,0] := 1.06684E-16; GammaY[21,0] := 3.45881E-16; GammaY[22,0] := 1.06417E-15; GammaY[23,0] := 3.12085E-15; GammaY[24,0] := 8.75812E-15; GammaY[25,0] := 2.36003E-14; GammaY[26,0] := 6.12523E-14; GammaY[27,0] := 1.53535E-13; GammaY[28,0] := 3.7259E-13; GammaY[29,0] := 8.77298E-13;
  //GammaY[30,0] := 2.00823E-12; GammaY[31,0] := 4.47723E-12; GammaY[32,0] := 9.73731E-12; GammaY[33,0] := 2.06893E-11; GammaY[34,0] := 4.3005E-11; GammaY[35,0] := 8.75577E-11; GammaY[36,0] := 1.7481E-10; GammaY[37,0] := 3.426E-10; GammaY[38,0] := 6.5975E-10; GammaY[39,0] := 1.24947E-09;
  //GammaY[40,0] := 2.32911E-09; GammaY[41,0] := 4.27662E-09; GammaY[42,0] := 7.74048E-09; GammaY[43,0] := 1.38191E-08; GammaY[44,0] := 2.43503E-08; GammaY[45,0] := 4.23733E-08; GammaY[46,0] := 7.28582E-08; GammaY[47,0] := 1.23846E-07; GammaY[48,0] := 2.08214E-07; GammaY[49,0] := 3.46381E-07;
  //GammaY[50,0] := 5.70425E-07; GammaY[51,0] := 9.3028E-07; GammaY[52,0] := 1.50301E-06; GammaY[53,0] := 2.40654E-06; GammaY[54,0] := 3.81993E-06; GammaY[55,0] := 6.01288E-06; GammaY[56,0] := 9.38869E-06; GammaY[57,0] := 1.4546E-05; GammaY[58,0] := 2.23674E-05; GammaY[59,0] := 3.41453E-05;
  //GammaY[60,0] := 5.176E-05; GammaY[61,0] := 7.79301E-05; GammaY[62,0] := 0.000116563; GammaY[63,0] := 0.000173239; GammaY[64,0] := 0.000255889; GammaY[65,0] := 0.000375715; GammaY[66,0] := 0.000548462; GammaY[67,0] := 0.000796141; GammaY[68,0] := 0.001149374; GammaY[69,0] := 0.001650554;
  //GammaY[70,0] := 0.002358097; GammaY[71,0] := 0.003352137; GammaY[72,0] := 0.004742114; GammaY[73,0] := 0.006676866; GammaY[74,0] := 0.009357994; GammaY[75,0] := 0.013057502; GammaY[76,0] := 0.018141037; GammaY[77,0] := 0.025098415; GammaY[78,0] := 0.034583673; GammaY[79,0] := 0.047467552;
  //GammaY[80,0] := 0.064906306; GammaY[81,0] := 0.088431997; GammaY[82,0] := 0.120071343; GammaY[83,0] := 0.162502883; GammaY[84,0] := 0.219266279; GammaY[85,0] := 0.295043857; GammaY[86,0] := 0.39604442; GammaY[87,0] := 0.530535713; GammaY[88,0] := 0.709599675; GammaY[89,0] := 0.948233568;
  //GammaY[90,0] := 1.267010288; GammaY[91,0] := 1.694685951; GammaY[92,0] := 2.272502531; GammaY[93,0] := 3.061731028; GammaY[94,0] := 4.157954154; GammaY[95,0] := 5.720883236; GammaY[96,0] := 8.046166969; GammaY[97,0] := 11.77952164; GammaY[98,0] := 18.87167807; GammaY[99,0] := 39.42310254;
  //GammaY[0,1] := 4.70573E-46; GammaY[1,1] := 7.49549E-39; GammaY[2,1] := 8.16112E-35; GammaY[3,1] := 5.97773E-32; GammaY[4,1] := 1.00049E-29; GammaY[5,1] := 6.59855E-28; GammaY[6,1] := 2.29145E-26; GammaY[7,1] := 4.97694E-25; GammaY[8,1] := 7.55102E-24; GammaY[9,1] := 8.62815E-23;
  //GammaY[10,1] := 7.83395E-22; GammaY[11,1] := 5.88021E-21; GammaY[12,1] := 3.76041E-20; GammaY[13,1] := 2.09748E-19; GammaY[14,1] := 1.03964E-18; GammaY[15,1] := 4.64874E-18; GammaY[16,1] := 1.89857E-17; GammaY[17,1] := 7.15508E-17; GammaY[18,1] := 2.50983E-16; GammaY[19,1] := 8.25444E-16;
  //GammaY[20,1] := 2.5613E-15; GammaY[21,1] := 7.53881E-15; GammaY[22,1] := 2.11467E-14; GammaY[23,1] := 5.67613E-14; GammaY[24,1] := 1.46316E-13; GammaY[25,1] := 3.63357E-13; GammaY[26,1] := 8.71766E-13; GammaY[27,1] := 2.02572E-12; GammaY[28,1] := 4.56924E-12; GammaY[29,1] := 1.00247E-11;
  //GammaY[30,1] := 2.14315E-11; GammaY[31,1] := 4.47197E-11; GammaY[32,1] := 9.12138E-11; GammaY[33,1] := 1.82108E-10; GammaY[34,1] := 3.56324E-10; GammaY[35,1] := 6.84069E-10; GammaY[36,1] := 1.28988E-09; GammaY[37,1] := 2.39117E-09; GammaY[38,1] := 4.36181E-09; GammaY[39,1] := 7.83566E-09;
  //GammaY[40,1] := 1.38728E-08; GammaY[41,1] := 2.42238E-08; GammaY[42,1] := 4.17434E-08; GammaY[43,1] := 7.10344E-08; GammaY[44,1] := 1.19435E-07; GammaY[45,1] := 1.9852E-07; GammaY[46,1] := 3.26365E-07; GammaY[47,1] := 5.30922E-07; GammaY[48,1] := 8.5502E-07; GammaY[49,1] := 1.36369E-06;
  //GammaY[50,1] := 2.15486E-06; GammaY[51,1] := 3.37475E-06; GammaY[52,1] := 5.24001E-06; GammaY[53,1] := 8.0692E-06; GammaY[54,1] := 1.23273E-05; GammaY[55,1] := 1.86885E-05; GammaY[56,1] := 2.81231E-05; GammaY[57,1] := 4.20192E-05; GammaY[58,1] := 6.23499E-05; GammaY[59,1] := 9.19026E-05;
  //GammaY[60,1] := 0.000134592; GammaY[61,1] := 0.000195887; GammaY[62,1] := 0.000283379; GammaY[63,1] := 0.000407562; GammaY[64,1] := 0.000582855; GammaY[65,1] := 0.00082898; GammaY[66,1] := 0.001172784; GammaY[67,1] := 0.001650633; GammaY[68,1] := 0.00231158; GammaY[69,1] := 0.003221494;
  //GammaY[70,1] := 0.004468454; GammaY[71,1] := 0.006169769; GammaY[72,1] := 0.008481062; GammaY[73,1] := 0.011608021; GammaY[74,1] := 0.015821535; GammaY[75,1] := 0.021477165; GammaY[76,1] := 0.029040142; GammaY[77,1] := 0.039117424; GammaY[78,1] := 0.052498806; GammaY[79,1] := 0.070209655;
  //GammaY[80,1] := 0.09357871; GammaY[81,1] := 0.124325514; GammaY[82,1] := 0.16467374; GammaY[83,1] := 0.217499172; GammaY[84,1] := 0.286524781; GammaY[85,1] := 0.376581214; GammaY[86,1] := 0.493960246; GammaY[87,1] := 0.646904005; GammaY[88,1] := 0.846298719; GammaY[89,1] := 1.106687381;
  //GammaY[90,1] := 1.447799743; GammaY[91,1] := 1.89696053; GammaY[92,1] := 2.493070783; GammaY[93,1] := 3.293606145; GammaY[94,1] := 4.387821482; GammaY[95,1] := 5.924447208; GammaY[96,1] := 8.178228183; GammaY[97,1] := 11.74814247; GammaY[98,1] := 18.44050195; GammaY[99,1] := 37.56242844;
  //GammaY[0,2] := 2.76522E-42; GammaY[1,2] := 1.17797E-35; GammaY[2,2] := 5.93041E-32; GammaY[3,2] := 2.51499E-29; GammaY[4,2] := 2.75906E-27; GammaY[5,2] := 1.29026E-25; GammaY[6,2] := 3.35298E-24; GammaY[7,2] := 5.66693E-23; GammaY[8,2] := 6.89121E-22; GammaY[9,2] := 6.45899E-21;
  //GammaY[10,2] := 4.90094E-20; GammaY[11,2] := 3.12185E-19; GammaY[12,2] := 1.71622E-18; GammaY[13,2] := 8.32002E-18; GammaY[14,2] := 3.61839E-17; GammaY[15,2] := 1.43143E-16; GammaY[16,2] := 5.20989E-16; GammaY[17,2] := 1.76114E-15; GammaY[18,2] := 5.57331E-15; GammaY[19,2] := 1.66229E-14;
  //GammaY[20,2] := 4.69972E-14; GammaY[21,2] := 1.26579E-13; GammaY[22,2] := 3.26172E-13; GammaY[23,2] := 8.07149E-13; GammaY[24,2] := 1.9245E-12; GammaY[25,2] := 4.43409E-12; GammaY[26,2] := 9.89771E-12; GammaY[27,2] := 2.14541E-11; GammaY[28,2] := 4.52509E-11; GammaY[29,2] := 9.30442E-11;
  //GammaY[30,2] := 1.8682E-10; GammaY[31,2] := 3.66848E-10; GammaY[32,2] := 7.05462E-10; GammaY[33,2] := 1.33024E-09; GammaY[34,2] := 2.46235E-09; GammaY[35,2] := 4.47908E-09; GammaY[36,2] := 8.01423E-09; GammaY[37,2] := 1.41173E-08; GammaY[38,2] := 2.45028E-08; GammaY[39,2] := 4.19351E-08;
  //GammaY[40,2] := 7.08174E-08; GammaY[41,2] := 1.18082E-07; GammaY[42,2] := 1.94522E-07; GammaY[43,2] := 3.16766E-07; GammaY[44,2] := 5.10175E-07; GammaY[45,2] := 8.1306E-07; GammaY[46,2] := 1.28276E-06; GammaY[47,2] := 2.00435E-06; GammaY[48,2] := 3.10299E-06; GammaY[49,2] := 4.76134E-06;
  //GammaY[50,2] := 7.24392E-06; GammaY[51,2] := 1.09309E-05; GammaY[52,2] := 1.63651E-05; GammaY[53,2] := 2.43155E-05; GammaY[54,2] := 3.58653E-05; GammaY[55,2] := 5.25299E-05; GammaY[56,2] := 7.64167E-05; GammaY[57,2] := 0.000110439; GammaY[58,2] := 0.000158602; GammaY[59,2] := 0.00022638;
  //GammaY[60,2] := 0.000321218; GammaY[61,2] := 0.000453186; GammaY[62,2] := 0.000635844; GammaY[63,2] := 0.000887354; GammaY[64,2] := 0.001231938; GammaY[65,2] := 0.001701753; GammaY[66,2] := 0.002339303; GammaY[67,2] := 0.003200539; GammaY[68,2] := 0.004358813; GammaY[69,2] := 0.005909916;
  //GammaY[70,2] := 0.007978483; GammaY[71,2] := 0.010726087; GammaY[72,2] := 0.014361482; GammaY[73,2] := 0.019153499; GammaY[74,2] := 0.025447291; GammaY[75,2] := 0.033684738; GammaY[76,2] := 0.044430103; GammaY[77,2] := 0.058402266; GammaY[78,2] := 0.076515304; GammaY[79,2] := 0.099929679;
  //GammaY[80,2] := 0.130117072; GammaY[81,2] := 0.168942939; GammaY[82,2] := 0.218772424; GammaY[83,2] := 0.282607538; GammaY[84,2] := 0.364267012; GammaY[85,2] := 0.468625662; GammaY[86,2] := 0.601938748; GammaY[87,2] := 0.772291111; GammaY[88,2] := 0.990234995; GammaY[89,2] := 1.269723023;
  //GammaY[90,2] := 1.629520913; GammaY[91,2] := 2.095435397; GammaY[92,2] := 2.704002669; GammaY[93,2] := 3.508977003; GammaY[94,2] := 4.593576121; GammaY[95,2] := 6.096160141; GammaY[96,2] := 8.271881794; GammaY[97,2] := 11.67618191; GammaY[98,2] := 17.98126345; GammaY[99,2] := 35.76307892;
  //GammaY[0,3] := 7.8545E-39; GammaY[1,3] := 9.98378E-33; GammaY[2,3] := 2.47795E-29; GammaY[3,3] := 6.36991E-27; GammaY[4,3] := 4.74767E-25; GammaY[5,3] := 1.62106E-23; GammaY[6,3] := 3.23097E-22; GammaY[7,3] := 4.33998E-21; GammaY[8,3] := 4.30886E-20; GammaY[9,3] := 3.36759E-19;
  //GammaY[10,3] := 2.16725E-18; GammaY[11,3] := 1.18744E-17; GammaY[12,3] := 5.68152E-17; GammaY[13,3] := 2.42148E-16; GammaY[14,3] := 9.33933E-16; GammaY[15,3] := 3.30151E-15; GammaY[16,3] := 1.08099E-14; GammaY[17,3] := 3.30688E-14; GammaY[18,3] := 9.5209E-14; GammaY[19,3] := 2.59591E-13;
  //GammaY[20,3] := 6.73833E-13; GammaY[21,3] := 1.67281E-12; GammaY[22,3] := 3.98742E-12; GammaY[23,3] := 9.15778E-12; GammaY[24,3] := 2.03263E-11; GammaY[25,3] := 4.37178E-11; GammaY[26,3] := 9.13325E-11; GammaY[27,3] := 1.85728E-10; GammaY[28,3] := 3.68333E-10; GammaY[29,3] := 7.13598E-10;
  //GammaY[30,3] := 1.35264E-09; GammaY[31,3] := 2.51208E-09; GammaY[32,3] := 4.5767E-09; GammaY[33,3] := 8.18917E-09; GammaY[34,3] := 1.44062E-08; GammaY[35,3] := 2.49403E-08; GammaY[36,3] := 4.25282E-08; GammaY[37,3] := 7.1487E-08; GammaY[38,3] := 1.18544E-07; GammaY[39,3] := 1.94057E-07;
  //GammaY[40,3] := 3.13804E-07; GammaY[41,3] := 5.0156E-07; GammaY[42,3] := 7.92798E-07; GammaY[43,3] := 1.23994E-06; GammaY[44,3] := 1.91974E-06; GammaY[45,3] := 2.94364E-06; GammaY[46,3] := 4.47207E-06; GammaY[47,3] := 6.73416E-06; GammaY[48,3] := 1.00547E-05; GammaY[49,3] := 1.48907E-05;
  //GammaY[50,3] := 2.18807E-05; GammaY[51,3] := 3.19112E-05; GammaY[52,3] := 4.62043E-05; GammaY[53,3] := 6.64353E-05; GammaY[54,3] := 9.48863E-05; GammaY[55,3] := 0.000134649; GammaY[56,3] := 0.000189887; GammaY[57,3] := 0.00026618; GammaY[58,3] := 0.000370967; GammaY[59,3] := 0.000514113;
  //GammaY[60,3] := 0.000708642; GammaY[61,3] := 0.000971666; GammaY[62,3] := 0.001325573; GammaY[63,3] := 0.00179952; GammaY[64,3] := 0.002431327; GammaY[65,3] := 0.003269856; GammaY[66,3] := 0.004377985; GammaY[67,3] := 0.00583633; GammaY[68,3] := 0.007747883; GammaY[69,3] := 0.010243766;
  //GammaY[70,3] := 0.013490374; GammaY[71,3] := 0.017698203; GammaY[72,3] := 0.023132764; GammaY[73,3] := 0.030128039; GammaY[74,3] := 0.039103095; GammaY[75,3] := 0.050582556; GammaY[76,3] := 0.065221897; GammaY[77,3] := 0.083838729; GammaY[78,3] := 0.107451618; GammaY[79,3] := 0.137328467;
  //GammaY[80,3] := 0.175047166; GammaY[81,3] := 0.222572202; GammaY[82,3] := 0.282352363; GammaY[83,3] := 0.357446786; GammaY[84,3] := 0.451689915; GammaY[85,3] := 0.569910936; GammaY[86,3] := 0.718231447; GammaY[87,3] := 0.904478331; GammaY[88,3] := 1.138771375; GammaY[89,3] := 1.434384702;
  //GammaY[90,3] := 1.809053631; GammaY[91,3] := 2.287038588; GammaY[92,3] := 2.902552786; GammaY[93,3] := 3.705749005; GammaY[94,3] := 4.774116538; GammaY[95,3] := 6.236284278; GammaY[96,3] := 8.329093266; GammaY[97,3] := 11.56745771; GammaY[98,3] := 17.49911776; GammaY[99,3] := 34.02618146;
  //GammaY[0,4] := 1.14555E-35; GammaY[1,4] := 4.80347E-30; GammaY[2,4] := 6.23405E-27; GammaY[3,4] := 1.0133E-24; GammaY[4,4] := 5.30312E-23; GammaY[5,4] := 1.35803E-21; GammaY[6,4] := 2.12314E-20; GammaY[7,4] := 2.31065E-19; GammaY[8,4] := 1.9048E-18; GammaY[9,4] := 1.26009E-17;
  //GammaY[10,4] := 6.97156E-17; GammaY[11,4] := 3.32621E-16; GammaY[12,4] := 1.40092E-15; GammaY[13,4] := 5.30461E-15; GammaY[14,4] := 1.83223E-14; GammaY[15,4] := 5.84117E-14; GammaY[16,4] := 1.73541E-13; GammaY[17,4] := 4.84363E-13; GammaY[18,4] := 1.27857E-12; GammaY[19,4] := 3.21025E-12;
  //GammaY[20,4] := 7.70428E-12; GammaY[21,4] := 1.77471E-11; GammaY[22,4] := 3.93828E-11; GammaY[23,4] := 8.44598E-11; GammaY[24,4] := 1.75538E-10; GammaY[25,4] := 3.54438E-10; GammaY[26,4] := 6.96795E-10; GammaY[27,4] := 1.33633E-09; GammaY[28,4] := 2.50451E-09; GammaY[29,4] := 4.59422E-09;
  //GammaY[30,4] := 8.26027E-09; GammaY[31,4] := 1.45756E-08; GammaY[32,4] := 2.52702E-08; GammaY[33,4] := 4.30927E-08; GammaY[34,4] := 7.23481E-08; GammaY[35,4] := 1.19691E-07; GammaY[36,4] := 1.95282E-07; GammaY[37,4] := 3.1445E-07; GammaY[38,4] := 5.00063E-07; GammaY[39,4] := 7.85885E-07;
  //GammaY[40,4] := 1.22127E-06; GammaY[41,4] := 1.87765E-06; GammaY[42,4] := 2.85752E-06; GammaY[43,4] := 4.30669E-06; GammaY[44,4] := 6.4308E-06; GammaY[45,4] := 9.5177E-06; GammaY[46,4] := 1.39673E-05; GammaY[47,4] := 2.03311E-05; GammaY[48,4] := 2.93647E-05; GammaY[49,4] := 4.20961E-05;
  //GammaY[50,4] := 5.99156E-05; GammaY[51,4] := 8.46919E-05; GammaY[52,4] := 0.000118922; GammaY[53,4] := 0.000165924; GammaY[54,4] := 0.000230084; GammaY[55,4] := 0.000317168; GammaY[56,4] := 0.00043472; GammaY[57,4] := 0.000592564; GammaY[58,4] := 0.000803431; GammaY[59,4] := 0.001083746;
  //GammaY[60,4] := 0.001454613; GammaY[61,4] := 0.00194303; GammaY[62,4] := 0.002583399; GammaY[63,4] := 0.003419392; GammaY[64,4] := 0.004506234; GammaY[65,4] := 0.005913519; GammaY[66,4] := 0.007728645; GammaY[67,4] := 0.010061009; GammaY[68,4] := 0.013047114; GammaY[69,4] := 0.016856786;
  //GammaY[70,4] := 0.021700707; GammaY[71,4] := 0.027839569; GammaY[72,4] := 0.035595151; GammaY[73,4] := 0.04536377; GammaY[74,4] := 0.057632583; GammaY[75,4] := 0.072999414; GammaY[76,4] := 0.092196899; GammaY[77,4] := 0.116122027; GammaY[78,4] := 0.145872431; GammaY[79,4] := 0.182791268;
  //GammaY[80,4] := 0.228523147; GammaY[81,4] := 0.285084497; GammaY[82,4] := 0.354953093; GammaY[83,4] := 0.441183487; GammaY[84,4] := 0.547558125; GammaY[85,4] := 0.678788701; GammaY[86,4] := 0.840789813; GammaY[87,4] := 1.041059419; GammaY[88,4] := 1.289221464; GammaY[89,4] := 1.597822838;
  //GammaY[90,4] := 1.983544095; GammaY[91,4] := 2.469120819; GammaY[92,4] := 3.086492451; GammaY[93,4] := 3.882395622; GammaY[94,4] := 4.928931381; GammaY[95,4] := 6.345589591; GammaY[96,4] := 8.35213953; GammaY[97,4] := 11.42580327; GammaY[98,4] := 16.99886403; GammaY[99,4] := 32.35248184;
  //GammaY[0,5] := 9.06714E-33; GammaY[1,5] := 1.37514E-27; GammaY[2,5] := 9.85083E-25; GammaY[3,5] := 1.0526E-22; GammaY[4,5] := 3.98707E-21; GammaY[5,5] := 7.8478E-20; GammaY[6,5] := 9.82315E-19; GammaY[7,5] := 8.81505E-18; GammaY[8,5] := 6.127E-17; GammaY[9,5] := 3.47797E-16;
  //GammaY[10,5] := 1.67477E-15; GammaY[11,5] := 7.03695E-15; GammaY[12,5] := 2.63612E-14; GammaY[13,5] := 8.95376E-14; GammaY[14,5] := 2.79461E-13; GammaY[15,5] := 8.10246E-13; GammaY[16,5] := 2.20168E-12; GammaY[17,5] := 5.64859E-12; GammaY[18,5] := 1.37678E-11; GammaY[19,5] := 3.20485E-11;
  //GammaY[20,5] := 7.15678E-11; GammaY[21,5] := 1.53912E-10; GammaY[22,5] := 3.19839E-10; GammaY[23,5] := 6.44112E-10; GammaY[24,5] := 1.26031E-09; GammaY[25,5] := 2.40141E-09; GammaY[26,5] := 4.46478E-09; GammaY[27,5] := 8.11439E-09; GammaY[28,5] := 1.44388E-08; GammaY[29,5] := 2.51912E-08;
  //GammaY[30,5] := 4.31493E-08; GammaY[31,5] := 7.26467E-08; GammaY[32,5] := 1.20348E-07; GammaY[33,5] := 1.96364E-07; GammaY[34,5] := 3.15843E-07; GammaY[35,5] := 5.01208E-07; GammaY[36,5] := 7.85283E-07; GammaY[37,5] := 1.2156E-06; GammaY[38,5] := 1.86032E-06; GammaY[39,5] := 2.81624E-06;
  //GammaY[40,5] := 4.21958E-06; GammaY[41,5] := 6.26044E-06; GammaY[42,5] := 9.20192E-06; GammaY[43,5] := 1.34053E-05; GammaY[44,5] := 1.93632E-05; GammaY[45,5] := 2.77421E-05; GammaY[46,5] := 3.94385E-05; GammaY[47,5] := 5.56495E-05; GammaY[48,5] := 7.79644E-05; GammaY[49,5] := 0.000108481;
  //GammaY[50,5] := 0.000149951; GammaY[51,5] := 0.000205967; GammaY[52,5] := 0.000281191; GammaY[53,5] := 0.000381647; GammaY[54,5] := 0.000515078; GammaY[55,5] := 0.000691391; GammaY[56,5] := 0.000923205; GammaY[57,5] := 0.001226525; GammaY[58,5] := 0.001621567; GammaY[59,5] := 0.00213376;
  //GammaY[60,5] := 0.002794972; GammaY[61,5] := 0.003644992; GammaY[62,5] := 0.004733318; GammaY[63,5] := 0.006121321; GammaY[64,5] := 0.007884842; GammaY[65,5] := 0.010117306; GammaY[66,5] := 0.012933464; GammaY[67,5] := 0.016473855; GammaY[68,5] := 0.02091014; GammaY[69,5] := 0.026451475;
  //GammaY[70,5] := 0.0333521; GammaY[71,5] := 0.041920412; GammaY[72,5] := 0.052529781; GammaY[73,5] := 0.065631498; GammaY[74,5] := 0.081770288; GammaY[75,5] := 0.101602969; GammaY[76,5] := 0.125920976; GammaY[77,5] := 0.155677717; GammaY[78,5] := 0.192021987; GammaY[79,5] := 0.236339146;
  //GammaY[80,5] := 0.290302301; GammaY[81,5] := 0.355936657; GammaY[82,5] := 0.43570141; GammaY[83,5] := 0.532595473; GammaY[84,5] := 0.65029616; GammaY[85,5] := 0.793344364; GammaY[86,5] := 0.967396826; GammaY[87,5] := 1.179577575; GammaY[88,5] := 1.438980068; GammaY[89,5] := 1.757405637;
  //GammaY[90,5] := 2.150486304; GammaY[91,5] := 2.639466955; GammaY[92,5] := 3.254128505; GammaY[93,5] := 4.037975022; GammaY[94,5] := 5.058032169; GammaY[95,5] := 6.425257377; GammaY[96,5] := 8.343515772; GammaY[97,5] := 11.25499918; GammaY[98,5] := 16.48493272; GammaY[99,5] := 30.74233697;
  //GammaY[0,6] := 4.09768E-30; GammaY[1,6] := 2.44571E-25; GammaY[2,6] := 1.01637E-22; GammaY[3,6] := 7.39981E-21; GammaY[4,6] := 2.0858E-19; GammaY[5,6] := 3.22709E-18; GammaY[6,6] := 3.29492E-17; GammaY[7,6] := 2.47728E-16; GammaY[8,6] := 1.47225E-15; GammaY[9,6] := 7.26124E-15;
  //GammaY[10,6] := 3.07781E-14; GammaY[11,6] := 1.15069E-13; GammaY[12,6] := 3.87069E-13; GammaY[13,6] := 1.18976E-12; GammaY[14,6] := 3.38331E-12; GammaY[15,6] := 8.99018E-12; GammaY[16,6] := 2.2506E-11; GammaY[17,6] := 5.34423E-11; GammaY[18,6] := 1.21062E-10; GammaY[19,6] := 2.62884E-10;
  //GammaY[20,6] := 5.4948E-10; GammaY[21,6] := 1.10946E-09; GammaY[22,6] := 2.17063E-09; GammaY[23,6] := 4.12613E-09; GammaY[24,6] := 7.63848E-09; GammaY[25,6] := 1.38002E-08; GammaY[26,6] := 2.43768E-08; GammaY[27,6] := 4.21697E-08; GammaY[28,6] := 7.15476E-08; GammaY[29,6] := 1.19216E-07;
  //GammaY[30,6] := 1.95316E-07; GammaY[31,6] := 3.14971E-07; GammaY[32,6] := 5.00451E-07; GammaY[33,6] := 7.84143E-07; GammaY[34,6] := 1.21262E-06; GammaY[35,6] := 1.85215E-06; GammaY[36,6] := 2.79604E-06; GammaY[37,6] := 4.17446E-06; GammaY[38,6] := 6.16734E-06; GammaY[39,6] := 9.02129E-06;
  //GammaY[40,6] := 1.30716E-05; GammaY[41,6] := 1.87704E-05; GammaY[42,6] := 2.67234E-05; GammaY[43,6] := 3.7736E-05; GammaY[44,6] := 5.2872E-05; GammaY[45,6] := 7.35279E-05; GammaY[46,6] := 0.000101525; GammaY[47,6] := 0.000139227; GammaY[48,6] := 0.000189682; GammaY[49,6] := 0.000256801;
  //GammaY[50,6] := 0.000345575; GammaY[51,6] := 0.000462345; GammaY[52,6] := 0.00061513; GammaY[53,6] := 0.00081402; GammaY[54,6] := 0.001071659; GammaY[55,6] := 0.001403828; GammaY[56,6] := 0.001830142; GammaY[57,6] := 0.002374887; GammaY[58,6] := 0.00306802; GammaY[59,6] := 0.003946368;
  //GammaY[60,6] := 0.005055043; GammaY[61,6] := 0.006449128; GammaY[62,6] := 0.00819567; GammaY[63,6] := 0.010376033; GammaY[64,6] := 0.013088679; GammaY[65,6] := 0.016452432; GammaY[66,6] := 0.020610334; GammaY[67,6] := 0.02573416; GammaY[68,6] := 0.032029739; GammaY[69,6] := 0.039743208;
  //GammaY[70,6] := 0.049168364; GammaY[71,6] := 0.060655334; GammaY[72,6] := 0.074620816; GammaY[73,6] := 0.0915602; GammaY[74,6] := 0.11206199; GammaY[75,6] := 0.136825023; GammaY[76,6] := 0.166679171; GammaY[77,6] := 0.20261037; GammaY[78,6] := 0.245791163; GammaY[79,6] := 0.297618282;
  //GammaY[80,6] := 0.359759397; GammaY[81,6] := 0.434211958; GammaY[82,6] := 0.523378219; GammaY[83,6] := 0.630162311; GammaY[84,6] := 0.758097873; GammaY[85,6] := 0.911518857; GammaY[86,6] := 1.095792681; GammaY[87,6] := 1.317645568; GammaY[88,6] := 1.585627951; GammaY[89,6] := 1.910799451;
  //GammaY[90,6] := 2.307777161; GammaY[91,6] := 2.796363492; GammaY[92,6] := 3.404292984; GammaY[93,6] := 4.172040243; GammaY[94,6] := 5.161873009; GammaY[95,6] := 6.47678965; GammaY[96,6] := 8.305854956; GammaY[97,6] := 11.05872183; GammaY[98,6] := 15.96138531; GammaY[99,6] := 29.19571274;
  //GammaY[0,7] := 1.10777E-27; GammaY[1,7] := 2.81141E-23; GammaY[2,7] := 7.09536E-21; GammaY[3,7] := 3.63792E-19; GammaY[4,7] := 7.82719E-18; GammaY[5,7] := 9.71502E-17; GammaY[6,7] := 8.22959E-16; GammaY[7,7] := 5.25994E-15; GammaY[8,7] := 2.70719E-14; GammaY[9,7] := 1.17342E-13;
  //GammaY[10,7] := 4.42356E-13; GammaY[11,7] := 1.48555E-12; GammaY[12,7] := 4.52638E-12; GammaY[13,7] := 1.26933E-11; GammaY[14,7] := 3.31365E-11; GammaY[15,7] := 8.12726E-11; GammaY[16,7] := 1.88698E-10; GammaY[17,7] := 4.17345E-10; GammaY[18,7] := 8.83919E-10; GammaY[19,7] := 1.80075E-09;
  //GammaY[20,7] := 3.54216E-09; GammaY[21,7] := 6.7496E-09; GammaY[22,7] := 1.24944E-08; GammaY[23,7] := 2.25246E-08; GammaY[24,7] := 3.96318E-08; GammaY[25,7] := 6.81882E-08; GammaY[26,7] := 1.14919E-07; GammaY[27,7] := 1.89997E-07; GammaY[28,7] := 3.08577E-07; GammaY[29,7] := 4.92912E-07;
  //GammaY[30,7] := 7.7525E-07; GammaY[31,7] := 1.20174E-06; GammaY[32,7] := 1.83765E-06; GammaY[33,7] := 2.77433E-06; GammaY[34,7] := 4.13828E-06; GammaY[35,7] := 6.10302E-06; GammaY[36,7] := 8.90439E-06; GammaY[37,7] := 1.28603E-05; GammaY[38,7] := 1.83955E-05; GammaY[39,7] := 2.60738E-05;
  //GammaY[40,7] := 3.66373E-05; GammaY[41,7] := 5.10567E-05; GammaY[42,7] := 7.05932E-05; GammaY[43,7] := 9.6875E-05; GammaY[44,7] := 0.000131992; GammaY[45,7] := 0.00017861; GammaY[46,7] := 0.000240114; GammaY[47,7] := 0.000320778; GammaY[48,7] := 0.000425969; GammaY[49,7] := 0.0005624;
  //GammaY[50,7] := 0.000738425; GammaY[51,7] := 0.000964396; GammaY[52,7] := 0.001253089; GammaY[53,7] := 0.001620205; GammaY[54,7] := 0.002084966; GammaY[55,7] := 0.002670819; GammaY[56,7] := 0.003406262; GammaY[57,7] := 0.004325814; GammaY[58,7] := 0.00547115; GammaY[59,7] := 0.006892431;
  //GammaY[60,7] := 0.008649843; GammaY[61,7] := 0.010815405; GammaY[62,7] := 0.013475058; GammaY[63,7] := 0.016731091; GammaY[64,7] := 0.020704961; GammaY[65,7] := 0.025540558; GammaY[66,7] := 0.031407989; GammaY[67,7] := 0.038507975; GammaY[68,7] := 0.047076944; GammaY[69,7] := 0.057392963;
  //GammaY[70,7] := 0.069782644; GammaY[71,7] := 0.084629216; GammaY[72,7] := 0.10238199; GammaY[73,7] := 0.123567508; GammaY[74,7] := 0.148802743; GammaY[75,7] := 0.17881082; GammaY[76,7] := 0.214439884; GammaY[77,7] := 0.256685912; GammaY[78,7] := 0.306720549; GammaY[79,7] := 0.365925422;
  //GammaY[80,7] := 0.435934904; GammaY[81,7] := 0.51869005; GammaY[82,7] := 0.616507547; GammaY[83,7] := 0.732169133; GammaY[84,7] := 0.869039414; GammaY[85,7] := 1.031223838; GammaY[86,7] := 1.223784634; GammaY[87,7] := 1.453042471; GammaY[88,7] := 1.727008289; GammaY[89,7] := 2.05602511;
  //GammaY[90,7] := 2.453716591; GammaY[91,7] := 2.938529738; GammaY[92,7] := 3.536280955; GammaY[93,7] := 4.284585955; GammaY[94,7] := 5.241276401; GammaY[95,7] := 6.501934253; GammaY[96,7] := 8.241860664; GammaY[97,7] := 10.84050136; GammaY[98,7] := 15.43191042; GammaY[99,7] := 27.71230189;
  //GammaY[0,8] := 1.86966E-25; GammaY[1,8] := 2.16604E-21; GammaY[2,8] := 3.46304E-19; GammaY[3,8] := 1.2889E-17; GammaY[4,8] := 2.16644E-16; GammaY[5,8] := 2.19756E-15; GammaY[6,8] := 1.56847E-14; GammaY[7,8] := 8.6359E-14; GammaY[8,8] := 3.89441E-13; GammaY[9,8] := 1.49904E-12;
  //GammaY[10,8] := 5.07385E-12; GammaY[11,8] := 1.54389E-11; GammaY[12,8] := 4.29531E-11; GammaY[13,8] := 1.10714E-10; GammaY[14,8] := 2.67178E-10; GammaY[15,8] := 6.08807E-10; GammaY[16,8] := 1.31904E-09; GammaY[17,8] := 2.73305E-09; GammaY[18,8] := 5.44187E-09; GammaY[19,8] := 1.04554E-08;
  //GammaY[20,8] := 1.9451E-08; GammaY[21,8] := 3.5145E-08; GammaY[22,8] := 6.18354E-08; GammaY[23,8] := 1.06182E-07; GammaY[24,8] := 1.7831E-07; GammaY[25,8] := 2.93342E-07; GammaY[26,8] := 4.73506E-07; GammaY[27,8] := 7.50986E-07; GammaY[28,8] := 1.17175E-06; GammaY[29,8] := 1.80061E-06;
  //GammaY[30,8] := 2.72786E-06; GammaY[31,8] := 4.07793E-06; GammaY[32,8] := 6.02046E-06; GammaY[33,8] := 8.78453E-06; GammaY[34,8] := 1.26767E-05; GammaY[35,8] := 1.81034E-05; GammaY[36,8] := 2.55998E-05; GammaY[37,8] := 3.58642E-05; GammaY[38,8] := 4.98021E-05; GammaY[39,8] := 6.85792E-05;
  //GammaY[40,8] := 9.36862E-05; GammaY[41,8] := 0.000127018; GammaY[42,8] := 0.000170969; GammaY[43,8] := 0.000228549; GammaY[44,8] := 0.000303518; GammaY[45,8] := 0.000400552; GammaY[46,8] := 0.000525441; GammaY[47,8] := 0.000685311; GammaY[48,8] := 0.000888905; GammaY[49,8] := 0.001146899;
  //GammaY[50,8] := 0.001472275; GammaY[51,8] := 0.00188076; GammaY[52,8] := 0.002391335; GammaY[53,8] := 0.003026828; GammaY[54,8] := 0.003814597; GammaY[55,8] := 0.004787325; GammaY[56,8] := 0.005983931; GammaY[57,8] := 0.007450626; GammaY[58,8] := 0.009242119; GammaY[59,8] := 0.011423005;
  //GammaY[60,8] := 0.014069361; GammaY[61,8] := 0.017270561; GammaY[62,8] := 0.021131368; GammaY[63,8] := 0.025774321; GammaY[64,8] := 0.031342464; GammaY[65,8] := 0.03800248; GammaY[66,8] := 0.045948284; GammaY[67,8] := 0.05540514; GammaY[68,8] := 0.066634414; GammaY[69,8] := 0.079939049;
  //GammaY[70,8] := 0.095669918; GammaY[71,8] := 0.114233201; GammaY[72,8] := 0.136099023; GammaY[73,8] := 0.161811587; GammaY[74,8] := 0.192001184; GammaY[75,8] := 0.227398481; GammaY[76,8] := 0.268851688; GammaY[77,8] := 0.317347353; GammaY[78,8] := 0.374035784; GammaY[79,8] := 0.440262468;
  //GammaY[80,8] := 0.51760732; GammaY[81,8] := 0.607934313; GammaY[82,8] := 0.713455038; GammaY[83,8] := 0.836811383; GammaY[84,8] := 0.981184566; GammaY[85,8] := 1.1504415; GammaY[86,8] := 1.349335154; GammaY[87,8] := 1.583784525; GammaY[88,8] := 1.861280769; GammaY[89,8] := 2.191461719;
  //GammaY[90,8] := 2.587020685; GammaY[91,8] := 3.065135495; GammaY[92,8] := 3.649805146; GammaY[93,8] := 4.375979852; GammaY[94,8] := 5.297354319; GammaY[95,8] := 6.502607614; GammaY[96,8] := 8.154249579; GammaY[97,8] := 10.60369384; GammaY[98,8] := 14.89983794; GammaY[99,8] := 26.29151032;
  //GammaY[0,9] := 2.04882E-23; GammaY[1,9] := 1.15639E-19; GammaY[2,9] := 1.21772E-17; GammaY[3,9] := 3.3829E-16; GammaY[4,9] := 4.53697E-15; GammaY[5,9] := 3.82498E-14; GammaY[6,9] := 2.33271E-13; GammaY[7,9] := 1.11988E-12; GammaY[8,9] := 4.47231E-12; GammaY[9,9] := 1.54348E-11;
  //GammaY[10,9] := 4.73151E-11; GammaY[11,9] := 1.31493E-10; GammaY[12,9] := 3.36504E-10; GammaY[13,9] := 8.02693E-10; GammaY[14,9] := 1.80214E-09; GammaY[15,9] := 3.83805E-09; GammaY[16,9] := 7.80365E-09; GammaY[17,9] := 1.52286E-08; GammaY[18,9] := 2.86507E-08; GammaY[19,9] := 5.21628E-08;
  //GammaY[20,9] := 9.2201E-08; GammaY[21,9] := 1.58658E-07; GammaY[22,9] := 2.66431E-07; GammaY[23,9] := 4.37532E-07; GammaY[24,9] := 7.03944E-07; GammaY[25,9] := 1.1114E-06; GammaY[26,9] := 1.72438E-06; GammaY[27,9] := 2.63255E-06; GammaY[28,9] := 3.95914E-06; GammaY[29,9] := 5.87151E-06;
  //GammaY[30,9] := 8.59462E-06; GammaY[31,9] := 1.24278E-05; GammaY[32,9] := 1.77656E-05; GammaY[33,9] := 2.51238E-05; GammaY[34,9] := 3.51708E-05; GammaY[35,9] := 4.87668E-05; GammaY[36,9] := 6.70097E-05; GammaY[37,9] := 9.12927E-05; GammaY[38,9] := 0.000123371; GammaY[39,9] := 0.000165442;
  //GammaY[40,9] := 0.000220245; GammaY[41,9] := 0.000291169; GammaY[42,9] := 0.000382389; GammaY[43,9] := 0.000499025; GammaY[44,9] := 0.000647321; GammaY[45,9] := 0.000834861; GammaY[46,9] := 0.001070813; GammaY[47,9] := 0.00136622; GammaY[48,9] := 0.001734328; GammaY[49,9] := 0.002190962;
  //GammaY[50,9] := 0.00275497; GammaY[51,9] := 0.003448712; GammaY[52,9] := 0.004298636; GammaY[53,9] := 0.005335925; GammaY[54,9] := 0.006597232; GammaY[55,9] := 0.008125522; GammaY[56,9] := 0.009971022; GammaY[57,9] := 0.012192291; GammaY[58,9] := 0.014857448; GammaY[59,9] := 0.01804554;
  //GammaY[60,9] := 0.021848107; GammaY[61,9] := 0.026370941; GammaY[62,9] := 0.031736079; GammaY[63,9] := 0.03808406; GammaY[64,9] := 0.045576488; GammaY[65,9] := 0.054398937; GammaY[66,9] := 0.064764262; GammaY[67,9] := 0.076916383; GammaY[68,9] := 0.091134613; GammaY[69,9] := 0.107738647;
  //GammaY[70,9] := 0.127094321; GammaY[71,9] := 0.1496203; GammaY[72,9] := 0.175795911; GammaY[73,9] := 0.206170331; GammaY[74,9] := 0.241373493; GammaY[75,9] := 0.282129091; GammaY[76,9] := 0.329270242; GammaY[77,9] := 0.383758498; GammaY[78,9] := 0.446707166; GammaY[79,9] := 0.519410186;
  //GammaY[80,9] := 0.603378287; GammaY[81,9] := 0.700384847; GammaY[82,9] := 0.812524703; GammaY[83,9] := 0.942290684; GammaY[84,9] := 1.092674764; GammaY[85,9] := 1.26730394; GammaY[86,9] := 1.470626232; GammaY[87,9] := 1.708175658; GammaY[88,9] := 1.986929447; GammaY[89,9] := 2.31586672;
  //GammaY[90,9] := 2.706811853; GammaY[91,9] := 3.175736734; GammaY[92,9] := 3.744945687; GammaY[93,9] := 4.446900239; GammaY[94,9] := 5.331442574; GammaY[95,9] := 6.480833134; GammaY[96,9] := 8.045699654; GammaY[97,9] := 10.35145373; GammaY[98,9] := 14.36814516; GammaY[99,9] := 24.93250123;
  //GammaY[0,10] := 1.51112E-21; GammaY[1,10] := 4.4108E-18; GammaY[2,10] := 3.17125E-16; GammaY[3,10] := 6.74564E-15; GammaY[4,10] := 7.35834E-14; GammaY[5,10] := 5.23544E-13; GammaY[6,10] := 2.7633E-12; GammaY[7,10] := 1.16954E-11; GammaY[8,10] := 4.1768E-11; GammaY[9,10] := 1.30382E-10;
  //GammaY[10,10] := 3.64888E-10; GammaY[11,10] := 9.32967E-10; GammaY[12,10] := 2.21105E-09; GammaY[13,10] := 4.91178E-09; GammaY[14,10] := 1.03198E-08; GammaY[15,10] := 2.06549E-08; GammaY[16,10] := 3.96159E-08; GammaY[17,10] := 7.31701E-08; GammaY[18,10] := 1.30677E-07; GammaY[19,10] := 2.2645E-07;
  //GammaY[20,10] := 3.81893E-07; GammaY[21,10] := 6.28368E-07; GammaY[22,10] := 1.01099E-06; GammaY[23,10] := 1.5936E-06; GammaY[24,10] := 2.46513E-06; GammaY[25,10] := 3.74782E-06; GammaY[26,10] := 5.60746E-06; GammaY[27,10] := 8.26635E-06; GammaY[28,10] := 1.20192E-05; GammaY[29,10] := 1.7253E-05;
  //GammaY[30,10] := 2.44707E-05; GammaY[31,10] := 3.43207E-05; GammaY[32,10] := 4.76315E-05; GammaY[33,10] := 6.54543E-05; GammaY[34,10] := 8.91125E-05; GammaY[35,10] := 0.000120261; GammaY[36,10] := 0.000160956; GammaY[37,10] := 0.000213738; GammaY[38,10] := 0.000281725; GammaY[39,10] := 0.000368726;
  //GammaY[40,10] := 0.000479368; GammaY[41,10] := 0.000619247; GammaY[42,10] := 0.000795095; GammaY[43,10] := 0.001014982; GammaY[44,10] := 0.001288533; GammaY[45,10] := 0.001627187; GammaY[46,10] := 0.002044488; GammaY[47,10] := 0.002556409; GammaY[48,10] := 0.003181729; GammaY[49,10] := 0.003942447;
  //GammaY[50,10] := 0.004864259; GammaY[51,10] := 0.005977084; GammaY[52,10] := 0.007315663; GammaY[53,10] := 0.008920226; GammaY[54,10] := 0.01083724; GammaY[55,10] := 0.013120243; GammaY[56,10] := 0.015830786; GammaY[57,10] := 0.019039471; GammaY[58,10] := 0.022827127; GammaY[59,10] := 0.027286115;
  //GammaY[60,10] := 0.032521794; GammaY[61,10] := 0.038654163; GammaY[62,10] := 0.0458197; GammaY[63,10] := 0.05417344; GammaY[64,10] := 0.063891308; GammaY[65,10] := 0.075172766; GammaY[66,10] := 0.08824381; GammaY[67,10] := 0.103360391; GammaY[68,10] := 0.120812328; GammaY[69,10] := 0.140927808;
  //GammaY[70,10] := 0.164078591; GammaY[71,10] := 0.190686071; GammaY[72,10] := 0.221228362; GammaY[73,10] := 0.256248663; GammaY[74,10] := 0.29636519; GammaY[75,10] := 0.34228306; GammaY[76,10] := 0.39480865; GammaY[77,10] := 0.45486706; GammaY[78,10] := 0.523523595; GammaY[79,10] := 0.602010409;
  //GammaY[80,10] := 0.691759988; GammaY[81,10] := 0.794447588; GammaY[82,10] := 0.912045757; GammaY[83,10] := 1.04689541; GammaY[84,10] := 1.201799737; GammaY[85,10] := 1.380150432; GammaY[86,10] := 1.58610526; GammaY[87,10] := 1.824814906; GammaY[88,10] := 2.102779119; GammaY[89,10] := 2.428365737;
  //GammaY[90,10] := 2.812568414; GammaY[91,10] := 3.270234349; GammaY[92,10] := 3.822086544; GammaY[93,10] := 4.498269352; GammaY[94,10] := 5.345039058; GammaY[95,10] := 6.438688922; GammaY[96,10] := 7.918816859; GammaY[97,10] := 10.08672452; GammaY[98,10] := 13.8394768; GammaY[99,10] := 23.63422651;
  //GammaY[0,11] := 7.75352E-20; GammaY[1,11] := 1.23623E-16; GammaY[2,11] := 6.27337E-15; GammaY[3,11] := 1.0458E-13; GammaY[4,11] := 9.44153E-13; GammaY[5,11] := 5.74849E-12; GammaY[6,11] := 2.65663E-11; GammaY[7,11] := 1.00133E-10; GammaY[8,11] := 3.22674E-10; GammaY[9,11] := 9.18423E-10;
  //GammaY[10,11] := 2.36379E-09; GammaY[11,11] := 5.59811E-09; GammaY[12,11] := 1.23629E-08; GammaY[13,11] := 2.57247E-08; GammaY[14,11] := 5.08529E-08; GammaY[15,11] := 9.61395E-08; GammaY[16,11] := 1.74773E-07; GammaY[17,11] := 3.06898E-07; GammaY[18,11] := 5.22518E-07; GammaY[19,11] := 8.6533E-07;
  //GammaY[20,11] := 1.39772E-06; GammaY[21,11] := 2.20718E-06; GammaY[22,11] := 3.41437E-06; GammaY[23,11] := 5.18336E-06; GammaY[24,11] := 7.73413E-06; GammaY[25,11] := 1.13581E-05; GammaY[26,11] := 1.64369E-05; GammaY[27,11] := 2.34652E-05; GammaY[28,11] := 3.30777E-05; GammaY[29,11] := 4.6082E-05;
  //GammaY[30,11] := 6.34964E-05; GammaY[31,11] := 8.65956E-05; GammaY[32,11] := 0.000116963; GammaY[33,11] := 0.000156553; GammaY[34,11] := 0.000207761; GammaY[35,11] := 0.000273507; GammaY[36,11] := 0.000357328; GammaY[37,11] := 0.000463487; GammaY[38,11] := 0.000597099; GammaY[39,11] := 0.000764263;
  //GammaY[40,11] := 0.000972228; GammaY[41,11] := 0.001229569; GammaY[42,11] := 0.001546386; GammaY[43,11] := 0.001934532; GammaY[44,11] := 0.002407865; GammaY[45,11] := 0.002982531; GammaY[46,11] := 0.003677278; GammaY[47,11] := 0.004513808; GammaY[48,11] := 0.005517168; GammaY[49,11] := 0.006716178;
  //GammaY[50,11] := 0.008143916; GammaY[51,11] := 0.009838246; GammaY[52,11] := 0.011842408; GammaY[53,11] := 0.014205665; GammaY[54,11] := 0.016984028; GammaY[55,11] := 0.020241043; GammaY[56,11] := 0.024048679; GammaY[57,11] := 0.028488301; GammaY[58,11] := 0.033651749; GammaY[59,11] := 0.03964254;
  //GammaY[60,11] := 0.046577203; GammaY[61,11] := 0.05458677; GammaY[62,11] := 0.06381844; GammaY[63,11] := 0.074437448; GammaY[64,11] := 0.086629165; GammaY[65,11] := 0.10060148; GammaY[66,11] := 0.116587491; GammaY[67,11] := 0.134848584; GammaY[68,11] := 0.155677961; GammaY[69,11] := 0.179404704;
  //GammaY[70,11] := 0.206398489; GammaY[71,11] := 0.237075082; GammaY[72,11] := 0.271902805; GammaY[73,11] := 0.311410171; GammaY[74,11] := 0.356194982; GammaY[75,11] := 0.406935256; GammaY[76,11] := 0.464402442; GammaY[77,11] := 0.529477537; GammaY[78,11] := 0.603170974; GammaY[79,11] := 0.686647308;
  //GammaY[80,11] := 0.781256215; GammaY[81,11] := 0.888571922; GammaY[82,11] := 1.01044386; GammaY[83,11] := 1.149062681; GammaY[84,11] := 1.307047543; GammaY[85,11] := 1.487567738; GammaY[86,11] := 1.694489793; GammaY[87,11] := 1.932611842; GammaY[88,11] := 2.207993217; GammaY[89,11] := 2.528411791;
  //GammaY[90,11] := 2.904089418; GammaY[91,11] := 3.348824285; GammaY[92,11] := 3.881864726; GammaY[93,11] := 4.53119815; GammaY[94,11] := 5.339747587; GammaY[95,11] := 6.378263457; GammaY[96,11] := 7.776105165; GammaY[97,11] := 9.812227301; GammaY[98,11] := 13.31615757; GammaY[99,11] := 22.39545786;
  //GammaY[0,12] := 2.85283E-18; GammaY[1,12] := 2.6125E-15; GammaY[2,12] := 9.6482E-14; GammaY[3,12] := 1.28749E-12; GammaY[4,12] := 9.77259E-12; GammaY[5,12] := 5.15628E-11; GammaY[6,12] := 2.10879E-10; GammaY[7,12] := 7.1443E-10; GammaY[8,12] := 2.09445E-09; GammaY[9,12] := 5.47608E-09;
  //GammaY[10,12] := 1.30494E-08; GammaY[11,12] := 2.88028E-08; GammaY[12,12] := 5.96131E-08; GammaY[13,12] := 1.16808E-07; GammaY[14,12] := 2.18335E-07; GammaY[15,12] := 3.91708E-07; GammaY[16,12] := 6.779E-07; GammaY[17,12] := 1.13641E-06; GammaY[18,12] := 1.85176E-06; GammaY[19,12] := 2.94164E-06;
  //GammaY[20,12] := 4.56707E-06; GammaY[21,12] := 6.94488E-06; GammaY[22,12] := 1.03629E-05; GammaY[23,12] := 1.51983E-05; GammaY[24,12] := 2.19394E-05; GammaY[25,12] := 3.12116E-05; GammaY[26,12] := 4.38078E-05; GammaY[27,12] := 6.07243E-05; GammaY[28,12] := 8.32018E-05; GammaY[29,12] := 0.000112774;
  //GammaY[30,12] := 0.000151322; GammaY[31,12] := 0.000201136; GammaY[32,12] := 0.000264992; GammaY[33,12] := 0.000346227; GammaY[34,12] := 0.000448833; GammaY[35,12] := 0.000577563; GammaY[36,12] := 0.000738044; GammaY[37,12] := 0.000936909; GammaY[38,12] := 0.001181938; GammaY[39,12] := 0.001482226;
  //GammaY[40,12] := 0.001848354; GammaY[41,12] := 0.002292591; GammaY[42,12] := 0.002829112; GammaY[43,12] := 0.003474239; GammaY[44,12] := 0.004246705; GammaY[45,12] := 0.005167943; GammaY[46,12] := 0.006262412; GammaY[47,12] := 0.007557943; GammaY[48,12] := 0.009086122; GammaY[49,12] := 0.010882714;
  //GammaY[50,12] := 0.012988122; GammaY[51,12] := 0.015447888; GammaY[52,12] := 0.01831325; GammaY[53,12] := 0.021641743; GammaY[54,12] := 0.02549786; GammaY[55,12] := 0.029953786; GammaY[56,12] := 0.03509019; GammaY[57,12] := 0.04099711; GammaY[58,12] := 0.047774928; GammaY[59,12] := 0.055535441;
  //GammaY[60,12] := 0.064403062; GammaY[61,12] := 0.074516148; GammaY[62,12] := 0.086028487; GammaY[63,12] := 0.099110973; GammaY[64,12] := 0.113953484; GammaY[65,12] := 0.130767016; GammaY[66,12] := 0.14978611; GammaY[67,12] := 0.171271624; GammaY[68,12] := 0.195513925; GammaY[69,12] := 0.222836583;
  //GammaY[70,12] := 0.253600656; GammaY[71,12] := 0.288209725; GammaY[72,12] := 0.327115807; GammaY[73,12] := 0.370826382; GammaY[74,12] := 0.41991277; GammaY[75,12] := 0.475020219; GammaY[76,12] := 0.536880143; GammaY[77,12] := 0.606325066; GammaY[78,12] := 0.684307028; GammaY[79,12] := 0.771920536;
  //GammaY[80,12] := 0.87043138; GammaY[81,12] := 0.981313243; GammaY[82,12] := 1.106299037; GammaY[83,12] := 1.247429637; GammaY[84,12] := 1.407143036; GammaY[85,12] := 1.588392893; GammaY[86,12] := 1.794784861; GammaY[87,12] := 2.030787803; GammaY[88,12] := 2.302033077; GammaY[89,12] := 2.615754119;
  //GammaY[90,12] := 2.981451498; GammaY[91,12] := 3.411945383; GammaY[92,12] := 3.925113196; GammaY[93,12] := 4.546934272; GammaY[94,12] := 5.317233775; GammaY[95,12] := 6.301615117; GammaY[96,12] := 7.619940688; GammaY[97,12] := 9.530457786; GammaY[98,12] := 12.80021455; GammaY[99,12] := 21.21480309;
  //GammaY[0,13] := 7.73995E-17; GammaY[1,13] := 4.26265E-14; GammaY[2,13] := 1.17845E-12; GammaY[3,13] := 1.28317E-11; GammaY[4,13] := 8.30643E-11; GammaY[5,13] := 3.84186E-10; GammaY[6,13] := 1.40403E-09; GammaY[7,13] := 4.31192E-09; GammaY[8,13] := 1.15874E-08; GammaY[9,13] := 2.80197E-08;
  //GammaY[10,13] := 6.22058E-08; GammaY[11,13] := 1.28694E-07; GammaY[12,13] := 2.50946E-07; GammaY[13,13] := 4.65297E-07; GammaY[14,13] := 8.26138E-07; GammaY[15,13] := 1.41254E-06; GammaY[16,13] := 2.33659E-06; GammaY[17,13] := 3.75366E-06; GammaY[18,13] := 5.87503E-06; GammaY[19,13] := 8.983E-06;
  //GammaY[20,13] := 1.3449E-05; GammaY[21,13] := 1.9755E-05; GammaY[22,13] := 2.85185E-05; GammaY[23,13] := 4.05219E-05; GammaY[24,13] := 5.67457E-05; GammaY[25,13] := 7.84077E-05; GammaY[26,13] := 0.000107007; GammaY[27,13] := 0.000144373; GammaY[28,13] := 0.000192723; GammaY[29,13] := 0.000254728;
  //GammaY[30,13] := 0.000333577; GammaY[31,13] := 0.000433064; GammaY[32,13] := 0.000557673; GammaY[33,13] := 0.000712674; GammaY[34,13] := 0.000904233; GammaY[35,13] := 0.001139532; GammaY[36,13] := 0.001426897; GammaY[37,13] := 0.001775944; GammaY[38,13] := 0.002197732; GammaY[39,13] := 0.002704936;
  //GammaY[40,13] := 0.003312035; GammaY[41,13] := 0.004035513; GammaY[42,13] := 0.004894078; GammaY[43,13] := 0.005908903; GammaY[44,13] := 0.007103884; GammaY[45,13] := 0.008505925; GammaY[46,13] := 0.010145238; GammaY[47,13] := 0.012055675; GammaY[48,13] := 0.014275089; GammaY[49,13] := 0.016845716;
  //GammaY[50,13] := 0.019814602; GammaY[51,13] := 0.023234059; GammaY[52,13] := 0.027162161; GammaY[53,13] := 0.031663286; GammaY[54,13] := 0.036808712; GammaY[55,13] := 0.04267726; GammaY[56,13] := 0.049356006; GammaY[57,13] := 0.056941064; GammaY[58,13] := 0.065538447; GammaY[59,13] := 0.075265023;
  //GammaY[60,13] := 0.086249576; GammaY[61,13] := 0.098633993; GammaY[62,13] := 0.112574589; GammaY[63,13] := 0.128243607; GammaY[64,13] := 0.145830908; GammaY[65,13] := 0.16554589; GammaY[66,13] := 0.187619689; GammaY[67,13] := 0.212307694; GammaY[68,13] := 0.239892457; GammaY[69,13] := 0.270687072;
  //GammaY[70,13] := 0.30503911; GammaY[71,13] := 0.343335241; GammaY[72,13] := 0.386006702; GammaY[73,13] := 0.433535769; GammaY[74,13] := 0.486463545; GammaY[75,13] := 0.545399297; GammaY[76,13] := 0.611031804; GammaY[77,13] := 0.684143248; GammaY[78,13] := 0.765626343; GammaY[79,13] := 0.856505657;
  //GammaY[80,13] := 0.957968586; GammaY[81,13] := 1.071386636; GammaY[82,13] := 1.198368369; GammaY[83,13] := 1.340824247; GammaY[84,13] := 1.501029071; GammaY[85,13] := 1.681728735; GammaY[86,13] := 1.886277843; GammaY[87,13] := 2.118833741; GammaY[88,13] := 2.384633865; GammaY[89,13] := 2.690401683;
  //GammaY[90,13] := 3.044966126; GammaY[91,13] := 3.460236397; GammaY[92,13] := 3.952814226; GammaY[93,13] := 4.546814675; GammaY[94,13] := 5.27918336; GammaY[95,13] := 6.210746682; GammaY[96,13] := 7.452560271; GammaY[97,13] := 9.243689501; GammaY[98,13] := 12.29339515; GammaY[99,13] := 20.09077933;
  //GammaY[0,14] := 1.58858E-15; GammaY[1,14] := 5.4882E-13; GammaY[2,14] := 1.16561E-11; GammaY[3,14] := 1.05374E-10; GammaY[4,14] := 5.89292E-10; GammaY[5,14] := 2.41435E-09; GammaY[6,14] := 7.9551E-09; GammaY[7,14] := 2.23198E-08; GammaY[8,14] := 5.5364E-08; GammaY[9,14] := 1.24596E-07;
  //GammaY[10,14] := 2.59177E-07; GammaY[11,14] := 5.05225E-07; GammaY[12,14] := 9.32652E-07; GammaY[13,14] := 1.64377E-06; GammaY[14,14] := 2.78391E-06; GammaY[15,14] := 4.55429E-06; GammaY[16,14] := 7.22746E-06; GammaY[17,14] := 1.11655E-05; GammaY[18,14] := 1.68413E-05; GammaY[19,14] := 2.48632E-05;
  //GammaY[20,14] := 3.60035E-05; GammaY[21,14] := 5.12307E-05; GammaY[22,14] := 7.17459E-05; GammaY[23,14] := 9.90242E-05; GammaY[24,14] := 0.000134861; GammaY[25,14] := 0.000181423; GammaY[26,14] := 0.000241306; GammaY[27,14] := 0.000317597; GammaY[28,14] := 0.000413943; GammaY[29,14] := 0.000534631;
  //GammaY[30,14] := 0.000684664; GammaY[31,14] := 0.000869859; GammaY[32,14] := 0.001096942; GammaY[33,14] := 0.001373653; GammaY[34,14] := 0.001708868; GammaY[35,14] := 0.002112716; GammaY[36,14] := 0.002596722; GammaY[37,14] := 0.003173945; GammaY[38,14] := 0.00385914; GammaY[39,14] := 0.004668924;
  //GammaY[40,14] := 0.005621955; GammaY[41,14] := 0.006739126; GammaY[42,14] := 0.008043776; GammaY[43,14] := 0.009561908; GammaY[44,14] := 0.01132243; GammaY[45,14] := 0.013357415; GammaY[46,14] := 0.015702371; GammaY[47,14] := 0.018396542; GammaY[48,14] := 0.021483224; GammaY[49,14] := 0.025010113;
  //GammaY[50,14] := 0.029029674; GammaY[51,14] := 0.033599546; GammaY[52,14] := 0.038782977; GammaY[53,14] := 0.044649303; GammaY[54,14] := 0.051274465; GammaY[55,14] := 0.05874158; GammaY[56,14] := 0.067141566; GammaY[57,14] := 0.076573829; GammaY[58,14] := 0.087147029; GammaY[59,14] := 0.098979928;
  //GammaY[60,14] := 0.112202333; GammaY[61,14] := 0.126956162; GammaY[62,14] := 0.143396636; GammaY[63,14] := 0.161693625; GammaY[64,14] := 0.182033185; GammaY[65,14] := 0.204619305; GammaY[66,14] := 0.229675912; GammaY[67,14] := 0.257449182; GammaY[68,14] := 0.288210208; GammaY[69,14] := 0.322258116;
  //GammaY[70,14] := 0.359923701; GammaY[71,14] := 0.401573698; GammaY[72,14] := 0.447615861; GammaY[73,14] := 0.498504977; GammaY[74,14] := 0.55475007; GammaY[75,14] := 0.616923108; GammaY[76,14] := 0.685669559; GammaY[77,14] := 0.761721313; GammaY[78,14] := 0.845912618; GammaY[79,14] := 0.939199937;
  //GammaY[80,14] := 1.042690408; GammaY[81,14] := 1.157662854; GammaY[82,14] := 1.285615672; GammaY[83,14] := 1.428327425; GammaY[84,14] := 1.587915656; GammaY[85,14] := 1.76693495; GammaY[86,14] := 1.968505705; GammaY[87,14] := 2.196494402; GammaY[88,14] := 2.455769956; GammaY[89,14] := 2.752581185;
  //GammaY[90,14] := 3.095132677; GammaY[91,14] := 3.494486759; GammaY[92,14] := 3.966057931; GammaY[93,14] := 4.532228648; GammaY[94,14] := 5.227272075; GammaY[95,14] := 6.10758163; GammaY[96,14] := 7.276048479; GammaY[97,14] := 8.953974415; GammaY[98,14] := 11.79718692; GammaY[99,14] := 19.0217858;
  //GammaY[0,15] := 2.52535E-14; GammaY[1,15] := 5.68861E-12; GammaY[2,15] := 9.50408E-11; GammaY[3,15] := 7.24573E-10; GammaY[4,15] := 3.54185E-09; GammaY[5,15] := 1.29776E-08; GammaY[6,15] := 3.88687E-08; GammaY[7,15] := 1.00348E-07; GammaY[8,15] := 2.31231E-07; GammaY[9,15] := 4.87111E-07;
  //GammaY[10,15] := 9.54389E-07; GammaY[11,15] := 1.76143E-06; GammaY[12,15] := 3.09205E-06; GammaY[13,15] := 5.20158E-06; GammaY[14,15] := 8.43559E-06; GammaY[15,15] := 1.32516E-05; GammaY[16,15] := 2.02439E-05; GammaY[17,15] := 3.01716E-05; GammaY[18,15] := 4.39902E-05; GammaY[19,15] := 6.28872E-05;
  //GammaY[20,15] := 8.83205E-05; GammaY[21,15] := 0.000122063; GammaY[22,15] := 0.000166247; GammaY[23,15] := 0.000223419; GammaY[24,15] := 0.000296597; GammaY[25,15] := 0.000389327; GammaY[26,15] := 0.000505753; GammaY[27,15] := 0.000650686; GammaY[28,15] := 0.000829683; GammaY[29,15] := 0.001049128;
  //GammaY[30,15] := 0.001316318; GammaY[31,15] := 0.00163956; GammaY[32,15] := 0.002028272; GammaY[33,15] := 0.002493084; GammaY[34,15] := 0.003045959; GammaY[35,15] := 0.00370031; GammaY[36,15] := 0.004471127; GammaY[37,15] := 0.005375116; GammaY[38,15] := 0.006430846; GammaY[39,15] := 0.007658895;
  //GammaY[40,15] := 0.009082022; GammaY[41,15] := 0.010725336; GammaY[42,15] := 0.012616479; GammaY[43,15] := 0.014785828; GammaY[44,15] := 0.017266702; GammaY[45,15] := 0.020095582; GammaY[46,15] := 0.023312359; GammaY[47,15] := 0.026960585; GammaY[48,15] := 0.031087752; GammaY[49,15] := 0.035745588;
  //GammaY[50,15] := 0.040990382; GammaY[51,15] := 0.046883331; GammaY[52,15] := 0.053490925; GammaY[53,15] := 0.060885354; GammaY[54,15] := 0.06914497; GammaY[55,15] := 0.078354786; GammaY[56,15] := 0.088607024; GammaY[57,15] := 0.100001733; GammaY[58,15] := 0.112647463; GammaY[59,15] := 0.126662032;
  //GammaY[60,15] := 0.142173373; GammaY[61,15] := 0.159320496; GammaY[62,15] := 0.178254572; GammaY[63,15] := 0.199140165; GammaY[64,15] := 0.222156635; GammaY[65,15] := 0.247499745; GammaY[66,15] := 0.275383508; GammaY[67,15] := 0.306042318; GammaY[68,15] := 0.339733429; GammaY[69,15] := 0.376739827;
  //GammaY[70,15] := 0.417373626; GammaY[71,15] := 0.461980038; GammaY[72,15] := 0.510942069; GammaY[73,15] := 0.564686142; GammaY[74,15] := 0.623688804; GammaY[75,15] := 0.688484827; GammaY[76,15] := 0.759677042; GammaY[77,15] := 0.837948373; GammaY[78,15] := 0.924076656; GammaY[79,15] := 1.018956386;
  //GammaY[80,15] := 1.123612106; GammaY[81,15] := 1.239234755; GammaY[82,15] := 1.367225587; GammaY[83,15] := 1.50923604; GammaY[84,15] := 1.667236112; GammaY[85,15] := 1.843601941; GammaY[86,15] := 2.041235788; GammaY[87,15] := 2.263732364; GammaY[88,15] := 2.515617467; GammaY[89,15] := 2.802699378;
  //GammaY[90,15] := 3.132600853; GammaY[91,15] := 3.51559694; GammaY[92,15] := 3.966000194; GammaY[93,15] := 4.504582631; GammaY[94,15] := 5.163137623; GammaY[95,15] := 5.9939463; GammaY[96,15] := 7.092332298; GammaY[97,15] := 8.663153597; GammaY[98,15] := 11.31283961; GammaY[99,15] := 18.00610257;
  //GammaY[0,16] := 3.17746E-13; GammaY[1,16] := 4.83504E-11; GammaY[2,16] := 6.49315E-10; GammaY[3,16] := 4.23378E-09; GammaY[4,16] := 1.82828E-08; GammaY[5,16] := 6.04373E-08; GammaY[6,16] := 1.6578E-07; GammaY[7,16] := 3.96436E-07; GammaY[8,16] := 8.53621E-07; GammaY[9,16] := 1.69223E-06;
  //GammaY[10,16] := 3.13806E-06; GammaY[11,16] := 5.5078E-06; GammaY[12,16] := 9.23178E-06; GammaY[13,16] := 1.48796E-05; GammaY[14,16] := 2.31887E-05; GammaY[15,16] := 3.50961E-05; GammaY[16,16] := 5.17729E-05; GammaY[17,16] := 7.46619E-05; GammaY[18,16] := 0.000105519; GammaY[19,16] := 0.000146458;
  //GammaY[20,16] := 0.000199995; GammaY[21,16] := 0.000269103; GammaY[22,16] := 0.000357267; GammaY[23,16] := 0.000468534; GammaY[24,16] := 0.000607586; GammaY[25,16] := 0.000779793; GammaY[26,16] := 0.00099129; GammaY[27,16] := 0.001249047; GammaY[28,16] := 0.001560942; GammaY[29,16] := 0.001935848;
  //GammaY[30,16] := 0.002383708; GammaY[31,16] := 0.002915633; GammaY[32,16] := 0.00354399; GammaY[33,16] := 0.004282498; GammaY[34,16] := 0.005146335; GammaY[35,16] := 0.006152244; GammaY[36,16] := 0.007318647; GammaY[37,16] := 0.008665761; GammaY[38,16] := 0.010215726; GammaY[39,16] := 0.011992741;
  //GammaY[40,16] := 0.014023195; GammaY[41,16] := 0.016335823; GammaY[42,16] := 0.018961861; GammaY[43,16] := 0.021935209; GammaY[44,16] := 0.025292616; GammaY[45,16] := 0.029073862; GammaY[46,16] := 0.033321971; GammaY[47,16] := 0.038083421; GammaY[48,16] := 0.043408389; GammaY[49,16] := 0.049351003;
  //GammaY[50,16] := 0.055969618; GammaY[51,16] := 0.063327128; GammaY[52,16] := 0.07149129; GammaY[53,16] := 0.080535092; GammaY[54,16] := 0.090537156; GammaY[55,16] := 0.101582183; GammaY[56,16] := 0.113761446; GammaY[57,16] := 0.12717334; GammaY[58,16] := 0.141923999; GammaY[59,16] := 0.158127986;
  //GammaY[60,16] := 0.175909071; GammaY[61,16] := 0.19540111; GammaY[62,16] := 0.216749041; GammaY[63,16] := 0.240110023; GammaY[64,16] := 0.26565473; GammaY[65,16] := 0.293568842; GammaY[66,16] := 0.324054755; GammaY[67,16] := 0.357333565; GammaY[68,16] := 0.393647375; GammaY[69,16] := 0.433261978;
  //GammaY[70,16] := 0.476469998; GammaY[71,16] := 0.5235946; GammaY[72,16] := 0.574993882; GammaY[73,16] := 0.631066099; GammaY[74,16] := 0.692255922; GammaY[75,16] := 0.759061988; GammaY[76,16] := 0.832046061; GammaY[77,16] := 0.911844212; GammaY[78,16] := 0.999183604; GammaY[79,16] := 1.094890766;
  //GammaY[80,16] := 1.199919485; GammaY[81,16] := 1.31538399; GammaY[82,16] := 1.442584655; GammaY[83,16] := 1.583057693; GammaY[84,16] := 1.738635773; GammaY[85,16] := 1.91152876; GammaY[86,16] := 2.104434775; GammaY[87,16] := 2.320695945; GammaY[88,16] := 2.564519976; GammaY[89,16] := 2.841304811;
  //GammaY[90,16] := 3.15813203; GammaY[91,16] := 3.524545471; GammaY[92,16] := 3.953833722; GammaY[93,16] := 4.465274833; GammaY[94,16] := 5.088359901; GammaY[95,16] := 5.87155678; GammaY[96,16] := 6.903178442; GammaY[97,16] := 8.372862807; GammaY[98,16] := 10.84138175; GammaY[99,16] := 17.04198486;
  //GammaY[0,17] := 3.22803E-12; GammaY[1,17] := 3.42729E-10; GammaY[2,17] := 3.7726E-09; GammaY[3,17] := 2.13067E-08; GammaY[4,17] := 8.20736E-08; GammaY[5,17] := 2.46746E-07; GammaY[6,17] := 6.2417E-07; GammaY[7,17] := 1.39097E-06; GammaY[8,17] := 2.81397E-06; GammaY[9,17] := 5.27532E-06;
  //GammaY[10,17] := 9.30014E-06; GammaY[11,17] := 1.55868E-05; GammaY[12,17] := 2.50394E-05; GammaY[13,17] := 3.8803E-05; GammaY[14,17] := 5.83005E-05; GammaY[15,17] := 8.52727E-05; GammaY[16,17] := 0.00012182; GammaY[17,17] := 0.000170445; GammaY[18,17] := 0.000234101; GammaY[19,17] := 0.00031624;
  //GammaY[20,17] := 0.000420859; GammaY[21,17] := 0.000552559; GammaY[22,17] := 0.000716593; GammaY[23,17] := 0.000918929; GammaY[24,17] := 0.001166304; GammaY[25,17] := 0.001466289; GammaY[26,17] := 0.001827347; GammaY[27,17] := 0.002258905; GammaY[28,17] := 0.002771418; GammaY[29,17] := 0.003376441;
  //GammaY[30,17] := 0.004086701; GammaY[31,17] := 0.004916175; GammaY[32,17] := 0.005880168; GammaY[33,17] := 0.006995393; GammaY[34,17] := 0.008280061; GammaY[35,17] := 0.009753968; GammaY[36,17] := 0.011438589; GammaY[37,17] := 0.013357175; GammaY[38,17] := 0.01553486; GammaY[39,17] := 0.017998765;
  //GammaY[40,17] := 0.02077812; GammaY[41,17] := 0.023904378; GammaY[42,17] := 0.027411354; GammaY[43,17] := 0.031335358; GammaY[44,17] := 0.035715348; GammaY[45,17] := 0.040593089; GammaY[46,17] := 0.046013325; GammaY[47,17] := 0.052023971; GammaY[48,17] := 0.058676314; GammaY[49,17] := 0.066025236;
  //GammaY[50,17] := 0.074129461; GammaY[51,17] := 0.083051819; GammaY[52,17] := 0.092859546; GammaY[53,17] := 0.103624604; GammaY[54,17] := 0.115424052; GammaY[55,17] := 0.128340441; GammaY[56,17] := 0.142462267; GammaY[57,17] := 0.157884477; GammaY[58,17] := 0.174709028; GammaY[59,17] := 0.193045531;
  //GammaY[60,17] := 0.213011962; GammaY[61,17] := 0.234735482; GammaY[62,17] := 0.25835336; GammaY[63,17] := 0.284014033; GammaY[64,17] := 0.311878306; GammaY[65,17] := 0.342120744; GammaY[66,17] := 0.374931274; GammaY[67,17] := 0.410517034; GammaY[68,17] := 0.449104511; GammaY[69,17] := 0.490942059;
  //GammaY[70,17] := 0.536302828; GammaY[71,17] := 0.585488231; GammaY[72,17] := 0.638832031; GammaY[73,17] := 0.696705222; GammaY[74,17] := 0.759521864; GammaY[75,17] := 0.827746092; GammaY[76,17] := 0.901900716; GammaY[77,17] := 0.982580226; GammaY[78,17] := 1.070455796; GammaY[79,17] := 1.166296448;
  //GammaY[80,17] := 1.270994773; GammaY[81,17] := 1.38558449; GammaY[82,17] := 1.511275447; GammaY[83,17] := 1.649496232; GammaY[84,17] := 1.80195138; GammaY[85,17] := 1.970696002; GammaY[86,17] := 2.158238706; GammaY[87,17] := 2.367687428; GammaY[88,17] := 2.602953515; GammaY[89,17] := 2.86905289;
  //GammaY[90,17] := 3.172565341; GammaY[91,17] := 3.52235549; GammaY[92,17] := 3.930760757; GammaY[93,17] := 4.415671803; GammaY[94,17] := 5.004444798; GammaY[95,17] := 5.742013282; GammaY[96,17] := 6.710195679; GammaY[97,17] := 8.084545472; GammaY[98,17] := 10.38364313; GammaY[99,17] := 16.12762459;
  //GammaY[0,18] := 2.6968E-11; GammaY[1,18] := 2.05775E-09; GammaY[2,18] := 1.88955E-08; GammaY[3,18] := 9.34967E-08; GammaY[4,18] := 3.24112E-07; GammaY[5,18] := 8.9276E-07; GammaY[6,18] := 2.09595E-06; GammaY[7,18] := 4.37727E-06; GammaY[8,18] := 8.36155E-06; GammaY[9,18] := 1.48904E-05;
  //GammaY[10,18] := 2.5059E-05; GammaY[11,18] := 4.02543E-05; GammaY[12,18] := 6.21934E-05; GammaY[13,18] := 9.29643E-05; GammaY[14,18] := 0.000135066; GammaY[15,18] := 0.000191452; GammaY[16,18] := 0.000265569; GammaY[17,18] := 0.000361406; GammaY[18,18] := 0.000483534; GammaY[19,18] := 0.000637154;
  //GammaY[20,18] := 0.000828139; GammaY[21,18] := 0.001063087; GammaY[22,18] := 0.001349362; GammaY[23,18] := 0.001695149; GammaY[24,18] := 0.002109496; GammaY[25,18] := 0.002602374; GammaY[26,18] := 0.003184718; GammaY[27,18] := 0.003868492; GammaY[28,18] := 0.004666731; GammaY[29,18] := 0.005593609;
  //GammaY[30,18] := 0.006664487; GammaY[31,18] := 0.007895979; GammaY[32,18] := 0.00930601; GammaY[33,18] := 0.010913885; GammaY[34,18] := 0.012740353; GammaY[35,18] := 0.014807676; GammaY[36,18] := 0.01713971; GammaY[37,18] := 0.019761976; GammaY[38,18] := 0.022701746; GammaY[39,18] := 0.025988134;
  //GammaY[40,18] := 0.029652185; GammaY[41,18] := 0.03372698; GammaY[42,18] := 0.038247745; GammaY[43,18] := 0.043251964; GammaY[44,18] := 0.048779511; GammaY[45,18] := 0.054872785; GammaY[46,18] := 0.061576864; GammaY[47,18] := 0.068939665; GammaY[48,18] := 0.077012129; GammaY[49,18] := 0.085848416;
  //GammaY[50,18] := 0.095506128; GammaY[51,18] := 0.106046548; GammaY[52,18] := 0.117534912; GammaY[53,18] := 0.130040704; GammaY[54,18] := 0.143637991; GammaY[55,18] := 0.158405797; GammaY[56,18] := 0.174428512; GammaY[57,18] := 0.191796366; GammaY[58,18] := 0.210605954; GammaY[59,18] := 0.230960822;
  //GammaY[60,18] := 0.252972146; GammaY[61,18] := 0.276759487; GammaY[62,18] := 0.302451658; GammaY[63,18] := 0.330187711; GammaY[64,18] := 0.360118071; GammaY[65,18] := 0.392405823; GammaY[66,18] := 0.427228212; GammaY[67,18] := 0.464778375; GammaY[68,18] := 0.505267344; GammaY[69,18] := 0.54892639;
  //GammaY[70,18] := 0.596009765; GammaY[71,18] := 0.646797933; GammaY[72,18] := 0.701601389; GammaY[73,18] := 0.760765185; GammaY[74,18] := 0.824674427; GammaY[75,18] := 0.893760802; GammaY[76,18] := 0.968512838; GammaY[77,18] := 1.049478503; GammaY[78,18] := 1.137282236; GammaY[79,18] := 1.232644438;
  //GammaY[80,18] := 1.336392531; GammaY[81,18] := 1.449488061; GammaY[82,18] := 1.573056359; GammaY[83,18] := 1.708426219; GammaY[84,18] := 1.857182728; GammaY[85,18] := 2.021237489; GammaY[86,18] := 2.202924199; GammaY[87,18] := 2.405131352; GammaY[88,18] := 2.631494585; GammaY[89,18] := 2.886676497;
  //GammaY[90,18] := 3.176790449; GammaY[91,18] := 3.510069355; GammaY[92,18] := 3.897968341; GammaY[93,18] := 4.357091365; GammaY[94,18] := 4.912813987; GammaY[95,18] := 5.60679387; GammaY[96,18] := 6.514835562; GammaY[97,18] := 7.799462745; GammaY[98,18] := 9.940272161; GammaY[99,18] := 15.26119062;
  //GammaY[0,19] := 1.88422E-10; GammaY[1,19] := 1.06144E-08; GammaY[2,19] := 8.26012E-08; GammaY[3,19] := 3.61796E-07; GammaY[4,19] := 1.13788E-06; GammaY[5,19] := 2.89122E-06; GammaY[6,19] := 6.33686E-06; GammaY[7,19] := 1.24666E-05; GammaY[8,19] := 2.25898E-05; GammaY[9,19] := 3.83728E-05;
  //GammaY[10,19] := 6.18782E-05; GammaY[11,19] := 9.56018E-05; GammaY[12,19] := 0.00014251; GammaY[13,19] := 0.000206079; GammaY[14,19] := 0.000290325; GammaY[15,19] := 0.000399845; GammaY[16,19] := 0.000539853; GammaY[17,19] := 0.000716209; GammaY[18,19] := 0.00093546; GammaY[19,19] := 0.001204873;
  //GammaY[20,19] := 0.001532466; GammaY[21,19] := 0.00192705; GammaY[22,19] := 0.002398259; GammaY[23,19] := 0.002956586; GammaY[24,19] := 0.003613419; GammaY[25,19] := 0.00438108; GammaY[26,19] := 0.005272859; GammaY[27,19] := 0.006303054; GammaY[28,19] := 0.007487008; GammaY[29,19] := 0.008841154;
  //GammaY[30,19] := 0.010383052; GammaY[31,19] := 0.012131437; GammaY[32,19] := 0.014106263; GammaY[33,19] := 0.016328753; GammaY[34,19] := 0.01882145; GammaY[35,19] := 0.021608273; GammaY[36,19] := 0.024714572; GammaY[37,19] := 0.028167197; GammaY[38,19] := 0.03199456; GammaY[39,19] := 0.036226711;
  //GammaY[40,19] := 0.040895416; GammaY[41,19] := 0.046034244; GammaY[42,19] := 0.051678659; GammaY[43,19] := 0.057866124; GammaY[44,19] := 0.064636213; GammaY[45,19] := 0.072030733; GammaY[46,19] := 0.08009386; GammaY[47,19] := 0.08887229; GammaY[48,19] := 0.0984154; GammaY[49,19] := 0.108775434;
  //GammaY[50,19] := 0.120007702; GammaY[51,19] := 0.132170807; GammaY[52,19] := 0.145326896; GammaY[53,19] := 0.159541932; GammaY[54,19] := 0.174886012; GammaY[55,19] := 0.19143371; GammaY[56,19] := 0.209264468; GammaY[57,19] := 0.228463035; GammaY[58,19] := 0.249119958; GammaY[59,19] := 0.271332134;
  //GammaY[60,19] := 0.295203452; GammaY[61,19] := 0.320845493; GammaY[62,19] := 0.348378346; GammaY[63,19] := 0.377931533; GammaY[64,19] := 0.409645066; GammaY[65,19] := 0.443670663; GammaY[66,19] := 0.480173145; GammaY[67,19] := 0.519332056; GammaY[68,19] := 0.561343536; GammaY[69,19] := 0.606422512;
  //GammaY[70,19] := 0.654805259; GammaY[71,19] := 0.706752392; GammaY[72,19] := 0.762552482; GammaY[73,19] := 0.822526269; GammaY[74,19] := 0.887031708; GammaY[75,19] := 0.956470113; GammaY[76,19] := 1.031295652; GammaY[77,19] := 1.112017892; GammaY[78,19] := 1.199217637; GammaY[79,19] := 1.293565184;
  //GammaY[80,19] := 1.39583018; GammaY[81,19] := 1.506906311; GammaY[82,19] := 1.627840948; GammaY[83,19] := 1.759870722; GammaY[84,19] := 1.90446952; GammaY[85,19] := 2.063415065; GammaY[86,19] := 2.238877684; GammaY[87,19] := 2.433543765; GammaY[88,19] := 2.650793179; GammaY[89,19] := 2.894959124;
  //GammaY[90,19] := 3.171721836; GammaY[91,19] := 3.48872839; GammaY[92,19] := 3.856614468; GammaY[93,19] := 4.290790362; GammaY[94,19] := 4.814795902; GammaY[95,19] := 5.467252912; GammaY[96,19] := 6.318398317; GammaY[97,19] := 7.518704064; GammaY[98,19] := 9.511753498; GammaY[99,19] := 14.44083625;
  //GammaY[0,20] := 1.11817E-09; GammaY[1,20] := 4.76549E-08; GammaY[2,20] := 3.18735E-07; GammaY[3,20] := 1.24741E-06; GammaY[4,20] := 3.58597E-06; GammaY[5,20] := 8.45793E-06; GammaY[6,20] := 1.74003E-05; GammaY[7,20] := 3.24005E-05; GammaY[8,20] := 5.59293E-05; GammaY[9,20] := 9.09713E-05;
  //GammaY[10,20] := 0.000141053; GammaY[11,20] := 0.000210267; GammaY[12,20] := 0.000303301; GammaY[13,20] := 0.000425458; GammaY[14,20] := 0.000582677; GammaY[15,20] := 0.000781562; GammaY[16,20] := 0.001029395; GammaY[17,20] := 0.001334161; GammaY[18,20] := 0.00170457; GammaY[19,20] := 0.002150072;
  //GammaY[20,20] := 0.002680882; GammaY[21,20] := 0.003308; GammaY[22,20] := 0.004043228; GammaY[23,20] := 0.004899196; GammaY[24,20] := 0.005889382; GammaY[25,20] := 0.007028132; GammaY[26,20] := 0.008330688; GammaY[27,20] := 0.009813212; GammaY[28,20] := 0.01149281; GammaY[29,20] := 0.013387563;
  //GammaY[30,20] := 0.015516555; GammaY[31,20] := 0.017899906; GammaY[32,20] := 0.020558809; GammaY[33,20] := 0.023515562; GammaY[34,20] := 0.026793616; GammaY[35,20] := 0.030417616; GammaY[36,20] := 0.034413447; GammaY[37,20] := 0.038808293; GammaY[38,20] := 0.043630691; GammaY[39,20] := 0.048910597;
  //GammaY[40,20] := 0.054679454; GammaY[41,20] := 0.060970273; GammaY[42,20] := 0.067817713; GammaY[43,20] := 0.075258179; GammaY[44,20] := 0.083329923; GammaY[45,20] := 0.092073158; GammaY[46,20] := 0.101530186; GammaY[47,20] := 0.111745536; GammaY[48,20] := 0.122766118; GammaY[49,20] := 0.134641397;
  //GammaY[50,20] := 0.147423579; GammaY[51,20] := 0.161167827; GammaY[52,20] := 0.175932496; GammaY[53,20] := 0.19177939; GammaY[54,20] := 0.208774063; GammaY[55,20] := 0.226986142; GammaY[56,20] := 0.246489695; GammaY[57,20] := 0.267363639; GammaY[58,20] := 0.289692216; GammaY[59,20] := 0.313565507;
  //GammaY[60,20] := 0.339080022; GammaY[61,20] := 0.366339373; GammaY[62,20] := 0.395455034; GammaY[63,20] := 0.42654721; GammaY[64,20] := 0.459745827; GammaY[65,20] := 0.495191668; GammaY[66,20] := 0.533037684; GammaY[67,20] := 0.573450506; GammaY[68,20] := 0.616612195; GammaY[69,20] := 0.662722273;
  //GammaY[70,20] := 0.712000153; GammaY[71,20] := 0.764687932; GammaY[72,20] := 0.821053686; GammaY[73,20] := 0.881395454; GammaY[74,20] := 0.94604596; GammaY[75,20] := 1.015380105; GammaY[76,20] := 1.089816342; GammaY[77,20] := 1.169828974; GammaY[78,20] := 1.255962103; GammaY[79,20] := 1.348836858;
  //GammaY[80,20] := 1.449169253; GammaY[81,20] := 1.557791255; GammaY[82,20] := 1.675677379; GammaY[83,20] := 1.803977622; GammaY[84,20] := 1.944063433; GammaY[85,20] := 2.097588763; GammaY[86,20] := 2.26657193; GammaY[87,20] := 2.453511078; GammaY[88,20] := 2.661548241; GammaY[89,20] := 2.894711315;
  //GammaY[90,20] := 3.158278696; GammaY[91,20] := 3.459353101; GammaY[92,20] := 3.807810721; GammaY[93,20] := 4.217954292; GammaY[94,20] := 4.711621439; GammaY[95,20] := 5.324622804; GammaY[96,20] := 6.122041178; GammaY[97,20] := 7.243202621; GammaY[98,20] := 9.09842599; GammaY[99,20] := 13.66469198;
  //GammaY[0,21] := 5.71684E-09; GammaY[1,21] := 1.88444E-07; GammaY[2,21] := 1.09691E-06; GammaY[3,21] := 3.86856E-06; GammaY[4,21] := 1.02351E-05; GammaY[5,21] := 2.2539E-05; GammaY[6,21] := 4.37423E-05; GammaY[7,21] := 7.74331E-05; GammaY[8,21] := 0.000127831; GammaY[9,21] := 0.000199795;
  //GammaY[10,21] := 0.000298822; GammaY[11,21] := 0.000431058; GammaY[12,21] := 0.0006033; GammaY[13,21] := 0.000822997; GammaY[14,21] := 0.00109826; GammaY[15,21] := 0.001437862; GammaY[16,21] := 0.001851246; GammaY[17,21] := 0.002348528; GammaY[18,21] := 0.002940506; GammaY[19,21] := 0.003638662;
  //GammaY[20,21] := 0.004455174; GammaY[21,21] := 0.005402922; GammaY[22,21] := 0.006495496; GammaY[23,21] := 0.007747209; GammaY[24,21] := 0.009173107; GammaY[25,21] := 0.010788983; GammaY[26,21] := 0.012611393; GammaY[27,21] := 0.014657669; GammaY[28,21] := 0.016945942; GammaY[29,21] := 0.019495159;
  //GammaY[30,21] := 0.022325111; GammaY[31,21] := 0.025456455; GammaY[32,21] := 0.028910744; GammaY[33,21] := 0.03271046; GammaY[34,21] := 0.036879053; GammaY[35,21] := 0.041440975; GammaY[36,21] := 0.046421727; GammaY[37,21] := 0.051847909; GammaY[38,21] := 0.057747273; GammaY[39,21] := 0.064148783;
  //GammaY[40,21] := 0.071082683; GammaY[41,21] := 0.078580566; GammaY[42,21] := 0.086675459; GammaY[43,21] := 0.09540191; GammaY[44,21] := 0.104796087; GammaY[45,21] := 0.114895889; GammaY[46,21] := 0.125741063; GammaY[47,21] := 0.137373338; GammaY[48,21] := 0.149836576; GammaY[49,21] := 0.163176932;
  //GammaY[50,21] := 0.177443035; GammaY[51,21] := 0.192686192; GammaY[52,21] := 0.208960607; GammaY[53,21] := 0.226323637; GammaY[54,21] := 0.24483606; GammaY[55,21] := 0.264562394; GammaY[56,21] := 0.285571242; GammaY[57,21] := 0.307935675; GammaY[58,21] := 0.331733674; GammaY[59,21] := 0.357048623;
  //GammaY[60,21] := 0.383969859; GammaY[61,21] := 0.412593306; GammaY[62,21] := 0.443022188; GammaY[63,21] := 0.47536784; GammaY[64,21] := 0.509750635; GammaY[65,21] := 0.546301051; GammaY[66,21] := 0.585160885; GammaY[67,21] := 0.626484685; GammaY[68,21] := 0.670441381; GammaY[69,21] := 0.717216172;
  //GammaY[70,21] := 0.767012779; GammaY[71,21] := 0.820056058; GammaY[72,21] := 0.8765951; GammaY[73,21] := 0.936906918; GammaY[74,21] := 1.001302402; GammaY[75,21] := 1.070126921; GammaY[76,21] := 1.143769947; GammaY[77,21] := 1.22267574; GammaY[78,21] := 1.307348719; GammaY[79,21] := 1.398367315;
  //GammaY[80,21] := 1.496397843; GammaY[81,21] := 1.602214305; GammaY[82,21] := 1.716723332; GammaY[83,21] := 1.840995302; GammaY[84,21] := 1.976306544; GammaY[85,21] := 2.124195299; GammaY[86,21] := 2.286539267; GammaY[87,21] := 2.465661786; GammaY[88,21] := 2.66448459; GammaY[89,21] := 2.886751848;
  //GammaY[90,21] := 3.137367845; GammaY[91,21] := 3.422931254; GammaY[92,21] := 3.752613438; GammaY[93,21] := 4.139690114; GammaY[94,21] := 4.604421484; GammaY[95,21] := 5.18001452; GammaY[96,21] := 5.92678203; GammaY[97,21] := 6.973744218; GammaY[98,21] := 8.700497871; GammaY[99,21] := 12.93091605;
  //GammaY[0,22] := 2.55113E-08; GammaY[1,22] := 6.63475E-07; GammaY[2,22] := 3.39873E-06; GammaY[3,22] := 1.0886E-05; GammaY[4,22] := 2.66748E-05; GammaY[5,22] := 5.51387E-05; GammaY[6,22] := 0.000101416; GammaY[7,22] := 0.000171364; GammaY[8,22] := 0.000271532; GammaY[9,22] := 0.000409126;
  //GammaY[10,22] := 0.000591993; GammaY[11,22] := 0.000828599; GammaY[12,22] := 0.001128016; GammaY[13,22] := 0.001499909; GammaY[14,22] := 0.001954523; GammaY[15,22] := 0.002502678; GammaY[16,22] := 0.003155758; GammaY[17,22] := 0.003925712; GammaY[18,22] := 0.004825046; GammaY[19,22] := 0.005866823;
  //GammaY[20,22] := 0.007064664; GammaY[21,22] := 0.008432752; GammaY[22,22] := 0.009985831; GammaY[23,22] := 0.011739217; GammaY[24,22] := 0.013708802; GammaY[25,22] := 0.015911069; GammaY[26,22] := 0.018363098; GammaY[27,22] := 0.021082584; GammaY[28,22] := 0.024087854; GammaY[29,22] := 0.027397887;
  //GammaY[30,22] := 0.031032332; GammaY[31,22] := 0.035011538; GammaY[32,22] := 0.039356582; GammaY[33,22] := 0.044089299; GammaY[34,22] := 0.049232316; GammaY[35,22] := 0.054809096; GammaY[36,22] := 0.060843978; GammaY[37,22] := 0.067362231; GammaY[38,22] := 0.074390097; GammaY[39,22] := 0.081954863;
  //GammaY[40,22] := 0.090084917; GammaY[41,22] := 0.098809823; GammaY[42,22] := 0.1081604; GammaY[43,22] := 0.11816881; GammaY[44,22] := 0.128868652; GammaY[45,22] := 0.140295069; GammaY[46,22] := 0.152484862; GammaY[47,22] := 0.165476623; GammaY[48,22] := 0.179310873; GammaY[49,22] := 0.194030217;
  //GammaY[50,22] := 0.209679523; GammaY[51,22] := 0.226306104; GammaY[52,22] := 0.243959943; GammaY[53,22] := 0.26269392; GammaY[54,22] := 0.282564077; GammaY[55,22] := 0.30362991; GammaY[56,22] := 0.325954697; GammaY[57,22] := 0.349605868; GammaY[58,22] := 0.374655407; GammaY[59,22] := 0.401180322;
  //GammaY[60,22] := 0.429263165; GammaY[61,22] := 0.458992618; GammaY[62,22] := 0.490464164; GammaY[63,22] := 0.523780848; GammaY[64,22] := 0.559054132; GammaY[65,22] := 0.596404918; GammaY[66,22] := 0.635964676; GammaY[67,22] := 0.677876747; GammaY[68,22] := 0.722297889; GammaY[69,22] := 0.769400051;
  //GammaY[70,22] := 0.819372455; GammaY[71,22] := 0.872424037; GammaY[72,22] := 0.928786335; GammaY[73,22] := 0.988718279; GammaY[74,22] := 1.052506261; GammaY[75,22] := 1.120471502; GammaY[76,22] := 1.192979153; GammaY[77,22] := 1.270441584; GammaY[78,22] := 1.353328276; GammaY[79,22] := 1.442177201;
  //GammaY[80,22] := 1.537608819; GammaY[81,22] := 1.640343818; GammaY[82,22] := 1.751225879; GammaY[83,22] := 1.871250995; GammaY[84,22] := 2.001607181; GammaY[85,22] := 2.143726018; GammaY[86,22] := 2.299354145; GammaY[87,22] := 2.470652557; GammaY[88,22] := 2.660338227; GammaY[89,22] := 2.871891663;
  //GammaY[90,22] := 3.109870989; GammaY[91,22] := 3.380407168; GammaY[92,22] := 3.692016839; GammaY[93,22] := 4.057023917; GammaY[94,22] := 4.494227706; GammaY[95,22] := 5.034423551; GammaY[96,22] := 5.73351068; GammaY[97,22] := 6.710981197; GammaY[98,22] := 8.318060809; GammaY[99,22] := 12.23766816;
  //GammaY[0,23] := 1.00559E-07; GammaY[1,23] := 2.10049E-06; GammaY[2,23] := 9.56339E-06; GammaY[3,23] := 2.8018E-05; GammaY[4,23] := 6.39594E-05; GammaY[5,23] := 0.000124715; GammaY[6,23] := 0.000218325; GammaY[7,23] := 0.000353457; GammaY[8,23] := 0.00053935; GammaY[9,23] := 0.00078577;
  //GammaY[10,23] := 0.001102972; GammaY[11,23] := 0.001501677; GammaY[12,23] := 0.001993043; GammaY[13,23] := 0.002588655; GammaY[14,23] := 0.003300505; GammaY[15,23] := 0.004140986; GammaY[16,23] := 0.005122881; GammaY[17,23] := 0.006259359; GammaY[18,23] := 0.007563972; GammaY[19,23] := 0.009050656;
  //GammaY[20,23] := 0.010733729; GammaY[21,23] := 0.012627902; GammaY[22,23] := 0.014748278; GammaY[23,23] := 0.017110363; GammaY[24,23] := 0.019730079; GammaY[25,23] := 0.022623771; GammaY[26,23] := 0.025808228; GammaY[27,23] := 0.029300698; GammaY[28,23] := 0.033118905; GammaY[29,23] := 0.037281076;
  //GammaY[30,23] := 0.041805962; GammaY[31,23] := 0.046712869; GammaY[32,23] := 0.052021687; GammaY[33,23] := 0.057752927; GammaY[34,23] := 0.063927753; GammaY[35,23] := 0.070568029; GammaY[36,23] := 0.077696363; GammaY[37,23] := 0.085336154; GammaY[38,23] := 0.093511651; GammaY[39,23] := 0.102248007;
  //GammaY[40,23] := 0.111571349; GammaY[41,23] := 0.121508847; GammaY[42,23] := 0.132088794; GammaY[43,23] := 0.143340685; GammaY[44,23] := 0.155295321; GammaY[45,23] := 0.1679849; GammaY[46,23] := 0.181443138; GammaY[47,23] := 0.195705389; GammaY[48,23] := 0.210808776; GammaY[49,23] := 0.226792351;
  //GammaY[50,23] := 0.243697253; GammaY[51,23] := 0.261566888; GammaY[52,23] := 0.280447134; GammaY[53,23] := 0.300386564; GammaY[54,23] := 0.321436691; GammaY[55,23] := 0.343652247; GammaY[56,23] := 0.367091489; GammaY[57,23] := 0.391816545; GammaY[58,23] := 0.417893796; GammaY[59,23] := 0.445394311;
  //GammaY[60,23] := 0.474394337; GammaY[61,23] := 0.50497584; GammaY[62,23] := 0.537227151; GammaY[63,23] := 0.571243666; GammaY[64,23] := 0.607128644; GammaY[65,23] := 0.644994152; GammaY[66,23] := 0.684962126; GammaY[67,23] := 0.727165598; GammaY[68,23] := 0.771750127; GammaY[69,23] := 0.818875454;
  //GammaY[70,23] := 0.86871744; GammaY[71,23] := 0.921470343; GammaY[72,23] := 0.977350705; GammaY[73,23] := 1.036597062; GammaY[74,23] := 1.099475759; GammaY[75,23] := 1.16628832; GammaY[76,23] := 1.237373415; GammaY[77,23] := 1.313114969; GammaY[78,23] := 1.393950326; GammaY[79,23] := 1.480380199;
  //GammaY[80,23] := 1.572982085; GammaY[81,23] := 1.672427218; GammaY[82,23] := 1.779501079; GammaY[83,23] := 1.895131094; GammaY[84,23] := 2.020422848; GammaY[85,23] := 2.156708422; GammaY[86,23] := 2.305613458; GammaY[87,23] := 2.469148009; GammaY[88,23] := 2.649838704; GammaY[89,23] := 2.850921836;
  //GammaY[90,23] := 3.076634264; GammaY[91,23] := 3.332672336; GammaY[92,23] := 3.626946331; GammaY[93,23] := 3.970897941; GammaY[94,23] := 4.381972605; GammaY[95,23] := 4.888732914; GammaY[96,23] := 5.542995001; GammaY[97,23] := 6.455443652; GammaY[98,23] := 7.951106078; GammaY[99,23] := 11.58313153;
  //GammaY[0,24] := 3.53949E-07; GammaY[1,24] := 6.03352E-06; GammaY[2,24] := 2.4632E-05; GammaY[3,24] := 6.64436E-05; GammaY[4,24] := 0.000142072; GammaY[5,24] := 0.000262526; GammaY[6,24] := 0.000439142; GammaY[7,24] := 0.000683527; GammaY[8,24] := 0.001007527; GammaY[9,24] := 0.001423207;
  //GammaY[10,24] := 0.001942831; GammaY[11,24] := 0.002578851; GammaY[12,24] := 0.003343906; GammaY[13,24] := 0.00425081; GammaY[14,24] := 0.005312557; GammaY[15,24] := 0.006542319; GammaY[16,24] := 0.007953452; GammaY[17,24] := 0.009559499; GammaY[18,24] := 0.011374197; GammaY[19,24] := 0.013411488;
  //GammaY[20,24] := 0.015685529; GammaY[21,24] := 0.018210701; GammaY[22,24] := 0.021001628; GammaY[23,24] := 0.024073189; GammaY[24,24] := 0.027440535; GammaY[25,24] := 0.03111911; GammaY[26,24] := 0.035124673; GammaY[27,24] := 0.039473318; GammaY[28,24] := 0.044181498; GammaY[29,24] := 0.049266056;
  //GammaY[30,24] := 0.054744254; GammaY[31,24] := 0.060633799; GammaY[32,24] := 0.066952885; GammaY[33,24] := 0.073720226; GammaY[34,24] := 0.080955094; GammaY[35,24] := 0.088677369; GammaY[36,24] := 0.096907579; GammaY[37,24] := 0.105666956; GammaY[38,24] := 0.114977487; GammaY[39,24] := 0.124861977;
  //GammaY[40,24] := 0.13534411; GammaY[41,24] := 0.146448522; GammaY[42,24] := 0.158200876; GammaY[43,24] := 0.170627945; GammaY[44,24] := 0.183757701; GammaY[45,24] := 0.197619413; GammaY[46,24] := 0.212243762; GammaY[47,24] := 0.227662945; GammaY[48,24] := 0.243910817; GammaY[49,24] := 0.261023025;
  //GammaY[50,24] := 0.279037165; GammaY[51,24] := 0.297992958; GammaY[52,24] := 0.317932433; GammaY[53,24] := 0.338900143; GammaY[54,24] := 0.360943393; GammaY[55,24] := 0.384112504; GammaY[56,24] := 0.408461095; GammaY[57,24] := 0.434046413; GammaY[58,24] := 0.460929678; GammaY[59,24] := 0.489176515;
  //GammaY[60,24] := 0.518857392; GammaY[61,24] := 0.55004813; GammaY[62,24] := 0.582830501; GammaY[63,24] := 0.617292882; GammaY[64,24] := 0.653531017; GammaY[65,24] := 0.691648879; GammaY[66,24] := 0.731759668; GammaY[67,24] := 0.773986953; GammaY[68,24] := 0.818466007; GammaY[69,24] := 0.865345346;
  //GammaY[70,24] := 0.91478851; GammaY[71,24] := 0.966976254; GammaY[72,24] := 1.0221101; GammaY[73,24] := 1.080412006; GammaY[74,24] := 1.142130192; GammaY[75,24] := 1.207545324; GammaY[76,24] := 1.27697301; GammaY[77,24] := 1.350770857; GammaY[78,24] := 1.429345617; GammaY[79,24] := 1.513164065;
  //GammaY[80,24] := 1.602764285; GammaY[81,24] := 1.698771015; GammaY[82,24] := 1.80191616; GammaY[83,24] := 1.913063043; GammaY[84,24] := 2.033240747; GammaY[85,24] := 2.163689613; GammaY[86,24] := 2.305922079; GammaY[87,24] := 2.461808517; GammaY[88,24] := 2.633699288; GammaY[89,24] := 2.824603573;
  //GammaY[90,24] := 3.038460138; GammaY[91,24] := 3.280562072; GammaY[92,24] := 3.55825745; GammaY[93,24] := 3.882170766; GammaY[94,24] := 4.268493246; GammaY[95,24] := 4.743720805; GammaY[96,24] := 5.355892187; GammaY[97,24] := 6.207550976; GammaY[98,24] := 7.599535362; GammaY[99,24] := 10.96552532;
  //GammaY[0,25] := 1.12356E-06; GammaY[1,25] := 1.58535E-05; GammaY[2,25] := 5.84972E-05; GammaY[3,25] := 0.00014617; GammaY[4,25] := 0.000294225; GammaY[5,25] := 0.000517411; GammaY[6,25] := 0.000830044; GammaY[7,25] := 0.001246107; GammaY[8,25] := 0.001779323; GammaY[9,25] := 0.0024432;
  //GammaY[10,25] := 0.003251082; GammaY[11,25] := 0.004216174; GammaY[12,25] := 0.00535158; GammaY[13,25] := 0.006670326; GammaY[14,25] := 0.008185387; GammaY[15,25] := 0.009909708; GammaY[16,25] := 0.011856233; GammaY[17,25] := 0.014037919; GammaY[18,25] := 0.016467767; GammaY[19,25] := 0.019158837;
  //GammaY[20,25] := 0.022124275; GammaY[21,25] := 0.025377335; GammaY[22,25] := 0.028931401; GammaY[23,25] := 0.032800013; GammaY[24,25] := 0.03699689; GammaY[25,25] := 0.041535958; GammaY[26,25] := 0.046431375; GammaY[27,25] := 0.051697561; GammaY[28,25] := 0.057349226; GammaY[29,25] := 0.063401401;
  //GammaY[30,25] := 0.069869473; GammaY[31,25] := 0.076769216; GammaY[32,25] := 0.084116832; GammaY[33,25] := 0.091928986; GammaY[34,25] := 0.100222848; GammaY[35,25] := 0.109016142; GammaY[36,25] := 0.118327184; GammaY[37,25] := 0.128174941; GammaY[38,25] := 0.138579082; GammaY[39,25] := 0.149560034;
  //GammaY[40,25] := 0.161139048; GammaY[41,25] := 0.173338264; GammaY[42,25] := 0.186180787; GammaY[43,25] := 0.199690766; GammaY[44,25] := 0.213893473; GammaY[45,25] := 0.228815407; GammaY[46,25] := 0.244484389; GammaY[47,25] := 0.260929674; GammaY[48,25] := 0.278182073; GammaY[49,25] := 0.296274089;
  //GammaY[50,25] := 0.31524006; GammaY[51,25] := 0.335116319; GammaY[52,25] := 0.355941377; GammaY[53,25] := 0.377756115; GammaY[54,25] := 0.400604005; GammaY[55,25] := 0.424531348; GammaY[56,25] := 0.449587541; GammaY[57,25] := 0.475825393; GammaY[58,25] := 0.503301457; GammaY[59,25] := 0.532076389;
  //GammaY[60,25] := 0.562215394; GammaY[61,25] := 0.5937887; GammaY[62,25] := 0.626872105; GammaY[63,25] := 0.661547592; GammaY[64,25] := 0.697904039; GammaY[65,25] := 0.736038021; GammaY[66,25] := 0.776054741; GammaY[67,25] := 0.81806908; GammaY[68,25] := 0.862206879; GammaY[69,25] := 0.908606359;
  //GammaY[70,25] := 0.957419767; GammaY[71,25] := 1.008816431; GammaY[72,25] := 1.06298181; GammaY[73,25] := 1.12012285; GammaY[74,25] := 1.180472806; GammaY[75,25] := 1.244292333; GammaY[76,25] := 1.311875323; GammaY[77,25] := 1.383554811; GammaY[78,25] := 1.459710165; GammaY[79,25] := 1.54077592;
  //GammaY[80,25] := 1.627253607; GammaY[81,25] := 1.719725419; GammaY[82,25] := 1.818872895; GammaY[83,25] := 1.925500093; GammaY[84,25] := 2.040565272; GammaY[85,25] := 2.165223166; GammaY[86,25] := 2.300881528; GammaY[87,25] := 2.449280845; GammaY[88,25] := 2.612607674; GammaY[89,25] := 2.793660655;
  //GammaY[90,25] := 2.996101636; GammaY[91,25] := 3.224850122; GammaY[92,25] := 3.486733362; GammaY[93,25] := 3.791618226; GammaY[94,25] := 4.154534407; GammaY[95,25] := 4.60006493; GammaY[96,25] := 5.17275456; GammaY[97,25] := 5.967622613; GammaY[98,25] := 7.263173624; GammaY[99,25] := 10.38309513;
  //GammaY[0,26] := 3.24548E-06; GammaY[1,26] := 3.83902E-05; GammaY[2,26] := 0.000128949; GammaY[3,26] := 0.000300164; GammaY[4,26] := 0.000571441; GammaY[5,26] := 0.000960117; GammaY[6,26] := 0.001482139; GammaY[7,26] := 0.002152432; GammaY[8,26] := 0.002985139; GammaY[9,26] := 0.003993781;
  //GammaY[10,26] := 0.005191384; GammaY[11,26] := 0.006590569; GammaY[12,26] := 0.008203631; GammaY[13,26] := 0.010042602; GammaY[14,26] := 0.012119304; GammaY[15,26] := 0.014445397; GammaY[16,26] := 0.017032425; GammaY[17,26] := 0.019891849; GammaY[18,26] := 0.02303509; GammaY[19,26] := 0.026473555;
  //GammaY[20,26] := 0.030218677; GammaY[21,26] := 0.034281943; GammaY[22,26] := 0.038674921; GammaY[23,26] := 0.043409298; GammaY[24,26] := 0.048496902; GammaY[25,26] := 0.053949738; GammaY[26,26] := 0.059780014; GammaY[27,26] := 0.066000176; GammaY[28,26] := 0.072622935; GammaY[29,26] := 0.079661303;
  //GammaY[30,26] := 0.087128628; GammaY[31,26] := 0.095038622; GammaY[32,26] := 0.103405408; GammaY[33,26] := 0.112243549; GammaY[34,26] := 0.121568095; GammaY[35,26] := 0.131394623; GammaY[36,26] := 0.141739283; GammaY[37,26] := 0.152618847; GammaY[38,26] := 0.164050758; GammaY[39,26] := 0.17605319;
  //GammaY[40,26] := 0.188645107; GammaY[41,26] := 0.201846319; GammaY[42,26] := 0.21567756; GammaY[43,26] := 0.23016056; GammaY[44,26] := 0.245318122; GammaY[45,26] := 0.261174213; GammaY[46,26] := 0.27775406; GammaY[47,26] := 0.295084251; GammaY[48,26] := 0.313192849; GammaY[49,26] := 0.332109522;
  //GammaY[50,26] := 0.351865669; GammaY[51,26] := 0.372494582; GammaY[52,26] := 0.394031605; GammaY[53,26] := 0.416514311; GammaY[54,26] := 0.439982725; GammaY[55,26] := 0.46447954; GammaY[56,26] := 0.490050356; GammaY[57,26] := 0.516743974; GammaY[58,26] := 0.54461271; GammaY[59,26] := 0.573712742;
  //GammaY[60,26] := 0.604104512; GammaY[61,26] := 0.635853171; GammaY[62,26] := 0.669029088; GammaY[63,26] := 0.703708427; GammaY[64,26] := 0.739973804; GammaY[65,26] := 0.777915016; GammaY[66,26] := 0.817629964; GammaY[67,26] := 0.859225615; GammaY[68,26] := 0.902819122; GammaY[69,26] := 0.948539212;
  //GammaY[70,26] := 0.99652858; GammaY[71,26] := 1.046943264; GammaY[72,26] := 1.099956204; GammaY[73,26] := 1.155761867; GammaY[74,26] := 1.214577086; GammaY[75,26] := 1.276644652; GammaY[76,26] := 1.342238358; GammaY[77,26] := 1.411668823; GammaY[78,26] := 1.48528968; GammaY[79,26] := 1.563506115;
  //GammaY[80,26] := 1.646785348; GammaY[81,26] := 1.735669894; GammaY[82,26] := 1.830794574; GammaY[83,26] := 1.932908589; GammaY[84,26] := 2.042904613; GammaY[85,26] := 2.161857722; GammaY[86,26] := 2.291078478; GammaY[87,26] := 2.432186796; GammaY[88,26] := 2.587217204; GammaY[89,26] := 2.758772988;
  //GammaY[90,26] := 2.950259307; GammaY[91,26] := 3.166249235; GammaY[92,26] := 3.4130869; GammaY[93,26] := 3.699937252; GammaY[94,26] := 4.040754549; GammaY[95,26] := 4.458351429; GammaY[96,26] := 4.99404109; GammaY[97,26] := 5.735889043; GammaY[98,26] := 6.941780197; GammaY[99,26] := 9.834132943;
  //GammaY[0,27] := 8.60029E-06; GammaY[1,27] := 8.62584E-05; GammaY[2,27] := 0.000265471; GammaY[3,27] := 0.000578699; GammaY[4,27] := 0.001046493; GammaY[5,27] := 0.001686038; GammaY[6,27] := 0.002512399; GammaY[7,27] := 0.003539171; GammaY[8,27] := 0.004778866; GammaY[9,27] := 0.006243174;
  //GammaY[10,27] := 0.00794315; GammaY[11,27] := 0.009889346; GammaY[12,27] := 0.012091922; GammaY[13,27] := 0.014560732; GammaY[14,27] := 0.017305393; GammaY[15,27] := 0.020335347; GammaY[16,27] := 0.023659915; GammaY[17,27] := 0.027288338; GammaY[18,27] := 0.031229823; GammaY[19,27] := 0.035493582;
  //GammaY[20,27] := 0.040088861; GammaY[21,27] := 0.04502498; GammaY[22,27] := 0.050311361; GammaY[23,27] := 0.05595756; GammaY[24,27] := 0.061973296; GammaY[25,27] := 0.068368483; GammaY[26,27] := 0.075153257; GammaY[27,27] := 0.082338006; GammaY[28,27] := 0.089933401; GammaY[29,27] := 0.097950428;
  //GammaY[30,27] := 0.106400419; GammaY[31,27] := 0.115295083; GammaY[32,27] := 0.124646541; GammaY[33,27] := 0.134467365; GammaY[34,27] := 0.144770612; GammaY[35,27] := 0.155569864; GammaY[36,27] := 0.166879273; GammaY[37,27] := 0.178713605; GammaY[38,27] := 0.191088282; GammaY[39,27] := 0.204019442;
  //GammaY[40,27] := 0.217523989; GammaY[41,27] := 0.231619653; GammaY[42,27] := 0.246325051; GammaY[43,27] := 0.261659762; GammaY[44,27] := 0.277644397; GammaY[45,27] := 0.294300681; GammaY[46,27] := 0.311651541; GammaY[47,27] := 0.329721206; GammaY[48,27] := 0.348535307; GammaY[49,27] := 0.368120994;
  //GammaY[50,27] := 0.388507068; GammaY[51,27] := 0.40972412; GammaY[52,27] := 0.431804677; GammaY[53,27] := 0.454783379; GammaY[54,27] := 0.478697169; GammaY[55,27] := 0.503585503; GammaY[56,27] := 0.529490579; GammaY[57,27] := 0.556457605; GammaY[58,27] := 0.584535088; GammaY[59,27] := 0.613775165;
  //GammaY[60,27] := 0.644233967; GammaY[61,27] := 0.675972046; GammaY[62,27] := 0.709054825; GammaY[63,27] := 0.743553178; GammaY[64,27] := 0.779544021; GammaY[65,27] := 0.817110986; GammaY[66,27] := 0.856345261; GammaY[67,27] := 0.897346507; GammaY[68,27] := 0.940223932; GammaY[69,27] := 0.985098394;
  //GammaY[70,27] := 1.03210122; GammaY[71,27] := 1.081377801; GammaY[72,27] := 1.133091311; GammaY[73,27] := 1.187422374; GammaY[74,27] := 1.244572882; GammaY[75,27] := 1.304769614; GammaY[76,27] := 1.368267903; GammaY[77,27] := 1.435357075; GammaY[78,27] := 1.506367053; GammaY[79,27] := 1.581675447;
  //GammaY[80,27] := 1.661717985; GammaY[81,27] := 1.747000517; GammaY[82,27] := 1.838114492; GammaY[83,27] := 1.935757996; GammaY[84,27] := 2.040762626; GammaY[85,27] := 2.154129678; GammaY[86,27] := 2.277079313; GammaY[87,27] := 2.411119679; GammaY[88,27] := 2.558144384; GammaY[89,27] := 2.720575307;
  //GammaY[90,27] := 2.901579365; GammaY[91,27] := 3.105408739; GammaY[92,27] := 3.337959795; GammaY[93,27] := 3.60774691; GammaY[94,27] := 3.927728568; GammaY[95,27] := 4.319079165; GammaY[96,27] := 4.820123351; GammaY[97,27] := 5.512500216; GammaY[98,27] := 6.635057991; GammaY[99,27] := 9.316972354;
  //GammaY[0,28] := 2.10606E-05; GammaY[1,28] := 0.000180951; GammaY[2,28] := 0.000513322; GammaY[3,28] := 0.001053033; GammaY[4,28] := 0.001816126; GammaY[5,28] := 0.002815283; GammaY[6,28] := 0.00406123; GammaY[7,28] := 0.005563411; GammaY[8,28] := 0.007330387; GammaY[9,28] := 0.009370082;
  //GammaY[10,28] := 0.011689962; GammaY[11,28] := 0.014297157; GammaY[12,28] := 0.017198558; GammaY[13,28] := 0.020400893; GammaY[14,28] := 0.023910785; GammaY[15,28] := 0.027734807; GammaY[16,28] := 0.031879521; GammaY[17,28] := 0.03635152; GammaY[18,28] := 0.04115746; GammaY[19,28] := 0.046304089;
  //GammaY[20,28] := 0.051798278; GammaY[21,28] := 0.057647049; GammaY[22,28] := 0.063857597; GammaY[23,28] := 0.070437318; GammaY[24,28] := 0.07739383; GammaY[25,28] := 0.084735004; GammaY[26,28] := 0.092468979; GammaY[27,28] := 0.100604196; GammaY[28,28] := 0.109149418; GammaY[29,28] := 0.118113758;
  //GammaY[30,28] := 0.127506704; GammaY[31,28] := 0.137338152; GammaY[32,28] := 0.147618429; GammaY[33,28] := 0.158358333; GammaY[34,28] := 0.169569159; GammaY[35,28] := 0.181262734; GammaY[36,28] := 0.193451461; GammaY[37,28] := 0.206148354; GammaY[38,28] := 0.219367082; GammaY[39,28] := 0.233122014;
  //GammaY[40,28] := 0.247428272; GammaY[41,28] := 0.262301784; GammaY[42,28] := 0.277759339; GammaY[43,28] := 0.293818653; GammaY[44,28] := 0.310498437; GammaY[45,28] := 0.327818472; GammaY[46,28] := 0.345799686; GammaY[47,28] := 0.364464253; GammaY[48,28] := 0.383835686; GammaY[49,28] := 0.403938936;
  //GammaY[50,28] := 0.42480052; GammaY[51,28] := 0.446448646; GammaY[52,28] := 0.468913358; GammaY[53,28] := 0.492226695; GammaY[54,28] := 0.516422864; GammaY[55,28] := 0.541538439; GammaY[56,28] := 0.567612576; GammaY[57,28] := 0.594687259; GammaY[58,28] := 0.62280757; GammaY[59,28] := 0.652021985;
  //GammaY[60,28] := 0.682382748; GammaY[61,28] := 0.713946245; GammaY[62,28] := 0.746773422; GammaY[63,28] := 0.780930314; GammaY[64,28] := 0.816488607; GammaY[65,28] := 0.853526291; GammaY[66,28] := 0.892128412; GammaY[67,28] := 0.932387935; GammaY[68,28] := 0.974406746; GammaY[69,28] := 1.018297468;
  //GammaY[70,28] := 1.064182818; GammaY[71,28] := 1.112198608; GammaY[72,28] := 1.16249693; GammaY[73,28] := 1.215246337; GammaY[74,28] := 1.270635222; GammaY[75,28] := 1.328875027; GammaY[76,28] := 1.390204139; GammaY[77,28] := 1.454892628; GammaY[78,28] := 1.52324808; GammaY[79,28] := 1.595622675;
  //GammaY[80,28] := 1.672422733; GammaY[81,28] := 1.754119898; GammaY[82,28] := 1.841265509; GammaY[83,28] := 1.934510025; GammaY[84,28] := 2.034628466; GammaY[85,28] := 2.14255378; GammaY[86,28] := 2.259422383; GammaY[87,28] := 2.386638221; GammaY[88,28] := 2.525963941; GammaY[89,28] := 2.679653594;
  //GammaY[90,28] := 2.850653143; GammaY[91,28] := 3.042916963; GammaY[92,28] := 3.261926467; GammaY[93,28] := 3.515593544; GammaY[94,28] := 3.815954925; GammaY[95,28] := 4.182668915; GammaY[96,28] := 4.651295969; GammaY[97,28] := 5.297536587; GammaY[98,28] := 6.342663609; GammaY[99,28] := 8.829999756;
  //GammaY[0,29] := 4.79744E-05; GammaY[1,29] := 0.000356418; GammaY[2,29] := 0.000937137; GammaY[3,29] := 0.001817413; GammaY[4,29] := 0.003000577; GammaY[5,29] := 0.004489403; GammaY[6,29] := 0.006286436; GammaY[7,29] := 0.008394144; GammaY[8,29] := 0.010814997; GammaY[9,29] := 0.013551529;
  //GammaY[10,29] := 0.016606368; GammaY[11,29] := 0.019982264; GammaY[12,29] := 0.023682116; GammaY[13,29] := 0.027708981; GammaY[14,29] := 0.032066097; GammaY[15,29] := 0.036756895; GammaY[16,29] := 0.041785008; GammaY[17,29] := 0.047154287; GammaY[18,29] := 0.052868814; GammaY[19,29] := 0.058932909;
  //GammaY[20,29] := 0.06535115; GammaY[21,29] := 0.072128377; GammaY[22,29] := 0.079269712; GammaY[23,29] := 0.086780568; GammaY[24,29] := 0.094666669; GammaY[25,29] := 0.102934056; GammaY[26,29] := 0.111589113; GammaY[27,29] := 0.120638578; GammaY[28,29] := 0.130089562; GammaY[29,29] := 0.139949571;
  //GammaY[30,29] := 0.150226525; GammaY[31,29] := 0.16092878; GammaY[32,29] := 0.172065153; GammaY[33,29] := 0.183644946; GammaY[34,29] := 0.195677977; GammaY[35,29] := 0.208174608; GammaY[36,29] := 0.221145776; GammaY[37,29] := 0.234603033; GammaY[38,29] := 0.248558578; GammaY[39,29] := 0.263025301;
  //GammaY[40,29] := 0.278016827; GammaY[41,29] := 0.293547567; GammaY[42,29] := 0.309632766; GammaY[43,29] := 0.32628856; GammaY[44,29] := 0.343532047; GammaY[45,29] := 0.361381352; GammaY[46,29] := 0.379855692; GammaY[47,29] := 0.398975469; GammaY[48,29] := 0.418762357; GammaY[49,29] := 0.439239401;
  //GammaY[50,29] := 0.460431124; GammaY[51,29] := 0.482363651; GammaY[52,29] := 0.505064838; GammaY[53,29] := 0.528564422; GammaY[54,29] := 0.552894185; GammaY[55,29] := 0.578088132; GammaY[56,29] := 0.604182687; GammaY[57,29] := 0.631216946; GammaY[58,29] := 0.659232915; GammaY[59,29] := 0.688275776;
  //GammaY[60,29] := 0.718394236; GammaY[61,29] := 0.749640875; GammaY[62,29] := 0.782072563; GammaY[63,29] := 0.815750926; GammaY[64,29] := 0.850742873; GammaY[65,29] := 0.887121213; GammaY[66,29] := 0.924965348; GammaY[67,29] := 0.964362081; GammaY[68,29] := 1.005407106; GammaY[69,29] := 1.048204396;
  //GammaY[70,29] := 1.092868645; GammaY[71,29] := 1.139527861; GammaY[72,29] := 1.188323396; GammaY[73,29] := 1.239412472; GammaY[74,29] := 1.292971112; GammaY[75,29] := 1.349196843; GammaY[76,29] := 1.408312026; GammaY[77,29] := 1.470568702; GammaY[78,29] := 1.53625373; GammaY[79,29] := 1.605695971;
  //GammaY[80,29] := 1.67927445; GammaY[81,29] := 1.757428588; GammaY[82,29] := 1.840672749; GammaY[83,29] := 1.929613603; GammaY[84,29] := 2.024973087; GammaY[85,29] := 2.127620503; GammaY[86,29] := 2.238614782; GammaY[87,29] := 2.359263336; GammaY[88,29] := 2.491207297; GammaY[89,29] := 2.636544706;
  //GammaY[90,29] := 2.798016768; GammaY[91,29] := 2.979301779; GammaY[92,29] := 3.185496493; GammaY[93,29] := 3.423955081; GammaY[94,29] := 3.70586066; GammaY[95,29] := 4.049468245; GammaY[96,29] := 4.487782439; GammaY[97,29] := 5.091015771; GammaY[98,29] := 6.064214442; GammaY[99,29] := 8.371648332;
  //GammaY[0,30] := 0.00010226; GammaY[1,30] := 0.000662607; GammaY[2,30] := 0.00162312; GammaY[3,30] := 0.002988499; GammaY[4,30] := 0.004739872; GammaY[5,30] := 0.00686473; GammaY[6,30] := 0.009354086; GammaY[7,30] := 0.012201248; GammaY[8,30] := 0.015401156; GammaY[9,30] := 0.01895;
  //GammaY[10,30] := 0.022844969; GammaY[11,30] := 0.027084079; GammaY[12,30] := 0.031666058; GammaY[13,30] := 0.036590257; GammaY[14,30] := 0.041856589; GammaY[15,30] := 0.047465476; GammaY[16,30] := 0.053417819; GammaY[17,30] := 0.059714966; GammaY[18,30] := 0.066358692; GammaY[19,30] := 0.073351187;
  //GammaY[20,30] := 0.080695042; GammaY[21,30] := 0.088393243; GammaY[22,30] := 0.096449169; GammaY[23,30] := 0.10486659; GammaY[24,30] := 0.113649667; GammaY[25,30] := 0.122802959; GammaY[26,30] := 0.132331426; GammaY[27,30] := 0.142240442; GammaY[28,30] := 0.1525358; GammaY[29,30] := 0.163223724;
  //GammaY[30,30] := 0.174310887; GammaY[31,30] := 0.185804426; GammaY[32,30] := 0.197711958; GammaY[33,30] := 0.2100416; GammaY[34,30] := 0.222801995; GammaY[35,30] := 0.236002333; GammaY[36,30] := 0.249652381; GammaY[37,30] := 0.263762512; GammaY[38,30] := 0.278343739; GammaY[39,30] := 0.29340775;
  //GammaY[40,30] := 0.308966943; GammaY[41,30] := 0.325034482; GammaY[42,30] := 0.341624339; GammaY[43,30] := 0.358751336; GammaY[44,30] := 0.376431215; GammaY[45,30] := 0.394680694; GammaY[46,30] := 0.413517542; GammaY[47,30] := 0.432960645; GammaY[48,30] := 0.453030097; GammaY[49,30] := 0.47374729;
  //GammaY[50,30] := 0.495135012; GammaY[51,30] := 0.517217562; GammaY[52,30] := 0.54002087; GammaY[53,30] := 0.563572629; GammaY[54,30] := 0.587902466; GammaY[55,30] := 0.613042104; GammaY[56,30] := 0.639025528; GammaY[57,30] := 0.665889223; GammaY[58,30] := 0.693672398; GammaY[59,30] := 0.722417257;
  //GammaY[60,30] := 0.752169295; GammaY[61,30] := 0.782977639; GammaY[62,30] := 0.814895425; GammaY[63,30] := 0.84798024; GammaY[64,30] := 0.882294616; GammaY[65,30] := 0.917906571; GammaY[66,30] := 0.954890327; GammaY[67,30] := 0.993327547; GammaY[68,30] := 1.033306607; GammaY[69,30] := 1.074924702;
  //GammaY[70,30] := 1.118289894; GammaY[71,30] := 1.16352137; GammaY[72,30] := 1.210751332; GammaY[73,30] := 1.26012665; GammaY[74,30] := 1.311811453; GammaY[75,30] := 1.365990199; GammaY[76,30] := 1.422870789; GammaY[77,30] := 1.482688255; GammaY[78,30] := 1.545710624; GammaY[79,30] := 1.612244815;
  //GammaY[80,30] := 1.682644077; GammaY[81,30] := 1.757318729; GammaY[82,30] := 1.836748406; GammaY[83,30] := 1.921498669; GammaY[84,30] := 2.01224294; GammaY[85,30] := 2.10979108; GammaY[86,30] := 2.21512979; GammaY[87,30] := 2.329477265; GammaY[88,30] := 2.45436176; GammaY[89,30] := 2.591736471;
  //GammaY[90,30] := 2.744152967; GammaY[91,30] := 2.915033503; GammaY[92,30] := 3.109118461; GammaY[93,30] := 3.333245334; GammaY[94,30] := 3.597806589; GammaY[95,30] := 3.919759363; GammaY[96,30] := 4.32974437; GammaY[97,30] := 4.892902215; GammaY[98,30] := 5.799297464; GammaY[99,30] := 7.940407258;
  //GammaY[0,31] := 0.000205062; GammaY[1,31] := 0.001168223; GammaY[2,31] := 0.002678974; GammaY[3,31] := 0.004701732; GammaY[4,31] := 0.007186964; GammaY[5,31] := 0.010103009; GammaY[6,31] := 0.013427447; GammaY[7,31] := 0.017143523; GammaY[8,31] := 0.021238312; GammaY[9,31] := 0.025701666;
  //GammaY[10,31] := 0.030525543; GammaY[11,31] := 0.035703566; GammaY[12,31] := 0.041230714; GammaY[13,31] := 0.047103101; GammaY[14,31] := 0.053317807; GammaY[15,31] := 0.059872762; GammaY[16,31] := 0.066766644; GammaY[17,31] := 0.073998806; GammaY[18,31] := 0.081569222; GammaY[19,31] := 0.089478438;
  //GammaY[20,31] := 0.097727539; GammaY[21,31] := 0.10631812; GammaY[22,31] := 0.115252265; GammaY[23,31] := 0.124532532; GammaY[24,31] := 0.134161942; GammaY[25,31] := 0.144143972; GammaY[26,31] := 0.154482551; GammaY[27,31] := 0.165182058; GammaY[28,31] := 0.176247328; GammaY[29,31] := 0.187683655;
  //GammaY[30,31] := 0.1994968; GammaY[31,31] := 0.211693002; GammaY[32,31] := 0.224278991; GammaY[33,31] := 0.237262002; GammaY[34,31] := 0.250649792; GammaY[35,31] := 0.264450665; GammaY[36,31] := 0.278673488; GammaY[37,31] := 0.293327724; GammaY[38,31] := 0.308423459; GammaY[39,31] := 0.323971428;
  //GammaY[40,31] := 0.339983058; GammaY[41,31] := 0.356470502; GammaY[42,31] := 0.373446686; GammaY[43,31] := 0.390925354; GammaY[44,31] := 0.408921122; GammaY[45,31] := 0.427449532; GammaY[46,31] := 0.44652712; GammaY[47,31] := 0.466171483; GammaY[48,31] := 0.486401355; GammaY[49,31] := 0.507236695;
  //GammaY[50,31] := 0.52869877; GammaY[51,31] := 0.550810281; GammaY[52,31] := 0.573595465; GammaY[53,31] := 0.597080209; GammaY[54,31] := 0.62129221; GammaY[55,31] := 0.646261128; GammaY[56,31] := 0.672018757; GammaY[57,31] := 0.698599229; GammaY[58,31] := 0.726039228; GammaY[59,31] := 0.754378243;
  //GammaY[60,31] := 0.78365884; GammaY[61,31] := 0.813926987; GammaY[62,31] := 0.845232384; GammaY[63,31] := 0.877628923; GammaY[64,31] := 0.911175121; GammaY[65,31] := 0.945934621; GammaY[66,31] := 0.981977295; GammaY[67,31] := 1.019378579; GammaY[68,31] := 1.05822103; GammaY[69,31] := 1.098596587;
  //GammaY[70,31] := 1.140606074; GammaY[71,31] := 1.184360527; GammaY[72,31] := 1.229983502; GammaY[73,31] := 1.277612521; GammaY[74,31] := 1.327400823; GammaY[75,31] := 1.379520739; GammaY[76,31] := 1.434166341; GammaY[77,31] := 1.491557369; GammaY[78,31] := 1.551944102; GammaY[79,31] := 1.615612683;
  //GammaY[80,31] := 1.682893188; GammaY[81,31] := 1.754168402; GammaY[82,31] := 1.829885782; GammaY[83,31] := 1.910572869; GammaY[84,31] := 1.996857517; GammaY[85,31] := 2.089494993; GammaY[86,31] := 2.189404843; GammaY[87,31] := 2.297722181; GammaY[88,31] := 2.415870658; GammaY[89,31] := 2.545669221;
  //GammaY[90,31] := 2.689493348; GammaY[91,31] := 2.850528035; GammaY[92,31] := 3.033183476; GammaY[93,31] := 3.243818353; GammaY[94,31] := 3.492093631; GammaY[95,31] := 3.793765078; GammaY[96,31] := 4.177287245; GammaY[97,31] := 4.703113618; GammaY[98,31] := 5.54747523; GammaY[99,31] := 7.534820025;
  //GammaY[0,32] := 0.000388737; GammaY[1,32] := 0.001961921; GammaY[2,32] := 0.004231054; GammaY[3,32] := 0.007104766; GammaY[4,32] := 0.010498477; GammaY[5,32] := 0.014360558; GammaY[6,32] := 0.018655535; GammaY[7,32] := 0.02335743; GammaY[8,32] := 0.02844647; GammaY[9,32] := 0.033907231;
  //GammaY[10,32] := 0.039727497; GammaY[11,32] := 0.045897513; GammaY[12,32] := 0.052409475; GammaY[13,32] := 0.059257168; GammaY[14,32] := 0.066435699; GammaY[15,32] := 0.073941299; GammaY[16,32] := 0.081771171; GammaY[17,32] := 0.089923377; GammaY[18,32] := 0.098396737; GammaY[19,32] := 0.107190769;
  //GammaY[20,32] := 0.116305622; GammaY[21,32] := 0.125742037; GammaY[22,32] := 0.135501307; GammaY[23,32] := 0.145585251; GammaY[24,32] := 0.155996196; GammaY[25,32] := 0.166736954; GammaY[26,32] := 0.177810818; GammaY[27,32] := 0.189221549; GammaY[28,32] := 0.200973379; GammaY[29,32] := 0.213071003;
  //GammaY[30,32] := 0.225519592; GammaY[31,32] := 0.23832479; GammaY[32,32] := 0.251492727; GammaY[33,32] := 0.265030029; GammaY[34,32] := 0.278943835; GammaY[35,32] := 0.293241816; GammaY[36,32] := 0.307932183; GammaY[37,32] := 0.323023722; GammaY[38,32] := 0.338525814; GammaY[39,32] := 0.354448463;
  //GammaY[40,32] := 0.370802332; GammaY[41,32] := 0.387598773; GammaY[42,32] := 0.404849872; GammaY[43,32] := 0.422568487; GammaY[44,32] := 0.440768302; GammaY[45,32] := 0.459463875; GammaY[46,32] := 0.4786707; GammaY[47,32] := 0.49840526; GammaY[48,32] := 0.518685125; GammaY[49,32] := 0.539529012;
  //GammaY[50,32] := 0.560956866; GammaY[51,32] := 0.582989969; GammaY[52,32] := 0.605651042; GammaY[53,32] := 0.628964365; GammaY[54,32] := 0.65295591; GammaY[55,32] := 0.677653485; GammaY[56,32] := 0.703086901; GammaY[57,32] := 0.729288153; GammaY[58,32] := 0.756291628; GammaY[59,32] := 0.784134319;
  //GammaY[60,32] := 0.812856126; GammaY[61,32] := 0.842500132; GammaY[62,32] := 0.873112903; GammaY[63,32] := 0.904744906; GammaY[64,32] := 0.937450927; GammaY[65,32] := 0.97129057; GammaY[66,32] := 1.006329137; GammaY[67,32] := 1.042637357; GammaY[68,32] := 1.080292736; GammaY[69,32] := 1.119380934;
  //GammaY[70,32] := 1.159996259; GammaY[71,32] := 1.202242819; GammaY[72,32] := 1.246236037; GammaY[73,32] := 1.292104493; GammaY[74,32] := 1.339991434; GammaY[75,32] := 1.390057926; GammaY[76,32] := 1.442485312; GammaY[77,32] := 1.497478888; GammaY[78,32] := 1.555272308; GammaY[79,32] := 1.616133017;
  //GammaY[80,32] := 1.680369078; GammaY[81,32] := 1.748337681; GammaY[82,32] := 1.820456634; GammaY[83,32] := 1.897218024; GammaY[84,32] := 1.979207513; GammaY[85,32] := 2.067129427; GammaY[86,32] := 2.161841086; GammaY[87,32] := 2.264400489; GammaY[88,32] := 2.376134219; GammaY[89,32] := 2.498736691;
  //GammaY[90,32] := 2.634419984; GammaY[91,32] := 2.786149345; GammaY[92,32] := 2.958028897; GammaY[93,32] := 3.155973897; GammaY[94,32] := 3.38896787; GammaY[95,32] := 3.671654896; GammaY[96,32] := 4.030467697; GammaY[97,32] := 4.521527532; GammaY[98,32] := 5.308291671; GammaY[99,32] := 7.1534888;
  //GammaY[0,33] := 0.000699745; GammaY[1,33] := 0.0031513; GammaY[2,33] := 0.006418742; GammaY[3,33] := 0.010348677; GammaY[4,33] := 0.01482429; GammaY[5,33] := 0.019777466; GammaY[6,33] := 0.02516285; GammaY[7,33] := 0.030948006; GammaY[8,33] := 0.0371087; GammaY[9,33] := 0.0436263;
  //GammaY[10,33] := 0.050486213; GammaY[11,33] := 0.057676882; GammaY[12,33] := 0.065189108; GammaY[13,33] := 0.073015578; GammaY[14,33] := 0.08115052; GammaY[15,33] := 0.08958945; GammaY[16,33] := 0.09832898; GammaY[17,33] := 0.107366672; GammaY[18,33] := 0.116700925; GammaY[19,33] := 0.126330879;
  //GammaY[20,33] := 0.136256353; GammaY[21,33] := 0.14647778; GammaY[22,33] := 0.156996171; GammaY[23,33] := 0.167813072; GammaY[24,33] := 0.178930544; GammaY[25,33] := 0.190351135; GammaY[26,33] := 0.202077873; GammaY[27,33] := 0.214114245; GammaY[28,33] := 0.226464202; GammaY[29,33] := 0.239132144;
  //GammaY[30,33] := 0.25212293; GammaY[31,33] := 0.265441878; GammaY[32,33] := 0.279094773; GammaY[33,33] := 0.293087873; GammaY[34,33] := 0.30742792; GammaY[35,33] := 0.322122163; GammaY[36,33] := 0.337178366; GammaY[37,33] := 0.352604834; GammaY[38,33] := 0.368410434; GammaY[39,33] := 0.384604621;
  //GammaY[40,33] := 0.401197467; GammaY[41,33] := 0.418199697; GammaY[42,33] := 0.435622721; GammaY[43,33] := 0.453478672; GammaY[44,33] := 0.471780463; GammaY[45,33] := 0.49054183; GammaY[46,33] := 0.509777377; GammaY[47,33] := 0.529502647; GammaY[48,33] := 0.549734186; GammaY[49,33] := 0.570489617;
  //GammaY[50,33] := 0.59178772; GammaY[51,33] := 0.613648522; GammaY[52,33] := 0.636093398; GammaY[53,33] := 0.659145182; GammaY[54,33] := 0.68282829; GammaY[55,33] := 0.707168856; GammaY[56,33] := 0.732194872; GammaY[57,33] := 0.757936397; GammaY[58,33] := 0.78442573; GammaY[59,33] := 0.811697598;
  //GammaY[60,33] := 0.839789431; GammaY[61,33] := 0.868741633; GammaY[62,33] := 0.898597885; GammaY[63,33] := 0.929405505; GammaY[64,33] := 0.961215849; GammaY[65,33] := 0.994085056; GammaY[66,33] := 1.02807376; GammaY[67,33] := 1.063248088; GammaY[68,33] := 1.099681272; GammaY[69,33] := 1.137453355;
  //GammaY[70,33] := 1.176652497; GammaY[71,33] := 1.217376119; GammaY[72,33] := 1.259732218; GammaY[73,33] := 1.303840946; GammaY[74,33] := 1.349836493; GammaY[75,33] := 1.397869341; GammaY[76,33] := 1.448109004; GammaY[77,33] := 1.500747342; GammaY[78,33] := 1.556002571; GammaY[79,33] := 1.614124734;
  //GammaY[80,33] := 1.675401438; GammaY[81,33] := 1.740166313; GammaY[82,33] := 1.808809178; GammaY[83,33] := 1.88178931; GammaY[84,33] := 1.959653358; GammaY[85,33] := 2.043058251; GammaY[86,33] := 2.132803531; GammaY[87,33] := 2.229875622; GammaY[88,33] := 2.335510935; GammaY[89,33] := 2.451288547;
  //GammaY[90,33] := 2.579269086; GammaY[91,33] := 2.722213728; GammaY[92,33] := 2.883943128; GammaY[93,33] := 3.069961721; GammaY[94,33] := 3.288625924; GammaY[95,33] := 3.553550954; GammaY[96,33] := 3.889299438; GammaY[97,33] := 4.347988418; GammaY[98,33] := 5.081278354; GammaY[99,33] := 6.795067826;
  //GammaY[0,34] := 0.001200866; GammaY[1,34] := 0.004859421; GammaY[2,34] := 0.009386611; GammaY[3,34] := 0.014578114; GammaY[4,34] := 0.020297394; GammaY[5,34] := 0.026468264; GammaY[6,34] := 0.03304154; GammaY[7,34] := 0.039982904; GammaY[8,34] := 0.047267231; GammaY[9,34] := 0.054875549;
  //GammaY[10,34] := 0.062793236; GammaY[11,34] := 0.071008888; GammaY[12,34] := 0.07951356; GammaY[13,34] := 0.088300248; GammaY[14,34] := 0.097363514; GammaY[15,34] := 0.106699214; GammaY[16,34] := 0.116304293; GammaY[17,34] := 0.126176629; GammaY[18,34] := 0.136314912; GammaY[19,34] := 0.146718557;
  //GammaY[20,34] := 0.157387621; GammaY[21,34] := 0.168322753; GammaY[22,34] := 0.179525147; GammaY[23,34] := 0.190996502; GammaY[24,34] := 0.202738999; GammaY[25,34] := 0.21475528; GammaY[26,34] := 0.227048425; GammaY[27,34] := 0.239621952; GammaY[28,34] := 0.252479805; GammaY[29,34] := 0.265626347;
  //GammaY[30,34] := 0.279066365; GammaY[31,34] := 0.292805071; GammaY[32,34] := 0.306848111; GammaY[33,34] := 0.321201568; GammaY[34,34] := 0.335871978; GammaY[35,34] := 0.350866342; GammaY[36,34] := 0.366192143; GammaY[37,34] := 0.381857362; GammaY[38,34] := 0.397870506; GammaY[39,34] := 0.414240625;
  //GammaY[40,34] := 0.430977342; GammaY[41,34] := 0.448090889; GammaY[42,34] := 0.465592143; GammaY[43,34] := 0.483492646; GammaY[44,34] := 0.501804666; GammaY[45,34] := 0.520541235; GammaY[46,34] := 0.539716201; GammaY[47,34] := 0.559344286; GammaY[48,34] := 0.579441147; GammaY[49,34] := 0.600023445;
  //GammaY[50,34] := 0.621108922; GammaY[51,34] := 0.642716485; GammaY[52,34] := 0.6648663; GammaY[53,34] := 0.687579879; GammaY[54,34] := 0.710880238; GammaY[55,34] := 0.734791996; GammaY[56,34] := 0.759341504; GammaY[57,34] := 0.78455703; GammaY[58,34] := 0.810468932; GammaY[59,34] := 0.837109858;
  //GammaY[60,34] := 0.864514975; GammaY[61,34] := 0.892722224; GammaY[62,34] := 0.921772614; GammaY[63,34] := 0.95171055; GammaY[64,34] := 0.982584185; GammaY[65,34] := 1.01444624; GammaY[66,34] := 1.04735343; GammaY[67,34] := 1.081367775; GammaY[68,34] := 1.11655804; GammaY[69,34] := 1.152999091;
  //GammaY[70,34] := 1.190773495; GammaY[71,34] := 1.229972455; GammaY[72,34] := 1.270697064; GammaY[73,34] := 1.31305975; GammaY[74,34] := 1.357185958; GammaY[75,34] := 1.403216643; GammaY[76,34] := 1.451310209; GammaY[77,34] := 1.501645945; GammaY[78,34] := 1.554428089; GammaY[79,34] := 1.609890008;
  //GammaY[80,34] := 1.668300513; GammaY[81,34] := 1.729971205; GammaY[82,34] := 1.795266021; GammaY[83,34] := 1.864614016; GammaY[84,34] := 1.938525135; GammaY[85,34] := 2.017612389; GammaY[86,34] := 2.102621839; GammaY[87,34] := 2.194473382; GammaY[88,34] := 2.294319838; GammaY[89,34] := 2.403633104;
  //GammaY[90,34] := 2.524333349; GammaY[91,34] := 2.658992644; GammaY[92,34] := 2.811169016; GammaY[93,34] := 2.985986377; GammaY[94,34] := 3.191220105; GammaY[95,34] := 3.439533435; GammaY[96,34] := 3.75375908; GammaY[97,34] := 4.182312508; GammaY[98,34] := 4.865958659; GammaY[99,34] := 6.458269652;
  //GammaY[0,35] := 0.001972153; GammaY[1,35] := 0.007219036; GammaY[2,35] := 0.013275382; GammaY[3,35] := 0.019921743; GammaY[4,35] := 0.027025325; GammaY[5,35] := 0.034515126; GammaY[6,35] := 0.042346719; GammaY[7,35] := 0.050489904; GammaY[8,35] := 0.058923092; GammaY[9,35] := 0.067630348;
  //GammaY[10,35] := 0.076599676; GammaY[11,35] := 0.085821957; GammaY[12,35] := 0.095290243; GammaY[13,35] := 0.104999285; GammaY[14,35] := 0.114945194; GammaY[15,35] := 0.125125195; GammaY[16,35] := 0.135537448; GammaY[17,35] := 0.14618091; GammaY[18,35] := 0.157055232; GammaY[19,35] := 0.168160677;
  //GammaY[20,35] := 0.179498061; GammaY[21,35] := 0.191068701; GammaY[22,35] := 0.202874377; GammaY[23,35] := 0.214917304; GammaY[24,35] := 0.227200113; GammaY[25,35] := 0.239725831; GammaY[26,35] := 0.252497866; GammaY[27,35] := 0.265520005; GammaY[28,35] := 0.278796408; GammaY[29,35] := 0.292331607;
  //GammaY[30,35] := 0.306130508; GammaY[31,35] := 0.320198394; GammaY[32,35] := 0.334540935; GammaY[33,35] := 0.349164196; GammaY[34,35] := 0.364074647; GammaY[35,35] := 0.37927918; GammaY[36,35] := 0.39478512; GammaY[37,35] := 0.410600254; GammaY[38,35] := 0.426732846; GammaY[39,35] := 0.443191656;
  //GammaY[40,35] := 0.459985973; GammaY[41,35] := 0.477125642; GammaY[42,35] := 0.4946211; GammaY[43,35] := 0.512483406; GammaY[44,35] := 0.530724287; GammaY[45,35] := 0.549356178; GammaY[46,35] := 0.56839227; GammaY[47,35] := 0.587846566; GammaY[48,35] := 0.607733939; GammaY[49,35] := 0.628070195;
  //GammaY[50,35] := 0.648872133; GammaY[51,35] := 0.670157659; GammaY[52,35] := 0.691945853; GammaY[53,35] := 0.714257045; GammaY[54,35] := 0.737112952; GammaY[55,35] := 0.760536789; GammaY[56,35] := 0.784553406; GammaY[57,35] := 0.809189431; GammaY[58,35] := 0.834473447; GammaY[59,35] := 0.860436175;
  //GammaY[60,35] := 0.887110688; GammaY[61,35] := 0.914532633; GammaY[62,35] := 0.942740553; GammaY[63,35] := 0.971776172; GammaY[64,35] := 1.001684893; GammaY[65,35] := 1.032515733; GammaY[66,35] := 1.064321952; GammaY[67,35] := 1.097162266; GammaY[68,35] := 1.131100677; GammaY[69,35] := 1.166207357;
  //GammaY[70,35] := 1.202559861; GammaY[71,35] := 1.240243514; GammaY[72,35] := 1.279352885; GammaY[73,35] := 1.319993422; GammaY[74,35] := 1.36228252; GammaY[75,35] := 1.406351794; GammaY[76,35] := 1.452349706; GammaY[77,35] := 1.500443913; GammaY[78,35] := 1.550825129; GammaY[79,35] := 1.603711773;
  //GammaY[80,35] := 1.659354953; GammaY[81,35] := 1.718045747; GammaY[82,35] := 1.780124297; GammaY[83,35] := 1.845991185; GammaY[84,35] := 1.916122546; GammaY[85,35] := 1.991090605; GammaY[86,35] := 2.071591568; GammaY[87,35] := 2.158483667; GammaY[88,35] := 2.252842144; GammaY[89,35] := 2.356039382;
  //GammaY[90,35] := 2.469865412; GammaY[91,35] := 2.596717041; GammaY[92,35] := 2.739908532; GammaY[93,35] := 2.904211457; GammaY[94,35] := 3.096863293; GammaY[95,35] := 3.329645741; GammaY[96,35] := 3.623791342; GammaY[97,35] := 4.024293328; GammaY[98,35] := 4.661852074; GammaY[99,35] := 6.141861145;
  //GammaY[0,36] := 0.00311012; GammaY[1,36] := 0.010365158; GammaY[2,36] := 0.018212874; GammaY[3,36] := 0.026484166; GammaY[4,36] := 0.035084094; GammaY[5,36] := 0.043964142; GammaY[6,36] := 0.053095068; GammaY[7,36] := 0.06245769; GammaY[8,36] := 0.072038814; GammaY[9,36] := 0.081829115;
  //GammaY[10,36] := 0.091821943; GammaY[11,36] := 0.102012579; GammaY[12,36] := 0.11239776; GammaY[13,36] := 0.122975359; GammaY[14,36] := 0.133744157; GammaY[15,36] := 0.144703682; GammaY[16,36] := 0.155854094; GammaY[17,36] := 0.167196097; GammaY[18,36] := 0.17873087; GammaY[19,36] := 0.190460022;
  //GammaY[20,36] := 0.202385549; GammaY[21,36] := 0.214509811; GammaY[22,36] := 0.226835508; GammaY[23,36] := 0.239365661; GammaY[24,36] := 0.252103609; GammaY[25,36] := 0.265052992; GammaY[26,36] := 0.278217755; GammaY[27,36] := 0.291602146; GammaY[28,36] := 0.305210711; GammaY[29,36] := 0.319048306;
  //GammaY[30,36] := 0.333120098; GammaY[31,36] := 0.347431576; GammaY[32,36] := 0.361988557; GammaY[33,36] := 0.376797195; GammaY[34,36] := 0.391864007; GammaY[35,36] := 0.40719588; GammaY[36,36] := 0.422800082; GammaY[37,36] := 0.438684292; GammaY[38,36] := 0.454856616; GammaY[39,36] := 0.471325611;
  //GammaY[40,36] := 0.488100313; GammaY[41,36] := 0.505190265; GammaY[42,36] := 0.522605548; GammaY[43,36] := 0.540356815; GammaY[44,36] := 0.558455332; GammaY[45,36] := 0.576913017; GammaY[46,36] := 0.595742477; GammaY[47,36] := 0.614957086; GammaY[48,36] := 0.634571022; GammaY[49,36] := 0.654599317;
  //GammaY[50,36] := 0.675057941; GammaY[51,36] := 0.695963874; GammaY[52,36] := 0.717335186; GammaY[53,36] := 0.73919113; GammaY[54,36] := 0.761552241; GammaY[55,36] := 0.784440452; GammaY[56,36] := 0.80787922; GammaY[57,36] := 0.831893663; GammaY[58,36] := 0.856510702; GammaY[59,36] := 0.881759287;
  //GammaY[60,36] := 0.907670569; GammaY[61,36] := 0.934278088; GammaY[62,36] := 0.961618076; GammaY[63,36] := 0.989729954; GammaY[64,36] := 1.018655943; GammaY[65,36] := 1.048441983; GammaY[66,36] := 1.079138698; GammaY[67,36] := 1.110800848; GammaY[68,36] := 1.143488385; GammaY[69,36] := 1.177267297;
  //GammaY[70,36] := 1.212210168; GammaY[71,36] := 1.248396962; GammaY[72,36] := 1.285916279; GammaY[73,36] := 1.324866867; GammaY[74,36] := 1.365358639; GammaY[75,36] := 1.407514791; GammaY[76,36] := 1.451474179; GammaY[77,36] := 1.497393822; GammaY[78,36] := 1.545452088; GammaY[79,36] := 1.595852985;
  //GammaY[80,36] := 1.648831452; GammaY[81,36] := 1.704659694; GammaY[82,36] := 1.763655379; GammaY[83,36] := 1.826192612; GammaY[84,36] := 1.892716261; GammaY[85,36] := 1.963760707; GammaY[86,36] := 2.039975786; GammaY[87,36] := 2.122162489; GammaY[88,36] := 2.211324062; GammaY[89,36] := 2.308740595;
  //GammaY[90,36] := 2.416081273; GammaY[91,36] := 2.535580387; GammaY[92,36] := 2.670326438; GammaY[93,36] := 2.824764285; GammaY[94,36] := 3.005633311; GammaY[95,36] := 3.223899288; GammaY[96,36] := 3.499313915; GammaY[97,36] := 3.873706185; GammaY[98,36] := 4.468478085; GammaY[99,36] := 5.844662524;
  //GammaY[0,37] := 0.004724949; GammaY[1,37] := 0.014426908; GammaY[2,37] := 0.024306094; GammaY[3,37] := 0.034340182; GammaY[4,37] := 0.044515037; GammaY[5,37] := 0.05482467; GammaY[6,37] := 0.065266405; GammaY[7,37] := 0.075839286; GammaY[8,37] := 0.08654339; GammaY[9,37] := 0.097379482;
  //GammaY[10,37] := 0.10834882; GammaY[11,37] := 0.119453036; GammaY[12,37] := 0.130694068; GammaY[13,37] := 0.142074109; GammaY[14,37] := 0.153595577; GammaY[15,37] := 0.165261092; GammaY[16,37] := 0.177073466; GammaY[17,37] := 0.189035689; GammaY[18,37] := 0.20115093; GammaY[19,37] := 0.213422529;
  //GammaY[20,37] := 0.225854; GammaY[21,37] := 0.23844903; GammaY[22,37] := 0.251211486; GammaY[23,37] := 0.264145414; GammaY[24,37] := 0.27725505; GammaY[25,37] := 0.290544823; GammaY[26,37] := 0.304019362; GammaY[27,37] := 0.317683508; GammaY[28,37] := 0.331542319; GammaY[29,37] := 0.345601083;
  //GammaY[30,37] := 0.359865331; GammaY[31,37] := 0.37434085; GammaY[32,37] := 0.389033689; GammaY[33,37] := 0.403950181; GammaY[34,37] := 0.419096958; GammaY[35,37] := 0.434480967; GammaY[36,37] := 0.45010949; GammaY[37,37] := 0.465990164; GammaY[38,37] := 0.482131004; GammaY[39,37] := 0.498540425;
  //GammaY[40,37] := 0.51522727; GammaY[41,37] := 0.532200839; GammaY[42,37] := 0.54947092; GammaY[43,37] := 0.567047811; GammaY[44,37] := 0.584942383; GammaY[45,37] := 0.603166109; GammaY[46,37] := 0.621731088; GammaY[47,37] := 0.640650114; GammaY[48,37] := 0.659936727; GammaY[49,37] := 0.679605267;
  //GammaY[50,37] := 0.699670938; GammaY[51,37] := 0.72014988; GammaY[52,37] := 0.741059247; GammaY[53,37] := 0.76241729; GammaY[54,37] := 0.784243457; GammaY[55,37] := 0.80655848; GammaY[56,37] := 0.829384536; GammaY[57,37] := 0.85274536; GammaY[58,37] := 0.876666355; GammaY[59,37] := 0.901174801;
  //GammaY[60,37] := 0.926300031; GammaY[61,37] := 0.952073636; GammaY[62,37] := 0.978529709; GammaY[63,37] := 1.005705226; GammaY[64,37] := 1.033639991; GammaY[65,37] := 1.062377262; GammaY[66,37] := 1.09196445; GammaY[67,37] := 1.122452982; GammaY[68,37] := 1.153899132; GammaY[69,37] := 1.186364783;
  //GammaY[70,37] := 1.219917932; GammaY[71,37] := 1.254633692; GammaY[72,37] := 1.290595091; GammaY[73,37] := 1.327894422; GammaY[74,37] := 1.3666348; GammaY[75,37] := 1.406931659; GammaY[76,37] := 1.448914898; GammaY[77,37] := 1.492731462; GammaY[78,37] := 1.538548263; GammaY[79,37] := 1.58655615;
  //GammaY[80,37] := 1.63697483; GammaY[81,37] := 1.69005875; GammaY[82,37] := 1.746104938; GammaY[83,37] := 1.805463127; GammaY[84,37] := 1.868548832; GammaY[85,37] := 1.935861168; GammaY[86,37] := 2.008007006; GammaY[87,37] := 2.085734226; GammaY[88,37] := 2.16997927; GammaY[89,37] := 2.261936542;
  //GammaY[90,37] := 2.363163105; GammaY[91,37] := 2.475742543; GammaY[92,37] := 2.602554056; GammaY[93,37] := 2.747739771; GammaY[94,37] := 2.917577553; GammaY[95,37] := 3.122277786; GammaY[96,37] := 3.380221903; GammaY[97,37] := 3.730312366; GammaY[98,37] := 4.285359261; GammaY[99,37] := 5.565547672;
  //GammaY[0,38] := 0.006935834; GammaY[1,38] := 0.019519665; GammaY[2,38] := 0.031635314; GammaY[3,38] := 0.043531768; GammaY[4,38] := 0.055324536; GammaY[5,38] := 0.067071352; GammaY[6,38] := 0.078807545; GammaY[7,38] := 0.090557328; GammaY[8,38] := 0.102338598; GammaY[9,38] := 0.114165345;
  //GammaY[10,38] := 0.126048988; GammaY[11,38] := 0.137999182; GammaY[12,38] := 0.150024329; GammaY[13,38] := 0.162131925; GammaY[14,38] := 0.174328797; GammaY[15,38] := 0.186621277; GammaY[16,38] := 0.199015329; GammaY[17,38] := 0.211516641; GammaY[18,38] := 0.224130708; GammaY[19,38] := 0.236862886;
  //GammaY[20,38] := 0.249718441; GammaY[21,38] := 0.262702593; GammaY[22,38] := 0.275820546; GammaY[23,38] := 0.289077522; GammaY[24,38] := 0.302478782; GammaY[25,38] := 0.316029655; GammaY[26,38] := 0.329735551; GammaY[27,38] := 0.343601996; GammaY[28,38] := 0.357634639; GammaY[29,38] := 0.371839273;
  //GammaY[30,38] := 0.386221855; GammaY[31,38] := 0.400788525; GammaY[32,38] := 0.415545621; GammaY[33,38] := 0.430499705; GammaY[34,38] := 0.445657573; GammaY[35,38] := 0.461026284; GammaY[36,38] := 0.476613174; GammaY[37,38] := 0.492425883; GammaY[38,38] := 0.508472376; GammaY[39,38] := 0.524760963;
  //GammaY[40,38] := 0.541300342; GammaY[41,38] := 0.558099621; GammaY[42,38] := 0.575168329; GammaY[43,38] := 0.592516474; GammaY[44,38] := 0.610154567; GammaY[45,38] := 0.628093668; GammaY[46,38] := 0.646345419; GammaY[47,38] := 0.664922098; GammaY[48,38] := 0.683836669; GammaY[49,38] := 0.70310283;
  //GammaY[50,38] := 0.722735082; GammaY[51,38] := 0.742748792; GammaY[52,38] := 0.76316025; GammaY[53,38] := 0.783986793; GammaY[54,38] := 0.805246876; GammaY[55,38] := 0.826960133; GammaY[56,38] := 0.849147535; GammaY[57,38] := 0.871831496; GammaY[58,38] := 0.895036018; GammaY[59,38] := 0.918786839;
  //GammaY[60,38] := 0.943111614; GammaY[61,38] := 0.968040109; GammaY[62,38] := 0.993604267; GammaY[63,38] := 1.019838948; GammaY[64,38] := 1.046781794; GammaY[65,38] := 1.074473487; GammaY[66,38] := 1.102958457; GammaY[67,38] := 1.132284989; GammaY[68,38] := 1.162505982; GammaY[69,38] := 1.193679377;
  //GammaY[70,38] := 1.225869041; GammaY[71,38] := 1.25914542; GammaY[72,38] := 1.293586355; GammaY[73,38] := 1.329278412; GammaY[74,38] := 1.366318081; GammaY[75,38] := 1.404813562; GammaY[76,38] := 1.444886564; GammaY[77,38] := 1.486674545; GammaY[78,38] := 1.5303338; GammaY[79,38] := 1.576042926;
  //GammaY[80,38] := 1.624007472; GammaY[81,38] := 1.674465489; GammaY[82,38] := 1.727694669; GammaY[83,38] := 1.78402193; GammaY[84,38] := 1.843835651; GammaY[85,38] := 1.90760233; GammaY[86,38] := 1.975888907; GammaY[87,38] := 2.049393816; GammaY[88,38] := 2.128991434; GammaY[89,38] := 2.215796695;
  //GammaY[90,38] := 2.311262568; GammaY[91,38] := 2.417332918; GammaY[92,38] := 2.536692937; GammaY[93,38] := 2.673204083; GammaY[94,38] := 2.832716628; GammaY[95,38] := 3.024741376; GammaY[96,38] := 3.266391563; GammaY[97,38] := 3.593862711; GammaY[98,38] := 4.11202408; GammaY[99,38] := 5.303446881;
  //GammaY[0,39] := 0.009864915; GammaY[1,39] := 0.025738436; GammaY[2,39] := 0.040250593; GammaY[3,39] := 0.054067725; GammaY[4,39] := 0.067486164; GammaY[5,39] := 0.08064814; GammaY[6,39] := 0.093637666; GammaY[7,39] := 0.106510353; GammaY[8,39] := 0.119305858; GammaY[9,39] := 0.132054031;
  //GammaY[10,39] := 0.14477829; GammaY[11,39] := 0.157497633; GammaY[12,39] := 0.170227909; GammaY[13,39] := 0.182982654; GammaY[14,39] := 0.195773674; GammaY[15,39] := 0.208611453; GammaY[16,39] := 0.221505455; GammaY[17,39] := 0.234464347; GammaY[18,39] := 0.247496173; GammaY[19,39] := 0.260608489;
  //GammaY[20,39] := 0.273808464; GammaY[21,39] := 0.287102975; GammaY[22,39] := 0.300498667; GammaY[23,39] := 0.31400202; GammaY[24,39] := 0.327619399; GammaY[25,39] := 0.341357095; GammaY[26,39] := 0.355221361; GammaY[27,39] := 0.369218449; GammaY[28,39] := 0.383354637; GammaY[29,39] := 0.397636262;
  //GammaY[30,39] := 0.412069742; GammaY[31,39] := 0.426661605; GammaY[32,39] := 0.441418507; GammaY[33,39] := 0.456347263; GammaY[34,39] := 0.471454864; GammaY[35,39] := 0.486748497; GammaY[36,39] := 0.502235585; GammaY[37,39] := 0.517923802; GammaY[38,39] := 0.533821087; GammaY[39,39] := 0.549935684;
  //GammaY[40,39] := 0.566276165; GammaY[41,39] := 0.582851459; GammaY[42,39] := 0.599670884; GammaY[43,39] := 0.616744179; GammaY[44,39] := 0.634081538; GammaY[45,39] := 0.651693648; GammaY[46,39] := 0.669591734; GammaY[47,39] := 0.687787597; GammaY[48,39] := 0.706293654; GammaY[49,39] := 0.725123025;
  //GammaY[50,39] := 0.744289568; GammaY[51,39] := 0.763807922; GammaY[52,39] := 0.783693607; GammaY[53,39] := 0.803963088; GammaY[54,39] := 0.824633864; GammaY[55,39] := 0.845724558; GammaY[56,39] := 0.867255026; GammaY[57,39] := 0.88924647; GammaY[58,39] := 0.911721569; GammaY[59,39] := 0.934704626;
  //GammaY[60,39] := 0.958221703; GammaY[61,39] := 0.982300882; GammaY[62,39] := 1.006972293; GammaY[63,39] := 1.032268733; GammaY[64,39] := 1.058225684; GammaY[65,39] := 1.084881266; GammaY[66,39] := 1.112277254; GammaY[67,39] := 1.140459169; GammaY[68,39] := 1.169476617; GammaY[69,39] := 1.199384059;
  //GammaY[70,39] := 1.23024131; GammaY[71,39] := 1.262114473; GammaY[72,39] := 1.295076687; GammaY[73,39] := 1.329209049; GammaY[74,39] := 1.364602062; GammaY[75,39] := 1.401357003; GammaY[76,39] := 1.439587906; GammaY[77,39] := 1.479423632; GammaY[78,39] := 1.521010469; GammaY[79,39] := 1.564515658;
  //GammaY[80,39] := 1.61013147; GammaY[81,39] := 1.658080656; GammaY[82,39] := 1.708623101; GammaY[83,39] := 1.762064494; GammaY[84,39] := 1.818768024; GammaY[85,39] := 1.87916972; GammaY[86,39] := 1.943799342; GammaY[87,39] := 2.01330956; GammaY[88,39] := 2.088517155; GammaY[89,39] := 2.170463236;
  //GammaY[90,39] := 2.260504268; GammaY[91,39] := 2.360454341; GammaY[92,39] := 2.472818669; GammaY[93,39] := 2.601198676; GammaY[94,39] := 2.751048417; GammaY[95,39] := 2.931230628; GammaY[96,39] := 3.157684452; GammaY[97,39] := 3.464101102; GammaY[98,39] := 3.948009317; GammaY[99,39] := 5.057317355;
  //GammaY[0,40] := 0.013630508; GammaY[1,40] := 0.033153083; GammaY[2,40] := 0.050170776; GammaY[3,40] := 0.065925594; GammaY[4,40] := 0.080944629; GammaY[5,40] := 0.095473572; GammaY[6,40] := 0.109654412; GammaY[7,40] := 0.123579348; GammaY[8,40] := 0.137312959; GammaY[9,40] := 0.150902993;
  //GammaY[10,40] := 0.164386241; GammaY[11,40] := 0.177792001; GammaY[12,40] := 0.191144247; GammaY[13,40] := 0.204463052; GammaY[14,40] := 0.217765567; GammaY[15,40] := 0.231066701; GammaY[16,40] := 0.244379629; GammaY[17,40] := 0.257716153; GammaY[18,40] := 0.27108699; GammaY[19,40] := 0.284501988;
  //GammaY[20,40] := 0.297970292; GammaY[21,40] := 0.31150049; GammaY[22,40] := 0.32510072; GammaY[23,40] := 0.338778755; GammaY[24,40] := 0.352542089; GammaY[25,40] := 0.366397995; GammaY[26,40] := 0.380353584; GammaY[27,40] := 0.394415851; GammaY[28,40] := 0.40859172; GammaY[29,40] := 0.422888081;
  //GammaY[30,40] := 0.437311823; GammaY[31,40] := 0.451869869; GammaY[32,40] := 0.466569199; GammaY[33,40] := 0.481416892; GammaY[34,40] := 0.49642015; GammaY[35,40] := 0.511586308; GammaY[36,40] := 0.526922877; GammaY[37,40] := 0.542437568; GammaY[38,40] := 0.558138315; GammaY[39,40] := 0.574033306;
  //GammaY[40,40] := 0.59013101; GammaY[41,40] := 0.606440205; GammaY[42,40] := 0.62297001; GammaY[43,40] := 0.639729919; GammaY[44,40] := 0.656729823; GammaY[45,40] := 0.673980079; GammaY[46,40] := 0.691491526; GammaY[47,40] := 0.709275519; GammaY[48,40] := 0.72734399; GammaY[49,40] := 0.745709501;
  //GammaY[50,40] := 0.764385289; GammaY[51,40] := 0.783385333; GammaY[52,40] := 0.802724418; GammaY[53,40] := 0.822418206; GammaY[54,40] := 0.842483319; GammaY[55,40] := 0.862937423; GammaY[56,40] := 0.883799313; GammaY[57,40] := 0.90508907; GammaY[58,40] := 0.926828159; GammaY[59,40] := 0.949039524;
  //GammaY[60,40] := 0.971747792; GammaY[61,40] := 0.994979321; GammaY[62,40] := 1.018762744; GammaY[63,40] := 1.043128872; GammaY[64,40] := 1.068110932; GammaY[65,40] := 1.093745072; GammaY[66,40] := 1.120070431; GammaY[67,40] := 1.147129741; GammaY[68,40] := 1.17496963; GammaY[69,40] := 1.20364132;
  //GammaY[70,40] := 1.233201088; GammaY[71,40] := 1.263710848; GammaY[72,40] := 1.295239169; GammaY[73,40] := 1.327862189; GammaY[74,40] := 1.361664725; GammaY[75,40] := 1.396741831; GammaY[76,40] := 1.433200367; GammaY[77,40] := 1.471160968; GammaY[78,40] := 1.510760726; GammaY[79,40] := 1.552156216;
  //GammaY[80,40] := 1.59552756; GammaY[81,40] := 1.641083258; GammaY[82,40] := 1.689066414; GammaY[83,40] := 1.739763093; GammaY[84,40] := 1.79351302; GammaY[85,40] := 1.850723915; GammaY[86,40] := 1.911891152; GammaY[87,40] := 1.977624779; GammaY[88,40] := 2.048687962; GammaY[89,40] := 2.126053354;
  //GammaY[90,40] := 2.210987816; GammaY[91,40] := 2.30518509; GammaY[92,40] := 2.410983432; GammaY[93,40] := 2.531743075; GammaY[94,40] := 2.672550855; GammaY[95,40] := 2.841669179; GammaY[96,40] := 3.053950012; GammaY[97,40] := 3.340767044; GammaY[98,40] := 3.792861905; GammaY[99,40] := 4.826195143;
  //GammaY[0,41] := 0.018340434; GammaY[1,41] := 0.041805697; GammaY[2,41] := 0.061384679; GammaY[3,41] := 0.07905526; GammaY[4,41] := 0.095620803; GammaY[5,41] := 0.111446596; GammaY[6,41] := 0.126740072; GammaY[7,41] := 0.141633999; GammaY[8,41] := 0.156220176; GammaY[9,41] := 0.170565665;
  //GammaY[10,41] := 0.184721533; GammaY[11,41] := 0.198727971; GammaY[12,41] := 0.212617475; GammaY[13,41] := 0.226416925; GammaY[14,41] := 0.240148996; GammaY[15,41] := 0.25383315; GammaY[16,41] := 0.267486353; GammaY[17,41] := 0.281123594; GammaY[18,41] := 0.294758298; GammaY[19,41] := 0.308402625;
  //GammaY[20,41] := 0.322067707; GammaY[21,41] := 0.335763843; GammaY[22,41] := 0.349500653; GammaY[23,41] := 0.363287199; GammaY[24,41] := 0.377132094; GammaY[25,41] := 0.391043585; GammaY[26,41] := 0.405029628; GammaY[27,41] := 0.419097953; GammaY[28,41] := 0.433256111; GammaY[29,41] := 0.447511538;
  //GammaY[30,41] := 0.461871592; GammaY[31,41] := 0.476343579; GammaY[32,41] := 0.490934805; GammaY[33,41] := 0.505652607; GammaY[34,41] := 0.520504379; GammaY[35,41] := 0.535497605; GammaY[36,41] := 0.550639893; GammaY[37,41] := 0.565938996; GammaY[38,41] := 0.581402846; GammaY[39,41] := 0.59703958;
  //GammaY[40,41] := 0.612857572; GammaY[41,41] := 0.628865449; GammaY[42,41] := 0.645072157; GammaY[43,41] := 0.661486968; GammaY[44,41] := 0.678119502; GammaY[45,41] := 0.694979788; GammaY[46,41] := 0.712078292; GammaY[47,41] := 0.72942596; GammaY[48,41] := 0.747034262; GammaY[49,41] := 0.764915241;
  //GammaY[50,41] := 0.783081566; GammaY[51,41] := 0.801546589; GammaY[52,41] := 0.820324403; GammaY[53,41] := 0.839429901; GammaY[54,41] := 0.858878893; GammaY[55,41] := 0.878688171; GammaY[56,41] := 0.898875557; GammaY[57,41] := 0.919460055; GammaY[58,41] := 0.940461958; GammaY[59,41] := 0.961902968;
  //GammaY[60,41] := 0.983806346; GammaY[61,41] := 1.006196946; GammaY[62,41] := 1.029101791; GammaY[63,41] := 1.052550001; GammaY[64,41] := 1.076572784; GammaY[65,41] := 1.101204049; GammaY[66,41] := 1.126480763; GammaY[67,41] := 1.15244317; GammaY[68,41] := 1.179135092; GammaY[69,41] := 1.206604596;
  //GammaY[70,41] := 1.234904492; GammaY[71,41] := 1.264092932; GammaY[72,41] := 1.294234337; GammaY[73,41] := 1.325400201; GammaY[74,41] := 1.357670081; GammaY[75,41] := 1.391133067; GammaY[76,41] := 1.425889309; GammaY[77,41] := 1.462051891; GammaY[78,41] := 1.499749333; GammaY[79,41] := 1.539128413;
  //GammaY[80,41] := 1.580357739; GammaY[81,41] := 1.623632511; GammaY[82,41] := 1.669180379; GammaY[83,41] := 1.717269057; GammaY[84,41] := 1.768216494; GammaY[85,41] := 1.822404204; GammaY[86,41] := 1.880295427; GammaY[87,41] := 1.942460576; GammaY[88,41] := 2.009613135; GammaY[89,41] := 2.082662141;
  //GammaY[90,41] := 2.162791236; GammaY[91,41] := 2.25158252; GammaY[92,41] := 2.351219339; GammaY[93,41] := 2.464838181; GammaY[94,41] := 2.59718547; GammaY[95,41] := 2.755967315; GammaY[96,41] := 2.955028743; GammaY[97,41] := 3.223598312; GammaY[98,41] := 3.646140641; GammaY[99,41] := 4.609155241;
  //GammaY[0,42] := 0.024086186; GammaY[1,42] := 0.051710081; GammaY[2,42] := 0.073853976; GammaY[3,42] := 0.09338359; GammaY[4,42] := 0.111417213; GammaY[5,42] := 0.128452366; GammaY[6,42] := 0.144767374; GammaY[7,42] := 0.160538266; GammaY[8,42] := 0.175885511; GammaY[9,42] := 0.19089629;
  //GammaY[10,42] := 0.205636405; GammaY[11,42] := 0.2201572; GammaY[12,42] := 0.234499835; GammaY[13,42] := 0.248698065; GammaY[14,42] := 0.262780112; GammaY[15,42] := 0.276769985; GammaY[16,42] := 0.290688418; GammaY[17,42] := 0.304553559; GammaY[18,42] := 0.318381497; GammaY[19,42] := 0.332186658;
  //GammaY[20,42] := 0.345982117; GammaY[21,42] := 0.359779842; GammaY[22,42] := 0.373590894; GammaY[23,42] := 0.387425584; GammaY[24,42] := 0.401293604; GammaY[25,42] := 0.415204134; GammaY[26,42] := 0.429165947; GammaY[27,42] := 0.443187479; GammaY[28,42] := 0.457276886; GammaY[29,42] := 0.47144212;
  //GammaY[30,42] := 0.485690973; GammaY[31,42] := 0.500031124; GammaY[32,42] := 0.514470183; GammaY[33,42] := 0.529015729; GammaY[34,42] := 0.543675345; GammaY[35,42] := 0.558456652; GammaY[36,42] := 0.573367342; GammaY[37,42] := 0.5884152; GammaY[38,42] := 0.603608159; GammaY[39,42] := 0.618954315;
  //GammaY[40,42] := 0.634461942; GammaY[41,42] := 0.650139544; GammaY[42,42] := 0.665995882; GammaY[43,42] := 0.68204; GammaY[44,42] := 0.698281269; GammaY[45,42] := 0.714729412; GammaY[46,42] := 0.731394549; GammaY[47,42] := 0.748287235; GammaY[48,42] := 0.765418502; GammaY[49,42] := 0.782799892;
  //GammaY[50,42] := 0.800443545; GammaY[51,42] := 0.818362237; GammaY[52,42] := 0.836569403; GammaY[53,42] := 0.855079244; GammaY[54,42] := 0.873906785; GammaY[55,42] := 0.893067957; GammaY[56,42] := 0.912579682; GammaY[57,42] := 0.932459971; GammaY[58,42] := 0.95272803; GammaY[59,42] := 0.973404387;
  //GammaY[60,42] := 0.994510931; GammaY[61,42] := 1.016071288; GammaY[62,42] := 1.038110971; GammaY[63,42] := 1.060657283; GammaY[64,42] := 1.0837397; GammaY[65,42] := 1.107390216; GammaY[66,42] := 1.131643563; GammaY[67,42] := 1.156537505; GammaY[68,42] := 1.182113374; GammaY[69,42] := 1.208416403;
  //GammaY[70,42] := 1.235496162; GammaY[71,42] := 1.263407322; GammaY[72,42] := 1.292210327; GammaY[73,42] := 1.321972248; GammaY[74,42] := 1.352767761; GammaY[75,42] := 1.384680498; GammaY[76,42] := 1.417804432; GammaY[77,42] := 1.452245599; GammaY[78,42] := 1.488124436; GammaY[79,42] := 1.525578472;
  //GammaY[80,42] := 1.564765744; GammaY[81,42] := 1.605869083; GammaY[82,42] := 1.649101743; GammaY[83,42] := 1.694714466; GammaY[84,42] := 1.743004822; GammaY[85,42] := 1.794329844; GammaY[86,42] := 1.849123034; GammaY[87,42] := 1.907917997; GammaY[88,42] := 1.971381987; GammaY[89,42] := 2.040364974;
  //GammaY[90,42] := 2.115973389; GammaY[91,42] := 2.199685576; GammaY[92,42] := 2.293541108; GammaY[93,42] := 2.400468991; GammaY[94,42] := 2.52489993; GammaY[95,42] := 2.674024432; GammaY[96,42] := 2.86075467; GammaY[97,42] := 3.112333075; GammaY[98,42] := 3.507417482; GammaY[99,42] := 4.405323476;
  //GammaY[0,43] := 0.030938488; GammaY[1,43] := 0.062853059; GammaY[2,43] := 0.087517194; GammaY[3,43] := 0.108819521; GammaY[4,43] := 0.128223461; GammaY[5,43] := 0.146367604; GammaY[6,43] := 0.163604564; GammaY[7,43] := 0.180155086; GammaY[8,43] := 0.196168942; GammaY[9,43] := 0.211753684;
  //GammaY[10,43] := 0.226989919; GammaY[11,43] := 0.24194011; GammaY[12,43] := 0.256654006; GammaY[13,43] := 0.271172134; GammaY[14,43] := 0.285528158; GammaY[15,43] := 0.29975052; GammaY[16,43] := 0.31386361; GammaY[17,43] := 0.327888631; GammaY[18,43] := 0.34184424; GammaY[19,43] := 0.35574704;
  //GammaY[20,43] := 0.369611963; GammaY[21,43] := 0.383452568; GammaY[22,43] := 0.397281285; GammaY[23,43] := 0.411109609; GammaY[24,43] := 0.424948254; GammaY[25,43] := 0.438807285; GammaY[26,43] := 0.452696236; GammaY[27,43] := 0.466624193; GammaY[28,43] := 0.480599884; GammaY[29,43] := 0.494631739;
  //GammaY[30,43] := 0.508727959; GammaY[31,43] := 0.522896561; GammaY[32,43] := 0.537145435; GammaY[33,43] := 0.551482372; GammaY[34,43] := 0.565915131; GammaY[35,43] := 0.580451459; GammaY[36,43] := 0.595099117; GammaY[37,43] := 0.609865927; GammaY[38,43] := 0.624759803; GammaY[39,43] := 0.639788779;
  //GammaY[40,43] := 0.654961043; GammaY[41,43] := 0.670284966; GammaY[42,43] := 0.685769135; GammaY[43,43] := 0.701422386; GammaY[44,43] := 0.717253838; GammaY[45,43] := 0.733272913; GammaY[46,43] := 0.749489408; GammaY[47,43] := 0.765913518; GammaY[48,43] := 0.782555849; GammaY[49,43] := 0.799427495;
  //GammaY[50,43] := 0.816540078; GammaY[51,43] := 0.8339058; GammaY[52,43] := 0.851537496; GammaY[53,43] := 0.8694487; GammaY[54,43] := 0.88765371; GammaY[55,43] := 0.906167666; GammaY[56,43] := 0.925006608; GammaY[57,43] := 0.944187626; GammaY[58,43] := 0.963728931; GammaY[59,43] := 0.98364993;
  //GammaY[60,43] := 1.003971344; GammaY[61,43] := 1.024715517; GammaY[62,43] := 1.045906404; GammaY[63,43] := 1.067569783; GammaY[64,43] := 1.089733548; GammaY[65,43] := 1.112427788; GammaY[66,43] := 1.135685216; GammaY[67,43] := 1.15954142; GammaY[68,43] := 1.184035234; GammaY[69,43] := 1.209209119;
  //GammaY[70,43] := 1.235109767; GammaY[71,43] := 1.261788595; GammaY[72,43] := 1.289302342; GammaY[73,43] := 1.317713995; GammaY[74,43] := 1.347093704; GammaY[75,43] := 1.37751992; GammaY[76,43] := 1.409080752; GammaY[77,43] := 1.441875754; GammaY[78,43] := 1.476017914; GammaY[79,43] := 1.511636142;
  //GammaY[80,43] := 1.548878582; GammaY[81,43] := 1.587916613; GammaY[82,43] := 1.62895001; GammaY[83,43] := 1.672213631; GammaY[84,43] := 1.717986169; GammaY[85,43] := 1.766601954; GammaY[86,43] := 1.818466816; GammaY[87,43] := 1.874080107; GammaY[88,43] := 1.934066161; GammaY[89,43] := 1.999220006;
  //GammaY[90,43] := 2.070576288; GammaY[91,43] := 2.149517089; GammaY[92,43] := 2.237948525; GammaY[93,43] := 2.338607065; GammaY[94,43] := 2.455630605; GammaY[95,43] := 2.595731529; GammaY[96,43] := 2.770957537; GammaY[97,43] := 3.006711712; GammaY[98,43] := 3.376278561; GammaY[99,43] := 4.213874156;
  //GammaY[0,44] := 0.038944543; GammaY[1,44] := 0.075197159; GammaY[2,44] := 0.102294289; GammaY[3,44] := 0.12525912; GammaY[4,44] := 0.145921212; GammaY[5,44] := 0.165065274; GammaY[6,44] := 0.183119649; GammaY[7,44] := 0.200350115; GammaY[8,44] := 0.216935663; GammaY[9,44] := 0.233003994;
  //GammaY[10,44] := 0.248650243; GammaY[11,44] := 0.263947729; GammaY[12,44] := 0.278954526; GammaY[13,44] := 0.293717695; GammaY[14,44] := 0.308276125; GammaY[15,44] := 0.322662495; GammaY[16,44] := 0.336904683; GammaY[17,44] := 0.351026785; GammaY[18,44] := 0.365049882; GammaY[19,44] := 0.378992632;
  //GammaY[20,44] := 0.392871711; GammaY[21,44] := 0.406702168; GammaY[22,44] := 0.420497708; GammaY[23,44] := 0.434270924; GammaY[24,44] := 0.448033475; GammaY[25,44] := 0.461796245; GammaY[26,44] := 0.475569465; GammaY[27,44] := 0.489362828; GammaY[28,44] := 0.503185575; GammaY[29,44] := 0.517046568;
  //GammaY[30,44] := 0.530954379; GammaY[31,44] := 0.544917337; GammaY[32,44] := 0.558943573; GammaY[33,44] := 0.573041082; GammaY[34,44] := 0.587217762; GammaY[35,44] := 0.601481453; GammaY[36,44] := 0.615839979; GammaY[37,44] := 0.630301179; GammaY[38,44] := 0.644872943; GammaY[39,44] := 0.659563245;
  //GammaY[40,44] := 0.674380174; GammaY[41,44] := 0.689331957; GammaY[42,44] := 0.704427018; GammaY[43,44] := 0.719673998; GammaY[44,44] := 0.735081763; GammaY[45,44] := 0.750659466; GammaY[46,44] := 0.76641658; GammaY[47,44] := 0.78236293; GammaY[48,44] := 0.798508733; GammaY[49,44] := 0.814864646;
  //GammaY[50,44] := 0.831441809; GammaY[51,44] := 0.848251892; GammaY[52,44] := 0.865307153; GammaY[53,44] := 0.882620475; GammaY[54,44] := 0.900205485; GammaY[55,44] := 0.918076598; GammaY[56,44] := 0.936249046; GammaY[57,44] := 0.954739025; GammaY[58,44] := 0.973563772; GammaY[59,44] := 0.992741621;
  //GammaY[60,44] := 1.012292233; GammaY[61,44] := 1.032236762; GammaY[62,44] := 1.052597812; GammaY[63,44] := 1.073399671; GammaY[64,44] := 1.094668604; GammaY[65,44] := 1.116433024; GammaY[66,44] := 1.138723746; GammaY[67,44] := 1.161574283; GammaY[68,44] := 1.185021188; GammaY[69,44] := 1.209104408;
  //GammaY[70,44] := 1.233867857; GammaY[71,44] := 1.259359869; GammaY[72,44] := 1.285633772; GammaY[73,44] := 1.312748744; GammaY[74,44] := 1.340770671; GammaY[75,44] := 1.369773217; GammaY[76,44] := 1.399839125; GammaY[77,44] := 1.431061786; GammaY[78,44] := 1.46354715; GammaY[79,44] := 1.497416207;
  //GammaY[80,44] := 1.532807913; GammaY[81,44] := 1.569882916; GammaY[82,44] := 1.608828479; GammaY[83,44] := 1.6498647; GammaY[84,44] := 1.693252721; GammaY[85,44] := 1.739305689; GammaY[86,44] := 1.788403637; GammaY[87,44] := 1.841014133; GammaY[88,44] := 1.897721563; GammaY[89,44] := 1.959269943;
  //GammaY[90,44] := 2.026627256; GammaY[91,44] := 2.10108612; GammaY[92,44] := 2.184428764; GammaY[93,44] := 2.279212898; GammaY[94,44] := 2.389304696; GammaY[95,44] := 2.520973162; GammaY[96,44] := 2.685464686; GammaY[97,44] := 2.906478393; GammaY[98,44] := 3.252325002; GammaY[99,44] := 4.034029648;
  //GammaY[0,45] := 0.048127012; GammaY[1,45] := 0.088684206; GammaY[2,45] := 0.11809132; GammaY[3,45] := 0.142590275; GammaY[4,45] := 0.164388546; GammaY[5,45] := 0.184418448; GammaY[6,45] := 0.203183741; GammaY[7,45] := 0.220994563; GammaY[8,45] := 0.238058431; GammaY[9,45] := 0.25452257;
  //GammaY[10,45] := 0.270496104; GammaY[11,45] := 0.286062732; GammaY[12,45] := 0.301288457; GammaY[13,45] := 0.316226533; GammaY[14,45] := 0.330920775; GammaY[15,45] := 0.345407842; GammaY[16,45] := 0.359718874; GammaY[17,45] := 0.373880669; GammaY[18,45] := 0.387916565; GammaY[19,45] := 0.401847114;
  //GammaY[20,45] := 0.415690601; GammaY[21,45] := 0.429463444; GammaY[22,45] := 0.443180523; GammaY[23,45] := 0.456855429; GammaY[24,45] := 0.470500686; GammaY[25,45] := 0.48412791; GammaY[26,45] := 0.497747972; GammaY[27,45] := 0.51137111; GammaY[28,45] := 0.52500702; GammaY[29,45] := 0.538664962;
  //GammaY[30,45] := 0.552353826; GammaY[31,45] := 0.566082203; GammaY[32,45] := 0.579858442; GammaY[33,45] := 0.593690701; GammaY[34,45] := 0.607587; GammaY[35,45] := 0.621555256; GammaY[36,45] := 0.635603333; GammaY[37,45] := 0.64973906; GammaY[38,45] := 0.663970299; GammaY[39,45] := 0.678304964;
  //GammaY[40,45] := 0.692751032; GammaY[41,45] := 0.707316603; GammaY[42,45] := 0.722009922; GammaY[43,45] := 0.736839415; GammaY[44,45] := 0.751813719; GammaY[45,45] := 0.766941715; GammaY[46,45] := 0.78223257; GammaY[47,45] := 0.797695764; GammaY[48,45] := 0.813341137; GammaY[49,45] := 0.829178907;
  //GammaY[50,45] := 0.845219769; GammaY[51,45] := 0.861474906; GammaY[52,45] := 0.877956014; GammaY[53,45] := 0.894675401; GammaY[54,45] := 0.911646029; GammaY[55,45] := 0.928881588; GammaY[56,45] := 0.946396565; GammaY[57,45] := 0.964206327; GammaY[58,45] := 0.982327213; GammaY[59,45] := 1.00077656;
  //GammaY[60,45] := 1.019573034; GammaY[61,45] := 1.038736613; GammaY[62,45] := 1.058288586; GammaY[63,45] := 1.078251932; GammaY[64,45] := 1.098651432; GammaY[65,45] := 1.119513878; GammaY[66,45] := 1.140868313; GammaY[67,45] := 1.162746308; GammaY[68,45] := 1.185182278; GammaY[69,45] := 1.208213857;
  //GammaY[70,45] := 1.231882298; GammaY[71,45] := 1.256233059; GammaY[72,45] := 1.281316337; GammaY[73,45] := 1.307187726; GammaY[74,45] := 1.333909135; GammaY[75,45] := 1.361549761; GammaY[76,45] := 1.390187307; GammaY[77,45] := 1.419909455; GammaY[78,45] := 1.450815679; GammaY[79,45] := 1.483019487;
  //GammaY[80,45] := 1.51665122; GammaY[81,45] := 1.551861594; GammaY[82,45] := 1.588826216; GammaY[83,45] := 1.62775142; GammaY[84,45] := 1.668882009; GammaY[85,45] := 1.712511458; GammaY[86,45] := 1.758995804; GammaY[87,45] := 1.808773015; GammaY[88,45] := 1.862390363; GammaY[89,45] := 1.920544396;
  //GammaY[90,45] := 1.984141105; GammaY[91,45] := 2.054390028; GammaY[92,45] := 2.132958305; GammaY[93,45] := 2.222237689; GammaY[94,45] := 2.325842174; GammaY[95,45] := 2.449629421; GammaY[96,45] := 2.604102781; GammaY[97,45] := 2.811382297; GammaY[98,45] := 3.135173367; GammaY[99,45] := 3.865058185;
  //GammaY[0,46] := 0.058484541; GammaY[1,46] := 0.103239351; GammaY[2,46] := 0.134804892; GammaY[3,46] := 0.160696809; GammaY[4,46] := 0.183503546; GammaY[5,46] := 0.204303346; GammaY[6,46] := 0.223673565; GammaY[7,46] := 0.241967203; GammaY[8,46] := 0.259419116; GammaY[9,46] := 0.276195105;
  //GammaY[10,46] := 0.292417527; GammaY[11,46] := 0.308179851; GammaY[12,46] := 0.323555499; GammaY[13,46] := 0.338603496; GammaY[14,46] := 0.353372232; GammaY[15,46] := 0.367902051; GammaY[16,46] := 0.382227091; GammaY[17,46] := 0.396376627; GammaY[18,46] := 0.410376059; GammaY[19,46] := 0.424247671;
  //GammaY[20,46] := 0.438011209; GammaY[21,46] := 0.45168433; GammaY[22,46] := 0.465282963; GammaY[23,46] := 0.478821607; GammaY[24,46] := 0.492313559; GammaY[25,46] := 0.505771096; GammaY[26,46] := 0.519205648; GammaY[27,46] := 0.532627923; GammaY[28,46] := 0.546048029; GammaY[29,46] := 0.55947556;
  //GammaY[30,46] := 0.572919688; GammaY[31,46] := 0.586389228; GammaY[32,46] := 0.599892709; GammaY[33,46] := 0.613438414; GammaY[34,46] := 0.627034462; GammaY[35,46] := 0.640688834; GammaY[36,46] := 0.654409404; GammaY[37,46] := 0.668203998; GammaY[38,46] := 0.68208042; GammaY[39,46] := 0.696046495;
  //GammaY[40,46] := 0.710110098; GammaY[41,46] := 0.724279184; GammaY[42,46] := 0.738561829; GammaY[43,46] := 0.752966254; GammaY[44,46] := 0.767500862; GammaY[45,46] := 0.782174255; GammaY[46,46] := 0.796995313; GammaY[47,46] := 0.8119732; GammaY[48,46] := 0.82711738; GammaY[49,46] := 0.842437687;
  //GammaY[50,46] := 0.857944362; GammaY[51,46] := 0.873648096; GammaY[52,46] := 0.889560077; GammaY[53,46] := 0.905692046; GammaY[54,46] := 0.922056355; GammaY[55,46] := 0.938666026; GammaY[56,46] := 0.955534803; GammaY[57,46] := 0.972677291; GammaY[58,46] := 0.990109006; GammaY[59,46] := 1.00784646;
  //GammaY[60,46] := 1.025907206; GammaY[61,46] := 1.044310036; GammaY[62,46] := 1.063075183; GammaY[63,46] := 1.082224352; GammaY[64,46] := 1.101780931; GammaY[65,46] := 1.121770194; GammaY[66,46] := 1.142219526; GammaY[67,46] := 1.163158676; GammaY[68,46] := 1.184620066; GammaY[69,46] := 1.206639154;
  //GammaY[70,46] := 1.229254765; GammaY[71,46] := 1.252509618; GammaY[72,46] := 1.276450934; GammaY[73,46] := 1.301131014; GammaY[74,46] := 1.326608051; GammaY[75,46] := 1.352947074; GammaY[76,46] := 1.380221083; GammaY[77,46] := 1.408512434; GammaY[78,46] := 1.437914537; GammaY[79,46] := 1.468533952;
  //GammaY[80,46] := 1.500493017; GammaY[81,46] := 1.533933176; GammaY[82,46] := 1.569019158; GammaY[83,46] := 1.605944496; GammaY[84,46] := 1.644938755; GammaY[85,46] := 1.686277063; GammaY[86,46] := 1.730293164; GammaY[87,46] := 1.77739748; GammaY[88,46] := 1.828102743; GammaY[89,46] := 1.883061373;
  //GammaY[90,46] := 1.943121744; GammaY[91,46] := 2.009416153; GammaY[92,46] := 2.083504819; GammaY[93,46] := 2.167625341; GammaY[94,46] := 2.265157567; GammaY[95,46] := 2.381577439; GammaY[96,46] := 2.526699074; GammaY[97,46] := 2.721178697; GammaY[98,46] := 3.024456092; GammaY[99,46] := 3.706272124;
  //GammaY[0,47] := 0.069993552; GammaY[1,47] := 0.118775177; GammaY[2,47] := 0.152326143; GammaY[3,47] := 0.179461918; GammaY[4,47] := 0.203147145; GammaY[5,47] := 0.224601617; GammaY[6,47] := 0.244473229; GammaY[7,47] := 0.26315568; GammaY[8,47] := 0.280909592; GammaY[9,47] := 0.297918162;
  //GammaY[10,47] := 0.314316062; GammaY[11,47] := 0.330205811; GammaY[12,47] := 0.345667676; GammaY[13,47] := 0.360765978; GammaY[14,47] := 0.375553284; GammaY[15,47] := 0.39007329; GammaY[16,47] := 0.404362859; GammaY[17,47] := 0.418453505; GammaY[18,47] := 0.432372483; GammaY[19,47] := 0.446143631;
  //GammaY[20,47] := 0.459788003; GammaY[21,47] := 0.473324359; GammaY[22,47] := 0.48676957; GammaY[23,47] := 0.500138928; GammaY[24,47] := 0.513446402; GammaY[25,47] := 0.526704849; GammaY[26,47] := 0.539926188; GammaY[27,47] := 0.553121541; GammaY[28,47] := 0.56630136; GammaY[29,47] := 0.57947552;
  //GammaY[30,47] := 0.59265343; GammaY[31,47] := 0.6058441; GammaY[32,47] := 0.619056194; GammaY[33,47] := 0.632298109; GammaY[34,47] := 0.64557802; GammaY[35,47] := 0.658903935; GammaY[36,47] := 0.672283731; GammaY[37,47] := 0.685725201; GammaY[38,47] := 0.699236091; GammaY[39,47] := 0.712824134;
  //GammaY[40,47] := 0.726497072; GammaY[41,47] := 0.740262726; GammaY[42,47] := 0.754129006; GammaY[43,47] := 0.76810392; GammaY[44,47] := 0.782195643; GammaY[45,47] := 0.796412531; GammaY[46,47] := 0.810763163; GammaY[47,47] := 0.825256371; GammaY[48,47] := 0.839901277; GammaY[49,47] := 0.854707333;
  //GammaY[50,47] := 0.869684361; GammaY[51,47] := 0.884842595; GammaY[52,47] := 0.900192707; GammaY[53,47] := 0.915745919; GammaY[54,47] := 0.93151402; GammaY[55,47] := 0.94750939; GammaY[56,47] := 0.963745113; GammaY[57,47] := 0.980235041; GammaY[58,47] := 0.996993872; GammaY[59,47] := 1.014037256;
  //GammaY[60,47] := 1.031381823; GammaY[61,47] := 1.049045367; GammaY[62,47] := 1.067047007; GammaY[63,47] := 1.08540725; GammaY[64,47] := 1.10414818; GammaY[65,47] := 1.12329365; GammaY[66,47] := 1.142869486; GammaY[67,47] := 1.16290374; GammaY[68,47] := 1.183426961; GammaY[69,47] := 1.204472527;
  //GammaY[70,47] := 1.226077045; GammaY[71,47] := 1.248280725; GammaY[72,47] := 1.271127948; GammaY[73,47] := 1.29466794; GammaY[74,47] := 1.318955425; GammaY[75,47] := 1.344051539; GammaY[76,47] := 1.370024894; GammaY[77,47] := 1.396952876; GammaY[78,47] := 1.424923236; GammaY[79,47] := 1.454036056;
  //GammaY[80,47] := 1.48440621; GammaY[81,47] := 1.516166466; GammaY[82,47] := 1.54947147; GammaY[83,47] := 1.584502826; GammaY[84,47] := 1.621475866; GammaY[85,47] := 1.660648668; GammaY[86,47] := 1.702334201; GammaY[87,47] := 1.746917248; GammaY[88,47] := 1.794878397; GammaY[89,47] := 1.846829018;
  //GammaY[90,47] := 1.903563896; GammaY[91,47] := 1.966143513; GammaY[92,47] := 2.036028692; GammaY[93,47] := 2.115313839; GammaY[94,47] := 2.207161341; GammaY[95,47] := 2.316692725; GammaY[96,47] := 2.453082596; GammaY[97,47] := 2.635629737; GammaY[98,47] := 2.919821556; GammaY[99,47] := 3.55703228;
  //GammaY[0,48] := 0.082610905; GammaY[1,48] := 0.135195588; GammaY[2,48] := 0.170544146; GammaY[3,48] := 0.198770917; GammaY[4,48] := 0.223205263; GammaY[5,48] := 0.245201941; GammaY[6,48] := 0.265475351; GammaY[7,48] := 0.284457237; GammaY[8,48] := 0.302432127; GammaY[9,48] := 0.31959926;
  //GammaY[10,48] := 0.336104611; GammaY[11,48] := 0.352058951; GammaY[12,48] := 0.367548759; GammaY[13,48] := 0.382643148; GammaY[14,48] := 0.397398449; GammaY[15,48] := 0.411861363; GammaY[16,48] := 0.426071189; GammaY[17,48] := 0.440061436; GammaY[18,48] := 0.453861008; GammaY[19,48] := 0.467495113;
  //GammaY[20,48] := 0.480985951; GammaY[21,48] := 0.494353253; GammaY[22,48] := 0.507614704; GammaY[23,48] := 0.520786289; GammaY[24,48] := 0.533882563; GammaY[25,48] := 0.546916871; GammaY[26,48] := 0.55990155; GammaY[27,48] := 0.57284808; GammaY[28,48] := 0.585767194; GammaY[29,48] := 0.598669012;
  //GammaY[30,48] := 0.611563125; GammaY[31,48] := 0.624458685; GammaY[32,48] := 0.637364467; GammaY[33,48] := 0.65028894; GammaY[34,48] := 0.663240321; GammaY[35,48] := 0.676226625; GammaY[36,48] := 0.689255698; GammaY[37,48] := 0.702335294; GammaY[38,48] := 0.715473092; GammaY[39,48] := 0.728676716;
  //GammaY[40,48] := 0.741953794; GammaY[41,48] := 0.755311985; GammaY[42,48] := 0.76875901; GammaY[43,48] := 0.782302688; GammaY[44,48] := 0.795950961; GammaY[45,48] := 0.809711935; GammaY[46,48] := 0.823593905; GammaY[47,48] := 0.837605379; GammaY[48,48] := 0.851755153; GammaY[49,48] := 0.866052324;
  //GammaY[50,48] := 0.880506301; GammaY[51,48] := 0.895126882; GammaY[52,48] := 0.909924291; GammaY[53,48] := 0.924909222; GammaY[54,48] := 0.940092895; GammaY[55,48] := 0.955487109; GammaY[56,48] := 0.971104307; GammaY[57,48] := 0.986957646; GammaY[58,48] := 1.003061066; GammaY[59,48] := 1.019429378;
  //GammaY[60,48] := 1.03607837; GammaY[61,48] := 1.053024909; GammaY[62,48] := 1.070287048; GammaY[63,48] := 1.087884167; GammaY[64,48] := 1.105837129; GammaY[65,48] := 1.124168451; GammaY[66,48] := 1.142902505; GammaY[67,48] := 1.162065766; GammaY[68,48] := 1.181687011; GammaY[69,48] := 1.201797675;
  //GammaY[70,48] := 1.222432243; GammaY[71,48] := 1.2436286; GammaY[72,48] := 1.26542855; GammaY[73,48] := 1.287878389; GammaY[74,48] := 1.311029602; GammaY[75,48] := 1.334939689; GammaY[76,48] := 1.359673165; GammaY[77,48] := 1.385302777; GammaY[78,48] := 1.411910994; GammaY[79,48] := 1.439591855;
  //GammaY[80,48] := 1.468453286; GammaY[81,48] := 1.498619965; GammaY[82,48] := 1.530237072; GammaY[83,48] := 1.563475129; GammaY[84,48] := 1.598536256; GammaY[85,48] := 1.635662612; GammaY[86,48] := 1.675147812; GammaY[87,48] := 1.717352768; GammaY[88,48] := 1.762728147; GammaY[89,48] := 1.811847112;
  //GammaY[90,48] := 1.865454594; GammaY[91,48] := 1.924544281; GammaY[92,48] := 1.990484596; GammaY[93,48] := 2.06523681; GammaY[94,48] := 2.15176128; GammaY[95,48] := 2.254850423; GammaY[96,48] := 2.383085204; GammaY[97,48] := 2.554505143; GammaY[98,48] := 2.820934186; GammaY[99,48] := 3.416733007;
  //GammaY[0,49] := 0.096277083; GammaY[1,49] := 0.152399311; GammaY[2,49] := 0.189348699; GammaY[3,49] := 0.218513343; GammaY[4,49] := 0.243570319; GammaY[5,49] := 0.266001048; GammaY[6,49] := 0.286581683; GammaY[7,49] := 0.305778997; GammaY[8,49] := 0.323899381; GammaY[9,49] := 0.34115665;
  //GammaY[10,49] := 0.357706983; GammaY[11,49] := 0.373668578; GammaY[12,49] := 0.389133473; GammaY[13,49] := 0.404175049; GammaY[14,49] := 0.418852981; GammaY[15,49] := 0.433216626; GammaY[16,49] := 0.447307424; GammaY[17,49] := 0.461160629; GammaY[18,49] := 0.474806583; GammaY[19,49] := 0.488271687;
  //GammaY[20,49] := 0.501579133; GammaY[21,49] := 0.51474948; GammaY[22,49] := 0.527801119; GammaY[23,49] := 0.540750625; GammaY[24,49] := 0.553613045; GammaY[25,49] := 0.566402142; GammaY[26,49] := 0.579130599; GammaY[27,49] := 0.591810171; GammaY[28,49] := 0.604451832; GammaY[29,49] := 0.617065889;
  //GammaY[30,49] := 0.629662079; GammaY[31,49] := 0.642249661; GammaY[32,49] := 0.654837476; GammaY[33,49] := 0.667434044; GammaY[34,49] := 0.680047604; GammaY[35,49] := 0.692686148; GammaY[36,49] := 0.705357496; GammaY[37,49] := 0.71806933; GammaY[38,49] := 0.730829229; GammaY[39,49] := 0.743644717;
  //GammaY[40,49] := 0.756523286; GammaY[41,49] := 0.76947244; GammaY[42,49] := 0.78249972; GammaY[43,49] := 0.795612728; GammaY[44,49] := 0.808819194; GammaY[45,49] := 0.822126984; GammaY[46,49] := 0.835544111; GammaY[47,49] := 0.849078796; GammaY[48,49] := 0.862739497; GammaY[49,49] := 0.876534943;
  //GammaY[50,49] := 0.890474169; GammaY[51,49] := 0.90456656; GammaY[52,49] := 0.91882189; GammaY[53,49] := 0.933250369; GammaY[54,49] := 0.947862671; GammaY[55,49] := 0.962670049; GammaY[56,49] := 0.977684362; GammaY[57,49] := 0.992918091; GammaY[58,49] := 1.008384489; GammaY[59,49] := 1.024097582;
  //GammaY[60,49] := 1.04007231; GammaY[61,49] := 1.05632466; GammaY[62,49] := 1.072871714; GammaY[63,49] := 1.089731796; GammaY[64,49] := 1.106924619; GammaY[65,49] := 1.124471452; GammaY[66,49] := 1.142395298; GammaY[67,49] := 1.160721118; GammaY[68,49] := 1.179476074; GammaY[69,49] := 1.198689819;
  //GammaY[70,49] := 1.218394828; GammaY[71,49] := 1.238626791; GammaY[72,49] := 1.259425076; GammaY[73,49] := 1.280833283; GammaY[74,49] := 1.302899851; GammaY[75,49] := 1.32567887; GammaY[76,49] := 1.349231044; GammaY[77,49] := 1.373624787; GammaY[78,49] := 1.398937639; GammaY[79,49] := 1.425257998;
  //GammaY[80,49] := 1.452687278; GammaY[81,49] := 1.481342643; GammaY[82,49] := 1.511360489; GammaY[83,49] := 1.542900951; GammaY[84,49] := 1.576153824; GammaY[85,49] := 1.611346444; GammaY[86,49] := 1.648754397; GammaY[87,49] := 1.688716358; GammaY[88,49] := 1.731655138; GammaY[89,49] := 1.778108323;
  //GammaY[90,49] := 1.828774434; GammaY[91,49] := 1.884585024; GammaY[92,49] := 1.946822552; GammaY[93,49] := 2.017324534; GammaY[94,49] := 2.098863548; GammaY[95,49] := 2.195926227; GammaY[96,49] := 2.316542299; GammaY[97,49] := 2.477582653; GammaY[98,49] := 2.727474268; GammaY[99,49] := 3.284809312;
  //GammaY[0,50] := 0.110919575; GammaY[1,50] := 0.170282912; GammaY[2,50] := 0.208632524; GammaY[3,50] := 0.238584474; GammaY[4,50] := 0.264142219; GammaY[5,50] := 0.286904289; GammaY[6,50] := 0.307703339; GammaY[7,50] := 0.327037924; GammaY[8,50] := 0.345234125; GammaY[9,50] := 0.362518811;
  //GammaY[10,50] := 0.379057247; GammaY[11,50] := 0.394974206; GammaY[12,50] := 0.410366635; GammaY[13,50] := 0.425311652; GammaY[14,50] := 0.439871847; GammaY[15,50] := 0.454098889; GammaY[16,50] := 0.468036071; GammaY[17,50] := 0.481720149; GammaY[18,50] := 0.495182695; GammaY[19,50] := 0.508451128;
  //GammaY[20,50] := 0.521549489; GammaY[21,50] := 0.53449904; GammaY[22,50] := 0.547318753; GammaY[23,50] := 0.560025687; GammaY[24,50] := 0.572635301; GammaY[25,50] := 0.585161697; GammaY[26,50] := 0.597617833; GammaY[27,50] := 0.610015694; GammaY[28,50] := 0.622366425; GammaY[29,50] := 0.634680479;
  //GammaY[30,50] := 0.646967708; GammaY[31,50] := 0.659237434; GammaY[32,50] := 0.671498552; GammaY[33,50] := 0.683759589; GammaY[34,50] := 0.696028765; GammaY[35,50] := 0.708314046; GammaY[36,50] := 0.720623196; GammaY[37,50] := 0.732963816; GammaY[38,50] := 0.745343389; GammaY[39,50] := 0.757769301;
  //GammaY[40,50] := 0.770248918; GammaY[41,50] := 0.782789593; GammaY[42,50] := 0.795398677; GammaY[43,50] := 0.808083582; GammaY[44,50] := 0.820851804; GammaY[45,50] := 0.833710951; GammaY[46,50] := 0.846668779; GammaY[47,50] := 0.859733219; GammaY[48,50] := 0.872912415; GammaY[49,50] := 0.886214736;
  //GammaY[50,50] := 0.899648863; GammaY[51,50] := 0.9132238; GammaY[52,50] := 0.926948879; GammaY[53,50] := 0.94083385; GammaY[54,50] := 0.954888913; GammaY[55,50] := 0.969124769; GammaY[56,50] := 0.98355268; GammaY[57,50] := 0.998184485; GammaY[58,50] := 1.013032787; GammaY[59,50] := 1.028110952;
  //GammaY[60,50] := 1.043433123; GammaY[61,50] := 1.059014416; GammaY[62,50] := 1.074871; GammaY[63,50] := 1.091020206; GammaY[64,50] := 1.107480672; GammaY[65,50] := 1.124272488; GammaY[66,50] := 1.141417379; GammaY[67,50] := 1.158938905; GammaY[68,50] := 1.176862692; GammaY[69,50] := 1.195216705;
  //GammaY[70,50] := 1.21403156; GammaY[71,50] := 1.233340893; GammaY[72,50] := 1.253181792; GammaY[73,50] := 1.273595308; GammaY[74,50] := 1.294627066; GammaY[75,50] := 1.316327983; GammaY[76,50] := 1.338755183; GammaY[77,50] := 1.361973041; GammaY[78,50] := 1.386054485; GammaY[79,50] := 1.411082643;
  //GammaY[80,50] := 1.437152868; GammaY[81,50] := 1.464375295; GammaY[82,50] := 1.492878109; GammaY[83,50] := 1.522811779; GammaY[84,50] := 1.554354598; GammaY[85,50] := 1.587720088; GammaY[86,50] := 1.623167031; GammaY[87,50] := 1.661013379; GammaY[88,50] := 1.701655977; GammaY[89,50] := 1.745599299;
  //GammaY[90,50] := 1.793498667; GammaY[91,50] := 1.84622778; GammaY[92,50] := 1.90498912; GammaY[93,50] := 1.971505078; GammaY[94,50] := 2.048373622; GammaY[95,50] := 2.139797201; GammaY[96,50] := 2.253293467; GammaY[97,50] := 2.404648354; GammaY[98,50] := 2.639137735; GammaY[99,50] := 3.160733458;
  //GammaY[0,51] := 0.126456206; GammaY[1,51] := 0.18874329; GammaY[2,51] := 0.228292903; GammaY[3,51] := 0.25888635; GammaY[4,51] := 0.284828924; GammaY[5,51] := 0.307825854; GammaY[6,51] := 0.328760726; GammaY[7,51] := 0.348160506; GammaY[8,51] := 0.366368751; GammaY[9,51] := 0.383623838;
  //GammaY[10,51] := 0.400098999; GammaY[11,51] := 0.415924734; GammaY[12,51] := 0.431202234; GammaY[13,51] := 0.446011849; GammaY[14,51] := 0.460418668; GammaY[15,51] := 0.474476327; GammaY[16,51] := 0.488229694; GammaY[17,51] := 0.501716802; GammaY[18,51] := 0.514970256; GammaY[19,51] := 0.528018319;
  //GammaY[20,51] := 0.540885726; GammaY[21,51] := 0.553594321; GammaY[22,51] := 0.566163551; GammaY[23,51] := 0.578610873; GammaY[24,51] := 0.59095206; GammaY[25,51] := 0.603201487; GammaY[26,51] := 0.615372332; GammaY[27,51] := 0.627476745; GammaY[28,51] := 0.639526011; GammaY[29,51] := 0.651530674;
  //GammaY[30,51] := 0.663500642; GammaY[31,51] := 0.675445286; GammaY[32,51] := 0.687373511; GammaY[33,51] := 0.699293834; GammaY[34,51] := 0.711214432; GammaY[35,51] := 0.723143228; GammaY[36,51] := 0.735087921; GammaY[37,51] := 0.747056012; GammaY[38,51] := 0.759054873; GammaY[39,51] := 0.771091776;
  //GammaY[40,51] := 0.783173929; GammaY[41,51] := 0.795308506; GammaY[42,51] := 0.807502687; GammaY[43,51] := 0.819763683; GammaY[44,51] := 0.832098767; GammaY[45,51] := 0.844515292; GammaY[46,51] := 0.857020765; GammaY[47,51] := 0.869622852; GammaY[48,51] := 0.882329377; GammaY[49,51] := 0.895148399;
  //GammaY[50,51] := 0.908088232; GammaY[51,51] := 0.921157484; GammaY[52,51] := 0.934365093; GammaY[53,51] := 0.947720371; GammaY[54,51] := 0.961233045; GammaY[55,51] := 0.974913289; GammaY[56,51] := 0.988771836; GammaY[57,51] := 1.002819977; GammaY[58,51] := 1.017069652; GammaY[59,51] := 1.03153353;
  //GammaY[60,51] := 1.046225032; GammaY[61,51] := 1.061158481; GammaY[62,51] := 1.076349184; GammaY[63,51] := 1.091813541; GammaY[64,51] := 1.107569175; GammaY[65,51] := 1.123635075; GammaY[66,51] := 1.140031765; GammaY[67,51] := 1.15678149; GammaY[68,51] := 1.173908436; GammaY[69,51] := 1.191438988;
  //GammaY[70,51] := 1.20940201; GammaY[71,51] := 1.227829225; GammaY[72,51] := 1.246755599; GammaY[73,51] := 1.2662198; GammaY[74,51] := 1.286264804; GammaY[75,51] := 1.306938569; GammaY[76,51] := 1.328294865; GammaY[77,51] := 1.350394278; GammaY[78,51] := 1.373305443; GammaY[79,51] := 1.39710657;
  //GammaY[80,51] := 1.421887351; GammaY[81,51] := 1.447751356; GammaY[82,51] := 1.474819113; GammaY[83,51] := 1.503232064; GammaY[84,51] := 1.533157779; GammaY[85,51] := 1.564796892; GammaY[86,51] := 1.598392517; GammaY[87,51] := 1.63424329; GammaY[88,51] := 1.672721858; GammaY[89,51] := 1.71430184;
  //GammaY[90,51] := 1.759598324; GammaY[91,51] := 1.809431136; GammaY[92,51] := 1.864928312; GammaY[93,51] := 1.927705113; GammaY[94,51] := 2.000197097; GammaY[95,51] := 2.086342467; GammaY[96,51] := 2.193182982; GammaY[97,51] := 2.335496903; GammaY[98,51] := 2.555635871; GammaY[99,51] := 3.044008115;
  //GammaY[0,52] := 0.142798243; GammaY[1,52] := 0.207679658; GammaY[2,52] := 0.248232854; GammaY[3,52] := 0.2793284; GammaY[4,52] := 0.30554668; GammaY[5,52] := 0.328688691; GammaY[6,52] := 0.349683243; GammaY[7,52] := 0.369082304; GammaY[8,52] := 0.38724469; GammaY[9,52] := 0.404418748;
  //GammaY[10,52] := 0.420784568; GammaY[11,52] := 0.436477554; GammaY[12,52] := 0.451602504; GammaY[13,52] := 0.466242499; GammaY[14,52] := 0.480464732; GammaY[15,52] := 0.494324486; GammaY[16,52] := 0.507867932; GammaY[17,52] := 0.521134142; GammaY[18,52] := 0.534156571; GammaY[19,52] := 0.54696417;
  //GammaY[20,52] := 0.55958223; GammaY[21,52] := 0.572033055; GammaY[22,52] := 0.584336478; GammaY[23,52] := 0.596510254; GammaY[24,52] := 0.60857041; GammaY[25,52] := 0.620531514; GammaY[26,52] := 0.632406893; GammaY[27,52] := 0.644208818; GammaY[28,52] := 0.655948661; GammaY[29,52] := 0.667637021;
  //GammaY[30,52] := 0.67928383; GammaY[31,52] := 0.69089847; GammaY[32,52] := 0.702489844; GammaY[33,52] := 0.714066425; GammaY[34,52] := 0.725636349; GammaY[35,52] := 0.737207458; GammaY[36,52] := 0.748787354; GammaY[37,52] := 0.760383443; GammaY[38,52] := 0.772002977; GammaY[39,52] := 0.783653091;
  //GammaY[40,52] := 0.795340828; GammaY[41,52] := 0.807073204; GammaY[42,52] := 0.818857227; GammaY[43,52] := 0.830699896; GammaY[44,52] := 0.842608265; GammaY[45,52] := 0.85458947; GammaY[46,52] := 0.866650748; GammaY[47,52] := 0.878799475; GammaY[48,52] := 0.891043196; GammaY[49,52] := 0.903389652;
  //GammaY[50,52] := 0.915846821; GammaY[51,52] := 0.928422924; GammaY[52,52] := 0.941126528; GammaY[53,52] := 0.953966546; GammaY[54,52] := 0.966952241; GammaY[55,52] := 0.980093329; GammaY[56,52] := 0.993399983; GammaY[57,52] := 1.006882964; GammaY[58,52] := 1.020553633; GammaY[59,52] := 1.034423974;
  //GammaY[60,52] := 1.048506736; GammaY[61,52] := 1.062815494; GammaY[62,52] := 1.077364745; GammaY[63,52] := 1.092170013; GammaY[64,52] := 1.107247969; GammaY[65,52] := 1.122616566; GammaY[66,52] := 1.138295198; GammaY[67,52] := 1.15430488; GammaY[68,52] := 1.170668437; GammaY[69,52] := 1.187410784;
  //GammaY[70,52] := 1.204559174; GammaY[71,52] := 1.222143503; GammaY[72,52] := 1.240196732; GammaY[73,52] := 1.25875532; GammaY[74,52] := 1.277859765; GammaY[75,52] := 1.297555249; GammaY[76,52] := 1.317892416; GammaY[77,52] := 1.338928315; GammaY[78,52] := 1.360727564; GammaY[79,52] := 1.383363776;
  //GammaY[80,52] := 1.406921351; GammaY[81,52] := 1.431497736; GammaY[82,52] := 1.4572063; GammaY[83,52] := 1.484180066; GammaY[84,52] := 1.512576594; GammaY[85,52] := 1.542584494; GammaY[86,52] := 1.574432264; GammaY[87,52] := 1.608400527; GammaY[88,52] := 1.644839378; GammaY[89,52] := 1.684193667;
  //GammaY[90,52] := 1.727040984; GammaY[91,52] := 1.774150981; GammaY[92,52] := 1.826582366; GammaY[93,52] := 1.885850669; GammaY[94,52] := 1.954240308; GammaY[95,52] := 2.035443695; GammaY[96,52] := 2.136060142; GammaY[97,52] := 2.269931605; GammaY[98,52] := 2.476694886; GammaY[99,52] := 2.934171205;
  //GammaY[0,53] := 0.159853162; GammaY[1,53] := 0.226995069; GammaY[2,53] := 0.2683619; GammaY[3,53] := 0.299827747; GammaY[4,53] := 0.326219988; GammaY[5,53] := 0.349424254; GammaY[6,53] := 0.370408859; GammaY[7,53] := 0.389747384; GammaY[8,53] := 0.407811722; GammaY[9,53] := 0.424858697;
  //GammaY[10,53] := 0.441074187; GammaY[11,53] := 0.456597702; GammaY[12,53] := 0.471537044; GammaY[13,53] := 0.485977541; GammaY[14,53] := 0.499988104; GammaY[15,53] := 0.513625352; GammaY[16,53] := 0.526936504; GammaY[17,53] := 0.539961469; GammaY[18,53] := 0.552734375; GammaY[19,53] := 0.565284706;
  //GammaY[20,53] := 0.577638192; GammaY[21,53] := 0.589817483; GammaY[22,53] := 0.601842684; GammaY[23,53] := 0.613731778; GammaY[24,53] := 0.625500969; GammaY[25,53] := 0.637164957; GammaY[26,53] := 0.648737161; GammaY[27,53] := 0.660229928; GammaY[28,53] := 0.671654682; GammaY[29,53] := 0.683022033;
  //GammaY[30,53] := 0.69434192; GammaY[31,53] := 0.705623697; GammaY[32,53] := 0.716876217; GammaY[33,53] := 0.728107907; GammaY[34,53] := 0.73932683; GammaY[35,53] := 0.750540742; GammaY[36,53] := 0.761757131; GammaY[37,53] := 0.772983299; GammaY[38,53] := 0.784226379; GammaY[39,53] := 0.795493354;
  //GammaY[40,53] := 0.806791122; GammaY[41,53] := 0.818126521; GammaY[42,53] := 0.829506357; GammaY[43,53] := 0.840937441; GammaY[44,53] := 0.852426614; GammaY[45,53] := 0.863980777; GammaY[46,53] := 0.875606903; GammaY[47,53] := 0.887312117; GammaY[48,53] := 0.899103694; GammaY[49,53] := 0.910989059;
  //GammaY[50,53] := 0.922975862; GammaY[51,53] := 0.935071999; GammaY[52,53] := 0.947285648; GammaY[53,53] := 0.959625303; GammaY[54,53] := 0.97209982; GammaY[55,53] := 0.984718458; GammaY[56,53] := 0.997490888; GammaY[57,53] := 1.010427355; GammaY[58,53] := 1.023538668; GammaY[59,53] := 1.036836197;
  //GammaY[60,53] := 1.050332034; GammaY[61,53] := 1.064039052; GammaY[62,53] := 1.077970984; GammaY[63,53] := 1.092142529; GammaY[64,53] := 1.106569462; GammaY[65,53] := 1.121268765; GammaY[66,53] := 1.136258773; GammaY[67,53] := 1.151559341; GammaY[68,53] := 1.167192041; GammaY[69,53] := 1.183180386;
  //GammaY[70,53] := 1.199550088; GammaY[71,53] := 1.21632937; GammaY[72,53] := 1.233549319; GammaY[73,53] := 1.251244312; GammaY[74,53] := 1.269452533; GammaY[75,53] := 1.288216572; GammaY[76,53] := 1.307584143; GammaY[77,53] := 1.327608988; GammaY[78,53] := 1.348351965; GammaY[79,53] := 1.369882392;
  //GammaY[80,53] := 1.392279728; GammaY[81,53] := 1.415635692; GammaY[82,53] := 1.440056973; GammaY[83,53] := 1.465668728; GammaY[84,53] := 1.492619172; GammaY[85,53] := 1.521085692; GammaY[86,53] := 1.551283147; GammaY[87,53] := 1.583475356; GammaY[88,53] := 1.617991379; GammaY[89,53] := 1.65524924;
  //GammaY[90,53] := 1.695791569; GammaY[91,53] := 1.740341264; GammaY[92,53] := 1.789892442; GammaY[93,53] := 1.845867751; GammaY[94,53] := 1.910410905; GammaY[95,53] := 1.986985584; GammaY[96,53] := 2.081779564; GammaY[97,53] := 2.207764444; GammaY[98,53] := 2.402055486; GammaY[99,53] := 2.83078892;
  //GammaY[0,54] := 0.177527023; GammaY[1,54] := 0.246597518; GammaY[2,54] := 0.288596521; GammaY[3,54] := 0.320309251; GammaY[4,54] := 0.346781402; GammaY[5,54] := 0.369972122; GammaY[6,54] := 0.390883582; GammaY[7,54] := 0.41010768; GammaY[8,54] := 0.428027279; GammaY[9,54] := 0.444906251;
  //GammaY[10,54] := 0.460935229; GammaY[11,54] := 0.476257063; GammaY[12,54] := 0.490981981; GammaY[13,54] := 0.505197105; GammaY[14,54] := 0.518972708; GammaY[15,54] := 0.53236646; GammaY[16,54] := 0.545426393; GammaY[17,54] := 0.55819305; GammaY[18,54] := 0.570701058; GammaY[19,54] := 0.582980301;
  //GammaY[20,54] := 0.595056822; GammaY[21,54] := 0.606953512; GammaY[22,54] := 0.618690672; GammaY[23,54] := 0.630286438; GammaY[24,54] := 0.641757118; GammaY[25,54] := 0.653117487; GammaY[26,54] := 0.664381023; GammaY[27,54] := 0.675560091; GammaY[28,54] := 0.686666108; GammaY[29,54] := 0.69770968;
  //GammaY[30,54] := 0.708700711; GammaY[31,54] := 0.719648499; GammaY[32,54] := 0.730561843; GammaY[33,54] := 0.741449103; GammaY[34,54] := 0.752318246; GammaY[35,54] := 0.763176926; GammaY[36,54] := 0.774032533; GammaY[37,54] := 0.784892232; GammaY[38,54] := 0.795763009; GammaY[39,54] := 0.806651707;
  //GammaY[40,54] := 0.817565064; GammaY[41,54] := 0.828509731; GammaY[42,54] := 0.839492341; GammaY[43,54] := 0.850519518; GammaY[44,54] := 0.861597878; GammaY[45,54] := 0.872734097; GammaY[46,54] := 0.883934923; GammaY[47,54] := 0.895207207; GammaY[48,54] := 0.906557936; GammaY[49,54] := 0.917994253;
  //GammaY[50,54] := 0.929523498; GammaY[51,54] := 0.941153214; GammaY[52,54] := 0.952891238; GammaY[53,54] := 0.964745706; GammaY[54,54] := 0.976725052; GammaY[55,54] := 0.9888381; GammaY[56,54] := 1.001094084; GammaY[57,54] := 1.01350273; GammaY[58,54] := 1.026074287; GammaY[59,54] := 1.038819575;
  //GammaY[60,54] := 1.051750072; GammaY[61,54] := 1.064877983; GammaY[62,54] := 1.078216325; GammaY[63,54] := 1.091779028; GammaY[64,54] := 1.105581015; GammaY[65,54] := 1.119638348; GammaY[66,54] := 1.133968374; GammaY[67,54] := 1.148589863; GammaY[68,54] := 1.163523197; GammaY[69,54] := 1.178790586;
  //GammaY[70,54] := 1.19441631; GammaY[71,54] := 1.210427006; GammaY[72,54] := 1.226852012; GammaY[73,54] := 1.243723761; GammaY[74,54] := 1.261078261; GammaY[75,54] := 1.278955663; GammaY[76,54] := 1.297400951; GammaY[77,54] := 1.316464774; GammaY[78,54] := 1.336204476; GammaY[79,54] := 1.356685355;
  //GammaY[80,54] := 1.37798225; GammaY[81,54] := 1.400181529; GammaY[82,54] := 1.423383637; GammaY[83,54] := 1.447706383; GammaY[84,54] := 1.473289253; GammaY[85,54] := 1.500299156; GammaY[86,54] := 1.528938217; GammaY[87,54] := 1.559454571; GammaY[88,54] := 1.59215765; GammaY[89,54] := 1.627440448;
  //GammaY[90,54] := 1.665813002; GammaY[91,54] := 1.707954614; GammaY[92,54] := 1.754799198; GammaY[93,54] := 1.807682866; GammaY[94,54] := 1.868618275; GammaY[95,54] := 1.940856162; GammaY[96,54] := 2.030201352; GammaY[97,54] := 2.148816019; GammaY[98,54] := 2.33147238; GammaY[99,54] := 2.73345551;
  //GammaY[0,55] := 0.195726439; GammaY[1,55] := 0.2664007; GammaY[2,55] := 0.308860333; GammaY[3,55] := 0.340705387; GammaY[4,55] := 0.367171191; GammaY[5,55] := 0.390279498; GammaY[6,55] := 0.411060867; GammaY[7,55] := 0.430122352; GammaY[8,55] := 0.447855763; GammaY[9,55] := 0.464530643;
  //GammaY[10,55] := 0.48034141; GammaY[11,55] := 0.495433546; GammaY[12,55] := 0.509919158; GammaY[13,55] := 0.523886746; GammaY[14,55] := 0.537407605; GammaY[15,55] := 0.550540175; GammaY[16,55] := 0.563333082; GammaY[17,55] := 0.575827325; GammaY[18,55] := 0.58805787; GammaY[19,55] := 0.600054876;
  //GammaY[20,55] := 0.611844596; GammaY[21,55] := 0.623450075; GammaY[22,55] := 0.634891718; GammaY[23,55] := 0.646187734; GammaY[24,55] := 0.65735448; GammaY[25,55] := 0.668406759; GammaY[26,55] := 0.679358049; GammaY[27,55] := 0.69022069; GammaY[28,55] := 0.701006077; GammaY[29,55] := 0.711724776;
  //GammaY[30,55] := 0.722386627; GammaY[31,55] := 0.733000865; GammaY[32,55] := 0.7435762; GammaY[33,55] := 0.754120889; GammaY[34,55] := 0.764642804; GammaY[35,55] := 0.775149486; GammaY[36,55] := 0.785648188; GammaY[37,55] := 0.796145953; GammaY[38,55] := 0.806649634; GammaY[39,55] := 0.817165912;
  //GammaY[40,55] := 0.827701361; GammaY[41,55] := 0.838262471; GammaY[42,55] := 0.848855677; GammaY[43,55] := 0.859487395; GammaY[44,55] := 0.870164042; GammaY[45,55] := 0.880892071; GammaY[46,55] := 0.891677979; GammaY[47,55] := 0.902528381; GammaY[48,55] := 0.913450011; GammaY[49,55] := 0.924449719;
  //GammaY[50,55] := 0.935534544; GammaY[51,55] := 0.94671173; GammaY[52,55] := 0.957988759; GammaY[53,55] := 0.969373386; GammaY[54,55] := 0.980873675; GammaY[55,55] := 0.992498041; GammaY[56,55] := 1.004255259; GammaY[57,55] := 1.016154598; GammaY[58,55] := 1.028205824; GammaY[59,55] := 1.040419196;
  //GammaY[60,55] := 1.052805611; GammaY[61,55] := 1.065376648; GammaY[62,55] := 1.078144647; GammaY[63,55] := 1.091122804; GammaY[64,55] := 1.104325254; GammaY[65,55] := 1.117767199; GammaY[66,55] := 1.131465049; GammaY[67,55] := 1.145436554; GammaY[68,55] := 1.159700981; GammaY[69,55] := 1.174279316;
  //GammaY[70,55] := 1.189194494; GammaY[71,55] := 1.204471668; GammaY[72,55] := 1.220138533; GammaY[73,55] := 1.236225699; GammaY[74,55] := 1.252767139; GammaY[75,55] := 1.269800726; GammaY[76,55] := 1.287368881; GammaY[77,55] := 1.30551936; GammaY[78,55] := 1.324306216; GammaY[79,55] := 1.343790989;
  //GammaY[80,55] := 1.364044196; GammaY[81,55] := 1.385147202; GammaY[82,55] := 1.407194615; GammaY[83,55] := 1.430297376; GammaY[84,55] := 1.454586815; GammaY[85,55] := 1.480220052; GammaY[86,55] := 1.507387318; GammaY[87,55] := 1.536322102; GammaY[88,55] := 1.567315513; GammaY[89,55] := 1.600737198;
  //GammaY[90,55] := 1.637066773; GammaY[91,55] := 1.676942874; GammaY[92,55] := 1.721243283; GammaY[93,55] := 1.771223443; GammaY[94,55] := 1.828773898; GammaY[95,55] := 1.896947041; GammaY[96,55] := 1.981191185; GammaY[97,55] := 2.092915408; GammaY[98,55] := 2.264713743; GammaY[99,55] := 2.641791593;
  //GammaY[0,56] := 0.21436016; GammaY[1,56] := 0.286324486; GammaY[2,56] := 0.329084097; GammaY[3,56] := 0.360955975; GammaY[4,56] := 0.387336901; GammaY[5,56] := 0.410300689; GammaY[6,56] := 0.430901039; GammaY[7,56] := 0.449757136; GammaY[8,56] := 0.467267828; GammaY[9,56] := 0.483707049;
  //GammaY[10,56] := 0.4992721; GammaY[11,56] := 0.514110413; GammaY[12,56] := 0.528335466; GammaY[13,56] := 0.542036741; GammaY[14,56] := 0.555286242; GammaY[15,56] := 0.568142937; GammaY[16,56] := 0.580655845; GammaY[17,56] := 0.592866243; GammaY[18,56] := 0.604809315; GammaY[19,56] := 0.616515363;
  //GammaY[20,56] := 0.628010737; GammaY[21,56] := 0.639318553; GammaY[22,56] := 0.650459248; GammaY[23,56] := 0.661451045; GammaY[24,56] := 0.672310304; GammaY[25,56] := 0.683051796; GammaY[26,56] := 0.693688959; GammaY[27,56] := 0.704234093; GammaY[28,56] := 0.714698519; GammaY[29,56] := 0.725092723;
  //GammaY[30,56] := 0.735426468; GammaY[31,56] := 0.745708898; GammaY[32,56] := 0.75594861; GammaY[33,56] := 0.766153762; GammaY[34,56] := 0.776332119; GammaY[35,56] := 0.78649109; GammaY[36,56] := 0.796637808; GammaY[37,56] := 0.806779163; GammaY[38,56] := 0.816921851; GammaY[39,56] := 0.827072404;
  //GammaY[40,56] := 0.83723723; GammaY[41,56] := 0.847422629; GammaY[42,56] := 0.857634865; GammaY[43,56] := 0.86788017; GammaY[44,56] := 0.878164744; GammaY[45,56] := 0.888494824; GammaY[46,56] := 0.898876691; GammaY[47,56] := 0.909316705; GammaY[48,56] := 0.919821329; GammaY[49,56] := 0.930397154;
  //GammaY[50,56] := 0.94105093; GammaY[51,56] := 0.951789577; GammaY[52,56] := 0.962620268; GammaY[53,56] := 0.973550434; GammaY[54,56] := 0.984587755; GammaY[55,56] := 0.99574024; GammaY[56,56] := 1.007016284; GammaY[57,56] := 1.018424705; GammaY[58,56] := 1.029974755; GammaY[59,56] := 1.041676202;
  //GammaY[60,56] := 1.053539392; GammaY[61,56] := 1.065575313; GammaY[62,56] := 1.077795666; GammaY[63,56] := 1.090212953; GammaY[64,56] := 1.10284057; GammaY[65,56] := 1.115692916; GammaY[66,56] := 1.128785516; GammaY[67,56] := 1.142135159; GammaY[68,56] := 1.155760067; GammaY[69,56] := 1.169680075;
  //GammaY[70,56] := 1.183916856; GammaY[71,56] := 1.198494173; GammaY[72,56] := 1.213438179; GammaY[73,56] := 1.228777774; GammaY[74,56] := 1.244545024; GammaY[75,56] := 1.26077567; GammaY[76,56] := 1.277509732; GammaY[77,56] := 1.294792256; GammaY[78,56] := 1.312674214; GammaY[79,56] := 1.33121363;
  //GammaY[80,56] := 1.350476976; GammaY[81,56] := 1.370540938; GammaY[82,56] := 1.391494663; GammaY[83,56] := 1.413442673; GammaY[84,56] := 1.436508672; GammaY[85,56] := 1.460840631; GammaY[86,56] := 1.48661767; GammaY[87,56] := 1.514059596; GammaY[88,56] := 1.543440403; GammaY[89,56] := 1.575107935;
  //GammaY[90,56] := 1.60951342; GammaY[91,56] := 1.647257548; GammaY[92,56] := 1.689165732; GammaY[93,56] := 1.736418187; GammaY[94,56] := 1.790791616; GammaY[95,56] := 1.85515359; GammaY[96,56] := 1.934620365; GammaY[97,56] := 2.039900005; GammaY[98,56] := 2.201560678; GammaY[99,56] := 2.555441898;
  //GammaY[0,57] := 0.233340292; GammaY[1,57] := 0.306295157; GammaY[2,57] := 0.349205552; GammaY[3,57] := 0.381007826; GammaY[4,57] := 0.407232901; GammaY[5,57] := 0.429996554; GammaY[6,57] := 0.450370659; GammaY[7,57] := 0.468983687; GammaY[8,57] := 0.486239752; GammaY[9,57] := 0.502415966;
  //GammaY[10,57] := 0.517711653; GammaY[11,57] := 0.532275581; GammaY[12,57] := 0.546222131; GammaY[13,57] := 0.559641409; GammaY[14,57] := 0.57260585; GammaY[15,57] := 0.58517471; GammaY[16,57] := 0.597397204; GammaY[17,57] := 0.609314745; GammaY[18,57] := 0.620962589; GammaY[19,57] := 0.632371084;
  //GammaY[20,57] := 0.643566597; GammaY[21,57] := 0.654572228; GammaY[22,57] := 0.665408391; GammaY[23,57] := 0.676093259; GammaY[24,57] := 0.686643127; GammaY[25,57] := 0.697072705; GammaY[26,57] := 0.707395352; GammaY[27,57] := 0.717623276; GammaY[28,57] := 0.727767712; GammaY[29,57] := 0.737839057;
  //GammaY[30,57] := 0.747846962; GammaY[31,57] := 0.757800459; GammaY[32,57] := 0.767708041; GammaY[33,57] := 0.777577734; GammaY[34,57] := 0.787417166; GammaY[35,57] := 0.797233618; GammaY[36,57] := 0.807034085; GammaY[37,57] := 0.816825296; GammaY[38,57] := 0.826613803; GammaY[39,57] := 0.836405995;
  //GammaY[40,57] := 0.846208098; GammaY[41,57] := 0.856026252; GammaY[42,57] := 0.865866525; GammaY[43,57] := 0.875734942; GammaY[44,57] := 0.885637514; GammaY[45,57] := 0.895580263; GammaY[46,57] := 0.905569232; GammaY[47,57] := 0.915610559; GammaY[48,57] := 0.925710475; GammaY[49,57] := 0.935875295;
  //GammaY[50,57] := 0.946111494; GammaY[51,57] := 0.956425717; GammaY[52,57] := 0.966824813; GammaY[53,57] := 0.977315864; GammaY[54,57] := 0.987906218; GammaY[55,57] := 0.998603494; GammaY[56,57] := 1.009415711; GammaY[57,57] := 1.020351276; GammaY[58,57] := 1.03141897; GammaY[59,57] := 1.042628081;
  //GammaY[60,57] := 1.053988437; GammaY[61,57] := 1.065510466; GammaY[62,57] := 1.07720527; GammaY[63,57] := 1.0890847; GammaY[64,57] := 1.101161451; GammaY[65,57] := 1.113449161; GammaY[66,57] := 1.125962527; GammaY[67,57] := 1.138717437; GammaY[68,57] := 1.151731128; GammaY[69,57] := 1.165022354;
  //GammaY[70,57] := 1.178611607; GammaY[71,57] := 1.192521344; GammaY[72,57] := 1.206776272; GammaY[73,57] := 1.221403686; GammaY[74,57] := 1.236433866; GammaY[75,57] := 1.251900551; GammaY[76,57] := 1.267841514; GammaY[77,57] := 1.28429926; GammaY[78,57] := 1.301321878; GammaY[79,57] := 1.3189641;
  //GammaY[80,57] := 1.337288611; GammaY[81,57] := 1.356367716; GammaY[82,57] := 1.376285456; GammaY[83,57] := 1.397140346; GammaY[84,57] := 1.419048959; GammaY[85,57] := 1.442150712; GammaY[86,57] := 1.466614338; GammaY[87,57] := 1.492646857; GammaY[88,57] := 1.520506274; GammaY[89,57] := 1.550520067;
  //GammaY[90,57] := 1.583112948; GammaY[91,57] := 1.618850182; GammaY[92,57] := 1.658508308; GammaY[93,57] := 1.703197353; GammaY[94,57] := 1.754587845; GammaY[95,57] := 1.815375048; GammaY[96,57] := 1.890365795; GammaY[97,57] := 1.989615306; GammaY[98,57] := 2.141806649; GammaY[99,57] := 2.474073262;
  //GammaY[0,58] := 0.252583212; GammaY[1,58] := 0.326245468; GammaY[2,58] := 0.36916917; GammaY[3,58] := 0.400814331; GammaY[4,58] := 0.426819854; GammaY[5,58] := 0.449333931; GammaY[6,58] := 0.469441954; GammaY[7,58] := 0.487779018; GammaY[8,58] := 0.504752821; GammaY[9,58] := 0.52064255;
  //GammaY[10,58] := 0.535648779; GammaY[11,58] := 0.549921039; GammaY[12,58] := 0.563574183; GammaY[13,58] := 0.576698597; GammaY[14,58] := 0.589366884; GammaY[15,58] := 0.601638382; GammaY[16,58] := 0.613562337; GammaY[17,58] := 0.625180159; GammaY[18,58] := 0.636527069; GammaY[19,58] := 0.647633353;
  //GammaY[20,58] := 0.658525303; GammaY[21,58] := 0.66922594; GammaY[22,58] := 0.679755588; GammaY[23,58] := 0.690132314; GammaY[24,58] := 0.700372318; GammaY[25,58] := 0.710490211; GammaY[26,58] := 0.720499237; GammaY[27,58] := 0.730411495; GammaY[28,58] := 0.7402381; GammaY[29,58] := 0.749989316;
  //GammaY[30,58] := 0.759674679; GammaY[31,58] := 0.769303095; GammaY[32,58] := 0.778882913; GammaY[33,58] := 0.788422035; GammaY[34,58] := 0.797927962; GammaY[35,58] := 0.807407825; GammaY[36,58] := 0.816868469; GammaY[37,58] := 0.826316487; GammaY[38,58] := 0.835758265; GammaY[39,58] := 0.845200011;
  //GammaY[40,58] := 0.854647798; GammaY[41,58] := 0.864107574; GammaY[42,58] := 0.873585237; GammaY[43,58] := 0.883086637; GammaY[44,58] := 0.892617573; GammaY[45,58] := 0.902183861; GammaY[46,58] := 0.911791343; GammaY[47,58] := 0.921445914; GammaY[48,58] := 0.93115355; GammaY[49,58] := 0.940920328;
  //GammaY[50,58] := 0.950752438; GammaY[51,58] := 0.960656261; GammaY[52,58] := 0.970638369; GammaY[53,58] := 0.980705513; GammaY[54,58] := 0.990864709; GammaY[55,58] := 1.001123238; GammaY[56,58] := 1.01148873; GammaY[57,58] := 1.021969177; GammaY[58,58] := 1.032572944; GammaY[59,58] := 1.043308862;
  //GammaY[60,58] := 1.054186268; GammaY[61,58] := 1.065215063; GammaY[62,58] := 1.076405782; GammaY[63,58] := 1.087769664; GammaY[64,58] := 1.099318745; GammaY[65,58] := 1.111065942; GammaY[66,58] := 1.123025174; GammaY[67,58] := 1.135211483; GammaY[68,58] := 1.14764118; GammaY[69,58] := 1.160332009;
  //GammaY[70,58] := 1.173303345; GammaY[71,58] := 1.186576417; GammaY[72,58] := 1.200174578; GammaY[73,58] := 1.214123618; GammaY[74,58] := 1.228452141; GammaY[75,58] := 1.243192011; GammaY[76,58] := 1.258378892; GammaY[77,58] := 1.274052911; GammaY[78,58] := 1.290259454; GammaY[79,58] := 1.307050166;
  //GammaY[80,58] := 1.324484191; GammaY[81,58] := 1.34262973; GammaY[82,58] := 1.361566045; GammaY[83,58] := 1.381386027; GammaY[84,58] := 1.402199581; GammaY[85,58] := 1.424138119; GammaY[86,58] := 1.44736066; GammaY[87,58] := 1.47206227; GammaY[88,58] := 1.498486008; GammaY[89,58] := 1.526940316;
  //GammaY[90,58] := 1.557825137; GammaY[91,58] := 1.591672648; GammaY[92,58] := 1.62921375; GammaY[93,58] := 1.671492953; GammaY[94,58] := 1.720081714; GammaY[95,58] := 1.777514579; GammaY[96,58] := 1.848309913; GammaY[97,58] := 1.941914658; GammaY[98,58] := 2.08525692; GammaY[99,58] := 2.397375231;
  //GammaY[0,59] := 0.272010196; GammaY[1,59] := 0.34611456; GammaY[2,59] := 0.388925814; GammaY[3,59] := 0.420335003; GammaY[4,59] := 0.446064223; GammaY[5,59] := 0.468285127; GammaY[6,59] := 0.488092256; GammaY[7,59] := 0.506124877; GammaY[8,59] := 0.522792749; GammaY[9,59] := 0.538376103;
  //GammaY[10,59] := 0.553076033; GammaY[11,59] := 0.567042318; GammaY[12,59] := 0.580389879; GammaY[13,59] := 0.593209092; GammaY[14,59] := 0.605572496; GammaY[15,59] := 0.617539321; GammaY[16,59] := 0.629158687; GammaY[17,59] := 0.640471861; GammaY[18,59] := 0.651513927; GammaY[19,59] := 0.662315019;
  //GammaY[20,59] := 0.672901288; GammaY[21,59] := 0.683295619; GammaY[22,59] := 0.693518184; GammaY[23,59] := 0.703586917; GammaY[24,59] := 0.713517869; GammaY[25,59] := 0.7233255; GammaY[26,59] := 0.733022921; GammaY[27,59] := 0.742622083; GammaY[28,59] := 0.752133966; GammaY[29,59] := 0.761568707;
  //GammaY[30,59] := 0.770935693; GammaY[31,59] := 0.780243687; GammaY[32,59] := 0.789500908; GammaY[33,59] := 0.798715105; GammaY[34,59] := 0.807893619; GammaY[35,59] := 0.817043443; GammaY[36,59] := 0.826171256; GammaY[37,59] := 0.835283508; GammaY[38,59] := 0.844386434; GammaY[39,59] := 0.853486072;
  //GammaY[40,59] := 0.862588322; GammaY[41,59] := 0.871698974; GammaY[42,59] := 0.880823734; GammaY[43,59] := 0.889968249; GammaY[44,59] := 0.899138137; GammaY[45,59] := 0.908338993; GammaY[46,59] := 0.917576462; GammaY[47,59] := 0.926856236; GammaY[48,59] := 0.936184045; GammaY[49,59] := 0.945565723;
  //GammaY[50,59] := 0.955007224; GammaY[51,59] := 0.964514647; GammaY[52,59] := 0.974094259; GammaY[53,59] := 0.983752531; GammaY[54,59] := 0.993496137; GammaY[55,59] := 1.003332049; GammaY[56,59] := 1.013267557; GammaY[57,59] := 1.023310245; GammaY[58,59] := 1.03346808; GammaY[59,59] := 1.04374946;
  //GammaY[60,59] := 1.054163257; GammaY[61,59] := 1.064718875; GammaY[62,59] := 1.075426313; GammaY[63,59] := 1.086296234; GammaY[64,59] := 1.097340047; GammaY[65,59] := 1.108569999; GammaY[66,59] := 1.119999275; GammaY[67,59] := 1.13164212; GammaY[68,59] := 1.143513973; GammaY[69,59] := 1.155631626;
  //GammaY[70,59] := 1.168013407; GammaY[71,59] := 1.180679395; GammaY[72,59] := 1.193651669; GammaY[73,59] := 1.206954607; GammaY[74,59] := 1.220615238; GammaY[75,59] := 1.234663668; GammaY[76,59] := 1.249133585; GammaY[77,59] := 1.264062883; GammaY[78,59] := 1.279494417; GammaY[79,59] := 1.295476941;
  //GammaY[80,59] := 1.312066276; GammaY[81,59] := 1.329326784; GammaY[82,59] := 1.347333248; GammaY[83,59] := 1.366173297; GammaY[84,59] := 1.38595059; GammaY[85,59] := 1.40678905; GammaY[86,59] := 1.42883861; GammaY[87,59] := 1.45228315; GammaY[88,59] := 1.47735175; GammaY[89,59] := 1.504335059;
  //GammaY[90,59] := 1.533609866; GammaY[91,59] := 1.565677425; GammaY[92,59] := 1.601226006; GammaY[93,59] := 1.641238933; GammaY[94,59] := 1.687195174; GammaY[95,59] := 1.741479298; GammaY[96,59] := 1.808340604; GammaY[97,59] := 1.896659001; GammaY[98,59] := 2.031727978; GammaY[99,59] := 2.325056986;
  //GammaY[0,60] := 0.291547841; GammaY[1,60] := 0.365847782; GammaY[2,60] := 0.408432362; GammaY[3,60] := 0.439535027; GammaY[4,60] := 0.464937764; GammaY[5,60] := 0.486827348; GammaY[6,60] := 0.506303462; GammaY[7,60] := 0.52400727; GammaY[8,60] := 0.540349174; GammaY[9,60] := 0.555609515;
  //GammaY[10,60] := 0.569989262; GammaY[11,60] := 0.583637963; GammaY[12,60] := 0.596670264; GammaY[13,60] := 0.60917625; GammaY[14,60] := 0.621228163; GammaY[15,60] := 0.632884959; GammaY[16,60] := 0.644195496; GammaY[17,60] := 0.655200799; GammaY[18,60] := 0.665935712; GammaY[19,60] := 0.676430152;
  //GammaY[20,60] := 0.686710054; GammaY[21,60] := 0.696798092; GammaY[22,60] := 0.706714245; GammaY[23,60] := 0.716476269; GammaY[24,60] := 0.726100044; GammaY[25,60] := 0.735599854; GammaY[26,60] := 0.744988642; GammaY[27,60] := 0.754278207; GammaY[28,60] := 0.763479362; GammaY[29,60] := 0.772602078;
  //GammaY[30,60] := 0.781655593; GammaY[31,60] := 0.790648508; GammaY[32,60] := 0.799588896; GammaY[33,60] := 0.808484366; GammaY[34,60] := 0.817342094; GammaY[35,60] := 0.826168919; GammaY[36,60] := 0.834971376; GammaY[37,60] := 0.843755746; GammaY[38,60] := 0.85252809; GammaY[39,60] := 0.861294291;
  //GammaY[40,60] := 0.870060066; GammaY[41,60] := 0.878831044; GammaY[42,60] := 0.887612766; GammaY[43,60] := 0.896410685; GammaY[44,60] := 0.905230226; GammaY[45,60] := 0.914076805; GammaY[46,60] := 0.922955847; GammaY[47,60] := 0.931872815; GammaY[48,60] := 0.940833228; GammaY[49,60] := 0.949842668;
  //GammaY[50,60] := 0.958906861; GammaY[51,60] := 0.968031666; GammaY[52,60] := 0.977223066; GammaY[53,60] := 0.986487243; GammaY[54,60] := 0.995830576; GammaY[55,60] := 1.00525972; GammaY[56,60] := 1.014781616; GammaY[57,60] := 1.024403485; GammaY[58,60] := 1.034132915; GammaY[59,60] := 1.043977893;
  //GammaY[60,60] := 1.053946854; GammaY[61,60] := 1.064048731; GammaY[62,60] := 1.074293017; GammaY[63,60] := 1.084689831; GammaY[64,60] := 1.095249994; GammaY[65,60] := 1.105985117; GammaY[66,60] := 1.116907695; GammaY[67,60] := 1.128031223; GammaY[68,60] := 1.139370321; GammaY[69,60] := 1.150940885;
  //GammaY[70,60] := 1.162760261; GammaY[71,60] := 1.174847443; GammaY[72,60] := 1.187223316; GammaY[73,60] := 1.199910931; GammaY[74,60] := 1.21293584; GammaY[75,60] := 1.226326499; GammaY[76,60] := 1.240114741; GammaY[77,60] := 1.254336367; GammaY[78,60] := 1.269031857; GammaY[79,60] := 1.284247254;
  //GammaY[80,60] := 1.300035267; GammaY[81,60] := 1.316456658; GammaY[82,60] := 1.333582015; GammaY[83,60] := 1.351494037; GammaY[84,60] := 1.370290535; GammaY[85,60] := 1.390088421; GammaY[86,60] := 1.411029118; GammaY[87,60] := 1.433286042; GammaY[88,60] := 1.457075196; GammaY[89,60] := 1.48267058;
  //GammaY[90,60] := 1.510427331; GammaY[91,60] := 1.540817796; GammaY[92,60] := 1.574490398; GammaY[93,60] := 1.612371293; GammaY[94,60] := 1.655853063; GammaY[95,60] := 1.707180253; GammaY[96,60] := 1.770351071; GammaY[97,60] := 1.85371658; GammaY[98,60] := 1.981046984; GammaY[99,60] := 2.256846699;
  //GammaY[0,61] := 0.311128291; GammaY[1,61] := 0.385396429; GammaY[2,61] := 0.427651314; GammaY[3,61] := 0.458384792; GammaY[4,61] := 0.483417038; GammaY[5,61] := 0.504942251; GammaY[6,61] := 0.524061548; GammaY[7,61] := 0.541415924; GammaY[8,61] := 0.557415151; GammaY[9,61] := 0.572338832;
  //GammaY[10,61] := 0.586387212; GammaY[11,61] := 0.599709166; GammaY[12,61] := 0.612418749; GammaY[13,61] := 0.624605514; GammaY[14,61] := 0.636341222; GammaY[15,61] := 0.647684405; GammaY[16,61] := 0.658683528; GammaY[17,61] := 0.669379261; GammaY[18,61] := 0.679806124; GammaY[19,61] := 0.689993748;
  //GammaY[20,61] := 0.699967803; GammaY[21,61] := 0.709750708; GammaY[22,61] := 0.719362214; GammaY[23,61] := 0.72881985; GammaY[24,61] := 0.73813928; GammaY[25,61] := 0.747334597; GammaY[26,61] := 0.756418545; GammaY[27,61] := 0.765402747; GammaY[28,61] := 0.774297851; GammaY[29,61] := 0.783113647;
  //GammaY[30,61] := 0.791859208; GammaY[31,61] := 0.800542979; GammaY[32,61] := 0.809172866; GammaY[33,61] := 0.817756303; GammaY[34,61] := 0.826300306; GammaY[35,61] := 0.834811564; GammaY[36,61] := 0.843296464; GammaY[37,61] := 0.851761113; GammaY[38,61] := 0.860211413; GammaY[39,61] := 0.86865308;
  //GammaY[40,61] := 0.877091678; GammaY[41,61] := 0.885532653; GammaY[42,61] := 0.893981354; GammaY[43,61] := 0.902443051; GammaY[44,61] := 0.910922995; GammaY[45,61] := 0.919426426; GammaY[46,61] := 0.927958557; GammaY[47,61] := 0.936524643; GammaY[48,61] := 0.945129987; GammaY[49,61] := 0.953779969;
  //GammaY[50,61] := 0.96248006; GammaY[51,61] := 0.971235857; GammaY[52,61] := 0.980053076; GammaY[53,61] := 0.988937645; GammaY[54,61] := 0.997895685; GammaY[55,61] := 1.006933523; GammaY[56,61] := 1.016057771; GammaY[57,61] := 1.025275311; GammaY[58,61] := 1.034593368; GammaY[59,61] := 1.044019544;
  //GammaY[60,61] := 1.053561855; GammaY[61,61] := 1.06322879; GammaY[62,61] := 1.073029366; GammaY[63,61] := 1.082973188; GammaY[64,61] := 1.093070523; GammaY[65,61] := 1.103332382; GammaY[66,61] := 1.11377061; GammaY[67,61] := 1.124397995; GammaY[68,61] := 1.135228386; GammaY[69,61] := 1.146276838;
  //GammaY[70,61] := 1.157559771; GammaY[71,61] := 1.169095162; GammaY[72,61] := 1.180902771; GammaY[73,61] := 1.193004404; GammaY[74,61] := 1.205424227; GammaY[75,61] := 1.218189143; GammaY[76,61] := 1.231329248; GammaY[77,61] := 1.244878377; GammaY[78,61] := 1.258874781; GammaY[79,61] := 1.273361963;
  //GammaY[80,61] := 1.288389711; GammaY[81,61] := 1.304015413; GammaY[82,61] := 1.320305725; GammaY[83,61] := 1.337338726; GammaY[84,61] := 1.355206749; GammaY[85,61] := 1.374020136; GammaY[86,61] := 1.393912344; GammaY[87,61] := 1.415046988; GammaY[88,61] := 1.43762783; GammaY[89,61] := 1.461913292;
  //GammaY[90,61] := 1.488238253; GammaY[91,61] := 1.517048018; GammaY[92,61] := 1.548953752; GammaY[93,61] := 1.58482817; GammaY[94,61] := 1.625983129; GammaY[95,61] := 1.674532379; GammaY[96,61] := 1.734239692; GammaY[97,61] := 1.812962655; GammaY[98,61] := 1.933051214; GammaY[99,61] := 2.192489852;
  //GammaY[0,62] := 0.33068931; GammaY[1,62] := 0.404717442; GammaY[2,62] := 0.446550366; GammaY[3,62] := 0.47685944; GammaY[4,62] := 0.501482945; GammaY[5,62] := 0.522615436; GammaY[6,62] := 0.541356105; GammaY[7,62] := 0.558343892; GammaY[8,62] := 0.573986768; GammaY[9,62] := 0.588562828;
  //GammaY[10,62] := 0.602271066; GammaY[11,62] := 0.615259319; GammaY[12,62] := 0.627640749; GammaY[13,62] := 0.639504148; GammaY[14,62] := 0.650920625; GammaY[15,62] := 0.66194815; GammaY[16,62] := 0.672634698; GammaY[17,62] := 0.683020492; GammaY[18,62] := 0.693139667; GammaY[19,62] := 0.703021493;
  //GammaY[20,62] := 0.71269131; GammaY[21,62] := 0.72217124; GammaY[22,62] := 0.73148077; GammaY[23,62] := 0.740637184; GammaY[24,62] := 0.749655905; GammaY[25,62] := 0.758550802; GammaY[26,62] := 0.767334422; GammaY[27,62] := 0.776018179; GammaY[28,62] := 0.784612522; GammaY[29,62] := 0.793127055;
  //GammaY[30,62] := 0.80157068; GammaY[31,62] := 0.809951682; GammaY[32,62] := 0.818277787; GammaY[33,62] := 0.826556263; GammaY[34,62] := 0.834793975; GammaY[35,62] := 0.842997436; GammaY[36,62] := 0.851172862; GammaY[37,62] := 0.859326208; GammaY[38,62] := 0.867463196; GammaY[39,62] := 0.875589389;
  //GammaY[40,62] := 0.8837102; GammaY[41,62] := 0.891830891; GammaY[42,62] := 0.89995664; GammaY[43,62] := 0.908092552; GammaY[44,62] := 0.916243688; GammaY[45,62] := 0.924415083; GammaY[46,62] := 0.93261177; GammaY[47,62] := 0.940838783; GammaY[48,62] := 0.949101237; GammaY[49,62] := 0.957404312;
  //GammaY[50,62] := 0.965753239; GammaY[51,62] := 0.974153376; GammaY[52,62] := 0.982610216; GammaY[53,62] := 0.991129408; GammaY[54,62] := 0.999716762; GammaY[55,62] := 1.008378347; GammaY[56,62] := 1.017120483; GammaY[57,62] := 1.025949716; GammaY[58,62] := 1.034872925; GammaY[59,62] := 1.043897343;
  //GammaY[60,62] := 1.053030595; GammaY[61,62] := 1.06228075; GammaY[62,62] := 1.071656372; GammaY[63,62] := 1.081166582; GammaY[64,62] := 1.090821123; GammaY[65,62] := 1.100630442; GammaY[66,62] := 1.110605771; GammaY[67,62] := 1.120759232; GammaY[68,62] := 1.13110395; GammaY[69,62] := 1.141654184;
  //GammaY[70,62] := 1.152425486; GammaY[71,62] := 1.163434876; GammaY[72,62] := 1.174701057; GammaY[73,62] := 1.186244664; GammaY[74,62] := 1.198088559; GammaY[75,62] := 1.210258192; GammaY[76,62] := 1.222782021; GammaY[77,62] := 1.235692038; GammaY[78,62] := 1.249024405; GammaY[79,62] := 1.262820237;
  //GammaY[80,62] := 1.277126584; GammaY[81,62] := 1.291997669; GammaY[82,62] := 1.30749646; GammaY[83,62] := 1.323696707; GammaY[84,62] := 1.340685604; GammaY[85,62] := 1.358567339; GammaY[86,62] := 1.377467904; GammaY[87,62] := 1.397541735; GammaY[88,62] := 1.418981129; GammaY[89,62] := 1.442029921;
  //GammaY[90,62] := 1.467004027; GammaY[91,62] := 1.494323449; GammaY[92,62] := 1.524564492; GammaY[93,62] := 1.558549892; GammaY[94,62] := 1.597516034; GammaY[95,62] := 1.643454431; GammaY[96,62] := 1.699909861; GammaY[97,62] := 1.7742792; GammaY[98,62] := 1.887587525; GammaY[99,62] := 2.131748724;
  //GammaY[0,63] := 0.350174255; GammaY[1,63] := 0.42377306; GammaY[2,63] := 0.465102001; GammaY[3,63] := 0.494938431; GammaY[4,63] := 0.519120289; GammaY[5,63] := 0.539836056; GammaY[6,63] := 0.558179928; GammaY[7,63] := 0.574787095; GammaY[8,63] := 0.590062693; GammaY[9,63] := 0.604282625;
  //GammaY[10,63] := 0.617644154; GammaY[11,63] := 0.630293711; GammaY[12,63] := 0.642343325; GammaY[13,63] := 0.653880844; GammaY[14,63] := 0.664976576; GammaY[15,63] := 0.675687801; GammaY[16,63] := 0.686061894; GammaY[17,63] := 0.696138555; GammaY[18,63] := 0.705951466; GammaY[19,63] := 0.715529496;
  //GammaY[20,63] := 0.724897612; GammaY[21,63] := 0.734077611; GammaY[22,63] := 0.743088669; GammaY[23,63] := 0.751947783; GammaY[24,63] := 0.760670115; GammaY[25,63] := 0.7692693; GammaY[26,63] := 0.777757666; GammaY[27,63] := 0.786146408; GammaY[28,63] := 0.794445771; GammaY[29,63] := 0.802665174;
  //GammaY[30,63] := 0.810813326; GammaY[31,63] := 0.818898321; GammaY[32,63] := 0.826927718; GammaY[33,63] := 0.834908601; GammaY[34,63] := 0.842847677; GammaY[35,63] := 0.850751309; GammaY[36,63] := 0.858625536; GammaY[37,63] := 0.866476149; GammaY[38,63] := 0.874308721; GammaY[39,63] := 0.88212864;
  //GammaY[40,63] := 0.889941139; GammaY[41,63] := 0.897751309; GammaY[42,63] := 0.90556417; GammaY[43,63] := 0.913384672; GammaY[44,63] := 0.921217682; GammaY[45,63] := 0.929068054; GammaY[46,63] := 0.936940635; GammaY[47,63] := 0.944840284; GammaY[48,63] := 0.952771899; GammaY[49,63] := 0.960740415;
  //GammaY[50,63] := 0.968750878; GammaY[51,63] := 0.976808438; GammaY[52,63] := 0.984918335; GammaY[53,63] := 0.993085972; GammaY[54,63] := 1.001316902; GammaY[55,63] := 1.009616916; GammaY[56,63] := 1.017992033; GammaY[57,63] := 1.026448497; GammaY[58,63] := 1.034992861; GammaY[59,63] := 1.043632008;
  //GammaY[60,63] := 1.052373192; GammaY[61,63] := 1.061224084; GammaY[62,63] := 1.070192822; GammaY[63,63] := 1.079288069; GammaY[64,63] := 1.088519074; GammaY[65,63] := 1.09789575; GammaY[66,63] := 1.107428753; GammaY[67,63] := 1.117129578; GammaY[68,63] := 1.127010664; GammaY[69,63] := 1.137085525;
  //GammaY[70,63] := 1.147368894; GammaY[71,63] := 1.157876891; GammaY[72,63] := 1.168627225; GammaY[73,63] := 1.17963943; GammaY[74,63] := 1.190935143; GammaY[75,63] := 1.202538445; GammaY[76,63] := 1.21447626; GammaY[77,63] := 1.226778847; GammaY[78,63] := 1.239480404; GammaY[79,63] := 1.252619807;
  //GammaY[80,63] := 1.266241537; GammaY[81,63] := 1.280396845; GammaY[82,63] := 1.295145241; GammaY[83,63] := 1.310556409; GammaY[84,63] := 1.32671273; GammaY[85,63] := 1.343712622; GammaY[86,63] := 1.361675075; GammaY[87,63] := 1.380745927; GammaY[88,63] := 1.401106728; GammaY[89,63] := 1.422987653;
  //GammaY[90,63] := 1.446686857; GammaY[91,63] := 1.472600651; GammaY[92,63] := 1.501272712; GammaY[93,63] := 1.533479008; GammaY[94,63] := 1.57038533; GammaY[95,63] := 1.613868893; GammaY[96,63] := 1.667269805; GammaY[97,63] := 1.737554603; GammaY[98,63] := 1.844511831; GammaY[99,63] := 2.074401221;
  //GammaY[0,64] := 0.369531949; GammaY[1,64] := 0.442530476; GammaY[2,64] := 0.483283083; GammaY[3,64] := 0.512605126; GammaY[4,64] := 0.536317369; GammaY[5,64] := 0.556596378; GammaY[6,64] := 0.574528621; GammaY[7,64] := 0.590744006; GammaY[8,64] := 0.605643887; GammaY[9,64] := 0.619501369;
  //GammaY[10,64] := 0.632511563; GammaY[11,64] := 0.644819208; GammaY[12,64] := 0.656534962; GammaY[13,64] := 0.667745539; GammaY[14,64] := 0.67852032; GammaY[15,64] := 0.688915798; GammaY[16,64] := 0.698978665; GammaY[17,64] := 0.708748041; GammaY[18,64] := 0.718257087; GammaY[19,64] := 0.727534209;
  //GammaY[20,64] := 0.736603965; GammaY[21,64] := 0.745487791; GammaY[22,64] := 0.754204542; GammaY[23,64] := 0.762770904; GammaY[24,64] := 0.77120177; GammaY[25,64] := 0.779510508; GammaY[26,64] := 0.787709199; GammaY[27,64] := 0.795808807; GammaY[28,64] := 0.803819372; GammaY[29,64] := 0.811750122;
  //GammaY[30,64] := 0.819609562; GammaY[31,64] := 0.8274056; GammaY[32,64] := 0.835145615; GammaY[33,64] := 0.842836531; GammaY[34,64] := 0.850484873; GammaY[35,64] := 0.85809681; GammaY[36,64] := 0.865678237; GammaY[37,64] := 0.873234795; GammaY[38,64] := 0.880771884; GammaY[39,64] := 0.888294727;
  //GammaY[40,64] := 0.895808395; GammaY[41,64] := 0.903317829; GammaY[42,64] := 0.91082787; GammaY[43,64] := 0.918343282; GammaY[44,64] := 0.925868756; GammaY[45,64] := 0.933408983; GammaY[46,64] := 0.940968645; GammaY[47,64] := 0.948552402; GammaY[48,64] := 0.956164956; GammaY[49,64] := 0.963811061;
  //GammaY[50,64] := 0.97149554; GammaY[51,64] := 0.97922331; GammaY[52,64] := 0.986999377; GammaY[53,64] := 0.994828923; GammaY[54,64] := 1.002717283; GammaY[55,64] := 1.010669959; GammaY[56,64] := 1.01869269; GammaY[57,64] := 1.026791431; GammaY[58,64] := 1.034972425; GammaY[59,64] := 1.043242225;
  //GammaY[60,64] := 1.051607731; GammaY[61,64] := 1.06007624; GammaY[62,64] := 1.068655485; GammaY[63,64] := 1.077353696; GammaY[64,64] := 1.086179659; GammaY[65,64] := 1.095142782; GammaY[66,64] := 1.104253175; GammaY[67,64] := 1.113521741; GammaY[68,64] := 1.122960277; GammaY[69,64] := 1.132581594;
  //GammaY[70,64] := 1.142399652; GammaY[71,64] := 1.152429725; GammaY[72,64] := 1.162688587; GammaY[73,64] := 1.173194735; GammaY[74,64] := 1.183968655; GammaY[75,64] := 1.195033141; GammaY[76,64] := 1.206413675; GammaY[77,64] := 1.21813889; GammaY[78,64] := 1.230241137; GammaY[79,64] := 1.242757189;
  //GammaY[80,64] := 1.255729113; GammaY[81,64] := 1.269205372; GammaY[82,64] := 1.283242228; GammaY[83,64] := 1.297905555; GammaY[84,64] := 1.313273213; GammaY[85,64] := 1.329438207; GammaY[86,64] := 1.346512965; GammaY[87,64] := 1.364635257; GammaY[88,64] := 1.383976566; GammaY[89,64] := 1.404754259;
  //GammaY[90,64] := 1.427249851; GammaY[91,64] := 1.451837462; GammaY[92,64] := 1.479030216; GammaY[93,64] := 1.509560295; GammaY[94,64] := 1.544527421; GammaY[95,64] := 1.585701887; GammaY[96,64] := 1.636232413; GammaY[97,64] := 1.702683366; GammaY[98,64] := 1.803688602; GammaY[99,64] := 2.020239377;
  //GammaY[0,65] := 0.388716493; GammaY[1,65] := 0.46096146; GammaY[2,65] := 0.501074468; GammaY[3,65] := 0.529846405; GammaY[4,65] := 0.553065599; GammaY[5,65] := 0.572891466; GammaY[6,65] := 0.590400262; GammaY[7,65] := 0.606215267; GammaY[8,65] := 0.620733241; GammaY[9,65] := 0.634223937;
  //GammaY[10,65] := 0.64687993; GammaY[11,65] := 0.658843999; GammaY[12,65] := 0.670225246; GammaY[13,65] := 0.681109113; GammaY[14,65] := 0.691563925; GammaY[15,65] := 0.701645286; GammaY[16,65] := 0.711399132; GammaY[17,65] := 0.72086394; GammaY[18,65] := 0.730072317; GammaY[19,65] := 0.739052164;
  //GammaY[20,65] := 0.747827609; GammaY[21,65] := 0.756419688; GammaY[22,65] := 0.764846883; GammaY[23,65] := 0.773125571; GammaY[24,65] := 0.781270357; GammaY[25,65] := 0.789294331; GammaY[26,65] := 0.79720932; GammaY[27,65] := 0.805026062; GammaY[28,65] := 0.812754368; GammaY[29,65] := 0.820403244;
  //GammaY[30,65] := 0.827980995; GammaY[31,65] := 0.835495347; GammaY[32,65] := 0.842953507; GammaY[33,65] := 0.85036221; GammaY[34,65] := 0.857727811; GammaY[35,65] := 0.865056321; GammaY[36,65] := 0.872353461; GammaY[37,65] := 0.879624695; GammaY[38,65] := 0.886875257; GammaY[39,65] := 0.894110223;
  //GammaY[40,65] := 0.901334514; GammaY[41,65] := 0.908552899; GammaY[42,65] := 0.915770054; GammaY[43,65] := 0.922990578; GammaY[44,65] := 0.930219012; GammaY[45,65] := 0.937459859; GammaY[46,65] := 0.944717592; GammaY[47,65] := 0.951996719; GammaY[48,65] := 0.959301773; GammaY[49,65] := 0.966637302;
  //GammaY[50,65] := 0.974007928; GammaY[51,65] := 0.981418361; GammaY[52,65] := 0.988873412; GammaY[53,65] := 0.996377991; GammaY[54,65] := 1.003937209; GammaY[55,65] := 1.011556357; GammaY[56,65] := 1.01924087; GammaY[57,65] := 1.026996439; GammaY[58,65] := 1.034829011; GammaY[59,65] := 1.042744825;
  //GammaY[60,65] := 1.050750448; GammaY[61,65] := 1.058852819; GammaY[62,65] := 1.067059294; GammaY[63,65] := 1.075377691; GammaY[64,65] := 1.083816356; GammaY[65,65] := 1.092384223; GammaY[66,65] := 1.101090886; GammaY[67,65] := 1.109946693; GammaY[68,65] := 1.118962832; GammaY[69,65] := 1.12815145;
  //GammaY[70,65] := 1.137525784; GammaY[71,65] := 1.147100307; GammaY[72,65] := 1.156890914; GammaY[73,65] := 1.166915127; GammaY[74,65] := 1.177192351; GammaY[75,65] := 1.187744167; GammaY[76,65] := 1.198594701; GammaY[77,65] := 1.209771055; GammaY[78,65] := 1.221303847; GammaY[79,65] := 1.233227874;
  //GammaY[80,65] := 1.245582937; GammaY[81,65] := 1.258414876; GammaY[82,65] := 1.271776906; GammaY[83,65] := 1.285731325; GammaY[84,65] := 1.300351747; GammaY[85,65] := 1.315726098; GammaY[86,65] := 1.331960649; GammaY[87,65] := 1.349185602; GammaY[88,65] := 1.367562996; GammaY[89,65] := 1.387298191;
  //GammaY[90,65] := 1.408657105; GammaY[91,65] := 1.431993057; GammaY[92,65] := 1.457790543; GammaY[93,65] := 1.486740738; GammaY[94,65] := 1.519881503; GammaY[95,65] := 1.558883049; GammaY[96,65] := 1.606715044; GammaY[97,65] := 1.669565806; GammaY[98,65] := 1.764990374; GammaY[99,65] := 1.969069024;
  //GammaY[0,66] := 0.407687029; GammaY[1,66] := 0.479042016; GammaY[2,66] := 0.518460629; GammaY[3,66] := 0.546652295; GammaY[4,66] := 0.569359161; GammaY[5,66] := 0.588718795; GammaY[6,66] := 0.605795078; GammaY[7,66] := 0.621203442; GammaY[8,66] := 0.635335316; GammaY[9,66] := 0.648456638;
  //GammaY[10,66] := 0.660757132; GammaY[11,66] := 0.672377377; GammaY[12,66] := 0.683424724; GammaY[13,66] := 0.693983224; GammaY[14,66] := 0.704120054; GammaY[15,66] := 0.71388985; GammaY[16,66] := 0.723337741; GammaY[17,66] := 0.732501501; GammaY[18,66] := 0.741413112; GammaY[19,66] := 0.75009996;
  //GammaY[20,66] := 0.758585711; GammaY[21,66] := 0.766890978; GammaY[22,66] := 0.775033874; GammaY[23,66] := 0.783030433; GammaY[24,66] := 0.79089494; GammaY[25,66] := 0.798640198; GammaY[26,66] := 0.806277778; GammaY[27,66] := 0.813818187; GammaY[28,66] := 0.821270997; GammaY[29,66] := 0.828644998;
  //GammaY[30,66] := 0.835948304; GammaY[31,66] := 0.843188436; GammaY[32,66] := 0.850372393; GammaY[33,66] := 0.85750675; GammaY[34,66] := 0.864597698; GammaY[35,66] := 0.871651073; GammaY[36,66] := 0.878672427; GammaY[37,66] := 0.885667064; GammaY[38,66] := 0.892640071; GammaY[39,66] := 0.899596352;
  //GammaY[40,66] := 0.906540642; GammaY[41,66] := 0.913477573; GammaY[42,66] := 0.920411678; GammaY[43,66] := 0.927347381; GammaY[44,66] := 0.934289059; GammaY[45,66] := 0.941241049; GammaY[46,66] := 0.948207671; GammaY[47,66] := 0.955193244; GammaY[48,66] := 0.962202086; GammaY[49,66] := 0.96923859;
  //GammaY[50,66] := 0.976307206; GammaY[51,66] := 0.983412427; GammaY[52,66] := 0.990558858; GammaY[53,66] := 0.9977512; GammaY[54,66] := 1.004994332; GammaY[55,66] := 1.0122933; GammaY[56,66] := 1.019653296; GammaY[57,66] := 1.027079744; GammaY[58,66] := 1.034578312; GammaY[59,66] := 1.042154942;
  //GammaY[60,66] := 1.049815887; GammaY[61,66] := 1.057567748; GammaY[62,66] := 1.065417519; GammaY[63,66] := 1.073372631; GammaY[64,66] := 1.081441015; GammaY[65,66] := 1.089631154; GammaY[66,66] := 1.097952161; GammaY[67,66] := 1.106413856; GammaY[68,66] := 1.115026853; GammaY[69,66] := 1.123802673;
  //GammaY[70,66] := 1.13275387; GammaY[71,66] := 1.141894167; GammaY[72,66] := 1.151238628; GammaY[73,66] := 1.160803859; GammaY[74,66] := 1.170608243; GammaY[75,66] := 1.180672226; GammaY[76,66] := 1.191018654; GammaY[77,66] := 1.201673191; GammaY[78,66] := 1.212664827; GammaY[79,66] := 1.224026499;
  //GammaY[80,66] := 1.235795876; GammaY[81,66] := 1.248016341; GammaY[82,66] := 1.260738242; GammaY[83,66] := 1.274020511; GammaY[84,66] := 1.287932785; GammaY[85,66] := 1.302558216; GammaY[86,66] := 1.317997294; GammaY[87,66] := 1.334373124; GammaY[88,66] := 1.351838881; GammaY[89,66] := 1.370588661;
  //GammaY[90,66] := 1.390873752; GammaY[91,66] := 1.413027971; GammaY[92,66] := 1.437508978; GammaY[93,66] := 1.46496952; GammaY[94,66] := 1.496389502; GammaY[95,66] := 1.533345418; GammaY[96,66] := 1.578639336; GammaY[97,66] := 1.638107769; GammaY[98,66] := 1.728297286; GammaY[99,66] := 1.92070855;
  //GammaY[0,67] := 0.42640747; GammaY[1,67] := 0.496752016; GammaY[2,67] := 0.535429303; GammaY[3,67] := 0.563015634; GammaY[4,67] := 0.58519468; GammaY[5,67] := 0.604078004; GammaY[6,67] := 0.620715162; GammaY[7,67] := 0.635712712; GammaY[8,67] := 0.649456107; GammaY[9,67] := 0.662207038;
  //GammaY[10,67] := 0.674152102; GammaY[11,67] := 0.685429484; GammaY[12,67] := 0.696144652; GammaY[13,67] := 0.706380139; GammaY[14,67] := 0.716201869; GammaY[15,67] := 0.725663458; GammaY[16,67] := 0.734809174; GammaY[17,67] := 0.743676042; GammaY[18,67] := 0.752295411; GammaY[19,67] := 0.760694104;
  //GammaY[20,67] := 0.768895276; GammaY[21,67] := 0.776919117; GammaY[22,67] := 0.784783358; GammaY[23,67] := 0.792503669; GammaY[24,67] := 0.800094018; GammaY[25,67] := 0.807566922; GammaY[26,67] := 0.814933675; GammaY[27,67] := 0.822204511; GammaY[28,67] := 0.829388785; GammaY[29,67] := 0.83649508;
  //GammaY[30,67] := 0.843531292; GammaY[31,67] := 0.850504746; GammaY[32,67] := 0.857422265; GammaY[33,67] := 0.864290234; GammaY[34,67] := 0.871114646; GammaY[35,67] := 0.877901189; GammaY[36,67] := 0.884655262; GammaY[37,67] := 0.891381997; GammaY[38,67] := 0.898086324; GammaY[39,67] := 0.904772989;
  //GammaY[40,67] := 0.911446585; GammaY[41,67] := 0.918111577; GammaY[42,67] := 0.92477231; GammaY[43,67] := 0.931433078; GammaY[44,67] := 0.938098112; GammaY[45,67] := 0.944771577; GammaY[46,67] := 0.951457627; GammaY[47,67] := 0.958160412; GammaY[48,67] := 0.964884097; GammaY[49,67] := 0.971632881;
  //GammaY[50,67] := 0.978410991; GammaY[51,67] := 0.985222764; GammaY[52,67] := 0.992072627; GammaY[53,67] := 0.998965059; GammaY[54,67] := 1.005904719; GammaY[55,67] := 1.012896425; GammaY[56,67] := 1.019945136; GammaY[57,67] := 1.027056021; GammaY[58,67] := 1.034234485; GammaY[59,67] := 1.041486186;
  //GammaY[60,67] := 1.048817079; GammaY[61,67] := 1.056233444; GammaY[62,67] := 1.063741933; GammaY[63,67] := 1.071349615; GammaY[64,67] := 1.079064024; GammaY[65,67] := 1.086893221; GammaY[66,67] := 1.094845859; GammaY[67,67] := 1.10293126; GammaY[68,67] := 1.111159502; GammaY[69,67] := 1.119541516;
  //GammaY[70,67] := 1.128089209; GammaY[71,67] := 1.136815596; GammaY[72,67] := 1.14573496; GammaY[73,67] := 1.154863043; GammaY[74,67] := 1.16421727; GammaY[75,67] := 1.173817012; GammaY[76,67] := 1.183683912; GammaY[77,67] := 1.193842284; GammaY[78,67] := 1.204319582; GammaY[79,67] := 1.215146997;
  //GammaY[80,67] := 1.226360195; GammaY[81,67] := 1.238000244; GammaY[82,67] := 1.250114799; GammaY[83,67] := 1.262759635; GammaY[84,67] := 1.276000643; GammaY[85,67] := 1.289916498; GammaY[86,67] := 1.304602261; GammaY[87,67] := 1.320174367; GammaY[88,67] := 1.336777671; GammaY[89,67] := 1.354595697;
  //GammaY[90,67] := 1.373866007; GammaY[91,67] := 1.394904125; GammaY[92,67] := 1.418142541; GammaY[93,67] := 1.44419797; GammaY[94,67] := 1.473996; GammaY[95,67] := 1.509025315; GammaY[96,67] := 1.551931021; GammaY[97,67] := 1.608220341; GammaY[98,67] := 1.69349664; GammaY[99,67] := 1.874988125;
  //GammaY[0,68] := 0.444846218; GammaY[1,68] := 0.514074873; GammaY[2,68] := 0.551971175; GammaY[3,68] := 0.578931773; GammaY[4,68] := 0.600570938; GammaY[5,68] := 0.618970578; GammaY[6,68] := 0.635164234; GammaY[7,68] := 0.649748652; GammaY[8,68] := 0.663102774; GammaY[9,68] := 0.675483701;
  //GammaY[10,68] := 0.687074641; GammaY[11,68] := 0.698011206; GammaY[12,68] := 0.708396863; GammaY[13,68] := 0.71831253; GammaY[14,68] := 0.72782283; GammaY[15,68] := 0.736980285; GammaY[16,68] := 0.745828233; GammaY[17,68] := 0.754402935; GammaY[18,68] := 0.762735078; GammaY[19,68] := 0.770850893;
  //GammaY[20,68] := 0.778773034; GammaY[21,68] := 0.78652123; GammaY[22,68] := 0.79411279; GammaY[23,68] := 0.801563035; GammaY[24,68] := 0.808885609; GammaY[25,68] := 0.816092724; GammaY[26,68] := 0.823195395; GammaY[27,68] := 0.830203611; GammaY[28,68] := 0.837126481; GammaY[29,68] := 0.843972344;
  //GammaY[30,68] := 0.850748904; GammaY[31,68] := 0.8574633; GammaY[32,68] := 0.864122155; GammaY[33,68] := 0.870731676; GammaY[34,68] := 0.877297693; GammaY[35,68] := 0.883825716; GammaY[36,68] := 0.890320956; GammaY[37,68] := 0.896788407; GammaY[38,68] := 0.903232854; GammaY[39,68] := 0.909658878;
  //GammaY[40,68] := 0.916070919; GammaY[41,68] := 0.92247329; GammaY[42,68] := 0.928870199; GammaY[43,68] := 0.935265772; GammaY[44,68] := 0.941664056; GammaY[45,68] := 0.948069083; GammaY[46,68] := 0.954484868; GammaY[47,68] := 0.960915384; GammaY[48,68] := 0.967364628; GammaY[49,68] := 0.973836629;
  //GammaY[50,68] := 0.980335458; GammaY[51,68] := 0.986865232; GammaY[52,68] := 0.993430185; GammaY[53,68] := 1.00003464; GammaY[54,68] := 1.006683031; GammaY[55,68] := 1.013379956; GammaY[56,68] := 1.020130147; GammaY[57,68] := 1.026938538; GammaY[58,68] := 1.033810279; GammaY[59,68] := 1.040750763;
  //GammaY[60,68] := 1.04776566; GammaY[61,68] := 1.054860947; GammaY[62,68] := 1.062042952; GammaY[63,68] := 1.069318398; GammaY[64,68] := 1.07669445; GammaY[65,68] := 1.084178768; GammaY[66,68] := 1.091779569; GammaY[67,68] := 1.099505708; GammaY[68,68] := 1.107366752; GammaY[69,68] := 1.115373077;
  //GammaY[70,68] := 1.12353598; GammaY[71,68] := 1.131867809; GammaY[72,68] := 1.140382111; GammaY[73,68] := 1.149093814; GammaY[74,68] := 1.158019435; GammaY[75,68] := 1.167177338; GammaY[76,68] := 1.176588039; GammaY[77,68] := 1.186274574; GammaY[78,68] := 1.196262954; GammaY[79,68] := 1.206582726;
  //GammaY[80,68] := 1.217267671; GammaY[81,68] := 1.228356681; GammaY[82,68] := 1.239894877; GammaY[83,68] := 1.251935066; GammaY[84,68] := 1.264539621; GammaY[85,68] := 1.277783004; GammaY[86,68] := 1.291755178; GammaY[87,68] := 1.306566313; GammaY[88,68] := 1.32235345; GammaY[89,68] := 1.339290188;
  //GammaY[90,68] := 1.357601198; GammaY[91,68] := 1.377584834; GammaY[92,68] := 1.39964997; GammaY[93,68] := 1.424379522; GammaY[94,68] := 1.452648143; GammaY[95,68] := 1.485862204; GammaY[96,68] := 1.526519734; GammaY[97,68] := 1.579819576; GammaY[98,68] := 1.660482467; GammaY[99,68] := 1.831748985;
  //GammaY[0,69] := 0.462975863; GammaY[1,69] := 0.530997217; GammaY[2,69] := 0.56807955; GammaY[3,69] := 0.594398267; GammaY[4,69] := 0.615488612; GammaY[5,69] := 0.633399625; GammaY[6,69] := 0.649147384; GammaY[7,69] := 0.663318034; GammaY[8,69] := 0.676283513; GammaY[9,69] := 0.688296031;
  //GammaY[10,69] := 0.699535224; GammaY[11,69] := 0.710133977; GammaY[12,69] := 0.720193629; GammaY[13,69] := 0.729793421; GammaY[14,69] := 0.738996607; GammaY[15,69] := 0.747854573; GammaY[16,69] := 0.756409714; GammaY[17,69] := 0.764697467; GammaY[18,69] := 0.772747826; GammaY[19,69] := 0.780586433;
  //GammaY[20,69] := 0.788235409; GammaY[21,69] := 0.795714015; GammaY[22,69] := 0.803039156; GammaY[23,69] := 0.810225763; GammaY[24,69] := 0.817287143; GammaY[25,69] := 0.824235214; GammaY[26,69] := 0.831080705; GammaY[27,69] := 0.837833345; GammaY[28,69] := 0.844502003; GammaY[29,69] := 0.851094804;
  //GammaY[30,69] := 0.857619222; GammaY[31,69] := 0.864082194; GammaY[32,69] := 0.870490176; GammaY[33,69] := 0.876849183; GammaY[34,69] := 0.883164876; GammaY[35,69] := 0.889442595; GammaY[36,69] := 0.895687404; GammaY[37,69] := 0.901904115; GammaY[38,69] := 0.908097357; GammaY[39,69] := 0.914271583;
  //GammaY[40,69] := 0.920431071; GammaY[41,69] := 0.926579987; GammaY[42,69] := 0.932722388; GammaY[43,69] := 0.938862253; GammaY[44,69] := 0.945003495; GammaY[45,69] := 0.951149964; GammaY[46,69] := 0.957305516; GammaY[47,69] := 0.963474001; GammaY[48,69] := 0.969659241; GammaY[49,69] := 0.9758651;
  //GammaY[50,69] := 0.982095481; GammaY[51,69] := 0.988354346; GammaY[52,69] := 0.99464571; GammaY[53,69] := 1.000973708; GammaY[54,69] := 1.007342612; GammaY[55,69] := 1.013756794; GammaY[56,69] := 1.020220772; GammaY[57,69] := 1.026739254; GammaY[58,69] := 1.033317151; GammaY[59,69] := 1.039959601;
  //GammaY[60,69] := 1.046672003; GammaY[61,69] := 1.053460051; GammaY[62,69] := 1.060329766; GammaY[63,69] := 1.067287542; GammaY[64,69] := 1.07434019; GammaY[65,69] := 1.081494992; GammaY[66,69] := 1.088759759; GammaY[67,69] := 1.0961429; GammaY[68,69] := 1.103653501; GammaY[69,69] := 1.111301412;
  //GammaY[70,69] := 1.119097359; GammaY[71,69] := 1.12705306; GammaY[72,69] := 1.135181368; GammaY[73,69] := 1.14349644; GammaY[74,69] := 1.152013944; GammaY[75,69] := 1.160751291; GammaY[76,69] := 1.169727932; GammaY[77,69] := 1.178965702; GammaY[78,69] := 1.188489255; GammaY[79,69] := 1.198326589;
  //GammaY[80,69] := 1.20850971; GammaY[81,69] := 1.219075464; GammaY[82,69] := 1.230066592; GammaY[83,69] := 1.241533104; GammaY[84,69] := 1.253534068; GammaY[85,69] := 1.266139985; GammaY[86,69] := 1.279436019; GammaY[87,69] := 1.293526456; GammaY[88,69] := 1.308540996; GammaY[89,69] := 1.32464391;
  //GammaY[90,69] := 1.342047766; GammaY[91,69] := 1.361034791; GammaY[92,69] := 1.381991693; GammaY[93,69] := 1.405469663; GammaY[94,69] := 1.432295564; GammaY[95,69] := 1.46379857; GammaY[96,69] := 1.502338829; GammaY[97,69] := 1.552826234; GammaY[98,69] := 1.629155139; GammaY[99,69] := 1.790842719;
  //GammaY[0,70] := 0.480772885; GammaY[1,70] := 0.547508576; GammaY[2,70] := 0.583750087; GammaY[3,70] := 0.609414633; GammaY[4,70] := 0.629950019; GammaY[5,70] := 0.647369655; GammaY[6,70] := 0.662670886; GammaY[7,70] := 0.676428593; GammaY[8,70] := 0.689007323; GammaY[9,70] := 0.700654123;
  //GammaY[10,70] := 0.711544869; GammaY[11,70] := 0.721809621; GammaY[12,70] := 0.731547515; GammaY[13,70] := 0.740836026; GammaY[14,70] := 0.749736987; GammaY[15,70] := 0.758300623; GammaY[16,70] := 0.766568343; GammaY[17,70] := 0.774574762; GammaY[18,70] := 0.782349154; GammaY[19,70] := 0.789916528;
  //GammaY[20,70] := 0.797298478; GammaY[21,70] := 0.804513801; GammaY[22,70] := 0.811578968; GammaY[23,70] := 0.818508542; GammaY[24,70] := 0.825315479; GammaY[25,70] := 0.832011369; GammaY[26,70] := 0.838606671; GammaY[27,70] := 0.845110864; GammaY[28,70] := 0.851532565; GammaY[29,70] := 0.857879675;
  //GammaY[30,70] := 0.864159467; GammaY[31,70] := 0.87037867; GammaY[32,70] := 0.876543529; GammaY[33,70] := 0.882659899; GammaY[34,70] := 0.888733282; GammaY[35,70] := 0.894768842; GammaY[36,70] := 0.900771484; GammaY[37,70] := 0.906745878; GammaY[38,70] := 0.912696489; GammaY[39,70] := 0.918627595;
  //GammaY[40,70] := 0.92454335; GammaY[41,70] := 0.930447789; GammaY[42,70] := 0.936344812; GammaY[43,70] := 0.942238253; GammaY[44,70] := 0.948131878; GammaY[45,70] := 0.95402941; GammaY[46,70] := 0.959934523; GammaY[47,70] := 0.965850914; GammaY[48,70] := 0.971782283; GammaY[49,70] := 0.977732322;
  //GammaY[50,70] := 0.98370477; GammaY[51,70] := 0.989703421; GammaY[52,70] := 0.995732139; GammaY[53,70] := 1.001794838; GammaY[54,70] := 1.007895604; GammaY[55,70] := 1.01403865; GammaY[56,70] := 1.020228271; GammaY[57,70] := 1.026468958; GammaY[58,70] := 1.032765392; GammaY[59,70] := 1.039122476;
  //GammaY[60,70] := 1.04554535; GammaY[61,70] := 1.05203943; GammaY[62,70] := 1.058610453; GammaY[63,70] := 1.065264502; GammaY[64,70] := 1.072008055; GammaY[65,70] := 1.078848036; GammaY[66,70] := 1.085791868; GammaY[67,70] := 1.092847543; GammaY[68,70] := 1.100023692; GammaY[69,70] := 1.10732967;
  //GammaY[70,70] := 1.11477566; GammaY[71,70] := 1.122372783; GammaY[72,70] := 1.130133239; GammaY[73,70] := 1.138070462; GammaY[74,70] := 1.146199313; GammaY[75,70] := 1.154536311; GammaY[76,70] := 1.163099904; GammaY[77,70] := 1.171910794; GammaY[78,70] := 1.180992354; GammaY[79,70] := 1.190371126;
  //GammaY[80,70] := 1.200077446; GammaY[81,70] := 1.21014623; GammaY[82,70] := 1.220617979; GammaY[83,70] := 1.231540074; GammaY[84,70] := 1.242968468; GammaY[85,70] := 1.254969939; GammaY[86,70] := 1.267625146; GammaY[87,70] := 1.281032836; GammaY[88,70] := 1.295315805; GammaY[89,70] := 1.310629556;
  //GammaY[90,70] := 1.327175283; GammaY[91,70] := 1.345220069; GammaY[92,70] := 1.365129795; GammaY[93,70] := 1.387425869; GammaY[94,70] := 1.412890292; GammaY[95,70] := 1.442779792; GammaY[96,70] := 1.479325196; GammaY[97,70] := 1.527165515; GammaY[98,70] := 1.599420973; GammaY[99,70] := 1.752130387;
  //GammaY[0,71] := 0.498217348; GammaY[1,71] := 0.563601113; GammaY[2,71] := 0.598980527; GammaY[3,71] := 0.623982098; GammaY[4,71] := 0.643958906; GammaY[5,71] := 0.660886367; GammaY[6,71] := 0.675742022; GammaY[7,71] := 0.689088917; GammaY[8,71] := 0.701283879; GammaY[9,71] := 0.712568593;
  //GammaY[10,71] := 0.723115026; GammaY[11,71] := 0.733050298; GammaY[12,71] := 0.742471285; GammaY[13,71] := 0.751453656; GammaY[14,71] := 0.760057772; GammaY[15,71] := 0.768332656; GammaY[16,71] := 0.776318724; GammaY[17,71] := 0.784049737; GammaY[18,71] := 0.791554241; GammaY[19,71] := 0.798856629;
  //GammaY[20,71] := 0.805977928; GammaY[21,71] := 0.812936449; GammaY[22,71] := 0.819748253; GammaY[23,71] := 0.826427511; GammaY[24,71] := 0.832986834; GammaY[25,71] := 0.839437508; GammaY[26,71] := 0.845789689; GammaY[27,71] := 0.852052591; GammaY[28,71] := 0.858234607; GammaY[29,71] := 0.864343403;
  //GammaY[30,71] := 0.870386043; GammaY[31,71] := 0.876369058; GammaY[32,71] := 0.882298517; GammaY[33,71] := 0.888180075; GammaY[34,71] := 0.89401906; GammaY[35,71] := 0.8998205; GammaY[36,71] := 0.905589129; GammaY[37,71] := 0.911329465; GammaY[38,71] := 0.917045824; GammaY[39,71] := 0.92274234;
  //GammaY[40,71] := 0.928423023; GammaY[41,71] := 0.934091763; GammaY[42,71] := 0.939752322; GammaY[43,71] := 0.945408392; GammaY[44,71] := 0.951063599; GammaY[45,71] := 0.956721523; GammaY[46,71] := 0.962385697; GammaY[47,71] := 0.96805967; GammaY[48,71] := 0.973746995; GammaY[49,71] := 0.979451209;
  //GammaY[50,71] := 0.985175899; GammaY[51,71] := 0.990924698; GammaY[52,71] := 0.996701303; GammaY[53,71] := 1.002509457; GammaY[54,71] := 1.008353067; GammaY[55,71] := 1.014236159; GammaY[56,71] := 1.020162833; GammaY[57,71] := 1.026137372; GammaY[58,71] := 1.032164244; GammaY[59,71] := 1.038248116;
  //GammaY[60,71] := 1.044393891; GammaY[61,71] := 1.050606731; GammaY[62,71] := 1.056892093; GammaY[63,71] := 1.063255766; GammaY[64,71] := 1.069703914; GammaY[65,71] := 1.076243124; GammaY[66,71] := 1.082880451; GammaY[67,71] := 1.089623487; GammaY[68,71] := 1.096480436; GammaY[69,71] := 1.103460187;
  //GammaY[70,71] := 1.11057241; GammaY[71,71] := 1.117827665; GammaY[72,71] := 1.125237534; GammaY[73,71] := 1.132814768; GammaY[74,71] := 1.140573473; GammaY[75,71] := 1.148529319; GammaY[76,71] := 1.156699805; GammaY[77,71] := 1.165104571; GammaY[78,71] := 1.173765783; GammaY[79,71] := 1.182708609;
  //GammaY[80,71] := 1.191961811; GammaY[81,71] := 1.201558495; GammaY[82,71] := 1.211537048; GammaY[83,71] := 1.221942375; GammaY[84,71] := 1.232827498; GammaY[85,71] := 1.244255693; GammaY[86,71] := 1.256303372; GammaY[87,71] := 1.269064075; GammaY[88,71] := 1.282654107; GammaY[89,71] := 1.297220733;
  //GammaY[90,71] := 1.312954446; GammaY[91,71] := 1.330108089; GammaY[92,71] := 1.349027979; GammaY[93,71] := 1.370207541; GammaY[94,71] := 1.394386643; GammaY[95,71] := 1.422754008; GammaY[96,71] := 1.457419089; GammaY[97,71] := 1.502766825; GammaY[98,71] := 1.571191878; GammaY[99,71] := 1.71548236;
  //GammaY[0,72] := 0.515292629; GammaY[1,72] := 0.579269336; GammaY[2,72] := 0.613770458; GammaY[3,72] := 0.638103399; GammaY[4,72] := 0.657520266; GammaY[5,72] := 0.673956474; GammaY[6,72] := 0.688368899; GammaY[7,72] := 0.701308267; GammaY[8,72] := 0.713123406; GammaY[9,72] := 0.724050474;
  //GammaY[10,72] := 0.734257429; GammaY[11,72] := 0.743868365; GammaY[12,72] := 0.752977825; GammaY[13,72] := 0.761659646; GammaY[14,72] := 0.769972701; GammaY[15,72] := 0.777964772; GammaY[16,72] := 0.785675257; GammaY[17,72] := 0.793137065; GammaY[18,72] := 0.800378001; GammaY[19,72] := 0.807421819;
  //GammaY[20,72] := 0.814289003; GammaY[21,72] := 0.820997358; GammaY[22,72] := 0.827562512; GammaY[23,72] := 0.833998267; GammaY[24,72] := 0.840316878; GammaY[25,72] := 0.846529319; GammaY[26,72] := 0.852645467; GammaY[27,72] := 0.858674255; GammaY[28,72] := 0.864623832; GammaY[29,72] := 0.870501661;
  //GammaY[30,72] := 0.876314585; GammaY[31,72] := 0.882068937; GammaY[32,72] := 0.887770603; GammaY[33,72] := 0.893425062; GammaY[34,72] := 0.899037475; GammaY[35,72] := 0.904612704; GammaY[36,72] := 0.910155333; GammaY[37,72] := 0.915669727; GammaY[38,72] := 0.92116006; GammaY[39,72] := 0.926630334;
  //GammaY[40,72] := 0.932084393; GammaY[41,72] := 0.937525989; GammaY[42,72] := 0.942958773; GammaY[43,72] := 0.948386287; GammaY[44,72] := 0.953812021; GammaY[45,72] := 0.959239417; GammaY[46,72] := 0.964671888; GammaY[47,72] := 0.970112809; GammaY[48,72] := 0.975565589; GammaY[49,72] := 0.981033654;
  //GammaY[50,72] := 0.986520423; GammaY[51,72] := 0.992029377; GammaY[52,72] := 0.997564026; GammaY[53,72] := 1.003128008; GammaY[54,72] := 1.008725053; GammaY[55,72] := 1.014358955; GammaY[56,72] := 1.020033654; GammaY[57,72] := 1.025753239; GammaY[58,72] := 1.031521971; GammaY[59,72] := 1.037344306;
  //GammaY[60,72] := 1.043224912; GammaY[61,72] := 1.049168705; GammaY[62,72] := 1.055180884; GammaY[63,72] := 1.061266958; GammaY[64,72] := 1.067432792; GammaY[65,72] := 1.073684649; GammaY[66,72] := 1.080029243; GammaY[67,72] := 1.086473793; GammaY[68,72] := 1.093026094; GammaY[69,72] := 1.099694592;
  //GammaY[70,72] := 1.106488474; GammaY[71,72] := 1.113417774; GammaY[72,72] := 1.120493485; GammaY[73,72] := 1.127727712; GammaY[74,72] := 1.135133849; GammaY[75,72] := 1.142726769; GammaY[76,72] := 1.150523077; GammaY[77,72] := 1.158541409; GammaY[78,72] := 1.166802794; GammaY[79,72] := 1.175331107;
  //GammaY[80,72] := 1.184153627; GammaY[81,72] := 1.193301743; GammaY[82,72] := 1.202811859; GammaY[83,72] := 1.212726545; GammaY[84,72] := 1.223096069; GammaY[85,72] := 1.233980407; GammaY[86,72] := 1.245451974; GammaY[87,72] := 1.257599409; GammaY[88,72] := 1.270532908; GammaY[89,72] := 1.284391977;
  //GammaY[90,72] := 1.299357055; GammaY[91,72] := 1.315667598; GammaY[92,72] := 1.33365152; GammaY[93,72] := 1.353775944; GammaY[94,72] := 1.376741154; GammaY[95,72] := 1.403671988; GammaY[96,72] := 1.436563949; GammaY[97,72] := 1.479563536; GammaY[98,72] := 1.544385013; GammaY[99,72] := 1.68077696;
  //GammaY[0,73] := 0.531985117; GammaY[1,73] := 0.594509861; GammaY[2,73] := 0.628121098; GammaY[3,73] := 0.651782567; GammaY[4,73] := 0.670640138; GammaY[5,73] := 0.686587566; GammaY[6,73] := 0.700560313; GammaY[7,73] := 0.713096442; GammaY[8,73] := 0.724536548; GammaY[9,73] := 0.735111119;
  //GammaY[10,73] := 0.744984022; GammaY[11,73] := 0.754276276; GammaY[12,73] := 0.763080049; GammaY[13,73] := 0.771467302; GammaY[14,73] := 0.779495397; GammaY[15,73] := 0.787210883; GammaY[16,73] := 0.794652111; GammaY[17,73] := 0.801851119; GammaY[18,73] := 0.80883498; GammaY[19,73] := 0.815626801;
  //GammaY[20,73] := 0.822246508; GammaY[21,73] := 0.828711418; GammaY[22,73] := 0.835036723; GammaY[23,73] := 0.841235836; GammaY[24,73] := 0.847320667; GammaY[25,73] := 0.853301875; GammaY[26,73] := 0.85918905; GammaY[27,73] := 0.864990866; GammaY[28,73] := 0.87071523; GammaY[29,73] := 0.876369378;
  //GammaY[30,73] := 0.881959947; GammaY[31,73] := 0.887493077; GammaY[32,73] := 0.892974467; GammaY[33,73] := 0.898409437; GammaY[34,73] := 0.903802958; GammaY[35,73] := 0.909159734; GammaY[36,73] := 0.914484218; GammaY[37,73] := 0.919780621; GammaY[38,73] := 0.925052969; GammaY[39,73] := 0.930305128;
  //GammaY[40,73] := 0.935540807; GammaY[41,73] := 0.94076362; GammaY[42,73] := 0.945977086; GammaY[43,73] := 0.951184615; GammaY[44,73] := 0.956389565; GammaY[45,73] := 0.961595245; GammaY[46,73] := 0.966804934; GammaY[47,73] := 0.972021873; GammaY[48,73] := 0.977249334; GammaY[49,73] := 0.982490601;
  //GammaY[50,73] := 0.987748951; GammaY[51,73] := 0.993027717; GammaY[52,73] := 0.998330261; GammaY[53,73] := 1.003660058; GammaY[54,73] := 1.009020679; GammaY[55,73] := 1.014415746; GammaY[56,73] := 1.019849023; GammaY[57,73] := 1.025324414; GammaY[58,73] := 1.030845983; GammaY[59,73] := 1.036417976;
  //GammaY[60,73] := 1.042044843; GammaY[61,73] := 1.047731272; GammaY[62,73] := 1.053482212; GammaY[63,73] := 1.059302905; GammaY[64,73] := 1.065198939; GammaY[65,73] := 1.071176274; GammaY[66,73] := 1.077241293; GammaY[67,73] := 1.083400862; GammaY[68,73] := 1.089662391; GammaY[69,73] := 1.096033907;
  //GammaY[70,73] := 1.102524142; GammaY[71,73] := 1.109142625; GammaY[72,73] := 1.115899802; GammaY[73,73] := 1.122807173; GammaY[74,73] := 1.12987745; GammaY[75,73] := 1.13712476; GammaY[76,73] := 1.144564861; GammaY[77,73] := 1.152215438; GammaY[78,73] := 1.160096451; GammaY[79,73] := 1.168230555;
  //GammaY[80,73] := 1.176643629; GammaY[81,73] := 1.185365452; GammaY[82,73] := 1.194430553; GammaY[83,73] := 1.203879307; GammaY[84,73] := 1.21375938; GammaY[85,73] := 1.224127635; GammaY[86,73] := 1.235052731; GammaY[87,73] := 1.246618694; GammaY[88,73] := 1.258929965; GammaY[89,73] := 1.272118751;
  //GammaY[90,73] := 1.286356024; GammaY[91,73] := 1.30186864; GammaY[92,73] := 1.318967212; GammaY[93,73] := 1.338094135; GammaY[94,73] := 1.35991247; GammaY[95,73] := 1.38548702; GammaY[96,73] := 1.416706242; GammaY[97,73] := 1.45749276; GammaY[98,73] := 1.518922458; GammaY[99,73] := 1.647900478;
  //GammaY[0,74] := 0.548283955; GammaY[1,74] := 0.609321186; GammaY[2,74] := 0.642035093; GammaY[3,74] := 0.665024768; GammaY[4,74] := 0.68332546; GammaY[5,74] := 0.698787926; GammaY[6,74] := 0.712325635; GammaY[7,74] := 0.724463673; GammaY[8,74] := 0.735534253; GammaY[9,74] := 0.74576209;
  //GammaY[10,74] := 0.755306887; GammaY[11,74] := 0.764286552; GammaY[12,74] := 0.772790841; GammaY[13,74] := 0.78088982; GammaY[14,74] := 0.788639333; GammaY[15,74] := 0.796084676; GammaY[16,74] := 0.803263166; GammaY[17,74] := 0.81020595; GammaY[18,74] := 0.816939353; GammaY[19,74] := 0.823485853;
  //GammaY[20,74] := 0.829864811; GammaY[21,74] := 0.83609306; GammaY[22,74] := 0.842185342; GammaY[23,74] := 0.848154684; GammaY[24,74] := 0.854012665; GammaY[25,74] := 0.859769621; GammaY[26,74] := 0.86543486; GammaY[27,74] := 0.871016804; GammaY[28,74] := 0.876523102; GammaY[29,74] := 0.881960768;
  //GammaY[30,74] := 0.887336252; GammaY[31,74] := 0.892655491; GammaY[32,74] := 0.897924005; GammaY[33,74] := 0.90314694; GammaY[34,74] := 0.908329107; GammaY[35,74] := 0.913475053; GammaY[36,74] := 0.918589083; GammaY[37,74] := 0.923675262; GammaY[38,74] := 0.928737479; GammaY[39,74] := 0.933779463;
  //GammaY[40,74] := 0.938804789; GammaY[41,74] := 0.943816943; GammaY[42,74] := 0.948819312; GammaY[43,74] := 0.953815179; GammaY[44,74] := 0.958807774; GammaY[45,74] := 0.963800279; GammaY[46,74] := 0.968795842; GammaY[47,74] := 0.973797575; GammaY[48,74] := 0.978808617; GammaY[49,74] := 0.98383212;
  //GammaY[50,74] := 0.988871222; GammaY[51,74] := 0.993929115; GammaY[52,74] := 0.999009011; GammaY[53,74] := 1.004114241; GammaY[54,74] := 1.009248223; GammaY[55,74] := 1.014414418; GammaY[56,74] := 1.019616416; GammaY[57,74] := 1.024857938; GammaY[58,74] := 1.030142867; GammaY[59,74] := 1.035475253;
  //GammaY[60,74] := 1.04085934; GammaY[61,74] := 1.046299593; GammaY[62,74] := 1.051800727; GammaY[63,74] := 1.057367738; GammaY[64,74] := 1.063005941; GammaY[65,74] := 1.068721007; GammaY[66,74] := 1.074519011; GammaY[67,74] := 1.080406483; GammaY[68,74] := 1.086390468; GammaY[69,74] := 1.092478602;
  //GammaY[70,74] := 1.098679186; GammaY[71,74] := 1.105001272; GammaY[72,74] := 1.111454788; GammaY[73,74] := 1.118050656; GammaY[74,74] := 1.124800952; GammaY[75,74] := 1.131719089; GammaY[76,74] := 1.138820033; GammaY[77,74] := 1.146120579; GammaY[78,74] := 1.153639669; GammaY[79,74] := 1.16139881;
  //GammaY[80,74] := 1.169422561; GammaY[81,74] := 1.177739176; GammaY[82,74] := 1.186381419; GammaY[83,74] := 1.195387597; GammaY[84,74] := 1.204802925; GammaY[85,74] := 1.214681337; GammaY[86,74] := 1.225087948; GammaY[87,74] := 1.23610243; GammaY[88,74] := 1.247823809; GammaY[89,74] := 1.260377421;
  //GammaY[90,74] := 1.273925329; GammaY[91,74] := 1.288682525; GammaY[92,74] := 1.30494334; GammaY[93,74] := 1.323126897; GammaY[94,74] := 1.343861259; GammaY[95,74] := 1.36815478; GammaY[96,74] := 1.397795301; GammaY[97,74] := 1.436495145; GammaY[98,74] := 1.494730921; GammaY[99,74] := 1.616746478;
  //GammaY[0,75] := 0.564180792; GammaY[1,75] := 0.623703473; GammaY[2,75] := 0.655516325; GammaY[3,75] := 0.677836125; GammaY[4,75] := 0.695583938; GammaY[5,75] := 0.71056645; GammaY[6,75] := 0.723674685; GammaY[7,75] := 0.735420532; GammaY[8,75] := 0.746127714; GammaY[9,75] := 0.756015088;
  //GammaY[10,75] := 0.765238147; GammaY[11,75] := 0.773911668; GammaY[12,75] := 0.782122982; GammaY[13,75] := 0.789940245; GammaY[14,75] := 0.79741777; GammaY[15,75] := 0.804599595; GammaY[16,75] := 0.811521995; GammaY[17,75] := 0.818215238; GammaY[18,75] := 0.824704903; GammaY[19,75] := 0.831012825;
  //GammaY[20,75] := 0.837157804; GammaY[21,75] := 0.843156184; GammaY[22,75] := 0.849022276; GammaY[23,75] := 0.854768721; GammaY[24,75] := 0.860406752; GammaY[25,75] := 0.865946395; GammaY[26,75] := 0.871396676; GammaY[27,75] := 0.876765758; GammaY[28,75] := 0.882061053; GammaY[29,75] := 0.887289356;
  //GammaY[30,75] := 0.892456913; GammaY[31,75] := 0.897569472; GammaY[32,75] := 0.902632375; GammaY[33,75] := 0.907650599; GammaY[34,75] := 0.912628793; GammaY[35,75] := 0.917571355; GammaY[36,75] := 0.922482443; GammaY[37,75] := 0.92736598; GammaY[38,75] := 0.93222572; GammaY[39,75] := 0.93706526;
  //GammaY[40,75] := 0.941888045; GammaY[41,75] := 0.946697433; GammaY[42,75] := 0.951496688; GammaY[43,75] := 0.956288966; GammaY[44,75] := 0.961077376; GammaY[45,75] := 0.965864974; GammaY[46,75] := 0.97065477; GammaY[47,75] := 0.975449781; GammaY[48,75] := 0.98025302; GammaY[49,75] := 0.985067477;
  //GammaY[50,75] := 0.989896176; GammaY[51,75] := 0.994742173; GammaY[52,75] := 0.999608536; GammaY[53,75] := 1.004498457; GammaY[54,75] := 1.009415212; GammaY[55,75] := 1.014362095; GammaY[56,75] := 1.019342542; GammaY[57,75] := 1.02436011; GammaY[58,75] := 1.029418499; GammaY[59,75] := 1.034521571;
  //GammaY[60,75] := 1.039673374; GammaY[61,75] := 1.044878168; GammaY[62,75] := 1.05014044; GammaY[63,75] := 1.055464946; GammaY[64,75] := 1.06085675; GammaY[65,75] := 1.066321251; GammaY[66,75] := 1.071864231; GammaY[67,75] := 1.077491902; GammaY[68,75] := 1.083210969; GammaY[69,75] := 1.089028688;
  //GammaY[70,75] := 1.09495295; GammaY[71,75] := 1.100992364; GammaY[72,75] := 1.10715636; GammaY[73,75] := 1.113455323; GammaY[74,75] := 1.11990073; GammaY[75,75] := 1.126505317; GammaY[76,75] := 1.1332833; GammaY[77,75] := 1.140250629; GammaY[78,75] := 1.147425292; GammaY[79,75] := 1.154827704;
  //GammaY[80,75] := 1.162481185; GammaY[81,75] := 1.170412557; GammaY[82,75] := 1.178652916; GammaY[83,75] := 1.187238613; GammaY[84,75] := 1.196212548; GammaY[85,75] := 1.2056259; GammaY[86,75] := 1.215540439; GammaY[87,75] := 1.226031742; GammaY[88,75] := 1.237193731; GammaY[89,75] := 1.249145264;
  //GammaY[90,75] := 1.262040004; GammaY[91,75] := 1.276081782; GammaY[92,75] := 1.291549599; GammaY[93,75] := 1.308840673; GammaY[94,75] := 1.328550125; GammaY[95,75] := 1.351633215; GammaY[96,75] := 1.379783176; GammaY[97,75] := 1.416514661; GammaY[98,75] := 1.471741437; GammaY[99,75] := 1.587215467;
  //GammaY[0,76] := 0.57966953; GammaY[1,76] := 0.63765834; GammaY[2,76] := 0.668569754; GammaY[3,76] := 0.690223596; GammaY[4,76] := 0.707423907; GammaY[5,76] := 0.721932491; GammaY[6,76] := 0.734617621; GammaY[7,76] := 0.745977829; GammaY[8,76] := 0.756328253; GammaY[9,76] := 0.765881864;
  //GammaY[10,76] := 0.774789922; GammaY[11,76] := 0.783164044; GammaY[12,76] := 0.791089139; GammaY[13,76] := 0.798631442; GammaY[14,76] := 0.805843726; GammaY[15,76] := 0.812768789; GammaY[16,76] := 0.819441855; GammaY[17,76] := 0.825892319; GammaY[18,76] := 0.832145012; GammaY[19,76] := 0.838221128;
  //GammaY[20,76] := 0.844138911; GammaY[21,76] := 0.849914219; GammaY[22,76] := 0.855560931; GammaY[23,76] := 0.861091308; GammaY[24,76] := 0.866516238; GammaY[25,76] := 0.871845443; GammaY[26,76] := 0.877087665; GammaY[27,76] := 0.882250814; GammaY[28,76] := 0.887342067; GammaY[29,76] := 0.892368004;
  //GammaY[30,76] := 0.89733467; GammaY[31,76] := 0.902247626; GammaY[32,76] := 0.907112037; GammaY[33,76] := 0.911932705; GammaY[34,76] := 0.916714146; GammaY[35,76] := 0.921460606; GammaY[36,76] := 0.926176079; GammaY[37,76] := 0.930864362; GammaY[38,76] := 0.935529077; GammaY[39,76] := 0.940173678;
  //GammaY[40,76] := 0.94480151; GammaY[41,76] := 0.949415809; GammaY[42,76] := 0.95401969; GammaY[43,76] := 0.958616205; GammaY[44,76] := 0.963208339; GammaY[45,76] := 0.967799032; GammaY[46,76] := 0.972391171; GammaY[47,76] := 0.976987654; GammaY[48,76] := 0.981591374; GammaY[49,76] := 0.986205195;
  //GammaY[50,76] := 0.990832016; GammaY[51,76] := 0.995474746; GammaY[52,76] := 1.000136352; GammaY[53,76] := 1.004819892; GammaY[54,76] := 1.009528463; GammaY[55,76] := 1.014265226; GammaY[56,76] := 1.019033469; GammaY[57,76] := 1.023836592; GammaY[58,76] := 1.028678122; GammaY[59,76] := 1.033561742;
  //GammaY[60,76] := 1.038491313; GammaY[61,76] := 1.043470893; GammaY[62,76] := 1.048504761; GammaY[63,76] := 1.053597453; GammaY[64,76] := 1.058753787; GammaY[65,76] := 1.063978903; GammaY[66,76] := 1.069278308; GammaY[67,76] := 1.074657912; GammaY[68,76] := 1.080124087; GammaY[69,76] := 1.085683742;
  //GammaY[70,76] := 1.09134438; GammaY[71,76] := 1.097114188; GammaY[72,76] := 1.103002134; GammaY[73,76] := 1.109018084; GammaY[74,76] := 1.115172943; GammaY[75,76] := 1.121478817; GammaY[76,76] := 1.12794921; GammaY[77,76] := 1.134599276; GammaY[78,76] := 1.141446107; GammaY[79,76] := 1.148509086;
  //GammaY[80,76] := 1.155810358; GammaY[81,76] := 1.163375391; GammaY[82,76] := 1.171233707; GammaY[83,76] := 1.179419815; GammaY[84,76] := 1.18797444; GammaY[85,76] := 1.196946154; GammaY[86,76] := 1.206393574; GammaY[87,76] := 1.21638841; GammaY[88,76] := 1.227019775; GammaY[89,76] := 1.238400423;
  //GammaY[90,76] := 1.250676107; GammaY[91,76] := 1.264040135; GammaY[92,76] := 1.278757062; GammaY[93,76] := 1.295203485; GammaY[94,76] := 1.313943517; GammaY[95,76] := 1.335882438; GammaY[96,76] := 1.362624479; GammaY[97,76] := 1.397498419; GammaY[98,76] := 1.449889113; GammaY[99,76] := 1.559214068;
  //GammaY[0,77] := 0.594746108; GammaY[1,77] := 0.651188712; GammaY[2,77] := 0.681201279; GammaY[3,77] := 0.702194817; GammaY[4,77] := 0.718854209; GammaY[5,77] := 0.732895797; GammaY[6,77] := 0.745164877; GammaY[7,77] := 0.756146538; GammaY[8,77] := 0.766147288; GammaY[9,77] := 0.775374192;
  //GammaY[10,77] := 0.783974267; GammaY[11,77] := 0.792055969; GammaY[12,77] := 0.799701781; GammaY[13,77] := 0.806976028; GammaY[14,77] := 0.813929953; GammaY[15,77] := 0.820605099; GammaY[16,77] := 0.827035654; GammaY[17,77] := 0.83325014; GammaY[18,77] := 0.839272648; GammaY[19,77] := 0.845123734;
  //GammaY[20,77] := 0.85082109; GammaY[21,77] := 0.856380091; GammaY[22,77] := 0.861814189; GammaY[23,77] := 0.867135268; GammaY[24,77] := 0.872353877; GammaY[25,77] := 0.877479433; GammaY[26,77] := 0.882520402; GammaY[27,77] := 0.887484432; GammaY[28,77] := 0.892378489; GammaY[29,77] := 0.89720894;
  //GammaY[30,77] := 0.901981614; GammaY[31,77] := 0.906701898; GammaY[32,77] := 0.911374783; GammaY[33,77] := 0.916004911; GammaY[34,77] := 0.920596642; GammaY[35,77] := 0.92515408; GammaY[36,77] := 0.929681077; GammaY[37,77] := 0.934181297; GammaY[38,77] := 0.938658232; GammaY[39,77] := 0.943115209;
  //GammaY[40,77] := 0.947555454; GammaY[41,77] := 0.951982081; GammaY[42,77] := 0.956398091; GammaY[43,77] := 0.960806415; GammaY[44,77] := 0.965209926; GammaY[45,77] := 0.96961143; GammaY[46,77] := 0.974013732; GammaY[47,77] := 0.978419614; GammaY[48,77] := 0.982831821; GammaY[49,77] := 0.987253114;
  //GammaY[50,77] := 0.991686252; GammaY[51,77] := 0.996134052; GammaY[52,77] := 1.000599356; GammaY[53,77] := 1.005085059; GammaY[54,77] := 1.009594138; GammaY[55,77] := 1.014129619; GammaY[56,77] := 1.018694636; GammaY[57,77] := 1.023292434; GammaY[58,77] := 1.027926382; GammaY[59,77] := 1.032599997;
  //GammaY[60,77] := 1.037316956; GammaY[61,77] := 1.042081132; GammaY[62,77] := 1.04689661; GammaY[63,77] := 1.051767705; GammaY[64,77] := 1.056699007; GammaY[65,77] := 1.061695412; GammaY[66,77] := 1.066762159; GammaY[67,77] := 1.071904876; GammaY[68,77] := 1.077129633; GammaY[69,77] := 1.082442998;
  //GammaY[70,77] := 1.087852112; GammaY[71,77] := 1.093364757; GammaY[72,77] := 1.098989456; GammaY[73,77] := 1.104735597; GammaY[74,77] := 1.110613544; GammaY[75,77] := 1.116634804; GammaY[76,77] := 1.122812214; GammaY[77,77] := 1.129160171; GammaY[78,77] := 1.135694909; GammaY[79,77] := 1.142434846;
  //GammaY[80,77] := 1.149401019; GammaY[81,77] := 1.156617622; GammaY[82,77] := 1.164112687; GammaY[83,77] := 1.171918984; GammaY[84,77] := 1.18007518; GammaY[85,77] := 1.188627388; GammaY[86,77] := 1.197631253; GammaY[87,77] := 1.207154847; GammaY[88,77] := 1.217282737; GammaY[89,77] := 1.228121918;
  //GammaY[90,77] := 1.239810688; GammaY[91,77] := 1.25253244; GammaY[92,77] := 1.266538118; GammaY[93,77] := 1.282184882; GammaY[94,77] := 1.30000764; GammaY[95,77] := 1.320864616; GammaY[96,77] := 1.346276259; GammaY[97,77] := 1.379396482; GammaY[98,77] := 1.429112873; GammaY[99,77] := 1.532654825;
  //GammaY[0,78] := 0.609408293; GammaY[1,78] := 0.664298641; GammaY[2,78] := 0.693417585; GammaY[3,78] := 0.713758015; GammaY[4,78] := 0.729884115; GammaY[5,78] := 0.7434664; GammaY[6,78] := 0.755327071; GammaY[7,78] := 0.765937732; GammaY[8,78] := 0.775596252; GammaY[9,78] := 0.784503792;
  //GammaY[10,78] := 0.792803132; GammaY[11,78] := 0.800599576; GammaY[12,78] := 0.807973188; GammaY[13,78] := 0.814986394; GammaY[14,78] := 0.821688911; GammaY[15,78] := 0.82812104; GammaY[16,78] := 0.834315938; GammaY[17,78] := 0.84030126; GammaY[18,78] := 0.846100363; GammaY[19,78] := 0.851733171;
  //GammaY[20,78] := 0.857216831; GammaY[21,78] := 0.862566238; GammaY[22,78] := 0.867794425; GammaY[23,78] := 0.872912899; GammaY[24,78] := 0.877931879; GammaY[25,78] := 0.882860477; GammaY[26,78] := 0.88770689; GammaY[27,78] := 0.892478516; GammaY[28,78] := 0.897182095; GammaY[29,78] := 0.901823785;
  //GammaY[30,78] := 0.906409222; GammaY[31,78] := 0.91094361; GammaY[32,78] := 0.915431763; GammaY[33,78] := 0.919878184; GammaY[34,78] := 0.924287084; GammaY[35,78] := 0.928662404; GammaY[36,78] := 0.93300787; GammaY[37,78] := 0.937327015; GammaY[38,78] := 0.941623193; GammaY[39,78] := 0.945899633;
  //GammaY[40,78] := 0.950159444; GammaY[41,78] := 0.954405601; GammaY[42,78] := 0.958640999; GammaY[43,78] := 0.962868447; GammaY[44,78] := 0.967090715; GammaY[45,78] := 0.97131053; GammaY[46,78] := 0.975530558; GammaY[47,78] := 0.979753456; GammaY[48,78] := 0.983981868; GammaY[49,78] := 0.988218425;
  //GammaY[50,78] := 0.9924658; GammaY[51,78] := 0.996726696; GammaY[52,78] := 1.001003792; GammaY[53,78] := 1.00529988; GammaY[54,78] := 1.009617817; GammaY[55,78] := 1.013960492; GammaY[56,78] := 1.018330892; GammaY[57,78] := 1.022732111; GammaY[58,78] := 1.027167373; GammaY[59,78] := 1.031640033;
  //GammaY[60,78] := 1.036153602; GammaY[61,78] := 1.040711771; GammaY[62,78] := 1.045318428; GammaY[63,78] := 1.049977691; GammaY[64,78] := 1.054693929; GammaY[65,78] := 1.059471809; GammaY[66,78] := 1.064316327; GammaY[67,78] := 1.069232835; GammaY[68,78] := 1.074227109; GammaY[69,78] := 1.079305401;
  //GammaY[70,78] := 1.084474502; GammaY[71,78] := 1.089741819; GammaY[72,78] := 1.095115463; GammaY[73,78] := 1.100604356; GammaY[74,78] := 1.106218357; GammaY[75,78] := 1.111968403; GammaY[76,78] := 1.117866694; GammaY[77,78] := 1.123926917; GammaY[78,78] := 1.130164502; GammaY[79,78] := 1.136596953;
  //GammaY[80,78] := 1.143244256; GammaY[81,78] := 1.150129393; GammaY[82,78] := 1.157278995; GammaY[83,78] := 1.164724193; GammaY[84,78] := 1.172501716; GammaY[85,78] := 1.18065535; GammaY[86,78] := 1.189237935; GammaY[87,78] := 1.1983141; GammaY[88,78] := 1.207964134; GammaY[89,78] := 1.218289606;
  //GammaY[90,78] := 1.229421773; GammaY[91,78] := 1.241534673; GammaY[92,78] := 1.254866421; GammaY[93,78] := 1.269755862; GammaY[94,78] := 1.286710375; GammaY[95,78] := 1.306543855; GammaY[96,78] := 1.330697861; GammaY[97,78] := 1.3621617; GammaY[98,78] := 1.409355207; GammaY[99,78] := 1.507455919;
  //GammaY[0,79] := 0.623655486; GammaY[1,79] := 0.676993156; GammaY[2,79] := 0.705226032; GammaY[3,79] := 0.724921885; GammaY[4,79] := 0.740523221; GammaY[5,79] := 0.753654547; GammaY[6,79] := 0.765114937; GammaY[7,79] := 0.775362521; GammaY[8,79] := 0.784686548; GammaY[9,79] := 0.793282294;
  //GammaY[10,79] := 0.801288325; GammaY[11,79] := 0.808806806; GammaY[12,79] := 0.815915395; GammaY[13,79] := 0.822674643; GammaY[14,79] := 0.829132756; GammaY[15,79] := 0.835328789; GammaY[16,79] := 0.841294878; GammaY[17,79] := 0.847057834; GammaY[18,79] := 0.852640288; GammaY[19,79] := 0.858061527;
  //GammaY[20,79] := 0.863338163; GammaY[21,79] := 0.868484612; GammaY[22,79] := 0.873513507; GammaY[23,79] := 0.878435987; GammaY[24,79] := 0.883261926; GammaY[25,79] := 0.888000148; GammaY[26,79] := 0.892658572; GammaY[27,79] := 0.89724437; GammaY[28,79] := 0.901764061; GammaY[29,79] := 0.90622358;
  //GammaY[30,79] := 0.910628379; GammaY[31,79] := 0.914983478; GammaY[32,79] := 0.919293546; GammaY[33,79] := 0.923562929; GammaY[34,79] := 0.927795675; GammaY[35,79] := 0.931995592; GammaY[36,79] := 0.936166276; GammaY[37,79] := 0.94031112; GammaY[38,79] := 0.944433378; GammaY[39,79] := 0.948536165;
  //GammaY[40,79] := 0.952622448; GammaY[41,79] := 0.956695102; GammaY[42,79] := 0.9607569; GammaY[43,79] := 0.964810565; GammaY[44,79] := 0.968858762; GammaY[45,79] := 0.972904087; GammaY[46,79] := 0.97694911; GammaY[47,79] := 0.980996379; GammaY[48,79] := 0.985048415; GammaY[49,79] := 0.98910777;
  //GammaY[50,79] := 0.993177008; GammaY[51,79] := 0.997258687; GammaY[52,79] := 1.001355376; GammaY[53,79] := 1.005469755; GammaY[54,79] := 1.009604566; GammaY[55,79] := 1.013762555; GammaY[56,79] := 1.017946584; GammaY[57,79] := 1.022159613; GammaY[58,79] := 1.026404712; GammaY[59,79] := 1.030685089;
  //GammaY[60,79] := 1.03500409; GammaY[61,79] := 1.039365227; GammaY[62,79] := 1.043772217; GammaY[63,79] := 1.048228983; GammaY[64,79] := 1.052739693; GammaY[65,79] := 1.057308789; GammaY[66,79] := 1.061941024; GammaY[67,79] := 1.066641501; GammaY[68,79] := 1.071415714; GammaY[69,79] := 1.076269619;
  //GammaY[70,79] := 1.081209684; GammaY[71,79] := 1.086242951; GammaY[72,79] := 1.091377137; GammaY[73,79] := 1.096620729; GammaY[74,79] := 1.101983102; GammaY[75,79] := 1.107474659; GammaY[76,79] := 1.113107003; GammaY[77,79] := 1.118893149; GammaY[78,79] := 1.12484776; GammaY[79,79] := 1.130987471;
  //GammaY[80,79] := 1.137331281; GammaY[81,79] := 1.143901032; GammaY[82,79] := 1.150722032; GammaY[83,79] := 1.157823856; GammaY[84,79] := 1.165241392; GammaY[85,79] := 1.173016242; GammaY[86,79] := 1.181198601; GammaY[87,79] := 1.189849845; GammaY[88,79] := 1.19904622; GammaY[89,79] := 1.208884169;
  //GammaY[90,79] := 1.219488315; GammaY[91,79] := 1.231023871; GammaY[92,79] := 1.243716844; GammaY[93,79] := 1.257888809; GammaY[94,79] := 1.274021196; GammaY[95,79] := 1.292886117; GammaY[96,79] := 1.315850798; GammaY[97,79] := 1.345749539; GammaY[98,79] := 1.390561964; GammaY[99,79] := 1.483540561;
  //GammaY[0,80] := 0.637488538; GammaY[1,80] := 0.689278137; GammaY[2,80] := 0.716634543; GammaY[3,80] := 0.735695506; GammaY[4,80] := 0.750781373; GammaY[5,80] := 0.763470628; GammaY[6,80] := 0.77453927; GammaY[7,80] := 0.784432004; GammaY[8,80] := 0.793429506; GammaY[9,80] := 0.801721203;
  //GammaY[10,80] := 0.809441475; GammaY[11,80] := 0.816689382; GammaY[12,80] := 0.823540198; GammaY[13,80] := 0.830052608; GammaY[14,80] := 0.836273322; GammaY[15,80] := 0.842240176; GammaY[16,80] := 0.847984294; GammaY[17,80] := 0.853531646; GammaY[18,80] := 0.858904142; GammaY[19,80] := 0.864120455;
  //GammaY[20,80] := 0.869196655; GammaY[21,80] := 0.874146711; GammaY[22,80] := 0.878982845; GammaY[23,80] := 0.88371582; GammaY[24,80] := 0.888355193; GammaY[25,80] := 0.892909487; GammaY[26,80] := 0.897386376; GammaY[27,80] := 0.901792795; GammaY[28,80] := 0.906135026; GammaY[29,80] := 0.91041881;
  //GammaY[30,80] := 0.914649405; GammaY[31,80] := 0.918831676; GammaY[32,80] := 0.922970132; GammaY[33,80] := 0.927068949; GammaY[34,80] := 0.931132041; GammaY[35,80] := 0.935163069; GammaY[36,80] := 0.93916551; GammaY[37,80] := 0.943142657; GammaY[38,80] := 0.947097625; GammaY[39,80] := 0.951033401;
  //GammaY[40,80] := 0.954952853; GammaY[41,80] := 0.958858731; GammaY[42,80] := 0.962753728; GammaY[43,80] := 0.966640465; GammaY[44,80] := 0.970521478; GammaY[45,80] := 0.974399269; GammaY[46,80] := 0.978276292; GammaY[47,80] := 0.982155004; GammaY[48,80] := 0.986037854; GammaY[49,80] := 0.989927261;
  //GammaY[50,80] := 0.993825667; GammaY[51,80] := 0.997735517; GammaY[52,80] := 1.001659299; GammaY[53,80] := 1.005599581; GammaY[54,80] := 1.009558957; GammaY[55,80] := 1.01354006; GammaY[56,80] := 1.017545631; GammaY[57,80] := 1.021578502; GammaY[58,80] := 1.0256416; GammaY[59,80] := 1.029737982;
  //GammaY[60,80] := 1.033870845; GammaY[61,80] := 1.038043545; GammaY[62,80] := 1.042259619; GammaY[63,80] := 1.04652281; GammaY[64,80] := 1.050837093; GammaY[65,80] := 1.055206697; GammaY[66,80] := 1.059636145; GammaY[67,80] := 1.064130302; GammaY[68,80] := 1.068694405; GammaY[69,80] := 1.07333412;
  //GammaY[70,80] := 1.078055599; GammaY[71,80] := 1.082865549; GammaY[72,80] := 1.087771311; GammaY[73,80] := 1.092780964; GammaY[74,80] := 1.09790343; GammaY[75,80] := 1.1031486; GammaY[76,80] := 1.108527512; GammaY[77,80] := 1.114052536; GammaY[78,80] := 1.119737622; GammaY[79,80] := 1.125598588;
  //GammaY[80,80] := 1.131653495; GammaY[81,80] := 1.137923104; GammaY[82,80] := 1.144431465; GammaY[83,80] := 1.151206691; GammaY[84,80] := 1.15828194; GammaY[85,80] := 1.16569674; GammaY[86,80] := 1.173498774; GammaY[87,80] := 1.181746361; GammaY[88,80] := 1.190511932; GammaY[89,80] := 1.199887085;
  //GammaY[90,80] := 1.209990175; GammaY[91,80] := 1.22097809; GammaY[92,80] := 1.23306542; GammaY[93,80] := 1.246557436; GammaY[94,80] := 1.261911092; GammaY[95,80] := 1.279859106; GammaY[96,80] := 1.301698638; GammaY[97,80] := 1.330117939; GammaY[98,80] := 1.372682143; GammaY[99,80] := 1.460836767;
  //GammaY[0,81] := 0.650909584; GammaY[1,81] := 0.701160191; GammaY[2,81] := 0.727651502; GammaY[3,81] := 0.746088256; GammaY[4,81] := 0.760668596; GammaY[5,81] := 0.772925121; GammaY[6,81] := 0.783610874; GammaY[7,81] := 0.793157225; GammaY[8,81] := 0.801836343; GammaY[9,81] := 0.80983186;
  //GammaY[10,81] := 0.817274015; GammaY[11,81] := 0.82425879; GammaY[12,81] := 0.830859106; GammaY[13,81] := 0.837131804; GammaY[14,81] := 0.843122116; GammaY[15,81] := 0.848866668; GammaY[16,81] := 0.854395605; GammaY[17,81] := 0.85973406; GammaY[18,81] := 0.864903226; GammaY[19,81] := 0.869921167;
  //GammaY[20,81] := 0.874803446; GammaY[21,81] := 0.879563572; GammaY[22,81] := 0.884213354; GammaY[23,81] := 0.888763204; GammaY[24,81] := 0.893222352; GammaY[25,81] := 0.897599053; GammaY[26,81] := 0.901900725; GammaY[27,81] := 0.90613405; GammaY[28,81] := 0.910305105; GammaY[29,81] := 0.914419424;
  //GammaY[30,81] := 0.918482102; GammaY[31,81] := 0.922497838; GammaY[32,81] := 0.926470963; GammaY[33,81] := 0.930405515; GammaY[34,81] := 0.934305255; GammaY[35,81] := 0.938173737; GammaY[36,81] := 0.942014311; GammaY[37,81] := 0.945830132; GammaY[38,81] := 0.949624207; GammaY[39,81] := 0.953399399;
  //GammaY[40,81] := 0.95715848; GammaY[41,81] := 0.960904122; GammaY[42,81] := 0.964638891; GammaY[43,81] := 0.968365292; GammaY[44,81] := 0.972085759; GammaY[45,81] := 0.975802709; GammaY[46,81] := 0.979518523; GammaY[47,81] := 0.983235536; GammaY[48,81] := 0.98695608; GammaY[49,81] := 0.990682472;
  //GammaY[50,81] := 0.994417064; GammaY[51,81] := 0.998162211; GammaY[52,81] := 1.0019203; GammaY[53,81] := 1.005693775; GammaY[54,81] := 1.009485098; GammaY[55,81] := 1.013296808; GammaY[56,81] := 1.017131518; GammaY[57,81] := 1.020991928; GammaY[58,81] := 1.02488084; GammaY[59,81] := 1.028801167;
  //GammaY[60,81] := 1.032755965; GammaY[61,81] := 1.036748442; GammaY[62,81] := 1.040781966; GammaY[63,81] := 1.044860104; GammaY[64,81] := 1.048986643; GammaY[65,81] := 1.053165617; GammaY[66,81] := 1.057401341; GammaY[67,81] := 1.061698448; GammaY[68,81] := 1.066061921; GammaY[69,81] := 1.070497152;
  //GammaY[70,81] := 1.075010003; GammaY[71,81] := 1.07960686; GammaY[72,81] := 1.084294713; GammaY[73,81] := 1.089081247; GammaY[74,81] := 1.093974949; GammaY[75,81] := 1.09898523; GammaY[76,81] := 1.104122594; GammaY[77,81] := 1.109398813; GammaY[78,81] := 1.11482715; GammaY[79,81] := 1.120422648;
  //GammaY[80,81] := 1.12620248; GammaY[81,81] := 1.132186389; GammaY[82,81] := 1.138397248; GammaY[83,81] := 1.144861774; GammaY[84,81] := 1.151611483; GammaY[85,81] := 1.158683951; GammaY[86,81] := 1.166124498; GammaY[87,81] := 1.17398854; GammaY[88,81] := 1.182344895; GammaY[89,81] := 1.1912806;
  //GammaY[90,81] := 1.200908091; GammaY[91,81] := 1.21137639; GammaY[92,81] := 1.222889301; GammaY[93,81] := 1.235736717; GammaY[94,81] := 1.250352497; GammaY[95,81] := 1.26743219; GammaY[96,81] := 1.288206882; GammaY[97,81] := 1.315227159; GammaY[98,81] := 1.355667685; GammaY[99,81] := 1.439276998;
  //GammaY[0,82] := 0.66392189; GammaY[1,82] := 0.712646529; GammaY[2,82] := 0.738285674; GammaY[3,82] := 0.756109742; GammaY[4,82] := 0.770195033; GammaY[5,82] := 0.782028538; GammaY[6,82] := 0.792340517; GammaY[7,82] := 0.801549133; GammaY[8,82] := 0.80991813; GammaY[9,82] := 0.817625421;
  //GammaY[10,82] := 0.824797152; GammaY[11,82] := 0.831526253; GammaY[12,82] := 0.837883341; GammaY[13,82] := 0.843923437; GammaY[14,82] := 0.849690301; GammaY[15,82] := 0.855219393; GammaY[16,82] := 0.860539877; GammaY[17,82] := 0.865676057; GammaY[18,82] := 0.870648427; GammaY[19,82] := 0.875474472;
  //GammaY[20,82] := 0.880169238; GammaY[21,82] := 0.884745774; GammaY[22,82] := 0.8892155; GammaY[23,82] := 0.89358847; GammaY[24,82] := 0.897873619; GammaY[25,82] := 0.902078924; GammaY[26,82] := 0.906211534; GammaY[27,82] := 0.9102779; GammaY[28,82] := 0.914283898; GammaY[29,82] := 0.918234888;
  //GammaY[30,82] := 0.922135773; GammaY[31,82] := 0.925991075; GammaY[32,82] := 0.92980497; GammaY[33,82] := 0.933581362; GammaY[34,82] := 0.9373239; GammaY[35,82] := 0.941035988; GammaY[36,82] := 0.944720844; GammaY[37,82] := 0.948381503; GammaY[38,82] := 0.952020873; GammaY[39,82] := 0.955641732;
  //GammaY[40,82] := 0.959246727; GammaY[41,82] := 0.962838415; GammaY[42,82] := 0.96641926; GammaY[43,82] := 0.969991681; GammaY[44,82] := 0.973558044; GammaY[45,82] := 0.977120644; GammaY[46,82] := 0.980681754; GammaY[47,82] := 0.984243611; GammaY[48,82] := 0.987808465; GammaY[49,82] := 0.991378564;
  //GammaY[50,82] := 0.994956138; GammaY[51,82] := 0.998543409; GammaY[52,82] := 1.002142693; GammaY[53,82] := 1.005756343; GammaY[54,82] := 1.009386699; GammaY[55,82] := 1.013036188; GammaY[56,82] := 1.016707311; GammaY[57,82] := 1.020402644; GammaY[58,82] := 1.024124857; GammaY[59,82] := 1.027876741;
  //GammaY[60,82] := 1.03166121; GammaY[61,82] := 1.035481319; GammaY[62,82] := 1.039340286; GammaY[63,82] := 1.043241509; GammaY[64,82] := 1.047188603; GammaY[65,82] := 1.05118542; GammaY[66,82] := 1.055236063; GammaY[67,82] := 1.059344946; GammaY[68,82] := 1.06351682; GammaY[69,82] := 1.067756824;
  //GammaY[70,82] := 1.072070536; GammaY[71,82] := 1.07646404; GammaY[72,82] := 1.080943988; GammaY[73,82] := 1.085517689; GammaY[74,82] := 1.090193227; GammaY[75,82] := 1.094979562; GammaY[76,82] := 1.099886685; GammaY[77,82] := 1.104925793; GammaY[78,82] := 1.110109504; GammaY[79,82] := 1.115452133;
  //GammaY[80,82] := 1.120970019; GammaY[81,82] := 1.126681933; GammaY[82,82] := 1.132609623; GammaY[83,82] := 1.138778502; GammaY[84,82] := 1.145218541; GammaY[85,82] := 1.151965455; GammaY[86,82] := 1.159062326; GammaY[87,82] := 1.166561835; GammaY[88,82] := 1.174529393; GammaY[89,82] := 1.183047713;
  //GammaY[90,82] := 1.192223635; GammaY[91,82] := 1.202198757; GammaY[92,82] := 1.213166703; GammaY[93,82] := 1.225402835; GammaY[94,82] := 1.239319212; GammaY[95,82] := 1.255576308; GammaY[96,82] := 1.275342869; GammaY[97,82] := 1.301039646; GammaY[98,82] := 1.339473299; GammaY[99,82] := 1.418797966;
  //GammaY[0,83] := 0.676529721; GammaY[1,83] := 0.723744892; GammaY[2,83] := 0.74854612; GammaY[3,83] := 0.76576973; GammaY[4,83] := 0.779370893; GammaY[5,83] := 0.790791371; GammaY[6,83] := 0.80073889; GammaY[7,83] := 0.809618554; GammaY[8,83] := 0.817685783; GammaY[9,83] := 0.825112846;
  //GammaY[10,83] := 0.832021848; GammaY[11,83] := 0.838502738; GammaY[12,83] := 0.844623847; GammaY[13,83] := 0.850438398; GammaY[14,83] := 0.855988723; GammaY[15,83] := 0.861309118; GammaY[16,83] := 0.866427788; GammaY[17,83] := 0.871368222; GammaY[18,83] := 0.876150246; GammaY[19,83] := 0.880790761;
  //GammaY[20,83] := 0.885304296; GammaY[21,83] := 0.889703459; GammaY[22,83] := 0.893999289; GammaY[23,83] := 0.898201516; GammaY[24,83] := 0.902318754; GammaY[25,83] := 0.90635869; GammaY[26,83] := 0.910328242; GammaY[27,83] := 0.914233655; GammaY[28,83] := 0.918080583; GammaY[29,83] := 0.921874192;
  //GammaY[30,83] := 0.925619208; GammaY[31,83] := 0.929320002; GammaY[32,83] := 0.932980621; GammaY[33,83] := 0.936604811; GammaY[34,83] := 0.940196078; GammaY[35,83] := 0.9437577; GammaY[36,83] := 0.947292787; GammaY[37,83] := 0.950804283; GammaY[38,83] := 0.954294968; GammaY[39,83] := 0.957767505;
  //GammaY[40,83] := 0.961224435; GammaY[41,83] := 0.964668229; GammaY[42,83] := 0.96810128; GammaY[43,83] := 0.971525892; GammaY[44,83] := 0.974944323; GammaY[45,83] := 0.978358773; GammaY[46,83] := 0.981771438; GammaY[47,83] := 0.985184489; GammaY[48,83] := 0.98860006; GammaY[49,83] := 0.992020277;
  //GammaY[50,83] := 0.995447301; GammaY[51,83] := 0.998883286; GammaY[52,83] := 1.002330424; GammaY[53,83] := 1.005790954; GammaY[54,83] := 1.009267128; GammaY[55,83] := 1.012761272; GammaY[56,83] := 1.016275768; GammaY[57,83] := 1.019813079; GammaY[58,83] := 1.023375763; GammaY[59,83] := 1.026966479;
  //GammaY[60,83] := 1.030588011; GammaY[61,83] := 1.034243272; GammaY[62,83] := 1.037935326; GammaY[63,83] := 1.041667425; GammaY[64,83] := 1.045443012; GammaY[65,83] := 1.049265753; GammaY[66,83] := 1.053139566; GammaY[67,83] := 1.057068651; GammaY[68,83] := 1.061057543; GammaY[69,83] := 1.065111139;
  //GammaY[70,83] := 1.06923475; GammaY[71,83] := 1.073434164; GammaY[72,83] := 1.077715721; GammaY[73,83] := 1.082086383; GammaY[74,83] := 1.086553846; GammaY[75,83] := 1.091126634; GammaY[76,83] := 1.095814253; GammaY[77,83] := 1.100627363; GammaY[78,83] := 1.105577976; GammaY[79,83] := 1.110679707;
  //GammaY[80,83] := 1.115948095; GammaY[81,83] := 1.121401006; GammaY[82,83] := 1.127059131; GammaY[83,83] := 1.132946636; GammaY[84,83] := 1.139092024; GammaY[85,83] := 1.145529262; GammaY[86,83] := 1.152299324; GammaY[87,83] := 1.15945229; GammaY[88,83] := 1.167050347; GammaY[89,83] := 1.175172126;
  //GammaY[90,83] := 1.183919184; GammaY[91,83] := 1.193426095; GammaY[92,83] := 1.203876857; GammaY[93,83] := 1.21553312; GammaY[94,83] := 1.228786343; GammaY[95,83] := 1.244263887; GammaY[96,83] := 1.263075657; GammaY[97,83] := 1.287519905; GammaY[98,83] := 1.324056287; GammaY[99,83] := 1.399340148;
  //GammaY[0,84] := 0.688738201; GammaY[1,84] := 0.734463436; GammaY[2,84] := 0.758442129; GammaY[3,84] := 0.77507809; GammaY[4,84] := 0.788206396; GammaY[5,84] := 0.799224078; GammaY[6,84] := 0.808816589; GammaY[7,84] := 0.817376153; GammaY[8,84] := 0.825150008; GammaY[9,84] := 0.832304858;
  //GammaY[10,84] := 0.838958828; GammaY[11,84] := 0.845198929; GammaY[12,84] := 0.851091251; GammaY[13,84] := 0.856687248; GammaY[14,84] := 0.862027862; GammaY[15,84] := 0.867146247; GammaY[16,84] := 0.872069643; GammaY[17,84] := 0.87682077; GammaY[18,84] := 0.881418788; GammaY[19,84] := 0.88588001;
  //GammaY[20,84] := 0.890218472; GammaY[21,84] := 0.894446363; GammaY[22,84] := 0.898574348; GammaY[23,84] := 0.902611806; GammaY[24,84] := 0.906567052; GammaY[25,84] := 0.910447522; GammaY[26,84] := 0.914259893; GammaY[27,84] := 0.918010177; GammaY[28,84] := 0.921703826; GammaY[29,84] := 0.925345832;
  //GammaY[30,84] := 0.92894077; GammaY[31,84] := 0.932492839; GammaY[32,84] := 0.936005931; GammaY[33,84] := 0.939483653; GammaY[34,84] := 0.942929392; GammaY[35,84] := 0.946346329; GammaY[36,84] := 0.949737437; GammaY[37,84] := 0.953105531; GammaY[38,84] := 0.956453304; GammaY[39,84] := 0.959783328;
  //GammaY[40,84] := 0.963098051; GammaY[41,84] := 0.966399837; GammaY[42,84] := 0.969690962; GammaY[43,84] := 0.97297366; GammaY[44,84] := 0.976250112; GammaY[45,84] := 0.979522433; GammaY[46,84] := 0.982792717; GammaY[47,84] := 0.98606302; GammaY[48,84] := 0.989335412; GammaY[49,84] := 0.992611959;
  //GammaY[50,84] := 0.995894705; GammaY[51,84] := 0.999185692; GammaY[52,84] := 1.002487037; GammaY[53,84] := 1.005800896; GammaY[54,84] := 1.00912941; GammaY[55,84] := 1.012474799; GammaY[56,84] := 1.015839346; GammaY[57,84] := 1.0192254; GammaY[58,84] := 1.022635409; GammaY[59,84] := 1.026071917;
  //GammaY[60,84] := 1.029537574; GammaY[61,84] := 1.033035163; GammaY[62,84] := 1.036567612; GammaY[63,84] := 1.040138025; GammaY[64,84] := 1.043749677; GammaY[65,84] := 1.047406061; GammaY[66,84] := 1.051110919; GammaY[67,84] := 1.054868257; GammaY[68,84] := 1.058682392; GammaY[69,84] := 1.062557987;
  //GammaY[70,84] := 1.066500102; GammaY[71,84] := 1.070514257; GammaY[72,84] := 1.074606491; GammaY[73,84] := 1.078783434; GammaY[74,84] := 1.083052406; GammaY[75,84] := 1.087421529; GammaY[76,84] := 1.091899856; GammaY[77,84] := 1.096497534; GammaY[78,84] := 1.101225991; GammaY[79,84] := 1.106098182;
  //GammaY[80,84] := 1.111128902; GammaY[81,84] := 1.116335147; GammaY[82,84] := 1.121736602; GammaY[83,84] := 1.127356258; GammaY[84,84] := 1.133221233; GammaY[85,84] := 1.139363842; GammaY[86,84] := 1.145823049; GammaY[87,84] := 1.152646488; GammaY[88,84] := 1.159893301; GammaY[89,84] := 1.16763825;
  //GammaY[90,84] := 1.175977892; GammaY[91,84] := 1.185040167; GammaY[92,84] := 1.194999973; GammaY[93,84] := 1.206105992; GammaY[94,84] := 1.218730233; GammaY[95,84] := 1.233468769; GammaY[96,84] := 1.251375943; GammaY[97,84] := 1.274634384; GammaY[98,84] := 1.30937639; GammaY[99,84] := 1.380847638;
  //GammaY[0,85] := 0.70055321; GammaY[1,85] := 0.744810675; GammaY[2,85] := 0.767983157; GammaY[3,85] := 0.784044761; GammaY[4,85] := 0.796711752; GammaY[5,85] := 0.807337017; GammaY[6,85] := 0.81658406; GammaY[7,85] := 0.824832434; GammaY[8,85] := 0.832321321; GammaY[9,85] := 0.839211939;
  //GammaY[10,85] := 0.845618529; GammaY[11,85] := 0.851625212; GammaY[12,85] := 0.857295865; GammaY[13,85] := 0.862680237; GammaY[14,85] := 0.867817884; GammaY[15,85] := 0.872740825; GammaY[16,85] := 0.877475396; GammaY[17,85] := 0.882043544; GammaY[18,85] := 0.886463754; GammaY[19,85] := 0.890751794;
  //GammaY[20,85] := 0.89492123; GammaY[21,85] := 0.898983819; GammaY[22,85] := 0.902949839; GammaY[23,85] := 0.906828356; GammaY[24,85] := 0.910627411; GammaY[25,85] := 0.914354167; GammaY[26,85] := 0.918015046; GammaY[27,85] := 0.921615863; GammaY[28,85] := 0.925161894; GammaY[29,85] := 0.928657937;
  //GammaY[30,85] := 0.932108384; GammaY[31,85] := 0.935517299; GammaY[32,85] := 0.93888844; GammaY[33,85] := 0.942225287; GammaY[34,85] := 0.945531092; GammaY[35,85] := 0.948808896; GammaY[36,85] := 0.952061582; GammaY[37,85] := 0.955291878; GammaY[38,85] := 0.958502357; GammaY[39,85] := 0.961695473;
  //GammaY[40,85] := 0.964873597; GammaY[41,85] := 0.968039013; GammaY[42,85] := 0.97119391; GammaY[43,85] := 0.974340423; GammaY[44,85] := 0.977480622; GammaY[45,85] := 0.980616559; GammaY[46,85] := 0.983750255; GammaY[47,85] := 0.986883685; GammaY[48,85] := 0.99001882; GammaY[49,85] := 0.993157614;
  //GammaY[50,85] := 0.996302049; GammaY[51,85] := 0.999454093; GammaY[52,85] := 1.002615764; GammaY[53,85] := 1.005789125; GammaY[54,85] := 1.008976228; GammaY[55,85] := 1.012179188; GammaY[56,85] := 1.015400183; GammaY[57,85] := 1.018641469; GammaY[58,85] := 1.021905379; GammaY[59,85] := 1.02519434;
  //GammaY[60,85] := 1.02851088; GammaY[61,85] := 1.031857663; GammaY[62,85] := 1.035237489; GammaY[63,85] := 1.038653306; GammaY[64,85] := 1.042108247; GammaY[65,85] := 1.045605646; GammaY[66,85] := 1.049149065; GammaY[67,85] := 1.052742328; GammaY[68,85] := 1.056389545; GammaY[69,85] := 1.060095157;
  //GammaY[70,85] := 1.063863992; GammaY[71,85] := 1.067701307; GammaY[72,85] := 1.071612851; GammaY[73,85] := 1.075604938; GammaY[74,85] := 1.079684546; GammaY[75,85] := 1.083859414; GammaY[76,85] := 1.088138157; GammaY[77,85] := 1.092530432; GammaY[78,85] := 1.097047122; GammaY[79,85] := 1.101700569;
  //GammaY[80,85] := 1.106504844; GammaY[81,85] := 1.111476122; GammaY[82,85] := 1.116633141; GammaY[83,85] := 1.121997782; GammaY[84,85] := 1.127595847; GammaY[85,85] := 1.133458076; GammaY[86,85] := 1.139621545; GammaY[87,85] := 1.146131571; GammaY[88,85] := 1.153044404; GammaY[89,85] := 1.160431158;
  //GammaY[90,85] := 1.168383661; GammaY[91,85] := 1.177023567; GammaY[92,85] := 1.18651718; GammaY[93,85] := 1.197100916; GammaY[94,85] := 1.209128393; GammaY[95,85] := 1.223166124; GammaY[96,85] := 1.240215963; GammaY[97,85] := 1.262351349; GammaY[98,85] := 1.295395624; GammaY[99,85] := 1.363268151;
  //GammaY[0,86] := 0.711981261; GammaY[1,86] := 0.754795389; GammaY[2,86] := 0.777178772; GammaY[3,86] := 0.792679677; GammaY[4,86] := 0.804897094; GammaY[5,86] := 0.815140431; GammaY[6,86] := 0.824051607; GammaY[7,86] := 0.831997704; GammaY[8,86] := 0.839209991; GammaY[9,86] := 0.84584433;
  //GammaY[10,86] := 0.852011134; GammaY[11,86] := 0.857791691; GammaY[12,86] := 0.863247727; GammaY[13,86] := 0.868427287; GammaY[14,86] := 0.873368587; GammaY[15,86] := 0.878102565; GammaY[16,86] := 0.882654642; GammaY[17,86] := 0.887045995; GammaY[18,86] := 0.89129449; GammaY[19,86] := 0.895415348;
  //GammaY[20,86] := 0.899421643; GammaY[21,86] := 0.903324734; GammaY[22,86] := 0.907134553; GammaY[23,86] := 0.910859835; GammaY[24,86] := 0.914508321; GammaY[25,86] := 0.918086933; GammaY[26,86] := 0.921601883; GammaY[27,86] := 0.925058761; GammaY[28,86] := 0.928462639; GammaY[29,86] := 0.931818155;
  //GammaY[30,86] := 0.935129562; GammaY[31,86] := 0.938400757; GammaY[32,86] := 0.941635346; GammaY[33,86] := 0.944836692; GammaY[34,86] := 0.948007941; GammaY[35,86] := 0.951152019; GammaY[36,86] := 0.954271693; GammaY[37,86] := 0.957369566; GammaY[38,86] := 0.960448134; GammaY[39,86] := 0.963509775;
  //GammaY[40,86] := 0.966556752; GammaY[41,86] := 0.969591237; GammaY[42,86] := 0.972615352; GammaY[43,86] := 0.975631159; GammaY[44,86] := 0.978640648; GammaY[45,86] := 0.981645778; GammaY[46,86] := 0.984648461; GammaY[47,86] := 0.987650614; GammaY[48,86] := 0.990654141; GammaY[49,86] := 0.993660916;
  //GammaY[50,86] := 0.996672811; GammaY[51,86] := 0.999691709; GammaY[52,86] := 1.002719569; GammaY[53,86] := 1.005758355; GammaY[54,86] := 1.008810021; GammaY[55,86] := 1.011876597; GammaY[56,86] := 1.01496017; GammaY[57,86] := 1.01806289; GammaY[58,86] := 1.021186991; GammaY[59,86] := 1.024334785;
  //GammaY[60,86] := 1.027508688; GammaY[61,86] := 1.030711241; GammaY[62,86] := 1.033945114; GammaY[63,86] := 1.037213125; GammaY[64,86] := 1.040518258; GammaY[65,86] := 1.0438637; GammaY[66,86] := 1.04725285; GammaY[67,86] := 1.050689348; GammaY[68,86] := 1.054177114; GammaY[69,86] := 1.057720386;
  //GammaY[70,86] := 1.061323765; GammaY[71,86] := 1.064992256; GammaY[72,86] := 1.068731332; GammaY[73,86] := 1.072547018; GammaY[74,86] := 1.076445957; GammaY[75,86] := 1.080435516; GammaY[76,86] := 1.0845239; GammaY[77,86] := 1.088720313; GammaY[78,86] := 1.093035119; GammaY[79,86] := 1.097480062;
  //GammaY[80,86] := 1.102068544; GammaY[81,86] := 1.106815968; GammaY[82,86] := 1.111740163; GammaY[83,86] := 1.116861955; GammaY[84,86] := 1.122205903; GammaY[85,86] := 1.127801265; GammaY[86,86] := 1.133683318; GammaY[87,86] := 1.139895182; GammaY[88,86] := 1.146490383; GammaY[89,86] := 1.15353657;
  //GammaY[90,86] := 1.1611211; GammaY[91,86] := 1.169359688; GammaY[92,86] := 1.178410493; GammaY[93,86] := 1.18849834; GammaY[94,86] := 1.199959451; GammaY[95,86] := 1.213332392; GammaY[96,86] := 1.229569412; GammaY[97,86] := 1.25064079; GammaY[98,86] := 1.282078153; GammaY[99,86] := 1.346552253;
  //GammaY[0,87] := 0.723029415; GammaY[1,87] := 0.764426565; GammaY[2,87] := 0.786038596; GammaY[3,87] := 0.80099275; GammaY[4,87] := 0.812772482; GammaY[5,87] := 0.822644435; GammaY[6,87] := 0.831229339; GammaY[7,87] := 0.838882046; GammaY[8,87] := 0.845826069; GammaY[9,87] := 0.852212011;
  //GammaY[10,87] := 0.85814654; GammaY[11,87] := 0.863708179; GammaY[12,87] := 0.868956528; GammaY[13,87] := 0.873937994; GammaY[14,87] := 0.878689478; GammaY[15,87] := 0.88324083; GammaY[16,87] := 0.8876166; GammaY[17,87] := 0.891837239; GammaY[18,87] := 0.89591997; GammaY[19,87] := 0.899879481;
  //GammaY[20,87] := 0.903728405; GammaY[21,87] := 0.907477688; GammaY[22,87] := 0.911136896; GammaY[23,87] := 0.914714466; GammaY[24,87] := 0.918217881; GammaY[25,87] := 0.921653798; GammaY[26,87] := 0.925028191; GammaY[27,87] := 0.928346464; GammaY[28,87] := 0.931613521; GammaY[29,87] := 0.934833818;
  //GammaY[30,87] := 0.938011431; GammaY[31,87] := 0.941150134; GammaY[32,87] := 0.944253416; GammaY[33,87] := 0.947324501; GammaY[34,87] := 0.950366394; GammaY[35,87] := 0.953381933; GammaY[36,87] := 0.956373788; GammaY[37,87] := 0.959344466; GammaY[38,87] := 0.962296346; GammaY[39,87] := 0.965231714;
  //GammaY[40,87] := 0.968152764; GammaY[41,87] := 0.971061587; GammaY[42,87] := 0.973960198; GammaY[43,87] := 0.976850578; GammaY[44,87] := 0.979734657; GammaY[45,87] := 0.982614304; GammaY[46,87] := 0.985491369; GammaY[47,87] := 0.988367663; GammaY[48,87] := 0.991245011; GammaY[49,87] := 0.994125233;
  //GammaY[50,87] := 0.997010126; GammaY[51,87] := 0.999901474; GammaY[52,87] := 1.002801142; GammaY[53,87] := 1.005711026; GammaY[54,87] := 1.008632995; GammaY[55,87] := 1.011568984; GammaY[56,87] := 1.014520994; GammaY[57,87] := 1.017491087; GammaY[58,87] := 1.020481385; GammaY[59,87] := 1.023494102;
  //GammaY[60,87] := 1.026531551; GammaY[61,87] := 1.029596156; GammaY[62,87] := 1.032690461; GammaY[63,87] := 1.035817151; GammaY[64,87] := 1.038979082; GammaY[65,87] := 1.04217929; GammaY[66,87] := 1.045421015; GammaY[67,87] := 1.048707727; GammaY[68,87] := 1.052043169; GammaY[69,87] := 1.055431386;
  //GammaY[70,87] := 1.058876755; GammaY[71,87] := 1.06238405; GammaY[72,87] := 1.065958492; GammaY[73,87] := 1.06960582; GammaY[74,87] := 1.073332359; GammaY[75,87] := 1.077145125; GammaY[76,87] := 1.081051941; GammaY[77,87] := 1.085061569; GammaY[78,87] := 1.089183876; GammaY[79,87] := 1.093430053;
  //GammaY[80,87] := 1.097812866; GammaY[81,87] := 1.102346974; GammaY[82,87] := 1.107049357; GammaY[83,87] := 1.111939847; GammaY[84,87] := 1.117041817; GammaY[85,87] := 1.122383115; GammaY[86,87] := 1.127997325; GammaY[87,87] := 1.133925483; GammaY[88,87] := 1.140218523; GammaY[89,87] := 1.146940826;
  //GammaY[90,87] := 1.154175513; GammaY[91,87] := 1.162032674; GammaY[92,87] := 1.170662773; GammaY[93,87] := 1.180279655; GammaY[94,87] := 1.191203089; GammaY[95,87] := 1.203945207; GammaY[96,87] := 1.219411352; GammaY[97,87] := 1.239474314; GammaY[98,87] := 1.269390151; GammaY[99,87] := 1.330653628;
  //GammaY[0,88] := 0.73370519; GammaY[1,88] := 0.773713355; GammaY[2,88] := 0.794572292; GammaY[3,88] := 0.808993835; GammaY[4,88] := 0.820347842; GammaY[5,88] := 0.82985898; GammaY[6,88] := 0.838127183; GammaY[7,88] := 0.845495337; GammaY[8,88] := 0.852179355; GammaY[9,88] := 0.858324696;
  //GammaY[10,88] := 0.864034369; GammaY[11,88] := 0.869384177; GammaY[12,88] := 0.874431669; GammaY[13,88] := 0.879221638; GammaY[14,88] := 0.88378969; GammaY[15,88] := 0.888164648; GammaY[16,88] := 0.892370183; GammaY[17,88] := 0.896426028; GammaY[18,88] := 0.900348814; GammaY[19,88] := 0.904152698;
  //GammaY[20,88] := 0.90784985; GammaY[21,88] := 0.911450839; GammaY[22,88] := 0.914964903; GammaY[23,88] := 0.918400158; GammaY[24,88] := 0.921763814; GammaY[25,88] := 0.925062303; GammaY[26,88] := 0.928301385; GammaY[27,88] := 0.931486251; GammaY[28,88] := 0.934621626; GammaY[29,88] := 0.937711815;
  //GammaY[30,88] := 0.940760751; GammaY[31,88] := 0.943772047; GammaY[32,88] := 0.946749063; GammaY[33,88] := 0.949694919; GammaY[34,88] := 0.95261251; GammaY[35,88] := 0.955504547; GammaY[36,88] := 0.958373598; GammaY[37,88] := 0.961222094; GammaY[38,88] := 0.964052321; GammaY[39,88] := 0.96686646;
  //GammaY[40,88] := 0.969666617; GammaY[41,88] := 0.972454821; GammaY[42,88] := 0.975233008; GammaY[43,88] := 0.97800306; GammaY[44,88] := 0.980766828; GammaY[45,88] := 0.983526127; GammaY[46,88] := 0.986282721; GammaY[47,88] := 0.989038347; GammaY[48,88] := 0.991794755; GammaY[49,88] := 0.994553687;
  //GammaY[50,88] := 0.997316866; GammaY[51,88] := 1.000085995; GammaY[52,88] := 1.002862866; GammaY[53,88] := 1.005649303; GammaY[54,88] := 1.008447081; GammaY[55,88] := 1.011258051; GammaY[56,88] := 1.014084128; GammaY[57,88] := 1.016927276; GammaY[58,88] := 1.019789531; GammaY[59,88] := 1.022673003;
  //GammaY[60,88] := 1.025579905; GammaY[61,88] := 1.028512555; GammaY[62,88] := 1.031473378; GammaY[63,88] := 1.034464944; GammaY[64,88] := 1.037489977; GammaY[65,88] := 1.040551366; GammaY[66,88] := 1.0436522; GammaY[67,88] := 1.046795797; GammaY[68,88] := 1.049985723; GammaY[69,88] := 1.05322583;
  //GammaY[70,88] := 1.056520294; GammaY[71,88] := 1.059873668; GammaY[72,88] := 1.063290934; GammaY[73,88] := 1.066777553; GammaY[74,88] := 1.070339557; GammaY[75,88] := 1.073983637; GammaY[76,88] := 1.077717241; GammaY[77,88] := 1.08154871; GammaY[78,88] := 1.085487453; GammaY[79,88] := 1.089544125;
  //GammaY[80,88] := 1.093730879; GammaY[81,88] := 1.098061686; GammaY[82,88] := 1.102552718; GammaY[83,88] := 1.107222853; GammaY[84,88] := 1.112094351; GammaY[85,88] := 1.117193732; GammaY[86,88] := 1.122552961; GammaY[87,88] := 1.128211102; GammaY[88,88] := 1.134216643; GammaY[89,88] := 1.140630859;
  //GammaY[90,88] := 1.147532857; GammaY[91,88] := 1.155027412; GammaY[92,88] := 1.16325769; GammaY[93,88] := 1.172427151; GammaY[94,88] := 1.182839991; GammaY[95,88] := 1.194983332; GammaY[96,88] := 1.209718152; GammaY[97,88] := 1.228825054; GammaY[98,88] := 1.257299679; GammaY[99,88] := 1.315528748;
  //GammaY[0,89] := 0.744016487; GammaY[1,89] := 0.782664999; GammaY[2,89] := 0.802789486; GammaY[3,89] := 0.816692702; GammaY[4,89] := 0.827632971; GammaY[5,89] := 0.836793827; GammaY[6,89] := 0.844754856; GammaY[7,89] := 0.851847214; GammaY[8,89] := 0.858279395; GammaY[9,89] := 0.864191826;
  //GammaY[10,89] := 0.869683938; GammaY[11,89] := 0.874828904; GammaY[12,89] := 0.879682244; GammaY[13,89] := 0.884287181; GammaY[14,89] := 0.88867807; GammaY[15,89] := 0.892882708; GammaY[16,89] := 0.896923944; GammaY[17,89] := 0.900820792; GammaY[18,89] := 0.904589291; GammaY[19,89] := 0.908243102;
  //GammaY[20,89] := 0.911793961; GammaY[21,89] := 0.915252043; GammaY[22,89] := 0.918626253; GammaY[23,89] := 0.921924432; GammaY[24,89] := 0.925153511; GammaY[25,89] := 0.928319686; GammaY[26,89] := 0.93142852; GammaY[27,89] := 0.934485016; GammaY[28,89] := 0.937493708; GammaY[29,89] := 0.940458744;
  //GammaY[30,89] := 0.943383924; GammaY[31,89] := 0.94627273; GammaY[32,89] := 0.949128378; GammaY[33,89] := 0.951953871; GammaY[34,89] := 0.954752011; GammaY[35,89] := 0.957525407; GammaY[36,89] := 0.960276511; GammaY[37,89] := 0.96300766; GammaY[38,89] := 0.96572107; GammaY[39,89] := 0.968418839;
  //GammaY[40,89] := 0.97110297; GammaY[41,89] := 0.97377541; GammaY[42,89] := 0.976438042; GammaY[43,89] := 0.979092671; GammaY[44,89] := 0.981741053; GammaY[45,89] := 0.984384928; GammaY[46,89] := 0.98702601; GammaY[47,89] := 0.989665967; GammaY[48,89] := 0.992306452; GammaY[49,89] := 0.994949134;
  //GammaY[50,89] := 0.997595686; GammaY[51,89] := 1.000247726; GammaY[52,89] := 1.002906971; GammaY[53,89] := 1.005575166; GammaY[54,89] := 1.008254011; GammaY[55,89] := 1.010945284; GammaY[56,89] := 1.013650806; GammaY[57,89] := 1.016372462; GammaY[58,89] := 1.019112191; GammaY[59,89] := 1.021872009;
  //GammaY[60,89] := 1.024654031; GammaY[61,89] := 1.027460465; GammaY[62,89] := 1.03029363; GammaY[63,89] := 1.033155982; GammaY[64,89] := 1.036050122; GammaY[65,89] := 1.038978808; GammaY[66,89] := 1.041944989; GammaY[67,89] := 1.044951831; GammaY[68,89] := 1.048002729; GammaY[69,89] := 1.051101354;
  //GammaY[70,89] := 1.054251695; GammaY[71,89] := 1.05745809; GammaY[72,89] := 1.060725285; GammaY[73,89] := 1.064058487; GammaY[74,89] := 1.067463448; GammaY[75,89] := 1.070946547; GammaY[76,89] := 1.074514879; GammaY[77,89] := 1.078176399; GammaY[78,89] := 1.081940067; GammaY[79,89] := 1.085816033;
  //GammaY[80,89] := 1.089815874; GammaY[81,89] := 1.093952898; GammaY[82,89] := 1.098242505; GammaY[83,89] := 1.102702684; GammaY[84,89] := 1.107354635; GammaY[85,89] := 1.112223608; GammaY[86,89] := 1.11734005; GammaY[87,89] := 1.122741152; GammaY[88,89] := 1.128473078; GammaY[89,89] := 1.134594165;
  //GammaY[90,89] := 1.141179717; GammaY[91,89] := 1.148329464; GammaY[92,89] := 1.156179671; GammaY[93,89] := 1.164923962; GammaY[94,89] := 1.174851792; GammaY[95,89] := 1.186426608; GammaY[96,89] := 1.200467401; GammaY[97,89] := 1.218667577; GammaY[98,89] := 1.245776573; GammaY[99,89] := 1.301136553;
  //GammaY[0,90] := 0.753971509; GammaY[1,90] := 0.791290802; GammaY[2,90] := 0.810699752; GammaY[3,90] := 0.824098987; GammaY[4,90] := 0.834637506; GammaY[5,90] := 0.843458569; GammaY[6,90] := 0.851121854; GammaY[7,90] := 0.857947078; GammaY[8,90] := 0.864135478; GammaY[9,90] := 0.869822576;
  //GammaY[10,90] := 0.875104311; GammaY[11,90] := 0.880051275; GammaY[12,90] := 0.884717038; GammaY[13,90] := 0.889143278; GammaY[14,90] := 0.893363128; GammaY[15,90] := 0.897403397; GammaY[16,90] := 0.90128611; GammaY[17,90] := 0.905029615; GammaY[18,90] := 0.908649356; GammaY[19,90] := 0.912158503;
  //GammaY[20,90] := 0.915568379; GammaY[21,90] := 0.918888789; GammaY[22,90] := 0.922128303; GammaY[23,90] := 0.925294475; GammaY[24,90] := 0.928394002; GammaY[25,90] := 0.931432835; GammaY[26,90] := 0.934416329; GammaY[27,90] := 0.937349317; GammaY[28,90] := 0.940236167; GammaY[29,90] := 0.943080856;
  //GammaY[30,90] := 0.945887043; GammaY[31,90] := 0.948658094; GammaY[32,90] := 0.951397097; GammaY[33,90] := 0.954106941; GammaY[34,90] := 0.956790322; GammaY[35,90] := 0.959449749; GammaY[36,90] := 0.96208758; GammaY[37,90] := 0.964706061; GammaY[38,90] := 0.967307324; GammaY[39,90] := 0.969893382;
  //GammaY[40,90] := 0.972466162; GammaY[41,90] := 0.975027534; GammaY[42,90] := 0.977579304; GammaY[43,90] := 0.980123205; GammaY[44,90] := 0.982660924; GammaY[45,90] := 0.985194129; GammaY[46,90] := 0.987724464; GammaY[47,90] := 0.990253529; GammaY[48,90] := 0.992782906; GammaY[49,90] := 0.995314197;
  //GammaY[50,90] := 0.997849003; GammaY[51,90] := 1.000388881; GammaY[52,90] := 1.002935465; GammaY[53,90] := 1.005490421; GammaY[54,90] := 1.008055384; GammaY[55,90] := 1.010632046; GammaY[56,90] := 1.013222159; GammaY[57,90] := 1.015827522; GammaY[58,90] := 1.018449989; GammaY[59,90] := 1.02109149;
  //GammaY[60,90] := 1.023754045; GammaY[61,90] := 1.026439758; GammaY[62,90] := 1.029150842; GammaY[63,90] := 1.031889643; GammaY[64,90] := 1.03465864; GammaY[65,90] := 1.037460466; GammaY[66,90] := 1.04029794; GammaY[67,90] := 1.043174083; GammaY[68,90] := 1.046092134; GammaY[69,90] := 1.049055597;
  //GammaY[70,90] := 1.052068276; GammaY[71,90] := 1.055134304; GammaY[72,90] := 1.058258201; GammaY[73,90] := 1.06144494; GammaY[74,90] := 1.064700002; GammaY[75,90] := 1.068029459; GammaY[76,90] := 1.071440084; GammaY[77,90] := 1.074939459; GammaY[78,90] := 1.078536118; GammaY[79,90] := 1.082239736;
  //GammaY[80,90] := 1.08606135; GammaY[81,90] := 1.090013633; GammaY[82,90] := 1.094111255; GammaY[83,90] := 1.098371354; GammaY[84,90] := 1.102814121; GammaY[85,90] := 1.107463613; GammaY[86,90] := 1.112348829; GammaY[87,90] := 1.11750519; GammaY[88,90] := 1.122976661; GammaY[89,90] := 1.12881879;
  //GammaY[90,90] := 1.135103275; GammaY[91,90] := 1.141925065; GammaY[92,90] := 1.149413883; GammaY[93,90] := 1.15775404; GammaY[94,90] := 1.16722103; GammaY[95,90] := 1.178255882; GammaY[96,90] := 1.191637843; GammaY[97,90] := 1.2089778; GammaY[98,90] := 1.234792328; GammaY[99,90] := 1.287438455;
  //GammaY[0,91] := 0.763578701; GammaY[1,91] := 0.799600087; GammaY[2,91] := 0.818312606; GammaY[3,91] := 0.831222219; GammaY[4,91] := 0.841370906; GammaY[5,91] := 0.849862565; GammaY[6,91] := 0.857237448; GammaY[7,91] := 0.863804081; GammaY[8,91] := 0.869756634; GammaY[9,91] := 0.875225844;
  //GammaY[10,91] := 0.880304249; GammaY[11,91] := 0.885059928; GammaY[12,91] := 0.889544537; GammaY[13,91] := 0.893798269; GammaY[14,91] := 0.897853064; GammaY[15,91] := 0.901734771; GammaY[16,91] := 0.905464607; GammaY[17,91] := 0.909060258; GammaY[18,91] := 0.912536625; GammaY[19,91] := 0.91590638;
  //GammaY[20,91] := 0.919180433; GammaY[21,91] := 0.922368244; GammaY[22,91] := 0.925478061; GammaY[23,91] := 0.928517163; GammaY[24,91] := 0.931491997; GammaY[25,91] := 0.934408296; GammaY[26,91] := 0.937271216; GammaY[27,91] := 0.940085411; GammaY[28,91] := 0.942855085; GammaY[29,91] := 0.945584068;
  //GammaY[30,91] := 0.948275878; GammaY[31,91] := 0.950933758; GammaY[32,91] := 0.953560676; GammaY[33,91] := 0.956159413; GammaY[34,91] := 0.95873256; GammaY[35,91] := 0.961282532; GammaY[36,91] := 0.963811596; GammaY[37,91] := 0.966321909; GammaY[38,91] := 0.968815521; GammaY[39,91] := 0.971294367;
  //GammaY[40,91] := 0.973760296; GammaY[41,91] := 0.976215104; GammaY[42,91] := 0.978660526; GammaY[43,91] := 0.981098226; GammaY[44,91] := 0.983529821; GammaY[45,91] := 0.985956912; GammaY[46,91] := 0.988381076; GammaY[47,91] := 0.990803835; GammaY[48,91] := 0.993226728; GammaY[49,91] := 0.995651289;
  //GammaY[50,91] := 0.998079033; GammaY[51,91] := 1.000511448; GammaY[52,91] := 1.002950112; GammaY[53,91] := 1.005396632; GammaY[54,91] := 1.007852556; GammaY[55,91] := 1.010319503; GammaY[56,91] := 1.012799147; GammaY[57,91] := 1.015293206; GammaY[58,91] := 1.017803452; GammaY[59,91] := 1.020331735;
  //GammaY[60,91] := 1.022879987; GammaY[61,91] := 1.025450215; GammaY[62,91] := 1.028044537; GammaY[63,91] := 1.030665193; GammaY[64,91] := 1.033314544; GammaY[65,91] := 1.035995103; GammaY[66,91] := 1.038709561; GammaY[67,91] := 1.041460796; GammaY[68,91] := 1.044251901; GammaY[69,91] := 1.047086222;
  //GammaY[70,91] := 1.04996739; GammaY[71,91] := 1.052899344; GammaY[72,91] := 1.055886399; GammaY[73,91] := 1.058933296; GammaY[74,91] := 1.062045256; GammaY[75,91] := 1.06522807; GammaY[76,91] := 1.068488193; GammaY[77,91] := 1.071832848; GammaY[78,91] := 1.075270176; GammaY[79,91] := 1.078809402;
  //GammaY[80,91] := 1.082461038; GammaY[81,91] := 1.086237167; GammaY[82,91] := 1.090151767; GammaY[83,91] := 1.094221161; GammaY[84,91] := 1.098464592; GammaY[85,91] := 1.102904976; GammaY[86,91] := 1.107569937; GammaY[87,91] := 1.112493224; GammaY[88,91] := 1.11771671; GammaY[89,91] := 1.123293302;
  //GammaY[90,91] := 1.129291285; GammaY[91,91] := 1.135801072; GammaY[92,91] := 1.142946185; GammaY[93,91] := 1.150902103; GammaY[94,91] := 1.159931099; GammaY[95,91] := 1.170452968; GammaY[96,91] := 1.183209322; GammaY[97,91] := 1.199732923; GammaY[98,91] := 1.224320012; GammaY[99,91] := 1.274398127;
  //GammaY[0,92] := 0.772846693; GammaY[1,92] := 0.807602146; GammaY[2,92] := 0.825637432; GammaY[3,92] := 0.838071743; GammaY[4,92] := 0.847842439; GammaY[5,92] := 0.856014989; GammaY[6,92] := 0.863110673; GammaY[7,92] := 0.869427123; GammaY[8,92] := 0.875151626; GammaY[9,92] := 0.880410252;
  //GammaY[10,92] := 0.885292231; GammaY[11,92] := 0.889863196; GammaY[12,92] := 0.894172942; GammaY[13,92] := 0.898260215; GammaY[14,92] := 0.902155792; GammaY[15,92] := 0.905884578; GammaY[16,92] := 0.909467032; GammaY[17,92] := 0.912920187; GammaY[18,92] := 0.916258401; GammaY[19,92] := 0.91949388;
  //GammaY[20,92] := 0.922637134; GammaY[21,92] := 0.925697274; GammaY[22,92] := 0.928682244; GammaY[23,92] := 0.931599052; GammaY[24,92] := 0.934453908; GammaY[25,92] := 0.937252337; GammaY[26,92] := 0.939999289; GammaY[27,92] := 0.942699244; GammaY[28,92] := 0.945356261; GammaY[29,92] := 0.94797402;
  //GammaY[30,92] := 0.950555907; GammaY[31,92] := 0.953105039; GammaY[32,92] := 0.955624274; GammaY[33,92] := 0.958116284; GammaY[34,92] := 0.960583564; GammaY[35,92] := 0.963028433; GammaY[36,92] := 0.965453069; GammaY[37,92] := 0.967859549; GammaY[38,92] := 0.970249839; GammaY[39,92] := 0.972625801;
  //GammaY[40,92] := 0.974989208; GammaY[41,92] := 0.977341787; GammaY[42,92] := 0.979685203; GammaY[43,92] := 0.982021041; GammaY[44,92] := 0.984350873; GammaY[45,92] := 0.986676238; GammaY[46,92] := 0.988998625; GammaY[47,92] := 0.991319502; GammaY[48,92] := 0.993640347; GammaY[49,92] := 0.995962631;
  //GammaY[50,92] := 0.998287789; GammaY[51,92] := 1.000617272; GammaY[52,92] := 1.002952584; GammaY[53,92] := 1.00529524; GammaY[54,92] := 1.007646747; GammaY[55,92] := 1.010008653; GammaY[56,92] := 1.01238255; GammaY[57,92] := 1.014770087; GammaY[58,92] := 1.017172956; GammaY[59,92] := 1.019592916;
  //GammaY[60,92] := 1.022031813; GammaY[61,92] := 1.024491566; GammaY[62,92] := 1.026974205; GammaY[63,92] := 1.029481868; GammaY[64,92] := 1.032016809; GammaY[65,92] := 1.03458143; GammaY[66,92] := 1.0371783; GammaY[67,92] := 1.039810161; GammaY[68,92] := 1.042479961; GammaY[69,92] := 1.045190898;
  //GammaY[70,92] := 1.047946424; GammaY[71,92] := 1.050750308; GammaY[72,92] := 1.053606671; GammaY[73,92] := 1.056520024; GammaY[74,92] := 1.059495351; GammaY[75,92] := 1.06253818; GammaY[76,92] := 1.065654658; GammaY[77,92] := 1.06885167; GammaY[78,92] := 1.07213698; GammaY[79,92] := 1.075519375;
  //GammaY[80,92] := 1.079008881; GammaY[81,92] := 1.082617017; GammaY[82,92] := 1.086357104; GammaY[83,92] := 1.090244704; GammaY[84,92] := 1.094298148; GammaY[85,92] := 1.098539271; GammaY[86,92] := 1.102994395; GammaY[87,92] := 1.107695672; GammaY[88,92] := 1.112682998; GammaY[89,92] := 1.118006776;
  //GammaY[90,92] := 1.123732054; GammaY[91,92] := 1.129944947; GammaY[92,92] := 1.136763093; GammaY[93,92] := 1.144353601; GammaY[94,92] := 1.152966207; GammaY[95,92] := 1.16300059; GammaY[96,92] := 1.175162707; GammaY[97,92] := 1.190911338; GammaY[98,92] := 1.214334159; GammaY[99,92] := 1.261981344;
  //GammaY[0,93] := 0.781784258; GammaY[1,93] := 0.815306244; GammaY[2,93] := 0.832683514; GammaY[3,93] := 0.844656759; GammaY[4,93] := 0.854061187; GammaY[5,93] := 0.861924774; GammaY[6,93] := 0.868750323; GammaY[7,93] := 0.874824865; GammaY[8,93] := 0.880328966; GammaY[9,93] := 0.885384166;
  //GammaY[10,93] := 0.890076472; GammaY[11,93] := 0.894469142; GammaY[12,93] := 0.898610163; GammaY[13,93] := 0.902536864; GammaY[14,93] := 0.906278907; GammaY[15,93] := 0.909860283; GammaY[16,93] := 0.913300704; GammaY[17,93] := 0.916616568; GammaY[18,93] := 0.919821701; GammaY[19,93] := 0.922927872;
  //GammaY[20,93] := 0.925945194; GammaY[21,93] := 0.928882442; GammaY[22,93] := 0.931747264; GammaY[23,93] := 0.934546406; GammaY[24,93] := 0.937285847; GammaY[25,93] := 0.939970897; GammaY[26,93] := 0.942606335; GammaY[27,93] := 0.945196471; GammaY[28,93] := 0.9477452; GammaY[29,93] := 0.950256066;
  //GammaY[30,93] := 0.952732327; GammaY[31,93] := 0.955176982; GammaY[32,93] := 0.957592778; GammaY[33,93] := 0.959982287; GammaY[34,93] := 0.962347906; GammaY[35,93] := 0.964691864; GammaY[36,93] := 0.967016255; GammaY[37,93] := 0.969323074; GammaY[38,93] := 0.971614212; GammaY[39,93] := 0.973891444;
  //GammaY[40,93] := 0.976156495; GammaY[41,93] := 0.97841102; GammaY[42,93] := 0.980656602; GammaY[43,93] := 0.982894769; GammaY[44,93] := 0.985127029; GammaY[45,93] := 0.987354859; GammaY[46,93] := 0.989579677; GammaY[47,93] := 0.99180291; GammaY[48,93] := 0.994025975; GammaY[49,93] := 0.996250248;
  //GammaY[50,93] := 0.998477138; GammaY[51,93] := 1.000708032; GammaY[52,93] := 1.002944354; GammaY[53,93] := 1.005187562; GammaY[54,93] := 1.00743909; GammaY[55,93] := 1.00970042; GammaY[56,93] := 1.011973084; GammaY[57,93] := 1.014258656; GammaY[58,93] := 1.016558753; GammaY[59,93] := 1.018875062;
  //GammaY[60,93] := 1.021209345; GammaY[61,93] := 1.023563434; GammaY[62,93] := 1.025939259; GammaY[63,93] := 1.028338863; GammaY[64,93] := 1.0307644; GammaY[65,93] := 1.033218168; GammaY[66,93] := 1.035702621; GammaY[67,93] := 1.038220373; GammaY[68,93] := 1.040774244; GammaY[69,93] := 1.043367283;
  //GammaY[70,93] := 1.046002783; GammaY[71,93] := 1.048684333; GammaY[72,93] := 1.051415863; GammaY[73,93] := 1.05420168; GammaY[74,93] := 1.05704654; GammaY[75,93] := 1.059955716; GammaY[76,93] := 1.062935069; GammaY[77,93] := 1.065991171; GammaY[78,93] := 1.069131418; GammaY[79,93] := 1.072364184;
  //GammaY[80,93] := 1.075699033; GammaY[81,93] := 1.079146937; GammaY[82,93] := 1.082720604; GammaY[83,93] := 1.086434866; GammaY[84,93] := 1.090307195; GammaY[85,93] := 1.094358406; GammaY[86,93] := 1.098613579; GammaY[87,93] := 1.103103352; GammaY[88,93] := 1.10786574; GammaY[89,93] := 1.11294877;
  //GammaY[90,93] := 1.118414417; GammaY[91,93] := 1.124344726; GammaY[92,93] := 1.130851755; GammaY[93,93] := 1.138094671; GammaY[94,93] := 1.146311327; GammaY[95,93] := 1.155882329; GammaY[96,93] := 1.167479847; GammaY[97,93] := 1.182492574; GammaY[98,93] := 1.204810684; GammaY[99,93] := 1.250155721;
  //GammaY[0,94] := 0.790400244; GammaY[1,94] := 0.822721544; GammaY[2,94] := 0.839459979; GammaY[3,94] := 0.850986279; GammaY[4,94] := 0.860036009; GammaY[5,94] := 0.86760063; GammaY[6,94] := 0.874164959; GammaY[7,94] := 0.880005699; GammaY[8,94] := 0.88529689; GammaY[9,94] := 0.890155664;
  //GammaY[10,94] := 0.894664896; GammaY[11,94] := 0.898885537; GammaY[12,94] := 0.902863816; GammaY[13,94] := 0.906635692; GammaY[14,94] := 0.910229737; GammaY[15,94] := 0.913669056; GammaY[16,94] := 0.916972634; GammaY[17,94] := 0.920156259; GammaY[18,94] := 0.923233247; GammaY[19,94] := 0.926214927;
  //GammaY[20,94] := 0.929111038; GammaY[21,94] := 0.931930024; GammaY[22,94] := 0.934679248; GammaY[23,94] := 0.937365203; GammaY[24,94] := 0.939993644; GammaY[25,94] := 0.942569679; GammaY[26,94] := 0.945097909; GammaY[27,94] := 0.947582479; GammaY[28,94] := 0.950027128; GammaY[29,94] := 0.952435281;
  //GammaY[30,94] := 0.954810071; GammaY[31,94] := 0.957154361; GammaY[32,94] := 0.959470813; GammaY[33,94] := 0.9617619; GammaY[34,94] := 0.964029911; GammaY[35,94] := 0.966276989; GammaY[36,94] := 0.968505161; GammaY[37,94] := 0.970716344; GammaY[38,94] := 0.972912331; GammaY[39,94] := 0.975094849;
  //GammaY[40,94] := 0.977265557; GammaY[41,94] := 0.979426027; GammaY[42,94] := 0.981577774; GammaY[43,94] := 0.983722285; GammaY[44,94] := 0.985861009; GammaY[45,94] := 0.98799533; GammaY[46,94] := 0.990126638; GammaY[47,94] := 0.992256302; GammaY[48,94] := 0.994385649; GammaY[49,94] := 0.996516028;
  //GammaY[50,94] := 0.998648789; GammaY[51,94] := 1.000785227; GammaY[52,94] := 1.002926728; GammaY[53,94] := 1.005074702; GammaY[54,94] := 1.007230512; GammaY[55,94] := 1.009395573; GammaY[56,94] := 1.01157134; GammaY[57,94] := 1.013759321; GammaY[58,94] := 1.015961067; GammaY[59,94] := 1.018178187;
  //GammaY[60,94] := 1.020412364; GammaY[61,94] := 1.02266536; GammaY[62,94] := 1.024939016; GammaY[63,94] := 1.027235282; GammaY[64,94] := 1.029556216; GammaY[65,94] := 1.031904002; GammaY[66,94] := 1.034280984; GammaY[67,94] := 1.03668966; GammaY[68,94] := 1.039132726; GammaY[69,94] := 1.04161309;
  //GammaY[70,94] := 1.044133897; GammaY[71,94] := 1.046698577; GammaY[72,94] := 1.04931087; GammaY[73,94] := 1.051974886; GammaY[74,94] := 1.054695163; GammaY[75,94] := 1.057476723; GammaY[76,94] := 1.060325165; GammaY[77,94] := 1.063246757; GammaY[78,94] := 1.066248551; GammaY[79,94] := 1.069338541;
  //GammaY[80,94] := 1.072525835; GammaY[81,94] := 1.075820898; GammaY[82,94] := 1.07923584; GammaY[83,94] := 1.082784806; GammaY[84,94] := 1.086484457; GammaY[85,94] := 1.090354629; GammaY[86,94] := 1.094419233; GammaY[87,94] := 1.09870747; GammaY[88,94] := 1.103255568; GammaY[89,94] := 1.108109295;
  //GammaY[90,94] := 1.113327711; GammaY[91,94] := 1.118988997; GammaY[92,94] := 1.12519992; GammaY[93,94] := 1.132112121; GammaY[94,94] := 1.139952167; GammaY[95,94] := 1.149082585; GammaY[96,94] := 1.160143517; GammaY[97,94] := 1.174457228; GammaY[98,94] := 1.195726804; GammaY[99,94] := 1.238890984;
  //GammaY[0,95] := 0.798703557; GammaY[1,95] := 0.829857131; GammaY[2,95] := 0.845975796; GammaY[3,95] := 0.857069109; GammaY[4,95] := 0.865775552; GammaY[5,95] := 0.873051041; GammaY[6,95] := 0.879362894; GammaY[7,95] := 0.884977776; GammaY[8,95] := 0.890063384; GammaY[9,95] := 0.894732571;
  //GammaY[10,95] := 0.89906517; GammaY[11,95] := 0.90311989; GammaY[12,95] := 0.906941254; GammaY[13,95] := 0.910563895; GammaY[14,95] := 0.914015324; GammaY[15,95] := 0.917317788; GammaY[16,95] := 0.92048956; GammaY[17,95] := 0.923545852; GammaY[18,95] := 0.926499481; GammaY[19,95] := 0.929361341;
  //GammaY[20,95] := 0.932140811; GammaY[21,95] := 0.934846019; GammaY[22,95] := 0.93748405; GammaY[23,95] := 0.940061151; GammaY[24,95] := 0.942582855; GammaY[25,95] := 0.945054087; GammaY[26,95] := 0.947479273; GammaY[27,95] := 0.949862383; GammaY[28,95] := 0.952207032; GammaY[29,95] := 0.954516512;
  //GammaY[30,95] := 0.956793825; GammaY[31,95] := 0.959041728; GammaY[32,95] := 0.961262781; GammaY[33,95] := 0.963459363; GammaY[34,95] := 0.965633664; GammaY[35,95] := 0.967787758; GammaY[36,95] := 0.969923595; GammaY[37,95] := 0.972042992; GammaY[38,95] := 0.974147692; GammaY[39,95] := 0.976239359;
  //GammaY[40,95] := 0.978319556; GammaY[41,95] := 0.980389811; GammaY[42,95] := 0.982451595; GammaY[43,95] := 0.984506318; GammaY[44,95] := 0.98655535; GammaY[45,95] := 0.988600046; GammaY[46,95] := 0.990641741; GammaY[47,95] := 0.992681716; GammaY[48,95] := 0.994721273; GammaY[49,95] := 0.996761705;
  //GammaY[50,95] := 0.998804274; GammaY[51,95] := 1.000850249; GammaY[52,95] := 1.002900958; GammaY[53,95] := 1.004957725; GammaY[54,95] := 1.007021867; GammaY[55,95] := 1.009094751; GammaY[56,95] := 1.011177763; GammaY[57,95] := 1.013272346; GammaY[58,95] := 1.015379977; GammaY[59,95] := 1.017502192;
  //GammaY[60,95] := 1.019640602; GammaY[61,95] := 1.021796883; GammaY[62,95] := 1.023972803; GammaY[63,95] := 1.026170227; GammaY[64,95] := 1.028391117; GammaY[65,95] := 1.03063757; GammaY[66,95] := 1.032911811; GammaY[67,95] := 1.035216226; GammaY[68,95] := 1.037553389; GammaY[69,95] := 1.03992607;
  //GammaY[70,95] := 1.042337278; GammaY[71,95] := 1.044790288; GammaY[72,95] := 1.047288667; GammaY[73,95] := 1.049836345; GammaY[74,95] := 1.052437641; GammaY[75,95] := 1.055097346; GammaY[76,95] := 1.057820803; GammaY[77,95] := 1.060613986; GammaY[78,95] := 1.063483626; GammaY[79,95] := 1.066437348;
  //GammaY[80,95] := 1.06948384; GammaY[81,95] := 1.072633084; GammaY[82,95] := 1.075896628; GammaY[83,95] := 1.079287949; GammaY[84,95] := 1.082822934; GammaY[85,95] := 1.086520501; GammaY[86,95] := 1.090403445; GammaY[87,95] := 1.094499607; GammaY[88,95] := 1.098843519; GammaY[89,95] := 1.103478804;
  //GammaY[90,95] := 1.108461751; GammaY[91,95] := 1.11386687; GammaY[92,95] := 1.119795914; GammaY[93,95] := 1.126393374; GammaY[94,95] := 1.133875123; GammaY[95,95] := 1.142586529; GammaY[96,95] := 1.153137357; GammaY[97,95] := 1.166786905; GammaY[98,95] := 1.187060958; GammaY[99,95] := 1.228158373;
  //GammaY[0,96] := 0.806703107; GammaY[1,96] := 0.836721966; GammaY[2,96] := 0.852239769; GammaY[3,96] := 0.862913871; GammaY[4,96] := 0.87128825; GammaY[5,96] := 0.878284255; GammaY[6,96] := 0.884352198; GammaY[7,96] := 0.889748991; GammaY[8,96] := 0.894636176; GammaY[9,96] := 0.899122448;
  //GammaY[10,96] := 0.903284691; GammaY[11,96] := 0.907179438; GammaY[12,96] := 0.910849559; GammaY[13,96] := 0.914328398; GammaY[14,96] := 0.917642439; GammaY[15,96] := 0.920813099; GammaY[16,96] := 0.923857966; GammaY[17,96] := 0.926791681; GammaY[18,96] := 0.929626572; GammaY[19,96] := 0.932373137;
  //GammaY[20,96] := 0.935040398; GammaY[21,96] := 0.937636161; GammaY[22,96] := 0.940167258; GammaY[23,96] := 0.942639701; GammaY[24,96] := 0.945058799; GammaY[25,96] := 0.947429296; GammaY[26,96] := 0.949755449; GammaY[27,96] := 0.952041075; GammaY[28,96] := 0.954289651; GammaY[29,96] := 0.956504336;
  //GammaY[30,96] := 0.958688021; GammaY[31,96] := 0.960843372; GammaY[32,96] := 0.962972837; GammaY[33,96] := 0.965078685; GammaY[34,96] := 0.967163043; GammaY[35,96] := 0.969227905; GammaY[36,96] := 0.97127512; GammaY[37,96] := 0.973306456; GammaY[38,96] := 0.975323589; GammaY[39,96] := 0.977328092;
  //GammaY[40,96] := 0.979321488; GammaY[41,96] := 0.981305245; GammaY[42,96] := 0.98328075; GammaY[43,96] := 0.98524937; GammaY[44,96] := 0.987212439; GammaY[45,96] := 0.98917123; GammaY[46,96] := 0.991127021; GammaY[47,96] := 0.993081069; GammaY[48,96] := 0.995034593; GammaY[49,96] := 0.996988832;
  //GammaY[50,96] := 0.998945023; GammaY[51,96] := 1.000904351; GammaY[52,96] := 1.00286809; GammaY[53,96] := 1.004837538; GammaY[54,96] := 1.006813931; GammaY[55,96] := 1.008798567; GammaY[56,96] := 1.010792781; GammaY[57,96] := 1.012797951; GammaY[58,96] := 1.014815503; GammaY[59,96] := 1.016846903;
  //GammaY[60,96] := 1.018893688; GammaY[61,96] := 1.020957459; GammaY[62,96] := 1.023039897; GammaY[63,96] := 1.025142784; GammaY[64,96] := 1.027267996; GammaY[65,96] := 1.029417536; GammaY[66,96] := 1.031593538; GammaY[67,96] := 1.03379828; GammaY[68,96] := 1.036034217; GammaY[69,96] := 1.038303993;
  //GammaY[70,96] := 1.040610472; GammaY[71,96] := 1.04295678; GammaY[72,96] := 1.045346326; GammaY[73,96] := 1.047782857; GammaY[74,96] := 1.050270504; GammaY[75,96] := 1.05281384; GammaY[76,96] := 1.055417955; GammaY[77,96] := 1.058088545; GammaY[78,96] := 1.060832035; GammaY[79,96] := 1.063655693;
  //GammaY[80,96] := 1.066567811; GammaY[81,96] := 1.069577912; GammaY[82,96] := 1.072697011; GammaY[83,96] := 1.075937959; GammaY[84,96] := 1.079315904; GammaY[85,96] := 1.082848883; GammaY[86,96] := 1.086558634; GammaY[87,96] := 1.090471705; GammaY[88,96] := 1.094621021; GammaY[89,96] := 1.099048169;
  //GammaY[90,96] := 1.103806805; GammaY[91,96] := 1.108967953; GammaY[92,96] := 1.114628597; GammaY[93,96] := 1.120926457; GammaY[94,96] := 1.128067249; GammaY[95,96] := 1.136380058; GammaY[96,96] := 1.146445833; GammaY[97,96] := 1.159464153; GammaY[98,96] := 1.178792733; GammaY[99,96] := 1.217930827;
  //GammaY[0,97] := 0.814407787; GammaY[1,97] := 0.843324861; GammaY[2,97] := 0.858260513; GammaY[3,97] := 0.868528972; GammaY[4,97] := 0.87658231; GammaY[5,97] := 0.883308285; GammaY[6,97] := 0.889140696; GammaY[7,97] := 0.894326991; GammaY[8,97] := 0.899022738; GammaY[9,97] := 0.903332594;
  //GammaY[10,97] := 0.907330592; GammaY[11,97] := 0.911071153; GammaY[12,97] := 0.914595544; GammaY[13,97] := 0.917935859; GammaY[14,97] := 0.921117589; GammaY[15,97] := 0.924161344; GammaY[16,97] := 0.927084051; GammaY[17,97] := 0.929899801; GammaY[18,97] := 0.932620449; GammaY[19,97] := 0.935256098;
  //GammaY[20,97] := 0.937815422; GammaY[21,97] := 0.940305942; GammaY[22,97] := 0.942734229; GammaY[23,97] := 0.94510605; GammaY[24,97] := 0.947426526; GammaY[25,97] := 0.949700216; GammaY[26,97] := 0.951931206; GammaY[27,97] := 0.954123184; GammaY[28,97] := 0.956279475; GammaY[29,97] := 0.958403123;
  //GammaY[30,97] := 0.960496913; GammaY[31,97] := 0.962563387; GammaY[32,97] := 0.96460491; GammaY[33,97] := 0.966623677; GammaY[34,97] := 0.968621712; GammaY[35,97] := 0.970600928; GammaY[36,97] := 0.972563125; GammaY[37,97] := 0.974509977; GammaY[38,97] := 0.976443094; GammaY[39,97] := 0.978364009;
  //GammaY[40,97] := 0.980274161; GammaY[41,97] := 0.982174958; GammaY[42,97] := 0.984067754; GammaY[43,97] := 0.985953839; GammaY[44,97] := 0.987834489; GammaY[45,97] := 0.989710953; GammaY[46,97] := 0.991584427; GammaY[47,97] := 0.993456119; GammaY[48,97] := 0.995327222; GammaY[49,97] := 0.997198896;
  //GammaY[50,97] := 0.999072301; GammaY[51,97] := 1.000948646; GammaY[52,97] := 1.002829132; GammaY[53,97] := 1.004714944; GammaY[54,97] := 1.006607309; GammaY[55,97] := 1.008507466; GammaY[56,97] := 1.010416695; GammaY[57,97] := 1.012336305; GammaY[58,97] := 1.014267649; GammaY[59,97] := 1.016212136;
  //GammaY[60,97] := 1.018171237; GammaY[61,97] := 1.020146492; GammaY[62,97] := 1.022139504; GammaY[63,97] := 1.024151975; GammaY[64,97] := 1.026185696; GammaY[65,97] := 1.028242573; GammaY[66,97] := 1.030324641; GammaY[67,97] := 1.032434084; GammaY[68,97] := 1.034573248; GammaY[69,97] := 1.036744655;
  //GammaY[70,97] := 1.038951048; GammaY[71,97] := 1.041195401; GammaY[72,97] := 1.043480967; GammaY[73,97] := 1.045811324; GammaY[74,97] := 1.048190408; GammaY[75,97] := 1.050622588; GammaY[76,97] := 1.053112727; GammaY[77,97] := 1.055666263; GammaY[78,97] := 1.058289317; GammaY[79,97] := 1.060988824;
  //GammaY[80,97] := 1.063772691; GammaY[81,97] := 1.066650005; GammaY[82,97] := 1.069631275; GammaY[83,97] := 1.072728763; GammaY[84,97] := 1.075956914; GammaY[85,97] := 1.079332928; GammaY[86,97] := 1.082877534; GammaY[87,97] := 1.086616049; GammaY[88,97] := 1.090579874; GammaY[89,97] := 1.094808664;
  //GammaY[90,97] := 1.099353578; GammaY[91,97] := 1.104282324; GammaY[92,97] := 1.10968736; GammaY[93,97] := 1.115699964; GammaY[94,97] := 1.122516222; GammaY[95,97] := 1.130449767; GammaY[96,97] := 1.140054196; GammaY[97,97] := 1.15247243; GammaY[98,97] := 1.170902802; GammaY[99,97] := 1.208182952;
  //GammaY[0,98] := 0.821826436; GammaY[1,98] := 0.84967449; GammaY[2,98] := 0.864046458; GammaY[3,98] := 0.873922612; GammaY[4,98] := 0.881665712; GammaY[5,98] := 0.888130908; GammaY[6,98] := 0.893735973; GammaY[7,98] := 0.898719176; GammaY[8,98] := 0.903230291; GammaY[9,98] := 0.907370067;
  //GammaY[10,98] := 0.911209767; GammaY[11,98] := 0.914801766; GammaY[12,98] := 0.918185782; GammaY[13,98] := 0.921392696; GammaY[14,98] := 0.924447039; GammaY[15,98] := 0.927368639; GammaY[16,98] := 0.930173784; GammaY[17,98] := 0.932876023; GammaY[18,98] := 0.93548677; GammaY[19,98] := 0.938015739;
  //GammaY[20,98] := 0.940471274; GammaY[21,98] := 0.942860612; GammaY[22,98] := 0.945190057; GammaY[23,98] := 0.947465171; GammaY[24,98] := 0.949690873; GammaY[25,98] := 0.951871546; GammaY[26,98] := 0.954011125; GammaY[27,98] := 0.956113137; GammaY[28,98] := 0.958180796; GammaY[29,98] := 0.960217029;
  //GammaY[30,98] := 0.962224492; GammaY[31,98] := 0.964205648; GammaY[32,98] := 0.966162763; GammaY[33,98] := 0.968097938; GammaY[34,98] := 0.970013139; GammaY[35,98] := 0.971910183; GammaY[36,98] := 0.973790797; GammaY[37,98] := 0.975656612; GammaY[38,98] := 0.97750915; GammaY[39,98] := 0.979349883;
  //GammaY[40,98] := 0.981180215; GammaY[41,98] := 0.983001474; GammaY[42,98] := 0.98481496; GammaY[43,98] := 0.986621932; GammaY[44,98] := 0.988423592; GammaY[45,98] := 0.990221134; GammaY[46,98] := 0.992015734; GammaY[47,98] := 0.993808522; GammaY[48,98] := 0.99560064; GammaY[49,98] := 0.997393229;
  //GammaY[50,98] := 0.999187365; GammaY[51,98] := 1.000984214; GammaY[52,98] := 1.002784956; GammaY[53,98] := 1.004590701; GammaY[54,98] := 1.006402613; GammaY[55,98] := 1.008221884; GammaY[56,98] := 1.010049741; GammaY[57,98] := 1.011887446; GammaY[58,98] := 1.013736295; GammaY[59,98] := 1.015597628;
  //GammaY[60,98] := 1.017472842; GammaY[61,98] := 1.01936341; GammaY[62,98] := 1.021270867; GammaY[63,98] := 1.023196839; GammaY[64,98] := 1.025143047; GammaY[65,98] := 1.027111313; GammaY[66,98] := 1.029103575; GammaY[67,98] := 1.031121911; GammaY[68,98] := 1.033168562; GammaY[69,98] := 1.035245939;
  //GammaY[70,98] := 1.037356659; GammaY[71,98] := 1.039503572; GammaY[72,98] := 1.041689784; GammaY[73,98] := 1.043918701; GammaY[74,98] := 1.046194081; GammaY[75,98] := 1.048520091; GammaY[76,98] := 1.050901373; GammaY[77,98] := 1.053343121; GammaY[78,98] := 1.055851175; GammaY[79,98] := 1.058432157;
  //GammaY[80,98] := 1.061093608; GammaY[81,98] := 1.063844191; GammaY[82,98] := 1.066693931; GammaY[83,98] := 1.069654529; GammaY[84,98] := 1.072739771; GammaY[85,98] := 1.075966066; GammaY[86,98] := 1.079353179; GammaY[87,98] := 1.082925253; GammaY[88,98] := 1.086712238; GammaY[89,98] := 1.090751956;
  //GammaY[90,98] := 1.095093194; GammaY[91,98] := 1.099800516; GammaY[92,98] := 1.104962072; GammaY[93,98] := 1.110703028; GammaY[94,98] := 1.11721032; GammaY[95,98] := 1.124782908; GammaY[96,98] := 1.133948428; GammaY[97,98] := 1.145796031; GammaY[98,98] := 1.163372857; GammaY[99,98] := 1.198890279;
  //GammaY[0,99] := 0.828967824; GammaY[1,99] := 0.855779361; GammaY[2,99] := 0.869605833; GammaY[3,99] := 0.879102769; GammaY[4,99] := 0.88654621; GammaY[5,99] := 0.892759667; GammaY[6,99] := 0.898145374; GammaY[7,99] := 0.902932698; GammaY[8,99] := 0.907265808; GammaY[9,99] := 0.911241668;
  //GammaY[10,99] := 0.914928843; GammaY[11,99] := 0.918377744; GammaY[12,99] := 0.921626579; GammaY[13,99] := 0.924705062; GammaY[14,99] := 0.927636789; GammaY[15,99] := 0.930440838; GammaY[16,99] := 0.933132871; GammaY[17,99] := 0.935725923; GammaY[18,99] := 0.938230972; GammaY[19,99] := 0.940657357;
  //GammaY[20,99] := 0.943013105; GammaY[21,99] := 0.945305169; GammaY[22,99] := 0.947539626; GammaY[23,99] := 0.949721808; GammaY[24,99] := 0.951856448; GammaY[25,99] := 0.953947761; GammaY[26,99] := 0.955999523; GammaY[27,99] := 0.958015143; GammaY[28,99] := 0.959997691; GammaY[29,99] := 0.961949978;
  //GammaY[30,99] := 0.963874571; GammaY[31,99] := 0.965773829; GammaY[32,99] := 0.967649936; GammaY[33,99] := 0.969504899; GammaY[34,99] := 0.971340603; GammaY[35,99] := 0.973158814; GammaY[36,99] := 0.974961171; GammaY[37,99] := 0.976749239; GammaY[38,99] := 0.978524502; GammaY[39,99] := 0.980288348;
  //GammaY[40,99] := 0.982042127; GammaY[41,99] := 0.983787135; GammaY[42,99] := 0.985524595; GammaY[43,99] := 0.987255717; GammaY[44,99] := 0.988981666; GammaY[45,99] := 0.990703583; GammaY[46,99] := 0.992422592; GammaY[47,99] := 0.994139777; GammaY[48,99] := 0.995856234; GammaY[49,99] := 0.997573053;
  //GammaY[50,99] := 0.999291269; GammaY[51,99] := 1.00101199; GammaY[52,99] := 1.002736345; GammaY[53,99] := 1.004465409; GammaY[54,99] := 1.006200295; GammaY[55,99] := 1.007942147; GammaY[56,99] := 1.009692128; GammaY[57,99] := 1.011451437; GammaY[58,99] := 1.013221315; GammaY[59,99] := 1.015003055;
  //GammaY[60,99] := 1.016798001; GammaY[61,99] := 1.018607553; GammaY[62,99] := 1.020433177; GammaY[63,99] := 1.02227642; GammaY[64,99] := 1.024138921; GammaY[65,99] := 1.026022429; GammaY[66,99] := 1.027928803; GammaY[67,99] := 1.029860026; GammaY[68,99] := 1.031818238; GammaY[69,99] := 1.033805739;
  //GammaY[70,99] := 1.035825023; GammaY[71,99] := 1.037878807; GammaY[72,99] := 1.039970062; GammaY[73,99] := 1.042102047; GammaY[74,99] := 1.044278352; GammaY[75,99] := 1.046502948; GammaY[76,99] := 1.04878026; GammaY[77,99] := 1.051115243; GammaY[78,99] := 1.053513479; GammaY[79,99] := 1.055981286;
  //GammaY[80,99] := 1.058525866; GammaY[81,99] := 1.06115548; GammaY[82,99] := 1.063879691; GammaY[83,99] := 1.066709663; GammaY[84,99] := 1.069658549; GammaY[85,99] := 1.072742006; GammaY[86,99] := 1.075978894; GammaY[87,99] := 1.079392241; GammaY[88,99] := 1.083010609; GammaY[89,99] := 1.086870077;
  //GammaY[90,99] := 1.091017179; GammaY[91,99] := 1.095513492; GammaY[92,99] := 1.100443081; GammaY[93,99] := 1.105925292; GammaY[94,99] := 1.112138369; GammaY[95,99] := 1.119367348; GammaY[96,99] := 1.128115207; GammaY[97,99] := 1.139420051; GammaY[98,99] := 1.156185549; GammaY[99,99] := 1.190030319;
  InitGammaX;
  InitGammaY;
  InitializeGammaTable;
end.
