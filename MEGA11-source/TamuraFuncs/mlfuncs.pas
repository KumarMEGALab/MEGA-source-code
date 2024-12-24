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

unit MLFuncs;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math, MegaConsts, MegaUtils;


function TN93MCLDistanceParams(Seqs: TStringList; D: PDistanceMatrix; var k1,k2: extended; f: PArrayOfInt): boolean;
function TN93MCLDistance(Seqs: TStringList; D: PDistanceMatrix; k1,k2: extended; f: PArrayOfInt): boolean;
function TN93GammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k1,k2,gamma: extended; f: PArrayOfInt): boolean;
function HKYMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k: extended; f: PArrayOfInt): boolean;
function HKYGammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k,gamma: extended; f: PArrayOfInt): boolean;
function T3MCLDistance(Seqs: TStringList; D: PDistanceMatrix; k: extended; f: PArrayOfInt): boolean;
function T3GammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k,gamma: extended; f: PArrayOfInt): boolean;
function K2MCLDistance(Seqs: TStringList; D: PDistanceMatrix; k: extended; f: PArrayOfInt): boolean;
function K2GammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k,gamma: extended; f: PArrayOfInt): boolean;

implementation


//////////////////////////
//  MCL distance
/////////////////////////


function TN93Matrix(P,Q,D: PDistanceMatrix; n: integer; pA,pT,pC,pG: double; var k1,k2: extended; computeparams: boolean): double;
var
  pR,pY: double;

  function P1(i,j: integer): double;
  begin
    result := P[i,j];
  end;

  function P2(i,j: integer): double;
  begin
    result := P[j,i];
  end;

  function R(i,j: integer): double;
  begin
    result := 1-P[i,j]-P[j,i]-Q[i,j];
  end;

  function EP1(i,j: integer):double;
  begin
    if pR > 0 then
      result := 2*pA*pG/pR*(pR +pY*exp(-2*D[i,j]) -exp(-2*(pR*k1 +pY)*D[i,j]))
    else
      result := 0;
  end;

  function EP2(i,j: integer):double;
  begin
    if pY > 0 then
      result := 2*pT*pC/pY*(pY +pR*exp(-2*D[i,j]) -exp(-2*(pY*k2 +pR)*D[i,j]))
    else
      result := 0;
  end;

  function EQ(i,j: integer):double;
  begin
    result := 2*pR*pY*(1 -exp(-2*D[i,j]));
  end;

  function dP1dk1(i,j: integer):double;
  begin
    result := 4*pA*pG*exp(-2*(pR*k1 +pY)*D[i,j])*D[i,j];
  end;

  function dP1dx(i,j: integer):double;
  begin
    result := -4*pA*pG/pR*(pY*exp(-2*D[i,j]) -(pR*k1 +pY)*exp(-2*(pR*k1 +pY)*D[i,j]));
  end;

  function dP2dk2(i,j: integer):double;
  begin
    result := 4*pT*pC*exp(-2*(pY*k2 +pR)*D[i,j])*D[i,j];
  end;

  function dP2dx(i,j: integer):double;
  begin
    result := -4*pT*pC/pY*(pR*exp(-2*D[i,j]) -(pY*k2 +pR)*exp(-2*(pY*k2 +pR)*D[i,j]));
  end;

  function dQdx(i,j: integer):double;
  begin
    result := 4*pR*pY*exp(-2*D[i,j]);
  end;

  function ddP1dx(i,j: integer):double;
  begin
    result := 8*pA*pG/pR*(pY*exp(-2*D[i,j]) -(pR*k1 +pY)*(pR*k1 +pY)*exp(-2*(pR*k1 +pY)*D[i,j]));
  end;

  function ddP1dk1(i,j: integer):double;
  begin
    result := -8*pR*pA*pG*exp(-2*(pR*k1 +pY)*D[i,j])*D[i,j]*D[i,j];
  end;

  function ddP2dx(i,j: integer):double;
  begin
    result := 8*pT*pC/pY*(pR*exp(-2*D[i,j]) -(pY*k2 +pR)*(pY*k2 +pR)*exp(-2*(pY*k2 +pR)*D[i,j]));
  end;

  function ddP2dk2(i,j: integer):double;
  begin
    result := -8*pY*pT*pC*EXP(-2*(pY*k2 +pR)*D[i,j])*D[i,j]*D[i,j];
  end;

  function ddQdx(i,j: integer):double;
  begin
    result := -8*pR*pY*exp(-2*D[i,j]);
  end;

  procedure IterateKappa;
  var
    i,j: integer;
    dP1,ddP1,dP2,ddP2,ER,s,dLdk1,ddLdk1,dLdk2,ddLdk2: double;
  begin
    dLdk1  := 0;
    ddLdk1 := 0;
    dLdk2  := 0;
    ddLdk2 := 0;
    for i := 1 to n-1 do
      for j := 0 to i-1 do
      begin
        dP1  := 0;
        ddP1 := 0;
        dP2  := 0;
        ddP2 := 0;
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          dP1  := dP1dk1(i,j);
          ddP1 := ddP1dk1(i,j);
          if p1(i,j) > 0 then
          begin
            dLdk1  := dLdk1  +p1(i,j)*dP1/s;
            ddLdk1 := ddLdk1 +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
          end;
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          dP2  := dP2dk2(i,j);
          ddP2 := ddP2dk2(i,j);
          if p2(i,j) > 0 then
          begin
            dLdk2  := dLdk2  +p2(i,j)*dP2/s;
            ddLdk2 := ddLdk2 +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
          end;
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
          ER := ER -s;
        if ER > 0 then
        begin
          dLdk1  := dLdk1  -r(i,j)*dP1/ER;
          ddLdk1 := ddLdk1 -r(i,j)*(ddP1 -dP1*dP1/ER)/ER;
          dLdk2  := dLdk2  -r(i,j)*dP2/ER;
          ddLdk2 := ddLdk2 -r(i,j)*(ddP2 -dP2*dP2/ER)/ER;
        end;
      end;
      if ddLdk1 <> 0 then
        if abs(dLdk1/ddLdk1) < k1 then
          k1 := k1 -dLdk1/ddLdk1;
      if ddLdk2 <> 0 then
        if abs(dLdk2/ddLdk2) < k2 then
          k2 := k2 -dLdk2/ddLdk2;
      if k1 > 100 then
        k1 := 100;
      if k2 > 100 then
        k2 := 100;

  end;

  function IterateDistance(i,j: integer):double;
  var
    dP1,dP2,dQ,dR,ddP1,ddP2,ddQ,ddR,ER,s,dL,ddL: double;
  begin
    dL   := 0;
    ddL  := 0;
    dP1  := 0;
    ddP1 := 0;
    dP2  := 0;
    ddP2 := 0;
    dQ   := 0;
    ddQ  := 0;
    ER   := 1;

    s := EP1(i,j);
    if s > 0 then
    begin
      dP1  := dP1dx(i,j);
      ddP1 := ddP1dx(i,j);
      if p1(i,j) > 0 then
      begin
        dL  := dL  +p1(i,j)*dP1/s;
        ddL := ddL +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
      end;
      ER := ER -s;
    end;
    s := EP2(i,j);
    if s > 0 then
    begin
      dP2  := dP2dx(i,j);
      ddP2 := ddP2dx(i,j);
      if p2(i,j) > 0 then
      begin
        dL  := dL  +p2(i,j)*dP2/s;
        ddL := ddL +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
      end;
      ER := ER -s;
    end;
    s := EQ(i,j);
    if s > 0 then
    begin
      dQ   := dQdx(i,j);
      ddQ  := ddQdx(i,j);
      if Q[i,j] > 0 then
      begin
        dL  := dL  +Q[i,j]*dQ/s;
        ddL := ddL +Q[i,j]*(ddQ -dQ*dQ/s)/s;
      end;
      ER := ER -s;
    end;
    if ER > 0 then
    begin
      dR   := -dP1 -dP2 -dQ;
      ddR  := -ddP1 -ddP2 -ddQ;
      dL  := dL  +r(i,j)*dR/ER;
      ddL := ddL +r(i,j)*(ddR -dR*dR/ER)/ER;
    end;
    if abs(ddL) > 0.000000000001 then
      if (D[i,j] > dL/ddL) then
        result := D[i,j] -dL/ddL
      else
        result := 0
    else
      result := D[i,j];
  end;

  function CL:double;
  var
    i,j: integer;
    s,ER: double;
  begin
    result := 0;
    for i := 1 to n-1 do
      for j := 0 to i-1 do
      begin
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          if p1(i,j) > 0 then
            result := result +p1(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          if p2(i,j) > 0 then
            result := result +p2(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
        begin
          if Q[i,j] > 0 then
            result := result +Q[i,j]*ln(s);
          ER := ER -s;
        end;
        if ER > 0 then
          result := result +r(i,j)*ln(ER)
        else
          result := result +r(i,j);
      end;
  end;

var
  Sa1,Sa2,Sb,s,dL,ddL,mind: double;
  i,j: integer;
begin
  pR := pA+pG;
  pY := pT+pC;
  Sa1 := 0;
  Sa2 := 0;
  Sb := 0;
  for i := 1 to n-1 do
    for j := 0 to i-1 do
    begin
      s := P[i,j]+P[j,i]+Q[i,j];
      if s > 0 then
      begin
        Sa1 := Sa1 +P[i,j]/s/s;
        Sa2 := Sa2 +P[j,i]/s/s;
        Sb  := Sb  +Q[i,j]/s/s;
        D[i,j] := s;
      end
      else
        D[i,j] := 0;
    end;
  if computeparams then
    if (Sb > 0) and (pR > 0) and (pY > 0) then
    begin
      if (pA > 0) and (pG > 0) then
        k1 := (Sa1/pA/pG)/(Sb/pR/pY)
      else
        k1 := 1;
      if (pT > 0) and (pC > 0) then
        k2 := (Sa2/pT/pC)/(Sb/pR/pY)
      else
        k2 := 1;
    end
    else
    begin
      k1 := 1;
      k2 := 1;
    end;

  if (pA*pG*k1 +pT*pC*k2 +pR*pY) > 0 then
    for i := 1 to n-1 do
      for j := 0 to i-1 do
        D[i,j] := D[i,j]/4/(pA*pG*k1 +pT*pC*k2 +pR*pY);

  dL := CL;
  if dL < 0 then
  begin
    mind := power(10, floor(log10(-dL))-8);
    repeat
      for i := 1 to n-1 do
        for j := 0 to i-1 do
          if D[i,j] > 0.000000000001 then
            D[i,j] := IterateDistance(i,j);

      if computeparams then
        IterateKappa;

      ddL := CL -dL;
      dL := dL +ddL;
    until ddL < mind;
  end;

  for i := 1 to n-1 do
    for j := 0 to i-1 do
    begin
      D[i,j] := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)*D[i,j];
      D[j,i] := D[i,j];
    end;

    result := dL;
end;

function TN93GammaMatrix(P,Q,D: PDistanceMatrix; n: integer; pA,pT,pC,pG,k1,k2,gamma: double): double;
var
  pR,pY: double;

  function P1(i,j: integer): double;
  begin
    result := P[i,j];
  end;

  function P2(i,j: integer): double;
  begin
    result := P[j,i];
  end;

  function R(i,j: integer): double;
  begin
    result := 1-P[i,j]-P[j,i]-Q[i,j];
  end;

  function c1(i,j: integer):double;
  begin
    result := 2*(pR*k1 +pY);
  end;

  function f1(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma+c1(i,j)*D[i,j]));
  end;

  function df1dx(i,j: integer):double;
  begin
    result := -gamma*c1(i,j)/(gamma +c1(i,j)*D[i,j]);
  end;

  function ddf1dx(i,j: integer):double;
  begin
    result := gamma*c1(i,j)*c1(i,j)/(gamma +c1(i,j)*D[i,j])/(gamma +c1(i,j)*D[i,j]);
  end;

  function df1dk1(i,j: integer):double;
  begin
    result := -2*gamma*pR*D[i,j]/(gamma +c1(i,j)*D[i,j]);
  end;

  function ddf1dk1(i,j: integer):double;
  begin
    result := 4*gamma*pR*pR*D[i,j]*D[i,j]/(gamma +c1(i,j)*D[i,j])/(gamma +c1(i,j)*D[i,j]);
  end;

  function c2(i,j: integer):double;
  begin
    result := 2*(pY*k2 +pR);
  end;

  function f2(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma +c2(i,j)*D[i,j]));
  end;

  function df2dx(i,j: integer):double;
  begin
    result := -gamma*c2(i,j)/(gamma +c2(i,j)*D[i,j]);
  end;

  function ddf2dx(i,j: integer):double;
  begin
    result := gamma*c2(i,j)*c2(i,j)/(gamma +c2(i,j)*D[i,j])/(gamma +c2(i,j)*D[i,j]);
  end;

  function df2dk2(i,j: integer):double;
  begin
    result := -2*gamma*pY*D[i,j]/(gamma +c2(i,j)*D[i,j]);
  end;

  function ddf2dk2(i,j: integer):double;
  begin
    result := 4*gamma*pY*pY*D[i,j]*D[i,j]/(gamma +c2(i,j)*D[i,j])/(gamma +c2(i,j)*D[i,j]);
  end;

  function f3(i,j: integer):double;
  begin
    result := gamma*ln(gamma/(gamma +2*D[i,j]));
  end;

  function df3dx(i,j: integer):double;
  begin
    result := -2*gamma/(gamma +2*D[i,j]);
  end;

  function ddf3dx(i,j: integer):double;
  begin
    result := 4*gamma/(gamma +2*D[i,j])/(gamma +2*D[i,j]);
  end;

  function EP1(i,j: integer):double;
  begin
    if pR > 0 then
      result := 2*pA*pG/pR*(pR -exp(f1(i,j)) +pY*exp(f3(i,j)))
    else
      result := 0;
  end;

  function EP2(i,j: integer):double;
  begin
    if pY > 0 then
      result := 2*pT*pC/pY*(pY -exp(f2(i,j)) +pR*exp(f3(i,j)))
    else
      result := 0;
  end;

  function EQ(i,j: integer):double;
  begin
    result := 2*pR*pY*(1 -exp(f3(i,j)));
  end;

  function dP1dx(i,j: integer):double;
  begin
    result := 2*pA*pG/pR*(-exp(f1(i,j))*df1dx(i,j) +pY*exp(f3(i,j))*df3dx(i,j));
  end;

  function dP1dk1(i,j: integer):double;
  begin
    result := -2*pA*pG/pR*exp(f1(i,j))*df1dk1(i,j);
  end;

  function dP2dx(i,j: integer):double;
  begin
    result := 2*pT*pC/pY*(-exp(f2(i,j))*df2dx(i,j) +pR*exp(f3(i,j))*df3dx(i,j));
  end;

  function dP2dk2(i,j: integer):double;
  begin
    result := -2*pT*pC/pY*exp(f2(i,j))*df2dk2(i,j);
  end;

  function dQdx(i,j: integer):double;
  begin
    result := -2*pR*pY*exp(f3(i,j))*df3dx(i,j);
  end;

  function ddP1dx(i,j: integer):double;
  begin
    result := 2*pA*pG/pR*(-exp(f1(i,j))*(df1dx(i,j)*df1dx(i,j) +ddf1dx(i,j))
                          +pY*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j)));
  end;

  function ddP1dk1(i,j: integer):double;
  begin
    result := -2*pA*pG/pR*exp(f1(i,j))*(df1dk1(i,j)*df1dk1(i,j) +ddf1dk1(i,j));
  end;

  function ddP2dx(i,j: integer):double;
  begin
    result := 2*pT*pC/pY*(-exp(f2(i,j))*(df2dx(i,j)*df2dx(i,j) +ddf2dx(i,j))
                          +pR*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j)));
  end;

  function ddP2dk2(i,j: integer):double;
  begin
    result := -2*pT*pC/pY*exp(f2(i,j))*(df2dk2(i,j)*df2dk2(i,j) +ddf2dk2(i,j));
  end;

  function ddQdx(i,j: integer):double;
  begin
    result := -2*pR*pY*exp(f3(i,j))*(df3dx(i,j)*df3dx(i,j) +ddf3dx(i,j));
  end;

  procedure IterateKappa;
  var
    i,j: integer;
    dP1,ddP1,dP2,ddP2,ER,s,dLdk1,ddLdk1,dLdk2,ddLdk2: double;
  begin
    dLdk1  := 0;
    ddLdk1 := 0;
    dLdk2  := 0;
    ddLdk2 := 0;
    for i := 1 to n-1 do
      for j := 0 to i-1 do
      begin
        dP1  := 0;
        ddP1 := 0;
        dP2  := 0;
        ddP2 := 0;
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          dP1  := dP1dk1(i,j);
          ddP1 := ddP1dk1(i,j);
          if p1(i,j) > 0 then
          begin
            dLdk1  := dLdk1  +p1(i,j)*dP1/s;
            ddLdk1 := ddLdk1 +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
          end;
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          dP2  := dP2dk2(i,j);
          ddP2 := ddP2dk2(i,j);
          if p2(i,j) > 0 then
          begin
            dLdk2  := dLdk2  +p2(i,j)*dP2/s;
            ddLdk2 := ddLdk2 +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
          end;
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
          ER := ER -s;
        if ER > 0 then
        begin
          dLdk1  := dLdk1  -r(i,j)*dP1/ER;
          ddLdk1 := ddLdk1 -r(i,j)*(ddP1 -dP1*dP1/ER)/ER;
          dLdk2  := dLdk2  -r(i,j)*dP2/ER;
          ddLdk2 := ddLdk2 -r(i,j)*(ddP2 -dP2*dP2/ER)/ER;
        end;
      end;
      if ddLdk1 <> 0 then
        if abs(dLdk1/ddLdk1) < k1 then
          k1 := k1 -dLdk1/ddLdk1;
      if ddLdk2 <> 0 then
        if abs(dLdk2/ddLdk2) < k2 then
          k2 := k2 -dLdk2/ddLdk2;

      if k1 > 100 then
        k1 := 100;
      if k2 > 100 then
        k2 := 100;
  end;

  function IterateDistance(i,j: integer):double;
  var
    dP1,dP2,dQ,dR,ddP1,ddP2,ddQ,ddR,ER,s,dL,ddL: double;
  begin
    ER   := 1;
    dL   := 0;
    ddL  := 0;
    dP1  := 0;
    ddP1 := 0;
    dP2  := 0;
    ddP2 := 0;
    dQ   := 0;
    ddQ  := 0;

    s := EP1(i,j);
    if s > 0 then
    begin
      dP1  := dP1dx(i,j);
      ddP1 := ddP1dx(i,j);
      if p1(i,j) > 0 then
      begin
        dL  := dL  +p1(i,j)*dP1/s;
        ddL := ddL +p1(i,j)*(ddP1 -dP1*dP1/s)/s;
      end;
      ER := ER -s;
    end;
    s := EP2(i,j);
    if s > 0 then
    begin
      dP2  := dP2dx(i,j);
      ddP2 := ddP2dx(i,j);
      if p2(i,j) > 0 then
      begin
        dL  := dL  +p2(i,j)*dP2/s;
        ddL := ddL +p2(i,j)*(ddP2 -dP2*dP2/s)/s;
      end;
      ER := ER -s;
    end;
    s := EQ(i,j);
    if s > 0 then
    begin
      dQ   := dQdx(i,j);
      ddQ  := ddQdx(i,j);
      if Q[i,j] > 0 then
      begin
        dL  := dL  +Q[i,j]*dQ/s;
        ddL := ddL +Q[i,j]*(ddQ -dQ*dQ/s)/s;
      end;
      ER := ER -s;
    end;
    if ER > 0 then
    begin
      dR   := -dP1 -dP2 -dQ;
      ddR  := -ddP1 -ddP2 -ddQ;
      dL  := dL  +r(i,j)*dR/ER;
      ddL := ddL +r(i,j)*(ddR -dR*dR/ER)/ER;
    end;
    if abs(ddL) > 0.000000000001 then
      if D[i,j] > dL/ddL then
        result := D[i,j] -dL/ddL
      else
        result := 0
    else
      result := D[i,j];
  end;

  function CL:double;
  var
    i,j: integer;
    s,ER: double;
  begin
    result := 0;
    for i := 1 to n-1 do
      for j := 0 to i-1 do
      begin
        ER := 1;
        s := EP1(i,j);
        if s > 0 then
        begin
          if p1(i,j) > 0 then
            result := result +p1(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EP2(i,j);
        if s > 0 then
        begin
          if p2(i,j) > 0 then
            result := result +p2(i,j)*ln(s);
          ER := ER -s;
        end;
        s := EQ(i,j);
        if s > 0 then
        begin
          if Q[i,j] > 0 then
            result := result +Q[i,j]*ln(s);
          ER := ER -s;
        end;
        if ER > 0 then
          result := result +r(i,j)*ln(ER)
        else
          result := result +r(i,j);
      end;
  end;

var
  Sa1,Sa2,Sb,s,dL,ddL,mind: double;
  i,j: integer;
begin
  pR := pA+pG;
  pY := pT+pC;

  Sa1 := 0;
  Sa2 := 0;
  Sb := 0;
  for i := 1 to n-1 do
    for j := 0 to i-1 do
    begin
      s := P[i,j]+P[j,i]+Q[i,j];
      if s > 0 then
      begin
        Sa1 := Sa1 +P[i,j]/s/s;
        Sa2 := Sa2 +P[j,i]/s/s;
        Sb  := Sb  +Q[i,j]/s/s;
        D[i,j] := s;
      end;
    end;
{
  if (Sb > 0) and (pR > 0) and (pY > 0) then
  begin
    if (pA > 0) and (pG > 0) then
      k1 := (Sa1/pA/pG)/(Sb/pR/pY)
    else
      k1 := 1;
    if (pT > 0) and (pC > 0) then
      k2 := (Sa2/pT/pC)/(Sb/pR/pY)
    else
      k2 := 1;
  end
  else
  begin
    k1 := 1;
    k2 := 1;
  end;
}
  for i := 1 to n-1 do
    for j := 0 to i-1 do
      D[i,j] := D[i,j]/4/(pA*pG*k1 +pT*pC*k2 +pR*pY);

  dL := CL;
  mind := power(10, floor(log10(-dL))-8);
  repeat
    for i := 1 to n-1 do
      for j := 0 to i-1 do
        if D[i,j] > 0.000000000001 then
          D[i,j] := IterateDistance(i,j);

//    IterateKappa;

    ddL := CL -dL;
    dL := dL +ddL;
  until ddL < mind;

  for i := 1 to n-1 do
    for j := 0 to i-1 do
    begin
      D[i,j] := 4*(pA*pG*k1 +pT*pC*k2 +pR*pY)*D[i,j];
      if D[i,j] > 2.0 then
        D[i,j] := 2.0;
      D[j,i] := D[i,j];
    end;

  result := dL;
end;

function MakePQMatrix(Seqs: TStringList; P,Q: PDistanceMatrix; var pA,pT,pC,pG: double; f: PArrayOfInt): boolean;

var
  m: array[0..3,0..3] of integer;
  n,nseqs, nsites,i,j,k,ii,jj: integer;
  s: double;
begin
  result := false;
  if Seqs.Count <= 1 then exit;

  nseqs  := Seqs.Count;
  nsites := length(Seqs[0]);

  result := true;
  pA := 0;
  pT := 0;
  pC := 0;
  pG := 0;
  for i := 1 to nseqs-1 do
    for j := 0 to i-1 do
    begin
      for ii := 0 to 3 do
        for jj := 0 to 3 do
          m[ii,jj] := 0;

      for k := 1 to nsites do
      begin
        if f = nil then
          n := 1
        else if f[k] = 0 then
          continue
        else
          n := f[k];
        case upcase(Seqs[i][k]) of
          'A'     : case upcase(Seqs[j][k]) of
                      'A'     : inc(m[0,0], n);
                      'T', 
					  'U'     : inc(m[0,1], n);
                      'C'     : inc(m[0,2], n);
                      'G'     : inc(m[0,3], n);
                    end;
          'T', 
		  'U'     : case upcase(Seqs[j][k]) of
                      'A'     : inc(m[1,0], n);
                      'T', 
					  'U'     : inc(m[1,1], n);
                      'C'     : inc(m[1,2], n);
                      'G'     : inc(m[1,3], n);
                    end;
          'C'     : case upcase(Seqs[j][k]) of
                      'A'     : inc(m[2,0], n);
                      'T', 
					  'U'     : inc(m[2,1], n);
                      'C'     : inc(m[2,2], n);
                      'G'     : inc(m[2,3], n);
                    end;
          'G'     : case upcase(Seqs[j][k]) of
                      'A'     : inc(m[3,0], n);
                      'T', 
	                  'U'     : inc(m[3,1], n);
                      'C'     : inc(m[3,2], n);
                      'G'     : inc(m[3,3], n);
                    end;
        end;
      end;

      pA := pA +(2*m[0,0]+m[0,1]+m[1,0]+m[0,2]+m[2,0]+m[0,3]+m[3,0]);
      pT := pT +(2*m[1,1]+m[1,0]+m[0,1]+m[1,2]+m[2,1]+m[1,3]+m[3,1]);
      pC := pC +(2*m[2,2]+m[2,0]+m[0,2]+m[2,1]+m[1,2]+m[2,3]+m[3,2]);
      pG := pG +(2*m[3,3]+m[3,0]+m[0,3]+m[3,1]+m[1,3]+m[3,2]+m[2,3]);

      n := 0;
      for ii := 0 to 3 do
        for jj := 0 to 3 do
          n := n +M[ii,jj];

      if n > 0 then
      begin
        P[i,j] := (M[0,3]+M[3,0])/n;
        P[j,i] := (M[1,2]+M[2,1])/n;
        Q[i,j] := (M[0,1]+M[1,0]+M[0,2]+M[2,0]+M[1,3]+M[3,1]+M[2,3]+M[3,2])/n;
      end
      else
      begin
        P[i,j] := 0;
        P[j,i] := 0;
        Q[i,j] := 0;
        result := false;
      end;
    end;
  s := pA +pT +pC +pG;
  if s > 0 then
  begin
    pA := pA/s;
    pT := pT/s;
    pC := pC/s;
    pG := pG/s;
  end
  else
  begin
    pA := 0.25;
    pT := 0.25;
    pC := 0.25;
    pG := 0.25;
  end;
end;

function TN93MCLDistanceParams(Seqs: TStringList; D: PDistanceMatrix; var k1,k2: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93Matrix(P,Q,D, nseqs, pA,pT,pC,pG, k1, k2, true);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function TN93MCLDistance(Seqs: TStringList; D: PDistanceMatrix; k1,k2: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93Matrix(P,Q,D, nseqs, pA,pT,pC,pG, k1, k2, false);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function TN93GammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k1,k2,gamma: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93GammaMatrix(P,Q,D, nseqs, pA,pT,pC,pG, k1, k2,gamma);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function HKYMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93Matrix(P,Q,D, nseqs, pA,pT,pC,pG, k, k, false);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function HKYGammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k, gamma: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93GammaMatrix(P,Q,D, nseqs, pA,pT,pC,pG, k,k, gamma);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function T3MCLDistance(Seqs: TStringList; D: PDistanceMatrix; k: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93Matrix(P,Q,D, nseqs, (pA+pT)/2,(pA+pT)/2,(pC+pG)/2,(pC+pG)/2, k, k, false);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function T3GammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k, gamma: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93GammaMatrix(P,Q,D, nseqs, (pA+pT)/2,(pA+pT)/2,(pC+pG)/2,(pC+pG)/2, k, k, gamma);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function K2MCLDistance(Seqs: TStringList; D: PDistanceMatrix; k: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93Matrix(P,Q,D, nseqs, 0.25,0.25,0.25,0.25, k, k, false);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

function K2GammaMCLDistance(Seqs: TStringList; D: PDistanceMatrix; k, gamma: extended; f: PArrayOfInt): boolean;
var
  P,Q: PDistanceMatrix;
  pA: double = 0;
  pT: double = 0;
  pC: double = 0;
  pG: double = 0;
  nseqs: integer;
begin
  nseqs  := Seqs.Count;
  P := CreateDistanceMatrix(nseqs);
  Q := CreateDistanceMatrix(nseqs);

  result := MakePQMatrix(Seqs, P,Q, pA,pT,pC,pG, f);
  TN93GammaMatrix(P,Q,D, nseqs, 0.25,0.25,0.25,0.25, k, k, gamma);

  DestroyDistanceMatrix(P, nseqs);
  DestroyDistanceMatrix(Q, nseqs);

end;

end.
