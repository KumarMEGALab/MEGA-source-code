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

unit MatrixFuncs;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Math, MegaConsts;

type
  TComplex = record
    X, Y: Extended;
  end;

const
  MaxComplexElts   = High(Longint) div sizeOf(TComplex);

type
  TArrayOfComplex = array [0..MaxComplexElts-1] of TComplex;
  PArrayOfComplex = ^TArrayOfComplex;

procedure CreateMatrix(var M: PMatrixOfExtended; n: integer);
procedure DestroyMatrix(var M: PMatrixOfExtended; n: integer);
function MatrixToDebugFile(M: PMatrixOfExtended; n: Integer; filename: String): Boolean;
procedure CopyMatrix(Dest, Source : PMatrixOfExtended; Lbound, Ubound : Integer);

procedure MultiplyMatrix(Left, Right, Result : PMatrixOfExtended; Lbound, Ubound : Integer);
procedure MultiplyDiagMatrix(D: PArrayOfExtended; M, Result: PMatrixOfExtended; Lbound, Ubound : Integer);
procedure MultiplyMatrixDiag(M: PMatrixOfExtended; D: PArrayOfExtended; Result: PMatrixOfExtended; Lbound, Ubound : Integer);


function InvMat(A              : PMatrixOfExtended;
                Lbound, Ubound : Integer;
                A_inv          : PMatrixOfExtended) : Integer;

function Jacobi(A: PMatrixOfExtended; Lbound, Ubound, MaxIter: Integer;
                Tol: extended; V: PMatrixOfExtended; Lambda: PArrayOfExtended) : Integer;

procedure EigenVect(A: PMatrixOfExtended; Lb, Ub : Integer;
                    Lambda: PArrayOfComplex; V: PMatrixOfExtended);



implementation

uses
  MGlobalSettings;

const
  MACHEP = 1.42108E-14;  { double point precision: 2^(-463) }
  SQRT2DIV2  = 0.70710678118654752440;
  MAT_OK       =   0;  { No error }
  MAT_SINGUL   = - 1;  { Singular matrix }
  MAT_NON_CONV = - 2;  { Non convergence of iterative procedure }
  //MAT_NOT_PD   = - 3;  { Matrix not positive definite }

procedure CreateMatrix(var M: PMatrixOfExtended; n: integer);
var
  i,j: integer;
begin
  GetMem(M, sizeof(pointer)*n);
  for i := 0 to n-1 do
    GetMem(M[i], sizeof(extended)*n);
  for i := 0 to n-1 do
    for j := 0 to n-1 do
      M[i,j] := 0;
end;

procedure DestroyMatrix(var M: PMatrixOfExtended; n: integer);
var
  i: integer;
begin
  if (not assigned(M)) or (M = nil) then exit;

  for i := 0 to n-1 do
    FreeMemAndNil(M[i], sizeof(extended)*n);
  FreeMemAndNil(M, sizeof(pointer)*n);
end;

function MatrixToDebugFile(M: PMatrixOfExtended; n: Integer; filename: String): Boolean;
var
  i, j: Integer;
  aFile: TextFile;
begin
  {$IFDEF DEBUG}
  Result := False;
  if n > 0 then
  begin
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      for i := 0 to n - 1 do
      begin
        for j := 0 to n - 1 do
          Write(aFile, M[i,j]);
        WriteLn(aFile);
      end;
    finally
      CloseFile(aFile);
    end;
  end;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

procedure CopyMatrix(Dest, Source : PMatrixOfExtended;
                     Lbound, Ubound : Integer);
var
  I, J : Integer;
begin
  for I := Lbound to Ubound do
    for J := Lbound to Ubound do
      Dest[I,J] := Source[I,J];
end;

procedure MultiplyMatrix(Left, Right, Result : PMatrixOfExtended;
                         Lbound, Ubound : Integer);
var
  i,j,k : Integer;
begin
  for i := Lbound to Ubound do
    for j := Lbound to Ubound do
    begin
      Result[i,j] := 0;
      for k := LBound to Ubound do
        Result[i,j] := Result[i,j] +Left[i,k]*Right[k,j];
    end;
end;

procedure MultiplyDiagMatrix(D: PArrayOfExtended; M, Result: PMatrixOfExtended;
                             Lbound, Ubound : Integer);
var
  i,j: integer;
begin
  for i := Lbound to Ubound do
    for j := Lbound to Ubound do
      Result[i,j] := D[i]*M[i,j];
end;

procedure MultiplyMatrixDiag(M: PMatrixOfExtended; D: PArrayOfExtended; Result: PMatrixOfExtended;
                             Lbound, Ubound : Integer);

var
  i,j: integer;
begin
  for i := Lbound to Ubound do
    for j := Lbound to Ubound do
      Result[i,j] := M[i,j]*D[j];
end;

procedure Swap(var X, Y : Extended);
var
  Temp : Extended;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

procedure SwapCols(J, K : Integer; A : PMatrixOfExtended; Lbound, Ubound : Integer);
var
  I : Integer;
begin
  for I := Lbound to Ubound do
    Swap(A[I,J], A[I,K]);
end;

procedure SwapRows(I, K : Integer; A : PMatrixOfExtended; Lbound, Ubound : Integer);
var
  J : Integer;
begin
  for J := Lbound to Ubound do
    Swap(A[I,J], A[K,J]);
end;

function InvMat(A              : PMatrixOfExtended;
                Lbound, Ubound : Integer;
                A_inv          : PMatrixOfExtended) : Integer;

var
  I, J, K    : Integer;         
  Ik, Jk     : Integer;         
  Pvt        : Extended;
  T          : extended;          
  PRow, PCol : PArrayOfInt;     
  MCol       : PArrayOfExtended;
begin
  GetMem(PRow, sizeof(integer)*(Ubound+1));
  GetMem(PCol, sizeof(integer)*(Ubound+1));
  GetMem(MCol, sizeof(extended)*(Ubound+1));

  CopyMatrix(A_inv, A, Lbound, Ubound);

  K := Lbound;
  while K <= Ubound do
    begin
      { Search for largest pivot in submatrix A_inv[K..Ubound, K..Ubound] }
      Pvt := A_inv[K,K];
      Ik := K;
      Jk := K;
      for I := K to Ubound do
        for J := K to Ubound do
          if Abs(A_inv[I,J]) > Abs(Pvt) then
            begin
              Pvt := A_inv[I,J];
              Ik := I;
              Jk := J;
            end;

      { Pivot too small ==> quasi-singular matrix }
      if Abs(Pvt) < MACHEP then
        begin
          result := MAT_SINGUL;
          break;
        end;

      { Save pivot position }
      PRow[K] := Ik;
      PCol[K] := Jk;

      { Exchange current row (K) with pivot row (Ik) }
      if Ik <> K then
        SwapRows(Ik, K, A_inv, Lbound, Ubound);

      { Exchange current column (K) with pivot column (Jk) }
      if Jk <> K then
        SwapCols(Jk, K, A_inv, Lbound, Ubound);

      { Store col. K of A_inv into MCol and set this col. to 0 }
      for I := Lbound to Ubound do
        if I <> K then
          begin
            MCol[I] := A_inv[I, K];
            A_inv[I, K] := 0.0;
          end
        else
          begin
            MCol[I] := 0.0;
            A_inv[I, K] := 1.0;
          end;

      { Transform pivot row }
      for J := Lbound to Ubound do
        A_inv[K,J] := A_inv[K,J] / Pvt;

      { Transform other rows }
      for I := Lbound to Ubound do
        if I <> K then
          begin
            T := MCol[I];
            for J := Lbound to Ubound do
              A_inv[I,J] := A_inv[I,J] - T * A_inv[K,J];
          end;
      Inc(K);
    end;

  { Exchange lines of inverse matrix }
  for I := Ubound downto Lbound do
    begin
      Ik := PCol[I];
      if Ik <> I then
        SwapRows(Ik, I, A_inv, Lbound, Ubound);
    end;

  { Exchange columns of inverse matrix }
  for J := Ubound downto Lbound do
    begin
      Jk := PRow[J];
      if Jk <> J then
        SwapCols(Jk, J, A_inv, Lbound, Ubound);
    end;

  FreeMemAndNil(PRow);
  FreeMemAndNil(PCol);
  FreeMemAndNil(MCol);

  result := MAT_OK;
end;

function Jacobi(A : PMatrixOfExtended; Lbound, Ubound, MaxIter : Integer;
                Tol : extended; V : PMatrixOfExtended; Lambda : PArrayOfExtended) : Integer;

  function Sgn(X : extended) : Integer;
  begin
    if X < 0.0 then
      result := -1
    else
      result := 1;
  end;

  function Pythag(X, Y : extended) : extended;
  { Computes Sqrt(X^2 + Y^2) without destructive underflow or overflow }
  var
    AbsX, AbsY : extended;
  begin
//    MathError := FN_OK;
    AbsX := Abs(X);
    AbsY := Abs(Y);
    if AbsX > AbsY then
      result := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
    else if AbsY = 0.0 then
      result := 0.0
    else
      result := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
  end;

var
  I, J, K, Im, Jm, Iter : Integer;
  B, C, C2, Na, Nd, P, Q, S, S2, R, T : extended;
begin
  Iter := 0;
  Na := 0.0;
  Nd := 0.0;
  R := 0.0;

  for I := Lbound to Ubound do
    begin
      V[I,I] := 1.0;
      Nd := Nd + Sqr(A[I,I]);
      if I <> Ubound then
        for J := Succ(I) to Ubound do
          begin
            R := R + Sqr(A[I,J]);
            V[I,J] := 0.0;
            V[J,I] := 0.0;
          end;
    end;

  Na := Nd + 2.0 * R;

  repeat
    R := 0.0;
    for I := Lbound to Pred(Ubound) do
      for J := Succ(I) to Ubound do
        begin
          T := Abs(A[I,J]);
          if T > R then
            begin
              R := T;
              Im := I;
              Jm := J;
            end;
        end;

    B := A[Im,Im] - A[Jm,Jm];

    if B = 0 then
      begin
        C := SQRT2DIV2;
        S := C * Sgn(A[Im,Jm]);
      end
    else
      begin
        P := 2.0 * A[Im,Jm] * Sgn(B);
        Q := Abs(B);
        R := Pythag(P, Q);
        C := Sqrt(0.5 * (1.0 + Q / R));
        S := 0.5 * P / (R * C);
      end;

    for K := Lbound to Ubound do
      begin
        R := V[K,Im];
        V[K,Im] := C * R + S * V[K,Jm];
        V[K,Jm] := C * V[K,Jm] - S * R;
      end;

    if Im <> Lbound then
      for K := Lbound to Pred(Im) do
        begin
          R := A[K,Im];
          A[K,Im] := C * R + S * A[K,Jm];
          A[K,Jm] := C * A[K,Jm] - S * R;
        end;

    if Jm <> Succ(Im) then
      for K := Succ(Im) to Pred(Jm) do
        begin
          R := A[Im,K];
          A[Im,K] := C * R + S * A[K,Jm];
          A[K,Jm] := C * A[K,Jm] - S * R;
        end;

    if Jm <> Ubound then
      for K := Succ(Jm) to Ubound do
        begin
          R := A[Im,K];
          A[Im,K] := C * R + S * A[Jm,K];
          A[Jm,K] := C * A[Jm,K] - S * R;
        end;

    Nd := Nd + 2.0 * Sqr(A[Im,Jm]);

    C2 := Sqr(C);
    S2 := Sqr(S);
    P := 2.0 * S * C * A[Im,Jm];
    R := A[Im,Im];
    A[Im,Im] := C2 * R + S2 * A[Jm,Jm] + P;
    A[Jm,Jm] := S2 * R + C2 * A[Jm,Jm] - P;
    A[Im,Jm] := 0.0;

    Inc(Iter);
    if Iter > MaxIter then
      begin
        result := MAT_NON_CONV;
        break;
      end;
  until Abs(1.0 - Na / Nd) < Tol;

  { The diagonal terms of the transformed matrix are the eigenvalues }
  for I := Lbound to Ubound do
    Lambda[I] := A[I,I];

  { Sort eigenvalues and eigenvectors }
  for I := Lbound to Pred(Ubound) do
    begin
      K := I;
      R := Lambda[I];
      for J := Succ(I) to Ubound do
        if Lambda[J] > R then
          begin
            K := J;
            R := Lambda[J];
          end;
      Swap(Lambda[I], Lambda[K]);
      SwapCols(I, K, V, Lbound, Ubound);
    end;
    
  { Make sure that the first component of each eigenvector is > 0 }
  for J := Lbound to Ubound do
    if V[Lbound, J] < 0.0 then
      for I := Lbound to Ubound do
        V[I,J] := - V[I,J];

  result := MAT_OK;
end;


procedure Balance(A: PMatrixOfExtended; Lb, Ub: Integer;
                  var I_low, I_igh: Integer; Scale: PArrayOfExtended);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK procedure Balanc.

  This procedure balances a real matrix and isolates eigenvalues
  whenever possible.

  On input:

    A contains the input matrix to be balanced.

    Lb, Ub are the lowest and highest indices of the elements of A.

  On output:

    A contains the balanced matrix.

    I_low and I_igh are two integers such that A[i,j]
    is equal to zero if
      (1) i is greater than j and
      (2) j=Lb,...,I_low-1 or i=I_igh+1,...,Ub.

    Scale contains information determining the permutations
    and scaling factors used.

    Suppose that the principal submatrix in rows I_low through I_igh
    has been balanced, that P[j] denotes the index interchanged
    with j during the permutation step, and that the elements
    of the diagonal matrix used are denoted by D[i,j].  then
        Scale[j] = P[j],    for j = Lb,...,I_low-1
                 = D[j,j],      j = I_low,...,I_igh
                 = P[j]         j = I_igh+1,...,Ub.
    the order in which the interchanges are made is
    Ub to I_igh+1, then Lb to I_low-1.

    Note that Lb is returned for I_igh if I_igh is < Lb formally
  ------------------------------------------------------------------ }

const
  RADIX = 2;  { Base used in floating number representation }

var
  I, J, M           : Integer;
  C, F, G, R, S, B2 : Extended;
  Flag, Found, Conv : Boolean;

  procedure Exchange;
  { Row and column exchange }
  var
    I : Integer;
  begin
    Scale^[M] := J;
    if J = M then Exit;

    for I := Lb to I_igh do
      begin
        F := A^[I]^[J];
        A^[I]^[J] := A^[I]^[M];
        A^[I]^[M] := F;
      end;

    for I := I_low to Ub do
      begin
        F := A^[J]^[I];
        A^[J]^[I] := A^[M]^[I];
        A^[M]^[I] := F;
      end;
  end;

begin
  B2 := RADIX * RADIX;
  I_low := Lb;
  I_igh := Ub;

  { Search for rows isolating an eigenvalue and push them down }
  repeat
    J := I_igh;
    repeat
      I := Lb;
      repeat
        Flag := (I <> J) and (A^[J]^[I] <> 0.0);
        I := I + 1;
      until Flag or (I > I_igh);
      Found := not Flag;
      if Found then
        begin
          M := I_igh;
          Exchange;
          I_igh := I_igh - 1;
        end;
      J := J - 1;
    until Found or (J < Lb);
  until (not Found) or (I_igh < Lb);

  if I_igh < Lb then I_igh := Lb;
  if I_igh = Lb then Exit;

  { Search for columns isolating an eigenvalue and push them left }
  repeat
    J := I_low;
    repeat
      I := I_low;
      repeat
        Flag := (I <> J) and (A^[I]^[J] <> 0.0);
        I := I + 1;
      until Flag or (I > I_igh);
      Found := not Flag;
      if Found then
        begin
          M := I_low;
          Exchange;
          I_low := I_low + 1;
        end;
      J := J + 1;
    until Found or (J > I_igh);
  until (not Found);

  { Now balance the submatrix in rows I_low to I_igh }
  for I := I_low to I_igh do
    Scale^[I] := 1.0;

  { Iterative loop for norm reduction }
  repeat
    Conv := True;

    for I := I_low to I_igh do
      begin
        C := 0.0;
        R := 0.0;

        for J := I_low to I_igh do
          if J <> I then
            begin
              C := C + Abs(A^[J]^[I]);
              R := R + Abs(A^[I]^[J]);
            end;

        { Guard against zero C or R due to underflow }
        if (C <> 0.0) and (R <> 0.0) then
          begin
            G := R / RADIX;
            F := 1.0;
            S := C + R;

            while C < G do
              begin
                F := F * RADIX;
                C := C * B2;
              end;

            G := R * RADIX;

            while C >= G do
              begin
                F := F / RADIX;
                C := C / B2;
              end;

            { Now balance }
            if (C + R) / F < 0.95 * S then
              begin
                G := 1.0 / F;
                Scale^[I] := Scale^[I] * F;
                Conv := False;
                for J := I_low to Ub do A^[I]^[J] := A^[I]^[J] * G;
                for J := Lb to I_igh do A^[J]^[I] := A^[J]^[I] * F;
              end;
          end;
      end;
  until Conv;
end;

procedure ElmHes(A: PMatrixOfExtended; Lb, Ub, I_low, I_igh: Integer; I_int: PArrayOfInt);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Elmhes

  Given a real general matrix, this procedure reduces a submatrix
  situated in rows and columns I_low through I_igh to upper
  Hessenberg form by stabilized elementary similarity transformations.

  On input:

    A contains the input matrix.

    Lb, Ub are the lowest and highest indices
    of the elements of A.

    I_low and I_igh are integers determined by the balancing procedure
    Balance. If Balance has not been used, set I_low = Lb, I_igh = Ub.

  On output:

    A contains the Hessenberg matrix. The multipliers which were used
    in the reduction are stored in the remaining triangle under the
    Hessenberg matrix.

    I_int contains information on the rows and columns interchanged
    in the reduction. Only elements I_low through I_igh are used.
  ------------------------------------------------------------------ }

var
  I, J, M, La, Kp1, Mm1, Mp1 : Integer;
  X, Y                       : Extended;

begin
  La := I_igh - 1;
  Kp1 := I_low + 1;
  if La < Kp1 then Exit;

  for M := Kp1 to La do
    begin
      Mm1 := M - 1;
      X := 0.0;
      I := M;

      for J := M to I_igh do
        if Abs(A^[J]^[Mm1]) > Abs(X) then
          begin
            X := A^[J]^[Mm1];
            I := J;
          end;

      I_int^[M] := I;

      { Interchange rows and columns of A }
      if I <> M then
        begin
          for J := Mm1 to Ub do
            begin
              Y := A^[I]^[J];
              A^[I]^[J] := A^[M]^[J];
              A^[M]^[J] := Y;
            end;

          for J := Lb to I_igh do
            begin
              Y := A^[J]^[I];
              A^[J]^[I] := A^[J]^[M];
              A^[J]^[M] := Y;
            end;
        end;

      if X <> 0.0 then
        begin
          Mp1 := M + 1;
          for I := Mp1 to I_igh do
            begin
              Y := A^[I]^[Mm1];
              if Y <> 0.0 then
                begin
                  Y := Y / X;
                  A^[I]^[Mm1] := Y;
                  for J := M to Ub do
                    A^[I]^[J] := A^[I]^[J] - Y * A^[M]^[J];
                  for J := Lb to I_igh do
                    A^[J]^[M] := A^[J]^[M] + Y * A^[J]^[I];
                end;
            end;
        end;
    end;
end;

procedure Eltran(A: PMatrixOfExtended; Lb, Ub, I_low, I_igh : Integer;
                 I_int: PArrayOfInt; Z: PMatrixOfExtended);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Eltran.

  This procedure accumulates the stabilized elementary similarity
  transformations used in the reduction of a real general matrix
  to upper Hessenberg form by Elmhes.

  On input:

    A contains the multipliers which were used in the reduction
    by Elmhes in its lower triangle below the subdiagonal.

    Lb, Ub are the lowest and highest indices
    of the elements of A

    I_low and I_igh are integers determined by the balancing procedure
    Balance. If Balance has not been used, set I_low=Lb, I_igh=Ub.

    I_int contains information on the rows and columns interchanged in
    the reduction by Elmhes. Only elements I_low through I_igh are used.

  On output:

    Z contains the transformation matrix produced in the reduction by
    Elmhes.
  ------------------------------------------------------------------ }

var
  I, J, Mp, Mp1 : Integer;

begin
  { Initialize Z to identity matrix }
  for I := Lb to Ub do
    for J := Lb to Ub do
      if I = J then Z^[I]^[J] := 1.0 else Z^[I]^[J] := 0.0;

  if I_igh < I_low then Exit;

  for Mp := I_igh - 1 downto I_low + 1 do
    begin
      Mp1 := Mp + 1;
      for I := Mp1 to I_igh do
        Z^[I]^[Mp] := A^[I]^[Mp - 1];
      I := I_int^[Mp];
      if I <> Mp then
        begin
          for J := Mp to I_igh do
            begin
              Z^[Mp]^[J] := Z^[I]^[J];
              Z^[I]^[J] := 0.0;
            end;
          Z^[I]^[Mp] := 1.0;
        end;
    end;
end;

function DSgn(A, B: Extended): Extended;
begin
  if B < 0.0 then
    Result := -Abs(A)
  else
    Result := Abs(A);
end;

function Hqr2(H: PMatrixOfExtended; Lb, Ub, I_low, I_igh: Integer;
               Lambda: PArrayOfComplex; Z: PMatrixOfExtended): integer;
{ ------------------------------------------------------------------
  This function is a translation of the EISPACK subroutine hqr2

  This procedure finds the eigenvalues and eigenvectors of a real
  upper Hessenberg matrix by the QR method.

  On input:

    H contains the upper Hessenberg matrix.

    Lb, Ub are the lowest and highest indices
    of the elements of H

    I_low and I_igh are integers determined by the balancing subroutine
    Balance. If Balance has not been used, set I_low=Lb, I_igh=Ub

    Z contains the transformation matrix produced by Eltran after the
    reduction by Elmhes, or by Ortran after the reduction by Orthes, if
    performed. If the eigenvectors of the Hessenberg matrix are desired,
    Z must contain the identity matrix.

  On output:

    H has been destroyed.

    Wr and Wi contain the real and imaginary parts, respectively, of
    the eigenvalues. The eigenvalues are unordered except that complex
    conjugate pairs of values appear consecutively with the eigenvalue
    having the positive imaginary part first.

    Z contains the real and imaginary parts of the eigenvectors. If the
    i-th eigenvalue is real, the i-th column of Z contains its eigenvector.
    If the i-th eigenvalue is complex with positive imaginary part, the i-th
    and (i+1)-th columns of Z contain the real and imaginary parts of its
    eigenvector. The eigenvectors are unnormalized. If an error exit is made,
    none of the eigenvectors has been found.

    The function returns an error code:
       zero       for normal return,
       -j         if the limit of 30*N iterations is exhausted
                  while the j-th eigenvalue is being sought
                  (N being the size of the matrix). The eigenvalues
                  should be correct for indices j+1,...,Ub.
  ------------------------------------------------------------------
  Note: This is a crude translation. Many of the original goto's
  have been kept !
  ------------------------------------------------------------------ }

    procedure Cdiv(Ar, Ai, Br, Bi : Extended; var Cr, Ci : Extended);
    { Complex division, (Cr,Ci) = (Ar,Ai)/(Br,Bi) }
    var
      S, Ars, Ais, Brs, Bis : Extended;
    begin
      S := Abs(Br) + Abs(Bi);
      Ars := Ar / S;
      Ais := Ai / S;
      Brs := Br / S;
      Bis := Bi / S;
      S := Sqr(Brs) + Sqr(Bis);
      Cr := (Ars * Brs + Ais * Bis) / S;
      Ci := (Ais * Brs - Ars * Bis) / S;
    end;

  var
    I, J, K, L, M, N, En, Na, Itn, Its, Mp2, Enm2                : Integer;
    P, Q, R, S, T, W, X, Y, Ra, Sa, Vi, Vr, Zz, Norm, Tst1, Tst2 : Extended;
    NotLas                                                       : Boolean;

  label
    60, 70, 100, 130, 150, 170, 225, 260, 270, 280, 320, 330, 340,
    600, 630, 635, 640, 680, 700, 710, 770, 780, 790, 795, 800;

  begin
    { Store roots isolated by Balance and compute matrix norm }
    Result := 0;

    K := Lb;
    Norm := 0.0;
    for I := Lb to Ub do
      begin
        for J := K to Ub do
          Norm := Norm + Abs(H^[I]^[J]);
        K := I;
        if (I < I_low) or (I > I_igh) then
          begin
            Lambda^[I].X := H^[I]^[I];
            Lambda^[I].Y := 0.0;
          end;
      end;

    N := Ub - Lb + 1;
    Itn := 30 * N;
    En := I_igh;
    T := 0.0;

60: { Search for next eigenvalues }
    if En < I_low then goto 340;
    Its := 0;
    Na := En - 1;
    Enm2 := Na - 1;

70: { Look for single small sub-diagonal element }
    for L := En downto I_low do
      begin
        if L = I_low then goto 100;
        S := Abs(H^[L - 1]^[L - 1]) + Abs(H^[L]^[L]);
        if S = 0.0 then S := Norm;
        Tst1 := S;
        Tst2 := Tst1 + Abs(H^[L]^[L - 1]);
        if Tst2 = Tst1 then goto 100;
      end;

100: { Form shift }
    X := H^[En]^[En];
    if L = En then goto 270;
    Y := H^[Na]^[Na];
    W := H^[En]^[Na] * H^[Na]^[En];
    if L = Na then goto 280;

    if Itn = 0 then
      begin
        { Set error -- all eigenvalues have not
          converged after 30*N iterations }
//        SetErrCode(- En);
        Result := -En;
        Exit;
      end;

    if (Its <> 10) and (Its <> 20) then goto 130;

    { Form exceptional shift }
    T := T + X;

    for I := I_low to En do
      H^[I]^[I] := H^[I]^[I] - X;

    S := Abs(H^[En]^[Na]) + Abs(H^[Na]^[Enm2]);
    X := 0.75 * S;
    Y := X;
    W := - 0.4375 * S * S;

130:
    Its := Its + 1;
    Itn := Itn - 1;

    { Look for two consecutive small sub-diagonal elements }
    for M := Enm2 downto L do
      begin
        Zz := H^[M]^[M];
        R := X - Zz;
        S := Y - Zz;
        P := (R * S - W) / H^[M + 1]^[M] + H^[M]^[M + 1];
        Q := H^[M + 1]^[M + 1] - Zz - R - S;
        R := H^[M + 2]^[M + 1];
        S := Abs(P) + Abs(Q) + Abs(R);
        P := P / S;
        Q := Q / S;
        R := R / S;
        if M = L then goto 150;
        Tst1 := Abs(P) * (Abs(H^[M - 1]^[M - 1]) + Abs(Zz) + Abs(H^[M + 1]^[M + 1]));
        Tst2 := Tst1 + Abs(H^[M]^[M - 1]) * (Abs(Q) + Abs(R));
        if Tst2 = Tst1 then goto 150;
      end;

150:
    Mp2 := M + 2;

    for I := Mp2 to En do
      begin
        H^[I]^[I - 2] := 0.0;
        if I <> Mp2 then H^[I]^[I - 3] := 0.0;
      end;

    { Double QR step involving rows L to En and columns M to En }
    for K := M to Na do
      begin
        NotLas := (K <> Na);
        if (K = M) then goto 170;
        P := H^[K]^[K - 1];
        Q := H^[K + 1]^[K - 1];
        R := 0.0;
        if NotLas then R := H^[K + 2]^[K - 1];
        X := Abs(P) + Abs(Q) + Abs(R);
        if X = 0.0 then goto 260;
        P := P / X;
        Q := Q / X;
        R := R / X;
170:    S := DSgn(Sqrt(P * P + Q * Q + R * R), P);
        if K <> M then
          H^[K]^[K - 1] := - S * X
        else if L <> M then
          H^[K]^[K - 1] := - H^[K]^[K - 1];
        P := P + S;
        X := P / S;
        Y := Q / S;
        Zz := R / S;
        Q := Q / P;
        R := R / P;
        if NotLas then goto 225;

        { Row modification }
        for J := K to Ub do
          begin
            P := H^[K]^[J] + Q * H^[K + 1]^[J];
            H^[K]^[J] := H^[K]^[J] - P * X;
            H^[K + 1]^[J] := H^[K + 1]^[J] - P * Y;
          end;

        J := Min(En, K + 3);

        { Column modification }
        for I := Lb to J do
          begin
            P := X * H^[I]^[K] + Y * H^[I]^[K + 1];
            H^[I]^[K] := H^[I]^[K] - P;
            H^[I]^[K + 1] := H^[I]^[K + 1] - P * Q;
          end;

        { Accumulate transformations }
        for I := I_low to I_igh do
          begin
            P := X * Z^[I]^[K] + Y * Z^[I]^[K + 1];
            Z^[I]^[K] := Z^[I]^[K] - P;
            Z^[I]^[K + 1] := Z^[I]^[K + 1] - P * Q;
          end;
        goto 260;

225:
        { Row modification }
        for J := K to Ub do
          begin
            P := H^[K]^[J] + Q * H^[K + 1]^[J] + R * H^[K + 2]^[J];
            H^[K]^[J] := H^[K]^[J] - P * X;
            H^[K + 1]^[J] := H^[K + 1]^[J] - P * Y;
            H^[K + 2]^[J] := H^[K + 2]^[J] - P * Zz;
          end;

        J := Min(En, K + 3);

        { Column modification }
        for I := Lb to J do
          begin
            P := X * H^[I]^[K] + Y * H^[I]^[K + 1] + Zz * H^[I]^[K + 2];
            H^[I]^[K] := H^[I]^[K] - P;
            H^[I]^[K + 1] := H^[I]^[K + 1] - P * Q;
            H^[I]^[K + 2] := H^[I]^[K + 2] - P * R;
          end;

        { Accumulate transformations }
        for I := I_low to I_igh do
          begin
            P := X * Z^[I]^[K] + Y * Z^[I]^[K + 1] + Zz * Z^[I]^[K + 2];
            Z^[I]^[K] := Z^[I]^[K] - P;
            Z^[I]^[K + 1] := Z^[I]^[K + 1] - P * Q;
            Z^[I]^[K + 2] := Z^[I]^[K + 2] - P * R;
          end;

260:  end;

    goto 70;

270: { One root found }
    H^[En]^[En] := X + T;
    Lambda^[En].X := H^[En]^[En];
    Lambda^[En].Y := 0.0;
    En := Na;
    goto 60;

280: { Two roots found }
    P := 0.5 * (Y - X);
    Q := P * P + W;
    Zz := Sqrt(Abs(Q));
    H^[En]^[En] := X + T;
    X := H^[En]^[En];
    H^[Na]^[Na] := Y + T;
    if Q < 0.0 then goto 320;

    { Real pair }
    Zz := P + DSgn(Zz, P);
    Lambda^[Na].X := X + Zz;
    Lambda^[En].X := Lambda^[Na].X;
    if Zz <> 0.0 then Lambda^[En].X := X - W / Zz;
    Lambda^[Na].Y := 0.0;
    Lambda^[En].Y := 0.0;
    X := H^[En]^[Na];
    S := Abs(X) + Abs(Zz);
    P := X / S;
    Q := Zz / S;
    R := Sqrt(P * P + Q * Q);
    P := P / R;
    Q := Q / R;

    { Row modification }
    for J := Na to Ub do
      begin
        Zz := H^[Na]^[J];
        H^[Na]^[J] := Q * Zz + P * H^[En]^[J];
        H^[En]^[J] := Q * H^[En]^[J] - P * Zz;
      end;

    { Column modification }
    for I := Lb to En do
      begin
        Zz := H^[I]^[Na];
        H^[I]^[Na] := Q * Zz + P * H^[I]^[En];
        H^[I]^[En] := Q * H^[I]^[En] - P * Zz;
      end;

    { Accumulate transformations }
    for I := I_low to I_igh do
      begin
        Zz := Z^[I]^[Na];
        Z^[I]^[Na] := Q * Zz + P * Z^[I]^[En];
        Z^[I]^[En] := Q * Z^[I]^[En] - P * Zz;
      end;

    goto 330;

320: { Complex pair }
    Lambda^[Na].X := X + P;
    Lambda^[En].X := Lambda^[Na].X;
    Lambda^[Na].Y := Zz;
    Lambda^[En].Y := - Zz;

330:
    En := Enm2;
    goto 60;

340:
    if Norm = 0.0 then Exit;

    { All roots found. Backsubstitute to find
      vectors of upper triangular form }
    for En := Ub downto Lb do
      begin
        P := Lambda^[En].X;
        Q := Lambda^[En].Y;
        Na := En - 1;
        if Q < 0.0 then
          goto 710
        else if Q = 0.0 then
          goto 600
        else
          goto 800;

600:    { Real vector }
        M := En;
        H^[En]^[En] := 1.0;
        if Na < Lb then goto 800;

        for I := Na downto Lb do
          begin
            W := H^[I]^[I] - P;
            R := 0.0;

            for J := M to En do
              R := R + H^[I]^[J] * H^[J]^[En];

            if Lambda^[I].Y >= 0.0 then goto 630;
            Zz := W;
            S := R;
            goto 700;
630:        M := I;
            if Lambda^[I].Y <> 0.0 then goto 640;
            T := W;
            if T <> 0.0 then goto 635;
            Tst1 := Norm;
            T := Tst1;
            repeat
              T := 0.01 * T;
              Tst2 := Norm + T;
            until Tst2 <= Tst1;
635:        H^[I]^[En] := - R / T;
            goto 680;

640:        { Solve real equations }
            X := H^[I]^[I + 1];
            Y := H^[I + 1]^[I];
            Q := Sqr(Lambda^[I].X - P) + Sqr(Lambda^[I].Y);
            T := (X * S - Zz * R) / Q;
            H^[I]^[En] := T;
            if Abs(X) > Abs(Zz) then
              H^[I + 1]^[En] := (- R - W * T) / X
            else
              H^[I + 1]^[En] := (- S - Y * T) / Zz;

680:        { Overflow control }
            T := Abs(H^[I]^[En]);
            if T = 0.0 then goto 700;
            Tst1 := T;
            Tst2 := Tst1 + 1.0 / Tst1;
            if Tst2 > Tst1 then goto 700;
            for J := I to En do
              H^[J]^[En] := H^[J]^[En] / T;
700:      end;
        { End real vector }
        goto 800;

        { Complex vector }
710:    M := Na;

        { Last vector component chosen imaginary so that
          eigenvector matrix is triangular }
        if Abs(H^[En]^[Na]) > Abs(H^[Na]^[En]) then
          begin
            H^[Na]^[Na] := Q / H^[En]^[Na];
            H^[Na]^[En] := - (H^[En]^[En] - P) / H^[En]^[Na];
          end
        else
          Cdiv(0.0, - H^[Na]^[En], H^[Na]^[Na] - P, Q, H^[Na]^[Na], H^[Na]^[En]);

        H^[En]^[Na] := 0.0;
        H^[En]^[En] := 1.0;
        Enm2 := Na - 1;
        if Enm2 < Lb then goto 800;

        for I := Enm2 downto Lb do
          begin
            W := H^[I]^[I] - P;
            Ra := 0.0;
            Sa := 0.0;

            for J := M to En do
              begin
                Ra := Ra + H^[I]^[J] * H^[J]^[Na];
                Sa := Sa + H^[I]^[J] * H^[J]^[En];
              end;

            if Lambda^[I].Y >= 0.0 then goto 770;
            Zz := W;
            R := Ra;
            S := Sa;
            goto 795;
770:        M := I;
            if Lambda^[I].Y <> 0.0 then goto 780;
            Cdiv(- Ra, - Sa, W, Q, H^[I]^[Na], H^[I]^[En]);
            goto 790;

            { Solve complex equations }
780:        X := H^[I]^[I + 1];
            Y := H^[I + 1]^[I];
            Vr := Sqr(Lambda^[I].X - P) + Sqr(Lambda^[I].Y) - Sqr(Q);
            Vi := (Lambda^[I].X - P) * 2.0 * Q;
            if (Vr = 0.0) and (Vi = 0.0) then
              begin
                Tst1 := Norm * (Abs(W) + Abs(Q) + Abs(X) + Abs(Y) + Abs(Zz));
                Vr := Tst1;
                repeat
                  Vr := 0.01 * Vr;
                  Tst2 := Tst1 + Vr;
                until Tst2 <= Tst1;
              end;
            Cdiv(X * R - Zz * Ra + Q * Sa, X * S - Zz * Sa - Q * Ra, Vr, Vi, H^[I]^[Na], H^[I]^[En]);
            if Abs(X) > Abs(Zz) + Abs(Q) then
              begin
                H^[I + 1]^[Na] := (- Ra - W * H^[I]^[Na] + Q * H^[I]^[En]) / X;
                H^[I + 1]^[En] := (- Sa - W * H^[I]^[En] - Q * H^[I]^[Na]) / X;
              end
            else
              Cdiv(- R - Y * H^[I]^[Na], - S - Y * H^[I]^[En], Zz, Q, H^[I + 1]^[Na], H^[I + 1]^[En]);

790:        { Overflow control }
            T := Max(Abs(H^[I]^[Na]), Abs(H^[I]^[En]));
            if T = 0.0 then goto 795;
            Tst1 := T;
            Tst2 := Tst1 + 1.0 / Tst1;
            if Tst2 > Tst1 then goto 795;
            for J := I to En do
              begin
                H^[J]^[Na] := H^[J]^[Na] / T;
                H^[J]^[En] := H^[J]^[En] / T;
              end;

795:      end;
      { End complex vector }
800:  end;

    { End back substitution.
      Vectors of isolated roots }
  for I := Lb to Ub do
    if (I < I_low) or (I > I_igh) then
      for J := I to Ub do
        Z^[I]^[J] := H^[I]^[J];

  { Multiply by transformation matrix to give
    vectors of original full matrix. }
  for J := Ub downto I_low do
    begin
      M := Min(J, I_igh);
      for I := I_low to I_igh do
        begin
          Zz := 0.0;
          for K := I_low to M do
            Zz := Zz + Z^[I]^[K] * H^[K]^[J];
          Z^[I]^[J] := Zz;
        end;
    end;

//  SetErrCode(0);
end;

procedure BalBak(Z                    : PMatrixOfExtended;
                 Lb, Ub, I_low, I_igh : Integer;
                 Scale                : PArrayOfExtended;
                 M                    : Integer);
{ ------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Balbak

  This procedure forms the eigenvectors of a real general matrix
  by back transforming those of the corresponding balanced matrix
  determined by Balance.

  On input:

    Z contains the real and imaginary parts of the eigenvectors
    to be back transformed.

    Lb, Ub are the lowest and highest indices
    of the elements of Z

    I_low and I_igh are integers determined by Balance.

    Scale contains information determining the permutations
    and scaling factors used by Balance.

    M is the index of the latest column of Z to be back transformed.

  On output:

    Z contains the real and imaginary parts of the transformed
    eigenvectors in its columns Lb..M
  ------------------------------------------------------------------ }
var
  I, J, K : Integer;
  S       : Extended;
begin
  if M < Lb then Exit;

  if I_igh <> I_low then
    for I := I_low to I_igh do
      begin
        S := Scale^[I];
        { Left hand eigenvectors are back transformed if the
          foregoing statement is replaced by S := 1.0 / Scale^[I] }
        for J := Lb to M do
          Z^[I]^[J] := Z^[I]^[J] * S;
      end;

  for I := (I_low - 1) downto Lb do
    begin
      K := Round(Scale^[I]);
      if K <> I then
        for J := Lb to M do
          begin
            S := Z^[I]^[J];
            Z^[I]^[J] := Z^[K]^[J];
            Z^[K]^[J] := S;
          end;
    end;

  for I := (I_igh + 1) to Ub do
    begin
      K := Round(Scale^[I]);
      if K <> I then
        for J := Lb to M do
          begin
            S := Z^[I]^[J];
            Z^[I]^[J] := Z^[K]^[J];
            Z^[K]^[J] := S;
          end;
    end;
end;

procedure EigenVect(A: PMatrixOfExtended; Lb, Ub : Integer;
                    Lambda: PArrayOfComplex; V: PMatrixOfExtended);
var
  I_low: Integer = -1;
  I_igh: Integer = -1;
  Scale        : PArrayOfExtended;
  I_Int        : PArrayOfInt;
begin
  GetMem(Scale, SizeOf(Extended)*(Ub+1));
  GetMem(I_Int, SizeOf(Integer)*(Ub+1));

  Balance(A, Lb, Ub, I_low, I_igh, Scale);
  ElmHes(A, Lb, Ub, I_low, I_igh, I_int);
  Eltran(A, Lb, Ub, I_low, I_igh, I_int, V);

  if Hqr2(A, Lb, Ub, I_low, I_igh, Lambda, V) = 0 then
    BalBak(V, Lb, Ub, I_low, I_igh, Scale, Ub);

  FreeMemAndNil(Scale);
  FreeMemAndNil(I_Int);
end;

end.
