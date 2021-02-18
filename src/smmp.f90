MODULE SMMP

  IMPLICIT NONE

  INTERFACE

    !> Symbolic matrix-matrix multiply.
    SUBROUTINE SYMBMM(N,M,L,IA,JA,DIAGA,IB,JB,DIAGB,IC,JC,DIAGC,INDEX)
      INTEGER :: N
      INTEGER :: M
      INTEGER :: L
      INTEGER :: IA(*)
      INTEGER :: JA(*)
      INTEGER :: DIAGA
      INTEGER :: IB(*)
      INTEGER :: JB(*)
      INTEGER :: DIAGB
      INTEGER :: IC(*)
      INTEGER :: JC(*)
      INTEGER :: DIAGC
      INTEGER :: INDEX(*)
    END SUBROUTINE SYMBMM

    !> Numerical matrix-matrix multiply.
    SUBROUTINE NUMBMM(N,M,L,IA,JA,DIAGA,A,IB,JB,DIAGB,B,IC,JC,DIAGC,C,TEMP)
      INTEGER :: N
      INTEGER :: M
      INTEGER :: L
      INTEGER :: IA(*)
      INTEGER :: JA(*)
      INTEGER :: DIAGA
      REAL :: A(*)
      INTEGER :: IB(*)
      INTEGER :: JB(*)
      INTEGER :: DIAGB
      REAL :: B(*)
      INTEGER :: IC(*)
      INTEGER :: JC(*)
      INTEGER :: DIAGC
      REAL :: C(*)
      REAL :: TEMP(*)
    END SUBROUTINE NUMBMM

    !> Transpose of a sparse matrix.
    SUBROUTINE TRANSP(N,M,IA,JA,DIAGA,A,IB,JB,B,MOVE)
      INTEGER :: N
      INTEGER :: M
      INTEGER :: IA(*)
      INTEGER :: JA(*)
      INTEGER :: DIAGA
      REAL :: A(*)
      INTEGER :: IB(*)
      INTEGER :: JB(*)
      REAL :: B(*)
      INTEGER :: MOVE
    END SUBROUTINE TRANSP

    !> Convert a Yale format sparse matrix into the Bank-Smith format.
    SUBROUTINE YTOBS(N,IA,JA,DIAGA,SYMA,A,IB,JB,B,MOVE)
      INTEGER :: N
      INTEGER :: IA(*)
      INTEGER :: JA(*)
      INTEGER :: DIAGA
      INTEGER :: SYMA
      REAL :: A(*)
      INTEGER :: IB(*)
      INTEGER :: JB(*)
      REAL :: B(*)
      INTEGER :: MOVE
    END SUBROUTINE YTOBS

    !> Convert a Bank-Smith format sparse matric into one of the Yale format.
    SUBROUTINE BSTOY(N,IA,JA,SYMA,A,IB,JB,DIAGB,B,MOVE)
      INTEGER :: N
      INTEGER :: IA(*)
      INTEGER :: JA(*)
      INTEGER :: SYMA
      REAL :: A(*)
      INTEGER :: IB(*)
      INTEGER :: JB(*)
      INTEGER :: DIAGB
      REAL :: B(*)
      INTEGER :: MOVE
    END SUBROUTINE BSTOY

  END INTERFACE

END MODULE