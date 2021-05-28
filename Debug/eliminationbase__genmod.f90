        !COMPILER-GENERATED INTERFACE MODULE: Thu May 27 20:36:19 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ELIMINATIONBASE__genmod
          INTERFACE 
            SUBROUTINE ELIMINATIONBASE(MATRIX,SIZE,ROWSWAPS)
              REAL(KIND=4) :: MATRIX(20,20)
              INTEGER(KIND=4), INTENT(IN) :: SIZE
              INTEGER(KIND=4) :: ROWSWAPS
            END SUBROUTINE ELIMINATIONBASE
          END INTERFACE 
        END MODULE ELIMINATIONBASE__genmod
