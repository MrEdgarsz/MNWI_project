        !COMPILER-GENERATED INTERFACE MODULE: Thu May 27 19:02:03 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SWAPROWS__genmod
          INTERFACE 
            SUBROUTINE SWAPROWS(MATRIX,SIZE,ROWSWAPS)
              REAL(KIND=4) :: MATRIX(20,20)
              INTEGER(KIND=4), INTENT(IN) :: SIZE
              INTEGER(KIND=4) :: ROWSWAPS
            END SUBROUTINE SWAPROWS
          END INTERFACE 
        END MODULE SWAPROWS__genmod
