        !COMPILER-GENERATED INTERFACE MODULE: Thu May 27 20:36:19 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHECKROWSINMATRIX__genmod
          INTERFACE 
            FUNCTION CHECKROWSINMATRIX(MATRIX,SIZE)
              REAL(KIND=4), INTENT(IN) :: MATRIX(20,20)
              INTEGER(KIND=4), INTENT(IN) :: SIZE
              LOGICAL(KIND=4) :: CHECKROWSINMATRIX
            END FUNCTION CHECKROWSINMATRIX
          END INTERFACE 
        END MODULE CHECKROWSINMATRIX__genmod
