
C$OPENAD XXX File_start [OAD_intrinsics.f90]
      MODULE oad_intrinsics
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      END MODULE

C$OPENAD XXX File_start [all_globals_mod.f]
      MODULE all_globals_mod
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      END MODULE

C$OPENAD XXX File_start [head.f]
      SUBROUTINE head(X, Y)
      use w2f__types
      use oad_intrinsics
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      TYPE (OpenADTy_active) X(1 : 3)
      INTENT(IN)  X
      TYPE (OpenADTy_active) Y(1 : 3)
      INTENT(OUT)  Y
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) I
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(X)
C$OPENAD DEPENDENT(Y)
C
C     **** Statements ****
C
C$OPENAD XXX Template ad_template.f
      IF(__value__(X(1)) .LT. 4.0D00) THEN
C$OPENAD XXX Simple loop
        DO I = 1, 3, 1
          __value__(Y(INT(I))) = __value__(X(I))
          CALL setderiv(__deriv__(Y(I)), __deriv__(X(I)))
        END DO
      ELSE
C$OPENAD XXX Simple loop
        DO I = 1, 3, 1
          __value__(Y(INT(I))) = (__value__(X(I)) * 2.0D00)
          CALL sax(2.0D00, __deriv__(X(I)), __deriv__(Y(I)))
        END DO
      ENDIF
      END SUBROUTINE
