C ***********************************************************
C Fortran file translated from WHIRL Mon May  4 15:13:26 2009
C ***********************************************************
C ***********************************************************

      MODULE a
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i4) LONGNAME
C
C     **** Statements ****
C
      END MODULE

      MODULE b
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i4) LONGNAME
C
C     **** Statements ****
C
      END MODULE

      MODULE a1
      use w2f__types
      use a ,only: S => LONGNAME
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      interface  A1FOO
        module procedure  BAR

      end interface 
      
      CONTAINS

        SUBROUTINE BAR
        use w2f__types
        IMPLICIT NONE
C
C       **** Statements ****
C
        S = S + 1
        RETURN
        END SUBROUTINE
      END

      MODULE b1
      use w2f__types
      use b ,only: S => LONGNAME
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      interface  B1FOO
        module procedure  FOO

      end interface 
      
      CONTAINS

        SUBROUTINE FOO
        use w2f__types
        IMPLICIT NONE
C
C       **** Statements ****
C
        S = S + 1
        RETURN
        END SUBROUTINE
      END

      PROGRAM multrename
      use w2f__types
      use a ,only: AL => LONGNAME
      use b ,only: BL => LONGNAME
      use a1
      use b1
      IMPLICIT NONE
C
C     **** Statements ****
C
      AL = 1
      BL = 2
      CALL BAR()
      CALL FOO()
      IF((AL .eq. 2) .AND.(BL .eq. 3)) THEN
        WRITE(*, *) 'OK'
      ELSE
        WRITE(*, *) 'FAILED'
      ENDIF
      
      END PROGRAM
