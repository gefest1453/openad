
      MODULE adsize
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) NCMAX
      PARAMETER ( NCMAX = 200)
C
C     **** Statements ****
C
      END MODULE

      MODULE size
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) NX
      PARAMETER ( NX = 20)
      INTEGER(w2f__i4) NY
      PARAMETER ( NY = 20)
C
C     **** Statements ****
C
      END MODULE

      MODULE parms
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      REAL(w2f__8) AH
      REAL(w2f__8) BETA
      LOGICAL(w2f__i4) CALC_HESS
      LOGICAL(w2f__i4) CARTESIAN
      CHARACTER(80) DEPTHFILE
      REAL(w2f__8) DT
      REAL(w2f__8) DT_DUMP
      CHARACTER(80) ETAINIFILE
      REAL(w2f__8) F0
      CHARACTER(80) FORCINGFILE
      CHARACTER(80) FOUTNAME
      LOGICAL(w2f__i4) FULLIO
      LOGICAL(w2f__i4) GRAD_CHECK
      LOGICAL(w2f__i4) INITIAL_GRAD
      INTEGER(w2f__i4) ITERATION
      CHARACTER(80) NCDATAFILE
      CHARACTER(80) NCRESTARTFILE
      INTEGER(w2f__i4) NT
      INTEGER(w2f__i4) NTSPINUP
      REAL(w2f__8) OM
      LOGICAL(w2f__i4) OPTIMIZE
      LOGICAL(w2f__i4) QUADFRIC
      REAL(w2f__8) RINI
      CHARACTER(80) RUNNAME
      LOGICAL(w2f__i4) SPHERICAL
      REAL(w2f__8) START_TIME
      LOGICAL(w2f__i4) SUPPRESSIO
      CHARACTER(80) UINIFILE
      CHARACTER(80) VINIFILE
      LOGICAL(w2f__i4) XPERIODIC
      REAL(w2f__8) XSTART
      LOGICAL(w2f__i4) YPERIODIC
      REAL(w2f__8) YSTART
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) DEG2RAD
      PARAMETER ( DEG2RAD = 1.74532925199432954744D-02)
      REAL(w2f__8) EARTH
      PARAMETER ( EARTH = 6.371D+06)
      REAL(w2f__8) G
      PARAMETER ( G = 9.81000000000000049738D00)
      REAL(w2f__8) INVRHO0
      PARAMETER ( INVRHO0 = 9.72762645914396900659D-04)
      REAL(w2f__8) PI
      PARAMETER ( PI = 3.141592653589793116D00)
      REAL(w2f__8) RHO0
      PARAMETER ( RHO0 = 1.028D+03)
C
C     **** Statements ****
C
      END MODULE

      MODULE vars
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      TYPE (oadactive) ETA(0 : 21, 0 : 21)
      TYPE (oadactive) U(0 : 21, 0 : 21)
      TYPE (oadactive) V(0 : 21, 0 : 21)
C
C     **** Statements ****
C
      END MODULE

      MODULE pfields
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      TYPE (oadactive) DEPTH(0 : 21, 0 : 21)
      REAL(w2f__8) DX(0 : 20)
      REAL(w2f__8) DY(0 : 20)
      REAL(w2f__8) ETAINI(1 : 20, 1 : 20)
      REAL(w2f__8) ETAMASK(0 : 21, 0 : 21)
      REAL(w2f__8) FCORIU(1 : 20, 1 : 20)
      REAL(w2f__8) FCORIV(1 : 20, 1 : 20)
      REAL(w2f__8) FRICT(0 : 21, 0 : 21)
      TYPE (oadactive) HU(0 : 21, 0 : 21)
      TYPE (oadactive) HV(0 : 21, 0 : 21)
      REAL(w2f__8) HY(0 : 21)
      REAL(w2f__8) INIDEPTH(1 : 20, 1 : 20)
      TYPE (oadactive) INVHU(0 : 21, 0 : 21)
      TYPE (oadactive) INVHV(0 : 21, 0 : 21)
      REAL(w2f__8) RX(0 : 21)
      REAL(w2f__8) RY
      REAL(w2f__8) SCALEDEPTH(1 : 20, 1 : 20)
      REAL(w2f__8) SCALEETA(1 : 20, 1 : 20)
      REAL(w2f__8) SCALEU(1 : 20, 1 : 20)
      REAL(w2f__8) SCALEV(1 : 20, 1 : 20)
      REAL(w2f__8) UINI(1 : 20, 1 : 20)
      REAL(w2f__8) UMASK(0 : 21, 0 : 21)
      REAL(w2f__8) VINI(1 : 20, 1 : 20)
      REAL(w2f__8) VMASK(0 : 21, 0 : 21)
      REAL(w2f__8) X(0 : 21)
      REAL(w2f__8) Y(0 : 21)
C
C     **** Statements ****
C
      END MODULE

      MODULE force
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      REAL(w2f__8) UFORCE(1 : 20, 1 : 20)
      REAL(w2f__8) VFORCE(1 : 20, 1 : 20)
C
C     **** Statements ****
C
      END MODULE

      MODULE data
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      REAL(w2f__8) DEPTH_DATA(1 : 20, 1 : 20)
      REAL(w2f__8) ETA_DATA(1 : 20, 1 : 20)
      REAL(w2f__8) ETA_DATA_TIME(1 : 1000)
      INTEGER(w2f__i4) NEDT
      REAL(w2f__8) U_DATA(1 : 20, 1 : 20)
      REAL(w2f__8) V_DATA(1 : 20, 1 : 20)
      REAL(w2f__8) ZONAL_TRANSPORT_DATA
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) NEDTMAX
      PARAMETER ( NEDTMAX = 1000)
C
C     **** Statements ****
C
      END MODULE

      MODULE weights
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      REAL(w2f__8) WEIGHT_DEPTH(1 : 20, 1 : 20)
      REAL(w2f__8) WEIGHT_ETA(1 : 20, 1 : 20)
      REAL(w2f__8) WEIGHT_GRADDEPTH(1 : 20, 1 : 20)
      REAL(w2f__8) WEIGHT_LAPLDEPTH(1 : 20, 1 : 20)
      REAL(w2f__8) WEIGHT_U(1 : 20, 1 : 20)
      REAL(w2f__8) WEIGHT_V(1 : 20, 1 : 20)
      REAL(w2f__8) WEIGHT_ZONAL_TRANSPORT
      REAL(w2f__8) WF_DEPTH
      REAL(w2f__8) WF_ETA
      REAL(w2f__8) WF_GRADDEPTH
      REAL(w2f__8) WF_LAPLDEPTH
      REAL(w2f__8) WF_U
      REAL(w2f__8) WF_V
      REAL(w2f__8) WF_ZONAL_TRANSPORT
C
C     **** Statements ****
C
      END MODULE

      MODULE mini
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      REAL(w2f__8) DF1
      REAL(w2f__8) DXMIN
      REAL(w2f__8) EPSG
      REAL(w2f__8) EPS_GRAD
      REAL(w2f__8) FACTR
      INTEGER(w2f__i4) IMPRES
      INTEGER(w2f__i4) IPRINT
      INTEGER(w2f__i4) MODE
      INTEGER(w2f__i4) NITER
      INTEGER(w2f__i4) NSIM
      REAL(w2f__8) PGTOL
C
C     **** Statements ****
C
      END MODULE

      MODULE size_small
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) NX
      PARAMETER ( NX = 20)
      INTEGER(w2f__i4) NY
      PARAMETER ( NY = 8)
C
C     **** Statements ****
C
      END MODULE

      SUBROUTINE inifields()
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_0
      INTEGER(w2f__i8) OpenAD_Symbol_1
      INTEGER(w2f__i8) OpenAD_Symbol_10
      INTEGER(w2f__i8) OpenAD_Symbol_11
      INTEGER(w2f__i8) OpenAD_Symbol_12
      INTEGER(w2f__i8) OpenAD_Symbol_13
      INTEGER(w2f__i8) OpenAD_Symbol_14
      INTEGER(w2f__i8) OpenAD_Symbol_15
      INTEGER(w2f__i8) OpenAD_Symbol_16
      INTEGER(w2f__i8) OpenAD_Symbol_17
      INTEGER(w2f__i8) OpenAD_Symbol_18
      INTEGER(w2f__i8) OpenAD_Symbol_19
      INTEGER(w2f__i8) OpenAD_Symbol_2
      INTEGER(w2f__i8) OpenAD_Symbol_20
      INTEGER(w2f__i8) OpenAD_Symbol_21
      INTEGER(w2f__i8) OpenAD_Symbol_22
      INTEGER(w2f__i8) OpenAD_Symbol_23
      INTEGER(w2f__i8) OpenAD_Symbol_24
      INTEGER(w2f__i8) OpenAD_Symbol_25
      INTEGER(w2f__i8) OpenAD_Symbol_26
      INTEGER(w2f__i8) OpenAD_Symbol_27
      INTEGER(w2f__i8) OpenAD_Symbol_28
      INTEGER(w2f__i8) OpenAD_Symbol_29
      INTEGER(w2f__i8) OpenAD_Symbol_3
      INTEGER(w2f__i8) OpenAD_Symbol_30
      INTEGER(w2f__i8) OpenAD_Symbol_31
      INTEGER(w2f__i8) OpenAD_Symbol_32
      INTEGER(w2f__i8) OpenAD_Symbol_33
      INTEGER(w2f__i8) OpenAD_Symbol_34
      INTEGER(w2f__i8) OpenAD_Symbol_35
      INTEGER(w2f__i8) OpenAD_Symbol_4
      INTEGER(w2f__i8) OpenAD_Symbol_5
      INTEGER(w2f__i8) OpenAD_Symbol_6
      INTEGER(w2f__i8) OpenAD_Symbol_7
      INTEGER(w2f__i8) OpenAD_Symbol_8
      INTEGER(w2f__i8) OpenAD_Symbol_9
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1310
      INTEGER(w2f__i4) OpenAD_Symbol_1311
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      NT = 0
      NTSPINUP = 0
      DT = 0.0D00
      START_TIME = 0.0D00
      DT_DUMP = 0.0D00
      ITERATION = (-9999)
      RINI = 0.0D00
      OM = 0.0D00
      F0 = 0.0D00
      BETA = 0.0D00
      XSTART = 0.0D00
      YSTART = 0.0D00
      XPERIODIC = .FALSE.
      YPERIODIC = .FALSE.
      SPHERICAL = .FALSE.
      CARTESIAN = .FALSE.
      QUADFRIC = .FALSE.
      SUPPRESSIO = .FALSE.
      FULLIO = .FALSE.
      INITIAL_GRAD = .FALSE.
      GRAD_CHECK = .FALSE.
      OPTIMIZE = .FALSE.
      CALC_HESS = .FALSE.
      FOUTNAME = ' '
      RUNNAME = ' '
      DEPTHFILE = ' '
      FORCINGFILE = ' '
      UINIFILE = ' '
      VINIFILE = ' '
      ETAINIFILE = ' '
      NCDATAFILE = ' '
      WF_DEPTH = 0.0D00
      WF_ETA = 0.0D00
      WF_U = 0.0D00
      WF_V = 0.0D00
      WF_ZONAL_TRANSPORT = 0.0D00
      WF_LAPLDEPTH = 0.0D00
      WF_GRADDEPTH = 0.0D00
C$OPENAD XXX Simple loop
      DO IX = 0, 21, 1
        X(INT(IX)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0, 21, 1
        Y(INT(IY)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IX = 0, 20, 1
        DX(INT(IX)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0, 20, 1
        DY(INT(IY)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0, 21, 1
        RX(INT(IY)) = 0.0D00
      END DO
      RY = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 0, 21, 1
        HY(INT(IY)) = 0.0D00
      END DO
      DT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          UFORCE(INT(IX), INT(IY)) = 0.0D00
          VFORCE(INT(IX), INT(IY)) = 0.0D00
          FCORIU(INT(IX), INT(IY)) = 0.0D00
          FCORIV(INT(IX), INT(IY)) = 0.0D00
          INIDEPTH(INT(IX), INT(IY)) = 0.0D00
          UINI(INT(IX), INT(IY)) = 0.0D00
          VINI(INT(IX), INT(IY)) = 0.0D00
          ETAINI(INT(IX), INT(IY)) = 0.0D00
          SCALEDEPTH(INT(IX), INT(IY)) = 0.0D00
          U_DATA(INT(IX), INT(IY)) = 0.0D00
          V_DATA(INT(IX), INT(IY)) = 0.0D00
          ETA_DATA(INT(IX), INT(IY)) = 0.0D00
          DEPTH_DATA(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_U(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_V(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_ETA(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = 0.0D00
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_DEPTH(INT(IX), INT(IY)) = 1.0D00
        END DO
      END DO
      ZONAL_TRANSPORT_DATA = 0.0D00
      WEIGHT_ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IX = 0, 21, 1
        DO IY = 0, 21, 1
          __value__(DEPTH(INT(IX), INT(IY))) = 0.0D00
          FRICT(INT(IX), INT(IY)) = 0.0D00
          __value__(U(INT(IX), INT(IY))) = 0.0D00
          __value__(V(INT(IX), INT(IY))) = 0.0D00
          __value__(ETA(INT(IX), INT(IY))) = 0.0D00
          UMASK(INT(IX), INT(IY)) = 0.0D00
          VMASK(INT(IX), INT(IY)) = 0.0D00
          ETAMASK(INT(IX), INT(IY)) = 0.0D00
          __value__(HU(INT(IX), INT(IY))) = 0.0D00
          __value__(HV(INT(IX), INT(IY))) = 0.0D00
          __value__(INVHU(INT(IX), INT(IY))) = 0.0D00
          __value__(INVHV(INT(IX), INT(IY))) = 0.0D00
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      NT = 0
      NTSPINUP = 0
      DT = 0.0D00
      START_TIME = 0.0D00
      DT_DUMP = 0.0D00
      ITERATION = (-9999)
      RINI = 0.0D00
      OM = 0.0D00
      F0 = 0.0D00
      BETA = 0.0D00
      XSTART = 0.0D00
      YSTART = 0.0D00
      XPERIODIC = .FALSE.
      YPERIODIC = .FALSE.
      SPHERICAL = .FALSE.
      CARTESIAN = .FALSE.
      QUADFRIC = .FALSE.
      SUPPRESSIO = .FALSE.
      FULLIO = .FALSE.
      INITIAL_GRAD = .FALSE.
      GRAD_CHECK = .FALSE.
      OPTIMIZE = .FALSE.
      CALC_HESS = .FALSE.
      WF_DEPTH = 0.0D00
      WF_ETA = 0.0D00
      WF_U = 0.0D00
      WF_V = 0.0D00
      WF_ZONAL_TRANSPORT = 0.0D00
      WF_LAPLDEPTH = 0.0D00
      WF_GRADDEPTH = 0.0D00
      FOUTNAME = ' '
      RUNNAME = ' '
      DEPTHFILE = ' '
      FORCINGFILE = ' '
      UINIFILE = ' '
      VINIFILE = ' '
      ETAINIFILE = ' '
      NCDATAFILE = ' '
C$OPENAD XXX Simple loop
      DO IX = 0, 21, 1
        X(INT(IX)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0, 21, 1
        Y(INT(IY)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IX = 0, 20, 1
        DX(INT(IX)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0, 20, 1
        DY(INT(IY)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0, 21, 1
        RX(INT(IY)) = 0.0D00
      END DO
      RY = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 0, 21, 1
        HY(INT(IY)) = 0.0D00
      END DO
      DT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          UFORCE(INT(IX), INT(IY)) = 0.0D00
          VFORCE(INT(IX), INT(IY)) = 0.0D00
          FCORIU(INT(IX), INT(IY)) = 0.0D00
          FCORIV(INT(IX), INT(IY)) = 0.0D00
          INIDEPTH(INT(IX), INT(IY)) = 0.0D00
          UINI(INT(IX), INT(IY)) = 0.0D00
          VINI(INT(IX), INT(IY)) = 0.0D00
          ETAINI(INT(IX), INT(IY)) = 0.0D00
          SCALEDEPTH(INT(IX), INT(IY)) = 0.0D00
          U_DATA(INT(IX), INT(IY)) = 0.0D00
          V_DATA(INT(IX), INT(IY)) = 0.0D00
          ETA_DATA(INT(IX), INT(IY)) = 0.0D00
          DEPTH_DATA(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_U(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_V(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_ETA(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = 0.0D00
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_DEPTH(INT(IX), INT(IY)) = 1.0D00
        END DO
      END DO
      ZONAL_TRANSPORT_DATA = 0.0D00
      WEIGHT_ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IX = 0, 21, 1
        DO IY = 0, 21, 1
          __value__(DEPTH(INT(IX), INT(IY))) = 0.0D00
          FRICT(INT(IX), INT(IY)) = 0.0D00
          __value__(U(INT(IX), INT(IY))) = 0.0D00
          __value__(V(INT(IX), INT(IY))) = 0.0D00
          __value__(ETA(INT(IX), INT(IY))) = 0.0D00
          UMASK(INT(IX), INT(IY)) = 0.0D00
          VMASK(INT(IX), INT(IY)) = 0.0D00
          ETAMASK(INT(IX), INT(IY)) = 0.0D00
          __value__(HU(INT(IX), INT(IY))) = 0.0D00
          __value__(HV(INT(IX), INT(IY))) = 0.0D00
          __value__(INVHU(INT(IX), INT(IY))) = 0.0D00
          __value__(INVHV(INT(IX), INT(IY))) = 0.0D00
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IX = 0 + 1 *((21 - 0) / 1)
      DO WHILE(IX .GE. 0)
        IY = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IY .GE. 0)
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(INVHV(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(INVHU(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HV(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HU(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(DEPTH(IX, IY)))
          IY = IY - 1
        END DO
        IX = IX - 1
      END DO
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      IY = 0 + 1 *((21 - 0) / 1)
      DO WHILE(IY .GE. 0)
        IY = IY - 1
      END DO
      IY = 0 + 1 *((21 - 0) / 1)
      DO WHILE(IY .GE. 0)
        IY = IY - 1
      END DO
      IY = 0 + 1 *((20 - 0) / 1)
      DO WHILE(IY .GE. 0)
        IY = IY - 1
      END DO
      IX = 0 + 1 *((20 - 0) / 1)
      DO WHILE(IX .GE. 0)
        IX = IX - 1
      END DO
      IY = 0 + 1 *((21 - 0) / 1)
      DO WHILE(IY .GE. 0)
        IY = IY - 1
      END DO
      IX = 0 + 1 *((21 - 0) / 1)
      DO WHILE(IX .GE. 0)
        IX = IX - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_HESS)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(GRAD_CHECK)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(INITIAL_GRAD)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(OPTIMIZE)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(QUADFRIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_DEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(DEPTHFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(ETAINIFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(FORCINGFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(FOUTNAME)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(NCDATAFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(RUNNAME)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(UINIFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(VINIFILE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_DEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_DEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(VINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(UINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(RUNNAME)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(NCDATAFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(FOUTNAME)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(FORCINGFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(ETAINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(DEPTHFILE)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_DEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(QUADFRIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(OPTIMIZE)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(INITIAL_GRAD)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(GRAD_CHECK)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_HESS)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(BETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      NT = 0
      NTSPINUP = 0
      DT = 0.0D00
      START_TIME = 0.0D00
      DT_DUMP = 0.0D00
      ITERATION = (-9999)
      RINI = 0.0D00
      OM = 0.0D00
      F0 = 0.0D00
      BETA = 0.0D00
      XSTART = 0.0D00
      YSTART = 0.0D00
      XPERIODIC = .FALSE.
      YPERIODIC = .FALSE.
      SPHERICAL = .FALSE.
      CARTESIAN = .FALSE.
      QUADFRIC = .FALSE.
      SUPPRESSIO = .FALSE.
      FULLIO = .FALSE.
      INITIAL_GRAD = .FALSE.
      GRAD_CHECK = .FALSE.
      OPTIMIZE = .FALSE.
      CALC_HESS = .FALSE.
      WF_DEPTH = 0.0D00
      WF_ETA = 0.0D00
      WF_U = 0.0D00
      WF_V = 0.0D00
      WF_ZONAL_TRANSPORT = 0.0D00
      WF_LAPLDEPTH = 0.0D00
      WF_GRADDEPTH = 0.0D00
      FOUTNAME = ' '
      RUNNAME = ' '
      DEPTHFILE = ' '
      FORCINGFILE = ' '
      UINIFILE = ' '
      VINIFILE = ' '
      ETAINIFILE = ' '
      NCDATAFILE = ' '
C$OPENAD XXX Simple loop
      OpenAD_Symbol_24 = 0_w2f__i8
      DO IX = 0, 21, 1
        X(INT(IX)) = 0.0D00
        OpenAD_Symbol_24 = (INT(OpenAD_Symbol_24) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_24)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_25 = 0_w2f__i8
      DO IY = 0, 21, 1
        Y(INT(IY)) = 0.0D00
        OpenAD_Symbol_25 = (INT(OpenAD_Symbol_25) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_25)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_26 = 0_w2f__i8
      DO IX = 0, 20, 1
        DX(INT(IX)) = 0.0D00
        OpenAD_Symbol_26 = (INT(OpenAD_Symbol_26) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_26)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_27 = 0_w2f__i8
      DO IY = 0, 20, 1
        DY(INT(IY)) = 0.0D00
        OpenAD_Symbol_27 = (INT(OpenAD_Symbol_27) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_27)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_28 = 0_w2f__i8
      DO IY = 0, 21, 1
        RX(INT(IY)) = 0.0D00
        OpenAD_Symbol_28 = (INT(OpenAD_Symbol_28) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_28)
      RY = 0.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_29 = 0_w2f__i8
      DO IY = 0, 21, 1
        HY(INT(IY)) = 0.0D00
        OpenAD_Symbol_29 = (INT(OpenAD_Symbol_29) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_29)
      DT = 0.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_30 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_31 = 0_w2f__i8
        DO IX = 1, 20, 1
          UFORCE(INT(IX), INT(IY)) = 0.0D00
          VFORCE(INT(IX), INT(IY)) = 0.0D00
          FCORIU(INT(IX), INT(IY)) = 0.0D00
          FCORIV(INT(IX), INT(IY)) = 0.0D00
          INIDEPTH(INT(IX), INT(IY)) = 0.0D00
          UINI(INT(IX), INT(IY)) = 0.0D00
          VINI(INT(IX), INT(IY)) = 0.0D00
          ETAINI(INT(IX), INT(IY)) = 0.0D00
          SCALEDEPTH(INT(IX), INT(IY)) = 0.0D00
          U_DATA(INT(IX), INT(IY)) = 0.0D00
          V_DATA(INT(IX), INT(IY)) = 0.0D00
          ETA_DATA(INT(IX), INT(IY)) = 0.0D00
          DEPTH_DATA(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_U(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_V(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_ETA(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = 0.0D00
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = 0.0D00
          OpenAD_Symbol_31 = (INT(OpenAD_Symbol_31) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_31)
        OpenAD_Symbol_30 = (INT(OpenAD_Symbol_30) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_30)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_32 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_33 = 0_w2f__i8
        DO IX = 1, 20, 1
          WEIGHT_DEPTH(INT(IX), INT(IY)) = 1.0D00
          OpenAD_Symbol_33 = (INT(OpenAD_Symbol_33) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_33)
        OpenAD_Symbol_32 = (INT(OpenAD_Symbol_32) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_32)
      ZONAL_TRANSPORT_DATA = 0.0D00
      WEIGHT_ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_34 = 0_w2f__i8
      DO IX = 0, 21, 1
        OpenAD_Symbol_35 = 0_w2f__i8
        DO IY = 0, 21, 1
          __value__(DEPTH(INT(IX), INT(IY))) = 0.0D00
          FRICT(INT(IX), INT(IY)) = 0.0D00
          __value__(U(INT(IX), INT(IY))) = 0.0D00
          __value__(V(INT(IX), INT(IY))) = 0.0D00
          __value__(ETA(INT(IX), INT(IY))) = 0.0D00
          UMASK(INT(IX), INT(IY)) = 0.0D00
          VMASK(INT(IX), INT(IY)) = 0.0D00
          ETAMASK(INT(IX), INT(IY)) = 0.0D00
          __value__(HU(INT(IX), INT(IY))) = 0.0D00
          __value__(HV(INT(IX), INT(IY))) = 0.0D00
          __value__(INVHU(INT(IX), INT(IY))) = 0.0D00
          __value__(INVHV(INT(IX), INT(IY))) = 0.0D00
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_35 = (INT(OpenAD_Symbol_35) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_35)
        OpenAD_Symbol_34 = (INT(OpenAD_Symbol_34) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_34)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_0)
      OpenAD_Symbol_1 = 1
      DO WHILE(INT(OpenAD_Symbol_1) .LE. INT(OpenAD_Symbol_0))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_2)
        OpenAD_Symbol_3 = 1
        DO WHILE(INT(OpenAD_Symbol_3) .LE. INT(OpenAD_Symbol_2))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1310)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1311)
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(INVHV(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(INVHU(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HV(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HU(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(DEPTH(OpenAD_Symbol_1311,
     >  OpenAD_Symbol_1310)))
          OpenAD_Symbol_3 = INT(OpenAD_Symbol_3) + 1
        END DO
        OpenAD_Symbol_1 = INT(OpenAD_Symbol_1) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_4)
      OpenAD_Symbol_5 = 1
      DO WHILE(INT(OpenAD_Symbol_5) .LE. INT(OpenAD_Symbol_4))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_6)
        OpenAD_Symbol_7 = 1
        DO WHILE(INT(OpenAD_Symbol_7) .LE. INT(OpenAD_Symbol_6))
          OpenAD_Symbol_7 = INT(OpenAD_Symbol_7) + 1
        END DO
        OpenAD_Symbol_5 = INT(OpenAD_Symbol_5) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_8)
      OpenAD_Symbol_9 = 1
      DO WHILE(INT(OpenAD_Symbol_9) .LE. INT(OpenAD_Symbol_8))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_10)
        OpenAD_Symbol_11 = 1
        DO WHILE(INT(OpenAD_Symbol_11) .LE. INT(OpenAD_Symbol_10))
          OpenAD_Symbol_11 = INT(OpenAD_Symbol_11) + 1
        END DO
        OpenAD_Symbol_9 = INT(OpenAD_Symbol_9) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_12)
      OpenAD_Symbol_13 = 1
      DO WHILE(INT(OpenAD_Symbol_13) .LE. INT(OpenAD_Symbol_12))
        OpenAD_Symbol_13 = INT(OpenAD_Symbol_13) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_14)
      OpenAD_Symbol_15 = 1
      DO WHILE(INT(OpenAD_Symbol_15) .LE. INT(OpenAD_Symbol_14))
        OpenAD_Symbol_15 = INT(OpenAD_Symbol_15) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_16)
      OpenAD_Symbol_17 = 1
      DO WHILE(INT(OpenAD_Symbol_17) .LE. INT(OpenAD_Symbol_16))
        OpenAD_Symbol_17 = INT(OpenAD_Symbol_17) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_18)
      OpenAD_Symbol_19 = 1
      DO WHILE(INT(OpenAD_Symbol_19) .LE. INT(OpenAD_Symbol_18))
        OpenAD_Symbol_19 = INT(OpenAD_Symbol_19) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_20)
      OpenAD_Symbol_21 = 1
      DO WHILE(INT(OpenAD_Symbol_21) .LE. INT(OpenAD_Symbol_20))
        OpenAD_Symbol_21 = INT(OpenAD_Symbol_21) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_22)
      OpenAD_Symbol_23 = 1
      DO WHILE(INT(OpenAD_Symbol_23) .LE. INT(OpenAD_Symbol_22))
        OpenAD_Symbol_23 = INT(OpenAD_Symbol_23) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(INVHV))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(ETA))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE readparms()
      use w2f__types
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      EXTERNAL check_cfl
      EXTERNAL ini_scales
      EXTERNAL make_masks
      EXTERNAL prep_coriolis
      EXTERNAL prep_depth
      EXTERNAL read_data_fields
      EXTERNAL read_data_file
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL read_data_file()
      CALL read_data_fields()
      CALL prep_depth()
      CALL check_cfl()
      CALL make_masks()
      CALL ini_scales()
      CALL prep_coriolis()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL read_data_file()
      CALL read_data_fields()
      CALL prep_depth()
      CALL check_cfl()
      CALL make_masks()
      CALL ini_scales()
      CALL prep_coriolis()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      CALL prep_coriolis()
      CALL ini_scales()
      CALL make_masks()
      CALL check_cfl()
      CALL prep_depth()
      CALL read_data_fields()
      CALL read_data_file()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(BETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL read_data_file()
      CALL read_data_fields()
      CALL prep_depth()
      CALL check_cfl()
      CALL make_masks()
      CALL ini_scales()
      CALL prep_coriolis()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
      CALL prep_coriolis()
      CALL ini_scales()
      CALL make_masks()
      CALL check_cfl()
      CALL prep_depth()
      CALL read_data_fields()
      CALL read_data_file()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_data_file()
      use w2f__types
      use size
      use parms
      use pfields
      use mini
      use weights
      use size
      use parms
      use pfields
      use mini
      use weights
      use size
      use parms
      use pfields
      use mini
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_100
      INTEGER(w2f__i8) OpenAD_Symbol_101
      INTEGER(w2f__i8) OpenAD_Symbol_102
      INTEGER(w2f__i8) OpenAD_Symbol_103
      INTEGER(w2f__i8) OpenAD_Symbol_104
      INTEGER(w2f__i8) OpenAD_Symbol_105
      INTEGER(w2f__i8) OpenAD_Symbol_106
      INTEGER(w2f__i8) OpenAD_Symbol_107
      INTEGER(w2f__i8) OpenAD_Symbol_108
      INTEGER(w2f__i8) OpenAD_Symbol_109
      INTEGER(w2f__i8) OpenAD_Symbol_110
      INTEGER(w2f__i8) OpenAD_Symbol_111
      INTEGER(w2f__i8) OpenAD_Symbol_112
      INTEGER(w2f__i8) OpenAD_Symbol_113
      INTEGER(w2f__i8) OpenAD_Symbol_114
      INTEGER(w2f__i8) OpenAD_Symbol_115
      INTEGER(w2f__i8) OpenAD_Symbol_116
      INTEGER(w2f__i8) OpenAD_Symbol_117
      INTEGER(w2f__i8) OpenAD_Symbol_118
      INTEGER(w2f__i8) OpenAD_Symbol_119
      INTEGER(w2f__i8) OpenAD_Symbol_120
      INTEGER(w2f__i8) OpenAD_Symbol_121
      INTEGER(w2f__i8) OpenAD_Symbol_122
      INTEGER(w2f__i8) OpenAD_Symbol_123
      INTEGER(w2f__i8) OpenAD_Symbol_124
      INTEGER(w2f__i8) OpenAD_Symbol_125
      INTEGER(w2f__i8) OpenAD_Symbol_126
      INTEGER(w2f__i8) OpenAD_Symbol_127
      INTEGER(w2f__i8) OpenAD_Symbol_128
      INTEGER(w2f__i8) OpenAD_Symbol_129
      INTEGER(w2f__i8) OpenAD_Symbol_130
      INTEGER(w2f__i8) OpenAD_Symbol_131
      INTEGER(w2f__i8) OpenAD_Symbol_132
      INTEGER(w2f__i8) OpenAD_Symbol_133
      INTEGER(w2f__i8) OpenAD_Symbol_134
      INTEGER(w2f__i8) OpenAD_Symbol_135
      INTEGER(w2f__i8) OpenAD_Symbol_136
      INTEGER(w2f__i8) OpenAD_Symbol_137
      INTEGER(w2f__i8) OpenAD_Symbol_138
      INTEGER(w2f__i8) OpenAD_Symbol_139
      INTEGER(w2f__i8) OpenAD_Symbol_140
      INTEGER(w2f__i8) OpenAD_Symbol_141
      INTEGER(w2f__i8) OpenAD_Symbol_142
      INTEGER(w2f__i8) OpenAD_Symbol_143
      INTEGER(w2f__i8) OpenAD_Symbol_144
      INTEGER(w2f__i8) OpenAD_Symbol_145
      INTEGER(w2f__i8) OpenAD_Symbol_146
      INTEGER(w2f__i8) OpenAD_Symbol_147
      INTEGER(w2f__i8) OpenAD_Symbol_148
      INTEGER(w2f__i8) OpenAD_Symbol_149
      INTEGER(w2f__i8) OpenAD_Symbol_150
      INTEGER(w2f__i8) OpenAD_Symbol_151
      INTEGER(w2f__i8) OpenAD_Symbol_152
      INTEGER(w2f__i8) OpenAD_Symbol_153
      INTEGER(w2f__i8) OpenAD_Symbol_154
      INTEGER(w2f__i8) OpenAD_Symbol_155
      INTEGER(w2f__i8) OpenAD_Symbol_36
      INTEGER(w2f__i8) OpenAD_Symbol_37
      INTEGER(w2f__i8) OpenAD_Symbol_38
      INTEGER(w2f__i8) OpenAD_Symbol_39
      INTEGER(w2f__i8) OpenAD_Symbol_40
      INTEGER(w2f__i8) OpenAD_Symbol_41
      INTEGER(w2f__i8) OpenAD_Symbol_42
      INTEGER(w2f__i8) OpenAD_Symbol_43
      INTEGER(w2f__i8) OpenAD_Symbol_44
      INTEGER(w2f__i8) OpenAD_Symbol_45
      INTEGER(w2f__i8) OpenAD_Symbol_46
      INTEGER(w2f__i8) OpenAD_Symbol_47
      INTEGER(w2f__i8) OpenAD_Symbol_48
      INTEGER(w2f__i8) OpenAD_Symbol_49
      INTEGER(w2f__i8) OpenAD_Symbol_50
      INTEGER(w2f__i8) OpenAD_Symbol_51
      INTEGER(w2f__i8) OpenAD_Symbol_52
      INTEGER(w2f__i8) OpenAD_Symbol_53
      INTEGER(w2f__i8) OpenAD_Symbol_54
      INTEGER(w2f__i8) OpenAD_Symbol_55
      INTEGER(w2f__i8) OpenAD_Symbol_56
      INTEGER(w2f__i8) OpenAD_Symbol_57
      INTEGER(w2f__i8) OpenAD_Symbol_58
      INTEGER(w2f__i8) OpenAD_Symbol_59
      INTEGER(w2f__i8) OpenAD_Symbol_60
      INTEGER(w2f__i8) OpenAD_Symbol_61
      INTEGER(w2f__i8) OpenAD_Symbol_62
      INTEGER(w2f__i8) OpenAD_Symbol_63
      INTEGER(w2f__i8) OpenAD_Symbol_64
      INTEGER(w2f__i8) OpenAD_Symbol_65
      INTEGER(w2f__i8) OpenAD_Symbol_66
      INTEGER(w2f__i8) OpenAD_Symbol_67
      INTEGER(w2f__i8) OpenAD_Symbol_68
      INTEGER(w2f__i8) OpenAD_Symbol_69
      INTEGER(w2f__i8) OpenAD_Symbol_70
      INTEGER(w2f__i8) OpenAD_Symbol_71
      INTEGER(w2f__i8) OpenAD_Symbol_72
      INTEGER(w2f__i8) OpenAD_Symbol_73
      INTEGER(w2f__i8) OpenAD_Symbol_74
      INTEGER(w2f__i8) OpenAD_Symbol_75
      INTEGER(w2f__i8) OpenAD_Symbol_76
      INTEGER(w2f__i8) OpenAD_Symbol_77
      INTEGER(w2f__i8) OpenAD_Symbol_78
      INTEGER(w2f__i8) OpenAD_Symbol_79
      INTEGER(w2f__i8) OpenAD_Symbol_80
      INTEGER(w2f__i8) OpenAD_Symbol_81
      INTEGER(w2f__i8) OpenAD_Symbol_82
      INTEGER(w2f__i8) OpenAD_Symbol_83
      INTEGER(w2f__i8) OpenAD_Symbol_84
      INTEGER(w2f__i8) OpenAD_Symbol_85
      INTEGER(w2f__i8) OpenAD_Symbol_86
      INTEGER(w2f__i8) OpenAD_Symbol_87
      INTEGER(w2f__i8) OpenAD_Symbol_88
      INTEGER(w2f__i8) OpenAD_Symbol_89
      INTEGER(w2f__i8) OpenAD_Symbol_90
      INTEGER(w2f__i8) OpenAD_Symbol_91
      INTEGER(w2f__i8) OpenAD_Symbol_92
      INTEGER(w2f__i8) OpenAD_Symbol_93
      INTEGER(w2f__i8) OpenAD_Symbol_94
      INTEGER(w2f__i8) OpenAD_Symbol_95
      INTEGER(w2f__i8) OpenAD_Symbol_96
      INTEGER(w2f__i8) OpenAD_Symbol_97
      INTEGER(w2f__i8) OpenAD_Symbol_98
      INTEGER(w2f__i8) OpenAD_Symbol_99
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) DELX
      REAL(w2f__8) DELY
      EXTERNAL determine_data_time
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      CHARACTER(4) t__14
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
C     open(20,file='data',form='formatted',status='old')
      t__14 = 'OLD '
      OPEN(UNIT = 20, FORM = 'FORMATTED', STATUS = t__14(1_w2f__i8 : 3)
     > , FILE = 'data')
      READ(20, *)
      READ(20, *) NT, NTSPINUP, DT, START_TIME, DT_DUMP
      READ(20, *)
      READ(20, *) RINI, F0, BETA
      READ(20, *)
      READ(20, *) XSTART, YSTART
      READ(20, *)
      READ(20, *) DELX, DELY
      READ(20, *)
      READ(20, *) XPERIODIC, YPERIODIC, SPHERICAL, CARTESIAN
      READ(20, *)
      READ(20, *) FULLIO, SUPPRESSIO
      READ(20, *)
      READ(20, *) INITIAL_GRAD, GRAD_CHECK, OPTIMIZE, CALC_HESS
      READ(20, *)
      READ(20, '(A80)') NCDATAFILE
      READ(20, *)
      READ(20, '(A80)') NCRESTARTFILE
      READ(20, *)
      READ(20, '(A80)') FOUTNAME
      READ(20, *)
      READ(20, '(A80)') RUNNAME
      READ(20, *)
      READ(20, *) WF_DEPTH, WF_ETA, WF_U, WF_V, WF_ZONAL_TRANSPORT,
     >  WF_LAPLDEPTH, WF_GRADDEPTH
      READ(20, *)
      READ(20, *) NSIM
      READ(20, *)
      READ(20, *) EPSG, DF1, DXMIN, NITER, IMPRES, MODE
      READ(20, *)
      READ(20, *) EPS_GRAD, PGTOL, FACTR, IPRINT
C     close(20)
      CLOSE(UNIT = 20)
      IF(CARTESIAN .AND. SPHERICAL) THEN
        WRITE(*, *) 'grid specification is ambiguous'
      ELSE
        IF(.NOT.(CARTESIAN .OR. SPHERICAL)) THEN
          WRITE(*, *) 'grid specification is ambiguous'
        ENDIF
      ENDIF
      IF(SPHERICAL .AND. YPERIODIC) THEN
        WRITE(*, *) 'spherical grid and periodic boundary conditions'
        WRITE(*, *) 'in latitude do not make sense'
      ENDIF
      IF(YPERIODIC .AND.(SPHERICAL .OR.(CARTESIAN .AND.(BETA .ne.
     >  0.0D00)))) THEN
        WRITE(*, *) 'yperiodic boundaries only make sense on an f-pla'
     >  // 'ne'
      ENDIF
      IF(SPHERICAL) THEN
        DO IY = 0, 21, 1
          Y(INT(IY)) = (YSTART +(((DELY / 6.371D+06) *(REAL(IY) +(
     > -5.0D-01))) / 1.74532925199432954744D-02))
        END DO
        DO IX = 0, 21, 1
          X(INT(IX)) = (XSTART +(((DELX / 6.371D+06) *(REAL(IX) +(
     > -5.0D-01))) / 1.74532925199432954744D-02))
        END DO
      ELSE
        IF(CARTESIAN) THEN
          DO IX = 0, 21, 1
            X(INT(IX)) = (DELX *(REAL(IX) +(-5.0D-01)))
          END DO
          DO IY = 0, 21, 1
            Y(INT(IY)) = (DELY *(REAL(IY) +(-5.0D-01)))
          END DO
        ENDIF
      ENDIF
      DO IX = 0, 20, 1
        DX(INT(IX)) = (X(IX + 1) - X(IX))
      END DO
      DO IY = 0, 20, 1
        DY(INT(IY)) = (Y(IY + 1) - Y(IY))
      END DO
      DO IY = 1, 20, 1
        IF(SPHERICAL) THEN
          RX(INT(IY)) = (COS((Y(IY) + Y(IY + 1)) * 5.0D-01 *
     >  1.74532925199432954744D-02) * 6.371D+06 *
     >  1.74532925199432954744D-02)
        ELSE
          IF(CARTESIAN) THEN
            RX(INT(IY)) = 1.0D00
          ENDIF
        ENDIF
      END DO
      IF(SPHERICAL) THEN
        RY = 1.11194926644558741827D+05
      ELSE
        IF(CARTESIAN) THEN
          RY = 1.0D00
        ENDIF
      ENDIF
      DO IY = 1, 21, 1
        IF(SPHERICAL) THEN
          HY(INT(IY)) = COS(Y(IY) * 1.74532925199432954744D-02)
        ELSE
          IF(CARTESIAN) THEN
            HY(INT(IY)) = 1.0D00
          ENDIF
        ENDIF
      END DO
      CALL determine_data_time(NCDATAFILE)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
C     open(20,file='data',form='formatted',status='old')
      t__14 = 'OLD '
      OPEN(UNIT = 20, FORM = 'FORMATTED', STATUS = t__14(1_w2f__i8 : 3)
     > , FILE = 'data')
      READ(20, *)
      READ(20, *) NT, NTSPINUP, DT, START_TIME, DT_DUMP
      READ(20, *)
      READ(20, *) RINI, F0, BETA
      READ(20, *)
      READ(20, *) XSTART, YSTART
      READ(20, *)
      READ(20, *) DELX, DELY
      READ(20, *)
      READ(20, *) XPERIODIC, YPERIODIC, SPHERICAL, CARTESIAN
      READ(20, *)
      READ(20, *) FULLIO, SUPPRESSIO
      READ(20, *)
      READ(20, *) INITIAL_GRAD, GRAD_CHECK, OPTIMIZE, CALC_HESS
      READ(20, *)
      READ(20, '(A80)') NCDATAFILE
      READ(20, *)
      READ(20, '(A80)') NCRESTARTFILE
      READ(20, *)
      READ(20, '(A80)') FOUTNAME
      READ(20, *)
      READ(20, '(A80)') RUNNAME
      READ(20, *)
      READ(20, *) WF_DEPTH, WF_ETA, WF_U, WF_V, WF_ZONAL_TRANSPORT,
     >  WF_LAPLDEPTH, WF_GRADDEPTH
      READ(20, *)
      READ(20, *) NSIM
      READ(20, *)
      READ(20, *) EPSG, DF1, DXMIN, NITER, IMPRES, MODE
      READ(20, *)
      READ(20, *) EPS_GRAD, PGTOL, FACTR, IPRINT
C     close(20)
      CLOSE(UNIT = 20)
      IF(CARTESIAN .AND. SPHERICAL) THEN
        WRITE(*, *) 'grid specification is ambiguous'
        OpenAD_Symbol_66 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_66)
      ELSE
        IF(.NOT.(CARTESIAN .OR. SPHERICAL)) THEN
          WRITE(*, *) 'grid specification is ambiguous'
          OpenAD_Symbol_64 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_64)
        ELSE
          OpenAD_Symbol_65 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_65)
        ENDIF
        OpenAD_Symbol_67 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_67)
      ENDIF
      IF(SPHERICAL .AND. YPERIODIC) THEN
        WRITE(*, *) 'spherical grid and periodic boundary conditions'
        WRITE(*, *) 'in latitude do not make sense'
        OpenAD_Symbol_68 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_68)
      ELSE
        OpenAD_Symbol_69 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_69)
      ENDIF
      IF(YPERIODIC .AND.(SPHERICAL .OR.(CARTESIAN .AND.(BETA .ne.
     >  0.0D00)))) THEN
        WRITE(*, *) 'yperiodic boundaries only make sense on an f-pla'
     >  // 'ne'
        OpenAD_Symbol_70 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_70)
      ELSE
        OpenAD_Symbol_71 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_71)
      ENDIF
      IF(SPHERICAL) THEN
        OpenAD_Symbol_72 = 0_w2f__i8
        DO IY = 0, 21, 1
          Y(INT(IY)) = (YSTART +(((DELY / 6.371D+06) *(REAL(IY) +(
     > -5.0D-01))) / 1.74532925199432954744D-02))
          OpenAD_Symbol_72 = (INT(OpenAD_Symbol_72) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_72)
        OpenAD_Symbol_73 = 0_w2f__i8
        DO IX = 0, 21, 1
          X(INT(IX)) = (XSTART +(((DELX / 6.371D+06) *(REAL(IX) +(
     > -5.0D-01))) / 1.74532925199432954744D-02))
          OpenAD_Symbol_73 = (INT(OpenAD_Symbol_73) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_73)
        OpenAD_Symbol_78 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_78)
      ELSE
        IF(CARTESIAN) THEN
          OpenAD_Symbol_74 = 0_w2f__i8
          DO IX = 0, 21, 1
            X(INT(IX)) = (DELX *(REAL(IX) +(-5.0D-01)))
            OpenAD_Symbol_74 = (INT(OpenAD_Symbol_74) + INT(1_w2f__i8))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_74)
          OpenAD_Symbol_75 = 0_w2f__i8
          DO IY = 0, 21, 1
            Y(INT(IY)) = (DELY *(REAL(IY) +(-5.0D-01)))
            OpenAD_Symbol_75 = (INT(OpenAD_Symbol_75) + INT(1_w2f__i8))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_75)
          OpenAD_Symbol_77 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_77)
        ELSE
          OpenAD_Symbol_76 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_76)
        ENDIF
        OpenAD_Symbol_79 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_79)
      ENDIF
      OpenAD_Symbol_80 = 0_w2f__i8
      DO IX = 0, 20, 1
        DX(INT(IX)) = (X(IX + 1) - X(IX))
        OpenAD_Symbol_80 = (INT(OpenAD_Symbol_80) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_80)
      OpenAD_Symbol_81 = 0_w2f__i8
      DO IY = 0, 20, 1
        DY(INT(IY)) = (Y(IY + 1) - Y(IY))
        OpenAD_Symbol_81 = (INT(OpenAD_Symbol_81) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_81)
      OpenAD_Symbol_82 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(SPHERICAL) THEN
          RX(INT(IY)) = (COS((Y(IY) + Y(IY + 1)) * 5.0D-01 *
     >  1.74532925199432954744D-02) * 6.371D+06 *
     >  1.74532925199432954744D-02)
          OpenAD_Symbol_85 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_85)
        ELSE
          IF(CARTESIAN) THEN
            RX(INT(IY)) = 1.0D00
            OpenAD_Symbol_83 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_83)
          ELSE
            OpenAD_Symbol_84 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_84)
          ENDIF
          OpenAD_Symbol_86 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_86)
        ENDIF
        OpenAD_Symbol_82 = (INT(OpenAD_Symbol_82) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_82)
      IF(SPHERICAL) THEN
        RY = 1.11194926644558741827D+05
        OpenAD_Symbol_89 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_89)
      ELSE
        IF(CARTESIAN) THEN
          RY = 1.0D00
          OpenAD_Symbol_87 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_87)
        ELSE
          OpenAD_Symbol_88 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_88)
        ENDIF
        OpenAD_Symbol_90 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_90)
      ENDIF
      OpenAD_Symbol_91 = 0_w2f__i8
      DO IY = 1, 21, 1
        IF(SPHERICAL) THEN
          HY(INT(IY)) = COS(Y(IY) * 1.74532925199432954744D-02)
          OpenAD_Symbol_94 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_94)
        ELSE
          IF(CARTESIAN) THEN
            HY(INT(IY)) = 1.0D00
            OpenAD_Symbol_92 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_92)
          ELSE
            OpenAD_Symbol_93 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_93)
          ENDIF
          OpenAD_Symbol_95 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_95)
        ENDIF
        OpenAD_Symbol_91 = (INT(OpenAD_Symbol_91) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_91)
      CALL determine_data_time(NCDATAFILE)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      CALL determine_data_time(NCDATAFILE)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_36)
      OpenAD_Symbol_37 = 1
      DO WHILE(INT(OpenAD_Symbol_37) .LE. INT(OpenAD_Symbol_36))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_38)
        IF(OpenAD_Symbol_38 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_39)
          IF(OpenAD_Symbol_39 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_37 = INT(OpenAD_Symbol_37) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_40)
      IF(OpenAD_Symbol_40 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_41)
        IF(OpenAD_Symbol_41 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_42)
      OpenAD_Symbol_43 = 1
      DO WHILE(INT(OpenAD_Symbol_43) .LE. INT(OpenAD_Symbol_42))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_44)
        IF(OpenAD_Symbol_44 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_45)
          IF(OpenAD_Symbol_45 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_43 = INT(OpenAD_Symbol_43) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_46)
      OpenAD_Symbol_47 = 1
      DO WHILE(INT(OpenAD_Symbol_47) .LE. INT(OpenAD_Symbol_46))
        OpenAD_Symbol_47 = INT(OpenAD_Symbol_47) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_48)
      OpenAD_Symbol_49 = 1
      DO WHILE(INT(OpenAD_Symbol_49) .LE. INT(OpenAD_Symbol_48))
        OpenAD_Symbol_49 = INT(OpenAD_Symbol_49) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_50)
      IF(OpenAD_Symbol_50 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_56)
        OpenAD_Symbol_57 = 1
        DO WHILE(INT(OpenAD_Symbol_57) .LE. INT(OpenAD_Symbol_56))
          OpenAD_Symbol_57 = INT(OpenAD_Symbol_57) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_58)
        OpenAD_Symbol_59 = 1
        DO WHILE(INT(OpenAD_Symbol_59) .LE. INT(OpenAD_Symbol_58))
          OpenAD_Symbol_59 = INT(OpenAD_Symbol_59) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_51)
        IF(OpenAD_Symbol_51 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_52)
          OpenAD_Symbol_53 = 1
          DO WHILE(INT(OpenAD_Symbol_53) .LE. INT(OpenAD_Symbol_52))
            OpenAD_Symbol_53 = INT(OpenAD_Symbol_53) + 1
          END DO
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_54)
          OpenAD_Symbol_55 = 1
          DO WHILE(INT(OpenAD_Symbol_55) .LE. INT(OpenAD_Symbol_54))
            OpenAD_Symbol_55 = INT(OpenAD_Symbol_55) + 1
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_60)
      IF(OpenAD_Symbol_60 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_61)
      IF(OpenAD_Symbol_61 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_62)
      IF(OpenAD_Symbol_62 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_63)
        IF(OpenAD_Symbol_63 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(BETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(YSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(XSTART)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
C     open(20,file='data',form='formatted',status='old')
      t__14 = 'OLD '
      OPEN(UNIT = 20, FORM = 'FORMATTED', STATUS = t__14(1_w2f__i8 : 3)
     > , FILE = 'data')
      READ(20, *)
      READ(20, *) NT, NTSPINUP, DT, START_TIME, DT_DUMP
      READ(20, *)
      READ(20, *) RINI, F0, BETA
      READ(20, *)
      READ(20, *) XSTART, YSTART
      READ(20, *)
      READ(20, *) DELX, DELY
      READ(20, *)
      READ(20, *) XPERIODIC, YPERIODIC, SPHERICAL, CARTESIAN
      READ(20, *)
      READ(20, *) FULLIO, SUPPRESSIO
      READ(20, *)
      READ(20, *) INITIAL_GRAD, GRAD_CHECK, OPTIMIZE, CALC_HESS
      READ(20, *)
      READ(20, '(A80)') NCDATAFILE
      READ(20, *)
      READ(20, '(A80)') NCRESTARTFILE
      READ(20, *)
      READ(20, '(A80)') FOUTNAME
      READ(20, *)
      READ(20, '(A80)') RUNNAME
      READ(20, *)
      READ(20, *) WF_DEPTH, WF_ETA, WF_U, WF_V, WF_ZONAL_TRANSPORT,
     >  WF_LAPLDEPTH, WF_GRADDEPTH
      READ(20, *)
      READ(20, *) NSIM
      READ(20, *)
      READ(20, *) EPSG, DF1, DXMIN, NITER, IMPRES, MODE
      READ(20, *)
      READ(20, *) EPS_GRAD, PGTOL, FACTR, IPRINT
C     close(20)
      CLOSE(UNIT = 20)
      IF(CARTESIAN .AND. SPHERICAL) THEN
        WRITE(*, *) 'grid specification is ambiguous'
        OpenAD_Symbol_126 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_126)
      ELSE
        IF(.NOT.(CARTESIAN .OR. SPHERICAL)) THEN
          WRITE(*, *) 'grid specification is ambiguous'
          OpenAD_Symbol_124 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_124)
        ELSE
          OpenAD_Symbol_125 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_125)
        ENDIF
        OpenAD_Symbol_127 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_127)
      ENDIF
      IF(SPHERICAL .AND. YPERIODIC) THEN
        WRITE(*, *) 'spherical grid and periodic boundary conditions'
        WRITE(*, *) 'in latitude do not make sense'
        OpenAD_Symbol_128 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_128)
      ELSE
        OpenAD_Symbol_129 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_129)
      ENDIF
      IF(YPERIODIC .AND.(SPHERICAL .OR.(CARTESIAN .AND.(BETA .ne.
     >  0.0D00)))) THEN
        WRITE(*, *) 'yperiodic boundaries only make sense on an f-pla'
     >  // 'ne'
        OpenAD_Symbol_130 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_130)
      ELSE
        OpenAD_Symbol_131 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_131)
      ENDIF
      IF(SPHERICAL) THEN
        OpenAD_Symbol_132 = 0_w2f__i8
        DO IY = 0, 21, 1
          Y(INT(IY)) = (YSTART +(((DELY / 6.371D+06) *(REAL(IY) +(
     > -5.0D-01))) / 1.74532925199432954744D-02))
          OpenAD_Symbol_132 = (INT(OpenAD_Symbol_132) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_132)
        OpenAD_Symbol_133 = 0_w2f__i8
        DO IX = 0, 21, 1
          X(INT(IX)) = (XSTART +(((DELX / 6.371D+06) *(REAL(IX) +(
     > -5.0D-01))) / 1.74532925199432954744D-02))
          OpenAD_Symbol_133 = (INT(OpenAD_Symbol_133) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_133)
        OpenAD_Symbol_138 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_138)
      ELSE
        IF(CARTESIAN) THEN
          OpenAD_Symbol_134 = 0_w2f__i8
          DO IX = 0, 21, 1
            X(INT(IX)) = (DELX *(REAL(IX) +(-5.0D-01)))
            OpenAD_Symbol_134 = (INT(OpenAD_Symbol_134) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_134)
          OpenAD_Symbol_135 = 0_w2f__i8
          DO IY = 0, 21, 1
            Y(INT(IY)) = (DELY *(REAL(IY) +(-5.0D-01)))
            OpenAD_Symbol_135 = (INT(OpenAD_Symbol_135) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_135)
          OpenAD_Symbol_137 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_137)
        ELSE
          OpenAD_Symbol_136 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_136)
        ENDIF
        OpenAD_Symbol_139 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_139)
      ENDIF
      OpenAD_Symbol_140 = 0_w2f__i8
      DO IX = 0, 20, 1
        DX(INT(IX)) = (X(IX + 1) - X(IX))
        OpenAD_Symbol_140 = (INT(OpenAD_Symbol_140) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_140)
      OpenAD_Symbol_141 = 0_w2f__i8
      DO IY = 0, 20, 1
        DY(INT(IY)) = (Y(IY + 1) - Y(IY))
        OpenAD_Symbol_141 = (INT(OpenAD_Symbol_141) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_141)
      OpenAD_Symbol_142 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(SPHERICAL) THEN
          RX(INT(IY)) = (COS((Y(IY) + Y(IY + 1)) * 5.0D-01 *
     >  1.74532925199432954744D-02) * 6.371D+06 *
     >  1.74532925199432954744D-02)
          OpenAD_Symbol_145 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_145)
        ELSE
          IF(CARTESIAN) THEN
            RX(INT(IY)) = 1.0D00
            OpenAD_Symbol_143 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_143)
          ELSE
            OpenAD_Symbol_144 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_144)
          ENDIF
          OpenAD_Symbol_146 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_146)
        ENDIF
        OpenAD_Symbol_142 = (INT(OpenAD_Symbol_142) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_142)
      IF(SPHERICAL) THEN
        RY = 1.11194926644558741827D+05
        OpenAD_Symbol_149 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_149)
      ELSE
        IF(CARTESIAN) THEN
          RY = 1.0D00
          OpenAD_Symbol_147 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_147)
        ELSE
          OpenAD_Symbol_148 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_148)
        ENDIF
        OpenAD_Symbol_150 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_150)
      ENDIF
      OpenAD_Symbol_151 = 0_w2f__i8
      DO IY = 1, 21, 1
        IF(SPHERICAL) THEN
          HY(INT(IY)) = COS(Y(IY) * 1.74532925199432954744D-02)
          OpenAD_Symbol_154 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_154)
        ELSE
          IF(CARTESIAN) THEN
            HY(INT(IY)) = 1.0D00
            OpenAD_Symbol_152 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_152)
          ELSE
            OpenAD_Symbol_153 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_153)
          ENDIF
          OpenAD_Symbol_155 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_155)
        ENDIF
        OpenAD_Symbol_151 = (INT(OpenAD_Symbol_151) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_151)
      CALL determine_data_time(NCDATAFILE)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
      CALL determine_data_time(NCDATAFILE)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_96)
      OpenAD_Symbol_97 = 1
      DO WHILE(INT(OpenAD_Symbol_97) .LE. INT(OpenAD_Symbol_96))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_98)
        IF(OpenAD_Symbol_98 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_99)
          IF(OpenAD_Symbol_99 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_97 = INT(OpenAD_Symbol_97) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_100)
      IF(OpenAD_Symbol_100 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_101)
        IF(OpenAD_Symbol_101 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_102)
      OpenAD_Symbol_103 = 1
      DO WHILE(INT(OpenAD_Symbol_103) .LE. INT(OpenAD_Symbol_102))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_104)
        IF(OpenAD_Symbol_104 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_105)
          IF(OpenAD_Symbol_105 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_103 = INT(OpenAD_Symbol_103) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_106)
      OpenAD_Symbol_107 = 1
      DO WHILE(INT(OpenAD_Symbol_107) .LE. INT(OpenAD_Symbol_106))
        OpenAD_Symbol_107 = INT(OpenAD_Symbol_107) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_108)
      OpenAD_Symbol_109 = 1
      DO WHILE(INT(OpenAD_Symbol_109) .LE. INT(OpenAD_Symbol_108))
        OpenAD_Symbol_109 = INT(OpenAD_Symbol_109) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_110)
      IF(OpenAD_Symbol_110 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_116)
        OpenAD_Symbol_117 = 1
        DO WHILE(INT(OpenAD_Symbol_117) .LE. INT(OpenAD_Symbol_116))
          OpenAD_Symbol_117 = INT(OpenAD_Symbol_117) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_118)
        OpenAD_Symbol_119 = 1
        DO WHILE(INT(OpenAD_Symbol_119) .LE. INT(OpenAD_Symbol_118))
          OpenAD_Symbol_119 = INT(OpenAD_Symbol_119) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_111)
        IF(OpenAD_Symbol_111 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_112)
          OpenAD_Symbol_113 = 1
          DO WHILE(INT(OpenAD_Symbol_113) .LE. INT(OpenAD_Symbol_112))
            OpenAD_Symbol_113 = INT(OpenAD_Symbol_113) + 1
          END DO
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_114)
          OpenAD_Symbol_115 = 1
          DO WHILE(INT(OpenAD_Symbol_115) .LE. INT(OpenAD_Symbol_114))
            OpenAD_Symbol_115 = INT(OpenAD_Symbol_115) + 1
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_120)
      IF(OpenAD_Symbol_120 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_121)
      IF(OpenAD_Symbol_121 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_122)
      IF(OpenAD_Symbol_122 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_123)
        IF(OpenAD_Symbol_123 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE boundary_conditions(NX, NY, FIELD, XPERIODIC,
     >  YPERIODIC)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_444
      INTEGER(w2f__i8) OpenAD_Symbol_445
      INTEGER(w2f__i8) OpenAD_Symbol_446
      INTEGER(w2f__i8) OpenAD_Symbol_447
      INTEGER(w2f__i8) OpenAD_Symbol_448
      INTEGER(w2f__i8) OpenAD_Symbol_449
      INTEGER(w2f__i8) OpenAD_Symbol_450
      INTEGER(w2f__i8) OpenAD_Symbol_451
      INTEGER(w2f__i8) OpenAD_Symbol_452
      INTEGER(w2f__i8) OpenAD_Symbol_453
      INTEGER(w2f__i8) OpenAD_Symbol_454
      INTEGER(w2f__i8) OpenAD_Symbol_455
      INTEGER(w2f__i8) OpenAD_Symbol_456
      INTEGER(w2f__i8) OpenAD_Symbol_457
      INTEGER(w2f__i8) OpenAD_Symbol_458
      INTEGER(w2f__i8) OpenAD_Symbol_459
      INTEGER(w2f__i8) OpenAD_Symbol_460
      INTEGER(w2f__i8) OpenAD_Symbol_461
      INTEGER(w2f__i8) OpenAD_Symbol_462
      INTEGER(w2f__i8) OpenAD_Symbol_463
      INTEGER(w2f__i8) OpenAD_Symbol_464
      INTEGER(w2f__i8) OpenAD_Symbol_465
      INTEGER(w2f__i8) OpenAD_Symbol_466
      INTEGER(w2f__i8) OpenAD_Symbol_467
      INTEGER(w2f__i8) OpenAD_Symbol_468
      INTEGER(w2f__i8) OpenAD_Symbol_469
      INTEGER(w2f__i8) OpenAD_Symbol_470
      INTEGER(w2f__i8) OpenAD_Symbol_471
      INTEGER(w2f__i8) OpenAD_Symbol_472
      INTEGER(w2f__i8) OpenAD_Symbol_473
      INTEGER(w2f__i8) OpenAD_Symbol_474
      INTEGER(w2f__i8) OpenAD_Symbol_475
      INTEGER(w2f__i8) OpenAD_Symbol_476
      INTEGER(w2f__i8) OpenAD_Symbol_477
      INTEGER(w2f__i8) OpenAD_Symbol_478
      INTEGER(w2f__i8) OpenAD_Symbol_479
      INTEGER(w2f__i8) OpenAD_Symbol_480
      INTEGER(w2f__i8) OpenAD_Symbol_481
      INTEGER(w2f__i8) OpenAD_Symbol_482
      INTEGER(w2f__i8) OpenAD_Symbol_483
      INTEGER(w2f__i8) OpenAD_Symbol_484
      INTEGER(w2f__i8) OpenAD_Symbol_485
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) NX
      INTEGER(w2f__i4) NY
      REAL(w2f__8) FIELD(0 : INT((NX + 1)), 0 : INT((NY + 1)))
      LOGICAL(w2f__i4) XPERIODIC
      LOGICAL(w2f__i4) YPERIODIC
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(XPERIODIC) THEN
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = FIELD(NX, IY)
          FIELD(NX + 1, INT(IY)) = FIELD(1, IY)
        END DO
      ELSE
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = 0.0D00
          FIELD(NX + 1, INT(IY)) = 0.0D00
        END DO
      ENDIF
      IF(YPERIODIC) THEN
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = FIELD(IX, NY)
          FIELD(INT(IX), NY + 1) = FIELD(IX, 1)
        END DO
      ELSE
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = 0.0D00
          FIELD(INT(IX), NY + 1) = 0.0D00
        END DO
      ENDIF
      IF(XPERIODIC .AND. YPERIODIC) THEN
        WRITE(*, *) 'boundary_conditions: ',
     >  'make sure that the corners are handled correctly'
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(XPERIODIC) THEN
        OpenAD_Symbol_455 = 0_w2f__i8
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = FIELD(NX, IY)
          FIELD(NX + 1, INT(IY)) = FIELD(1, IY)
          OpenAD_Symbol_455 = (INT(OpenAD_Symbol_455) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_455)
        OpenAD_Symbol_457 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_457)
      ELSE
        OpenAD_Symbol_456 = 0_w2f__i8
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = 0.0D00
          FIELD(NX + 1, INT(IY)) = 0.0D00
          OpenAD_Symbol_456 = (INT(OpenAD_Symbol_456) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_456)
        OpenAD_Symbol_458 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_458)
      ENDIF
      IF(YPERIODIC) THEN
        OpenAD_Symbol_459 = 0_w2f__i8
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = FIELD(IX, NY)
          FIELD(INT(IX), NY + 1) = FIELD(IX, 1)
          OpenAD_Symbol_459 = (INT(OpenAD_Symbol_459) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_459)
        OpenAD_Symbol_461 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_461)
      ELSE
        OpenAD_Symbol_460 = 0_w2f__i8
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = 0.0D00
          FIELD(INT(IX), NY + 1) = 0.0D00
          OpenAD_Symbol_460 = (INT(OpenAD_Symbol_460) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_460)
        OpenAD_Symbol_462 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_462)
      ENDIF
      IF(XPERIODIC .AND. YPERIODIC) THEN
        WRITE(*, *) 'boundary_conditions: ',
     >  'make sure that the corners are handled correctly'
        OpenAD_Symbol_463 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_463)
      ELSE
        OpenAD_Symbol_464 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_464)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_444)
      IF(OpenAD_Symbol_444 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_445)
      IF(OpenAD_Symbol_445 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_448)
        OpenAD_Symbol_449 = 1
        DO WHILE(INT(OpenAD_Symbol_449) .LE. INT(OpenAD_Symbol_448))
          OpenAD_Symbol_449 = INT(OpenAD_Symbol_449) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_446)
        OpenAD_Symbol_447 = 1
        DO WHILE(INT(OpenAD_Symbol_447) .LE. INT(OpenAD_Symbol_446))
          OpenAD_Symbol_447 = INT(OpenAD_Symbol_447) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_450)
      IF(OpenAD_Symbol_450 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_453)
        OpenAD_Symbol_454 = 1
        DO WHILE(INT(OpenAD_Symbol_454) .LE. INT(OpenAD_Symbol_453))
          OpenAD_Symbol_454 = INT(OpenAD_Symbol_454) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_451)
        OpenAD_Symbol_452 = 1
        DO WHILE(INT(OpenAD_Symbol_452) .LE. INT(OpenAD_Symbol_451))
          OpenAD_Symbol_452 = INT(OpenAD_Symbol_452) + 1
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FIELD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FIELD)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NX)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FIELD)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FIELD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FIELD)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FIELD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(XPERIODIC) THEN
        OpenAD_Symbol_476 = 0_w2f__i8
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = FIELD(NX, IY)
          FIELD(NX + 1, INT(IY)) = FIELD(1, IY)
          OpenAD_Symbol_476 = (INT(OpenAD_Symbol_476) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_476)
        OpenAD_Symbol_478 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_478)
      ELSE
        OpenAD_Symbol_477 = 0_w2f__i8
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = 0.0D00
          FIELD(NX + 1, INT(IY)) = 0.0D00
          OpenAD_Symbol_477 = (INT(OpenAD_Symbol_477) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_477)
        OpenAD_Symbol_479 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_479)
      ENDIF
      IF(YPERIODIC) THEN
        OpenAD_Symbol_480 = 0_w2f__i8
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = FIELD(IX, NY)
          FIELD(INT(IX), NY + 1) = FIELD(IX, 1)
          OpenAD_Symbol_480 = (INT(OpenAD_Symbol_480) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_480)
        OpenAD_Symbol_482 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_482)
      ELSE
        OpenAD_Symbol_481 = 0_w2f__i8
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = 0.0D00
          FIELD(INT(IX), NY + 1) = 0.0D00
          OpenAD_Symbol_481 = (INT(OpenAD_Symbol_481) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_481)
        OpenAD_Symbol_483 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_483)
      ENDIF
      IF(XPERIODIC .AND. YPERIODIC) THEN
        WRITE(*, *) 'boundary_conditions: ',
     >  'make sure that the corners are handled correctly'
        OpenAD_Symbol_484 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_484)
      ELSE
        OpenAD_Symbol_485 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_485)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_465)
      IF(OpenAD_Symbol_465 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_466)
      IF(OpenAD_Symbol_466 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_469)
        OpenAD_Symbol_470 = 1
        DO WHILE(INT(OpenAD_Symbol_470) .LE. INT(OpenAD_Symbol_469))
          OpenAD_Symbol_470 = INT(OpenAD_Symbol_470) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_467)
        OpenAD_Symbol_468 = 1
        DO WHILE(INT(OpenAD_Symbol_468) .LE. INT(OpenAD_Symbol_467))
          OpenAD_Symbol_468 = INT(OpenAD_Symbol_468) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_471)
      IF(OpenAD_Symbol_471 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_474)
        OpenAD_Symbol_475 = 1
        DO WHILE(INT(OpenAD_Symbol_475) .LE. INT(OpenAD_Symbol_474))
          OpenAD_Symbol_475 = INT(OpenAD_Symbol_475) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_472)
        OpenAD_Symbol_473 = 1
        DO WHILE(INT(OpenAD_Symbol_473) .LE. INT(OpenAD_Symbol_472))
          OpenAD_Symbol_473 = INT(OpenAD_Symbol_473) + 1
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_data_fields()
      use w2f__types
      use size
      use parms
      use pfields
      use force
      use size
      use parms
      use pfields
      use force
      use size
      use parms
      use pfields
      use force
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_156
      INTEGER(w2f__i8) OpenAD_Symbol_157
      INTEGER(w2f__i8) OpenAD_Symbol_158
      INTEGER(w2f__i8) OpenAD_Symbol_159
      INTEGER(w2f__i8) OpenAD_Symbol_160
      INTEGER(w2f__i8) OpenAD_Symbol_161
      INTEGER(w2f__i8) OpenAD_Symbol_162
      INTEGER(w2f__i8) OpenAD_Symbol_163
      INTEGER(w2f__i8) OpenAD_Symbol_164
      INTEGER(w2f__i8) OpenAD_Symbol_165
      INTEGER(w2f__i8) OpenAD_Symbol_166
      INTEGER(w2f__i8) OpenAD_Symbol_167
      INTEGER(w2f__i8) OpenAD_Symbol_168
      INTEGER(w2f__i8) OpenAD_Symbol_169
      INTEGER(w2f__i8) OpenAD_Symbol_170
      INTEGER(w2f__i8) OpenAD_Symbol_171
      INTEGER(w2f__i8) OpenAD_Symbol_172
      INTEGER(w2f__i8) OpenAD_Symbol_173
      INTEGER(w2f__i8) OpenAD_Symbol_174
      INTEGER(w2f__i8) OpenAD_Symbol_175
      INTEGER(w2f__i8) OpenAD_Symbol_176
      INTEGER(w2f__i8) OpenAD_Symbol_177
      INTEGER(w2f__i8) OpenAD_Symbol_178
      INTEGER(w2f__i8) OpenAD_Symbol_179
      INTEGER(w2f__i8) OpenAD_Symbol_180
      INTEGER(w2f__i8) OpenAD_Symbol_181
      INTEGER(w2f__i8) OpenAD_Symbol_182
      INTEGER(w2f__i8) OpenAD_Symbol_183
      INTEGER(w2f__i8) OpenAD_Symbol_184
      INTEGER(w2f__i8) OpenAD_Symbol_185
      INTEGER(w2f__i8) OpenAD_Symbol_186
      INTEGER(w2f__i8) OpenAD_Symbol_187
      INTEGER(w2f__i8) OpenAD_Symbol_188
      INTEGER(w2f__i8) OpenAD_Symbol_189
      INTEGER(w2f__i8) OpenAD_Symbol_190
      INTEGER(w2f__i8) OpenAD_Symbol_191
      INTEGER(w2f__i8) OpenAD_Symbol_192
      INTEGER(w2f__i8) OpenAD_Symbol_193
      INTEGER(w2f__i8) OpenAD_Symbol_194
      INTEGER(w2f__i8) OpenAD_Symbol_195
      INTEGER(w2f__i8) OpenAD_Symbol_196
      INTEGER(w2f__i8) OpenAD_Symbol_197
      INTEGER(w2f__i8) OpenAD_Symbol_198
      INTEGER(w2f__i8) OpenAD_Symbol_199
      INTEGER(w2f__i8) OpenAD_Symbol_200
      INTEGER(w2f__i8) OpenAD_Symbol_201
      INTEGER(w2f__i8) OpenAD_Symbol_202
      INTEGER(w2f__i8) OpenAD_Symbol_203
C
C     **** Local Variables and Functions ****
C
      EXTERNAL boundary_conditions
      LOGICAL(w2f__i4) EXISTS
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      REAL(w2f__8) MYTIME
      EXTERNAL read_extended_field
      EXTERNAL read_field
      CHARACTER(80) STR1
      CHARACTER(80) STR2
      CHARACTER(80) STR3
      REAL(w2f__8) X_IN(1 : 20)
      REAL(w2f__8) Y_IN(1 : 20)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYTIME = (-1.0D00)
      DO IX = 1, 20, 1
        X_IN(INT(IX)) = 0.0D00
      END DO
      DO IY = 1, 20, 1
        Y_IN(INT(IY)) = 0.0D00
      END DO
C     inquire(file=ncdatafile,exist=exists)
      INQUIRE(EXIST = EXISTS, FILE = NCDATAFILE)
      IF(.NOT. EXISTS) THEN
        WRITE(*, *) NCDATAFILE, ' not found, cannot continue'
      ELSE
        STR1 = 'depth'
        STR2 = 'frict'
        CALL read_field(NCDATAFILE, MYTIME, STR1, INIDEPTH)
        CALL read_extended_field(NCDATAFILE, STR2, FRICT)
        IF(RINI .ne. 0.0D00) THEN
          WRITE(*, *) 'rini = ', RINI
          WRITE(*, *) 'will overwrite frict with rini'
          DO IX = 1, 20, 1
            DO IY = 1, 20, 1
              FRICT(INT(IX), INT(IY)) = RINI
            END DO
          END DO
        ENDIF
        CALL boundary_conditions(MYNX, MYNY, FRICT, XPERIODIC,
     >  YPERIODIC)
        STR1 = 'uforce'
        STR2 = 'vforce'
        CALL read_field(NCDATAFILE, MYTIME, STR1, UFORCE)
        CALL read_field(NCDATAFILE, MYTIME, STR2, VFORCE)
        IF(START_TIME .eq. 0.0D00) THEN
          WRITE(*, *) 'cold start from initial fields'
          STR1 = 'uini'
          STR2 = 'vini'
          STR3 = 'etaini'
          CALL read_field(NCDATAFILE, START_TIME, STR1, UINI)
          CALL read_field(NCDATAFILE, START_TIME, STR2, VINI)
          CALL read_field(NCDATAFILE, START_TIME, STR3, ETAINI)
        ELSE
          WRITE(*, *) 'warm restart from time ', START_TIME
          WRITE(*, *) 'in restart file ', NCRESTARTFILE
C         inquire( file = ncrestartfile, exist = exists ) 
          INQUIRE(EXIST = EXISTS, FILE = NCRESTARTFILE)
          IF(.NOT. EXISTS) THEN
            WRITE(*, *) NCRESTARTFILE, ' not found'
          ELSE
            STR1 = 'U'
            STR2 = 'V'
            STR3 = 'ETA'
            CALL read_field(NCRESTARTFILE, START_TIME, STR1, UINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR2, VINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR3, ETAINI)
          ENDIF
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYTIME = (-1.0D00)
      OpenAD_Symbol_168 = 0_w2f__i8
      DO IX = 1, 20, 1
        X_IN(INT(IX)) = 0.0D00
        OpenAD_Symbol_168 = (INT(OpenAD_Symbol_168) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_168)
      OpenAD_Symbol_169 = 0_w2f__i8
      DO IY = 1, 20, 1
        Y_IN(INT(IY)) = 0.0D00
        OpenAD_Symbol_169 = (INT(OpenAD_Symbol_169) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_169)
C     inquire(file=ncdatafile,exist=exists)
      INQUIRE(EXIST = EXISTS, FILE = NCDATAFILE)
      IF(.NOT. EXISTS) THEN
        WRITE(*, *) NCDATAFILE, ' not found, cannot continue'
        OpenAD_Symbol_178 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_178)
      ELSE
        STR1 = 'depth'
        STR2 = 'frict'
        CALL read_field(NCDATAFILE, MYTIME, STR1, INIDEPTH)
        CALL read_extended_field(NCDATAFILE, STR2, FRICT)
        IF(RINI .ne. 0.0D00) THEN
          WRITE(*, *) 'rini = ', RINI
          WRITE(*, *) 'will overwrite frict with rini'
          OpenAD_Symbol_170 = 0_w2f__i8
          DO IX = 1, 20, 1
            OpenAD_Symbol_171 = 0_w2f__i8
            DO IY = 1, 20, 1
              FRICT(INT(IX), INT(IY)) = RINI
              OpenAD_Symbol_171 = (INT(OpenAD_Symbol_171) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_171)
            OpenAD_Symbol_170 = (INT(OpenAD_Symbol_170) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_170)
          OpenAD_Symbol_173 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_173)
        ELSE
          OpenAD_Symbol_172 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_172)
        ENDIF
        CALL boundary_conditions(MYNX, MYNY, FRICT, XPERIODIC,
     >  YPERIODIC)
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(MYNX)
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(MYNY)
        STR1 = 'uforce'
        STR2 = 'vforce'
        CALL read_field(NCDATAFILE, MYTIME, STR1, UFORCE)
        CALL read_field(NCDATAFILE, MYTIME, STR2, VFORCE)
        IF(START_TIME .eq. 0.0D00) THEN
          WRITE(*, *) 'cold start from initial fields'
          STR1 = 'uini'
          STR2 = 'vini'
          STR3 = 'etaini'
          CALL read_field(NCDATAFILE, START_TIME, STR1, UINI)
          CALL read_field(NCDATAFILE, START_TIME, STR2, VINI)
          CALL read_field(NCDATAFILE, START_TIME, STR3, ETAINI)
          OpenAD_Symbol_176 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_176)
        ELSE
          WRITE(*, *) 'warm restart from time ', START_TIME
          WRITE(*, *) 'in restart file ', NCRESTARTFILE
C         inquire( file = ncrestartfile, exist = exists ) 
          INQUIRE(EXIST = EXISTS, FILE = NCRESTARTFILE)
          IF(.NOT. EXISTS) THEN
            WRITE(*, *) NCRESTARTFILE, ' not found'
            OpenAD_Symbol_174 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_174)
          ELSE
            STR1 = 'U'
            STR2 = 'V'
            STR3 = 'ETA'
            CALL read_field(NCRESTARTFILE, START_TIME, STR1, UINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR2, VINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR3, ETAINI)
            OpenAD_Symbol_175 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_175)
          ENDIF
          OpenAD_Symbol_177 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_177)
        ENDIF
        OpenAD_Symbol_179 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_179)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_156)
      IF(OpenAD_Symbol_156 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_157)
        IF(OpenAD_Symbol_157 .ne. 0) THEN
          CALL read_field(NCDATAFILE, START_TIME, STR3, ETAINI)
          CALL read_field(NCDATAFILE, START_TIME, STR2, VINI)
          CALL read_field(NCDATAFILE, START_TIME, STR1, UINI)
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_158)
          IF(OpenAD_Symbol_158 .ne. 0) THEN
          ELSE
            CALL read_field(NCRESTARTFILE, START_TIME, STR3, ETAINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR2, VINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR1, UINI)
          ENDIF
        ENDIF
        CALL read_field(NCDATAFILE, MYTIME, STR2, VFORCE)
        CALL read_field(NCDATAFILE, MYTIME, STR1, UFORCE)
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(MYNY)
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(MYNX)
        CALL boundary_conditions(MYNX, MYNY, FRICT, XPERIODIC,
     >  YPERIODIC)
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_159)
        IF(OpenAD_Symbol_159 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_160)
          OpenAD_Symbol_161 = 1
          DO WHILE(INT(OpenAD_Symbol_161) .LE. INT(OpenAD_Symbol_160))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_162)
            OpenAD_Symbol_163 = 1
            DO WHILE(INT(OpenAD_Symbol_163) .LE. INT(OpenAD_Symbol_162)
     > )
              OpenAD_Symbol_163 = INT(OpenAD_Symbol_163) + 1
            END DO
            OpenAD_Symbol_161 = INT(OpenAD_Symbol_161) + 1
          END DO
        ENDIF
        CALL read_extended_field(NCDATAFILE, STR2, FRICT)
        CALL read_field(NCDATAFILE, MYTIME, STR1, INIDEPTH)
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_164)
      OpenAD_Symbol_165 = 1
      DO WHILE(INT(OpenAD_Symbol_165) .LE. INT(OpenAD_Symbol_164))
        OpenAD_Symbol_165 = INT(OpenAD_Symbol_165) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_166)
      OpenAD_Symbol_167 = 1
      DO WHILE(INT(OpenAD_Symbol_167) .LE. INT(OpenAD_Symbol_166))
        OpenAD_Symbol_167 = INT(OpenAD_Symbol_167) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RINI)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYTIME = (-1.0D00)
      OpenAD_Symbol_192 = 0_w2f__i8
      DO IX = 1, 20, 1
        X_IN(INT(IX)) = 0.0D00
        OpenAD_Symbol_192 = (INT(OpenAD_Symbol_192) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_192)
      OpenAD_Symbol_193 = 0_w2f__i8
      DO IY = 1, 20, 1
        Y_IN(INT(IY)) = 0.0D00
        OpenAD_Symbol_193 = (INT(OpenAD_Symbol_193) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_193)
C     inquire(file=ncdatafile,exist=exists)
      INQUIRE(EXIST = EXISTS, FILE = NCDATAFILE)
      IF(.NOT. EXISTS) THEN
        WRITE(*, *) NCDATAFILE, ' not found, cannot continue'
        OpenAD_Symbol_202 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_202)
      ELSE
        STR1 = 'depth'
        STR2 = 'frict'
        CALL read_field(NCDATAFILE, MYTIME, STR1, INIDEPTH)
        CALL read_extended_field(NCDATAFILE, STR2, FRICT)
        IF(RINI .ne. 0.0D00) THEN
          WRITE(*, *) 'rini = ', RINI
          WRITE(*, *) 'will overwrite frict with rini'
          OpenAD_Symbol_194 = 0_w2f__i8
          DO IX = 1, 20, 1
            OpenAD_Symbol_195 = 0_w2f__i8
            DO IY = 1, 20, 1
              FRICT(INT(IX), INT(IY)) = RINI
              OpenAD_Symbol_195 = (INT(OpenAD_Symbol_195) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_195)
            OpenAD_Symbol_194 = (INT(OpenAD_Symbol_194) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_194)
          OpenAD_Symbol_197 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_197)
        ELSE
          OpenAD_Symbol_196 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_196)
        ENDIF
        CALL boundary_conditions(MYNX, MYNY, FRICT, XPERIODIC,
     >  YPERIODIC)
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(MYNX)
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(MYNY)
        STR1 = 'uforce'
        STR2 = 'vforce'
        CALL read_field(NCDATAFILE, MYTIME, STR1, UFORCE)
        CALL read_field(NCDATAFILE, MYTIME, STR2, VFORCE)
        IF(START_TIME .eq. 0.0D00) THEN
          WRITE(*, *) 'cold start from initial fields'
          STR1 = 'uini'
          STR2 = 'vini'
          STR3 = 'etaini'
          CALL read_field(NCDATAFILE, START_TIME, STR1, UINI)
          CALL read_field(NCDATAFILE, START_TIME, STR2, VINI)
          CALL read_field(NCDATAFILE, START_TIME, STR3, ETAINI)
          OpenAD_Symbol_200 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_200)
        ELSE
          WRITE(*, *) 'warm restart from time ', START_TIME
          WRITE(*, *) 'in restart file ', NCRESTARTFILE
C         inquire( file = ncrestartfile, exist = exists ) 
          INQUIRE(EXIST = EXISTS, FILE = NCRESTARTFILE)
          IF(.NOT. EXISTS) THEN
            WRITE(*, *) NCRESTARTFILE, ' not found'
            OpenAD_Symbol_198 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_198)
          ELSE
            STR1 = 'U'
            STR2 = 'V'
            STR3 = 'ETA'
            CALL read_field(NCRESTARTFILE, START_TIME, STR1, UINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR2, VINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR3, ETAINI)
            OpenAD_Symbol_199 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_199)
          ENDIF
          OpenAD_Symbol_201 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_201)
        ENDIF
        OpenAD_Symbol_203 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_203)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_180)
      IF(OpenAD_Symbol_180 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_181)
        IF(OpenAD_Symbol_181 .ne. 0) THEN
          CALL read_field(NCDATAFILE, START_TIME, STR3, ETAINI)
          CALL read_field(NCDATAFILE, START_TIME, STR2, VINI)
          CALL read_field(NCDATAFILE, START_TIME, STR1, UINI)
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_182)
          IF(OpenAD_Symbol_182 .ne. 0) THEN
          ELSE
            CALL read_field(NCRESTARTFILE, START_TIME, STR3, ETAINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR2, VINI)
            CALL read_field(NCRESTARTFILE, START_TIME, STR1, UINI)
          ENDIF
        ENDIF
        CALL read_field(NCDATAFILE, MYTIME, STR2, VFORCE)
        CALL read_field(NCDATAFILE, MYTIME, STR1, UFORCE)
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(MYNY)
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(MYNX)
        CALL boundary_conditions(MYNX, MYNY, FRICT, XPERIODIC,
     >  YPERIODIC)
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_183)
        IF(OpenAD_Symbol_183 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_184)
          OpenAD_Symbol_185 = 1
          DO WHILE(INT(OpenAD_Symbol_185) .LE. INT(OpenAD_Symbol_184))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_186)
            OpenAD_Symbol_187 = 1
            DO WHILE(INT(OpenAD_Symbol_187) .LE. INT(OpenAD_Symbol_186)
     > )
              OpenAD_Symbol_187 = INT(OpenAD_Symbol_187) + 1
            END DO
            OpenAD_Symbol_185 = INT(OpenAD_Symbol_185) + 1
          END DO
        ENDIF
        CALL read_extended_field(NCDATAFILE, STR2, FRICT)
        CALL read_field(NCDATAFILE, MYTIME, STR1, INIDEPTH)
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_188)
      OpenAD_Symbol_189 = 1
      DO WHILE(INT(OpenAD_Symbol_189) .LE. INT(OpenAD_Symbol_188))
        OpenAD_Symbol_189 = INT(OpenAD_Symbol_189) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_190)
      OpenAD_Symbol_191 = 1
      DO WHILE(INT(OpenAD_Symbol_191) .LE. INT(OpenAD_Symbol_190))
        OpenAD_Symbol_191 = INT(OpenAD_Symbol_191) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE prep_depth()
      use w2f__types
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_204
      INTEGER(w2f__i8) OpenAD_Symbol_205
      INTEGER(w2f__i8) OpenAD_Symbol_206
      INTEGER(w2f__i8) OpenAD_Symbol_207
      INTEGER(w2f__i8) OpenAD_Symbol_208
      INTEGER(w2f__i8) OpenAD_Symbol_209
      INTEGER(w2f__i8) OpenAD_Symbol_210
      INTEGER(w2f__i8) OpenAD_Symbol_211
      INTEGER(w2f__i8) OpenAD_Symbol_212
      INTEGER(w2f__i8) OpenAD_Symbol_213
      INTEGER(w2f__i8) OpenAD_Symbol_214
      INTEGER(w2f__i8) OpenAD_Symbol_215
      INTEGER(w2f__i8) OpenAD_Symbol_216
      INTEGER(w2f__i8) OpenAD_Symbol_217
      INTEGER(w2f__i8) OpenAD_Symbol_218
      INTEGER(w2f__i8) OpenAD_Symbol_219
      INTEGER(w2f__i8) OpenAD_Symbol_220
      INTEGER(w2f__i8) OpenAD_Symbol_221
      INTEGER(w2f__i8) OpenAD_Symbol_222
      INTEGER(w2f__i8) OpenAD_Symbol_223
      INTEGER(w2f__i8) OpenAD_Symbol_224
      INTEGER(w2f__i8) OpenAD_Symbol_225
      INTEGER(w2f__i8) OpenAD_Symbol_226
      INTEGER(w2f__i8) OpenAD_Symbol_227
      INTEGER(w2f__i8) OpenAD_Symbol_228
      INTEGER(w2f__i8) OpenAD_Symbol_229
      INTEGER(w2f__i8) OpenAD_Symbol_230
      INTEGER(w2f__i8) OpenAD_Symbol_231
      INTEGER(w2f__i8) OpenAD_Symbol_232
      INTEGER(w2f__i8) OpenAD_Symbol_233
      INTEGER(w2f__i8) OpenAD_Symbol_234
      INTEGER(w2f__i8) OpenAD_Symbol_235
      INTEGER(w2f__i8) OpenAD_Symbol_236
      INTEGER(w2f__i8) OpenAD_Symbol_237
      INTEGER(w2f__i8) OpenAD_Symbol_238
      INTEGER(w2f__i8) OpenAD_Symbol_239
C
C     **** Local Variables and Functions ****
C
      EXTERNAL boundary_conditions
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      REAL(w2f__8) MAXDEPTH
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      EXTERNAL read_depth_data
      INTEGER(w2f__i4) OpenAD_Symbol_1312
      INTEGER(w2f__i4) OpenAD_Symbol_1313
      REAL(w2f__8) OpenAD_tyc_0(0 : 21, 0 : 21)
      REAL(w2f__8) OpenAD_tyc_5(0 : 21, 0 : 21)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MAXDEPTH = 0.0D00
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(INIDEPTH(IX, IY) .GT. MAXDEPTH) THEN
            MAXDEPTH = INIDEPTH(IX, IY)
          ENDIF
        END DO
      END DO
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          __value__(DEPTH(INT(IX), INT(IY))) = INIDEPTH(IX, IY)
          SCALEDEPTH(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
          IF(__value__(DEPTH(IX, IY)) .LT. MAXDEPTH) THEN
          ENDIF
        END DO
      END DO
      CALL read_depth_data()
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(OpenAD_tyc_0, __deriv__(DEPTH))
      CALL boundary_conditions(MYNX, MYNY, OpenAD_tyc_0, XPERIODIC,
     >  YPERIODIC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(DEPTH), OpenAD_tyc_0)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MAXDEPTH = 0.0D00
      OpenAD_Symbol_214 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_215 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(INIDEPTH(IX, IY) .GT. MAXDEPTH) THEN
            MAXDEPTH = INIDEPTH(IX, IY)
            OpenAD_Symbol_216 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_216)
          ELSE
            OpenAD_Symbol_217 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_217)
          ENDIF
          OpenAD_Symbol_215 = (INT(OpenAD_Symbol_215) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_215)
        OpenAD_Symbol_214 = (INT(OpenAD_Symbol_214) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_214)
      OpenAD_Symbol_218 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_219 = 0_w2f__i8
        DO IY = 1, 20, 1
          __value__(DEPTH(INT(IX), INT(IY))) = INIDEPTH(IX, IY)
          SCALEDEPTH(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          IF(__value__(DEPTH(IX, IY)) .LT. MAXDEPTH) THEN
            OpenAD_Symbol_220 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_220)
          ELSE
            OpenAD_Symbol_221 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_221)
          ENDIF
          OpenAD_Symbol_219 = (INT(OpenAD_Symbol_219) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_219)
        OpenAD_Symbol_218 = (INT(OpenAD_Symbol_218) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_218)
      CALL read_depth_data()
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(OpenAD_tyc_0, __deriv__(DEPTH))
      CALL boundary_conditions(MYNX, MYNY, OpenAD_tyc_0, XPERIODIC,
     >  YPERIODIC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(DEPTH), OpenAD_tyc_0)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL boundary_conditions(MYNX, MYNY, OpenAD_tyc_5, XPERIODIC,
     >  YPERIODIC)
      CALL read_depth_data()
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_204)
      OpenAD_Symbol_205 = 1
      DO WHILE(INT(OpenAD_Symbol_205) .LE. INT(OpenAD_Symbol_204))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_206)
        OpenAD_Symbol_207 = 1
        DO WHILE(INT(OpenAD_Symbol_207) .LE. INT(OpenAD_Symbol_206))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_208)
          IF(OpenAD_Symbol_208 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1312)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1313)
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(DEPTH(OpenAD_Symbol_1313,
     >  OpenAD_Symbol_1312)))
          OpenAD_Symbol_207 = INT(OpenAD_Symbol_207) + 1
        END DO
        OpenAD_Symbol_205 = INT(OpenAD_Symbol_205) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_209)
      OpenAD_Symbol_210 = 1
      DO WHILE(INT(OpenAD_Symbol_210) .LE. INT(OpenAD_Symbol_209))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_211)
        OpenAD_Symbol_212 = 1
        DO WHILE(INT(OpenAD_Symbol_212) .LE. INT(OpenAD_Symbol_211))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_213)
          IF(OpenAD_Symbol_213 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_212 = INT(OpenAD_Symbol_212) + 1
        END DO
        OpenAD_Symbol_210 = INT(OpenAD_Symbol_210) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MAXDEPTH = 0.0D00
      OpenAD_Symbol_232 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_233 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(INIDEPTH(IX, IY) .GT. MAXDEPTH) THEN
            MAXDEPTH = INIDEPTH(IX, IY)
            OpenAD_Symbol_234 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_234)
          ELSE
            OpenAD_Symbol_235 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_235)
          ENDIF
          OpenAD_Symbol_233 = (INT(OpenAD_Symbol_233) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_233)
        OpenAD_Symbol_232 = (INT(OpenAD_Symbol_232) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_232)
      OpenAD_Symbol_236 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_237 = 0_w2f__i8
        DO IY = 1, 20, 1
          __value__(DEPTH(INT(IX), INT(IY))) = INIDEPTH(IX, IY)
          SCALEDEPTH(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          IF(__value__(DEPTH(IX, IY)) .LT. MAXDEPTH) THEN
            OpenAD_Symbol_238 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_238)
          ELSE
            OpenAD_Symbol_239 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_239)
          ENDIF
          OpenAD_Symbol_237 = (INT(OpenAD_Symbol_237) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_237)
        OpenAD_Symbol_236 = (INT(OpenAD_Symbol_236) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_236)
      CALL read_depth_data()
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(OpenAD_tyc_0, __deriv__(DEPTH))
      CALL boundary_conditions(MYNX, MYNY, OpenAD_tyc_0, XPERIODIC,
     >  YPERIODIC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(DEPTH), OpenAD_tyc_0)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL boundary_conditions(MYNX, MYNY, OpenAD_tyc_5, XPERIODIC,
     >  YPERIODIC)
      CALL read_depth_data()
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_222)
      OpenAD_Symbol_223 = 1
      DO WHILE(INT(OpenAD_Symbol_223) .LE. INT(OpenAD_Symbol_222))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_224)
        OpenAD_Symbol_225 = 1
        DO WHILE(INT(OpenAD_Symbol_225) .LE. INT(OpenAD_Symbol_224))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_226)
          IF(OpenAD_Symbol_226 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1312)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1313)
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(DEPTH(OpenAD_Symbol_1313,
     >  OpenAD_Symbol_1312)))
          OpenAD_Symbol_225 = INT(OpenAD_Symbol_225) + 1
        END DO
        OpenAD_Symbol_223 = INT(OpenAD_Symbol_223) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_227)
      OpenAD_Symbol_228 = 1
      DO WHILE(INT(OpenAD_Symbol_228) .LE. INT(OpenAD_Symbol_227))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_229)
        OpenAD_Symbol_230 = 1
        DO WHILE(INT(OpenAD_Symbol_230) .LE. INT(OpenAD_Symbol_229))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_231)
          IF(OpenAD_Symbol_231 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_230 = INT(OpenAD_Symbol_230) + 1
        END DO
        OpenAD_Symbol_228 = INT(OpenAD_Symbol_228) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE ini_scales()
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_354
      INTEGER(w2f__i8) OpenAD_Symbol_355
      INTEGER(w2f__i8) OpenAD_Symbol_356
      INTEGER(w2f__i8) OpenAD_Symbol_357
      INTEGER(w2f__i8) OpenAD_Symbol_358
      INTEGER(w2f__i8) OpenAD_Symbol_359
      INTEGER(w2f__i8) OpenAD_Symbol_360
      INTEGER(w2f__i8) OpenAD_Symbol_361
      INTEGER(w2f__i8) OpenAD_Symbol_362
      INTEGER(w2f__i8) OpenAD_Symbol_363
      INTEGER(w2f__i8) OpenAD_Symbol_364
      INTEGER(w2f__i8) OpenAD_Symbol_365
      INTEGER(w2f__i8) OpenAD_Symbol_366
      INTEGER(w2f__i8) OpenAD_Symbol_367
      INTEGER(w2f__i8) OpenAD_Symbol_368
      INTEGER(w2f__i8) OpenAD_Symbol_369
      INTEGER(w2f__i8) OpenAD_Symbol_370
      INTEGER(w2f__i8) OpenAD_Symbol_371
      INTEGER(w2f__i8) OpenAD_Symbol_372
      INTEGER(w2f__i8) OpenAD_Symbol_373
      INTEGER(w2f__i8) OpenAD_Symbol_374
      INTEGER(w2f__i8) OpenAD_Symbol_375
      INTEGER(w2f__i8) OpenAD_Symbol_376
      INTEGER(w2f__i8) OpenAD_Symbol_377
      INTEGER(w2f__i8) OpenAD_Symbol_378
      INTEGER(w2f__i8) OpenAD_Symbol_379
      INTEGER(w2f__i8) OpenAD_Symbol_380
      INTEGER(w2f__i8) OpenAD_Symbol_381
      INTEGER(w2f__i8) OpenAD_Symbol_382
      INTEGER(w2f__i8) OpenAD_Symbol_383
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      REAL(w2f__8) VARETA
      EXTERNAL variance
      REAL(w2f__8) VARU
      REAL(w2f__8) VARV
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL variance(MYNX, MYNY, UINI, UMASK, VARU)
      IF(VARU .eq. 0.0D00) THEN
        VARU = 1.0D00
      ENDIF
      CALL variance(MYNX, MYNY, VINI, VMASK, VARV)
      IF(VARU .eq. 0.0D00) THEN
        VARV = 1.0D00
      ENDIF
      CALL variance(MYNX, MYNY, ETAINI, ETAMASK, VARETA)
      IF(VARU .eq. 0.0D00) THEN
        VARETA = 1.0D00
      ENDIF
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          SCALEU(INT(IX), INT(IY)) = (UMASK(IX, IY) * SQRT(VARU))
          SCALEV(INT(IX), INT(IY)) = (VMASK(IX, IY) * SQRT(VARV))
          SCALEETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * SQRT(VARETA))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL variance(MYNX, MYNY, UINI, UMASK, VARU)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      IF(VARU .eq. 0.0D00) THEN
        VARU = 1.0D00
        OpenAD_Symbol_361 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_361)
      ELSE
        OpenAD_Symbol_362 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_362)
      ENDIF
      CALL variance(MYNX, MYNY, VINI, VMASK, VARV)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      IF(VARU .eq. 0.0D00) THEN
        VARV = 1.0D00
        OpenAD_Symbol_363 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_363)
      ELSE
        OpenAD_Symbol_364 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_364)
      ENDIF
      CALL variance(MYNX, MYNY, ETAINI, ETAMASK, VARETA)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      IF(VARU .eq. 0.0D00) THEN
        VARETA = 1.0D00
        OpenAD_Symbol_365 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_365)
      ELSE
        OpenAD_Symbol_366 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_366)
      ENDIF
      OpenAD_Symbol_367 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_368 = 0_w2f__i8
        DO IX = 1, 20, 1
          SCALEU(INT(IX), INT(IY)) = (UMASK(IX, IY) * SQRT(VARU))
          SCALEV(INT(IX), INT(IY)) = (VMASK(IX, IY) * SQRT(VARV))
          SCALEETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * SQRT(VARETA))
          OpenAD_Symbol_368 = (INT(OpenAD_Symbol_368) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_368)
        OpenAD_Symbol_367 = (INT(OpenAD_Symbol_367) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_367)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_354)
      OpenAD_Symbol_355 = 1
      DO WHILE(INT(OpenAD_Symbol_355) .LE. INT(OpenAD_Symbol_354))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_356)
        OpenAD_Symbol_357 = 1
        DO WHILE(INT(OpenAD_Symbol_357) .LE. INT(OpenAD_Symbol_356))
          OpenAD_Symbol_357 = INT(OpenAD_Symbol_357) + 1
        END DO
        OpenAD_Symbol_355 = INT(OpenAD_Symbol_355) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_358)
      IF(OpenAD_Symbol_358 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL variance(MYNX, MYNY, ETAINI, ETAMASK, VARETA)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_359)
      IF(OpenAD_Symbol_359 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL variance(MYNX, MYNY, VINI, VMASK, VARV)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_360)
      IF(OpenAD_Symbol_360 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL variance(MYNX, MYNY, UINI, UMASK, VARU)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL variance(MYNX, MYNY, UINI, UMASK, VARU)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      IF(VARU .eq. 0.0D00) THEN
        VARU = 1.0D00
        OpenAD_Symbol_376 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_376)
      ELSE
        OpenAD_Symbol_377 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_377)
      ENDIF
      CALL variance(MYNX, MYNY, VINI, VMASK, VARV)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      IF(VARU .eq. 0.0D00) THEN
        VARV = 1.0D00
        OpenAD_Symbol_378 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_378)
      ELSE
        OpenAD_Symbol_379 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_379)
      ENDIF
      CALL variance(MYNX, MYNY, ETAINI, ETAMASK, VARETA)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      IF(VARU .eq. 0.0D00) THEN
        VARETA = 1.0D00
        OpenAD_Symbol_380 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_380)
      ELSE
        OpenAD_Symbol_381 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_381)
      ENDIF
      OpenAD_Symbol_382 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_383 = 0_w2f__i8
        DO IX = 1, 20, 1
          SCALEU(INT(IX), INT(IY)) = (UMASK(IX, IY) * SQRT(VARU))
          SCALEV(INT(IX), INT(IY)) = (VMASK(IX, IY) * SQRT(VARV))
          SCALEETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * SQRT(VARETA))
          OpenAD_Symbol_383 = (INT(OpenAD_Symbol_383) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_383)
        OpenAD_Symbol_382 = (INT(OpenAD_Symbol_382) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_382)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_369)
      OpenAD_Symbol_370 = 1
      DO WHILE(INT(OpenAD_Symbol_370) .LE. INT(OpenAD_Symbol_369))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_371)
        OpenAD_Symbol_372 = 1
        DO WHILE(INT(OpenAD_Symbol_372) .LE. INT(OpenAD_Symbol_371))
          OpenAD_Symbol_372 = INT(OpenAD_Symbol_372) + 1
        END DO
        OpenAD_Symbol_370 = INT(OpenAD_Symbol_370) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_373)
      IF(OpenAD_Symbol_373 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL variance(MYNX, MYNY, ETAINI, ETAMASK, VARETA)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_374)
      IF(OpenAD_Symbol_374 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL variance(MYNX, MYNY, VINI, VMASK, VARV)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_375)
      IF(OpenAD_Symbol_375 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL variance(MYNX, MYNY, UINI, UMASK, VARU)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE prep_coriolis()
      use w2f__types
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_384
      INTEGER(w2f__i8) OpenAD_Symbol_385
      INTEGER(w2f__i8) OpenAD_Symbol_386
      INTEGER(w2f__i8) OpenAD_Symbol_387
      INTEGER(w2f__i8) OpenAD_Symbol_388
      INTEGER(w2f__i8) OpenAD_Symbol_389
      INTEGER(w2f__i8) OpenAD_Symbol_390
      INTEGER(w2f__i8) OpenAD_Symbol_391
      INTEGER(w2f__i8) OpenAD_Symbol_392
      INTEGER(w2f__i8) OpenAD_Symbol_393
      INTEGER(w2f__i8) OpenAD_Symbol_394
      INTEGER(w2f__i8) OpenAD_Symbol_395
      INTEGER(w2f__i8) OpenAD_Symbol_396
      INTEGER(w2f__i8) OpenAD_Symbol_397
      INTEGER(w2f__i8) OpenAD_Symbol_398
      INTEGER(w2f__i8) OpenAD_Symbol_399
      INTEGER(w2f__i8) OpenAD_Symbol_400
      INTEGER(w2f__i8) OpenAD_Symbol_401
      INTEGER(w2f__i8) OpenAD_Symbol_402
      INTEGER(w2f__i8) OpenAD_Symbol_403
      INTEGER(w2f__i8) OpenAD_Symbol_404
      INTEGER(w2f__i8) OpenAD_Symbol_405
      INTEGER(w2f__i8) OpenAD_Symbol_406
      INTEGER(w2f__i8) OpenAD_Symbol_407
      INTEGER(w2f__i8) OpenAD_Symbol_408
      INTEGER(w2f__i8) OpenAD_Symbol_409
      INTEGER(w2f__i8) OpenAD_Symbol_410
      INTEGER(w2f__i8) OpenAD_Symbol_411
      INTEGER(w2f__i8) OpenAD_Symbol_412
      INTEGER(w2f__i8) OpenAD_Symbol_413
      INTEGER(w2f__i8) OpenAD_Symbol_414
      INTEGER(w2f__i8) OpenAD_Symbol_415
      INTEGER(w2f__i8) OpenAD_Symbol_416
      INTEGER(w2f__i8) OpenAD_Symbol_417
      INTEGER(w2f__i8) OpenAD_Symbol_418
      INTEGER(w2f__i8) OpenAD_Symbol_419
      INTEGER(w2f__i8) OpenAD_Symbol_420
      INTEGER(w2f__i8) OpenAD_Symbol_421
      INTEGER(w2f__i8) OpenAD_Symbol_422
      INTEGER(w2f__i8) OpenAD_Symbol_423
      INTEGER(w2f__i8) OpenAD_Symbol_424
      INTEGER(w2f__i8) OpenAD_Symbol_425
      INTEGER(w2f__i8) OpenAD_Symbol_426
      INTEGER(w2f__i8) OpenAD_Symbol_427
      INTEGER(w2f__i8) OpenAD_Symbol_428
      INTEGER(w2f__i8) OpenAD_Symbol_429
      INTEGER(w2f__i8) OpenAD_Symbol_430
      INTEGER(w2f__i8) OpenAD_Symbol_431
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) FAUX
      REAL(w2f__8) FCORI(0 : 21, 0 : 21)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MY = NINT(REAL(20) * 5.0D-01)
      DO IY = 0, 21, 1
        IF(SPHERICAL) THEN
          FAUX = (SIN(Y(IY) * 1.74532925199432954744D-02) * OM * 2.0D00
     > )
        ELSE
          IF(CARTESIAN) THEN
            FAUX = (F0 + BETA *(Y(IY) - Y(MY)))
          ENDIF
        ENDIF
        DO IX = 0, 21, 1
          FCORI(INT(IX), INT(IY)) = FAUX
        END DO
      END DO
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          FAUX = (VMASK(IX + (-1), IY + 1) + VMASK(IX, IY + 1) + VMASK(
     > IX, IY) + VMASK(IX + (-1), IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
          ELSE
            FAUX = (UMASK(IX, IY) * 2.5D-01)
          ENDIF
          FCORIU(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX +
     >  (-1), IY)) * 5.0D-01)
          FAUX = (UMASK(IX + 1, IY + (-1)) + UMASK(IX, IY + (-1)) +
     >  UMASK(IX, IY) + UMASK(IX + 1, IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
          ELSE
            FAUX = (VMASK(IX, IY) * 2.5D-01)
          ENDIF
          FCORIV(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX,
     >  IY + (-1))) * 5.0D-01)
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MY = NINT(REAL(20) * 5.0D-01)
      OpenAD_Symbol_396 = 0_w2f__i8
      DO IY = 0, 21, 1
        IF(SPHERICAL) THEN
          FAUX = (SIN(Y(IY) * 1.74532925199432954744D-02) * OM * 2.0D00
     > )
          OpenAD_Symbol_399 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_399)
        ELSE
          IF(CARTESIAN) THEN
            FAUX = (F0 + BETA *(Y(IY) - Y(MY)))
            OpenAD_Symbol_397 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_397)
          ELSE
            OpenAD_Symbol_398 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_398)
          ENDIF
          OpenAD_Symbol_400 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_400)
        ENDIF
        OpenAD_Symbol_401 = 0_w2f__i8
        DO IX = 0, 21, 1
          FCORI(INT(IX), INT(IY)) = FAUX
          OpenAD_Symbol_401 = (INT(OpenAD_Symbol_401) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_401)
        OpenAD_Symbol_396 = (INT(OpenAD_Symbol_396) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_396)
      OpenAD_Symbol_402 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_403 = 0_w2f__i8
        DO IY = 1, 20, 1
          FAUX = (VMASK(IX + (-1), IY + 1) + VMASK(IX, IY + 1) + VMASK(
     > IX, IY) + VMASK(IX + (-1), IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
            OpenAD_Symbol_404 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_404)
          ELSE
            FAUX = (UMASK(IX, IY) * 2.5D-01)
            OpenAD_Symbol_405 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_405)
          ENDIF
          FCORIU(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX +
     >  (-1), IY)) * 5.0D-01)
          FAUX = (UMASK(IX + 1, IY + (-1)) + UMASK(IX, IY + (-1)) +
     >  UMASK(IX, IY) + UMASK(IX + 1, IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
            OpenAD_Symbol_406 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_406)
          ELSE
            FAUX = (VMASK(IX, IY) * 2.5D-01)
            OpenAD_Symbol_407 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_407)
          ENDIF
          FCORIV(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX,
     >  IY + (-1))) * 5.0D-01)
          OpenAD_Symbol_403 = (INT(OpenAD_Symbol_403) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_403)
        OpenAD_Symbol_402 = (INT(OpenAD_Symbol_402) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_402)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_384)
      OpenAD_Symbol_385 = 1
      DO WHILE(INT(OpenAD_Symbol_385) .LE. INT(OpenAD_Symbol_384))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_386)
        OpenAD_Symbol_387 = 1
        DO WHILE(INT(OpenAD_Symbol_387) .LE. INT(OpenAD_Symbol_386))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_388)
          IF(OpenAD_Symbol_388 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_389)
          IF(OpenAD_Symbol_389 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_387 = INT(OpenAD_Symbol_387) + 1
        END DO
        OpenAD_Symbol_385 = INT(OpenAD_Symbol_385) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_390)
      OpenAD_Symbol_391 = 1
      DO WHILE(INT(OpenAD_Symbol_391) .LE. INT(OpenAD_Symbol_390))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_392)
        OpenAD_Symbol_393 = 1
        DO WHILE(INT(OpenAD_Symbol_393) .LE. INT(OpenAD_Symbol_392))
          OpenAD_Symbol_393 = INT(OpenAD_Symbol_393) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_394)
        IF(OpenAD_Symbol_394 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_395)
          IF(OpenAD_Symbol_395 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_391 = INT(OpenAD_Symbol_391) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(BETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(OM)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(F0)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(BETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MY = NINT(REAL(20) * 5.0D-01)
      OpenAD_Symbol_420 = 0_w2f__i8
      DO IY = 0, 21, 1
        IF(SPHERICAL) THEN
          FAUX = (SIN(Y(IY) * 1.74532925199432954744D-02) * OM * 2.0D00
     > )
          OpenAD_Symbol_423 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_423)
        ELSE
          IF(CARTESIAN) THEN
            FAUX = (F0 + BETA *(Y(IY) - Y(MY)))
            OpenAD_Symbol_421 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_421)
          ELSE
            OpenAD_Symbol_422 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_422)
          ENDIF
          OpenAD_Symbol_424 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_424)
        ENDIF
        OpenAD_Symbol_425 = 0_w2f__i8
        DO IX = 0, 21, 1
          FCORI(INT(IX), INT(IY)) = FAUX
          OpenAD_Symbol_425 = (INT(OpenAD_Symbol_425) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_425)
        OpenAD_Symbol_420 = (INT(OpenAD_Symbol_420) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_420)
      OpenAD_Symbol_426 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_427 = 0_w2f__i8
        DO IY = 1, 20, 1
          FAUX = (VMASK(IX + (-1), IY + 1) + VMASK(IX, IY + 1) + VMASK(
     > IX, IY) + VMASK(IX + (-1), IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
            OpenAD_Symbol_428 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_428)
          ELSE
            FAUX = (UMASK(IX, IY) * 2.5D-01)
            OpenAD_Symbol_429 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_429)
          ENDIF
          FCORIU(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX +
     >  (-1), IY)) * 5.0D-01)
          FAUX = (UMASK(IX + 1, IY + (-1)) + UMASK(IX, IY + (-1)) +
     >  UMASK(IX, IY) + UMASK(IX + 1, IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
            OpenAD_Symbol_430 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_430)
          ELSE
            FAUX = (VMASK(IX, IY) * 2.5D-01)
            OpenAD_Symbol_431 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_431)
          ENDIF
          FCORIV(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX,
     >  IY + (-1))) * 5.0D-01)
          OpenAD_Symbol_427 = (INT(OpenAD_Symbol_427) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_427)
        OpenAD_Symbol_426 = (INT(OpenAD_Symbol_426) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_426)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_408)
      OpenAD_Symbol_409 = 1
      DO WHILE(INT(OpenAD_Symbol_409) .LE. INT(OpenAD_Symbol_408))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_410)
        OpenAD_Symbol_411 = 1
        DO WHILE(INT(OpenAD_Symbol_411) .LE. INT(OpenAD_Symbol_410))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_412)
          IF(OpenAD_Symbol_412 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_413)
          IF(OpenAD_Symbol_413 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_411 = INT(OpenAD_Symbol_411) + 1
        END DO
        OpenAD_Symbol_409 = INT(OpenAD_Symbol_409) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_414)
      OpenAD_Symbol_415 = 1
      DO WHILE(INT(OpenAD_Symbol_415) .LE. INT(OpenAD_Symbol_414))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_416)
        OpenAD_Symbol_417 = 1
        DO WHILE(INT(OpenAD_Symbol_417) .LE. INT(OpenAD_Symbol_416))
          OpenAD_Symbol_417 = INT(OpenAD_Symbol_417) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_418)
        IF(OpenAD_Symbol_418 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_419)
          IF(OpenAD_Symbol_419 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_415 = INT(OpenAD_Symbol_415) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_masks()
      use w2f__types
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_288
      INTEGER(w2f__i8) OpenAD_Symbol_289
      INTEGER(w2f__i8) OpenAD_Symbol_290
      INTEGER(w2f__i8) OpenAD_Symbol_291
      INTEGER(w2f__i8) OpenAD_Symbol_292
      INTEGER(w2f__i8) OpenAD_Symbol_293
      INTEGER(w2f__i8) OpenAD_Symbol_294
      INTEGER(w2f__i8) OpenAD_Symbol_295
      INTEGER(w2f__i8) OpenAD_Symbol_296
      INTEGER(w2f__i8) OpenAD_Symbol_297
      INTEGER(w2f__i8) OpenAD_Symbol_298
      INTEGER(w2f__i8) OpenAD_Symbol_299
      INTEGER(w2f__i8) OpenAD_Symbol_300
      INTEGER(w2f__i8) OpenAD_Symbol_301
      INTEGER(w2f__i8) OpenAD_Symbol_302
      INTEGER(w2f__i8) OpenAD_Symbol_303
      INTEGER(w2f__i8) OpenAD_Symbol_304
      INTEGER(w2f__i8) OpenAD_Symbol_305
      INTEGER(w2f__i8) OpenAD_Symbol_306
      INTEGER(w2f__i8) OpenAD_Symbol_307
      INTEGER(w2f__i8) OpenAD_Symbol_308
      INTEGER(w2f__i8) OpenAD_Symbol_309
      INTEGER(w2f__i8) OpenAD_Symbol_310
      INTEGER(w2f__i8) OpenAD_Symbol_311
      INTEGER(w2f__i8) OpenAD_Symbol_312
      INTEGER(w2f__i8) OpenAD_Symbol_313
      INTEGER(w2f__i8) OpenAD_Symbol_314
      INTEGER(w2f__i8) OpenAD_Symbol_315
      INTEGER(w2f__i8) OpenAD_Symbol_316
      INTEGER(w2f__i8) OpenAD_Symbol_317
      INTEGER(w2f__i8) OpenAD_Symbol_318
      INTEGER(w2f__i8) OpenAD_Symbol_319
      INTEGER(w2f__i8) OpenAD_Symbol_320
      INTEGER(w2f__i8) OpenAD_Symbol_321
      INTEGER(w2f__i8) OpenAD_Symbol_322
      INTEGER(w2f__i8) OpenAD_Symbol_323
      INTEGER(w2f__i8) OpenAD_Symbol_324
      INTEGER(w2f__i8) OpenAD_Symbol_325
      INTEGER(w2f__i8) OpenAD_Symbol_326
      INTEGER(w2f__i8) OpenAD_Symbol_327
      INTEGER(w2f__i8) OpenAD_Symbol_328
      INTEGER(w2f__i8) OpenAD_Symbol_329
      INTEGER(w2f__i8) OpenAD_Symbol_330
      INTEGER(w2f__i8) OpenAD_Symbol_331
      INTEGER(w2f__i8) OpenAD_Symbol_332
      INTEGER(w2f__i8) OpenAD_Symbol_333
      INTEGER(w2f__i8) OpenAD_Symbol_334
      INTEGER(w2f__i8) OpenAD_Symbol_335
      INTEGER(w2f__i8) OpenAD_Symbol_336
      INTEGER(w2f__i8) OpenAD_Symbol_337
      INTEGER(w2f__i8) OpenAD_Symbol_338
      INTEGER(w2f__i8) OpenAD_Symbol_339
      INTEGER(w2f__i8) OpenAD_Symbol_340
      INTEGER(w2f__i8) OpenAD_Symbol_341
      INTEGER(w2f__i8) OpenAD_Symbol_342
      INTEGER(w2f__i8) OpenAD_Symbol_343
      INTEGER(w2f__i8) OpenAD_Symbol_344
      INTEGER(w2f__i8) OpenAD_Symbol_345
      INTEGER(w2f__i8) OpenAD_Symbol_346
      INTEGER(w2f__i8) OpenAD_Symbol_347
      INTEGER(w2f__i8) OpenAD_Symbol_348
      INTEGER(w2f__i8) OpenAD_Symbol_349
      INTEGER(w2f__i8) OpenAD_Symbol_350
      INTEGER(w2f__i8) OpenAD_Symbol_351
      INTEGER(w2f__i8) OpenAD_Symbol_352
      INTEGER(w2f__i8) OpenAD_Symbol_353
C
C     **** Local Variables and Functions ****
C
      EXTERNAL boundary_conditions
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      REAL(w2f__8) MINDEPTH
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. __value__(DEPTH(IX + (-1),
     >  IY))) THEN
            MINDEPTH = __value__(DEPTH(IX, IY))
          ELSE
            MINDEPTH = __value__(DEPTH(IX + (-1), IY))
          ENDIF
          IF(MINDEPTH .ne. 0.0D00) THEN
            UMASK(INT(IX), INT(IY)) = 1.0D00
          ELSE
            UMASK(INT(IX), INT(IY)) = 0.0D00
          ENDIF
        END DO
      END DO
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. __value__(DEPTH(IX, IY + (-1
     > )))) THEN
            MINDEPTH = __value__(DEPTH(IX, IY))
          ELSE
            MINDEPTH = __value__(DEPTH(IX, IY + (-1)))
          ENDIF
          IF(MINDEPTH .ne. 0.0D00) THEN
            VMASK(INT(IX), INT(IY)) = 1.0D00
          ELSE
            VMASK(INT(IX), INT(IY)) = 0.0D00
          ENDIF
        END DO
      END DO
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .ne. 0.0D00) THEN
            ETAMASK(INT(IX), INT(IY)) = 1.0D00
          ELSE
            ETAMASK(INT(IX), INT(IY)) = 0.0D00
          ENDIF
        END DO
      END DO
      CALL boundary_conditions(MYNX, MYNY, UMASK, XPERIODIC, YPERIODIC)
      CALL boundary_conditions(MYNX, MYNY, VMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_305 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_306 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. __value__(DEPTH(IX + (-1),
     >  IY))) THEN
            MINDEPTH = __value__(DEPTH(IX, IY))
            OpenAD_Symbol_307 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_307)
          ELSE
            MINDEPTH = __value__(DEPTH(IX + (-1), IY))
            OpenAD_Symbol_308 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_308)
          ENDIF
          IF(MINDEPTH .ne. 0.0D00) THEN
            UMASK(INT(IX), INT(IY)) = 1.0D00
            OpenAD_Symbol_309 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_309)
          ELSE
            UMASK(INT(IX), INT(IY)) = 0.0D00
            OpenAD_Symbol_310 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_310)
          ENDIF
          OpenAD_Symbol_306 = (INT(OpenAD_Symbol_306) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_306)
        OpenAD_Symbol_305 = (INT(OpenAD_Symbol_305) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_305)
      OpenAD_Symbol_311 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_312 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. __value__(DEPTH(IX, IY + (-1
     > )))) THEN
            MINDEPTH = __value__(DEPTH(IX, IY))
            OpenAD_Symbol_313 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_313)
          ELSE
            MINDEPTH = __value__(DEPTH(IX, IY + (-1)))
            OpenAD_Symbol_314 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_314)
          ENDIF
          IF(MINDEPTH .ne. 0.0D00) THEN
            VMASK(INT(IX), INT(IY)) = 1.0D00
            OpenAD_Symbol_315 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_315)
          ELSE
            VMASK(INT(IX), INT(IY)) = 0.0D00
            OpenAD_Symbol_316 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_316)
          ENDIF
          OpenAD_Symbol_312 = (INT(OpenAD_Symbol_312) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_312)
        OpenAD_Symbol_311 = (INT(OpenAD_Symbol_311) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_311)
      OpenAD_Symbol_317 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_318 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .ne. 0.0D00) THEN
            ETAMASK(INT(IX), INT(IY)) = 1.0D00
            OpenAD_Symbol_319 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_319)
          ELSE
            ETAMASK(INT(IX), INT(IY)) = 0.0D00
            OpenAD_Symbol_320 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_320)
          ENDIF
          OpenAD_Symbol_318 = (INT(OpenAD_Symbol_318) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_318)
        OpenAD_Symbol_317 = (INT(OpenAD_Symbol_317) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_317)
      CALL boundary_conditions(MYNX, MYNY, UMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      CALL boundary_conditions(MYNX, MYNY, VMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL boundary_conditions(MYNX, MYNY, VMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL boundary_conditions(MYNX, MYNY, UMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_288)
      OpenAD_Symbol_289 = 1
      DO WHILE(INT(OpenAD_Symbol_289) .LE. INT(OpenAD_Symbol_288))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_290)
        OpenAD_Symbol_291 = 1
        DO WHILE(INT(OpenAD_Symbol_291) .LE. INT(OpenAD_Symbol_290))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_292)
          IF(OpenAD_Symbol_292 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_291 = INT(OpenAD_Symbol_291) + 1
        END DO
        OpenAD_Symbol_289 = INT(OpenAD_Symbol_289) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_293)
      OpenAD_Symbol_294 = 1
      DO WHILE(INT(OpenAD_Symbol_294) .LE. INT(OpenAD_Symbol_293))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_295)
        OpenAD_Symbol_296 = 1
        DO WHILE(INT(OpenAD_Symbol_296) .LE. INT(OpenAD_Symbol_295))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_297)
          IF(OpenAD_Symbol_297 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_298)
          IF(OpenAD_Symbol_298 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_296 = INT(OpenAD_Symbol_296) + 1
        END DO
        OpenAD_Symbol_294 = INT(OpenAD_Symbol_294) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_299)
      OpenAD_Symbol_300 = 1
      DO WHILE(INT(OpenAD_Symbol_300) .LE. INT(OpenAD_Symbol_299))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_301)
        OpenAD_Symbol_302 = 1
        DO WHILE(INT(OpenAD_Symbol_302) .LE. INT(OpenAD_Symbol_301))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_303)
          IF(OpenAD_Symbol_303 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_304)
          IF(OpenAD_Symbol_304 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_302 = INT(OpenAD_Symbol_302) + 1
        END DO
        OpenAD_Symbol_300 = INT(OpenAD_Symbol_300) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_338 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_339 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. __value__(DEPTH(IX + (-1),
     >  IY))) THEN
            MINDEPTH = __value__(DEPTH(IX, IY))
            OpenAD_Symbol_340 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_340)
          ELSE
            MINDEPTH = __value__(DEPTH(IX + (-1), IY))
            OpenAD_Symbol_341 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_341)
          ENDIF
          IF(MINDEPTH .ne. 0.0D00) THEN
            UMASK(INT(IX), INT(IY)) = 1.0D00
            OpenAD_Symbol_342 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_342)
          ELSE
            UMASK(INT(IX), INT(IY)) = 0.0D00
            OpenAD_Symbol_343 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_343)
          ENDIF
          OpenAD_Symbol_339 = (INT(OpenAD_Symbol_339) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_339)
        OpenAD_Symbol_338 = (INT(OpenAD_Symbol_338) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_338)
      OpenAD_Symbol_344 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_345 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. __value__(DEPTH(IX, IY + (-1
     > )))) THEN
            MINDEPTH = __value__(DEPTH(IX, IY))
            OpenAD_Symbol_346 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_346)
          ELSE
            MINDEPTH = __value__(DEPTH(IX, IY + (-1)))
            OpenAD_Symbol_347 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_347)
          ENDIF
          IF(MINDEPTH .ne. 0.0D00) THEN
            VMASK(INT(IX), INT(IY)) = 1.0D00
            OpenAD_Symbol_348 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_348)
          ELSE
            VMASK(INT(IX), INT(IY)) = 0.0D00
            OpenAD_Symbol_349 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_349)
          ENDIF
          OpenAD_Symbol_345 = (INT(OpenAD_Symbol_345) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_345)
        OpenAD_Symbol_344 = (INT(OpenAD_Symbol_344) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_344)
      OpenAD_Symbol_350 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_351 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .ne. 0.0D00) THEN
            ETAMASK(INT(IX), INT(IY)) = 1.0D00
            OpenAD_Symbol_352 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_352)
          ELSE
            ETAMASK(INT(IX), INT(IY)) = 0.0D00
            OpenAD_Symbol_353 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_353)
          ENDIF
          OpenAD_Symbol_351 = (INT(OpenAD_Symbol_351) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_351)
        OpenAD_Symbol_350 = (INT(OpenAD_Symbol_350) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_350)
      CALL boundary_conditions(MYNX, MYNY, UMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
      CALL boundary_conditions(MYNX, MYNY, VMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNX)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(MYNY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL boundary_conditions(MYNX, MYNY, VMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNY)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(MYNX)
      CALL boundary_conditions(MYNX, MYNY, UMASK, XPERIODIC, YPERIODIC)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_321)
      OpenAD_Symbol_322 = 1
      DO WHILE(INT(OpenAD_Symbol_322) .LE. INT(OpenAD_Symbol_321))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_323)
        OpenAD_Symbol_324 = 1
        DO WHILE(INT(OpenAD_Symbol_324) .LE. INT(OpenAD_Symbol_323))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_325)
          IF(OpenAD_Symbol_325 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_324 = INT(OpenAD_Symbol_324) + 1
        END DO
        OpenAD_Symbol_322 = INT(OpenAD_Symbol_322) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_326)
      OpenAD_Symbol_327 = 1
      DO WHILE(INT(OpenAD_Symbol_327) .LE. INT(OpenAD_Symbol_326))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_328)
        OpenAD_Symbol_329 = 1
        DO WHILE(INT(OpenAD_Symbol_329) .LE. INT(OpenAD_Symbol_328))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_330)
          IF(OpenAD_Symbol_330 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_331)
          IF(OpenAD_Symbol_331 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_329 = INT(OpenAD_Symbol_329) + 1
        END DO
        OpenAD_Symbol_327 = INT(OpenAD_Symbol_327) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_332)
      OpenAD_Symbol_333 = 1
      DO WHILE(INT(OpenAD_Symbol_333) .LE. INT(OpenAD_Symbol_332))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_334)
        OpenAD_Symbol_335 = 1
        DO WHILE(INT(OpenAD_Symbol_335) .LE. INT(OpenAD_Symbol_334))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_336)
          IF(OpenAD_Symbol_336 .ne. 0) THEN
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_337)
          IF(OpenAD_Symbol_337 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_335 = INT(OpenAD_Symbol_335) + 1
        END DO
        OpenAD_Symbol_333 = INT(OpenAD_Symbol_333) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE variance(NX, NY, F, FMASK, VARF)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_552
      INTEGER(w2f__i8) OpenAD_Symbol_553
      INTEGER(w2f__i8) OpenAD_Symbol_554
      INTEGER(w2f__i8) OpenAD_Symbol_555
      INTEGER(w2f__i8) OpenAD_Symbol_556
      INTEGER(w2f__i8) OpenAD_Symbol_557
      INTEGER(w2f__i8) OpenAD_Symbol_558
      INTEGER(w2f__i8) OpenAD_Symbol_559
      INTEGER(w2f__i8) OpenAD_Symbol_560
      INTEGER(w2f__i8) OpenAD_Symbol_561
      INTEGER(w2f__i8) OpenAD_Symbol_562
      INTEGER(w2f__i8) OpenAD_Symbol_563
      INTEGER(w2f__i8) OpenAD_Symbol_564
      INTEGER(w2f__i8) OpenAD_Symbol_565
      INTEGER(w2f__i8) OpenAD_Symbol_566
      INTEGER(w2f__i8) OpenAD_Symbol_567
      INTEGER(w2f__i8) OpenAD_Symbol_568
      INTEGER(w2f__i8) OpenAD_Symbol_569
      INTEGER(w2f__i8) OpenAD_Symbol_570
      INTEGER(w2f__i8) OpenAD_Symbol_571
      INTEGER(w2f__i8) OpenAD_Symbol_572
      INTEGER(w2f__i8) OpenAD_Symbol_573
      INTEGER(w2f__i8) OpenAD_Symbol_574
      INTEGER(w2f__i8) OpenAD_Symbol_575
      INTEGER(w2f__i8) OpenAD_Symbol_576
      INTEGER(w2f__i8) OpenAD_Symbol_577
      INTEGER(w2f__i8) OpenAD_Symbol_578
      INTEGER(w2f__i8) OpenAD_Symbol_579
      INTEGER(w2f__i8) OpenAD_Symbol_580
      INTEGER(w2f__i8) OpenAD_Symbol_581
      INTEGER(w2f__i8) OpenAD_Symbol_582
      INTEGER(w2f__i8) OpenAD_Symbol_583
      INTEGER(w2f__i8) OpenAD_Symbol_584
      INTEGER(w2f__i8) OpenAD_Symbol_585
      INTEGER(w2f__i8) OpenAD_Symbol_586
      INTEGER(w2f__i8) OpenAD_Symbol_587
      INTEGER(w2f__i8) OpenAD_Symbol_588
      INTEGER(w2f__i8) OpenAD_Symbol_589
      INTEGER(w2f__i8) OpenAD_Symbol_590
      INTEGER(w2f__i8) OpenAD_Symbol_591
      INTEGER(w2f__i8) OpenAD_Symbol_592
      INTEGER(w2f__i8) OpenAD_Symbol_593
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) NX
      INTEGER(w2f__i4) NY
      REAL(w2f__8) F(1 : NX, 1 : NY)
      REAL(w2f__8) FMASK(0 : INT((NX + 1)), 0 : INT((NY + 1)))
      REAL(w2f__8) VARF
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) K
      REAL(w2f__8) MEANF
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      VARF = 0.0D00
      MEANF = 0.0D00
      K = 0
      DO IY = 1, NY, 1
        DO IX = 1, NX, 1
          IF(FMASK(IX, IY) .ne. 0.0D00) THEN
            MEANF = (F(IX, IY) + MEANF)
            K = (K + 1)
          ENDIF
        END DO
      END DO
      IF(K .ne. 0) THEN
        MEANF = (MEANF / REAL(K))
      ENDIF
      DO IY = 1, NY, 1
        DO IX = 1, NX, 1
          VARF = (VARF + FMASK(IX, IY) *((F(IX, IY) - MEANF) ** 2))
        END DO
      END DO
      IF(K .GT. 1) THEN
        VARF = (VARF / REAL(K +(-1)))
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      VARF = 0.0D00
      MEANF = 0.0D00
      K = 0
      OpenAD_Symbol_563 = 0_w2f__i8
      DO IY = 1, NY, 1
        OpenAD_Symbol_564 = 0_w2f__i8
        DO IX = 1, NX, 1
          IF(FMASK(IX, IY) .ne. 0.0D00) THEN
            MEANF = (F(IX, IY) + MEANF)
            K = (K + 1)
            OpenAD_Symbol_565 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_565)
          ELSE
            OpenAD_Symbol_566 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_566)
          ENDIF
          OpenAD_Symbol_564 = (INT(OpenAD_Symbol_564) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_564)
        OpenAD_Symbol_563 = (INT(OpenAD_Symbol_563) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_563)
      IF(K .ne. 0) THEN
        MEANF = (MEANF / REAL(K))
        OpenAD_Symbol_567 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_567)
      ELSE
        OpenAD_Symbol_568 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_568)
      ENDIF
      OpenAD_Symbol_569 = 0_w2f__i8
      DO IY = 1, NY, 1
        OpenAD_Symbol_570 = 0_w2f__i8
        DO IX = 1, NX, 1
          VARF = (VARF + FMASK(IX, IY) *((F(IX, IY) - MEANF) ** 2))
          OpenAD_Symbol_570 = (INT(OpenAD_Symbol_570) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_570)
        OpenAD_Symbol_569 = (INT(OpenAD_Symbol_569) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_569)
      IF(K .GT. 1) THEN
        VARF = (VARF / REAL(K +(-1)))
        OpenAD_Symbol_571 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_571)
      ELSE
        OpenAD_Symbol_572 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_572)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_552)
      IF(OpenAD_Symbol_552 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_553)
      OpenAD_Symbol_554 = 1
      DO WHILE(INT(OpenAD_Symbol_554) .LE. INT(OpenAD_Symbol_553))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_555)
        OpenAD_Symbol_556 = 1
        DO WHILE(INT(OpenAD_Symbol_556) .LE. INT(OpenAD_Symbol_555))
          OpenAD_Symbol_556 = INT(OpenAD_Symbol_556) + 1
        END DO
        OpenAD_Symbol_554 = INT(OpenAD_Symbol_554) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_557)
      IF(OpenAD_Symbol_557 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_558)
      OpenAD_Symbol_559 = 1
      DO WHILE(INT(OpenAD_Symbol_559) .LE. INT(OpenAD_Symbol_558))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_560)
        OpenAD_Symbol_561 = 1
        DO WHILE(INT(OpenAD_Symbol_561) .LE. INT(OpenAD_Symbol_560))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_562)
          IF(OpenAD_Symbol_562 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_561 = INT(OpenAD_Symbol_561) + 1
        END DO
        OpenAD_Symbol_559 = INT(OpenAD_Symbol_559) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(VARF)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(F)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(F)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(VARF)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NX)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(VARF)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(VARF)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(F)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(F)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(VARF)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NY)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(VARF)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      VARF = 0.0D00
      MEANF = 0.0D00
      K = 0
      OpenAD_Symbol_584 = 0_w2f__i8
      DO IY = 1, NY, 1
        OpenAD_Symbol_585 = 0_w2f__i8
        DO IX = 1, NX, 1
          IF(FMASK(IX, IY) .ne. 0.0D00) THEN
            MEANF = (F(IX, IY) + MEANF)
            K = (K + 1)
            OpenAD_Symbol_586 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_586)
          ELSE
            OpenAD_Symbol_587 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_587)
          ENDIF
          OpenAD_Symbol_585 = (INT(OpenAD_Symbol_585) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_585)
        OpenAD_Symbol_584 = (INT(OpenAD_Symbol_584) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_584)
      IF(K .ne. 0) THEN
        MEANF = (MEANF / REAL(K))
        OpenAD_Symbol_588 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_588)
      ELSE
        OpenAD_Symbol_589 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_589)
      ENDIF
      OpenAD_Symbol_590 = 0_w2f__i8
      DO IY = 1, NY, 1
        OpenAD_Symbol_591 = 0_w2f__i8
        DO IX = 1, NX, 1
          VARF = (VARF + FMASK(IX, IY) *((F(IX, IY) - MEANF) ** 2))
          OpenAD_Symbol_591 = (INT(OpenAD_Symbol_591) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_591)
        OpenAD_Symbol_590 = (INT(OpenAD_Symbol_590) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_590)
      IF(K .GT. 1) THEN
        VARF = (VARF / REAL(K +(-1)))
        OpenAD_Symbol_592 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_592)
      ELSE
        OpenAD_Symbol_593 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_593)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_573)
      IF(OpenAD_Symbol_573 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_574)
      OpenAD_Symbol_575 = 1
      DO WHILE(INT(OpenAD_Symbol_575) .LE. INT(OpenAD_Symbol_574))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_576)
        OpenAD_Symbol_577 = 1
        DO WHILE(INT(OpenAD_Symbol_577) .LE. INT(OpenAD_Symbol_576))
          OpenAD_Symbol_577 = INT(OpenAD_Symbol_577) + 1
        END DO
        OpenAD_Symbol_575 = INT(OpenAD_Symbol_575) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_578)
      IF(OpenAD_Symbol_578 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_579)
      OpenAD_Symbol_580 = 1
      DO WHILE(INT(OpenAD_Symbol_580) .LE. INT(OpenAD_Symbol_579))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_581)
        OpenAD_Symbol_582 = 1
        DO WHILE(INT(OpenAD_Symbol_582) .LE. INT(OpenAD_Symbol_581))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_583)
          IF(OpenAD_Symbol_583 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_582 = INT(OpenAD_Symbol_582) + 1
        END DO
        OpenAD_Symbol_580 = INT(OpenAD_Symbol_580) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE check_cfl()
      use w2f__types
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_240
      INTEGER(w2f__i8) OpenAD_Symbol_241
      INTEGER(w2f__i8) OpenAD_Symbol_242
      INTEGER(w2f__i8) OpenAD_Symbol_243
      INTEGER(w2f__i8) OpenAD_Symbol_244
      INTEGER(w2f__i8) OpenAD_Symbol_245
      INTEGER(w2f__i8) OpenAD_Symbol_246
      INTEGER(w2f__i8) OpenAD_Symbol_247
      INTEGER(w2f__i8) OpenAD_Symbol_248
      INTEGER(w2f__i8) OpenAD_Symbol_249
      INTEGER(w2f__i8) OpenAD_Symbol_250
      INTEGER(w2f__i8) OpenAD_Symbol_251
      INTEGER(w2f__i8) OpenAD_Symbol_252
      INTEGER(w2f__i8) OpenAD_Symbol_253
      INTEGER(w2f__i8) OpenAD_Symbol_254
      INTEGER(w2f__i8) OpenAD_Symbol_255
      INTEGER(w2f__i8) OpenAD_Symbol_256
      INTEGER(w2f__i8) OpenAD_Symbol_257
      INTEGER(w2f__i8) OpenAD_Symbol_258
      INTEGER(w2f__i8) OpenAD_Symbol_259
      INTEGER(w2f__i8) OpenAD_Symbol_260
      INTEGER(w2f__i8) OpenAD_Symbol_261
      INTEGER(w2f__i8) OpenAD_Symbol_262
      INTEGER(w2f__i8) OpenAD_Symbol_263
      INTEGER(w2f__i8) OpenAD_Symbol_264
      INTEGER(w2f__i8) OpenAD_Symbol_265
      INTEGER(w2f__i8) OpenAD_Symbol_266
      INTEGER(w2f__i8) OpenAD_Symbol_267
      INTEGER(w2f__i8) OpenAD_Symbol_268
      INTEGER(w2f__i8) OpenAD_Symbol_269
      INTEGER(w2f__i8) OpenAD_Symbol_270
      INTEGER(w2f__i8) OpenAD_Symbol_271
      INTEGER(w2f__i8) OpenAD_Symbol_272
      INTEGER(w2f__i8) OpenAD_Symbol_273
      INTEGER(w2f__i8) OpenAD_Symbol_274
      INTEGER(w2f__i8) OpenAD_Symbol_275
      INTEGER(w2f__i8) OpenAD_Symbol_276
      INTEGER(w2f__i8) OpenAD_Symbol_277
      INTEGER(w2f__i8) OpenAD_Symbol_278
      INTEGER(w2f__i8) OpenAD_Symbol_279
      INTEGER(w2f__i8) OpenAD_Symbol_280
      INTEGER(w2f__i8) OpenAD_Symbol_281
      INTEGER(w2f__i8) OpenAD_Symbol_282
      INTEGER(w2f__i8) OpenAD_Symbol_283
      INTEGER(w2f__i8) OpenAD_Symbol_284
      INTEGER(w2f__i8) OpenAD_Symbol_285
      INTEGER(w2f__i8) OpenAD_Symbol_286
      INTEGER(w2f__i8) OpenAD_Symbol_287
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) CFLX
      REAL(w2f__8) CFLY
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      REAL(w2f__8) MAXIMUM
      REAL(w2f__8) MDEP
      REAL(w2f__8) MDX
      REAL(w2f__8) MDY
      REAL(w2f__8) MINIMUM
      REAL(w2f__8) WAVESPEED
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MDEP = 0.0D00
      MDX = 9.99999999999999916114D+22
      MDY = 9.99999999999999916114D+22
      DO IX = 1, 20, 1
        IF(DX(IX) .GT. MDX) THEN
          MINIMUM = MDX
        ELSE
          MINIMUM = DX(IX)
        ENDIF
        MDX = MINIMUM
      END DO
      DO IY = 1, 20, 1
        IF(DY(IY) .GT. MDY) THEN
          MINIMUM = MDY
        ELSE
          MINIMUM = DY(IY)
        ENDIF
        MDY = MINIMUM
      END DO
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. MDEP) THEN
            MAXIMUM = MDEP
          ELSE
            MAXIMUM = __value__(DEPTH(IX, IY))
          ENDIF
          MDEP = MAXIMUM
        END DO
      END DO
      WAVESPEED = SQRT(MDEP * 9.81000000000000049738D00)
      CFLX = ((WAVESPEED * DT) / MDX)
      CFLY = ((WAVESPEED * DT) / MDY)
      WRITE(*, *) 'rough check of CLF criterion:'
      IF((CFLX .GE. 1.0D00) .OR.(CFLY .GE. 1.0D00)) THEN
        WRITE(*, *) 'warning: CLF criterion not met'
        WRITE(*, *) 'sqrt(g*max(depth))*dt/min(dx) = ', CFLX
        WRITE(*, *) 'sqrt(g*max(depth))*dt/min(dy) = ', CFLY
      ELSE
        WRITE(*, *) 'OK'
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MDEP = 0.0D00
      MDX = 9.99999999999999916114D+22
      MDY = 9.99999999999999916114D+22
      OpenAD_Symbol_252 = 0_w2f__i8
      DO IX = 1, 20, 1
        IF(DX(IX) .GT. MDX) THEN
          MINIMUM = MDX
          OpenAD_Symbol_253 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_253)
        ELSE
          MINIMUM = DX(IX)
          OpenAD_Symbol_254 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_254)
        ENDIF
        MDX = MINIMUM
        OpenAD_Symbol_252 = (INT(OpenAD_Symbol_252) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_252)
      OpenAD_Symbol_255 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(DY(IY) .GT. MDY) THEN
          MINIMUM = MDY
          OpenAD_Symbol_256 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_256)
        ELSE
          MINIMUM = DY(IY)
          OpenAD_Symbol_257 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_257)
        ENDIF
        MDY = MINIMUM
        OpenAD_Symbol_255 = (INT(OpenAD_Symbol_255) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_255)
      OpenAD_Symbol_258 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_259 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. MDEP) THEN
            MAXIMUM = MDEP
            OpenAD_Symbol_260 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_260)
          ELSE
            MAXIMUM = __value__(DEPTH(IX, IY))
            OpenAD_Symbol_261 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_261)
          ENDIF
          MDEP = MAXIMUM
          OpenAD_Symbol_259 = (INT(OpenAD_Symbol_259) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_259)
        OpenAD_Symbol_258 = (INT(OpenAD_Symbol_258) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_258)
      WAVESPEED = SQRT(MDEP * 9.81000000000000049738D00)
      CFLX = ((WAVESPEED * DT) / MDX)
      CFLY = ((WAVESPEED * DT) / MDY)
      WRITE(*, *) 'rough check of CLF criterion:'
      IF((CFLX .GE. 1.0D00) .OR.(CFLY .GE. 1.0D00)) THEN
        WRITE(*, *) 'warning: CLF criterion not met'
        WRITE(*, *) 'sqrt(g*max(depth))*dt/min(dx) = ', CFLX
        WRITE(*, *) 'sqrt(g*max(depth))*dt/min(dy) = ', CFLY
        OpenAD_Symbol_262 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_262)
      ELSE
        WRITE(*, *) 'OK'
        OpenAD_Symbol_263 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_263)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_240)
      IF(OpenAD_Symbol_240 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_241)
      OpenAD_Symbol_242 = 1
      DO WHILE(INT(OpenAD_Symbol_242) .LE. INT(OpenAD_Symbol_241))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_243)
        OpenAD_Symbol_244 = 1
        DO WHILE(INT(OpenAD_Symbol_244) .LE. INT(OpenAD_Symbol_243))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_245)
          IF(OpenAD_Symbol_245 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_244 = INT(OpenAD_Symbol_244) + 1
        END DO
        OpenAD_Symbol_242 = INT(OpenAD_Symbol_242) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_246)
      OpenAD_Symbol_247 = 1
      DO WHILE(INT(OpenAD_Symbol_247) .LE. INT(OpenAD_Symbol_246))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_248)
        IF(OpenAD_Symbol_248 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_247 = INT(OpenAD_Symbol_247) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_249)
      OpenAD_Symbol_250 = 1
      DO WHILE(INT(OpenAD_Symbol_250) .LE. INT(OpenAD_Symbol_249))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_251)
        IF(OpenAD_Symbol_251 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_250 = INT(OpenAD_Symbol_250) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MDEP = 0.0D00
      MDX = 9.99999999999999916114D+22
      MDY = 9.99999999999999916114D+22
      OpenAD_Symbol_276 = 0_w2f__i8
      DO IX = 1, 20, 1
        IF(DX(IX) .GT. MDX) THEN
          MINIMUM = MDX
          OpenAD_Symbol_277 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_277)
        ELSE
          MINIMUM = DX(IX)
          OpenAD_Symbol_278 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_278)
        ENDIF
        MDX = MINIMUM
        OpenAD_Symbol_276 = (INT(OpenAD_Symbol_276) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_276)
      OpenAD_Symbol_279 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(DY(IY) .GT. MDY) THEN
          MINIMUM = MDY
          OpenAD_Symbol_280 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_280)
        ELSE
          MINIMUM = DY(IY)
          OpenAD_Symbol_281 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_281)
        ENDIF
        MDY = MINIMUM
        OpenAD_Symbol_279 = (INT(OpenAD_Symbol_279) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_279)
      OpenAD_Symbol_282 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_283 = 0_w2f__i8
        DO IY = 1, 20, 1
          IF(__value__(DEPTH(IX, IY)) .LT. MDEP) THEN
            MAXIMUM = MDEP
            OpenAD_Symbol_284 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_284)
          ELSE
            MAXIMUM = __value__(DEPTH(IX, IY))
            OpenAD_Symbol_285 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_285)
          ENDIF
          MDEP = MAXIMUM
          OpenAD_Symbol_283 = (INT(OpenAD_Symbol_283) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_283)
        OpenAD_Symbol_282 = (INT(OpenAD_Symbol_282) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_282)
      WAVESPEED = SQRT(MDEP * 9.81000000000000049738D00)
      CFLX = ((WAVESPEED * DT) / MDX)
      CFLY = ((WAVESPEED * DT) / MDY)
      WRITE(*, *) 'rough check of CLF criterion:'
      IF((CFLX .GE. 1.0D00) .OR.(CFLY .GE. 1.0D00)) THEN
        WRITE(*, *) 'warning: CLF criterion not met'
        WRITE(*, *) 'sqrt(g*max(depth))*dt/min(dx) = ', CFLX
        WRITE(*, *) 'sqrt(g*max(depth))*dt/min(dy) = ', CFLY
        OpenAD_Symbol_286 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_286)
      ELSE
        WRITE(*, *) 'OK'
        OpenAD_Symbol_287 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_287)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_264)
      IF(OpenAD_Symbol_264 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_265)
      OpenAD_Symbol_266 = 1
      DO WHILE(INT(OpenAD_Symbol_266) .LE. INT(OpenAD_Symbol_265))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_267)
        OpenAD_Symbol_268 = 1
        DO WHILE(INT(OpenAD_Symbol_268) .LE. INT(OpenAD_Symbol_267))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_269)
          IF(OpenAD_Symbol_269 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_268 = INT(OpenAD_Symbol_268) + 1
        END DO
        OpenAD_Symbol_266 = INT(OpenAD_Symbol_266) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_270)
      OpenAD_Symbol_271 = 1
      DO WHILE(INT(OpenAD_Symbol_271) .LE. INT(OpenAD_Symbol_270))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_272)
        IF(OpenAD_Symbol_272 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_271 = INT(OpenAD_Symbol_271) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_273)
      OpenAD_Symbol_274 = 1
      DO WHILE(INT(OpenAD_Symbol_274) .LE. INT(OpenAD_Symbol_273))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_275)
        IF(OpenAD_Symbol_275 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_274 = INT(OpenAD_Symbol_274) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_extended_field(NCDATAFILE, FNAME, FIELD)
      use w2f__types
      use size
      use size
      use size
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_516
      INTEGER(w2f__i8) OpenAD_Symbol_517
      INTEGER(w2f__i8) OpenAD_Symbol_518
      INTEGER(w2f__i8) OpenAD_Symbol_519
      INTEGER(w2f__i8) OpenAD_Symbol_520
      INTEGER(w2f__i8) OpenAD_Symbol_521
      INTEGER(w2f__i8) OpenAD_Symbol_522
      INTEGER(w2f__i8) OpenAD_Symbol_523
      INTEGER(w2f__i8) OpenAD_Symbol_524
      INTEGER(w2f__i8) OpenAD_Symbol_525
      INTEGER(w2f__i8) OpenAD_Symbol_526
      INTEGER(w2f__i8) OpenAD_Symbol_527
      INTEGER(w2f__i8) OpenAD_Symbol_528
      INTEGER(w2f__i8) OpenAD_Symbol_529
      INTEGER(w2f__i8) OpenAD_Symbol_530
      INTEGER(w2f__i8) OpenAD_Symbol_531
      INTEGER(w2f__i8) OpenAD_Symbol_532
      INTEGER(w2f__i8) OpenAD_Symbol_533
      INTEGER(w2f__i8) OpenAD_Symbol_534
      INTEGER(w2f__i8) OpenAD_Symbol_535
      INTEGER(w2f__i8) OpenAD_Symbol_536
      INTEGER(w2f__i8) OpenAD_Symbol_537
      INTEGER(w2f__i8) OpenAD_Symbol_538
      INTEGER(w2f__i8) OpenAD_Symbol_539
C
C     **** Parameters and Result ****
C
      CHARACTER(80) NCDATAFILE
      CHARACTER(80) FNAME
      REAL(w2f__8) FIELD(0 : 21, 0 : 21)
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) F_IN(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      EXTERNAL read_field_netcdf
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
        END DO
      END DO
      CALL read_field_netcdf(NCDATAFILE, FNAME, MYNX, MYNY, F_IN)
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_524 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_525 = 0_w2f__i8
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
          OpenAD_Symbol_525 = (INT(OpenAD_Symbol_525) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_525)
        OpenAD_Symbol_524 = (INT(OpenAD_Symbol_524) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_524)
      CALL read_field_netcdf(NCDATAFILE, FNAME, MYNX, MYNY, F_IN)
      OpenAD_Symbol_526 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_527 = 0_w2f__i8
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
          OpenAD_Symbol_527 = (INT(OpenAD_Symbol_527) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_527)
        OpenAD_Symbol_526 = (INT(OpenAD_Symbol_526) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_526)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_516)
      OpenAD_Symbol_517 = 1
      DO WHILE(INT(OpenAD_Symbol_517) .LE. INT(OpenAD_Symbol_516))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_518)
        OpenAD_Symbol_519 = 1
        DO WHILE(INT(OpenAD_Symbol_519) .LE. INT(OpenAD_Symbol_518))
          OpenAD_Symbol_519 = INT(OpenAD_Symbol_519) + 1
        END DO
        OpenAD_Symbol_517 = INT(OpenAD_Symbol_517) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_520)
      OpenAD_Symbol_521 = 1
      DO WHILE(INT(OpenAD_Symbol_521) .LE. INT(OpenAD_Symbol_520))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_522)
        OpenAD_Symbol_523 = 1
        DO WHILE(INT(OpenAD_Symbol_523) .LE. INT(OpenAD_Symbol_522))
          OpenAD_Symbol_523 = INT(OpenAD_Symbol_523) + 1
        END DO
        OpenAD_Symbol_521 = INT(OpenAD_Symbol_521) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FIELD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FIELD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_536 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_537 = 0_w2f__i8
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
          OpenAD_Symbol_537 = (INT(OpenAD_Symbol_537) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_537)
        OpenAD_Symbol_536 = (INT(OpenAD_Symbol_536) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_536)
      CALL read_field_netcdf(NCDATAFILE, FNAME, MYNX, MYNY, F_IN)
      OpenAD_Symbol_538 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_539 = 0_w2f__i8
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
          OpenAD_Symbol_539 = (INT(OpenAD_Symbol_539) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_539)
        OpenAD_Symbol_538 = (INT(OpenAD_Symbol_538) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_538)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_528)
      OpenAD_Symbol_529 = 1
      DO WHILE(INT(OpenAD_Symbol_529) .LE. INT(OpenAD_Symbol_528))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_530)
        OpenAD_Symbol_531 = 1
        DO WHILE(INT(OpenAD_Symbol_531) .LE. INT(OpenAD_Symbol_530))
          OpenAD_Symbol_531 = INT(OpenAD_Symbol_531) + 1
        END DO
        OpenAD_Symbol_529 = INT(OpenAD_Symbol_529) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_532)
      OpenAD_Symbol_533 = 1
      DO WHILE(INT(OpenAD_Symbol_533) .LE. INT(OpenAD_Symbol_532))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_534)
        OpenAD_Symbol_535 = 1
        DO WHILE(INT(OpenAD_Symbol_535) .LE. INT(OpenAD_Symbol_534))
          OpenAD_Symbol_535 = INT(OpenAD_Symbol_535) + 1
        END DO
        OpenAD_Symbol_533 = INT(OpenAD_Symbol_533) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_field(NCDATAFILE, START_TIME, FNAME, FIELD)
      use w2f__types
      use size
      use size
      use size
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_486
      INTEGER(w2f__i8) OpenAD_Symbol_487
      INTEGER(w2f__i8) OpenAD_Symbol_488
      INTEGER(w2f__i8) OpenAD_Symbol_489
      INTEGER(w2f__i8) OpenAD_Symbol_490
      INTEGER(w2f__i8) OpenAD_Symbol_491
      INTEGER(w2f__i8) OpenAD_Symbol_492
      INTEGER(w2f__i8) OpenAD_Symbol_493
      INTEGER(w2f__i8) OpenAD_Symbol_494
      INTEGER(w2f__i8) OpenAD_Symbol_495
      INTEGER(w2f__i8) OpenAD_Symbol_496
      INTEGER(w2f__i8) OpenAD_Symbol_497
      INTEGER(w2f__i8) OpenAD_Symbol_498
      INTEGER(w2f__i8) OpenAD_Symbol_499
      INTEGER(w2f__i8) OpenAD_Symbol_500
      INTEGER(w2f__i8) OpenAD_Symbol_501
      INTEGER(w2f__i8) OpenAD_Symbol_502
      INTEGER(w2f__i8) OpenAD_Symbol_503
      INTEGER(w2f__i8) OpenAD_Symbol_504
      INTEGER(w2f__i8) OpenAD_Symbol_505
      INTEGER(w2f__i8) OpenAD_Symbol_506
      INTEGER(w2f__i8) OpenAD_Symbol_507
      INTEGER(w2f__i8) OpenAD_Symbol_508
      INTEGER(w2f__i8) OpenAD_Symbol_509
      INTEGER(w2f__i8) OpenAD_Symbol_510
      INTEGER(w2f__i8) OpenAD_Symbol_511
      INTEGER(w2f__i8) OpenAD_Symbol_512
      INTEGER(w2f__i8) OpenAD_Symbol_513
      INTEGER(w2f__i8) OpenAD_Symbol_514
      INTEGER(w2f__i8) OpenAD_Symbol_515
C
C     **** Parameters and Result ****
C
      CHARACTER(80) NCDATAFILE
      REAL(w2f__8) START_TIME
      CHARACTER(80) FNAME
      REAL(w2f__8) FIELD(1 : 20, 1 : 20)
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) F_IN(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      SAVE MYNX
      INTEGER(w2f__i4) MYNY
      SAVE MYNY
      EXTERNAL read_field_netcdf
      EXTERNAL read_snap_netcdf
C
C     **** Initializers ****
C
      DATA MYNX / 20 /
      DATA MYNY / 20 /
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
        END DO
      END DO
      IF(START_TIME .LE. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, FNAME, MYNX, MYNY, F_IN)
      ELSE
        CALL read_snap_netcdf(NCDATAFILE, START_TIME, MYNX, MYNY, FNAME
     > , F_IN)
      ENDIF
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      OpenAD_Symbol_495 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_496 = 0_w2f__i8
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
          OpenAD_Symbol_496 = (INT(OpenAD_Symbol_496) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_496)
        OpenAD_Symbol_495 = (INT(OpenAD_Symbol_495) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_495)
      IF(START_TIME .LE. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, FNAME, MYNX, MYNY, F_IN)
        OpenAD_Symbol_497 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_497)
      ELSE
        CALL read_snap_netcdf(NCDATAFILE, START_TIME, MYNX, MYNY, FNAME
     > , F_IN)
        OpenAD_Symbol_498 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_498)
      ENDIF
      OpenAD_Symbol_499 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_500 = 0_w2f__i8
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
          OpenAD_Symbol_500 = (INT(OpenAD_Symbol_500) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_500)
        OpenAD_Symbol_499 = (INT(OpenAD_Symbol_499) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_499)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_486)
      OpenAD_Symbol_487 = 1
      DO WHILE(INT(OpenAD_Symbol_487) .LE. INT(OpenAD_Symbol_486))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_488)
        OpenAD_Symbol_489 = 1
        DO WHILE(INT(OpenAD_Symbol_489) .LE. INT(OpenAD_Symbol_488))
          OpenAD_Symbol_489 = INT(OpenAD_Symbol_489) + 1
        END DO
        OpenAD_Symbol_487 = INT(OpenAD_Symbol_487) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_490)
      IF(OpenAD_Symbol_490 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_491)
      OpenAD_Symbol_492 = 1
      DO WHILE(INT(OpenAD_Symbol_492) .LE. INT(OpenAD_Symbol_491))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_493)
        OpenAD_Symbol_494 = 1
        DO WHILE(INT(OpenAD_Symbol_494) .LE. INT(OpenAD_Symbol_493))
          OpenAD_Symbol_494 = INT(OpenAD_Symbol_494) + 1
        END DO
        OpenAD_Symbol_492 = INT(OpenAD_Symbol_492) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FIELD)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FIELD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      OpenAD_Symbol_510 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_511 = 0_w2f__i8
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
          OpenAD_Symbol_511 = (INT(OpenAD_Symbol_511) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_511)
        OpenAD_Symbol_510 = (INT(OpenAD_Symbol_510) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_510)
      IF(START_TIME .LE. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, FNAME, MYNX, MYNY, F_IN)
        OpenAD_Symbol_512 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_512)
      ELSE
        CALL read_snap_netcdf(NCDATAFILE, START_TIME, MYNX, MYNY, FNAME
     > , F_IN)
        OpenAD_Symbol_513 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_513)
      ENDIF
      OpenAD_Symbol_514 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_515 = 0_w2f__i8
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
          OpenAD_Symbol_515 = (INT(OpenAD_Symbol_515) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_515)
        OpenAD_Symbol_514 = (INT(OpenAD_Symbol_514) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_514)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_501)
      OpenAD_Symbol_502 = 1
      DO WHILE(INT(OpenAD_Symbol_502) .LE. INT(OpenAD_Symbol_501))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_503)
        OpenAD_Symbol_504 = 1
        DO WHILE(INT(OpenAD_Symbol_504) .LE. INT(OpenAD_Symbol_503))
          OpenAD_Symbol_504 = INT(OpenAD_Symbol_504) + 1
        END DO
        OpenAD_Symbol_502 = INT(OpenAD_Symbol_502) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_505)
      IF(OpenAD_Symbol_505 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_506)
      OpenAD_Symbol_507 = 1
      DO WHILE(INT(OpenAD_Symbol_507) .LE. INT(OpenAD_Symbol_506))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_508)
        OpenAD_Symbol_509 = 1
        DO WHILE(INT(OpenAD_Symbol_509) .LE. INT(OpenAD_Symbol_508))
          OpenAD_Symbol_509 = INT(OpenAD_Symbol_509) + 1
        END DO
        OpenAD_Symbol_507 = INT(OpenAD_Symbol_507) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE ini_io()
      use w2f__types
      use size
      use parms
      use pfields
      use weights
      use size
      use parms
      use pfields
      use weights
      use size
      use parms
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_594
      INTEGER(w2f__i8) OpenAD_Symbol_595
      INTEGER(w2f__i8) OpenAD_Symbol_596
      INTEGER(w2f__i8) OpenAD_Symbol_597
      INTEGER(w2f__i8) OpenAD_Symbol_598
      INTEGER(w2f__i8) OpenAD_Symbol_599
      INTEGER(w2f__i8) OpenAD_Symbol_600
      INTEGER(w2f__i8) OpenAD_Symbol_601
      INTEGER(w2f__i8) OpenAD_Symbol_602
      INTEGER(w2f__i8) OpenAD_Symbol_603
      INTEGER(w2f__i8) OpenAD_Symbol_604
      INTEGER(w2f__i8) OpenAD_Symbol_605
      INTEGER(w2f__i8) OpenAD_Symbol_606
      INTEGER(w2f__i8) OpenAD_Symbol_607
      INTEGER(w2f__i8) OpenAD_Symbol_608
      INTEGER(w2f__i8) OpenAD_Symbol_609
      INTEGER(w2f__i8) OpenAD_Symbol_610
      INTEGER(w2f__i8) OpenAD_Symbol_611
      INTEGER(w2f__i8) OpenAD_Symbol_612
      INTEGER(w2f__i8) OpenAD_Symbol_613
      INTEGER(w2f__i8) OpenAD_Symbol_614
      INTEGER(w2f__i8) OpenAD_Symbol_615
      INTEGER(w2f__i8) OpenAD_Symbol_616
      INTEGER(w2f__i8) OpenAD_Symbol_617
      INTEGER(w2f__i8) OpenAD_Symbol_618
      INTEGER(w2f__i8) OpenAD_Symbol_619
      INTEGER(w2f__i8) OpenAD_Symbol_620
      INTEGER(w2f__i8) OpenAD_Symbol_621
      INTEGER(w2f__i8) OpenAD_Symbol_622
      INTEGER(w2f__i8) OpenAD_Symbol_623
      INTEGER(w2f__i8) OpenAD_Symbol_624
      INTEGER(w2f__i8) OpenAD_Symbol_625
      INTEGER(w2f__i8) OpenAD_Symbol_626
      INTEGER(w2f__i8) OpenAD_Symbol_627
      INTEGER(w2f__i8) OpenAD_Symbol_628
      INTEGER(w2f__i8) OpenAD_Symbol_629
      INTEGER(w2f__i8) OpenAD_Symbol_630
      INTEGER(w2f__i8) OpenAD_Symbol_631
      INTEGER(w2f__i8) OpenAD_Symbol_632
      INTEGER(w2f__i8) OpenAD_Symbol_633
      INTEGER(w2f__i8) OpenAD_Symbol_634
      INTEGER(w2f__i8) OpenAD_Symbol_635
      INTEGER(w2f__i8) OpenAD_Symbol_636
      INTEGER(w2f__i8) OpenAD_Symbol_637
      INTEGER(w2f__i8) OpenAD_Symbol_638
      INTEGER(w2f__i8) OpenAD_Symbol_639
      INTEGER(w2f__i8) OpenAD_Symbol_640
      INTEGER(w2f__i8) OpenAD_Symbol_641
      INTEGER(w2f__i8) OpenAD_Symbol_642
      INTEGER(w2f__i8) OpenAD_Symbol_643
      INTEGER(w2f__i8) OpenAD_Symbol_644
      INTEGER(w2f__i8) OpenAD_Symbol_645
      INTEGER(w2f__i8) OpenAD_Symbol_646
      INTEGER(w2f__i8) OpenAD_Symbol_647
      INTEGER(w2f__i8) OpenAD_Symbol_648
      INTEGER(w2f__i8) OpenAD_Symbol_649
      INTEGER(w2f__i8) OpenAD_Symbol_650
      INTEGER(w2f__i8) OpenAD_Symbol_651
      INTEGER(w2f__i8) OpenAD_Symbol_652
      INTEGER(w2f__i8) OpenAD_Symbol_653
C
C     **** Local Variables and Functions ****
C
      EXTERNAL add_coordinates_netcdf
      EXTERNAL add_gatta_netcdf
      EXTERNAL add_gatti_netcdf
      EXTERNAL add_gattr_netcdf
      EXTERNAL add_recvar_netcdf
      EXTERNAL create_netcdf
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      REAL(w2f__8) MYEARTH
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      CHARACTER(80) STR1
      CHARACTER(80) STR2
      CHARACTER(80) STR3
      CHARACTER(80) STR4
      REAL(w2f__8) XOUT(1 : 20)
      REAL(w2f__8) YOUT(1 : 20)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYEARTH = 6.371D+06
      IF(FULLIO) THEN
        WRITE(*, *) 'initializing I/O'
      ENDIF
      CALL create_netcdf(FOUTNAME, RUNNAME, MYNX, MYNY)
      IF(SPHERICAL) THEN
        STR1 = 'grid_type'
        STR2 = 'spherical'
        STR3 = 'earth_radius'
        STR4 = 'Omega'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        CALL add_gattr_netcdf(FOUTNAME, STR3, MYEARTH)
        CALL add_gattr_netcdf(FOUTNAME, STR4, OM)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'grid_type'
          STR2 = 'cartesian'
          STR3 = 'f0'
          STR4 = 'beta'
          CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
          CALL add_gattr_netcdf(FOUTNAME, STR3, F0)
          CALL add_gattr_netcdf(FOUTNAME, STR4, BETA)
        ENDIF
      ENDIF
      STR1 = 'r_ini'
      STR2 = 'time_step'
      CALL add_gattr_netcdf(FOUTNAME, STR1, RINI)
      CALL add_gattr_netcdf(FOUTNAME, STR2, DT)
      IF(XPERIODIC) THEN
        STR1 = 'zonal_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
      ENDIF
      IF(YPERIODIC) THEN
        STR1 = 'meridional_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
      ENDIF
      STR1 = 'data_files'
      STR2 = NCDATAFILE // ' ' // DEPTHFILE // ' ' // FORCINGFILE //
     >  ' ' // UINIFILE // ' ' // VINIFILE // ' ' // ETAINIFILE
      CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
      IF(START_TIME .ne. 0.0D00) THEN
        STR1 = 'restart_file'
        CALL add_gatta_netcdf(FOUTNAME, STR1, NCRESTARTFILE)
      ENDIF
      STR1 = 'ntspinup'
      STR2 = 'wf_depth'
      STR3 = 'wf_eta'
      STR4 = 'wf_u'
      CALL add_gatti_netcdf(FOUTNAME, STR1, NTSPINUP)
      CALL add_gattr_netcdf(FOUTNAME, STR2, WF_DEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR3, WF_ETA)
      CALL add_gattr_netcdf(FOUTNAME, STR4, WF_U)
      STR1 = 'wf_v'
      STR2 = 'wf_lapldepth'
      STR3 = 'wf_graddepth'
      STR4 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_V)
      CALL add_gattr_netcdf(FOUTNAME, STR2, WF_LAPLDEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR3, WF_GRADDEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR4, WF_ZONAL_TRANSPORT)
      DO IX = 1, 20, 1
        XOUT(INT(IX)) = X(IX)
      END DO
      DO IY = 1, 20, 1
        YOUT(INT(IY)) = Y(IY)
      END DO
      IF(SPHERICAL) THEN
        STR1 = 'deg'
        CALL add_coordinates_netcdf(FOUTNAME, MYNX, XOUT, MYNY, YOUT,
     >  STR1)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'meters'
          CALL add_coordinates_netcdf(FOUTNAME, MYNX, XOUT, MYNY, YOUT,
     >  STR1)
        ENDIF
      ENDIF
      STR1 = 'U'
      STR2 = 'zonal velocity'
      STR3 = 'meters/seconds'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
      STR1 = 'V'
      STR2 = 'meridional velocity'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
      STR1 = 'ETA'
      STR2 = 'sea-surface elevation'
      STR3 = 'meters'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYEARTH = 6.371D+06
      IF(FULLIO) THEN
        WRITE(*, *) 'initializing I/O'
        OpenAD_Symbol_606 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_606)
      ELSE
        OpenAD_Symbol_607 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_607)
      ENDIF
      CALL create_netcdf(FOUTNAME, RUNNAME, MYNX, MYNY)
      IF(SPHERICAL) THEN
        STR1 = 'grid_type'
        STR2 = 'spherical'
        STR3 = 'earth_radius'
        STR4 = 'Omega'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        CALL add_gattr_netcdf(FOUTNAME, STR3, MYEARTH)
        CALL add_gattr_netcdf(FOUTNAME, STR4, OM)
        OpenAD_Symbol_610 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_610)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'grid_type'
          STR2 = 'cartesian'
          STR3 = 'f0'
          STR4 = 'beta'
          CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
          CALL add_gattr_netcdf(FOUTNAME, STR3, F0)
          CALL add_gattr_netcdf(FOUTNAME, STR4, BETA)
          OpenAD_Symbol_608 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_608)
        ELSE
          OpenAD_Symbol_609 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_609)
        ENDIF
        OpenAD_Symbol_611 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_611)
      ENDIF
      STR1 = 'r_ini'
      STR2 = 'time_step'
      CALL add_gattr_netcdf(FOUTNAME, STR1, RINI)
      CALL add_gattr_netcdf(FOUTNAME, STR2, DT)
      IF(XPERIODIC) THEN
        STR1 = 'zonal_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        OpenAD_Symbol_612 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_612)
      ELSE
        OpenAD_Symbol_613 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_613)
      ENDIF
      IF(YPERIODIC) THEN
        STR1 = 'meridional_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        OpenAD_Symbol_614 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_614)
      ELSE
        OpenAD_Symbol_615 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_615)
      ENDIF
      STR1 = 'data_files'
      STR2 = NCDATAFILE // ' ' // DEPTHFILE // ' ' // FORCINGFILE //
     >  ' ' // UINIFILE // ' ' // VINIFILE // ' ' // ETAINIFILE
      CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
      IF(START_TIME .ne. 0.0D00) THEN
        STR1 = 'restart_file'
        CALL add_gatta_netcdf(FOUTNAME, STR1, NCRESTARTFILE)
        OpenAD_Symbol_616 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_616)
      ELSE
        OpenAD_Symbol_617 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_617)
      ENDIF
      STR1 = 'ntspinup'
      STR2 = 'wf_depth'
      STR3 = 'wf_eta'
      STR4 = 'wf_u'
      CALL add_gatti_netcdf(FOUTNAME, STR1, NTSPINUP)
      CALL add_gattr_netcdf(FOUTNAME, STR2, WF_DEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR3, WF_ETA)
      CALL add_gattr_netcdf(FOUTNAME, STR4, WF_U)
      STR1 = 'wf_v'
      STR2 = 'wf_lapldepth'
      STR3 = 'wf_graddepth'
      STR4 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_V)
      CALL add_gattr_netcdf(FOUTNAME, STR2, WF_LAPLDEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR3, WF_GRADDEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR4, WF_ZONAL_TRANSPORT)
      OpenAD_Symbol_618 = 0_w2f__i8
      DO IX = 1, 20, 1
        XOUT(INT(IX)) = X(IX)
        OpenAD_Symbol_618 = (INT(OpenAD_Symbol_618) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_618)
      OpenAD_Symbol_619 = 0_w2f__i8
      DO IY = 1, 20, 1
        YOUT(INT(IY)) = Y(IY)
        OpenAD_Symbol_619 = (INT(OpenAD_Symbol_619) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_619)
      IF(SPHERICAL) THEN
        STR1 = 'deg'
        CALL add_coordinates_netcdf(FOUTNAME, MYNX, XOUT, MYNY, YOUT,
     >  STR1)
        OpenAD_Symbol_622 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_622)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'meters'
          CALL add_coordinates_netcdf(FOUTNAME, MYNX, XOUT, MYNY, YOUT,
     >  STR1)
          OpenAD_Symbol_620 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_620)
        ELSE
          OpenAD_Symbol_621 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_621)
        ENDIF
        OpenAD_Symbol_623 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_623)
      ENDIF
      STR1 = 'U'
      STR2 = 'zonal velocity'
      STR3 = 'meters/seconds'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
      STR1 = 'V'
      STR2 = 'meridional velocity'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
      STR1 = 'ETA'
      STR2 = 'sea-surface elevation'
      STR3 = 'meters'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_594)
      IF(OpenAD_Symbol_594 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_595)
        IF(OpenAD_Symbol_595 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_596)
      OpenAD_Symbol_597 = 1
      DO WHILE(INT(OpenAD_Symbol_597) .LE. INT(OpenAD_Symbol_596))
        OpenAD_Symbol_597 = INT(OpenAD_Symbol_597) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_598)
      OpenAD_Symbol_599 = 1
      DO WHILE(INT(OpenAD_Symbol_599) .LE. INT(OpenAD_Symbol_598))
        OpenAD_Symbol_599 = INT(OpenAD_Symbol_599) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_600)
      IF(OpenAD_Symbol_600 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_601)
      IF(OpenAD_Symbol_601 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_602)
      IF(OpenAD_Symbol_602 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_603)
      IF(OpenAD_Symbol_603 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_604)
        IF(OpenAD_Symbol_604 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_605)
      IF(OpenAD_Symbol_605 .ne. 0) THEN
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(DEPTHFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(ETAINIFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(FORCINGFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(NCDATAFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(NCRESTARTFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(UINIFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(VINIFILE)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(VINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(UINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(NCRESTARTFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(NCDATAFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(FORCINGFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(ETAINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(DEPTHFILE)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CARTESIAN)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(DEPTHFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(ETAINIFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(FORCINGFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(NCDATAFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(NCRESTARTFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(UINIFILE)
C     $OpenAD$ INLINE cp_arg_store_string_scalar(subst)
      CALL cp_arg_store_string_scalar(VINIFILE)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(X)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(Y)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(Y)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(X)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(VINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(UINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(NCRESTARTFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(NCDATAFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(FORCINGFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(ETAINIFILE)
C     $OpenAD$ INLINE cp_arg_restore_string_scalar(subst)
      CALL cp_arg_restore_string_scalar(DEPTHFILE)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SPHERICAL)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CARTESIAN)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYEARTH = 6.371D+06
      IF(FULLIO) THEN
        WRITE(*, *) 'initializing I/O'
        OpenAD_Symbol_636 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_636)
      ELSE
        OpenAD_Symbol_637 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_637)
      ENDIF
      CALL create_netcdf(FOUTNAME, RUNNAME, MYNX, MYNY)
      IF(SPHERICAL) THEN
        STR1 = 'grid_type'
        STR2 = 'spherical'
        STR3 = 'earth_radius'
        STR4 = 'Omega'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        CALL add_gattr_netcdf(FOUTNAME, STR3, MYEARTH)
        CALL add_gattr_netcdf(FOUTNAME, STR4, OM)
        OpenAD_Symbol_640 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_640)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'grid_type'
          STR2 = 'cartesian'
          STR3 = 'f0'
          STR4 = 'beta'
          CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
          CALL add_gattr_netcdf(FOUTNAME, STR3, F0)
          CALL add_gattr_netcdf(FOUTNAME, STR4, BETA)
          OpenAD_Symbol_638 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_638)
        ELSE
          OpenAD_Symbol_639 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_639)
        ENDIF
        OpenAD_Symbol_641 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_641)
      ENDIF
      STR1 = 'r_ini'
      STR2 = 'time_step'
      CALL add_gattr_netcdf(FOUTNAME, STR1, RINI)
      CALL add_gattr_netcdf(FOUTNAME, STR2, DT)
      IF(XPERIODIC) THEN
        STR1 = 'zonal_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        OpenAD_Symbol_642 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_642)
      ELSE
        OpenAD_Symbol_643 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_643)
      ENDIF
      IF(YPERIODIC) THEN
        STR1 = 'meridional_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
        OpenAD_Symbol_644 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_644)
      ELSE
        OpenAD_Symbol_645 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_645)
      ENDIF
      STR1 = 'data_files'
      STR2 = NCDATAFILE // ' ' // DEPTHFILE // ' ' // FORCINGFILE //
     >  ' ' // UINIFILE // ' ' // VINIFILE // ' ' // ETAINIFILE
      CALL add_gatta_netcdf(FOUTNAME, STR1, STR2)
      IF(START_TIME .ne. 0.0D00) THEN
        STR1 = 'restart_file'
        CALL add_gatta_netcdf(FOUTNAME, STR1, NCRESTARTFILE)
        OpenAD_Symbol_646 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_646)
      ELSE
        OpenAD_Symbol_647 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_647)
      ENDIF
      STR1 = 'ntspinup'
      STR2 = 'wf_depth'
      STR3 = 'wf_eta'
      STR4 = 'wf_u'
      CALL add_gatti_netcdf(FOUTNAME, STR1, NTSPINUP)
      CALL add_gattr_netcdf(FOUTNAME, STR2, WF_DEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR3, WF_ETA)
      CALL add_gattr_netcdf(FOUTNAME, STR4, WF_U)
      STR1 = 'wf_v'
      STR2 = 'wf_lapldepth'
      STR3 = 'wf_graddepth'
      STR4 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_V)
      CALL add_gattr_netcdf(FOUTNAME, STR2, WF_LAPLDEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR3, WF_GRADDEPTH)
      CALL add_gattr_netcdf(FOUTNAME, STR4, WF_ZONAL_TRANSPORT)
      OpenAD_Symbol_648 = 0_w2f__i8
      DO IX = 1, 20, 1
        XOUT(INT(IX)) = X(IX)
        OpenAD_Symbol_648 = (INT(OpenAD_Symbol_648) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_648)
      OpenAD_Symbol_649 = 0_w2f__i8
      DO IY = 1, 20, 1
        YOUT(INT(IY)) = Y(IY)
        OpenAD_Symbol_649 = (INT(OpenAD_Symbol_649) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_649)
      IF(SPHERICAL) THEN
        STR1 = 'deg'
        CALL add_coordinates_netcdf(FOUTNAME, MYNX, XOUT, MYNY, YOUT,
     >  STR1)
        OpenAD_Symbol_652 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_652)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'meters'
          CALL add_coordinates_netcdf(FOUTNAME, MYNX, XOUT, MYNY, YOUT,
     >  STR1)
          OpenAD_Symbol_650 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_650)
        ELSE
          OpenAD_Symbol_651 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_651)
        ENDIF
        OpenAD_Symbol_653 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_653)
      ENDIF
      STR1 = 'U'
      STR2 = 'zonal velocity'
      STR3 = 'meters/seconds'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
      STR1 = 'V'
      STR2 = 'meridional velocity'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
      STR1 = 'ETA'
      STR2 = 'sea-surface elevation'
      STR3 = 'meters'
      CALL add_recvar_netcdf(FOUTNAME, STR1, STR2, STR3)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_624)
      IF(OpenAD_Symbol_624 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_625)
        IF(OpenAD_Symbol_625 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_626)
      OpenAD_Symbol_627 = 1
      DO WHILE(INT(OpenAD_Symbol_627) .LE. INT(OpenAD_Symbol_626))
        OpenAD_Symbol_627 = INT(OpenAD_Symbol_627) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_628)
      OpenAD_Symbol_629 = 1
      DO WHILE(INT(OpenAD_Symbol_629) .LE. INT(OpenAD_Symbol_628))
        OpenAD_Symbol_629 = INT(OpenAD_Symbol_629) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_630)
      IF(OpenAD_Symbol_630 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_631)
      IF(OpenAD_Symbol_631 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_632)
      IF(OpenAD_Symbol_632 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_633)
      IF(OpenAD_Symbol_633 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_634)
        IF(OpenAD_Symbol_634 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_635)
      IF(OpenAD_Symbol_635 .ne. 0) THEN
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE state_io(TIME, NIO)
      use w2f__types
      use size
      use parms
      use pfields
      use vars
      use size
      use parms
      use pfields
      use vars
      use size
      use parms
      use pfields
      use vars
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_654
      INTEGER(w2f__i8) OpenAD_Symbol_655
      INTEGER(w2f__i8) OpenAD_Symbol_656
      INTEGER(w2f__i8) OpenAD_Symbol_657
      INTEGER(w2f__i8) OpenAD_Symbol_658
      INTEGER(w2f__i8) OpenAD_Symbol_659
      INTEGER(w2f__i8) OpenAD_Symbol_660
      INTEGER(w2f__i8) OpenAD_Symbol_661
      INTEGER(w2f__i8) OpenAD_Symbol_662
      INTEGER(w2f__i8) OpenAD_Symbol_663
      INTEGER(w2f__i8) OpenAD_Symbol_664
      INTEGER(w2f__i8) OpenAD_Symbol_665
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
      INTEGER(w2f__i4) NIO
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ETAOUT(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      CHARACTER(80) STR1
      CHARACTER(80) STR2
      REAL(w2f__8) UOUT(1 : 20, 1 : 20)
      REAL(w2f__8) VOUT(1 : 20, 1 : 20)
      EXTERNAL write_state_netcdf
      EXTERNAL write_time_netcdf
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          UOUT(INT(IX), INT(IY)) = (__value__(U(IX, IY)) + REAL(INT(
     > UMASK(IX, IY)) +(-1)) * 9.9D+01)
          VOUT(INT(IX), INT(IY)) = (__value__(V(IX, IY)) + REAL(INT(
     > VMASK(IX, IY)) +(-1)) * 9.9D+01)
          ETAOUT(INT(IX), INT(IY)) = (__value__(ETA(IX, IY)) + REAL(INT
     > (ETAMASK(IX, IY)) +(-1)) * 9.9D+01)
        END DO
      END DO
      STR1 = 'TIME'
      STR2 = 'U'
      CALL write_time_netcdf(FOUTNAME, NIO, STR1, TIME)
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR2, UOUT)
      STR1 = 'V'
      STR2 = 'ETA'
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR1, VOUT)
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR2, ETAOUT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_658 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_659 = 0_w2f__i8
        DO IY = 1, 20, 1
          UOUT(INT(IX), INT(IY)) = (__value__(U(IX, IY)) + REAL(INT(
     > UMASK(IX, IY)) +(-1)) * 9.9D+01)
          VOUT(INT(IX), INT(IY)) = (__value__(V(IX, IY)) + REAL(INT(
     > VMASK(IX, IY)) +(-1)) * 9.9D+01)
          ETAOUT(INT(IX), INT(IY)) = (__value__(ETA(IX, IY)) + REAL(INT
     > (ETAMASK(IX, IY)) +(-1)) * 9.9D+01)
          OpenAD_Symbol_659 = (INT(OpenAD_Symbol_659) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_659)
        OpenAD_Symbol_658 = (INT(OpenAD_Symbol_658) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_658)
      STR1 = 'TIME'
      STR2 = 'U'
      CALL write_time_netcdf(FOUTNAME, NIO, STR1, TIME)
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR2, UOUT)
      STR1 = 'V'
      STR2 = 'ETA'
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR1, VOUT)
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR2, ETAOUT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_654)
      OpenAD_Symbol_655 = 1
      DO WHILE(INT(OpenAD_Symbol_655) .LE. INT(OpenAD_Symbol_654))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_656)
        OpenAD_Symbol_657 = 1
        DO WHILE(INT(OpenAD_Symbol_657) .LE. INT(OpenAD_Symbol_656))
          OpenAD_Symbol_657 = INT(OpenAD_Symbol_657) + 1
        END DO
        OpenAD_Symbol_655 = INT(OpenAD_Symbol_655) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_664 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_665 = 0_w2f__i8
        DO IY = 1, 20, 1
          UOUT(INT(IX), INT(IY)) = (__value__(U(IX, IY)) + REAL(INT(
     > UMASK(IX, IY)) +(-1)) * 9.9D+01)
          VOUT(INT(IX), INT(IY)) = (__value__(V(IX, IY)) + REAL(INT(
     > VMASK(IX, IY)) +(-1)) * 9.9D+01)
          ETAOUT(INT(IX), INT(IY)) = (__value__(ETA(IX, IY)) + REAL(INT
     > (ETAMASK(IX, IY)) +(-1)) * 9.9D+01)
          OpenAD_Symbol_665 = (INT(OpenAD_Symbol_665) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_665)
        OpenAD_Symbol_664 = (INT(OpenAD_Symbol_664) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_664)
      STR1 = 'TIME'
      STR2 = 'U'
      CALL write_time_netcdf(FOUTNAME, NIO, STR1, TIME)
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR2, UOUT)
      STR1 = 'V'
      STR2 = 'ETA'
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR1, VOUT)
      CALL write_state_netcdf(FOUTNAME, MYNX, MYNY, NIO, STR2, ETAOUT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_660)
      OpenAD_Symbol_661 = 1
      DO WHILE(INT(OpenAD_Symbol_661) .LE. INT(OpenAD_Symbol_660))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_662)
        OpenAD_Symbol_663 = 1
        DO WHILE(INT(OpenAD_Symbol_663) .LE. INT(OpenAD_Symbol_662))
          OpenAD_Symbol_663 = INT(OpenAD_Symbol_663) + 1
        END DO
        OpenAD_Symbol_661 = INT(OpenAD_Symbol_661) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE pfields_io()
      use w2f__types
      use size
      use parms
      use pfields
      use force
      use size
      use parms
      use pfields
      use force
      use size
      use parms
      use pfields
      use force
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_666
      INTEGER(w2f__i8) OpenAD_Symbol_667
      INTEGER(w2f__i8) OpenAD_Symbol_668
      INTEGER(w2f__i8) OpenAD_Symbol_669
      INTEGER(w2f__i8) OpenAD_Symbol_670
      INTEGER(w2f__i8) OpenAD_Symbol_671
      INTEGER(w2f__i8) OpenAD_Symbol_672
      INTEGER(w2f__i8) OpenAD_Symbol_673
      INTEGER(w2f__i8) OpenAD_Symbol_674
      INTEGER(w2f__i8) OpenAD_Symbol_675
      INTEGER(w2f__i8) OpenAD_Symbol_676
      INTEGER(w2f__i8) OpenAD_Symbol_677
      INTEGER(w2f__i8) OpenAD_Symbol_678
      INTEGER(w2f__i8) OpenAD_Symbol_679
      INTEGER(w2f__i8) OpenAD_Symbol_680
      INTEGER(w2f__i8) OpenAD_Symbol_681
      INTEGER(w2f__i8) OpenAD_Symbol_682
      INTEGER(w2f__i8) OpenAD_Symbol_683
      INTEGER(w2f__i8) OpenAD_Symbol_684
      INTEGER(w2f__i8) OpenAD_Symbol_685
      INTEGER(w2f__i8) OpenAD_Symbol_686
      INTEGER(w2f__i8) OpenAD_Symbol_687
      INTEGER(w2f__i8) OpenAD_Symbol_688
      INTEGER(w2f__i8) OpenAD_Symbol_689
      INTEGER(w2f__i8) OpenAD_Symbol_690
      INTEGER(w2f__i8) OpenAD_Symbol_691
      INTEGER(w2f__i8) OpenAD_Symbol_692
      INTEGER(w2f__i8) OpenAD_Symbol_693
      INTEGER(w2f__i8) OpenAD_Symbol_694
      INTEGER(w2f__i8) OpenAD_Symbol_695
      INTEGER(w2f__i8) OpenAD_Symbol_696
      INTEGER(w2f__i8) OpenAD_Symbol_697
      INTEGER(w2f__i8) OpenAD_Symbol_698
      INTEGER(w2f__i8) OpenAD_Symbol_699
      INTEGER(w2f__i8) OpenAD_Symbol_700
      INTEGER(w2f__i8) OpenAD_Symbol_701
      INTEGER(w2f__i8) OpenAD_Symbol_702
      INTEGER(w2f__i8) OpenAD_Symbol_703
      INTEGER(w2f__i8) OpenAD_Symbol_704
      INTEGER(w2f__i8) OpenAD_Symbol_705
      INTEGER(w2f__i8) OpenAD_Symbol_706
      INTEGER(w2f__i8) OpenAD_Symbol_707
      INTEGER(w2f__i8) OpenAD_Symbol_708
      INTEGER(w2f__i8) OpenAD_Symbol_709
      INTEGER(w2f__i8) OpenAD_Symbol_710
      INTEGER(w2f__i8) OpenAD_Symbol_711
      INTEGER(w2f__i8) OpenAD_Symbol_712
      INTEGER(w2f__i8) OpenAD_Symbol_713
C
C     **** Local Variables and Functions ****
C
      EXTERNAL add_pfield_netcdf
      REAL(w2f__8) AUX(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      CHARACTER(80) STR1
      CHARACTER(80) STR2
      CHARACTER(80) STR3
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = __value__(DEPTH(IX, IY))
        END DO
      END DO
      STR1 = 'depth'
      STR2 = 'water depth'
      STR3 = 'meters'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = UFORCE(IX, IY)
        END DO
      END DO
      STR1 = 'uforce'
      STR2 = 'zonal forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = VFORCE(IX, IY)
        END DO
      END DO
      STR1 = 'vforce'
      STR2 = 'meridional forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = FRICT(IX, IY)
        END DO
      END DO
      STR1 = 'frict'
      STR2 = 'linear bottom friction coefficient'
      STR3 = '1/seconds'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_682 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_683 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = __value__(DEPTH(IX, IY))
          OpenAD_Symbol_683 = (INT(OpenAD_Symbol_683) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_683)
        OpenAD_Symbol_682 = (INT(OpenAD_Symbol_682) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_682)
      STR1 = 'depth'
      STR2 = 'water depth'
      STR3 = 'meters'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      OpenAD_Symbol_684 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_685 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = UFORCE(IX, IY)
          OpenAD_Symbol_685 = (INT(OpenAD_Symbol_685) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_685)
        OpenAD_Symbol_684 = (INT(OpenAD_Symbol_684) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_684)
      STR1 = 'uforce'
      STR2 = 'zonal forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      OpenAD_Symbol_686 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_687 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = VFORCE(IX, IY)
          OpenAD_Symbol_687 = (INT(OpenAD_Symbol_687) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_687)
        OpenAD_Symbol_686 = (INT(OpenAD_Symbol_686) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_686)
      STR1 = 'vforce'
      STR2 = 'meridional forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      OpenAD_Symbol_688 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_689 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = FRICT(IX, IY)
          OpenAD_Symbol_689 = (INT(OpenAD_Symbol_689) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_689)
        OpenAD_Symbol_688 = (INT(OpenAD_Symbol_688) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_688)
      STR1 = 'frict'
      STR2 = 'linear bottom friction coefficient'
      STR3 = '1/seconds'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_666)
      OpenAD_Symbol_667 = 1
      DO WHILE(INT(OpenAD_Symbol_667) .LE. INT(OpenAD_Symbol_666))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_668)
        OpenAD_Symbol_669 = 1
        DO WHILE(INT(OpenAD_Symbol_669) .LE. INT(OpenAD_Symbol_668))
          OpenAD_Symbol_669 = INT(OpenAD_Symbol_669) + 1
        END DO
        OpenAD_Symbol_667 = INT(OpenAD_Symbol_667) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_670)
      OpenAD_Symbol_671 = 1
      DO WHILE(INT(OpenAD_Symbol_671) .LE. INT(OpenAD_Symbol_670))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_672)
        OpenAD_Symbol_673 = 1
        DO WHILE(INT(OpenAD_Symbol_673) .LE. INT(OpenAD_Symbol_672))
          OpenAD_Symbol_673 = INT(OpenAD_Symbol_673) + 1
        END DO
        OpenAD_Symbol_671 = INT(OpenAD_Symbol_671) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_674)
      OpenAD_Symbol_675 = 1
      DO WHILE(INT(OpenAD_Symbol_675) .LE. INT(OpenAD_Symbol_674))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_676)
        OpenAD_Symbol_677 = 1
        DO WHILE(INT(OpenAD_Symbol_677) .LE. INT(OpenAD_Symbol_676))
          OpenAD_Symbol_677 = INT(OpenAD_Symbol_677) + 1
        END DO
        OpenAD_Symbol_675 = INT(OpenAD_Symbol_675) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_678)
      OpenAD_Symbol_679 = 1
      DO WHILE(INT(OpenAD_Symbol_679) .LE. INT(OpenAD_Symbol_678))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_680)
        OpenAD_Symbol_681 = 1
        DO WHILE(INT(OpenAD_Symbol_681) .LE. INT(OpenAD_Symbol_680))
          OpenAD_Symbol_681 = INT(OpenAD_Symbol_681) + 1
        END DO
        OpenAD_Symbol_679 = INT(OpenAD_Symbol_679) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_706 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_707 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = __value__(DEPTH(IX, IY))
          OpenAD_Symbol_707 = (INT(OpenAD_Symbol_707) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_707)
        OpenAD_Symbol_706 = (INT(OpenAD_Symbol_706) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_706)
      STR1 = 'depth'
      STR2 = 'water depth'
      STR3 = 'meters'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      OpenAD_Symbol_708 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_709 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = UFORCE(IX, IY)
          OpenAD_Symbol_709 = (INT(OpenAD_Symbol_709) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_709)
        OpenAD_Symbol_708 = (INT(OpenAD_Symbol_708) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_708)
      STR1 = 'uforce'
      STR2 = 'zonal forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      OpenAD_Symbol_710 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_711 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = VFORCE(IX, IY)
          OpenAD_Symbol_711 = (INT(OpenAD_Symbol_711) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_711)
        OpenAD_Symbol_710 = (INT(OpenAD_Symbol_710) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_710)
      STR1 = 'vforce'
      STR2 = 'meridional forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
      OpenAD_Symbol_712 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_713 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = FRICT(IX, IY)
          OpenAD_Symbol_713 = (INT(OpenAD_Symbol_713) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_713)
        OpenAD_Symbol_712 = (INT(OpenAD_Symbol_712) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_712)
      STR1 = 'frict'
      STR2 = 'linear bottom friction coefficient'
      STR3 = '1/seconds'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, STR1, STR2,
     >  STR3)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_690)
      OpenAD_Symbol_691 = 1
      DO WHILE(INT(OpenAD_Symbol_691) .LE. INT(OpenAD_Symbol_690))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_692)
        OpenAD_Symbol_693 = 1
        DO WHILE(INT(OpenAD_Symbol_693) .LE. INT(OpenAD_Symbol_692))
          OpenAD_Symbol_693 = INT(OpenAD_Symbol_693) + 1
        END DO
        OpenAD_Symbol_691 = INT(OpenAD_Symbol_691) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_694)
      OpenAD_Symbol_695 = 1
      DO WHILE(INT(OpenAD_Symbol_695) .LE. INT(OpenAD_Symbol_694))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_696)
        OpenAD_Symbol_697 = 1
        DO WHILE(INT(OpenAD_Symbol_697) .LE. INT(OpenAD_Symbol_696))
          OpenAD_Symbol_697 = INT(OpenAD_Symbol_697) + 1
        END DO
        OpenAD_Symbol_695 = INT(OpenAD_Symbol_695) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_698)
      OpenAD_Symbol_699 = 1
      DO WHILE(INT(OpenAD_Symbol_699) .LE. INT(OpenAD_Symbol_698))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_700)
        OpenAD_Symbol_701 = 1
        DO WHILE(INT(OpenAD_Symbol_701) .LE. INT(OpenAD_Symbol_700))
          OpenAD_Symbol_701 = INT(OpenAD_Symbol_701) + 1
        END DO
        OpenAD_Symbol_699 = INT(OpenAD_Symbol_699) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_702)
      OpenAD_Symbol_703 = 1
      DO WHILE(INT(OpenAD_Symbol_703) .LE. INT(OpenAD_Symbol_702))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_704)
        OpenAD_Symbol_705 = 1
        DO WHILE(INT(OpenAD_Symbol_705) .LE. INT(OpenAD_Symbol_704))
          OpenAD_Symbol_705 = INT(OpenAD_Symbol_705) + 1
        END DO
        OpenAD_Symbol_703 = INT(OpenAD_Symbol_703) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE save_gradient_io(N, ADXC, GNAME)
      use w2f__types
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) N
      REAL(w2f__8) ADXC(1 : N)
      CHARACTER(80) GNAME
C
C     **** Local Variables and Functions ****
C
      EXTERNAL add_pfield_netcdf
      REAL(w2f__8) GRAD(1 : 20, 1 : 20)
      EXTERNAL map_gradient
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      CHARACTER(80) STR1
      CHARACTER(80) STR2
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL map_gradient(N, ADXC, GRAD)
      STR1 = 'gradient of cost function with respect to depth'
      STR2 = 'cost function units/m'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, GRAD, GNAME, STR1,
     >  STR2)
      SUPPRESSIO = .TRUE.
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL map_gradient(N, ADXC, GRAD)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(N)
      STR1 = 'gradient of cost function with respect to depth'
      STR2 = 'cost function units/m'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, GRAD, GNAME, STR1,
     >  STR2)
      SUPPRESSIO = .TRUE.
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(N)
      CALL map_gradient(N, ADXC, GRAD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL map_gradient(N, ADXC, GRAD)
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(N)
      STR1 = 'gradient of cost function with respect to depth'
      STR2 = 'cost function units/m'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, GRAD, GNAME, STR1,
     >  STR2)
      SUPPRESSIO = .TRUE.
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(N)
      CALL map_gradient(N, ADXC, GRAD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE save_depth_io(N, XC, DNAME)
      use w2f__types
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      use size
      use parms
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_738
      INTEGER(w2f__i8) OpenAD_Symbol_739
      INTEGER(w2f__i8) OpenAD_Symbol_740
      INTEGER(w2f__i8) OpenAD_Symbol_741
      INTEGER(w2f__i8) OpenAD_Symbol_742
      INTEGER(w2f__i8) OpenAD_Symbol_743
      INTEGER(w2f__i8) OpenAD_Symbol_744
      INTEGER(w2f__i8) OpenAD_Symbol_745
      INTEGER(w2f__i8) OpenAD_Symbol_746
      INTEGER(w2f__i8) OpenAD_Symbol_747
      INTEGER(w2f__i8) OpenAD_Symbol_748
      INTEGER(w2f__i8) OpenAD_Symbol_749
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) N
      REAL(w2f__8) XC(1 : N)
      CHARACTER(80) DNAME
C
C     **** Local Variables and Functions ****
C
      EXTERNAL add_pfield_netcdf
      REAL(w2f__8) AUX(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      EXTERNAL map_from_control_vector
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      CHARACTER(80) STR1
      CHARACTER(80) STR2
      TYPE (oadactive) OpenAD_tyc_1(:)
      ALLOCATABLE OpenAD_tyc_1
      TYPE (oadactive) OpenAD_tyc_7(:)
      ALLOCATABLE OpenAD_tyc_7
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
C     $OpenAD$ INLINE oad_AllocateMatching(subst,subst)
      CALL oad_AllocateMatching(__deriv__(OpenAD_tyc_1), XC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_1), XC)
      CALL map_from_control_vector(N, __deriv__(OpenAD_tyc_1))
C     $OpenAD$ INLINE oad_ShapeTest(subst,subst)
      CALL oad_ShapeTest(__deriv__(OpenAD_tyc_1), XC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(XC, __deriv__(OpenAD_tyc_1))
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = __value__(DEPTH(IX, IY))
        END DO
      END DO
      STR1 = 'water depth after optimization'
      STR2 = 'm'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, DNAME, STR1,
     >  STR2)
      SUPPRESSIO = .TRUE.
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
C     $OpenAD$ INLINE oad_AllocateMatching(subst,subst)
      CALL oad_AllocateMatching(__deriv__(OpenAD_tyc_1), XC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_1), XC)
      CALL map_from_control_vector(N, __deriv__(OpenAD_tyc_1))
C     $OpenAD$ INLINE oad_ShapeTest(subst,subst)
      CALL oad_ShapeTest(__deriv__(OpenAD_tyc_1), XC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(XC, __deriv__(OpenAD_tyc_1))
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(N)
      OpenAD_Symbol_742 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_743 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = __value__(DEPTH(IX, IY))
          OpenAD_Symbol_743 = (INT(OpenAD_Symbol_743) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_743)
        OpenAD_Symbol_742 = (INT(OpenAD_Symbol_742) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_742)
      STR1 = 'water depth after optimization'
      STR2 = 'm'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, DNAME, STR1,
     >  STR2)
      SUPPRESSIO = .TRUE.
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_738)
      OpenAD_Symbol_739 = 1
      DO WHILE(INT(OpenAD_Symbol_739) .LE. INT(OpenAD_Symbol_738))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_740)
        OpenAD_Symbol_741 = 1
        DO WHILE(INT(OpenAD_Symbol_741) .LE. INT(OpenAD_Symbol_740))
          OpenAD_Symbol_741 = INT(OpenAD_Symbol_741) + 1
        END DO
        OpenAD_Symbol_739 = INT(OpenAD_Symbol_739) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(N)
C     $OpenAD$ INLINE oad_AllocateMatching(subst,subst)
      CALL oad_AllocateMatching(__deriv__(OpenAD_tyc_7), XC)
      CALL map_from_control_vector(N, __deriv__(OpenAD_tyc_7))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
C     $OpenAD$ INLINE oad_AllocateMatching(subst,subst)
      CALL oad_AllocateMatching(__deriv__(OpenAD_tyc_1), XC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_1), XC)
      CALL map_from_control_vector(N, __deriv__(OpenAD_tyc_1))
C     $OpenAD$ INLINE oad_ShapeTest(subst,subst)
      CALL oad_ShapeTest(__deriv__(OpenAD_tyc_1), XC)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(XC, __deriv__(OpenAD_tyc_1))
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(N)
      OpenAD_Symbol_748 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_749 = 0_w2f__i8
        DO IY = 1, 20, 1
          AUX(INT(IX), INT(IY)) = __value__(DEPTH(IX, IY))
          OpenAD_Symbol_749 = (INT(OpenAD_Symbol_749) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_749)
        OpenAD_Symbol_748 = (INT(OpenAD_Symbol_748) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_748)
      STR1 = 'water depth after optimization'
      STR2 = 'm'
      CALL add_pfield_netcdf(FOUTNAME, MYNX, MYNY, AUX, DNAME, STR1,
     >  STR2)
      SUPPRESSIO = .TRUE.
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_744)
      OpenAD_Symbol_745 = 1
      DO WHILE(INT(OpenAD_Symbol_745) .LE. INT(OpenAD_Symbol_744))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_746)
        OpenAD_Symbol_747 = 1
        DO WHILE(INT(OpenAD_Symbol_747) .LE. INT(OpenAD_Symbol_746))
          OpenAD_Symbol_747 = INT(OpenAD_Symbol_747) + 1
        END DO
        OpenAD_Symbol_745 = INT(OpenAD_Symbol_745) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(N)
C     $OpenAD$ INLINE oad_AllocateMatching(subst,subst)
      CALL oad_AllocateMatching(__deriv__(OpenAD_tyc_7), XC)
      CALL map_from_control_vector(N, __deriv__(OpenAD_tyc_7))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE inimini_io()
      use w2f__types
      IMPLICIT NONE
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE save_weights_io()
      use w2f__types
      use size
      use parms
      use weights
      use size
      use parms
      use weights
      use size
      use parms
      use weights
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      EXTERNAL add_gattr_netcdf
      CHARACTER(80) STR1
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      STR1 = 'wf_depth'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_DEPTH)
      STR1 = 'wf_eta'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_ETA)
      STR1 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      STR1 = 'wf_depth'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_DEPTH)
      STR1 = 'wf_eta'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_ETA)
      STR1 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      STR1 = 'wf_depth'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_DEPTH)
      STR1 = 'wf_eta'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_ETA)
      STR1 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME, STR1, WF_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_data(TIME)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_eta_data(TIME)
      use w2f__types
      use size
      use data
      use parms
      use size
      use data
      use parms
      use size
      use data
      use parms
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_774
      INTEGER(w2f__i8) OpenAD_Symbol_775
      INTEGER(w2f__i8) OpenAD_Symbol_776
      INTEGER(w2f__i8) OpenAD_Symbol_777
      INTEGER(w2f__i8) OpenAD_Symbol_778
      INTEGER(w2f__i8) OpenAD_Symbol_779
      INTEGER(w2f__i8) OpenAD_Symbol_780
      INTEGER(w2f__i8) OpenAD_Symbol_781
      INTEGER(w2f__i8) OpenAD_Symbol_782
      INTEGER(w2f__i8) OpenAD_Symbol_783
      INTEGER(w2f__i8) OpenAD_Symbol_784
      INTEGER(w2f__i8) OpenAD_Symbol_785
      INTEGER(w2f__i8) OpenAD_Symbol_786
      INTEGER(w2f__i8) OpenAD_Symbol_787
      INTEGER(w2f__i8) OpenAD_Symbol_788
      INTEGER(w2f__i8) OpenAD_Symbol_789
      INTEGER(w2f__i8) OpenAD_Symbol_790
      INTEGER(w2f__i8) OpenAD_Symbol_791
      INTEGER(w2f__i8) OpenAD_Symbol_792
      INTEGER(w2f__i8) OpenAD_Symbol_793
      INTEGER(w2f__i8) OpenAD_Symbol_794
      INTEGER(w2f__i8) OpenAD_Symbol_795
      INTEGER(w2f__i8) OpenAD_Symbol_796
      INTEGER(w2f__i8) OpenAD_Symbol_797
      INTEGER(w2f__i8) OpenAD_Symbol_798
      INTEGER(w2f__i8) OpenAD_Symbol_799
      INTEGER(w2f__i8) OpenAD_Symbol_800
      INTEGER(w2f__i8) OpenAD_Symbol_801
      INTEGER(w2f__i8) OpenAD_Symbol_802
      INTEGER(w2f__i8) OpenAD_Symbol_803
      INTEGER(w2f__i8) OpenAD_Symbol_804
      INTEGER(w2f__i8) OpenAD_Symbol_805
      INTEGER(w2f__i8) OpenAD_Symbol_806
      INTEGER(w2f__i8) OpenAD_Symbol_807
      INTEGER(w2f__i8) OpenAD_Symbol_808
      INTEGER(w2f__i8) OpenAD_Symbol_809
      INTEGER(w2f__i8) OpenAD_Symbol_810
      INTEGER(w2f__i8) OpenAD_Symbol_811
      INTEGER(w2f__i8) OpenAD_Symbol_812
      INTEGER(w2f__i8) OpenAD_Symbol_813
      INTEGER(w2f__i8) OpenAD_Symbol_814
      INTEGER(w2f__i8) OpenAD_Symbol_815
      INTEGER(w2f__i8) OpenAD_Symbol_816
      INTEGER(w2f__i8) OpenAD_Symbol_817
      INTEGER(w2f__i8) OpenAD_Symbol_818
      INTEGER(w2f__i8) OpenAD_Symbol_819
      INTEGER(w2f__i8) OpenAD_Symbol_820
      INTEGER(w2f__i8) OpenAD_Symbol_821
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) F_IN(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      CHARACTER(80) MYETA
      CHARACTER(80) MYETADATA
      INTEGER(w2f__i4) MYNX
      INTEGER(w2f__i4) MYNY
      EXTERNAL read_field_netcdf
      EXTERNAL read_snap_netcdf
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYETADATA = 'etadata'
      MYETA = 'eta'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, MYETADATA, MYNX, MYNY, F_IN)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
          END DO
        END DO
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, MYETA,
     >  F_IN)
          DO IY = 1, 20, 1
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            END DO
          END DO
        ELSE
          DO IY = 1, 20, 1
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = 0.0D00
            END DO
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYETADATA = 'etadata'
      MYETA = 'eta'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, MYETADATA, MYNX, MYNY, F_IN)
        OpenAD_Symbol_788 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_789 = 0_w2f__i8
          DO IX = 1, 20, 1
            ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_789 = (INT(OpenAD_Symbol_789) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_789)
          OpenAD_Symbol_788 = (INT(OpenAD_Symbol_788) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_788)
        OpenAD_Symbol_796 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_796)
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, MYETA,
     >  F_IN)
          OpenAD_Symbol_790 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_791 = 0_w2f__i8
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_791 = (INT(OpenAD_Symbol_791) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_791)
            OpenAD_Symbol_790 = (INT(OpenAD_Symbol_790) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_790)
          OpenAD_Symbol_794 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_794)
        ELSE
          OpenAD_Symbol_792 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_793 = 0_w2f__i8
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = 0.0D00
              OpenAD_Symbol_793 = (INT(OpenAD_Symbol_793) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_793)
            OpenAD_Symbol_792 = (INT(OpenAD_Symbol_792) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_792)
          OpenAD_Symbol_795 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_795)
        ENDIF
        OpenAD_Symbol_797 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_797)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_774)
      IF(OpenAD_Symbol_774 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_784)
        OpenAD_Symbol_785 = 1
        DO WHILE(INT(OpenAD_Symbol_785) .LE. INT(OpenAD_Symbol_784))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_786)
          OpenAD_Symbol_787 = 1
          DO WHILE(INT(OpenAD_Symbol_787) .LE. INT(OpenAD_Symbol_786))
            OpenAD_Symbol_787 = INT(OpenAD_Symbol_787) + 1
          END DO
          OpenAD_Symbol_785 = INT(OpenAD_Symbol_785) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_775)
        IF(OpenAD_Symbol_775 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_780)
          OpenAD_Symbol_781 = 1
          DO WHILE(INT(OpenAD_Symbol_781) .LE. INT(OpenAD_Symbol_780))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_782)
            OpenAD_Symbol_783 = 1
            DO WHILE(INT(OpenAD_Symbol_783) .LE. INT(OpenAD_Symbol_782)
     > )
              OpenAD_Symbol_783 = INT(OpenAD_Symbol_783) + 1
            END DO
            OpenAD_Symbol_781 = INT(OpenAD_Symbol_781) + 1
          END DO
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_776)
          OpenAD_Symbol_777 = 1
          DO WHILE(INT(OpenAD_Symbol_777) .LE. INT(OpenAD_Symbol_776))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_778)
            OpenAD_Symbol_779 = 1
            DO WHILE(INT(OpenAD_Symbol_779) .LE. INT(OpenAD_Symbol_778)
     > )
              OpenAD_Symbol_779 = INT(OpenAD_Symbol_779) + 1
            END DO
            OpenAD_Symbol_777 = INT(OpenAD_Symbol_777) + 1
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYETADATA = 'etadata'
      MYETA = 'eta'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, MYETADATA, MYNX, MYNY, F_IN)
        OpenAD_Symbol_812 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_813 = 0_w2f__i8
          DO IX = 1, 20, 1
            ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_813 = (INT(OpenAD_Symbol_813) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_813)
          OpenAD_Symbol_812 = (INT(OpenAD_Symbol_812) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_812)
        OpenAD_Symbol_820 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_820)
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, MYETA,
     >  F_IN)
          OpenAD_Symbol_814 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_815 = 0_w2f__i8
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_815 = (INT(OpenAD_Symbol_815) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_815)
            OpenAD_Symbol_814 = (INT(OpenAD_Symbol_814) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_814)
          OpenAD_Symbol_818 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_818)
        ELSE
          OpenAD_Symbol_816 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_817 = 0_w2f__i8
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = 0.0D00
              OpenAD_Symbol_817 = (INT(OpenAD_Symbol_817) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_817)
            OpenAD_Symbol_816 = (INT(OpenAD_Symbol_816) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_816)
          OpenAD_Symbol_819 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_819)
        ENDIF
        OpenAD_Symbol_821 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_821)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_798)
      IF(OpenAD_Symbol_798 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_808)
        OpenAD_Symbol_809 = 1
        DO WHILE(INT(OpenAD_Symbol_809) .LE. INT(OpenAD_Symbol_808))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_810)
          OpenAD_Symbol_811 = 1
          DO WHILE(INT(OpenAD_Symbol_811) .LE. INT(OpenAD_Symbol_810))
            OpenAD_Symbol_811 = INT(OpenAD_Symbol_811) + 1
          END DO
          OpenAD_Symbol_809 = INT(OpenAD_Symbol_809) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_799)
        IF(OpenAD_Symbol_799 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_804)
          OpenAD_Symbol_805 = 1
          DO WHILE(INT(OpenAD_Symbol_805) .LE. INT(OpenAD_Symbol_804))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_806)
            OpenAD_Symbol_807 = 1
            DO WHILE(INT(OpenAD_Symbol_807) .LE. INT(OpenAD_Symbol_806)
     > )
              OpenAD_Symbol_807 = INT(OpenAD_Symbol_807) + 1
            END DO
            OpenAD_Symbol_805 = INT(OpenAD_Symbol_805) + 1
          END DO
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_800)
          OpenAD_Symbol_801 = 1
          DO WHILE(INT(OpenAD_Symbol_801) .LE. INT(OpenAD_Symbol_800))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_802)
            OpenAD_Symbol_803 = 1
            DO WHILE(INT(OpenAD_Symbol_803) .LE. INT(OpenAD_Symbol_802)
     > )
              OpenAD_Symbol_803 = INT(OpenAD_Symbol_803) + 1
            END DO
            OpenAD_Symbol_801 = INT(OpenAD_Symbol_801) + 1
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_uv_data(TIME)
      use w2f__types
      use size
      use data
      use parms
      use size
      use data
      use parms
      use size
      use data
      use parms
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_822
      INTEGER(w2f__i8) OpenAD_Symbol_823
      INTEGER(w2f__i8) OpenAD_Symbol_824
      INTEGER(w2f__i8) OpenAD_Symbol_825
      INTEGER(w2f__i8) OpenAD_Symbol_826
      INTEGER(w2f__i8) OpenAD_Symbol_827
      INTEGER(w2f__i8) OpenAD_Symbol_828
      INTEGER(w2f__i8) OpenAD_Symbol_829
      INTEGER(w2f__i8) OpenAD_Symbol_830
      INTEGER(w2f__i8) OpenAD_Symbol_831
      INTEGER(w2f__i8) OpenAD_Symbol_832
      INTEGER(w2f__i8) OpenAD_Symbol_833
      INTEGER(w2f__i8) OpenAD_Symbol_834
      INTEGER(w2f__i8) OpenAD_Symbol_835
      INTEGER(w2f__i8) OpenAD_Symbol_836
      INTEGER(w2f__i8) OpenAD_Symbol_837
      INTEGER(w2f__i8) OpenAD_Symbol_838
      INTEGER(w2f__i8) OpenAD_Symbol_839
      INTEGER(w2f__i8) OpenAD_Symbol_840
      INTEGER(w2f__i8) OpenAD_Symbol_841
      INTEGER(w2f__i8) OpenAD_Symbol_842
      INTEGER(w2f__i8) OpenAD_Symbol_843
      INTEGER(w2f__i8) OpenAD_Symbol_844
      INTEGER(w2f__i8) OpenAD_Symbol_845
      INTEGER(w2f__i8) OpenAD_Symbol_846
      INTEGER(w2f__i8) OpenAD_Symbol_847
      INTEGER(w2f__i8) OpenAD_Symbol_848
      INTEGER(w2f__i8) OpenAD_Symbol_849
      INTEGER(w2f__i8) OpenAD_Symbol_850
      INTEGER(w2f__i8) OpenAD_Symbol_851
      INTEGER(w2f__i8) OpenAD_Symbol_852
      INTEGER(w2f__i8) OpenAD_Symbol_853
      INTEGER(w2f__i8) OpenAD_Symbol_854
      INTEGER(w2f__i8) OpenAD_Symbol_855
      INTEGER(w2f__i8) OpenAD_Symbol_856
      INTEGER(w2f__i8) OpenAD_Symbol_857
      INTEGER(w2f__i8) OpenAD_Symbol_858
      INTEGER(w2f__i8) OpenAD_Symbol_859
      INTEGER(w2f__i8) OpenAD_Symbol_860
      INTEGER(w2f__i8) OpenAD_Symbol_861
      INTEGER(w2f__i8) OpenAD_Symbol_862
      INTEGER(w2f__i8) OpenAD_Symbol_863
      INTEGER(w2f__i8) OpenAD_Symbol_864
      INTEGER(w2f__i8) OpenAD_Symbol_865
      INTEGER(w2f__i8) OpenAD_Symbol_866
      INTEGER(w2f__i8) OpenAD_Symbol_867
      INTEGER(w2f__i8) OpenAD_Symbol_868
      INTEGER(w2f__i8) OpenAD_Symbol_869
      INTEGER(w2f__i8) OpenAD_Symbol_870
      INTEGER(w2f__i8) OpenAD_Symbol_871
      INTEGER(w2f__i8) OpenAD_Symbol_872
      INTEGER(w2f__i8) OpenAD_Symbol_873
      INTEGER(w2f__i8) OpenAD_Symbol_874
      INTEGER(w2f__i8) OpenAD_Symbol_875
      INTEGER(w2f__i8) OpenAD_Symbol_876
      INTEGER(w2f__i8) OpenAD_Symbol_877
      INTEGER(w2f__i8) OpenAD_Symbol_878
      INTEGER(w2f__i8) OpenAD_Symbol_879
      INTEGER(w2f__i8) OpenAD_Symbol_880
      INTEGER(w2f__i8) OpenAD_Symbol_881
      INTEGER(w2f__i8) OpenAD_Symbol_882
      INTEGER(w2f__i8) OpenAD_Symbol_883
      INTEGER(w2f__i8) OpenAD_Symbol_884
      INTEGER(w2f__i8) OpenAD_Symbol_885
      INTEGER(w2f__i8) OpenAD_Symbol_886
      INTEGER(w2f__i8) OpenAD_Symbol_887
      INTEGER(w2f__i8) OpenAD_Symbol_888
      INTEGER(w2f__i8) OpenAD_Symbol_889
      INTEGER(w2f__i8) OpenAD_Symbol_890
      INTEGER(w2f__i8) OpenAD_Symbol_891
      INTEGER(w2f__i8) OpenAD_Symbol_892
      INTEGER(w2f__i8) OpenAD_Symbol_893
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) F_IN(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) MYNX
      SAVE MYNX
      INTEGER(w2f__i4) MYNY
      SAVE MYNY
      EXTERNAL read_field_netcdf
      EXTERNAL read_snap_netcdf
      CHARACTER(80) STRU
      CHARACTER(80) STRUDATA
      CHARACTER(80) STRV
      CHARACTER(80) STRVDATA
C
C     **** Initializers ****
C
      DATA MYNX / 20 /
      DATA MYNY / 20 /
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRUDATA = 'udata'
      STRVDATA = 'vdata'
      STRU = 'U'
      STRV = 'V'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, STRUDATA, MYNX, MYNY, F_IN)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
          END DO
        END DO
        CALL read_field_netcdf(NCDATAFILE, STRVDATA, MYNX, MYNY, F_IN)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
          END DO
        END DO
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, STRU,
     >  F_IN)
          DO IY = 1, 20, 1
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            END DO
          END DO
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, STRV,
     >  F_IN)
          DO IY = 1, 20, 1
            DO IX = 1, 20, 1
              V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            END DO
          END DO
        ELSE
          DO IY = 1, 20, 1
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = 0.0D00
              V_DATA(INT(IX), INT(IY)) = 0.0D00
            END DO
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRUDATA = 'udata'
      STRVDATA = 'vdata'
      STRU = 'U'
      STRV = 'V'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, STRUDATA, MYNX, MYNY, F_IN)
        OpenAD_Symbol_844 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_845 = 0_w2f__i8
          DO IX = 1, 20, 1
            U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_845 = (INT(OpenAD_Symbol_845) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_845)
          OpenAD_Symbol_844 = (INT(OpenAD_Symbol_844) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_844)
        CALL read_field_netcdf(NCDATAFILE, STRVDATA, MYNX, MYNY, F_IN)
        OpenAD_Symbol_846 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_847 = 0_w2f__i8
          DO IX = 1, 20, 1
            V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_847 = (INT(OpenAD_Symbol_847) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_847)
          OpenAD_Symbol_846 = (INT(OpenAD_Symbol_846) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_846)
        OpenAD_Symbol_856 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_856)
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, STRU,
     >  F_IN)
          OpenAD_Symbol_848 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_849 = 0_w2f__i8
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_849 = (INT(OpenAD_Symbol_849) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_849)
            OpenAD_Symbol_848 = (INT(OpenAD_Symbol_848) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_848)
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, STRV,
     >  F_IN)
          OpenAD_Symbol_850 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_851 = 0_w2f__i8
            DO IX = 1, 20, 1
              V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_851 = (INT(OpenAD_Symbol_851) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_851)
            OpenAD_Symbol_850 = (INT(OpenAD_Symbol_850) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_850)
          OpenAD_Symbol_854 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_854)
        ELSE
          OpenAD_Symbol_852 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_853 = 0_w2f__i8
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = 0.0D00
              V_DATA(INT(IX), INT(IY)) = 0.0D00
              OpenAD_Symbol_853 = (INT(OpenAD_Symbol_853) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_853)
            OpenAD_Symbol_852 = (INT(OpenAD_Symbol_852) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_852)
          OpenAD_Symbol_855 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_855)
        ENDIF
        OpenAD_Symbol_857 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_857)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_822)
      IF(OpenAD_Symbol_822 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_836)
        OpenAD_Symbol_837 = 1
        DO WHILE(INT(OpenAD_Symbol_837) .LE. INT(OpenAD_Symbol_836))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_838)
          OpenAD_Symbol_839 = 1
          DO WHILE(INT(OpenAD_Symbol_839) .LE. INT(OpenAD_Symbol_838))
            OpenAD_Symbol_839 = INT(OpenAD_Symbol_839) + 1
          END DO
          OpenAD_Symbol_837 = INT(OpenAD_Symbol_837) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_840)
        OpenAD_Symbol_841 = 1
        DO WHILE(INT(OpenAD_Symbol_841) .LE. INT(OpenAD_Symbol_840))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_842)
          OpenAD_Symbol_843 = 1
          DO WHILE(INT(OpenAD_Symbol_843) .LE. INT(OpenAD_Symbol_842))
            OpenAD_Symbol_843 = INT(OpenAD_Symbol_843) + 1
          END DO
          OpenAD_Symbol_841 = INT(OpenAD_Symbol_841) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_823)
        IF(OpenAD_Symbol_823 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_828)
          OpenAD_Symbol_829 = 1
          DO WHILE(INT(OpenAD_Symbol_829) .LE. INT(OpenAD_Symbol_828))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_830)
            OpenAD_Symbol_831 = 1
            DO WHILE(INT(OpenAD_Symbol_831) .LE. INT(OpenAD_Symbol_830)
     > )
              OpenAD_Symbol_831 = INT(OpenAD_Symbol_831) + 1
            END DO
            OpenAD_Symbol_829 = INT(OpenAD_Symbol_829) + 1
          END DO
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_832)
          OpenAD_Symbol_833 = 1
          DO WHILE(INT(OpenAD_Symbol_833) .LE. INT(OpenAD_Symbol_832))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_834)
            OpenAD_Symbol_835 = 1
            DO WHILE(INT(OpenAD_Symbol_835) .LE. INT(OpenAD_Symbol_834)
     > )
              OpenAD_Symbol_835 = INT(OpenAD_Symbol_835) + 1
            END DO
            OpenAD_Symbol_833 = INT(OpenAD_Symbol_833) + 1
          END DO
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_824)
          OpenAD_Symbol_825 = 1
          DO WHILE(INT(OpenAD_Symbol_825) .LE. INT(OpenAD_Symbol_824))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_826)
            OpenAD_Symbol_827 = 1
            DO WHILE(INT(OpenAD_Symbol_827) .LE. INT(OpenAD_Symbol_826)
     > )
              OpenAD_Symbol_827 = INT(OpenAD_Symbol_827) + 1
            END DO
            OpenAD_Symbol_825 = INT(OpenAD_Symbol_825) + 1
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRUDATA = 'udata'
      STRVDATA = 'vdata'
      STRU = 'U'
      STRV = 'V'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE, STRUDATA, MYNX, MYNY, F_IN)
        OpenAD_Symbol_880 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_881 = 0_w2f__i8
          DO IX = 1, 20, 1
            U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_881 = (INT(OpenAD_Symbol_881) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_881)
          OpenAD_Symbol_880 = (INT(OpenAD_Symbol_880) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_880)
        CALL read_field_netcdf(NCDATAFILE, STRVDATA, MYNX, MYNY, F_IN)
        OpenAD_Symbol_882 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_883 = 0_w2f__i8
          DO IX = 1, 20, 1
            V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_883 = (INT(OpenAD_Symbol_883) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_883)
          OpenAD_Symbol_882 = (INT(OpenAD_Symbol_882) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_882)
        OpenAD_Symbol_892 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_892)
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, STRU,
     >  F_IN)
          OpenAD_Symbol_884 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_885 = 0_w2f__i8
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_885 = (INT(OpenAD_Symbol_885) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_885)
            OpenAD_Symbol_884 = (INT(OpenAD_Symbol_884) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_884)
          CALL read_snap_netcdf(NCDATAFILE, TIME, MYNX, MYNY, STRV,
     >  F_IN)
          OpenAD_Symbol_886 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_887 = 0_w2f__i8
            DO IX = 1, 20, 1
              V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_887 = (INT(OpenAD_Symbol_887) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_887)
            OpenAD_Symbol_886 = (INT(OpenAD_Symbol_886) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_886)
          OpenAD_Symbol_890 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_890)
        ELSE
          OpenAD_Symbol_888 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_889 = 0_w2f__i8
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = 0.0D00
              V_DATA(INT(IX), INT(IY)) = 0.0D00
              OpenAD_Symbol_889 = (INT(OpenAD_Symbol_889) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_889)
            OpenAD_Symbol_888 = (INT(OpenAD_Symbol_888) + INT(1_w2f__i8
     > ))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_888)
          OpenAD_Symbol_891 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_891)
        ENDIF
        OpenAD_Symbol_893 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_893)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_858)
      IF(OpenAD_Symbol_858 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_872)
        OpenAD_Symbol_873 = 1
        DO WHILE(INT(OpenAD_Symbol_873) .LE. INT(OpenAD_Symbol_872))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_874)
          OpenAD_Symbol_875 = 1
          DO WHILE(INT(OpenAD_Symbol_875) .LE. INT(OpenAD_Symbol_874))
            OpenAD_Symbol_875 = INT(OpenAD_Symbol_875) + 1
          END DO
          OpenAD_Symbol_873 = INT(OpenAD_Symbol_873) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_876)
        OpenAD_Symbol_877 = 1
        DO WHILE(INT(OpenAD_Symbol_877) .LE. INT(OpenAD_Symbol_876))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_878)
          OpenAD_Symbol_879 = 1
          DO WHILE(INT(OpenAD_Symbol_879) .LE. INT(OpenAD_Symbol_878))
            OpenAD_Symbol_879 = INT(OpenAD_Symbol_879) + 1
          END DO
          OpenAD_Symbol_877 = INT(OpenAD_Symbol_877) + 1
        END DO
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_859)
        IF(OpenAD_Symbol_859 .ne. 0) THEN
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_864)
          OpenAD_Symbol_865 = 1
          DO WHILE(INT(OpenAD_Symbol_865) .LE. INT(OpenAD_Symbol_864))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_866)
            OpenAD_Symbol_867 = 1
            DO WHILE(INT(OpenAD_Symbol_867) .LE. INT(OpenAD_Symbol_866)
     > )
              OpenAD_Symbol_867 = INT(OpenAD_Symbol_867) + 1
            END DO
            OpenAD_Symbol_865 = INT(OpenAD_Symbol_865) + 1
          END DO
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_868)
          OpenAD_Symbol_869 = 1
          DO WHILE(INT(OpenAD_Symbol_869) .LE. INT(OpenAD_Symbol_868))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_870)
            OpenAD_Symbol_871 = 1
            DO WHILE(INT(OpenAD_Symbol_871) .LE. INT(OpenAD_Symbol_870)
     > )
              OpenAD_Symbol_871 = INT(OpenAD_Symbol_871) + 1
            END DO
            OpenAD_Symbol_869 = INT(OpenAD_Symbol_869) + 1
          END DO
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_860)
          OpenAD_Symbol_861 = 1
          DO WHILE(INT(OpenAD_Symbol_861) .LE. INT(OpenAD_Symbol_860))
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_862)
            OpenAD_Symbol_863 = 1
            DO WHILE(INT(OpenAD_Symbol_863) .LE. INT(OpenAD_Symbol_862)
     > )
              OpenAD_Symbol_863 = INT(OpenAD_Symbol_863) + 1
            END DO
            OpenAD_Symbol_861 = INT(OpenAD_Symbol_861) + 1
          END DO
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_zonal_transport_data(TIME)
      use w2f__types
      use size
      use data
      use pfields
      use vars
      use size
      use data
      use pfields
      use vars
      use size
      use data
      use pfields
      use vars
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_894
      INTEGER(w2f__i8) OpenAD_Symbol_895
      INTEGER(w2f__i8) OpenAD_Symbol_896
      INTEGER(w2f__i8) OpenAD_Symbol_897
      INTEGER(w2f__i8) OpenAD_Symbol_898
      INTEGER(w2f__i8) OpenAD_Symbol_899
      INTEGER(w2f__i8) OpenAD_Symbol_900
      INTEGER(w2f__i8) OpenAD_Symbol_901
      INTEGER(w2f__i8) OpenAD_Symbol_902
      INTEGER(w2f__i8) OpenAD_Symbol_903
      INTEGER(w2f__i8) OpenAD_Symbol_904
      INTEGER(w2f__i8) OpenAD_Symbol_905
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      PARAMETER ( IX = 6)
      INTEGER(w2f__i4) IY
      REAL(w2f__8) ZONAL_TRANSPORT
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(TIME .eq. 0.0D00) THEN
        ZONAL_TRANSPORT_DATA = 5.26265D+07
      ELSE
        ZONAL_TRANSPORT = 0.0D00
        DO IY = 1, 20, 1
          ZONAL_TRANSPORT = (ZONAL_TRANSPORT + __value__(HU(6, IY)) *
     >  DY(IY) * __value__(U(6, IY)))
        END DO
        ZONAL_TRANSPORT_DATA = ZONAL_TRANSPORT
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(TIME .eq. 0.0D00) THEN
        ZONAL_TRANSPORT_DATA = 5.26265D+07
        OpenAD_Symbol_898 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_898)
      ELSE
        ZONAL_TRANSPORT = 0.0D00
        OpenAD_Symbol_897 = 0_w2f__i8
        DO IY = 1, 20, 1
          ZONAL_TRANSPORT = (ZONAL_TRANSPORT + __value__(HU(6, IY)) *
     >  DY(IY) * __value__(U(6, IY)))
          OpenAD_Symbol_897 = (INT(OpenAD_Symbol_897) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_897)
        ZONAL_TRANSPORT_DATA = ZONAL_TRANSPORT
        OpenAD_Symbol_899 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_899)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_894)
      IF(OpenAD_Symbol_894 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_895)
        OpenAD_Symbol_896 = 1
        DO WHILE(INT(OpenAD_Symbol_896) .LE. INT(OpenAD_Symbol_895))
          OpenAD_Symbol_896 = INT(OpenAD_Symbol_896) + 1
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(TIME .eq. 0.0D00) THEN
        ZONAL_TRANSPORT_DATA = 5.26265D+07
        OpenAD_Symbol_904 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_904)
      ELSE
        ZONAL_TRANSPORT = 0.0D00
        OpenAD_Symbol_903 = 0_w2f__i8
        DO IY = 1, 20, 1
          ZONAL_TRANSPORT = (ZONAL_TRANSPORT + __value__(HU(6, IY)) *
     >  DY(IY) * __value__(U(6, IY)))
          OpenAD_Symbol_903 = (INT(OpenAD_Symbol_903) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_903)
        ZONAL_TRANSPORT_DATA = ZONAL_TRANSPORT
        OpenAD_Symbol_905 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_905)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_900)
      IF(OpenAD_Symbol_900 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_901)
        OpenAD_Symbol_902 = 1
        DO WHILE(INT(OpenAD_Symbol_902) .LE. INT(OpenAD_Symbol_901))
          OpenAD_Symbol_902 = INT(OpenAD_Symbol_902) + 1
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE read_depth_data()
      use w2f__types
      use size
      use pfields
      use data
      use size
      use pfields
      use data
      use size
      use pfields
      use data
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_540
      INTEGER(w2f__i8) OpenAD_Symbol_541
      INTEGER(w2f__i8) OpenAD_Symbol_542
      INTEGER(w2f__i8) OpenAD_Symbol_543
      INTEGER(w2f__i8) OpenAD_Symbol_544
      INTEGER(w2f__i8) OpenAD_Symbol_545
      INTEGER(w2f__i8) OpenAD_Symbol_546
      INTEGER(w2f__i8) OpenAD_Symbol_547
      INTEGER(w2f__i8) OpenAD_Symbol_548
      INTEGER(w2f__i8) OpenAD_Symbol_549
      INTEGER(w2f__i8) OpenAD_Symbol_550
      INTEGER(w2f__i8) OpenAD_Symbol_551
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          DEPTH_DATA(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      OpenAD_Symbol_544 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_545 = 0_w2f__i8
        DO IX = 1, 20, 1
          DEPTH_DATA(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
          OpenAD_Symbol_545 = (INT(OpenAD_Symbol_545) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_545)
        OpenAD_Symbol_544 = (INT(OpenAD_Symbol_544) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_544)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_540)
      OpenAD_Symbol_541 = 1
      DO WHILE(INT(OpenAD_Symbol_541) .LE. INT(OpenAD_Symbol_540))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_542)
        OpenAD_Symbol_543 = 1
        DO WHILE(INT(OpenAD_Symbol_543) .LE. INT(OpenAD_Symbol_542))
          OpenAD_Symbol_543 = INT(OpenAD_Symbol_543) + 1
        END DO
        OpenAD_Symbol_541 = INT(OpenAD_Symbol_541) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      OpenAD_Symbol_550 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_551 = 0_w2f__i8
        DO IX = 1, 20, 1
          DEPTH_DATA(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
          OpenAD_Symbol_551 = (INT(OpenAD_Symbol_551) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_551)
        OpenAD_Symbol_550 = (INT(OpenAD_Symbol_550) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_550)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_546)
      OpenAD_Symbol_547 = 1
      DO WHILE(INT(OpenAD_Symbol_547) .LE. INT(OpenAD_Symbol_546))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_548)
        OpenAD_Symbol_549 = 1
        DO WHILE(INT(OpenAD_Symbol_549) .LE. INT(OpenAD_Symbol_548))
          OpenAD_Symbol_549 = INT(OpenAD_Symbol_549) + 1
        END DO
        OpenAD_Symbol_547 = INT(OpenAD_Symbol_547) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE determine_data_time(NCDATAFILE)
      use w2f__types
      use size
      use data
      use size
      use data
      use size
      use data
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_432
      INTEGER(w2f__i8) OpenAD_Symbol_433
      INTEGER(w2f__i8) OpenAD_Symbol_434
      INTEGER(w2f__i8) OpenAD_Symbol_435
      INTEGER(w2f__i8) OpenAD_Symbol_436
      INTEGER(w2f__i8) OpenAD_Symbol_437
      INTEGER(w2f__i8) OpenAD_Symbol_438
      INTEGER(w2f__i8) OpenAD_Symbol_439
      INTEGER(w2f__i8) OpenAD_Symbol_440
      INTEGER(w2f__i8) OpenAD_Symbol_441
      INTEGER(w2f__i8) OpenAD_Symbol_442
      INTEGER(w2f__i8) OpenAD_Symbol_443
C
C     **** Parameters and Result ****
C
      CHARACTER(80) NCDATAFILE
C
C     **** Local Variables and Functions ****
C
      EXTERNAL get_length_netcdf
      INTEGER(w2f__i4) K
      EXTERNAL read_vector_netcdf
      CHARACTER(80) STRTIME
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRTIME = 'TIME'
      CALL get_length_netcdf(NCDATAFILE, STRTIME, NEDT)
      IF(NEDT .GT. 1000) THEN
        WRITE(*, *) 'determine_data_time: too many data,;'
        WRITE(*, *) 'increase nedtmax to ', NEDT
      ELSE
        IF(NEDT .LE. 0) THEN
          WRITE(*, *) 'no time dependent data found in ' // NCDATAFILE
        ELSE
          CALL read_vector_netcdf(NCDATAFILE, STRTIME, NEDT,
     >  ETA_DATA_TIME)
          WRITE(*, *) 'determine_data_time: # of data times = ', NEDT
        ENDIF
      ENDIF
      WRITE(*, *)(ETA_DATA_TIME(K), K = 1, NEDT, 1)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRTIME = 'TIME'
      CALL get_length_netcdf(NCDATAFILE, STRTIME, NEDT)
      IF(NEDT .GT. 1000) THEN
        WRITE(*, *) 'determine_data_time: too many data,;'
        WRITE(*, *) 'increase nedtmax to ', NEDT
        OpenAD_Symbol_436 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_436)
      ELSE
        IF(NEDT .LE. 0) THEN
          WRITE(*, *) 'no time dependent data found in ' // NCDATAFILE
          OpenAD_Symbol_434 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_434)
        ELSE
          CALL read_vector_netcdf(NCDATAFILE, STRTIME, NEDT,
     >  ETA_DATA_TIME)
          WRITE(*, *) 'determine_data_time: # of data times = ', NEDT
          OpenAD_Symbol_435 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_435)
        ENDIF
        OpenAD_Symbol_437 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_437)
      ENDIF
      WRITE(*, *)(ETA_DATA_TIME(K), K = 1, NEDT, 1)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_432)
      IF(OpenAD_Symbol_432 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_433)
        IF(OpenAD_Symbol_433 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRTIME = 'TIME'
      CALL get_length_netcdf(NCDATAFILE, STRTIME, NEDT)
      IF(NEDT .GT. 1000) THEN
        WRITE(*, *) 'determine_data_time: too many data,;'
        WRITE(*, *) 'increase nedtmax to ', NEDT
        OpenAD_Symbol_442 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_442)
      ELSE
        IF(NEDT .LE. 0) THEN
          WRITE(*, *) 'no time dependent data found in ' // NCDATAFILE
          OpenAD_Symbol_440 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_440)
        ELSE
          CALL read_vector_netcdf(NCDATAFILE, STRTIME, NEDT,
     >  ETA_DATA_TIME)
          WRITE(*, *) 'determine_data_time: # of data times = ', NEDT
          OpenAD_Symbol_441 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_441)
        ENDIF
        OpenAD_Symbol_443 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_443)
      ENDIF
      WRITE(*, *)(ETA_DATA_TIME(K), K = 1, NEDT, 1)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_438)
      IF(OpenAD_Symbol_438 .ne. 0) THEN
      ELSE
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_439)
        IF(OpenAD_Symbol_439 .ne. 0) THEN
        ENDIF
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE is_eta_data_time(TIME, RESULT)
      use w2f__types
      use size
      use data
      use size
      use data
      use size
      use data
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_906
      INTEGER(w2f__i8) OpenAD_Symbol_907
      INTEGER(w2f__i8) OpenAD_Symbol_908
      INTEGER(w2f__i8) OpenAD_Symbol_909
      INTEGER(w2f__i8) OpenAD_Symbol_910
      INTEGER(w2f__i8) OpenAD_Symbol_911
      INTEGER(w2f__i8) OpenAD_Symbol_912
      INTEGER(w2f__i8) OpenAD_Symbol_913
      INTEGER(w2f__i8) OpenAD_Symbol_914
      INTEGER(w2f__i8) OpenAD_Symbol_915
      INTEGER(w2f__i8) OpenAD_Symbol_916
      INTEGER(w2f__i8) OpenAD_Symbol_917
      INTEGER(w2f__i8) OpenAD_Symbol_918
      INTEGER(w2f__i8) OpenAD_Symbol_919
      INTEGER(w2f__i8) OpenAD_Symbol_920
      INTEGER(w2f__i8) OpenAD_Symbol_921
      INTEGER(w2f__i8) OpenAD_Symbol_922
      INTEGER(w2f__i8) OpenAD_Symbol_923
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
      LOGICAL(w2f__i4) RESULT
C
C     **** Local Variables and Functions ****
C
      LOGICAL(w2f__i4) ALLDONE
      INTEGER(w2f__i4) IT
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ALLDONE = .FALSE.
      RESULT = .FALSE.
      IT = 1
      DO WHILE(.NOT. ALLDONE)
        IF(ABS(ETA_DATA_TIME(IT) - TIME) .LT.
     >  1.00000000000000002092D-08) THEN
          RESULT = .TRUE.
          ALLDONE = .TRUE.
        ELSE
          IF(IT .LT. NEDT) THEN
            IT = (IT + 1)
          ELSE
            ALLDONE = .TRUE.
          ENDIF
        ENDIF
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ALLDONE = .FALSE.
      RESULT = .FALSE.
      IT = 1
      OpenAD_Symbol_910 = 0_w2f__i8
      DO WHILE(.NOT. ALLDONE)
        IF(ABS(ETA_DATA_TIME(IT) - TIME) .LT.
     >  1.00000000000000002092D-08) THEN
          RESULT = .TRUE.
          ALLDONE = .TRUE.
          OpenAD_Symbol_913 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_913)
        ELSE
          IF(IT .LT. NEDT) THEN
            IT = (IT + 1)
            OpenAD_Symbol_911 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_911)
          ELSE
            ALLDONE = .TRUE.
            OpenAD_Symbol_912 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_912)
          ENDIF
          OpenAD_Symbol_914 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_914)
        ENDIF
        OpenAD_Symbol_910 = (INT(OpenAD_Symbol_910) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_910)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_906)
      OpenAD_Symbol_907 = 1
      DO WHILE(INT(OpenAD_Symbol_907) .LE. INT(OpenAD_Symbol_906))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_908)
        IF(OpenAD_Symbol_908 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_909)
          IF(OpenAD_Symbol_909 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_907 = INT(OpenAD_Symbol_907) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ETA_DATA_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(RESULT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ETA_DATA_TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(RESULT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ALLDONE = .FALSE.
      RESULT = .FALSE.
      IT = 1
      OpenAD_Symbol_919 = 0_w2f__i8
      DO WHILE(.NOT. ALLDONE)
        IF(ABS(ETA_DATA_TIME(IT) - TIME) .LT.
     >  1.00000000000000002092D-08) THEN
          RESULT = .TRUE.
          ALLDONE = .TRUE.
          OpenAD_Symbol_922 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_922)
        ELSE
          IF(IT .LT. NEDT) THEN
            IT = (IT + 1)
            OpenAD_Symbol_920 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_920)
          ELSE
            ALLDONE = .TRUE.
            OpenAD_Symbol_921 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_921)
          ENDIF
          OpenAD_Symbol_923 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_923)
        ENDIF
        OpenAD_Symbol_919 = (INT(OpenAD_Symbol_919) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_919)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_915)
      OpenAD_Symbol_916 = 1
      DO WHILE(INT(OpenAD_Symbol_916) .LE. INT(OpenAD_Symbol_915))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_917)
        IF(OpenAD_Symbol_917 .ne. 0) THEN
        ELSE
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_918)
          IF(OpenAD_Symbol_918 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_916 = INT(OpenAD_Symbol_916) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights()
      use w2f__types
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      EXTERNAL make_weights_eta
      EXTERNAL make_weights_graddepth
      EXTERNAL make_weights_lapldepth
      EXTERNAL make_weights_uv
      EXTERNAL make_weights_zonal_transport
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL make_weights_eta()
      CALL make_weights_uv()
      CALL make_weights_zonal_transport()
      CALL make_weights_lapldepth()
      CALL make_weights_graddepth()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL make_weights_eta()
      CALL make_weights_uv()
      CALL make_weights_zonal_transport()
      CALL make_weights_lapldepth()
      CALL make_weights_graddepth()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      CALL make_weights_graddepth()
      CALL make_weights_lapldepth()
      CALL make_weights_zonal_transport()
      CALL make_weights_uv()
      CALL make_weights_eta()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL make_weights_eta()
      CALL make_weights_uv()
      CALL make_weights_zonal_transport()
      CALL make_weights_lapldepth()
      CALL make_weights_graddepth()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
      CALL make_weights_graddepth()
      CALL make_weights_lapldepth()
      CALL make_weights_zonal_transport()
      CALL make_weights_uv()
      CALL make_weights_eta()
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights_depth()
      use w2f__types
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) MYNX
      SAVE MYNX
      INTEGER(w2f__i4) MYNY
      SAVE MYNY
      INTEGER(w2f__i4) NFLAG
      CHARACTER(80) STRW
      CHARACTER(80) STRWD
C
C     **** Initializers ****
C
      DATA MYNX / 20 /
      DATA MYNY / 20 /
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRWD = 'weights_depth.nc'
      STRW = 'W'
      NFLAG = 0
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRWD = 'weights_depth.nc'
      STRW = 'W'
      NFLAG = 0
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRWD = 'weights_depth.nc'
      STRW = 'W'
      NFLAG = 0
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights_eta()
      use w2f__types
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_924
      INTEGER(w2f__i8) OpenAD_Symbol_925
      INTEGER(w2f__i8) OpenAD_Symbol_926
      INTEGER(w2f__i8) OpenAD_Symbol_927
      INTEGER(w2f__i8) OpenAD_Symbol_928
      INTEGER(w2f__i8) OpenAD_Symbol_929
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ERROR_ETA
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ETA = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_ETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_ETA *((
     > 1.0D00 / ERROR_ETA) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ETA = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_ETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_ETA *((
     > 1.0D00 / ERROR_ETA) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ETA = 1.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_928 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_929 = 0_w2f__i8
        DO IX = 1, 20, 1
          WEIGHT_ETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_ETA *((
     > 1.0D00 / ERROR_ETA) ** 2))
          OpenAD_Symbol_929 = (INT(OpenAD_Symbol_929) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_929)
        OpenAD_Symbol_928 = (INT(OpenAD_Symbol_928) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_928)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_924)
      OpenAD_Symbol_925 = 1
      DO WHILE(INT(OpenAD_Symbol_925) .LE. INT(OpenAD_Symbol_924))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_926)
        OpenAD_Symbol_927 = 1
        DO WHILE(INT(OpenAD_Symbol_927) .LE. INT(OpenAD_Symbol_926))
          OpenAD_Symbol_927 = INT(OpenAD_Symbol_927) + 1
        END DO
        OpenAD_Symbol_925 = INT(OpenAD_Symbol_925) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights_uv()
      use w2f__types
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_930
      INTEGER(w2f__i8) OpenAD_Symbol_931
      INTEGER(w2f__i8) OpenAD_Symbol_932
      INTEGER(w2f__i8) OpenAD_Symbol_933
      INTEGER(w2f__i8) OpenAD_Symbol_934
      INTEGER(w2f__i8) OpenAD_Symbol_935
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ERROR_U
      REAL(w2f__8) ERROR_V
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_U = 1.0D00
      ERROR_V = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_U(INT(IX), INT(IY)) = (UMASK(IX, IY) * WF_U *((1.0D00
     >  / ERROR_U) ** 2))
          WEIGHT_V(INT(IX), INT(IY)) = (VMASK(IX, IY) * WF_V *((1.0D00
     >  / ERROR_V) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_U = 1.0D00
      ERROR_V = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_U(INT(IX), INT(IY)) = (UMASK(IX, IY) * WF_U *((1.0D00
     >  / ERROR_U) ** 2))
          WEIGHT_V(INT(IX), INT(IY)) = (VMASK(IX, IY) * WF_V *((1.0D00
     >  / ERROR_V) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_U)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_V)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_U = 1.0D00
      ERROR_V = 1.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_934 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_935 = 0_w2f__i8
        DO IX = 1, 20, 1
          WEIGHT_U(INT(IX), INT(IY)) = (UMASK(IX, IY) * WF_U *((1.0D00
     >  / ERROR_U) ** 2))
          WEIGHT_V(INT(IX), INT(IY)) = (VMASK(IX, IY) * WF_V *((1.0D00
     >  / ERROR_V) ** 2))
          OpenAD_Symbol_935 = (INT(OpenAD_Symbol_935) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_935)
        OpenAD_Symbol_934 = (INT(OpenAD_Symbol_934) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_934)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_930)
      OpenAD_Symbol_931 = 1
      DO WHILE(INT(OpenAD_Symbol_931) .LE. INT(OpenAD_Symbol_930))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_932)
        OpenAD_Symbol_933 = 1
        DO WHILE(INT(OpenAD_Symbol_933) .LE. INT(OpenAD_Symbol_932))
          OpenAD_Symbol_933 = INT(OpenAD_Symbol_933) + 1
        END DO
        OpenAD_Symbol_931 = INT(OpenAD_Symbol_931) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights_zonal_transport()
      use w2f__types
      use size
      use weights
      use size
      use weights
      use size
      use weights
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ERROR_ZONAL_TRANSPORT
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ZONAL_TRANSPORT = 1.0D+06
      WEIGHT_ZONAL_TRANSPORT = (WF_ZONAL_TRANSPORT *((1.0D00 /
     >  ERROR_ZONAL_TRANSPORT) ** 2))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ZONAL_TRANSPORT = 1.0D+06
      WEIGHT_ZONAL_TRANSPORT = (WF_ZONAL_TRANSPORT *((1.0D00 /
     >  ERROR_ZONAL_TRANSPORT) ** 2))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ZONAL_TRANSPORT = 1.0D+06
      WEIGHT_ZONAL_TRANSPORT = (WF_ZONAL_TRANSPORT *((1.0D00 /
     >  ERROR_ZONAL_TRANSPORT) ** 2))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_bounds_for_x(NC, LOWER_BOUND, UPPER_BOUND)
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_948
      INTEGER(w2f__i8) OpenAD_Symbol_949
      INTEGER(w2f__i8) OpenAD_Symbol_950
      INTEGER(w2f__i8) OpenAD_Symbol_951
      INTEGER(w2f__i8) OpenAD_Symbol_952
      INTEGER(w2f__i8) OpenAD_Symbol_953
      INTEGER(w2f__i8) OpenAD_Symbol_954
      INTEGER(w2f__i8) OpenAD_Symbol_955
      INTEGER(w2f__i8) OpenAD_Symbol_956
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) NC
      REAL(w2f__8) LOWER_BOUND(1 : NC)
      REAL(w2f__8) UPPER_BOUND(1 : NC)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) K
      REAL(w2f__8) LB
      REAL(w2f__8) UB
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      UB = 5.0D+01
      LB = (-5.0D+01)
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            UPPER_BOUND(INT(K)) = (UB / INIDEPTH(IX, IY))
            LOWER_BOUND(INT(K)) = (LB / INIDEPTH(IX, IY))
          ENDIF
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      UB = 5.0D+01
      LB = (-5.0D+01)
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            UPPER_BOUND(INT(K)) = (UB / INIDEPTH(IX, IY))
            LOWER_BOUND(INT(K)) = (LB / INIDEPTH(IX, IY))
          ENDIF
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
          ENDIF
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(INIDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(INIDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      UB = 5.0D+01
      LB = (-5.0D+01)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_953 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_954 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            UPPER_BOUND(INT(K)) = (UB / INIDEPTH(IX, IY))
            LOWER_BOUND(INT(K)) = (LB / INIDEPTH(IX, IY))
            OpenAD_Symbol_955 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_955)
          ELSE
            OpenAD_Symbol_956 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_956)
          ENDIF
          OpenAD_Symbol_954 = (INT(OpenAD_Symbol_954) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_954)
        OpenAD_Symbol_953 = (INT(OpenAD_Symbol_953) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_953)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_948)
      OpenAD_Symbol_949 = 1
      DO WHILE(INT(OpenAD_Symbol_949) .LE. INT(OpenAD_Symbol_948))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_950)
        OpenAD_Symbol_951 = 1
        DO WHILE(INT(OpenAD_Symbol_951) .LE. INT(OpenAD_Symbol_950))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_952)
          IF(OpenAD_Symbol_952 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_951 = INT(OpenAD_Symbol_951) + 1
        END DO
        OpenAD_Symbol_949 = INT(OpenAD_Symbol_949) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights_lapldepth()
      use w2f__types
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_936
      INTEGER(w2f__i8) OpenAD_Symbol_937
      INTEGER(w2f__i8) OpenAD_Symbol_938
      INTEGER(w2f__i8) OpenAD_Symbol_939
      INTEGER(w2f__i8) OpenAD_Symbol_940
      INTEGER(w2f__i8) OpenAD_Symbol_941
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ERROR
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) *
     >  WF_LAPLDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) *
     >  WF_LAPLDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_940 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_941 = 0_w2f__i8
        DO IX = 1, 20, 1
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) *
     >  WF_LAPLDEPTH *((1.0D00 / ERROR) ** 2))
          OpenAD_Symbol_941 = (INT(OpenAD_Symbol_941) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_941)
        OpenAD_Symbol_940 = (INT(OpenAD_Symbol_940) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_940)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_936)
      OpenAD_Symbol_937 = 1
      DO WHILE(INT(OpenAD_Symbol_937) .LE. INT(OpenAD_Symbol_936))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_938)
        OpenAD_Symbol_939 = 1
        DO WHILE(INT(OpenAD_Symbol_939) .LE. INT(OpenAD_Symbol_938))
          OpenAD_Symbol_939 = INT(OpenAD_Symbol_939) + 1
        END DO
        OpenAD_Symbol_937 = INT(OpenAD_Symbol_937) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE make_weights_graddepth()
      use w2f__types
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      use size
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_942
      INTEGER(w2f__i8) OpenAD_Symbol_943
      INTEGER(w2f__i8) OpenAD_Symbol_944
      INTEGER(w2f__i8) OpenAD_Symbol_945
      INTEGER(w2f__i8) OpenAD_Symbol_946
      INTEGER(w2f__i8) OpenAD_Symbol_947
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ERROR
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) *
     >  WF_GRADDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) *
     >  WF_GRADDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WF_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_946 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_947 = 0_w2f__i8
        DO IX = 1, 20, 1
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) *
     >  WF_GRADDEPTH *((1.0D00 / ERROR) ** 2))
          OpenAD_Symbol_947 = (INT(OpenAD_Symbol_947) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_947)
        OpenAD_Symbol_946 = (INT(OpenAD_Symbol_946) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_946)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_942)
      OpenAD_Symbol_943 = 1
      DO WHILE(INT(OpenAD_Symbol_943) .LE. INT(OpenAD_Symbol_942))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_944)
        OpenAD_Symbol_945 = 1
        DO WHILE(INT(OpenAD_Symbol_945) .LE. INT(OpenAD_Symbol_944))
          OpenAD_Symbol_945 = INT(OpenAD_Symbol_945) + 1
        END DO
        OpenAD_Symbol_943 = INT(OpenAD_Symbol_943) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE map_from_control_vector(N, XC)
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_750
      INTEGER(w2f__i8) OpenAD_Symbol_751
      INTEGER(w2f__i8) OpenAD_Symbol_752
      INTEGER(w2f__i8) OpenAD_Symbol_753
      INTEGER(w2f__i8) OpenAD_Symbol_754
      INTEGER(w2f__i8) OpenAD_Symbol_755
      INTEGER(w2f__i8) OpenAD_Symbol_756
      INTEGER(w2f__i8) OpenAD_Symbol_757
      INTEGER(w2f__i8) OpenAD_Symbol_758
      INTEGER(w2f__i8) OpenAD_Symbol_759
      INTEGER(w2f__i8) OpenAD_Symbol_760
      INTEGER(w2f__i8) OpenAD_Symbol_761
      INTEGER(w2f__i8) OpenAD_Symbol_762
      INTEGER(w2f__i8) OpenAD_Symbol_763
      INTEGER(w2f__i8) OpenAD_Symbol_764
      INTEGER(w2f__i8) OpenAD_Symbol_765
      INTEGER(w2f__i8) OpenAD_Symbol_766
      INTEGER(w2f__i8) OpenAD_Symbol_767
      INTEGER(w2f__i8) OpenAD_Symbol_768
      INTEGER(w2f__i8) OpenAD_Symbol_769
      INTEGER(w2f__i8) OpenAD_Symbol_770
      INTEGER(w2f__i8) OpenAD_Symbol_771
      INTEGER(w2f__i8) OpenAD_Symbol_772
      INTEGER(w2f__i8) OpenAD_Symbol_773
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) N
      TYPE (oadactive) XC(1 : N)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) K
      REAL(w2f__8) ONE
      PARAMETER ( ONE = 1.0D00)
      INTEGER(w2f__i4) OpenAD_Symbol_1314
      INTEGER(w2f__i4) OpenAD_Symbol_1315
      INTEGER(w2f__i4) OpenAD_Symbol_1316
      REAL(w2f__8) OpenAD_Symbol_1317
      REAL(w2f__8) OpenAD_aux_0
      REAL(w2f__8) OpenAD_lin_0
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            __value__(DEPTH(INT(IX), INT(IY))) = (SCALEDEPTH(IX, IY) *(
     > __value__(XC(K)) + 1.0D00))
          ENDIF
        END DO
      END DO
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_from_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_756 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_757 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            OpenAD_aux_0 = (__value__(XC(K)) + 1.0D00)
            OpenAD_lin_0 = SCALEDEPTH(IX, IY)
            __value__(DEPTH(INT(IX), INT(IY))) = (SCALEDEPTH(IX, IY) *
     >  OpenAD_aux_0)
C           $OpenAD$ INLINE push_s0(subst)
            CALL push_s0(OpenAD_lin_0)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(K)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IX)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IY)
            OpenAD_Symbol_758 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_758)
          ELSE
            OpenAD_Symbol_759 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_759)
          ENDIF
          OpenAD_Symbol_757 = (INT(OpenAD_Symbol_757) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_757)
        OpenAD_Symbol_756 = (INT(OpenAD_Symbol_756) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_756)
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_from_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
        OpenAD_Symbol_760 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_760)
      ELSE
        OpenAD_Symbol_761 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_761)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_750)
      IF(OpenAD_Symbol_750 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_751)
      OpenAD_Symbol_752 = 1
      DO WHILE(INT(OpenAD_Symbol_752) .LE. INT(OpenAD_Symbol_751))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_753)
        OpenAD_Symbol_754 = 1
        DO WHILE(INT(OpenAD_Symbol_754) .LE. INT(OpenAD_Symbol_753))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_755)
          IF(OpenAD_Symbol_755 .ne. 0) THEN
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1314)
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1315)
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1316)
C           $OpenAD$ INLINE pop_s0(subst)
            CALL pop_s0(OpenAD_Symbol_1317)
C           $OpenAD$ INLINE Saxpy(subst,subst,subst)
            CALL Saxpy(OpenAD_Symbol_1317, __deriv__(DEPTH(
     > OpenAD_Symbol_1315, OpenAD_Symbol_1314)), __deriv__(XC(
     > OpenAD_Symbol_1316)))
C           $OpenAD$ INLINE ZeroDeriv(subst)
            CALL ZeroDeriv(__deriv__(DEPTH(OpenAD_Symbol_1315,
     >  OpenAD_Symbol_1314)))
          ENDIF
          OpenAD_Symbol_754 = INT(OpenAD_Symbol_754) + 1
        END DO
        OpenAD_Symbol_752 = INT(OpenAD_Symbol_752) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(N)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_vector_a(subst)
      CALL cp_arg_store_real_vector_a(__deriv__(XC))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector_a(subst)
      CALL cp_arg_restore_real_vector_a(__deriv__(XC))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(N)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(N)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_vector_a(subst)
      CALL cp_arg_store_real_vector_a(__deriv__(XC))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector_a(subst)
      CALL cp_arg_restore_real_vector_a(__deriv__(XC))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(N)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_768 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_769 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            OpenAD_aux_0 = (__value__(XC(K)) + 1.0D00)
            OpenAD_lin_0 = SCALEDEPTH(IX, IY)
            __value__(DEPTH(INT(IX), INT(IY))) = (SCALEDEPTH(IX, IY) *
     >  OpenAD_aux_0)
C           $OpenAD$ INLINE push_s0(subst)
            CALL push_s0(OpenAD_lin_0)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(K)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IX)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IY)
            OpenAD_Symbol_770 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_770)
          ELSE
            OpenAD_Symbol_771 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_771)
          ENDIF
          OpenAD_Symbol_769 = (INT(OpenAD_Symbol_769) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_769)
        OpenAD_Symbol_768 = (INT(OpenAD_Symbol_768) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_768)
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_from_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
        OpenAD_Symbol_772 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_772)
      ELSE
        OpenAD_Symbol_773 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_773)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_762)
      IF(OpenAD_Symbol_762 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_763)
      OpenAD_Symbol_764 = 1
      DO WHILE(INT(OpenAD_Symbol_764) .LE. INT(OpenAD_Symbol_763))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_765)
        OpenAD_Symbol_766 = 1
        DO WHILE(INT(OpenAD_Symbol_766) .LE. INT(OpenAD_Symbol_765))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_767)
          IF(OpenAD_Symbol_767 .ne. 0) THEN
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1314)
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1315)
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1316)
C           $OpenAD$ INLINE pop_s0(subst)
            CALL pop_s0(OpenAD_Symbol_1317)
C           $OpenAD$ INLINE Saxpy(subst,subst,subst)
            CALL Saxpy(OpenAD_Symbol_1317, __deriv__(DEPTH(
     > OpenAD_Symbol_1315, OpenAD_Symbol_1314)), __deriv__(XC(
     > OpenAD_Symbol_1316)))
C           $OpenAD$ INLINE ZeroDeriv(subst)
            CALL ZeroDeriv(__deriv__(DEPTH(OpenAD_Symbol_1315,
     >  OpenAD_Symbol_1314)))
          ENDIF
          OpenAD_Symbol_766 = INT(OpenAD_Symbol_766) + 1
        END DO
        OpenAD_Symbol_764 = INT(OpenAD_Symbol_764) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE map_to_control_vector(N, XC)
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_957
      INTEGER(w2f__i8) OpenAD_Symbol_958
      INTEGER(w2f__i8) OpenAD_Symbol_959
      INTEGER(w2f__i8) OpenAD_Symbol_960
      INTEGER(w2f__i8) OpenAD_Symbol_961
      INTEGER(w2f__i8) OpenAD_Symbol_962
      INTEGER(w2f__i8) OpenAD_Symbol_963
      INTEGER(w2f__i8) OpenAD_Symbol_964
      INTEGER(w2f__i8) OpenAD_Symbol_965
      INTEGER(w2f__i8) OpenAD_Symbol_966
      INTEGER(w2f__i8) OpenAD_Symbol_967
      INTEGER(w2f__i8) OpenAD_Symbol_968
      INTEGER(w2f__i8) OpenAD_Symbol_969
      INTEGER(w2f__i8) OpenAD_Symbol_970
      INTEGER(w2f__i8) OpenAD_Symbol_971
      INTEGER(w2f__i8) OpenAD_Symbol_972
      INTEGER(w2f__i8) OpenAD_Symbol_973
      INTEGER(w2f__i8) OpenAD_Symbol_974
      INTEGER(w2f__i8) OpenAD_Symbol_975
      INTEGER(w2f__i8) OpenAD_Symbol_976
      INTEGER(w2f__i8) OpenAD_Symbol_977
      INTEGER(w2f__i8) OpenAD_Symbol_978
      INTEGER(w2f__i8) OpenAD_Symbol_979
      INTEGER(w2f__i8) OpenAD_Symbol_980
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) N
      REAL(w2f__8) XC(1 : N)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) K
      REAL(w2f__8) ONE
      PARAMETER ( ONE = 1.0D00)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            XC(INT(K)) = ((__value__(DEPTH(IX, IY)) / SCALEDEPTH(IX, IY
     > )) +(-1.0D00))
          ENDIF
        END DO
      END DO
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_to_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_963 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_964 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            XC(INT(K)) = ((__value__(DEPTH(IX, IY)) / SCALEDEPTH(IX, IY
     > )) +(-1.0D00))
            OpenAD_Symbol_965 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_965)
          ELSE
            OpenAD_Symbol_966 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_966)
          ENDIF
          OpenAD_Symbol_964 = (INT(OpenAD_Symbol_964) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_964)
        OpenAD_Symbol_963 = (INT(OpenAD_Symbol_963) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_963)
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_to_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
        OpenAD_Symbol_967 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_967)
      ELSE
        OpenAD_Symbol_968 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_968)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_957)
      IF(OpenAD_Symbol_957 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_958)
      OpenAD_Symbol_959 = 1
      DO WHILE(INT(OpenAD_Symbol_959) .LE. INT(OpenAD_Symbol_958))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_960)
        OpenAD_Symbol_961 = 1
        DO WHILE(INT(OpenAD_Symbol_961) .LE. INT(OpenAD_Symbol_960))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_962)
          IF(OpenAD_Symbol_962 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_961 = INT(OpenAD_Symbol_961) + 1
        END DO
        OpenAD_Symbol_959 = INT(OpenAD_Symbol_959) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_975 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_976 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            XC(INT(K)) = ((__value__(DEPTH(IX, IY)) / SCALEDEPTH(IX, IY
     > )) +(-1.0D00))
            OpenAD_Symbol_977 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_977)
          ELSE
            OpenAD_Symbol_978 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_978)
          ENDIF
          OpenAD_Symbol_976 = (INT(OpenAD_Symbol_976) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_976)
        OpenAD_Symbol_975 = (INT(OpenAD_Symbol_975) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_975)
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_to_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
        OpenAD_Symbol_979 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_979)
      ELSE
        OpenAD_Symbol_980 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_980)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_969)
      IF(OpenAD_Symbol_969 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_970)
      OpenAD_Symbol_971 = 1
      DO WHILE(INT(OpenAD_Symbol_971) .LE. INT(OpenAD_Symbol_970))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_972)
        OpenAD_Symbol_973 = 1
        DO WHILE(INT(OpenAD_Symbol_973) .LE. INT(OpenAD_Symbol_972))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_974)
          IF(OpenAD_Symbol_974 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_973 = INT(OpenAD_Symbol_973) + 1
        END DO
        OpenAD_Symbol_971 = INT(OpenAD_Symbol_971) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE map_gradient(N, ADXC, GRAD)
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_714
      INTEGER(w2f__i8) OpenAD_Symbol_715
      INTEGER(w2f__i8) OpenAD_Symbol_716
      INTEGER(w2f__i8) OpenAD_Symbol_717
      INTEGER(w2f__i8) OpenAD_Symbol_718
      INTEGER(w2f__i8) OpenAD_Symbol_719
      INTEGER(w2f__i8) OpenAD_Symbol_720
      INTEGER(w2f__i8) OpenAD_Symbol_721
      INTEGER(w2f__i8) OpenAD_Symbol_722
      INTEGER(w2f__i8) OpenAD_Symbol_723
      INTEGER(w2f__i8) OpenAD_Symbol_724
      INTEGER(w2f__i8) OpenAD_Symbol_725
      INTEGER(w2f__i8) OpenAD_Symbol_726
      INTEGER(w2f__i8) OpenAD_Symbol_727
      INTEGER(w2f__i8) OpenAD_Symbol_728
      INTEGER(w2f__i8) OpenAD_Symbol_729
      INTEGER(w2f__i8) OpenAD_Symbol_730
      INTEGER(w2f__i8) OpenAD_Symbol_731
      INTEGER(w2f__i8) OpenAD_Symbol_732
      INTEGER(w2f__i8) OpenAD_Symbol_733
      INTEGER(w2f__i8) OpenAD_Symbol_734
      INTEGER(w2f__i8) OpenAD_Symbol_735
      INTEGER(w2f__i8) OpenAD_Symbol_736
      INTEGER(w2f__i8) OpenAD_Symbol_737
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) N
      REAL(w2f__8) ADXC(1 : N)
      REAL(w2f__8) GRAD(1 : 20, 1 : 20)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) K
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            GRAD(INT(IX), INT(IY)) = (ADXC(K) / SCALEDEPTH(IX, IY))
          ENDIF
        END DO
      END DO
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_from_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_720 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_721 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            GRAD(INT(IX), INT(IY)) = (ADXC(K) / SCALEDEPTH(IX, IY))
            OpenAD_Symbol_722 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_722)
          ELSE
            OpenAD_Symbol_723 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_723)
          ENDIF
          OpenAD_Symbol_721 = (INT(OpenAD_Symbol_721) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_721)
        OpenAD_Symbol_720 = (INT(OpenAD_Symbol_720) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_720)
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_from_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
        OpenAD_Symbol_724 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_724)
      ELSE
        OpenAD_Symbol_725 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_725)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_714)
      IF(OpenAD_Symbol_714 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_715)
      OpenAD_Symbol_716 = 1
      DO WHILE(INT(OpenAD_Symbol_716) .LE. INT(OpenAD_Symbol_715))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_717)
        OpenAD_Symbol_718 = 1
        DO WHILE(INT(OpenAD_Symbol_718) .LE. INT(OpenAD_Symbol_717))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_719)
          IF(OpenAD_Symbol_719 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_718 = INT(OpenAD_Symbol_718) + 1
        END DO
        OpenAD_Symbol_716 = INT(OpenAD_Symbol_716) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(N)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ADXC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ADXC)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(N)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(GRAD)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(N)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ADXC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ADXC)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(N)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(GRAD)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_732 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_733 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            GRAD(INT(IX), INT(IY)) = (ADXC(K) / SCALEDEPTH(IX, IY))
            OpenAD_Symbol_734 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_734)
          ELSE
            OpenAD_Symbol_735 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_735)
          ENDIF
          OpenAD_Symbol_733 = (INT(OpenAD_Symbol_733) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_733)
        OpenAD_Symbol_732 = (INT(OpenAD_Symbol_732) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_732)
      IF(N .ne. K) THEN
        WRITE(*, *) 'map_from_control_vector: ',
     >  'dimensions of control vector are wrong'
        WRITE(*, *) K, ' should be ', N
        OpenAD_Symbol_736 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_736)
      ELSE
        OpenAD_Symbol_737 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_737)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_726)
      IF(OpenAD_Symbol_726 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_727)
      OpenAD_Symbol_728 = 1
      DO WHILE(INT(OpenAD_Symbol_728) .LE. INT(OpenAD_Symbol_727))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_729)
        OpenAD_Symbol_730 = 1
        DO WHILE(INT(OpenAD_Symbol_730) .LE. INT(OpenAD_Symbol_729))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_731)
          IF(OpenAD_Symbol_731 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_730 = INT(OpenAD_Symbol_730) + 1
        END DO
        OpenAD_Symbol_728 = INT(OpenAD_Symbol_728) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE length_of_control_vector(N)
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_981
      INTEGER(w2f__i8) OpenAD_Symbol_982
      INTEGER(w2f__i8) OpenAD_Symbol_983
      INTEGER(w2f__i8) OpenAD_Symbol_984
      INTEGER(w2f__i8) OpenAD_Symbol_985
      INTEGER(w2f__i8) OpenAD_Symbol_986
      INTEGER(w2f__i8) OpenAD_Symbol_987
      INTEGER(w2f__i8) OpenAD_Symbol_988
      INTEGER(w2f__i8) OpenAD_Symbol_989
      INTEGER(w2f__i8) OpenAD_Symbol_990
      INTEGER(w2f__i8) OpenAD_Symbol_991
      INTEGER(w2f__i8) OpenAD_Symbol_992
      INTEGER(w2f__i8) OpenAD_Symbol_993
      INTEGER(w2f__i8) OpenAD_Symbol_994
      INTEGER(w2f__i8) OpenAD_Symbol_995
      INTEGER(w2f__i8) OpenAD_Symbol_996
      INTEGER(w2f__i8) OpenAD_Symbol_997
      INTEGER(w2f__i8) OpenAD_Symbol_998
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) N
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) K
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
          ENDIF
        END DO
      END DO
      N = K
      WRITE(*, *) 'dimensions of control vector = ', N
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_986 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_987 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            OpenAD_Symbol_988 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_988)
          ELSE
            OpenAD_Symbol_989 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_989)
          ENDIF
          OpenAD_Symbol_987 = (INT(OpenAD_Symbol_987) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_987)
        OpenAD_Symbol_986 = (INT(OpenAD_Symbol_986) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_986)
      N = K
      WRITE(*, *) 'dimensions of control vector = ', N
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_981)
      OpenAD_Symbol_982 = 1
      DO WHILE(INT(OpenAD_Symbol_982) .LE. INT(OpenAD_Symbol_981))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_983)
        OpenAD_Symbol_984 = 1
        DO WHILE(INT(OpenAD_Symbol_984) .LE. INT(OpenAD_Symbol_983))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_985)
          IF(OpenAD_Symbol_985 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_984 = INT(OpenAD_Symbol_984) + 1
        END DO
        OpenAD_Symbol_982 = INT(OpenAD_Symbol_982) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_995 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_996 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            OpenAD_Symbol_997 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_997)
          ELSE
            OpenAD_Symbol_998 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_998)
          ENDIF
          OpenAD_Symbol_996 = (INT(OpenAD_Symbol_996) + INT(1_w2f__i8))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_996)
        OpenAD_Symbol_995 = (INT(OpenAD_Symbol_995) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_995)
      N = K
      WRITE(*, *) 'dimensions of control vector = ', N
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_990)
      OpenAD_Symbol_991 = 1
      DO WHILE(INT(OpenAD_Symbol_991) .LE. INT(OpenAD_Symbol_990))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_992)
        OpenAD_Symbol_993 = 1
        DO WHILE(INT(OpenAD_Symbol_993) .LE. INT(OpenAD_Symbol_992))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_994)
          IF(OpenAD_Symbol_994 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_993 = INT(OpenAD_Symbol_993) + 1
        END DO
        OpenAD_Symbol_991 = INT(OpenAD_Symbol_991) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE time_step(IT, LOCAL_U, LOCAL_V, LOCAL_ETA)
      use w2f__types
      use size
      use vars
      use size
      use vars
      use size
      use vars
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1000
      INTEGER(w2f__i8) OpenAD_Symbol_1001
      INTEGER(w2f__i8) OpenAD_Symbol_1002
      INTEGER(w2f__i8) OpenAD_Symbol_1003
      INTEGER(w2f__i8) OpenAD_Symbol_1004
      INTEGER(w2f__i8) OpenAD_Symbol_999
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) IT
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_V(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_ETA(0 : 21, 0 : 21)
C
C     **** Local Variables and Functions ****
C
      EXTERNAL continuity
      EXTERNAL umomentum
      EXTERNAL vmomentum
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(MOD(IT, 2) .ne. 0) THEN
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
      ELSE
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
      ENDIF
      CALL continuity(__deriv__(LOCAL_U), __deriv__(LOCAL_V), __deriv__
     > (LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(MOD(IT, 2) .ne. 0) THEN
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        OpenAD_Symbol_1000 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1000)
      ELSE
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        OpenAD_Symbol_1001 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1001)
      ENDIF
      CALL continuity(__deriv__(LOCAL_U), __deriv__(LOCAL_V), __deriv__
     > (LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      CALL continuity(__deriv__(LOCAL_U), __deriv__(LOCAL_V), __deriv__
     > (LOCAL_ETA))
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_999)
      IF(OpenAD_Symbol_999 .ne. 0) THEN
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
      ELSE
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(IT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(MOD(IT, 2) .ne. 0) THEN
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        OpenAD_Symbol_1003 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1003)
      ELSE
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        OpenAD_Symbol_1004 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1004)
      ENDIF
      CALL continuity(__deriv__(LOCAL_U), __deriv__(LOCAL_V), __deriv__
     > (LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
      CALL continuity(__deriv__(LOCAL_U), __deriv__(LOCAL_V), __deriv__
     > (LOCAL_ETA))
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1002)
      IF(OpenAD_Symbol_1002 .ne. 0) THEN
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
      ELSE
        CALL umomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
        CALL vmomentum(__deriv__(LOCAL_U), __deriv__(LOCAL_V),
     >  __deriv__(LOCAL_ETA))
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE umomentum(LOCAL_U, LOCAL_V, LOCAL_ETA)
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use force
      use size
      use parms
      use vars
      use pfields
      use force
      use size
      use parms
      use vars
      use pfields
      use force
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1005
      INTEGER(w2f__i8) OpenAD_Symbol_1006
      INTEGER(w2f__i8) OpenAD_Symbol_1007
      INTEGER(w2f__i8) OpenAD_Symbol_1008
      INTEGER(w2f__i8) OpenAD_Symbol_1009
      INTEGER(w2f__i8) OpenAD_Symbol_1010
      INTEGER(w2f__i8) OpenAD_Symbol_1011
      INTEGER(w2f__i8) OpenAD_Symbol_1012
      INTEGER(w2f__i8) OpenAD_Symbol_1013
      INTEGER(w2f__i8) OpenAD_Symbol_1014
      INTEGER(w2f__i8) OpenAD_Symbol_1015
      INTEGER(w2f__i8) OpenAD_Symbol_1016
      INTEGER(w2f__i8) OpenAD_Symbol_1017
      INTEGER(w2f__i8) OpenAD_Symbol_1018
      INTEGER(w2f__i8) OpenAD_Symbol_1019
      INTEGER(w2f__i8) OpenAD_Symbol_1020
      INTEGER(w2f__i8) OpenAD_Symbol_1021
      INTEGER(w2f__i8) OpenAD_Symbol_1022
      INTEGER(w2f__i8) OpenAD_Symbol_1023
      INTEGER(w2f__i8) OpenAD_Symbol_1024
      INTEGER(w2f__i8) OpenAD_Symbol_1025
      INTEGER(w2f__i8) OpenAD_Symbol_1026
      INTEGER(w2f__i8) OpenAD_Symbol_1027
      INTEGER(w2f__i8) OpenAD_Symbol_1028
C
C     **** Parameters and Result ****
C
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_V(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_ETA(0 : 21, 0 : 21)
C
C     **** Local Variables and Functions ****
C
      TYPE (oadactive) FRICTU
      TYPE (oadactive) FV
      TYPE (oadactive) GRADETAU
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1029
      INTEGER(w2f__i4) OpenAD_Symbol_1030
      INTEGER(w2f__i4) OpenAD_Symbol_1031
      INTEGER(w2f__i4) OpenAD_Symbol_1032
      INTEGER(w2f__i4) OpenAD_Symbol_1033
      INTEGER(w2f__i4) OpenAD_Symbol_1318
      INTEGER(w2f__i4) OpenAD_Symbol_1319
      REAL(w2f__8) OpenAD_Symbol_1320
      REAL(w2f__8) OpenAD_Symbol_1321
      REAL(w2f__8) OpenAD_Symbol_1322
      REAL(w2f__8) OpenAD_Symbol_1323
      REAL(w2f__8) OpenAD_Symbol_1324
      INTEGER(w2f__i4) OpenAD_Symbol_1325
      INTEGER(w2f__i4) OpenAD_Symbol_1326
      INTEGER(w2f__i4) OpenAD_Symbol_1327
      INTEGER(w2f__i4) OpenAD_Symbol_1328
      INTEGER(w2f__i4) OpenAD_Symbol_1329
      INTEGER(w2f__i4) OpenAD_Symbol_1330
      INTEGER(w2f__i4) OpenAD_Symbol_1331
      REAL(w2f__8) OpenAD_acc_0
      REAL(w2f__8) OpenAD_acc_1
      REAL(w2f__8) OpenAD_acc_2
      REAL(w2f__8) OpenAD_acc_3
      REAL(w2f__8) OpenAD_acc_4
      REAL(w2f__8) OpenAD_aux_1
      REAL(w2f__8) OpenAD_aux_10
      REAL(w2f__8) OpenAD_aux_11
      REAL(w2f__8) OpenAD_aux_12
      REAL(w2f__8) OpenAD_aux_2
      REAL(w2f__8) OpenAD_aux_3
      REAL(w2f__8) OpenAD_aux_4
      REAL(w2f__8) OpenAD_aux_5
      REAL(w2f__8) OpenAD_aux_6
      REAL(w2f__8) OpenAD_aux_7
      REAL(w2f__8) OpenAD_aux_8
      REAL(w2f__8) OpenAD_aux_9
      REAL(w2f__8) OpenAD_lin_1
      REAL(w2f__8) OpenAD_lin_10
      REAL(w2f__8) OpenAD_lin_11
      REAL(w2f__8) OpenAD_lin_12
      REAL(w2f__8) OpenAD_lin_13
      REAL(w2f__8) OpenAD_lin_2
      REAL(w2f__8) OpenAD_lin_3
      REAL(w2f__8) OpenAD_lin_4
      REAL(w2f__8) OpenAD_lin_5
      REAL(w2f__8) OpenAD_lin_6
      REAL(w2f__8) OpenAD_lin_7
      REAL(w2f__8) OpenAD_lin_8
      REAL(w2f__8) OpenAD_lin_9
      TYPE (oadactive) OpenAD_prp_0
      TYPE (oadactive) OpenAD_prp_1
      TYPE (oadactive) OpenAD_prp_2
      TYPE (oadactive) OpenAD_prp_3
      TYPE (oadactive) OpenAD_prp_4
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          __value__(FRICTU) = (__value__(INVHU(IX, IY)) *(FRICT(IX, IY)
     >  + FRICT(IX + (-1), IY)) * 5.0D-01)
          __value__(GRADETAU) = ((__value__(LOCAL_ETA(IX, IY)) -
     >  __value__(LOCAL_ETA(IX + (-1), IY))) /(RX(IY) * 5.0D-01 *(DX(IX
     > ) + DX(IX + (-1)))))
          __value__(FV) = (FCORIU(IX, IY) *(__value__(LOCAL_V(IX + (-1)
     > , IY + 1)) + __value__(LOCAL_V(IX, IY + 1)) + __value__(LOCAL_V(
     > IX, IY)) + __value__(LOCAL_V(IX + (-1), IY))))
          __value__(LOCAL_U(INT(IX), INT(IY))) = ((UMASK(IX, IY) /(DT *
     >  __value__(FRICTU) * 5.0D-01 + 1.0D00)) *(__value__(FV) * DT +
     >  __value__(LOCAL_U(IX, IY)) *(1.0D00 - DT * __value__(FRICTU) *
     >  5.0D-01) - __value__(GRADETAU) * DT * 9.81000000000000049738D00
     >  + __value__(INVHU(IX, IY)) * UFORCE(IX, IY) * DT))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(LOCAL_U(21, INT(IY))) = __value__(LOCAL_U(1, IY))
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(LOCAL_U(INT(IX), 0)) = __value__(LOCAL_U(IX, 20))
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          OpenAD_aux_1 = ((FRICT(IX, IY) + FRICT(IX + (-1), IY)) *
     >  5.0D-01)
          OpenAD_lin_1 = OpenAD_aux_1
          __value__(FRICTU) = (__value__(INVHU(IX, IY)) * OpenAD_aux_1)
          OpenAD_aux_2 = (__value__(LOCAL_ETA(IX, IY)) - __value__(
     > LOCAL_ETA(IX + (-1), IY)))
          OpenAD_aux_3 = (RX(IY) * 5.0D-01 *(DX(IX) + DX(IX + (-1))))
          OpenAD_lin_2 = (INT(1_w2f__i8) / OpenAD_aux_3)
          __value__(GRADETAU) = (OpenAD_aux_2 / OpenAD_aux_3)
          OpenAD_aux_4 = (__value__(LOCAL_V(IX + (-1), IY + 1)) +
     >  __value__(LOCAL_V(IX, IY + 1)) + __value__(LOCAL_V(IX, IY)) +
     >  __value__(LOCAL_V(IX + (-1), IY)))
          OpenAD_lin_3 = FCORIU(IX, IY)
          __value__(FV) = (FCORIU(IX, IY) * OpenAD_aux_4)
          OpenAD_aux_8 = (__value__(FRICTU) * 5.0D-01)
          OpenAD_aux_7 = (DT * OpenAD_aux_8 + 1.0D00)
          OpenAD_aux_5 = (UMASK(IX, IY) / OpenAD_aux_7)
          OpenAD_aux_10 = (__value__(FRICTU) * 5.0D-01)
          OpenAD_aux_9 = (1.0D00 - DT * OpenAD_aux_10)
          OpenAD_aux_11 = (DT * 9.81000000000000049738D00)
          OpenAD_aux_12 = (UFORCE(IX, IY) * DT)
          OpenAD_aux_6 = (__value__(FV) * DT + __value__(LOCAL_U(IX, IY
     > )) * OpenAD_aux_9 - __value__(GRADETAU) * OpenAD_aux_11 +
     >  __value__(INVHU(IX, IY)) * OpenAD_aux_12)
          OpenAD_lin_7 = DT
          OpenAD_lin_6 = (-(UMASK(IX, IY) /(OpenAD_aux_7 * OpenAD_aux_7
     > )))
          OpenAD_lin_4 = OpenAD_aux_6
          OpenAD_lin_8 = DT
          OpenAD_lin_9 = OpenAD_aux_9
          OpenAD_lin_11 = DT
          OpenAD_lin_10 = __value__(LOCAL_U(IX, IY))
          OpenAD_lin_12 = OpenAD_aux_11
          OpenAD_lin_13 = OpenAD_aux_12
          OpenAD_lin_5 = OpenAD_aux_5
          __value__(LOCAL_U(INT(IX), INT(IY))) = (OpenAD_aux_5 *
     >  OpenAD_aux_6)
          OpenAD_acc_0 = (OpenAD_lin_13 * OpenAD_lin_5)
          OpenAD_acc_1 = (OpenAD_lin_3 * OpenAD_lin_8 * OpenAD_lin_5)
          OpenAD_acc_2 = (OpenAD_lin_9 * OpenAD_lin_5)
          OpenAD_acc_3 = (OpenAD_lin_2 * OpenAD_lin_12 * INT((
     > -1_w2f__i8)) * OpenAD_lin_5)
          OpenAD_acc_4 = (OpenAD_lin_1 * 5.0D-01 * OpenAD_lin_11 * INT(
     > (-1_w2f__i8)) * OpenAD_lin_10 * OpenAD_lin_5 + OpenAD_lin_1 *
     >  5.0D-01 * OpenAD_lin_7 * OpenAD_lin_6 * OpenAD_lin_4)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_0)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_1)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_2)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_3)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_4)
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(LOCAL_U(21, INT(IY))) = __value__(LOCAL_U(1, IY))
        END DO
        OpenAD_Symbol_1008 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1008)
      ELSE
        OpenAD_Symbol_1007 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1007)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(LOCAL_U(INT(IX), 0)) = __value__(LOCAL_U(IX, 20))
        END DO
        OpenAD_Symbol_1010 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1010)
      ELSE
        OpenAD_Symbol_1009 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1009)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1005)
      IF(OpenAD_Symbol_1005 .ne. 0) THEN
        IX = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IX .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_U(IX, 0)), __deriv__(
     > OpenAD_prp_4))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_U(IX, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_4), __deriv__(LOCAL_U(IX,
     >  20)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_4))
          IX = IX - 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1006)
      IF(OpenAD_Symbol_1006 .ne. 0) THEN
        IY = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IY .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_U(21, IY)), __deriv__(
     > OpenAD_prp_3))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_U(21, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_3), __deriv__(LOCAL_U(1,
     >  IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_3))
          IY = IY - 1
        END DO
      ENDIF
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1320)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1321)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1322)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1323)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1324)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1320, __deriv__(LOCAL_U(IX, IY)),
     >  __deriv__(INVHU(IX, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1321, __deriv__(LOCAL_U(IX, IY)),
     >  __deriv__(OpenAD_prp_1))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1322, __deriv__(LOCAL_U(IX, IY)),
     >  __deriv__(OpenAD_prp_0))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1323, __deriv__(LOCAL_U(IX, IY)),
     >  __deriv__(OpenAD_prp_2))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1324, __deriv__(LOCAL_U(IX, IY)),
     >  __deriv__(INVHU(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_U(IX, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(IX +
     >  (-1), IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(IX,
     >  IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(IX,
     >  IY + 1)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(IX +
     >  (-1), IY + 1)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_2))
C         $OpenAD$ INLINE DecDeriv(subst,subst)
          CALL DecDeriv(__deriv__(OpenAD_prp_1), __deriv__(LOCAL_ETA(IX
     >  + (-1), IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_1), __deriv__(LOCAL_ETA(IX
     > , IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_1))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_0), __deriv__(LOCAL_U(IX,
     >  IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_0))
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1021 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1022 = 0_w2f__i8
        DO IX = 1, 20, 1
          OpenAD_aux_1 = ((FRICT(IX, IY) + FRICT(IX + (-1), IY)) *
     >  5.0D-01)
          OpenAD_lin_1 = OpenAD_aux_1
          __value__(FRICTU) = (__value__(INVHU(IX, IY)) * OpenAD_aux_1)
          OpenAD_aux_2 = (__value__(LOCAL_ETA(IX, IY)) - __value__(
     > LOCAL_ETA(IX + (-1), IY)))
          OpenAD_aux_3 = (RX(IY) * 5.0D-01 *(DX(IX) + DX(IX + (-1))))
          OpenAD_lin_2 = (INT(1_w2f__i8) / OpenAD_aux_3)
          __value__(GRADETAU) = (OpenAD_aux_2 / OpenAD_aux_3)
          OpenAD_aux_4 = (__value__(LOCAL_V(IX + (-1), IY + 1)) +
     >  __value__(LOCAL_V(IX, IY + 1)) + __value__(LOCAL_V(IX, IY)) +
     >  __value__(LOCAL_V(IX + (-1), IY)))
          OpenAD_lin_3 = FCORIU(IX, IY)
          __value__(FV) = (FCORIU(IX, IY) * OpenAD_aux_4)
          OpenAD_aux_8 = (__value__(FRICTU) * 5.0D-01)
          OpenAD_aux_7 = (DT * OpenAD_aux_8 + 1.0D00)
          OpenAD_aux_5 = (UMASK(IX, IY) / OpenAD_aux_7)
          OpenAD_aux_10 = (__value__(FRICTU) * 5.0D-01)
          OpenAD_aux_9 = (1.0D00 - DT * OpenAD_aux_10)
          OpenAD_aux_11 = (DT * 9.81000000000000049738D00)
          OpenAD_aux_12 = (UFORCE(IX, IY) * DT)
          OpenAD_aux_6 = (__value__(FV) * DT + __value__(LOCAL_U(IX, IY
     > )) * OpenAD_aux_9 - __value__(GRADETAU) * OpenAD_aux_11 +
     >  __value__(INVHU(IX, IY)) * OpenAD_aux_12)
          OpenAD_lin_7 = DT
          OpenAD_lin_6 = (-(UMASK(IX, IY) /(OpenAD_aux_7 * OpenAD_aux_7
     > )))
          OpenAD_lin_4 = OpenAD_aux_6
          OpenAD_lin_8 = DT
          OpenAD_lin_9 = OpenAD_aux_9
          OpenAD_lin_11 = DT
          OpenAD_lin_10 = __value__(LOCAL_U(IX, IY))
          OpenAD_lin_12 = OpenAD_aux_11
          OpenAD_lin_13 = OpenAD_aux_12
          OpenAD_lin_5 = OpenAD_aux_5
          __value__(LOCAL_U(INT(IX), INT(IY))) = (OpenAD_aux_5 *
     >  OpenAD_aux_6)
          OpenAD_acc_0 = (OpenAD_lin_13 * OpenAD_lin_5)
          OpenAD_acc_1 = (OpenAD_lin_3 * OpenAD_lin_8 * OpenAD_lin_5)
          OpenAD_acc_2 = (OpenAD_lin_9 * OpenAD_lin_5)
          OpenAD_acc_3 = (OpenAD_lin_2 * OpenAD_lin_12 * INT((
     > -1_w2f__i8)) * OpenAD_lin_5)
          OpenAD_acc_4 = (OpenAD_lin_1 * 5.0D-01 * OpenAD_lin_11 * INT(
     > (-1_w2f__i8)) * OpenAD_lin_10 * OpenAD_lin_5 + OpenAD_lin_1 *
     >  5.0D-01 * OpenAD_lin_7 * OpenAD_lin_6 * OpenAD_lin_4)
          OpenAD_Symbol_1029 = (IX +(-1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1029)
          OpenAD_Symbol_1030 = (IX +(-1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1030)
          OpenAD_Symbol_1031 = (IY + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1031)
          OpenAD_Symbol_1032 = (IY + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1032)
          OpenAD_Symbol_1033 = (IX +(-1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1033)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_0)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_1)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_2)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_3)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_4)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1022 = (INT(OpenAD_Symbol_1022) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1022)
        OpenAD_Symbol_1021 = (INT(OpenAD_Symbol_1021) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1021)
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1023 = 0_w2f__i8
        DO IY = 0, 21, 1
          __value__(LOCAL_U(21, INT(IY))) = __value__(LOCAL_U(1, IY))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1023 = (INT(OpenAD_Symbol_1023) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1023)
        OpenAD_Symbol_1025 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1025)
      ELSE
        OpenAD_Symbol_1024 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1024)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1026 = 0_w2f__i8
        DO IX = 0, 21, 1
          __value__(LOCAL_U(INT(IX), 0)) = __value__(LOCAL_U(IX, 20))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
          OpenAD_Symbol_1026 = (INT(OpenAD_Symbol_1026) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1026)
        OpenAD_Symbol_1028 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1028)
      ELSE
        OpenAD_Symbol_1027 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1027)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1011)
      IF(OpenAD_Symbol_1011 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1012)
        OpenAD_Symbol_1013 = 1
        DO WHILE(INT(OpenAD_Symbol_1013) .LE. INT(OpenAD_Symbol_1012))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1331)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_U(OpenAD_Symbol_1331, 0)),
     >  __deriv__(OpenAD_prp_4))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_U(OpenAD_Symbol_1331, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_4), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1331, 20)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_4))
          OpenAD_Symbol_1013 = INT(OpenAD_Symbol_1013) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1014)
      IF(OpenAD_Symbol_1014 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1015)
        OpenAD_Symbol_1016 = 1
        DO WHILE(INT(OpenAD_Symbol_1016) .LE. INT(OpenAD_Symbol_1015))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1330)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_U(21, OpenAD_Symbol_1330)),
     >  __deriv__(OpenAD_prp_3))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_U(21, OpenAD_Symbol_1330)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_3), __deriv__(LOCAL_U(1,
     >  OpenAD_Symbol_1330)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_3))
          OpenAD_Symbol_1016 = INT(OpenAD_Symbol_1016) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1017)
      OpenAD_Symbol_1018 = 1
      DO WHILE(INT(OpenAD_Symbol_1018) .LE. INT(OpenAD_Symbol_1017))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1019)
        OpenAD_Symbol_1020 = 1
        DO WHILE(INT(OpenAD_Symbol_1020) .LE. INT(OpenAD_Symbol_1019))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1318)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1319)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1320)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1321)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1322)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1323)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1324)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1320, __deriv__(LOCAL_U(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)), __deriv__(INVHU(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1321, __deriv__(LOCAL_U(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)), __deriv__(OpenAD_prp_1
     > ))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1322, __deriv__(LOCAL_U(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)), __deriv__(OpenAD_prp_0
     > ))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1323, __deriv__(LOCAL_U(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)), __deriv__(OpenAD_prp_2
     > ))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1324, __deriv__(LOCAL_U(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)), __deriv__(INVHU(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_U(OpenAD_Symbol_1319,
     >  OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1325)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1325, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1326)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1326)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1327)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1328)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_2), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1328, OpenAD_Symbol_1327)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_2))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1329)
C         $OpenAD$ INLINE DecDeriv(subst,subst)
          CALL DecDeriv(__deriv__(OpenAD_prp_1), __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1329, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_1), __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_1))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_0), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1319, OpenAD_Symbol_1318)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_0))
          OpenAD_Symbol_1020 = INT(OpenAD_Symbol_1020) + 1
        END DO
        OpenAD_Symbol_1018 = INT(OpenAD_Symbol_1018) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE vmomentum(LOCAL_U, LOCAL_V, LOCAL_ETA)
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use force
      use size
      use parms
      use vars
      use pfields
      use force
      use size
      use parms
      use vars
      use pfields
      use force
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1034
      INTEGER(w2f__i8) OpenAD_Symbol_1035
      INTEGER(w2f__i8) OpenAD_Symbol_1036
      INTEGER(w2f__i8) OpenAD_Symbol_1037
      INTEGER(w2f__i8) OpenAD_Symbol_1038
      INTEGER(w2f__i8) OpenAD_Symbol_1039
      INTEGER(w2f__i8) OpenAD_Symbol_1040
      INTEGER(w2f__i8) OpenAD_Symbol_1041
      INTEGER(w2f__i8) OpenAD_Symbol_1042
      INTEGER(w2f__i8) OpenAD_Symbol_1043
      INTEGER(w2f__i8) OpenAD_Symbol_1044
      INTEGER(w2f__i8) OpenAD_Symbol_1045
      INTEGER(w2f__i8) OpenAD_Symbol_1046
      INTEGER(w2f__i8) OpenAD_Symbol_1047
      INTEGER(w2f__i8) OpenAD_Symbol_1048
      INTEGER(w2f__i8) OpenAD_Symbol_1049
      INTEGER(w2f__i8) OpenAD_Symbol_1050
      INTEGER(w2f__i8) OpenAD_Symbol_1051
      INTEGER(w2f__i8) OpenAD_Symbol_1052
      INTEGER(w2f__i8) OpenAD_Symbol_1053
      INTEGER(w2f__i8) OpenAD_Symbol_1054
      INTEGER(w2f__i8) OpenAD_Symbol_1055
      INTEGER(w2f__i8) OpenAD_Symbol_1056
      INTEGER(w2f__i8) OpenAD_Symbol_1057
C
C     **** Parameters and Result ****
C
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_V(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_ETA(0 : 21, 0 : 21)
C
C     **** Local Variables and Functions ****
C
      TYPE (oadactive) FRICTV
      TYPE (oadactive) FU
      TYPE (oadactive) GRADETAV
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1058
      INTEGER(w2f__i4) OpenAD_Symbol_1059
      INTEGER(w2f__i4) OpenAD_Symbol_1060
      INTEGER(w2f__i4) OpenAD_Symbol_1061
      INTEGER(w2f__i4) OpenAD_Symbol_1062
      INTEGER(w2f__i4) OpenAD_Symbol_1332
      INTEGER(w2f__i4) OpenAD_Symbol_1333
      REAL(w2f__8) OpenAD_Symbol_1334
      REAL(w2f__8) OpenAD_Symbol_1335
      REAL(w2f__8) OpenAD_Symbol_1336
      REAL(w2f__8) OpenAD_Symbol_1337
      REAL(w2f__8) OpenAD_Symbol_1338
      INTEGER(w2f__i4) OpenAD_Symbol_1339
      INTEGER(w2f__i4) OpenAD_Symbol_1340
      INTEGER(w2f__i4) OpenAD_Symbol_1341
      INTEGER(w2f__i4) OpenAD_Symbol_1342
      INTEGER(w2f__i4) OpenAD_Symbol_1343
      INTEGER(w2f__i4) OpenAD_Symbol_1344
      INTEGER(w2f__i4) OpenAD_Symbol_1345
      REAL(w2f__8) OpenAD_acc_5
      REAL(w2f__8) OpenAD_acc_6
      REAL(w2f__8) OpenAD_acc_7
      REAL(w2f__8) OpenAD_acc_8
      REAL(w2f__8) OpenAD_acc_9
      REAL(w2f__8) OpenAD_aux_13
      REAL(w2f__8) OpenAD_aux_14
      REAL(w2f__8) OpenAD_aux_15
      REAL(w2f__8) OpenAD_aux_16
      REAL(w2f__8) OpenAD_aux_17
      REAL(w2f__8) OpenAD_aux_18
      REAL(w2f__8) OpenAD_aux_19
      REAL(w2f__8) OpenAD_aux_20
      REAL(w2f__8) OpenAD_aux_21
      REAL(w2f__8) OpenAD_aux_22
      REAL(w2f__8) OpenAD_aux_23
      REAL(w2f__8) OpenAD_aux_24
      REAL(w2f__8) OpenAD_lin_14
      REAL(w2f__8) OpenAD_lin_15
      REAL(w2f__8) OpenAD_lin_16
      REAL(w2f__8) OpenAD_lin_17
      REAL(w2f__8) OpenAD_lin_18
      REAL(w2f__8) OpenAD_lin_19
      REAL(w2f__8) OpenAD_lin_20
      REAL(w2f__8) OpenAD_lin_21
      REAL(w2f__8) OpenAD_lin_22
      REAL(w2f__8) OpenAD_lin_23
      REAL(w2f__8) OpenAD_lin_24
      REAL(w2f__8) OpenAD_lin_25
      REAL(w2f__8) OpenAD_lin_26
      TYPE (oadactive) OpenAD_prp_5
      TYPE (oadactive) OpenAD_prp_6
      TYPE (oadactive) OpenAD_prp_7
      TYPE (oadactive) OpenAD_prp_8
      TYPE (oadactive) OpenAD_prp_9
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          __value__(FRICTV) = (__value__(INVHV(IX, IY)) *(FRICT(IX, IY)
     >  + FRICT(IX, IY + (-1))) * 5.0D-01)
          __value__(GRADETAV) = ((__value__(LOCAL_ETA(IX, IY)) -
     >  __value__(LOCAL_ETA(IX, IY + (-1)))) /(RY * 5.0D-01 *(DY(IY) +
     >  DY(IY + (-1)))))
          __value__(FU) = (FCORIV(IX, IY) *(__value__(LOCAL_U(IX + 1,
     >  IY + (-1))) + __value__(LOCAL_U(IX, IY + (-1))) + __value__(
     > LOCAL_U(IX, IY)) + __value__(LOCAL_U(IX + 1, IY))))
          __value__(LOCAL_V(INT(IX), INT(IY))) = ((VMASK(IX, IY) /(DT *
     >  __value__(FRICTV) * 5.0D-01 + 1.0D00)) *(__value__(INVHV(IX, IY
     > )) * VFORCE(IX, IY) * DT + __value__(LOCAL_V(IX, IY)) *(1.0D00 -
     >  DT * __value__(FRICTV) * 5.0D-01) - __value__(GRADETAV) * DT *
     >  9.81000000000000049738D00 - __value__(FU) * DT))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(LOCAL_V(0, INT(IY))) = __value__(LOCAL_V(20, IY))
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(LOCAL_V(INT(IX), 21)) = __value__(LOCAL_V(IX, 1))
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          OpenAD_aux_13 = ((FRICT(IX, IY) + FRICT(IX, IY + (-1))) *
     >  5.0D-01)
          OpenAD_lin_14 = OpenAD_aux_13
          __value__(FRICTV) = (__value__(INVHV(IX, IY)) * OpenAD_aux_13
     > )
          OpenAD_aux_14 = (__value__(LOCAL_ETA(IX, IY)) - __value__(
     > LOCAL_ETA(IX, IY + (-1))))
          OpenAD_aux_15 = (RY * 5.0D-01 *(DY(IY) + DY(IY + (-1))))
          OpenAD_lin_15 = (INT(1_w2f__i8) / OpenAD_aux_15)
          __value__(GRADETAV) = (OpenAD_aux_14 / OpenAD_aux_15)
          OpenAD_aux_16 = (__value__(LOCAL_U(IX + 1, IY + (-1))) +
     >  __value__(LOCAL_U(IX, IY + (-1))) + __value__(LOCAL_U(IX, IY))
     >  + __value__(LOCAL_U(IX + 1, IY)))
          OpenAD_lin_16 = FCORIV(IX, IY)
          __value__(FU) = (FCORIV(IX, IY) * OpenAD_aux_16)
          OpenAD_aux_20 = (__value__(FRICTV) * 5.0D-01)
          OpenAD_aux_19 = (DT * OpenAD_aux_20 + 1.0D00)
          OpenAD_aux_17 = (VMASK(IX, IY) / OpenAD_aux_19)
          OpenAD_aux_21 = (VFORCE(IX, IY) * DT)
          OpenAD_aux_23 = (__value__(FRICTV) * 5.0D-01)
          OpenAD_aux_22 = (1.0D00 - DT * OpenAD_aux_23)
          OpenAD_aux_24 = (DT * 9.81000000000000049738D00)
          OpenAD_aux_18 = (__value__(INVHV(IX, IY)) * OpenAD_aux_21 +
     >  __value__(LOCAL_V(IX, IY)) * OpenAD_aux_22 - __value__(GRADETAV
     > ) * OpenAD_aux_24 - __value__(FU) * DT)
          OpenAD_lin_20 = DT
          OpenAD_lin_19 = (-(VMASK(IX, IY) /(OpenAD_aux_19 *
     >  OpenAD_aux_19)))
          OpenAD_lin_17 = OpenAD_aux_18
          OpenAD_lin_21 = OpenAD_aux_21
          OpenAD_lin_22 = OpenAD_aux_22
          OpenAD_lin_24 = DT
          OpenAD_lin_23 = __value__(LOCAL_V(IX, IY))
          OpenAD_lin_25 = OpenAD_aux_24
          OpenAD_lin_26 = DT
          OpenAD_lin_18 = OpenAD_aux_17
          __value__(LOCAL_V(INT(IX), INT(IY))) = (OpenAD_aux_17 *
     >  OpenAD_aux_18)
          OpenAD_acc_5 = (OpenAD_lin_21 * OpenAD_lin_18)
          OpenAD_acc_6 = (OpenAD_lin_16 * OpenAD_lin_26 * INT((
     > -1_w2f__i8)) * OpenAD_lin_18)
          OpenAD_acc_7 = (OpenAD_lin_22 * OpenAD_lin_18)
          OpenAD_acc_8 = (OpenAD_lin_15 * OpenAD_lin_25 * INT((
     > -1_w2f__i8)) * OpenAD_lin_18)
          OpenAD_acc_9 = (OpenAD_lin_14 * 5.0D-01 * OpenAD_lin_24 * INT
     > ((-1_w2f__i8)) * OpenAD_lin_23 * OpenAD_lin_18 + OpenAD_lin_14 *
     >  5.0D-01 * OpenAD_lin_20 * OpenAD_lin_19 * OpenAD_lin_17)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_5)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_6)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_7)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_8)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_9)
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(LOCAL_V(0, INT(IY))) = __value__(LOCAL_V(20, IY))
        END DO
        OpenAD_Symbol_1037 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1037)
      ELSE
        OpenAD_Symbol_1036 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1036)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(LOCAL_V(INT(IX), 21)) = __value__(LOCAL_V(IX, 1))
        END DO
        OpenAD_Symbol_1039 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1039)
      ELSE
        OpenAD_Symbol_1038 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1038)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1034)
      IF(OpenAD_Symbol_1034 .ne. 0) THEN
        IX = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IX .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_V(IX, 21)), __deriv__(
     > OpenAD_prp_9))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_V(IX, 21)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_9), __deriv__(LOCAL_V(IX,
     >  1)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_9))
          IX = IX - 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1035)
      IF(OpenAD_Symbol_1035 .ne. 0) THEN
        IY = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IY .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_V(0, IY)), __deriv__(
     > OpenAD_prp_8))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_V(0, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_8), __deriv__(LOCAL_V(20,
     >  IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_8))
          IY = IY - 1
        END DO
      ENDIF
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1334)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1335)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1336)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1337)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1338)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1334, __deriv__(LOCAL_V(IX, IY)),
     >  __deriv__(INVHV(IX, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1335, __deriv__(LOCAL_V(IX, IY)),
     >  __deriv__(OpenAD_prp_6))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1336, __deriv__(LOCAL_V(IX, IY)),
     >  __deriv__(OpenAD_prp_5))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1337, __deriv__(LOCAL_V(IX, IY)),
     >  __deriv__(OpenAD_prp_7))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1338, __deriv__(LOCAL_V(IX, IY)),
     >  __deriv__(INVHV(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_V(IX, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(IX +
     >  1, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(IX,
     >  IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(IX,
     >  IY + (-1))))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(IX +
     >  1, IY + (-1))))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_7))
C         $OpenAD$ INLINE DecDeriv(subst,subst)
          CALL DecDeriv(__deriv__(OpenAD_prp_6), __deriv__(LOCAL_ETA(IX
     > , IY + (-1))))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_6), __deriv__(LOCAL_ETA(IX
     > , IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_6))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_5), __deriv__(LOCAL_V(IX,
     >  IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_5))
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1050 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1051 = 0_w2f__i8
        DO IX = 1, 20, 1
          OpenAD_aux_13 = ((FRICT(IX, IY) + FRICT(IX, IY + (-1))) *
     >  5.0D-01)
          OpenAD_lin_14 = OpenAD_aux_13
          __value__(FRICTV) = (__value__(INVHV(IX, IY)) * OpenAD_aux_13
     > )
          OpenAD_aux_14 = (__value__(LOCAL_ETA(IX, IY)) - __value__(
     > LOCAL_ETA(IX, IY + (-1))))
          OpenAD_aux_15 = (RY * 5.0D-01 *(DY(IY) + DY(IY + (-1))))
          OpenAD_lin_15 = (INT(1_w2f__i8) / OpenAD_aux_15)
          __value__(GRADETAV) = (OpenAD_aux_14 / OpenAD_aux_15)
          OpenAD_aux_16 = (__value__(LOCAL_U(IX + 1, IY + (-1))) +
     >  __value__(LOCAL_U(IX, IY + (-1))) + __value__(LOCAL_U(IX, IY))
     >  + __value__(LOCAL_U(IX + 1, IY)))
          OpenAD_lin_16 = FCORIV(IX, IY)
          __value__(FU) = (FCORIV(IX, IY) * OpenAD_aux_16)
          OpenAD_aux_20 = (__value__(FRICTV) * 5.0D-01)
          OpenAD_aux_19 = (DT * OpenAD_aux_20 + 1.0D00)
          OpenAD_aux_17 = (VMASK(IX, IY) / OpenAD_aux_19)
          OpenAD_aux_21 = (VFORCE(IX, IY) * DT)
          OpenAD_aux_23 = (__value__(FRICTV) * 5.0D-01)
          OpenAD_aux_22 = (1.0D00 - DT * OpenAD_aux_23)
          OpenAD_aux_24 = (DT * 9.81000000000000049738D00)
          OpenAD_aux_18 = (__value__(INVHV(IX, IY)) * OpenAD_aux_21 +
     >  __value__(LOCAL_V(IX, IY)) * OpenAD_aux_22 - __value__(GRADETAV
     > ) * OpenAD_aux_24 - __value__(FU) * DT)
          OpenAD_lin_20 = DT
          OpenAD_lin_19 = (-(VMASK(IX, IY) /(OpenAD_aux_19 *
     >  OpenAD_aux_19)))
          OpenAD_lin_17 = OpenAD_aux_18
          OpenAD_lin_21 = OpenAD_aux_21
          OpenAD_lin_22 = OpenAD_aux_22
          OpenAD_lin_24 = DT
          OpenAD_lin_23 = __value__(LOCAL_V(IX, IY))
          OpenAD_lin_25 = OpenAD_aux_24
          OpenAD_lin_26 = DT
          OpenAD_lin_18 = OpenAD_aux_17
          __value__(LOCAL_V(INT(IX), INT(IY))) = (OpenAD_aux_17 *
     >  OpenAD_aux_18)
          OpenAD_acc_5 = (OpenAD_lin_21 * OpenAD_lin_18)
          OpenAD_acc_6 = (OpenAD_lin_16 * OpenAD_lin_26 * INT((
     > -1_w2f__i8)) * OpenAD_lin_18)
          OpenAD_acc_7 = (OpenAD_lin_22 * OpenAD_lin_18)
          OpenAD_acc_8 = (OpenAD_lin_15 * OpenAD_lin_25 * INT((
     > -1_w2f__i8)) * OpenAD_lin_18)
          OpenAD_acc_9 = (OpenAD_lin_14 * 5.0D-01 * OpenAD_lin_24 * INT
     > ((-1_w2f__i8)) * OpenAD_lin_23 * OpenAD_lin_18 + OpenAD_lin_14 *
     >  5.0D-01 * OpenAD_lin_20 * OpenAD_lin_19 * OpenAD_lin_17)
          OpenAD_Symbol_1058 = (IY +(-1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1058)
          OpenAD_Symbol_1059 = (IX + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1059)
          OpenAD_Symbol_1060 = (IY +(-1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1060)
          OpenAD_Symbol_1061 = (IY +(-1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1061)
          OpenAD_Symbol_1062 = (IX + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1062)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_5)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_6)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_7)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_8)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_9)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1051 = (INT(OpenAD_Symbol_1051) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1051)
        OpenAD_Symbol_1050 = (INT(OpenAD_Symbol_1050) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1050)
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1052 = 0_w2f__i8
        DO IY = 0, 21, 1
          __value__(LOCAL_V(0, INT(IY))) = __value__(LOCAL_V(20, IY))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1052 = (INT(OpenAD_Symbol_1052) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1052)
        OpenAD_Symbol_1054 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1054)
      ELSE
        OpenAD_Symbol_1053 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1053)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1055 = 0_w2f__i8
        DO IX = 0, 21, 1
          __value__(LOCAL_V(INT(IX), 21)) = __value__(LOCAL_V(IX, 1))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
          OpenAD_Symbol_1055 = (INT(OpenAD_Symbol_1055) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1055)
        OpenAD_Symbol_1057 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1057)
      ELSE
        OpenAD_Symbol_1056 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1056)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1040)
      IF(OpenAD_Symbol_1040 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1041)
        OpenAD_Symbol_1042 = 1
        DO WHILE(INT(OpenAD_Symbol_1042) .LE. INT(OpenAD_Symbol_1041))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1345)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_V(OpenAD_Symbol_1345, 21)),
     >  __deriv__(OpenAD_prp_9))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_V(OpenAD_Symbol_1345, 21)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_9), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1345, 1)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_9))
          OpenAD_Symbol_1042 = INT(OpenAD_Symbol_1042) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1043)
      IF(OpenAD_Symbol_1043 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1044)
        OpenAD_Symbol_1045 = 1
        DO WHILE(INT(OpenAD_Symbol_1045) .LE. INT(OpenAD_Symbol_1044))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1344)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_V(0, OpenAD_Symbol_1344)),
     >  __deriv__(OpenAD_prp_8))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_V(0, OpenAD_Symbol_1344)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_8), __deriv__(LOCAL_V(20,
     >  OpenAD_Symbol_1344)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_8))
          OpenAD_Symbol_1045 = INT(OpenAD_Symbol_1045) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1046)
      OpenAD_Symbol_1047 = 1
      DO WHILE(INT(OpenAD_Symbol_1047) .LE. INT(OpenAD_Symbol_1046))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1048)
        OpenAD_Symbol_1049 = 1
        DO WHILE(INT(OpenAD_Symbol_1049) .LE. INT(OpenAD_Symbol_1048))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1332)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1333)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1334)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1335)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1336)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1337)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1338)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1334, __deriv__(LOCAL_V(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)), __deriv__(INVHV(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1335, __deriv__(LOCAL_V(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)), __deriv__(OpenAD_prp_6
     > ))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1336, __deriv__(LOCAL_V(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)), __deriv__(OpenAD_prp_5
     > ))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1337, __deriv__(LOCAL_V(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)), __deriv__(OpenAD_prp_7
     > ))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1338, __deriv__(LOCAL_V(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)), __deriv__(INVHV(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_V(OpenAD_Symbol_1333,
     >  OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1339)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1339, OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1340)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1340)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1341)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1342)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_7), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1342, OpenAD_Symbol_1341)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_7))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1343)
C         $OpenAD$ INLINE DecDeriv(subst,subst)
          CALL DecDeriv(__deriv__(OpenAD_prp_6), __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1343)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_6), __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_6))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_5), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1333, OpenAD_Symbol_1332)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_5))
          OpenAD_Symbol_1049 = INT(OpenAD_Symbol_1049) + 1
        END DO
        OpenAD_Symbol_1047 = INT(OpenAD_Symbol_1047) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE continuity(LOCAL_U, LOCAL_V, LOCAL_ETA)
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use size
      use parms
      use vars
      use pfields
      use size
      use parms
      use vars
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1063
      INTEGER(w2f__i8) OpenAD_Symbol_1064
      INTEGER(w2f__i8) OpenAD_Symbol_1065
      INTEGER(w2f__i8) OpenAD_Symbol_1066
      INTEGER(w2f__i8) OpenAD_Symbol_1067
      INTEGER(w2f__i8) OpenAD_Symbol_1068
      INTEGER(w2f__i8) OpenAD_Symbol_1069
      INTEGER(w2f__i8) OpenAD_Symbol_1070
      INTEGER(w2f__i8) OpenAD_Symbol_1071
      INTEGER(w2f__i8) OpenAD_Symbol_1072
      INTEGER(w2f__i8) OpenAD_Symbol_1073
      INTEGER(w2f__i8) OpenAD_Symbol_1074
      INTEGER(w2f__i8) OpenAD_Symbol_1075
      INTEGER(w2f__i8) OpenAD_Symbol_1076
      INTEGER(w2f__i8) OpenAD_Symbol_1077
      INTEGER(w2f__i8) OpenAD_Symbol_1078
      INTEGER(w2f__i8) OpenAD_Symbol_1079
      INTEGER(w2f__i8) OpenAD_Symbol_1080
      INTEGER(w2f__i8) OpenAD_Symbol_1081
      INTEGER(w2f__i8) OpenAD_Symbol_1082
      INTEGER(w2f__i8) OpenAD_Symbol_1083
      INTEGER(w2f__i8) OpenAD_Symbol_1084
      INTEGER(w2f__i8) OpenAD_Symbol_1085
      INTEGER(w2f__i8) OpenAD_Symbol_1086
C
C     **** Parameters and Result ****
C
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_V(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_ETA(0 : 21, 0 : 21)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1087
      INTEGER(w2f__i4) OpenAD_Symbol_1088
      INTEGER(w2f__i4) OpenAD_Symbol_1089
      INTEGER(w2f__i4) OpenAD_Symbol_1090
      INTEGER(w2f__i4) OpenAD_Symbol_1346
      INTEGER(w2f__i4) OpenAD_Symbol_1347
      REAL(w2f__8) OpenAD_Symbol_1348
      REAL(w2f__8) OpenAD_Symbol_1349
      REAL(w2f__8) OpenAD_Symbol_1350
      REAL(w2f__8) OpenAD_Symbol_1351
      REAL(w2f__8) OpenAD_Symbol_1352
      REAL(w2f__8) OpenAD_Symbol_1353
      REAL(w2f__8) OpenAD_Symbol_1354
      REAL(w2f__8) OpenAD_Symbol_1355
      INTEGER(w2f__i4) OpenAD_Symbol_1356
      INTEGER(w2f__i4) OpenAD_Symbol_1357
      INTEGER(w2f__i4) OpenAD_Symbol_1358
      INTEGER(w2f__i4) OpenAD_Symbol_1359
      INTEGER(w2f__i4) OpenAD_Symbol_1360
      INTEGER(w2f__i4) OpenAD_Symbol_1361
      REAL(w2f__8) OpenAD_acc_10
      REAL(w2f__8) OpenAD_acc_11
      REAL(w2f__8) OpenAD_acc_12
      REAL(w2f__8) OpenAD_acc_13
      REAL(w2f__8) OpenAD_acc_14
      REAL(w2f__8) OpenAD_acc_15
      REAL(w2f__8) OpenAD_acc_16
      REAL(w2f__8) OpenAD_acc_17
      REAL(w2f__8) OpenAD_acc_18
      REAL(w2f__8) OpenAD_acc_19
      REAL(w2f__8) OpenAD_acc_20
      REAL(w2f__8) OpenAD_acc_21
      REAL(w2f__8) OpenAD_acc_22
      REAL(w2f__8) OpenAD_aux_25
      REAL(w2f__8) OpenAD_aux_26
      REAL(w2f__8) OpenAD_aux_27
      REAL(w2f__8) OpenAD_aux_28
      REAL(w2f__8) OpenAD_aux_29
      REAL(w2f__8) OpenAD_aux_30
      REAL(w2f__8) OpenAD_aux_31
      REAL(w2f__8) OpenAD_aux_32
      REAL(w2f__8) OpenAD_lin_27
      REAL(w2f__8) OpenAD_lin_28
      REAL(w2f__8) OpenAD_lin_29
      REAL(w2f__8) OpenAD_lin_30
      REAL(w2f__8) OpenAD_lin_31
      REAL(w2f__8) OpenAD_lin_32
      REAL(w2f__8) OpenAD_lin_33
      REAL(w2f__8) OpenAD_lin_34
      REAL(w2f__8) OpenAD_lin_35
      REAL(w2f__8) OpenAD_lin_36
      REAL(w2f__8) OpenAD_lin_37
      REAL(w2f__8) OpenAD_lin_38
      REAL(w2f__8) OpenAD_lin_39
      TYPE (oadactive) OpenAD_prp_10
      TYPE (oadactive) OpenAD_prp_11
      TYPE (oadactive) OpenAD_prp_12
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          __value__(LOCAL_ETA(INT(IX), INT(IY))) = (__value__(LOCAL_ETA
     > (IX, IY)) - ETAMASK(IX, IY) * DT *(((__value__(LOCAL_U(IX + 1,
     >  IY)) * __value__(HU(IX + 1, IY)) - __value__(LOCAL_U(IX, IY)) *
     >  __value__(HU(IX, IY))) /(DX(IX) * RX(IY))) +((__value__(LOCAL_V
     > (IX, IY + 1)) * HY(IY + 1) * __value__(HV(IX, IY + 1)) -
     >  __value__(LOCAL_V(IX, IY)) * HY(IY) * __value__(HV(IX, IY))) /(
     > DY(IY) * RY))))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(LOCAL_ETA(0, INT(IY))) = __value__(LOCAL_ETA(20, IY
     > ))
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(LOCAL_ETA(INT(IX), 0)) = __value__(LOCAL_ETA(IX, 20
     > ))
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          OpenAD_aux_25 = (ETAMASK(IX, IY) * DT)
          OpenAD_aux_27 = (__value__(LOCAL_U(IX + 1, IY)) * __value__(
     > HU(IX + 1, IY)) - __value__(LOCAL_U(IX, IY)) * __value__(HU(IX,
     >  IY)))
          OpenAD_aux_28 = (DX(IX) * RX(IY))
          OpenAD_aux_31 = (HY(IY + 1) * __value__(HV(IX, IY + 1)))
          OpenAD_aux_32 = (HY(IY) * __value__(HV(IX, IY)))
          OpenAD_aux_29 = (__value__(LOCAL_V(IX, IY + 1)) *
     >  OpenAD_aux_31 - __value__(LOCAL_V(IX, IY)) * OpenAD_aux_32)
          OpenAD_aux_30 = (DY(IY) * RY)
          OpenAD_aux_26 = ((OpenAD_aux_27 / OpenAD_aux_28) +(
     > OpenAD_aux_29 / OpenAD_aux_30))
          OpenAD_lin_29 = __value__(HU(IX + 1, IY))
          OpenAD_lin_30 = __value__(LOCAL_U(IX + 1, IY))
          OpenAD_lin_31 = __value__(HU(IX, IY))
          OpenAD_lin_32 = __value__(LOCAL_U(IX, IY))
          OpenAD_lin_28 = (INT(1_w2f__i8) / OpenAD_aux_28)
          OpenAD_lin_34 = OpenAD_aux_31
          OpenAD_lin_36 = HY(IY + 1)
          OpenAD_lin_35 = __value__(LOCAL_V(IX, IY + 1))
          OpenAD_lin_37 = OpenAD_aux_32
          OpenAD_lin_39 = HY(IY)
          OpenAD_lin_38 = __value__(LOCAL_V(IX, IY))
          OpenAD_lin_33 = (INT(1_w2f__i8) / OpenAD_aux_30)
          OpenAD_lin_27 = OpenAD_aux_25
          __value__(LOCAL_ETA(INT(IX), INT(IY))) = (__value__(LOCAL_ETA
     > (IX, IY)) - OpenAD_aux_25 * OpenAD_aux_26)
          OpenAD_acc_10 = (OpenAD_lin_27 * INT((-1_w2f__i8)))
          OpenAD_acc_11 = (OpenAD_lin_33 * OpenAD_acc_10)
          OpenAD_acc_12 = (OpenAD_lin_28 * OpenAD_acc_10)
          OpenAD_acc_13 = (INT((-1_w2f__i8)) * OpenAD_acc_11)
          OpenAD_acc_14 = (OpenAD_lin_37 * OpenAD_acc_13)
          OpenAD_acc_15 = (OpenAD_lin_39 * OpenAD_lin_38 *
     >  OpenAD_acc_13)
          OpenAD_acc_16 = (OpenAD_lin_34 * OpenAD_acc_11)
          OpenAD_acc_17 = (OpenAD_lin_36 * OpenAD_lin_35 *
     >  OpenAD_acc_11)
          OpenAD_acc_18 = (INT((-1_w2f__i8)) * OpenAD_acc_12)
          OpenAD_acc_19 = (OpenAD_lin_31 * OpenAD_acc_18)
          OpenAD_acc_20 = (OpenAD_lin_32 * OpenAD_acc_18)
          OpenAD_acc_21 = (OpenAD_lin_29 * OpenAD_acc_12)
          OpenAD_acc_22 = (OpenAD_lin_30 * OpenAD_acc_12)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_14)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_15)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_16)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_17)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_19)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_20)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_21)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_22)
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(LOCAL_ETA(0, INT(IY))) = __value__(LOCAL_ETA(20, IY
     > ))
        END DO
        OpenAD_Symbol_1066 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1066)
      ELSE
        OpenAD_Symbol_1065 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1065)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(LOCAL_ETA(INT(IX), 0)) = __value__(LOCAL_ETA(IX, 20
     > ))
        END DO
        OpenAD_Symbol_1068 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1068)
      ELSE
        OpenAD_Symbol_1067 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1067)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1063)
      IF(OpenAD_Symbol_1063 .ne. 0) THEN
        IX = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IX .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_ETA(IX, 0)), __deriv__(
     > OpenAD_prp_12))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_ETA(IX, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_12), __deriv__(LOCAL_ETA(
     > IX, 20)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_12))
          IX = IX - 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1064)
      IF(OpenAD_Symbol_1064 .ne. 0) THEN
        IY = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IY .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_ETA(0, IY)), __deriv__(
     > OpenAD_prp_11))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_ETA(0, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_11), __deriv__(LOCAL_ETA(
     > 20, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_11))
          IY = IY - 1
        END DO
      ENDIF
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1348)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1349)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1350)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1351)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1352)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1353)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1354)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1355)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1348, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(HU(IX + 1, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1349, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(LOCAL_U(IX + 1, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1350, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(HU(IX, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1351, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(LOCAL_U(IX, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1352, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(HV(IX, IY + 1)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1353, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(LOCAL_V(IX, IY + 1)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1354, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(HV(IX, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1355, __deriv__(LOCAL_ETA(IX, IY)),
     >  __deriv__(LOCAL_V(IX, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_ETA(IX, IY)), __deriv__(
     > OpenAD_prp_10))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_ETA(IX, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_10), __deriv__(LOCAL_ETA(
     > IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_10))
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1079 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1080 = 0_w2f__i8
        DO IX = 1, 20, 1
          OpenAD_aux_25 = (ETAMASK(IX, IY) * DT)
          OpenAD_aux_27 = (__value__(LOCAL_U(IX + 1, IY)) * __value__(
     > HU(IX + 1, IY)) - __value__(LOCAL_U(IX, IY)) * __value__(HU(IX,
     >  IY)))
          OpenAD_aux_28 = (DX(IX) * RX(IY))
          OpenAD_aux_31 = (HY(IY + 1) * __value__(HV(IX, IY + 1)))
          OpenAD_aux_32 = (HY(IY) * __value__(HV(IX, IY)))
          OpenAD_aux_29 = (__value__(LOCAL_V(IX, IY + 1)) *
     >  OpenAD_aux_31 - __value__(LOCAL_V(IX, IY)) * OpenAD_aux_32)
          OpenAD_aux_30 = (DY(IY) * RY)
          OpenAD_aux_26 = ((OpenAD_aux_27 / OpenAD_aux_28) +(
     > OpenAD_aux_29 / OpenAD_aux_30))
          OpenAD_lin_29 = __value__(HU(IX + 1, IY))
          OpenAD_lin_30 = __value__(LOCAL_U(IX + 1, IY))
          OpenAD_lin_31 = __value__(HU(IX, IY))
          OpenAD_lin_32 = __value__(LOCAL_U(IX, IY))
          OpenAD_lin_28 = (INT(1_w2f__i8) / OpenAD_aux_28)
          OpenAD_lin_34 = OpenAD_aux_31
          OpenAD_lin_36 = HY(IY + 1)
          OpenAD_lin_35 = __value__(LOCAL_V(IX, IY + 1))
          OpenAD_lin_37 = OpenAD_aux_32
          OpenAD_lin_39 = HY(IY)
          OpenAD_lin_38 = __value__(LOCAL_V(IX, IY))
          OpenAD_lin_33 = (INT(1_w2f__i8) / OpenAD_aux_30)
          OpenAD_lin_27 = OpenAD_aux_25
          __value__(LOCAL_ETA(INT(IX), INT(IY))) = (__value__(LOCAL_ETA
     > (IX, IY)) - OpenAD_aux_25 * OpenAD_aux_26)
          OpenAD_acc_10 = (OpenAD_lin_27 * INT((-1_w2f__i8)))
          OpenAD_acc_11 = (OpenAD_lin_33 * OpenAD_acc_10)
          OpenAD_acc_12 = (OpenAD_lin_28 * OpenAD_acc_10)
          OpenAD_acc_13 = (INT((-1_w2f__i8)) * OpenAD_acc_11)
          OpenAD_acc_14 = (OpenAD_lin_37 * OpenAD_acc_13)
          OpenAD_acc_15 = (OpenAD_lin_39 * OpenAD_lin_38 *
     >  OpenAD_acc_13)
          OpenAD_acc_16 = (OpenAD_lin_34 * OpenAD_acc_11)
          OpenAD_acc_17 = (OpenAD_lin_36 * OpenAD_lin_35 *
     >  OpenAD_acc_11)
          OpenAD_acc_18 = (INT((-1_w2f__i8)) * OpenAD_acc_12)
          OpenAD_acc_19 = (OpenAD_lin_31 * OpenAD_acc_18)
          OpenAD_acc_20 = (OpenAD_lin_32 * OpenAD_acc_18)
          OpenAD_acc_21 = (OpenAD_lin_29 * OpenAD_acc_12)
          OpenAD_acc_22 = (OpenAD_lin_30 * OpenAD_acc_12)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_14)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_15)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_16)
          OpenAD_Symbol_1087 = (IY + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1087)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_17)
          OpenAD_Symbol_1088 = (IY + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1088)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_19)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_20)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_21)
          OpenAD_Symbol_1089 = (IX + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1089)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_acc_22)
          OpenAD_Symbol_1090 = (IX + 1)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1090)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1080 = (INT(OpenAD_Symbol_1080) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1080)
        OpenAD_Symbol_1079 = (INT(OpenAD_Symbol_1079) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1079)
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1081 = 0_w2f__i8
        DO IY = 0, 21, 1
          __value__(LOCAL_ETA(0, INT(IY))) = __value__(LOCAL_ETA(20, IY
     > ))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1081 = (INT(OpenAD_Symbol_1081) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1081)
        OpenAD_Symbol_1083 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1083)
      ELSE
        OpenAD_Symbol_1082 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1082)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1084 = 0_w2f__i8
        DO IX = 0, 21, 1
          __value__(LOCAL_ETA(INT(IX), 0)) = __value__(LOCAL_ETA(IX, 20
     > ))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
          OpenAD_Symbol_1084 = (INT(OpenAD_Symbol_1084) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1084)
        OpenAD_Symbol_1086 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1086)
      ELSE
        OpenAD_Symbol_1085 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1085)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1069)
      IF(OpenAD_Symbol_1069 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1070)
        OpenAD_Symbol_1071 = 1
        DO WHILE(INT(OpenAD_Symbol_1071) .LE. INT(OpenAD_Symbol_1070))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1361)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_ETA(OpenAD_Symbol_1361, 0)),
     >  __deriv__(OpenAD_prp_12))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_ETA(OpenAD_Symbol_1361, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_12), __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1361, 20)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_12))
          OpenAD_Symbol_1071 = INT(OpenAD_Symbol_1071) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1072)
      IF(OpenAD_Symbol_1072 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1073)
        OpenAD_Symbol_1074 = 1
        DO WHILE(INT(OpenAD_Symbol_1074) .LE. INT(OpenAD_Symbol_1073))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1360)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_ETA(0, OpenAD_Symbol_1360)),
     >  __deriv__(OpenAD_prp_11))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_ETA(0, OpenAD_Symbol_1360)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_11), __deriv__(LOCAL_ETA(
     > 20, OpenAD_Symbol_1360)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_11))
          OpenAD_Symbol_1074 = INT(OpenAD_Symbol_1074) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1075)
      OpenAD_Symbol_1076 = 1
      DO WHILE(INT(OpenAD_Symbol_1076) .LE. INT(OpenAD_Symbol_1075))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1077)
        OpenAD_Symbol_1078 = 1
        DO WHILE(INT(OpenAD_Symbol_1078) .LE. INT(OpenAD_Symbol_1077))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1346)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1347)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1348)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1349)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1350)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1351)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1352)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1353)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1354)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1355)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1356)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1348, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(HU(
     > OpenAD_Symbol_1356, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1357)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1349, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1357, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1350, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(HU(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1351, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(LOCAL_U(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1358)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1352, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(HV(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1358)))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1359)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1353, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1359)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1354, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(HV(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1355, __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)), __deriv__(LOCAL_V(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(LOCAL_ETA(OpenAD_Symbol_1347,
     >  OpenAD_Symbol_1346)), __deriv__(OpenAD_prp_10))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(LOCAL_ETA(OpenAD_Symbol_1347,
     >  OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_10), __deriv__(LOCAL_ETA(
     > OpenAD_Symbol_1347, OpenAD_Symbol_1346)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_10))
          OpenAD_Symbol_1078 = INT(OpenAD_Symbol_1078) + 1
        END DO
        OpenAD_Symbol_1076 = INT(OpenAD_Symbol_1076) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE calc_overturning(OVERTURNING, LOCAL_U)
      use w2f__types
      use size
      use vars
      use pfields
      use size
      use vars
      use pfields
      use size
      use vars
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1091
      INTEGER(w2f__i8) OpenAD_Symbol_1092
      INTEGER(w2f__i8) OpenAD_Symbol_1093
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) OVERTURNING
      REAL(w2f__8) LOCAL_U(0 : 21, 0 : 21)
      INTENT(IN)  LOCAL_U
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      PARAMETER ( IX = 7)
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      OVERTURNING = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 10, 1
        OVERTURNING = (OVERTURNING + __value__(HU(7, IY)) * DY(IY) *
     >  LOCAL_U(7, IY))
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      OVERTURNING = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 10, 1
        OVERTURNING = (OVERTURNING + __value__(HU(7, IY)) * DY(IY) *
     >  LOCAL_U(7, IY))
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((10 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      OVERTURNING = 0.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1093 = 0_w2f__i8
      DO IY = 1, 10, 1
        OVERTURNING = (OVERTURNING + __value__(HU(7, IY)) * DY(IY) *
     >  LOCAL_U(7, IY))
        OpenAD_Symbol_1093 = (INT(OpenAD_Symbol_1093) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1093)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1091)
      OpenAD_Symbol_1092 = 1
      DO WHILE(INT(OpenAD_Symbol_1092) .LE. INT(OpenAD_Symbol_1091))
        OpenAD_Symbol_1092 = INT(OpenAD_Symbol_1092) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE calc_zonal_transport_split(ZONAL_TRANSPORT, LOCAL_U)
      use w2f__types
      use size
      use vars
      use pfields
      use size
      use vars
      use pfields
      use size
      use vars
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1094
      INTEGER(w2f__i8) OpenAD_Symbol_1095
      INTEGER(w2f__i8) OpenAD_Symbol_1096
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) ZONAL_TRANSPORT
      REAL(w2f__8) LOCAL_U(0 : 21, 0 : 21)
      INTENT(IN)  LOCAL_U
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      PARAMETER ( IX = 6)
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        ZONAL_TRANSPORT = (ZONAL_TRANSPORT + __value__(HU(6, IY)) * DY(
     > IY) * LOCAL_U(6, IY))
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        ZONAL_TRANSPORT = (ZONAL_TRANSPORT + __value__(HU(6, IY)) * DY(
     > IY) * LOCAL_U(6, IY))
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(LOCAL_U)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(LOCAL_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(LOCAL_U)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(LOCAL_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1096 = 0_w2f__i8
      DO IY = 1, 20, 1
        ZONAL_TRANSPORT = (ZONAL_TRANSPORT + __value__(HU(6, IY)) * DY(
     > IY) * LOCAL_U(6, IY))
        OpenAD_Symbol_1096 = (INT(OpenAD_Symbol_1096) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1096)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1094)
      OpenAD_Symbol_1095 = 1
      DO WHILE(INT(OpenAD_Symbol_1095) .LE. INT(OpenAD_Symbol_1094))
        OpenAD_Symbol_1095 = INT(OpenAD_Symbol_1095) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE initial_values()
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use size
      use parms
      use vars
      use pfields
      use size
      use parms
      use vars
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1097
      INTEGER(w2f__i8) OpenAD_Symbol_1098
      INTEGER(w2f__i8) OpenAD_Symbol_1099
      INTEGER(w2f__i8) OpenAD_Symbol_1100
      INTEGER(w2f__i8) OpenAD_Symbol_1101
      INTEGER(w2f__i8) OpenAD_Symbol_1102
      INTEGER(w2f__i8) OpenAD_Symbol_1103
      INTEGER(w2f__i8) OpenAD_Symbol_1104
      INTEGER(w2f__i8) OpenAD_Symbol_1105
      INTEGER(w2f__i8) OpenAD_Symbol_1106
      INTEGER(w2f__i8) OpenAD_Symbol_1107
      INTEGER(w2f__i8) OpenAD_Symbol_1108
      INTEGER(w2f__i8) OpenAD_Symbol_1109
      INTEGER(w2f__i8) OpenAD_Symbol_1110
      INTEGER(w2f__i8) OpenAD_Symbol_1111
      INTEGER(w2f__i8) OpenAD_Symbol_1112
      INTEGER(w2f__i8) OpenAD_Symbol_1113
      INTEGER(w2f__i8) OpenAD_Symbol_1114
      INTEGER(w2f__i8) OpenAD_Symbol_1115
      INTEGER(w2f__i8) OpenAD_Symbol_1116
      INTEGER(w2f__i8) OpenAD_Symbol_1117
      INTEGER(w2f__i8) OpenAD_Symbol_1118
      INTEGER(w2f__i8) OpenAD_Symbol_1119
      INTEGER(w2f__i8) OpenAD_Symbol_1120
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1362
      INTEGER(w2f__i4) OpenAD_Symbol_1363
      INTEGER(w2f__i4) OpenAD_Symbol_1364
      INTEGER(w2f__i4) OpenAD_Symbol_1365
      TYPE (oadactive) OpenAD_prp_13
      TYPE (oadactive) OpenAD_prp_14
      TYPE (oadactive) OpenAD_prp_15
      TYPE (oadactive) OpenAD_prp_16
      TYPE (oadactive) OpenAD_prp_17
      TYPE (oadactive) OpenAD_prp_18
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          __value__(U(INT(IX), INT(IY))) = (UINI(IX, IY) * UMASK(IX, IY
     > ))
          __value__(V(INT(IX), INT(IY))) = (VINI(IX, IY) * VMASK(IX, IY
     > ))
          __value__(ETA(INT(IX), INT(IY))) = (ETAINI(IX, IY) * ETAMASK(
     > IX, IY))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(U(21, INT(IY))) = __value__(U(1, IY))
          __value__(V(0, INT(IY))) = __value__(V(20, IY))
          __value__(ETA(0, INT(IY))) = __value__(ETA(20, IY))
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(U(INT(IX), 0)) = __value__(U(IX, 20))
          __value__(V(INT(IX), 21)) = __value__(V(IX, 1))
          __value__(ETA(INT(IX), 0)) = __value__(ETA(IX, 0))
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          __value__(U(INT(IX), INT(IY))) = (UINI(IX, IY) * UMASK(IX, IY
     > ))
          __value__(V(INT(IX), INT(IY))) = (VINI(IX, IY) * VMASK(IX, IY
     > ))
          __value__(ETA(INT(IX), INT(IY))) = (ETAINI(IX, IY) * ETAMASK(
     > IX, IY))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0, 21, 1
          __value__(U(21, INT(IY))) = __value__(U(1, IY))
          __value__(V(0, INT(IY))) = __value__(V(20, IY))
          __value__(ETA(0, INT(IY))) = __value__(ETA(20, IY))
        END DO
        OpenAD_Symbol_1100 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1100)
      ELSE
        OpenAD_Symbol_1099 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1099)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0, 21, 1
          __value__(U(INT(IX), 0)) = __value__(U(IX, 20))
          __value__(V(INT(IX), 21)) = __value__(V(IX, 1))
          __value__(ETA(INT(IX), 0)) = __value__(ETA(IX, 0))
        END DO
        OpenAD_Symbol_1102 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1102)
      ELSE
        OpenAD_Symbol_1101 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1101)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1097)
      IF(OpenAD_Symbol_1097 .ne. 0) THEN
        IX = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IX .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(ETA(IX, 0)), __deriv__(OpenAD_prp_18)
     > )
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(IX, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(V(IX, 21)), __deriv__(OpenAD_prp_17))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(IX, 21)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(U(IX, 0)), __deriv__(OpenAD_prp_16))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(IX, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_18), __deriv__(ETA(IX, 0))
     > )
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_18))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_17), __deriv__(V(IX, 1)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_17))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_16), __deriv__(U(IX, 20)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_16))
          IX = IX - 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1098)
      IF(OpenAD_Symbol_1098 .ne. 0) THEN
        IY = 0 + 1 *((21 - 0) / 1)
        DO WHILE(IY .GE. 0)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(ETA(0, IY)), __deriv__(OpenAD_prp_15)
     > )
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(0, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(V(0, IY)), __deriv__(OpenAD_prp_14))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(0, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(U(21, IY)), __deriv__(OpenAD_prp_13))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(21, IY)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_15), __deriv__(ETA(20, IY)
     > ))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_15))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_14), __deriv__(V(20, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_14))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_13), __deriv__(U(1, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_13))
          IY = IY - 1
        END DO
      ENDIF
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(IX, IY)))
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1113 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1114 = 0_w2f__i8
        DO IX = 1, 20, 1
          __value__(U(INT(IX), INT(IY))) = (UINI(IX, IY) * UMASK(IX, IY
     > ))
          __value__(V(INT(IX), INT(IY))) = (VINI(IX, IY) * VMASK(IX, IY
     > ))
          __value__(ETA(INT(IX), INT(IY))) = (ETAINI(IX, IY) * ETAMASK(
     > IX, IY))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1114 = (INT(OpenAD_Symbol_1114) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1114)
        OpenAD_Symbol_1113 = (INT(OpenAD_Symbol_1113) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1113)
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1115 = 0_w2f__i8
        DO IY = 0, 21, 1
          __value__(U(21, INT(IY))) = __value__(U(1, IY))
          __value__(V(0, INT(IY))) = __value__(V(20, IY))
          __value__(ETA(0, INT(IY))) = __value__(ETA(20, IY))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          OpenAD_Symbol_1115 = (INT(OpenAD_Symbol_1115) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1115)
        OpenAD_Symbol_1117 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1117)
      ELSE
        OpenAD_Symbol_1116 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1116)
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        OpenAD_Symbol_1118 = 0_w2f__i8
        DO IX = 0, 21, 1
          __value__(U(INT(IX), 0)) = __value__(U(IX, 20))
          __value__(V(INT(IX), 21)) = __value__(V(IX, 1))
          __value__(ETA(INT(IX), 0)) = __value__(ETA(IX, 0))
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
          OpenAD_Symbol_1118 = (INT(OpenAD_Symbol_1118) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1118)
        OpenAD_Symbol_1120 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1120)
      ELSE
        OpenAD_Symbol_1119 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1119)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1103)
      IF(OpenAD_Symbol_1103 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1104)
        OpenAD_Symbol_1105 = 1
        DO WHILE(INT(OpenAD_Symbol_1105) .LE. INT(OpenAD_Symbol_1104))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1365)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(ETA(OpenAD_Symbol_1365, 0)),
     >  __deriv__(OpenAD_prp_18))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(OpenAD_Symbol_1365, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(V(OpenAD_Symbol_1365, 21)), __deriv__
     > (OpenAD_prp_17))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(OpenAD_Symbol_1365, 21)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(U(OpenAD_Symbol_1365, 0)), __deriv__(
     > OpenAD_prp_16))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(OpenAD_Symbol_1365, 0)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_18), __deriv__(ETA(
     > OpenAD_Symbol_1365, 0)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_18))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_17), __deriv__(V(
     > OpenAD_Symbol_1365, 1)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_17))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_16), __deriv__(U(
     > OpenAD_Symbol_1365, 20)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_16))
          OpenAD_Symbol_1105 = INT(OpenAD_Symbol_1105) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1106)
      IF(OpenAD_Symbol_1106 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1107)
        OpenAD_Symbol_1108 = 1
        DO WHILE(INT(OpenAD_Symbol_1108) .LE. INT(OpenAD_Symbol_1107))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1364)
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(ETA(0, OpenAD_Symbol_1364)),
     >  __deriv__(OpenAD_prp_15))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(0, OpenAD_Symbol_1364)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(V(0, OpenAD_Symbol_1364)), __deriv__(
     > OpenAD_prp_14))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(0, OpenAD_Symbol_1364)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(U(21, OpenAD_Symbol_1364)), __deriv__
     > (OpenAD_prp_13))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(21, OpenAD_Symbol_1364)))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_15), __deriv__(ETA(20,
     >  OpenAD_Symbol_1364)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_15))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_14), __deriv__(V(20,
     >  OpenAD_Symbol_1364)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_14))
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(OpenAD_prp_13), __deriv__(U(1,
     >  OpenAD_Symbol_1364)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(OpenAD_prp_13))
          OpenAD_Symbol_1108 = INT(OpenAD_Symbol_1108) + 1
        END DO
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1109)
      OpenAD_Symbol_1110 = 1
      DO WHILE(INT(OpenAD_Symbol_1110) .LE. INT(OpenAD_Symbol_1109))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1111)
        OpenAD_Symbol_1112 = 1
        DO WHILE(INT(OpenAD_Symbol_1112) .LE. INT(OpenAD_Symbol_1111))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1362)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1363)
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(ETA(OpenAD_Symbol_1363,
     >  OpenAD_Symbol_1362)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(V(OpenAD_Symbol_1363,
     >  OpenAD_Symbol_1362)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(U(OpenAD_Symbol_1363,
     >  OpenAD_Symbol_1362)))
          OpenAD_Symbol_1112 = INT(OpenAD_Symbol_1112) + 1
        END DO
        OpenAD_Symbol_1110 = INT(OpenAD_Symbol_1110) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(V))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(ETA))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE calc_depth_uv()
      use w2f__types
      use size
      use pfields
      use size
      use pfields
      use size
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1121
      INTEGER(w2f__i8) OpenAD_Symbol_1122
      INTEGER(w2f__i8) OpenAD_Symbol_1123
      INTEGER(w2f__i8) OpenAD_Symbol_1124
      INTEGER(w2f__i8) OpenAD_Symbol_1125
      INTEGER(w2f__i8) OpenAD_Symbol_1126
      INTEGER(w2f__i8) OpenAD_Symbol_1127
      INTEGER(w2f__i8) OpenAD_Symbol_1128
      INTEGER(w2f__i8) OpenAD_Symbol_1129
      INTEGER(w2f__i8) OpenAD_Symbol_1130
      INTEGER(w2f__i8) OpenAD_Symbol_1131
      INTEGER(w2f__i8) OpenAD_Symbol_1132
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1366
      INTEGER(w2f__i4) OpenAD_Symbol_1367
      REAL(w2f__8) OpenAD_Symbol_1368
      REAL(w2f__8) OpenAD_Symbol_1369
      INTEGER(w2f__i4) OpenAD_Symbol_1370
      INTEGER(w2f__i4) OpenAD_Symbol_1371
      REAL(w2f__8) OpenAD_Symbol_1372
      INTEGER(w2f__i4) OpenAD_Symbol_1373
      INTEGER(w2f__i4) OpenAD_Symbol_1374
      REAL(w2f__8) OpenAD_Symbol_1375
      REAL(w2f__8) OpenAD_lin_40
      REAL(w2f__8) OpenAD_lin_41
      REAL(w2f__8) OpenAD_lin_42
      REAL(w2f__8) OpenAD_lin_43
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 21, 1
        DO IX = 1, 21, 1
          __value__(HU(INT(IX), INT(IY))) = (__value__(DEPTH(IX, IY)) *
     >  UMASK(IX, IY))
          __value__(HV(INT(IX), INT(IY))) = (__value__(DEPTH(IX, IY)) *
     >  VMASK(IX, IY))
          IF(__value__(HU(IX, IY)) .ne. 0.0D00) THEN
            __value__(INVHU(INT(IX), INT(IY))) = (1.0D00 / __value__(HU
     > (IX, IY)))
          ENDIF
          IF(__value__(HV(IX, IY)) .ne. 0.0D00) THEN
            __value__(INVHV(INT(IX), INT(IY))) = (1.0D00 / __value__(HV
     > (IX, IY)))
          ENDIF
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 21, 1
        DO IX = 1, 21, 1
          OpenAD_lin_40 = UMASK(IX, IY)
          __value__(HU(INT(IX), INT(IY))) = (__value__(DEPTH(IX, IY)) *
     >  UMASK(IX, IY))
          OpenAD_lin_41 = VMASK(IX, IY)
          __value__(HV(INT(IX), INT(IY))) = (__value__(DEPTH(IX, IY)) *
     >  VMASK(IX, IY))
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_lin_40)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_lin_41)
          IF(__value__(HU(IX, IY)) .ne. 0.0D00) THEN
            OpenAD_lin_42 = (-(1.0D00 /(__value__(HU(IX, IY)) *
     >  __value__(HU(IX, IY)))))
            __value__(INVHU(INT(IX), INT(IY))) = (1.0D00 / __value__(HU
     > (IX, IY)))
C           $OpenAD$ INLINE push_s0(subst)
            CALL push_s0(OpenAD_lin_42)
          ENDIF
          IF(__value__(HV(IX, IY)) .ne. 0.0D00) THEN
            OpenAD_lin_43 = (-(1.0D00 /(__value__(HV(IX, IY)) *
     >  __value__(HV(IX, IY)))))
            __value__(INVHV(INT(IX), INT(IY))) = (1.0D00 / __value__(HV
     > (IX, IY)))
C           $OpenAD$ INLINE push_s0(subst)
            CALL push_s0(OpenAD_lin_43)
          ENDIF
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((21 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((21 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IF(__value__(HV(IX, IY)) .ne. 0.0D00) THEN
C           $OpenAD$ INLINE pop_s0(subst)
            CALL pop_s0(OpenAD_Symbol_1375)
C           $OpenAD$ INLINE Saxpy(subst,subst,subst)
            CALL Saxpy(OpenAD_Symbol_1375, __deriv__(INVHV(IX, IY)),
     >  __deriv__(HV(IX, IY)))
C           $OpenAD$ INLINE ZeroDeriv(subst)
            CALL ZeroDeriv(__deriv__(INVHV(IX, IY)))
          ENDIF
          IF(__value__(HU(IX, IY)) .ne. 0.0D00) THEN
C           $OpenAD$ INLINE pop_s0(subst)
            CALL pop_s0(OpenAD_Symbol_1372)
C           $OpenAD$ INLINE Saxpy(subst,subst,subst)
            CALL Saxpy(OpenAD_Symbol_1372, __deriv__(INVHU(IX, IY)),
     >  __deriv__(HU(IX, IY)))
C           $OpenAD$ INLINE ZeroDeriv(subst)
            CALL ZeroDeriv(__deriv__(INVHU(IX, IY)))
          ENDIF
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1368)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1369)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1368, __deriv__(HV(IX, IY)),
     >  __deriv__(DEPTH(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HV(IX, IY)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1369, __deriv__(HU(IX, IY)),
     >  __deriv__(DEPTH(IX, IY)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HU(IX, IY)))
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1127 = 0_w2f__i8
      DO IY = 1, 21, 1
        OpenAD_Symbol_1128 = 0_w2f__i8
        DO IX = 1, 21, 1
          OpenAD_lin_40 = UMASK(IX, IY)
          __value__(HU(INT(IX), INT(IY))) = (__value__(DEPTH(IX, IY)) *
     >  UMASK(IX, IY))
          OpenAD_lin_41 = VMASK(IX, IY)
          __value__(HV(INT(IX), INT(IY))) = (__value__(DEPTH(IX, IY)) *
     >  VMASK(IX, IY))
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_lin_40)
C         $OpenAD$ INLINE push_s0(subst)
          CALL push_s0(OpenAD_lin_41)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IX)
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(IY)
          IF(__value__(HU(IX, IY)) .ne. 0.0D00) THEN
            OpenAD_lin_42 = (-(1.0D00 /(__value__(HU(IX, IY)) *
     >  __value__(HU(IX, IY)))))
            __value__(INVHU(INT(IX), INT(IY))) = (1.0D00 / __value__(HU
     > (IX, IY)))
C           $OpenAD$ INLINE push_s0(subst)
            CALL push_s0(OpenAD_lin_42)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IX)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IY)
            OpenAD_Symbol_1129 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_1129)
          ELSE
            OpenAD_Symbol_1130 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_1130)
          ENDIF
          IF(__value__(HV(IX, IY)) .ne. 0.0D00) THEN
            OpenAD_lin_43 = (-(1.0D00 /(__value__(HV(IX, IY)) *
     >  __value__(HV(IX, IY)))))
            __value__(INVHV(INT(IX), INT(IY))) = (1.0D00 / __value__(HV
     > (IX, IY)))
C           $OpenAD$ INLINE push_s0(subst)
            CALL push_s0(OpenAD_lin_43)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IX)
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(IY)
            OpenAD_Symbol_1131 = 1_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_1131)
          ELSE
            OpenAD_Symbol_1132 = 0_w2f__i8
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_1132)
          ENDIF
          OpenAD_Symbol_1128 = (INT(OpenAD_Symbol_1128) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1128)
        OpenAD_Symbol_1127 = (INT(OpenAD_Symbol_1127) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1127)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1121)
      OpenAD_Symbol_1122 = 1
      DO WHILE(INT(OpenAD_Symbol_1122) .LE. INT(OpenAD_Symbol_1121))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1123)
        OpenAD_Symbol_1124 = 1
        DO WHILE(INT(OpenAD_Symbol_1124) .LE. INT(OpenAD_Symbol_1123))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1125)
          IF(OpenAD_Symbol_1125 .ne. 0) THEN
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1373)
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1374)
C           $OpenAD$ INLINE pop_s0(subst)
            CALL pop_s0(OpenAD_Symbol_1375)
C           $OpenAD$ INLINE Saxpy(subst,subst,subst)
            CALL Saxpy(OpenAD_Symbol_1375, __deriv__(INVHV(
     > OpenAD_Symbol_1374, OpenAD_Symbol_1373)), __deriv__(HV(
     > OpenAD_Symbol_1374, OpenAD_Symbol_1373)))
C           $OpenAD$ INLINE ZeroDeriv(subst)
            CALL ZeroDeriv(__deriv__(INVHV(OpenAD_Symbol_1374,
     >  OpenAD_Symbol_1373)))
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1126)
          IF(OpenAD_Symbol_1126 .ne. 0) THEN
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1370)
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1371)
C           $OpenAD$ INLINE pop_s0(subst)
            CALL pop_s0(OpenAD_Symbol_1372)
C           $OpenAD$ INLINE Saxpy(subst,subst,subst)
            CALL Saxpy(OpenAD_Symbol_1372, __deriv__(INVHU(
     > OpenAD_Symbol_1371, OpenAD_Symbol_1370)), __deriv__(HU(
     > OpenAD_Symbol_1371, OpenAD_Symbol_1370)))
C           $OpenAD$ INLINE ZeroDeriv(subst)
            CALL ZeroDeriv(__deriv__(INVHU(OpenAD_Symbol_1371,
     >  OpenAD_Symbol_1370)))
          ENDIF
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1366)
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1367)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1368)
C         $OpenAD$ INLINE pop_s0(subst)
          CALL pop_s0(OpenAD_Symbol_1369)
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1368, __deriv__(HV(
     > OpenAD_Symbol_1367, OpenAD_Symbol_1366)), __deriv__(DEPTH(
     > OpenAD_Symbol_1367, OpenAD_Symbol_1366)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HV(OpenAD_Symbol_1367,
     >  OpenAD_Symbol_1366)))
C         $OpenAD$ INLINE Saxpy(subst,subst,subst)
          CALL Saxpy(OpenAD_Symbol_1369, __deriv__(HU(
     > OpenAD_Symbol_1367, OpenAD_Symbol_1366)), __deriv__(DEPTH(
     > OpenAD_Symbol_1367, OpenAD_Symbol_1366)))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(HU(OpenAD_Symbol_1367,
     >  OpenAD_Symbol_1366)))
          OpenAD_Symbol_1124 = INT(OpenAD_Symbol_1124) + 1
        END DO
        OpenAD_Symbol_1122 = INT(OpenAD_Symbol_1122) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(INVHV))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(HU))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE calc_zonal_transport_joint(ZONAL_TRANSPORT, LOCAL_U)
      use w2f__types
      use size
      use vars
      use pfields
      use size
      use vars
      use pfields
      use size
      use vars
      use pfields
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1133
      INTEGER(w2f__i8) OpenAD_Symbol_1134
      INTEGER(w2f__i8) OpenAD_Symbol_1135
C
C     **** Parameters and Result ****
C
      TYPE (oadactive) ZONAL_TRANSPORT
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      PARAMETER ( IX = 6)
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) OpenAD_Symbol_1376
      REAL(w2f__8) OpenAD_Symbol_1377
      REAL(w2f__8) OpenAD_Symbol_1378
      REAL(w2f__8) OpenAD_acc_23
      REAL(w2f__8) OpenAD_aux_33
      REAL(w2f__8) OpenAD_lin_44
      REAL(w2f__8) OpenAD_lin_45
      REAL(w2f__8) OpenAD_lin_46
      TYPE (oadactive) OpenAD_prp_19
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
      __value__(ZONAL_TRANSPORT) = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        __value__(ZONAL_TRANSPORT) = (__value__(ZONAL_TRANSPORT) +
     >  __value__(HU(6, IY)) * DY(IY) * __value__(LOCAL_U(6, IY)))
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
      __value__(ZONAL_TRANSPORT) = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        OpenAD_aux_33 = (DY(IY) * __value__(LOCAL_U(6, IY)))
        OpenAD_lin_44 = OpenAD_aux_33
        OpenAD_lin_46 = DY(IY)
        OpenAD_lin_45 = __value__(HU(6, IY))
        __value__(ZONAL_TRANSPORT) = (__value__(ZONAL_TRANSPORT) +
     >  __value__(HU(6, IY)) * OpenAD_aux_33)
        OpenAD_acc_23 = (OpenAD_lin_46 * OpenAD_lin_45)
C       $OpenAD$ INLINE push_s0(subst)
        CALL push_s0(OpenAD_lin_44)
C       $OpenAD$ INLINE push_s0(subst)
        CALL push_s0(OpenAD_acc_23)
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
C       $OpenAD$ INLINE pop_s0(subst)
        CALL pop_s0(OpenAD_Symbol_1377)
C       $OpenAD$ INLINE pop_s0(subst)
        CALL pop_s0(OpenAD_Symbol_1378)
C       $OpenAD$ INLINE Saxpy(subst,subst,subst)
        CALL Saxpy(OpenAD_Symbol_1377, __deriv__(ZONAL_TRANSPORT),
     >  __deriv__(LOCAL_U(6, IY)))
C       $OpenAD$ INLINE Saxpy(subst,subst,subst)
        CALL Saxpy(OpenAD_Symbol_1378, __deriv__(ZONAL_TRANSPORT),
     >  __deriv__(HU(6, IY)))
C       $OpenAD$ INLINE IncDeriv(subst,subst)
        CALL IncDeriv(__deriv__(ZONAL_TRANSPORT), __deriv__(
     > OpenAD_prp_19))
C       $OpenAD$ INLINE ZeroDeriv(subst)
        CALL ZeroDeriv(__deriv__(ZONAL_TRANSPORT))
C       $OpenAD$ INLINE IncDeriv(subst,subst)
        CALL IncDeriv(__deriv__(OpenAD_prp_19), __deriv__(
     > ZONAL_TRANSPORT))
C       $OpenAD$ INLINE ZeroDeriv(subst)
        CALL ZeroDeriv(__deriv__(OpenAD_prp_19))
        IY = IY - 1
      END DO
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
      CALL cp_arg_store_real_scalar_a(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
      CALL cp_arg_restore_real_scalar_a(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
      CALL cp_arg_store_real_scalar_a(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
      CALL cp_arg_store_real_scalar_a(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
      CALL cp_arg_restore_real_scalar_a(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
      CALL cp_arg_restore_real_scalar_a(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
      __value__(ZONAL_TRANSPORT) = 0.0D00
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1135 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_aux_33 = (DY(IY) * __value__(LOCAL_U(6, IY)))
        OpenAD_lin_44 = OpenAD_aux_33
        OpenAD_lin_46 = DY(IY)
        OpenAD_lin_45 = __value__(HU(6, IY))
        __value__(ZONAL_TRANSPORT) = (__value__(ZONAL_TRANSPORT) +
     >  __value__(HU(6, IY)) * OpenAD_aux_33)
        OpenAD_acc_23 = (OpenAD_lin_46 * OpenAD_lin_45)
C       $OpenAD$ INLINE push_s0(subst)
        CALL push_s0(OpenAD_lin_44)
C       $OpenAD$ INLINE push_s0(subst)
        CALL push_s0(OpenAD_acc_23)
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(IY)
        OpenAD_Symbol_1135 = (INT(OpenAD_Symbol_1135) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1135)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1133)
      OpenAD_Symbol_1134 = 1
      DO WHILE(INT(OpenAD_Symbol_1134) .LE. INT(OpenAD_Symbol_1133))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1376)
C       $OpenAD$ INLINE pop_s0(subst)
        CALL pop_s0(OpenAD_Symbol_1377)
C       $OpenAD$ INLINE pop_s0(subst)
        CALL pop_s0(OpenAD_Symbol_1378)
C       $OpenAD$ INLINE Saxpy(subst,subst,subst)
        CALL Saxpy(OpenAD_Symbol_1377, __deriv__(ZONAL_TRANSPORT),
     >  __deriv__(LOCAL_U(6, OpenAD_Symbol_1376)))
C       $OpenAD$ INLINE Saxpy(subst,subst,subst)
        CALL Saxpy(OpenAD_Symbol_1378, __deriv__(ZONAL_TRANSPORT),
     >  __deriv__(HU(6, OpenAD_Symbol_1376)))
C       $OpenAD$ INLINE IncDeriv(subst,subst)
        CALL IncDeriv(__deriv__(ZONAL_TRANSPORT), __deriv__(
     > OpenAD_prp_19))
C       $OpenAD$ INLINE ZeroDeriv(subst)
        CALL ZeroDeriv(__deriv__(ZONAL_TRANSPORT))
C       $OpenAD$ INLINE IncDeriv(subst,subst)
        CALL IncDeriv(__deriv__(OpenAD_prp_19), __deriv__(
     > ZONAL_TRANSPORT))
C       $OpenAD$ INLINE ZeroDeriv(subst)
        CALL ZeroDeriv(__deriv__(OpenAD_prp_19))
        OpenAD_Symbol_1134 = INT(OpenAD_Symbol_1134) + 1
      END DO
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_scalar_a_d(subst)
      CALL cp_arg_store_real_scalar_a_d(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_scalar_a_d(subst)
      CALL cp_arg_restore_real_scalar_a_d(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE smoothness_lapldepth(CF)
      use w2f__types
      use size
      use parms
      use pfields
      use weights
      use size
      use parms
      use pfields
      use weights
      use size
      use parms
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1136
      INTEGER(w2f__i8) OpenAD_Symbol_1137
      INTEGER(w2f__i8) OpenAD_Symbol_1138
      INTEGER(w2f__i8) OpenAD_Symbol_1139
      INTEGER(w2f__i8) OpenAD_Symbol_1140
      INTEGER(w2f__i8) OpenAD_Symbol_1141
      INTEGER(w2f__i8) OpenAD_Symbol_1142
      INTEGER(w2f__i8) OpenAD_Symbol_1143
      INTEGER(w2f__i8) OpenAD_Symbol_1144
      INTEGER(w2f__i8) OpenAD_Symbol_1145
      INTEGER(w2f__i8) OpenAD_Symbol_1146
      INTEGER(w2f__i8) OpenAD_Symbol_1147
      INTEGER(w2f__i8) OpenAD_Symbol_1148
      INTEGER(w2f__i8) OpenAD_Symbol_1149
      INTEGER(w2f__i8) OpenAD_Symbol_1150
      INTEGER(w2f__i8) OpenAD_Symbol_1151
      INTEGER(w2f__i8) OpenAD_Symbol_1152
      INTEGER(w2f__i8) OpenAD_Symbol_1153
      INTEGER(w2f__i8) OpenAD_Symbol_1154
      INTEGER(w2f__i8) OpenAD_Symbol_1155
      INTEGER(w2f__i8) OpenAD_Symbol_1156
      INTEGER(w2f__i8) OpenAD_Symbol_1157
      INTEGER(w2f__i8) OpenAD_Symbol_1158
      INTEGER(w2f__i8) OpenAD_Symbol_1159
      INTEGER(w2f__i8) OpenAD_Symbol_1160
      INTEGER(w2f__i8) OpenAD_Symbol_1161
      INTEGER(w2f__i8) OpenAD_Symbol_1162
      INTEGER(w2f__i8) OpenAD_Symbol_1163
      INTEGER(w2f__i8) OpenAD_Symbol_1164
      INTEGER(w2f__i8) OpenAD_Symbol_1165
      INTEGER(w2f__i8) OpenAD_Symbol_1166
      INTEGER(w2f__i8) OpenAD_Symbol_1167
      INTEGER(w2f__i8) OpenAD_Symbol_1168
      INTEGER(w2f__i8) OpenAD_Symbol_1169
      INTEGER(w2f__i8) OpenAD_Symbol_1170
      INTEGER(w2f__i8) OpenAD_Symbol_1171
      INTEGER(w2f__i8) OpenAD_Symbol_1172
      INTEGER(w2f__i8) OpenAD_Symbol_1173
      INTEGER(w2f__i8) OpenAD_Symbol_1174
      INTEGER(w2f__i8) OpenAD_Symbol_1175
      INTEGER(w2f__i8) OpenAD_Symbol_1176
      INTEGER(w2f__i8) OpenAD_Symbol_1177
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) CF
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) DDFDDX(1 : 20, 1 : 20)
      REAL(w2f__8) DDFDDY(1 : 20, 1 : 20)
      REAL(w2f__8) DFDX(1 : 20, 1 : 20)
      REAL(w2f__8) DFDY(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      REAL(w2f__8) LAPLDEPTH
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 19, 1
          DFDX(INT(IX), INT(IY)) = ((__value__(DEPTH(IX + 1, IY)) -
     >  __value__(DEPTH(IX, IY))) / DX(IX + 1))
        END DO
        IF(XPERIODIC) THEN
          DFDX(20, INT(IY)) = ((__value__(DEPTH(1, IY)) - __value__(
     > DEPTH(20, IY))) / DX(1))
        ELSE
          DFDX(20, INT(IY)) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1, 20, 1
        DO IY = 1, 19, 1
          DFDY(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY + 1)) -
     >  __value__(DEPTH(IX, IY))) / DY(IY + 1))
        END DO
        IF(YPERIODIC) THEN
          DFDY(INT(IX), 20) = ((__value__(DEPTH(IX, 1)) - __value__(
     > DEPTH(IX, 20))) / DY(1))
        ELSE
          DFDY(INT(IX), 20) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DDFDDX(1, INT(IY)) = ((DFDX(1, IY) - DFDX(20, IY)) / DX(1))
        ELSE
          DDFDDX(1, INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2, 20, 1
          DDFDDX(INT(IX), INT(IY)) = ((DFDX(IX, IY) - DFDX(IX + (-1),
     >  IY)) / DX(IX))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1, 20, 1
        IF(YPERIODIC) THEN
          DDFDDY(INT(IX), 1) = ((DFDY(IX, 1) - DFDY(IX, 20)) / DY(1))
        ELSE
          DDFDDY(INT(IX), 1) = 0.0D00
        ENDIF
        DO IY = 2, 20, 1
          DDFDDY(INT(IX), INT(IY)) = ((DFDY(IX, IY) - DFDY(IX, IY + (-1
     > ))) / DY(IY))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          LAPLDEPTH = (DDFDDX(IX, IY) + DDFDDY(IX, IY))
          CF = (CF + WEIGHT_LAPLDEPTH(IX, IY) * LAPLDEPTH * LAPLDEPTH *
     >  5.0D-01)
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 19, 1
          DFDX(INT(IX), INT(IY)) = ((__value__(DEPTH(IX + 1, IY)) -
     >  __value__(DEPTH(IX, IY))) / DX(IX + 1))
        END DO
        IF(XPERIODIC) THEN
          DFDX(20, INT(IY)) = ((__value__(DEPTH(1, IY)) - __value__(
     > DEPTH(20, IY))) / DX(1))
        ELSE
          DFDX(20, INT(IY)) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1, 20, 1
        DO IY = 1, 19, 1
          DFDY(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY + 1)) -
     >  __value__(DEPTH(IX, IY))) / DY(IY + 1))
        END DO
        IF(YPERIODIC) THEN
          DFDY(INT(IX), 20) = ((__value__(DEPTH(IX, 1)) - __value__(
     > DEPTH(IX, 20))) / DY(1))
        ELSE
          DFDY(INT(IX), 20) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DDFDDX(1, INT(IY)) = ((DFDX(1, IY) - DFDX(20, IY)) / DX(1))
        ELSE
          DDFDDX(1, INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2, 20, 1
          DDFDDX(INT(IX), INT(IY)) = ((DFDX(IX, IY) - DFDX(IX + (-1),
     >  IY)) / DX(IX))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1, 20, 1
        IF(YPERIODIC) THEN
          DDFDDY(INT(IX), 1) = ((DFDY(IX, 1) - DFDY(IX, 20)) / DY(1))
        ELSE
          DDFDDY(INT(IX), 1) = 0.0D00
        ENDIF
        DO IY = 2, 20, 1
          DDFDDY(INT(IX), INT(IY)) = ((DFDY(IX, IY) - DFDY(IX, IY + (-1
     > ))) / DY(IY))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          LAPLDEPTH = (DDFDDX(IX, IY) + DDFDDY(IX, IY))
          CF = (CF + WEIGHT_LAPLDEPTH(IX, IY) * LAPLDEPTH * LAPLDEPTH *
     >  5.0D-01)
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      IX = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IX .GE. 1)
        IY = 2 + 1 *((20 - 2) / 1)
        DO WHILE(IY .GE. 2)
          IY = IY - 1
        END DO
        IF(YPERIODIC) THEN
        ENDIF
        IX = IX - 1
      END DO
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 2 + 1 *((20 - 2) / 1)
        DO WHILE(IX .GE. 2)
          IX = IX - 1
        END DO
        IF(XPERIODIC) THEN
        ENDIF
        IY = IY - 1
      END DO
      IX = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IX .GE. 1)
        IF(YPERIODIC) THEN
        ENDIF
        IY = 1 + 1 *((19 - 1) / 1)
        DO WHILE(IY .GE. 1)
          IY = IY - 1
        END DO
        IX = IX - 1
      END DO
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IF(XPERIODIC) THEN
        ENDIF
        IX = 1 + 1 *((19 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_LAPLDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1160 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1161 = 0_w2f__i8
        DO IX = 1, 19, 1
          DFDX(INT(IX), INT(IY)) = ((__value__(DEPTH(IX + 1, IY)) -
     >  __value__(DEPTH(IX, IY))) / DX(IX + 1))
          OpenAD_Symbol_1161 = (INT(OpenAD_Symbol_1161) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1161)
        IF(XPERIODIC) THEN
          DFDX(20, INT(IY)) = ((__value__(DEPTH(1, IY)) - __value__(
     > DEPTH(20, IY))) / DX(1))
          OpenAD_Symbol_1162 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1162)
        ELSE
          DFDX(20, INT(IY)) = 0.0D00
          OpenAD_Symbol_1163 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1163)
        ENDIF
        OpenAD_Symbol_1160 = (INT(OpenAD_Symbol_1160) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1160)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1164 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_1165 = 0_w2f__i8
        DO IY = 1, 19, 1
          DFDY(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY + 1)) -
     >  __value__(DEPTH(IX, IY))) / DY(IY + 1))
          OpenAD_Symbol_1165 = (INT(OpenAD_Symbol_1165) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1165)
        IF(YPERIODIC) THEN
          DFDY(INT(IX), 20) = ((__value__(DEPTH(IX, 1)) - __value__(
     > DEPTH(IX, 20))) / DY(1))
          OpenAD_Symbol_1166 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1166)
        ELSE
          DFDY(INT(IX), 20) = 0.0D00
          OpenAD_Symbol_1167 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1167)
        ENDIF
        OpenAD_Symbol_1164 = (INT(OpenAD_Symbol_1164) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1164)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1168 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DDFDDX(1, INT(IY)) = ((DFDX(1, IY) - DFDX(20, IY)) / DX(1))
          OpenAD_Symbol_1169 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1169)
        ELSE
          DDFDDX(1, INT(IY)) = 0.0D00
          OpenAD_Symbol_1170 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1170)
        ENDIF
        OpenAD_Symbol_1171 = 0_w2f__i8
        DO IX = 2, 20, 1
          DDFDDX(INT(IX), INT(IY)) = ((DFDX(IX, IY) - DFDX(IX + (-1),
     >  IY)) / DX(IX))
          OpenAD_Symbol_1171 = (INT(OpenAD_Symbol_1171) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1171)
        OpenAD_Symbol_1168 = (INT(OpenAD_Symbol_1168) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1168)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1172 = 0_w2f__i8
      DO IX = 1, 20, 1
        IF(YPERIODIC) THEN
          DDFDDY(INT(IX), 1) = ((DFDY(IX, 1) - DFDY(IX, 20)) / DY(1))
          OpenAD_Symbol_1173 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1173)
        ELSE
          DDFDDY(INT(IX), 1) = 0.0D00
          OpenAD_Symbol_1174 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1174)
        ENDIF
        OpenAD_Symbol_1175 = 0_w2f__i8
        DO IY = 2, 20, 1
          DDFDDY(INT(IX), INT(IY)) = ((DFDY(IX, IY) - DFDY(IX, IY + (-1
     > ))) / DY(IY))
          OpenAD_Symbol_1175 = (INT(OpenAD_Symbol_1175) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1175)
        OpenAD_Symbol_1172 = (INT(OpenAD_Symbol_1172) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1172)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1176 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1177 = 0_w2f__i8
        DO IX = 1, 20, 1
          LAPLDEPTH = (DDFDDX(IX, IY) + DDFDDY(IX, IY))
          CF = (CF + WEIGHT_LAPLDEPTH(IX, IY) * LAPLDEPTH * LAPLDEPTH *
     >  5.0D-01)
          OpenAD_Symbol_1177 = (INT(OpenAD_Symbol_1177) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1177)
        OpenAD_Symbol_1176 = (INT(OpenAD_Symbol_1176) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1176)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1136)
      OpenAD_Symbol_1137 = 1
      DO WHILE(INT(OpenAD_Symbol_1137) .LE. INT(OpenAD_Symbol_1136))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1138)
        OpenAD_Symbol_1139 = 1
        DO WHILE(INT(OpenAD_Symbol_1139) .LE. INT(OpenAD_Symbol_1138))
          OpenAD_Symbol_1139 = INT(OpenAD_Symbol_1139) + 1
        END DO
        OpenAD_Symbol_1137 = INT(OpenAD_Symbol_1137) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1140)
      OpenAD_Symbol_1141 = 1
      DO WHILE(INT(OpenAD_Symbol_1141) .LE. INT(OpenAD_Symbol_1140))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1142)
        OpenAD_Symbol_1143 = 1
        DO WHILE(INT(OpenAD_Symbol_1143) .LE. INT(OpenAD_Symbol_1142))
          OpenAD_Symbol_1143 = INT(OpenAD_Symbol_1143) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1144)
        IF(OpenAD_Symbol_1144 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_1141 = INT(OpenAD_Symbol_1141) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1145)
      OpenAD_Symbol_1146 = 1
      DO WHILE(INT(OpenAD_Symbol_1146) .LE. INT(OpenAD_Symbol_1145))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1147)
        OpenAD_Symbol_1148 = 1
        DO WHILE(INT(OpenAD_Symbol_1148) .LE. INT(OpenAD_Symbol_1147))
          OpenAD_Symbol_1148 = INT(OpenAD_Symbol_1148) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1149)
        IF(OpenAD_Symbol_1149 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_1146 = INT(OpenAD_Symbol_1146) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1150)
      OpenAD_Symbol_1151 = 1
      DO WHILE(INT(OpenAD_Symbol_1151) .LE. INT(OpenAD_Symbol_1150))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1152)
        IF(OpenAD_Symbol_1152 .ne. 0) THEN
        ENDIF
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1153)
        OpenAD_Symbol_1154 = 1
        DO WHILE(INT(OpenAD_Symbol_1154) .LE. INT(OpenAD_Symbol_1153))
          OpenAD_Symbol_1154 = INT(OpenAD_Symbol_1154) + 1
        END DO
        OpenAD_Symbol_1151 = INT(OpenAD_Symbol_1151) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1155)
      OpenAD_Symbol_1156 = 1
      DO WHILE(INT(OpenAD_Symbol_1156) .LE. INT(OpenAD_Symbol_1155))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1157)
        IF(OpenAD_Symbol_1157 .ne. 0) THEN
        ENDIF
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1158)
        OpenAD_Symbol_1159 = 1
        DO WHILE(INT(OpenAD_Symbol_1159) .LE. INT(OpenAD_Symbol_1158))
          OpenAD_Symbol_1159 = INT(OpenAD_Symbol_1159) + 1
        END DO
        OpenAD_Symbol_1156 = INT(OpenAD_Symbol_1156) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE smoothness_graddepth(CF)
      use w2f__types
      use size
      use parms
      use pfields
      use weights
      use size
      use parms
      use pfields
      use weights
      use size
      use parms
      use pfields
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1178
      INTEGER(w2f__i8) OpenAD_Symbol_1179
      INTEGER(w2f__i8) OpenAD_Symbol_1180
      INTEGER(w2f__i8) OpenAD_Symbol_1181
      INTEGER(w2f__i8) OpenAD_Symbol_1182
      INTEGER(w2f__i8) OpenAD_Symbol_1183
      INTEGER(w2f__i8) OpenAD_Symbol_1184
      INTEGER(w2f__i8) OpenAD_Symbol_1185
      INTEGER(w2f__i8) OpenAD_Symbol_1186
      INTEGER(w2f__i8) OpenAD_Symbol_1187
      INTEGER(w2f__i8) OpenAD_Symbol_1188
      INTEGER(w2f__i8) OpenAD_Symbol_1189
      INTEGER(w2f__i8) OpenAD_Symbol_1190
      INTEGER(w2f__i8) OpenAD_Symbol_1191
      INTEGER(w2f__i8) OpenAD_Symbol_1192
      INTEGER(w2f__i8) OpenAD_Symbol_1193
      INTEGER(w2f__i8) OpenAD_Symbol_1194
      INTEGER(w2f__i8) OpenAD_Symbol_1195
      INTEGER(w2f__i8) OpenAD_Symbol_1196
      INTEGER(w2f__i8) OpenAD_Symbol_1197
      INTEGER(w2f__i8) OpenAD_Symbol_1198
      INTEGER(w2f__i8) OpenAD_Symbol_1199
      INTEGER(w2f__i8) OpenAD_Symbol_1200
      INTEGER(w2f__i8) OpenAD_Symbol_1201
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) CF
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) DFDX(1 : 20, 1 : 20)
      REAL(w2f__8) DFDY(1 : 20, 1 : 20)
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DFDX(1, INT(IY)) = ((__value__(DEPTH(1, IY)) - __value__(
     > DEPTH(20, IY))) / DX(1))
        ELSE
          DFDX(1, INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2, 20, 1
          DFDX(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY)) -
     >  __value__(DEPTH(IX + (-1), IY))) / DX(IX))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1, 20, 1
        IF(YPERIODIC) THEN
          DFDY(INT(IX), 1) = ((__value__(DEPTH(IX, 1)) - __value__(
     > DEPTH(IX, 20))) / DY(1))
        ELSE
          DFDY(INT(IX), 1) = 0.0D00
        ENDIF
        DO IY = 2, 20, 1
          DFDY(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY)) -
     >  __value__(DEPTH(IX, IY + (-1)))) / DY(IY))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          CF = (CF + WEIGHT_GRADDEPTH(IX, IY) * 5.0D-01 *(UMASK(IX, IY)
     >  * DFDX(IX, IY) * DFDX(IX, IY) * 5.0D-01 + VMASK(IX, IY) * DFDY(
     > IX, IY) * DFDY(IX, IY) * 5.0D-01))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DFDX(1, INT(IY)) = ((__value__(DEPTH(1, IY)) - __value__(
     > DEPTH(20, IY))) / DX(1))
        ELSE
          DFDX(1, INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2, 20, 1
          DFDX(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY)) -
     >  __value__(DEPTH(IX + (-1), IY))) / DX(IX))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1, 20, 1
        IF(YPERIODIC) THEN
          DFDY(INT(IX), 1) = ((__value__(DEPTH(IX, 1)) - __value__(
     > DEPTH(IX, 20))) / DY(1))
        ELSE
          DFDY(INT(IX), 1) = 0.0D00
        ENDIF
        DO IY = 2, 20, 1
          DFDY(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY)) -
     >  __value__(DEPTH(IX, IY + (-1)))) / DY(IY))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          CF = (CF + WEIGHT_GRADDEPTH(IX, IY) * 5.0D-01 *(UMASK(IX, IY)
     >  * DFDX(IX, IY) * DFDX(IX, IY) * 5.0D-01 + VMASK(IX, IY) * DFDY(
     > IX, IY) * DFDY(IX, IY) * 5.0D-01))
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      IX = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IX .GE. 1)
        IY = 2 + 1 *((20 - 2) / 1)
        DO WHILE(IY .GE. 2)
          IY = IY - 1
        END DO
        IF(YPERIODIC) THEN
        ENDIF
        IX = IX - 1
      END DO
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 2 + 1 *((20 - 2) / 1)
        DO WHILE(IX .GE. 2)
          IX = IX - 1
        END DO
        IF(XPERIODIC) THEN
        ENDIF
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_GRADDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1192 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DFDX(1, INT(IY)) = ((__value__(DEPTH(1, IY)) - __value__(
     > DEPTH(20, IY))) / DX(1))
          OpenAD_Symbol_1193 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1193)
        ELSE
          DFDX(1, INT(IY)) = 0.0D00
          OpenAD_Symbol_1194 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1194)
        ENDIF
        OpenAD_Symbol_1195 = 0_w2f__i8
        DO IX = 2, 20, 1
          DFDX(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY)) -
     >  __value__(DEPTH(IX + (-1), IY))) / DX(IX))
          OpenAD_Symbol_1195 = (INT(OpenAD_Symbol_1195) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1195)
        OpenAD_Symbol_1192 = (INT(OpenAD_Symbol_1192) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1192)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1196 = 0_w2f__i8
      DO IX = 1, 20, 1
        IF(YPERIODIC) THEN
          DFDY(INT(IX), 1) = ((__value__(DEPTH(IX, 1)) - __value__(
     > DEPTH(IX, 20))) / DY(1))
          OpenAD_Symbol_1197 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1197)
        ELSE
          DFDY(INT(IX), 1) = 0.0D00
          OpenAD_Symbol_1198 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1198)
        ENDIF
        OpenAD_Symbol_1199 = 0_w2f__i8
        DO IY = 2, 20, 1
          DFDY(INT(IX), INT(IY)) = ((__value__(DEPTH(IX, IY)) -
     >  __value__(DEPTH(IX, IY + (-1)))) / DY(IY))
          OpenAD_Symbol_1199 = (INT(OpenAD_Symbol_1199) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1199)
        OpenAD_Symbol_1196 = (INT(OpenAD_Symbol_1196) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1196)
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1200 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1201 = 0_w2f__i8
        DO IX = 1, 20, 1
          CF = (CF + WEIGHT_GRADDEPTH(IX, IY) * 5.0D-01 *(UMASK(IX, IY)
     >  * DFDX(IX, IY) * DFDX(IX, IY) * 5.0D-01 + VMASK(IX, IY) * DFDY(
     > IX, IY) * DFDY(IX, IY) * 5.0D-01))
          OpenAD_Symbol_1201 = (INT(OpenAD_Symbol_1201) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1201)
        OpenAD_Symbol_1200 = (INT(OpenAD_Symbol_1200) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1200)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1178)
      OpenAD_Symbol_1179 = 1
      DO WHILE(INT(OpenAD_Symbol_1179) .LE. INT(OpenAD_Symbol_1178))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1180)
        OpenAD_Symbol_1181 = 1
        DO WHILE(INT(OpenAD_Symbol_1181) .LE. INT(OpenAD_Symbol_1180))
          OpenAD_Symbol_1181 = INT(OpenAD_Symbol_1181) + 1
        END DO
        OpenAD_Symbol_1179 = INT(OpenAD_Symbol_1179) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1182)
      OpenAD_Symbol_1183 = 1
      DO WHILE(INT(OpenAD_Symbol_1183) .LE. INT(OpenAD_Symbol_1182))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1184)
        OpenAD_Symbol_1185 = 1
        DO WHILE(INT(OpenAD_Symbol_1185) .LE. INT(OpenAD_Symbol_1184))
          OpenAD_Symbol_1185 = INT(OpenAD_Symbol_1185) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1186)
        IF(OpenAD_Symbol_1186 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_1183 = INT(OpenAD_Symbol_1183) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1187)
      OpenAD_Symbol_1188 = 1
      DO WHILE(INT(OpenAD_Symbol_1188) .LE. INT(OpenAD_Symbol_1187))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1189)
        OpenAD_Symbol_1190 = 1
        DO WHILE(INT(OpenAD_Symbol_1190) .LE. INT(OpenAD_Symbol_1189))
          OpenAD_Symbol_1190 = INT(OpenAD_Symbol_1190) + 1
        END DO
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1191)
        IF(OpenAD_Symbol_1191 .ne. 0) THEN
        ENDIF
        OpenAD_Symbol_1188 = INT(OpenAD_Symbol_1188) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE cost_function(TIME, CF, LOCAL_U, LOCAL_V, LOCAL_ETA)
      use w2f__types
      use size
      use vars
      use data
      use weights
      use size
      use vars
      use data
      use weights
      use size
      use vars
      use data
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1202
      INTEGER(w2f__i8) OpenAD_Symbol_1203
      INTEGER(w2f__i8) OpenAD_Symbol_1204
      INTEGER(w2f__i8) OpenAD_Symbol_1205
      INTEGER(w2f__i8) OpenAD_Symbol_1206
      INTEGER(w2f__i8) OpenAD_Symbol_1207
      INTEGER(w2f__i8) OpenAD_Symbol_1208
      INTEGER(w2f__i8) OpenAD_Symbol_1209
      INTEGER(w2f__i8) OpenAD_Symbol_1210
      INTEGER(w2f__i8) OpenAD_Symbol_1211
      INTEGER(w2f__i8) OpenAD_Symbol_1212
      INTEGER(w2f__i8) OpenAD_Symbol_1213
      INTEGER(w2f__i8) OpenAD_Symbol_1214
      INTEGER(w2f__i8) OpenAD_Symbol_1215
      INTEGER(w2f__i8) OpenAD_Symbol_1216
      INTEGER(w2f__i8) OpenAD_Symbol_1217
      INTEGER(w2f__i8) OpenAD_Symbol_1218
      INTEGER(w2f__i8) OpenAD_Symbol_1219
      INTEGER(w2f__i8) OpenAD_Symbol_1220
      INTEGER(w2f__i8) OpenAD_Symbol_1221
      INTEGER(w2f__i8) OpenAD_Symbol_1222
      INTEGER(w2f__i8) OpenAD_Symbol_1223
      INTEGER(w2f__i8) OpenAD_Symbol_1224
      INTEGER(w2f__i8) OpenAD_Symbol_1225
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME
      REAL(w2f__8) CF
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      REAL(w2f__8) LOCAL_V(0 : 21, 0 : 21)
      INTENT(IN)  LOCAL_V
      REAL(w2f__8) LOCAL_ETA(0 : 21, 0 : 21)
      INTENT(IN)  LOCAL_ETA
C
C     **** Local Variables and Functions ****
C
      EXTERNAL calc_zonal_transport_split
      EXTERNAL is_eta_data_time
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      EXTERNAL read_data
      LOGICAL(w2f__i4) RESULT
      REAL(w2f__8) ZONAL_TRANSPORT
      REAL(w2f__8) OpenAD_tyc_2(0 : 21, 0 : 21)
      REAL(w2f__8) OpenAD_tyc_9(0 : 21, 0 : 21)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL is_eta_data_time(TIME, RESULT)
      IF(RESULT) THEN
        CALL read_data(TIME)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
      IF((NEDT .eq.(-1)) .OR. RESULT) THEN
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_2, __deriv__(LOCAL_U))
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT, OpenAD_tyc_2)
        CF = (CF + WEIGHT_ZONAL_TRANSPORT *((ZONAL_TRANSPORT -
     >  ZONAL_TRANSPORT_DATA) ** 2) * 5.0D-01)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            CF = (CF + WEIGHT_ETA(IX, IY) *((LOCAL_ETA(IX, IY) -
     >  ETA_DATA(IX, IY)) ** 2) * 5.0D-01)
            CF = (CF + WEIGHT_U(IX, IY) *((__value__(LOCAL_U(IX, IY)) -
     >  U_DATA(IX, IY)) ** 2) * 5.0D-01)
            CF = (CF + WEIGHT_V(IX, IY) *((LOCAL_V(IX, IY) - V_DATA(IX,
     >  IY)) ** 2) * 5.0D-01)
          END DO
        END DO
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL is_eta_data_time(TIME, RESULT)
      IF(RESULT) THEN
        CALL read_data(TIME)
        OpenAD_Symbol_1208 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1208)
      ELSE
        OpenAD_Symbol_1209 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1209)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
      IF((NEDT .eq.(-1)) .OR. RESULT) THEN
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_2, __deriv__(LOCAL_U))
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT, OpenAD_tyc_2)
        CF = (CF + WEIGHT_ZONAL_TRANSPORT *((ZONAL_TRANSPORT -
     >  ZONAL_TRANSPORT_DATA) ** 2) * 5.0D-01)
        OpenAD_Symbol_1210 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_1211 = 0_w2f__i8
          DO IX = 1, 20, 1
            CF = (CF + WEIGHT_ETA(IX, IY) *((LOCAL_ETA(IX, IY) -
     >  ETA_DATA(IX, IY)) ** 2) * 5.0D-01)
            CF = (CF + WEIGHT_U(IX, IY) *((__value__(LOCAL_U(IX, IY)) -
     >  U_DATA(IX, IY)) ** 2) * 5.0D-01)
            CF = (CF + WEIGHT_V(IX, IY) *((LOCAL_V(IX, IY) - V_DATA(IX,
     >  IY)) ** 2) * 5.0D-01)
            OpenAD_Symbol_1211 = (INT(OpenAD_Symbol_1211) + INT(
     > 1_w2f__i8))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1211)
          OpenAD_Symbol_1210 = (INT(OpenAD_Symbol_1210) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1210)
        OpenAD_Symbol_1213 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1213)
      ELSE
        OpenAD_Symbol_1212 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1212)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1202)
      IF(OpenAD_Symbol_1202 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1203)
        OpenAD_Symbol_1204 = 1
        DO WHILE(INT(OpenAD_Symbol_1204) .LE. INT(OpenAD_Symbol_1203))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1205)
          OpenAD_Symbol_1206 = 1
          DO WHILE(INT(OpenAD_Symbol_1206) .LE. INT(OpenAD_Symbol_1205)
     > )
            OpenAD_Symbol_1206 = INT(OpenAD_Symbol_1206) + 1
          END DO
          OpenAD_Symbol_1204 = INT(OpenAD_Symbol_1204) + 1
        END DO
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT, OpenAD_tyc_9)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1207)
      IF(OpenAD_Symbol_1207 .ne. 0) THEN
        CALL read_data(TIME)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(LOCAL_V)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(LOCAL_ETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(LOCAL_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(LOCAL_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(LOCAL_V)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(LOCAL_ETA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(LOCAL_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(LOCAL_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(CF)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL is_eta_data_time(TIME, RESULT)
      IF(RESULT) THEN
        CALL read_data(TIME)
        OpenAD_Symbol_1220 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1220)
      ELSE
        OpenAD_Symbol_1221 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1221)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
      IF((NEDT .eq.(-1)) .OR. RESULT) THEN
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_2, __deriv__(LOCAL_U))
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT, OpenAD_tyc_2)
        CF = (CF + WEIGHT_ZONAL_TRANSPORT *((ZONAL_TRANSPORT -
     >  ZONAL_TRANSPORT_DATA) ** 2) * 5.0D-01)
        OpenAD_Symbol_1222 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_1223 = 0_w2f__i8
          DO IX = 1, 20, 1
            CF = (CF + WEIGHT_ETA(IX, IY) *((LOCAL_ETA(IX, IY) -
     >  ETA_DATA(IX, IY)) ** 2) * 5.0D-01)
            CF = (CF + WEIGHT_U(IX, IY) *((__value__(LOCAL_U(IX, IY)) -
     >  U_DATA(IX, IY)) ** 2) * 5.0D-01)
            CF = (CF + WEIGHT_V(IX, IY) *((LOCAL_V(IX, IY) - V_DATA(IX,
     >  IY)) ** 2) * 5.0D-01)
            OpenAD_Symbol_1223 = (INT(OpenAD_Symbol_1223) + INT(
     > 1_w2f__i8))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1223)
          OpenAD_Symbol_1222 = (INT(OpenAD_Symbol_1222) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1222)
        OpenAD_Symbol_1225 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1225)
      ELSE
        OpenAD_Symbol_1224 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1224)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1214)
      IF(OpenAD_Symbol_1214 .ne. 0) THEN
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1215)
        OpenAD_Symbol_1216 = 1
        DO WHILE(INT(OpenAD_Symbol_1216) .LE. INT(OpenAD_Symbol_1215))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1217)
          OpenAD_Symbol_1218 = 1
          DO WHILE(INT(OpenAD_Symbol_1218) .LE. INT(OpenAD_Symbol_1217)
     > )
            OpenAD_Symbol_1218 = INT(OpenAD_Symbol_1218) + 1
          END DO
          OpenAD_Symbol_1216 = INT(OpenAD_Symbol_1216) + 1
        END DO
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT, OpenAD_tyc_9)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1219)
      IF(OpenAD_Symbol_1219 .ne. 0) THEN
        CALL read_data(TIME)
      ENDIF
      CALL is_eta_data_time(TIME, RESULT)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE cost_depth(CF)
      use w2f__types
      use size
      use pfields
      use data
      use weights
      use size
      use pfields
      use data
      use weights
      use size
      use pfields
      use data
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1226
      INTEGER(w2f__i8) OpenAD_Symbol_1227
      INTEGER(w2f__i8) OpenAD_Symbol_1228
      INTEGER(w2f__i8) OpenAD_Symbol_1229
      INTEGER(w2f__i8) OpenAD_Symbol_1230
      INTEGER(w2f__i8) OpenAD_Symbol_1231
      INTEGER(w2f__i8) OpenAD_Symbol_1232
      INTEGER(w2f__i8) OpenAD_Symbol_1233
      INTEGER(w2f__i8) OpenAD_Symbol_1234
      INTEGER(w2f__i8) OpenAD_Symbol_1235
      INTEGER(w2f__i8) OpenAD_Symbol_1236
      INTEGER(w2f__i8) OpenAD_Symbol_1237
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) CF
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      INTEGER(w2f__i4) IY
      INTEGER(w2f__i4) JX
      INTEGER(w2f__i4) JY
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO JY = 1, 20, 1
          DO IX = 1, 20, 1
            DO JX = 1, 20, 1
              CF = (CF +(__value__(DEPTH(IX, IY)) - DEPTH_DATA(IX, IY))
     >  * 5.0D-01 *(__value__(DEPTH(JX, JY)) - DEPTH_DATA(JX, JY)))
            END DO
          END DO
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO JY = 1, 20, 1
          DO IX = 1, 20, 1
            DO JX = 1, 20, 1
              CF = (CF +(__value__(DEPTH(IX, IY)) - DEPTH_DATA(IX, IY))
     >  * 5.0D-01 *(__value__(DEPTH(JX, JY)) - DEPTH_DATA(JX, JY)))
            END DO
          END DO
        END DO
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        JY = 1 + 1 *((20 - 1) / 1)
        DO WHILE(JY .GE. 1)
          IX = 1 + 1 *((20 - 1) / 1)
          DO WHILE(IX .GE. 1)
            JX = 1 + 1 *((20 - 1) / 1)
            DO WHILE(JX .GE. 1)
              JX = JX - 1
            END DO
            IX = IX - 1
          END DO
          JY = JY - 1
        END DO
        IY = IY - 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(CF)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(CF)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(CF)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      OpenAD_Symbol_1234 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_1235 = 0_w2f__i8
        DO JY = 1, 20, 1
          OpenAD_Symbol_1236 = 0_w2f__i8
          DO IX = 1, 20, 1
            OpenAD_Symbol_1237 = 0_w2f__i8
            DO JX = 1, 20, 1
              CF = (CF +(__value__(DEPTH(IX, IY)) - DEPTH_DATA(IX, IY))
     >  * 5.0D-01 *(__value__(DEPTH(JX, JY)) - DEPTH_DATA(JX, JY)))
              OpenAD_Symbol_1237 = (INT(OpenAD_Symbol_1237) + INT(
     > 1_w2f__i8))
            END DO
C           $OpenAD$ INLINE push_i_s0(subst)
            CALL push_i_s0(OpenAD_Symbol_1237)
            OpenAD_Symbol_1236 = (INT(OpenAD_Symbol_1236) + INT(
     > 1_w2f__i8))
          END DO
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1236)
          OpenAD_Symbol_1235 = (INT(OpenAD_Symbol_1235) + INT(1_w2f__i8
     > ))
        END DO
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1235)
        OpenAD_Symbol_1234 = (INT(OpenAD_Symbol_1234) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1234)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1226)
      OpenAD_Symbol_1227 = 1
      DO WHILE(INT(OpenAD_Symbol_1227) .LE. INT(OpenAD_Symbol_1226))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1228)
        OpenAD_Symbol_1229 = 1
        DO WHILE(INT(OpenAD_Symbol_1229) .LE. INT(OpenAD_Symbol_1228))
C         $OpenAD$ INLINE pop_i_s0(subst)
          CALL pop_i_s0(OpenAD_Symbol_1230)
          OpenAD_Symbol_1231 = 1
          DO WHILE(INT(OpenAD_Symbol_1231) .LE. INT(OpenAD_Symbol_1230)
     > )
C           $OpenAD$ INLINE pop_i_s0(subst)
            CALL pop_i_s0(OpenAD_Symbol_1232)
            OpenAD_Symbol_1233 = 1
            DO WHILE(INT(OpenAD_Symbol_1233) .LE. INT(
     > OpenAD_Symbol_1232))
              OpenAD_Symbol_1233 = INT(OpenAD_Symbol_1233) + 1
            END DO
            OpenAD_Symbol_1231 = INT(OpenAD_Symbol_1231) + 1
          END DO
          OpenAD_Symbol_1229 = INT(OpenAD_Symbol_1229) + 1
        END DO
        OpenAD_Symbol_1227 = INT(OpenAD_Symbol_1227) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE loop_body_wrapper_inner(LOCAL_U, LOCAL_V, LOCAL_ETA,
     >  COST, CALC_COST, TIME, TIME_INDEX, NIO)
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1238
      INTEGER(w2f__i8) OpenAD_Symbol_1239
      INTEGER(w2f__i8) OpenAD_Symbol_1240
      INTEGER(w2f__i8) OpenAD_Symbol_1241
      INTEGER(w2f__i8) OpenAD_Symbol_1242
      INTEGER(w2f__i8) OpenAD_Symbol_1243
      INTEGER(w2f__i8) OpenAD_Symbol_1244
      INTEGER(w2f__i8) OpenAD_Symbol_1245
      INTEGER(w2f__i8) OpenAD_Symbol_1246
      INTEGER(w2f__i8) OpenAD_Symbol_1247
      INTEGER(w2f__i8) OpenAD_Symbol_1248
      INTEGER(w2f__i8) OpenAD_Symbol_1249
C
C     **** Parameters and Result ****
C
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_V(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_ETA(0 : 21, 0 : 21)
      REAL(w2f__8) COST
      LOGICAL(w2f__i4) CALC_COST
      REAL(w2f__8) TIME
      INTEGER(w2f__i4) TIME_INDEX
      INTEGER(w2f__i4) NIO
C
C     **** Local Variables and Functions ****
C
      EXTERNAL cost_function
      EXTERNAL time_step
      REAL(w2f__8) OpenAD_tyc_11(0 : 21, 0 : 21)
      REAL(w2f__8) OpenAD_tyc_12(0 : 21, 0 : 21)
      REAL(w2f__8) OpenAD_tyc_3(0 : 21, 0 : 21)
      REAL(w2f__8) OpenAD_tyc_4(0 : 21, 0 : 21)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint_split_iif.f
      CALL time_step(TIME_INDEX, __deriv__(LOCAL_U), __deriv__(LOCAL_V)
     > , __deriv__(LOCAL_ETA))
      IF((TIME_INDEX .GT. NTSPINUP) .AND. CALC_COST) THEN
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_3, __deriv__(LOCAL_V))
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_4, __deriv__(LOCAL_ETA))
        CALL cost_function(TIME, COST, __deriv__(LOCAL_U), OpenAD_tyc_3
     > , OpenAD_tyc_4)
      ENDIF
      IF((MOD(DT * REAL(TIME_INDEX), DT_DUMP) .eq. 0.0D00) .AND.(FULLIO
     >  .AND.(.NOT. SUPPRESSIO))) THEN
        NIO = (NIO + 1)
        WRITE(*, *) 'Writing Time Step ', TIME_INDEX
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint_split_iif.f
      CALL time_step(TIME_INDEX, __deriv__(LOCAL_U), __deriv__(LOCAL_V)
     > , __deriv__(LOCAL_ETA))
      IF((TIME_INDEX .GT. NTSPINUP) .AND. CALC_COST) THEN
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_3, __deriv__(LOCAL_V))
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_4, __deriv__(LOCAL_ETA))
        CALL cost_function(TIME, COST, __deriv__(LOCAL_U), OpenAD_tyc_3
     > , OpenAD_tyc_4)
        OpenAD_Symbol_1240 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1240)
      ELSE
        OpenAD_Symbol_1241 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1241)
      ENDIF
      IF((MOD(DT * REAL(TIME_INDEX), DT_DUMP) .eq. 0.0D00) .AND.(FULLIO
     >  .AND.(.NOT. SUPPRESSIO))) THEN
        NIO = (NIO + 1)
        WRITE(*, *) 'Writing Time Step ', TIME_INDEX
        OpenAD_Symbol_1242 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1242)
      ELSE
        OpenAD_Symbol_1243 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1243)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1238)
      IF(OpenAD_Symbol_1238 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1239)
      IF(OpenAD_Symbol_1239 .ne. 0) THEN
        CALL cost_function(TIME, COST, __deriv__(LOCAL_U),
     >  OpenAD_tyc_11, OpenAD_tyc_12)
      ENDIF
      CALL time_step(TIME_INDEX, __deriv__(LOCAL_U), __deriv__(LOCAL_V)
     > , __deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NIO)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(COST)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint_split_iif.f
      CALL time_step(TIME_INDEX, __deriv__(LOCAL_U), __deriv__(LOCAL_V)
     > , __deriv__(LOCAL_ETA))
      IF((TIME_INDEX .GT. NTSPINUP) .AND. CALC_COST) THEN
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_3, __deriv__(LOCAL_V))
C       $OpenAD$ INLINE oad_convert(subst,subst)
        CALL oad_convert(OpenAD_tyc_4, __deriv__(LOCAL_ETA))
        CALL cost_function(TIME, COST, __deriv__(LOCAL_U), OpenAD_tyc_3
     > , OpenAD_tyc_4)
        OpenAD_Symbol_1246 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1246)
      ELSE
        OpenAD_Symbol_1247 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1247)
      ENDIF
      IF((MOD(DT * REAL(TIME_INDEX), DT_DUMP) .eq. 0.0D00) .AND.(FULLIO
     >  .AND.(.NOT. SUPPRESSIO))) THEN
        NIO = (NIO + 1)
        WRITE(*, *) 'Writing Time Step ', TIME_INDEX
        OpenAD_Symbol_1248 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1248)
      ELSE
        OpenAD_Symbol_1249 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1249)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1244)
      IF(OpenAD_Symbol_1244 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1245)
      IF(OpenAD_Symbol_1245 .ne. 0) THEN
        CALL cost_function(TIME, COST, __deriv__(LOCAL_U),
     >  OpenAD_tyc_11, OpenAD_tyc_12)
      ENDIF
      CALL time_step(TIME_INDEX, __deriv__(LOCAL_U), __deriv__(LOCAL_V)
     > , __deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE loop_body_wrapper_outer(LOCAL_U, LOCAL_V, LOCAL_ETA,
     >  COST, CALC_COST, TIME, TIME_INDEX, NIO, IT, NTOTAL, NINNER)
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1250
      INTEGER(w2f__i8) OpenAD_Symbol_1251
      INTEGER(w2f__i8) OpenAD_Symbol_1252
      INTEGER(w2f__i8) OpenAD_Symbol_1253
      INTEGER(w2f__i8) OpenAD_Symbol_1254
      INTEGER(w2f__i8) OpenAD_Symbol_1255
      INTEGER(w2f__i8) OpenAD_Symbol_1256
      INTEGER(w2f__i8) OpenAD_Symbol_1257
      INTEGER(w2f__i8) OpenAD_Symbol_1258
      INTEGER(w2f__i8) OpenAD_Symbol_1259
      INTEGER(w2f__i8) OpenAD_Symbol_1260
      INTEGER(w2f__i8) OpenAD_Symbol_1261
C
C     **** Parameters and Result ****
C
      TYPE (oadactive) LOCAL_U(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_V(0 : 21, 0 : 21)
      TYPE (oadactive) LOCAL_ETA(0 : 21, 0 : 21)
      REAL(w2f__8) COST
      LOGICAL(w2f__i4) CALC_COST
      REAL(w2f__8) TIME
      INTEGER(w2f__i4) TIME_INDEX
      INTEGER(w2f__i4) NIO
      INTEGER(w2f__i4) IT
      INTEGER(w2f__i4) NTOTAL
      INTEGER(w2f__i4) NINNER
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) JT
      EXTERNAL loop_body_wrapper_inner
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template.joint_split_oif.f
      DO JT = 1, NINNER, 1
        TIME_INDEX = (JT + NINNER *(IT +(-1)))
        TIME = (START_TIME + TIME_INDEX * DT)
        IF(TIME_INDEX .LE. NTOTAL) THEN
          CALL loop_body_wrapper_inner(__deriv__(LOCAL_U), __deriv__(
     > LOCAL_V), __deriv__(LOCAL_ETA), COST, CALC_COST, TIME,
     >  TIME_INDEX, NIO)
        ENDIF
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template.joint_split_oif.f
      OpenAD_Symbol_1253 = 0_w2f__i8
      DO JT = 1, NINNER, 1
        TIME_INDEX = (JT + NINNER *(IT +(-1)))
        TIME = (START_TIME + TIME_INDEX * DT)
        IF(TIME_INDEX .LE. NTOTAL) THEN
          CALL loop_body_wrapper_inner(__deriv__(LOCAL_U), __deriv__(
     > LOCAL_V), __deriv__(LOCAL_ETA), COST, CALC_COST, TIME,
     >  TIME_INDEX, NIO)
          OpenAD_Symbol_1254 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1254)
        ELSE
          OpenAD_Symbol_1255 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1255)
        ENDIF
        OpenAD_Symbol_1253 = (INT(OpenAD_Symbol_1253) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1253)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1250)
      OpenAD_Symbol_1251 = 1
      DO WHILE(INT(OpenAD_Symbol_1251) .LE. INT(OpenAD_Symbol_1250))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1252)
        IF(OpenAD_Symbol_1252 .ne. 0) THEN
          CALL loop_body_wrapper_inner(__deriv__(LOCAL_U), __deriv__(
     > LOCAL_V), __deriv__(LOCAL_ETA), COST, CALC_COST, TIME,
     >  TIME_INDEX, NIO)
        ENDIF
        OpenAD_Symbol_1251 = INT(OpenAD_Symbol_1251) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTOTAL)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NINNER)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NINNER)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTOTAL)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTOTAL)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NINNER)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NINNER)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTOTAL)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(IT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_COST)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(COST)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NIO)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(TIME_INDEX)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(COST)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template.joint_split_oif.f
      OpenAD_Symbol_1259 = 0_w2f__i8
      DO JT = 1, NINNER, 1
        TIME_INDEX = (JT + NINNER *(IT +(-1)))
        TIME = (START_TIME + TIME_INDEX * DT)
        IF(TIME_INDEX .LE. NTOTAL) THEN
          CALL loop_body_wrapper_inner(__deriv__(LOCAL_U), __deriv__(
     > LOCAL_V), __deriv__(LOCAL_ETA), COST, CALC_COST, TIME,
     >  TIME_INDEX, NIO)
          OpenAD_Symbol_1260 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1260)
        ELSE
          OpenAD_Symbol_1261 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1261)
        ENDIF
        OpenAD_Symbol_1259 = (INT(OpenAD_Symbol_1259) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1259)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1256)
      OpenAD_Symbol_1257 = 1
      DO WHILE(INT(OpenAD_Symbol_1257) .LE. INT(OpenAD_Symbol_1256))
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_1258)
        IF(OpenAD_Symbol_1258 .ne. 0) THEN
          CALL loop_body_wrapper_inner(__deriv__(LOCAL_U), __deriv__(
     > LOCAL_V), __deriv__(LOCAL_ETA), COST, CALC_COST, TIME,
     >  TIME_INDEX, NIO)
        ENDIF
        OpenAD_Symbol_1257 = INT(OpenAD_Symbol_1257) + 1
      END DO
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(LOCAL_U))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE

      SUBROUTINE forward_model(NCTRL, XC, COST_FINAL)
      use w2f__types
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      INTEGER(w2f__i8) OpenAD_Symbol_1262
      INTEGER(w2f__i8) OpenAD_Symbol_1263
      INTEGER(w2f__i8) OpenAD_Symbol_1264
      INTEGER(w2f__i8) OpenAD_Symbol_1265
      INTEGER(w2f__i8) OpenAD_Symbol_1266
      INTEGER(w2f__i8) OpenAD_Symbol_1267
      INTEGER(w2f__i8) OpenAD_Symbol_1268
      INTEGER(w2f__i8) OpenAD_Symbol_1269
      INTEGER(w2f__i8) OpenAD_Symbol_1270
      INTEGER(w2f__i8) OpenAD_Symbol_1271
      INTEGER(w2f__i8) OpenAD_Symbol_1272
      INTEGER(w2f__i8) OpenAD_Symbol_1273
      INTEGER(w2f__i8) OpenAD_Symbol_1274
      INTEGER(w2f__i8) OpenAD_Symbol_1275
      INTEGER(w2f__i8) OpenAD_Symbol_1276
      INTEGER(w2f__i8) OpenAD_Symbol_1277
      INTEGER(w2f__i8) OpenAD_Symbol_1278
      INTEGER(w2f__i8) OpenAD_Symbol_1279
      INTEGER(w2f__i8) OpenAD_Symbol_1280
      INTEGER(w2f__i8) OpenAD_Symbol_1281
      INTEGER(w2f__i8) OpenAD_Symbol_1282
      INTEGER(w2f__i8) OpenAD_Symbol_1283
      INTEGER(w2f__i8) OpenAD_Symbol_1284
      INTEGER(w2f__i8) OpenAD_Symbol_1285
      INTEGER(w2f__i8) OpenAD_Symbol_1286
      INTEGER(w2f__i8) OpenAD_Symbol_1287
      INTEGER(w2f__i8) OpenAD_Symbol_1288
      INTEGER(w2f__i8) OpenAD_Symbol_1289
      INTEGER(w2f__i8) OpenAD_Symbol_1290
      INTEGER(w2f__i8) OpenAD_Symbol_1291
      INTEGER(w2f__i8) OpenAD_Symbol_1292
      INTEGER(w2f__i8) OpenAD_Symbol_1293
      INTEGER(w2f__i8) OpenAD_Symbol_1294
      INTEGER(w2f__i8) OpenAD_Symbol_1295
      INTEGER(w2f__i8) OpenAD_Symbol_1296
      INTEGER(w2f__i8) OpenAD_Symbol_1297
      INTEGER(w2f__i8) OpenAD_Symbol_1298
      INTEGER(w2f__i8) OpenAD_Symbol_1299
      INTEGER(w2f__i8) OpenAD_Symbol_1300
      INTEGER(w2f__i8) OpenAD_Symbol_1301
      INTEGER(w2f__i8) OpenAD_Symbol_1302
      INTEGER(w2f__i8) OpenAD_Symbol_1303
      INTEGER(w2f__i8) OpenAD_Symbol_1304
      INTEGER(w2f__i8) OpenAD_Symbol_1305
      INTEGER(w2f__i8) OpenAD_Symbol_1306
      INTEGER(w2f__i8) OpenAD_Symbol_1307
      INTEGER(w2f__i8) OpenAD_Symbol_1308
      INTEGER(w2f__i8) OpenAD_Symbol_1309
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) NCTRL
      TYPE (oadactive) XC(1 : NCTRL)
      TYPE (oadactive) COST_FINAL
C
C     **** Local Variables and Functions ****
C
      LOGICAL(w2f__i4) CALC_COST
      EXTERNAL calc_depth_uv
      EXTERNAL calc_zonal_transport_joint
      REAL(w2f__8) COST
      REAL(w2f__8) COST_D
      EXTERNAL cost_depth
      REAL(w2f__8) COST_GD
      REAL(w2f__8) COST_SD
      EXTERNAL initial_values
      INTEGER(w2f__i4) IT
      EXTERNAL loop_body_wrapper_outer
      EXTERNAL map_from_control_vector
      INTEGER(w2f__i4) NINNER
      INTEGER(w2f__i4) NIO
      INTEGER(w2f__i4) NOUTER
      INTEGER(w2f__i4) NTOTAL
      REAL(w2f__8) ONE
      PARAMETER ( ONE = 1.0D00)
      REAL(w2f__8) ROUTIN
      REAL(w2f__8) TIME
      INTEGER(w2f__i4) TIME_INDEX
      TYPE (oadactive) ZONAL_TRANSPORT
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(XC)
C$OPENAD DEPENDENT(COST_FINAL)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template OADrts/ad_template_timing.joint.f
      IF(CALC_HESS .OR.(OPTIMIZE .OR.(GRAD_CHECK .OR. INITIAL_GRAD)))
     >  THEN
        CALC_COST = .TRUE.
      ELSE
        CALC_COST = .FALSE.
      ENDIF
      NINNER = 500
      COST_D = 0.0D00
      COST_SD = 0.0D00
      COST_GD = 0.0D00
      COST = 0.0D00
      CALL map_from_control_vector(NCTRL, __deriv__(XC))
      CALL calc_depth_uv()
      CALL initial_values()
      IF(.NOT. SUPPRESSIO) THEN
      ENDIF
      IF(CALC_COST) THEN
        CALL cost_depth(COST_D)
      ENDIF
      NIO = 0
      TIME_INDEX = 0
      TIME = 0.0D00
      NTOTAL = (NT + NTSPINUP)
      ROUTIN = (REAL(NTOTAL) / REAL(NINNER))
      IF((NTOTAL / NINNER) .ne. ROUTIN) THEN
        NOUTER = (INT(ROUTIN) + 1)
      ELSE
        NOUTER = INT(ROUTIN)
      ENDIF
      IF(FULLIO .AND.(.NOT. SUPPRESSIO)) THEN
        WRITE(*, *) 'number of outer loops',
     >  ' = number of tape records = ', NOUTER
      ENDIF
      TIME_INDEX = 0
      TIME = (START_TIME + TIME_INDEX * DT)
      IF(.NOT. SUPPRESSIO) THEN
        NIO = (NIO + 1)
        WRITE(*, *) 'Writing Time Step ', TIME_INDEX
      ENDIF
      DO IT = 1, NOUTER, 1
        WRITE(*, *) 'outer loop ', IT
        CALL loop_body_wrapper_outer(__deriv__(U), __deriv__(V),
     >  __deriv__(ETA), COST, CALC_COST, TIME, TIME_INDEX, NIO, IT,
     >  NTOTAL, NINNER)
      END DO
      __value__(ZONAL_TRANSPORT) = 0.0D00
      CALL calc_zonal_transport_joint(__deriv__(ZONAL_TRANSPORT),
     >  __deriv__(U))
      WRITE(*, *) 'zonal volume transport = ', (__value__(
     > ZONAL_TRANSPORT) * 9.99999999999999954748D-07), ' Sv'
      __value__(COST_FINAL) = __value__(ZONAL_TRANSPORT)
      IF(ITERATION .GE. 0) THEN
C       open(10,file='cost.txt',form='formatted',position='append')
        OPEN(UNIT = 10, POSITION = 'APPEND', FORM = 'FORMATTED', FILE =
     >  'cost.txt')
        WRITE(10, '(I5,5E15.8)') ITERATION, COST_D, COST_SD, COST_GD,
     >  COST, __value__(COST_FINAL)
C       close(10)
        CLOSE(UNIT = 10)
        ITERATION = (ITERATION + 1)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template OADrts/ad_template_timing.joint.f
      IF(CALC_HESS .OR.(OPTIMIZE .OR.(GRAD_CHECK .OR. INITIAL_GRAD)))
     >  THEN
        CALC_COST = .TRUE.
        OpenAD_Symbol_1271 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1271)
      ELSE
        CALC_COST = .FALSE.
        OpenAD_Symbol_1272 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1272)
      ENDIF
      NINNER = 500
      COST_D = 0.0D00
      COST_SD = 0.0D00
      COST_GD = 0.0D00
      COST = 0.0D00
      CALL map_from_control_vector(NCTRL, __deriv__(XC))
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(NCTRL)
      CALL calc_depth_uv()
      CALL initial_values()
      IF(.NOT. SUPPRESSIO) THEN
        OpenAD_Symbol_1273 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1273)
      ELSE
        OpenAD_Symbol_1274 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1274)
      ENDIF
      IF(CALC_COST) THEN
        CALL cost_depth(COST_D)
        OpenAD_Symbol_1275 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1275)
      ELSE
        OpenAD_Symbol_1276 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1276)
      ENDIF
      NIO = 0
      TIME_INDEX = 0
      TIME = 0.0D00
      NTOTAL = (NT + NTSPINUP)
      ROUTIN = (REAL(NTOTAL) / REAL(NINNER))
      IF((NTOTAL / NINNER) .ne. ROUTIN) THEN
        NOUTER = (INT(ROUTIN) + 1)
        OpenAD_Symbol_1277 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1277)
      ELSE
        NOUTER = INT(ROUTIN)
        OpenAD_Symbol_1278 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1278)
      ENDIF
      IF(FULLIO .AND.(.NOT. SUPPRESSIO)) THEN
        WRITE(*, *) 'number of outer loops',
     >  ' = number of tape records = ', NOUTER
        OpenAD_Symbol_1279 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1279)
      ELSE
        OpenAD_Symbol_1280 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1280)
      ENDIF
      TIME_INDEX = 0
      TIME = (START_TIME + TIME_INDEX * DT)
      IF(.NOT. SUPPRESSIO) THEN
        NIO = (NIO + 1)
        WRITE(*, *) 'Writing Time Step ', TIME_INDEX
        OpenAD_Symbol_1281 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1281)
      ELSE
        OpenAD_Symbol_1282 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1282)
      ENDIF
      OpenAD_Symbol_1283 = 0_w2f__i8
      DO IT = 1, NOUTER, 1
        WRITE(*, *) 'outer loop ', IT
        CALL loop_body_wrapper_outer(__deriv__(U), __deriv__(V),
     >  __deriv__(ETA), COST, CALC_COST, TIME, TIME_INDEX, NIO, IT,
     >  NTOTAL, NINNER)
        OpenAD_Symbol_1283 = (INT(OpenAD_Symbol_1283) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1283)
      __value__(ZONAL_TRANSPORT) = 0.0D00
      CALL calc_zonal_transport_joint(__deriv__(ZONAL_TRANSPORT),
     >  __deriv__(U))
      WRITE(*, *) 'zonal volume transport = ', (__value__(
     > ZONAL_TRANSPORT) * 9.99999999999999954748D-07), ' Sv'
      __value__(COST_FINAL) = __value__(ZONAL_TRANSPORT)
      IF(ITERATION .GE. 0) THEN
C       open(10,file='cost.txt',form='formatted',position='append')
        OPEN(UNIT = 10, POSITION = 'APPEND', FORM = 'FORMATTED', FILE =
     >  'cost.txt')
        WRITE(10, '(I5,5E15.8)') ITERATION, COST_D, COST_SD, COST_GD,
     >  COST, __value__(COST_FINAL)
C       close(10)
        CLOSE(UNIT = 10)
        ITERATION = (ITERATION + 1)
        OpenAD_Symbol_1284 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1284)
      ELSE
        OpenAD_Symbol_1285 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1285)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1262)
      IF(OpenAD_Symbol_1262 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE IncDeriv(subst,subst)
      CALL IncDeriv(__deriv__(COST_FINAL), __deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(COST_FINAL))
      CALL calc_zonal_transport_joint(__deriv__(ZONAL_TRANSPORT),
     >  __deriv__(U))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1263)
      OpenAD_Symbol_1264 = 1
      DO WHILE(INT(OpenAD_Symbol_1264) .LE. INT(OpenAD_Symbol_1263))
        CALL loop_body_wrapper_outer(__deriv__(U), __deriv__(V),
     >  __deriv__(ETA), COST, CALC_COST, TIME, TIME_INDEX, NIO, IT,
     >  NTOTAL, NINNER)
        OpenAD_Symbol_1264 = INT(OpenAD_Symbol_1264) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1265)
      IF(OpenAD_Symbol_1265 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1266)
      IF(OpenAD_Symbol_1266 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1267)
      IF(OpenAD_Symbol_1267 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1268)
      IF(OpenAD_Symbol_1268 .ne. 0) THEN
        CALL cost_depth(COST_D)
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1269)
      IF(OpenAD_Symbol_1269 .ne. 0) THEN
      ENDIF
      CALL initial_values()
      CALL calc_depth_uv()
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(NCTRL)
      CALL map_from_control_vector(NCTRL, __deriv__(XC))
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1270)
      IF(OpenAD_Symbol_1270 .ne. 0) THEN
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 4
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_HESS)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(GRAD_CHECK)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(INITIAL_GRAD)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(OPTIMIZE)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_vector(subst)
      CALL cp_arg_store_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(WEIGHT_V)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 5
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 6
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_V)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_U)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(WEIGHT_ETA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(V_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(U_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(ETA_DATA_TIME)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETA_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(DEPTH_DATA)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UFORCE)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(VINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(UINI)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(SCALEDEPTH)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(RX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(HY)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FRICT)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIV)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(FCORIU)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAINI)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DY)
C     $OpenAD$ INLINE cp_arg_restore_real_vector(subst)
      CALL cp_arg_restore_real_vector(DX)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(WEIGHT_ZONAL_TRANSPORT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(ZONAL_TRANSPORT_DATA)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NEDT)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(RY)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(YPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(XPERIODIC)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(OPTIMIZE)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(INITIAL_GRAD)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(GRAD_CHECK)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT_DUMP)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_HESS)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 7
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 8
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix(subst)
      CALL cp_arg_store_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a(subst)
      CALL cp_arg_store_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(CALC_HESS)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(GRAD_CHECK)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(INITIAL_GRAD)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NT)
C     $OpenAD$ INLINE cp_arg_store_integer_scalar(subst)
      CALL cp_arg_store_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(OPTIMIZE)
C     $OpenAD$ INLINE cp_arg_store_real_scalar(subst)
      CALL cp_arg_store_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_store_bool_scalar(subst)
      CALL cp_arg_store_bool_scalar(SUPPRESSIO)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 9
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(SUPPRESSIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(START_TIME)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(OPTIMIZE)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NTSPINUP)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(NT)
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(ITERATION)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(INITIAL_GRAD)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(GRAD_CHECK)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(FULLIO)
C     $OpenAD$ INLINE cp_arg_restore_real_scalar(subst)
      CALL cp_arg_restore_real_scalar(DT)
C     $OpenAD$ INLINE cp_arg_restore_bool_scalar(subst)
      CALL cp_arg_restore_bool_scalar(CALC_HESS)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix(subst)
      CALL cp_arg_restore_real_matrix(ETAMASK)
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a(subst)
      CALL cp_arg_restore_real_matrix_a(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_restore_integer_scalar(subst)
      CALL cp_arg_restore_integer_scalar(ITERATION)
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 10
C$OPENAD XXX Template OADrts/ad_template_timing.joint.f
      IF(CALC_HESS .OR.(OPTIMIZE .OR.(GRAD_CHECK .OR. INITIAL_GRAD)))
     >  THEN
        CALC_COST = .TRUE.
        OpenAD_Symbol_1295 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1295)
      ELSE
        CALC_COST = .FALSE.
        OpenAD_Symbol_1296 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1296)
      ENDIF
      NINNER = 500
      COST_D = 0.0D00
      COST_SD = 0.0D00
      COST_GD = 0.0D00
      COST = 0.0D00
      CALL map_from_control_vector(NCTRL, __deriv__(XC))
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(NCTRL)
      CALL calc_depth_uv()
      CALL initial_values()
      IF(.NOT. SUPPRESSIO) THEN
        OpenAD_Symbol_1297 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1297)
      ELSE
        OpenAD_Symbol_1298 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1298)
      ENDIF
      IF(CALC_COST) THEN
        CALL cost_depth(COST_D)
        OpenAD_Symbol_1299 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1299)
      ELSE
        OpenAD_Symbol_1300 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1300)
      ENDIF
      NIO = 0
      TIME_INDEX = 0
      TIME = 0.0D00
      NTOTAL = (NT + NTSPINUP)
      ROUTIN = (REAL(NTOTAL) / REAL(NINNER))
      IF((NTOTAL / NINNER) .ne. ROUTIN) THEN
        NOUTER = (INT(ROUTIN) + 1)
        OpenAD_Symbol_1301 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1301)
      ELSE
        NOUTER = INT(ROUTIN)
        OpenAD_Symbol_1302 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1302)
      ENDIF
      IF(FULLIO .AND.(.NOT. SUPPRESSIO)) THEN
        WRITE(*, *) 'number of outer loops',
     >  ' = number of tape records = ', NOUTER
        OpenAD_Symbol_1303 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1303)
      ELSE
        OpenAD_Symbol_1304 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1304)
      ENDIF
      TIME_INDEX = 0
      TIME = (START_TIME + TIME_INDEX * DT)
      IF(.NOT. SUPPRESSIO) THEN
        NIO = (NIO + 1)
        WRITE(*, *) 'Writing Time Step ', TIME_INDEX
        OpenAD_Symbol_1305 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1305)
      ELSE
        OpenAD_Symbol_1306 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1306)
      ENDIF
      OpenAD_Symbol_1307 = 0_w2f__i8
      DO IT = 1, NOUTER, 1
        WRITE(*, *) 'outer loop ', IT
        CALL loop_body_wrapper_outer(__deriv__(U), __deriv__(V),
     >  __deriv__(ETA), COST, CALC_COST, TIME, TIME_INDEX, NIO, IT,
     >  NTOTAL, NINNER)
        OpenAD_Symbol_1307 = (INT(OpenAD_Symbol_1307) + INT(1_w2f__i8))
      END DO
C     $OpenAD$ INLINE push_i_s0(subst)
      CALL push_i_s0(OpenAD_Symbol_1307)
      __value__(ZONAL_TRANSPORT) = 0.0D00
      CALL calc_zonal_transport_joint(__deriv__(ZONAL_TRANSPORT),
     >  __deriv__(U))
      WRITE(*, *) 'zonal volume transport = ', (__value__(
     > ZONAL_TRANSPORT) * 9.99999999999999954748D-07), ' Sv'
      __value__(COST_FINAL) = __value__(ZONAL_TRANSPORT)
      IF(ITERATION .GE. 0) THEN
C       open(10,file='cost.txt',form='formatted',position='append')
        OPEN(UNIT = 10, POSITION = 'APPEND', FORM = 'FORMATTED', FILE =
     >  'cost.txt')
        WRITE(10, '(I5,5E15.8)') ITERATION, COST_D, COST_SD, COST_GD,
     >  COST, __value__(COST_FINAL)
C       close(10)
        CLOSE(UNIT = 10)
        ITERATION = (ITERATION + 1)
        OpenAD_Symbol_1308 = 1_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1308)
      ELSE
        OpenAD_Symbol_1309 = 0_w2f__i8
C       $OpenAD$ INLINE push_i_s0(subst)
        CALL push_i_s0(OpenAD_Symbol_1309)
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1286)
      IF(OpenAD_Symbol_1286 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE IncDeriv(subst,subst)
      CALL IncDeriv(__deriv__(COST_FINAL), __deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(COST_FINAL))
      CALL calc_zonal_transport_joint(__deriv__(ZONAL_TRANSPORT),
     >  __deriv__(U))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(ZONAL_TRANSPORT))
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1287)
      OpenAD_Symbol_1288 = 1
      DO WHILE(INT(OpenAD_Symbol_1288) .LE. INT(OpenAD_Symbol_1287))
        CALL loop_body_wrapper_outer(__deriv__(U), __deriv__(V),
     >  __deriv__(ETA), COST, CALC_COST, TIME, TIME_INDEX, NIO, IT,
     >  NTOTAL, NINNER)
        OpenAD_Symbol_1288 = INT(OpenAD_Symbol_1288) + 1
      END DO
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1289)
      IF(OpenAD_Symbol_1289 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1290)
      IF(OpenAD_Symbol_1290 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1291)
      IF(OpenAD_Symbol_1291 .ne. 0) THEN
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1292)
      IF(OpenAD_Symbol_1292 .ne. 0) THEN
        CALL cost_depth(COST_D)
      ENDIF
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1293)
      IF(OpenAD_Symbol_1293 .ne. 0) THEN
      ENDIF
      CALL initial_values()
      CALL calc_depth_uv()
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(NCTRL)
      CALL map_from_control_vector(NCTRL, __deriv__(XC))
C     $OpenAD$ INLINE pop_i_s0(subst)
      CALL pop_i_s0(OpenAD_Symbol_1294)
      IF(OpenAD_Symbol_1294 .ne. 0) THEN
      ENDIF
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 12
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(ETA))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_store_real_matrix_a_d(subst)
      CALL cp_arg_store_real_matrix_a_d(__deriv__(INVHV))
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 13
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(INVHV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(INVHU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(HV))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(HU))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(DEPTH))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(V))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(U))
C     $OpenAD$ INLINE cp_arg_restore_real_matrix_a_d(subst)
      CALL cp_arg_restore_real_matrix_a_d(__deriv__(ETA))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE
