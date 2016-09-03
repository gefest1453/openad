
      MODULE adsize
      use OAD_active
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
      use OAD_active
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
      use OAD_active
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
      use OAD_active
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: ETA(0:21,0:21)
      type(active) :: U(0:21,0:21)
      type(active) :: V(0:21,0:21)
C
C     **** Statements ****
C
      END MODULE

      MODULE pfields
      use OAD_active
      use w2f__types
      use size
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: DEPTH(0:21,0:21)
      REAL(w2f__8) DX(0 : 20)
      REAL(w2f__8) DY(0 : 20)
      REAL(w2f__8) ETAINI(1 : 20, 1 : 20)
      REAL(w2f__8) ETAMASK(0 : 21, 0 : 21)
      REAL(w2f__8) FCORIU(1 : 20, 1 : 20)
      REAL(w2f__8) FCORIV(1 : 20, 1 : 20)
      REAL(w2f__8) FRICT(0 : 21, 0 : 21)
      type(active) :: HU(0:21,0:21)
      type(active) :: HV(0:21,0:21)
      REAL(w2f__8) HY(0 : 21)
      REAL(w2f__8) INIDEPTH(1 : 20, 1 : 20)
      type(active) :: INVHU(0:21,0:21)
      type(active) :: INVHV(0:21,0:21)
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
      use OAD_active
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
      use OAD_active
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
      use OAD_active
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
      use OAD_active
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
      use OAD_active
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
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter inifields:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter inifields:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " inifields: entering plain"
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
          DEPTH(INT(IX),INT(IY))%v = 0.0D00
          FRICT(INT(IX),INT(IY)) = 0.0D00
          U(INT(IX),INT(IY))%v = 0.0D00
          V(INT(IX),INT(IY))%v = 0.0D00
          ETA(INT(IX),INT(IY))%v = 0.0D00
          UMASK(INT(IX),INT(IY)) = 0.0D00
          VMASK(INT(IX),INT(IY)) = 0.0D00
          ETAMASK(INT(IX),INT(IY)) = 0.0D00
          HU(INT(IX),INT(IY))%v = 0.0D00
          HV(INT(IX),INT(IY))%v = 0.0D00
          INVHU(INT(IX),INT(IY))%v = 0.0D00
          INVHV(INT(IX),INT(IY))%v = 0.0D00
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " inifields: entering tape"
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
      DO IX = 0,21,1
        X(INT(IX)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0,21,1
        Y(INT(IY)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IX = 0,20,1
        DX(INT(IX)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0,20,1
        DY(INT(IY)) = 0.0D00
      END DO
C$OPENAD XXX Simple loop
      DO IY = 0,21,1
        RX(INT(IY)) = 0.0D00
      END DO
      RY = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 0,21,1
        HY(INT(IY)) = 0.0D00
      END DO
      DT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          UFORCE(INT(IX),INT(IY)) = 0.0D00
          VFORCE(INT(IX),INT(IY)) = 0.0D00
          FCORIU(INT(IX),INT(IY)) = 0.0D00
          FCORIV(INT(IX),INT(IY)) = 0.0D00
          INIDEPTH(INT(IX),INT(IY)) = 0.0D00
          UINI(INT(IX),INT(IY)) = 0.0D00
          VINI(INT(IX),INT(IY)) = 0.0D00
          ETAINI(INT(IX),INT(IY)) = 0.0D00
          SCALEDEPTH(INT(IX),INT(IY)) = 0.0D00
          U_DATA(INT(IX),INT(IY)) = 0.0D00
          V_DATA(INT(IX),INT(IY)) = 0.0D00
          ETA_DATA(INT(IX),INT(IY)) = 0.0D00
          DEPTH_DATA(INT(IX),INT(IY)) = 0.0D00
          WEIGHT_U(INT(IX),INT(IY)) = 0.0D00
          WEIGHT_V(INT(IX),INT(IY)) = 0.0D00
          WEIGHT_ETA(INT(IX),INT(IY)) = 0.0D00
          WEIGHT_LAPLDEPTH(INT(IX),INT(IY)) = 0.0D00
          WEIGHT_GRADDEPTH(INT(IX),INT(IY)) = 0.0D00
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          WEIGHT_DEPTH(INT(IX),INT(IY)) = 1.0D00
        END DO
      END DO
      ZONAL_TRANSPORT_DATA = 0.0D00
      WEIGHT_ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IX = 0,21,1
        DO IY = 0,21,1
          DEPTH(INT(IX),INT(IY))%v = 0.0D00
          FRICT(INT(IX),INT(IY)) = 0.0D00
          U(INT(IX),INT(IY))%v = 0.0D00
          V(INT(IX),INT(IY))%v = 0.0D00
          ETA(INT(IX),INT(IY))%v = 0.0D00
          UMASK(INT(IX),INT(IY)) = 0.0D00
          VMASK(INT(IX),INT(IY)) = 0.0D00
          ETAMASK(INT(IX),INT(IY)) = 0.0D00
          HU(INT(IX),INT(IY))%v = 0.0D00
          HV(INT(IX),INT(IY))%v = 0.0D00
          INVHU(INT(IX),INT(IY))%v = 0.0D00
          INVHV(INT(IX),INT(IY))%v = 0.0D00
        END DO
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " inifields: entering adjoint"
      IX = 0+1*((21-0)/1)
      do while (IX.GE.0)
        IY = 0+1*((21-0)/1)
        do while (IY.GE.0)
          INVHV(IX,IY)%d = 0.0d0
          INVHU(IX,IY)%d = 0.0d0
          HV(IX,IY)%d = 0.0d0
          HU(IX,IY)%d = 0.0d0
          ETA(IX,IY)%d = 0.0d0
          V(IX,IY)%d = 0.0d0
          U(IX,IY)%d = 0.0d0
          DEPTH(IX,IY)%d = 0.0d0
          IY = IY-1
        END DO
        IX = IX-1
      END DO
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      IY = 0+1*((21-0)/1)
      do while (IY.GE.0)
        IY = IY-1
      END DO
      IY = 0+1*((21-0)/1)
      do while (IY.GE.0)
        IY = IY-1
      END DO
      IY = 0+1*((20-0)/1)
      do while (IY.GE.0)
        IY = IY-1
      END DO
      IX = 0+1*((20-0)/1)
      do while (IX.GE.0)
        IX = IX-1
      END DO
      IY = 0+1*((21-0)/1)
      do while (IY.GE.0)
        IY = IY-1
      END DO
      IX = 0+1*((21-0)/1)
      do while (IX.GE.0)
        IX = IX-1
      END DO
      end if
      end subroutine inifields

      SUBROUTINE readparms()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter readparms:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter readparms:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " readparms: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL read_data_file()
      CALL read_data_fields()
      CALL prep_depth()
      CALL check_cfl()
      CALL make_masks()
      CALL ini_scales()
      CALL prep_coriolis()
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " readparms: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL read_data_file()
      CALL read_data_fields()
      CALL prep_depth()
      CALL check_cfl()
      CALL make_masks()
      CALL ini_scales()
      CALL prep_coriolis()
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " readparms: entering adjoint"
      CALL prep_coriolis()
      CALL ini_scales()
      CALL make_masks()
      CALL check_cfl()
      CALL prep_depth()
      CALL read_data_fields()
      CALL read_data_file()
      end if
      end subroutine readparms

      SUBROUTINE read_data_file()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_data_file:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_data_file:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data_file: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
C     open(20,file='data',form='formatted',status='old')
      t__14 = 'OLD '
      OPEN(UNIT = 20, FORM = 'FORMATTED', STATUS = t__14(1_w2f__i8 : 3) 
     +, FILE = 'data')
      READ(20,*)
      READ(20,*) NT,NTSPINUP,DT,START_TIME,DT_DUMP
      READ(20,*)
      READ(20,*) RINI,F0,BETA
      READ(20,*)
      READ(20,*) XSTART,YSTART
      READ(20,*)
      READ(20,*) DELX,DELY
      READ(20,*)
      READ(20,*) XPERIODIC,YPERIODIC,SPHERICAL,CARTESIAN
      READ(20,*)
      READ(20,*) FULLIO,SUPPRESSIO
      READ(20,*)
      READ(20,*) INITIAL_GRAD,GRAD_CHECK,OPTIMIZE,CALC_HESS
      READ(20,*)
      READ(20,'(A80)') NCDATAFILE
      READ(20,*)
      READ(20,'(A80)') NCRESTARTFILE
      READ(20,*)
      READ(20,'(A80)') FOUTNAME
      READ(20,*)
      READ(20,'(A80)') RUNNAME
      READ(20,*)
      READ(20,*) WF_DEPTH,WF_ETA,WF_U,WF_V,WF_ZONAL_TRANSPORT,WF_LAPLDEP
     +TH,WF_GRADDEPTH
      READ(20,*)
      READ(20,*) NSIM
      READ(20,*)
      READ(20,*) EPSG,DF1,DXMIN,NITER,IMPRES,MODE
      READ(20,*)
      READ(20,*) EPS_GRAD,PGTOL,FACTR,IPRINT
C     close(20)
      CLOSE(UNIT = 20)
      IF(CARTESIAN .AND. SPHERICAL) THEN
        WRITE(*,*) 'grid specification is ambiguous'
      ELSE
        IF(.NOT.(CARTESIAN .OR. SPHERICAL)) THEN
          WRITE(*,*) 'grid specification is ambiguous'
        ENDIF
      ENDIF
      IF(SPHERICAL .AND. YPERIODIC) THEN
        WRITE(*,*) 'spherical grid and periodic boundary conditions'
        WRITE(*,*) 'in latitude do not make sense'
      ENDIF
      IF(YPERIODIC .AND.(SPHERICAL .OR.(CARTESIAN .AND.(BETA .ne. 0.0D00
     +)))) THEN
        WRITE(*,*) 'yperiodic boundaries only make sense on an f-pla'//'
     +ne'
      ENDIF
      IF(SPHERICAL) THEN
        DO IY = 0, 21, 1
          Y(INT(IY)) = (YSTART +(((DELY / 6.371D+06) *(REAL(IY) +( -5.0D
     +-01))) / 1.74532925199432954744D-02))
        END DO
        DO IX = 0, 21, 1
          X(INT(IX)) = (XSTART +(((DELX / 6.371D+06) *(REAL(IX) +( -5.0D
     +-01))) / 1.74532925199432954744D-02))
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
          RX(INT(IY)) = (COS((Y(IY) + Y(IY + 1)) * 5.0D-01 * 1.745329251
     +99432954744D-02) * 6.371D+06 * 1.74532925199432954744D-02)
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data_file: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
C     open(20,file='data',form='formatted',status='old')
      t__14 = 'OLD '
      OPEN(UNIT = 20, FORM = 'FORMATTED', STATUS = t__14(1_w2f__i8 : 3) 
     +, FILE = 'data')
      READ(20,*)
      READ(20,*) NT,NTSPINUP,DT,START_TIME,DT_DUMP
      READ(20,*)
      READ(20,*) RINI,F0,BETA
      READ(20,*)
      READ(20,*) XSTART,YSTART
      READ(20,*)
      READ(20,*) DELX,DELY
      READ(20,*)
      READ(20,*) XPERIODIC,YPERIODIC,SPHERICAL,CARTESIAN
      READ(20,*)
      READ(20,*) FULLIO,SUPPRESSIO
      READ(20,*)
      READ(20,*) INITIAL_GRAD,GRAD_CHECK,OPTIMIZE,CALC_HESS
      READ(20,*)
      READ(20,'(A80)') NCDATAFILE
      READ(20,*)
      READ(20,'(A80)') NCRESTARTFILE
      READ(20,*)
      READ(20,'(A80)') FOUTNAME
      READ(20,*)
      READ(20,'(A80)') RUNNAME
      READ(20,*)
      READ(20,*) WF_DEPTH,WF_ETA,WF_U,WF_V,WF_ZONAL_TRANSPORT,WF_LAPLDEP
     +TH,WF_GRADDEPTH
      READ(20,*)
      READ(20,*) NSIM
      READ(20,*)
      READ(20,*) EPSG,DF1,DXMIN,NITER,IMPRES,MODE
      READ(20,*)
      READ(20,*) EPS_GRAD,PGTOL,FACTR,IPRINT
C     close(20)
      CLOSE(UNIT = 20)
      IF(CARTESIAN .AND. SPHERICAL) THEN
        WRITE(*,*) 'grid specification is ambiguous'
        OpenAD_Symbol_66 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_66
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(.NOT.(CARTESIAN .OR. SPHERICAL)) THEN
          WRITE(*,*) 'grid specification is ambiguous'
          OpenAD_Symbol_64 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_64
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_65 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_65
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_67 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_67
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(SPHERICAL .AND. YPERIODIC) THEN
        WRITE(*,*) 'spherical grid and periodic boundary conditions'
        WRITE(*,*) 'in latitude do not make sense'
        OpenAD_Symbol_68 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_68
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_69 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_69
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC .AND.(SPHERICAL .OR.(CARTESIAN .AND.(BETA .ne. 0.0D00
     +)))) THEN
        WRITE(*,*) 'yperiodic boundaries only make sense on an f-pla'//'
     +ne'
        OpenAD_Symbol_70 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_70
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_71 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_71
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(SPHERICAL) THEN
        OpenAD_Symbol_72 = 0_w2f__i8
        DO IY = 0, 21, 1
          Y(INT(IY)) = (YSTART +(((DELY / 6.371D+06) *(REAL(IY) +( -5.0D
     +-01))) / 1.74532925199432954744D-02))
          OpenAD_Symbol_72 = (INT(OpenAD_Symbol_72) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_72
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_73 = 0_w2f__i8
        DO IX = 0, 21, 1
          X(INT(IX)) = (XSTART +(((DELX / 6.371D+06) *(REAL(IX) +( -5.0D
     +-01))) / 1.74532925199432954744D-02))
          OpenAD_Symbol_73 = (INT(OpenAD_Symbol_73) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_73
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_78 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_78
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(CARTESIAN) THEN
          OpenAD_Symbol_74 = 0_w2f__i8
          DO IX = 0, 21, 1
            X(INT(IX)) = (DELX *(REAL(IX) +(-5.0D-01)))
            OpenAD_Symbol_74 = (INT(OpenAD_Symbol_74) + INT(1_w2f__i8))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_74
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_75 = 0_w2f__i8
          DO IY = 0, 21, 1
            Y(INT(IY)) = (DELY *(REAL(IY) +(-5.0D-01)))
            OpenAD_Symbol_75 = (INT(OpenAD_Symbol_75) + INT(1_w2f__i8))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_75
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_77 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_77
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_76 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_76
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_79 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_79
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      OpenAD_Symbol_80 = 0_w2f__i8
      DO IX = 0, 20, 1
        DX(INT(IX)) = (X(IX + 1) - X(IX))
        OpenAD_Symbol_80 = (INT(OpenAD_Symbol_80) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_80
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_81 = 0_w2f__i8
      DO IY = 0, 20, 1
        DY(INT(IY)) = (Y(IY + 1) - Y(IY))
        OpenAD_Symbol_81 = (INT(OpenAD_Symbol_81) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_81
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_82 = 0_w2f__i8
      DO IY = 1, 20, 1
        IF(SPHERICAL) THEN
          RX(INT(IY)) = (COS((Y(IY) + Y(IY + 1)) * 5.0D-01 * 1.745329251
     +99432954744D-02) * 6.371D+06 * 1.74532925199432954744D-02)
          OpenAD_Symbol_85 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_85
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          IF(CARTESIAN) THEN
            RX(INT(IY)) = 1.0D00
            OpenAD_Symbol_83 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_83
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_84 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_84
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_86 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_86
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_82 = (INT(OpenAD_Symbol_82) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_82
      integer_tape_pointer = integer_tape_pointer+1
      IF(SPHERICAL) THEN
        RY = 1.11194926644558741827D+05
        OpenAD_Symbol_89 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_89
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(CARTESIAN) THEN
          RY = 1.0D00
          OpenAD_Symbol_87 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_87
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_88 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_88
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_90 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_90
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      OpenAD_Symbol_91 = 0_w2f__i8
      DO IY = 1, 21, 1
        IF(SPHERICAL) THEN
          HY(INT(IY)) = COS(Y(IY) * 1.74532925199432954744D-02)
          OpenAD_Symbol_94 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_94
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          IF(CARTESIAN) THEN
            HY(INT(IY)) = 1.0D00
            OpenAD_Symbol_92 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_92
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_93 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_93
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_95 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_95
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_91 = (INT(OpenAD_Symbol_91) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_91
      integer_tape_pointer = integer_tape_pointer+1
      CALL determine_data_time(NCDATAFILE)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data_file: entering adjoint"
      CALL determine_data_time(NCDATAFILE)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_36 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_37 = 1
      DO WHILE(INT(OpenAD_Symbol_37) .LE. INT(OpenAD_Symbol_36))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_38 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_38 .ne. 0) THEN
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_39 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_39 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_37 = INT(OpenAD_Symbol_37) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_40 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_40 .ne. 0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_41 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_41 .ne. 0) THEN
        ENDIF
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_42 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_43 = 1
      DO WHILE(INT(OpenAD_Symbol_43) .LE. INT(OpenAD_Symbol_42))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_44 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_44 .ne. 0) THEN
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_45 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_45 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_43 = INT(OpenAD_Symbol_43) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_46 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_47 = 1
      DO WHILE(INT(OpenAD_Symbol_47) .LE. INT(OpenAD_Symbol_46))
        OpenAD_Symbol_47 = INT(OpenAD_Symbol_47) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_48 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_49 = 1
      DO WHILE(INT(OpenAD_Symbol_49) .LE. INT(OpenAD_Symbol_48))
        OpenAD_Symbol_49 = INT(OpenAD_Symbol_49) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_50 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_50 .ne. 0) THEN
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_56 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_57 = 1
        DO WHILE(INT(OpenAD_Symbol_57) .LE. INT(OpenAD_Symbol_56))
          OpenAD_Symbol_57 = INT(OpenAD_Symbol_57) + 1
        END DO
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_58 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_59 = 1
        DO WHILE(INT(OpenAD_Symbol_59) .LE. INT(OpenAD_Symbol_58))
          OpenAD_Symbol_59 = INT(OpenAD_Symbol_59) + 1
        END DO
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_51 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_51 .ne. 0) THEN
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_52 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_53 = 1
          DO WHILE(INT(OpenAD_Symbol_53) .LE. INT(OpenAD_Symbol_52))
            OpenAD_Symbol_53 = INT(OpenAD_Symbol_53) + 1
          END DO
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_54 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_55 = 1
          DO WHILE(INT(OpenAD_Symbol_55) .LE. INT(OpenAD_Symbol_54))
            OpenAD_Symbol_55 = INT(OpenAD_Symbol_55) + 1
          END DO
        ENDIF
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_60 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_60 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_61 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_61 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_62 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_62 .ne. 0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_63 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_63 .ne. 0) THEN
        ENDIF
      ENDIF
      end if
      end subroutine read_data_file

      SUBROUTINE boundary_conditions(NX, NY, FIELD, XPERIODIC, YPERIODIC
     +)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter boundary_conditions:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter boundary_conditions:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " boundary_conditions: entering plai
C n"
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
        WRITE(*,*) 'boundary_conditions: ','make sure that the corners a
     +re handled correctly'
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " boundary_conditions: entering tape
C "
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(XPERIODIC) THEN
        OpenAD_Symbol_455 = 0_w2f__i8
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = FIELD(NX, IY)
          FIELD(NX + 1, INT(IY)) = FIELD(1, IY)
          OpenAD_Symbol_455 = (INT(OpenAD_Symbol_455) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_455
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_457 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_457
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_456 = 0_w2f__i8
        DO IY = 0, (NY + 1), 1
          FIELD(0, INT(IY)) = 0.0D00
          FIELD(NX + 1, INT(IY)) = 0.0D00
          OpenAD_Symbol_456 = (INT(OpenAD_Symbol_456) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_456
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_458 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_458
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC) THEN
        OpenAD_Symbol_459 = 0_w2f__i8
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = FIELD(IX, NY)
          FIELD(INT(IX), NY + 1) = FIELD(IX, 1)
          OpenAD_Symbol_459 = (INT(OpenAD_Symbol_459) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_459
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_461 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_461
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_460 = 0_w2f__i8
        DO IX = 0, (NX + 1), 1
          FIELD(INT(IX), 0) = 0.0D00
          FIELD(INT(IX), NY + 1) = 0.0D00
          OpenAD_Symbol_460 = (INT(OpenAD_Symbol_460) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_460
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_462 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_462
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(XPERIODIC .AND. YPERIODIC) THEN
        WRITE(*,*) 'boundary_conditions: ','make sure that the corners a
     +re handled correctly'
        OpenAD_Symbol_463 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_463
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_464 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_464
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " boundary_conditions: entering adjo
C int"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_444 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_444 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_445 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_445 .ne. 0) THEN
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_448 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_449 = 1
        DO WHILE(INT(OpenAD_Symbol_449) .LE. INT(OpenAD_Symbol_448))
          OpenAD_Symbol_449 = INT(OpenAD_Symbol_449) + 1
        END DO
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_446 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_447 = 1
        DO WHILE(INT(OpenAD_Symbol_447) .LE. INT(OpenAD_Symbol_446))
          OpenAD_Symbol_447 = INT(OpenAD_Symbol_447) + 1
        END DO
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_450 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_450 .ne. 0) THEN
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_453 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_454 = 1
        DO WHILE(INT(OpenAD_Symbol_454) .LE. INT(OpenAD_Symbol_453))
          OpenAD_Symbol_454 = INT(OpenAD_Symbol_454) + 1
        END DO
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_451 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_452 = 1
        DO WHILE(INT(OpenAD_Symbol_452) .LE. INT(OpenAD_Symbol_451))
          OpenAD_Symbol_452 = INT(OpenAD_Symbol_452) + 1
        END DO
      ENDIF
      end if
      end subroutine boundary_conditions

      SUBROUTINE read_data_fields()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_data_fields:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_data_fields:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data_fields: entering plain"
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
        WRITE(*,*) NCDATAFILE,' not found, cannot continue'
      ELSE
        STR1 = 'depth'
        STR2 = 'frict'
        CALL read_field(NCDATAFILE,MYTIME,STR1,INIDEPTH)
        CALL read_extended_field(NCDATAFILE,STR2,FRICT)
        IF(RINI .ne. 0.0D00) THEN
          WRITE(*,*) 'rini = ',RINI
          WRITE(*,*) 'will overwrite frict with rini'
          DO IX = 1, 20, 1
            DO IY = 1, 20, 1
              FRICT(INT(IX), INT(IY)) = RINI
            END DO
          END DO
        ENDIF
        CALL boundary_conditions(MYNX,MYNY,FRICT,XPERIODIC,YPERIODIC)
        STR1 = 'uforce'
        STR2 = 'vforce'
        CALL read_field(NCDATAFILE,MYTIME,STR1,UFORCE)
        CALL read_field(NCDATAFILE,MYTIME,STR2,VFORCE)
        IF(START_TIME .eq. 0.0D00) THEN
          WRITE(*,*) 'cold start from initial fields'
          STR1 = 'uini'
          STR2 = 'vini'
          STR3 = 'etaini'
          CALL read_field(NCDATAFILE,START_TIME,STR1,UINI)
          CALL read_field(NCDATAFILE,START_TIME,STR2,VINI)
          CALL read_field(NCDATAFILE,START_TIME,STR3,ETAINI)
        ELSE
          WRITE(*,*) 'warm restart from time ',START_TIME
          WRITE(*,*) 'in restart file ',NCRESTARTFILE
C         inquire( file = ncrestartfile, exist = exists )

          INQUIRE(EXIST = EXISTS, FILE = NCRESTARTFILE)
          IF(.NOT. EXISTS) THEN
            WRITE(*,*) NCRESTARTFILE,' not found'
          ELSE
            STR1 = 'U'
            STR2 = 'V'
            STR3 = 'ETA'
            CALL read_field(NCRESTARTFILE,START_TIME,STR1,UINI)
            CALL read_field(NCRESTARTFILE,START_TIME,STR2,VINI)
            CALL read_field(NCRESTARTFILE,START_TIME,STR3,ETAINI)
          ENDIF
        ENDIF
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data_fields: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYTIME = (-1.0D00)
      OpenAD_Symbol_168 = 0_w2f__i8
      DO IX = 1, 20, 1
        X_IN(INT(IX)) = 0.0D00
        OpenAD_Symbol_168 = (INT(OpenAD_Symbol_168) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_168
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_169 = 0_w2f__i8
      DO IY = 1, 20, 1
        Y_IN(INT(IY)) = 0.0D00
        OpenAD_Symbol_169 = (INT(OpenAD_Symbol_169) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_169
      integer_tape_pointer = integer_tape_pointer+1
C     inquire(file=ncdatafile,exist=exists)
      INQUIRE(EXIST = EXISTS, FILE = NCDATAFILE)
      IF(.NOT. EXISTS) THEN
        WRITE(*,*) NCDATAFILE,' not found, cannot continue'
        OpenAD_Symbol_178 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_178
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        STR1 = 'depth'
        STR2 = 'frict'
        CALL read_field(NCDATAFILE,MYTIME,STR1,INIDEPTH)
        CALL read_extended_field(NCDATAFILE,STR2,FRICT)
        IF(RINI .ne. 0.0D00) THEN
          WRITE(*,*) 'rini = ',RINI
          WRITE(*,*) 'will overwrite frict with rini'
          OpenAD_Symbol_170 = 0_w2f__i8
          DO IX = 1, 20, 1
            OpenAD_Symbol_171 = 0_w2f__i8
            DO IY = 1, 20, 1
              FRICT(INT(IX), INT(IY)) = RINI
              OpenAD_Symbol_171 = (INT(OpenAD_Symbol_171) + INT( 1_w2f__
     +i8))
            END DO
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_171
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_170 = (INT(OpenAD_Symbol_170) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_170
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_173 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_173
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_172 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_172
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        CALL boundary_conditions(MYNX,MYNY,FRICT,XPERIODIC,YPERIODIC)
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = MYNX
        integer_tape_pointer = integer_tape_pointer+1
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = MYNY
        integer_tape_pointer = integer_tape_pointer+1
        STR1 = 'uforce'
        STR2 = 'vforce'
        CALL read_field(NCDATAFILE,MYTIME,STR1,UFORCE)
        CALL read_field(NCDATAFILE,MYTIME,STR2,VFORCE)
        IF(START_TIME .eq. 0.0D00) THEN
          WRITE(*,*) 'cold start from initial fields'
          STR1 = 'uini'
          STR2 = 'vini'
          STR3 = 'etaini'
          CALL read_field(NCDATAFILE,START_TIME,STR1,UINI)
          CALL read_field(NCDATAFILE,START_TIME,STR2,VINI)
          CALL read_field(NCDATAFILE,START_TIME,STR3,ETAINI)
          OpenAD_Symbol_176 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_176
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          WRITE(*,*) 'warm restart from time ',START_TIME
          WRITE(*,*) 'in restart file ',NCRESTARTFILE
C         inquire( file = ncrestartfile, exist = exists )

          INQUIRE(EXIST = EXISTS, FILE = NCRESTARTFILE)
          IF(.NOT. EXISTS) THEN
            WRITE(*,*) NCRESTARTFILE,' not found'
            OpenAD_Symbol_174 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_174
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            STR1 = 'U'
            STR2 = 'V'
            STR3 = 'ETA'
            CALL read_field(NCRESTARTFILE,START_TIME,STR1,UINI)
            CALL read_field(NCRESTARTFILE,START_TIME,STR2,VINI)
            CALL read_field(NCRESTARTFILE,START_TIME,STR3,ETAINI)
            OpenAD_Symbol_175 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_175
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_177 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_177
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_179 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_179
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data_fields: entering adjoint
C "
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_156 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_156 .ne. 0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_157 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_157 .ne. 0) THEN
          CALL read_field(NCDATAFILE,START_TIME,STR3,ETAINI)
          CALL read_field(NCDATAFILE,START_TIME,STR2,VINI)
          CALL read_field(NCDATAFILE,START_TIME,STR1,UINI)
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_158 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_158 .ne. 0) THEN
          ELSE
            CALL read_field(NCRESTARTFILE,START_TIME,STR3,ETAINI)
            CALL read_field(NCRESTARTFILE,START_TIME,STR2,VINI)
            CALL read_field(NCRESTARTFILE,START_TIME,STR1,UINI)
          ENDIF
        ENDIF
        CALL read_field(NCDATAFILE,MYTIME,STR2,VFORCE)
        CALL read_field(NCDATAFILE,MYTIME,STR1,UFORCE)
        integer_tape_pointer = integer_tape_pointer-1
        MYNY = integer_tape(integer_tape_pointer)
        integer_tape_pointer = integer_tape_pointer-1
        MYNX = integer_tape(integer_tape_pointer)
        CALL boundary_conditions(MYNX,MYNY,FRICT,XPERIODIC,YPERIODIC)
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_159 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_159 .ne. 0) THEN
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_160 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_161 = 1
          DO WHILE(INT(OpenAD_Symbol_161) .LE. INT(OpenAD_Symbol_160))
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_162 = integer_tape(integer_tape_pointer)
            OpenAD_Symbol_163 = 1
            DO WHILE(INT(OpenAD_Symbol_163) .LE. INT(OpenAD_Symbol_162) 
     +)
              OpenAD_Symbol_163 = INT(OpenAD_Symbol_163) + 1
            END DO
            OpenAD_Symbol_161 = INT(OpenAD_Symbol_161) + 1
          END DO
        ENDIF
        CALL read_extended_field(NCDATAFILE,STR2,FRICT)
        CALL read_field(NCDATAFILE,MYTIME,STR1,INIDEPTH)
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_164 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_165 = 1
      DO WHILE(INT(OpenAD_Symbol_165) .LE. INT(OpenAD_Symbol_164))
        OpenAD_Symbol_165 = INT(OpenAD_Symbol_165) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_166 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_167 = 1
      DO WHILE(INT(OpenAD_Symbol_167) .LE. INT(OpenAD_Symbol_166))
        OpenAD_Symbol_167 = INT(OpenAD_Symbol_167) + 1
      END DO
      end if
      end subroutine read_data_fields

      SUBROUTINE prep_depth()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter prep_depth:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter prep_depth:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " prep_depth: entering plain"
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
          DEPTH(INT(IX),INT(IY))%v = INIDEPTH(IX,IY)
          SCALEDEPTH(INT(IX),INT(IY)) = INIDEPTH(IX,IY)
          IF (DEPTH(IX,IY)%v.LT.MAXDEPTH) THEN
          ENDIF
        END DO
      END DO
      CALL read_depth_data()
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_0,DEPTH)
      CALL boundary_conditions(MYNX,MYNY,OpenAD_tyc_0,XPERIODIC,YPERIODI
     +C)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(DEPTH,OpenAD_tyc_0)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " prep_depth: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MAXDEPTH = 0.0D00
      OpenAD_Symbol_214 = 0_w2f__i8
      DO IY = 1,20,1
        OpenAD_Symbol_215 = 0_w2f__i8
        DO IX = 1,20,1
          IF (INIDEPTH(IX,IY).GT.MAXDEPTH) THEN
            MAXDEPTH = INIDEPTH(IX,IY)
            OpenAD_Symbol_216 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_216
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_217 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_217
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_215 = (INT(OpenAD_Symbol_215)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_215
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_214 = (INT(OpenAD_Symbol_214)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_214
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_218 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_219 = 0_w2f__i8
        DO IY = 1,20,1
          DEPTH(INT(IX),INT(IY))%v = INIDEPTH(IX,IY)
          SCALEDEPTH(INT(IX),INT(IY)) = INIDEPTH(IX,IY)
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = IX
          integer_tape_pointer = integer_tape_pointer+1
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = IY
          integer_tape_pointer = integer_tape_pointer+1
          IF (DEPTH(IX,IY)%v.LT.MAXDEPTH) THEN
            OpenAD_Symbol_220 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_220
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_221 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_221
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_219 = (INT(OpenAD_Symbol_219)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_219
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_218 = (INT(OpenAD_Symbol_218)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_218
      integer_tape_pointer = integer_tape_pointer+1
      CALL read_depth_data()
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_0,DEPTH)
      CALL boundary_conditions(MYNX,MYNY,OpenAD_tyc_0,XPERIODIC,YPERIODI
     +C)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(DEPTH,OpenAD_tyc_0)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNX
      integer_tape_pointer = integer_tape_pointer+1
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNY
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " prep_depth: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      MYNY = integer_tape(integer_tape_pointer)
      integer_tape_pointer = integer_tape_pointer-1
      MYNX = integer_tape(integer_tape_pointer)
      CALL boundary_conditions(MYNX,MYNY,OpenAD_tyc_5,XPERIODIC,YPERIODI
     +C)
      CALL read_depth_data()
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_204 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_205 = 1
      do while (INT(OpenAD_Symbol_205).LE.INT(OpenAD_Symbol_204))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_206 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_207 = 1
        do while (INT(OpenAD_Symbol_207).LE.INT(OpenAD_Symbol_206))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_208 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_208.ne.0) THEN
          ENDIF
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_1312 = integer_tape(integer_tape_pointer)
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_1313 = integer_tape(integer_tape_pointer)
          DEPTH(OpenAD_Symbol_1313,OpenAD_Symbol_1312)%d = 0.0d0
          OpenAD_Symbol_207 = INT(OpenAD_Symbol_207)+1
        END DO
        OpenAD_Symbol_205 = INT(OpenAD_Symbol_205)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_209 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_210 = 1
      do while (INT(OpenAD_Symbol_210).LE.INT(OpenAD_Symbol_209))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_211 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_212 = 1
        do while (INT(OpenAD_Symbol_212).LE.INT(OpenAD_Symbol_211))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_213 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_213.ne.0) THEN
          ENDIF
          OpenAD_Symbol_212 = INT(OpenAD_Symbol_212)+1
        END DO
        OpenAD_Symbol_210 = INT(OpenAD_Symbol_210)+1
      END DO
      end if
      end subroutine prep_depth

      SUBROUTINE ini_scales()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter ini_scales:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter ini_scales:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " ini_scales: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL variance(MYNX,MYNY,UINI,UMASK,VARU)
      IF(VARU .eq. 0.0D00) THEN
        VARU = 1.0D00
      ENDIF
      CALL variance(MYNX,MYNY,VINI,VMASK,VARV)
      IF(VARU .eq. 0.0D00) THEN
        VARV = 1.0D00
      ENDIF
      CALL variance(MYNX,MYNY,ETAINI,ETAMASK,VARETA)
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " ini_scales: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL variance(MYNX,MYNY,UINI,UMASK,VARU)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNX
      integer_tape_pointer = integer_tape_pointer+1
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNY
      integer_tape_pointer = integer_tape_pointer+1
      IF(VARU .eq. 0.0D00) THEN
        VARU = 1.0D00
        OpenAD_Symbol_361 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_361
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_362 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_362
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      CALL variance(MYNX,MYNY,VINI,VMASK,VARV)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNX
      integer_tape_pointer = integer_tape_pointer+1
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNY
      integer_tape_pointer = integer_tape_pointer+1
      IF(VARU .eq. 0.0D00) THEN
        VARV = 1.0D00
        OpenAD_Symbol_363 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_363
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_364 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_364
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      CALL variance(MYNX,MYNY,ETAINI,ETAMASK,VARETA)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNX
      integer_tape_pointer = integer_tape_pointer+1
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNY
      integer_tape_pointer = integer_tape_pointer+1
      IF(VARU .eq. 0.0D00) THEN
        VARETA = 1.0D00
        OpenAD_Symbol_365 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_365
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_366 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_366
        integer_tape_pointer = integer_tape_pointer+1
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
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_368
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_367 = (INT(OpenAD_Symbol_367) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_367
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " ini_scales: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_354 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_355 = 1
      DO WHILE(INT(OpenAD_Symbol_355) .LE. INT(OpenAD_Symbol_354))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_356 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_357 = 1
        DO WHILE(INT(OpenAD_Symbol_357) .LE. INT(OpenAD_Symbol_356))
          OpenAD_Symbol_357 = INT(OpenAD_Symbol_357) + 1
        END DO
        OpenAD_Symbol_355 = INT(OpenAD_Symbol_355) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_358 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_358 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      MYNY = integer_tape(integer_tape_pointer)
      integer_tape_pointer = integer_tape_pointer-1
      MYNX = integer_tape(integer_tape_pointer)
      CALL variance(MYNX,MYNY,ETAINI,ETAMASK,VARETA)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_359 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_359 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      MYNY = integer_tape(integer_tape_pointer)
      integer_tape_pointer = integer_tape_pointer-1
      MYNX = integer_tape(integer_tape_pointer)
      CALL variance(MYNX,MYNY,VINI,VMASK,VARV)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_360 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_360 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      MYNY = integer_tape(integer_tape_pointer)
      integer_tape_pointer = integer_tape_pointer-1
      MYNX = integer_tape(integer_tape_pointer)
      CALL variance(MYNX,MYNY,UINI,UMASK,VARU)
      end if
      end subroutine ini_scales

      SUBROUTINE prep_coriolis()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter prep_coriolis:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter prep_coriolis:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " prep_coriolis: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MY = NINT(REAL(20) * 5.0D-01)
      DO IY = 0, 21, 1
        IF(SPHERICAL) THEN
          FAUX = (SIN(Y(IY) * 1.74532925199432954744D-02) * OM * 2.0D00 
     +)
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
     +IX, IY) + VMASK(IX + (-1), IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
          ELSE
            FAUX = (UMASK(IX, IY) * 2.5D-01)
          ENDIF
          FCORIU(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX + 
     +(-1), IY)) * 5.0D-01)
          FAUX = (UMASK(IX + 1, IY + (-1)) + UMASK(IX, IY + (-1)) + UMAS
     +K(IX, IY) + UMASK(IX + 1, IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
          ELSE
            FAUX = (VMASK(IX, IY) * 2.5D-01)
          ENDIF
          FCORIV(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX, I
     +Y + (-1))) * 5.0D-01)
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " prep_coriolis: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MY = NINT(REAL(20) * 5.0D-01)
      OpenAD_Symbol_396 = 0_w2f__i8
      DO IY = 0, 21, 1
        IF(SPHERICAL) THEN
          FAUX = (SIN(Y(IY) * 1.74532925199432954744D-02) * OM * 2.0D00 
     +)
          OpenAD_Symbol_399 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_399
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          IF(CARTESIAN) THEN
            FAUX = (F0 + BETA *(Y(IY) - Y(MY)))
            OpenAD_Symbol_397 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_397
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_398 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_398
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_400 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_400
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_401 = 0_w2f__i8
        DO IX = 0, 21, 1
          FCORI(INT(IX), INT(IY)) = FAUX
          OpenAD_Symbol_401 = (INT(OpenAD_Symbol_401) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_401
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_396 = (INT(OpenAD_Symbol_396) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_396
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_402 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_403 = 0_w2f__i8
        DO IY = 1, 20, 1
          FAUX = (VMASK(IX + (-1), IY + 1) + VMASK(IX, IY + 1) + VMASK( 
     +IX, IY) + VMASK(IX + (-1), IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
            OpenAD_Symbol_404 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_404
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            FAUX = (UMASK(IX, IY) * 2.5D-01)
            OpenAD_Symbol_405 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_405
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          FCORIU(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX + 
     +(-1), IY)) * 5.0D-01)
          FAUX = (UMASK(IX + 1, IY + (-1)) + UMASK(IX, IY + (-1)) + UMAS
     +K(IX, IY) + UMASK(IX + 1, IY))
          IF(FAUX .eq. 0.0D00) THEN
            FAUX = 0.0D00
            OpenAD_Symbol_406 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_406
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            FAUX = (VMASK(IX, IY) * 2.5D-01)
            OpenAD_Symbol_407 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_407
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          FCORIV(INT(IX), INT(IY)) = (FAUX *(FCORI(IX, IY) + FCORI(IX, I
     +Y + (-1))) * 5.0D-01)
          OpenAD_Symbol_403 = (INT(OpenAD_Symbol_403) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_403
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_402 = (INT(OpenAD_Symbol_402) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_402
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " prep_coriolis: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_384 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_385 = 1
      DO WHILE(INT(OpenAD_Symbol_385) .LE. INT(OpenAD_Symbol_384))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_386 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_387 = 1
        DO WHILE(INT(OpenAD_Symbol_387) .LE. INT(OpenAD_Symbol_386))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_388 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_388 .ne. 0) THEN
          ENDIF
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_389 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_389 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_387 = INT(OpenAD_Symbol_387) + 1
        END DO
        OpenAD_Symbol_385 = INT(OpenAD_Symbol_385) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_390 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_391 = 1
      DO WHILE(INT(OpenAD_Symbol_391) .LE. INT(OpenAD_Symbol_390))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_392 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_393 = 1
        DO WHILE(INT(OpenAD_Symbol_393) .LE. INT(OpenAD_Symbol_392))
          OpenAD_Symbol_393 = INT(OpenAD_Symbol_393) + 1
        END DO
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_394 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_394 .ne. 0) THEN
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_395 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_395 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_391 = INT(OpenAD_Symbol_391) + 1
      END DO
      end if
      end subroutine prep_coriolis

      SUBROUTINE make_masks()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_masks:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_masks:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_masks: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          IF (DEPTH(IX,IY)%v.LT.DEPTH(IX+(-1),IY)%v) THEN
            MINDEPTH = DEPTH(IX,IY)%v
          ELSE
            MINDEPTH = DEPTH(IX+(-1),IY)%v
          ENDIF
          IF (MINDEPTH.ne.0.0D00) THEN
            UMASK(INT(IX),INT(IY)) = 1.0D00
          ELSE
            UMASK(INT(IX),INT(IY)) = 0.0D00
          ENDIF
        END DO
      END DO
      DO IX = 1,20,1
        DO IY = 1,20,1
          IF (DEPTH(IX,IY)%v.LT.DEPTH(IX,IY+(-1))%v) THEN
            MINDEPTH = DEPTH(IX,IY)%v
          ELSE
            MINDEPTH = DEPTH(IX,IY+(-1))%v
          ENDIF
          IF (MINDEPTH.ne.0.0D00) THEN
            VMASK(INT(IX),INT(IY)) = 1.0D00
          ELSE
            VMASK(INT(IX),INT(IY)) = 0.0D00
          ENDIF
        END DO
      END DO
      DO IX = 1,20,1
        DO IY = 1,20,1
          IF (DEPTH(IX,IY)%v.ne.0.0D00) THEN
            ETAMASK(INT(IX),INT(IY)) = 1.0D00
          ELSE
            ETAMASK(INT(IX),INT(IY)) = 0.0D00
          ENDIF
        END DO
      END DO
      CALL boundary_conditions(MYNX,MYNY,UMASK,XPERIODIC,YPERIODIC)
      CALL boundary_conditions(MYNX,MYNY,VMASK,XPERIODIC,YPERIODIC)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_masks: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_305 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_306 = 0_w2f__i8
        DO IY = 1,20,1
          IF (DEPTH(IX,IY)%v.LT.DEPTH(IX+(-1),IY)%v) THEN
            MINDEPTH = DEPTH(IX,IY)%v
            OpenAD_Symbol_307 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_307
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            MINDEPTH = DEPTH(IX+(-1),IY)%v
            OpenAD_Symbol_308 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_308
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          IF (MINDEPTH.ne.0.0D00) THEN
            UMASK(INT(IX),INT(IY)) = 1.0D00
            OpenAD_Symbol_309 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_309
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            UMASK(INT(IX),INT(IY)) = 0.0D00
            OpenAD_Symbol_310 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_310
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_306 = (INT(OpenAD_Symbol_306)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_306
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_305 = (INT(OpenAD_Symbol_305)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_305
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_311 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_312 = 0_w2f__i8
        DO IY = 1,20,1
          IF (DEPTH(IX,IY)%v.LT.DEPTH(IX,IY+(-1))%v) THEN
            MINDEPTH = DEPTH(IX,IY)%v
            OpenAD_Symbol_313 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_313
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            MINDEPTH = DEPTH(IX,IY+(-1))%v
            OpenAD_Symbol_314 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_314
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          IF (MINDEPTH.ne.0.0D00) THEN
            VMASK(INT(IX),INT(IY)) = 1.0D00
            OpenAD_Symbol_315 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_315
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            VMASK(INT(IX),INT(IY)) = 0.0D00
            OpenAD_Symbol_316 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_316
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_312 = (INT(OpenAD_Symbol_312)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_312
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_311 = (INT(OpenAD_Symbol_311)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_311
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_317 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_318 = 0_w2f__i8
        DO IY = 1,20,1
          IF (DEPTH(IX,IY)%v.ne.0.0D00) THEN
            ETAMASK(INT(IX),INT(IY)) = 1.0D00
            OpenAD_Symbol_319 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_319
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            ETAMASK(INT(IX),INT(IY)) = 0.0D00
            OpenAD_Symbol_320 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_320
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_318 = (INT(OpenAD_Symbol_318)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_318
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_317 = (INT(OpenAD_Symbol_317)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_317
      integer_tape_pointer = integer_tape_pointer+1
      CALL boundary_conditions(MYNX,MYNY,UMASK,XPERIODIC,YPERIODIC)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNX
      integer_tape_pointer = integer_tape_pointer+1
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNY
      integer_tape_pointer = integer_tape_pointer+1
      CALL boundary_conditions(MYNX,MYNY,VMASK,XPERIODIC,YPERIODIC)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNX
      integer_tape_pointer = integer_tape_pointer+1
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = MYNY
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_masks: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      MYNY = integer_tape(integer_tape_pointer)
      integer_tape_pointer = integer_tape_pointer-1
      MYNX = integer_tape(integer_tape_pointer)
      CALL boundary_conditions(MYNX,MYNY,VMASK,XPERIODIC,YPERIODIC)
      integer_tape_pointer = integer_tape_pointer-1
      MYNY = integer_tape(integer_tape_pointer)
      integer_tape_pointer = integer_tape_pointer-1
      MYNX = integer_tape(integer_tape_pointer)
      CALL boundary_conditions(MYNX,MYNY,UMASK,XPERIODIC,YPERIODIC)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_288 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_289 = 1
      do while (INT(OpenAD_Symbol_289).LE.INT(OpenAD_Symbol_288))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_290 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_291 = 1
        do while (INT(OpenAD_Symbol_291).LE.INT(OpenAD_Symbol_290))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_292 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_292.ne.0) THEN
          ENDIF
          OpenAD_Symbol_291 = INT(OpenAD_Symbol_291)+1
        END DO
        OpenAD_Symbol_289 = INT(OpenAD_Symbol_289)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_293 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_294 = 1
      do while (INT(OpenAD_Symbol_294).LE.INT(OpenAD_Symbol_293))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_295 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_296 = 1
        do while (INT(OpenAD_Symbol_296).LE.INT(OpenAD_Symbol_295))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_297 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_297.ne.0) THEN
          ENDIF
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_298 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_298.ne.0) THEN
          ENDIF
          OpenAD_Symbol_296 = INT(OpenAD_Symbol_296)+1
        END DO
        OpenAD_Symbol_294 = INT(OpenAD_Symbol_294)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_299 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_300 = 1
      do while (INT(OpenAD_Symbol_300).LE.INT(OpenAD_Symbol_299))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_301 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_302 = 1
        do while (INT(OpenAD_Symbol_302).LE.INT(OpenAD_Symbol_301))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_303 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_303.ne.0) THEN
          ENDIF
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_304 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_304.ne.0) THEN
          ENDIF
          OpenAD_Symbol_302 = INT(OpenAD_Symbol_302)+1
        END DO
        OpenAD_Symbol_300 = INT(OpenAD_Symbol_300)+1
      END DO
      end if
      end subroutine make_masks

      SUBROUTINE variance(NX, NY, F, FMASK, VARF)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter variance:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter variance:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " variance: entering plain"
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " variance: entering tape"
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
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_565
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_566 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_566
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_564 = (INT(OpenAD_Symbol_564) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_564
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_563 = (INT(OpenAD_Symbol_563) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_563
      integer_tape_pointer = integer_tape_pointer+1
      IF(K .ne. 0) THEN
        MEANF = (MEANF / REAL(K))
        OpenAD_Symbol_567 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_567
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_568 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_568
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      OpenAD_Symbol_569 = 0_w2f__i8
      DO IY = 1, NY, 1
        OpenAD_Symbol_570 = 0_w2f__i8
        DO IX = 1, NX, 1
          VARF = (VARF + FMASK(IX, IY) *((F(IX, IY) - MEANF) ** 2))
          OpenAD_Symbol_570 = (INT(OpenAD_Symbol_570) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_570
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_569 = (INT(OpenAD_Symbol_569) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_569
      integer_tape_pointer = integer_tape_pointer+1
      IF(K .GT. 1) THEN
        VARF = (VARF / REAL(K +(-1)))
        OpenAD_Symbol_571 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_571
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_572 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_572
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " variance: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_552 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_552 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_553 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_554 = 1
      DO WHILE(INT(OpenAD_Symbol_554) .LE. INT(OpenAD_Symbol_553))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_555 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_556 = 1
        DO WHILE(INT(OpenAD_Symbol_556) .LE. INT(OpenAD_Symbol_555))
          OpenAD_Symbol_556 = INT(OpenAD_Symbol_556) + 1
        END DO
        OpenAD_Symbol_554 = INT(OpenAD_Symbol_554) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_557 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_557 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_558 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_559 = 1
      DO WHILE(INT(OpenAD_Symbol_559) .LE. INT(OpenAD_Symbol_558))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_560 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_561 = 1
        DO WHILE(INT(OpenAD_Symbol_561) .LE. INT(OpenAD_Symbol_560))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_562 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_562 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_561 = INT(OpenAD_Symbol_561) + 1
        END DO
        OpenAD_Symbol_559 = INT(OpenAD_Symbol_559) + 1
      END DO
      end if
      end subroutine variance

      SUBROUTINE check_cfl()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter check_cfl:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter check_cfl:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " check_cfl: entering plain"
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
          IF (DEPTH(IX,IY)%v.LT.MDEP) THEN
            MAXIMUM = MDEP
          ELSE
            MAXIMUM = DEPTH(IX,IY)%v
          ENDIF
          MDEP = MAXIMUM
        END DO
      END DO
      WAVESPEED = SQRT(MDEP*9.81000000000000049738D00)
      CFLX = ((WAVESPEED*DT)/MDX)
      CFLY = ((WAVESPEED*DT)/MDY)
      WRITE(*,*) 'rough check of CLF criterion:'
      IF ((CFLX.GE.1.0D00).OR.(CFLY.GE.1.0D00)) THEN
        WRITE(*,*) 'warning: CLF criterion not met'
        WRITE(*,*) 'sqrt(g*max(depth))*dt/min(dx) = ',CFLX
        WRITE(*,*) 'sqrt(g*max(depth))*dt/min(dy) = ',CFLY
      ELSE
        WRITE(*,*) 'OK'
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " check_cfl: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MDEP = 0.0D00
      MDX = 9.99999999999999916114D+22
      MDY = 9.99999999999999916114D+22
      OpenAD_Symbol_252 = 0_w2f__i8
      DO IX = 1,20,1
        IF (DX(IX).GT.MDX) THEN
          MINIMUM = MDX
          OpenAD_Symbol_253 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_253
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          MINIMUM = DX(IX)
          OpenAD_Symbol_254 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_254
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        MDX = MINIMUM
        OpenAD_Symbol_252 = (INT(OpenAD_Symbol_252)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_252
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_255 = 0_w2f__i8
      DO IY = 1,20,1
        IF (DY(IY).GT.MDY) THEN
          MINIMUM = MDY
          OpenAD_Symbol_256 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_256
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          MINIMUM = DY(IY)
          OpenAD_Symbol_257 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_257
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        MDY = MINIMUM
        OpenAD_Symbol_255 = (INT(OpenAD_Symbol_255)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_255
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_258 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_259 = 0_w2f__i8
        DO IY = 1,20,1
          IF (DEPTH(IX,IY)%v.LT.MDEP) THEN
            MAXIMUM = MDEP
            OpenAD_Symbol_260 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_260
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            MAXIMUM = DEPTH(IX,IY)%v
            OpenAD_Symbol_261 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_261
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          MDEP = MAXIMUM
          OpenAD_Symbol_259 = (INT(OpenAD_Symbol_259)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_259
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_258 = (INT(OpenAD_Symbol_258)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_258
      integer_tape_pointer = integer_tape_pointer+1
      WAVESPEED = SQRT(MDEP*9.81000000000000049738D00)
      CFLX = ((WAVESPEED*DT)/MDX)
      CFLY = ((WAVESPEED*DT)/MDY)
      WRITE(*,*) 'rough check of CLF criterion:'
      IF ((CFLX.GE.1.0D00).OR.(CFLY.GE.1.0D00)) THEN
        WRITE(*,*) 'warning: CLF criterion not met'
        WRITE(*,*) 'sqrt(g*max(depth))*dt/min(dx) = ',CFLX
        WRITE(*,*) 'sqrt(g*max(depth))*dt/min(dy) = ',CFLY
        OpenAD_Symbol_262 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_262
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        WRITE(*,*) 'OK'
        OpenAD_Symbol_263 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_263
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " check_cfl: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_240 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_240.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_241 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_242 = 1
      do while (INT(OpenAD_Symbol_242).LE.INT(OpenAD_Symbol_241))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_243 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_244 = 1
        do while (INT(OpenAD_Symbol_244).LE.INT(OpenAD_Symbol_243))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_245 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_245.ne.0) THEN
          ENDIF
          OpenAD_Symbol_244 = INT(OpenAD_Symbol_244)+1
        END DO
        OpenAD_Symbol_242 = INT(OpenAD_Symbol_242)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_246 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_247 = 1
      do while (INT(OpenAD_Symbol_247).LE.INT(OpenAD_Symbol_246))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_248 = integer_tape(integer_tape_pointer)
        IF (OpenAD_Symbol_248.ne.0) THEN
        ENDIF
        OpenAD_Symbol_247 = INT(OpenAD_Symbol_247)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_249 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_250 = 1
      do while (INT(OpenAD_Symbol_250).LE.INT(OpenAD_Symbol_249))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_251 = integer_tape(integer_tape_pointer)
        IF (OpenAD_Symbol_251.ne.0) THEN
        ENDIF
        OpenAD_Symbol_250 = INT(OpenAD_Symbol_250)+1
      END DO
      end if
      end subroutine check_cfl

      SUBROUTINE read_extended_field(NCDATAFILE, FNAME, FIELD)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_extended_field:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_extended_field:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_extended_field: entering plai
C n"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
        END DO
      END DO
      CALL read_field_netcdf(NCDATAFILE,FNAME,MYNX,MYNY,F_IN)
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_extended_field: entering tape
C "
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
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_525
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_524 = (INT(OpenAD_Symbol_524) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_524
      integer_tape_pointer = integer_tape_pointer+1
      CALL read_field_netcdf(NCDATAFILE,FNAME,MYNX,MYNY,F_IN)
      OpenAD_Symbol_526 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_527 = 0_w2f__i8
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
          OpenAD_Symbol_527 = (INT(OpenAD_Symbol_527) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_527
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_526 = (INT(OpenAD_Symbol_526) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_526
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_extended_field: entering adjo
C int"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_516 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_517 = 1
      DO WHILE(INT(OpenAD_Symbol_517) .LE. INT(OpenAD_Symbol_516))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_518 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_519 = 1
        DO WHILE(INT(OpenAD_Symbol_519) .LE. INT(OpenAD_Symbol_518))
          OpenAD_Symbol_519 = INT(OpenAD_Symbol_519) + 1
        END DO
        OpenAD_Symbol_517 = INT(OpenAD_Symbol_517) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_520 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_521 = 1
      DO WHILE(INT(OpenAD_Symbol_521) .LE. INT(OpenAD_Symbol_520))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_522 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_523 = 1
        DO WHILE(INT(OpenAD_Symbol_523) .LE. INT(OpenAD_Symbol_522))
          OpenAD_Symbol_523 = INT(OpenAD_Symbol_523) + 1
        END DO
        OpenAD_Symbol_521 = INT(OpenAD_Symbol_521) + 1
      END DO
      end if
      end subroutine read_extended_field

      SUBROUTINE read_field(NCDATAFILE, START_TIME, FNAME, FIELD)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_field:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_field:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_field: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
        END DO
      END DO
      IF(START_TIME .LE. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE,FNAME,MYNX,MYNY,F_IN)
      ELSE
        CALL read_snap_netcdf(NCDATAFILE,START_TIME,MYNX,MYNY,FNAME,F_IN
     +)
      ENDIF
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_field: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      OpenAD_Symbol_495 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_496 = 0_w2f__i8
        DO IY = 1, 20, 1
          F_IN(INT(IX), INT(IY)) = 0.0D00
          OpenAD_Symbol_496 = (INT(OpenAD_Symbol_496) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_496
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_495 = (INT(OpenAD_Symbol_495) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_495
      integer_tape_pointer = integer_tape_pointer+1
      IF(START_TIME .LE. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE,FNAME,MYNX,MYNY,F_IN)
        OpenAD_Symbol_497 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_497
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        CALL read_snap_netcdf(NCDATAFILE,START_TIME,MYNX,MYNY,FNAME,F_IN
     +)
        OpenAD_Symbol_498 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_498
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      OpenAD_Symbol_499 = 0_w2f__i8
      DO IX = 1, 20, 1
        OpenAD_Symbol_500 = 0_w2f__i8
        DO IY = 1, 20, 1
          FIELD(INT(IX), INT(IY)) = F_IN(IX, IY)
          OpenAD_Symbol_500 = (INT(OpenAD_Symbol_500) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_500
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_499 = (INT(OpenAD_Symbol_499) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_499
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_field: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_486 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_487 = 1
      DO WHILE(INT(OpenAD_Symbol_487) .LE. INT(OpenAD_Symbol_486))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_488 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_489 = 1
        DO WHILE(INT(OpenAD_Symbol_489) .LE. INT(OpenAD_Symbol_488))
          OpenAD_Symbol_489 = INT(OpenAD_Symbol_489) + 1
        END DO
        OpenAD_Symbol_487 = INT(OpenAD_Symbol_487) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_490 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_490 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_491 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_492 = 1
      DO WHILE(INT(OpenAD_Symbol_492) .LE. INT(OpenAD_Symbol_491))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_493 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_494 = 1
        DO WHILE(INT(OpenAD_Symbol_494) .LE. INT(OpenAD_Symbol_493))
          OpenAD_Symbol_494 = INT(OpenAD_Symbol_494) + 1
        END DO
        OpenAD_Symbol_492 = INT(OpenAD_Symbol_492) + 1
      END DO
      end if
      end subroutine read_field

      SUBROUTINE ini_io()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter ini_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter ini_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " ini_io: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYEARTH = 6.371D+06
      IF(FULLIO) THEN
        WRITE(*,*) 'initializing I/O'
      ENDIF
      CALL create_netcdf(FOUTNAME,RUNNAME,MYNX,MYNY)
      IF(SPHERICAL) THEN
        STR1 = 'grid_type'
        STR2 = 'spherical'
        STR3 = 'earth_radius'
        STR4 = 'Omega'
        CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
        CALL add_gattr_netcdf(FOUTNAME,STR3,MYEARTH)
        CALL add_gattr_netcdf(FOUTNAME,STR4,OM)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'grid_type'
          STR2 = 'cartesian'
          STR3 = 'f0'
          STR4 = 'beta'
          CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
          CALL add_gattr_netcdf(FOUTNAME,STR3,F0)
          CALL add_gattr_netcdf(FOUTNAME,STR4,BETA)
        ENDIF
      ENDIF
      STR1 = 'r_ini'
      STR2 = 'time_step'
      CALL add_gattr_netcdf(FOUTNAME,STR1,RINI)
      CALL add_gattr_netcdf(FOUTNAME,STR2,DT)
      IF(XPERIODIC) THEN
        STR1 = 'zonal_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
      ENDIF
      IF(YPERIODIC) THEN
        STR1 = 'meridional_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
      ENDIF
      STR1 = 'data_files'
      STR2 = NCDATAFILE // ' ' // DEPTHFILE // ' ' // FORCINGFILE // ' '
     + // UINIFILE // ' ' // VINIFILE // ' ' // ETAINIFILE
      CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
      IF(START_TIME .ne. 0.0D00) THEN
        STR1 = 'restart_file'
        CALL add_gatta_netcdf(FOUTNAME,STR1,NCRESTARTFILE)
      ENDIF
      STR1 = 'ntspinup'
      STR2 = 'wf_depth'
      STR3 = 'wf_eta'
      STR4 = 'wf_u'
      CALL add_gatti_netcdf(FOUTNAME,STR1,NTSPINUP)
      CALL add_gattr_netcdf(FOUTNAME,STR2,WF_DEPTH)
      CALL add_gattr_netcdf(FOUTNAME,STR3,WF_ETA)
      CALL add_gattr_netcdf(FOUTNAME,STR4,WF_U)
      STR1 = 'wf_v'
      STR2 = 'wf_lapldepth'
      STR3 = 'wf_graddepth'
      STR4 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_V)
      CALL add_gattr_netcdf(FOUTNAME,STR2,WF_LAPLDEPTH)
      CALL add_gattr_netcdf(FOUTNAME,STR3,WF_GRADDEPTH)
      CALL add_gattr_netcdf(FOUTNAME,STR4,WF_ZONAL_TRANSPORT)
      DO IX = 1, 20, 1
        XOUT(INT(IX)) = X(IX)
      END DO
      DO IY = 1, 20, 1
        YOUT(INT(IY)) = Y(IY)
      END DO
      IF(SPHERICAL) THEN
        STR1 = 'deg'
        CALL add_coordinates_netcdf(FOUTNAME,MYNX,XOUT,MYNY,YOUT,STR1)
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'meters'
          CALL add_coordinates_netcdf(FOUTNAME,MYNX,XOUT,MYNY,YOUT,STR1)
        ENDIF
      ENDIF
      STR1 = 'U'
      STR2 = 'zonal velocity'
      STR3 = 'meters/seconds'
      CALL add_recvar_netcdf(FOUTNAME,STR1,STR2,STR3)
      STR1 = 'V'
      STR2 = 'meridional velocity'
      CALL add_recvar_netcdf(FOUTNAME,STR1,STR2,STR3)
      STR1 = 'ETA'
      STR2 = 'sea-surface elevation'
      STR3 = 'meters'
      CALL add_recvar_netcdf(FOUTNAME,STR1,STR2,STR3)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " ini_io: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYEARTH = 6.371D+06
      IF(FULLIO) THEN
        WRITE(*,*) 'initializing I/O'
        OpenAD_Symbol_606 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_606
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_607 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_607
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      CALL create_netcdf(FOUTNAME,RUNNAME,MYNX,MYNY)
      IF(SPHERICAL) THEN
        STR1 = 'grid_type'
        STR2 = 'spherical'
        STR3 = 'earth_radius'
        STR4 = 'Omega'
        CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
        CALL add_gattr_netcdf(FOUTNAME,STR3,MYEARTH)
        CALL add_gattr_netcdf(FOUTNAME,STR4,OM)
        OpenAD_Symbol_610 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_610
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'grid_type'
          STR2 = 'cartesian'
          STR3 = 'f0'
          STR4 = 'beta'
          CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
          CALL add_gattr_netcdf(FOUTNAME,STR3,F0)
          CALL add_gattr_netcdf(FOUTNAME,STR4,BETA)
          OpenAD_Symbol_608 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_608
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_609 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_609
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_611 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_611
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      STR1 = 'r_ini'
      STR2 = 'time_step'
      CALL add_gattr_netcdf(FOUTNAME,STR1,RINI)
      CALL add_gattr_netcdf(FOUTNAME,STR2,DT)
      IF(XPERIODIC) THEN
        STR1 = 'zonal_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
        OpenAD_Symbol_612 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_612
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_613 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_613
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC) THEN
        STR1 = 'meridional_boundary_conditions'
        STR2 = 'periodic'
        CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
        OpenAD_Symbol_614 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_614
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_615 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_615
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      STR1 = 'data_files'
      STR2 = NCDATAFILE // ' ' // DEPTHFILE // ' ' // FORCINGFILE // ' '
     + // UINIFILE // ' ' // VINIFILE // ' ' // ETAINIFILE
      CALL add_gatta_netcdf(FOUTNAME,STR1,STR2)
      IF(START_TIME .ne. 0.0D00) THEN
        STR1 = 'restart_file'
        CALL add_gatta_netcdf(FOUTNAME,STR1,NCRESTARTFILE)
        OpenAD_Symbol_616 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_616
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_617 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_617
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      STR1 = 'ntspinup'
      STR2 = 'wf_depth'
      STR3 = 'wf_eta'
      STR4 = 'wf_u'
      CALL add_gatti_netcdf(FOUTNAME,STR1,NTSPINUP)
      CALL add_gattr_netcdf(FOUTNAME,STR2,WF_DEPTH)
      CALL add_gattr_netcdf(FOUTNAME,STR3,WF_ETA)
      CALL add_gattr_netcdf(FOUTNAME,STR4,WF_U)
      STR1 = 'wf_v'
      STR2 = 'wf_lapldepth'
      STR3 = 'wf_graddepth'
      STR4 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_V)
      CALL add_gattr_netcdf(FOUTNAME,STR2,WF_LAPLDEPTH)
      CALL add_gattr_netcdf(FOUTNAME,STR3,WF_GRADDEPTH)
      CALL add_gattr_netcdf(FOUTNAME,STR4,WF_ZONAL_TRANSPORT)
      OpenAD_Symbol_618 = 0_w2f__i8
      DO IX = 1, 20, 1
        XOUT(INT(IX)) = X(IX)
        OpenAD_Symbol_618 = (INT(OpenAD_Symbol_618) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_618
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_619 = 0_w2f__i8
      DO IY = 1, 20, 1
        YOUT(INT(IY)) = Y(IY)
        OpenAD_Symbol_619 = (INT(OpenAD_Symbol_619) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_619
      integer_tape_pointer = integer_tape_pointer+1
      IF(SPHERICAL) THEN
        STR1 = 'deg'
        CALL add_coordinates_netcdf(FOUTNAME,MYNX,XOUT,MYNY,YOUT,STR1)
        OpenAD_Symbol_622 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_622
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(CARTESIAN) THEN
          STR1 = 'meters'
          CALL add_coordinates_netcdf(FOUTNAME,MYNX,XOUT,MYNY,YOUT,STR1)
          OpenAD_Symbol_620 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_620
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_621 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_621
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_623 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_623
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      STR1 = 'U'
      STR2 = 'zonal velocity'
      STR3 = 'meters/seconds'
      CALL add_recvar_netcdf(FOUTNAME,STR1,STR2,STR3)
      STR1 = 'V'
      STR2 = 'meridional velocity'
      CALL add_recvar_netcdf(FOUTNAME,STR1,STR2,STR3)
      STR1 = 'ETA'
      STR2 = 'sea-surface elevation'
      STR3 = 'meters'
      CALL add_recvar_netcdf(FOUTNAME,STR1,STR2,STR3)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " ini_io: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_594 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_594 .ne. 0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_595 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_595 .ne. 0) THEN
        ENDIF
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_596 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_597 = 1
      DO WHILE(INT(OpenAD_Symbol_597) .LE. INT(OpenAD_Symbol_596))
        OpenAD_Symbol_597 = INT(OpenAD_Symbol_597) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_598 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_599 = 1
      DO WHILE(INT(OpenAD_Symbol_599) .LE. INT(OpenAD_Symbol_598))
        OpenAD_Symbol_599 = INT(OpenAD_Symbol_599) + 1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_600 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_600 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_601 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_601 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_602 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_602 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_603 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_603 .ne. 0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_604 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_604 .ne. 0) THEN
        ENDIF
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_605 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_605 .ne. 0) THEN
      ENDIF
      end if
      end subroutine ini_io

      SUBROUTINE state_io(TIME, NIO)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter state_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter state_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " state_io: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          UOUT(INT(IX),INT(IY)) = (U(IX,IY)%v+REAL(INT(UMASK(IX,IY))+(-1
     +))*9.9D+01)
          VOUT(INT(IX),INT(IY)) = (V(IX,IY)%v+REAL(INT(VMASK(IX,IY))+(-1
     +))*9.9D+01)
          ETAOUT(INT(IX),INT(IY)) = (ETA(IX,IY)%v+REAL(INT(ETAMASK(IX,IY
     +))+(-1))*9.9D+01)
        END DO
      END DO
      STR1 = 'TIME'
      STR2 = 'U'
      CALL write_time_netcdf(FOUTNAME,NIO,STR1,TIME)
      CALL write_state_netcdf(FOUTNAME,MYNX,MYNY,NIO,STR2,UOUT)
      STR1 = 'V'
      STR2 = 'ETA'
      CALL write_state_netcdf(FOUTNAME,MYNX,MYNY,NIO,STR1,VOUT)
      CALL write_state_netcdf(FOUTNAME,MYNX,MYNY,NIO,STR2,ETAOUT)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " state_io: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_658 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_659 = 0_w2f__i8
        DO IY = 1,20,1
          UOUT(INT(IX),INT(IY)) = (U(IX,IY)%v+REAL(INT(UMASK(IX,IY))+(-1
     +))*9.9D+01)
          VOUT(INT(IX),INT(IY)) = (V(IX,IY)%v+REAL(INT(VMASK(IX,IY))+(-1
     +))*9.9D+01)
          ETAOUT(INT(IX),INT(IY)) = (ETA(IX,IY)%v+REAL(INT(ETAMASK(IX,IY
     +))+(-1))*9.9D+01)
          OpenAD_Symbol_659 = (INT(OpenAD_Symbol_659)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_659
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_658 = (INT(OpenAD_Symbol_658)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_658
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'TIME'
      STR2 = 'U'
      CALL write_time_netcdf(FOUTNAME,NIO,STR1,TIME)
      CALL write_state_netcdf(FOUTNAME,MYNX,MYNY,NIO,STR2,UOUT)
      STR1 = 'V'
      STR2 = 'ETA'
      CALL write_state_netcdf(FOUTNAME,MYNX,MYNY,NIO,STR1,VOUT)
      CALL write_state_netcdf(FOUTNAME,MYNX,MYNY,NIO,STR2,ETAOUT)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " state_io: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_654 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_655 = 1
      do while (INT(OpenAD_Symbol_655).LE.INT(OpenAD_Symbol_654))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_656 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_657 = 1
        do while (INT(OpenAD_Symbol_657).LE.INT(OpenAD_Symbol_656))
          OpenAD_Symbol_657 = INT(OpenAD_Symbol_657)+1
        END DO
        OpenAD_Symbol_655 = INT(OpenAD_Symbol_655)+1
      END DO
      end if
      end subroutine state_io

      SUBROUTINE pfields_io()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter pfields_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter pfields_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " pfields_io: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      DO IX = 1, 20, 1
        DO IY = 1, 20, 1
          AUX(INT(IX),INT(IY)) = DEPTH(IX,IY)%v
        END DO
      END DO
      STR1 = 'depth'
      STR2 = 'water depth'
      STR3 = 'meters'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      DO IX = 1,20,1
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = UFORCE(IX,IY)
        END DO
      END DO
      STR1 = 'uforce'
      STR2 = 'zonal forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      DO IX = 1,20,1
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = VFORCE(IX,IY)
        END DO
      END DO
      STR1 = 'vforce'
      STR2 = 'meridional forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      DO IX = 1,20,1
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = FRICT(IX,IY)
        END DO
      END DO
      STR1 = 'frict'
      STR2 = 'linear bottom friction coefficient'
      STR3 = '1/seconds'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " pfields_io: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      OpenAD_Symbol_682 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_683 = 0_w2f__i8
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = DEPTH(IX,IY)%v
          OpenAD_Symbol_683 = (INT(OpenAD_Symbol_683)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_683
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_682 = (INT(OpenAD_Symbol_682)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_682
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'depth'
      STR2 = 'water depth'
      STR3 = 'meters'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      OpenAD_Symbol_684 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_685 = 0_w2f__i8
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = UFORCE(IX,IY)
          OpenAD_Symbol_685 = (INT(OpenAD_Symbol_685)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_685
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_684 = (INT(OpenAD_Symbol_684)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_684
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'uforce'
      STR2 = 'zonal forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      OpenAD_Symbol_686 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_687 = 0_w2f__i8
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = VFORCE(IX,IY)
          OpenAD_Symbol_687 = (INT(OpenAD_Symbol_687)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_687
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_686 = (INT(OpenAD_Symbol_686)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_686
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'vforce'
      STR2 = 'meridional forcing'
      STR3 = 'forcing units'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      OpenAD_Symbol_688 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_689 = 0_w2f__i8
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = FRICT(IX,IY)
          OpenAD_Symbol_689 = (INT(OpenAD_Symbol_689)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_689
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_688 = (INT(OpenAD_Symbol_688)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_688
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'frict'
      STR2 = 'linear bottom friction coefficient'
      STR3 = '1/seconds'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,STR1,STR2,STR3)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " pfields_io: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_666 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_667 = 1
      do while (INT(OpenAD_Symbol_667).LE.INT(OpenAD_Symbol_666))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_668 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_669 = 1
        do while (INT(OpenAD_Symbol_669).LE.INT(OpenAD_Symbol_668))
          OpenAD_Symbol_669 = INT(OpenAD_Symbol_669)+1
        END DO
        OpenAD_Symbol_667 = INT(OpenAD_Symbol_667)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_670 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_671 = 1
      do while (INT(OpenAD_Symbol_671).LE.INT(OpenAD_Symbol_670))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_672 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_673 = 1
        do while (INT(OpenAD_Symbol_673).LE.INT(OpenAD_Symbol_672))
          OpenAD_Symbol_673 = INT(OpenAD_Symbol_673)+1
        END DO
        OpenAD_Symbol_671 = INT(OpenAD_Symbol_671)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_674 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_675 = 1
      do while (INT(OpenAD_Symbol_675).LE.INT(OpenAD_Symbol_674))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_676 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_677 = 1
        do while (INT(OpenAD_Symbol_677).LE.INT(OpenAD_Symbol_676))
          OpenAD_Symbol_677 = INT(OpenAD_Symbol_677)+1
        END DO
        OpenAD_Symbol_675 = INT(OpenAD_Symbol_675)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_678 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_679 = 1
      do while (INT(OpenAD_Symbol_679).LE.INT(OpenAD_Symbol_678))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_680 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_681 = 1
        do while (INT(OpenAD_Symbol_681).LE.INT(OpenAD_Symbol_680))
          OpenAD_Symbol_681 = INT(OpenAD_Symbol_681)+1
        END DO
        OpenAD_Symbol_679 = INT(OpenAD_Symbol_679)+1
      END DO
      end if
      end subroutine pfields_io

      SUBROUTINE save_gradient_io(N, ADXC, GNAME)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter save_gradient_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter save_gradient_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_gradient_io: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL map_gradient(N,ADXC,GRAD)
      STR1 = 'gradient of cost function with respect to depth'
      STR2 = 'cost function units/m'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,GRAD,GNAME,STR1,STR2)
      SUPPRESSIO = .TRUE.
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_gradient_io: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      CALL map_gradient(N,ADXC,GRAD)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = N
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'gradient of cost function with respect to depth'
      STR2 = 'cost function units/m'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,GRAD,GNAME,STR1,STR2)
      SUPPRESSIO = .TRUE.
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_gradient_io: entering adjoint
C "
      integer_tape_pointer = integer_tape_pointer-1
      N = integer_tape(integer_tape_pointer)
      CALL map_gradient(N,ADXC,GRAD)
      end if
      end subroutine save_gradient_io

      SUBROUTINE save_depth_io(N, XC, DNAME)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: OpenAD_tyc_1(:)
      ALLOCATABLE OpenAD_tyc_1
      type(active) :: OpenAD_tyc_7(:)
      ALLOCATABLE OpenAD_tyc_7


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter save_depth_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter save_depth_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_depth_io: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
C!! requested inline of 'oad_AllocateMatching' has no defn
      CALL oad_AllocateMatching(OpenAD_tyc_1,XC)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_1,XC)
      CALL map_from_control_vector(N,OpenAD_tyc_1)
C!! requested inline of 'oad_ShapeTest' has no defn
      CALL oad_ShapeTest(OpenAD_tyc_1,XC)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(XC,OpenAD_tyc_1)
      DO IX = 1,20,1
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = DEPTH(IX,IY)%v
        END DO
      END DO
      STR1 = 'water depth after optimization'
      STR2 = 'm'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,DNAME,STR1,STR2)
      SUPPRESSIO = .TRUE.
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_depth_io: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
C!! requested inline of 'oad_AllocateMatching' has no defn
      CALL oad_AllocateMatching(OpenAD_tyc_1,XC)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_1,XC)
      CALL map_from_control_vector(N,OpenAD_tyc_1)
C!! requested inline of 'oad_ShapeTest' has no defn
      CALL oad_ShapeTest(OpenAD_tyc_1,XC)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(XC,OpenAD_tyc_1)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = N
      integer_tape_pointer = integer_tape_pointer+1
      OpenAD_Symbol_742 = 0_w2f__i8
      DO IX = 1,20,1
        OpenAD_Symbol_743 = 0_w2f__i8
        DO IY = 1,20,1
          AUX(INT(IX),INT(IY)) = DEPTH(IX,IY)%v
          OpenAD_Symbol_743 = (INT(OpenAD_Symbol_743)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_743
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_742 = (INT(OpenAD_Symbol_742)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_742
      integer_tape_pointer = integer_tape_pointer+1
      STR1 = 'water depth after optimization'
      STR2 = 'm'
      CALL add_pfield_netcdf(FOUTNAME,MYNX,MYNY,AUX,DNAME,STR1,STR2)
      SUPPRESSIO = .TRUE.
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_depth_io: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_738 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_739 = 1
      do while (INT(OpenAD_Symbol_739).LE.INT(OpenAD_Symbol_738))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_740 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_741 = 1
        do while (INT(OpenAD_Symbol_741).LE.INT(OpenAD_Symbol_740))
          OpenAD_Symbol_741 = INT(OpenAD_Symbol_741)+1
        END DO
        OpenAD_Symbol_739 = INT(OpenAD_Symbol_739)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      N = integer_tape(integer_tape_pointer)
C!! requested inline of 'oad_AllocateMatching' has no defn
      CALL oad_AllocateMatching(OpenAD_tyc_7,XC)
      CALL map_from_control_vector(N,OpenAD_tyc_7)
      end if
      end subroutine save_depth_io

      SUBROUTINE inimini_io()
      use OAD_tape
      use OAD_rev

      use OAD_active
      use w2f__types
      IMPLICIT NONE


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter inimini_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter inimini_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " inimini_io: entering plain"
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " inimini_io: entering tape"
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " inimini_io: entering adjoint"
      end if
      end subroutine inimini_io

      SUBROUTINE save_weights_io()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter save_weights_io:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter save_weights_io:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_weights_io: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      STR1 = 'wf_depth'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_DEPTH)
      STR1 = 'wf_eta'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_ETA)
      STR1 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_ZONAL_TRANSPORT)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_weights_io: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      STR1 = 'wf_depth'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_DEPTH)
      STR1 = 'wf_eta'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_ETA)
      STR1 = 'wf_zonal_transport'
      CALL add_gattr_netcdf(FOUTNAME,STR1,WF_ZONAL_TRANSPORT)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " save_weights_io: entering adjoint"
      end if
      end subroutine save_weights_io

      SUBROUTINE read_data(TIME)
      use OAD_tape
      use OAD_rev

      use OAD_active
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__8) TIME


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_data:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_data:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data: entering plain"
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data: entering tape"
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_data: entering adjoint"
      end if
      end subroutine read_data

      SUBROUTINE read_eta_data(TIME)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_eta_data:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_eta_data:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_eta_data: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYETADATA = 'etadata'
      MYETA = 'eta'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE,MYETADATA,MYNX,MYNY,F_IN)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
          END DO
        END DO
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE,TIME,MYNX,MYNY,MYETA,F_IN)
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_eta_data: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      MYNX = 20
      MYNY = 20
      MYETADATA = 'etadata'
      MYETA = 'eta'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE,MYETADATA,MYNX,MYNY,F_IN)
        OpenAD_Symbol_788 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_789 = 0_w2f__i8
          DO IX = 1, 20, 1
            ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_789 = (INT(OpenAD_Symbol_789) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_789
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_788 = (INT(OpenAD_Symbol_788) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_788
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_796 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_796
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE,TIME,MYNX,MYNY,MYETA,F_IN)
          OpenAD_Symbol_790 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_791 = 0_w2f__i8
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_791 = (INT(OpenAD_Symbol_791) + INT( 1_w2f__
     +i8))
            END DO
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_791
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_790 = (INT(OpenAD_Symbol_790) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_790
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_794 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_794
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_792 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_793 = 0_w2f__i8
            DO IX = 1, 20, 1
              ETA_DATA(INT(IX), INT(IY)) = 0.0D00
              OpenAD_Symbol_793 = (INT(OpenAD_Symbol_793) + INT( 1_w2f__
     +i8))
            END DO
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_793
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_792 = (INT(OpenAD_Symbol_792) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_792
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_795 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_795
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_797 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_797
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_eta_data: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_774 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_774 .ne. 0) THEN
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_784 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_785 = 1
        DO WHILE(INT(OpenAD_Symbol_785) .LE. INT(OpenAD_Symbol_784))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_786 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_787 = 1
          DO WHILE(INT(OpenAD_Symbol_787) .LE. INT(OpenAD_Symbol_786))
            OpenAD_Symbol_787 = INT(OpenAD_Symbol_787) + 1
          END DO
          OpenAD_Symbol_785 = INT(OpenAD_Symbol_785) + 1
        END DO
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_775 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_775 .ne. 0) THEN
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_780 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_781 = 1
          DO WHILE(INT(OpenAD_Symbol_781) .LE. INT(OpenAD_Symbol_780))
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_782 = integer_tape(integer_tape_pointer)
            OpenAD_Symbol_783 = 1
            DO WHILE(INT(OpenAD_Symbol_783) .LE. INT(OpenAD_Symbol_782) 
     +)
              OpenAD_Symbol_783 = INT(OpenAD_Symbol_783) + 1
            END DO
            OpenAD_Symbol_781 = INT(OpenAD_Symbol_781) + 1
          END DO
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_776 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_777 = 1
          DO WHILE(INT(OpenAD_Symbol_777) .LE. INT(OpenAD_Symbol_776))
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_778 = integer_tape(integer_tape_pointer)
            OpenAD_Symbol_779 = 1
            DO WHILE(INT(OpenAD_Symbol_779) .LE. INT(OpenAD_Symbol_778) 
     +)
              OpenAD_Symbol_779 = INT(OpenAD_Symbol_779) + 1
            END DO
            OpenAD_Symbol_777 = INT(OpenAD_Symbol_777) + 1
          END DO
        ENDIF
      ENDIF
      end if
      end subroutine read_eta_data

      SUBROUTINE read_uv_data(TIME)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_uv_data:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_uv_data:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_uv_data: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRUDATA = 'udata'
      STRVDATA = 'vdata'
      STRU = 'U'
      STRV = 'V'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE,STRUDATA,MYNX,MYNY,F_IN)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
          END DO
        END DO
        CALL read_field_netcdf(NCDATAFILE,STRVDATA,MYNX,MYNY,F_IN)
        DO IY = 1, 20, 1
          DO IX = 1, 20, 1
            V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
          END DO
        END DO
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE,TIME,MYNX,MYNY,STRU,F_IN)
          DO IY = 1, 20, 1
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            END DO
          END DO
          CALL read_snap_netcdf(NCDATAFILE,TIME,MYNX,MYNY,STRV,F_IN)
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_uv_data: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRUDATA = 'udata'
      STRVDATA = 'vdata'
      STRU = 'U'
      STRV = 'V'
      IF(TIME .eq. 0.0D00) THEN
        CALL read_field_netcdf(NCDATAFILE,STRUDATA,MYNX,MYNY,F_IN)
        OpenAD_Symbol_844 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_845 = 0_w2f__i8
          DO IX = 1, 20, 1
            U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_845 = (INT(OpenAD_Symbol_845) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_845
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_844 = (INT(OpenAD_Symbol_844) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_844
        integer_tape_pointer = integer_tape_pointer+1
        CALL read_field_netcdf(NCDATAFILE,STRVDATA,MYNX,MYNY,F_IN)
        OpenAD_Symbol_846 = 0_w2f__i8
        DO IY = 1, 20, 1
          OpenAD_Symbol_847 = 0_w2f__i8
          DO IX = 1, 20, 1
            V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
            OpenAD_Symbol_847 = (INT(OpenAD_Symbol_847) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_847
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_846 = (INT(OpenAD_Symbol_846) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_846
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_856 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_856
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(TIME .GT. 0.0D00) THEN
          CALL read_snap_netcdf(NCDATAFILE,TIME,MYNX,MYNY,STRU,F_IN)
          OpenAD_Symbol_848 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_849 = 0_w2f__i8
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_849 = (INT(OpenAD_Symbol_849) + INT( 1_w2f__
     +i8))
            END DO
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_849
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_848 = (INT(OpenAD_Symbol_848) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_848
          integer_tape_pointer = integer_tape_pointer+1
          CALL read_snap_netcdf(NCDATAFILE,TIME,MYNX,MYNY,STRV,F_IN)
          OpenAD_Symbol_850 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_851 = 0_w2f__i8
            DO IX = 1, 20, 1
              V_DATA(INT(IX), INT(IY)) = F_IN(IX, IY)
              OpenAD_Symbol_851 = (INT(OpenAD_Symbol_851) + INT( 1_w2f__
     +i8))
            END DO
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_851
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_850 = (INT(OpenAD_Symbol_850) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_850
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_854 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_854
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_852 = 0_w2f__i8
          DO IY = 1, 20, 1
            OpenAD_Symbol_853 = 0_w2f__i8
            DO IX = 1, 20, 1
              U_DATA(INT(IX), INT(IY)) = 0.0D00
              V_DATA(INT(IX), INT(IY)) = 0.0D00
              OpenAD_Symbol_853 = (INT(OpenAD_Symbol_853) + INT( 1_w2f__
     +i8))
            END DO
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_853
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_852 = (INT(OpenAD_Symbol_852) + INT(1_w2f__i8 
     +))
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_852
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_855 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_855
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_857 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_857
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_uv_data: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_822 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_822 .ne. 0) THEN
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_836 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_837 = 1
        DO WHILE(INT(OpenAD_Symbol_837) .LE. INT(OpenAD_Symbol_836))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_838 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_839 = 1
          DO WHILE(INT(OpenAD_Symbol_839) .LE. INT(OpenAD_Symbol_838))
            OpenAD_Symbol_839 = INT(OpenAD_Symbol_839) + 1
          END DO
          OpenAD_Symbol_837 = INT(OpenAD_Symbol_837) + 1
        END DO
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_840 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_841 = 1
        DO WHILE(INT(OpenAD_Symbol_841) .LE. INT(OpenAD_Symbol_840))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_842 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_843 = 1
          DO WHILE(INT(OpenAD_Symbol_843) .LE. INT(OpenAD_Symbol_842))
            OpenAD_Symbol_843 = INT(OpenAD_Symbol_843) + 1
          END DO
          OpenAD_Symbol_841 = INT(OpenAD_Symbol_841) + 1
        END DO
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_823 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_823 .ne. 0) THEN
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_828 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_829 = 1
          DO WHILE(INT(OpenAD_Symbol_829) .LE. INT(OpenAD_Symbol_828))
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_830 = integer_tape(integer_tape_pointer)
            OpenAD_Symbol_831 = 1
            DO WHILE(INT(OpenAD_Symbol_831) .LE. INT(OpenAD_Symbol_830) 
     +)
              OpenAD_Symbol_831 = INT(OpenAD_Symbol_831) + 1
            END DO
            OpenAD_Symbol_829 = INT(OpenAD_Symbol_829) + 1
          END DO
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_832 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_833 = 1
          DO WHILE(INT(OpenAD_Symbol_833) .LE. INT(OpenAD_Symbol_832))
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_834 = integer_tape(integer_tape_pointer)
            OpenAD_Symbol_835 = 1
            DO WHILE(INT(OpenAD_Symbol_835) .LE. INT(OpenAD_Symbol_834) 
     +)
              OpenAD_Symbol_835 = INT(OpenAD_Symbol_835) + 1
            END DO
            OpenAD_Symbol_833 = INT(OpenAD_Symbol_833) + 1
          END DO
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_824 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_825 = 1
          DO WHILE(INT(OpenAD_Symbol_825) .LE. INT(OpenAD_Symbol_824))
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_826 = integer_tape(integer_tape_pointer)
            OpenAD_Symbol_827 = 1
            DO WHILE(INT(OpenAD_Symbol_827) .LE. INT(OpenAD_Symbol_826) 
     +)
              OpenAD_Symbol_827 = INT(OpenAD_Symbol_827) + 1
            END DO
            OpenAD_Symbol_825 = INT(OpenAD_Symbol_825) + 1
          END DO
        ENDIF
      ENDIF
      end if
      end subroutine read_uv_data

      SUBROUTINE read_zonal_transport_data(TIME)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_zonal_transport_data:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_zonal_transport_dat
C a:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_zonal_transport_data: enterin
C g plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(TIME .eq. 0.0D00) THEN
        ZONAL_TRANSPORT_DATA = 5.26265D+07
      ELSE
        ZONAL_TRANSPORT = 0.0D00
        DO IY = 1, 20, 1
          ZONAL_TRANSPORT = (ZONAL_TRANSPORT+HU(6,IY)%v*DY(IY)*U(6,IY)%v
     +)
        END DO
        ZONAL_TRANSPORT_DATA = ZONAL_TRANSPORT
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_zonal_transport_data: enterin
C g tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF (TIME.eq.0.0D00) THEN
        ZONAL_TRANSPORT_DATA = 5.26265D+07
        OpenAD_Symbol_898 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_898
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        ZONAL_TRANSPORT = 0.0D00
        OpenAD_Symbol_897 = 0_w2f__i8
        DO IY = 1,20,1
          ZONAL_TRANSPORT = (ZONAL_TRANSPORT+HU(6,IY)%v*DY(IY)*U(6,IY)%v
     +)
          OpenAD_Symbol_897 = (INT(OpenAD_Symbol_897)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_897
        integer_tape_pointer = integer_tape_pointer+1
        ZONAL_TRANSPORT_DATA = ZONAL_TRANSPORT
        OpenAD_Symbol_899 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_899
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_zonal_transport_data: enterin
C g adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_894 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_894.ne.0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_895 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_896 = 1
        do while (INT(OpenAD_Symbol_896).LE.INT(OpenAD_Symbol_895))
          OpenAD_Symbol_896 = INT(OpenAD_Symbol_896)+1
        END DO
      ENDIF
      end if
      end subroutine read_zonal_transport_data

      SUBROUTINE read_depth_data()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter read_depth_data:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter read_depth_data:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_depth_data: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          DEPTH_DATA(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_depth_data: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      OpenAD_Symbol_544 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_545 = 0_w2f__i8
        DO IX = 1, 20, 1
          DEPTH_DATA(INT(IX), INT(IY)) = INIDEPTH(IX, IY)
          OpenAD_Symbol_545 = (INT(OpenAD_Symbol_545) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_545
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_544 = (INT(OpenAD_Symbol_544) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_544
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " read_depth_data: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_540 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_541 = 1
      DO WHILE(INT(OpenAD_Symbol_541) .LE. INT(OpenAD_Symbol_540))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_542 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_543 = 1
        DO WHILE(INT(OpenAD_Symbol_543) .LE. INT(OpenAD_Symbol_542))
          OpenAD_Symbol_543 = INT(OpenAD_Symbol_543) + 1
        END DO
        OpenAD_Symbol_541 = INT(OpenAD_Symbol_541) + 1
      END DO
      end if
      end subroutine read_depth_data

      SUBROUTINE determine_data_time(NCDATAFILE)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter determine_data_time:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter determine_data_time:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " determine_data_time: entering plai
C n"
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRTIME = 'TIME'
      CALL get_length_netcdf(NCDATAFILE,STRTIME,NEDT)
      IF(NEDT .GT. 1000) THEN
        WRITE(*,*) 'determine_data_time: too many data,;'
        WRITE(*,*) 'increase nedtmax to ',NEDT
      ELSE
        IF(NEDT .LE. 0) THEN
          WRITE(*,*) 'no time dependent data found in '//NCDATAFILE
        ELSE
          CALL read_vector_netcdf(NCDATAFILE,STRTIME,NEDT,ETA_DATA_TIME)
          WRITE(*,*) 'determine_data_time: # of data times = ',NEDT
        ENDIF
      ENDIF
      WRITE(*,*) (ETA_DATA_TIME(K),K=1,NEDT,1)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " determine_data_time: entering tape
C "
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRTIME = 'TIME'
      CALL get_length_netcdf(NCDATAFILE,STRTIME,NEDT)
      IF(NEDT .GT. 1000) THEN
        WRITE(*,*) 'determine_data_time: too many data,;'
        WRITE(*,*) 'increase nedtmax to ',NEDT
        OpenAD_Symbol_436 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_436
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        IF(NEDT .LE. 0) THEN
          WRITE(*,*) 'no time dependent data found in '//NCDATAFILE
          OpenAD_Symbol_434 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_434
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          CALL read_vector_netcdf(NCDATAFILE,STRTIME,NEDT,ETA_DATA_TIME)
          WRITE(*,*) 'determine_data_time: # of data times = ',NEDT
          OpenAD_Symbol_435 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_435
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_437 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_437
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      WRITE(*,*) (ETA_DATA_TIME(K),K=1,NEDT,1)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " determine_data_time: entering adjo
C int"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_432 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_432 .ne. 0) THEN
      ELSE
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_433 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_433 .ne. 0) THEN
        ENDIF
      ENDIF
      end if
      end subroutine determine_data_time

      SUBROUTINE is_eta_data_time(TIME, RESULT)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter is_eta_data_time:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter is_eta_data_time:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " is_eta_data_time: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ALLDONE = .FALSE.
      RESULT = .FALSE.
      IT = 1
      DO WHILE(.NOT. ALLDONE)
        IF(ABS(ETA_DATA_TIME(IT) - TIME) .LT. 1.00000000000000002092D-08
     +) THEN
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " is_eta_data_time: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ALLDONE = .FALSE.
      RESULT = .FALSE.
      IT = 1
      OpenAD_Symbol_910 = 0_w2f__i8
      DO WHILE(.NOT. ALLDONE)
        IF(ABS(ETA_DATA_TIME(IT) - TIME) .LT. 1.00000000000000002092D-08
     +) THEN
          RESULT = .TRUE.
          ALLDONE = .TRUE.
          OpenAD_Symbol_913 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_913
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          IF(IT .LT. NEDT) THEN
            IT = (IT + 1)
            OpenAD_Symbol_911 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_911
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            ALLDONE = .TRUE.
            OpenAD_Symbol_912 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_912
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_914 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_914
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_910 = (INT(OpenAD_Symbol_910) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_910
      integer_tape_pointer = integer_tape_pointer+1
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " is_eta_data_time: entering adjoint
C "
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_906 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_907 = 1
      DO WHILE(INT(OpenAD_Symbol_907) .LE. INT(OpenAD_Symbol_906))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_908 = integer_tape(integer_tape_pointer)
        IF(OpenAD_Symbol_908 .ne. 0) THEN
        ELSE
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_909 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_909 .ne. 0) THEN
          ENDIF
        ENDIF
        OpenAD_Symbol_907 = INT(OpenAD_Symbol_907) + 1
      END DO
      end if
      end subroutine is_eta_data_time

      SUBROUTINE make_weights()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL make_weights_eta()
      CALL make_weights_uv()
      CALL make_weights_zonal_transport()
      CALL make_weights_lapldepth()
      CALL make_weights_graddepth()
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL make_weights_eta()
      CALL make_weights_uv()
      CALL make_weights_zonal_transport()
      CALL make_weights_lapldepth()
      CALL make_weights_graddepth()
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights: entering adjoint"
      CALL make_weights_graddepth()
      CALL make_weights_lapldepth()
      CALL make_weights_zonal_transport()
      CALL make_weights_uv()
      CALL make_weights_eta()
      end if
      end subroutine make_weights

      SUBROUTINE make_weights_depth()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights_depth:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights_depth:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_depth: entering plain
C "
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRWD = 'weights_depth.nc'
      STRW = 'W'
      NFLAG = 0
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_depth: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      STRWD = 'weights_depth.nc'
      STRW = 'W'
      NFLAG = 0
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_depth: entering adjoi
C nt"
      end if
      end subroutine make_weights_depth

      SUBROUTINE make_weights_eta()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights_eta:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights_eta:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_eta: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ETA = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_ETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_ETA *(( 1
     +.0D00 / ERROR_ETA) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_eta: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ETA = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_ETA(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_ETA *(( 1
     +.0D00 / ERROR_ETA) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_eta: entering adjoint
C "
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      end if
      end subroutine make_weights_eta

      SUBROUTINE make_weights_uv()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights_uv:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights_uv:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_uv: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_U = 1.0D00
      ERROR_V = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_U(INT(IX), INT(IY)) = (UMASK(IX, IY) * WF_U *((1.0D00 /
     + ERROR_U) ** 2))
          WEIGHT_V(INT(IX), INT(IY)) = (VMASK(IX, IY) * WF_V *((1.0D00 /
     + ERROR_V) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_uv: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_U = 1.0D00
      ERROR_V = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_U(INT(IX), INT(IY)) = (UMASK(IX, IY) * WF_U *((1.0D00 /
     + ERROR_U) ** 2))
          WEIGHT_V(INT(IX), INT(IY)) = (VMASK(IX, IY) * WF_V *((1.0D00 /
     + ERROR_V) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_uv: entering adjoint"
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      end if
      end subroutine make_weights_uv

      SUBROUTINE make_weights_zonal_transport()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights_zonal_transport:
C ",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights_zonal_trans
C port:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_zonal_transport: ente
C ring plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ZONAL_TRANSPORT = 1.0D+06
      WEIGHT_ZONAL_TRANSPORT = (WF_ZONAL_TRANSPORT *((1.0D00 / ERROR_ZON
     +AL_TRANSPORT) ** 2))
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_zonal_transport: ente
C ring tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR_ZONAL_TRANSPORT = 1.0D+06
      WEIGHT_ZONAL_TRANSPORT = (WF_ZONAL_TRANSPORT *((1.0D00 / ERROR_ZON
     +AL_TRANSPORT) ** 2))
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_zonal_transport: ente
C ring adjoint"
      end if
      end subroutine make_weights_zonal_transport

      SUBROUTINE make_bounds_for_x(NC, LOWER_BOUND, UPPER_BOUND)
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_bounds_for_x:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_bounds_for_x:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_bounds_for_x: entering plain"
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
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_bounds_for_x: entering tape"
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
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_bounds_for_x: entering adjoin
C t"
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
      end if
      end subroutine make_bounds_for_x

      SUBROUTINE make_weights_lapldepth()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights_lapldepth:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights_lapldepth:"
C ,
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_lapldepth: entering p
C lain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_LAP
     +LDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_lapldepth: entering t
C ape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_LAPLDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_LAP
     +LDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_lapldepth: entering a
C djoint"
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      end if
      end subroutine make_weights_lapldepth

      SUBROUTINE make_weights_graddepth()
      use OAD_tape
      use OAD_rev

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter make_weights_graddepth:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter make_weights_graddepth:"
C ,
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_graddepth: entering p
C lain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_GRA
     +DDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_graddepth: entering t
C ape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ERROR = 1.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          WEIGHT_GRADDEPTH(INT(IX), INT(IY)) = (ETAMASK(IX, IY) * WF_GRA
     +DDEPTH *((1.0D00 / ERROR) ** 2))
        END DO
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " make_weights_graddepth: entering a
C djoint"
      IY = 1 + 1 *((20 - 1) / 1)
      DO WHILE(IY .GE. 1)
        IX = 1 + 1 *((20 - 1) / 1)
        DO WHILE(IX .GE. 1)
          IX = IX - 1
        END DO
        IY = IY - 1
      END DO
      end if
      end subroutine make_weights_graddepth

      SUBROUTINE map_from_control_vector(N, XC)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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
      type(active) :: XC(1:N)
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter ma
     +p_from_control_vector:",our_rev_mode%arg_store,our_rev_mode%arg_re
     +store,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode
     +%plain,our_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter map_from_control_vector:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," map_from_control_v
     +ector: entering arg store"
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = N
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(SCALEDEPTH,1),ubound(SCALEDEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(SCALEDEPTH,2)
     +-lbound(SCALEDEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(SCALEDEPTH,2),ubound(SCALEDEPTH,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = SCALEDEPTH(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(XC,1)-lbound(
     +XC,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(XC,1),ubound(XC,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = XC(cp_loop_variable_1)%v
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," map_from_control_v
     +ector: entering arg restore"
      do cp_loop_variable_1 = ubound(XC,1),lbound(XC,1),-1
      XC(cp_loop_variable_1)%v = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +XC(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(SCALEDEPTH,1),lbound(SCALEDEPTH,1),
     +-1
      do cp_loop_variable_2 = ubound(SCALEDEPTH,2),lbound(SCALEDEPTH,2),
     +-1
      SCALEDEPTH(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      N = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," map_from_control_v
     +ector: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            DEPTH(INT(IX),INT(IY))%v = (SCALEDEPTH(IX,IY)*(XC(K)%v+1.0D0
     +0))
          ENDIF
        END DO
      END DO
      IF (N.ne.K) THEN
        WRITE(*,*) 'map_from_control_vector: ','dimensions of control ve
     +ctor are wrong'
        WRITE(*,*) K,' should be ',N
      ENDIF
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," map_from_control_v
     +ector: entering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_756 = 0_w2f__i8
      DO IY = 1,20,1
        OpenAD_Symbol_757 = 0_w2f__i8
        DO IX = 1,20,1
          IF (ETAMASK(IX,IY).ne.0.0D00) THEN
            K = (K+1)
            OpenAD_aux_0 = (XC(K)%v+1.0D00)
            OpenAD_lin_0 = SCALEDEPTH(IX,IY)
            DEPTH(INT(IX),INT(IY))%v = (SCALEDEPTH(IX,IY)*OpenAD_aux_0)
            if (double_tape_size.lt.double_tape_pointer) then
            allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_vari
     +able_1)
            if (cp_loop_variable_1.gt.0) then
            print *,'allocation failed with',cp_loop_variable_1
            stop
            end if
            double_tmp_tape = double_tape
            deallocate(double_tape)
            allocate(double_tape(double_tape_size*2))
            print *,"DT+"
            double_tape(1:double_tape_size) = double_tmp_tape
            deallocate(double_tmp_tape)
            double_tape_size = double_tape_size*2
            end if
            double_tape(double_tape_pointer) = OpenAD_lin_0
            double_tape_pointer = double_tape_pointer+1
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = K
            integer_tape_pointer = integer_tape_pointer+1
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = IX
            integer_tape_pointer = integer_tape_pointer+1
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = IY
            integer_tape_pointer = integer_tape_pointer+1
            OpenAD_Symbol_758 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_758
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_759 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_759
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_757 = (INT(OpenAD_Symbol_757)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_757
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_756 = (INT(OpenAD_Symbol_756)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_756
      integer_tape_pointer = integer_tape_pointer+1
      IF (N.ne.K) THEN
        WRITE(*,*) 'map_from_control_vector: ','dimensions of control ve
     +ctor are wrong'
        WRITE(*,*) K,' should be ',N
        OpenAD_Symbol_760 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_760
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_761 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_761
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," map_from_control_v
     +ector: entering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_750 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_750.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_751 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_752 = 1
      do while (INT(OpenAD_Symbol_752).LE.INT(OpenAD_Symbol_751))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_753 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_754 = 1
        do while (INT(OpenAD_Symbol_754).LE.INT(OpenAD_Symbol_753))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_755 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_755.ne.0) THEN
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_1314 = integer_tape(integer_tape_pointer)
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_1315 = integer_tape(integer_tape_pointer)
            integer_tape_pointer = integer_tape_pointer-1
            OpenAD_Symbol_1316 = integer_tape(integer_tape_pointer)
            double_tape_pointer = double_tape_pointer-1
            OpenAD_Symbol_1317 = double_tape(double_tape_pointer)
            XC(OpenAD_Symbol_1316)%d = XC(OpenAD_Symbol_1316)%d+DEPTH(Op
     +enAD_Symbol_1315,OpenAD_Symbol_1314)%d*(OpenAD_Symbol_1317)
            DEPTH(OpenAD_Symbol_1315,OpenAD_Symbol_1314)%d = 0.0d0
          ENDIF
          OpenAD_Symbol_754 = INT(OpenAD_Symbol_754)+1
        END DO
        OpenAD_Symbol_752 = INT(OpenAD_Symbol_752)+1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave map_from_control_vector:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave ma
     +p_from_control_vector:",our_rev_mode%arg_store,our_rev_mode%arg_re
     +store,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode
     +%plain,our_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine map_from_control_vector

      SUBROUTINE map_to_control_vector(N, XC)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter ma
     +p_to_control_vector:",our_rev_mode%arg_store,our_rev_mode%arg_rest
     +ore,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%p
     +lain,our_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter map_to_control_vector:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," map_to_control_vec
     +tor: entering arg store"
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(SCALEDEPTH,1),ubound(SCALEDEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(SCALEDEPTH,2)
     +-lbound(SCALEDEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(SCALEDEPTH,2),ubound(SCALEDEPTH,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = SCALEDEPTH(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," map_to_control_vec
     +tor: entering arg restore"
      do cp_loop_variable_1 = ubound(SCALEDEPTH,1),lbound(SCALEDEPTH,1),
     +-1
      do cp_loop_variable_2 = ubound(SCALEDEPTH,2),lbound(SCALEDEPTH,2),
     +-1
      SCALEDEPTH(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," map_to_control_vec
     +tor: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            XC(INT(K)) = ((DEPTH(IX,IY)%v/SCALEDEPTH(IX,IY))+(-1.0D00))
          ENDIF
        END DO
      END DO
      IF (N.ne.K) THEN
        WRITE(*,*) 'map_to_control_vector: ','dimensions of control vect
     +or are wrong'
        WRITE(*,*) K,' should be ',N
      ENDIF
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," map_to_control_vec
     +tor: entering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_963 = 0_w2f__i8
      DO IY = 1,20,1
        OpenAD_Symbol_964 = 0_w2f__i8
        DO IX = 1,20,1
          IF (ETAMASK(IX,IY).ne.0.0D00) THEN
            K = (K+1)
            XC(INT(K)) = ((DEPTH(IX,IY)%v/SCALEDEPTH(IX,IY))+(-1.0D00))
            OpenAD_Symbol_965 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_965
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_966 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_966
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_964 = (INT(OpenAD_Symbol_964)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_964
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_963 = (INT(OpenAD_Symbol_963)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_963
      integer_tape_pointer = integer_tape_pointer+1
      IF (N.ne.K) THEN
        WRITE(*,*) 'map_to_control_vector: ','dimensions of control vect
     +or are wrong'
        WRITE(*,*) K,' should be ',N
        OpenAD_Symbol_967 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_967
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_968 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_968
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," map_to_control_vec
     +tor: entering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_957 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_957.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_958 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_959 = 1
      do while (INT(OpenAD_Symbol_959).LE.INT(OpenAD_Symbol_958))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_960 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_961 = 1
        do while (INT(OpenAD_Symbol_961).LE.INT(OpenAD_Symbol_960))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_962 = integer_tape(integer_tape_pointer)
          IF (OpenAD_Symbol_962.ne.0) THEN
          ENDIF
          OpenAD_Symbol_961 = INT(OpenAD_Symbol_961)+1
        END DO
        OpenAD_Symbol_959 = INT(OpenAD_Symbol_959)+1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave map_to_control_vector:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave ma
     +p_to_control_vector:",our_rev_mode%arg_store,our_rev_mode%arg_rest
     +ore,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%p
     +lain,our_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine map_to_control_vector

      SUBROUTINE map_gradient(N, ADXC, GRAD)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter ma
     +p_gradient:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_r
     +ev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our_
     +rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter map_gradient:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," map_gradient: ente
     +ring arg store"
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = N
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(SCALEDEPTH,1),ubound(SCALEDEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(SCALEDEPTH,2)
     +-lbound(SCALEDEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(SCALEDEPTH,2),ubound(SCALEDEPTH,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = SCALEDEPTH(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(ADXC,1)-lboun
     +d(ADXC,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(ADXC,1),ubound(ADXC,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ADXC(cp_loop_variable_1)
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," map_gradient: ente
     +ring arg restore"
      do cp_loop_variable_1 = ubound(ADXC,1),lbound(ADXC,1),-1
      ADXC(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +ADXC(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(SCALEDEPTH,1),lbound(SCALEDEPTH,1),
     +-1
      do cp_loop_variable_2 = ubound(SCALEDEPTH,2),lbound(SCALEDEPTH,2),
     +-1
      SCALEDEPTH(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      N = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," map_gradient: ente
     +ring plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
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
        WRITE(*,*) 'map_from_control_vector: ','dimensions of control ve
     +ctor are wrong'
        WRITE(*,*) K,' should be ',N
      ENDIF
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," map_gradient: ente
     +ring tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
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
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_722
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_723 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_723
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_721 = (INT(OpenAD_Symbol_721) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_721
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_720 = (INT(OpenAD_Symbol_720) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_720
      integer_tape_pointer = integer_tape_pointer+1
      IF(N .ne. K) THEN
        WRITE(*,*) 'map_from_control_vector: ','dimensions of control ve
     +ctor are wrong'
        WRITE(*,*) K,' should be ',N
        OpenAD_Symbol_724 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_724
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_725 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_725
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," map_gradient: ente
     +ring adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_714 = integer_tape(integer_tape_pointer)
      IF(OpenAD_Symbol_714 .ne. 0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_715 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_716 = 1
      DO WHILE(INT(OpenAD_Symbol_716) .LE. INT(OpenAD_Symbol_715))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_717 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_718 = 1
        DO WHILE(INT(OpenAD_Symbol_718) .LE. INT(OpenAD_Symbol_717))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_719 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_719 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_718 = INT(OpenAD_Symbol_718) + 1
        END DO
        OpenAD_Symbol_716 = INT(OpenAD_Symbol_716) + 1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave map_gradient:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave ma
     +p_gradient:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_r
     +ev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our_
     +rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine map_gradient

      SUBROUTINE length_of_control_vector(N)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter le
     +ngth_of_control_vector:",our_rev_mode%arg_store,our_rev_mode%arg_r
     +estore,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mod
     +e%plain,our_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter length_of_control_vector:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," length_of_control_
     +vector: entering arg store"
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," length_of_control_
     +vector: entering arg restore"
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," length_of_control_
     +vector: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
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
      WRITE(*,*) 'dimensions of control vector = ',N
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," length_of_control_
     +vector: entering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
      K = 0
      OpenAD_Symbol_986 = 0_w2f__i8
      DO IY = 1, 20, 1
        OpenAD_Symbol_987 = 0_w2f__i8
        DO IX = 1, 20, 1
          IF(ETAMASK(IX, IY) .ne. 0.0D00) THEN
            K = (K + 1)
            OpenAD_Symbol_988 = 1_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_988
            integer_tape_pointer = integer_tape_pointer+1
          ELSE
            OpenAD_Symbol_989 = 0_w2f__i8
            if (integer_tape_size.lt.integer_tape_pointer) then
            allocate(integer_tmp_tape(integer_tape_size))
            integer_tmp_tape = integer_tape
            deallocate(integer_tape)
            allocate(integer_tape(integer_tape_size*2))
            print *,"IT+"
            integer_tape(1:integer_tape_size) = integer_tmp_tape
            deallocate(integer_tmp_tape)
            integer_tape_size = integer_tape_size*2
            end if
            integer_tape(integer_tape_pointer) = OpenAD_Symbol_989
            integer_tape_pointer = integer_tape_pointer+1
          ENDIF
          OpenAD_Symbol_987 = (INT(OpenAD_Symbol_987) + INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_987
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_986 = (INT(OpenAD_Symbol_986) + INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_986
      integer_tape_pointer = integer_tape_pointer+1
      N = K
      WRITE(*,*) 'dimensions of control vector = ',N
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," length_of_control_
     +vector: entering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_981 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_982 = 1
      DO WHILE(INT(OpenAD_Symbol_982) .LE. INT(OpenAD_Symbol_981))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_983 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_984 = 1
        DO WHILE(INT(OpenAD_Symbol_984) .LE. INT(OpenAD_Symbol_983))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_985 = integer_tape(integer_tape_pointer)
          IF(OpenAD_Symbol_985 .ne. 0) THEN
          ENDIF
          OpenAD_Symbol_984 = INT(OpenAD_Symbol_984) + 1
        END DO
        OpenAD_Symbol_982 = INT(OpenAD_Symbol_982) + 1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave length_of_control_vector:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave le
     +ngth_of_control_vector:",our_rev_mode%arg_store,our_rev_mode%arg_r
     +estore,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mod
     +e%plain,our_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine length_of_control_vector

      SUBROUTINE time_step(IT, LOCAL_U, LOCAL_V, LOCAL_ETA)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      type(active) :: LOCAL_V(0:21,0:21)
      type(active) :: LOCAL_ETA(0:21,0:21)
C
C     **** Local Variables and Functions ****
C
      EXTERNAL continuity
      EXTERNAL umomentum
      EXTERNAL vmomentum


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter time_step:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter time_step:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " time_step: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF(MOD(IT, 2) .ne. 0) THEN
        CALL umomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        CALL vmomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
      ELSE
        CALL vmomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        CALL umomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
      ENDIF
      CALL continuity(LOCAL_U,LOCAL_V,LOCAL_ETA)
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " time_step: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      IF (MOD(IT,2).ne.0) THEN
        CALL umomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        CALL vmomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        OpenAD_Symbol_1000 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1000
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        CALL vmomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        CALL umomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        OpenAD_Symbol_1001 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1001
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      CALL continuity(LOCAL_U,LOCAL_V,LOCAL_ETA)
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " time_step: entering adjoint"
      CALL continuity(LOCAL_U,LOCAL_V,LOCAL_ETA)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_999 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_999.ne.0) THEN
        CALL vmomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        CALL umomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
      ELSE
        CALL umomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
        CALL vmomentum(LOCAL_U,LOCAL_V,LOCAL_ETA)
      ENDIF
      end if
      end subroutine time_step

      SUBROUTINE umomentum(LOCAL_U, LOCAL_V, LOCAL_ETA)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      type(active) :: LOCAL_V(0:21,0:21)
      type(active) :: LOCAL_ETA(0:21,0:21)
C
C     **** Local Variables and Functions ****
C
      type(active) :: FRICTU
      type(active) :: FV
      type(active) :: GRADETAU
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
      type(active) :: OpenAD_prp_0
      type(active) :: OpenAD_prp_1
      type(active) :: OpenAD_prp_2
      type(active) :: OpenAD_prp_3
      type(active) :: OpenAD_prp_4


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter umomentum:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter umomentum:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " umomentum: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          FRICTU%v = (INVHU(IX,IY)%v*(FRICT(IX,IY)+FRICT(IX+(-1),IY))*5.
     +0D-01)
          GRADETAU%v = ((LOCAL_ETA(IX,IY)%v-LOCAL_ETA(IX+(-1),IY)%v)/(RX
     +(IY)*5.0D-01*(DX(IX)+DX(IX+(-1)))))
          FV%v = (FCORIU(IX,IY)*(LOCAL_V(IX+(-1),IY+1)%v+LOCAL_V(IX,IY+1
     +)%v+LOCAL_V(IX,IY)%v+LOCAL_V(IX+(-1),IY)%v))
          LOCAL_U(INT(IX),INT(IY))%v = ((UMASK(IX,IY)/(DT*FRICTU%v*5.0D-
     +01+1.0D00))*(FV%v*DT+LOCAL_U(IX,IY)%v*(1.0D00-DT*FRICTU%v*5.0D-01)
     +-GRADETAU%v*DT*9.81000000000000049738D00+INVHU(IX,IY)%v*UFORCE(IX,
     +IY)*DT))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          LOCAL_U(21,INT(IY))%v = LOCAL_U(1,IY)%v
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          LOCAL_U(INT(IX),0)%v = LOCAL_U(IX,20)%v
        END DO
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " umomentum: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          OpenAD_aux_1 = ((FRICT(IX,IY)+FRICT(IX+(-1),IY))*5.0D-01)
          OpenAD_lin_1 = OpenAD_aux_1
          FRICTU%v = (INVHU(IX,IY)%v*OpenAD_aux_1)
          OpenAD_aux_2 = (LOCAL_ETA(IX,IY)%v-LOCAL_ETA(IX+(-1),IY)%v)
          OpenAD_aux_3 = (RX(IY)*5.0D-01*(DX(IX)+DX(IX+(-1))))
          OpenAD_lin_2 = (INT(1_w2f__i8)/OpenAD_aux_3)
          GRADETAU%v = (OpenAD_aux_2/OpenAD_aux_3)
          OpenAD_aux_4 = (LOCAL_V(IX+(-1),IY+1)%v+LOCAL_V(IX,IY+1)%v+LOC
     +AL_V(IX,IY)%v+LOCAL_V(IX+(-1),IY)%v)
          OpenAD_lin_3 = FCORIU(IX,IY)
          FV%v = (FCORIU(IX,IY)*OpenAD_aux_4)
          OpenAD_aux_8 = (FRICTU%v*5.0D-01)
          OpenAD_aux_7 = (DT*OpenAD_aux_8+1.0D00)
          OpenAD_aux_5 = (UMASK(IX,IY)/OpenAD_aux_7)
          OpenAD_aux_10 = (FRICTU%v*5.0D-01)
          OpenAD_aux_9 = (1.0D00-DT*OpenAD_aux_10)
          OpenAD_aux_11 = (DT*9.81000000000000049738D00)
          OpenAD_aux_12 = (UFORCE(IX,IY)*DT)
          OpenAD_aux_6 = (FV%v*DT+LOCAL_U(IX,IY)%v*OpenAD_aux_9-GRADETAU
     +%v*OpenAD_aux_11+INVHU(IX,IY)%v*OpenAD_aux_12)
          OpenAD_lin_7 = DT
          OpenAD_lin_6 = (-(UMASK(IX,IY)/(OpenAD_aux_7*OpenAD_aux_7)))
          OpenAD_lin_4 = OpenAD_aux_6
          OpenAD_lin_8 = DT
          OpenAD_lin_9 = OpenAD_aux_9
          OpenAD_lin_11 = DT
          OpenAD_lin_10 = LOCAL_U(IX,IY)%v
          OpenAD_lin_12 = OpenAD_aux_11
          OpenAD_lin_13 = OpenAD_aux_12
          OpenAD_lin_5 = OpenAD_aux_5
          LOCAL_U(INT(IX),INT(IY))%v = (OpenAD_aux_5*OpenAD_aux_6)
          OpenAD_acc_0 = (OpenAD_lin_13*OpenAD_lin_5)
          OpenAD_acc_1 = (OpenAD_lin_3*OpenAD_lin_8*OpenAD_lin_5)
          OpenAD_acc_2 = (OpenAD_lin_9*OpenAD_lin_5)
          OpenAD_acc_3 = (OpenAD_lin_2*OpenAD_lin_12*INT((-1_w2f__i8))*O
     +penAD_lin_5)
          OpenAD_acc_4 = (OpenAD_lin_1*5.0D-01*OpenAD_lin_11*INT((-1_w2f
     +__i8))*OpenAD_lin_10*OpenAD_lin_5+OpenAD_lin_1*5.0D-01*OpenAD_lin_
     +7*OpenAD_lin_6*OpenAD_lin_4)
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_0
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_1
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_2
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_3
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_4
          double_tape_pointer = double_tape_pointer+1
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          LOCAL_U(21,INT(IY))%v = LOCAL_U(1,IY)%v
        END DO
        OpenAD_Symbol_1008 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1008
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1007 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1007
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          LOCAL_U(INT(IX),0)%v = LOCAL_U(IX,20)%v
        END DO
        OpenAD_Symbol_1010 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1010
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1009 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1009
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " umomentum: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1005 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1005.ne.0) THEN
        IX = 0+1*((21-0)/1)
        do while (IX.GE.0)
          OpenAD_prp_4%d = OpenAD_prp_4%d+LOCAL_U(IX,0)%d
          LOCAL_U(IX,0)%d = 0.0d0
          LOCAL_U(IX,20)%d = LOCAL_U(IX,20)%d+OpenAD_prp_4%d
          OpenAD_prp_4%d = 0.0d0
          IX = IX-1
        END DO
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1006 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1006.ne.0) THEN
        IY = 0+1*((21-0)/1)
        do while (IY.GE.0)
          OpenAD_prp_3%d = OpenAD_prp_3%d+LOCAL_U(21,IY)%d
          LOCAL_U(21,IY)%d = 0.0d0
          LOCAL_U(1,IY)%d = LOCAL_U(1,IY)%d+OpenAD_prp_3%d
          OpenAD_prp_3%d = 0.0d0
          IY = IY-1
        END DO
      ENDIF
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1320 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1321 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1322 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1323 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1324 = double_tape(double_tape_pointer)
          INVHU(IX,IY)%d = INVHU(IX,IY)%d+LOCAL_U(IX,IY)%d*(OpenAD_Symbo
     +l_1320)
          OpenAD_prp_1%d = OpenAD_prp_1%d+LOCAL_U(IX,IY)%d*(OpenAD_Symbo
     +l_1321)
          OpenAD_prp_0%d = OpenAD_prp_0%d+LOCAL_U(IX,IY)%d*(OpenAD_Symbo
     +l_1322)
          OpenAD_prp_2%d = OpenAD_prp_2%d+LOCAL_U(IX,IY)%d*(OpenAD_Symbo
     +l_1323)
          INVHU(IX,IY)%d = INVHU(IX,IY)%d+LOCAL_U(IX,IY)%d*(OpenAD_Symbo
     +l_1324)
          LOCAL_U(IX,IY)%d = 0.0d0
          LOCAL_V(IX+(-1),IY)%d = LOCAL_V(IX+(-1),IY)%d+OpenAD_prp_2%d
          LOCAL_V(IX,IY)%d = LOCAL_V(IX,IY)%d+OpenAD_prp_2%d
          LOCAL_V(IX,IY+1)%d = LOCAL_V(IX,IY+1)%d+OpenAD_prp_2%d
          LOCAL_V(IX+(-1),IY+1)%d = LOCAL_V(IX+(-1),IY+1)%d+OpenAD_prp_2
     +%d
          OpenAD_prp_2%d = 0.0d0
          LOCAL_ETA(IX+(-1),IY)%d = LOCAL_ETA(IX+(-1),IY)%d-OpenAD_prp_1
     +%d
          LOCAL_ETA(IX,IY)%d = LOCAL_ETA(IX,IY)%d+OpenAD_prp_1%d
          OpenAD_prp_1%d = 0.0d0
          LOCAL_U(IX,IY)%d = LOCAL_U(IX,IY)%d+OpenAD_prp_0%d
          OpenAD_prp_0%d = 0.0d0
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      end if
      end subroutine umomentum

      SUBROUTINE vmomentum(LOCAL_U, LOCAL_V, LOCAL_ETA)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      type(active) :: LOCAL_V(0:21,0:21)
      type(active) :: LOCAL_ETA(0:21,0:21)
C
C     **** Local Variables and Functions ****
C
      type(active) :: FRICTV
      type(active) :: FU
      type(active) :: GRADETAV
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
      type(active) :: OpenAD_prp_5
      type(active) :: OpenAD_prp_6
      type(active) :: OpenAD_prp_7
      type(active) :: OpenAD_prp_8
      type(active) :: OpenAD_prp_9


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter vmomentum:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter vmomentum:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " vmomentum: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          FRICTV%v = (INVHV(IX,IY)%v*(FRICT(IX,IY)+FRICT(IX,IY+(-1)))*5.
     +0D-01)
          GRADETAV%v = ((LOCAL_ETA(IX,IY)%v-LOCAL_ETA(IX,IY+(-1))%v)/(RY
     +*5.0D-01*(DY(IY)+DY(IY+(-1)))))
          FU%v = (FCORIV(IX,IY)*(LOCAL_U(IX+1,IY+(-1))%v+LOCAL_U(IX,IY+(
     +-1))%v+LOCAL_U(IX,IY)%v+LOCAL_U(IX+1,IY)%v))
          LOCAL_V(INT(IX),INT(IY))%v = ((VMASK(IX,IY)/(DT*FRICTV%v*5.0D-
     +01+1.0D00))*(INVHV(IX,IY)%v*VFORCE(IX,IY)*DT+LOCAL_V(IX,IY)%v*(1.0
     +D00-DT*FRICTV%v*5.0D-01)-GRADETAV%v*DT*9.81000000000000049738D00-F
     +U%v*DT))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          LOCAL_V(0,INT(IY))%v = LOCAL_V(20,IY)%v
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          LOCAL_V(INT(IX),21)%v = LOCAL_V(IX,1)%v
        END DO
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " vmomentum: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          OpenAD_aux_13 = ((FRICT(IX,IY)+FRICT(IX,IY+(-1)))*5.0D-01)
          OpenAD_lin_14 = OpenAD_aux_13
          FRICTV%v = (INVHV(IX,IY)%v*OpenAD_aux_13)
          OpenAD_aux_14 = (LOCAL_ETA(IX,IY)%v-LOCAL_ETA(IX,IY+(-1))%v)
          OpenAD_aux_15 = (RY*5.0D-01*(DY(IY)+DY(IY+(-1))))
          OpenAD_lin_15 = (INT(1_w2f__i8)/OpenAD_aux_15)
          GRADETAV%v = (OpenAD_aux_14/OpenAD_aux_15)
          OpenAD_aux_16 = (LOCAL_U(IX+1,IY+(-1))%v+LOCAL_U(IX,IY+(-1))%v
     ++LOCAL_U(IX,IY)%v+LOCAL_U(IX+1,IY)%v)
          OpenAD_lin_16 = FCORIV(IX,IY)
          FU%v = (FCORIV(IX,IY)*OpenAD_aux_16)
          OpenAD_aux_20 = (FRICTV%v*5.0D-01)
          OpenAD_aux_19 = (DT*OpenAD_aux_20+1.0D00)
          OpenAD_aux_17 = (VMASK(IX,IY)/OpenAD_aux_19)
          OpenAD_aux_21 = (VFORCE(IX,IY)*DT)
          OpenAD_aux_23 = (FRICTV%v*5.0D-01)
          OpenAD_aux_22 = (1.0D00-DT*OpenAD_aux_23)
          OpenAD_aux_24 = (DT*9.81000000000000049738D00)
          OpenAD_aux_18 = (INVHV(IX,IY)%v*OpenAD_aux_21+LOCAL_V(IX,IY)%v
     +*OpenAD_aux_22-GRADETAV%v*OpenAD_aux_24-FU%v*DT)
          OpenAD_lin_20 = DT
          OpenAD_lin_19 = (-(VMASK(IX,IY)/(OpenAD_aux_19*OpenAD_aux_19))
     +)
          OpenAD_lin_17 = OpenAD_aux_18
          OpenAD_lin_21 = OpenAD_aux_21
          OpenAD_lin_22 = OpenAD_aux_22
          OpenAD_lin_24 = DT
          OpenAD_lin_23 = LOCAL_V(IX,IY)%v
          OpenAD_lin_25 = OpenAD_aux_24
          OpenAD_lin_26 = DT
          OpenAD_lin_18 = OpenAD_aux_17
          LOCAL_V(INT(IX),INT(IY))%v = (OpenAD_aux_17*OpenAD_aux_18)
          OpenAD_acc_5 = (OpenAD_lin_21*OpenAD_lin_18)
          OpenAD_acc_6 = (OpenAD_lin_16*OpenAD_lin_26*INT((-1_w2f__i8))*
     +OpenAD_lin_18)
          OpenAD_acc_7 = (OpenAD_lin_22*OpenAD_lin_18)
          OpenAD_acc_8 = (OpenAD_lin_15*OpenAD_lin_25*INT((-1_w2f__i8))*
     +OpenAD_lin_18)
          OpenAD_acc_9 = (OpenAD_lin_14*5.0D-01*OpenAD_lin_24*INT((-1_w2
     +f__i8))*OpenAD_lin_23*OpenAD_lin_18+OpenAD_lin_14*5.0D-01*OpenAD_l
     +in_20*OpenAD_lin_19*OpenAD_lin_17)
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_5
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_6
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_7
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_8
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_9
          double_tape_pointer = double_tape_pointer+1
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          LOCAL_V(0,INT(IY))%v = LOCAL_V(20,IY)%v
        END DO
        OpenAD_Symbol_1037 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1037
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1036 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1036
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          LOCAL_V(INT(IX),21)%v = LOCAL_V(IX,1)%v
        END DO
        OpenAD_Symbol_1039 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1039
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1038 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1038
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " vmomentum: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1034 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1034.ne.0) THEN
        IX = 0+1*((21-0)/1)
        do while (IX.GE.0)
          OpenAD_prp_9%d = OpenAD_prp_9%d+LOCAL_V(IX,21)%d
          LOCAL_V(IX,21)%d = 0.0d0
          LOCAL_V(IX,1)%d = LOCAL_V(IX,1)%d+OpenAD_prp_9%d
          OpenAD_prp_9%d = 0.0d0
          IX = IX-1
        END DO
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1035 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1035.ne.0) THEN
        IY = 0+1*((21-0)/1)
        do while (IY.GE.0)
          OpenAD_prp_8%d = OpenAD_prp_8%d+LOCAL_V(0,IY)%d
          LOCAL_V(0,IY)%d = 0.0d0
          LOCAL_V(20,IY)%d = LOCAL_V(20,IY)%d+OpenAD_prp_8%d
          OpenAD_prp_8%d = 0.0d0
          IY = IY-1
        END DO
      ENDIF
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1334 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1335 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1336 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1337 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1338 = double_tape(double_tape_pointer)
          INVHV(IX,IY)%d = INVHV(IX,IY)%d+LOCAL_V(IX,IY)%d*(OpenAD_Symbo
     +l_1334)
          OpenAD_prp_6%d = OpenAD_prp_6%d+LOCAL_V(IX,IY)%d*(OpenAD_Symbo
     +l_1335)
          OpenAD_prp_5%d = OpenAD_prp_5%d+LOCAL_V(IX,IY)%d*(OpenAD_Symbo
     +l_1336)
          OpenAD_prp_7%d = OpenAD_prp_7%d+LOCAL_V(IX,IY)%d*(OpenAD_Symbo
     +l_1337)
          INVHV(IX,IY)%d = INVHV(IX,IY)%d+LOCAL_V(IX,IY)%d*(OpenAD_Symbo
     +l_1338)
          LOCAL_V(IX,IY)%d = 0.0d0
          LOCAL_U(IX+1,IY)%d = LOCAL_U(IX+1,IY)%d+OpenAD_prp_7%d
          LOCAL_U(IX,IY)%d = LOCAL_U(IX,IY)%d+OpenAD_prp_7%d
          LOCAL_U(IX,IY+(-1))%d = LOCAL_U(IX,IY+(-1))%d+OpenAD_prp_7%d
          LOCAL_U(IX+1,IY+(-1))%d = LOCAL_U(IX+1,IY+(-1))%d+OpenAD_prp_7
     +%d
          OpenAD_prp_7%d = 0.0d0
          LOCAL_ETA(IX,IY+(-1))%d = LOCAL_ETA(IX,IY+(-1))%d-OpenAD_prp_6
     +%d
          LOCAL_ETA(IX,IY)%d = LOCAL_ETA(IX,IY)%d+OpenAD_prp_6%d
          OpenAD_prp_6%d = 0.0d0
          LOCAL_V(IX,IY)%d = LOCAL_V(IX,IY)%d+OpenAD_prp_5%d
          OpenAD_prp_5%d = 0.0d0
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      end if
      end subroutine vmomentum

      SUBROUTINE continuity(LOCAL_U, LOCAL_V, LOCAL_ETA)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      type(active) :: LOCAL_V(0:21,0:21)
      type(active) :: LOCAL_ETA(0:21,0:21)
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
      type(active) :: OpenAD_prp_10
      type(active) :: OpenAD_prp_11
      type(active) :: OpenAD_prp_12


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter continuity:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter continuity:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " continuity: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          LOCAL_ETA(INT(IX),INT(IY))%v = (LOCAL_ETA(IX,IY)%v-ETAMASK(IX,
     +IY)*DT*(((LOCAL_U(IX+1,IY)%v*HU(IX+1,IY)%v-LOCAL_U(IX,IY)%v*HU(IX,
     +IY)%v)/(DX(IX)*RX(IY)))+((LOCAL_V(IX,IY+1)%v*HY(IY+1)*HV(IX,IY+1)%
     +v-LOCAL_V(IX,IY)%v*HY(IY)*HV(IX,IY)%v)/(DY(IY)*RY))))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          LOCAL_ETA(0,INT(IY))%v = LOCAL_ETA(20,IY)%v
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          LOCAL_ETA(INT(IX),0)%v = LOCAL_ETA(IX,20)%v
        END DO
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " continuity: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          OpenAD_aux_25 = (ETAMASK(IX,IY)*DT)
          OpenAD_aux_27 = (LOCAL_U(IX+1,IY)%v*HU(IX+1,IY)%v-LOCAL_U(IX,I
     +Y)%v*HU(IX,IY)%v)
          OpenAD_aux_28 = (DX(IX)*RX(IY))
          OpenAD_aux_31 = (HY(IY+1)*HV(IX,IY+1)%v)
          OpenAD_aux_32 = (HY(IY)*HV(IX,IY)%v)
          OpenAD_aux_29 = (LOCAL_V(IX,IY+1)%v*OpenAD_aux_31-LOCAL_V(IX,I
     +Y)%v*OpenAD_aux_32)
          OpenAD_aux_30 = (DY(IY)*RY)
          OpenAD_aux_26 = ((OpenAD_aux_27/OpenAD_aux_28)+(OpenAD_aux_29/
     +OpenAD_aux_30))
          OpenAD_lin_29 = HU(IX+1,IY)%v
          OpenAD_lin_30 = LOCAL_U(IX+1,IY)%v
          OpenAD_lin_31 = HU(IX,IY)%v
          OpenAD_lin_32 = LOCAL_U(IX,IY)%v
          OpenAD_lin_28 = (INT(1_w2f__i8)/OpenAD_aux_28)
          OpenAD_lin_34 = OpenAD_aux_31
          OpenAD_lin_36 = HY(IY+1)
          OpenAD_lin_35 = LOCAL_V(IX,IY+1)%v
          OpenAD_lin_37 = OpenAD_aux_32
          OpenAD_lin_39 = HY(IY)
          OpenAD_lin_38 = LOCAL_V(IX,IY)%v
          OpenAD_lin_33 = (INT(1_w2f__i8)/OpenAD_aux_30)
          OpenAD_lin_27 = OpenAD_aux_25
          LOCAL_ETA(INT(IX),INT(IY))%v = (LOCAL_ETA(IX,IY)%v-OpenAD_aux_
     +25*OpenAD_aux_26)
          OpenAD_acc_10 = (OpenAD_lin_27*INT((-1_w2f__i8)))
          OpenAD_acc_11 = (OpenAD_lin_33*OpenAD_acc_10)
          OpenAD_acc_12 = (OpenAD_lin_28*OpenAD_acc_10)
          OpenAD_acc_13 = (INT((-1_w2f__i8))*OpenAD_acc_11)
          OpenAD_acc_14 = (OpenAD_lin_37*OpenAD_acc_13)
          OpenAD_acc_15 = (OpenAD_lin_39*OpenAD_lin_38*OpenAD_acc_13)
          OpenAD_acc_16 = (OpenAD_lin_34*OpenAD_acc_11)
          OpenAD_acc_17 = (OpenAD_lin_36*OpenAD_lin_35*OpenAD_acc_11)
          OpenAD_acc_18 = (INT((-1_w2f__i8))*OpenAD_acc_12)
          OpenAD_acc_19 = (OpenAD_lin_31*OpenAD_acc_18)
          OpenAD_acc_20 = (OpenAD_lin_32*OpenAD_acc_18)
          OpenAD_acc_21 = (OpenAD_lin_29*OpenAD_acc_12)
          OpenAD_acc_22 = (OpenAD_lin_30*OpenAD_acc_12)
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_14
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_15
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_16
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_17
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_19
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_20
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_21
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_acc_22
          double_tape_pointer = double_tape_pointer+1
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          LOCAL_ETA(0,INT(IY))%v = LOCAL_ETA(20,IY)%v
        END DO
        OpenAD_Symbol_1066 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1066
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1065 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1065
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          LOCAL_ETA(INT(IX),0)%v = LOCAL_ETA(IX,20)%v
        END DO
        OpenAD_Symbol_1068 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1068
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1067 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1067
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " continuity: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1063 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1063.ne.0) THEN
        IX = 0+1*((21-0)/1)
        do while (IX.GE.0)
          OpenAD_prp_12%d = OpenAD_prp_12%d+LOCAL_ETA(IX,0)%d
          LOCAL_ETA(IX,0)%d = 0.0d0
          LOCAL_ETA(IX,20)%d = LOCAL_ETA(IX,20)%d+OpenAD_prp_12%d
          OpenAD_prp_12%d = 0.0d0
          IX = IX-1
        END DO
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1064 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1064.ne.0) THEN
        IY = 0+1*((21-0)/1)
        do while (IY.GE.0)
          OpenAD_prp_11%d = OpenAD_prp_11%d+LOCAL_ETA(0,IY)%d
          LOCAL_ETA(0,IY)%d = 0.0d0
          LOCAL_ETA(20,IY)%d = LOCAL_ETA(20,IY)%d+OpenAD_prp_11%d
          OpenAD_prp_11%d = 0.0d0
          IY = IY-1
        END DO
      ENDIF
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1348 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1349 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1350 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1351 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1352 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1353 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1354 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1355 = double_tape(double_tape_pointer)
          HU(IX+1,IY)%d = HU(IX+1,IY)%d+LOCAL_ETA(IX,IY)%d*(OpenAD_Symbo
     +l_1348)
          LOCAL_U(IX+1,IY)%d = LOCAL_U(IX+1,IY)%d+LOCAL_ETA(IX,IY)%d*(Op
     +enAD_Symbol_1349)
          HU(IX,IY)%d = HU(IX,IY)%d+LOCAL_ETA(IX,IY)%d*(OpenAD_Symbol_13
     +50)
          LOCAL_U(IX,IY)%d = LOCAL_U(IX,IY)%d+LOCAL_ETA(IX,IY)%d*(OpenAD
     +_Symbol_1351)
          HV(IX,IY+1)%d = HV(IX,IY+1)%d+LOCAL_ETA(IX,IY)%d*(OpenAD_Symbo
     +l_1352)
          LOCAL_V(IX,IY+1)%d = LOCAL_V(IX,IY+1)%d+LOCAL_ETA(IX,IY)%d*(Op
     +enAD_Symbol_1353)
          HV(IX,IY)%d = HV(IX,IY)%d+LOCAL_ETA(IX,IY)%d*(OpenAD_Symbol_13
     +54)
          LOCAL_V(IX,IY)%d = LOCAL_V(IX,IY)%d+LOCAL_ETA(IX,IY)%d*(OpenAD
     +_Symbol_1355)
          OpenAD_prp_10%d = OpenAD_prp_10%d+LOCAL_ETA(IX,IY)%d
          LOCAL_ETA(IX,IY)%d = 0.0d0
          LOCAL_ETA(IX,IY)%d = LOCAL_ETA(IX,IY)%d+OpenAD_prp_10%d
          OpenAD_prp_10%d = 0.0d0
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      end if
      end subroutine continuity

      SUBROUTINE calc_overturning(OVERTURNING, LOCAL_U)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      INTENT(IN) LOCAL_U
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      PARAMETER ( IX = 7)
      INTEGER(w2f__i4) IY


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter calc_overturning:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter calc_overturning:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " calc_overturning: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      OVERTURNING = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 10, 1
        OVERTURNING = (OVERTURNING+HU(7,IY)%v*DY(IY)*LOCAL_U(7,IY))
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " calc_overturning: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      OVERTURNING = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1,10,1
        OVERTURNING = (OVERTURNING+HU(7,IY)%v*DY(IY)*LOCAL_U(7,IY))
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " calc_overturning: entering adjoint
C "
      IY = 1+1*((10-1)/1)
      do while (IY.GE.1)
        IY = IY-1
      END DO
      end if
      end subroutine calc_overturning

      SUBROUTINE calc_zonal_transport_split(ZONAL_TRANSPORT, LOCAL_U)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      INTENT(IN) LOCAL_U
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) IX
      PARAMETER ( IX = 6)
      INTEGER(w2f__i4) IY


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter calc_zonal_transport_split:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter calc_zonal_transport_spl
C it:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " calc_zonal_transport_split: enteri
C ng plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        ZONAL_TRANSPORT = (ZONAL_TRANSPORT+HU(6,IY)%v*DY(IY)*LOCAL_U(6,I
     +Y))
      END DO
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " calc_zonal_transport_split: enteri
C ng tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      ZONAL_TRANSPORT = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        ZONAL_TRANSPORT = (ZONAL_TRANSPORT+HU(6,IY)%v*DY(IY)*LOCAL_U(6,I
     +Y))
      END DO
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " calc_zonal_transport_split: enteri
C ng adjoint"
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IY = IY-1
      END DO
      end if
      end subroutine calc_zonal_transport_split

      SUBROUTINE initial_values()
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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
      type(active) :: OpenAD_prp_13
      type(active) :: OpenAD_prp_14
      type(active) :: OpenAD_prp_15
      type(active) :: OpenAD_prp_16
      type(active) :: OpenAD_prp_17
      type(active) :: OpenAD_prp_18


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter in
     +itial_values:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our
     +_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,ou
     +r_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter initial_values:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," initial_values: en
     +tering arg store"
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = XPERIODIC
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = YPERIODIC
      do cp_loop_variable_1 = lbound(ETA,1),ubound(ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA,2)-lbound
     +(ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETA,2),ubound(ETA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA(cp_loop_variable_1,cp_loop_vari
     +able_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(U,1),ubound(U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(U,2)-lbound(U
     +,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(U,2),ubound(U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = U(cp_loop_variable_1,cp_loop_variab
     +le_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(V,1),ubound(V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(V,2)-lbound(V
     +,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(V,2),ubound(V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = V(cp_loop_variable_1,cp_loop_variab
     +le_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(ETAINI,1),ubound(ETAINI,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAINI,2)-lbo
     +und(ETAINI,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAINI,2),ubound(ETAINI,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAINI(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UINI,1),ubound(UINI,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UINI,2)-lboun
     +d(UINI,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UINI,2),ubound(UINI,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UINI(cp_loop_variable_1,cp_loop_var
     +iable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UMASK,1),ubound(UMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UMASK,2)-lbou
     +nd(UMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UMASK,2),ubound(UMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VINI,1),ubound(VINI,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VINI,2)-lboun
     +d(VINI,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VINI,2),ubound(VINI,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VINI(cp_loop_variable_1,cp_loop_var
     +iable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VMASK,1),ubound(VMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VMASK,2)-lbou
     +nd(VMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VMASK,2),ubound(VMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," initial_values: en
     +tering arg restore"
      do cp_loop_variable_1 = ubound(VMASK,1),lbound(VMASK,1),-1
      do cp_loop_variable_2 = ubound(VMASK,2),lbound(VMASK,2),-1
      VMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VINI,1),lbound(VINI,1),-1
      do cp_loop_variable_2 = ubound(VINI,2),lbound(VINI,2),-1
      VINI(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UMASK,1),lbound(UMASK,1),-1
      do cp_loop_variable_2 = ubound(UMASK,2),lbound(UMASK,2),-1
      UMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UINI,1),lbound(UINI,1),-1
      do cp_loop_variable_2 = ubound(UINI,2),lbound(UINI,2),-1
      UINI(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAINI,1),lbound(ETAINI,1),-1
      do cp_loop_variable_2 = ubound(ETAINI,2),lbound(ETAINI,2),-1
      ETAINI(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(V,1),lbound(V,1),-1
      do cp_loop_variable_2 = ubound(V,2),lbound(V,2),-1
      V(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_p
     +ointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(U,1),lbound(U,1),-1
      do cp_loop_variable_2 = ubound(U,2),lbound(U,2),-1
      U(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_p
     +ointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETA,1),lbound(ETA,1),-1
      do cp_loop_variable_2 = ubound(ETA,2),lbound(ETA,2),-1
      ETA(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      YPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      XPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," initial_values: en
     +tering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 20, 1
          U(INT(IX),INT(IY))%v = (UINI(IX,IY)*UMASK(IX,IY))
          V(INT(IX),INT(IY))%v = (VINI(IX,IY)*VMASK(IX,IY))
          ETA(INT(IX),INT(IY))%v = (ETAINI(IX,IY)*ETAMASK(IX,IY))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          U(21,INT(IY))%v = U(1,IY)%v
          V(0,INT(IY))%v = V(20,IY)%v
          ETA(0,INT(IY))%v = ETA(20,IY)%v
        END DO
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          U(INT(IX),0)%v = U(IX,20)%v
          V(INT(IX),21)%v = V(IX,1)%v
          ETA(INT(IX),0)%v = ETA(IX,0)%v
        END DO
      ENDIF
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," initial_values: en
     +tering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          U(INT(IX),INT(IY))%v = (UINI(IX,IY)*UMASK(IX,IY))
          V(INT(IX),INT(IY))%v = (VINI(IX,IY)*VMASK(IX,IY))
          ETA(INT(IX),INT(IY))%v = (ETAINI(IX,IY)*ETAMASK(IX,IY))
        END DO
      END DO
      IF(XPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IY = 0,21,1
          U(21,INT(IY))%v = U(1,IY)%v
          V(0,INT(IY))%v = V(20,IY)%v
          ETA(0,INT(IY))%v = ETA(20,IY)%v
        END DO
        OpenAD_Symbol_1100 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1100
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1099 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1099
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(YPERIODIC) THEN
C$OPENAD XXX Simple loop
        DO IX = 0,21,1
          U(INT(IX),0)%v = U(IX,20)%v
          V(INT(IX),21)%v = V(IX,1)%v
          ETA(INT(IX),0)%v = ETA(IX,0)%v
        END DO
        OpenAD_Symbol_1102 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1102
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1101 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1101
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," initial_values: en
     +tering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1097 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1097.ne.0) THEN
        IX = 0+1*((21-0)/1)
        do while (IX.GE.0)
          OpenAD_prp_18%d = OpenAD_prp_18%d+ETA(IX,0)%d
          ETA(IX,0)%d = 0.0d0
          OpenAD_prp_17%d = OpenAD_prp_17%d+V(IX,21)%d
          V(IX,21)%d = 0.0d0
          OpenAD_prp_16%d = OpenAD_prp_16%d+U(IX,0)%d
          U(IX,0)%d = 0.0d0
          ETA(IX,0)%d = ETA(IX,0)%d+OpenAD_prp_18%d
          OpenAD_prp_18%d = 0.0d0
          V(IX,1)%d = V(IX,1)%d+OpenAD_prp_17%d
          OpenAD_prp_17%d = 0.0d0
          U(IX,20)%d = U(IX,20)%d+OpenAD_prp_16%d
          OpenAD_prp_16%d = 0.0d0
          IX = IX-1
        END DO
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1098 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1098.ne.0) THEN
        IY = 0+1*((21-0)/1)
        do while (IY.GE.0)
          OpenAD_prp_15%d = OpenAD_prp_15%d+ETA(0,IY)%d
          ETA(0,IY)%d = 0.0d0
          OpenAD_prp_14%d = OpenAD_prp_14%d+V(0,IY)%d
          V(0,IY)%d = 0.0d0
          OpenAD_prp_13%d = OpenAD_prp_13%d+U(21,IY)%d
          U(21,IY)%d = 0.0d0
          ETA(20,IY)%d = ETA(20,IY)%d+OpenAD_prp_15%d
          OpenAD_prp_15%d = 0.0d0
          V(20,IY)%d = V(20,IY)%d+OpenAD_prp_14%d
          OpenAD_prp_14%d = 0.0d0
          U(1,IY)%d = U(1,IY)%d+OpenAD_prp_13%d
          OpenAD_prp_13%d = 0.0d0
          IY = IY-1
        END DO
      ENDIF
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          ETA(IX,IY)%d = 0.0d0
          V(IX,IY)%d = 0.0d0
          U(IX,IY)%d = 0.0d0
          IX = IX-1
        END DO
        IY = IY-1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave initial_values:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave in
     +itial_values:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our
     +_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,ou
     +r_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine initial_values

      SUBROUTINE calc_depth_uv()
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter ca
     +lc_depth_uv:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_
     +rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our
     +_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter calc_depth_uv:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_depth_uv: ent
     +ering arg store"
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(HU,1),ubound(HU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HU,2)-lbound(
     +HU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HU,2),ubound(HU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HU(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(HV,1),ubound(HV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HV,2)-lbound(
     +HV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HV,2),ubound(HV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HV(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(UMASK,1),ubound(UMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UMASK,2)-lbou
     +nd(UMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UMASK,2),ubound(UMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VMASK,1),ubound(VMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VMASK,2)-lbou
     +nd(VMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VMASK,2),ubound(VMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_depth_uv: ent
     +ering arg restore"
      do cp_loop_variable_1 = ubound(VMASK,1),lbound(VMASK,1),-1
      do cp_loop_variable_2 = ubound(VMASK,2),lbound(VMASK,2),-1
      VMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UMASK,1),lbound(UMASK,1),-1
      do cp_loop_variable_2 = ubound(UMASK,2),lbound(UMASK,2),-1
      UMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HV,1),lbound(HV,1),-1
      do cp_loop_variable_2 = ubound(HV,2),lbound(HV,2),-1
      HV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HU,1),lbound(HU,1),-1
      do cp_loop_variable_2 = ubound(HU,2),lbound(HU,2),-1
      HU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_depth_uv: ent
     +ering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 21, 1
        DO IX = 1, 21, 1
          HU(INT(IX),INT(IY))%v = (DEPTH(IX,IY)%v*UMASK(IX,IY))
          HV(INT(IX),INT(IY))%v = (DEPTH(IX,IY)%v*VMASK(IX,IY))
          IF (HU(IX,IY)%v.ne.0.0D00) THEN
            INVHU(INT(IX),INT(IY))%v = (1.0D00/HU(IX,IY)%v)
          ENDIF
          IF (HV(IX,IY)%v.ne.0.0D00) THEN
            INVHV(INT(IX),INT(IY))%v = (1.0D00/HV(IX,IY)%v)
          ENDIF
        END DO
      END DO
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_depth_uv: ent
     +ering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1,21,1
        DO IX = 1,21,1
          OpenAD_lin_40 = UMASK(IX,IY)
          HU(INT(IX),INT(IY))%v = (DEPTH(IX,IY)%v*UMASK(IX,IY))
          OpenAD_lin_41 = VMASK(IX,IY)
          HV(INT(IX),INT(IY))%v = (DEPTH(IX,IY)%v*VMASK(IX,IY))
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_lin_40
          double_tape_pointer = double_tape_pointer+1
          if (double_tape_size.lt.double_tape_pointer) then
          allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variab
     +le_1)
          if (cp_loop_variable_1.gt.0) then
          print *,'allocation failed with',cp_loop_variable_1
          stop
          end if
          double_tmp_tape = double_tape
          deallocate(double_tape)
          allocate(double_tape(double_tape_size*2))
          print *,"DT+"
          double_tape(1:double_tape_size) = double_tmp_tape
          deallocate(double_tmp_tape)
          double_tape_size = double_tape_size*2
          end if
          double_tape(double_tape_pointer) = OpenAD_lin_41
          double_tape_pointer = double_tape_pointer+1
          IF (HU(IX,IY)%v.ne.0.0D00) THEN
            OpenAD_lin_42 = (-(1.0D00/(HU(IX,IY)%v*HU(IX,IY)%v)))
            INVHU(INT(IX),INT(IY))%v = (1.0D00/HU(IX,IY)%v)
            if (double_tape_size.lt.double_tape_pointer) then
            allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_vari
     +able_1)
            if (cp_loop_variable_1.gt.0) then
            print *,'allocation failed with',cp_loop_variable_1
            stop
            end if
            double_tmp_tape = double_tape
            deallocate(double_tape)
            allocate(double_tape(double_tape_size*2))
            print *,"DT+"
            double_tape(1:double_tape_size) = double_tmp_tape
            deallocate(double_tmp_tape)
            double_tape_size = double_tape_size*2
            end if
            double_tape(double_tape_pointer) = OpenAD_lin_42
            double_tape_pointer = double_tape_pointer+1
          ENDIF
          IF (HV(IX,IY)%v.ne.0.0D00) THEN
            OpenAD_lin_43 = (-(1.0D00/(HV(IX,IY)%v*HV(IX,IY)%v)))
            INVHV(INT(IX),INT(IY))%v = (1.0D00/HV(IX,IY)%v)
            if (double_tape_size.lt.double_tape_pointer) then
            allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_vari
     +able_1)
            if (cp_loop_variable_1.gt.0) then
            print *,'allocation failed with',cp_loop_variable_1
            stop
            end if
            double_tmp_tape = double_tape
            deallocate(double_tape)
            allocate(double_tape(double_tape_size*2))
            print *,"DT+"
            double_tape(1:double_tape_size) = double_tmp_tape
            deallocate(double_tmp_tape)
            double_tape_size = double_tape_size*2
            end if
            double_tape(double_tape_pointer) = OpenAD_lin_43
            double_tape_pointer = double_tape_pointer+1
          ENDIF
        END DO
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_depth_uv: ent
     +ering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      IY = 1+1*((21-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((21-1)/1)
        do while (IX.GE.1)
          IF (HV(IX,IY)%v.ne.0.0D00) THEN
            double_tape_pointer = double_tape_pointer-1
            OpenAD_Symbol_1375 = double_tape(double_tape_pointer)
            HV(IX,IY)%d = HV(IX,IY)%d+INVHV(IX,IY)%d*(OpenAD_Symbol_1375
     +)
            INVHV(IX,IY)%d = 0.0d0
          ENDIF
          IF (HU(IX,IY)%v.ne.0.0D00) THEN
            double_tape_pointer = double_tape_pointer-1
            OpenAD_Symbol_1372 = double_tape(double_tape_pointer)
            HU(IX,IY)%d = HU(IX,IY)%d+INVHU(IX,IY)%d*(OpenAD_Symbol_1372
     +)
            INVHU(IX,IY)%d = 0.0d0
          ENDIF
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1368 = double_tape(double_tape_pointer)
          double_tape_pointer = double_tape_pointer-1
          OpenAD_Symbol_1369 = double_tape(double_tape_pointer)
          DEPTH(IX,IY)%d = DEPTH(IX,IY)%d+HV(IX,IY)%d*(OpenAD_Symbol_136
     +8)
          HV(IX,IY)%d = 0.0d0
          DEPTH(IX,IY)%d = DEPTH(IX,IY)%d+HU(IX,IY)%d*(OpenAD_Symbol_136
     +9)
          HU(IX,IY)%d = 0.0d0
          IX = IX-1
        END DO
        IY = IY-1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave calc_depth_uv:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave ca
     +lc_depth_uv:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_
     +rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our
     +_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine calc_depth_uv

      SUBROUTINE calc_zonal_transport_joint(ZONAL_TRANSPORT, LOCAL_U)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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
      type(active) :: ZONAL_TRANSPORT
      type(active) :: LOCAL_U(0:21,0:21)
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
      type(active) :: OpenAD_prp_19
C
C     **** Statements ****
C


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C$OPENAD XXX Template OADrts/ad_template.joint.f
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter ca
     +lc_zonal_transport_joint:",our_rev_mode%arg_store,our_rev_mode%arg
     +_restore,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_m
     +ode%plain,our_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter calc_zonal_transport_joint:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_zonal_transpo
     +rt_joint: entering arg store"
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ZONAL_TRANSPORT%v
      do while (cp_double_size.lt.cp_double_pointer+ubound(DY,1)-lbound(
     +DY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DY,1),ubound(DY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(HU,1),ubound(HU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HU,2)-lbound(
     +HU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HU,2),ubound(HU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HU(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(LOCAL_U,1),ubound(LOCAL_U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_U,2)-lb
     +ound(LOCAL_U,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_U,2),ubound(LOCAL_U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_U(cp_loop_variable_1,cp_loop_
     +variable_2)%v
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_zonal_transpo
     +rt_joint: entering arg restore"
      do cp_loop_variable_1 = ubound(LOCAL_U,1),lbound(LOCAL_U,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_U,2),lbound(LOCAL_U,2),-1
      LOCAL_U(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_do
     +uble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HU,1),lbound(HU,1),-1
      do cp_loop_variable_2 = ubound(HU,2),lbound(HU,2),-1
      HU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DY,1),lbound(DY,1),-1
      DY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DY(cp_loop_variable_1)%v
      end do
      ZONAL_TRANSPORT%v = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_zonal_transpo
     +rt_joint: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
      ZONAL_TRANSPORT%v = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        ZONAL_TRANSPORT%v = (ZONAL_TRANSPORT%v+HU(6,IY)%v*DY(IY)*LOCAL_U
     +(6,IY)%v)
      END DO
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_zonal_transpo
     +rt_joint: entering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
      ZONAL_TRANSPORT%v = 0.0D00
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        OpenAD_aux_33 = (DY(IY)*LOCAL_U(6,IY)%v)
        OpenAD_lin_44 = OpenAD_aux_33
        OpenAD_lin_46 = DY(IY)
        OpenAD_lin_45 = HU(6,IY)%v
        ZONAL_TRANSPORT%v = (ZONAL_TRANSPORT%v+HU(6,IY)%v*OpenAD_aux_33)
        OpenAD_acc_23 = (OpenAD_lin_46*OpenAD_lin_45)
        if (double_tape_size.lt.double_tape_pointer) then
        allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variable
     +_1)
        if (cp_loop_variable_1.gt.0) then
        print *,'allocation failed with',cp_loop_variable_1
        stop
        end if
        double_tmp_tape = double_tape
        deallocate(double_tape)
        allocate(double_tape(double_tape_size*2))
        print *,"DT+"
        double_tape(1:double_tape_size) = double_tmp_tape
        deallocate(double_tmp_tape)
        double_tape_size = double_tape_size*2
        end if
        double_tape(double_tape_pointer) = OpenAD_lin_44
        double_tape_pointer = double_tape_pointer+1
        if (double_tape_size.lt.double_tape_pointer) then
        allocate(double_tmp_tape(double_tape_size),STAT=cp_loop_variable
     +_1)
        if (cp_loop_variable_1.gt.0) then
        print *,'allocation failed with',cp_loop_variable_1
        stop
        end if
        double_tmp_tape = double_tape
        deallocate(double_tape)
        allocate(double_tape(double_tape_size*2))
        print *,"DT+"
        double_tape(1:double_tape_size) = double_tmp_tape
        deallocate(double_tmp_tape)
        double_tape_size = double_tape_size*2
        end if
        double_tape(double_tape_pointer) = OpenAD_acc_23
        double_tape_pointer = double_tape_pointer+1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," calc_zonal_transpo
     +rt_joint: entering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        double_tape_pointer = double_tape_pointer-1
        OpenAD_Symbol_1377 = double_tape(double_tape_pointer)
        double_tape_pointer = double_tape_pointer-1
        OpenAD_Symbol_1378 = double_tape(double_tape_pointer)
        LOCAL_U(6,IY)%d = LOCAL_U(6,IY)%d+ZONAL_TRANSPORT%d*(OpenAD_Symb
     +ol_1377)
        HU(6,IY)%d = HU(6,IY)%d+ZONAL_TRANSPORT%d*(OpenAD_Symbol_1378)
        OpenAD_prp_19%d = OpenAD_prp_19%d+ZONAL_TRANSPORT%d
        ZONAL_TRANSPORT%d = 0.0d0
        ZONAL_TRANSPORT%d = ZONAL_TRANSPORT%d+OpenAD_prp_19%d
        OpenAD_prp_19%d = 0.0d0
        IY = IY-1
      END DO
      ZONAL_TRANSPORT%d = 0.0d0
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave calc_zonal_transport_joint:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave ca
     +lc_zonal_transport_joint:",our_rev_mode%arg_store,our_rev_mode%arg
     +_restore,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_m
     +ode%plain,our_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine calc_zonal_transport_joint

      SUBROUTINE smoothness_lapldepth(CF)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter sm
     +oothness_lapldepth:",our_rev_mode%arg_store,our_rev_mode%arg_resto
     +re,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%pl
     +ain,our_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter smoothness_lapldepth:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_lapldep
     +th: entering arg store"
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = XPERIODIC
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = YPERIODIC
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DX,1)-lbound(
     +DX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DX,1),ubound(DX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DX(cp_loop_variable_1)
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DY,1)-lbound(
     +DY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DY,1),ubound(DY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_LAPLDEPTH,1),ubound(WEIGHT_L
     +APLDEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_LAPLDE
     +PTH,2)-lbound(WEIGHT_LAPLDEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_LAPLDEPTH,2),ubound(WEIGHT_L
     +APLDEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_LAPLDEPTH(cp_loop_variable_1
     +,cp_loop_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_lapldep
     +th: entering arg restore"
      do cp_loop_variable_1 = ubound(WEIGHT_LAPLDEPTH,1),lbound(WEIGHT_L
     +APLDEPTH,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_LAPLDEPTH,2),lbound(WEIGHT_L
     +APLDEPTH,2),-1
      WEIGHT_LAPLDEPTH(cp_loop_variable_1,cp_loop_variable_2) = cp_doubl
     +e(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DY,1),lbound(DY,1),-1
      DY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DX,1),lbound(DX,1),-1
      DX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DX(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      YPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      XPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_lapldep
     +th: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO IX = 1, 19, 1
          DFDX(INT(IX),INT(IY)) = ((DEPTH(IX+1,IY)%v-DEPTH(IX,IY)%v)/DX(
     +IX+1))
        END DO
        IF(XPERIODIC) THEN
          DFDX(20,INT(IY)) = ((DEPTH(1,IY)%v-DEPTH(20,IY)%v)/DX(1))
        ELSE
          DFDX(20,INT(IY)) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1,20,1
        DO IY = 1,19,1
          DFDY(INT(IX),INT(IY)) = ((DEPTH(IX,IY+1)%v-DEPTH(IX,IY)%v)/DY(
     +IY+1))
        END DO
        IF(YPERIODIC) THEN
          DFDY(INT(IX),20) = ((DEPTH(IX,1)%v-DEPTH(IX,20)%v)/DY(1))
        ELSE
          DFDY(INT(IX),20) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        IF(XPERIODIC) THEN
          DDFDDX(1,INT(IY)) = ((DFDX(1,IY)-DFDX(20,IY))/DX(1))
        ELSE
          DDFDDX(1,INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2,20,1
          DDFDDX(INT(IX),INT(IY)) = ((DFDX(IX,IY)-DFDX(IX+(-1),IY))/DX(I
     +X))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1,20,1
        IF(YPERIODIC) THEN
          DDFDDY(INT(IX),1) = ((DFDY(IX,1)-DFDY(IX,20))/DY(1))
        ELSE
          DDFDDY(INT(IX),1) = 0.0D00
        ENDIF
        DO IY = 2,20,1
          DDFDDY(INT(IX),INT(IY)) = ((DFDY(IX,IY)-DFDY(IX,IY+(-1)))/DY(I
     +Y))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          LAPLDEPTH = (DDFDDX(IX,IY)+DDFDDY(IX,IY))
          CF = (CF+WEIGHT_LAPLDEPTH(IX,IY)*LAPLDEPTH*LAPLDEPTH*5.0D-01)
        END DO
      END DO
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_lapldep
     +th: entering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,19,1
          DFDX(INT(IX),INT(IY)) = ((DEPTH(IX+1,IY)%v-DEPTH(IX,IY)%v)/DX(
     +IX+1))
        END DO
        IF(XPERIODIC) THEN
          DFDX(20,INT(IY)) = ((DEPTH(1,IY)%v-DEPTH(20,IY)%v)/DX(1))
        ELSE
          DFDX(20,INT(IY)) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1,20,1
        DO IY = 1,19,1
          DFDY(INT(IX),INT(IY)) = ((DEPTH(IX,IY+1)%v-DEPTH(IX,IY)%v)/DY(
     +IY+1))
        END DO
        IF(YPERIODIC) THEN
          DFDY(INT(IX),20) = ((DEPTH(IX,1)%v-DEPTH(IX,20)%v)/DY(1))
        ELSE
          DFDY(INT(IX),20) = 0.0D00
        ENDIF
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        IF(XPERIODIC) THEN
          DDFDDX(1,INT(IY)) = ((DFDX(1,IY)-DFDX(20,IY))/DX(1))
        ELSE
          DDFDDX(1,INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2,20,1
          DDFDDX(INT(IX),INT(IY)) = ((DFDX(IX,IY)-DFDX(IX+(-1),IY))/DX(I
     +X))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1,20,1
        IF(YPERIODIC) THEN
          DDFDDY(INT(IX),1) = ((DFDY(IX,1)-DFDY(IX,20))/DY(1))
        ELSE
          DDFDDY(INT(IX),1) = 0.0D00
        ENDIF
        DO IY = 2,20,1
          DDFDDY(INT(IX),INT(IY)) = ((DFDY(IX,IY)-DFDY(IX,IY+(-1)))/DY(I
     +Y))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          LAPLDEPTH = (DDFDDX(IX,IY)+DDFDDY(IX,IY))
          CF = (CF+WEIGHT_LAPLDEPTH(IX,IY)*LAPLDEPTH*LAPLDEPTH*5.0D-01)
        END DO
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_lapldep
     +th: entering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      IX = 1+1*((20-1)/1)
      do while (IX.GE.1)
        IY = 2+1*((20-2)/1)
        do while (IY.GE.2)
          IY = IY-1
        END DO
        IF(YPERIODIC) THEN
        ENDIF
        IX = IX-1
      END DO
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 2+1*((20-2)/1)
        do while (IX.GE.2)
          IX = IX-1
        END DO
        IF(XPERIODIC) THEN
        ENDIF
        IY = IY-1
      END DO
      IX = 1+1*((20-1)/1)
      do while (IX.GE.1)
        IF(YPERIODIC) THEN
        ENDIF
        IY = 1+1*((19-1)/1)
        do while (IY.GE.1)
          IY = IY-1
        END DO
        IX = IX-1
      END DO
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IF(XPERIODIC) THEN
        ENDIF
        IX = 1+1*((19-1)/1)
        do while (IX.GE.1)
          IX = IX-1
        END DO
        IY = IY-1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave smoothness_lapldepth:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave sm
     +oothness_lapldepth:",our_rev_mode%arg_store,our_rev_mode%arg_resto
     +re,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%pl
     +ain,our_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine smoothness_lapldepth

      SUBROUTINE smoothness_graddepth(CF)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter sm
     +oothness_graddepth:",our_rev_mode%arg_store,our_rev_mode%arg_resto
     +re,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%pl
     +ain,our_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter smoothness_graddepth:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_graddep
     +th: entering arg store"
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = XPERIODIC
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = YPERIODIC
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DX,1)-lbound(
     +DX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DX,1),ubound(DX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DX(cp_loop_variable_1)
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DY,1)-lbound(
     +DY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DY,1),ubound(DY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(UMASK,1),ubound(UMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UMASK,2)-lbou
     +nd(UMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UMASK,2),ubound(UMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VMASK,1),ubound(VMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VMASK,2)-lbou
     +nd(VMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VMASK,2),ubound(VMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_GRADDEPTH,1),ubound(WEIGHT_G
     +RADDEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_GRADDE
     +PTH,2)-lbound(WEIGHT_GRADDEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_GRADDEPTH,2),ubound(WEIGHT_G
     +RADDEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_GRADDEPTH(cp_loop_variable_1
     +,cp_loop_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_graddep
     +th: entering arg restore"
      do cp_loop_variable_1 = ubound(WEIGHT_GRADDEPTH,1),lbound(WEIGHT_G
     +RADDEPTH,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_GRADDEPTH,2),lbound(WEIGHT_G
     +RADDEPTH,2),-1
      WEIGHT_GRADDEPTH(cp_loop_variable_1,cp_loop_variable_2) = cp_doubl
     +e(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VMASK,1),lbound(VMASK,1),-1
      do cp_loop_variable_2 = ubound(VMASK,2),lbound(VMASK,2),-1
      VMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UMASK,1),lbound(UMASK,1),-1
      do cp_loop_variable_2 = ubound(UMASK,2),lbound(UMASK,2),-1
      UMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DY,1),lbound(DY,1),-1
      DY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DX,1),lbound(DX,1),-1
      DX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DX(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      YPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      XPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_graddep
     +th: entering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        IF(XPERIODIC) THEN
          DFDX(1,INT(IY)) = ((DEPTH(1,IY)%v-DEPTH(20,IY)%v)/DX(1))
        ELSE
          DFDX(1,INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2,20,1
          DFDX(INT(IX),INT(IY)) = ((DEPTH(IX,IY)%v-DEPTH(IX+(-1),IY)%v)/
     +DX(IX))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1,20,1
        IF(YPERIODIC) THEN
          DFDY(INT(IX),1) = ((DEPTH(IX,1)%v-DEPTH(IX,20)%v)/DY(1))
        ELSE
          DFDY(INT(IX),1) = 0.0D00
        ENDIF
        DO IY = 2,20,1
          DFDY(INT(IX),INT(IY)) = ((DEPTH(IX,IY)%v-DEPTH(IX,IY+(-1))%v)/
     +DY(IY))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          CF = (CF+WEIGHT_GRADDEPTH(IX,IY)*5.0D-01*(UMASK(IX,IY)*DFDX(IX
     +,IY)*DFDX(IX,IY)*5.0D-01+VMASK(IX,IY)*DFDY(IX,IY)*DFDY(IX,IY)*5.0D
     +-01))
        END DO
      END DO
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_graddep
     +th: entering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        IF(XPERIODIC) THEN
          DFDX(1,INT(IY)) = ((DEPTH(1,IY)%v-DEPTH(20,IY)%v)/DX(1))
        ELSE
          DFDX(1,INT(IY)) = 0.0D00
        ENDIF
        DO IX = 2,20,1
          DFDX(INT(IX),INT(IY)) = ((DEPTH(IX,IY)%v-DEPTH(IX+(-1),IY)%v)/
     +DX(IX))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IX = 1,20,1
        IF(YPERIODIC) THEN
          DFDY(INT(IX),1) = ((DEPTH(IX,1)%v-DEPTH(IX,20)%v)/DY(1))
        ELSE
          DFDY(INT(IX),1) = 0.0D00
        ENDIF
        DO IY = 2,20,1
          DFDY(INT(IX),INT(IY)) = ((DEPTH(IX,IY)%v-DEPTH(IX,IY+(-1))%v)/
     +DY(IY))
        END DO
      END DO
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO IX = 1,20,1
          CF = (CF+WEIGHT_GRADDEPTH(IX,IY)*5.0D-01*(UMASK(IX,IY)*DFDX(IX
     +,IY)*DFDX(IX,IY)*5.0D-01+VMASK(IX,IY)*DFDY(IX,IY)*DFDY(IX,IY)*5.0D
     +-01))
        END DO
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," smoothness_graddep
     +th: entering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 1+1*((20-1)/1)
        do while (IX.GE.1)
          IX = IX-1
        END DO
        IY = IY-1
      END DO
      IX = 1+1*((20-1)/1)
      do while (IX.GE.1)
        IY = 2+1*((20-2)/1)
        do while (IY.GE.2)
          IY = IY-1
        END DO
        IF(YPERIODIC) THEN
        ENDIF
        IX = IX-1
      END DO
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        IX = 2+1*((20-2)/1)
        do while (IX.GE.2)
          IX = IX-1
        END DO
        IF(XPERIODIC) THEN
        ENDIF
        IY = IY-1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave smoothness_graddepth:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave sm
     +oothness_graddepth:",our_rev_mode%arg_store,our_rev_mode%arg_resto
     +re,our_rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%pl
     +ain,our_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine smoothness_graddepth

      SUBROUTINE cost_function(TIME, CF, LOCAL_U, LOCAL_V, LOCAL_ETA)
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      REAL(w2f__8) LOCAL_V(0 : 21, 0 : 21)
      INTENT(IN) LOCAL_V
      REAL(w2f__8) LOCAL_ETA(0 : 21, 0 : 21)
      INTENT(IN) LOCAL_ETA
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$      our_indent=our_indent+1
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter cost_function:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$      write(*,'(A,A,A,I8,A,I8)')
C$$$     +     indentation(1:our_indent), "enter cost_function:",
C$$$     +     " DT:",double_tape_pointer, 
C$$$     +     " IT:",integer_tape_pointer


      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " cost_function: entering plain"
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL is_eta_data_time(TIME,RESULT)
      IF(RESULT) THEN
        CALL read_data(TIME)
      ENDIF
      CALL is_eta_data_time(TIME,RESULT)
      IF((NEDT .eq.(-1)) .OR. RESULT) THEN
C!! requested inline of 'oad_convert' has no defn
        CALL oad_convert(OpenAD_tyc_2,LOCAL_U)
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT,OpenAD_tyc_2)
        CF = (CF+WEIGHT_ZONAL_TRANSPORT*((ZONAL_TRANSPORT-ZONAL_TRANSPOR
     +T_DATA)**2)*5.0D-01)
        DO IY = 1,20,1
          DO IX = 1,20,1
            CF = (CF+WEIGHT_ETA(IX,IY)*((LOCAL_ETA(IX,IY)-ETA_DATA(IX,IY
     +))**2)*5.0D-01)
            CF = (CF+WEIGHT_U(IX,IY)*((LOCAL_U(IX,IY)%v-U_DATA(IX,IY))**
     +2)*5.0D-01)
            CF = (CF+WEIGHT_V(IX,IY)*((LOCAL_V(IX,IY)-V_DATA(IX,IY))**2)
     +*5.0D-01)
          END DO
        END DO
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " cost_function: entering tape"
C$OPENAD XXX Template OADrts/ad_template.split.f
      CALL is_eta_data_time(TIME,RESULT)
      IF(RESULT) THEN
        CALL read_data(TIME)
        OpenAD_Symbol_1208 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1208
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1209 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1209
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      CALL is_eta_data_time(TIME,RESULT)
      IF ((NEDT.eq.(-1)).OR.RESULT) THEN
C!! requested inline of 'oad_convert' has no defn
        CALL oad_convert(OpenAD_tyc_2,LOCAL_U)
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT,OpenAD_tyc_2)
        CF = (CF+WEIGHT_ZONAL_TRANSPORT*((ZONAL_TRANSPORT-ZONAL_TRANSPOR
     +T_DATA)**2)*5.0D-01)
        OpenAD_Symbol_1210 = 0_w2f__i8
        DO IY = 1,20,1
          OpenAD_Symbol_1211 = 0_w2f__i8
          DO IX = 1,20,1
            CF = (CF+WEIGHT_ETA(IX,IY)*((LOCAL_ETA(IX,IY)-ETA_DATA(IX,IY
     +))**2)*5.0D-01)
            CF = (CF+WEIGHT_U(IX,IY)*((LOCAL_U(IX,IY)%v-U_DATA(IX,IY))**
     +2)*5.0D-01)
            CF = (CF+WEIGHT_V(IX,IY)*((LOCAL_V(IX,IY)-V_DATA(IX,IY))**2)
     +*5.0D-01)
            OpenAD_Symbol_1211 = (INT(OpenAD_Symbol_1211)+INT(1_w2f__i8)
     +)
          END DO
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_1211
          integer_tape_pointer = integer_tape_pointer+1
          OpenAD_Symbol_1210 = (INT(OpenAD_Symbol_1210)+INT(1_w2f__i8))
        END DO
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1210
        integer_tape_pointer = integer_tape_pointer+1
        OpenAD_Symbol_1213 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1213
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1212 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1212
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      end if
      if (our_rev_mode%adjoint) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " cost_function: entering adjoint"
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1202 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1202.ne.0) THEN
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_1203 = integer_tape(integer_tape_pointer)
        OpenAD_Symbol_1204 = 1
        do while (INT(OpenAD_Symbol_1204).LE.INT(OpenAD_Symbol_1203))
          integer_tape_pointer = integer_tape_pointer-1
          OpenAD_Symbol_1205 = integer_tape(integer_tape_pointer)
          OpenAD_Symbol_1206 = 1
          do while (INT(OpenAD_Symbol_1206).LE.INT(OpenAD_Symbol_1205))
            OpenAD_Symbol_1206 = INT(OpenAD_Symbol_1206)+1
          END DO
          OpenAD_Symbol_1204 = INT(OpenAD_Symbol_1204)+1
        END DO
        CALL calc_zonal_transport_split(ZONAL_TRANSPORT,OpenAD_tyc_9)
      ENDIF
      CALL is_eta_data_time(TIME,RESULT)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1207 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1207.ne.0) THEN
        CALL read_data(TIME)
      ENDIF
      CALL is_eta_data_time(TIME,RESULT)
      end if
      end subroutine cost_function

      SUBROUTINE cost_depth(CF)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter co
     +st_depth:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_rev
     +_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our_re
     +v_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter cost_depth:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," cost_depth: enteri
     +ng arg store"
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = CF
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(DEPTH_DATA,1),ubound(DEPTH_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH_DATA,2)
     +-lbound(DEPTH_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH_DATA,2),ubound(DEPTH_DATA,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH_DATA(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," cost_depth: enteri
     +ng arg restore"
      do cp_loop_variable_1 = ubound(DEPTH_DATA,1),lbound(DEPTH_DATA,1),
     +-1
      do cp_loop_variable_2 = ubound(DEPTH_DATA,2),lbound(DEPTH_DATA,2),
     +-1
      DEPTH_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      CF = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," cost_depth: enteri
     +ng plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1, 20, 1
        DO JY = 1, 20, 1
          DO IX = 1, 20, 1
            DO JX = 1, 20, 1
              CF = (CF+(DEPTH(IX,IY)%v-DEPTH_DATA(IX,IY))*5.0D-01*(DEPTH
     +(JX,JY)%v-DEPTH_DATA(JX,JY)))
            END DO
          END DO
        END DO
      END DO
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," cost_depth: enteri
     +ng tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint.f
C$OPENAD XXX Simple loop
      DO IY = 1,20,1
        DO JY = 1,20,1
          DO IX = 1,20,1
            DO JX = 1,20,1
              CF = (CF+(DEPTH(IX,IY)%v-DEPTH_DATA(IX,IY))*5.0D-01*(DEPTH
     +(JX,JY)%v-DEPTH_DATA(JX,JY)))
            END DO
          END DO
        END DO
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," cost_depth: enteri
     +ng adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      IY = 1+1*((20-1)/1)
      do while (IY.GE.1)
        JY = 1+1*((20-1)/1)
        do while (JY.GE.1)
          IX = 1+1*((20-1)/1)
          do while (IX.GE.1)
            JX = 1+1*((20-1)/1)
            do while (JX.GE.1)
              JX = JX-1
            END DO
            IX = IX-1
          END DO
          JY = JY-1
        END DO
        IY = IY-1
      END DO
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave cost_depth:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave co
     +st_depth:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_rev
     +_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our_re
     +v_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine cost_depth

      SUBROUTINE loop_body_wrapper_inner(LOCAL_U, LOCAL_V, LOCAL_ETA, CO
     +ST, CALC_COST, TIME, TIME_INDEX, NIO)
      use OAD_cp
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      type(active) :: LOCAL_V(0:21,0:21)
      type(active) :: LOCAL_ETA(0:21,0:21)
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$
C$$$      our_indent=our_indent+1
C$$$
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter loop_body_wrapper_inner:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter loop_body_wrapper_inner:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_inner: entering 
C arg store"
      do cp_loop_variable_1 = lbound(LOCAL_U,1),ubound(LOCAL_U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_U,2)-lb
     +ound(LOCAL_U,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_U,2),ubound(LOCAL_U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_U(cp_loop_variable_1,cp_loop_
     +variable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(LOCAL_V,1),ubound(LOCAL_V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_V,2)-lb
     +ound(LOCAL_V,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_V,2),ubound(LOCAL_V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_V(cp_loop_variable_1,cp_loop_
     +variable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(LOCAL_ETA,1),ubound(LOCAL_ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_ETA,2)-
     +lbound(LOCAL_ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_ETA,2),ubound(LOCAL_ETA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_ETA(cp_loop_variable_1,cp_loo
     +p_variable_2)%v
      end do
      end do
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = COST
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = TIME
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = TIME_INDEX
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DT
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DT_DUMP
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = FULLIO
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NTSPINUP
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = SUPPRESSIO
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = XPERIODIC
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = YPERIODIC
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = RY
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NEDT
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ZONAL_TRANSPORT_DATA
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_ZONAL_TRANSPORT
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = CALC_COST
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NIO
      do while (cp_double_size.lt.cp_double_pointer+ubound(DX,1)-lbound(
     +DX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DX,1),ubound(DX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DX(cp_loop_variable_1)
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DY,1)-lbound(
     +DY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DY,1),ubound(DY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FCORIU,1),ubound(FCORIU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FCORIU,2)-lbo
     +und(FCORIU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FCORIU,2),ubound(FCORIU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FCORIU(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FCORIV,1),ubound(FCORIV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FCORIV,2)-lbo
     +und(FCORIV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FCORIV,2),ubound(FCORIV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FCORIV(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FRICT,1),ubound(FRICT,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FRICT,2)-lbou
     +nd(FRICT,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FRICT,2),ubound(FRICT,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FRICT(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(HU,1),ubound(HU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HU,2)-lbound(
     +HU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HU,2),ubound(HU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HU(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(HV,1),ubound(HV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HV,2)-lbound(
     +HV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HV,2),ubound(HV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HV(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(HY,1)-lbound(
     +HY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(HY,1),ubound(HY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(INVHU,1),ubound(INVHU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(INVHU,2)-lbou
     +nd(INVHU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(INVHU,2),ubound(INVHU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = INVHU(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(INVHV,1),ubound(INVHV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(INVHV,2)-lbou
     +nd(INVHV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(INVHV,2),ubound(INVHV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = INVHV(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(RX,1)-lbound(
     +RX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(RX,1),ubound(RX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = RX(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(UMASK,1),ubound(UMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UMASK,2)-lbou
     +nd(UMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UMASK,2),ubound(UMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VMASK,1),ubound(VMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VMASK,2)-lbou
     +nd(VMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VMASK,2),ubound(VMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UFORCE,1),ubound(UFORCE,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UFORCE,2)-lbo
     +und(UFORCE,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UFORCE,2),ubound(UFORCE,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UFORCE(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VFORCE,1),ubound(VFORCE,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VFORCE,2)-lbo
     +und(VFORCE,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VFORCE,2),ubound(VFORCE,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VFORCE(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(ETA_DATA,1),ubound(ETA_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA_DATA,2)-l
     +bound(ETA_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETA_DATA,2),ubound(ETA_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA_DATA(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA_DATA_TIME
     +,1)-lbound(ETA_DATA_TIME,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(ETA_DATA_TIME,1),ubound(ETA_DATA_TI
     +ME,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA_DATA_TIME(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(U_DATA,1),ubound(U_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(U_DATA,2)-lbo
     +und(U_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(U_DATA,2),ubound(U_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = U_DATA(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(V_DATA,1),ubound(V_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(V_DATA,2)-lbo
     +und(V_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(V_DATA,2),ubound(V_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = V_DATA(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_ETA,1),ubound(WEIGHT_ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_ETA,2)
     +-lbound(WEIGHT_ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_ETA,2),ubound(WEIGHT_ETA,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_ETA(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_U,1),ubound(WEIGHT_U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_U,2)-l
     +bound(WEIGHT_U,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_U,2),ubound(WEIGHT_U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_U(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_V,1),ubound(WEIGHT_V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_V,2)-l
     +bound(WEIGHT_V,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_V,2),ubound(WEIGHT_V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_V(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_inner: entering 
C arg restore"
      do cp_loop_variable_1 = ubound(WEIGHT_V,1),lbound(WEIGHT_V,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_V,2),lbound(WEIGHT_V,2),-1
      WEIGHT_V(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(WEIGHT_U,1),lbound(WEIGHT_U,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_U,2),lbound(WEIGHT_U,2),-1
      WEIGHT_U(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(WEIGHT_ETA,1),lbound(WEIGHT_ETA,1),
     +-1
      do cp_loop_variable_2 = ubound(WEIGHT_ETA,2),lbound(WEIGHT_ETA,2),
     +-1
      WEIGHT_ETA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(V_DATA,1),lbound(V_DATA,1),-1
      do cp_loop_variable_2 = ubound(V_DATA,2),lbound(V_DATA,2),-1
      V_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(U_DATA,1),lbound(U_DATA,1),-1
      do cp_loop_variable_2 = ubound(U_DATA,2),lbound(U_DATA,2),-1
      U_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETA_DATA_TIME,1),lbound(ETA_DATA_TI
     +ME,1),-1
      ETA_DATA_TIME(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +ETA_DATA_TIME(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(ETA_DATA,1),lbound(ETA_DATA,1),-1
      do cp_loop_variable_2 = ubound(ETA_DATA,2),lbound(ETA_DATA,2),-1
      ETA_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VFORCE,1),lbound(VFORCE,1),-1
      do cp_loop_variable_2 = ubound(VFORCE,2),lbound(VFORCE,2),-1
      VFORCE(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UFORCE,1),lbound(UFORCE,1),-1
      do cp_loop_variable_2 = ubound(UFORCE,2),lbound(UFORCE,2),-1
      UFORCE(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VMASK,1),lbound(VMASK,1),-1
      do cp_loop_variable_2 = ubound(VMASK,2),lbound(VMASK,2),-1
      VMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UMASK,1),lbound(UMASK,1),-1
      do cp_loop_variable_2 = ubound(UMASK,2),lbound(UMASK,2),-1
      UMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(RX,1),lbound(RX,1),-1
      RX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +RX(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(INVHV,1),lbound(INVHV,1),-1
      do cp_loop_variable_2 = ubound(INVHV,2),lbound(INVHV,2),-1
      INVHV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(INVHU,1),lbound(INVHU,1),-1
      do cp_loop_variable_2 = ubound(INVHU,2),lbound(INVHU,2),-1
      INVHU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HY,1),lbound(HY,1),-1
      HY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +HY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(HV,1),lbound(HV,1),-1
      do cp_loop_variable_2 = ubound(HV,2),lbound(HV,2),-1
      HV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HU,1),lbound(HU,1),-1
      do cp_loop_variable_2 = ubound(HU,2),lbound(HU,2),-1
      HU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FRICT,1),lbound(FRICT,1),-1
      do cp_loop_variable_2 = ubound(FRICT,2),lbound(FRICT,2),-1
      FRICT(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FCORIV,1),lbound(FCORIV,1),-1
      do cp_loop_variable_2 = ubound(FCORIV,2),lbound(FCORIV,2),-1
      FCORIV(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FCORIU,1),lbound(FCORIU,1),-1
      do cp_loop_variable_2 = ubound(FCORIU,2),lbound(FCORIU,2),-1
      FCORIU(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DY,1),lbound(DY,1),-1
      DY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DX,1),lbound(DX,1),-1
      DX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DX(cp_loop_variable_1)%v
      end do
      NIO = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      CALC_COST = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      WEIGHT_ZONAL_TRANSPORT = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      ZONAL_TRANSPORT_DATA = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      NEDT = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      RY = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      YPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      XPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      SUPPRESSIO = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      NTSPINUP = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      FULLIO = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      DT_DUMP = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      DT = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      TIME_INDEX = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      TIME = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      COST = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      do cp_loop_variable_1 = ubound(LOCAL_ETA,1),lbound(LOCAL_ETA,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_ETA,2),lbound(LOCAL_ETA,2),-1
      LOCAL_ETA(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_
     +double_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(LOCAL_V,1),lbound(LOCAL_V,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_V,2),lbound(LOCAL_V,2),-1
      LOCAL_V(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_do
     +uble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(LOCAL_U,1),lbound(LOCAL_U,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_U,2),lbound(LOCAL_U,2),-1
      LOCAL_U(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_do
     +uble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      end if
      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_inner: entering 
C plain"
C$OPENAD XXX Template OADrts/ad_template.joint_split_iif.f
      CALL time_step(TIME_INDEX,LOCAL_U,LOCAL_V,LOCAL_ETA)
      IF ((TIME_INDEX.GT.NTSPINUP).AND.CALC_COST) THEN
C!! requested inline of 'oad_convert' has no defn
        CALL oad_convert(OpenAD_tyc_3,LOCAL_V)
C!! requested inline of 'oad_convert' has no defn
        CALL oad_convert(OpenAD_tyc_4,LOCAL_ETA)
        CALL cost_function(TIME,COST,LOCAL_U,OpenAD_tyc_3,OpenAD_tyc_4)
      ENDIF
      IF ((MOD(DT*REAL(TIME_INDEX),DT_DUMP).eq.0.0D00).AND.(FULLIO.AND.(
     +.not. SUPPRESSIO))) THEN
        NIO = (NIO+1)
        WRITE(*,*) 'Writing Time Step ',TIME_INDEX
      ENDIF
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_inner: entering 
C tape"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint_split_iif.f
      CALL time_step(TIME_INDEX,LOCAL_U,LOCAL_V,LOCAL_ETA)
      IF ((TIME_INDEX.GT.NTSPINUP).AND.CALC_COST) THEN
C!! requested inline of 'oad_convert' has no defn
        CALL oad_convert(OpenAD_tyc_3,LOCAL_V)
C!! requested inline of 'oad_convert' has no defn
        CALL oad_convert(OpenAD_tyc_4,LOCAL_ETA)
        CALL cost_function(TIME,COST,LOCAL_U,OpenAD_tyc_3,OpenAD_tyc_4)
        OpenAD_Symbol_1240 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1240
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1241 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1241
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF ((MOD(DT*REAL(TIME_INDEX),DT_DUMP).eq.0.0D00).AND.(FULLIO.AND.(
     +.not. SUPPRESSIO))) THEN
        NIO = (NIO+1)
        WRITE(*,*) 'Writing Time Step ',TIME_INDEX
        OpenAD_Symbol_1242 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1242
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1243 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1243
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF

C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_inner: following
C  with adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1238 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1238.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1239 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1239.ne.0) THEN
        CALL cost_function(TIME,COST,LOCAL_U,OpenAD_tyc_11,OpenAD_tyc_12
     +)
      ENDIF
      CALL time_step(TIME_INDEX,LOCAL_U,LOCAL_V,LOCAL_ETA)
         our_rev_mode=our_orig_mode
      end if
      end subroutine loop_body_wrapper_inner

      SUBROUTINE loop_body_wrapper_outer(LOCAL_U, LOCAL_V, LOCAL_ETA, CO
     +ST, CALC_COST, TIME, TIME_INDEX, NIO, IT, NTOTAL, NINNER)
      use OAD_cp
      use OAD_tape
      use OAD_rev

      use OAD_active
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
      type(active) :: LOCAL_U(0:21,0:21)
      type(active) :: LOCAL_V(0:21,0:21)
      type(active) :: LOCAL_ETA(0:21,0:21)
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


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer :: special_counter=0

      integer iaddr
      external iaddr
C
C     **** Statements ****
C

C$$$      character*(80):: indentation='                                 
C        
C$$$     +                                         '
C$$$
C$$$      our_indent=our_indent+1
C$$$
C$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
C$$$     +indentation(1:our_indent), "enter loop_body_wrapper_outer:",
C$$$     +our_rev_mode%arg_store,
C$$$     +our_rev_mode%arg_restore,
C$$$     +our_rev_mode%res_store,
C$$$     +our_rev_mode%res_restore,
C$$$     +our_rev_mode%plain,
C$$$     +our_rev_mode%tape,
C$$$     +our_rev_mode%adjoint
C$$$
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter loop_body_wrapper_outer:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer

      if (our_rev_mode%arg_store) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_outer: entering 
C arg store"
      do cp_loop_variable_1 = lbound(LOCAL_U,1),ubound(LOCAL_U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_U,2)-lb
     +ound(LOCAL_U,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_U,2),ubound(LOCAL_U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_U(cp_loop_variable_1,cp_loop_
     +variable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(LOCAL_V,1),ubound(LOCAL_V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_V,2)-lb
     +ound(LOCAL_V,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_V,2),ubound(LOCAL_V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_V(cp_loop_variable_1,cp_loop_
     +variable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(LOCAL_ETA,1),ubound(LOCAL_ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(LOCAL_ETA,2)-
     +lbound(LOCAL_ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(LOCAL_ETA,2),ubound(LOCAL_ETA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = LOCAL_ETA(cp_loop_variable_1,cp_loo
     +p_variable_2)%v
      end do
      end do
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = COST
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = CALC_COST
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = TIME
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = TIME_INDEX
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NIO
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DT
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DT_DUMP
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = FULLIO
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NTSPINUP
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = START_TIME
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = SUPPRESSIO
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = XPERIODIC
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = YPERIODIC
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = RY
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NEDT
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ZONAL_TRANSPORT_DATA
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_ZONAL_TRANSPORT
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = IT
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NTOTAL
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NINNER
      do while (cp_double_size.lt.cp_double_pointer+ubound(DX,1)-lbound(
     +DX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DX,1),ubound(DX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DX(cp_loop_variable_1)
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DY,1)-lbound(
     +DY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DY,1),ubound(DY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FCORIU,1),ubound(FCORIU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FCORIU,2)-lbo
     +und(FCORIU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FCORIU,2),ubound(FCORIU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FCORIU(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FCORIV,1),ubound(FCORIV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FCORIV,2)-lbo
     +und(FCORIV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FCORIV,2),ubound(FCORIV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FCORIV(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FRICT,1),ubound(FRICT,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FRICT,2)-lbou
     +nd(FRICT,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FRICT,2),ubound(FRICT,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FRICT(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(HU,1),ubound(HU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HU,2)-lbound(
     +HU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HU,2),ubound(HU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HU(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(HV,1),ubound(HV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HV,2)-lbound(
     +HV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HV,2),ubound(HV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HV(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(HY,1)-lbound(
     +HY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(HY,1),ubound(HY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(INVHU,1),ubound(INVHU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(INVHU,2)-lbou
     +nd(INVHU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(INVHU,2),ubound(INVHU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = INVHU(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(INVHV,1),ubound(INVHV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(INVHV,2)-lbou
     +nd(INVHV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(INVHV,2),ubound(INVHV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = INVHV(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(RX,1)-lbound(
     +RX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(RX,1),ubound(RX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = RX(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(UMASK,1),ubound(UMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UMASK,2)-lbou
     +nd(UMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UMASK,2),ubound(UMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VMASK,1),ubound(VMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VMASK,2)-lbou
     +nd(VMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VMASK,2),ubound(VMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UFORCE,1),ubound(UFORCE,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UFORCE,2)-lbo
     +und(UFORCE,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UFORCE,2),ubound(UFORCE,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UFORCE(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VFORCE,1),ubound(VFORCE,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VFORCE,2)-lbo
     +und(VFORCE,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VFORCE,2),ubound(VFORCE,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VFORCE(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(ETA_DATA,1),ubound(ETA_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA_DATA,2)-l
     +bound(ETA_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETA_DATA,2),ubound(ETA_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA_DATA(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA_DATA_TIME
     +,1)-lbound(ETA_DATA_TIME,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(ETA_DATA_TIME,1),ubound(ETA_DATA_TI
     +ME,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA_DATA_TIME(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(U_DATA,1),ubound(U_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(U_DATA,2)-lbo
     +und(U_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(U_DATA,2),ubound(U_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = U_DATA(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(V_DATA,1),ubound(V_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(V_DATA,2)-lbo
     +und(V_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(V_DATA,2),ubound(V_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = V_DATA(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_ETA,1),ubound(WEIGHT_ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_ETA,2)
     +-lbound(WEIGHT_ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_ETA,2),ubound(WEIGHT_ETA,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_ETA(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_U,1),ubound(WEIGHT_U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_U,2)-l
     +bound(WEIGHT_U,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_U,2),ubound(WEIGHT_U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_U(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_V,1),ubound(WEIGHT_V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_V,2)-l
     +bound(WEIGHT_V,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_V,2),ubound(WEIGHT_V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_V(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_outer: entering 
C arg restore"
      do cp_loop_variable_1 = ubound(WEIGHT_V,1),lbound(WEIGHT_V,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_V,2),lbound(WEIGHT_V,2),-1
      WEIGHT_V(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(WEIGHT_U,1),lbound(WEIGHT_U,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_U,2),lbound(WEIGHT_U,2),-1
      WEIGHT_U(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(WEIGHT_ETA,1),lbound(WEIGHT_ETA,1),
     +-1
      do cp_loop_variable_2 = ubound(WEIGHT_ETA,2),lbound(WEIGHT_ETA,2),
     +-1
      WEIGHT_ETA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(V_DATA,1),lbound(V_DATA,1),-1
      do cp_loop_variable_2 = ubound(V_DATA,2),lbound(V_DATA,2),-1
      V_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(U_DATA,1),lbound(U_DATA,1),-1
      do cp_loop_variable_2 = ubound(U_DATA,2),lbound(U_DATA,2),-1
      U_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETA_DATA_TIME,1),lbound(ETA_DATA_TI
     +ME,1),-1
      ETA_DATA_TIME(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +ETA_DATA_TIME(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(ETA_DATA,1),lbound(ETA_DATA,1),-1
      do cp_loop_variable_2 = ubound(ETA_DATA,2),lbound(ETA_DATA,2),-1
      ETA_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VFORCE,1),lbound(VFORCE,1),-1
      do cp_loop_variable_2 = ubound(VFORCE,2),lbound(VFORCE,2),-1
      VFORCE(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UFORCE,1),lbound(UFORCE,1),-1
      do cp_loop_variable_2 = ubound(UFORCE,2),lbound(UFORCE,2),-1
      UFORCE(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VMASK,1),lbound(VMASK,1),-1
      do cp_loop_variable_2 = ubound(VMASK,2),lbound(VMASK,2),-1
      VMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UMASK,1),lbound(UMASK,1),-1
      do cp_loop_variable_2 = ubound(UMASK,2),lbound(UMASK,2),-1
      UMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(RX,1),lbound(RX,1),-1
      RX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +RX(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(INVHV,1),lbound(INVHV,1),-1
      do cp_loop_variable_2 = ubound(INVHV,2),lbound(INVHV,2),-1
      INVHV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(INVHU,1),lbound(INVHU,1),-1
      do cp_loop_variable_2 = ubound(INVHU,2),lbound(INVHU,2),-1
      INVHU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HY,1),lbound(HY,1),-1
      HY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +HY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(HV,1),lbound(HV,1),-1
      do cp_loop_variable_2 = ubound(HV,2),lbound(HV,2),-1
      HV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HU,1),lbound(HU,1),-1
      do cp_loop_variable_2 = ubound(HU,2),lbound(HU,2),-1
      HU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FRICT,1),lbound(FRICT,1),-1
      do cp_loop_variable_2 = ubound(FRICT,2),lbound(FRICT,2),-1
      FRICT(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FCORIV,1),lbound(FCORIV,1),-1
      do cp_loop_variable_2 = ubound(FCORIV,2),lbound(FCORIV,2),-1
      FCORIV(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FCORIU,1),lbound(FCORIU,1),-1
      do cp_loop_variable_2 = ubound(FCORIU,2),lbound(FCORIU,2),-1
      FCORIU(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DY,1),lbound(DY,1),-1
      DY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DX,1),lbound(DX,1),-1
      DX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DX(cp_loop_variable_1)%v
      end do
      NINNER = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      NTOTAL = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      IT = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      WEIGHT_ZONAL_TRANSPORT = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      ZONAL_TRANSPORT_DATA = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      NEDT = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      RY = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      YPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      XPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      SUPPRESSIO = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      START_TIME = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      NTSPINUP = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      FULLIO = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      DT_DUMP = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      DT = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      NIO = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      TIME_INDEX = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      TIME = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      CALC_COST = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      COST = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      do cp_loop_variable_1 = ubound(LOCAL_ETA,1),lbound(LOCAL_ETA,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_ETA,2),lbound(LOCAL_ETA,2),-1
      LOCAL_ETA(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_
     +double_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(LOCAL_V,1),lbound(LOCAL_V,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_V,2),lbound(LOCAL_V,2),-1
      LOCAL_V(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_do
     +uble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(LOCAL_U,1),lbound(LOCAL_U,1),-1
      do cp_loop_variable_2 = ubound(LOCAL_U,2),lbound(LOCAL_U,2),-1
      LOCAL_U(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_do
     +uble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      end if
      if (our_rev_mode%plain) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_outer: entering 
C plain"
         special_counter=special_counter+1
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint_split_oif.f
      DO JT = 1, NINNER, 1
        TIME_INDEX = (JT + NINNER *(IT +(-1)))
        TIME = (START_TIME + TIME_INDEX * DT)
        IF(TIME_INDEX .LE. NTOTAL) THEN
          CALL loop_body_wrapper_inner(LOCAL_U,LOCAL_V,LOCAL_ETA,COST,CA
     +LC_COST,TIME,TIME_INDEX,NIO)
        ENDIF
      END DO
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_outer: entering 
C tape"
         write(*,'(A,I16,A)') " outer loop ", special_counter, " taping"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
C$OPENAD XXX Template OADrts/ad_template.joint_split_oif.f
      OpenAD_Symbol_1253 = 0_w2f__i8
      DO JT = 1,NINNER,1
        TIME_INDEX = (JT+NINNER*(IT+(-1)))
        TIME = (START_TIME+TIME_INDEX*DT)
        IF (TIME_INDEX.LE.NTOTAL) THEN
          CALL loop_body_wrapper_inner(LOCAL_U,LOCAL_V,LOCAL_ETA,COST,CA
     +LC_COST,TIME,TIME_INDEX,NIO)
          OpenAD_Symbol_1254 = 1_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_1254
          integer_tape_pointer = integer_tape_pointer+1
        ELSE
          OpenAD_Symbol_1255 = 0_w2f__i8
          if (integer_tape_size.lt.integer_tape_pointer) then
          allocate(integer_tmp_tape(integer_tape_size))
          integer_tmp_tape = integer_tape
          deallocate(integer_tape)
          allocate(integer_tape(integer_tape_size*2))
          print *,"IT+"
          integer_tape(1:integer_tape_size) = integer_tmp_tape
          deallocate(integer_tmp_tape)
          integer_tape_size = integer_tape_size*2
          end if
          integer_tape(integer_tape_pointer) = OpenAD_Symbol_1255
          integer_tape_pointer = integer_tape_pointer+1
        ENDIF
        OpenAD_Symbol_1253 = (INT(OpenAD_Symbol_1253)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_1253
      integer_tape_pointer = integer_tape_pointer+1

C$$$         write(*,'(A,A)') 
C$$$     +indentation(1:our_indent), " loop_body_wrapper_outer: following
C  with adjoint"
         write(*,'(A,I16,A)') " outer loop ", special_counter, " adjoint
     +"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.TRUE.
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1250 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_1251 = 1
      do while (INT(OpenAD_Symbol_1251).LE.INT(OpenAD_Symbol_1250))
        integer_tape_pointer = integer_tape_pointer-1
        OpenAD_Symbol_1252 = integer_tape(integer_tape_pointer)
        IF (OpenAD_Symbol_1252.ne.0) THEN
          CALL loop_body_wrapper_inner(LOCAL_U,LOCAL_V,LOCAL_ETA,COST,CA
     +LC_COST,TIME,TIME_INDEX,NIO)
        ENDIF
        OpenAD_Symbol_1251 = INT(OpenAD_Symbol_1251)+1
      END DO
         our_rev_mode=our_orig_mode
        special_counter=special_counter-1
      end if
      end subroutine loop_body_wrapper_outer

      SUBROUTINE forward_model(NCTRL, XC, COST_FINAL)
      use OAD_tape
      use OAD_rev
      use OAD_cp

      use OAD_active
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
      type(active) :: XC(1:NCTRL)
      type(active) :: COST_FINAL
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
      type(active) :: ZONAL_TRANSPORT


      integer :: cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable
     +_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

      character*(80):: indentation='                                    
     +                                             '
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(XC)
C$OPENAD DEPENDENT(COST_FINAL)
C
C     **** Statements ****
C
      our_indent=our_indent+1
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"enter fo
     +rward_model:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_
     +rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our
     +_rev_mode%tape,our_rev_mode%adjoint

C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "enter forward_model:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
C$$$
      if (our_rev_mode%arg_store) then
         write(*,'(A,A)') indentation(1:our_indent)," forward_model: ent
     +ering arg store"
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = CALC_HESS
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DT
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DT_DUMP
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = FULLIO
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = GRAD_CHECK
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = INITIAL_GRAD
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = ITERATION
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NT
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NTSPINUP
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = OPTIMIZE
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = START_TIME
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = SUPPRESSIO
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = XPERIODIC
      if (cp_boolean_size.lt.cp_boolean_pointer+1) then
      allocate(cp_boolean_tmp(cp_boolean_size))
      cp_boolean_tmp = cp_boolean
      deallocate(cp_boolean)
      allocate(cp_boolean(cp_boolean_size*2))
      print *,'CPB+'
      cp_boolean(1:cp_boolean_size) = cp_boolean_tmp
      deallocate(cp_boolean_tmp)
      cp_boolean_size = cp_boolean_size*2
      end if
      cp_boolean_pointer = cp_boolean_pointer+1
      cp_boolean(cp_boolean_pointer) = YPERIODIC
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = RY
      if (cp_integer_size.lt.cp_integer_pointer+1) then
      allocate(cp_integer_tmp(cp_integer_size))
      cp_integer_tmp = cp_integer
      deallocate(cp_integer)
      allocate(cp_integer(cp_integer_size*2))
      print *,'CPI+'
      cp_integer(1:cp_integer_size) = cp_integer_tmp
      deallocate(cp_integer_tmp)
      cp_integer_size = cp_integer_size*2
      end if
      cp_integer_pointer = cp_integer_pointer+1
      cp_integer(cp_integer_pointer) = NEDT
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ZONAL_TRANSPORT_DATA
      if (cp_double_size.lt.cp_double_pointer+1) then
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end if
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_ZONAL_TRANSPORT
      do cp_loop_variable_1 = lbound(ETA,1),ubound(ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA,2)-lbound
     +(ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETA,2),ubound(ETA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA(cp_loop_variable_1,cp_loop_vari
     +able_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(U,1),ubound(U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(U,2)-lbound(U
     +,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(U,2),ubound(U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = U(cp_loop_variable_1,cp_loop_variab
     +le_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(V,1),ubound(V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(V,2)-lbound(V
     +,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(V,2),ubound(V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = V(cp_loop_variable_1,cp_loop_variab
     +le_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(DEPTH,1),ubound(DEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH,2)-lbou
     +nd(DEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH,2),ubound(DEPTH,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DX,1)-lbound(
     +DX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DX,1),ubound(DX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DX(cp_loop_variable_1)
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(DY,1)-lbound(
     +DY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(DY,1),ubound(DY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(ETAINI,1),ubound(ETAINI,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAINI,2)-lbo
     +und(ETAINI,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAINI,2),ubound(ETAINI,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAINI(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(ETAMASK,1),ubound(ETAMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETAMASK,2)-lb
     +ound(ETAMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETAMASK,2),ubound(ETAMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETAMASK(cp_loop_variable_1,cp_loop_
     +variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FCORIU,1),ubound(FCORIU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FCORIU,2)-lbo
     +und(FCORIU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FCORIU,2),ubound(FCORIU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FCORIU(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FCORIV,1),ubound(FCORIV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FCORIV,2)-lbo
     +und(FCORIV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FCORIV,2),ubound(FCORIV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FCORIV(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(FRICT,1),ubound(FRICT,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(FRICT,2)-lbou
     +nd(FRICT,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(FRICT,2),ubound(FRICT,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = FRICT(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(HU,1),ubound(HU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HU,2)-lbound(
     +HU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HU,2),ubound(HU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HU(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(HV,1),ubound(HV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(HV,2)-lbound(
     +HV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(HV,2),ubound(HV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HV(cp_loop_variable_1,cp_loop_varia
     +ble_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(HY,1)-lbound(
     +HY,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(HY,1),ubound(HY,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = HY(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(INVHU,1),ubound(INVHU,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(INVHU,2)-lbou
     +nd(INVHU,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(INVHU,2),ubound(INVHU,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = INVHU(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do cp_loop_variable_1 = lbound(INVHV,1),ubound(INVHV,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(INVHV,2)-lbou
     +nd(INVHV,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(INVHV,2),ubound(INVHV,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = INVHV(cp_loop_variable_1,cp_loop_va
     +riable_2)%v
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(RX,1)-lbound(
     +RX,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(RX,1),ubound(RX,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = RX(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(SCALEDEPTH,1),ubound(SCALEDEPTH,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(SCALEDEPTH,2)
     +-lbound(SCALEDEPTH,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(SCALEDEPTH,2),ubound(SCALEDEPTH,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = SCALEDEPTH(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UINI,1),ubound(UINI,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UINI,2)-lboun
     +d(UINI,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UINI,2),ubound(UINI,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UINI(cp_loop_variable_1,cp_loop_var
     +iable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UMASK,1),ubound(UMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UMASK,2)-lbou
     +nd(UMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UMASK,2),ubound(UMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VINI,1),ubound(VINI,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VINI,2)-lboun
     +d(VINI,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VINI,2),ubound(VINI,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VINI(cp_loop_variable_1,cp_loop_var
     +iable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VMASK,1),ubound(VMASK,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VMASK,2)-lbou
     +nd(VMASK,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VMASK,2),ubound(VMASK,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VMASK(cp_loop_variable_1,cp_loop_va
     +riable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(UFORCE,1),ubound(UFORCE,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(UFORCE,2)-lbo
     +und(UFORCE,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(UFORCE,2),ubound(UFORCE,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = UFORCE(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(VFORCE,1),ubound(VFORCE,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(VFORCE,2)-lbo
     +und(VFORCE,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(VFORCE,2),ubound(VFORCE,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = VFORCE(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(DEPTH_DATA,1),ubound(DEPTH_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(DEPTH_DATA,2)
     +-lbound(DEPTH_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(DEPTH_DATA,2),ubound(DEPTH_DATA,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = DEPTH_DATA(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(ETA_DATA,1),ubound(ETA_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA_DATA,2)-l
     +bound(ETA_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(ETA_DATA,2),ubound(ETA_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA_DATA(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      do while (cp_double_size.lt.cp_double_pointer+ubound(ETA_DATA_TIME
     +,1)-lbound(ETA_DATA_TIME,1))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_1 = lbound(ETA_DATA_TIME,1),ubound(ETA_DATA_TI
     +ME,1),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = ETA_DATA_TIME(cp_loop_variable_1)
      end do
      do cp_loop_variable_1 = lbound(U_DATA,1),ubound(U_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(U_DATA,2)-lbo
     +und(U_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(U_DATA,2),ubound(U_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = U_DATA(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(V_DATA,1),ubound(V_DATA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(V_DATA,2)-lbo
     +und(V_DATA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(V_DATA,2),ubound(V_DATA,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = V_DATA(cp_loop_variable_1,cp_loop_v
     +ariable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_ETA,1),ubound(WEIGHT_ETA,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_ETA,2)
     +-lbound(WEIGHT_ETA,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_ETA,2),ubound(WEIGHT_ETA,2),
     +1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_ETA(cp_loop_variable_1,cp_lo
     +op_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_U,1),ubound(WEIGHT_U,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_U,2)-l
     +bound(WEIGHT_U,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_U,2),ubound(WEIGHT_U,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_U(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      do cp_loop_variable_1 = lbound(WEIGHT_V,1),ubound(WEIGHT_V,1)
      do while (cp_double_size.lt.cp_double_pointer+ubound(WEIGHT_V,2)-l
     +bound(WEIGHT_V,2))
      allocate(cp_double_tmp(cp_double_size))
      cp_double_tmp = cp_double
      deallocate(cp_double)
      allocate(cp_double(cp_double_size*2))
      print *,'CPD+'
      cp_double(1:cp_double_size) = cp_double_tmp
      deallocate(cp_double_tmp)
      cp_double_size = cp_double_size*2
      end do
      do cp_loop_variable_2 = lbound(WEIGHT_V,2),ubound(WEIGHT_V,2),1
      cp_double_pointer = cp_double_pointer+1
      cp_double(cp_double_pointer) = WEIGHT_V(cp_loop_variable_1,cp_loop
     +_variable_2)
      end do
      end do
      end if
      if (our_rev_mode%arg_restore) then
         write(*,'(A,A)') indentation(1:our_indent)," forward_model: ent
     +ering arg restore"
      do cp_loop_variable_1 = ubound(WEIGHT_V,1),lbound(WEIGHT_V,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_V,2),lbound(WEIGHT_V,2),-1
      WEIGHT_V(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(WEIGHT_U,1),lbound(WEIGHT_U,1),-1
      do cp_loop_variable_2 = ubound(WEIGHT_U,2),lbound(WEIGHT_U,2),-1
      WEIGHT_U(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(WEIGHT_ETA,1),lbound(WEIGHT_ETA,1),
     +-1
      do cp_loop_variable_2 = ubound(WEIGHT_ETA,2),lbound(WEIGHT_ETA,2),
     +-1
      WEIGHT_ETA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(V_DATA,1),lbound(V_DATA,1),-1
      do cp_loop_variable_2 = ubound(V_DATA,2),lbound(V_DATA,2),-1
      V_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(U_DATA,1),lbound(U_DATA,1),-1
      do cp_loop_variable_2 = ubound(U_DATA,2),lbound(U_DATA,2),-1
      U_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETA_DATA_TIME,1),lbound(ETA_DATA_TI
     +ME,1),-1
      ETA_DATA_TIME(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +ETA_DATA_TIME(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(ETA_DATA,1),lbound(ETA_DATA,1),-1
      do cp_loop_variable_2 = ubound(ETA_DATA,2),lbound(ETA_DATA,2),-1
      ETA_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_dou
     +ble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DEPTH_DATA,1),lbound(DEPTH_DATA,1),
     +-1
      do cp_loop_variable_2 = ubound(DEPTH_DATA,2),lbound(DEPTH_DATA,2),
     +-1
      DEPTH_DATA(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VFORCE,1),lbound(VFORCE,1),-1
      do cp_loop_variable_2 = ubound(VFORCE,2),lbound(VFORCE,2),-1
      VFORCE(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UFORCE,1),lbound(UFORCE,1),-1
      do cp_loop_variable_2 = ubound(UFORCE,2),lbound(UFORCE,2),-1
      UFORCE(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VMASK,1),lbound(VMASK,1),-1
      do cp_loop_variable_2 = ubound(VMASK,2),lbound(VMASK,2),-1
      VMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(VINI,1),lbound(VINI,1),-1
      do cp_loop_variable_2 = ubound(VINI,2),lbound(VINI,2),-1
      VINI(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UMASK,1),lbound(UMASK,1),-1
      do cp_loop_variable_2 = ubound(UMASK,2),lbound(UMASK,2),-1
      UMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(UINI,1),lbound(UINI,1),-1
      do cp_loop_variable_2 = ubound(UINI,2),lbound(UINI,2),-1
      UINI(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(SCALEDEPTH,1),lbound(SCALEDEPTH,1),
     +-1
      do cp_loop_variable_2 = ubound(SCALEDEPTH,2),lbound(SCALEDEPTH,2),
     +-1
      SCALEDEPTH(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_d
     +ouble_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(RX,1),lbound(RX,1),-1
      RX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +RX(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(INVHV,1),lbound(INVHV,1),-1
      do cp_loop_variable_2 = ubound(INVHV,2),lbound(INVHV,2),-1
      INVHV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(INVHU,1),lbound(INVHU,1),-1
      do cp_loop_variable_2 = ubound(INVHU,2),lbound(INVHU,2),-1
      INVHU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HY,1),lbound(HY,1),-1
      HY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +HY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(HV,1),lbound(HV,1),-1
      do cp_loop_variable_2 = ubound(HV,2),lbound(HV,2),-1
      HV(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(HU,1),lbound(HU,1),-1
      do cp_loop_variable_2 = ubound(HU,2),lbound(HU,2),-1
      HU(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_
     +pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FRICT,1),lbound(FRICT,1),-1
      do cp_loop_variable_2 = ubound(FRICT,2),lbound(FRICT,2),-1
      FRICT(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FCORIV,1),lbound(FCORIV,1),-1
      do cp_loop_variable_2 = ubound(FCORIV,2),lbound(FCORIV,2),-1
      FCORIV(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(FCORIU,1),lbound(FCORIU,1),-1
      do cp_loop_variable_2 = ubound(FCORIU,2),lbound(FCORIU,2),-1
      FCORIU(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAMASK,1),lbound(ETAMASK,1),-1
      do cp_loop_variable_2 = ubound(ETAMASK,2),lbound(ETAMASK,2),-1
      ETAMASK(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETAINI,1),lbound(ETAINI,1),-1
      do cp_loop_variable_2 = ubound(ETAINI,2),lbound(ETAINI,2),-1
      ETAINI(cp_loop_variable_1,cp_loop_variable_2) = cp_double(cp_doubl
     +e_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(DY,1),lbound(DY,1),-1
      DY(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DY(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DX,1),lbound(DX,1),-1
      DX(cp_loop_variable_1) = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
C          write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +DX(cp_loop_variable_1)%v
      end do
      do cp_loop_variable_1 = ubound(DEPTH,1),lbound(DEPTH,1),-1
      do cp_loop_variable_2 = ubound(DEPTH,2),lbound(DEPTH,2),-1
      DEPTH(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_doub
     +le_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(V,1),lbound(V,1),-1
      do cp_loop_variable_2 = ubound(V,2),lbound(V,2),-1
      V(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_p
     +ointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(U,1),lbound(U,1),-1
      do cp_loop_variable_2 = ubound(U,2),lbound(U,2),-1
      U(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double_p
     +ointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      do cp_loop_variable_1 = ubound(ETA,1),lbound(ETA,1),-1
      do cp_loop_variable_2 = ubound(ETA,2),lbound(ETA,2),-1
      ETA(cp_loop_variable_1,cp_loop_variable_2)%v = cp_double(cp_double
     +_pointer)
      cp_double_pointer = cp_double_pointer-1
      end do
      end do
      WEIGHT_ZONAL_TRANSPORT = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      ZONAL_TRANSPORT_DATA = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      NEDT = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      RY = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      YPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      XPERIODIC = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      SUPPRESSIO = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      START_TIME = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      OPTIMIZE = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      NTSPINUP = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      NT = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      ITERATION = cp_integer(cp_integer_pointer)
      cp_integer_pointer = cp_integer_pointer-1
      INITIAL_GRAD = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      GRAD_CHECK = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      FULLIO = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      DT_DUMP = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      DT = cp_double(cp_double_pointer)
      cp_double_pointer = cp_double_pointer-1
      CALC_HESS = cp_boolean(cp_boolean_pointer)
      cp_boolean_pointer = cp_boolean_pointer-1
      end if
      if (our_rev_mode%plain) then
         write(*,'(A,A)') indentation(1:our_indent)," forward_model: ent
     +ering plain"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
C$OPENAD XXX Template OADrts/ad_template_timing.joint.f
      IF(CALC_HESS .OR.(OPTIMIZE .OR.(GRAD_CHECK .OR. INITIAL_GRAD))) TH
     +EN
        CALC_COST = .TRUE.
      ELSE
        CALC_COST = .FALSE.
      ENDIF
      NINNER = 500
      COST_D = 0.0D00
      COST_SD = 0.0D00
      COST_GD = 0.0D00
      COST = 0.0D00
      CALL map_from_control_vector(NCTRL,XC)
      CALL calc_depth_uv()
      CALL initial_values()
      IF (.not. SUPPRESSIO) THEN
      ENDIF
      IF(CALC_COST) THEN
        CALL cost_depth(COST_D)
      ENDIF
      NIO = 0
      TIME_INDEX = 0
      TIME = 0.0D00
      NTOTAL = (NT+NTSPINUP)
      ROUTIN = (REAL(NTOTAL)/REAL(NINNER))
      IF ((NTOTAL/NINNER).ne.ROUTIN) THEN
        NOUTER = (INT(ROUTIN)+1)
      ELSE
        NOUTER = INT(ROUTIN)
      ENDIF
      IF (FULLIO.AND.(.not. SUPPRESSIO)) THEN
        WRITE(*,*) 'number of outer loops',' = number of tape records = 
     +',NOUTER
      ENDIF
      TIME_INDEX = 0
      TIME = (START_TIME+TIME_INDEX*DT)
      IF (.not. SUPPRESSIO) THEN
        NIO = (NIO+1)
        WRITE(*,*) 'Writing Time Step ',TIME_INDEX
      ENDIF
      DO IT = 1,NOUTER,1
        WRITE(*,*) 'outer loop ',IT
        CALL loop_body_wrapper_outer(U,V,ETA,COST,CALC_COST,TIME,TIME_IN
     +DEX,NIO,IT,NTOTAL,NINNER)
      END DO
      ZONAL_TRANSPORT%v = 0.0D00
      CALL calc_zonal_transport_joint(ZONAL_TRANSPORT,U)
      WRITE(*,*) 'zonal volume transport = ',(ZONAL_TRANSPORT%v*9.999999
     +99999999954748D-07),' Sv'
      COST_FINAL%v = ZONAL_TRANSPORT%v
      IF (ITERATION.GE.0) THEN
C       open(10,file='cost.txt',form='formatted',position='append')
        open(unit=10,file='cost.txt',form='FORMATTED',position='APPEND')
        WRITE(10,'(I5,5E15.8)') ITERATION,COST_D,COST_SD,COST_GD,COST,CO
     +ST_FINAL%v
C       close(10)
        close(unit=10)
        ITERATION = (ITERATION+1)
      ENDIF
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then
         write(*,'(A,A)') indentation(1:our_indent)," forward_model: ent
     +ering tape"
         our_rev_mode%arg_store=.TRUE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.TRUE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.FALSE.
       call timeratio()
C$OPENAD XXX Template OADrts/ad_template_timing.joint.f
      IF (CALC_HESS.OR.(OPTIMIZE.OR.(GRAD_CHECK.OR.INITIAL_GRAD))) THEN
        CALC_COST = .TRUE.
        OpenAD_Symbol_1271 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1271
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        CALC_COST = .FALSE.
        OpenAD_Symbol_1272 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1272
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      NINNER = 500
      COST_D = 0.0D00
      COST_SD = 0.0D00
      COST_GD = 0.0D00
      COST = 0.0D00
      CALL map_from_control_vector(NCTRL,XC)
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = NCTRL
      integer_tape_pointer = integer_tape_pointer+1
      CALL calc_depth_uv()
      CALL initial_values()
      IF (.not. SUPPRESSIO) THEN
        OpenAD_Symbol_1273 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1273
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1274 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1274
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF(CALC_COST) THEN
        CALL cost_depth(COST_D)
        OpenAD_Symbol_1275 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1275
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1276 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1276
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      NIO = 0
      TIME_INDEX = 0
      TIME = 0.0D00
      NTOTAL = (NT+NTSPINUP)
      ROUTIN = (REAL(NTOTAL)/REAL(NINNER))
      IF ((NTOTAL/NINNER).ne.ROUTIN) THEN
        NOUTER = (INT(ROUTIN)+1)
        OpenAD_Symbol_1277 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1277
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        NOUTER = INT(ROUTIN)
        OpenAD_Symbol_1278 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1278
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      IF (FULLIO.AND.(.not. SUPPRESSIO)) THEN
        WRITE(*,*) 'number of outer loops',' = number of tape records = 
     +',NOUTER
        OpenAD_Symbol_1279 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1279
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1280 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1280
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      TIME_INDEX = 0
      TIME = (START_TIME+TIME_INDEX*DT)
      IF (.not. SUPPRESSIO) THEN
        NIO = (NIO+1)
        WRITE(*,*) 'Writing Time Step ',TIME_INDEX
        OpenAD_Symbol_1281 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1281
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1282 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1282
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
      OpenAD_Symbol_1283 = 0_w2f__i8
      DO IT = 1,NOUTER,1
        WRITE(*,*) 'outer loop ',IT
        CALL loop_body_wrapper_outer(U,V,ETA,COST,CALC_COST,TIME,TIME_IN
     +DEX,NIO,IT,NTOTAL,NINNER)
        OpenAD_Symbol_1283 = (INT(OpenAD_Symbol_1283)+INT(1_w2f__i8))
      END DO
      if (integer_tape_size.lt.integer_tape_pointer) then
      allocate(integer_tmp_tape(integer_tape_size))
      integer_tmp_tape = integer_tape
      deallocate(integer_tape)
      allocate(integer_tape(integer_tape_size*2))
      print *,"IT+"
      integer_tape(1:integer_tape_size) = integer_tmp_tape
      deallocate(integer_tmp_tape)
      integer_tape_size = integer_tape_size*2
      end if
      integer_tape(integer_tape_pointer) = OpenAD_Symbol_1283
      integer_tape_pointer = integer_tape_pointer+1
      ZONAL_TRANSPORT%v = 0.0D00
      CALL calc_zonal_transport_joint(ZONAL_TRANSPORT,U)
      WRITE(*,*) 'zonal volume transport = ',(ZONAL_TRANSPORT%v*9.999999
     +99999999954748D-07),' Sv'
      COST_FINAL%v = ZONAL_TRANSPORT%v
      IF (ITERATION.GE.0) THEN
C       open(10,file='cost.txt',form='formatted',position='append')
        open(unit=10,file='cost.txt',form='FORMATTED',position='APPEND')
        WRITE(10,'(I5,5E15.8)') ITERATION,COST_D,COST_SD,COST_GD,COST,CO
     +ST_FINAL%v
C       close(10)
        close(unit=10)
        ITERATION = (ITERATION+1)
        OpenAD_Symbol_1284 = 1_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1284
        integer_tape_pointer = integer_tape_pointer+1
      ELSE
        OpenAD_Symbol_1285 = 0_w2f__i8
        if (integer_tape_size.lt.integer_tape_pointer) then
        allocate(integer_tmp_tape(integer_tape_size))
        integer_tmp_tape = integer_tape
        deallocate(integer_tape)
        allocate(integer_tape(integer_tape_size*2))
        print *,"IT+"
        integer_tape(1:integer_tape_size) = integer_tmp_tape
        deallocate(integer_tmp_tape)
        integer_tape_size = integer_tape_size*2
        end if
        integer_tape(integer_tape_pointer) = OpenAD_Symbol_1285
        integer_tape_pointer = integer_tape_pointer+1
      ENDIF
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
      end if
      if (our_rev_mode%adjoint) then
         write(*,'(A,A)') indentation(1:our_indent)," forward_model: ent
     +ering adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
       call timeratio()
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1262 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1262.ne.0) THEN
      ENDIF
      ZONAL_TRANSPORT%d = ZONAL_TRANSPORT%d+COST_FINAL%d
      COST_FINAL%d = 0.0d0
      CALL calc_zonal_transport_joint(ZONAL_TRANSPORT,U)
      ZONAL_TRANSPORT%d = 0.0d0
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1263 = integer_tape(integer_tape_pointer)
      OpenAD_Symbol_1264 = 1
      do while (INT(OpenAD_Symbol_1264).LE.INT(OpenAD_Symbol_1263))
        CALL loop_body_wrapper_outer(U,V,ETA,COST,CALC_COST,TIME,TIME_IN
     +DEX,NIO,IT,NTOTAL,NINNER)
        OpenAD_Symbol_1264 = INT(OpenAD_Symbol_1264)+1
      END DO
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1265 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1265.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1266 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1266.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1267 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1267.ne.0) THEN
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1268 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1268.ne.0) THEN
        CALL cost_depth(COST_D)
      ENDIF
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1269 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1269.ne.0) THEN
      ENDIF
      CALL initial_values()
      CALL calc_depth_uv()
      integer_tape_pointer = integer_tape_pointer-1
      NCTRL = integer_tape(integer_tape_pointer)
      CALL map_from_control_vector(NCTRL,XC)
      integer_tape_pointer = integer_tape_pointer-1
      OpenAD_Symbol_1270 = integer_tape(integer_tape_pointer)
      IF (OpenAD_Symbol_1270.ne.0) THEN
      ENDIF
         call timeratio()
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.TRUE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
      end if
C$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
C$$$     +indentation(1:our_indent), "leave forward_model:",
C$$$     +" AF:",theArgFStackoffset, 
C$$$     +" AI:",theArgIStackoffset, 
C$$$     +" RF:",theResFStackoffset, 
C$$$     +" RI:",theResIStackoffset, 
C$$$     +" DT:",double_tape_pointer, 
C$$$     +" IT:",integer_tape_pointer
      write(*,'(A,A,L,L,L,L,L,L,L)') indentation(1:our_indent),"leave fo
     +rward_model:",our_rev_mode%arg_store,our_rev_mode%arg_restore,our_
     +rev_mode%res_store,our_rev_mode%res_restore,our_rev_mode%plain,our
     +_rev_mode%tape,our_rev_mode%adjoint
      our_indent=our_indent-1
      end subroutine forward_model
