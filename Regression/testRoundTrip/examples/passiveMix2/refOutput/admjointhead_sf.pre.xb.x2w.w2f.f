
C$OPENAD XXX File_start [OAD_intrinsics.f90]
      MODULE oad_intrinsics
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Top Level Pragmas ****
C
      interface  OAD_S_MIN
        module procedure  OAD_S_MIN_D

      end interface 
      
C
C     **** Statements ****
C
      CONTAINS

        SUBROUTINE OAD_S_MIN_D(A0, A1, R)
        use w2f__types
        IMPLICIT NONE
C
C       **** Global Variables & Derived Type Definitions ****
C
        INTEGER(w2f__i8) OpenAD_Symbol_0
        INTEGER(w2f__i8) OpenAD_Symbol_1
        INTEGER(w2f__i8) OpenAD_Symbol_2
        INTEGER(w2f__i8) OpenAD_Symbol_3
        INTEGER(w2f__i8) OpenAD_Symbol_4
        INTEGER(w2f__i8) OpenAD_Symbol_5
C
C       **** Parameters and Result ****
C
        TYPE (OpenADTy_active) A0
        TYPE (OpenADTy_active) A1
        TYPE (OpenADTy_active) R
C
C       **** Statements ****
C
C       $OpenAD$ BEGIN REPLACEMENT 1
        IF(__value__(A0) .LT. __value__(A1)) THEN
          __value__(R) = __value__(A0)
        ELSE
          __value__(R) = __value__(A1)
        ENDIF
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 2
        IF(__value__(A0) .LT. __value__(A1)) THEN
          __value__(R) = __value__(A0)
          OpenAD_Symbol_1 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_1)
        ELSE
          __value__(R) = __value__(A1)
          OpenAD_Symbol_2 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_2)
        ENDIF
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 3
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_0)
        IF(OpenAD_Symbol_0 .ne. 0) THEN
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(R), __deriv__(A0))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(R))
        ELSE
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(R), __deriv__(A1))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(R))
        ENDIF
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 4
C       $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
        CALL cp_arg_store_real_scalar_a(__deriv__(A0))
C       $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
        CALL cp_arg_store_real_scalar_a(__deriv__(A1))
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 5
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 6
C       $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
        CALL cp_arg_restore_real_scalar_a(__deriv__(A1))
C       $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
        CALL cp_arg_restore_real_scalar_a(__deriv__(A0))
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 7
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 8
C       $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
        CALL cp_arg_store_real_scalar_a(__deriv__(R))
C       $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
        CALL cp_arg_store_real_scalar_a(__deriv__(A0))
C       $OpenAD$ INLINE cp_arg_store_real_scalar_a(subst)
        CALL cp_arg_store_real_scalar_a(__deriv__(A1))
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 9
C       $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
        CALL cp_arg_restore_real_scalar_a(__deriv__(A1))
C       $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
        CALL cp_arg_restore_real_scalar_a(__deriv__(A0))
C       $OpenAD$ INLINE cp_arg_restore_real_scalar_a(subst)
        CALL cp_arg_restore_real_scalar_a(__deriv__(R))
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 10
        IF(__value__(A0) .LT. __value__(A1)) THEN
          __value__(R) = __value__(A0)
          OpenAD_Symbol_4 = 1_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_4)
        ELSE
          __value__(R) = __value__(A1)
          OpenAD_Symbol_5 = 0_w2f__i8
C         $OpenAD$ INLINE push_i_s0(subst)
          CALL push_i_s0(OpenAD_Symbol_5)
        ENDIF
C       $OpenAD$ END REPLACEMENT
C       $OpenAD$ BEGIN REPLACEMENT 11
C       $OpenAD$ INLINE pop_i_s0(subst)
        CALL pop_i_s0(OpenAD_Symbol_3)
        IF(OpenAD_Symbol_3 .ne. 0) THEN
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(R), __deriv__(A0))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(R))
        ELSE
C         $OpenAD$ INLINE IncDeriv(subst,subst)
          CALL IncDeriv(__deriv__(R), __deriv__(A1))
C         $OpenAD$ INLINE ZeroDeriv(subst)
          CALL ZeroDeriv(__deriv__(R))
        ENDIF
C       $OpenAD$ END REPLACEMENT
        END SUBROUTINE
      END

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
      use oad_intrinsics
      use oad_intrinsics
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      TYPE (OpenADTy_active) X(1 : 1)
      TYPE (OpenADTy_active) Y(1 : 1)
C
C     **** Local Variables and Functions ****
C
      TYPE (OpenADTy_active) OAD_CTMP0
      TYPE (OpenADTy_active) OAD_CTMP1
      REAL(w2f__8) OAD_CTMP2
      REAL(w2f__8) OAD_CTMP3
      REAL(w2f__8) T
      TYPE (OpenADTy_active) T1
      REAL(w2f__8) T2
      TYPE (OpenADTy_active) OpenAD_tyc_0
      TYPE (OpenADTy_active) OpenAD_tyc_1
      TYPE (OpenADTy_active) OpenAD_tyc_11
      TYPE (OpenADTy_active) OpenAD_tyc_13
      TYPE (OpenADTy_active) OpenAD_tyc_2
      TYPE (OpenADTy_active) OpenAD_tyc_3
      TYPE (OpenADTy_active) OpenAD_tyc_4
      TYPE (OpenADTy_active) OpenAD_tyc_5
      TYPE (OpenADTy_active) OpenAD_tyc_6
      TYPE (OpenADTy_active) OpenAD_tyc_7
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(X)
C$OPENAD DEPENDENT(Y)
C
C     **** Statements ****
C
C     $OpenAD$ BEGIN REPLACEMENT 1
C$OPENAD XXX Template ad_template.f
      T = 2.0D00
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_0), T)
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_0), __deriv__(X(1)),
     >  __deriv__(OAD_CTMP0))
      __value__(T1) = __value__(OAD_CTMP0)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_1), T)
      CALL OAD_S_MIN_D(__deriv__(T1), __deriv__(OpenAD_tyc_1),
     >  __deriv__(OAD_CTMP1))
      __value__(Y(1)) = __value__(OAD_CTMP1)
      OAD_CTMP3 = 2.5D00
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_2), T)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_3), OAD_CTMP3)
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_2), __deriv__(OpenAD_tyc_3)
     > , __deriv__(OpenAD_tyc_4))
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(OAD_CTMP2, __deriv__(OpenAD_tyc_4))
      T2 = OAD_CTMP2
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 2
C$OPENAD XXX Template ad_template.f
      T = 2.0D00
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_0), T)
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_0), __deriv__(X(1)),
     >  __deriv__(OAD_CTMP0))
      __value__(T1) = __value__(OAD_CTMP0)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_1), T)
      CALL OAD_S_MIN_D(__deriv__(T1), __deriv__(OpenAD_tyc_1),
     >  __deriv__(OAD_CTMP1))
      __value__(Y(1)) = __value__(OAD_CTMP1)
      OAD_CTMP3 = 2.5D00
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_2), T)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_3), OAD_CTMP3)
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_2), __deriv__(OpenAD_tyc_3)
     > , __deriv__(OpenAD_tyc_4))
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(OAD_CTMP2, __deriv__(OpenAD_tyc_4))
      T2 = OAD_CTMP2
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 3
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_5), __deriv__(OpenAD_tyc_6)
     > , __deriv__(OpenAD_tyc_7))
C     $OpenAD$ INLINE IncDeriv(subst,subst)
      CALL IncDeriv(__deriv__(Y(1)), __deriv__(OAD_CTMP1))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(Y(1)))
      CALL OAD_S_MIN_D(__deriv__(T1), __deriv__(OpenAD_tyc_11),
     >  __deriv__(OAD_CTMP1))
C     $OpenAD$ INLINE IncDeriv(subst,subst)
      CALL IncDeriv(__deriv__(T1), __deriv__(OAD_CTMP0))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(T1))
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_13), __deriv__(X(1)),
     >  __deriv__(OAD_CTMP0))
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
C$OPENAD XXX Template ad_template.f
      T = 2.0D00
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_0), T)
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_0), __deriv__(X(1)),
     >  __deriv__(OAD_CTMP0))
      __value__(T1) = __value__(OAD_CTMP0)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_1), T)
      CALL OAD_S_MIN_D(__deriv__(T1), __deriv__(OpenAD_tyc_1),
     >  __deriv__(OAD_CTMP1))
      __value__(Y(1)) = __value__(OAD_CTMP1)
      OAD_CTMP3 = 2.5D00
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_2), T)
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(__deriv__(OpenAD_tyc_3), OAD_CTMP3)
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_2), __deriv__(OpenAD_tyc_3)
     > , __deriv__(OpenAD_tyc_4))
C     $OpenAD$ INLINE oad_convert(subst,subst)
      CALL oad_convert(OAD_CTMP2, __deriv__(OpenAD_tyc_4))
      T2 = OAD_CTMP2
C     $OpenAD$ END REPLACEMENT
C     $OpenAD$ BEGIN REPLACEMENT 11
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_5), __deriv__(OpenAD_tyc_6)
     > , __deriv__(OpenAD_tyc_7))
C     $OpenAD$ INLINE IncDeriv(subst,subst)
      CALL IncDeriv(__deriv__(Y(1)), __deriv__(OAD_CTMP1))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(Y(1)))
      CALL OAD_S_MIN_D(__deriv__(T1), __deriv__(OpenAD_tyc_11),
     >  __deriv__(OAD_CTMP1))
C     $OpenAD$ INLINE IncDeriv(subst,subst)
      CALL IncDeriv(__deriv__(T1), __deriv__(OAD_CTMP0))
C     $OpenAD$ INLINE ZeroDeriv(subst)
      CALL ZeroDeriv(__deriv__(T1))
      CALL OAD_S_MIN_D(__deriv__(OpenAD_tyc_13), __deriv__(X(1)),
     >  __deriv__(OAD_CTMP0))
C     $OpenAD$ END REPLACEMENT
      END SUBROUTINE
