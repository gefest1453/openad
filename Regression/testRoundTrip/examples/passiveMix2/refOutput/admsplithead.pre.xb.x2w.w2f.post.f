C#########################################################
C This file is part of OpenAD released under the LGPL.   #
C The full COPYRIGHT notice can be found in the top      #
C level directory of the OpenAD distribution             #
C#########################################################
C
C$OPENAD XXX File_start [head.f]
      SUBROUTINE head(X, Y)
          use OAD_tape
          use OAD_rev

C original arguments get inserted before version
C         ! and declared here together with all local variables
C         ! generated by xaifBooster

      use OAD_active
      use w2f__types
      use oad_intrinsics
      use oad_intrinsics
      use oad_intrinsics
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      type(active) :: X(1:1)
      type(active) :: Y(1:1)
C
C     **** Local Variables and Functions ****
C
      type(active) :: OAD_CTMP0
      type(active) :: OAD_CTMP1
      REAL(w2f__8) OAD_CTMP2
      REAL(w2f__8) OAD_CTMP3
      REAL(w2f__8) T
      type(active) :: T1
      REAL(w2f__8) T2
      type(active) :: OpenAD_tyc_0
      type(active) :: OpenAD_tyc_1
      type(active) :: OpenAD_tyc_11
      type(active) :: OpenAD_tyc_13
      type(active) :: OpenAD_tyc_2
      type(active) :: OpenAD_tyc_3
      type(active) :: OpenAD_tyc_4
      type(active) :: OpenAD_tyc_5
      type(active) :: OpenAD_tyc_6
      type(active) :: OpenAD_tyc_7


          integer iaddr
          external iaddr
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(X)
C$OPENAD DEPENDENT(Y)
C
C     **** Statements ****
C

         if (our_rev_mode%plain) then
C original function
C$OPENAD XXX Template ad_template.f
      T = 2.0D00
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_0,T)
      CALL OAD_S_MIN_D(OpenAD_tyc_0,X(1),OAD_CTMP0)
      T1%v = OAD_CTMP0%v
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_1,T)
      CALL OAD_S_MIN_D(T1,OpenAD_tyc_1,OAD_CTMP1)
      Y(1)%v = OAD_CTMP1%v
      OAD_CTMP3 = 2.5D00
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_2,T)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_3,OAD_CTMP3)
      CALL OAD_S_MIN_D(OpenAD_tyc_2,OpenAD_tyc_3,OpenAD_tyc_4)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OAD_CTMP2,OpenAD_tyc_4)
      T2 = OAD_CTMP2
          end if
          if (our_rev_mode%tape) then
C taping
C$OPENAD XXX Template ad_template.f
      T = 2.0D00
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_0,T)
      CALL OAD_S_MIN_D(OpenAD_tyc_0,X(1),OAD_CTMP0)
      T1%v = OAD_CTMP0%v
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_1,T)
      CALL OAD_S_MIN_D(T1,OpenAD_tyc_1,OAD_CTMP1)
      Y(1)%v = OAD_CTMP1%v
      OAD_CTMP3 = 2.5D00
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_2,T)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_3,OAD_CTMP3)
      CALL OAD_S_MIN_D(OpenAD_tyc_2,OpenAD_tyc_3,OpenAD_tyc_4)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OAD_CTMP2,OpenAD_tyc_4)
      T2 = OAD_CTMP2
          end if
          if (our_rev_mode%adjoint) then
C adjoint
      CALL OAD_S_MIN_D(OpenAD_tyc_5,OpenAD_tyc_6,OpenAD_tyc_7)
      OAD_CTMP1%d = OAD_CTMP1%d+Y(1)%d
      Y(1)%d = 0.0d0
      CALL OAD_S_MIN_D(T1,OpenAD_tyc_11,OAD_CTMP1)
      OAD_CTMP0%d = OAD_CTMP0%d+T1%d
      T1%d = 0.0d0
      CALL OAD_S_MIN_D(OpenAD_tyc_13,X(1),OAD_CTMP0)
          end if
        end subroutine head