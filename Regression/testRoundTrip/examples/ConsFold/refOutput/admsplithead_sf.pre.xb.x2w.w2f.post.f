
!$OPENAD XXX File_start [OAD_intrinsics.f90]
MODULE oad_intrinsics
use OAD_active
use w2f__types
IMPLICIT NONE
SAVE
!
!     **** Statements ****
!
END MODULE

C$OPENAD XXX File_start [all_globals_mod.f]
      MODULE all_globals_mod
      use OAD_active
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Statements ****
C
      END MODULE
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
      type(active) :: T1
      type(active) :: T2
      type(active) :: T3
      type(active) :: T4
      type(active) :: T5
      type(active) :: T6
      type(active) :: T7
      type(active) :: Y1
      REAL(w2f__8) OpenAD_Symbol_0
      REAL(w2f__8) OpenAD_acc_0
      REAL(w2f__8) OpenAD_aux_0
      REAL(w2f__8) OpenAD_lin_0
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(X)
C$OPENAD DEPENDENT(Y)
C
C     **** Statements ****
C


          integer iaddr
          external iaddr
C$OPENAD XXX Template ad_template.f
C$OPENAD XXX Template ad_template.f

         if (our_rev_mode%plain) then
C original function
      T1%v = X(1)%v
      T2%v = (T1%v+2.0D00)
      T3%v = (T2%v*5.0D-01)
      T4%v = (T3%v*2.0D00)
      T5%v = (T4%v+4.0D00)
      T6%v = (T5%v+3.0D00)
      T7%v = (T6%v+(-9.0D00))
      Y1%v = EXP(T7%v)
      Y(1)%v = Y1%v
          end if
          if (our_rev_mode%tape) then
C taping
      T1%v = X(1)%v
      T2%v = (T1%v+2.0D00)
      T3%v = (T2%v*5.0D-01)
      T4%v = (T3%v*2.0D00)
      T5%v = (T4%v+4.0D00)
      T6%v = (T5%v+3.0D00)
      T7%v = (T6%v+(-9.0D00))
      OpenAD_aux_0 = EXP(T7%v)
      OpenAD_lin_0 = OpenAD_aux_0
      Y1%v = OpenAD_aux_0
      Y(1)%v = Y1%v
      OpenAD_acc_0 = (1.0D00*OpenAD_lin_0)
      double_tape(double_tape_pointer) = OpenAD_acc_0
      double_tape_pointer = double_tape_pointer+1
          end if
          if (our_rev_mode%adjoint) then
C adjoint
      double_tape_pointer = double_tape_pointer-1
      OpenAD_Symbol_0 = double_tape(double_tape_pointer)
      X(1)%d = X(1)%d+Y(1)%d*(OpenAD_Symbol_0)
      Y(1)%d = 0.0d0
          end if
        end subroutine head