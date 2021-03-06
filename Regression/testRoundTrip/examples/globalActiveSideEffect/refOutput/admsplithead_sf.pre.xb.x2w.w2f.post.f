
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

C$OPENAD XXX File_start [head.f]
      MODULE globals
      use OAD_active
      use w2f__types
      IMPLICIT NONE
      SAVE
C
C     **** Global Variables & Derived Type Definitions ****
C
      type(active) :: AGLOBALACTIVE
C
C     **** Statements ****
C
      END MODULE
C#########################################################
C This file is part of OpenAD released under the LGPL.   #
C The full COPYRIGHT notice can be found in the top      #
C level directory of the OpenAD distribution             #
C#########################################################

      SUBROUTINE foo(P)
          use OAD_tape
          use OAD_rev

C original arguments get inserted before version
C         ! and declared here together with all local variables
C         ! generated by xaifBooster

      use OAD_active
      use w2f__types
      use oad_intrinsics
      use globals
      use oad_intrinsics
      use globals
      use oad_intrinsics
      use globals
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      type(active) :: P
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) OpenAD_Symbol_0
      REAL(w2f__8) OpenAD_Symbol_1
      REAL(w2f__8) OpenAD_lin_0
      REAL(w2f__8) OpenAD_lin_1
      type(active) :: OpenAD_prp_0
C
C     **** Statements ****
C


          integer iaddr
          external iaddr

         if (our_rev_mode%plain) then
C original function
      AGLOBALACTIVE%v = (P%v*AGLOBALACTIVE%v)
          end if
          if (our_rev_mode%tape) then
C taping
      OpenAD_lin_0 = AGLOBALACTIVE%v
      OpenAD_lin_1 = P%v
      AGLOBALACTIVE%v = (P%v*AGLOBALACTIVE%v)
      double_tape(double_tape_pointer) = OpenAD_lin_0
      double_tape_pointer = double_tape_pointer+1
      double_tape(double_tape_pointer) = OpenAD_lin_1
      double_tape_pointer = double_tape_pointer+1
          end if
          if (our_rev_mode%adjoint) then
C adjoint
      double_tape_pointer = double_tape_pointer-1
      OpenAD_Symbol_0 = double_tape(double_tape_pointer)
      double_tape_pointer = double_tape_pointer-1
      OpenAD_Symbol_1 = double_tape(double_tape_pointer)
      OpenAD_prp_0%d = OpenAD_prp_0%d+AGLOBALACTIVE%d*(OpenAD_Symbol_0)
      P%d = P%d+AGLOBALACTIVE%d*(OpenAD_Symbol_1)
      AGLOBALACTIVE%d = 0.0d0
      AGLOBALACTIVE%d = AGLOBALACTIVE%d+OpenAD_prp_0%d
      OpenAD_prp_0%d = 0.0d0
          end if
        end subroutine foo
C#########################################################
C This file is part of OpenAD released under the LGPL.   #
C The full COPYRIGHT notice can be found in the top      #
C level directory of the OpenAD distribution             #
C#########################################################

      SUBROUTINE head(X, Y)
          use OAD_tape
          use OAD_rev

C original arguments get inserted before version
C         ! and declared here together with all local variables
C         ! generated by xaifBooster

      use OAD_active
      use w2f__types
      use oad_intrinsics
      use globals
      use oad_intrinsics
      use globals
      use oad_intrinsics
      use globals
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      type(active) :: X(1:1)
      type(active) :: Y(1:1)
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__8) ANINACTIVE
      EXTERNAL foo
      REAL(w2f__8) OpenAD_Symbol_2
      REAL(w2f__8) OpenAD_lin_2
      type(active) :: OpenAD_tyc_0
      type(active) :: OpenAD_tyc_1


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
      ANINACTIVE = 4.445600128173828125D+01
      AGLOBALACTIVE%v = 1.61803400516510009766D00
      CALL foo(X(1))
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_0,ANINACTIVE)
      CALL foo(OpenAD_tyc_0)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(ANINACTIVE,OpenAD_tyc_0)
      Y(1)%v = SIN(AGLOBALACTIVE%v)
          end if
          if (our_rev_mode%tape) then
C taping
C$OPENAD XXX Template ad_template.f
      ANINACTIVE = 4.445600128173828125D+01
      AGLOBALACTIVE%v = 1.61803400516510009766D00
      CALL foo(X(1))
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(OpenAD_tyc_0,ANINACTIVE)
      CALL foo(OpenAD_tyc_0)
C!! requested inline of 'oad_convert' has no defn
      CALL oad_convert(ANINACTIVE,OpenAD_tyc_0)
      OpenAD_lin_2 = COS(AGLOBALACTIVE%v)
      Y(1)%v = SIN(AGLOBALACTIVE%v)
      double_tape(double_tape_pointer) = OpenAD_lin_2
      double_tape_pointer = double_tape_pointer+1
          end if
          if (our_rev_mode%adjoint) then
C adjoint
      double_tape_pointer = double_tape_pointer-1
      OpenAD_Symbol_2 = double_tape(double_tape_pointer)
      AGLOBALACTIVE%d = AGLOBALACTIVE%d+Y(1)%d*(OpenAD_Symbol_2)
      Y(1)%d = 0.0d0
      CALL foo(OpenAD_tyc_1)
      CALL foo(X(1))
      AGLOBALACTIVE%d = 0.0d0
          end if
        end subroutine head
