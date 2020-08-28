program boxmodel_adm_driver

  use OAD_active
  use OAD_rev
  use OAD_tape
  use RevStats
  use all_globals_mod
  implicit none

  integer, parameter :: kdim=3
  integer i,j
  double precision jac(2*kdim,2*kdim)


  external box_model_body

  write (*,fmt='(a)',advance='no') 'number of Revolve checkpoints = '
  read (*,*) revStatsRevolveCPcount
  write(*,*) "adjoint model, joint mode"
  call oad_tape_init()
!  do i=1,kdim
    xx%v=0
     call revStatsInit()
     do j=1,kdim
        call zero_deriv(xx(j))
     end do
     call OAD_revPlain()
     call box_ini_params
     call box_ini_fields
     do j=1,kdim
        call zero_deriv(tnew(j))
        call zero_deriv(snew(j))
     enddo
     snew(1)%d=1.0
!     if (i>kdim) then
!        do j=1,kdim
!           if (j==i-kdim) then
!              snew(j)%d=1
!           end if
!        end do
!     else
!        do j=1,kdim
!           if (j==i) then
!              tnew(j)%d=1
!           end if
!        end do
!     end if
     call OAD_revTape()
     call box_model_body
     print *,snew(1)
     call OAD_revAdjoint()
     call box_model_body
     do j=1,kdim
        write(*,*) "F(",i,",",j,")=",xx(j)%d
     end do
!  end do
  call revStatsDump()

   end program 
