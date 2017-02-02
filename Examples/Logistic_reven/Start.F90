Program bom


  use OAD_active
  use OAD_rev
  use OAD_tape
  use OAD_cp
  use csv_file
  use GRID
  use nfunct
  use prefix
  !      use main_prog
  IMPLICIT NONE
  integer,parameter:: vsize=im*jm*kb
  integer i,j,k
  type(active)::f,vec(im,jm,kb)
  logical test
  real::ret0(im,jm)
  ! set the execution mode
  call oad_tape_init()
  our_rev_mode%arg_store=.FALSE.
  our_rev_mode%arg_restore=.FALSE.
  our_rev_mode%res_store=.FALSE.
  our_rev_mode%res_restore=.FALSE.
  our_rev_mode%plain=.TRUE.
  our_rev_mode%tape=.FALSE.
  our_rev_mode%adjoint=.FALSE.

  ! initialize the tape

  ! initialize the checkpoint areas
  !      call cp_init()
  f%v=0;f%d=1;

  vec%d=0.
  inquire(file=trim(adjustl(FILE_PREFIX_COST))//'inp.txt',EXIST=test)
  if (.not.test) then
     goto 30
  endif
  open (file=trim(adjustl(FILE_PREFIX_COST))//'inp.txt',unit=9732,action='read',err=30)
  do i=1,im
     do j=1,jm
        do k=1,kb
           read (9732,*)vec(i,j,k)%v
        enddo
     enddo
  enddo
  close(9732)
  goto 50
30 print *,'first start ',trim(adjustl(FILE_PREFIX_COST))//'inp.txt' 
  vec%v=0.
  open (file=trim(adjustl(FILE_PREFIX_COST))//'inp.txt',unit=9732,action='write',err=30)
  do i=1,im
     do j=1,jm
        do k=1,kb
           write (9732,*)vec(i,j,k)%v
        enddo
     enddo
  enddo
  close(9732)
50 our_rev_mode%arg_store=.FALSE.
  our_rev_mode%arg_restore=.FALSE.
  our_rev_mode%res_store=.FALSE.
  our_rev_mode%res_restore=.FALSE.
  our_rev_mode%plain=.FALSE.
  our_rev_mode%tape=.TRUE.
  our_rev_mode%adjoint=.TRUE.
  vec(1,1,1)%v=3.0
  call model_counter(vec, F)

  do k=1,kb
     do i=1,im
        do j=1,jm

           ret0(i,j)=vec(i,j,k)%d
        end do
     end do
     OPEN(1116,FILE=open_filesg1(k,1,"OUTs"))
     CALL csv_write(1116,ret0(:,:))
     CLOSE(1116)
  enddo
  open (file=trim(adjustl(FILE_PREFIX_COST))//'out.txt',unit=9732,action='write')
  do i=1,im
     do j=1,jm
        do k=1,kb
           write (9732,*)vec(i,j,k)%d
        enddo
     enddo
  enddo
  close(9732)
  open (file=trim(adjustl(FILE_PREFIX_COST))//'val.txt',unit=9732,action='write')

  write (9732,*)F%v

  close(9732)
end program bom
