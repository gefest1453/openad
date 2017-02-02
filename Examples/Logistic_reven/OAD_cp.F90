!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
module OAD_cp

  implicit none

  private :: cp_file_number, cp_open

  public :: cp_io_unit, cp_init, cp_write_open, cp_read_open, cp_close, cp_fNumber

  integer :: cp_file_number, cp_io_unit

  interface cp_init
     module procedure init_i
  end interface cp_init

  interface cp_open
     module procedure open_i
  end interface cp_open

  interface cp_write_open
     module procedure write_open_i
     module procedure write_openX_i
  end interface cp_write_open

  interface cp_read_open
     module procedure read_open_i
     module procedure read_openX_i
  end interface cp_read_open

  interface cp_close
     module procedure close_i
  end interface cp_close

  interface cp_findunit
     module procedure findunit_i
  end interface cp_findunit

contains

  subroutine init_i
    implicit none
    cp_file_number=1
  end subroutine init_i

  subroutine write_open_i()
    implicit none
    call cp_open()
    !    print *, 'writing ', cp_file_number
    cp_file_number=cp_file_number+1
  end subroutine write_open_i

  subroutine write_openX_i(X)
    implicit none
    integer X
    cp_file_number=X
    !    print *, 'writing ', cp_file_number
    call cp_open()
  end subroutine write_openX_i

  subroutine read_open_i()
    implicit none
    cp_file_number=cp_file_number-1
    !    print *, 'reading ', cp_file_number
    call cp_open()
  end subroutine read_open_i

  subroutine read_openX_i(X)
    implicit none
    integer X
    cp_file_number=X
    !    print *, 'reading ', cp_file_number
    call cp_open()
  end subroutine read_openX_i

  subroutine open_i()
    implicit none
    integer rank, mpirc
    character*128 fname ! file name
    ! get unit
    rank=0
    call cp_findunit()
    ! construct the file name
    write(fname,'(A,I9.9)') 'oad_cp.',cp_file_number
!    print *,fname
    open( UNIT=cp_io_unit,FILE=TRIM(fname),FORM="UNFORMATTED",STATUS='UNKNOWN' )
  end subroutine open_i

  subroutine close_i()
    implicit none
    close( UNIT=cp_io_unit)
  end subroutine close_i

  subroutine findunit_i()
    ! returns a valid, unused unit number for Fortran I/O
    ! the routine stops the program if an error occurs in the process
    ! of searching the I/O channels.
    implicit none
    ! Local
    integer ii,jj
    logical op
    integer ios
    character*(1024) msgbuf
    ! Sweep through a valid range of unit numbers
    cp_io_unit=-1
    do jj=9,999
       ii=jj*2+1
       if (cp_io_unit.eq.-1) then
          inquire(unit=ii,iostat=ios,opened=op)
          if (ios.ne.0) then
             write(msgbuf,'(a,i2.2)')  'OAD_cp:findunit_i: inquiring unit number = ',ii
             print *, msgBuf
             write(msgbuf,'(a)') 'OAD_cp:findunit_i: inquire statement failed!'
             print *, msgBuf
             stop 'ABNORMAL END: S/R OAD_cp:findunit_i'
          endif
          if (.NOT. op) then
             cp_io_unit=ii
          end if
       end if
    end do
    ! Was there an available unit number
    if (cp_io_unit.eq.-1) then
       write(msgbuf,'(a)')  'OAD_cp:findunit_i: could not find an available unit number!'
       print *, msgBuf
       stop 'ABNORMAL END: S/R OAD_cp:findunit_i'
    endif
  end subroutine findunit_i

  function cp_fNumber()
    integer cp_fNumber
    cp_fNumber=cp_file_number
  end function cp_fNumber

end module OAD_cp
