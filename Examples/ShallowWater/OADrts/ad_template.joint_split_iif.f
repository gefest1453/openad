      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr

c$$$      character*(80):: indentation='                                        
c$$$     +                                         '
c$$$
c$$$      our_indent=our_indent+1
c$$$
c$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
c$$$     +indentation(1:our_indent), "enter __SRNAME__:",
c$$$     +our_rev_mode%arg_store,
c$$$     +our_rev_mode%arg_restore,
c$$$     +our_rev_mode%res_store,
c$$$     +our_rev_mode%res_restore,
c$$$     +our_rev_mode%plain,
c$$$     +our_rev_mode%tape,
c$$$     +our_rev_mode%adjoint
c$$$
c$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
c$$$     +indentation(1:our_indent), "enter __SRNAME__:",
c$$$     +" AF:",theArgFStackoffset, 
c$$$     +" AI:",theArgIStackoffset, 
c$$$     +" RF:",theResFStackoffset, 
c$$$     +" RI:",theResIStackoffset, 
c$$$     +" DT:",double_tape_pointer, 
c$$$     +" IT:",integer_tape_pointer
c$$$
      if (our_rev_mode%arg_store) then 
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering arg store"
!$PLACEHOLDER_PRAGMA$ id=4
      end if 
      if (our_rev_mode%arg_restore) then
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering arg restore"
!$PLACEHOLDER_PRAGMA$ id=6
      end if
      if (our_rev_mode%plain) then
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering plain"
!$PLACEHOLDER_PRAGMA$ id=1
      end if
      if (our_rev_mode%tape) then
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: entering tape"
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=2
c$$$         write(*,'(A,A)') 
c$$$     +indentation(1:our_indent), " __SRNAME__: following with adjoint"
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%res_store=.FALSE.
         our_rev_mode%res_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
!$PLACEHOLDER_PRAGMA$ id=3
         our_rev_mode=our_orig_mode
      end if 
c$$$
c$$$          write(*,'(A,A,A,I16,A,I6,A,I6,A,I6,A,I8,A,I8)')
c$$$     +indentation(1:our_indent), "leave __SRNAME__:",
c$$$     +" AF:",theArgFStackoffset, 
c$$$     +" AI:",theArgIStackoffset, 
c$$$     +" RF:",theResFStackoffset, 
c$$$     +" RI:",theResIStackoffset, 
c$$$     +" DT:",double_tape_pointer, 
c$$$     +" IT:",integer_tape_pointer
c$$$
c$$$      write(*,'(A,A,L,L,L,L,L,L,L)') 
c$$$     +indentation(1:our_indent), "leave __SRNAME__:",
c$$$     +our_rev_mode%arg_store,
c$$$     +our_rev_mode%arg_restore,
c$$$     +our_rev_mode%res_store,
c$$$     +our_rev_mode%res_restore,
c$$$     +our_rev_mode%plain,
c$$$     +our_rev_mode%tape,
c$$$     +our_rev_mode%adjoint
c$$$
c$$$      our_indent=our_indent-1
c$$$
      end subroutine template
