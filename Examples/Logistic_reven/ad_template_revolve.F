      subroutine template()
      use OAD_tape
      use OAD_rev
      use OAD_cp
      use revolve
      use RevStats
!$TEMPLATE_PRAGMA_DECLARATIONS
      LOGICAL :: initialized=.FALSE.
      TYPE(rvAction),save :: theAction
      CHARACTER(80) :: errorMsg
      integer, save :: jointCPCount
      integer, save :: currIter
      
      if (our_rev_mode%arg_store) then 
         call cp_write_open
!$PLACEHOLDER_PRAGMA$ id=4
         call cp_close
      end if 
      if (our_rev_mode%arg_restore) then
         call cp_read_open
!$PLACEHOLDER_PRAGMA$ id=6
         call cp_close
      end if
      if (our_rev_mode%plain) then
         revStatsFCounters%__SRNAME__=revStatsFCounters%__SRNAME__+1
         DO I = 1, N, 1
            CALL  ad_cost_work(x,k,l,dt,i,xop)
         END DO
      end if 
      if (our_rev_mode%tape) then
         revStatsFCounters%__SRNAME__=revStatsFCounters%__SRNAME__+1
         currIter=0
         jointCPcount=cp_fNumber()
         initialized=rvInit(n,revStatsRevolveCPcount,
     +                      errorMsg,theAction)
         IF (.NOT.initialized) WRITE(*,'(A,A)') 'Error: ', errorMsg
         do while (theAction%actionFlag/=rvDone)
            theAction=rvNextAction()
            select case (theAction%actionFlag) 
         case (rvStore)
            call cp_write_open(theAction%cpNum+jointCPCount)
            call revStatsUpdateCP(cp_fNumber()+1)
!$PLACEHOLDER_PRAGMA$ id=4
            call cp_close
         case (rvForward)
            call OAD_revPlain
            do currIter=currIter,theAction%iteration-1
               call   ad_cost_work(x,k,l,dt,currIter+1,xop)
            end do
            call OAD_revTape
         case (rvFirstUTurn)
            call ad_cost_work(x,k,l,dt,currIter+1,xop)
! get out now ...
            exit                
         end select   
      end do
      end if 
      if (our_rev_mode%adjoint) then
         IF (.NOT.initialized) WRITE(*,'(A)') 'Error: not initialized'
         do while (theAction%actionFlag/=rvDone)
           select case (theAction%actionFlag) 
           case (rvFirstUTurn)
!we taped already ... see above
              call ad_cost_work(x,k,l,dt,currIter+1,xop)
           case (rvStore)
              call cp_write_open(theAction%cpNum+jointCPCount)
              call revStatsUpdateCP(cp_fNumber()+1)
!$PLACEHOLDER_PRAGMA$ id=4
              call cp_close
           case (rvRestore)
              call cp_read_open(theAction%cpNum+jointCPCount)
!$PLACEHOLDER_PRAGMA$ id=6
              currIter=theAction%iteration
              call cp_close
           case (rvForward)
              call OAD_revPlain
              do currIter=currIter,theAction%iteration-1
                 call ad_cost_work(x,k,l,dt,currIter+1,xop)
              end do
              call OAD_revAdjoint
           case (rvUTurn)
              call OAD_revTape
              call ad_cost_work(x,k,l,dt,currIter+1,xop)
              call OAD_revAdjoint
              call ad_cost_work(x,k,l,dt,currIter+1,xop)
           end select   
           theAction=rvNextAction()
        end do
      end if 
      end subroutine template

      
