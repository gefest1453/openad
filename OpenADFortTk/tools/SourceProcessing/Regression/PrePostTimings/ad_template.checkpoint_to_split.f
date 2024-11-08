

      subroutine template()
      use OAD_cp
      use OAD_tape
      use OAD_rev

c we may need these for the checkpointing
      use SIZE_mod
      use EEPARAMS_mod
      use PARAMS_mod
      use BAR2_mod
      use BARRIER_mod
      use CD_CODE_VARS_mod
      use CG2D_mod
      use CG3D_mod
      use DFILE_mod
      use DYNVARS_mod
      use EESUPPORT_mod
      use EOS_mod
      use EXCH_JAM_mod
      use EXCH_mod
      use FC_NAMEMANGLE_mod
      use FFIELDS_mod
      use GAD_mod
      use GLOBAL_MAX_mod
      use GLOBAL_SUM_mod
      use GMREDI_mod
      use GMREDI_TAVE_mod
      use GRID_mod
      use MPI_INFO_mod
      use SOLVE_FOR_PRESSURE3D_mod
      use SOLVE_FOR_PRESSURE_mod
      use SURFACE_mod
      use tamc_mod
      use tamc_keys_mod
      use cost_mod
      use g_cost_mod
      use ctrl_mod
      use ctrl_dummy_mod
      use ctrl_weights_mod
      use optim_mod
      use grdchk_mod
      
!$TEMPLATE_PRAGMA_DECLARATIONS

      integer :: cp_loop_variable_1,cp_loop_variable_2,
     +     cp_loop_variable_3,cp_loop_variable_4,cp_loop_variable_5

      type(modeType) :: our_orig_mode

      integer iaddr
      external iaddr


      if (our_rev_mode%switchedToCheckpoint) then

      if (our_rev_mode%arg_store) then 
         call cp_write_open()
!$PLACEHOLDER_PRAGMA$ id=8
         call cp_close()
      end if 
      if (our_rev_mode%arg_restore) then
         call cp_read_open()
!$PLACEHOLDER_PRAGMA$ id=9
         call cp_close()
      end if
      if (our_rev_mode%plain) then
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=1
         our_rev_mode=our_orig_mode
      end if
      if (our_rev_mode%tape) then 
         our_orig_mode=our_rev_mode
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.TRUE.
         our_rev_mode%adjoint=.FALSE.
         our_rev_mode%strictAnonymous=.FALSE.
!$PLACEHOLDER_PRAGMA$ id=2
         our_rev_mode%arg_store=.FALSE.
         our_rev_mode%arg_restore=.FALSE.
         our_rev_mode%plain=.FALSE.
         our_rev_mode%tape=.FALSE.
         our_rev_mode%adjoint=.TRUE.
!$PLACEHOLDER_PRAGMA$ id=3
         call openad_dumpAdjoint(myTime, myIter, myThid )
         our_rev_mode=our_orig_mode
      end if 

      else

         write(errorMessageUnit,'(A)') 
     +'OAD ERROR __SRNAME__: c2s not switched is the wrong logic'
        stop

      end if 
 

      end subroutine template
