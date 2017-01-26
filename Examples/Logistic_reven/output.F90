!module mod_output
! private output1,output2
!use gnuplot


!PRIVATE open_FILES,open_filescost,open_filesg,open_filesgtmp

!    interface output
!    module procedure output1,output2
!    end interface
!    CONTAINS



SUBROUTINE OUTPUT(istep,ETA,u,v,w,rho,rhoaver,tracer,D,DU,DV)
  !REFACTORED
  !USE MOD_MAIN
  !   USE CSV_FILE
  use types
  use prefix
  use OAD_ACTIVE
  use nfunct
  !        USE INP_OUT_FUNC
  !USE SUB_NRG3D
  USE GRID
  USE MOD_MAIN,ONLY:NUMSTEP,COST_FUNCTION_SAVE_COUNT
  IMPLICIT NONE
  INTEGER,PARAMETER::IUOUT=10011
  REAL(kind=SINGLE_REAL),DIMENSION(IM,JM,KB):: TRACER0
  real(kind=SINGLE_REAL),intent(in)::rhoaver
  REAL(kind=SINGLE_REAL),dimension(im,jm),intent(in)::D,DU,DV
  REAL(kind=SINGLE_REAL),DIMENSION(IM,JM,KB),intent(in)::u,v,w,rho
  real(kind=SINGLE_REAL),intent(in)::tracer(im,jm,kb)
  REAL(kind=SINGLE_REAL),DIMENSION(IM,JM),intent(in):: ETA
  REAL(kind=SINGLE_REAL) EKIN,VOLUME,MASS,EETA
  integer,intent(in)::istep
  LOGICAL IF_COST_POINT_INIT
  DATA IF_COST_POINT_INIT/.FALSE./
  CHARACTER(256)::FN
  integer i,j,k
  !        IF (.NOT. IF_COST_POINT_INIT) THEN
  !            IF_COST_POINT_INIT=.TRUE.
  !            WRITE(192,*) FLOOR(1.0*NUMSTEP/COST_FUNCTION_SAVE_COUNT)
  !        ENDIF

  !        IF (MOD(ISTEP,COST_FUNCTION_SAVE_COUNT).EQ.0) THEN
  !            DO I=1,SIZE(COST_FUNCTION_POINTS,DIM=1)
  !                WRITE(192,*)TRACER(COST_FUNCTION_POINTS(I,1),COST_FUNCTION_POINTS(I,2),COST_FUNCTION_POINTS(I,3))
  !            ENDDO

  !        ENDIF


  !INTEGE
  !  For each time step print out energies ++
  !
  IF (MOD(ISTEP,1).EQ.0) THEN
     ! WRITE(*,*)ISTEP,MAXLOC(U),MAXVAL(U)
     ! WRITE(*,*)ISTEP,MAXLOC(S),MAXVAL(S)
#ifndef OPENAD
     OPEN(981,FILE=TRIM(ADJUSTL(FILE_PREFIX))//'/BlackSea/'//TRIM(ADJUSTL("OUT.TXT")),ACCESS = 'APPEND')
     !CALL NRG3D(EKIN,VOLUME,MASS,EETA,ETA,D,DU,DV,u,v,w,rho,rhoaver)
     WRITE(*,*)ISTEP,EKIN,VOLUME,MASS
     WRITE(981,*)ISTEP,EKIN,VOLUME,MASS
     CLOSE(981)
#endif
     !   WRITE(*,*)ETA(1,13),ETA(4,13),ETA(10,13)
     !   WRITE(*,*)UA(1,14),UA(4,14),UA(10,14)
     !   WRITE(*,*)U(1,13,KB-2),U(2,13,KB-2),U(3,13,KB-2)
     !   WRITE(*,*)U(1,14,KB-2),U(2,14,KB-2),U(3,14,KB-2)
     !    WRITE(43,1005)ISTEP*DT/(3600.*24.),EKIN,VOLUME,MASS,EETA
     !   WRITE(83,1006)ISTEP*DT/(3600.*24.),ETA(10,20),U(10,20,1),V(10,20,1)
     !   WRITE(84,1006)ISTEP*DT/(3600.*24),ETA(13,11),U(13,11,1),V(13,11,1)
     !  WRITE(85,1006)ISTEP*DT/(3600.*24),ETA(23,19),U(23,19,1),V(23,19,1)
  ENDIF
1005 FORMAT(1X,F8.4,2X,E12.5,2X,E10.4,2X,E11.5,2X,F8.4)
1006 FORMAT(1X,F8.4,2X,E11.4,2X,E10.4,2X,E10.4)

  IF (MOD(ISTEP,(6*NDIVIS)).EQ.0) THEN
     !
     !   Compute velocities and RHO at 2m depth and at 5m depth
     !
     !  CALL PZLEVEL
     !  CALL OUTETAFEM
     !
     !
     !  Dump data along a section from the open boundary to Arnavaagen
     !
     ! CALL OUTSECTIONS
  ENDIF
  IF (ISTEP.EQ.1) THEN
     WRITE(86,*)'TITLE = "ARNA " '
     WRITE(86,*)'VARIABLES = "TIME" "S" "ETA" "U" "V" '
     WRITE(86,*)'ZONE T="ARNA"', 'I=',NUMSTEP
  ENDIF
  I = 24
  J = 16
  ! WRITE(86,*)ISTEP,S(I,J,1),ETA(I,J),U(I,J,1),V(I,J+1,1)
  !

  IF ((MOD(ISTEP,COST_FUNCTION_SAVE_COUNT).LE.0)) THEN

     DO  I=1,IM
        DO J=1,JM
           IF (D(I,J).NE.0.0) THEN
              TRACER0(I,J,:)=TRACER(I,J,:)/D(I,J)
           ELSE
              TRACER0(I,J,:)=0
           ENDIF
        ENDDO
     ENDDO
     WRITE(FN,*)ISTEP
     !        call graph(tracer,dx,dy,TRIM(ADJUSTL(FN))//'.GIF')
     DO K=1,KB
        write(*,*)'write fields for layer', k
        !OPEN(116,FILE=open_files(ISTEP,K,"U"))
        !CALL CSV_WRITE(116,U(:,:,K))
        !CLOSE(116)
        call OUTGRD (U(:,:,K),DUM,IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"U"),&
             &REAL(1.0e+0,SINGLE_REAL))




        !                OPEN(116,FILE=open_files(ISTEP,K,"V"))
        !                CALL CSV_WRITE(116,V(:,:,K))
        !                CLOSE(116)
        call OUTGRD (U(:,:,K),DUM,IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"U"),&
             &REAL(1.0e+0,SINGLE_REAL))



        !               OPEN(116,FILE=open_files(ISTEP,K,"W"))
        !                CALL CSV_WRITE(116,W(:,:,K))
        !               CLOSE(116)
        call OUTGRD(W(:,:,K),FSM,IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"W"),&
             &REAL(1.0*1.0,SINGLE_REAL ))

        !              OPEN(116,FILE=open_files(ISTEP,K,"TRACER"))
        !              CALL CSV_WRITE(116,TRACER(:,:,K))
        !              CLOSE(116)

        call OUTGRD ((TRACER0(:,:,K)),(FSM),IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,'tracer'),&
             &REAL(1.0e+0*1.0,SINGLE_REAL ))

     ENDDO
     !OPEN(116,FILE=open_files(ISTEP,1,"ETA"))
     !CALL CSV_WRITE(116,ETA)
     !CLOSE(116)
     call OUTGRD ((ETA),(FSM),IM&
          &,JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL)&
          &,REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"ETA"),&
          &REAL(1.0e+0*1.0,SINGLE_REAL ))


  ENDIF
END SUBROUTINE OUTPUT

SUBROUTINE OUTPUT2(istep,ETA,u,v,w,rho,rhoaver,tracer,D,DU,DV)
  !REFACTORED
  !USE MOD_MAIN
  !   USE CSV_FILE
  use types
  use prefix
  use OAD_ACTIVE
  use nfunct
  !        USE INP_OUT_FUNC
  !USE SUB_NRG3D
  USE GRID
  USE MOD_MAIN,ONLY:NUMSTEP,COST_FUNCTION_SAVE_COUNT
  IMPLICIT NONE
  INTEGER,PARAMETER::IUOUT=10011
  REAL(kind=SINGLE_REAL),DIMENSION(IM,JM,KB):: TRACER0
  real(kind=SINGLE_REAL),intent(in)::rhoaver
  REAL(kind=SINGLE_REAL),dimension(im,jm),intent(in)::D,DU,DV
  REAL(kind=SINGLE_REAL),DIMENSION(IM,JM,KB),intent(in)::u,v,w,rho
  real(kind=SINGLE_REAL),intent(in)::tracer(im,jm,kb)
  REAL(kind=SINGLE_REAL),DIMENSION(IM,JM),intent(in):: ETA
  REAL(kind=SINGLE_REAL) EKIN,VOLUME,MASS,EETA
  integer,intent(in)::istep
  LOGICAL IF_COST_POINT_INIT
  DATA IF_COST_POINT_INIT/.FALSE./
  CHARACTER(256)::FN
  integer i,j,k
  !        IF (.NOT. IF_COST_POINT_INIT) THEN
  !            IF_COST_POINT_INIT=.TRUE.
  !            WRITE(192,*) FLOOR(1.0*NUMSTEP/COST_FUNCTION_SAVE_COUNT)
  !        ENDIF

  !        IF (MOD(ISTEP,COST_FUNCTION_SAVE_COUNT).EQ.0) THEN
  !            DO I=1,SIZE(COST_FUNCTION_POINTS,DIM=1)
  !                WRITE(192,*)TRACER(COST_FUNCTION_POINTS(I,1),COST_FUNCTION_POINTS(I,2),COST_FUNCTION_POINTS(I,3))
  !            ENDDO

  !        ENDIF


  !INTEGE
  !  For each time step print out energies ++
  !
  IF (MOD(ISTEP,1).EQ.0) THEN
     ! WRITE(*,*)ISTEP,MAXLOC(U),MAXVAL(U)
     ! WRITE(*,*)ISTEP,MAXLOC(S),MAXVAL(S)
#ifndef OPENAD
     OPEN(981,FILE=TRIM(ADJUSTL(FILE_PREFIX))//'/BlackSea/'//TRIM(ADJUSTL("OUT.TXT")),ACCESS = 'APPEND')
     !CALL NRG3D(EKIN,VOLUME,MASS,EETA,ETA,D,DU,DV,u,v,w,rho,rhoaver)
     WRITE(*,*)ISTEP,EKIN,VOLUME,MASS
     WRITE(981,*)ISTEP,EKIN,VOLUME,MASS
     CLOSE(981)
#endif
     !   WRITE(*,*)ETA(1,13),ETA(4,13),ETA(10,13)
     !   WRITE(*,*)UA(1,14),UA(4,14),UA(10,14)
     !   WRITE(*,*)U(1,13,KB-2),U(2,13,KB-2),U(3,13,KB-2)
     !   WRITE(*,*)U(1,14,KB-2),U(2,14,KB-2),U(3,14,KB-2)
     !    WRITE(43,1005)ISTEP*DT/(3600.*24.),EKIN,VOLUME,MASS,EETA
     !   WRITE(83,1006)ISTEP*DT/(3600.*24.),ETA(10,20),U(10,20,1),V(10,20,1)
     !   WRITE(84,1006)ISTEP*DT/(3600.*24),ETA(13,11),U(13,11,1),V(13,11,1)
     !  WRITE(85,1006)ISTEP*DT/(3600.*24),ETA(23,19),U(23,19,1),V(23,19,1)
  ENDIF
1005 FORMAT(1X,F8.4,2X,E12.5,2X,E10.4,2X,E11.5,2X,F8.4)
1006 FORMAT(1X,F8.4,2X,E11.4,2X,E10.4,2X,E10.4)

  IF (MOD(ISTEP,(6*NDIVIS)).EQ.0) THEN
     !
     !   Compute velocities and RHO at 2m depth and at 5m depth
     !
     !  CALL PZLEVEL
     !  CALL OUTETAFEM
     !
     !
     !  Dump data along a section from the open boundary to Arnavaagen
     !
     ! CALL OUTSECTIONS
  ENDIF
  IF (ISTEP.EQ.1) THEN
     WRITE(86,*)'TITLE = "ARNA " '
     WRITE(86,*)'VARIABLES = "TIME" "S" "ETA" "U" "V" '
     WRITE(86,*)'ZONE T="ARNA"', 'I=',NUMSTEP
  ENDIF
  I = 24
  J = 16
  ! WRITE(86,*)ISTEP,S(I,J,1),ETA(I,J),U(I,J,1),V(I,J+1,1)
  !

  IF ((MOD(ISTEP,COST_FUNCTION_SAVE_COUNT).LE.0)) THEN

     DO  I=1,IM
        DO J=1,JM
           IF (D(I,J).NE.0.0) THEN
              TRACER0(I,J,:)=TRACER(I,J,:)/D(I,J)
           ELSE
              TRACER0(I,J,:)=0
           ENDIF
        ENDDO
     ENDDO
     WRITE(FN,*)ISTEP
     !        call graph(tracer,dx,dy,TRIM(ADJUSTL(FN))//'.GIF')
     DO K=1,KB
        write(*,*)'write fields for layer', k
        !OPEN(116,FILE=open_files(ISTEP,K,"U"))
        !CALL CSV_WRITE(116,U(:,:,K))
        !CLOSE(116)
        call OUTGRD (U(:,:,K),DUM,IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"U"),&
             &REAL(1.0e+0,SINGLE_REAL))


        !                OPEN(116,FILE=open_files(ISTEP,K,"V"))
        !                CALL CSV_WRITE(116,V(:,:,K))
        !                CLOSE(116)
        call OUTGRD (V(:,:,K),DVM,IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"V"),&
             &REAL(1.0*1.0,SINGLE_REAL) )

        !               OPEN(116,FILE=open_files(ISTEP,K,"W"))
        !                CALL CSV_WRITE(116,W(:,:,K))
        !               CLOSE(116)
        call OUTGRD(W(:,:,K),FSM,IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"W"),&
             &REAL(1.0*1.0,SINGLE_REAL ))

        !              OPEN(116,FILE=open_files(ISTEP,K,"TRACER"))
        !              CALL CSV_WRITE(116,TRACER(:,:,K))
        !              CLOSE(116)
        call OUTGRD ((TRACER0(:,:,K)),(FSM),IM,&
             &JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL),&
             &REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,'tracer'),&
             &REAL(1.0e+0*1.0,SINGLE_REAL ))

     ENDDO
     !OPEN(116,FILE=open_files(ISTEP,1,"ETA"))
     !CALL CSV_WRITE(116,ETA)
     !CLOSE(116)
     call OUTGRD ((ETA),(FSM),IM&
          &,JM,REAL(0e+0*1.0,SINGLE_REAL),REAL(dx*(im-1),SINGLE_REAL)&
          &,REAL(0e+0*1.0,SINGLE_REAL),REAL(dy*(jm-1),SINGLE_REAL),open_filesg(ISTEP,K,"ETA"),&
          &REAL(1.0e+0*1.0,SINGLE_REAL ))


  ENDIF
END SUBROUTINE OUTPUT2






!  END MODULE
