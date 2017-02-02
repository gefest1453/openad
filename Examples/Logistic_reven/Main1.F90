program hello

  !  use prefix
  ! use nfunct
  use line_searchs
  use BFGS
  !  use prog_bar
  implicit none
  integer:: nn,nmax,istart,iflag,big_iter,out_opts(2)
  integer,parameter::levels=4
  logical if_restart
  logical if_exceed/.false./
  double precision,dimension(:),allocatable :: grad,x0,x00,d,grad0,grad1,W00,direct
  double precision eps0,f,fp,f0,f1,lambda0,alpha,beta,lambda1,sigma

  logical if_fail
  integer i,j,ncomp,k,l,m,n
  integer,parameter::method=0
  character(len=1024)::s1,s2
  character(len=1024),parameter::file_prefix_cost='.'




  nmax=40;eps0=1e-6;beta=0.1;sigma=0.0000005;istart=1

  nn=3
  ncomp=0;
  allocate(d(nn))


  allocate(grad(nn),grad0(nn),grad1(nn),x0(nn),x00(nn))
  if (method.eq.0) then
     allocate(w00(nn*(2*levels+1)+2*levels))
  endif
  if (method.eq.1) then
     allocate(direct(nn))
  endif
  out_opts(1)=2
  out_opts(2)=0
  x0=0.0;
  call compute1(x0,f,grad)


  !    call compute(nn,x0,f,grad)
  !x0(im*jm*kb+1:im*jm*kb+inp_kh_lev_dim)=0
  !grad(im*jm*kb+1:im*jm*kb+inp_kh_lev_dim)=0
  lambda0=1d-1/norma(GRAD)
  do big_iter=istart,nmax
     if (method.eq.0) then
110     ncomp=ncomp+1
        call compute1(x0,f,grad)
        call LBFGS(Nn,levels,X0,F,Grad,.false.,grad1,out_opts,1d-6,1d-67,W00,IFLAG)
        if (iflag.eq.1) then
           goto 110
        endif
     else
        if ((istart.eq.big_iter).or.if_exceed) then
           x00=x0;grad0=grad;f0=f;direct=-1.0*grad0
           call LINE_SEARCH1(NN,X0,GRAD,F,direct,0.1/norma(grad),3,.false.,if_exceed,ncomp,if_fail)
           if (if_fail) then
              stop
           endif
           call save_res(nn,grad,x0,f)
        endif
        beta=norma(grad)**2/(direct.x.(grad-grad0))
        direct=beta*direct-grad
        x00=x0;grad0=grad;f0=f;
        call LINE_SEARCH1(NN,X0,GRAD,F,direct,lambda0,3,.false.,if_exceed,ncomp,if_fail)
        if (if_fail) then
           stop
        endif
        call save_res(nn,grad,x0,f)
     endif

  enddo



  !     do k=1,kb
  !        do i=1,im!
  !           do j=1,jm
  !!
  !             tracer(i,j)=X0(i-1+im*(j-1)+im*jm*(k-1)+1)
  !           enddo
  !        enddo
  !        call OUTGRD (DBLE(tracer(:,:)),DBLE(DUM),IM,&
  !             &jm,DBLE(0e+0*1.0),(dx*(im-1)),&
  !             &DBLE(0e+0*1.0),(dy*(jm-1)),open_filesg0(1,K,"FINAL"),&
  !             &DBLE(1.0e+0))
  !  open(file=open_filesg01(1,K,"FINAL"),unit=9732,action='write')
  !  call csv_write(9732,tracer)
  !  close(9732)           

  !enddo
  ! enddo

  write(0, *) 'END MINIMIZATION',ncomp
end program hello
 subroutine save_res(nn,grad,x0,f)
!    use prefix
 character(len=256),parameter::file_prefix='.'
    integer,intent(in)::nn
    doubleprecision,intent(in)::f
    doubleprecision,dimension(nn),intent(in)::x0,grad
    integer i
         open (file=trim(adjustl(file_PREFIX))//'/out.txt',unit=9732,action='write')
     do i=1,nn
        write (9732,*)grad(i)
     enddo
     open (file=trim(adjustl(file_PREFIX))//'/inp.txt',unit=9732,action='write')
     do i=1,nn
        write (9732,*)x0(i)
     enddo
     close(9732)
     open(file=trim(adjustl(file_PREFIX))//'/val.txt',unit=9732,action='write')
     write(9732,*)f
     close(9732)
     end subroutine
