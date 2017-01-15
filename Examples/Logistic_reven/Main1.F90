!#define  BGMP
!#define WITH_VALUES
#define CONG_gradients
#define LBFGS
#ifndef LBFGS
program hello
  
  
  use line_searchs
  !use prog_bar
  implicit none
  integer:: nn,nmax,istart
  logical if_restart,if_exceed
  double precision,dimension(:),allocatable :: grad,x0,x00,d
  
  double precision eps0,f,fp
#ifdef fast_gradients
  double precision:: beta,alpha
  
  double precision lambda0,f0
  DOUBLE PRECISION,PARAMETER::SIGMA=0.3e-16
  DOUBLE PRECISION,ALLOCATABLE::grad0(:)
  INTEGER I,J,K,ncomp
#ifndef DEBUG
  character(len=9):: ctrlname,costname
#else 
  character(len=9):: ctrlname,costname
#endif
  
  
#ifdef DEBUG
  ctrlname='inp.txt'
  costname='out.txt'
#endif
  nmax=40;eps0=1e-6;
  nn=3 
  ncomp=0;
  allocate(grad(nn),x0(nn),d(nn),X00(NN),grad0(nn))
  !  call optim_readdata(nn,ctrlname,.false.,f,x0)
  !  call optim_readdata(nn,costname,.false.,f,grad)
  x0=0.01
  call compute(x0,f,grad)
  lambda0=1/norma(GRAD)
  print *,lambda0
  do k=1,nmax
     write(0,'(a,e20.8,a,e20.8,a,i8,a,i8)'),'function val',f,'|grad|=',norma(grad),'iteration',k,'evaluations',ncomp
     beta=0.3;
     I=1;
     DO
        X00=X0-(LAMBDA0*(BETA**I))*GRAD
        call compute(nn,x00,f0,grad0,ctrlname,costname,grad)
        ncomp=ncomp+1
        write (0,*),'f=',f0,'|grad|=',norma(grad0),'alpha=',(grad/norma(grad).x.grad0/norma(grad0))
        
        print *,f0,f,sigma*lambda0*(beta**i)*norma(grad)**2,beta**i
        if (f0.le.f-sigma*lambda0*(beta**i)*norma(grad)**2) then
           
           x0=x00;f=f0;grad=grad0;
           exit
        endif
        
        i=i+1;
        
     ENDDO
     call optim_write_control(nn,x0)
  enddo
  
  
  
  call optim_write_control(nn,x0)
  write(0, *) 'END MINIMIZATION',ncomp
#else
  
  
#ifdef CONG_gradients
  double precision,dimension(:),allocatable :: gradp,xp,dp,d0,grad0
  double precision:: delta,mult,mulnp,mulng,beta,alpha
  doubleprecision,parameter::mu=0.7,omega=0.1
  
  
#elif (defined WITH_VALUES)
  doubleprecision,dimension(:,:),allocatable::x,g,pv,yy
  double precision,dimension(:),allocatable::ff,alpha,dir,beta
  double precision omega
#else  
  doubleprecision beta
  double precision,dimension(:,:),allocatable::x,g
  double precision,allocatable,dimension(:)::rho,alpha,q,r
#endif
  integer i,j,ncomp,k,l,m
  character(len=1024)::s1,s2
#ifndef DEBUG
  character(len=9):: ctrlname,costname
#else 
  character(len=9):: ctrlname,costname
#endif
  
  
#ifdef DEBUG
  ctrlname='inp.txt'
  costname='out.txt'
#endif
  !  call optim_numbmod(nn,ctrlname,costname)
  ncomp=0;nn=3
  allocate(grad(nn),x0(nn),d(nn))
#ifdef CONG_gradients
  allocate(x00(nn))
  
  mult=0.15;nmax=40
111 FORMAT (A,i3,E21.11E2,E21.11E2,E21.11E2,E21.11E2)
222 FORMAT (A,i3,E21.11E2,E21.11E2,E21.11E2)
  allocate(gradp(nn),xp(nn),dp(nn))
  allocate(grad0(nn),d0(nn))
  !  call optim_readdata(nn,ctrlname,.false.,f,x0)
  !  call optim_readdata(nn,costname,.false.,f,grad)
  m=100
  do k=1,m
     call restore_data(nn,x0);i=1
     
     call compute(x0,f,grad)
     !delta=0.5*mult*(grad.x.grad)/norma(grad)
     do while (i.le.nmax)
        !     mulnp=max(maxval(grad),-minval(grad))
        if (i.eq.1) then
           d=-grad
           write(0,222)'cycle',0,delta,f,-2*delta
        else
           beta=(grad.x.grad)/(gradp.x.gradp)
           !((grad.x.(grad-gradp))/((1-mu-omega)*(gradp.x.gradp)+mu*dp.x.(grad-gradp)-omega*(dp.x.gradp)))
           d=-grad+beta*gradp
        endif
        !     grad0=grad/mulnp
        !!    mulng=max(maxval(d),-minval(d))
        !    d0=d/mulng
        !    x00=x0
        !    alpha=-delta*(grad0.x.d0)/(d0.x.d0);
        !    alpha=alpha*(mulnp/mulng)
1917    write(0,*)
        xp=x0;dp=d;gradp=grad;fp=f
        
        j=36
        ncomp=0
        call LINE_SEARCH0(NN,X0,GRAD,F,d,0.1*f/abs(d.X.GRAD),j,ctrlname,costname,.false.,if_exceed,ncomp)
        if ((ncomp>j-1).and.(i>1)) then
           i=nmax
        endif
        write(0,*)'cycle',i,beta,f,d.x.grad,norma(grad-gradp)/norma(x0-xp)
        !     if (((grad.x.d)>0).or.(fp<f)) then 
        !        alpha=min(abs(mult*delta*(grad0.x.d0)/(d0.x.d0)*(mulnp/mulng)/10),abs(davidon(0d0,alpha,fp,f,gradp.x.d,grad.x.d)*&
        !            &(norma(grad-gradp)/norma(x0-xp))/((grad0.x.d0)/(d0.x.d0))))
        !       delta=mult*1/(norma(grad-gradp)/norma(x0-xp))
        !       ;x0=x00;grad=gradp;fp=f;mult=mult/4
        !       write(0,*)"Perelet"
        !       goto 1917
        !    endif
        
        !     delta=mult*1/(norma(grad-gradp)/norma(x0-xp))
        call optim_write_control(nn,x0)
        i=i+1;
     enddo
  enddo
#elif (defined WITH_VALUES)
  nmax=6
  allocate(x(nmax+1,nn),yy(nmax+1,nn),g(nmax+1,nn),pv(nmax+1,nn),alpha(nmax+1),ff(nmax+1),dir(nn),beta(nmax))
  x0=1d-4;grad=0.;d=0.;x=.0;g=.0;alpha=0;beta=0;ff=0;pv=0
  
  !  call optim_readdata(nn,ctrlname,.false.,f,x0)
  !  call optim_readdata(nn,costname,.false.,f,grad)
  call compute(x0,f,grad)
  x(1,:)=x0;
  ff(1)=f
  g(1,:)=grad;
  yy=0
  do i=2,nmax+1
     write(0,'(a,i4)')'cycle ',i
     j=66
     if (i.eq.2) then
        call LINE_SEARCH(NN,X0,GRAD,F,-grad,0.01*f/SQRT(GRAD.X.GRAD),j,ctrlname,costname,.false.,if_exceed,ncomp)
     else
        call LINE_SEARCH(NN,X0,GRAD,F,-dir,0.1*f/abs(dir.X.GRAD),j,ctrlname,costname,.false.,if_exceed,ncomp)
     endif
     if (if_exceed) then
        call optim_write_control(nn,x0)
        write(0,*)"wolfe condition incorrect,n func eval",ncomp
        stop
     endif
     x(i,:)=x0;
     g(i,:)=grad;
     ff(i)=f;
     omega=3*(ff(i-1)-ff(i))+3*(g(i,:)+g(i-1,:)).x.(x(i,:)-x(i-1,:))
     yy(i-1,:)=(1+omega/((g(i,:)-g(i-1,:)).x.(x(i,:)-x(i-1,:))))*(g(i,:)-g(i-1,:));
     if (i.ne.nmax+1) then
        dir=-h_mul(i,nn,nmax,yy,x,grad)
     endif
  enddo
#else
  
  allocate(x(nmax+1,nn),g(nmax+1,nn),rho(nmax+1),alpha(nmax+1),q(nn),r(nn))
  x0=1d-4;grad=0.;d=0.;x=.0;g=.0;rho=.0;alpha=.0;q=.0;r=.0
  call optim_readdata(nn,ctrlname,.false.,f,x0)
  call optim_readdata(nn,costname,.false.,f,grad)
  !    call compute(nn,x0,f,grad)
  x(1,:)=x0;
  g(1,:)=grad;
  inquire(file='main.sav',exist=if_restart)
  istart=1
  
  if(if_restart) then
     call load_state(nn,nmax,istart,grad,x0,d,x,g,rho,alpha,q,r,beta)
  endif
  do i=istart,nmax
     write(0,'(a,i4)')'cycle ',i
     write(s2,'(i4)')i
     s1='x0_'//trim(adjustl(s2))//'.csv'
#ifdef GRADIENT
     j=6
     
     call LINE_SEARCH(NN,X0,GRAD,F,-grad,f*1e-2/SQRT(GRAD.X.GRAD),j,ctrlname,costname,if_restart,if_exceed)
     !     call optim_write_controls(nn,x0)
     
#endif
#ifdef BGMP
     j=6
     call save_state(nn,nmax,i,grad,x0,d,x,g,rho,alpha,q,r,beta)
     if (i.eq.1) then
        call LINE_SEARCH(NN,X0,GRAD,F,-grad,0.05*f/SQRT(GRAD.X.GRAD),j,ctrlname,costname,if_restart,if_exceed,ncomp)
     else
        call LINE_SEARCH(NN,X0,GRAD,F,-r,0.01*f/SQRT(GRAD.X.GRAD),j,ctrlname,costname,if_restart,if_exceed,ncomp)
     endif
     if (if_exceed) then
        call optim_write_control(nn,x0)
        write(0,*)"wolfe condition incorrect,n func eval",ncomp
        stop
     endif
     !     call optim_write_controls(nn,x0)
     x(i+1,:)=x0;g(i+1,:)=grad
     rho(i)=1./((g(i+1,:)-g(i,:)).x.(x(i+1,:)-x(i,:)))
     q=grad
     do j=i,1,-1
        alpha(j)=rho(j)*((x(j+1,:)-x(j,:)).x.q)
        q=q-alpha(i)*(g(j+1,:)-g(j,:))
     enddo
     r=q
     do j=1,i
        beta=rho(j)*((x(j+1,:)-x(j,:)).x.r)
        r=r+(g(j+1,:)-g(j,:))*(alpha(j)-beta)
     enddo
     
#endif
     
  enddo
  
  
  
#endif
  call optim_write_control(nn,x0)
  write(0, *) 'END MINIMIZATION',ncomp
#endif
end program hello
#else /*LBFGS*/
program lbfgs
  use line_searchs
  double precision,dimension(3)::x,grad
  double precision:: f
  x(1)=190;x(2)=0.25;x(3)=120
  print *,"inp arg", x
  call compute(x,f,grad)
  print *,"Value f=",f
  print *,"GRAD f=",grad

end program lbfgs
#endif

subroutine    optim_write_control(nn,x0)
  integer,intent(in) ::nn
  doubleprecision,intent(in)::x0(nn)
  integer i;
  open (unit=1112,file="d:\2.txt")
  write(1112,*)nn
  do i=1,nn
     write(1112,*)x0(i)
  enddo
  close(1112)
end subroutine optim_write_control
subroutine restore_data(nn,x0)
  integer i
  integer ,intent(in)::nn
  doubleprecision,intent(out)::x0(nn)
  open (unit=1112,file="d:\2.txt")
  read(1112,*)i
  if (i.ne.nn) then
     stop
  endif
  do i=1,nn
     read(1112,*)x0(i)
  enddo
  close(1112)
  
end subroutine restore_data
subroutine load_state(nn,nmax,istart,grad,x0,d,x,g,rho,alpha,q,r,beta)
  integer,intent(inout)::nn,istart,nmax
  doubleprecision,intent(inout):: grad(nn),x0(nn),d(nn)
  doubleprecision,intent(inout):: x(nmax+1,nn),g(nmax+1,nn),rho(nmax+1),alpha(nmax+1),q(nn),r(nn)
  open(file='main.sav',action='read',unit=177)
  read(177,*),istart
  read(177,*),nmax
  read(177,*),grad
  read(177,*),x0
  read(177,*),d
  read(177,*),x
  read(177,*),g
  read(177,*),rho
  read(177,*),alpha
  read(177,*),q
  read(177,*),r
  read(177,*),beta
  close(177)
end subroutine load_state
subroutine save_state(nn,nmax,istart,grad,x0,d,x,g,rho,alpha,q,r,beta)
  integer::nn,istart,nmax
  doubleprecision grad(nn),x0(nn),d(nn)
  doubleprecision x(nmax+1,nn),g(nmax+1,nn),rho(nmax+1),alpha(nmax+1),q(nn),r(nn)
  open(file='main.sav',status='REPLACE',action='write',unit=177)
  write(177,*),istart
  write(177,*),nmax
  write(177,*),grad
  write(177,*),x0
  write(177,*),d
  write(177,*),x
  write(177,*),g
  write(177,*),rho
  write(177,*),alpha
  write(177,*),q
  write(177,*),r
  write(177,*),beta
  close(177)
  call system("sync")
  
end subroutine save_state


