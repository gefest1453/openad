program main
  use line_searchs
  use log_eq
!  use prog_bar
  implicit none
  integer:: nn,nmax,istart,ncomp,j,i
  logical if_restart,if_exceed
  double precision,dimension(:),allocatable :: grad,x0,x00,d,grad0,grad1
  double precision eps0,f,fp,f0,f1,lambda0,alpha,beta,lambda1,sigma
  character(len=9):: ctrlname,costname
  nn=3
  
  nmax=40;eps0=1e-6;beta=0.1;sigma=0.0000005
    allocate(d(nn))
  allocate(grad(nn),grad0(nn),grad1(nn),x0(nn),x00(nn))
  x0=0
  call compute(x0,f,grad)
  lambda0=1d-2/norma(GRAD)
      j=0;
   do i=istart,nmax
   
        print *,lambda0
     write(0,'(a,e20.8,a,e20.8,a,i5,a,i5,e20.8)'),'function val',f,'|grad|=',norma(grad),'iteration',i,'evaluations',ncomp,-(grad.x.grad)
     beta=0.1;
 
        X00=X0-(LAMBDA0*(BETA**j))*GRAD
        call compute(nn,x00,f0,grad0,ctrlname,costname,grad);
        


        ncomp=ncomp+1
        write (0,*),'f=',f0,'|grad|=',norma(grad0),'alpha=',(grad.x.grad0)/(norma(grad)*norma(grad0))
        lambda1=davidon(0d0,lambda0,f,f0,-(grad.x.grad)/norma(grad),-(grad0.x.grad)/norma(grad))

        if ((lambda1.gt.lambda0*beta**8)) then
           X00=X0-(LAMBDA1*(BETA**j))*GRAD
           call compute(nn,x00,f1,grad1,ctrlname,costname,grad)
             ncomp=ncomp+1
           if (f1.le.min(f,f0)) then
              write(0,*),'Succesful approximation',f1,f0
              f0=f1;grad0=grad1;lambda0=lambda1
           else
              write(0,*),'unSuccesful approximation',f1,f0
           endif


        endif

        if (f0.le.f-sigma*lambda0*(beta**j)*norma(grad)**2) then

           x0=X0-(LAMBDA0*(BETA**j))*GRAD;f=f0;grad=grad0;
           exit
        else
           write(0,*),'next iteration',j+1,beta**(j+1)
        endif

        j=j+1;
     enddo
   

end program



