
     module log_eq
     double precision:: xcurr,k_act,l_act,dt_act,summ
     integer Nmax_act
!     private xcurr,k_act,l_act,dt_act,Nmax_act
   contains
subroutine  ad_cost()
implicit none

integer iloop

!$openad INDEPENDENT(xcurr)
!$openad INDEPENDENT(k_act)
!$openad INDEPENDENT(l_act)

do iloop=1,Nmax_act
call COST_WORK(iloop)
!read (111,*) time,xop

enddo
!!print *,'aaa',dt*N,time,xop,x

!summ=(xop-x)**2
!$openad DEPENDENT(summ)


end subroutine

   subroutine setup_par(x0,l,k,dt,nmax)
   double precision x0,k,l,dt
   integer nmax
   xcurr=0.0
   xcurr=xcurr+x0
   l_act=0
   l_act=l_act+l
   k_act=k
   dt_act=dt
   Nmax_act=nmax
   end subroutine

subroutine COST_WORK(step)
implicit none
integer step,i,j
double precision:: f(4),time,xop
character*128,parameter::fnam="./out1.txt"


f(1)=logistic(xcurr,k_act,l_act)
f(2)=logistic(xcurr+.5*dt_act*f(1),k_act,l_act)
f(3)=logistic(xcurr+.5*dt_act*f(2),k_act,l_act)
f(4)=logistic(xcurr+dt_act*f(3),k_act,l_act)
xcurr=xcurr+(dt_act/6)*(f(1)+f(4)+2*(f(2)+f(3)))

if (step.eq.1) then
summ=0
endif

if (mod(step,10).eq.0) then
open(file=fnam,unit=111,action='read')
do i=1,step
read(111,*)time,xop
enddo
close(111)
summ=summ+(xcurr-xop)**2
endif


end subroutine
    function logistic(x,k,l) result(ret)
implicit none
real*8,intent(in) :: x,k,l
real*8 ret
ret=x*(l**2)*(1-x/k)
end function

  subroutine tabulate(x0,k,l,dt,N)
implicit none
real*8,intent(in)::x0,k,l,dt
integer,intent(in):: N
integer i
real*8:: x,r,x1,f(4)
x=x0
do i=1,N
f(1)=logistic(x,k,l)
f(2)=logistic(x+.5*dt*f(1),k,l)
f(3)=logistic(x+.5*dt*f(2),k,l)
f(4)=logistic(x+dt*f(3),k,l)
x=x+(dt/6)*(f(1)+f(4)+2*(f(2)+f(3)))
print *,i*dt,x
enddo


  end subroutine
end module
