     module log_eq
   contains
subroutine  ad_cost(x,k,l,dt,Nstep,summ)
implicit none
doubleprecision x,k,l,dt,r,time,xop,summ,x1
integer Nstep,step0

!$openad INDEPENDENT(x)
!$openad INDEPENDENT(k)
!$openad INDEPENDENT(l)

do step0=1,Nstep
call ad_cost_work(x,k,l,dt,step0,summ)
!read (111,*) time,xop

enddo
!!print *,'aaa',dt*N,time,xop,x

!summ=(xop-x)**2
!$openad DEPENDENT(summ)


end subroutine

subroutine ad_cost_work(x,k,l,dt,step,summ)
implicit none
double precision :: x,k,l,dt,xop0,time
double precision summ
integer step,i,j
doubleprecision f(4)
character*128,parameter::fnam="./out.txt"
open(file=fnam,unit=111,action='read')
do i=1,step
read(111,*)time,xop0
enddo
close(111)
if (step.eq.1) then
summ=0.0
endif
if (step.eq.25) then
summ=summ+(x-xop0)**2
endif
f(1)=logistic(x,k,l)
f(2)=logistic(x+.5*dt*f(1),k,l)
f(3)=logistic(x+.5*dt*f(2),k,l)
f(4)=logistic(x+dt*f(3),k,l)
x=x+(dt/6)*(f(1)+f(4)+2*(f(2)+f(3)))


end subroutine
    function logistic(x,k,l) result(ret)
implicit none
real*8,intent(in) :: x,k,l
real*8 ret
ret=x*l*(1-x/k)
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
