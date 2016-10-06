     module log_eq
   contains
subroutine  ad_cost(x0,k,l,dt,N,summ)
doubleprecision x0,k,l,dt,r,x,time,xop,summ,x1
doubleprecision f(4)
character*8,parameter::fnam="./1.txt"
integer N
!$openad INDEPENDENT(x0)
!$openad INDEPENDENT(k)
!$openad INDEPENDENT(l)
open(file=fnam,unit=111,action='read')

x=x0;summ=0
do i=1,N
f(1)=logistic(x,k,l)
f(2)=logistic(x+.5*dt*f(1),k,l)
f(3)=logistic(x+.5*dt*f(2),k,l)
f(4)=logistic(x+dt*f(3),k,l)
x=x+(dt/6)*(f(1)+f(4)+2*(f(2)+f(3)))
read (111,*) time,xop
summ=summ+(xop-x)**2
enddo
!summ=(xop-x)**2
!$openad DEPENDENT(summ)
close(111)

end subroutine
    function logistic(x,k,l) result(ret)
real*8,intent(in) :: x,k,l
real*8 ret
ret=x*l*(1-x/k)
end function

  subroutine tabulate(x0,k,l,dt,N)
real*8,intent(in)::x0,k,l,dt
integer,intent(in):: N
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
