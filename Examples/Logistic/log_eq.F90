     module log_eq
   contains
subroutine  ad_cost(x0,k,l,dt,N,summ)
doubleprecision x0,k,l,dt,r,x,time,xop,summ,x1
character*8,parameter::fnam="./1.txt"
integer N
!$openad INDEPENDENT(x0)
!$openad INDEPENDENT(k)
!$openad INDEPENDENT(l)
open(file=fnam,unit=111,action='read')

x=x0;summ=0
do i=1,N
r=logistic(x,k,l)
x1=x+dt*r
x=x+dt*(r+logistic(x1,k,l))/2.0
read (111,*) time,xop
summ=summ+(xop-x)**2
enddo

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
real*8:: x,r,x1
x=x0
do i=1,N
r=logistic(x,k,l)
x1=x+dt*r
x=x+dt*(r+logistic(x1,k,l))/2.0
print *,i*dt,x
enddo


  end subroutine
end module
