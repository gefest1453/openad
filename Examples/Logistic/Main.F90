
program main
use log_eq
doubleprecision :: x0,l,k,dt
integer N
N=10000
dt=0.1
x0=0.001
l=0.03
k=200.0
call tabulate(x0,k,l,dt,N)
end program
