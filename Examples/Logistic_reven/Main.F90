
program main
  use log_eq
  doubleprecision :: x0,l,k,dt
  integer N
  N=40000
  dt=0.01
  x0=175.0
  l=0.18
  k=200.0
  call tabulate(x0,k,l,dt,N)
end program main
