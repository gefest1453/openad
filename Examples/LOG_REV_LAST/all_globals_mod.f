      module all_globals_mod
        integer ndim
        parameter ( ndim = 3 )

        integer n_max
        double precision::tnew(ndim) , snew(ndim),t(3)


        double precision xx(ndim)
        double precision,parameter::dt=0.01
      contains
         function logistic(x,k,l) result(ret)
        implicit none
        doubleprecision,intent(in) :: x,k,l
        doubleprecision ret
          ret=x*l*(1-x/k)
       end function
      end module

