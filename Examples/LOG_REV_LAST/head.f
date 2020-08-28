c-----------------------------------------------------------------------
      subroutine box_forward ( iloop )
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer  i, j
      integer iloop
       doubleprecision f(4)
        doubleprecision z,k,l
        z=tnew(1);
        l=tnew(2)
        k=tnew(3)
         f(1)=logistic(z,k,l)
        f(2)=logistic(z+.5*dt*f(1),k,l)
        f(3)=logistic(z+.5*dt*f(2),k,l)
         f(4)=logistic(z+dt*f(3),k,l)
         tnew(1)=z+(dt/6)*(f(1)+f(4)+2*(f(2)+f(3)))

         if (iloop.eq.n_max) then
         snew(1)=tnew(1)
         snew(2)=(tnew(1)- 176.15752879616772 )**2
         endif
         snew(3)=tnew(1)

c-- calculate dens
      end

c-----------------------------------------------------------------------
      subroutine box_final_state
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

      end

c-----------------------------------------------------------------------
      subroutine box_ini_fields
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:

c-- local variables:

c-- routine body

c-

      T(1)= 190.0
      T(2)= 0.25
      T(3)= 120

c-- initialise final state

c-- apply perturbations
      do l = 1, ndim
         t(l) = t(l) + xx(l)
         !s(l) = s(l) + xx(l+ndim)
      end do

c-- map onto model state
      do l = 1, ndim
         tNew(l) = t(l)
         sNew(l) = 0.0

      end do


      end

c-----------------------------------------------------------------------
      subroutine box_ini_params
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
        n_max=30


c--
c-- units are in CGS!
c--



      end

c-----------------------------------------------------------------------
      subroutine box_model_body
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:

c-- local variables:

      integer ilev1
      integer ilev2
      integer maxlev2

c-- routine body

c$openad INDEPENDENT(xx)

c-- inititialise this before loop to avoid additional forward run!
CADJ INIT tapelev2    = USER
CADJ INIT comlevfinal = COMMON, 1

      call box_ini_fields


      call box_model_loop

cph(
cph      print *, 'box_metric START'
cph      call box_metric
cph)
      call box_final_state

cph(
c-- taf seems to be confused by this
c-- It should recognize that there is no more dependency of
c-- tsvec on xx beyond box_final_state, shouldn't it?
c-- So, when tsvec rather than metric is the dependent variable,
c-- move this call to box_main, in order to hide it from TAF.
c-- Alternatively, I move box_metric before cost_finale_state:
c-- Now, taf correctly recognizes that metric isn't needed
c-- and kicks it out. That's good, but of course now it's missing.
cph      print *, 'box_metric START'
cph      call box_metric
cph)

c$openad DEPENDENT(tnew)
c$openad DEPENDENT(snew)

      end

      subroutine box_model_loop
      use all_globals_mod
      implicit none
      integer iloop
      do iloop = 1, n_max
         call box_forward ( iloop )
      enddo
      end

