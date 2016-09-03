      module adsize

!c
!c     parameters and arrays, necessary for adjoint and hessian calculation
!c
      SAVE
      integer ncmax
      parameter( ncmax = 200 )

      end module
      module size
!c
!c     size of the domain
!c
        SAVE
      integer nx, ny
      parameter ( nx = 20, ny = 20 )
      end module
      module parms
!c
!c     basic parameters
!c
        SAVE
      real g, earth, pi, deg2rad, rho0, invrho0
      parameter ( g = 9.81, earth = 6371000 )
      parameter ( pi = 3.14159265358979323844, deg2rad = pi/180. )
      parameter ( rho0 = 1028., invrho0 = 1./rho0 )
!c
!c     ``variable'' parameters
!c
!un      common /timeparameter/ dt, start_time, dt_dump, nt, ntspinup
!ju   all constant after init/read from netcdf
      real dt, start_time, dt_dump
      integer nt, ntspinup
!c     
!un      common /iterpar/ iteration
!ju   don't see how this is used properly 
      integer iteration
!c
!un      common /parms/ om, f0, beta, rini, ah, xstart, ystart
!ju   all constant after init/read from netcdf
      real om      
      real f0, beta
      real rini
      real ah
      real xstart, ystart
!c
!c     flags 
!c
!un      common /flags/ xperiodic, yperiodic, spherical, cartesian,
!un     &     quadfric, suppressio, fullio,
!un     &     initial_grad, grad_check, optimize, calc_hess
      logical xperiodic, yperiodic, spherical, cartesian, quadfric
      logical suppressio, fullio
      logical initial_grad, grad_check, optimize, calc_hess
!c
!c     names
!c
!un      common /names/ foutname, runname, depthfile, forcingfile,
!un     &     uinifile, vinifile, etainifile, ncdatafile, ncrestartfile
      character*(80) foutname
      character*(80) runname
      character*(80) depthfile, forcingfile
      character*(80) uinifile, vinifile, etainifile
      character*(80) ncdatafile, ncrestartfile
!c
      end module
      module vars
      use size
!c     dynamic variables velocity and sea-surface height
!un      common /vars/ u, v, eta
        SAVE
      real u(0:nx+1,0:ny+1)
      real v(0:nx+1,0:ny+1)
      real eta(0:nx+1,0:ny+1)
!c     
      end module
      module pfields
      use size
!c     parameter fields, like depth, grid etc.
!un      common /pfields/ fcoriu, fcoriv, depth, frict, 
!un     &     hu, hv, invhu, invhv
!c     coriolis parameter, depth, and friction coefficient 
!c     defined at eta grid point 
        SAVE
      real fcoriu(nx,ny) 
      real fcoriv(nx,ny) 
      real depth(0:nx+1,0:ny+1)
      real hu(0:nx+1,0:ny+1), hv(0:nx+1,0:ny+1)
      real invhu(0:nx+1,0:ny+1), invhv(0:nx+1,0:ny+1)
      real frict(0:nx+1,0:ny+1)
!c
!un      common /geom/ x, y, dx, dy, rx, ry, hy
      real x(0:nx+1), y(0:ny+1)
      real dx(0:nx), dy(0:ny)
      real rx(0:ny+1), ry, hy(0:ny+1)
!c     
!un      common /mask/ umask, vmask, etamask
      real umask(0:nx+1,0:ny+1)
      real vmask(0:nx+1,0:ny+1)
      real etamask(0:nx+1,0:ny+1)
!c     
!un      common /ini/ inidepth, uini, vini, etaini
      real inidepth(nx,ny)
      real uini(nx,ny)
      real vini(nx,ny)
      real etaini(nx,ny)
!c
!un      common /scales/ scaledepth, scaleu, scalev, scaleeta
      real scaledepth(nx,ny)
      real scaleu(nx,ny), scalev(nx,ny), scaleeta(nx,ny)

      end module
      module force
      use size 
!c     forcing fields for momentum equations
!un      common /force/ uforce, vforce
        SAVE
      real uforce(nx,ny)
      real vforce(nx,ny)
!c
      end module
     
      module data
      use size

!c
!c     data arrays
!c
!un      common /data/ eta_data, u_data, v_data, depth_data,
!un     &     eta_data_time, zonal_transport_data, nedt
        SAVE
      integer nedt, nedtmax
      parameter ( nedtmax = 1000 ) 
      real eta_data(nx,ny), u_data(nx,ny), v_data(nx,ny)
      real depth_data(nx,ny)
      real eta_data_time(nedtmax)
      real zonal_transport_data
      
      end module
      module weights
      use size
!c
!c     weight matices are all diagonal for now
!c
!un      common /weight_factors/ wf_depth, wf_eta, wf_u, wf_v,
!un     &     wf_zonal_transport, wf_lapldepth, wf_graddepth
        SAVE
      real wf_depth, wf_eta, wf_u, wf_v, wf_lapldepth, wf_graddepth
      real wf_zonal_transport
!c
!un      common /weights/ weight_depth, weight_eta, weight_u, weight_v, 
!un     &     weight_lapldepth, weight_graddepth, weight_zonal_transport
      real weight_depth(nx,ny)
      real weight_eta(nx,ny)
      real weight_u(nx,ny)
      real weight_v(nx,ny)
      real weight_lapldepth(nx,ny)
      real weight_graddepth(nx,ny)
      real weight_zonal_transport
!c
      end module
      module mini
!c     minimization parameters, mainly used by the minimization routines
!c     m1qn3
!un      common /mini_m1qn3/ epsg, df1, dxmin, niter, nsim, impres, mode
        SAVE
      integer niter, nsim, impres, mode
      real epsg, df1, dxmin
!c     lbfgs
!un      common /mini_lbfgsl/ eps_grad, pgtol, factr, iprint
      integer iprint
      double precision eps_grad, pgtol, factr
!c
      end module
      
      module size_small
!c
!c     size of the domain
!c
        SAVE
      integer nx, ny
      parameter ( nx = 20, ny = 8 )
      end module
# 1 "inifields.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "inifields.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine inifields

      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights

      implicit none

!un #include "size.inc"
!un #include "parms.inc"
!un #include "vars.inc"
!un #include "pfields.inc"
!un #include "force.inc"
!un #include "data.inc"
!un #include "weights.inc"
      
      integer ix, iy, ix2, iy2
      
c     set everything to zero
      nt = 0
      ntspinup = 0
      dt = 0.
      start_time = 0.
      dt_dump = 0.
c
c     iteration counter has to be initialized explicitly before starting
c     any minimization, set to some impossible value here.
c      
      iteration = -9999
c
      rini = 0.
      om = 0.
      f0 = 0.
      beta = 0. 
      xstart = 0.
      ystart = 0. 
c     
      xperiodic = .false.
      yperiodic = .false.
      spherical = .false.
      cartesian = .false. 
      quadfric = .false.
c
      suppressio = .false.
      fullio = .false.
c
      initial_grad = .false.
      grad_check = .false.
      optimize = .false.
      calc_hess = .false.
c
      foutname = ' '
      runname = ' '
      depthfile = ' '
      forcingfile = ' '
      uinifile = ' '
      vinifile = ' '
      etainifile = ' '
      ncdatafile = ' '
c
      wf_depth = 0.
      wf_eta = 0.
      wf_u = 0.
      wf_v = 0.
      wf_zonal_transport = 0.
      wf_lapldepth = 0.
      wf_graddepth = 0.
c
c$openad xxx simple loop
      do ix = 0, nx+1
         x(ix) = 0.
      end do
c$openad xxx simple loop
      do iy = 0, ny+1
         y(iy) = 0.
      end do
c$openad xxx simple loop
      do ix = 0, nx
         dx(ix) = 0.
      end do
c$openad xxx simple loop
      do iy = 0, ny
         dy(iy) = 0.
      end do
c$openad xxx simple loop
      do iy = 0, ny+1
         rx(iy) = 0.
      end do
      ry = 0.
c$openad xxx simple loop
      do iy = 0, ny+1
         hy(iy) = 0.
      end do
      dt = 0.
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            uforce(ix,iy) = 0.
            vforce(ix,iy) = 0.
            fcoriu(ix,iy) = 0.
            fcoriv(ix,iy) = 0.
            inidepth(ix,iy) = 0.
            uini(ix,iy) = 0.
            vini(ix,iy) = 0.
            etaini(ix,iy) = 0.
c     
            scaledepth(ix,iy) = 0.
c     data
            u_data(ix,iy) = 0.
            v_data(ix,iy) = 0.
            eta_data(ix,iy) = 0.
            depth_data(ix,iy) = 0.
            weight_u(ix,iy) = 0.
            weight_v(ix,iy) = 0.
            weight_eta(ix,iy) = 0.
            weight_lapldepth(ix,iy) = 0.
            weight_graddepth(ix,iy) = 0.
         end do
      end do
c$openad xxx simple loop
      do iy = 1, ny
c         do iy2 = 1, ny
            do ix = 1, nx
c               do ix2 = 1, ny
                  weight_depth(ix,iy) = 1.
c               end do
            end do
c         end do
      end do
c
      zonal_transport_data = 0.
      weight_zonal_transport = 0.
c
c$openad xxx simple loop
      do ix = 0, nx+1
         do iy = 0, ny+1
            depth(ix,iy) = 0.
            frict(ix,iy) = 0.
            u(ix,iy) = 0.
            v(ix,iy) = 0.
            eta(ix,iy) = 0.
            umask(ix,iy) = 0.
            vmask(ix,iy) = 0.
            etamask(ix,iy) = 0.
            hu(ix,iy) = 0.
            hv(ix,iy) = 0.
            invhu(ix,iy) = 0.
            invhv(ix,iy) = 0.
         end do
      end do
c     
      end ! subroutine inifields
# 1 "readparms.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "readparms.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine readparms

      implicit none
c
c     general parameters and flags from file
c
      call read_data_file
c
c     read depth, friction, and forcing from a file
c     likewise the initial conditions
c
      call read_data_fields
c
      call prep_depth
c
      call check_cfl
c
      call make_masks
c
      call ini_scales
c
      call prep_coriolis

      return
      end ! subroutine readparms

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_data_file

      use size
      use parms
      use pfields
      use mini
      use weights

      implicit none

!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"
!un #include "mini.inc"
!un #include "weights.inc"

      integer ix, iy
      real delx, dely
c
c     general parameters and flags from file "data"
c
cph      open(20,file='data',form='formatted',status='old',action='read')
      open(20,file='data',form='formatted',status='old')

      read(20,*)
      read(20,*) nt, ntspinup, dt, start_time, dt_dump
      read(20,*)
c      read(20,*) rini, ah, f0, beta
      read(20,*) rini, f0, beta
      read(20,*)
      read(20,*) xstart, ystart
      read(20,*)
      read(20,*) delx, dely

c$$$      print*,'nt=',nt
c$$$      print*,'dt=',dt
c$$$      print*,'start_time=',start_time
c$$$      print*,'rini=',rini
c$$$      print*,'f0=',f0
c$$$      print*,'beta=',beta
c$$$      print*,'xstart=',xstart
c$$$      print*,'ystart=',ystart
c$$$      print*,'delx=',delx
c$$$      print*,'dely=',dely

      read(20,*)
      read(20,*) xperiodic, yperiodic, spherical, cartesian
      read(20,*)
      read(20,*) fullio, suppressio
      read(20,*)
      read(20,*) initial_grad, grad_check, optimize, calc_hess

c      print*,'xperiodic,yperiodic,spherical,cartesian',
c     & xperiodic,yperiodic,spherical,cartesian
c      print*,'fullio,suppressio', fullio, suppressio

      read(20,*)
      read(20,'(A80)') ncdatafile
      read(20,*)
      read(20,'(A80)') ncrestartfile
      read(20,*)
      read(20,'(A80)') foutname
      read(20,*)
      read(20,'(A80)') runname
      read(20,*)
      read(20,*) wf_depth, wf_eta, wf_u, wf_v, wf_zonal_transport,
     &     wf_lapldepth, wf_graddepth

c      print*, foutname, runname, depthfile, forcingfile, uinifile
c      print*, vinifile, etainifile, datafile

      read(20,*)
      read(20,*) nsim
      read(20,*)
      read(20,*) epsg, df1, dxmin, niter, impres, mode
      read(20,*)
      read(20,*) eps_grad, pgtol, factr, iprint
      
      close(20)
c
c     end parameter file
c
c     some first checks for parameter consistency
c      if ( mod(nt,ntcp1) .ne. 0. ) then
c         print *, 'nt must be a multiple of ntcp1 =', ntcp1
c         stop 'in readparms'
c      end if
      if ( spherical .and. cartesian ) then
         print *, 'grid specification is ambiguous'
cju         stop 'in read_data_file' 
      else if ( .not. spherical .and. .not. cartesian ) then
         print *, 'grid specification is ambiguous'
cju         stop 'in read_data_file'
      end if
c
      if ( spherical .and. yperiodic ) then
         print *, 'spherical grid and periodic boundary conditions'
         print *, 'in latitude do not make sense'
cju         stop 'in read_data_file'
      end if
c     with periodic meridional boundary condition only an f-plane makes sense
      if ( yperiodic .and. ( spherical .or. 
     &     ( cartesian .and. beta .ne. 0 ) ) ) then
         print *, 'yperiodic boundaries only make sense on an f-plane'
cju         stop 'in read_data_file'
      end if
c
c     grid steps
c
      if ( spherical ) then
         do iy = 0, ny+1
            y(iy) = dely/earth*(real(iy)-.5)/deg2rad + ystart 
         end do
         do ix = 0, nx+1
            x(ix) = delx/earth*(real(ix)-.5)/deg2rad + xstart
         end do
      else if ( cartesian ) then
         do ix = 0, nx+1
            x(ix) = delx*(real(ix)-0.5)
         end do
         do iy = 0, ny+1
            y(iy) = dely*(real(iy)-0.5)
         end do
      end if
c     
      do ix = 0, nx
         dx(ix) = x(ix+1)-x(ix)
      end do
c
      do iy = 0, ny
         dy(iy) = y(iy+1)-y(iy)
      end do
c
c     once, x and y are set, these fields follow
c
      do iy = 1, ny
         if ( spherical ) then
            rx(iy) = earth*cos(.5*(y(iy)+y(iy+1))*deg2rad)*deg2rad
         else if ( cartesian ) then
            rx(iy) = 1.
         end if
      end do
c
      if ( spherical ) then
         ry = earth*deg2rad
      else if ( cartesian ) then
         ry = 1.
      end if
c
      do iy = 1, ny+1
         if ( spherical ) then
            hy(iy) = cos(y(iy)*deg2rad)
         else if ( cartesian ) then
            hy(iy) = 1.
         end if
      end do
c
c     
c     find out about data
c
      call determine_data_time( ncdatafile )
c
      return
      end !subroutine read_data_file

c$openad XXX Template OADrts/ad_template.split.f
      subroutine boundary_conditions( nx, ny, field, 
     &     xperiodic, yperiodic )
c     
c     implement boundary conditions, only makes sense for fields that are
c     defined like this: 0:nx+1, 0:ny+1
c     
      implicit none
c     interface
      integer nx, ny
      real field(0:nx+1,0:ny+1)
      logical xperiodic, yperiodic
c     end interface

c     locals
      integer ix, iy
      
      if ( xperiodic ) then
         do iy = 0, ny+1
            field(0,iy)    = field(nx,iy)
            field(nx+1,iy) = field(1,iy)
         end do
      else
         do iy = 0, ny+1
            field(0,iy)    = 0.
            field(nx+1,iy) = 0.
         end do
      end if
      if ( yperiodic ) then
         do ix = 0, nx+1
            field(ix,0)    = field(ix,ny)
            field(ix,ny+1) = field(ix,1)
         end do
      else
         do ix = 0, nx+1
            field(ix,0)    = 0.
            field(ix,ny+1) = 0.
         end do
      end if
      if ( xperiodic .and. yperiodic ) then
         print *, 'boundary_conditions: ',
     &        'make sure that the corners are handled correctly'
      end if

      return
      end !subroutine boundary_conditions

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_data_fields

      use size
      use parms
      use pfields
      use force

c
c     read depth, friction, and forcing from a file
c     likewise the initial conditions
c
      implicit none
!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"
!un #include "force.inc"
c     locals
      integer ix, iy
      real x_in(1:nx), y_in(1:ny)
      logical exists
!nt
      real :: mytime
      character*(80) :: str1, str2, str3
      integer mynx, myny
      mynx = nx ; myny = ny
      mytime = -1.

      do ix = 1, nx
         x_in(ix) = 0.
      end do
      do iy = 1, ny
         y_in(iy) = 0.
      end do

      inquire(file=ncdatafile,exist=exists)
      if ( .not. exists ) then
         print *, ncdatafile, ' not found, cannot continue'
cju         stop 'in read_data_fields'
      else
c$$$         call read_vector_netcdf( ncdatafile, 'X', nx, x_in )
c$$$         do ix = 1, nx
c$$$            x(ix) = x_in(ix)
c$$$         end do
c$$$         call read_vector_netcdf( ncdatafile, 'Y', ny, y_in )
c$$$         do iy = 1, ny
c$$$            y(iy) = y_in(i)
c$$$         end do
         str1 = 'depth' ; str2 = 'frict'
         call read_field( ncdatafile, mytime, str1, inidepth )
         call read_extended_field( ncdatafile, str2, frict )
         if ( rini .ne. 0 ) then
            print *, 'rini = ', rini
            print *, 'will overwrite frict with rini'
            do ix = 1, nx
               do iy = 1, ny
                  frict(ix,iy) = rini
               end do
            end do
         end if
         call boundary_conditions(mynx,myny,frict,xperiodic,yperiodic) 
c     forcing
         str1 = 'uforce' ; str2 = 'vforce'
         call read_field( ncdatafile, mytime, str1, uforce )
         call read_field( ncdatafile, mytime, str2, vforce )
c     initial conditions
         if ( start_time .eq. 0. ) then 
            print *, 'cold start from initial fields'
            str1 = 'uini' ; str2 = 'vini' ; str3 = 'etaini'
            call read_field( ncdatafile, start_time, str1, uini )
            call read_field( ncdatafile, start_time, str2, vini )
            call read_field( ncdatafile, start_time, str3, etaini )
         else 
            print *, 'warm restart from time ', start_time
            print *, 'in restart file ', ncrestartfile
            inquire( file = ncrestartfile, exist = exists ) 
            if ( .not. exists ) then
               print *, ncrestartfile, ' not found'
cju               stop 'in read_data_fields'
            else 
               str1 = 'U' ; str2 = 'V' ; str3 = 'ETA'
               call read_field( ncrestartfile, start_time, str1, uini )
               call read_field( ncrestartfile, start_time, str2, vini )
               call read_field( ncrestartfile, start_time, str3, 
     &              etaini )
            end if
         end if
      end if ! ncdatafile exists
c
      return
      end !subroutine read_data_fields

c$openad XXX Template OADrts/ad_template.split.f
      subroutine prep_depth

      use size
      use parms
      use pfields
      
c
c     prepare control variable depth, boundary conditions, etc. from inidepth
c     also scaling for control variable
c
      implicit none

!nt #include "size.inc"
!nt #include "parms.inc"
!nt #include "pfields.inc"

      integer ix, iy
      real maxdepth
 !nt
      integer mynx, myny
      mynx = nx ; myny = ny

c
c     prepare depth
c
      maxdepth = 0.
      do iy = 1, ny
         do ix = 1, nx
            if ( inidepth(ix,iy) .gt. maxdepth ) then
               maxdepth = inidepth(ix,iy)
            end if
         end do
      end do
      do ix = 1, nx
         do iy = 1, ny
            depth(ix,iy) = inidepth(ix,iy)
            scaledepth(ix,iy) = inidepth(ix,iy)
c     perturb initial depth a little to test algorithm
            if ( depth(ix,iy) .lt. maxdepth ) then
c               depth(ix,iy) = .9*(depth(ix,iy)-maxdepth)+maxdepth
            end if
         end do
      end do
c
c     assign values to data_depth
c
      call read_depth_data
c
c     I guess this is completely superfluous, since boundaries are handled
c     in calc_depth_uv for hu and hv
c
      call boundary_conditions(mynx,myny,depth,xperiodic,yperiodic) 
c
      return
      end !subroutine prep_depth

c$openad XXX Template OADrts/ad_template.split.f
      subroutine ini_scales

      use size
      use pfields

c
c     compute scales for initial conditions
c
      implicit none

!un #include "size.inc"
!un #include "pfields.inc"

      integer ix, iy
      real varu, varv, vareta
 !nt
      integer mynx, myny
      mynx = nx ; myny = ny

      call variance( mynx, myny, uini, umask, varu )
      if ( varu .eq. 0. ) then
         varu = 1.
      end if
      call variance( mynx, myny, vini, vmask, varv )
      if ( varu .eq. 0. ) then
         varv = 1.
      end if
      call variance( mynx, myny, etaini, etamask, vareta )
      if ( varu .eq. 0. ) then
         vareta = 1.
      end if
      do iy = 1, ny
         do ix = 1, nx
            scaleu(ix,iy)   = sqrt(varu)*umask(ix,iy)
            scalev(ix,iy)   = sqrt(varv)*vmask(ix,iy)
            scaleeta(ix,iy) = sqrt(vareta)*etamask(ix,iy)
         end do
      end do
c
      return
      end ! subroutine ini_scales

c$openad XXX Template OADrts/ad_template.split.f
      subroutine prep_coriolis

      use size
      use parms
      use pfields

      implicit none

!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"
c     locals
      integer ix, iy, my
      real faux
      real fcori(0:nx+1,0:ny+1)
c
      my = nint(real(ny)/2.)
c
c     coriolis
c
      do iy = 0, ny+1
         if ( spherical ) then
            faux = 2*om*sin(y(iy)*deg2rad)
         else if ( cartesian ) then
            faux = f0 + beta*(y(iy)-y(my))
         end if
         do ix = 0, nx+1
            fcori(ix,iy) = faux
         end do
      end do
c     now the two fields on u and v points
      do ix = 1, nx
         do iy = 1, ny
c     u point
            faux = vmask(ix,iy)   + vmask(ix-1,iy)
     &           + vmask(ix,iy+1) + vmask(ix-1,iy+1)
            if ( faux .eq. 0. ) then
               faux = 0.
            else
c               faux = 1./faux*umask(ix,iy)
               faux = .25*umask(ix,iy)
            end if
            fcoriu(ix,iy) = .5*( fcori(ix,iy) + fcori(ix-1,iy) )*faux
c     v point
            faux = umask(ix,iy)   + umask(ix+1,iy)
     &           + umask(ix,iy-1) + umask(ix+1,iy-1)
            if ( faux .eq. 0. ) then
               faux = 0.
            else
c               faux = 1./faux*vmask(ix,iy)
               faux = .25*vmask(ix,iy)
            end if
            fcoriv(ix,iy) = .5*( fcori(ix,iy) + fcori(ix,iy-1) )*faux
         end do
      end do
c
      return
      end !subroutine readparms

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_masks

      use size
      use parms
      use pfields

c     
c     land sea masking follows from depth
c
      implicit none
!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"

      integer ix, iy
      real mindepth
!nt
      integer mynx, myny
      mynx = nx ; myny = ny

      do ix = 1, nx
         do iy = 1, ny
!nt         if ( min(depth(ix,iy),depth(ix-1,iy)) .ne. 0 ) then
            if (depth(ix,iy) .lt. depth(ix-1,iy)) then 
               mindepth=depth(ix,iy)
            else 
               mindepth=depth(ix-1,iy)   
            endif
            if ( mindepth .ne. 0 ) then
               umask(ix,iy) = 1.
            else
               umask(ix,iy) = 0.
            end if
         end do
      end do
      do ix = 1, nx
         do iy = 1, ny
!nt         if ( min(depth(ix,iy),depth(ix,iy-1)) .ne. 0 ) then
            if ( depth(ix,iy) .lt. depth(ix,iy-1)) then 
               mindepth=depth(ix,iy)
            else 
               mindepth=depth(ix,iy-1)
            end if 
            if ( mindepth .ne. 0 ) then
               vmask(ix,iy) = 1.
            else
               vmask(ix,iy) = 0.
            end if
         end do
      end do
      do ix = 1, nx
         do iy = 1, ny
            if ( depth(ix,iy) .ne. 0 ) then
               etamask(ix,iy) = 1.
            else
               etamask(ix,iy) = 0.
            end if
         end do               
      end do
      call boundary_conditions(mynx,myny,umask,xperiodic,yperiodic)
      call boundary_conditions(mynx,myny,vmask,xperiodic,yperiodic)
c$$$      if ( xperiodic ) then
c$$$         do iy = 0, ny+1
c$$$            umask(0,iy)    = umask(nx,iy) ! not really necessary
c$$$            umask(nx+1,iy) = umask(1,iy)
c$$$c     for coriolis we need these
c$$$            vmask(0,iy)    = vmask(nx,iy)
c$$$            vmask(nx+1,iy) = vmask(1,iy)
c$$$         end do
c$$$      end if
c$$$      if ( yperiodic ) then
c$$$         do ix = 0, nx+1
c$$$            vmask(ix,0)    = vmask(ix,ny) ! not really necessary
c$$$            vmask(ix,ny+1) = vmask(ix,1)
c$$$c     for coriolis we need these
c$$$            umask(ix,0)    = umask(ix,ny)
c$$$            umask(ix,ny+1) = umask(ix,1)
c$$$         end do
c$$$      end if
c$$$      if ( xperiodic .and. yperiodic ) then
c$$$         print *, 'make sure the masks are set correctly'
c$$$      end if
c
      return
      end !subroutine make_masks

c$openad XXX Template OADrts/ad_template.split.f
      subroutine variance( nx, ny, f, fmask, varf )

      implicit none

c     interface
      integer nx, ny
      real f(nx,ny), fmask(0:nx+1,0:ny+1)
      real varf
c     end interface
c     locals
      integer k, ix, iy
      real meanf

      varf = 0.
      meanf = 0.
      k = 0
      do iy = 1, ny
         do ix = 1, nx
            if ( fmask(ix,iy) .ne. 0. ) then
               meanf = meanf + f(ix,iy)
               k = k+1
            end if
         end do
      end do
      if ( k .ne. 0 ) then
         meanf = meanf/real(k)
      end if
      do iy = 1, ny
         do ix = 1, nx
            varf = varf+(f(ix,iy)-meanf)**2*fmask(ix,iy)
         end do
      end do
      if ( k .gt. 1 ) then
         varf = varf/real(k-1)
      end if

      return
      end ! subroutine variance

c$openad XXX Template OADrts/ad_template.split.f
      subroutine check_cfl

      use size
      use parms
      use pfields

      implicit none
!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"

c     local variables
      integer ix, iy
      real mdep, mdx, mdy
      real cflx, cfly, wavespeed
      real minimum, maximum

      mdep = 0.
      mdx = 1.e23
      mdy = 1.e23

      do ix = 1, nx
!nt      mdx = min(mdx,dx(ix))
         if ( mdx .lt. dx(ix)) then 
            minimum=mdx
         else
            minimum=dx(ix)
         end if
         mdx = minimum
      end do
      do iy = 1, ny
!nt: should be mdy
!nt      mdy = min(mdx,dy(iy))
         if ( mdy .lt. dy(iy)) then 
            minimum=mdy
         else
            minimum=dy(iy)
         end if 
         mdy = minimum
      end do
      do ix = 1, nx
         do iy = 1, ny
!nt         mdep = max(mdep,depth(ix,iy))
            if (mdep .gt. depth(ix,iy) ) then 
               maximum=mdep
            else
               maximum=depth(ix,iy)
            end if 
            mdep = maximum
         end do
      end do
      wavespeed = sqrt(g*mdep)
      cflx = wavespeed*dt/mdx
      cfly = wavespeed*dt/mdy

      print *, 'rough check of CLF criterion:'
      if ( ( cflx .ge. 1. ) .or. ( cfly .ge. 1. ) ) then
         print *, 'warning: CLF criterion not met'
         print *, 'sqrt(g*max(depth))*dt/min(dx) = ', cflx
         print *, 'sqrt(g*max(depth))*dt/min(dy) = ', cfly
      else
         print *, 'OK'
      end if
      
      return
      end ! subroutine check_cfl

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_extended_field( ncdatafile, fname, field )

      use size

c     
c     wrapper for importing a data field (nx,ny) from netcdf file
c     as opposed to read_field, this subroutine deals with fields whose
c     dimensions are (0:nx+1) X (0:ny+1),
c     also time parameter is omitted
c     
      implicit none
!un #include "size.inc"
c     interface
      character*(80) ncdatafile, fname
      real field(0:nx+1,0:ny+1)
c     end interface
c     locals
      integer ix, iy
      real f_in(1:nx,1:ny)
      logical exists
!nt
      integer mynx, myny
      mynx = nx ; myny = ny

      do ix = 1, nx
         do iy = 1, ny
            f_in(ix,iy) = 0.
         end do
      end do
      call read_field_netcdf( ncdatafile, fname, mynx, myny, f_in )
      do ix = 1, nx
         do iy = 1, ny
            field(ix,iy) = f_in(ix,iy)
         end do
      end do

      return
      end !subroutine read_extended_field

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_field( ncdatafile, start_time, fname, field )

      use size

c     
c     wrapper for importing a data field (nx,ny) from netcdf file
c     
      implicit none
!un #include "size.inc"
c     interface
      character*(80) ncdatafile, fname
      real start_time
      real field(1:nx,1:ny)
c     end interface
c     locals
      integer ix, iy
      real f_in(1:nx,1:ny)
      logical exists
!nt
      integer :: mynx = nx, myny = ny
      
      do ix = 1, nx
         do iy = 1, ny
            f_in(ix,iy) = 0.
         end do
      end do
      if ( start_time .le. 0. ) then
         call read_field_netcdf( ncdatafile, fname, mynx, myny, f_in )
      else
         call read_snap_netcdf( ncdatafile, 
     &        start_time, mynx, myny, fname, f_in )
      end if
      do ix = 1, nx
         do iy = 1, ny
            field(ix,iy) = f_in(ix,iy)
         end do
      end do

      return
      end !subroutine read_field
# 1 "iowrapper.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "iowrapper.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine ini_io

      use size
      use parms
      use pfields
      use weights

      implicit none

!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"
!un #include "weights.inc"

      integer ix, iy
      real xout(nx), yout(ny)
!nt
      character*(80) :: str1, str2, str3, str4
      integer :: mynx, myny
      real :: myearth
      mynx = nx ; myny = ny
      myearth = earth

      if ( fullio ) then
         print *, 'initializing I/O'
      end if

      call create_netcdf(foutname,runname,mynx,myny)
      if ( spherical ) then
         str1 = 'grid_type' ; str2 = 'spherical'
         str3 = 'earth_radius' ; str4 = 'Omega'
         call add_gatta_netcdf(foutname,str1,str2)
         call add_gattr_netcdf(foutname,str3,myearth)
         call add_gattr_netcdf(foutname,str4,om)
      else if ( cartesian ) then
         str1 = 'grid_type' ; str2 = 'cartesian'
         str3 = 'f0' ; str4 = 'beta'
         call add_gatta_netcdf(foutname,str1,str2)
         call add_gattr_netcdf(foutname,str3,f0)
         call add_gattr_netcdf(foutname,str4,beta)
      end if
      str1 = 'r_ini' ; str2 = 'time_step'
      call add_gattr_netcdf(foutname,str1,rini)
      call add_gattr_netcdf(foutname,str2,dt)
      if ( xperiodic ) then
         str1 = 'zonal_boundary_conditions' ; str2 = 'periodic'
         call add_gatta_netcdf(foutname, str1, str2)
      end if
      if ( yperiodic ) then
         str1 = 'meridional_boundary_conditions' ; str2 = 'periodic'
        call add_gatta_netcdf(foutname, str1, str2)
      end if
      str1 = 'data_files'
      str2 = ncdatafile//' '//depthfile//' '//forcingfile//' '//
     &     uinifile//' '//vinifile//' '//etainifile
      call add_gatta_netcdf(foutname,str1,str2)
      if ( start_time .ne. 0 ) then
         str1 = 'restart_file'
         call add_gatta_netcdf(foutname,str1,ncrestartfile)
      end if
      str1 = 'ntspinup' ; str2 = 'wf_depth'
      str3 = 'wf_eta' ; str4 = 'wf_u'
      call add_gatti_netcdf(foutname,str1,ntspinup)
c     
      call add_gattr_netcdf(foutname,str2,wf_depth)
      call add_gattr_netcdf(foutname,str3,wf_eta)
      call add_gattr_netcdf(foutname,str4,wf_u)
      str1 = 'wf_v' ; str2 = 'wf_lapldepth'
      str3 = 'wf_graddepth' ; str4 = 'wf_zonal_transport'
      call add_gattr_netcdf(foutname,str1,wf_v)
      call add_gattr_netcdf(foutname,str2,wf_lapldepth)
      call add_gattr_netcdf(foutname,str3,wf_graddepth)
      call add_gattr_netcdf(foutname,str4,wf_zonal_transport)
c     
      do ix = 1, nx
         xout(ix) = x(ix)
      end do
      do iy = 1, ny
         yout(iy) = y(iy)
      end do
      if ( spherical ) then
         str1 = 'deg'
         call add_coordinates_netcdf(foutname,mynx,xout,myny,yout,str1)
      else if ( cartesian ) then
         str1 = 'meters'
         call add_coordinates_netcdf(foutname,mynx,xout,myny,yout,str1)
      end if
c     
      str1 = 'U' ; str2 = 'zonal velocity' ; str3 = 'meters/seconds'
      call add_recvar_netcdf(foutname, str1, str2, str3 )
      str1 = 'V' ; str2 = 'meridional velocity'
      call add_recvar_netcdf(foutname, str1, str2, str3 )
      str1 = 'ETA' ; str2 = 'sea-surface elevation' ; str3 = 'meters'
      call add_recvar_netcdf(foutname, str1, str2, str3 )

      return
      end ! ini_io

c$$$      subroutine adstate_io( time, nio )
c$$$
c$$$      implicit none
c$$$#include "size.inc"
c$$$#include "parms.inc"
c$$$#include "pfields.inc"
c$$$C==============================================
c$$$C define adjoint common blocks
c$$$C==============================================
c$$$      real adeta(0:nx+1,0:ny+1)
c$$$      real adu(0:nx+1,0:ny+1)
c$$$      real adv(0:nx+1,0:ny+1)
c$$$      common /advars/ adu, adv, adeta
c$$$
c$$$c     begin interface
c$$$      real time
c$$$      integer nio
c$$$c     end interface
c$$$      integer ix, iy
c$$$      real aduout(nx,ny), advout(nx,ny), adetaout(nx,ny)
c$$$
c$$$c
c$$$c     copy u, v, and eta to properly sized array, 
c$$$c     so that size(u) = [nx,ny]
c$$$c
c$$$      do ix = 1, nx
c$$$         do iy = 1, ny
c$$$            aduout(ix,iy) = adu(ix,iy)+real(int(umask(ix,iy))-1)*99.
c$$$            advout(ix,iy) = adv(ix,iy)+real(int(vmask(ix,iy))-1)*99.
c$$$            adetaout(ix,iy) =
c$$$     &           adeta(ix,iy)+real(int(etamask(ix,iy))-1)*99.
c$$$         end do
c$$$      end do
c$$$      call write_adstate_netcdf(foutname, nx, ny, nio, 'TIME', time)
c$$$      call write_adstate_netcdf(foutname, nx, ny, nio, 'ADU', aduout)
c$$$      call write_adstate_netcdf(foutname, nx, ny, nio, 'ADV', advout)
c$$$      call write_adstate_netcdf(foutname, nx, ny, nio, 'ADETA', adetaout)
c$$$c
c$$$c     stream function as an additional diagnostic, set the integration
c$$$c     to zero at the southern boundary, compute values at u-points
c$$$c
c$$$      return
c$$$      end                       ! adstate_io

c$openad XXX Template OADrts/ad_template.split.f
      subroutine state_io( time, nio )

      use size
      use parms
      use pfields
      use vars

      implicit none
!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"
!un #include "vars.inc"

c     begin interface
      real time
      integer nio
c     end interface
      integer ix, iy
      real uout(nx,ny), vout(nx,ny), etaout(nx,ny)
!nt
      character*(80) :: str1, str2
      integer mynx, myny
      mynx = nx ; myny = ny

c
c     copy u, v, and eta to properly sized array, 
c     so that size(u) = [nx,ny]
c
      do ix = 1, nx
         do iy = 1, ny
            uout(ix,iy) = u(ix,iy)+real(int(umask(ix,iy))-1)*99.
            vout(ix,iy) = v(ix,iy)+real(int(vmask(ix,iy))-1)*99.
            etaout(ix,iy) = eta(ix,iy)+real(int(etamask(ix,iy))-1)*99.
         end do
      end do
      str1 = 'TIME' ; str2 = 'U'
      call write_time_netcdf(foutname, nio, str1, time)
      call write_state_netcdf(foutname, mynx, myny, nio, str2, uout)
      str1 = 'V' ; str2 = 'ETA'
      call write_state_netcdf(foutname, mynx, myny, nio, str1, vout)
      call write_state_netcdf(foutname, mynx, myny, nio, str2, etaout)
c
c     stream function as an additional diagnostic, set the integration
c     to zero at the southern boundary, compute values at u-points
c
      return
      end                       ! state_io

c$openad XXX Template OADrts/ad_template.split.f
      subroutine pfields_io

      use size      
      use parms
      use pfields
      use force

      implicit none
!un #include "size.inc"      
!un #include "parms.inc"
!un #include "pfields.inc"
!un #include "force.inc"

      integer ix, iy
      real aux(nx,ny)
!nt
      character*(80) :: str1, str2, str3
      integer mynx, myny
      mynx = nx ; myny = ny

      do ix = 1, nx
         do iy = 1, ny
            aux(ix,iy) = depth(ix,iy)
         end do
      end do
      str1 = 'depth' ; str2 = 'water depth' ; str3 = 'meters'
      call add_pfield_netcdf(foutname, mynx, myny, aux,
     &     str1, str2, str3 )
      do ix = 1, nx
         do iy = 1, ny
            aux(ix,iy) = uforce(ix,iy)
         end do
      end do
      str1 = 'uforce' ; str2 = 'zonal forcing' ; str3 = 'forcing units'
      call add_pfield_netcdf(foutname, mynx, myny, aux, str1,
     &     str2, str3 )
      do ix = 1, nx
         do iy = 1, ny
            aux(ix,iy) = vforce(ix,iy)
         end do
      end do
      str1 = 'vforce' ; str2 = 'meridional forcing' 
      str3 = 'forcing units'
      call add_pfield_netcdf(foutname, mynx, myny, aux, str1,
     &     str2, str3 )
      do ix = 1, nx
         do iy = 1, ny
            aux(ix,iy) = frict(ix,iy)
         end do
      end do
      str1 = 'frict' ; str2 = 'linear bottom friction coefficient'
      str3 = '1/seconds'
      call add_pfield_netcdf(foutname, mynx, myny, aux, str1,
     &     str2, str3 )

      return
      end ! pfields_io

c$openad XXX Template OADrts/ad_template.split.f
      subroutine save_gradient_io( n, adxc, gname )
c
c     this routine maps the gradient vector to a 2D field, the map is
c     defined by map_from_control_vector and should always be compared
c     to that routine!
c

      use size
      use parms
      use pfields

      implicit none
!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"

c     interface
      integer n
      real adxc(n)
      character*(80) gname
c     end interface
c     local variables
      real grad(nx,ny)
!nt
      character*(80) :: str1, str2
      integer mynx, myny
      mynx = nx ; myny = ny

      call map_gradient( n, adxc, grad )
      str1 = 'gradient of cost function with respect to depth'
      str2 = 'cost function units/m'
      call add_pfield_netcdf(foutname, mynx, myny, grad,
     &     gname, str1, str2 )

c     
c     suppress any further io to netcdf file to avoid overwriting
c     
      suppressio = .true.

      return
      end ! subroutine save_gradient_io

c$openad XXX Template OADrts/ad_template.split.f
      subroutine save_depth_io( n, xc, dname )
      
      use size
      use parms
      use pfields

      implicit none
!un #include "size.inc"
!un #include "parms.inc"
!un #include "pfields.inc"

c     interface
      integer n
      real xc(n)
      character*(80) dname
c     end interface
c     local variables
      integer ix, iy
      real aux(nx,ny)
!nt
      character*(80) :: str1, str2
      integer mynx, myny
      mynx = nx ; myny = ny

      call map_from_control_vector( n, xc )
      do ix = 1, nx
         do iy = 1, ny
            aux(ix,iy) = depth(ix,iy)
         end do
      end do
      str1 = 'water depth after optimization' ; str2 = 'm'
      call add_pfield_netcdf(foutname, mynx, myny, aux,
     &     dname, str1, str2 )

c     
c     suppress any further io to netcdf file
c     
      suppressio = .true.

      return
      end ! subroutine save_depth_io

c$openad XXX Template OADrts/ad_template.split.f
      subroutine inimini_io

      implicit none

      end ! subroutine inimin_io

c$openad XXX Template OADrts/ad_template.split.f
      subroutine save_weights_io

      use size
      use parms
      use weights

      implicit none

!nt #include "size.inc"
!nt #include "parms.inc"
!nt #include "weights.inc"
!nt
      character*(80) :: str1

      str1 = 'wf_depth'
      call add_gattr_netcdf(foutname,str1,wf_depth)
      str1 = 'wf_eta'
      call add_gattr_netcdf(foutname,str1,wf_eta)
      str1 = 'wf_zonal_transport'
      call add_gattr_netcdf(foutname,str1, 
     &     wf_zonal_transport)

      end ! subroutine save_weight_io
# 1 "read_data.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "read_data.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_data( time )

      implicit none
c     begin interface
      real time
c     end interface

cph(
cph choose any of the following
c      call read_eta_data( time )
c      call read_uv_data( time )
c      call read_zonal_transport_data( time )
cph)

      end ! subroutine read_data

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_eta_data( time )

      use size
      use data
      use parms

      implicit none
c     begin interface
      real time
c     end interface

!un #include "size.inc"
!un #include "data.inc"
!un #include "parms.inc"

c     begin interface
c     real time
c     end interface

c     locals
      integer ix, iy
      real f_in(nx,ny)
!nt
      character*(80) :: myetadata
      character*(80) :: myeta
      integer mynx, myny
      mynx = nx ; myny = ny
      myetadata = 'etadata'
      myeta = 'eta'

      if ( time .eq. 0. ) then
c     assume that there is only one data slab that is to be use at all
c     times
         call read_field_netcdf(ncdatafile,myetadata,mynx,myny,f_in)
         do iy = 1, ny
            do ix = 1, nx
               eta_data(ix,iy) = f_in(ix,iy)
            end do
         end do
      else if ( time .gt. 0. ) then
         call read_snap_netcdf( ncdatafile, time, mynx, myny, 
     &        myeta, f_in )
         do iy = 1, ny
            do ix = 1, nx
               eta_data(ix,iy) = f_in(ix,iy)
            end do
         end do
      else
c     set data to zero
         do iy = 1, ny
            do ix = 1, nx
               eta_data(ix,iy) = 0.
            end do
         end do
      end if

      end ! subroutine read_eta_data

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_uv_data( time )

      use size
      use data
      use parms

      implicit none
c     begin interface
      real time
c     end interface

!un #include "size.inc"
!un #include "data.inc"
!un #include "parms.inc"

c     begin interface
c     real time
c     end interface

c     locals
      integer ix, iy
      real f_in(nx,ny)
!nt
      integer :: mynx = nx, myny = ny
      character*(80) :: strudata, strvdata
      character*(80) :: strU, strV
      strudata = 'udata' ; strvdata = 'vdata'
      strU = 'U' ; strV = 'V'

      if ( time .eq. 0. ) then
c     assume that there is only one data slab that is to be use at all
c     times
         call read_field_netcdf(ncdatafile,strudata,mynx,myny,f_in)
         do iy = 1, ny
            do ix = 1, nx
               u_data(ix,iy) = f_in(ix,iy)
            end do
         end do
         call read_field_netcdf(ncdatafile,strvdata,mynx,myny,f_in)
         do iy = 1, ny
            do ix = 1, nx
               v_data(ix,iy) = f_in(ix,iy)
            end do
         end do
      else if ( time .gt. 0. ) then
         call read_snap_netcdf(ncdatafile,time,mynx,myny,strU,f_in)
         do iy = 1, ny
            do ix = 1, nx
               u_data(ix,iy) = f_in(ix,iy)
            end do
         end do
         call read_snap_netcdf(ncdatafile,time,mynx,myny,strV,f_in)
         do iy = 1, ny
            do ix = 1, nx
               v_data(ix,iy) = f_in(ix,iy)
            end do
         end do
      else
c     set data to zero
         do iy = 1, ny
            do ix = 1, nx
               u_data(ix,iy) = 0.
               v_data(ix,iy) = 0.
            end do
         end do
      end if

      end ! subroutine read_uv_data

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_zonal_transport_data( time )

      use size
      use data
      use pfields
      use vars

      implicit none
c     begin interface
      real time
c     end interface

!un #include "size.inc"
!un #include "data.inc"
!un #include "pfields.inc"
!un #include "vars.inc"
c     locals
c     zonal volume transport
      real zonal_transport
cph new from inlining
      integer ix, iy, jx, jy
      parameter ( ix = 6 ) 
      if ( time .eq. 0. ) then
c     hack
         zonal_transport_data = 52.6265e+06
      else
cph(
cph         call calc_zonal_transport( zonal_transport )
cju      ix = 6
      zonal_transport = 0.
      do iy = 1, ny
         zonal_transport = zonal_transport 
     &        + u(ix,iy)*dy(iy)*hu(ix,iy)
c     &       + u(ix,iy)*dy(iy)*( eta(ix-1,iy)+eta(ix,iy) )
      end do
cph)
         zonal_transport_data = zonal_transport
      end if

      end ! subroutine read_zonal_transport_data

c$openad XXX Template OADrts/ad_template.split.f
      subroutine read_depth_data

      use size
      use pfields
      use data

      implicit none
c     interface
c     end interface

!un #include "size.inc"
!un #include "pfields.inc"
!un #include "data.inc"

c     local
      integer ix, iy

      do iy = 1, ny
         do ix = 1, nx
            depth_data(ix,iy) = inidepth(ix,iy)
         end do
      end do
c$$$c     this is another hack!
c$$$      print *, 'warning: depth_data has been set to zero'//
c$$$     &     ' in read_depth_data'
c$$$      do iy = 1, ny
c$$$         do ix = 1, nx
c$$$            depth_data(ix,iy) = 0.
c$$$         end do
c$$$      end do

      end ! subroutine read_depth_data

c$openad XXX Template OADrts/ad_template.split.f
      subroutine determine_data_time( ncdatafile )

      use size
      use data

      implicit none
c     interface
      character*(80) ncdatafile
c     integer nedt
c     end interface

!un #include "size.inc"
!un #include "data.inc"

      integer k
 !nt
      character*(80) :: strtime
      strtime = 'TIME'

      call get_length_netcdf( ncdatafile, strtime, nedt )
      if ( nedt .gt. nedtmax ) then
         print *, 'determine_data_time: too many data,;'
         print *, 'increase nedtmax to ', nedt
cju         stop 'abnormal in determine_data_time'
      else if ( nedt .le. 0 ) then
         print *, 'no time dependent data found in '//ncdatafile
      else
         call read_vector_netcdf( ncdatafile, strtime, 
     &        nedt, eta_data_time )
         print *, 'determine_data_time: # of data times = ', nedt
      end if
      print *, (eta_data_time(k), k=1,nedt)

      end ! subroutine determine_data_time

!nt converted to subroutine
c$openad XXX Template OADrts/ad_template.split.f
      subroutine is_eta_data_time( time, result )

      use size
      use data

      implicit none
c     interface 
      real time
      logical result
c     end interface

!un #include "size.inc"
!un #include "data.inc"
c     locals
      integer it
      logical allDone	

      allDone = .false.
      result = .false.
      it = 1
      do while (.NOT. allDone) 
       if ( abs(eta_data_time(it)-time) .lt. 1.e-8 ) then
         result = .true.
         allDone = .true.
        else
         if ( it .lt. nedt ) then
            it = it + 1
         else
            allDone = .true.
         end if
        end if
      end do
      return
      end subroutine
# 1 "make_weights.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "make_weights.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights

      implicit none

ctest      call make_weights_depth
      call make_weights_eta
      call make_weights_uv
      call make_weights_zonal_transport
      call make_weights_lapldepth
      call make_weights_graddepth

      end !subroutine make_weights

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights_depth

      use size
      use pfields
      use weights

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"
!un #include "weights.inc"

c     locals
      integer ix, iy, jx, jy, nflag
      real offdiag
      real error_depth
!nt
      integer :: mynx = nx, myny = ny
      character*(80) :: strwd 
      character*(80) :: strW
      strwd = 'weights_depth.nc'
      strW = 'W'

      nflag = 0
ctest      call read_weight_depth_netcdf( strwd, strW, 
ctest     &     weight_depth, mynx, myny, nflag )
c     this is the backup matrix
# 78 "make_weights.F"
c$$$c     print out the matrix
c$$$      open(10,file='weights_depth.asc');
c$$$      do iy = 1, ny
c$$$         do ix = 1, nx
c$$$            write(10,*) '(nx,ny) = (',ix,',',iy,')'
c$$$            write(10,'(20ES12.4)') 
c$$$     &           ((weight_depth(ix,jx,iy,jy),jx=1,nx),jy=1,ny)
c$$$         end do
c$$$      end do
c$$$      close(10)

      end !subroutine make_weights_depth

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights_eta

      use size
      use pfields
      use weights

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"
!un #include "weights.inc"

c     locals
      integer ix, iy
      real error_eta

      error_eta = 1.
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            weight_eta(ix,iy) = 
     &           wf_eta*(1./error_eta)**2*etamask(ix,iy)
         end do
      end do

      end !subroutine make_weights_eta

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights_uv

      use size
      use pfields
      use weights

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"
!un #include "weights.inc"

c     locals
      integer ix, iy
      real error_u, error_v

      error_u = 1.
      error_v = 1.
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            weight_u(ix,iy) = 
     &           wf_u*(1./error_u)**2*umask(ix,iy)
            weight_v(ix,iy) = 
     &           wf_v*(1./error_v)**2*vmask(ix,iy)
         end do
      end do

      end !subroutine make_weights_eta

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights_zonal_transport

      use size
      use weights

      implicit none
!un #include "size.inc"
!un #include "weights.inc"

c     locals
      real error_zonal_transport
      
      error_zonal_transport = 1.e+06
      weight_zonal_transport = 
     &     wf_zonal_transport*(1./error_zonal_transport)**2

      end !subroutine make_weights_zonal_transport

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_bounds_for_x( nc, lower_bound, upper_bound )

      use size
      use pfields

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"

c     interface
      integer nc
      real lower_bound(nc), upper_bound(nc)
c     end interface

      integer ix, iy, k
      real ub, lb

      ub =  50. ! meter
      lb = -50. ! meter

c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            if ( etamask(ix,iy) .ne. 0 ) then
               k = k+1
               upper_bound(k) = ub/inidepth(ix,iy)
               lower_bound(k) = lb/inidepth(ix,iy)
            end if
         end do
      end do

      end ! subroutine make_bounds_x

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights_lapldepth

      use size
      use pfields
      use weights

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"
!un #include "weights.inc"

c     locals
      integer ix, iy
      real error

      error = 1.
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            weight_lapldepth(ix,iy) = 
     &           wf_lapldepth*(1./error)**2*etamask(ix,iy)
         end do
      end do

      end ! subroutine make_weights_lapldepth

c$openad XXX Template OADrts/ad_template.split.f
      subroutine make_weights_graddepth

      use size
      use pfields
      use weights

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"
!un #include "weights.inc"

c     locals
      integer ix, iy
      real error

      error = 1.
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            weight_graddepth(ix,iy) = 
     &           wf_graddepth*(1./error)**2*etamask(ix,iy)
         end do
      end do

      end ! subroutine make_weights_graddepth
# 1 "map_control_vector.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "map_control_vector.F"
c$openad XXX Template OADrts/ad_template.joint.f
      subroutine map_from_control_vector( n, xc ) 

      use size
      use pfields

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"

c     interface
      integer n
      real xc(n)
c     end interface

c     locals
      integer k, ix, iy
c     makes sure, that we use the correct precision when adding one
      real one
      parameter ( one = 1. )

      k = 0
      do iy = 1, ny
         do ix = 1, nx
            if ( etamask(ix,iy) .ne. 0 ) then
               k = k+1
               depth(ix,iy) = scaledepth(ix,iy)*( one + xc(k) )
c               print *, ix, iy, xc(k), depth(ix,iy)
c               depth(ix,iy) = scaledepth(ix,iy) + xc(k)
c               depth(ix,iy) = xc(k)
            end if
         end do
      end do
      if ( k .ne. n ) then
         print *, 'map_from_control_vector: ',
     &        'dimensions of control vector are wrong'
         print *, k, ' should be ', n
cju         stop
      end if
      
      end ! subroutine map_from_control_vector

c$openad XXX Template OADrts/ad_template.joint.f
      subroutine map_to_control_vector( n, xc ) 

      use size
      use pfields

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"

c     interface
      integer n
      real xc(n)
c     end interface

c     locals
      integer k, ix, iy
c     makes sure, that we use the correct precision when substracting one
      real one
      parameter ( one = 1. )

      k = 0
      do iy = 1, ny
         do ix = 1, nx
            if ( etamask(ix,iy) .ne. 0 ) then
               k = k+1
               xc(k) = depth(ix,iy)/scaledepth(ix,iy) - one
c               xc(k) = depth(ix,iy) - scaledepth(ix,iy)
c               xc(k) = depth(ix,iy) 
            end if
         end do
      end do
      if ( k .ne. n ) then
         print *, 'map_to_control_vector: ',
     &        'dimensions of control vector are wrong'
         print *, k, ' should be ', n
cju         stop
      end if
      
      end ! subroutine map_to_control_vector

c$openad XXX Template OADrts/ad_template.joint.f
      subroutine map_gradient( n, adxc, grad ) 

      use size
      use pfields

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"

c     interface
      integer n
      real adxc(n)
      real grad(nx,ny)
c     end interface

c     locals
      integer k, ix, iy

      k = 0
      do iy = 1, ny
         do ix = 1, nx
            if ( etamask(ix,iy) .ne. 0 ) then
               k = k+1
               grad(ix,iy) = adxc(k)/scaledepth(ix,iy)
            end if
         end do
      end do
      if ( k .ne. n ) then
         print *, 'map_from_control_vector: ',
     &        'dimensions of control vector are wrong'
         print *, k, ' should be ', n
cju         stop
      end if
      
      end ! subroutine map_gradient

c$openad XXX Template OADrts/ad_template.joint.f
      subroutine length_of_control_vector( n )
      
      use size
      use pfields

      implicit none

!un #include "size.inc"
!un #include "pfields.inc"

c     interface
      integer n
c     end interface

      integer ix, iy, k

      k = 0
      do iy = 1, ny
         do ix = 1, nx
            if ( etamask(ix,iy) .ne. 0 ) then
               k = k+1
            end if
         end do
      end do
      n = k
      print *, 'dimensions of control vector = ', n

      return
      end ! subroutine length_of_control_vector
# 1 "small_routines_split.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "small_routines_split.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine time_step( it, local_u, local_v, local_eta )

      use size
      use vars

      implicit none

c     begin interface
      integer it
c     end interface

      real local_u(0:nx+1,0:ny+1)
      real local_v(0:nx+1,0:ny+1)
      real local_eta(0:nx+1,0:ny+1)

cju #include "size.inc"
cju #include "vars.inc"

!cadj init tape_level_aux = common, 1
c     alternate evaluation of momentum equations, because coriolis is
c     treated explicitly
      if ( mod(it,2) .ne. 0 ) then
!cadj store u = tape_level_aux
         call umomentum(local_u, local_v, local_eta)
         call vmomentum(local_u, local_v, local_eta)
      else 
!cadj store v = tape_level_aux
         call vmomentum(local_u, local_v, local_eta)
         call umomentum(local_u, local_v, local_eta)
      end if
c
c     continuity equation calculates eta
c
      call continuity(local_u, local_v, local_eta)

      return
      end ! subroutine time_step

c$openad XXX Template OADrts/ad_template.split.f
      subroutine umomentum(local_u,local_v, local_eta)

      use size
      use parms
      use vars
      use pfields
      use force

      implicit none

      real, intent(inout) :: local_u(0:nx+1,0:ny+1)
      real, intent(in) :: local_v(0:nx+1,0:ny+1)
      real, intent(in) :: local_eta(0:nx+1,0:ny+1)


cju #include "size.inc"
cju #include "parms.inc"
cju #include "vars.inc"
cju #include "pfields.inc"
cju #include "force.inc"

c     local variables
      integer ix, iy
      real frictu
      real gradetau
      real fv
c
c     evaluate u momentum equation
c
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            frictu = .5*(frict(ix,iy)+frict(ix-1,iy))*invhu(ix,iy)
            gradetau = (local_eta(ix,iy)-local_eta(ix-1,iy))
     &           /(rx(iy)*.5*(dx(ix)+dx(ix-1)))
c     factor .25 is contained in fcoriu
            fv = fcoriu(ix,iy)*( local_v(ix,iy)  + local_v(ix-1,iy)
     &                         + local_v(ix,iy+1)+ local_v(ix-1,iy+1))
            local_u(ix,iy) = umask(ix,iy)/(1+0.5*frictu*dt)*(
     &           (1-.5*frictu*dt)*local_u(ix,iy) - g*dt*gradetau + dt*fv 
     &           + dt*uforce(ix,iy)*invhu(ix,iy)
     &           )
         end do
      end do

c     handle domain boundaries
      if ( xperiodic ) then
c$openad xxx simple loop
         do iy = 0, ny+1
            local_u(nx+1,iy) = local_u(1,iy)
         end do
      end if
      if ( yperiodic ) then
c     for coriolis term in v equation
c$openad xxx simple loop
         do ix = 0, nx+1
            local_u(ix,0) = local_u(ix,ny)
         end do
      end if
c
      end ! subroutine umomentum

c$openad XXX Template OADrts/ad_template.split.f
      subroutine vmomentum(local_u, local_v, local_eta)

      use size
      use parms
      use vars
      use pfields
      use force

      implicit none

      real, intent(in) :: local_u(0:nx+1,0:ny+1)
      real, intent(inout) :: local_v(0:nx+1,0:ny+1)
      real, intent(in) :: local_eta(0:nx+1,0:ny+1)

cju #include "size.inc"
cju #include "parms.inc"
cju #include "vars.inc"
cju #include "pfields.inc"
cju #include "force.inc"

      integer ix, iy
      real frictv
      real gradetav
      real fu
c
c     evaluate v momentum equation
c
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            frictv = .5*(frict(ix,iy)+frict(ix,iy-1))*invhv(ix,iy)
            gradetav = (local_eta(ix,iy)-local_eta(ix,iy-1))
     &           /(ry*.5*(dy(iy)+dy(iy-1)))
c     factor .25 is contained in fcoriv
            fu = fcoriv(ix,iy)*( local_u(ix,iy)  + local_u(ix+1,iy)
     &                         + local_u(ix,iy-1)+ local_u(ix+1,iy-1))
            local_v(ix,iy) = vmask(ix,iy)/(1+0.5*frictv*dt)*(
     &           (1-.5*frictv*dt)*local_v(ix,iy) - g*dt*gradetav - dt*fu
     &           + dt*vforce(ix,iy)*invhv(ix,iy)
     &           )
         end do
      end do
c     handle domain boundaries
      if ( xperiodic ) then
c$openad xxx simple loop
         do iy = 0, ny+1
c     for coriolis term in u equation
            local_v(0,iy) = local_v(nx,iy)
         end do
      end if
      if ( yperiodic ) then
c$openad xxx simple loop
         do ix = 0, nx+1
            local_v(ix,ny+1) = local_v(ix,1)
         end do
      end if
c
      end ! subroutine vmomentum

c$openad XXX Template OADrts/ad_template.split.f
      subroutine continuity(local_u, local_v, local_eta)
      
      use size
      use parms
      use vars
      use pfields

      implicit none

      real, intent(in) :: local_u(0:nx+1,0:ny+1)
      real, intent(in) :: local_v(0:nx+1,0:ny+1)
      real, intent(inout) :: local_eta(0:nx+1,0:ny+1)

cju #include "size.inc"
cju #include "parms.inc"
cju #include "pfields.inc"
cju #include "vars.inc"
      
      integer ix, iy

c     eta equation
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            local_eta(ix,iy) = local_eta(ix,iy) - etamask(ix,iy)*dt*(
     &        (hu(ix+1,iy)*local_u(ix+1,iy) - hu(ix,iy)*local_u(ix,iy))
     &           /(rx(iy)*dx(ix))
     &           + ( hy(iy+1)* hv(ix,iy+1)*local_v(ix,iy+1) 
     &             - hy(iy)  * hv(ix,iy)  *local_v(ix,iy)  )
     &           /(ry*dy(iy))
     &           )
         end do
      end do
c     handle domain boundaries
      if ( xperiodic ) then
c$openad xxx simple loop
         do iy = 0, ny+1
            local_eta(0,iy) = local_eta(nx,iy)
         end do
      end if
      if ( yperiodic ) then
c$openad xxx simple loop
         do ix = 0, nx+1
            local_eta(ix,0) = local_eta(ix,ny)
         end do
      end if

      return
      end ! subroutine continuity

c$openad XXX Template OADrts/ad_template.split.f
      subroutine calc_overturning( overturning, local_u)
c
c     returns the (linearized) overturning tranport 
c
      use size
      use vars
      use pfields

      implicit none
c     interface
      real overturning
c     end interface

      real, intent(in) :: local_u(0:nx+1,0:ny+1)

cju #include "size.inc"
cju #include "pfields.inc"
cju #include "vars.inc"
c     locals
      integer ix
      integer iy
      parameter ( ix = 7 ) 
cju      ix = 7
      overturning = 0.
c$openad xxx simple loop
      do iy = 1, ny/2
         overturning = overturning
     &        + local_u(ix,iy)*dy(iy)*hu(ix,iy)
      end do

      return
      end !subroutine calc_overturning

c$openad XXX Template OADrts/ad_template.split.f
      subroutine calc_zonal_transport_split( zonal_transport,local_u )
c
c     returns the (linearized) zonal volume tranport through the 
c     western face of the ix'th grid box column
c
      use size
      use vars
      use pfields

      implicit none
c     interface
      real zonal_transport
c     end interface

      real, intent(in) :: local_u(0:nx+1,0:ny+1)

cju #include "size.inc"
cju #include "pfields.inc"
cju #include "vars.inc"
c     locals
      integer ix
      integer iy
      parameter ( ix = 6 ) 
cju      ix = 6
      zonal_transport = 0.
c$openad xxx simple loop
      do iy = 1, ny
         zonal_transport = zonal_transport 
     &        + local_u(ix,iy)*dy(iy)*hu(ix,iy)
c     &       + local_u(ix,iy)*dy(iy)*( eta(ix-1,iy)+eta(ix,iy) )
      end do

      return
      end !subroutine zonal_transport
# 1 "small_routines_joint.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "small_routines_joint.F"
c$openad XXX Template OADrts/ad_template.joint.f
      subroutine initial_values

      use size
      use parms
      use vars
      use pfields

      implicit none

cju #include "size.inc"
cju #include "parms.inc"
cju #include "vars.inc"
cju #include "pfields.inc"

      integer ix, iy

c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            u(ix,iy)   = uini(ix,iy)*umask(ix,iy)
            v(ix,iy)   = vini(ix,iy)*vmask(ix,iy)
            eta(ix,iy) = etaini(ix,iy)*etamask(ix,iy)
         end do
      end do
c     handle domain boundaries
      if ( xperiodic ) then
c$openad xxx simple loop
         do iy = 0, ny+1
            u(nx+1,iy) = u(1,iy)
            v(0,iy)    = v(nx,iy)
            eta(0,iy)  = eta(nx,iy)
         end do
      end if
      if ( yperiodic ) then
c$openad xxx simple loop
         do ix = 0, nx+1
            u(ix,0)    = u(ix,ny)
            v(ix,ny+1) = v(ix,1)
            eta(ix,0)  = eta(ix,0)
         end do
      end if

      return
      end !subroutine initial_values

c$openad XXX Template OADrts/ad_template.joint.f
      subroutine calc_depth_uv

      use size
      use pfields

      implicit none

cju #include "size.inc"
cju #include "pfields.inc"

      integer ix, iy

c     create depth at u and v points
c$openad xxx simple loop
      do iy = 1, ny+1
         do ix = 1, nx+1

            hu(ix,iy) = depth(ix,iy)*umask(ix,iy)
            hv(ix,iy) = depth(ix,iy)*vmask(ix,iy)

            if ( hu(ix,iy) .ne. 0. ) then
               invhu(ix,iy) = 1./hu(ix,iy)
            end if
            if ( hv(ix,iy) .ne. 0. ) then
               invhv(ix,iy) = 1./hv(ix,iy)
            end if
         end do
      end do

c$$$      open(20,file='huhv.dat',form='formatted',recl=102*20)      
c$$$      write(20,'(102E20.12)') ((hu(ix,iy),ix=0,nx+1),iy=ny+1,0,-1)
c$$$      write(20,'(102E20.12)') ((hu(ix,iy),ix=0,nx+1),iy=ny+1,0,-1)
c$$$      close(20)

      return
      end

c$openad XXX Template OADrts/ad_template.joint.f
      subroutine calc_zonal_transport_joint( zonal_transport,local_u )
c
c     returns the (linearized) zonal volume tranport through the 
c     western face of the ix'th grid box column
c
      use size
      use vars
      use pfields

      implicit none
c     interface
      real zonal_transport
c     end interface

      real, intent(in) :: local_u(0:nx+1,0:ny+1)

cju #include "size.inc"
cju #include "pfields.inc"
cju #include "vars.inc"
c     locals
      integer ix
      integer iy

      parameter ( ix = 6 )
cju      ix = 6
      zonal_transport = 0.
c$openad xxx simple loop
      do iy = 1, ny
         zonal_transport = zonal_transport 
     &        + local_u(ix,iy)*dy(iy)*hu(ix,iy)
c     &       + local_u(ix,iy)*dy(iy)*( eta(ix-1,iy)+eta(ix,iy) )
      end do

      return
      end !subroutine zonal_transport

c$openad XXX Template OADrts/ad_template.joint.f
      subroutine smoothness_lapldepth( cf )

      use size
      use parms
      use pfields
      use weights

      implicit none
c     interface
      real cf
c     end interface
cju #include "size.inc"
cju #include "parms.inc"
cju #include "pfields.inc"
cju #include "weights.inc"

c     locals
      integer ix, iy
      real dfdx(nx,ny), ddfddx(nx,ny)
      real dfdy(nx,ny), ddfddy(nx,ny)
      real lapldepth

c     first derivative: forward differences
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx-1
            dfdx(ix,iy) = (depth(ix+1,iy)-depth(ix,iy))/dx(ix+1)
         end do
         if ( xperiodic ) then
            dfdx(nx,iy) = (depth(1,iy)-depth(nx,iy))/dx(1)
         else
            dfdx(nx,iy) = 0.
         end if
      end do
c
c$openad xxx simple loop
      do ix = 1, nx
         do iy = 1, ny-1
            dfdy(ix,iy) = (depth(ix,iy+1)-depth(ix,iy))/dy(iy+1)
         end do
         if ( yperiodic ) then
            dfdy(ix,ny) = (depth(ix,1)-depth(ix,ny))/dy(1)
         else
            dfdy(ix,ny) = 0.
         end if
      end do
c     second derivatives: backward differences
c$openad xxx simple loop
      do iy = 1, ny
         if ( xperiodic ) then
            ddfddx(1,iy) = (dfdx(1,iy)-dfdx(nx,iy))/dx(1)
         else
            ddfddx(1,iy) = 0.
         end if
         do ix = 2, nx
            ddfddx(ix,iy) = (dfdx(ix,iy)-dfdx(ix-1,iy))/dx(ix)
         end do
      end do
c
c$openad xxx simple loop
      do ix = 1, nx
         if ( yperiodic ) then
            ddfddy(ix,1) = (dfdy(ix,1)-dfdy(ix,ny))/dy(1)
         else
            ddfddy(ix,1) = 0.
         end if
         do iy = 2, ny
            ddfddy(ix,iy) = (dfdy(ix,iy)-dfdy(ix,iy-1))/dy(iy)
         end do
      end do
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            lapldepth = ddfddx(ix,iy) + ddfddy(ix,iy)
            cf = cf + .5*lapldepth*lapldepth*weight_lapldepth(ix,iy)
         end do
      end do

      return
      end ! subroutine smoothness_lapldepth
c
c$openad XXX Template OADrts/ad_template.joint.f
      subroutine smoothness_graddepth( cf )

      use size
      use parms
      use pfields
      use weights

      implicit none
c     interface
      real cf
c     end interface
cju #include "size.inc"
cju #include "parms.inc"
cju #include "pfields.inc"
cju #include "weights.inc"

c     locals
      integer ix, iy
      real dfdx(nx,ny)
      real dfdy(nx,ny)
      real graddepth

c     first derivative: backward differences
c$openad xxx simple loop
      do iy = 1, ny
         if ( xperiodic ) then
            dfdx(1,iy) = (depth(1,iy)-depth(nx,iy))/dx(1)
         else
            dfdx(1,iy) = 0.
         end if
         do ix = 2, nx
            dfdx(ix,iy) = (depth(ix,iy)-depth(ix-1,iy))/dx(ix)
         end do
      end do
c
c$openad xxx simple loop
      do ix = 1, nx
         if ( yperiodic ) then
            dfdy(ix,1) = (depth(ix,1)-depth(ix,ny))/dy(1)
         else
            dfdy(ix,1) = 0.
         end if
         do iy = 2, ny
            dfdy(ix,iy) = (depth(ix,iy)-depth(ix,iy-1))/dy(iy)
         end do
      end do
c
c$openad xxx simple loop
      do iy = 1, ny
         do ix = 1, nx
            cf = cf + .5*weight_graddepth(ix,iy)*
     &           ( .5*dfdx(ix,iy)*dfdx(ix,iy)*umask(ix,iy)
     &           + .5*dfdy(ix,iy)*dfdy(ix,iy)*vmask(ix,iy) )
         end do
      end do

      return
      end ! subroutine smoothness_graddepth
# 1 "cost_function.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "cost_function.F"
c$openad XXX Template OADrts/ad_template.split.f
      subroutine cost_function( time, cf, local_u, local_v, local_eta )
c
c     specify your cost function here
c     
      use size
      use vars
      use data
      use weights

      implicit none
cju #include "size.inc"
cju #include "vars.inc"
c#include "pfields.inc"
cju #include "weights.inc"
cju #include "data.inc"
      
c     interface
      real time, cf
c     end interface

      real, intent(in) :: local_u(0:nx+1,0:ny+1)
      real, intent(in) :: local_v(0:nx+1,0:ny+1)
      real, intent(in) :: local_eta(0:nx+1,0:ny+1)

c     local variables
      integer ix, iy
c     zonal volume transport
      real zonal_transport
c
cju      logical is_eta_data_time
cju      external is_eta_data_time

      logical result
c
c     load the data
c
      call is_eta_data_time( time ,result)
      if ( result ) then
         call read_data( time )
      end if
c     
c     calculate cost function, if there is only one data slab, or if
c     the time step matches a data time
c     
      call is_eta_data_time( time ,result)
      if ( nedt .eq. -1 .or. result ) then
c
c     calculate cost function terms
c

c     zonal transport 
cju in the code for calc_zonal_transport the v and eta 
cju references had always been commented out
cju         call calc_zonal_transport_split( zonal_transport, local_u, local_v,
cju     &local_eta )
         call calc_zonal_transport_split( zonal_transport, local_u )
c$$$         cf = cf + zonal_transport*wf_zonal_transport
         cf = cf + .5*(zonal_transport-zonal_transport_data)**2
     &        *weight_zonal_transport
c
         do iy = 1, ny
            do ix = 1, nx
c     sea surface height
               cf = cf + .5*(local_eta(ix,iy)-eta_data(ix,iy))**2
     &              *weight_eta(ix,iy)
c     velocity
               cf = cf + .5*(local_u(ix,iy)-u_data(ix,iy))**2
     &              *weight_u(ix,iy)
               cf = cf + .5*(local_v(ix,iy)-v_data(ix,iy))**2
     &              *weight_v(ix,iy)
            end do
         end do

      end if

      return
      end ! subroutine cost_function
# 1 "cost_depth.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "cost_depth.F"
c$openad XXX Template OADrts/ad_template.joint.f
      subroutine cost_depth( cf )

      use size
      use pfields
      use data
      use weights

      implicit none

c     interface
      real cf
c     end interface
cju #include "size.inc"
cju #include "pfields.inc"
cju #include "data.inc"
cju #include "weights.inc"

c     locals
      integer ix, iy, jx, jy
c$openad xxx simple loop
      do iy = 1, ny
         do jy = 1, ny
            do ix = 1, nx
               do jx = 1, nx
                  cf = cf 
     &                 + .5*(depth(ix,iy)-depth_data(ix,iy))
     &                 *(depth(jx,jy)-depth_data(jx,jy))
ctest     &                 *weight_depth(ix,jx,iy,jy)
               end do
            end do
         end do
      end do

      end ! subroutine cost_depth

# 1 "loop_body_wrapper_inner.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "loop_body_wrapper_inner.F"
c$openad XXX Template OADrts/ad_template.joint_split_iif.f
      subroutine loop_body_wrapper_inner(local_u,local_v,local_eta,cost,
     & calc_cost, time, time_index, nio) 

      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights
      
      implicit none

      real local_u(0:nx+1,0:ny+1)
      real local_v(0:nx+1,0:ny+1)
      real local_eta(0:nx+1,0:ny+1)
      real cost
      logical calc_cost
      real time
      integer time_index, nio 
c
cadj store u,v,eta = tape_level_2

cph(
               call time_step( time_index, local_u, local_v, local_eta )
cph)

               if ( ( time_index .gt. ntspinup ) .and. calc_cost ) then
cph(
                  call cost_function( time , cost , local_u, local_v, 
     &local_eta)
cph)
               end if
c
c     after stepping the model forward, store the dynamic variables
c
               if ( fullio .and. .not. suppressio .and.
     &              ( mod(real(time_index)*dt,dt_dump) .eq. 0. ) ) then
                  nio = nio + 1
                  print *, 'Writing Time Step ', time_index
cph(
cph this contains netcdf stuff
cph                  call state_io( time, nio )
cph)
               end if
      end subroutine
# 1 "loop_body_wrapper_outer.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "loop_body_wrapper_outer.F"
c$openad XXX Template OADrts/ad_template.joint_split_oif.f
      subroutine loop_body_wrapper_outer(local_u,local_v,local_eta,cost,
     & calc_cost, time, time_index, nio, it, ntotal, ninner) 

      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights

      implicit none
      
      real local_u(0:nx+1,0:ny+1)
      real local_v(0:nx+1,0:ny+1)
      real local_eta(0:nx+1,0:ny+1)
      real cost
      logical calc_cost
      real time
      integer time_index, nio, it, ntotal, ninner 

      integer jt

      do jt = 1, ninner
         time_index = (it-1)*ninner+jt
         time = start_time+time_index*dt 
         if ( time_index .le. ntotal ) then
c            print *, 'inner index', jt
            call loop_body_wrapper_inner(local_u, local_v, local_eta,  
     & cost, calc_cost, time, time_index, nio) 
         end if
      end do
      end subroutine
# 1 "forward_model.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "forward_model.F"
cadj subroutine ini_io
cadj subroutine ini_io input = 
cadj subroutine ini_io output = 
cadj subroutine pfields_io
cadj subroutine pfields_io input = 
cadj subroutine pfields_io output = 
cadj subroutine state_io
cadj subroutine state_io input = 1, 2
cadj subroutine state_io output =
cadj subroutine determine_data_time
cadj subroutine determine_data_time input = 1
cadj subroutine determine_data_time output =
cadj function is_eta_data_time
cadj function is_eta_data_time input = 1
cadj subroutine read_eta_data
cadj subroutine read_eta_data input = 1
cadj subroutine read_eta_data output = 1
!cadj subroutine read_eta_data adname = read_eta_data
!cadj subroutine read_eta_data ftlname = read_eta_data



c$openad XXX Template OADrts/ad_template_timing.joint.f
       subroutine forward_model( nctrl, xc, cost_final )

      use size
      use parms
      use vars
      use pfields
      use force
      use data
      use weights

      implicit none

cju #include "size.inc"
cju #include "parms.inc"
cju #include "vars.inc"
cph(
cju #include "pfields.inc"
cju #include "weights.inc"
cju #include "data.inc"
cju #include "force.inc"
cph)

c     interface
      integer nctrl
      real xc(nctrl)
      real cost_final
c     end interface

c     locals
      integer time_index, ntotal
      integer it, jt, nio 
      logical calc_cost
      real cost_d, cost_sd, cost_gd, cost, time
c     zonal volume transport
      real zonal_transport
c     checkpoint frequency for tamc code
      integer nouter, ninner
      real routin
cph new from inlining
      integer ix, iy, jx, jy
      integer k
      real frictu, frictv
      real gradetau, gradetav
      real fv, fu
cju      logical is_eta_data_time
cju      external is_eta_data_time
      real one
      parameter ( one = 1. )
c 
c     we will need to store the dynamic variables u, v, and eta, so they
c     do not have to be recomputed during the adjoint calculations, so far
c     use two level checkpointing, intialize the outer tape (first tape 
c     level) here to avoid unecessary recomputation of the forward model.
c 
cadj init tape_level_1 = 'state'
c
c     determine whether to calculate cost function ( can save some time )
c      

c$openad INDEPENDENT(xc)

      if ( initial_grad  .or. grad_check .or. optimize
     &     .or. calc_hess ) then
         calc_cost = .true.
      else
         calc_cost = .false.
      end if
c
c     initial local variables
c
      ninner = 500 
      cost_d = 0.
      cost_sd = 0.
      cost_gd = 0.
      cost = 0.
c
cph(
      call map_from_control_vector( nctrl, xc )
cph)
c     
c     calculate depth at the faces of the grid points ( u and v points )
c
cph(
      call calc_depth_uv
cph)
c
c     initialize dynamic variables
c
cph(
      call initial_values
cph)
c     
c     initialize I/O
      if ( .not. suppressio )  then
cph(
cph these contains netcdf stuff for diagnostics. Can be avoided for OpenAD.
cph         call ini_io
cph         call pfields_io
cph)
      end if
c     first cost function contribution is the time independend variable
c     depth
      if ( calc_cost ) then
cph(
         call cost_depth( cost_d )
cph)
c         call smoothness_lapldepth( cost_sd )
c         call smoothness_graddepth( cost_gd )
      end if
c
c     start time stepping
c
      nio = 0
      time_index = 0
      time = 0.
c
c     total number of time steps include spin up
c
      ntotal = nt+ntspinup
c$$$c
c$$$c     store the dynamic variables u, v, and eta, so they do not have 
c$$$c     to be recomputed during the adjoint calculations, so far use
c$$$c     two level checkpointing
c$$$c
c$$$cadj init tape_level_1 = 'state'
c
c     start integration over assimilation interval, determine the number
c     of cycles for the outer loop of check pointing, round to the nearest 
c     integer larger than routin (ratio of total to inner loop cycles)
c
      routin = real(ntotal)/real(ninner)
      if ( routin .ne. ntotal/ninner ) then
         nouter = int( routin ) + 1
      else
         nouter = int( routin ) 
      end if
      if ( fullio .and. .not. suppressio ) then
         print *, 'number of outer loops',
     &        ' = number of tape records = ', nouter
      end if
      time_index = 0
      time = start_time+time_index*dt
      if ( .not. suppressio ) then
         nio = nio + 1
         print *, 'Writing Time Step ', time_index
cph(
cph this contains netcdf stuff
cph         call state_io( time, nio )
cph)
      end if
      
c      ninner=10
c      nouter=6

      do it = 1, nouter
c
cadj store u,v,eta = tape_level_1, key=it
cadj init tape_level_2 = common, ninner
c
         print *, 'outer loop ', it
         call loop_body_wrapper_outer(u, v, eta,  
     & cost, calc_cost, time, time_index, nio, it, ntotal, ninner) 
c
      end do
c$$$c     overturning
c$$$      zonal_transport = 0.
c$$$      call calc_overturning( zonal_transport, u, v )
c$$$      print *, 'overturning transport = ', zonal_transport*1e-6, ' Sv'
c$$$      cost = zonal_transport
c     zonal transport
      zonal_transport = 0.
cph(
cju in the code for calc_zonal_transport the v and eta 
cju references had always been commented out
cju      call calc_zonal_transport_joint( zonal_transport, u, v, eta)
      call calc_zonal_transport_joint( zonal_transport, u)
cph)
      print *, 'zonal volume transport = ', zonal_transport*1e-6, ' Sv'
c
c     save cost function value (to trick TAMC)
c
cju changed according to Patrick's instructions:
cju      cost_final = cost + cost_d + cost_sd + cost_gd
      cost_final = zonal_transport

      if ( iteration .ge. 0 ) then
         open(10,file='cost.txt',form='formatted',position='append')
         write(10,'(I5,5E15.8)') iteration, cost_d, cost_sd, cost_gd,
     &        cost, cost_final
         close(10)
         iteration = iteration + 1
      end if

c$openad DEPENDENT(cost_final)

      return
      end !subroutine forward_model
