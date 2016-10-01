module line_searchs


  use norma0
private compute1,compute2
interface compute
module procedure compute1,compute2
end interface
contains


subroutine compute1(x,f,grad)
use OAD_active
use OAD_tape
use OAD_rev
use w2f__types
use log_eq
doubleprecision ::dt,f,x(3),grad(3)
  type(active) :: X0
  type(active) :: K
  type(active) :: L
  type(active) ::ad_cost0
integer N
N=100
dt=0.1
x0%v=x(1)
x0%d=0
l%v=x(2)
l%d=0
k%v=x(3)
k%d=0
AD_COST0%d=1.0
call tape_init()
our_rev_mode%tape=.TRUE.
call AD_COST(X0, K, L, DT, N, AD_COST0)
f=ad_cost0%v
grad(1)=x0%d
grad(2)=l%d
grad(3)=k%d
end subroutine

  subroutine compute2(nn,x0,f,grad,ctrlname,costname,direction)
  !USE GRID
    implicit none
    integer,intent(in)::nn
    double precision::x0(nn),direction(nn)
#define DEBUG
    !double precision,intent(in)::direction(size(x0))
    double precision,intent(inout)::f,grad(size(x0))
    integer i,j,k
    character(len=*):: ctrlname,costname
    do k=1,nn
       if (my_is_nan(x0(k))) then
          f=1e+38
          grad(1:nn)=1e+10/nn
          return
       endif
    enddo

!#ifdef DEBUG
    !call system("del d:/intel/inp.txt  ")
	!call system("del d:/intel/out.txt  ")
if (size(x0).eq.3) then
    call compute1(x0,f,grad)
else
stop
endif


    if (my_is_nan(f)) then
       f=1e+38
       grad=1e+38*direction
       return
    endif
    do k=1,nn

       if (my_is_nan(grad(k))) then

          grad(1:nn)=1e+38*direction
          f=1e+38
          return
       endif
    enddo



  end subroutine compute2


#undef VALUE_BASED

  subroutine LINE_SEARCH(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)
    integer ,intent(in)::nn
    INTEGER,INTENT(IN) ::NITER
    doubleprecision,INTENT(IN)::DIRECTION(nn)
    doubleprecision::X0(nn),GRAD0(nn),EPS0,F0
    integer,intent(inout)::ncomp
    character(len=9):: ctrlname,costname
    logical if_restart,if_exceed
#undef VALUE_BASED
#ifdef VALUE_BASED
    call LINE_SEARCH2(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)
#else
    call LINE_SEARCH1(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)
#endif    


  end subroutine LINE_SEARCH

  subroutine LINE_SEARCH2(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)

    use norma0
    implicit none
    integer ,intent(in)::nn
    INTEGER,INTENT(IN) ::NITER
    doubleprecision,INTENT(IN)::DIRECTION(nn)
    doubleprecision::X0(nn),GRAD0(nn),EPS0,F0
    DOUBLEPRECISION X1,X2,A,B,C,C0,D,F1,F2,fn,fnew,fa,fb,fc,g0,fc0
    DOUBLEPRECISION XC1,XC2,ffa,ffb,ffc,ffc0
    doubleprecision,dimension(nn)::grada,gradb,gradc,gradc0
    integer,intent(inout)::ncomp
    integer i,counter
    logical if_suff,if_restart,if_exceed
    character(len=9):: ctrlname,costname
111 FORMAT (A,E19.11E2,E19.11E2,E19.11E2)
114 FORMAT (A,E19.11E2,E19.11E2,E19.11E2,E19.11E2)
112 FORMAT (i3,A,E19.11E2,E19.11E2,E19.11E2)
113 FORMAT (i3,A,E19.11E2,E19.11E2,E19.11E2,E19.11E2)
    a=0;b=eps0;grada=grad0;fa=f0;if_suff=.false.;counter=1
    do i=1,nn
       if (my_is_nan(x0(i)).or.my_is_nan(grad0(i))) then
          print *,i,x0(i),grad0(i)
       endif
    enddo
    ffa=grada.x.direction;g0=ffa
    write(0,114) "initial a " ,a,fa,ffa,norma(direction)

777    call compute(nn,x0+eps0*direction,fb,gradb,ctrlname,costname,direction)
    ffb=gradb.x.direction;
    write(0,114) "initial b " ,b,fb,ffb,norma(gradb)
    c=-(ffa*b-ffb*a)/(ffb-ffa)
    call compute(nn,x0+c*direction,fc,gradc,ctrlname,costname,direction)
    ffc=gradc.x.direction;
    write(0,111) "val c " ,c,fc,ffc
!    if ((fc<fb).and.(ffc>0).and.(ffa<0)) then
!		c0=davidon(a,c,fa,fc,ffa,ffc)     else
		c0=davidon(a,b,fa,fb,ffa,ffb)
!    endif
    call compute(nn,x0+c0*direction,fc0,gradc0,ctrlname,costname,direction)
    ffc0=gradc0.x.direction;
    write(0,111) "val c0 " ,c,fc,ffc
    if ((fc.eq.min(fa,fb,fc,fc0,f0)).and.wolfe_condition(f0,g0,fc,ffc,c)) then
			grad0=gradc;
			x0=x0+c*direction
			;f0=fc;
			if_exceed=.false.
    elseif ((fc0.eq.min(fa,fb,fc,fc0,f0)).and.wolfe_condition(f0,g0,fc0,ffc0,c0)) then
			grad0=gradc0;x0=x0+c0*direction;f0=fc0;if_exceed=.false.
    elseif ((fb.eq.min(fa,fb,fc,fc0,f0)).and.wolfe_condition(f0,g0,fb,ffb,b)) then
			grad0=gradb;x0=x0+b*direction;f0=fb;if_exceed=.false.
    elseif ((fc<f0).and.wolfe_condition(f0,g0,fc,ffc,c)) then
			grad0=gradc;x0=x0+c*direction;f0=fc;if_exceed=.false.    
    elseif ((fc0<f0).and.wolfe_condition(f0,g0,fc0,ffc0,c0)) then
			grad0=gradc0;x0=x0+c0*direction;f0=fc0;if_exceed=.false.
	elseif ((fb<f0).and.wolfe_condition(f0,g0,fb,ffb,b)) then
			grad0=gradb;x0=x0+b*direction;f0=fb;if_exceed=.false.
	elseif ((fc0<min(fa,fb,fc,f0))) then
			grad0=gradc0;x0=x0+c0*direction;f0=fc0;if_exceed=.true.
	elseif ((fc<min(fa,fb,fc0,f0))) then
			grad0=gradc;x0=x0+c*direction;f0=fc;if_exceed=.true.
	elseif (fb<min(fa,f0,fc,fc0)) then
	       grad0=gradb;x0=x0+b*direction;f0=fb;if_exceed=.true.
	 else 
	       eps0=eps0/10
          goto 777
	  endif
	  ncomp=ncomp+counter;
  end subroutine LINE_SEARCH2
  subroutine LINE_SEARCH1(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)
    use norma0
    implicit none
    integer ,intent(in)::nn
    INTEGER,INTENT(IN) ::NITER
    doubleprecision,INTENT(IN)::DIRECTION(nn)
    doubleprecision::X0(nn),GRAD0(nn),EPS0,F0
    DOUBLEPRECISION X1,X2,A,B,C,C0,D,F1,F2,fn,fnew,fa,fb,fc,g0,b0,FC0,FFC0
    DOUBLEPRECISION XC1,XC2,ffa,ffb,ffc,gamma
    doubleprecision,dimension(nn)::grada,gradb,gradc,GRADC0
    integer i,counter
    integer,intent(inout)::ncomp
    logical if_suff,if_restart,i_exist,ifsucc,if_exceed
    character(len=9):: ctrlname,costname
111 FORMAT (A,E19.11E2,E19.11E2,E19.11E2)
112 FORMAT (i3,A,E19.11E2,E19.11E2,E19.11E2)
     gamma=1.0
10  a=0;b=eps0;grada=grad0;fa=f0;if_suff=.false.;counter=1
    do i=1,nn
       if (my_is_nan(x0(i)).or.my_is_nan(grad0(i))) then
          print *,i,x0(i),grad0(i)
       endif
    enddo
    ffa=gamma*(grada.x.(direction));g0=ffa
    if (ffa>0) then 
      gamma=-1.0;
      goto 10
    endif
    
    write(0,111) "initial a " ,a,fa,ffa
    if (if_restart) then
       INQUIRE (FILE='linesearch.sav', EXIST=I_EXIST) 
       if (I_EXIST) then 
          if_restart=.false.
          open(FILE="linesearch.sav",unit=171,action='read',ERR=10)

          read(171,*)i
          close(171)
          if_restart=.true.
          write(0,'(a)')"Штатный Перезапуск"
          if (i.eq.1) then

             call restore_res(nn,a,grada,fa,'a',if_restart)
             if (.not.if_restart) then
                write(0,'(a)')"Точка восстановления не найдена. Перезапуск"
                goto 10
             end if
             ffa=grada.x.direction
             call restore_res(nn,b,gradb,fb,'b',if_restart)
             if (.not.if_restart) then
                write(0,'(a)')"Точка восстановления не найдена. Перезапуск"
                goto 10
             end if
             ffb=gradb.x.direction
             goto 30
          elseif (i.eq.2) then
                       call restore_res(nn,a,grada,fa,'a',if_restart)
             if (.not.if_restart) then
                write(0,'(a)')"Точка восстановления не найдена. Перезапуск"
                goto 10
             end if
             ffa=grada.x.direction
             call restore_res(nn,b,gradb,fb,'b',if_restart)
             if (.not.if_restart) then
                write(0,'(a)')"Точка восстановления не найдена. Перезапуск"
                goto 10
             end if
             ffb=gradb.x.direction
             goto 40
          else
             write(0,'(a)')"Точка восстановления не найдена. Перезапуск"
             if_restart=.false.
             goto 10
          endif
       else
          write(0,'(a)')"Точка восстановления не найдена. Перезапуск"
          if_restart=.false.
          goto 10
       endif

    else
!       call  dump_res(nn,0.0,grad0,f0,'a')
       call system("sync")
    endif

    !    call compute(nn,x0,fa,grada,ctrlname,costname,direction)
    call compute(nn,x0+eps0*gamma*direction,fb,gradb,ctrlname,costname,direction)
    ffb=gradb.x.direction;
    write(0,111) "initial b " ,b,fb,ffb
    call  dump_res(nn,eps0,gradb,fb,'b')
    call system("sync")
30  do while(((ffb*ffa.ge.0)))
       if (ffa.eq.ffb) then
          b0=b*10
       elseif (-(ffa*b-ffb*a)/(ffb-ffa).ge.sqrt(2.0)*b) then
          b0=max(4*b,min(-(ffa*b-ffb*a)/(ffb-ffa),10*b))
       else
          b0=b*sqrt(2.)
          if ((ffb<0.0).and.(fb<fa)) then
             b0=b0*sqrt(101.0)
          endif
       endif
       if ((fb<fa).and.(ffb<0)) then
           a=b;fa=fb;grada=gradb;ffa=ffb
    endif
       write(0,'(a,E16.8E2)')"Приближение изоляции минимума",b0
       b=b0

       call compute(nn,x0+b*gamma*direction,fb,gradb,ctrlname,costname,direction)
       call  dump_res(nn,b,gradb,fb,'b')
       ffb=gamma*(gradb.x.direction);
       write(0,111) "initial b " ,b,fb,ffb
       counter=counter+1
       open(FILE="linesearch.sav",unit=171,action='write',status='replace',ERR=10)
       write(171,*)1
       close(171)
       call  dump_res(nn,a,grada,fa,'a')
       call  dump_res(nn,b,gradb,fb,'b')
       call system("sync")
    enddo

 40   if_suff=(((fa<0.9*f0).and.(wolfe_condition(f0,g0,fa,ffa,a))).or.((wolfe_condition(f0,g0,fb,ffb,b)).and.(fb<0.9*f0)))
    do  while((.not. if_suff))
       c=davidon(a,b,fa,fb,ffa,ffb)

       if ((ffa.le.0).and.((c-a)*(c-b)<0)) then
          if (min(abs(a-c),abs(b-c))<1e-12*abs(a-b)) then
             if (abs(a-c)<abs(b-c)) then
                c=a+(b-a)*(1-(sqrt(5.)-1)/2)
             else
                c=a+(b-a)*(sqrt(5.)-1)/2
             endif
          endif
       else
          c=-(ffa*b-ffb*a)/(ffb-ffa)
          if (((c-a)*(c-b)>0)) then
          c=0.5*(2*a*(fb-fa)-ffa*(b**2-a**2))/(fb-fa-ffa*(b-a))
          endif
          if ((my_is_nan(c)).or.(min(abs(a-c),abs(b-c))<1e-12*abs(a-b)).or.((c-a)*(c-b)>0)) then
             c=(a+b)/2
          endif

       endif
       write(0,'(a,E16.8E2)')"Интерполяция давидона дала",c

       call compute(nn,x0+c*gamma*direction,fc,gradc,ctrlname,costname,direction) 
       ffc=gradc.x.direction;
       counter=counter+1
       if_exceed=counter.gt.niter
       if_suff=if_suff.or.(((fc<f0).and.wolfe_condition(f0,g0,fc,ffc,c))).or.if_exceed
       write(0,111) "a " ,a,fa,ffa
       write(0,111) "b " ,b,fb,ffb
       write(0,112) counter,"c" ,c,fc,ffc

       if (ffa*ffb<0) then
          if (ffa*ffc<0) then
             b=c;gradb=gradc;ffb=ffc;fb=fc
          else
              if (fc>fa) then 
				C0=davidon(a,C,fa,fC,ffa,ffC)
				call compute(nn,x0+c0*gamma*direction,fc0,gradc0,ctrlname,costname,direction) 
                ffc0=gradc0.x.direction;
                 IF ((FC0<min(fb,FA)).AND.(FFA*FFC0>0)) THEN
                   if (c0<a) then
                    B=a;GRADB=GRADa;FFB=FFa;FB=Fa
                   endif
                   A=C0;GRADA=GRADC0;FFA=FFC0;FA=FC0
                 ELSEIF (FFB*FFC0>0) THEN
                   B=C0;GRADB=GRADC0;FFB=FFC0;FB=FC0
                 
                 ENDIF
              else
                  a=c;grada=gradc;ffa=ffc;fa=fc
              endif    
          endif

       else
          if (ffa*ffc<0) then
             b=c;gradb=gradc;ffb=ffc;fb=fc
          else
             if (fc<fa) then
                if (ffa>0) then
                   b=a;gradb=grada;ffb=ffa;fb=fa
                endif
                a=c;grada=gradc;ffa=ffc;fa=fc
             else
                b=c;gradb=gradc;ffb=ffc;fb=fc
             endif

          endif

       endif
       open(FILE="linesearch.sav",unit=171,action='write',status='replace',ERR=10)
       write(171,*)2
       close(171)
       call  dump_res(nn,a,grada,fa,'a')
       call  dump_res(nn,b,gradb,fb,'b')
       call system("sync")
    enddo
300 if (fa<fb) then
       f0=fa;grad0=grada;x0=x0+a*gamma*direction

    else
       f0=fb;grad0=gradb;x0=x0+b*gamma*direction
    endif




    call system("rm *.txt")
    call system("rm linesearch.sav")
    call system("sync")
  ncomp=ncomp+counter
  END SUBROUTINE LINE_SEARCH1
  subroutine LINE_SEARCH3(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)
    use norma0
    implicit none
    integer ,intent(in)::nn
    INTEGER,INTENT(IN) ::NITER
    doubleprecision,INTENT(IN)::DIRECTION(nn)
    doubleprecision::X0(nn),GRAD0(nn),EPS0,F0
    DOUBLEPRECISION X1,X2,A,B,C,C0,D,F1,F2,fn,fnew,fa,fb,fc,g0,b0,FC0,FFC0
    DOUBLEPRECISION XC1,XC2,ffa,ffb,ffc,gamma
    doubleprecision,dimension(nn)::grada,gradb,gradc,GRADC0
    integer i,counter
    integer,intent(inout)::ncomp
    logical if_suff,if_restart,i_exist,ifsucc,if_exceed
    character(len=9):: ctrlname,costname
111 FORMAT (A,E19.11E2,E19.11E2,E19.11E2)
112 FORMAT (i3,A,E19.11E2,E19.11E2,E19.11E2)
     gamma=1.0
101  a=0;b=eps0;grada=grad0;fa=f0;if_exceed=.false.;counter=1
    do i=1,nn
       if (my_is_nan(x0(i)).or.my_is_nan(grad0(i))) then
          print *,i,x0(i),grad0(i)
       endif
    enddo
    ffa=gamma*(grada.x.(direction));g0=ffa
    if (ffa>0) then 
      gamma=-1.0;
      goto 101
    endif
    
    write(0,111) "initial a " ,a,fa,ffa
    
    !    call compute(nn,x0,fa,grada,ctrlname,costname,direction)
    call compute(nn,x0+eps0*gamma*direction,fb,gradb,ctrlname,costname,direction)
    ffb=gamma*(gradb.x.direction);
    write(0,111) "initial b " ,b,fb,ffb
    call system("sync")
    if ((fb.le.f0)) then

                  if (fb.eq.f0) then
                      return
                  endif

 		f0=fb;grad0=gradb;x0=x0+b*gamma*direction;eps0=b*gamma
 		if_exceed=.true.
		return
	 endif

401 if (ffb>0) then
   c=davidon(a,b,fa,fb,ffa,ffb)
 else
   c=a+0.2*(b-a)
endif

call compute(nn,x0+c*gamma*direction,fc,gradc,ctrlname,costname,direction)
   ffc=gamma*(gradc.x.direction);
       write(0,111) "a " ,a,fa,ffa
       write(0,111) "b " ,b,fb,ffb
       write(0,111) "c " ,c,fc,ffc
    if ((fc<f0)) then

 		f0=fc;grad0=gradc;x0=x0+c*gamma*direction;eps0=c*gamma
		return
	 endif
 b=c;gradb=gradc;ffb=ffc;fb=fc




goto 401


  END SUBROUTINE LINE_SEARCH3
  
  DOUBLEPRECISION function davidon(a,b,fa,fb,ffa,ffb)
    ! f1 - f(x1),f3 -f'(x3)
    ! f2 -f(x2),f4 -f'(x4)
    implicit none
    DOUBLE PRECISION,INTENT(IN)::a,b,fa,fb,ffa,ffb
    DOUBLEPRECISION::a0,b0,c0,x1,x2
    a0=(b*ffb-a*ffb+b*ffa-a*ffa-2*fb+2*fa)/(b-a)**3
    b0=-(b**2*ffb+a*b*ffb-2*a**2*ffb+2*b**2*ffa-a*b*ffa-a**2*ffa-3*b*fb-&
         &3*a*fb+3*b*fa+3*a*fa)/(b-a)**3
    c0=(2*a*b**2*ffb-a**2*b*ffb-a**3*ffb+b**3*ffa+a*b**2*ffa-2*a**2*b*ff&
         &a-6*a*b*fb+6*a*b*fa)/(b-a)**3
    if (a0.eq.0*a0) then
       davidon=a
    endif
    if (b0**2-a0*c0.lt.0) then
       davidon=a
    endif

    x1=(-b0+sqrt(b0**2-a0*c0))/(a0)
    x2=(-b0+sqrt(b0**2-a0*c0))/(a0)
    if (6*a0*x1+2*b0>0) then
       davidon=x1
    else
       davidon=x2
    endif

  end function davidon

  logical function wolfe_condition(f0,g0,f1,g1,alpha)
    doubleprecision,intent(in)::f0,g0,f1,g1,alpha
    doubleprecision,parameter::alpha2=1e-3,alpha1=0.5e-4

    wolfe_condition=(g1.ge.alpha2*g0).and.(f1 .le. f0 + alpha1*alpha*g0)



  end function wolfe_condition

  function Ak(m,nn,k,g,x,q,z)
    use norma0
    integer m,nn
    double precision ,dimension(m+1,nn)::g,x,q
    double precision alpha,beta,z(nn),Ak(nn)
    integer r,i,j,k
    real*8 summ
    alpha=(g(k+1,:)-g(k,:)).x.(x(k+1,:)-x(k,:))
    beta=(x(k+1,:)-x(k,:)-q(k,:)).x.(g(k+1,:)-g(k,:))/alpha**2
    do i=1,nn
       summ=0
       do j=1,nn
          summ=summ+z(j)*(((x(k+1,i)-x(k,i)-q(k,i))*(x(k+1,j)-x(k,j))+(x(k+1,i)-x(k,i))*(x(k+1,j)-x(k,j)-q(k,j)))/alpha&
               &+ beta*(x(k+1,i)-x(k,i))*(x(k+1,j)-x(k,j)))
       enddo
       Ak(i)=summ
    enddo
  end function Ak
    subroutine dump_res(nn,x,grad,f,pref)
    integer,intent(in)::nn
    double precision,intent(in)::x,grad(nn),f
    character(len=*)::pref

    open(unit=1943,file=pref//'_func_val.txt',action='write',status='replace')
    write(1943,*),f
    close(1943) 



    open(unit=1943,file=pref//'_func_grad.txt',action='write',status='replace')
    write(1943,*),grad
    close(1943) 


    open(unit=1943,file=pref//'_func_x.txt',action='write',status='replace')
    write(1943,*),x
    close(1943) 


    open(unit=1943,file=pref//'_func_val.txt',action='write',status='replace')
    write(1943,*),f
    close(1943) 

  end subroutine dump_res
  subroutine restore_res(nn,x,grad,f,pref,ifsucc)
    integer,intent(in)::nn
    double precision,intent(inout)::x,grad(nn),f
    character(len=*)::pref
    logical ifsucc
    ifsucc=.true.
    inquire(file=pref//'_func_val.txt',EXIST=ifsucc)
    if(.not.ifsucc) then
       return
    endif
    open(unit=1943,file=pref//'_func_val.txt',action='read',ERR=100)
    read(1943,*),f
    close(1943) 

    inquire(file=pref//'_func_grad.txt',EXIST=ifsucc)
    if(.not.ifsucc) then
       return
    endif

    open(unit=1943,file=pref//'_func_grad.txt',action='read',ERR=100)
    read(1943,*),grad
    close(1943) 


    inquire(file=pref//'_func_x.txt',EXIST=ifsucc)
    if(.not.ifsucc) then
       return
    endif

    open(unit=1943,file=pref//'_func_x.txt',action='read',ERR=100)
    read(1943,*),x
    close(1943) 


    open(unit=1943,file=pref//'_func_val.txt',action='read')
    read(1943,*),f
    close(1943) 

    return
100 ifsucc=.false.
    return
  end subroutine restore_res
  
end module
