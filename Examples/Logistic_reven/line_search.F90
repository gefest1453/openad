module line_searchs
  
  
  use norma0
  private compute1,compute2
  interface compute
     module procedure compute1,compute2
  end interface compute
  contains
  recursive function h_mul(k,nn,nmax,yy,xx,x0) result(ret)
    use norma0
    !  use prog_bar
    integer,intent(in)::k,nn,nmax
    double precision,dimension(nmax,nn)::yy,xx
    doubleprecision::ret(nn),ret0(nn),ret1(nn),alpha
    doubleprecision,intent(in)::x0(nn)
    
    integer m,n,p
    
    if (k.eq.1) then
       ret=x0;
    else
       ret=x0;
       do m=1,nn
          ret0(m)=0
          do n=1,nn
             ret0(m)=ret0(m)+(xx(k,n)-xx(k-1,n))*x0(n)
          enddo
          ret0(m)=ret0(m)*(xx(k,m)-xx(k-1,m))
       enddo
       ret1=h_mul(k-1,nn,nmax,yy,xx,yy(k-1,:))
       alpha=((xx(k,:)-xx(k-1,:)).x.yy(k-1,:))
       ret=ret+ret0*(1+yy(k-1,:).x.ret1/alpha)/alpha
       ret0=0
       do m=1,nn
          do n=1,nn
             ret0(m)=ret0(m)+ret1(m)*yy(k-1,n)+ret1(n)*yy(k-1,m)
          enddo
       enddo
       ret=ret+ret0/alpha
    endif
  end function h_mul
  
  subroutine compute1(x,f,grad)
    use OAD_active
    use OAD_tape
    use OAD_rev
    use w2f__types
    use log_eq
    use revstats
    implicit none
    doubleprecision ::dt,f,x(3),grad(3)
    type(active) :: X0
    type(active) :: K
    type(active) :: L
    type(active) ::ad_cost0
    integer N
    N=10;dt=0.01
    revStatsRevolveCPcount=N
    call oad_tape_init()
    call revStatsInit()
    
    call OAD_revPlain()
    call zero_deriv(xcurr)
    call zero_deriv(k_act)
    call zero_deriv(l_act)
    call setup_par(x(1),x(2),x(3),dt,30)
    
    summ%d=1.0
    summ%v=0.0
    call OAD_revTape()
    call AD_COST()
    f=summ%v
    
    call OAD_revAdjoint()
    call AD_COST()
    grad(1)=xcurr%d
    grad(2)=l_act%d
    grad(3)=k_act%d
    
  end subroutine compute1
  
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
    
777 call compute(nn,x0+eps0*direction,fb,gradb,ctrlname,costname,direction)
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
  subroutine LINE_SEARCH0(NN,X0,GRAD0,F0,DIRECTION,EPS0,NITER,ctrlname,costname,if_restart,if_exceed,ncomp)
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
    
    !    call compute(nn,x0,fa,grada,ctrlname,costname,direction)
    call compute(nn,x0+eps0*gamma*direction,fb,gradb,ctrlname,costname,direction)
    ffb=gradb.x.direction;
    write(0,111) "initial b " ,b,fb,ffb
    
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
       
       ffb=gamma*(gradb.x.direction);
       write(0,111) "initial b " ,b,fb,ffb
       counter=counter+1
    enddo
    
40  if_suff=(((fa<0.9*f0).and.(wolfe_condition0(grada,direction))).or.((wolfe_condition0(gradb,direction)).and.(fb<0.9*f0)))
    do  while((.not. if_suff))
       c=davidon(a,b,fa,fb,ffa,ffb)
       
       if ((ffa.le.0).and.((c-a)*(c-b)<0)) then
          if (min(abs(a-c),abs(b-c))<1e-4*abs(a-b)) then
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
       if_suff=if_suff.or.(((fc<f0).and.wolfe_condition0(gradc,direction))).or.if_exceed
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
    enddo
300 if (fa<fb) then
       f0=fa;grad0=grada;x0=x0+a*gamma*direction
       
    else
       f0=fb;grad0=gradb;x0=x0+b*gamma*direction
    endif
    
    
    
    
    ncomp=ncomp+counter
  END SUBROUTINE LINE_SEARCH0
  
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
    
40  if_suff=(((fa<0.9*f0).and.(wolfe_condition(f0,g0,fa,ffa,a))).or.((wolfe_condition(f0,g0,fb,ffb,b)).and.(fb<0.9*f0)))
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
101 a=0;b=eps0;grada=grad0;fa=f0;if_exceed=.false.;counter=1
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
  logical function wolfe_condition0(grad1,dir)
    doubleprecision,intent(in)::grad1(:),dir(:)
    doubleprecision,parameter::alpha2=1e-3,alpha1=0.5e-4
    
    wolfe_condition0=(-grad1.x.dir).ge.alpha2*norma(grad1)*norma(dir)
    
    
    
  end function wolfe_condition0
  
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
  
end module line_searchs
    module lb2
         
      INTEGER LP,MP
      DOUBLE PRECISION GTOL,STPMIN,STPMAX
!      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
      DATA MP,LP,GTOL,STPMIN,STPMAX/6,6,9.0D-01,1.0D-20,1.0D+20/

    end module
    
module BFGS
private daxpy,mcstep, MCSRCH,LB1

    contains
    SUBROUTINE LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG)

      USE LB2
      USE NORMA0
      IMPLICIT NONE
      INTEGER N,M,IPRINT(2),IFLAG
      DOUBLE PRECISION X(N),G(N),DIAG(N),W(N*(2*M+1)+2*M)
      DOUBLE PRECISION F,EPS,XTOL
      LOGICAL DIAGCO
!
!        LIMITED MEMORY BFGS METHOD FOR LARGE SCALE OPTIMIZATION
!                          JORGE NOCEDAL
!                        *** July 1990 ***
!
! 
!     This subroutine solves the unconstrained minimization problem
! 
!                      min F(x),    x= (x1,x2,...,xN),
!
!      using the limited memory BFGS method. The routine is especially
!      effective on problems involving a large number of variables. In
!      a typical iteration of this method an approximation Hk to the
!      inverse of the Hessian is obtained by applying M BFGS updates to
!      a diagonal matrix Hk0, using information from the previous M steps.
!      The user specifies the number M, which determines the amount of
!      storage required by the routine. The user may also provide the
!      diagonal matrices Hk0 if not satisfied with the default choice.
!      The algorithm is described in "On the limited memory BFGS method
!      for large scale optimization", by D. Liu and J. Nocedal,
!      Mathematical Programming B 45 (1989) 503-528.
! 
!      The user is required to calculate the function value F and its
!      gradient G. In order to allow the user complete control over
!      these computations, reverse  communication is used. The routine
!      must be called repeatedly under the control of the parameter
!      IFLAG. 
!
!      The steplength is determined at each iteration by means of the
!      line search routine MCVSRCH, which is a slight modification of
!      the routine CSRCH written by More' and Thuente.
! 
!      The calling statement is 
! 
!          CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG)
! 
!      where
! 
!     N       is an INTEGER variable that must be set by the user to the
!             number of variables. It is not altered by the routine.
!             Restriction: N>0.
! 
!     M       is an INTEGER variable that must be set by the user to
!             the number of corrections used in the BFGS update. It
!!             is not altered by the routine. Values of M less than 3 are
!             not recommended; large values of M will result in excessive
!             computing time. 3<= M <=7 is recommended. Restriction: M>0.
! 
!     X       is a DOUBLE PRECISION array of length N. On initial entry
!             it must be set by the user to the values of the initial
!             estimate of the solution vector. On exit with IFLAG=0, it
!             contains the values of the variables at the best point
!             found (usually a solution).
! 
!     F       is a DOUBLE PRECISION variable. Before initial entry and on
!             a re-entry with IFLAG=1, it must be set by the user to
!             contain the value of the function F at the point X.
! 
!     G       is a DOUBLE PRECISION array of length N. Before initial
!             entry and on a re-entry with IFLAG=1, it must be set by
!             the user to contain the components of the gradient G at
!             the point X.
! 
!     DIAGCO  is a LOGICAL variable that must be set to .TRUE. if the
!             user  wishes to provide the diagonal matrix Hk0 at each
!             iteration. Otherwise it should be set to .FALSE., in which
!             case  LBFGS will use a default value described below. If
!             DIAGCO is set to .TRUE. the routine will return at each
!             iteration of the algorithm with IFLAG=2, and the diagonal
!              matrix Hk0  must be provided in the array DIAG.
! 
! 
!     DIAG    is a DOUBLE PRECISION array of length N. If DIAGCO=.TRUE.,
!             then on initial entry or on re-entry with IFLAG=2, DIAG
!             it must be set by the user to contain the values of the 
!             diagonal matrix Hk0.  Restriction: all elements of DIAG
!             must be positive.
! 
!     IPRINT  is an INTEGER array of length two which must be set by the
!             user.
! 
!             IPRINT(1) specifies the frequency of the output:
!                IPRINT(1) < 0 : no output is generated,
!                IPRINT(1) = 0 : output only at first and last iteration,
!                IPRINT(1) > 0 : output every IPRINT(1) iterations.
! 
!             IPRINT(2) specifies the type of output generated:
!                IPRINT(2) = 0 : iteration count, number of function 
!                                evaluations, function value, norm of the
!                                gradient, and steplength,
!                IPRINT(2) = 1 : same as IPRINT(2)=0, plus vector of
!                                variables and  gradient vector at the
!                                initial point,
!                IPRINT(2) = 2 : same as IPRINT(2)=1, plus vector of
!                                variables,
!                IPRINT(2) = 3 : same as IPRINT(2)=2, plus gradient vector.
! 
! 
!     EPS     is a positive DOUBLE PRECISION variable that must be set by
!             the user, and determines the accuracy with which the solution
!             is to be found. The subroutine terminates when
!
!                         ||G|| < EPS max(1,||X||),
!
!             where ||.|| denotes the Euclidean norm.
! 
!     XTOL    is a  positive DOUBLE PRECISION variable that must be set by
!             the user to an estimate of the machine precision (e.g.
!             10**(-16) on a SUN station 3/60). The line search routine will
!             terminate if the relative width of the interval of uncertainty
!             is less than XTOL.
! 
!     W       is a DOUBLE PRECISION array of length N(2M+1)+2M used as
!             workspace for LBFGS. This array must not be altered by the
!             user.
! 
!     IFLAG   is an INTEGER variable that must be set to 0 on initial entry
!             to the subroutine. A return with IFLAG<0 indicates an error,
!             and IFLAG=0 indicates that the routine has terminated without
!             detecting errors. On a return with IFLAG=1, the user must
!             evaluate the function F and gradient G. On a return with
!             IFLAG=2, the user must provide the diagonal matrix Hk0.
! 
!             The following negative values of IFLAG, detecting an error,
!             are possible:
! 
!              IFLAG=-1  The line search routine MCSRCH failed. The
!                        parameter INFO provides more detailed information
!                        (see also the documentation of MCSRCH):
!
!                       INFO = 0  IMPROPER INPUT PARAMETERS.
!
!                       INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF
!                                 UNCERTAINTY IS AT MOST XTOL.
!
!                       INFO = 3  MORE THAN 20 FUNCTION EVALUATIONS WERE
!                                 REQUIRED AT THE PRESENT ITERATION.
!
!                       INFO = 4  THE STEP IS TOO SMALL.
!
!                       INFO = 5  THE STEP IS TOO LARGE.
!
!                       INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS. 
!                                 THERE MAY NOT BE A STEP WHICH SATISFIES
!                                 THE SUFFICIENT DECREASE AND CURVATURE
!                                 CONDITIONS. TOLERANCES MAY BE TOO SMALL.
!
! 
!              IFLAG=-2  The i-th diagonal element of the diagonal inverse
!                        Hessian approximation, given in DIAG, is not
!                        positive.
!           
!              IFLAG=-3  Improper input parameters for LBFGS (N or M are
!                        not positive).
! 
!
!
!    ON THE DRIVER:
!
!    The program that calls LBFGS must contain the declaration:
!
!                       EXTERNAL LB2
!
!    LB2 is a BLOCK DATA that defines the default values of several
!    parameters described in the COMMON section. 
!
! 
! 
!    COMMON:
! 
!     The subroutine contains one common area, which the user may wish to
!    reference:
! 

! 
!    MP  is an INTEGER variable with default value 6. It is used as the
!        unit number for the printing of the monitoring information
!        controlled by IPRINT.
! 
!    LP  is an INTEGER variable with default value 6. It is used as the
!        unit number for the printing of error messages. This printing
!        may be suppressed by setting LP to a non-positive value.
! 
!    GTOL is a DOUBLE PRECISION variable with default value 0.9, which
!        controls the accuracy of the line search routine MCSRCH. If the
!        function and gradient evaluations are inexpensive with respect
!        to the cost of the iteration (which is sometimes the case when
!        solving very large problems) it may be advantageous to set GTOL
!        to a small value. A typical small value is 0.1.  Restriction:
!        GTOL should be greater than 1.D-04.
! 
!    STPMIN and STPMAX are non-negative DOUBLE PRECISION variables which
!        specify lower and uper bounds for the step in the line search.
!        Their default values are 1.D-20 and 1.D+20, respectively. These
!        values need not be modified unless the exponents are too large
!        for the machine being used, or unless the problem is extremely
!        badly scaled (in which case the exponents should be increased).
! 
!
!  MACHINE DEPENDENCIES
!
!        The only variables that are machine-dependent are XTOL,
!        STPMIN and STPMAX.
! 
!
!  GENERAL INFORMATION
! 
!    Other routines called directly:  DAXPY, DDOT, LB1, MCSRCH
! 
!    Input/Output  :  No input; diagnostic messages on unit MP and
!                     error messages on unit LP.
! 
! 
!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      DOUBLE PRECISION ONE,ZERO,GNORM,STP1,FTOL,&
          &STP,YS,YY,SQ,YR,BETA,XNORM
      INTEGER ITER,NFUN,POINT,ISPT,IYPT,MAXFEV,INFO,&
          &BOUND,NPT,CP,I,NFEV,INMC,IYCN,ISCN
      LOGICAL FINISH
!
      SAVE
      DATA ONE,ZERO/1.0D+0,0.0D+0/
      200  FORMAT(/' IFLAG= -1 ',/' LINE SEARCH FAILED. SEE'&
           &' DOCUMENTATION OF ROUTINE MCSRCH',/' ERROR RETURN'&
           &' OF LINE SEARCH: INFO= ',I2,/&
           &'POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT',/,&
           &' OR INCORRECT TOLERANCES')
 235  FORMAT(/' IFLAG= -2',/' THE',I5,'-TH DIAGONAL ELEMENT OF THE',/,&
            &' INVERSE HESSIAN APPROXIMATION IS NOT POSITIVE')
 240  FORMAT(/' IFLAG= -3',/' IMPROPER INPUT PARAMETERS (N OR M',&
           &' ARE NOT POSITIVE)')
 245  FORMAT(/'  GTOL IS LESS THAN OR EQUAL TO 1.D-04',&
           &/ ' IT HAS BEEN RESET TO 9.D-01')
 
!
!     INITIALIZE
!     ----------
!
      IF(IFLAG.EQ.0) GO TO 10
      GO TO (172,100) IFLAG
  10  ITER= 0
      IF(N.LE.0.OR.M.LE.0) GO TO 196
      IF(GTOL.LE.1.D-04) THEN
        IF(LP.GT.0) WRITE(LP,245)
        GTOL=9.D-01
      ENDIF
      NFUN= 1
      POINT= 0
      FINISH= .FALSE.
      IF(DIAGCO) THEN
         DO 30 I=1,N
 30      IF (DIAG(I).LE.ZERO) GO TO 195
      ELSE
         DO 40 I=1,N
 40      DIAG(I)= 1.0D0
      ENDIF
!
!     THE WORK VECTOR W IS DIVIDED AS FOLLOWS:
!     ---------------------------------------
!     THE FIRST N LOCATIONS ARE USED TO STORE THE GRADIENT AND
!        OTHER TEMPORARY INFORMATION.
!     LOCATIONS (N+1)...(N+M) STORE THE SCALARS RHO.
!     LOCATIONS (N+M+1)...(N+2M) STORE THE NUMBERS ALPHA USED
!         IN THE FORMULA THAT COMPUTES H*G.
!     LOCATIONS (N+2M+1)...(N+2M+NM) STORE THE LAST M SEARCH
!         STEPS.
!     LOCATIONS (N+2M+NM+1)...(N+2M+2NM) STORE THE LAST M
!         GRADIENT DIFFERENCES.
!
!     THE SEARCH STEPS AND GRADIENT DIFFERENCES ARE STORED IN A
!     CIRCULAR ORDER CONTROLLED BY THE PARAMETER POINT.
!
      ISPT= N+2*M
      IYPT= ISPT+N*M     
      DO 50 I=1,N
 50   W(ISPT+I)= -G(I)*DIAG(I)
      GNORM= DSQRT(DDOT(N,G,1,G,1))
      STP1= ONE/GNORM
!
!     PARAMETERS FOR LINE SEARCH ROUTINE
!     
      FTOL= 1.0D-4
      MAXFEV= 20
!
      IF(IPRINT(1).GE.0) THEN
       CALL LB1(IPRINT,ITER,NFUN,GNORM,N,M,X,F,G,STP,FINISH)
       ENDIF
!
!    --------------------
!     MAIN ITERATION LOOP
!    --------------------
!
 80   ITER= ITER+1
      INFO=0
      BOUND=ITER-1
      IF(ITER.EQ.1) GO TO 165
      IF (ITER .GT. M)BOUND=M
!
         YS= DDOT(N,W(IYPT+NPT+1:N*(2*M+1)+2*M),1,W(ISPT+NPT+1:N*(2*M+1)+2*M),1)
      IF(.NOT.DIAGCO) THEN
         YY= DDOT(N,W(IYPT+NPT+1:N*(2*M+1)+2*M),1,W(IYPT+NPT+1:N*(2*M+1)+2*M),1)
         DO 90 I=1,N
   90    DIAG(I)= YS/YY
      ELSE
         IFLAG=2
         RETURN
      ENDIF
 100  CONTINUE
      IF(DIAGCO) THEN
        DO 110 I=1,N
 110    IF (DIAG(I).LE.ZERO) GO TO 195
      ENDIF
!
!     COMPUTE -H*G USING THE FORMULA GIVEN IN: Nocedal, J. 1980,
!     "Updating quasi-Newton matrices with limited storage",
!     Mathematics of Computation, Vol.24, No.151, pp. 773-782.
!     ---------------------------------------------------------
!
      CP= POINT
      IF (POINT.EQ.0) CP=M
      W(N+CP)= ONE/YS
      DO 112 I=1,N
 112  W(I)= -G(I)
      CP= POINT
      DO 125 I= 1,BOUND
         CP=CP-1
         IF (CP.EQ. -1)CP=M-1
         SQ= DDOT(N,W(ISPT+CP*N+1:),1,W,1)
         INMC=N+M+CP+1
         IYCN=IYPT+CP*N
         W(INMC)= W(N+CP+1)*SQ
         CALL DAXPY(N,-W(INMC),W(IYCN+1),1,W,1)
 125  CONTINUE
!
      DO 130 I=1,N
 130  W(I)=DIAG(I)*W(I)
!
      DO 145 I=1,BOUND
         YR= DDOT(N,W(IYPT+CP*N+1:),1,W,1)
         BETA= W(N+CP+1)*YR
         INMC=N+M+CP+1
         BETA= W(INMC)-BETA
         ISCN=ISPT+CP*N
         CALL DAXPY(N,BETA,W(ISCN+1),1,W,1)
         CP=CP+1
         IF (CP.EQ.M)CP=0
 145  CONTINUE
!
!     STORE THE NEW SEARCH DIRECTION
!     ------------------------------
!
       DO 160 I=1,N
 160   W(ISPT+POINT*N+I)= W(I)
!
!     OBTAIN THE ONE-DIMENSIONAL MINIMIZER OF THE FUNCTION 
!     BY USING THE LINE SEARCH ROUTINE MCSRCH
!     ----------------------------------------------------
 165  NFEV=0
      STP=ONE
      IF (ITER.EQ.1) STP=STP1
      DO 170 I=1,N
 170  W(I)=G(I)
 172  CONTINUE
      CALL MCSRCH(N,X,F,G,W(ISPT+POINT*N+1),STP,FTOL,XTOL,MAXFEV,INFO,NFEV,DIAG)
      IF (INFO .EQ. -1) THEN
        IFLAG=1
        RETURN
      ENDIF
      IF (INFO .NE. 1) GO TO 190
      NFUN= NFUN + NFEV
!
!     COMPUTE THE NEW STEP AND GRADIENT CHANGE 
!     -----------------------------------------
!
      NPT=POINT*N
      DO 175 I=1,N
      W(ISPT+NPT+I)= STP*W(ISPT+NPT+I)
 175  W(IYPT+NPT+I)= G(I)-W(I)
      POINT=POINT+1
      IF (POINT.EQ.M)POINT=0
!
!     TERMINATION TEST
!     ----------------
!
      
      GNORM= norma(G)

      XNORM= norma(x)
      XNORM= DMAX1(1.0D0,XNORM)
      IF (GNORM/XNORM .LE. EPS) FINISH=.TRUE.
!
      IF(IPRINT(1).GE.0) CALL LB1(IPRINT,ITER,NFUN,GNORM,N,M,X,F,G,STP,FINISH)
      IF (FINISH) THEN
         IFLAG=0
         RETURN
      ENDIF
      GO TO 80
!
!     ------------------------------------------------------------
!     END OF MAIN ITERATION LOOP. ERROR EXITS.
!     ------------------------------------------------------------
!
 190  IFLAG=-1
      IF(LP.GT.0) WRITE(LP,200) INFO
      RETURN
 195  IFLAG=-2
      IF(LP.GT.0) WRITE(LP,235) I
      RETURN
 196  IFLAG= -3
      IF(LP.GT.0) WRITE(LP,240)
!
!     FORMATS
!     -------
!
      RETURN
      END subroutine


   SUBROUTINE LB1(IPRINT,ITER,NFUN,GNORM,N,M,X,F,G,STP,FINISH)
   USE lb2   
   USE NORMA0
IMPLICIT NONE   
   INTEGER IPRINT(2),ITER,NFUN,N,M,I
      DOUBLE PRECISION X(N),G(N),F,GNORM,STP
      LOGICAL FINISH
      

 10   FORMAT('*************************************************')
 20   FORMAT('  N=',I5,'   NUMBER OF CORRECTIONS=',I2,&
           &/,  '       INITIAL VALUES')
 30   FORMAT(' F= ',1PD10.3,'   GNORM= ',1PD10.3)
 40   FORMAT(' VECTOR X= ')
 50   FORMAT(6(2X,1PD10.3))
 60   FORMAT(' GRADIENT VECTOR G= ')
 70   FORMAT(/'   I   NFN',4X,'FUNC',8X,'GNORM',7X,'STEPLENGTH'/)
 80   FORMAT(2(I4,1X),3X,3(1PD10.3,2X))
 90   FORMAT(' FINAL POINT X= ')
 100  FORMAT(/' THE MINIMIZATION TERMINATED WITHOUT DETECTING ERRORS.',&
       & /' IFLAG = 0')
      IF (ITER.EQ.0)THEN
           WRITE(MP,10)
           WRITE(MP,20) N,M
           WRITE(MP,30)F,GNORM
                 IF (IPRINT(2).GE.1)THEN
                     WRITE(MP,40)
                     WRITE(MP,50) (X(I),I=1,N)
                     WRITE(MP,60)
                     WRITE(MP,50) (G(I),I=1,N)
                  ENDIF
           WRITE(MP,10)
           WRITE(MP,70)
      ELSE
          IF ((IPRINT(1).EQ.0).AND.(ITER.NE.1.AND..NOT.FINISH))RETURN
              IF (IPRINT(1).NE.0)THEN
                   IF(MOD(ITER-1,IPRINT(1)).EQ.0.OR.FINISH)THEN
                         IF(IPRINT(2).GT.1.AND.ITER.GT.1) WRITE(MP,70)
                         WRITE(MP,80)ITER,NFUN,F,GNORM,STP
                   ELSE
                         RETURN
                   ENDIF
              ELSE
                   IF( IPRINT(2).GT.1.AND.FINISH) WRITE(MP,70)
                   WRITE(MP,80)ITER,NFUN,F,GNORM,STP
              ENDIF
              IF (IPRINT(2).EQ.2.OR.IPRINT(2).EQ.3)THEN
                    IF (FINISH)THEN
                        WRITE(MP,90)
                    ELSE
                        WRITE(MP,40)
                    ENDIF
                      WRITE(MP,50)(X(I),I=1,N)
                  IF (IPRINT(2).EQ.3)THEN
                      WRITE(MP,60)
                      WRITE(MP,50)(G(I),I=1,N)
                  ENDIF
              ENDIF
            IF (FINISH) WRITE(MP,*)
      ENDIF


      RETURN
      END SUBROUTINE

SUBROUTINE MCSRCH(N,X,F,G,S,STP,FTOL,XTOL,MAXFEV,INFO,NFEV,WA)
      use lb2
      USE NORMA0
IMPLICIT NONE
      INTEGER N,MAXFEV,INFO,NFEV
      DOUBLE PRECISION F,STP,FTOL,XTOL
      DOUBLE PRECISION X(N),G(N),S(N),WA(N)
      
      SAVE
!
!                     SUBROUTINE MCSRCH
!                
!     A slight modification of the subroutine CSRCH of More' and Thuente.
!     The changes are to allow reverse communication, and do not affect
!     the performance of the routine. 
!
!     THE PURPOSE OF MCSRCH IS TO FIND A STEP WHICH SATISFIES
!     A SUFFICIENT DECREASE CONDITION AND A CURVATURE CONDITION.
!
!     AT EACH STAGE THE SUBROUTINE UPDATES AN INTERVAL OF
!     UNCERTAINTY WITH ENDPOINTS STX AND STY. THE INTERVAL OF
!     UNCERTAINTY IS INITIALLY CHOSEN SO THAT IT CONTAINS A
!     MINIMIZER OF THE MODIFIED FUNCTION
!
!          F(X+STP*S) - F(X) - FTOL*STP*(GRADF(X)'S).
!
!     IF A STEP IS OBTAINED FOR WHICH THE MODIFIED FUNCTION
!     HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE DERIVATIVE,
!     THEN THE INTERVAL OF UNCERTAINTY IS CHOSEN SO THAT IT
!     CONTAINS A MINIMIZER OF F(X+STP*S).
!
!     THE ALGORITHM IS DESIGNED TO FIND A STEP WHICH SATISFIES
!     THE SUFFICIENT DECREASE CONDITION
!
!           F(X+STP*S) .LE. F(X) + FTOL*STP*(GRADF(X)'S),
!
!     AND THE CURVATURE CONDITION
!
!           ABS(GRADF(X+STP*S)'S)) .LE. GTOL*ABS(GRADF(X)'S).
!
!     IF FTOL IS LESS THAN GTOL AND IF, FOR EXAMPLE, THE FUNCTION
!     IS BOUNDED BELOW, THEN THERE IS ALWAYS A STEP WHICH SATISFIES
!     BOTH CONDITIONS. IF NO STEP CAN BE FOUND WHICH SATISFIES BOTH
!     CONDITIONS, THEN THE ALGORITHM USUALLY STOPS WHEN ROUNDING
!     ERRORS PREVENT FURTHER PROGRESS. IN THIS CASE STP ONLY
!     SATISFIES THE SUFFICIENT DECREASE CONDITION.
!
!     THE SUBROUTINE STATEMENT IS
!
!        SUBROUTINE MCSRCH(N,X,F,G,S,STP,FTOL,XTOL, MAXFEV,INFO,NFEV,WA)
!     WHERE
!
!       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
!         OF VARIABLES.
!
!       X IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!         BASE POINT FOR THE LINE SEARCH. ON OUTPUT IT CONTAINS
!         X + STP*S.
!
!       F IS A VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F
!         AT X. ON OUTPUT IT CONTAINS THE VALUE OF F AT X + STP*S.
!
!       G IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!         GRADIENT OF F AT X. ON OUTPUT IT CONTAINS THE GRADIENT
!         OF F AT X + STP*S.
!
!       S IS AN INPUT ARRAY OF LENGTH N WHICH SPECIFIES THE
!         SEARCH DIRECTION.
!
!       STP IS A NONNEGATIVE VARIABLE. ON INPUT STP CONTAINS AN
!         INITIAL ESTIMATE OF A SATISFACTORY STEP. ON OUTPUT
!         STP CONTAINS THE FINAL ESTIMATE.
!
!       FTOL AND GTOL ARE NONNEGATIVE INPUT VARIABLES. (In this reverse
!         communication implementation GTOL is defined in a COMMON
!         statement.) TERMINATION OCCURS WHEN THE SUFFICIENT DECREASE
!         CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
!        SATISFIED.
!
!       XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS
!         WHEN THE RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
!         IS AT MOST XTOL.
!
!       STPMIN AND STPMAX ARE NONNEGATIVE INPUT VARIABLES WHICH
!         SPECIFY LOWER AND UPPER BOUNDS FOR THE STEP. (In this reverse
!         communication implementatin they are defined in a COMMON
!         statement).
!
!       MAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION
!         OCCURS WHEN THE NUMBER OF CALLS TO FCN IS AT LEAST
!         MAXFEV BY THE END OF AN ITERATION.
!
!       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!
!         INFO = 0  IMPROPER INPUT PARAMETERS.
!
!         INFO =-1  A RETURN IS MADE TO COMPUTE THE FUNCTION AND GRADIENT.
!
!         INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
!                   DIRECTIONAL DERIVATIVE CONDITION HOLD.
!
!                   IS AT MOST XTOL.
!
!         INFO = 3  NUMBER OF CALLS TO FCN HAS REACHED MAXFEV.
!
!         INFO = 4  THE STEP IS AT THE LOWER BOUND STPMIN.
!
!         INFO = 5  THE STEP IS AT THE UPPER BOUND STPMAX.
!
!         INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
!                   THERE MAY NOT BE A STEP WHICH SATISFIES THE
!                   SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
!                   TOLERANCES MAY BE TOO SMALL.
!
!       NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF
!         CALLS TO FCN.
!
!       WA IS A WORK ARRAY OF LENGTH N.
!
!     SUBPROGRAMS CALLED
!
!       MCSTEP
!
!       FORTRAN-SUPPLIED...ABS,MAX,MIN
!
!     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!    JORGE J. MORE', DAVID J. THUENTE
!
!     **********
      INTEGER INFOC,J
      LOGICAL BRACKT,STAGE1
      DOUBLE PRECISION DG,DGM,DGINIT,DGTEST,DGX,DGXM,DGY,DGYM,&
      &FINIT,FTEST1,FM,FX,FXM,FY,FYM,P5,P66,STX,STY,&
       &STMIN,STMAX,WIDTH,WIDTH1,XTRAPF,ZERO
      DATA P5,P66,XTRAPF,ZERO /0.5D0,0.66D0,4.0D0,0.0D0/
      IF(INFO.EQ.-1) GO TO 45 
      INFOC = 1
!
!     CHECK THE INPUT PARAMETERS FOR ERRORS.
!
      IF (N .LE. 0 .OR. STP .LE. ZERO .OR. FTOL .LT. ZERO .OR.&
         &GTOL .LT. ZERO .OR. XTOL .LT. ZERO .OR. STPMIN .LT. ZERO&
     &.OR. STPMAX .LT. STPMIN .OR. MAXFEV .LE. 0) RETURN
!
!     COMPUTE THE INITIAL GRADIENT IN THE SEARCH DIRECTION
!     AND CHECK THAT S IS A DESCENT DIRECTION.
!
      DGINIT = ZERO
      DO 10 J = 1, N
         DGINIT = DGINIT + G(J)*S(J)
   10    CONTINUE
      IF (DGINIT .GE. ZERO) then
         write(LP,15)
   15    FORMAT(/'  THE SEARCH DIRECTION IS NOT A DESCENT DIRECTION')
         RETURN
         ENDIF
!
!     INITIALIZE LOCAL VARIABLES.
!
      BRACKT = .FALSE.
      STAGE1 = .TRUE.
      NFEV = 0
      FINIT = F
      DGTEST = FTOL*DGINIT
      WIDTH = STPMAX - STPMIN
      WIDTH1 = WIDTH/P5
      DO 20 J = 1, N
         WA(J) = X(J)
   20    CONTINUE
!
!     THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
!     FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
!     THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
!     FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
!     THE INTERVAL OF UNCERTAINTY.
!     THE VARIABLES STP, F, DG CONTAIN THE VALUES OF THE STEP,
!     FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
!
      STX = ZERO
      FX = FINIT
      DGX = DGINIT
      STY = ZERO
      FY = FINIT
      DGY = DGINIT
!
!     START OF ITERATION.
!
   30 CONTINUE
!
!        SET THE MINIMUM AND MAXIMUM STEPS TO CORRESPOND
!        TO THE PRESENT INTERVAL OF UNCERTAINTY.
!
         IF (BRACKT) THEN
            STMIN = MIN(STX,STY)
            STMAX = MAX(STX,STY)
         ELSE
            STMIN = STX
            STMAX = STP + XTRAPF*(STP - STX)
            END IF
!
!        FORCE THE STEP TO BE WITHIN THE BOUNDS STPMAX AND STPMIN.
!
         STP = MAX(STP,STPMIN)
         STP = MIN(STP,STPMAX)
!
!        IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
!        STP BE THE LOWEST POINT OBTAINED SO FAR.
!
         IF ((BRACKT .AND. (STP .LE. STMIN .OR. STP .GE. STMAX))&
           &.OR. NFEV .GE. MAXFEV-1 .OR. INFOC .EQ. 0&
           &.OR. (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX)) THEN
            STP = STX
            ENDIF
!
!        EVALUATE THE FUNCTION AND GRADIENT AT STP
!        AND COMPUTE THE DIRECTIONAL DERIVATIVE.
!        We return to main program to obtain F and G.
!
         DO 40 J = 1, N
            X(J) = WA(J) + STP*S(J)
   40       CONTINUE
         INFO=-1
         RETURN

   45    INFO=0
         NFEV = NFEV + 1
         DG = ZERO
         DO 50 J = 1, N
            DG = DG + G(J)*S(J)
   50       CONTINUE
         FTEST1 = FINIT + STP*DGTEST
!
!       TEST FOR CONVERGENCE.
!
         IF ((BRACKT .AND. (STP .LE. STMIN .OR. STP .GE. STMAX))&
          &.OR. INFOC .EQ. 0) INFO = 6
         IF (STP .EQ. STPMAX .AND.&
            &F .LE. FTEST1 .AND. DG .LE. DGTEST) INFO = 5
         IF (STP .EQ. STPMIN .AND.&
          &(F .GT. FTEST1 .OR. DG .GE. DGTEST)) INFO = 4
         IF (NFEV .GE. MAXFEV) INFO = 3
         IF (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX) INFO = 2
         IF (F .LE. FTEST1 .AND. ABS(DG) .LE. GTOL*(-DGINIT)) INFO = 1
!
!        CHECK FOR TERMINATION.
!
         IF (INFO .NE. 0) RETURN
!
!        IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
!        FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
!
         IF (STAGE1 .AND. F .LE. FTEST1 .AND.&
          &DG .GE. MIN(FTOL,GTOL)*DGINIT) STAGE1 = .FALSE.
!
!        A MODIFIED FUNCTION IS USED TO PREDICT THE STEP ONLY IF
!        WE HAVE NOT OBTAINED A STEP FOR WHICH THE MODIFIED
!        FUNCTION HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE
!        DERIVATIVE, AND IF A LOWER FUNCTION VALUE HAS BEEN
!        OBTAINED BUT THE DECREASE IS NOT SUFFICIENT.
!
         IF (STAGE1 .AND. F .LE. FX .AND. F .GT. FTEST1) THEN
!
!           DEFINE THE MODIFIED FUNCTION AND DERIVATIVE VALUES.
!
            FM = F - STP*DGTEST
            FXM = FX - STX*DGTEST
            FYM = FY - STY*DGTEST
            DGM = DG - DGTEST
            DGXM = DGX - DGTEST
            DGYM = DGY - DGTEST
!
!           CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
!           AND TO COMPUTE THE NEW STEP.
!
            CALL MCSTEP(STX,FXM,DGXM,STY,FYM,DGYM,STP,FM,DGM,&
              &BRACKT,STMIN,STMAX,INFOC)
!
!           RESET THE FUNCTION AND GRADIENT VALUES FOR F.
!
            FX = FXM + STX*DGTEST
            FY = FYM + STY*DGTEST
            DGX = DGXM + DGTEST
            DGY = DGYM + DGTEST
         ELSE
!
!           CALL MCSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
!           AND TO COMPUTE THE NEW STEP.
!
            CALL MCSTEP(STX,FX,DGX,STY,FY,DGY,STP,F,DG,BRACKT,STMIN,STMAX,INFOC)
            END IF
!
!        FORCE A SUFFICIENT DECREASE IN THE SIZE OF THE
!        INTERVAL OF UNCERTAINTY.
!
         IF (BRACKT) THEN
            IF (ABS(STY-STX) .GE. P66*WIDTH1) THEN
               STP = STX + P5*(STY - STX)
              ENDIF
            WIDTH1 = WIDTH
            WIDTH = ABS(STY-STX)
            END IF
!
!        END OF ITERATION.
!
         GO TO 30

      END SUBROUTINE

      subroutine daxpy(n,da,dx,incx,dy,incy)
            USE NORMA0
           IMPLICIT NONE
!
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),da
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end subroutine
    SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,STP,FP,DP,BRACKT,STPMIN,STPMAX,INFO)
          USE NORMA0
          IMPLICIT NONE
      INTEGER INFO
      DOUBLE PRECISION STX,FX,DX,STY,FY,DY,STP,FP,DP,STPMIN,STPMAX
      LOGICAL BRACKT,BOUND
!
!     SUBROUTINE MCSTEP
!
!     THE PURPOSE OF MCSTEP IS TO COMPUTE A SAFEGUARDED STEP FOR
!     A LINESEARCH AND TO UPDATE AN INTERVAL OF UNCERTAINTY FOR
!     A MINIMIZER OF THE FUNCTION.
!
!     THE PARAMETER STX CONTAINS THE STEP WITH THE LEAST FUNCTION
!     VALUE. THE PARAMETER STP CONTAINS THE CURRENT STEP. IT IS
!     ASSUMED THAT THE DERIVATIVE AT STX IS NEGATIVE IN THE
!     DIRECTION OF THE STEP. IF BRACKT IS SET TRUE THEN A
!     MINIMIZER HAS BEEN BRACKETED IN AN INTERVAL OF UNCERTAINTY
!     WITH ENDPOINTS STX AND STY.
!
!     THE SUBROUTINE STATEMENT IS
!
!       SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,STP,FP,DP,BRACKT,
!                        STPMIN,STPMAX,INFO)
!
!     WHERE
!
!       STX, FX, AND DX ARE VARIABLES WHICH SPECIFY THE STEP,
!         THE FUNCTION, AND THE DERIVATIVE AT THE BEST STEP OBTAINED
!         SO FAR. THE DERIVATIVE MUST BE NEGATIVE IN THE DIRECTION
!         OF THE STEP, THAT IS, DX AND STP-STX MUST HAVE OPPOSITE
!         SIGNS. ON OUTPUT THESE PARAMETERS ARE UPDATED APPROPRIATELY.
!
!       STY, FY, AND DY ARE VARIABLES WHICH SPECIFY THE STEP,
!         THE FUNCTION, AND THE DERIVATIVE AT THE OTHER ENDPOINT OF
!         THE INTERVAL OF UNCERTAINTY. ON OUTPUT THESE PARAMETERS ARE
!         UPDATED APPROPRIATELY.
!
!       STP, FP, AND DP ARE VARIABLES WHICH SPECIFY THE STEP,
!         THE FUNCTION, AND THE DERIVATIVE AT THE CURRENT STEP.
!         IF BRACKT IS SET TRUE THEN ON INPUT STP MUST BE
!         BETWEEN STX AND STY. ON OUTPUT STP IS SET TO THE NEW STEP.
!
!       BRACKT IS A LOGICAL VARIABLE WHICH SPECIFIES IF A MINIMIZER
!         HAS BEEN BRACKETED. IF THE MINIMIZER HAS NOT BEEN BRACKETED
!         THEN ON INPUT BRACKT MUST BE SET FALSE. IF THE MINIMIZER
!         IS BRACKETED THEN ON OUTPUT BRACKT IS SET TRUE.
!
!       STPMIN AND STPMAX ARE INPUT VARIABLES WHICH SPECIFY LOWER
!         AND UPPER BOUNDS FOR THE STEP.
!
!       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!         IF INFO = 1,2,3,4,5, THEN THE STEP HAS BEEN COMPUTED
!         ACCORDING TO ONE OF THE FIVE CASES BELOW. OTHERWISE
!         INFO = 0, AND THIS INDICATES IMPROPER INPUT PARAMETERS.
!
!     SUBPROGRAMS CALLED
!
!       FORTRAN-SUPPLIED ... ABS,MAX,MIN,SQRT
!
!     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!     JORGE J. MORE', DAVID J. THUENTE
!
      DOUBLE PRECISION GAMMA,P,Q,R,S,SGND,STPC,STPF,STPQ,THETA
      INFO = 0
!
!     CHECK THE INPUT PARAMETERS FOR ERRORS.
!
      IF ((BRACKT .AND. (STP .LE. MIN(STX,STY) .OR.&
     &    STP .GE. MAX(STX,STY))) .OR.&
     &    DX*(STP-STX) .GE. 0.0 .OR. STPMAX .LT. STPMIN) RETURN
!
!     DETERMINE IF THE DERIVATIVES HAVE OPPOSITE SIGN.
!
      SGND = DP*(DX/ABS(DX))
!
!     FIRST CASE. A HIGHER FUNCTION VALUE.
!     THE MINIMUM IS BRACKETED. IF THE CUBIC STEP IS CLOSER
!     TO STX THAN THE QUADRATIC STEP, THE CUBIC STEP IS TAKEN,
!     ELSE THE AVERAGE OF THE CUBIC AND QUADRATIC STEPS IS TAKEN.
!
      IF (FP .GT. FX) THEN
         INFO = 1
         BOUND = .TRUE.
         THETA = 3*(FX - FP)/(STP - STX) + DX + DP
         S = MAX(ABS(THETA),ABS(DX),ABS(DP))
         GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DP/S))
         IF (STP .LT. STX) GAMMA = -GAMMA
         P = (GAMMA - DX) + THETA
         Q = ((GAMMA - DX) + GAMMA) + DP
         R = P/Q
         STPC = STX + R*(STP - STX)
         STPQ = STX + ((DX/((FX-FP)/(STP-STX)+DX))/2)*(STP - STX)
         IF (ABS(STPC-STX) .LT. ABS(STPQ-STX)) THEN
            STPF = STPC
         ELSE
           STPF = STPC + (STPQ - STPC)/2
           END IF
         BRACKT = .TRUE.
!
!     SECOND CASE. A LOWER FUNCTION VALUE AND DERIVATIVES OF
!     OPPOSITE SIGN. THE MINIMUM IS BRACKETED. IF THE CUBIC
!     STEP IS CLOSER TO STX THAN THE QUADRATIC (SECANT) STEP,
!     THE CUBIC STEP IS TAKEN, ELSE THE QUADRATIC STEP IS TAKEN.
!
      ELSE IF (SGND .LT. 0.0) THEN
         INFO = 2
         BOUND = .FALSE.
         THETA = 3*(FX - FP)/(STP - STX) + DX + DP
         S = MAX(ABS(THETA),ABS(DX),ABS(DP))
         GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DP/S))
         IF (STP .GT. STX) GAMMA = -GAMMA
         P = (GAMMA - DP) + THETA
         Q = ((GAMMA - DP) + GAMMA) + DX
         R = P/Q
         STPC = STP + R*(STX - STP)
         STPQ = STP + (DP/(DP-DX))*(STX - STP)
         IF (ABS(STPC-STP) .GT. ABS(STPQ-STP)) THEN
            STPF = STPC
         ELSE
            STPF = STPQ
            END IF
         BRACKT = .TRUE.
!
!     THIRD CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
!     SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DECREASES.
!     THE CUBIC STEP IS ONLY USED IF THE CUBIC TENDS TO INFINITY
!     IN THE DIRECTION OF THE STEP OR IF THE MINIMUM OF THE CUBIC
!     IS BEYOND STP. OTHERWISE THE CUBIC STEP IS DEFINED TO BE
!     EITHER STPMIN OR STPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
!     COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
!     CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
!
      ELSE IF (ABS(DP) .LT. ABS(DX)) THEN
         INFO = 3
         BOUND = .TRUE.
         THETA = 3*(FX - FP)/(STP - STX) + DX + DP
         S = MAX(ABS(THETA),ABS(DX),ABS(DP))
!
!        THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
!        TO INFINITY IN THE DIRECTION OF THE STEP.
!
         GAMMA = S*SQRT(MAX(0.0D0,(THETA/S)**2 - (DX/S)*(DP/S)))
         IF (STP .GT. STX) GAMMA = -GAMMA
         P = (GAMMA - DP) + THETA
         Q = (GAMMA + (DX - DP)) + GAMMA
         R = P/Q
         IF (R .LT. 0.0 .AND. GAMMA .NE. 0.0) THEN
            STPC = STP + R*(STX - STP)
         ELSE IF (STP .GT. STX) THEN
            STPC = STPMAX
         ELSE
            STPC = STPMIN
            END IF
         STPQ = STP + (DP/(DP-DX))*(STX - STP)
         IF (BRACKT) THEN
            IF (ABS(STP-STPC) .LT. ABS(STP-STPQ)) THEN
               STPF = STPC
            ELSE
               STPF = STPQ
               END IF
         ELSE
            IF (ABS(STP-STPC) .GT. ABS(STP-STPQ)) THEN
               STPF = STPC
            ELSE
               STPF = STPQ
               END IF
            END IF
!
!     FOURTH CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
!     SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DOES
!     NOT DECREASE. IF THE MINIMUM IS NOT BRACKETED, THE STEP
!     IS EITHER STPMIN OR STPMAX, ELSE THE CUBIC STEP IS TAKEN.
!
      ELSE
         INFO = 4
         BOUND = .FALSE.
         IF (BRACKT) THEN
            THETA = 3*(FP - FY)/(STY - STP) + DY + DP
            S = MAX(ABS(THETA),ABS(DY),ABS(DP))
            GAMMA = S*SQRT((THETA/S)**2 - (DY/S)*(DP/S))
            IF (STP .GT. STY) GAMMA = -GAMMA
            P = (GAMMA - DP) + THETA
            Q = ((GAMMA - DP) + GAMMA) + DY
            R = P/Q
            STPC = STP + R*(STY - STP)
            STPF = STPC
         ELSE IF (STP .GT. STX) THEN
            STPF = STPMAX
         ELSE
            STPF = STPMIN
            END IF
         END IF
!
!     UPDATE THE INTERVAL OF UNCERTAINTY. THIS UPDATE DOES NOT
!     DEPEND ON THE NEW STEP OR THE CASE ANALYSIS ABOVE.
!
      IF (FP .GT. FX) THEN
         STY = STP
         FY = FP
         DY = DP
      ELSE
         IF (SGND .LT. 0.0) THEN
            STY = STX
            FY = FX
            DY = DX
            END IF
         STX = STP
         FX = FP
         DX = DP
         END IF
!
!     COMPUTE THE NEW STEP AND SAFEGUARD IT.
!
      STPF = MIN(STPMAX,STPF)
      STPF = MAX(STPMIN,STPF)
      STP = STPF
      IF (BRACKT .AND. BOUND) THEN
         IF (STY .GT. STX) THEN
            STP = MIN(STX+0.66*(STY-STX),STP)
         ELSE
            STP = MAX(STX+0.66*(STY-STX),STP)
            END IF
         END IF
      RETURN
!
!     LAST LINE OF SUBROUTINE MCSTEP.
!
      END subroutine


end module
