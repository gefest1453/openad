c$openad XXX Template ad_template.f
	subroutine head(x,y) 
	  double precision, dimension(3) :: x
	  double precision, dimension(1) :: y
c$openad INDEPENDENT(x)
          y=sum(x)
c$openad DEPENDENT(y)
	end subroutine
