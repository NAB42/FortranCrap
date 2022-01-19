program hello
	implicit none
	real :: r
	real :: pi
	real :: area
	real :: cir
	
	pi = 3.141592653589
	
	print *, 'Radius: '
	read(*,*) r
	
	area = pi * r**2.0
	cir = 2.0*r*pi
	
	print *, 'Area: ',area
	print *, 'Circumference: ',cir
	
end program hello