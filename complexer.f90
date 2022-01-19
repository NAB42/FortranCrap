program complexer
	!Trying to screw around with random crap
	implicit none
	integer, dimension(10) :: array
	integer :: num1
	integer :: i
	
	print *, 'Number: '
	read(*,*) num1 
	
	do i=1,10
		array(i) = num1*i 
	end do
	do i=1,10
		print *, array(i)
	end do
	
	call test(num1)
	
	
	
	contains
	
subroutine test(a)
	integer, intent(in) :: a 
	integer :: b
	b = a**a
	print *, b
	
end subroutine
	
end program