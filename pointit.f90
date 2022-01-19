! POINTIT PROGRAM -------------------
! AUTHOR :: Nate Boulton
! VERSION :: 1.0
! 
! Asks for number of coordinates, and 
! then inquires for what each POINT is,
! and then asks for 2 of the points and 
! calculates the distance between them.
!
!------------------------------------
program pointit
	implicit none
	! Creates the POINT type
	type :: point
		integer :: x
		integer :: y
	end type
	! Variable Declarations
	type(point), allocatable :: coords(:)
	integer :: i
	
	integer :: temp_x
	integer :: temp_y
	integer :: num 
	integer :: i2
	! INTRO
	print *, '~~Welcome to POINTIT~~~'
	print *, 'You will input a number to determine number of '
	print *, 'coordinates, then you enter each one and choose 2 '
	print *, 'of them. Then this program calculates the distance '
	print *, 'between each. Enjoy!'
	print *, ' '
	! Asks for number of coordinates to input into the coords ARRAY
	print *, 'Number of coordinates?'
	read(*,*) num
	if((num > 999).or.(num < 2)) then ! If response isn't in range, pick 10
		print *, 'Not gonna work. You now have 10 coordinates waiting to be used.'
		num = 10
	end if 
	allocate(coords(num))
	
	! Asks for each value *2
	do i = 1, num
		print '(a4,i3)', 'Set ',i
		print *, 'X value: '
		read(*,*) temp_x 
		print *, 'Y value: '
		read(*,*) temp_y 
		
		coords(i)%x = temp_x 
		coords(i)%y = temp_y
	end do
	i = num ** 2 ! Any number over num works
	
	! Asks for a number, than prints the coordinate 
	! associated with it from coords. Does this twice.
	
	first_loop: do while((i > num))
		print '(a29,i3,a1)', 'Enter a number between 1 and ',num,':'
		read(*,*) i 
		if(i > num) then
			print *, 'Too big! Try again'
		else if(i < 1) then
			print *, 'Too small! Try again'
		else
			exit
		end if
	end do first_loop
	
	second_loop: do while((i2 > num))
		print '(a29,i3,a1)', 'Enter another number between 1 and ',num,':'
		read(*,*) i2 
		if(i2 > num) then
			print *, 'Too big! Try again'
		else if(i2 < 1) then
			print *, 'Too small! Try again'
		else
			exit
		end if
	end do second_loop 
	
	! Calls the to_string() method
	print *, 'First coordinate: '
	call to_string(coords(i))
	print *, 'Second coordinate: '
	call to_string(coords(i2))
	
	call distance(coords(i),coords(i2))
	
	contains
	
	! SUBROUTINE to_string(self) takes a POINT 
	! type and prints out a string consisting 
	! of (x,y)
	subroutine to_string(self)
		class(point), intent(in) :: self 
		character :: cx
		character :: cy
		! Checks length of self%x and self%y 
		if(self%x <100) then 
			if(self%x < 10) then 
				cx = '1'
			else
				cx = '2'
			end if
		else
			cx = '3'
		end if
		if(self%y <100) then 
			if(self%y < 10) then 
				cy = '1'
			else
				cy = '2'
			end if
		else
			cy = '3'
		end if
		! Prints coordinate as (x,y)
		print '(a1,i'//cx//',a1,i'//cy//',a1)', "(",self%x,",",self%y,")"
	end subroutine
	
	! Calculates the distance as the crow flies from 
	! first point to second point.
	subroutine distance(coor1,coor2)
		class(point), intent(in) :: coor1 
		class(point), intent(in) :: coor2 
		integer :: distx
		integer :: disty
		real :: dist
		real :: distf
		distx = coor2%x - coor1%x
		disty = coor2%y - coor1%y
		dist = real(distx**2 + disty**2)
		distf = sqrt(dist)
		print *, 'The distance is ', distf
	end subroutine
end program 