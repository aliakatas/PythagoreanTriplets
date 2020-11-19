!  pythagore_triplets.f90 
!
!  FUNCTIONS:
!  pythagore_triplets - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: pythagore_triplets
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program pythagore_triplets

    implicit none

    ! Variables
    integer             :: narg
    character(256)      :: arg
    integer             :: perimeter
    integer             :: a, b, c
    real                :: abound, bbound
    logical             :: foundit

    ! Body of pythagore_triplets
    narg = command_argument_count()
    if (narg < 1) then 
        write(*,*) 'ERROR: Need constraint as input!'
        write(*,*) 'Please try again...'
        call exit_proc()
    end if
        
    call get_command_argument(1, arg)
    read(arg,*) perimeter
    
    abound = (perimeter - 3.) / 3.
    foundit = .false.
    
    write(*,*) 'Generating a Pythagorean triplet (a, b, c)...'
    write(*,'(A,I0,A)') 'Using perimeter = ', perimeter, ' as constraint...'
    write(*,'(A,F10.3)') 'a should be less than or equal to ', abound
    
    a = 1
    outerLoop: do while (a <= floor(abound))
        bbound = (perimeter - a) / 2.
        do b = a + 1, floor(bbound)
            c = perimeter - a - b
            if (a * a + b * b == c * c) then
                foundit = .true.
                exit outerLoop
            end if
        end do
        a = a + 1
    end do outerLoop
    
    if (foundit) then
        write(*,'(A,I0,A,I0,A,I0,A)') 'The triplet is (a, b, c) = (', a, ', ', b, ', ', c, ')'
    else
        write(*,*) 'Could not find triplet with this constraint!'
    end if
    
    call exit_proc()
    
    contains
    
    subroutine exit_proc()
    write(*,*) 'Press Enter to terminate'
    read(*,*)
    stop
    end subroutine
    end program pythagore_triplets

