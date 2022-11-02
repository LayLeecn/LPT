subroutine isleapyear(year, flag)
    ! check whether the input year is a leap year
    implicit none
    
    integer,intent(in)  :: year
    logical,intent(out) :: flag
    
    integer :: mod4,mod100,mod400
    
    mod400 = mod(year,400)
    mod100 = mod(year,100)
    mod4   = mod(year,4)
    
    if (mod4 .NE. 0) then       ! if the year cannot be divided by 4 -> not leap year
        flag = .false.
    elseif (mod400 .EQ. 0) then ! if the year can be divided by 400 -> leap year
        flag = .true.
    elseif (mod100 .EQ. 0) then ! if the year can be divided by 100 but 400 -> not leap year
        flag = .false.
    else                    ! else -> leap year
        flag = .true.
    end if
    
    end subroutine isleapyear