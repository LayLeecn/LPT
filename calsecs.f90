subroutine calsecs(date,secs)
    ! calculate seconds of the given date from 1900-01-01-00:00:00
    ! date -> YYYYMMDDHHMMSS
    
    implicit none
    
    character(len=14),intent(in) :: date
    integer(kind=8),intent(out) :: secs ! be careful !!!! the length of secs must at least be 8 Byte !!!!!
    
    integer :: years,months,days,hours,minutes,seconds
    integer :: year
    logical :: yearstatus
    !                                     Jan.    Feb.    March   April   May     June
    !                                     July    Aug,    Sep.    Oct.    Nov.    Dec.
    integer,dimension(12) :: leapmonsecs = (/2678400,2505600,2678400,2592000,2678400,2592000,&
    2678400,2678400,2592000,2678400,2592000,2678400/)
    integer,dimension(12) :: monsecs     = (/2678400,2419200,2678400,2592000,2678400,2592000,&
    2678400,2678400,2592000,2678400,2592000,2678400/)
    
    read(date(1:4),'(I4)')  years
    read(date(5:6),'(I2)')  months 
    read(date(7:8),'(I2)')  days
    read(date(9:10),'(I2)') hours
    read(date(11:12),'(I2)')minutes
    read(date(13:14),'(I2)')seconds
    
    secs = 0
    ! check how many years passed through 1900 to the given date.
    if(years.GT.1900)then
        do year = 1900,years-1,1
            call isleapyear(year,yearstatus)
            if(yearstatus .EQV. .true.) then
                secs = secs + 31622400
            else
                secs = secs + 31536000
            end if
        end do
    else
        continue
    end if
    
    ! check how many years passed through the first day of the given date to the date.
    if (months.EQ.1)then
        continue
    else
        call isleapyear(years,yearstatus)
        if(yearstatus .EQV. .TRUE.) then
            secs = secs + sum(leapmonsecs(1:months-1))
        else
            secs = secs + sum(monsecs(1:months-1))
        end if
    end if
    
    ! check how many days passed throug the beginning of the date day 
    secs = secs + 86400 * (days-1)
    
    ! check how many hours passed
    secs = secs + 3600 * hours ! hour begin from 0
    
    ! chech how many minutes passed
    secs = secs + 60 * minutes ! minute begin from 0
    
    ! add seconds
    secs = secs + seconds
    
    end subroutine calsecs