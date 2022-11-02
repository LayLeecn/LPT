subroutine core
    ! main subroutine of the LPT
    ! allocate, push, record particles
    ! kill them if neccesary

    ! two directions FORWARD and BACKWARD
    ! the only difference is the time step

    use variables
    use inputs

    implicit none

    integer :: idate

    if(direction=="forward")then 
        do idate=1,filenum-1
            ! 1.
            ! check the sim begin? running? stopped?
            call calsecs(datelist(idate),secl)
            call calsecs(datelist(idate+1),secr)

            if(secr<secbtsim)then
                cycle ! simulation not begin
            end if            
            if(secl>secetsim)then
                cycle ! simulation have ended
            end if

            ! if running
            ! 2.
            ! reading data
            print *,"Read meteorology files needed."
            call readmeteovars(idate,idate+2)

            ! 3.
            ! distribute dynamic parameters
            

            ! 4. 
            ! push particles until the particle time is out of the local time domain

            






        end do

    end if

    if(direction=="backward")then
    end if



end subroutine core