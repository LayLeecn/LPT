subroutine readinputs
    use inputs
    implicit none

    character(len=256) :: line
    character(len=1) :: head
    integer :: error

    open(unit=20,file="input",status='old',action="read")
    do while(.true.)
        read(20,"(A)",iostat=error) line
        if(error/=0)exit
        head = line(1:1)
        if(head/='&')then
            call setinput(line)
        end if
    end do
    close(20)


    ! post process of inputs
    call calsecs(btsim,secbtsim)
    call calsecs(etsim,secetsim)
    call calsecs(btrel,secbtrel)
    call calsecs(etrel,secetrel)

    call readdatefile

    end subroutine readinputs