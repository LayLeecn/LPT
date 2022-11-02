subroutine allocateparticles
    ! allocate particles and distribute static parameters of the particles
    ! the static parameters are weight,status,reltime,lon,lat,agl   only 6
    use variables
    use inputs

    implicit none

    integer :: ip
    real :: rn

    if(allocated(particles))then
        print *,"Fatal error! Particles are allocated before initialsed."
        stop
    end if

    allocate(particles(particlenumber))

    do ip=1,particlenumber
        particles(ip)%weight = 1.0 ! no loss at the beginning
        particles(ip)%status = -1  ! dynamic parameters are not allocated 

        call random_seed()
        call random_number(rn)
        particles(ip)%reltime = secbtrel + int(float(secetrel-secbtrel)*rn)
        particles(ip)%time = secbtrel + int(float(secetrel-secbtrel)*rn)
        ! at the very beginning, the particle time is the released time

        call random_number(rn)
        particles(ip)%lon = slonw + (slone-slonw)*rn

        call random_number(rn)
        particles(ip)%lat = slats + (slatn-slats)*rn

        call random_number(rn)
        particles(ip)%agl = saglb + (saglt-saglb)*rn

    end do    

    print *,"Memory distributed for all particles."
    
end subroutine allocateparticles