subroutine readmeteovars(idatestart,idatestop)
    ! read meteorology file 
    ! process the data 
    ! reshape them
    use variables
    use constants
    use inputs
    
    implicit none

    integer,intent(in) :: idatestart,idatestop

    integer :: inl,idate,idatefile,indatenum
    ! inl     i of nest level
    integer :: ilon,ilat,ilev
    integer(kind=8) :: secs
    character(len=1) :: strinl
    character(len=256) :: fullpath
    real :: t2m_,d2m_,sp_,q0_,g0_,z0_,lat_,lon_,&
            asl_,z_,p_,t_,q_

    ! GUESS HOW MANY FILES WILL BE READ IN THE SUBROUTINE
    indatenum = abs(idatestart-idatestop)+1    

    ! FIRST OF ALL
    ! DEALLOCATE ARRAYS IF ALLOCATED
    if(allocated(lonfirst))then
        deallocate(timecache)
        deallocate(longitude,latitude,level)
        deallocate(lonfirst,lonlast,dlon,lonnum,&
                  &latfirst,latlast,dlat,latnum,levnum)
        deallocate(u10,v10,t2m,blh,fsr,zust,z0,sp,&
                  &sshf,d2m,lsm,cbh,crr,lsrr,&
                  &asl0,rho0,q0)
        deallocate(z,q,u,v,w,t,&
                  &asl,agl,rho)
        deallocate(pxyz)
        deallocate(pxyz0)
    end if
    ! ALLOCATE PEACEFULLY
    allocate(lonfirst(nestlevel+1,indatenum),  &
            &lonlast(nestlevel+1,indatenum),   &
            &dlon(nestlevel+1,indatenum),      &
            &lonnum(nestlevel+1,indatenum),    &
            &latfirst(nestlevel+1,indatenum),  &
            &latlast(nestlevel+1,indatenum),   &
            &dlat(nestlevel+1,indatenum),      &
            &latnum(nestlevel+1,indatenum),    &
            &levnum(nestlevel+1,indatenum)     )
    allocate(longitude(nestlevel+1,indatenum,maxlonnum),  &
            &latitude(nestlevel+1,indatenum,maxlatnum),   &
            &level(nestlevel+1,indatenum,maxznum))
    allocate(u10(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &v10(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &t2m(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &blh(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &fsr(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &zust(nestlevel+1,indatenum,maxlonnum,maxlatnum),  &
            &z0(nestlevel+1,indatenum,maxlonnum,maxlatnum),    &
            &sp(nestlevel+1,indatenum,maxlonnum,maxlatnum),    &
            &sshf(nestlevel+1,indatenum,maxlonnum,maxlatnum),  &
            &d2m(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &lsm(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &cbh(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &crr(nestlevel+1,indatenum,maxlonnum,maxlatnum),   &
            &lsrr(nestlevel+1,indatenum,maxlonnum,maxlatnum),  &
            &asl0(nestlevel+1,indatenum,maxlonnum,maxlatnum),  &
            &rho0(nestlevel+1,indatenum,maxlonnum,maxlatnum),  &
            &q0(nestlevel+1,indatenum,maxlonnum,maxlatnum)     )
    allocate(z(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum),   &
            &q(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum),   &
            &u(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum),   &
            &v(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum),   &
            &w(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum),   &
            &t(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum),   &
            &asl(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum), &
            &agl(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum), &
            &rho(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum)  )
    allocate(timecache(indatenum))
    allocate(pxyz0(nestlevel+1,indatenum,maxlonnum,maxlatnum))
    allocate(pxyz(nestlevel+1,indatenum,maxlonnum,maxlatnum,maxznum))

    ! read files
    do idatefile=idatestart,idatestop
        call calsecs(datelist(idatefile),secs)
        idate = idatefile - idatestart + 1
        timecache(idate) = secs
    do inl=0,nestlevel
        ! read the origin meteorology data
        write(strinl,'(I1)') inl
        fullpath=trim(adjustl(meteofilepath))//strinl//"."//trim(adjustl(filelist(idatefile)))     
        call readfield(fullpath,md)

        lonnum(inl+1,idate)   = size(md%longitude)
        lonfirst(inl+1,idate) = md%longitude(1)
        lonlast(inl+1,idate)  = md%longitude(lonnum(inl+1,idate))
        dlon(inl+1,idate)     = md%longitude(2)-md%longitude(1)

        latnum(inl+1,idate)   = size(md%latitude)
        latfirst(inl+1,idate) = md%latitude(1)
        latlast(inl+1,idate)  = md%latitude(latnum(inl+1,idate))
        dlat(inl+1,idate)     = md%latitude(2)-md%latitude(1)

        levnum(inl+1,idate)  = size(md%level)

        ! read dimension 
        longitude(inl+1,idate,:) = md%longitude
        latitude(inl+1,idate,:)  = md%latitude
        level(inl+1,idate,:)     = md%level

        ! read ground variables
        z0(inl+1,idate,:,:) = md%z0
        sp(inl+1,idate,:,:) = md%sp
        u10(inl+1,idate,:,:) = md%u10        
        v10(inl+1,idate,:,:) = md%v10
        t2m(inl+1,idate,:,:) = md%t2m
        blh(inl+1,idate,:,:) = md%blh
        fsr(inl+1,idate,:,:) = md%fsr
        d2m(inl+1,idate,:,:) = md%d2m
        lsm(inl+1,idate,:,:) = md%lsm
        cbh(inl+1,idate,:,:) = md%cbh
        crr(inl+1,idate,:,:) = md%crr
        zust(inl+1,idate,:,:) = md%zust
        sshf(inl+1,idate,:,:) = md%sshf
        lsrr(inl+1,idate,:,:) = md%lsrr

        ! read pressure levels variables
        z(inl+1,idate,:,:,:) = md%z
        q(inl+1,idate,:,:,:) = md%q
        u(inl+1,idate,:,:,:) = md%u
        v(inl+1,idate,:,:,:) = md%v
        w(inl+1,idate,:,:,:) = md%w
        t(inl+1,idate,:,:,:) = md%t

        ! calculate ground height above sea level - asl0
        !           air density at ground         - rho0
        !           specific humidty              - q0
        do ilon = 1,lonnum(inl+1,idate)
            lon_ = lonfirst(inl+1,idate) + dlon(inl+1,idate)*(ilon-1)
        do ilat = 1,latnum(inl+1,idate)
            lat_ = latfirst(inl+1,idate) + dlat(inl+1,idate)*(ilat-1)
            call calga(0.,lat_,g0_)
            d2m_ = d2m(inl+1,idate,ilon,ilat)
            t2m_ = t2m(inl+1,idate,ilon,ilat)
            q0_ = exp(17.65*((d2m_ - 273.15)/(d2m_ - 30.1) - (t2m_ - 273.15)/(t2m_ - 30.1)))
            q0(inl+1,idate,ilon,ilat) = q0_
            rho0(inl+1,idate,ilon,ilat) = sp_ /(Rd*t2m_*(1+0.608*q0_))
            z0_ = z0(inl+1,idate,ilon,ilat)
            asl0(inl+1,idate,ilon,ilat) = z0_*RE/(g0_*RE-z0_)

            pxyz0(inl+1,idate,ilon,ilat)%z =  RE + z0_
            pxyz0(inl+1,idate,ilon,ilat)%x = (RE + z0_)*COS(PI*lat_/180.)*COS(PI*lon_/180.)
            pxyz0(inl+1,idate,ilon,ilat)%y = (RE + z0_)*COS(PI*lat_/180.)*SIN(PI*lon_/180.)

            do ilev = 1,levnum(inl+1,idate)

                z_ = z(inl+1,idate,ilon,ilat,ilev)
                asl_ = z_*RE/(g0_*RE-z_)

                asl(inl+1,idate,ilon,ilat,ilev) = asl_
                agl(inl+1,idate,ilon,ilat,ilev) = asl_ - 0.
                
                p_ = level(inl+1,idate,ilev) 
                t_ = t(inl+1,idate,ilon,ilat,ilev) 
                q_ = q(inl+1,idate,ilon,ilat,ilev)
                rho(inl+1,idate,ilon,ilat,ilev) = p_*100/(Rd*t_*(1+0.608*q_))

                pxyz(inl+1,idate,ilon,ilat,ilev)%z =  RE + z_
                pxyz(inl+1,idate,ilon,ilat,ilev)%x = (RE + z_)*COS(PI*lat_/180.)*COS(PI*lon_/180.)
                pxyz(inl+1,idate,ilon,ilat,ilev)%y = (RE + z_)*COS(PI*lat_/180.)*SIN(PI*lon_/180.)

            end do
        end do
        end do


    end do
    end do    
 

end subroutine readmeteovars