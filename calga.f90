subroutine calga(z,lat,ga)
    ! get gravity acceleration at the given altitude and latitude
    ! z    altitude above sea level (m)
    ! lat  latitude (degree)
    ! ga   gravity acceleration (m/s^2)
        use constants
        implicit none
    
        real,intent(in) :: z,lat
        real,intent(out) :: ga
        real :: lat_
    
        lat_ = PI*lat/180
        ga = 9.80616*(1-0.00259*cos(2*lat_))*(1-3.14E-7*z)
        
        end subroutine calga