module constants
    ! define constants used in the program
    implicit none

    real,parameter :: RE = 6370949 ! mean earth radius (m) 
    real,parameter :: PI = 3.1415926535897932 ! PI
    real,parameter :: Cp = 1004.07 ! dry air heat ratio (given pressure) (J/(kg K))
    real,parameter :: Rd = 287     ! dry air gas constant (J /(kg K))
    real,parameter :: karman = 0.4 ! karman constant
    real,parameter :: g0 = 9.816   ! standard gravity acceleration (m/s2)
    real,parameter :: OMEGA = 7.292116E-5  ! angular velocity of the earth spin  rad/s
    real,parameter :: LC = 1.0  ! loss coefficient in one hit

    integer,parameter :: maxlonnum=1445,maxlatnum=725,maxznum=50

end module constants


module newtype
    ! deifne some new types in the program
    implicit none

    type particle
        real :: weight                   
        integer(kind=8) :: time
        integer(kind=8) :: reltime
        integer :: status
        integer :: id,coreid=0
        real :: lon,lat,asl,agl
        real :: u=0.,v=0.,w=0.
        real :: uturb=0.,vturb=0.,wturb=0.
        ! weight      particle weight, no particle lost in the program
        ! time        particle time (secs from 1990-01-01 00:00)
        ! reltime     particle releasing time
        ! status      0 normal 1 dead -1 not initialised
        ! id          particle id
        ! coreid      CPU core id used to process particle(parrell only)
        ! agl         above ground level
        ! uu          turbulence sigma
    end type particle

    type meteodata
        ! universal variables
        real,allocatable,dimension(:) :: longitude,latitude,level
        ! ground variables in SI unit
        real,allocatable,dimension(:,:) :: u10,v10,t2m,blh,fsr,zust,z0,sp,sshf,d2m,lsm,cbh,crr,lsrr
        ! u10     10m u wind
        ! v10     10m v wind
        ! t2m     2m temperature
        ! blh     boudary layer height
        ! fsr     forecast surface roughness
        ! zust    friction velocity
        ! z0      surface geopotential
        ! sp      surface pressure
        ! sshf    surface sensible heat flux
        ! d2m     2m dew point
        ! lsm     land sea mask(land fraction)
        ! cbh     cloud base height
        ! crr     convective rain rate
        ! lsrr    large scale rain rate

        ! levels variables in SI unit
        real,allocatable,dimension(:,:,:) :: z,q,u,v,w,t
        ! z       geopotential
        ! q       relative humidity
        ! u,v     u and v wind
        ! w       vertical velocity (pressure)
        ! t       temperature

        ! parameters needed to be calculated
        ! these parameters are required but not offered by the meteo data
        real,allocatable,dimension(:,:) :: asl0,rho0,q0
        ! asl0    surface geometry height Above Sea Level
        ! rho0    surface air density 
        ! q0      specific humidity
        real,allocatable,dimension(:,:,:) :: asl,agl,rho
        ! asl     geometry height Above Sea Level
        ! agl     geometry height Above Gorund Level
        ! rho     air density 
    end type meteodata

    type xyz
        real :: x,y,z
        ! coordinates in Cartesian frame
    end type xyz

end module newtype

module inputs
    implicit none
    ! begin control
    character(len=256) :: meteofilepath,direction="forward"
    integer :: nestlevel=0,particlenumber=1000

    ! begin source
    character(len=14) :: btsim,etsim,btrel,etrel,recordertime
    integer(kind=8) :: secbtsim,secetsim,secbtrel,secetrel
    real :: slonw,slone,slats,slatn,saglb,saglt

    ! begin particles
    character(len=256) :: particlename
    real :: molemass, Vdry, Awet, Bwet  

    ! begin nest
    real :: nestlonw, nestlone, nestlats, nestlatn

    ! begin recorders
    character(len=256),allocatable :: recordernames(:)
    real,allocatable :: recorderlon(:), recorderlat(:), recorderagl(:) 
    integer :: recordernum
    ! recordernames      names of each recorder 
    ! recorderlon        longitude of each recorder
    ! recordernum        total number of recorder
    integer :: avgtime = 1800, samtime = 300
    ! avgtime   - average time used for both receptor and outgrid  (s)
    ! samtime   - sampling time used for both receptor and outgrid sampling(s)
    real :: ogridlonstart,ogriddlon,&
            ogridlatstart,ogriddlat
    integer :: ogridnagl,ogridnlon,ogridnlat
    real,allocatable :: ogridagls(:)

    integer :: filenum
    character(len=14),allocatable,dimension(:) :: datelist
    character(len=64),allocatable,dimension(:) :: filelist
    ! filenum     number of datefiles
    ! datelist    date list of the time used in the program 
    !              must in YYYYMMDDHHMMSS format
    ! filelist    file list of the date file linked by the datelist

end module inputs

module variables
    ! define common variables in the program
    use newtype

    implicit none
    type(particle),allocatable,dimension(:) :: particles
    ! particles    store all particles used in the running
    type(meteodata) :: md
    ! md           meteorology data
    integer(kind=8) :: secl,secr
    ! secl(r)      second at the left(right) boundary

    integer(kind=8),allocatable,dimension(:) :: timecache
    ! time cache is the time dots read in one time loop
    integer,allocatable,dimension(:,:) :: lonnum,latnum,levnum
    !                       nest,1D
    !                             |___ time
    real,allocatable,dimension(:,:) :: lonfirst,lonlast,dlon,&
                                      &latfirst,latlast,dlat
    !                       nest,1D
    !                             |___ time
    real,allocatable,dimension(:,:,:) :: longitude,latitude,level

    real,allocatable,dimension(:,:,:,:) :: u10,v10,t2m,blh,fsr,zust,z0,sp,&
                                          &sshf,d2m,lsm,cbh,crr,lsrr,&
                                          &asl0,rho0,q0 ! asl0 rho0 q0 is caled not read                              
    !                      nest  3D
    !                      nest   |___ time,lon,lat
    ! 2D variables in the meteodata, the time domain is added by the author
    ! if you want to now why, u can contact him
    real,allocatable,dimension(:,:,:,:,:) :: z,q,u,v,w,t,&
                                            &asl,agl,rho !asl,agl,rho is caled not read
    !                      nest  4D
    !                      nest   |___ time,lon,lat,lev
    ! 2D variables in the meteodata, the time domain is added by the author
    ! if you want to now why, u can contact him
    type(xyz),allocatable,dimension(:,:,:,:)   :: pxyz0
    type(xyz),allocatable,dimension(:,:,:,:,:) :: pxyz
    
end module variables