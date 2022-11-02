subroutine readfield(filename,data)

    use newtype
    use netcdf
    
    implicit none
    
    include 'netcdf.inc'
    
    character(len=256),intent(in) ::filename
    type(meteodata),intent(out) :: data
    integer :: ierr,ncid,&
               lonvarid,latvarid,lvarid,&
               zvarid,qvarid,uvarid,vvarid,wvarid,tvarid,&
               u10varid,v10varid,t2mvarid,fsrvarid,blhvarid,&
               zustvarid,z0varid,spvarid,sshfvarid,d2mvarid,&
               lsmvarid,cbhvarid,crrvarid,lsrrvarid
    integer :: nlons,nlats,nlevels
    integer :: londimid,latdimid,leveldimid
    integer,allocatable,dimension(:,:)   :: temp2d
    integer,allocatable,dimension(:,:,:) :: temp3d
    real :: bias, slope ! transfer from short into real
    
    if (allocated(data%longitude)) then
    deallocate (data%longitude, data%latitude,data%level,&
                data%d2m,data%t2m,data%blh,&
                data%sp,data%z0,data%fsr,&
                data%v10,data%u10,data%sshf,data%zust,&
                data%u,data%v,data%w,data%q,data%z,&
                data%t,data%lsm,data%cbh,data%crr,&
                data%lsrr)
    end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 1 --> Open file and get the file id.                                   !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! open netcdf file
    print *, "Read meteorology file: "//trim(filename)
    ierr = nf90_open(trim(adjustl(filename)),mode = nf90_nowrite,ncid = ncid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 2 --> Read dimension information and calculate the length of each      !
    ! dimension.                                                                  !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! read dimension information
    ierr = nf90_inq_dimid(ncid,'longitude', londimid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_inq_dimid(ncid,'latitude', latdimid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_inq_dimid(ncid,'level',leveldimid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ! How many values of "lon" "lat" "level" are there?
    ierr = nf90_inquire_dimension(ncid, londimid, len = nlons)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_inquire_dimension(ncid, latdimid, len = nlats)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_inquire_dimension(ncid, leveldimid, len = nlevels)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 3 --> Initialise dimension of each variable in the struct.             !
    !            Fortran dimension sequence is inversed compared to the original  !
    !            data. For example, in the origin data, wind component U is shown !
    !            by:                                                              !
    !            level-latitude-longitude                                         !
    !            in fortran code, the sequence is:                                !
    !            longitude-latitude-level                                         !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    allocate(data%longitude(nlons),data%latitude(nlats),&
    data%level(nlevels) )
    allocate(data%d2m(nlons,nlats),data%t2m(nlons,nlats),&
    data%v10(nlons,nlats),data%u10(nlons,nlats),&
    data%sp(nlons,nlats), data%z0(nlons,nlats),&
    data%sshf(nlons,nlats),data%blh(nlons,nlats),&
    data%lsm(nlons,nlats),data%cbh(nlons,nlats),&
    data%crr(nlons,nlats),data%lsrr(nlons,nlats) )
    allocate(data%u(nlons,nlats,nlevels),data%v(nlons,nlats,nlevels),&
    data%w(nlons,nlats,nlevels),data%q(nlons,nlats,nlevels),&
    data%z(nlons,nlats,nlevels),data%t(nlons,nlats,nlevels) )
    allocate(temp2d(nlons,nlats),temp3d(nlons,nlats,nlevels))
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! step 4 read data from the ncfile into the struct.                           !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! read longitude info
    ierr = nf90_inq_varid(ncid,'longitude',lonvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, lonvarid, data%longitude)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ! read latitude info
    ierr = nf90_inq_varid(ncid,'latitude',latvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, latvarid, data%latitude)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ! read level info
    ierr = nf90_inq_varid(ncid,'level',lvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, lvarid, data%level)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! read ground info                                                            !
    ! Be careful the original data in nc files may be short in type.              !
    ! We use REAL type in actual program. Type transfer needed.                   !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    ! read boundary layer height - blh
    ierr = nf90_inq_varid(ncid,'blh',blhvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, blhvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, blhvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, blhvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%blh = temp2d * slope + bias
    
    ! read dew point at 2 meters - d2m
    ierr = nf90_inq_varid(ncid,'d2m',d2mvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, d2mvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, d2mvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, d2mvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%d2m = temp2d * slope + bias
    
    ! read temperature at 2meters - t2m
    ierr = nf90_inq_varid(ncid,'t2m',t2mvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, t2mvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, t2mvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, t2mvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%t2m = temp2d * slope + bias
    
    ! read v10
    ierr = nf90_inq_varid(ncid,'v10',v10varid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, v10varid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, v10varid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, v10varid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%v10 = temp2d * slope + bias
    
    ! read u10
    ierr = nf90_inq_varid(ncid,'u10',u10varid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, u10varid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, u10varid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, u10varid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%u10 = temp2d * slope + bias
    
    ! read sp
    ierr = nf90_inq_varid(ncid,'sp',spvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, spvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, spvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, spvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%sp = temp2d * slope + bias
    
    ! read z0
    ierr = nf90_inq_varid(ncid,'z0',z0varid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, z0varid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, z0varid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, z0varid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%z0 = temp2d * slope + bias
    
    ! read zust - friction velocity
    ierr = nf90_inq_varid(ncid,'zust',zustvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, zustvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, zustvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, zustvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%zust = temp2d * slope + bias
    
    ! read fsr - forecast surface roughness
    ierr = nf90_inq_varid(ncid,'fsr',fsrvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, fsrvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, fsrvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, fsrvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%fsr = temp2d * slope + bias
    
    ! read sshf
    ierr = nf90_inq_varid(ncid,'sshf',sshfvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, sshfvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, sshfvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, sshfvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%sshf = temp2d * slope + bias
    
    ! read lsm
    ierr = nf90_inq_varid(ncid,'lsm',lsmvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, lsmvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, lsmvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, lsmvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%lsm = temp2d * slope + bias
    
    ! read cbh
    ierr = nf90_inq_varid(ncid,'cbh',cbhvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, cbhvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, cbhvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, cbhvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%cbh = temp2d * slope + bias
    
    ! read crr
    ierr = nf90_inq_varid(ncid,'crr',crrvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, crrvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, crrvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, crrvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%crr = temp2d * slope + bias
    
    ! read lsrr
    ierr = nf90_inq_varid(ncid,'lsrr',lsrrvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, lsrrvarid, temp2d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, lsrrvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, lsrrvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%lsrr = temp2d * slope + bias
    
    ! read levels data
    ! read u,v,w,q,z,t
    ! read temperature
    ierr = nf90_inq_varid(ncid,'t',tvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, tvarid, temp3d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, tvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, tvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%t = temp3d * slope + bias
    
    ! read U
    ierr = nf90_inq_varid(ncid,'u',uvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, uvarid, temp3d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, uvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, uvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%u = temp3d * slope + bias
    ! read V
    ierr = nf90_inq_varid(ncid,'v',vvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, vvarid, temp3d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, vvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, vvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%v = temp3d * slope + bias
    ! read W
    ierr = nf90_inq_varid(ncid,'w',wvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, wvarid, temp3d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, wvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, wvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%w = temp3d * slope + bias
    ! read q
    ierr = nf90_inq_varid(ncid,'q',qvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, qvarid, temp3d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, qvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, qvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%q = temp3d * slope + bias
    ! read z
    ierr = nf90_inq_varid(ncid,'z',zvarid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_var(ncid, zvarid, temp3d)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, zvarid, "scale_factor", slope)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    ierr = nf90_get_att(ncid, zvarid, "add_offset", bias)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    data%z = temp3d * slope + bias
    
    ! close netcdf file
    ierr = nf90_close(ncid)
    if (ierr /= nf90_noerr) call handle_err(ierr)
    
    end subroutine readfield