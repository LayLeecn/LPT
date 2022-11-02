subroutine handle_err(ierr)
    ! error handle subroutine for netcdf 
    
    use netcdf 
    
    implicit none
    
    include 'netcdf.inc'
    
    integer,intent(in) :: ierr
    
    print *, nf90_strerror(ierr)
    stop
    
    end subroutine handle_err