subroutine readdatefile

    use inputs
    implicit none

    character(len=256) :: filename,line
    integer :: error,iline,iab

    ! check the number of files
    filename = trim(adjustl(meteofilepath))//"filelist"
    open(unit=20,file=filename,status='old',action="read")
    filenum = 0
    do while(.true.)
        read(20,"(A)",iostat=error) line
        if(error/=0)exit
        filenum = filenum + 1
    end do        
    close(20)

    if(allocated(datelist))deallocate(datelist)
    if(allocated(filelist))deallocate(filelist)

    allocate(datelist(filenum))
    allocate(filelist(filenum))

    open(unit=20,file=filename,status='old',action="read")
    do iline=1,filenum
        read(20,"(A)",iostat=error) line
        iab = index(line,'>')
        datelist(iline) = line(1:iab-1)
        filelist(iline) = line(iab+1:)

    end do
    close(20)

    end subroutine readdatefile