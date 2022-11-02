subroutine setinput(line)

    use inputs

    implicit none

    character(len=256) :: line,flag,val
    integer :: ieq,icoma,irec

    ieq = index(line,'=')

    flag = trim(adjustl(line(1:ieq-1)))
    val = trim(adjustl(line(ieq+1:)))

    if(flag=="meteofilepath")then
        meteofilepath=val
    else if(flag=="direction")then
        direction=val
    else if(flag=="nestlevel")then
        read(val,*) nestlevel

    else if(flag=="btsim")then
        read(val,*) btsim
    else if(flag=="etsim")then
        read(val,*) etsim
    else if(flag=="btrel")then
        read(val,*) btrel
    else if(flag=="etrel")then
        read(val,*) etrel
    else if(flag=="slonw")then
        read(val,*) slonw
    else if(flag=="slone")then
        read(val,*) slone
    else if(flag=="slats")then
        read(val,*) slats
    else if(flag=="slatn")then
        read(val,*) slatn
    else if(flag=="saglb")then
        read(val,*) saglb
    else if(flag=="saglt")then
        read(val,*) saglt

    else if(flag=="particlename")then
        read(val,*) particlename
    else if(flag=="particlenumber")then
        read(val,*) particlenumber
    else if(flag=="molemass")then
        read(val,*) molemass
    else if(flag=="Vdry")then
        read(val,*) Vdry
    else if(flag=="Awet")then
        read(val,*) Awet
    else if(flag=="Bwet")then
        read(val,*) Bwet

    else if(flag=="nestlonw")then
        read(val,*) nestlonw
    else if(flag=="nestlone")then
        read(val,*) nestlone
    else if(flag=="nestlats")then
        read(val,*) nestlats
    else if(flag=="nestlatn")then
        read(val,*) nestlatn

    else if(flag=="avgtime")then
        read(val,*) avgtime
    else if(flag=="samtime")then
        read(val,*) samtime

    else if(flag=="ogridlonstart")then
        read(val,*) ogridlonstart
    else if(flag=="ogriddlon")then
        read(val,*) ogriddlon
    else if(flag=="ogridnlon")then
        read(val,*) ogridnlon

    else if(flag=="ogridlatstart")then
        read(val,*) ogridlatstart
    else if(flag=="ogriddlat")then
        read(val,*) ogriddlat
    else if(flag=="ogridnlat")then
        read(val,*) ogridnlat

    else if(flag=="ogridnagl")then
        read(val,*) ogridnagl
        allocate(ogridagls(ogridnagl))
    else if(flag=="ogridagls")then
        irec=1
        do while(.true.)
            icoma = index(val,',')
            if(icoma==0)exit
            read(val(1:icoma-1),*) ogridagls(irec)
            val = val(icoma+1:)
            irec = irec + 1
        end do     

    else if(flag=="recordertime")then
        read(val,*) recordertime
    else if(flag=="recordernum")then
        read(val,*) recordernum   
        allocate(recordernames(recordernum))
        allocate(recorderlon(recordernum))
        allocate(recorderlat(recordernum))
        allocate(recorderagl(recordernum))    
    else if(flag=="recordernames")then
        irec=1
        do while(.true.)
            icoma = index(val,',')
            if(icoma==0)exit
            recordernames(irec)=val(1:icoma-1)
            val = val(icoma+1:)
            irec = irec + 1
        end do

    else if(flag=="recorderlon")then
        irec=1
        do while(.true.)
            icoma = index(val,',')
            if(icoma==0)exit
            read(val(1:icoma-1),*) recorderlon(irec)
            val = val(icoma+1:)
            irec = irec + 1
        end do

    else if(flag=="recorderlat")then
        irec=1
        do while(.true.)
            icoma = index(val,',')
            if(icoma==0)exit
            read(val(1:icoma-1),*) recorderlat(irec)
            val = val(icoma+1:)
            irec = irec + 1
        end do

    else if(flag=="recorderagl")then
        irec=1
        do while(.true.)
            icoma = index(val,',')
            if(icoma==0)exit
            read(val(1:icoma-1),*) recorderagl(irec)
            val = val(icoma+1:)
            irec = irec + 1
        end do
    end if
    
    end subroutine setinput