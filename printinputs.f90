subroutine printinputs
    use inputs

    implicit none
    
    print *,"Start Date of Simulation:",btsim(1:4),"-",btsim(5:6),"-",btsim(7:8),"-",btsim(9:10)
    print *,"End   Date of Simualtion:",etsim(1:4),"-",etsim(5:6),"-",etsim(7:8),"-",etsim(9:10)
    print *,"Releasing start at:",btrel(1:4),"-",btrel(5:6),"-",btrel(7:8),"-",btrel(9:10)
    print *,"Releasing end   at:",etrel(1:4),"-",etrel(5:6),"-",etrel(7:8),"-",etrel(9:10)

    print *,"Name of tracer is ",trim(particlename)
    if(Vdry<=0.)then
        print *,"No dry deposition."
    end if
    if((Awet<=0.).or.(Bwet<=0.))then
        print *,"No wet deposition."
    end if

end subroutine printinputs