subroutine maxndays(ndays_sim)
    use parm
    integer, intent(out):: ndays_sim
    integer :: k, max_year, beg_year, beg_jday, end_jday !local vaariables
    character :: titledum     
    
    
    open (23,file="file.cio") 
    read (23,*) titledum
    read (23,*) titledum
    read (23,*) titledum
    read (23,*) titledum
    read (23,*) titledum
    read (23,*) titledum
    read (23,*) max_year
    read (23,*) beg_year
    read (23,*) beg_jday
    read (23,*) end_jday
    close(23)
    ndays_sim = 0
    do k = 1,max_year
        if (k == 1) then
            if (mod(beg_year,4)==0) then
                ndyear = 366
            else
                ndyear = 365
            endif
            ndays_sim = ndays_sim + ((ndyear-beg_jday) +1)
            beg_year = beg_year + 1
            
        elseif (k == max_year) then
            ndays_sim = ndays_sim + end_jday
            
        else
            if (mod(beg_year,4)==0) then
                ndyear = 366
            else
                ndyear = 365
            endif
            ndays_sim = ndays_sim +ndyear
            beg_year = beg_year + 1
        endif

    end do
    end subroutine 