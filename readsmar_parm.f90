subroutine readsmar_parm
    ! This subroutine reads all input parameters for SMAR
    use parm
    
    integer :: ii,jj
    real,dimension(5) :: temp_smar        
    if (smsat == 1) then
        ! read parameter datasets
        open (608, FILE = "Max_sm_99.5_smos.txt")
        do ii=1,msub  
           read (608,*)(sm_max(ii,jj),jj=1,2)
        end do
        close(608)
        
        open (609, FILE = "fcl.txt")
            read (609,*) fcl1
            read (609,*) fcl2
        close (609)
            
        open (610, FILE = "CalSMAR_marol_NSE_obs_smos.txt")
        do ii=1,msub  
           read (610,*)(temp_smar(jj),jj=1,5)
           a_smar(ii,1)   = temp_smar(1)
           b_smar(ii,1)   = temp_smar(2)
           sw2_smar(ii,1) = temp_smar(3)
           sc1_smar(ii,1) = temp_smar(4)
        end do
        close(610)        
    elseif (smsat == 2) then
        open (608, FILE = "Max_sm_99.5_ascat.txt")
        do ii=1,msub  
           read (608,*)(sm_max(ii,jj),jj=1,2)
        end do
        close(608)
        
        open (609, FILE = "fcl.txt")
            read (609,*) fcl1
            read (609,*) fcl2
        close (609)
            
        open (610, FILE = "CalSMAR_marol_NSE_obs_ascat.txt")
        do ii=1,msub  
           read (610,*)(temp_smar(jj),jj=1,5)
           a_smar(ii,1)   = temp_smar(1)
           b_smar(ii,1)   = temp_smar(2)
           sw2_smar(ii,1) = temp_smar(3)
           sc1_smar(ii,1) = temp_smar(4)
        end do
        close(610)
   endif
    end