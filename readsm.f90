   subroutine readsm
    use parm
    integer :: ii, jj
   if (smsat == 1) then
        open (506,FILE='sm_ad_mn_std_monthly_marol.txt')
        do ii=1,n_days_sim  
            read (506,*)(smobs(ii,jj),jj=1,mhru)
        end do
        close (506)
    
        open (553,FILE='smdqx_ad_marol.txt')
        do ii=1,n_days_sim  
            read (553,*)(sm_dqx(ii,jj),jj=1,mhru)
        end do
        close (553)
    
        open (554,FILE='smrfi_ad_marol.txt')
        do ii=1,n_days_sim  
            read (554,*)(sm_rfi(ii,jj),jj=1,mhru)
        end do
        close (554)
    elseif (smsat == 2) then
        open (506,FILE='sm_idw_mn_std_monthly_marol_ascat_10_15.txt')
        do ii=1,n_days_sim  
            read (506,*)(smobs(ii,jj),jj=1,mhru)
        end do
        close (506)
    
        open (553,FILE='sm_error_marol_ascat_10_15.txt')
        do ii=1,n_days_sim  
            read (553,*)(sm_dqx(ii,jj),jj=1,mhru)
        end do
        close (553)
    sm_rfi = 0.
    endif 
    end subroutine