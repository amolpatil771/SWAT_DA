subroutine SMAR_DA(jj)
    use parm
    ! This subroutine implements SMAR and assimilates the extended soil mositure estimates in to SWAT soil profile
    real :: xbb_mean1, xbb_var1, xbb_std1, xbb_mean2, xbb_var2, xbb_std2, gain1, gain2
    real :: y_mean, y_var, y_std, tmstp, y_smar, y2_mean, y2_var, y2_std
    real, dimension(1,n_en_member) :: y23smar
    real, dimension(1,n_en_member) :: xaa_01
    integer, intent(in) :: jj   !hru no
    integer :: ii
    
    ! calculate mean and variance of all layers soil moisture
    xbb_mean1 = sum(xbb(1,1:n_en_member))/n_en_member
    xbb_var1 = 0
    do kk=1, n_en_member
         xbb_var1 = xbb_var1 + (((xbb(1,kk)-xbb_mean1)**2.0)/n_en_member)
    end do
    xbb_std1 = xbb_var1**0.5
    ! 2nd layer
    xbb_mean2 = sum(xbb(2,1:n_en_member))/n_en_member
    xbb_var2 = 0
    do kk=1, n_en_member
         xbb_var2 = xbb_var2 + (((xbb(2,kk)-xbb_mean2)**2.0)/n_en_member)
    end do
    xbb_std2 = xbb_var2**0.5
    
    !Observation
    y_mean = sum(y(1,1:n_en_member))/n_en_member
    y_var = 0
    do kk=1, n_en_member
         y_var = y_var + (((y(1,kk)-y_mean)**2.0)/n_en_member)
    end do
    y_std = y_var**0.5
    
    ! Assimilate Surface soil moisture
    
    if (xbb_var1+y_var == 0 ) then
        gain1 = 0.5
    else
        gain1 = min(1., max(0.,(xbb_var1/(xbb_var1+y_var))))
    end if
    do ii = 1,n_en_member
    xaa(1,ii) = xbb(1,ii) + (gain1)*(y(1,ii) - xbb(1,ii))
    end do
    
    
    ! Predict and assimilate Sub Surface Soil Moisture

    if (da_daycount==1) then
        tmstp =1
    else
        tmstp = tmstp_all(da_daycount-1,jj)
    end if
    tmstp =1
! Layer 2   
        ! Predict layer 2 sm
        do ii = 1,n_en_member
        xbb_1(2,ii) = (xbb_1(2,ii))/sm_max(jj,2)
        y(1,ii) = (y(1,ii))/sm_max(jj,1)
        xaa_01(1,ii) = (xaa(1,ii))/sm_max(jj,1)
        
        y_smar = max(0.,y(1,ii)-sc1_smar(jj,1));
        y23smar(1,ii) = min(fcl1,(sw2_smar(jj,1) + ((xbb_1(2,ii)-sw2_smar(jj,1))*(exp(-a_smar(jj,1)*(tmstp)))) + ((1-sw2_smar(jj,1))*b_smar(jj,1)*y_smar*tmstp)))
        
        end do
        !Analysis layer2
        !convert to depth values
        do ii = 1,n_en_member
        y23smar(1,ii) = (y23smar(1,ii)*sm_max(jj,2))
        end do
        !Mean, Varaiance & Std. Dev.
        y2_mean = sum(y23smar(1,1:n_en_member))/n_en_member
        y2_var = 0
        do kk=1, n_en_member
             y2_var = y2_var + (((y23smar(1,kk)-y2_mean)**2.0)/n_en_member)
        end do
        y2_std = y2_var**0.5
        !Compute analysis
        if (xbb_var2+y2_var == 0 ) then
            gain2 = 0.5
        else
            gain2 = min(1., max(0.,(xbb_var2/(xbb_var2+y2_var))))
        end if
        do ii = 1,n_en_member
            xaa(2,ii) = xbb(2,ii) + (gain2)*(y23smar(1,ii) - xbb(2,ii))
        end do
       
        ! For Printing
        do ii = 1,n_en_member
        y(1,ii) = (y(1,ii))*sm_max(jj,1)
        xaa_01(1,ii) = (xaa_01(1,ii))*sm_max(jj,2)
        end do
       sm_obs1(da_daycount,jj) = sum(y(1,1:n_en_member))/n_en_member
       sm_obs2(da_daycount,jj) = sum(xaa_01(1,1:n_en_member))/n_en_member
        
    end subroutine