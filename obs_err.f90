subroutine obs_error
    use parm
    
    integer :: ii,kk
    real :: meanvect(no_obs_var), meanyr(no_obs_var,1)
    
    meanvect = sum(y,2)/size(y,2)
    
    do kk = 1,no_obs_var
       meanyr(kk,1)= meanvect(kk)
    enddo
    
    do kk = 1,no_obs_var
        do ii = 1,n_en_member
        err_y(kk,ii) = y(kk,ii) - meanyr(kk,1)
        end do
    end do
    
    end subroutine