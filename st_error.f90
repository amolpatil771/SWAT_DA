subroutine st_error
        use parm
    
    integer :: ii,kk
    real :: meanvect(no_sta_var), meanxbr(no_sta_var,1)
    
    meanvect = sum(xbb,2)/size(xbb,2)
    
    do kk = 1,no_sta_var
       meanxbr(kk,1)= meanvect(kk)
    enddo
    
    do kk = 1,no_sta_var
        do ii = 1,n_en_member
    err_xb(kk,ii) = xbb(kk,ii) - meanxbr(kk,1)
        end do
    end do
    
    end subroutine