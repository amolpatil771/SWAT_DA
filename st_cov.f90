subroutine st_cov
    use parm
    integer :: ii,kk
    real :: trans (n_en_member,no_sta_var)
    real :: dummy4(1,n_en_member), dummy5(1,n_en_member), dummy6(1,n_en_member), dummy7(n_en_member,1)
    trans = transpose(err_xb)
    st_er_cov = (matmul(err_xb,trans))/(n_en_member-1)
    
    st_par_cov(1,1) = st_er_cov(1,1)
    st_par_cov(2,1) = st_er_cov(2,1)
    
    
    
    
    end subroutine