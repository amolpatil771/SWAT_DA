subroutine st_obs
    use parm
    real :: trans (n_en_member,no_obs_var)
    trans = transpose(err_y)
    st_obs_cov = (matmul(err_xb,trans))/(n_en_member-1)
    
    end subroutine