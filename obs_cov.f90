subroutine obs_cov
    use parm
    real :: trans (n_en_member,no_obs_var)
    trans = transpose(err_y)
    obs_er_cov = (matmul(err_y,trans))/(n_en_member-1)
    
    end subroutine