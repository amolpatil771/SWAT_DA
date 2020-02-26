subroutine Kalman_Gain_calc
    use parm
    real :: a(no_obs_var,no_obs_var), invmat(no_obs_var,no_obs_var), dummy
    real :: c(no_obs_var,no_obs_var) 
    integer :: n,kk
    
    n = no_obs_var
    dummy=st_er_cov(1,1);
    a = (dummy + obs_er_cov)
    call inverse(a,c,n)
    invmat = c
    kalman_gain = matmul (st_par_cov,invmat)
    
    
    
end subroutine 