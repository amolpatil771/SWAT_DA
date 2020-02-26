subroutine readinput
    use parm
    ! This subroutine reads Data Assimilation Input

    open (503, file = "input.da")
    
        read (503,*) da   ! code for data assimilation 0 = no DA, 1 = DA
        read (503,1111) pcp_std_par !precipitation error coefficient
        read (503,1111) fcpercent !fcpercent
        read (503,1111) fcpercent2 !fcpercent2
        read (503,1111) fcpercent3 !fcpercent3
        read (503,1111) sm_dqx_co !observed soil moisture DQX coefficient
        read (503,1111) sm_rfi_co !observed soil moisture rfi coefficient
        read (503,1111) sm_std1 !soil moisture error std. deviaition mm/mm (layer1)
        read (503,1111) sm_std2 !soil moisture error std. deviaition mm/mm (layer2)
        read (503,1111) sm_std3 !soil moisture error std. deviaition mm/mm (layer3)
        read (503,1111) cn_std  !CN error std
        read (503,*) n_en_member  ! number of ensemble members 
        read (503,*) n_ensembles  ! number of ensembles
        read (503,*) n_parameters  !number of parameters
        read (503,*) n_sampling_method  ! sampling method code  !1 = Sobol Sequence; 2 = Halton Sequence; 3 = LHS; 4 = random sampling
        read (503,*) sol_st_std       ! std. Dev. for soil storage initialisation
        read (503,*) no_sta_var         !total number of states and parameters in state vector
        read (503,*) no_obs_var         !total number of observations in observation vector	
        read (503,*) wrdtf         !'0' for no '1' for yes, write data to file or not
        read (503,*) da_stopday     ! day from which DA will be stopped (forecasting performance check)
        read (503,*) da_alg         !Assimilation Method 1 = EnKF_covariance_Update 2 = BLUE
        read (503,*) smsat          !Soil moisture observations 1 = SMOS 2 = ASCAT
        close(503)
1111    format(f5.2)
        
       
        
    end subroutine