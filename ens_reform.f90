subroutine ens_reform(j)
    ! this subroutine initialises all soil layer ensembles using todays random number seed in order to avoid development of unrealistic ensemble spreads.
    use parm
    integer, intent(in) :: j
    real :: st1mean, st2mean, kk, st1var, st2var, st1std, st2std
    
    rand_da_sm1 (1:n_en_member,1) = ens_samp(cnt_strt:cnt_end,3)
    rand_da_sm2 (1:n_en_member,1) = ens_samp(cnt_strt:cnt_end,4)
    
    st1mean = sum(ens_sol_st(1:n_en_member,1,j))/n_en_member
    st1var  = 0.
    do kk=1, n_en_member
         st1var = st1var + (((ens_sol_st(kk,1,j)-st1mean)**2.0)/n_en_member)
    end do
    st1std = st1var**0.5
    
    st2mean = sum(ens_sol_st(1:n_en_member,2,j))/n_en_member
    st2var  = 0.
    do kk=1, n_en_member
         st2var = st2var + (((ens_sol_st(kk,2,j)-st2mean)**2.0)/n_en_member)
    end do
    st2std = st2var**0.5
    
    
    do kk = 1,n_en_member
    ens_sol_st(kk,1,j)=(st1mean)+(rand_da_sm1(kk,1)*st1std)
    ens_sol_st(kk,2,j)=(st2mean)+(rand_da_sm2(kk,1)*st2std)
    end do

    sol_st(1,j) = sum(ens_sol_st(1:n_en_member,1,j))/n_en_member 
    sol_st(2,j) = sum(ens_sol_st(1:n_en_member,2,j))/n_en_member


  
     
    end