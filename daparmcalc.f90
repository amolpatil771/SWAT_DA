subroutine daparmcalc
    use parm
    
    cnt_strt = (da_daycount*n_en_member)-(n_en_member-1)
    cnt_end = (da_daycount*n_en_member)
    da_flag = 0.
    sm_obs1(:,:) = -99
    sm_obs2(:,:) = -99
    end subroutine
    
    
    