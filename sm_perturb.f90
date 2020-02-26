subroutine sm_perturb(j)
    use parm
    integer :: jj
    integer, intent(in) :: j   !hru no

    rand_da_sm1 (1:n_en_member,1) = ens_samp(cnt_strt:cnt_end,3)
    rand_da_sm2(1:n_en_member,1) = ens_samp(cnt_strt:cnt_end,4)

    sm_std_1 = sol_st(1,j)*sm_std1
    sm_std_2 = sol_st(2,j)*sm_std2

    
        do jj=1,n_en_member
            ens_sol_st(jj,1,j) = ens_sol_st(jj,1,j)  + (rand_da_sm1(jj,1)*sm_std_1)
        end do
        do jj=1,n_en_member
            ens_sol_st(jj,2,j) = ens_sol_st(jj,2,j) + (rand_da_sm2(jj,1)*sm_std_2)
        end do
        

        
        do jj = 1,2
            sol_st(jj,j) = sum(ens_sol_st(1:n_en_member,jj,j))/n_en_member
        end do
    end