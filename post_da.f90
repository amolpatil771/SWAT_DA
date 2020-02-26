subroutine post_da(jj)
    use parm
    integer::kk
    integer, intent(in) :: jj   !hru no
    ens_sol_st(1:n_en_member,1,jj) = xaa(1,1:n_en_member)
    ens_sol_st(1:n_en_member,2,jj) = xaa(2,1:n_en_member)
    
    
    sol_st(1,jj) = sum(ens_sol_st(1:n_en_member,1,jj))/n_en_member
    sol_st(2,jj) = sum(ens_sol_st(1:n_en_member,2,jj))/n_en_member
    
    
    if (sol_st(1,jj)<0) then
        ens_sol_st(1:n_en_member,1,jj) = 0
        sol_st(1,jj) = 0
    end if
    if (sol_st(2,jj)<0) then
        ens_sol_st(1:n_en_member,2,jj) = 0
        sol_st(2,jj) = 0
    end if

    !update profile water content
    
    sol_sw(jj) = 0.
    ens_sol_sw(1:n_en_member,(jj))=0
    
    do kk = 1, sol_nly(jj)
      do ii = 1,n_en_member
          ens_sol_sw(ii,jj)=ens_sol_sw(ii,jj)+ens_sol_st(ii,kk,jj)
    end do
    end do
    
    sol_sw(jj)= sum(ens_sol_sw(1:n_en_member,jj))/n_en_member
    
    !write da_flag = 1
    da_flag(da_daycount,jj) = 1.;
    

    end subroutine