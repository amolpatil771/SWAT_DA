subroutine sol_ens_rest
	use parm
    real :: sw_var1, sw_std1, sol_std_exp,rn_no1(n_en_member,1),tempary_dummy
	integer :: jj, kk, ll
	do jj = 1,mhru
	    do kk = 1,sol_nly(jj)
            if (kk == 1) then 
                tempary_dummy = fcpercent
            elseif (kk == 2) then 
                tempary_dummy = fcpercent2
            elseif (kk == 3) then 
                tempary_dummy = fcpercent3
            end if
            sol_st(kk,jj) = sum(ens_sol_st(1:n_en_member,kk,jj))/n_en_member
            if (sol_st(kk,jj) < 0.) then
            sol_st(kk,jj) = 0.
            end if
        
	        sw_var1 = 0
            sw_std1 = 0
	        do ll=1, n_en_member
                sw_var1=sw_var1+(((ens_sol_st(ll,kk,jj)-sol_st(kk,jj))**2.0)/n_en_member)
	        end do 
	        sw_std1 = sqrt(sw_var1)  
            sol_std_exp =  min(sw_std1, sol_st(kk,jj), max((sol_fc(kk,jj)*tempary_dummy),max(0.,(sol_fc(kk,jj)-sol_st(kk,jj)))))
            
            if (sol_std_exp /= sw_std1) then
                rn_no1(1:n_en_member,1) = ens_sol_st(1:n_en_member,kk,jj)-sol_st(kk,jj)
                rn_no1(1:n_en_member,1) = rn_no1(1:n_en_member,1)/sw_std1
                do ll = 1,n_en_member
                    ens_sol_st(ll,kk,jj)=((sol_st(kk,jj))+((rn_no1(ll,1)*sol_std_exp)))
                end do
            end if
            sol_st(kk,jj) = sum(ens_sol_st(1:n_en_member,kk,jj))/n_en_member
        end do
    end do
end