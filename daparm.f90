subroutine daparm
    use parm
    
    allocate (ens_precip (n_en_member,1))
    allocate (rand_da (n_en_member,1))
    allocate (rand_da_cn (n_en_member,1))
    allocate (rand_da_sm1 (n_en_member,1))
    allocate (rand_da_sm2 (n_en_member,1))
    allocate (rand_da_sm3 (n_en_member,1))
    allocate (rand_da_en (n_en_member,1))
    allocate (ens_surfq_l (n_en_member,1))
    
    end
    