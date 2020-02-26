subroutine rngen
    use parm
    ! This subroutine calls Matlab executable required for generating samples for ensemble simulation and    
    ! Reads generated samples from 'sample.txt'
    integer :: ii, jj
    character :: system_path*150, system_call*174, ens_member*6
    character :: ens_size*6, ens_param*6, ens_samplmethod*6
    
    
    
    call maxndays(n_days_sim)
    write (*,1234) n_days_sim
                
    
    !! Actual number of ensembles
    
    n_ensembles=n_days_sim*n_ensembles
    samp_nrows = n_ensembles*n_en_member
    samp_ncolumns = n_parameters
    
    allocate (ens_samp(samp_nrows,samp_ncolumns))
    !real :: ens_samp (PUT=samp_nrows,PUT=samp_ncolumns)
    !! Generate String
    write(ens_member, "(i6)") n_en_member
    write(ens_size, "(i6)") n_ensembles
    write(ens_param, "(i6)") n_parameters
    write(ens_samplmethod, "(i6)") n_sampling_method
    
    !system_path = "C:\Users\Amol\Documents\MATLAB\sampledrawml15\for_testing\sampledrawml15.exe"
    !system_call = system_path//ens_member//ens_size//ens_param//ens_samplmethod
    !call system(system_call) 
    
    
    
    ! Read generated sample.txt as array
    
        open (502,FILE='sample.txt')
        do ii=1,samp_nrows  
        read (502,*)(ens_samp(ii,jj),jj=1,samp_ncolumns)
        end do

1234    format (1x,' Total number of days for simulation', i5)    
    end subroutine