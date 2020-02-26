      subroutine surq_daycn

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Predicts daily runoff given daily precipitation and snow melt
!!    using a modified SCS curve number approach

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnday(:)    |none          |curve number for current day, HRU and at 
!!                               |current soil moisture
!!    fcimp(:)    |fraction      |fraction of HRU area that is classified
!!                               |as directly connected impervious
!!    ihru        |none          |HRU number
!!    iurban(:)   |none          |urban simulation code:
!!                               |0  no urban sections in HRU
!!                               |1  urban sections in HRU, simulate using USGS
!!                               |   regression equations
!!                               |2  urban sections in HRU, simulate using build
!!                               |   up/wash off algorithm
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    urblu(:)    |none          |urban land type identification number from
!!                               |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bb          |none          |variable used to store intermediate 
!!                               |calculation result
!!    cnimp       |none          |curve number for impervious areas
!!    j           |none          |HRU number
!!    pb          |none          |variable used to store intermediate
!!                               |calculation result
!!    r2          |none          |retention parameter in CN equation
!!    surfqimp    |mm H2O        |surface runoff from impervious area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      
      use parm

      integer :: ii,j,counter,jj
      real :: r2, bb, pb, cnimp, surfqimp,pcp_std1,stncount,pcp_std2,
     &  precipday_temp,r2_temp,bb_temp,pb_temp,ens_surfq_mean,
     &  surfq_temp
      real :: cn_ens(n_en_member,1),r2_ens(n_en_member,1),
     &  bb_ens(n_en_member,1),pb_ens(n_en_member,1),
     &  cnimp_ens(n_en_member,1), ens_surfqimp(n_en_member,1)
      
      
      
      
      if (da == 0 ) then
      j = 0
      j = ihru
      
      r2 = 0.
      bb = 0.
      pb = 0.
      r2 = 25400. / cnday(j) - 254.
      bb = .2 * r2
      pb = precipday - bb
      
      
      
      if (pb > 0.) then
        surfq(j) = pb * pb / (precipday + .8 * r2)
      end if


      if (iurban(j) > 0) then
        r2 = 0.
        bb = 0.
        pb = 0.
        surfqimp = 0.
        cnimp = 98.
        r2 = 25400. / cnimp - 254.
        bb = .2 * r2
        pb = precipday - bb
        if (pb > 0.) then
          surfqimp = pb * pb / (precipday + .8 * r2)
        end if
        surfq(j) = surfq(j) * (1. - fcimp(urblu(j))) +                  
     &                                        surfqimp * fcimp(urblu(j))
        
      endif    
      
      
      cn_flag(da_daycount,j) = 1
      
      !! DA Loop
      
      else
      !!!! Calculate determinstic values first
      j = 0
      j = ihru
      
      r2 = 0.
      bb = 0.
      pb = 0.
      r2 = 25400. / cnday(j) - 254.
      bb = .2 * r2
      pb = precipday - bb
      
      
      
      if (pb > 0.) then
        surfq(j) = pb * pb / (precipday + .8 * r2)
      end if
      !!!! END
      !!!!Calculate ensemble calculations and then correct biases with determinstic run
          r2_ens = 0.
          bb_ens = 0.
          pb_ens = 0.
          cn_ens = 0.
          ens_surfqimp = 0.
          cnimp = 98.
          ens_surfq_mean = 0.
      !!!perturb precipitation and curve number
          rand_da_cn (1:n_en_member,1) = ens_samp(cnt_strt:cnt_end,6)
          do ii = 1,n_en_member
          cn_ens(ii,1)=(cnday(j)+(rand_da_cn(ii,1)*cn_std))
          cnimp_ens(ii,1)=(cnimp+(rand_da_cn(ii,1)*cn_std))
          end do
                  
          
          if (precipday>=0.01) then

          pcp_std2 = (pcp_std_par*precipday)
          if (pcp_std2> precipday) pcp_std2 = precipday
          do ii = 1,n_en_member
          ens_precip(ii,1)=((precipday+(rand_da(ii,1)*pcp_std2)))
          end do
          else
          ens_precip(1:n_en_member,1) = precipday
          end if           
          
          do ii = 1,n_en_member
          if (ens_precip(ii,1)<0) then
              ens_precip(ii,1)=0
          end if
          end do
          precipday_temp = sum(ens_precip)/n_en_member
          
          if (abs(precipday_temp-precipday)>1.e-6) then
          ens_precip(1:n_en_member,1) = ens_precip(1:n_en_member,1)*
     &        (precipday/precipday_temp)
          end if         
          precipday = sum(ens_precip)/n_en_member
          
          if (precipday<0.) then
              precipday=0.
              ens_precip=0.
          end if 
      !!!! END
      !!!! Calculate surfq
          do ii = 1,n_en_member
          r2_ens(ii,1) = 25400. / cn_ens(ii,1) - 254.
          end do
          r2_temp = (sum(r2_ens))/ n_en_member
          bb_ens = .2 * r2_ens
          bb_temp = (sum(bb_ens))/ n_en_member
          do ii = 1,n_en_member
          pb_ens(ii,1) = max(0.,ens_precip(ii,1) - bb_ens(ii,1))
          end do
          pb_temp = (sum(pb_ens))/ n_en_member              
          
          if (abs(pb_temp-pb)>0 .and. pb>0 .and. pb_temp>0) then
          pb_ens(1:n_en_member,1) = pb_ens(1:n_en_member,1)*
     &        (pb/pb_temp)
          end if      
          pb_temp = sum(pb_ens)/ n_en_member  
          pb      = sum(pb_ens)/n_en_member
          
          if (pb > 0.) then
                            
          ! calculate ensemble of excess precip
          do  ii = 1,n_en_member
          if (pb_ens(ii,1)>0. .and. ens_precip(ii,1)>0.) then
          ens_surfq(ii,j)=(pb_ens(ii,1)*pb_ens(ii,1))
     &    /(ens_precip(ii,1) + (.8 * r2_ens(ii,1))) 
          else
          ens_surfq(ii,j)=0.
          end if
          end do
          ens_surfq_mean = sum(ens_surfq(1:n_en_member,j))/n_en_member
      if(abs(ens_surfq_mean-surfq(j))>1.e-6 .and. ens_surfq_mean>0)then
      ens_surfq(1:n_en_member,j) = ens_surfq(1:n_en_member,j)*
     &        (surfq(j)/ens_surfq_mean)
      end if
          ens_surfq_mean = sum (ens_surfq(1:n_en_member,j))/n_en_member
          surfq(j)       = sum (ens_surfq(1:n_en_member,j))/n_en_member
          end if
          
      if (iurban(j) > 0) then
      r2 = 0.
      bb = 0.
      pb = 0.
      surfqimp = 0.
      cnimp = 98.
      r2 = 25400. / cnimp - 254.
      bb = .2 * r2
      pb = precipday - bb
      if (pb > 0.) then
      surfqimp = pb * pb / (precipday + .8 * r2)
      end if
      surfq(j) = surfq(j) * (1. - fcimp(urblu(j))) +     
     &           surfqimp * fcimp(urblu(j))
        
       
         
      r2_ens = 0.
      bb_ens = 0.
      pb_ens = 0.
      ens_surfqimp = 0.
      cnimp = 98.
      do ii = 1,n_en_member
          r2_ens(ii,1) = 25400. / cnimp_ens(ii,1) - 254.
      end do
      r2 = (sum(r2_ens))/ n_en_member
      bb_ens = .2 * r2_ens
      bb = (sum(bb_ens))/ n_en_member
      do ii = 1,n_en_member
          pb_ens(ii,1) = max(0.,ens_precip(ii,1) - bb_ens(ii,1)) 
      end do
      pb_temp = (sum(pb_ens))/ n_en_member              
      if (abs(pb_temp-pb)>0. .and. pb_temp>0.) then
          pb_ens(1:n_en_member,1) = pb_ens(1:n_en_member,1)*
     &    (pb/pb_temp)
      end if
      pb_temp = sum(pb_ens)/ n_en_member  
      pb      = sum(pb_ens)/n_en_member

      if (pb > 0.) then
      do  ii = 1,n_en_member
      if (pb_ens(ii,1)>0. .and. ens_precip(ii,1)>0.) then
          ens_surfqimp(ii,1)=(pb_ens(ii,1)*pb_ens(ii,1))
     &    /(ens_precip(ii,1) + (.8 * r2_ens(ii,1)))
      else
          ens_surfqimp(ii,1)=0.
      end if
      end do
        ens_surfq_mean = sum(ens_surfqimp(1:n_en_member,1))/n_en_member
      if (abs(ens_surfq_mean-surfqimp)>1.e-6) then
          ens_surfqimp(1:n_en_member,1) = ens_surfqimp(1:n_en_member,1)*
     &        (surfqimp/ens_surfq_mean)
      end if
      ens_surfq_mean=sum (ens_surfqimp(1:n_en_member,1))/n_en_member
      !mean of surface flow ensemble for impervious layer
      end if
      do ii = 1,n_en_member
          ens_surfq(ii,j)=(ens_surfq(ii,j))*
     &    (1. - fcimp(urblu(j)))+(ens_surfqimp(ii,1))*
     &    (fcimp(urblu(j)))
      end do
          surfq_temp = sum(ens_surfq(1:n_en_member,j))/n_en_member
      if (abs(surfq_temp-surfq(j))>0. .and. surfq(j)>0) then
          ens_surfq(1:n_en_member,j) = ens_surfq(1:n_en_member,j)*
     &        (surfq(j)/surfq_temp)
      else if (surfq(j) == 0) then
          ens_surfq(1:n_en_member,j) = 0.
      end if      
          surfq_temp = sum (ens_surfq(1:n_en_member,j))/n_en_member
          surfq(j)   = sum (ens_surfq(1:n_en_member,j))/n_en_member
      end if 
      endif
          cn_flag(da_daycount,j) = 1
          precipday_all(da_daycount,j) = precipday
      return
      end