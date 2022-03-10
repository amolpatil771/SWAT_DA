# SWAT_DA source code
The current SWAT hydrological model source code has been improved to facilitate the assimilation of satellite based soil moisture observations using EnKF.

The detailed data assimilation process was presented in https://www.sciencedirect.com/science/article/abs/pii/S0022169417307357.

In addition to the SWAT project, 5 more files are required for data assimilation, which are listed below.

1) dqx_hru.txt   > number of simulation days *  number of hrus   >   soil moisture dqx error index for SMOS (-ve if unavailable) (see manuscript)
2) rfi_hru.txt   > number of simulation days *  number of hrus   >   soil moisture rfi error index for SMOS (-ve if unavailable)
3) sample.txt    > random numbers (ensemble members * number of simulation days) * 6  
4) sm_obs.txt    > number of simulation days *  number of hrus   >   soil moisture observation after quality control (-ve if unavailable)
5) input.da      > DA setup
