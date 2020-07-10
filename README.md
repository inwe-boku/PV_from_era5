# Simulation of multi-annual time series of solar photovoltaic power: is the ERA5-land reanalysis the next big step? 
This repository contains the scripts necesary to calculate PV electricity generation from ERA5-land data and for the validation against measured PV output data for the paper "Simulation of multi-annual time series of solar photovoltaic power: is the ERA5-land reanalysis the next big step?". The preprint can be found [here][https://www.researchgate.net/publication/339814539_Simulation_of_long-term_time_series_of_solar_photovoltaic_power_is_the_ERA5-land_reanalysis_the_next_big_step] and the full paper, once published here. The code is split in two parts:

## Python: Download of reanalysis data, simualtion of PV timeseries, and calculation of simulation quality indicators
add instructions

## R: Quality assessment of data, classification and clustering, final figures
Dependencies in R: tidyverse, irenabpdata, lubridate, dtw

To run the analysis, run script "run_analysis.R".

The script downloads the intermediate results produced by python from Zenodo, classifies installations and creates figures.