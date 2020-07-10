# Simulation of multi-annual time series of solar photovoltaic power: is the ERA5-land reanalysis the next big step? 
This repository contains the scripts necesary to calculate PV electricity generation from ERA5-land data and for the validation against meassured PV output data. The code is split in two parts:

## Python: Download of reanalysis data, simualtion of PV timeseries, and calculation of simulation quality indicators
add instructions

## R: Quality assessment of data, classification and clustering, final figures
Dependencies in R: tidyverse, irenabpdata, lubridate, dtw

To run the analysis, run script "run_analysis.R".

The script downloads the intermediate results produced by python from Zenodo, classifies installations and creates figures.