# Simulation of multi-annual time series of solar photovoltaic power: is the ERA5-land reanalysis the next big step? 
This repository contains the scripts necesary to calculate PV electricity generation from ERA5-land data and for the validation against measured PV output data for the paper "Simulation of multi-annual time series of solar photovoltaic power: is the ERA5-land reanalysis the next big step?". The preprint can be found [here](https://arxiv.org/abs/2003.04131) and the full paper [here](https://www.sciencedirect.com/science/article/pii/S221313882031256X). The code is split in two parts:

## Data download
The necessary data can be downloaded from Zenodo [here](https://zenodo.org/record/3939047#.YCabF2hKg2w).

## Python: clean and prepare the official data of PV output, get the necessary variables from the reanalysis data sets, simulation of PV timeseries, and calculation of simulation quality indicators
Dependencies in Python: os, itertools, xarray, pandas, numpy, pvlib, matplotlib.pyplot, geopandas, glob, tkinter, gc, unidecode, scipy, pathlib (for particularities please see the individual notebooks)

The multiple processing steps were divided in four numbered python Jupyter notebooks: 
- 1_clean_measured_pv_data_installations_chile 
- 2_get_time_series_from_era5_land_and_merra2_for_pv_calculation
- 3_pv_output_from_ERA5_land_and_merra2
- 4_pv_validation_ERA5_land_MERRA2

The last two notebooks with the PV output and performance assessment (3_pv_output_from_ERA5_land_and_merra2 and 4_pv_validation_ERA5_land_MERRA2) can be run from the intermediate results produced by 1_clean_measured_pv_data_installations_chile and 2_get_time_series_from_era5_land_and_merra2_for_pv_calculation. The inputs to run 1_clean_measured_pv_data_installations_chile as well as the outputs of 2_get_time_series_from_era5_land_and_merra2_for_pv_calculation are available in Zenodo (doi: 10.5281/zenodo.3939047). 2_get_time_series_from_era5_land_and_merra2_for_pv_calculation assumes that the user already has downloaded the MERRA-2 and ERA5-land data for the territory of chile and the period 2014-2018.

## R: Quality assessment of data, classification and clustering, final figures
Dependencies in R: tidyverse, irenabpdata, lubridate, dtw

To run the analysis, run script "src/R/run_analysis.R".

We gratefully acknowledge support from the European Research Council (“reFUEL” ERC2017-STG 758149).

