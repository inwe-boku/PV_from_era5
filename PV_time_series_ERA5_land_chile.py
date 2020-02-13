#!/usr/bin/env python
# coding: utf-8

# ### this script calculates the time series of PV in an optimal slope an aspect based on ERA5 land data for the location of the PV plants in chile

# In[1]:


#import the Librarires
import xarray as xr
import pvlib
import pandas as pd
from pandas.plotting import register_matplotlib_converters
import numpy as np
import pvlib
from pvlib.pvsystem import PVSystem
from pvlib.location import Location
from pvlib.modelchain import ModelChain
import matplotlib.pyplot as plt
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
import geopandas as gpd
import glob
from tkinter import Tcl
import gc
import unidecode
import datetime
import scipy as sp


# In[4]:


#define the data sets that are going to be used
input_era5l_data = "/home/lramirez/my-data/ERA5_land_data/derived/era5l_*"
installations_chile = "/home/lramirez/my-data/pv_generation_per_country/chile/solares_fd0779de_0870_4194_b962_83a842d8c316.shp"
#import the locations of PV plants in Chile
#transfrom to wg84 coordinates and addapt the longitudes to the ERA5 land data
pvs_chile = gpd.read_file(installations_chile)

pvs_chile['geometry']
pvs_chile_4326 = pvs_chile.to_crs(epsg=4326)

installations_list = pvs_chile_4326["NOMBRE"]

#create a dataframe with the key data that is necesary for the comparison
installations = pd.DataFrame({'latitude': pvs_chile_4326.geometry.y.values , 
                        'longitude': pvs_chile_4326.geometry.x.values + 360 , 
                        'size': pvs_chile_4326['POTENCIA'].values, 
                        'start_time': pd.to_datetime(pvs_chile_4326['F_OPERACIO'].values), 
                        'end_time': pd.to_datetime('2018-12-30')},
                       index=installations_list)
installations


# In[5]:


era5 = xr.open_mfdataset(input_era5l_data, combine='nested', concat_dim='time', chunks={'lon': 200, 'lat':200})


# In[9]:


def pv_output_system(start_time,end_time,lons,lats,system_size):
    lons_wgs84 = lons - 360
    location = Location(latitude=lats, longitude=lons_wgs84)
    slope = lats
    if lats >= 0:
        aspect = 0
    elif lats < 0:
        aspect = 180
    GHI_in_j = era5.var169.sel(lon=lons, lat=lats, method='nearest').diff("time", 1, label="upper")
    ghi_era5l = (GHI_in_j.where(GHI_in_j > 0, 0))/3600
    timepre = ghi_era5l["time"].values
    timeindex = pd.to_datetime(timepre, dayfirst=True, utc=True, infer_datetime_format=True)
    dayofyear = ghi_era5l.time.dt.dayofyear.values
    Zenith = pvlib.solarposition.get_solarposition(timepre, latitude=lats, longitude=lons_wgs84).zenith.values
    erbs = pvlib.irradiance.erbs(ghi_era5l, zenith=Zenith, datetime_or_doy=dayofyear)
    dni_era5l = erbs["dni"]
    dhi_era5l = erbs["dhi"]
    temperature_pre = (era5.var167.sel(lon=lons,lat=lats, method='nearest'))-273.15
    temperature = temperature_pre[1:].values
    wind_speed_pre2 = (np.sqrt(((era5.var165.sel(lon=lons,lat=lats, method='nearest'))**2)+
             ((era5.var166.sel(lon=lons,lat=lats, method='nearest'))**2)))
    wind_speed_pre1 = wind_speed_pre2 * ((np.log(1/0.25))/(np.log(10/0.25)))
    wind_speed = wind_speed_pre1[1:].values
    weather = pd.DataFrame({'ghi': ghi_era5l.values, 
                        'dni': dni_era5l, 
                        'dhi': dhi_era5l, 
                        'temp_air': temperature, 
                        'wind_speed': wind_speed},
                       index=timeindex)
    sandia_modules = pvlib.pvsystem.retrieve_sam('SandiaMod')
    cec_inverters = pvlib.pvsystem.retrieve_sam('cecinverter')
    #the follow selection requires some sort of automatization
    sandia_module = sandia_modules['Silevo_Triex_U300_Black__2014_']
    #Tries with the stc where understimating the yearly sum. Decided to use the PTC
    PTC = 280.5
    cec_inverter = cec_inverters['ABB__MICRO_0_3_I_OUTD_US_240_240V__CEC_2014_']
    #check that the Paco is at least equal to the STC
    number_of_panels_1kWp = 1000/PTC
    area_1kWp = number_of_panels_1kWp * sandia_module['Area']
    system = PVSystem(surface_tilt=slope, surface_azimuth=aspect,
                  module_parameters=sandia_module,
                  inverter_parameters=cec_inverter)
    mc = ModelChain(system, location)
    mc.run_model(times=weather.index, weather=weather)
    pv_output = mc.ac * number_of_panels_1kWp * system_size
    return pv_output


# In[10]:


#create a data frame to store the results for every plant
installations_list = installations.index
date_range = pd.date_range(start='2014-01-01 00:00:00', end='2018-12-31 23:00:00', freq='H')
timeseries_PV_installations = pd.DataFrame(columns=installations_list, index=pd.to_datetime(date_range))


# In[11]:


for plant in installations.index[:]:
    print(str(installations.loc[plant,'longitude'])+","+str(installations.loc[plant,'latitude'])+"    "+str(datetime.datetime.now()))
    timeseries_PV_installations.loc[:-1,plant] = pv_output_system("2014-01-01 00:00:00", "2018-12-31 00:00:00",installations.loc[plant,'longitude'],installations.loc[plant,'latitude'],1).values
    timeseries_PV_installations.to_csv("dataframe_pv_output_chile_v2.csv")





