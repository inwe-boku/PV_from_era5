#!/usr/bin/env python
# coding: utf-8



import pandas as pd
import numpy as np
import geopandas
import requests
import time
import json

#create a list with all data sets with more than on year of data
results_pre = pd.read_csv("indicators_first_attempt.csv", index_col=0)
results = results_pre.drop(["observations", "rmse_h", "p-value_h"], axis=1).copy()
months_more_than_one_year = np.percentile(results["months"],35)
selected_results =  results.loc[results["months"] > months_more_than_one_year].copy()

#imports the locations of the installations
installations_chile_pre = "/home/lramirez/my-data/pv_generation_per_country/chile/solares_fd0779de_0870_4194_b962_83a842d8c316.shp"
pvs_chile = geopandas.read_file(installations_chile_pre)
pvs_chile_4326 = pvs_chile.to_crs(epsg=4326)
installations_list = pvs_chile_4326["NOMBRE"]

#create a dataframe with the key data that is necesary for the comparison
installations_chile = pd.DataFrame({'latitude': pvs_chile_4326.geometry.y.values , 
                        'longitude': pvs_chile_4326.geometry.x.values, 
                        'size': pvs_chile_4326['POTENCIA'].values, 
                        'start_time': pd.to_datetime(pvs_chile_4326['F_OPERACIO'].values), 
                        'end_time': pd.to_datetime('2018-12-30')},
                       index=installations_list)


#create a pandas dataframe to store the time series from renewables ninja
date_range = pd.date_range(start='1/1/2014', end='01/01/2019', freq='H')
timeseries_rn = pd.DataFrame(columns=selected_results.index, index=pd.to_datetime(date_range))


token = '46a50a2516a2c1387c0aeba7d253d772183aaaac'
api_base = 'https://www.renewables.ninja/api/'

s = requests.session()
# Send token header with each request
s.headers = {'Authorization': 'Token ' + token}

#lat = -24.0
#lon = -68.5
#year = 2014

##
# PV example
##
for i in list(selected_results.index):
    end = 0
    for year in range(2014,2019):
        if year == 2016:
            mult = 8784
        else:
            mult = 8760
        start = end
        end = start + mult
        print(str(start)+" "+str(end))
        lat = installations_chile.loc[i]["latitude"]
        lon = installations_chile.loc[i]["longitude"]
        url = api_base + 'data/pv'
        args = {
            'lat': lat,
            'lon': lon,
            'date_from': str(str(year)+'-01-01'),
            'date_to': str(str(year)+'-12-31'),
            'dataset': 'merra2',
            'capacity': 1.0,
            'system_loss': 0.0,
            'tracking': 0,
            'tilt': abs(lat),
            'azim': 180,
            'format': 'json'
        }

        r = s.get(url, params=args)

        # Parse JSON to get a pandas.DataFrame of data and dict of metadata
        parsed_response = json.loads(r.text)

        data = pd.read_json(json.dumps(parsed_response['data']), orient='index')
        timeseries_rn[i][start:end] = np.array(data['electricity'].values.copy())
        #print(timeseries_rn[i][start:end])
        print(str(year)+" "+str(i))
        timeseries_rn.to_csv("timeseries_rn.csv")
        #metadata = parsed_response['metadata']
    time.sleep(710)













