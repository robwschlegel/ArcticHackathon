# -*- coding: utf-8 -*-
"""

@author: Adrien Wehrlé, GEUS (Geological Survey of Denmark and Greenland)

"""


import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr
from scipy.spatial import distance
from shapely.geometry import Point
import glob
import os
from tqdm import tqdm
import matplotlib.pyplot as plt
import scipy.stats 

era = xr.open_dataset("H:/Hack_the_Arctic/ERA5/ERA5_Svalbard.nc")

era_t2m = np.array(era.t2m)
era_sst = np.array(era.sst)

era_time = pd.to_datetime(np.array(era['time']), format='%Y-%*-%dT00:00:00.000000000')
    
lon = np.array(era['longitude'])
lat = np.array(era['latitude'])

lons, lats = np.meshgrid(lon, lat)

era_t2m_anl = era.t2m.resample(time='1Y').mean()

era_time_anl = pd.to_datetime(np.array(era_var_anl.time))


# %% 3D matrix of annual datetimes

era_time_rp_anl = np.repeat(era_time_anl, era_t2m.shape[2] * era_t2m.shape[3])
era_time_rp_mtl = np.repeat(era_time, era_t2m.shape[2] * era_t2m.shape[3])

lats_anl = np.repeat(lats.T[:, :, np.newaxis], era_t2m_anl.shape[0], axis=2).transpose((2, 1, 0))
lons_anl = np.repeat(lons[:, :, np.newaxis], era_t2m_anl.shape[0], axis=2).transpose((2, 0, 1))

lats_mtl = np.repeat(lats.T[:, :, np.newaxis], era_t2m.shape[0], axis=2).transpose((2, 1, 0))
lons_mtl = np.repeat(lons[:, :, np.newaxis], era_t2m.shape[0], axis=2).transpose((2, 0, 1))


# %%

# high temporal resolution
# annual
# anomalies

variables = ['t2m', 'sst', 'siconc', 'z', 'msl', 'hmax', 'smlt']


for i, variable in enumerate(variables):
    
    era_var = np.array(era[variable])
    
    era_var_anl = era[variable].resample(time='1Y').mean()
    
    era_time_anl = pd.to_datetime(np.array(era_var_anl.time))
    
    base_time = era_time_anl < pd.to_datetime('2000-01-01')
    
    # anomalies from 1979-2000
    era_var_anl_base = era_var_anl[base_time, 0, :, :]
    era_var_baseline = np.nanmean(era_var_anl_base, axis=0)
    era_var_anl_anomalies = np.array(era_var_anl) - np.array(era_var_baseline)
    
    era_var_anl_anomalies_rs = np.array(era_var_anl_anomalies)[:, 0, :, :]
    era_var_mtl_means_rs = era_var[:, 0, :, :]
    era_var_anl_means_rs = np.array(era_var_anl)[:, 0, :, :]

    if i == 0:
        
        monthly_means = pd.DataFrame({'longitude': lons_mtl.flatten(), 
                                      'latitude': lats_mtl.flatten(),
                                      'time': era_time_rp_mtl,
                                      't2m': era_var_mtl_means_rs.flatten()})
        
        annual_means = pd.DataFrame({'longitude': lons_anl.flatten(), 
                                     'latitude': lats_anl.flatten(),
                                     'time': era_time_rp_anl,
                                     't2m': era_var_anl_means_rs.flatten()})
        
        
        annual_anomalies = pd.DataFrame({'longitude': lons_anl.flatten(), 
                                         'latitude': lats_anl.flatten(),
                                         'time': era_time_rp_anl,
                                         't2m': era_var_anl_anomalies_rs.flatten()})
    else:
        
        monthly_means[variable] = era_var_mtl_means_rs.flatten()
        annual_means[variable] = era_var_anl_means_rs.flatten()
        annual_anomalies[variable] = era_var_anl_anomalies_rs.flatten()
        
github_repo = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/data_for_shiny/'
monthly_means.to_csv(github_repo + 'ERA5_monthly_means.csv')
annual_means.to_csv(github_repo + 'ERA5_annual_means.csv')
annual_anomalies.to_csv(github_repo + 'ERA5_annual_anomalies.csv')
        
        
# %% anomalies from 1979-2000


era_t2m_base = era_t2m[base_time, 0, :, :]

era_sst_base = era_sst[base_time, 0, :, :]

era_t2m_baseline = np.nanmean(era_t2m_base, axis=0)
era_sst_baseline = np.nanmean(era_sst_base, axis=0)



# %%

def linreg_idx(A_arr, cm):
    
    mask = [(~np.isnan(A_arr)) & (~np.isnan(cm))]
    results = linregress(A_arr[mask], cm[mask])
    corrcoeff = results.rvalue
    
    return corrcoeff




# %% decadal trends

def linregress_time(A_arr):

    x = np.arange(0, len(A_arr))
    
    mask = [(~np.isnan(A_arr)) & (~np.isnan(x))]
    results = linregress(x[mask], A_arr[mask])
    y = results.slope * x + results.intercept
    
    slope = results.slope
    
    rvalue = results.rvalue 
    
    residuals = np.nanmean(np.abs(y - A_arr))
    
    variation = y[-1] - y[0]
    
    ratio = variation / residuals 
    
    return slope, rvalue, residuals, viariation, ratio 


for i, variable in enumerate(variables):
    
    ev = np.array(era[variable])[:, 0, :, :]
    
    ev_anl = era[variable].resample(time='1Y').mean()
    
    ev_anl_rp =  np.array(ev_anl[variable])[:, 0, :, :]
    
    slopes, rvalues, residualss, \
        viariations, ratios  = np.apply_along_axis(linregress_time, 0, ev)
        
    if i == 0:
        
        trends = pd.DataFrame({'longitude': lons_anl.flatten(), 
                               'latitude': lats_anl.flatten(),
                               'time': era_time_rp_anl,
                               't2m': era_var_mtl_means_rs.flatten()})
        
    else:
        
        monthly_means[variable] = era_var_mtl_means_rs.flatten()
        
        
    
    
    
    