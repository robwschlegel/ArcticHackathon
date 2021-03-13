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


era_files = glob.glob('H:/Hack_the_Arctic/sea_ice/ice_thickness_nh'
                      + '_ease2-250_cdr-v1p0_*.nc')

for era_file in tqdm(era_files):

    era = xr.open_dataset(era_file)

    era_sit = np.array(era.sea_ice_thickness)
        
    lons = np.array(era['lon'])
    lats = np.array(era['lat'])

    svalbard = (lats.flatten() > 75) & (lats.flatten() < 81) &\
        (lons.flatten() < 38) & (lons.flatten() > 3)
    
    results = pd.DataFrame({'longitude': lons.flatten(), 'latitude': lats.flatten(),
                            'sea_ice_thickness': era_sit.flatten()})
    
    results_svalbard = results[svalbard]
    
        
    
