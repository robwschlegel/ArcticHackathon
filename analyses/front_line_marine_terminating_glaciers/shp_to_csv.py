# -*- coding: utf-8 -*-
"""

@author: Adrien Wehrlé, GEUS (Geological Survey of Denmark and Greenland)

"""


import geopandas as gpd
import numpy as np
import pandas as pd

shapefile = gpd.read_file('C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/'
                          + 'analyses/ESA_land_classes/Svalbard_coastline_wgs84_postproc.shp')



for i in range(0, len(shapefile)):
    
    poly = shapefile.geometry[i]
    
    xcoords = np.array(poly.exterior.xy[0])
    ycoords = np.array(poly.exterior.xy[1])
    
    group_id = np.repeat(i, len(xcoords))
    
    res = pd.DataFrame({'longitude': xcoords, 'latitude': ycoords,
                        'ID': group_id})
    
    if i == 0:
        
        results = res
        
    else:
        
        results = results.append(res)

results.to_csv('C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/'
                          + 'analyses/ESA_land_classes/Svalbard_coastline_wgs84_postproc.csv')