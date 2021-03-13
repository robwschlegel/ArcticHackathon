# -*- coding: utf-8 -*-
"""

@author: Adrien Wehrlé, GEUS (Geological Survey of Denmark and Greenland)

"""


import geopandas as gpd
import numpy as np
import pandas as pd
import glob 
from tqdm import tqdm
import os

frontline_files = glob.glob('H:/Hack_the_Arctic/glacier_frontlines/'
                            + 'frontlines_reproj/Fronts*.shp')

for frontline_file in tqdm(frontline_files):
    
    shapefile = gpd.read_file(frontline_file)

    for i in range(0, len(shapefile)):
        
        poly = shapefile.geometry[i]
        
        xcoords = np.array(poly.coords.xy[0])
        ycoords = np.array(poly.coords.xy[1])
        
        group_id = np.repeat(i, len(xcoords))
        
        res = pd.DataFrame({'longitude': xcoords, 'latitude': ycoords,
                            'ID': group_id})
        
        if i == 0:
            
            results = res
            
        else:
            
            results = results.append(res)

    output_file = frontline_file.split(os.sep)[-1].split('.')[0] + '.csv'

    results.to_csv("H:/Hack_the_Arctic/glacier_frontlines/frontlines_csv/" 
                   + output_file)
