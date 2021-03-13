# -*- coding: utf-8 -*-
"""

@author: Adrien Wehrlé, GEUS (Geological Survey of Denmark and Greenland)

"""

import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling
import numpy as np
from osgeo import gdal, ogr, gdalconst
import geopandas as gpd
import matplotlib.pyplot as plt


# %%

# load ESA land classes
input_file = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/analyses/'\
    + 'ESA_land_classes/Svalbard.tif'
esalc_reader = rasterio.open(input_file)
esalc = esalc_reader.read(1)

# save metadata to create a file for glaciers
meta = esalc_reader.meta


#%% 

# create mask for "permanent snow and ice"
glaciers = esalc == 220

# load ESA land classes
output_file = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/analyses/'\
        + 'ESA_land_classes/Svalbard_glaciers.tif'

with rasterio.open(output_file, 'w+', **meta) as dst:
        dst.write(glaciers.astype('uint8'), 1)
        
# %% 
        
# reproject Svalbard glaciers to WGS84
output_file_reproj = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/analyses/'\
        + 'ESA_land_classes/Svalbard_glaciers_wgs84.tif'
        
dst_crs = {'init': 'EPSG:4326'}

with rasterio.open(output_file) as src:
    transform, width, height = calculate_default_transform(src.crs, dst_crs, 
                                                           src.width, 
                                                           src.height, 
                                                           *src.bounds)
    kwargs = src.meta.copy()
    kwargs.update({'crs': dst_crs,'transform': transform, 'width': width,'height': height})

    with rasterio.open(output_file_reproj, 'w', **kwargs) as dst:
            reproject(source=rasterio.band(src, 1),destination=rasterio.band(dst, 1),
                src_transform=src.transform,
                src_crs=src.crs,
                dst_transform=transform,
                dst_crs=dst_crs,
                resampling=Resampling.nearest)
            
# %% 
        
# reproject Svalbard glaciers to WGS84
output_file_shp = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/analyses/'\
        + 'ESA_land_classes/Svalbard_glaciers_wgs84'
        
# create shapefile out of raster
def raster_polygonize(mask_temp_mp, shp_temp_mp):
            
    src_ds = gdal.Open(mask_temp_mp)
    srcband = src_ds.GetRasterBand(1)
    dst_layername = shp_temp_mp
    drv = ogr.GetDriverByName("ESRI Shapefile")
    dst_ds = drv.CreateDataSource( dst_layername + ".shp" )
    dst_layer = dst_ds.CreateLayer(dst_layername, srs = None )
    fd = ogr.FieldDefn("DN", ogr.OFTInteger)
    dst_layer.CreateField(fd)
    dst_field = dst_layer.GetLayerDefn().GetFieldIndex("DN")
    gdal.Polygonize(srcband, None, dst_layer, 0, [], callback=None)
    del src_ds, dst_ds, dst_layer, dst_field 
    
    return dst_layername

raster_polygonize(output_file_reproj, output_file_shp)


# %% 

output_file_shp_postproc = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/analyses/'\
        + 'ESA_land_classes/Svalbard_glaciers_wgs84_postproc.shp'
        
# post-processing on glacier polygons
shapefile = gpd.read_file(output_file_shp + ".shp")    

shapefile['area'] = shapefile.area

# get rid of contour polygons
[shapefile.drop(shapefile['area'].idxmax(), inplace=True) for i in range(4)]

# shapefile_sorted_m = shapefile.to_crs("EPSG:32633")

shapefile_sorted = shapefile.sort_values(by='area')

# shapefile_sorted_processed = shapefile_sorted[shapefile_sorted.area > 0.5]

centroids = shapefile.centroid.reset_index(drop=True)

cx = [centroids[i].xy[0][0] for i in range(0, len(centroids))]
cy = [centroids[i].xy[1][0] for i in range(0, len(centroids))]

glaciers_wgs84_reader = rasterio.open(output_file_reproj)

glaciers_wgs84 = glaciers_wgs84_reader.read(1)

to_keep = []

ax = plt.subplot(111)

for i in range(0, len(cx)):
    
    row, col = glaciers_wgs84_reader.index(cx[i], cy[i])
    
    plt.scatter(cx[i], cy[i])
    
    cell = glaciers_wgs84[row, col]
    
    if cell == 0:
        
        to_keep.append(True)
        
    else:
        
        to_keep.append(False)
        
        
shapefile_sorted['ice'] = to_keep


shapefile_sorted.boundary.plot(color='black', ax=ax)
shapefile_sorted[shapefile_sorted.ice == 1].boundary.plot(color='red', ax=ax)
    
    

# shapefile_sorted_degrees = shapefile_sorted.to_crs('EPSG:4326')

# ax = plt.subplot(111)

# shapefile_sorted.boundary.plot(color='black', ax=ax)

# shapefile_sorted_processed.boundary.plot(color='red', ax=ax)

# %%

shapefile_sorted_processed.to_file(output_file_shp_postproc)


# %% 


from rasterio.mask import mask
from shapely.geometry import mapping

# extract the geometry in GeoJSON format
geoms = shapefile.geometry.values # list of shapely geometries


for i in range(0, len(geoms)):

    geoms2 = [mapping(geoms[i])]
    # extract the raster values values within the polygon 
    with rasterio.open(output_file_reproj) as src:
         out_image, out_transform = mask(src, geoms2, crop=True)
         
    print(np.shape(out_image))
         
    vals = out_image.flatten()
     
    if np.sum(vals == 0) / len(vals) > 0.1:
         
        to_keep.append(True)
        
    else:
        
        to_keep.append(False)
        
        
shapefile_sorted['ice'] = to_keep


shapefile_sorted.boundary.plot(color='black', ax=ax)
shapefile_sorted[shapefile_sorted.ice == 1].boundary.plot(color='red', ax=ax)


