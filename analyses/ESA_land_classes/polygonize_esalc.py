# -*- coding: utf-8 -*-
"""

@author: Adrien Wehrlé, GEUS (Geological Survey of Denmark and Greenland)

"""

import rasterio
from rasterio.warp import calculate_default_transform, reproject, Resampling

# %%

# load ESA land classes
input_file = 'C:/Users/Pascal/Desktop/UZH_2020/ArcticHackathon/analyses/'\
    + 'ESA_land_classes/Svalbard.tif'
esalc_reader = rasterio.open(input_file)
esalc = esalc_reader.read(1)

# save metadata to create a file for glaciers
meta = esalc_reader.meta

x_positions, y_positions = esalc_reader.xy(np.arange(0, np.shape(esalc)[0]), 
                                           np.arange(0, np.shape(esalc)[1]))


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