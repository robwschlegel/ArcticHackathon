# analyses/data_proc.R
# This script preps results etc. for use in the shiny app
# Note that this script is intended to be run from the shiny folder as the working directory
# This is because this script was being run while also developing the shiny script

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Svalbard bbox
sval_bbox <- c(9, 30, 76, 81)


# High-res coastline ------------------------------------------------------

# High res coastline
map_base <- read_csv("../data_for_shiny/coastline/Svalbard_coastline_wgs84_postproc.csv") %>% 
  dplyr::rename(lon = longitude, lat = latitude, group = ID) %>% 
  dplyr::select(lon, lat, group) %>% 
  mutate(lon = round(lon, 4), 
         lat = round(lat, 4))
saveRDS(map_base, "data/map_base.Rds")


# Glacier -----------------------------------------------------------------

# Load and process shapefile
glacier_shp <- raster::shapefile("../analyses/ESA_land_classes/Svalbard_glaciers_wgs84_postproc.shp")
glacier_fortified <- broom::tidy(glacier_shp, group = "DN") %>% 
  dplyr::rename(lon = long) %>% 
  filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
         lat >= sval_bbox[3], lat <= sval_bbox[4]) %>% 
  mutate(lon = round(lon, 4),
         lat = round(lat, 4)) %>% 
  dplyr::select(lon, lat, group)
saveRDS(glacier_fortified, "data/glacier_fortified.Rds")


# Glacier fronts ----------------------------------------------------------

# The CSV files
front_csv_files <- dir("../data_for_shiny/glacier_frontlines", pattern = ".csv", full.names = T)

# Function for loading and adding year column
load_front <- function(file_name){
  dat <- read_csv("../data_for_shiny/glacier_frontlines/Fronts2008_wgs84.csv") %>% 
    dplyr::rename(lon = longitude, lat = latitude, group = ID) %>% 
    dplyr::select(lon, lat, group) %>% 
    mutate(lon = round(lon, 4), 
           lat = round(lat, 4),
           year = as.numeric(substring(file_name, 44, 47)))
}

# One combined file
glacier_frontlines <- map_df(front_csv_files, load_front)
saveRDS(glacier_frontlines, "data/glacier_frontlines.Rds")


# Create smaller files for shiny ------------------------------------------

# ERA5 annual anomalies
ERA5_anom <- read_csv("../data_for_shiny/ERA5_annual_anomalies.csv")
ERA5_anom_smol <- ERA5_anom %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  mutate(year = year(time),
         t2m = round(t2m, 2),
         sst = round(sst, 2),
         siconc = round(siconc, 2),
         msl = round(msl, 2),
         smlt = round(smlt*100*100, 4),
         lon = lon-0.1,
         lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  dplyr::select(lon, lat, year, t2m, sst, siconc, msl, smlt) %>% 
  `colnames<-`(c("lon", "lat", "year", "T2m", "SST", "sea ice cover", "MSLP", "snow melt")) #%>% 
  # na.omit() 
saveRDS(ERA5_anom_smol, "data/ERA5_anom_smol.Rds")

# ERA5 annual means
ERA5_mean <- read_csv("../data_for_shiny/ERA5_annual_means.csv")
ERA5_mean_smol <- ERA5_mean %>% 
  dplyr::rename(lon = longitude, lat = latitude) %>% 
  mutate(year = year(time),
         t2m = round(t2m-273.15, 2),
         sst = round(sst-273.15, 2),
         siconc = round(siconc, 2),
         msl = round(msl, 2),
         smlt = round(smlt*100*100, 4),
         lon = lon-0.1,
         lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  dplyr::select(lon, lat, year, t2m, sst, siconc, msl, smlt) %>% 
  `colnames<-`(c("lon", "lat", "year", "T2m", "SST", "sea ice cover", "MSLP", "snow melt")) #%>% 
  # na.omit() 
saveRDS(ERA5_mean_smol, "data/ERA5_mean_smol.Rds")

# ERA5 annual trends
ERA5_trend <- read_csv("../data_for_shiny/ERA5_trends.csv")
ERA5_trend_smol <- ERA5_trend %>% 
  dplyr::rename(lon = longitude, lat = latitude, name = variable, value = slope) %>% 
  filter(name %in% c("t2m", "sst", "siconc", "msl", "smlt")) %>% 
  mutate(value = ifelse(name == "smlt", round(value*10*100*100, 2), round(value*10, 2)),
         rvalue = round(rvalue, 2),
         pvalue = round(pvalue, 2),
         lon = lon-0.1,
         lon = round(lon, 2),
         lat = round(lat, 2)) %>% 
  dplyr::select(lon, lat, name, value, pvalue, rvalue) %>%
  mutate(name = case_when(name == "t2m" ~ "T2m",
                          name == "sst" ~ "SST",
                          name == "siconc" ~ "sea ice cover",
                          name == "msl" ~ "MSLP",
                          name == "smlt" ~ "snow melt"))
saveRDS(ERA5_trend_smol, "data/ERA5_trend_smol.Rds")

# Test plots
ERA5_anom_smol %>% 
  # na.omit() %>% 
  filter(year == 1991) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = T2m))


# Station data ------------------------------------------------------------

# Airport data
station_airport <- read.csv("../data_for_shiny/meto_stations/Svalbard_Airport.csv") %>% 
  dplyr::select(YEAR:DEC) %>% 
  pivot_longer(JAN:DEC, names_to = "month", values_to = "T2m") %>% 
  filter(T2m < 30)

# Ny Alesund data
station_ny <- read.csv("../data_for_shiny/meto_stations/ny_alesund.csv") %>% 
  dplyr::select(YEAR:DEC) %>% 
  pivot_longer(JAN:DEC, names_to = "month", values_to = "T2m") %>% 
  filter(T2m < 30)

