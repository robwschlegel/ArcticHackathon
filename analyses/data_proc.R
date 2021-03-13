# analyses/data_proc.R
# This script preps results etc. for use in the shiny app


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Svalbard bbox
sval_bbox <- c(9, 30, 76, 81)


# Crop global files -------------------------------------------------------

# Trim down the Oliver 2018 paper results
oliver <- readRDS("data/Oliver_2018.Rds")
oliver_sub <- oliver %>% 
  filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
         lat >= sval_bbox[3], lat <= sval_bbox[4]) %>% 
  mutate(val = round(val, 4))
saveRDS(oliver_sub, "shiny/data/Oliver_2018_sub.Rds")

# Trim MHW annual summaries 
MHW_summary_files <- dir("data", pattern = "MHW_cat_pixel", full.names = T)
MHW_summary_all <- map_df(MHW_summary_files, readRDS)
MHW_summary_sub <- MHW_summary_all %>% 
  filter(lon >= sval_bbox[1], lon <= sval_bbox[2],
         lat >= sval_bbox[3], lat <= sval_bbox[4])
saveRDS(MHW_summary_sub, "shiny/data/MHW_summary_sub.Rds")


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
  mutate(value = ifelse(name == "smlt", round(value*10*100*100, 4), round(value*10, 4)),
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

