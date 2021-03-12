# analyses/data_proc.R
# This script preps results etc. for use in the shiny app


# Setup -------------------------------------------------------------------

library(tidyverse)

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
