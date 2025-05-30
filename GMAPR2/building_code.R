library(devtools)
library(testthat)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(jpeg)
library(grid)
library(cowplot)
library(geosphere)
library(lubridate)
library(sf)
library(terra)
library(ggimage)
library(terrainr)
library(data.table)
library(png)
library(santoku)
# library(RStoolbox)

load_all("C:/Users/rfranc01/OneDrive - Environmental Protection Agency (EPA)/Documents/GMAP_Xact/gmap_package/GMAPR/GMAPR2")
library(GMAPR2)

use_r("rawdataprep")
use_r("rawlist_2_df")
use_r("onoff")
use_r("MA_ST_bind")
use_r("splitsville")
use_r("subsamp_temporal_syft")
# use_r("subsamp_temporal_syft_zero")
use_r("subsamp_temporal_pic")
use_r("pic_flagging")
use_r("time_flagging")
use_r("syft_flagging")
# use_r("raster_comb")
use_r("ground_speed_flagging")
use_r("gps_precision_flagging")
use_r("flow_check")
use_r("met_analyte_comb")
use_r("transect_max")
use_r("transect_time_minmax")
use_r("output_csv_data")
use_r("method_split_syft")
# use_r("ts_table")
use_r("ts_plot")
use_r("MA_map")
use_r("ST_map")
use_r("qual_sum")
use_r("dash_prep")
