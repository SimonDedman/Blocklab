# Postage Stamp Plots, Simon Dedman, simondedman@gmail.com 2021-08-18
# Load functions ####
library(dplyr)
library(sf)
library(sp)
library(ggplot2)
library(ggspatial)
library(marmap)
library(lubridate) # today()
library(tidylog)

AllDailies <- readRDS("/home/simon/Documents/Si Work/Blocklab/abft_diving/All_Daily/AllDailies_HIFSDA_Stocknames_StocksPaperVersion.Rds")

# choose specific fish, 5 each of GOM & Med, select only required cols
# Manually chosen GOM (check they're all in AllDailies)
# gomlist <- c(5107037, 5107043, 5107046, 5108017, 5108020, 5108021, 5108024, 5109026, 5109027, 5110062, 5110065, 5110070, 5110075, 5110076, 5110078, 5111016, 5111022, 5111026, 5111045, 5112037, 5117004)
gomlist <- c(5107037, 5109026, 5110070, 5117004) # 5111026,  was 4th
# Manually chosen Med
# medlist <- c(5102410, 5103497, 5103505, 5103539, 5103546, 5105027, 5108022, 5109003, 5110008, 5111017, 5112001, 5112003, 5114009, 5116014)
medlist <- c(5102410, 5105027, 5109003, 5112003) # 5103546,  was 2nd
bothlist <- c(gomlist, medlist)
# bothlist <- 5102410 # fail fast to fix problems below
df <- AllDailies %>%
  select(toppid, lon, lat, Stock) %>%
  filter(toppid %in% bothlist)

# Params to set ####
# input data, ordered as desired e.g. only certain fish
# input data lon & lat column names
# input data fishID column name, e.g. toppid
# input data grouping column name, e.g. Stock
# nrow & ncol for facet

# Auto Extents ####
# lonmin = min(df$lon, na.rm = TRUE)
# lonmax = max(df$lon, na.rm = TRUE)
# latmin = min(df$lat, na.rm = TRUE)
# latmax = max(df$lat, na.rm = TRUE)

# Manually set extents of plots.
lonmin = -97 # -97 full df extents
lonmax = 17 # 14.3
latmin = 18 # 19.8
latmax = 63 # 61.5

sf_lines <- sf::st_as_sf(df, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) %>%
  dplyr::group_by(toppid) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("MULTILINESTRING") %>%
  left_join(AllDailies %>%
              select(toppid, Stock) %>% # add stock
              distinct) %>%
  mutate(toppid = factor(toppid, levels = bothlist))

# Coastline ####
# Get this automatically from data using gbm.auto::gbm.basemap
natlantic <- read_sf("/home/simon/Documents/Si Work/Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map.shp")

# Bathymetry ####
b = marmap::getNOAA.bathy(lon1 = lonmin,
                  lon2 = lonmax,
                  lat1 = latmin,
                  lat2 = latmax,
                  resolution = 4, #1 max? 4 default
                  keep = TRUE,
                  path = "/home/simon/Documents/Si Work/Blocklab/MapData/getNOAAbathy/")

# TODO####
## facet_wrap - make param {{ summary_vars }} or .data[[var]]
# axis labels: remove degrees N/W?
# set extents per plot rather than all the same? would need to loop if so? Can turn off universal scales in facet?
# bathy colour scale like james' - trail off at 2000 etc (is fine currently though?)
# ggsave width/height as function of latlon extent & nrow ncol

autoplot.bathy(x = b, geom = "raster", coast = FALSE) + # in place of ggplot
  annotation_spatial(sf_lines, #  %>% filter(Stock != "UNK", DataType != "P")
                     size = 0.5, #line thickness on map & legend
                     color = "black", #colour on map & legend
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  annotation_spatial(natlantic, fill = "grey", lwd = 0) +
  facet_wrap(. ~ toppid, nrow = 2) + # , scales = "free_x" getNOAA.bathy
  scale_fill_gradient2(low = "dodgerblue4", mid = "gainsboro", high = "darkgreen") +
  scale_x_continuous(breaks = seq(-180, 180, by = 20), expand = c(0,0)) + # x tick frequency
  scale_y_continuous(expand = c(0,0)) + # buffering space remove
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none", #c(0.2, 0.8), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.text = element_blank(), # don't want/need facet headerings for toppids, mention them in caption instead
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        panel.spacing = unit(0.5, "mm"),
        plot.margin = unit(c(0, 0, 0, 0), "mm")) + # removed whitespace buffer around legend boxes which is nice
  ggsave(paste0("/home/simon/Dropbox/Blocklab Monterey/Papers/StocksPaper/Figures/", today(), "_FacetToppidPostStampPlot.png"), # noUnk
         plot = last_plot(), device = "png", path = "", scale = 2, width = 6, # 0.04 * 71 # 2.84 Unk
         height = 1.7, units = "in", dpi = 300, limitsize = TRUE)
