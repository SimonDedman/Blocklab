# 2020-08-26 - 2021-09-30 Compilation of code from SSMplotAllDailies.R & HomeRangeChange.R
# Only for figures to be used in the GOM/Med stocks paper
# Simon Dedman, simondedman@gmail.com

# Load & Prep ####
library(ggpubr) # group_by, mutate
library(tidyverse) # broom      cli        crayon     dbplyr     dplyr      forcats    ggplot2    haven      hms        httr       jsonlite   lubridate
# magrittr   modelr     pillar     purrr      readr      readxl     reprex     rlang      rstudioapi  rvest      stringr    tibble     tidyr      xml2       tidyverse
library(lubridate)
library(magrittr) # tidyverse magrittr doesn't load %<>%
library(tidylog)
library(data.table) # sudo apt install libgit2-dev # libssl-dev # libssh2-1-dev # libxml2-dev # libudunits2-dev # rgdal : gdal-config
library(sf)
library(ggspatial)
library(scales)
library(beepr)
options(error = function() beep(9))  # give warning noise if it fails
machine <- "/home/simon/Documents/Si Work/" #Aquarius
loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #per saveloc in MolaFoldersExtractLoop.R
# AllDailies <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds"))
AllDailies <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames_StocksPaperVersion.Rds"))
# Already done in StocksPaperVersion
# AllDailies %<>%
#   ungroup %>% # if grouped when saved, can cause issues
#   filter(!str_detect(seriesname, "Gulf|Corsica|Cartagena|Ireland|France|WWF|Morocco|Israel|England|Spain"), # Manually remove ineligible tag series
#          Stock %in% c("GOM", "Med")) %>% # keep only assigned GOM & Med fish
#   drop_na(Date, lat, lon) %>% # omit rows with NA values
#   mutate(DataType = str_sub(fishID, -1, -1), # # "C" "D" "P"
#          Day = lubridate::yday(Date))

# Issue: drop_na(Date, lat, lon) used on whole dbase but removes usable data!####
# 3 instances within script, but input data likely already filtered this way.

# AllDailies %>%
#   + group_by(Stock) %>%
#   + summarise(toppids = length(unique(toppid)))
# Stock toppids
# GOM        78
# Med        42

AllDailies %>%
  group_by(toppid) %>%
  summarise(ndays = length(Date)) %>%
  ungroup() %>%
  summarise(ndayrange = range(ndays,na.rm = T))

# Already done in StocksPaperVersion
# AllDailies %<>% # remove dupes, nrow 31473 before; 30055 after; 1418 removed. 159 vars before, 140 after, 19 removed:
#   # DeeperDay          DayDepthO          NightDepthU        MeanLtU            DayMinusNtLtU      DVM                WhichForageHours   ForageHoursDepth
#   # WhichForageHoursA  WhichForageHoursB  WhichForageHoursC  Umidtime           UdurMins           Umeandepth         SdaStartTime       SdaPeakItempTime
#   # SdaDepBins         MarineAreaThisYear spawner
#   group_by(toppid, Date) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE),
#             across(where(~ is.character(.) | is.POSIXt(.)), first)) %>% # library(lubridate)
#   mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
#          across(where(~ is.character(.)), ~ ifelse(is.nan(.), NA, .))) %>% # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
#   ungroup

natlantic <- read_sf(paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map.shp"))
natlanticextents <- data.frame(lon = c(-95,35), #set manual extents to force the same map size each time
                               lat = c(10,65)) #tightened as display expands a buffer, data are -98W:36E & 8N:65N
extents <- sf::st_as_sf(natlanticextents, coords = c("lon","lat")) %>% sf::st_set_crs(4326) #using jlondon example, may be incorrect? Is N.Pac
datemin <- min(year(AllDailies$Date))
datemax <- max(year(AllDailies$Date))
print(paste0("n fishdays GOM: ", AllDailies %>% filter(Stock == "GOM") %>% nrow,
             ". n fishdays Med: ", AllDailies %>% filter(Stock == "Med") %>% nrow))
# "n fishdays GOM: 16088. n fishdays Med: 13967"
# 2021-08-17  "n fishdays GOM: 16396. n fishdays Med: 13967"
# Already done in StocksPaperVersion
# fishlist <- read_csv(file = "/home/simon/Dropbox/Blocklab Monterey/Papers/StocksPaper/Figures/AllFishUsedInPaperTable.csv") %>%
#   mutate(toppid = as.integer(TOPPID)) %>%
#   pull(toppid)
# AllDailies %<>% filter(toppid %in% fishlist) # filter: removed 246 rows (1%), 30,117 rows remaining
# rm(fishlist)

# colourblind palette colours
CB_PALETTE      = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# plot(x = 1:8, y = 1:8, col = CB_PALETTE, pch = 16, cex = 4)
CB_BLUE         = CB_PALETTE[6] # "#0072B2"
CB_RED          = CB_PALETTE[7] # "#D55E00"


# 2020.11.05 Compare only midsized individuals####
# AllDailies %<>% filter(between(FishLengthCm, 200, 250)) # keep only assigned GOM & Med fish
# print(paste0("n fishdays GOM: ", AllDailies %>% filter(Stock == "GOM") %>% nrow,
#              ". n fishdays Med: ", AllDailies %>% filter(Stock == "Med") %>% nrow))
# "n fishdays GOM: 4459. n fishdays Med: 3099"
# saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/SM_200-250cm/") #ensure trailing /slash





# Fig SM was 1: median location tracks, velocity colours####
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/Maps/") #ensure trailing /slash
# median location per yearday (better to use Emils loop-normalising code for northernmost point etc)

ADyday <- AllDailies %>%
  group_by(Stock, Day) %>%
  summarise(lat = median(lat, na.rm = T),
            lon = median(lon, na.rm = T),
            MeanVelBL = mean(StepLengthBL, na.rm = T),
            MeanVelKm = mean(StepLengthKm, na.rm = T),
            Month = first(Month))

ADmonth <- ADyday %>% # for month stats
  group_by(Stock, Month) %>%
  summarise(MeanVelBL = mean(MeanVelBL, na.rm = T),
            MeanVelKm = mean(MeanVelKm, na.rm = T))

sfADyday <- sf::st_as_sf(ADyday, coords = c("lon","lat")) %>% sf::st_set_crs(4326) #points by day
# sfADyweek <- sf::st_as_sf(ADyweek, coords = c("lon","lat")) %>% sf::st_set_crs(4326) #points by day

sfMonth_lines <- sfADyday %>%
  dplyr::group_by(Stock, Month) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("MULTILINESTRING") %>% # nrow 177
  left_join(ADmonth)

# sfMonth_lines <- sfADyweek %>%
#   dplyr::group_by(Stock, Month) %>%
#   dplyr::summarise(do_union = FALSE) %>%
#   sf::st_cast("MULTILINESTRING") %>% # nrow 177
#   left_join(ADmonth)

sfLabels <- st_centroid(sfMonth_lines)
# range(sfMonth_lines$MeanVelBL) # 17251.90 41158.17

zonelines <- data.frame(lon = c(-74.55, -9.5, -45, -45), # data for ablines for plot
                        lat = c(39.3, 39.3, 10, 60),
                        linename = c("EW", "EW", "NS", "NS"))
sf_zonepoints <- sf::st_as_sf(zonelines, coords = c("lon","lat")) %>% sf::st_set_crs(4326) #points by day
sf_zonelines <- sf_zonepoints %>%
  dplyr::group_by(linename) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("MULTILINESTRING")

natlanticextents <- data.frame(lon = c(-93, -58), lat = c(25, 47))
extents <- sf::st_as_sf(natlanticextents, coords = c("lon","lat")) %>% sf::st_set_crs(4326)

ggplot() +   #plot lines by year
  annotation_spatial(sf_zonelines, # sf_points
                     size = 0.3, #line/point thickness on map & legend
                     color = "black", #colour on map & legend
                     key_glyph = "abline") +
  annotation_spatial(natlantic, fill = "grey", lwd = 0) + #layer_spatial trains scales, annotation_ doesnt
  layer_spatial(extents, size = 0, col = "white") +
  annotation_spatial(sfMonth_lines, #  %>% filter(Stock != "UNK", DataType != "P")
                     size = 0.5, #line thickness on map & legend
                     aes(colour = MeanVelBL, #colour on map & legend
                         linetype = Stock),
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  geom_sf_label(data = sfMonth_lines,
                aes(label = month.abb[Month]),
                label.padding = unit(0, "lines"),
                label.r = unit(0, "lines"),
                label.size = 0,
                fill = NA) +
  scale_linetype_manual(values = c("solid", "dashed")) + # add "dotted" for a 3rd group
  scale_colour_gradientn(colours = rev(rainbow(12))) +
  coord_sf(xlim = c(natlanticextents[1,1], natlanticextents[2,1]), # instead of scale_x_continuous, gives white background with rug ticks but no lines
           ylim = c(natlanticextents[1,2], natlanticextents[2,2]),
           expand = F) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.84, 0.1), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  ggtitle(paste0("Mean daily distance travelled (body lengths) by ABFT"),
          subtitle = paste0("By stock and month, ", datemin, ":", datemax,
                            "; n fish=", length(unique(AllDailies$toppid)),
                            ", GOM: ", AllDailies %>% filter(Stock == "GOM") %>% summarise(toppid = unique(toppid)) %>% nrow,
                            ", Med: ", AllDailies %>% filter(Stock == "Med") %>% summarise(toppid = unique(toppid)) %>% nrow)) + # change sflines
  ggsave(paste0(saveloc, today(), "_MonthlyVelByStock.png"),
         plot = last_plot(), device = "png", path = "",
         scale = 2, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 6, #NA default. Manually adjust plot box in RStudio after ggplot()
         height = 4.5, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
         units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
         dpi = 300, limitsize = TRUE)







# Fig2 6panel Dot facet & violin plots FIGURE 6####
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/points/")
AllDailies$MeanDepth24h <- -AllDailies$MeanDepth24h
AllDailies %<>%
  mutate(MarineZone_f = factor(
    case_when(MarineZone == "ForageW" ~ "Forage",
              MarineZone == "MixingW" ~ "Mixing",
              TRUE ~ MarineZone), # everything else
    levels = c("GSL", "Forage", "Mixing", "GOM", "Med")))
source('~/Dropbox/Blocklab Monterey/Blocklab/recurve_scales.R')
myseq <- c(seq(-600, -100, by = 100), -50, -25, -10, -5, -1)

# MeanDepth
ggplot(AllDailies %>% drop_na(MarineZone_f), # ,MeanDepth24h > 1
       aes(x = as.Date(Day, origin = as.Date("2018-01-01")),
           y = MeanDepth24h)) +
  geom_point(aes(colour = Stock), size = 0.5) + # , col = toppid # , group = toppid
  stat_smooth(aes(colour = Stock)) +
  facet_wrap(.~MarineZone_f) + # , scales = "free" default fixed scales allows sharing axis labels
  labs(x = "Year Day", y = "Mean Depth, m") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE, "grey")) +
  # avoid terminal Jan: https://stackoverflow.com/questions/14759676/specification-of-first-and-last-tick-marks-with-scale-x-date
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2018-01-01"), as.Date("2018-12-30")), expand = c(0,0)) +
  # scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100), trans = recurve_trans) + # depth as positive, 0 at bottom (bad)
  # scale_y_continuous(limits = c(600, 0), breaks = seq(600, 0, by = -100), trans = recurve_trans) + # depth as positive, 0 at top: no data plots, excluding limits plots data but 0bottom, excluding breaks 0top decent breaks no data
  scale_y_continuous(limits = c(-600, 0), breaks = myseq, trans = recurve_trans) + # depth as negative, works
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   # legend.key.width = unit(2, "cm"),
                                   panel.grid.minor = element_blank(), # remove mid value x & y axis gridlines
                                   panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_FacetZone_MeanDepth", ".png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)

# MeanDepth 6th Panel violin
ggplot(AllDailies %>% drop_na(MarineZone_f),
       aes(x = MarineZone_f, y = MeanDepth24h, colour = Stock, fill = Stock)) +
  geom_violin() + # , size = 0.5
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, position = position_dodge(0.9), colour = "white") + # , fill = "black" kills stock separation
  # stat_summary(fun.data = mean_sdl, mult = 1, geom = "pointrange", color = "black") +
  labs(x = "Marine Zone", y = "Mean Depth, m") +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  scale_y_continuous(limits = c(-600, 0), breaks = myseq, trans = recurve_trans) + # depth as negative, works
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.82, 0.08),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_FacetZoneExtra_MeanDepthViolin", ".png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)



# MeanETemp24h
ggplot(AllDailies %>% drop_na(MarineZone_f),
       aes(x = as.Date(Day, origin = as.Date("2018-01-01")),
           y = MeanETemp24h)) +
  geom_point(aes(colour = Stock), size = 0.5) + # , col = toppid # , group = toppid
  stat_smooth(aes(colour = Stock)) +
  facet_wrap(.~MarineZone_f) + #, scales = "free"
  labs(x = "Year Day", y = "Mean External Temperature, C") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE, "grey")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2018-01-01"), as.Date("2018-12-30")), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5), limits = c(5, 30)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_FacetZone_MeanTemp", ".png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)

# MeanTemp 6th Panel violin
ggplot(AllDailies %>% drop_na(MarineZone_f),
       aes(x = MarineZone_f, y = MeanETemp24h, colour = Stock, fill = Stock)) +
  geom_violin() + # , size = 0.5
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, position = position_dodge(0.9), colour = "white") + # , fill = "black" kills stock separation
  labs(x = "Marine Zone", y = "Mean External Temperature, C") +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5), limits = c(5, 30)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.82, 0.08),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_FacetZoneExtra_MeanTempViolin", ".png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)



# Mixing W hotspot addition
setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/ByZoneGroupStock/MixingWhotspot"))
# MeanDepth
ggplot(AllDailies %>% filter(between(lon, -79, -73),
                             between(lat, 31, 36)), # ,MeanDepth24h > 1
       aes(x = as.Date(Day, origin = as.Date("2018-01-01")),
           y = MeanDepth24h)) +
  geom_point(aes(colour = Stock), size = 0.5) + # , col = toppid # , group = toppid
  stat_smooth(aes(colour = Stock)) +
  labs(x = "Year Day", y = "Mean Depth, m") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE, "grey")) +
  # avoid terminal Jan: https://stackoverflow.com/questions/14759676/specification-of-first-and-last-tick-marks-with-scale-x-date
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2018-01-01"), as.Date("2018-12-30")), expand = c(0,0)) +
  # scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100), trans = recurve_trans) + # depth as positive, 0 at bottom (bad)
  # scale_y_continuous(limits = c(600, 0), breaks = seq(600, 0, by = -100), trans = recurve_trans) + # depth as positive, 0 at top: no data plots, excluding limits plots data but 0bottom, excluding breaks 0top decent breaks no data
  scale_y_continuous(limits = c(-600, 0), breaks = myseq, trans = recurve_trans) + # depth as negative, works
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_MeanDepth", ".png"),
         plot = last_plot(), device = "png", scale = 3.5, width = 8,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)

# MeanDepth 6th Panel violin
ggplot(AllDailies %>% filter(between(lon, -79, -73),
                             between(lat, 31, 36)),
       aes(x = Stock, y = MeanDepth24h, colour = Stock, fill = Stock)) +
  geom_violin() + # , size = 0.5
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, position = position_dodge(0.9), colour = "white") + # , fill = "black" kills stock separation
  # stat_summary(fun.data = mean_sdl, mult = 1, geom = "pointrange", color = "black") +
  labs(x = "Marine Zone", y = "Mean Depth, m") +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  scale_y_continuous(limits = c(-600, 0), breaks = myseq, trans = recurve_trans) + # depth as negative, works
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.82, 0.08),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_MeanDepthViolin", ".png"),
         plot = last_plot(), device = "png", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)

# MeanETemp24h
ggplot(AllDailies %>% filter(between(lon, -79, -73),
                             between(lat, 31, 36)),
       aes(x = as.Date(Day, origin = as.Date("2018-01-01")),
           y = MeanETemp24h)) +
  geom_point(aes(colour = Stock), size = 0.5) + # , col = toppid # , group = toppid
  stat_smooth(aes(colour = Stock)) +
  labs(x = "Year Day", y = "Mean External Temperature, C") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE, "grey")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2018-01-01"), as.Date("2018-12-30")), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5), limits = c(5, 30)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.03, 0.98),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_MeanTemp", ".png"),
         plot = last_plot(), device = "png", scale = 3.5, width = 8,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)

# MeanTemp 6th Panel violin
ggplot(AllDailies %>% filter(between(lon, -79, -73),
                             between(lat, 31, 36)),
       aes(x = Stock, y = MeanETemp24h, colour = Stock, fill = Stock)) +
  geom_violin() + # , size = 0.5
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, position = position_dodge(0.9), colour = "white") + # , fill = "black" kills stock separation
  labs(x = "Marine Zone", y = "Mean External Temperature, C") +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  scale_y_continuous(breaks = seq(5, 30, by = 5), limits = c(5, 30)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.82, 0.08),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(today(), "_MeanTempViolin", ".png"),
         plot = last_plot(), device = "png", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)




# Fig3etc 2d boxplots FIGURE 7&9####
if (min(AllDailies$MeanDepth24h, na.rm = T) < 0) AllDailies$MeanDepth24h <- -AllDailies$MeanDepth24h # above re-reverses depths which are made negative above
library(mapplots)
library(maptools)
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/Maps/2DBarplots/") #ensure trailing /slash
# saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/Maps/2DBarplots/200-250cmSubset/") #ensure trailing /slash
# Following Probabilistic Assignment section, do these lines then repoeast this section:
# dir.create(paste0(saveloc, "/ProbAssign"))
# saveloc = paste0(saveloc, "ProbAssign/")
# AllDailies %<>% filter(Stock %in% c("algorithmGOM", "algorithmMed"))
# Also: L526 & 7
natlantic2 <- readShapeSpatial(paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map.shp"))
proj4string(natlantic2) <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
#  https://gis.stackexchange.com/questions/48416/aggregating-points-to-grid-using-r/48434#48434
barplot2dMap <- function(x,
                         latcol = "lat",
                         loncol = "lon",
                         origin = c(0,0),
                         cellsize = c(1,1),
                         groupcol = "Stock",
                         mycolours = c(CB_RED, CB_BLUE),
                         legendtitle = "2D Barplot",
                         zCol = "Count", # MeanETemp24h MeanDepth24h
                         zFun = "sum",
                         baseplot = natlantic2,
                         xlim = c(-95,35),
                         ylim = c(10,65),
                         lon1 = MarZonesTable$lonmin[i],
                         lon2 = MarZonesTable$lonmax[i],
                         lat1 = MarZonesTable$latmin[i],
                         lat2 = MarZonesTable$latmax[i],
                         bathyres = 1, # resolution for getNOAA.bathy
                         bathysavepath = paste0(machine, "Blocklab/MapData/getNOAAbathy/"),
                         pngwidth = 2000,
                         pngheight = 1000,
                         saveloc = "/home/simon/Documents/Si Work/Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/Maps/",
                         plotname = paste0(today(), "_2DBarplot_Count")) {
  library(maptools)
  library(dplyr)
  library(tidyverse)
  library(marmap)
  # get bathymetry data
  b = getNOAA.bathy(lon1 = lon1,
                    lon2 = lon2,
                    lat1 = lat1,
                    lat2 = lat2,
                    resolution = bathyres,
                    keep = TRUE,
                    path = bathysavepath)

  xy = cbind(x[,loncol], x[,latcol])
  adminCells <-  t(apply(xy, 1, function(z) cellsize/2 + origin + cellsize*(floor((z - origin)/cellsize)))) # data to cell bins
  x$X <- adminCells[, 1]
  x$Y <- adminCells[, 2]
  x$Cell <- paste(x$X, x$Y)

  cellSummaries <- x %>%
    group_by(Cell, !!sym(groupcol)) %>% # group by cell & groupcol
    summarise(Count = n(), # add Count
              !!zCol := match.fun(zFun)(!!sym(zCol), na.rm = T), # output colname zCol created as zFun on zCol
              # https://stackoverflow.com/questions/62906259/r-user-defined-dynamic-summary-function-within-dplyrsummarise#62906361
              across(c(X, Y), first)) %>% # add back X & Y for xyz fun
    ungroup

  xyz <- make.xyz(x = cellSummaries$X,
                  y = cellSummaries$Y,
                  z = cellSummaries %>% pull(zCol),
                  group = cellSummaries %>% pull(groupcol),
                  FUN = noquote(zFun)) # should do nothing, cellSummaries should be 1:1 size for xyz so no summarising in xyz needed

  png(filename = paste0(saveloc, plotname, ".png"),
      width = pngwidth, #NA default. Manually adjust plot box in RStudio after ggplot()
      height = pngheight, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
      units = "px",
      bg = "white",
      family = "",
      type = "cairo-png")
  plot(NA, # make extents, graphics::plot.default
       xlim = xlim,
       ylim = ylim,
       xlab = "longitude",
       ylab = "latitude",
       cex.axis = 2, # put this in 3 places and it still doesn't affect things. I think this works here but is then overwritten.
       cex.lab = 2,
       bg = 'white') # does nothing?
  plot.bathy(b, deep = -200, shallow = -200, step = 0, col = "black") # add 200m bathymetry contour
  draw.barplot2D(xyz$x, xyz$y, xyz$z,
                 width = cellsize[1], height = cellsize[2], scale = T, # scale = sum of Z values. Fine for count, CPUE, fish age?, but not good for depth, sst?
                 cex.axis = 2,
                 cex.lab = 2,
                 col = mycolours, col.frame = NA, lwd.frame = 1, silent = T, border = NA) # ... border density lwd
  plot(baseplot, # add coastline, overlaps & masks data on land
       type = "poly",
       xlim = xlim,
       ylim = ylim,
       col = 'grey',
       bg = 'white', # does nothing when added?
       cex.axis = 2,
       cex.lab = 2,
       add = TRUE)
  legend("topleft", legend = colnames(xyz$z), fill = mycolours, bg = "white", inset = 0.02,
         title = legendtitle, title.adj = 0)
  dev.off()
}

MarZonesTable <- data.frame(Name = c("WholeArea", "ForageW", "MixingW", "MixingWhotspot", "GSL", "CentralAtl", "Med", "GOM"),
                            lonmin = c(c(-100), c(-75), c(-82), c(-79), c(-70), c(-58), c(-6), c(-98)),
                            lonmax = c(c(40), c(-45), c(-45), c(-73), c(-55), c(-10), c(36), c(-80)),
                            latmin = c(c(10), c(39.3), c(23), c(31), c(45), c(32), c(31), c(18)),
                            latmax = c(c(65), c(50), c(39.3), c(36), c(51), c(55), c(44), c(30)),
                            pngwidth = c((1000/55*100), (1000/11*27), (1000/16*26), (1000/5*6), (1000/6*15), (1000/23*39), (1000/13*36), (1000/12*13)),
                            pngheight = c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000),
                            MarineZone = c(NA, "ForageW", "MixingW", "MixingW", "GSL", NA, "Med", "GOM"), # could use MarineZone as a filter for AllDailies
                            bathyres = c(2,1,1,1,1,1,1,1))

# Run function as loop, 1:12 months, 4 variables
for (j in c("Count")) { # ,  "MeanETemp24h", "MinETemp24h", "MeanDepth24h"
  for (i in 1:nrow(MarZonesTable)) { # i <- 8
    print(paste0(j, " ", i, "/", nrow(MarZonesTable), ", ", MarZonesTable$Name[i]))
    mycolours = c(CB_RED, CB_BLUE)
    if (MarZonesTable$Name[i] == "Med") mycolours = CB_BLUE # %in% c("CentralAtl", "Med")
    if (MarZonesTable$Name[i] == "GOM") mycolours = CB_RED

    for (k in 1:12) { # loop through months
      print(paste0("month ", k, "/12"))
      adminFilt <- AllDailies %>%
        filter(between(lon, MarZonesTable$lonmin[i], MarZonesTable$lonmax[i]),
               between(lat, MarZonesTable$latmin[i], MarZonesTable$latmax[i]),
               Month == k, # loop through months
               if (!is.na(MarZonesTable$MarineZone[i])) { # constrain to marzone if it's set, but for forageW also include GSL
                 if (MarZonesTable$MarineZone[i] == "ForageW") MarineZone %in% c("ForageW", "GSL") else MarineZone == MarZonesTable$MarineZone[i]
               } else TRUE)

      if (nrow(adminFilt) == 0) next
      mylejtitle <- paste0("N fishdays GOM: ", adminFilt %>% filter(Stock == "GOM") %>% nrow, # original data
                           ", Med: ", adminFilt %>% filter(Stock == "Med") %>% nrow)
      # mylejtitle <- paste0("N fishdays GOM: ", adminFilt %>% filter(Stock == "algorithmGOM") %>% nrow, # algorithm data
      #                      ", Med: ", adminFilt %>% filter(Stock == "algorithmMed") %>% nrow)
      if (MarZonesTable$Name[i] == "CentralAtl" & k %in% c(1, 2)) mycolours = CB_BLUE
      barplot2dMap(x = adminFilt,
                   cellsize = c(0.5, 0.5),
                   mycolours = mycolours,
                   legendtitle = mylejtitle,
                   zCol = j, # Count MeanETemp24h MeanDepth24h
                   zFun = "mean",
                   xlim = c(MarZonesTable$lonmin[i], MarZonesTable$lonmax[i]),
                   ylim = c(MarZonesTable$latmin[i], MarZonesTable$latmax[i]),
                   bathyres = MarZonesTable$bathyres[i],
                   pngwidth = MarZonesTable$pngwidth[i],
                   pngheight = MarZonesTable$pngheight[i],
                   saveloc = saveloc,
                   plotname = paste0(today(), "_2DBarplot_", j, "_", MarZonesTable$Name[i], "_Month", k)) # edited for loop through months
    } # close k loop through months
  } # close i
  beep(8) # announce completion
} # close j
# Don't run next section for algorithmGOM/Med

# 2D barplots SOG & Keys####
AllDailies$Marine_Zone <- AllDailies$MarineZone
AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37)), "Marine_Zone"] <- "SOG" # -6.5 -5, 35.5 36.5
AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5)), "Marine_Zone"] <- "Keys"
AllDailies[which(AllDailies$MarineZone == "Med"), "Marine_Zone"] <- "Med" # overwrite SoG box Med zone
AllDailies[which(AllDailies$MarineZone == "GOM"), "Marine_Zone"] <- "GOM" # overwrite Keys box GOM zone

# Run function as loop, all months together, Count variable only
for (i in 1:nrow(MarZonesTable)) { # i <- 8
  print(paste0(i, "/", nrow(MarZonesTable), ", ", MarZonesTable$Name[i]))
  mycolours = c(CB_RED, CB_BLUE)
  if (MarZonesTable$Name[i] == "Med") mycolours = c(CB_BLUE, "deepskyblue") # %in% c("CentralAtl", "Med")
  if (MarZonesTable$Name[i] == "GOM") mycolours = c(CB_RED, "violetred2")

  adminFilt <- AllDailies %>%
    filter(between(lon, MarZonesTable$lonmin[i], MarZonesTable$lonmax[i]),
           between(lat, MarZonesTable$latmin[i], MarZonesTable$latmax[i]))

  if (!is.na(MarZonesTable$MarineZone[i])) { # constrain to marzone if it's set, but for forageW also include GSL
    if (MarZonesTable$MarineZone[i] == "ForageW") adminFilt <- AllDailies %>% filter(MarineZone %in% c("ForageW", "GSL"))
    if (MarZonesTable$MarineZone[i] == "GOM") adminFilt <- AllDailies %>% filter(Marine_Zone %in% c("GOM", "Keys"))
    if (MarZonesTable$MarineZone[i] == "Med") adminFilt <- AllDailies %>% filter(Marine_Zone %in% c("Med", "SOG"))
    if (!MarZonesTable$MarineZone[i] %in% c("ForageW", "GOM", "Med")) MarineZone == MarZonesTable$MarineZone[i]
  }

  if (nrow(adminFilt) == 0) next
  mylejtitle <- paste0("N fishdays GOM: ", adminFilt %>% filter(Stock == "GOM") %>% nrow,
                       ", Med: ", adminFilt %>% filter(Stock == "Med") %>% nrow)
  if (MarZonesTable$MarineZone[i] == "GOM") mylejtitle <- paste0("N fishdays GOM: ", adminFilt %>% filter(Marine_Zone == "GOM") %>% nrow,
                                                                 ", Keys: ", adminFilt %>% filter(Marine_Zone == "Keys") %>% nrow)
  if (MarZonesTable$MarineZone[i] == "Med") mylejtitle <- paste0("N fishdays Med: ", adminFilt %>% filter(Marine_Zone == "Med") %>% nrow,
                                                                 ", SoG: ", adminFilt %>% filter(Marine_Zone == "SOG") %>% nrow)
  barplot2dMap(x = adminFilt,
               cellsize = c(0.5, 0.5),
               groupcol = if (MarZonesTable$MarineZone[i] %in% c("GOM", "Med")) "Marine_Zone" else "Stock",
               mycolours = mycolours,
               legendtitle = mylejtitle,
               zFun = "mean",
               xlim = c(MarZonesTable$lonmin[i], MarZonesTable$lonmax[i]),
               ylim = c(MarZonesTable$latmin[i], MarZonesTable$latmax[i]),
               bathyres = MarZonesTable$bathyres[i],
               pngwidth = MarZonesTable$pngwidth[i],
               pngheight = MarZonesTable$pngheight[i],
               saveloc = saveloc,
               plotname = paste0(today(), "_2DBarplot_Count_", MarZonesTable$Name[i])) # edited for loop through months
} # close i

# Need to add "in keys entry box but not in GOM" (& same for med)








# Fig 4 etc ViolinPlots & ppgHistos FIGURE8 & SM####
setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/ByZoneGroupStock"))

if (min(AllDailies$MeanDepth24h, na.rm = T) >= 0) AllDailies$MeanDepth24h <- -AllDailies$MeanDepth24h # make depths negative
if (min(AllDailies$MaxDepth24h, na.rm = T) >= 0) AllDailies$MaxDepth24h <- -AllDailies$MaxDepth24h # make depths negative
if (min(AllDailies$OceanDepth, na.rm = T) >= 0) AllDailies$OceanDepth <- -AllDailies$OceanDepth # same for oceandepth
AllDailies$EddySpeedAmp <- AllDailies$speed_average * AllDailies$cyclonicAmp
myzones <- c(unique(AllDailies$MarineZone), "MixingWhotspot", "Keys", "SOG") # "MixingW" "ForageW" "GOM"     "MixingE" "ForageE" "Med"     "GSL"
myvars <- c("OceanDepth", "MaxDepth24h", "MinETemp24h", "MeanETemp24h", "li5day", "speed_average", "cyclonicAmp", "EddySpeedAmp", "DistanceToShoreKm", "lat", "Hrs50mLesDepRange", "SurfFreq24h", "StepLengthBL", "MeanDepth24h", "ild_dive_cnt_desc_gl_Sum")
myvarnames <- c("Ocean Depth (m)", "Daily Maximum Depth (m)", "Daily Minimum External Temperature (°C)", "Daily Mean External Temperature (°C)", "5-day linearity index",
                "Average Speed (cm/s)", "Cyclonic Amplitude", "Eddy Speed * Amplitude", "Distance To Shore (Km)",
                "Latitude (°N)", "Hours w/ <=50m Depth Range", "Surface Frequency (<10m)", "Daily travel (body lengths)", "Daily mean depth (m)", "Dives through thermocline")

for (i in myzones) {
  dir.create(i)
  setwd(i)
  for (j in 1:length(myvars)) {
    ADsub <- AllDailies %>% filter(MarineZone == i) # will silently fail by removing all rows if i isn't a named MarineZone i.e. "MixingWhotspot", "Keys", "SOG"
    if (i == "MixingWhotspot") ADsub <- AllDailies[which(between(AllDailies$lon, -79, -73) & between(AllDailies$lat, 31, 36)), ]
    if (i == "Keys") ADsub <- AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5)), ]
    if (i == "SOG") ADsub <- AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37)), ]
    ggplot(ADsub,
           aes(x = Stock, y = .data[[myvars[j]]])) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
      geom_violin(aes(fill = Stock, colour = Stock)) +
      stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
      labs(y = myvarnames[j]) +
      scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
      # bar some exceptions where the defaults are bad e.g. rounding scales which makes 2500 be 2k
      {if (
        ((i == "MixingW" | i == "MixingE") & j == 9) # dist to shore
        |
        (i == "MixingW" & j == 2) # max depth
        |
        ((i == "MixingW" | i == "MixingE" | i == "ForageW") & (j == 7 | j == 8)) # "cyclonicAmp", "EddySpeedAmp"
      ) scale_y_continuous(label = scales::label_number_si(accuracy = 0.1)) } + # scales package. use million billion suffixes vs 1e10 etc.
      {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                  breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                  labels = c("0", "0.25", "0.5", "0.75", "1"))} +
      {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                                breaks = seq(from = 0, to = 24, by = 3),
                                                                labels = as.character(seq(from = 0, to = 24, by = 3)))} +
      scale_colour_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
      {if (i == "SOG" | i == "Med") scale_colour_manual(values = c(CB_BLUE))} + # if only med stock, only red. If only GOM, will use first value which is blue which is right
      scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
      {if (i == "SOG" | i == "Med") scale_fill_manual(values = c(CB_BLUE))} +
      theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                       title = element_text(size = rel(2)),
                                       legend.position = "none",
                                       panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      ggsave(paste0(today(), "_Stock_", myvars[j], ".png"),
             plot = last_plot(), device = "png", scale = 1.75, width = 4,
             height = 6, units = "in", dpi = 300, limitsize = TRUE)
  } # close j
  setwd("../") # back to root for next folder of i
} # close i




# Fig 4 etc1 GSL plots halfsize Yaxis####
myvars <- c("OceanDepth", "MaxDepth24h", "MinETemp24h")
myvarnames <- c("Ocean Depth (m)", "Daily Maximum Depth (m)", "Daily Minimum External Temperature (°C)")
setwd("GSL")
for (j in 1:length(myvars)) {
  ADsub <- AllDailies %>% filter(MarineZone == "GSL") # will silently fail by removing all rows if i isn't a named MarineZone i.e. "MixingWhotspot", "Keys", "SOG"
  ggplot(ADsub, aes(x = Stock, y = .data[[myvars[j]]])) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
    geom_violin(aes(fill = Stock, colour = Stock)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(y = myvarnames[j]) +
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    # bar some exceptions where the defaults are bad e.g. rounding scales which makes 2500 be 2k
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
    scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_Stock_", myvars[j], ".png"),
           plot = last_plot(), device = "png", scale = 1.75, width = 4,
           height = 3, units = "in", dpi = 300, limitsize = TRUE)
} # close j
setwd("../") # back to root for next folder of i





# Fig 4 etc1 WMix plots halfsize Yaxis####
myvars <- c("li5day")
myvarnames <- c("5-day linearity index")
setwd("MixingW")
for (j in 1:length(myvars)) {
  ADsub <- AllDailies %>% filter(MarineZone == "MixingW") # will silently fail by removing all rows if i isn't a named MarineZone i.e. "MixingWhotspot", "Keys", "SOG"
  ggplot(ADsub, aes(x = Stock, y = .data[[myvars[j]]])) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
    geom_violin(aes(fill = Stock, colour = Stock)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(y = myvarnames[j]) +
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    # bar some exceptions where the defaults are bad e.g. rounding scales which makes 2500 be 2k
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
    scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_Stock_", myvars[j], "_halfheight.png"),
           plot = last_plot(), device = "png", scale = 1.75, width = 4,
           height = 3, units = "in", dpi = 300, limitsize = TRUE)
} # close j
setwd("../") # back to root for next folder of i



# Fig 4 etc2 SOG & Keys ViolinPlots####
# SOG & Keys MaxDepth & MinETemp
source('~/Dropbox/Blocklab Monterey/Blocklab/recurve_scales.R')
AllDailies$Marine_Zone <- as.character(rep("Other", nrow(AllDailies)))
AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37)), "Marine_Zone"] <- "SOG" # -6.5 -5, 35.5 36.5
AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5)), "Marine_Zone"] <- "Keys"
#
if (min(AllDailies$MaxDepth24h, na.rm = T) >= 0) AllDailies$MaxDepth24h <- -AllDailies$MaxDepth24h # make depths negative
myseq <- c(-1250, -1000, -750, seq(-500, -100, by = 100), -50, -25, -10, -5, -1)

# Keys GOM
dir.create("Keys_GOM")
setwd("Keys_GOM")
for (j in 1:length(myvars)) {
  ggplot(AllDailies %>% filter(Stock == "GOM"), aes(x = factor(Marine_Zone, levels = c("Keys", "Other")), y = .data[[myvars[j]]])) +
    geom_violin(aes(fill = Marine_Zone, colour = Marine_Zone)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(x = "Marine Zone", y = myvarnames[j]) +
    scale_x_discrete(labels = c("Keys", "Other")) + # see above, adds N to names
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    {if (j %in% c(2, 9, 13)) scale_y_continuous(label = scales::label_number_si(accuracy = 0.1)) } + # max depth, dst, step length
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c("violetred2", CB_RED)) + # manually colour plots if specified
    scale_fill_manual(values = c("violetred2", CB_RED)) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_Stock_", myvars[j], ".png"), plot = last_plot(), device = "png", scale = 1.75, width = 6,
           height = 6, units = "in", dpi = 300, limitsize = TRUE)
} # close j



# SOG Med
dir.create("../SOG_Med/")
setwd("../SOG_Med/")
for (j in 1:length(myvars)) {
  ggplot(AllDailies %>% filter(Stock == "Med"), aes(x = factor(Marine_Zone, levels = c("SOG", "Other")), y = .data[[myvars[j]]])) +
    geom_violin(aes(fill = Marine_Zone, colour = Marine_Zone)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(x = "Marine Zone", y = myvarnames[j]) +
    scale_x_discrete(labels = c("SOG", "Other")) + # see above, adds N to names
    # rounding scales which makes 2500 be 2k
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    # bar some exceptions where the defaults are bad
    {if (j %in% c(2, 7:9, 13)) scale_y_continuous(label = scales::label_number_si(accuracy = 0.1)) } + # max depth, eddies, dist to shore, step length
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c(CB_BLUE, "deepskyblue")) + # manually colour plots if specified
    scale_fill_manual(values = c(CB_BLUE, "deepskyblue")) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_Stock_", myvars[j], ".png"), plot = last_plot(), device = "png", scale = 1.75, width = 6,
           height = 6, units = "in", dpi = 300, limitsize = TRUE)
} # close j


# GOM Med combined
dir.create("../GOM_Med/")
setwd("../GOM_Med/")
for (j in 1:length(myvars)) {
  ggplot(AllDailies %>% filter(MarineZone == "Med" | MarineZone == "GOM"), aes(x = factor(MarineZone, levels = c("GOM", "Med")), y = .data[[myvars[j]]])) +
    geom_violin(aes(fill = MarineZone, colour = MarineZone)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(x = "Marine Zone", y = myvarnames[j]) +
    scale_x_discrete(labels = c("GOM", "Med")) + # see above, adds N to names
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    {if (j %in% c(2, 7:9, 13)) scale_y_continuous(label = scales::label_number_si(accuracy = 0.1)) } + # max depth, eddies, dist to shore, step length
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
    scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_Stock_", myvars[j], ".png"), plot = last_plot(), device = "png", scale = 1.75, width = 6,
           height = 6, units = "in", dpi = 300, limitsize = TRUE)
} # close j



# Fig 4 etc3 SGs vs Mixing Ground ViolinPlots####
if (min(AllDailies$MeanDepth24h, na.rm = T) >= 0) AllDailies$MeanDepth24h <- -AllDailies$MeanDepth24h # make depths negative
if (min(AllDailies$MaxDepth24h, na.rm = T) >= 0) AllDailies$MaxDepth24h <- -AllDailies$MaxDepth24h # make depths negative
if (min(AllDailies$OceanDepth, na.rm = T) >= 0) AllDailies$OceanDepth <- -AllDailies$OceanDepth # same for oceandepth
AllDailies$EddySpeedAmp <- AllDailies$speed_average * AllDailies$cyclonicAmp
myseq <- c(-1250, -1000, -750, seq(-500, -100, by = 100), -50, -25, -10, -5, -1)
AllDailies[which(AllDailies$MarineZone == "GOM"), "MarineZone2"] <- "GOM"
AllDailies[which(AllDailies$MarineZone == "Med"), "MarineZone2"] <- "Med"
AllDailies[which(between(AllDailies$lon, -79, -73) & between(AllDailies$lat, 31, 36)), "MarineZone2"] <- "MixingWhotspot"

# MixingWhotspot GOM
dir.create("../MixingWhotspot_GOM/")
setwd("../MixingWhotspot_GOM/")
for (j in 1:length(myvars)) {
  ggplot(AllDailies %>% filter(MarineZone2 %in% c("MixingWhotspot", "GOM"), Stock == "GOM"), aes(x = MarineZone2, y = .data[[myvars[j]]])) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
    geom_violin(aes(fill = MarineZone2, colour = MarineZone2)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(y = myvarnames[j]) +
    # except for MixingW EddySpeedAmp, urn off rounding scales which makes 2500 be 2k
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c(CB_BLUE, "deepskyblue")) + # manually colour plots if specified
    scale_fill_manual(values = c(CB_BLUE, "deepskyblue")) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_GOM_vs_MixingW_", myvars[j], ".png"),
           plot = last_plot(), device = "png", scale = 1.75, width = 4,
           height = 6, units = "in", dpi = 300, limitsize = TRUE)
} # close j

# MixingWhotspot Med
dir.create("../MixingWhotspot_Med/")
setwd("../MixingWhotspot_Med/")
for (j in 1:length(myvars)) {
  ggplot(AllDailies %>% filter(MarineZone2 %in% c("MixingWhotspot", "Med"), Stock == "Med"), aes(x = MarineZone2, y = .data[[myvars[j]]])) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
    geom_violin(aes(fill = MarineZone2, colour = MarineZone2)) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
    labs(y = myvarnames[j]) +
    # except for MixingW EddySpeedAmp, urn off rounding scales which makes 2500 be 2k
    scale_y_continuous(label = scales::label_number_si()) + # scales package. use million billion suffixes vs 1e10 etc.
    {if (myvars[j] == "li5day" | myvars[j] == "SurfFreq24h") scale_y_continuous(limits = c(0,1),
                                                                                breaks = c(0, 0.25, 0.5, 0.75, 1),
                                                                                labels = c("0", "0.25", "0.5", "0.75", "1"))} +
    {if (myvars[j] == "Hrs50mLesDepRange") scale_y_continuous(limits = c(0, 24),
                                                              breaks = seq(from = 0, to = 24, by = 3),
                                                              labels = as.character(seq(from = 0, to = 24, by = 3)))} +
    scale_colour_manual(values = c(CB_RED, "violetred2")) + # manually colour plots if specified
    scale_fill_manual(values = c(CB_RED, "violetred2")) + # manually colour plots if specified
    theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                     title = element_text(size = rel(2)),
                                     legend.position = "none",
                                     panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggsave(paste0(today(), "_Med_vs_MixingW_", myvars[j], ".png"),
           plot = last_plot(), device = "png", scale = 1.75, width = 4,
           height = 6, units = "in", dpi = 300, limitsize = TRUE)
} # close j





# Fig 4 extras: Percent-per-Group histograms####
if (min(AllDailies$MaxDepth24h, na.rm = T) < 0) AllDailies$MaxDepth24h <- -AllDailies$MaxDepth24h # make depths positive
# GSL: ppgHisto maxdepth @ 90
source('~/Dropbox/Blocklab Monterey/Blocklab/ppgHisto.R')
setwd("../GSL/")

ppgHisto(x = AllDailies %>% filter(!is.na(MaxDepth24h), MarineZone == "GSL"),
         colname = "MaxDepth24h", # character. named variable column within x,
         cutbreaks = c(0, 90, Inf), # vector of numeric breaks for colname variable bins
         cutlabels = c("0-90", "90-500"), # vector of bin names corresponding to cutbreaks, one fewer
         groupcol = Stock,
         groupcolours = c(CB_RED, CB_BLUE)) +
  ggsave(paste0(today(), "_ppgHisto_MaxDepth24h_90m.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 4,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)

AllDailies %>% filter(Stock == "GOM", !is.na(MaxDepth24h), MarineZone == "GSL", MaxDepth24h > 90) %>% nrow # 29
AllDailies %>% filter(Stock == "Med", !is.na(MaxDepth24h), MarineZone == "GSL", MaxDepth24h > 90) %>% nrow # 118
# 118/29 # 4.07 times more Med at these depths than GOM


# MixingW: speed_average, cyclonicAmp, speed/amp combo
setwd("../MixingW/")
ppgHisto(x = AllDailies %>% filter(!is.na(speed_average), MarineZone == "MixingW"),
         colname = "speed_average", # character. named variable column within x,
         cutbreaks = c(-Inf, 75, Inf), # vector of numeric breaks for colname variable bins
         cutlabels = c("<75", ">75"), # vector of bin names corresponding to cutbreaks, one fewer
         groupcol = Stock,
         groupcolours = c(CB_RED, CB_BLUE)) +
  ggsave(paste0(today(), "_ppgHisto_speed_average_75cms.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 4,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)
(AllDailies %>% filter(Stock == "Med", !is.na(speed_average), MarineZone == "MixingW", speed_average > 75) %>% nrow) / # 118
  (AllDailies %>% filter(Stock == "GOM", !is.na(speed_average), MarineZone == "MixingW", speed_average > 75) %>% nrow) # 29
# 2.16X more Med at this speed average than GOM

ppgHisto(x = AllDailies %>% filter(!is.na(cyclonicAmp), MarineZone == "MixingW"),
         colname = "cyclonicAmp", # character. named variable column within x,
         cutbreaks = c(-Inf, -35, Inf), # vector of numeric breaks for colname variable bins
         cutlabels = c("<-35", ">-35"), # vector of bin names corresponding to cutbreaks, one fewer
         groupcol = Stock,
         groupcolours = c(CB_RED, CB_BLUE)) +
  ggsave(paste0(today(), "_ppgHisto_cyclonicAmp_-35cm.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 4,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)
(AllDailies %>% filter(Stock == "Med", !is.na(cyclonicAmp), MarineZone == "MixingW", cyclonicAmp < -35) %>% nrow) / # 118
  (AllDailies %>% filter(Stock == "GOM", !is.na(cyclonicAmp), MarineZone == "MixingW", cyclonicAmp < -35) %>% nrow) # 29
# 2.185X more Med at this cyclonicAmp than GOM

ppgHisto(x = AllDailies %>% filter(!is.na(EddySpeedAmp), MarineZone == "MixingW"),
         colname = "EddySpeedAmp", # character. named variable column within x,
         cutbreaks = c(-Inf, -2625, Inf), # vector of numeric breaks for colname variable bins
         cutlabels = c("<-2625", ">-2625"), # vector of bin names corresponding to cutbreaks, one fewer
         groupcol = Stock,
         groupcolours = c(CB_RED, CB_BLUE)) +
  ggsave(paste0(today(), "_ppgHisto_EddySpeedAmp_-2625cms2.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 4,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)
(AllDailies %>% filter(Stock == "Med", !is.na(EddySpeedAmp), MarineZone == "MixingW", EddySpeedAmp < -2625) %>% nrow) / # 118
  (AllDailies %>% filter(Stock == "GOM", !is.na(EddySpeedAmp), MarineZone == "MixingW", EddySpeedAmp < -2625) %>% nrow) # 29
# 2.22X more Med at this EddySpeedAmp than GOM
# not 2X more Med in absolute numbers: proportion of Med fish in this (vs Med fish in the other bin) is 2X the proportion of GOM fish in this bin (vs GOM fish in the other bin)







# Fig SM body size & age violins ####
saveloc <- "/home/simon/Documents/Si Work/Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/"
ggplot(AllDailies,
       aes(x = Stock, y = FishLengthCm)) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
  geom_violin(aes(fill = Stock, colour = Stock)) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
  labs(y = "Fish Length (cm)") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_Stock_FishLengthCM.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 4,
         height = 6, units = "in", dpi = 300, limitsize = TRUE)

ggplot(AllDailies,
       aes(x = Stock, y = age)) + # https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
  geom_violin(aes(fill = Stock, colour = Stock)) +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, stroke = 2, colour = "white") +
  labs(y = "Fish Age (years)") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_Stock_FishAgeYears.png"),
         plot = last_plot(), device = "png", scale = 1.75, width = 4,
         height = 6, units = "in", dpi = 300, limitsize = TRUE)




# Fig SM was 5 ILD & dives thru####
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/") #ensure trailing /slash

ggplot(AllDailies %>% filter(MarineZone == "GSL"), aes(x = Stock, y = ild_dive_cnt_desc_gl_Sum)) +
  geom_boxplot(aes(fill = Stock), colour = "black") +
  ggtitle(paste0("Daily dives through ILD (GSL)"),
          subtitle = paste0("GOM ndays=", AllDailies %>% filter(MarineZone == "GSL", Stock == "GOM") %>% nrow, ", ",
                            "Med n=", AllDailies %>% filter(MarineZone == "GSL", Stock == "Med") %>% nrow)) +
  labs(x = "Stock", y = "Daily ILD Dives") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("GOM", "Med")) + # manually relabel to remove underscores
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_ILDDivesBoxGOMMed.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4.5,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

if (min(AllDailies$MeanILD24h, na.rm = T) > 0) AllDailies$MeanILD24h <- -AllDailies$MeanILD24h # make depths negative

ggplot(AllDailies %>% filter(MarineZone == "GSL"), aes(x = Stock, y = MeanILD24h)) +
  geom_boxplot(aes(fill = Stock), colour = "black") +
  ggtitle(paste0("Mean daily ILD (GSL)"),
          subtitle = paste0("GOM ndays=", AllDailies %>% filter(MarineZone == "GSL", Stock == "GOM") %>% nrow, ", ",
                            "Med n=", AllDailies %>% filter(MarineZone == "GSL", Stock == "Med") %>% nrow)) +
  labs(x = "Stock", y = "Intermediate layer depth (m)") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("GOM", "Med")) + # manually relabel to remove underscores
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_ILDGOMMedGSL.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4.5,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)






# Fig 6? GSL/Northernmost to SG journey time####
AllDailies$toppid2 <- factor(paste0(str_sub(as.character(as.Date(str_sub(AllDailies$toppid, 3, 4), format = "%y")), 1, 4), # 2 digit year to Date, adds M & D, only keep YYYY
                                    str_sub(AllDailies$toppid, 5, 7),  # add back the final 3
                                    "_", AllDailies$Year)) # add year for the year of data, not deploy

# GSL TO GOM
# max yday in GSL per toppid2 = last day of the year in GSL, per toppid2 i.e. per fish per year
# what if a fish leaves GSL on 1st Jan, returns autumn, leaves again 31st Dec? 1st Jan season is ignored.
leftGSL <- AllDailies %>%
  filter(Stock == "GOM", MarineZone == "GSL") %>%
  group_by(toppid2) %>%
  summarise(max(Day, na.rm = T))

# 2020.08.11 convert to yday of northernmost point that year,
# Need a split day to divide years, i.e. not 1st Jan. Day 80 looks good by eye.
# So 80 this year all the way round to 80 next year, is one seasonal year
AllDailies$Year2 <- ifelse(AllDailies$Day <= 80, # make a new year based on dates
                           AllDailies$Year - 1, # year <= 80 = year-1
                           AllDailies$Year) # year > 80 = year
AllDailies$toppid3 <- factor(paste0(str_sub(as.character(as.Date(str_sub(AllDailies$toppid, 3, 4), format = "%y")), 1, 4), # 2 digit year to Date, adds M & D, only keep YYYY
                                    str_sub(AllDailies$toppid, 5, 7),  # add back the final 3
                                    "_", AllDailies$Year2)) # add year for the year of data, not deploy
northernmost <- AllDailies %>%
  filter(Stock == "GOM",
         lat >= 39.3) %>% # >= ForageW lat (39.3), might as well get rid of these values now
  group_by(toppid3) %>%
  summarise(MaxLat = max(lat, na.rm = T), # max lat
            MaxLatDay = Day[which(lat == max(lat, na.rm = T))]) # Day corresponding to the highest lat


# arrived in GOM that year
arrivedGOM <- AllDailies %>%
  filter(Stock == "GOM", MarineZone == "GOM", Day > 274) %>%
  group_by(toppid2) %>%
  summarise(arrivedGOMdate = min(Day, na.rm = T)) # day later than Oct, by eye on purple marinearea stock plots from above
# By doing this in calendar years, fish that first arrive after Jan1 are excluded
# Need to split years at 1 July

names(leftGSL)[2] <- "leftGSLdate"
names(northernmost)[3] <- "leftNorthdate"
mean(arrivedGOM$arrivedGOMdate) # 341.5455

GSLtoGOM <- left_join(leftGSL, arrivedGOM) %>% # Joining, by = "toppid2"
  drop_na(arrivedGOMdate) %>%
  mutate(GSLtoGOMdays = arrivedGOMdate - leftGSLdate)
mean(GSLtoGOM$GSLtoGOMdays) # mean GSL to GOM travel days = 55.68. n = 20
# GOM arrivals that year i.e. oct to Dec31, not including fish who got to GOM after 31st Dec, in the next year.

NorthtoGOM <- left_join(northernmost, arrivedGOM, by = c("toppid3" = "toppid2")) %>% # Joining, by = "toppid2"
  drop_na(arrivedGOMdate) %>%
  mutate(NorthtoGOMdays = arrivedGOMdate - leftNorthdate)
mean(NorthtoGOM$leftNorthdate) # mean North to GOM travel days = 278.9091 n = 21
# is arrivedGOM$toppid2 comparable to northernmost$toppid3? YES: arrivedGOM is filtered for Day > 274 which is >80 so year is the same



# get first gom date for the toppids' next years
# leftGSL$toppid2 # remove factor, convert to numeric, add 1,
leftGSLlist <- as.character(leftGSL$toppid2)
leftGSLlistDF <- as.data.frame(str_split_fixed(string = leftGSLlist, pattern = "_", n = 2))
leftGSLlistDF[,2] <- as.numeric(leftGSLlistDF[,2])
leftGSLlistDF$NextYear <- leftGSLlistDF[,2] + 1
leftGSLlistDF$toppid2new <- paste0(leftGSLlistDF[,1], "_", leftGSLlistDF$NextYear) # that's your list of toppid2's to scan for as min day in GOM
leftGSLlistDF$toppid2 <- paste0(leftGSLlistDF[,1], "_", leftGSLlistDF[,2]) # rebuild toppid2 for left_join later

leftNorthlist <- as.character(northernmost$toppid3)
leftNorthlistDF <- as.data.frame(str_split_fixed(string = leftNorthlist, pattern = "_", n = 2))
leftNorthlistDF[,2] <- as.numeric(leftNorthlistDF[,2])
leftNorthlistDF$NextYear <- leftNorthlistDF[,2] + 1
leftNorthlistDF$toppid3new <- paste0(leftNorthlistDF[,1], "_", leftNorthlistDF$NextYear) # that's your list of toppid2's to scan for as min day in GOM
leftNorthlistDF$toppid3 <- paste0(leftNorthlistDF[,1], "_", leftNorthlistDF[,2]) # rebuild toppid2 for left_join later

arrivedGOM2 <- AllDailies %>%
  filter(Stock == "GOM",
         MarineZone == "GOM",
         # toppid2 %in% leftGSLlistDF$toppid2new, # GSL, n=51
         toppid2 %in% leftNorthlistDF$toppid3new, # North. n=59
         Day < 182) %>% # select fishyears for the next year (toppid2new)
  group_by(toppid2) %>%
  summarise(min(Day, na.rm = T)) # earliest day in GOM before DATE, after which all are assumed to have left;
# Don't want to capture fish who went GSL, mixingground, GSL, GOM, and sum from GSL1 to GOM, i.e. across 2 cycles.
# 1st July looks good. yday(as.Date("2018-07-01")) 182
names(arrivedGOM2)[2] <- "arrivedGOMdate"
# need to left join but initially remove those which were already matched
GSLtoGOM2 <- left_join(leftGSL, arrivedGOM) %>%
  filter(is.na(arrivedGOMdate)) %>% # need to KEEP ONLY the NAs
  select(-arrivedGOMdate) # n = 51
# left join won't work, need to match toppid2 but arrivedGOM2$toppid2 is the next year
NorthtoGOM2 <- left_join(northernmost, arrivedGOM, by = c("toppid3" = "toppid2")) %>%
  filter(is.na(arrivedGOMdate)) %>% # need to KEEP ONLY the NAs
  select(-arrivedGOMdate) # n = 78

# need to subtract 1 from arrivedGOM2$toppid2 first
arrivedGOM2 %<>% mutate(toppid2 = as.character(toppid2))
arrivedGOM2listDF <- as.data.frame(str_split_fixed(string = arrivedGOM2$toppid2, pattern = "_", n = 2))
arrivedGOM2listDF[,2] <- as.numeric(arrivedGOM2listDF[,2])
# arrivedGOM2listDF[,1] <- as.character(levels(arrivedGOM2listDF[,1]))[arrivedGOM2listDF[,1]]
arrivedGOM2$toppid2 <- paste0(arrivedGOM2listDF[,1], "_", arrivedGOM2listDF[,2] - 1)
# get mean GOM arrival date from arrivedGOM & arrivedGOM2. Need to account for yearday circularity
# add 365 to arrivedGOM2 so numberline goes ...363,364,365,366,367,368... not 1,2,3,363,364,365
# concatenate with arrivedGOM$arrivedGOMdate,
# take mean & subtract 365
mean(c((arrivedGOM2$arrivedGOMdate + 365), arrivedGOM$arrivedGOMdate)) - 365 # 22.67059


GSLtoGOM2 %<>%
  left_join(arrivedGOM2) %>% # Joining, by = "toppid2"
  mutate(GSLtoGOMdays = arrivedGOMdate + (366 - leftGSLdate)) # n = 51
NorthtoGOM2 %<>%
  left_join(arrivedGOM2, by = c("toppid3" = "toppid2")) %>% # Joining, by = "toppid2"
  mutate(NorthtoGOMdays = arrivedGOMdate + (366 - leftNorthdate)) # n = 78

GSLtoGOMall <- rbind(GSLtoGOM, GSLtoGOM2) %>% drop_na(GSLtoGOMdays) # n = 58
NorthtoGOMall <- rbind(NorthtoGOM, NorthtoGOM2) %>% drop_na(NorthtoGOMdays) # n = 67

mean(GSLtoGOMall$GSLtoGOMdays) # mean GSL to GOM travel days = 105. n = 58
# max 214 days, /home/simon/Documents/Si Work/Blocklab/abft_diving/X_PlotsMisc/Tracks/Gifs/GOM/5118006_18P0647_CA18_P21P.gif seems legit
mean(NorthtoGOMall$NorthtoGOMdays) # 109.6716
# Doesn't account for the unequal lats/distances travelled from GOM (static) to each individual fishes highest lat that year (dynamic)
# do lat difference to 25N, bottom of everglades / mid keys.
NorthtoGOMall$LatDiff <- NorthtoGOMall$MaxLat - 25
NorthtoGOMall$JourneySpeedDegDay <- NorthtoGOMall$LatDiff / NorthtoGOMall$NorthtoGOMdays

NorthtoGOMall %<>%
  rename(leftdate = leftNorthdate,
         arriveddate = arrivedGOMdate,
         JourneyDays = NorthtoGOMdays) %>%
  mutate(Direction = "North_to_GOM")


# GOM TO GSL
# first GSL day
arrivedGSL <- AllDailies %>%
  filter(Stock == "GOM", MarineZone == "GSL") %>%
  group_by(toppid2) %>%
  summarise(min(Day, na.rm = T)) # n=63, values 170:301
# could be the date of tagging within the GSL though!
names(arrivedGSL)[2] <- "arrivedGSLdate"
# last GOM day before that
leftGOM <- AllDailies %>%
  filter(Stock == "GOM",
         MarineZone == "GOM",
         Day < 182) %>%  # same as above
  group_by(toppid2) %>%
  summarise(max(Day, na.rm = T))
names(leftGOM)[2] <- "leftGOMdate"
# highest value is 181, v close to 182, surprising??
# Leaving GOM means MarineZone == GOM on day 1, and MarineZone == CentralWAtl on day 2
# Use RLE to find these. Or just use this process directly for GSL:
# North-WAtl day 1, GSL day 2.
# NB: the code in the succeeding section is horrible but I was coming down with a cold
AllDailies$Index <- 1:nrow(AllDailies) # rebuild index post filtering at top
GSLrle <- data.frame(unclass(rle(AllDailies$MarineZone)))
GSLrle$cumlengths <- cumsum(GSLrle$lengths)
GSLrle$valueslag <- c(GSLrle$values[2:length(GSLrle$values)], NA) # create lag column
GSLrle$valueslagdate <- AllDailies$Date[GSLrle$cumlengths + 1] # add dates
GSLrle$valueslagindex <- AllDailies$Index[GSLrle$cumlengths + 1] # add iNDEX
GSLrle <- GSLrle[1:(nrow(GSLrle) - 1), ] # remove last row
GSLrle %<>% unite("valuecombo", c(values, valueslag), remove = FALSE) # create combo column
GSLrle %<>% filter(valuecombo == "ForageW_GSL")
GSLrle$valuesindex <- GSLrle$valueslagindex - 1 # create lag column for index

ADgslEntry <- AllDailies %>% # check: filter alldailies by those rows
  select(Date, Stock, toppid, MarineZone, Index, Day) %>%
  filter(Index %in% c(GSLrle$valuesindex, GSLrle$valueslagindex))
# toppid pairs have to be the same
ADgslEntry$toppidlag <- c(ADgslEntry$toppid[2:length(ADgslEntry$toppid)], NA) # create toppid lag column
ADgslEntry$Datelag <- c(ADgslEntry$Date[2:length(ADgslEntry$Date)], NA) # create Date lag column
ADgslEntry$Daylag <- c(ADgslEntry$Day[2:length(ADgslEntry$Day)], NA) # create Day lag column
ADgslEntry$Indexlag <- c(ADgslEntry$Index[2:length(ADgslEntry$Index)], NA) # create Day lag column
ADgslEntry %<>% filter(MarineZone == "ForageW") # remove even columns
ADgslEntry %<>% select(Date, Datelag, Stock, toppid, toppidlag, Day, Daylag, Index, Indexlag) # tidy
ADgslEntry$toppidmatch <- ADgslEntry$toppid == ADgslEntry$toppidlag # logical
ADgslEntry %<>% filter(toppidmatch)
ADgslEntry$daylagmatch <- ADgslEntry$Daylag == ADgslEntry$Day + 1 # logical
# 3 daylagmatch are FALSE but their indexes are only 1 different so these are fine.
mean(ADgslEntry$Daylag) # 238.8077 # 27th August
# need to do this on a per-stock basis
ADgslEntry %>%
  group_by(Stock) %>%
  summarise(MeanGSLentry = mean(Daylag)) # 239 for both. Check correct:
# tmp <- ADgslEntry %>% filter(Stock == "Med")
# mean(tmp$Daylag) # 238.5882 GOM, 239.2222 Med. Is correct.

GOMtoGSL <- left_join(leftGOM, arrivedGSL) %>%
  drop_na(arrivedGSLdate) %>%
  mutate(GOMtoGSLdays = arrivedGSLdate - leftGOMdate)
mean(GOMtoGSL$GOMtoGSLdays) # mean GOM to GSL travel days = 52. n = 8

GOMtoNorth <- left_join(leftGOM, northernmost, by = c("toppid2" = "toppid3")) %>%
  # OK to use toppid2 against toppid3 if ALL northernmost$leftNorthdate > 80 which these are.
  drop_na(leftNorthdate) %>%
  mutate(GOMtoNorthdays = leftNorthdate - leftGOMdate)
GOMtoNorth %<>% filter(GOMtoNorthdays >= 0) # 5103535 only gets to 40 lat & heads south @ day 120 2004, then later leaves GOM @ day 167 same year (then tags died), hence -47 day commute
GOMtoNorth$LatDiff <- GOMtoNorth$MaxLat - 25
GOMtoNorth$JourneySpeedDegDay <- GOMtoNorth$LatDiff / GOMtoNorth$GOMtoNorthdays
GOMtoNorth %<>%
  select(toppid2, MaxLat, leftGOMdate, leftNorthdate, GOMtoNorthdays, LatDiff, JourneySpeedDegDay) %>%
  rename(leftdate = leftGOMdate,
         arriveddate = leftNorthdate,
         JourneyDays = GOMtoNorthdays) %>%
  mutate(Direction = "GOM_to_North")
mean(GOMtoNorth$JourneyDays) # mean GOM to GSL travel days = 59.2963 n = 27



GSLtoGOMall %<>%
  select(toppid2, GSLtoGOMdays) %>%
  rename(JourneyDays = GSLtoGOMdays) %>%
  mutate(Direction = "GSL_to_GOM")
GOMtoGSL %<>%
  select(toppid2, GOMtoGSLdays) %>%
  rename(JourneyDays = GOMtoGSLdays) %>%
  mutate(Direction = "GOM_to_GSL")


AllJourneys <- rbind(GSLtoGOMall, GOMtoGSL) # n = 66
AllJourneysNorth <- rbind(NorthtoGOMall %>% rename(toppid2 = toppid3), # n = 94
                          GOMtoNorth)
write.csv(AllJourneys, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_GSL_JourneyDays.csv", row.names = F) # moved to HomeRangeChange
write.csv(AllJourneysNorth, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_North_JourneyDays.csv", row.names = F) # moved to HomeRangeChange
AllJourneys <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_GSL_JourneyDays.csv")
AllJourneysNorth <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_North_JourneyDays.csv")


# violinplot
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/")
mycolours <- c("#F8766D", "#7CAE00")

# GSL
names(mycolours) <- c("GOM_to_GSL", "GSL_to_GOM")
ggplot(AllJourneys, aes(x = Direction, y = JourneyDays)) +
  geom_violin(aes(colour = Direction, fill = Direction)) +
  # scale_fill_manual(values = mycolours) + #F8766D # 7CAE00 # nothing works to force colours
  ggtitle(paste0("GOM fish GOM/GSL transit"),
          subtitle = paste0("GOM to GSL n=", AllJourneys %>% filter(Direction == "GOM_to_GSL") %>% nrow, ", ",
                            "GSL to GOM n=", AllJourneys %>% filter(Direction == "GSL_to_GOM") %>% nrow)) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  # geom_text(data = categoryn, aes(label = count), position = position_dodge(width = 1.0)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysGOM.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

# North
names(mycolours) <- c("GOM_to_North", "North_to_GOM")

ggplot(AllJourneysNorth, aes(x = Direction, y = JourneyDays)) + # journeydays
  geom_violin(aes(colour = Direction, fill = Direction)) +
  ggtitle(paste0("GOM fish GOM/North transit"),
          subtitle = paste0("GOM to North n=", AllJourneysNorth %>% filter(Direction == "GOM_to_North") %>% nrow, ", ",
                            "North to GOM n=", AllJourneysNorth %>% filter(Direction == "North_to_GOM") %>% nrow)) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysGOMNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

ggplot(AllJourneysNorth, aes(x = Direction, y = JourneySpeedDegDay)) + # journey degrees per day
  geom_violin(aes(colour = Direction, fill = Direction)) +
  ggtitle(paste0("GOM fish GOM/North transit"),
          subtitle = paste0("GOM to North n=", AllJourneysNorth %>% filter(Direction == "GOM_to_North") %>% nrow, ", ",
                            "North to GOM n=", AllJourneysNorth %>% filter(Direction == "North_to_GOM") %>% nrow)) +
  labs(x = "Direction", y = "Mean Journey Speed, Degrees per Day") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneySpeedDegDayGOMNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

ggplot(AllJourneysNorth, aes(x = Direction, y = LatDiff)) + # latdiff
  geom_violin(aes(colour = Direction, fill = Direction)) +
  ggtitle(paste0("GOM fish GOM/North transit"),
          subtitle = paste0("GOM to North n=", AllJourneysNorth %>% filter(Direction == "GOM_to_North") %>% nrow, ", ",
                            "North to GOM n=", AllJourneysNorth %>% filter(Direction == "North_to_GOM") %>% nrow)) +
  labs(x = "Direction", y = "Latitude Difference of Journey") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_LatDiffGOMNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)
# GOM_to_north range is 17.5 to 25 degrees, equally spaced. Breaking down where fish are leaving GOM and not going far? Or not doing a direct migration but instead meandering around?

# boxplot GSL
ggplot(AllJourneys, aes(x = Direction, y = JourneyDays)) +
  geom_boxplot(aes(fill = Direction), colour = "black", notch = F) +
  ggtitle(paste0("GOM fish GOM/GSL transit"),
          subtitle = paste0("GOM to GSL n=", AllJourneys %>% filter(Direction == "GOM_to_GSL") %>% nrow, ", ",
                            "GSL to GOM n=", AllJourneys %>% filter(Direction == "GSL_to_GOM") %>% nrow)) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("GOM to GSL", "GSL to GOM")) + # manually relabel to remove underscores
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysBoxGOM.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

# boxplot North
ggplot(AllJourneysNorth, aes(x = Direction, y = JourneySpeedDegDay)) +
  geom_boxplot(aes(fill = Direction), colour = "black", notch = F) +
  ggtitle(paste0("GOM fish GOM/North transit"),
          subtitle = paste0("GOM to North n=", AllJourneysNorth %>% filter(Direction == "GOM_to_North") %>% nrow, ", ",
                            "North to GOM n=", AllJourneysNorth %>% filter(Direction == "North_to_GOM") %>% nrow)) +
  labs(x = "Direction", y = "Mean Journey Speed, Degrees per Day") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("GOM to North", "North to GOM")) + # manually relabel to remove underscores
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneySpeedDegDayBoxGOMNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

AllJourneys %>%
  group_by(Direction) %>%
  summarise(Min = min(JourneyDays),
            Mean = mean(JourneyDays),
            Max = max(JourneyDays))
# Direction    Min  Mean   Max
# GOM_to_GSL    24   52     89
# GSL_to_GOM    32  105.   214
AllJourneysNorth %>%
  group_by(Direction) %>%
  summarise(Min = min(JourneyDays),
            Mean = mean(JourneyDays),
            Max = max(JourneyDays))
# Direction    Min  Mean   Max
# GOM_to_North    21  59.3   159
# North_to_GOM    34 110    214

write.csv(AllJourneys, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_GSL_Toppids.csv", row.names = F)
write.csv(AllJourneysNorth, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_North_Toppids.csv", row.names = F)
# KS test
ks.test(AllJourneys %>% filter(Direction == "GOM_to_GSL") %>% pull(JourneyDays),
        AllJourneys %>% filter(Direction == "GSL_to_GOM") %>% pull(JourneyDays))
# D = 0.58621, p-value = 0.01594 (not significant). Was ** significant with 9 GOMtoGSL & 60 GSLtoGOM
# alternative hypothesis: two-sided
# Warning: cannot compute exact p-value with ties
ks.test(AllJourneysNorth %>% filter(Direction == "GOM_to_North") %>% pull(JourneyDays),
        AllJourneysNorth %>% filter(Direction == "North_to_GOM") %>% pull(JourneyDays))
# D = 0.53123, p-value = 3.085e-05 (significance not noted, still too low n?)
# alternative hypothesis: two-sided


# GSL TO Med
# max yday in GSL per toppid2 = last day of the year in GSL, per toppid2 i.e. per fish per year
leftGSL <- AllDailies %>%
  filter(Stock == "Med", MarineZone == "GSL") %>%
  group_by(toppid2) %>%
  summarise(max(Day, na.rm = T))

northernmostMed <- AllDailies %>%
  filter(Stock == "Med",
         lat >= 39.3,
         lon <= -45) %>% # >= ForageW lat (39.3), might as well get rid of these values now
  group_by(toppid3) %>%
  summarise(MaxLat = max(lat, na.rm = T), # max lat
            MaxLatDay = Day[which(lat == max(lat, na.rm = T))]) # Day corresponding to the highest lat


# arrived in Med that year
arrivedMed <- AllDailies %>%
  filter(Stock == "Med",
         Day > 274, # why this constraint?
         MarineZone == "Med") %>%
  group_by(toppid2) %>%
  summarise(arrivedMeddate = min(Day, na.rm = T)) # day later than Oct, by eye on purple marinearea stock plots from above

arrivedMed <- AllDailies %>%
  filter(Stock == "Med",
         MarineZone == "Med") %>%
  group_by(toppid2) %>%
  summarise(arrivedMeddate = min(Day, na.rm = T)) # day later than Oct, by eye on purple marinearea stock plots from above
mean(arrivedMed$arrivedMeddate) # 145.6364 = 25th May

names(leftGSL)[2] <- "leftGSLdate"
names(northernmostMed)[3] <- "leftNorthdate"


GSLtoMed <- left_join(leftGSL, arrivedMed) %>% # joins on toppid2. 2020-11-25 no matches. leftgsl nrow 14 arrivedmed nrow 1.
  drop_na(arrivedMeddate) %>%
  mutate(GSLtoMeddays = arrivedMeddate - leftGSLdate)
mean(GSLtoMed$GSLtoMeddays) # mean GSL to Med travel days = 55.55. n = 20
# Med arrivals that year i.e. oct to Dec31, not including fish who got to Med after 31st Dec, in the next year.
# NONE 2020-11-25 i.e. NaN, ok good.
NorthtoMed <- left_join(northernmostMed, arrivedMed, by = c("toppid3" = "toppid2")) %>% # Joining, by = "toppid2"
  drop_na(arrivedMeddate) %>%
  mutate(NorthtoMeddays = arrivedMeddate - leftNorthdate)
mean(NorthtoMed$leftNorthdate) # mean North to Med travel days = 279.1935 n = 31
# is arrivedGOM$toppid2 comparable to northernmost$toppid3? YES: arrivedGOM is filtered for Day > 274 which is >80 so year is the same
# 2020-11-25 NorthtoMed nrow 0 no matches


# get first Med date for the toppids' next years
leftGSLlist <- AllDailies %>%
  filter(Stock == "Med", MarineZone == "GSL") %>%
  group_by(toppid2) %>%
  summarise(max(Day, na.rm = T)) %>%
  select(toppid2) %>%
  mutate(toppid2 = as.character(toppid2))
leftGSLlist <- leftGSLlist$toppid2 # remove tibble
leftGSLlistDF <- as.data.frame(str_split_fixed(string = leftGSLlist, pattern = "_", n = 2))
leftGSLlistDF[,2] <- as.numeric(leftGSLlistDF[,2])
leftGSLlistDF$NextYear <- leftGSLlistDF[,2] + 1
leftGSLlistDF$toppid2new <- paste0(leftGSLlistDF[,1], "_", leftGSLlistDF$NextYear) # that's your list of toppid2's to scan for as min day in Med
leftGSLlistDF$toppid2 <- paste0(leftGSLlistDF[,1], "_", leftGSLlistDF[,2]) # rebuild toppid2 for left_join later

leftNorthMedlist <- as.character(northernmostMed$toppid3)
leftNorthMedlistDF <- as.data.frame(str_split_fixed(string = leftNorthMedlist, pattern = "_", n = 2))
leftNorthMedlistDF[,2] <- as.numeric(leftNorthMedlistDF[,2])
leftNorthMedlistDF$NextYear <- leftNorthMedlistDF[,2] + 1
leftNorthMedlistDF$toppid3new <- paste0(leftNorthMedlistDF[,1], "_", leftNorthMedlistDF$NextYear) # that's your list of toppid2's to scan for as min day in GOM
leftNorthMedlistDF$toppid3 <- paste0(leftNorthMedlistDF[,1], "_", leftNorthMedlistDF[,2]) # rebuild toppid2 for left_join later

arrivedMed2 <- AllDailies %>%
  filter(toppid2 %in% leftGSLlistDF$toppid2new,
         Stock == "Med",
         # Day < 182,
         MarineZone == "Med") %>% # select fishyears for the next year (toppid2new)
  group_by(toppid2) %>%
  summarise(min(Day, na.rm = T)) # earliest day in Med before DATE, after which all are assumed to have left;
# Don't want to capture fish who went GSL, mixingground, GSL, Med, and sum from GSL1 to Med, i.e. across 2 cycles.
# 1st July looks good. yday(as.Date("2018-07-01")) 182
names(arrivedMed2)[2] <- "arrivedMeddate"
# need to left join but initially remove those which were already matched
GSLtoMed2 <- left_join(leftGSL, arrivedMed) %>%
  filter(is.na(arrivedMeddate)) %>% # need to KEEP ONLY the NAs
  select(-arrivedMeddate)

NorthtoMed2 <- left_join(northernmostMed, arrivedMed, by = c("toppid3" = "toppid2")) %>%
  filter(is.na(arrivedMeddate)) %>% # need to KEEP ONLY the NAs
  select(-arrivedMeddate) # n = 75

# left join won't work, need to match toppid2 but arrivedMed2$toppid2 is the next year
# need to subtract 1 from arrivedMed2$toppid2 first
arrivedMed2 %<>% mutate(toppid2 = as.character(toppid2))
arrivedMed2listDF <- as.data.frame(str_split_fixed(string = arrivedMed2$toppid2, pattern = "_", n = 2))
arrivedMed2listDF[,2] <- as.numeric(arrivedMed2listDF[,2])
# arrivedMed2listDF[,1] <- as.character(levels(arrivedMed2listDF[,1]))[arrivedMed2listDF[,1]]
arrivedMed2$toppid2 <- paste0(arrivedMed2listDF[,1], "_", arrivedMed2listDF[,2] - 1)
GSLtoMed2 %<>%
  left_join(arrivedMed2) %>%
  # select(-toppid2original) %>%
  mutate(GSLtoMeddays = arrivedMeddate + (366 - leftGSLdate))

NorthtoMed2 %<>%
  left_join(arrivedMed2, by = c("toppid3" = "toppid2")) %>% # Joining, by = "toppid2"
  mutate(NorthtoMeddays = arrivedMeddate + (366 - leftNorthdate)) # n = 75

GSLtoMedall <- rbind(GSLtoMed, GSLtoMed2) %>% drop_na(GSLtoMeddays)
NorthtoMedall <- rbind(NorthtoMed, NorthtoMed2) %>% drop_na(NorthtoMeddays) # n = 63
mean(GSLtoMedall$GSLtoMeddays) # 215.125 n=8
mean(NorthtoMedall$NorthtoMeddays) # 47 n=39. 2020-11-25: 223.875, n = 8
# do lat difference to 25N, bottom of everglades / mid keys.
NorthtoMedall$LatDiff <- NorthtoMedall$MaxLat - 36.2 # top of SOG IHO area
NorthtoMedall$JourneySpeedDegDay <- NorthtoMedall$LatDiff / NorthtoMedall$NorthtoMeddays

NorthtoMedall %<>%
  rename(leftdate = leftNorthdate,
         arriveddate = arrivedMeddate,
         JourneyDays = NorthtoMeddays) %>%
  mutate(Direction = "North_to_Med")

# Med TO GSL
# first GSL day
arrivedGSL <- AllDailies %>%
  filter(Stock == "Med", MarineZone == "GSL") %>%
  group_by(toppid2) %>%
  summarise(min(Day, na.rm = T)) # n=63, values 170:301. 2020-11-25 n = 14
names(arrivedGSL)[2] <- "arrivedGSLdate"
# last Med day before that
leftMed <- AllDailies %>%
  filter(Stock == "Med",
         # Day < 182,  # same as above
         MarineZone == "Med") %>%
  group_by(toppid2) %>%
  summarise(max(Day, na.rm = T))
names(leftMed)[2] <- "leftMeddate"
# highest value is 181, v close to 182, surprising?? No since they can leave later?

MedtoGSL <- left_join(leftMed, arrivedGSL) %>%
  drop_na(arrivedGSLdate) %>%
  mutate(MedtoGSLdays = arrivedGSLdate - leftMeddate)
mean(MedtoGSL$MedtoGSLdays) # mean Med to GSL travel days = 52. n = 8
# 2020-11-25 NaN, MedtoGSL nrow 0

MedtoNorth <- left_join(leftMed, northernmostMed, by = c("toppid2" = "toppid3")) %>%
  # OK to use toppid2 against toppid3 if ALL northernmost$leftNorthdate > 80 which these are.
  drop_na(leftNorthdate) %>%
  mutate(MedtoNorthdays = leftNorthdate - leftMeddate)
MedtoNorth %<>% filter(MedtoNorthdays >= 0)
MedtoNorth$LatDiff <- MedtoNorth$MaxLat - 36.2
MedtoNorth$JourneySpeedDegDay <- MedtoNorth$LatDiff / MedtoNorth$MedtoNorthdays
MedtoNorth %<>%
  select(toppid2, MaxLat, leftMeddate, leftNorthdate, MedtoNorthdays, LatDiff, JourneySpeedDegDay) %>%
  rename(leftdate = leftMeddate,
         arriveddate = leftNorthdate,
         JourneyDays = MedtoNorthdays) %>%
  mutate(Direction = "Med_to_North")
mean(MedtoNorth$JourneyDays) # mean Med to North travel days = 42.5. n = 4. 2020-11-25 28.66667 n=3


GSLtoMedall %<>%
  select(toppid2, GSLtoMeddays) %>%
  rename(JourneyDays = GSLtoMeddays) %>%
  mutate(Direction = "GSL_to_Med")
MedtoGSL %<>%
  select(toppid2, MedtoGSLdays) %>%
  rename(JourneyDays = MedtoGSLdays) %>%
  mutate(Direction = "Med_to_GSL")


AllJourneys2 <- rbind(GSLtoMedall, MedtoGSL)
AllJourneysNorthMed <- rbind(NorthtoMedall %>% rename(toppid2 = toppid3), # n = 91
                             MedtoNorth)
write.csv(AllJourneys2, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_GSL_JourneyDays.csv", row.names = F) # moved to HomeRangeChange
write.csv(AllJourneysNorthMed, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_North_JourneyDays.csv", row.names = F) # moved to HomeRangeChange
AllJourneys2 <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_GSL_JourneyDays.csv")
AllJourneysNorthMed <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_North_JourneyDays.csv")

# violinplot
# GSL
names(mycolours) <- c("Med_to_GSL", "GSL_to_Med")
ggplot(AllJourneys2, aes(x = Direction, y = JourneyDays)) +
  geom_violin(aes(colour = Direction, fill = Direction)) +
  # scale_fill_manual(values = mycolours) + #F8766D # 7CAE00 # nothing works to force colours
  ggtitle(paste0("Med fish Med/GSL transit"),
          subtitle = paste0("GSL to Med n=", AllJourneys2 %>% filter(Direction == "GSL_to_Med") %>% nrow, ", ",
                            "Med to GSL n=", AllJourneys2 %>% filter(Direction == "Med_to_GSL") %>% nrow)) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  # geom_text(data = categoryn, aes(label = count), position = position_dodge(width = 1.0)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDays.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

# North
names(mycolours) <- c("Med_to_North", "North_to_Med")
ggplot(AllJourneysNorthMed, aes(x = Direction, y = JourneyDays)) +
  geom_violin(aes(colour = Direction, fill = Direction)) +
  # scale_fill_manual(values = mycolours) + #F8766D # 7CAE00 # nothing works to force colours
  ggtitle(paste0("Med fish Med/North transit"),
          subtitle = paste0("North to Med n=", AllJourneysNorthMed %>% filter(Direction == "North_to_Med") %>% nrow, ", ",
                            "Med to North n=", AllJourneysNorthMed %>% filter(Direction == "Med_to_North") %>% nrow)) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  # geom_text(data = categoryn, aes(label = count), position = position_dodge(width = 1.0)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysMedNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

ggplot(AllJourneysNorthMed, aes(x = Direction, y = JourneySpeedDegDay)) + # journey degrees per day
  geom_violin(aes(colour = Direction, fill = Direction)) +
  ggtitle(paste0("Med fish Med/North transit"),
          subtitle = paste0("Med to North n=", AllJourneysNorthMed %>% filter(Direction == "Med_to_North") %>% nrow, ", ",
                            "North to Med n=", AllJourneysNorthMed %>% filter(Direction == "North_to_Med") %>% nrow)) +
  labs(x = "Direction", y = "Mean Journey Speed, Degrees per Day") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneySpeedDegDayMedNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

ggplot(AllJourneysNorthMed, aes(x = Direction, y = LatDiff)) + # latdiff
  geom_violin(aes(colour = Direction, fill = Direction)) +
  ggtitle(paste0("Med fish Med/North transit"),
          subtitle = paste0("Med to North n=", AllJourneysNorthMed %>% filter(Direction == "Med_to_North") %>% nrow, ", ",
                            "North to Med n=", AllJourneysNorthMed %>% filter(Direction == "North_to_Med") %>% nrow)) +
  labs(x = "Direction", y = "Latitude Difference of Journey") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_LatDiffMedNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)


# boxplot GSL
ggplot(AllJourneys2, aes(x = Direction, y = JourneyDays)) +
  geom_boxplot(aes(fill = Direction), colour = "black", notch = F) +
  ggtitle(paste0("Med fish Med/GSL transit"),
          subtitle = paste0("GSL to Med n=", AllJourneys2 %>% filter(Direction == "GSL_to_Med") %>% nrow, ", ",
                            "Med to GSL n=", AllJourneys2 %>% filter(Direction == "Med_to_GSL") %>% nrow)) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("GSL to Med", "Med to GSL")) + # manually relabel to remove underscores
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysBoxMed.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

# boxplot North
ggplot(AllJourneysNorthMed, aes(x = Direction, y = JourneySpeedDegDay)) +
  geom_boxplot(aes(fill = Direction), colour = "black", notch = F) +
  ggtitle(paste0("Med fish Med/North transit"),
          subtitle = paste0("Med to North n=", AllJourneysNorthMed %>% filter(Direction == "Med_to_North") %>% nrow, ", ",
                            "North to Med n=", AllJourneysNorthMed %>% filter(Direction == "North_to_Med") %>% nrow)) +
  labs(x = "Direction", y = "Mean Journey Speed, Degrees per Day") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("Med to North", "North to Med")) + # manually relabel to remove underscores
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneySpeedDegDayBoxMedNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)



AllJourneys2 %>%
  group_by(Direction) %>%
  summarise(Min = min(JourneyDays),
            Mean = mean(JourneyDays),
            Max = max(JourneyDays))
# Direction    Min  Mean   Max
# GSL_to_Med   182  215.   257
AllJourneysNorthMed %>%
  group_by(Direction) %>%
  summarise(Min = min(JourneyDays),
            Mean = mean(JourneyDays),
            Max = max(JourneyDays))
# Direction      Min  Mean   Max
# Med_to_North    26  28.7    31
# North_to_Med   185 224.    259
write.csv(AllJourneys2, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_GSL_Toppids.csv", row.names = F)
write.csv(AllJourneysNorthMed, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_North_Toppids.csv", row.names = F)
# KS test
ks.test(AllJourneys2 %>% filter(Direction == "Med_to_GSL") %>% pull(JourneyDays),
        AllJourneys2 %>% filter(Direction == "GSL_to_Med") %>% pull(JourneyDays))
# Error in ks.test(AllJourneys2 %>% filter(Direction == "Med_to_GSL") %>%  : not enough 'x' data
ks.test(AllJourneysNorthMed %>% filter(Direction == "Med_to_North") %>% pull(JourneyDays),
        AllJourneysNorthMed %>% filter(Direction == "North_to_Med") %>% pull(JourneyDays))
# D = 1, p-value = 0.01212




# Compare GOM to Med times
# Standardise distances. GOM GSL:2000. Med GSL 2800.
AllJourneys2$JourneyDays <- AllJourneys2$JourneyDays / 2800 * 2000
ks.test(AllJourneys %>% filter(Direction == "GOM_to_GSL") %>% pull(JourneyDays),
        AllJourneys2 %>% filter(Direction == "GSL_to_Med") %>% pull(JourneyDays))
# D = 1, p-value = 0.0001554 # no significance given, due to low n?
# alternative hypothesis: two-sided

ks.test(AllJourneys %>% filter(Direction == "GSL_to_GOM") %>% pull(JourneyDays),
        AllJourneys2 %>% filter(Direction == "GSL_to_Med") %>% pull(JourneyDays))
# Error in ks.test(AllJourneys2 %>% filter(Direction == "Med_to_GSL") %>%  : not enough 'x' data


# All 4 boxes on one plot, GSL
AllJourneys <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_GSL_JourneyDays.csv")
AllJourneys2 <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_GSL_JourneyDays.csv")
AllJourneys <- rbind(AllJourneys, AllJourneys2) # combine em
AllJourneys$Direction <- factor(AllJourneys$Direction, levels = c("GSL_to_GOM", "GOM_to_GSL", "GSL_to_Med", "Med_to_GSL")) # reorder GSL GOM, GOM GSL, GSL Med, Med GSL

ggplot(AllJourneys, aes(x = Direction, y = JourneyDays)) +
  geom_boxplot(aes(fill = Direction), colour = "black", notch = F) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("GSL-GOM", "GOM-GSL", "GSL-Med", "Med-GSL")) + # manually relabel to remove underscores
  scale_fill_manual(values = c(CB_RED, CB_RED, CB_BLUE, CB_BLUE)) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysBoxGOMMed.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4.5,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

AllJourneys %>%
  group_by(Direction) %>%
  summarise(Count = length(toppid2))
# GSL_to_GOM    58
# GOM_to_GSL     8
# GSL_to_Med     8

# All 4 boxes on one plot, North
AllJourneysNorth <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/GOM_North_JourneyDays.csv")
AllJourneysNorthMed <- read_csv("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/Med_North_JourneyDays.csv")
AllJourneysNorthMed %<>% select(colnames(AllJourneysNorth)) # remove extra cols so it'll rbind
AllJourneysNorth <- rbind(AllJourneysNorth, AllJourneysNorthMed) # combine em
AllJourneysNorth$Direction <- factor(AllJourneysNorth$Direction, levels = c("North_to_GOM", "GOM_to_North", "North_to_Med", "Med_to_North")) # reorder North GOM, GOM North, North Med, Med North

AllJourneysNorth %<>% filter(JourneyDays > 0) # remove weird negative

ggplot(AllJourneysNorth, aes(x = Direction, y = JourneyDays)) +
  geom_boxplot(aes(fill = Direction), colour = "black", notch = F) +
  labs(x = "Direction", y = "Journey Time, Days") +
  stat_summary(fun.y = mean, geom = "point", shape = 23, size = 5, colour = "white") + # adds mean point
  scale_x_discrete(labels = c("North-GOM", "GOM-North", "North-Med", "Med-North")) + # manually relabel to remove underscores
  scale_y_continuous(limits = c(0,260), breaks = seq(0, 250, by = 50)) +
  scale_fill_manual(values = c(CB_RED, CB_RED, CB_BLUE, CB_BLUE)) + # manually colour plots if specified
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_JourneyTimeDaysBoxGOMMedNorth.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 4.5,
         height = 5, units = "in", dpi = 300, limitsize = TRUE)

AllJourneysNorth %>%
  group_by(Direction) %>%
  summarise(Count = length(toppid2),
            MinJD = min(JourneyDays, na.rm = T),
            MaxJD = max(JourneyDays, na.rm = T),
            MeanJD = mean(JourneyDays, na.rm = T),
            MeanJSDD = mean(JourneySpeedDegDay, na.rm = T))
# Direction    Count MinJD MaxJD MeanJD MeanJSDD
# North_to_GOM    67    34   214  110.    0.248
# GOM_to_North    27    21   159   59.3   0.432
# North_to_Med     8   185   259  224.    0.0504
# Med_to_North     3    26    31   28.7   0.379

t.test(x = AllJourneysNorth[which(AllJourneysNorth$Direction == "North_to_GOM"), "JourneyDays"],
       y = AllJourneysNorth[which(AllJourneysNorth$Direction == "GOM_to_North"), "JourneyDays"],
       alternative = "two.sided",
       mu = 0, # difference in means
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)

# data:  AllJourneysNorth[which(AllJourneysNorth$Direction == "North_to_GOM"), "JourneyDays"] and AllJourneysNorth[which(AllJourneysNorth$Direction == "GOM_to_North"), "JourneyDays"]
# t = 5.9076, df = 67.176, p-value = 1.274e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval: 33.35570 67.39499
# sample estimates: mean of x mean of y
# 109.6716   59.2963

t.test(x = AllJourneysNorth[which(AllJourneysNorth$Direction == "North_to_Med"), "JourneyDays"],
       y = AllJourneysNorth[which(AllJourneysNorth$Direction == "Med_to_North"), "JourneyDays"],
       alternative = "two.sided",
       mu = 0, # difference in means
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)

# data:  AllJourneysNorth[which(AllJourneysNorth$Direction == "North_to_Med"), "JourneyDays"] and AllJourneysNorth[which(AllJourneysNorth$Direction == "Med_to_North"), "JourneyDays"]
# t = 21.92, df = 7.3688, p-value = 5.587e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval: 174.3623 216.0544
# sample estimates:
# mean of x mean of y
# 223.87500  28.66667

myaov <- aov(JourneyDays ~ Direction, AllJourneysNorth)
#                 Direction Residuals
# Sum of Squares   200277.5  188714.6
# Deg. of Freedom         3       103
# Residual standard error: 42.80398
# Estimated effects may be unbalanced

TukeyHSD(myaov)
#                                 diff        lwr        upr     p adj
# GOM_to_North-North_to_GOM  -50.37535  -75.19056  -25.56013 0.0000040
# North_to_Med-North_to_GOM  114.20336   73.48205  154.92466 0.0000000
# Med_to_North-North_to_GOM  -81.00498 -145.24777  -16.76218 0.0073389
# North_to_Med-GOM_to_North  164.57870  120.75787  208.39953 0.0000000
# Med_to_North-GOM_to_North  -30.62963  -96.88050   35.62124 0.6233921
# Med_to_North-North_to_Med -195.20833 -268.90777 -121.50889 0.0000000






# Fig 7? Monthly DST violins & lines####
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/")

MonthMeanDST <- AllDailies %>%
  group_by(Stock, Month) %>%
  summarise(DistanceToShoreKm = mean(DistanceToShoreKm, na.rm = T))

ggplot(data = AllDailies,
       aes(x = factor(Month), y = DistanceToShoreKm, colour = Stock, fill = Stock)) +
  geom_violin() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2, position = position_dodge(0.9), colour = "white") + # , fill = "black" kills stock separation
  geom_line(data = MonthMeanDST,
            aes(x = Month,
                y = DistanceToShoreKm,
                colour = Stock)) +
  scale_x_discrete(labels = month.abb) +
  labs(x = "Month", y = "Distance From Shore, Km") +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.82, 0.88),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_MonthlyDST_NearFar_ByStock.png"), # WATL TAG FILTER _WatlTagged
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)






# FigureSM 2 violinplot marinezone presence by stock####

# AllDailies %<>% drop_na(Stock, MarineZone)
AllDailies$MarineZoneStock <- paste0(AllDailies$MarineZone, "_", AllDailies$Stock)
mygroups <- c("ForageW_GOM", # mid blue
              "MixingW_GOM", # purple
              "GOM_GOM", # red
              "ForageW_Med", # mid blue
              "ForageE_Med", # darker blue
              "MixingW_Med", # purple
              "MixingE_Med", # darker purple
              "Med_Med") # red
mycolours <- c(CB_BLUE, "purple", CB_RED, CB_BLUE, "blue3", "purple", "purple3", CB_RED)
names(mycolours) <- mygroups

namevec <- vector() # blank repository
for (l in 1:length(mygroups)) namevec[l] <- AllDailies %>% filter(MarineZoneStock %in% mygroups[l]) %>% nrow # loop through group members, count rows of each
namevec <- paste0(mygroups, ":", namevec) # add group names to front of nrow numbers

ggplot(AllDailies %>% filter(MarineZoneStock %in% mygroups), # TagLocation %in% WAtl,
       aes(x = factor(MarineZoneStock, levels = mygroups),
           y = as.Date(Day - 1, origin = as.Date("2018-01-01")))) + # Day= 1:366, needs 0:365
  geom_violin(aes(fill = MarineZoneStock, colour = MarineZoneStock), position = "identity") +
  scale_fill_manual(values = mycolours) +
  scale_colour_manual(values = mycolours) +
  # limits needed because day isn't 0:365
  scale_y_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0), limits = c(as.Date("2018-01-01"), as.Date("2018-12-31"))) + #  # labels = c(month.abb, "", "")
  scale_x_discrete(labels = namevec) +
  labs(x = "Marine Area [n=fishdays in area]", y = "Month") +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, colour = "white") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   axis.text.x = element_text(angle = 90, size = 15),
                                   title = element_text(size = rel(2)),
                                   panel.grid.minor = element_blank(),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_MarineStockZoneYDayViolinWatlTaggedGOMMedSelected.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 9,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)


# FigureSM 3 violinplot marinezone presence by stock not WAtl only####
ADtmp <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds")) %>%
  ungroup %>% # if grouped when saved, can cause issues
  mutate(MarineZoneStock = paste0(MarineZone, "_", Stock)) %>%
  filter(Stock %in% c("GOM", "Med"), # keep only assigned GOM & Med fish
         MarineZoneStock %in% mygroups) %>%
  drop_na(Date, lat, lon) %>% # omit rows with NA values
  mutate(DataType = str_sub(fishID, -1, -1), # # "C" "D" "P"
         Day = lubridate::yday(Date))

namevec <- vector() # blank repository
for (l in 1:length(mygroups)) namevec[l] <- ADtmp %>% filter(MarineZoneStock %in% mygroups[l]) %>% nrow # loop through group members, count rows of each
namevec <- paste0(mygroups, ":", namevec) # add group names to front of nrow numbers

ggplot(ADtmp,
       aes(x = factor(MarineZoneStock, levels = mygroups),
           y = as.Date(Day - 1, origin = as.Date("2018-01-01")))) +
  geom_violin(aes(fill = MarineZoneStock, colour = MarineZoneStock), position = "identity") +
  scale_fill_manual(values = mycolours) +
  scale_colour_manual(values = mycolours) +
  # scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2018-01-01", "%Y-%m-%d"), as.Date("2018-12-25", "%Y-%m-%d"))) +
  scale_y_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0), limits = c(as.Date("2018-01-01"), as.Date("2018-12-31"))) +
  scale_x_discrete(labels = namevec) +
  labs(x = "Marine Area [n=fishdays in area]", y = "Month") +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, colour = "white") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   axis.text.x = element_text(angle = 90, size = 15),
                                   title = element_text(size = rel(2)),
                                   panel.grid.minor = element_blank(),
                                   legend.position = "none",
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_MarineStockZoneYDayViolinGOMMedSelected2.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, width = 9,
         height = 4, units = "in", dpi = 300, limitsize = TRUE)






# Fig SM size distribution####
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/")
mu <- AllDailies %>%
  group_by(Stock) %>%
  dplyr::summarise(grp.mean = mean(FishLengthCm, na.rm = T))
# mean GOM 253cm Med 183cm

ggplot(data = AllDailies,
       aes(x = FishLengthCm, fill = Stock, colour = Stock)) + # y = ..count..,
  geom_histogram(aes(y = ..density..),
                 fill = "white", #
                 alpha = 0.75,
                 position = "identity") +
  labs(y = "Density") + # Count
  geom_density(alpha = 0.2, fill = NA) + # aes(y = ..count..),
  geom_vline(data = mu,
             aes(xintercept = grp.mean,
                 color = Stock),
             linetype = "dashed") +
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.2, 0.88),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_SizeDistribution_SM.png"), # WATL TAG FILTER _WatlTagged
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)






# 2021-06-15 EA area propn by month lineplots FIGURE 5####
# Connects S-T fishing to time. Clean takeaway.

# xaxis: thru the year (monthly e.g.)
# yaxis, lines: what proportion of fish in box is GOM vs Med. Could do GOM & Med independently, red & blue lines
#  Density of GOM or Med. Fishdays (corrected?) start with raw.
# 75W-65W & 65W-55W & E of 55W (& 39.3 - 50N),
# & same for WMix hotspot. 79W 73W 31N 36N lonmin max latmin max
# & GSL itself. 70 55 45 51
# & S of ~35N?

# make table of data, summary
# plottable <- AllDailies %>%
#   mutate(LinesArea = factor(
#     case_when(
#       between(lon, -70, -55) & between(lat, 45, 51) ~ "GSL",
#       between(lon, -75, -65) & between(lat, 39.3, 46) ~ "GoMaine",
#       between(lon, -65, -55) & between(lat, 39.3, 46) & MarineZone != "GSL" ~ "Nova Scotia",
#       lon > -55 & between(lat, 39.3, 46) ~ "N Atlantic Offshore",
#       between(lon, -79, -73) & between(lat, 31, 36) ~ "W.Mix Hotspot",
#       lat < 35 & MarineZone != "GOM" ~ "Hatteras Offshore", #  !& (between(lon, -79, -73) & between(lat, 31, 36))
#       TRUE ~ NA_character_), # TRUE for all other oldcolvals not specified above
#     # levels = c("GSL", "GoMaine", "Nova Scotia", "N.N.Atl", "W.Mix Hotspot", "S.N.Atl"))) %>%
#     levels = c("GSL", "Nova Scotia", "GoMaine", "N Atlantic Offshore", "W.Mix Hotspot", "Hatteras Offshore"))) %>%
#   drop_na(LinesArea) %>%
#   group_by(LinesArea, Stock, Month) %>%
#   distinct() %>%
#   # summarise(NFishdays = n()) # raw fishdays initially
#   summarise(NFishdays = n()) %>% # raw fishdays initially
#   mutate(CorrValue = case_when(
#     Stock == "GOM" ~ 78,
#     Stock == "Med" ~ 42),
#     NFishdaysCorr = NFishdays / CorrValue)

# make table of data, summary - normal zones
plottable <- AllDailies %>%
  filter(MarineZone %in% c("GSL", "ForageW", "MixingW", "GOM", "Med")) %>%
  mutate(LinesArea = factor(
    case_when(MarineZone == "ForageW" ~ "North-WAtl",
              MarineZone == "MixingW" ~ "Central-WAtl",
              TRUE ~ MarineZone), # everything else
    levels = c("GSL", "North-WAtl", "Central-WAtl", "GOM", "Med"))) %>%
  drop_na(LinesArea) %>%
  group_by(LinesArea, Stock, Month) %>%
  distinct() %>%
  summarise(NFishdays = n()) %>% # raw fishdays initially
  mutate(CorrValue = case_when(
    Stock == "GOM" ~ 78,
    Stock == "Med" ~ 42),
    NFishdaysCorr = NFishdays / CorrValue)

# unique(plottable$LinesArea)
# unique(AllDailies$MarineZone)
# AllDailies %>%
#   select(Stock, toppid) %>%
#   group_by(Stock, toppid) %>%
#   distinct() %>%
#   group_by(Stock) %>%
#   summarise(n = n())

saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/")

ggplot(data = plottable,
       aes(x = Month, y = NFishdaysCorr, fill = Stock, colour = Stock)) +
  facet_wrap(.~LinesArea, ncol = 1) +
  geom_line(aes(colour = Stock), size = 0.5) + # , col = toppid # , group = toppid
  scale_colour_manual(values = c(CB_RED, CB_BLUE)) +
  scale_x_discrete(breaks = 1:12,
                   limits = 1:12,
                   labels = str_sub(month.abb, 1, 1),
                   expand = c(0,0.1)) +
  labs(y = "Number of Fishdays (corrected)") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(1.5)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.1, 0.95),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.grid.minor = element_blank(),
                                   plot.background = element_rect(fill = "white", colour = "grey50"), # white background
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_xMonth_yFishdaysCorr_colStock_facetRegion.png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)




# Stats: T-tests for all GOM/Med pairs####
saveloc = "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/"
# Are means different, GOM vs Med stocks?
# Areas: GSL, ForageW, MixingW, MixingWhotspot, GOM vs Med SGs
# Variables: Mean Depth, Max Depth, OceanDepth, Mean Temp, Min Temp, Li5day, Eddy speed, cyc amp, combo?,
myareas <- c("GSL", "ForageW", "MixingW", "MixingWhotspot", "GOM_Med", "Keys_SOG", "Keys_Other", "SOG_Other")
myvariables <- c("MeanDepth24h", "MaxDepth24h", "OceanDepth", "MeanETemp24h", "MinETemp24h", "li5day", "speed_average", "cyclonicAmp",
                 "EddySpeedAmp", "DistanceToShoreKm", "lat", "Hrs50mLesDepRange", "SurfFreq24h", "StepLengthBL", "ild_dive_cnt_desc_gl_Sum")

resultsDF <- as.data.frame(matrix(nrow = length(myvariables),
                                  ncol = length(myareas),
                                  dimnames = list(myvariables, myareas)))
resultsDFsig <- resultsDF
resultsDFn <- resultsDF

shapiroDF <- c(paste0(myareas, "_GOM"),
               paste0(myareas, "_Med"))
shapiroDF <- shapiroDF[order(c(seq_along(1:(length(shapiroDF) / 2)), seq_along(1:(length(shapiroDF) / 2))))]
# https://stackoverflow.com/questions/16443260/interleave-lists-in-r/33882030
shapiroDF <- as.data.frame(matrix(nrow = length(myvariables),
                                  ncol = length(shapiroDF),
                                  dimnames = list(myvariables, shapiroDF)))
shapiroNormal <- shapiroDF
shapiroN <- shapiroDF

for (i in myareas) { # i <- myareas[1]
  for (j in myvariables) { # j <- myvariables[1]
    i2 <- i
    if (i == "GOM_Med") i2 <- c("GOM", "Med")
    myx <- AllDailies[which(AllDailies$MarineZone %in% i2 & AllDailies$Stock == "GOM"), j]
    myy <- AllDailies[which(AllDailies$MarineZone %in% i2 & AllDailies$Stock == "Med"), j]

    if (i == "MixingWhotspot") {
      myx <- AllDailies[which(between(AllDailies$lon, -79, -73) & between(AllDailies$lat, 31, 36) & AllDailies$Stock == "GOM"), j]
      myy <- AllDailies[which(between(AllDailies$lon, -79, -73) & between(AllDailies$lat, 31, 36) & AllDailies$Stock == "Med"), j]
    }

    if (i == "Keys_SOG") {
      myx <- AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5) & AllDailies$Stock == "GOM"), j] # keys
      myy <- AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37) & AllDailies$Stock == "Med"), j] # SOG
    }

    if (i == "Keys_Other") {
      myx <- AllDailies[which(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5) & AllDailies$Stock == "GOM"), j] # keys
      myy <- AllDailies[which(!(between(AllDailies$lon, -83.5, -80.2) & between(AllDailies$lat, 23.5, 25.5) & AllDailies$Stock == "GOM")), j] # not keys
    }

    if (i == "SOG_Other") {
      myx <- AllDailies[which(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37) & AllDailies$Stock == "Med"), j] # SOG
      myy <- AllDailies[which(!(between(AllDailies$lon, -7, -4.5) & between(AllDailies$lat, 35, 37) & AllDailies$Stock == "Med")), j] # not keys
    }

    if (all(is.na(myx)) | all(is.na(myy))) next # if either myx or myy are all NA, skip to next j

    tmp <- t.test(x = myx,
                  y = myy,
                  alternative = "two.sided",
                  mu = 0, # difference in means
                  paired = FALSE,
                  var.equal = FALSE, # default. Uses Welch's t-test
                  conf.level = 0.95) # default
    resultsDF[j, i] <- tmp$p.value
    if (between(tmp$p.value, 0.01, 0.05)) resultsDFsig[j, i] <- "*" # <0.05
    if (between(tmp$p.value, 0.001, 0.01)) resultsDFsig[j, i] <- "**" # < 0.01
    if (tmp$p.value < 0.001) resultsDFsig[j, i] <- "***" # < 0.001
    rm(tmp) # prevents using this value in future loops in case there's an error for that future loop

    myx <- myx %>% pull # removes vector from tibble
    myx <- myx[!is.na(myx)]
    if (length(myx) > 5000) myx <- sample(x = myx, size = 5000) # sample size must be between 3 and 5000
    shapiro <- shapiro.test(myx)
    shapiroDF[j, paste0(i, "_GOM")] <- shapiro$p.value
    shapiroNormal[j, paste0(i, "_GOM")] <- ifelse(shapiro$p.value > 0.05, TRUE, FALSE) # p-value > 0.05 = is normal

    ggpubr::ggqqplot(data = myx, title = paste(paste0("Region: ", i),
                                               paste0("Variable: ", j),
                                               "Stock: GOM", sep = "\n")) +
      ggsave(paste0(saveloc, today(), "_QQPlot_", i, "_", j, "_GOM.png"), # WATL TAG FILTER _WatlTagged
             plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
             height = 4/2, units = "in", dpi = 300, limitsize = TRUE)

    myy <- myy %>% pull
    myy <- myy[!is.na(myy)]
    if (length(myy) > 5000) myy <- sample(x = myy, size = 5000)
    shapiro <- shapiro.test(myy)
    shapiro$p.value # 3.290644e-66
    shapiroDF[j, paste0(i, "_Med")] <- shapiro$p.value
    shapiroNormal[j, paste0(i, "_Med")] <- ifelse(shapiro$p.value > 0.05, TRUE, FALSE) # p-value > 0.05 = is normal

    ggpubr::ggqqplot(data = myy, title = paste(paste0("Region: ", i),
                                               paste0("Variable: ", j),
                                               "Stock: Med", sep = "\n")) +
      ggsave(paste0(saveloc, today(), "_QQPlot_", i, "_", j, "_Med.png"), # WATL TAG FILTER _WatlTagged
             plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
             height = 4/2, units = "in", dpi = 300, limitsize = TRUE)

    resultsDFn[j, i] <- length(myx) + length(myy)
    shapiroN[j, paste0(i, "_GOM")] <- length(myx)
    shapiroN[j, paste0(i, "_Med")] <- length(myy)

  } # close j myvariables
} #  close i myareas
write.csv(resultsDF, paste0(saveloc, "ttestPvalues.csv"))
write.csv(resultsDFsig, paste0(saveloc, "ttestPvaluesSig.csv"))
write.csv(shapiroDF, paste0(saveloc, "shapiroDF.csv"))
write.csv(shapiroNormal, paste0(saveloc, "shapiroNormal.csv"))
write.csv(resultsDFn, paste0(saveloc, "ttestTotalN.csv"))
write.csv(shapiroN, paste0(saveloc, "ttestGroupNs.csv"))

myareas <- c("GOM", "Med")
myvariables <- c("MeanDepth24h", "MaxDepth24h", "OceanDepth", "MeanETemp24h", "MinETemp24h", "li5day", "speed_average", "cyclonicAmp",
                 "EddySpeedAmp", "DistanceToShoreKm", "lat", "Hrs50mLesDepRange", "SurfFreq24h", "StepLengthBL", "ild_dive_cnt_desc_gl_Sum")
resultsDF <- as.data.frame(matrix(nrow = length(myvariables),
                                  ncol = length(myareas),
                                  dimnames = list(myvariables, myareas)))
resultsDFsig <- resultsDF

for (i in myareas) { # i <- myareas[1]
  for (j in myvariables) { # j <- myvariables[1]
    myx <- AllDailies[which(AllDailies$MarineZone2 == i & AllDailies$Stock == i), j]
    myy <- AllDailies[which(AllDailies$MarineZone2 == "MixingWhotspot" & AllDailies$Stock == i), j]
    if (all(is.na(myx)) | all(is.na(myy))) next # if either myx or myy are all NA, skip to next j
    tmp <- t.test(x = myx,
                  y = myy,
                  alternative = "two.sided",
                  mu = 0, # difference in means
                  paired = FALSE,
                  var.equal = FALSE,
                  conf.level = 0.95)
    resultsDF[j, i] <- tmp$p.value
    if (between(tmp$p.value, 0.01, 0.05)) resultsDFsig[j, i] <- "*" # <0.05
    if (between(tmp$p.value, 0.001, 0.01)) resultsDFsig[j, i] <- "**" # < 0.01
    if (tmp$p.value < 0.001) resultsDFsig[j, i] <- "***" # < 0.001
    rm(tmp) # prevents using this value in future loops in case there's an error for that future loop
  } # close j myvariables
} #  close i myareas
write.csv(resultsDF, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/ttestPvaluesSGsVsMixingWhotspot.csv")
write.csv(resultsDFsig, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/ttestPvaluesSigSGsVsMixingWhotspot.csv")


# Monthly DST t-tests
resultsDF <- as.data.frame(matrix(nrow = 12, ncol = 2, dimnames = list(month.abb, c("P", "Sig"))))
for (i in 1:12) {
  tmp <- t.test(x = AllDailies[which(AllDailies$Stock == "GOM" & AllDailies$Month == i), "DistanceToShoreKm"],
                y = AllDailies[which(AllDailies$Stock == "Med" & AllDailies$Month == i), "DistanceToShoreKm"],
                alternative = "two.sided",
                mu = 0, # difference in means
                paired = FALSE,
                var.equal = FALSE,
                conf.level = 0.95)
  resultsDF[i, "P"] <- tmp$p.value
  if (between(tmp$p.value, 0.01, 0.05)) resultsDF[i, "Sig"] <- "*" # <0.05
  if (between(tmp$p.value, 0.001, 0.01)) resultsDF[i, "Sig"] <- "**" # < 0.01
  if (tmp$p.value < 0.001) resultsDF[i, "Sig"] <- "***" # < 0.001
  rm(tmp) # prevents using this value in future loops in case there's an error for that future loop
} # close i
write.csv(resultsDF, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/MonthlyDSTttests.csv")

# Wilcoxon Mann Whitney (replaces KS) tests####
source('~/Dropbox/Blocklab Monterey/Blocklab/wmwbrute.R')
setwd("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/WMWtests/")
if (min(AllDailies$MeanDepth24h, na.rm = T) >= -0.5) AllDailies$MeanDepth24h <- -AllDailies$MeanDepth24h # make depths negative
if (min(AllDailies$MaxDepth24h, na.rm = T) >= 0) AllDailies$MaxDepth24h <- -AllDailies$MaxDepth24h # make depths negative
if (min(AllDailies$OceanDepth, na.rm = T) >= 0) AllDailies$OceanDepth <- -AllDailies$OceanDepth # make depths negative

resultsdf <- wmwbrute(df = AllDailies %>%
                        select(Stock, MeanETemp24hU2M, MeanDepth24h, OceanDepth, DistanceToShoreKm,
                               MaxDepth24h, MaxSST24h, MinSST24h, MeanETemp24h, MinETemp24h, li5day,
                               speed_average, cyclonicAmp, EddySpeedAmp, Hrs50mLesDepRange,
                               SurfFreq24h, StepLengthBL, ild_dive_cnt_desc_gl_Sum),
                      groupcol = "Stock",
                      violinplots = TRUE,
                      membersonly = c("GOM", "Med"),
                      membercolours = c(CB_RED, CB_BLUE))
# results saved in folder

# MEANS
stock_means <- AllDailies %>%
  select(Stock, MeanETemp24hU2M, MeanDepth24h, OceanDepth, DistanceToShoreKm,
         MaxDepth24h, MaxSST24h, MinSST24h, MeanETemp24h, MinETemp24h, li5day,
         speed_average, cyclonicAmp, EddySpeedAmp, Hrs50mLesDepRange,
         SurfFreq24h, StepLengthBL, ild_dive_cnt_desc_gl_Sum) %>%
  filter(Stock != "UNK") %>%
  group_by(Stock) %>%
  summarise_all(mean, na.rm = T) %>%
  mutate(across(where(is.numeric), round, digits = 2))
# very weird: block above runs fine if you sleect all of it, but errors if you ctrl+enter it:
# Error: Assigned data `\`%>%\`(...)` must be compatible with existing data.
# x Existing data has 30117 rows.
# x Assigned data has 2 rows.
# ℹ Only vectors of size 1 are recycled.
write.csv(x = stock_means,
          file = "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/stock_means.csv",
          row.names = F)




# Stats: compare ext.temp GSL surface/depth####
# "It may be that GOM fish’s (larval) adaptation to their hot spawning grounds
# reduces their thermal tolerance range (Dahlke et al., 2020) which limits access
# to the cold depths when in the northern regions"
## Is it that cold at depth in the northern regions?
## Or just cold in general there?
sst <- AllDailies %>% #mean sst by month & zone
  filter(MarineZone %in% c("GSL", "ForageW", "MixingW", "Med", "GOM")) %>%
  group_by(MarineZone, Month) %>%
  summarise(MeanSST = mean(MeanETemp24hU2M, na.rm = TRUE)) %>%
  pivot_wider(names_from = MarineZone,
              values_from = MeanSST) %>% # marinezone to column
  select(Month, GSL, ForageW, MixingW, Med, GOM) # reorder cols
write.csv(x = sst, file = "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/MonthlySSTbyZone.csv", row.names = FALSE)
## Perhaps give temperature ranges for surface versus at depth.

machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
loadloc <- paste0(machine, "Blocklab/abft_diving/All_Minutes/")
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/boxplots/")

GOM <- readRDS(paste0(loadloc, "GOM.Rds"))
GOM %<>% # GOM from HomeRangeChange.R L2189
  mutate(Stock = "GOM",
         Date = date(DateTimeUTCmin5)) %>%
  select(Date, lat, Depth.m., ExtTemp.C., Stock) %>%
  filter(!is.na(Depth.m.),
         !is.na(lat),
         lat > 39.3) %>%  # for latitudes > 39.3N (i.e. ForageW)
  select(-lat)

Med <- readRDS(paste0(loadloc, "Med.Rds"))
Med %<>% # Med from HomeRangeChange.R L2299
  mutate(Stock = "Med",
         Date = date(DateTimeUTCmin5)) %>%
  select(Date, lat, Depth.m., ExtTemp.C., Stock) %>%
  filter(!is.na(Depth.m.),
         !is.na(lat),
         lat > 39.3) %>%  # for latitudes > 39.3N (i.e. ForageW)
  select(-lat)

Med %<>% bind_rows(GOM) # bind together to plot together
rm(GOM) # remove GOM to conserve memory

# temperatures at different depths. Boxplot. Depths 0-10m, 10-50, 50-100, 100-250, 250+
Med$depthbins = cut(Med[, "Depth.m."],
                    breaks = c(-Inf, 10, 50, 100, 250, Inf), # vector of numeric breaks for colname variable bins
                    labels = c("0-10", "10-50", "50-100", "100-250", ">250")) # vector of bin names corresponding to cutbreaks, one fewer

ggplot(Med, aes(x = factor(depthbins), y = ExtTemp.C.)) +
  geom_boxplot(aes(fill = Stock), colour = "black", notch = F, position = position_dodge(preserve = "single")) +
  # scale_y_continuous(limits = c(0, round(max(shark$PCL, na.rm = T), -2)),
  #                    breaks = seq(from = 0, to = round(max(shark$PCL, na.rm = T), -2), by = 50)) +
  scale_fill_manual(values = c(CB_RED, CB_BLUE)) +
  labs(x = "Depth (m)") +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2.5)),
                                   # axis.title.x = element_text(vjust = -2), # move x axis label down a bit
                                   title = element_text(size = rel(2.5)),
                                   legend.position = c(0.95, 0.88),
                                   legend.text = element_text(size = rel(2.5)),
                                   legend.key.size = unit(2, 'cm'),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_Temps_at_Depth_by_Stock_LatO39.3.png"),
         plot = last_plot(), device = "png", path = "", scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
         width = 15, height = 6, units = "in", dpi = 600, limitsize = TRUE)




# Data summary table TO COMPLETE####
range(AllDailies$Year) # 1997 2020
# total number of fish tagged in qualifying locations
sort(unique(AllDailies$seriesname))
# canada 07:19, 12 years
# hatteras 2007
# nantucket 2001
# NC 1997:2012 not inc, 11 years

# n tagged per location
# deploy table?

# n of gom & med per location
AllDailies %>%
  group_by(TagLocation, Stock) %>%
  summarise(Count = length(unique(toppid)))
# TagLocation    Stock Count
# Canada         GOM      67
# Canada         Med       8
# Hatteras       Med       2
# Nantucket      GOM       1
# North_Carolina GOM       8
# North_Carolina Med      32

# n of gom + med total
length(unique(AllDailies$toppid)) # 118


# all fish deployed
# needs deploy table!
deploy %>%
  mutate(Year = year(taggingdate)) %>%
  filter(str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket|New England"),
         Year <= 2019) %>%
  summarise(Count = length(unique(toppid)))
# 1312

# tagging months, all fish
deploy %>%
  filter(str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket")) %>%
  mutate(TaggingMonth = month(taggingdate)) %>%
  group_by(seriesname, TaggingMonth) %>%
  summarise(Count = length(unique(toppid))) %>%
  ungroup() %>%
  arrange(TaggingMonth) %>% # ensures column order from pivotwider is numerically sorted
  pivot_wider(names_from = TaggingMonth, values_from = Count) %>%
  arrange(seriesname) %>%
  mutate(Series = str_sub(seriesname, 1, -6)) %>% # cut off year
  select(Series, everything(), -seriesname) %>%
  group_by(Series) %>%
  summarise_all(sum, na.rm = T)
# Month             `1`   `2`   `3`   `4`   `7`   `8`   `9`  `10`  `11`  `12`
# Canada             0     0     0     0     2     3   178   139     6     0
# Hatteras           0     0    27     0     0     0     0     0     0     0
# Nantucket          0     0     0     0     0     0     5    23     1     0
# North Carolina   546    52   283    23     0     0     0     0     0    23

# tagging months, this study's fish
deploy %>%
  filter(str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket"),
         toppid %in% unique(AllDailies$toppid)) %>%
  mutate(TaggingMonth = month(taggingdate)) %>%
  group_by(seriesname, TaggingMonth) %>%
  summarise(Count = length(unique(toppid))) %>%
  ungroup() %>%
  arrange(TaggingMonth) %>% # ensures column order from pivotwider is numerically sorted
  pivot_wider(names_from = TaggingMonth, values_from = Count) %>%
  arrange(seriesname) %>%
  mutate(Series = str_sub(seriesname, 1, -6)) %>% # cut off year
  select(Series, everything(), -seriesname) %>%
  group_by(Series) %>%
  summarise_all(sum, na.rm = T)
# Series            `1`   `2`   `3`   `4`   `9`  `10`  `11`
# Canada             0     0     0     0    47    26     2
# Hatteras           0     0     2     0     0     0     0
# Nantucket          0     0     0     0     0     1     0
# North Carolina    22     3    14     1     0     0     0

# make & model numbers of tags, all fish
deploy %>%
  filter(str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket")) %>%
  mutate(Series = str_sub(seriesname, 1, -6)) %>% # cut off year
  select(Series, tagtype, toppid, -seriesname) %>% # tagmodel,
  group_by(Series, tagtype) %>% # tagmodel,
  summarise(Count = length(unique(toppid)))
# Series         tagtype       Count
# Canada         accelerometer     2
# Canada         acoustic        221
# Canada         archival         11
# Canada         cats             11
# Canada         satellite       212
# Hatteras       archival         27
# Nantucket      archival          2
# Nantucket      satellite        28
# North Carolina acoustic          4
# North Carolina archival        698
# North Carolina satellite       253

# make & model numbers of tags, this study's fish
deploy %>%
  filter(str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket"),
         toppid %in% unique(AllDailies$toppid)) %>%
  mutate(Series = str_sub(seriesname, 1, -6)) %>% # cut off year
  select(Series, tagtype, toppid, -seriesname) %>% # tagmodel,
  group_by(Series, tagtype) %>% # tagmodel,
  summarise(Count = length(unique(toppid)))
# Series         tagtype   Count
# Canada         acoustic     40
# Canada         satellite    75
# Hatteras       archival      2
# Nantucket      satellite     1
# North Carolina archival     38
# North Carolina satellite     4


# fishing type used, all fish
tblfgarchivaldeployment %>%
  filter(str_sub(toppid, 1, 2) == 51,
         str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket")) %>%
  mutate(Series = str_sub(seriesname, 1, -6)) %>% # cut off year
  select(Series, fishmethod, toppid, -seriesname) %>%
  group_by(Series, fishmethod) %>%
  summarise(Count = length(unique(toppid)))
# Series         fishmethod    Count
# Canada         Rod-and-reel      1
# Canada         Rod-and-Reel      1
# Canada         NA                9
# Hatteras       Rod-and-Reel     27
# Nantucket      NA                2
# North Carolina Rod and Reel     86
# North Carolina Rod- and-reel     1
# North Carolina Rod-and-Reel      2
# North Carolina Troll            17
# North Carolina Trolling         15
# North Carolina NA              581

# fishing type used, all fish
tblfgarchivaldeployment %>%
  filter(toppid %in% unique(AllDailies$toppid),
         str_detect(seriesname, "Canada|Carolina|Hatteras|Nantucket")) %>%
  mutate(Series = str_sub(seriesname, 1, -6)) %>% # cut off year
  select(Series, fishmethod, toppid, -seriesname) %>%
  group_by(Series, fishmethod) %>%
  summarise(Count = length(unique(toppid)))
# Series         fishmethod   Count
# Hatteras       Rod-and-Reel     2
# North Carolina Rod and Reel    11
# North Carolina Rod-and-Reel     1
# North Carolina Troll            1
# North Carolina NA              25


# All Fish table####
# needs deploy table!
AllDailies %<>% left_join(deploy %>%
                            select(toppid, tagtype, tagmodel, taggingdate))

allfishtable <- AllDailies %>%
  group_by(toppid) %>%
  summarise(Tag_Type = first(tagtype),
            Tag_Model = first(tagmodel),
            Tag_Location = first(TagLocation),
            Deployment_Date = first(taggingdate),
            CFL_cm = first(FishLengthCm),
            End_Date = last(Date),
            End_latitude = last(lat),
            End_longitude = last(lon),
            Days_at_liberty = last(Date) - first(Date),
            Stock = first(Stock)) %>%
  arrange(Deployment_Date)

doubletagged <- AllDailies %>%  # get doubletagged toppids
  group_by(toppid, tagtype) %>%
  select(tagmodel) %>%
  unique %>%
  ungroup %>%
  mutate(Dupe = duplicated(toppid)) %>%
  filter(Dupe) %>%
  select(toppid) %>%
  pull

# need to join doubletags into single lines e.g.
# 5104457 satellite PAT3
# 5104457 archival LTD2310

doubletagbind <- AllDailies %>%  # get doubletagged toppids
  group_by(toppid, tagtype) %>%
  select(tagmodel) %>%
  unique %>%
  ungroup %>%
  filter(toppid %in% doubletagged) %>%
  group_by(toppid) %>%
  mutate(tagtype = paste(tagtype, collapse = " & "),
         tagmodel = paste(tagmodel, collapse = " & ")) %>%
  group_by(toppid) %>%
  summarise_all(first) %>%
  rename(Tag_Type = tagtype,
         Tag_Model = tagmodel)

# Need to update Tag_Type & Tag_Model in allfishtable
library(data.table)
setDT(doubletagbind)
setDT(allfishtable)
allfishtable[doubletagbind, on = c("toppid"), Tag_Type := i.Tag_Type]
allfishtable[doubletagbind, on = c("toppid"), Tag_Model := i.Tag_Model]
setDF(allfishtable)
write.csv(allfishtable, "/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/AllFishUsedInPaperTable.csv",row.names = F)


# Probabilistic Assignment ####
if (min(AllDailies$MeanDepth24h, na.rm = T) < 0) AllDailies$MeanDepth24h <- -AllDailies$MeanDepth24h # above re-reverses depths which are made negative above
if (min(AllDailies$MaxDepth24h, na.rm = T) < 0) AllDailies$MaxDepth24h <- -AllDailies$MaxDepth24h
if (min(AllDailies$OceanDepth, na.rm = T) < 0) AllDailies$OceanDepth <- -AllDailies$OceanDepth

# Test performance of tests on already-assigned fish
report <- data.frame("toppid" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),  # build report
                     "Lat_o_50" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "Lon_o_55W" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "Lon_u_80.2W" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "GSL_MinEtemp_u_0" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "GSL_MaxDepth_oe_90" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "GSL_MeanEtemp_u_9" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "NWF_MaxDepth_o_900" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "NWF_Hrs50_e_24" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "NWF_DST_o_800" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "NWF_OcDep_o_5275" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "WM_Hrs50_u_9" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "WM_StepL_o_600k" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "WM_DST_o_1600" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "WM_Lat_u_30N" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "June_MarZ_e_GSL" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "JunJul_MWH" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "May_NWF" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "JND_WM_DST_o_500" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "ND_NWF_DST_o_500" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "Length_o_250" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "Jan_NWF_Maine" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "Apr_Hatteras" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "JO_CCod" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "JA_GSL_StLR" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "SO_GSL_PtHood" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                     "Sept_GSL_Anticosti" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))))

# Loop through all fish in AllDailies (filtered at top to GOM/Med only)
# Conduct all tests on all fish, print results to report df
for (i in unique(AllDailies$toppid)) {
  report[which(unique(AllDailies$toppid) %in% i), "toppid"] <- i
  # GOM never above 50°N
  report[which(unique(AllDailies$toppid) %in% i), "Lat_o_50"] <- AllDailies %>% # send outputs to report
    drop_na(lat) %>% filter(toppid == i) %>% summarise(Fishdays = length(toppid[lat > 50]) / length(toppid))
  # GOM rarely E of 55°W
  report[which(unique(AllDailies$toppid) %in% i), "Lon_o_55W"] <- AllDailies %>%
    drop_na(lon) %>% filter(toppid == i) %>% summarise(Fishdays = length(toppid[lon > -55]) / length(toppid))
  # Med fish never W of 80.2°W
  report[which(unique(AllDailies$toppid) %in% i), "Lon_u_80.2W"] <- AllDailies %>%
    drop_na(lon) %>% filter(toppid == i) %>% summarise(Fishdays = length(toppid[lon < -80.2]) / length(toppid))
  # Mostly Med fish below 0°C
  report[which(unique(AllDailies$toppid) %in% i), "GSL_MinEtemp_u_0"] <- AllDailies %>%
    drop_na(MinETemp24h) %>% filter(MarineZone == "GSL", toppid == i) %>%
    summarise(Fishdays = length(toppid[MinETemp24h < 0]) / length(toppid))
  # GSL: MaxDepth >90m more likely to be Med
  report[which(unique(AllDailies$toppid) %in% i), "GSL_MaxDepth_oe_90"] <- AllDailies %>%
    filter(MarineZone == "GSL", toppid == i) %>% drop_na(MaxDepth24h) %>%
    summarise(Fishdays = length(toppid[MaxDepth24h >= 90]) / length(toppid))
  # GSL: daily mean etemp, GOM never <9°C
  report[which(unique(AllDailies$toppid) %in% i), "GSL_MeanEtemp_u_9"] <- AllDailies %>%
    drop_na(MeanETemp24h) %>% filter(MarineZone == "GSL", toppid == i) %>%
    summarise(Fishdays = length(toppid[MeanETemp24h < 9]) / length(toppid))
  # NWforage: Max depth: Med never >900m
  report[which(unique(AllDailies$toppid) %in% i), "NWF_MaxDepth_o_900"] <- AllDailies %>%
    drop_na(MaxDepth24h) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[MaxDepth24h > 900]) / length(toppid))
  # NWforage: GOM much more likely to spend 24 hrs within 50m depth?
  report[which(unique(AllDailies$toppid) %in% i), "NWF_Hrs50_e_24"] <- AllDailies %>%
    drop_na(Hrs50mLesDepRange) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[Hrs50mLesDepRange == 24]) / length(toppid))
  # NWforage: GOM never > ~800km from shore?
  report[which(unique(AllDailies$toppid) %in% i), "NWF_DST_o_800"] <- AllDailies %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 800]) / length(toppid))
  # NWforage: GOM Never >5.3km OcDep?
  report[which(unique(AllDailies$toppid) %in% i), "NWF_OcDep_o_5275"] <- AllDailies %>%
    drop_na(OceanDepth) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[OceanDepth > 5275]) / length(toppid))
  # WMix: GOM >> more likely to have HrsW<=50mDepRange <9
  report[which(unique(AllDailies$toppid) %in% i), "WM_Hrs50_u_9"] <- AllDailies %>%
    drop_na(Hrs50mLesDepRange) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[Hrs50mLesDepRange < 9]) / length(toppid))
  # WMix: No Med steplengthsbl > 600k
  report[which(unique(AllDailies$toppid) %in% i), "WM_StepL_o_600k"] <- AllDailies %>%
    drop_na(StepLengthBL) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[StepLengthBL > 600000]) / length(toppid))
  # WMix: No GOM DistToShore > 1600Km
  report[which(unique(AllDailies$toppid) %in% i), "WM_DST_o_1600"] <- AllDailies %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 1600]) / length(toppid))
  # WMix: Hardly any Med below 30°N
  report[which(unique(AllDailies$toppid) %in% i), "WM_Lat_u_30N"] <- AllDailies %>%
    drop_na(lat) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[lat < 30]) / length(toppid))
  # Only GOM in GSL in June
  report[which(unique(AllDailies$toppid) %in% i), "June_MarZ_e_GSL"] <- AllDailies %>%
    drop_na(MarineZone) %>% filter(Month == 6, toppid == i) %>%
    summarise(Fishdays = length(toppid[MarineZone == "GSL"]) / length(toppid))
  # MixingWhotspot: Med gone June-July but some GOM?
  report[which(unique(AllDailies$toppid) %in% i), "JunJul_MWH"] <- AllDailies %>%
    drop_na(lon, lat) %>% filter(between(Month, 6, 7), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -79, -73) & between(lat, 31, 36)]) / length(toppid))
  # May: Massively predominantly Med in NW Forage area
  report[which(unique(AllDailies$toppid) %in% i), "May_NWF"] <- AllDailies %>%
    drop_na(MarineZone) %>% filter(Month == 5, toppid == i) %>%
    summarise(Fishdays = length(toppid[MarineZone == "ForageW"]) / length(toppid))
  # June: GOM predominantly offshore in MixingW
  report[which(unique(AllDailies$toppid) %in% i), "JND_WM_DST_o_500"] <- AllDailies %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "MixingW", Month %in% c(6, 11, 12), toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 500]) / length(toppid))
  # November & December: Med more offshore N of 42N
  report[which(unique(AllDailies$toppid) %in% i), "ND_NWF_DST_o_500"] <- AllDailies %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "ForageW", between(Month, 11, 12), toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 500]) / length(toppid))
  # Fish length > 250cm almost never Med (current length not tagged, and based on consistent size-at-age VonB calc, not stock-specific).
  report[which(unique(AllDailies$toppid) %in% i), "Length_o_250"] <- AllDailies %>%
    drop_na(FishLengthCm) %>% filter(toppid == i) %>%
    summarise(Fishdays = length(toppid[FishLengthCm > 250]) / length(toppid))
  # January: Only Med N&E of line coming SE out of GoMaine entrance. E of 62.5W & N of 38.5N
  report[which(unique(AllDailies$toppid) %in% i), "Jan_NWF_Maine"] <- AllDailies %>%
    drop_na(lon, lat) %>% filter(MarineZone == "ForageW",Month == 1, toppid == i) %>%
    summarise(Fishdays = length(toppid[lon > -62.5 & lat > 38.5]) / length(toppid))
  # April: Predominantly Med off Hatteras: E of 75W to 73.5W, 33-35N
  report[which(unique(AllDailies$toppid) %in% i), "Apr_Hatteras"] <- AllDailies %>%
    drop_na(lon, lat) %>% filter(Month == 4, toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -75, -73.5) & between(lat, 33, 35)]) / length(toppid))
  # June-October: Only Med in NW GoMaine off shelf off Cape Cod, 42.25 – 42.75N, 67.75 – 69.25W
  report[which(unique(AllDailies$toppid) %in% i), "JO_CCod"] <- AllDailies %>%
    drop_na(lon, lat) %>% filter(between(Month, 6, 10), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -69.25, -67.75) & (between(lat, 42.25, 42.75) | between(lat, 39.25, 40.25))]) / length(toppid))
  # July/August: Only GOM in St Lawrence river estuary. 48 – 50.5N & W of 65W
  report[which(unique(AllDailies$toppid) %in% i), "JA_GSL_StLR"] <- AllDailies %>%
    drop_na(lon, lat, OceanDepth) %>% filter(MarineZone == "GSL", between(Month, 7, 8), toppid == i) %>%
    summarise(Fishdays = length(toppid[lon > -65 & between(lat, 48, 50.5) & OceanDepth < 300]) / length(toppid))
  # September/October: Port Hood area only GOM 62.5 – 60.5W, 45.5 – 47.5N
  report[which(unique(AllDailies$toppid) %in% i), "SO_GSL_PtHood"] <- AllDailies %>%
    drop_na(lon, lat) %>% filter(MarineZone == "GSL", between(Month, 9, 10), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -62.5, -60.5) & between(lat, 45.5, 47.5)]) / length(toppid))
  # September: Anticosti Island area Med, 48.5 – 50.5N, 63W – 57.5W
  report[which(unique(AllDailies$toppid) %in% i), "Sept_GSL_Anticosti"] <- AllDailies %>%
    drop_na(lon, lat) %>% filter(MarineZone == "GSL", between(Month, 9, 10), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -63, -57.5) & between(lat, 48.5, 50.5)]) / length(toppid))
} # NaNs: 0/0: 0 days satisfying criteria out of 0 days eligible

# Join Stock
report %<>% left_join(AllDailies %>%
                        group_by(toppid) %>%
                        summarise(Stock = first(Stock))) %>%
  select(Stock, everything())

# Create means for skill testing
reportmeans <- report %>%
  group_by(Stock) %>%
  summarise_all(mean, na.rm = T) %>%
  ungroup

# instead of just means, get distributions, create threshold limits, if UNK values fall outside of thresholds then discard.
saveloc <- "/home/simon/Documents/Si Work/Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/SM_ProbAssign/"

# loop histogram plots by stock by variable
# for (i in colnames(report)[3:28]) {
#   mu <- report %>%
#     group_by(Stock) %>%
#     dplyr::summarise(grp.mean = mean(.data[[i]], na.rm = T))
#
#   ggplot(data = report,
#          aes(x = .data[[i]], fill = Stock, colour = Stock)) + # y = ..count..,
#     geom_histogram(aes(y = ..density..),
#                    fill = "white", #
#                    alpha = 0.75,
#                    position = "identity") +
#     labs(y = "Density") + # Count
#     geom_density(alpha = 0.2, fill = NA) + # aes(y = ..count..),
#     geom_vline(data = mu,
#                aes(xintercept = grp.mean,
#                    color = Stock),
#                linetype = "dashed") +
#     scale_colour_manual(values = c(CB_RED, CB_BLUE),
#                         guide = guide_legend(direction = "vertical")) +
#     theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
#                                      title = element_text(size = rel(2)),
#                                      legend.text = element_text(size = rel(1.5)),
#                                      legend.position = c(0.2, 0.88),
#                                      legend.direction = "horizontal",
#                                      legend.title = element_blank(),
#                                      strip.text.x = element_text(size = rel(2)),
#                                      panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
#     ggsave(paste0(saveloc, today(), "_ProbAsignHist_", i, ".png"), # WATL TAG FILTER _WatlTagged
#            plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
#            height = 4/2, units = "in", dpi = 300, limitsize = TRUE)
# }

results <- list() # create blank list for outputs
reportmeanslong <- bind_cols(reportmeans %>%
                               filter(Stock == "GOM") %>%
                               gather(Key, GOM) %>% # convert reportmeans to long format from wide. Should use pivot_longer but gather works
                               select(GOM),
                             reportmeans %>%
                               filter(Stock == "Med") %>%
                               gather(Key, Med) %>%
                               select(Med))

reportmeanslongsave <- as.data.frame(reportmeanslong[3:28,])
rownames(reportmeanslongsave) <- colnames(report)[3:28]
write.csv(reportmeanslongsave, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_AssignedFishMeansLong.csv"), row.names = T)
reportmeanslong <- read_csv(paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_AssignedFishMeansLong.csv"))

# Loop through fish, test values against means
for (i in unique(AllDailies$toppid)) {
  results[[which(unique(AllDailies$toppid) %in% i)]] <- report %>%
    filter(toppid == i) %>%
    gather(Key, Value) %>% # report is df, gather works
    bind_cols(bind_rows(data.frame(X1 = as.character(c(NA, NA)), # need to add 2 dummy rows at the start of reportmeanslong to make it the same nrow as gathered report
                                   GOM = as.numeric(c(NA, NA)),
                                   Med = as.numeric(c(NA, NA))),
                        reportmeanslong)) %>%
    mutate(Stock = first(Value),
           toppid = Value[2]) %>%
    select(toppid, Stock, everything()) %>%
    filter(!Key %in% c("Stock", "toppid")) %>%
    mutate(Value = as.numeric(Value),
           GOM = as.numeric(GOM),
           Med = as.numeric(Med)) %>%
    rowwise %>%
    mutate(WhichMin = unlist(list(which.min(c(GOM, Med))))) %>%
    ungroup %>%
    mutate(PctGomMean = ifelse(Value == 0, ifelse(WhichMin == 1, 1, 0), GOM / Value), # value as % of GOM. If value=0, make 1 if min is GOM
           PctMedMean = ifelse(Value == 0, ifelse(WhichMin == 2, 1, 0), Med / Value),
           PctGomMeanMod = abs(1 - PctGomMean), # closest to 1
           PctMedMeanMod = abs(1 - PctMedMean)) %>%
    rowwise %>%
    mutate(GomMed = ifelse(is.na(PctGomMeanMod), NA, which.min(c(PctGomMeanMod, PctMedMeanMod)))) %>% # after closest to 1 operation before. NAs break it.
    ungroup %>%
    mutate(GomMed = ifelse(is.na(GomMed), NA, ifelse(GomMed == 1, "GOM", "Med"))) %>%
    rowwise %>%
    # if value >= highest mean then score fish 1 point for that stock (highest), else NA (conservative, doesn't score middle values)
    mutate(GomMed = ifelse(Value >= max(c(GOM, Med), na.rm = T), ifelse(WhichMin == 1, "Med", "GOM"), NA), # reverse whichmin from above since were using max
           GomMed = ifelse((Key == "Lat_o_50" & Value > 0) | #values are being tested against the assigned fish stock group means,
                             (Key == "GSL_MeanEtemp_u_9" & Value > 0) | # which will be zero for those stock/test combos.
                             (Key == "NWF_DST_o_800" & Value > 0) | # And not-quite zero for the other stock.
                             (Key == "NWF_OcDep_o_5275" & Value > 0) | # if the fish's value is closer to 0 than the other stock mean,
                             (Key == "Jan_NWF_Maine" & Value > 0), # it'll count as the zero stock, rather than the non-zero stock.
                           "Med", GomMed), # this block overwrites final GOM/Med result if the Value is > 0
           GomMed = ifelse((Key == "Lon_u_80.2W" & Value > 0) |
                             (Key == "NWF_MaxDepth_o_900" & Value > 0) |
                             (Key == "WM_StepL_o_600k" & Value > 0) |
                             (Key == "June_MarZ_e_GSL" & Value > 0),
                           "GOM", GomMed)) %>%
    ungroup
}
# Could be Med filters since GOM almost never do it?
# GSL_MinEtemp_u_0 GOM 0.003
# WM_DST_o_1600 GOM 0.0003

# Neither do it? Remove?
# NWF_MaxDepth_o_900 GOM 0.002 Med 0


results %<>%
  bind_rows() %>% # append all fish to one big table
  mutate(toppid = as.numeric(toppid))

# save intermediary result for later
write.csv(results, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_AssignedFishAllScores.csv"), row.names = F)

results %<>%
  group_by(toppid) %>% # summarise table, per fish: toppid, stock, med score, gom score, which.max, success/fail?
  summarise(Stock = first(Stock),
            GomValueSum = sum(PctGomMeanMod, na.rm = T),
            MedValueSum = sum(PctMedMeanMod, na.rm = T),
            GomScore = length(which(GomMed == "GOM")),
            MedScore = length(which(GomMed == "Med"))) %>%
  rowwise %>%
  mutate(HighestScore = ifelse(GomScore == MedScore, 0, which.max(c(GomScore, MedScore))), # score 0 if tied
         LowestValue = ifelse(GomValueSum == MedValueSum, 0, which.min(c(GomValueSum, MedValueSum)))) %>%  # score 0 if tied
  ungroup %>%
  mutate(TotalScore = GomScore + MedScore,
         GomMed = ifelse(is.na(HighestScore), NA, ifelse(HighestScore == 1, "GOM", "Med")),
         GomMedValue = ifelse(is.na(LowestValue), NA, ifelse(LowestValue == 1, "GOM", "Med")),
         Correct = GomMed == Stock,
         CorrectValue = GomMedValue == Stock)

# summarise summary: count successes, fails
print(paste0(nrow(results),
             " already-assigned fish were analysed using ",
             ncol(report) - 2,
             " tests. ",
             length(which(results$Correct)),
             " were correctly classified (",
             round(length(which(results$Correct)) / length(results$Correct) * 100, 2),
             "%). Mean number of tests qualified for per fish was ",
             round(mean(results$TotalScore), 1),
             " i.e. ",
             round(mean(results$TotalScore) / (ncol(report) - 2) * 100, 1),
             "%."))
# "118 already-assigned fish were analysed using 26 tests.
# 115 were correctly classified (97.46%)
# Mean number of tests qualified for per fish was 3.6 i.e. 13.8%

print(paste0(nrow(results),
             " already-assigned fish were analysed using ",
             ncol(report) - 2,
             " tests. ",
             length(which(results$CorrectValue)),
             " were correctly classified (",
             round(length(which(results$CorrectValue)) / length(results$CorrectValue) * 100, 2),
             "%). Mean number of tests qualified for per fish was ",
             round(mean(results$TotalScore), 1),
             " i.e. ",
             round(mean(results$TotalScore) / (ncol(report) - 2) * 100, 1),
             "%."))
# "118 already-assigned fish were analysed using 26 tests. 103 were correctly classified (87.29%)
# according to CorrectValue approach

report %<>%
  bind_cols(results %>%
              select(-toppid, -Stock))

write.csv(report, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_AssignedFish.csv"), row.names = F)

# produce accuracy metrics (TSS AUC etc)? Not valid, need presence absence data.
# Does excluding any metrics increase the accuracy? Leave-one-out testing.
# Test UNKNOWNs against criteria, discuss results - how do unassigned fish compare to assigned fish in terms of tag length, fish length, anything else biasing/caveats?

AllDailiesUnk <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds"))
AllDailiesUnk %<>%
  ungroup %>% # if grouped when saved, can cause issues
  filter(!str_detect(seriesname, "Gulf|Corsica|Cartagena|Ireland|France|WWF|Morocco|Israel"), # Manually remove ineligible tag series
         !Stock %in% c("GOM", "Med")) %>% # REMOVE assigned GOM & Med fish
  drop_na(Date, lat, lon) %>% # omit rows with NA values
  mutate(DataType = str_sub(fishID, -1, -1), # # "C" "D" "P"
         Day = lubridate::yday(Date)) %>%
  group_by(toppid, Date) %>% # remove dupes nrows from 119730 to ? to 40408
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(~ is.character(.) | is.POSIXt(.)), first)) %>% # library(lubridate)
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
         across(where(~ is.character(.)), ~ ifelse(is.nan(.), NA, .))) %>% # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
  ungroup

# build report for unknowns
reportUnk <- data.frame("Stock" = as.character(rep("UNK", length(unique(AllDailies$toppid)))),  # build report
                        "toppid" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Lat_o_50" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Lon_o_55W" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Lon_u_80.2W" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "GSL_MinEtemp_u_0" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "GSL_MaxDepth_oe_90" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "GSL_MeanEtemp_u_9" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "NWF_MaxDepth_o_900" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "NWF_Hrs50_e_24" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "NWF_DST_o_800" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "NWF_OcDep_o_5275" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "WM_Hrs50_u_9" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "WM_StepL_o_600k" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "WM_DST_o_1600" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "WM_Lat_u_30N" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "June_MarZ_e_GSL" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "JunJul_MWH" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "May_NWF" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "JND_WM_DST_o_500" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "ND_NWF_DST_o_500" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Length_o_250" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Jan_NWF_Maine" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Apr_Hatteras" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "JO_CCod" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "JA_GSL_StLR" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "SO_GSL_PtHood" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))),
                        "Sept_GSL_Anticosti" = as.numeric(rep(NA, length(unique(AllDailies$toppid)))))


# run tests
for (i in unique(AllDailiesUnk$toppid)) {
  print(paste0(
    which(unique(AllDailiesUnk$toppid) %in% i),
    "/", length(unique(AllDailiesUnk$toppid)),
    ": ", round(which(unique(AllDailiesUnk$toppid) %in% i) / length(unique(AllDailiesUnk$toppid)) * 100, 1),
    "%"))
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "toppid"] <- i
  # GOM never above 50°N
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Lat_o_50"] <- AllDailiesUnk %>% # send outputs to reportUnk
    drop_na(lat) %>% filter(toppid == i) %>% summarise(Fishdays = length(toppid[lat > 50]) / length(toppid))
  # GOM rarely E of 55°W
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Lon_o_55W"] <- AllDailiesUnk %>%
    drop_na(lon) %>% filter(toppid == i) %>% summarise(Fishdays = length(toppid[lon > -55]) / length(toppid))
  # Med fish never W of 80.2°W
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Lon_u_80.2W"] <- AllDailiesUnk %>%
    drop_na(lon) %>% filter(toppid == i) %>% summarise(Fishdays = length(toppid[lon < -80.2]) / length(toppid))
  # Mostly Med fish below 0°C
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "GSL_MinEtemp_u_0"] <- AllDailiesUnk %>%
    drop_na(MinETemp24h) %>% filter(MarineZone == "GSL", toppid == i) %>%
    summarise(Fishdays = length(toppid[MinETemp24h < 0]) / length(toppid))
  # GSL: MaxDepth >90m more likely to be Med
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "GSL_MaxDepth_oe_90"] <- AllDailiesUnk %>%
    filter(MarineZone == "GSL", toppid == i) %>% drop_na(MaxDepth24h) %>%
    summarise(Fishdays = length(toppid[MaxDepth24h >= 90]) / length(toppid))
  # GSL: daily mean etemp, GOM never <9°C
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "GSL_MeanEtemp_u_9"] <- AllDailiesUnk %>%
    drop_na(MeanETemp24h) %>% filter(MarineZone == "GSL", toppid == i) %>%
    summarise(Fishdays = length(toppid[MeanETemp24h < 9]) / length(toppid))
  # NWforage: Max depth: Med never >900m
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "NWF_MaxDepth_o_900"] <- AllDailiesUnk %>%
    drop_na(MaxDepth24h) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[MaxDepth24h > 900]) / length(toppid))
  # NWforage: GOM much more likely to spend 24 hrs within 50m depth?
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "NWF_Hrs50_e_24"] <- AllDailiesUnk %>%
    drop_na(Hrs50mLesDepRange) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[Hrs50mLesDepRange == 24]) / length(toppid))
  # NWforage: GOM never > ~800km from shore?
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "NWF_DST_o_800"] <- AllDailiesUnk %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 800]) / length(toppid))
  # NWforage: GOM Never >5.3km OcDep?
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "NWF_OcDep_o_5275"] <- AllDailiesUnk %>%
    drop_na(OceanDepth) %>% filter(MarineZone == "ForageW", toppid == i) %>%
    summarise(Fishdays = length(toppid[OceanDepth > 5275]) / length(toppid))
  # WMix: GOM >> more likely to have HrsW<=50mDepRange <9
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "WM_Hrs50_u_9"] <- AllDailiesUnk %>%
    drop_na(Hrs50mLesDepRange) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[Hrs50mLesDepRange < 9]) / length(toppid))
  # WMix: No Med steplengthsbl > 600k
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "WM_StepL_o_600k"] <- AllDailiesUnk %>%
    drop_na(StepLengthBL) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[StepLengthBL > 600000]) / length(toppid))
  # WMix: No GOM DistToShore > 1600Km
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "WM_DST_o_1600"] <- AllDailiesUnk %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 1600]) / length(toppid))
  # WMix: Hardly any Med below 30°N
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "WM_Lat_u_30N"] <- AllDailiesUnk %>%
    drop_na(lat) %>% filter(MarineZone == "MixingW", toppid == i) %>%
    summarise(Fishdays = length(toppid[lat < 30]) / length(toppid))
  # Only GOM in GSL in June
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "June_MarZ_e_GSL"] <- AllDailiesUnk %>%
    drop_na(MarineZone) %>% filter(Month == 6, toppid == i) %>%
    summarise(Fishdays = length(toppid[MarineZone == "GSL"]) / length(toppid))
  # MixingWhotspot: Med gone June-July but some GOM?
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "JunJul_MWH"] <- AllDailiesUnk %>%
    drop_na(lon, lat) %>% filter(between(Month, 6, 7), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -79, -73) & between(lat, 31, 36)]) / length(toppid))
  # May: Massively predominantly Med in NW Forage area
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "May_NWF"] <- AllDailiesUnk %>%
    drop_na(MarineZone) %>% filter(Month == 5, toppid == i) %>%
    summarise(Fishdays = length(toppid[MarineZone == "ForageW"]) / length(toppid))
  # June: GOM predominantly offshore in MixingW
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "JND_WM_DST_o_500"] <- AllDailiesUnk %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "MixingW", Month %in% c(6, 11, 12), toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 500]) / length(toppid))
  # November & December: Med more offshore N of 42N
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "ND_NWF_DST_o_500"] <- AllDailiesUnk %>%
    drop_na(DistanceToShoreKm) %>% filter(MarineZone == "ForageW", between(Month, 11, 12), toppid == i) %>%
    summarise(Fishdays = length(toppid[DistanceToShoreKm > 500]) / length(toppid))
  # Fish length > 250cm almost never Med (current length not tagged, and based on consistent size-at-age VonB calc, not stock-specific).
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Length_o_250"] <- AllDailiesUnk %>%
    drop_na(FishLengthCm) %>% filter(toppid == i) %>%
    summarise(Fishdays = length(toppid[FishLengthCm > 250]) / length(toppid))
  # January: Only Med N&E of line coming SE out of GoMaine entrance. E of 62.5W & N of 38.5N
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Jan_NWF_Maine"] <- AllDailiesUnk %>%
    drop_na(lon, lat) %>% filter(MarineZone == "ForageW",Month == 1, toppid == i) %>%
    summarise(Fishdays = length(toppid[lon > -62.5 & lat > 38.5]) / length(toppid))
  # April: Predominantly Med off Hatteras: E of 75W to 73.5W, 33-35N
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Apr_Hatteras"] <- AllDailiesUnk %>%
    drop_na(lon, lat) %>% filter(Month == 4, toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -75, -73.5) & between(lat, 33, 35)]) / length(toppid))
  # June-October: Only Med in NW GoMaine off shelf off Cape Cod, 42.25 – 42.75N, 67.75 – 69.25W
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "JO_CCod"] <- AllDailiesUnk %>%
    drop_na(lon, lat) %>% filter(between(Month, 6, 10), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -69.25, -67.75) & (between(lat, 42.25, 42.75) | between(lat, 39.25, 40.25))]) / length(toppid))
  # July/August: Only GOM in St Lawrence river estuary. 48 – 50.5N & W of 65W
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "JA_GSL_StLR"] <- AllDailiesUnk %>%
    drop_na(lon, lat, OceanDepth) %>% filter(MarineZone == "GSL", between(Month, 7, 8), toppid == i) %>%
    summarise(Fishdays = length(toppid[lon > -65 & between(lat, 48, 50.5) & OceanDepth < 300]) / length(toppid))
  # September/October: Port Hood area only GOM 62.5 – 60.5W, 45.5 – 47.5N
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "SO_GSL_PtHood"] <- AllDailiesUnk %>%
    drop_na(lon, lat) %>% filter(MarineZone == "GSL", between(Month, 9, 10), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -62.5, -60.5) & between(lat, 45.5, 47.5)]) / length(toppid))
  # September: Anticosti Island area Med, 48.5 – 50.5N, 63W – 57.5W
  reportUnk[which(unique(AllDailiesUnk$toppid) %in% i), "Sept_GSL_Anticosti"] <- AllDailiesUnk %>%
    drop_na(lon, lat) %>% filter(MarineZone == "GSL", between(Month, 9, 10), toppid == i) %>%
    summarise(Fishdays = length(toppid[between(lon, -63, -57.5) & between(lat, 48.5, 50.5)]) / length(toppid))
} # NaNs: 0/0: 0 days satisfying criteria out of 0 days eligible

resultsUnk <- list() # create blank list for outputs
# Loop through fish, test values against means
for (i in unique(AllDailiesUnk$toppid)) {
  print(paste0(
    which(unique(AllDailiesUnk$toppid) %in% i),
    "/", length(unique(AllDailiesUnk$toppid)),
    ": ", round(which(unique(AllDailiesUnk$toppid) %in% i) / length(unique(AllDailiesUnk$toppid)) * 100, 1),
    "%"))
  resultsUnk[[which(unique(AllDailiesUnk$toppid) %in% i)]] <- reportUnk %>%
    filter(toppid == i) %>%
    gather(Key, Value) %>% # report is df, gather works
    bind_cols(bind_rows(data.frame(X1 = as.character(c(NA, NA)), # need to add 2 dummy rows at the start of reportmeanslong to make it the same nrow as gathered report
                                   GOM = as.numeric(c(NA, NA)),
                                   Med = as.numeric(c(NA, NA))),
                        reportmeanslong)) %>%
    mutate(Stock = first(Value),
           toppid = Value[2]) %>%
    select(toppid, Stock, everything()) %>%
    filter(Key != "Stock", Key != "toppid") %>%
    mutate(Value = as.numeric(Value),
           GOM = as.numeric(GOM),
           Med = as.numeric(Med)) %>%
    rowwise %>%
    mutate(WhichMin = unlist(list(which.min(c(GOM, Med))))) %>%
    ungroup %>%
    mutate(PctGomMean = ifelse(Value == 0, ifelse(WhichMin == 1, 1, 0), GOM / Value), # value as % of GOM. If value=0, make 1 if min is GOM
           PctMedMean = ifelse(Value == 0, ifelse(WhichMin == 2, 1, 0), Med / Value),
           PctGomMeanMod = abs(1 - PctGomMean), # closest to 1
           PctMedMeanMod = abs(1 - PctMedMean)) %>%
    rowwise %>%
    mutate(GomMed = ifelse(is.na(PctGomMeanMod), NA, which.min(c(PctGomMeanMod, PctMedMeanMod)))) %>% # after closest to 1 operation before. NAs break it.
    ungroup %>%
    mutate(GomMed = ifelse(is.na(GomMed), NA, ifelse(GomMed == 1, "GOM", "Med"))) %>%
    rowwise %>%
    # if value >= highest mean then score fish 1 point for that stock (highest), else NA (conservative, doesn't score middle values)
    mutate(GomMed = ifelse(Value >= max(c(GOM, Med), na.rm = T), ifelse(WhichMin == 1, "Med", "GOM"), NA), # reverse whichmin from above since were using max
           GomMed = ifelse((Key == "Lat_o_50" & Value > 0) | #values are being tested against the assigned fish stock group means,
                             (Key == "GSL_MeanEtemp_u_9" & Value > 0) | # which will be zero for those stock/test combos.
                             (Key == "NWF_DST_o_800" & Value > 0) | # And not-quite zero for the other stock.
                             (Key == "NWF_OcDep_o_5275" & Value > 0) | # if the fish's value is closer to 0 than the other stock mean,
                             (Key == "Jan_NWF_Maine" & Value > 0), # it'll count as the zero stock, rather than the non-zero stock.
                           "Med", GomMed), # this block overwrites final GOM/Med result if the Value is > 0
           GomMed = ifelse((Key == "Lon_u_80.2W" & Value > 0) |
                             (Key == "NWF_MaxDepth_o_900" & Value > 0) |
                             (Key == "WM_StepL_o_600k" & Value > 0) |
                             (Key == "June_MarZ_e_GSL" & Value > 0),
                           "GOM", GomMed)) %>%
    ungroup
}

resultsUnk %<>%
  bind_rows() %>% # append all fish to one big table
  mutate(toppid = as.numeric(toppid))

write.csv(resultsUnk, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_UnAssignedFishAllScores.csv"), row.names = F)
resultsUnk <- read_csv(paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_UnAssignedFishAllScores.csv"))

resultsUnk %<>%
  group_by(toppid) %>% # summarise table, per fish: toppid, stock, med score, gom score, which.max, success/fail?
  summarise(Stock = first(Stock),
            GomValueSum = sum(PctGomMeanMod, na.rm = T),
            MedValueSum = sum(PctMedMeanMod, na.rm = T),
            GomScore = length(which(GomMed == "GOM")),
            MedScore = length(which(GomMed == "Med"))) %>%
  rowwise %>%
  mutate(HighestScore = ifelse(GomScore == MedScore, 0, which.max(c(GomScore, MedScore))), # score 0 if tied
         LowestValue = ifelse(GomValueSum == MedValueSum, 0, which.min(c(GomValueSum, MedValueSum)))) %>%  # score 0 if tied
  ungroup %>%
  mutate(TotalScore = GomScore + MedScore,
         GomMed = ifelse(is.na(HighestScore), NA, ifelse(HighestScore == 0, NA, ifelse(HighestScore == 1, "GOM", "Med"))), # NA if NA or 0 (passes no tests)
         GomMedValue = ifelse(is.na(LowestValue), NA, ifelse(LowestValue == 1, "GOM", "Med")))

# summarise summary: how many GOM Med
print(paste0(nrow(resultsUnk),
             " unassigned fish were analysed using ",
             ncol(reportUnk) - 2,
             " tests. ",
             length(which(resultsUnk$GomMed == "GOM")),
             " were classified as GOM (",
             round(length(which(resultsUnk$GomMed == "GOM")) / length(resultsUnk$GomMed) * 100, 2),
             "%), ",
             length(which(resultsUnk$GomMed == "Med")),
             " were classified as Med (",
             round(length(which(resultsUnk$GomMed == "Med")) / length(resultsUnk$GomMed) * 100, 2),
             "%), ",
             length(which(is.na(resultsUnk$GomMed))),
             " remained unclassified due to passing zero tests (",
             round(length(which(is.na(resultsUnk$GomMed))) / length(resultsUnk$GomMed) * 100, 2),
             "%). Mean number of tests qualified for per fish was ",
             round(mean(resultsUnk$TotalScore), 1),
             " i.e. ",
             round(mean(resultsUnk$TotalScore) / (ncol(reportUnk) - 2) * 100, 1),
             "%."))
# "290 unassigned fish were analysed using 26 tests.
# 71 were classified as GOM (24.48%) 2020.11.17
# 119 were classified as Med (41.03%) 2020.11.19
# 100 remained unclassified due to passing zero tests (34.48%)
# Mean number of tests qualified for per fish was 1.8 i.e. 5.1%."

# Mean number of tests qualified for per fish, by assigned stock:
resultsUnk %>%
  mutate(GomMed = replace_na(GomMed, "UNK")) %>%
  group_by(GomMed) %>%
  summarise(TotalScore = mean(TotalScore))
# GomMed         TotalScore
# 1 GOM          2.90
# 2 Med          2.13
# 3 UNK          0.6

newGOMtoppids <- resultsUnk %>%
  filter(GomMed == "GOM") %>%
  pull(toppid)
newMedtoppids <- resultsUnk %>%
  filter(GomMed == "Med") %>%
  pull(toppid)

AdTmp <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds")) %>%
  ungroup %>% # if grouped when saved, can cause issues
  filter(!str_detect(seriesname, "Gulf|Corsica|Cartagena|Ireland|France|WWF|Morocco|Israel"), # Manually remove ineligible tag series
         toppid %in% c(newGOMtoppids, newMedtoppids)) %>% # only new GOM & Med
  drop_na(Date, lat, lon) %>% # omit rows with NA values
  group_by(toppid, Date) %>% # remove dupes nrows from 119730 to ? to 40408
  select(toppid, Date, lat, lon) %>% # reduce columns to work on for dupe removal
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(~ is.character(.) | is.POSIXt(.)), first)) %>% # library(lubridate)
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
         across(where(~ is.character(.)), ~ ifelse(is.nan(.), NA, .))) %>% # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
  ungroup
AdTmp %>% filter(toppid %in% newGOMtoppids) %>% nrow() # 10626 # 2020-11-25:6519
AdTmp %>% filter(toppid %in% newMedtoppids) %>% nrow() # 21419 # 2020-11-25:28348
# 32045 total. 32045/30055 = 1.066212 = 107% of original database, 2.07X
rm(AdTmp)

reportUnk %<>%
  bind_cols(resultsUnk %>%
              select(-toppid, -Stock))

write.csv(reportUnk, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", today(), "_ProbabilityAssignmentResults_UnknownFish.csv"), row.names = F)
reportUnk <- read_csv(paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/", "2020-11-30", "_ProbabilityAssignmentResults_UnknownFish.csv"))
# variance etc of tests passed
summary(reportUnk$TotalScore)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00    0.00    1.00    1.79    3.00   11.00
var(reportUnk$TotalScore) # 3.267021
sd(reportUnk$TotalScore) # 1.80749

# list of toppids of resultsUnk GOM & Med fish. Tracks of those? etc??
algorithmGOM <- unique(reportUnk[which(reportUnk$GomMed == "GOM"), "toppid"]) %>% pull(toppid)
algorithmMed <- unique(reportUnk[which(reportUnk$GomMed == "Med"), "toppid"]) %>% pull(toppid)
# plot the size distributions

AllDailies <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds"))
# re-read in original data, but don't filter for only gom/med tagged fish
AllDailies %<>%
  ungroup %>% # if grouped when saved, can cause issues
  filter(!str_detect(seriesname, "Gulf|Corsica|Cartagena|Ireland|France|WWF|Morocco|Israel")) %>% # Manually remove ineligible tag series
  # DON'T FILTER FOR ONLY GOM OR MED I.E. LEAVE UNKNOWNS IN
  drop_na(Date, lat, lon) %>% # omit rows with NA values
  mutate(DataType = str_sub(fishID, -1, -1), # # "C" "D" "P"
         Day = lubridate::yday(Date)) %>%
  group_by(toppid, Date) %>% # remove dupes
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(~ is.character(.) | is.POSIXt(.)), first)) %>% # library(lubridate)
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
         across(where(~ is.character(.)), ~ ifelse(is.nan(.), NA, .))) %>% # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
  ungroup

AllDailies[which(AllDailies$toppid %in% as.integer(algorithmGOM$toppid)), "Stock"] <- "algorithmGOM"
AllDailies[which(AllDailies$toppid %in% as.integer(algorithmMed$toppid)), "Stock"] <- "algorithmMed"
# unique(AllDailies$Stock) # "algorithmGOM" NA             "algorithmMed" "Med"          "GOM"
AllDailies$Stock %<>% replace_na("UNK")
# Now re-do Fig2 2D barplots above


#Compare to Carol's genetic assignment
# The genetic assignments from Carol’s recent (2019 was the last update) data are in tblabft_dna.

# connectTOPP: tblabft_dna = tbl(con, "tblabft_dna") %>% collect() # 2020.09.22 acoustic pings @ GSL OTN (& elsewhere?) for ABFT
tblabft_dna %<>%
  filter(run == "2019dec3") %>% # You want to filter by the ‘run’ field where the most recent value is ‘2019dec3’.
  select(-tkey, -comments, -run) %>% #remove comments & tkey columns
  rename(assignment_G = assignment)
write.csv(tblabft_dna, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/tblabft_dna.csv"), row.names = F) # save csv
# test against reportUnk. Want TF F1 false positive style output. Merge lists.
algorithmGOM %<>%
  as_tibble() %>%
  rename(toppid = value) %>%
  mutate(assignment_PA = "GOM")
algorithmMed %<>%
  as_tibble() %>%
  rename(toppid = value) %>%
  mutate(assignment_PA = "MED")
algorithm <- rbind(algorithmGOM, algorithmMed) %>%
  arrange(toppid)
tblabft_dna %<>%
  full_join(algorithm) %>%
  arrange(toppid)
write.csv(tblabft_dna, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/tblabft_dna_and_pa.csv"), row.names = F) # save csv
tblabft_dna_pa <- tblabft_dna %>%
  drop_na(assignment_G, assignment_PA)
write.csv(tblabft_dna_pa, paste0("/home/simon/Dropbox/Blocklab Monterey/Data/HomeRangeChange/tblabft_dna_and_pa_only.csv"), row.names = F) # save csv
library(caret)
confmat <- caret::confusionMatrix(data = factor(tblabft_dna_pa$assignment_PA), # predicted
                                  reference = factor(tblabft_dna_pa$assignment_G)) # true results
# positive = NULL,
# dnn = c("Prediction", "Reference"), ...)
confmat$table
#           Reference
# Prediction GOM MED
#        GOM  18   9
#        MED   9  14

#                Accuracy : 0.64
#                  95% CI : (0.4919, 0.7708)
#     No Information Rate : 0.54
#     P-Value [Acc > NIR] : 0.1002
#                   Kappa : 0.2754
#  Mcnemar's Test P-Value : 1.0000
#             Sensitivity : 0.6667
#             Specificity : 0.6087
#          Pos Pred Value : 0.6667
#          Neg Pred Value : 0.6087
#              Prevalence : 0.5400
#          Detection Rate : 0.3600
#    Detection Prevalence : 0.5400
#       Balanced Accuracy : 0.6377
#        'Positive' Class : GOM





# Fig SM size distribution####
saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/AgeLonStocksPlot/")
mu <- AllDailies %>%
  group_by(Stock) %>%
  dplyr::summarise(grp.mean = mean(FishLengthCm, na.rm = T))

ggplot(data = AllDailies,
       aes(x = FishLengthCm, fill = Stock, colour = Stock)) + # y = ..count..,
  labs(y = "Count", x = "Fish Length (cm)") + # Count
  geom_density(aes(y = ..count..), alpha = 0.2, fill = NA) + # aes(y = ..count..),
  geom_vline(data = mu,
             aes(xintercept = grp.mean,
                 color = Stock),
             linetype = "dashed") +
  scale_colour_manual(values = c("violetred2", "deepskyblue", CB_RED, CB_BLUE, "black"),
                      guide = guide_legend(direction = "vertical")) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.2, 0.88),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_SizeDistributionUnk_SM.png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)




# Fig SM tag duration distribution####
dur <- AllDailies %>%
  group_by(Stock, toppid) %>%
  dplyr::summarise(Duration = length(Date)) %>%
  ungroup

mu <- dur %>%
  group_by(Stock) %>%
  dplyr::summarise(grp.mean = mean(Duration, na.rm = T))

ggplot(data = dur,
       aes(x = Duration, fill = Stock, colour = Stock)) + # y = ..count..,
  labs(y = "Count", x = "Days at liberty") + # Count
  geom_density(aes(y = ..count..), alpha = 0.2, fill = NA) +
  geom_vline(data = mu,
             aes(xintercept = grp.mean,
                 color = Stock),
             linetype = "dashed") +
  scale_colour_manual(values = c("violetred2", "deepskyblue", CB_RED, CB_BLUE, "black"),
                      guide = guide_legend(direction = "vertical")) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.8, 0.88),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_DurationDistributionUnk_SM.png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)

dur <- AllDailies %>%
  group_by(Stock, toppid) %>%
  dplyr::summarise(Duration = log1p(length(Date))) %>%
  ungroup

mu <- dur %>%
  group_by(Stock) %>%
  dplyr::summarise(grp.mean = mean(Duration, na.rm = T))

ggplot(data = dur,
       aes(x = Duration, fill = Stock, colour = Stock)) + # y = ..count..,
  labs(y = "Count", x = "Days at liberty (log1p)") + # Count
  geom_density(aes(y = ..count..), alpha = 0.2, fill = NA) + # aes(y = ..count..),
  geom_vline(data = mu,
             aes(xintercept = grp.mean,
                 color = Stock),
             linetype = "dashed") +
  scale_colour_manual(values = c("violetred2", "deepskyblue", CB_RED, CB_BLUE, "black"),
                      guide = guide_legend(direction = "vertical")) +
  theme_minimal() %+replace% theme(axis.text = element_text(size = rel(2)),
                                   title = element_text(size = rel(2)),
                                   legend.text = element_text(size = rel(1.5)),
                                   legend.position = c(0.8, 0.88),
                                   legend.direction = "horizontal",
                                   legend.title = element_blank(),
                                   strip.text.x = element_text(size = rel(2)),
                                   panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  ggsave(paste0(saveloc, today(), "_LogDurationDistributionUnk_SM.png"),
         plot = last_plot(), device = "png", path = "", scale = 3.5, width = 8/3,
         height = 4/2, units = "in", dpi = 300, limitsize = TRUE)






# known vs algorithm tracks ####
# plot known-GOM vs. estimated GOM and known-MED vs estimated MED, then one figure, 4 sub-panels
sfAllDailies <- sf::st_as_sf(AllDailies, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) #points by day

sf_lines <- sfAllDailies %>%
  dplyr::group_by(toppid) %>% # FishLengthCm but relies on age-length curves
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("MULTILINESTRING") %>% # nrow 177
  left_join(AllDailies %>%
              select(toppid, Stock) %>% # add stock
              distinct) %>%
  left_join(reportUnk %>% # resultsUnk
              select(toppid, TotalScore) %>%
              distinct)

# new column, gom types, med types, facet by that, then within facets, colour by stock i.e. gom vs algorithmgom
sf_lines %<>%
  mutate(StockTypes = recode(Stock,
                             "GOM" = "GOM",
                             "algorithmGOM" = "GOM",
                             "Med" = "Med",
                             "algorithmMed" = "Med"))

# GOMextents <- sf::st_as_sf(data.frame(lon = c(-95, -9), #set manual extents to force the same map size each time
#                                       lat = c(10,65)),
#                            coords = c("lon","lat")) %>%
#   sf::st_set_crs(4326) #using jlondon example, may be incorrect? Is N.Pac

GOMextents <- sf::st_as_sf(data.frame(lon = c(-95, 30), # Emil request for both to use same scale
                                      lat = c(10,65)),
                           coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) #using jlondon example, may be incorrect? Is N.Pac

ggplot() +
  layer_spatial(GOMextents, size = 0, col = "white") + #layer_spatial trains scales, annotation_ doesnt
  annotation_spatial(sf_lines %>% filter(Stock != "UNK",
                                         StockTypes == "GOM"),
                     size = 0.25, #line thickness on map & legend
                     aes(color = factor(Stock, levels = c("GOM", "algorithmGOM"))), #colour on map & legend
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  annotation_spatial(natlantic, fill = "grey", lwd = 0) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.8, 0.8), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 0.9))) + # rows for legend, thickness of lines (not relative to 1)
  scale_colour_manual(values = c(CB_RED, "black")) + # , CB_BLUE, "black"
  scale_x_continuous(breaks = seq(-180, 180, by = 10)) + # sets x ticks frequency
  ggtitle(paste0("ABFT tracks, GOM vs algorithm-assigned GOM fish; western-tagged"), # stock and E/W Atlantic tag location
          subtitle = paste0("GOM n=", sf_lines %>% filter(Stock == "GOM") %>% pull(toppid) %>% unique %>% length,
                            ", algorithmGOM n=", sf_lines %>% filter(Stock == "algorithmGOM") %>% pull(toppid) %>% unique %>% length)) +
  ggsave(paste0(saveloc, "Maps/", today(), "_FacetByStockincAlgorithmStocksGOM.png"), # noUnk
         plot = last_plot(), device = "png", path = "", scale = 2, width = 3.44,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)
# 3 height = 75 degrees of lat. GOM lon -95:-9 = 86. Med lon -80:30=110
# 3/75 # = 0.04 degrees per inch
# 0.04 * 86 # 3.44 GOM
# 0.04 * 110 # 4.4 Med

# Medextents <- sf::st_as_sf(data.frame(lon = c(-80, 30), #set manual extents to force the same map size each time
#                                       lat = c(10,65)),
#                            coords = c("lon","lat")) %>%
#   sf::st_set_crs(4326) #using jlondon example, may be incorrect? Is N.Pac
Medextents <- GOMextents # emil request

ggplot() +
  layer_spatial(Medextents, size = 0, col = "white") +
  annotation_spatial(sf_lines %>% filter(Stock != "UNK",
                                         StockTypes == "Med"),
                     size = 0.25, #line thickness on map & legend
                     aes(color = factor(Stock, levels = c("Med", "algorithmMed"))), #colour on map & legend
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  annotation_spatial(natlantic, fill = "grey", lwd = 0) + #layer_spatial trains scales, annotation_ doesnt #
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.8, 0.8), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 0.9))) + # rows for legend, thickness of lines (not relative to 1)
  scale_colour_manual(values = c(CB_BLUE, "black")) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10)) + # sets x ticks frequency
  ggtitle(paste0("ABFT tracks, Med vs algorithm-assigned Med fish; western-tagged"), # stock and E/W Atlantic tag location
          subtitle = paste0("Med n=", sf_lines %>% filter(Stock == "Med") %>% pull(toppid) %>% unique %>% length,
                            ", algorithmMed n=", sf_lines %>% filter(Stock == "algorithmMed") %>% pull(toppid) %>% unique %>% length)) +
  ggsave(paste0(saveloc, "Maps/", today(), "_FacetByStockincAlgorithmStocksMed.png"),
         plot = last_plot(), device = "png", path = "", scale = 2, width = 4.4,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)


# track lines coloured by TotalScore FIGURE 10####
UNKextents <- sf::st_as_sf(data.frame(lon = c(-82, -6), #set manual extents to force the same map size each time
                                      lat = c(10,65)),
                           coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) #using jlondon example, may be incorrect? Is N.Pac

ggplot() +
  annotation_spatial(natlantic, fill = "grey", lwd = 0) + #layer_spatial trains scales, annotation_ doesnt #
  layer_spatial(UNKextents, size = 0, col = "white") +
  annotation_spatial(sf_lines %>% filter(Stock == "algorithmGOM"), #  %>% filter(Stock != "UNK", DataType != "P")
                     size = 0.25, #line thickness on map & legend
                     aes(color = TotalScore), #colour on map & legend
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.2, 0.8), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 0.9))) + # rows for legend, thickness of lines (not relative to 1)
  scale_colour_gradientn(colours = heat.colors(12, rev = T),
                         limits = c(0,11),
                         breaks = seq(from = 0, to = 11, by = 1)) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10)) + # sets x ticks frequency
  ggtitle(paste0("ABFT tracks by algorithm-defined Stock"), # stock and E/W Atlantic tag location
          subtitle = paste0("algorithmGOM n=", sf_lines %>% filter(Stock == "algorithmGOM") %>% pull(toppid) %>% unique %>% length)) +
  ggsave(paste0(saveloc, "Maps/", today(), "_FacetByAlgorithmStockTotalScoreColourGOM.png"), # noUnk
         plot = last_plot(), device = "png", path = "", scale = 2, width = 3, # 0.04 * 71 # 2.84 Unk
         height = 3, units = "in", dpi = 300, limitsize = TRUE)

ggplot() +
  annotation_spatial(natlantic, fill = "grey", lwd = 0) + #layer_spatial trains scales, annotation_ doesnt #
  layer_spatial(UNKextents, size = 0, col = "white") +
  annotation_spatial(sf_lines %>% filter(Stock == "algorithmMed"), #  %>% filter(Stock != "UNK", DataType != "P")
                     size = 0.25, #line thickness on map & legend
                     aes(color = TotalScore), #colour on map & legend
                     key_glyph = "path") + #legend symbol as line. Other option "rect" & unknown others. Default (omitted) makes boxes
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.2, 0.8), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'cm'), #compress spacing between legend items, this is min
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"), # white background
        legend.key = element_blank()) + # removed whitespace buffer around legend boxes which is nice
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 0.9))) + # rows for legend, thickness of lines (not relative to 1)
  scale_colour_gradientn(colours = heat.colors(12, rev = T),
                         limits = c(0,11),
                         breaks = seq(from = 0, to = 11, by = 1)) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10)) + # sets x ticks frequency
  ggtitle(paste0("ABFT tracks by algorithm-defined Stock"), # stock and E/W Atlantic tag location
          subtitle = paste0("algorithmMed n=", sf_lines %>% filter(Stock == "algorithmMed") %>% pull(toppid) %>% unique %>% length)) +
  ggsave(paste0(saveloc, "Maps/", today(), "_FacetByAlgorithmStockTotalScoreColourMed.png"), # noUnk
         plot = last_plot(), device = "png", path = "", scale = 2, width = 3,
         height = 3, units = "in", dpi = 300, limitsize = TRUE)


range(sf_lines %>% filter(Stock %in% c("algorithmGOM", "algorithmMed")) %>% pull(TotalScore)) # 1:11

# tmp <- AllDailies %>%
#   left_join(resultsUnk %>%
#               select(toppid, TotalScore) %>%
#               distinct) %>%
#   group_by(TotalScore, Stock) %>%
#   select(toppid) %>%
#   distinct %>%
#   summarise(Count = length(toppid)) %>%
#   drop_na(TotalScore)

# slide showing table of identified metrics
# slide showing success of known-stock assignment, then success of unknown assignment
# figure comparing known-GOM with assigned GOM, size distribution and spatial range (i.e., 2 colors of dots on a map)
# same for med
