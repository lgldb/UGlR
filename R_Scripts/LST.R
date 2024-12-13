library(raster)
library(sf)
library(tidyverse)
library(terra)
library(BAMMtools)

#Zeitgrenze für Download erhöhen
#options(timeout=3600)

#Download der LST-Rasterdaten von https://data.ceda.ac.uk/neodc/esacci/land_surface_temperature/data/SENTINEL3B_SLSTR/L3C/0.01/v3.00
#year <- c(2019, 2020)
#month_1 <- c(1:9)
#month_2 <- c(10:12)


#lst_download_path_1 <- tidyr::expand_grid(year, month_1, dn) %>%
#  glue::glue_data("https://dap.ceda.ac.uk/neodc/esacci/land_surface_temperature/data/SENTINEL3B_SLSTR/L3C/0.01/v3.00/monthly/{year}/0{month_1}/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_{dn}-{year}0{month_1}01000000-fv3.00.nc?download=1")
#lst_download_path_2 <- tidyr::expand_grid(year, month_2, dn) %>%
#  glue::glue_data("https://dap.ceda.ac.uk/neodc/esacci/land_surface_temperature/data/SENTINEL3B_SLSTR/L3C/0.01/v3.00/monthly/{year}/{month_2}/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_{dn}-{year}{month_2}01000000-fv3.00.nc?download=1")
#lst_download_path <- c("https://dap.ceda.ac.uk/neodc/esacci/land_surface_temperature/data/SENTINEL3B_SLSTR/L3C/0.01/v3.00/monthly/2018/12/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_DAY-20181201000000-fv3.00.nc?download=1", 
#                       "https://dap.ceda.ac.uk/neodc/esacci/land_surface_temperature/data/SENTINEL3B_SLSTR/L3C/0.01/v3.00/monthly/2018/12/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_NIGHT-20181201000000-fv3.00.nc?download=1", 
#                       lst_download_path_1, 
#                       lst_download_path_2)
  
#lst_nc_path_1 <- tidyr::expand_grid(year, month_1, dn) %>%
#  glue::glue_data("data/LST/raw/nc_files/{dn}/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_{dn}-{year}0{month_1}01000000-fv3.00.nc")
#lst_nc_path_2 <- tidyr::expand_grid(year, month_2, dn) %>%
#  glue::glue_data("data/LST/raw/nc_files/{dn}/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_{dn}-{year}{month_2}01000000-fv3.00.nc")
#lst_nc_path <- c("data/LST/raw/nc_files/DAY/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_DAY-20181201000000-fv3.00.nc", 
#                 "data/LST/raw/nc_files/NIGHT/ESACCI-LST-L3C-LST-SLSTRB-0.01deg_1MONTHLY_NIGHT-20181201000000-fv3.00.nc",
#                 lst_nc_path_1, 
#                 lst_nc_path_2)

#lst_raster_path_1 <- tidyr::expand_grid(year, month_1, dn) %>%
#  glue::glue_data("data/LST/raw/raster/{dn}/LST_DE_{dn}_{year}_0{month_1}.tif")
#lst_raster_path_2 <- tidyr::expand_grid(year, month_2, dn) %>%
#  glue::glue_data("data/LST/raw/raster/{dn}/LST_DE_{dn}_{year}_{month_2}.tif")
#lst_raster_path <- c("data/LST/raw/raster/DAY/LST_DE_DAY_2018_12.tiff", 
#                     "data/LST/raw/raster/NIGHT/LST_DE_NIGHT_2018_12.tif", 
#                     lst_nc_path_1, 
#                     lst_nc_path_2)

#Einlesen der Regionsdaten
regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)
#bbox_DE <- st_bbox(regiostar_sf)

###LST DAY
#Einlesen der Daten für 2020 als Rasterstack, Zuschneiden auf Deutschland und Erstellen von neuem Raster mit Maximalwerten
#lst_day_path <- "data/LST/raw/nc_files/DAY"
#lst_day_files <- list.files(lst_day_path, 
#                            full.names = TRUE)

#lst_d_20_rast <- raster::stack(lst_day_files[14:25], 
#                            varname = "lst")
#lst_d_20_DE <- crop(lst_d_20_rast, bbox_DE)
#lst_d_20_DE_C <- raster::calc(lst_d_20_DE, fun = function(x){x-273.15})


#lst_d_20_DE_max <- raster::stackApply(lst_d_20_DE_C, indices = rep(1, nlayers(lst_d_20_DE_C)), fun = max, na.rm = TRUE)

#writeRaster(lst_d_20_DE_max, "data/LST/raw/raster/DAY/LST2020max.tif", overwrite = TRUE)

#Einlesen des neuen Datensatzes mit Maximalwerten und Ausschneiden von Zellen mit Siedlungsfläche  
lst_d_20_DE_max <- raster("data/LST/raw/raster/DAY/LST2020max.tif")
lst_d_20_DE_max <- rast(lst_d_20_DE_max)

clc_sdl_sf <- st_read("data/Corine_Landcover/clc_siedl.shp") 

lst_sdl_d_20_max <- terra::mask(lst_d_20_DE_max, clc_sdl_sf, touch = TRUE)
lst_sdl_d_20_max <- raster(lst_sdl_d_20_max)

writeRaster(lst_sdl_d_20_max, "data/LST/raw/raster/DAY/LST2020max_Siedlungsflächen.tif")

lst_sdl_d_20_max <- raster("data/LST/raw/raster/DAY/LST2020max_Siedlungsflächen.tif")

#Aggregieren der Mittleren Maximalwerte pro Gemeinde-Polygon für das Jahr 2020
regiostar_lst_siedl <- regiostar_sf %>%
  mutate(lst_day_20_max = raster::extract(lst_sdl_d_20_max,
                                          regiostar_sf,
                                          fun=mean, 
                                          na.rm = TRUE))

#Einlesen der Daten für 2019 als Rasterstack, Zuschneiden auf Deutschland und Erstellen von neuem Raster mit Maximalwerten
#lst_d_19_rast <- raster::stack(lst_day_files[2:13], 
#                               varname = "lst")
#lst_d_19_DE <- crop(lst_d_19_rast, bbox_DE)
#lst_d_19_DE_C <- raster::calc(lst_d_19_DE, fun = function(x){x-273.15})

#lst_d_19_DE_max <- raster::stackApply(lst_d_19_DE_C, indices = rep(1, nlayers(lst_d_19_DE_C)), fun = max, na.rm = TRUE)

#writeRaster(lst_d_19_DE_max, "data/LST/raw/raster/DAY/LST2019max.tif", overwrite = TRUE) 

#Einlesen des neuen Datensatzes mit Maximalwerten und Ausschneiden von Zellen mit Siedlungsfläche  
lst_d_19_DE_max <- raster("data/LST/raw/raster/DAY/LST2019max.tif")
lst_d_19_DE_max <- rast(lst_d_19_DE_max)

clc_sdl_sf <- st_read("data/Corine_Landcover/clc_siedl.shp") 

lst_sdl_d_19_max <- terra::mask(lst_d_19_DE_max, clc_sdl_sf, touch = TRUE)
lst_sdl_d_19_max <- raster(lst_sdl_d_19_max)

writeRaster(lst_sdl_d_19_max, "data/LST/raw/raster/DAY/LST2019max_Siedlungsflächen.tif")

lst_sdl_d_19_max <- raster("data/LST/raw/raster/DAY/LST2019max_Siedlungsflächen.tif")

#Aggregieren der Mittleren Maximalwerte pro Gemeinde-Polygon für das Jahr 2019
regiostar_lst_siedl <- regiostar_lst_siedl %>%
  mutate(lst_day_19_max = raster::extract(lst_sdl_d_19_max,
                                          regiostar_sf,
                                          fun=mean, 
                                          na.rm = TRUE))


#Klassifizierung der Belastung anhand inhaltlich gesetzter Grenzwerte orienriert an den Vorgaben zu Hitzewarnungen durch den DWD
regiostar_lst_siedl <- regiostar_lst_siedl %>%
  dplyr::select(!c(lst_d_19_, lst_d_20_, lst_n_20_, lst_n_19_)) %>%
  mutate(lst_kl =  case_when(lst_day < 30 ~ 1, 
                                 lst_day < 32 & lst_day >= 30 ~ 2,
                                 lst_day >= 32 ~ 3))

#Reduzieren auf relevante Spalten
regiostar_lst_siedl <- regiostar_lst_siedl %>% 
  select(c(ARS, lst_day, lst_kl))

#Abspeichern des finalen Datensatzes
st_write(regiostar_lst_siedl, "data/LST/results/shape/regiostar_siedl_LST_fin.shp")
