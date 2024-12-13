library(raster)
library(sf)
library(tidyverse)
library(dplyr)
library(gdal)
library(BAMMtools)

#Daten händisch heruntergeladen von https://www.copernicus.eu/en/access-data/copernicus-services-catalogue/cams-europe-air-quality-reanalyses
####################################2018########################################
#Daten einlesen
raster_path_18 <- list.files("data/Luft_PM25/raw/raster/",
                             pattern = "2018_rasterstack.tif$",
                             full.names = TRUE)
raster_name_18 <- tools::file_path_sans_ext(list.files("data/Luft_PM25/raw/raster/", 
                                                       pattern = "2018_rasterstack.tif$",
                                                       full.names = FALSE))
regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)

#Datensatz zuschneiden
for (i in 1:length(raster_path_18)) {  
  pm25_raster_18 <- stack(raster_path_18[i])
  bbox_DE <- st_bbox(regiostar_sf)
  
  pm25_rast_DE_18 <- raster::crop(pm25_raster_18, 
                                  bbox_DE, 
                                  snap="out")
  
  raster::writeRaster(pm25_rast_DE_18, 
                      filename = paste0("data/Luft_PM25/raw/raster/", raster_name_18[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Jahresmittelwert berechnen
pm25_18_raster <- stack(list.files("data/Luft_PM25/raw/raster/",
                                   pattern = "2018_rasterstack_DE.tif$",
                                   full.names = TRUE))
pm25_18_mean <- mean(pm25_18_raster)

raster::writeRaster(pm25_18_mean,
                    filename = paste0("data/Luft_PM25/results/raster/", "PM25_2018_DE.tif"), 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der mittleren PM25 Belastung pro Gemeinde-Polygon
pm25_18_mean <- raster(paste0("data/Luft_PM25/results/raster/", "PM25_2018_DE.tif"))
regiostar_PM25 <- regiostar_sf %>%
  mutate(mean_PM25_18= raster::extract(pm25_18_mean, 
                                       regiostar_sf, 
                                       fun=mean))

####################################2019########################################
#Daten einlesen
raster_path_19 <- list.files("data/Luft_PM25/raw/raster/",
                             pattern = "2019_rasterstack.tif$",
                             full.names = TRUE)
raster_name_19 <- tools::file_path_sans_ext(list.files("data/Luft_PM25/raw/raster/", 
                                                       pattern = "2019_rasterstack.tif$",
                                                       full.names = FALSE))

#Datensatz zuschneiden
for (i in 1:length(raster_path_19)) {  
  pm25_raster_19 <- stack(raster_path_19[i])
  
  bbox_DE <- st_bbox(regiostar_sf)
  
  pm25_rast_DE_19 <- raster::crop(pm25_raster_19, 
                                  bbox_DE, 
                                  snap="out")
  
  raster::writeRaster(pm25_rast_DE_19, 
                      filename = paste0("data/Luft_PM25/raw/raster/", raster_name_19[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Jahresmittelwert berechnen
pm25_19_raster <- stack(list.files("data/Luft_PM25/raw/raster/",
                                   pattern = "2019_rasterstack_DE.tif$",
                                   full.names = TRUE))
pm25_19_mean <- mean(pm25_19_raster)

raster::writeRaster(pm25_19_mean,
                    filename = paste0("data/Luft_PM25/results/raster/", "PM25_2019_DE.tif"), 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der mittleren PM25 Belastung pro Gemeinde-Polygon
pm25_19_mean <- raster(paste0("data/Luft_PM25/results/raster/", "PM25_2019_DE.tif"))

regiostar_PM25 <- regiostar_PM25 %>%
  mutate(mean_PM25_19= raster::extract(pm25_19_mean, 
                                       regiostar_sf, 
                                       fun=mean))

####################################2020########################################
#Daten einlesen
raster_path_20 <- list.files("data/Luft_PM25/raw/raster/",
                             pattern = "2020_rasterstack.tif$",
                             full.names = TRUE)
raster_name_20 <- tools::file_path_sans_ext(list.files("data/Luft_PM25/raw/raster/", 
                                                       pattern = "2020_rasterstack.tif$",
                                                       full.names = FALSE))

#Datensatz zuschneiden 
for (i in 1:length(raster_path_20)) {  
  pm25_raster_20 <- stack(raster_path_20[i])

  bbox_DE <- st_bbox(regiostar_sf)
  
  pm25_rast_DE_20 <- raster::crop(pm25_raster_20, 
                                  bbox_DE, 
                                  snap="out")
  
  raster::writeRaster(pm25_rast_DE, 
                      filename = paste0("data/Luft_PM25/raw/raster/", raster_name_20[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Jahresmittelwert berechnen
pm25_20_raster <- stack(list.files("data/Luft_PM25/raw/raster/",
                                   pattern = "2020_rasterstack_DE.tif$",
                                   full.names = TRUE))
pm25_20_mean <- mean(pm25_20_raster)

raster::writeRaster(pm25_20_mean,
                    filename = paste0("data/Luft_PM25/results/raster/", "PM25_2020_DE.tif"), 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der mittleren PM25 Belastung pro Gemeinde-Polygon
pm25_20_mean <- raster(paste0("data/Luft_PM25/results/raster/", "PM25_2020_DE.tif"))
regiostar_PM25 <- regiostar_PM25 %>%
  mutate(mean_PM25_20= raster::extract(pm25_20_mean, 
                                       regiostar_sf, 
                                       fun=mean))

#Umfassenden Datensatz abspeichern 
st_write(regiostar_PM25, 
         "data/Luft_PM25/results/shape/regiostar_PM25_ges.shp")

#Berechnung der mittleren Belastung und Klassifizierung der Belastung anhand von natürlichen Unterbrechungen im Datensatz
regiostar_PM25 <- st_read("data/Luft_PM25/results/shape/regiostar_PM25_ges.shp")
regiostar_PM25 <- regiostar_PM25 %>%
  mutate(PM25 = (m_PM25_18+m_PM25_19+m_PM25_2)/3) %>%
  dplyr::select(c(ARS, PM25))
#getJenksBreaks(regiostar_PM25$PM25, 4, subset = NULL)
#5.477982  8.561515  9.778348 13.076217
regiostar_PM25 <- regiostar_PM25 %>%
  mutate(PM25_kl =  case_when(PM25 <= 8.561515 ~ 1, 
                              PM25 <= 9.778348 & PM25 > 8.561515 ~ 2,
                              PM25 > 9.778348 ~ 3))
#Speichern des zusammengefassten Datensatzes
st_write(regiostar_PM25, "data/Luft_PM25/results/shape/regiostar_PM25_fin.shp")
