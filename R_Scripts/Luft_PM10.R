library(tidyverse)
library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(BAMMtools)

#Daten händisch heruntergeladen von https://www.copernicus.eu/en/access-data/copernicus-services-catalogue/cams-europe-air-quality-reanalyses
############################2018################################################
#Daten einlesen
raster_path_18 <- list.files("data/Luft_PM10/raw/raster/", 
                          pattern = "2018_rasterstack.tif$",
                          full.names = TRUE)
raster_name_18 <- tools::file_path_sans_ext(list.files("data/Luft_PM10/raw/raster/", 
                                                    pattern = "2018_rasterstack.tif$",
                                                    full.names = FALSE))

regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)

#Datensatz zuschneiden
for (i in 1:length(raster_path_18)) {
  pm10_raster_18 <- stack(raster_path_18[i])

  bbox_DE <- st_bbox(regiostar_sf)
  
  pm10_rast_18_DE <- raster::crop(pm10_raster_18, 
                                  bbox_DE)
  
  raster::writeRaster(pm10_rast_18_DE, 
                      filename = paste0("data/Luft_PM10/raw/raster/", raster_name_18[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Jahresmittelwert berechnen
pm10_18_raster <- stack(list.files("data/Luft_PM10/raw/raster/",
                                   pattern = "DE.tif$",
                                   full.names = TRUE))
pm10_18_mean <- mean(pm10_18_raster)

raster::writeRaster(pm10_18_mean,
                    filename = paste0("data/Luft_PM10/results/raster/", "PM10_2018_DE.tif"), 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der mittleren PM25 Belastung pro Gemeinde-Polygon
pm10_18 <- raster(paste0("data/Luft_PM10/results/raster/", "PM10_2018_DE.tif"))
regiostar_PM10 <- regiostar_sf %>%
  mutate(mean_PM10_18 = raster::extract(x=pm10_18, 
                                        y=regiostar_sf, 
                                        fun=mean, 
                                        na.rm=TRUE))

############################2019################################################
#Daten einlesen
raster_path_19 <- list.files("data/Luft_PM10/raw/raster/", 
                          pattern = "2019_rasterstack.tif$",
                          full.names = TRUE)
raster_name_19 <- tools::file_path_sans_ext(list.files("data/Luft_PM10/raw/raster/", 
                                                    pattern = "2019_rasterstack.tif$",
                                                    full.names = FALSE))
#Datensatz zuschneiden
for (i in 1:length(raster_path_19)) {
  pm10_raster_19 <- raster::stack(raster_path_19[i])
  bbox_DE <- st_bbox(regiostar_sf)
  
  pm10_rast_19_DE <- raster::crop(pm10_raster_19, 
                       bbox_DE)
  
  raster::writeRaster(pm10_rast_19_DE, 
                      filename = paste0("data/Luft_PM10/raw/raster/", raster_name_19[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Jahresmittelwert berechnen
pm10_19_raster <- stack(list.files("data/Luft_PM10/raw/raster/",
                                   pattern = "2019_rasterstack_DE.tif$",
                                   full.names = TRUE))
pm10_19_mean <- mean(pm10_19_raster)

raster::writeRaster(pm10_19_mean,
                    filename = paste0("data/Luft_PM10/results/raster/", "PM10_2019_DE.tif"), 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der mittleren PM25 Belastung pro Gemeinde-Polygon
pm10_19 <- raster(paste0("data/Luft_PM10/results/raster/", "PM10_2019_DE.tif"))

regiostar_PM10 <- regiostar_PM10 %>%
  mutate(mean_PM10_19 = raster::extract(x=pm10_19, 
                                        y=regiostar_sf, 
                                        fun=mean, 
                                        na.rm=TRUE))
############################2020################################################
#Daten einlesen
raster_path_20 <- list.files("data/Luft_PM10/raw/raster/", 
                             pattern = "2020_rasterstack.tif$",
                             full.names = TRUE)
raster_name_20 <- tools::file_path_sans_ext(list.files("data/Luft_PM10/raw/raster/", 
                                                       pattern = "2020_rasterstack.tif$",
                                                       full.names = FALSE))

#Datensatz zuschneiden
for (i in 1:length(raster_path_20)) {
  pm10_raster_20 <- raster::stack(raster_path_20[i])

  bbox_DE <- st_bbox(regiostar_sf)
  
  pm10_rast_20_DE <- raster::crop(pm10_raster_20, 
                                  bbox_DE)
  
  raster::writeRaster(pm10_rast_20_DE, 
                      filename = paste0("data/Luft_PM10/raw/raster/", raster_name_20[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Jahresmittelwert berechnen
pm10_20_raster <- stack(list.files("data/Luft_PM10/raw/raster/",
                                   pattern = "2020_rasterstack_DE.tif$",
                                   full.names = TRUE))
pm10_20_mean <- mean(pm10_20_raster)

raster::writeRaster(pm10_20_mean,
                    filename = paste0("data/Luft_PM10/results/raster/", "PM10_2020_DE.tif"), 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der mittleren PM25 Belastung pro Gemeinde-Polygon

pm10_20 <- raster(paste0("data/Luft_PM10/results/raster/", "PM10_2020_DE.tif"))

regiostar_PM10 <- regiostar_PM10 %>%
  mutate(mean_PM10_20 = raster::extract(x=pm10_20, 
                                        y=regiostar_sf, 
                                        fun=mean, 
                                        na.rm=TRUE))
#Umfassenden Datensatz abspeichern 
st_write(regiostar_PM10, 
         "data/Luft_PM10/results/shape/regiostar_PM10_ges.shp")

#Berechnung der mittleren Belastung und Klassifizierung der Belastung anhand von natürlichen Unterbrechungen im Datensatz
regiostar_PM10 <- regiostar_PM10 %>%
  mutate(PM10 = (m_PM10_18+m_PM10_19+m_PM10_2)/3) %>%
  dplyr::select(c(ARS, PM10))


#getJenksBreaks(regiostar_PM10$PM10, 4, subset = NULL)
#6.800887 12.001347 13.604748 19.466757
regiostar_PM10 <- regiostar_PM10 %>%
  mutate(PM10_kl =  case_when(PM10 <= 12.001347  ~ 1, 
                              PM10 <= 13.604748  & PM10 > 12.001347  ~ 2,
                              PM10 > 13.604748  ~ 3))

#Speichern des zusammengefassten Datensatzes
st_write(regiostar_PM10, 
         "data/Luft_PM10/results/shape/regiostar_PM10_fin.shp")
