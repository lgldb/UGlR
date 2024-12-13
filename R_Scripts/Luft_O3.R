##Grenzwerte: 
# Mittlere Belastung von 60 in den am stärksten belasteten 8 h am Tag //
# DE langfristig: Max. 8h-Mittel von 120
# Informationsschwelle 180 (1h-Wert)
# Alarmschwelle 240 (1h-Wert)

library(raster)
library(sf)
library(tidyverse)
library(dplyr)
library(BAMMtools)

#Daten händisch heruntergeladen von https://www.copernicus.eu/en/access-data/copernicus-services-catalogue/cams-europe-air-quality-reanalyses

#Daten einlesen
raster_path_18 <- list.files("data/Luft_O3/raw/raster/",
                             pattern = "2018_rasterstack.tif$",
                             full.names = TRUE)
raster_name_18 <- tools::file_path_sans_ext(list.files("data/Luft_O3/raw/raster/", 
                                                       pattern = "2018_rasterstack.tif$",
                                                       full.names = FALSE))
raster_path_19 <- list.files("data/Luft_O3/raw/raster/",
                             pattern = "2019_rasterstack.tif$",
                             full.names = TRUE)
raster_name_19 <- tools::file_path_sans_ext(list.files("data/Luft_O3/raw/raster/", 
                                                       pattern = "2019_rasterstack.tif$",
                                                       full.names = FALSE))
raster_path_20 <- list.files("data/Luft_O3/raw/raster/",
                             pattern = "2020_rasterstack.tif$",
                             full.names = TRUE)
raster_name_20 <- tools::file_path_sans_ext(list.files("data/Luft_O3/raw/raster/", 
                                                       pattern = "2020_rasterstack.tif$",
                                                       full.names = FALSE))

regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)

#Datensatz zuschneiden
for (i in 1:length(raster_path_18)) {  
  o3_18_raster <- raster::stack(raster_path_18[i])
  
  bbox_DE <- st_bbox(regiostar_sf)
  
  o3_18_rast_DE <- raster::crop(o3_18_raster, 
                                bbox_DE, 
                                snap="out")
  
  raster::writeRaster(o3_18_rast_DE, 
                      filename = paste0("data/Luft_O3/raw/raster/", raster_name_18[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

for (i in 1:length(raster_path_19)) {  
  o3_19_raster <- raster::stack(raster_path_19[i])
  
  bbox_DE <- st_bbox(regiostar_sf)
  
  o3_19_rast_DE <- raster::crop(o3_19_raster, 
                                bbox_DE, 
                                snap="out")
  
  raster::writeRaster(o3_19_rast_DE, 
                      filename = paste0("data/Luft_O3/raw/raster/", raster_name_19[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

for (i in 1:length(raster_path_20)) {  
  o3_20_raster <- raster::stack(raster_path_20[i])
  
  bbox_DE <- st_bbox(regiostar_sf)
  
  o3_20_rast_DE <- raster::crop(o3_20_raster, 
                                bbox_DE, 
                                snap="out")
  
  raster::writeRaster(o3_20_rast_DE, 
                      filename = paste0("data/Luft_O3/raw/raster/", raster_name_20[i], "_DE.tif"), 
                      format="GTiff", 
                      overwrite = TRUE)
}

#Berechnen des höchsten 8h-Mittelwert pro Tag und Rasterzelle
o3_18_rast_DE_path <- list.files("data/Luft_O3/raw/raster",
                                 pattern = "2018_rasterstack_DE.tif$",
                                 full.names = TRUE)
for (i in 1:length(o3_18_rast_DE_path)) {
  o3_18_rast <- raster::stack(o3_18_rast_DE_path[i])
  o3_18_rast_layers <- raster::nlayers(o3_18_rast)
  for (j in 1:(o3_18_rast_layers/24)) {
    
    day_x <- j*24
    o3_18_rast_day <- o3_18_rast[[(day_x-23):day_x]]
    o3_18_rast_day_1 <- mean(o3_18_rast_day[[1:8]])
    o3_18_rast_day_2 <- mean(o3_18_rast_day[[2:9]])
    o3_18_rast_day_3 <- mean(o3_18_rast_day[[3:10]])
    o3_18_rast_day_4 <- mean(o3_18_rast_day[[4:11]])
    o3_18_rast_day_5 <- mean(o3_18_rast_day[[5:12]])
    o3_18_rast_day_6 <- mean(o3_18_rast_day[[6:13]])
    o3_18_rast_day_7 <- mean(o3_18_rast_day[[7:14]])
    o3_18_rast_day_8 <- mean(o3_18_rast_day[[8:15]])
    o3_18_rast_day_9 <- mean(o3_18_rast_day[[9:16]])
    o3_18_rast_day_10 <- mean(o3_18_rast_day[[10:17]])
    o3_18_rast_day_11 <- mean(o3_18_rast_day[[11:18]])
    o3_18_rast_day_12 <- mean(o3_18_rast_day[[12:19]])
    o3_18_rast_day_13 <- mean(o3_18_rast_day[[13:20]])
    o3_18_rast_day_14 <- mean(o3_18_rast_day[[14:21]])
    o3_18_rast_day_15 <- mean(o3_18_rast_day[[15:22]])
    o3_18_rast_day_16 <- mean(o3_18_rast_day[[16:23]])
    o3_18_rast_day_17 <- mean(o3_18_rast_day[[17:24]])
    
    o3_18_day_stack <- raster::stack(o3_18_rast_day_1, o3_18_rast_day_2, o3_18_rast_day_3,
                                     o3_18_rast_day_4, o3_18_rast_day_5, o3_18_rast_day_6,
                                     o3_18_rast_day_7, o3_18_rast_day_8, o3_18_rast_day_9,
                                     o3_18_rast_day_10, o3_18_rast_day_11, o3_18_rast_day_12,
                                     o3_18_rast_day_13, o3_18_rast_day_14, o3_18_rast_day_15,
                                     o3_18_rast_day_16, o3_18_rast_day_17)
    o3_18_8hmax_rast <- calc(o3_18_day_stack, function(x){max(x)})
    raster::writeRaster(o3_18_8hmax_rast, 
                        filename = paste0("data/Luft_O3/raw/raster/daily_max/CAMS_O3_", j, 
                                          "_", i, "_2018_8hMean_DE.tif", sep = ""), 
                        format="GTiff", 
                        overwrite = TRUE)
    
  }
}  

o3_19_rast_DE_path <- list.files("data/Luft_O3/raw/raster",
                                 pattern = "2019_rasterstack_DE.tif$",
                                 full.names = TRUE)
for (i in 1:length(o3_19_rast_DE_path)) {
  o3_19_rast <- raster::stack(o3_19_rast_DE_path[i])
  o3_19_rast_layers <- raster::nlayers(o3_19_rast)
  for (j in 1:(o3_19_rast_layers/24)) {
    
    day_x <- j*24
    o3_19_rast_day <- o3_19_rast[[(day_x-23):day_x]]
    o3_19_rast_day_1 <- mean(o3_19_rast_day[[1:8]])
    o3_19_rast_day_2 <- mean(o3_19_rast_day[[2:9]])
    o3_19_rast_day_3 <- mean(o3_19_rast_day[[3:10]])
    o3_19_rast_day_4 <- mean(o3_19_rast_day[[4:11]])
    o3_19_rast_day_5 <- mean(o3_19_rast_day[[5:12]])
    o3_19_rast_day_6 <- mean(o3_19_rast_day[[6:13]])
    o3_19_rast_day_7 <- mean(o3_19_rast_day[[7:14]])
    o3_19_rast_day_8 <- mean(o3_19_rast_day[[8:15]])
    o3_19_rast_day_9 <- mean(o3_19_rast_day[[9:16]])
    o3_19_rast_day_10 <- mean(o3_19_rast_day[[10:17]])
    o3_19_rast_day_11 <- mean(o3_19_rast_day[[11:18]])
    o3_19_rast_day_12 <- mean(o3_19_rast_day[[12:19]])
    o3_19_rast_day_13 <- mean(o3_19_rast_day[[13:20]])
    o3_19_rast_day_14 <- mean(o3_19_rast_day[[14:21]])
    o3_19_rast_day_15 <- mean(o3_19_rast_day[[15:22]])
    o3_19_rast_day_16 <- mean(o3_19_rast_day[[16:23]])
    o3_19_rast_day_17 <- mean(o3_19_rast_day[[17:24]])
    
    o3_19_day_stack <- raster::stack(o3_19_rast_day_1, o3_19_rast_day_2, o3_19_rast_day_3,
                                     o3_19_rast_day_4, o3_19_rast_day_5, o3_19_rast_day_6,
                                     o3_19_rast_day_7, o3_19_rast_day_8, o3_19_rast_day_9,
                                     o3_19_rast_day_10, o3_19_rast_day_11, o3_19_rast_day_12,
                                     o3_19_rast_day_13, o3_19_rast_day_14, o3_19_rast_day_15,
                                     o3_19_rast_day_16, o3_19_rast_day_17)
    o3_19_8hmax_rast <- calc(o3_19_day_stack, function(x){max(x)})
    raster::writeRaster(o3_19_8hmax_rast, 
                        filename = paste0("data/Luft_O3/raw/raster/daily_max/CAMS_O3_", j, 
                                          "_", i, "_2019_8hMean_DE.tif", sep = ""), 
                        format="GTiff", 
                        overwrite = TRUE)
    
  }
} 

o3_20_rast_DE_path <- list.files("data/Luft_O3/raw/raster",
                                 pattern = "2020_rasterstack_DE.tif$",
                                 full.names = TRUE)
for (i in 1:length(o3_20_rast_DE_path)) {
  o3_20_rast <- raster::stack(o3_20_rast_DE_path[i])
  o3_20_rast_layers <- raster::nlayers(o3_20_rast)
  for (j in 1:(o3_20_rast_layers/24)) {
    
    day_x <- j*24
    o3_20_rast_day <- o3_20_rast[[(day_x-23):day_x]]
    o3_20_rast_day_1 <- mean(o3_20_rast_day[[1:8]])
    o3_20_rast_day_2 <- mean(o3_20_rast_day[[2:9]])
    o3_20_rast_day_3 <- mean(o3_20_rast_day[[3:10]])
    o3_20_rast_day_4 <- mean(o3_20_rast_day[[4:11]])
    o3_20_rast_day_5 <- mean(o3_20_rast_day[[5:12]])
    o3_20_rast_day_6 <- mean(o3_20_rast_day[[6:13]])
    o3_20_rast_day_7 <- mean(o3_20_rast_day[[7:14]])
    o3_20_rast_day_8 <- mean(o3_20_rast_day[[8:15]])
    o3_20_rast_day_9 <- mean(o3_20_rast_day[[9:16]])
    o3_20_rast_day_10 <- mean(o3_20_rast_day[[10:17]])
    o3_20_rast_day_11 <- mean(o3_20_rast_day[[11:18]])
    o3_20_rast_day_12 <- mean(o3_20_rast_day[[12:19]])
    o3_20_rast_day_13 <- mean(o3_20_rast_day[[13:20]])
    o3_20_rast_day_14 <- mean(o3_20_rast_day[[14:21]])
    o3_20_rast_day_15 <- mean(o3_20_rast_day[[15:22]])
    o3_20_rast_day_16 <- mean(o3_20_rast_day[[16:23]])
    o3_20_rast_day_17 <- mean(o3_20_rast_day[[17:24]])
    
    o3_20_day_stack <- raster::stack(o3_20_rast_day_1, o3_20_rast_day_2, o3_20_rast_day_3,
                                     o3_20_rast_day_4, o3_20_rast_day_5, o3_20_rast_day_6,
                                     o3_20_rast_day_7, o3_20_rast_day_8, o3_20_rast_day_9,
                                     o3_20_rast_day_10, o3_20_rast_day_11, o3_20_rast_day_12,
                                     o3_20_rast_day_13, o3_20_rast_day_14, o3_20_rast_day_15,
                                     o3_20_rast_day_16, o3_20_rast_day_17)
    o3_20_8hmax_rast <- calc(o3_20_day_stack, function(x){max(x)})
    raster::writeRaster(o3_20_8hmax_rast, 
                        filename = paste0("data/Luft_O3/raw/raster/daily_max/CAMS_O3_", j, 
                                          "_", i, "_2020_8hMean_DE.tif", sep = ""), 
                        format="GTiff", 
                        overwrite = TRUE)
    
  }
}  

#Berechnung der Anzahl der Tage pro Rasterzelle über dem Grenzwert 60
ind_60 <- 60

file_path_18 <- list.files("data/Luft_O3/raw/raster/daily_max/",
                           pattern = "2018_8hMean_DE.tif$",
                           full.names = TRUE)
rast_60_18 <- raster::stack(file_path_18)
rast_60_18[rast_60_18<ind_60] <- 0 
rast_60_18[rast_60_18>=ind_60] <- 1
rast_60_2018 <- sum(rast_60_18)
raster::writeRaster(rast_60_2018, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_60_2018_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

file_path_19 <- list.files("data/Luft_O3/raw/raster/daily_max/",
                           pattern = "2019_8hMean_DE.tif$",
                           full.names = TRUE)
rast_60_19 <- raster::stack(file_path_19)
rast_60_19[rast_60_19<ind_60] <- 0 
rast_60_19[rast_60_19>=ind_60] <- 1
rast_60_2019 <- sum(rast_60_19)
raster::writeRaster(rast_60_2019, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_60_2019_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

file_path_20 <- list.files("data/Luft_O3/raw/raster/daily_max/",
                           pattern = "2020_8hMean_DE.tif$",
                           full.names = TRUE)
rast_60_20 <- raster::stack(file_path_20)
rast_60_20[rast_60_20<ind_60] <- 0 
rast_60_20[rast_60_20>=ind_60] <- 1
rast_60_2020 <- sum(rast_60_20)
raster::writeRaster(rast_60_2020, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_60_2020_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der Anzahl der Tage pro Rasterzelle über dem Grenzwert 100
ind_100 <- 100

rast_100_18 <- raster::stack(file_path_18)
rast_100_18[rast_100_18<ind_100] <- 0 
rast_100_18[rast_100_18>=ind_100] <- 1
rast_100_2018 <- sum(rast_100_18)
raster::writeRaster(rast_100_2018, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_100_2018_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

rast_100_19 <- raster::stack(file_path_19)
rast_100_19[rast_100_19<ind_100] <- 0 
rast_100_19[rast_100_19>=ind_100] <- 1
rast_100_2019 <- sum(rast_100_19)
raster::writeRaster(rast_100_2019, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_100_2019_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

rast_100_20 <- raster::stack(file_path_20)
rast_100_20[rast_100_20<ind_100] <- 0 
rast_100_20[rast_100_20>=ind_100] <- 1
rast_100_2020 <- sum(rast_100_20)
raster::writeRaster(rast_100_2020, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_100_2020_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnung der Anzahl der Tage pro Rasterzelle über dem Grenzwert 120
ind_120 <- 120

rast_120_18 <- raster::stack(file_path_18)
rast_120_18[rast_120_18<ind_120] <- 0 
rast_120_18[rast_120_18>=ind_120] <- 1
rast_120_2018 <- sum(rast_120_18)
raster::writeRaster(rast_120_2018, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_120_2018_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

rast_120_19 <- raster::stack(file_path_19)
rast_120_19[rast_120_19<ind_120] <- 0 
rast_120_19[rast_120_19>=ind_120] <- 1
rast_120_2019 <- sum(rast_120_19)
raster::writeRaster(rast_120_2019, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_120_2019_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

rast_120_20 <- raster::stack(file_path_20)
rast_120_20[rast_120_20<ind_120] <- 0 
rast_120_20[rast_120_20>=ind_120] <- 1
rast_120_2020 <- sum(rast_120_20)
raster::writeRaster(rast_120_2020, 
                    filename = "data/Luft_O3/results/raster/O3_8hMean_120_2020_DE.tif", 
                    format="GTiff", 
                    overwrite = TRUE)

#Berechnen der mittleren Anzahl der Tage über dem Grenzwert 60 pro Gemeinde-Polygon
O3_60_18 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_60_2018_DE.tif")
O3_60_19 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_60_2019_DE.tif")
O3_60_20 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_60_2020_DE.tif")

regiostar_O3 <- regiostar_sf %>%
  mutate(O3_60_18 = raster::extract(O3_60_18,
                                    regiostar_sf,
                                    fun=mean)) %>%
  mutate(O3_60_19 = raster::extract(O3_60_19,
                                    regiostar_sf,
                                    fun=mean)) %>%
  mutate(O3_60_20 = raster::extract(O3_60_20,
                                    regiostar_sf,
                                    fun=mean))

regiostar_O3 <- regiostar_O3 %>%
  mutate(O3_60 = (O3_60_18 + O3_60_19 + O3_60_20)/3)
st_write(regiostar_O3, "data/Luft_O3/results/shape/regiostar_O3_60.shp")

#Berechnen der mittleren Anzahl der Tage über dem Grenzwert 100 pro Gemeinde-Polygon
O3_100_18 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_100_2018_DE.tif")
O3_100_19 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_100_2019_DE.tif")
O3_100_20 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_100_2020_DE.tif")

regiostar_O3 <- regiostar_sf %>%
  mutate(O3_100_18 = raster::extract(O3_100_18,
                                     regiostar_sf,
                                     fun=mean)) %>%
  mutate(O3_100_19 = raster::extract(O3_100_19,
                                     regiostar_O3,
                                     fun=mean)) %>%
  mutate(O3_100_20 = raster::extract(O3_100_20,
                                     regiostar_O3,
                                     fun=mean))

regiostar_O3 <- regiostar_O3 %>%
  mutate(O3_100 = (O3_100_18 + O3_100_19 + O3_100_20)/3)
st_write(regiostar_O3, "data/Luft_O3/results/shape/regiostar_O3_100.shp")


#Berechnen der mittleren Anzahl der Tage über dem Grenzwert 120 pro Gemeinde-Polygon
O3_120_18 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_120_2018_DE.tif")
O3_120_19 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_120_2019_DE.tif")
O3_120_20 <- raster::raster("data/Luft_O3/results/raster/O3_8hMean_120_2020_DE.tif")

regiostar_O3 <- regiostar_sf %>%
  mutate(O3_120_18 = raster::extract(O3_120_18,
                                     regiostar_sf,
                                     fun=mean)) %>%
  mutate(O3_120_19 = raster::extract(O3_120_19,
                                     regiostar_sf,
                                     fun=mean)) %>%
  mutate(O3_120_20 = raster::extract(O3_120_20,
                                     regiostar_sf,
                                     fun=mean))

regiostar_O3 <- regiostar_O3 %>%
  mutate(O3_120 = (O3_120_18 + O3_120_19 + O3_120_20)/3)
st_write(regiostar_O3, "data/Luft_O3/results/shape/regiostar_O3_120.shp")

#Klassifizierung der Belastung anhand der ANzahl der Tage mit Überschreitung von Grenzwerten
regiostar_O3_60 <- st_read("data/Luft_O3/results/shape/regiostar_O3_60.shp")%>%
  select(c(ARS, O3_60))

regiostar_O3_100 <- st_read("data/Luft_O3/results/shape/regiostar_O3_100.shp") %>%
  select(c(ARS, O3_100)) %>%
  st_drop_geometry()

regiostar_O3_120 <- st_read("data/Luft_O3/results/shape/regiostar_O3_120.shp") %>%
  select(c(ARS, O3_120))  %>%
  st_drop_geometry()

regiostar_O3 <- full_join(regiostar_O3_60, regiostar_O3_100, by ="ARS")
regiostar_O3 <- full_join(regiostar_O3, regiostar_O3_120, by ="ARS")

regiostar_O3 <- regiostar_O3 %>%
  mutate(O3_120_kl =  case_when(O3_120 <= 15 ~ 1, 
                                O3_120 > 15 & O3_120 < 25 ~ 2,
                                O3_120 >= 25 ~ 3)) %>%
  mutate(O3_100_kl = case_when(O3_100 <= 50 ~ 1, 
                               O3_100 > 50 & O3_100 < 70 ~ 2,
                               O3_100 >= 70 ~ 3))

regiostar_O3 <- regiostar_O3 %>%
  mutate(O3_komb = (O3_120_kl + O3_100_kl)/2)

regiostar_O3 <- regiostar_O3 %>%
  mutate(O3_kl = case_when(O3_komb < 2 ~ 1, 
                           O3_komb == 2 ~ 2,
                           O3_komb > 2 ~ 3)) %>%
  select(c(ARS, O3_kl))

st_write(regiostar_O3, "data/Luft_O3/results/shape/regiostar_O3_fin.shp")