library(raster)
library(terra)
library(sf)
library(tidyverse)
library(BAMMtools)

#Händischer Download der Daten von https://earthexplorer.usgs.gov/ und https://lpdaac.usgs.gov/products/mod13a3v061/ 

##Umwandeln der Daten in Geotiff
raw_path <- "data/NDVI/raw/"
NDVI_hdf_path <- "data/NDVI/raw/hdf/"
NDVI_tif_path <- "data/NDVI/raw/tif/"
NDVI_names_hdf <- list.files(NDVI_hdf_path,
                         pattern = ".hdf$")
NDVI_names_tif <- gsub(".hdf", ".tif", NDVI_names_hdf)
hdfs <- terra::rast(paste0(NDVI_hdf_path, NDVI_names))

for (i in 1:length(NDVI_names)) {
  hdfs <- terra::rast(paste0(NDVI_hdf_path, NDVI_names_hdf[i]))
  NDVI_layer <- hdfs[[1]]
  terra::writeRaster(NDVI_layer, 
                     filename=paste0(NDVI_tif_path, NDVI_names_tif[i]), 
                     overwrite = TRUE)
}

##Teilstücke der Rasterdaten zusammenfügen
rastpath_A <- list.files(NDVI_tif_path, 
                         pattern = "A.tif$", 
                         full.names = TRUE)
rastpath_B <- list.files(NDVI_tif_path, 
                         pattern = "B.tif$", 
                         full.names = TRUE)
filenames <- gsub("A", "", list.files(NDVI_tif_path, 
                                        pattern = "A.tif$"))
filenames <- gsub("MOD133", "NDVI", filenames)

for (i in 1:length(filenames)) {
  rast_A <- raster::raster(rastpath_A[i])
  rast_B <- raster::raster(rastpath_B[i])
  temp_rast <- raster::mosaic(rast_A, rast_B, fun = "mean")
  raster::writeRaster(temp_rast, 
                      filename = paste0(raw_path, filenames[i]), 
                      overwrite = TRUE)
  
}

#Erstellen von neuem Rasterdatensatz mit höchtem Wert pro Pixel pro Jahr
NDVI_files_path <- list.files(raw_path, 
                              pattern = ".tif$")
NDVI_2018_path <- NDVI_files_path[1:12]
NDVI_2018_stack <- raster::stack(paste0(raw_path, NDVI_2018_path))

NDVI_2018_max <- max(NDVI_2018_stack)

writeRaster(NDVI_2018_max,
            filename = "data/NDVI/results/raster/NDVI_max_2018.tif",
            overwrite = TRUE)

NDVI_2019_path <- NDVI_files_path[13:24]
NDVI_2019_stack <- raster::stack(paste0(raw_path, NDVI_2019_path))

NDVI_2019_max <- max(NDVI_2019_stack)

writeRaster(NDVI_2019_max,
            filename = "data/NDVI/results/raster/NDVI_max_2019.tif",
            overwrite = TRUE)

NDVI_2020_path <- NDVI_files_path[25:36]
NDVI_2020_stack <- raster::stack(paste0(raw_path, NDVI_2020_path))

NDVI_2020_max <- max(NDVI_2020_stack)

writeRaster(NDVI_2020_max,
            filename = "data/NDVI/results/raster/NDVI_max_2020.tif",
            overwrite = TRUE)

#Berechtnung des mittleren NDVI pro Gemeinde-Polygon
NDVI_2018_max <- raster::raster("data/NDVI/results/raster/NDVI_max_2018.tif")
NDVI_2018_max <- NDVI_2018_max*0.0001
NDVI_2018_max <- NDVI_2018_max*0.0001
NDVI_2019_max <- raster::raster("data/NDVI/results/raster/NDVI_max_2019.tif")
NDVI_2019_max <- NDVI_2019_max*0.0001
NDVI_2019_max <- NDVI_2019_max*0.0001
NDVI_2020_max <- raster::raster("data/NDVI/results/raster/NDVI_max_2020.tif")
NDVI_2020_max <- NDVI_2020_max*0.0001
NDVI_2020_max <- NDVI_2020_max*0.0001

regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)

regiostar_NDVI <- regiostar_sf %>%
  mutate('NDVI_2018' = raster::extract(NDVI_2018_max,
                                       regiostar_sf,
                                       fun=mean, 
                                       na.rm = TRUE)) %>%
  mutate('NDVI_2019' = raster::extract(NDVI_2019_max,
                                       regiostar_sf,
                                       fun=mean, 
                                       na.rm = TRUE)) %>%
  mutate('NDVI_2020' = raster::extract(NDVI_2020_max,
                                       regiostar_sf,
                                       fun=mean, 
                                       na.rm = TRUE))
#Speichern des umfassenden Datensatzes mit Belastungswerten für die einzelnen Jahre
st_write(regiostar_NDVI, 
         "data/NDVI/results/shape/regiostar_NDVI_ges.shp")

#Berechnung des mittleren NDVI über die betrachteten Jahre 2018-2020
regiostar_NDVI <- st_read("data/NDVI/results/shape/regiostar_NDVI_ges.shp")
regiostar_NDVI <- regiostar_NDVI %>%
  mutate(NDVI = (NDVI_2018+NDVI_2019+NDVI_2020)/3) %>%
  select(c(ARS, NDVI))

#Klassifizierung der Belastung anhand von natürlichen Unterbrechungen im Datensatz
#getJenksBreaks(regiostar_NDVI$NDVI, 4, subset = NULL)
#0.4492268 0.7301183 0.7873350 0.9050030
regiostar_NDVI <- regiostar_NDVI %>%
  mutate(NDVI_kl =  case_when(NDVI < 0.7301183 ~ 3, 
                              NDVI <= 0.7873350 & NDVI >= 0.7301183 ~ 2,
                              NDVI > 0.7873350 ~ 1, ))
#Speichern des zusammengefassten Datensatzes
st_write(regiostar_NDVI, 
         "data/NDVI/results/shape/regiostar_NDVI_fin.shp")
