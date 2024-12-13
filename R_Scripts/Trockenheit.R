library(tidyverse)
library(raster)
library(sf)
library(BAMMtools)



## Auflösung = 4km
## SMI = Bodenfeuchteindex
  # SMI 0,20 - 0,30 = ungewöhnliche Trockenheit
  # SMI 0,10 - 0,20 = moderate Dürre
  # SMI 0,05 - 0,10 = schwere Dürre
  # SMI 0,02 - 0,05 = extreme Dürre
  # SMI 0,00 - 0,02 = außergewöhnliche Dürre

#Rasterdaten des Dürremonitors händisch heruntergeladen von der Website des UFZ: https://www.ufz.de/index.php?de=37937

#Einlesen der Daten und Auswählen der relevanten Datensätze für die Jahre 2018-2020, Berechnen der Jahresmittelwerte
###Oberboden
SMI_oberboden_nc_path <- "data/Dürremonitor/raw/nc_files/SMI_Oberboden_monatlich.nc"
SMI_rast_ob <- raster::stack(SMI_oberboden_nc_path)
crs(SMI_rast_ob) <- CRS('+init=EPSG:31468')

SMI_rast_ob_2018 <- SMI_rast_ob[[805:816]]
SMI_rast_ob_2019 <- SMI_rast_ob[[817:828]]
SMI_rast_ob_2020 <- SMI_rast_ob[[829:840]]
writeRaster(SMI_rast_ob_2018, 
            "data/Dürremonitor/raw/raster/ob/SMI_Oberboden_monatlich_WGS84_2018.tif", 
            overwrite = TRUE)
writeRaster(SMI_rast_ob_2019, 
            "data/Dürremonitor/raw/raster/ob/SMI_Oberboden_monatlich_WGS84_2019.tif", 
            overwrite = TRUE)
writeRaster(SMI_rast_ob_2020, 
            "data/Dürremonitor/raw/raster/ob/SMI_Oberboden_monatlich_WGS84_2020.tif", 
            overwrite = TRUE)

rast_SMI_ob_list <- list.files("data/Dürremonitor/raw/raster/ob/",
                          full.names = TRUE)


mean_classes <- c("SMI_ob_mean_2018", "SMI_ob_mean_2019", "SMI_ob_mean_2020") 

for (i in 1:length(rast_SMI_ob_list)) {
  rast_SMI_ob <- raster::stack(rast_SMI_ob_list[i])
  rast_SMI_ob_mean <- raster::calc(rast_SMI_ob, function(x){mean(x)})

  writeRaster(rast_SMI_ob_mean,  
              paste("data/Dürremonitor/results/raster/ob/", mean_classes[i], ".tif", sep = ""), 
              overwrite = TRUE)
}

###Gesamtboden
SMI_gesamtboden_nc_path <- "data/Dürremonitor/raw/nc_files/SMI_Gesamtboden_monatlich.nc"

SMI_rast_gb <- raster::stack(SMI_gesamtboden_nc_path)
crs(SMI_rast_gb) <- CRS('+init=EPSG:31468')

SMI_rast_gb_2018 <- SMI_rast_gb[[805:816]]
SMI_rast_gb_2019 <- SMI_rast_gb[[817:828]]
SMI_rast_gb_2020 <- SMI_rast_gb[[829:840]]
writeRaster(SMI_rast_gb_2018, 
            "data/Dürremonitor/raw/raster/gb/SMI_gesamtboden_monatlich_WGS84_2018.tif", 
            overwrite = TRUE)
writeRaster(SMI_rast_gb_2019, 
            "data/Dürremonitor/raw/raster/gb/SMI_gesamtboden_monatlich_WGS84_2019.tif", 
            overwrite = TRUE)
writeRaster(SMI_rast_gb_2020, 
            "data/Dürremonitor/raw/raster/gb/SMI_gesamtboden_monatlich_WGS84_2020.tif", 
            overwrite = TRUE)
rast_SMI_gb_list <- list.files("data/Dürremonitor/raw/raster/gb/",
                               full.names = TRUE)

mean_classes <- c("SMI_gb_mean_2018", "SMI_gb_mean_2019", "SMI_gb_mean_2020") 

for (i in 1:length(rast_SMI_gb_list)) {
  rast_SMI_gb <- raster::stack(rast_SMI_gb_list[i])
  rast_SMI_gb_mean <- raster::calc(rast_SMI_gb, function(x){mean(x)})
  
  writeRaster(rast_SMI_gb_mean,  
              paste("data/Dürremonitor/results/raster/gb/", mean_classes[i], ".tif", sep = ""), 
              overwrite = TRUE)

}

#Einlesen der Regiostar-Daten
regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)
regiostar_smi <- regiostar_sf

#Einlesen der zuvor erstellten Rasterdaten zur mittleren Belastung in Ober- und Gesamtboden
smi_ob_path <- list.files("data/Dürremonitor/results/raster/ob/", 
                          pattern = ".tif$", 
                          full.names = TRUE)
smi_gb_path <- list.files("data/Dürremonitor/results/raster/gb/", 
                          pattern = ".tif$", 
                          full.names = TRUE)
smi_gb_2018 <- raster::raster(smi_gb_path[1])
smi_gb_2019 <- raster::raster(smi_gb_path[2])
smi_gb_2020 <- raster::raster(smi_gb_path[3])

smi_ob_2018 <- raster::raster(smi_ob_path[1])
smi_ob_2019 <- raster::raster(smi_ob_path[2])
smi_ob_2020 <- raster::raster(smi_ob_path[3])

#Berechnung der mittleren Belastung pro Gemeinde-Polygon
regiostar_smi <- regiostar_smi %>%
  mutate('SMI_gb_2018' = raster::extract(smi_gb_2018,
                                         regiostar_smi,
                                         fun=mean, 
                                         na.rm = TRUE)) %>%
  mutate('SMI_gb_2019' = raster::extract(smi_gb_2019,
                                         regiostar_smi,
                                         fun=mean, 
                                         na.rm = TRUE)) %>%
  mutate('SMI_gb_2020' = raster::extract(smi_gb_2020,
                                         regiostar_smi,
                                         fun=mean, 
                                         na.rm = TRUE)) %>%
  mutate('SMI_ob_2018' = raster::extract(smi_ob_2018,
                                         regiostar_smi,
                                         fun=mean, 
                                         na.rm = TRUE)) %>%
  mutate('SMI_ob_2019' = raster::extract(smi_ob_2019,
                                         regiostar_smi,
                                         fun=mean, 
                                         na.rm = TRUE)) %>%
  mutate('SMI_ob_2020' = raster::extract(smi_ob_2020,
                                         regiostar_smi,
                                         fun=mean, 
                                         na.rm = TRUE))

#Berechnung der mittleren Belastung über die Jahre 2018-2020
regiostar_smi <- regiostar_smi %>%
  mutate(SMI_ob = (SMI_ob_2018 + SMI_ob_2019 + SMI_ob_2020)/3) %>%
  mutate(SMI_gb = (SMI_gb_2018 + SMI_gb_2019 + SMI_gb_2020)/3) 

#Speichern des umfassenden Datensatzes mit Belastungswerten für alle Jahre
st_write(regiostar_smi, "data/Dürremonitor/results/shape/regiostar_smi_ges.shp")

#Klassifizierung der Belastung anhand von natürlichen Unterbrechungen im Datensatz
regiostar_smi <- regiostar_smi %>%
  mutate(SMI_ob_kl = case_when(SMI_ob < 0.3008393 ~ 3, 
                               SMI_ob <= 0.3485259 & SMI_ob >= 0.3008393 ~ 2,
                               SMI_ob > 0.3485259 ~ 1)) %>%
  mutate(SMI_gb_kl = case_when(SMI_gb < 0.1504770 ~ 3, 
                               SMI_gb <= 0.2410943 & SMI_gb >= 0.1504770 ~ 2,
                               SMI_gb > 0.2410943 ~ 1))

regiostar_smi <- regiostar_smi %>%
  mutate(SMI_komb = (SMI_ob_kl + SMI_gb_kl))
regiostar_smi <- regiostar_smi %>%
  mutate(SMI_kl = case_when(SMI_komb <= 2 ~ 1, 
                            SMI_komb > 2 & SMI_komb < 5 ~ 2,
                            SMI_komb >= 5 ~ 3)) %>%
  select(c(ARS, SMI_ob, SMI_gb, SMI_kl))

#Abspeichern des zusammengefassten Datensatzes 
st_write(regiostar_smi, "data/Dürremonitor/results/shape/regiostar_smi_fin.shp")
