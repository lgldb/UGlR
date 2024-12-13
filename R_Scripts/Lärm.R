library(sf)
library(tidyverse)
library(BAMMtools)

#Datensätze wurden händisch über GOVDATA Das Datenportal für Deutschland heruntergeladen unter: 
#https://www.govdata.de/web/guest/daten/-/searchresult/q/l%C3%A4rmbelastung/f/type%3Adataset%2C/s/relevance_desc

#Vereinheitlichung der Datenformate
#laerm_gdb_d <- st_read("data/Lärm/raw/Mroad_Lden/Mroad_Lden.gdb")
#laerm_gdb_n <- st_read("data/Lärm/raw/Mroad_Lnight/Mroad_Lnight.gdb")

#st_write(laerm_gdb_d, "data/Lärm/raw/Mroad_Lden/Mroad_Lden_17.shp")
#st_write(laerm_gdb_n, "data/Lärm/raw/Mroad_Lnight/Mroad_Lnight_17.shp")

#Einlesen der Regiostar-Daten
regiostar_laerm_ges <- st_read("data/Regiostar/results/regiostar.shp")

#Berechnen der mittleren Lärmbelastung pro Gemeinde-Polygone für alle Lärmquellen
sf_use_s2(FALSE)
l_names <- c("AggAir_Lden", "AggAir_Lnight", 
             "AggInd_Lden", "AggInd_Lnight",
             "AggRail_Lden", "AggRail_Lnight",
             "AggRoad_Lden", "AggRoad_Lnight",
             "Mair_Lden", "Mair_Lnight", 
             "Mrail_Lden", "Mrail_Lnight",
             "Mroad_Lden", "Mroad_Lnight"
             )
regiostar <- regiostar_laerm_ges %>%
  dplyr::select("ARS") %>%
  mutate(reg_area = st_area(.))
for (i in 1:length(l_names)) {
  laerm <- st_read(paste("data/Lärm/raw/", l_names[i], "/", l_names[i], "_17.shp", sep = "")) %>%
    dplyr::select(c("DB_Low", "DB_High")) %>%
    st_transform(., crs = st_crs(regiostar))
  
  regiostar_laerm <- st_intersection(regiostar, st_make_valid(laerm)) %>%
    mutate(part_area = st_area(.))
  regiostar_laerm <- st_drop_geometry(regiostar_laerm)
  
  regiostar_laerm <- regiostar_laerm %>%
    mutate(faktor = (part_area / reg_area))
  regiostar_laerm <- regiostar_laerm %>%
    mutate(DB_Low_part = faktor*DB_Low) %>%
    mutate(DB_High_part = faktor*DB_High) %>%
    dplyr::select(c("ARS", "DB_Low_part", "DB_High_part"))
  
  regiostar_laerm_high <- regiostar_laerm %>%
    dplyr::select(!"DB_Low_part") %>%
    group_by(ARS) %>%
    summarise(DB_High_part = sum(DB_High_part))
  regiostar_laerm_low <- regiostar_laerm %>%
    dplyr::select(!"DB_High_part") %>%
    group_by(ARS) %>%
    summarise(DB_Low_part = sum(DB_Low_part))
  
  regiostar_laerm_ges <- full_join(regiostar_laerm_ges, regiostar_laerm_low, by = "ARS")
  regiostar_laerm_ges <- full_join(regiostar_laerm_ges, regiostar_laerm_high, by = "ARS")
  
  lnames <- c(paste(l_names[i], "_high", sep = ""),  paste(l_names[i], "_low", sep = ""))
  
  names(regiostar_laerm_ges)[names(regiostar_laerm_ges)=="DB_High_part"] <- lnames[1]
  names(regiostar_laerm_ges)[names(regiostar_laerm_ges)=="DB_Low_part"] <- lnames[2]
  
}

#Umwandeln der Belastungswerte in numerische Zahlen
regiostar_laerm_fin <- regiostar_laerm_ges %>%
  mutate_at(c('AggAir_Lden_high', 'AggAir_Lden_low', 'AggAir_Lnight_high','AggAir_Lnight_low', 
              'AggInd_Lden_high', 'AggInd_Lden_low', 'AggInd_Lnight_high', 'AggInd_Lnight_low',
              'AggRail_Lden_high', 'AggRail_Lden_low', 'AggRail_Lnight_high', 'AggRail_Lnight_low',
              'AggRoad_Lden_high', 'AggRoad_Lden_low', 'AggRoad_Lnight_high', 'AggRoad_Lnight_low',
              'Mair_Lden_high', 'Mair_Lden_low', 'Mair_Lnight_high', 'Mair_Lnight_low',
              'Mrail_Lden_high', 'Mrail_Lden_low', 'Mrail_Lnight_high', 'Mrail_Lnight_low',
              'Mroad_Lden_high',  'Mroad_Lden_low', 'Mroad_Lnight_high', 'Mroad_Lnight_low'), 
            as.numeric)

#Berechnung der Belastung durch alle Lärmquellen pro Gemeinde-Polygon
regiostar_laerm_fin <- regiostar_laerm_fin %>%
  rowwise() %>%
  mutate(laermDh = sum(AggAir_Lden_high, AggInd_Lden_high,
                       AggRail_Lden_high,AggRoad_Lden_high,
                       Mair_Lden_high, Mrail_Lden_high,
                       Mroad_Lden_high, na.rm = T)) %>%
  mutate(laermDl = sum(AggAir_Lden_low, AggInd_Lden_low,
                       AggRail_Lden_low,AggRoad_Lden_low,
                       Mair_Lden_low, Mrail_Lden_low,
                       Mroad_Lden_low, na.rm = T)) %>%
  mutate(laermNh = sum(AggAir_Lnight_high, AggInd_Lnight_high,
                       AggRail_Lnight_high,AggRoad_Lnight_high,
                       Mair_Lnight_high, Mrail_Lnight_high,
                       Mroad_Lnight_high, na.rm = T)) %>%
  mutate(laermNl = sum(AggAir_Lnight_low, AggInd_Lnight_low,
                       AggRail_Lnight_low,AggRoad_Lnight_low,
                       Mair_Lnight_low, Mrail_Lnight_low,
                       Mroad_Lnight_low, na.rm = T))

#Umbenennen der einzelnen Belastungsklassen 
regiostar_laerm_fin <- regiostar_laerm_fin %>%
  rename(AAirLDl = AggAir_Lden_low, AAirLDh = AggAir_Lden_high, 
         AAirLNl = AggAir_Lnight_low, AAirLNh = AggAir_Lnight_high, 
         AIndLDl = AggInd_Lden_low, AIndLDh = AggInd_Lden_high, 
         AIndLNl = AggInd_Lnight_low, AIndLNh = AggInd_Lnight_high, 
         ARailLDl = AggRail_Lden_low, ARailLDh = AggRail_Lden_high, 
         ARailLNl = AggRail_Lnight_low, ARailLNh = AggRail_Lnight_high, 
         ARoadLDl = AggRoad_Lden_low, ARoadLDh = AggRoad_Lden_high, 
         ARoadLNl = AggRoad_Lnight_low, ARoadLNh = AggRoad_Lnight_high, 
         MAirLDl = Mair_Lden_low, MAirLDh = Mair_Lden_high, 
         MAirLNl = Mair_Lnight_low, MAirLNh = Mair_Lnight_high, 
         MRailLDl = Mrail_Lden_low, MRailLDh = Mrail_Lden_high, 
         MRailLNl = Mrail_Lnight_low, MRailLNh = Mrail_Lnight_high, 
         MRoadLDl = Mroad_Lden_low, MRoadLDh = Mroad_Lden_high, 
         MRoadLNl = Mroad_Lnight_low, MRoadLNh = Mroad_Lnight_high) 

#Abspeichern des ausführlichen Datensatzes mit allen Lärmquellen
st_write(regiostar_laerm_fin, "data/Lärm/results/regiostar_laerm_ges.shp")

#Klassifizierung der Belastung anhand von natürlichen Unterbrechungen im Datensatz
regiostar_laerm <- st_read("data/Lärm/results/regiostar_laerm_ges.shp") %>%
  select(c(ARS, laermDh, laermNh))
regiostar_laerm$laermDh <- na_if(regiostar_laerm$laermDh, 0)
regiostar_laerm$laermNh <- na_if(regiostar_laerm$laermNh, 0)

#getJenksBreaks(regiostar_laerm$laermDh, 4, subset = NULL)
#6.571886e-06 1.186586e+01 3.244766e+01 1.266375e+02
#getJenksBreaks(regiostar_laerm$laermNh, 4, subset = NULL)
#2.636505e-07 8.634717e+00 2.379910e+01 8.943323e+01

regiostar_laerm <- regiostar_laerm %>%
  mutate(laermD_kl = case_when(laermDh <= 1.186586e+01 ~ 1, 
                               laermDh > 1.186586e+01 & laermDh <= 3.244766e+01 ~ 2, 
                               laermDh > 3.244766e+01 ~ 3)) %>%
  mutate(laermN_kl = case_when(laermNh <= 8.634717e+00 ~ 1, 
                               laermNh > 8.634717e+00 & laermNh <= 2.379910e+01 ~ 2, 
                               laermNh > 2.379910e+01 ~ 3))

regiostar_laerm <- regiostar_laerm %>%
  mutate(laerm_komb = (laermD_kl + laermN_kl)/2)

regiostar_laerm <- regiostar_laerm %>%
  mutate(laerm_kl = case_when(laerm_komb <= 1 ~ 1, 
                              laerm_komb > 1 & laerm_komb <= 2 ~ 2, 
                              laerm_komb > 2 ~ 3)) %>%
  select(ARS, laermDh, laermNh, laerm_kl)

#Abspeichern des zusammengefassten Datensatzes 
st_write(regiostar_laerm, "data/Lärm/results/regiostar_laerm_fin.shp")
