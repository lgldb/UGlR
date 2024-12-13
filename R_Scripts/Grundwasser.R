library(sf)
library(tidyverse)
library(BAMMtools)

#Datengrundlage UBA Grundwasserberwertung nach EU-WRRL:
#https://www.umweltbundesamt.de/sites/default/files/medien/2875/dokumente/wrrl_bewertung_grundwasser_2021.zip

#Einlesen der Daten und Anpassung von Datensatz und Dateiformat
gwas <- read_sf("data/Grundwasser/raw/WRRL_Bewertung_Grundwasser_2021.geojson")

st_write(gwas_sf, "data/Grundwasser/raw/Grundwasserbeschaffenheit.shp")

regiostar_path <- "data/Regiostar/results/regiostar.shp"
regiostar_sf <- st_read(regiostar_path)
regiostar <- regiostar_sf %>%
  dplyr::select("ARS") %>%
  mutate(reg_area = st_area(.))
gwas <- st_read("data/Grundwasser/raw/Grundwasserbeschaffenheit.shp") %>%
  mutate(MengeZ_num = case_when(MengeZ == "schlecht" ~ 1, 
                                MengeZ == "gut" ~0)) %>%
  mutate(ChemZ_num = case_when(ChemZ == "schlecht" ~ 1, 
                               ChemZ == "gut" ~0))

#Berechnung der mittleren Belastung pro Gemeinde-Polygon durch sinkendes Grundwasser
gwas_menge <- gwas %>%
  select("MengeZ_num", "GWK_ID")%>%
  filter(MengeZ_num == 1) %>%
  st_transform(., crs = st_crs(regiostar_sf))
regiostar_menge <- st_intersection(regiostar, gwas_menge) %>%
  mutate(part_area = st_area(.))
regiostar_menge <- regiostar_menge %>%
  mutate(faktor = as.numeric((part_area / reg_area)))
regiostar_menge <- regiostar_menge %>%
  group_by(ARS) %>%
  summarise(gw_Menge = sum(faktor))
regiostar_menge <- st_drop_geometry(regiostar_menge)

regiostar_w <- left_join(regiostar_sf, regiostar_menge, by="ARS")
regiostar_w$gw_Menge[is.na(regiostar_w$gw_Menge)] <- 0

#Berechnung der mittleren Belastung pro Gemeinde-Polygon durch verunreinigtes Grundwasser
gwas_quali <- gwas %>%
  select("ChemZ_num", "GWK_ID")%>%
  filter(ChemZ_num == 1) %>%
  st_transform(., crs = st_crs(regiostar_sf))
regiostar_quali <- st_intersection(regiostar, gwas_quali) %>%
  mutate(part_area = st_area(.))
regiostar_quali <- regiostar_quali %>%
  mutate(faktor = as.numeric((part_area / reg_area)))
regiostar_quali <- regiostar_quali %>%
  group_by(ARS) %>%
  summarise(gw_Quali = sum(faktor))
regiostar_quali <- st_drop_geometry(regiostar_quali)

regiostar_w <- left_join(regiostar_w, regiostar_quali, by="ARS")
regiostar_w$gw_Quali[is.na(regiostar_w$gw_Quali)] <- 0

#händische Einteilung von Klassen anhand der Datenverteilung
regiostar_w <- regiostar_w %>%
  mutate(GW_Quali_kl = case_when(gw_Qual <= 0.2 ~ 1, 
                                 gw_Qual > 0.2 & gw_Qual < 0.7 ~ 2,
                                 gw_Qual >= 0.7 ~ 3)) %>%
  mutate(GW_Quanti_kl = case_when(gw_Meng == 0 ~ 1,
                                  gw_Meng > 0 & gw_Meng <= 0.5 ~ 2, 
                                  gw_Meng > 0.5 ~ 3)) %>%
  select(c(ARS, gw_Meng, gw_Qual, GW_Quali_kl, GW_Quanti_kl))

#Abspeichern des finalen Datensatzes  
st_write(regiostar_w, "data/Grundwasser/results/regiostar_Grundwasser_fin.shp")


