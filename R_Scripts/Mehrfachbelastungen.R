library(sf)
library(tidyverse)
#Einlesen der Datensätze
GW_rs <- st_read("data/Grundwasser/results/regiostar_Grundwasser_fin.shp") %>%     # Grundwasser
  dplyr::select(c("ARS", "GW_Ql_k", "GW_Qnt_"))
laerm_rs <- st_read("data/Lärm/results/regiostar_laerm_fin.shp") %>%
  dplyr::select(c("ARS", "laerm_kl")) %>%
  st_drop_geometry()
LST_rs <- st_read("data/LST/results/shape/regiostar_siedl_LST_fin.shp") %>%              # Wärme
  dplyr::select(c("ARS", "lst_kl")) %>%
  st_drop_geometry()      
O3_rs <- st_read("data/Luft_O3/results/shape/regiostar_O3_fin.shp") %>%
  st_drop_geometry()
PM10_rs <- st_read("data/Luft_PM10/results/shape/regiostar_PM10_fin.shp") %>%
  dplyr::select(c("ARS", "PM10_kl")) %>%
  st_drop_geometry()
PM25_rs <- st_read("data/Luft_PM25/results/shape/regiostar_PM25_fin.shp") %>%
  dplyr::select(c("ARS", "PM25_kl")) %>%
  st_drop_geometry()
NDVI_rs <- st_read("data/NDVI/results/shape/regiostar_NDVI_fin.shp") %>%
  dplyr::select(c("ARS", "NDVI_kl")) %>%
  st_drop_geometry()
SMI_rs <- st_read("data/Dürremonitor/results/shape/regiostar_smi_fin.shp") %>%     # Trockenheit
  dplyr::select(c("ARS", "SMI_kl")) %>%
  st_drop_geometry()      
#Zusammenfügen aller Belastungen in einer Übersichtsdatei
uw_bel_rs <- left_join(GW_rs, laerm_rs, by = "ARS")
uw_bel_rs <- uw_bel_rs %>%
  left_join(., LST_rs, by = "ARS") %>%
  left_join(., O3_rs, by = "ARS") %>%
  left_join(., PM10_rs, by = "ARS") %>%
  left_join(., PM25_rs, by = "ARS") %>%
  left_join(., NDVI_rs, by = "ARS") %>%
  left_join(., SMI_rs, by = "ARS")# %>%
#Klassifizierung aller Belastungen: Belastung liegt vor / liegt nicht vor
uw_bel_rs <- uw_bel_rs %>%
  mutate(GWm_bel = case_when(GW_Qnt_ <= 2 ~ 0,
                             GW_Qnt_ > 2 ~ 1,
                             GW_Qnt_ == NA ~ NA)) %>%
  mutate(GWq_bel = case_when(GW_Ql_k <= 2 ~ 0,
                             GW_Ql_k > 2 ~ 1,
                             GW_Ql_k == NA ~ NA)) %>%
  mutate(laerm_bel = case_when(laerm_kl <= 2 ~ 0,
                               laerm_kl > 2 ~ 1,
                               laerm_kl == NA ~ NA)) %>%
  mutate(LST_bel = case_when(lst_kl <= 2 ~ 0,
                             lst_kl > 2 ~ 1,
                             lst_kl == NA ~ NA)) %>%
  mutate(O3_bel = case_when(O3_kl <= 2 ~ 0,
                            O3_kl > 2 ~ 1,
                            O3_kl == NA ~ NA)) %>%
  mutate(PM10_bel = case_when(PM10_kl <= 2 ~ 0,
                              PM10_kl > 2 ~ 1,
                              PM10_kl == NA ~ NA)) %>%
  mutate(PM25_bel = case_when(PM25_kl <= 2 ~ 0,
                              PM25_kl > 2 ~ 1,
                              PM25_kl == NA ~ NA)) %>%
  mutate(NDVI_bel = case_when(NDVI_kl <= 2 ~ 0,
                              NDVI_kl > 2 ~ 1,
                              NDVI_kl == NA ~ NA)) %>%
  mutate(SMI_bel = case_when(SMI_kl <= 2 ~ 0,
                             SMI_kl > 2 ~ 1,
                             SMI_kl == NA ~ NA))

#Berechnen der Anzahl der Belastungen pro Gemeinde-Polygon
uw_bel_rs <- uw_bel_rs %>%
  rowwise() %>%
  mutate(anz_bel = sum(PM10_bel, PM25_bel, O3_bel, LST_bel, GWm_bel, 
                       GWq_bel, SMI_bel, NDVI_bel, laerm_bel, na.rm = T))

#Speichern der Daten zu Mehrfachbelastungen
st_write(uw_bel_rs, "data/Mehrfachbelastung/Mehrfachbelastung_regiostar_fin.shp")
