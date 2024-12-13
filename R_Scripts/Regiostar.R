library(sf)
library(tidyverse)
library(readxl)

#Regiostar-Daten einlesen und korrigieren
regiostar_table_2020 <- read_excel("data/Regiostar/raw/BMVI RegioStaR Referenzdatei Gemeinden Zeitreihe_v1.xlsx", 
                                  sheet = "ReferenzGebietsstand2020") %>%
                        rename("GEN" = "name_20", "ARS"= "gemrs_20")
##nicht matchende ARS händisch korrigieren (gefunden über merchen der gesamten Daten und filtern nach NAs)
#gemeinde_grenzen_sf[which(gemeinde_grenzen_sf$GEN=="Langelsheim"), 1]
#regiostar_table_2020[which(regiostar_table_2020$GEN=="Rain, Stadt"),2]
#regiostar_table_2020[which(regiostar_table_2020$GEN=="Langelsheim"),2]
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Balge")]=32565411001
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Bastheim")]=96735633116
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Binnen")]=32565411002
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Eisenach, Stadt")]=160630105105
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Greußen, Stadt")]=160650089089
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Gusow-Platkow")]=120645412190
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Langelsheim, Stadt")]=31530019019
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Liebenau, Flecken")]=32565411019
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Märkische Höhe")]=120645408303
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Marklohe")]=32565411021
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Neitersen")]=71325010502
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Neuhardenberg")]=120645412340
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Pennigsehl")]=32565411023
regiostar_table_2020$ARS[which(regiostar_table_2020$ARS == 120735310440)]=120735051440
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Rain, Stadt")]=97790201201
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Schwedt/Oder, Stadt")]=120735051532
regiostar_table_2020$ARS[which(regiostar_table_2020$GEN == "Wietzen")]=32565411036

#Gemeindegrenzen einlesen  
gemeinde_grenzen_sf <- st_read("data/Gemeindegrenzen/raw/VG250_GEM.shp") %>%
  select(c("ARS", "AGS", "SDV_ARS", "GEN"))
gemeinde_grenzen_sf$ARS <- sub("^0+", "", gemeinde_grenzen_sf$ARS)
gemeinde_grenzen_sf$ARS <- as.numeric(gemeinde_grenzen_sf$ARS)

#Regiostar mit Gemeindegrenzen verbinden und Dupplungen korrigieren
regiostar_sf <- full_join(gemeinde_grenzen_sf, regiostar_table_2020, by= "ARS") %>%
  filter(!st_is_empty(.))

regiostar_duplicates <- regiostar_sf %>%
  group_by(ARS) %>%
  filter(n() > 1) %>%
  ungroup()

double_ind <- regiostar_sf[which(duplicated(regiostar_sf$ARS)), 1] %>%
  st_drop_geometry(.)
double_ind <- unlist(double_ind)
double_ind <- unname(double_ind)
regiostar_clean <-  regiostar_sf %>%
  filter(!ARS %in% double_ind)
for (i in 1:length(double_ind)) {
  ars <- double_ind[i]

  test <- regiostar_duplicates %>%
    filter(ARS == ars)
  test_union <- st_union(test) %>%
    st_sf(.) 
  test_union <- test_union%>%
    mutate(ARS = ars)
  test <- st_drop_geometry(test)
  test <- unique(test)
  test <- full_join(test, test_union, by = "ARS")
  regiostar_clean <- bind_rows(regiostar_clean, test)
  
}

#finale Regiostar-Shapedatei speichern
st_write(regiostar_clean, "data/Regiostar/results/regiostar.shp")

