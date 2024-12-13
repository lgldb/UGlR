library(sf)
library(tidyverse)

#Einlesen der Daten
clc <- st_read("data/Corine_Landcover/clc5_class1xx.shp")

#Herausfiltern der relevanten Landnutzungsklassen
clc_sied_111 <- clc %>%
  select("CLC18") %>%
  filter(CLC18 == "111")

clc_sied_112 <- clc %>%
  select("CLC18") %>%
  filter(CLC18 == "112")

clc_siedl <- bind_rows(clc_sied_111, clc_sied_112)

#Anpassung der Projektion an andere verwendete Datensätze
clc_siedl <- st_transform(clc_siedl, "+proj=longlat +datum=WGS84 +no_defs")

#Abspeichern der Corine-Landcover-Daten zur Siedlungsfläche
st_write(clc_siedl, "data/Corine_Landcover/clc_siedl.shp")


