library(sf)
library(tidyverse)

#Datensätze einlesen 
regiostar_sf <- st_read("data/Regiostar/results/regiostar.shp") %>%
  select("ARS")
ökonomie_sf <- st_read("data/sozioökonomie/NUTS250_N3.shp")%>%
  select("ökonomie")
#Zuordnen der sozioökonomischen Regionen zu Gemeinden
regiostar_oek <- st_join(regiostar_sf, ökonomie_sf, largest = TRUE)
#ausführliche Benennung der Klassen
regiostar_oek <- regiostar_oek %>%
  mutate(Wohlstand = case_when(ökonomie == 1 ~ 'unterdurchschnittlich', 
                               ökonomie == 2 ~ 'eher unterdurchschnittlich', 
                               ökonomie == 3 ~ 'durchschnittlich', 
                               ökonomie == 4 ~ 'eher überdurchschnittlich', 
                               ökonomie == 5 ~ 'überdurchschnittlich'))
#finalen Datensatz speichern
st_write(regiostar_oek, "data/sozioökonomie/gem_soziooek.shp")
