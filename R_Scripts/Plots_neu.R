library(sf)
library(tidyverse)
library(ggplot2)

#Laden und Zusammenfügen der Datensätze
regiostar <- st_read("data/Regiostar/results/regiostar.shp") %>%
  select(ARS, RgStR17, RegStR4)
uw_bel <- st_read("data/Mehrfachbelastung/Mehrfachbelastung_regiostar_fin.shp") %>% 
  select(ARS, anz_bel) %>%
  st_drop_geometry()
oekon <- st_read("data/sozioökonomie/gem_soziooek.shp") %>%
  st_drop_geometry()

mfb_rs <- left_join(regiostar, oekon, by = "ARS")
mfb_rs <- left_join(mfb_rs, uw_bel, by = "ARS")

#Benennen der Klassen
mfb_rs <- mfb_rs %>% 
  mutate(Belastungen = case_when(anz_bel < 2 ~ '0-1', 
                                        anz_bel >= 2 & anz_bel< 4 ~ '2-3', 
                                        anz_bel >= 4 & anz_bel< 6 ~ '4-5', 
                                        anz_bel >= 6 ~ '6 und mehr'))
#Laden der Basemap
base <- st_read("data/vg2500_sta.shp")
#Festsetzen der Farbgebung für die Klassen
cols <- c('0-1' =  "#d3e9ff", 
          '2-3' = "#afd1e7", 
          '4-5' = "#3e8ec4", 
          '6 und mehr' = "#08306b")

#Plot Karte Mehrfachbelastung Gesamtdeutschland
plot_mfb_rs <- ggplot() +
  geom_sf(data = mfb_rs, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Anzahl der Umweltbelastungen pro Gemeinde")

tiff("plots/Regiostar/mehrfachbelastung_ges_regiostar.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_rs
dev.off()

#Filtern nach Regionstypen
mfb_metrosr <- mfb_rs %>%                                                       #Metropolitane Stadtregion
  filter(RegStR4 == 11)
mfb_regiosr <- mfb_rs %>%                                                       #Regiopolitane Stadtregion
  filter(RegStR4 == 12)
mfb_srlr <- mfb_rs %>%                                                          #Stadtregionsnahe ländliche Region
  filter(RegStR4 == 21)
mfb_rs_plr <- mfb_rs %>%                                                        #Periphere ländliche Region
  filter(RegStR4 == 22)

#Plot Karte Mehrfachbelastung Metropolitane Stadtregionen
plot_mfb_metrosr <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_metrosr, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Metropolitane Stadtregionen")

tiff("plots/Regiostar/Regionstypen/mehrfachbelastung_Metropolitane_Stadtregion.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_metrosr
dev.off()

#Plot Karte Mehrfachbelastung Regiopolitane Stadtregionen
plot_mfb_regiosr <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_regiosr, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Regiopolitane Stadtregionen")

tiff("plots/Regiostar/Regionstypen/mehrfachbelastung_Regiopolitane_Stadtregion.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_regiosr
dev.off()

#Plot Karte Mehrfachbelastung Stadtregionsnahe ländliche Regionen
plot_mfb_srlr <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_srlr, aes(fill=Belastungen ), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Stadtregionsnahe ländliche Regionen")

tiff("plots/Regiostar/Regionstypen/mehrfachbelastung_Stadtregionsnahe_ländliche_Region.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_srlr
dev.off()

#Plot Karte Mehrfachbelastung Periphere ländliche Regionen
plot_mfb_rs_plr <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_rs_plr, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Periphere ländliche Regionen")

tiff("plots/Regiostar/Regionstypen/mehrfachbelastung_Periphere_ländliche_Region.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_rs_plr
dev.off()

#Filtern nach Siedlungstypen
mfb_pole <- mfb_rs %>%                                                          #Metropole, Regiopole, Zentrale Stadt (2x)
  filter(RgStR17 %in% c(111, 121, 211, 221))
mfb_grst <- mfb_rs %>%                                                          #Großstadt
  filter(RgStR17 %in% c(112))
mfb_mst <- mfb_rs %>%                                                           #Mittelstadt
  filter(RgStR17 %in% c(113, 123, 213, 223))
mfb_sr <- mfb_rs %>%                                                            #Städtischer Raum
  filter(RgStR17%in% c(114, 124, 214, 224))
mfb_kstd <- mfb_rs %>%                                                          #Kleinstädtischer dörflicher Raum
  filter(RgStR17 %in% c(115, 125, 215, 225))

#Plot Karte Mehrfachbelastung Siedlungstypen Metropole, Regiopole, Zentrale Stadt
plot_mfb_pole <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_pole, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Metropole, Regiopole, Zentrale Stadt")

tiff("plots/Regiostar/Siedlungstypen/mehrfachbelastung_Metropole_Regiopole_ZentraleStadt.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_pole
dev.off()

#Plot Karte Mehrfachbelastung für Siedlungstyp Großstadt
plot_mfb_grst <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_grst, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Großstadt")

tiff("plots/Regiostar/Siedlungstypen/mehrfachbelastung_Großstadt.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_grst
dev.off()

#Plot Karte Mehrfachbelastung für Sieldungstyp Mittelstadt
plot_mfb_mst <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_mst, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Mittelstadt")

tiff("plots/Regiostar/Siedlungstypen/mehrfachbelastung_Mittelstadt.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_mst
dev.off()

#Plot Karte Mehrfachbelastung für Siedlungstyp Städtischer Raum
plot_mfb_sr <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_sr, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Städtischer Raum")

tiff("plots/Regiostar/Siedlungstypen/mehrfachbelastung_StädtischerRaum.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_sr
dev.off()

#Plot Karte Mehrfachbelastung für Siedlungstyp Kleinstädtischer dörflicher Raum
plot_mfb_kstd <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_kstd, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Kleinstädtischer, dörflicher Raum")

tiff("plots/Regiostar/Siedlungstypen/mehrfachbelastung_Kleinstädtischer_dörflicherRaum.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_kstd
dev.off()

#Filtern nach regionalem Wohlstandsnieveau
mfb_oekud_1 <- mfb_rs %>% 
  filter(ökonomie == 1)
mfb_oekud_2 <- mfb_rs %>% 
  filter(ökonomie == 2)
mfb_oekud <- bind_rows(mfb_oekud_1, mfb_oekud_2)
mfb_oekd <- mfb_rs %>% 
  filter(ökonomie == 3)
mfb_oeküd_4 <- mfb_rs %>% 
  filter(ökonomie == 4)
mfb_oeküd_5 <- mfb_rs %>% 
  filter(ökonomie == 5)
mfb_oeküd <- bind_rows(mfb_oeküd_4, mfb_oeküd_5)

#Plot Karte Mehrfachbelastung Regionen mit unterdurchschnittlichem Wohlstandsniveau
plot_mfb_oekud <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_oekud, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Unterdurchschnittlicher Wohlstand")

tiff("plots/Regiostar/Wohlstand/Wohlstand_unterdurchschnittlich.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_oekud
dev.off()

#Plot Karte Mehrfachbelastung Regionen mit durchschnittlichem Wohlstandsniveau
plot_mfb_oekd <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_oekd, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Durchschnittlicher Wohlstand")

tiff("plots/Regiostar/Wohlstand/Wohlstand_durchschnittlich.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_oekd
dev.off()
  
#Plot Karte Mehrfachbelastung Regionen mit überdurchschnittlichem Wohlstandsniveau
plot_mfb_oeküd <- ggplot() +
  geom_sf(data= base, fill="snow3", col=NA, aes(alpha = 0,5))+ 
  geom_sf(data = mfb_oeküd, aes(fill=Belastungen), col=NA) +
  scale_fill_manual(values = cols) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Überdurchschnittlicher Wohlstand")

tiff("plots/Regiostar/Wohlstand/Wohlstand_überdurchschnittlich.tiff", units="mm", width=200, height=300, res=300)
plot_mfb_oeküd
dev.off()
