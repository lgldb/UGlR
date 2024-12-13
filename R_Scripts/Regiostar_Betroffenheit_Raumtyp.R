library(sf)
library(tidyverse)
#Einlesen und Zusammenfügen der Datensätze
uw_bel <- st_read("data/Mehrfachbelastung/Mehrfachbelastung_regiostar_fin.shp") %>%
  st_drop_geometry()
regiostar <- st_read("data/Regiostar/results/regiostar.shp") %>%
  select(ARS, RgStR17, RegStR4) %>%
  st_drop_geometry()
oekon <- st_read("data/sozioökonomie/gem_soziooek.shp") %>%
  st_drop_geometry()

uw_data <- left_join(regiostar, oekon, by = "ARS")
uw_data <- left_join(uw_data, uw_bel, by = "ARS")

#Filtern der Daten nach Regionstyp
df <- data.frame(matrix(ncol = 4, nrow = 5))
x <- c("Regionstyp", 
       "geringe Belastung (1)", 
       "mittlere Belastung (2)", 
       "hohe Belastung (3)")
colnames(df) <- x
df[1,1] <- "Metropolitane Stadtregionen"
df[2,1] <- "Regiopolitane Stadtregionen"  
df[3,1] <- "Stadtregionsnahe ländliche Regionen"  
df[4,1] <- "Periphere ländliche Regionen"  
df[5,1] <- "Gesamt" 

bel_msr <- uw_data %>%
  filter(RegStR4 == 11)
bel_rsr <- uw_data %>%
  filter(RegStR4 == 12)
bel_slr <- uw_data %>%
  filter(RegStR4 == 21)
bel_plr <- uw_data %>%
  filter(RegStR4 == 22)

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Grundwasserqualität 
GW_Ql_k_msr <- bel_msr %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_msr))
GW_Ql_k_rsr <- bel_rsr %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_rsr))
GW_Ql_k_slr <- bel_slr %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_slr))
GW_Ql_k_plr <- bel_plr %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_plr))
GW_Ql_k <- uw_data %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(uw_data))

GW_Ql_k_df <- df
GW_Ql_k_df[1,2] <- GW_Ql_k_msr[1,2]
GW_Ql_k_df[1,3] <- GW_Ql_k_msr[2,2]
GW_Ql_k_df[1,4] <- GW_Ql_k_msr[3,2]

GW_Ql_k_df[2,2] <- GW_Ql_k_rsr[1,2]
GW_Ql_k_df[2,3] <- GW_Ql_k_rsr[2,2]
GW_Ql_k_df[2,4] <- GW_Ql_k_rsr[3,2]

GW_Ql_k_df[3,2] <- GW_Ql_k_slr[1,2]
GW_Ql_k_df[3,3] <- GW_Ql_k_slr[2,2]
GW_Ql_k_df[3,4] <- GW_Ql_k_slr[3,2]

GW_Ql_k_df[4,2] <- GW_Ql_k_plr[1,2]
GW_Ql_k_df[4,3] <- GW_Ql_k_plr[2,2]
GW_Ql_k_df[4,4] <- GW_Ql_k_plr[3,2]

GW_Ql_k_df[5,2] <- GW_Ql_k[1,2]
GW_Ql_k_df[5,3] <- GW_Ql_k[2,2]
GW_Ql_k_df[5,4] <- GW_Ql_k[3,2]

GW_Ql_k_df <- GW_Ql_k_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(GW_Ql_k_df, "data/Betroffenheit_Raumtypen/Regionstypen/grundwasser_qualitaet.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Grundwassermenge
GW_Qnt_msr <- bel_msr %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_msr))
GW_Qnt_rsr <- bel_rsr %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_rsr))
GW_Qnt_slr <- bel_slr %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_slr))
GW_Qnt_plr <- bel_plr %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_plr))
GW_Qnt <- uw_data %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(uw_data))

GW_Qnt_df <- df
GW_Qnt_df[1,2] <- GW_Qnt_msr[1,2]
GW_Qnt_df[1,3] <- GW_Qnt_msr[2,2]
GW_Qnt_df[1,4] <- GW_Qnt_msr[3,2]

GW_Qnt_df[2,2] <- GW_Qnt_rsr[1,2]
GW_Qnt_df[2,3] <- GW_Qnt_rsr[2,2]
GW_Qnt_df[2,4] <- GW_Qnt_rsr[3,2]

GW_Qnt_df[3,2] <- GW_Qnt_slr[1,2]
GW_Qnt_df[3,3] <- GW_Qnt_slr[2,2]
GW_Qnt_df[3,4] <- GW_Qnt_slr[3,2]

GW_Qnt_df[4,2] <- GW_Qnt_plr[1,2]
GW_Qnt_df[4,3] <- GW_Qnt_plr[2,2]
GW_Qnt_df[4,4] <- GW_Qnt_plr[3,2]

GW_Qnt_df[5,2] <- GW_Qnt[1,2]
GW_Qnt_df[5,3] <- GW_Qnt[2,2]
GW_Qnt_df[5,4] <- GW_Qnt[3,2]

GW_Qnt_df <- GW_Qnt_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(GW_Qnt_df, "data/Betroffenheit_Raumtypen/Regionstypen/grundwasser_menge.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Lärm 
bel_msr_l <- bel_msr %>%
  drop_na(laerm_kl)
laerm_msr <- bel_msr_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_msr_l))
bel_rsr_l <- bel_rsr %>%
  drop_na(laerm_kl)
laerm_rsr <- bel_rsr_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr_l))
bel_slr_l <- bel_slr %>%
  drop_na(laerm_kl)
laerm_slr <- bel_slr_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_slr_l))
bel_plr_l <- bel_plr %>%
  drop_na(laerm_kl)
laerm_plr <- bel_plr_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_plr_l))
uw_data_l <- uw_data %>%
  drop_na(laerm_kl)
laerm <- uw_data_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(uw_data_l))

laerm_df <- df
laerm_df[1,2] <- laerm_msr[1,2]
laerm_df[1,3] <- laerm_msr[2,2]
laerm_df[1,4] <- laerm_msr[3,2]

laerm_df[2,2] <- laerm_rsr[1,2]
laerm_df[2,3] <- laerm_rsr[2,2]
laerm_df[2,4] <- laerm_rsr[3,2]

laerm_df[3,2] <- laerm_slr[1,2]
laerm_df[3,3] <- laerm_slr[2,2]
laerm_df[3,4] <- laerm_slr[3,2]

laerm_df[4,2] <- laerm_plr[1,2]
laerm_df[4,3] <- laerm_plr[2,2]
laerm_df[4,4] <- laerm_plr[3,2]

laerm_df[5,2] <- laerm[1,2]
laerm_df[5,3] <- laerm[2,2]
laerm_df[5,4] <- laerm[3,2]

laerm_df <- laerm_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(laerm_df, "data/Betroffenheit_Raumtypen/Regionstypen/laerm.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: LST (Wärmebelastung) 
bel_msr_l <- bel_msr %>%
  drop_na(lst_kl)
lst_msr <- bel_msr_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_msr_l))
bel_rsr_l <- bel_rsr %>%
  drop_na(lst_kl)
lst_rsr <- bel_rsr_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr_l))
bel_slr_l <- bel_slr %>%
  drop_na(lst_kl)
lst_slr <- bel_slr_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_slr_l))
bel_plr_l <- bel_plr %>%
  drop_na(lst_kl)
lst_plr <- bel_plr_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_plr_l))
uw_data_l <- uw_data %>%
  drop_na(lst_kl)
lst <- uw_data_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(uw_data_l))

lst_df <- df
lst_df[1,2] <- lst_msr[1,2]
lst_df[1,3] <- lst_msr[2,2]
lst_df[1,4] <- lst_msr[3,2]

lst_df[2,2] <- lst_rsr[1,2]
lst_df[2,3] <- lst_rsr[2,2]
lst_df[2,4] <- lst_rsr[3,2]

lst_df[3,2] <- lst_slr[1,2]
lst_df[3,3] <- lst_slr[2,2]
lst_df[3,4] <- lst_slr[3,2]

lst_df[4,2] <- lst_plr[1,2]
lst_df[4,3] <- lst_plr[2,2]
lst_df[4,4] <- lst_plr[3,2]

lst_df[5,2] <- lst[1,2]
lst_df[5,3] <- lst[2,2]
lst_df[5,4] <- lst[3,2]

lst_df <- lst_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(lst_df, "data/Betroffenheit_Raumtypen/Regionstypen/lst.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Luft O3
O3_msr <- bel_msr %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_msr))
O3_rsr <- bel_rsr %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr))
O3_slr <- bel_slr %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_slr))
bel_plr_o <- bel_plr %>%
  drop_na(O3_kl)
O3_plr <- bel_plr_o %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_plr_o))
uw_data_o <- uw_data %>%
  drop_na(O3_kl)
O3 <- uw_data_o %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(uw_data_o))

O3_df <- df
O3_df[1,2] <- O3_msr[1,2]
O3_df[1,3] <- O3_msr[2,2]
O3_df[1,4] <- O3_msr[3,2]

O3_df[2,2] <- O3_rsr[1,2]
O3_df[2,3] <- O3_rsr[2,2]
O3_df[2,4] <- O3_rsr[3,2]

O3_df[3,2] <- O3_slr[1,2]
O3_df[3,3] <- O3_slr[2,2]
O3_df[3,4] <- O3_slr[3,2]

O3_df[4,2] <- O3_plr[1,2]
O3_df[4,3] <- O3_plr[2,2]
O3_df[4,4] <- O3_plr[3,2]

O3_df[5,2] <- O3[1,2]
O3_df[5,3] <- O3[2,2]
O3_df[5,4] <- O3[3,2]

O3_df <- O3_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(O3_df, "data/Betroffenheit_Raumtypen/Regionstypen/O3.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Luft PM10 
PM10_msr <- bel_msr %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_msr))
PM10_rsr <- bel_rsr %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr))
PM10_slr <- bel_slr %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_slr))
PM10_plr <- bel_plr %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_plr))
PM10 <- uw_data %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(uw_data))

PM10_df <- df
PM10_df[1,2] <- PM10_msr[1,2]
PM10_df[1,3] <- PM10_msr[2,2]
PM10_df[1,4] <- PM10_msr[3,2]

PM10_df[2,2] <- PM10_rsr[1,2]
PM10_df[2,3] <- PM10_rsr[2,2]
PM10_df[2,4] <- PM10_rsr[3,2]

PM10_df[3,2] <- PM10_slr[1,2]
PM10_df[3,3] <- PM10_slr[2,2]
PM10_df[3,4] <- PM10_slr[3,2]

PM10_df[4,2] <- PM10_plr[1,2]
PM10_df[4,3] <- PM10_plr[2,2]
PM10_df[4,4] <- PM10_plr[3,2]

PM10_df[5,2] <- PM10[1,2]
PM10_df[5,3] <- PM10[2,2]
PM10_df[5,4] <- PM10[3,2]

PM10_df <- PM10_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(PM10_df, "data/Betroffenheit_Raumtypen/Regionstypen/PM10.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Luft PM25 
PM25_msr <- bel_msr %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_msr))
PM25_rsr <- bel_rsr %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr))
PM25_slr <- bel_slr %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_slr))
bel_plr_p <- bel_plr %>%
  drop_na(PM25_kl)
PM25_plr <- bel_plr_p %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_plr_p))
uw_data_p <- uw_data %>%
  drop_na(PM25_kl)
PM25 <- uw_data_p %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(uw_data_p))

PM25_df <- df
PM25_df[1,2] <- PM25_msr[1,2]
PM25_df[1,3] <- PM25_msr[2,2]
PM25_df[1,4] <- PM25_msr[3,2]

PM25_df[2,2] <- PM25_rsr[1,2]
PM25_df[2,3] <- PM25_rsr[2,2]
PM25_df[2,4] <- PM25_rsr[3,2]

PM25_df[3,2] <- PM25_slr[1,2]
PM25_df[3,3] <- PM25_slr[2,2]
PM25_df[3,4] <- PM25_slr[3,2]

PM25_df[4,2] <- PM25_plr[1,2]
PM25_df[4,3] <- PM25_plr[2,2]
PM25_df[4,4] <- PM25_plr[3,2]

PM25_df[5,2] <- PM25[1,2]
PM25_df[5,3] <- PM25[2,2]
PM25_df[5,4] <- PM25[3,2]

PM25_df <- PM25_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(PM25_df, "data/Betroffenheit_Raumtypen/Regionstypen/PM25.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: NDVI (Grünausstattung) 
NDVI_msr <- bel_msr %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_msr))
NDVI_rsr <- bel_rsr %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr))
NDVI_slr <- bel_slr %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_slr))
bel_plr_n <- bel_plr %>%
  drop_na(NDVI_kl)
NDVI_plr <- bel_plr_n %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_plr_n))
uw_data_n <- uw_data %>%
  drop_na(NDVI_kl)
NDVI <- uw_data_n %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(uw_data_n))

NDVI_df <- df
NDVI_df[1,2] <- NDVI_msr[1,2]
NDVI_df[1,3] <- NDVI_msr[2,2]
NDVI_df[1,4] <- NDVI_msr[3,2]

NDVI_df[2,2] <- NDVI_rsr[1,2]
NDVI_df[2,3] <- NDVI_rsr[2,2]
NDVI_df[2,4] <- NDVI_rsr[3,2]

NDVI_df[3,2] <- NDVI_slr[1,2]
NDVI_df[3,3] <- NDVI_slr[2,2]
NDVI_df[3,4] <- NDVI_slr[3,2]

NDVI_df[4,2] <- NDVI_plr[1,2]
NDVI_df[4,3] <- NDVI_plr[2,2]
NDVI_df[4,4] <- NDVI_plr[3,2]

NDVI_df[5,2] <- NDVI[1,2]
NDVI_df[5,3] <- NDVI[2,2]
NDVI_df[5,4] <- NDVI[3,2]

NDVI_df <- NDVI_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(NDVI_df, "data/Betroffenheit_Raumtypen/Regionstypen/NDVI.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: SMI (Trockenheit/Dürre) 
SMI_msr <- bel_msr %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_msr))
SMI_rsr <- bel_rsr %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_rsr))
SMI_slr <- bel_slr %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_slr))
bel_plr_s <- bel_plr %>%
  drop_na(SMI_kl)
SMI_plr <- bel_plr_s %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_plr_s))
uw_data_s <- uw_data %>%
  drop_na(SMI_kl)
SMI <- uw_data_s %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(uw_data_s))

SMI_df <- df
SMI_df[1,2] <- SMI_msr[1,2]
SMI_df[1,3] <- SMI_msr[2,2]
SMI_df[1,4] <- SMI_msr[3,2]

SMI_df[2,2] <- SMI_rsr[1,2]
SMI_df[2,3] <- SMI_rsr[2,2]
SMI_df[2,4] <- SMI_rsr[3,2]

SMI_df[3,2] <- SMI_slr[1,2]
SMI_df[3,3] <- SMI_slr[2,2]
SMI_df[3,4] <- SMI_slr[3,2]

SMI_df[4,2] <- SMI_plr[1,2]
SMI_df[4,3] <- SMI_plr[2,2]
SMI_df[4,4] <- SMI_plr[3,2]

SMI_df[5,2] <- SMI[1,2]
SMI_df[5,3] <- SMI[2,2]
SMI_df[5,4] <- SMI[3,2]

SMI_df <- SMI_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(SMI_df, "data/Betroffenheit_Raumtypen/Regionstypen/SMI.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Regionstyps und abspeichern als Tabelle: Mehrfachbelastung 
df_m <- data.frame(matrix(ncol = 10, nrow = 5))
x <- c("Regionstyp", "keine Belastung (0)", "1 Belastung", 
       "2 Belastungen", "3 Belastungen", "4 Belastungen", "5 Belastungen", 
       "6 Belastungen", "7 Belastungen", "8 Belastungen")
colnames(df_m) <- x
df_m[1,1] <- "Metropolitane Stadtregionen"
df_m[2,1] <- "Regiopolitane Stadtregionen"  
df_m[3,1] <- "Stadtregionsnahe ländliche Regionen"  
df_m[4,1] <- "Periphere ländliche Regionen"  
df_m[5,1] <- "Gesamt" 

mfb_msr <- bel_msr %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_msr))
mfb_rsr <- bel_rsr %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_rsr))
mfb_slr <- bel_slr %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_slr))
mfb_plr <- bel_plr %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_plr))
mfb_k <- uw_data %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(uw_data))

df_m[1,2] <- mfb_msr[1,2]
df_m[1,3] <- mfb_msr[2,2]
df_m[1,4] <- mfb_msr[3,2]
df_m[1,5] <- mfb_msr[4,2]
df_m[1,6] <- mfb_msr[5,2]
df_m[1,7] <- mfb_msr[6,2]
df_m[1,8] <- mfb_msr[7,2]
df_m[1,9] <- mfb_msr[8,2]
df_m[1,10] <- mfb_msr[9,2]

df_m[2,2] <- mfb_rsr[1,2]
df_m[2,3] <- mfb_rsr[2,2]
df_m[2,4] <- mfb_rsr[3,2]
df_m[2,5] <- mfb_rsr[4,2]
df_m[2,6] <- mfb_rsr[5,2]
df_m[2,7] <- mfb_rsr[6,2]
df_m[2,8] <- mfb_rsr[7,2]
df_m[2,9] <- mfb_rsr[8,2]
df_m[2,10] <- mfb_rsr[9,2]

df_m[3,2] <- mfb_slr[1,2]
df_m[3,3] <- mfb_slr[2,2]
df_m[3,4] <- mfb_slr[3,2]
df_m[3,5] <- mfb_slr[4,2]
df_m[3,6] <- mfb_slr[5,2]
df_m[3,7] <- mfb_slr[6,2]
df_m[3,8] <- mfb_slr[7,2]
df_m[3,9] <- mfb_slr[8,2]
df_m[3,10] <- mfb_slr[9,2]

df_m[4,2] <- mfb_plr[1,2]
df_m[4,3] <- mfb_plr[2,2]
df_m[4,4] <- mfb_plr[3,2]
df_m[4,5] <- mfb_plr[4,2]
df_m[4,6] <- mfb_plr[5,2]
df_m[4,7] <- mfb_plr[6,2]
df_m[4,8] <- mfb_plr[7,2]
df_m[4,9] <- mfb_plr[8,2]
df_m[4,10] <- mfb_plr[9,2]

df_m[5,2] <- mfb_k[1,2]
df_m[5,3] <- mfb_k[2,2]
df_m[5,4] <- mfb_k[3,2]
df_m[5,5] <- mfb_k[4,2]
df_m[5,6] <- mfb_k[5,2]
df_m[5,7] <- mfb_k[6,2]
df_m[5,8] <- mfb_k[7,2]
df_m[5,9] <- mfb_k[8,2]
df_m[5,10] <- mfb_k[9,2]

df_m <- df_m %>% 
  mutate(gesamt = (`keine Belastung (0)` + `1 Belastung` + `2 Belastungen` + 
                   `3 Belastungen` + `4 Belastungen` + `5 Belastungen` + 
                   `6 Belastungen` + `7 Belastungen` + `8 Belastungen`))

df_m_z <- df_m %>%
  mutate(`0-1 Belastung` = (`keine Belastung (0)` + `1 Belastung`)) %>%
  mutate(`2-3 Belastungen` = (`2 Belastungen` + `3 Belastungen`)) %>%
  mutate(`4-5 Belastungen` = (`4 Belastungen` + `5 Belastungen`)) %>%
  mutate(`6+ Belastungen` = (`6 Belastungen` + `7 Belastungen` + `8 Belastungen`)) 

df_m_z <- df_m_z %>%
  select(c(Regionstyp, `0-1 Belastung`, `2-3 Belastungen`, 
           `4-5 Belastungen`, `6+ Belastungen`)) %>%
  mutate(gesamt = (`0-1 Belastung` + `2-3 Belastungen` +
                   `4-5 Belastungen` + `6+ Belastungen`))
  
write.csv(df_m, "data/Betroffenheit_Raumtypen/Regionstypen/mehrfachbelastungen.csv")
write.csv(df_m_z, "data/Betroffenheit_Raumtypen/Regionstypen/mehrfachbelastungen_zusammenfassung.csv")

#Filtern der Daten nach Siedlungstyp
df <- data.frame(matrix(ncol = 4, nrow = 6))
x <- c("Siedlungstyp", 
       "geringe Belastung (1)", 
       "mittlere Belastung (2)", 
       "hohe Belastung (3)")
colnames(df) <- x
df[1,1] <- "Zentrale Stadtregion"
df[2,1] <- "Großstadt"  
df[3,1] <- "Mittelstadt"  
df[4,1] <- "Städtischer Raum"  
df[5,1] <- "Kleinstädtischer, dörflicher Raum"
df[6,1] <- "Gesamt" 

bel_pole <- uw_data %>%                                                          #Metropole, Regiopole, Zentrale Stadt (2x)
  filter(RgStR17 %in% c(111, 121, 211, 221))
bel_grst <- uw_data %>%                                                          #Großstadt
  filter(RgStR17 %in% c(112))
bel_mst <- uw_data %>%                                                           #Mittelstadt
  filter(RgStR17 %in% c(113, 123, 213, 223))
bel_sr <- uw_data %>%                                                            #Städtischer Raum
  filter(RgStR17%in% c(114, 124, 214, 224))
bel_kstd <- uw_data %>%                                                          #Kleinstädtischer dörflicher Raum
  filter(RgStR17 %in% c(115, 125, 215, 225))

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Grundwasserqualität 
GW_Ql_k_pole <- bel_pole %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_pole))
GW_Ql_k_grst <- bel_grst %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_grst))
GW_Ql_k_mst <- bel_mst %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_mst))
GW_Ql_k_sr <- bel_sr %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_sr))
GW_Ql_k_kstd <- bel_kstd %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_kstd))
GW_Ql_k <- uw_data %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(uw_data))

GW_Ql_k_df <- df
GW_Ql_k_df[1,2] <- GW_Ql_k_pole[1,2]
GW_Ql_k_df[1,3] <- GW_Ql_k_pole[2,2]
GW_Ql_k_df[1,4] <- GW_Ql_k_pole[3,2]

GW_Ql_k_df[2,2] <- GW_Ql_k_grst[1,2]
GW_Ql_k_df[2,3] <- GW_Ql_k_grst[2,2]
GW_Ql_k_df[2,4] <- GW_Ql_k_grst[3,2]

GW_Ql_k_df[3,2] <- GW_Ql_k_mst[1,2]
GW_Ql_k_df[3,3] <- GW_Ql_k_mst[2,2]
GW_Ql_k_df[3,4] <- GW_Ql_k_mst[3,2]

GW_Ql_k_df[4,2] <- GW_Ql_k_sr[1,2]
GW_Ql_k_df[4,3] <- GW_Ql_k_sr[2,2]
GW_Ql_k_df[4,4] <- GW_Ql_k_sr[3,2]

GW_Ql_k_df[5,2] <- GW_Ql_k_kstd[1,2]
GW_Ql_k_df[5,3] <- GW_Ql_k_kstd[2,2]
GW_Ql_k_df[5,4] <- GW_Ql_k_kstd[3,2]

GW_Ql_k_df[6,2] <- GW_Ql_k[1,2]
GW_Ql_k_df[6,3] <- GW_Ql_k[2,2]
GW_Ql_k_df[6,4] <- GW_Ql_k[3,2]

GW_Ql_k_df <- GW_Ql_k_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(GW_Ql_k_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/grundwasser_qualitaet.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Grundwassermenge 
GW_Qnt_pole <- bel_pole %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_pole))
GW_Qnt_grst <- bel_grst %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_grst))
GW_Qnt_mst <- bel_mst %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_mst))
GW_Qnt_sr <- bel_sr %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_sr))
GW_Qnt_kstd <- bel_kstd %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_kstd))
GW_Qnt <- uw_data %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(uw_data))

GW_Qnt_df <- df
GW_Qnt_df[1,2] <- GW_Qnt_pole[1,2]
GW_Qnt_df[1,3] <- GW_Qnt_pole[2,2]
GW_Qnt_df[1,4] <- GW_Qnt_pole[3,2]

GW_Qnt_df[2,2] <- GW_Qnt_grst[1,2]
GW_Qnt_df[2,3] <- GW_Qnt_grst[2,2]
GW_Qnt_df[2,4] <- GW_Qnt_grst[3,2]

GW_Qnt_df[3,2] <- GW_Qnt_mst[1,2]
GW_Qnt_df[3,3] <- GW_Qnt_mst[2,2]
GW_Qnt_df[3,4] <- GW_Qnt_mst[3,2]

GW_Qnt_df[4,2] <- GW_Qnt_sr[1,2]
GW_Qnt_df[4,3] <- GW_Qnt_sr[2,2]
GW_Qnt_df[4,4] <- GW_Qnt_sr[3,2]

GW_Qnt_df[5,2] <- GW_Qnt_kstd[1,2]
GW_Qnt_df[5,3] <- GW_Qnt_kstd[2,2]
GW_Qnt_df[5,4] <- GW_Qnt_kstd[3,2]

GW_Qnt_df[6,2] <- GW_Qnt[1,2]
GW_Qnt_df[6,3] <- GW_Qnt[2,2]
GW_Qnt_df[6,4] <- GW_Qnt[3,2]

GW_Qnt_df <- GW_Qnt_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(GW_Qnt_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/grundwasser_menge.csv")


#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Lärm 
bel_pole_l <- bel_pole %>% 
  drop_na(laerm_kl)
laerm_pole <- bel_pole_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_pole_l))
bel_grst_l <- bel_grst %>% 
  drop_na(laerm_kl)
laerm_grst <- bel_grst_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_grst_l))
bel_mst_l <- bel_mst %>% 
  drop_na(laerm_kl)
laerm_mst <- bel_mst_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_mst_l))
bel_sr_l <- bel_sr %>% 
  drop_na(laerm_kl)
laerm_sr <- bel_sr_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_sr_l))
bel_kstd_l <- bel_kstd %>% 
  drop_na(laerm_kl)
laerm_kstd <- bel_kstd_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd_l))
uw_data_l <- uw_data %>% 
  drop_na(laerm_kl)
laerm <- uw_data_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(uw_data_l))

laerm_df <- df
laerm_df[1,2] <- laerm_pole[1,2]
laerm_df[1,3] <- laerm_pole[2,2]
laerm_df[1,4] <- laerm_pole[3,2]

laerm_df[2,2] <- laerm_grst[1,2]
laerm_df[2,3] <- laerm_grst[2,2]
laerm_df[2,4] <- laerm_grst[3,2]

laerm_df[3,2] <- laerm_mst[1,2]
laerm_df[3,3] <- laerm_mst[2,2]
laerm_df[3,4] <- laerm_mst[3,2]

laerm_df[4,2] <- laerm_sr[1,2]
laerm_df[4,3] <- laerm_sr[2,2]
laerm_df[4,4] <- laerm_sr[3,2]

laerm_df[5,2] <- laerm_kstd[1,2]
laerm_df[5,3] <- laerm_kstd[2,2]
laerm_df[5,4] <- laerm_kstd[3,2]

laerm_df[6,2] <- laerm[1,2]
laerm_df[6,3] <- laerm[2,2]
laerm_df[6,4] <- laerm[3,2]

laerm_df <- laerm_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(laerm_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/laerm.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: LST (Wärmebelastung) 
lst_pole <- bel_pole %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_pole))
lst_grst <- bel_grst %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_grst))
lst_mst <- bel_mst %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_mst))
bel_sr_l <- bel_sr %>% 
  drop_na(lst_kl)
lst_sr <- bel_sr_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_sr_l))
bel_kstd_l <- bel_kstd %>% 
  drop_na(lst_kl)
lst_kstd <- bel_kstd_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd_l))
uw_data_l <- uw_data %>% 
  drop_na(lst_kl)
lst <- uw_data_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(uw_data_l))

lst_df <- df
lst_df[1,2] <- lst_pole[1,2]
lst_df[1,3] <- lst_pole[2,2]
lst_df[1,4] <- lst_pole[3,2]

lst_df[2,2] <- lst_grst[1,2]
lst_df[2,3] <- lst_grst[2,2]
lst_df[2,4] <- lst_grst[3,2]

lst_df[3,2] <- lst_mst[1,2]
lst_df[3,3] <- lst_mst[2,2]
lst_df[3,4] <- lst_mst[3,2]

lst_df[4,2] <- lst_sr[1,2]
lst_df[4,3] <- lst_sr[2,2]
lst_df[4,4] <- lst_sr[3,2]

lst_df[5,2] <- lst_kstd[1,2]
lst_df[5,3] <- lst_kstd[2,2]
lst_df[5,4] <- lst_kstd[3,2]

lst_df[6,2] <- lst[1,2]
lst_df[6,3] <- lst[2,2]
lst_df[6,4] <- lst[3,2]

lst_df <- lst_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(lst_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/lst.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Luft O3 
O3_pole <- bel_pole %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_pole))
O3_grst <- bel_grst %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_grst))
O3_mst <- bel_mst %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_mst))
O3_sr <- bel_sr %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_sr))
bel_kstd_o <- bel_kstd %>% 
  drop_na(O3_kl)
O3_kstd <- bel_kstd_o %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd_o))
uw_data_o <- uw_data %>% 
  drop_na(O3_kl)
O3 <- uw_data_o %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(uw_data_o))

O3_df <- df
O3_df[1,2] <- O3_pole[1,2]
O3_df[1,3] <- O3_pole[2,2]
O3_df[1,4] <- O3_pole[3,2]

O3_df[2,2] <- O3_grst[1,2]
O3_df[2,3] <- O3_grst[2,2]
O3_df[2,4] <- O3_grst[3,2]

O3_df[3,2] <- O3_mst[1,2]
O3_df[3,3] <- O3_mst[2,2]
O3_df[3,4] <- O3_mst[3,2]

O3_df[4,2] <- O3_sr[1,2]
O3_df[4,3] <- O3_sr[2,2]
O3_df[4,4] <- O3_sr[3,2]

O3_df[5,2] <- O3_kstd[1,2]
O3_df[5,3] <- O3_kstd[2,2]
O3_df[5,4] <- O3_kstd[3,2]

O3_df[6,2] <- O3[1,2]
O3_df[6,3] <- O3[2,2]
O3_df[6,4] <- O3[3,2]

O3_df <- O3_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(O3_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/O3.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Luft PM10 
PM10_pole <- bel_pole %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_pole))
PM10_grst <- bel_grst %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_grst))
PM10_mst <- bel_mst %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_mst))
PM10_sr <- bel_sr %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_sr))
PM10_kstd <- bel_kstd %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd))
PM10 <- uw_data %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(uw_data))

PM10_df <- df
PM10_df[1,2] <- PM10_pole[1,2]
PM10_df[1,3] <- PM10_pole[2,2]
PM10_df[1,4] <- PM10_pole[3,2]

PM10_df[2,2] <- PM10_grst[1,2]
PM10_df[2,3] <- PM10_grst[2,2]
PM10_df[2,4] <- PM10_grst[3,2]

PM10_df[3,2] <- PM10_mst[1,2]
PM10_df[3,3] <- PM10_mst[2,2]
PM10_df[3,4] <- PM10_mst[3,2]

PM10_df[4,2] <- PM10_sr[1,2]
PM10_df[4,3] <- PM10_sr[2,2]
PM10_df[4,4] <- PM10_sr[3,2]

PM10_df[5,2] <- PM10_kstd[1,2]
PM10_df[5,3] <- PM10_kstd[2,2]
PM10_df[5,4] <- PM10_kstd[3,2]

PM10_df[6,2] <- PM10[1,2]
PM10_df[6,3] <- PM10[2,2]
PM10_df[6,4] <- PM10[3,2]

PM10_df <- PM10_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(PM10_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/PM10.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Luft PM25 
PM25_pole <- bel_pole %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_pole))
PM25_grst <- bel_grst %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_grst))
PM25_mst <- bel_mst %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_mst))
PM25_sr <- bel_sr %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_sr))
bel_kstd_p <- bel_kstd %>% 
  drop_na(PM25_kl)
PM25_kstd <- bel_kstd_p %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd_p))
uw_data_p <- uw_data %>% 
  drop_na(PM25_kl)
PM25 <- uw_data_p %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(uw_data_p))

PM25_df <- df
PM25_df[1,2] <- PM25_pole[1,2]
PM25_df[1,3] <- PM25_pole[2,2]
PM25_df[1,4] <- PM25_pole[3,2]

PM25_df[2,2] <- PM25_grst[1,2]
PM25_df[2,3] <- PM25_grst[2,2]
PM25_df[2,4] <- PM25_grst[3,2]

PM25_df[3,2] <- PM25_mst[1,2]
PM25_df[3,3] <- PM25_mst[2,2]
PM25_df[3,4] <- PM25_mst[3,2]

PM25_df[4,2] <- PM25_sr[1,2]
PM25_df[4,3] <- PM25_sr[2,2]
PM25_df[4,4] <- PM25_sr[3,2]

PM25_df[5,2] <- PM25_kstd[1,2]
PM25_df[5,3] <- PM25_kstd[2,2]
PM25_df[5,4] <- PM25_kstd[3,2]

PM25_df[6,2] <- PM25[1,2]
PM25_df[6,3] <- PM25[2,2]
PM25_df[6,4] <- PM25[3,2]

PM25_df <- PM25_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(PM25_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/PM25.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: NDVI (Grünausstattung) 
NDVI_pole <- bel_pole %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_pole))
NDVI_grst <- bel_grst %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_grst))
NDVI_mst <- bel_mst %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_mst))
NDVI_sr <- bel_sr %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_sr))
bel_kstd_n <- bel_kstd %>% 
  drop_na(NDVI_kl)
NDVI_kstd <- bel_kstd_n %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd_n))
uw_data_n <- uw_data %>% 
  drop_na(NDVI_kl)
NDVI <- uw_data_n %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(uw_data_n))

NDVI_df <- df
NDVI_df[1,2] <- NDVI_pole[1,2]
NDVI_df[1,3] <- NDVI_pole[2,2]
NDVI_df[1,4] <- NDVI_pole[3,2]

NDVI_df[2,2] <- NDVI_grst[1,2]
NDVI_df[2,3] <- NDVI_grst[2,2]
NDVI_df[2,4] <- NDVI_grst[3,2]

NDVI_df[3,2] <- NDVI_mst[1,2]
NDVI_df[3,3] <- NDVI_mst[2,2]
NDVI_df[3,4] <- NDVI_mst[3,2]

NDVI_df[4,2] <- NDVI_sr[1,2]
NDVI_df[4,3] <- NDVI_sr[2,2]
NDVI_df[4,4] <- NDVI_sr[3,2]

NDVI_df[5,2] <- NDVI_kstd[1,2]
NDVI_df[5,3] <- NDVI_kstd[2,2]
NDVI_df[5,4] <- NDVI_kstd[3,2]

NDVI_df[6,2] <- NDVI[1,2]
NDVI_df[6,3] <- NDVI[2,2]
NDVI_df[6,4] <- NDVI[3,2]

NDVI_df <- NDVI_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(NDVI_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/NDVI.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: SMI (Trockenheit/Dürre) 
SMI_pole <- bel_pole %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_pole))
SMI_grst <- bel_grst %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_grst))
SMI_mst <- bel_mst %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_mst))
SMI_sr <- bel_sr %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_sr))
bel_kstd_n <- bel_kstd %>% 
  drop_na(SMI_kl)
SMI_kstd <- bel_kstd_n %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_kstd_n))
uw_data_s <- uw_data %>% 
  drop_na(SMI_kl)
SMI <- uw_data_s %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(uw_data_s))

SMI_df <- df
SMI_df[1,2] <- SMI_pole[1,2]
SMI_df[1,3] <- SMI_pole[2,2]
SMI_df[1,4] <- SMI_pole[3,2]

SMI_df[2,2] <- SMI_grst[1,2]
SMI_df[2,3] <- SMI_grst[2,2]
SMI_df[2,4] <- SMI_grst[3,2]

SMI_df[3,2] <- SMI_mst[1,2]
SMI_df[3,3] <- SMI_mst[2,2]
SMI_df[3,4] <- SMI_mst[3,2]

SMI_df[4,2] <- SMI_sr[1,2]
SMI_df[4,3] <- SMI_sr[2,2]
SMI_df[4,4] <- SMI_sr[3,2]

SMI_df[5,2] <- SMI_kstd[1,2]
SMI_df[5,3] <- SMI_kstd[2,2]
SMI_df[5,4] <- SMI_kstd[3,2]

SMI_df[6,2] <- SMI[1,2]
SMI_df[6,3] <- SMI[2,2]
SMI_df[6,4] <- SMI[3,2]

SMI_df <- SMI_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(SMI_df, "data/Betroffenheit_Raumtypen/Siedlungstypen/SMI.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Siedlungstyps und abspeichern als Tabelle: Mehrfachbelastung 
df_m <- data.frame(matrix(ncol = 10, nrow = 5))
x <- c("Siedlungstyp", "keine Belastung (0)", "1 Belastung", 
       "2 Belastungen", "3 Belastungen", "4 Belastungen", "5 Belastungen", 
       "6 Belastungen", "7 Belastungen", "8 Belastungen")
colnames(df_m) <- x
df_m[1,1] <- "Zentrale Stadtregion"
df_m[2,1] <- "Großstadt"  
df_m[3,1] <- "Mittelstadt"  
df_m[4,1] <- "Städtischer Raum"  
df_m[5,1] <- "Kleinstädtischer, dörflicher Raum"
df_m[6,1] <- "Gesamt" 

mfb_pole <- bel_pole %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_pole))
mfb_grst <- bel_grst %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_grst))
mfb_mst <- bel_mst %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_mst))
mfb_sr <- bel_sr %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_sr))
mfb_kstd <- bel_kstd %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_kstd))
mfb_k <- uw_data %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(uw_data))

df_m[1,2] <- mfb_pole[1,2]
df_m[1,3] <- mfb_pole[2,2]
df_m[1,4] <- mfb_pole[3,2]
df_m[1,5] <- mfb_pole[4,2]
df_m[1,6] <- mfb_pole[5,2]
df_m[1,7] <- mfb_pole[6,2]
df_m[1,8] <- mfb_pole[7,2]
df_m[1,9] <- mfb_pole[8,2]
df_m[1,10] <- mfb_pole[9,2]

df_m[2,2] <- mfb_grst[1,2]
df_m[2,3] <- mfb_grst[2,2]
df_m[2,4] <- mfb_grst[3,2]
df_m[2,5] <- mfb_grst[4,2]
df_m[2,6] <- mfb_grst[5,2]
df_m[2,7] <- mfb_grst[6,2]
df_m[2,8] <- mfb_grst[7,2]
df_m[2,9] <- mfb_grst[8,2]
df_m[2,10] <- mfb_grst[9,2]

df_m[3,2] <- mfb_mst[1,2]
df_m[3,3] <- mfb_mst[2,2]
df_m[3,4] <- mfb_mst[3,2]
df_m[3,5] <- mfb_mst[4,2]
df_m[3,6] <- mfb_mst[5,2]
df_m[3,7] <- mfb_mst[6,2]
df_m[3,8] <- mfb_mst[7,2]
df_m[3,9] <- mfb_mst[8,2]
df_m[3,10] <- mfb_mst[9,2]

df_m[4,2] <- mfb_sr[1,2]
df_m[4,3] <- mfb_sr[2,2]
df_m[4,4] <- mfb_sr[3,2]
df_m[4,5] <- mfb_sr[4,2]
df_m[4,6] <- mfb_sr[5,2]
df_m[4,7] <- mfb_sr[6,2]
df_m[4,8] <- mfb_sr[7,2]
df_m[4,9] <- mfb_sr[8,2]
df_m[4,10] <- mfb_sr[9,2]

df_m[5,2] <- mfb_kstd[1,2]
df_m[5,3] <- mfb_kstd[2,2]
df_m[5,4] <- mfb_kstd[3,2]
df_m[5,5] <- mfb_kstd[4,2]
df_m[5,6] <- mfb_kstd[5,2]
df_m[5,7] <- mfb_kstd[6,2]
df_m[5,8] <- mfb_kstd[7,2]
df_m[5,9] <- mfb_kstd[8,2]
df_m[5,10] <- mfb_kstd[9,2]

df_m[6,2] <- mfb_k[1,2]
df_m[6,3] <- mfb_k[2,2]
df_m[6,4] <- mfb_k[3,2]
df_m[6,5] <- mfb_k[4,2]
df_m[6,6] <- mfb_k[5,2]
df_m[6,7] <- mfb_k[6,2]
df_m[6,8] <- mfb_k[7,2]
df_m[6,9] <- mfb_k[8,2]
df_m[6,10] <- mfb_k[9,2]

df_m <- df_m %>% 
  mutate(gesamt = (`keine Belastung (0)` + `1 Belastung` + `2 Belastungen` + 
                     `3 Belastungen` + `4 Belastungen` + `5 Belastungen` + 
                     `6 Belastungen` + `7 Belastungen` + `8 Belastungen`))

df_m_z <- df_m %>%
  mutate(`0-1 Belastung` = (`keine Belastung (0)` + `1 Belastung`)) %>%
  mutate(`2-3 Belastungen` = (`2 Belastungen` + `3 Belastungen`)) %>%
  mutate(`4-5 Belastungen` = (`4 Belastungen` + `5 Belastungen`)) %>%
  mutate(`6+ Belastungen` = (`6 Belastungen` + `7 Belastungen` + `8 Belastungen`)) 

df_m_z <- df_m_z %>%
  select(c(Siedlungstyp, `0-1 Belastung`, `2-3 Belastungen`, 
           `4-5 Belastungen`, `6+ Belastungen`)) %>%
  mutate(gesamt = (`0-1 Belastung` + `2-3 Belastungen` +
                     `4-5 Belastungen` + `6+ Belastungen`))

write.csv(df_m, "data/Betroffenheit_Raumtypen/Siedlungstypen/mehrfachbelastungen.csv")
write.csv(df_m_z, "data/Betroffenheit_Raumtypen/Siedlungstypen/mehrfachbelastungen_zusammenfassung.csv")

#Filtern der Daten nach regionalem Wohlstandsniveau
df <- data.frame(matrix(ncol = 4, nrow = 4))
x <- c("Wohlstand", 
       "geringe Belastung (1)", 
       "mittlere Belastung (2)", 
       "hohe Belastung (3)")
colnames(df) <- x
df[1,1] <- "unterdurchschnittlich"
df[2,1] <- "durchschnittlich"  
df[3,1] <- "überdurchschnittlich"  
df[4,1] <- "Gesamt" 

bel_wud <- uw_data %>%                                                              #unterdurchschnittlich
  filter(ökonomie %in% c(1, 2))
bel_wd <- uw_data %>%                                                               #durchschnittlich
  filter(ökonomie %in% c(3))
bel_wüd <- uw_data %>%                                                              #überdurchschnittlich
  filter(ökonomie %in% c(4, 5))

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Grundwasserqualität 
GW_Ql_k_wud <- bel_wud %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_wud))
GW_Ql_k_wd <- bel_wd %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_wd))
GW_Ql_k_wüd <- bel_wüd %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(bel_wüd))
GW_Ql_k <- uw_data %>%
  group_by(GW_Ql_k) %>%
  summarize(Ant = n()/nrow(uw_data))

GW_Ql_k_df <- df
GW_Ql_k_df[1,2] <- GW_Ql_k_wud[1,2]
GW_Ql_k_df[1,3] <- GW_Ql_k_wud[2,2]
GW_Ql_k_df[1,4] <- GW_Ql_k_wud[3,2]

GW_Ql_k_df[2,2] <- GW_Ql_k_wd[1,2]
GW_Ql_k_df[2,3] <- GW_Ql_k_wd[2,2]
GW_Ql_k_df[2,4] <- GW_Ql_k_wd[3,2]

GW_Ql_k_df[3,2] <- GW_Ql_k_wüd[1,2]
GW_Ql_k_df[3,3] <- GW_Ql_k_wüd[2,2]
GW_Ql_k_df[3,4] <- GW_Ql_k_wüd[3,2]

GW_Ql_k_df[4,2] <- GW_Ql_k[1,2]
GW_Ql_k_df[4,3] <- GW_Ql_k[2,2]
GW_Ql_k_df[4,4] <- GW_Ql_k[3,2]

GW_Ql_k_df <- GW_Ql_k_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(GW_Ql_k_df, "data/Betroffenheit_Raumtypen/Wohlstand/grundwasser_qualitaet.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Grundwassermenge 
GW_Qnt_wud <- bel_wud %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_wud))
GW_Qnt_wd <- bel_wd %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_wd))
GW_Qnt_wüd <- bel_wüd %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(bel_wüd))
GW_Qnt <- uw_data %>%
  group_by(GW_Qnt_) %>%
  summarize(Ant = n()/nrow(uw_data))

GW_Qnt_df <- df
GW_Qnt_df[1,2] <- GW_Qnt_wud[1,2]
GW_Qnt_df[1,3] <- GW_Qnt_wud[2,2]
GW_Qnt_df[1,4] <- GW_Qnt_wud[3,2]

GW_Qnt_df[2,2] <- GW_Qnt_wd[1,2]
GW_Qnt_df[2,3] <- GW_Qnt_wd[2,2]
GW_Qnt_df[2,4] <- GW_Qnt_wd[3,2]

GW_Qnt_df[3,2] <- GW_Qnt_wüd[1,2]
GW_Qnt_df[3,3] <- GW_Qnt_wüd[2,2]
GW_Qnt_df[3,4] <- GW_Qnt_wüd[3,2]

GW_Qnt_df[4,2] <- GW_Qnt[1,2]
GW_Qnt_df[4,3] <- GW_Qnt[2,2]
GW_Qnt_df[4,4] <- GW_Qnt[3,2]

GW_Qnt_df <- GW_Qnt_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(GW_Qnt_df, "data/Betroffenheit_Raumtypen/Wohlstand/grundwasser_menge.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Lärm 
bel_wud_l <- bel_wud %>%
  drop_na(laerm_kl)
laerm_wud <- bel_wud_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_wud_l))
bel_wd_l <- bel_wd %>%
  drop_na(laerm_kl)
laerm_wd <- bel_wd_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_wd_l))
bel_wüd_l <- bel_wüd %>%
  drop_na(laerm_kl)
laerm_wüd <- bel_wüd_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd_l))
uw_data_l <- uw_data %>%
  drop_na(laerm_kl)
laerm <- uw_data_l %>%
  group_by(laerm_kl) %>%
  summarize(Ant = n()/nrow(uw_data_l))

laerm_df <- df
laerm_df[1,2] <- laerm_wud[1,2]
laerm_df[1,3] <- laerm_wud[2,2]
laerm_df[1,4] <- laerm_wud[3,2]

laerm_df[2,2] <- laerm_wd[1,2]
laerm_df[2,3] <- laerm_wd[2,2]
laerm_df[2,4] <- laerm_wd[3,2]

laerm_df[3,2] <- laerm_wüd[1,2]
laerm_df[3,3] <- laerm_wüd[2,2]
laerm_df[3,4] <- laerm_wüd[3,2]

laerm_df[4,2] <- laerm[1,2]
laerm_df[4,3] <- laerm[2,2]
laerm_df[4,4] <- laerm[3,2]

laerm_df <- laerm_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(laerm_df, "data/Betroffenheit_Raumtypen/Wohlstand/laerm.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: LST (Wärmebelastung) 
bel_wud_l <- bel_wud %>%
  drop_na(lst_kl)
lst_wud <- bel_wud_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_wud_l))
bel_wd_l <- bel_wd %>%
  drop_na(lst_kl)
lst_wd <- bel_wd_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_wd_l))
bel_wüd_l <- bel_wüd %>%
  drop_na(lst_kl)
lst_wüd <- bel_wüd_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd_l))
uw_data_l <- uw_data %>%
  drop_na(lst_kl)
lst <- uw_data_l %>%
  group_by(lst_kl) %>%
  summarize(Ant = n()/nrow(uw_data_l))

lst_df <- df
lst_df[1,2] <- lst_wud[1,2]
lst_df[1,3] <- lst_wud[2,2]
lst_df[1,4] <- lst_wud[3,2]

lst_df[2,2] <- lst_wd[1,2]
lst_df[2,3] <- lst_wd[2,2]
lst_df[2,4] <- lst_wd[3,2]

lst_df[3,2] <- lst_wüd[1,2]
lst_df[3,3] <- lst_wüd[2,2]
lst_df[3,4] <- lst_wüd[3,2]

lst_df[4,2] <- lst[1,2]
lst_df[4,3] <- lst[2,2]
lst_df[4,4] <- lst[3,2]

lst_df <- lst_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(lst_df, "data/Betroffenheit_Raumtypen/Wohlstand/lst.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Luft O3 
O3_wud <- bel_wud %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_wud))
bel_wd_o <- bel_wd %>%
  drop_na(O3_kl)
O3_wd <- bel_wd_o %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_wd_o))
O3_wüd <- bel_wüd %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd))
uw_data_o <- uw_data %>%
  drop_na(O3_kl)
O3 <- uw_data_o %>%
  group_by(O3_kl) %>%
  summarize(Ant = n()/nrow(uw_data_o))

O3_df <- df
O3_df[1,2] <- O3_wud[1,2]
O3_df[1,3] <- O3_wud[2,2]
O3_df[1,4] <- O3_wud[3,2]

O3_df[2,2] <- O3_wd[1,2]
O3_df[2,3] <- O3_wd[2,2]
O3_df[2,4] <- O3_wd[3,2]

O3_df[3,2] <- O3_wüd[1,2]
O3_df[3,3] <- O3_wüd[2,2]
O3_df[3,4] <- O3_wüd[3,2]

O3_df[4,2] <- O3[1,2]
O3_df[4,3] <- O3[2,2]
O3_df[4,4] <- O3[3,2]

O3_df <- O3_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(O3_df, "data/Betroffenheit_Raumtypen/Wohlstand/O3.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Luft PM10 
PM10_wud <- bel_wud %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_wud))
PM10_wd <- bel_wd %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_wd))
PM10_wüd <- bel_wüd %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd))
PM10 <- uw_data %>%
  group_by(PM10_kl) %>%
  summarize(Ant = n()/nrow(uw_data))

PM10_df <- df
PM10_df[1,2] <- PM10_wud[1,2]
PM10_df[1,3] <- PM10_wud[2,2]
PM10_df[1,4] <- PM10_wud[3,2]

PM10_df[2,2] <- PM10_wd[1,2]
PM10_df[2,3] <- PM10_wd[2,2]
PM10_df[2,4] <- PM10_wd[3,2]

PM10_df[3,2] <- PM10_wüd[1,2]
PM10_df[3,3] <- PM10_wüd[2,2]
PM10_df[3,4] <- PM10_wüd[3,2]

PM10_df[4,2] <- PM10[1,2]
PM10_df[4,3] <- PM10[2,2]
PM10_df[4,4] <- PM10[3,2]

PM10_df <- PM10_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(PM10_df, "data/Betroffenheit_Raumtypen/Wohlstand/PM10.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Luft PM25 
PM25_wud <- bel_wud %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_wud))
bel_wd_p <- bel_wd %>%
  drop_na(PM25_kl)
PM25_wd <- bel_wd_p %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_wd_p))
PM25_wüd <- bel_wüd %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd))
uw_data_p <- uw_data %>%
  drop_na(PM25_kl)
PM25 <- uw_data_p %>%
  group_by(PM25_kl) %>%
  summarize(Ant = n()/nrow(uw_data_p))

PM25_df <- df
PM25_df[1,2] <- PM25_wud[1,2]
PM25_df[1,3] <- PM25_wud[2,2]
PM25_df[1,4] <- PM25_wud[3,2]

PM25_df[2,2] <- PM25_wd[1,2]
PM25_df[2,3] <- PM25_wd[2,2]
PM25_df[2,4] <- PM25_wd[3,2]

PM25_df[3,2] <- PM25_wüd[1,2]
PM25_df[3,3] <- PM25_wüd[2,2]
PM25_df[3,4] <- PM25_wüd[3,2]

PM25_df[4,2] <- PM25[1,2]
PM25_df[4,3] <- PM25[2,2]
PM25_df[4,4] <- PM25[3,2]

PM25_df <- PM25_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(PM25_df, "data/Betroffenheit_Raumtypen/Wohlstand/PM25.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: NDVI (Grünausstattung) 
NDVI_wud <- bel_wud %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_wud))
bel_wd_n <- bel_wd %>%
  drop_na(NDVI_kl)
NDVI_wd <- bel_wd_n %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_wd_n))
NDVI_wüd <- bel_wüd %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd))
uw_data_n <- uw_data %>%
  drop_na(NDVI_kl)
NDVI <- uw_data_n %>%
  group_by(NDVI_kl) %>%
  summarize(Ant = n()/nrow(uw_data_n))

NDVI_df <- df
NDVI_df[1,2] <- NDVI_wud[1,2]
NDVI_df[1,3] <- NDVI_wud[2,2]
NDVI_df[1,4] <- NDVI_wud[3,2]

NDVI_df[2,2] <- NDVI_wd[1,2]
NDVI_df[2,3] <- NDVI_wd[2,2]
NDVI_df[2,4] <- NDVI_wd[3,2]

NDVI_df[3,2] <- NDVI_wüd[1,2]
NDVI_df[3,3] <- NDVI_wüd[2,2]
NDVI_df[3,4] <- NDVI_wüd[3,2]

NDVI_df[4,2] <- NDVI[1,2]
NDVI_df[4,3] <- NDVI[2,2]
NDVI_df[4,4] <- NDVI[3,2]

NDVI_df <- NDVI_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(NDVI_df, "data/Betroffenheit_Raumtypen/Wohlstand/NDVI.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: SMI (Trockenheit/Dürre) 
SMI_wud <- bel_wud %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_wud))
bel_wd_s <- bel_wd %>%
  drop_na(SMI_kl)
SMI_wd <- bel_wd_s %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_wd_s))
SMI_wüd <- bel_wüd %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(bel_wüd))
uw_data_s <- uw_data %>%
  drop_na(SMI_kl)
SMI <- uw_data_s %>%
  group_by(SMI_kl) %>%
  summarize(Ant = n()/nrow(uw_data_s))

SMI_df <- df
SMI_df[1,2] <- SMI_wud[1,2]
SMI_df[1,3] <- SMI_wud[2,2]
SMI_df[1,4] <- SMI_wud[3,2]

SMI_df[2,2] <- SMI_wd[1,2]
SMI_df[2,3] <- SMI_wd[2,2]
SMI_df[2,4] <- SMI_wd[3,2]

SMI_df[3,2] <- SMI_wüd[1,2]
SMI_df[3,3] <- SMI_wüd[2,2]
SMI_df[3,4] <- SMI_wüd[3,2]

SMI_df[4,2] <- SMI[1,2]
SMI_df[4,3] <- SMI[2,2]
SMI_df[4,4] <- SMI[3,2]

SMI_df <- SMI_df %>% 
  mutate(gesamt = (`geringe Belastung (1)` + `mittlere Belastung (2)` + `hohe Belastung (3)`))

write.csv(SMI_df, "data/Betroffenheit_Raumtypen/Wohlstand/SMI.csv")

#Berechnung des Anteils der Belastungsklassen an allen Polygonen des Wohlstandsniveaus und abspeichern als Tabelle: Mehrfachbelastung 
df_m <- data.frame(matrix(ncol = 10, nrow = 4))
x <- c("Wohlstand", "keine Belastung (0)", "1 Belastung", 
       "2 Belastungen", "3 Belastungen", "4 Belastungen", "5 Belastungen", 
       "6 Belastungen", "7 Belastungen", "8 Belastungen")
colnames(df_m) <- x
df_m[1,1] <- "unterdurchschnittlich"
df_m[2,1] <- "durchschnittlich"  
df_m[3,1] <- "überdurchschnittlich"  
df_m[4,1] <- "Gesamt" 

mfb_wud <- bel_wud %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_wud))
mfb_wd <- bel_wd %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_wd))
mfb_wüd <- bel_wüd %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(bel_wüd))
mfb_k <- uw_data %>%
  group_by(anz_bel) %>%
  summarize(Ant = n()/nrow(uw_data))

df_m[1,2] <- mfb_wud[1,2]
df_m[1,3] <- mfb_wud[2,2]
df_m[1,4] <- mfb_wud[3,2]
df_m[1,5] <- mfb_wud[4,2]
df_m[1,6] <- mfb_wud[5,2]
df_m[1,7] <- mfb_wud[6,2]
df_m[1,8] <- mfb_wud[7,2]
df_m[1,9] <- mfb_wud[8,2]
df_m[1,10] <- mfb_wud[9,2]

df_m[2,2] <- mfb_wd[1,2]
df_m[2,3] <- mfb_wd[2,2]
df_m[2,4] <- mfb_wd[3,2]
df_m[2,5] <- mfb_wd[4,2]
df_m[2,6] <- mfb_wd[5,2]
df_m[2,7] <- mfb_wd[6,2]
df_m[2,8] <- mfb_wd[7,2]
df_m[2,9] <- mfb_wd[8,2]
df_m[2,10] <- mfb_wd[9,2]

df_m[3,2] <- mfb_wüd[1,2]
df_m[3,3] <- mfb_wüd[2,2]
df_m[3,4] <- mfb_wüd[3,2]
df_m[3,5] <- mfb_wüd[4,2]
df_m[3,6] <- mfb_wüd[5,2]
df_m[3,7] <- mfb_wüd[6,2]
df_m[3,8] <- mfb_wüd[7,2]
df_m[3,9] <- mfb_wüd[8,2]
df_m[3,10] <- mfb_wüd[9,2]

df_m[4,2] <- mfb_k[1,2]
df_m[4,3] <- mfb_k[2,2]
df_m[4,4] <- mfb_k[3,2]
df_m[4,5] <- mfb_k[4,2]
df_m[4,6] <- mfb_k[5,2]
df_m[4,7] <- mfb_k[6,2]
df_m[4,8] <- mfb_k[7,2]
df_m[4,9] <- mfb_k[8,2]
df_m[4,10] <- mfb_k[9,2]

df_m <- df_m %>% 
  mutate(gesamt = (`keine Belastung (0)` + `1 Belastung` + `2 Belastungen` + 
                     `3 Belastungen` + `4 Belastungen` + `5 Belastungen` + 
                     `6 Belastungen` + `7 Belastungen` + `8 Belastungen`))

df_m_z <- df_m %>%
  mutate(`0-1 Belastung` = (`keine Belastung (0)` + `1 Belastung`)) %>%
  mutate(`2-3 Belastungen` = (`2 Belastungen` + `3 Belastungen`)) %>%
  mutate(`4-5 Belastungen` = (`4 Belastungen` + `5 Belastungen`)) %>%
  mutate(`6+ Belastungen` = (`6 Belastungen` + `7 Belastungen` + `8 Belastungen`)) 

df_m_z <- df_m_z %>%
  select(c(Wohlstand, `0-1 Belastung`, `2-3 Belastungen`, 
           `4-5 Belastungen`, `6+ Belastungen`)) %>%
  mutate(gesamt = (`0-1 Belastung` + `2-3 Belastungen` +
                     `4-5 Belastungen` + `6+ Belastungen`))

write.csv(df_m, "data/Betroffenheit_Raumtypen/Wohlstand/mehrfachbelastungen.csv")
write.csv(df_m_z, "data/Betroffenheit_Raumtypen/Wohlstand/mehrfachbelastungen_zusammenfassung.csv")
