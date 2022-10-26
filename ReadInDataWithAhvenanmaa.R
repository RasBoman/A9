library(sf)
library(tidyverse)

# Kansio paikkatietoaineistoille
PT <- "C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/"

# Luetaan ihmispaineistetut laguunit sisään
LaguLajiPath <- "Laguunit_lajit_ihmispaineet_kumulatiivinen_paine_lisätty/lgdata_laguuneissa_laguunintiedoilla.shp"
LaguIP <- read_sf(paste0(PT, LaguPath))

# Laguunien lajiaineisto
LaguPath <- "Laguunit_lajit_ihmispaineet_kumulatiivinen_paine_lisätty/Laguunit2021_lajisto_kumulatiivisetihmispaineet.shp"

# Luetaan "laguuniMasterTaulukko" sisään
LaguKaikkiPath <- "Kaikki_laguunit4/kaikki_laguunit_4.shp"
laguKaikki <- read_sf(paste0(PT, LaguKaikkiPath))
laguKaikki <- st_transform(laguKaikki, crs = 4326)

# Rakennukset Suomessa

rakSuomiPath <- "RakennuksetSuomi/Rakennukset_suomi.shp"
rakSuomi <- read_sf(paste0(PT, rakSuomiPath))


# Lasketaan rakennukset laguunien ympärillä
# Luodaan 60 metrin bufferi laguunien ympärille
LaguBuff60 <- st_buffer(laguKaikki, dist = 60)
LaguBuffIntersect <- LaguBuff60
LaguBuff60$rak_maara <- lengths(st_intersects(LaguBuff60, rakSuomi))

# Lasketaan bufferien sisään osuvat rakennukset

# Luodaan kerroin, joka kertoo kuinka paljon ruoppaukset vaikuttavat
RuopKerroin <- 5
LaitKerroin <- 2
SatamaKerroin <- 20

ggplot(data = LaguIP) +
  geom_bar(aes(x = RuopYht)) +
  geom_bar(aes(x = LaitYht), fill = "green")

LaguKertoimilla <- LaguIP %>%
  mutate(RuopKertoimella = RuopYht * RuopKerroin,
         LaitKertoimella = LaitYht * LaitKerroin, 
         SataKertoimella = (Satama8 + Satama9) * SatamaKerroin)
