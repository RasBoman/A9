# Tämä skripti lukee sisään kaikki tarvittavat aineistot ja muokkaa lopullisen "master-taulukon"

# Kaikki kirjastot ja muutama funktio helper_functioneissa
source("helper_functions.R")

# Määritellään pari yleisintä polkua
PT <- "C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/"
MG <- "//mhnet.metsa.fi/dfs/MHGis/Tausta-aineistot/MERI_GIS/"

# Ahvenanmaan laguunit
LaguAhvPath <- "Laguuniaineisto_Ahvenanmaa_ja_muu_Suomi_lajisto_ihmispaineet/laguuniaineisto_ahvenanmaajasuomi_ip5_lajisto.shp"
LaguAhv <- read_sf(paste0(PT, LaguAhvPath)) %>% mutate(OBJECTID = as.numeric(OBJECTID))
glimpse(LaguAhv)
crs(LaguAhv)

# Saaristovyohykkeet
saaristoluokat <- read_sf(paste0(MG, "RAJAT_ja_RUUDUKOT/Saariston_vyöhykejako/SYKE_yhdistetty/Saaristoluokka.shp")) %>% st_transform(crs = 4326)
ulkosaaristo <- read_sf(MG, "RAJAT_ja_RUUDUKOT/Saariston_vyöhykejako/SYKE_yhdistetty/Ulkosaaristo.shp") %>% st_transform(crs = 4326)

# Ruovikkomalli SYKE(")
ruovikot_og <- rast(paste0(PT, "/RUOVIKOT2020_Syke_satelliitti/ruovikot.tif"))
ruovikot <-  ruovikot_og %>% project("epsg:4326")

# Rakennukset Suomessa
rakSuomiPath <- "RakennuksetSuomi/Rakennukset_suomi.shp"
rakSuomi <- read_sf(paste0(PT, rakSuomiPath))









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




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




MH_aluejako <- st_read(paste0(PT, "MerenhoidonAluejako/MerenhoidonAluejako.shp"))
MH_aluejako <- st_read(paste0(PT, "MerenhoidonAluejako/MerenhoidonAluejako.shp")) %>% st_transform(crs = 4326)
MH_aluejako
MH_aluejako %>%
  select(MHtaso4)
MH_aluejako <- MH_aluejako %>%
  select(MHtaso4)
MH_aluejako
MH_lagu <- laguKor2 %>% st_join(MH_aluejako)
source("helper_functions.R")
Lagu_final <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_final2.shp")
MH_aluejako <- st_read(paste0(PT, "MerenhoidonAluejako/MerenhoidonAluejako.shp")) %>% st_transform(crs = 4326)
MH_aluejako <- MH_aluejako %>%
  select(MHtaso4)
Lagu_final
Lagu_final <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_final3.shp")
Lagu_final <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_final3.shp")
MH_aluejako <- st_read(paste0(PT, "MerenhoidonAluejako/MerenhoidonAluejako.shp")) %>% st_transform(crs = 4326)
MH_aluejako <- MH_aluejako %>%
  select(MHtaso4)
MH_lagu <- Lagu_final %>% st_join(MH_aluejako)
sf::sf_use_s2(FALSE)
MH_lagu <- Lagu_final %>% st_join(MH_aluejako)


