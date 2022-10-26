
source("helper_functions.R")

# laguKor <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_final3.shp")
laguKor <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_MHAlueella.shp")


# Kokeillaan manuaalista koritusta
laguKorit <- laguKor %>%
  mutate(Kori = case_when(rknnstM < 2 & ip_yht < 2 & ulksrst == 1 & Suu == 1 & ruovikt < 0.2 ~ "TET", # Toimenpidetarvetta ei tunnistettu
                          rknnstM < 2 & ip_yht < 2 & ulksrst == 1 & Suu == 1 & ruovikt > 0.2 ~ "TET_RUOV",
                          rknnstM > 15 | ip_yht > 20 | Satama8 > 0 | Satama9 > 0 | kumIPMx > 200 ~ "TH", # Toimenpiteet haastavia 
                          TRUE ~ as.character("UNCERT")))


ggplot(data = laguKorit) +
  geom_bar(aes(x = Kori)) +
  theme_bw() +
  theme()

#ggsave("Visualisointeja/Peruskoritus.png", width = 5600, height = 2600, units = "px")

TT_kori <- laguKorit %>%
  filter(Kori == "UNCERT")

TT_kori %>% 
  certain_feats_as_strings() %>% # From helper_functions.R
  ggplot(aes(x = vltnLSA)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Kohde luonnonsuojelualueilla") +
  blue_theme() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1))

ggsave("Visualisointeja/TT_kori_LSA.png", width = 2000, height = 2600, units = "px")

# Ruovikkomallit
TT_kori %>%
  ggplot(aes(x = ruovikt * 100)) +
  geom_histogram(stat = "bin",
                 bins = 20) +
  blue_theme() +
  xlab("Ruovikkopeittävyys laguuneissa (%)") +
  ylab("Laguunien määrä") + 
  ggtitle("Laguunit ja ruovikkomalli")

ggsave("Visualisointeja/Ruovikkomalli.png", width = 2000, height = 2600, units = "px")

TT_kori %>%
  certain_feats_as_strings() %>%
  filter(krtPntC > 0) %>%
  ggplot(aes(x = krtLjLk, y = krtPntC, color = ulksrst)) +
  geom_point(alpha = 0.6, size = 5) +
  blue_theme() +
  xlab("Kartoitettujen lajien lkm / laguuni") +
  ylab("Kartoituspisteiden määrä / laguuni") +
  geom_smooth(method = lm, se = FALSE)

ggsave("Visualisointeja/kartoitusPisteetJaLajit.png", width = 5000, height = 2600, units = "px")

TT_kori 
laguKor %>%
  certain_feats_as_strings() %>%
  mutate(Kohde_kartoitettu = ifelse(krtPntC > 0, "Kyllä", "Ei")) %>%
  ggplot(aes(x = Kohde_kartoitettu)) +
    geom_bar() +
    facet_wrap(~vltnLSA) +
    blue_theme() +
    geom_label(stat = 'count',aes(label=..count..), vjust = 0.5) +
  ggtitle("Kohde kartoitettu")

ggsave("Visualisointeja/KohdeKartoitettuSuojelualueittain.png", width = 5000, height = 2600, units = "px")
