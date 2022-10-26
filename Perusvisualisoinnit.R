# All the data is read in here:
# source("ReadInData.R")
source("helper_functions.R")



# Read in the "master table"
laguViz <- st_read("C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/A9_tuloksia/Laguuni_final2.shp")



m <- laguViz %>% certain_feats_as_strings() # From helper_functions.R

# Laguunit valtion alueilla

m %>%
  ggplot(aes(x = rknnstM)) +
  geom_bar()

# Ihmispaineiden visualisointi sisä/ulkosaaristossa
m %>%
ggplot(aes(x = ip_yht, fill = ulksrst)) +
  geom_bar(width = 0.5) +
  stat_bin(aes(label=..count..), vjust=-0.5, 
           geom="text", position="stack", binwidth = 1) +
  facet_wrap(~vltnLSA) +
  coord_cartesian(xlim = c(0,40), ylim = c(0, 500)) +
  scale_y_continuous(oob = scales::squish) +
  blue_theme() +
  labs(title="Ihmispaineiden määrä laguuneissa (satelliittikuvien perusteella)",
       x ="Ihmispaineita yhteensä laguunissa", 
       y = "Laguunien määrä")

ggsave("Visualisointeja/ihmispaineet.png", width = 5600, height = 2600, units = "px")

# Pelkästään ihmispaine == 0, koska elämä on hankalaa
m %>%
  filter(ip_yht == 0) %>%
  ggplot(aes(x = ip_yht, fill = ulksrst)) +
  geom_bar(width = 0.5) +
  stat_bin(aes(label=..count..), vjust=-0.5, 
           geom="text", position="stack", binwidth = 1) +
  facet_wrap(~vltnLSA) +
  scale_y_continuous(oob = scales::squish) +
  blue_theme() +
  labs(title="Ihmispaineiden määrä laguuneissa (satelliittikuvien perusteella)",
       x ="Ihmispaineita yhteensä laguunissa", 
       y = "Laguunien määrä")
ggsave("Visualisointeja/ihmispaineet0.png", width = 1000, height = 3000, units = "px")

# Ihmispaineet suhteessa eri fladatyyppeihin
m %>%
  ggplot(aes(x = ip_yht, fill = Morfolg)) +
  geom_bar(width = 0.5) +
#  stat_bin(aes(label=..count..), vjust=-0.5, 
 #          geom="text", position="stack", binwidth = 1) +
  facet_wrap(~vltnLSA) +
  coord_cartesian(xlim = c(0,40), ylim = c(0, 500)) +
  scale_y_continuous(oob = scales::squish) +
  blue_theme() +
  labs(title="Ihmispaineiden määrä laguuneissa (satelliittikuvien perusteella)",
       x ="Ihmispaineita yhteensä laguunissa", 
       y = "Laguunien määrä") +
  scale_fill_brewer(palette = "Accent")

ggsave("Visualisointeja/ihmispaineet_flada_asteet.png", width = 5600, height = 2600, units = "px")

m %>%
  filter(ip_yht == 0) %>%
  ggplot(aes(x = ip_yht, fill = Morfolg)) +
  geom_bar(width = 0.5) +
  #  stat_bin(aes(label=..count..), vjust=-0.5, 
  #          geom="text", position="stack", binwidth = 1) +
  facet_wrap(~vltnLSA) +
  scale_y_continuous(oob = scales::squish) +
  blue_theme() +
  labs(title="Ihmispaineiden määrä laguuneissa (satelliittikuvien perusteella)",
       x ="Ihmispaineita yhteensä laguunissa", 
       y = "Laguunien määrä") +
  scale_fill_brewer(palette = "Accent")

ggsave("Visualisointeja/ihmispaineet0_flada_asteet.png", width = 1000, height = 3000, units = "px")

m %>%
  ggplot(aes(x = as.integer(Shap_Ar))) +
  geom_bar()

