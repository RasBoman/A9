# HERE BE LIBRARIES AND HELPER FUNCTIONS (AND NOTHING ELSE) FOR DATA WRANGLING AND VISUALISATION
library(sf)
library(tidyverse)
library(terra)
library(scales)
library(RColorBrewer)
library(viridis)

# Kansio paikkatietoaineistoille
PT <- "C:/Users/rasmusbo/OneDrive - Metsahallitus/PAIKKATIETO/"

certain_feats_as_strings <- function(lagu_df) {lagu_df %>%
  mutate(vltnLSA = case_when(vltnLSA == 0 ~ "Muut alueet",
                             vltnLSA == 1 ~ "Valtion luonnonsuojelualue",
                             vltnLSA == 2 ~ "Yksityinen suojelualue",
                             vltnLSA == 3 ~ "YSA ja Valtion suojelualue",
                             TRUE ~ as.character("NA"))) %>%
  mutate(ulksrst = case_when(ulksrst == 0 ~ "Sisäsaaristo",
                             ulksrst == 1 | 2 ~ "Ulkosaaristo",
                             TRUE ~ as.character("NA"))) %>%
  mutate(Morfolg = case_when(Morfolg == 1 ~ "Esiflada",
                             Morfolg == 2 ~ "Flada",
                             Morfolg == 3 ~ "Kluuviflada",
                             Morfolg == 4 ~ "Kluuvi",
                             Morfolg == 5 ~ "Kluuvijärvi",
                             Morfolg == 6 ~ "Laguuni",
                             TRUE ~ as.character("NA")))
}

# Create theme for plotting consistency
blue_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
    # color background 2)
    panel.background = element_rect(fill = "aliceblue"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "steelblue",),
    axis.title = element_text(colour = "steelblue", family = "Times New Roman"),
    axis.ticks = element_line(colour = "steelblue"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    # legend title removed
    legend.title=element_blank()
  )
}

