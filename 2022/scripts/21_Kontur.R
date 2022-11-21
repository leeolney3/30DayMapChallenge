# 30DayMapChallenge
# 21 Data: Kontur Population Dataset
# UAE population 2022, data from https://data.humdata.org/dataset/kontur-population-united-arab-emirates

# Libraries
library(tidyverse)
library(sf)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Inter")
f1 = "Inter"
font_add_google("Archivo Narrow")
f2 = "Archivo Narrow"

# Data
R.utils::gunzip("kontur_population_AE_20220630.gpkg.gz", remove=FALSE)
kontur <- st_read("kontur_population_AE_20220630.gpkg")

# Map
ggplot() +
  geom_sf(data = kontur, aes(fill = population,colour = population), show.legend = FALSE) +
  scale_fill_viridis_c(option="cividis",trans = "pseudo_log",direction=-1) +
  scale_color_viridis_c(option="cividis",trans = "pseudo_log",direction=-1) +
  coord_sf() +
  annotate(geom="text", x=5733056, y=3000000, label="United Arab Emirates\nPopulation", hjust=0, family=f1, fontface="bold", size=4.5, lineheight=1, vjust=0, color="#0A244F") +
  annotate(geom="text", x=5733056, y=2985000, label="#30DayMapChallenge 21 Data: Kontur Population Dataset", hjust=0, family=f2, size=2.5, vjust=0, color="#0A244F") +
  cowplot::theme_map()

ggsave("21_Kontur_w.png",height=6, width=7.3, bg="#fafafa")
