# 30DayMapChallenge 2022, Day 13: 5 minute map, data from US Census via {tidycensus} R package

# Libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(usmap)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Inter",bold.wt = 600)
f1 = "Inter"

# Data (code reference: https://walker-data.com/census-r/an-introduction-to-tidycensus.html)
d1 = get_acs(
  geography = "county",
  state = .midwest_region,
  variables = c(medage = "B01002_001"),
  output = "wide",
  year = 2020,
  geometry = TRUE
)

# Map
ggplot() +
  geom_sf(data=d1, aes(fill=medageE), size=.11, color="white") +
  scale_fill_stepsn(colors=rev(PNWColors::pnw_palette("Sunset2"))) +
  coord_sf(expand=FALSE) +
  cowplot::theme_map(12) +
  theme(text=element_text(family=f1),
        legend.title=element_blank(),
        legend.position="top",
        legend.margin = margin(l=-5,t=5, b=-10),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, size=9, color="grey20",margin=margin(t=15)),
        plot.margin=margin(.7,.7,.5,.7,unit="cm")) +
  labs(caption="#30DayChartChallenge Day 13: 5 minute map | Source: US Census via {tidycensus} package",
       title="Estimated median age in the Midwest by county, 2016-2020 5-year ACS") +
  guides(fill=guide_colorsteps(barwidth = unit(7, "lines"),barheight = unit(.6, "lines")))

ggsave("15_5min.png", height=7, width=7.7, bg="#F0F0F0")