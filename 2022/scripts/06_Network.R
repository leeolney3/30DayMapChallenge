# 30DayMapChallenge 2022
# Day 6 Network
# Road network in Harris County, Texas

# Libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("JetBrains Mono")
f1 = "JetBrains Mono"

# Data
harris_roads = tigris::roads(state="TX",county="Harris County")

# Plot
ggplot(harris_roads) +
  geom_sf(size=.1, color="#FFFDE8") +
  theme_void() +
  theme(text=element_text(family=f1, color="#FFFDE8"),
        legend.title=element_text(hjust=.5),
        plot.title=element_text(hjust=.5),
        plot.caption=element_text(size=7, hjust=.5),
        plot.margin=margin(.5,.7,.3,.5, unit="cm"),
        plot.background = element_rect(fill="#1D253B", color=NA)) +
  labs(caption="#30DayMapChallenge Day 6 Network | Source: US Census Bureau via {tigris} R package",
       title="Road Network in Harris County, TX")
       
 ggsave("06_Network.png", height=6, width=7.5)           
