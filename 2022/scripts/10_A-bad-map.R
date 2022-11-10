# 30DayMapChallenge 2022
# Day 10 A bad map
# Data source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-11-08

# Libraries
library(tidyverse)
library(cartogram)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Serif")
font_add_google("IBM Plex Sans")

# Data
state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

sf = tigris::states(cb=TRUE) %>%
  filter(!STUSPS %in% c("AK","HI","PR","GU","AS","MP","VI","DC"))
sf = sf::st_transform(sf, crs = "ESRI:102003") #USA Contiguous Albers Equal Area Conic ESRI

# Map data
stations= state_stations %>% count(state) %>% 
  mutate(STUSPS=str_replace_all(state,"_"," "),
         STUSPS=state.abb[match(STUSPS,state.name)])%>%
  filter(!STUSPS %in% c("AK","HI","PR","GU","AS","MP","VI","DC")) %>%
  select(-state)

joined = sf %>% left_join(stations, by="STUSPS") 

ncont = cartogram_ncont(joined, weight = "n") #Non-Contiguous Cartogram

# Overlapping map
ggplot() +
  geom_sf(data=ncont,fill="#31504B",color="#FFFBF7", size=.4) +
  coord_sf() +
  cowplot::theme_map(12) +
  theme(plot.margin=margin(.7,.5,.3,.5,unit="cm"),
        plot.background = element_rect(color=NA, fill="#FFFBF7"),
        plot.title=element_text(hjust=.5,family="IBM Plex Serif", color="#31504B"),
        plot.caption=element_text(size=8.5,hjust=.5,color="#31504B", family="IBM Plex Sans"),
        ) +
  labs(title="Lower 48 states in proportion to the number of radio stations\n",
       caption="#30DayMapChallenge Day 10: A bad map | Data from Wikipedia via #TidyTuesday")
       
ggsave("10_A-bad-map.png", height=6, width=8)       