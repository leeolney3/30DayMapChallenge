#30DayMapChallenge 17 Land
#Land use districts in Calgary, Canada
#Data source: https://data.calgary.ca/Base-Maps/Land-Use-Districts/qe6k-p9nh (updated: Nov 11, 2021)

# load libraries 
library(tidyverse)
library(ggtext)
library(sf)

# read shp file 
land = read_sf("data/Land_Use_Districts/geo_export_966e037b-626b-4d41-b00a-ccd1894dd0fe.shp")

land2 = land %>% mutate(major = na_if(major, "<Null>")) %>%
  mutate(major=case_when(major=="Commercial - Core" ~ "Commercial",
                         TRUE~major))
                         
# load fonts
library(sysfonts)
library(showtext)
font_add_google("Roboto Condensed")
showtext_auto()

# plot
ggplot() +
  geom_sf(data=land2,aes(fill=major), size=.1, color ="white") +
  scale_fill_manual(values=c("#ae2012","#457b9d","#f0efeb","#4a4e69","#b58bae",
                             "#ccb7ae","#111d13","#606c38","#adc178","#f3722c",
                             "#f9c74f","#f8961e","#013a63")) +
  coord_sf(expand=F) +
  theme_void(base_size = 8) +
  theme(plot.margin=margin(.5,.5,.5,.5,unit="cm"),
        legend.key.height = unit(.4, 'cm'),
        legend.key.width = unit(.25, 'cm'),
        legend.text=element_text(family="Roboto Condensed"),
        plot.caption.position = "plot",
        plot.caption=element_text(size=6),
        legend.title=element_markdown(lineheight = 1.5,margin=margin(b=6))) +
  labs(fill="<span style = 'font-size:11pt';>**Land Use Districts<br>in Calgary, Canada**</span><br><span style = 'font-size:6pt;';>*Data source: data.calgary.ca*</span>",
       caption="#30DayMapChallenge Nov 2021") 
  
# save
ggsave("17_land.pdf", width=7, height=7, unit="in")
