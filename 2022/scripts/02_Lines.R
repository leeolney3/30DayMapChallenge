# 30DayMapChallenge 2022
# Day 2 Lines
# City of Chicago bus lines (as of October 7, 2022)
# Data source: Chicago Transit Authority, https://data.cityofchicago.org/Transportation/CTA-Bus-Routes-Shapefile/d5bx-dr8z

# Libraries
library(tidyverse)
library(sf)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Fonts
font_add_google("Space Mono")
f1="Space Mono"
font_add_google("Open Sans")
f2="Open Sans"

# Data
bus= read_sf("CTA_BusRoutes__2_/CTA_BusRoutes.shp")

# Wrangle
bus2= bus %>% mutate(grp = case_when(WKDAY==1 & SAT==1 & SUN==1 ~ "Weekdays and Weekends (n=86)",
                                     WKDAY==1 & SAT==1 & SUN==0 ~ "Weekdays and Saturday (n=6)",
                                     WKDAY==1 & SAT==0 & SUN==0 ~ "Weekdays (n=32)",
                                     WKDAY==0 & SAT==1 & SUN==1 ~ "Weekends (n=1)")) %>%
  drop_na(grp) %>%
  st_as_sf()

# Map
ggplot() +
  geom_sf(data=bus2, aes(color=factor(grp,levels=c("Weekdays and Weekends (n=86)","Weekdays (n=32)","Weekdays and Saturday (n=6)","Weekends (n=1)"))), alpha=.8) +
  scale_color_manual(values=c("#ffc300","#04724d","#721cb8","#FF5150")) +
  #scale_color_manual(values=c("#FCAD00","#00396B","#5C7D21","#B70300")) +
  coord_sf(expand=FALSE) +
  cowplot::theme_map(10) +
  theme(text=element_text(family=f1),
        legend.margin = margin(t=-200, l=-20),
        legend.title=element_markdown(size=16.5, lineheight = 0),
        legend.text=element_text(size=7.5),
        plot.caption.position = "plot",
        plot.caption=element_text(size=6.5,family=f2, hjust=.5, margin=margin(t=10)),
        plot.margin=margin(.5,1.5,.3,.5,unit="cm"),
        plot.background=element_rect(fill="#fafafa", color=NA)) +
  labs(color="<span style = 'font-size:10pt'>City of Chicago<br><br></span>CTA Bus Lines<br><span style = 'font-size:7pt'>(as of October 7, 2022)</span>",
       caption="#30DayMapChallenge Day 2 Lines  •  Source: Chicago Transit Authority") +
  guides(color=guide_legend(override.aes = list(alpha=1, size=4)))

ggsave("02_Lines.png",height=7, width=6.9)




