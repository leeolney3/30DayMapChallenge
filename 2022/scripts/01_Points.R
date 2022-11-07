# 30DayMapChallenge 2022
# Day 1 Points
# Data source: Mapping Museums, https://museweb.dcs.bbk.ac.uk/data

# Libraries
library(tidyverse)
library(ggtext)
library(sf)
library(rnaturalearth)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Fonts
font_add_google("Open Sans")
f1 = "Open Sans"
font_add_google("Encode Sans")
f2 = "Encode Sans"

# Data
museums= readr::read_csv("data/MappingMuseumsData2021_09_30.csv") %>% janitor::clean_names()

# shp
sf1= ne_countries(country = 'united kingdom',returnclass = "sf", scale=10)
sf2 = ne_states(country = 'united kingdom',returnclass = "sf")
st_bbox(sf2)

# Wrangle
museums1 = museums %>% filter(latitude<70) %>% 
  separate(year_opened, sep=":", c("year_opened1", "year_opened2")) %>%
  separate(year_closed, sep=":", c("year_closed1", "year_closed2")) %>%
  mutate(closed=case_when(year_closed2==9999~"Still open", TRUE~"Closed"),
         gov = case_when(str_detect(governance,"Government")~"Government\nmuseums",
                         str_detect(governance,"Independent")~"Independent\nmuseums",
                         str_detect(governance,"University")~"University",
                         str_detect(governance,"Unknown")~"Unknown")) %>%
  filter(gov == "Government\nmuseums"|gov == "Independent\nmuseums") 

museums2 = st_as_sf(museums1, coords=c("longitude","latitude"), crs=raster::crs(sf1))
museums2a = st_intersection(museums2, sf1)

museums3 = st_as_sf(museums1 %>% group_by(gov) %>% mutate(id=row_number()) %>% ungroup() %>% filter(id==1) %>%
                      mutate(latitude=60.5, longitude=-7), coords=c("longitude","latitude"), crs=raster::crs(sf1))
museums3a = st_as_sf(museums1 %>% group_by(gov) %>% mutate(id=row_number()) %>% ungroup() %>% filter(id==1) %>%
                       mutate(latitude=60, longitude=-7), coords=c("longitude","latitude"), crs=raster::crs(sf1))
museums3b = st_as_sf(museums1 %>% group_by(gov) %>% mutate(id=row_number()) %>% ungroup() %>% filter(id==1) %>%
                       mutate(latitude=59.75, longitude=-7), coords=c("longitude","latitude"), crs=raster::crs(sf1))

# Plot
ggplot() +
  geom_sf(data=sf1, color=NA) +
  geom_sf(data=sf2, fill="transparent", color="#141318", size=.1) +
  geom_sf(data=museums2a, aes(fill=closed), size=1.2, shape=21, color="white", stroke=.1) +
  geom_sf_text(data=museums3, aes(label=gov), size=3.5, family=f1, hjust=0, lineheight=1, color="#D7DDE8") +
  geom_sf_text(data=museums3a, aes(label=c("548 closed","225	closed")), size=3, family=f1, hjust=0,fontface="bold", color="#EB4112") +
  geom_sf_text(data=museums3b, aes(label=c("2406 open","791	open")), size=3, family=f1, hjust=0,fontface="bold", color="#0091C6") +
  scale_fill_manual(values=c("#EB4112","#0091C6")) +
  facet_wrap(~gov) +
  coord_sf(xlim=c(-8.5,2.3), #https://gist.github.com/graydon/11198540
           datum = sf::st_crs(4258),
           expand=FALSE) +
  cowplot::theme_map(14) +
  theme(text=element_text(family=f1, color="#D7DDE8"),
        legend.position = "none",
        legend.title=element_blank(),
        legend.text=element_text(size=9),
        legend.justification = "center",
        legend.margin = margin(l=-10, b=-5, t=-5),
        panel.spacing = unit(2.2, "lines"),
        strip.text = element_blank(),
        plot.margin=margin(.5,.5,.5,.5,unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, margin=margin(b=15)),
        plot.caption.position = "plot",
        plot.caption=element_markdown(color="#C6C8D1", hjust=0, size=7.5, margin=margin(t=10), lineheight=1.3),
        plot.background = element_rect(fill="#141318", color=NA)
  ) +
  labs(title="UK Museums",
       caption="*Note: Locations of 2954 independent museums and 1016 government in UK, covering the period 1960 to September 2021.*<br>#30DayMapChallenge Day 1 Points  •  Source: Mapping Museums") +
  guides(fill=guide_legend(override.aes = list(size = 3)))

# Save
ggsave("01_points.png", height=7, width=7.6, unit="in")
