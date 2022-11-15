# 30DayMapChallenge 2022, Day 15: Food/drink
# Data source: https://github.com/friendlycities-gatech/chainness
# Citation: Liang, X., & Andris, C. (2021). Measuring McCities: Landscapes of chain and independent restaurants in the United States. Environment and Planning B: Urban Analytics and City Science, 49(2), 585-602. https://doi.org/10.1177/23998083211014896

# Libraries
library(tidyverse)
library(sf)
library(albersusa)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Fonts
font_add_google("Open Sans", bold.wt = 600)
f1 = "Open Sans"
font_add_google("Libre Franklin")
f2 = "Libre Franklin"

# Data
# csv from https://github.com/friendlycities-gatech/chainness/tree/main/data
p1 = readr::read_csv("https://raw.githubusercontent.com/friendlycities-gatech/chainness/main/data/chainness_point_2021_part1.csv")
p2 = readr::read_csv("https://raw.githubusercontent.com/friendlycities-gatech/chainness/main/data/chainness_point_2021_part2.csv")
p3 = readr::read_csv("https://raw.githubusercontent.com/friendlycities-gatech/chainness/main/data/chainness_point_2021_part3.csv")
chain = rbind(p1,p2,p3)

# share of chain restaurants by MSA/CBSAs
chain1= chain %>% 
  drop_na(MSA_GEOID) %>%
  count(MSA_GEOID,isChain) %>%
  group_by(MSA_GEOID) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% filter(isChain==1) %>%
  mutate(MSA_GEOID=as.character(MSA_GEOID))

# CBSAs boundary source: https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
cbsa = read_sf("data/cb_2021_us_cbsa_20m/cb_2021_us_cbsa_20m.shp")
join = cbsa %>% left_join(chain1, by=c("CBSAFP"="MSA_GEOID")) # join with chain data
join2 = points_elided_sf(join) #Shift points around Alaska and Hawaii

# Map
join2 %>% 
  ggplot() +
  geom_sf(data=usa_sf("laea"), size=.1, fill="white", alpha=.4) +
  geom_sf(aes(fill=prop), size=.1) +
  scale_fill_stepsn(colors=rev(scico::scico(6, palette = 'lapaz')),na.value="transparent", labels=scales::percent, guide=guide_colorsteps(title.position="top",barwidth = unit(11, "lines"), barheight=unit(.5,"lines"))) +
  coord_sf(xlim=c(-2100000,2516374), ylim=c(-2500000,732103.3), expand = FALSE) +
  cowplot::theme_map(11) +
  theme(text=element_text(family=f1, color="#212529"),
        legend.justification = "center",
        legend.position = "top",
        legend.title=element_blank(),
        legend.margin=margin(b=-13),
        plot.title=element_text(hjust=.5,family=f2),
        plot.margin=margin(.5,.5,.3,.5,unit="cm"),
        plot.caption=element_text(size=7,hjust=0, color="grey25",margin=margin(t=15)),
        plot.background = element_rect(color=NA, fill="#fafafa")) +
  labs(caption="#30DayMapChallenge Day 15 Food/drink\nSource: Liang, X., & Andris, C. (2021). Measuring McCities: Landscapes of chain and independent restaurants in the United States. Environment and\nPlanning B: Urban Analytics and City Science, 49(2), 585-602. https://doi.org/10.1177/23998083211014896",
       title="Share of chain restaurants by Core Based Statistical Area (CBSA)") 

ggsave("15_Food-drink.png", height=6, width=7)

