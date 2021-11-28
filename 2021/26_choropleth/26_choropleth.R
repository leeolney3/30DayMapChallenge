#30DayMapChallenge 26 Choropleth map
#NYC population density per sq mile and bus stop shelters by neighbourhood area. 
#Data source: NYC Open Data
#https://data.cityofnewyork.us/City-Government/Internet-Master-Plan-Adoption-and-Infrastructure-D/fg5j-q5nk
##https://data.cityofnewyork.us/City-Government/2010-Neighborhood-Tabulation-Areas-NTAs-/cpf4-rkhq

library(tidyverse)
library(sf)
library(biscale)
library(cowplot)

library(sysfonts)
library(showtext)
font_add_google("Fira Sans Condensed")
font_add_google("Libre Franklin")
showtext_auto()
f1= "Fira Sans Condensed"
f2= "Libre Franklin"

inf = read_csv("data/Internet_Master_Plan__Adoption_and_Infrastructure_Data_by_Neighborhood.csv") %>%
  janitor::clean_names()

nta2010 = read_sf("data/2010_NTAs/geo_export_0c2105e0-475b-46e4-aef1-58948cdff0a7.shp")

inf2 = inf %>% 
  dplyr::select(ntacode=neighborhood_tabulation_area_code_nta_code,
                total_population,
                population_density_per_sq_mi,
                street_furniture_bus_stop_shelters)

inf_sf = left_join(nta2010, inf2, by="ntacode")

inf_sf2 = bi_class(inf_sf, x=population_density_per_sq_mi, y=street_furniture_bus_stop_shelters,
                   style = "quantile", dim = 3)
                   
map = ggplot() +
  geom_sf(data=inf_sf2, aes(fill=bi_class), size=.1,show.legend=F, color="white") +
  bi_scale_fill(pal="Brown", dim=3) +
  coord_sf(expand=F) +
  theme_void() +
  theme(plot.background=element_blank())
  
legend = bi_legend(pal = "Brown", 
            dim = 3,
            ylab = "Bus stop shelters",
            xlab = "Population density",
            size = 2.5) + 
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 16, family=f1, hjust=0, 
                                    color = "white", margin=margin(t=-5)),
        axis.title.y = element_text(size = 16, family=f1, hjust=0,
                                    color = "white", margin=margin(r=-5)),
        legend.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major=element_blank(),
        plot.background = element_blank(),
        legend.text.align = 0)

p1 = ggdraw(map) +
  #draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.64, 0.22, 0.22) +
  draw_label("NEW YORK CITY", x=0.04, y=0.94, fontfamily = f2, hjust=0, vjust=0,size=22, color="white", fontface="bold") +
  draw_label("Population density per sq mile and\nnumber of bus stop shelters by\nneighbourhood tabulation areas (NTAs)", x=0.04, y=0.85, fontfamily = f2, hjust=0, vjust=0, size=18.5, color="white", lineheight = 0.4) +
  draw_label("#30DayMapChallenge Nov 2021 | Data from NYC Open Data", x=0.8, y=0, size = 15, fontfamily = f2, color="white") +
  theme(plot.background = element_rect(fill="#212A2E", color=NA),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"))

ggsave("26_choropleth.png", 
       width=5, height=5, unit="in",
       dpi=300,
       type = "cairo-png"
       )
