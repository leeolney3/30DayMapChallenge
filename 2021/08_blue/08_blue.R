#30DayMapChallenge 08 Blue
#Travis County: Water bodies 
#Data source: https://data.austintexas.gov/Locations-and-Maps/Hydrography-Polygons-2006/99y8-6pgc (updated Nov 1, 2021)

# load libraries
library(tidyverse)
library(sf)
library(ggtext)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Roboto")
font_add_google("Libre Franklin")
showtext_auto()

# read hyrdography polygons shp
hydro = read_sf('Hydrography_Polygons_2006/geo_export_92fa45d1-89f3-478e-83d4-087ecfbfdc83.shp')

# read county boundaries shp
# source: https://tnr-traviscountytx.opendata.arcgis.com/datasets/boundary/explore
traviscounty = read_sf('Boundary/Boundary.shp')

# read municipal jurisdictions shp
# source: https://tnr-traviscountytx.opendata.arcgis.com/datasets/municipal-jurisdictions/explore?location=30.325000%2C-97.770000%2C10.52
municipal = read_sf('data/Municipal_Jurisdictions/Municipal_Jurisdictions.shp')

# plot
ggplot() + 
  geom_sf(data=traviscounty, color="black", lwd=.4) +
  geom_sf(data=municipal, color="black", lwd=.2, fill="white") +
  geom_sf(data = hydro, color="#01a7c2", fill="#01a7c2", alpha=.5) +
  coord_sf(expand=F) +
  theme_void(base_family = "Libre Franklin") +
  theme(plot.title = element_markdown(size = 10,face="bold", hjust=.5),
        plot.caption=element_text(size=6, hjust=.5, family="Roboto", margin=margin(10,0,0,0)),
        plot.background=element_rect(fill="white", color=NA),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.subtitle = element_markdown(size = 8, hjust=.5,margin=margin(3, 0, 0, 0))) +
  labs(title = "Travis County <span style='color:#01a7c2;'>Water Bodies</span>", 
       subtitle = "30.2097°N / 97.6982°W<br>",
       caption="#30DayMapChallenge | Data source: data.austintexas.gov (updated Nov 1, 2021)")
       
# save
ggsave(file="08_blue.pdf", width=6, height=6,units="in")
ggsave(file="08_blue.png", width=6, height=6,units="in")

