#30DayMapChallenge 06 Green
#East Melbourne Green Canopy
#Data source: https://data.melbourne.vic.gov.au/Environment/Tree-Canopies-2021-Urban-Forest-/krg7-hmyt (last updated: Aug 12, 2021)

# load libraries 
library(sf)
library(tidyverse)
library(ggtext)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Libre Franklin")
font_add_google("Roboto")
showtext_auto()

# read tree canopy shp file
tree = read_sf('data/Tree_Canopies_2021/geo_export_e32098a2-c9e6-451b-82e7-d554ccd3213f.shp')

# read VIC suburb/locality boundaries shp file
# source: https://data.gov.au/dataset/ds-dga-af33dd8c-0534-4e18-9245-fc64440f742e/details
sub = read_sf('data/VIC_LOC_POLYGON_shp/vic_localities.shp') 
# get polygon of East Melbourne
east_melb = sub %>% filter(LOC_NAME=="East Melbourne")


# get East Melbourne area coords
obj = ggplot(data = east_melb) +
  geom_sf(color="black") 

xmin <- ggplot_build(obj)$layout$panel_params[[1]]$x_range[1]
xmax <- ggplot_build(obj)$layout$panel_params[[1]]$x_range[2]
ymin <- ggplot_build(obj)$layout$panel_params[[1]]$y_range[1]
ymax <- ggplot_build(obj)$layout$panel_params[[1]]$y_range[2]

# plot
ggplot(data = east_melb) +
  geom_sf(color="black", fill=NA) +
  geom_sf(data= tree, color="#7e8e49", alpha=.8, fill="#a3b18a") +
  coord_sf(xlim = c(144.9704, 144.9923), 
           ylim = c(-37.82389, -37.80696),
           expand = FALSE) +
  theme_void(base_family = "Libre Franklin") +
  theme(plot.title = element_markdown(size = 10,face="bold", hjust=.5),
        plot.caption=element_text(size=6, hjust=.5, family="Roboto", margin=margin(5,0,0,0)),
        plot.background=element_rect(fill="white", color=NA),
        plot.subtitle = element_markdown(size = 8, hjust=.5,margin=margin(3, 0, 0, 0))) +
  labs(title = "EAST MELBOURNE <span style='color:#7e8e49;'>TREE CANOPY</span>", 
       subtitle = "37.8130°S / 144.9850°E<br>",
       caption="#30DayMapChallenge Day 05 | Data source: data.melbourne.vic.gov.au (last updated: Aug 12, 2021)")

# save plot
ggsave(file="07_green.png", width=5, height=5.8, bg="white")
ggsave(file="07_green.pdf", width=5, height=5.8)

