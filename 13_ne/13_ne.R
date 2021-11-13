#30DayMapChallenge 13 (Data challenge 2: Natural Earth)
#Countries in Central America with state boundaries, shaded relief, hypsography, and ocean bottom
#Data from Natural Earth via {rnaturalearth}

library(tidyverse)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

# load country polygon
ca <- ne_countries(country = c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama"),returnclass = "sf")

# load state polygon
states <- ne_states(country = c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama"),returnclass = "sf")

# load gray earth with shaded relief, hypsography, and ocean bottom
grey = raster("data/GRAY_50M_SR_OB/GRAY_50M_SR_OB.tif")

# crop raster
grey2 = crop(grey, tmaptools::bb(ca, 1.1))

# plot 
tm_shape(grey2) + tm_raster(palette = "Greys", legend.show = F) + 
  tm_shape(states) + 
  tm_fill(alpha=.4, col="admin", 
              palette=c("#7F3C8D","#11A579","#3969AC","#F2B701","#E73F74","#80BA5A","#E68310"),
              #palette=c("#1D6996","#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05","#CC503E"),
              legend.show=F) +
  tm_borders(col="grey20", lwd=.5) +
  tm_shape(ca) + tm_text("admin", col="white", size=.6, shadow=TRUE) +
  tm_layout(frame = FALSE,
            title.position = c("left","bottom"),
            title="Countries in Central America with state boundaries,\nshaded relief, hypsography, and ocean bottom.\n#30DayMapChallenge Nov 2021\nData from Natural Earth via {rnaturalearth}",
            title.size = .5) 
            
# save
tmap_save(plot, "13_ne.png", height=6)
tmap_save(plot, "13_ne.pdf", height=6)