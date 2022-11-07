# 30DayMapChallenge 2022
# Day 5 Ukraine
# Annual precipitation in Ukraine

# Libraries
library(tidyverse)
library(sf)
library(raster)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Barlow")
f1 = "Barlow"

# Get climate data
climate=getData('worldclim', var='bio',res=2.5)
raster <- climate2$bio12 

# Mask raster
region = ne_states(country="ukraine", returnclass = "sf")
region2 = st_transform(region,crs(climate))
m = mask(raster, region2) 

regionb =ne_states(country="russia", returnclass = "sf") %>% filter(name=="Crimea")
regionb2 = st_transform(regionb,crs(climate))
m2 = mask(raster, regionb) 

# Dataframe
df_prec <- as.data.frame(m,xy=TRUE) %>% drop_na()
df_prec2 <- as.data.frame(m2,xy=TRUE) %>% drop_na()

# Plot
# Color palette
pal_ukraine <- c('#FFD700','#C0B72E','#40778A','#0057B7')

ggplot()+
  geom_raster(aes(x=x,y=y,fill=bio12),data=df_prec) +
  geom_raster(aes(x=x,y=y,fill=bio12),data=df_prec2) +
  geom_sf(data=region2, fill="transparent", color="#fafafa", size=.2) +
  geom_sf(data=regionb, fill="transparent", color="#fafafa", size=.2) +
  scale_fill_gradientn(colours=rev(pal_ukraine)) +
  coord_sf(expand=FALSE) +
  cowplot::theme_map(10) +
  theme(text=element_text(family=f1),
        legend.direction = "horizontal",
        legend.position = c(0,.1),
        plot.margin=margin(.5,1,.5,1, unit="cm"),
        plot.background=element_rect(fill="white",color=NA),
        legend.title=element_markdown(size=10),
        legend.text=element_markdown(size=8),
        plot.caption=element_text(size=7, color="grey20"),
        plot.title=element_text(vjust=-85, size=14, face="plain")
        ) +
  guides(fill=guide_colorbar(title.position = "top",barwidth = unit(8, "lines"),barheight = unit(.5, "lines"))) +
  labs(caption="#30DayMapChallenge Day 5 Ukraine\nSource: WorldClim via {raster} R package",
       fill="Annual precipitation (mm):",
       title="UKRAINE")

ggsave("05_Ukraine.png",bg="#fafafa", width=7, height=5)