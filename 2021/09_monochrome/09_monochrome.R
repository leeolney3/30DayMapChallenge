#30DayMap Challenge 09 Monochrome
#City of VancouverZoning districts and labels (October 11, 2021) from 
#Data source: https://opendata.vancouver.ca/explore/dataset/zoning-districts-and-labels/export/?location=14,49.25788,-123.12405

# load libraries 
library(tidyverse)
library(ggtext)
library(sf)
library(scico)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Lato")
font_add_google("Libre Franklin")
showtext_auto()

# read shp
zone = read_sf('data/zoning-districts-and-labels/zoning-districts-and-labels.shp')

# wrangle
dewelling = zone %>% 
  mutate(grp = case_when(zoning_clas=="Multiple Dwelling"~"Multiple Dwelling",
                         zoning_clas=="Two-Family Dwelling"~"Two-Family Dwelling",
                         zoning_clas=="One-Family Dwelling"~"One-Family Dwelling",
                         TRUE~"Other Uses"
                         )) %>%
  mutate(grp = factor(grp, levels=c("Other Uses","One-Family Dwelling","Two-Family Dwelling","Multiple Dwelling")))
  
# get gray colors from {scico}
scico(5, palette='grayC')

# plot
ggplot() +
  geom_sf(data = dewelling, 
          aes(fill=grp, color=grp)) +
  scale_fill_manual("<b>Vancouver Residential<br>Zoning Districts</b><br><span style='font-size:5.5pt;'>as of October 11, 2021</span>",
                    values=c("#B8B8B8","#767676","#3B3B3B","#000000")) +
  scale_color_manual("<b>Vancouver Residential<br>Zoning Districts</b><br><span style='font-size:5.5pt;'>as of October 11, 2021</span>",
                     values=c("#B8B8B8","#767676","#3B3B3B","#000000")) +
  guides(color=guide_legend(reverse=T), fill=guide_legend(reverse=T)) +
  theme_void(base_family = "Lato", base_size = 7)+
  theme(legend.key.size = unit(.3, 'cm'),
        legend.position = c(0.15, 0.835),
        legend.title=element_markdown(
          lineheight = 1.3, size=10, family="Libre Franklin", margin=margin(b=5)),
        ) +
  annotate(geom="text", x=-123.24, y=49.203, family="Lato", hjust=0, size=2,
           label="#30DayMapChallenge 09 Monochrome\nData source: opendata.vancouver.ca")
 