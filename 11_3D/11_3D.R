# Agriculture land use in Africa: Percentage of country land used for agriculture in 2019 
# data source: https://www.fao.org/faostat/en/#data/RL
# shp files source: {afrilearndata}

library(tidyverse)
library(janitor)
library(afrilearndata)
library(sf)
library(rayshader)

# import data 
data = read_csv("agriculture.csv") %>% clean_names()

# prepare data
df1 = data %>% dplyr::select(area, item, unit, value) %>%
  group_by(area) %>%
  mutate(pct = (value/lag(value))) %>%
  drop_na() %>%
  ungroup() %>%
  rename(name_long=area)
  
summary(df1$pct)
  
s1 = merge(df1,africountries)
merged <- st_as_sf(s1)

# ggplot 2d
p1 = ggplot() +
  #geom_sf(data=africountries) +
  geom_sf(data=merged, aes(fill=pct)) +
  colorspace::scale_fill_continuous_sequential(palette="Peach", #batlow
                                               limits=c(0.0337,0.7942),
                                               breaks=c(0.0337,0.2,0.4,0.6,0.7942),
                                               labels=c("3.37%","20%","40%","60%","79.42%")) +
  theme_void() +
  theme(plot.margin=margin(1,1,1,1, unit="cm"),
  		plot.title.position="plot",
  		plot.caption.position="plot",
  		plot.title = element_text(hjust=.5, face="bold", size=10),
  		plot.subtitle = element_text(hjust=.5, size=8),
  		plot.caption=element_text(size=6),
  		legend.text=element_text(size=7),
  		plot.background=element_rect(fill="white", color=NA)) +
  labs(fill="", title="Agriculture land use in Africa", subtitle= "Percentage of country land area used for agriculture in 2019", caption="Note: Countries with no data on agriculture land area are not shown in this map.\n#30DayMapChallenge Nov 2021 | Data from fao.org and {afrilearndata}") +
  guides(fill=guide_colorbar(barwidth = unit(.5, "lines"),
                             barheight = unit(8, "lines")))

# 3d plot  		
render_snapshot(clear=TRUE)
plot_gg(p1,theta=-10, phi=70, width=5, height=5, zoom=.6)
