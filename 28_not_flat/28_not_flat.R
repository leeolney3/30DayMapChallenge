#30DayMapChallenge 28 The Earth Is Not Flat
#Total economic damages from disasters as a share of GDP 
#Data from Data from OurWorldinData.org

# load libraries
library(tidyverse)
library(sf)

# world shp 
world = spData::world %>% 
  st_as_sf()

world2 = world %>% select(iso_a2,name_long,geom) %>% arrange(name_long)

# copy data with {datapasta} 
# source: https://ourworldindata.org/explorers/natural-disasters?tab=table&time=latest&facet=none&Disaster+Type=All+disasters&Impact=Economic+damages+%28%25+GDP%29&Timespan=Annual&Per+capita=false&country=~OWID_WRL

df1 = df %>% mutate(pct=parse_number(pct)) %>% 
  rename(name_long=Country) %>%
  mutate(pct_bin = cut(pct,breaks=c(0.00, 0.05,0.1,0.5,1,10,21),
                        include.lowest = TRUE)) %>%
  mutate(name_long = case_when(name_long=="Democratic Republic of Congo"~"Democratic Republic of the Congo",
                     name_long=="Congo" ~ "Republic of the Congo", 
                     name_long=="South Korea" ~ "Republic of Korea",
                     TRUE~name_long))

# join world shp and df
joined = world2 %>% left_join(df1)

# crs
world_two_point_Equidistant = "+proj=tpeqd +lat_1=0 +lon_1=0 +lat_2=60 +lon_2=60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

world_polyconic = "+proj=poly +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# color pal
pal = c("#94d2bd","#0a9396","#005f73","#ee9b00","#ca6702","#ae2012")
# labels
lab = c("(0.00,0.05] %","(0.05,0.1] %","(0.1,0.5] %","(0.5,1] %","(1,10] %","(10,21] %","No Data")

# World Two Point Equidistant Projection
p1 = ggplot() +
  ggspatial::layer_spatial(data = joined, aes(fill=pct_bin),
                           size=.2, color="white") +
  scale_fill_manual(values=pal, labels=lab1, na.value="#adb5bd") +
  coord_sf(crs=world_two_point_Equidistant) +
  labs(title="Total economic damages from disasters as a share of 2020 GDP",
       subtitle="Geophysical, meteorological and climate events included",
       fill="",
       caption="World Two Point Equidistant Projection | #30DayMapChallenge Nov 2021| Data from OurWorldinData.org")+
  theme_minimal(base_size = 9.5, base_family = "sans") +
  theme(plot.background=element_rect(fill="white", color=NA),
        panel.grid=element_line(size=.4, color="#ced4da"),
        legend.position=c(0.03,0.9),
        legend.key.size = unit(.44, 'cm'),
        plot.title.position = "plot",
        plot.margin=margin(t=0.5, b=0.5, unit="cm"),
        plot.title=element_text(face="bold")
        )
        
ggsave("not_flat_p1.png", height=7, width=7, unit="in", dpi=300,
       type = "cairo-png")
       
# World Polyconic Projection
p2 = ggplot() +
  ggspatial::layer_spatial(data = joined, aes(fill=pct_bin),
                           size=.2,color="white") +
  scale_fill_manual(values=pal, labels=lab1, na.value="#adb5bd") +
  coord_sf(crs=world_polyconic) +
  labs(title="Total economic damages from disasters as a share of 2020 GDP",
       subtitle="Geophysical, meteorological and climate events included",
       fill="",
       caption="World Polyconic Projection | #30DayMapChallenge Nov 2021| Data from OurWorldinData.org")+
  theme_minimal(base_size = 9.5, base_family = "sans") +
  theme(plot.background=element_rect(fill="white", color=NA),
        legend.position=c(0.043,0.9),
        plot.title.position = "plot",
        plot.margin=margin(rep(.5,4),unit="cm"),
        legend.key.size = unit(.44, 'cm'),
        plot.title=element_text(face="bold")
        )

ggsave("not_flat_p2.png", height=7, width=7, unit="in", dpi=300,
       type = "cairo-png")