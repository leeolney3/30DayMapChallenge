# 30DayMapChallenge 2022
# Day 3 Polygons
# Border changes in Africa 1886-2000
# Data source: https://icr.ethz.ch/data/cshapes/

# Libraries
library(tidyverse)
library(sf)
library(patchwork)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Data
s1 = read_sf("CShapes-2.0/CShapes-2.0.shp")
s2 = afrilearndata::africontinent 

s1a = st_transform(s1, crs=st_crs(s2))
s1a = st_make_valid(s1a)
s1b = st_crop(s1a, st_bbox(s2))
s1c = st_intersection(s1b, s2)

# Theme
font_add_google("Libre Franklin",bold.wt = 600)
f1 = "Libre Franklin"

c1="#192C30"
c2="#7CB0AE"
c3= "white"

theme1= cowplot::theme_map() +
  theme(plot.subtitle=element_text(family=f1, hjust=.5, size=10, color="#212529", face="bold"))

# Map
p3 = ggplot() +
  geom_sf(data=s1c %>% filter(gwsyear==1886), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1887, 1900)), 
          color="white", fill="#ee9b00", size=.3, alpha=.7) +
  theme1 + labs(subtitle="1887 - 1900")
  
p4 = ggplot() +
  geom_sf(data=s1c %>% filter(gwsyear==1886), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1887, 1900)), 
          color="white", fill="#cbc0d3", size=.3, alpha=.7) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1901, 1920)), 
          color="white", fill="#ee9b00", size=.3, alpha=.7) +
  theme1 + labs(subtitle="1901 - 1920")

p5 = ggplot() +
  geom_sf(data=s1c %>% filter(gwsyear==1886), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1887, 1900)), 
          color="white", fill="#cbc0d3", size=.3, alpha=.7) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1901, 1920)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1921, 1940)), 
          color="white", fill="#ee9b00", size=.3, alpha=.7) +
  theme1 + labs(subtitle="1921 - 1940")

p6 = ggplot() +
  geom_sf(data=s1c %>% filter(gwsyear==1886), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1887, 1900)), 
          color="white", fill="#cbc0d3", size=.3, alpha=.7) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1901, 1920)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1921, 1940)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1941, 1960)), 
          color="white", fill="#ee9b00", size=.3, alpha=.7) +
  theme1 + labs(subtitle="1941 - 1960")
  
p7 = ggplot() +
  geom_sf(data=s1c %>% filter(gwsyear==1886), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1887, 1900)), 
          color="white", fill="#cbc0d3", size=.3, alpha=.7) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1901, 1920)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1921, 1940)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1941, 1960)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1961, 1980)), 
          color="white", fill="#ee9b00", size=.3, alpha=.7) +
  theme1 + labs(subtitle="1961 - 1980")

p8 = ggplot() +
  geom_sf(data=s1c %>% filter(gwsyear==1886), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1887, 1900)), 
          color="white", fill="#cbc0d3", size=.3, alpha=.7) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1901, 1920)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1921, 1940)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1941, 1960)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1961, 1980)), 
          color="white", fill="#cbc0d3", size=.3) +
  geom_sf(data=s1c %>% filter(between(gwsyear, 1981, 2000)), 
          color="white", fill="#ee9b00", size=.3, alpha=.7) +
  theme1 + labs(subtitle="1981 - 2000")
  
# Combine plots
combined = (p3 + p4 + p5)/
  (p6 + p7 + p8)

combined +
  plot_annotation(title="Border changes in Africa (1886-2000)", 
                  caption="#30DayMapChallenge Day 3 Polygons  •  Source: The CShapes 2.0 Dataset",
                  theme=theme(plot.caption=element_text(family=f1, hjust=.5, color="#495057"),
                              plot.title=element_text(family=f1, hjust=.5,face="bold",margin=margin(t=10,b=10)))
  ) &
  theme(plot.background=element_rect(fill="#fafafa", color=NA))

ggsave("03_Polygons.png", width=8, height=7)

