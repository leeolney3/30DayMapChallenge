#30DayMapChallenge 18 Water
#The Great Lakes
#Map data: Natural Earth via {rnaturalearth}
#Fishing data: Great Lakes Fishery Commission (TidyTuesday w24)
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-08/readme.md

# load libraries 
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggnewscale)
library(raster)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Roboto Condensed")
font_add_google("Libre Franklin")
showtext_auto()
f1 = "Roboto Condensed"

#import fishing data
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

# lake whitefish 
fish = fishing %>% filter(species=="Lake Whitefish", year==2015) %>%
  dplyr::select(name=lake, n=grand_total) %>% distinct() %>%
  mutate(n=case_when(name=="Michigan"~3095.0000, TRUE~n))
fish

# great lakes shp
gl = ne_download(scale=50, type="lakes", category="physical", returnclass = "sf")

lake_names <- c("Erie","Huron","Michigan","Ontario","Saint Clair","Superior")

gl2 = gl %>% mutate(name = stringr::str_remove(name, "Lake ")) %>%
  filter(name %in% lake_names) %>%
  mutate(name= factor(name, levels = lake_names,
                       ordered = TRUE))
                       
gl_fish = inner_join(gl2, fish, by="name") # join 

# state base map
p1 = ggplot() +
  #state lines
  geom_sf(data=states2, aes(fill=geonunit), show.legend=F, size=.3) +
  scale_fill_manual(values=c("#adb5bd","#ced4da")) +
  geom_sf_text(data=states2 %>% filter(name!="Ontario", name!="Pennsylvania", name!="Michigan",
                                       name!="Iowa"), 
               aes(label=str_to_upper(name)), size=2.5, family=f1) +
  geom_sf_text(data=states2 %>% filter(name=="Ontario"), aes(label=str_to_upper(name)), size=2.5, family=f1, vjust=1.8) +
  geom_sf_text(data=states2 %>% filter(name=="Ontario"), aes(label="QUEBEC"), size=2.5, family=f1, vjust=1.8, hjust=-11) +
  geom_sf_text(data=states2 %>% filter(name=="Pennsylvania"), aes(label=str_to_upper (name)), size=2.5, family=f1, vjust=-1.2) +
  geom_sf_text(data=states2 %>% filter(name=="Pennsylvania"), aes(label="OHIO"), size=2.5, family=f1, vjust=-1.2, hjust=9.6) +
  geom_sf_text(data=states2 %>% filter(name=="Pennsylvania"), aes(label="ILLINOIS"), size=2.5, family=f1, vjust=-1.2, hjust=13.5) +
  geom_sf_text(data=states2 %>% filter(name=="Pennsylvania"), aes(label="INDIANA"), size=2.5, family=f1, vjust=-1.2, hjust=10) +
  geom_sf_text(data=states2 %>% filter(name=="Michigan"), aes(label=str_to_upper(name)), size=2.5, family=f1, vjust=16, hjust=-1) +
  geom_sf_text(data=states2 %>% filter(name=="Iowa"), aes(label=str_to_upper(name)), size=2.5, family=f1, hjust=-1) +
  geom_sf_text(data=states2 %>% filter(name=="Iowa"), aes(label="MINNESOTA"), size=2.5, family=f1, hjust=-.4,vjust=-42) +
  #pop places
  geom_sf(data=places, aes(label=NAME), size=1) +
  geom_sf_text(data=places %>% filter(NAME!="Ottawa"), 
               aes(label=NAME), size=2.5, family=f1,hjust=1.2) +
  geom_sf_text(data=places %>% filter(NAME=="Ottawa"),
               aes(label=NAME), size=2.5, family=f1,vjust=1.3)
               
# add lake white fish data
p1 + 
  geom_sf_text(data=gl2 %>% filter(name=="Saint Clair"),
               aes(label=glue::glue("Lake {name}")),nudge_x = -1.3,
               family=f1, size=2.75, color="black", fontface="bold") +
  new_scale_fill() +
  geom_sf(data=gl_fish, aes(fill=factor(n)), show.legend=F, color="black", size=.2) +
  geom_sf_text(data=gl_fish %>% filter(name!="Ontario", name!="Superior", name!="Michigan"), aes(label=glue::glue("Lake {name}\n{round(n)}")), size=3, color="white",family=f1, fontface="bold", lineheight=1) +
  geom_sf_text(data=gl_fish %>% filter(name=="Ontario"), aes(label=glue::glue("Lake {name}\n{round(n)}")), size=3, color="white",family=f1, fontface="bold", lineheight=1, nudge_y = -.2) + 
  geom_sf_text(data=gl_fish %>% filter(name=="Superior"), aes(label=glue::glue("Lake {name}\n{round(n)} thousand pounds")), size=3, color="white",family=f1, fontface="bold", lineheight=1, nudge_y=.1, nudge_x = .2) +
  geom_sf_text(data=gl_fish %>% filter(name=="Michigan"), aes(label=glue::glue("Lake\n{name}\n{round(n)}")), size=3, color="white",family=f1, fontface="bold", lineheight=1) + 
  #scale_fill_manual(values=c("#93caf6","#8eb5f0","#7364d2","#5829a7","#461177")) +
  scale_fill_manual(values=c("#242B47","#15607a","#606c38","#BF9926","#A35D08")) + 
  coord_sf(xlim=c(-92.10703,-75.77568),
           ylim=c(41.3981,49.00142)) +
  theme_void() +
  theme(plot.title=element_markdown(family=f1,
                                margin=margin(t=7, b=5),color="#2b2d42", size=10.5, hjust=.5),
        plot.caption = element_text(size=7, margin=margin(t=5,b=4), hjust=.5, family=f1),
        ) +
  labs(title="**THE GREAT LAKES : Lake Whitefish Production in 2015** (rounded to the nearest thousand pounds)",
       caption="#30DayMapChallenge Nov 2021 | Data: Great Lakes Fishery Commission and Natural Earth")

#save
ggsave("18_water.pdf", width=7, height=5.2, unit="in")