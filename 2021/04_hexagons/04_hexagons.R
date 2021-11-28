#30DayMapChallenge 04 Hexagons
#Number of registered fire departments in the US 2021, data from U.S. Fire Administration.
#Data source: https://apps.usfa.fema.gov/registry/download (registry last updated Nov 1, 2021)

#load libraries 
library(tidyverse)
library(broom)
library(geojsonio)
library(rgdal)
library(rgeos)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Roboto")
font_add_google("Libre Franklin")
showtext_auto()

# import fire dept data
fire_dept = read_csv("data/usfa-registry-national.csv") %>% janitor::clean_names()

# hex bin data preparation reference: https://github.com/TamayoLeivaJ/TidyTuesday/blob/gh-pages/2021/2021_Week_041/2021_Week_041.R

# get geoJSON file
us_states <- geojson_read("https://raw.githubusercontent.com/TamayoLeivaJ/TidyTuesday/gh-pages/2021/2021_Week_041/Data/us_states_hexgrid.geojson",  what = "sp") 

# map data
us_states@data = us_states@data %>% 
                 # Remove the "(United States)" of all state names, then remove white space at the right
                 mutate(google_name = str_remove(google_name, pattern = "\\(United States\\)") %>% str_trim(side = "right")) 

# get centroid of each hexbin 
centers <- cbind.data.frame(data.frame(gCentroid(us_states, byid=TRUE), id = us_states@data$iso3166_2, State = us_states@data$google_name)) 

# transform "sp" object to "data frame" 
us_states_df <- tidy(us_states, region = "google_name") 

# plot data
hex_data = fire_dept %>% count(hq_state) %>%
  mutate(State = state.name[match(hq_state, state.abb)]) %>%
  mutate(State = case_when(hq_state=="DC" ~ "District of Columbia", TRUE~State)) %>%
  select(State, n, state_abb=hq_state) %>%
  right_join(us_states_df, by = c("State" = "id")) %>% 
  left_join(centers %>% select(id, State), by = c("State"))

# plot 
hex_data %>%
  ggplot() +
  geom_polygon(aes(fill = n, x = long, y = lat, group = group), color="white") +
  coord_map(clip="on") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", family="Libre Franklin", size=3.5) +
  theme_void(base_family = "Roboto", base_size = 10) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(.5, .8, .3, .7), "cm"),
        plot.background = element_rect(fill="#f0efeb", color=NA),
        plot.title=element_text(face="bold", hjust=.5, family="Libre Franklin"),
        plot.caption=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=.5),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        ) +
  scale_fill_stepsn("Number of registered fire departments",
                    colours = c("#BD2700","#D97900","#A7791C","#354055","#001964"), #nord::victory_bonds
                    limits = c(3, 1799), breaks = seq(3, 1799, length.out = 6),
                    labels=scales::comma_format(accuracy = 1)) +
   guides(fill = guide_colorsteps(barwidth = unit(18, "lines"), 
                                  barheight = unit(0.4, "lines"), 
                                  show.limits = TRUE, 
                                  frame.colour = "black", 
                                  frame.linewidth = unit(0.2, "lines"), 
                                  title.position = "top", 
                                  title.hjust = 0.5, 
                                  label.position = "bottom")) +
  labs(title="Registered Fire Departments in the U.S, 2021",
       subtitle="The state of Pennsylvania has the most registered fire departments (n=1799), followed by New York\n(n=1666) and Texas (n=1514). District of Columbia has the least registered fire departments (n=3).",
       caption="\nData source: U.S. Fire Administration (registry last updated: Nov 1, 2021)")
       
