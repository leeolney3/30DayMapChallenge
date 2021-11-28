#30DayMapChallenge 16 Urban/Rural
#2010 Percent Urban and Rural by County
#Data source: https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-urban-lists-record-layout.html

# load libaries 
library(tidyverse)
library(readxl)
library(maps)
library(mapdata)
library(rcartocolor)
library(ggtext)

# load fonts
library(sysfonts)
library(showtext)
font_add_google("Libre Franklin")
font_add_google("Roboto Condensed")
font_add_google("Roboto")
showtext_auto()

# import data
county = read_excel("data/PctUrbanRural_County.xls")

# prepare data
t1 = county %>% filter(STATENAME=="Texas") %>%
  mutate(st = str_to_lower(STATENAME),
         co = str_to_lower(COUNTYNAME)) %>%
  select(st, co, POPPCT_URBAN) %>%
  mutate(subregion = case_when(co=="dewitt" ~ "de witt", TRUE~ co))
  
# get TX counties polygon
counties <- map_data("county")
tx_county <- subset(counties, region=="texas")

# join
t2 = tx_county %>% left_join(t1, by="subregion")

# plot
ggplot() +
  geom_polygon(data=t2, mapping=aes(x=long, y=lat, group=group, fill=POPPCT_URBAN/100), color="white", size=.1) +
  coord_fixed(1.3, expand=F) +
  scale_fill_carto_c(palette="ag_Sunset", limits=c(0,1), labels=scales::percent) +
  theme_void() +
  theme(text=element_text(color="white"),
        legend.position = "top",
        legend.title=element_markdown(size=9, family="Roboto Condensed"),
        legend.text=element_text(size=8, family="Roboto Condensed"),
        legend.margin=margin(t=2, b=8),
        plot.caption=element_text(size=6.5, family="Roboto", hjust=.5, color="#f8f9fa"),
        plot.margin=margin(1,.5,1,.5, unit="cm"),
        plot.background=element_rect(fill="#495057", color=NA),
        plot.title=element_text(hjust=.5, family="Libre Franklin", face="bold", size=16,
                                margin=margin(b=8))) +
  guides(fill = guide_colorbar(title.position = "top", 
                                title.hjust = .5, 
                                barwidth = unit(21, "lines"), 
                                barheight = unit(.4, "lines"))) +
  labs(fill="Percentage of the total population of the county represented by the **urban population**", title="TEXAS", caption="\n#30DayMapChallenge Nov 2021 | 2010 Percent Urban and Rural by County | Data from census.gov")
  
# save
ggsave("16_urban.pdf", width=7, height=7, unit="in")

