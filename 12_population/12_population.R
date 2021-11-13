#30DayMapChallenge Day 11 Population
#Madagascar population density in 2020, aggregated to aggregated to 20km squares (colored by quantiles)
#Data from afrimapr team via {afrilearndata}

# load libraries
library(tidyverse)
library(afrilearndata)
library(sf)
library(raster)
library(ggtext)
library(gtools)

# import font
library(sysfonts)
library(showtext)
font_add_google("Lato") 
showtext_auto()

# get polygon of Madagascar  
ma_poly = africountries %>% filter(name_long=="Madagascar")

# mask raster
ma_ras = mask(afripop2020, ma_poly)

# data frame 
df = as.data.frame(ma_ras, xy=TRUE) %>% 
  drop_na() %>% 
  rename(pop = ppp_2020_1km_Aggregated)
range(df$pop)

# cut population into quantiles using {gtools}
df$pop_grp3 = quantcut(df$pop, c(0,0.1,0.25,0.5,0.75,0.9,0.999,1))

# create labels
df = df %>% 
  mutate(pop_lab = case_when(pop_grp3 == "[0.92,4.92]"~"0 - 0.1 quantile\n[0.92,4.92]",
                             pop_grp3 == "(4.92,8.91]"~"0.1 - 0.25 quantile\n(4.92,8.91]",
                             pop_grp3 == "(8.91,18.4]"~"0.25 - 0.5 quantile\n(8.91,18.4]",
                             pop_grp3 == "(18.4,37.3]"~"0.5 - 0.75 quantile\n(18.4,37.3]",
                             pop_grp3 == "(37.3,72.9]"~"0.75 - 0.9 quantile\n(37.3,72.9]",
                             pop_grp3 == "(72.9,1.38e+03]"~"0.9 - 0.999 quantile\n(72.9,1380]",
                             pop_grp3 == "(1.38e+03,4.1e+03]"~"0.999 - 1 quantile\n(1380,4100]"))
                             
# plot 
ggplot() + 
  geom_tile(data=df, aes(x=x, y=y, fill=pop_lab)) +
  scale_fill_viridis_d(option="cividis") +
  coord_fixed(expand=F, clip="off") +
  theme_void(base_family = "Lato") +
  theme(legend.text = element_text(size=8, color="white"),
        legend.key.width = unit(.2,"cm"),
        legend.key.height = unit(0.9,"cm"),
        legend.title=element_markdown(color="white", size=9.5, lineheight = 1.5),
        plot.margin=margin(.75,1,.75,1,unit="cm"),
        plot.background = element_rect(fill="#343a40", color=NA),
        plot.title.position="plot",
        legend.margin = margin(t=100, l=-10),
        plot.caption.position = "plot",
        plot.caption=element_text(color="white", hjust=1, size=7.7)
        ) +
  guides(fill=guide_legend(reverse=T)) +
  labs(fill="<span style = 'font-size:16pt;'>**MADAGASCAR**</span><br>*Population density in 2020,<br>aggregated to 20km squares*<br>",
       caption="#TidyTuesday #30DayMapChallenge | Data from afrimapr team via {afrilearndata}")
       
# save
ggsave("population.pdf",width=7, height=8.5, units="in")
ggsave("population.png",width=7, height=8.5, units="in")