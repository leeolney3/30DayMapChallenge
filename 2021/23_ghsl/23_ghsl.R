#30DayMapChallenge 23 Data Challenge 3: GHSL
#Belgium 1km resolution, population in 2015 and population difference 1975 and 2015

#Data source
#https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_MT_GLOBE_R2019A/GHS_POP_E2015_GLOBE_R2019A_54009_1K/V1-0/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.zip
#https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_MT_GLOBE_R2019A/GHS_POP_E1975_GLOBE_R2019A_54009_1K/V1-0/GHS_POP_E1975_GLOBE_R2019A_54009_1K_V1_0.zip

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(raster)
library(ggplot2)
library(ggtext)

# import raster
pop2015 = raster("data/GHSL/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.tif")
pop1975 = raster("data/GHSL/GHS_POP_E1975_GLOBE_R2019A_54009_1K_V1_0.tif")

# get BE shp files and transform 
be = ne_countries(scale=10,country = 'belgium', returnclass = "sf")
# transform 
be2 = st_transform(be, crs(pop2015))

# 2015 df
b15_crop = crop(pop2015, extent(be2))
b15_mask = mask(b15_crop, be2)

b15_df = as.data.frame(b15_mask, xy=TRUE)
colnames(b15_df)[3] <- "year_2015"
b15_df2 = b15_df %>% dplyr::filter(!is.na(year_2015))
range(b15_df2$year_2015)

# 1975 df
b75_crop = crop(pop1975, extent(be2))
b75_mask = mask(b75_crop, be2)

b75_df = as.data.frame(b75_mask, xy=TRUE)
colnames(b75_df)[3] <- "year_1975"
b75_df2 = b75_df %>% dplyr::filter(!is.na(year_1975))
range(b75_df2$year_1975)

# population difference (2015 minus 1975)
diff_df = dplyr::left_join(b15_df2,b75_df2, by=c("x"="x","y"="y")) %>% 
  dplyr::mutate(pop_diff = year_2015-year_1975)
summary(diff_df$pop_diff)

# plot 1: population in 2015
p1 = ggplot() +
  geom_raster(data=b15_df2, aes(x=x, y=y, fill=year_2015)) +
  colorspace::scale_fill_continuous_sequential(palette="rocket", trans="pseudo_log",
                                               breaks=c(0,200,20000), labels=scales::comma) +
  theme_void(base_size = 8) +
  coord_fixed() +
  theme(plot.background = element_rect(fill="#e5e5e5", color=NA), 
        legend.position="top",
        legend.justification = "center",
        plot.title=element_markdown(lineheight=1.3, size=9, hjust = .5),
        plot.caption=element_text(size=6, margin=margin(t=-5,b=10), hjust=.5),
        plot.margin=margin(.3,0,0,0, unit="cm"),
        legend.margin=margin(t=-5)
        ) +
  guides(fill = guide_colorbar(title.position = "top",
                                barwidth = unit(11, "lines"), 
                                barheight = unit(.4, "lines"))) +
  labs(fill="",
       title="**BELGIUM**<br>**Population in 2015** (1km Resolution)",
       caption="#30DayMapChallenge Nov 2021 | Data source: Global Human Settlement Layer")
p1

# plot2: population difference 1975 and 2015
p2 = ggplot() +
  geom_raster(data =diff_df, aes(x=x, y=y, fill=pop_diff)) +
  colorspace::scale_fill_continuous_diverging(palette="Broc", mid=0,trans="pseudo_log",
                                              breaks=c(-2616, -100, 0, 100,9752), rev=TRUE) +
  theme_void(base_size = 8) +
  coord_fixed() +
  theme(plot.background = element_rect(fill="#e5e5e5", color=NA), 
        legend.position="top",
        legend.justification = "center",
        plot.title=element_markdown(lineheight=1.3, size=9, hjust = .5),
        plot.caption=element_text(size=6, margin=margin(t=-5,b=10), hjust=.5),
        plot.margin=margin(.3,0,0,0, unit="cm"),
        legend.margin=margin(t=-5)
        ) +
  guides(fill = guide_colorbar(title.position = "top",
                                barwidth = unit(11, "lines"), 
                                barheight = unit(.4, "lines"))) +
  labs(fill="",
       title="**BELGIUM**<br>**Population difference 1975 and 2015** (1km Resolution)",
       caption="#30DayMapChallenge Nov 2021 | Data source: Global Human Settlement Layer")

p2

# combine plots
# library(patchwork)
# p1 + p2
