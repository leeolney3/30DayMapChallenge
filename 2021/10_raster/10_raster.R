#30DayMapChallenge 10 Raster
#Namibia Annual Precipitation
#Data: WorldClim by way of {raster} R package

# load libaries 
library(tidyverse)
library(raster)
library(sf)
library(ggtext)

# add font
library(sysfonts)
library(showtext)
font_add_google("Roboto Mono")
showtext_auto()

# get climate data {raster}
climate=getData('worldclim', var='bio',res=2.5)

# get country shape 
sf<- rnaturalearth::ne_countries(country = 'Namibia', returnclass = "sf")
#crs(climate)
#crs(sf)
# transform crs
sf2 = st_transform(sf,crs(climate))

# crop 
climate2 <- crop(climate,extent(10.864,27.871,-29.654,-16.008))
#plot(climate2)

# annual precipitation df
raster <- climate2$bio12 #get raster
m = mask(raster, sf2) #mask

df_prec <- as.data.frame(m,xy=TRUE)%>%drop_na()
summary(df_prec$bio12)

# plot
ggplot()+
  geom_raster(aes(x=x,y=y,fill=bio12),data=df_prec)+
  geom_sf(fill='transparent',data=sf2, color="transparent")+
  #rcartocolor::scale_fill_carto_c(palette="ag_GrnYl") +
  colorspace::scale_fill_continuous_sequential(palette="PuBuGn", rev=F,
                                               limits=c(8,698),
                                               breaks= c(8,200,400,600,698)) +
  coord_sf(expand=c(0,0))+
  labs(x='Longitude',y='Latitude',
       fill="<span style='font-size:14pt'><b>NAMIBIA</b></span'><br>Annual precipitation (mm)",
       caption='Data: WorldClim (2020) by way of {raster} R package') +
  theme_void(base_size = 9, base_family = "Roboto mono") +
  theme(plot.background = element_rect(fill="#6c757d", color=NA),
        plot.margin=margin(.5,.5,.3,.5, unit="cm"),
        legend.position = "top",
        legend.title=element_markdown(lineheight = 2),
        text=element_text(color="white")) +
  guides(fill = guide_colorbar(title.position = "top", 
                                title.hjust = .5, 
                                barwidth = unit(17, "lines"), 
                                barheight = unit(.3, "lines")))
                                
# save
ggsave(file="10_raster.pdf", width=5.2, height=6,units="in")
ggsave(file="10_raster.png", width=5.2, height=6,units="in")
