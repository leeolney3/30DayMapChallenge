#30DayMapChallenge 21 Elevation
#Glacier National Park

library(rayshader) 
library(rayvista)

glacier <- plot_3d_vista(lat=48.7446731, long=-113.8591203, zscale=5, zoom=0.5,
                          overlay_detail=14, theta=-65, windowsize =1200, 
                          phi=25)

render_highquality(lightdirection = 220, clear=TRUE)