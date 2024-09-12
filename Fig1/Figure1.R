library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(cowplot)
library(viridis)
library(magick)
library(pdftools)
library(grImport2)
library(here)

# data wrangling/plotting
library(tidyverse)
library(janitor)

# stats
library(broom)

# other plotting
library(cowplot)
library(ggtext)
library(ggbeeswarm)
library(ggrepel)

# misc
library(conflicted)
library(here)

conflict_prefer("filter", "dplyr")

#set wd

setwd<-here() 

# read in survey data
survey_b<-readRDS('../Data/survey_b.RDS')
survey_i<-readRDS('../Data/survey_i.RDS')
survey<-readRDS('../Data/survey.RDS')

#set theme for plotting
aesthetic <- 
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),  axis.line = ggplot2::element_line(size = 0.25, colour = "black"),
    legend.text = element_text(size = 10),
    axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1,face='plain', size=8),
    axis.text.y = element_text(face = "plain", size = 8),
    strip.text.y = element_text(size=12, face='plain'),        
    axis.title.y = element_text(size = 12, angle=90,face = "plain"),
    panel.border = element_blank(),
    strip.text = ggplot2::element_text(face = "plain", size = 10))

##########################
#maps for Benin, Fig 1 A
#########################

let_bb <- c(
  left = 1.88,
  bottom = 6.2,
  right = 2,
  top = 6.5
)


#countries <- getbb("Come Benin")%>%
countries <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("country")) %>%
  osmdata_sf()

cities <-getbb("Benin")%>%
#cities <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("city")) %>%
  osmdata_sf()

#towns <- getbb("Come Benin")%>%
towns <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("town")) %>%
  osmdata_sf()

#villages <- getbb("Come Benin")%>%
villages <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("village")) %>%
  osmdata_sf()

#streets <- getbb("Come Benin")%>%
streets <- let_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

#small_streets <- getbb("Come Benin")%>%
small_streets <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

#river <- getbb("Come Benin")%>%
river <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

#lakes <- getbb("Come Benin")%>%
lakes <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

borders2 <- getbb("Benin")%>%
#borders <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "admin_level", 
                  value = "2") %>%
  osmdata_sf()

#border outlines (inset)
map_border<-survey_b%>%
  ggplot() +
  geom_sf(data = borders2$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 1,
          alpha = 1)+
  coord_sf(
    xlim = c(-2,6), #lat_bounds,
    ylim = c(5.5,15), #lon_bounds,
    expand = TRUE) +
  geom_point(data=survey_b,aes(x=lon, y=lat),color= "white", fill= "#29103e", size=1.8, pch=21)+
guides(alpha=FALSE)+
  labs(fill="", size= "")+
  aesthetic+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 65, hjust = 1,face='plain'),
    legend.position = '',
    plot.background = element_rect(fill = "#FFFFFF"),
    axis.title = element_blank()
  ) +
  geom_rect(
    xmin = 1.6,
    ymin = 6.1,
    xmax = 2.2,
    ymax = 6.7,
    fill = NA, 
    colour = "#800000",
    size = 1
  )+
  NULL
map_border

#save_plot('../figures/Fig1A2.pdf', map_border, base_height = 3, base_width = 3)

# Benin map with sample site locations
borders <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "admin_level", 
                  value = "2") %>%
  osmdata_sf()
#map
map <- survey_b%>%
  ggplot() +
  geom_sf(data = lakes$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "#0fb0f5",
          color = "#0fb0f5",
          size = 0.51,
          alpha = .5) +
  geom_sf(data = river$osm_lines,
          #inherit.aes = FALSE,
          fill = "#0fb0f5",
          color = "#0fb0f5",
          size = 0.15,
          alpha = .5) +
  annotate("text", x=1.85, y=6.45, label = "Comè", size = 4, fontface=2) +
   coord_sf(
     xlim = c(1.8,2), #lat_bounds,
     ylim = c(6.3,6.6), #lon_bounds,
     expand = TRUE) +
  geom_point(data=survey_b,aes(x=lon, y=lat), color= "white", fill= "#29103e", size=1.8, pch=21)+
  guides(alpha=FALSE)+
  labs(fill="", size= "")+
  aesthetic+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 65, hjust = 1, face='bold'),
    legend.position = '',
    plot.background = element_rect(fill = "#FFFFFF"),
    axis.title = element_blank()
  ) +
  
  NULL
map

#save_plot('../figures/Fig1A.pdf', map, base_height = 6, base_width = 6)

#########################
#maps for India, Fig 1 B 
#########################

let_bb <- c(
  left = 78,
  bottom = 12,
  right = 80,
  top = 13
)

borders <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "admin_level", 
                  value = "2") %>%
  osmdata_sf()

#countries <- getbb("Come Benin")%>%
countries <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("country")) %>%
  osmdata_sf()

#cities <-getbb("Come Benin")%>%
cities <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("city")) %>%
  osmdata_sf()

#towns <- getbb("Come Benin")%>%
towns <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("town")) %>%
  osmdata_sf()

#villages <- getbb("Come Benin")%>%
villages <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "place", 
                  value = c("village")) %>%
  osmdata_sf()

#streets <- getbb("Come Benin")%>%
streets <- let_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

#small_streets <- getbb("Come Benin")%>%
small_streets <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

#river <- getbb("Come Benin")%>%
river <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

#lakes <- getbb("Come Benin")%>%
lakes <- let_bb%>%
  opq()%>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

borders2 <- getbb("India")%>%
  #borders <- let_bb %>%
  opq()%>%
  add_osm_feature(key = "admin_level", 
                  value = "2") %>%
  osmdata_sf()

#map borders (inset)
map_border_i<-survey_i%>%
  ggplot() +
  geom_sf(data = borders2$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 1,
          alpha = 1)+
  coord_sf(
    xlim = c(65,95), #lat_bounds,
    ylim = c(5,30), #lon_bounds,
    expand = TRUE) +
  geom_point(data=survey_i,aes(x=lon, y=lat),color= "white",fill='#F37413', size=1.8, pch=21)+
guides(alpha=FALSE)+
  labs(fill="", size= "")+
  aesthetic+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 65, hjust = 1,face='plain'),
    legend.position = '',
    plot.background = element_rect(fill = "#FFFFFF"),
    axis.title = element_blank()
  ) +
  geom_rect(
    xmin = 78,
    ymin = 11,
    xmax = 80,
    ymax = 14.5,
    fill = NA, 
    colour = "#800000",
    size = 1
  )+
  
  NULL
map_border_i

#save_plot('../figures/Fig1B2.pdf', map_border_i, base_height = 3, base_width = 3)

# India map with sample site locations
map_i <- survey_i%>%
  ggplot() +
  geom_sf(data = lakes$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "#0fb0f5",
          color = "#0fb0f5",
          size = 0.51,
          alpha = .5) +
  geom_sf(data = river$osm_lines,
          #inherit.aes = FALSE,
          fill = "#0fb0f5",
          color = "#0fb0f5",
          size = 0.15,
          alpha = .5) +
  annotate("text", x=79.15, y=12.85, label = "Vellore", size = 4, fontface=2) +
  coord_sf(
    xlim = c(78.9,79.5), #lat_bounds,
    ylim = c(12.5,12.9), #lon_bounds,
    expand = TRUE) +
  geom_point(data=survey_i,aes(x=lon, y=lat), color= "white",fill='#F37413', size=1.8, pch=21)+
  guides(alpha=FALSE)+
  labs(fill="", size= "")+
  aesthetic+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 65, hjust = 1, face='bold'),
    legend.position = '',
    plot.background = element_rect(fill = "#FFFFFF"),
    axis.title = element_blank()
  ) +
  
  NULL
map_i
#save_plot('../figures/Fig1B.pdf', map_i, base_height = 6, base_width = 6)


#--------------------------------------------figure plot
#load in scematics of methods 
illust.fig1A <- image_read_pdf("../Figures/Fig1A.pdf", density = 600)
illust.fig1B <- image_read_pdf("../Figures/Fig1B.pdf", density = 600)
illust.fig1C <- image_read_pdf("../Figures/Schematics/Soil sampling method.pdf", density = 600)
illust.fig1D <- image_read_pdf("../Figures/Schematics/Wastewater sampling methods.pdf", density = 600)

Fig1A <- ggdraw() +
  draw_image(illust.fig1A, scale = 1)
Fig1B <- ggdraw() +
  draw_image(illust.fig1B, scale = 1)
Fig1C <- ggdraw() +
  draw_image(illust.fig1C, scale = 1)
Fig1D <- ggdraw() +
  draw_image(illust.fig1D, scale = 1)


left<- plot_grid(Fig1A,Fig1B, labels = c('A','B'), axis = 'l',ncol=1)
left 

right<- plot_grid(Fig1C,Fig1D,labels = c('C','D'), ncol=1, align = 'h',axis = 'r')
right

fig1<-plot_grid(left,right, ncol = 2, rel_widths= c(.8,1),rel_heights = c(1,1))
fig1

save_plot("../Figures/fig1.pdf", fig1, base_height = 8, base_width =8)
