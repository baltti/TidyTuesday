
library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(ggblur)

loadfonts()

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')
wind_turcanada<-ne_countries(country = "Canada",returnclass = "sf") #%>% 
  st_transform(crs=3978) #Albers equal area

turbines<-wind_turbine %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_transform(crs=3978)


ggplot()+
  geom_sf(data=canada,colour = "black", fill = '#061147')+
  geom_point_blur(data=wind_turbine,aes(x=longitude,y=latitude,blur_size=turbine_rated_capacity_k_w), 
                      size=1, colour="#f9d51f", alpha=0.6) +
  scale_blur_size_continuous(range = c(1, 20)) +
  ggtitle(label = "Canadian wind turbines")+
  theme_void()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 28, family = "Candara",face = "italic"))+
  ggsave((paste0("wind-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 12, height = 12)
