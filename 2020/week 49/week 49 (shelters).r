
library(tidyverse)
library(extrafont)
library(ggforce)
library(tidygeocoder)
library(lubridate)
library(patchwork)
library(sf)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

adresses<-shelters %>% geocode(street = shelter_address, city=shelter_city, method="osm") %>% 
  drop_na(long, lat) %>% 
  sf::st_as_sf(coords=c("long","lat"))

adresses1<-st_as_sf(as.data.frame(st_coordinates(adresses)) %>% 
                      distinct(X,Y), coords=1:2, crs=st_crs(adresses))

adresses1<-adresses1 %>% 
  mutate(long=st_coordinates(adresses1)[,1],
         lat=st_coordinates(adresses1)[,2])

map_shelter<-ggplot()+
  basemapR::base_map(bbox=st_bbox(adresses1),basemap = "positron", increase_zoom = 2, nolabels = TRUE) +
  ggblur::geom_point_blur(data=adresses1,aes(x=long,y=lat,blur_size=30), 
                  alpha =0.8, size=1, colour="#f4e225") +
  theme_void()
  

sankey_df <- 
  shelters %>% 
  filter(year(occupancy_date) == 2019) %>% 
  mutate(
    organization_name = fct_lump(organization_name, n = 5),
  ) %>% 
  filter(organization_name != "Other") %>% 
  distinct(organization_name, program_name, sector) %>% 
  group_by(organization_name) %>% 
  count(sector) %>% 
  ungroup() %>% 
  mutate(
    organization_name = factor(organization_name, levels = c("Society of St.Vincent De Paul", "Fred Victor Centre", "Homes First Society", "The Salvation Army of Canada", "City of Toronto")),
    sector = factor(sector, levels = c("Co-ed", "Families", "Men", "Women", "Youth"))
  ) %>% 
  select(sector, organization_name, everything())

org_labels <- 
  sankey_df %>% 
  count(organization_name, wt = n, sort = T) %>% 
  mutate(
    cum_n = cumsum(n),
    offset = c(0, rep(4.15, 4) * 1:4),
    n_mid = n / 2,
    y = lag(cum_n, default = 0) + n_mid + offset,
    x = 2.02,
    label = organization_name)

sector_labels <- 
  sankey_df %>% 
  count(sector, wt = n, sort = T) %>% 
  arrange(rev(sector)) %>% 
  mutate(
    cum_n = cumsum(n),
    offset = c(0, rep(4.15, 3) * 1:3),
    n_mid = n / 2,
    y = lag(cum_n, default = 0) + n_mid + offset,
    x = 0.98,
    label = sector) 


plot_1 <- 
  gather_set_data(sankey_df, 1:2) %>% 
  mutate(x = factor(x, levels = c("sector", "organization_name")) %>% as.numeric()) %>% 
  ggplot(aes(x = x, id = id, split = y, value = n)) +
  geom_parallel_sets(aes(fill = sector), alpha = 0.6, axis.width = 0.04,n=50) +
  geom_parallel_sets_axes(axis.width = 0.01, fill = "grey50") +
  geom_text(
    data = org_labels,
    aes(
      x = x,
      y = y,
      label = label
    ),
    size = 3,
    family = "Ostrich Sans",
    fontface = "bold",
    color = "grey35",
    hjust = 0.5,
    vjust=0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = sector_labels,
    aes(
      x = x,
      y = y,
      label = label,
      color = sector
    ),
    size = 4,
    family = "Ostrich Sans",
    fontface = "bold",
    hjust = 1,
    vjust=1,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("#df6020","#328217","#1763cf","#8a426c","#fce883")) +
  scale_color_manual(values = c("#df6020","#328217","#1763cf","#8a426c","#fce883")) +
  coord_cartesian(clip = "off") +
  coord_flip()+
  theme_void() +
  ggtitle(label = "Toronto shelters")+
  theme(plot.title = element_text(hjust = 0.5,vjust=0.1,size = 30, 
                                  family = "Ostrich Sans",
                                  colour = "black"),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm")) 


cowplot::plot_grid(plot_1, map_shelter, nrow = 2)

ggsave((paste0("shelters-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 500, width = 10, height =10)
