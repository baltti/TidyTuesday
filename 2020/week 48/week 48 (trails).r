library(tidyverse)
library(extrafont)
library(ggdist)
library(ggsci)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

df_trails_feat <-
  hike_data %>% 
  unnest(features) %>% 
  mutate(
    rating = as.numeric(rating),
    gain=as.numeric(gain),
    features = str_to_title(features),
    features = str_replace(features, " For ", " for "),
    features = str_replace(features, " On ", " on "),
    features = str_replace(features, "/", " & ")
  ) %>% 
  group_by(features) %>% 
  mutate(med = median(rating, na.rm = TRUE)) %>% 
  mutate(med_gain=median(gain, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(features = fct_reorder(features, med))

ggplot(df_trails_feat, 
       aes(rating, features, 
           color = med, 
           fill = med)) +
  stat_gradientinterval(inherit.aes = TRUE)+
  stat_summary(
    orientation = "y",
    fun = median,
    geom = "point", 
    size = 3,
    shape = 23,
    stroke = 1,
    color = "black"
  ) +
  scale_fill_material("brown", reverse = TRUE)+
  scale_color_material("brown", reverse = TRUE)+
  ggtitle(label="Washington trails", subtitle = "Trails grouped by feature and ordered by median rating")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 30, 
                                  family = "Walkway",face = "bold",
                                  colour = "black"),
        text = element_text(size = 15, 
                            family = "Walkway",face = "bold",
                            colour = "black"))+
  ggsave((paste0("hikes-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 500, width = 10, height = 10)
