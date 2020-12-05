
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 32)


country_totals<-tuesdata$country_totals

energy <- tuesdata$energy_types %>% 
  mutate(
    country_name = if_else(country == "EL", "Greece", country_name),
    country = if_else(country == "EL", "GR", country)
  ) %>% 
  filter(level == "Level 1") %>% 
  mutate(
    type_agg = if_else(
      type %in% c("Conventional thermal", "Nuclear"),
      "Non-renewable", "Renewable"
    )
  ) %>% 
  group_by(country, country_name, type_agg) %>% 
  ungroup() %>% 
  mutate(country_name = if_else(country == "GB", "United Kingdom", country_name)) %>% 
  rename(code=country,value=7) 

library(geofacet)
library(ggdark)
ggplot(energy, aes("", value, fill = type_agg)) +
  geom_col(alpha = 0.8, width = 1) +
  scale_fill_manual(values=c("#3b111f","#4b9b73"),name=NULL)+
  facet_geo(~ code, grid = "eu_grid1", scales = "free_y")+
  dark_theme_void() +
    ggtitle(label = "European energy production, 2018")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave((paste0("energy-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 18)


