library(tidyverse)
library(ggwaffle)
library(ggstream)
library(cowplot)


key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')

crop_yields_ru<-key_crop_yields %>% filter(Entity=="Russia") %>% 
  pivot_longer(cols=4:last_col(),names_to="crop",values_to="yield") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  drop_na(yield)
crop_yields_ussr<-key_crop_yields %>% filter(Entity=="USSR")%>% 
  pivot_longer(cols=4:last_col(),names_to="crop",values_to="yield") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  drop_na(yield)


ru<-ggplot(crop_yields_ru, aes(x=Year, y=yield, fill=crop))+
  geom_stream(method = "raw")+
  scale_fill_manual(values = c("#f5e0a3","#783a4f","#247d8f","#472106","#38947d",
                               "#c7c9d1","#7e98ce","#984f1b"),
                    guide=F) + 
  scale_x_continuous(limits = c(1992, 2018),
                     breaks = seq(1992, 2018, by = 5))+
  labs(title = "Crop yields in Russia (1992-2018)")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))
ggsave((paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 18, height = 9)


ussr<-ggplot(crop_yields_ussr, aes(x=Year, y=yield, fill=crop))+
  geom_stream(method = "raw")+
  scale_fill_manual(values = c("#f5e0a3","#783a4f","#247d8f","#472106","#38947d",
                               "#c7c9d1","#7e98ce","#984f1b")) + 
  scale_x_continuous(limits = c(1961, 1991),
                     breaks = seq(1961, 1991, by = 5))+
  labs(title = "Crop yields in USSR (1961-1991)")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.position="bottom",
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.key.size = unit(0.4,"inches"))+
  ggsave((paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 9)

  
ggdraw() +
  draw_plot(ussr, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(ru, x = 0.5, y = 0.12, width = 0.5, height = 0.88)+
  ggsave((paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 9)



library(geofacet)
ggplot(key_crop_yields %>% pivot_longer(cols=4:last_col(),names_to="crop",values_to="yield") %>% 
         filter(Year==2018) %>% 
         mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")),
       aes("", yield, fill = crop)) +
  geom_col(alpha = 0.8, width = 1) +
  scale_fill_manual(values = c("#f5e0a3","#783a4f","#247d8f","#472106","#38947d",
                               "#c7c9d1","#7e98ce","#984f1b","#0a3776","#ec8013",
                               "#704785"),name=NULL)+
  facet_geo(~ Entity, grid = "eu_grid1", scales = "free_y",move_axes = TRUE)+
  labs(title = "Crop structure in Europe by country, 2018")+
    theme_void()+
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank(),
          legend.box = "horizontal",
          legend.position="bottom",
          legend.justification = "center",
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.key.size = unit(0.4,"inches"))+
    ggsave((paste0("crops-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                   ".png")), device = "png",dpi = 300, width = 18, height = 18)
