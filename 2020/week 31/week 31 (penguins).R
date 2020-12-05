
library(tidyverse)
library(ggridges)

tuesdata <- tidytuesdayR::tt_load('2020-07-28')
tuesdata <- tidytuesdayR::tt_load(2020, week = 31)

penguins <- tuesdata$penguins %>% na.omit() 

penguins_raw.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')


p1<-ggplot(penguins, aes(x = bill_length_mm, y = species, fill = species)) +
  geom_density_ridges() +
  theme_void()+
  scale_fill_manual(values = c("#fc9d03","#9122f2","#13856e"),
                    name=NULL)+
  ggtitle(label = "Bill length")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave((paste0("penguins-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 9)


p2<-ggplot(penguins, aes(x = bill_depth_mm, y = species, fill = species)) +
  geom_density_ridges() +
  theme_void()+
  scale_fill_manual(values = c("#fc9d03","#9122f2","#13856e"),
                    name=NULL)+
  ggtitle(label = "Bill depth")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave((paste0("penguins-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 9)


p3<-ggplot(penguins, aes(x = flipper_length_mm, y = species, fill = species)) +
  geom_density_ridges() +
  theme_void()+
  scale_fill_manual(values = c("#fc9d03","#9122f2","#13856e"),
                    name=NULL)+
  ggtitle(label = "Flipper length")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave((paste0("penguins-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 9)

p4<-ggplot(penguins, aes(x = body_mass_g, y = species, fill = species)) +
  geom_density_ridges() +
  theme_void()+
  scale_fill_manual(values = c("#fc9d03","#9122f2","#13856e"),
                    name=NULL)+
  ggtitle(label = "Body mass")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave((paste0("penguins-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 9)

(p1+p2)/(p3+p4)
ggsave((paste0("penguins-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 18, height = 18)


p<-ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species,size=body_mass_g)) +
  geom_point() +
  scale_color_manual(values = c("#fc9d03","#9122f2","#13856e"),
                    name=NULL)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Bill length, mm", y="Bill depth, mm")
  
ggExtra::ggMarginal(p,type = "histogram",fill="#c6e2ec")
  

