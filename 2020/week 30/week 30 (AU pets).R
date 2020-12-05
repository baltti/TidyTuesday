
library(tidyverse)
library(cowplot)
library(ggpubr)
library(ggalluvial)

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')

pets<-animal_outcomes %>% 
  group_by(animal_type,outcome,year) %>% 
  summarise(Total,.groups="keep") %>%  
  replace_na(list("Total"=0))


pets %>% 
  arrange(Total) %>% 
ggplot(aes(axis1 = outcome,axis2=animal_type,
           y = Total)) +
  scale_x_discrete(limits = c("animal", "outcome")) +
  scale_fill_manual(values = c("#ad9285","#a1a4aa","#dd493c",
                               "#366f2a","#96b8e9","#e29b36",
                               "#0f4c8a","#593fa6"),
                    name=NULL)+
  #xlab("Demographic") +
  stat_flow(aes(fill = outcome),curve_type = "cubic") +
  stat_stratum(aes(fill = outcome)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size=3) +
  theme_void() 
  
ggsave((paste0("pets-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 18, height = 9)
