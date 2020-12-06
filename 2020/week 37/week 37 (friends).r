
library(tidyverse)

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

entities<-friends::friends_entities

friends_emotions<- friends_emotions %>% 
  group_by(season, episode, utterance, emotion) %>% 
  count()

ggplot() + 
  geom_bar(data=friends_emotions, 
           aes(x=episode, fill=emotion), position = "fill")+
  scale_fill_manual(values = c("#fab938","#c70515","#999999","#3e743e","#5e3894","#57afdb","#ad9285"),
                    name=NULL)+
  facet_wrap(~season, 
             labeller = labeller(season = 
                                   c("1" = "Season One",
                                     "2" = "Season Two",
                                     "3" = "Season Three",
                                     "4" = "Season Four")))+
  theme_void()+
  ggtitle("The one with all the emotions", subtitle = "Percentage of every emotion by episode")+
  theme(plot.title = element_text(hjust = 0.5, size = 32, family = "Gabriel Weiss' Friends Font"),
        plot.subtitle = element_text(hjust = 0.5, size = 22, family = "Gabriel Weiss' Friends Font"),
        legend.text = element_text(size = 12, family = "Gabriel Weiss' Friends Font"),
        text = element_text(hjust = 0.5, size = 18, family = "Gabriel Weiss' Friends Font"))+
  ggsave((paste0("friends-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 18)
