

library(tidyverse)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')


avatar_stream<-avatar %>% 
  group_by(character,book, book_num,chapter_num) %>% 
  count()%>% filter(character=="Aang"|character=="Katara"|character=="Sokka"|
                      character== "Zuko"|character== "Toph Beifong"|
                      character=="Iroh"|character=="Azula") %>% 
  mutate(chapter_num=case_when(book=="Water" ~chapter_num,
                               book=="Fire" ~chapter_num+20,
                               book=="Earth" ~chapter_num+40)) 

library(ggstream)

ggplot(avatar_stream, aes(x=chapter_num, y=n, fill=character))+
  geom_stream(n_grid = 5000, bw=0.2, method = "loess")+
  geom_vline(data = tibble(x = c(21, seq(21, 60, by = 20), 41)),
             aes(xintercept = x),color="white")+
  geom_text(x=10, y=77, label="Book Water",family="Slayer")+
  geom_text(x=30, y=77, label="Book Fire",family="Slayer")+
  geom_text(x=50, y=77, label="Book Earth",family="Slayer")+
  tvthemes::scale_fill_avatar()+
  tvthemes::theme_avatar(title.font="Slayer",
                         legend.font = "Slayer",
                         title.size = 18)+
  ggtitle(label = "Number of lines spoken by main characters in Avatar: The Last Airbender")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank())+
ggsave((paste0("avatar-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 18, height = 9)
