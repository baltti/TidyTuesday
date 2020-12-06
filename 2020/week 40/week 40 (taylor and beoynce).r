
library(tidyverse)
library(tidytext)
library(reshape2)
library(syuzhet)
library(extrafont)
library(cowplot)


beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

taylor_tidy_lyrics <- taylor_swift_lyrics %>%
  unnest_tokens(word, Lyrics) %>% 
  anti_join(stop_words, by=c("word"="word")) 

taylor_posneg<- taylor_tidy_lyrics %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% rename(word=rowname)

taylor_tidy_lyrics<- taylor_tidy_lyrics %>% 
  inner_join(taylor_posneg) %>% 
  unique() %>% 
  group_by(Title) %>% 
  summarise_at(c("positive", "negative"), sum) %>% 
  inner_join(taylor_swift_lyrics) %>% 
  arrange(Album, Title)  


tsw<-ggplot(taylor_tidy_lyrics) + 
  geom_bar(aes(x=Title,y=positive),fill="#f4d757", 
           stat = "identity")+
  geom_bar(aes(x=Title,y=-negative),fill="#516181", stat = "identity")+
  #annotate("text", x = "This Love", y = 320, size=5, label = "This love")+
  #annotate("text", x = "Shake It Off", y = -300, size=5,label = "Shake It Off")+
  theme_void()+
  ggtitle(label = "Frequency of positive and negative words in Taylor Swift songs")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, family = "Ink Free"), 
        legend.position = "none")+
  ggsave((paste0("taylor_beyonce-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 10)

  
beyonce_tidy_lyrics <- beyonce_lyrics %>%
  unnest_tokens(word, line) %>% 
  anti_join(stop_words, by=c("word"="word")) 

beyonce_posneg<- beyonce_tidy_lyrics %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% rename(word=rowname)

beyonce_tidy_lyrics<- beyonce_tidy_lyrics %>% 
  inner_join(taylor_posneg) %>% 
  select(song_name,word, negative,positive)  %>% unique() %>% 
  group_by(song_name) %>% 
  summarise_at(c("positive", "negative"), sum)  
  
  

bee<-ggplot(beyonce_tidy_lyrics) + 
  geom_bar(aes(x=song_name,y=positive),fill="#f4d757", stat = "identity")+
  geom_bar(aes(x=song_name,y=-negative),fill="#516181", stat = "identity")+
  #annotate("text", x = "Love on Top (DJ Escape & Tony Coluccio Remix)", y = 13300, 
         #  label = "Most positive song - Love on Top (DJ Escape & Tony Coluccio Remix)",
         #  size=5)+
  #annotate("text", x = "Hold Up", y = 12400, size=4,
         #  label = "Second most positive song - Hold Up and its italian version")+
 # annotate("text", x = "Creole", y = -5400, size=5,
           #label = "Most negative song - Creole")+
  theme_void()+
  ggtitle(label = "Frequency of positive and negative words in Beyonce songs")+
  theme(plot.title = element_text(hjust = 0.5, size = 32, family = "Ink Free"))+
  ggsave((paste0("taylor_beyonce-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 10)


ggdraw() +
  draw_plot(tsw, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot(bee, x = 0, y = 0.5, width = 1, height = 0.5)+
  ggsave((paste0("taylor_beyonce-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 18, height = 20)
