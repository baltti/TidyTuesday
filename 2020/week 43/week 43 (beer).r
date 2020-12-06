
library(tidyverse)
library(extrafont)
library(digest)
library(gt)

loadfonts()

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

gold<-beer_awards %>% 
  filter(medal=="Gold") %>% group_by(state) %>% 
  count(name="gold") %>% 
  left_join(data.frame(state = datasets::state.abb,
                       name = datasets::state.name)) 

gold %>% 
  ggplot(aes(x = 0.975, y = gold, size = gold)) +
  scale_size(range = c(5,15)) +
  geom_polygon(inherit.aes = F, aes(x = x, y = n),
               data = data.frame(x = c(.9, .9, .7, .7, 1.3, 1.3, 1.2, 1.2),
                                 n = c(-10, 100, 210, 340, 340, 210, 100, -10)),
               fill = "#171219", color = NA, alpha = .5) +
  geom_polygon(inherit.aes = F, aes(x = x, y = n),
               data = data.frame(x = c(.8, .8, .7, .7, 1.25, 1.25, 1.15, 1.15),
                                 n = c(-5, 90, 200, 325, 325, 200, 90, -5)),
                                fill = "#9c5916", color = NA) +
  geom_polygon(inherit.aes = F, aes(x=x,y=n),
               data = data.frame(x = c(.7, .7, 1.25, 1.25),
                                 n = c(325, 350, 350, 325)),
               fill = "white", color = NA) +
  geom_hline(yintercept = c(0,100,200,300), lty = 2, color = "#171219") +
  geom_point(shape = 21, position = position_jitter(seed = 28, width = .17),
             color = "#C96E12", fill = "#EC9D00")+
  geom_text(
    aes(label = if_else(gold >= 100, name, NULL), size = gold/50),
    hjust = .5,
    vjust = .95, 
    position = position_jitter(seed = 28, width = .17)
  ) +
  ggtitle(label = "Great American beer festival",
          subtitle = "Number of gold medals by state")+
  theme_void()+
  theme(legend.position = "none",
        plot.title = element_text(size = 28, family = "CityBlueprint",face = "bold"),
        text = element_text(size = 22, family = "CityBlueprint"),
        plot.background = element_rect(color = NA, fill = "#7aa3b8"),
        axis.text.y = element_text(color = "white")) +
  scale_y_continuous(breaks = seq(0,300,100))+
  ggsave((paste0("beer-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 9, height = 12)


