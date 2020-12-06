
library(tidyverse)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

library(extrafont)
ggplot(kids %>% filter(variable=="highered"&state!="District of Columbia"))+
  geom_tile(aes(x=year, y=state, fill=inf_adj_perchild))+
  scale_fill_gradientn(colours = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"), 
                       na.value = "white",aesthetics = "fill", 
                       guide = "colourbar", name=NULL)+
  theme_minimal()+
  ggtitle("State spending on high education", subtitle = "Adjusted for inflation, per child")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, family = "Romantic"),
        plot.subtitle = element_text(hjust = 0.5, size = 22, family = "Romantic"),
        text = element_text(hjust = 0.5, size = 24, family = "Romantic"),
        axis.title = element_blank())+
ggsave((paste0("kids-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 18, height = 18)
