
library(tidyverse)
library(extrafont)
library(treemapify)

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

ikea<-ikea %>% 
  mutate(price=price*0.27) %>% 
  group_by(category) %>% count()


ggplot(ikea,aes(area = n,label=category,subgroup=category))+
  geom_treemap(fill = "#0051BA")+
  geom_treemap_text(family="Verdana", colour = "#FFDA1A", place = "centre",
                    reflow = TRUE,size=25)+
  geom_treemap_subgroup_border(colour = "#FFDA1A",lwd=5) +
  ggtitle(label = "IKEA furniture categories")+
  theme_void()+
  theme(plot.background = element_rect(color = NA, fill = "#FFDA1A"),
        legend.position = "none",plot.margin = margin(1, 2, 2, 2, "cm"),
        plot.title = element_text(hjust = 0.5,vjust=0,size = 50,margin =margin(0, 1, 1, 1, "cm"), 
                                  family = "Verdana",face = "bold",colour = "#0051BA"))+
  ggsave((paste0("ikea-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 12, height = 12)
