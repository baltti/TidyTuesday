
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)

plants <- tuesdata$plants

data<-plants %>% group_by(continent, group) %>% count()


data %>% filter(group=="Flowering Plant") %>% 
  arrange (n) %>% 
  mutate(continent=factor(continent, levels=continent)) %>%
  ggplot(aes(x=continent, y=n, fill=continent)) +       
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#666666","#f5b83d","#1474b8","#2e6b5c","#981b1b","#589b4b"),
                    aesthetics = "fill", name=NULL)+
  ylim(-20,206) +
  coord_polar(start = 0)+
  theme_void(base_size = 18)+
  ggtitle(label="Flowering plants in danger")+
  theme(legend.position = c(0.4, 0.2),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5,vjust = 0.5))+
ggsave((paste0("plants-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 9, height = 9)


data %>%  
  arrange (n) %>% 
  ggplot(aes(x=group, y=n, fill=continent)) +       
  geom_bar(position="fill",stat="identity") +
  scale_fill_manual(values = c("#666666","#f5b83d","#1474b8","#2e6b5c","#981b1b","#589b4b"),
                    aesthetics = "fill", name=NULL)+
  coord_polar(start = 0)+
  theme_minimal(base_size = 10)+
  ggtitle(label="Plants in danger")+
  xlab(label=NULL)+ylab(label = NULL)+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,vjust = 0.5))+
  ggsave((paste0("plants-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 9, height = 9)
