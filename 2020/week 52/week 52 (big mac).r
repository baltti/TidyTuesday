library(tidyverse)
library(lubridate)
library(patchwork)
library(extrafont)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') %>% 
  mutate(year=format(date, format = "%Y")) %>% 
  mutate(year=as.numeric(year))

post_soviet <- big_mac %>% filter(name %in% c("Russia", "Ukraine", "Czech Republic","Hungary","Poland"))
  
rect <- tibble(x1 = c(-Inf, max(post_soviet$year)), x2 = c(1, Inf), y1 = -Inf, y2 = Inf)

g <- post_soviet %>% 
  ggplot(aes(x = year, y = .5, fill = dollar_price)) +
  geom_tile(color = "transparent") +
  geom_step(aes(x = year + .5, y = dollar_price), color = "grey20", size = 3) +
  geom_step(aes(x = year + .5, y = dollar_price), color = "white", size = 1.5) +
  scale_fill_gradient(
    low = "#1763cf", high = "#f9311f",
    na.value = "transparent"
  )+
  scale_x_continuous(limits = c(2000, 2020)) +
  scale_y_continuous(limits = c(0, max(post_soviet$dollar_price))) +
  ggtitle(label = "The Big Mac index")+
  facet_wrap(facets = vars(name),nrow = 5,ncol = 1)+
  theme_void() +
  theme(plot.title = element_text(size = 30, 
                                  family = "BigNoodleTitling",
                                  colour = "grey20"),
        text = element_text(hjust = 0.5,vjust=-0.5,size = 20, 
                            family = "BigNoodleTitling",colour = "grey40"),
        legend.position = "none",
        axis.text.x = element_text(color = "grey60", size = 10, margin = margin(t = 10)))+
  ggsave((paste0("big-mac-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 500, width = 8, height =10)
  
 
