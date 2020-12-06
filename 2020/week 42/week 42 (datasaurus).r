
library(tidyverse)
library(ggforce)
library(patchwork)
library(extrafont)


loadfonts()

datasaurus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')


force<-ggplot(datasaurus, aes(x=x, y=y, colour=dataset))+
  geom_point()+
  geom_mark_hull(aes(fill=dataset),expand = unit(4, "mm")) +
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=1)

boxplotx<-ggplot(datasaurus, aes(x=x, colour=dataset))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, family = "ScriptC"))+
  ggtitle(label = "Distribution by x axis")+
  facet_wrap(~dataset, ncol=1)

boxploty<-ggplot(datasaurus, aes(y=y, colour=dataset))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, family = "ScriptC"))+
  ggtitle(label = "Distribution by y axis")+
  facet_wrap(~dataset, ncol=1)

allplot<-force+boxplotx+boxploty+
  plot_annotation(title = "Datasaurus dozen",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 36, family = "ScriptC",
                                                          face = "bold")))+
  ggsave((paste0("datasaurus-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 12, height = 30)



