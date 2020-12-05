
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)
astronauts <- tuesdata$astronauts

library(tidyverse)

astr<-astronauts %>% 
  group_by(nationality) %>% 
  summarise(across(where(is.numeric),mean)) 

library(ggdark)

total<-astr %>% 
  arrange(total_hrs_sum) %>% 
  mutate(nationality=factor(nationality, levels=nationality)) %>%
ggplot(aes(x=nationality,y=total_hrs_sum)) +
  geom_segment(aes(x=nationality, xend=nationality, y=0, yend=total_hrs_sum), color="#ec8013",alpha=0.5) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=8,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=7,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=6,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=5,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=4,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=3,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#ec8013", size=1,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=total_hrs_sum), color="#f4c025", size=1,pch=16) +
  #coord_flip()+
  coord_polar(theta="y")+
  ylim(0,7600)+
  dark_theme_void()+
  theme(legend.position = "right")+
  labs(title="Average duration of all missions",
       x="",
       y="")
  
ggsave((paste0("astronauts-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 9, height = 9)

mission<-astr %>% 
  arrange(hours_mission) %>% 
  mutate(nationality=factor(nationality, levels=nationality)) %>%
  ggplot(aes(x=nationality,y=hours_mission))+
  geom_segment(aes(x=nationality, xend=nationality, y=0, yend=hours_mission), color="#ec8013",alpha=0.5) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=8,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=7,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=6,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=5,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=4,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=3,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#ec8013", size=1,alpha=0.1,pch=16) +
  geom_point(aes(x=nationality, y=hours_mission), color="#f4c025", size=1,pch=16) +
  coord_flip()+
  dark_theme_minimal()+
  theme(legend.position = "right")+
  labs(title="Average duration of one mission",
       x="",
       y="")

library(gridExtra)
grid.arrange(total, mission, ncol=2)


by_sex<-astronauts %>% 
  group_by(sex,nationality) %>% 
  count()

male<-by_sex %>% filter(sex=="male")
female<-by_sex %>% filter(sex=="female")

plotcolors<-c("#a11212","#167b9c" )

sex<-ggplot(by_sex, aes(fill=sex, y=n, x=nationality)) + 
  geom_bar(position="fill", stat="identity",alpha=0.5)+
  scale_fill_manual(values = c("#e63c19","#167b9c"),name=NULL)+
  coord_flip()+
  dark_theme_minimal()+
  labs(title="Percent of male and female astronauts",
       x="",
       y="")

by_military<-astronauts %>% 
  group_by(military_civilian,nationality) %>% 
  count()

military<-ggplot(by_military, aes(fill=military_civilian, y=n, x=nationality)) + 
  geom_bar(position="fill", stat="identity",alpha=0.5)+
  scale_fill_manual(values = c("#708bc2","#628470"),name=NULL)+
  coord_flip()+
  dark_theme_minimal()+
  labs(title="Percent of military and civilian astronauts",
       x="",
       y="")

grid.arrange(total, mission,arrangeGrob(sex,military,nrow=2), ncol=3)
