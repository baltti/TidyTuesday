
library(tidyverse)
library(emojifont)
library(extrafont)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

champions = tournament %>% 
  filter(tourney_finish == 'Champ'|tourney_finish == 'N2nd'|tourney_finish == 'NSF') %>% 
  group_by(school) %>%
  mutate(champ_wins =n()) %>% 
  mutate(color=case_when(tourney_finish == 'Champ'~"#fa7938",
            tourney_finish == 'N2nd'~"#103870",
            tourney_finish == 'NSF'~"grey"))

loadfonts(device = "win", quiet = TRUE)
ggplot(champions, aes(x = year, y = reorder(school, champ_wins))) +
  emojifont::geom_emoji(
    alias = 'basketball',
    x = champions$year,
    y = reorder(champions$school,champions$champ_wins),
    size = 22,color=champions$color, vjust = 0.1
  ) +
  labs(
    x = '', y = '',
    title = "NCAA Women's Basketball Champions and runners up", 
    subtitle = "winners for each year are orange, the runners up are blue, teams that lost the semifinals are grey"
  ) +
  scale_x_continuous(breaks = seq(1982, 2018, 2)) +
  theme_minimal() +
  theme(panel.grid = element_line(size = 0.35)
  )+
  ggsave((paste0("basketball-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 10, height = 10)
