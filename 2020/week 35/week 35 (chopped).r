
library(tidyverse)
library(cowplot)
library(extrafont)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

chopped_avg <-
  chopped %>% drop_na(episode_rating) %>% 
  arrange(season, series_episode) %>% 
  mutate(episode_id = row_number()) %>% 
  group_by(season) %>% 
  mutate(
    avg = mean(episode_rating),
    episode_mod = episode_id + (9 * season),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(season = factor(season))
df_lines <-
  chopped_avg %>% 
  group_by(season) %>% 
  summarize(
    start_x = min(episode_mod) - 5,
    end_x = max(episode_mod) + 5,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )





chopped_avg %>% 
  ggplot(aes(episode_mod, episode_rating)) +
  geom_hline(data = tibble(y = 7:10),
             aes(yintercept = y),
             color = "#f4c025",
             size = 0.5) +
  geom_line(data = df_lines,
            aes(x, y),
            color = "#f4c025") +
  geom_line(data = df_lines,
            aes(x_group, y, 
                color = season, 
                color = after_scale(colorspace::darken(color, .2))),
            size = 2.5) +
  scale_x_continuous(expand = c(.015, .015)) +
  scale_y_continuous(expand = c(.03, .03),
                     limits = c(7, 9.5),
                     breaks = seq(7, 9.5, by = .5),
                     sec.axis = dup_axis(name = NULL)) +
  scico::scale_color_scico_d(palette = "lajolla", guide = F)+
  labs(x = NULL, y = "IMDb Rating",
       title = "Average rating of Chopped by season") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#f7edba"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 16, face = "bold", colour = "#b34019"))+
ggsave((paste0("chopped-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 300, width = 18, height = 9)
