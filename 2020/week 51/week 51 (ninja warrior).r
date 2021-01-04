
library(tidyverse)
library(ggtext)
library(cowplot)

ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

round_stages_list <- c(
  "Qualifying",
  "Semi-Finals/\nRegional Finals",
  "National Finals\nStage 1",
  "National Finals\nStage 2",
  "National Finals\nStage 3",
  "National Finals\nStage 4"
)

obstacles <- ninja_warrior %>% 
  distinct(season, round_stage, obstacle_name, obstacle_order) %>% 
  add_count(obstacle_name) %>% 
  mutate(
    round_stage = case_when(
      str_detect(round_stage, "Qualifying") ~ "Qualifying",
      str_detect(round_stage, "Semi") ~ "Semi-Finals/\nRegional Finals",
      str_detect(round_stage, "Finals \\(Regional") ~ "Semi-Finals/\nRegional Finals",
      str_detect(round_stage, "National Finals") ~ str_replace(round_stage, " - ", "\n"),
      TRUE ~ round_stage
    ),
    round_stage = fct_relevel(round_stage, round_stages_list)
  ) %>% 
  mutate(color = case_when(
    str_detect(obstacle_name, "Steps") ~ "#D8282B",
    str_detect(obstacle_name, "Ladder") ~ "#284C88",
    str_detect(obstacle_name, "Climb") ~ "#4dcb62",
    TRUE ~ "grey85"
  ) 
  )

ggplot() +
  # gray lines and points
  geom_path(data = subset(obstacles), 
            aes(obstacle_order, season, group = obstacle_name,color=color), size = 1.2, lineend = "round") +
  geom_point(data = subset(obstacles, color == "grey85"), 
             aes(obstacle_order, season, color = color), size = 2.5) +
  geom_path(data = subset(obstacles, color != "grey85"), aes(obstacle_order, season, group = obstacle_name, color = color), size = 1.2, lineend = "round") +
  geom_point(data = subset(obstacles, color != "grey85"), aes(obstacle_order, season, color = color), size = 2.5) +
  # scales, etc
  scale_y_continuous(breaks = 1:10, sec.axis = dup_axis()) +
  scale_x_reverse(breaks = 1:10) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  coord_flip()+
  facet_wrap(vars(round_stage), nrow =6) +
  # title, etc
  labs(
    title = "<span style = 'color:#D8282B;'>Steps</span><span>, </span><span style = 'color:#284C88;'>Ladders</span> and </span><span style = 'color:#4dcb62;'>Climbs</span>",
    subtitle = "in American Ninja Warrior",
    caption = "Source:Data.World/Sasukepdia",
    x = "Obstacle Order in Round",
    y = "Season"
  ) +
  # theme
  theme_minimal(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "grey96", color = NA),
    panel.spacing.x = unit(0.9, "lines"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9, family = "JetBrains Mono Bold", hjust = 0),
    plot.title = element_textbox_simple(size = 30, family = "Diamante Bold", hjust = 0.552, color = "#4B5259", width = unit(6, "in")),
    plot.subtitle = element_text(size = 28, family = "Diamante Bold", hjust = 0.5, color = "#4B5259", margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(size = 8, family = "JetBrains Mono", hjust = 1, color = "grey30", margin = margin(-5, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 9, hjust = 0),
    axis.title.y.left = element_text(margin = margin(0, 15, 0, 0), size = 9, hjust = 1),
    axis.title.y.right = element_text(margin = margin(0, 0, 0, 15), size = 9, hjust = 0),
    plot.margin = margin(30, 20, 20, 20)
  ) +
  ggsave((paste0("ninja-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 500, width = 10, height =12)
