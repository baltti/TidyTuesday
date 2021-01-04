
library(tidyverse)
library(gt)
library(extrafont)

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

w_table<-women %>% 
  group_by(category) %>% 
  relocate(img, before=name) %>% 
  gt(rowname_col = "Category") %>% 
  text_transform(
    locations = cells_body(columns=vars(img)),
    fn = function(x){
      web_image(url = x, height = px(100))
    }
  )%>%
  cols_label(
    `img` = "",
    `name` = "Name",
    `country` = "Country",
    `role`="Role",
    `description`=""
  ) %>% 
  tab_header(
    title = md("**Women of 2020**"),
    subtitle = html("<em>100 inspiring and influential women from around the world<em>")
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_source_note(source_note = "Data: BBC")%>% 
  tab_options(
    row_group.background.color="#faeb9e",
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "grey",
    table.border.top.color = "grey60",
    table.border.top.width = px(1),
    table.border.bottom.color = "grey60",
    table.border.bottom.width = px(1),
    heading.subtitle.font.size = 12,
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
    table.background.color="#faf7eb",
    heading.title.font.size=30
  )

gtsave(w_table,(paste0("women-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                       ".png")))

gtsave(w_table, "women-2020.html")
