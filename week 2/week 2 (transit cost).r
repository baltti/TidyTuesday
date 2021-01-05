
library(tidyverse)
library(gt)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_eu<-transit_cost %>% 
  filter(country %in% c("NL",'BG',"PL","RO","RU","CZ","HU","FR","IT","FI","SE","DE","BE","DK","NO","GR",
                        "UK","AT","ES","PT","CH","UA")) %>% 
  mutate(country=case_when(
    country=="NL"~"Netherlands",
    country=='BG'~"Bulgaria",
    country=="PL"~"Poland",
    country=="RO"~"Romania",
    country=="RU"~"Russia",
    country=="CZ"~"Czech Republic",
    country=="HU"~"Hungary",
    country=="FR"~"France",
    country=="IT"~"Italy",
    country=="FI"~"Finland",
    country=="SE"~"Sweden",
    country=="DE"~"Germany",
    country=="BE"~"Belgium",
    country=="DK"~"Denmark",
    country=="NO"~"Norway",
    country=="GR"~"Greece",
    country=="UK"~"United Kingdom",
    country=="AT"~"Austria",
    country=="ES"~"Spain",
    country=="PT"~"Portugal",
    country=="CH"~"Switzerland",
    country=="UA"~"Ukraine"
  )) %>% 
  mutate(real_cost=as.numeric(real_cost)) %>% 
  group_by(country) %>% 
  gt(rowname_col="city") %>% 
  cols_merge_range(
    col_begin = vars(start_year),
    col_end = vars(end_year)
  ) %>%
  cols_merge(
    columns = vars(cost, currency),
    pattern = "{1} {2}"
  ) %>% 
  fmt_number(
    columns = vars(cost,cost_km_millions), # reference cols by position
    decimals = 3 # decrease decimal places
  ) %>% 
  cols_hide(columns = vars(e,rr,source1,source2,reference,
                           tunnel,year, ppp_rate)) %>% 
  text_transform(
    locations = cells_body(
      columns = vars(city),
      rows = 1
    ),
    fn = function(x){
      paste0("")
    }
  ) %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(cost,cost_km_millions,real_cost),
    fns = list(
      average = "mean",
      total = "sum")
  ) %>% 
  data_color(
    columns = vars(cost_km_millions),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  cols_label(
    `line`="Line",
    `start_year`="Years of construction",
    `length`="Proposed length",
    `tunnel_per`="Completed",
    `stations`="Number of stations",
    `cost`="Cost, millions",
    `real_cost`="Real cost, millions of USD",
    `cost_km_millions`="Cost per km, millions of USD"
  ) %>% 
  tab_spanner(
    label = md("**Transit project characterictics**"),
    columns = 4:11
  )  %>% 
  tab_spanner(
    label = md("**Project cost**"),
    columns = 12:20
  )  %>% 
 tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_row_groups(),
      cells_column_labels(everything())
    )
  )  %>% 
  tab_source_note(md("**Table**: @baltti | **Data**: Transit Costs Project<br>")) %>% 
  tab_header(
    title = md("**Cost of transit infrastructure projects in Europe**")
  ) %>% 
  cols_width(
    vars(start_year,length,tunnel_per,stations,real_cost,cost_km_millions) ~ px(125),
    vars(line,cost)~ px(200)
  ) %>% 
  tab_options(
    column_labels.vlines.width = px(2),
    column_labels.vlines.color = "black",
    row_group.background.color="#c2ced6",
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    row_group.padding = 10,
    row_group.font.size = 20,
    table.border.top.color = "grey60",
    table.border.top.width = px(1),
    table.border.bottom.color = "grey60",
    table.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
    heading.title.font.size=30,
    summary_row.background.color = "#e0e5eb"
)

gtsave(transit_eu,(paste0("transit-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                       ".png")))

gtsave(transit_eu, "transit-cost-europe.html")
