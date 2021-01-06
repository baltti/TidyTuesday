
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



transit_world<-transit_cost %>% 
  drop_na(cost,cost_km_millions,real_cost) %>% 
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
    country=="UA"~"Ukraine",
    country=="CA"~"Canada",
    country=="US"~"United States of America",
    country=="MX"~"Mexico",
    country=="TW"~"Taiwan",
    country=="VN"~"Vietnam",
    country=="KR"~"South Korea",
    country=="AU"~"Australia",
    country=="SG"~"Singapore",
    country=="IN"~"India",
    country=="CL"~"Chile",
    country=="BR"~"Brazil",
    country=="PA"~"Panama",
    country=="PH"~"Philippines",
    country=="IR"~"Iran",
    country=="EC"~"Ecuador",
    country=="IL"~"Israel",
    country=="NZ"~"New Zealand",
    country=="BD"~"Bangladesh",
    country=="ID"~"Indonesia",
    country=="PK"~"Pakistan",
    country=="TH"~"Thailand",
    country=="JP"~"Japan",
    country=="AR"~"Argentina",
    country=="PE"~"Peru",
    country=="MY"~"Malaysia",
    country=="TR"~"Turkey",
    country=="CN"~"China",
    country=="SA"~"Saudi Arabia",
    country=="AE"~"United Arab Emirates ",
    country=="EG"~"Egypt",
    country=="KW"~"Kuwait",
    country=="UZ"~"Uzbekistan",
    country=="QA"~"Qatar",
    country=="BH"~"Bahrain"
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
  fmt_number(
    columns = vars(length), # reference cols by position
    decimals = 1 # decrease decimal places
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
    `length`="Proposed length, km",
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
    title = md("**Cost of transit infrastructure projects**")
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

gtsave(transit_world, "transit-cost-world.html")


library(reactable)
library(htmltools)
library(htmlwidgets)

cost<-transit_cost %>% 
  drop_na(cost,cost_km_millions,real_cost) %>% 
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
    country=="UA"~"Ukraine",
    country=="CA"~"Canada",
    country=="US"~"USA",
    country=="MX"~"Mexico",
    country=="TW"~"Taiwan",
    country=="VN"~"Vietnam",
    country=="KR"~"South Korea",
    country=="AU"~"Australia",
    country=="SG"~"Singapore",
    country=="IN"~"India",
    country=="CL"~"Chile",
    country=="BR"~"Brazil",
    country=="PA"~"Panama",
    country=="PH"~"Philippines",
    country=="IR"~"Iran",
    country=="EC"~"Ecuador",
    country=="IL"~"Israel",
    country=="NZ"~"New Zealand",
    country=="BD"~"Bangladesh",
    country=="ID"~"Indonesia",
    country=="PK"~"Pakistan",
    country=="TH"~"Thailand",
    country=="JP"~"Japan",
    country=="AR"~"Argentina",
    country=="PE"~"Peru",
    country=="MY"~"Malaysia",
    country=="TR"~"Turkey",
    country=="CN"~"China",
    country=="SA"~"Saudi Arabia",
    country=="AE"~"United Arab Emirates ",
    country=="EG"~"Egypt",
    country=="KW"~"Kuwait",
    country=="UZ"~"Uzbekistan",
    country=="QA"~"Qatar",
    country=="BH"~"Bahrain"
  )) %>% 
  mutate(real_cost=as.numeric(real_cost)) %>% 
  group_by(country) %>% 
  select(-e,-tunnel,-source1,-source2, -ppp_rate,-reference,-rr,-year) %>% 
  mutate(years=glue::glue("{start_year}-{end_year}"),
         cost=glue::glue("{cost} {currency}"),
         cost_km_millions=round(cost_km_millions,digits=2)) %>% 
  select(-start_year,-end_year,-currency) 

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

library(crosstalk)

data <- SharedData$new(cost)

bscols(
  widths = c(3, 9),
  list(
    filter_slider("real_cost", "Cost", data, ~real_cost, width = "100%"),
    filter_slider("cost_km_millions", "Cost per km", data, ~cost_km_millions, width = "100%"),
    filter_select("country", "Country", data, ~country)
  ),
  reactable(data,
            fullWidth = TRUE,
            showPageSizeOptions = TRUE, 
            pageSizeOptions = c(10, 20, 50,100), 
            defaultPageSize = 20,
            theme = reactableTheme(
              headerStyle = list(
                "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                borderColor = "#555"
              )
            ),
            columns = list(
              country = colDef(name="Country"),
              city = colDef(filterable = TRUE,
                            name="City"),
              length=colDef(name = "Length, km"),
              tunnel_per=colDef(name = "Completed"),
              stations=colDef(name = "Number of stations"),
              cost=colDef(name = "Cost, local currency"),
              years=colDef(name = "Years of construction"),
              real_cost = colDef(
                name = "Cost",
                defaultSortOrder = "desc",
                # Render the bar charts using a custom cell render function
                cell = JS("function(cellInfo) { return '$'+' '+cellInfo.value + ' M'}"),
                # And left-align the columns
                align = "right",
                class = "border-left"),
              cost_km_millions = colDef(
                name = "Cost per km",
                style = function(value) {
                  value
                  normalized <- (value - min(cost$cost_km_millions)) / (max(cost$cost_km_millions) - min(cost$cost_km_millions))
                  color <- good_color(normalized)
                  list(background = color)
                },
                cell = JS("function(cellInfo) { return '$' +' '+cellInfo.value + ' M'}"),
                align = "right"
              )
            ))
)
