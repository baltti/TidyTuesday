
library(tidyverse)
library(rnaturalearth)
library(sf)
library(cowplot)
library(RColorBrewer)
library(extrafont)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

glimpse(mobile)

countries<-ne_countries(type="countries", returnclass = "sf")

mobile$entity=recode(mobile$entity, "United States" ="United States of America")
mobile<-left_join(countries,mobile,by = c("admin" = "entity")) %>% 
  filter(year==2017) %>% 
  st_transform("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ") 


quantile_subs<- mobile %>%
  pull(mobile_subs) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

quantiles_gdp <- mobile %>%
  pull(gdp_per_cap) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

# нужно создать цветовую схему для кодировки переменных
# желтый цвет - ожидаемая продолжительность
# синий - индекс счастья
#каждой комбинации будет соответствовать свой цвет 
#цвета заданы в кодировке HEX - шестнадцатеричное представление RGB
bivariate_color_scale <- tibble(
  "3 - 3" = "#0d8000", # высокая продолжительность, высокий индекс счастья
  "2 - 3" = "#81b700",
  "1 - 3" = "#e8ca00", # низкая продолжительность, высокий индекс счастья 
  "3 - 2" = "#0d808b",
  "2 - 2" = "#81b78b", # средняя продолжительность, средний индекс счастья
  "1 - 2" = "#e8dc8b",
  "3 - 1" = "#0d80d9", # высокая продолжительность, низкий индекс счастья
  "2 - 1" = "#81b7e1",
  "1 - 1" = "#e8e8e8" # низкая продолжительность, низкий индекс счастья
) %>%
  gather("group", "fill")
# делим объекты на группы и присоединяем отдельным атрибутом 
mobile %<>%
  mutate(subs_quantile = cut(mobile_subs,
                             breaks = quantile_subs,
                             include.lowest = TRUE),
         gdp_quantiles = cut(gdp_per_cap,
                             breaks = quantiles_gdp,
                             include.lowest = TRUE),
         # вносим номера групп по обоим переменным в один столбец
         # это нужно, чтобы присоединить цветовую кодировку
         group = paste(as.numeric(subs_quantile), "-",
                       as.numeric(gdp_quantiles))
  ) %>%
  # теперь присоединяем цветовую кодировку
  # теперь у каждой страны есть свой цветовой код на основе значений
  #ВВП и ожидаемой продолжительности жизни
  left_join(bivariate_color_scale, by = "group")

#создание карты
map<-mobile%>%
  ggplot() +
  geom_sf(aes(fill=fill))+ 
  scale_fill_identity() +
  #подписи осей и заголовок
  labs(x = NULL,
       y = NULL,
       title = "Mobile subscriptions and GDP per capita, 2017")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text =element_blank())+
  labs(caption = "source: Hannah Ritchie (2017) - Technology Adoption.")+
  theme(plot.title = element_text(hjust = 0.5,size = 30, 
                                  family = "Walkway",face = "bold",
                                  colour = "black"),
        text = element_text(size = 15, 
                            family = "Walkway",
                            colour = "black"))

#для создания легенды нужно выделить отдельно продолжительность и индекс счастья
#так как при подготовке цветовой схемы каждый цвет был соотнесен с группой
#группы закодированы в виде "1-2" 
bivariate_color_scale %<>%
  separate(group, into = c("mobile_subs", "gdp_per_cap"), sep = " - ") %>%
  mutate(mobile_subs = as.integer(mobile_subs),
         gdp_per_cap = as.integer(gdp_per_cap))

#подготовка легенды
legend <- ggplot() +
  geom_tile(data = bivariate_color_scale,
            mapping = aes(
              x = mobile_subs,
              y = gdp_per_cap,
              fill = fill)) +
  scale_fill_identity() +
  labs(x = "More subscriptions-->",
       y = "Higher GDP per capita-->") +
  theme_minimal() +
  # размер шрифта
  theme(axis.title = element_text(size = 10, 
                                  family = "Walkway",face = "bold")) +
  # квадратные ячейки
  coord_fixed()

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)

ggsave((paste0("phones-", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".png")), device = "png",dpi = 500, width = 10, height = 8)
