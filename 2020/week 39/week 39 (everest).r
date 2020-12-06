
library(tidyverse)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

library(extrafont)
library(ggridges)

members<-members %>% group_by(peak_name) %>% 
  left_join(peaks, by="peak_name") %>% 
  filter(height_metres>8000)

packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# Load packages ----
library(ggplot2)

# Defining the geom_flat_violin function ----
# Note: the below code modifies the
# existing github page by removing a parenthesis in line 50

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(
                ymin = min(y),
                ymax = max(y),
                xmin = x,
                xmax = x + width / 2
              )
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x)
            )
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(
              plyr::arrange(transform(data, x = xminv), y),
              plyr::arrange(transform(data, x = xmaxv), -y)
            )
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1, ])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(
            weight = 1, colour = "grey20", fill = "white", size = 0.5,
            alpha = NA, linetype = "solid"
          ),
          
          required_aes = c("x", "y")
  )


ggplot(members %>% group_by(peak_name), aes(x=peak_name, y=age, fill=peak_name))+
geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2, width=1)+
  geom_point(position = position_jitter(width =0.15), size = .1)+
  scale_fill_manual(values = c("#639be9","#8f8fbc","#999999","#7ab3b8","#0f4c8a",
                             "#ad9985","#666666","#597bc0","#8b98c1","#8596ad",
                             "#36717d","#4d6eb3","#74593e","#c5d6c2","#bfbfbf"),
                  name=NULL,guide=FALSE)+
  coord_flip()+
  theme_minimal()+
  ggtitle("Age distribution of himalayan climbers")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, family = "Romantic"),
        text = element_text(hjust = 0.5, size = 24, family = "Romantic"),
        axis.title = element_blank())+
  ggsave((paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"),
                 ".png")), device = "png",dpi = 300, width = 10, height = 18)
