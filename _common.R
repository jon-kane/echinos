# _common.R
library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())

# If dark mode...
bg_color_ = "#070607"

theme_update(
  plot.background = element_rect(fill = bg_color_, color = bg_color_),
  panel.background = element_rect(fill = bg_color_, color = NA),
  legend.background = element_rect(fill = bg_color_, color = NA), 
  legend.key = element_rect(fill = bg_color_, color = NA),     
  text = element_text(color = "gray80"),
  axis.text = element_text(color = "gray50"),
  axis.ticks = element_line(color = "gray50"),
  axis.line = element_line(color = "gray50"),
  strip.text = element_text(color = "gray80")
)

update_geom_defaults("point", list(color = "gray80"))
update_geom_defaults("line",  list(color = "gray80"))

# Functions (using camelCase per preference)
orangesRamp = colorRampPalette(colors = c("#ffead5", "#ff8100"))
greensRamp = colorRampPalette(colors = c("#a4fba6", "#006203"))