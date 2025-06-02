library(tidyverse)
library(plotly)
library(sf)
library(ggplot2)
library(dplyr)
library(gganimate)
library(transformr)
library(rnaturalearth)
library(rnaturalearthdata)
library(gifski)  # Für das Rendern als GIF

swiss_data <- read_csv("data/Enriched Swiss protest data.csv")

# 1. load geodata ----
world <- ne_countries(scale = "medium", returnclass = "sf")

switzerland <- world %>% filter(admin == "Switzerland")

# 2. filter data ----
climate_data <- swiss_data %>%
  filter(Topic == "Climate")

# 3. create sf-object ----
climate_sf <- st_as_sf(
  climate_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# 4. create gif ----
# basic plot
p <- ggplot() +
  geom_sf(data = switzerland, fill = "white", color = "black") +
  geom_sf(data = climate_sf, color = "red", size = 3) +
  labs(title = "Klimaproteste in der Schweiz: {frame_time}") +
  theme_minimal()

#  Animation definieren
animation <- p +
  transition_time(event_date) +
  shadow_mark(past = TRUE, alpha = 0.2) +
  ease_aes("linear")

# GIF exportieren
animate(animation,
        width = 700, height = 700,
        duration = 10, fps = 10,
        renderer = gifski_renderer("plot/climate_protests.gif"))
