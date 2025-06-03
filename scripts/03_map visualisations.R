library(tidyverse)
library(plotly)
library(sf)
library(ggplot2)
library(dplyr)
library(gganimate)
library(transformr)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(gifski)  # Für das Rendern als GIF

swiss_data <- read_csv("data/Enriched Swiss protest data.csv")

# 1. load geodata ----
world <- ne_countries(scale = "medium", returnclass = "sf")

switzerland <- world %>% filter(admin == "Switzerland")

# better map with cantons
kantone_url <- "https://labs.karavia.ch/swiss-boundaries-geojson/geojson/2020/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.geojson"

kantone <- st_read(kantone_url)

# 2. filter data ----
climate_data <- swiss_data %>%
  filter(Topic == "Climate")  %>%
  mutate(
    event_date = as.Date(event_date),
    event_month = floor_date(event_date, "month")  # z.B. 2025-05-01
  )

# 3. create sf-object ----
climate_sf <- st_as_sf(
  climate_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# 4. create gif ----
# basic plot
p <- ggplot() +
  geom_sf(data = kantone, fill = "white", color = "black") +
  geom_sf(data = climate_sf, aes(color = "red", size = 2)) +
  scale_size(range = c(4, 14)) +
  labs(title = "Klimaproteste in der Schweiz: {format(frame_time, '%B %Y')}") +
  theme_minimal()

#  Animation definieren
animation <- p +
  transition_time(climate_sf$event_month) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 1) +
  ease_aes("linear")

# GIF exportieren
animate(animation,
        width = 900, height = 650,
        duration = 10, fps = 6,
        renderer = gifski_renderer("plot/climate_protests.gif"))
