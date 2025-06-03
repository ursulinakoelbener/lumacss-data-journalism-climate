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

# 2. Climate GIF ----

## 2a. filter data ----
climate_data <- swiss_data %>%
  filter(Topic == "Climate")  %>%
  mutate(
    event_date = as.Date(event_date),
    event_month = floor_date(event_date, "month")  # z.B. 2025-05-01
  )

## 2b. create sf-object ----
climate_sf <- st_as_sf(
  climate_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

## 2c. create gif ----
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


# 3. Workers GIF ----

## 3a. filter data ----
workers_data <- swiss_data %>%
  filter(Topic == "Workers")  %>%
  mutate(
    event_date = as.Date(event_date),
    event_month = floor_date(event_date, "month")  # z.B. 2025-05-01
  )

## 3b. create sf-object ----
workers_sf <- st_as_sf(
  workers_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

## 3c. create gif ----
# basic plot
p_workers <- ggplot() +
  geom_sf(data = kantone, fill = "white", color = "black") +
  geom_sf(data = workers_sf, aes(color = "red", size = 1)) +
  labs(title = "Proteste von Arbeitnehmenden in der Schweiz: {format(frame_time, '%B %Y')}") +
  theme_minimal()

#  Animation definieren
animation_workers <- p_workers +
  transition_time(workers_sf$event_month) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 1) +
  ease_aes("linear")

# GIF exportieren
animate(animation_workers,
        width = 900, height = 650,
        duration = 10, fps = 6,
        renderer = gifski_renderer("plot/workers_protests.gif"))

# 4. Feminist GIF ----

## 4a. filter data ----
feminist_data <- swiss_data %>%
  filter(Topic == "Feminist")  %>%
  mutate(
    event_date = as.Date(event_date),
    event_month = floor_date(event_date, "month")  # z.B. 2025-05-01
  )

## 4b. create sf-object ----
feminist_sf <- st_as_sf(
  feminist_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

## 4c. create gif ----
# basic plot
p_feminist <- ggplot() +
  geom_sf(data = kantone, fill = "white", color = "black") +
  geom_sf(data = feminist_sf, aes(color = "red", size = 1)) +
  labs(title = "Frauenstreiks in der Schweiz: {format(frame_time, '%B %Y')}") +
  theme_minimal()

#  Animation definieren
animation_feminist <- p_feminist +
  transition_time(feminist_sf$event_month) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 1) +
  ease_aes("linear")

# GIF exportieren
animate(animation_feminist,
        width = 900, height = 650,
        duration = 10, fps = 6,
        renderer = gifski_renderer("plot/feminist_protests.gif"))

# 5. Migrants GIF ----

## 5a. filter data ----
migrants_data <- swiss_data %>%
  filter(Topic == "Migrants")  %>%
  mutate(
    event_date = as.Date(event_date),
    event_month = floor_date(event_date, "month")  # z.B. 2025-05-01
  )

## 5b. create sf-object ----
migrants_sf <- st_as_sf(
  migrants_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

## 5c. create gif ----
# basic plot
p_migrants <- ggplot() +
  geom_sf(data = kantone, fill = "white", color = "black") +
  geom_sf(data = migrants_sf, aes(color = "red", size = 1)) +
  labs(title = "Frauenstreiks in der Schweiz: {format(frame_time, '%B %Y')}") +
  theme_minimal()

#  Animation definieren
animation_migrants <- p_migrants +
  transition_time(migrants_sf$event_month) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 1) +
  ease_aes("linear")

# GIF exportieren
animate(animation_migrants,
        width = 900, height = 650,
        duration = 10, fps = 6,
        renderer = gifski_renderer("plot/migrants_protests.gif"))
