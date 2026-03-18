# Elena Nirgiotis 
# 03/18/2026
# Semirelict Underwing moth MCP analysis
# This script cleans occurrence data, creates seasonal subsets,
# and generates one faceted MCP figure for Summer and Fall (2018–2024).

# Clear Environment 
rm(list = ls())

# Load packages 
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(sp)
library(adehabitatHR)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# 1. Read, clean, and filter occurrence data
moth_raw <- read.csv(here("Data", "Semirelect_Underwing.csv")) |>
  clean_names() |>
  dplyr::select(observed_on, latitude, longitude) |>
  drop_na() |>
  mutate(
    observed_on = as.Date(observed_on),
    year = year(observed_on),
    month = month(observed_on)) |>
  filter(
    observed_on >= as.Date("2018-01-01"),
    observed_on <= as.Date("2024-12-31"))

# 2. Assign seasons and remove Fall 2021
moth_seasonal <- moth_raw |>
  mutate(
    season = case_when(
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE                    ~ "Winter"),
    year_num = as.integer(year),
    year_season = paste(season, year)) |>
  filter(season %in% c("Summer", "Fall")) |>
  filter(year_season != "Fall 2021") |>
  mutate(season = factor(season, levels = c("Summer", "Fall")))

# 3. Basemap
north_am <- ne_countries(
  country = c("United States of America", "Canada"),
  scale = "medium",
  returnclass = "sf")

# 4. Bounding box from all moth points
bbox <- st_bbox(
  st_as_sf(moth_seasonal, coords = c("longitude", "latitude"), crs = 4326))

# 5. Function to make MCP objects for one season
make_season_mcp <- function(season_name) {
  
  dat <- moth_seasonal |>
    filter(season == season_name)
  
  if (nrow(dat) < 5) {
    stop(paste("Not enough data for", season_name)) }
  
  season_fill <- case_when(
    season_name == "Summer" ~ "#F2B701",
    season_name == "Fall"   ~ "#E16C09")
  
  moth_sp <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(5070) |>
    as_Spatial()
  
  moth_sp$group <- season_name
  moth_sp <- moth_sp["group"]
  
  mcp95_sf <- mcp(moth_sp, percent = 95) |>
    st_as_sf() |>
    st_transform(4326) |>
    mutate(
      season = season_name,
      level = "95%",
      fill_col = season_fill)
  
  mcp50_sf <- mcp(moth_sp, percent = 50) |>
    st_as_sf() |>
    st_transform(4326) |>
    mutate(
      season = season_name,
      level = "50%",
      fill_col = season_fill)
  
  list(mcp95 = mcp95_sf, mcp50 = mcp50_sf)}

# 6. Build MCP objects for both seasons
summer_objs <- make_season_mcp("Summer")
fall_objs   <- make_season_mcp("Fall")

mcp95_all <- bind_rows(summer_objs$mcp95, fall_objs$mcp95)
mcp50_all <- bind_rows(summer_objs$mcp50, fall_objs$mcp50)

# 7. Plot as one faceted figure
p_moth <- ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "black") +
  geom_sf(
    data = mcp95_all,
    fill = c(adjustcolor("#E16C09", alpha.f = 0.35),
             adjustcolor("#F2B701", alpha.f = 0.35)),
    color = c("#E16C09", "#F2B701"),
    linewidth = 0.3) +
  geom_sf(
    data = mcp50_all,
    fill = c(adjustcolor("#E16C09", alpha.f = 0.65),
             adjustcolor("#F2B701", alpha.f = 0.65)),
    color = c("#E16C09", "#F2B701"),
    linewidth = 0.8) +
  facet_wrap(~season, nrow = 1) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "gray85", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 9) )

print(p_moth)

ggsave(
  filename = here("Figures", "Moth_seasonal_MCP.png"),
  plot = p_moth,
  width = 10,
  height = 5,
  dpi = 300)
