# Elena Nirgiotis 
# 03/11/2026
# Semirelict Underwing moth MCP analysis
# This script cleans occurrence data, creates seasonal subsets,
# and generates annual MCP plots for Summer and Fall observations (2018–2024).

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
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ "Winter"),
    year_num = as.integer(year),
    year_season = paste(season, year)) |>
  filter(season %in% c("Summer", "Fall")) |>
  filter(year_season != "Fall 2021") |>
  mutate(season = factor(season, levels = c("Summer", "Fall")))

# 3. Create basemap and bounding box
north_am <- ne_countries(
  country = c("United States of America", "Canada"),
  scale = "medium",
  returnclass = "sf")

bbox <- st_bbox(
  st_as_sf(moth_seasonal, coords = c("longitude", "latitude"), crs = 4326))

# 4. Function to generate seasonal MCP plots by year
plot_season_mcp <- function(season_name, min_n = 5) 
  {dat <- moth_seasonal |>
    filter(season == season_name) |>
    group_by(year_num) |>
    filter(n() >= min_n) |>
    ungroup()
  
  if (nrow(dat) == 0) {
    stop(paste("No data for", season_name, "after filtering. Lower min_n.")) }
  
  moth_sp <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(5070) |>
    as_Spatial()
  
  moth_sp <- moth_sp["year_num"]
  
  mcp95 <- mcp(moth_sp, percent = 95)
  mcp50 <- mcp(moth_sp, percent = 50)
  
  mcp95_sf <- st_as_sf(mcp95) |>
    st_transform(4326) |>
    rename(year = id)
  
  mcp50_sf <- st_as_sf(mcp50) |>
    st_transform(4326) |>
    rename(year = id)
  
  ggplot() +
    geom_sf(data = north_am, fill = "gray95", color = "black") +
    geom_sf(
      data = mcp95_sf,
      fill = "purple",
      alpha = 0.15,
      color = "purple",
      linewidth = 0.3) +
    geom_sf(
      data = mcp50_sf,
      fill = "purple",
      alpha = 0.45,
      color = "purple",
      linewidth = 0.8) +
    facet_wrap(~year) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      strip.background = element_rect(fill = "gray85", color = "black", linewidth = 0.8),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "none")}

# 5. Generate figures
p_summer <- plot_season_mcp("Summer")
p_fall   <- plot_season_mcp("Fall")
