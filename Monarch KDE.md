# Loading data and setting up library

library(here) 
library(tidyverse) 
library(janitor)
library(rinat) 
library(sf) 
library(rnaturalearth)
library(rnaturalearthdata) 
library(patchwork) 
library(sp)
library(adehabitatHR)

monarch1<- read.csv("C:/Users/Monarch_research_1_1_2010_through_1_1_2020.csv")
monarch2<- read.csv("C:/Users/Monarch_research_1_2_2020_through_1_1_2023.csv")
monarch3<- read.csv("C:/Users/observations-685188 (1).csv")

joined1and2<- full_join(monarch1, monarch2)

joinedall<- full_join(joined1and2, monarch3)

#plot of all monarch points

bbox <- list(
  swlat = 15.02673,
  swlng = -137.879,
  nelat = 59.86927,
  nelng = -40.75595) 

monarch_clean<- joinedall |> dplyr::select(latitude, longitude) |>drop_na()

min_lat <- 15
max_lat <- 59
min_lon <- -138
max_lon <- -40

monarch_filter <- monarch_clean |>
  filter(
    latitude >= min_lat & latitude <= max_lat &
      longitude >= min_lon & longitude <= max_lon)

monarch_sf<-st_as_sf(monarch_filter, coords = c("longitude", "latitude"),crs = 4326) 

north_am <- ne_countries(country = c("United States of America", "Canada", "Mexico"),
                         scale="medium", returnclass = "sf")

kde_monarch <- ggplot() +
  geom_sf(data = north_am, fill = "white", color = "gray10") +
  geom_sf(data = monarch_sf, color="orange", alpha = 0.6,size = .1) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat)) +
  labs(title = "Monarchs",
       subtitle = "all time") +
  theme_void(base_size = 14)

#  KDE of all Monarch data 

monarch_clean_year<- joinedall |>
  dplyr::select(observed_on, latitude, longitude)|>
  drop_na() |>
  mutate(year = year(as.Date(observed_on)))|>
  filter(year >= 2015 & year <= 2024) |>
  filter( latitude >= min_lat & latitude <= max_lat &
      longitude >= min_lon & longitude <= max_lon)

monarch_sf <- st_as_sf(monarch_clean_year, coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(5070)

monarch_sp <- as_Spatial(monarch_sf)
kud <- kernelUD(monarch_sp, h = "href")
kde_poly90 <- getverticeshr(kud, percent = 90)
kde_poly50 <- getverticeshr(kud, percent = 50)

monarch_proj<-st_transform(monarch_sf, 5070)
monarch_sp_points<-SpatialPoints(coords=st_coordinates(monarch_proj), 
                                 proj4string = CRS(st_crs(monarch_proj)$proj4string))

kde_sf_monarch_90 <- st_as_sf(kde_poly_yearly) |>
  st_transform(4326) 
kde_sf_monarch_50 <- st_as_sf(kde_poly_yearly50) |>
  st_transform(4326) 

p_monarch_kde_50_90 <- ggplot() +
  geom_sf(data = north_am, fill = "white", color = "gray30") +
  geom_sf(data = monarch_sf, color="orange", alpha = 0.6,size = 1)  +
  geom_sf(data = kde_sf_monarch_90, color="orange2",size = 1)  +
  geom_sf(data = kde_sf_monarch_50, color="orange2",fill="orange3", alpha=0.3, size = 1)  +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat)) +
  labs(title = "iNaturalist Observations of Monarch Butterflies",
       subtitle = "Year:2010-2024") +
  theme_void(base_size = 1)

# Yearly KDE of Monarchs 

monarch_sf_yearly <-(st_as_sf(monarch_clean_year, coords = c("longitude", "latitude"), crs =4326)|>
  st_transform(5070))["year"]

monarch_sp_yearly <- as_Spatial(monarch_sf_yearly["year"])

kud_yearly <- kernelUD(monarch_sp_yearly, h = "href")
  
kde_poly_yearly90 <- st_as_sf(getverticeshr(kud_yearly, percent = 90)) |>
                                st_transform(4326)|>
                                rename(year = id)
kde_poly_yearly50 <- st_as_sf(getverticeshr(kud_yearly, percent = 50))|>
                                st_transform(4326) |>
                                rename(year = id)

yearly_plot <-ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "black") +
  geom_sf(data = kde_poly_yearly90, fill ="darkorange2", alpha = 0.15, color = "darkorange2", linewidth = 0.3) +
  geom_sf(data = kde_poly_yearly50, fill ="darkorange2", alpha = 0.45, color = "darkorange2", linewidth = 0.8) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat))  +
  facet_wrap(~year) +           
  theme_bw() +
  theme( axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         panel.grid = element_blank(), 
         panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), 
         strip.background = element_rect(fill = "gray85", color = "black", linewidth = 0.8), 
         strip.text = element_text(face = "bold", size = 9),
         legend.position = "none")

# Seasonal KDE of Monarch 

monarch_clean_date<- joinedall |> dplyr::select(observed_on, latitude, longitude) |>drop_na() |>
  mutate(year = year(as.Date(observed_on))) |>
  filter(year >= 2015 & year <=2024)|>
  filter( latitude >= min_lat & latitude <= max_lat &
            longitude >= min_lon & longitude <= max_lon)

monarch_sf_date <- st_as_sf(monarch_clean_date, coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(5070)

monarch_sp_date <- SpatialPoints(coords=st_coordinates(monarch_proj),
                                   proj4string = CRS(st_crs(monarch_proj)$proj4string))

monarch_seasonal <- monarch_sf_date|>
  mutate(month = month(as.Date(observed_on)),
         season = case_when(
           month %in% c(3, 4, 5) ~ "Spring",
           month %in% c(6, 7, 8) ~ "Summer",
           month %in% c(9, 10, 11) ~ "Fall",
           TRUE ~ "Winter"
         )) |>
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))

monarch_sp_season <- as_Spatial(st_as_sf(monarch_seasonal, coords = c("longitude", "latitude"), crs = 4326) |>
                                  st_transform(5070))["season"]
kud_season <- kernelUD(monarch_sp_season, h = "href")
kde_sf_season <- st_as_sf(getverticeshr(kud_season, percent = 90)) |>
  st_transform(4326) |>
  rename(season = id)
kde_sf_season50 <- st_as_sf(getverticeshr(kud_season, percent = 50)) |>
  st_transform(4326) |>
  rename(season = id)

seasonal_plot <- ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "black") +
  geom_sf(data = kde_sf_season, fill = "darkorange2", alpha = 0.15, color = "darkorange2", linewidth = 0.3) +
  geom_sf(data = kde_sf_season50, fill = "darkorange2", alpha = 0.45, color = "darkorange2", linewidth = 0.8) +
  facet_wrap(~season) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat))  +
  theme_minimal(base_size = 12)+
  theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), 
          strip.background = element_rect(fill = "gray85", color = "black", linewidth = 0.8), 
          strip.text = element_text(face = "bold", size = 9),
         legend.position = "none")

# Seasonal and Yearly KDE of Monarch 

monarch_winter_yearly <- monarch_seasonal |>
  filter(season== "Winter")|>
  mutate(year_season = paste(season, year, sep = " "))

monarch_fall_yearly <- monarch_seasonal |>
    filter(season== "Fall")|>
  mutate(year_season = paste(season, year, sep = " "))

monarch_spring_yearly <- monarch_seasonal |>
    filter(season== "Spring")|>
  mutate(year_season = paste(season, year, sep = " "))

monarch_summer_yearly <- monarch_seasonal |>
    filter(season== "Summer")|>
  mutate(year_season = paste(season, year, sep = " "))


monarch_sp_winter_yearly <- as_Spatial(st_as_sf(monarch_winter_yearly, coords = c("longitude", "latitude"), crs = 4326) |> 
                                           st_transform(5070))["year_season"]
monarch_sp_fall_yearly <- as_Spatial(st_as_sf(monarch_fall_yearly, coords = c("longitude", "latitude"), crs = 4326) |> 
                                           st_transform(5070))["year_season"]
monarch_sp_spring_yearly <- as_Spatial(st_as_sf(monarch_spring_yearly, coords = c("longitude", "latitude"), crs = 4326) |> 
                                           st_transform(5070))["year_season"]
monarch_sp_summer_yearly <- as_Spatial(st_as_sf(monarch_summer_yearly, coords = c("longitude", "latitude"), crs = 4326) |> 
                                           st_transform(5070))["year_season"]

kud_w_yearly <- kernelUD(monarch_sp_winter_yearly, h = "href", extent=2)
kde_sf_w_yearly <- st_as_sf(getverticeshr(kud_w_yearly, percent = 90)) |> 
  st_transform(4326) |> 
  rename(year_season = id)
kde_sf_w_yearly50 <- st_as_sf(getverticeshr(kud_w_yearly, percent = 50)) |> 
  st_transform(4326) |> 
  rename(year_season = id)

kud_f_yearly <- kernelUD(monarch_sp_fall_yearly, h = "href", extent=2)
kde_sf_f_yearly <- st_as_sf(getverticeshr(kud_f_yearly, percent = 90)) |> 
  st_transform(4326) |> 
  rename(year_season = id)
kde_sf_f_yearly50 <- st_as_sf(getverticeshr(kud_f_yearly, percent = 50)) |> 
  st_transform(4326) |> 
  rename(year_season = id)

kud_sp_yearly <- kernelUD(monarch_sp_spring_yearly, h = "href", extent=2)
kde_sf_sp_yearly <- st_as_sf(getverticeshr(kud_sp_yearly, percent = 90)) |> 
  st_transform(4326) |> 
  rename(year_season = id)
kde_sf_sp_yearly50 <- st_as_sf(getverticeshr(kud_sp_yearly, percent = 50)) |> 
  st_transform(4326) |> 
  rename(year_season = id)

kud_su_yearly <- kernelUD(monarch_sp_summer_yearly, h = "href", extent=2)
kde_sf_su_yearly <- st_as_sf(getverticeshr(kud_su_yearly, percent = 90)) |> 
  st_transform(4326) |> 
  rename(year_season = id)
kde_sf_su_yearly50 <- st_as_sf(getverticeshr(kud_su_yearly, percent = 50)) |> 
  st_transform(4326) |> 
  rename(year_season = id)

monarch_w_and_year <- ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "white") +
  geom_sf(data = kde_sf_w_yearly, fill = "#3681b6", alpha = 0.15, color = "#3681b6", linewidth = 0.3) +
  geom_sf(data = kde_sf_w_yearly50, fill = "#3681b6", alpha = 0.45, color = "#3681b6", linewidth = 0.8) +
  facet_wrap(~year_season) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat))  +
  theme_minimal(base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_rect(color = "black",  fill = NA, linewidth = 0.8), 
        strip.background = element_rect(fill = "gray85",color = "black", linewidth = 0.8), 
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "none")

monarch_f_and_year <- ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "white") +
  geom_sf(data = kde_sf_f_yearly, fill = "#d7570d", alpha = 0.15, color = "#d7570d", linewidth = 0.3)+
  geom_sf(data = kde_sf_f_yearly50, fill = "#d7570d", alpha = 0.45, color = "#d7570d", linewidth = 0.8)  +
  facet_wrap(~year_season) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat))  +
  theme_minimal(base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_rect(color = "black",fill = NA, linewidth = 0.8), 
        strip.background = element_rect(fill = "gray85", color = "black", linewidth = 0.8), 
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "none")

monarch_su_and_year <- ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "white") +
  geom_sf(data = kde_sf_su_yearly, fill = "#edaa09", alpha = 0.15, color = "#edaa09", linewidth = 0.3)+
  geom_sf(data = kde_sf_su_yearly50, fill = "#edaa09", alpha = 0.45, color = "#edaa09", linewidth = 0.8)  +
  facet_wrap(~year_season) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat))  +
  theme_minimal(base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), 
        strip.background = element_rect(fill = "gray85",  color = "black", linewidth = 0.8), 
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "none")

monarch_sp_and_year <- ggplot() +
  geom_sf(data = north_am, fill = "gray95", color = "white") +
  geom_sf(data = kde_sf_sp_yearly,fill = "#4f8f5b", alpha = 0.15, color = "#4f8f5b", linewidth = 0.3)+
  geom_sf(data = kde_sf_sp_yearly50,fill = "#4f8f5b", alpha = 0.45, color = "#4f8f5b", linewidth = 0.8)  +
  facet_wrap(~year_season) +
  coord_sf(xlim = c(bbox$swlng, bbox$nelng), ylim = c(bbox$swlat, bbox$nelat))  +
  theme_minimal(base_size = 12)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), 
        strip.background = element_rect(fill = "gray85", color = "black", linewidth = 0.8), 
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "none")

