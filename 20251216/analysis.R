library(dplyr)
library(ggplot2)
library(maps)
library(stringr)

options(scipen = 999) # disable scientific notation

download <- tidytuesdayR::tt_load("2025-12-16")

roundabouts_clean <- download$roundabouts_clean

rm(download)
gc()

summary(roundabouts_clean)

# roundabouts completed by year
sort(unique(roundabouts_clean$year_completed))

rnd_by_yr_all <- roundabouts_clean |> 
  filter(year_completed != 0) |> 
  group_by(year_completed) |> 
  count()

ggplot(rnd_by_yr_all, aes(x = year_completed, y = n)) + 
  geom_line()

# look from 2000 onwards only as data is minimal before then
rnd_by_yr <- roundabouts_clean |> 
  filter(year_completed >= 2000) |> 
  group_by(country, year_completed) |> 
  count()

# get country longitude and latitude
world <- map_data("world")

roundabouts_clean <- roundabouts_clean |> 
  mutate(country = case_when(
           country == "United Kingdom" ~ "UK",
           country == "United States" ~ "USA",
           TRUE ~ country
         ))

# country by number of roundabouts in the database
rnd_by_country <- roundabouts_clean |> 
  group_by(country) |> 
  count()

joined <- inner_join(world, rnd_by_country, by = c("region" = "country"))

# plot map
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

world_roundabouts <- ggplot(joined, aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(aes(fill = n)) + 
  scale_fill_distiller(palette = "RdBu", direction = -1) + 
  plain

world_roundabouts


