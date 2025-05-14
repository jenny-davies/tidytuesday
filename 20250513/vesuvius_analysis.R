library(dplyr)
library(ggplot2)
library(lubridate)

# download data
download <- tidytuesdayR::tt_load("2025-05-13")

data <- download$vesuvius

colnames(data)

unique(data$type)
unique(data$area)
unique(data$review_level)
unique(data$year)
unique(data$latitude)
unique(data$longitude)

summary(data)

# sort by time to check if revised and preliminary means revised should overwrite preliminary
# if so, duplicates may need removing
sorted <- data |>
  arrange(time, review_level)

# number of earthquakes per year
n_per_year <- data |> 
  group_by(year) |> 
  count()

# exclude first two years with only one report
filtered <- data |> 
  filter(!year %in% c(2011, 2012))

n_per_year_filtered <- filtered |> 
  group_by(year) |> 
  count()

ggplot(n_per_year_filtered, aes(x = year, y = n)) + 
  geom_col()

# group by month and year
filtered <- filtered |> 
  mutate(mnth = month(time),
         dy = day(time),
         hr = hour(time),
         year = as.character(year))

# group by different date fields and count number
n_per_month <- filtered |> 
  group_by(year, mnth) |> 
  count() |> 
  mutate(mnth = factor(mnth, levels = c(1,2,3,4,5,6,7,8,9,10,11,12)))

ggplot(n_per_month, aes(x = mnth, y = n)) + 
  geom_col(position = "dodge") + 
  facet_wrap(facets = vars(year))

n_per_day <- filtered |> 
  group_by(year, dy) |> 
  count() |> 
  mutate(dy = factor(dy, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                                    19,20,21,22,23,24,25,26,27,28,29,30,31)))

ggplot(n_per_day, aes(x = dy, y = n)) + 
  geom_col(position = "dodge") + 
  facet_wrap(facets = vars(year))

n_per_hour <- filtered |> 
  group_by(year, hr) |> 
  count() |> 
  mutate(hr = factor(hr, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                                    19,20,21,22,23,24)))

ggplot(n_per_hour, aes(x = hr, y = n)) + 
  geom_col(position = "dodge") + 
  facet_wrap(facets = vars(year))

# tidy up last plot
ggplot(n_per_hour, aes(x = hr, y = n)) + 
  geom_col(position = "dodge") + 
  facet_wrap(facets = vars(year)) + 
  xlab("Hour of the day") +
  ylab("Number of reported events") + 
  ggtitle("Number of reported seismic events at Mount Vesuvius by hour of the day and year")


