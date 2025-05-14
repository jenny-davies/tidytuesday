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
         hr = hour(time))

# group by different date fields and count number
n_per_month <- filtered |> 
  group_by(year, mnth) |> 
  count()

ggplot(n_per_month, aes(x = mnth, y = n, fill = year)) + 
  geom_col(position = "dodge")

n_per_day <- filtered |> 
  group_by(year, dy) |> 
  count()

ggplot(n_per_day, aes(x = dy, y = n, fill = year)) + 
  geom_col(position = "dodge")

n_per_hour <- filtered |> 
  group_by(year, hr) |> 
  count()

ggplot(n_per_hour, aes(x = hr, y = n, fill = year)) + 
  geom_col(position = "dodge")
