
# Import data ----
source("R/munge.R")
library(viridis)

# Histogram ----
train_long %>%
  ggplot(aes(value, fill = city)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ key, scales = "free") +
  scale_fill_viridis(discrete = TRUE)

# Line graph ----
train_long %>%
  ggplot(aes(week_start_date, value, color = city)) +
  geom_line() +
  facet_wrap(~ key, scales = "free") +
  scale_color_viridis(discrete = TRUE)

# Total cases ----
train_long %>%
  filter(key == "total_cases") %>% 
  ggplot(aes(week_start_date, value, color = city)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE)

# Total cases per weekofyear ----
train_long %>% 
  filter(key == "total_cases") %>% 
  ggplot(aes(weekofyear, value, color = city, group = year)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~ city)

# Distribution of events ----
train_long %>%
  filter(key == "total_cases") %>% 
  ggplot(aes(log2(value + 1), fill = city)) +
  geom_histogram(bins = 60) +
  scale_fill_viridis(discrete = TRUE)
