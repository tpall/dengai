
library(tidyverse)
library(prophet)

train_features <- read_csv("data/dengue_features_train.csv")
train_labs <- read_csv("data/dengue_labels_train.csv")

train <- left_join(train_labs, train_features)

# Example -----------------------------------------------------------------

# df <- read.csv('https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_peyton_manning.csv')
# 
# df %>% 
#   ggplot(aes(y)) +
#   geom_histogram(bins = 60) +
#   scale_fill_viridis(discrete = TRUE)
# 
# any(df$y == 0)
# 
# df <- mutate(df, y = log(y))
# head(df)
# 
# m <- prophet(df)
# future <- make_future_dataframe(m, periods = 365)
# tail(future)
# forecast <- predict(m, future)
# tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# plot(m, forecast)
# prophet_plot_components(m, forecast)


# Dengai ------------------------------------------------------------------

train_nested <- train %>% 
  select(-year, -weekofyear) %>% 
  rename(ds = week_start_date, y = total_cases) %>% 
  mutate(cap = case_when(
    city == "sj" ~ 500,
    city == "iq" ~ 200
  )) %>% 
  group_by(city) %>% 
  nest()

train_nested$data

train_nested <- train_nested %>% 
  mutate(m = map(data, prophet, weekly.seasonality = TRUE, growth = 'logistic'))

train_nested <- train_nested %>% 
  mutate(future = map(m, make_future_dataframe, periods = 36, freq = "month"),
         cap = case_when(
           city == "sj" ~ 500,
           city == "iq" ~ 200
         ),
         future = map2(future, cap, ~ {.x$cap <- .y; .x}),
         forecast = map2(m, future, predict))

train_nested <- train_nested %>% 
  mutate(p = map2(m, forecast, plot),
         pc = map2(m, forecast, prophet_plot_components))
train_nested$p

