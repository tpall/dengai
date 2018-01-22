
library(tidyverse)
library(modelr)
library(prophet)


dengue_features <- read_csv("data/dengue_features_train.csv")
dengue_labs <- read_csv("data/dengue_labels_train.csv")
dengue <- left_join(dengue_labs, dengue_features)
dengue$week_start_date %>% is.na() %>% sum
dengue$total_cases %>% is.na() %>% sum

library(mice)
md.pattern(dengue)
dengue_imp <- mice(select(dengue, -c(1:5)), method = "norm.predict", m = 1, maxit = 1)
dengue_imp <- bind_cols(select(dengue, c(1:5)), complete(dengue_imp))

dengue_nested <- dengue_imp %>%
  select(-year, -weekofyear) %>% 
  rename(ds = week_start_date, y = total_cases) %>% 
  mutate(y = log(y + 1),
    cap = case_when(
    city == "sj" ~ log(500),
    city == "iq" ~ log(200)
  )) %>%
  group_by(city) %>% 
  nest() %>% 
  mutate(train = map(data, ~ .x[1:(nrow(.x) - 101),]),
         test = map(data, ~ .x[(nrow(.x) - 100):nrow(.x),]))

# Dengai ------------------------------------------------------------------

dengue_nested <- dengue_nested %>% 
  mutate(mod = map(train, 
                   prophet, 
                   weekly.seasonality = FALSE,
                   daily.seasonality = FALSE,
                   growth = 'linear',
                   mcmc.samples = 4,
                   fit = FALSE), 
         mod_prec_amt = map(mod, add_regressor, name = "precipitation_amt_mm"),
         mod_air_temp_k = map(mod, add_regressor, name = "reanalysis_air_temp_k"))

dengue_nested_fits <- dengue_nested %>% 
  mutate(mod = map2(mod, train, fit.prophet),
         mod_prec_amt = map2(mod_prec_amt, train, fit.prophet),
         mod_air_temp_k = map2(mod_air_temp_k, train, fit.prophet))

dengue_nested_fits <- dengue_nested_fits %>% 
  mutate(mod_pred = map2(mod, test, predict))


dengue_nested$forecast[[1]] %>% 
  mutate(ds = lubridate::ymd(ds)) %>% 
  select(ds, yhat, yhat_lower, yhat_upper) %>% 
  left_join(select(dengue_nested$test[[1]], ds, y)) %>% 
  mutate_at(2:5, ~ exp(.x) - 1)



dengue_nested <- dengue_nested %>% 
  mutate(p = map2(mod, forecast, plot),
         pc = map2(mod, forecast, prophet_plot_components))
dengue_nested$p
dengue_nested %>% 
  mutate(p = map2(p, test, ~ .x + geom_point(aes(ds, y), data = .y, color = "red")))
p <- dengue_nested$p[[1]]
