
#' Load libraries
library(tidyverse)
library(lubridate)
library(viridis)
library(glue)
library(rlang)
library(prophet)

#' Import data
dengue_features <- read_csv("data/dengue_features_train.csv")
dengue_labs <- read_csv("data/dengue_labels_train.csv")
dengue <- left_join(dengue_labs, dengue_features)

#' Impute missing values
library(mice)
md.pattern(dengue)
dengue_imp <- mice(select(dengue, -c(1:5)), 
                   method = "norm.predict", 
                   m = 2, 
                   maxit = 1)
dengue_imputed <- bind_cols(select(dengue, c(1:5)), complete(dengue_imp))
md.pattern(dengue_imputed)

#' Split data into test and train set
dengue_nested <- dengue_imputed %>%
  filter(complete.cases(.)) %>% 
  select(-year, -weekofyear) %>% 
  rename(ds = week_start_date, 
         y = total_cases) %>% 
  mutate(cap = case_when(
           city == "sj" ~ 395326,
           city == "iq" ~ 471993
         ),
         floor = 0) %>%
  group_by(city) %>% 
  nest() %>% 
  mutate(train = map(data, ~ .x[1:(nrow(.x) - 101),]),
         test = map(data, ~ .x[(nrow(.x) - 100):nrow(.x),]))

#' Seasonality
seasons <- dengue_nested %>% 
  select(city, train) %>% 
  unnest() %>% 
  mutate(week_of_year = week(ds)) %>% 
  select(city, week_of_year, 4:23) %>% 
  gather(key, value, -city, -week_of_year) %>% 
  group_by(city, week_of_year, key) %>% 
  summarise(Mean = mean(value),
            SD = sd(value))

#' Seasonality plot
seasons %>% 
  ggplot(aes(week_of_year, Mean, color = city, group = city)) +
  geom_line() +
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.2) +
  facet_wrap(~ key, scales = "free") +
  scale_color_viridis(discrete = TRUE)

# Dengai ------------------------------------------------------------------

#' Setup base model
dengue_nested <- dengue_nested %>% 
  mutate(mod = map(train, 
                   prophet, 
                   weekly.seasonality = FALSE,
                   daily.seasonality = FALSE,
                   growth = 'logistic',
                   seasonality.prior.scale = 400,
                   changepoint.prior.scale = 1,
                   # mcmc.samples = 4,
                   # uncertainty.samples = 4000,
                   interval.width = 0.95,
                   fit = FALSE))

#' Add extra regressors to model
regressors <- dengue_nested$train[[1]] %>% 
  colnames() %>% 
  setdiff(c("y", "ds", "cap", "floor"))
args_add_reg <- map(regressors, ~ quo(map(mod, add_regressor, name = !! .x))) %>% 
  set_names(glue('mod_{regressors}'))
dengue_nested <- dengue_nested %>% mutate(!!! args_add_reg)

#' Fit models
models <- c("mod", glue('mod_{regressors}'))
args_fit <- map(models, ~ quo(map2(!!! sym(.x), train, fit.prophet))) %>%
  set_names(models)
dengue_nested <- dengue_nested %>% mutate(!!! args_fit)

#' Add predictions
args_preds <-  map(models, ~ quo(map2(!!! sym(.x), test, predict))) %>%
  set_names(glue("{models}_pred"))
dengue_nested <- dengue_nested %>% mutate(!!! args_preds)

test_unnested <- dengue_nested %>%
  select(city, test) %>% 
  unnest() %>% 
  select(city, ds, y)

#' Calculate residuals
dengeue_resid <- dengue_nested %>% 
  select(city, ends_with("pred")) %>% 
  gather(key = model, value, -city) %>% 
  unnest() %>% 
  mutate(ds = ymd(ds)) %>% 
  select(city, model, ds, starts_with("yhat")) %>% 
  left_join(test_unnested) %>% 
  # mutate_at(vars(starts_with("y")), ~ exp(.x) - 1) %>%
  mutate(resid = yhat - y)

#' RMSE
dengeue_resid %>% 
  group_by(city, model) %>% 
  summarise(rmse = sqrt(sum(resid^2) / n())) %>% 
  arrange(rmse) %>% 
  top_n(5, 1 / rmse)

#' Plot predictions
dengeue_resid %>% 
  ggplot(aes(ds, yhat, group = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.1) +
  geom_point(data = test_unnested, aes(ds, y, group = 1), color = "blue") +
  facet_wrap(~ city, scales = "free")
