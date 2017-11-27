
library(tidyverse)

train_features <- read_csv("data/dengue_features_train.csv")
train_labs <- read_csv("data/dengue_labels_train.csv")

train <- left_join(train_labs, train_features)

train_long <- train %>%
  gather(key, value, -c(1:3, 5))
