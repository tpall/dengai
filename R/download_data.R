
library(tidyverse)

# Download data
train <- read_csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_features_train.csv")
train_y <- read_csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_labels_train.csv")
write_csv(train, "data/dengue_features_train.csv")
write_csv(train_y, "data/dengue_labels_train.csv")

test <- read_csv("https://s3.amazonaws.com/drivendata/data/44/public/dengue_features_test.csv")
write_csv(test, "data/dengue_features_test.csv")

submit <- read_csv("https://s3.amazonaws.com/drivendata/data/44/public/submission_format.csv")
write_csv(submit, "data/submission_format.csv")

