library(tidyverse)

setwd("C:/Users/ryanwade44/Documents/UKPBI/Script")
model <- readRDS("./Models/model_R.rds")
boston_housing <- read_csv("./Data/BostonHousingInfo.csv")

model_data <- boston_housing[, c("crim","rm","tax","lstat")]
pred_medv <- predict(model, model_data)

final_output <- cbind(model_data, pred_medv)
