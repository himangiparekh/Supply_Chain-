install.packages('Rcpp')

library(dplyr)
library(haven)
library(labelled)
library(coop)
library(stringr)
library(digitTests)
library(gamlr)
library(glmnet)

y <- data_frame$f1

df_analysislasso <- df_analysis[-c(7)]
x <- data.matrix(data_frame[, colnames(df_analysislasso)])

cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
