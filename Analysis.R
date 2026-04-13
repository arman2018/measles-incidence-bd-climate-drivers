#My directory
setwd("D:/Research/Measles")

#Required packages
# Core
library(readxl)
library(tidyverse)
library(data.table)

# Visualization
library(ggplot2)
library(gridExtra)
library(corrplot)

# ML
library(caret)
library(randomForest)
library(xgboost)
library(lightgbm)

# Explainability
library(SHAPforxgboost)
library(shapviz)

# Time series
library(forecast)
library(tseries)

# Metrics
library(Metrics)

#Load data
data<-read_excel("Data.xlsx",sheet = "Measles data")

# Check structure
str(data)
data$Cases<-as.numeric(data$Cases)
summary(data)
sum(data$Cases)

# Convert to data.table
data <- as.data.table(data)

#Summary statistics
summary_stats <- data %>%
  summarise(across(c(Cases, T2M, T2M_MAX, T2M_MIN, RH2M, PREP, WS2M, Population),
                   list(mean = mean, sd = sd, min = min, max = max)))

print(summary_stats)

#Time series plot
ggplot(data, aes(x = Year, y = Cases)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "", y = "Cases") +
  theme_minimal()

#Climate trends
vars <- c("T2M","T2M_MAX","T2M_MIN", "PREP", "RH2M","WS2M")

plots <- lapply(vars, function(v) {
  ggplot(data, aes_string(x = "Year", y = v)) +
    geom_line() +
    theme_minimal() +
    ggtitle(v)
})

grid.arrange(grobs = plots, ncol = 3)

#Correlation analysis
data_corr <- data[, -c(1, 9)]

# Compute correlation matrix
cor_matrix <- cor(data_corr, use = "complete.obs")

# Plot
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",     # show correlation values
  tl.col = "black",          # label color
  tl.srt = 45,               # rotate labels
  tl.cex = 0.9,              # label size
  number.cex = 0.7,          # correlation number size
  col = colorRampPalette(c("blue", "white", "red"))(200),
  diag = FALSE
)

#Feature engineering
#Incidence rate
data$Incidence <- (data$Cases / data$Population) * 100000

#Train-test split (time based)
train <- data[data$Year <= 2015, ]
test  <- data[data$Year > 2015, ]

#Machine learning models
#data preparation
features <- c("T2M", "T2M_MAX", "T2M_MIN", "RH2M", "PREP", "WS2M")

x_train <- as.matrix(train[, ..features])
y_train <- train$Incidence

x_test <- as.matrix(test[, ..features])
y_test <- test$Incidence

#Random forest
set.seed(123)

rf_model <- randomForest(x = x_train, y = y_train,
                         ntree = 500,
                         importance = TRUE)

rf_pred <- predict(rf_model, x_test)

# Importance
varImpPlot(rf_model)

#XGBoost model
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test, label = y_test)

params <- list(
  objective = "reg:squarederror",
  eta = 0.01,
  max_depth = 3,
  subsample = 0.1,
  colsample_bytree = 0.1
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain),
  verbose = 0
)

xgb_pred <- predict(xgb_model, dtest)

#LightGBM model
lgb_train <- lgb.Dataset(data = x_train, label = y_train)

params_lgb <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 5,
  min_data_in_leaf = 15,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 1
)

lgb_model <- lgb.train(
  params_lgb,
  lgb_train,
  nrounds = 1000
)

lgb_pred <- predict(lgb_model, x_test)

#Model evaluation
evaluate <- function(true, pred){
  c(
    RMSE = rmse(true, pred),
    MAE  = mae(true, pred),
    MAPE = mape(true, pred)
  )
}

rf_eval  <- evaluate(y_test, rf_pred)
xgb_eval <- evaluate(y_test, xgb_pred)
lgb_eval <- evaluate(y_test, lgb_pred)

results <- rbind(RF = rf_eval, XGB = xgb_eval, LGB = lgb_eval)
print(results)



#Compute shap values from LightGBM
colnames(x_train) <- features

# SHAP values (correct)
shap_values <- predict(
  lgb_model,
  x_train,
  type = "contrib"
)

shap_values <- as.matrix(shap_values)

# Remove baseline column
shap_matrix <- shap_values[, -ncol(shap_values)]

# Assign feature names
colnames(shap_matrix) <- features

X_df <- as.data.frame(x_train)
colnames(X_df) <- features

sv <- shapviz(
  shap_matrix,
  X = X_df
)

sv_importance(sv)
sv_importance(sv, kind = "bee")
sv_dependence(sv, "Population")
