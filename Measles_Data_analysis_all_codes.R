# Measles Cases Trend Visualization 

library("readxl")
library("ggplot2")
library("scales")

# 1. File paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/Time-series analysis/Trend visualization"

# 2. Import and prepare data
data <- read_excel(data_path, sheet = 1)
data <- data[, c("Year", "Cases")]
data$Year  <- as.integer(data$Year)
data$Cases <- as.numeric(data$Cases)
data <- data[order(data$Year), ]

# 3. Build the trend plot
p <- ggplot(data, aes(x = Year, y = Cases)) +
  geom_line(color = "#2CA87F", linewidth = 0.7) +
  geom_point(color = "#2CA87F", size = 1.9, shape = 16) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), labels = comma,
                     expand = expansion(mult = c(0.03, 0.08))) +
  labs(x = "Year", y = "Measles Cases") +
  theme_bw(base_size = 12, base_family = "serif") +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey85", linewidth = 0.3),
    panel.border      = element_rect(color = "black", linewidth = 0.6, fill = NA),
    axis.title.x      = element_text(face = "bold", size = 13, color = "black", margin = margin(t = 8)),
    axis.title.y      = element_text(face = "bold", size = 13, color = "black", margin = margin(r = 8)),
    axis.text         = element_text(size = 11, color = "black"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    axis.ticks        = element_line(color = "black", linewidth = 0.4),
    plot.margin       = margin(t = 10, r = 15, b = 10, l = 10)
  )

print(p)

# 4. Export 
ggsave(file.path(output_path, "Measles_Trend_Plot.tiff"),
       plot = p, width = 180, height = 100, units = "mm",
       dpi = 600, compression = "lzw")
ggsave(file.path(output_path, "Measles_Trend_Plot.png"),
       plot = p, width = 180, height = 100, units = "mm", dpi = 300)



# Spearman Correlation Matrix

library("readxl")
library("dplyr")
library("stringr")
library("corrplot")
library("RColorBrewer")
library("openxlsx")
library("ggcorrplot")

# 1. Paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Analysis.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/Check/Corr"

# 2. Read and clean data
data <- read_excel(data_path, guess_max = 100000)
names(data) <- gsub("\\s+", "_", str_trim(names(data)))

# 3. Define dependent and independent variables
dependent_var <- "Cases"

independent_vars <- c(
  "T2M", "T2M_MAX", "T2M_MIN", "RH2M", "PREP", "WS2M",
  "Population", "Pop_density", "Pop_growth",
  "Pop_aged_0_4", "Pop_aged_5_9",
  "Vac_cov", "Net_migration", "Air_passengers",
  "GDP", "GNI", "CPI", "Poverty_rate",
  "Literacy_rate", "Pri_comp_rate", "Sec_comp_rate",
  "THE", "Dispensary", "Hosp_beds", "Reg_doc", "Reg_nurse", "UHC"
)

all_vars <- c(dependent_var, independent_vars)

missing_vars <- setdiff(all_vars, names(data))
if (length(missing_vars) > 0) {
  stop("The following variables were not found in the dataset: ",
       paste(missing_vars, collapse = ", "))
}

# 4. Display labels (short codes)
display_labels <- c(
  Cases            = "Measles_Cases",
  T2M              = "A1",
  T2M_MAX          = "A2",
  T2M_MIN          = "A3",
  RH2M             = "A4",
  PREP             = "A5",
  WS2M             = "A6",
  Population       = "B1",
  Pop_density      = "B2",
  Pop_growth       = "B3",
  Pop_aged_0_4     = "B4",
  Pop_aged_5_9     = "B5",
  Vac_cov          = "C1",
  Net_migration    = "D1",
  Air_passengers   = "D2",
  GDP              = "E1",
  GNI              = "E2",
  CPI              = "E3",
  Poverty_rate     = "E4",
  Literay_rate     = "F1",
  Pri_comp_rate    = "F2",
  Sec_comp_rate    = "F3",
  THE              = "G1",
  Dispensary       = "G2",
  Hosp_beds        = "G3",
  Reg_doc          = "G4",
  Reg_nurse        = "G5",
  UHC              = "G6"
)

# 5. Select variables and force numeric coercion
cor_data <- data %>% select(all_of(all_vars))

col_classes <- sapply(cor_data, class)
print(col_classes)


zero_var_cols <- names(cor_data)[sapply(cor_data, function(x) {
  v <- x[!is.na(x)]
  length(unique(v)) <= 1
})]
if (length(zero_var_cols) > 0) {
  cat("\n\u26a0 Zero-variance column(s) detected (correlation undefined):\n")
  print(zero_var_cols)
}

# 6. Compute Spearman correlation matrix and p-values
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = "spearman")

p_mat <- suppressWarnings(
  ggcorrplot::cor_pmat(cor_data, method = "spearman", exact = FALSE)
)
colnames(cor_matrix) <- display_labels[colnames(cor_matrix)]
rownames(cor_matrix) <- display_labels[rownames(cor_matrix)]
colnames(p_mat) <- display_labels[colnames(p_mat)]
rownames(p_mat) <- display_labels[rownames(p_mat)]

# 7. Build the correlation plot
diverging_col <- colorRampPalette(c("#67001F", "#B2182B", "#F7F7F7",
                                    "#2166AC", "#053061"))(200)

draw_corrplot <- function() {
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xpd = TRUE)
  corrplot(
    cor_matrix,
    method        = "circle",
    type          = "upper",
    diag          = FALSE,
    order         = "original",
    col           = diverging_col,   # red = negative, white = 0, blue = positive
    col.lim       = c(-1, 1),
    is.corr       = TRUE,
    addgrid.col   = "grey85",
    tl.col        = "black",
    tl.font       = 18,
    tl.srt        = 90,
    tl.cex        = 1.8,
    tl.offset     = 0.4,
    cl.pos        = "r",
    cl.cex        = 1.2,
    cl.ratio      = 0.10,
    cl.align.text = "l",
    mar           = c(0, 0, 0, 0)
  )
}

# 8. Export 
tiff(
  filename = file.path(output_path, "Spearman_Correlation_Plot.tiff"),
  width = 17, height = 17, units = "in",
  res = 600, compression = "lzw"
)
draw_corrplot()
dev.off()

png(
  filename = file.path(output_path, "Spearman_Correlation_Plot.png"),
  width = 17, height = 17, units = "in", res = 400
)
draw_corrplot()
dev.off()

draw_corrplot()

# 9. Export correlation and p-value matrices to Excel
mask_upper_triangle <- function(m) {
  m_masked <- m
  m_masked[lower.tri(m_masked, diag = TRUE)] <- NA
  m_masked
}

cor_matrix_tri <- mask_upper_triangle(cor_matrix)
p_mat_tri       <- mask_upper_triangle(p_mat)

wb <- createWorkbook()
addWorksheet(wb, "Spearman_Correlation")
addWorksheet(wb, "P_values")
writeData(wb, "Spearman_Correlation", cor_matrix_tri, rowNames = TRUE, na.string = "")
writeData(wb, "P_values", p_mat_tri, rowNames = TRUE, na.string = "")
saveWorkbook(wb, file.path(output_path, "Spearman_Correlation_and_Pvalues.xlsx"), overwrite = TRUE)


# Violin Plot 

#Library
library("readxl")
library("tidyverse")
library("ggplot2")
library("patchwork")

# 1. paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data preprocessing/Imputation/Log+Normalized_Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/Check/Violin_plot"


# 2. Import and clean data
data <- read_excel(data_path, sheet = 1)


# 3. Variable groups
group_list <- list(
  "A. Climatic factors"        = c("T2M", "T2M_MAX", "T2M_MIN", "RH2M", "PREP", "WS2M"),
  "B. Demographic factors"     = c("Population", "Pop_density", "Pop_growth", "Pop_aged_0_4", "Pop_aged_5_9"),
  "C. Immunization"            = c("Vac_cov"),
  "D. Mobility factors"        = c("Net_migration", "Air_passengers"),
  "E. Socio-economic factors"  = c("GDP", "GNI", "CPI", "Poverty_rate"),
  "F. Educational factors"     = c("Literacy_rate", "Pri_comp_rate", "Sec_comp_rate"),
  "G. Healthcare system factors" = c("THE", "Dispensary", "Hosp_beds", "Reg_doc", "Reg_nurse", "UHC")
)


# 4. Display labels 
label_list <- list(
  "A. Climatic factors"          = c(T2M = "A1", T2M_MAX = "A2", T2M_MIN = "A3",
                                     RH2M = "A4", PREP = "A5", WS2M = "A6"),
  "B. Demographic factors"       = c(Population = "B1", Pop_density = "B2", Pop_growth = "B3",
                                     Pop_aged_0_4 = "B4", Pop_aged_5_9 = "B5"),
  "C. Immunization"              = c(Vac_cov = "C1"),
  "D. Mobility factors"          = c(Net_migration = "D1", Air_passengers = "D2"),
  "E. Socio-economic factors"    = c(GDP = "E1", GNI = "E2", CPI = "E3", Poverty_rate = "E4"),
  "F. Educational factors"       = c(Literacy_rate = "F1", Pri_comp_rate = "F2", Sec_comp_rate = "F3"),
  "G. Healthcare system factors" = c(THE = "G1", Dispensary = "G2", Hosp_beds = "G3",
                                     Reg_doc = "G4", Reg_nurse = "G5", UHC = "G6")
)

# 5. Colour palette per panel
palette_list <- list(
  "A. Climatic factors"          = c("#2E75B6", "#4F94C7", "#7FB3D8", "#1F5C8B", "#8FC1DE", "#B7D9E8"),
  "B. Demographic factors"       = c("#4C8C3B", "#8FBF3F", "#2D5A27", "#D98A1F", "#E8B168"),
  "C. Immunization"              = c("#B22222"),
  "D. Mobility factors"          = c("#E8963A", "#E06A4E"),
  "E. Socio-economic factors"    = c("#7A3FA0", "#9B5FC0", "#B888D6", "#5E2B85"),
  "F. Educational factors"       = c("#1FA090", "#2ED9B0", "#0E6E62"),
  "G. Healthcare system factors" = c("#E8552A", "#2E2E2E", "#F4C542", "#9BC948", "#3AA88F", "#3D7A78")
)

# 6. Function to build one violin panel
make_violin_panel <- function(vars, title, colours, labels) {
  
  plot_df <- data %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    mutate(Variable = factor(Variable, levels = vars, labels = labels[vars]))
  
  p <- ggplot(plot_df, aes(x = Variable, y = Value, fill = Variable)) +
    geom_violin(trim = TRUE, colour = "black", linewidth = 0.45, width = 0.9, alpha = 0.95) +
    geom_boxplot(width = 0.12, fill = "white", colour = "black",
                 outlier.shape = 21, outlier.size = 1.3, outlier.fill = "grey30",
                 linewidth = 0.4) +
    scale_fill_manual(values = colours) +
    labs(title = title, x = NULL, y = NULL) +
    theme_classic(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 20, hjust = 0.5,
                                      colour = "black", margin = margin(b = 9)),
      axis.text.x      = element_text(face = "bold", angle = 0, hjust = 0.5, vjust = 1,
                                      size = 22, colour = "black"),
      axis.text.y      = element_text(face = "bold", size = 14, colour = "black"),
      axis.line        = element_line(colour = "black", linewidth = 0.5),
      axis.ticks       = element_line(colour = "black", linewidth = 0.4),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor  = element_blank(),
      legend.position   = "none",
      plot.margin       = margin(t = 8, r = 10, b = 6, l = 10)
    )
  
  return(p)
}

# 7. Build each panel
panel_A <- make_violin_panel(
  group_list[["A. Climatic factors"]], "A. Climatic factors",
  palette_list[["A. Climatic factors"]], label_list[["A. Climatic factors"]]
)
panel_B <- make_violin_panel(
  group_list[["B. Demographic factors"]], "B. Demographic factors",
  palette_list[["B. Demographic factors"]], label_list[["B. Demographic factors"]]
)
panel_C <- make_violin_panel(
  group_list[["C. Immunization"]], "C. Immunization",
  palette_list[["C. Immunization"]], label_list[["C. Immunization"]]
)
panel_D <- make_violin_panel(
  group_list[["D. Mobility factors"]], "D. Mobility factors",
  palette_list[["D. Mobility factors"]], label_list[["D. Mobility factors"]]
)
panel_E <- make_violin_panel(
  group_list[["E. Socio-economic factors"]], "E. Socio-economic factors",
  palette_list[["E. Socio-economic factors"]], label_list[["E. Socio-economic factors"]]
)
panel_F <- make_violin_panel(
  group_list[["F. Educational factors"]], "F. Educational factors",
  palette_list[["F. Educational factors"]], label_list[["F. Educational factors"]]
)
panel_G <- make_violin_panel(
  group_list[["G. Healthcare system factors"]], "G. Healthcare system factors",
  palette_list[["G. Healthcare system factors"]], label_list[["G. Healthcare system factors"]]
)

# 8. Compose full figure 
layout <- "
AA
BC
DE
FG
"

final_figure <- panel_A + panel_B + panel_C + panel_D + panel_E + panel_F + panel_G +
  plot_layout(design = layout, widths = c(1, 1.3), heights = c(1, 1, 1, 1))

# 9. Export 
ggsave(
  filename = file.path(output_path, "Figure_Violinplots.png"),
  plot     = final_figure,
  width    = 14, height = 18, units = "in", dpi = 600, bg = "white"
)

ggsave(
  filename = file.path(output_path, "Figure_Violinplots.tiff"),
  plot     = final_figure,
  width    = 14, height = 18, units = "in", dpi = 600, bg = "white",
  compression = "lzw"
)



#ML Modeling

# XGBoost 

library("readxl")
library("openxlsx")
library("dplyr")
library("purrr")
library("tibble")
library("xgboost")

set.seed(2026)

# 1. Paths 
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/ML/XGBoost"

# 2. Load and clean data
data <- read_excel(data_path, sheet = 1)
data <- data[!is.na(data$Year), ]
data$Cases <- as.numeric(data$Cases)
data <- as.data.frame(data[order(data$Year), ])

stopifnot(all(diff(data$Year) == 1))
cat("Loaded", nrow(data), "rows, years", min(data$Year), "-", max(data$Year), "\n")

# 3. Predictor sets, Model 1-8 
model_vars <- list(
  Model1 = c("T2M","RH2M","PREP","WS2M"),
  Model2 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4"),
  Model3 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov"),
  Model4 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers"),
  Model5 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate"),
  Model6 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate"),
  Model7 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds"),
  Model8 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds","T2M_lag1","RH2M_lag1","PREP_lag1","Vac_cov_lag1")
)

# 4. Outer rolling-origin folds 
folds <- list(
  A = list(train = 1981:2010, test = 2011:2013),
  B = list(train = 1981:2013, test = 2014:2016),
  C = list(train = 1981:2016, test = 2017:2019),
  D = list(train = 1981:2019, test = 2020:2022),
  E = list(train = 1981:2022, test = 2023:2025)
)

# 5. Metric functions 
rmse_fn <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae_fn  <- function(actual, pred) mean(abs(actual - pred))
mape_fn <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

# 6. Nested inner-CV folds for hyperparameter selection 
make_inner_folds <- function(train_years, n_test_inner = 3, n_folds_inner = 3) {
  ty <- sort(train_years)
  L  <- length(ty)
  needed <- n_folds_inner * n_test_inner
  if (L < needed + 3) n_folds_inner <- max(1, floor((L - 3) / n_test_inner))
  out <- list()
  for (k in seq_len(n_folds_inner)) {
    cut <- L - (n_folds_inner - k + 1) * n_test_inner
    if (cut < 3) next
    fit_idx <- 1:cut
    val_idx <- (cut + 1):min(cut + n_test_inner, L)
    out[[length(out) + 1]] <- list(fit = ty[fit_idx], val = ty[val_idx])
  }
  out
}

# 7. Hyperparameter grid 
hp_grid <- expand.grid(
  max_depth = c(2, 3),
  eta       = c(0.05, 0.1),
  nrounds   = c(50, 100, 200),
  min_child_weight = 3
)
FIXED <- list(subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 3,
              lambda = 1, alpha = 0.1)

score_config <- function(train_df, vars, inner_folds, max_depth, eta, nrounds, seed = 2026) {
  rmses <- numeric(0)
  for (f in inner_folds) {
    fit_d <- train_df[train_df$Year %in% f$fit, ]
    val_d <- train_df[train_df$Year %in% f$val, ]
    if (nrow(fit_d) < 5 || nrow(val_d) == 0) next
    dfit <- xgb.DMatrix(data = as.matrix(fit_d[, vars, drop = FALSE]), label = fit_d$Cases)
    dval <- xgb.DMatrix(data = as.matrix(val_d[, vars, drop = FALSE]), label = val_d$Cases)
    params <- c(list(objective = "reg:squarederror", eval_metric = "rmse",
                     max_depth = max_depth, eta = eta,
                     base_score = mean(fit_d$Cases)), FIXED)
    set.seed(seed)
    m <- xgb.train(params = params, data = dfit, nrounds = nrounds, verbose = 0)
    pred <- predict(m, dval)
    rmses <- c(rmses, sqrt(mean((val_d$Cases - pred)^2)))
  }
  if (length(rmses) == 0) return(Inf)
  mean(rmses)
}

# 8. Core fit function
fit_one <- function(data, vars, train_years, test_years, seed = 2026) {
  
  df <- data[data$Year %in% c(train_years, test_years), c("Year", "Cases", vars)]
  df <- df[stats::complete.cases(df), ]   # drops Model 8's 1981 lag-NA row
  
  train_df <- df[df$Year %in% train_years, ]
  test_df  <- df[df$Year %in% test_years, ]
  
  if (nrow(train_df) < 10 || nrow(test_df) == 0) return(NULL)
  
  inner_folds <- make_inner_folds(train_df$Year)
  
  best <- NULL
  for (i in seq_len(nrow(hp_grid))) {
    g  <- hp_grid[i, ]
    sc <- score_config(train_df, vars, inner_folds, g$max_depth, g$eta, g$nrounds, seed)
    if (is.null(best) || sc < best$score) {
      best <- list(max_depth = g$max_depth, eta = g$eta, nrounds = g$nrounds, score = sc)
    }
  }
  
  final_params <- c(list(objective = "reg:squarederror", eval_metric = "rmse",
                         max_depth = best$max_depth, eta = best$eta,
                         base_score = mean(train_df$Cases)), FIXED)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_df[, vars, drop = FALSE]), label = train_df$Cases)
  dtest  <- xgb.DMatrix(data = as.matrix(test_df[, vars, drop = FALSE]),  label = test_df$Cases)
  
  set.seed(seed)
  final_model <- xgb.train(params = final_params, data = dtrain, nrounds = best$nrounds, verbose = 0)
  
  pred_train <- predict(final_model, dtrain)
  pred_test  <- predict(final_model, dtest)
  
  metrics <- tibble(
    n_train         = nrow(train_df),
    n_test          = nrow(test_df),
    Train_RMSE      = rmse_fn(train_df$Cases, pred_train),
    Train_MAE       = mae_fn(train_df$Cases, pred_train),
    Train_MAPE      = mape_fn(train_df$Cases, pred_train),
    Test_RMSE       = rmse_fn(test_df$Cases, pred_test),
    Test_MAE        = mae_fn(test_df$Cases, pred_test),
    Test_MAPE       = mape_fn(test_df$Cases, pred_test),
    best_max_depth  = best$max_depth,
    best_eta        = best$eta,
    best_nrounds    = best$nrounds,
    inner_cv_rmse   = best$score
  )
  
  preds <- bind_rows(
    tibble(Year = train_df$Year, Set = "Train", Actual = train_df$Cases, Predicted = pred_train),
    tibble(Year = test_df$Year,  Set = "Test",  Actual = test_df$Cases,  Predicted = pred_test)
  )
  
  list(metrics = metrics, preds = preds)
}

# 9. Run pipeline across all folds x all models 
all_metrics <- list()
all_preds   <- list()

for (fold_name in names(folds)) {
  train_years <- folds[[fold_name]]$train
  test_years  <- folds[[fold_name]]$test
  
  fold_metrics <- list()
  fold_preds   <- list()
  
  for (mname in names(model_vars)) {
    res <- fit_one(data, model_vars[[mname]], train_years, test_years)
    if (is.null(res)) {
      warning(sprintf("Fold %s / %s produced no result (insufficient rows) - skipped.", fold_name, mname))
      next
    }
    m <- res$metrics
    m$Fold        <- fold_name
    m$Model       <- mname
    m$Train_Years <- paste0(min(train_years), "-", max(train_years))
    m$Test_Years  <- paste0(min(test_years), "-", max(test_years))
    fold_metrics[[mname]] <- m
    
    p <- res$preds
    p$Fold  <- fold_name
    p$Model <- mname
    fold_preds[[mname]] <- p
  }
  
  fold_metrics_df <- bind_rows(fold_metrics) %>%
    select(Fold, Model, Train_Years, Test_Years, n_train, n_test,
           Train_RMSE, Train_MAE, Train_MAPE,
           Test_RMSE, Test_MAE, Test_MAPE,
           best_max_depth, best_eta, best_nrounds, inner_cv_rmse)
  
  fold_preds_df <- bind_rows(fold_preds) %>%
    select(Fold, Model, Year, Set, Actual, Predicted)
  
  all_metrics[[fold_name]] <- fold_metrics_df
  all_preds[[fold_name]]   <- fold_preds_df
  
  fname <- file.path(output_path, paste0("Model ", fold_name, ".xlsx"))
  write.xlsx(
    list(Metrics = fold_metrics_df, Predictions = fold_preds_df),
    fname, overwrite = TRUE
  )
}


# 10. Combined workbook across all folds 
combined_metrics <- bind_rows(all_metrics)

write.xlsx(
  list(
    All_Folds_Metrics   = combined_metrics,
    All_Predictions     = combined_preds
  ),
  file.path(output_path, "XGBoost_All_Models_Combined.xlsx"),
  overwrite = TRUE
)



# Random Forest

library("readxl")
library("openxlsx")
library("dplyr")
library("purrr")
library("tibble")
library("randomForest")

set.seed(2026)

# 1. Paths 
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/ML/RF_3"

# 2. Load and clean data 
data <- read_excel(data_path, sheet = 1)
data <- data[!is.na(data$Year), ]
data$Cases <- as.numeric(data$Cases)
data <- as.data.frame(data[order(data$Year), ])

stopifnot(all(diff(data$Year) == 1))
cat("Loaded", nrow(data), "rows, years", min(data$Year), "-", max(data$Year), "\n")

# 3. Predictor sets, Model 1-8 
model_vars <- list(
  Model1 = c("T2M","RH2M","PREP","WS2M"),
  Model2 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4"),
  Model3 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov"),
  Model4 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers"),
  Model5 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate"),
  Model6 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate"),
  Model7 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds"),
  Model8 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds","T2M_lag1","RH2M_lag1","PREP_lag1","Vac_cov_lag1")
)

# 4. Outer rolling-origin folds 
folds <- list(
  A = list(train = 1981:2010, test = 2011:2013),
  B = list(train = 1981:2013, test = 2014:2016),
  C = list(train = 1981:2016, test = 2017:2019),
  D = list(train = 1981:2019, test = 2020:2022),
  E = list(train = 1981:2022, test = 2023:2025)
)

# 5. Metric functions 
rmse_fn <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae_fn  <- function(actual, pred) mean(abs(actual - pred))
mape_fn <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

# 6. Nested inner-CV folds for hyperparameter selection 
make_inner_folds <- function(train_years, n_test_inner = 3, n_folds_inner = 3) {
  ty <- sort(train_years)
  L  <- length(ty)
  needed <- n_folds_inner * n_test_inner
  if (L < needed + 3) n_folds_inner <- max(1, floor((L - 3) / n_test_inner))
  out <- list()
  for (k in seq_len(n_folds_inner)) {
    cut <- L - (n_folds_inner - k + 1) * n_test_inner
    if (cut < 3) next
    fit_idx <- 1:cut
    val_idx <- (cut + 1):min(cut + n_test_inner, L)
    out[[length(out) + 1]] <- list(fit = ty[fit_idx], val = ty[val_idx])
  }
  out
}

# 7. Hyperparameter grid (mtry, ntree, nodesize) 
make_hp_grid <- function(p) {
  mtry_candidates <- sort(unique(pmax(1, round(c(p / 3, sqrt(p), p / 2)))))
  expand.grid(
    mtry     = mtry_candidates,
    ntree    = c(500, 1000),
    nodesize = c(3, 5)
  )
}

score_config <- function(train_df, vars, inner_folds, mtry, ntree, nodesize, seed = 2026) {
  rmses <- numeric(0)
  for (f in inner_folds) {
    fit_d <- train_df[train_df$Year %in% f$fit, ]
    val_d <- train_df[train_df$Year %in% f$val, ]
    if (nrow(fit_d) < 5 || nrow(val_d) == 0) next
    set.seed(seed)
    m <- randomForest(
      x = fit_d[, vars, drop = FALSE], y = fit_d$Cases,
      mtry = mtry, ntree = ntree, nodesize = nodesize
    )
    pred <- predict(m, val_d[, vars, drop = FALSE])
    rmses <- c(rmses, sqrt(mean((val_d$Cases - pred)^2)))
  }
  if (length(rmses) == 0) return(Inf)
  mean(rmses)
}

# 8. Core fit function
fit_one <- function(data, vars, train_years, test_years, seed = 2026) {
  
  df <- data[data$Year %in% c(train_years, test_years), c("Year", "Cases", vars)]
  df <- df[stats::complete.cases(df), ]   # drops Model 8's 1981 lag-NA row
  
  train_df <- df[df$Year %in% train_years, ]
  test_df  <- df[df$Year %in% test_years, ]
  
  if (nrow(train_df) < 10 || nrow(test_df) == 0) return(NULL)
  
  inner_folds <- make_inner_folds(train_df$Year)
  hp_grid     <- make_hp_grid(length(vars))
  
  best <- NULL
  for (i in seq_len(nrow(hp_grid))) {
    g  <- hp_grid[i, ]
    sc <- score_config(train_df, vars, inner_folds, g$mtry, g$ntree, g$nodesize, seed)
    if (is.null(best) || sc < best$score) {
      best <- list(mtry = g$mtry, ntree = g$ntree, nodesize = g$nodesize, score = sc)
    }
  }
  
  set.seed(seed)
  final_model <- randomForest(
    x = train_df[, vars, drop = FALSE], y = train_df$Cases,
    mtry = best$mtry, ntree = best$ntree, nodesize = best$nodesize
  )
  
  pred_train <- predict(final_model, train_df[, vars, drop = FALSE])
  pred_test  <- predict(final_model, test_df[, vars, drop = FALSE])
  
  metrics <- tibble(
    n_train         = nrow(train_df),
    n_test          = nrow(test_df),
    Train_RMSE      = rmse_fn(train_df$Cases, pred_train),
    Train_MAE       = mae_fn(train_df$Cases, pred_train),
    Train_MAPE      = mape_fn(train_df$Cases, pred_train),
    Test_RMSE       = rmse_fn(test_df$Cases, pred_test),
    Test_MAE        = mae_fn(test_df$Cases, pred_test),
    Test_MAPE       = mape_fn(test_df$Cases, pred_test),
    best_mtry       = best$mtry,
    best_ntree      = best$ntree,
    best_nodesize   = best$nodesize,
    inner_cv_rmse   = best$score
  )
  
  preds <- bind_rows(
    tibble(Year = train_df$Year, Set = "Train", Actual = train_df$Cases, Predicted = pred_train),
    tibble(Year = test_df$Year,  Set = "Test",  Actual = test_df$Cases,  Predicted = pred_test)
  )
  
  list(metrics = metrics, preds = preds)
}

# 9. Run pipeline across all folds x all models 
all_metrics <- list()
all_preds   <- list()

for (fold_name in names(folds)) {
  train_years <- folds[[fold_name]]$train
  test_years  <- folds[[fold_name]]$test
  
  fold_metrics <- list()
  fold_preds   <- list()
  
  for (mname in names(model_vars)) {
    res <- fit_one(data, model_vars[[mname]], train_years, test_years)
    if (is.null(res)) {
      warning(sprintf("Fold %s / %s produced no result (insufficient rows) - skipped.", fold_name, mname))
      next
    }
    m <- res$metrics
    m$Fold        <- fold_name
    m$Model       <- mname
    m$Train_Years <- paste0(min(train_years), "-", max(train_years))
    m$Test_Years  <- paste0(min(test_years), "-", max(test_years))
    fold_metrics[[mname]] <- m
    
    p <- res$preds
    p$Fold  <- fold_name
    p$Model <- mname
    fold_preds[[mname]] <- p
  }
  
  fold_metrics_df <- bind_rows(fold_metrics) %>%
    select(Fold, Model, Train_Years, Test_Years, n_train, n_test,
           Train_RMSE, Train_MAE, Train_MAPE,
           Test_RMSE, Test_MAE, Test_MAPE,
           best_mtry, best_ntree, best_nodesize, inner_cv_rmse)
  
  fold_preds_df <- bind_rows(fold_preds) %>%
    select(Fold, Model, Year, Set, Actual, Predicted)
  
  all_metrics[[fold_name]] <- fold_metrics_df
  all_preds[[fold_name]]   <- fold_preds_df
  
  fname <- file.path(output_path, paste0("Model ", fold_name, ".xlsx"))
  write.xlsx(
    list(Metrics = fold_metrics_df, Predictions = fold_preds_df),
    fname, overwrite = TRUE
  )
}

# 10. Combined workbook across all folds 
combined_metrics <- bind_rows(all_metrics)
combined_preds   <- bind_rows(all_preds)

write.xlsx(
  list(
    All_Folds_Metrics = combined_metrics,
    All_Predictions   = combined_preds
  ),
  file.path(output_path, "RF_All_Models_Combined.xlsx"),
  overwrite = TRUE
)



#LightGBM 

library("readxl")
library("openxlsx")
library("dplyr")
library("purrr")
library("tibble")
library("lightgbm")
library("ggplot2")
library("ggbeeswarm")
library("patchwork")

set.seed(2026)

# 1. Paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/ML/LightGBM_1"

# 2. Load and clean data
data <- read_excel(data_path, sheet = 1)
data <- data[!is.na(data$Year), ]
data$Cases <- as.numeric(data$Cases)
data <- as.data.frame(data[order(data$Year), ])

stopifnot(all(diff(data$Year) == 1))
cat("Loaded", nrow(data), "rows, years", min(data$Year), "-", max(data$Year), "\n")

# 3. Predictor sets, Model 1-8 
model_vars <- list(
  Model1 = c("T2M","RH2M","PREP","WS2M"),
  Model2 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4"),
  Model3 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov"),
  Model4 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers"),
  Model5 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate"),
  Model6 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate"),
  Model7 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds"),
  Model8 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds","T2M_lag1","RH2M_lag1","PREP_lag1","Vac_cov_lag1")
)

# 4. Outer rolling-origin folds 
folds <- list(
  A = list(train = 1981:2010, test = 2011:2013),
  B = list(train = 1981:2013, test = 2014:2016),
  C = list(train = 1981:2016, test = 2017:2019),
  D = list(train = 1981:2019, test = 2020:2022),
  E = list(train = 1981:2022, test = 2023:2025)
)

# 5. Metric functions 
rmse_fn <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae_fn  <- function(actual, pred) mean(abs(actual - pred))
mape_fn <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

# 6. Nested inner-CV folds for hyperparameter selection 
make_inner_folds <- function(train_years, n_test_inner = 3, n_folds_inner = 3) {
  ty <- sort(train_years)
  L  <- length(ty)
  needed <- n_folds_inner * n_test_inner
  if (L < needed + 3) n_folds_inner <- max(1, floor((L - 3) / n_test_inner))
  out <- list()
  for (k in seq_len(n_folds_inner)) {
    cut <- L - (n_folds_inner - k + 1) * n_test_inner
    if (cut < 3) next
    fit_idx <- 1:cut
    val_idx <- (cut + 1):min(cut + n_test_inner, L)
    out[[length(out) + 1]] <- list(fit = ty[fit_idx], val = ty[val_idx])
  }
  out
}

# 7. Hyperparameter grid 
hp_grid <- expand.grid(
  max_depth     = c(2, 3),
  learning_rate = c(0.05, 0.1),
  nrounds       = c(50, 100, 200)
)
hp_grid$num_leaves <- pmin(2^hp_grid$max_depth - 1, 7)

FIXED <- list(
  bagging_fraction   = 0.8,
  feature_fraction   = 0.8,
  bagging_freq       = 1,
  min_data_in_leaf   = 3,
  lambda_l2          = 1,
  lambda_l1          = 0.1,
  min_gain_to_split  = 0,
  boost_from_average = TRUE,
  deterministic      = TRUE,
  force_row_wise     = TRUE,
  num_threads        = 1,
  verbose            = -1
)

score_config <- function(train_df, vars, inner_folds, max_depth, num_leaves,
                         learning_rate, nrounds, seed = 2026) {
  rmses <- numeric(0)
  for (f in inner_folds) {
    fit_d <- train_df[train_df$Year %in% f$fit, ]
    val_d <- train_df[train_df$Year %in% f$val, ]
    if (nrow(fit_d) < 5 || nrow(val_d) == 0) next
    dfit <- lgb.Dataset(data = as.matrix(fit_d[, vars, drop = FALSE]), label = fit_d$Cases)
    params <- c(list(objective = "regression", metric = "rmse",
                     max_depth = max_depth, num_leaves = num_leaves,
                     learning_rate = learning_rate), FIXED)
    set.seed(seed)
    m <- lgb.train(params = params, data = dfit, nrounds = nrounds, verbose = -1)
    pred <- predict(m, as.matrix(val_d[, vars, drop = FALSE]))
    rmses <- c(rmses, sqrt(mean((val_d$Cases - pred)^2)))
  }
  if (length(rmses) == 0) return(Inf)
  mean(rmses)
}

# 8. Core fit function
fit_one <- function(data, vars, train_years, test_years, seed = 2026) {
  
  df <- data[data$Year %in% c(train_years, test_years), c("Year", "Cases", vars)]
  df <- df[stats::complete.cases(df), ]   # drops Model 8's 1981 lag-NA row
  
  train_df <- df[df$Year %in% train_years, ]
  test_df  <- df[df$Year %in% test_years, ]
  
  if (nrow(train_df) < 10 || nrow(test_df) == 0) return(NULL)
  
  inner_folds <- make_inner_folds(train_df$Year)
  
  best <- NULL
  for (i in seq_len(nrow(hp_grid))) {
    g  <- hp_grid[i, ]
    sc <- score_config(train_df, vars, inner_folds, g$max_depth, g$num_leaves,
                       g$learning_rate, g$nrounds, seed)
    if (is.null(best) || sc < best$score) {
      best <- list(max_depth = g$max_depth, num_leaves = g$num_leaves,
                   learning_rate = g$learning_rate, nrounds = g$nrounds, score = sc)
    }
  }
  
  final_params <- c(list(objective = "regression", metric = "rmse",
                         max_depth = best$max_depth, num_leaves = best$num_leaves,
                         learning_rate = best$learning_rate), FIXED)
  
  dtrain <- lgb.Dataset(data = as.matrix(train_df[, vars, drop = FALSE]), label = train_df$Cases)
  
  set.seed(seed)
  final_model <- lgb.train(params = final_params, data = dtrain, nrounds = best$nrounds, verbose = -1)
  
  pred_train <- predict(final_model, as.matrix(train_df[, vars, drop = FALSE]))
  pred_test  <- predict(final_model, as.matrix(test_df[, vars, drop = FALSE]))
  
  train_mean <- mean(train_df$Cases)
  
  metrics <- tibble(
    n_train            = nrow(train_df),
    n_test             = nrow(test_df),
    Train_R2           = r2_score(train_df$Cases, pred_train, train_mean),
    Train_RMSE         = rmse_fn(train_df$Cases, pred_train),
    Train_MAE          = mae_fn(train_df$Cases, pred_train),
    Train_MAPE         = mape_fn(train_df$Cases, pred_train),
    Test_R2            = r2_score(test_df$Cases, pred_test, train_mean),
    Test_RMSE          = rmse_fn(test_df$Cases, pred_test),
    Test_MAE           = mae_fn(test_df$Cases, pred_test),
    Test_MAPE          = mape_fn(test_df$Cases, pred_test),
    best_max_depth     = best$max_depth,
    best_num_leaves    = best$num_leaves,
    best_learning_rate = best$learning_rate,
    best_nrounds       = best$nrounds,
    inner_cv_rmse      = best$score
  )
  
  preds <- bind_rows(
    tibble(Year = train_df$Year, Set = "Train", Actual = train_df$Cases, Predicted = pred_train),
    tibble(Year = test_df$Year,  Set = "Test",  Actual = test_df$Cases,  Predicted = pred_test)
  )
  
  list(metrics = metrics, preds = preds)
}

# 9. Run pipeline across all folds x all models 
all_metrics <- list()
all_preds   <- list()

for (fold_name in names(folds)) {
  train_years <- folds[[fold_name]]$train
  test_years  <- folds[[fold_name]]$test
  
  fold_metrics <- list()
  fold_preds   <- list()
  
  for (mname in names(model_vars)) {
    res <- fit_one(data, model_vars[[mname]], train_years, test_years)
    if (is.null(res)) {
      warning(sprintf("Fold %s / %s produced no result (insufficient rows) - skipped.", fold_name, mname))
      next
    }
    m <- res$metrics
    m$Fold        <- fold_name
    m$Model       <- mname
    m$Train_Years <- paste0(min(train_years), "-", max(train_years))
    m$Test_Years  <- paste0(min(test_years), "-", max(test_years))
    fold_metrics[[mname]] <- m
    
    p <- res$preds
    p$Fold  <- fold_name
    p$Model <- mname
    fold_preds[[mname]] <- p
  }
  
  fold_metrics_df <- bind_rows(fold_metrics) %>%
    select(Fold, Model, Train_Years, Test_Years, n_train, n_test,
           Train_R2, Train_RMSE, Train_MAE, Train_MAPE,
           Test_R2, Test_RMSE, Test_MAE, Test_MAPE,
           best_max_depth, best_num_leaves, best_learning_rate, best_nrounds, inner_cv_rmse)
  
  fold_preds_df <- bind_rows(fold_preds) %>%
    select(Fold, Model, Year, Set, Actual, Predicted)
  
  all_metrics[[fold_name]] <- fold_metrics_df
  all_preds[[fold_name]]   <- fold_preds_df
  
  fname <- file.path(output_path, paste0("Model ", fold_name, ".xlsx"))
  write.xlsx(
    list(Metrics = fold_metrics_df, Predictions = fold_preds_df),
    fname, overwrite = TRUE
  )
}


# 11. Combined workbook across all folds 
combined_metrics <- bind_rows(all_metrics)

write.xlsx(
  list(
    All_Folds_Metrics   = combined_metrics,
    Pooled_Test_Summary = pooled_test_summary,
    All_Predictions     = combined_preds
  ),
  file.path(output_path, "LightGBM_All_Models_Combined.xlsx"),
  overwrite = TRUE
)




# 12. SHAP analysis, Fold E (train 1981-2022, test 2023-2025),Model 7 


# 12.1 Rebuild the exact Fold E / Model 7 data 
fold_id  <- "E"
model_id <- "Model7"
vars_M7  <- model_vars[[model_id]]

train_years_E <- folds[[fold_id]]$train
test_years_E  <- folds[[fold_id]]$test

df_E <- data[data$Year %in% c(train_years_E, test_years_E), c("Year", "Cases", vars_M7)]
df_E <- df_E[stats::complete.cases(df_E), ]

train_df_E <- df_E[df_E$Year %in% train_years_E, ]

# 12.2 Reuse the best hyperparameters selected for E / Model 7 
best_row_E7 <- combined_metrics[combined_metrics$Fold == fold_id &
                                  combined_metrics$Model == model_id, ]
stopifnot(nrow(best_row_E7) == 1)

final_params_E7 <- c(
  list(objective     = "regression",
       metric        = "rmse",
       max_depth     = best_row_E7$best_max_depth,
       num_leaves    = best_row_E7$best_num_leaves,
       learning_rate = best_row_E7$best_learning_rate),
  FIXED
)

# 12.3 Refit the final Fold E / Model 7 model 
dtrain_E7 <- lgb.Dataset(data  = as.matrix(train_df_E[, vars_M7, drop = FALSE]),
                         label = train_df_E$Cases)

set.seed(2026)
final_model_E7 <- lgb.train(params  = final_params_E7,
                            data    = dtrain_E7,
                            nrounds = best_row_E7$best_nrounds,
                            verbose = -1)

# 12.4 TreeSHAP contributions 
shap_input <- as.matrix(df_E[, vars_M7, drop = FALSE])
shap_raw   <- predict(final_model_E7, shap_input, type = "contrib")

bias_col    <- ncol(shap_raw)   # last column is the Shapley base value
shap_matrix <- shap_raw[, -bias_col, drop = FALSE]
colnames(shap_matrix) <- vars_M7

# 12.5 Global feature importance = mean(|SHAP value|) 
shap_importance <- data.frame(
  Feature       = vars_M7,
  Mean_Abs_SHAP = colMeans(abs(shap_matrix))
)
shap_importance <- shap_importance[order(-shap_importance$Mean_Abs_SHAP), ]

shap_importance_summary <- shap_importance
shap_importance_summary$Feature <- as.character(shap_importance_summary$Feature)

shap_importance$Feature <- factor(shap_importance$Feature,
                                  levels = rev(shap_importance$Feature))

# 12.6 SHAP feature-importance bar plot 
n_feat     <- nrow(shap_importance)
bar_colors <- colorRampPalette(c("#00BFA6", "#3B4CCA", "#9C27B0", "#E6007A"))(n_feat)

shap_plot <- ggplot(shap_importance, aes(x = Feature, y = Mean_Abs_SHAP, fill = Feature)) +
  geom_col(width = 0.55, color = NA) +
  coord_flip() +
  scale_fill_manual(values = bar_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "mean(|SHAP value|)", title = "A) SHAP feature importance") +
  theme_classic(base_size = 16) +
  theme(
    text          = element_text(family = "serif"),
    plot.title    = element_text(face = "bold", colour = "black", size = 20,
                                 hjust = 0, margin = margin(b = 10)),
    axis.line     = element_line(colour = "black", linewidth = 0.6),
    axis.ticks    = element_line(colour = "black", linewidth = 0.5),
    axis.text.y   = element_text(face = "bold", colour = "black", size = 17,
                                 margin = margin(r = 6)),
    axis.text.x   = element_text(colour = "black", size = 14),
    axis.title.x  = element_text(face = "bold", size = 16, margin = margin(t = 8)),
    plot.margin   = margin(t = 10, r = 20, b = 10, l = 10)
  )

print(shap_plot)

# 12.7 Save the plot 
height_mm <- max(140, n_feat * 12)

ggsave(file.path(output_path, "SHAP_FoldE_Model7.tiff"), shap_plot,
       width = 180, height = height_mm, units = "mm", dpi = 600, compression = "lzw")
ggsave(file.path(output_path, "SHAP_FoldE_Model7.png"), shap_plot,
       width = 180, height = height_mm, units = "mm", dpi = 600)

# 12.8 Export SHAP values to Excel 
shap_values_df <- cbind(
  Year = df_E$Year,
  Set  = ifelse(df_E$Year %in% train_years_E, "Train", "Test"),
  as.data.frame(shap_matrix)
)

write.xlsx(
  list(
    SHAP_Values             = shap_values_df,
    SHAP_Importance_Summary = shap_importance_summary[, c("Feature", "Mean_Abs_SHAP")]
  ),
  file.path(output_path, "SHAP_FoldE_Model7.xlsx"),
  overwrite = TRUE
)



# 13 SHAP beeswarm plot, Fold E / Model 7

feature_order <- levels(shap_importance$Feature)

beeswarm_df <- do.call(rbind, lapply(vars_M7, function(v) {
  fv        <- df_E[[v]]
  rng       <- range(fv, na.rm = TRUE)
  fv_scaled <- if (diff(rng) == 0) rep(0.5, length(fv)) else (fv - rng[1]) / diff(rng)
  data.frame(
    Feature             = v,
    SHAP                = shap_matrix[, v],
    FeatureValue        = fv,
    FeatureValue_scaled = fv_scaled
  )
}))
beeswarm_df$Feature <- factor(beeswarm_df$Feature, levels = feature_order)

# 13.1 Beeswarm  SHAP summary plot 
beeswarm_plot <- ggplot(beeswarm_df, aes(x = Feature, y = SHAP, color = FeatureValue_scaled)) +
  geom_quasirandom(groupOnX = TRUE, width = 0.38, size = 1.5, alpha = 0.85) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.7) +
  coord_flip() +
  scale_color_gradientn(
    colours = c("#3B4CC0", "#B266FF", "#FF0D57"),
    breaks  = c(0, 1),
    labels  = c("Low", "High"),
    name    = "Feature value",
    guide   = guide_colorbar(barheight = unit(n_feat * 1.05, "lines"),
                             barwidth  = unit(0.5, "cm"),
                             ticks     = FALSE)
  ) +
  labs(x = NULL, y = "SHAP value (impact on model output)", title = "B) SHAP summary") +
  theme_classic(base_size = 16) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", colour = "black", size = 20,
                                      hjust = 0, margin = margin(b = 10)),
    axis.line.y        = element_blank(),
    axis.line.x        = element_line(colour = "black", linewidth = 0.6),
    axis.ticks.y       = element_blank(),
    axis.ticks.x       = element_line(colour = "black", linewidth = 0.5),
    axis.text.y        = element_blank(),
    axis.text.x        = element_text(colour = "black", size = 14),
    axis.title.x       = element_text(face = "bold", size = 16, margin = margin(t = 8)),
    panel.grid.major.x = element_line(colour = "grey85", linetype = "dashed", linewidth = 0.3),
    legend.title       = element_text(face = "bold", size = 13),
    legend.text        = element_text(size = 11),
    plot.margin        = margin(t = 10, r = 20, b = 10, l = 4)
  )

print(beeswarm_plot)

# 13.2 Combine panel A (importance) and B (beeswarm) 
combined_shap_fig <- shap_plot + plot_spacer() + beeswarm_plot +
  patchwork::plot_layout(ncol = 3, widths = c(1, 0.04, 1))

print(combined_shap_fig)

combined_width_mm  <- 340
combined_height_mm <- height_mm

ggsave(file.path(output_path, "SHAP_Combined_FoldE_Model7.tiff"), combined_shap_fig,
       width = combined_width_mm, height = combined_height_mm, units = "mm",
       dpi = 600, compression = "lzw")
ggsave(file.path(output_path, "SHAP_Combined_FoldE_Model7.png"), combined_shap_fig,
       width = combined_width_mm, height = combined_height_mm, units = "mm", dpi = 600)

# 13.3 Export beeswarm-related values to Excel 
beeswarm_summary <- do.call(rbind, lapply(vars_M7, function(v) {
  data.frame(
    Feature           = v,
    Mean_SHAP         = mean(shap_matrix[, v]),
    Mean_Abs_SHAP     = mean(abs(shap_matrix[, v])),
    Mean_FeatureValue = mean(df_E[[v]], na.rm = TRUE),
    SD_FeatureValue   = sd(df_E[[v]], na.rm = TRUE)
  )
}))
beeswarm_summary <- beeswarm_summary[order(-beeswarm_summary$Mean_Abs_SHAP), ]

write.xlsx(
  list(
    Beeswarm_Raw_SHAP      = cbind(Year = df_E$Year, as.data.frame(shap_matrix)),
    Beeswarm_FeatureValues = cbind(Year = df_E$Year, df_E[, vars_M7]),
    Beeswarm_Summary       = beeswarm_summary
  ),
  file.path(output_path, "SHAP_Beeswarm_FoldE_Model7.xlsx"),
  overwrite = TRUE
)

# 14 SHAP Dependence Plots, Fold E / Model 7 

# 14.1 Feature order
dep_feature_order <- shap_importance_summary$Feature   # 14 names, most -> least important


dep_df <- do.call(rbind, lapply(vars_M7, function(v) {
  data.frame(
    Feature      = v,
    FeatureValue = df_E[[v]],
    SHAP         = shap_matrix[, v],
    Year         = df_E$Year,
    Set          = ifelse(df_E$Year %in% train_years_E, "Train", "Test")
  )
}))
dep_df$Feature <- factor(dep_df$Feature, levels = dep_feature_order)

# 14.2 dependence-plot function 

make_dep_plot <- function(feat) {
  d <- dep_df[dep_df$Feature == feat, ]
  ggplot(d, aes(x = FeatureValue, y = SHAP)) +
    geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.45) +
    geom_point(colour = "#1B6FB5", alpha = 0.65, size = 2.0) +
    geom_smooth(method = "loess", se = FALSE, colour = "#D62728",
                linewidth = 1.15, span = 0.85) +
    scale_x_continuous(expand = expansion(mult = 0.06)) +
    scale_y_continuous(expand = expansion(mult = 0.08)) +
    labs(x = feat, y = "SHAP value") +
    theme_classic(base_size = 15) +
    theme(
      text         = element_text(family = "serif"),
      axis.title.x = element_text(face = "bold", colour = "black", size = 15, margin = margin(t = 6)),
      axis.title.y = element_text(face = "bold", colour = "black", size = 17, margin = margin(r = 6)),
      axis.text    = element_text(colour = "black", size = 12),
      axis.line    = element_line(colour = "black", linewidth = 0.55),
      axis.ticks   = element_line(colour = "black", linewidth = 0.45),
      plot.margin  = margin(t = 6, r = 10, b = 6, l = 6)
    )
}

dep_plots <- setNames(lapply(dep_feature_order, make_dep_plot), dep_feature_order)

# 14.3 3x3 grid layout
row1 <- dep_plots[[1]]  | dep_plots[[2]]  | dep_plots[[3]]  | dep_plots[[4]]
row2 <- dep_plots[[5]]  | dep_plots[[6]]  | dep_plots[[7]]  | dep_plots[[8]]
row3 <- dep_plots[[9]]  | dep_plots[[10]] | dep_plots[[11]] | dep_plots[[12]]
row4 <- plot_spacer()   | dep_plots[[13]] | dep_plots[[14]] | plot_spacer()

dep_combined <- (row1 / row2 / row3 / row4) +
  patchwork::plot_layout(heights = c(1, 1, 1, 1))

print(dep_combined)

# 14.4 Save 
ggsave(file.path(output_path, "SHAP_Dependence_FoldE_Model7.tiff"), dep_combined,
       width = 340, height = 300, units = "mm", dpi = 600, compression = "lzw")
ggsave(file.path(output_path, "SHAP_Dependence_FoldE_Model7.png"), dep_combined,
       width = 340, height = 300, units = "mm", dpi = 600)

# 14.5 Export underlying values to Excel 
dep_df_export <- dep_df
dep_df_export$Feature <- as.character(dep_df_export$Feature)

write.xlsx(
  list(SHAP_Dependence_Data = dep_df_export),
  file.path(output_path, "SHAP_Dependence_FoldE_Model7.xlsx"),
  overwrite = TRUE
)



# Linear Regression 

library("readxl")
library("openxlsx")
library("dplyr")
library("purrr")
library("tibble")

set.seed(2026)

# 1. Paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/ML/LR_1"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# 2. Load and clean data
data <- read_excel(data_path, sheet = 1)
data <- data[!is.na(data$Year), ]
data$Cases <- as.numeric(data$Cases)
data <- as.data.frame(data[order(data$Year), ])

stopifnot(all(diff(data$Year) == 1))
cat("Loaded", nrow(data), "rows, years", min(data$Year), "-", max(data$Year), "\n")

# 3. Predictor sets, Model 1-8
model_vars <- list(
  Model1 = c("T2M","RH2M","PREP","WS2M"),
  Model2 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4"),
  Model3 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov"),
  Model4 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers"),
  Model5 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate"),
  Model6 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate"),
  Model7 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds"),
  Model8 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds","T2M_lag1","RH2M_lag1","PREP_lag1","Vac_cov_lag1")
)

# 4. Outer rolling-origin folds 
folds <- list(
  A = list(train = 1981:2010, test = 2011:2013),
  B = list(train = 1981:2013, test = 2014:2016),
  C = list(train = 1981:2016, test = 2017:2019),
  D = list(train = 1981:2019, test = 2020:2022),
  E = list(train = 1981:2022, test = 2023:2025)
)

# 5. Metric functions
rmse_fn <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae_fn  <- function(actual, pred) mean(abs(actual - pred))
mape_fn <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

# 6. Core fit function
fit_one <- function(data, vars, train_years, test_years) {
  
  df <- data[data$Year %in% c(train_years, test_years), c("Year", "Cases", vars)]
  df <- df[stats::complete.cases(df), ]   # drops Model 8's 1981 lag-NA row
  
  train_df <- df[df$Year %in% train_years, ]
  test_df  <- df[df$Year %in% test_years, ]
  
  n_predictors <- length(vars)
  if (nrow(train_df) < n_predictors + 2 || nrow(test_df) == 0) return(NULL)
  
  fmla <- as.formula(paste("Cases ~", paste(vars, collapse = " + ")))
  fit  <- lm(fmla, data = train_df)
  
  n_aliased <- sum(is.na(coef(fit)))   # >0 flags collinear/dropped predictors
  
  pred_train <- predict(fit, newdata = train_df)
  pred_test  <- predict(fit, newdata = test_df)
  
  metrics <- tibble(
    n_train          = nrow(train_df),
    n_test           = nrow(test_df),
    n_predictors     = n_predictors,
    n_aliased_coefs  = n_aliased,
    Train_RMSE       = rmse_fn(train_df$Cases, pred_train),
    Train_MAE        = mae_fn(train_df$Cases, pred_train),
    Train_MAPE       = mape_fn(train_df$Cases, pred_train),
    Test_RMSE        = rmse_fn(test_df$Cases, pred_test),
    Test_MAE         = mae_fn(test_df$Cases, pred_test),
    Test_MAPE        = mape_fn(test_df$Cases, pred_test)
  )
  
  preds <- bind_rows(
    tibble(Year = train_df$Year, Set = "Train", Actual = train_df$Cases, Predicted = pred_train),
    tibble(Year = test_df$Year,  Set = "Test",  Actual = test_df$Cases,  Predicted = pred_test)
  )
  
  list(metrics = metrics, preds = preds, model = fit)
}

# 7. Run pipeline across all folds x all models
all_metrics <- list()
all_preds   <- list()

for (fold_name in names(folds)) {
  train_years <- folds[[fold_name]]$train
  test_years  <- folds[[fold_name]]$test
  
  fold_metrics <- list()
  fold_preds   <- list()
  
  for (mname in names(model_vars)) {
    res <- fit_one(data, model_vars[[mname]], train_years, test_years)
    if (is.null(res)) {
      warning(sprintf("Fold %s / %s produced no result (insufficient rows) - skipped.", fold_name, mname))
      next
    }
    m <- res$metrics
    m$Fold        <- fold_name
    m$Model       <- mname
    m$Train_Years <- paste0(min(train_years), "-", max(train_years))
    m$Test_Years  <- paste0(min(test_years), "-", max(test_years))
    fold_metrics[[mname]] <- m
    
    p <- res$preds
    p$Fold  <- fold_name
    p$Model <- mname
    fold_preds[[mname]] <- p
  }
  
  fold_metrics_df <- bind_rows(fold_metrics) %>%
    select(Fold, Model, Train_Years, Test_Years, n_train, n_test, n_predictors, n_aliased_coefs,
           Train_RMSE, Train_MAE, Train_MAPE,
           Test_RMSE, Test_MAE, Test_MAPE)
  
  fold_preds_df <- bind_rows(fold_preds) %>%
    select(Fold, Model, Year, Set, Actual, Predicted)
  
  all_metrics[[fold_name]] <- fold_metrics_df
  all_preds[[fold_name]]   <- fold_preds_df
  
  fname <- file.path(output_path, paste0("Model ", fold_name, ".xlsx"))
  write.xlsx(
    list(Metrics = fold_metrics_df, Predictions = fold_preds_df),
    fname, overwrite = TRUE
  )
  cat("Saved:", fname, "\n")
}

# 8. Combined workbook across all folds
combined_metrics <- bind_rows(all_metrics)
combined_preds   <- bind_rows(all_preds)

write.xlsx(
  list(
    All_Folds_Metrics = combined_metrics,
    All_Predictions   = combined_preds
  ),
  file.path(output_path, "LinearRegression_All_Models_Combined.xlsx"),
  overwrite = TRUE
)



# Negative Binomial Regression 

library("readxl")
library("openxlsx")
library("dplyr")
library("purrr")
library("tibble")
library("MASS")

set.seed(2026)

# 1. Paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Data.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/ML/NB_1"

# 2. Load and clean data
data <- read_excel(data_path, sheet = 1)
data <- data[!is.na(data$Year), ]
data$Cases <- as.numeric(data$Cases)
data <- as.data.frame(data[order(data$Year), ])

stopifnot(all(diff(data$Year) == 1))
cat("Loaded", nrow(data), "rows, years", min(data$Year), "-", max(data$Year), "\n")

# 3. Predictor sets, Model 1-8
model_vars <- list(
  Model1 = c("T2M","RH2M","PREP","WS2M"),
  Model2 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4"),
  Model3 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov"),
  Model4 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers"),
  Model5 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate"),
  Model6 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate"),
  Model7 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds"),
  Model8 = c("T2M","RH2M","PREP","WS2M","Population","Pop_aged_0_4","Vac_cov",
             "Net_migration","Air_passengers","GDP","Poverty_rate","Literacy_rate",
             "THE","Hosp_beds","T2M_lag1","RH2M_lag1","PREP_lag1","Vac_cov_lag1")
)

# 4. Outer rolling-origin folds 
folds <- list(
  A = list(train = 1981:2010, test = 2011:2013),
  B = list(train = 1981:2013, test = 2014:2016),
  C = list(train = 1981:2016, test = 2017:2019),
  D = list(train = 1981:2019, test = 2020:2022),
  E = list(train = 1981:2022, test = 2023:2025)
)

# 5. Metric functions
rmse_fn <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae_fn  <- function(actual, pred) mean(abs(actual - pred))
mape_fn <- function(actual, pred) mean(abs((actual - pred) / actual)) * 100

# 6. Core fit function
fit_one <- function(data, vars, train_years, test_years) {
  
  df <- data[data$Year %in% c(train_years, test_years), c("Year", "Cases", vars)]
  df <- df[stats::complete.cases(df), ]   # drops Model 8's 1981 lag-NA row
  df$Cases_raw <- df$Cases
  df$Cases     <- as.integer(round(df$Cases))   # glm.nb() requires non-negative integer counts
  
  train_df <- df[df$Year %in% train_years, ]
  test_df  <- df[df$Year %in% test_years, ]
  
  n_predictors <- length(vars)
  if (nrow(train_df) < n_predictors + 2 || nrow(test_df) == 0) return(NULL)
  
  fmla <- as.formula(paste("Cases ~", paste(vars, collapse = " + ")))
  
  conv_warning <- NA_character_
  fit <- tryCatch({
    withCallingHandlers(
      MASS::glm.nb(fmla, data = train_df, control = glm.control(maxit = 200)),
      warning = function(w) {
        conv_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) NULL)
  
  if (is.null(fit)) return(NULL)
  
  coef_mat <- summary(fit)$coefficients
  ci_mat   <- suppressWarnings(tryCatch(confint.default(fit), error = function(e) {
    matrix(NA_real_, nrow = nrow(coef_mat), ncol = 2, dimnames = list(rownames(coef_mat), NULL))
  }))
  
  coef_tbl <- tibble(
    Variable    = rownames(coef_mat),
    Estimate    = coef_mat[, "Estimate"],
    Std_Error   = coef_mat[, "Std. Error"],
    z_value     = coef_mat[, "z value"],
    p_value     = coef_mat[, "Pr(>|z|)"],
    IRR         = exp(coef_mat[, "Estimate"]),
    CI_lower_95 = exp(ci_mat[, 1]),
    CI_upper_95 = exp(ci_mat[, 2])
  )
  
  pred_train <- predict(fit, newdata = train_df, type = "response")
  pred_test  <- predict(fit, newdata = test_df,  type = "response")
  
  metrics <- tibble(
    n_train             = nrow(train_df),
    n_test              = nrow(test_df),
    n_predictors        = n_predictors,
    AIC                 = AIC(fit),
    Theta               = fit$theta,
    Theta_SE            = fit$SE.theta,
    Converged           = isTRUE(fit$converged),
    Convergence_Warning = conv_warning,
    Train_RMSE          = rmse_fn(train_df$Cases, pred_train),
    Train_MAE           = mae_fn(train_df$Cases, pred_train),
    Train_MAPE          = mape_fn(train_df$Cases, pred_train),
    Test_RMSE           = rmse_fn(test_df$Cases, pred_test),
    Test_MAE            = mae_fn(test_df$Cases, pred_test),
    Test_MAPE           = mape_fn(test_df$Cases, pred_test)
  )
  
  preds <- bind_rows(
    tibble(Year = train_df$Year, Set = "Train", Actual = train_df$Cases,
           Actual_Raw = train_df$Cases_raw, Predicted = pred_train),
    tibble(Year = test_df$Year,  Set = "Test",  Actual = test_df$Cases,
           Actual_Raw = test_df$Cases_raw,  Predicted = pred_test)
  )
  
  list(metrics = metrics, coefs = coef_tbl, preds = preds)
}

# 7. Run pipeline across all folds x all models
all_metrics <- list()
all_coefs   <- list()
all_preds   <- list()

for (fold_name in names(folds)) {
  train_years <- folds[[fold_name]]$train
  test_years  <- folds[[fold_name]]$test
  
  fold_metrics <- list()
  fold_coefs   <- list()
  fold_preds   <- list()
  
  for (mname in names(model_vars)) {
    res <- fit_one(data, model_vars[[mname]], train_years, test_years)
    if (is.null(res)) {
      warning(sprintf("Fold %s / %s produced no result (insufficient rows or fit error) - skipped.", fold_name, mname))
      next
    }
    m <- res$metrics
    m$Fold        <- fold_name
    m$Model       <- mname
    m$Train_Years <- paste0(min(train_years), "-", max(train_years))
    m$Test_Years  <- paste0(min(test_years), "-", max(test_years))
    fold_metrics[[mname]] <- m
    
    cft <- res$coefs
    cft$Fold  <- fold_name
    cft$Model <- mname
    fold_coefs[[mname]] <- cft
    
    p <- res$preds
    p$Fold  <- fold_name
    p$Model <- mname
    fold_preds[[mname]] <- p
  }
  
  fold_metrics_df <- bind_rows(fold_metrics) %>%
    dplyr::select(Fold, Model, Train_Years, Test_Years, n_train, n_test, n_predictors,
                  AIC, Theta, Theta_SE, Converged, Convergence_Warning,
                  Train_RMSE, Train_MAE, Train_MAPE,
                  Test_RMSE, Test_MAE, Test_MAPE)
  
  fold_coefs_df <- bind_rows(fold_coefs) %>%
    dplyr::select(Fold, Model, Variable, Estimate, Std_Error, z_value, p_value,
                  IRR, CI_lower_95, CI_upper_95)
  
  fold_preds_df <- bind_rows(fold_preds) %>%
    dplyr::select(Fold, Model, Year, Set, Actual, Actual_Raw, Predicted)
  
  all_metrics[[fold_name]] <- fold_metrics_df
  all_coefs[[fold_name]]   <- fold_coefs_df
  all_preds[[fold_name]]   <- fold_preds_df
  
  fname <- file.path(output_path, paste0("Model ", fold_name, ".xlsx"))
  write.xlsx(
    list(Metrics = fold_metrics_df, Coefficients = fold_coefs_df, Predictions = fold_preds_df),
    fname, overwrite = TRUE
  )
}

# 8. Combined workbook across all folds
combined_metrics <- bind_rows(all_metrics)
combined_coefs   <- bind_rows(all_coefs)
combined_preds   <- bind_rows(all_preds)

write.xlsx(
  list(
    All_Folds_Metrics = combined_metrics,
    All_Coefficients  = combined_coefs,
    All_Predictions   = combined_preds
  ),
  file.path(output_path, "NegBinomial_All_Models_Combined.xlsx"),
  overwrite = TRUE
)



# Error line plot

library("readxl")
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

# path
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data preprocessing/Error plot/ALL_models_RMSE.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/Data preprocessing/Error plot"

# Import data
data_raw <- read_excel(data_path)

data <- data_raw %>%
  separate(RMSE, into = c("Mean", "SD"), sep = "±") %>%
  mutate(
    Mean = as.numeric(trimws(Mean)),
    SD   = as.numeric(trimws(SD)),
    Model_Group = str_extract(Models, "^[A-Za-z]+"),
    Model_No    = as.integer(str_extract(Models, "[0-9]+$"))
  ) %>%
  mutate(
    Model_Group = case_when(
      Model_Group == "XGBoost"  ~ "XGBoost",
      Model_Group == "RF"       ~ "Random Forest",
      Model_Group == "LightGBM" ~ "LightGBM",
      Model_Group == "LR"       ~ "Linear Regression",
      Model_Group == "NB"       ~ "Negative Binomial",
      TRUE ~ Model_Group
    ),
    Model_Group = factor(Model_Group,
                         levels = c("XGBoost", "Random Forest", "LightGBM",
                                    "Linear Regression", "Negative Binomial")),
    Model_Label = factor(paste0("Model ", Model_No), levels = paste0("Model ", 1:8))
  ) %>%
  arrange(Model_Group, Model_No)

model_colors <- c("XGBoost" = "#1f77b4",
                  "Random Forest" = "#ff7f0e",
                  "LightGBM" = "#2ca02c",
                  "Linear Regression" = "#d62728",
                  "Negative Binomial" = "#9467bd")

model_shapes <- c("XGBoost" = 16,
                  "Random Forest" = 15,
                  "LightGBM" = 17,
                  "Linear Regression" = 18,
                  "Negative Binomial" = 6)

p <- ggplot(data, aes(x = Model_Label, y = Mean, group = Model_Group, color = Model_Group)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = 0.15, linewidth = 0.7) +
  geom_line(linewidth = 1.15) +
  geom_point(aes(shape = Model_Group), size = 3.4, stroke = 1) +
  scale_color_manual(values = model_colors, name = NULL) +
  scale_shape_manual(values = model_shapes, name = NULL) +
  scale_y_continuous(limits = c(0, 7000), breaks = seq(0, 7000, 1000),
                     expand = expansion(mult = c(0.01, 0.03))) +
  labs(x = "Models", y = "RMSE (Mean \u00B1 SD)") +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    plot.title   = element_blank(),
    plot.caption = element_blank(),
    axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 12)),
    axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 12)),
    axis.text    = element_text(size = 12.5, color = "black"),
    axis.line    = element_line(linewidth = 0.7, color = "black"),
    axis.ticks   = element_line(linewidth = 0.6, color = "black"),
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.text  = element_text(size = 15, face = "bold"),
    legend.key.size   = unit(1.3, "lines"),
    legend.spacing.x  = unit(0.8, "cm"),
    legend.box.margin = margin(t = 10),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor    = element_blank(),
    plot.margin = margin(15, 20, 10, 15)
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4, linewidth = 1.5)),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  )

print(p)

ggsave(filename = file.path(output_path, "ALL_models_RMSE_plot.tiff"),
       plot = p, width = 10, height = 6.5, units = "in", dpi = 600, compression = "lzw")

ggsave(filename = file.path(output_path, "ALL_models_RMSE_plot.png"),
       plot = p, width = 10, height = 6.5, units = "in", dpi = 600)




# Grouped Pairs Plots 

#library

library("readxl")
library("GGally")
library("ggplot2")
library("dplyr")
library("grid")

# 1. Paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Analysis.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/Check/Pairs_Plots"

# 2. Load and clean data
data <- as.data.frame(readxl::read_excel(data_path, sheet = 1))

# 3. Variable groups 
group_list <- list(
  "Climatic factors"           = c("T2M", "T2M_MAX", "T2M_MIN", "RH2M", "PREP", "WS2M"),
  "Demographic factors"        = c("Population", "Pop_density", "Pop_growth", "Pop_aged_0_4", "Pop_aged_5_9"),
  "Immunization"                = c("Vac_cov"),
  "Mobility"                    = c("Net_migration", "Air_passengers"),
  "Socio-economic factors"     = c("GDP", "GNI", "CPI", "Poverty_rate"),
  "Educational factors"        = c("Literacy_rate", "Pri_comp_rate", "Sec_comp_rate"),
  "Healthcare system factors"  = c("THE", "Dispensary", "Hosp_beds", "Reg_doc", "Reg_nurse", "UHC")
)

# 4. Display labels 
label_list <- list(
  "Climatic factors"           = c(T2M = "A1", T2M_MAX = "A2", T2M_MIN = "A3",
                                   RH2M = "A4", PREP = "A5", WS2M = "A6"),
  "Demographic factors"        = c(Population = "B1", Pop_density = "B2", Pop_growth = "B3",
                                   Pop_aged_0_4 = "B4", Pop_aged_5_9 = "B5"),
  "Immunization"                = c(Vac_cov = "C1"),
  "Mobility"                    = c(Net_migration = "D1", Air_passengers = "D2"),
  "Socio-economic factors"     = c(GDP = "E1", GNI = "E2", CPI = "E3", Poverty_rate = "E4"),
  "Educational factors"        = c(Literacy_rate = "F1", Pri_comp_rate = "F2", Sec_comp_rate = "F3"),
  "Healthcare system factors"  = c(THE = "G1", Dispensary = "G2", Hosp_beds = "G3",
                                   Reg_doc = "G4", Reg_nurse = "G5", UHC = "G6")
)


# .5 Correlation method
corr_method <- "spearman"


col_low   <- "#2166AC"   
col_mid   <- "#F7F7F7"   
col_high  <- "#B2182B"   
col_point <- "#2E1A47"   
col_dens_fill <- "#AED6E8"  
col_dens_line <- "#154360"  
col_text  <- "#0B1F3A"      
col_deep  <- "#050B1E"     
col_axis_num <- "#000000"   

# 6. Panel cell size
cell_in <- 3.6   # physical size (inches) of each matrix cell

# 7. Upper-panel function
corr_fun <- function(data, mapping, method = corr_method,
                     bar_height_in = 8, bar_width_in = 0.65, ...) {
  
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  ct <- suppressWarnings(stats::cor.test(x, y, method = method))
  r  <- unname(ct$estimate)
  p  <- ct$p.value
  
  # Significance stars: * p<0.05, ** p<0.01, *** p<0.001
  stars <- dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
  
  value_lab <- paste0(sprintf("%.3f", r), stars)
  
  df_tile <- data.frame(x = 1, y = 1, corr = r)
  
  ggplot(df_tile, aes(x = x, y = y, fill = corr)) +
    geom_tile() +
    scale_fill_gradient2(
      low = col_low, mid = col_mid, high = col_high,
      midpoint = 0, limits = c(-1, 1), space = "Lab", name = "Corr.",
      breaks = c(-1, -0.5, 0, 0.5, 1),
      guide = guide_colorbar(
        barheight   = grid::unit(bar_height_in, "in"),
        barwidth    = grid::unit(bar_width_in, "in"),
        title.theme = element_text(size = 22, face = "bold", colour = col_deep,
                                   margin = margin(b = 10)),
        label.theme = element_text(size = 17, face = "bold", colour = col_deep),
        ticks.colour = col_deep,
        frame.colour = "grey40"
      )
    ) +
    annotate("text", x = 1, y = 1.20, label = "Corr:",
             size = 7.2, fontface = "bold", colour = col_deep) +
    annotate("text", x = 1, y = 0.80, label = value_lab,
             size = 9.2, fontface = "bold", colour = col_deep) +
    theme_void() +
    theme(
      legend.position = "right",
      panel.border = element_rect(colour = "grey50", fill = NA, linewidth = 0.5)
    )
}

# 8. Master function to build one group's plot

make_group_plot <- function(data, vars, title, labels, method = corr_method) {
  
  sub_df <- data %>% dplyr::select(dplyr::all_of(vars))
  n <- length(vars)
  
  # Single-variable group (Immunization / C1): density plot instead of a matrix
  if (n == 1) {
    v   <- vars[1]
    lbl <- unname(labels[v])   # display code, e.g. "C1"
    p <- ggplot(sub_df, aes(x = .data[[v]])) +
      geom_density(fill = col_dens_fill, colour = col_dens_line,
                   alpha = 0.75, linewidth = 1.2) +
      geom_rug(alpha = 0.5, colour = col_point) +
      labs(title = title, x = lbl, y = "Density") +
      theme_bw(base_size = 14) +
      theme(
        plot.title  = element_text(size = 24, face = "bold", hjust = 0.5,
                                   colour = col_text, margin = margin(b = 10)),
        axis.title  = element_text(size = 18, face = "bold", colour = col_text),
        axis.text   = element_text(size = 20, face = "bold", colour = col_axis_num),
        axis.text.x = element_text(size = 20, face = "bold", colour = col_axis_num,
                                   margin = margin(t = 8)),
        axis.text.y = element_text(size = 20, face = "bold", colour = col_axis_num,
                                   margin = margin(r = 6)),
        axis.ticks         = element_line(colour = col_axis_num, linewidth = 0.5),
        axis.ticks.length  = grid::unit(0.25, "cm"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey55", fill = NA)
      )
    return(p)
  }
  
  # Colour-bar height 
  bar_h <- cell_in * min(4, max(2, n - 2))
  bar_w <- 0.65
  
  # Display codes 
  display_labels <- unname(labels[vars])
  
  p <- GGally::ggpairs(
    sub_df,
    columns = 1:n,
    columnLabels = display_labels,
    upper = list(continuous = GGally::wrap(corr_fun, method = method,
                                           bar_height_in = bar_h,
                                           bar_width_in  = bar_w)),
    lower = list(continuous = GGally::wrap("points", alpha = 0.55,
                                           size = 1.8, colour = col_point)),
    diag  = list(continuous = GGally::wrap("densityDiag", fill = col_dens_fill,
                                           colour = col_dens_line, alpha = 0.75,
                                           linewidth = 1.0)),
    legend = c(1, 2)
  ) +
    labs(title = title) +
    theme_bw(base_size = 13) +
    theme(
      plot.title        = element_text(size = 26, face = "bold", hjust = 0.5,
                                       colour = col_text, margin = margin(b = 14)),
      strip.text        = element_text(size = 19, face = "bold", colour = col_text),
      strip.background  = element_rect(fill = "grey92", colour = "grey40"),
      axis.text.x       = element_text(size = 20, face = "bold", colour = col_axis_num,
                                       angle = 45, hjust = 1, vjust = 1,
                                       margin = margin(t = 10)),
      axis.text.y       = element_text(size = 20, face = "bold", colour = col_axis_num,
                                       margin = margin(r = 6)),
      axis.ticks.length  = grid::unit(0.25, "cm"),
      axis.ticks        = element_line(colour = col_axis_num, linewidth = 0.5),
      axis.title        = element_text(size = 14, face = "bold", colour = col_text),
      panel.grid.major  = element_line(colour = "grey88"),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(colour = "grey55", fill = NA),
      legend.title       = element_text(size = 22, face = "bold", colour = col_deep),
      legend.text        = element_text(size = 17, face = "bold", colour = col_deep),
      legend.key.height  = grid::unit(bar_h / 5, "in"),
      legend.key.width   = grid::unit(bar_w,     "in"),
      plot.margin        = margin(15, 20, 22, 15)
    )
  
  return(p)
}

# 9. Define the seven output files 
plot_specs <- list(
  list(group = "Climatic factors",          file = "01_Climatic_factors"),
  list(group = "Demographic factors",       file = "02_Demographic_factors"),
  list(group = "Immunization",              file = "03_Immunization"),
  list(group = "Mobility",                  file = "04_Mobility"),
  list(group = "Socio-economic factors",    file = "05_Socio_economic_factors"),
  list(group = "Educational factors",       file = "06_Educational_factors"),
  list(group = "Healthcare system factors", file = "07_Healthcare_system_factors")
)

# 10. Build and save every plot 
for (spec in plot_specs) {
  
  vars   <- group_list[[spec$group]]
  labels <- label_list[[spec$group]]
  n      <- length(vars)
  
  p <- make_group_plot(data, vars, spec$group, labels)
  
  if (n == 1) {
    w <- 8.5; h <- 6.5
  } else {
    w <- max(9, n * cell_in) + 3.0
    h <- max(9, n * cell_in) + 0.6
  }
  
  fname_png  <- file.path(output_path, paste0(spec$file, ".png"))
  fname_tiff <- file.path(output_path, paste0(spec$file, ".tiff"))
  
  ggsave(fname_png,  plot = p, width = w, height = h, dpi = 400,
         units = "in", bg = "white", limitsize = FALSE)
  ggsave(fname_tiff, plot = p, width = w, height = h, dpi = 600,
         units = "in", compression = "lzw", bg = "white", limitsize = FALSE)
  
}



# Histogram and Density Panel Plots

library("readxl")
library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("purrr")
library("scales")
library("showtext")
library("sysfonts")
library("patchwork")

# 1. Paths
data_path   <- "C:/Users/hp/OneDrive/Desktop/Measles/Data/Analysis.xlsx"
output_path <- "C:/Users/hp/OneDrive/Desktop/Measles/Check/Histogram"

# 2. Load and clean data
data_raw <- readxl::read_excel(data_path, sheet = 1)


data <- data_raw

plot_vars <- setdiff(names(data), "Year")

# 3. Plot theme 
theme_q1_hist <- function() {
  theme_minimal(base_size = 16, base_family = "pub_font") +
    theme(
      text              = element_text(color = "black"),
      axis.title.x      = element_text(face = "bold", size = 20, color = "black",
                                       margin = ggplot2::margin(t = 12)),
      axis.title.y      = element_text(face = "bold", size = 20, color = "black",
                                       margin = ggplot2::margin(r = 12)),
      axis.text.x       = element_text(face = "bold", size = 15, color = "black",
                                       angle = 30, hjust = 1, vjust = 1),
      axis.text.y       = element_text(face = "bold", size = 16, color = "black"),
      axis.line         = element_line(color = "black", linewidth = 0.8),
      axis.ticks        = element_line(color = "black", linewidth = 0.6),
      panel.grid.major.y = element_line(color = "grey80", linetype = "dashed", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      plot.margin       = ggplot2::margin(15, 20, 15, 15),
      legend.position   = "none"
    )
}

# 4. Pretty axis labels
label_overrides <- c(
  Cases          = "Measles Cases",
  T2M             = "Mean Temperature ",
  T2M_MAX         = "Maximum Temperature ",
  T2M_MIN         = "Minimum Temperature ",
  RH2M            = "Relative Humidity ",
  PREP            = "Precipitation ",
  WS2M            = "Wind Speed ",
  Population      = "Population",
  Pop_density     = "Population Density ",
  Pop_growth      = "Population Growth",
  Pop_aged_0_4    = "Population Aged 0\u20134 Years",
  Pop_aged_5_9    = "Population Aged 5\u20139 Years",
  Vac_cov         = "Vaccination Coverage",
  Net_migration   = "Net Migration",
  Air_passengers  = "Air Passengers",
  GDP             = "GDP ",
  GNI             = "GNI ",
  CPI             = "Consumer Price Index",
  Poverty_rate    = "Poverty Rate ",
  Literacy_rate   = "Literacy Rate ",
  Pri_comp_rate   = "Primary Completion Rate ",
  Sec_comp_rate   = "Secondary Completion Rate ",
  THE             = "Total Health Expenditure",
  Dispensary      = "Number of Dispensaries",
  Hosp_beds       = "Hospital Beds",
  Reg_doc         = "Registered Doctors",
  Reg_nurse       = "Registered Nurses",
  UHC             = "UHC"
)

get_label <- function(var) {
  if (!is.na(label_overrides[var])) return(unname(label_overrides[var]))
  var |> gsub("_", " ", x = _) |> str_to_title()
}

# 5. Freedman-Diaconis bin count (data-driven)
fd_bins <- function(x) {
  x <- x[!is.na(x)]
  n  <- length(x)
  iqr <- IQR(x)
  if (iqr == 0 || n < 2) return(15)
  bw <- 2 * iqr / (n^(1/3))
  if (bw <= 0) return(15)
  bins <- ceiling(diff(range(x)) / bw)
  max(8, min(bins, 30))
}

# 6. Fonts 
sysfonts::font_add(
  family  = "pub_font",
  regular = "C:/Windows/Fonts/calibri.ttf",
  bold    = "C:/Windows/Fonts/calibrib.ttf"
)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)

# 7. Plotting function
make_hist_density <- function(var_name, data) {
  
  x_vec    <- data[[var_name]]
  mean_val <- mean(x_vec, na.rm = TRUE)
  n_bins   <- fd_bins(x_vec)
  
  p <- ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins   = n_bins,
      fill   = "#BBD6EC",
      color  = "white",
      alpha  = 0.85,
      linewidth = 0.3
    ) +
    geom_density(
      color     = "red",
      linetype  = "solid",
      linewidth = 1.4,
      na.rm     = TRUE
    ) +
    geom_vline(
      xintercept = mean_val,
      color      = "black",
      linetype   = "dashed",
      linewidth  = 1.1
    ) +
    labs(
      x = get_label(var_name),
      y = "Density"
    ) +
    theme_q1_hist()
  
  return(p)
}

# 8. Predictor groups for panel layout
var_groups <- list(
  Climate       = c("T2M", "T2M_MAX", "T2M_MIN", "RH2M", "PREP", "WS2M"),
  Demography    = c("Population", "Pop_density", "Pop_growth",
                    "Pop_aged_0_4", "Pop_aged_5_9"),
  Vaccination   = c("Vac_cov"),
  Mobility      = c("Net_migration", "Air_passengers"),
  Economy       = c("GDP", "GNI", "CPI", "Poverty_rate"),
  Education     = c("Literacy_rate", "Pri_comp_rate", "Sec_comp_rate"),
  Health_System = c("THE", "Dispensary", "Hosp_beds", "Reg_doc",
                    "Reg_nurse", "UHC")
)

group_titles <- c(
  Climate       = "Climatic Factors",
  Demography    = "Demographic Factors",
  Vaccination   = "Immunization",
  Mobility      = "Mobility Factors",
  Economy       = "Socio-Demographic Factors",
  Education     = "Educational Factors",
  Health_System = "Healthcare System Factors"
)

# 9. Panel layout settings 
group_layout <- list(
  Climate       = list(ncol = 3, width = 15, height = 9),
  Demography    = list(ncol = 3, width = 15, height = 9),
  Vaccination   = list(ncol = 1, width = 6,  height = 5),
  Mobility      = list(ncol = 2, width = 10, height = 5),
  Economy       = list(ncol = 2, width = 10, height = 9),
  Education     = list(ncol = 3, width = 15, height = 5),
  Health_System = list(ncol = 3, width = 15, height = 9)
)

# 10. Build and save combined panel figures
walk(names(var_groups), function(grp_name) {
  
  vars_in_grp <- intersect(var_groups[[grp_name]], plot_vars)
  
  
  plots_list <- map(vars_in_grp, make_hist_density, data = data)
  
  lay <- group_layout[[grp_name]]
  ncol_grp <- if (!is.null(lay)) lay$ncol else 2
  
  combined_plot <- wrap_plots(plots_list, ncol = ncol_grp)
  
  fname <- paste0("Histogram_Density_Panel_", grp_name)
  w <- if (!is.null(lay)) lay$width  else 12
  h <- if (!is.null(lay)) lay$height else 9
  
  ggsave(filename = file.path(output_path, paste0(fname, ".tiff")),
         plot = combined_plot, width = w, height = h, units = "in",
         dpi = 600, compression = "lzw", device = "tiff")
  
  ggsave(filename = file.path(output_path, paste0(fname, ".png")),
         plot = combined_plot, width = w, height = h, units = "in",
         dpi = 600, device = "png")
})


