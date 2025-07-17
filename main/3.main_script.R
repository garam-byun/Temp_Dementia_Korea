# Required packages
library(dplyr)
library(purrr)
source("main/run_model.R")
source("main/plot_model.R")

# Load preprocessed data
load("data/input_data.RData")

# Parameter grid
param_grid <- expand.grid(
  outcome     = c("DM_all"),            # outcome variable
  exposure    = c("temp_mean"),         # temperature variable name
  lag_max     = c(14),                  # max lag days
  knots       = I(list(c(0.25, 0.5, 0.75))),  # quantile knots
  nk          = c(2),                   # number of lag knots
  adjust_pm   = c(FALSE),               # adjust for PM10 crossbasis?
  stringsAsFactors = FALSE
)

# Run all model settings
results <- pmap(param_grid, function(outcome, exposure, lag_max, knots, nk, adjust_pm) {
  label <- paste0(outcome, "_", exposure, "_lag", lag_max, "_k", paste(knots, collapse = "-"), "_nk", nk)
  cat("Running model:", label, "\n")
  
  model_result <- run_model(
    data         = df_dm,
    outcome      = outcome,
    exposure     = exposure,
    lag_max      = lag_max,
    knots        = knots,
    nk           = nk,
    adjust_pm    = adjust_pm,
    humidity_type = ifelse(grepl("wt$", exposure), "humidity_wt", "humidity"),
    heatwave_var = "heatwave",
    coldspell_var = "coldspell"
  )
  
  plot_results(model_result, output_dir = "results/plots", label = label)
  
  return(model_result)
})