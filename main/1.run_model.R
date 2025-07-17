# Required packages
# install.packages(c("dlnm", "gnm", "splines", "dplyr"))
library(dlnm)
library(gnm)
library(splines)
library(dplyr)

run_model <- function(data, outcome, exposure, lag_max, knots, nk, adjust_pm = FALSE,
                      humidity_type = "humidity", heatwave_var = "heatwave", coldspell_var = "coldspell") {
  # Temperature quantiles
  temp_range <- quantile(temp[[exposure]], probs = seq(0, 1, by = 0.001), na.rm = TRUE)
  temp_q <- quantile(temp[[exposure]], probs = c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99), na.rm = TRUE)
  
  # Data subsets
  data_heat <- filter(data, month %in% c(6, 7, 8, 9)) %>% mutate(city_year = as.factor(paste(city, year)))
  data_cold <- filter(data, month %in% c(12, 1, 2, 3)) %>%
    mutate(season = ifelse(month == 12, as.numeric(as.character(year)) + 1, as.numeric(as.character(year))),
           city_season = as.factor(paste(city, season)))
  
  # Knots
  temp_knots <- quantile(data[[exposure]], probs = knots, na.rm = TRUE)
  humid_knots <- quantile(data[[humidity_type]], probs = knots, na.rm = TRUE)
  humid_heat_knots <- quantile(data_heat[[humidity_type]], probs = knots, na.rm = TRUE)
  humid_cold_knots <- quantile(data_cold[[humidity_type]], probs = knots, na.rm = TRUE)
  lag_knots <- logknots(lag_max, nk)
  
  # Crossbasis
  cb.temp <- crossbasis(data[[exposure]], lag = lag_max, group = data$city,
                        argvar = list(fun = "bs", knots = temp_knots),
                        arglag = list(knots = lag_knots))
  cb.rh <- crossbasis(data[[humidity_type]], lag = lag_max, group = data$city,
                      argvar = list(fun = "bs", knots = humid_knots),
                      arglag = list(knots = lag_knots))
  cb.pm <- crossbasis(data$PM10, lag = lag_max, group = data$city,
                      argvar = list(fun = "lin"), arglag = list(knots = lag_knots))
  
  cb.heat <- crossbasis(data_heat[[heatwave_var]], lag = lag_max, group = data_heat$city_year,
                        argvar = list(fun = "lin"), arglag = list(knots = lag_knots))
  cb.cold <- crossbasis(data_cold[[coldspell_var]], lag = lag_max, group = data_cold$city_season,
                        argvar = list(fun = "lin"), arglag = list(knots = lag_knots))
  
  cb.rh_h <- crossbasis(data_heat[[humidity_type]], lag = lag_max, group = data_heat$city_year,
                        argvar = list(fun = "bs", knots = humid_heat_knots),
                        arglag = list(knots = lag_knots))
  cb.rh_c <- crossbasis(data_cold[[humidity_type]], lag = lag_max, group = data_cold$city_season,
                        argvar = list(fun = "bs", knots = humid_cold_knots),
                        arglag = list(knots = lag_knots))
  
  cb.pm_h <- crossbasis(data_heat$PM10, lag = lag_max, group = data_heat$city_year,
                        argvar = list(fun = "lin"), arglag = list(knots = lag_knots))
  cb.pm_c <- crossbasis(data_cold$PM10, lag = lag_max, group = data_cold$city_season,
                        argvar = list(fun = "lin"), arglag = list(knots = lag_knots))
  
  # Formula
  base_terms <- paste0("cb.temp", if (exposure %in% c("AT_hw", "HI_C")) "" else " + cb.rh", " + as.factor(holiday)")
  pm_term <- if (adjust_pm) "+ cb.pm" else ""
  fmla_temp <- as.formula(paste0(outcome, " ~ ", base_terms, pm_term))
  fmla_heat <- as.formula(paste0(outcome, " ~ cb.heat + cb.rh_h + as.factor(holiday)", if (adjust_pm) " + cb.pm_h" else ""))
  fmla_cold <- as.formula(paste0(outcome, " ~ cb.cold + cb.rh_c + as.factor(holiday)", if (adjust_pm) " + cb.pm_c" else ""))
  
  # Models
  model_temp <- gnm(fmla_temp, data = data, family = quasipoisson(), eliminate = factor(stratum))
  model_heat <- gnm(fmla_heat, data = data_heat, family = quasipoisson(), eliminate = factor(stratum))
  model_cold <- gnm(fmla_cold, data = data_cold, family = quasipoisson(), eliminate = factor(stratum))
  
  # Predictions
  pred_temp <- crosspred(cb.temp, model_temp, at = temp_range, bylag = 1, cen = temp_q["50%"], cumul = TRUE)
  pred_heat <- crosspred(cb.heat, model_heat, cumul = TRUE)
  pred_cold <- crosspred(cb.cold, model_cold, cumul = TRUE)
  
  # QAIC Calculation Function
  get_qaic <- function(model) {
    phi <- summary(model)$dispersion
    logLik_approx <- -0.5 * sum(residuals(model, type = "deviance")^2)
    k <- length(coef(model))
    QAIC <- -2 * logLik_approx + 2 * k / phi
    return(QAIC)
  }
  
  list(
    prediction       = pred_temp,
    prediction_heat  = pred_heat,
    prediction_cold  = pred_cold,
    temp_range       = temp_range,
    temp_quantiles   = temp_q,
    qaic_temp        = get_qaic(model_temp),
    qaic_heat        = get_qaic(model_heat),
    qaic_cold        = get_qaic(model_cold)
  )
}
