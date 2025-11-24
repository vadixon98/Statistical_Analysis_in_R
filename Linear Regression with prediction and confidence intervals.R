# Linear regression workflow helper
# --------------------------------------------
# Provides a reusable pipeline for fitting a simple linear regression model,
# generating diagnostics, and exporting prediction/confidence intervals.

required_pkgs <- c("ggplot2", "broom")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing)) {
  stop("Install missing packages before running the workflow: ", paste(missing, collapse = ", "))
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(broom)
})

prepare_regression_data <- function(data = NULL,
                                    file = NULL,
                                    response,
                                    predictor) {
  if (is.null(data) && is.null(file)) {
    stop("Supply either `data` (data frame) or `file` (CSV path).")
  }
  if (!is.null(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
  }
  if (!(response %in% names(data))) stop("Response column not found.")
  if (!(predictor %in% names(data))) stop("Predictor column not found.")
  
  df <- data[, c(response, predictor)]
  names(df) <- c("response", "predictor")
  df <- na.omit(df)
  if (!is.numeric(df$response) || !is.numeric(df$predictor)) {
    stop("Both response and predictor must be numeric.")
  }
  df
}

plot_regression_fit <- function(df, model, title, save_plots, prefix) {
  fitted_df <- augment(model)
  fit_plot <- ggplot(fitted_df, aes(predictor, response)) +
    geom_point(color = "#238b45") +
    geom_line(aes(y = .fitted), color = "#08589e", linewidth = 1) +
    labs(
      title = paste(title, "- Linear Fit"),
      x = "Predictor",
      y = "Response"
    ) +
    theme_minimal()
  
  resid_plot <- ggplot(fitted_df, aes(.fitted, .resid)) +
    geom_point(color = "#7a0177") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste(title, "- Residuals vs Fitted"),
      x = "Fitted",
      y = "Residuals"
    ) +
    theme_minimal()
  
  qq_plot <- ggplot(fitted_df, aes(sample = .resid)) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = paste(title, "- Residual Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  if (save_plots) {
    ggsave(paste0(prefix, "_fit.png"), fit_plot, width = 6, height = 4, dpi = 300)
    ggsave(paste0(prefix, "_residuals.png"), resid_plot, width = 6, height = 4, dpi = 300)
    ggsave(paste0(prefix, "_qq.png"), qq_plot, width = 6, height = 4, dpi = 300)
  } else {
    print(fit_plot)
    print(resid_plot)
    print(qq_plot)
  }
}

generate_intervals <- function(model,
                               newdata = NULL,
                               conf.level = 0.95) {
  if (is.null(newdata)) {
    newdata <- data.frame(predictor = seq(min(model$model$predictor), max(model$model$predictor), length.out = 100))
  }
  pred_int <- predict(model, newdata = newdata, interval = "prediction", level = conf.level)
  conf_int <- predict(model, newdata = newdata, interval = "confidence", level = conf.level)
  cbind(newdata, pred_int, conf_int_lwr = conf_int[, "lwr"], conf_int_upr = conf_int[, "upr"])
}

run_linear_regression_workflow <- function(data = NULL,
                                           file = NULL,
                                           response,
                                           predictor,
                                           conf.level = 0.95,
                                           save_plots = FALSE,
                                           plot_prefix = "linear_regression",
                                           title = "Linear Regression",
                                           prediction_points = NULL) {
  df <- prepare_regression_data(data = data, file = file, response = response, predictor = predictor)
  model <- lm(response ~ predictor, data = df)
  
  diagnostics <- glance(model)
  plot_regression_fit(df, model, title, save_plots, plot_prefix)
  
  interval_grid <- generate_intervals(model, newdata = prediction_points, conf.level = conf.level)
  
  list(
    model = model,
    diagnostics = diagnostics,
    tidy = tidy(model),
    intervals = interval_grid,
    augment = augment(model)
  )
}

predict_from_model <- function(model, new_predictor_values, conf.level = 0.95) {
  new_df <- data.frame(predictor = new_predictor_values)
  predict(model, newdata = new_df, interval = "prediction", level = conf.level)
}

# Example usage:
# data <- data.frame(
#   NEA = c(-94,-57,-29,135,143,151,245,355,392,473,486,535,571,580,620,690),
#   Fat_Gain = c(4.2,3.0,3.7,2.7,3.2,3.6,2.4,1.3,3.8,1.7,1.6,2.2,1.0,0.4,2.3,1.1)
# )
# results <- run_linear_regression_workflow(
#   data = data,
#   response = "Fat_Gain",
#   predictor = "NEA",
#   save_plots = TRUE,
#   plot_prefix = "nea_fat_gain"
# )
# predict_from_model(results$model, c(-50, 0, 100, 200))

