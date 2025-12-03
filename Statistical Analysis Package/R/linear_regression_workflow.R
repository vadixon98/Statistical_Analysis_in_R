#' Linear Regression Workflow
#'
#' Comprehensive linear regression workflow with diagnostics, prediction intervals, and visualizations.
#'
#' @param data A data frame (optional if file is provided).
#' @param file Path to a CSV file (optional if data is provided).
#' @param response Character string name of the response variable column.
#' @param predictor Character string name of the predictor variable column.
#' @param conf.level Confidence level for intervals (default: 0.95).
#' @param save_plots Logical, whether to save plots (default: FALSE).
#' @param plot_prefix Character string prefix for plot filenames (default: "linear_regression").
#' @param title Character string title for plots (default: "Linear Regression").
#' @param prediction_points Numeric vector of predictor values for predictions (optional).
#' @return A list containing the model, diagnostics, tidy coefficients, prediction/confidence intervals, and augmented data.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal ggsave stat_qq stat_qq_line
#' @importFrom broom tidy glance augment
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
  
  diagnostics <- broom::glance(model)
  plot_regression_fit(df, model, title, save_plots, plot_prefix)
  
  interval_grid <- generate_intervals(model, newdata = prediction_points, conf.level = conf.level)
  
  list(
    model = model,
    diagnostics = diagnostics,
    tidy = broom::tidy(model),
    intervals = interval_grid,
    augment = broom::augment(model)
  )
}

#' Predict from Model
#'
#' Generate predictions with intervals from a linear regression model.
#'
#' @param model Linear model object from run_linear_regression_workflow.
#' @param new_predictor_values Numeric vector of predictor values for predictions.
#' @param conf.level Confidence level (default: 0.95).
#' @return Matrix with predictions and intervals.
#' @export
predict_from_model <- function(model, new_predictor_values, conf.level = 0.95) {
  new_df <- data.frame(predictor = new_predictor_values)
  predict(model, newdata = new_df, interval = "prediction", level = conf.level)
}

#' Prepare Regression Data
#'
#' Helper function to prepare data for regression analysis.
#'
#' @param data A data frame (optional if file is provided).
#' @param file Path to a CSV file (optional if data is provided).
#' @param response Character string name of the response variable column.
#' @param predictor Character string name of the predictor variable column.
#' @return A prepared data frame.
#' @keywords internal
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

#' Plot Regression Fit
#'
#' Helper function to create regression diagnostic plots.
#'
#' @param df Prepared data frame.
#' @param model Linear model object.
#' @param title Character string title for plots.
#' @param save_plots Logical, whether to save plots.
#' @param prefix Character string prefix for filenames.
#' @keywords internal
plot_regression_fit <- function(df, model, title, save_plots, prefix) {
  fitted_df <- broom::augment(model)
  fit_plot <- ggplot2::ggplot(fitted_df, ggplot2::aes(predictor, response)) +
    ggplot2::geom_point(color = "#238b45") +
    ggplot2::geom_line(ggplot2::aes(y = .fitted), color = "#08589e", linewidth = 1) +
    ggplot2::labs(
      title = paste(title, "- Linear Fit"),
      x = "Predictor",
      y = "Response"
    ) +
    ggplot2::theme_minimal()
  
  resid_plot <- ggplot2::ggplot(fitted_df, ggplot2::aes(.fitted, .resid)) +
    ggplot2::geom_point(color = "#7a0177") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = paste(title, "- Residuals vs Fitted"),
      x = "Fitted",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal()
  
  qq_plot <- ggplot2::ggplot(fitted_df, ggplot2::aes(sample = .resid)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(
      title = paste(title, "- Residual Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()
  
  if (save_plots) {
    ggplot2::ggsave(paste0(prefix, "_fit.png"), fit_plot, width = 6, height = 4, dpi = 300)
    ggplot2::ggsave(paste0(prefix, "_residuals.png"), resid_plot, width = 6, height = 4, dpi = 300)
    ggplot2::ggsave(paste0(prefix, "_qq.png"), qq_plot, width = 6, height = 4, dpi = 300)
  } else {
    print(fit_plot)
    print(resid_plot)
    print(qq_plot)
  }
}

#' Generate Intervals
#'
#' Helper function to generate prediction and confidence intervals.
#'
#' @param model Linear model object.
#' @param newdata Data frame with predictor values (optional).
#' @param conf.level Confidence level.
#' @return Data frame with predictions and intervals.
#' @keywords internal
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

