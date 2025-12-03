#' ANOVA Workflow
#'
#' Performs one-way ANOVA with Tukey HSD post-hoc tests, assumption checks, and visualizations.
#'
#' @param data A data frame (optional if file is provided).
#' @param file Path to a CSV file (optional if data is provided).
#' @param response Character string name of the response variable column.
#' @param group Character string name of the grouping variable column.
#' @param alpha Significance level for assumption checks (default: 0.05).
#' @param conf.level Confidence level for intervals (default: 0.95).
#' @param save_plots Logical, whether to save plots to files (default: FALSE).
#' @param plot_prefix Character string prefix for plot filenames (default: "anova_diagnostics").
#' @param title_prefix Character string prefix for plot titles (default: "ANOVA Results").
#' @return A list containing assumptions checks, ANOVA table, effect sizes, Tukey HSD results, and the model.
#' @export
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter labs theme_minimal guides ggsave
#' @importFrom ggplot2 geom_point geom_hline stat_qq stat_qq_line
#' @importFrom broom tidy
#' @importFrom car leveneTest
#' @importFrom effectsize eta_squared
run_anova_workflow <- function(data = NULL,
                               file = NULL,
                               response,
                               group,
                               alpha = 0.05,
                               conf.level = 0.95,
                               save_plots = FALSE,
                               plot_prefix = "anova_diagnostics",
                               title_prefix = "ANOVA Results") {
  df <- prepare_anova_data(data = data, file = file, response = response, group = group)
  formula <- as.formula("response ~ group")
  
  plot_anova_eda(df, title_prefix, save_plots, plot_prefix)
  
  model <- aov(formula, data = df)
  assumption_checks <- check_anova_assumptions(df, formula, model, alpha)
  
  anova_tbl <- broom::tidy(model)
  effect_tbl <- effectsize::eta_squared(model, ci = conf.level)
  tukey <- TukeyHSD(model, conf.level = conf.level)
  
  plot_residuals(model, title_prefix, save_plots, plot_prefix)
  
  list(
    assumptions = assumption_checks,
    anova_table = anova_tbl,
    effect_sizes = effect_tbl,
    tukey = tukey,
    model = model
  )
}

#' Prepare ANOVA Data
#'
#' Helper function to prepare data for ANOVA analysis.
#'
#' @param data A data frame (optional if file is provided).
#' @param file Path to a CSV file (optional if data is provided).
#' @param response Character string name of the response variable column.
#' @param group Character string name of the grouping variable column.
#' @return A prepared data frame with response and group columns.
#' @keywords internal
prepare_anova_data <- function(data = NULL,
                               file = NULL,
                               response,
                               group) {
  if (is.null(data) && is.null(file)) {
    stop("Provide either a data frame via `data` or a path via `file`.")
  }
  if (!is.null(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
  }
  if (!(response %in% names(data))) stop("Response column not found.")
  if (!(group %in% names(data))) stop("Group column not found.")
  
  df <- data[, c(response, group)]
  names(df) <- c("response", "group")
  df <- na.omit(df)
  df$group <- factor(df$group)
  if (!is.numeric(df$response)) {
    stop("Response column must be numeric.")
  }
  df
}

#' Plot ANOVA EDA
#'
#' Helper function to create exploratory plots for ANOVA.
#'
#' @param df Prepared data frame.
#' @param title_prefix Character string prefix for plot titles.
#' @param save_plots Logical, whether to save plots.
#' @param prefix Character string prefix for filenames.
#' @keywords internal
plot_anova_eda <- function(df, title_prefix, save_plots, prefix) {
  box <- ggplot2::ggplot(df, ggplot2::aes(group, response, fill = group)) +
    ggplot2::geom_boxplot(alpha = 0.75) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
    ggplot2::labs(
      title = paste(title_prefix, "- Group Distributions"),
      x = "Group",
      y = "Response"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none")
  
  if (save_plots) {
    ggplot2::ggsave(paste0(prefix, "_boxplot.png"), box, width = 6, height = 4, dpi = 300)
  } else {
    print(box)
  }
}

#' Plot Residuals
#'
#' Helper function to create residual diagnostic plots.
#'
#' @param model ANOVA model object.
#' @param title_prefix Character string prefix for plot titles.
#' @param save_plots Logical, whether to save plots.
#' @param prefix Character string prefix for filenames.
#' @keywords internal
plot_residuals <- function(model, title_prefix, save_plots, prefix) {
  res_df <- data.frame(
    fitted = fitted(model),
    residuals = resid(model)
  )
  
  res_plot <- ggplot2::ggplot(res_df, ggplot2::aes(fitted, residuals)) +
    ggplot2::geom_point(color = "#2b8cbe") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = paste(title_prefix, "- Residuals vs Fitted"),
      x = "Fitted",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal()
  
  qq_plot <- ggplot2::ggplot(res_df, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(
      title = paste(title_prefix, "- Residual Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()
  
  if (save_plots) {
    ggplot2::ggsave(paste0(prefix, "_residuals.png"), res_plot, width = 6, height = 4, dpi = 300)
    ggplot2::ggsave(paste0(prefix, "_qq.png"), qq_plot, width = 6, height = 4, dpi = 300)
  } else {
    print(res_plot)
    print(qq_plot)
  }
}

#' Check ANOVA Assumptions
#'
#' Helper function to check ANOVA assumptions (normality and homogeneity of variance).
#'
#' @param df Prepared data frame.
#' @param formula ANOVA formula.
#' @param model ANOVA model object.
#' @param alpha Significance level.
#' @return List with Shapiro-Wilk and Levene's test results.
#' @keywords internal
check_anova_assumptions <- function(df, formula, model, alpha) {
  shapiro <- shapiro.test(residuals(model))
  levene <- car::leveneTest(formula, data = df)
  
  list(
    shapiro = list(
      statistic = shapiro$statistic,
      p_value = shapiro$p.value,
      passed = shapiro$p.value > alpha
    ),
    levene = list(
      statistic = levene[1, "F value"],
      p_value = levene[1, "Pr(>F)"],
      passed = levene[1, "Pr(>F)"] > alpha
    )
  )
}

