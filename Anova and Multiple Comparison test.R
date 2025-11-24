# ANOVA + Tukey workflow helper
# --------------------------------------------
# Source this file, then call run_anova_workflow() with your data frame or a CSV.

required_pkgs <- c("ggplot2", "broom", "car", "effectsize")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs)) {
  stop(
    "Install missing packages before running the workflow: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(broom)
  library(car)
  library(effectsize)
})

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

plot_anova_eda <- function(df, title_prefix, save_plots, prefix) {
  box <- ggplot(df, aes(group, response, fill = group)) +
    geom_boxplot(alpha = 0.75) +
    geom_jitter(width = 0.1, alpha = 0.4) +
    labs(
      title = paste(title_prefix, "- Group Distributions"),
      x = "Group",
      y = "Response"
    ) +
    theme_minimal() +
    guides(fill = "none")
  
  if (save_plots) {
    ggsave(paste0(prefix, "_boxplot.png"), box, width = 6, height = 4, dpi = 300)
  } else {
    print(box)
  }
}

plot_residuals <- function(model, title_prefix, save_plots, prefix) {
  res_df <- data.frame(
    fitted = fitted(model),
    residuals = resid(model)
  )
  
  res_plot <- ggplot(res_df, aes(fitted, residuals)) +
    geom_point(color = "#2b8cbe") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste(title_prefix, "- Residuals vs Fitted"),
      x = "Fitted",
      y = "Residuals"
    ) +
    theme_minimal()
  
  qq_plot <- ggplot(res_df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(
      title = paste(title_prefix, "- Residual Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  if (save_plots) {
    ggsave(paste0(prefix, "_residuals.png"), res_plot, width = 6, height = 4, dpi = 300)
    ggsave(paste0(prefix, "_qq.png"), qq_plot, width = 6, height = 4, dpi = 300)
  } else {
    print(res_plot)
    print(qq_plot)
  }
}

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

# Example usage
# data <- read.csv("rat_study.csv")
# results <- run_anova_workflow(
#   data = data,
#   response = "weight",
#   group = "diet",
#   save_plots = TRUE,
#   plot_prefix = "rat_study"
# )
# results$anova_table
