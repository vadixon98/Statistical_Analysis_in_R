<div align="center">

# 📊 Statistical Analysis in R

Modern, script-first workflows for the most common hypothesis tests and exploratory analytics — all in pure R.

[![R >= 4.0](https://img.shields.io/badge/R-%3E%3D%204.0-276DC3?logo=r&logoColor=white)](#requirements) [![Made for RStudio](https://img.shields.io/badge/RStudio-friendly-75AADB?logo=rstudio&logoColor=white)](#getting-started) [![MIT License](https://img.shields.io/badge/License-MIT-black.svg)](#license) [![Contributions Welcome](https://img.shields.io/badge/PRs-welcome-4CAF50.svg)](#contributing)

</div>

---

## ✨ Why This Repo

- **Single-run scripts** you can source directly into RStudio or any R console.
- **Interactive helpers** for quick EDA, diagnostics, and reporting.
- **Readable outputs**: tables, plots, and effect sizes ready for stakeholders.
- **Zero packages beyond tidy staples** (`ggplot2`, `reshape2`, `stats`).

---

## 🚀 Getting Started

```bash
git clone <repository-url>
cd <repository-directory>
```

```r
install.packages(c("ggplot2", "reshape2"))
```

> ✅ Requires R ≥ 4.0. RStudio recommended for plot panes and interactive prompts.

---

## 🧰 Script Gallery

| Script | What it does | Highlights |
| --- | --- | --- |
| `Anova and Multiple Comparison test.R` | One-way ANOVA + Tukey HSD | Automatic assumption checks, tidy post-hoc table |
| `Implementing a Chi Square test.R` | Chi-square test of independence | 2×2 contingency helper + visualization |
| `Linear regression with prediction and confidence intervals.R` | Simple linear model | Prediction bands + confidence intervals |
| `Various t.tests.R` | One-sample, paired, and two-sample t-tests | Effect sizes, Shapiro-Wilk, variance checks |
| `Interactive Stat Tools Extended.R` | RStudio gadget-style helpers | See full menu below |

---

## 🧪 Interactive Suite Breakdown

```r
source("Interactive Stat Tools Extended.R")
```

- Summary stats (count, mean, median, SD, IQR, missing values)
- Frequency tables + optional bar plots
- Correlation matrices with heatmaps
- Two-proportion test with visualization
- Normality diagnostics (Shapiro-Wilk, skewness, kurtosis, histogram, Q-Q plot)
- Outlier detection via IQR & Z-score methods
- Two-sample t-test with Cohen’s d
- Chi-square association testing
- Simple linear regression + diagnostic panels
- Simple logistic regression (odds ratios + confusion matrix)
- Pairwise t-tests with multiple-comparison corrections

---

## 💡 Quick Usage Patterns

- **ANOVA**: Replace `file.choose()` with a fixed CSV path for reproducibility.
- **Chi-square**: Tweak the supplied contingency matrix to match observed counts.
- **Regression**: Customize `ggplot2` layers to match your reporting theme.
- **t-tests**: Use `mu` for one-sample hypotheses; set `paired = TRUE` when needed.
- **Interactive tools**: Source once, then call helpers as needed in your session.

---

## 🤝 Contributing

PRs, issues, and ideas are welcome! Fork the repo, create a feature branch, and open a pull request describing the change. Screenshots/GIFs of interactive tools are appreciated.

---

## 📄 License

Released under the MIT License. Include a copy when redistributing and cite this repo if it helps your work.
