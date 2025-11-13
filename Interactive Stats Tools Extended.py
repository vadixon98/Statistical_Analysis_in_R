"""
Interactive Stat Tools Extended.py
Interactive versions of common small statistical analyses for use in Python/Jupyter.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from scipy.stats import shapiro, chi2_contingency, ttest_ind, ttest_rel
from statsmodels.stats.proportion import proportions_ztest
from statsmodels.stats.multitest import multipletests
from statsmodels.formula.api import ols
from statsmodels.stats.outliers_influence import variance_inflation_factor
import warnings
warnings.filterwarnings('ignore')

# Set style for plots
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (10, 6)


# ---- 1. Summary statistics ------------------------------------------------
def summary_stats(df, cols=None):
    """
    Computes descriptive statistics for numeric columns in a data frame
    
    Parameters:
        df: pandas DataFrame containing the data
        cols: optional list of column names to summarize; 
              if None, summarizes all numeric columns
    
    Returns:
        DataFrame with rows (count, mean, median, sd, IQR, min, max, missing) 
        and columns for each numeric variable, rounded to 4 decimal places
    """
    # Identify numeric columns to summarize
    if cols is None:
        # If no columns specified, use all numeric columns
        nums = df.select_dtypes(include=[np.number]).columns.tolist()
    else:
        # Otherwise, use intersection of specified columns and numeric columns
        nums = [col for col in cols if col in df.columns]
        nums = [col for col in nums if pd.api.types.is_numeric_dtype(df[col])]
    
    if len(nums) == 0:
        raise ValueError("No numeric columns to summarize.")
    
    # Helper function to compute summary statistics for a single variable
    def summary_fn(x):
        nmiss = x.isna().sum()  # Count missing values
        x2 = x.dropna()         # Remove missing values for calculations
        
        if len(x2) == 0:
            return pd.Series({
                'count': 0,
                'mean': np.nan,
                'median': np.nan,
                'sd': np.nan,
                'IQR': np.nan,
                'min': np.nan,
                'max': np.nan,
                'missing': nmiss
            })
        
        return pd.Series({
            'count': len(x2),           # Sample size (non-missing)
            'mean': np.mean(x2),        # Arithmetic mean
            'median': np.median(x2),    # Median (50th percentile)
            'sd': np.std(x2, ddof=1),    # Standard deviation (sample)
            'IQR': np.percentile(x2, 75) - np.percentile(x2, 25),  # Interquartile range
            'min': np.min(x2),          # Minimum value
            'max': np.max(x2),          # Maximum value
            'missing': nmiss            # Count of missing values
        })
    
    # Apply summary function to each numeric column
    res = pd.DataFrame({col: summary_fn(df[col]) for col in nums})
    return res.round(4)


# ---- 2. Frequency table ---------------------------------------------------
def frequency_table(df, column, plot=True, plot_path=None):
    """
    Creates a frequency table and optional bar chart for a categorical variable
    
    Parameters:
        df: pandas DataFrame containing the data
        column: string name of the column to analyze
        plot: bool, whether to create a bar chart (default: True)
        plot_path: optional string path to save the plot; 
                   if None, plot is displayed only
    
    Returns:
        Dictionary with 'counts' (frequency table) and 'rel_freq' (proportions)
    """
    if column not in df.columns:
        raise ValueError("Column not found")
    
    # Create frequency table (counts for each level)
    tbl = df[column].value_counts().sort_index()
    # Calculate relative frequencies (proportions)
    rel = df[column].value_counts(normalize=True).sort_index()
    
    # Print results
    print("Counts:")
    print(tbl)
    print("\nRelative frequencies:")
    print(rel.round(4))
    
    # Create bar chart if requested
    if plot:
        plt.figure(figsize=(8, 5))
        sns.barplot(x=tbl.index.astype(str), y=tbl.values, palette='viridis')
        plt.title(f"Frequency of {column}")
        plt.xlabel(column)
        plt.ylabel("Count")
        plt.xticks(rotation=45, ha='right')
        plt.tight_layout()
        
        if plot_path is not None:
            # Save plot to file
            plt.savefig(plot_path, dpi=300, bbox_inches='tight')
            print(f"Saved barplot to {plot_path}")
        else:
            # Display plot
            plt.show()
        plt.close()
    
    return {'counts': tbl, 'rel_freq': rel}


# ---- 3. Correlation matrix ------------------------------------------------
def correlation_matrix(df, method='pearson', plot=True, plot_path=None):
    """
    Computes correlation matrix for all numeric variables and optionally visualizes it
    
    Parameters:
        df: pandas DataFrame containing the data
        method: correlation method, either 'pearson' (default), 'spearman', or 'kendall'
        plot: bool, whether to create a heatmap visualization (default: True)
        plot_path: optional string path to save the plot; 
                   if None, plot is displayed only
    
    Returns:
        Correlation matrix (symmetric DataFrame with values between -1 and 1)
    """
    # Extract only numeric columns
    num_df = df.select_dtypes(include=[np.number])
    if num_df.shape[1] < 2:
        raise ValueError("Need at least two numeric columns")
    
    # Compute correlation matrix using pairwise complete observations
    # (handles missing values by using all available pairs for each variable pair)
    corr = num_df.corr(method=method)
    print(corr.round(3))
    
    # Create heatmap visualization
    if plot:
        plt.figure(figsize=(10, 8))
        mask = np.triu(np.ones_like(corr, dtype=bool))  # Mask upper triangle for cleaner look
        sns.heatmap(corr, annot=True, fmt='.2f', cmap='RdBu_r', center=0,
                   square=True, linewidths=0.5, cbar_kws={"shrink": 0.8},
                   vmin=-1, vmax=1, mask=mask)
        plt.title(f"Correlation matrix ({method})")
        plt.tight_layout()
        
        if plot_path is not None:
            plt.savefig(plot_path, dpi=300, bbox_inches='tight')
            print(f"Saved correlation heatmap to {plot_path}")
        else:
            plt.show()
        plt.close()
    
    return corr


# ---- 4. Proportion test ---------------------------------------------------
def proportion_test(df, outcome_col, group_col, correct=False):
    """
    Performs a two-sample test of proportions (comparing proportions between two groups)
    
    Parameters:
        df: pandas DataFrame containing the data
        outcome_col: string name of binary outcome column (coded as 0/1)
        group_col: string name of grouping variable (must have exactly 2 levels)
        correct: bool, whether to apply continuity correction (default: False)
    
    Returns:
        Dictionary containing test statistics and p-value
    """
    if outcome_col not in df.columns or group_col not in df.columns:
        raise ValueError("Columns missing")
    
    # Check that group column has exactly two levels
    groups = df[group_col].unique()
    if len(groups) != 2:
        raise ValueError("Group column must have exactly two levels")
    
    # Count successes (outcome == 1) for each group
    counts = df.groupby(group_col)[outcome_col].apply(lambda x: (x == 1).sum()).values
    # Count total observations (non-missing) for each group
    totals = df.groupby(group_col)[outcome_col].apply(lambda x: x.notna().sum()).values
    
    print("Success counts:")
    print(dict(zip(groups, counts)))
    print("Total per group:")
    print(dict(zip(groups, totals)))
    
    # Perform two-sample proportion test
    # Using statsmodels proportions_ztest (equivalent to R's prop.test)
    zstat, pvalue = proportions_ztest(count=counts, nobs=totals, alternative='two-sided')
    
    # Calculate proportions
    prop1 = counts[0] / totals[0]
    prop2 = counts[1] / totals[1]
    
    result = {
        'z_statistic': zstat,
        'p_value': pvalue,
        'proportion_1': prop1,
        'proportion_2': prop2,
        'count_1': counts[0],
        'count_2': counts[1],
        'total_1': totals[0],
        'total_2': totals[1]
    }
    
    print(f"\nTwo-sample proportion test:")
    print(f"Z-statistic: {zstat:.4f}")
    print(f"P-value: {pvalue:.4f}")
    print(f"Proportion 1 ({groups[0]}): {prop1:.4f}")
    print(f"Proportion 2 ({groups[1]}): {prop2:.4f}")
    
    return result


# ---- 5. Normality check ---------------------------------------------------
def normality_check(df, variable, out_prefix="normality"):
    """
    Assesses whether a numeric variable follows a normal distribution using 
    descriptive statistics, Shapiro-Wilk test, histogram, and Q-Q plot
    
    Parameters:
        df: pandas DataFrame containing the data
        variable: string name of the numeric variable to check
        out_prefix: string prefix for output file names (default: "normality")
    
    Returns:
        None (prints results and saves plots to files)
    """
    if variable not in df.columns:
        raise ValueError("Variable not found")
    
    # Extract variable and remove missing values
    x = df[variable].dropna()
    if not pd.api.types.is_numeric_dtype(x):
        raise ValueError("Variable must be numeric")
    
    n = len(x)
    mean_x = np.mean(x)
    sd_x = np.std(x, ddof=1)
    
    # Calculate skewness: measure of asymmetry (0 = symmetric, >0 = right-skewed, <0 = left-skewed)
    skew = stats.skew(x)
    # Calculate excess kurtosis: measure of tail heaviness (0 = normal, >0 = heavy tails, <0 = light tails)
    kurt = stats.kurtosis(x)  # Already excess kurtosis (subtracts 3)
    
    print(f"Variable: {variable}")
    print(f"N: {n}")
    print(f"Mean: {mean_x:.3f}")
    print(f"SD: {sd_x:.3f}")
    print(f"Skewness: {skew:.3f}")
    print(f"Kurtosis (excess): {kurt:.3f}\n")
    
    # Shapiro-Wilk test for normality (only valid for sample sizes 3-5000)
    if 3 <= n <= 5000:
        sw_stat, sw_pvalue = shapiro(x)
        print("Shapiro-Wilk test:")
        print(f"  Statistic: {sw_stat:.4f}")
        print(f"  P-value: {sw_pvalue:.4f}")
        if sw_pvalue < 0.05:
            print("  Result: Reject null hypothesis (data is not normally distributed)")
        else:
            print("  Result: Fail to reject null hypothesis (data may be normally distributed)")
    else:
        print("Shapiro-Wilk not run (n out of range 3-5000)")
    
    # Create histogram with normal distribution overlay
    plt.figure(figsize=(8, 5))
    plt.hist(x, bins=30, density=True, alpha=0.7, color='grey', edgecolor='black', label='Data')
    
    # Overlay normal distribution
    x_norm = np.linspace(x.min(), x.max(), 100)
    y_norm = stats.norm.pdf(x_norm, mean_x, sd_x)
    plt.plot(x_norm, y_norm, 'r--', linewidth=2, label='Normal distribution')
    plt.title(f"Histogram of {variable}")
    plt.xlabel(variable)
    plt.ylabel("Density")
    plt.legend()
    plt.tight_layout()
    
    hist_path = f"{out_prefix}_{variable}_hist.png"
    plt.savefig(hist_path, dpi=300, bbox_inches='tight')
    print(f"\nSaved histogram to {hist_path}")
    plt.close()
    
    # Create Q-Q plot (quantile-quantile plot) to assess normality
    # Points should fall approximately on the line if data is normally distributed
    plt.figure(figsize=(8, 5))
    stats.probplot(x, dist="norm", plot=plt)
    plt.title(f"Q-Q Plot of {variable}")
    plt.tight_layout()
    
    qq_path = f"{out_prefix}_{variable}_qq.png"
    plt.savefig(qq_path, dpi=300, bbox_inches='tight')
    print(f"Saved Q-Q plot to {qq_path}")
    plt.close()


# ---- 6. Outlier detection ------------------------------------------------
def outlier_detection(df, column, method='iqr', threshold=1.5):
    """
    Identifies outliers in a numeric column using IQR rule or Z-score method
    
    Parameters:
        df: pandas DataFrame containing the data
        column: string name of the numeric column to analyze
        method: string, either 'iqr' (default) or 'zscore'
        threshold: numeric threshold for outlier detection
                   - For IQR: multiplier for IQR (default 1.5, typical range 1.5-3.0)
                   - For Z-score: absolute Z-score threshold (default 1.5)
    
    Returns:
        pandas Index of row indices containing outliers
    """
    if column not in df.columns:
        raise ValueError("Column missing")
    
    x = df[column]
    xnum = x.dropna()  # Non-missing values for calculations
    if not pd.api.types.is_numeric_dtype(xnum):
        raise ValueError("Column must be numeric")
    
    outlier_idx = None
    
    if method == 'iqr':
        # IQR method: outliers are values outside Q1 - threshold*IQR and Q3 + threshold*IQR
        Q1 = np.percentile(xnum, 25)  # First quartile (25th percentile)
        Q3 = np.percentile(xnum, 75)   # Third quartile (75th percentile)
        IQRv = Q3 - Q1                # Interquartile range
        lower = Q1 - threshold * IQRv  # Lower bound
        upper = Q3 + threshold * IQRv  # Upper bound
        # Find outliers (using original x to preserve NA positions)
        outlier_idx = df.index[(x < lower) | (x > upper)]
        print(f"IQR rule: lower={lower:.3f} upper={upper:.3f}")
    elif method == 'zscore':
        # Z-score method: outliers are values with |z| > threshold
        mu = np.mean(xnum)
        sdv = np.std(xnum, ddof=1)
        z = (x - mu) / sdv  # Calculate Z-scores (using original x to preserve NA positions)
        outlier_idx = df.index[np.abs(z) > threshold]
        print(f"Z-score threshold: |z| > {threshold:.2f}")
    else:
        raise ValueError("Method must be 'iqr' or 'zscore'")
    
    print(f"Found {len(outlier_idx)} outliers in column '{column}':")
    if len(outlier_idx) > 0:
        outlier_df = pd.DataFrame({
            'index': outlier_idx,
            'value': df.loc[outlier_idx, column].values
        })
        print(outlier_df.to_string(index=False))
    
    return outlier_idx


# ---- 7. Two-sample t-test -------------------------------------------------
def two_sample_ttest(df, outcome, group, paired=False, var_equal=False):
    """
    Performs a two-sample t-test to compare means between two groups
    
    Parameters:
        df: pandas DataFrame containing the data
        outcome: string name of the numeric outcome variable
        group: string name of the grouping variable (must have exactly 2 levels)
        paired: bool, whether to perform paired t-test (default: False)
        var_equal: bool, whether to assume equal variances (default: False, uses Welch's test)
    
    Returns:
        Dictionary containing test statistics, p-value, and confidence interval
    """
    if outcome not in df.columns or group not in df.columns:
        raise ValueError("Columns missing")
    
    groups = df[group].unique()
    if len(groups) != 2:
        raise ValueError("Group must have exactly two levels")
    
    # Split data by groups
    grp1 = df[df[group] == groups[0]][outcome].dropna()
    grp2 = df[df[group] == groups[1]][outcome].dropna()
    
    # Perform t-test
    if paired:
        if len(grp1) != len(grp2):
            raise ValueError("For paired test, groups must have equal sizes")
        test_result = ttest_rel(grp1, grp2)
        test_type = "Paired t-test"
    else:
        test_result = ttest_ind(grp1, grp2, equal_var=var_equal)
        test_type = "Welch's t-test" if not var_equal else "Student's t-test"
    
    t_stat = test_result.statistic
    p_value = test_result.pvalue
    
    # Calculate means and standard deviations
    m1 = np.mean(grp1)
    m2 = np.mean(grp2)
    s1 = np.std(grp1, ddof=1)
    s2 = np.std(grp2, ddof=1)
    n1 = len(grp1)
    n2 = len(grp2)
    
    # Calculate Cohen's d (effect size measure)
    # Cohen's d = (mean1 - mean2) / pooled_standard_deviation
    # Calculate pooled standard deviation
    sp = np.sqrt(((n1 - 1) * s1**2 + (n2 - 1) * s2**2) / (n1 + n2 - 2))
    d = (m1 - m2) / sp
    
    # Calculate confidence interval
    df_degrees = n1 + n2 - 2 if var_equal else test_result.df
    ci = stats.t.interval(0.95, df_degrees, loc=m1 - m2, scale=test_result.stderr if hasattr(test_result, 'stderr') else sp * np.sqrt(1/n1 + 1/n2))
    
    print(f"{test_type}:")
    print(f"  Group 1 ({groups[0]}): mean={m1:.4f}, sd={s1:.4f}, n={n1}")
    print(f"  Group 2 ({groups[1]}): mean={m2:.4f}, sd={s2:.4f}, n={n2}")
    print(f"  t-statistic: {t_stat:.4f}")
    print(f"  P-value: {p_value:.4f}")
    print(f"  Cohen's d (pooled): {d:.3f}")
    print(f"  95% CI for difference: [{ci[0]:.4f}, {ci[1]:.4f}]")
    
    result = {
        't_statistic': t_stat,
        'p_value': p_value,
        'mean_1': m1,
        'mean_2': m2,
        'cohens_d': d,
        'confidence_interval': ci
    }
    
    return result


# ---- 8. Chi-square test --------------------------------------------------
def chi_square_test(df, col1, col2):
    """
    Performs a chi-square test of independence for two categorical variables
    Tests whether there is a significant association between the two variables
    
    Parameters:
        df: pandas DataFrame containing the data
        col1: string name of the first categorical variable
        col2: string name of the second categorical variable
    
    Returns:
        Dictionary containing test statistics and p-value
    """
    if col1 not in df.columns or col2 not in df.columns:
        raise ValueError("Columns missing")
    
    # Create contingency table (cross-tabulation)
    tbl = pd.crosstab(df[col1], df[col2])
    print("Contingency table:")
    print(tbl)
    print()
    
    # Perform chi-square test of independence
    chi2, p_value, dof, expected = chi2_contingency(tbl)
    
    print("Chi-square test of independence:")
    print(f"  Chi-square statistic: {chi2:.4f}")
    print(f"  Degrees of freedom: {dof}")
    print(f"  P-value: {p_value:.4f}")
    
    if p_value < 0.05:
        print("  Result: Reject null hypothesis (variables are associated)")
    else:
        print("  Result: Fail to reject null hypothesis (no significant association)")
    
    result = {
        'chi2_statistic': chi2,
        'p_value': p_value,
        'degrees_of_freedom': dof,
        'expected_frequencies': expected
    }
    
    return result


# ---- 9. Simple linear regression -----------------------------------------
def simple_linear_regression(df, response, predictor):
    """
    Fits a simple linear regression model (one predictor) and provides diagnostics
    
    Parameters:
        df: pandas DataFrame containing the data
        response: string name of the numeric response/dependent variable
        predictor: string name of the numeric predictor/independent variable
    
    Returns:
        Dictionary containing model results, coefficients, and statistics
    """
    if response not in df.columns or predictor not in df.columns:
        raise ValueError("Columns missing")
    
    # Remove missing values
    df_clean = df[[response, predictor]].dropna()
    
    # Fit linear model using ordinary least squares
    from sklearn.linear_model import LinearRegression
    from sklearn.metrics import r2_score
    
    X = df_clean[[predictor]]
    y = df_clean[response]
    
    model = LinearRegression()
    model.fit(X, y)
    
    # Get predictions
    y_pred = model.predict(X)
    residuals = y - y_pred
    
    # Calculate R-squared
    r2 = r2_score(y, y_pred)
    
    # Calculate standard errors and t-statistics manually
    n = len(y)
    mse = np.sum(residuals**2) / (n - 2)
    se_intercept = np.sqrt(mse * (1/n + np.mean(X.values)**2 / np.sum((X.values - np.mean(X.values))**2)))
    se_slope = np.sqrt(mse / np.sum((X.values - np.mean(X.values))**2))
    
    t_intercept = model.intercept_ / se_intercept
    t_slope = model.coef_[0] / se_slope
    
    p_intercept = 2 * (1 - stats.t.cdf(abs(t_intercept), n - 2))
    p_slope = 2 * (1 - stats.t.cdf(abs(t_slope), n - 2))
    
    print("Linear Regression Results:")
    print(f"  Response: {response}")
    print(f"  Predictor: {predictor}")
    print(f"  Intercept: {model.intercept_:.4f} (SE: {se_intercept:.4f}, t: {t_intercept:.4f}, p: {p_intercept:.4f})")
    print(f"  Slope ({predictor}): {model.coef_[0]:.4f} (SE: {se_slope:.4f}, t: {t_slope:.4f}, p: {p_slope:.4f})")
    print(f"  R-squared: {r2:.4f}")
    print(f"  Adjusted R-squared: {1 - (1 - r2) * (n - 1) / (n - 2):.4f}")
    
    # Diagnostic plot: residuals vs fitted values
    # Should show random scatter around zero with no patterns
    plt.figure(figsize=(8, 5))
    plt.scatter(y_pred, residuals, alpha=0.6)
    plt.axhline(y=0, color='r', linestyle='--', linewidth=2)
    plt.title(f"Residuals vs Fitted: {response} ~ {predictor}")
    plt.xlabel("Fitted")
    plt.ylabel("Residuals")
    plt.tight_layout()
    plt.show()
    plt.close()
    
    result = {
        'intercept': model.intercept_,
        'slope': model.coef_[0],
        'r_squared': r2,
        'residuals': residuals,
        'fitted': y_pred,
        'model': model
    }
    
    return result


# ----10. Simple logistic regression ---------------------------------------
def simple_logistic_regression(df, outcome, predictor):
    """
    Fits a simple logistic regression model (one predictor) for binary outcomes
    
    Parameters:
        df: pandas DataFrame containing the data
        outcome: string name of the binary outcome variable (must be coded as 0/1)
        predictor: string name of the predictor variable (can be numeric or categorical)
    
    Returns:
        Dictionary containing model results and statistics
    """
    if outcome not in df.columns or predictor not in df.columns:
        raise ValueError("Columns missing")
    
    if not all(df[outcome].dropna().isin([0, 1])):
        raise ValueError("Outcome must be coded 0/1")
    
    # Remove missing values
    df_clean = df[[outcome, predictor]].dropna()
    
    # Fit logistic regression model using maximum likelihood estimation
    from sklearn.linear_model import LogisticRegression
    
    X = df_clean[[predictor]] if pd.api.types.is_numeric_dtype(df_clean[predictor]) else pd.get_dummies(df_clean[[predictor]], drop_first=True)
    y = df_clean[outcome]
    
    model = LogisticRegression()
    model.fit(X, y)
    
    # Get coefficients and odds ratios
    if X.shape[1] == 1:
        coef = model.coef_[0][0]
        intercept = model.intercept_[0]
        OR = np.exp(coef)
        print("Logistic Regression Results:")
        print(f"  Intercept: {intercept:.4f}")
        print(f"  Coefficient ({predictor}): {coef:.4f}")
        print(f"  Odds ratios:")
        print(f"    {predictor}: {OR:.4f}")
    else:
        print("Logistic Regression Results:")
        print(f"  Intercept: {model.intercept_[0]:.4f}")
        for i, col in enumerate(X.columns):
            print(f"  Coefficient ({col}): {model.coef_[0][i]:.4f}")
        print("  Odds ratios:")
        for i, col in enumerate(X.columns):
            print(f"    {col}: {np.exp(model.coef_[0][i]):.4f}")
    
    # Generate predicted probabilities
    probs = model.predict_proba(X)[:, 1]
    
    # Create confusion matrix using 0.5 as classification threshold
    pred = (probs >= 0.5).astype(int)
    from sklearn.metrics import confusion_matrix
    tab = confusion_matrix(y, pred)
    print("\nConfusion matrix (threshold=0.5):")
    print("                Predicted")
    print("                0      1")
    print(f"Actual 0    {tab[0,0]:4d}  {tab[0,1]:4d}")
    print(f"        1    {tab[1,0]:4d}  {tab[1,1]:4d}")
    
    result = {
        'model': model,
        'probabilities': probs,
        'predictions': pred,
        'confusion_matrix': tab
    }
    
    return result


# ----11. Pairwise comparisons (t-tests) -----------------------------------
def pairwise_comparisons(df, outcome, group, p_adjust_method='bonferroni'):
    """
    Performs pairwise t-tests between all groups with p-value adjustment for multiple comparisons
    Useful for post-hoc analysis after ANOVA or when comparing multiple groups
    
    Parameters:
        df: pandas DataFrame containing the data
        outcome: string name of the numeric outcome variable
        group: string name of the grouping variable (can have 2+ levels)
        p_adjust_method: string method for p-value adjustment
                         Options: 'bonferroni' (default), 'holm', 'fdr', 'none', etc.
                         See scipy.stats.multitest for full list
    
    Returns:
        DataFrame containing p-values for all pairwise comparisons
    """
    if outcome not in df.columns or group not in df.columns:
        raise ValueError("Columns missing")
    
    if not pd.api.types.is_numeric_dtype(df[outcome]):
        raise ValueError("Outcome must be numeric")
    
    groups = df[group].unique()
    if len(groups) < 2:
        raise ValueError("Need at least two groups")
    
    # Perform pairwise t-tests
    from itertools import combinations
    from scipy.stats import ttest_ind
    
    comparisons = []
    p_values = []
    
    for grp1, grp2 in combinations(groups, 2):
        data1 = df[df[group] == grp1][outcome].dropna()
        data2 = df[df[group] == grp2][outcome].dropna()
        
        t_stat, p_val = ttest_ind(data1, data2)
        comparisons.append(f"{grp1} vs {grp2}")
        p_values.append(p_val)
    
    # Adjust p-values for multiple comparisons
    if p_adjust_method.lower() != 'none':
        # Map R method names to scipy method names
        method_map = {
            'bonferroni': 'bonferroni',
            'holm': 'holm',
            'fdr': 'fdr_bh',
            'bh': 'fdr_bh',
            'by': 'fdr_by'
        }
        scipy_method = method_map.get(p_adjust_method.lower(), 'bonferroni')
        _, p_adjusted, _, _ = multipletests(p_values, method=scipy_method)
    else:
        p_adjusted = p_values
    
    # Create results DataFrame
    results_df = pd.DataFrame({
        'Comparison': comparisons,
        'P-value': p_values,
        'Adjusted P-value': p_adjusted
    })
    
    print(f"Pairwise t-tests (p-value adjustment: {p_adjust_method}):")
    print(results_df.to_string(index=False))
    
    return results_df


# ---- Example usage in Python/Jupyter -------------------------------------
if __name__ == "__main__":
    # Example usage:
    # import pandas as pd
    # df = pd.read_csv("mock_normality.csv")
    # summary_stats(df)
    # frequency_table(df, "group")
    # correlation_matrix(df)
    # proportion_test(df, outcome_col="outcome", group_col="group")
    # normality_check(df, "value", out_prefix="demo")
    # outlier_detection(df, "value", method="iqr")
    # two_sample_ttest(df, outcome="value", group="group")
    # chi_square_test(df, "cat1", "cat2")
    # simple_linear_regression(df, response="y", predictor="x1")
    # simple_logistic_regression(df, outcome="binary", predictor="x1")
    # pairwise_comparisons(df, outcome="value", group="group_factor")
    pass

