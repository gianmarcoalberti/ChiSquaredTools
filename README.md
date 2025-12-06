# ChiSquaredTools

A comprehensive jamovi module for chi-squared analysis of contingency tables, designed as an educational resource for undergraduate students learning categorical data analysis.

## Overview

ChiSquaredTools provides a point-and-click interface for contingency table analysis, addressing the challenge students face when learning both statistical concepts and software simultaneously. The module includes extensive pedagogical scaffolding with toggleable method explanations, academic citations, and interpretation guidance.

## Features

The module includes six analytical facilities accessible from the **Chi² Tools** menu in jamovi:

### 1. Test of Independence
Chi-squared test of independence with multiple testing methods:
- Traditional Pearson chi-squared test
- (N−1)/N adjusted chi-squared test (for small samples)
- Permutation-based exact test
- Monte Carlo test with Phipson & Smyth (2010) p-value computation
- M test (maximum adjusted residual method)

### 2. Association & Effect Sizes
Over 20 association measures organised by statistical foundation:
- Chi-squared-based measures (Cramér's V, Contingency Coefficient, etc.)
- Margin-free measures (Yule's Q, Odds Ratio, etc.)
- Proportional Reduction in Error (PRE) measures (Lambda, Tau, Uncertainty Coefficient, etc.)
- Bootstrap confidence intervals for selected measures

### 3. Post-Hoc Analysis
Cell-level diagnostic metrics including:
- Standardised and adjusted standardised residuals
- Moment-corrected standardised residuals
- Quetelet Index and IJ association factor
- Backwards-stepping outlier detection
- PEM (Percentage of Maximum Deviation) with bootstrap confidence intervals
- Median polish residuals (standardised and adjusted)
- Goodman-Kruskal residuals
- Difference in Estimated Proportions (DEP)

### 4. Row/Column Clustering
Hierarchical clustering analysis using:
- Ward's method with chi-squared distance (Greenacre, 2017)
- Significance testing via permutation
- Dendrogram visualisation

### 5. Stratified Analysis (2×2×K)
For 2×2 tables across K strata:
- Cochran-Mantel-Haenszel test for conditional independence
- Mantel-Haenszel pooled odds ratio with confidence intervals
- Breslow-Day and Tarone tests for homogeneity of odds ratios
- Stratum-specific statistics and forest plots

### 6. Stratified Analysis (R×C×K)
For general R×C tables across K strata:
- Generalised Cochran-Mantel-Haenszel test
- Log-linear homogeneity test
- Stratum-specific chi-squared statistics

## Educational Features

Each facility includes:
- **Method explanations**: Toggleable descriptions with rationale, formulas, and usage guidance
- **Automatic highlighting**: Significant or noteworthy results are colour-coded for easy identification
- **Academic citations**: Proper references to statistical literature throughout
- **Interpretation guidance**: Decision support for choosing appropriate methods

## Installation

### From jamovi Module Library

*(Coming soon — pending submission to the jamovi module store)*

Once available, this will be the recommended installation method, as the jamovi library provides pre-built versions for all supported platforms (Windows, macOS Intel, macOS Apple Silicon, and Linux).

### From GitHub (for testing)

**Option 1: Pre-built module**

1. Download the `.jmo` file from the [Releases](../../releases) page
2. In jamovi, go to **Modules** → **jamovi library** → **Sideload** (⋮ menu)
3. Select the downloaded `.jmo` file

> ⚠️ **Important:** The pre-built `.jmo` file was compiled on macOS (Apple Silicon) with jamovi 2.6.x. It will work on Macs running jamovi 2.6.x (Apple Silicon version). It may **not** work on:
> - Windows or Linux systems
> - Macs running the Intel version of jamovi
> - Systems running jamovi 2.7.x or later
>
> Users in these situations should use Option 2 below.

**Option 2: Build from source (all platforms)**

If the pre-built `.jmo` file is not compatible with your system, you can build the module from source:

1. Ensure you have R and the `jmvtools` package installed:
   ```r
   install.packages('jmvtools', repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))
   ```
2. Clone or download this repository
3. Open R in the repository directory and run:
   ```r
   jmvtools::install()
   ```

This will compile the module for your specific platform and install it into your local jamovi installation.

## Requirements

- jamovi 2.6 or later
- R 4.4.1 or later (bundled with jamovi; required only if building from source)

## Author

**Gianmarco Alberti**  
University of Malta  
Email: gianmarco.alberti@um.edu.mt

## Citation

If you use this module in your research or teaching, please cite:

> Alberti, G. (2025). *ChiSquaredTools: Chi-Squared Analysis Tools* [jamovi module]. Version 1.0.0. https://github.com/gianmarcoalberti/ChiSquaredTools

## Related Resources

- **chisquare**: The author's CRAN package providing many of the underlying statistical functions
- **stratastats**: The author's CRAN package providing many of the underlying statistical functions
- **From Data to Insights: A Beginner's Guide to Cross-Tabulation Analysis** (Chapman & Hall, 2024. ISBN 9781032720388)

## Licence

This project is licensed under the GNU General Public License v3.0 — see the [LICENSE](LICENSE) file for details.
