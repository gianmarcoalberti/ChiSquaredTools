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

### From GitHub

**Pre-built module (macOS only):**

1. Download the `.jmo` file from the [Releases](../../releases) page
2. In jamovi, go to **Modules** → **jamovi library** → **Sideload** (⋮ menu)
3. Select the downloaded `.jmo` file

> ⚠️ **Note:** The pre-built `.jmo` file was compiled on macOS and will only work on macOS systems. Windows and Linux users will need to build the module from source using `jmvtools::install()` in R, or wait for the module to become available in the jamovi library.

### From jamovi Module Library

*(Coming soon — pending submission to the jamovi module store)*

## Requirements

- jamovi 2.6 or later
- R 4.4.1 or later (bundled with jamovi)

## Author

**Gianmarco Alberti**  
University of Malta  
Email: gianmarcoalberti@gmail.com

## Citation

If you use this module in your research or teaching, please cite:

> Alberti, G. (2025). *ChiSquaredTools: Chi-Squared Analysis Tools* [jamovi module]. Version 1.0.0. https://github.com/gianmarcoalberti/ChiSquaredTools

## Related Resources

- **chisquare**: The author's CRAN package providing the underlying statistical functions
- **Cross-Tabulation Analysis: A Beginner's Guide** (Alberti, forthcoming): Companion textbook

## Licence

This project is licensed under the GNU General Public License v3.0 — see the [LICENSE](LICENSE) file for details.

## Feedback and Bug Reports

Please report issues or suggestions via the [GitHub Issues](../../issues) page or the [jamovi forum](https://forum.jamovi.org/).

## Acknowledgements

This module was developed with support from the jamovi developer community. Thanks to all beta testers who provided feedback during development.
