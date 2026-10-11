# ChiSquaredTools News

## Version 1.2.0

### New Features
- Added Attribute Assessment facility to Correspondence Analysis, implementing Krzanowski's (1993) Procrustes-based method for identifying which column variables (attributes) contribute most to the row configuration.
- Attribute Influence Ranking table shows single-pass M² values measuring the distortion caused by removing each attribute.
- Backward Elimination Sequence table performs iterative removal of least-influential attributes with automatic elbow detection to suggest an optimal subset.
- Influence Bar Chart visualises attribute importance with colour-coding for high/low influence categories.
- Elimination Trajectory Plot displays M² values across elimination steps with elbow annotation.
- Reduced-Data CA Plot shows the correspondence analysis with low-influence attributes either faded or hidden, allowing visual comparison with the full-data symmetric plot.
- Dimensionality for Procrustes comparison automatically set to capture ≥80% of inertia (minimum 3 dimensions), following Krzanowski (1993).

### Bug Fixes
- Correspondence Analysis no longer fails on tables with 2 rows or 2 columns (or whenever more dimensions are requested than the table has); the number of dimensions is capped at min(rows, columns) − 1 and a notice explains the cap.
- Correspondence Analysis no longer discards a genuine first dimension with inertia close to 1 (e.g. a perfectly diagonal table); eigenvalues, total inertia and Malinvaud's test are corrected in such cases.
- Plots now render when exported (CA, Test of Independence permutation and Monte Carlo distributions, Clustering dendrograms): each plot draws from its saved state.
- Stratified analyses no longer return NaN for the CMH test, common odds ratio CI and homogeneity tests with large samples (integer overflow inside the CMH computation).
- Permutation and Monte Carlo results (Test of Independence) and all Post-Hoc tables and plots are refreshed when the counts variable changes.
- Pairwise Yule's Q and Y p-values now use the correct standard errors; in 2×2 tables Q and Y follow the selected reference categories, consistently with the odds ratio.
- Confidence intervals for φ, signed φ, C, Cramér's V and Cohen's w: the search for the limits of the χ² noncentrality parameter used fixed brackets and failed for small χ² values (for weak associations the upper limit was capped at 3χ² and the lower limit could not reach 0); it now uses a bracketing root-finder (verified against Smithson 2003, Example 6.3).
- The noncentrality limits are now converted to the effect-size scale through λ = N·w² (Cohen 1988), the same transformation used for the point estimate, instead of Smithson's λ + df. The interval therefore always contains the estimate; results for clear associations change only marginally. The CI note and Method Details explain the method.
- Confidence intervals for Yule's Q and Y in 2×2 tables are now obtained by transforming the confidence interval of the log odds ratio (as already done for pairwise comparisons and as stated in the table note), so they always lie within −1 and +1.
- Monte Carlo test (Test of Independence) draws each simulated table in a single multinomial step, giving the same reference distribution much faster with large samples.
- R×C×K analysis: bootstrap confidence intervals are reproducible (new random seed option) and much faster with large samples; a log-linear model that cannot be fitted is reported as unavailable instead of p = 1.
- Partition selection (Husson criterion) returns the correct step for tables with very few categories.
- Greenacre critical values (Clustering and CA) re-transcribed from Pearson & Hartley (1972, Table 51): several entries for tables with more than 13 categories on the larger side had been keyed to the wrong row or column (up to 15% too high, leading to too few clusters). The table is now shared by both facilities, extends to 201 categories on the larger side, and agrees within 0.7% with a Monte Carlo check of the largest-root distribution.
- Power analysis gives a plain message instead of an R error when the target power cannot be reached or is not greater than α.
- Effect-size labels for uncorrected φ, C, V, W-hat and Sakoda's D now always account for the maximum attainable value, whether or not the threshold table is shown.
- Variable names with spaces or special characters are handled by all analyses, and they can no longer be interpreted as R code.
- A row or column level with no observations (for example after filtering) now stops the analysis with a clear message.

### Changes
- Expected-frequencies table (Test of Independence) now shows row and column totals.
- Effect size threshold table (Association) is shown by default, and all threshold values are shown with three decimals.
- Chi-squared-maximising table shows integer counts.
- The homogeneity test based on inverse-variance weighted log odds ratios (2×2×K) is now correctly labelled Woolf's test; CMH footnotes reworded.
- Odds-ratio interpretation sentences name the variables explicitly.
- Options panels of Association, Post-Hoc and Test of Independence are organised in collapsible sections; PEM and resampling settings are enabled only when relevant.
- Checkbox titles name the output (e.g. "Scree plot" instead of "Show scree plot").
- CA plots and dendrograms follow the jamovi plot theme; CA plots are framed on all four sides in the theme's axis style (the R×C×K decision tree keeps its own flowchart style).
- Help-text corrections (Šidák formula, adjusted residuals, Malinvaud's test, confidence levels, Monte Carlo p-values), accents in references, option descriptions and R usage examples.
- References cited in the Association, Test of Independence and Power help texts and notes now also appear in the results reference list.
- CA attribute-assessment plots are shown only when the corresponding table (which computes them) is selected, matching the options panel; clearer option descriptions and corrected R usage examples (Association, SRD).
- Internal: shared table-building helper, removal of unused code and of an unused result cache, base packages and scales declared in Imports, unused entries removed from the reference list.

### Updates
- Method Details section now includes explanation of Procrustes-based attribute assessment, backward elimination algorithm, elbow detection, and reduced-data plot options.
- Overdispersion Diagnostic Plot repositioned to appear after all Attribute Assessment outputs.
- 00refs.yaml extended with Krzanowski (1993) reference.

---

## Version 1.1.0

### Bug Fixes
- Fix to the highlighting of Quetelet Index and IJ Factor: values that are beyond the empirical thresholds are now highlighted.
- Fix to column clustering table note displaying wrong method information.
- In SRD, changed column reduction to operate on the pruned matrix (not the row-reduced matrix) so the Column-Grouped Table displays original rows with merged columns.
- In SRD, changed column type from number to integer for count-based columns to remove unnecessary decimal places.

### New Features
- Added Test Validity Assessment table to the Test of Independence facility, providing diagnostic evaluation of chi-squared test validity based on sample size (N ≥ 10), average cell count (N/k ≥ 5), sparsity index (N²/k ≥ 10), and minimum expected frequency (min(E) ≥ 1).
- Decision rule for test selection now follows a structured path: Traditional χ² when N/k ≥ 5; (N-1)/N adjusted χ² when N/k < 5 but N²/k ≥ 10 and min(E) ≥ 1; Permutation or Monte Carlo methods otherwise.
- Added Koehler & Larntz (1980) sparsity criterion to validity diagnostics.
- Integrated Correspondence Analysis facility into ChiSquaredTools, providing multiple visualisation options (Symmetric CA Plot, Geometric Interpretation Plot, Contribution Biplot) with support for both Pearson residuals (χ² distance) and Freeman-Tukey residuals (Hellinger distance). Features include Malinvaud's test for dimensionality, SRD-based clustering of interpreted categories, overdispersion diagnostics, and optional adjusted standardised residuals.

### Updates
- Method Details section now includes explanation of validity assessment criteria and decision rules.
- Gambirasio reference corrected from placeholder to full OSF preprint citation.
- NAMESPACE now includes ggrepel import for CA plot labels.
- 00refs.yaml extended with CA-related references (Beh 2012, Beh et al. 2018, Borg & Groenen 2005, Greenacre 2017, Saporta 2006, Yelland 2010).

### Removed
- Redundant recommendation footnote from Test Results table (guidance now provided by Test Validity Assessment table).

## Version 1.0.0

- Initial release.