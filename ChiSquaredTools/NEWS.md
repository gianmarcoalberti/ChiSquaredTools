# ChiSquaredTools News

## Version 1.2.1

### Bug Fixes
- Association: Goodman-Kruskal tau and Kvålseth's corrected lambda had their 'rows dependent' and 'cols dependent' values swapped; they now follow the same convention as the plain lambda (the named variable is the one being predicted).
- Association: Kvålseth's corrected lambda (symmetric) now uses his definition, the larger of the two directional values (Kvålseth 2018b, eq. 32), as the help text and its interval already did; previously the estimate used a weighted mean and could fall outside its own interval.
- Association: the confidence-interval note no longer loses its last words ('[0, 1]'), which jamovi's note renderer dropped because they were in square brackets.
- Association: the p-values of Yule's Q and Yule's Y (2×2 and pairwise) now come from the log-odds-ratio test, so they agree with the Odds Ratio and with their own confidence intervals (previously a Wald test on Q or Y could give p < .05 with an interval that included 0).
- Association: the bootstrap confidence intervals for the corrected measures (φ, C and V corrected, W-hat and Sakoda's D, plain and corrected) no longer stop with an error when a resampled table loses a rare row or column; such replicates are left out of the percentile interval.
- Association: φ, signed φ, C, Cramér's V and Cohen's w now also report Value (λ̂), the point estimate on the same noncentrality scale as their confidence interval, λ̂ = max(0, χ² − df), which always lies within the interval. The conventional value uses χ² itself and, with many degrees of freedom, can lie above the upper limit. The CI footnote and Method Details explain the difference; the earlier statement that the interval always contains the conventional estimate was incorrect.
- Association: bias-corrected Cramér's V now uses Bergsma's (2013) corrected numbers of rows and columns, Ṽ = √(φ̃² / min(r̃ − 1, c̃ − 1)); the previous denominator gave no correction for 2×J tables and slightly too small values otherwise.
- Test of Independence: in the Monte Carlo test, simulated tables with an empty row or column are redrawn instead of being counted as χ² = 0, which made p-values too small for sparse tables; a note reports how many were redrawn. The help text now states that the Monte Carlo test holds only N fixed, whereas the permutation test holds both margins fixed.
- Correspondence Analysis, Attribute Assessment: backward elimination now compares each step with the full-data row configuration, as in Krzanowski (1993), so the M² trajectory is the cumulative distortion described in the help text; the elbow is suggested only when the trajectory stays low and then rises sharply (otherwise all attributes are kept), instead of always dropping at least one attribute.
- Correspondence Analysis, Attribute Assessment: tables with fewer than 3 rows or 3 columns get a notice instead of meaningless rankings; plots that cannot be drawn are hidden instead of spinning; the influence plot no longer re-runs the elimination while rendering.
- Correspondence Analysis: the reduced-data plot updates when 'Equal axis scales' changes, its axis percentages match the main CA plots, and row and column labels no longer overlap each other.
- Power analysis: an a priori analysis with target power not above α now gives a plain message instead of a sample size of 4; the effect-size sensitivity curve marks the computed N (a priori) or the achieved power (post hoc) instead of the values in the disabled boxes, and it updates when the effect size changes.
- 2×2×K: when the Mantel-Haenszel common odds ratio is not finite (e.g. the same cell is 0 in every stratum) it is reported as unavailable with a footnote, left out of the forest plot, and the interpretation no longer calls it a valid summary. The help text now states correctly that the Haldane-Anscombe correction applies to the stratum-specific and marginal odds ratios and to Woolf's test, not to the common odds ratio. The trajectory plot's odds-ratio axis now follows the data range.
- R×C×K: bootstrap replicates in which a resampled table has an empty row or column are left out of the V corrected intervals (instead of counting as V = 0, which pulled the lower limits towards 0), and a note reports how many; a log-linear model that cannot be fitted now leaves the cells blank.
- Association: Goodman-Kruskal lambda (plain and corrected) and tau intervals are truncated to [0, 1]; when a 2×2 table has a zero cell, a note says that the Odds Ratio, Yule's Q and Yule's Y include the Haldane-Anscombe correction.
- Test of Independence: with non-integer counts, the permutation and Monte Carlo tests use the counts rounded to integers (previously the permutation test truncated them), and a note says so.
- Post-Hoc: the backwards-stepping table no longer shows empty rows when the number of steps requested exceeds the number of cells minus one.
- SRD: when every row (or column) is below the critical weight, or pruning would leave fewer than two, the categories are listed as kept rather than as removed.
- Clustering: when the Greenacre method cannot be applied (e.g. a 2×J table), the dendrogram is hidden instead of left blank, next to the explanatory note.

### New Features
- R×C×K: pooled chi-squared test (sum of the partial-table tests), reported in the stratum table. As in 2×2×K, the generalised CMH test decides conditional dependence when there is no evidence of heterogeneity and the pooled test decides when there is; cells whose adjusted residuals are significant in opposite directions in different strata are named as a pattern reversal (the R×C counterpart of odds ratios reversing direction). The decision tree now starts from the homogeneity test.
- Test of Independence: a new Cells section adds row, column and total percentages to the observed contingency table, shown as sub-rows under the counts as in jamovi's own Contingency Tables.

### Changes
- Attribute influence table and bar chart use the same importance classes; infinite M² values are explained in a footnote; the elbow is marked with a footnote.
- Reduced-data plot choices renamed 'Full data, low-influence faded' and 'Reduced data only'.
- Options that only affect a particular output are now enabled only when that output is selected (CA: residual type, symmetric, geometric and contribution plot settings; Post-Hoc: Šidák correction).
- Attribute Assessment: with fewer than three elimination steps the table says that no elbow can be judged; the trajectory axis starts at 0.
- The permutation and Monte Carlo distribution plots (Test of Independence), the PEM and DEP plots (Post-Hoc) and the forest plots (2×2×K and R×C×K) are now drawn with ggplot2 and follow the jamovi plot theme, like the other plots (the CA plots keep their own styling).
- PEM and DEP plots: significant positive values in gold, significant negative values in maroon, non-significant values in grey; the category colours are no longer replaced by the jamovi theme palette.
- Stratified Analysis (2×2×K and R×C×K): options are grouped in collapsible sections, as in the other analyses.
- 2×2×K and R×C×K: the interpretation summaries, test notes and method explanations now match what the tests can support: a non-significant homogeneity test is reported as 'no evidence that the association differs across strata' rather than as proof of homogeneity; when the stratum odds ratios reverse direction, the CMH result and the common odds ratio are flagged as not interpretable; in 2×2×K the CMH test decides conditional dependence when there is no evidence of heterogeneity and the pooled chi-squared test decides when there is; the former 'Spuriousness (CMH Discordant)' scenario, in which consistent strata are pooled by the CMH test, is now 'Consistent Weak Association'.
- 2×2×K: confounding is judged by the change-in-estimate criterion (marginal odds ratio against the common odds ratio, 10% threshold), giving 'Replication' or 'Partial Confounding'; R×C×K reports the marginal V corrected next to the weighted average for the same comparison.
- Association: when the χ² test is significant but the noncentral intervals for φ, C, V and w reach 0 (p between α/2 and α), the CI note explains why and that a 1 − 2α interval would exclude 0.
- Association and 2×2×K: the odds-ratio interpretation notes now share one wording, with columns as the groups (independent variable in the columns) and rows as the outcome and the change given as a percentage both above and below 1 (e.g. "for OPINION = 'Favour', the odds of GENDER = 'Female' rather than 'Male' are 3.91 times those for 'Oppose' (291% higher)"); the method explanations say so.
- 2×2×K forest plot: the legend lists only the significance categories present in the plot.
- Option labels in sentence case and without abbreviations (e.g. "Traditional chi-squared test", "Minimum quality (cos²) for correlations", "Row dendrogram"); redundant qualifiers removed from the PEM and DEP settings; the CA scree plot moved to Diagnostics.
- Correspondence Analysis: Malinvaud's test now uses the min(I, J) − 1 dimensions of the solution, as the help text states (for Freeman-Tukey and adjusted residuals it previously also included one non-structural dimension).
- SRD: method explanations are off by default, as in the other analyses.
- Help texts updated for all the changes above (confidence intervals, bootstrap replicates, Haldane correction, truncated Lambda and Tau intervals, Monte Carlo and permutation tests, Attribute Assessment, SRD pruning, backwards-stepping steps) and corrected where they disagreed with the output (Post-Hoc highlighting, W-hat interval percentiles).
- Internal: dead code removed (unused trivial-axis branches in CA, never-used caches in the stratified analyses); grid declared in Imports.
- Krzanowski (1993) reference formatting corrected.
- Added a tests/ folder with testthat cases (rare categories, 2×J tables, CI containment with many degrees of freedom, bias-corrected V, sparse Monte Carlo, non-finite common odds ratio, R×C×K bootstrap with a rare row, power-analysis limits).

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
