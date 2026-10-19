# This file is a generated template, your changes will not be overwritten

#' @export
chisqtestClass <- R6::R6Class(
  "chisqtestClass",
  inherit = chisqtestBase,
  private = list(
    
    # Store computed values for plotting
    .permStats = NULL,
    .mcStats = NULL,
    .observedChiSq = NULL,
    
    # Table structure cache
    .rowLevels = NULL,
    .pcTypes = NULL,
    .colLevels = NULL,
    .I = NULL,
    .J = NULL,
    
    .init = function() {
      
      # -----------------------------------------------------------------
      # 1. Early return if no variables - nothing to pre-build
      # -----------------------------------------------------------------
      if (is.null(self$options$rows) || is.null(self$options$cols))
        return()
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      data <- self$data
      
      # Ensure variables are factors
      if (!is.factor(data[[rowVar]]))
        data[[rowVar]] <- as.factor(data[[rowVar]])
      if (!is.factor(data[[colVar]]))
        data[[colVar]] <- as.factor(data[[colVar]])
      
      # Get levels for table structure
      row_levels <- levels(data[[rowVar]])
      col_levels <- levels(data[[colVar]])
      I <- length(row_levels)
      J <- length(col_levels)
      
      # Store for use in .run()
      private$.rowLevels <- row_levels
      private$.colLevels <- col_levels
      private$.I <- I
      private$.J <- J
      
      # -----------------------------------------------------------------
      # 2. Pre-build crosstabTable with full structure
      # -----------------------------------------------------------------
      crosstab <- self$results$crosstabTable
      anyPct <- self$options$pctRow || self$options$pctCol || self$options$pctTotal
      crosstab$setTitle(paste0(if (anyPct) "Observed Frequencies and Percentages: " else
                                 "Observed Frequencies: ", rowVar, " × ", colVar))
      
      # Columns named 'x[count]', 'x[pcRow]', ... are folded by jamovi into
      # sub-rows of the same row (as in jamovi's own Contingency Tables);
      # the percentage sub-rows appear only when their option is ticked
      pcTypes <- list(
        list(key = 'count',   label = 'Observed',          visible = '(pctRow || pctCol || pctTotal)'),
        list(key = 'pcRow',   label = '% within row',      visible = '(pctRow)'),
        list(key = 'pcCol',   label = '% within column',   visible = '(pctCol)'),
        list(key = 'pcTotal', label = '% of total',        visible = '(pctTotal)'))
      private$.pcTypes <- pcTypes
      
      crosstab$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (pt in pcTypes)
        crosstab$addColumn(name = paste0('type[', pt$key, ']'), title = '',
                           type = 'text', visible = pt$visible)
      for (j in c(seq_len(J), 0)) {
        base <- if (j == 0) 'rowtotal' else paste0('col', j)
        for (pt in pcTypes) {
          if (pt$key == 'count') {
            crosstab$addColumn(name = paste0(base, '[count]'),
                               title = if (j == 0) 'Total' else col_levels[j],
                               type = 'integer',
                               superTitle = if (j == 0) NULL else colVar)
          } else {
            crosstab$addColumn(name = paste0(base, '[', pt$key, ']'),
                               title = if (j == 0) 'Total' else col_levels[j],
                               type = 'number', format = 'pc',
                               superTitle = if (j == 0) NULL else colVar,
                               visible = pt$visible)
          }
        }
      }
      
      # Pre-add rows with placeholders
      for (i in seq_len(I)) {
        crosstab$addRow(rowKey = i, values = list(rowname = row_levels[i]))
      }
      crosstab$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      
      # -----------------------------------------------------------------
      # 3. Pre-build expectedTable with full structure
      # -----------------------------------------------------------------
      expected <- self$results$expectedTable
      expected$setTitle(paste0("Expected Frequencies: ", rowVar, " × ", colVar))
      
      expected$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (j in seq_len(J)) {
        expected$addColumn(
          name = paste0('col', j),
          title = col_levels[j],
          type = 'number',
          superTitle = colVar
        )
      }
      # Margins (equal to the observed margins by construction)
      expected$addColumn(name = 'rowtotal', title = 'Total', type = 'number')
      
      # Pre-add rows with placeholders
      for (i in seq_len(I)) {
        expected$addRow(rowKey = i, values = list(rowname = row_levels[i]))
      }
      expected$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      
      # -----------------------------------------------------------------
      # 4. Pre-build tableCharacteristics (already uses setRow pattern)
      # -----------------------------------------------------------------
      charTable <- self$results$tableCharacteristics
      charTable$addRow(rowKey = "size", values = list(characteristic = "Table size", value = "."))
      charTable$addRow(rowKey = "total", values = list(characteristic = "Grand total (N)", value = "."))
      charTable$addRow(rowKey = "cells", values = list(characteristic = "Number of cells", value = "."))
      charTable$addRow(rowKey = "minexp", values = list(characteristic = "Minimum expected frequency", value = "."))
      charTable$addRow(rowKey = "avgexp", values = list(characteristic = "Average expected frequency", value = "."))
      
      # -----------------------------------------------------------------
      # 5. Pre-build validityAssessment table
      # -----------------------------------------------------------------
      validityTable <- self$results$validityAssessment
      validityTable$addRow(rowKey = "n", values = list(
        diagnostic = "Sample size (N)", value = ".", threshold = ".", status = "."
      ))
      validityTable$addRow(rowKey = "lambda", values = list(
        diagnostic = "Average cell count (N/k)", value = ".", threshold = ".", status = "."
      ))
      validityTable$addRow(rowKey = "sparsity", values = list(
        diagnostic = "Sparsity index (N\u00B2/k)", value = ".", threshold = ".", status = "."
      ))
      validityTable$addRow(rowKey = "minexp", values = list(
        diagnostic = "Minimum expected frequency", value = ".", threshold = ".", status = "."
      ))
      validityTable$addRow(rowKey = "assessment", values = list(
        diagnostic = "Overall assessment", value = "", threshold = "", status = "."
      ))
      
      # -----------------------------------------------------------------
      # 6. Pre-build testResults rows based on selected options
      # -----------------------------------------------------------------
      testTable <- self$results$testResults
      
      if (self$options$traditionalTest)
        testTable$addRow(rowKey = 'traditional', values = list(method = 'Traditional χ²'))
      if (self$options$n1Test)
        testTable$addRow(rowKey = 'n1adjusted', values = list(method = '(N-1)/N adjusted χ²'))
      if (self$options$permTest)
        testTable$addRow(rowKey = 'permutation', values = list(
          method = paste0('Permutation (', self$options$nPerms, ' permutations)')
        ))
      if (self$options$monteCarloTest)
        testTable$addRow(rowKey = 'montecarlo', values = list(
          method = paste0('Monte Carlo (', self$options$nPerms, ' simulations)')
        ))
      if (self$options$mTest)
        testTable$addRow(rowKey = 'mtest', values = list(method = 'M Test'))
    },
    
    .run = function() {
      
      # Check if variables are selected
      if (is.null(self$options$rows) || is.null(self$options$cols)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      data <- self$data
      
      # Ensure variables are factors
      if (!is.factor(data[[rowVar]])) {
        data[[rowVar]] <- as.factor(data[[rowVar]])
      }
      if (!is.factor(data[[colVar]])) {
        data[[colVar]] <- as.factor(data[[colVar]])
      }
      
      # Build contingency table safely (shared helper in utils.R):
      # no formula parsing of variable names, double storage, all levels kept
      contingency_table <- .ctBuildTable(data, c(rowVar, colVar), self$options$counts)
      .ctRejectTooFewLevels(contingency_table, rowVar, colVar)
      .ctRejectEmptyMargins(contingency_table, rowVar, colVar)
      
      # Calculate expected frequencies
      n <- sum(contingency_table)
      row_totals <- rowSums(contingency_table)
      col_totals <- colSums(contingency_table)
      expected <- outer(row_totals, col_totals) / n
      dimnames(expected) <- dimnames(contingency_table)
      
      # Populate crosstab and expected tables
      private$.populateCrosstab(contingency_table)
      private$.populateExpectedTable(expected)
      
      # Populate table characteristics (replaces methodGuidance HTML)
      private$.populateTableCharacteristics(contingency_table, expected)
      
      # Populate validity assessment table
      private$.populateValidityAssessment(contingency_table, expected)
      
      # Run tests and add dynamic recommendation as footnote
      private$.runTests(contingency_table, expected, data, rowVar, colVar)
      
      # Populate method info
      private$.populateMethodInfo()
      
      # Populate references
      #private$.populateReferences()
    },
    
    .populateCrosstab = function(contingency_table) {
      
      table <- self$results$crosstabTable
      
      I <- nrow(contingency_table)
      J <- ncol(contingency_table)
      
      n <- sum(contingency_table)
      row_tot <- rowSums(contingency_table)
      col_tot <- colSums(contingency_table)
      
      # Counts and the three kinds of percentages (as proportions; the
      # columns use jamovi's 'pc' format) for one row of the table
      cellValues <- function(counts, rowTotal) {
        v <- list()
        for (pt in private$.pcTypes) v[[paste0('type[', pt$key, ']')]] <- pt$label
        for (j in c(seq_len(J), 0)) {
          base <- if (j == 0) 'rowtotal' else paste0('col', j)
          x <- if (j == 0) sum(counts) else counts[j]
          colTotal <- if (j == 0) n else col_tot[j]
          v[[paste0(base, '[count]')]] <- x
          v[[paste0(base, '[pcRow]')]] <- if (rowTotal > 0) x / rowTotal else NaN
          v[[paste0(base, '[pcCol]')]] <- if (colTotal > 0) x / colTotal else NaN
          v[[paste0(base, '[pcTotal]')]] <- x / n
        }
        v
      }
      
      for (i in 1:I)
        table$setRow(rowKey = i, values = cellValues(contingency_table[i, ], row_tot[i]))
      table$setRow(rowKey = 'total', values = cellValues(col_tot, n))
    },
    
    .populateExpectedTable = function(expected) {
      
      table <- self$results$expectedTable
      
      I <- nrow(expected)
      J <- ncol(expected)
      
      # Use setRow() to update pre-built rows
      for (i in 1:I) {
        row_values <- list()
        for (j in 1:J) {
          row_values[[paste0("col", j)]] <- expected[i, j]
        }
        row_values[['rowtotal']] <- sum(expected[i, ])
        table$setRow(rowKey = i, values = row_values)
      }
      
      # Total row
      total_values <- list()
      for (j in 1:J) {
        total_values[[paste0("col", j)]] <- sum(expected[, j])
      }
      total_values[['rowtotal']] <- sum(expected)
      table$setRow(rowKey = 'total', values = total_values)
    },
    
    .populateTableCharacteristics = function(contingency_table, expected) {
      
      table <- self$results$tableCharacteristics
      
      I <- nrow(contingency_table)
      J <- ncol(contingency_table)
      grand_total <- sum(contingency_table)
      num_cells <- I * J
      min_expected <- min(expected)
      avg_expected <- grand_total / num_cells
      
      # Determine table size description
      table_size <- if (I == 2 && J == 2) "2×2" else paste0(I, "×", J)
      
      # Update rows (rows already created in .init())
      table$setRow(rowKey = "size", values = list(
        characteristic = "Table size",
        value = table_size
      ))
      table$setRow(rowKey = "total", values = list(
        characteristic = "Grand total (N)",
        value = as.character(grand_total)
      ))
      table$setRow(rowKey = "cells", values = list(
        characteristic = "Number of cells",
        value = as.character(num_cells)
      ))
      table$setRow(rowKey = "minexp", values = list(
        characteristic = "Minimum expected frequency",
        value = sprintf("%.3f", min_expected)
      ))
      table$setRow(rowKey = "avgexp", values = list(
        characteristic = "Average expected frequency",
        value = sprintf("%.3f", avg_expected)
      ))
    },
    
    .populateValidityAssessment = function(contingency_table, expected) {
      
      table <- self$results$validityAssessment
      
      # Calculate diagnostics
      n <- sum(contingency_table)
      I <- nrow(contingency_table)
      J <- ncol(contingency_table)
      k <- I * J
      lambda <- n / k
      sparsity_index <- (n^2) / k
      min_exp <- min(expected)
      
      # Evaluate each criterion
      n_ok <- n >= 10
      lambda_ok <- lambda >= 5
      sparsity_ok <- sparsity_index >= 10
      minexp_ok <- min_exp >= 1
      
      # Helper function for status text
      status_text <- function(ok, pass_label = "Adequate", fail_label = "Below threshold") {
        if (ok) paste0("\u2713 ", pass_label) else paste0("\u2717 ", fail_label)
      }
      
      # Populate rows
      table$setRow(rowKey = "n", values = list(
        diagnostic = "Sample size (N)",
        value = as.character(n),
        threshold = "\u2265 10",
        status = status_text(n_ok)
      ))
      
      table$setRow(rowKey = "lambda", values = list(
        diagnostic = "Average cell count (N/k)",
        value = sprintf("%.2f", lambda),
        threshold = "\u2265 5",
        status = status_text(lambda_ok)
      ))
      
      table$setRow(rowKey = "sparsity", values = list(
        diagnostic = "Sparsity index (N\u00B2/k)",
        value = sprintf("%.2f", sparsity_index),
        threshold = "\u2265 10",
        status = status_text(sparsity_ok, "Non-sparse", "Sparse")
      ))
      
      table$setRow(rowKey = "minexp", values = list(
        diagnostic = "Minimum expected frequency",
        value = sprintf("%.3f", min_exp),
        threshold = "\u2265 1",
        status = status_text(minexp_ok)
      ))
      
      # Determine overall assessment
      if (n < 10) {
        assessment <- "Permutation or Monte Carlo tests recommended (N < 10)"
      } else if (lambda_ok) {
        # N/k ≥ 5: traditional chi-squared appropriate
        assessment <- "Traditional \u03C7\u00B2 test appropriate"
      } else if (sparsity_ok && minexp_ok) {
        # N/k < 5 but N²/k ≥ 10 and min(E) ≥ 1
        assessment <- "(N-1)/N adjusted \u03C7\u00B2 recommended"
      } else {
        # Either N²/k < 10 or min(E) < 1
        assessment <- "Permutation or Monte Carlo tests recommended"
      }
      
      table$setRow(rowKey = "assessment", values = list(
        diagnostic = "Overall assessment",
        value = "",
        threshold = "",
        status = assessment
      ))
      
      # Add footnote with references
      table$setNote(
        key = "validity_refs",
        note = paste0(
          "Thresholds based on: N \u2265 10 and N\u00B2/k \u2265 10 (Koehler & Larntz 1980); ",
          "N/k \u2265 5 (Roscoe & Byars 1971; Zar 2014); ",
          "min(E) \u2265 1 for (N-1)/N adjustment (Campbell 2007; Richardson 2011). ",
          "See also: Alberti 2024."
        ),
        init = FALSE
      )
    },
    
    .runTests = function(observed, expected, data, rowVar, colVar) {
      
      table <- self$results$testResults
      permImage <- self$results$permDistPlot
      mcImage <- self$results$mcDistPlot
      
      n <- sum(observed)
      df <- (nrow(observed) - 1) * (ncol(observed) - 1)
      chisq_stat <- sum((observed - expected)^2 / expected)
      private$.observedChiSq <- chisq_stat
      
      # Always restore private variables from cache (needed for plots)
      if (!is.null(permImage$state) && !is.null(permImage$state$perm_stats)) {
        private$.permStats <- permImage$state$perm_stats
      }
      # (states saved before 1.2.1 lack n_redrawn and scored degenerate tables as 0,
      # so they are recomputed)
      if (!is.null(mcImage$state) && !is.null(mcImage$state$mc_stats) &&
          !is.null(mcImage$state$n_redrawn)) {
        private$.mcStats <- mcImage$state$mc_stats
      }
      
      # Traditional chi-squared test
      if (self$options$traditionalTest) {
        p_value <- pchisq(chisq_stat, df, lower.tail = FALSE)
        table$setRow(rowKey = 'traditional', values = list(
          method = 'Traditional χ²',
          statistic = chisq_stat,
          df = df,
          mstat = '',
          pvalue = p_value
        ))
      }
      
      # (N-1) adjusted chi-squared test
      if (self$options$n1Test) {
        chisq_adj <- chisq_stat * (n - 1) / n
        p_adj <- pchisq(chisq_adj, df, lower.tail = FALSE)
        table$setRow(rowKey = 'n1adjusted', values = list(
          method = '(N-1)/N adjusted χ²',
          statistic = chisq_adj,
          df = df,
          mstat = '',
          pvalue = p_adj
        ))
      }
      
      # The permutation and Monte Carlo tests resample individual cases, so
      # they need whole-number counts: non-integer counts are rounded for
      # these two tests only (a note reports it)
      obs_rs <- round(observed)
      nonInteger <- any(abs(observed - obs_rs) > 1e-8)
      if (nonInteger) {
        n_rs <- sum(obs_rs)
        e_rs <- outer(rowSums(obs_rs), colSums(obs_rs)) / n_rs
        stat_rs <- sum((obs_rs - e_rs)^2 / e_rs)
      } else {
        n_rs <- n
        stat_rs <- chisq_stat
      }
      if (nonInteger && (self$options$permTest || self$options$monteCarloTest)) {
        table$setNote(key = "rounded", note = "The counts are not whole numbers: they were rounded to integers for the permutation and Monte Carlo tests.", init = FALSE)
      } else {
        table$setNote(key = "rounded", note = NULL, init = FALSE)
      }
      
      # Permutation test
      if (self$options$permTest) {
        
        # Check cache first
        if (!is.null(private$.permStats)) {
          # Already restored from cache above
          perm_stats <- private$.permStats
          B_extreme <- sum(perm_stats >= stat_rs)
          p_perm <- (B_extreme + 1) / (length(perm_stats) + 1)
          # States saved by older versions lack the observed statistic
          if (is.null(permImage$state$observed))
            permImage$setState(list(perm_stats = perm_stats, p_perm = p_perm,
                                    observed = stat_rs))
        } else {
          # No cache - run expensive computation
          set.seed(self$options$seed)
          
          row_indices <- rep(1:nrow(obs_rs), times = rowSums(obs_rs))
          col_indices <- unlist(lapply(1:nrow(obs_rs), function(i) {
            rep(1:ncol(obs_rs), times = obs_rs[i, ])
          }))
          
          B <- self$options$nPerms
          perm_stats <- numeric(B)
          
          for (b in 1:B) {
            perm_col_indices <- sample(col_indices)
            perm_table <- table(row_indices, perm_col_indices)
            perm_row_totals <- rowSums(perm_table)
            perm_col_totals <- colSums(perm_table)
            perm_expected <- outer(perm_row_totals, perm_col_totals) / n_rs
            perm_stats[b] <- sum((perm_table - perm_expected)^2 / perm_expected)
          }
          
          B_extreme <- sum(perm_stats >= stat_rs)
          p_perm <- (B_extreme + 1) / (B + 1)
          
          private$.permStats <- perm_stats
          permImage$setState(list(perm_stats = perm_stats, p_perm = p_perm,
                                  observed = stat_rs))
        }
        
        table$setRow(rowKey = 'permutation', values = list(
          method = paste0('Permutation (', self$options$nPerms, ' permutations)'),
          statistic = stat_rs,
          df = df,
          mstat = '',
          pvalue = p_perm
        ))
      }
      
      # Monte Carlo test
      if (self$options$monteCarloTest) {
        
        # Check cache first
        if (!is.null(private$.mcStats)) {
          # Already restored from cache above
          mc_stats <- private$.mcStats
          B_extreme <- sum(mc_stats >= stat_rs)
          p_mc <- (B_extreme + 1) / (length(mc_stats) + 1)

        } else {
          # No cache - run expensive computation
          set.seed(self$options$seed)
          
          B <- self$options$nPerms
          mc_stats <- numeric(B)
          
          row_probs <- rowSums(obs_rs) / n_rs
          col_probs <- colSums(obs_rs) / n_rs
          
          # Under independence each case falls in cell (i, j) with probability
          # row_prob[i] * col_prob[j]; drawing n cases one at a time is
          # equivalent to one multinomial draw, which is far faster for large n
          cell_probs <- as.vector(outer(row_probs, col_probs))
          n_mc <- n_rs
          n_redrawn <- 0
          
          for (b in 1:B) {
            # A simulated table with an empty row or column has no chi-squared
            # statistic on the observed table's dimensions; such tables are
            # redrawn, so the reference distribution is conditional on every
            # row and column being non-empty (as in the observed table)
            for (attempt in 1:1000) {
              mc_table <- matrix(stats::rmultinom(1, n_mc, cell_probs),
                                 nrow = nrow(obs_rs), ncol = ncol(obs_rs))
              mc_row_totals <- rowSums(mc_table)
              mc_col_totals <- colSums(mc_table)
              if (all(mc_row_totals > 0) && all(mc_col_totals > 0)) break
              n_redrawn <- n_redrawn + 1
            }
            if (any(mc_row_totals == 0) || any(mc_col_totals == 0)) {
              mc_stats[b] <- NA_real_
              next
            }
            mc_expected <- outer(mc_row_totals, mc_col_totals) / n_mc
            mc_stats[b] <- sum((mc_table - mc_expected)^2 / mc_expected)
          }
          mc_stats <- mc_stats[!is.na(mc_stats)]
          B <- length(mc_stats)
          
          B_extreme <- sum(mc_stats >= stat_rs)
          p_mc <- (B_extreme + 1) / (B + 1)
          
          private$.mcStats <- mc_stats
          mcImage$setState(list(mc_stats = mc_stats, p_mc = p_mc,
                                observed = stat_rs, n_redrawn = n_redrawn))
        }
        
        n_redrawn <- mcImage$state$n_redrawn
        if (isTRUE(n_redrawn > 0)) {
          table$setNote(key = "mc_redraw", note = sprintf(
            "Monte Carlo: %d simulated table(s) with an empty row or column were redrawn.", n_redrawn), init = FALSE)
        } else {
          table$setNote(key = "mc_redraw", note = NULL, init = FALSE)
        }
        table$setRow(rowKey = 'montecarlo', values = list(
          method = paste0('Monte Carlo (', self$options$nPerms, ' simulations)'),
          statistic = stat_rs,
          df = df,
          mstat = '',
          pvalue = p_mc
        ))
      }
      
      if (!self$options$monteCarloTest)
        table$setNote(key = "mc_redraw", note = NULL, init = FALSE)
      
      # M Test (Fuchs & Kenett, 1980)
      if (self$options$mTest) {
        
        N <- sum(observed)
        I <- nrow(observed)
        J <- ncol(observed)
        k <- I * J  # Total number of cells
        
        row_sums <- rowSums(observed)
        col_sums <- colSums(observed)
        
        # Calculate adjusted residuals (Eq. 4.1 in Fuchs & Kenett, 1980)
        adj_residuals <- matrix(0, nrow = I, ncol = J)
        
        for (i in 1:I) {
          for (j in 1:J) {
            E_ij <- (row_sums[i] * col_sums[j]) / N
            num <- observed[i, j] - E_ij
            
            # Variance term: [n_i+ * n_+j * (N - n_i+) * (N - n_+j)] / N^3
            var_term <- (row_sums[i] * col_sums[j] * (N - row_sums[i]) * (N - col_sums[j])) / (N^3)
            
            # Handle edge case where variance is zero
            if (var_term <= 0) {
              adj_residuals[i, j] <- 0
            } else {
              adj_residuals[i, j] <- num / sqrt(var_term)
            }
          }
        }
        
        # M statistic: maximum absolute adjusted residual
        M_stat <- max(abs(adj_residuals))
        
        # Bonferroni-adjusted p-value (conservative, based on upper bound)
        # p = k * 2 * [1 - Phi(M)], capped at 1
        p_m <- min(1, k * 2 * (1 - pnorm(M_stat)))
        
        table$setRow(rowKey = 'mtest', values = list(
          method = 'M Test',
          statistic = '',
          df = '',
          mstat = M_stat,
          pvalue = p_m
        ))
      }
      
      # Add M test note if M test is selected
      if (self$options$mTest) {
        table$setNote(
          key = "mtest_note",
          note = "M test (Fuchs & Kenett 1980) may have higher power when association is driven by one or a few outlying cells.",
          init = FALSE
        )
      } else {
        # Remove a note left from an earlier run with the M test ticked
        # (the table is not cleared when only this option changes)
        table$setNote(key = "mtest_note", note = NULL, init = FALSE)
      }
    },
    
    .plotPermDist = function(image, ggtheme, theme, ...) {
      
      # Read from the image state (private fields are NULL on export)
      st <- image$state
      if (is.null(st) || is.null(st$perm_stats) || is.null(st$observed)) {
        return(FALSE)
      }
      
      plt <- .ctDistPlot(st$perm_stats, st$observed,
                         title = "Permutation Distribution of \u03c7\u00b2 Statistic",
                         label = "\u03c7\u00b2", fill = "#85C1E9", ggtheme = ggtheme)
      print(plt)
      TRUE
    },
    
    .plotMCDist = function(image, ggtheme, theme, ...) {
      
      # Read from the image state (private fields are NULL on export)
      st <- image$state
      if (is.null(st) || is.null(st$mc_stats) || is.null(st$observed)) {
        return(FALSE)
      }
      
      plt <- .ctDistPlot(st$mc_stats, st$observed,
                         title = "Monte Carlo Distribution of \u03c7\u00b2 Statistic",
                         label = "\u03c7\u00b2", fill = "#82E0AA", ggtheme = ggtheme)
      print(plt)
      TRUE
    },
    
    .populateMethodInfo = function() {
      
      if (!self$options$showMethodInfo) {
        return()
      }
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.95em;'>"
      html <- paste0(html, "<h3 style='color: #3E6DA6;'>Method Details</h3>")
      
      # Validity Assessment explanation (always shown)
      html <- paste0(html, "<h4 style='color: #3E6DA6;'>Test Validity Assessment</h4>")
      html <- paste0(html, "<p style='margin-bottom: 12px;'>The Test Validity Assessment table evaluates whether the ",
                     "chi-squared approximation is appropriate for your data. The recommendation follows these rules:</p>")
      html <- paste0(html, "<ol style='margin-bottom: 12px;'>")
      html <- paste0(html, "<li>If <strong>N < 10</strong>: sample size too small for asymptotic tests; ",
                     "use Permutation or Monte Carlo methods.</li>")
      html <- paste0(html, "<li>If <strong>N/k \u2265 5</strong> (average cell count at least 5): ",
                     "Traditional \u03C7\u00B2 test is appropriate.</li>")
      html <- paste0(html, "<li>If <strong>N/k < 5</strong> but <strong>N\u00B2/k \u2265 10</strong> and <strong>min(E) \u2265 1</strong>: ",
                     "(N-1)/N adjusted \u03C7\u00B2 test is recommended.</li>")
      html <- paste0(html, "<li>If <strong>N\u00B2/k < 10</strong> or <strong>min(E) < 1</strong>: ",
                     "Permutation or Monte Carlo methods are recommended.</li>")
      html <- paste0(html, "</ol>")
      html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: ",
                     "Koehler & Larntz 1980; Roscoe & Byars 1971; Zar 2014; Campbell 2007; Richardson 2011; Alberti 2024.</em></p>")
      
      if (self$options$traditionalTest) {
        html <- paste0(html, "<h4 style='color: #3E6DA6;'>Traditional Chi-Squared Test</h4>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>Rationale:</strong> The traditional chi-squared test of independence evaluates ",
                       "whether two categorical variables are associated. The test statistic is:</p>")
        html <- paste0(html, "<p style='text-align: center; margin-bottom: 12px;'>χ² = Σ[(O<sub>ij</sub> - E<sub>ij</sub>)² / E<sub>ij</sub>]</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'>where O<sub>ij</sub> are observed frequencies and E<sub>ij</sub> ",
                       "are expected frequencies under independence.</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>When the test is robust:</strong> The traditional chi-squared test's validity is not as fragile as ",
                       "once thought, especially when considering the average expected frequency across all cells, rather than the minimum ",
                       "expected value in any single cell. An average expected frequency of at least 5 across all cells should be sufficient ",
                       "for maintaining the chi-squared test's reliability at α = 0.05. As a consequence, a table's grand total equal to or ",
                       "larger than 5 times the number of cells ensures the applicability of the traditional chi-squared test.</p>")
        html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: Roscoe & Byars 1971; ",
                       "Greenwood & Nikulin 1996; Zar 2014; Agresti et al. 2022; Alberti 2024.</em></p>")
      }
      
      if (self$options$n1Test) {
        html <- paste0(html, "<h4 style='color: #3E6DA6;'>(N-1)/N Adjusted Chi-Squared Test</h4>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'>The adjustment is done by multiplying the chi-squared statistics by (N-1)/N, ",
                       "where N is the table grand total (sample size). The p-value of the corrected statistic is calculated ",
                       "the regular way (i.e., using the same degrees of freedom as in the traditional test). The correction ",
                       "seems particularly relevant for tables where N is smaller than 20 and where the expected frequencies ",
                       "are equal or larger than 1. The adjusted chi-squared test proves more conservative when the sample ",
                       "size is small. As N increases, the term (N-1)/N approaches 1, making the adjusted chi-squared value ",
                       "virtually equivalent to the unadjusted value.</p>")
        html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: Upton 1982; ",
                       "Rhoades & Overall 1982; Campbell 2007; Richardson 2011; Alberti 2024.</em></p>")
      }
      
      if (self$options$permTest) {
        html <- paste0(html, "<h4 style='color: #3E6DA6;'>Permutation Test</h4>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>Rationale:</strong> The permutation test randomly shuffles the labels of one variable ",
                       "repeatedly (", self$options$nPerms, " permutations) and recalculates the chi-squared ",
                       "statistic for each shuffled dataset, creating an empirical null distribution of the test statistic under the assumption ",
                       "of independence. The p-value represents the proportion of permuted statistics that are at least as extreme as the observed statistic, ",
                       "adjusted using the Phipson & Smyth 2010 method: p = (B + 1)/(m + 1), where B is the number of ",
                       "permuted statistics at least as extreme as the observed statistic, and m is the ",
                       "total number of permutations. This ensures p-values are never exactly zero and ",
                       "Type I error rates are correctly controlled. Because the permutation and Monte Carlo tests ",
                       "resample individual cases, non-integer counts are rounded to whole numbers for these two tests ",
                       "(a table note reports it).</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>When to use:</strong> Permutation tests are distribution-free and particularly useful ",
                       "when sample sizes are small or expected frequencies are low (minimum expected < 1), making asymptotic ",
                       "approximations unreliable.</p>")
        html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: Agresti et al. 2022; ",
                       "Phipson & Smyth 2010.</em></p>")
      }
      
      if (self$options$monteCarloTest) {
        html <- paste0(html, "<h4 style='color: #3E6DA6;'>Monte Carlo Test</h4>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>Rationale:</strong> The Monte Carlo test generates random contingency tables under the ",
                       "null hypothesis of independence (", self$options$nPerms, " simulated tables): each table distributes the N cases over the cells ",
                       "with probabilities equal to the products of the observed row and column proportions (a parametric bootstrap of the independence model). ",
                       "Only the total N is held fixed, so, unlike the permutation test, which keeps both sets of margins fixed, the row and column totals vary from table to table. ",
                       "Simulated tables with an empty row or column are redrawn. For each simulated table, the chi-squared statistic ",
                       "is calculated, creating an empirical null distribution. The p-value represents the proportion of simulated statistics ",
                       "at least as extreme as the observed statistic, computed as (B + 1)/(m + 1) following Phipson & Smyth 2010, ",
                       "which avoids p-values of exactly zero.</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>When to use:</strong> Monte Carlo tests are particularly useful for sparse tables ",
                       "or when cell counts are very small (minimum expected < 1), providing simulation-based p-values (approximations whose precision grows with the number of replicates) without relying on ",
                       "asymptotic approximations.</p>")
        html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: Beh & Lombardo 2014: 62-64; ",
                       "Howell 2011; Utts 2014; Lin et al. 2015; Phipson & Smyth 2010.</em></p>")
      }
      if (self$options$mTest) {
        html <- paste0(html, "<h4 style='color: #3E6DA6;'>M Test</h4>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>Rationale:</strong> The M test is an alternative ",
                       "test of independence based on the maximum absolute adjusted standardised residual (Haberman 1973) rather than the sum of ",
                       "squared residuals used by the chi-squared test. The test statistic is:</p>")
        html <- paste0(html, "<p style='text-align: center; margin-bottom: 12px;'>M = max|Z<sub>ij</sub>|</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'>where Z<sub>ij</sub> is the adjusted residual for cell (i, j). ",
                       "The null hypothesis of independence is rejected when M exceeds a critical value determined by ",
                       "the Bonferroni correction for multiple comparisons. The p-value is computed as:</p>")
        html <- paste0(html, "<p style='text-align: center; margin-bottom: 12px;'>p = min(1, k × 2 × [1 - Φ(M)])</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'>where k is the total number of cells (I × J) and Φ is the ",
                       "standard normal cumulative distribution function. This p-value is conservative (i.e., it may be ",
                       "slightly larger than the true p-value).</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>When to use:</strong> The M test can be used alongside ",
                       "the traditional chi-squared test. It may have higher power when association is driven by a single ",
                       "outlying cell or a small number of cells with unequal deviations. The test is asymptotically more ",
                       "powerful than the chi-squared test in these situations. When the null hypothesis is rejected, the ",
                       "cell(s) with |Z<sub>ij</sub>| > M* can be identified as outlying cells (see the adjusted standardised residuals provided by the Post-Hoc ",
                       "Analysis facility).</p>")
        html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: ",
                       "Haberman 1973; Fuchs & Kenett 1980.</em></p>")
      }
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    }#,
    #.populateReferences = function() {
    # Placeholder - do nothing
    #}
  )
)