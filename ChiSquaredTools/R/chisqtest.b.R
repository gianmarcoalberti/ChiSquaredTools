# This file is a generated template, your changes will not be overwritten

#' @importFrom stats chisq.test qnorm pchisq quantile
#' @importFrom stats medpolish xtabs as.formula
#' @export
chisqtestClass <- R6::R6Class(
  "chisqtestClass",
  inherit = chisqtestBase,
  private = list(
    
    # Store computed values for plotting
    .permStats = NULL,
    .mcStats = NULL,
    .observedChiSq = NULL,
    
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
      
      # Build contingency table based on data format
      if (is.null(self$options$counts)) {
        # Long format: one row per observation
        contingency_table <- table(data[[rowVar]], data[[colVar]])
      } else {
        # Wide format: aggregated data with counts variable
        countsVar <- self$options$counts
        formula_str <- paste0("`", countsVar, "` ~ `", rowVar, "` + `", colVar, "`")
        contingency_table <- stats::xtabs(stats::as.formula(formula_str), data = data)
        contingency_table <- as.table(contingency_table)
      }
      
      # Calculate expected frequencies
      n <- sum(contingency_table)
      row_totals <- rowSums(contingency_table)
      col_totals <- colSums(contingency_table)
      expected <- outer(row_totals, col_totals) / n
      dimnames(expected) <- dimnames(contingency_table)
      
      # Populate crosstab and expected tables first (refinement point 1)
      private$.populateCrosstab(contingency_table)
      private$.populateExpectedTable(expected)
      
      # Populate method guidance after tables (refinement point 2)
      if (self$options$showMethodGuidance) {
        guidance <- private$.generateMethodGuidance(contingency_table, expected)
        self$results$methodGuidance$setContent(guidance)
      }
      
      # Always call .runTests() - it handles caching internally
      private$.runTests(contingency_table, expected, data, rowVar, colVar)
      
      # Populate method info
      private$.populateMethodInfo()
      
      # Populate references
      private$.populateReferences()
    },
    
    .generateMethodGuidance = function(cross_tab, expected) {
      
      grand_total <- sum(cross_tab)
      num_cells <- nrow(cross_tab) * ncol(cross_tab)
      min_expected <- min(expected)
      
      # Check if the cross-tab is 2x2 or larger
      cross_tab_size <- ifelse(nrow(cross_tab) == 2 && ncol(cross_tab) == 2, "2x2", "larger")
      
      # Decision logic with explanations
      explanation <- "<div style='font-family: sans-serif; margin-bottom: 0px;'>"
      explanation <- paste0(explanation, "<h3>Method Selection Guidance</h3>")
      explanation <- paste0(explanation, "<p><strong>Table characteristics:</strong></p>")
      explanation <- paste0(explanation, "<ul>")
      explanation <- paste0(explanation, "<li>Table size: ", cross_tab_size, " (", 
                            nrow(cross_tab), " × ", ncol(cross_tab), ")</li>")
      explanation <- paste0(explanation, "<li>Grand total: ", grand_total, "</li>")
      explanation <- paste0(explanation, "<li>Number of cells: ", num_cells, "</li>")
      explanation <- paste0(explanation, "<li>Minimum expected frequency: ", 
                            sprintf("%.3f", min_expected), "</li>")
      explanation <- paste0(explanation, "<li>Average expected frequency: ", 
                            sprintf("%.3f", grand_total/num_cells), "</li>")
      explanation <- paste0(explanation, "</ul>")
      
      explanation <- paste0(explanation, "<p><strong>Recommendation:</strong></p><p>")
      
      if (cross_tab_size == "2x2") {
        explanation <- paste0(explanation, "Since the table is 2×2, ")
        if (grand_total >= 5 * num_cells) {
          explanation <- paste0(explanation, "and the grand total (", grand_total,
                                ") is ≥ 5 times the number of cells (", 5 * num_cells, 
                                "), use the <strong>traditional chi-squared test</strong>. ",
                                "Permutation or Monte Carlo methods can also be considered for robustness.")
        } else {
          if (min_expected >= 1) {
            explanation <- paste0(explanation, "the grand total (", grand_total,
                                  ") is < 5 times the number of cells (", 5 * num_cells, 
                                  "), and the minimum expected count (", sprintf("%.3f", min_expected), 
                                  ") is ≥ 1, use the <strong>(N-1)/N adjusted chi-squared test</strong>. ",
                                  "Permutation or Monte Carlo methods can also be considered.")
          } else {
            explanation <- paste0(explanation, "the grand total (", grand_total,
                                  ") is < 5 times the number of cells (", 5 * num_cells, 
                                  "), and the minimum expected count (", sprintf("%.3f", min_expected), 
                                  ") is < 1, use the <strong>Permutation or Monte Carlo method</strong>.")
          }
        }
      } else { # Larger than 2x2
        explanation <- paste0(explanation, "Since the table is larger than 2×2, ")
        if (grand_total >= 5 * num_cells) {
          explanation <- paste0(explanation, "and the grand total (", grand_total,
                                ") is ≥ 5 times the number of cells (", 5 * num_cells, 
                                "), use the <strong>traditional chi-squared test</strong>. ",
                                "Permutation or Monte Carlo methods can also be considered for robustness.")
        } else {
          if (min_expected >= 1) {
            explanation <- paste0(explanation, "the grand total (", grand_total,
                                  ") is < 5 times the number of cells (", 5 * num_cells, 
                                  "), and the minimum expected count (", sprintf("%.3f", min_expected), 
                                  ") is ≥ 1, use the <strong>(N-1)/N adjusted chi-squared test</strong>. ",
                                  "Permutation or Monte Carlo methods can also be considered.")
          } else {
            explanation <- paste0(explanation, "the grand total (", grand_total,
                                  ") is < 5 times the number of cells (", 5 * num_cells, 
                                  "), and the minimum expected count (", sprintf("%.3f", min_expected), 
                                  ") is < 1, use the <strong>Permutation or Monte Carlo method</strong>.")
          }
        }
      }
      
      explanation <- paste0(explanation, "</p>")
      
      explanation <- paste0(explanation, "<p style='font-size: 90%; color: #666; margin-bottom: 0px;'>",
                            "<em>Note: The threshold of 5 times the number of cells ensures an average ",
                            "expected frequency of at least 5, which maintains the chi-squared test's ",
                            "reliability at α = 0.05. See: Roscoe & Byars 1971; Greenwood & Nikulin 1996; Zar 2014; Alberti 2024.</em></p>")
      
      explanation <- paste0(explanation, "<p style='font-size: 90%; color: #666; margin-top: 10px; margin-bottom: 0px;'>",
                            "<em>Additionally, the M test (Fuchs & Kenett, 1980) can be used alongside any of the above tests. ",
                            "It may have higher power when association is driven by one or a few outlying cells.</em></p>")
      
      explanation <- paste0(explanation, "</div>")
      
      return(explanation)
    },
    
    .populateCrosstab = function(contingency_table) {
      
      table <- self$results$crosstabTable
      
      I <- nrow(contingency_table)
      J <- ncol(contingency_table)
      row_names <- rownames(contingency_table)
      col_names <- colnames(contingency_table)
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      table$setTitle(paste0("Observed Frequencies: ", rowVar, " × ", colVar))
      
      table$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text',
        combineBelow = FALSE
      )
      
      for (j in 1:J) {
        table$addColumn(
          name = paste0("col", j),
          title = col_names[j],
          type = 'integer',
          superTitle = colVar
        )
      }
      
      table$addColumn(
        name = 'rowtotal',
        title = 'Total',
        type = 'integer'
      )
      
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        for (j in 1:J) {
          row_values[[paste0("col", j)]] <- contingency_table[i, j]
        }
        row_values[['rowtotal']] <- sum(contingency_table[i, ])
        table$addRow(rowKey = i, values = row_values)
      }
      
      total_values <- list(rowname = 'Total')
      for (j in 1:J) {
        total_values[[paste0("col", j)]] <- sum(contingency_table[, j])
      }
      total_values[['rowtotal']] <- sum(contingency_table)
      table$addRow(rowKey = 'total', values = total_values)
    },
    
    .populateExpectedTable = function(expected) {
      
      table <- self$results$expectedTable
      
      I <- nrow(expected)
      J <- ncol(expected)
      row_names <- rownames(expected)
      col_names <- colnames(expected)
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      table$setTitle(paste0("Expected Frequencies: ", rowVar, " × ", colVar))
      
      table$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text',
        combineBelow = FALSE
      )
      
      for (j in 1:J) {
        table$addColumn(
          name = paste0("col", j),
          title = col_names[j],
          type = 'number',
          superTitle = colVar
        )
      }
      
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        for (j in 1:J) {
          row_values[[paste0("col", j)]] <- expected[i, j]
        }
        table$addRow(rowKey = i, values = row_values)
      }
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
      if (!is.null(mcImage$state) && !is.null(mcImage$state$mc_stats)) {
        private$.mcStats <- mcImage$state$mc_stats
      }
      
      # If table already has rows, skip repopulation entirely
      if (table$rowCount > 0) {
        return()
      }
      
      # --- Table is empty: populate it ---
      
      # Traditional chi-squared test
      if (self$options$traditionalTest) {
        p_value <- pchisq(chisq_stat, df, lower.tail = FALSE)
        table$addRow(rowKey = 'traditional', values = list(
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
        table$addRow(rowKey = 'n1adjusted', values = list(
          method = '(N-1)/N adjusted χ²',
          statistic = chisq_adj,
          df = df,
          mstat = '',
          pvalue = p_adj
        ))
      }
      
      # Permutation test
      if (self$options$permTest) {
        
        # Check cache first
        if (!is.null(private$.permStats)) {
          # Already restored from cache above
          perm_stats <- private$.permStats
          B_extreme <- sum(perm_stats >= chisq_stat)
          p_perm <- (B_extreme + 1) / (length(perm_stats) + 1)
        } else {
          # No cache - run expensive computation
          set.seed(self$options$seed)
          
          row_indices <- rep(1:nrow(observed), times = rowSums(observed))
          col_indices <- unlist(lapply(1:nrow(observed), function(i) {
            rep(1:ncol(observed), times = observed[i, ])
          }))
          
          B <- self$options$nPerms
          perm_stats <- numeric(B)
          
          for (b in 1:B) {
            perm_col_indices <- sample(col_indices)
            perm_table <- table(row_indices, perm_col_indices)
            perm_row_totals <- rowSums(perm_table)
            perm_col_totals <- colSums(perm_table)
            perm_expected <- outer(perm_row_totals, perm_col_totals) / n
            perm_stats[b] <- sum((perm_table - perm_expected)^2 / perm_expected)
          }
          
          B_extreme <- sum(perm_stats >= chisq_stat)
          p_perm <- (B_extreme + 1) / (B + 1)
          
          private$.permStats <- perm_stats
          permImage$setState(list(perm_stats = perm_stats, p_perm = p_perm))
        }
        
        table$addRow(rowKey = 'permutation', values = list(
          method = paste0('Permutation (', self$options$nPerms, ' permutations)'),
          statistic = chisq_stat,
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
          B_extreme <- sum(mc_stats >= chisq_stat)
          p_mc <- (B_extreme + 1) / (length(mc_stats) + 1)
        } else {
          # No cache - run expensive computation
          set.seed(self$options$seed)
          
          B <- self$options$nPerms
          mc_stats <- numeric(B)
          
          row_probs <- rowSums(observed) / n
          col_probs <- colSums(observed) / n
          
          for (b in 1:B) {
            mc_table <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
            
            for (i in 1:n) {
              row_idx <- sample(1:nrow(observed), size = 1, prob = row_probs)
              col_idx <- sample(1:ncol(observed), size = 1, prob = col_probs)
              mc_table[row_idx, col_idx] <- mc_table[row_idx, col_idx] + 1
            }
            
            mc_row_totals <- rowSums(mc_table)
            mc_col_totals <- colSums(mc_table)
            
            if (any(mc_row_totals == 0) || any(mc_col_totals == 0)) {
              mc_stats[b] <- 0
              next
            }
            
            mc_expected <- outer(mc_row_totals, mc_col_totals) / n
            mc_stats[b] <- sum((mc_table - mc_expected)^2 / mc_expected)
          }
          
          B_extreme <- sum(mc_stats >= chisq_stat)
          p_mc <- (B_extreme + 1) / (B + 1)
          
          private$.mcStats <- mc_stats
          mcImage$setState(list(mc_stats = mc_stats, p_mc = p_mc))
        }
        
        table$addRow(rowKey = 'montecarlo', values = list(
          method = paste0('Monte Carlo (', self$options$nPerms, ' simulations)'),
          statistic = chisq_stat,
          df = df,
          mstat = '',
          pvalue = p_mc
        ))
      }
      
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
        
        table$addRow(rowKey = 'mtest', values = list(
          method = 'M Test',
          statistic = '',
          df = '',
          mstat = M_stat,
          pvalue = p_m
        ))
      }
    },
    
    .plotPermDist = function(image, ...) {
      
      if (is.null(private$.permStats)) {
        return(FALSE)
      }
      
      perm_stats <- private$.permStats
      observed_chisq <- private$.observedChiSq
      
      # Create histogram
      hist_data <- hist(perm_stats, plot = FALSE, breaks = 30)
      
      plot(hist_data,
           main = "Permutation Distribution of χ² Statistic",
           xlab = "χ² Statistic",
           ylab = "Frequency",
           col = "lightblue",
           border = "white")
      
      # Determine plot x-axis range
      x_max <- max(hist_data$breaks)
      
      # Calculate p-value (needed for all cases)
      B_extreme <- sum(perm_stats >= observed_chisq)
      p_perm <- (B_extreme + 1) / (length(perm_stats) + 1)
      
      # Three-way logic: well inside / near edge / off-scale
      if (observed_chisq <= x_max * 0.9) {
        # Case 1: Observed value well inside plot range
        # Draw line with text to the RIGHT
        abline(v = observed_chisq, col = "red", lwd = 2, lty = 2)
        
        legend("topright",
               legend = c("Permuted χ²", "Observed χ²"),
               col = c("lightblue", "red"),
               lwd = c(10, 2),
               lty = c(1, 2),
               bty = "n")
        
        text(x = observed_chisq, 
             y = max(hist_data$counts) * 0.7,
             labels = if (p_perm < 0.001) {
               "p < 0.001"
             } else {
               sprintf("p = %.3f", p_perm)
             },
             pos = 4,  # Text to the RIGHT of line
             col = "red",
             font = 2)
        
      } else if (observed_chisq <= x_max) {
        # Case 2: Observed value near right edge but still on-scale
        # Draw line with text to the LEFT to avoid truncation
        abline(v = observed_chisq, col = "red", lwd = 2, lty = 2)
        
        legend("topright",
               legend = c("Permuted χ²", "Observed χ²"),
               col = c("lightblue", "red"),
               lwd = c(10, 2),
               lty = c(1, 2),
               bty = "n")
        
        text(x = observed_chisq, 
             y = max(hist_data$counts) * 0.7,
             labels = if (p_perm < 0.001) {
               "p < 0.001"
             } else {
               sprintf("p = %.3f", p_perm)
             },
             pos = 2,  # Text to the LEFT of line (avoids right margin)
             col = "red",
             font = 2)
        
      } else {
        # Case 3: Observed value is off-scale
        # Draw arrow + annotation at plot boundary
        legend("topright",
               legend = c("Permuted χ²"),
               col = c("lightblue"),
               lwd = c(10),
               lty = c(1),
               bty = "n")
        
        # Add arrow at right edge pointing rightward
        y_arrow <- max(hist_data$counts) * 0.5
        arrows(x0 = x_max * 0.95, y0 = y_arrow, 
               x1 = x_max * 0.99, y1 = y_arrow,
               col = "red", lwd = 2, length = 0.15, angle = 20)
        
        # Add text annotation
        p_text <- if (p_perm < 0.001) {
          "< 0.001"
        } else {
          sprintf("%.3f", p_perm)
        }
        
        text(x = x_max * 0.92, 
             y = y_arrow,
             labels = sprintf("Obs. χ² = %.2f\n(off scale)\np = %s", observed_chisq, p_text),
             pos = 2,
             col = "red",
             font = 2,
             cex = 0.9)
      }
      
      TRUE
    },
    
    .plotMCDist = function(image, ...) {
      
      if (is.null(private$.mcStats)) {
        return(FALSE)
      }
      
      mc_stats <- private$.mcStats
      observed_chisq <- private$.observedChiSq
      
      # Create histogram
      hist_data <- hist(mc_stats, plot = FALSE, breaks = 30)
      
      plot(hist_data,
           main = "Monte Carlo Distribution of χ² Statistic",
           xlab = "χ² Statistic",
           ylab = "Frequency",
           col = "lightgreen",
           border = "white")
      
      # Determine plot x-axis range
      x_max <- max(hist_data$breaks)
      
      # Calculate p-value (needed for all cases)
      B_extreme <- sum(mc_stats >= observed_chisq)
      p_mc <- (B_extreme + 1) / (length(mc_stats) + 1)
      
      # Three-way logic: well inside / near edge / off-scale
      if (observed_chisq <= x_max * 0.9) {
        # Case 1: Observed value well inside plot range
        # Draw line with text to the RIGHT
        abline(v = observed_chisq, col = "red", lwd = 2, lty = 2)
        
        legend("topright",
               legend = c("Monte Carlo χ²", "Observed χ²"),
               col = c("lightgreen", "red"),
               lwd = c(10, 2),
               lty = c(1, 2),
               bty = "n")
        
        text(x = observed_chisq, 
             y = max(hist_data$counts) * 0.7,
             labels = if (p_mc < 0.001) {
               "p < 0.001"
             } else {
               sprintf("p = %.3f", p_mc)
             },
             pos = 4,  # Text to the RIGHT of line
             col = "red",
             font = 2)
        
      } else if (observed_chisq <= x_max) {
        # Case 2: Observed value near right edge but still on-scale
        # Draw line with text to the LEFT to avoid truncation
        abline(v = observed_chisq, col = "red", lwd = 2, lty = 2)
        
        legend("topright",
               legend = c("Monte Carlo χ²", "Observed χ²"),
               col = c("lightgreen", "red"),
               lwd = c(10, 2),
               lty = c(1, 2),
               bty = "n")
        
        text(x = observed_chisq, 
             y = max(hist_data$counts) * 0.7,
             labels = if (p_mc < 0.001) {
               "p < 0.001"
             } else {
               sprintf("p = %.3f", p_mc)
             },
             pos = 2,  # Text to the LEFT of line (avoids right margin)
             col = "red",
             font = 2)
        
      } else {
        # Case 3: Observed value is off-scale
        # Draw arrow + annotation at plot boundary
        legend("topright",
               legend = c("Monte Carlo χ²"),
               col = c("lightgreen"),
               lwd = c(10),
               lty = c(1),
               bty = "n")
        
        # Add arrow at right edge pointing rightward
        y_arrow <- max(hist_data$counts) * 0.5
        arrows(x0 = x_max * 0.95, y0 = y_arrow, 
               x1 = x_max * 0.99, y1 = y_arrow,
               col = "red", lwd = 2, length = 0.15, angle = 20)
        
        # Add text annotation
        p_text <- if (p_mc < 0.001) {
          "< 0.001"
        } else {
          sprintf("%.3f", p_mc)
        }
        
        text(x = x_max * 0.92, 
             y = y_arrow,
             labels = sprintf("Obs. χ² = %.2f\n(off scale)\np = %s", observed_chisq, p_text),
             pos = 2,
             col = "red",
             font = 2,
             cex = 0.9)
      }
      
      TRUE
    },
    
    .populateMethodInfo = function() {
      
      if (!self$options$showMethodInfo) {
        return()
      }
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.95em;'>"
      html <- paste0(html, "<h3 style='color: #3E6DA6;'>Method Details</h3>")
      
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
                       "Greenwood & Nikulin 1996; Zar 2014; Agresti et al. 200; Alberti 2024.</em></p>")
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
                       "Type I error rates are correctly controlled.</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>When to use:</strong> Permutation tests are distribution-free and particularly useful ",
                       "when sample sizes are small or expected frequencies are low (minimum expected < 1), making asymptotic ",
                       "approximations unreliable.</p>")
        html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: Agresti et al. 2022; ",
                       "Phipson & Smyth 2010.</em></p>")
      }
      
      if (self$options$monteCarloTest) {
        html <- paste0(html, "<h4 style='color: #3E6DA6;'>Monte Carlo Test</h4>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>Rationale:</strong> The Monte Carlo test generates random contingency tables under the ",
                       "null hypothesis of independence (", self$options$nPerms, " simulated tables) by sampling ",
                       "from the marginal distributions. For each simulated table, the chi-squared statistic ",
                       "is calculated, creating an empirical null distribution. The p-value represents the proportion of simulated statistics ",
                       "at least as extreme as the observed statistic. P-values are computed using the Phipson & Smyth 2010 method, ",
                       "ensuring accurate Type I error control and avoiding zero p-values.</p>")
        html <- paste0(html, "<p style='margin-bottom: 12px;'><strong>When to use:</strong> Monte Carlo tests are particularly useful for sparse tables ",
                       "or when cell counts are very small (minimum expected < 1), providing exact p-values without relying on ",
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
    },
    
    .populateReferences = function() {
      
      references_html <- paste0(
        "<div style='font-size: 0.85em; color: #444; margin: 15px 0; line-height: 1.5;'>",
        "<h3 style='color: #2874A6; margin-top: 0.5em; margin-bottom: 0.5em;'>References</h3>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Agresti, A., Franklin, C., & Klingenberg, B. (2022). <em>Statistics: The Art and Science of Learning from Data</em> (5th ed.). Pearson Education.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Beh, E. J., & Lombardo, R. (2014). <em>Correspondence Analysis: Theory, Practice and New Strategies</em>. Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Campbell, I. (2007). Chi-squared and Fisher–Irwin tests of two-by-two tables with small sample recommendations. <em>Statistics in Medicine</em>, 26(19), 3661-3675.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Fuchs, C., & Kenett, R. (1980). A test for detecting outlying cells in the multinomial distribution and two-way contingency tables. <em>Journal of the American Statistical Association</em>, 75(370), 395-398.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Greenwood, P. E., & Nikulin, M. S. (1996). <em>A Guide to Chi-Squared Testing</em>. Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Haberman, S. J. (1973). The analysis of residuals in cross-classified tables. <em>Biometrics</em>, 29(1), 205-220.</p>",
        "Howell, D. C. (2011). <em>Statistical Methods for Psychology</em> (8th ed.). Wadsworth Publishing.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Lin, J.-J., Chang, C.-H., & Pal, N. (2015). A revisit to contingency table and tests of independence: Bootstrap is preferred to chi-square approximations as well as Fisher's exact test. <em>Journal of Biopharmaceutical Statistics</em>, 25(3), 438-458.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Phipson, B., & Smyth, G. K. (2010). Permutation p-values should never be zero: calculating exact p-values when permutations are randomly drawn. <em>Statistical Applications in Genetics and Molecular Biology</em>, 9(1), Article 39.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Rhoades, H. M., & Overall, J. E. (1982). A sample size correction for Pearson chi-squared in 2 × 2 contingency tables. <em>Psychological Bulletin</em>, 91(2), 418-423.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Richardson, J. T. E. (2011). The analysis of 2 × 2 contingency tables—yet again. <em>Statistics in Medicine</em>, 30(8), 890.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Roscoe, J. T., & Byars, J. A. (1971). An investigation of the restraints with respect to sample size commonly imposed on the use of the chi-squared statistic. <em>Journal of the American Statistical Association</em>, 66(336), 755-759.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Upton, G. J. G. (1982). A comparison of alternative tests for the 2 × 2 comparative trial. <em>Journal of the Royal Statistical Society, Series A</em>, 145(1), 86-105.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Utts, J. M. (2014). <em>Seeing Through Statistics</em> (4th ed.). Brooks/Cole.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Zar, J. H. (2014). <em>Biostatistical Analysis</em> (5th ed.). Pearson Education Limited.</p>",
        "</div>"
      )
      
      self$results$legendNote$setContent(references_html)
    }
  )
)