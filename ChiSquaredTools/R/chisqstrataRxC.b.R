# This file is a generated template, your changes will not be overwritten

#' @importFrom stats chisq.test mantelhaen.test pchisq xtabs as.formula glm poisson anova quantile
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh geom_hline geom_vline geom_segment geom_rect geom_text geom_line geom_ribbon labs theme_minimal theme element_text element_blank scale_x_continuous scale_y_discrete scale_y_continuous scale_color_manual scale_shape_manual coord_cartesian annotate
#' @export
chisqstrataRxCClass <- R6::R6Class(
  "chisqstrataRxCClass",
  inherit = chisqstrataRxCBase,
  private = list(
    
    # Cache fields for expensive computations
    .cachedDataSignature = NULL,
    .cachedVResults = NULL,
    .cachedMarginalVResult = NULL,
    .cachedWeightedVcorr = NULL,
    .cachedWeightedCI = NULL,
    .cachedChiSqResults = NULL,
    .cachedMarginalChiSq = NULL,
    .cachedCmhTest = NULL,
    .cachedLoglinResult = NULL,
    
    .run = function() {
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 1. Input validation
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (is.null(self$options$rows) || 
          is.null(self$options$cols) || 
          is.null(self$options$strata)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      strataVar <- self$options$strata
      nBootstrap <- self$options$nBootstrap
      
      data <- self$data
      
      # Ensure variables are factors
      if (!is.factor(data[[rowVar]])) {
        data[[rowVar]] <- as.factor(data[[rowVar]])
      }
      if (!is.factor(data[[colVar]])) {
        data[[colVar]] <- as.factor(data[[colVar]])
      }
      if (!is.factor(data[[strataVar]])) {
        data[[strataVar]] <- as.factor(data[[strataVar]])
      }
      
      # Check for valid data
      if (nrow(data) == 0) {
        return()
      }
      
      # Get dimensions
      nRowLevels <- nlevels(data[[rowVar]])
      nColLevels <- nlevels(data[[colVar]])
      nStrataLevels <- nlevels(data[[strataVar]])
      
      # Validate minimum requirements
      if (nRowLevels < 2) {
        jmvcore::reject(
          paste0("Row variable '", rowVar, "' must have at least 2 levels (has ", 
                 nRowLevels, ")"),
          code = "invalid_row_levels"
        )
      }
      
      if (nColLevels < 2) {
        jmvcore::reject(
          paste0("Column variable '", colVar, "' must have at least 2 levels (has ", 
                 nColLevels, ")"),
          code = "invalid_col_levels"
        )
      }
      
      if (nStrataLevels < 2) {
        jmvcore::reject(
          paste0("Stratifying variable '", strataVar, "' must have at least 2 levels"),
          code = "invalid_strata_levels"
        )
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 2. Build the 3D array of partial tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      array3D <- private$.buildStratifiedArray(data, rowVar, colVar, strataVar)
      strataNames <- dimnames(array3D)[[3]]
      K <- dim(array3D)[3]
      
      # Convert to list of matrices for internal processing
      listOfTables <- lapply(1:K, function(i) array3D[,,i])
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 3. Populate partial tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populatePartialTables(array3D, strataNames, rowVar, colVar, strataVar)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 4. Compute and populate marginal table
      # ═══════════════════════════════════════════════════════════════════════════
      
      marginalTable <- Reduce("+", listOfTables)
      dimnames(marginalTable) <- dimnames(array3D)[1:2]
      private$.populateMarginalTable(marginalTable, rowVar, colVar)
      
      # Add section header for marginal table
      self$results$marginalTableHeader$setContent(
        "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Marginal Table</h2>"
      )
      
      # Add section header for analysis results
      self$results$analysisResultsHeader$setContent(
        "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Statistical Tests and Summary Measures</h2>"
      )
      
      # Add section header for interpretation guide
      if (self$options$showInterpretation) {
        self$results$interpretationGuideHeader$setContent(
          "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Interpretation Guide</h2>"
        )
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 5. Compute V corrected and bootstrap CIs for each stratum
      # ═══════════════════════════════════════════════════════════════════════════
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 5. Compute V corrected (point estimates always; bootstrap CIs only if needed)
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Determine whether bootstrap CIs are actually required for display
      # CIs are shown in: stratumResultsTable (always), summaryMeasureTable (always),
      # forestPlot (if enabled), interpretation (if enabled)
      # Since the core tables always display CIs, we need bootstrap on first computation.
      # However, we can CACHE the results to avoid recomputation on display-only changes.
      
      # Create a signature of the data and analytical parameters
      dataSignature <- paste(
        paste(dim(array3D), collapse = "x"),
        nBootstrap,
        self$options$rows,
        self$options$cols,
        self$options$strata,
        sep = "|"
      )
      
      # Check if cached results exist and match current data
      if (!is.null(private$.cachedDataSignature) && 
          private$.cachedDataSignature == dataSignature &&
          !is.null(private$.cachedVResults)) {
        
        # Reuse cached results (display options changed, data unchanged)
        vResults <- private$.cachedVResults
        marginalVResult <- private$.cachedMarginalVResult
        weighted_vcorr <- private$.cachedWeightedVcorr
        weightedCI <- private$.cachedWeightedCI
        chiSqResults <- private$.cachedChiSqResults
        marginalChiSq <- private$.cachedMarginalChiSq
        cmhTest <- private$.cachedCmhTest
        loglinResult <- private$.cachedLoglinResult
        
      } else {
        
        # Compute fresh (first run or data/parameters changed)
        vResults <- lapply(listOfTables, private$.calculateVcorrectedAndCI, nBootstrap)
        marginalVResult <- private$.calculateVcorrectedAndCI(marginalTable, nBootstrap)
        
        stratum_totals <- sapply(listOfTables, sum)
        v_corrected_values <- sapply(vResults, function(x) x$vcorr)
        weighted_vcorr <- sum(stratum_totals * v_corrected_values) / sum(stratum_totals)
        
        weightedCI <- private$.bootstrapWeightedVcorrected(listOfTables, nBootstrap)
        
        # ═══════════════════════════════════════════════════════════════════════════
        # 6. Chi-squared tests for partial tables
        # ═══════════════════════════════════════════════════════════════════════════
        
        suppressWarnings({
          chiSqResults <- lapply(listOfTables, function(tbl) {
            test <- stats::chisq.test(tbl, correct = FALSE)
            list(
              statistic = as.numeric(test$statistic),
              df = as.integer(test$parameter),
              pvalue = test$p.value
            )
          })
          
          marginalChiSq <- stats::chisq.test(marginalTable, correct = FALSE)
          
          # Generalised CMH test
          cmhTest <- stats::mantelhaen.test(array3D, correct = FALSE)
        })
        
        # ═══════════════════════════════════════════════════════════════════════════
        # 7. Log-linear homogeneity test
        # ═══════════════════════════════════════════════════════════════════════════
        
        loglinResult <- private$.loglinearHomogeneityTest(array3D)
        
        # Cache results for potential reuse
        private$.cachedDataSignature <- dataSignature
        private$.cachedVResults <- vResults
        private$.cachedMarginalVResult <- marginalVResult
        private$.cachedWeightedVcorr <- weighted_vcorr
        private$.cachedWeightedCI <- weightedCI
        private$.cachedChiSqResults <- chiSqResults
        private$.cachedMarginalChiSq <- marginalChiSq
        private$.cachedCmhTest <- cmhTest
        private$.cachedLoglinResult <- loglinResult
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 8. Populate results tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateStratumResultsTable(vResults, marginalVResult, chiSqResults, 
                                            marginalChiSq, strataNames)
      
      private$.populateCMHTable(cmhTest)
      private$.populateHomogeneityTable(loglinResult)
      private$.populateSummaryMeasureTable(weighted_vcorr, weightedCI)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 8b. Compute and populate adjusted standardised residuals (if requested)
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (self$options$showResiduals) {
        private$.populateResidualsSection(listOfTables, strataNames, rowVar, colVar)
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 9. Prepare plot states
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (self$options$showForestPlot) {
        forestData <- private$.prepareForestData(vResults, marginalVResult, 
                                                  weighted_vcorr, weightedCI, 
                                                  strataNames, K)
        image <- self$results$forestPlot
        image$setState(forestData)
        # Dynamic height: 200 base + 60 per stratum + 80 for marginal/weighted
        image$setSize(700, 200 + (K * 60) + 80)
      }
      
      if (self$options$showDiagnosticTree) {
        treeData <- private$.prepareDiagnosticTreeData(cmhTest$p.value, 
                                                        loglinResult$pvalue)
        image <- self$results$diagnosticTree
        image$setState(treeData)
      }
      
      if (self$options$showTrajectoryPlot && self$options$strataOrdered) {
        trajectoryData <- private$.prepareTrajectoryData(vResults, strataNames)
        image <- self$results$trajectoryPlot
        image$setState(trajectoryData)
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 10. Generate interpretation
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (self$options$showInterpretation) {
        private$.populateInterpretation(
          chiSqResults, marginalChiSq, cmhTest, loglinResult,
          vResults, weighted_vcorr, weightedCI, strataNames,
          rowVar, colVar, strataVar, nRowLevels, nColLevels
        )
      }
      
      # Populate diagnostic summary
      if (self$options$showDiagnosticSummary) {
        private$.populateDiagnosticSummary(cmhTest, loglinResult, 
                                           rowVar, colVar, strataVar)
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 11. Populate method information and references
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateMethodInfo()
      private$.populateReferences()
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Build 3D array from data
    # ═══════════════════════════════════════════════════════════════════════════
    
    .buildStratifiedArray = function(data, rowVar, colVar, strataVar) {
      
      if (is.null(self$options$counts)) {
        # Long format: one row per observation
        fullTable <- table(data[[rowVar]], data[[colVar]], data[[strataVar]])
      } else {
        # Wide format: aggregated data with counts variable
        countsVar <- self$options$counts
        formula_str <- paste0(countsVar, " ~ ", rowVar, " + ", colVar, " + ", strataVar)
        fullTable <- stats::xtabs(stats::as.formula(formula_str), data = data)
      }
      
      # Ensure it is a proper 3D array
      fullTable <- as.array(fullTable)
      
      return(fullTable)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Calculate V, V_max, V_corrected and bootstrap CI
    # ═══════════════════════════════════════════════════════════════════════════
    
    .calculateVcorrectedAndCI = function(tbl, nBootstrap) {
      
      n <- sum(tbl)
      nr <- nrow(tbl)
      nc <- ncol(tbl)
      
      # Handle empty strata (n=0) to prevent NaN errors
      if (n == 0) {
        return(list(
          chisq = 0,
          df = (nr - 1) * (nc - 1),
          v = 0,
          vmax = 0,
          vcorr = 0,
          ci_lower = 0,
          ci_upper = 0
        ))
      }
      k <- min(nr - 1, nc - 1)
      
      row_totals <- rowSums(tbl)
      col_totals <- colSums(tbl)
      
      # 1. Compute Observed Statistics
      expected <- outer(row_totals, col_totals) / n
      
      # Handle potential division by zero for zero expected counts
      term <- (tbl - expected)^2 / expected
      term[expected == 0] <- 0
      chisq_obs <- sum(term)
      v_obs <- sqrt(chisq_obs / (n * k))
      
      # 2. Compute Max Statistics (Observed)
      max_result <- private$.max_chisq(row_totals, col_totals)
      chisq_max <- max_result$max_chisq
      v_max <- sqrt(chisq_max / (n * k))
      
      # 3. Point Estimate: V_corrected
      # Avoid division by zero if chisq_max is 0
      if (chisq_max > 0) {
        v_corr <- sqrt(as.numeric(chisq_obs) / chisq_max)
      } else {
        v_corr <- 0
      }
      if (v_corr > 1) v_corr <- 1
      
      # 4. Bootstrap Procedure
      # Expand table to raw dataframe for proper resampling of observations
      df_raw <- as.data.frame(as.table(tbl)) 
      # df_raw columns are: Var1, Var2, Freq. Expand rows:
      df_expanded <- df_raw[rep(1:nrow(df_raw), df_raw$Freq), c(1, 2)]
      
      boot_v_corr <- numeric(nBootstrap)
      
      for (b in 1:nBootstrap) {
        # A. Resample observations (Row-wise bootstrap)
        indices <- sample.int(n, n, replace = TRUE)
        df_boot <- df_expanded[indices, ]
        
        # B. Reconstruct Table
        # We must maintain original dimensions/levels even if some counts drop to 0
        tbl_boot <- table(
          factor(df_boot[[1]], levels = rownames(tbl)),
          factor(df_boot[[2]], levels = colnames(tbl))
        )
        
        # C. Calculate Bootstrapped Chi-Observed
        # Use suppressWarnings for small cell counts in bootstrap samples
        suppressWarnings({
          test_boot <- stats::chisq.test(tbl_boot, correct = FALSE)
          chi_boot_obs <- as.numeric(test_boot$statistic)
        })
        
        # D. Calculate Bootstrapped Chi-Max
        # CRITICAL: Max must be recalculated because marginals change in bootstrap
        r_boot <- rowSums(tbl_boot)
        c_boot <- colSums(tbl_boot)
        max_res_boot <- private$.max_chisq(r_boot, c_boot)
        chi_boot_max <- max_res_boot$max_chisq
        
        # E. Calculate V_corrected for this replicate
        if (chi_boot_max > 0) {
          val <- sqrt(chi_boot_obs / chi_boot_max)
        } else {
          val <- 0
        }
        if (val > 1) val <- 1
        boot_v_corr[b] <- val
      }
      
      # Percentile CI
      ci <- quantile(boot_v_corr, probs = c(0.025, 0.975), na.rm = TRUE)
      
      return(list(
        chisq = chisq_obs,
        df = (nr - 1) * (nc - 1),
        v = v_obs,
        vmax = v_max,
        vcorr = v_corr,
        ci_lower = as.numeric(ci[1]),
        ci_upper = as.numeric(ci[2])
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Bootstrap weighted V_corrected CI
    # ═══════════════════════════════════════════════════════════════════════════
    
    .bootstrapWeightedVcorrected = function(listOfTables, nBootstrap) {
      
      K <- length(listOfTables)
      stratum_totals <- sapply(listOfTables, sum)
      
      # Pre-expand all partial tables to dataframes to speed up the loop
      listOfDataFrames <- lapply(listOfTables, function(tbl) {
        df_raw <- as.data.frame(as.table(tbl))
        df_expanded <- df_raw[rep(1:nrow(df_raw), df_raw$Freq), c(1, 2)]
        return(list(
          data = df_expanded, 
          r_levels = rownames(tbl), 
          c_levels = colnames(tbl)
        ))
      })
      
      boot_weighted <- numeric(nBootstrap)
      
      for (b in 1:nBootstrap) {
        boot_v_k <- numeric(K)
        
        for (i in 1:K) {
          # Extract prep info
          df_curr <- listOfDataFrames[[i]]$data
          r_lev <- listOfDataFrames[[i]]$r_levels
          c_lev <- listOfDataFrames[[i]]$c_levels
          n <- nrow(df_curr)
          
          # A. Resample
          indices <- sample.int(n, n, replace = TRUE)
          df_boot <- df_curr[indices, ]
          
          # B. Form Table
          tbl_boot <- table(
            factor(df_boot[[1]], levels = r_lev),
            factor(df_boot[[2]], levels = c_lev)
          )
          
          # C. Chi Obs
          suppressWarnings({
            chi_obs <- stats::chisq.test(tbl_boot, correct = FALSE)$statistic
          })
          
          # D. Chi Max (Recalculate!)
          r_boot <- rowSums(tbl_boot)
          c_boot <- colSums(tbl_boot)
          max_res <- private$.max_chisq(r_boot, c_boot)
          chi_max <- max_res$max_chisq
          
          # E. V Corrected for Stratum i
          if (chi_max > 0) {
            val <- sqrt(as.numeric(chi_obs) / chi_max)
          } else {
            val <- 0
          }
          if (val > 1) val <- 1
          boot_v_k[i] <- val
        }
        
        # Calculate weighted average for this bootstrap iteration
        # Weights are sample sizes (fixed across bootstraps)
        boot_weighted[b] <- sum(stratum_totals * boot_v_k) / sum(stratum_totals)
      }
      
      # Percentile CI
      ci <- quantile(boot_weighted, probs = c(0.025, 0.975), na.rm = TRUE)
      
      return(list(
        ci_lower = as.numeric(ci[1]),
        ci_upper = as.numeric(ci[2])
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Maximum chi-squared given marginals (for V_corrected)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .max_chisq = function(row_totals, col_totals) {
      
      n <- sum(row_totals)
      
      # Return 0 if n is 0 to avoid division by zero
      if (n == 0) {
        return(list(
          max_chisq = 0, 
          max_table = matrix(0, length(row_totals), length(col_totals))
        ))
      }
      
      nr <- length(row_totals)
      nc <- length(col_totals)
      
      # Sort marginals in descending order
      r_sorted <- sort(row_totals, decreasing = TRUE)
      c_sorted <- sort(col_totals, decreasing = TRUE)
      
      # Greedy algorithm to find maximum chi-squared table
      max_table <- matrix(0, nr, nc)
      r_remain <- r_sorted
      c_remain <- c_sorted
      
      for (i in 1:nr) {
        for (j in 1:nc) {
          fill <- min(r_remain[i], c_remain[j])
          max_table[i, j] <- fill
          r_remain[i] <- r_remain[i] - fill
          c_remain[j] <- c_remain[j] - fill
        }
      }
      
      # Calculate chi-squared for max table
      expected <- outer(r_sorted, c_sorted) / n
      
      # Avoid division by zero for zero expected values
      chisq_max <- 0
      for (i in 1:nr) {
        for (j in 1:nc) {
          if (expected[i, j] > 0) {
            chisq_max <- chisq_max + (max_table[i, j] - expected[i, j])^2 / expected[i, j]
          }
        }
      }
      
      return(list(max_chisq = chisq_max, max_table = max_table))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Log-linear homogeneity test
    # ═══════════════════════════════════════════════════════════════════════════
    
    .loglinearHomogeneityTest = function(array3D) {
      
      # Convert 3D array to data frame for glm
      df <- as.data.frame(as.table(array3D))
      names(df) <- c("Row", "Col", "Stratum", "Freq")
      
      # Fit homogeneous model: Row*Col + Row*Stratum + Col*Stratum
      # (no three-way interaction)
      suppressWarnings({
        model_homo <- stats::glm(Freq ~ Row*Col + Row*Stratum + Col*Stratum,
                                  data = df, family = stats::poisson())
        
        # Fit saturated model: Row*Col*Stratum
        model_sat <- stats::glm(Freq ~ Row*Col*Stratum,
                                 data = df, family = stats::poisson())
      })
      
      # Likelihood ratio test
      lrt <- stats::anova(model_homo, model_sat, test = "LRT")
      
      # Extract G² statistic (deviance difference)
      G2 <- lrt$Deviance[2]
      df_diff <- lrt$Df[2]
      pvalue <- lrt$`Pr(>Chi)`[2]
      
      # Safeguard against NA values
      if (is.na(pvalue)) pvalue <- 1
      if (is.na(G2)) G2 <- 0
      if (is.na(df_diff)) df_diff <- 0
      
      return(list(
        statistic = G2,
        df = df_diff,
        pvalue = pvalue
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial tables (one per stratum)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populatePartialTables = function(array3D, strataNames, rowVar, colVar, strataVar) {
      
      K <- dim(array3D)[3]
      rowNames <- dimnames(array3D)[[1]]
      colNames <- dimnames(array3D)[[2]]
      nRows <- length(rowNames)
      nCols <- length(colNames)
      
      partialTablesGroup <- self$results$partialTablesGroup
      
      for (k in 1:K) {
        
        # Get the partial table
        partialTab <- array3D[,,k]
        
        # Add table to array
        partialTablesGroup$addItem(key = k)
        table <- partialTablesGroup$get(key = k)
        
        # Set title
        table$setTitle(paste0(strataVar, " = ", strataNames[k]))
        
        # Add row name column
        table$addColumn(
          name = 'rowname',
          title = rowVar,
          type = 'text'
        )
        
        # Add data columns
        for (j in 1:nCols) {
          table$addColumn(
            name = paste0("col", j),
            title = colNames[j],
            superTitle = colVar,
            type = 'integer'
          )
        }
        
        # Add row total column
        table$addColumn(
          name = 'rowtotal',
          title = 'Total',
          type = 'integer'
        )
        
        # Populate rows
        for (i in 1:nRows) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:nCols) {
            rowValues[[paste0("col", j)]] <- partialTab[i, j]
          }
          rowValues[['rowtotal']] <- sum(partialTab[i,])
          table$addRow(rowKey = i, values = rowValues)
        }
        
        # Add column totals row
        totalRowValues <- list(rowname = 'Total')
        for (j in 1:nCols) {
          totalRowValues[[paste0("col", j)]] <- sum(partialTab[,j])
        }
        totalRowValues[['rowtotal']] <- sum(partialTab)
        table$addRow(rowKey = 'total', values = totalRowValues)
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Marginal table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMarginalTable = function(marginalTable, rowVar, colVar) {
      
      table <- self$results$marginalTable
      table$setTitle("Marginal Table (Collapsed)")
      rowNames <- rownames(marginalTable)
      colNames <- colnames(marginalTable)
      nRows <- nrow(marginalTable)
      nCols <- ncol(marginalTable)
      
      # Add row name column
      table$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text'
      )
      
      # Add data columns
      for (j in 1:nCols) {
        table$addColumn(
          name = paste0("col", j),
          title = colNames[j],
          superTitle = colVar,
          type = 'integer'
        )
      }
      
      # Add row total column
      table$addColumn(
        name = 'rowtotal',
        title = 'Total',
        type = 'integer'
      )
      
      # Populate rows
      for (i in 1:nRows) {
        rowValues <- list(rowname = rowNames[i])
        for (j in 1:nCols) {
          rowValues[[paste0("col", j)]] <- marginalTable[i, j]
        }
        rowValues[['rowtotal']] <- sum(marginalTable[i,])
        table$addRow(rowKey = i, values = rowValues)
      }
      
      # Add column totals row
      totalRowValues <- list(rowname = 'Total')
      for (j in 1:nCols) {
        totalRowValues[[paste0("col", j)]] <- sum(marginalTable[,j])
      }
      totalRowValues[['rowtotal']] <- sum(marginalTable)
      table$addRow(rowKey = 'total', values = totalRowValues)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Stratum-specific results table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateStratumResultsTable = function(vResults, marginalVResult, chiSqResults, 
                                             marginalChiSq, strataNames) {
      
      table <- self$results$stratumResultsTable
      K <- length(vResults)
      
      # Add rows for each partial table
      for (k in 1:K) {
        table$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          chisq = chiSqResults[[k]]$statistic,
          df = chiSqResults[[k]]$df,
          pvalue = chiSqResults[[k]]$pvalue,
          v = vResults[[k]]$v,
          vmax = vResults[[k]]$vmax,
          vcorr = vResults[[k]]$vcorr,
          ciLower = vResults[[k]]$ci_lower,
          ciUpper = vResults[[k]]$ci_upper
        ))
      }
      
      # Add marginal table row
      table$addRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        chisq = as.numeric(marginalChiSq$statistic),
        df = as.integer(marginalChiSq$parameter),
        pvalue = marginalChiSq$p.value,
        v = marginalVResult$v,
        vmax = marginalVResult$vmax,
        vcorr = marginalVResult$vcorr,
        ciLower = marginalVResult$ci_lower,
        ciUpper = marginalVResult$ci_upper
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: CMH test table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateCMHTable = function(cmhTest) {
      
      table <- self$results$cmhTestTable
      
      table$addRow(rowKey = 1, values = list(
        test = "Generalised CMH",
        statistic = as.numeric(cmhTest$statistic),
        df = as.integer(cmhTest$parameter),
        pvalue = cmhTest$p.value
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Homogeneity test table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateHomogeneityTable = function(loglinResult) {
      
      table <- self$results$homogeneityTable
      
      table$addRow(rowKey = 1, values = list(
        test = "Log-Linear Likelihood Ratio",
        statistic = loglinResult$statistic,
        df = loglinResult$df,
        pvalue = loglinResult$pvalue
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Summary measure table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateSummaryMeasureTable = function(weighted_vcorr, weightedCI) {
      
      table <- self$results$summaryMeasureTable
      
      table$addRow(rowKey = 1, values = list(
        measure = "Weighted Average V corrected",
        estimate = weighted_vcorr,
        ciLower = weightedCI$ci_lower,
        ciUpper = weightedCI$ci_upper
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Compute adjusted standardised residuals for a single table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .computeAdjustedStandardisedResiduals = function(tbl) {
      
      n <- sum(tbl)
      row_totals <- rowSums(tbl)
      col_totals <- colSums(tbl)
      expected <- outer(row_totals, col_totals) / n
      
      row_props <- row_totals / n
      col_props <- col_totals / n
      
      I <- nrow(tbl)
      J <- ncol(tbl)
      
      adjstdres <- matrix(NA, nrow = I, ncol = J)
      
      for (i in 1:I) {
        for (j in 1:J) {
          if (expected[i, j] > 0) {
            adjustment <- sqrt((1 - row_props[i]) * (1 - col_props[j]))
            if (adjustment > 0) {
              adjstdres[i, j] <- (tbl[i, j] - expected[i, j]) / 
                (sqrt(expected[i, j]) * adjustment)
            } else {
              adjstdres[i, j] <- 0
            }
          } else {
            adjstdres[i, j] <- 0
          }
        }
      }
      
      dimnames(adjstdres) <- dimnames(tbl)
      return(adjstdres)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Residuals section (header, tables, note)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateResidualsSection = function(listOfTables, strataNames, rowVar, colVar) {
      
      K <- length(listOfTables)
      
      # Add section header
      self$results$residualsHeader$setContent(
        "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Adjusted Standardised Residuals</h2>"
      )
      
      residualsGroup <- self$results$residualsGroup
      
      for (k in 1:K) {
        
        # Compute ASR for this stratum
        tbl <- listOfTables[[k]]
        asr <- private$.computeAdjustedStandardisedResiduals(tbl)
        
        rowNames <- rownames(asr)
        colNames <- colnames(asr)
        nRows <- nrow(asr)
        nCols <- ncol(asr)
        
        # Add table to array
        residualsGroup$addItem(key = k)
        table <- residualsGroup$get(key = k)
        
        # Set title
        table$setTitle(paste0("Stratum: ", strataNames[k]))
        
        # Add row name column
        table$addColumn(
          name = 'rowname',
          title = rowVar,
          type = 'text'
        )
        
        # Add data columns
        for (j in 1:nCols) {
          table$addColumn(
            name = paste0("col", j),
            title = colNames[j],
            superTitle = colVar,
            type = 'text'
          )
        }
        
        # Populate rows with colour-coded values
        for (i in 1:nRows) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:nCols) {
            value <- asr[i, j]
            formatted_value <- sprintf("%.3f", value)
            
            # Colour-code: red for significant positive, blue for significant negative
            if (abs(value) > 1.96) {
              if (value > 0) {
                formatted_value <- paste0(
                  "<span style='display: block; text-align: center; color: red;'>",
                  formatted_value, "</span>"
                )
              } else {
                formatted_value <- paste0(
                  "<span style='display: block; text-align: center; color: blue;'>",
                  formatted_value, "</span>"
                )
              }
            } else {
              formatted_value <- paste0(
                "<span style='display: block; text-align: center;'>",
                formatted_value, "</span>"
              )
            }
            
            rowValues[[paste0("col", j)]] <- formatted_value
          }
          table$addRow(rowKey = i, values = rowValues)
        }
      }
      
      # Add explanatory note with cross-reference to Post-Hoc facility
      noteHtml <- paste0(
        "<p style='font-size: 0.9em; color: #555; margin-top: 1em;'>",
        "<em>Note:</em> Adjusted standardised residuals follow an approximate standard normal distribution ",
        "under the null hypothesis of independence. Values exceeding &plusmn;1.96 (highlighted in ",
        "<span style='color: red;'>red</span> for positive, <span style='color: blue;'>blue</span> ",
        "for negative) indicate cells contributing significantly to the chi-squared statistic at ",
        "&alpha; = 0.05. Residuals are computed within each stratum using that stratum's marginal totals.",
        "</p>",
        "<p style='font-size: 0.9em; color: #555; margin-top: 0.5em;'>",
        "<em>For additional cell-level diagnostics</em>  ",
        "and alternative residual measures, filter your data to a single ",
        "stratum and use the <strong>Post-Hoc Analysis</strong> facility of this module.",
        "</p>"
      )
      
      self$results$residualsNote$setContent(noteHtml)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Interpretation
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateInterpretation = function(chiSqResults, marginalChiSq, cmhTest, loglinResult,
                                        vResults, weighted_vcorr, weightedCI, strataNames,
                                        rowVar, colVar, strataVar, nRowLevels, nColLevels) {
      
      
      # Guard against invalid test results
      if (is.null(cmhTest$p.value) || is.na(cmhTest$p.value) ||
          is.null(loglinResult$pvalue) || is.na(loglinResult$pvalue)) {
        self$results$interpretationNote$setContent(
          "<p style='color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px;'>
          <strong>Note:</strong> Interpretation could not be generated because the 
          statistical tests returned invalid results. If using aggregated data, 
          please assign the counts variable.</p>"
        )
        return()
      }
      
      
      K <- length(chiSqResults)
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.95em;'>"
      
      # ─────────────────────────────────────────────────────────────────────────
      # (A) Chi-squared test results
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "<p><strong>(A) Chi-squared test results:</strong></p>")
      html <- paste0(html, "<ul style='margin-left: 0; padding-left: 1.5em;'>")
      
      for (k in 1:K) {
        pval <- chiSqResults[[k]]$pvalue
        if (!is.na(pval) && pval < 0.05) {
          sigText <- "significant"
        } else {
          sigText <- "not significant"
        }
        html <- paste0(html, "<li>Partial Table ", k, " (", strataNames[k], "): ",
                       "\u03C7\u00B2 = ", sprintf("%.2f", chiSqResults[[k]]$statistic),
                       ", df = ", chiSqResults[[k]]$df,
                       ", p = ", sprintf("%.3f", pval),
                       " (", sigText, ")</li>")
      }
      
      margSigText <- if (marginalChiSq$p.value < 0.05) "significant" else "not significant"
      html <- paste0(html, "<li>Marginal Table: ",
                     "\u03C7\u00B2 = ", sprintf("%.2f", as.numeric(marginalChiSq$statistic)),
                     ", df = ", as.integer(marginalChiSq$parameter),
                     ", p = ", sprintf("%.3f", marginalChiSq$p.value),
                     " (", margSigText, ")</li>")
      
      html <- paste0(html, "</ul>")
      
      # ─────────────────────────────────────────────────────────────────────────
      # (B) CMH test
      # ─────────────────────────────────────────────────────────────────────────
      
      cmhSig <- cmhTest$p.value < 0.05
      cmhSigText <- if (cmhSig) "" else "not "
      cmhInterpretation <- if (cmhSig) {
        "conditional dependence (the association between the row and column variables is not zero in at least one stratum)"
      } else {
        "conditional independence (the association between the row and column variables is zero in all strata)"
      }
      
      html <- paste0(html, 
        "<p><strong>(B)</strong> The generalised Cochran-Mantel-Haenszel test is <strong>",
        cmhSigText, "significant</strong> (\u03C7\u00B2 = ", 
        sprintf("%.2f", as.numeric(cmhTest$statistic)),
        ", df = ", as.integer(cmhTest$parameter),
        ", p = ", sprintf("%.3f", cmhTest$p.value),
        "), suggesting ", cmhInterpretation, ".</p>"
      )
      
      # ─────────────────────────────────────────────────────────────────────────
      # (C) Log-linear homogeneity test
      # ─────────────────────────────────────────────────────────────────────────
      
      llSig <- loglinResult$pvalue < 0.05
      llSigText <- if (llSig) "" else "not "
      llInterpretation <- if (llSig) "heterogeneity" else "homogeneity"
      
      html <- paste0(html,
        "<p><strong>(C)</strong> The log-linear likelihood ratio test for homogeneity of association is <strong>",
        llSigText, "significant</strong> (G\u00B2 = ",
        sprintf("%.2f", loglinResult$statistic),
        ", df = ", loglinResult$df,
        ", p = ", sprintf("%.3f", loglinResult$pvalue),
        "), indicating ", llInterpretation, " of the association pattern across strata.</p>"
      )
      
      # ─────────────────────────────────────────────────────────────────────────
      # (D) Association measures summary
      # ─────────────────────────────────────────────────────────────────────────
      
      vcorr_values <- sapply(vResults, function(x) x$vcorr)
      vcorr_range <- max(vcorr_values) - min(vcorr_values)
      
      html <- paste0(html,
        "<p><strong>(D) Association strength:</strong> ",
        "Cram\u00E9r's V corrected ranges from ", sprintf("%.3f", min(vcorr_values)),
        " to ", sprintf("%.3f", max(vcorr_values)),
        " across strata (range = ", sprintf("%.3f", vcorr_range), "). ",
        "The weighted average V<sub>corrected</sub> is ", sprintf("%.3f", weighted_vcorr),
        " (95% CI: ", sprintf("%.3f", weightedCI$ci_lower), 
        "\u2013", sprintf("%.3f", weightedCI$ci_upper), ").</p>"
      )
      
      # ─────────────────────────────────────────────────────────────────────────
      # (E) Overall interpretation
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "<p><strong>(E) Overall interpretation:</strong> ")
      
      if (llSig) {
        # Heterogeneity exists - interaction effect
        if (cmhSig) {
          html <- paste0(html,
            "The CMH test indicates a significant conditional association between '",
            rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ",
            "However, significant heterogeneity of association across strata has been detected, ",
            "indicating an <strong>interaction effect</strong>: '", strataVar,
            "' modifies the <strong>strength</strong> of the association between '", rowVar, 
            "' and '", colVar, "'. ",
            "Because the association varies in magnitude across strata, ",
            "stratum-specific V<sub>corrected</sub> values should be reported rather than a single weighted average."
          )
        } else {
          html <- paste0(html,
            "The CMH test does not indicate a significant conditional association between '",
            rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ",
            "However, significant heterogeneity has been detected, suggesting that ",
            "associations may exist in some strata but cancel out overall. ",
            "Stratum-specific chi-squared tests should be evaluated to assess within-stratum associations."
          )
        }
      } else {
        # Homogeneity exists
        if (cmhSig) {
          # Significant CMH with homogeneity: consistent conditional association
          html <- paste0(html,
                         "The CMH test indicates significant conditional dependence between '",
                         rowVar, "' and '", colVar, "' after controlling for '", strataVar, "': ",
                         "the association between the row and column variables is not zero in at least one stratum. ",
                         "Given the homogeneity of association across strata, '", strataVar,
                         "' does not significantly modify this association. ",
                         "The conditional association between '", rowVar, "' and '", colVar,
                         "' is consistent across levels of '", strataVar,
                         "', and can be summarised using the weighted average V<sub>corrected</sub> (",
                         sprintf("%.3f", weighted_vcorr), "; 95% CI: ", 
                         sprintf("%.3f", weightedCI$ci_lower), "\u2013", sprintf("%.3f", weightedCI$ci_upper), ")."
          )
        } else {
          # Non-significant CMH with homogeneity: conditional independence
          html <- paste0(html,
                         "The CMH test does not indicate a significant conditional association between '",
                         rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ",
                         "This suggests <strong>conditional independence</strong>: any marginal association, if present, ",
                         "vanishes when the data are stratified by '", strataVar, "'."
          )
        }
      }
      
      html <- paste0(html, "</p>")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Cautionary note
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html,
        "<p style='font-size: 0.9em; color: #666; margin-top: 1em;'><em>",
        "Note: The interpretation guidelines provided are suggested based on the statistical tests' outcomes ",
        "and should be further evaluated within the context of your study. For tables larger than 2\u00D72, ",
        "Cram\u00E9r's V corrected is used as the association measure. The correction adjusts for the maximum ",
        "possible chi-squared given the marginal totals, making values comparable across tables with different structures. ",
        "Confidence intervals are computed via bootstrap (",
        self$options$nBootstrap, " replicates).",
        "</em></p>"
      )
      
      html <- paste0(html, "</div>")
      
      self$results$interpretationNote$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Diagnostic Summary
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateDiagnosticSummary = function(cmhTest, loglinResult, rowVar, colVar, strataVar) {
      
      # Guard against invalid test results
      if (is.null(cmhTest$p.value) || is.na(cmhTest$p.value) ||
          is.null(loglinResult$pvalue) || is.na(loglinResult$pvalue)) {
        return()
      }
      
      # Determine significance
      cmhSig <- cmhTest$p.value < 0.05
      loglinSig <- loglinResult$pvalue < 0.05
      
      # Determine scenario (must match decision tree exactly)
      if (cmhSig && !loglinSig) {
        # Scenario 1: CMH significant, homogeneity holds
        scenario <- "Replication (Homogeneous Association)"
        explanation <- paste0(
          "<p><strong>Replication</strong> (homogeneous association) occurs when the association between two variables ",
          "remains consistent in strength across all levels of a third variable.</p>",
          "<p>In this analysis:</p>",
          "<ul>",
          "<li>The <strong>generalised CMH test</strong> is significant (\u03C7\u00B2 = ",
          sprintf("%.2f", as.numeric(cmhTest$statistic)), ", p = ", sprintf("%.3f", cmhTest$p.value),
          "), indicating that '", rowVar, "' and '", colVar, 
          "' are <em>not</em> conditionally independent given '", strataVar, "'.</li>",
          "<li>The <strong>log-linear homogeneity test</strong> is not significant (G\u00B2 = ",
          sprintf("%.2f", loglinResult$statistic), ", p = ", sprintf("%.3f", loglinResult$pvalue),
          "), indicating that the strength of association is consistent across strata.</li>",
          "</ul>",
          "<p><strong>Conclusion:</strong> A conditional association exists between '", rowVar,
          "' and '", colVar, "' given '", strataVar, "', and '", strataVar, 
          "' is neither a confounder nor an effect modifier. The weighted average V<sub>corrected</sub> provides a valid summary of this conditional association.</p>"
        )
        
      } else if (cmhSig && loglinSig) {
        # Scenario 2: CMH significant, heterogeneity present
        scenario <- "Heterogeneous Association (Effect Modification)"
        explanation <- paste0(
          "<p><strong>Heterogeneous association</strong> (effect modification) indicates <em>conditional dependence with interaction</em>: ",
          "a significant conditional association exists between the row and column variables after controlling for the stratifying variable, ",
          "but the strength of this association varies across strata.</p>",
          "<p>In this analysis:</p>",
          "<ul>",
          "<li>The <strong>generalised CMH test</strong> is significant (\u03C7\u00B2 = ",
          sprintf("%.2f", as.numeric(cmhTest$statistic)), ", p = ", sprintf("%.3f", cmhTest$p.value),
          "), indicating that '", rowVar, "' and '", colVar, 
          "' are <em>not</em> conditionally independent given '", strataVar, "'.</li>",
          "<li>The <strong>log-linear homogeneity test</strong> is significant (G\u00B2 = ",
          sprintf("%.2f", loglinResult$statistic), ", p = ", sprintf("%.3f", loglinResult$pvalue),
          "), indicating that the strength of association varies across strata.</li>",
          "</ul>",
          "<p><strong>Conclusion:</strong> A conditional association exists between '", rowVar,
          "' and '", colVar, "' given '", strataVar, "', but '", strataVar, 
          "' acts as an <em>effect modifier</em>. A single weighted average is not meaningful; stratum-specific V<sub>corrected</sub> values should be reported.</p>"
        )
        
      } else if (!cmhSig && !loglinSig) {
        # Scenario 3: CMH not significant, homogeneity holds
        scenario <- "Conditional Independence"
        explanation <- paste0(
          "<p><strong>Conditional independence</strong> indicates <em>no conditional association</em> between the row and column variables ",
          "after controlling for the stratifying variable, with consistently near-zero effects across all strata.</p>",
          "<p>In this analysis:</p>",
          "<ul>",
          "<li>The <strong>generalised CMH test</strong> is not significant (\u03C7\u00B2 = ",
          sprintf("%.2f", as.numeric(cmhTest$statistic)), ", p = ", sprintf("%.3f", cmhTest$p.value),
          "), indicating that '", rowVar, "' and '", colVar, 
          "' are conditionally independent given '", strataVar, "'.</li>",
          "<li>The <strong>log-linear homogeneity test</strong> is not significant (G\u00B2 = ",
          sprintf("%.2f", loglinResult$statistic), ", p = ", sprintf("%.3f", loglinResult$pvalue),
          "), indicating that this absence of association is consistent across strata.</li>",
          "</ul>",
          "<p><strong>Conclusion:</strong> No conditional association exists between '", rowVar,
          "' and '", colVar, "' given '", strataVar, 
          "'. If a marginal association was observed, '", strataVar, "' may have acted as a confounder.</p>"
        )
        
      } else {
        # Scenario 4: CMH not significant, heterogeneity present
        scenario <- "Opposing Associations (Cancellation)"
        explanation <- paste0(
          "<p><strong>Opposing associations</strong> indicate a complex pattern: the overall CMH test suggests <em>conditional independence</em>, ",
          "yet the significant heterogeneity test reveals that stratum-specific conditional associations exist but differ in ways that cancel out when aggregated.</p>",
          "<p>In this analysis:</p>",
          "<ul>",
          "<li>The <strong>generalised CMH test</strong> is not significant (\u03C7\u00B2 = ",
          sprintf("%.2f", as.numeric(cmhTest$statistic)), ", p = ", sprintf("%.3f", cmhTest$p.value),
          "), suggesting no overall conditional association between '", rowVar, "' and '", colVar, "'.</li>",
          "<li>The <strong>log-linear homogeneity test</strong> is significant (G\u00B2 = ",
          sprintf("%.2f", loglinResult$statistic), ", p = ", sprintf("%.3f", loglinResult$pvalue),
          "), indicating that the association patterns differ meaningfully across strata.</li>",
          "</ul>",
          "<p><strong>Conclusion:</strong> Although the CMH test does not detect an overall conditional association, ",
          "the heterogeneity indicates that conditional associations likely exist within individual strata but cancel out in aggregate. ",
          "Alternatively, this may reflect insufficient statistical power. ",
          "Stratum-specific chi-squared tests and V<sub>corrected</sub> values should be examined.</p>"
        )
      }
      
      # Build final HTML
      html <- paste0(
        "<div style='background-color: #f0f7fb; border-left: 4px solid #2874A6; ",
        "padding: 15px; margin: 10px 0; font-family: sans-serif;'>",
        "<h4 style='color: #2874A6; margin-top: 0;'>Diagnostic Summary: ", scenario, "</h4>",
        explanation,
        "</div>"
      )
      
      self$results$diagnosticSummary$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Method information
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMethodInfo = function() {
      
      if (!self$options$showMethodInfo) {
        return()
      }
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
      
      # ─────────────────────────────────────────────────────────────────────────
      # Overview
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 0.5em;'>Stratified Analysis Overview</h3>
        <p>Stratified analysis examines the relationship between two categorical variables 
        (row variable and column variable) whilst controlling for a third variable (the stratifying variable). 
        By creating separate contingency tables for each level of the stratifying variable, this approach 
        allows researchers to assess whether the association between the row and column variables is 
        <strong>confounded</strong> or <strong>modified</strong> by the stratifying variable.</p>
        
        <p>The analysis addresses two fundamental questions:</p>
        <ol>
          <li><strong>Conditional independence:</strong> Is there an association between the two categorical variables 
          after controlling for the stratifying variable?</li>
          <li><strong>Homogeneity:</strong> Is the strength of association consistent 
          across strata, or does it vary (effect modification/interaction)?</li>
        </ol>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # V corrected
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Cram\u00E9r's V Corrected</h3>
        <p>For contingency tables of any size, Cram\u00E9r's V is a widely used measure of association. 
        However, standard V has a maximum value that depends on the marginal distributions of the table, 
        which can make comparisons across tables with different structures misleading.</p>
        
        <p>The corrected version, V<sub>corrected</sub>, adjusts for this by dividing the observed 
        chi-squared by the maximum possible chi-squared given the marginal totals:</p>
        
        <p style='text-align: center;'>V<sub>corr</sub> = \u221A(\u03C7\u00B2 / \u03C7\u00B2<sub>max</sub>)</p>
        
        <p>This correction ensures that V<sub>corrected</sub> can reach 1.0 for any set of marginals, 
        making values comparable across tables with different structures. The ratio \u03C7\u00B2/\u03C7\u00B2<sub>max</sub> 
        is discussed by Berry et al. 2018, which refers to it as <em>R</em>.</p>
        
        <p>Confidence intervals are computed via bootstrap resampling of observations, 
        recalculating both \u03C7\u00B2 and \u03C7\u00B2<sub>max</sub> for each replicate since 
        the marginals change with resampling. The resulting percentile-based confidence intervals 
        are not necessarily symmetric around the point estimate; this asymmetry reflects the 
        shape of the bootstrap distribution rather than a computational error.</p>
        
        <p><strong>Important limitation:</strong> Because V<sub>corrected</sub> is a strictly 
        non-negative statistic (bounded between 0 and 1), the percentile bootstrap confidence 
        interval cannot, by definition, include zero. This has two consequences for interpretation:</p>
        <ul>
          <li><strong>For hypothesis testing:</strong> The confidence interval should <em>not</em> 
          be used to test whether the association is significantly different from zero. 
          For this purpose, use the chi-squared test (for individual strata) or the CMH test 
          (for the conditional association), both of which are reported in the output.</li>
          <li><strong>For the forest plot:</strong> Unlike forest plots for odds ratios or 
          mean differences (where intervals crossing 1 or 0, respectively, indicate non-significance), 
          a V<sub>corrected</sub> interval that does not touch zero does <em>not</em> imply 
          statistical significance. The forest plot should be interpreted as a visual comparison 
          of association <em>strength</em> across strata, not as a significance display. 
          Overlapping intervals suggest similar association strength; non-overlapping intervals 
          suggest differing strength\u2014but statistical inference about the presence or absence 
          of association should rely on the formal tests. (See the <em>Log-Linear Test for 
          Homogeneity of Association</em> section below for how the forest plot and the 
          homogeneity test complement each other.)</li>
        </ul>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Generalised CMH Test
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Generalised Cochran-Mantel-Haenszel (CMH) Test</h3>
        <p>The CMH test generalises to R\u00D7C\u00D7K tables, testing conditional independence 
        across all strata. The null hypothesis states that there is no association between 
        the row and column variables in any stratum.</p>
        
        <p>For R\u00D7C tables, the test statistic follows a chi-squared distribution with 
        (R\u22121)(C\u22121) degrees of freedom. A significant result indicates that, 
        after controlling for the stratifying variable, there is evidence of association 
        between the row and column variables in at least one stratum.</p>
        
        <p>Unlike the 2\u00D72 case, the generalised CMH test does not provide a common 
        measure of effect size (such as a common odds ratio). Instead, summary association 
        measures like the weighted average V<sub>corrected</sub> are used when homogeneity holds.</p>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Log-linear homogeneity test
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Log-Linear Test for Homogeneity of Association</h3>
        <p>For R\u00D7C tables, homogeneity of association is assessed using log-linear models. 
        The test compares two nested models:</p>
        <ul>
          <li><strong>Homogeneous model:</strong> Row\u00D7Col + Row\u00D7Stratum + Col\u00D7Stratum 
          (association pattern is the same across strata)</li>
          <li><strong>Heterogeneous model:</strong> Row\u00D7Col\u00D7Stratum 
          (association pattern varies across strata; saturated model)</li>
        </ul>
        
        <p>The likelihood ratio test statistic (G\u00B2) compares the fit of these models. 
        A significant result indicates that the Row\u00D7Column association pattern 
        varies across strata, suggesting <strong>effect modification</strong> (interaction) 
        by the stratifying variable.</p>
        
        <p><strong>Important clarification:</strong> The log-linear test assesses whether the 
        <em>pattern</em> of association (the structure of departures from independence) is the same 
        across strata. This is distinct from comparing the <em>strength</em> of association 
        (V<sub>corrected</sub> values) across strata. Two strata may have identical V<sub>corrected</sub> 
        values but different association patterns (e.g., concentration along the diagonal in one stratum 
        versus concentration in off-diagonal cells in another). Conversely, strata may show different 
        V<sub>corrected</sub> values whilst sharing the same underlying pattern structure. The log-linear 
        test is the formal inferential tool for homogeneity; V<sub>corrected</sub> serves as a descriptive 
        measure of association strength within each stratum.</p>
        
        <p>In most cases, homogeneity of pattern (as indicated by a non-significant log-linear test) 
        will coincide with similar V<sub>corrected</sub> values and overlapping confidence intervals 
        across strata. However, in rare cases, the log-linear test may detect heterogeneity even when 
        V<sub>corrected</sub> values appear similar\u2014this occurs when the <em>structure</em> of 
        the association differs across strata despite comparable overall strength. For this reason, 
        the log-linear test and the forest plot of V<sub>corrected</sub> are complementary: the former 
        provides formal inference about pattern homogeneity, whilst the latter offers a visual summary 
        of association strength across strata.</p>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Weighted average
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Weighted Average V<sub>corrected</sub></h3>
        <p>Following the approach described by Blalock (1979) and Reynolds (1977) for 
        combining association measures across strata, a weighted average of V<sub>corrected</sub> 
        is computed:</p>
        
        <p style='text-align: center;'>V<sub>weighted</sub> = \u03A3(n<sub>k</sub> \u00D7 V<sub>corr,k</sub>) / \u03A3(n<sub>k</sub>)</p>
        
        <p>where n<sub>k</sub> is the total count in stratum k. This provides an overall 
        summary measure of association strength when the association patterns are homogeneous 
        across strata. When heterogeneity is detected, the weighted average may not be an 
        appropriate summary, and stratum-specific values should be examined instead.</p>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Interpretational Scenarios
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Interpretational Scenarios</h3>
        <p>The joint pattern of CMH and homogeneity test results leads to four interpretational scenarios:</p>
        
        <ol>
          <li><strong>Significant CMH with homogeneity:</strong> Indicates conditional dependence with 
          consistent association across strata. The weighted average V<sub>corrected</sub> is a reliable summary.</li>
          
          <li><strong>Significant CMH with heterogeneity:</strong> Suggests conditional dependence but 
          varying strength of association across strata (interaction). Stratum-specific V<sub>corrected</sub> 
          values should be reported rather than the weighted average.</li>
          
         <li><strong>Non-significant CMH with homogeneity:</strong> Implies conditional independence: 
          no association is detected within strata. Any marginal association (if originally present) vanishes 
          when the data are stratified.</li>
          
          <li><strong>Non-significant CMH with heterogeneity:</strong> The most complex scenario. 
          The overall CMH test does not detect association, but the association pattern varies across strata. 
          This may indicate that associations exist in some strata but are absent or weaker in others, 
          or that different patterns across strata obscure an overall effect. Stratum-specific chi-squared 
          tests should be evaluated to understand the pattern.</li>
        </ol>
      ")
      
      html <- paste0(html,
                     "<h3 style='color: #2874A6; margin-top: 1.5em;'>Methodological Framework</h3>",
                     "<p>The approach implemented in this facility extends the framework described by ",
                     "Ott et al. (1992) for controlling for a third variable when analysing nominal variables ",
                     "with three or more categories. While Ott et al. recommend measures such as Goodman and ",
                     "Kruskal's lambda for larger tables (since Yule's Q is limited to 2\u00D72 tables), this facility ",
                     "employs Cram\u00E9r's V corrected as the association measure. The pooled chi-squared approach described by Ott et al. ",
                     "for testing conditional independence is equivalent to the generalised Cochran-Mantel-Haenszel ",
                     "test implemented here.</p>")
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: References
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateReferences = function() {
      
      references_html <- paste0(
        "<div style='font-size: 0.85em; color: #444; margin: 15px 0; line-height: 1.5;'>",
        "<h3 style='color: #2874A6; margin-top: 0.5em; margin-bottom: 0.5em;'>References</h3>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Agresti, A. (2013). <em>Categorical Data Analysis</em> (3rd ed.). Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Azen, R., & Walker, C. M. (2021). <em>Categorical Data Analysis for the Behavioral and Social Sciences</em> (2nd ed.). Routledge.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Berry, K. J., Johnston, J. E., & Mielke, P. W., Jr. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Blalock, H. M. (1979). <em>Social Statistics</em> (Rev. 2nd ed.). McGraw-Hill.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Ott, R. L., Larson, R., Rexroat, C., & Mendenhall, W. (1992). <em>Statistics: A Tool for the Social Sciences</em> (5th ed.). PWS-KENT Publishing Company.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.</p>",
        "</div>"
      )
      
      self$results$legendNote$setContent(references_html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Data Preparation: Forest Plot
    # ═══════════════════════════════════════════════════════════════════════════
    
    .prepareForestData = function(vResults, marginalVResult, weighted_vcorr, 
                                   weightedCI, strataNames, K) {
      
      # Extract V corrected estimates and CIs for each stratum
      vcorr_vec <- sapply(vResults, function(x) x$vcorr)
      ci_lower_vec <- sapply(vResults, function(x) x$ci_lower)
      ci_upper_vec <- sapply(vResults, function(x) x$ci_upper)
      
      # Build data frame
      plotData <- data.frame(
        stratum = strataNames,
        estimate = vcorr_vec,
        ciLower = ci_lower_vec,
        ciUpper = ci_upper_vec,
        type = "Stratum-specific",
        stringsAsFactors = FALSE
      )
      
      # Add marginal V corrected
      marginal_row <- data.frame(
        stratum = "Marginal (collapsed)",
        estimate = marginalVResult$vcorr,
        ciLower = marginalVResult$ci_lower,
        ciUpper = marginalVResult$ci_upper,
        type = "Marginal",
        stringsAsFactors = FALSE
      )
      
      # Add weighted average
      weighted_row <- data.frame(
        stratum = "Weighted Average",
        estimate = weighted_vcorr,
        ciLower = weightedCI$ci_lower,
        ciUpper = weightedCI$ci_upper,
        type = "Weighted",
        stringsAsFactors = FALSE
      )
      
      plotData <- rbind(plotData, marginal_row, weighted_row)
      
      # Reverse order so first stratum appears at top
      plotData$stratum <- factor(plotData$stratum, levels = rev(plotData$stratum))
      
      return(plotData)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Data Preparation: Diagnostic Tree
    # ═══════════════════════════════════════════════════════════════════════════
    
    .prepareDiagnosticTreeData = function(cmh_p, loglin_p) {
      
      
      # Guard against NULL, empty, or NA values
      if (is.null(cmh_p) || is.null(loglin_p) ||
          length(cmh_p) == 0 || length(loglin_p) == 0 ||
          is.na(cmh_p) || is.na(loglin_p)) {
        return(NULL)
      }
      
      cmh_sig <- cmh_p < 0.05
      loglin_sig <- loglin_p < 0.05
      
      # Determine scenario (matching 2×2×K interpretation logic)
      if (cmh_sig && !loglin_sig) {
        scenario <- "Conditional dependence with homogeneity"
        scenario_num <- 1
        scenario_description <- "Homogeneous association:\nConsistent relationship across strata"
      } else if (cmh_sig && loglin_sig) {
        scenario <- "Conditional dependence with heterogeneity"
        scenario_num <- 2
        scenario_description <- "Heterogeneous association:\nEffect modification (interaction) present"
      } else if (!cmh_sig && !loglin_sig) {
        scenario <- "Conditional independence with homogeneity"
        scenario_num <- 3
        scenario_description <- "Conditional independence:\nAssociation (if any) vanishes when stratified"
      } else {
        scenario <- "Conditional independence with heterogeneity"
        scenario_num <- 4
        scenario_description <- "Opposing associations:\nEffects cancel across strata"
      }
      
      return(list(
        cmh_sig = cmh_sig,
        loglin_sig = loglin_sig,
        cmh_p = cmh_p,
        loglin_p = loglin_p,
        scenario = scenario,
        scenario_num = scenario_num,
        scenario_description = scenario_description
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Data Preparation: Trajectory Plot
    # ═══════════════════════════════════════════════════════════════════════════
    
    .prepareTrajectoryData = function(vResults, strataNames) {
      
      vcorr_vec <- sapply(vResults, function(x) x$vcorr)
      ci_lower_vec <- sapply(vResults, function(x) x$ci_lower)
      ci_upper_vec <- sapply(vResults, function(x) x$ci_upper)
      
      plotData <- data.frame(
        stratum = strataNames,
        position = seq_along(strataNames),
        vcorr = vcorr_vec,
        ciLower = ci_lower_vec,
        ciUpper = ci_upper_vec,
        stringsAsFactors = FALSE
      )
      
      return(plotData)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Forest Plot Render Function
    # ═══════════════════════════════════════════════════════════════════════════
    
    .forestPlot = function(image, ggtheme, theme, ...) {
      
      plotData <- image$state
      
      if (is.null(plotData)) {
        return(FALSE)
      }
      
      n_points <- nrow(plotData)
      
      # Colour scheme for V corrected
      # (CI-based significance colouring is not meaningful for non-negative statistics)
      col_estimate <- "#D4AC0D"
      col_ci <- "#2874A6"
      
      point_cols <- rep(col_estimate, nrow(plotData))
      ci_cols <- rep(col_ci, nrow(plotData))
      
      # Define point shapes: circle=21 for strata, diamond=23 for weighted, square=22 for marginal
      point_pch <- ifelse(plotData$type == "Weighted", 23,
                          ifelse(plotData$type == "Marginal", 22, 21))
      
      # Define point sizes
      point_cex <- ifelse(plotData$type == "Weighted", 1.6,
                          ifelse(plotData$type == "Marginal", 1.4, 1.2))
      
      # V corrected is bounded 0-1, so always use full range
      x_min <- 0
      x_max <- 1
      
      # Calculate margins for labels
      max_label_chars <- max(nchar(as.character(plotData$stratum)))
      left_margin <- max(10, max_label_chars * 0.5)
      
      # Set up plot
      par(
        mar = c(4, left_margin, 3, 2) + 0.1,
        family = "sans"
      )
      
      # Y positions (reversed so first stratum appears at top)
      y_pos <- rev(seq_len(n_points))
      
      # Create empty plot
      plot(
        x = NULL, y = NULL,
        xlim = c(x_min, x_max),
        ylim = c(0.5, n_points + 0.5),
        xlab = "",
        ylab = "",
        axes = FALSE,
        frame.plot = FALSE
      )
      
      # Add vertical reference line at 0 (no association)
      abline(v = 0, lty = 2, col = "#34495E", lwd = 1.5)
      
      # Add light horizontal grid lines for readability
      abline(h = seq_len(n_points), col = "#E8E8E8", lwd = 0.5)
      
      # Draw confidence interval whiskers
      segments(
        x0 = plotData$ciLower,
        x1 = plotData$ciUpper,
        y0 = y_pos,
        y1 = y_pos,
        col = ci_cols,
        lwd = 1.5
      )
      
      # Draw whisker caps
      cap_height <- 0.12
      segments(
        x0 = plotData$ciLower,
        x1 = plotData$ciLower,
        y0 = y_pos - cap_height,
        y1 = y_pos + cap_height,
        col = ci_cols,
        lwd = 1.5
      )
      segments(
        x0 = plotData$ciUpper,
        x1 = plotData$ciUpper,
        y0 = y_pos - cap_height,
        y1 = y_pos + cap_height,
        col = ci_cols,
        lwd = 1.5
      )
      
      # Draw point estimates
      points(
        x = plotData$estimate,
        y = y_pos,
        pch = point_pch,
        col = ci_cols,
        bg = col_estimate,
        cex = point_cex
      )
      
      # Add x-axis
      axis_breaks <- seq(0, 1, by = 0.2)
      axis_breaks <- axis_breaks[axis_breaks <= x_max]
      axis(1, at = axis_breaks, col = "#808080", col.axis = "#505050")
      
      # Add y-axis with stratum labels
      axis(2, at = y_pos, labels = plotData$stratum, las = 1, tick = FALSE,
           col.axis = "#505050", cex.axis = 0.9)
      
      # Add axis label (V corrected with subscript)
      mtext(expression(V[corrected]), side = 1, line = 2.5, col = "#505050")
      
      # Add title
      mtext("V Corrected Across Strata", side = 3, line = 1,
            font = 2, cex = 1.1, col = "#303030")
      
      # Add interpretive note instead of significance legend
      legend(
        "top",
        legend = c(
          "Note: V corrected is non-negative; CIs cannot include zero by definition.",
          "Use chi-squared tests for significance. Overlapping CIs suggest similar strength."
        ),
        bty = "n",
        cex = 0.80,
        text.col = "#707070",
        text.font = 3  
      )
      
      TRUE
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Diagnostic Tree Render Function
    # ═══════════════════════════════════════════════════════════════════════════
    
    .diagnosticTree = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      treeData <- image$state
      
      if (is.null(treeData)) {
        return(FALSE)
      }
      
      # Build flow-chart style decision tree
      # Define node positions
      nodes <- data.frame(
        x = c(2, 1, 3, 0.5, 1.5, 2.5, 3.5),  # Horizontal positions
        y = c(4, 2, 2, 0, 0, 0, 0),  # Vertical positions (4=top, 0=bottom)
        label = c(
          "CMH Test",
          "Homogeneity Test\n(Log-Linear)",
          "Homogeneity Test\n(Log-Linear)",
          "Conditional independence:\nAssociation (if any) vanishes\nwhen stratified",
          "Opposing associations:\nEffects cancel across strata",
          "Homogeneous association:\nConsistent relationship\n(replication) across strata",
          "Heterogeneous association:\nEffect modification\n(interaction) present"
        ),
        node_type = c("decision", "decision", "decision", 
                      "terminal", "terminal", "terminal", "terminal"),
        is_active = c(
          TRUE,  # CMH always evaluated
          !treeData$cmh_sig,  # Left homogeneity (if CMH non-sig)
          treeData$cmh_sig,   # Right homogeneity (if CMH sig)
          !treeData$cmh_sig && !treeData$loglin_sig,  # Scenario 3
          !treeData$cmh_sig && treeData$loglin_sig,   # Scenario 4
          treeData$cmh_sig && !treeData$loglin_sig,   # Scenario 1
          treeData$cmh_sig && treeData$loglin_sig     # Scenario 2
        ),
        stringsAsFactors = FALSE
      )
      
      # Define edges (connections between nodes)
      edges <- data.frame(
        x = c(2, 2, 1, 1, 3, 3),
        y = c(4, 4, 2, 2, 2, 2),
        xend = c(1, 3, 0.5, 1.5, 2.5, 3.5),
        yend = c(2, 2, 0, 0, 0, 0),
        label = c(
          "p > .05\n(non-sig)",
          "p < .05\n(sig)",
          "p > .05",
          "p < .05",
          "p > .05",
          "p < .05"
        ),
        is_active = c(
          !treeData$cmh_sig,
          treeData$cmh_sig,
          !treeData$cmh_sig && !treeData$loglin_sig,
          !treeData$cmh_sig && treeData$loglin_sig,
          treeData$cmh_sig && !treeData$loglin_sig,
          treeData$cmh_sig && treeData$loglin_sig
        ),
        stringsAsFactors = FALSE
      )
      
      # Colour coding: active path in blue, inactive greyed out
      nodes$fill_color <- ifelse(nodes$is_active & nodes$node_type == "terminal",
                                 "#2874A6",  # Active terminal: blue
                                 ifelse(nodes$is_active, "#ECF0F1",  # Active decision: light grey
                                        "#BDC3C7"))  # Inactive: medium grey
      
      nodes$border_color <- ifelse(nodes$is_active, "#2C3E50", "#95A5A6")
      nodes$text_color <- ifelse(nodes$is_active & nodes$node_type == "terminal",
                                 "white", "#2C3E50")
      
      edges$line_color <- ifelse(edges$is_active, "#2C3E50", "#D5DBDB")
      edges$line_width <- ifelse(edges$is_active, 1.2, 0.6)
      
      # Create plot
      plot <- ggplot2::ggplot() +
        
        # Draw edges (arrows)
        ggplot2::geom_segment(data = edges,
                              ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                           color = line_color, linewidth = line_width),
                              arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), 
                                                     type = "closed")) +
        
        # Edge labels
        ggplot2::geom_text(data = edges,
                           ggplot2::aes(x = (x + xend) / 2, y = (y + yend) / 2, label = label),
                           size = 2.9, hjust = 0.5, vjust = -0.5, color = "#2C3E50") +
        
        # Draw nodes (rectangles)
        # Terminal nodes get extra width for longer text
        ggplot2::geom_rect(data = nodes,
                           ggplot2::aes(xmin = ifelse(node_type == "terminal", x - 0.47, x - 0.37),
                                        xmax = ifelse(node_type == "terminal", x + 0.47, x + 0.37),
                                        ymin = y - 0.4, ymax = y + 0.4,
                                        fill = fill_color, color = border_color),
                           linewidth = 1) +
        
        # Node labels
        ggplot2::geom_text(data = nodes,
                           ggplot2::aes(x = x, y = y, label = label, color = text_color),
                           size = 3.5, fontface = "bold", lineheight = 0.9) +
        
        # Manual scales
        ggplot2::scale_color_identity() +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_linewidth_identity() +
        
        # Coordinate system and limits
        ggplot2::coord_cartesian(xlim = c(-0.5, 4.5), ylim = c(-0.8, 4.8)) +
        
        # Labels
        ggplot2::labs(
          title = "Diagnostic Decision Path",
          subtitle = paste0("Your data: ", treeData$scenario)
        ) +
        
        # Theme
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold",
                                             margin = ggplot2::margin(b = 5)),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, 
                                                color = "#2874A6", face = "bold",
                                                margin = ggplot2::margin(b = 10))
        )
      
      print(plot)
      return(TRUE)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Trajectory Plot Render Function
    # ═══════════════════════════════════════════════════════════════════════════
    
    .trajectoryPlot = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      plotData <- image$state
      
      if (is.null(plotData) || nrow(plotData) < 2) {
        # Need at least 2 strata for trajectory
        return(FALSE)
      }
      
      # Create trajectory plot
      plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = position, y = vcorr)) +
        
        # Reference line at 0
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", 
                            color = "#34495E", linewidth = 0.5) +
        
        # Confidence ribbon
        ggplot2::geom_ribbon(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
                             fill = "#85C1E9", alpha = 0.3) +
        
        # Trajectory line
        ggplot2::geom_line(color = "#2874A6", linewidth = 1) +
        
        # Points
        ggplot2::geom_point(color = "#2874A6", size = 3, shape = 21, fill = "white") +
        
        # X-axis with stratum labels
        ggplot2::scale_x_continuous(breaks = plotData$position,
                                    labels = plotData$stratum) +
        
        # Y-axis (V corrected is bounded 0-1)
        ggplot2::scale_y_continuous(limits = c(0, min(1, max(plotData$ciUpper) * 1.1))) +
        
        # Labels
        ggplot2::labs(
          x = "Stratum (ordered)",
          y = expression(V[corrected]),
          title = "V Corrected Trajectory Across Ordered Strata"
        ) +
        
        # Apply jamovi theme
        ggtheme +
        
        # Additional customisation
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)
        )
      
      print(plot)
      return(TRUE)
    }
  )
)
