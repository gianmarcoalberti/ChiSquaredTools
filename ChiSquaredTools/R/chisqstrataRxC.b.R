# This file is a generated template, your changes will not be overwritten

#' @export
chisqstrataRxCClass <- R6::R6Class(
  "chisqstrataRxCClass",
  inherit = chisqstrataRxCBase,
  private = list(
    
    
    
    # ═══════════════════════════════════════════════════════════════════════════
    # .init() - Pre-create table structures to prevent flicker
    # ═══════════════════════════════════════════════════════════════════════════
    
    .init = function() {
      
      # Early return if required variables not specified
      if (is.null(self$options$rows) || 
          is.null(self$options$cols) || 
          is.null(self$options$strata)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      strataVar <- self$options$strata
      data <- self$data
      
      # Ensure variables are factors for level extraction
      if (!is.factor(data[[rowVar]])) {
        data[[rowVar]] <- as.factor(data[[rowVar]])
      }
      if (!is.factor(data[[colVar]])) {
        data[[colVar]] <- as.factor(data[[colVar]])
      }
      if (!is.factor(data[[strataVar]])) {
        data[[strataVar]] <- as.factor(data[[strataVar]])
      }
      
      # Get factor levels
      rowLevels <- levels(data[[rowVar]])
      colLevels <- levels(data[[colVar]])
      strataLevels <- levels(data[[strataVar]])
      K <- length(strataLevels)
      nRows <- length(rowLevels)
      nCols <- length(colLevels)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise partial tables (one per stratum)
      # ═══════════════════════════════════════════════════════════════════════════
      
      partialTablesGroup <- self$results$partialTablesGroup
      
      for (k in 1:K) {
        # Add item to array
        partialTablesGroup$addItem(key = k)
        table <- partialTablesGroup$get(key = k)
        
        # Set title
        table$setTitle(paste0(strataVar, " = ", strataLevels[k]))
        
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
            title = colLevels[j],
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
        
        # Add empty rows (data rows + total row)
        for (i in 1:nRows) {
          table$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
        }
        table$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise marginal table
      # ═══════════════════════════════════════════════════════════════════════════
      
      marginalTable <- self$results$marginalTable
      marginalTable$setTitle("Marginal Table (Collapsed)")
      
      # Add row name column
      marginalTable$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text'
      )
      
      # Add data columns
      for (j in 1:nCols) {
        marginalTable$addColumn(
          name = paste0("col", j),
          title = colLevels[j],
          superTitle = colVar,
          type = 'integer'
        )
      }
      
      # Add row total column
      marginalTable$addColumn(
        name = 'rowtotal',
        title = 'Total',
        type = 'integer'
      )
      
      # Add empty rows
      for (i in 1:nRows) {
        marginalTable$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
      }
      marginalTable$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise stratum results table
      # ═══════════════════════════════════════════════════════════════════════════
      
      stratumTable <- self$results$stratumResultsTable
      
      # Add rows for each stratum
      for (k in 1:K) {
        stratumTable$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataLevels[k], ")")
        ))
      }
      # Add marginal row
      stratumTable$addRow(rowKey = 'marginal', values = list(stratum = "Marginal Table"))
      # Pooled chi-squared row (sum of the partial tables)
      stratumTable$addRow(rowKey = 'pooled', values = list(stratum = "Pooled (sum of partial)"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise residuals tables (one per stratum)
      # ═══════════════════════════════════════════════════════════════════════════
      
      residualsGroup <- self$results$residualsGroup
      
      for (k in 1:K) {
        # Add item to array
        residualsGroup$addItem(key = k)
        table <- residualsGroup$get(key = k)
        
        # Set title
        table$setTitle(paste0("Adjusted Standardised Residuals: ", strataLevels[k]))
        
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
            title = colLevels[j],
            superTitle = colVar,
            type = 'number',
            format = 'zto'
          )
        }
        
        # Add empty rows (no total row for residuals)
        for (i in 1:nRows) {
          table$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
        }
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise CMH test table
      # ═══════════════════════════════════════════════════════════════════════════
      
      cmhTable <- self$results$cmhTestTable
      cmhTable$addRow(rowKey = 1, values = list(test = "Generalised CMH"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise homogeneity test table
      # ═══════════════════════════════════════════════════════════════════════════
      
      homogTable <- self$results$homogeneityTable
      homogTable$addRow(rowKey = 1, values = list(test = "Log-Linear Likelihood Ratio"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise summary measure table
      # ═══════════════════════════════════════════════════════════════════════════
      
      summaryTable <- self$results$summaryMeasureTable
      summaryTable$addRow(rowKey = 1, values = list(measure = "Weighted Average V corrected"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise interpretation table
      # ═══════════════════════════════════════════════════════════════════════════
      
      interpTable <- self$results$interpretationTable
      interpTable$addRow(rowKey = 'condIndep', values = list(topic = 'Conditional Independence/Dependence'))
      interpTable$addRow(rowKey = 'homogeneity', values = list(topic = 'Homogeneity/Heterogeneity'))
      interpTable$addRow(rowKey = 'scenario', values = list(topic = 'Interpretation'))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise method info HTML element with empty content
      # ═══════════════════════════════════════════════════════════════════════════
      
      self$results$methodInfo$setContent('')
    },
    
    .run = function() {
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 1. Input validation
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (is.null(self$options$rows) || 
          is.null(self$options$cols) || 
          is.null(self$options$strata)) {
        return()
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 1b. Check if this is a display-only option change
      # ═══════════════════════════════════════════════════════════════════════════
      
      # (A former display-only shortcut was removed: jamovi creates a new
      # analysis object for every run, so a private cache is never reused,
      # and an early return here would have left the tables empty.)
      
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
      # 2b. Validate strata for degenerate tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Check each stratum for validity
      zeroStrata <- character(0)
      degenerateStrata <- character(0)
      
      for (k in 1:K) {
        tbl <- listOfTables[[k]]
        stratumName <- strataNames[k]
        
        # Check for all-zero table (no observations in this stratum)
        if (sum(tbl) == 0) {
          zeroStrata <- c(zeroStrata, stratumName)
          next
        }
        
        # Check for zero row margins (entire row is zero)
        rowSums_k <- rowSums(tbl)
        if (any(rowSums_k == 0)) {
          degenerateStrata <- c(degenerateStrata, stratumName)
          next
        }
        
        # Check for zero column margins (entire column is zero)
        colSums_k <- colSums(tbl)
        if (any(colSums_k == 0)) {
          degenerateStrata <- c(degenerateStrata, stratumName)
        }
      }
      
      # Build informative error message if problems detected
      if (length(zeroStrata) > 0) {
        jmvcore::reject(
          paste0(
            "The following strata contain no observations: ",
            paste(zeroStrata, collapse = ", "), ". ",
            "This typically occurs when a filtering or transformation creates ",
            "empty cross-tabulations. Please check your data or stratifying variable."
          ),
          code = "empty_strata"
        )
      }
      
      if (length(degenerateStrata) > 0) {
        jmvcore::reject(
          paste0(
            "The following strata have degenerate structure (entire row or column is zero): ",
            paste(degenerateStrata, collapse = ", "), ". ",
            "Chi-squared tests and association measures require variation in both ",
            "rows and columns within each stratum."
          ),
          code = "degenerate_strata"
        )
      }
      
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
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 5. Compute V corrected and bootstrap CIs for each stratum
      # ═══════════════════════════════════════════════════════════════════════════
      
      # (bootstrap CIs are always shown in the stratum and summary tables)
      # Fix the seed so bootstrap CIs are reproducible across re-runs
      set.seed(self$options$seed)
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
      
      # Log-linear homogeneity test
      loglinResult <- private$.loglinearHomogeneityTest(array3D)
      
      # Pooled chi-squared test: the sum of the partial-table chi-squared values,
      # which (unlike the generalised CMH test) does not assume that the strata
      # share the same pattern of association, so opposite patterns cannot cancel
      pooledStat <- sum(sapply(chiSqResults, function(x) x$statistic))
      pooledDf <- sum(sapply(chiSqResults, function(x) x$df))
      pooledChiSq <- list(
        statistic = pooledStat, df = pooledDf,
        pvalue = if (is.finite(pooledStat) && pooledDf > 0)
          stats::pchisq(pooledStat, pooledDf, lower.tail = FALSE) else NA_real_
      )
      
      # Pattern reversal, the R x C counterpart of odds ratios pointing in opposite
      # directions: a cell with a significant positive adjusted residual in one
      # stratum and a significant negative one in another
      residList <- lapply(listOfTables, private$.computeAdjustedStandardisedResiduals)
      reversalCells <- character(0)
      rowLv <- rownames(listOfTables[[1]]); colLv <- colnames(listOfTables[[1]])
      for (i in seq_along(rowLv)) for (j in seq_along(colLv)) {
        vals <- sapply(residList, function(r) r[i, j])
        if (any(vals > 1.96, na.rm = TRUE) && any(vals < -1.96, na.rm = TRUE))
          reversalCells <- c(reversalCells, paste0("'", rowLv[i], "' \u00D7 '", colLv[j], "'"))
      }
      
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 7. Populate results tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateStratumResultsTable(vResults, marginalVResult, chiSqResults, 
                                           marginalChiSq, strataNames, pooledChiSq)
      
      # Populate residuals section
      private$.populateResidualsSection(listOfTables, strataNames, rowVar, colVar)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 8. Populate test tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateCMHTable(cmhTest)
      private$.populateHomogeneityTable(loglinResult)
      private$.populateSummaryMeasureTable(weighted_vcorr, weightedCI, loglinResult, cmhTest)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 9. Prepare plot states
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Forest plot state
      forestData <- private$.prepareForestData(vResults, marginalVResult, weighted_vcorr, weightedCI, strataNames, K)
      self$results$forestPlot$setState(forestData)
      
      # Diagnostic tree state
      # (no decision path can be drawn if either test could not be computed)
      if (is.na(cmhTest$p.value) || is.na(loglinResult$pvalue)) {
        self$results$diagnosticTree$setState(NULL)
      } else {
        llSigTree <- loglinResult$pvalue < 0.05
        # The deciding test: CMH without heterogeneity, pooled chi-squared with it
        decideSig <- if (llSigTree) isTRUE(pooledChiSq$pvalue < 0.05) else cmhTest$p.value < 0.05
        diagnosticData <- list(
          loglin_sig = llSigTree,
          decide_sig = decideSig,
          scenario = private$.determineScenario(llSigTree, decideSig)
        )
        self$results$diagnosticTree$setState(diagnosticData)
      }
      
      # Trajectory plot state (only if strata marked as ordered)
      if (self$options$showTrajectoryPlot && self$options$strataOrdered) {
        trajectoryData <- private$.prepareTrajectoryData(vResults, strataNames)
        self$results$trajectoryPlot$setState(trajectoryData)
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 10. Generate interpretation
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateInterpretationTable(
        cmhTest, loglinResult,
        weighted_vcorr, weightedCI,
        rowVar, colVar, strataVar,
        marginalP = marginalChiSq$p.value, marginalVcorr = marginalVResult$vcorr,
        pooledChiSq = pooledChiSq, reversalCells = reversalCells
      )
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 11. Populate method information
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateMethodInfo()
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 12. Populate references
      # ═══════════════════════════════════════════════════════════════════════════
      
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Build stratified array
    # ═══════════════════════════════════════════════════════════════════════════
    
    .buildStratifiedArray = function(data, rowVar, colVar, strataVar) {
      
      # Shared helper (utils.R): safe with any variable name, and stores
      # counts as doubles so that mantelhaen.test() cannot overflow with
      # large samples (integer products of margins returned NA before)
      fullTable <- .ctBuildTable(data, c(rowVar, colVar, strataVar),
                                 self$options$counts)
      .ctRejectEmptyMargins(fullTable, rowVar, colVar)
      
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
      
      # Handle empty or invalid tables
      if (is.null(tbl) || n == 0 || is.na(n) || nr < 2 || nc < 2) {
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
      # Resampling n observations with replacement is equivalent to one
      # multinomial draw with the observed cell proportions; drawing the
      # table directly keeps the cost independent of the sample size
      cell_p <- as.vector(tbl) / n
      
      boot_v_corr <- numeric(nBootstrap)
      
      for (b in 1:nBootstrap) {
        # A-B. Resampled table (original dimensions kept; cells may be 0)
        tbl_boot <- matrix(stats::rmultinom(1, round(n), cell_p),
                           nrow = nr, ncol = nc, dimnames = dimnames(tbl))
        
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
        
        # E. Calculate V_corrected for this replicate (a replicate with an
        # empty row or column has no V on the observed dimensions: dropped)
        if (any(r_boot == 0) || any(c_boot == 0)) {
          val <- NA_real_
        } else if (is.na(chi_boot_obs) || chi_boot_max <= 0) {
          val <- 0
        } else {
          val <- sqrt(chi_boot_obs / chi_boot_max)
          if (is.na(val) || val > 1) val <- 1
        }
        boot_v_corr[b] <- val
        
      }
      
      # Percentile CI
      ci <- quantile(boot_v_corr, probs = c(0.025, 0.975), na.rm = TRUE)
      
      return(list(
        n_dropped = sum(is.na(boot_v_corr)),
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
      
      # Cell proportions of each partial table (stratum sizes stay fixed)
      listOfProbs <- lapply(listOfTables, function(tbl) {
        list(p = as.vector(tbl) / sum(tbl), n = sum(tbl),
             nr = nrow(tbl), nc = ncol(tbl), dn = dimnames(tbl))
      })
      
      boot_weighted <- numeric(nBootstrap)
      
      for (b in 1:nBootstrap) {
        boot_v_k <- numeric(K)
        
        for (i in 1:K) {
          # A-B. Resampled partial table: one multinomial draw with the
          # observed cell proportions (equivalent to resampling cases)
          pr <- listOfProbs[[i]]
          tbl_boot <- matrix(stats::rmultinom(1, round(pr$n), pr$p),
                             nrow = pr$nr, ncol = pr$nc, dimnames = pr$dn)
          
          # C. Chi Obs
          suppressWarnings({
            chi_obs <- stats::chisq.test(tbl_boot, correct = FALSE)$statistic
          })
          
          # D. Chi Max (Recalculate!)
          r_boot <- rowSums(tbl_boot)
          c_boot <- colSums(tbl_boot)
          max_res <- private$.max_chisq(r_boot, c_boot)
          chi_max <- max_res$max_chisq
          
          # E. V Corrected for Stratum i (empty row or column: replicate dropped)
          if (any(r_boot == 0) || any(c_boot == 0)) {
            val <- NA_real_
          } else if (is.na(chi_obs) || chi_max <= 0) {
            val <- 0
          } else {
            val <- sqrt(as.numeric(chi_obs) / chi_max)
            if (is.na(val) || val > 1) val <- 1
          }
          boot_v_k[i] <- val
        }
        
        # Calculate weighted average for this bootstrap iteration
        # Weights are sample sizes (fixed across bootstraps)
        boot_weighted[b] <- sum(stratum_totals * boot_v_k) / sum(stratum_totals)
      }
      
      # Percentile CI
      ci <- quantile(boot_weighted, probs = c(0.025, 0.975), na.rm = TRUE)
      
      return(list(
        n_dropped = sum(is.na(boot_weighted)),
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
      
      # A failed fit is reported as NaN (never as p = 1, which would
      # present a failure as evidence of homogeneity)
      failed <- list(statistic = NaN, df = NaN, pvalue = NaN, failed = TRUE)
      
      lrt <- tryCatch({
        suppressWarnings({
          # Homogeneous model: Row*Col + Row*Stratum + Col*Stratum
          # (no three-way interaction)
          model_homo <- stats::glm(Freq ~ Row*Col + Row*Stratum + Col*Stratum,
                                   data = df, family = stats::poisson())
          # Saturated model: Row*Col*Stratum
          model_sat <- stats::glm(Freq ~ Row*Col*Stratum,
                                  data = df, family = stats::poisson())
        })
        if (!model_homo$converged || !model_sat$converged) NULL
        else stats::anova(model_homo, model_sat, test = "LRT")
      }, error = function(e) NULL)
      
      if (is.null(lrt)) return(failed)
      
      # Extract G² statistic (deviance difference)
      G2 <- lrt$Deviance[2]
      df_diff <- lrt$Df[2]
      pvalue <- lrt$`Pr(>Chi)`[2]
      
      if (is.na(G2) || is.na(df_diff) || is.na(pvalue)) return(failed)
      
      return(list(
        statistic = G2,
        df = df_diff,
        pvalue = pvalue,
        failed = FALSE
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial tables (one per stratum) - USES setRow()
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
        
        # Get pre-existing table from array (structure created in .init)
        table <- partialTablesGroup$get(key = k)
        
        # Populate rows using setRow (structure already created in .init)
        for (i in 1:nRows) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:nCols) {
            rowValues[[paste0("col", j)]] <- partialTab[i, j]
          }
          rowValues[['rowtotal']] <- sum(partialTab[i,])
          table$setRow(rowKey = i, values = rowValues)
        }
        
        # Populate column totals row
        totalRowValues <- list(rowname = 'Total')
        for (j in 1:nCols) {
          totalRowValues[[paste0("col", j)]] <- sum(partialTab[,j])
        }
        totalRowValues[['rowtotal']] <- sum(partialTab)
        table$setRow(rowKey = 'total', values = totalRowValues)
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Marginal table - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMarginalTable = function(marginalTable, rowVar, colVar) {
      
      table <- self$results$marginalTable
      rowNames <- rownames(marginalTable)
      colNames <- colnames(marginalTable)
      nRows <- nrow(marginalTable)
      nCols <- ncol(marginalTable)
      
      # Populate rows using setRow (structure already created in .init)
      for (i in 1:nRows) {
        rowValues <- list(rowname = rowNames[i])
        for (j in 1:nCols) {
          rowValues[[paste0("col", j)]] <- marginalTable[i, j]
        }
        rowValues[['rowtotal']] <- sum(marginalTable[i,])
        table$setRow(rowKey = i, values = rowValues)
      }
      
      # Populate column totals row
      totalRowValues <- list(rowname = 'Total')
      for (j in 1:nCols) {
        totalRowValues[[paste0("col", j)]] <- sum(marginalTable[,j])
      }
      totalRowValues[['rowtotal']] <- sum(marginalTable)
      table$setRow(rowKey = 'total', values = totalRowValues)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Stratum-specific results table - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateStratumResultsTable = function(vResults, marginalVResult, chiSqResults, 
                                            marginalChiSq, strataNames, pooledChiSq = NULL) {
      
      table <- self$results$stratumResultsTable
      K <- length(vResults)
      
      # Update rows for each partial table using setRow
      for (k in 1:K) {
        table$setRow(rowKey = k, values = list(
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
      
      # Update marginal table row
      table$setRow(rowKey = 'marginal', values = list(
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
      
      # Pooled row: a test only, so the V columns stay blank
      if (!is.null(pooledChiSq)) {
        table$setRow(rowKey = 'pooled', values = list(
          stratum = "Pooled (sum of partial)",
          chisq = pooledChiSq$statistic, df = pooledChiSq$df, pvalue = pooledChiSq$pvalue
        ))
      }
      
      # Add footnote about V corrected CI interpretation
      table$setNote('vcorrCINote', 
                    paste0("V corrected is bounded between 0 and 1 by definition; ",
                           "its bootstrap confidence interval therefore ",
                           "cannot be used to test the null hypothesis of no association."))
      
      nDropped <- sum(sapply(vResults, function(x) if (is.null(x$n_dropped)) 0 else x$n_dropped)) +
        (if (is.null(marginalVResult$n_dropped)) 0 else marginalVResult$n_dropped)
      if (nDropped > 0) {
        table$setNote('bootDropped', sprintf(paste0(
          "Bootstrap: %d resampled table(s) with an empty row or column (across all tables) ",
          "were left out of the percentile intervals."), nDropped))
      } else {
        table$setNote('bootDropped', NULL)
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: CMH test table - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateCMHTable = function(cmhTest) {
      
      table <- self$results$cmhTestTable
      
      table$setRow(rowKey = 1, values = list(
        test = "Generalised CMH",
        statistic = as.numeric(cmhTest$statistic),
        df = as.integer(cmhTest$parameter),
        pvalue = cmhTest$p.value
      ))
      
      # Add conditional footnote based on significance
      cmhSig <- cmhTest$p.value < 0.05
      if (cmhSig) {
        cmhFootnote <- paste0(
          "The CMH test is significant: evidence of conditional dependence, that is, of an ",
          "association between the row and column variables in at least one stratum. ",
          "See the homogeneity test to judge whether the association differs across strata."
        )
      } else {
        cmhFootnote <- paste0(
          "The CMH test is not significant: no evidence of conditional dependence, that is, the data ",
          "are compatible with no association in any stratum. Different patterns in different strata ",
          "can cancel out, so check the homogeneity test and the pooled chi-squared test as well."
        )
      }
      table$setNote('cmhInterpretation', cmhFootnote)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Homogeneity test table - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateHomogeneityTable = function(loglinResult) {
      
      table <- self$results$homogeneityTable
      
      # A failed fit leaves the cells blank (the note explains why)
      fitted <- !is.na(loglinResult$pvalue)
      table$setRow(rowKey = 1, values = list(
        test = "Log-Linear Likelihood Ratio",
        statistic = if (fitted) loglinResult$statistic else "",
        df = if (fitted) loglinResult$df else "",
        pvalue = if (fitted) loglinResult$pvalue else ""
      ))
      
      # Add conditional footnote based on significance
      if (is.na(loglinResult$pvalue)) {
        table$setNote('homogeneityInterpretation', paste0(
          "The log-linear models could not be fitted (for example because of ",
          "very sparse partial tables), so the homogeneity test is not available."))
        return()
      }
      llSig <- loglinResult$pvalue < 0.05
      if (llSig) {
        homogFootnote <- paste0(
          "The test is significant: the association pattern is heterogeneous across strata. ",
          "Examine stratum-specific values rather than the weighted average."
        )
      } else {
        homogFootnote <- paste0(
          "The test is not significant: no evidence that the association differs across strata ",
          "(the test cannot show that it is the same in every stratum)."
        )
      }
      table$setNote('homogeneityInterpretation', homogFootnote)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Summary measure table - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateSummaryMeasureTable = function(weighted_vcorr, weightedCI, loglinResult, cmhTest) {
      
      table <- self$results$summaryMeasureTable
      
      table$setRow(rowKey = 1, values = list(
        measure = "Weighted Average V corrected",
        estimate = weighted_vcorr,
        ciLower = weightedCI$ci_lower,
        ciUpper = weightedCI$ci_upper
      ))
      if (isTRUE(weightedCI$n_dropped > 0)) {
        table$setNote('bootDropped', sprintf(paste0(
          "Bootstrap: %d replicate(s) in which a resampled partial table had an empty row or column ",
          "were left out of the percentile interval."), weightedCI$n_dropped))
      } else {
        table$setNote('bootDropped', NULL)
      }
      
      # Add footnote about interpretation
      llSig <- isTRUE(loglinResult$pvalue < 0.05)
      cmhSig <- isTRUE(cmhTest$p.value < 0.05)
      
      if (is.na(loglinResult$pvalue) || is.na(cmhTest$p.value)) {
        summaryFootnote <- paste0(
          "One of the tests could not be computed, so whether this summary ",
          "is meaningful cannot be judged from the tests.")
      } else if (llSig) {
        # Heterogeneous association: no single summary
        summaryFootnote <- paste0(
          "Heterogeneity detected: this summary may be misleading. ",
          "Report stratum-specific V corrected values instead."
        )
      } else if (!cmhSig) {
        # No conditional association detected
        summaryFootnote <- paste0(
          "No conditional association detected (CMH test not significant). ",
          "This summary measure is not substantively meaningful."
        )
      } else {
        # Conditional association exists and is homogeneous
        summaryFootnote <- paste0(
          "No evidence of heterogeneity: this weighted average can be used as a summary of the conditional association."
        )
      }
      table$setNote('summaryInterpretation', summaryFootnote)
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
    # Populate: Residuals section - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateResidualsSection = function(listOfTables, strataNames, rowVar, colVar) {
      
      K <- length(listOfTables)
      
      residualsGroup <- self$results$residualsGroup
      
      for (k in 1:K) {
        
        # Compute ASR for this stratum
        tbl <- listOfTables[[k]]
        asr <- private$.computeAdjustedStandardisedResiduals(tbl)
        
        rowNames <- rownames(asr)
        colNames <- colnames(asr)
        nRows <- nrow(asr)
        nCols <- ncol(asr)
        
        # Get pre-existing table from array (structure created in .init)
        table <- residualsGroup$get(key = k)
        
        # Populate rows with colour coding for significant residuals
        for (i in 1:nRows) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:nCols) {
            rowValues[[paste0("col", j)]] <- asr[i, j]
          }
          table$setRow(rowKey = i, values = rowValues)
          
          # Mark residuals beyond \u00b11.96 with a footnote (native marker)
          for (j in 1:nCols) {
            value <- asr[i, j]
            if (!is.na(value) && abs(value) > 1.96) {
              table$addFootnote(rowKey = i, col = paste0('col', j), "|residual| > 1.96 (p < .05)")
              table$addFormat(rowKey = i, col = paste0('col', j), format = jmvcore::Cell.NEGATIVE)   # red, kept pending the Phase 4 decision
            }
          }
        }
        
        # Add footnote to last table only
        if (k == K) {
          table$setNote('residualsExplanation', 
                        paste0("Adjusted standardised residuals follow an approximate standard normal distribution. ",
                               "Values exceeding \u00B11.96 indicate cells contributing significantly to the ",
                               "chi-squared statistic at \u03B1 = 0.05. For additional residual measures and ",
                               "multiple comparison corrections, filter your data to a single stratum and use ",
                               "the Post-Hoc Analysis facility.")
          )
        }
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Interpretation Summary Table - USES setRow()
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateInterpretationTable = function(cmhTest, loglinResult,
                                            weighted_vcorr, weightedCI,
                                            rowVar, colVar, strataVar,
                                            marginalP = NA, marginalVcorr = NA,
                                            pooledChiSq = NULL, reversalCells = character(0)) {
      
      table <- self$results$interpretationTable
      
      # ─────────────────────────────────────────────────────────────────────────
      # Guard against invalid test results
      # ─────────────────────────────────────────────────────────────────────────
      
      if (is.null(cmhTest$p.value) || is.na(cmhTest$p.value) ||
          is.null(loglinResult$pvalue) || is.na(loglinResult$pvalue)) {
        
        table$setRow(rowKey = 'condIndep', values = list(
          result = "Could not be determined (invalid test results)"
        ))
        table$setRow(rowKey = 'homogeneity', values = list(
          result = "Could not be determined (invalid test results)"
        ))
        table$setRow(rowKey = 'scenario', values = list(
          result = "Interpretation unavailable"
        ))
        return()
      }
      
      # ─────────────────────────────────────────────────────────────────────────
      # Determine significance flags
      # ─────────────────────────────────────────────────────────────────────────
      
      cmhSig <- cmhTest$p.value < 0.05
      llSig <- loglinResult$pvalue < 0.05
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 1: Determine conditional dependence status
      # ─────────────────────────────────────────────────────────────────────────
      
      pairTxt <- paste0("'", rowVar, "' and '", colVar, "'")
      marginalSig <- isTRUE(marginalP < 0.05)
      pooledSig <- isTRUE(pooledChiSq$pvalue < 0.05)
      pooledPTxt <- if (isTRUE(pooledChiSq$pvalue < 0.001)) "p < 0.001" else
        sprintf("p = %.3f", pooledChiSq$pvalue)
      reversal <- length(reversalCells) > 0
      reversalTxt <- if (reversal) paste0(
        " The pattern reverses between strata in ", paste(reversalCells, collapse = ", "),
        " (adjusted residual above 1.96 in one stratum and below \u22121.96 in another).") else ""
      
      # Which test decides conditional dependence: both tests share the null hypothesis
      # of conditional independence, but the generalised CMH test is aimed at
      # a common pattern of association, so it is used when there is no evidence of
      # heterogeneity (where it is the most powerful test); with significant
      # heterogeneity the pooled chi-squared test, which is not aimed at a common pattern,
      # is used instead
      if (llSig) {
        if (pooledSig) {
          stage1Text <- paste0(
            "Pooled chi-squared test significant (", pooledPTxt, "): ", pairTxt,
            " are associated within strata, but the association differs across them. The CMH test (",
            if (cmhSig) "significant" else "not significant",
            "), which is designed to detect a pattern common to all strata, should ", if (reversal) "not be interpreted." else "be read with caution.",
            reversalTxt
          )
        } else {
          stage1Text <- paste0(
            "Pooled chi-squared test not significant: no evidence of an ",
            "association between ", pairTxt, " within strata of '", strataVar, "'. ",
            "Because the strata are heterogeneous, the CMH test (", if (cmhSig) "significant" else "not significant",
            "), which assumes a pattern common to all strata, is not used for this judgement."
          )
        }
      } else if (cmhSig) {
        stage1Text <- paste0(
          "CMH test significant (p < 0.05): conditional dependence; ", pairTxt,
          " are associated in at least one stratum of '", strataVar, "'."
        )
      } else {
        stage1Text <- paste0(
          "CMH test not significant (p \u2265 0.05): no evidence of conditional dependence; the data ",
          "are compatible with no association between ", pairTxt, " in any stratum of '", strataVar, "'."
        )
      }
      
      if (llSig) {
        stage2Text <- paste0(
          "Log-linear homogeneity test significant: the association differs across strata ",
          "(effect modification)."
        )
      } else {
        stage2Text <- paste0(
          "Log-linear homogeneity test not significant: no evidence that the association ",
          "differs across strata (the test cannot show that it is the same)."
        )
      }
      
      fmt3 <- function(x) sprintf("%.3f", x)
      if (!llSig && cmhSig) {
        scenario <- "Homogeneous Association"
        recommendation <- paste0(
          "the association is present within strata, with no evidence that it differs across them. ",
          "The weighted average V corrected (", fmt3(weighted_vcorr),
          if (is.finite(weightedCI$ci_lower) && is.finite(weightedCI$ci_upper))
            paste0("; 95% CI: ", fmt3(weightedCI$ci_lower), "\u2013", fmt3(weightedCI$ci_upper)) else
            "; no bootstrap interval, every replicate was dropped",
          ") can be used as a summary.",
          if (is.finite(marginalVcorr)) paste0(
            " Compare it with the marginal V corrected (", fmt3(marginalVcorr), "): a clear ",
            "difference would suggest that '", strataVar, "' changes the strength of the association.")
          else ""
        )
      } else if (!llSig && !cmhSig) {
        scenario <- "Conditional Independence"
        recommendation <- paste0(
          "no evidence of an association between ", pairTxt, " within strata of '", strataVar, "'.",
          if (marginalSig) paste0(
            " Because the marginal table does show an association, this is consistent with '",
            strataVar, "' accounting for it (confounding).")
          else " The marginal table shows no association either."
        )
      } else if (llSig && pooledSig) {
        scenario <- if (reversal) "Interaction (Pattern Reversal)" else "Interaction"
        recommendation <- paste0(
          "'", strataVar, "' acts as an effect modifier: the association differs across strata",
          if (reversal) ", and in some cells it reverses" else "",
          ". Report stratum-specific V corrected values and residuals rather than the weighted average."
        )
      } else {
        scenario <- "Heterogeneous Pattern Without Overall Association"
        recommendation <- paste0(
          "the homogeneity test indicates that the strata differ, but the pooled chi-squared test, used in ",
          "place of the CMH test when the strata are heterogeneous, finds no association within them. ",
          "This may reflect insufficient power. ",
          "Examine the stratum-specific chi-squared tests and residuals."
        )
      }
      
      stage3Text <- paste0(scenario, ": ", recommendation, " (Based on \u03B1 = 0.05)")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Populate table using setRow (structure created in .init)
      # ─────────────────────────────────────────────────────────────────────────
      
      table$setRow(rowKey = 'condIndep', values = list(result = stage1Text))
      table$setRow(rowKey = 'homogeneity', values = list(result = stage2Text))
      table$setRow(rowKey = 'scenario', values = list(result = stage3Text))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Determine scenario label
    # ═══════════════════════════════════════════════════════════════════════════
    
    .determineScenario = function(llSig, decideSig) {
      if (!llSig && decideSig) {
        return("Conditional dependence, no evidence of heterogeneity")
      } else if (!llSig && !decideSig) {
        return("Conditional independence")
      } else if (llSig && decideSig) {
        return("Conditional dependence with heterogeneity (Interaction)")
      } else {
        return("Heterogeneity, but no association found within strata")
      }
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
          in at least one stratum (conditional dependence), or in none (conditional independence)?</li>
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
        the marginals change with resampling. Resampled tables in which a row or column is empty 
        have no V<sub>corrected</sub> on the observed dimensions and are left out (a table note 
        reports how many). The resulting percentile-based confidence intervals 
        are not necessarily symmetric around the point estimate; this asymmetry reflects the 
        shape of the bootstrap distribution rather than a computational error.</p>
        
        <p><strong>Important limitation:</strong> Because V<sub>corrected</sub> is a strictly 
        non-negative statistic (bounded between 0 and 1), the percentile bootstrap confidence 
        interval cannot extend below zero. This has two consequences for interpretation:</p>
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
        <p>The CMH test generalises to R\u00D7C\u00D7K tables (Landis, Heyman & Koch 1978; the 
        general-association statistic computed by R's <code>mantelhaen.test()</code>), testing conditional 
        independence across all strata. The null hypothesis states that there is no association between 
        the row and column variables in any stratum. It does not test whether the association is 
        the same across strata, which is the job of the log-linear homogeneity test.</p>
        
        <p>For R\u00D7C tables, the test statistic follows a chi-squared distribution with 
        (R\u22121)(C\u22121) degrees of freedom. A significant result indicates conditional 
        dependence: evidence of association between the row and column variables in at least one 
        stratum (the generalised CMH test is most powerful when the 
        association follows a similar pattern in each stratum; opposing patterns can cancel out).</p>
        
        <p>Unlike the 2\u00D72 case, the generalised CMH test does not provide a common 
        measure of effect size (such as a common odds ratio). Instead, summary association 
        measures like the weighted average V<sub>corrected</sub> are used when there is no evidence 
        of heterogeneity.</p>
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
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Pooled Chi-Squared Test</h3>
        <p>The pooled chi-squared test sums the chi-squared values of the partial tables (with the 
        degrees of freedom summed likewise). The generalised CMH test combines the departures from 
        independence of each cell across strata before squaring them, so a cell that is over-represented 
        in one stratum and under-represented in another can cancel out; the pooled test squares within 
        each stratum first, so it cannot. The pooled test is therefore the appropriate test of conditional 
        dependence when the association differs across strata, whereas the CMH test is more powerful when 
        the strata share the same pattern.</p>
        
        <p>With nominal variables there is no single direction of association to compare, as there is with 
        the odds ratio of a 2\u00D72 table. The counterpart of an odds ratio reversing direction is a 
        <em>pattern reversal</em>: a cell whose adjusted standardised residual is significantly positive 
        (above 1.96) in one stratum and significantly negative (below \u22121.96) in another. The 
        interpretation names any such cells. V<sub>corrected</sub> cannot show this, since it measures 
        strength only.</p>
        
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Interpretational Scenarios</h3>
        <p>The homogeneity test is read first, because it decides which test of conditional dependence 
        applies (as shown in the diagnostic decision tree):</p>
        
        <ol>
          <li><strong>Homogeneity test not significant, CMH significant:</strong> conditional dependence with 
          no evidence that the association differs across strata (a non-significant test cannot show that it 
          is the same). The weighted average V<sub>corrected</sub> can be used as a summary; comparing it with 
          the marginal V<sub>corrected</sub> shows whether controlling for the stratifying variable changes the 
          strength of the association.</li>
          
          <li><strong>Neither the homogeneity test nor the CMH test significant:</strong> no evidence of 
          association within strata. If the marginal table shows an association, this is consistent with the 
          stratifying variable accounting for it (confounding).</li>
          
          <li><strong>Homogeneity test significant, pooled test significant:</strong> the association is present 
          within strata but differs across them (interaction, or effect modification). The CMH result (designed to 
          detect a pattern common to all strata) is read with caution, or not interpreted when the pattern reverses. Report 
          stratum-specific V<sub>corrected</sub> values and residuals rather than the weighted average.</li>
          
          <li><strong>Homogeneity test significant, pooled test not significant:</strong> the strata appear to 
          differ, but no association is found within them; this may reflect insufficient power. Examine the 
          stratum-specific chi-squared tests and residuals.</li>
        </ol>
      ")
      
      html <- paste0(html,
                     "<h3 style='color: #2874A6; margin-top: 1.5em;'>Methodological Framework</h3>",
                     "<p>The approach implemented in this facility extends the framework described by ",
                     "Ott et al. (1992) for controlling for a third variable when analysing nominal variables ",
                     "with three or more categories. While Ott et al. recommend measures such as Goodman and ",
                     "Kruskal's lambda for larger tables (since Yule's Q is limited to 2\u00D72 tables), this facility ",
                     "employs Cram\u00E9r's V corrected as the association measure. The pooled chi-squared test described by Ott et al. ",
                     "(the sum of the stratum chi-squared statistics, with the sum of their degrees of freedom) and the ",
                     "generalised Cochran-Mantel-Haenszel test (Landis, Heyman & Koch 1978) share the null hypothesis of ",
                     "conditional independence but are different statistics, as explained above: the generalised CMH test ",
                     "is aimed at a common pattern of association across strata, the pooled test is not. Both are reported here.</p>")
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
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
    .forestPlot = function(image, ggtheme, theme, ...) {
      plotData <- image$state
      if (is.null(plotData)) {
        return(FALSE)
      }
      
      # First stratum at the top
      plotData$stratum <- factor(as.character(plotData$stratum),
                                 levels = rev(as.character(plotData$stratum)))
      # Circle for strata, square for the marginal table, diamond for the weighted average
      plotData$type <- factor(plotData$type, levels = c("Stratum-specific", "Marginal", "Weighted"))
      
      # jamovi theme first: it carries its own colour scales, which would
      # otherwise replace the manual scales below
      plt <- ggplot2::ggplot(plotData, ggplot2::aes(y = stratum)) + ggtheme +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#34495E", linewidth = 0.7) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = ciLower, xmax = ciUpper),
                               colour = "#2874A6", width = 0.25, linewidth = 0.7,
                               orientation = "y", na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(x = estimate, shape = type, size = type),
                            colour = "#2874A6", fill = "#D4AC0D", na.rm = TRUE) +
        ggplot2::scale_shape_manual(values = c("Stratum-specific" = 21, "Marginal" = 22, "Weighted" = 23),
                                    name = NULL, drop = TRUE,
                                    guide = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::scale_size_manual(values = c("Stratum-specific" = 3, "Marginal" = 3.5, "Weighted" = 4),
                                   guide = "none", drop = TRUE) +
        ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        ggplot2::labs(x = expression(V[corrected]), y = NULL, title = "V Corrected Across Strata",
                      caption = paste0("V corrected is non-negative, so its interval cannot test 'no association':\n",
                                       "use the chi-squared tests for that. Overlapping intervals suggest similar strength.")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
                       legend.position = "bottom")
      
      print(plt)
      TRUE
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Diagnostic Tree Render Function
    # ═══════════════════════════════════════════════════════════════════════════
    
    .diagnosticTree = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      treeData <- image$state
      
      # (states saved by earlier versions used a different layout)
      if (is.null(treeData) || is.null(treeData$decide_sig)) {
        return(FALSE)
      }
      
      # Build flow-chart style decision tree
      # Define node positions
      nodes <- data.frame(
        x = c(2, 1, 3, 0.5, 1.5, 2.5, 3.5),  # Horizontal positions
        y = c(4, 2, 2, 0, 0, 0, 0),  # Vertical positions (4=top, 0=bottom)
        label = c(
          "Homogeneity Test\n(Log-Linear)",
          "CMH Test",
          "Pooled \u03C7\u00B2 Test",
          "Conditional independence:\nno evidence of association\nwithin strata",
          "Homogeneous association:\nno evidence that it\ndiffers across strata",
          "Heterogeneity, but no\nassociation found\nwithin strata",
          "Heterogeneous association:\neffect modification\n(interaction) present"
        ),
        node_type = c("decision", "decision", "decision", 
                      "terminal", "terminal", "terminal", "terminal"),
        is_active = c(
          TRUE,                                         # homogeneity always evaluated
          !treeData$loglin_sig,                         # CMH decides (no heterogeneity)
          treeData$loglin_sig,                          # pooled test decides (heterogeneity)
          !treeData$loglin_sig && !treeData$decide_sig,
          !treeData$loglin_sig && treeData$decide_sig,
          treeData$loglin_sig && !treeData$decide_sig,
          treeData$loglin_sig && treeData$decide_sig
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
          !treeData$loglin_sig,
          treeData$loglin_sig,
          !treeData$loglin_sig && !treeData$decide_sig,
          !treeData$loglin_sig && treeData$decide_sig,
          treeData$loglin_sig && !treeData$decide_sig,
          treeData$loglin_sig && treeData$decide_sig
        ),
        stringsAsFactors = FALSE
      )
      
      # Text colour and background follow the jamovi theme (light or dark);
      # the node boxes keep their own fills, with text drawn on top of them
      thm <- .ctThemeColours(ggtheme)
      
      # Colour coding: active path in blue, inactive greyed out
      nodes$fill_color <- ifelse(nodes$is_active & nodes$node_type == "terminal",
                                 "#2874A6",  # Active terminal: blue
                                 ifelse(nodes$is_active, "#ECF0F1",  # Active decision: light grey
                                        "#BDC3C7"))  # Inactive: medium grey
      
      nodes$border_color <- ifelse(nodes$is_active, "#2C3E50", "#95A5A6")
      nodes$text_color <- ifelse(nodes$is_active & nodes$node_type == "terminal",
                                 "white", "#2C3E50")
      
      edges$line_color <- ifelse(edges$is_active, thm$text, "#95A5A6")
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
                           size = 2.9, hjust = 0.5, vjust = -0.5, color = thm$text) +
        
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
                           size = 3.1, fontface = "bold", lineheight = 0.9) +
        
        # Manual scales
        ggplot2::scale_color_identity() +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_linewidth_identity() +
        
        # Coordinate system and limits
        ggplot2::coord_cartesian(xlim = c(-0.5, 4.5), ylim = c(-0.8, 4.8)) +
        
        # Labels
        ggplot2::labs(
          # (the jamovi image already shows the title)
          title = NULL,
          subtitle = paste0("Your data: ", treeData$scenario)
        ) +
        
        # Theme
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = thm$background, colour = NA),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold",
                                             colour = thm$text, margin = ggplot2::margin(b = 5)),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, 
                                                colour = "#2874A6", face = "bold",
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
