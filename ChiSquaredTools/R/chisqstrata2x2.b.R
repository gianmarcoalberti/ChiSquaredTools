# This file is a generated template, your changes will not be overwritten

#' @export
chisqstrata2x2Class <- R6::R6Class(
  "chisqstrata2x2Class",
  inherit = chisqstrata2x2Base,
  private = list(
    
    .init = function() {
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Early return if required variables not specified
      # ═══════════════════════════════════════════════════════════════════════════
      
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
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise partial tables (one per stratum)
      # ═══════════════════════════════════════════════════════════════════════════
      
      partialTablesGroup <- self$results$partialTablesGroup
      
      for (k in 1:K) {
        # Always add item - jamovi handles duplicates gracefully when clearWith triggers
        partialTablesGroup$addItem(key = k)
        table <- partialTablesGroup$get(key = k)
        
        # Set title (safe to call repeatedly)
        table$setTitle(paste0(strataVar, " = ", strataLevels[k]))
        
        # Add row name column
        table$addColumn(
          name = 'rowname',
          title = rowVar,
          type = 'text'
        )
        
        # Add data columns (2 columns for 2x2 table)
        for (j in 1:2) {
          table$addColumn(
            name = paste0("col", j),
            title = colLevels[j],
            type = 'integer',
            superTitle = colVar
          )
        }
        
        # Add row total column
        table$addColumn(
          name = 'rowtotal',
          title = 'Total',
          type = 'integer'
        )
        
        # Add empty rows (2 data rows + 1 total row)
        for (i in 1:2) {
          table$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
        }
        table$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise marginal table
      # ═══════════════════════════════════════════════════════════════════════════
      
      marginalTable <- self$results$marginalTable
      marginalTable$setTitle("Marginal Table (collapsed across strata)")
      
      # Add row name column
      marginalTable$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text'
      )
      
      # Add data columns
      for (j in 1:2) {
        marginalTable$addColumn(
          name = paste0("col", j),
          title = colLevels[j],
          type = 'integer',
          superTitle = colVar
        )
      }
      
      # Add row total column
      marginalTable$addColumn(
        name = 'rowtotal',
        title = 'Total',
        type = 'integer'
      )
      
      # Add empty rows
      for (i in 1:2) {
        marginalTable$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
      }
      marginalTable$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise partial chi-squared table (always shown)
      # ═══════════════════════════════════════════════════════════════════════════
      
      chiSqTable <- self$results$partialChiSqTable
      
      # Add rows for each stratum
      for (k in 1:K) {
        chiSqTable$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataLevels[k], ")")
        ))
      }
      # Add marginal row
      chiSqTable$addRow(rowKey = 'marginal', values = list(stratum = "Marginal Table"))
      # Add pooled row
      chiSqTable$addRow(rowKey = 'pooled', values = list(stratum = "Pooled (sum of partial)"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise odds ratio table
      # ═══════════════════════════════════════════════════════════════════════════
      
      orTable <- self$results$oddsRatioTable
      
      # Add rows for each stratum
      for (k in 1:K) {
        orTable$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataLevels[k], ")")
        ))
      }
      # Add marginal row
      orTable$addRow(rowKey = 'marginal', values = list(stratum = "Marginal Table"))
      # Add common OR row
      orTable$addRow(rowKey = 'common', values = list(stratum = "MH Common OR"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise CMH test table
      # ═══════════════════════════════════════════════════════════════════════════
      
      cmhTable <- self$results$cmhTestTable
      cmhTable$addRow(rowKey = 1, values = list(test = "Cochran-Mantel-Haenszel"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise homogeneity tests table
      # ═══════════════════════════════════════════════════════════════════════════
      
      homogTable <- self$results$homogeneityTable
      homogTable$addRow(rowKey = 1, values = list(test = "Woolf"))
      homogTable$addRow(rowKey = 2, values = list(test = "Breslow-Day-Tarone"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise interpretation table
      # ═══════════════════════════════════════════════════════════════════════════
      
      interpTable <- self$results$interpretationTable
      interpTable$addRow(rowKey = 'condIndep', values = list(topic = 'Conditional Independence/Dependence'))
      interpTable$addRow(rowKey = 'homogeneity', values = list(topic = 'Homogeneity/Heterogeneity'))
      interpTable$addRow(rowKey = 'scenario', values = list(topic = 'Interpretation'))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise method info HTML element with empty content
      # This prevents layout shift when showMethodInfo is toggled
      # ═══════════════════════════════════════════════════════════════════════════
      
      self$results$methodInfo$setContent('')
    },
    
    .run = function() {
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 1a. Input validation
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (is.null(self$options$rows) || 
          is.null(self$options$cols) || 
          is.null(self$options$strata)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      strataVar <- self$options$strata
      
      # Get the selected level names from LevelSelector
      rowRefLevel <- self$options$rowRef
      colRefLevel <- self$options$colRef
      
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
      
      # Validate 2x2 requirement
      nRowLevels <- nlevels(data[[rowVar]])
      nColLevels <- nlevels(data[[colVar]])
      nStrataLevels <- nlevels(data[[strataVar]])
      
      if (nRowLevels != 2) {
        jmvcore::reject(
          paste0("Row variable '", rowVar, "' must have exactly 2 levels (has ", 
                 nRowLevels, ")"),
          code = "invalid_row_levels"
        )
      }
      
      if (nColLevels != 2) {
        jmvcore::reject(
          paste0("Column variable '", colVar, "' must have exactly 2 levels (has ", 
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
      # 1b. Determine reference category positions from level names
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Get factor levels
      rowLevels <- levels(data[[rowVar]])
      colLevels <- levels(data[[colVar]])
      
      # Determine which position (1 or 2) corresponds to selected level
      # Default to position 2 for row and position 1 for column if not specified
      if (is.null(rowRefLevel) || !(rowRefLevel %in% rowLevels)) {
        rowRefPos <- 2
        rowRefLevel <- rowLevels[2]
      } else {
        rowRefPos <- which(rowLevels == rowRefLevel)
      }
      
      if (is.null(colRefLevel) || !(colRefLevel %in% colLevels)) {
        colRefPos <- 1
        colRefLevel <- colLevels[1]
      } else {
        colRefPos <- which(colLevels == colRefLevel)
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 2. Build the 3D array of partial tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      array3D <- private$.buildStratifiedArray(data, rowVar, colVar, strataVar)
      strataNames <- dimnames(array3D)[[3]]
      K <- dim(array3D)[3]
      
      # Convert to list of 2x2 matrices for internal processing
      listOfTables <- lapply(1:K, function(i) array3D[,,i])
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 2b. Validate strata for degenerate tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Check each stratum for validity
      invalidStrata <- character(0)
      zeroStrata <- character(0)
      singleRowStrata <- character(0)
      singleColStrata <- character(0)
      
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
          singleRowStrata <- c(singleRowStrata, stratumName)
        }
        
        # Check for zero column margins (entire column is zero)
        colSums_k <- colSums(tbl)
        if (any(colSums_k == 0)) {
          singleColStrata <- c(singleColStrata, stratumName)
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
      
      if (length(singleRowStrata) > 0 || length(singleColStrata) > 0) {
        problemStrata <- unique(c(singleRowStrata, singleColStrata))
        jmvcore::reject(
          paste0(
            "The following strata have degenerate structure (entire row or column is zero): ",
            paste(problemStrata, collapse = ", "), ". ",
            "Chi-squared tests and odds ratio calculations require variation in both ",
            "rows and columns within each stratum."
          ),
          code = "degenerate_strata"
        )
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 3. Compute marginal table (population happens later, after OR calculation)
      # ═══════════════════════════════════════════════════════════════════════════
      
      marginalTable <- Reduce("+", listOfTables)
      dimnames(marginalTable) <- dimnames(array3D)[1:2]
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 4. Compute odds ratios and confidence intervals
      # ═══════════════════════════════════════════════════════════════════════════
      
      orsAndCIs <- lapply(listOfTables, private$.calculateORandCI, rowRefPos, colRefPos)
      marginalORandCI <- private$.calculateORandCI(marginalTable, rowRefPos, colRefPos)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 5. Populate tables (must be after OR calculation for annotations)
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Populate partial tables with OR annotations
      private$.populatePartialTables(array3D, strataNames, rowVar, colVar, strataVar,
                                     orsAndCIs, rowRefPos, colRefPos)
      
      # Populate marginal table with OR annotation
      private$.populateMarginalTable(marginalTable, rowVar, colVar,
                                     marginalORandCI, rowRefPos, colRefPos)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 6. Run statistical tests
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Suppress warnings for chi-squared tests with small expected counts
      suppressWarnings({
        
        # Chi-squared tests for each partial table
        chiSqResults <- lapply(listOfTables, function(tbl) {
          test <- stats::chisq.test(tbl, correct = FALSE)
          list(
            statistic = as.numeric(test$statistic),
            df = as.integer(test$parameter),
            pvalue = test$p.value
          )
        })
        
        # Chi-squared test for marginal table
        marginalChiSq <- stats::chisq.test(marginalTable, correct = FALSE)
        
        # Pooled chi-squared test (sum of stratum-specific chi-squared values)
        # This test is robust when stratum-specific associations operate in opposite directions
        pooledChiSq <- list(
          statistic = sum(sapply(chiSqResults, function(x) x$statistic)),
          df = sum(sapply(chiSqResults, function(x) x$df)),
          pvalue = stats::pchisq(
            sum(sapply(chiSqResults, function(x) x$statistic)),
            df = sum(sapply(chiSqResults, function(x) x$df)),
            lower.tail = FALSE
          )
        )
        
        # Cochran-Mantel-Haenszel test
        cmhTest <- stats::mantelhaen.test(array3D, correct = FALSE)
        
      })
      
      # Extract common OR and CI, adjusting for reference category choices
      # R's mantelhaen.test uses standard cell layout (a*d)/(b*c)
      # We need to transform if user selected different references
      commonOR_raw <- as.numeric(cmhTest$estimate)
      commonCI_raw <- as.numeric(cmhTest$conf.int)
      
      # Determine if we need to invert based on reference selections
      # Default R calculation assumes row1=outcome, col1=exposure giving (a*d)/(b*c)
      # Our default (rowRefPos=2, colRefPos=1) is the reciprocal
      needsInvert <- (rowRefPos == 2 && colRefPos == 1) ||
        (rowRefPos == 1 && colRefPos == 2)
      
      if (needsInvert) {
        commonOR <- 1 / commonOR_raw
        commonCI <- 1 / rev(commonCI_raw)
      } else {
        commonOR <- commonOR_raw
        commonCI <- commonCI_raw
      }
      
      # The MH estimate is not finite when, for example, the same
      # off-diagonal cell is 0 in every stratum: report it as unavailable
      commonValid <- is.finite(commonOR) && commonOR > 0 &&
        all(is.finite(commonCI)) && all(commonCI > 0)
      if (!commonValid) {
        commonOR <- NA_real_
        commonCI <- c(NA_real_, NA_real_)
      }
      
      # Woolf test for homogeneity (inverse-variance weighted log ORs)
      mhResult <- private$.woolfHomogeneityTest(listOfTables, rowRefPos, colRefPos)
      
      # Breslow-Day-Tarone test
      bdtResult <- private$.breslowDayTaroneTest(array3D)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 7. Populate results tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateOddsRatioTable(orsAndCIs, marginalORandCI, commonOR, 
                                      commonCI, strataNames)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 8. Prepare plot states
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Forest plot state
      # Always prepare forest plot state (visibility controlled by r.yaml)
      forestData <- private$.prepareForestData(orsAndCIs, marginalORandCI, commonOR, 
                                               commonCI, strataNames, K)
      self$results$forestPlot$setState(forestData)
      
      # Trajectory plot state (only if strata marked as ordered)
      if (self$options$showTrajectoryPlot && self$options$strataOrdered) {
        trajectoryData <- private$.prepareTrajectoryData(orsAndCIs, strataNames)
        image <- self$results$trajectoryPlot
        image$setState(trajectoryData)
      }
      
      private$.populatePartialChiSqTable(chiSqResults, marginalChiSq, pooledChiSq, strataNames)
      
      # Populate CMH table with conditional footnote
      private$.populateCMHTable(cmhTest, commonOR, commonCI)
      
      # Populate Homogeneity table with conditional footnote
      private$.populateHomogeneityTable(mhResult, bdtResult, K)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 9. Generate interpretation Table
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateInterpretationTable(
        marginalChiSq, pooledChiSq, cmhTest, mhResult, bdtResult,
        orsAndCIs, commonOR, commonCI, strataNames,
        rowVar, colVar, strataVar, marginalOR = marginalORandCI$or
      )
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 10. Populate method information
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateMethodInfo()
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Build 3D array from data
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
    # Helper: Calculate OR and 95% CI with Haldane-Anscombe correction
    # ═══════════════════════════════════════════════════════════════════════════
    
    .calculateORandCI = function(tbl, rowRefPos, colRefPos) {
      
      # Apply Haldane-Anscombe correction if any diagonal product is zero
      if (tbl[1,1] * tbl[2,2] == 0 || tbl[1,2] * tbl[2,1] == 0) {
        tbl <- tbl + 0.5
      }
      
      # Cell references based on user-selected reference categories
      # rowRefPos: 1 means row 1 is outcome; 2 means row 2 is outcome
      # colRefPos: 1 means col 1 is exposure; 2 means col 2 is exposure
      #
      # Standard OR = (outcome_exposed * non-outcome_unexposed) / 
      #               (outcome_unexposed * non-outcome_exposed)
      
      if (rowRefPos == 2 && colRefPos == 1) {
        # Outcome = row 2, Exposure = col 1 (default)
        # OR = (row2_col1 * row1_col2) / (row2_col2 * row1_col1)
        or <- (tbl[2,1] * tbl[1,2]) / (tbl[2,2] * tbl[1,1])
      } else if (rowRefPos == 2 && colRefPos == 2) {
        # Outcome = row 2, Exposure = col 2
        # OR = (row2_col2 * row1_col1) / (row2_col1 * row1_col2)
        or <- (tbl[2,2] * tbl[1,1]) / (tbl[2,1] * tbl[1,2])
      } else if (rowRefPos == 1 && colRefPos == 1) {
        # Outcome = row 1, Exposure = col 1
        # OR = (row1_col1 * row2_col2) / (row1_col2 * row2_col1)
        or <- (tbl[1,1] * tbl[2,2]) / (tbl[1,2] * tbl[2,1])
      } else {
        # rowRefPos == 1 && colRefPos == 2
        # Outcome = row 1, Exposure = col 2
        # OR = (row1_col2 * row2_col1) / (row1_col1 * row2_col2)
        or <- (tbl[1,2] * tbl[2,1]) / (tbl[1,1] * tbl[2,2])
      }
      
      # Standard error of log OR (same regardless of direction)
      a <- tbl[1,1]; b <- tbl[1,2]; c <- tbl[2,1]; d <- tbl[2,2]
      se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
      ci_lower <- exp(log(or) - 1.96 * se_log_or)
      ci_upper <- exp(log(or) + 1.96 * se_log_or)
      
      return(list(or = or, ci_lower = ci_lower, ci_upper = ci_upper))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Generate OR annotation text (plain text for table footnote)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .generateORAnnotationText = function(or, rowLevels, colLevels, rowRefPos, colRefPos) {
      
      # Same wording as the Association facility: the column variable defines
      # the groups (independent variable in the columns), the row variable the
      # outcome (the OR is the same either way)
      rowRef <- rowLevels[rowRefPos]
      rowOther <- rowLevels[3 - rowRefPos]
      colRef <- colLevels[colRefPos]
      colOther <- colLevels[3 - colRefPos]
      rowVar <- self$options$rows
      colVar <- self$options$cols
      fmtNum <- function(x) if (abs(x) >= 0.01) sprintf("%.2f", x) else format(signif(x, 2))
      
      if (!is.finite(or))
        return(paste0("Interpretation: the odds ratio is not finite for this table, ",
                      "so it cannot be expressed as a multiple of the odds."))
      
      if (isTRUE(all.equal(or, 1))) {
        core <- "the same as"
        pct <- ""
      } else {
        core <- paste0(fmtNum(or), " times")
        pct <- if (or > 1) sprintf(" (%s%% higher)", format(round((or - 1) * 100), big.mark = ","))
               else sprintf(" (%d%% lower)", round((1 - or) * 100))
      }
      
      paste0("Interpretation (OR = ", fmtNum(or), "): for ", colVar, " = '", colRef,
             "', the odds of ", rowVar, " = '", rowRef, "' rather than '", rowOther,
             "' are ", core, " those for '", colOther, "'", pct, ". ",
             "See tables below for confidence intervals and significance tests.")
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Woolf (1955) test for homogeneity of odds ratios
    # ═══════════════════════════════════════════════════════════════════════════
    
    .woolfHomogeneityTest = function(listOfTables, rowRefPos, colRefPos) {
      
      # Inverse-variance (Woolf) weights of the stratum log odds ratios
      mhCalculateWeights <- function(tbl) {
        # Apply Haldane-Anscombe correction if needed
        if (tbl[1,1] * tbl[2,2] == 0 || tbl[1,2] * tbl[2,1] == 0) {
          tbl <- tbl + 0.5
        }
        a <- tbl[1,1]; b <- tbl[1,2]; c <- tbl[2,1]; d <- tbl[2,2]
        mh_weight <- 1 / (1/a + 1/b + 1/c + 1/d)
        return(mh_weight)
      }
      
      # Calculate weights for each partial table
      mhWeights <- sapply(listOfTables, mhCalculateWeights)
      
      # Calculate log OR for each partial table
      mhLogOR <- log(sapply(listOfTables, function(tbl) {
        private$.calculateORandCI(tbl, rowRefPos, colRefPos)$or
      }))
      
      # Compute the weighted average of the log ORs
      Y_bar <- sum(mhWeights * mhLogOR) / sum(mhWeights)
      
      # Compute the MH test statistic
      mhStatistic <- sum(mhWeights * (mhLogOR - Y_bar)^2)
      
      # Degrees of freedom
      df <- length(listOfTables) - 1
      
      # P-value
      pvalue <- stats::pchisq(mhStatistic, df, lower.tail = FALSE)
      
      return(list(statistic = mhStatistic, df = df, pvalue = pvalue))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Breslow-Day-Tarone test for homogeneity
    # (Implementation based on Hoehle 2000, equations from Lachin 2000)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .breslowDayTaroneTest = function(x) {
      
      # Number of strata
      K <- dim(x)[3]
      
      # Get the common OR based on Mantel-Haenszel
      # Wrap in tryCatch to handle edge cases
      mhResult <- tryCatch(
        stats::mantelhaen.test(x),
        error = function(e) NULL
      )
      
      if (is.null(mhResult)) {
        return(list(statistic = NA, df = K - 1, pvalue = NA, 
                    error = "Could not compute MH common OR"))
      }
      
      or_hat_mh <- as.numeric(mhResult$estimate)
      
      # Handle edge cases where OR is exactly 0, Inf, or NA
      if (is.na(or_hat_mh) || is.infinite(or_hat_mh) || or_hat_mh == 0) {
        return(list(statistic = NA, df = K - 1, pvalue = NA,
                    error = "Common odds ratio is undefined or extreme"))
      }
      
      # Initialise
      X2_HBD <- 0
      a <- tildea <- Var_a <- numeric(K)
      validStrata <- 0
      
      for (j in 1:K) {
        # Find marginals of table j
        mj <- apply(x[,,j], MARGIN = 1, sum)
        nj <- apply(x[,,j], MARGIN = 2, sum)
        
        # Skip strata with zero marginals (degenerate tables)
        if (any(mj == 0) || any(nj == 0)) {
          a[j] <- NA
          tildea[j] <- NA
          Var_a[j] <- NA
          next
        }
        
        # Solve for tilde(a)_j using quadratic formula
        coef <- c(-mj[1] * nj[1] * or_hat_mh, 
                  nj[2] - mj[1] + or_hat_mh * (nj[1] + mj[1]),
                  1 - or_hat_mh)
        
        # Check for invalid polynomial coefficients
        if (any(is.na(coef)) || any(is.infinite(coef))) {
          a[j] <- NA
          tildea[j] <- NA
          Var_a[j] <- NA
          next
        }
        
        # Attempt to find roots
        sols <- tryCatch(
          Re(polyroot(coef)),
          error = function(e) NULL
        )
        
        if (is.null(sols)) {
          a[j] <- NA
          tildea[j] <- NA
          Var_a[j] <- NA
          next
        }
        
        # Take the root which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
        tildeaj <- sols[(0 < sols) & (sols <= min(nj[1], mj[1]))]
        
        # Handle edge case where no valid root is found
        if (length(tildeaj) == 0) {
          tildeaj <- min(nj[1], mj[1]) / 2
        } else {
          tildeaj <- tildeaj[1]
        }
        
        # Observed value
        aj <- x[1,1,j]
        
        # Determine other expected cell entries
        tildebj <- mj[1] - tildeaj
        tildecj <- nj[1] - tildeaj
        tildedj <- mj[2] - tildecj
        
        # Check for zero expected cells (would cause division by zero)
        if (tildeaj <= 0 || tildebj <= 0 || tildecj <= 0 || tildedj <= 0) {
          a[j] <- NA
          tildea[j] <- NA
          Var_a[j] <- NA
          next
        }
        
        # Compute variance estimate
        Var_aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
        
        # Compute contribution to statistic
        X2_HBD <- X2_HBD + as.numeric((aj - tildeaj)^2 / Var_aj)
        
        # Store values
        a[j] <- aj
        tildea[j] <- tildeaj
        Var_a[j] <- Var_aj
        validStrata <- validStrata + 1
      }
      
      # Need at least 2 valid strata for the test
      if (validStrata < 2) {
        return(list(statistic = NA, df = K - 1, pvalue = NA,
                    error = "Insufficient valid strata for homogeneity test"))
      }
      
      # Remove NAs for final calculation
      a_valid <- a[!is.na(a)]
      tildea_valid <- tildea[!is.na(tildea)]
      Var_a_valid <- Var_a[!is.na(Var_a)]
      
      # Compute Tarone corrected test
      X2_HBDT <- as.numeric(X2_HBD - (sum(a_valid) - sum(tildea_valid))^2 / sum(Var_a_valid))
      
      # Handle negative test statistic (can occur with correction)
      if (X2_HBDT < 0) {
        X2_HBDT <- 0
      }
      
      # Compute p-value based on the Tarone corrected test
      pvalue <- 1 - stats::pchisq(X2_HBDT, df = K - 1)
      
      return(list(statistic = X2_HBDT, df = K - 1, pvalue = pvalue))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial tables (one per stratum)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populatePartialTables = function(array3D, strataNames, rowVar, colVar, strataVar,
                                      orsAndCIs, rowRefPos, colRefPos) {
      
      K <- dim(array3D)[3]
      rowNames <- dimnames(array3D)[[1]]
      colNames <- dimnames(array3D)[[2]]
      
      partialTablesGroup <- self$results$partialTablesGroup
      
      for (k in 1:K) {
        
        # Get the partial table
        partialTab <- array3D[,,k]
        
        # Get pre-existing table from array
        table <- partialTablesGroup$get(key = k)
        
        # Populate rows using setRow (structure already created in .init)
        for (i in 1:2) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:2) {
            rowValues[[paste0("col", j)]] <- partialTab[i, j]
          }
          rowValues[['rowtotal']] <- sum(partialTab[i,])
          table$setRow(rowKey = i, values = rowValues)
        }
        
        # Populate column totals row
        totalRowValues <- list(rowname = 'Total')
        for (j in 1:2) {
          totalRowValues[[paste0("col", j)]] <- sum(partialTab[,j])
        }
        totalRowValues[['rowtotal']] <- sum(partialTab)
        table$setRow(rowKey = 'total', values = totalRowValues)
        
        # Add OR annotation as table footnote (always shown)
        annotationText <- private$.generateORAnnotationText(
          or = orsAndCIs[[k]]$or,
          rowLevels = rowNames,
          colLevels = colNames,
          rowRefPos = rowRefPos,
          colRefPos = colRefPos
        )
        table$setNote('orNote', annotationText)
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Marginal table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMarginalTable = function(marginalTable, rowVar, colVar,
                                      marginalORandCI, rowRefPos, colRefPos) {
      
      table <- self$results$marginalTable
      rowNames <- rownames(marginalTable)
      colNames <- colnames(marginalTable)
      
      # Populate rows using setRow (structure already created in .init)
      for (i in 1:2) {
        rowValues <- list(rowname = rowNames[i])
        for (j in 1:2) {
          rowValues[[paste0("col", j)]] <- marginalTable[i, j]
        }
        rowValues[['rowtotal']] <- sum(marginalTable[i,])
        table$setRow(rowKey = i, values = rowValues)
      }
      
      # Populate column totals row
      totalRowValues <- list(rowname = 'Total')
      for (j in 1:2) {
        totalRowValues[[paste0("col", j)]] <- sum(marginalTable[,j])
      }
      totalRowValues[['rowtotal']] <- sum(marginalTable)
      table$setRow(rowKey = 'total', values = totalRowValues)
      
      # Add OR annotation (always shown)
      annotationText <- private$.generateORAnnotationText(
        or = marginalORandCI$or,
        rowLevels = rowNames,
        colLevels = colNames,
        rowRefPos = rowRefPos,
        colRefPos = colRefPos
      )
      table$setNote('orNote', annotationText)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Odds ratio table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateOddsRatioTable = function(orsAndCIs, marginalORandCI, commonOR, 
                                       commonCI, strataNames) {
      
      table <- self$results$oddsRatioTable
      K <- length(orsAndCIs)
      
      # Update rows for each partial table using setRow
      for (k in 1:K) {
        or_val <- orsAndCIs[[k]]$or
        ci_lower <- orsAndCIs[[k]]$ci_lower
        ci_upper <- orsAndCIs[[k]]$ci_upper
        
        table$setRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          or = or_val,
          ciLower = ci_lower,
          ciUpper = ci_upper
        ))
        
        # Highlight OR in red if CI excludes 1 (statistically significant)
        if (!is.na(ci_lower) && !is.na(ci_upper) && (ci_lower > 1 || ci_upper < 1)) {
          table$addFormat(rowKey = k, col = 'or', format = jmvcore::Cell.NEGATIVE)
        }
      }
      
      # Update marginal table row
      table$setRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        or = marginalORandCI$or,
        ciLower = marginalORandCI$ci_lower,
        ciUpper = marginalORandCI$ci_upper
      ))
      
      # Highlight marginal OR if significant
      if (!is.na(marginalORandCI$ci_lower) && !is.na(marginalORandCI$ci_upper) && 
          (marginalORandCI$ci_lower > 1 || marginalORandCI$ci_upper < 1)) {
        table$addFormat(rowKey = 'marginal', col = 'or', format = jmvcore::Cell.NEGATIVE)
      }
      
      # Update MH common OR row
      table$setRow(rowKey = 'common', values = list(
        stratum = "MH Common OR",
        or = if (is.na(commonOR)) "" else commonOR,
        ciLower = if (is.na(commonCI[1])) "" else commonCI[1],
        ciUpper = if (is.na(commonCI[2])) "" else commonCI[2]
      ))
      if (is.na(commonOR)) {
        table$addFootnote(rowKey = 'common', col = 'or', paste0(
          "The Mantel-Haenszel common odds ratio is not finite for these data ",
          "(e.g. the same cell is 0 in every stratum)."))
      }
      
      # Highlight common OR if significant
      if (!is.na(commonCI[1]) && !is.na(commonCI[2]) && (commonCI[1] > 1 || commonCI[2] < 1)) {
        table$addFormat(rowKey = 'common', col = 'or', format = jmvcore::Cell.NEGATIVE)
      }
      # Add footnote about CI overlap interpretation
      table$setNote(
        'ciOverlapNote',
        paste0(
          "Overlapping stratum-specific confidence intervals do not necessarily indicate homogeneity of odds ratios. ",
          "Two 95% CIs can overlap substantially yet still differ significantly (Cumming & Finch 2005). ",
          "Use the formal homogeneity tests below to assess whether odds ratios are equal across strata."
        )
      )
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial chi-squared table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populatePartialChiSqTable = function(chiSqResults, marginalChiSq, pooledChiSq, strataNames) {
      
      table <- self$results$partialChiSqTable
      K <- length(chiSqResults)
      
      # Update rows for each partial table using setRow
      for (k in 1:K) {
        table$setRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          chisq = chiSqResults[[k]]$statistic,
          df = chiSqResults[[k]]$df,
          pvalue = chiSqResults[[k]]$pvalue
        ))
      }
      
      # Update marginal table row
      table$setRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        chisq = as.numeric(marginalChiSq$statistic),
        df = as.integer(marginalChiSq$parameter),
        pvalue = marginalChiSq$p.value
      ))
      
      # Update pooled chi-squared row
      table$setRow(rowKey = 'pooled', values = list(
        stratum = "Pooled (sum of partial)",
        chisq = pooledChiSq$statistic,
        df = pooledChiSq$df,
        pvalue = pooledChiSq$pvalue
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: CMH test table (with conditional footnote)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateCMHTable = function(cmhTest, commonOR, commonCI) {
      
      table <- self$results$cmhTestTable
      
      table$setRow(rowKey = 1, values = list(
        test = "Cochran-Mantel-Haenszel",
        statistic = as.numeric(cmhTest$statistic),
        df = as.integer(cmhTest$parameter),
        pvalue = cmhTest$p.value,
        commonOR = if (is.na(commonOR)) "" else commonOR,
        commonCILower = if (is.na(commonCI[1])) "" else commonCI[1],
        commonCIUpper = if (is.na(commonCI[2])) "" else commonCI[2]
      ))
      if (is.na(commonOR)) {
        table$addFootnote(rowKey = 1, col = 'commonOR', paste0(
          "The Mantel-Haenszel common odds ratio is not finite for these data ",
          "(e.g. the same cell is 0 in every stratum)."))
      }
      
      # Add conditional footnote based on significance
      # The CMH test assesses whether the common (Mantel-Haenszel) odds
      # ratio equals 1; it is most powerful when stratum ORs agree in direction
      cmhSig <- isTRUE(cmhTest$p.value < 0.05)
      if (is.na(cmhTest$p.value)) {
        cmhFootnote <- "The CMH test could not be computed for these data."
      } else if (cmhSig) {
        cmhFootnote <- paste0(
          "The CMH test is significant: evidence of conditional dependence, that is, of an ",
          "association between the row and column variables in at least one stratum (at least one ",
          "stratum odds ratio differs from 1). See the homogeneity tests to judge whether the ",
          "common odds ratio is a fair summary of the strata."
        )
      } else {
        cmhFootnote <- paste0(
          "The CMH test is not significant: no evidence of conditional dependence, that is, ",
          "the data are compatible with no association in any stratum (every stratum odds ratio ",
          "equal to 1). Stratum odds ratios pointing in opposite directions can cancel out, ",
          "so check the homogeneity tests and the pooled chi-squared test as well."
        )
      }
      table$setNote('cmhInterpretation', cmhFootnote)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Homogeneity tests table (with conditional footnote)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateHomogeneityTable = function(mhResult, bdtResult, K) {
      
      table <- self$results$homogeneityTable
      
      # Update Woolf homogeneity test row
      table$setRow(rowKey = 1, values = list(
        test = "Woolf",
        statistic = mhResult$statistic,
        df = mhResult$df,
        pvalue = mhResult$pvalue
      ))
      
      # Update Breslow-Day-Tarone test row
      # Check if BDT returned an error
      if (!is.null(bdtResult$error)) {
        table$setRow(rowKey = 2, values = list(
          test = "Breslow-Day-Tarone",
          statistic = NA,
          df = bdtResult$df,
          pvalue = NA
        ))
        table$addFootnote(rowKey = 2, col = 'statistic', 
                          paste0("Could not compute: ", bdtResult$error))
      } else {
        table$setRow(rowKey = 2, values = list(
          test = "Breslow-Day-Tarone",
          statistic = bdtResult$statistic,
          df = bdtResult$df,
          pvalue = bdtResult$pvalue
        ))
      }
      
      # Add conditional footnote based on significance
      # Handle NA p-values gracefully
      mhSig <- !is.na(mhResult$pvalue) && mhResult$pvalue < 0.05
      bdtSig <- !is.na(bdtResult$pvalue) && bdtResult$pvalue < 0.05
      
      if (is.na(mhResult$pvalue) && is.na(bdtResult$pvalue)) {
        homogFootnote <- paste0(
          "Neither homogeneity test could be computed due to degenerate table structure. ",
          "Examine stratum-specific odds ratios individually."
        )
      } else if (mhSig || bdtSig) {
        homogFootnote <- paste0(
          "At least one test is significant: the odds ratios differ across strata ",
          "(heterogeneity). ",
          "The common (Mantel-Haenszel) odds ratio does not adequately summarise the association; ",
          "examine stratum-specific odds ratios."
        )
      } else {
        homogFootnote <- paste0(
          "Neither test is significant: no evidence that the odds ratios differ across strata ",
          "(the tests have limited power with few or small strata, so this does not show that ",
          "they are equal). The common (Mantel-Haenszel) odds ratio can be used as a summary."
        )
      }
      table$setNote('homogeneityInterpretation', homogFootnote)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Add Interpretation Table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateInterpretationTable = function(marginalChiSq, pooledChiSq, cmhTest, 
                                            mhResult, bdtResult, orsAndCIs,
                                            commonOR, commonCI, strataNames,
                                            rowVar, colVar, strataVar, marginalOR = NA) {
      
      # ─────────────────────────────────────────────────────────────────────────
      # Guard against invalid test results
      # ─────────────────────────────────────────────────────────────────────────
      
      if (is.null(cmhTest$p.value) || is.na(cmhTest$p.value) ||
          is.null(mhResult$pvalue) || is.na(mhResult$pvalue) ||
          is.null(bdtResult$pvalue) || is.na(bdtResult$pvalue)) {
        
        table <- self$results$interpretationTable
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
      
      marginalSig <- marginalChiSq$p.value < 0.05
      pooledSig <- pooledChiSq$pvalue < 0.05
      cmhSig <- cmhTest$p.value < 0.05
      mhSig <- mhResult$pvalue < 0.05
      bdtSig <- bdtResult$pvalue < 0.05
      heterogeneitySig <- mhSig || bdtSig
      
      # ─────────────────────────────────────────────────────────────────────────
      # Assess directional reversal
      # ─────────────────────────────────────────────────────────────────────────
      
      anyStratumSigAbove1 <- any(sapply(orsAndCIs, function(x) x$ci_lower > 1))
      anyStratumSigBelow1 <- any(sapply(orsAndCIs, function(x) x$ci_upper < 1))
      trueDirectionalReversal <- anyStratumSigAbove1 && anyStratumSigBelow1
      anyStratumSig <- anyStratumSigAbove1 || anyStratumSigBelow1
      nStrataSigAbove1 <- sum(sapply(orsAndCIs, function(x) x$ci_lower > 1))
      nStrataSigBelow1 <- sum(sapply(orsAndCIs, function(x) x$ci_upper < 1))
      nStrataSig <- nStrataSigAbove1 + nStrataSigBelow1
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 1: Conditional dependence text
      # ─────────────────────────────────────────────────────────────────────────
      
      cmhFailedDueToReversal <- !cmhSig && pooledSig && trueDirectionalReversal
      pairTxt <- paste0("'", rowVar, "' and '", colVar, "'")
      
      # Which test decides conditional dependence: both tests have the same null
      # hypothesis (conditional independence), but the CMH test is aimed at a common
      # odds ratio, so it is used when there is no evidence of heterogeneity (where
      # it is the most powerful test); with significant heterogeneity the pooled
      # chi-squared test, which is not aimed at any particular direction, is used
      pooledPTxt <- if (pooledChiSq$pvalue < 0.001) "p < 0.001" else sprintf("p = %.3f", pooledChiSq$pvalue)
      if (heterogeneitySig) {
        conditionalDependence <- pooledSig
        if (pooledSig && trueDirectionalReversal) {
          stage1Text <- paste0(
            "Pooled chi-squared test significant (", pooledPTxt, "): ", pairTxt, " are associated within strata, but in opposite directions. The CMH test ",
            "(", if (cmhSig) "significant" else "not significant", "), which is designed to detect an association in the same direction in every stratum, ",
            "and the common odds ratio should not be interpreted."
          )
        } else if (pooledSig) {
          stage1Text <- paste0(
            "Pooled chi-squared test significant (", pooledPTxt, "): ", pairTxt, " are associated within strata, but the strength of the association ",
            "differs across them; the CMH test (", if (cmhSig) "significant" else "not significant",
            ") is designed to detect a common odds ratio and should be read with caution."
          )
        } else {
          stage1Text <- paste0(
            "Neither the pooled chi-squared test nor the CMH test is significant: no evidence of an ",
            "association between ", pairTxt, " within strata of '", strataVar, "'."
          )
        }
      } else if (cmhSig) {
        stage1Text <- paste0(
          "CMH test significant (p < 0.05): conditional dependence; ", pairTxt,
          " are associated in at least one stratum of '", strataVar, "'."
        )
        conditionalDependence <- TRUE
      } else {
        stage1Text <- paste0(
          "CMH test not significant (p \u2265 0.05): no evidence of conditional dependence; the data ",
          "are compatible with no association between ", pairTxt, " in any stratum of '", strataVar, "'."
        )
        conditionalDependence <- FALSE
      }
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 2: Homogeneity text
      # ─────────────────────────────────────────────────────────────────────────
      
      if (heterogeneitySig) {
        stage2Text <- "Significant heterogeneity: odds ratios differ across strata."
        if (trueDirectionalReversal) {
          stage2Text <- paste0(stage2Text, " Effects reverse direction across strata.")
        } else if (nStrataSig == 1) {
          stage2Text <- paste0(stage2Text, " One stratum appears to drive this heterogeneity.")
        }
      } else {
        stage2Text <- paste0("No significant heterogeneity: no evidence that the odds ratios differ ",
                             "across strata (the tests cannot show that they are equal).")
      }
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 3: Scenario and recommendation
      # ─────────────────────────────────────────────────────────────────────────
      
      # Change in the odds ratio when Z is controlled for (marginal vs common OR):
      # a change above 10% is the usual change-in-estimate signal of confounding
      fmtOR <- function(x) if (abs(x) >= 0.01) sprintf("%.2f", x) else format(signif(x, 2))
      orChange <- if (is.finite(marginalOR) && is.finite(commonOR) && marginalOR > 0)
        abs(commonOR / marginalOR - 1) else NA
      confounded <- isTRUE(orChange > 0.10)
      changeTxt <- if (is.na(orChange)) "" else paste0(
        " Controlling for '", strataVar, "' changes the odds ratio by ", round(100 * orChange),
        "% (marginal OR ", fmtOR(marginalOR), ", common OR ", fmtOR(commonOR), ")",
        if (confounded) paste0(", so the marginal odds ratio is partly confounded by '", strataVar, "'.")
        else ", too little to suggest confounding.")
      
      if (marginalSig && !conditionalDependence && !heterogeneitySig) {
        scenario <- "Spuriousness"
        recommendation <- paste0(
          "the marginal association is not found once '", strataVar, "' is controlled for, ",
          "which is consistent with '", strataVar, "' confounding it."
        )
      } else if (marginalSig && !cmhSig && heterogeneitySig && !cmhFailedDueToReversal) {
        scenario <- "Effect Modification"
        recommendation <- paste0(
          "the odds ratios differ across strata, so the CMH test finds no common association",
          if (pooledSig) ", while the pooled chi-squared test shows an association within strata" else "",
          "; '", strataVar, "' acts as an effect modifier. Report stratum-specific odds ratios."
        )
      } else if (cmhFailedDueToReversal) {
        scenario <- "Interaction (Directional Reversal)"
        recommendation <- paste0(
          "'", strataVar, "' is an effect modifier and the association reverses direction ",
          "across strata. Report stratum-specific odds ratios."
        )
      } else if (marginalSig && cmhSig && !heterogeneitySig) {
        if (anyStratumSig) {
          scenario <- if (confounded) "Partial Confounding" else "Replication"
          recommendation <- paste0(
            "the association is present overall and within strata, with no evidence that it ",
            "differs across strata.", changeTxt, " ",
            private$.commonORPhrase(commonOR)
          )
        } else {
          scenario <- if (confounded) "Consistent Weak Association, Partly Confounded" else "Consistent Weak Association"
          recommendation <- paste0(
            "no stratum is significant on its own, but the strata point the same way and the ",
            "CMH test, which pools them, detects the association.", changeTxt, " ",
            private$.commonORPhrase(commonOR)
          )
        }
      } else if (marginalSig && cmhSig && heterogeneitySig) {
        if (trueDirectionalReversal) {
          scenario <- "Interaction (Directional Reversal)"
          recommendation <- paste0(
            "'", strataVar, "' is an effect modifier and the association reverses direction ",
            "across strata, so the common odds ratio describes none of them. ",
            "Report stratum-specific odds ratios."
          )
        } else {
          scenario <- "Interaction"
          recommendation <- paste0(
            "'", strataVar, "' acts as an effect modifier: the strength of the association ",
            "differs across strata. Report stratum-specific odds ratios."
          )
        }
      } else if (!marginalSig && cmhSig) {
        scenario <- "Suppression"
        if (!heterogeneitySig) {
          recommendation <- paste0(
            "'", strataVar, "' was masking the association, which appears only within strata. ",
            private$.commonORPhrase(commonOR)
          )
        } else {
          recommendation <- paste0(
            "'", strataVar, "' was masking the association, but the odds ratios differ across ",
            "strata. Report stratum-specific odds ratios."
          )
        }
      } else if (!marginalSig && !conditionalDependence && !heterogeneitySig) {
        scenario <- "No Association"
        recommendation <- paste0(
          "no evidence of an association between ", pairTxt,
          ", either overall or within strata of '", strataVar, "'."
        )
      } else {
        scenario <- "Ambiguous Pattern"
        recommendation <- paste0(
          "no overall association is detected, but the odds ratios differ across strata. ",
          "Examine stratum-specific results carefully."
        )
      }
      
      stage3Text <- paste0(scenario, ": ", recommendation, " (Based on \u03B1 = 0.05)")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Populate table
      # ─────────────────────────────────────────────────────────────────────────
      
      table <- self$results$interpretationTable
      table$setRow(rowKey = 'condIndep', values = list(result = stage1Text))
      table$setRow(rowKey = 'homogeneity', values = list(result = stage2Text))
      table$setRow(rowKey = 'scenario', values = list(result = stage3Text))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Method information
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMethodInfo = function() {
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
      
      # ─────────────────────────────────────────────────────────────────────────
      # Overview
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 0.5em;'>Stratified Analysis Overview</h3>
        <p>Stratified analysis examines the relationship between two dichotomous variables 
        (row variable and column variable) whilst controlling for a third variable (the stratifying variable). 
        By creating separate 2\u00D72 tables for each level of the stratifying variable, this approach 
        allows researchers to assess whether the association between the row and column variables is 
        <strong>confounded</strong> or <strong>modified</strong> by the stratifying variable.</p>
        
        <p>The analysis addresses two fundamental questions:</p>
        <ol>
          <li><strong>Conditional independence:</strong> Is there an association between the two dichotomous variables 
          in at least one stratum (conditional dependence), or in none (conditional independence, every 
          stratum odds ratio equal to 1)?</li>
          <li><strong>Homogeneity:</strong> Is the strength (and direction) of association consistent 
          across strata, or does it vary (effect modification/interaction)?</li>
        </ol>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # CMH Test
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Cochran-Mantel-Haenszel (CMH) Test</h3>
        <p>The CMH test tests the null hypothesis of conditional independence: no association between the 
        row and column variables within any stratum (every stratum odds ratio equals 1). It does not test 
        whether the odds ratios are equal across strata, which is the job of the homogeneity tests below. 
        The test statistic follows a chi-squared distribution with 1 degree of freedom, and it is most 
        powerful when the association has the same direction (ideally a common odds ratio) in every 
        stratum; with opposite directions, the stratum contributions can cancel out.</p>
        
        <p>The Mantel-Haenszel common odds ratio is a weighted average of the stratum-specific 
        odds ratios, with weights b<sub>k</sub>c<sub>k</sub>/n<sub>k</sub> (the product of the two 
        off-diagonal cells divided by the stratum total; Mantel &amp; Haenszel 1959). It remains 
        defined when a stratum has a zero cell and is close to inverse-variance weighting when the 
        odds ratios are near 1. It is a meaningful summary only when the odds ratios are 
        homogeneous across strata.</p>
        
        <p>The interpretation note under each table reads the column variable as the groups being 
        compared and the row variable as the outcome, so it reads most naturally when the independent 
        (explanatory) variable is placed in the columns. The odds ratio itself is the same whichever 
        variable is placed where.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Homogeneity Tests
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Tests for Homogeneity of Odds Ratios</h3>
        <p>Two tests are provided to assess whether the stratum-specific odds ratios are homogeneous:</p>
        
        <p><strong>Woolf Test:</strong> Tests whether the log odds ratios are consistent across 
        strata, using inverse-variance weights to assess departures from homogeneity (Woolf, 1955).</p>
        
        <p><strong>Breslow-Day-Tarone Test:</strong> A more commonly used test that compares observed 
        cell frequencies to those expected under a common odds ratio. The Tarone correction improves 
        the chi-squared approximation, particularly with sparse data.</p>
        
        <p>If either test is significant, this suggests <strong>heterogeneity</strong>: the association 
        between the row and column variables varies across levels of the stratifying variable 
        (effect modification or interaction). In such cases, stratum-specific odds ratios should be 
        reported rather than the common odds ratio.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Pooled Chi-Squared
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Pooled Chi-Squared Test</h3>
        <p>The pooled chi-squared test sums the stratum-specific chi-squared values, providing an 
        alternative test for conditional dependence. Unlike the CMH test, this approach is robust 
        when stratum-specific associations operate in opposite directions (some odds ratios &gt; 1 
        and others &lt; 1), which can cause the CMH test to fail due to cancellation effects.</p>
        
        <p>The two tests are therefore used in turn. When there is no evidence of heterogeneity, the 
        CMH test decides whether there is a conditional association, because it is the more powerful 
        test when the odds ratios share a common value. When the homogeneity tests are significant, the 
        pooled chi-squared test decides instead, because the CMH test is designed to detect an 
        association in the same direction in every stratum and loses power when the odds ratios differ. 
        Both tests have the same null hypothesis, conditional independence; neither tests homogeneity, 
        which is the job of the Woolf and Breslow-Day-Tarone tests. 
        With directional reversal the CMH result is unreliable either way: a non-significant result can 
        reflect cancellation, and a significant one can simply reflect the strata that dominate the 
        pooled estimate, so neither it nor the common odds ratio is interpreted.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Diagnostic Scenarios
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Diagnostic Scenarios</h3>
        <p>The joint pattern of test results indicates different substantive conclusions. 
        Let X denote the row variable, Y denote the column variable, and Z denote the stratifying variable:</p>
        
        <ol style='line-height: 1.8;'>
          <li><strong>Spuriousness:</strong> Marginal association is significant, but the CMH test finds 
          no conditional association and there is no evidence of heterogeneity. This is consistent with the 
          apparent X\u2013Y relationship being due to their separate associations with Z, that is, with Z 
          being a <em>confounder</em>.</li>
          
          <li><strong>Consistent Weak Association:</strong> Both marginal and CMH tests are significant 
          and there is no evidence of heterogeneity, but no individual stratum is significant on its own 
          (all confidence intervals include 1). The strata point the same way and the CMH test, which pools 
          them, detects the association that each stratum is too small to detect alone; pooling consistent 
          strata in this way is what the CMH test is designed to do.</li>
          
          <li><strong>Effect Modification:</strong> Marginal association is significant, but CMH test is 
          non-significant with significant heterogeneity (without true directional reversal). 
          Stratum-specific associations vary in magnitude such that the aggregate effect is nullified. 
          Z is an <em>effect modifier</em>.</li>
          
          <li><strong>Replication:</strong> Both marginal and conditional associations are significant, 
          there is no evidence of heterogeneity, at least one stratum individually confirms the association 
          (confidence interval excluding 1), and controlling for Z changes the odds ratio by no more than 10% 
          (marginal OR compared with the common OR, the usual change-in-estimate criterion). There is no 
          evidence that Z is a confounder or an effect modifier.</li>
          
          <li><strong>Partial Confounding:</strong> As for Replication, but controlling for Z changes the 
          odds ratio by more than 10%: the association is real but its marginal size is partly due to Z, 
          so the common (adjusted) odds ratio should be reported.</li>
          
          <li><strong>Interaction:</strong> Both marginal and conditional associations are significant, 
          but odds ratios are heterogeneous. The X\u2013Y relationship varies in strength across levels 
          of Z. Z is an <em>effect modifier</em>. When the stratum odds ratios point in opposite directions, 
          the CMH result and the common odds ratio describe none of the strata and should not be interpreted.</li>
          
          <li><strong>Interaction (Directional Reversal):</strong> The CMH test is non-significant, but 
          the pooled chi-squared test is significant and stratum-specific odds ratios show genuine 
          directional reversal (at least one confidence interval entirely above 1 and at least one 
          entirely below 1). The CMH test fails in this scenario because opposing effects cancel out. 
          The pooled test correctly identifies conditional dependence. Z is an <em>effect modifier</em> 
          with qualitative interaction.</li>
          
          <li><strong>Suppression:</strong> No marginal association, but a conditional association 
          emerges when stratified (significant CMH). Z was masking a real X\u2013Y relationship. 
          If there is no evidence of heterogeneity, the common odds ratio can be used as a summary; 
          if the odds ratios are heterogeneous, report stratum-specific odds ratios.</li>
          
          <li><strong>No Association:</strong> Neither marginal nor conditional association is significant, 
          and no evidence of heterogeneity. There is no evidence of a relationship between X and Y, 
          regardless of Z.</li>
          
          <li><strong>Ambiguous Pattern:</strong> No overall association is detected, but odds ratios 
          are heterogeneous across strata. This may reflect opposing effects that are individually too 
          weak to detect, insufficient statistical power, or sampling variability. Examine stratum-specific 
          results carefully and consider the study context.</li>
        </ol>
        
        <p style='margin-top: 1em;'><strong>Note on Simpson's Paradox:</strong> In the broader literature 
        (e.g., Azen &amp; Walker, 2021), Simpson's Paradox refers to any situation where marginal and 
        conditional conclusions conflict. This encompasses both <em>Spuriousness</em> (marginal dependence 
        but conditional independence) and <em>Suppression</em> (marginal independence but conditional 
        dependence). The labels used here provide more specific mechanistic descriptions of how the 
        paradox manifests.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Haldane-Anscombe Correction
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Haldane-Anscombe Correction</h3>
        <p>When a 2\u00D72 table contains a zero cell, the odds ratio is either 0 or undefined. 
        The Haldane-Anscombe correction adds 0.5 to all four cells of the table, allowing 
        estimation of the odds ratio and its confidence interval. This correction is automatically 
        applied when needed to the stratum-specific and marginal odds ratios and in Woolf's test 
        (whose inverse-variance weights it enters). The Mantel-Haenszel common odds ratio uses the 
        observed counts; when it is not finite (e.g. the same cell is 0 in every stratum) it is 
        reported as unavailable.</p>
        ")
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Data Preparation Functions
    # ═══════════════════════════════════════════════════════════════════════════
    
    # Sentence used in the interpretation when the common OR summarises the strata
    .commonORPhrase = function(commonOR) {
      if (is.na(commonOR))
        "The common OR is not finite for these data, so report stratum-specific odds ratios."
      else
        paste0("The common OR (", sprintf("%.2f", commonOR), ") can be used as a summary.")
    },
    
    .prepareForestData = function(orsAndCIs, marginalORandCI, commonOR, 
                                  commonCI, strataNames, K) {
      
      # Extract OR estimates and CIs for each stratum
      or_vec <- sapply(orsAndCIs, function(x) x$or)
      ci_lower_vec <- sapply(orsAndCIs, function(x) x$ci_lower)
      ci_upper_vec <- sapply(orsAndCIs, function(x) x$ci_upper)
      
      # Build data frame
      plotData <- data.frame(
        stratum = strataNames,
        or = or_vec,
        ciLower = ci_lower_vec,
        ciUpper = ci_upper_vec,
        type = "Stratum-specific",
        stringsAsFactors = FALSE
      )
      
      # Add marginal OR
      marginal_row <- data.frame(
        stratum = "Marginal (collapsed)",
        or = marginalORandCI$or,
        ciLower = marginalORandCI$ci_lower,
        ciUpper = marginalORandCI$ci_upper,
        type = "Marginal",
        stringsAsFactors = FALSE
      )
      
      # Add common MH OR
      common_row <- data.frame(
        stratum = "Common (MH pooled)",
        or = commonOR,
        ciLower = commonCI[1],
        ciUpper = commonCI[2],
        type = "Common",
        stringsAsFactors = FALSE
      )
      
      plotData <- if (is.na(commonOR)) rbind(plotData, marginal_row) else
        rbind(plotData, marginal_row, common_row)
      
      # Reverse order so first stratum appears at top
      plotData$stratum <- factor(plotData$stratum, levels = rev(plotData$stratum))
      
      return(list(
        data = plotData,
        n_strata = K
      ))
    },
    
    .prepareTrajectoryData = function(orsAndCIs, strataNames) {
      
      # Extract OR estimates and CIs
      or_vec <- sapply(orsAndCIs, function(x) x$or)
      ci_lower_vec <- sapply(orsAndCIs, function(x) x$ci_lower)
      ci_upper_vec <- sapply(orsAndCIs, function(x) x$ci_upper)
      
      # Build data frame with strata as ordered positions
      plotData <- data.frame(
        stratum = strataNames,
        position = 1:length(strataNames),
        or = or_vec,
        log_or = log(or_vec),
        log_ciLower = log(ci_lower_vec),
        log_ciUpper = log(ci_upper_vec),
        stringsAsFactors = FALSE
      )
      
      return(plotData)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Rendering Functions
    # ═══════════════════════════════════════════════════════════════════════════
    
    .forestPlot = function(image, ggtheme, theme, ...) {
      # Retrieve state
      plotState <- image$state
      if (is.null(plotState) || is.null(plotState$data)) {
        return(FALSE)
      }
      plotData <- plotState$data
      
      # First stratum at the top
      plotData$stratum <- factor(as.character(plotData$stratum),
                                 levels = rev(as.character(plotData$stratum)))
      # Significant (CI excludes 1) vs non-significant (CI includes 1)
      plotData$sig <- factor(ifelse(!is.na(plotData$ciLower) & !is.na(plotData$ciUpper) &
                                      (plotData$ciLower > 1 | plotData$ciUpper < 1),
                                    "Significant (CI excludes 1)", "Non-significant (CI includes 1)"),
                             levels = c("Significant (CI excludes 1)", "Non-significant (CI includes 1)"))
      # Circle for strata, square for the marginal OR, diamond for the common OR
      plotData$type <- factor(plotData$type, levels = c("Stratum-specific", "Marginal", "Common"))
      
      # Log-scale range that includes the data and the reference line at 1
      x_min <- min(c(plotData$ciLower, 1), na.rm = TRUE) * 0.8
      x_max <- max(c(plotData$ciUpper, 1), na.rm = TRUE) * 1.25
      # Log breaks: 1-2-5 steps over the range, or finer steps for narrow ranges
      candidates <- sort(unique(c(outer(c(1, 2, 5), 10^(-3:3)))))
      breaks <- candidates[candidates >= x_min & candidates <= x_max]
      if (length(breaks) < 4) {
        fine <- sort(unique(c(outer(c(1, 1.5, 2, 3, 4, 5, 6, 7, 8), 10^(-3:3)))))
        breaks <- fine[fine >= x_min & fine <= x_max]
      }
      
      # jamovi theme first: it carries its own colour scales, which would
      # otherwise replace the manual scales below
      plt <- ggplot2::ggplot(plotData, ggplot2::aes(y = stratum)) + ggtheme +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#34495E", linewidth = 0.7) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = ciLower, xmax = ciUpper, colour = sig),
                               width = 0.25, linewidth = 0.7, orientation = "y", na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(x = or, colour = sig, fill = sig, shape = type, size = type),
                            na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = c("Significant (CI excludes 1)" = "#D4AC0D",
                                                "Non-significant (CI includes 1)" = "#2874A6"),
                                     # list only the categories present in the plot
                                     name = NULL, drop = TRUE,
                                     guide = ggplot2::guide_legend(order = 1)) +
        ggplot2::scale_fill_manual(values = c("Significant (CI excludes 1)" = "#D4AC0D",
                                              "Non-significant (CI includes 1)" = "#2874A6"),
                                   guide = "none", drop = TRUE) +
        ggplot2::scale_shape_manual(values = c("Stratum-specific" = 21, "Marginal" = 22, "Common" = 23),
                                    name = NULL, drop = TRUE,
                                    # filled grey keys, so the shape legend does not suggest a colour
                                    guide = ggplot2::guide_legend(order = 2, override.aes = list(
                                      fill = "grey70", colour = "grey30", size = 3))) +
        ggplot2::scale_size_manual(values = c("Stratum-specific" = 3, "Marginal" = 3.5, "Common" = 4),
                                   guide = "none", drop = TRUE) +
        ggplot2::scale_x_log10(breaks = breaks, labels = format(breaks, drop0trailing = TRUE, trim = TRUE),
                               limits = c(x_min, x_max)) +
        ggplot2::labs(x = "Odds ratio (log scale)", y = NULL, title = "Odds Ratios Across Strata") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom", legend.box = "vertical")
      
      print(plt)
      TRUE
    },
    
    .trajectoryPlot = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      plotData <- image$state
      
      if (is.null(plotData) || nrow(plotData) < 2) {
        # Need at least 2 strata for trajectory
        return(FALSE)
      }
      
      # OR breaks covering the plotted range (and 1)
      yRange <- range(c(0, plotData$log_or, plotData$log_ciLower, plotData$log_ciUpper),
                      finite = TRUE)
      candidates <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 1,
                      2, 4, 10, 20, 50, 100, 200, 500, 1000)
      yBreaks <- candidates[log(candidates) >= yRange[1] - 1e-9 &
                            log(candidates) <= yRange[2] + 1e-9]
      if (length(yBreaks) < 2) yBreaks <- candidates[candidates %in% c(0.5, 1, 2)]
      
      # Create trajectory plot
      plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = position, y = log_or)) +
        
        # Reference line at log(OR) = 0 (OR = 1)
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", 
                            color = "#34495E", linewidth = 0.5) +
        
        # Confidence ribbon
        ggplot2::geom_ribbon(ggplot2::aes(ymin = log_ciLower, ymax = log_ciUpper),
                             fill = "#85C1E9", alpha = 0.3) +
        
        # Trajectory line
        ggplot2::geom_line(color = "#2874A6", linewidth = 1) +
        
        # Points
        ggplot2::geom_point(color = "#2874A6", size = 3, shape = 21, fill = "white") +
        
        # X-axis with stratum labels
        ggplot2::scale_x_continuous(breaks = plotData$position,
                                    labels = plotData$stratum) +
        
        # Y-axis: OR breaks taken from the data range
        ggplot2::scale_y_continuous(
          breaks = log(yBreaks),
          labels = format(yBreaks, drop0trailing = TRUE, trim = TRUE)
        ) +
        
        # Labels
        ggplot2::labs(
          x = "Stratum (ordered)",
          y = "Odds Ratio (log scale)",
          title = "Odds Ratio Trajectory Across Ordered Strata"
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