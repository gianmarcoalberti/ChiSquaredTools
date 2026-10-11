# This file is a generated template, your changes will not be overwritten

#' @export
chisqassocClass <- R6::R6Class(
    "chisqassocClass",
    inherit = chisqassocBase,
    private = list(
        
        .init = function() {
            
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
            
            # --- Get factor levels from data to set up table structures ---
            rowVar <- self$options$rows
            colVar <- self$options$cols
            data <- self$data
            
            # Get levels (handles both factor and non-factor)
            if (is.factor(data[[rowVar]])) {
                row_levels <- levels(data[[rowVar]])
            } else {
                row_levels <- levels(as.factor(data[[rowVar]]))
            }
            
            if (is.factor(data[[colVar]])) {
                col_levels <- levels(data[[colVar]])
            } else {
                col_levels <- levels(as.factor(data[[colVar]]))
            }
            
            I <- length(row_levels)
            J <- length(col_levels)
            is_2x2 <- (I == 2 && J == 2)
            
            # --- Set up crosstabTable structure ---
            crosstab <- self$results$crosstabTable
            crosstab$setTitle(paste0(rowVar, " × ", colVar))
            
            crosstab$addColumn(
                name = 'rowname',
                title = rowVar,
                type = 'text',
                combineBelow = FALSE
            )
            
            for (j in 1:J) {
                crosstab$addColumn(
                    name = paste0("col", j),
                    title = col_levels[j],
                    type = 'integer',
                    superTitle = colVar
                )
            }
            
            crosstab$addColumn(
                name = 'rowtotal',
                title = 'Total',
                type = 'integer'
            )
            
            # Add empty rows for crosstab
            for (i in 1:I) {
                crosstab$addRow(rowKey = i, values = list(rowname = row_levels[i]))
            }
            crosstab$addRow(rowKey = 'total', values = list(rowname = 'Total'))
            
            # --- Set up chisqMaxTable structure (if enabled) ---
            chisqMaxTab <- self$results$chisqMaxTable
            chisqMaxTab$setTitle(paste0("Chi-Squared-Maximising Table: ", rowVar, " × ", colVar))
            
            chisqMaxTab$addColumn(
                name = 'rowname',
                title = rowVar,
                type = 'text',
                combineBelow = FALSE
            )
            
            for (j in 1:J) {
                chisqMaxTab$addColumn(
                    name = paste0("col", j),
                    title = col_levels[j],
                    type = 'integer',
                    superTitle = colVar
                )
            }
            
            # Add rowtotal column
            chisqMaxTab$addColumn(
                name = 'rowtotal',
                title = 'Total',
                type = 'integer'
            )
            
            # Add empty rows including Total row
            for (i in 1:I) {
                chisqMaxTab$addRow(rowKey = i, values = list(rowname = row_levels[i]))
            }
            chisqMaxTab$addRow(rowKey = 'total', values = list(rowname = 'Total'))
            
            # --- Set up standardisedTable structure (if enabled) ---
            standTab <- self$results$standardisedTable
            standTab$setTitle(paste0("Standardised Table: ", rowVar, " × ", colVar))
            
            standTab$addColumn(
                name = 'rowname',
                title = rowVar,
                type = 'text',
                combineBelow = FALSE
            )
            
            for (j in 1:J) {
                standTab$addColumn(
                    name = paste0("col", j),
                    title = col_levels[j],
                    type = 'number',
                    superTitle = colVar
                )
            }
            
            # Add rowtotal column
            standTab$addColumn(
                name = 'rowtotal',
                title = 'Total',
                type = 'number'
            )
            
            # Add empty rows including Total row
            for (i in 1:I) {
                standTab$addRow(rowKey = i, values = list(rowname = row_levels[i]))
            }
            standTab$addRow(rowKey = 'total', values = list(rowname = 'Total'))
            
            # --- Set up resultsTable rows in correct order ---
            # Pre-add ALL possible rows based on options AND table size (is_2x2)
            table <- self$results$resultsTable
            
            # 2×2-only chi-squared-based measures (first)
            if (is_2x2) {
                if (self$options$phi)
                    table$addRow(rowKey = 'phi', values = list(measure = ''))
                if (self$options$phiSigned)
                    table$addRow(rowKey = 'phiSigned', values = list(measure = ''))
                if (self$options$phiCorrected)
                    table$addRow(rowKey = 'phiCorrected', values = list(measure = ''))
                if (self$options$coleC7)
                    table$addRow(rowKey = 'coleC7', values = list(measure = ''))
                if (self$options$zysnoPhi)
                    table$addRow(rowKey = 'zysnoPhi', values = list(measure = ''))
            }
            
            # Universal chi-squared-based measures (any table size)
            if (self$options$contingencyC)
                table$addRow(rowKey = 'contingencyC', values = list(measure = ''))
            if (self$options$cAdjusted)
                table$addRow(rowKey = 'cAdjusted', values = list(measure = ''))
            if (self$options$cCorrected)
                table$addRow(rowKey = 'cCorrected', values = list(measure = ''))
            if (self$options$cramersV)
                table$addRow(rowKey = 'cramersV', values = list(measure = ''))
            if (self$options$vCorrected)
                table$addRow(rowKey = 'vCorrected', values = list(measure = ''))
            if (self$options$vStandardised)
                table$addRow(rowKey = 'vStandardised', values = list(measure = ''))
            if (self$options$vBiasCorrected)
                table$addRow(rowKey = 'vBiasCorrected', values = list(measure = ''))
            if (self$options$cohenW)
                table$addRow(rowKey = 'cohenW', values = list(measure = ''))
            
            # Distance-based measures (any table size)
            if (self$options$wHat)
                table$addRow(rowKey = 'wHat', values = list(measure = ''))
            if (self$options$wHatCorrected)
                table$addRow(rowKey = 'wHatCorrected', values = list(measure = ''))
            if (self$options$sakoda)
                table$addRow(rowKey = 'sakoda', values = list(measure = ''))
            if (self$options$sakodaCorrected)
                table$addRow(rowKey = 'sakodaCorrected', values = list(measure = ''))
            
            # 2×2-only margin-free measures (after distance-based, before PRE)
            if (is_2x2 && self$options$marginFree) {
                table$addRow(rowKey = 'oddsRatio', values = list(measure = ''))
                table$addRow(rowKey = 'yulesQ', values = list(measure = ''))
                table$addRow(rowKey = 'yulesY', values = list(measure = ''))
            }
            
            # PRE measures (any table size - always last)
            if (self$options$gkLambda) {
                table$addRow(rowKey = 'lambdaRow', values = list(measure = ''))
                table$addRow(rowKey = 'lambdaCol', values = list(measure = ''))
                table$addRow(rowKey = 'lambdaSym', values = list(measure = ''))
            }
            if (self$options$gkLambdaCorrected) {
                table$addRow(rowKey = 'lambdaCorrRow', values = list(measure = ''))
                table$addRow(rowKey = 'lambdaCorrCol', values = list(measure = ''))
                table$addRow(rowKey = 'lambdaCorrSym', values = list(measure = ''))
            }
            if (self$options$gkTau) {
                table$addRow(rowKey = 'tauRow', values = list(measure = ''))
                table$addRow(rowKey = 'tauCol', values = list(measure = ''))
            }
            
            # --- Set up sakodaLocalTable structure (if enabled) ---
            sakodaTab <- self$results$sakodaLocalTable
            sakodaTab$setTitle(paste0("Sakoda's D (Local) / PEM: ", rowVar, " × ", colVar))
            
            sakodaTab$addColumn(
                name = 'rowname',
                title = rowVar,
                type = 'text',
                combineBelow = FALSE
            )
            
            for (j in 1:J) {
                sakodaTab$addColumn(
                    name = paste0("col", j),
                    title = col_levels[j],
                    type = 'number',
                    superTitle = colVar
                )
            }
            
            # Add empty rows
            for (i in 1:I) {
                sakodaTab$addRow(rowKey = i, values = list(rowname = row_levels[i]))
            }
            
            # --- Set up thresholdsTable rows ---
            threshTable <- self$results$thresholdsTable
            
            # RxC measures
            if (self$options$contingencyC) {
                threshTable$addRow(rowKey = 'contingencyC', values = list(measure = ''))
            }
            if (self$options$cAdjusted) {
                threshTable$addRow(rowKey = 'cAdjusted', values = list(measure = ''))
            }
            if (self$options$cCorrected) {
                threshTable$addRow(rowKey = 'cCorrected', values = list(measure = ''))
            }
            if (self$options$cramersV) {
                threshTable$addRow(rowKey = 'cramersV', values = list(measure = ''))
            }
            if (self$options$vCorrected) {
                threshTable$addRow(rowKey = 'vCorrected', values = list(measure = ''))
            }
            if (self$options$vStandardised) {
                threshTable$addRow(rowKey = 'vStandardised', values = list(measure = ''))
            }
            if (self$options$vBiasCorrected) {
                threshTable$addRow(rowKey = 'vBiasCorrected', values = list(measure = ''))
            }
            if (self$options$cohenW) {
                threshTable$addRow(rowKey = 'cohenW', values = list(measure = ''))
            }
            if (self$options$wHat) {
                threshTable$addRow(rowKey = 'wHat', values = list(measure = ''))
            }
            if (self$options$wHatCorrected) {
                threshTable$addRow(rowKey = 'wHatCorrected', values = list(measure = ''))
            }
            if (self$options$sakoda) {
                threshTable$addRow(rowKey = 'sakoda', values = list(measure = ''))
            }
            if (self$options$sakodaCorrected) {
                threshTable$addRow(rowKey = 'sakodaCorrected', values = list(measure = ''))
            }
            if (self$options$sakodaLocal) {
                threshTable$addRow(rowKey = 'sakodaLocal', values = list(measure = ''))
            }
            
            # --- Set up pairwise comparison tables structure (for RxC tables only) ---
            # Only set up if marginFree is selected AND table is larger than 2x2
            if (self$options$marginFree && !(I == 2 && J == 2)) {
                # Calculate number of pairwise comparisons
                # For each pair of rows (i,j) and pair of columns (k,l)
                n_row_pairs <- choose(I, 2)  # Number of ways to choose 2 rows from I
                n_col_pairs <- choose(J, 2)  # Number of ways to choose 2 columns from J
                n_comparisons <- n_row_pairs * n_col_pairs
                
                # Pre-add rows to pairwise OR table
                pairORTable <- self$results$pairwiseORTable
                comparison_idx <- 0
                for (i in 1:(I - 1)) {
                    for (j in (i + 1):I) {
                        for (k in 1:(J - 1)) {
                            for (l in (k + 1):J) {
                                comparison_idx <- comparison_idx + 1
                                rowKey <- paste0("or_", i, "_", j, "_", k, "_", l)
                                pairORTable$addRow(rowKey = rowKey, values = list(comparison = ''))
                            }
                        }
                    }
                }
                
                # Pre-add rows to pairwise Q table
                pairQTable <- self$results$pairwiseQTable
                comparison_idx <- 0
                for (i in 1:(I - 1)) {
                    for (j in (i + 1):I) {
                        for (k in 1:(J - 1)) {
                            for (l in (k + 1):J) {
                                comparison_idx <- comparison_idx + 1
                                rowKey <- paste0("q_", i, "_", j, "_", k, "_", l)
                                pairQTable$addRow(rowKey = rowKey, values = list(comparison = ''))
                            }
                        }
                    }
                }
                
                # Pre-add rows to pairwise Y table
                pairYTable <- self$results$pairwiseYTable
                comparison_idx <- 0
                for (i in 1:(I - 1)) {
                    for (j in (i + 1):I) {
                        for (k in 1:(J - 1)) {
                            for (l in (k + 1):J) {
                                comparison_idx <- comparison_idx + 1
                                rowKey <- paste0("y_", i, "_", j, "_", k, "_", l)
                                pairYTable$addRow(rowKey = rowKey, values = list(comparison = ''))
                            }
                        }
                    }
                }
            }
        },
        
        .run = function() {
            
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
            
            rowVar <- self$options$rows
            colVar <- self$options$cols
            countsVar <- self$options$counts
            
            data <- self$data
            
            if (!is.factor(data[[rowVar]])) {
                data[[rowVar]] <- as.factor(data[[rowVar]])
            }
            if (!is.factor(data[[colVar]])) {
                data[[colVar]] <- as.factor(data[[colVar]])
            }
            
            # Build contingency table safely (shared helper in utils.R)
            contingency_table <- .ctBuildTable(data, c(rowVar, colVar), countsVar)
            .ctRejectTooFewLevels(contingency_table, rowVar, colVar)
            .ctRejectEmptyMargins(contingency_table, rowVar, colVar)
            
            # Populate observed contingency table FIRST
            private$.populateCrosstab(contingency_table, rowVar, colVar)
            
            nr <- nrow(contingency_table)
            nc <- ncol(contingency_table)
            n <- sum(contingency_table)
            is_2x2 <- (nr == 2 && nc == 2)
            
            row_totals <- rowSums(contingency_table)
            col_totals <- colSums(contingency_table)
            expected <- outer(row_totals, col_totals) / n
            chisq_obs <- sum((contingency_table - expected)^2 / expected)
            df <- (nr - 1) * (nc - 1)
            
            # Calculate chi-sq max if needed (for corrected measures)
            chisq_max_table <- NULL
            chisq_max_value <- NULL
            phi_max <- NULL
            c_max <- NULL
            v_max <- NULL
            
            # Calculate max values if:
            # 1. User wants to see the chi-sq max table
            # 2. Any corrected measure is requested
            # 3. Any uncorrected measure is requested: its effect-size label
            #    (Tier 2) is scaled by the maximum, whether or not the
            #    thresholds table is shown
            needs_max_for_thresholds <- (self$options$phi || 
                     self$options$phiSigned || 
                     self$options$contingencyC || 
                     self$options$cramersV ||
                     self$options$wHat ||
                     self$options$sakoda)
            
            if (self$options$showChisqMax || 
                self$options$phiCorrected || 
                self$options$cCorrected || 
                self$options$vCorrected ||
                self$options$wHatCorrected ||
                self$options$sakodaCorrected ||
                needs_max_for_thresholds) {
                max_result <- private$.max_chisq(row_totals, col_totals)
                chisq_max_table <- max_result$max_table
                chisq_max_value <- max_result$max_chisq
                
                # Calculate max values for display
                phi_max <- sqrt(chisq_max_value / n)
                c_max <- sqrt(chisq_max_value / (chisq_max_value + n))
                k <- min(nr - 1, nc - 1)
                v_max <- sqrt(chisq_max_value / (n * k))
            }
            
            # Populate standardised table AFTER observed contingency table
            standardised_table <- NULL
            if (self$options$showStandardised || self$options$vStandardised) {
                std_result <- private$.standardise_crosstabs(
                    contingency_table,
                    marginal.type = "average"
                )
                standardised_table <- std_result$table.stand
                
                if (self$options$showStandardised) {
                    private$.populateStandardisedTable(standardised_table, rowVar, colVar)
                    self$results$standardisedTable$setNote(
                        key = "standardised_explanation",
                        note = "In the standardised table, counts are rescaled (via Iterative Proportional Fitting) so that all rows have equal totals (N/r) and all columns have equal totals (N/c).",
                        init = FALSE
                    )
                }
            }
            
            # Populate chi-sq max table AFTER standardised table
            if (self$options$showChisqMax && !is.null(chisq_max_table)) {
                private$.populateChisqMaxTable(chisq_max_table, rowVar, colVar)
                
                ratio_value <- chisq_obs / chisq_max_value
                sqrt_ratio <- sqrt(ratio_value)
                
                # Determine which corrected measure the sqrt corresponds to
                if (is_2x2) {
                    measure_name <- "Phi corrected"
                } else {
                    measure_name <- "Cramér's V corrected"
                }
                
                self$results$chisqMaxTable$setNote(
                    key = "chisq_max_explanation",
                    note = sprintf(
                        "Observed χ² = %.3f; Maximum possible χ² = %.3f; Ratio = %.3f; √Ratio = %.3f (%s)",
                        chisq_obs,
                        chisq_max_value,
                        ratio_value,
                        sqrt_ratio,
                        measure_name
                    ),
                    init = FALSE
                )
            }
            
            # Populate Sakoda local table if requested
            if (self$options$sakodaLocal) {
                private$.populateSakodaLocalTable(contingency_table, rowVar, colVar)
            }
            
            # Populate results table (effect sizes)
            private$.populateResultsTable(
                contingency_table, 
                expected, 
                chisq_obs, 
                df, 
                n, 
                nr, 
                nc, 
                is_2x2,
                chisq_max_table,
                chisq_max_value,
                standardised_table,
                phi_max,
                c_max,
                v_max
            )
            
            # Manage pairwise comparison tables (RxC only)
            # YAML controls base visibility via (marginFree)
            # R code only HIDES for 2x2 tables and populates for larger tables
            if (self$options$marginFree) {
                if (is_2x2) {
                    # Hide tables for 2x2 (marginFree measures shown in main results table)
                    self$results$pairwiseORTable$setVisible(FALSE)
                    self$results$pairwiseQTable$setVisible(FALSE)
                    self$results$pairwiseYTable$setVisible(FALSE)
                } else {
                    # RxC table: populate pairwise tables (deleteRows is inside each function)
                    private$.populatePairwiseORTable(contingency_table, nr, nc)
                    private$.populatePairwiseQTable(contingency_table, nr, nc)
                    private$.populatePairwiseYTable(contingency_table, nr, nc)
                }
            }
            
            if (self$options$showMethodInfo) {
                private$.populateMethodInfo()
            }
            
            if (self$options$showThresholds) {
                private$.populateThresholdsTable(nr, nc, is_2x2, contingency_table, expected, phi_max, c_max, v_max, chisq_max_table)
            }
            
            # Check for unilateral association warning (2x2 tables only)
            # Only warn when Phi/V selected and pattern is unilateral (suggest Q/OR instead)
            if (is_2x2) {
                # Check if any phi-like measure is selected (including Cramér's V for 2x2)
                phi_like_selected <- self$options$phi || self$options$phiSigned || 
                    self$options$phiCorrected || self$options$cramersV || 
                    self$options$vCorrected || self$options$vBiasCorrected
                
                if (phi_like_selected) {
                    # Calculate row-wise odds
                    odds_row1 <- contingency_table[1,1] / contingency_table[1,2]
                    odds_row2 <- contingency_table[2,1] / contingency_table[2,2]
                    
                    # Calculate column-wise odds
                    odds_col1 <- contingency_table[1,1] / contingency_table[2,1]
                    odds_col2 <- contingency_table[1,2] / contingency_table[2,2]
                    
                    # Handle zero cells safely
                    all_odds_valid <- !is.na(odds_row1) && !is.infinite(odds_row1) && 
                        !is.na(odds_row2) && !is.infinite(odds_row2) &&
                        !is.na(odds_col1) && !is.infinite(odds_col1) && 
                        !is.na(odds_col2) && !is.infinite(odds_col2) &&
                        odds_row1 > 0 && odds_row2 > 0 && 
                        odds_col1 > 0 && odds_col2 > 0
                    
                    if (all_odds_valid) {
                        # Calculate ratios for both directions
                        min_row_odds <- min(odds_row1, odds_row2)
                        max_row_odds <- max(odds_row1, odds_row2)
                        row_ratio <- max_row_odds / min_row_odds
                        
                        min_col_odds <- min(odds_col1, odds_col2)
                        max_col_odds <- max(odds_col1, odds_col2)
                        col_ratio <- max_col_odds / min_col_odds
                        
                        # Check if either direction has ANY odd close to 1.0 (balanced)
                        row_has_balanced <- (odds_row1 >= 0.5 && odds_row1 <= 2.0) || 
                            (odds_row2 >= 0.5 && odds_row2 <= 2.0)
                        col_has_balanced <- (odds_col1 >= 0.5 && odds_col1 <= 2.0) || 
                            (odds_col2 >= 0.5 && odds_col2 <= 2.0)
                        
                        # Unilateral pattern: large ratio AND one direction has balanced odds
                        is_unilateral <- (row_has_balanced || col_has_balanced) && 
                            (row_ratio > 3.0 || col_ratio > 3.0)
                        
                        if (is_unilateral) {
                            warning_text <- paste0(
                                "The input table exhibits non-symmetric marginal distributions with individual odds suggesting ",
                                "unilateral rather than bilateral association. ",
                                "Phi (=Cramér's V in 2×2 tables) is sensitive to bilateral (reciprocal) associations ",
                                "but may underrepresent unilateral patterns. ",
                                "Consider using Yule's Q or the Odds Ratio, bearing in mind that the choice between measures should be informed by the type of association that is of analytical interest ",
                                "(Kendall & Stuart 1961; Buchanan 1974; Bruner 1976; Bonett & Price 2007; von Eye & Mun 2003; Mueller & Schuessler 1961; Alberti 2024)."
                            )
                            self$results$crosstabTable$setNote(
                                key = "unilateral_warning",
                                note = warning_text,
                                init = FALSE
                            )
                        }
                    }
                }
            }
        },
        
        # *** Helper functions for Cramér's V CI (Smithson 2003) ***
        # Confidence limits for the noncentrality parameter of chi-squared
        # (Smithson 2003): find ncp such that P(X <= chival | df, ncp) equals
        # 1 - alpha/2 (lower limit) or alpha/2 (upper limit). When the
        # observed chi-squared lies below the corresponding central quantile,
        # the limit is 0. The search interval is widened until it brackets the
        # root (the former fixed brackets failed for small chi-squared values).
        .ncpLimit = function(chival, df, p) {
            if (is.na(chival) || is.null(chival) || chival <= 0 || df <= 0) return(0)
            if (stats::pchisq(chival, df, ncp = 0) <= p) return(0)
            upper <- max(10, 2 * chival)
            while (stats::pchisq(chival, df, ncp = upper) > p && upper < 1e7) upper <- upper * 2
            stats::uniroot(function(l) stats::pchisq(chival, df, ncp = l) - p,
                           interval = c(0, upper), tol = 1e-10)$root
        },
        
        .lochi = function(chival, df, conf) {
            # Lower noncentrality limit
            private$.ncpLimit(chival, df, 1 - (1 - conf) / 2)
        },
        
        .hichi = function(chival, df, conf) {
            # Upper noncentrality limit
            private$.ncpLimit(chival, df, (1 - conf) / 2)
        },
        
        .populateCrosstab = function(contingency_table, rowVar, colVar) {
            table <- self$results$crosstabTable
            
            I <- nrow(contingency_table)
            J <- ncol(contingency_table)
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            # Populate rows using setRow (structure already created in .init)
            for (i in 1:I) {
                row_values <- list(rowname = row_names[i])
                for (j in 1:J) {
                    row_values[[paste0("col", j)]] <- contingency_table[i, j]
                }
                row_values[['rowtotal']] <- sum(contingency_table[i, ])
                table$setRow(rowKey = i, values = row_values)
            }
            
            # Set total row
            total_values <- list(rowname = 'Total')
            for (j in 1:J) {
                total_values[[paste0("col", j)]] <- sum(contingency_table[, j])
            }
            total_values[['rowtotal']] <- sum(contingency_table)
            table$setRow(rowKey = 'total', values = total_values)
        },
        
        .populateChisqMaxTable = function(chisq_max_table, rowVar, colVar) {
            table <- self$results$chisqMaxTable
            
            I <- nrow(chisq_max_table)
            J <- ncol(chisq_max_table)
            row_names <- rownames(chisq_max_table)
            
            # Populate rows using setRow (structure already created in .init)
            for (i in 1:I) {
                row_values <- list(rowname = row_names[i])
                for (j in 1:J) {
                    row_values[[paste0("col", j)]] <- chisq_max_table[i, j]
                }
                row_values[['rowtotal']] <- sum(chisq_max_table[i, ])
                table$setRow(rowKey = i, values = row_values)
            }
            
            # Set total row
            total_values <- list(rowname = 'Total')
            for (j in 1:J) {
                total_values[[paste0("col", j)]] <- sum(chisq_max_table[, j])
            }
            total_values[['rowtotal']] <- sum(chisq_max_table)
            table$setRow(rowKey = 'total', values = total_values)
        },
        
        .populateStandardisedTable = function(standardised_table, rowVar, colVar) {
            table <- self$results$standardisedTable
            
            I <- nrow(standardised_table)
            J <- ncol(standardised_table)
            row_names <- rownames(standardised_table)
            
            # Populate rows using setRow (structure already created in .init)
            for (i in 1:I) {
                row_values <- list(rowname = row_names[i])
                for (j in 1:J) {
                    row_values[[paste0("col", j)]] <- standardised_table[i, j]
                }
                row_values[['rowtotal']] <- sum(standardised_table[i, ])
                table$setRow(rowKey = i, values = row_values)
            }
            
            # Set total row
            total_values <- list(rowname = 'Total')
            for (j in 1:J) {
                total_values[[paste0("col", j)]] <- sum(standardised_table[, j])
            }
            total_values[['rowtotal']] <- sum(standardised_table)
            table$setRow(rowKey = 'total', values = total_values)
        },
        
        .populateSakodaLocalTable = function(contingency_table, rowVar, colVar) {
            # Compute Sakoda's D_ij (local index) - mathematically equivalent to PEM
            table <- self$results$sakodaLocalTable
            
            N_matrix <- as.matrix(contingency_table)
            I <- nrow(N_matrix)
            J <- ncol(N_matrix)
            N <- sum(N_matrix)
            
            N_iplus <- rowSums(N_matrix)
            N_plusj <- colSums(N_matrix)
            E_ij <- outer(N_iplus, N_plusj) / N
            
            # Compute D_ij for each cell (Sakoda 1981, equivalent to Cibois PEM as proportion)
            D_ij <- matrix(NA, nrow = I, ncol = J)
            
            for (i in 1:I) {
                for (j in 1:J) {
                    n_ij <- N_matrix[i, j]
                    e_ij <- E_ij[i, j]
                    
                    if (n_ij >= e_ij) {
                        # Positive deviation (attraction)
                        max_ij <- min(N_iplus[i], N_plusj[j])
                        if (max_ij > e_ij) {
                            D_ij[i, j] <- (n_ij - e_ij) / (max_ij - e_ij)
                        } else {
                            D_ij[i, j] <- 0
                        }
                    } else {
                        # Negative deviation (repulsion)
                        min_ij <- max(0, N_iplus[i] + N_plusj[j] - N)
                        D_ij[i, j] <- -((e_ij - n_ij) / (e_ij - min_ij))
                    }
                }
            }
            
            dimnames(D_ij) <- dimnames(N_matrix)
            row_names <- rownames(D_ij)
            
            # Populate rows with color coding using setRow (structure already created in .init)
            for (i in 1:I) {
                row_values <- list(rowname = row_names[i])
                for (j in 1:J) {
                    value <- D_ij[i, j]
                    formatted_value <- sprintf("%.3f", value)
                    
                    # Color code: red for strong positive (>=0.5), blue for strong negative (<=-0.5)
                    if (value >= 0.5) {
                        formatted_value <- paste0("<span style='display: block; text-align: center; color: red;'>", formatted_value, "</span>")
                    } else if (value <= -0.5) {
                        formatted_value <- paste0("<span style='display: block; text-align: center; color: blue;'>", formatted_value, "</span>")
                    } else {
                        formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
                    }
                    
                    row_values[[paste0("col", j)]] <- formatted_value
                }
                table$setRow(rowKey = i, values = row_values)
            }
            
            # Add table note
            self$results$sakodaLocalTable$setNote(
                key = "interpretation",
                note = "−1 (maximum repulsion) through 0 (independence) to +1 (maximum attraction). Empirical thresholds: exceptional attraction (≥0.5); exceptional repulsion (≤−0.5). Thresholds based on Cibois's (1993) thresholds for PEM (≥50%). For confidence intervals, see the Post-hoc Analysis facility. (Sakoda 1981; Cibois 1993)"
            )
        },
        
        .populateResultsTable = function(contingency_table, expected, chisq_obs, df, n, nr, nc, is_2x2,
                                         chisq_max_table, chisq_max_value, standardised_table,
                                         phi_max, c_max, v_max) {
            
            table <- self$results$resultsTable
            
            # Note: All rows are pre-added in .init() - we only use setRow() here
            # to populate values. This prevents table flickering.
            
            # Compute W-hat and Sakoda max values if chi-sq max table is available
            w_hat_max <- NULL
            sakoda_max <- NULL
            
            # Compute W-hat and Sakoda max values if chi-sq max table is available
            w_hat_max <- NULL
            sakoda_max <- NULL
            if (!is.null(chisq_max_table)) {
                w_hat_max <- private$.wHat(chisq_max_table)
                sakoda_max <- private$.sakoda(chisq_max_table)
            }
            conf_level <- self$options$confLevel / 100  # Convert from percentage to proportion
            B <- self$options$bootstrapReps
            
            # ═══════════════════════════════════════════════════════════════════════════
            # Generate bootstrap data ONCE if any corrected measure with bootstrap CI is selected
            # ═══════════════════════════════════════════════════════════════════════════
            
            boot_data <- NULL
            needs_bootstrap <- self$options$phiCorrected || 
                self$options$cCorrected || 
                self$options$vCorrected ||
                self$options$wHatCorrected ||
                self$options$sakodaCorrected ||
                self$options$wHat ||
                self$options$sakoda ||
                self$options$cAdjusted
            
            if (needs_bootstrap) {
                set.seed(self$options$seed)
                boot_data <- private$.generateBootstrapData(contingency_table, B)
            }
            
            # Track if any rows were actually populated
            rows_populated <- FALSE
            
            # PHI - WITH CI (Smithson noncentral chi-square method)
            if (self$options$phi) {
                if (is_2x2) {
                    phi_val <- sqrt(chisq_obs / n)
                    
                    # Smithson (2003) noncentral chi-square CI
                    delta_lower <- private$.lochi(chisq_obs, df, conf_level)
                    delta_upper <- private$.hichi(chisq_obs, df, conf_level)
                    # limits for lambda (Smithson 2003) converted with lambda = N * w^2 (Cohen 1988)
                    phi_lower <- sqrt(delta_lower / n)
                    phi_upper <- sqrt(delta_upper / n)
                    
                    effect <- private$.interpretChiSqEffect(phi_val, "phi", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    table$setRow(rowKey = 'phi', values = list(
                        measure = "Phi (\u03C6)",
                        value = phi_val,
                        lowerCI = phi_lower,
                        upperCI = phi_upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                }
            }
            
            # PHI SIGNED - WITH CI (Smithson method with sign applied)
            if (self$options$phiSigned) {
                if (is_2x2) {
                    a <- contingency_table[1, 1]
                    b <- contingency_table[1, 2]
                    c <- contingency_table[2, 1]
                    d <- contingency_table[2, 2]
                    
                    cross_product <- (a * d) - (b * c)
                    phi_unsigned <- sqrt(chisq_obs / n)
                    phi_signed_val <- sign(cross_product) * phi_unsigned
                    
                    delta_lower <- private$.lochi(chisq_obs, df, conf_level)
                    delta_upper <- private$.hichi(chisq_obs, df, conf_level)
                    # limits for lambda (Smithson 2003) converted with lambda = N * w^2 (Cohen 1988)
                    phi_mag_lower <- sqrt(delta_lower / n)
                    phi_mag_upper <- sqrt(delta_upper / n)
                    
                    if (cross_product >= 0) {
                        phi_lower <- phi_mag_lower
                        phi_upper <- phi_mag_upper
                    } else {
                        phi_lower <- -phi_mag_upper
                        phi_upper <- -phi_mag_lower
                    }
                    
                    effect <- private$.interpretChiSqEffect(abs(phi_signed_val), "phi_signed", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    table$setRow(rowKey = 'phiSigned', values = list(
                        measure = "Phi signed (\u03C6)",
                        value = phi_signed_val,
                        lowerCI = phi_lower,
                        upperCI = phi_upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                }
            }
            
            # PHI CORRECTED - WITH BOOTSTRAP CI
            if (self$options$phiCorrected) {
                if (is_2x2 && !is.null(chisq_max_value)) {
                    phi_val <- sqrt(chisq_obs / n)
                    phi_corr <- phi_val / phi_max
                    
                    boot_ci <- private$.bootci_corrected(boot_data, phi_corr, phi_max, conf_level,
                                                         stat_function = NULL, use_chisq_ratio = TRUE)
                    
                    effect <- private$.interpretChiSqEffect(phi_corr, "phi_corrected", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    measure_text <- sprintf("Phi corrected (\u03C6<sub>max</sub> = %.3f)", phi_max)
                    
                    table$setRow(rowKey = 'phiCorrected', values = list(
                        measure = measure_text,
                        value = phi_corr,
                        lowerCI = boot_ci$lower,
                        upperCI = boot_ci$upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                }
            }
            
            # COLE'S C7 (2x2 only) - NO CI
            if (self$options$coleC7) {
                if (is_2x2) {
                    cole_val <- private$.coleC7(contingency_table)
                    effect <- private$.interpretMarginFreeEffect(cole_val, "cole_c7")
                    
                    table$setRow(rowKey = 'coleC7', values = list(
                        measure = "Cole's C7",
                        value = cole_val,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                }
            }
            
            # ZYSNO'S PHI* (2x2 only) - NO CI
            if (self$options$zysnoPhi) {
                if (is_2x2) {
                    zysno_val <- private$.zysnoPhi(contingency_table)
                    effect <- private$.interpretMarginFreeEffect(zysno_val, "zysno_phi")
                    
                    table$setRow(rowKey = 'zysnoPhi', values = list(
                        measure = "Zysno's φ*",
                        value = zysno_val,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                }
            }
            
            # CONTINGENCY COEFFICIENT C - WITH CI (noncentrality limits, Smithson 2003; lambda = N w^2)
            if (self$options$contingencyC) {
                C_val <- sqrt(chisq_obs / (chisq_obs + n))
                
                delta_lower <- private$.lochi(chisq_obs, df, conf_level)
                delta_upper <- private$.hichi(chisq_obs, df, conf_level)
                # limits for lambda (Smithson 2003) converted with lambda = N * w^2 (Cohen 1988);
                # C = sqrt(w^2 / (1 + w^2)) = sqrt(lambda / (lambda + N))
                C_lower <- sqrt(delta_lower / (delta_lower + n))
                C_upper <- sqrt(delta_upper / (delta_upper + n))
                
                effect <- private$.interpretChiSqEffect(C_val, "contingency_c", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                table$setRow(rowKey = 'contingencyC', values = list(
                    measure = "Contingency Coefficient (C)",
                    value = C_val,
                    lowerCI = C_lower,
                    upperCI = C_upper,
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # CONTINGENCY COEFFICIENT C ADJUSTED - WITH BOOTSTRAP CI
            if (self$options$cAdjusted) {
                C_val <- sqrt(chisq_obs / (chisq_obs + n))
                k_dim <- min(nr, nc)
                C_max_adj <- sqrt((k_dim - 1) / k_dim)
                C_adj <- C_val / C_max_adj
                
                # Bootstrap CI uses shared boot_data (already generated above)
                boot_ci <- private$.bootci_C_adjusted(boot_data, conf_level)
                
                effect <- private$.interpretChiSqEffect(C_adj, "contingency_c_adj", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                measure_text <- sprintf("C adjusted (C<sub>max</sub> = %.3f)", C_max_adj)
                
                table$setRow(rowKey = 'cAdjusted', values = list(
                    measure = measure_text,
                    value = C_adj,
                    lowerCI = boot_ci$lower,
                    upperCI = boot_ci$upper,
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # CONTINGENCY COEFFICIENT C CORRECTED - WITH BOOTSTRAP CI
            if (self$options$cCorrected) {
                if (!is.null(chisq_max_value)) {
                    C_val <- sqrt(chisq_obs / (chisq_obs + n))
                    C_corr <- C_val / c_max
                    
                    B_len <- length(boot_data$tables)
                    boot_vals <- numeric(B_len)
                    for (b in 1:B_len) {
                        chisq_b <- boot_data$chisq_obs[b]
                        chisq_max_b <- boot_data$max_chisq[b]
                        C_b <- sqrt(chisq_b / (chisq_b + n))
                        C_max_b <- sqrt(chisq_max_b / (chisq_max_b + n))
                        if (C_max_b > 0) {
                            boot_vals[b] <- C_b / C_max_b
                        } else {
                            boot_vals[b] <- 0
                        }
                        if (boot_vals[b] > 1) boot_vals[b] <- 1
                    }
                    alpha <- 1 - conf_level
                    boot_ci_lower <- as.numeric(stats::quantile(boot_vals, alpha / 2, na.rm = TRUE))
                    boot_ci_upper <- as.numeric(stats::quantile(boot_vals, 1 - alpha / 2, na.rm = TRUE))
                    
                    effect <- private$.interpretChiSqEffect(C_corr, "contingency_c_corrected", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    measure_text <- sprintf("C corrected (C<sub>max</sub> = %.3f)", c_max)
                    
                    table$setRow(rowKey = 'cCorrected', values = list(
                        measure = measure_text,
                        value = C_corr,
                        lowerCI = boot_ci_lower,
                        upperCI = boot_ci_upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                } else {
                    table$setRow(rowKey = 'cCorrected', values = list(
                        measure = "C corrected",
                        value = NaN,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = ''
                    ))
                }
            }
            
            # CRAMÉR'S V - WITH CI
            if (self$options$cramersV) {
                k <- min(nr - 1, nc - 1)
                V_val <- sqrt(chisq_obs / (n * k))
                
                delta_lower <- private$.lochi(chisq_obs, df, conf_level)
                delta_upper <- private$.hichi(chisq_obs, df, conf_level)
                # limits for lambda (Smithson 2003) converted with lambda = N * w^2 (Cohen 1988)
                V_lower <- sqrt(delta_lower / (n * k))
                V_upper <- sqrt(delta_upper / (n * k))
                
                effect <- private$.interpretChiSqEffect(V_val, "cramers_v", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                table$setRow(rowKey = 'cramersV', values = list(
                    measure = "Cramér's V",
                    value = V_val,
                    lowerCI = V_lower,
                    upperCI = V_upper,
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # CRAMÉR'S V CORRECTED - WITH BOOTSTRAP CI
            if (self$options$vCorrected) {
                if (!is.null(chisq_max_value)) {
                    k <- min(nr - 1, nc - 1)
                    V_val <- sqrt(chisq_obs / (n * k))
                    V_corr <- V_val / v_max
                    
                    boot_ci <- private$.bootci_corrected(boot_data, V_corr, v_max, conf_level,
                                                         stat_function = NULL, use_chisq_ratio = TRUE)
                    
                    effect <- private$.interpretChiSqEffect(V_corr, "cramers_v_corrected", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    measure_text <- sprintf("Cramér's V corrected (V<sub>max</sub> = %.3f)", v_max)
                    
                    table$setRow(rowKey = 'vCorrected', values = list(
                        measure = measure_text,
                        value = V_corr,
                        lowerCI = boot_ci$lower,
                        upperCI = boot_ci$upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                } else {
                    table$setRow(rowKey = 'vCorrected', values = list(
                        measure = "Cramér's V corrected",
                        value = NaN,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = ''
                    ))
                }
            }
            
            # CRAMÉR'S V STANDARDISED - NO CI
            if (self$options$vStandardised) {
                if (!is.null(standardised_table)) {
                    n_std <- sum(standardised_table)
                    row_totals_std <- rowSums(standardised_table)
                    col_totals_std <- colSums(standardised_table)
                    expected_std <- outer(row_totals_std, col_totals_std) / n_std
                    std_chisq <- sum((standardised_table - expected_std)^2 / expected_std)
                    
                    k <- min(nr - 1, nc - 1)
                    V_stand <- sqrt(std_chisq / (n * k))
                    effect <- private$.interpretChiSqEffect(V_stand, "cramers_v_standardised", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    table$setRow(rowKey = 'vStandardised', values = list(
                        measure = "Cramér's V standardised",
                        value = V_stand,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                } else {
                    table$setRow(rowKey = 'vStandardised', values = list(
                        measure = "Cramér's V standardised",
                        value = NaN,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = ''
                    ))
                }
            }
            
            # CRAMÉR'S V BIAS-CORRECTED - NO CI
            if (self$options$vBiasCorrected) {
                k <- min(nr - 1, nc - 1)
                
                phi_sq <- chisq_obs / n
                phi_sq_bc <- max(0, phi_sq - ((nr - 1) * (nc - 1)) / (n - 1))
                k_bc <- k - ((k^2 - 1) / (n - 1))
                V_bc <- sqrt(phi_sq_bc / k_bc)
                
                effect <- private$.interpretChiSqEffect(V_bc, "cramers_v_bias_corrected", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                table$setRow(rowKey = 'vBiasCorrected', values = list(
                    measure = "Cramér's V bias-corrected",
                    value = V_bc,
                    lowerCI = '',
                    upperCI = '',
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # COHEN'S W - WITH CI
            if (self$options$cohenW) {
                w_val <- sqrt(chisq_obs / n)
                
                delta_lower <- private$.lochi(chisq_obs, df, conf_level)
                delta_upper <- private$.hichi(chisq_obs, df, conf_level)
                # limits for lambda (Smithson 2003) converted with lambda = N * w^2 (Cohen 1988)
                w_lower <- sqrt(delta_lower / n)
                w_upper <- sqrt(delta_upper / n)
                
                effect <- private$.interpretChiSqEffect(w_val, "cohen_w", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                table$setRow(rowKey = 'cohenW', values = list(
                    measure = "Cohen's w",
                    value = w_val,
                    lowerCI = w_lower,
                    upperCI = w_upper,
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # W-HAT - WITH BOOTSTRAP CI (using shared boot_data)
            if (self$options$wHat) {
                W_val <- private$.wHat(contingency_table)
                
                # Compute W-hat for each pre-generated bootstrap table
                boot_W_vals <- sapply(boot_data$tables, private$.wHat)
                alpha <- 1 - conf_level
                W_lower <- stats::quantile(boot_W_vals, alpha / 2)
                W_upper <- stats::quantile(boot_W_vals, 1 - alpha / 2)
                
                effect <- private$.interpretChiSqEffect(W_val, "w_hat", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                table$setRow(rowKey = 'wHat', values = list(
                    measure = "W-hat coefficient",
                    value = W_val,
                    lowerCI = W_lower,
                    upperCI = W_upper,
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # W-HAT CORRECTED - WITH BOOTSTRAP CI
            if (self$options$wHatCorrected) {
                if (!is.null(chisq_max_table)) {
                    W_val <- private$.wHat(contingency_table)
                    W_max <- private$.wHat(chisq_max_table)
                    W_corr <- W_val / W_max
                    
                    boot_ci <- private$.bootci_corrected(boot_data, W_corr, W_max, conf_level,
                                                         stat_function = private$.wHat, 
                                                         use_chisq_ratio = FALSE)
                    
                    effect <- private$.interpretChiSqEffect(W_corr, "w_hat_corrected", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    measure_text <- sprintf("W-hat corrected (W<sub>max</sub> = %.3f)", W_max)
                    
                    table$setRow(rowKey = 'wHatCorrected', values = list(
                        measure = measure_text,
                        value = W_corr,
                        lowerCI = boot_ci$lower,
                        upperCI = boot_ci$upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                } else {
                    table$setRow(rowKey = 'wHatCorrected', values = list(
                        measure = "W-hat corrected",
                        value = NaN,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = ''
                    ))
                }
            }
            
            # SAKODA'S D_G - WITH BOOTSTRAP CI (using shared boot_data)
            if (self$options$sakoda) {
                D_G <- private$.sakoda(contingency_table)
                
                # Compute Sakoda for each pre-generated bootstrap table
                boot_D_vals <- sapply(boot_data$tables, private$.sakoda)
                alpha <- 1 - conf_level
                D_lower <- stats::quantile(boot_D_vals, alpha / 2)
                D_upper <- stats::quantile(boot_D_vals, 1 - alpha / 2)
                
                effect <- private$.interpretChiSqEffect(D_G, "sakoda", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                
                table$setRow(rowKey = 'sakoda', values = list(
                    measure = "Sakoda's D Global",
                    value = D_G,
                    lowerCI = D_lower,
                    upperCI = D_upper,
                    pvalue = '',
                    effectSize = effect
                ))
                rows_populated <- TRUE
            }
            
            # SAKODA'S D_G CORRECTED - WITH BOOTSTRAP CI
            if (self$options$sakodaCorrected) {
                if (!is.null(chisq_max_table)) {
                    D_G <- private$.sakoda(contingency_table)
                    D_G_max <- private$.sakoda(chisq_max_table)
                    D_G_corr <- D_G / D_G_max
                    
                    boot_ci <- private$.bootci_corrected(boot_data, D_G_corr, D_G_max, conf_level,
                                                         stat_function = private$.sakoda, 
                                                         use_chisq_ratio = FALSE)
                    
                    effect <- private$.interpretChiSqEffect(D_G_corr, "sakoda_corrected", nr, nc, contingency_table, expected, phi_max, c_max, v_max, w_hat_max, sakoda_max)
                    
                    measure_text <- sprintf("Sakoda's D Global corrected (D<sub>max</sub> = %.3f)", D_G_max)
                    
                    table$setRow(rowKey = 'sakodaCorrected', values = list(
                        measure = measure_text,
                        value = D_G_corr,
                        lowerCI = boot_ci$lower,
                        upperCI = boot_ci$upper,
                        pvalue = '',
                        effectSize = effect
                    ))
                    rows_populated <- TRUE
                } else {
                    table$setRow(rowKey = 'sakodaCorrected', values = list(
                        measure = "Sakoda's D Global corrected",
                        value = NaN,
                        lowerCI = '',
                        upperCI = '',
                        pvalue = '',
                        effectSize = ''
                    ))
                }
            }
            
            # MARGIN-FREE MEASURES (2x2 only) - Odds Ratio, Yule's Q, Yule's Y
            # All computed together when marginFree option is selected
            if (self$options$marginFree && is_2x2) {
                
                # Apply Haldane correction if any cell is zero
                if (any(contingency_table == 0)) {
                    ct_corrected <- contingency_table + 0.5
                } else {
                    ct_corrected <- contingency_table
                }
                
                a <- ct_corrected[1, 1]
                b <- ct_corrected[1, 2]
                c <- ct_corrected[2, 1]
                d <- ct_corrected[2, 2]
                
                or_val <- (a * d) / (b * c)
                log_or <- log(or_val)
                se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
                alpha <- 1 - conf_level
                z <- stats::qnorm(1 - alpha / 2)
                
                # --- ODDS RATIO ---
                # Handle reference category selection for interpretation
                row_levels <- rownames(contingency_table)
                col_levels <- colnames(contingency_table)
                
                rowRef <- self$options$rowRef
                colRef <- self$options$colRef
                
                if (is.null(rowRef) || rowRef == "") {
                    rowRef <- row_levels[1]
                }
                if (is.null(colRef) || colRef == "") {
                    colRef <- col_levels[1]
                }
                
                rowOther <- row_levels[row_levels != rowRef][1]
                colOther <- col_levels[col_levels != colRef][1]
                
                # Reorder table based on reference categories for OR calculation
                row_order <- c(which(row_levels == rowRef), which(row_levels != rowRef))
                col_order <- c(which(col_levels == colRef), which(col_levels != colRef))
                ct_reordered <- ct_corrected[row_order, col_order, drop = FALSE]
                
                a_or <- ct_reordered[1, 1]
                b_or <- ct_reordered[1, 2]
                c_or <- ct_reordered[2, 1]
                d_or <- ct_reordered[2, 2]
                
                or_val_ref <- (a_or * d_or) / (b_or * c_or)
                log_or_ref <- log(or_val_ref)
                se_log_or_ref <- sqrt(1/a_or + 1/b_or + 1/c_or + 1/d_or)
                or_lower <- exp(log_or_ref - z * se_log_or_ref)
                or_upper <- exp(log_or_ref + z * se_log_or_ref)
                z_test_or <- log_or_ref / se_log_or_ref
                or_p <- 2 * stats::pnorm(-abs(z_test_or))
                effect_or <- private$.interpretMarginFreeEffect(or_val_ref, "odds_ratio")
                
                table$setRow(rowKey = 'oddsRatio', values = list(
                    measure = "Odds Ratio",
                    value = or_val_ref,
                    lowerCI = or_lower,
                    upperCI = or_upper,
                    pvalue = or_p,
                    effectSize = effect_or
                ))
                
                # OR interpretation as table note (variables named explicitly,
                # so the sentence reads correctly whatever the level labels)
                rowVar <- self$options$rows
                colVar <- self$options$cols
                if (or_val_ref == 1) {
                    or_text <- sprintf(
                        "Interpretation (OR = %.2f): the odds of %s = '%s' are the same for %s = '%s' and %s = '%s'.",
                        or_val_ref, rowVar, rowRef, colVar, colRef, colVar, colOther)
                } else if (or_val_ref > 1) {
                    or_text <- sprintf(
                        "Interpretation (OR = %.2f): the odds of %s = '%s' are %.2f times as high for %s = '%s' as for %s = '%s'.",
                        or_val_ref, rowVar, rowRef, or_val_ref, colVar, colRef, colVar, colOther)
                } else {
                    pctLower <- round((1 - or_val_ref) * 100)
                    or_text <- sprintf(
                        "Interpretation (OR = %.2f): the odds of %s = '%s' are %.2f times as high (%d%% lower) for %s = '%s' as for %s = '%s'.",
                        or_val_ref, rowVar, rowRef, or_val_ref, pctLower, colVar, colRef, colVar, colOther)
                }
                self$results$crosstabTable$setNote(
                    key = "or_interpretation",
                    note = or_text,
                    init = FALSE
                )
                
                # --- YULE'S Q ---
                # Q and Y from the reference-ordered OR, so that their sign
                # agrees with the reported odds ratio
                Q_val <- (or_val_ref - 1) / (or_val_ref + 1)
                Q_se <- sqrt((1/4) * (1 - Q_val^2)^2 * (1/a + 1/b + 1/c + 1/d))
                # CI: transform the log-OR CI (bounded within ±1), as in the pairwise path
                Q_lower <- (exp(log_or_ref - z * se_log_or_ref) - 1) / (exp(log_or_ref - z * se_log_or_ref) + 1)
                Q_upper <- (exp(log_or_ref + z * se_log_or_ref) - 1) / (exp(log_or_ref + z * se_log_or_ref) + 1)
                z_test_Q <- Q_val / Q_se
                Q_p <- 2 * stats::pnorm(-abs(z_test_Q))
                effect_Q <- private$.interpretMarginFreeEffect(abs(Q_val), "yules_q")
                
                table$setRow(rowKey = 'yulesQ', values = list(
                    measure = "Yule's Q",
                    value = Q_val,
                    lowerCI = Q_lower,
                    upperCI = Q_upper,
                    pvalue = Q_p,
                    effectSize = effect_Q
                ))
                
                # --- YULE'S Y ---
                Y_val <- (sqrt(or_val_ref) - 1) / (sqrt(or_val_ref) + 1)
                Y_se <- sqrt((1/16) * (1 - Y_val^2)^2 * (1/a + 1/b + 1/c + 1/d))
                # CI: transform the log-OR CI (bounded within ±1), as in the pairwise path
                Y_lower <- (sqrt(exp(log_or_ref - z * se_log_or_ref)) - 1) / (sqrt(exp(log_or_ref - z * se_log_or_ref)) + 1)
                Y_upper <- (sqrt(exp(log_or_ref + z * se_log_or_ref)) - 1) / (sqrt(exp(log_or_ref + z * se_log_or_ref)) + 1)
                z_test_Y <- Y_val / Y_se
                Y_p <- 2 * stats::pnorm(-abs(z_test_Y))
                effect_Y <- private$.interpretMarginFreeEffect(abs(Y_val), "yules_y")
                
                table$setRow(rowKey = 'yulesY', values = list(
                    measure = "Yule's Y",
                    value = Y_val,
                    lowerCI = Y_lower,
                    upperCI = Y_upper,
                    pvalue = Y_p,
                    effectSize = effect_Y
                ))
                
                rows_populated <- TRUE
            }
            
            # GOODMAN-KRUSKAL LAMBDA - WITH CI
            if (self$options$gkLambda) {
                # Lambda (rows dependent)
                lambda_row <- private$.gkLambda(contingency_table, direction = "row")
                lambda_row_ci <- private$.gkLambdaCI(contingency_table, direction = "row", conf_level)
                
                table$setRow(rowKey = 'lambdaRow', values = list(
                    measure = "Lambda (rows dependent)",
                    value = lambda_row,
                    lowerCI = lambda_row_ci[1],
                    upperCI = lambda_row_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                
                # Lambda (cols dependent)
                lambda_col <- private$.gkLambda(contingency_table, direction = "col")
                lambda_col_ci <- private$.gkLambdaCI(contingency_table, direction = "col", conf_level)
                
                table$setRow(rowKey = 'lambdaCol', values = list(
                    measure = "Lambda (cols dependent)",
                    value = lambda_col,
                    lowerCI = lambda_col_ci[1],
                    upperCI = lambda_col_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                
                # Lambda (symmetric)
                lambda_sym <- private$.gkLambda(contingency_table, direction = "symmetric")
                lambda_sym_ci <- private$.gkLambdaCI(contingency_table, direction = "symmetric", conf_level)
                
                table$setRow(rowKey = 'lambdaSym', values = list(
                    measure = "Lambda (symmetric)",
                    value = lambda_sym,
                    lowerCI = lambda_sym_ci[1],
                    upperCI = lambda_sym_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                rows_populated <- TRUE
            }
            
            # GOODMAN-KRUSKAL LAMBDA CORRECTED - WITH CI
            if (self$options$gkLambdaCorrected) {
                lambda_row_corr <- private$.gkLambdaCorrected(contingency_table, direction = "row")
                lambda_row_corr_ci <- private$.gkLambdaCorrectedCI(contingency_table, direction = "row", conf_level)
                table$setRow(rowKey = 'lambdaCorrRow', values = list(
                    measure = "Lambda corrected (rows dependent)",
                    value = lambda_row_corr,
                    lowerCI = lambda_row_corr_ci[1],
                    upperCI = lambda_row_corr_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                
                lambda_col_corr <- private$.gkLambdaCorrected(contingency_table, direction = "col")
                lambda_col_corr_ci <- private$.gkLambdaCorrectedCI(contingency_table, direction = "col", conf_level)
                table$setRow(rowKey = 'lambdaCorrCol', values = list(
                    measure = "Lambda corrected (cols dependent)",
                    value = lambda_col_corr,
                    lowerCI = lambda_col_corr_ci[1],
                    upperCI = lambda_col_corr_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                
                lambda_sym_corr <- private$.gkLambdaCorrected(contingency_table, direction = "symmetric")
                lambda_sym_corr_ci <- private$.gkLambdaCorrectedCI(contingency_table, direction = "symmetric", conf_level)
                table$setRow(rowKey = 'lambdaCorrSym', values = list(
                    measure = "Lambda corrected (symmetric)",
                    value = lambda_sym_corr,
                    lowerCI = lambda_sym_corr_ci[1],
                    upperCI = lambda_sym_corr_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                rows_populated <- TRUE
            }
            
            # GOODMAN-KRUSKAL TAU - WITH CI
            if (self$options$gkTau) {
                tau_row <- private$.gkTau(contingency_table, direction = "row")
                tau_row_ci <- private$.gkTauCI(contingency_table, direction = "row", conf_level)
                
                table$setRow(rowKey = 'tauRow', values = list(
                    measure = "Tau (rows dependent)",
                    value = tau_row,
                    lowerCI = tau_row_ci[1],
                    upperCI = tau_row_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                
                tau_col <- private$.gkTau(contingency_table, direction = "col")
                tau_col_ci <- private$.gkTauCI(contingency_table, direction = "col", conf_level)
                
                table$setRow(rowKey = 'tauCol', values = list(
                    measure = "Tau (cols dependent)",
                    value = tau_col,
                    lowerCI = tau_col_ci[1],
                    upperCI = tau_col_ci[2],
                    pvalue = '',
                    effectSize = ''
                ))
                rows_populated <- TRUE
            }
            
            # If no rows were populated, simply return
            if (!rows_populated) {
                return()
            }
            
            # Determine which CI types are present
            has_smithson_ci <- self$options$phi || self$options$phiSigned || 
                self$options$contingencyC || self$options$cramersV || self$options$cohenW
            
            has_bootstrap_ci <- self$options$phiCorrected || self$options$cAdjusted ||
                self$options$cCorrected || self$options$vCorrected ||
                self$options$wHatCorrected || self$options$sakodaCorrected ||
                self$options$wHat || self$options$sakoda
            
            has_lambda_ci <- self$options$gkLambda
            has_lambda_corr_ci <- self$options$gkLambdaCorrected
            has_tau_ci <- self$options$gkTau
            has_margin_free_ci <- is_2x2 && self$options$marginFree
            
            any_ci_present <- has_smithson_ci || has_bootstrap_ci || has_lambda_ci || 
                has_lambda_corr_ci || has_tau_ci || has_margin_free_ci
            
            if (any_ci_present) {
                ci_note_parts <- c()
                
                if (has_smithson_ci) {
                    ci_note_parts <- c(ci_note_parts, 
                                       "Phi, Phi signed, Contingency C, Cramér's V, and Cohen's w use confidence limits for the noncentrality parameter λ of the χ² distribution (Smithson 2003), converted to each measure through λ = N·w² (Cohen 1988)")
                }
                
                if (has_bootstrap_ci) {
                    ci_note_parts <- c(ci_note_parts,
                                       sprintf("Phi corrected, C adjusted, C corrected, V corrected, W-hat, W-hat corrected, Sakoda's D, and Sakoda's D corrected use the bootstrap percentile method (%d resampled tables); bootstrap intervals estimate precision but cannot test H₀: no association", 
                                               self$options$bootstrapReps))
                }
                
                if (has_lambda_ci) {
                    ci_note_parts <- c(ci_note_parts,
                                       "Lambda uses the asymptotic variance formula (Reynolds 1977); lambda has a degenerate sampling distribution at boundaries—if sample λ = 0 exactly, H₀: λ = 0 cannot be rejected regardless of CI width; if sample λ ≠ 0, population λ is definitively non-zero even when CI extends below zero")
                }
                
                if (has_lambda_corr_ci) {
                    ci_note_parts <- c(ci_note_parts,
                                       "Lambda corrected uses the delta-method asymptotic variance formula (Kvålseth 2018b)")
                }
                
                if (has_tau_ci) {
                    ci_note_parts <- c(ci_note_parts,
                                       "Tau uses the asymptotic variance formula (Bishop et al. 2007: 391–392)")
                }
                
                if (has_margin_free_ci) {
                    ci_note_parts <- c(ci_note_parts,
                                       "Odds Ratio, Yule's Q, and Yule's Y use asymptotic variance formulas derived from the log-odds ratio (Reynolds 1977)")
                }
                
                ci_note <- paste(ci_note_parts, collapse = ". ")
                table$setNote(key = "ci_method", note = paste0("<em>CI methods.</em> ", ci_note, "."), init = FALSE)
            } else {
                table$setNote(key = "ci_method", note = "The selected measure(s) do not have confidence intervals.", init = FALSE)
            }
        },
        
        .populatePairwiseORTable = function(contingency_table, nr, nc) {
            # Populate pairwise Odds Ratios for tables > 2x2
            if (nr == 2 && nc == 2) return()
            
            pairTable <- self$results$pairwiseORTable
            
            conf_level <- self$options$confLevel / 100
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            for (i in 1:(nr - 1)) {
                for (j in (i + 1):nr) {
                    for (k in 1:(nc - 1)) {
                        for (l in (k + 1):nc) {
                            sub_table <- contingency_table[c(i, j), c(k, l)]
                            if (any(sub_table == 0)) sub_table <- sub_table + 0.5
                            
                            a <- sub_table[1, 1]; b <- sub_table[1, 2]
                            c <- sub_table[2, 1]; d <- sub_table[2, 2]
                            
                            comparison_label <- sprintf("%s vs %s | %s vs %s", 
                                                        row_names[i], row_names[j],
                                                        col_names[k], col_names[l])
                            rowKey <- paste0("or_", i, "_", j, "_", k, "_", l)
                            
                            or_val <- (a * d) / (b * c)
                            log_or <- log(or_val)
                            se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
                            alpha <- 1 - conf_level
                            z <- stats::qnorm(1 - alpha / 2)
                            
                            or_lower <- exp(log_or - z * se_log_or)
                            or_upper <- exp(log_or + z * se_log_or)
                            z_test <- log_or / se_log_or
                            or_p <- 2 * stats::pnorm(-abs(z_test))
                            effect <- private$.interpretMarginFreeEffect(or_val, "odds_ratio")
                            
                            pairTable$setRow(rowKey = rowKey, values = list(
                                comparison = comparison_label,
                                value = or_val,
                                lowerCI = or_lower,
                                upperCI = or_upper,
                                pvalue = or_p,
                                effectSize = effect
                            ))
                        }
                    }
                }
            }
        },
        
        .populatePairwiseQTable = function(contingency_table, nr, nc) {
            # Populate pairwise Yule's Q for tables > 2x2
            if (nr == 2 && nc == 2) return()
            
            pairTable <- self$results$pairwiseQTable
            
            conf_level <- self$options$confLevel / 100
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            for (i in 1:(nr - 1)) {
                for (j in (i + 1):nr) {
                    for (k in 1:(nc - 1)) {
                        for (l in (k + 1):nc) {
                            sub_table <- contingency_table[c(i, j), c(k, l)]
                            if (any(sub_table == 0)) sub_table <- sub_table + 0.5
                            
                            a <- sub_table[1, 1]; b <- sub_table[1, 2]
                            c <- sub_table[2, 1]; d <- sub_table[2, 2]
                            
                            comparison_label <- sprintf("%s vs %s | %s vs %s", 
                                                        row_names[i], row_names[j],
                                                        col_names[k], col_names[l])
                            rowKey <- paste0("q_", i, "_", j, "_", k, "_", l)
                            
                            or_val <- (a * d) / (b * c)
                            log_or <- log(or_val)
                            se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
                            alpha <- 1 - conf_level
                            z <- stats::qnorm(1 - alpha / 2)
                            
                            Q_val <- (or_val - 1) / (or_val + 1)
                            log_or_lower <- log_or - z * se_log_or
                            log_or_upper <- log_or + z * se_log_or
                            Q_lower <- (exp(log_or_lower) - 1) / (exp(log_or_lower) + 1)
                            Q_upper <- (exp(log_or_upper) - 1) / (exp(log_or_upper) + 1)
                            
                            # SE(Q) = (1/2)(1 - Q^2) sqrt(sum 1/n), as in the 2x2 path
                            z_test <- Q_val / sqrt((1/4) * (1 - Q_val^2)^2 * (1/a + 1/b + 1/c + 1/d))
                            Q_p <- 2 * stats::pnorm(-abs(z_test))
                            effect <- private$.interpretMarginFreeEffect(abs(Q_val), "yules_q")
                            
                            pairTable$setRow(rowKey = rowKey, values = list(
                                comparison = comparison_label,
                                value = Q_val,
                                lowerCI = Q_lower,
                                upperCI = Q_upper,
                                pvalue = Q_p,
                                effectSize = effect
                            ))
                        }
                    }
                }
            }
        },
        
        .populatePairwiseYTable = function(contingency_table, nr, nc) {
            # Populate pairwise Yule's Y for tables > 2x2
            if (nr == 2 && nc == 2) return()
            
            pairTable <- self$results$pairwiseYTable
            
            conf_level <- self$options$confLevel / 100
            row_names <- rownames(contingency_table)
            col_names <- colnames(contingency_table)
            
            for (i in 1:(nr - 1)) {
                for (j in (i + 1):nr) {
                    for (k in 1:(nc - 1)) {
                        for (l in (k + 1):nc) {
                            sub_table <- contingency_table[c(i, j), c(k, l)]
                            if (any(sub_table == 0)) sub_table <- sub_table + 0.5
                            
                            a <- sub_table[1, 1]; b <- sub_table[1, 2]
                            c <- sub_table[2, 1]; d <- sub_table[2, 2]
                            
                            comparison_label <- sprintf("%s vs %s | %s vs %s", 
                                                        row_names[i], row_names[j],
                                                        col_names[k], col_names[l])
                            rowKey <- paste0("y_", i, "_", j, "_", k, "_", l)
                            
                            or_val <- (a * d) / (b * c)
                            log_or <- log(or_val)
                            se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
                            alpha <- 1 - conf_level
                            z <- stats::qnorm(1 - alpha / 2)
                            
                            Y_val <- (sqrt(or_val) - 1) / (sqrt(or_val) + 1)
                            log_or_lower <- log_or - z * se_log_or
                            log_or_upper <- log_or + z * se_log_or
                            Y_lower <- (sqrt(exp(log_or_lower)) - 1) / (sqrt(exp(log_or_lower)) + 1)
                            Y_upper <- (sqrt(exp(log_or_upper)) - 1) / (sqrt(exp(log_or_upper)) + 1)
                            
                            # SE(Y) = (1/4)(1 - Y^2) sqrt(sum 1/n), as in the 2x2 path
                            z_test <- Y_val / sqrt((1/16) * (1 - Y_val^2)^2 * (1/a + 1/b + 1/c + 1/d))
                            Y_p <- 2 * stats::pnorm(-abs(z_test))
                            effect <- private$.interpretMarginFreeEffect(abs(Y_val), "yules_y")
                            
                            pairTable$setRow(rowKey = rowKey, values = list(
                                comparison = comparison_label,
                                value = Y_val,
                                lowerCI = Y_lower,
                                upperCI = Y_upper,
                                pvalue = Y_p,
                                effectSize = effect
                            ))
                        }
                    }
                }
            }
        },
        
        .populateThresholdsTable = function(nr, nc, is_2x2, contingency_table, expected, phi_max, c_max, v_max, chisq_max_table) {
            
            table <- self$results$thresholdsTable
            table$deleteRows()
            
            row_count <- 0
            k <- min(nr - 1, nc - 1)
            
            # Calculate Tier 1 scaled thresholds (dynamically computed)
            if (k > 0) {
                t1_small <- 0.100 / sqrt(k)
                t1_medium <- 0.300 / sqrt(k)
                t1_large <- 0.500 / sqrt(k)
            } else {
                t1_small <- 0.100
                t1_medium <- 0.300
                t1_large <- 0.500
            }
            
            if (self$options$phi && is_2x2) {
                # Tier 1 + Tier 2 for uncorrected phi
                if (!is.null(phi_max)) {
                    small_final <- t1_small * phi_max
                    medium_final <- t1_medium * phi_max
                    large_final <- t1_large * phi_max
                    note_text <- sprintf("Cohen 1988: 0.1, 0.3, 0.5 × φ<sub>max</sub> (%.3f)", phi_max)
                } else {
                    small_final <- t1_small
                    medium_final <- t1_medium
                    large_final <- t1_large
                    note_text <- "Cohen 1988: 0.1, 0.3, 0.5"
                }
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Phi (φ)",
                    small = sprintf("%.3f", small_final),
                    medium = sprintf("%.3f", medium_final),
                    large = sprintf("%.3f", large_final),
                    notes = note_text
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$phiSigned && is_2x2) {
                if (!is.null(phi_max)) {
                    small_final <- t1_small * phi_max
                    medium_final <- t1_medium * phi_max
                    large_final <- t1_large * phi_max
                    note_text <- sprintf("Cohen 1988: 0.1, 0.3, 0.5 × φ<sub>max</sub> (%.3f)", phi_max)
                } else {
                    small_final <- t1_small
                    medium_final <- t1_medium
                    large_final <- t1_large
                    note_text <- "Cohen 1988: 0.1, 0.3, 0.5"
                }
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Phi signed (φ)",
                    small = sprintf("%.3f", small_final),
                    medium = sprintf("%.3f", medium_final),
                    large = sprintf("%.3f", large_final),
                    notes = note_text
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$phiCorrected && is_2x2) {
                # For 2x2 tables, k=1, so no scaling needed - Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "Phi corrected (φ<sub>corr</sub>)",
                    small = "0.100",
                    medium = "0.300",
                    large = "0.500",
                    notes = "Cohen 1988 fixed: 0.1, 0.3, 0.5"
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$coleC7 && is_2x2) {
                table$addRow(rowKey = row_count, values = list(
                    measure = "Cole's C7",
                    small = "0.100",
                    medium = "0.300",
                    large = "0.500",
                    notes = "Cohen 1988 fixed: 0.1, 0.3, 0.5"
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$zysnoPhi && is_2x2) {
                table$addRow(rowKey = row_count, values = list(
                    measure = "Zysno's φ*",
                    small = "0.100",
                    medium = "0.300",
                    large = "0.500",
                    notes = "Cohen 1988 fixed: 0.1, 0.3, 0.5"
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$contingencyC) {
                # Tier 1 + Tier 2 for uncorrected C
                if (!is.null(c_max)) {
                    small_final <- t1_small * c_max
                    medium_final <- t1_medium * c_max
                    large_final <- t1_large * c_max
                    note_text <- sprintf("Cohen 1988 scaled by √df (%.3f, %.3f, %.3f), × C<sub>max</sub> (%.3f)", 
                                         t1_small, t1_medium, t1_large, c_max)
                } else {
                    small_final <- t1_small
                    medium_final <- t1_medium
                    large_final <- t1_large
                    note_text <- sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                         t1_small, t1_medium, t1_large)
                }
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Contingency Coefficient (C)",
                    small = sprintf("%.3f", small_final),
                    medium = sprintf("%.3f", medium_final),
                    large = sprintf("%.3f", large_final),
                    notes = note_text
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$cAdjusted) {
                # Tier 1 only (C adj is a corrected measure)
                table$addRow(rowKey = row_count, values = list(
                    measure = "C adjusted",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$cCorrected) {
                # Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "C corrected",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$cramersV) {
                # Tier 1 + Tier 2 for uncorrected V
                if (!is.null(v_max)) {
                    small_final <- t1_small * v_max
                    medium_final <- t1_medium * v_max
                    large_final <- t1_large * v_max
                    note_text <- sprintf("Cohen 1988 scaled by √df (%.3f, %.3f, %.3f), × V<sub>max</sub> (%.3f)", 
                                         t1_small, t1_medium, t1_large, v_max)
                } else {
                    small_final <- t1_small
                    medium_final <- t1_medium
                    large_final <- t1_large
                    note_text <- sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                         t1_small, t1_medium, t1_large)
                }
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Cramér's V",
                    small = sprintf("%.3f", small_final),
                    medium = sprintf("%.3f", medium_final),
                    large = sprintf("%.3f", large_final),
                    notes = note_text
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$vCorrected) {
                # Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "Cramér's V corrected",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$vStandardised) {
                # Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "Cramér's V standardised",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$vBiasCorrected) {
                # Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "Cramér's V bias-corrected",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$cohenW) {
                # No tiers - fixed thresholds
                table$addRow(rowKey = row_count, values = list(
                    measure = "Cohen's w",
                    small = "0.100",
                    medium = "0.300",
                    large = "0.500",
                    notes = "Cohen 1988 fixed: 0.1, 0.3, 0.5"
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$wHat) {
                # Tier 1 + Tier 2 for uncorrected W-hat
                # Compute W-hat max for Tier 2 adjustment
                w_hat_obs <- private$.wHat(contingency_table)
                if (!is.null(chisq_max_table)) {
                    w_hat_max_val <- private$.wHat(chisq_max_table)
                    small_final <- t1_small * w_hat_max_val
                    medium_final <- t1_medium * w_hat_max_val
                    large_final <- t1_large * w_hat_max_val
                    note_text <- sprintf("Cohen 1988 scaled by √df (%.3f, %.3f, %.3f), × W<sub>max</sub> (%.3f)", 
                                         t1_small, t1_medium, t1_large, w_hat_max_val)
                } else {
                    small_final <- t1_small
                    medium_final <- t1_medium
                    large_final <- t1_large
                    note_text <- sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                         t1_small, t1_medium, t1_large)
                }
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "W-hat coefficient",
                    small = sprintf("%.3f", small_final),
                    medium = sprintf("%.3f", medium_final),
                    large = sprintf("%.3f", large_final),
                    notes = note_text
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$wHatCorrected) {
                # Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "W-hat corrected",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$sakoda) {
                # Tier 1 + Tier 2 for uncorrected Sakoda
                # Compute Sakoda D_G max for Tier 2 adjustment
                sakoda_obs <- private$.sakoda(contingency_table)
                if (!is.null(chisq_max_table)) {
                    sakoda_max_val <- private$.sakoda(chisq_max_table)
                    small_final <- t1_small * sakoda_max_val
                    medium_final <- t1_medium * sakoda_max_val
                    large_final <- t1_large * sakoda_max_val
                    note_text <- sprintf("Cohen 1988 scaled by √df (%.3f, %.3f, %.3f), × D<sub>max</sub> (%.3f)", 
                                         t1_small, t1_medium, t1_large, sakoda_max_val)
                } else {
                    small_final <- t1_small
                    medium_final <- t1_medium
                    large_final <- t1_large
                    note_text <- sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                         t1_small, t1_medium, t1_large)
                }
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Sakoda's D Global",
                    small = sprintf("%.3f", small_final),
                    medium = sprintf("%.3f", medium_final),
                    large = sprintf("%.3f", large_final),
                    notes = note_text
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$sakodaCorrected) {
                # Tier 1 only
                table$addRow(rowKey = row_count, values = list(
                    measure = "Sakoda's D Global corrected",
                    small = sprintf("%.3f", t1_small),
                    medium = sprintf("%.3f", t1_medium),
                    large = sprintf("%.3f", t1_large),
                    notes = sprintf("Cohen 1988 scaled by √df: %.3f, %.3f, %.3f", 
                                    t1_small, t1_medium, t1_large)
                ))
                row_count <- row_count + 1
            }
            
            if (self$options$marginFree) {
                table$addRow(rowKey = row_count, values = list(
                    measure = "Odds Ratio",
                    small = "2.0",
                    medium = "3.0",
                    large = "4.0",
                    notes = "Ferguson 2009"
                ))
                row_count <- row_count + 1
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Yule's Q",
                    small = "0.330",
                    medium = "0.500",
                    large = "0.600",
                    notes = "OR-based (Ferguson 2009)"
                ))
                row_count <- row_count + 1
                
                table$addRow(rowKey = row_count, values = list(
                    measure = "Yule's Y",
                    small = "0.171",
                    medium = "0.268",
                    large = "0.333",
                    notes = "OR-based (Ferguson 2009)"
                ))
                row_count <- row_count + 1
            }
            
            # Only show df note if any measure using Tier 1 (df scaling) is selected
            show_df_note <- self$options$phi || self$options$phiSigned || self$options$phiCorrected ||
                self$options$contingencyC || self$options$cAdjusted || self$options$cCorrected ||
                self$options$cramersV || self$options$vCorrected || self$options$vStandardised ||
                self$options$vBiasCorrected || self$options$wHat || self$options$wHatCorrected ||
                self$options$sakoda || self$options$sakodaCorrected
            
            if (show_df_note) {
                table$setNote(
                    key = "df_note",
                    note = sprintf("df = min(rows − 1, columns − 1); for this table, df = %d", k)
                )
            }
        },
        
        .populateMethodInfo = function() {
            
            if (!self$options$showMethodInfo) {
                return()
            }
            
            html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
            
            # Chi-square-based measures
            if (self$options$phi || self$options$phiSigned || self$options$phiCorrected ||
                self$options$coleC7 || self$options$zysnoPhi ||
                self$options$contingencyC || self$options$cAdjusted || self$options$cCorrected ||
                self$options$cramersV || self$options$vCorrected || self$options$vStandardised ||
                self$options$vBiasCorrected || self$options$cohenW) {
                
                html <- paste0(html, "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Chi²-Based Measures of Association</h2>")
                html <- paste0(html, "<p>These measures quantify association strength based on the chi-squared statistic.</p>")
                
                if (self$options$phi || self$options$phiSigned) {
                    html <- paste0(html, 
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Phi Coefficient (φ)</h3>",
                                   "<p>A measure of association for 2×2 tables, calculated as the square root of the chi-squared statistic divided by the table's grand total. The φ coefficient ranges from 0.0 (no association) to 1.0 (perfect association). However, the maximum achievable value is 1.0 only under like marginal sets. Under unlike marginal sets, the maximum achievable value of φ is less than 1.0. φ is particularly sensitive to the bilateral (two-way) nature of associations across the categories being compared. This means it captures relationships where each level of one variable tends to pair with the opposite level of the other variable. In contrast, other measures like Yule's Q are more sensitive to unilateral (one-way) associations, where one level of a variable pairs predominantly with one level of another variable. The choice between measures should be informed by the type of association that is of analytical interest.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Cramér, H. (1946). <em>Mathematical Methods of Statistics</em>. Princeton University Press.<br/>",
                                   "Mueller, J. H., & Schuessler, K. F. (1961). <em>Statistical Reasoning in Sociology</em>. Oxford & IBH Publishing Co.<br/>",
                                   "Buchanan, W. (1974). Nominal and ordinal bivariate statistics: The practitioner's view. <em>American Journal of Political Science, 18</em>(3), 625–646.<br/>",
                                   "Smithson, M. (2003). <em>Confidence Intervals</em>. Sage.<br/>",
                                   "Cohen, J. (1988). <em>Statistical Power Analysis for the Behavioral Sciences</em> (2nd ed.). Erlbaum.<br/>",
                                   "Bonett, D. G., & Price, R. M. (2007). Statistical inference for generalized Yule coefficients in 2×2 contingency tables. <em>Sociological Methods & Research, 35</em>(3), 429–446.<br/>",
                                   "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>")
                }
                
                if (self$options$phiCorrected) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Phi Corrected (φ<sub>corr</sub>)</h3>",
                                   "<p>A normalised version of the φ coefficient that accounts for the fact that uncorrected φ has 1.0 as its maximum achievable value only in cross-tabs featuring like marginal sets. This makes the uncorrected φ not always directly comparable across different tables. φ corrected normalises the observed φ by dividing it by its maximum attainable value given the marginal configuration. This adjustment allows for comparisons across tables. φ corrected should be interpreted as the proportion of the maximum potential association realised by the observed data. It indicates how much of the theoretical maximum divergence from expected frequencies under independence is achieved, given the marginal constraints. A value close to 1.0 suggests that the observed association is the largest possible given the marginal configuration.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Cureton, E. E. (1959). Note on phi/phimax. <em>Psychometrika, 24</em>(1), 89–91.<br/>",
                                   "Mueller, J. H., & Schuessler, K. F. (1961). <em>Statistical Reasoning in Sociology</em>. Oxford & IBH Publishing Co.<br/>",
                                   "Liu, R. (1980). A note on phi-coefficient comparison. <em>Research in Higher Education, 13</em>(1), 3–8.<br/>",
                                   "Davenport, E. C., & El-Sanhurry, N. A. (1991). Phi/Phimax: Review and synthesis. <em>Educational and Psychological Measurement, 51</em>(4), 821–828.<br/>",
                                   "Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). <em>Statistics in Psychology Using R and SPSS</em>. Wiley.<br/>",
                                   "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.<br/>",
                                   "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>")
                }
                
                if (self$options$coleC7) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Cole's C7 Coefficient</h3>",
                                   "<p>Cole's C7 is a margin-free measure of association for 2×2 tables that normalises the cross-product difference (ad − bc) by its maximum achievable value given the marginal totals. Unlike standard phi, which can be constrained below unity by unequal marginals, Cole's C7 can always achieve ±1 regardless of marginal distribution. The coefficient reaches +1 when there is perfect positive association (one of b or c equals zero) and −1 when there is perfect negative association (one of a or d equals zero).</p>",
                                   "<p>The original formulation by Cole (1949) omitted one case; Ratliff (1982) provided the complete set of four formulas covering all possible configurations. For positive association (ad ≥ bc): if c ≤ b, the denominator is (a+b)(b+d); otherwise it is (a+c)(c+d). For negative association (ad < bc): if a ≤ d, the denominator is (a+b)(a+c); otherwise it is (b+d)(c+d).</p>",
                                   "<p>Cole's C7 combines the margin-free property of Yule's Q (always achieving ±1 for complete associations) with greater sensitivity to association strength in non-perfect cases. Unlike Yule's Q, which saturates at ±1 whenever any cell is zero, Cole's C7 only reaches ±1 when the zero is in the 'limiting' cell that constrains maximum association.</p>",
                                   "<p><em>Note:</em> While grouped with chi-squared-based measures due to its conceptual relationship with phi (as a margin-free alternative), Cole's C7 is not technically χ²-based; it normalises the cross-product difference (ad − bc) directly rather than deriving from the chi-squared statistic.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Cole, L. C. (1949). The measurement of interspecific association. <em>Ecology, 30</em>(4), 411–424.<br/>",
                                   "Ratliff, R. D. (1982). A correction of Cole's C7 and Hurlbert's C8 coefficients of interspecific association. <em>Ecology, 63</em>(5), 1605–1606.</p>")
                }
                
                if (self$options$zysnoPhi) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Zysno's φ* (Phi-Star)</h3>",
                                   "<p>Zysno's φ* is a modification of the phi coefficient designed to reduce its dependence on marginal distributions while retaining sensitivity to association strength. The formula is: φ* = (ad − bc) / (|ad − bc| + N × e), where e is the 'error term' representing the smaller element of the weaker diagonal (min(b,c) for positive associations, min(a,d) for negative associations).</p>",
                                   "<p>Like Cole's C7 (to which it is related but not identical), Zysno's φ* can always achieve ±1 regardless of marginal distribution, making it directly comparable across tables with different marginal configurations. It reaches +1 when min(b,c) = 0 with positive association, and −1 when min(a,d) = 0 with negative association.</p>",
                                   "<p>Zysno's φ* combines desirable properties from multiple measures: (1) like phi-corrected, its theoretical ceiling is always unity regardless of marginals; (2) like Yule's Q, it can reach ±1 for 'complete' associations where only one off-diagonal cell is zero; (3) unlike Yule's Q, it retains sensitivity to association strength in non-perfect cases rather than saturating at ±1.</p>",
                                   "<p><em>Note:</em> While grouped with chi-squared-based measures due to its conceptual relationship with phi (as a margin-free alternative), Zysno's φ* is not technically χ²-based; it normalises the cross-product difference (ad − bc) directly rather than deriving from the chi-squared statistic. Warrens (2008) showed that a formula using min(n₁u₂, n₂u₁) as denominator is algebraically equivalent to Zysno's φ*; however, this differs from Cole's C7 with Ratliff's correction, which uses conditional formulas based on cell relationships.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Zysno, P. V. (1997). The modification of the phi-coefficient reducing its dependence on the marginal distributions. <em>Methods of Psychological Research Online, 2</em>, 41–52.<br/>",
                                   "Warrens, M. J. (2008). A comment on Zysno's 'The modification of the phi-coefficient reducing its dependence on the marginal distributions'. <em>Methods of Psychological Research Online, 13</em>, 15–19.</p>")
                }
                
                if (self$options$contingencyC || self$options$cAdjusted || self$options$cCorrected) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Contingency Coefficient (C)</h3>",
                                   "<p>A measure to quantify the strength of association between two categorical variables. It is calculated by dividing the chi-squared value by itself plus the table's grand total, and then taking the square root. It ranges between 0.0 (indicating independence) and less than 1.0. The maximum achievable value depends on the size of the cross-tab, approaching unity only when a table is fairly large.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Smithson, M. (2003). <em>Confidence Intervals</em>. Sage.<br/>",
                                   "Cohen, J. (1988). <em>Statistical Power Analysis for the Behavioral Sciences</em> (2nd ed.). Erlbaum.<br/>",
                                   "Sheskin, D. J. (2011). <em>Handbook of Parametric and Nonparametric Statistical Procedures</em> (5th ed.). Chapman and Hall/CRC.<br/>",
                                   "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>")
                }
                
                if (self$options$cAdjusted || self$options$cCorrected) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Contingency Coefficient Adjusted and Corrected</h3>",
                                   "<p>The module computes the adjusted version of C (C adj), which is equal to C divided by √[(k−1)/k], where k is the number of rows or columns, whichever is smaller. The adjustment factor is equal to the maximum value C can attain on the basis of the table's size alone. However, that does not take into account the actual configuration of the marginals. Therefore, a 'proper' correction is based on dividing C by the value it achieves in the chi-squared-maximising table. This maximum-corrected version of C is computed by the module and reported as C corrected as opposed to C adj, which refers to the first above-described adjustment. When interpreting the magnitude of the association, unadjusted C is assessed against Cohen's thresholds adjusted by C's maximum possible value given the table's marginals, while C adj and C corrected are assessed against the standard Cohen's thresholds scaled by the table's size.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Sakoda, J. M. (1981). A generalized index of dissimilarity. <em>Demography, 18</em>(2), 245–250.<br/>",
                                   "Blaikie, N. (2003). <em>Analyzing Quantitative Data: From Description to Explanation</em>. SAGE Publications Ltd.<br/>",
                                   "Sheskin, D. J. (2011). <em>Handbook of Parametric and Nonparametric Statistical Procedures</em> (5th ed.). Chapman and Hall/CRC.<br/>",
                                   "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.</p>")
                }
                
                if (self$options$cramersV) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Cramér's V</h3>",
                                   "<p>A measure of association for categorical variables, calculated from the chi-squared statistic divided by N × min(r−1, c−1)—that is, the table's grand total times the minimum dimension of the table (minus 1). The division by N × min(r−1, c−1), which corresponds to the maximum chi-squared value attainable given the table's size under like marginal sets, ensures the value stays between 0.0 (no association) and 1.0 (perfect association), and makes the coefficient comparable across tables of different sizes. This measure is suitable for tables larger than 2×2. However, under unlike marginal sets, the upper ceiling of V can be less than unity. Also, with tables featuring a concentration of observations in specific cells, sparse cells with very low or zero counts, or uneven marginal frequencies, V can overestimate the strength of association (see the W-hat coefficient).</p>",
                                   "<p><strong>Confidence interval.</strong> The interval (at the level set in the options) is obtained by inverting the noncentral χ² distribution to find confidence limits for its noncentrality parameter λ (Smithson 2003). The limits are converted to the effect-size scale through the relation λ = N·w² that links the noncentrality to the population effect size (Cohen 1988), so that V = √(λ / (N × min(r−1, c−1))); the same conversion is used for φ, C (= √(λ / (λ + N))) and Cohen's w. Using the same transformation as for the point estimate guarantees that the interval contains the estimate, and the lower limit is 0 unless χ² exceeds the upper (1 − confidence level)/2 point of the central χ² distribution. Smithson (2003, p. 40) converts the limits with λ + df instead; this gives very similar limits for clear associations, but for weak associations it yields a lower limit above the point estimate.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Cramér, H. (1946). <em>Mathematical Methods of Statistics</em>. Princeton University Press.<br/>",
                                   "Smithson, M. (2003). <em>Confidence Intervals</em>. Sage.<br/>",
                                   "Cohen, J. (1988). <em>Statistical Power Analysis for the Behavioral Sciences</em> (2nd ed.). Erlbaum.<br/>",
                                   "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.<br/>",
                                   "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>")
                }
                
                if (self$options$vCorrected) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Cramér's V Corrected</h3>",
                                   "<p>Cramér's V corrected addresses a fundamental limitation of the standard Cramér's V coefficient: its maximum achievable value depends on the marginal distribution of the table and rarely reaches 1.0, even when there is perfect association. This makes direct comparisons of V values across tables with different marginal configurations problematic. To compute V corrected, the module first calculates V max, which represents the maximum possible value of Cramér's V that could be achieved given the observed marginal totals. V max is derived from the chi-squared statistic computed on the chi-squared-maximising table, which represents the theoretical upper bound of association given the constraints imposed by the marginals. V corrected is then calculated as the ratio V / V max. This correction serves two important purposes: (1) It scales V relative to its theoretically achievable maximum, allowing V corrected to range from 0 to 1 under any marginal configuration. A value of 1.0 indicates that the observed association is as strong as is possible given the marginal constraints. (2) It enables meaningful comparisons of association strength across tables with different marginal distributions. The interpretation of V corrected follows standard Cohen thresholds scaled for table size, since the correction ensures the coefficient is already scaled relative to its maximum achievable value.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.</p>")
                }
                
                if (self$options$vStandardised) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Cramér's V Standardised</h3>",
                                   "<p>This version of the coefficient is computed on the standardised table, which is returned and optionally rendered by the module. Since a number of association measures, among which Cramér's V, are affected by the skewness in the marginal distributions, the original table is first standardised and then Cramér's V is computed. The rationale for using standardised tables as the basis to compute Cramér's V is that coefficients calculated on standardised tables are comparable across tables because the impact of different marginal distributions is controlled for. The value obtained by subtracting the ratio Cramér's V to Cramér's V standardised from 1 gives an idea of the reduction of the magnitude of V due to the skewness of the marginal sums (multiplied by 100 can be interpreted as percentage reduction).</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Fienberg, S. E. (1971). A statistical technique for historians: Standardizing tables of counts. <em>The Journal of Interdisciplinary History, 1</em>(2), 305–315.<br/>",
                                   "Smith, K. W. (1976). Marginal standardization and table shrinking: Aids in the traditional analysis of contingency tables. <em>Social Forces, 54</em>(3), 669–693.<br/>",
                                   "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.</p>")
                }
                
                if (self$options$vBiasCorrected) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Cramér's V Bias-Corrected</h3>",
                                   "<p>The bias-corrected Cramér's V addresses the finite-sample bias present in the standard V coefficient. In small to moderate sample sizes, V tends to overestimate the true population association. The bias-correction adjusts the observed phi-squared value by subtracting the expected bias term, then adjusts the degrees of freedom term similarly, before taking the square root to obtain the bias-corrected V. This provides a less biased estimate of association strength, particularly valuable when sample sizes are limited.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Bergsma, W. (2013). A bias correction for Cramér's V and Tschuprow's T. <em>Journal of the Korean Statistical Society, 42</em>(3), 323–328.</p>")
                }
                
                if (self$options$cohenW) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Cohen's w</h3>",
                                   "<p>Cohen's w is an effect size measure for the chi-squared test, calculated as the square root of the chi-squared statistic divided by the sample size. It provides a standardised measure of the magnitude of deviation from independence. Unlike Cramér's V, Cohen's w is not adjusted for table dimensions and uses fixed thresholds (0.1, 0.3, 0.5 for small, medium, and large effects) regardless of table size, as it was specifically calibrated for power analysis in 2×2 tables.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Cohen, J. (1988). <em>Statistical Power Analysis for the Behavioral Sciences</em> (2nd ed.). Erlbaum.<br/>",
                                   "Smithson, M. (2003). <em>Confidence Intervals</em>. Sage.<br/>",
                                   "Sheskin, D. J. (2011). <em>Handbook of Parametric and Nonparametric Statistical Procedures</em> (5th ed.). Chapman and Hall/CRC.</p>")
                }
            }
            
            # Distance-based measures
            if (self$options$wHat || self$options$sakoda) {
                html <- paste0(html,
                               "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Distance-based Measures</h2>")
            }
            
            if (self$options$wHat || self$options$wHatCorrected) {
                html <- paste0(html,
                               "<h3 style='color: #2874A6; margin-top: 1.5em;'>W-hat (Ŵ)</h3>",
                               "<p>Alternative to Cramér's V based on Euclidean distance ",
                               "between the joint probability distribution and the independence distribution. ",
                               "Unlike chi-squared-based measures that use directed divergence, W-hat employs a metric distance function. ",
                               "Because it uses squared differences between probabilities rather than squared deviations divided by expected values, ",
                               "W-hat avoids the inflation that can affect Cramér's V when some expected cell counts are very small. ",
                               "W-hat controls for marginal distribution unevenness and satisfies value-validity conditions, ensuring meaningful interpretations ",
                               "and valid comparisons of association strength. ",
                               "W-hat ranges from 0 (independence) to a theoretical maximum of 1 (perfect association). However, in tables with asymmetric marginal distributions, ",
                               "W-hat cannot achieve its theoretical maximum of unity due to structural constraints imposed by the marginals. ",
                               "In such cases, a maximum-corrected version (W-hat corrected) adjusts the observed W-hat by dividing it by its maximum achievable value, ",
                               "computed from the chi-squared-maximising table. This correction ensures the measure ranges from 0 to 1 under any marginal configuration. ",
                               "Unlike Kvålseth 2018a, for the time being, the calculation of the confidence interval (at the level set in the options) is based on a bootstrap approach, ",
                               "employing B resampled tables, and the 2.5th and 97.5th percentiles of the bootstrap distribution. ",
                               "B is set by default to 999, but can be customised by the user.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Kvålseth, T. O. (2018a). An alternative to Cramér's coefficient of association. <em>Communications in Statistics - Theory and Methods, 47</em>(23), 5662–5674.<br/>",
                               "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.</p>")
            }
            
            if (self$options$sakoda || self$options$sakodaCorrected) {
                html <- paste0(html,
                               "<h3 style='color: #2874A6; margin-top: 1.5em;'>Sakoda's D Global: Generalized Index of Dissimilarity (D<sub>G</sub>)</h3>",
                               "<p>Sakoda's D<sub>G</sub> is a distance-based measure that generalises the index of dissimilarity to multi-way contingency tables. The index represents the proportion of cases that would need to be redistributed to achieve statistical independence (even distribution) across all cells. The numerator equals half the sum of absolute deviations between observed and expected frequencies, representing the minimum number of cases requiring redistribution. The denominator equals the maximum possible sum of absolute deviations achievable given the marginal totals, calculated as the sum across columns of N×P<sub>j</sub>×(1−P<sub>j</sub>), where P<sub>j</sub> is the column proportion. D<sub>G</sub> ranges from 0 (perfect independence) to a theoretical maximum of 1 (maximum association).</p>",
                               "<p>However, in tables with asymmetric marginal distributions, D<sub>G</sub> cannot achieve its theoretical maximum of unity due to structural constraints imposed by the marginals. ",
                               "In such cases, a maximum-corrected version (Sakoda's D Global corrected) adjusts the observed D<sub>G</sub> by dividing it by its maximum achievable value, ",
                               "computed from the chi-squared-maximising table. This correction ensures the measure ranges from 0 to 1 under any marginal configuration.</p>",
                               "<p>Unlike chi-squared-based measures—which use squared deviations divided by expected values—D<sub>G</sub> uses absolute deviations. As Sakoda (1981) noted, squaring residuals gives greater weight to extreme cells; additionally, because D<sub>G</sub> does not divide by expected values, it avoids the inflation that can affect Cramér's V when some expected cell counts are very small (Kvålseth 2018a). D<sub>G</sub> also has an intuitive interpretation: it indicates the fraction of cases that must move from cells with excess observations to cells with deficit observations to eliminate association. Bootstrap confidence intervals are computed to quantify uncertainty.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Sakoda, J. M. (1981). A generalized index of dissimilarity. <em>Demography, 18</em>(2), 245–250.<br/>",
                               "Kvålseth, T. O. (2018a). An alternative to Cramér's coefficient of association. <em>Communications in Statistics - Theory and Methods, 47</em>(23), 5662–5674.<br/>",
                               "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.</p>")
            }
            
            if (self$options$sakodaLocal) {
                html <- paste0(html,
                               "<h3 style='color: #2874A6; margin-top: 1.5em;'>Sakoda's Local D (D<sub>ij</sub>) / PEM</h3>",
                               "<p>Sakoda's Local D (D<sub>ij</sub>) is the cell-level counterpart to D<sub>G</sub>, measuring how far each individual cell deviates from independence relative to its maximum possible deviation. This index is mathematically equivalent to Cibois's PEM (Pourcentage de l'Écart Maximum, or Percentage of Maximum deviation), differing only in presentation: Sakoda expresses the value as a proportion (−1 to +1), whilst Cibois multiplies by 100 to express it as a percentage (−100% to +100%).</p>",
                               "<p>The formula compares the observed deviation (n<sub>ij</sub> − e<sub>ij</sub>) to the maximum possible deviation given the marginal constraints. For cells with more observations than expected (attraction), the maximum is min(N<sub>i+</sub>, N<sub>+j</sub>) − e<sub>ij</sub>. For cells with fewer observations than expected (repulsion), the maximum is e<sub>ij</sub> − max(0, N<sub>i+</sub> + N<sub>+j</sub> − N).</p>",
                               "<p>A value of +1 indicates the cell has reached its maximum possible count given the marginals (complete attraction). A value of −1 indicates the cell is empty despite having a positive expected count (maximum repulsion). A value of 0 indicates the observed count equals the expected count (independence).</p>",
                               "<p><strong>Effect size interpretation and colour coding:</strong> Unlike global association measures (such as Cramér's V or Sakoda's D<sub>G</sub>) where Cohen's thresholds can be applied or scaled, no established statistical convention exists for interpreting cell-level effect sizes. The colour coding and thresholds used in this module to flag noteworthy D<sub>ij</sub> values are based on the empirical guidelines proposed by Cibois (1993) for PEM: values exceeding ±0.50 (equivalent to ±50% when expressed as PEM) are considered indicative of substantively meaningful attraction or repulsion. These thresholds are practical rules of thumb derived from applied experience rather than statistical theory, and users should exercise judgement based on their specific research context.</p>",
                               "<p><strong>Note:</strong> The rendered table provides point estimates without confidence intervals. For formal inference with bootstrap confidence intervals, use the <em>PEM</em> option in the <em>Post-hoc Analysis</em> facility of this module.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Sakoda, J. M. (1981). A generalized index of dissimilarity. <em>Demography, 18</em>(2), 245–250.<br/>",
                               "Cibois, P. (1993). Le PEM, pourcentage de l'écart maximum: Un indice de liaison entre modalités d'un tableau de contingence. <em>Bulletin de Méthodologie Sociologique, 40</em>, 43–63.</p>")
            }
            
            # Margin-free measures
            if (self$options$marginFree) {
                html <- paste0(html,
                               "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Margin-Free Measures of Association</h2>",
                               "<p>These measures are unaffected by marginal distributions and focus on the internal structure of association.</p>")
                
                html <- paste0(html,
                               "<h3 style='color: #2874A6; margin-top: 1.5em;'>Yule's Q</h3>",
                               "<p>A measure of association related to the odds ratio, used only for 2×2 contingency tables. It ranges from −1.0 (perfect negative association) to 1.0 (perfect positive association), with 0.0 indicating no association. It has a probabilistic interpretation in that it reflects how much more likely it is to draw concordant pairs as opposed to discordant pairs. Q has a symmetric PRE (Proportional Reduction in Error) interpretation, meaning that knowing one variable significantly improves the prediction of the other and this reduction in prediction errors applies equally whether predicting one variable from the other. Care is needed in interpreting Q, especially when the table contains zero frequencies, as it can reflect complete rather than absolute associations. Q is sensitive to unilateral (one-way) associations and is particularly appropriate for cross-tabs with unlike marginal sets and counts mainly polarised in three of the four cells, whereas the φ coefficient is sensitive to bilateral associations and possibly more appropriate for tables with like marginal sets and counts mainly concentrated along one diagonal. For tables larger than 2×2, Q is computed pairwise for all 2×2 sub-tables.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Yule, G. U. (1912). On the methods of measuring association between two attributes. <em>Journal of the Royal Statistical Society, 75</em>(6), 579–652.<br/>",
                               "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.<br/>",
                               "Bonett, D. G., & Price, R. M. (2007). Statistical inference for generalized Yule coefficients in 2×2 contingency tables. <em>Sociological Methods & Research, 35</em>(3), 429–446.<br/>",
                               "Sheskin, D. J. (2011). <em>Handbook of Parametric and Nonparametric Statistical Procedures</em> (5th ed.). Chapman and Hall/CRC.<br/>",
                               "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>")
                
                html <- paste0(html,
                               "<h3 style='color: #2874A6; margin-top: 1.5em;'>Yule's Y</h3>",
                               "<p>A measure of association for 2×2 contingency tables, introduced by George Udny Yule in 1912. Termed the coefficient of colligation, it is calculated using the square root of the odds ratio minus 1 in the numerator, and the square root of the OR plus 1 in the denominator. Y ranges from −1 to +1, with 0 indicating no association. A property of Y is its equivalence to the φ coefficient when calculated on a standardised table with equal marginals, making it inherently sensitive to bilateral associations. Y represents the difference between the probabilities in the diagonal and off-diagonal cells of the equivalent standardised table. It provides a more conservative measure compared to Yule's Q, particularly with extreme values or uneven distributions. For tables larger than 2×2, Y is computed pairwise for all 2×2 sub-tables.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Yule, G. U. (1912). On the methods of measuring association between two attributes. <em>Journal of the Royal Statistical Society, 75</em>(6), 579–652.<br/>",
                               "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.<br/>",
                               "Bonett, D. G., & Price, R. M. (2007). Statistical inference for generalized Yule coefficients in 2×2 contingency tables. <em>Sociological Methods & Research, 35</em>(3), 429–446.</p>")
                
                html <- paste0(html,
                               "<h3 style='color: #2874A6; margin-top: 1.5em;'>Odds Ratio</h3>",
                               "<p>A measure used in the analysis of cross-tabs for quantifying the strength and direction of the dependence between pairs of categorical variables. If a, b, c, and d are the cells of a 2×2 cross-tab, with a being the top-left and d being the bottom-right one, the OR is computed as (a × d) / (b × c). It represents the ratio of the product of the counts along one diagonal to the product of the off-diagonal counts. An OR of 1 indicates independence between two variables. If OR > 1, there is a positive association, while OR < 1 indicates a negative association. It is not reliant on the chi-squared statistic. For larger tables, the OR can be computed by partitioning the cross-tab into multiple 2×2 tables. By using all rows and columns of the larger table in a pairwise fashion, all possible ORs can be obtained, providing a comprehensive set that reflects the associations between different pairs of levels within the larger table.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.<br/>",
                               "Agresti, A. (2013). <em>Categorical Data Analysis</em> (3rd ed.). Wiley.<br/>",
                               "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>")
            }
            
            # PRE measures
            if (self$options$gkLambda || self$options$gkLambdaCorrected || self$options$gkTau) {
                html <- paste0(html,
                               "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Proportional Reduction in Error (PRE) Measures</h2>",
                               "<p>PRE measures quantify how much prediction error is reduced when using knowledge of one variable to predict another, compared to predicting without that knowledge.</p>")
                
                if (self$options$gkLambda) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Goodman-Kruskal Lambda (λ)</h3>",
                                   "<p>A measure of association based on the idea of Proportional Reduction in Error. It quantifies how much the error in predicting one variable (dependent) is reduced when using knowledge of another variable (independent). The coefficient is asymmetric, meaning that the value of λ can differ depending on which variable is considered independent. λ ranges from 0.0 to 1.0, inclusive. A value of 0.0 indicates that knowing the independent variable does not reduce any error in predicting the dependent variable, whilst a value of 1.0 indicates a complete elimination of prediction errors (100% reduction). For example, a λ of 0.42 can be interpreted as a 42% reduction in prediction errors for the dependent variable when using the independent variable compared to not using it. In square tables, when the counts are concentrated solely along one of the table's diagonals, λ reaches its maximum value of 1.0 for both variables, regardless of which is considered independent.</p>",
                                   "<p><strong>Confidence interval around Lambda:</strong> The confidence interval uses the asymptotic variance formula (Reynolds 1977: 50, equation 2.32) and is not truncated at zero, even though Lambda itself ranges from 0 to 1. This correctly reflects sampling uncertainty. The variance formula assumes multinomial sampling. Lambda has a degenerate sampling distribution at its boundaries: if the population λ = 0, the sample λ will always equal exactly 0; if the population λ = 1, the sample λ will always equal exactly 1. <em>Inference rules:</em> (1) If sample λ = 0 exactly, we cannot reject H₀: λ = 0, regardless of confidence interval width. (2) If sample λ differs from 0 (even by a tiny amount), we can definitively rule out that the population λ equals exactly zero—however, confidence intervals extending below zero indicate the population λ might be very close to (but not equal to) zero. A CI that includes zero does not mean 'no association' in the usual sense; rather, it indicates the population λ is definitely not exactly zero but could be negligibly small.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Goodman, L. A., & Kruskal, W. H. (1954). Measures of association for cross classifications. <em>Journal of the American Statistical Association, 49</em>(268), 732–764.<br/>",
                                   "Goodman, L. A., & Kruskal, W. H. (1972). Measures of association for cross classifications, IV: Simplification of asymptotic variances. <em>Journal of the American Statistical Association, 67</em>(338), 415–421.<br/>",
                                   "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.<br/>",
                                   "Goodman, L. A., & Kruskal, W. H. (1979). <em>Measures of Association for Cross Classifications</em>. Springer-Verlag.<br/>",
                                   "Bishop, Y. M., Fienberg, S. E., & Holland, P. W. (2007). <em>Discrete Multivariate Analysis: Theory and Practice</em>. Springer.</p>")
                }
                
                if (self$options$gkLambdaCorrected) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Goodman-Kruskal Lambda Corrected (λ<sup>K</sup>)</h3>",
                                   "<p>Kvålseth's Lambda corrected (λ<sup>K</sup>) is an alternative to the standard Goodman-Kruskal Lambda that addresses a well-known limitation: the standard λ can equal 0 even when the variables are not independent, simply because all row (or column) modal frequencies fall in the same column (or row). This tends to occur when marginal distributions are highly uneven.</p>",
                                   "<p>Like the standard λ, the corrected version is a PRE (Proportional Reduction in Error) measure based on the modal prediction strategy. The difference lies in how the overall probability of correct prediction is computed: standard λ uses the first-order (arithmetic) weighted mean of conditional prediction probabilities, whilst λ<sup>K</sup> uses the second-order (root mean square) weighted mean. Since RMS ≥ arithmetic mean, λ<sup>K</sup> ≥ λ always, with equality when λ = 0 under true independence or when λ = 1.</p>",
                                   "<p>The corrected Lambda ranges from 0 to 1 and has the same interpretation as standard Lambda: the proportional reduction in prediction error when using knowledge of one variable to predict the other. For 2×2 tables, λ<sup>K</sup> = 0 if and only if the variables are completely independent (unlike standard λ, which can equal 0 with dependence present).</p>",
                                   "<p>The symmetric version is defined as the maximum of the two asymmetric versions: λ<sup>K</sup> = max(λ<sup>K</sup><sub>Y|X</sub>, λ<sup>K</sup><sub>X|Y</sub>), which ensures it equals 1 when all nonzero probabilities fall on a longest diagonal, regardless of table dimensions.</p>",
                                   "<p><strong>Confidence interval:</strong> The confidence interval uses the delta-method asymptotic variance formula derived from partial derivatives of λ<sup>K</sup> with respect to cell probabilities.</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Kvålseth, T. O. (2018b). Measuring association between nominal categorical variables: An alternative to the Goodman-Kruskal lambda. <em>Journal of Applied Statistics, 45</em>(6), 1118–1132.</p>")
                }
                
                if (self$options$gkTau) {
                    html <- paste0(html,
                                   "<h3 style='color: #2874A6; margin-top: 1.5em;'>Goodman-Kruskal Tau (τ)</h3>",
                                   "<p>Goodman-Kruskal Tau is a PRE measure based on variance rather than mode prediction error. Unlike Lambda, which focuses on the modal category, Tau considers the entire distribution of the dependent variable and measures the proportional reduction in variance. It is more sensitive to the full distribution shape than λ. The coefficient is asymmetric, meaning separate values are computed depending on which variable is treated as dependent. Tau ranges from 0.0 (no reduction in prediction error) to 1.0 (perfect prediction).</p>")
                    html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                                   "Goodman, L. A., & Kruskal, W. H. (1954). Measures of association for cross classifications. <em>Journal of the American Statistical Association, 49</em>(268), 732–764.<br/>",
                                   "Goodman, L. A., & Kruskal, W. H. (1972). Measures of association for cross classifications, IV: Simplification of asymptotic variances. <em>Journal of the American Statistical Association, 67</em>(338), 415–421.<br/>",
                                   "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.<br/>",
                                   "Goodman, L. A., & Kruskal, W. H. (1979). <em>Measures of Association for Cross Classifications</em>. Springer-Verlag.<br/>",
                                   "Bishop, Y. M., Fienberg, S. E., & Holland, P. W. (2007). <em>Discrete Multivariate Analysis: Theory and Practice</em>. Springer.</p>")
                }
            }
            
            # Chi-square-maximising table
            if (self$options$showChisqMax) {
                html <- paste0(html,
                               "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Chi-Squared-Maximising Table</h2>",
                               "<p>A theoretically constructed contingency table that, while preserving the original table's marginal totals, arranges the cell frequencies in a way that maximises the chi-squared statistic and, consequently, the measures of association based on it. This table represents the greatest possible deviation from the expected frequencies under the assumption of independence, given the constraints imposed by the marginal totals. The chi-squared-maximising cross-tab is instrumental in determining the maximum achievable value of association measures for a given table structure, which is crucial for calculating the maximum-corrected versions of these measures. The construction involves an iterative process of allocating frequencies to cells based on the marginal totals, aiming to concentrate the levels of one variable into specific levels of the other to the maximum extent possible. The ratio between the observed value of an association measure and its maximum achievable value (derived from the chi-squared-maximising cross-tab) provides a normalised measure of association that can be compared across tables with different marginal configurations.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Berry, K. J., Johnston, J. E., & Mielke, P. W. (2018). <em>The Measurement of Association: A Permutation Statistical Approach</em>. Springer.</p>")
            }
            
            # Standardised table
            if (self$options$showStandardised) {
                html <- paste0(html,
                               "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Standardised Contingency Table</h2>",
                               "<p>A contingency table that has undergone iterative proportional fitting to adjust its cell frequencies. The standardisation process ensures that the marginal sums of the table align with predetermined values, typically to make different tables comparable in terms of their marginal distributions. Standardised cross-tabulations are used in the analysis of categorical data to reveal inherent relationships without the distortive effects of varying marginal totals, or to account for the influence of differing marginal distributions on measures of association, thereby facilitating their comparability across tables. The iterative proportional fitting technique involves recursively adjusting rows and columns until convergence is achieved.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Fienberg, S. E. (1971). A statistical technique for historians: Standardizing tables of counts. <em>The Journal of Interdisciplinary History, 1</em>(2), 305–315.<br/>",
                               "Smith, K. W. (1976). Marginal standardization and table shrinking: Aids in the traditional analysis of contingency tables. <em>Social Forces, 54</em>(3), 669–693.<br/>",
                               "Reynolds, H. T. (1977). <em>The Analysis of Cross-Classifications</em>. The Free Press.</p>")
            }
            
            # Effect size interpretation
            if (self$options$showThresholds) {
                html <- paste0(html,
                               "<h2 style='color: #2874A6; margin-top: 1.5em; font-size: 1.15em;'>Effect Size Interpretation</h2>",
                               "<p>The verbal articulation of effect size is based on Cohen (1988), with an enhanced approach (building on Olivier & Bell 2013) to account for the maximum achievable values of certain measures. Cohen's base thresholds (0.1, 0.3, 0.5) are adjusted using a two-tier system:</p>",
                               "<p><strong>Tier 1 – Table-Size Scaling (Cohen 1988):</strong> For tables larger than 2×2, base thresholds are divided by √[min(nr, nc) − 1]. This applies to all measures except Cohen's w. The scaling formulae are: small effect = 0.100 / √df; medium effect = 0.300 / √df; large effect = 0.500 / √df, where df corresponds to the table's minimum dimension minus 1.</p>",
                               "<p><strong>Tier 2 – Maximum-Value Adjustment (Olivier & Bell 2013):</strong> For uncorrected measures only, the table-size-scaled thresholds are then multiplied by the coefficient's maximum achievable value (Phi max, C max, V max, W-hat max, or Sakoda D<sub>G</sub> max). This compensates for marginal constraints that prevent these measures from reaching 1.0.</p>",
                               "<p><strong>Implementation by measure type:</strong> Uncorrected measures (Phi, Phi signed, C, Cramér's V, W-hat, Sakoda's D<sub>G</sub>) receive both Tier 1 scaling and Tier 2 adjustment. Corrected measures (Phi corrected, C adj, C corrected, V corrected, V standardised, bias-corrected V, W-hat corrected, Sakoda's D<sub>G</sub> corrected) receive Tier 1 scaling only. Cohen's w uses fixed thresholds (0.1, 0.3, 0.5) regardless of table size. Cole's C7 and Zysno's φ* also use Cohen's fixed thresholds (0.1, 0.3, 0.5) as they are margin-free measures with a guaranteed range of −1 to +1, making their values directly interpretable without scaling.</p>",
                               "<p>For margin-free measures (Yule's Q, Yule's Y, Odds Ratios), effect size thresholds are based on Ferguson (2009), who provides guidelines for Odds Ratios. Since Q and Y are monotonic transformations of the Odds Ratio, their thresholds are derived by applying the respective transformation functions to Ferguson's OR thresholds.</p>")
                html <- paste0(html, "<p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>References:</em><br/>",
                               "Cohen, J. (1988). <em>Statistical Power Analysis for the Behavioral Sciences</em> (2nd ed.). Erlbaum.<br/>",
                               "Ferguson, C. J. (2009). An effect size primer: A guide for clinicians and researchers. <em>Professional Psychology: Research and Practice, 40</em>(5), 532–538.<br/>",
                               "Olivier, J., & Bell, M. L. (2013). Effect sizes for 2×2 contingency tables. <em>PLoS ONE, 8</em>(3), e58777.</p>")
            }
            
            html <- paste0(html, "</div>")
            
            self$results$methodInfo$setContent(html)
        },
        
        # Helper functions (calculation methods)
        
        # ═══════════════════════════════════════════════════════════════════════════
        # START of Bootstrap infrastructure for corrected measures
        # ═══════════════════════════════════════════════════════════════════════════
        
        .generateBootstrapData = function(contingency_table, B) {
            # Generate B bootstrap resampled tables and precompute their max chi-squared values
            # This allows efficient reuse across multiple corrected measures
            
            n <- sum(contingency_table)
            probs <- as.vector(contingency_table) / n
            nr <- nrow(contingency_table)
            nc <- ncol(contingency_table)
            
            boot_tables <- vector("list", B)
            boot_max_chisq <- numeric(B)
            boot_chisq_obs <- numeric(B)
            
            for (b in 1:B) {
                # Resample from multinomial distribution
                boot_sample <- rmultinom(1, n, probs)
                boot_table <- matrix(boot_sample, nrow = nr, ncol = nc,
                                     dimnames = dimnames(contingency_table))
                boot_tables[[b]] <- boot_table
                
                # Compute observed chi-squared for this bootstrap sample
                boot_row_totals <- rowSums(boot_table)
                boot_col_totals <- colSums(boot_table)
                boot_expected <- outer(boot_row_totals, boot_col_totals) / n
                
                # Handle zero expected values
                term <- (boot_table - boot_expected)^2 / boot_expected
                term[boot_expected == 0] <- 0
                boot_chisq_obs[b] <- sum(term)
                
                # Compute max chi-squared for this bootstrap sample (marginals change!)
                max_result <- private$.max_chisq(boot_row_totals, boot_col_totals)
                boot_max_chisq[b] <- max_result$max_chisq
            }
            
            return(list(
                tables = boot_tables,
                chisq_obs = boot_chisq_obs,
                max_chisq = boot_max_chisq,
                n = n,
                nr = nr,
                nc = nc
            ))
        },
        
        .bootci_corrected = function(boot_data, stat_obs, stat_max_obs, conf_level, 
                                     stat_function = NULL, use_chisq_ratio = TRUE) {
            # Generic bootstrap CI for corrected measures
            # 
            # If use_chisq_ratio = TRUE: computes sqrt(chisq_obs / chisq_max) for each replicate
            # If use_chisq_ratio = FALSE: uses stat_function to compute statistic on each table,
            #                              then divides by max computed via stat_function on max table
            
            B <- length(boot_data$tables)
            boot_vals <- numeric(B)
            
            if (use_chisq_ratio) {
                # For phi_corrected, V_corrected, C_corrected: ratio of chi-squared values
                for (b in 1:B) {
                    if (boot_data$max_chisq[b] > 0) {
                        boot_vals[b] <- sqrt(boot_data$chisq_obs[b] / boot_data$max_chisq[b])
                    } else {
                        boot_vals[b] <- 0
                    }
                    # Bound at 1
                    if (boot_vals[b] > 1) boot_vals[b] <- 1
                }
            } else {
                # For W-hat corrected, Sakoda corrected: need to compute statistic and its max
                for (b in 1:B) {
                    tbl <- boot_data$tables[[b]]
                    stat_val <- stat_function(tbl)
                    
                    # Compute max table for this bootstrap sample
                    boot_row_totals <- rowSums(tbl)
                    boot_col_totals <- colSums(tbl)
                    max_result <- private$.max_chisq(boot_row_totals, boot_col_totals)
                    max_table <- max_result$max_table
                    
                    stat_max <- stat_function(max_table)
                    
                    if (stat_max > 0) {
                        boot_vals[b] <- stat_val / stat_max
                    } else {
                        boot_vals[b] <- 0
                    }
                    # Bound at 1
                    if (boot_vals[b] > 1) boot_vals[b] <- 1
                }
            }
            
            # Percentile CI
            alpha <- 1 - conf_level
            lower <- stats::quantile(boot_vals, alpha / 2, na.rm = TRUE)
            upper <- stats::quantile(boot_vals, 1 - alpha / 2, na.rm = TRUE)
            
            return(list(lower = as.numeric(lower), upper = as.numeric(upper)))
        },
        
        .bootci_C_adjusted = function(boot_data, conf_level) {
            # Bootstrap CI for C adjusted (theoretical max based on table dimensions)
            # C_max = sqrt((k-1)/k) where k = min(nr, nc)
            
            B <- length(boot_data$tables)
            k <- min(boot_data$nr, boot_data$nc)
            C_max_theoretical <- sqrt((k - 1) / k)
            
            boot_vals <- numeric(B)
            n <- boot_data$n
            
            for (b in 1:B) {
                # C = sqrt(chisq / (chisq + n))
                chisq_b <- boot_data$chisq_obs[b]
                C_b <- sqrt(chisq_b / (chisq_b + n))
                boot_vals[b] <- C_b / C_max_theoretical
                
                # Bound at 1
                if (boot_vals[b] > 1) boot_vals[b] <- 1
            }
            
            # Percentile CI
            alpha <- 1 - conf_level
            lower <- stats::quantile(boot_vals, alpha / 2, na.rm = TRUE)
            upper <- stats::quantile(boot_vals, 1 - alpha / 2, na.rm = TRUE)
            
            return(list(lower = as.numeric(lower), upper = as.numeric(upper)))
        },
        
        # ═══════════════════════════════════════════════════════════════════════════
        # END of Bootstrap infrastructure for corrected measures
        # ═══════════════════════════════════════════════════════════════════════════
        
        .max_chisq = function(row_totals, col_totals) {
            # Store dimensions
            nr <- length(row_totals)
            nc <- length(col_totals)
            n <- sum(row_totals)
            
            # Initialize max table
            max_table <- matrix(0, nrow = nr, ncol = nc)
            rownames(max_table) <- names(row_totals)
            colnames(max_table) <- names(col_totals)
            
            # Create working copies of totals
            row_remaining <- row_totals
            col_remaining <- col_totals
            
            # CORRECTED ALGORITHM (from CRAN package)
            # This strategically maximizes chi-square
            while (any(row_remaining > 0) && any(col_remaining > 0)) {
                # Step 1: Handle exact matches first (optimal strategy)
                for (i in seq_along(row_remaining)) {
                    for (j in seq_along(col_remaining)) {
                        if (row_remaining[i] == col_remaining[j] && row_remaining[i] > 0) {
                            max_table[i, j] <- row_remaining[i]
                            row_remaining[i] <- 0
                            col_remaining[j] <- 0
                        }
                    }
                }
                
                # Step 2: Pair largest remaining row with largest remaining column
                largest_row_idx <- which.max(row_remaining)
                largest_col_idx <- which.max(col_remaining)
                
                if (row_remaining[largest_row_idx] > 0 && col_remaining[largest_col_idx] > 0) {
                    smaller_of_two <- min(row_remaining[largest_row_idx], col_remaining[largest_col_idx])
                    max_table[largest_row_idx, largest_col_idx] <- smaller_of_two
                    row_remaining[largest_row_idx] <- row_remaining[largest_row_idx] - smaller_of_two
                    col_remaining[largest_col_idx] <- col_remaining[largest_col_idx] - smaller_of_two
                }
            }
            
            # Calculate chi-square on the maximising table
            expected <- outer(row_totals, col_totals) / n
            max_chisq <- sum((max_table - expected)^2 / expected)
            
            return(list(max_table = max_table, max_chisq = max_chisq))
        },
        
        
        .standardise_crosstabs = function(contingency_table, marginal.type = "average") {
            # Standardise the table to target margins by iterative
            # proportional fitting (IPF), preserving the odds-ratio structure
            nr <- nrow(contingency_table)
            nc <- ncol(contingency_table)
            n <- sum(contingency_table)
            
            if (marginal.type == "average") {
                target_row <- rep(n / nr, nr)
                target_col <- rep(n / nc, nc)
            } else {
                target_row <- rowSums(contingency_table)
                target_col <- colSums(contingency_table)
            }
            
            table_ipf <- contingency_table
            max_iter <- 100
            tol <- 1e-8
            
            for (iter in 1:max_iter) {
                # Adjust rows
                row_factors <- target_row / rowSums(table_ipf)
                table_ipf <- sweep(table_ipf, 1, row_factors, "*")
                
                # Adjust columns
                col_factors <- target_col / colSums(table_ipf)
                table_ipf <- sweep(table_ipf, 2, col_factors, "*")
                
                # Check convergence
                row_diff <- max(abs(rowSums(table_ipf) - target_row))
                col_diff <- max(abs(colSums(table_ipf) - target_col))
                
                if (row_diff < tol && col_diff < tol) {
                    break
                }
            }
            
            return(list(table.stand = table_ipf))
        },
        
        
        .wHat = function(contingency_table) {
            # Convert to proportions (joint probabilities)
            pij <- contingency_table / sum(contingency_table)
            
            # Calculate marginal probabilities
            pi_plus <- rowSums(pij)
            pj_plus <- colSums(pij)
            
            # Calculate d² (squared distance)
            d2 <- sum((pij - outer(pi_plus, pj_plus))^2)
            
            # Calculate normalization term (Kvålseth 2018 formula)
            normalization <- d2 - sum(pij^2) + min(sum(pi_plus^2), sum(pj_plus^2))
            
            # Calculate W-hat
            W_hat <- sqrt(d2) / sqrt(normalization)
            return(W_hat)
        },
        
        .sakoda = function(contingency_table) {
            # Sakoda's generalized index of dissimilarity (D_G)
            # Based on Sakoda (1981) Demography 18(2):245-250
            
            n <- sum(contingency_table)
            row_totals <- rowSums(contingency_table)
            col_totals <- colSums(contingency_table)
            
            # Expected frequencies under independence
            expected <- outer(row_totals, col_totals) / n
            
            # Numerator: sum of absolute deviations divided by 2
            numerator <- sum(abs(contingency_table - expected)) / 2
            
            # Denominator: sum of N × P_j × (1 - P_j) across all columns
            p_j <- col_totals / n
            denominator <- sum(n * p_j * (1 - p_j))
            
            # Calculate D_G
            D_G <- numerator / denominator
            
            return(D_G)
        },
        
        # Cole's C7 with Ratliff's (1982) correction
        # References: Cole (1949) Ecology 30:411-424; Ratliff (1982) Ecology 63:1605-1606
        # Note: Not χ²-based; normalises (ad-bc) directly
        .coleC7 = function(contingency_table) {
            x <- as.matrix(contingency_table)
            a <- x[1, 1]; b <- x[1, 2]; c <- x[2, 1]; d <- x[2, 2]
            
            diff <- a*d - b*c
            
            # Perfect association cases
            if(diff > 0 && (b == 0 || c == 0)) return(1.0)
            if(diff < 0 && (a == 0 || d == 0)) return(-1.0)
            if(diff == 0) return(0.0)
            
            # Apply Ratliff's formulas
            if(diff >= 0) {
                if(c <= b) {
                    denom <- (a + b) * (b + d)
                } else {
                    denom <- (a + c) * (c + d)
                }
            } else {
                if(a <= d) {
                    denom <- (a + b) * (a + c)
                } else {
                    denom <- (b + d) * (c + d)
                }
            }
            
            return(diff / denom)
        },
        
        # Zysno's φ* coefficient
        # Reference: Zysno (1997) Methods Psychol Res Online 2:41-52
        # Note: Not χ²-based; normalises (ad-bc) directly
        .zysnoPhi = function(contingency_table) {
            x <- as.matrix(contingency_table)
            a <- x[1, 1]; b <- x[1, 2]; c <- x[2, 1]; d <- x[2, 2]
            
            diff <- a*d - b*c
            N <- a + b + c + d
            
            # Perfect association cases
            if(diff > 0 && min(b, c) == 0) return(1.0)
            if(diff < 0 && min(a, d) == 0) return(-1.0)
            if(diff == 0) return(0.0)
            
            # Error term
            if(diff >= 0) {
                e <- min(b, c)
            } else {
                e <- min(a, d)
            }
            
            return(diff / (abs(diff) + N * e))
        },
        
        .gkLambda = function(contingency_table, direction = "row") {
            nr <- nrow(contingency_table)
            nc <- ncol(contingency_table)
            n <- sum(contingency_table)
            
            sr <- rowSums(contingency_table)
            sc <- colSums(contingency_table)
            
            if (direction == "row") {
                # Row-dependent (CRAN formula)
                E1 <- n - max(sr)
                E2 <- sum(sc - apply(contingency_table, 2, max))
                lambda <- (E1 - E2) / E1
            } else if (direction == "col") {
                # Column-dependent (CRAN formula)
                E1 <- n - max(sc)
                E2 <- sum(sr - apply(contingency_table, 1, max))
                lambda <- (E1 - E2) / E1
            } else {  # symmetric
                # Use proper Goodman-Kruskal symmetric formula
                max_col_wise <- apply(contingency_table, 2, max)
                max_row_wise <- apply(contingency_table, 1, max)
                
                numerator <- (sum(max_col_wise) + sum(max_row_wise)) - max(sr) - max(sc)
                denominator <- (2*n) - max(sr) - max(sc)
                lambda <- numerator / denominator
            }
            
            return(max(0, lambda))
        },
        
        .gkLambdaCI = function(contingency_table, direction = "row", conf_level) {
            nr <- nrow(contingency_table)
            nc <- ncol(contingency_table)
            n <- sum(contingency_table)
            
            sr <- rowSums(contingency_table)
            sc <- colSums(contingency_table)
            
            alpha <- 1 - conf_level
            z <- stats::qnorm(1 - alpha / 2)
            
            if (direction == "row") {
                # Row-dependent lambda variance (Goodman & Kruskal 1963)
                col_maxima <- apply(contingency_table, 2, max)
                sum_fmj <- sum(col_maxima)
                max_row_index <- which.max(sr)
                sum_fmj_star <- sum(contingency_table[max_row_index, ] * 
                                        (contingency_table[max_row_index, ] == col_maxima))
                
                numerator <- (n - sum_fmj) * (sum_fmj + max(sr) - 2 * sum_fmj_star)
                denominator <- (n - max(sr))^3
                lambda_var <- numerator / denominator
                
            } else if (direction == "col") {
                # Column-dependent lambda variance
                row_maxima <- apply(contingency_table, 1, max)
                sum_fmi <- sum(row_maxima)
                max_col_index <- which.max(sc)
                sum_fmi_star <- sum(contingency_table[, max_col_index] * 
                                        (contingency_table[, max_col_index] == row_maxima))
                
                numerator <- (n - sum_fmi) * (sum_fmi + max(sc) - 2 * sum_fmi_star)
                denominator <- (n - max(sc))^3
                lambda_var <- numerator / denominator
                
            } else {  # symmetric
                # Symmetric lambda variance
                max_col_wise <- apply(contingency_table, 2, max)
                max_row_wise <- apply(contingency_table, 1, max)
                sum_fmj <- sum(max_col_wise)
                sum_fmi <- sum(max_row_wise)
                
                max_row_index <- which.max(sr)
                max_col_index <- which.max(sc)
                
                sum_fmj_star <- sum(contingency_table[max_row_index, ] * 
                                        (contingency_table[max_row_index, ] == max_col_wise))
                sum_fmi_star <- sum(contingency_table[, max_col_index] * 
                                        (contingency_table[, max_col_index] == max_row_wise))
                
                numerator <- (2*n - sum_fmi - sum_fmj) * 
                    (sum_fmi + sum_fmj + max(sr) + max(sc) - 
                         2*(sum_fmi_star + sum_fmj_star))
                denominator <- (2*n - max(sr) - max(sc))^3
                lambda_var <- numerator / denominator
            }
            
            lambda_se <- sqrt(lambda_var)
            lambda <- private$.gkLambda(contingency_table, direction)
            
            lower <- lambda - z * lambda_se
            upper <- lambda + z * lambda_se
            
            return(c(lower, upper))
        },
        
        .gkLambdaCorrected = function(contingency_table, direction = "row") {
            # Convert to proportions
            pij <- contingency_table / sum(contingency_table)
            
            nr <- nrow(pij)
            nc <- ncol(pij)
            
            # Calculate marginals
            pi_plus <- rowSums(pij)
            p_plusj <- colSums(pij)
            
            # Calculate maximums for rows and columns
            pim <- apply(pij, 1, max)  # Max within each row
            pmj <- apply(pij, 2, max)  # Max within each column
            
            if (direction == "row") {
                # Lambda Y|X corrected (Kvålseth 2018b)
                numerator <- sqrt(sum(pim^2 / pi_plus)) - max(p_plusj)
                denominator <- 1 - max(p_plusj)
                lambda_corr <- numerator / denominator
                
            } else if (direction == "col") {
                # Lambda X|Y corrected (Kvålseth 2018b)
                numerator <- sqrt(sum(pmj^2 / p_plusj)) - max(pi_plus)
                denominator <- 1 - max(pi_plus)
                lambda_corr <- numerator / denominator
                
            } else {  # symmetric
                # Symmetric lambda corrected (proper weighted formula, not average)
                numerator <- sqrt(sum(pim^2 / pi_plus)) + sqrt(sum(pmj^2 / p_plusj)) - 
                    max(p_plusj) - max(pi_plus)
                denominator <- 2 - max(p_plusj) - max(pi_plus)
                lambda_corr <- numerator / denominator
            }
            
            return(max(0, min(1, lambda_corr)))
        },
        
        .gkLambdaCorrectedCI = function(contingency_table, direction = "row", conf_level) {
            # Compute CI for Kvålseth's Lambda corrected using delta method
            # Following equations (34)-(35d) from Kvålseth (2018)
            
            n <- sum(contingency_table)
            nr <- nrow(contingency_table)
            nc <- ncol(contingency_table)
            
            # Convert to proportions
            pij <- contingency_table / n
            pi_plus <- rowSums(pij)
            p_plusj <- colSums(pij)
            
            # Row and column maxima
            pim <- apply(pij, 1, max)
            pmj <- apply(pij, 2, max)
            
            # Modal column and row indices (for marginal maxima)
            p_plus_m <- max(p_plusj)
            pm_plus <- max(pi_plus)
            col_m <- which.max(p_plusj)  # Column index of marginal max
            row_m <- which.max(pi_plus)  # Row index of marginal max
            
            alpha <- 1 - conf_level
            z <- stats::qnorm(1 - alpha / 2)
            
            if (direction == "row") {
                # Lambda Y|X corrected (row-dependent: rows predict columns)
                # c = sqrt(sum(pim^2 / pi_plus))
                c_val <- sqrt(sum(pim^2 / pi_plus))
                lambda_corr <- (c_val - p_plus_m) / (1 - p_plus_m)
                
                # Identify which column contains each row's modal value
                modal_cols <- apply(pij, 1, which.max)
                
                # Compute partial derivatives (equations 35a-35d)
                delta_ij <- matrix(0, nrow = nr, ncol = nc)
                
                for (i in 1:nr) {
                    for (j in 1:nc) {
                        is_modal_cell <- (j == modal_cols[i])
                        is_modal_column <- (j == col_m)
                        
                        if (is_modal_cell && is_modal_column) {
                            # Equation (35a): pim in same column as p+m
                            delta_ij[i,j] <- (1 / (1 - p_plus_m)) * (
                                (1 / (2 * c_val)) * (1 - (1 - pim[i] / pi_plus[i])^2) - 
                                    1 + lambda_corr
                            )
                        } else if (is_modal_cell && !is_modal_column) {
                            # Equation (35b): pim not in same column as p+m
                            delta_ij[i,j] <- (1 / (1 - p_plus_m)) * (
                                (1 / (2 * c_val)) * (1 - (1 - pim[i] / pi_plus[i])^2)
                            )
                        } else if (!is_modal_cell && is_modal_column) {
                            # Equation (35c): pij (not modal) in same column as p+m
                            delta_ij[i,j] <- (1 / (1 - p_plus_m)) * (
                                -(1 / (2 * c_val)) * (pim[i] / pi_plus[i])^2 -
                                    1 + lambda_corr
                            )
                        } else {
                            # Equation (35d): pij (not modal) not in modal column
                            delta_ij[i,j] <- (1 / (1 - p_plus_m)) * (
                                -(1 / (2 * c_val)) * (pim[i] / pi_plus[i])^2
                            )
                        }
                    }
                }
                
            } else if (direction == "col") {
                # Lambda X|Y corrected (column-dependent: columns predict rows)
                # Same logic but with rows and columns swapped
                c_val <- sqrt(sum(pmj^2 / p_plusj))
                lambda_corr <- (c_val - pm_plus) / (1 - pm_plus)
                
                # Identify which row contains each column's modal value
                modal_rows <- apply(pij, 2, which.max)
                
                # Compute partial derivatives (transposed logic)
                delta_ij <- matrix(0, nrow = nr, ncol = nc)
                
                for (i in 1:nr) {
                    for (j in 1:nc) {
                        is_modal_cell <- (i == modal_rows[j])
                        is_modal_row <- (i == row_m)
                        
                        if (is_modal_cell && is_modal_row) {
                            delta_ij[i,j] <- (1 / (1 - pm_plus)) * (
                                (1 / (2 * c_val)) * (1 - (1 - pmj[j] / p_plusj[j])^2) - 
                                    1 + lambda_corr
                            )
                        } else if (is_modal_cell && !is_modal_row) {
                            delta_ij[i,j] <- (1 / (1 - pm_plus)) * (
                                (1 / (2 * c_val)) * (1 - (1 - pmj[j] / p_plusj[j])^2)
                            )
                        } else if (!is_modal_cell && is_modal_row) {
                            delta_ij[i,j] <- (1 / (1 - pm_plus)) * (
                                -(1 / (2 * c_val)) * (pmj[j] / p_plusj[j])^2 -
                                    1 + lambda_corr
                            )
                        } else {
                            delta_ij[i,j] <- (1 / (1 - pm_plus)) * (
                                -(1 / (2 * c_val)) * (pmj[j] / p_plusj[j])^2
                            )
                        }
                    }
                }
                
            } else {  # symmetric
                # For symmetric version, use the maximum of the two asymmetric versions
                # as per equation (32): λ^K = max{λ^K_Y|X, λ^K_X|Y}
                lambda_row_corr <- private$.gkLambdaCorrected(contingency_table, "row")
                lambda_col_corr <- private$.gkLambdaCorrected(contingency_table, "col")
                
                if (lambda_row_corr >= lambda_col_corr) {
                    # Use row-dependent CI
                    return(private$.gkLambdaCorrectedCI(contingency_table, "row", conf_level))
                } else {
                    # Use column-dependent CI
                    return(private$.gkLambdaCorrectedCI(contingency_table, "col", conf_level))
                }
            }
            
            # Compute variance using equation (34)
            # σ² = Σᵢ Σⱼ pᵢⱼ δᵢⱼ² - (Σᵢ Σⱼ pᵢⱼ δᵢⱼ)²
            sigma_sq <- sum(pij * delta_ij^2) - (sum(pij * delta_ij))^2
            
            # Standard error
            se <- sqrt(max(0, sigma_sq)) / sqrt(n)
            
            # Confidence interval
            lower <- lambda_corr - z * se
            upper <- lambda_corr + z * se
            
            return(c(lower, upper))
        },
        
        .gkTau = function(contingency_table, direction = "row") {
            # Note: direction "row" means rows are dependent on columns
            # This matches the reference implementation where tau.row.dep <- calc.tau(t(df))
            
            if (direction == "row") {
                # Rows dependent: transpose the table
                x <- t(contingency_table)
            } else {
                # Columns dependent: use table as-is
                x <- contingency_table
            }
            
            # Total prediction errors without knowledge of columns
            tot_n_errors_rowwise <- sum(((sum(x) - rowSums(x)) / sum(x)) * rowSums(x))
            
            # Prediction errors with knowledge of columns
            errors_col_wise <- matrix(nrow = nrow(x), ncol = ncol(x))
            for (i in 1:nrow(x)) {
                for (j in 1:ncol(x)) {
                    errors_col_wise[i,j] <- ((colSums(x)[j] - x[i,j]) / colSums(x)[j]) * x[i,j]
                }
            }
            
            # Tau is the proportional reduction in error
            tau <- (tot_n_errors_rowwise - sum(errors_col_wise)) / tot_n_errors_rowwise
            
            return(max(0, min(1, tau)))
        },
        
        .gkTauCI = function(contingency_table, direction = "row", conf_level) {
            # Compute tau standard error using the proper Goodman-Kruskal formula
            
            # Helper function matching the uploaded compute_single_se
            compute_single_se <- function(P) {
                N <- sum(P)
                P <- P / N
                pi_plus <- rowSums(P)
                p_plus_j <- colSums(P)
                
                # Calculate v using equation 11.3-22
                v <- sum(sapply(1:nrow(P), function(i) {
                    sapply(1:ncol(P), function(j) {
                        if(p_plus_j[j] > 0) (P[i,j] - pi_plus[i]*p_plus_j[j])^2/p_plus_j[j] else 0
                    })
                }))
                
                # Calculate delta (denominator term)
                delta <- 1 - sum(pi_plus^2)
                
                # Initialize variance sum for equation 11.3-24
                var_sum <- 0
                
                # Calculate variance components
                for(i in 1:nrow(P)) {
                    for(j in 1:ncol(P)) {
                        if(P[i,j] == 0) next
                        term1 <- 2 * v * sum(pi_plus[-i])
                        term2 <- 2 * sum(P[-i,j]/p_plus_j[j])
                        term3 <- sum(sapply(1:ncol(P), function(l) {
                            if(p_plus_j[l] > 0) sum(P[-i,l]/p_plus_j[l]) else 0
                        }))
                        bracket_term <- term1 - delta * (term2 - term3)
                        var_sum <- var_sum + (P[i,j] * bracket_term^2)
                    }
                }
                
                # Return standard error
                sqrt(max(var_sum / (N * delta^4), 0))
            }
            
            # Calculate SE based on direction
            if (direction == "row") {
                # Row-dependent: use original table
                tau_se <- compute_single_se(contingency_table)
            } else {
                # Column-dependent: use transposed table
                tau_se <- compute_single_se(t(contingency_table))
            }
            
            tau <- private$.gkTau(contingency_table, direction)
            
            alpha <- 1 - conf_level
            z <- stats::qnorm(1 - alpha / 2)
            lower <- tau - z * tau_se
            upper <- tau + z * tau_se
            
            return(c(lower, upper))
        },
        
        .interpretChiSqEffect = function(value, measure_type, nr, nc, contingency_table, expected, phi_max = NULL, c_max = NULL, v_max = NULL, w_hat_max = NULL, sakoda_max = NULL) {
            # Two-tier effect size interpretation (Cohen 1988; Olivier-Bell 2013)
            k <- min(nr - 1, nc - 1)
            
            # Base Cohen thresholds
            base_small <- 0.100
            base_medium <- 0.300
            base_large <- 0.500
            
            # TIER 1: Table-size scaling
            if (measure_type == "cohen_w") {
                # Cohen's w: NO scaling (neither tier)
                small <- base_small
                medium <- base_medium
                large <- base_large
            } else {
                # All other measures: apply Tier 1 scaling
                if (k > 0) {
                    small <- base_small / sqrt(k)
                    medium <- base_medium / sqrt(k)
                    large <- base_large / sqrt(k)
                } else {
                    small <- base_small
                    medium <- base_medium
                    large <- base_large
                }
            }
            
            # TIER 2: Maximum-value adjustment (ONLY for uncorrected measures)
            uncorrected_measures <- c("phi", "phi_signed", "contingency_c", "cramers_v", "w_hat", "sakoda")
            
            if (measure_type %in% uncorrected_measures) {
                # Apply Tier 2 adjustment
                if (measure_type == "phi" && !is.null(phi_max)) {
                    small <- small * phi_max
                    medium <- medium * phi_max
                    large <- large * phi_max
                } else if (measure_type == "phi_signed" && !is.null(phi_max)) {
                    small <- small * phi_max
                    medium <- medium * phi_max
                    large <- large * phi_max
                } else if (measure_type == "contingency_c" && !is.null(c_max)) {
                    small <- small * c_max
                    medium <- medium * c_max
                    large <- large * c_max
                } else if (measure_type == "cramers_v" && !is.null(v_max)) {
                    small <- small * v_max
                    medium <- medium * v_max
                    large <- large * v_max
                } else if (measure_type == "w_hat" && !is.null(w_hat_max)) {
                    small <- small * w_hat_max
                    medium <- medium * w_hat_max
                    large <- large * w_hat_max
                } else if (measure_type == "sakoda" && !is.null(sakoda_max)) {
                    small <- small * sakoda_max
                    medium <- medium * sakoda_max
                    large <- large * sakoda_max
                }
            }
            
            # Corrected measures: Tier 1 only (already applied above)
            # measure_type: "phi_corrected", "contingency_c_adj", "contingency_c_corrected", 
            #               "cramers_v_corrected", "cramers_v_standardised", "cramers_v_bias_corrected"
            # W-hat and Sakoda: Tier 1 only
            
            # Apply thresholds (a missing value has no interpretation)
            if (is.na(value)) return("")
            if (value < small) return("Negligible")
            if (value < medium) return("Small")
            if (value < large) return("Medium")
            return("Large")
        },
        
        .interpretMarginFreeEffect = function(value, measure_type) {
            # Effect size interpretation for margin-free measures
            if (is.null(value) || is.na(value)) return("")
            if (measure_type == "yules_q") {
                abs_val <- abs(value)
                if (abs_val < 0.330) return("Negligible")
                if (abs_val < 0.500) return("Small")
                if (abs_val < 0.600) return("Medium")
                return("Large")
            } else if (measure_type == "yules_y") {
                abs_val <- abs(value)
                if (abs_val < 0.171) return("Negligible")
                if (abs_val < 0.268) return("Small")
                if (abs_val < 0.333) return("Medium")
                return("Large")
            } else if (measure_type == "cole_c7" || measure_type == "zysno_phi") {
                # Cohen's standard thresholds (margin-free measures with range -1 to +1)
                abs_val <- abs(value)
                if (abs_val < 0.100) return("Negligible")
                if (abs_val < 0.300) return("Small")
                if (abs_val < 0.500) return("Medium")
                return("Large")
            } else if (measure_type == "odds_ratio") {
                if (value < 2.0 && value >= 1/2.0) return("Negligible")
                if ((value >= 2.0 && value < 3.0) || (value < 1/2.0 && value >= 1/3.0)) return("Small")
                if ((value >= 3.0 && value < 4.0) || (value < 1/3.0 && value >= 1/4.0)) return("Medium")
                return("Large")
            }
            
            return("")
        }
    )
)