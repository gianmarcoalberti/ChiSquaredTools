# This file is a generated template, your changes will not be overwritten

#' @export
chisqposthocClass <- R6::R6Class(
  "chisqposthocClass",
  inherit = chisqposthocBase,
  private = list(
    
    # -------------------------------------------------------------------------
    # Initialise: populate dynamic options
    # -------------------------------------------------------------------------
    .init = function() {
      
      # Early return if no variables selected
      if (is.null(self$options$rows) || is.null(self$options$cols))
        return()
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      data <- self$data
      
      # Ensure factors
      if (!is.factor(data[[rowVar]]))
        data[[rowVar]] <- as.factor(data[[rowVar]])
      if (!is.factor(data[[colVar]]))
        data[[colVar]] <- as.factor(data[[colVar]])
      
      # Get levels
      row_levels <- levels(data[[rowVar]])
      col_levels <- levels(data[[colVar]])
      
      I <- length(row_levels)
      J <- length(col_levels)
      
      # Store for later use in .run()
      private$.rowLevels <- row_levels
      private$.colLevels <- col_levels
      private$.I <- I
      private$.J <- J
      
      # ---------------------------------------------------------------------
      # 1. Crosstab Table - pre-build structure (uses setRow in .run)
      # ---------------------------------------------------------------------
      table <- self$results$crosstabTable
      table$setTitle(paste0(rowVar, " × ", colVar))
      
      table$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (j in seq_len(J)) {
        table$addColumn(name = paste0('col', j), title = col_levels[j], type = 'integer', superTitle = colVar)
      }
      table$addColumn(name = 'rowtotal', title = 'Total', type = 'integer')
      
      # Pre-add rows (including total) - these will be filled with setRow()
      for (i in seq_len(I)) {
        table$addRow(rowKey = paste0('row', i), values = list(
          rowname = row_levels[i],
          rowtotal = ''
        ))
      }
      table$addRow(rowKey = 'total', values = list(rowname = 'Total', rowtotal = ''))
      
      # ---------------------------------------------------------------------
      # 2. Pre-build metric tables that use setRow() pattern
      # ---------------------------------------------------------------------
      private$.prebuildMetricTable('stdresTable', 'Standardised Residuals', row_levels, col_levels, rowVar, colVar)
      private$.prebuildMetricTable('momcorrstdresTable', 'Moment-Corrected Standardised Residuals', row_levels, col_levels, rowVar, colVar)
      private$.prebuildMetricTable('adjstdresTable', 'Adjusted Standardised Residuals', row_levels, col_levels, rowVar, colVar)
      private$.prebuildMetricTable('queteletTable', 'Quetelet Index', row_levels, col_levels, rowVar, colVar)
      private$.prebuildMetricTable('ijTable', 'IJ Association Factor', row_levels, col_levels, rowVar, colVar)
      private$.prebuildMetricTable('medpolishTable', 'Standardised Median Polish Residuals', row_levels, col_levels, rowVar, colVar)
      private$.prebuildMetricTable('adjmedpolishTable', 'Adjusted Standardised Median Polish Residuals', row_levels, col_levels, rowVar, colVar)
      
      # ---------------------------------------------------------------------
      # 3. Tables that are built entirely in .run() - DO NOT pre-build rows
      # ---------------------------------------------------------------------
      # PEM, GK residuals, DEP, BS Outlier tables are populated dynamically
      # with addRow() in their respective populate methods, so we don't
      # pre-build rows here.
      
      # depOutcome dropdown default
      if (!is.null(self$options$cols)) {
        if (colVar %in% names(self$data)) {
          col_data <- self$data[[colVar]]
          levels_list <- levels(as.factor(col_data))
          depOutcome_option <- self$options$depOutcome
          if (is.null(depOutcome_option) || !(depOutcome_option %in% levels_list)) {
            private$.defaultDepOutcome <- levels_list[1]
          }
        }
      }
      
      # ---------------------------------------------------------------------
      # 4. Pre-build column structure for tables with dynamic columns
      #    These tables use addRow() in .run(), so we need columns ready
      # ---------------------------------------------------------------------
      
      # PEM Table - matrix format like other metrics
      private$.prebuildMetricTable('pemTable', 'PEM with Confidence Intervals', row_levels, col_levels, rowVar, colVar)
      
      # GK Residuals Tables (Columns as Predictor)
      gk_col_table <- self$results$gkresColTable
      gk_col_table$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (j in seq_along(col_levels)) {
        gk_col_table$addColumn(name = paste0('col', j), title = col_levels[j], type = 'text', superTitle = colVar)
      }
      
      # GK Residuals Tables (Rows as Predictor)
      gk_row_table <- self$results$gkresRowTable
      gk_row_table$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (j in seq_along(col_levels)) {
        gk_row_table$addColumn(name = paste0('col', j), title = col_levels[j], type = 'text', superTitle = colVar)
      }
      
      # Pre-build GK Col table rows
      for (i in seq_len(I)) {
        gk_col_table$addRow(rowKey = i, values = list(rowname = row_levels[i]))
      }
      
      # Pre-build GK Row table rows
      for (i in seq_len(I)) {
        gk_row_table$addRow(rowKey = i, values = list(rowname = row_levels[i]))
      }
      
      # BS Outlier Matrix Table
      bs_matrix_table <- self$results$bsOutlierMatrixTable
      bs_matrix_table$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (j in seq_along(col_levels)) {
        bs_matrix_table$addColumn(name = paste0('col', j), title = col_levels[j], type = 'text', superTitle = colVar)
      }
      
      # Pre-build BS Outlier Matrix rows
      for (i in seq_len(I)) {
        bs_matrix_table$addRow(rowKey = i, values = list(rowname = row_levels[i]))
      }
      
      # Pre-build BS Outlier Detail table rows (up to bsKmax)
      bs_detail_table <- self$results$bsOutlierDetailTable
      # (at most I x J - 1 cells can be set aside, as in the computation)
      k_max <- min(self$options$bsKmax, I * J - 1)
      for (k in seq_len(k_max)) {
        bs_detail_table$addRow(rowKey = k, values = list(rank = k))
      }
      
      # DEP Table - columns are already defined in YAML
      dep_table <- self$results$depTable
      dep_table$setTitle(paste0("Dependence Evaluator Proportion (DEP): ", rowVar, " → ", colVar))
      
      # Pre-build DEP rows (one per row level)
      for (i in seq_len(I)) {
        dep_table$addRow(rowKey = i, values = list(rowCat = row_levels[i]))
      }
    },
    
    .prebuildMetricTable = function(table_name, title_base, row_levels, col_levels, rowVar, colVar) {
      table <- self$results[[table_name]]
      table$setTitle(paste0(title_base, ": ", rowVar, " × ", colVar))
      
      table$addColumn(name = 'rowname', title = rowVar, type = 'text')
      for (j in seq_along(col_levels)) {
        table$addColumn(name = paste0('col', j), title = col_levels[j], type = 'number', superTitle = colVar)
      }
      
      I <- length(row_levels)
      for (i in seq_len(I)) {
        table$addRow(rowKey = paste0('row', i), values = list(rowname = row_levels[i]))
      }
    },
    
    .run = function() {
      
      if (is.null(self$options$rows) || is.null(self$options$cols)) {
        return()
      }
      
      # === CLEAR ALL POSSIBLE FOOTNOTES FIRST ===
      self$results$stdresTable$setNote('interp_stdres', NULL, init = FALSE)
      self$results$momcorrstdresTable$setNote('interp_momcorr', NULL, init = FALSE)
      self$results$adjstdresTable$setNote('interp_adjstdres', NULL, init = FALSE)
      self$results$queteletTable$setNote('interp_quetelet', NULL, init = FALSE)
      self$results$ijTable$setNote('interp_ij', NULL, init = FALSE)
      self$results$medpolishTable$setNote('interp_medpolish', NULL, init = FALSE)
      self$results$adjmedpolishTable$setNote('interp_adjmedpolish', NULL, init = FALSE)
      self$results$pemTable$setNote('interp_pem', NULL, init = FALSE)
      self$results$gkresColTable$setNote('interp_gkres', NULL, init = FALSE)
      self$results$gkresRowTable$setNote('interp_gkres', NULL, init = FALSE)
      self$results$depTable$setNote('interp_dep', NULL, init = FALSE)
      self$results$bsOutlierMatrixTable$setNote('interp_bsoutlier', NULL, init = FALSE)
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      data <- self$data
      
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
      
      # Always populate the crosstab
      private$.populateCrosstab(contingency_table)
      
      n <- sum(contingency_table)
      row_totals <- rowSums(contingency_table)
      col_totals <- colSums(contingency_table)
      expected <- outer(row_totals, col_totals) / n
      
      # Convert confidence level from percentage to proportion once
      private$.confLevelProp <- self$options$confLevel / 100
      
      if (self$options$stdres) {
        stdres <- private$.computeStandardisedResiduals(contingency_table, expected)
        # Compute threshold (Šidák-corrected or standard)
        if (self$options$sidakCorrection) {
          k <- private$.I * private$.J
          alpha <- 0.05
          alpha_corr <- 1 - (1 - alpha)^(1/k)
          stdres_threshold <- qnorm(1 - alpha_corr / 2)
        } else {
          stdres_threshold <- 1.96
        }
        private$.populatePlainMetricTable(stdres, 'stdresTable', 
                                          threshold = stdres_threshold, 
                                          threshold_type = "abs")
      }
      
      if (self$options$momcorrstdres) {
        momcorrstdres <- private$.computeMomentCorrectedStandardisedResiduals(contingency_table, expected)
        # Compute threshold (Šidák-corrected or standard)
        if (self$options$sidakCorrection) {
          k <- private$.I * private$.J
          alpha <- 0.05
          alpha_corr <- 1 - (1 - alpha)^(1/k)
          momcorr_threshold <- qnorm(1 - alpha_corr / 2)
        } else {
          momcorr_threshold <- 1.96
        }
        private$.populatePlainMetricTable(momcorrstdres, 'momcorrstdresTable', 
                                          threshold = momcorr_threshold, 
                                          threshold_type = "abs")
      }
      
      if (self$options$adjstdres) {
        adjstdres <- private$.computeAdjustedStandardisedResiduals(contingency_table, expected, n)
        # Compute threshold (Šidák-corrected or standard)
        if (self$options$sidakCorrection) {
          k <- private$.I * private$.J
          alpha <- 0.05
          alpha_corr <- 1 - (1 - alpha)^(1/k)
          adjstdres_threshold <- qnorm(1 - alpha_corr / 2)
        } else {
          adjstdres_threshold <- 1.96
        }
        private$.populatePlainMetricTable(adjstdres, 'adjstdresTable', 
                                          threshold = adjstdres_threshold, 
                                          threshold_type = "abs")
      }
      
      if (self$options$quetelet) {
        quetelet <- private$.computeQueteletIndex(contingency_table, expected)
        private$.populatePlainMetricTable(quetelet, 'queteletTable', 
                                          threshold = TRUE, 
                                          threshold_type = "empirical_q")
      }
      
      if (self$options$ij) {
        ij <- private$.computeIJAssociation(contingency_table, expected)
        private$.populatePlainMetricTable(ij, 'ijTable', 
                                          threshold = TRUE, 
                                          threshold_type = "empirical_ij")
      }
      
      if (self$options$bsOutlier) {
        bsoutlier_results <- private$.computeBackwardsSteppingOutliers(contingency_table)
        private$.populateBSOutlierMatrixTable(bsoutlier_results, rowVar, colVar)
        private$.populateBSOutlierDetailTable(bsoutlier_results, rowVar, colVar)
      }
      
      if (self$options$pem) {
        # Check if bootstrap recomputation is actually needed
        if (private$.needsPEMRecomputation(contingency_table)) {
          pem_results <- private$.computePEM(contingency_table)
          private$.cachePEMMetadata(contingency_table)
        } else {
          # Use cached results
          pem_results <- private$.lastPEM
        }
        
        private$.populatePEMTable(pem_results, rowVar, colVar)
        
        # Prepare PEM plot if requested
        if (self$options$showPemPlot) {
          private$.preparePemPlotData(pem_results, rowVar, colVar)
        }
      }
      
      if (self$options$medpolish || self$options$adjmedpolish) {
        mp_results <- private$.computeMedianPolishResiduals(contingency_table)
        
        if (self$options$medpolish) {
          # Use boxplot fence thresholds for median polish
          mp_vals <- as.vector(mp_results$pearson_mp_residuals)
          F_U <- as.numeric(quantile(mp_vals, 0.75))
          F_L <- as.numeric(quantile(mp_vals, 0.25))
          d_F <- F_U - F_L
          mp_lower <- F_L - 1.5 * d_F
          mp_upper <- F_U + 1.5 * d_F
          
          private$.populatePlainMetricTable(mp_results$pearson_mp_residuals, 'medpolishTable', 
                                            threshold = c(mp_lower, mp_upper), 
                                            threshold_type = "fence")
        }
        
        if (self$options$adjmedpolish) {
          # Use boxplot fence thresholds for adjusted median polish
          adjmp_vals <- as.vector(mp_results$haberman_mp_residuals)
          adjmp_F_U <- as.numeric(quantile(adjmp_vals, 0.75))
          adjmp_F_L <- as.numeric(quantile(adjmp_vals, 0.25))
          adjmp_d_F <- adjmp_F_U - adjmp_F_L
          adjmp_lower <- adjmp_F_L - 1.5 * adjmp_d_F
          adjmp_upper <- adjmp_F_U + 1.5 * adjmp_d_F
          
          private$.populatePlainMetricTable(mp_results$haberman_mp_residuals, 'adjmedpolishTable', 
                                            threshold = c(adjmp_lower, adjmp_upper), 
                                            threshold_type = "fence")
        }
      }
      
      if (self$options$gkres) {
        gk_results <- private$.computeGKResiduals(contingency_table)
        private$.populateGKResidualTable(gk_results$col_predictor, 'gkresColTable', 
                                         'Goodman-Kruskal Residuals (Columns as Predictor)', 
                                         rowVar, colVar, 'col')
        private$.populateGKResidualTable(gk_results$row_predictor, 'gkresRowTable', 
                                         'Goodman-Kruskal Residuals (Rows as Predictor)', 
                                         rowVar, colVar, 'row')
      }
      
      if (self$options$dep) {
        dep_results <- private$.computeDEP(contingency_table, colVar)
        private$.populateDEPTable(dep_results, rowVar, colVar)
        
        if (self$options$showDepPlot) {
          private$.prepareDepPlotData(dep_results)
        }
      }
      
      private$.populateMethodInfo()
      
      # === APPLY FOOTNOTES (MUST BE AT THE END FOR RELIABLE RENDERING) ===
      if (self$options$stdres) private$.addStdResNotice()
      if (self$options$momcorrstdres) private$.addMomCorrStdResNotice()
      if (self$options$adjstdres) private$.addAdjStdResNotice()
      if (self$options$quetelet) private$.addQueteletNotice()
      if (self$options$ij) private$.addIJNotice()
      if (self$options$medpolish) private$.addMedPolishNotice()
      if (self$options$adjmedpolish) private$.addAdjMedPolishNotice()
      if (self$options$pem) private$.addPEMNotice()
      if (self$options$gkres) private$.addGKResNotice()
      if (self$options$dep) private$.addDEPNotice()
      if (self$options$bsOutlier) private$.addBSOutlierNotice()
    },
    
    .populateCrosstab = function(contingency_table) {
      
      table <- self$results$crosstabTable
      
      I <- private$.I
      J <- private$.J
      row_levels <- private$.rowLevels
      col_levels <- private$.colLevels
      
      # Fill pre-existing rows
      for (i in seq_len(I)) {
        row_values <- list()
        for (j in seq_len(J)) {
          row_values[[paste0("col", j)]] <- contingency_table[i, j]
        }
        row_values[['rowtotal']] <- sum(contingency_table[i, ])
        
        table$setRow(rowKey = paste0('row', i), values = row_values)
      }
      
      # Total row
      total_values <- list()
      for (j in seq_len(J)) {
        total_values[[paste0("col", j)]] <- sum(contingency_table[, j])
      }
      total_values[['rowtotal']] <- sum(contingency_table)
      
      table$setRow(rowKey = 'total', values = total_values)
    },
    
    .computeStandardisedResiduals = function(observed, expected) {
      
      stdres <- (observed - expected) / sqrt(expected)
      
      private$.lastStdRes <- stdres
      
      return(stdres)
    },
    
    .computeMomentCorrectedStandardisedResiduals = function(observed, expected) {
      
      stdres <- (observed - expected) / sqrt(expected)
      
      nr <- nrow(observed)
      nc <- ncol(observed)
      
      correction_factor <- sqrt((nr - 1) * (nc - 1) / (nr * nc))
      
      momcorrstdres <- stdres / correction_factor
      
      private$.lastMomCorrStdRes <- momcorrstdres
      
      return(momcorrstdres)
    },
    
    .computeAdjustedStandardisedResiduals = function(observed, expected, n) {
      
      row_props <- rowSums(observed) / n
      col_props <- colSums(observed) / n
      
      I <- nrow(observed)
      J <- ncol(observed)
      
      adjstdres <- matrix(NA, nrow = I, ncol = J)
      
      for (i in 1:I) {
        for (j in 1:J) {
          adjustment <- sqrt((1 - row_props[i]) * (1 - col_props[j]))
          adjstdres[i, j] <- (observed[i, j] - expected[i, j]) / 
            (sqrt(expected[i, j]) * adjustment)
        }
      }
      
      dimnames(adjstdres) <- dimnames(observed)
      
      private$.lastAdjStdRes <- adjstdres
      
      return(adjstdres)
    },
    
    .addStdResNotice = function() {
      table <- self$results$stdresTable
      
      if (self$options$sidakCorrection) {
        k <- private$.I * private$.J
        alpha <- 0.05
        alpha_corr <- 1 - (1 - alpha)^(1/k)
        threshold <- qnorm(1 - alpha_corr / 2)
        notice_text <- paste0(
          "Values greater than ", sprintf("%.3f", threshold),
          " or less than ", sprintf("%.3f", -threshold),
          " indicate significant deviation (Šidák-corrected α = ", sprintf("%.4f", alpha_corr), "). ",
          "See Agresti (2013)."
        )
      } else {
        notice_text <- paste0(
          "Values greater than 1.96 or less than -1.96 indicate significant deviation (α = 0.05). ",
          "See Agresti (2013)."
        )
      }
      
      table$setNote('interp_stdres', notice_text, init = FALSE)  # Table-wide footer note
    },
    
    .addMomCorrStdResNotice = function() {
      table <- self$results$momcorrstdresTable
      
      if (self$options$sidakCorrection) {
        k <- private$.I * private$.J
        alpha <- 0.05
        alpha_corr <- 1 - (1 - alpha)^(1/k)
        threshold <- qnorm(1 - alpha_corr / 2)
        notice_text <- sprintf(
          "Moment-corrected residuals adjust for table dimensions. Values > %.3f or < %.3f indicate significant deviation (Šidák-corrected α = %.4f). See García-Pérez & Núñez-Antón (2003).",
          threshold, -threshold, alpha_corr
        )
      } else {
        notice_text <- "Moment-corrected residuals adjust for table dimensions. Values > 1.96 or < -1.96 indicate significant deviation (α = 0.05). See García-Pérez & Núñez-Antón (2003)."
      }
      
      table$setNote('interp_momcorr', notice_text, init = FALSE)
    },
    
    .addAdjStdResNotice = function() {
      table <- self$results$adjstdresTable
      
      if (self$options$sidakCorrection) {
        k <- private$.I * private$.J
        alpha <- 0.05
        alpha_corr <- 1 - (1 - alpha)^(1/k)
        threshold <- qnorm(1 - alpha_corr / 2)
        notice_text <- sprintf(
          "Adjusted residuals account for unequal marginal totals. Values > %.3f or < %.3f indicate significant deviation (Šidák-corrected α = %.4f). See Haberman (1973).",
          threshold, -threshold, alpha_corr
        )
      } else {
        notice_text <- "Adjusted residuals account for unequal marginal totals. Values > 1.96 or < -1.96 indicate significant deviation (α = 0.05). See Haberman (1973)."
      }
      
      table$setNote('interp_adjstdres', notice_text, init = FALSE)
    },
    
    .addQueteletNotice = function() {
      table <- self$results$queteletTable
      notice_text <- "Quetelet Index = (observed/expected) − 1. Empirical thresholds: >1.0 noteworthy positive, <-0.50 noteworthy negative. See Mirkin (2001, 2023)."
      table$setNote('interp_quetelet', notice_text, init = FALSE)
    },
    
    .addIJNotice = function() {
      table <- self$results$ijTable
      notice_text <- "IJ Factor = observed/expected. Empirical thresholds: >2.0 noteworthy positive, <0.5 noteworthy negative. See Good (1956); Agresti (2013)."
      table$setNote('interp_ij', notice_text, init = FALSE)
    },
    
    .addMedPolishNotice = function() {
      table <- self$results$medpolishTable
      
      if (is.null(private$.lastMedPolish)) return()
      
      all_values <- as.vector(private$.lastMedPolish)
      n_cells <- length(all_values)
      F_U <- as.numeric(quantile(all_values, 0.75))
      F_L <- as.numeric(quantile(all_values, 0.25))
      d_F <- F_U - F_L
      lower_cutoff <- F_L - 1.5 * d_F
      upper_cutoff <- F_U + 1.5 * d_F
      expected_outliers <- 0.007 * n_cells + 0.4
      n_outliers <- sum(all_values < lower_cutoff | all_values > upper_cutoff)
      
      if (n_outliers > round(expected_outliers) + 1) {
        outlier_text <- sprintf("Found %d extreme cells vs. ~%.1f expected; excess suggests genuine outliers.", n_outliers, expected_outliers)
      } else if (n_outliers <= round(expected_outliers)) {
        outlier_text <- sprintf("Found %d extreme cell(s), within %.1f expected; consistent with chance.", n_outliers, expected_outliers)
      } else {
        outlier_text <- sprintf("Found %d extreme cell(s) vs. ~%.1f expected; modest excess.", n_outliers, expected_outliers)
      }
      
      notice_text <- sprintf(
        "Fourth-spread rule: F_L = %.3f, F_U = %.3f, d_F = %.3f; cutoffs = %.3f / %.3f. %s See Mosteller & Parunak (1985); Simonoff (2003).",
        F_L, F_U, d_F, lower_cutoff, upper_cutoff, outlier_text
      )
      
      table$setNote('interp_medpolish', notice_text, init = FALSE)
    },
    
    .addAdjMedPolishNotice = function() {
      table <- self$results$adjmedpolishTable
      
      if (is.null(private$.lastAdjMedPolish)) return()
      
      all_values <- as.vector(private$.lastAdjMedPolish)
      n_cells <- length(all_values)
      F_U <- as.numeric(quantile(all_values, 0.75))
      F_L <- as.numeric(quantile(all_values, 0.25))
      d_F <- F_U - F_L
      lower_cutoff <- F_L - 1.5 * d_F
      upper_cutoff <- F_U + 1.5 * d_F
      expected_outliers <- 0.007 * n_cells + 0.4
      n_outliers <- sum(all_values < lower_cutoff | all_values > upper_cutoff)
      
      if (n_outliers > round(expected_outliers) + 1) {
        outlier_text <- sprintf("Found %d extreme cells vs. ~%.1f expected; excess suggests genuine outliers.", n_outliers, expected_outliers)
      } else if (n_outliers <= round(expected_outliers)) {
        outlier_text <- sprintf("Found %d extreme cell(s), within %.1f expected; consistent with chance.", n_outliers, expected_outliers)
      } else {
        outlier_text <- sprintf("Found %d extreme cell(s) vs. ~%.1f expected; modest excess.", n_outliers, expected_outliers)
      }
      
      notice_text <- sprintf(
        "Haberman-adjusted. Fourth-spread rule: F_L = %.3f, F_U = %.3f, d_F = %.3f; cutoffs = %.3f / %.3f. %s See Mosteller & Parunak (1985).",
        F_L, F_U, d_F, lower_cutoff, upper_cutoff, outlier_text
      )
      
      table$setNote('interp_adjmedpolish', notice_text, init = FALSE)
    },
    
    .addPEMNotice = function() {
      table <- self$results$pemTable
      
      conf_level <- self$options$confLevel
      n_reps <- self$options$bootstrapReps
      
      notice_text <- sprintf(
        "PEM (Sakoda's D Local × 100) ranges -100%% to +100%%. Empirical thresholds: <5%% negligible, 5-10%% weak, ≥10%% interesting/noteworthy, ≥50%% exceptional. Significance: %.0f%% bootstrap CI (%d reps) excludes zero. See Cibois (1993); Lefèvre & Champely (2009); Sakoda (1981).",
        conf_level, n_reps
      )
      
      table$setNote('interp_pem', notice_text, init = FALSE)
    },
    
    .addGKResNotice = function() {
      notice_text <- "GK residuals = P(response|predictor) - P(response). Positive = predictor increases probability; negative = decreases. No formal thresholds. See Kroonenberg & Lombardo (1999)."
      
      self$results$gkresColTable$setNote('interp_gkres', notice_text, init = FALSE)
      self$results$gkresRowTable$setNote('interp_gkres', notice_text, init = FALSE)
    },
    
    .addDEPNotice = function() {
      table <- self$results$depTable
      
      notice_text <- "Here DEP treats rows as independent (predictor) and columns as dependent (outcome). DEP ranges -1 to +1. Positive = outcome more probable; negative = less probable. Significance via chi-squared bounds (α = 0.05). See Gambirasio (2019)."
      
      table$setNote('interp_dep', notice_text, init = FALSE)
    },
    
    .addBSOutlierNotice = function() {
      table <- self$results$bsOutlierMatrixTable
      
      if (is.null(private$.lastBSOutlier)) return()
      
      results <- private$.lastBSOutlier
      detected_count <- results$detected_count
      k_max <- results$k_max
      critical_value <- results$critical_value
      nr <- nrow(results$del_res_matrix)
      nc <- ncol(results$del_res_matrix)
      RC <- nr * nc
      
      # Compute Simonoff's recommended range (20-30% of cells)
      simonoff_low <- max(1, ceiling(0.20 * RC))
      simonoff_high <- max(1, ceiling(0.30 * RC))
      
      if (detected_count > 0) {
        detection_text <- sprintf("Detected %d outlier cell(s).", detected_count)
      } else {
        detection_text <- "No outlier cells detected."
      }
      
      notice_text <- sprintf(
        "%s Tested %d cells (k_max = %d; Simonoff suggests %d-%d for this %d x %d table). Cells flagged when G² drop > %.3f (χ²₁, α = 0.05/%d). See Simonoff (1988)",
        detection_text, k_max, k_max, simonoff_low, simonoff_high, nr, nc, critical_value, RC
      )
      
      table$setNote('interp_bsoutlier', notice_text, init = FALSE)
    },
    
    # -------------------------------------------------------------------------
    # Populate PEM table
    # -------------------------------------------------------------------------
    .populatePEMTable = function(pem_results, rowVar, colVar) {
      
      table <- self$results$pemTable
      
      conf_level <- self$options$confLevel
      table$setTitle(paste0("PEM (%) with ", conf_level, "% CI: ", rowVar, " × ", colVar))
      
      pem <- pem_results$pem
      ci_lower <- pem_results$ci_lower
      ci_upper <- pem_results$ci_upper
      
      I <- private$.I
      J <- private$.J
      
      for (i in seq_len(I)) {
        row_values <- list()
        for (j in seq_len(J)) {
          pem_val <- pem[i, j]
          ci_lo <- ci_lower[i, j]
          ci_hi <- ci_upper[i, j]
          
          # Format as "value [lower, upper]" with colour if CI excludes zero
          if (is.na(pem_val)) {
            cell_text <- ""
          } else {
            # Check if CI excludes zero (significant)
            # isTRUE(): a missing bootstrap bound must not stop the analysis
            ci_excludes_zero <- isTRUE(ci_lo > 0) || isTRUE(ci_hi < 0)
            
            if (ci_excludes_zero) {
              # Apply red colour to significant values
              cell_text <- sprintf(
                "<span style='color: #C00000;'>%.1f [%.1f, %.1f]</span>", 
                pem_val, ci_lo, ci_hi
              )
            } else {
              cell_text <- sprintf("%.1f [%.1f, %.1f]", pem_val, ci_lo, ci_hi)
            }
          }
          
          row_values[[paste0("col", j)]] <- cell_text
        }
        table$setRow(rowKey = paste0('row', i), values = row_values)
      }
    },
    
    .computeQueteletIndex = function(observed, expected) {
      
      QI <- (observed - expected) / expected
      
      private$.lastQuetelet <- QI
      
      return(QI)
    },
    
    .computeIJAssociation = function(observed, expected) {
      
      IJ <- observed / expected
      
      private$.lastIJ <- IJ
      
      return(IJ)
    },
    
    .computePEM = function(contingency_table) {
      
      set.seed(self$options$seed)
      
      N_matrix <- as.matrix(contingency_table)
      I <- nrow(N_matrix)
      J <- ncol(N_matrix)
      N <- sum(N_matrix)
      
      N_iplus <- rowSums(N_matrix)
      N_plusj <- colSums(N_matrix)
      
      E_ij <- outer(N_iplus, N_plusj) / N
      
      pem_local <- matrix(NA, nrow = I, ncol = J)
      
      for (i in 1:I) {
        for (j in 1:J) {
          n_ij <- N_matrix[i, j]
          e_ij <- E_ij[i, j]
          
          if (n_ij >= e_ij) {
            max_ij <- min(N_iplus[i], N_plusj[j])
            if (max_ij > e_ij) {
              pem_local[i, j] <- ((n_ij - e_ij) / (max_ij - e_ij)) * 100
            } else {
              pem_local[i, j] <- 0
            }
          } else {
            # Compute minimum possible value considering structural constraints
            min_ij <- max(0, N_iplus[i] + N_plusj[j] - N)
            pem_local[i, j] <- -((e_ij - n_ij) / (e_ij - min_ij)) * 100
          }
        }
      }
      
      dimnames(pem_local) <- dimnames(N_matrix)
      
      B <- self$options$bootstrapReps
      alpha <- 1 - private$.confLevelProp
      
      pem_boot <- array(NA, dim = c(I, J, B))
      
      # Build expanded data correctly: one row per observation
      # Iterate through cells to ensure correct row/col assignment
      expanded_rows <- integer(0)
      expanded_cols <- integer(0)
      for (i in 1:I) {
        for (j in 1:J) {
          count <- N_matrix[i, j]
          if (count > 0) {
            expanded_rows <- c(expanded_rows, rep(i, count))
            expanded_cols <- c(expanded_cols, rep(j, count))
          }
        }
      }
      expanded_data <- data.frame(row = expanded_rows, col = expanded_cols)
      
      for (b in 1:B) {
        boot_indices <- sample(1:nrow(expanded_data), replace = TRUE)
        boot_sample <- expanded_data[boot_indices, ]
        
        # Convert to factors with fixed levels to ensure table dimensions match original
        boot_row_factor <- factor(boot_sample$row, levels = 1:I)
        boot_col_factor <- factor(boot_sample$col, levels = 1:J)
        boot_table <- table(boot_row_factor, boot_col_factor)
        
        boot_N_iplus <- rowSums(boot_table)
        boot_N_plusj <- colSums(boot_table)
        boot_N <- sum(boot_table)
        
        # Avoid division by zero if bootstrap sample is degenerate
        if (boot_N == 0) next
        
        boot_E_ij <- outer(boot_N_iplus, boot_N_plusj) / boot_N
        
        for (i in 1:I) {
          for (j in 1:J) {
            boot_n_ij <- boot_table[i, j]
            boot_e_ij <- boot_E_ij[i, j]
            
            # Handle edge case where expected is zero
            if (boot_e_ij == 0) {
              pem_boot[i, j, b] <- NA
              next
            }
            
            if (boot_n_ij >= boot_e_ij) {
              boot_max_ij <- min(boot_N_iplus[i], boot_N_plusj[j])
              if (boot_max_ij > boot_e_ij) {
                pem_boot[i, j, b] <- ((boot_n_ij - boot_e_ij) / 
                                        (boot_max_ij - boot_e_ij)) * 100
              } else {
                pem_boot[i, j, b] <- 0
              }
            } else {
              boot_min_ij <- max(0, boot_N_iplus[i] + boot_N_plusj[j] - boot_N)
              denom <- boot_e_ij - boot_min_ij
              if (denom > 0) {
                pem_boot[i, j, b] <- -((boot_e_ij - boot_n_ij) / denom) * 100
              } else {
                pem_boot[i, j, b] <- 0
              }
            }
          }
        }
      }
      
      ci_lower <- matrix(NA, nrow = I, ncol = J)
      ci_upper <- matrix(NA, nrow = I, ncol = J)
      
      for (i in 1:I) {
        for (j in 1:J) {
          boot_values <- pem_boot[i, j, ]
          ci_lower[i, j] <- quantile(boot_values, alpha / 2, na.rm = TRUE)
          ci_upper[i, j] <- quantile(boot_values, 1 - alpha / 2, na.rm = TRUE)
        }
      }
      
      dimnames(ci_lower) <- dimnames(N_matrix)
      dimnames(ci_upper) <- dimnames(N_matrix)
      
      private$.lastPEM <- list(
        pem = pem_local,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      )
      
      return(list(
        pem = pem_local,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      ))
    },
    
    # -------------------------------------------------------------------------
    # Check if PEM bootstrap needs recomputation
    # -------------------------------------------------------------------------
    .needsPEMRecomputation = function(contingency_table) {
      
      # Create simple fingerprint of current data
      current_data_hash <- paste(
        paste(dim(contingency_table), collapse = "x"),
        sum(contingency_table),
        paste(round(as.vector(contingency_table), 4), collapse = ","),
        collapse = "|"
      )
      
      # Capture bootstrap-relevant options
      current_bootstrap_options <- list(
        bootstrapReps = self$options$bootstrapReps,
        confLevel = private$.confLevelProp,
        seed = self$options$seed
      )
      
      # Check if cached results exist
      if (is.null(private$.lastPEM)) {
        return(TRUE)
      }
      
      # Check if data changed
      if (is.null(private$.lastDataHash) || 
          private$.lastDataHash != current_data_hash) {
        return(TRUE)
      }
      
      # Check if bootstrap options changed
      if (is.null(private$.lastBootstrapOptions) ||
          !identical(private$.lastBootstrapOptions, current_bootstrap_options)) {
        return(TRUE)
      }
      
      return(FALSE)
    },
    
    # -------------------------------------------------------------------------
    # Store cache metadata after PEM computation
    # -------------------------------------------------------------------------
    .cachePEMMetadata = function(contingency_table) {
      
      private$.lastDataHash <- paste(
        paste(dim(contingency_table), collapse = "x"),
        sum(contingency_table),
        paste(round(as.vector(contingency_table), 4), collapse = ","),
        collapse = "|"
      )
      
      private$.lastBootstrapOptions <- list(
        bootstrapReps = self$options$bootstrapReps,
        confLevel = private$.confLevelProp,
        seed = self$options$seed
      )
    },
    
    .computeMedianPolishResiduals = function(contingency_table) {
      
      log_counts <- log(as.matrix(contingency_table) + 0.001)
      
      median_polish_result <- medpolish(log_counts, trace.iter = FALSE)
      
      log_expected <- outer(median_polish_result$row,
                            median_polish_result$col, "+") +
        median_polish_result$overall
      robust_expected <- exp(log_expected)
      
      pearson_mp_residuals <- (as.matrix(contingency_table) - robust_expected) /
        sqrt(robust_expected)
      
      n <- sum(contingency_table)
      row_props <- rowSums(contingency_table)/n
      col_props <- colSums(contingency_table)/n
      
      I <- nrow(contingency_table)
      J <- ncol(contingency_table)
      
      adj_factors <- matrix(0, nrow = I, ncol = J)
      for (i in 1:I) {
        for (j in 1:J) {
          adj_factors[i,j] <- sqrt((1 - row_props[i]) * (1 - col_props[j]))
        }
      }
      
      haberman_mp_residuals <- pearson_mp_residuals / adj_factors
      
      dimnames(pearson_mp_residuals) <- dimnames(contingency_table)
      dimnames(haberman_mp_residuals) <- dimnames(contingency_table)
      
      private$.lastMedPolish <- pearson_mp_residuals
      private$.lastAdjMedPolish <- haberman_mp_residuals
      
      return(list(
        pearson_mp_residuals = pearson_mp_residuals,
        haberman_mp_residuals = haberman_mp_residuals
      ))
    },
    
    # -------------------------------------------------------------------------
    # Compute Goodman-Kruskal Residuals
    # -------------------------------------------------------------------------
    .computeGKResiduals = function(contingency_table) {
      
      # Convert to matrix
      df <- as.matrix(contingency_table)
      nr <- nrow(df)
      nc <- ncol(df)
      n <- sum(df)
      
      # Row and column sums
      sr <- rowSums(df)
      sc <- colSums(df)
      
      # ---- Columns as predictor (rows as response) ----
      # Column profiles: proportion within each column
      prop_col <- df / matrix(rep(sc, each = nr), nrow = nr)
      
      # Overall row proportions (marginal distribution of response)
      overall_row_props <- sr / n
      overall_row_props_matrix <- matrix(rep(overall_row_props, nc), ncol = nc)
      
      # GK residuals: column profile minus marginal row proportions
      gk_res_col <- prop_col - overall_row_props_matrix
      
      # ---- Rows as predictor (columns as response) ----
      # Row profiles: proportion within each row
      prop_row <- df / matrix(rep(sr, nc), ncol = nc)
      
      # Overall column proportions (marginal distribution of response)
      overall_col_props <- sc / n
      overall_col_props_matrix <- matrix(rep(overall_col_props, each = nr), nrow = nr)
      
      # GK residuals: row profile minus marginal column proportions
      gk_res_row <- prop_row - overall_col_props_matrix
      
      # Preserve dimnames
      dimnames(gk_res_col) <- dimnames(contingency_table)
      dimnames(gk_res_row) <- dimnames(contingency_table)
      
      # Store for later use
      private$.lastGKResCol <- gk_res_col
      private$.lastGKResRow <- gk_res_row
      
      return(list(
        col_predictor = gk_res_col,
        row_predictor = gk_res_row
      ))
    },
    
    # -------------------------------------------------------------------------
    # Compute Backwards-Stepping Outlier Detection (Simonoff 1988)
    # -------------------------------------------------------------------------
    .computeBackwardsSteppingOutliers = function(contingency_table) {
      
      # Get user-specified k_max or use default (20% of cells, minimum 1)
      k_max <- self$options$bsKmax
      if (is.null(k_max) || k_max < 1) {
        k_max <- max(1, floor(0.2 * length(contingency_table)))
      }
      
      # Fixed alpha = 0.05 for consistency with other methods
      alpha <- 0.05
      
      # Convergence parameters for quasi-independence fitting
      max_iter <- 1000
      tol <- 1e-6
      
      # Convert to matrix
      data_table <- as.matrix(contingency_table)
      nr <- nrow(data_table)
      nc <- ncol(data_table)
      RC <- nr * nc
      
      # Ensure k_max does not exceed RC - 1
      k_max <- min(k_max, RC - 1)
      
      # Row and column names for labelling
      row_names <- rownames(data_table)
      col_names <- colnames(data_table)
      if (is.null(row_names)) row_names <- paste0("Row", 1:nr)
      if (is.null(col_names)) col_names <- paste0("Col", 1:nc)
      
      # -----------------------------------------------------------------------
      # Helper function: Fit quasi-independence model via Newton algorithm
      # (Bishop, Fienberg & Holland 1975, Appendix A)
      # -----------------------------------------------------------------------
      fit_quasi_independence <- function(n_ij, delta_ij) {
        # n_ij: observed counts (matrix)
        # delta_ij: indicator matrix (1 = included, 0 = deleted)
        
        # Initialise column effects
        b_j <- rep(1, ncol(n_ij))
        
        for (v in 1:max_iter) {
          b_j_prev <- b_j
          
          # Row totals for non-deleted cells
          n_i_dot <- rowSums(n_ij * delta_ij)
          
          # Estimate row effects: a_i = n_i. / sum_j(delta_ij * b_j)
          denom_a <- as.vector(delta_ij %*% b_j)
          a_i <- ifelse(denom_a == 0, 0, n_i_dot / denom_a)
          
          # Column totals for non-deleted cells
          n_dot_j <- colSums(n_ij * delta_ij)
          
          # Estimate column effects: b_j = n_.j / sum_i(delta_ij * a_i)
          denom_b <- as.vector(t(delta_ij) %*% a_i)
          b_j <- ifelse(denom_b == 0, 0, n_dot_j / denom_b)
          
          # Check convergence
          if (all(abs(b_j - b_j_prev) < tol)) break
        }
        
        # Fitted values: e_ij = delta_ij * a_i * b_j
        e_ij <- outer(a_i, b_j) * delta_ij
        
        # Likelihood ratio statistic G^2
        valid <- (n_ij > 0) & (e_ij > 0)
        G2 <- 2 * sum(n_ij[valid] * log(n_ij[valid] / e_ij[valid]))
        
        return(list(expected = e_ij, G2 = G2, a_i = a_i, b_j = b_j))
      }
      
      # -----------------------------------------------------------------------
      # Phase 1: Identification (stepping out)
      # -----------------------------------------------------------------------
      
      # Delta matrix tracks which cells are still "in" the model
      delta_current <- matrix(1, nr, nc)
      
      identified_cells <- list()
      deleted_residuals <- rep(NA_real_, k_max)
      g2_drop_values <- rep(NA_real_, k_max)
      residual_signs <- rep(NA_real_, k_max)
      
      for (i in 1:k_max) {
        
        # Fit model to currently included cells
        fit_current <- fit_quasi_independence(data_table, delta_current)
        g2_current <- fit_current$G2
        
        # Find the most outlying cell among those still included
        max_abs_del_res <- -Inf
        most_outlying <- NULL
        best_g2_deleted <- NA
        best_del_res <- NA
        
        for (r in 1:nr) {
          for (c in 1:nc) {
            # Skip cells already identified
            if (delta_current[r, c] == 0) next
            
            # Temporarily delete this cell
            delta_temp <- delta_current
            delta_temp[r, c] <- 0
            
            # Fit model with cell deleted
            fit_deleted <- fit_quasi_independence(data_table, delta_temp)
            
            # Expected value under deleted model
            # (reconstruct what e_rc would be if we hadn't deleted it)
            e_star_rc <- fit_deleted$a_i[r] * fit_deleted$b_j[c]
            
            # Deleted residual (equation 1.2)
            n_rc <- data_table[r, c]
            
            # Skip cells where expected value is effectively zero
            # (indicates model collapse for this cell)
            if (e_star_rc < 1e-6) next
            
            r_star <- (n_rc - e_star_rc) / sqrt(e_star_rc)
            
            if (abs(r_star) > max_abs_del_res) {
              max_abs_del_res <- abs(r_star)
              most_outlying <- c(r, c)
              best_g2_deleted <- fit_deleted$G2
              best_del_res <- r_star
            }
          }
        }
        
        # Record the i-th most extreme cell
        # If no valid cell could be found, stop identification early
        if (is.null(most_outlying)) {
          break
        }
        
        # Record the i-th most extreme cell
        identified_cells[[i]] <- most_outlying
        deleted_residuals[i] <- best_del_res
        residual_signs[i] <- sign(best_del_res)
        
        # G^2 drop for this cell
        T_i <- g2_current - best_g2_deleted
        g2_drop_values[i] <- T_i
        
        # Mark this cell as deleted for subsequent iterations
        delta_current[most_outlying[1], most_outlying[2]] <- 0
      }
      
      # -----------------------------------------------------------------------
      # Phase 2: Testing (stepping back)
      # -----------------------------------------------------------------------
      
      # Bonferroni-corrected critical value
      alpha_bonf <- alpha / RC
      critical_value <- qchisq(1 - alpha_bonf, df = 1)
      
      # Test from least extreme (k_max) to most extreme (1)
      detected_count <- 0
      for (i in k_max:1) {
        # Skip NA values (cells that couldn't be evaluated)
        if (is.na(g2_drop_values[i])) next
        if (g2_drop_values[i] > critical_value) {
          detected_count <- i
          break
        }
      }
      
      # -----------------------------------------------------------------------
      # Prepare results
      # -----------------------------------------------------------------------
      
      # Build matrix of deleted residuals for colour-coded display
      del_res_matrix <- matrix(NA, nr, nc)
      dimnames(del_res_matrix) <- list(row_names, col_names)
      
      # Build detection matrix (TRUE = detected outlier)
      detection_matrix <- matrix(FALSE, nr, nc)
      dimnames(detection_matrix) <- list(row_names, col_names)
      
      # Populate matrices and build detail list
      detail_list <- list()
      for (i in seq_along(identified_cells)) {
        cell <- identified_cells[[i]]
        r <- cell[1]
        c <- cell[2]
        
        del_res_matrix[r, c] <- deleted_residuals[i]
        
        is_detected <- (i <= detected_count)
        if (is_detected) {
          detection_matrix[r, c] <- TRUE
        }
        
        detail_list[[i]] <- list(
          rank = i,
          row_name = row_names[r],
          col_name = col_names[c],
          row_idx = r,
          col_idx = c,
          deleted_residual = deleted_residuals[i],
          g2_drop = g2_drop_values[i],
          detected = is_detected
        )
      }
      
      # Store results for significance table extraction
      private$.lastBSOutlier <- list(
        del_res_matrix = del_res_matrix,
        detection_matrix = detection_matrix,
        detail_list = detail_list,
        critical_value = critical_value,
        alpha = alpha,
        k_max = length(detail_list),
        detected_count = detected_count
      )
      
      return(private$.lastBSOutlier)
    },
    
    # -------------------------------------------------------------------------
    # Compute Dependence Evaluator Proportion (DEP) - Gambirasio method
    # -------------------------------------------------------------------------
    .computeDEP = function(contingency_table, colVar) {
      rx_c_table <- as.matrix(contingency_table)
      
      # Get the selected outcome category
      outcome_category <- self$options$depOutcome
      col_names <- colnames(rx_c_table)
      
      # Find the index of the outcome column
      if (is.null(outcome_category) || outcome_category == "" || 
          !(outcome_category %in% col_names)) {
        # Default to first column if not specified or invalid
        effect_col_index <- 1
        outcome_category <- col_names[1]
      } else {
        effect_col_index <- which(col_names == outcome_category)
      }
      
      row_labels <- rownames(rx_c_table)
      if (is.null(row_labels)) {
        row_labels <- paste("Row", 1:nrow(rx_c_table))
      }
      
      results_list <- list()
      
      for (i in 1:nrow(rx_c_table)) {
        
        # Define active cause (current row) vs inactive cause (all other rows)
        active_cause_row <- rx_c_table[i, ]
        inactive_cause_rows <- rx_c_table[-i, , drop = FALSE]
        inactive_cause_row <- colSums(inactive_cause_rows)
        
        # Build 2x2 table for one-vs-rest contrast
        n_X_Y <- active_cause_row[effect_col_index]
        n_X_notY <- sum(active_cause_row[-effect_col_index])
        n_notX_Y <- inactive_cause_row[effect_col_index]
        n_notX_notY <- sum(inactive_cause_row[-effect_col_index])
        
        temp_2x2 <- matrix(c(n_X_Y, n_X_notY, n_notX_Y, n_notX_notY), 
                           nrow = 2, byrow = TRUE)
        
        # Calculate marginals
        n_X <- n_X_Y + n_X_notY
        n_notX <- n_notX_Y + n_notX_notY
        n_Y <- n_X_Y + n_notX_Y
        n_notY <- n_X_notY + n_notX_notY
        n_T <- sum(temp_2x2)
        
        # Check for zero marginals
        if (n_X == 0 || n_notX == 0 || n_Y == 0 || n_notY == 0) {
          results_list[[i]] <- list(
            row = row_labels[i],
            outcome = outcome_category,
            dep = NA,
            sig_lower = NA,
            sig_upper = NA,
            significant = FALSE,
            conclusion = "Insufficient data"
          )
          next
        }
        
        # Calculate conditional probabilities
        P_Y_given_X <- n_X_Y / n_X
        P_Y_given_notX <- n_notX_Y / n_notX
        
        # Calculate DEP
        numerator <- P_Y_given_X - P_Y_given_notX
        denominator <- P_Y_given_X + P_Y_given_notX
        dep_val <- ifelse(denominator == 0, 0, numerator / denominator)
        
        # Calculate significance bounds using chi-squared critical value
        x0 <- (n_X * n_Y) / n_T  # Expected count under independence
        
        k_param <- (1 / x0) + (1 / (n_X - x0)) + (1 / (n_Y - x0)) + 
          (1 / (n_T - n_X - n_Y + x0))
        
        chi_sq_crit <- 3.841  # Critical chi-square for p = 0.05, df = 1
        
        delta <- sqrt(chi_sq_crit / k_param)
        x1 <- max(0, x0 - delta)
        x2 <- min(min(n_X, n_Y), x0 + delta)
        
        # Function to calculate DEP from hypothetical count
        get_dep_from_x <- function(x_hyp) {
          p_y_x_hyp <- x_hyp / n_X
          p_y_notx_hyp <- (n_Y - x_hyp) / n_notX
          num <- p_y_x_hyp - p_y_notx_hyp
          den <- p_y_x_hyp + p_y_notx_hyp
          return(ifelse(den == 0, 0, num / den))
        }
        
        dep_at_x1 <- get_dep_from_x(x1)
        dep_at_x2 <- get_dep_from_x(x2)
        
        sig_bounds <- sort(c(dep_at_x1, dep_at_x2))
        sig_lower <- sig_bounds[1]
        sig_upper <- sig_bounds[2]
        
        # Determine significance
        is_significant <- (dep_val < sig_lower) || (dep_val > sig_upper)
        conclusion <- ifelse(is_significant, 
                             "Significant (p < 0.05)", 
                             "Not Significant (p ≥ 0.05)")
        
        results_list[[i]] <- list(
          row = row_labels[i],
          outcome = outcome_category,
          dep = dep_val,
          sig_lower = sig_lower,
          sig_upper = sig_upper,
          significant = is_significant,
          conclusion = conclusion
        )
      }
      
      private$.lastDEP <- results_list
      
      return(results_list)
    },
    
    .populatePlainMetricTable = function(metric_matrix, table_name, threshold = NULL, 
                                         threshold_type = "abs") {
      # threshold_type: "abs" for |value| > threshold (residuals)
      #                 "empirical_q" for Quetelet (>1.0 or <-0.5)
      #                 "empirical_ij" for IJ (>2.0 or <0.5)
      
      table <- self$results[[table_name]]
      
      I <- private$.I
      J <- private$.J
      
      for (i in seq_len(I)) {
        row_values <- list()
        for (j in seq_len(J)) {
          value <- metric_matrix[i, j]
          cell_text <- if (is.na(value)) "" else sprintf("%.3f", value)
          row_values[[paste0("col", j)]] <- cell_text
        }
        table$setRow(rowKey = paste0('row', i), values = row_values)
        
        # Apply highlighting if threshold is provided
        if (!is.null(threshold)) {
          for (j in seq_len(J)) {
            value <- metric_matrix[i, j]
            if (!is.na(value)) {
              is_significant <- FALSE
              
              if (threshold_type == "abs") {
                # Symmetric threshold: |value| > threshold
                is_significant <- abs(value) > threshold
              } else if (threshold_type == "empirical_q") {
                # Quetelet: >1.0 or <-0.5
                is_significant <- (value > 1.0) || (value < -0.5)
              } else if (threshold_type == "empirical_ij") {
                # IJ: >2.0 or <0.5
                is_significant <- (value > 2.0) || (value < 0.5)
              } else if (threshold_type == "fence") {
                # Boxplot fences: value < lower OR value > upper
                is_significant <- (value < threshold[1]) || (value > threshold[2])
              }
              
              if (is_significant) {
                table$addFormat(
                  rowKey = paste0('row', i),
                  col = paste0('col', j),
                  format = jmvcore::Cell.NEGATIVE
                )
              }
            }
          }
        }
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate DEP table
    # -------------------------------------------------------------------------
    .populateDEPTable = function(dep_results, rowVar, colVar) {
      
      table <- self$results$depTable
      
      outcome_cat <- if (length(dep_results) > 0) dep_results[[1]]$outcome else ""
      table$setTitle(paste0("Dependence Evaluator Proportion (DEP): ", 
                            rowVar, " → ", outcome_cat))
      
      for (i in seq_along(dep_results)) {
        result <- dep_results[[i]]
        
        # Plain text significance (no colour)
        sig_text <- if (is.na(result$dep)) result$conclusion else result$conclusion
        
        table$setRow(rowKey = i, values = list(
          rowCat = result$row,
          depOutcome = result$outcome,
          depValue = if (is.na(result$dep)) NA else round(result$dep, 4),
          sigLower = if (is.na(result$sig_lower)) NA else round(result$sig_lower, 4),
          sigUpper = if (is.na(result$sig_upper)) NA else round(result$sig_upper, 4),
          significance = sig_text
        ))
        
        # Apply highlighting to DEP value if significant
        if (!is.na(result$dep) && result$significant) {
          table$addFormat(
            rowKey = i,
            col = 'depValue',
            format = jmvcore::Cell.NEGATIVE
          )
        }
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate Goodman-Kruskal Residuals table
    # -------------------------------------------------------------------------
    .populateGKResidualTable = function(gk_matrix, table_name, title_label, rowVar, colVar, predictor_type) {
      
      table <- self$results[[table_name]]
      
      if (predictor_type == 'col') {
        table$setTitle(paste0(title_label, ": ", colVar, " → ", rowVar))
      } else {
        table$setTitle(paste0(title_label, ": ", rowVar, " → ", colVar))
      }
      
      I <- nrow(gk_matrix)
      J <- ncol(gk_matrix)
      row_names <- rownames(gk_matrix)
      col_names <- colnames(gk_matrix)
      
      # Use setRow on pre-built rows
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- gk_matrix[i, j]
          
          formatted_value <- sprintf("%.3f", value)
          formatted_value <- paste0("<span style='display: block; text-align: center;'>", 
                                    formatted_value, "</span>")
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$setRow(rowKey = i, values = row_values)
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate Backwards-Stepping Outlier Matrix Table (deleted residuals)
    # -------------------------------------------------------------------------
    .populateBSOutlierMatrixTable = function(results, rowVar, colVar) {
      
      table <- self$results$bsOutlierMatrixTable
      
      table$setTitle(paste0("Deleted Residuals (Backwards-Stepping): ", rowVar, " × ", colVar))
      
      del_res_matrix <- results$del_res_matrix
      detection_matrix <- results$detection_matrix
      
      I <- nrow(del_res_matrix)
      J <- ncol(del_res_matrix)
      row_names <- rownames(del_res_matrix)
      col_names <- colnames(del_res_matrix)
      
      # Use setRow on pre-built rows
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- del_res_matrix[i, j]
          is_detected <- detection_matrix[i, j]
          
          if (is.na(value)) {
            formatted_value <- "—"
          } else {
            # Format the numeric value
            num_str <- sprintf("%.3f", value)
            
            # Apply red colouring if this cell is detected as an outlier
            if (is_detected) {
              formatted_value <- sprintf("<span style='color: #C00000;'>%s</span>", num_str)
            } else {
              formatted_value <- num_str
            }
          }
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$setRow(rowKey = i, values = row_values)
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate Backwards-Stepping Outlier Detail Table
    # -------------------------------------------------------------------------
    .populateBSOutlierDetailTable = function(results, rowVar, colVar) {
      
      table <- self$results$bsOutlierDetailTable
      
      table$setTitle(paste0("Backwards-Stepping: Identified Cells (by extremeness)"))
      
      detail_list <- results$detail_list
      k_max <- table$rowCount  # rows pre-built in .init(): min(bsKmax, I x J - 1)
      actual_identified <- length(detail_list)
      
      # Use setRow on pre-built rows, but only populate rows with actual data
      for (i in seq_len(k_max)) {
        if (i <= actual_identified && !is.null(detail_list[[i]])) {
          item <- detail_list[[i]]
          
          # Format cell name with actual level names
          cell_name <- paste0(item$row_name, " / ", item$col_name)
          
          # Format detected column (plain text, no colour)
          detected_text <- if (item$detected) "Yes" else "No"
          
          table$setRow(rowKey = i, values = list(
            rank = item$rank,
            cell = cell_name,
            delRes = round(item$deleted_residual, 3),
            g2Drop = round(item$g2_drop, 3),
            detected = detected_text
          ))
        } else {
          # Hide unused pre-built rows by setting them invisible or blank
          table$setRow(rowKey = i, values = list(
            rank = '',
            cell = '',
            delRes = '',
            g2Drop = '',
            detected = ''
          ))
        }
      }
    },
    
    # -------------------------------------------------------------------------
    # Prepare data for DEP forest plot
    # -------------------------------------------------------------------------
    .prepareDepPlotData = function(dep_results) {
      
      # Get outcome category from first result
      outcome_cat <- if (length(dep_results) > 0) dep_results[[1]]$outcome else ""
      
      plot_data <- data.frame(
        label = character(),
        dep = numeric(),
        sig_lower = numeric(),
        sig_upper = numeric(),
        significant = logical(),
        stringsAsFactors = FALSE
      )
      
      for (result in dep_results) {
        if (!is.na(result$dep)) {
          plot_data <- rbind(plot_data, data.frame(
            label = result$row,
            dep = result$dep,
            sig_lower = result$sig_lower,
            sig_upper = result$sig_upper,
            significant = result$significant,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Sort by DEP value
      plot_data <- plot_data[order(plot_data$dep), ]
      
      # Store outcome category as attribute
      attr(plot_data, "outcome") <- outcome_cat
      
      private$.depPlotData <- plot_data
      
      image <- self$results$depPlot
      image$setState(plot_data)
    },
    
    # -------------------------------------------------------------------------
    # Prepare data for PEM forest plot
    # -------------------------------------------------------------------------
    .preparePemPlotData = function(pem_results, rowVar, colVar) {
      
      pem <- pem_results$pem
      ci_lower <- pem_results$ci_lower
      ci_upper <- pem_results$ci_upper
      
      row_names <- rownames(pem)
      col_names <- colnames(pem)
      
      # Build a data frame: one row per cell
      plot_data <- data.frame(
        label = character(),
        pem = numeric(),
        lower = numeric(),
        upper = numeric(),
        significant = logical(),
        stringsAsFactors = FALSE
      )
      
      for (i in seq_along(row_names)) {
        for (j in seq_along(col_names)) {
          pem_val <- pem[i, j]
          ci_lo <- ci_lower[i, j]
          ci_hi <- ci_upper[i, j]
          is_sig <- (ci_lo > 0) || (ci_hi < 0)
          
          plot_data <- rbind(plot_data, data.frame(
            label = paste0(row_names[i], " / ", col_names[j]),
            pem = pem_val,
            lower = ci_lo,
            upper = ci_hi,
            significant = is_sig,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Apply filter based on user selection
      filter_choice <- self$options$pemPlotFilter
      if (filter_choice == "sigOnly") {
        plot_data <- plot_data[plot_data$significant, ]
      } else if (filter_choice == "nonsigOnly") {
        plot_data <- plot_data[!plot_data$significant, ]
      }
      # else: "all" — keep everything
      
      # Sort by PEM value (ascending, so negative at top when plotted horizontally)
      plot_data <- plot_data[order(plot_data$pem), ]
      
      # Store for the render function
      private$.pemPlotData <- plot_data
      
      # Set the state on the image so the render function can access it
      image <- self$results$pemPlot
      image$setState(plot_data)
    },
    
    .populateMethodInfo = function() {
      
      if (!self$options$showMethodInfo) {
        return()
      }
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
      
      # Standardised Residuals
      if (self$options$stdres) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Standardised Residuals</h3>
          <p>Standardised residuals quantify the departure of each cell's observed count from the count expected under independence, expressed in standard deviation units. The calculation is (<i>observed</i> − <i>expected</i>) / √<i>expected</i>. Under the null hypothesis of independence, these residuals approximately follow a standard normal distribution, which allows threshold-based interpretation using critical values from the normal distribution (typically ±1.96 for α = 0.05).</p>
          
          <p>The ±1.96 threshold corresponds to a 95% confidence level when examining a single cell in isolation. However, when inspecting multiple cells simultaneously—as is typical in contingency table analysis—the probability of observing at least one false positive increases. The Šidák correction addresses this multiple comparison problem by adjusting the threshold to maintain an overall Type I error rate of 0.05 across all cells. The corrected threshold is calculated as Φ<sup>−1</sup>(1 − α<sub>S</sub>/2), with α<sub>S</sub> = 1 − (1 − α)<sup>1/<i>k</i></sup>, where <i>k</i> is the number of cells and Φ<sup>−1</sup> is the inverse standard normal cumulative distribution function.</p>
          
          <p><strong>Key limitation:</strong> Standardised residuals do not have constant variance across cells. Their theoretical variance is always less than or equal to 1, and varies depending on the cell's marginal totals. This heterogeneity can make direct cell-to-cell comparisons misleading, particularly when marginal distributions are unbalanced. For more accurate comparisons across cells with varying marginal totals, consider using adjusted standardised residuals, which explicitly correct for this variance heterogeneity.</p>
        ")
      }
      
      # Moment-Corrected Standardised Residuals
      if (self$options$momcorrstdres) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Moment-Corrected Standardised Residuals</h3>
          <p>Moment-corrected standardised residuals address a fundamental misconception about standard residuals: while they are called 'standardised', their variance is not actually 1. Instead, the theoretical variance of standardised residuals averages (<i>I</i> − 1)(<i>J</i> − 1)/(<i>IJ</i>), where <i>I</i> and <i>J</i> are the number of rows and columns. This non-unit variance makes standard statistical tests overly conservative—apparent 'significant' residuals may not actually be as extreme as they appear.</p>
          
          <p>The moment-correction rectifies this by dividing each standardised residual by the square root of its theoretical variance: <i>e</i><sub>ij</sub><sup>#</sup> = <i>e</i><sub>ij</sub> / √[(<i>I</i> − 1)(<i>J</i> − 1)/(<i>IJ</i>)]. This transformation produces residuals with a true variance of 1, allowing proper comparison against the standard normal distribution. Crucially, this correction factor depends only on table dimensions, not on the observed data, making it simpler than data-dependent corrections.</p>
          
          <p><strong>Practical interpretation:</strong> Empirical studies demonstrate that when marginal distributions are relatively uniform, moment-corrected residuals and adjusted residuals (Haberman's method) produce nearly identical results. However, when marginal distributions are highly peaked or skewed, adjusted residuals may perform slightly better because they account for cell-specific variance differences. The choice between methods often depends on whether the analyst prioritises computational simplicity (moment-correction) or maximum precision across varying marginal patterns (adjustment).</p>
        ")
      }
      
      # Adjusted Standardised Residuals
      if (self$options$adjstdres) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Adjusted Standardised Residuals</h3>
          <p>Adjusted standardised residuals correct a critical limitation of standard residuals: the variance of standardised residuals is not constant across cells, but instead depends on each cell's marginal totals. Specifically, the asymptotic variance of a standardised residual in cell (<i>i</i>, <i>j</i>) is <i>v</i><sub><i>ij</i></sub> = (1 − <i>n</i><sub><i>i</i>+</sub>/<i>n</i>) × (1 − <i>n</i><sub>+<i>j</i></sub>/<i>n</i>), where <i>n</i><sub><i>i</i>+</sub> and <i>n</i><sub>+<i>j</i></sub> are the row and column totals. This variance is always less than 1 and varies systematically—cells in rows or columns with large marginal totals have smaller variance.</p>
          
          <p>The adjustment involves dividing the standardised residual by the square root of its estimated variance: <i>d</i><sub><i>ij</i></sub> = <i>e</i><sub><i>ij</i></sub> / √<i>v</i><sub><i>ij</i></sub>. This produces residuals that are each approximately distributed as standard normal deviates (<i>Z</i> ~ <i>N</i>(0, 1)), making them directly comparable across all cells regardless of marginal structure. The same threshold (e.g., ±1.96 for α = 0.05) can now be confidently applied to identify genuinely unusual cells.</p>
          
          <p><strong>Practical advantage:</strong> Adjusted residuals can reveal significant departures from independence that might be masked by the variance heterogeneity in standard residuals. This is particularly important in tables where marginal totals vary substantially—for instance, when comparing a dominant category against several smaller categories. The adjustment prevents cells in dominant margins from appearing spuriously significant simply due to their structural position in the table.</p>
        ")
      }
      
      # Šidák Correction for Multiple Comparisons
      if (self$options$sidakCorrection && (self$options$stdres || self$options$momcorrstdres || self$options$adjstdres)) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Šidák Correction for Multiple Comparisons</h3>
          <p>When examining residuals across multiple cells simultaneously, the familywise error rate (the probability of making at least one Type I error) increases above the nominal α level. For instance, testing 10 cells at α = 0.05 would yield approximately a 40% chance of at least one false positive under the null hypothesis. The Šidák correction addresses this multiple comparison problem by adjusting the per-comparison threshold to maintain the desired overall Type I error rate across all cells in the table.</p>
          
          <p>The correction calculates an adjusted significance level: α<sub>adjusted</sub> = 1 − (1 − α)<sup>1/<i>k</i></sup>, where <i>k</i> is the number of cells being tested and α is the desired familywise error rate (typically 0.05). The corresponding threshold for standardised residuals becomes Φ<sup>−1</sup>(1 − α<sub>adjusted</sub>/2), where Φ<sup>−1</sup> is the inverse standard normal cumulative distribution function. This adjusted threshold is more stringent than the uncorrected ±1.96, with the degree of stringency increasing as the number of cells grows.</p>
          
          <p><strong>When to apply:</strong> The Šidák correction is appropriate when conducting exploratory analysis where all cells are being examined for potential significance. It provides strong protection against false discoveries in large tables. However, if hypotheses about specific cells were formulated <em>a priori</em> (before examining the data), or if only a small subset of cells is of substantive interest, the correction may be unnecessarily conservative. The correction applies equally to standardised residuals, moment-corrected standardised residuals, and adjusted standardised residuals—in each case, it modifies only the threshold for declaring significance, not the residuals themselves.</p>
          
          <p>See: Beasley & Schumacker 1995.</p>
        ")
      }
      
      # Quetelet Index
      if (self$options$quetelet) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Quetelet Index</h3>
          <p>The Quetelet index provides a directly interpretable measure of association strength for individual cells by quantifying the proportional change in probability. It is calculated as (<i>observed</i> / <i>expected</i>) − 1. This simple formula measures how much the observed cell count deviates from independence as a proportion of what independence would predict.</p>
          
          <p>The index has immediate operational meaning: a value of 1.40 indicates that the observed count is 140% higher than expected under independence—in other words, knowing the row category increases the probability of the column category by 140% compared to the baseline marginal probability. A value of −0.50 means the observed count is 50% lower than expected—the probability is halved when the row category is known. Zero indicates perfect independence for that cell.</p>
          
          <p><strong>Connection to χ²:</strong> The Quetelet index has an elegant relationship to the overall chi-squared statistic. Specifically, the average of all Quetelet index values across cells, weighted by their expected counts, equals the phi-squared statistic (φ² = χ²/<i>N</i>). This establishes an operational interpretation for the chi-squared test itself: it measures the average proportional deviation from independence across the entire table. Values exceeding 1.0 (or falling below −0.50) are generally considered noteworthy, though these thresholds are empirical rather than based on formal probability distributions.</p>
        ")
      }
      
      # IJ Association Factor
      if (self$options$ij) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>IJ Association Factor</h3>
          <p>The IJ association factor expresses the strength of association in each cell as a simple ratio: <i>observed</i> / <i>expected</i>. This straightforward calculation directly compares what was observed against what independence would predict, making it highly intuitive. The factor can be interpreted as the ratio of the observed cell probability to the probability expected under independence given the marginal distributions.</p>
          
          <p>A value of exactly 1.0 indicates perfect independence: the cell contains precisely the number of observations expected if the variables were unrelated. Values greater than 1 indicate <strong>attraction</strong>—the combination of row and column categories occurs more frequently than independence would predict. Values less than 1 indicate <strong>repulsion</strong>—the combination occurs less frequently than expected. For instance, α = 2.5 means the cell contains 2.5 times as many observations as independence predicts; α = 0.4 means it contains only 40% of the expected observations.</p>
          
          <p><strong>Interpretation guidelines:</strong> Following recommendation in relevant literature, departures from independence are generally considered noteworthy when α > 2.0 (attraction) or α < 0.5 (repulsion). These empirical thresholds provide a practical rule for identifying cells that contribute substantially to overall association. The multiplicative interpretation makes the IJ factor particularly intuitive for understanding effect magnitude—unlike residuals measured in standard deviations, these values map directly onto familiar concepts of 'twice as many' or 'half as many' observations.</p>
        ")
      }
      
      # Backwards-Stepping Outlier Detection
      if (self$options$bsOutlier) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Backwards-Stepping Outlier Detection</h3>
          <p>This method identifies outlying cells in contingency tables using a statistically principled procedure that avoids both <strong>masking</strong> (where multiple outliers hide each other) and <strong>swamping</strong> (where outliers cause non-outliers to be incorrectly flagged). The approach combines deleted residuals for cell identification with backwards-stepping for testing, providing robust outlier detection even when multiple unusual cells are present.</p>
          
          <p><strong>The masking problem:</strong> Standard forwards-stepping approaches (testing cells from most to least extreme) can fail when multiple outliers exist. Outliers are compared against less extreme cells that might themselves be outliers, reducing the chance of detecting any of them. Backwards-stepping reverses this logic: cells are tested from <em>least</em> extreme to <em>most</em> extreme, ensuring that each test compares a candidate outlier against cells already confirmed to be non-outlying.</p>
          
          <p><strong>Deleted residuals:</strong> Rather than using standard residuals (which can be distorted by outliers influencing the expected values), the method uses <strong>deleted residuals</strong>. For each cell, the quasi-independence model is fitted with that cell temporarily removed, and the residual is calculated using the expected value from this deleted-cell fit. This prevents any single cell from influencing its own assessment, making the identification more reliable.</p>
          
          <p><strong>The procedure:</strong></p>
          <ol style='margin-left: 2em;'>
            <li><strong>Identification phase:</strong> For each iteration <i>i</i> = 1 to <i>k</i><sub>max</sub>, fit the quasi-independence model to remaining cells. Identify the cell with the largest absolute deleted residual as the <i>i</i>-th most extreme. Calculate <i>T</i><sub><i>i</i></sub> = G²<sub>current</sub> − G²<sub>deleted</sub>, the drop in the likelihood ratio statistic when this cell is removed.</li>
            <li><strong>Testing phase:</strong> Test cells from least extreme (<i>k</i><sub>max</sub>) to most extreme (1). Compare each <i>T</i><sub><i>i</i></sub> to a Bonferroni-corrected critical value (χ²<sub>1</sub> at α/<i>RC</i>, where <i>RC</i> is the total number of cells). If <i>T</i><sub><i>i</i></sub> exceeds this threshold, declare cells 1 through <i>i</i> as outliers and stop.</li>
          </ol>
          
          <p><strong>Output interpretation:</strong> Two outputs are provided: (1) a matrix showing deleted residuals for the <i>k</i><sub>max</sub> most extreme cells, with detected outliers highlighted; (2) a detail table listing cells in order of extremeness with their G² drop values. Positive deleted residuals indicate the cell contains more observations than expected (attraction); negative residuals indicate fewer than expected (repulsion). The G² drop quantifies how much each cell contributes to overall lack-of-fit—larger values indicate cells that deviate more substantially from the independence model.</p>
          
          <p><strong>Setting <i>k</i><sub>max</sub>:</strong> The parameter <i>k</i><sub>max</sub> determines the maximum number of potential outliers to examine. Simonoff (1988) suggests this might be 20%-30% of cells, though context should guide the choice. Setting <i>k</i><sub>max</sub> too low risks missing outliers; setting it too high increases computation but does not inflate the false positive rate (the Bonferroni correction accounts for all <i>RC</i> cells regardless of <i>k</i><sub>max</sub>). At most <i>RC</i> − 1 cells can be set aside, so a larger <i>k</i><sub>max</sub> is reduced to <i>RC</i> − 1.</p>
        ")
      }
      
      # PEM / Sakoda D Local
      if (self$options$pem) {
        html <- paste0(html, "
    <h3 style='color: #2874A6; margin-top: 1.5em;'>PEM (Percentage of Maximum Deviation from Independence) / Sakoda's D Local</h3>
    <p>PEM is a local effect size measure that asks: 'what proportion of the maximum possible deviation from independence is present in this specific cell?' The calculation involves three steps: (1) compute the observed deviation from independence (<i>n</i><sub><i>ij</i></sub> − <i>e</i><sub><i>ij</i></sub>); (2) determine the maximum possible deviation given the fixed marginal totals (the smaller of the row and column totals, minus the expected count); (3) express the observed deviation as a percentage of the maximum: PEM = [(<i>n</i><sub><i>ij</i></sub> − <i>e</i><sub><i>ij</i></sub>) / (<i>n</i><sub>max</sub> − <i>e</i><sub><i>ij</i></sub>)] × 100.</p>
    
    <p><strong>Equivalence to Sakoda's D Local:</strong> PEM, introduced by Cibois (1993), is mathematically identical to Sakoda's local (i.e., cell-level) index of dissimilarity (D<sub>ij</sub>), developed independently by Sakoda (1981). The only difference is presentation: Sakoda expresses the value as a proportion (−1 to +1), whilst Cibois multiplies by 100 to express it as a percentage (−100% to +100%). Both measures quantify how far each cell deviates from independence relative to its maximum possible deviation given the marginal constraints.</p>
    
    <p>PEM values range from −100% to +100%. Positive values indicate <strong>attraction</strong> (the cell contains more observations than expected), with +100% meaning the cell has reached its maximum possible count given the marginals. Negative values indicate <strong>repulsion</strong> (fewer observations than expected), with −100% meaning the cell is completely empty despite having a positive expected count. A value of 0% indicates perfect independence for that cell.</p>
    
    <p><strong>Effect size interpretation and colour coding:</strong> Unlike global association measures (such as Cramér's V) where Cohen's thresholds (0.1, 0.3, 0.5) can be applied or scaled, no established statistical convention exists for interpreting cell-level effect sizes. The colour coding and thresholds used in this module to flag noteworthy PEM values are based on the empirical guidelines proposed by Cibois (1993): values exceeding ±50% are considered indicative of substantively meaningful attraction or repulsion. These thresholds are practical rules of thumb derived from applied experience rather than statistical theory, and users should exercise judgement based on their specific research context.</p>
    
    <p><strong>Statistical inference:</strong> PEM / Sakoda D Local is accompanied by bootstrap confidence intervals, allowing formal hypothesis testing. If the confidence interval excludes zero, the cell's deviation from independence is statistically significant. This combination of interpretable effect size and inferential capacity makes PEM particularly powerful for identifying and quantifying specific patterns of association through significant strong attraction and repulsion.</p>
    
    <p><strong>Forest plot visualisation:</strong> The optional PEM forest plot provides a ranked visual summary of all cell-level associations. Each cell is represented by a point (the PEM estimate) with horizontal whiskers showing the bootstrap confidence interval (Lefèvre & Champely 2009). Statistically significant associations (confidence interval excludes zero) appear in gold when positive (attraction) and in maroon when negative (repulsion); non-significant associations appear in grey. The vertical line at zero represents independence. This visualisation allows rapid identification of the strongest and most reliable patterns of attraction and repulsion, sorted by effect magnitude. Note that because PEM is bounded (−100% to +100%) and its sampling distribution is typically skewed, bootstrap confidence intervals are often asymmetric around the point estimate; this is expected behaviour consistent with the percentile bootstrap method (Efron & Tibshirani 1993; Lefèvre & Champely 2009).</p>
  ")
      }
      
      ## Standardised Median Polish Residuals
      if (self$options$medpolish) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Standardised Median Polish Residuals</h3>
          <p>This method addresses a fundamental problem in outlier detection: extreme cell counts distort the row, column, and grand totals used to calculate expected values, which in turn affects all other cells' residuals. This creates a <strong>masking effect</strong> where outliers hide themselves and each other. The solution is to fit the independence model using a <strong>resistant</strong> procedure that isn't influenced by extreme values.</p>
          
          <p><strong>Why standard residuals may fail with multiple outliers:</strong> When calculating expected values in the usual way, extreme cell counts distort three components simultaneously: (1) the row total that includes the outlier, (2) the column total that includes the outlier, and (3) the grand total. This means the outlier 'pulls' the expected value towards itself, reducing its own apparent extremeness. When <em>multiple</em> outliers exist, they can collectively distort the fitted model so severely that standard residuals exhibit two systematic failures:</p>
          
          <ul style='margin-left: 2em;'>
          <li><strong>Masking:</strong> Two or more outliers effectively hide each other. By jointly distorting the marginal totals, genuine outliers appear less extreme than they truly are, and may not be flagged at all. The outliers mask their own presence by corrupting the very baseline (independence model) against which they are being compared.</li>
          
          <li><strong>Swamping:</strong> Outliers draw the fitted values sufficiently towards themselves that <em>non-outlying cells</em> are mistakenly identified as extreme. This occurs because the distorted expected values make legitimate cells appear discrepant.</li>
          </ul>
          
          <p>Median polish eliminates both problems because it calculates the independence model using row and column <strong>medians</strong> rather than row and column totals. Since medians are unaffected by extreme values, the robust expected counts accurately reflect what independence would predict <em>without distortion from outliers</em>. This allows genuine outliers to reveal themselves clearly whilst preventing non-outlying cells from being falsely implicated.</p>
          
          <p>The method works in several stages: (1) The observed cell counts are log-transformed to stabilise variance and make the multiplicative independence model additive. (2) <strong>Median polish</strong> is applied to the log-counts—this robust technique iteratively sweeps out row and column medians rather than means, making the fitted values insensitive to outliers. (3) The fitted log-values are exponentiated back to the original count scale, yielding <strong>robust expected counts</strong> that represent what independence would predict without distortion from extreme cells. (4) Pearson residuals are calculated as (<i>n</i><sub><i>ij</i></sub> − <i>e</i><sub>robust</sub>) / √<i>e</i><sub>robust</sub>.</p>
          
          <p>Rather than comparing these residuals to a theoretical normal distribution (±1.96), the method uses the <strong>fourth-spread rule</strong> for outlier identification. The fourth-spread (<i>d</i><sub>F</sub>) is the difference between the upper and lower fourths (75th and 25th percentiles) of the residual distribution. Cells are flagged as extreme if they exceed <i>F</i><sub>U</sub> + 1.5 × <i>d</i><sub>F</sub> or fall below <i>F</i><sub>L</sub> − 1.5 × <i>d</i><sub>F</sub>. This mirrors boxplot logic and is itself resistant to outliers. The method also calculates the <strong>expected number of outliers</strong> under normal sampling variation (≈ 0.007 × <i>k</i> + 0.4, where <i>k</i> is the number of cells), allowing assessment of whether observed extreme values exceed what chance alone would produce.</p>
        ")
      }
      
      # Adjusted Standardised Median Polish Residuals
      if (self$options$adjmedpolish) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Adjusted Standardised Median Polish Residuals</h3>
          <p>This method combines two robust strategies: <strong>resistant fitting</strong> to generate unbiased expected values, and <strong>variance adjustment</strong> to ensure fair cell-to-cell comparisons. Like the standard median polish approach, it begins by fitting the independence model through a procedure immune to outlier influence, but it then applies Haberman's adjustment to correct for heterogeneous variance across cells.</p>
          
          <p><strong>Protection against masking and swamping:</strong> The robust calculation via median polish ensures that extreme cells do not corrupt the baseline independence model. This addresses both failure modes of standard residuals: <strong>masking</strong> (where multiple outliers hide each other by jointly distorting marginal totals) and <strong>swamping</strong> (where outliers draw the fitted values towards themselves so severely that non-outlying cells appear falsely extreme). The subsequent Haberman adjustment guarantees that all cells are compared on an equal variance basis, preventing cells in dominant margins from appearing spuriously significant due solely to their structural position. Together, these provide comprehensive protection for outlier identification in complex tables.</p>
          
          <p>The computational process mirrors standard median polish for stages 1-3: (1) Log-transform observed counts; (2) Apply median polish to iteratively sweep row and column medians from the log-table; (3) Exponentiate the fitted values to obtain robust expected counts. At stage 4, however, rather than computing simple Pearson residuals, the method calculates <strong>adjusted residuals</strong>: <i>d</i><sub><i>ij</i></sub> = [(<i>n</i><sub><i>ij</i></sub> − <i>e</i><sub>robust</sub>) / √<i>e</i><sub>robust</sub>] / √[(1 − <i>n</i><sub><i>i</i>+</sub>/<i>N</i>) × (1 − <i>n</i><sub>+<i>j</i></sub>/<i>N</i>)]. This adjustment ensures that cells in dominant margins don't appear spuriously extreme simply due to their structural position.</p>
          
          <p>The fourth-spread rule is then applied to these adjusted residuals to identify outliers. Because both the expected values (via median polish) and the thresholds (via fourth-spread) are resistant to extreme values, this method provides <strong>doubly-robust</strong> outlier detection. It can reliably identify genuinely unusual cells even in tables with multiple outliers and unbalanced marginal distributions. The expected outlier count (≈ 0.007 × <i>k</i> + 0.4) provides a benchmark: if substantially more cells are flagged than this formula predicts, it suggests the presence of real systematic departures from independence rather than random sampling variation.</p>
        ")
      }
      
      # Goodman-Kruskal Residuals
      if (self$options$gkres) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Goodman-Kruskal Residuals</h3>
          <p>Goodman-Kruskal residuals provide a <strong>directional</strong> perspective on association, explicitly distinguishing between a <strong>predictor</strong> (independent) variable and a <strong>response</strong> (dependent) variable. Unlike symmetric measures that treat rows and columns interchangeably, these residuals ask: 'How does knowing the predictor category change our expectation about the response category?'</p>
          
          <p><strong>Calculation:</strong> For each cell, the residual is computed as the difference between the <strong>conditional probability</strong> of the response category given the predictor category and the <strong>marginal probability</strong> of that response category:</p>
          
          <p style='text-align: center; font-style: italic;'>
            GK Residual = P(Response | Predictor) − P(Response)
          </p>
          
          <p>When columns serve as the predictor, P(Response | Predictor) is the <strong>column profile</strong>: the proportion of observations in each row within a given column. The marginal probability P(Response) is simply the row margin divided by the grand total. The residual measures how much the conditional distribution within a column deviates from what the marginal distribution would predict.</p>
          
          <p><strong>Interpretation:</strong></p>
          <ul style='margin-left: 2em;'>
            <li><strong>Positive residual:</strong> Knowing the predictor category <em>increases</em> the probability of the response category above its marginal rate. The combination occurs more frequently than the marginal distribution would suggest.</li>
            <li><strong>Negative residual:</strong> Knowing the predictor category <em>decreases</em> the probability of the response category below its marginal rate. The combination occurs less frequently than expected.</li>
            <li><strong>Zero residual:</strong> The conditional probability equals the marginal probability — knowing the predictor provides no information about this response category.</li>
          </ul>
          
          <p><strong>Two tables:</strong> Because directionality matters, two tables are produced: one treating columns as the predictor (rows as response), one treating rows as the predictor (columns as response). Users should select the table corresponding to their substantive research question — which variable is conceptually the predictor?</p>
          
          <p><strong>No formal significance thresholds:</strong> Unlike other residuals, Goodman-Kruskal residuals do not follow a standard probability distribution, so no formal p-values or critical thresholds exist. Interpretation should focus on <em>relative magnitudes</em> within the table and on substantive context. Cells with larger absolute residuals contribute more to the predictive relationship between variables.</p>
          
          <p><strong>Connection to global measures:</strong> These residuals complement Goodman-Kruskal's lambda and tau coefficients, which summarise predictability at the table level. The residuals decompose that global predictability into cell-level contributions, revealing <em>which</em> specific category combinations drive the overall association.</p>
        ")
      }
      
      # Dependence Evaluator Proportion (DEP)
      if (self$options$dep) {
        html <- paste0(html, "
          <h3 style='color: #2874A6; margin-top: 1.5em;'>Dependence Evaluator Proportion (DEP)</h3>
          <p>The Dependence Evaluator Proportion (DEP) measures, for each row category, whether the probability of a designated outcome (as opposed to other outcomes) is higher or lower than in all other rows combined. Unlike symmetric post-hoc measures that treat all cells equally, DEP is <strong>asymmetric</strong>: in this module, the row variable is treated as the <strong>independent variable</strong> (predictor), whilst the column variable is treated as the <strong>dependent variable</strong> (outcome). The user must select one column category as the <strong>outcome of interest</strong>, and DEP then evaluates whether each row category increases or decreases the probability of that outcome.</p>
          
          <p><strong>Calculation:</strong> For each row category <i>i</i>, DEP computes the probability of the designated outcome (versus all other column categories) within that row, and compares it to the same probability across all other rows combined. Specifically, it calculates the difference between P(Outcome | Row = <i>i</i>) and P(Outcome | Row ≠ <i>i</i>), normalised by their sum:</p>
          
          <p style='text-align: center; font-style: italic;'>
            DEP = [P(Y|X<sub>i</sub>) − P(Y|¬X<sub>i</sub>)] / [P(Y|X<sub>i</sub>) + P(Y|¬X<sub>i</sub>)]
          </p>
          
          <p>This normalisation bounds DEP to the interval [−1, +1], where:</p>
          <ul style='margin-left: 2em;'>
            <li><strong>DEP = 0:</strong> Independence — the row category has no effect on outcome probability</li>
            <li><strong>DEP > 0:</strong> Attraction — the outcome is <em>more</em> probable in this row than in other rows</li>
            <li><strong>DEP < 0:</strong> Repulsion — the outcome is <em>less</em> probable in this row than in other rows</li>
            <li><strong>DEP = +1:</strong> Perfect positive association — the outcome occurs <em>only</em> in this row category</li>
            <li><strong>DEP = −1:</strong> Perfect negative association — the outcome <em>never</em> occurs in this row category</li>
          </ul>
          
          <p><strong>Statistical significance:</strong> Significance bounds are derived from the chi-squared distribution (α = 0.05, df = 1). The method calculates the range of DEP values that would be expected under the null hypothesis of independence, given the observed marginal totals. If the observed DEP falls outside these bounds, the association is considered statistically significant.</p>
          
          <p><strong>One-versus-rest contrast:</strong> Each row category is evaluated independently against all other rows combined. For example, 'Does living in a large town predict feeling very safe (versus other feelings), compared to living elsewhere?' Each row receives its own DEP value reflecting how it stands out from the rest.</p>
          
          <p><strong>Important notes:</strong> (1) DEP requires designating which variable is dependent and which column category is the outcome of interest. This asymmetric design differs from measures like adjusted standardised residuals, which treat rows and columns symmetrically. (2) The method originates from a preprint and has not yet undergone peer review. Users should interpret results with appropriate caution.</p>
          
          <p><strong>Forest plot visualisation:</strong> The optional DEP forest plot displays each row category's DEP value as a point. Because significance bounds vary across rows (depending on each row's marginal totals), each row shows its own acceptance region as a horizontal grey segment. Points falling outside their row's acceptance region represent statistically significant associations. Significant positive values (attraction) appear in gold, significant negative values (repulsion) in maroon, and non-significant values in grey. The vertical line at zero represents independence.</p>
        ")
      }
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # -------------------------------------------------------------------------
    # Render PEM forest plot — Version B: Dot-and-whisker
    # -------------------------------------------------------------------------
    .pemPlot = function(image, ggtheme, theme, ...) {
      
      # Retrieve plot data from state
      plot_data <- image$state
      
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        # Message instead of an empty plot when the filter leaves no cells
        print(ggplot2::ggplot() + ggtheme + ggplot2::theme_void() +
                ggplot2::annotate("text", x = 0, y = 0, size = 5, colour = "grey50",
                                  label = "No cells match the current filter."))
        return(TRUE)
      }
      
      # Keep the current order (already sorted by PEM), first row at the bottom
      plot_data$label <- factor(plot_data$label, levels = plot_data$label)
      # Point colours: maroon for significant negative, goldenrod for
      # significant positive, grey for non-significant cells
      catCols <- c("Significant negative" = "#8B1A1A", "Significant positive" = "#DAA520",
                   "Non-significant" = "#A6A6A6")
      plot_data$category <- factor(ifelse(!plot_data$significant, "Non-significant",
        ifelse(plot_data$pem < 0, "Significant negative", "Significant positive")),
        levels = names(catCols))
      
      # jamovi theme first: it carries its own colour scales, which would
      # otherwise replace the manual scales below
      plt <- ggplot2::ggplot(plot_data, ggplot2::aes(y = label)) + ggtheme +
        ggplot2::geom_vline(xintercept = 0, colour = "#808080", linewidth = 0.7) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper),
                               width = 0.2, colour = "#505050", linewidth = 0.6,
                               orientation = "y", na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(x = pem, colour = category), size = 3) +
        ggplot2::scale_colour_manual(values = catCols, drop = TRUE, name = NULL) +
        ggplot2::scale_x_continuous(limits = c(-110, 110), breaks = seq(-100, 100, by = 50)) +
        ggplot2::labs(x = "PEM (%)", y = NULL,
                      title = "PEM Scores with Bootstrap Confidence Intervals") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")
      
      print(plt)
      TRUE
    },
    
    # -------------------------------------------------------------------------
    # Render DEP forest plot
    # -------------------------------------------------------------------------
    .depPlot = function(image, ggtheme, theme, ...) {
      plot_data <- image$state
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        print(ggplot2::ggplot() + ggtheme + ggplot2::theme_void() +
                ggplot2::annotate("text", x = 0, y = 0, size = 5, colour = "grey50",
                                  label = "No data available for DEP plot."))
        return(TRUE)
      }
      # Get outcome category from attribute
      outcome_cat <- attr(plot_data, "outcome")
      if (is.null(outcome_cat)) outcome_cat <- ""
      
      # Keep the current order (already sorted by DEP), first row at the bottom
      plot_data$label <- factor(plot_data$label, levels = plot_data$label)
      # Point colours: maroon for significant negative, goldenrod for
      # significant positive, grey for non-significant cells
      catCols <- c("Significant negative" = "#8B1A1A", "Significant positive" = "#DAA520",
                   "Non-significant" = "#A6A6A6")
      plot_data$category <- factor(ifelse(!plot_data$significant, "Non-significant",
        ifelse(plot_data$dep < 0, "Significant negative", "Significant positive")),
        levels = names(catCols))
      
      # jamovi theme first: it carries its own colour scales, which would
      # otherwise replace the manual scales below
      plt <- ggplot2::ggplot(plot_data, ggplot2::aes(y = label)) + ggtheme +
        ggplot2::geom_vline(xintercept = 0, colour = "#808080", linewidth = 0.7) +
        # Acceptance region of each row (grey band)
        ggplot2::geom_linerange(ggplot2::aes(xmin = sig_lower, xmax = sig_upper,
                                             linetype = "Acceptance region"),
                                colour = "#D0D0D0", linewidth = 2.5) +
        ggplot2::geom_point(ggplot2::aes(x = dep, colour = category), size = 3.2) +
        ggplot2::scale_colour_manual(values = catCols, drop = TRUE, name = NULL) +
        ggplot2::scale_linetype_manual(values = c("Acceptance region" = "solid"), name = NULL) +
        ggplot2::scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, by = 0.5)) +
        ggplot2::labs(x = "DEP", y = NULL, title = paste0("DEP: Outcome = ", outcome_cat)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       legend.position = "bottom")
      
      print(plt)
      TRUE
    },
    
    .lastStdRes = NULL,
    .lastMomCorrStdRes = NULL,
    .lastAdjStdRes = NULL,
    .lastQuetelet = NULL,
    .lastIJ = NULL,
    .lastPEM = NULL,
    .lastMedPolish = NULL,
    .lastAdjMedPolish = NULL,
    .lastMedPolishThresholds = NULL,
    .pemPlotData = NULL,
    .lastDEP = NULL,
    .depPlotData = NULL,
    .lastGKResCol = NULL,
    .lastGKResRow = NULL,
    .lastBSOutlier = NULL,
    
    # New fields for table stability
    .rowLevels = NULL,
    .colLevels = NULL,
    .I = NULL,
    .J = NULL,
    .defaultDepOutcome = NULL,
    
    # Cache control fields
    .lastDataHash = NULL,
    .lastBootstrapOptions = NULL,
    .confLevelProp = NULL
  )
)