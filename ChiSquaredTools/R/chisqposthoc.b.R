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
      
      if (!is.null(self$options$cols)) {
        colVar <- self$options$cols
        
        if (colVar %in% names(self$data)) {
          col_data <- self$data[[colVar]]
          
          if (is.factor(col_data)) {
            levels_list <- levels(col_data)
          } else {
            levels_list <- unique(as.character(col_data))
          }
          
          # Populate the depOutcome dropdown
          depOutcome_option <- self$options$depOutcome
          if (is.null(depOutcome_option) || !(depOutcome_option %in% levels_list)) {
            # Set default to first level if not already set
            private$.defaultDepOutcome <- levels_list[1]
          }
          
          # Update the option levels for the UI
          self$results$depTable$setVisible(self$options$dep)
        }
      }
    },
    
    .run = function() {
      
      if (is.null(self$options$rows) || is.null(self$options$cols)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      data <- self$data
      
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
      
      # Always populate the crosstab
      private$.populateCrosstab(contingency_table)
      
      n <- sum(contingency_table)
      row_totals <- rowSums(contingency_table)
      col_totals <- colSums(contingency_table)
      expected <- outer(row_totals, col_totals) / n
      
      if (self$options$stdres) {
        stdres <- private$.computeStandardisedResiduals(contingency_table, expected)
        private$.populateMetricTable(stdres, 'stdresTable', 'Standardised Residuals', rowVar, colVar)
        private$.populateStdresNote()
      }
      
      if (self$options$momcorrstdres) {
        momcorrstdres <- private$.computeMomentCorrectedStandardisedResiduals(contingency_table, expected)
        private$.populateMetricTable(momcorrstdres, 'momcorrstdresTable', 'Moment-Corrected Standardised Residuals', rowVar, colVar)
        private$.populateMomCorrStdresNote()
      }
      
      if (self$options$adjstdres) {
        adjstdres <- private$.computeAdjustedStandardisedResiduals(contingency_table, expected, n)
        private$.populateMetricTable(adjstdres, 'adjstdresTable', 'Adjusted Standardised Residuals', rowVar, colVar)
        private$.populateAdjstdresNote()
      }
      
      if (self$options$quetelet) {
        quetelet <- private$.computeQueteletIndex(contingency_table, expected)
        private$.populateMetricTableEmpirical(quetelet, 'queteletTable', 'Quetelet Index', c(1.0, -0.50), rowVar, colVar)
        private$.populateQueteletNote()
      }
      
      if (self$options$ij) {
        ij <- private$.computeIJAssociation(contingency_table, expected)
        private$.populateMetricTableEmpirical(ij, 'ijTable', 'IJ Association Factor', c(2.0, 0.5), rowVar, colVar)
        private$.populateIJNote()
      }
      
      if (self$options$bsOutlier) {
        bsoutlier_results <- private$.computeBackwardsSteppingOutliers(contingency_table)
        private$.populateBSOutlierMatrixTable(bsoutlier_results, rowVar, colVar)
        private$.populateBSOutlierDetailTable(bsoutlier_results, rowVar, colVar)
        private$.populateBSOutlierNote(bsoutlier_results)
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
        private$.populatePEMNote()
        
        # Prepare PEM plot if requested
        if (self$options$showPemPlot) {
          private$.preparePemPlotData(pem_results, rowVar, colVar)
        }
      }
      
      if (self$options$medpolish || self$options$adjmedpolish) {
        mp_results <- private$.computeMedianPolishResiduals(contingency_table)
        
        if (self$options$medpolish) {
          private$.populateMetricTableFourthSpread(mp_results$pearson_mp_residuals, 
                                                   'medpolishTable', 
                                                   'Standardised Median Polish Residuals', 
                                                   rowVar, colVar)
          private$.populateMedianPolishNote()
        }
        
        if (self$options$adjmedpolish) {
          private$.populateMetricTableFourthSpread(mp_results$haberman_mp_residuals, 
                                                   'adjmedpolishTable', 
                                                   'Adjusted Standardised Median Polish Residuals', 
                                                   rowVar, colVar)
          private$.populateAdjMedianPolishNote()
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
        private$.populateGKResNote()
      }
      
      if (self$options$dep) {
        dep_results <- private$.computeDEP(contingency_table, colVar)
        private$.populateDEPTable(dep_results, rowVar, colVar)
        private$.populateDEPNote()
        
        if (self$options$showDepPlot) {
          private$.prepareDepPlotData(dep_results)
        }
      }
      
      if (self$options$showSignificanceTables) {
        private$.populateSignificanceTables()
      }
      
      private$.populateReferences()
      private$.populateMethodInfo()
    },
    .populateCrosstab = function(contingency_table) {
      
      table <- self$results$crosstabTable
      
      I <- nrow(contingency_table)
      J <- ncol(contingency_table)
      row_names <- rownames(contingency_table)
      col_names <- colnames(contingency_table)
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      table$setTitle(paste0(rowVar, " × ", colVar))
      
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
      alpha <- 1 - self$options$confLevel
      
      pem_boot <- array(NA, dim = c(I, J, B))
      
      expanded_data <- data.frame(
        row = rep(rep(1:I, J), times = as.vector(N_matrix)),
        col = rep(rep(1:J, each = I), times = as.vector(N_matrix))
      )
      
      for (b in 1:B) {
        boot_indices <- sample(1:nrow(expanded_data), replace = TRUE)
        boot_table <- table(expanded_data[boot_indices, 1], 
                            expanded_data[boot_indices, 2])
        
        boot_N_iplus <- rowSums(boot_table)
        boot_N_plusj <- colSums(boot_table)
        boot_E_ij <- outer(boot_N_iplus, boot_N_plusj) / N
        
        for (i in 1:I) {
          for (j in 1:J) {
            boot_n_ij <- boot_table[i, j]
            boot_e_ij <- boot_E_ij[i, j]
            
            if (boot_n_ij >= boot_e_ij) {
              boot_max_ij <- min(boot_N_iplus[i], boot_N_plusj[j])
              if (boot_max_ij > boot_e_ij) {
                pem_boot[i, j, b] <- ((boot_n_ij - boot_e_ij) / 
                                        (boot_max_ij - boot_e_ij)) * 100
              } else {
                pem_boot[i, j, b] <- 0
              }
            } else {
              boot_min_ij <- 0
              pem_boot[i, j, b] <- -((boot_e_ij - boot_n_ij) / 
                                       (boot_e_ij - boot_min_ij)) * 100
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
        confLevel = self$options$confLevel,
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
        confLevel = self$options$confLevel,
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
      deleted_residuals <- numeric()
      g2_drop_values <- numeric()
      residual_signs <- numeric()
      
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
        k_max = k_max,
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
    
    .populateMetricTable = function(metric_matrix, table_name, metric_label, rowVar, colVar) {
      
      table <- self$results[[table_name]]
      
      table$setTitle(paste0(metric_label, ": ", rowVar, " × ", colVar))
      
      I <- nrow(metric_matrix)
      J <- ncol(metric_matrix)
      row_names <- rownames(metric_matrix)
      col_names <- colnames(metric_matrix)
      
      if (metric_label %in% c('Standardised Residuals', 'Moment-Corrected Standardised Residuals', 'Adjusted Standardised Residuals')) {
        
        if (self$options$sidakCorrection) {
          k <- I * J
          alpha <- 0.05
          alpha_corrected <- 1 - (1 - alpha)^(1/k)
          threshold <- qnorm(1 - alpha_corrected/2)
        } else {
          threshold <- 1.96
        }
        
      } else if (metric_label %in% c('Standardised Median Polish Residuals', 
                                     'Adjusted Standardised Median Polish Residuals')) {
        threshold <- 3
      } else {
        threshold <- NULL
      }
      
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
          type = 'text',
          superTitle = colVar,
          combineBelow = FALSE
        )
      }
      
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- metric_matrix[i, j]
          
          formatted_value <- sprintf("%.3f", value)
          
          if (!is.null(threshold)) {
            if (abs(value) > threshold) {
              if (value > 0) {
                formatted_value <- paste0("<span style='display: block; text-align: center; color: red;'>", formatted_value, "</span>")
              } else {
                formatted_value <- paste0("<span style='display: block; text-align: center; color: blue;'>", formatted_value, "</span>")
              }
            } else {
              formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
            }
          } else {
            formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
          }
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$addRow(rowKey = i, values = row_values)
      }
    },
    .populateMetricTableFourthSpread = function(metric_matrix, table_name, metric_label, rowVar, colVar) {
      
      table <- self$results[[table_name]]
      
      table$setTitle(paste0(metric_label, ": ", rowVar, " × ", colVar))
      
      I <- nrow(metric_matrix)
      J <- ncol(metric_matrix)
      row_names <- rownames(metric_matrix)
      col_names <- colnames(metric_matrix)
      
      # Compute fourth-spread thresholds (Hoaglin et al. 1985)
      all_values <- as.vector(metric_matrix)
      n_cells <- length(all_values)
      F_U <- quantile(all_values, 0.75)  # Upper fourth (75th percentile)
      F_L <- quantile(all_values, 0.25)  # Lower fourth (25th percentile)
      d_F <- F_U - F_L                   # Fourth-spread (interquartile range)
      
      lower_cutoff <- F_L - 1.5 * d_F
      upper_cutoff <- F_U + 1.5 * d_F
      
      # Expected number of outliers (Hoaglin, Iglewicz, & Tukey 1981)
      expected_outliers <- 0.007 * n_cells + 0.4
      
      # Count actual outliers
      n_outliers <- sum(all_values < lower_cutoff | all_values > upper_cutoff)
      
      # Store thresholds for later use
      private$.lastMedPolishThresholds <- list(
        lower = lower_cutoff,
        upper = upper_cutoff,
        F_L = F_L,
        F_U = F_U,
        d_F = d_F,
        n_cells = n_cells,
        expected_outliers = expected_outliers,
        n_outliers = n_outliers
      )
      
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
          type = 'text',
          superTitle = colVar,
          combineBelow = FALSE
        )
      }
      
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- metric_matrix[i, j]
          
          formatted_value <- sprintf("%.3f", value)
          
          if (value > upper_cutoff) {
            formatted_value <- paste0("<span style='display: block; text-align: center; color: red;'>", formatted_value, "</span>")
          } else if (value < lower_cutoff) {
            formatted_value <- paste0("<span style='display: block; text-align: center; color: blue;'>", formatted_value, "</span>")
          } else {
            formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
          }
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$addRow(rowKey = i, values = row_values)
      }
    },
    
    .populateMetricTableEmpirical = function(metric_matrix, table_name, metric_label, thresholds, rowVar, colVar) {
      
      table <- self$results[[table_name]]
      
      table$setTitle(paste0(metric_label, ": ", rowVar, " × ", colVar))
      
      I <- nrow(metric_matrix)
      J <- ncol(metric_matrix)
      row_names <- rownames(metric_matrix)
      col_names <- colnames(metric_matrix)
      
      positive_threshold <- thresholds[1]
      negative_threshold <- thresholds[2]
      
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
          type = 'text',
          superTitle = colVar,
          combineBelow = FALSE
        )
      }
      
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- metric_matrix[i, j]
          
          formatted_value <- sprintf("%.3f", value)
          
          if (value > positive_threshold) {
            formatted_value <- paste0("<span style='display: block; text-align: center; color: red;'>", formatted_value, "</span>")
          } else if (value < negative_threshold) {
            formatted_value <- paste0("<span style='display: block; text-align: center; color: blue;'>", formatted_value, "</span>")
          } else {
            formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
          }
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$addRow(rowKey = i, values = row_values)
      }
    },
    
    .populatePEMTable = function(pem_results, rowVar, colVar) {
      
      table <- self$results$pemTable
      
      table$setTitle(paste0("PEM / Sakoda's D Local with Confidence Intervals: ", rowVar, " × ", colVar))
      
      pem <- pem_results$pem
      ci_lower <- pem_results$ci_lower
      ci_upper <- pem_results$ci_upper
      
      I <- nrow(pem)
      J <- ncol(pem)
      row_names <- rownames(pem)
      col_names <- colnames(pem)
      
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
          type = 'text',
          superTitle = colVar,
          combineBelow = FALSE
        )
      }
      
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          pem_value <- pem[i, j]
          ci_low <- ci_lower[i, j]
          ci_up <- ci_upper[i, j]
          
          is_significant <- (ci_low > 0) || (ci_up < 0)
          
          formatted_value <- sprintf("%.1f [%.1f, %.1f]", pem_value, ci_low, ci_up)
          
          if (is_significant) {
            if (pem_value > 0) {
              formatted_value <- paste0("<span style='display: block; text-align: center; color: red;'>", formatted_value, "</span>")
            } else {
              formatted_value <- paste0("<span style='display: block; text-align: center; color: blue;'>", formatted_value, "</span>")
            }
          } else {
            formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
          }
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$addRow(rowKey = i, values = row_values)
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
        
        # Format significance cell with colour
        if (!is.na(result$dep)) {
          if (result$significant) {
            if (result$dep > 0) {
              sig_formatted <- paste0("<span style='color: red;'>", result$conclusion, "</span>")
            } else {
              sig_formatted <- paste0("<span style='color: blue;'>", result$conclusion, "</span>")
            }
          } else {
            sig_formatted <- result$conclusion
          }
        } else {
          sig_formatted <- result$conclusion
        }
        
        table$addRow(rowKey = i, values = list(
          rowCat = result$row,
          depOutcome = result$outcome,
          depValue = if (is.na(result$dep)) NA else round(result$dep, 4),
          sigLower = if (is.na(result$sig_lower)) NA else round(result$sig_lower, 4),
          sigUpper = if (is.na(result$sig_upper)) NA else round(result$sig_upper, 4),
          significance = sig_formatted
        ))
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate Goodman-Kruskal Residuals table
    # -------------------------------------------------------------------------
    .populateGKResidualTable = function(gk_matrix, table_name, title_label, rowVar, colVar, predictor_type) {
      
      table <- self$results[[table_name]]
      
      if (predictor_type == 'col') {
        # Columns as predictor, rows as response
        table$setTitle(paste0(title_label, ": ", colVar, " → ", rowVar))
      } else {
        # Rows as predictor, columns as response
        table$setTitle(paste0(title_label, ": ", rowVar, " → ", colVar))
      }
      
      I <- nrow(gk_matrix)
      J <- ncol(gk_matrix)
      row_names <- rownames(gk_matrix)
      col_names <- colnames(gk_matrix)
      
      # Add row name column
      table$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text',
        combineBelow = FALSE
      )
      
      # Add columns for each column category
      for (j in 1:J) {
        table$addColumn(
          name = paste0("col", j),
          title = col_names[j],
          type = 'text',
          superTitle = colVar,
          combineBelow = FALSE
        )
      }
      
      # Populate rows
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- gk_matrix[i, j]
          
          # Format to 3 decimal places, no colour-coding (no formal thresholds)
          formatted_value <- sprintf("%.3f", value)
          formatted_value <- paste0("<span style='display: block; text-align: center;'>", 
                                    formatted_value, "</span>")
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$addRow(rowKey = i, values = row_values)
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
      
      # Add row name column
      table$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text',
        combineBelow = FALSE
      )
      
      # Add columns for each column category
      for (j in 1:J) {
        table$addColumn(
          name = paste0("col", j),
          title = col_names[j],
          type = 'text',
          superTitle = colVar,
          combineBelow = FALSE
        )
      }
      
      # Populate rows
      for (i in 1:I) {
        row_values <- list(rowname = row_names[i])
        
        for (j in 1:J) {
          value <- del_res_matrix[i, j]
          is_detected <- detection_matrix[i, j]
          
          if (is.na(value)) {
            # Cell was not among the k_max identified
            formatted_value <- paste0("<span style='display: block; text-align: center; color: #999;'>—</span>")
          } else {
            formatted_value <- sprintf("%.3f", value)
            
            if (is_detected) {
              # Detected outlier: colour by sign (no bold, consistent with other methods)
              if (value > 0) {
                formatted_value <- paste0("<span style='display: block; text-align: center; color: red;'>", formatted_value, "</span>")
              } else {
                formatted_value <- paste0("<span style='display: block; text-align: center; color: blue;'>", formatted_value, "</span>")
              }
            } else {
              # Identified but not detected
              formatted_value <- paste0("<span style='display: block; text-align: center;'>", formatted_value, "</span>")
            }
          }
          
          row_values[[paste0("col", j)]] <- formatted_value
        }
        
        table$addRow(rowKey = i, values = row_values)
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate Backwards-Stepping Outlier Detail Table
    # -------------------------------------------------------------------------
    .populateBSOutlierDetailTable = function(results, rowVar, colVar) {
      
      table <- self$results$bsOutlierDetailTable
      
      table$setTitle(paste0("Backwards-Stepping: Identified Cells (by extremeness)"))
      
      detail_list <- results$detail_list
      critical_value <- results$critical_value
      
      for (i in seq_along(detail_list)) {
        item <- detail_list[[i]]
        
        # Format cell name with actual level names
        cell_name <- paste0(item$row_name, " / ", item$col_name)
        
        # Format detected column with colour
        if (item$detected) {
          if (item$deleted_residual > 0) {
            detected_formatted <- paste0("<span style='color: red;'>Yes</span>")
          } else {
            detected_formatted <- paste0("<span style='color: blue;'>Yes</span>")
          }
        } else {
          detected_formatted <- "No"
        }
        
        table$addRow(rowKey = i, values = list(
          rank = item$rank,
          cell = cell_name,
          delRes = round(item$deleted_residual, 3),
          g2Drop = round(item$g2_drop, 3),
          detected = detected_formatted
        ))
      }
    },
    
    # -------------------------------------------------------------------------
    # Populate Backwards-Stepping Outlier interpretation note
    # -------------------------------------------------------------------------
    .populateBSOutlierNote = function(results) {
      
      critical_value <- results$critical_value
      k_max <- results$k_max
      detected_count <- results$detected_count
      alpha <- results$alpha
      nr <- nrow(results$del_res_matrix)
      nc <- ncol(results$del_res_matrix)
      RC <- nr * nc
      
      if (detected_count > 0) {
        detection_text <- sprintf(
          "The backwards-stepping procedure detected <strong>%d outlier cell(s)</strong>. ",
          detected_count
        )
      } else {
        detection_text <- "The backwards-stepping procedure detected <strong>no outlier cells</strong>. "
      }
      
      # Compute Simonoff's recommended range (20-30% of cells)
      simonoff_low <- max(1, ceiling(0.20 * RC))
      simonoff_high <- max(1, ceiling(0.30 * RC))
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        detection_text,
        sprintf(
          "Up to %d cells were tested (k<sub>max</sub> = %d). ",
          k_max, k_max
        ),
        sprintf(
          "<em>Note:</em> For this %d × %d table (%d cells), Simonoff (1988, p. 340) suggests k<sub>max</sub> in the range %d–%d (i.e., 20–30%% of cells). ",
          nr, nc, RC, simonoff_low, simonoff_high
        ),
        sprintf(
          "The Bonferroni-corrected critical value was %.3f (α = %.3f / %d cells, χ²<sub>1</sub>). ",
          critical_value, alpha, RC
        ),
        "Cells are ordered by extremeness based on deleted residuals. ",
        "The G² drop measures how much the lack-of-fit statistic decreases when each cell is removed from the model. ",
        "Detected outliers are those whose G² drop exceeds the critical value, testing from least to most extreme (backwards-stepping). ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = significant positive (attraction), ",
        "<span style='color: blue;'>blue</span> = significant negative (repulsion).</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'>",
        "<em>See: Simonoff 1988.</em></p>",
        "</div>"
      )
      
      self$results$bsOutlierNote$setContent(note_html)
    },
    
    # -------------------------------------------------------------------------
    # Populate GK Residuals interpretation note
    # -------------------------------------------------------------------------
    .populateGKResNote = function() {
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Goodman-Kruskal residuals measure how knowing the predictor category changes the probability ",
        "of each response category relative to its marginal (unconditional) probability. ",
        "For <strong>", colVar, " → ", rowVar, "</strong>: positive values indicate that knowing the column category ",
        "<em>increases</em> the probability of the row category above its marginal rate; ",
        "negative values indicate a <em>decrease</em>. ",
        "For <strong>", rowVar, " → ", colVar, "</strong>: the interpretation is reversed (rows predict columns). ",
        "Values are comparable within each table and reflect the <em>magnitude</em> of predictability shift. ",
        "No formal significance thresholds exist; interpret relative magnitudes in context.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'>",
        "<em>See: Kroonenberg & Lombardo 1999; Beh & Lombardo 2021.</em></p>",
        "</div>"
      )
      self$results$gkresNote$setContent(note_html)
    },
    
    .populateDEPNote = function() {
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "For each row category, DEP measures whether the probability of the designated outcome ",
        "(as opposed to other outcomes) is higher or lower than in all other rows combined. ",
        "DEP = 0 indicates independence; positive values indicate the outcome is more probable in this row; ",
        "negative values indicate it is less probable. ",
        "DEP ranges from -1 to +1. ",
        "<strong>Significance:</strong> Determined by chi-squared-derived bounds (α = 0.05). ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = significant positive, ",
        "<span style='color: blue;'>blue</span> = significant negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'>",
        "<em>Note: DEP treats the column variable as the dependent variable. ",
        "See: Gambirasio 2024 (preprint).</em></p>",
        "</div>"
      )
      self$results$depNote$setContent(note_html)
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
    
    .populateStdresNote = function() {
      
      # Calculate threshold (with or without Šidák correction)
      if (self$options$sidakCorrection && !is.null(private$.lastStdRes)) {
        k <- nrow(private$.lastStdRes) * ncol(private$.lastStdRes)
        alpha <- 0.05
        alpha_corrected <- 1 - (1 - alpha)^(1/k)
        threshold_used <- qnorm(1 - alpha_corrected/2)
        correction_text <- sprintf(
          " with Šidák correction (α<sub>adjusted</sub> = %.4f, threshold = ±%.3f for %d comparisons)",
          alpha_corrected, threshold_used, k
        )
        threshold_display <- sprintf("%.3f", threshold_used)
      } else {
        correction_text <- ""
        threshold_display <- "1.96"
      }
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Standardised residuals measure deviation from independence. ",
        "Values &gt;", threshold_display, " or &lt;-", threshold_display, " (α=0.05) indicate significant association",
        correction_text,
        ". Positive values indicate attraction between levels, negative values indicate repulsion. ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = significant positive, ",
        "<span style='color: blue;'>blue</span> = significant negative.</p>",
        "</div>"
      )
      self$results$stdresNote$setContent(note_html)
    },
    
    .populateMomCorrStdresNote = function() {
      
      # Calculate threshold (with or without Šidák correction)
      if (self$options$sidakCorrection && !is.null(private$.lastMomCorrStdRes)) {
        k <- nrow(private$.lastMomCorrStdRes) * ncol(private$.lastMomCorrStdRes)
        alpha <- 0.05
        alpha_corrected <- 1 - (1 - alpha)^(1/k)
        threshold_used <- qnorm(1 - alpha_corrected/2)
        correction_text <- sprintf(
          " with Šidák correction (α<sub>adjusted</sub> = %.4f, threshold = ±%.3f for %d comparisons)",
          alpha_corrected, threshold_used, k
        )
        threshold_display <- sprintf("%.3f", threshold_used)
      } else {
        correction_text <- ""
        threshold_display <- "1.96"
      }
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Moment-corrected standardised residuals adjust for the number of rows and columns, ",
        "providing a correction that accounts for table dimensions. ",
        "Values &gt;", threshold_display, " or &lt;-", threshold_display, " (α=0.05) indicate significant association",
        correction_text,
        ". Positive values indicate attraction between levels, negative values indicate repulsion. ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = significant positive, ",
        "<span style='color: blue;'>blue</span> = significant negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Garcia-Perez & Nunez-Anton 2003.</em></p>",
        "</div>"
      )
      self$results$momcorrstdresNote$setContent(note_html)
    },
    .populateAdjstdresNote = function() {
      
      # Calculate threshold (with or without Šidák correction)
      if (self$options$sidakCorrection && !is.null(private$.lastAdjStdRes)) {
        k <- nrow(private$.lastAdjStdRes) * ncol(private$.lastAdjStdRes)
        alpha <- 0.05
        alpha_corrected <- 1 - (1 - alpha)^(1/k)
        threshold_used <- qnorm(1 - alpha_corrected/2)
        correction_text <- sprintf(
          " with Šidák correction (α<sub>adjusted</sub> = %.4f, threshold = ±%.3f for %d comparisons)",
          alpha_corrected, threshold_used, k
        )
        threshold_display <- sprintf("%.3f", threshold_used)
      } else {
        correction_text <- ""
        threshold_display <- "1.96"
      }
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Adjusted standardised residuals account for unequal marginal totals. ",
        "Values &gt;", threshold_display, " or &lt;-", threshold_display, " (α=0.05) indicate significant association",
        correction_text,
        ". Positive values indicate attraction between levels, negative values indicate repulsion. ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = significant positive, ",
        "<span style='color: blue;'>blue</span> = significant negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Haberman 1973.</em></p>",
        "</div>"
      )
      self$results$adjstdresNote$setContent(note_html)
    },
    
    .populateQueteletNote = function() {
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Quetelet Index = (observed freq/expected freq) − 1. ",
        "It expresses the relative change in probability when considering the association. ",
        "A value of 0.50 means the probability is 50% higher than expected under independence; ",
        "-0.30 means 30% lower. ",
        "<strong>Empirical magnitude thresholds:</strong> &gt;1.0 (noteworthy positive), &lt;-0.50 (noteworthy negative). ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = noteworthy positive, ",
        "<span style='color: blue;'>blue</span> = noteworthy negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Mirkin 2001, 2023.</em></p>",
        "</div>"
      )
      self$results$queteletNote$setContent(note_html)
    },
    
    .populateIJNote = function() {
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "IJ Factor = observed freq/expected freq. ",
        "It represents the factor by which the probability changes when considering the association. ",
        "A value of 2.0 means the probability is twice what would be expected under independence; ",
        "0.5 means half. ",
        "<strong>Empirical magnitude thresholds:</strong> &gt;2.0 (noteworthy positive), &lt;0.5 (noteworthy negative). ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = noteworthy positive, ",
        "<span style='color: blue;'>blue</span> = noteworthy negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Good 1956; Agresti 2013.</em></p>",
        "</div>"
      )
      self$results$ijNote$setContent(note_html)
    },
    
    .populatePEMNote = function() {
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "PEM, equivalent to Sakoda's D Local × 100, ranges from -100% (maximum repulsion) through 0% (independence) to +100% (maximum attraction). ",
        "Positive values indicate attraction, negative values indicate repulsion. ",
        "<strong>Significance:</strong> Bootstrap confidence intervals not containing zero. ",
        "<strong>Empirical magnitude thresholds:</strong> ",
        "&lt;5% = Negligible, 5-10% = Weak, &ge;10% = Interesting/noteworthy, &ge;50% = Exceptional. ",
        "<strong>Colour-coding:</strong> <span style='color: red;'>red</span> = significant positive, ",
        "<span style='color: blue;'>blue</span> = significant negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Sakoda 1981; Cibois 1993; Lefèvre & Champely 2009.</em></p>",
        "</div>"
      )
      self$results$pemNote$setContent(note_html)
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
    
    .populateMedianPolishNote = function() {
      
      # Get the stored thresholds
      thresholds <- private$.lastMedPolishThresholds
      
      # ENHANCED: Assess outlier pattern (Priority 1 & 2)
      excess_outliers <- thresholds$n_outliers - round(thresholds$expected_outliers)
      
      if (thresholds$n_outliers > thresholds$expected_outliers) {
        
        if (excess_outliers >= 2) {
          outlier_interpretation <- sprintf(
            "<strong>Outlier assessment:</strong> Found %d extreme values vs. ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; excess = %d − %.0f = %d). The excess suggests genuine extreme associations are present. Focus interpretation on the most extreme values (those furthest from the cutoffs), as these are most likely to represent true outliers rather than chance fluctuations.",
            thresholds$n_outliers, thresholds$expected_outliers,
            thresholds$n_cells, thresholds$expected_outliers,
            thresholds$n_outliers, round(thresholds$expected_outliers), excess_outliers
          )
        } else {
          outlier_interpretation <- sprintf(
            "<strong>Outlier assessment:</strong> Found %d extreme value(s) vs. ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; excess = %d − %.0f = %d). This modest excess suggests caution: values barely exceeding the cutoffs may represent normal variation. Prioritise interpretation of the most extreme values.",
            thresholds$n_outliers, thresholds$expected_outliers,
            thresholds$n_cells, thresholds$expected_outliers,
            thresholds$n_outliers, round(thresholds$expected_outliers), excess_outliers
          )
        }
        
      } else if (thresholds$n_outliers == round(thresholds$expected_outliers)) {
        outlier_interpretation <- sprintf(
          "<strong>Outlier assessment:</strong> Found %d extreme value(s), matching the ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; excess = 0). This pattern is consistent with normal sampling variation. Values flagged as extreme should be interpreted cautiously and may not represent genuine outliers.",
          thresholds$n_outliers, thresholds$expected_outliers,
          thresholds$n_cells, thresholds$expected_outliers
        )
      } else {
        outlier_interpretation <- sprintf(
          "<strong>Outlier assessment:</strong> Found %d extreme value(s) vs. ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; deficit = %.0f − %d = %d). The observed count is within or below expectation, suggesting the data conform well to the model.",
          thresholds$n_outliers, thresholds$expected_outliers,
          thresholds$n_cells, thresholds$expected_outliers,
          round(thresholds$expected_outliers), thresholds$n_outliers,
          round(thresholds$expected_outliers) - thresholds$n_outliers
        )
      }
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Median polish is applied to log-transformed counts to generate a resistant fit less affected by outliers. ",
        "Pearson residuals are calculated as (Observed − RobustExpected)/sqrt(RobustExpected). ",
        sprintf(
          "Extreme values are identified using the fourth-spread rule: F<sub>L</sub>=%.3f, F<sub>U</sub>=%.3f, d<sub>F</sub>=%.3f. Lower cutoff = %.3f, Upper cutoff = %.3f. ",
          thresholds$F_L, thresholds$F_U, thresholds$d_F,
          thresholds$lower, thresholds$upper
        ),
        outlier_interpretation,
        " <strong>Colour-coding:</strong> <span style='color: red;'>red</span> = extreme positive, ",
        "<span style='color: blue;'>blue</span> = extreme negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Mosteller & Parunak 1985; Simonoff 2003.</em></p>",
        "</div>"
      )
      self$results$medpolishNote$setContent(note_html)
    },
    
    .populateAdjMedianPolishNote = function() {
      
      # Get the stored thresholds (same as for simple median polish)
      thresholds <- private$.lastMedPolishThresholds
      
      # ENHANCED: Assess outlier pattern (Priority 1 & 2)
      excess_outliers <- thresholds$n_outliers - round(thresholds$expected_outliers)
      
      if (thresholds$n_outliers > thresholds$expected_outliers) {
        
        if (excess_outliers >= 2) {
          outlier_interpretation <- sprintf(
            "<strong>Outlier assessment:</strong> Found %d extreme values vs. ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; excess = %d − %.0f = %d). The excess suggests genuine extreme associations are present. Focus interpretation on the most extreme values (those furthest from the cutoffs), as these are most likely to represent true outliers rather than chance fluctuations.",
            thresholds$n_outliers, thresholds$expected_outliers,
            thresholds$n_cells, thresholds$expected_outliers,
            thresholds$n_outliers, round(thresholds$expected_outliers), excess_outliers
          )
        } else {
          outlier_interpretation <- sprintf(
            "<strong>Outlier assessment:</strong> Found %d extreme value(s) vs. ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; excess = %d − %.0f = %d). This modest excess suggests caution: values barely exceeding the cutoffs may represent normal variation. Prioritise interpretation of the most extreme values.",
            thresholds$n_outliers, thresholds$expected_outliers,
            thresholds$n_cells, thresholds$expected_outliers,
            thresholds$n_outliers, round(thresholds$expected_outliers), excess_outliers
          )
        }
        
      } else if (thresholds$n_outliers == round(thresholds$expected_outliers)) {
        outlier_interpretation <- sprintf(
          "<strong>Outlier assessment:</strong> Found %d extreme value(s), matching the ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; excess = 0). This pattern is consistent with normal sampling variation. Values flagged as extreme should be interpreted cautiously and may not represent genuine outliers.",
          thresholds$n_outliers, thresholds$expected_outliers,
          thresholds$n_cells, thresholds$expected_outliers
        )
      } else {
        outlier_interpretation <- sprintf(
          "<strong>Outlier assessment:</strong> Found %d extreme value(s) vs. ~%.1f expected under normality (expected = 0.007×%d + 0.4 ≈ %.1f; deficit = %.0f − %d = %d). The observed count is within or below expectation, suggesting the data conform well to the model.",
          thresholds$n_outliers, thresholds$expected_outliers,
          thresholds$n_cells, thresholds$expected_outliers,
          round(thresholds$expected_outliers), thresholds$n_outliers,
          round(thresholds$expected_outliers) - thresholds$n_outliers
        )
      }
      
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> ",
        "Median polish is applied to log-transformed counts to generate a resistant fit less affected by outliers. ",
        "Pearson residuals are calculated as (Observed − RobustExpected)/sqrt(RobustExpected), then adjusted using Haberman's method: ",
        "dividing by sqrt((1−row prop)×(1−col prop)) for each cell. ",
        sprintf(
          "Extreme values are identified using the fourth-spread rule: F<sub>L</sub>=%.3f, F<sub>U</sub>=%.3f, d<sub>F</sub>=%.3f. Lower cutoff = %.3f, Upper cutoff = %.3f. ",
          thresholds$F_L, thresholds$F_U, thresholds$d_F,
          thresholds$lower, thresholds$upper
        ),
        outlier_interpretation,
        " <strong>Colour-coding:</strong> <span style='color: red;'>red</span> = extreme positive, ",
        "<span style='color: blue;'>blue</span> = extreme negative.</p>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 5px;'><em>See: Mosteller & Parunak 1985.</em></p>",
        "</div>"
      )
      self$results$adjmedpolishNote$setContent(note_html)
    },
    
    
    .populateReferences = function() {
      
      references_html <- paste0(
        "<div style='font-size: 0.85em; color: #444; margin: 15px 0; line-height: 1.5;'>",
        "<h3 style='color: #2874A6; margin-top: 0.5em; margin-bottom: 0.5em;'>References</h3>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Agresti, A. (2013). <em>Categorical Data Analysis</em> (3rd ed.). Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Beasley, T. M., & Schumacker, R. E. (1995). Multiple regression approach to analyzing contingency tables: Post hoc and planned comparison procedures. <em>The Journal of Experimental Education</em>, 64(1), 79-93.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Beh, E. J., & Lombardo, R. (2021). <em>An Introduction to Correspondence Analysis</em>. John Wiley & Sons.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Cibois, P. (1993). Le PEM, pourcentage de l'écart maximum: Un indice de liaison entre modalités d'un tableau de contingence. <em>Bulletin de Methodologie Sociologique</em>, 40, 43-63.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Efron, B., & Tibshirani, R. J. (1993). <em>An Introduction to the Bootstrap</em>. Chapman & Hall/CRC.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Gambirasio, A. (2024). Dependence evaluator proportion: A measure for contingency table analysis. <em>OSF Preprints</em>. https://doi.org/10.31219/osf.io/s94zr [Preprint]</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Garcia-Perez, M. A., & Nunez-Anton, V. (2003). Cellwise residual analysis in two-way contingency tables. <em>Educational and Psychological Measurement</em>, 63(5), 825-839.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Good, I. J. (1956). On the Estimation of Small Frequencies in Contingency Tables. <em>Journal of the Royal Statistical Society. Series B (Methodological)</em>, 18(1), 113-124.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Haberman, S. J. (1973). The analysis of residuals in cross-classified tables. <em>Biometrics</em>, 29, 205-220.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Kroonenberg, P. M., & Lombardo, R. (1999). Nonsymmetric correspondence analysis: A tool for analysing contingency tables with a dependence structure. <em>Multivariate Behavioral Research</em>, 34(3), 367–396.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Lefèvre, B., & Champely, S. (2009). Méthodes statistiques globales et locales d'analyse d'un tableau de contingence par les tailles d'effet et leurs intervalles de confiance. <em>Bulletin de Methodologie Sociologique</em>, 103, 50-65.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Mirkin, B. (2001). Eleven ways to look at the chi-squared coefficient for contingency tables. <em>The American Statistician</em>, 55(2), 111-120.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Mirkin, B. (2023). A straightforward approach to chi-squared analysis of associations in contingency tables. In E. J. Beh, R. Bertault, P. J. F. Groenen, & Y. Takane (Eds.), <em>Analysis of Categorical Data from Historical Perspectives</em> (pp. 59-72). Springer Nature Singapore.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Mosteller, F., & Parunak, A. (1985). Identifying extreme cells in a sizable contingency table: Probabilistic and exploratory approaches. In D. C. Hoaglin, F. Mosteller, & J. W. Tukey (Eds.), <em>Exploring Data Tables, Trends, and Shapes</em> (pp. 189-224). Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Sakoda, J. M. (1981). A generalized index of dissimilarity. <em>Demography</em>, 18(2), 245–250.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Simonoff, J. S. (1988). Detecting outlying cells in two-way contingency tables via backwards-stepping. <em>Technometrics</em>, 30(3), 339-345.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Simonoff, J. S. (2003). <em>Analyzing Categorical Data</em>. Springer.</p>",
        "</div>"
      )
      
      self$results$legendNote$setContent(references_html)
    },
    
    .populateSignificanceTables = function() {
      
      sig_pos_table <- self$results$sigPosTable
      sig_neg_table <- self$results$sigNegTable
      non_sig_table <- self$results$nonSigTable
      
      all_results <- list()
      
      has_statistical <- FALSE
      has_empirical <- FALSE
      has_extreme <- FALSE
      
      if (self$options$stdres && !is.null(private$.lastStdRes)) {
        all_results <- c(all_results, 
                         private$.extractSignificance(private$.lastStdRes, 'Std. Residual'))
        has_statistical <- TRUE
      }
      
      if (self$options$momcorrstdres && !is.null(private$.lastMomCorrStdRes)) {
        all_results <- c(all_results, 
                         private$.extractSignificance(private$.lastMomCorrStdRes, 'Mom. Corr. Std. Residual'))
        has_statistical <- TRUE
      }
      
      if (self$options$adjstdres && !is.null(private$.lastAdjStdRes)) {
        all_results <- c(all_results, 
                         private$.extractSignificance(private$.lastAdjStdRes, 'Adj. Std. Residual'))
        has_statistical <- TRUE
      }
      
      if (self$options$quetelet && !is.null(private$.lastQuetelet)) {
        all_results <- c(all_results, 
                         private$.extractSignificance(private$.lastQuetelet, 'Quetelet Index', 
                                                      threshold = c(1.0, -0.50), use_empirical = TRUE))
        has_empirical <- TRUE
      }
      
      if (self$options$ij && !is.null(private$.lastIJ)) {
        all_results <- c(all_results, 
                         private$.extractSignificance(private$.lastIJ, 'IJ Factor', 
                                                      threshold = c(2.0, 0.5), use_empirical = TRUE))
        has_empirical <- TRUE
      }
      
      if (self$options$pem && !is.null(private$.lastPEM)) {
        all_results <- c(all_results, 
                         private$.extractPEMSignificance(private$.lastPEM))
        has_statistical <- TRUE
      }
      
      if (self$options$medpolish && !is.null(private$.lastMedPolish)) {
        all_results <- c(all_results, 
                         private$.extractSignificanceFourthSpread(private$.lastMedPolish, 'Std. Med. Polish Residual'))
        has_extreme <- TRUE
      }
      
      if (self$options$adjmedpolish && !is.null(private$.lastAdjMedPolish)) {
        all_results <- c(all_results, 
                         private$.extractSignificanceFourthSpread(private$.lastAdjMedPolish, 'Adj. Std. Med. Polish Residual'))
        has_extreme <- TRUE
      }
      
      if (self$options$dep && !is.null(private$.lastDEP)) {
        all_results <- c(all_results, 
                         private$.extractDEPSignificance(private$.lastDEP))
        has_statistical <- TRUE
      }
      
      if (self$options$bsOutlier && !is.null(private$.lastBSOutlier)) {
        all_results <- c(all_results, 
                         private$.extractBSOutlierSignificance(private$.lastBSOutlier))
        has_statistical <- TRUE
      }
      
      terms <- c()
      
      if (has_statistical) terms <- c(terms, "Significant")
      if (has_empirical) terms <- c(terms, "Noteworthy")
      if (has_extreme) terms <- c(terms, "Extreme")
      
      combined_term <- paste(terms, collapse = "/")
      
      pos_term <- paste(combined_term, "Positive Associations")
      neg_term <- paste(combined_term, "Negative Associations")
      non_term <- paste0("Non-", combined_term, " Associations")
      
      sig_pos_table$setTitle(pos_term)
      sig_neg_table$setTitle(neg_term)
      non_sig_table$setTitle(non_term)
      
      sig_pos <- Filter(function(x) x$significant && x$direction == 'positive', all_results)
      sig_neg <- Filter(function(x) x$significant && x$direction == 'negative', all_results)
      non_sig <- Filter(function(x) !x$significant, all_results)
      
      # For positive associations
      if (length(sig_pos) > 0) {
        for (i in seq_along(sig_pos)) {
          sig_pos_table$addRow(rowKey = i, values = list(
            rowCat = sig_pos[[i]]$row,
            colCat = sig_pos[[i]]$col,
            metric = sig_pos[[i]]$metric,
            value = paste0("<span style='display: block; text-align: right;'>", 
                           sprintf("%.3f", sig_pos[[i]]$value), 
                           "</span>")
          ))
        }
      }
      
      # For negative associations
      if (length(sig_neg) > 0) {
        for (i in seq_along(sig_neg)) {
          sig_neg_table$addRow(rowKey = i, values = list(
            rowCat = sig_neg[[i]]$row,
            colCat = sig_neg[[i]]$col,
            metric = sig_neg[[i]]$metric,
            value = paste0("<span style='display: block; text-align: right;'>", 
                           sprintf("%.3f", sig_neg[[i]]$value), 
                           "</span>")
          ))
        }
      }
      
      # For non-significant associations
      if (length(non_sig) > 0) {
        for (i in seq_along(non_sig)) {
          non_sig_table$addRow(rowKey = i, values = list(
            rowCat = non_sig[[i]]$row,
            colCat = non_sig[[i]]$col,
            metric = non_sig[[i]]$metric,
            value = paste0("<span style='display: block; text-align: right;'>", 
                           sprintf("%.3f", non_sig[[i]]$value), 
                           "</span>")
          ))
        }
      }
    },
    
    .extractSignificance = function(metric_matrix, metric_label, threshold = 1.96, use_empirical = FALSE) {
      
      I <- nrow(metric_matrix)
      J <- ncol(metric_matrix)
      row_names <- rownames(metric_matrix)
      col_names <- colnames(metric_matrix)
      
      if (!use_empirical && !is.null(threshold) && 
          metric_label %in% c('Std. Residual', 'Mom. Corr. Std. Residual', 'Adj. Std. Residual') &&
          self$options$sidakCorrection) {
        k <- I * J
        alpha <- 0.05
        alpha_corrected <- 1 - (1 - alpha)^(1/k)
        threshold <- qnorm(1 - alpha_corrected/2)
      }
      
      results <- list()
      
      for (i in 1:I) {
        for (j in 1:J) {
          value <- metric_matrix[i, j]
          
          if (is.null(threshold)) {
            sig <- FALSE
            dir <- if (value > 0) 'positive' else 'negative'
          } else if (use_empirical) {
            positive_threshold <- threshold[1]
            negative_threshold <- threshold[2]
            
            if (value > positive_threshold) {
              sig <- TRUE
              dir <- 'positive'
            } else if (value < negative_threshold) {
              sig <- TRUE
              dir <- 'negative'
            } else {
              sig <- FALSE
              dir <- if (value > 0) 'positive' else 'negative'
            }
          } else {
            sig <- abs(value) > threshold
            dir <- if (value > 0) 'positive' else 'negative'
          }
          
          results[[length(results) + 1]] <- list(
            row = row_names[i],
            col = col_names[j],
            metric = metric_label,
            value = value,
            significant = sig,
            direction = dir
          )
        }
      }
      
      return(results)
    },
    .extractSignificanceFourthSpread = function(metric_matrix, metric_label) {
      
      I <- nrow(metric_matrix)
      J <- ncol(metric_matrix)
      row_names <- rownames(metric_matrix)
      col_names <- colnames(metric_matrix)
      
      # Compute fourth-spread thresholds
      all_values <- as.vector(metric_matrix)
      F_U <- quantile(all_values, 0.75)
      F_L <- quantile(all_values, 0.25)
      d_F <- F_U - F_L
      
      lower_cutoff <- F_L - 1.5 * d_F
      upper_cutoff <- F_U + 1.5 * d_F
      
      results <- list()
      
      for (i in 1:I) {
        for (j in 1:J) {
          value <- metric_matrix[i, j]
          
          if (value > upper_cutoff) {
            sig <- TRUE
            dir <- 'positive'
          } else if (value < lower_cutoff) {
            sig <- TRUE
            dir <- 'negative'
          } else {
            sig <- FALSE
            dir <- if (value > 0) 'positive' else 'negative'
          }
          
          results[[length(results) + 1]] <- list(
            row = row_names[i],
            col = col_names[j],
            metric = metric_label,
            value = value,
            significant = sig,
            direction = dir
          )
        }
      }
      
      return(results)
    },
    
    .extractPEMSignificance = function(pem_list) {
      
      pem <- pem_list$pem
      ci_lower <- pem_list$ci_lower
      ci_upper <- pem_list$ci_upper
      
      I <- nrow(pem)
      J <- ncol(pem)
      row_names <- rownames(pem)
      col_names <- colnames(pem)
      
      results <- list()
      
      for (i in 1:I) {
        for (j in 1:J) {
          value <- pem[i, j]
          ci_low <- ci_lower[i, j]
          ci_up <- ci_upper[i, j]
          
          sig <- (ci_low > 0) || (ci_up < 0)
          dir <- if (value > 0) 'positive' else 'negative'
          
          results[[length(results) + 1]] <- list(
            row = row_names[i],
            col = col_names[j],
            metric = 'PEM',
            value = value,
            significant = sig,
            direction = dir
          )
        }
      }
      
      return(results)
    },
    
    .extractDEPSignificance = function(dep_list) {
      
      results <- list()
      
      for (result in dep_list) {
        if (!is.na(result$dep)) {
          dir <- if (result$dep > 0) 'positive' else 'negative'
          
          results[[length(results) + 1]] <- list(
            row = result$row,
            col = result$outcome,
            metric = 'DEP',
            value = result$dep,
            significant = result$significant,
            direction = dir
          )
        }
      }
      
      return(results)
    },
    
    .extractBSOutlierSignificance = function(bsoutlier_list) {
      
      del_res_matrix <- bsoutlier_list$del_res_matrix
      detection_matrix <- bsoutlier_list$detection_matrix
      
      row_names <- rownames(del_res_matrix)
      col_names <- colnames(del_res_matrix)
      
      I <- nrow(del_res_matrix)
      J <- ncol(del_res_matrix)
      
      results <- list()
      
      for (i in 1:I) {
        for (j in 1:J) {
          value <- del_res_matrix[i, j]
          
          if (!is.na(value)) {
            is_detected <- detection_matrix[i, j]
            dir <- if (value > 0) 'positive' else 'negative'
            
            results[[length(results) + 1]] <- list(
              row = row_names[i],
              col = col_names[j],
              metric = 'BS Outlier',
              value = value,
              significant = is_detected,
              direction = dir
            )
          }
        }
      }
      
      return(results)
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
          
          <p>The ±1.96 threshold corresponds to a 95% confidence level when examining a single cell in isolation. However, when inspecting multiple cells simultaneously—as is typical in contingency table analysis—the probability of observing at least one false positive increases. The Šidák correction addresses this multiple comparison problem by adjusting the threshold to maintain an overall Type I error rate of 0.05 across all cells. The corrected threshold is calculated as Φ<sup>−1</sup>(1 − α/(2<i>k</i>)), where <i>k</i> is the number of cells and Φ<sup>−1</sup> is the inverse standard normal cumulative distribution function.</p>
          
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
          
          <p><strong>Setting <i>k</i><sub>max</sub>:</strong> The parameter <i>k</i><sub>max</sub> determines the maximum number of potential outliers to examine. Simonoff (1988) suggests this might be 20%-30% of cells, though context should guide the choice. Setting <i>k</i><sub>max</sub> too low risks missing outliers; setting it too high increases computation but does not inflate the false positive rate (the Bonferroni correction accounts for all <i>RC</i> cells regardless of <i>k</i><sub>max</sub>).</p>
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
    
    <p><strong>Forest plot visualisation:</strong> The optional PEM forest plot provides a ranked visual summary of all cell-level associations. Each cell is represented by a point (the PEM estimate) with horizontal whiskers showing the bootstrap confidence interval (Lefèvre & Champely 2009). Positive values (attraction) appear in gold/amber, negative values (repulsion) in maroon/red. Saturated colours indicate statistically significant associations (confidence interval excludes zero), whilst desaturated colours indicate non-significant associations. The vertical line at zero represents independence. This visualisation allows rapid identification of the strongest and most reliable patterns of attraction and repulsion, sorted by effect magnitude. Note that because PEM is bounded (−100% to +100%) and its sampling distribution is typically skewed, bootstrap confidence intervals are often asymmetric around the point estimate; this is expected behaviour consistent with the percentile bootstrap method (Efron & Tibshirani 1993; Lefèvre & Champely 2009).</p>
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
          <p>The Dependence Evaluator Proportion (DEP) measures, for each row category, whether the probability of a designated outcome (as opposed to other outcomes) is higher or lower than in all other rows combined. Unlike symmetric post-hoc measures that treat all cells equally, DEP explicitly models the column variable as the <strong>dependent variable</strong> and requires selecting one column category as the <strong>outcome of interest</strong>.</p>
          
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
          
          <p><strong>Forest plot visualisation:</strong> The optional DEP forest plot displays each row category's DEP value as a point. Because significance bounds vary across rows (depending on each row's marginal totals), each row shows its own acceptance region as a horizontal grey segment. Points falling outside their row's acceptance region represent statistically significant associations. Positive values (attraction) appear in gold/amber, negative values (repulsion) in maroon/red. Saturated colours indicate significance; desaturated colours indicate non-significance. The vertical line at zero represents independence.</p>
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
        # Display message if no data after filtering
        plot.new()
        text(0.5, 0.5, "No cells match the current filter.", cex = 1.2, col = "grey50")
        return(TRUE)
      }
      
      # Convert label to factor with current order (already sorted by pem)
      plot_data$label <- factor(plot_data$label, levels = plot_data$label)
      
      # Define colours: maroon for negative, goldenrod for positive
      # Desaturated versions for non-significant cells
      col_neg_sig <- "#8B1A1A"
      col_pos_sig <- "#DAA520"
      col_neg_nonsig <- "#C4A0A0"
      col_pos_nonsig <- "#D4C890"  # Slightly darker for visibility
      
      point_cols <- ifelse(
        plot_data$pem < 0,
        ifelse(plot_data$significant, col_neg_sig, col_neg_nonsig),
        ifelse(plot_data$significant, col_pos_sig, col_pos_nonsig)
      )
      
      # Calculate margins for long labels
      max_label_chars <- max(nchar(as.character(plot_data$label)))
      left_margin <- max(8, max_label_chars * 0.45)
      
      # Set up plot
      par(
        mar = c(4, left_margin, 3, 2) + 0.1,
        family = "sans"
      )
      
      n_points <- nrow(plot_data)
      
      # Create empty plot
      plot(
        x = NULL, y = NULL,
        xlim = c(-110, 110),
        ylim = c(0.5, n_points + 0.5),
        xlab = "",
        ylab = "",
        axes = FALSE,
        frame.plot = FALSE
      )
      
      # Add vertical reference line at zero
      abline(v = 0, lty = 1, col = "#808080", lwd = 1.5)
      
      # Add light horizontal grid lines for readability
      abline(h = seq_len(n_points), col = "#E8E8E8", lwd = 0.5)
      
      # Y positions
      y_pos <- seq_len(n_points)
      
      # Draw confidence interval whiskers
      segments(
        x0 = plot_data$lower,
        x1 = plot_data$upper,
        y0 = y_pos,
        y1 = y_pos,
        col = "#505050",
        lwd = 1.3
      )
      
      # Draw whisker caps
      segments(
        x0 = plot_data$lower,
        x1 = plot_data$lower,
        y0 = y_pos - 0.08,
        y1 = y_pos + 0.08,
        col = "#505050",
        lwd = 1.3
      )
      segments(
        x0 = plot_data$upper,
        x1 = plot_data$upper,
        y0 = y_pos - 0.08,
        y1 = y_pos + 0.08,
        col = "#505050",
        lwd = 1.3
      )
      
      # Draw point estimates as filled circles
      points(
        x = plot_data$pem,
        y = y_pos,
        pch = 19,
        col = point_cols,
        cex = 1.4
      )
      
      # Add axes
      axis(1, at = seq(-100, 100, by = 50), col = "#808080", col.axis = "#505050")
      axis(2, at = y_pos, labels = plot_data$label, las = 1, tick = FALSE,
           col.axis = "#505050", cex.axis = 0.85)
      
      # Add labels
      mtext("PEM (%)", side = 1, line = 2.5, col = "#505050")
      mtext("PEM Scores with Bootstrap Confidence Intervals", side = 3, line = 1,
            font = 2, cex = 1.1, col = "#303030")
      
      # Build legend dynamically based on what categories are present in the data
      has_neg_sig <- any(plot_data$pem < 0 & plot_data$significant)
      has_neg_nonsig <- any(plot_data$pem < 0 & !plot_data$significant)
      has_pos_sig <- any(plot_data$pem >= 0 & plot_data$significant)
      has_pos_nonsig <- any(plot_data$pem >= 0 & !plot_data$significant)
      
      legend_labels <- c()
      legend_cols <- c()
      
      if (has_neg_sig) {
        legend_labels <- c(legend_labels, "Significant negative")
        legend_cols <- c(legend_cols, col_neg_sig)
      }
      if (has_neg_nonsig) {
        legend_labels <- c(legend_labels, "Non-significant negative")
        legend_cols <- c(legend_cols, col_neg_nonsig)
      }
      if (has_pos_sig) {
        legend_labels <- c(legend_labels, "Significant positive")
        legend_cols <- c(legend_cols, col_pos_sig)
      }
      if (has_pos_nonsig) {
        legend_labels <- c(legend_labels, "Non-significant positive")
        legend_cols <- c(legend_cols, col_pos_nonsig)
      }
      
      # Only draw legend if there are items to show
      if (length(legend_labels) > 0) {
        legend(
          "bottomright",
          legend = legend_labels,
          pch = 19,
          col = legend_cols,
          bty = "n",
          cex = 0.75,
          text.col = "#505050"
        )
      }
      
      TRUE
    },
    
    # -------------------------------------------------------------------------
    # Render DEP forest plot
    # -------------------------------------------------------------------------
    .depPlot = function(image, ggtheme, theme, ...) {
      
      plot_data <- image$state
      
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for DEP plot.", cex = 1.2, col = "grey50")
        return(TRUE)
      }
      
      # Get outcome category from attribute
      outcome_cat <- attr(plot_data, "outcome")
      if (is.null(outcome_cat)) outcome_cat <- ""
      
      # Convert label to factor with current order (already sorted by dep)
      plot_data$label <- factor(plot_data$label, levels = plot_data$label)
      
      # Define colours: maroon for negative, goldenrod for positive
      col_neg_sig <- "#8B1A1A"
      col_pos_sig <- "#DAA520"
      col_neg_nonsig <- "#C4A0A0"
      col_pos_nonsig <- "#D4C890"
      
      point_cols <- ifelse(
        plot_data$dep < 0,
        ifelse(plot_data$significant, col_neg_sig, col_neg_nonsig),
        ifelse(plot_data$significant, col_pos_sig, col_pos_nonsig)
      )
      
      # Calculate margins for labels
      max_label_chars <- max(nchar(as.character(plot_data$label)))
      left_margin <- max(8, max_label_chars * 0.45)
      
      par(
        mar = c(4, left_margin, 4, 2) + 0.1,
        family = "sans"
      )
      
      n_points <- nrow(plot_data)
      
      # Determine x-axis range (DEP is bounded -1 to +1)
      x_range <- c(-1.1, 1.1)
      
      # Create empty plot
      plot(
        x = NULL, y = NULL,
        xlim = x_range,
        ylim = c(0.5, n_points + 0.5),
        xlab = "",
        ylab = "",
        axes = FALSE,
        frame.plot = FALSE
      )
      
      # Add vertical reference line at zero (independence)
      abline(v = 0, lty = 1, col = "#808080", lwd = 1.5)
      
      # Add light horizontal grid lines
      abline(h = seq_len(n_points), col = "#E8E8E8", lwd = 0.5)
      
      # Draw individual significance bounds for each row as small horizontal segments
      y_pos <- seq_len(n_points)
      for (i in seq_len(n_points)) {
        segments(
          x0 = plot_data$sig_lower[i], y0 = y_pos[i],
          x1 = plot_data$sig_upper[i], y1 = y_pos[i],
          col = "#D0D0D0", lwd = 4, lend = 1
        )
      }
      
      # Draw point estimates
      points(
        x = plot_data$dep,
        y = y_pos,
        pch = 19,
        col = point_cols,
        cex = 1.6
      )
      
      # Add axes
      axis(1, at = seq(-1, 1, by = 0.5), col = "#808080", col.axis = "#505050")
      axis(2, at = y_pos, labels = plot_data$label, las = 1, tick = FALSE,
           col.axis = "#505050", cex.axis = 0.85)
      
      # Add labels
      mtext("DEP", side = 1, line = 2.5, col = "#505050")
      
      # Build title with outcome category
      plot_title <- paste0("DEP: Outcome = ", outcome_cat)
      mtext(plot_title, side = 3, line = 1,
            font = 2, cex = 1.1, col = "#303030")
      
      # Build legend dynamically
      has_neg_sig <- any(plot_data$dep < 0 & plot_data$significant)
      has_neg_nonsig <- any(plot_data$dep < 0 & !plot_data$significant)
      has_pos_sig <- any(plot_data$dep >= 0 & plot_data$significant)
      has_pos_nonsig <- any(plot_data$dep >= 0 & !plot_data$significant)
      
      legend_labels <- c()
      legend_cols <- c()
      legend_pch <- c()
      
      if (has_neg_sig) {
        legend_labels <- c(legend_labels, "Significant negative")
        legend_cols <- c(legend_cols, col_neg_sig)
        legend_pch <- c(legend_pch, 19)
      }
      if (has_neg_nonsig) {
        legend_labels <- c(legend_labels, "Non-significant negative")
        legend_cols <- c(legend_cols, col_neg_nonsig)
        legend_pch <- c(legend_pch, 19)
      }
      if (has_pos_sig) {
        legend_labels <- c(legend_labels, "Significant positive")
        legend_cols <- c(legend_cols, col_pos_sig)
        legend_pch <- c(legend_pch, 19)
      }
      if (has_pos_nonsig) {
        legend_labels <- c(legend_labels, "Non-significant positive")
        legend_cols <- c(legend_cols, col_pos_nonsig)
        legend_pch <- c(legend_pch, 19)
      }
      
      # Add significance bounds to legend
      legend_labels <- c(legend_labels, "Acceptance region")
      legend_cols <- c(legend_cols, "#D0D0D0")
      legend_pch <- c(legend_pch, NA)
      
      if (length(legend_labels) > 0) {
        legend(
          "bottomright",
          legend = legend_labels,
          pch = legend_pch,
          lwd = c(rep(NA, length(legend_pch) - 1), 4),
          col = legend_cols,
          bty = "n",
          cex = 0.75,
          text.col = "#505050"
        )
      }
      
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
    
    # Cache control fields
    .lastDataHash = NULL,
    .lastBootstrapOptions = NULL
  )
)