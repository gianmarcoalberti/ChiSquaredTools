# Simultaneous Reduction of Dimension (SRD) for Sparse Contingency Tables
# Based on Orton, C. and P. Tyers (1991)

#' @export
chisqsrdClass <- R6::R6Class(
  "chisqsrdClass",
  inherit = chisqsrdBase,
  private = list(
    
    # =========================================================================
    # Storage fields for table stability
    # =========================================================================
    .originalRowLevels = NULL,
    .originalColLevels = NULL,
    .nOrigRows = NULL,
    .nOrigCols = NULL,
    
    # =========================================================================
    # .init() - Pre-build table structures to prevent flickering
    # =========================================================================
    .init = function() {
      
      # Pre-populate method info immediately to prevent flickering
      if (self$options$showMethodInfo) {
        private$.populateMethodInfo()
      }
      
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
      
      # Store for later use
      private$.originalRowLevels <- row_levels
      private$.originalColLevels <- col_levels
      private$.nOrigRows <- I
      private$.nOrigCols <- J
      
      # ---------------------------------------------------------------------
      # 1. Original Table - pre-build structure
      # ---------------------------------------------------------------------
      origTable <- self$results$originalTable
      origTable$setTitle(paste0("Original: ", rowVar, " × ", colVar))
      
      origTable$addColumn(name = 'rowName', title = rowVar, type = 'text')
      for (j in seq_len(J)) {
        origTable$addColumn(name = paste0('col', j), title = col_levels[j],
                            type = 'number', superTitle = colVar)
      }
      origTable$addColumn(name = 'rowTotal', title = 'Total', type = 'number')
      
      # Pre-add rows
      for (i in seq_len(I)) {
        origTable$addRow(rowKey = paste0('row', i), 
                         values = list(rowName = row_levels[i]))
      }
      origTable$addRow(rowKey = 'total', values = list(rowName = 'Total'))
      
      # ---------------------------------------------------------------------
      # 2. Summary Table - pre-build rows
      # ---------------------------------------------------------------------
      summaryTable <- self$results$summaryTable
      summaryTable$addRow(rowKey = 'original', values = list(stage = 'Original'))
      summaryTable$addRow(rowKey = 'pruned', values = list(stage = 'After Pruning'))
      summaryTable$addRow(rowKey = 'final', values = list(stage = 'After SRD'))
      
      # ---------------------------------------------------------------------
      # 3. Pruning Table - pre-add placeholder row
      # ---------------------------------------------------------------------
      pruningTable <- self$results$pruningTable
      pruningTable$addRow(rowKey = 'placeholder', values = list(
        dimension = '—',
        category = 'Analysing...',
        weight = NA,
        criticalWeight = NA,
        reason = ''
      ))
      
      # ---------------------------------------------------------------------
      # 4. Row Merge Table - pre-add placeholder row
      # ---------------------------------------------------------------------
      rowMergeTable <- self$results$rowMergeTable
      rowMergeTable$addRow(rowKey = 'placeholder', values = list(
        step = 0,
        item1 = 'Analysing...',
        item2 = '',
        weightedDist = NA,
        df = NA,
        pValue = NA,
        decision = ''
      ))
      
      # ---------------------------------------------------------------------
      # 5. Row Groups Table - pre-add placeholder row
      # ---------------------------------------------------------------------
      rowGroupsTable <- self$results$rowGroupsTable
      rowGroupsTable$addRow(rowKey = 'placeholder', values = list(
        groupLabel = 'Analysing...',
        members = '',
        nMembers = NA
      ))
      
      # ---------------------------------------------------------------------
      # 6. Column Merge Table - pre-add placeholder row
      # ---------------------------------------------------------------------
      colMergeTable <- self$results$colMergeTable
      colMergeTable$addRow(rowKey = 'placeholder', values = list(
        step = 0,
        item1 = 'Analysing...',
        item2 = '',
        weightedDist = NA,
        df = NA,
        pValue = NA,
        decision = ''
      ))
      
      # ---------------------------------------------------------------------
      # 7. Column Groups Table - pre-add placeholder row
      # ---------------------------------------------------------------------
      colGroupsTable <- self$results$colGroupsTable
      colGroupsTable$addRow(rowKey = 'placeholder', values = list(
        groupLabel = 'Analysing...',
        members = '',
        nMembers = NA
      ))
      
      # ---------------------------------------------------------------------
      # 8-10. Dynamic tables (rowReducedTable, colReducedTable, reducedTable)
      # Built in .run() because dimensions depend on analysis results
      # ---------------------------------------------------------------------
    },
    
    # =========================================================================
    # .run() - Main analysis execution
    # =========================================================================
    .run = function() {
      
      if (is.null(self$options$rows) || is.null(self$options$cols)) {
        return()
      }
      
      # Clear footnotes
      self$results$summaryTable$setNote('srdSummary', NULL, init = FALSE)
      self$results$rowGroupsTable$setNote('rowGroupsNote', NULL, init = FALSE)
      self$results$colGroupsTable$setNote('colGroupsNote', NULL, init = FALSE)
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      data <- self$data
      
      # Ensure factors
      if (!is.factor(data[[rowVar]]))
        data[[rowVar]] <- as.factor(data[[rowVar]])
      if (!is.factor(data[[colVar]]))
        data[[colVar]] <- as.factor(data[[colVar]])
      
      # Build contingency table safely (shared helper in utils.R):
      # no formula parsing of variable names, double storage, all levels kept
      contingency_table <- .ctBuildTable(data, c(rowVar, colVar), self$options$counts)
      .ctRejectTooFewLevels(contingency_table, rowVar, colVar)
      .ctRejectEmptyMargins(contingency_table, rowVar, colVar)
      
      # Convert to matrix for processing
      orig_matrix <- as.matrix(contingency_table)
      
      # Store original statistics
      orig_N <- sum(orig_matrix)
      orig_chi <- suppressWarnings(stats::chisq.test(orig_matrix)$statistic)
      orig_nRows <- nrow(orig_matrix)
      orig_nCols <- ncol(orig_matrix)
      orig_df <- (orig_nRows - 1) * (orig_nCols - 1)
      
      # Populate original table
      private$.populateOriginalTable(orig_matrix)
      
      # ---------------------------------------------------------------------
      # STEP 1: PRUNING (if enabled)
      # ---------------------------------------------------------------------
      alpha <- self$options$alpha
      working_matrix <- orig_matrix
      pruning_info <- list(rows = list(), cols = list())
      
      if (self$options$doPruning) {
        prune_result <- private$.pruneMatrix(working_matrix, alpha)
        working_matrix <- prune_result$matrix
        pruning_info <- prune_result$pruning_info
        
        # After pruning, remove any rows/columns that have become all zeros
        # (This can happen when pruned rows were the only ones with values in certain columns)
        row_sums <- rowSums(working_matrix)
        col_sums <- colSums(working_matrix)
        
        # Track additionally removed items due to becoming zero
        if (any(row_sums == 0)) {
          zero_rows <- which(row_sums == 0)
          for (idx in zero_rows) {
            pruning_info$rows[[length(pruning_info$rows) + 1]] <- list(
              category = rownames(working_matrix)[idx],
              weight = 0,
              critical = NA,
              reason = "Zero total after column pruning"
            )
          }
          working_matrix <- working_matrix[row_sums > 0, , drop = FALSE]
        }
        
        if (any(col_sums == 0)) {
          zero_cols <- which(col_sums == 0)
          for (idx in zero_cols) {
            pruning_info$cols[[length(pruning_info$cols) + 1]] <- list(
              category = colnames(working_matrix)[idx],
              weight = 0,
              critical = NA,
              reason = "Zero total after row pruning"
            )
          }
          working_matrix <- working_matrix[, col_sums > 0, drop = FALSE]
        }
        
        # Populate pruning table
        private$.populatePruningTable(pruning_info)
      }
      
      pruned_N <- sum(working_matrix)
      pruned_chi <- if (nrow(working_matrix) > 1 && ncol(working_matrix) > 1) {
        chi_result <- suppressWarnings(stats::chisq.test(working_matrix)$statistic)
        if (is.na(chi_result) || is.nan(chi_result)) NA else chi_result
      } else {
        NA
      }
      pruned_nRows <- nrow(working_matrix)
      pruned_nCols <- ncol(working_matrix)
      pruned_df <- (pruned_nRows - 1) * (pruned_nCols - 1)
      
      # Store matrix after pruning (before any SRD) for intermediate tables
      matrix_after_pruning <- working_matrix
      
      # ---------------------------------------------------------------------
      # STEP 2: SRD - ROW REDUCTION
      # ---------------------------------------------------------------------
      row_merge_history <- NULL
      row_groups <- NULL
      
      if (nrow(working_matrix) > 1) {
        row_result <- private$.performSRD(working_matrix, dim = "row", alpha = alpha)
        working_matrix <- row_result$matrix
        row_merge_history <- row_result$merge_history
        row_groups <- row_result$groups
        
        # Populate row merge table
        private$.populateMergeTable(row_merge_history, self$results$rowMergeTable)
        
        # Populate row groups table
        private$.populateGroupsTable(row_groups, self$results$rowGroupsTable, "row", orig_nRows, alpha)
      } else {
        # Only one row - no merging possible
        private$.populateSingleItemTable(self$results$rowMergeTable, "row")
        row_groups <- list()
        row_groups[[rownames(working_matrix)[1]]] <- rownames(working_matrix)[1]
        private$.populateGroupsTable(row_groups, self$results$rowGroupsTable, "row", orig_nRows, alpha)
      }
      
      # Store matrix after row reduction (for row-grouped table)
      matrix_after_row_reduction <- working_matrix
      
      # Populate Row-Grouped Table (rows merged, original columns)
      private$.populateDynamicTable(matrix_after_row_reduction, 
                                    self$results$rowReducedTable,
                                    paste0("Row-Grouped Table (", nrow(matrix_after_row_reduction), 
                                           " × ", ncol(matrix_after_row_reduction), ")"))
      
      # ---------------------------------------------------------------------
      # STEP 3: SRD - COLUMN REDUCTION (on pruned matrix, NOT row-reduced)
      # ---------------------------------------------------------------------
      col_merge_history <- NULL
      col_groups <- NULL
      matrix_after_col_reduction <- matrix_after_pruning  # Start fresh from pruned matrix
      
      if (ncol(matrix_after_pruning) > 1) {
        col_result <- private$.performSRD(matrix_after_pruning, dim = "col", alpha = alpha)
        matrix_after_col_reduction <- col_result$matrix
        col_merge_history <- col_result$merge_history
        col_groups <- col_result$groups
        
        # Populate column merge table
        private$.populateMergeTable(col_merge_history, self$results$colMergeTable)
        
        # Populate column groups table
        private$.populateGroupsTable(col_groups, self$results$colGroupsTable, "column", orig_nCols, alpha)
      } else {
        # Only one column - no merging possible
        private$.populateSingleItemTable(self$results$colMergeTable, "column")
        col_groups <- list()
        col_groups[[colnames(matrix_after_pruning)[1]]] <- colnames(matrix_after_pruning)[1]
        private$.populateGroupsTable(col_groups, self$results$colGroupsTable, "column", orig_nCols, alpha)
      }
      
      # Populate Column-Grouped Table (original rows, columns merged)
      private$.populateDynamicTable(matrix_after_col_reduction, 
                                    self$results$colReducedTable,
                                    paste0("Column-Grouped Table (", nrow(matrix_after_col_reduction), 
                                           " × ", ncol(matrix_after_col_reduction), ")"))
      
      # ---------------------------------------------------------------------
      # STEP 4: FINAL COMBINED TABLE (apply column groupings to row-reduced matrix)
      # ---------------------------------------------------------------------
      final_matrix <- matrix_after_row_reduction
      
      if (length(col_groups) > 0 && length(col_groups) < ncol(matrix_after_row_reduction)) {
        # Aggregate columns according to col_groups
        new_col_names <- names(col_groups)
        n_new_cols <- length(new_col_names)
        
        combined_matrix <- matrix(0, nrow = nrow(final_matrix), ncol = n_new_cols)
        rownames(combined_matrix) <- rownames(final_matrix)
        colnames(combined_matrix) <- new_col_names
        
        for (g in seq_along(col_groups)) {
          group_name <- new_col_names[g]
          member_cols <- col_groups[[g]]
          # Sum the columns that belong to this group
          if (length(member_cols) == 1) {
            combined_matrix[, g] <- final_matrix[, member_cols, drop = TRUE]
          } else {
            combined_matrix[, g] <- rowSums(final_matrix[, member_cols, drop = FALSE])
          }
        }
        final_matrix <- combined_matrix
      }
      
      # Final statistics
      final_N <- sum(final_matrix)
      final_chi <- if (nrow(final_matrix) > 1 && ncol(final_matrix) > 1) {
        chi_result <- suppressWarnings(stats::chisq.test(final_matrix)$statistic)
        if (is.na(chi_result) || is.nan(chi_result)) NA else chi_result
      } else {
        NA
      }
      final_nRows <- nrow(final_matrix)
      final_nCols <- ncol(final_matrix)
      final_df <- (final_nRows - 1) * (final_nCols - 1)
      
      # ---------------------------------------------------------------------
      # Populate Summary Table
      # ---------------------------------------------------------------------
      private$.populateSummaryTable(
        orig_nRows, orig_nCols, orig_df, orig_N, orig_chi,
        pruned_nRows, pruned_nCols, pruned_df, pruned_N, pruned_chi,
        final_nRows, final_nCols, final_df, final_N, final_chi
      )
      
      # ---------------------------------------------------------------------
      # Populate Final Reduced Table
      # ---------------------------------------------------------------------
      private$.populateDynamicTable(final_matrix, 
                                    self$results$reducedTable,
                                    paste0("Reduced Contingency Table (", final_nRows, 
                                           " × ", final_nCols, ")"))
      
      # ---------------------------------------------------------------------
      # Add interpretation notices
      # ---------------------------------------------------------------------
      private$.addInterpretationNotices(
        orig_nRows, orig_nCols, final_nRows, final_nCols,
        orig_N, final_N, orig_chi, final_chi
      )
    },
    
    # =========================================================================
    # PRUNING FUNCTION
    # Based on Section 19.2.5 of Orton & Tyers (1991)
    # =========================================================================
    .pruneMatrix = function(mat, alpha) {
      # Based on Section 19.2.5 of Orton & Tyers (1991)
      # 
      # A category is "too small" if it could never differ significantly
      # from the mean profile regardless of its actual profile.
      #
      # Formula: x_crit = χ²(α, k-1) / (k-1)
      # where k = number of columns (for row pruning) or rows (for column pruning)
      # and x_crit is in absolute units (counts), not proportions.
      
      pruning_info <- list(rows = list(), cols = list())
      
      k_cols <- ncol(mat)
      k_rows <- nrow(mat)
      
      # Critical weight for ROWS (based on number of columns)
      # x_crit = χ²(α, k-1) / (k-1)  [Equation from Section 19.2.5]
      df_row <- k_cols - 1
      critical_chi_row <- stats::qchisq(1 - alpha, df = df_row)
      critical_weight_row <- critical_chi_row / df_row  # This is in absolute units
      
      # Critical weight for COLUMNS (based on number of rows)
      df_col <- k_rows - 1
      critical_chi_col <- stats::qchisq(1 - alpha, df = df_col)
      critical_weight_col <- critical_chi_col / df_col  # This is in absolute units
      
      # Get row and column totals (absolute counts)
      row_totals <- rowSums(mat)
      col_totals <- colSums(mat)
      
      # Identify rows and columns below their critical weight. They are
      # removed only if at least two rows (columns) remain; otherwise
      # they are listed but kept, and the reason says so
      rows_to_prune <- which(row_totals < critical_weight_row)
      cols_to_prune <- which(col_totals < critical_weight_col)
      prune_rows <- length(rows_to_prune) > 0 && nrow(mat) - length(rows_to_prune) >= 2
      prune_cols <- length(cols_to_prune) > 0 && ncol(mat) - length(cols_to_prune) >= 2
      
      for (idx in rows_to_prune) {
        pruning_info$rows[[length(pruning_info$rows) + 1]] <- list(
          category = rownames(mat)[idx],
          weight = row_totals[idx],
          critical = critical_weight_row,
          reason = if (prune_rows) "Weight below critical threshold" else
            "Weight below critical threshold; kept, because pruning would leave fewer than two rows"
        )
      }
      
      for (idx in cols_to_prune) {
        pruning_info$cols[[length(pruning_info$cols) + 1]] <- list(
          category = colnames(mat)[idx],
          weight = col_totals[idx],
          critical = critical_weight_col,
          reason = if (prune_cols) "Weight below critical threshold" else
            "Weight below critical threshold; kept, because pruning would leave fewer than two columns"
        )
      }
      
      if (prune_rows) mat <- mat[-rows_to_prune, , drop = FALSE]
      if (prune_cols) mat <- mat[, -cols_to_prune, drop = FALSE]
      
      list(matrix = mat, pruning_info = pruning_info)
    },
    
    # =========================================================================
    # SRD ALGORITHM
    # Based on Section 19.2.3 of Orton & Tyers (1991)
    # =========================================================================
    .performSRD = function(mat, dim = "row", alpha = 0.05) {
      
      # Transpose if working on columns
      if (dim == "col") {
        mat <- t(mat)
      }
      
      n_items <- nrow(mat)
      current_labels <- rownames(mat)
      if (is.null(current_labels)) {
        current_labels <- paste0("R", seq_len(n_items))
        rownames(mat) <- current_labels
      }
      
      merge_history <- data.frame(
        step = integer(),
        item1 = character(),
        item2 = character(),
        weightedDist = numeric(),
        df = integer(),
        pValue = numeric(),
        decision = character(),
        stringsAsFactors = FALSE
      )
      
      # Track group membership
      group_membership <- as.list(current_labels)
      names(group_membership) <- current_labels
      
      step <- 0
      
      # Iterative merging
      while (nrow(mat) > 1) {
        
        # Calculate all pairwise weighted chi-squared distances
        n_current <- nrow(mat)
        best_pair <- NULL
        best_weighted_dist <- Inf
        best_p_value <- 0
        best_df <- 1
        
        # Number of non-zero columns for df calculation
        n_cols <- ncol(mat)
        
        for (i in 1:(n_current - 1)) {
          for (j in (i + 1):n_current) {
            
            # Get row totals
            weight_i <- sum(mat[i, ])
            weight_j <- sum(mat[j, ])
            
            # Skip if either row has zero total (can't calculate profile)
            if (weight_i == 0 || weight_j == 0) next
            
            # Calculate chi-squared distance between profiles (Equation 19.1)
            profile_i <- mat[i, ] / weight_i
            profile_j <- mat[j, ] / weight_j
            col_masses <- colSums(mat) / sum(mat)
            
            # Avoid division by zero - need valid columns
            valid_cols <- col_masses > 0
            if (sum(valid_cols) < 2) next
            
            # Calculate squared distance
            d_squared <- sum(((profile_i[valid_cols] - profile_j[valid_cols])^2) / 
                               col_masses[valid_cols])
            
            # Skip if distance is NA or NaN
            if (is.na(d_squared) || is.nan(d_squared)) next
            
            # Calculate weighted distance (Equation 19.2)
            weighted_dist <- d_squared * (weight_i * weight_j) / (weight_i + weight_j)
            
            # Skip if weighted distance is NA or NaN
            if (is.na(weighted_dist) || is.nan(weighted_dist)) next
            
            # Track minimum
            if (weighted_dist < best_weighted_dist) {
              best_weighted_dist <- weighted_dist
              best_pair <- c(i, j)
              
              # Degrees of freedom: number of non-zero columns minus 1
              best_df <- sum(valid_cols) - 1
              best_p_value <- 1 - stats::pchisq(weighted_dist, df = best_df)
            }
          }
        }
        
        if (is.null(best_pair)) break
        
        step <- step + 1
        
        # Decision: merge only if NOT significantly different (p > alpha)
        if (best_p_value > alpha) {
          # MERGE
          idx1 <- best_pair[1]
          idx2 <- best_pair[2]
          
          label1 <- rownames(mat)[idx1]
          label2 <- rownames(mat)[idx2]
          
          # Create new merged row
          merged_row <- mat[idx1, , drop = FALSE] + mat[idx2, , drop = FALSE]
          new_label <- paste0(label1, "+", label2)
          rownames(merged_row) <- new_label
          
          # Update group membership
          members1 <- if (label1 %in% names(group_membership)) {
            group_membership[[label1]]
          } else {
            unlist(strsplit(label1, "\\+"))
          }
          members2 <- if (label2 %in% names(group_membership)) {
            group_membership[[label2]]
          } else {
            unlist(strsplit(label2, "\\+"))
          }
          group_membership[[new_label]] <- c(members1, members2)
          
          # Remove old rows and add merged
          mat <- mat[-c(idx1, idx2), , drop = FALSE]
          mat <- rbind(mat, merged_row)
          
          # Record merge
          merge_history <- rbind(merge_history, data.frame(
            step = step,
            item1 = label1,
            item2 = label2,
            weightedDist = best_weighted_dist,
            df = best_df,
            pValue = best_p_value,
            decision = "Merged",
            stringsAsFactors = FALSE
          ))
          
        } else {
          # STOP - the closest pair is significantly different
          # Record the failed merge attempt
          idx1 <- best_pair[1]
          idx2 <- best_pair[2]
          label1 <- rownames(mat)[idx1]
          label2 <- rownames(mat)[idx2]
          
          merge_history <- rbind(merge_history, data.frame(
            step = step,
            item1 = label1,
            item2 = label2,
            weightedDist = best_weighted_dist,
            df = best_df,
            pValue = best_p_value,
            decision = "Stopped (p < α)",
            stringsAsFactors = FALSE
          ))
          
          break
        }
      }
      
      # Build final groups
      final_labels <- rownames(mat)
      groups <- list()
      for (lab in final_labels) {
        if (lab %in% names(group_membership)) {
          groups[[lab]] <- group_membership[[lab]]
        } else {
          groups[[lab]] <- lab
        }
      }
      
      # Transpose back if we were working on columns
      if (dim == "col") {
        mat <- t(mat)
      }
      
      list(
        matrix = mat,
        merge_history = merge_history,
        groups = groups
      )
    },
    
    # =========================================================================
    # TABLE POPULATION FUNCTIONS
    # =========================================================================
    
    .populateOriginalTable = function(mat) {
      
      origTable <- self$results$originalTable
      I <- nrow(mat)
      J <- ncol(mat)
      
      # Fill rows using setRow
      for (i in seq_len(I)) {
        row_values <- list()
        for (j in seq_len(J)) {
          row_values[[paste0('col', j)]] <- mat[i, j]
        }
        row_values[['rowTotal']] <- sum(mat[i, ])
        origTable$setRow(rowKey = paste0('row', i), values = row_values)
      }
      
      # Total row
      total_values <- list()
      for (j in seq_len(J)) {
        total_values[[paste0('col', j)]] <- sum(mat[, j])
      }
      total_values[['rowTotal']] <- sum(mat)
      origTable$setRow(rowKey = 'total', values = total_values)
    },
    
    .populateSummaryTable = function(orig_nRows, orig_nCols, orig_df, orig_N, orig_chi,
                                     pruned_nRows, pruned_nCols, pruned_df, pruned_N, pruned_chi,
                                     final_nRows, final_nCols, final_df, final_N, final_chi) {
      
      summaryTable <- self$results$summaryTable
      
      # Original
      summaryTable$setRow(rowKey = 'original', values = list(
        nRows = orig_nRows,
        nCols = orig_nCols,
        df = orig_df,
        totalN = orig_N,
        nPercent = 100,
        chiSquare = orig_chi,
        chiPercent = 100
      ))
      
      # Pruned
      pruned_chi_pct <- if (!is.na(orig_chi) && orig_chi > 0 && !is.na(pruned_chi)) {
        (pruned_chi / orig_chi) * 100
      } else {
        NA
      }
      summaryTable$setRow(rowKey = 'pruned', values = list(
        nRows = pruned_nRows,
        nCols = pruned_nCols,
        df = pruned_df,
        totalN = pruned_N,
        nPercent = (pruned_N / orig_N) * 100,
        chiSquare = if (is.na(pruned_chi)) NaN else pruned_chi,
        chiPercent = if (is.na(pruned_chi_pct)) NaN else pruned_chi_pct
      ))
      
      # Final
      final_chi_pct <- if (!is.na(orig_chi) && orig_chi > 0 && !is.na(final_chi)) {
        (final_chi / orig_chi) * 100
      } else {
        NA
      }
      summaryTable$setRow(rowKey = 'final', values = list(
        nRows = final_nRows,
        nCols = final_nCols,
        df = final_df,
        totalN = final_N,
        nPercent = (final_N / orig_N) * 100,
        chiSquare = if (is.na(final_chi)) NaN else final_chi,
        chiPercent = if (is.na(final_chi_pct)) NaN else final_chi_pct
      ))
    },
    
    .populatePruningTable = function(pruning_info) {
      
      pruningTable <- self$results$pruningTable
      pruningTable$deleteRows()
      
      row_key <- 1
      
      # Rows pruned
      for (info in pruning_info$rows) {
        pruningTable$addRow(rowKey = row_key, values = list(
          dimension = "Row",
          category = info$category,
          weight = info$weight,
          criticalWeight = if (is.na(info$critical)) NA else info$critical,
          reason = info$reason
        ))
        row_key <- row_key + 1
      }
      
      # Columns pruned
      for (info in pruning_info$cols) {
        pruningTable$addRow(rowKey = row_key, values = list(
          dimension = "Column",
          category = info$category,
          weight = info$weight,
          criticalWeight = if (is.na(info$critical)) NA else info$critical,
          reason = info$reason
        ))
        row_key <- row_key + 1
      }
      
      if (row_key == 1) {
        # No pruning occurred
        pruningTable$addRow(rowKey = 1, values = list(
          dimension = "—",
          category = "No categories pruned",
          weight = NA,
          criticalWeight = NA,
          reason = "All categories above critical weight"
        ))
      }
    },
    
    .populateMergeTable = function(merge_history, table) {
      
      table$deleteRows()
      
      if (nrow(merge_history) == 0) {
        table$addRow(rowKey = 1, values = list(
          step = 0,
          item1 = "—",
          item2 = "—",
          weightedDist = NA,
          df = NA,
          pValue = NA,
          decision = "No merges possible"
        ))
        return()
      }
      
      for (i in seq_len(nrow(merge_history))) {
        table$addRow(rowKey = i, values = list(
          step = merge_history$step[i],
          item1 = merge_history$item1[i],
          item2 = merge_history$item2[i],
          weightedDist = merge_history$weightedDist[i],
          df = merge_history$df[i],
          pValue = merge_history$pValue[i],
          decision = merge_history$decision[i]
        ))
      }
    },
    
    .populateSingleItemTable = function(table, dim_name) {
      table$deleteRows()
      table$addRow(rowKey = 1, values = list(
        step = 0,
        item1 = "—",
        item2 = "—",
        weightedDist = NA,
        df = NA,
        pValue = NA,
        decision = paste0("Only one ", dim_name, " — no merging possible")
      ))
    },
    
    .populateGroupsTable = function(groups, table, dim_type, orig_count, alpha) {
      
      table$deleteRows()
      
      for (i in seq_along(groups)) {
        label <- names(groups)[i]
        members <- groups[[i]]
        
        table$addRow(rowKey = i, values = list(
          groupLabel = label,
          members = paste(members, collapse = ", "),
          nMembers = length(members)
        ))
      }
      
      # Add note
      final_count <- length(groups)
      if (final_count < orig_count) {
        table$setNote('groupNote',
                      paste0(orig_count, " original ", dim_type, "s merged into ", final_count, 
                             " groups. Categories within each group have statistically ",
                             "indistinguishable profiles (p > ", alpha, ")."),
                      init = FALSE)
      } else {
        table$setNote('groupNote',
                      paste0("No ", dim_type, "s could be merged—all have significantly different profiles."),
                      init = FALSE)
      }
    },
    
    .populateDynamicTable = function(mat, table, title) {
      
      # Set title
      table$setTitle(title)
      
      # Clear and rebuild columns
      row_labels <- rownames(mat)
      col_labels <- colnames(mat)
      I <- nrow(mat)
      J <- ncol(mat)
      
      if (is.null(row_labels)) row_labels <- paste0("R", seq_len(I))
      if (is.null(col_labels)) col_labels <- paste0("C", seq_len(J))
      
      # Add columns dynamically
      table$addColumn(name = 'rowName', title = 'Row', type = 'text')
      for (j in seq_len(J)) {
        table$addColumn(name = paste0('col', j), title = col_labels[j],
                        type = 'integer')
      }
      table$addColumn(name = 'rowTotal', title = 'Total', type = 'integer')
      
      # Add data rows
      for (i in seq_len(I)) {
        row_values <- list(rowName = row_labels[i])
        for (j in seq_len(J)) {
          row_values[[paste0('col', j)]] <- mat[i, j]
        }
        row_values[['rowTotal']] <- sum(mat[i, ])
        table$addRow(rowKey = paste0('row', i), values = row_values)
      }
      
      # Total row
      total_values <- list(rowName = 'Total')
      for (j in seq_len(J)) {
        total_values[[paste0('col', j)]] <- sum(mat[, j])
      }
      total_values[['rowTotal']] <- sum(mat)
      table$addRow(rowKey = 'total', values = total_values)
    },
    
    .addInterpretationNotices = function(orig_nRows, orig_nCols, final_nRows, final_nCols,
                                         orig_N, final_N, orig_chi, final_chi) {
      
      alpha <- self$options$alpha
      
      # Calculate reductions
      row_reduction <- orig_nRows - final_nRows
      col_reduction <- orig_nCols - final_nCols
      n_retained_pct <- (final_N / orig_N) * 100
      
      # Handle potential NA in final_chi
      chi_retained_pct <- if (!is.na(orig_chi) && orig_chi > 0 && !is.na(final_chi)) {
        (final_chi / orig_chi) * 100
      } else {
        NA
      }
      
      # Summary interpretation
      if (row_reduction == 0 && col_reduction == 0) {
        interpretation <- paste0(
          "No reduction was possible at α = ", alpha, ". ",
          "All rows and columns are statistically distinguishable from each other."
        )
      } else {
        if (!is.na(chi_retained_pct)) {
          interpretation <- paste0(
            "SRD reduced the table from ", orig_nRows, "×", orig_nCols, 
            " to ", final_nRows, "×", final_nCols, " at α = ", alpha, ". ",
            sprintf("%.1f%% of the original data and %.1f%% of the chi-squared statistic were retained.",
                    n_retained_pct, chi_retained_pct)
          )
        } else {
          interpretation <- paste0(
            "SRD reduced the table from ", orig_nRows, "×", orig_nCols, 
            " to ", final_nRows, "×", final_nCols, " at α = ", alpha, ". ",
            sprintf("%.1f%% of the original data was retained.", n_retained_pct)
          )
        }
      }
      
      self$results$summaryTable$setNote('srdSummary', interpretation, init = FALSE)
    },
    
    # =========================================================================
    # METHOD INFORMATION
    # =========================================================================
    .populateMethodInfo = function() {
      
      alpha <- self$options$alpha
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.95em;'>"
      html <- paste0(html, "<h3 style='color: #3E6DA6;'>Simultaneous Reduction of Dimension (SRD)</h3>")
      
      # Overview
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>Overview</h4>",
                     "<p style='margin-bottom: 12px;'>SRD is a technique for simplifying sparse contingency tables by systematically ",
                     "merging rows and/or columns that are statistically indistinguishable. Unlike ",
                     "conventional hierarchical clustering which merges greedily and cuts the tree ",
                     "post-hoc, SRD performs a significance test at each potential merge and only ",
                     "combines categories when their profiles cannot be distinguished at the specified ",
                     "significance level (α = ", alpha, ").</p>"
      )
      
      # The Problem
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>The Problem SRD Addresses</h4>",
                     "<p style='margin-bottom: 12px;'>Sparse contingency tables—those with many small or zero cell counts—present ",
                     "two difficulties for chi-squared analysis:</p>",
                     "<ul style='margin-bottom: 12px;'>",
                     "<li><strong>Inflated degrees of freedom:</strong> Near-empty cells contribute little ",
                     "to χ² but add degrees of freedom, potentially masking genuine patterns.</li>",
                     "<li><strong>Unreliable statistics:</strong> Small expected values can produce erratic ",
                     "chi-squared values that violate asymptotic assumptions.</li>",
                     "</ul>"
      )
      
      # Chi-squared distance
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>The Chi-Squared Distance</h4>",
                     "<p style='margin-bottom: 12px;'>SRD uses the chi-squared metric from correspondence analysis to measure ",
                     "similarity between row profiles (or column profiles). For two rows <em>i</em> and ",
                     "<em>j</em>, the squared distance is:</p>",
                     "<p style='text-align: center; margin-bottom: 12px;'>",
                     "d²(i,j) = Σ<sub>k</sub> (p<sub>ik</sub> − p<sub>jk</sub>)² / c<sub>k</sub></p>",
                     "<p style='margin-bottom: 12px;'>where p<sub>ik</sub> is row <em>i</em>'s profile value in column <em>k</em>, ",
                     "and c<sub>k</sub> is the column mass. Rows with similar proportional distributions ",
                     "across columns have small distances.</p>"
      )
      
      # Weighted statistic
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>The Weighted Chi-Squared Statistic</h4>",
                     "<p style='margin-bottom: 12px;'>The raw distance is converted to a chi-squared statistic for hypothesis testing:</p>",
                     "<p style='text-align: center; margin-bottom: 12px;'>",
                     "χ² = d² × (n<sub>i</sub> × n<sub>j</sub>) / (n<sub>i</sub> + n<sub>j</sub>)</p>",
                     "<p style='margin-bottom: 12px;'>This weighted distance accounts for row sizes. Under the null hypothesis that ",
                     "rows <em>i</em> and <em>j</em> are samples from the same population, this statistic ",
                     "follows a chi-squared distribution with (k−1) degrees of freedom, where k is the ",
                     "number of columns with non-zero totals.</p>"
      )
      
      # Algorithm
      # Algorithm
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>The Algorithm</h4>",
                     "<ol style='margin-bottom: 12px;'>",
                     "<li><strong>Pruning (optional):</strong> Remove categories whose total weight is so ",
                     "small they could never differ significantly from the mean profile.</li>",
                     "<li><strong>Calculate distances:</strong> Compute weighted chi-squared distances ",
                     "between all pairs of rows (or columns).</li>",
                     "<li><strong>Test the closest pair:</strong> If p > α, the pair cannot be distinguished; ",
                     "merge them. If p ≤ α, the closest pair differs significantly—stop.</li>",
                     "<li><strong>Update and repeat:</strong> After merging, recalculate distances involving ",
                     "the new combined category and return to step 2.</li>",
                     "</ol>"
      )
      
      # Pruning details
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>Pruning (Pre-treatment)</h4>",
                     "<p style='margin-bottom: 12px;'>Pruning removes categories that are too small to ever ",
                     "produce a statistically significant difference from the mean profile, regardless of their ",
                     "actual distribution. This prevents 'meaningless' merges where tiny categories combine with ",
                     "others simply because their low weight makes any distance non-significant.</p>",
                     "<p style='margin-bottom: 12px;'>The critical weight threshold is calculated as:</p>",
                     "<p style='text-align: center; margin-bottom: 12px;'>",
                     "x<sub>crit</sub> = χ²<sub>α,k−1</sub> / (k − 1)</p>",
                     "<p style='margin-bottom: 12px;'>where k is the number of columns (for row pruning) or rows ",
                     "(for column pruning), and χ²<sub>α,k−1</sub> is the critical chi-squared value at the chosen ",
                     "significance level. Categories with total weight below this threshold are removed before SRD begins.</p>",
                     "<p style='margin-bottom: 12px;'>Additionally, if pruning causes any remaining row or column to have ",
                     "a zero total (because all its non-zero values were in pruned categories), that row or column ",
                     "is also removed. Categories are removed only if at least two rows (columns) remain; otherwise ",
                     "they are listed as below the threshold but kept.</p>",
                     "<p style='margin-bottom: 12px;'><em>Note:</em> Pruning can be aggressive with small sample sizes ",
                     "or when data are measured in small units. If important categories are being pruned, consider ",
                     "disabling this option and relying on SRD alone to handle sparse categories through merging.</p>"
      )
      
      # Interpreting results
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>Interpreting Results</h4>",
                     "<p style='margin-bottom: 12px;'><strong>Merge history:</strong> Each step shows the closest pair, their weighted ",
                     "chi-squared distance, degrees of freedom, and p-value. 'Merged' indicates the pair ",
                     "was combined; 'Stopped' indicates the procedure terminated because even the closest ",
                     "remaining pair was significantly different.</p>",
                     "<p style='margin-bottom: 12px;'><strong>Final groups:</strong> Categories within each group have statistically ",
                     "indistinguishable profiles at the chosen α level. The composite labels show which ",
                     "original categories were combined.</p>",
                     "<p style='margin-bottom: 12px;'><strong>Chi-squared retention:</strong> The ratio of final to initial χ² indicates ",
                     "how much of the table's association structure was preserved. High retention (>80%) ",
                     "suggests the reduction removed noise rather than signal.</p>"
      )
      
      # References - formatted like power analysis
      html <- paste0(html,
                     "<h4 style='color: #3E6DA6;'>References</h4>",
                     "<p style='font-size: 0.85em; color: #666; margin-top: 8px;'>",
                     "Greenacre, M. J. (1984). <em>Theory and Applications of Correspondence Analysis</em>. Academic Press.<br>",
                     "Orton, C., & Tyers, P. (1991). A technique for reducing the size of sparse contingency tables. ",
                     "In S. Rahtz & K. Lockyear (Eds.), <em>CAA90: Computer Applications and Quantitative Methods ",
                     "in Archaeology 1990</em> (BAR International Series 565, pp. 121–126). Tempus Reparatum.",
                     "</p>"
      )
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    }
    
  )
)