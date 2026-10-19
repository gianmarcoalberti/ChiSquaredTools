# This file is a generated template, your changes will not be overwritten

#' @export
chisqclusterClass <- R6::R6Class(
  "chisqclusterClass",
  inherit = chisqclusterBase,
  private = list(
    
    # -------------------------------------------------------------------------
    # Initialise: pre-build table structure
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
      # 1. Crosstab Table - pre-build structure
      # ---------------------------------------------------------------------
      crosstab <- self$results$crosstabTable
      crosstab$setTitle(paste0(rowVar, " × ", colVar))
      
      crosstab$addColumn(name = 'rowName', title = rowVar, type = 'text')
      for (j in seq_len(J)) {
        crosstab$addColumn(name = paste0('col', j), title = col_levels[j], 
                           type = 'integer', superTitle = colVar)
      }
      crosstab$addColumn(name = 'rowTotal', title = 'Total', type = 'integer')
      
      # Pre-add rows (including total)
      for (i in seq_len(I)) {
        crosstab$addRow(rowKey = paste0('row', i), values = list(rowName = row_levels[i]))
      }
      crosstab$addRow(rowKey = 'total', values = list(rowName = 'Total'))
      
      # ---------------------------------------------------------------------
      # 2. Row Cluster Table - pre-build rows (step 0 to I-1)
      # ---------------------------------------------------------------------
      rowClusterTable <- self$results$rowClusterTable
      for (step in 0:(I - 1)) {
        rowClusterTable$addRow(rowKey = paste0('step', step), values = list(step = step))
      }
      
      # ---------------------------------------------------------------------
      # 4. Column Cluster Table - pre-build rows (step 0 to J-1)
      # ---------------------------------------------------------------------
      colClusterTable <- self$results$colClusterTable
      for (step in 0:(J - 1)) {
        colClusterTable$addRow(rowKey = paste0('step', step), values = list(step = step))
      }
    },
    
    .run = function() {
      
      if (is.null(self$options$rows) || is.null(self$options$cols)) {
        return()
      }
      
      # === CLEAR ALL FOOTNOTES FIRST ===
      self$results$rowClusterTable$setNote('rowClusterSummary', NULL, init = FALSE)
      self$results$rowGroupsTable$setNote('rowGroupsInterpretation', NULL, init = FALSE)
      self$results$colClusterTable$setNote('colClusterSummary', NULL, init = FALSE)
      self$results$colGroupsTable$setNote('colGroupsInterpretation', NULL, init = FALSE)
      
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
      
      # Perform clustering analysis - row clustering
      tryCatch({
        row_result <- private$.clusterContingency(contingency_table, dim = "row")
        private$.rowClusterResult <- row_result
        private$.populateRowResults(row_result)
        # Save only what the dendrogram draws (needed on export)
        self$results$rowDendro$setState(private$.dendroState(row_result))
      }, error = function(e) {
        self$results$rowClusterTable$setNote('clusterError', 
                                             paste0("Row clustering is not available: ", e$message), init = FALSE)
        # Nothing to draw: hide the dendrogram rather than leave it blank
        self$results$rowDendro$setVisible(FALSE)
      })
      
      # Perform clustering analysis - column clustering
      tryCatch({
        col_result <- private$.clusterContingency(contingency_table, dim = "col")
        private$.colClusterResult <- col_result
        private$.populateColResults(col_result)
        # Save only what the dendrogram draws (needed on export)
        self$results$colDendro$setState(private$.dendroState(col_result))
      }, error = function(e) {
        self$results$colClusterTable$setNote('clusterError', 
                                             paste0("Column clustering is not available: ", e$message), init = FALSE)
        # Nothing to draw: hide the dendrogram rather than leave it blank
        self$results$colDendro$setVisible(FALSE)
      })
      
      # Populate method info and references
      private$.populateMethodInfo()
    },
    
    .populateCrosstab = function(contingency_table) {
      
      crosstab <- self$results$crosstabTable
      
      I <- private$.I
      J <- private$.J
      
      # Fill pre-existing rows using setRow
      for (i in seq_len(I)) {
        row_values <- list()
        for (j in seq_len(J)) {
          row_values[[paste0('col', j)]] <- as.integer(contingency_table[i, j])
        }
        row_values[['rowTotal']] <- sum(contingency_table[i, ])
        
        crosstab$setRow(rowKey = paste0('row', i), values = row_values)
      }
      
      # Total row
      total_values <- list()
      for (j in seq_len(J)) {
        total_values[[paste0('col', j)]] <- sum(contingency_table[, j])
      }
      total_values[['rowTotal']] <- sum(contingency_table)
      
      crosstab$setRow(rowKey = 'total', values = total_values)
    },
    
    .getCriticalChiSquare = function(nrow, ncol) {
      # Pearson & Hartley (1972, Table 51) critical values, alpha = 0.05;
      # single shared copy in utils.R (also used by the CA facility)
      .ctGreenacreCritical(nrow, ncol)
    },
    
    # -------------------------------------------------------------------------
    # Find optimal partition using inertia gain ratio (Husson et al., 2017)
    # -------------------------------------------------------------------------
    .findOptimalPartitionInertia = function(merge_stats, qmin = 2, qmax = NULL) {
      # merge_stats has columns: step, items_merged, groups_remaining, 
      #                          chi_square, reduction, reduction_percent
      # 
      
      # At step k, we have (n - k) clusters remaining, where n = total items.
      # The 'reduction' column gives Δ(q) = chi²_before - chi²_after for each merge.
      #
      # Husson criterion: find q that minimises Δ(q) / Δ(q+1)
      # where q is the number of clusters AFTER the merge.
      #
      # In our merge_stats:
      #   - step 0: n clusters (initial state, reduction = 0)
      #   - step 1: n-1 clusters (first merge, reduction[2])
      #   - step k: n-k clusters (reduction[k+1])
      #
      # To go from q clusters to q-1 clusters is step (n - q).
      # Δ(q) = reduction at step (n - q + 1) in 1-indexed R terms.
      
      n_items <- nrow(merge_stats)  # This equals n (original number of items)
      n <- n_items - 1  # Number of merges possible
      
      if (is.null(qmax)) {
        qmax <- min(10, n)  # Default maximum clusters to consider
      }
      
      # Ensure sensible bounds
      qmin <- max(2, qmin)
      qmax <- min(qmax, n)
      
      
      if (qmax <= qmin) {
        # Not enough range to compute ratios; return qmin
        return(list(
          optimal_clusters = qmin,
          # step that leaves qmin clusters (n_items items, one merge per step)
          optimal_step = max(0, n_items - qmin),
          ratios = NULL
        ))
      }
      
      # Compute ratios for each candidate q in [qmin, qmax]
      # ratio(q) = Δ(q) / Δ(q+1)
      # where Δ(q) is the inertia gain when going from q to q-1 clusters
      
      ratios <- data.frame(
        q = integer(0),
        delta_q = numeric(0),
        delta_q_plus_1 = numeric(0),
        ratio = numeric(0)
      )
      
      for (q in qmax:(qmin + 1)) {
        # Step to go from q clusters to q-1: step = n - q + 1 (but step is 0-indexed in merge_stats)
        # In R 1-indexed: row index for this step is (n - q + 2)
        step_q <- n - q + 1
        step_q_plus_1 <- n - (q + 1) + 1  # = n - q
        
        # Reduction values (row indices are step + 1 due to step 0)
        delta_q <- merge_stats$reduction[step_q + 1]
        delta_q_plus_1 <- merge_stats$reduction[step_q_plus_1 + 1]
        
        # Avoid division by zero
        if (delta_q_plus_1 > 0) {
          r <- delta_q / delta_q_plus_1
        } else {
          r <- Inf
        }
        
        ratios <- rbind(ratios, data.frame(
          q = q,
          delta_q = delta_q,
          delta_q_plus_1 = delta_q_plus_1,
          ratio = r
        ))
      }
      
      # Find q that minimises the ratio (largest relative jump)
      if (nrow(ratios) == 0 || all(is.infinite(ratios$ratio))) {
        optimal_q <- qmin
      } else {
        optimal_q <- ratios$q[which.min(ratios$ratio)]
      }
      
      # Convert optimal_q to step number
      optimal_step <- n_items - optimal_q
      
      # Verify: number of clusters at optimal_step
      actual_clusters <- n_items - optimal_step
      
      list(
        optimal_clusters = actual_clusters,
        optimal_step = optimal_step,
        ratios = ratios
      )
    },
    
    .clusterContingency = function(crosstab, dim = "row") {
      
      if (!is.data.frame(crosstab)) {
        crosstab <- as.data.frame.matrix(crosstab)
      }
      
      orig_crosstab <- crosstab
      
      if (dim == "col") {
        crosstab <- as.data.frame(t(crosstab))
      }
      
      crosstab <- as.data.frame(sapply(crosstab, as.numeric))
      orig_labels <- if (dim == "col") colnames(orig_crosstab) else rownames(orig_crosstab)
      rownames(crosstab) <- orig_labels
      
      # Calculate initial chi-square on ORIGINAL table
      chi_init <- suppressWarnings(stats::chisq.test(as.matrix(orig_crosstab))$statistic)
      
      # Get critical value using ORIGINAL table dimensions
      # Only required for Greenacre method; for Husson method, compute if possible but don't fail
      table_dims <- dim(orig_crosstab)
      partition_method <- self$options$partitionMethod
      
      critical_value <- tryCatch({
        private$.getCriticalChiSquare(table_dims[1], table_dims[2])
      }, error = function(e) {
        if (partition_method == "greenacre") {
          stop(e$message)  # Re-throw error for Greenacre method
        } else {
          NA  # For Husson method, proceed without critical value
        }
      })
      
      n <- nrow(crosstab)
      merge_stats <- data.frame(
        step = 0:(n-1),
        items_merged = character(n),
        groups_remaining = character(n),
        chi_square = numeric(n),
        reduction = numeric(n),
        reduction_percent = numeric(n),
        stringsAsFactors = FALSE
      )
      
      # Step 0: initial state
      merge_stats$items_merged[1] <- "Initial"
      merge_stats$groups_remaining[1] <- paste(orig_labels, collapse = ", ")
      merge_stats$chi_square[1] <- chi_init
      merge_stats$reduction[1] <- 0
      merge_stats$reduction_percent[1] <- 0
      
      # Track cluster membership
      cluster_membership <- list()
      for (label in orig_labels) {
        cluster_membership[[label]] <- label
      }
      
      current_data <- crosstab
      current_labels <- orig_labels
      prev_chi <- chi_init
      
      # Store active clusters at each step
      active_clusters_history <- list(orig_labels)
      
      # NEW: Track merges for hclust construction
      # Each merge will record which two items/clusters were joined
      merge_record <- data.frame(
        item1 = character(n-1),
        item2 = character(n-1),
        stringsAsFactors = FALSE
      )
      
      # Ward clustering
      for (i in 1:(n-1)) {
        min_reduction <- Inf
        best_pair <- c(NA, NA)
        best_merged_data <- NULL
        best_merged_labels <- NULL
        best_new_chi <- NULL
        
        # Try all possible pairs
        pairs <- utils::combn(1:nrow(current_data), 2)
        for (j in 1:ncol(pairs)) {
          idx1 <- pairs[1, j]
          idx2 <- pairs[2, j]
          
          # Merge the pair
          merged <- colSums(current_data[c(idx1, idx2), , drop = FALSE])
          temp_data <- current_data[-c(idx1, idx2), , drop = FALSE]
          temp_data <- rbind(temp_data, merged)
          
          merged_labels <- c(current_labels[-c(idx1, idx2)],
                             paste0("(", current_labels[idx1], " + ", current_labels[idx2], ")"))
          rownames(temp_data) <- merged_labels
          
          # Calculate chi-square
          new_chi <- if (nrow(temp_data) > 1) {
            suppressWarnings(stats::chisq.test(as.matrix(temp_data))$statistic)
          } else {
            0
          }
          
          reduction <- prev_chi - new_chi
          
          # Keep pair that minimizes reduction (Ward criterion)
          if (reduction < min_reduction) {
            min_reduction <- reduction
            best_pair <- c(idx1, idx2)
            best_merged_data <- temp_data
            best_merged_labels <- merged_labels
            best_new_chi <- new_chi
          }
        }
        
        # Update cluster membership tracking
        label1 <- current_labels[best_pair[1]]
        label2 <- current_labels[best_pair[2]]
        new_label <- paste0("(", label1, " + ", label2, ")")
        
        # RECORD THE MERGE (this is crucial for hclust construction)
        merge_record$item1[i] <- label1
        merge_record$item2[i] <- label2
        
        # Combine memberships
        members1 <- if (label1 %in% names(cluster_membership)) {
          cluster_membership[[label1]]
        } else {
          strsplit(gsub("[()]", "", label1), " \\+ ")[[1]]
        }
        
        members2 <- if (label2 %in% names(cluster_membership)) {
          cluster_membership[[label2]]
        } else {
          strsplit(gsub("[()]", "", label2), " \\+ ")[[1]]
        }
        
        cluster_membership[[new_label]] <- c(members1, members2)
        
        # Record merge statistics
        merge_stats$items_merged[i+1] <- paste(label1, "+", label2)
        merge_stats$groups_remaining[i+1] <- paste(best_merged_labels, collapse = ", ")
        merge_stats$chi_square[i+1] <- best_new_chi
        merge_stats$reduction[i+1] <- min_reduction
        merge_stats$reduction_percent[i+1] <- (min_reduction / chi_init) * 100
        
        # Update for next iteration
        current_data <- best_merged_data
        current_labels <- best_merged_labels
        prev_chi <- best_new_chi
        
        # Store active clusters at this step
        active_clusters_history[[i+1]] <- current_labels
      }
      
      # -----------------------------------------------------------------------
      # Determine optimal partition based on selected method
      # -----------------------------------------------------------------------
      partition_method <- self$options$partitionMethod
      
      if (partition_method == "inertia") {
        # Husson et al. (2017) inertia gain ratio method
        inertia_result <- private$.findOptimalPartitionInertia(merge_stats)
        sig_step <- inertia_result$optimal_step
        optimal_clusters <- inertia_result$optimal_clusters
        
        # For inertia method, we don't use critical_value for partition selection
        # but we still compute it for reference/comparison
        method_info <- list(
          method = "inertia",
          optimal_clusters = optimal_clusters,
          ratios = inertia_result$ratios
        )
      } else {
        # Greenacre (2017) critical chi-square method (default)
        sig_step <- 0
        
        # Start from step 1 (first merge) and check each reduction
        for (i in 2:nrow(merge_stats)) {
          # Check if this merge's reduction is below the critical threshold
          if (merge_stats$reduction[i] < critical_value) {
            # This merge is acceptable (small reduction = similar categories)
            sig_step <- merge_stats$step[i]
          } else {
            # This merge exceeds the threshold - stop before it
            break
          }
        }
        
        method_info <- list(
          method = "greenacre",
          critical_value = critical_value
        )
      }
      
      # Get active clusters at significant step
      active_clusters <- active_clusters_history[[sig_step + 1]]
      
      # Build membership list for only active clusters
      sig_membership <- list()
      for (cluster_label in active_clusters) {
        if (cluster_label %in% names(cluster_membership)) {
          sig_membership[[cluster_label]] <- cluster_membership[[cluster_label]]
        } else {
          sig_membership[[cluster_label]] <- cluster_label
        }
      }
      
      # Build hclust object components
      n_items <- length(orig_labels)
      
      if (n_items < 2) {
        return(list(
          initial_chi_square = chi_init,
          critical_value = critical_value,
          merge_statistics = merge_stats,
          significant_step = sig_step,
          significant_membership = sig_membership,
          initial_table = orig_crosstab,
          initial_labels = orig_labels,
          merge_matrix = NULL,
          height_vector = NULL,
          merge_order = orig_labels
        ))
      }
      
      # BUILD MERGE MATRIX USING RECORDED MERGES
      merge_matrix <- matrix(0, nrow = n_items - 1, ncol = 2)
      height_vector <- numeric(n_items - 1)
      
      # Map from label to current cluster index
      # Original items get negative indices
      label_map <- list()
      for (idx in seq_along(orig_labels)) {
        label_map[[orig_labels[idx]]] <- -idx
      }
      
      # Process each merge in order
      for (i in 1:(n_items - 1)) {
        item1 <- merge_record$item1[i]
        item2 <- merge_record$item2[i]
        
        # Get indices for both items
        idx1 <- label_map[[item1]]
        idx2 <- label_map[[item2]]
        
        # Safety check
        if (is.null(idx1) || is.null(idx2)) {
          stop(paste("Failed to find indices for merge:", item1, "+", item2))
        }
        
        # Store in merge matrix (smaller index first)
        if (idx1 < idx2) {
          merge_matrix[i, 1] <- idx1
          merge_matrix[i, 2] <- idx2
        } else {
          merge_matrix[i, 1] <- idx2
          merge_matrix[i, 2] <- idx1
        }
        
        # Height = initial_chi - chi_after_merge
        height_vector[i] <- chi_init - merge_stats$chi_square[i + 1]
        
        # Update label_map: new cluster gets positive index i
        new_cluster_label <- paste0("(", item1, " + ", item2, ")")
        label_map[[new_cluster_label]] <- i
        
        # Remove merged items
        label_map[[item1]] <- NULL
        label_map[[item2]] <- NULL
      }
      
      # Return complete result
      list(
        initial_chi_square = chi_init,
        critical_value = critical_value,
        merge_statistics = merge_stats,
        significant_step = sig_step,
        significant_membership = sig_membership,
        initial_table = orig_crosstab,
        initial_labels = orig_labels,
        merge_matrix = merge_matrix,
        height_vector = height_vector,
        merge_order = orig_labels,
        method_info = method_info
      )
    },
    
    .populateRowResults = function(row_result) {
      
      # Build interpretation text based on partition method
      sig_step <- row_result$significant_step
      method_info <- row_result$method_info
      
      if (method_info$method == "inertia") {
        # Inertia gain ratio method interpretation
        interpretation <- if (sig_step == 0) {
          "No rows can be merged—all rows are distinct."
        } else if (sig_step == nrow(row_result$merge_statistics) - 1) {
          "All rows can be merged into a single group—there is no heterogeneity among rows."
        } else {
          paste0("Optimal partition at step ", sig_step, " yields ", 
                 method_info$optimal_clusters, " cluster",
                 if(method_info$optimal_clusters > 1) "s" else "",
                 " based on the largest relative drop in inertia gain. See Husson et al. (2017).")
        }
        
        summary_text <- paste0(
          "Initial χ² = ", sprintf("%.3f", row_result$initial_chi_square), "; ",
          "Optimal clusters = ", method_info$optimal_clusters, "; ",
          "Selected step = ", sig_step, ". ",
          interpretation
        )
      } else {
        # Greenacre critical chi-square method interpretation
        interpretation <- if (sig_step == 0) {
          "No rows can be merged without losing significant association—all rows are distinct. This is common with large samples, where even small differences between profiles are statistically significant; the inertia gain ratio method (Husson et al., 2017) does not depend on sample size."
        } else if (sig_step == nrow(row_result$merge_statistics) - 1) {
          "All rows can be merged into a single group—there is no significant heterogeneity among rows."
        } else {
          paste0("Merging is justified through step ", sig_step, ", at which point ", 
                 length(row_result$significant_membership), " distinct row group",
                 if(length(row_result$significant_membership) > 1) "s remain" else " remains",
                 ". Further merging would reduce the association below the significance threshold. See Greenacre (2017).")
        }
        
        summary_text <- paste0(
          "Initial χ² = ", sprintf("%.3f", row_result$initial_chi_square), "; ",
          "Critical value (p=0.05) = ", sprintf("%.3f", row_result$critical_value), "; ",
          "Last significant step = ", sig_step, ". ",
          interpretation
        )
      }
      
      # Populate merging sequence table using setRow
      table <- self$results$rowClusterTable
      for (i in 1:nrow(row_result$merge_statistics)) {
        values <- list(
          step = row_result$merge_statistics$step[i],
          itemsMerged = row_result$merge_statistics$items_merged[i],
          groupsRemaining = row_result$merge_statistics$groups_remaining[i],
          chiSquare = row_result$merge_statistics$chi_square[i],
          reduction = row_result$merge_statistics$reduction[i],
          reductionPercent = row_result$merge_statistics$reduction_percent[i]
        )
        table$setRow(rowKey = paste0('step', i - 1), values = values)
      }
      
      # Add summary as table footnote
      table$setNote('rowClusterSummary', summary_text, init = FALSE)
      
      # Populate significant groups table using addRow
      groups_table <- self$results$rowGroupsTable
      groups_table$deleteRows()  # <-- ADD THIS LINE
      row_groups <- private$.extractClusterGroups(row_result)
      
      group_num <- 1
      
      for (group_name in names(row_groups)) {
        values <- list(
          groupName = paste("Group", group_num),
          members = paste(row_groups[[group_name]], collapse = ", ")
        )
        groups_table$addRow(rowKey = group_num, values = values)
        group_num <- group_num + 1
      }
      
      # Add interpretation as table footnote
      groups_note <- paste0(
        "Groups contain row categories with similar column profiles. ",
        "Categories within a group can be treated as equivalent without materially affecting the association."
      )
      groups_table$setNote('rowGroupsInterpretation', groups_note, init = FALSE)
    },
    
    .populateColResults = function(col_result) {
      
      # Build interpretation text based on partition method
      sig_step <- col_result$significant_step
      method_info <- col_result$method_info
      
      if (method_info$method == "inertia") {
        # Inertia gain ratio method interpretation
        interpretation <- if (sig_step == 0) {
          "No columns can be merged—all columns are distinct."
        } else if (sig_step == nrow(col_result$merge_statistics) - 1) {
          "All columns can be merged into a single group—there is no heterogeneity among columns."
        } else {
          paste0("Optimal partition at step ", sig_step, " yields ", 
                 method_info$optimal_clusters, " cluster",
                 if(method_info$optimal_clusters > 1) "s" else "",
                 " based on the largest relative drop in inertia gain. See Husson et al. (2017).")
        }
        
        summary_text <- paste0(
          "Initial χ² = ", sprintf("%.3f", col_result$initial_chi_square), "; ",
          "Optimal clusters = ", method_info$optimal_clusters, "; ",
          "Selected step = ", sig_step, ". ",
          interpretation
        )
      } else {
        # Greenacre critical chi-square method interpretation
        interpretation <- if (sig_step == 0) {
          "No columns can be merged without losing significant association—all columns are distinct. This is common with large samples, where even small differences between profiles are statistically significant; the inertia gain ratio method (Husson et al., 2017) does not depend on sample size."
        } else if (sig_step == nrow(col_result$merge_statistics) - 1) {
          "All columns can be merged into a single group—there is no significant heterogeneity among columns."
        } else {
          paste0("Merging is justified through step ", sig_step, ", at which point ", 
                 length(col_result$significant_membership), " distinct column group",
                 if(length(col_result$significant_membership) > 1) "s remain" else " remains",
                 ". Further merging would reduce the association below the significance threshold. See Greenacre (2017).")
        }
        
        summary_text <- paste0(
          "Initial χ² = ", sprintf("%.3f", col_result$initial_chi_square), "; ",
          "Critical value (p=0.05) = ", sprintf("%.3f", col_result$critical_value), "; ",
          "Last significant step = ", sig_step, ". ",
          interpretation
        )
      }
      
      # Populate merging sequence table using setRow
      table <- self$results$colClusterTable
      for (i in 1:nrow(col_result$merge_statistics)) {
        values <- list(
          step = col_result$merge_statistics$step[i],
          itemsMerged = col_result$merge_statistics$items_merged[i],
          groupsRemaining = col_result$merge_statistics$groups_remaining[i],
          chiSquare = col_result$merge_statistics$chi_square[i],
          reduction = col_result$merge_statistics$reduction[i],
          reductionPercent = col_result$merge_statistics$reduction_percent[i]
        )
        table$setRow(rowKey = paste0('step', i - 1), values = values)
      }
      
      # Add summary as table footnote
      table$setNote('colClusterSummary', summary_text, init = FALSE)
      
      # Populate significant groups table using addRow
      groups_table <- self$results$colGroupsTable
      groups_table$deleteRows()
      col_groups <- private$.extractClusterGroups(col_result)
      
      group_num <- 1
      
      for (group_name in names(col_groups)) {
        values <- list(
          groupName = paste("Group", group_num),
          members = paste(col_groups[[group_name]], collapse = ", ")
        )
        groups_table$addRow(rowKey = group_num, values = values)
        group_num <- group_num + 1
      }
      
      # Add interpretation as table footnote
      groups_note <- paste0(
        "Groups contain column categories with similar row profiles. ",
        "Categories within a group can be treated as equivalent without materially affecting the association."
      )
      groups_table$setNote('colGroupsInterpretation', groups_note, init = FALSE)
    },
    
    .extractClusterGroups = function(result) {
      
      sig_step <- result$significant_step
      
      if (sig_step == 0) {
        # No merging - all items separate
        groups <- list()
        for (label in result$initial_labels) {
          groups[[label]] <- list(label)
        }
        return(groups)
      }
      
      membership <- result$significant_membership
      
      if (is.null(membership) || length(membership) == 0) {
        groups <- list()
        for (label in result$initial_labels) {
          groups[[label]] <- list(label)
        }
        return(groups)
      }
      
      # Extract groups from membership
      # These are ONLY the clusters that exist at the significant step
      groups <- list()
      for (cluster_label in names(membership)) {
        members <- membership[[cluster_label]]
        group_name <- paste(sort(members), collapse = ", ")
        groups[[group_name]] <- sort(members)
      }
      
      return(groups)
    },
    
    # Extract the pieces a dendrogram needs (not the whole result list)
    .dendroState = function(result) {
      list(
        merge_order = result$merge_order,
        merge_matrix = result$merge_matrix,
        height_vector = result$height_vector,
        significant_step = result$significant_step,
        initial_chi_square = result$initial_chi_square,
        method_info = list(
          method = result$method_info$method,
          optimal_clusters = result$method_info$optimal_clusters
        )
      )
    },
    
    .plotRowDendro = function(image, ggtheme, theme, ...) {
      
      # Read from the image state (private fields are NULL on export)
      result <- image$state
      
      if (is.null(result)) {
        return(FALSE)
      }
      
      # Build hclust object
      n_items <- length(result$merge_order)
      merge_matrix <- result$merge_matrix
      height_vector <- result$height_vector
      labels <- result$merge_order
      
      hc <- list(
        merge = merge_matrix,
        height = height_vector,
        order = seq_len(n_items),
        labels = labels,
        method = "ward",
        dist.method = "chi-squared"
      )
      class(hc) <- "hclust"
      
      # Compute proper dendrogram order
      hc$order <- stats::order.dendrogram(stats::as.dendrogram(hc))
      
      # Calculate cut position in dendrogram space
      sig_step <- result$significant_step
      
      if (sig_step > 0 && sig_step < length(height_vector)) {
        last_accepted_height <- height_vector[sig_step]
        first_rejected_height <- height_vector[sig_step + 1]
        cut_position <- (last_accepted_height + first_rejected_height) / 2
      } else if (sig_step == 0) {
        cut_position <- 0
      } else {
        cut_position <- result$initial_chi_square
      }
      
      # Calculate the displayed chi-square value at this position
      displayed_chi_square <- result$initial_chi_square - cut_position
      
      # Build caption text for the reference line
      caption_text <- if (result$method_info$method == "inertia") {
        sprintf("Dashed line: optimal partition (%d clusters, \u03C7\u00B2 = %.1f)", 
                result$method_info$optimal_clusters, displayed_chi_square)
      } else {
        sprintf("Dashed line: clustering threshold (\u03C7\u00B2 = %.1f)", displayed_chi_square)
      }
      
      # Use ggdendro to extract dendrogram data
      ddata <- ggdendro::dendro_data(hc)
      
      # Create ggplot dendrogram
      plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = ddata$segments,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
        ) +
        ggplot2::geom_hline(
          yintercept = cut_position,
          color = "red",
          linetype = "dashed",
          linewidth = 1,
          alpha = 0.7
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          breaks = scales::pretty_breaks(n = 6),
          labels = function(x) round(result$initial_chi_square - x, 1),
          limits = c(0, result$initial_chi_square)
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(hc$labels),
          labels = hc$labels[hc$order]
        ) +
        ggplot2::labs(
          title = "Row Clustering Dendrogram",
          x = "",
          y = "Chi-squared statistic",
          caption = caption_text
        ) +
        ggtheme +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 10),
          panel.grid.minor = ggplot2::element_blank(),
          plot.caption = ggplot2::element_text(color = "red", hjust = 0.5, size = 9)
        )
      
      print(plot)
      
      TRUE
    },
    
    .plotColDendro = function(image, ggtheme, theme, ...) {
      
      # Read from the image state (private fields are NULL on export)
      result <- image$state
      
      if (is.null(result)) {
        return(FALSE)
      }
      
      # Build hclust object
      n_items <- length(result$merge_order)
      merge_matrix <- result$merge_matrix
      height_vector <- result$height_vector
      labels <- result$merge_order
      
      hc <- list(
        merge = merge_matrix,
        height = height_vector,
        order = seq_len(n_items),
        labels = labels,
        method = "ward",
        dist.method = "chi-squared"
      )
      class(hc) <- "hclust"
      
      # Compute proper dendrogram order
      hc$order <- stats::order.dendrogram(stats::as.dendrogram(hc))
      
      # Calculate cut position in dendrogram space
      sig_step <- result$significant_step
      
      if (sig_step > 0 && sig_step < length(height_vector)) {
        last_accepted_height <- height_vector[sig_step]
        first_rejected_height <- height_vector[sig_step + 1]
        cut_position <- (last_accepted_height + first_rejected_height) / 2
      } else if (sig_step == 0) {
        cut_position <- 0
      } else {
        cut_position <- result$initial_chi_square
      }
      
      # Calculate the displayed chi-square value at this position
      displayed_chi_square <- result$initial_chi_square - cut_position
      
      # Build caption text for the reference line
      caption_text <- if (result$method_info$method == "inertia") {
        sprintf("Dashed line: optimal partition (%d clusters, \u03C7\u00B2 = %.1f)", 
                result$method_info$optimal_clusters, displayed_chi_square)
      } else {
        sprintf("Dashed line: clustering threshold (\u03C7\u00B2 = %.1f)", displayed_chi_square)
      }
      
      # Use ggdendro to extract dendrogram data
      ddata <- ggdendro::dendro_data(hc)
      
      # Create ggplot dendrogram
      plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = ddata$segments,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
        ) +
        ggplot2::geom_hline(
          yintercept = cut_position,
          color = "red",
          linetype = "dashed",
          linewidth = 1,
          alpha = 0.7
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          breaks = scales::pretty_breaks(n = 6),
          labels = function(x) round(result$initial_chi_square - x, 1),
          limits = c(0, result$initial_chi_square)
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(hc$labels),
          labels = hc$labels[hc$order]
        ) +
        ggplot2::labs(
          title = "Column Clustering Dendrogram",
          x = "",
          y = "Chi-squared statistic",
          caption = caption_text
        ) +
        ggtheme +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 10),
          panel.grid.minor = ggplot2::element_blank(),
          plot.caption = ggplot2::element_text(color = "red", hjust = 0.5, size = 9)
        )
      
      print(plot)
      
      TRUE
    },
    
    .populateMethodInfo = function() {
      
      partition_method <- self$options$partitionMethod
      
      html <- paste0(
        "<div style='font-size: 0.9em; color: #444; line-height: 1.6;'>",
        
        "<h3 style='color: #2874A6; margin-top: 1em;'>Clustering Method Overview</h3>",
        "<p>This hierarchical clustering method identifies homogeneous groups of rows and columns ",
        "in a contingency table that can be merged without substantially reducing the table's association. ",
        "The procedure uses the <strong>chi-squared statistic as a distance measure</strong> between categories, ",
        "applying Ward's linkage criterion to create clusters that preserve as much of the original association ",
        "as possible.</p>",
        
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Theoretical Foundation</h3>",
        "<p>The method is based on a fundamental principle: when two rows (or columns) have similar profiles ",
        "across the other dimension, merging them will cause only a small reduction in the table's chi-squared ",
        "statistic. This reduction quantifies the 'cost' of treating the two categories as identical. The ",
        "clustering algorithm systematically finds pairs whose merger minimises this cost, building a ",
        "hierarchical tree that shows the natural groupings in the data.</p>",
        
        "<p>The chi-squared statistic for a contingency table measures the overall deviation from independence. ",
        "When we merge two rows (or two columns), we reduce the table's degrees of freedom, which typically reduces χ². The key ",
        "insight is that the <strong>amount of reduction reveals how similar</strong> the merged categories are: ",
        "merging near-identical rows (or columns) causes minimal reduction, whilst merging dissimilar rows causes substantial ",
        "reduction.</p>",
        
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Algorithmic Procedure</h3>",
        "<p>The clustering procedure is identical regardless of the partition selection method:</p>",
        
        "<ol style='margin-left: 2em;'>",
        "<li><strong>Step 0 (Initial state):</strong> Calculate χ² for the full table. This serves as the ",
        "baseline from which all reductions are measured.</li>",
        
        "<li><strong>Iterative merging:</strong> At each step, evaluate all possible pairs of rows (or columns) ",
        "that could be merged. For each candidate pair: (a) temporarily merge the two rows by summing their ",
        "frequencies; (b) recalculate χ² for the reduced table; (c) compute the reduction = χ²<sub>before</sub> − ",
        "χ²<sub>after</sub>. The pair producing the <strong>smallest reduction</strong> is merged permanently. ",
        "This is Ward's criterion applied to chi-squared distance.</li>",
        
        "<li><strong>Tracking the hierarchy:</strong> Each merge is recorded with: the items merged, the ",
        "resulting χ², the absolute reduction, and the percentage reduction relative to the initial χ². ",
        "This creates the 'merging sequence' table.</li>",
        
        "<li><strong>Selecting the partition:</strong> The final step is to determine where to 'cut' the ",
        "hierarchical tree to obtain the final groupings. Two methods are available for this purpose.</li>",
        "</ol>",
        
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Partition Selection Methods</h3>",
        
        # Method 1: Greenacre
        "<h4 style='color: #1a5276; margin-top: 1em;'>Method 1: Critical Chi-Squared (Greenacre, 2017)",
        if (partition_method == "greenacre") " — <em>currently selected</em>" else "",
        "</h4>",
        
        "<p>This method uses a hypothesis-testing framework to determine which merges are statistically ",
        "justified. At each step, the chi-squared reduction caused by merging is compared against a ",
        "<strong>critical threshold</strong> derived from the distribution of the largest characteristic ",
        "root (Pearson & Hartley, 1972, Table 51; reproduced in Greenacre, 2017, Exhibit A.1).</p>",
        
        "<p>Merging continues as long as the reduction at each step remains <em>below</em> the critical ",
        "value. When a proposed merge would cause a reduction that <em>exceeds</em> the threshold, ",
        "the algorithm stops—this merge would combine categories whose profiles differ significantly ",
        "at α = 0.05.</p>",
        
        "<p><strong>Interpretation:</strong> The resulting groups represent categories that are ",
        "statistically indistinguishable in their profiles. Merging is justified by hypothesis testing, ",
        "providing a principled basis for simplifying the table.</p>",
        
        "<p><strong>Table size constraints:</strong> Critical values are only available for tables where ",
        "3 ≤ min(rows, columns) ≤ 11 and max(rows, columns) ≤ 201. For larger tables, use the inertia ",
        "gain ratio method instead.</p>",
        
        # Method 2: Husson
        "<h4 style='color: #1a5276; margin-top: 1.5em;'>Method 2: Inertia Gain Ratio (Husson et al., 2017)",
        if (partition_method == "inertia") " — <em>currently selected</em>" else "",
        "</h4>",
        
        "<p>This method uses a data-driven criterion to find the 'natural' number of clusters by ",
        "identifying the point in the hierarchy where the <strong>relative gain in between-cluster ",
        "inertia drops most sharply</strong>.</p>",
        
        "<p>At each step going from <em>q</em> clusters to <em>q</em>−1 clusters, the algorithm records ",
        "the inertia gain Δ(<em>q</em>). Since total inertia equals χ²/<em>N</em>, this is equivalent to ",
        "tracking the chi-squared reduction at each merge. The optimal partition is the one that minimises:</p>",
        
        "<p style='text-align: center; font-style: italic;'>Δ(q) / Δ(q+1)</p>",
        
        "<p>This ratio captures the 'elbow' in the inertia curve—the point where further merging ",
        "yields diminishing returns. A small ratio indicates that the current merge causes relatively little ",
        "loss compared to the next merge, suggesting the current partition captures substantially more ",
        "structure than the next coarser partition.</p>",
        
        "<p><strong>Interpretation:</strong> The resulting groups represent a parsimonious summary of ",
        "the table's structure, balancing detail against simplicity. Unlike the critical chi-squared ",
        "method, this approach does not make formal significance claims—it identifies structure rather ",
        "than testing hypotheses.</p>",
        
        "<p><strong>Table size:</strong> This method has no table size constraints and can be applied ",
        "to contingency tables of any dimension.</p>",
        
        "<p><strong>Note on implementation:</strong> The criterion described here originates from Husson ",
        "et al. (2017) and is implemented in FactoMineR's <code>HCPC()</code> function. In FactoMineR, ",
        "the criterion is typically applied to a hierarchy built from Correspondence Analysis coordinates ",
        "using Euclidean distance. In this module, we apply the same criterion to a hierarchy built ",
        "directly from chi-squared distances between row (or column) profiles. Both approaches use the ",
        "same partition selection logic; they differ only in how the underlying hierarchy is constructed.</p>",
        
        # Dendrogram interpretation
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Interpreting the Dendrogram</h3>",
        "<p>The dendrogram (cluster tree) visualises the hierarchical structure. The horizontal axis represents ",
        "the <strong>remaining χ² statistic</strong> after each merge: high values appear on the left (near the ",
        "category labels, where little merging has occurred) and low values on the right (where most categories ",
        "have been combined).</p>",
        
        "<p><strong>Reading the dendrogram:</strong> Categories that merge early (toward the left of the plot, ",
        "at high remaining χ² values) are most similar—their merger causes only a small reduction in χ². ",
        "Categories that only join later (toward the right, at lower remaining χ² values) are more distinct.</p>",
        
        "<p>The <strong>red dashed line</strong> marks the partition cutoff:</p>",
        "<ul style='margin-left: 2em;'>",
        "<li>For the <strong>critical chi-squared method</strong>, the line is positioned between the last ",
        "statistically justified merge and the first unjustified merge. Clusters forming entirely to the ",
        "left of this line represent statistically significant groupings at α = 0.05.</li>",
        "<li>For the <strong>inertia gain ratio method</strong>, the line indicates the optimal partition ",
        "identified by the largest relative drop in inertia gain.</li>",
        "</ul>",
        
        # Practical considerations
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Practical Considerations</h3>",
        
        "<p><strong>Comparing the two methods:</strong> The critical chi-squared method provides a formal ",
        "significance test but is limited to smaller tables. The inertia gain ratio method is more flexible ",
        "and often suggests fewer, larger clusters. When both methods are applicable, comparing their results ",
        "can provide useful perspective on the table's structure.</p>",
        
        "<p><strong>Reduction percentages:</strong> The percentage reduction at each merge indicates how ",
        "'costly' that merge was in terms of lost association. Small percentages (< 5%) suggest the merged ",
        "categories were nearly identical; large percentages (> 20%) suggest substantial heterogeneity.</p>",
        
        "<p><strong>Statistical vs. substantive significance:</strong> Both methods are data-driven and do ",
        "not incorporate domain knowledge. Just because two categories <em>can</em> be merged does not ",
        "necessarily mean they <em>should</em> be merged in practice. Domain expertise should guide the ",
        "final decision about whether identified groups make conceptual sense.</p>",
        
        "<p><strong>Asymmetry of rows and columns:</strong> The clusters identified for rows are independent ",
        "of those for columns. It is entirely possible to find, for instance, that rows naturally group into ",
        "three clusters whilst columns group into two.</p>",
        
        "</div>"
      )
      
      self$results$methodInfo$setContent(html)
    },
    
    # Store clustering results for plotting
    .rowClusterResult = NULL,
    .colClusterResult = NULL,
    # Table stability fields
    .rowLevels = NULL,
    .colLevels = NULL,
    .I = NULL,
    .J = NULL
  )
)