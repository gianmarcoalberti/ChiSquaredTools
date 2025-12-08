# This file is a generated template, your changes will not be overwritten

#' @export
chisqclusterClass <- R6::R6Class(
  "chisqclusterClass",
  inherit = chisqclusterBase,
  private = list(
    
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
      
      # Perform clustering analysis
      tryCatch({
        # Row clustering
        row_result <- private$.clusterContingency(contingency_table, dim = "row")
        private$.populateRowResults(row_result)
        
        # Column clustering
        col_result <- private$.clusterContingency(contingency_table, dim = "col")
        private$.populateColResults(col_result)
        
        # Store results for plotting
        private$.rowClusterResult <- row_result
        private$.colClusterResult <- col_result
        
      }, error = function(e) {
        # If clustering fails (e.g., table too small), show error message
        self$results$rowClusterSummary$setContent(
          paste0("<div style='color: red; padding: 10px;'>",
                 "WARNING: ", e$message, "</div>")
        )
      })
      
      # Populate method info and references
      private$.populateMethodInfo()
      private$.populateReferences()
    },
    
    .populateCrosstab = function(contingency_table) {
      
      crosstab <- self$results$crosstabTable
      
      row_names <- rownames(contingency_table)
      col_names <- colnames(contingency_table)
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      
      # Set table title
      crosstab$setTitle(paste0(rowVar, " × ", colVar))
      
      # Add row names column
      crosstab$addColumn(
        name = "rowName",
        title = rowVar,
        type = "text"
      )
      
      # Add data columns with superTitle
      for (j in seq_along(col_names)) {
        crosstab$addColumn(
          name = paste0("col", j),
          title = col_names[j],
          type = "integer",
          superTitle = colVar
        )
      }
      
      # Add row total column
      crosstab$addColumn(
        name = "rowTotal",
        title = "Total",
        type = "integer"
      )
      
      # Populate data rows
      for (i in seq_along(row_names)) {
        values <- list(rowName = row_names[i])
        for (j in seq_along(col_names)) {
          values[[paste0("col", j)]] <- as.integer(contingency_table[i, j])
        }
        values[["rowTotal"]] <- sum(contingency_table[i, ])
        crosstab$addRow(rowKey = i, values = values)
      }
      
      # Add total row
      total_values <- list(rowName = "Total")
      for (j in seq_along(col_names)) {
        total_values[[paste0("col", j)]] <- sum(contingency_table[, j])
      }
      total_values[["rowTotal"]] <- sum(contingency_table)
      crosstab$addRow(rowKey = "total", values = total_values)
    },
    
    .getCriticalChiSquare = function(nrow, ncol) {
      # Critical values table from Greenacre (2017), Exhibit A.1, p.254
      # Source: Pearson & Hartley (1972), Biometrika Tables
      
      critical_values <- matrix(NA, nrow = 13, ncol = 11)
      
      # Populate the critical values table
      critical_values[3, 3] <- 8.59
      critical_values[4, 3] <- 10.74
      critical_values[4, 4] <- 13.11
      critical_values[5, 3] <- 12.68
      critical_values[5, 4] <- 15.24
      critical_values[5, 5] <- 17.52
      critical_values[6, 3] <- 14.49
      critical_values[6, 4] <- 17.21
      critical_values[6, 5] <- 19.63
      critical_values[6, 6] <- 21.85
      critical_values[7, 3] <- 16.21
      critical_values[7, 4] <- 19.09
      critical_values[7, 5] <- 21.62
      critical_values[7, 6] <- 23.95
      critical_values[7, 7] <- 26.14
      critical_values[8, 3] <- 17.88
      critical_values[8, 4] <- 20.88
      critical_values[8, 5] <- 23.53
      critical_values[8, 6] <- 25.96
      critical_values[8, 7] <- 28.23
      critical_values[8, 8] <- 30.40
      critical_values[9, 3] <- 19.49
      critical_values[9, 4] <- 22.62
      critical_values[9, 5] <- 25.37
      critical_values[9, 6] <- 27.88
      critical_values[9, 7] <- 30.24
      critical_values[9, 8] <- 32.48
      critical_values[9, 9] <- 34.63
      critical_values[10, 3] <- 21.06
      critical_values[10, 4] <- 24.31
      critical_values[10, 5] <- 27.15
      critical_values[10, 6] <- 29.75
      critical_values[10, 7] <- 32.18
      critical_values[10, 8] <- 34.50
      critical_values[10, 9] <- 36.70
      critical_values[10, 10] <- 38.84
      critical_values[11, 3] <- 22.61
      critical_values[11, 4] <- 25.96
      critical_values[11, 5] <- 28.90
      critical_values[11, 6] <- 31.57
      critical_values[11, 7] <- 34.08
      critical_values[11, 8] <- 36.45
      critical_values[11, 9] <- 38.72
      critical_values[11, 10] <- 40.91
      critical_values[11, 11] <- 43.04
      critical_values[12, 3] <- 24.12
      critical_values[12, 4] <- 27.58
      critical_values[12, 5] <- 30.60
      critical_values[12, 6] <- 33.35
      critical_values[12, 7] <- 35.93
      critical_values[12, 8] <- 38.36
      critical_values[12, 9] <- 40.69
      critical_values[12, 10] <- 42.93
      critical_values[12, 11] <- 45.10
      critical_values[13, 3] <- 25.61
      critical_values[13, 4] <- 29.17
      critical_values[13, 5] <- 32.27
      critical_values[13, 6] <- 35.09
      critical_values[13, 7] <- 37.73
      critical_values[13, 8] <- 40.22
      critical_values[13, 9] <- 42.60
      critical_values[13, 10] <- 44.90
      critical_values[13, 11] <- 47.12
      
      # Handle table symmetry
      if (nrow >= 3 && nrow <= 13 && ncol >= 3 && ncol <= 11) {
        return(critical_values[nrow, ncol])
      } else if (ncol >= 3 && ncol <= 13 && nrow >= 3 && nrow <= 11) {
        return(critical_values[ncol, nrow])
      } else if (nrow < 3 || ncol < 3) {
        stop(paste0("Table too small (", nrow, "×", ncol, 
                    "). Critical values only available for tables ≥ 3×3."))
      } else {
        stop(paste0("Table too large (", nrow, "×", ncol, 
                    "). Critical values only available for tables up to 13×11."))
      }
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
      table_dims <- dim(orig_crosstab)
      critical_value <- private$.getCriticalChiSquare(table_dims[1], table_dims[2])
      
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
      
      # Find last significant step using the Greenacre/Hirotsu criterion:
      # Stop merging when an individual merge reduction exceeds the critical threshold
      
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
        merge_order = orig_labels
      )
    },
    
    .populateRowResults = function(row_result) {
      
      # Summary HTML
      # Summary HTML with interpretation
      sig_step <- row_result$significant_step
      interpretation <- if (sig_step == 0) {
        "No rows can be merged without losing significant association—all rows are distinct."
      } else if (sig_step == nrow(row_result$merge_statistics) - 1) {
        "All rows can be merged into a single group—there is no significant heterogeneity among rows."
      } else {
        paste0("Merging is justified through step ", sig_step, ", at which point ", 
               length(row_result$significant_membership), " distinct row group",
               if(length(row_result$significant_membership) > 1) "s remain" else " remains",
               " (shown below). Further merging would reduce the association below the significance threshold.")
      }
      
      summary_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Initial χ²:</strong> ", sprintf("%.3f", row_result$initial_chi_square), " | ",
        "<strong>Critical value (p=0.05):</strong> ", sprintf("%.3f", row_result$critical_value), " | ",
        "<strong>Last significant step:</strong> ", sig_step, "</p>",
        "<p style='margin-top: 8px; padding: 8px; background-color: #f0f8ff; border-left: 3px solid #2874A6;'>",
        "<strong>Interpretation:</strong> ", interpretation, "</p>",
        "</div>"
      )
      
      self$results$rowClusterSummary$setContent(summary_html)
      
      # Populate merging sequence table
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
        table$addRow(rowKey = i, values = values)
      }
      
      # Populate significant groups table
      groups_table <- self$results$rowGroupsTable
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
      
      # Add explanatory note
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> These groups represent row categories that are statistically similar ",
        "in their relationship with the column variable. Categories within the same group have been merged ",
        "without reducing the chi-squared statistic below the critical threshold (p=0.05). This means they can ",
        "be treated as equivalent for practical purposes—their column profiles are so similar that ",
        "distinguishing between them does not contribute meaningfully to the overall association.</p>",
        "</div>"
      )
      self$results$rowGroupsNote$setContent(note_html)
    },
    
    .populateColResults = function(col_result) {
      
      # Summary HTML with interpretation
      sig_step <- col_result$significant_step
      interpretation <- if (sig_step == 0) {
        "No columns can be merged without losing significant association—all columns are distinct."
      } else if (sig_step == nrow(col_result$merge_statistics) - 1) {
        "All columns can be merged into a single group—there is no significant heterogeneity among columns."
      } else {
        paste0("Merging is justified through step ", sig_step, ", at which point ", 
               length(col_result$significant_membership), " distinct column group",
               if(length(col_result$significant_membership) > 1) "s remain" else " remains",
               " (shown below). Further merging would reduce the association below the significance threshold.")
      }
      
      summary_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Initial χ²:</strong> ", sprintf("%.3f", col_result$initial_chi_square), " | ",
        "<strong>Critical value (p=0.05):</strong> ", sprintf("%.3f", col_result$critical_value), " | ",
        "<strong>Last significant step:</strong> ", sig_step, "</p>",
        "<p style='margin-top: 8px; padding: 8px; background-color: #f0f8ff; border-left: 3px solid #2874A6;'>",
        "<strong>Interpretation:</strong> ", interpretation, "</p>",
        "</div>"
      )
      self$results$colClusterSummary$setContent(summary_html)
      
      # Populate merging sequence table
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
        table$addRow(rowKey = i, values = values)
      }
      
      # Populate significant groups table
      groups_table <- self$results$colGroupsTable
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
      
      # Add explanatory note
      note_html <- paste0(
        "<div style='font-size: 0.9em; color: #555; margin: 10px 0;'>",
        "<p><strong>Interpretation:</strong> These groups represent column categories that are statistically similar ",
        "in their relationship with the row variable. Categories within the same group have been merged ",
        "without reducing the chi-squared statistic below the critical threshold (p=0.05). This means they can ",
        "be treated as equivalent for practical purposes—their row profiles are so similar that ",
        "distinguishing between them does not contribute meaningfully to the overall association.</p>",
        "</div>"
      )
      self$results$colGroupsNote$setContent(note_html)
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
    
    .plotRowDendro = function(image, ggtheme, theme, ...) {
      
      result <- private$.rowClusterResult
      
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
        order = seq_len(n_items),  # Temporary placeholder
        labels = labels,
        method = "ward",
        dist.method = "chi-squared"
      )
      class(hc) <- "hclust"
      
      # Compute proper dendrogram order
      hc$order <- stats::order.dendrogram(stats::as.dendrogram(hc))
      
      # Calculate cut position in dendrogram space
      # Position the line BETWEEN the last accepted merge and first rejected merge
      sig_step <- result$significant_step
      
      if (sig_step > 0 && sig_step < length(height_vector)) {
        # Position halfway between last accepted and first rejected merge
        last_accepted_height <- height_vector[sig_step]
        first_rejected_height <- height_vector[sig_step + 1]
        cut_position <- (last_accepted_height + first_rejected_height) / 2
      } else if (sig_step == 0) {
        # No merging justified - put line at bottom
        cut_position <- 0
      } else {
        # All merges justified - put line at top
        cut_position <- result$initial_chi_square
      }
      
      # Calculate the displayed chi-square value at this position
      displayed_chi_square <- result$initial_chi_square - cut_position
      
      # Determine text alignment to avoid truncation at plot edges
      # If critical value is small (< 40% of initial chi²), place text LEFT of line
      text_hjust <- if (result$critical_value < 0.4 * result$initial_chi_square) 1 else 0
      
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
          y = "Chi-squared statistic"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 10),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = cut_position,
          label = sprintf("Clustering threshold (χ² = %.1f)", displayed_chi_square),
          hjust = text_hjust,
          vjust = -0.5,
          color = "red",
          size = 3.5
        )
      
      print(plot)
      
      TRUE
    },
    
    .plotColDendro = function(image, ggtheme, theme, ...) {
      
      result <- private$.colClusterResult
      
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
        order = seq_len(n_items),  # Temporary placeholder
        labels = labels,
        method = "ward",
        dist.method = "chi-squared"
      )
      class(hc) <- "hclust"
      
      # Compute proper dendrogram order
      hc$order <- stats::order.dendrogram(stats::as.dendrogram(hc))
      
      # Calculate cut position in dendrogram space
      # Position the line BETWEEN the last accepted merge and first rejected merge
      sig_step <- result$significant_step
      
      if (sig_step > 0 && sig_step < length(height_vector)) {
        # Position halfway between last accepted and first rejected merge
        last_accepted_height <- height_vector[sig_step]
        first_rejected_height <- height_vector[sig_step + 1]
        cut_position <- (last_accepted_height + first_rejected_height) / 2
      } else if (sig_step == 0) {
        # No merging justified - put line at bottom
        cut_position <- 0
      } else {
        # All merges justified - put line at top
        cut_position <- result$initial_chi_square
      }
      
      # Calculate the displayed chi-square value at this position
      displayed_chi_square <- result$initial_chi_square - cut_position
      
      # Determine text alignment to avoid truncation at plot edges
      # If critical value is small (< 40% of initial chi²), place text LEFT of line
      text_hjust <- if (result$critical_value < 0.4 * result$initial_chi_square) 1 else 0
      
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
          y = "Chi-squared statistic"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 10),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = cut_position,
          label = sprintf("Clustering threshold (χ² = %.1f)", displayed_chi_square),
          hjust = text_hjust,
          vjust = -0.5,
          color = "red",
          size = 3.5
        )
      
      print(plot)
      
      TRUE
    },
    
    .populateMethodInfo = function() {
      
      html <- paste0(
        "<div style='font-size: 0.9em; color: #444; line-height: 1.6;'>",
        
        "<h3 style='color: #2874A6; margin-top: 1em;'>Clustering Method Overview</h3>",
        "<p>This hierarchical clustering method identifies homogeneous groups of rows and columns ",
        "in a contingency table that can be merged without significantly reducing the table's association. ",
        "The procedure uses the <strong>chi-squared statistic as a distance measure</strong> between categories, ",
        "applying Ward's linkage criterion to create clusters that preserve as much of the original association ",
        "as possible (see: Greenacre 2017).</p>",
        
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
        "<p>The analysis proceeds through the following stages:</p>",
        
        "<ol style='margin-left: 2em;'>",
        "<li><strong>Step 0 (Initial state):</strong> Calculate χ² for the full table. This serves as the ",
        "baseline from which all reductions are measured.</li>",
        
        "<li><strong>Iterative merging:</strong> At each step, evaluate all possible pairs of rows (or columns) ",
        "that could be merged. For each candidate pair: (a) temporarily merge the two rows by summing their ",
        "frequencies; (b) recalculate χ² for the reduced table; (c) compute the reduction = χ²<sub>before</sub> − ",
        "χ²<sub>after</sub>. The pair producing the <strong>smallest reduction</strong> is merged permanently. ",
        "This is Ward's criterion applied to chi-squared distance.</li>",
        
        "<li><strong>Tracking the hierarchy:</strong> Each merge is recorded with: the items merged, the ",
        "resulting χ², the absolute reduction, and the percentage reduction relative to the previous step. ",
        "This creates the 'merging sequence' table, which shows how the association progressively declines ",
        "as categories are combined.</li>",
        
        "<li><strong>Stopping criterion:</strong> Merging continues until an individual merge would cause ",
        "a χ² reduction ≥ the critical threshold (Greenacre 2017, Hirotsu 1983). This critical value ",
        "(from Pearson & Hartley 1972, as tabulated in Greenacre 2017, Exhibit A.1, p.254) controls ",
        "the family-wise error rate at α = 0.05, accounting for the fact that we are conducting many ",
        "implicit tests as we consider different ways to partition the table.</li>",
        
        "<li><strong>Identifying significant groups:</strong> The 'last significant step' is the final merge ",
        "at which χ² remains above the critical threshold. The clusters present at this step represent the ",
        "<strong>significant homogeneous groups</strong>—categories that are sufficiently similar to be ",
        "treated as equivalent without materially distorting the table's association structure.</li>",
        "</ol>",
        
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Interpreting the Dendrogram</h3>",
        "<p>The dendrogram (cluster tree) visualises the hierarchical structure. The vertical axis represents ",
        "the chi-squared statistic, plotted in <strong>reverse</strong>: height = initial χ² − current χ². This ",
        "means that the bottom of the tree (where individual items join) shows high χ² (close to the initial ",
        "value), whilst the top shows low χ² (as categories are progressively merged).</p>",
        
        "<p><strong>Reading the dendrogram:</strong> The horizontal axis shows the remaining χ² statistic at ",
        "each stage of merging. The red line's label indicates the χ² value at that position. This helps you ",
        "verify visually that clusters to the left maintain sufficient association, whilst those to the right ",
        "do not. The actual critical threshold value (which determines where the line is positioned) is shown ",
        "in the summary statistics above.</p>",
        
        "<p>The <strong>red dashed line</strong> marks the clustering threshold—the point at which further ",
        "merging would cause excessive information loss. The line is positioned in the gap between the last ",
        "accepted merge and the first rejected merge. Clusters that form to the <em>left</em> of this line ",
        "(where χ² is high) are statistically significant at p = 0.05 after adjustment for multiple comparisons. ",
        "Merges occurring to the <em>right</em> of the line (where χ² is low) would destroy significant structure ",
        "and are not statistically justified.</p>",
        
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Practical Interpretation</h3>",
        "<p><strong>Significant groups:</strong> Categories within the same group at the last significant ",
        "step can be treated as effectively equivalent in their relationship with the other dimension. For ",
        "example, if rows A, B, and C form a single cluster, this means their column profiles are so similar ",
        "that distinguishing between them does not contribute meaningfully to the table's overall association.</p>",
        
        "<p><strong>Reduction percentages:</strong> The percentage reduction at each merge indicates how ",
        "'costly' that merge was in terms of lost association. Small percentages (< 5%) suggest the merged ",
        "categories were nearly identical; large percentages (> 20%) suggest substantial heterogeneity, ",
        "implying the categories should probably not be combined.</p>",
        
        "<p><strong>Hierarchical structure:</strong> The dendrogram reveals not just which categories are ",
        "similar, but also the <em>degree</em> of similarity. Categories that merge early (at the bottom of ",
        "the tree) are most similar; those that only join near the top are quite different. This hierarchical ",
        "information can guide decisions about whether to use a coarse or fine categorisation.</p>",
        
        "<h3 style='color: #2874A6; margin-top: 1.5em;'>Limitations and Considerations</h3>",
        "<p><strong>Table size constraints:</strong> Critical values are only available for tables with 3–13 ",
        "rows and 3–11 columns. Smaller tables (< 3×3) have too few degrees of freedom for meaningful ",
        "clustering; larger tables exceed the published critical value tables.</p>",
        
        "<p><strong>Statistical vs. substantive significance:</strong> The clustering procedure is purely ",
        "data-driven and does not incorporate substantive knowledge about the categories. Just because two ",
        "categories <em>can</em> be merged without statistical penalty does not necessarily mean they ",
        "<em>should</em> be merged in practice. Domain expertise should guide the final decision about ",
        "whether identified groups make conceptual sense.</p>",
        
        "<p><strong>Asymmetry of rows and columns:</strong> The clusters identified for rows are independent ",
        "of those for columns. It is entirely possible to find, for instance, that rows naturally group into ",
        "three clusters whilst columns group into two. This asymmetry reflects the potentially different ",
        "structure of the row and column variables.</p>",
        
        "</div>"
      )
      
      self$results$methodInfo$setContent(html)
    },
    
    .populateReferences = function() {
      
      references_html <- paste0(
        "<div style='font-size: 0.85em; color: #444; margin: 15px 0; line-height: 1.5;'>",
        "<h3 style='color: #2874A6; margin-top: 0.5em; margin-bottom: 0.5em;'>References</h3>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Bendixen, M.T. (1995). Compositional Perceptual Mapping Using Chi-squared Trees Analysis and Correspondence Analysis. ",
        "<em>Journal of Marketing Management</em>, 11, 571–581.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Greenacre, M. (1988). Clustering the Rows and Columns of a Contingency Table. <em>Journal of Classification</em>, 5, 39–51.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Greenacre, M. (2017). <em>Correspondence Analysis in Practice</em> (3rd ed.). Chapman & Hall/CRC.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Hirotsu, C. (1983). Defining the Pattern of Association in Two-way Contingency Tables. ",
        "<em>Biometrika</em>, 70(3), 579–589.</p>",
        "Pearson, E. S., & Hartley, H. O. (1972). <em>Biometrika Tables for Statisticians, Volume 2</em>. Cambridge University Press.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Ward, J. H. (1963). Hierarchical grouping to optimize an objective function. <em>Journal of the American Statistical Association</em>, 58(301), 236–244.</p>",
        "</div>"
      )
      
      self$results$legendNote$setContent(references_html)
    },
    
    # Store clustering results for plotting
    .rowClusterResult = NULL,
    .colClusterResult = NULL
  )
)
