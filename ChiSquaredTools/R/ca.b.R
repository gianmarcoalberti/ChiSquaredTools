# =============================================================================
# CORRESPONDENCE ANALYSIS - MULTIPLE VISUALISATIONS WITH INTERPRETIVE TABLES
# =============================================================================
# Author: Gianmarco Alberti
# Date: January 2025
# Version: 1.2.0 - Added symmetric plot, geometric interpretation, adjusted residuals
# =============================================================================
# References:
#   Greenacre, M. (2017). Correspondence Analysis in Practice (3rd ed.)
#   Beh, E.J., Lombardo, R., & Alberti, G. (2018). Correspondence analysis and
#     the Freeman-Tukey statistic. Computational Statistics & Data Analysis.
#   Borg, I., & Groenen, P.J.F. (2005). Modern Multidimensional Scaling.
#   Yelland, P. (2010). An Introduction to Correspondence Analysis.
#   Orton, C., & Tyers, P. (1991). A technique for reducing the size of sparse
#     contingency tables.
# =============================================================================

#' @export
caClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "caClass",
    inherit = caBase,
    
    private = list(
        
        # =====================================================================
        # CACHED RESULTS
        # =====================================================================
        .contingencyTable = NULL,
        .caResult = NULL,
        .caResultAdj = NULL,
        .srdResultRows = NULL,
        .srdResultCols = NULL,
        .clusterResultRows = NULL,
        .clusterResultCols = NULL,
        .attrInfluence = NULL,
        .attrElimination = NULL,
        .attrElbowStep = NULL,
        
        # =====================================================================
        # INIT - Set up table structure to prevent flickering
        # =====================================================================
        .init = function() {
            
            # Check if we have enough variables specified
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
            
            # -----------------------------------------------------------------
            # Pre-build observed contingency table structure
            # -----------------------------------------------------------------
            # Get levels from the data to build stable column structure
            data <- self$data
            rowVar <- self$options$rows
            colVar <- self$options$cols
            
            rowLevels <- levels(data[[rowVar]])
            colLevels <- levels(data[[colVar]])
            
            if (is.null(rowLevels) || is.null(colLevels)) return()
            
            I <- length(rowLevels)
            J <- length(colLevels)
            
            table <- self$results$observedTable
            table$setTitle(paste0("Observed Contingency Table: ", rowVar, " \u00d7 ", colVar))
            
            # Add row name column
            table$addColumn(name = "rowname", title = rowVar, type = "text")
            
            # Add columns for each column category
            for (j in seq_len(J)) {
                table$addColumn(
                    name = paste0("col", j),
                    title = colLevels[j],
                    type = "integer",
                    superTitle = colVar
                )
            }
            
            # Add row total column
            table$addColumn(name = "rowtotal", title = "Total", type = "integer")
            
            # Pre-populate rows with placeholders
            for (i in seq_len(I)) {
                values <- list(rowname = rowLevels[i])
                for (j in seq_len(J)) {
                    values[[paste0("col", j)]] <- NA
                }
                values$rowtotal <- NA
                table$addRow(rowKey = paste0("row", i), values = values)
            }
            
            # Add totals row placeholder
            total_values <- list(rowname = "Total")
            for (j in seq_len(J)) {
                total_values[[paste0("col", j)]] <- NA
            }
            total_values$rowtotal <- NA
            table$addRow(rowKey = "total", values = total_values)
            
            # Format the totals row
            table$addFormat(
                rowKey = "total",
                col = "rowname",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            # Pre-populate summary table with placeholder
            if (self$options$showSummary) {
                table <- self$results$summaryTable
                table$setRow(rowNo = 1, values = list(
                    statistic = NA, df = NA, pvalue = NA,
                    totalInertia = NA, nRows = NA, nCols = NA
                ))
            }
            
            # Pre-populate inertia table
            if (self$options$showInertia) {
                table <- self$results$inertiaTable
                nAxes <- private$.effectiveNAxes()
                for (k in 1:nAxes) {
                    table$addRow(rowKey = k, values = list(
                        dim = k, eigenvalue = NA, percent = NA, cumPercent = NA,
                        malinChi = NA, malinDf = NA, malinP = NA
                    ))
                }
            }
            
            # Pre-populate symmetric coordinates table columns
            if (self$options$showSymmetricCoords) {
                table <- self$results$symmetricCoordsTable
                nAxes <- private$.effectiveNAxes()
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("dim", k),
                        title = paste0("Dim ", k),
                        type = "number",
                        format = "zto"
                    )
                }
            }
            
            # Pre-populate seriated table structure (columns built in .run)
            # We just ensure it's visible if requested
            if (self$options$showSeriatedTable) {
                # Structure will be built dynamically in .populateSeriatedTable
                # based on the sorted column levels
            }
            
            # Pre-populate contribution biplot coordinates table columns
            if (self$options$showContribCoords) {
                table <- self$results$contribCoordsTable
                nAxes <- private$.effectiveNAxes()
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("dim", k),
                        title = paste0("Dim ", k),
                        type = "number",
                        format = "zto"
                    )
                }
            }
            
            # Pre-populate quality & contributions table columns
            if (self$options$showQualityContrib) {
                table <- self$results$qualityContribTable
                nAxes <- private$.effectiveNAxes()
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("cos2_", k),
                        title = paste0("Dim ", k),
                        superTitle = "Quality (cos\u00b2)",
                        type = "number",
                        format = "zto"
                    )
                    table$addColumn(
                        name = paste0("contrib_", k),
                        title = paste0("Dim ", k),
                        superTitle = "Contribution %",
                        type = "number",
                        format = "zto"
                    )
                }
                table$addColumn(
                    name = "totalQuality",
                    title = "Total Quality",
                    type = "number",
                    format = "zto"
                )
            }
            
            # Pre-populate dimension definition table (structure only)
            if (self$options$showDimDef) {
                table <- self$results$dimDefTable
                table$addRow(rowKey = 1, values = list(
                    dimension = "Dim 1", category = "...",
                    contrib = NA, cumulContrib = NA, direction = ""
                ))
            }
            
            # Pre-populate dimension summary table
            if (self$options$showDimSummary) {
                table <- self$results$dimSummaryTable
                nAxes <- private$.effectiveNAxes()
                for (k in 1:nAxes) {
                    table$addRow(rowKey = k, values = list(
                        dimension = paste("Dim", k), variance = NA,
                        negativePole = "...", positivePole = "..."
                    ))
                }
            }
            
            # Pre-populate profiles table columns (dynamic based on contribFocus)
            if (self$options$showProfiles) {
                table <- self$results$profilesTable
                table$addColumn(name = "category", title = "Category", type = "text")
                table$addColumn(name = "mass", title = "Mass", type = "number", format = "zto")
            }
            
            # Pre-populate residual matrix table (structure only - columns built in .run)
            if (self$options$showCellContrib) {
                # Table structure will be built dynamically in .populateResidualMatrixTable
                # based on the column variable levels
            }
            
            # Pre-populate correlations table columns
            if (self$options$showCorrelations) {
                table <- self$results$correlTable
                nAxes <- private$.effectiveNAxes()
                
                table$addColumn(name = "category", title = "Category", type = "text")
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("corr", k),
                        title = paste0("Dim ", k),
                        superTitle = "Correlation (\u00b1\u221acos\u00b2)",
                        type = "number",
                        format = "zto"
                    )
                }
                table$addColumn(name = "primaryDim", title = "Primary", type = "text")
                table$addColumn(name = "quality", title = "Quality", type = "number", format = "zto")
            }
            
            # Pre-populate interpretation table (structure only)
            if (self$options$showInterpretation) {
                table <- self$results$interpTable
                table$addRow(rowKey = 1, values = list(
                    category = "...", association = "..."
                ))
            }
        },
        
        # =====================================================================
        # RUN - Main analysis
        # =====================================================================
        .run = function() {
            
            # Early exit if variables not specified
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
            
            # Clear cached results at start of each run to prevent stale data
            private$.contingencyTable <- NULL
            private$.caResult <- NULL
            private$.caResultAdj <- NULL
            private$.srdResultRows <- NULL
            private$.srdResultCols <- NULL
            
            # Build contingency table
            private$.contingencyTable <- private$.buildContingencyTable()
            if (is.null(private$.contingencyTable)) return()
            
            # Run CA based on selected metric and residual type
            private$.caResult <- private$.runCA(private$.contingencyTable)
            if (is.null(private$.caResult)) return()
            
            # Run clustering for both rows and columns (method determined by option)
            private$.clusterResultRows <- private$.runClusteringForVariable("rows")
            private$.clusterResultCols <- private$.runClusteringForVariable("cols")
            
            # Maintain backward compatibility aliases
            private$.srdResultRows <- private$.clusterResultRows
            private$.srdResultCols <- private$.clusterResultCols
            
            # Populate observed table first (always shown)
            private$.populateObservedTable()
            
            # Populate tables
            if (self$options$showSummary) private$.populateSummaryTable()
            if (self$options$showInertia) private$.populateInertiaTable()
            if (self$options$showSymmetricCoords) private$.populateSymmetricCoordsTable()
            if (self$options$showContribCoords) private$.populateContribCoordsTable()
            if (self$options$showQualityContrib) private$.populateQualityContribTable()
            if (self$options$showDimDef) private$.populateDimDefTable()
            if (self$options$showDimSummary) private$.populateDimSummaryTable()
            if (self$options$showProfiles) private$.populateProfilesTable()
            if (self$options$showCellContrib) private$.populateResidualMatrixTable()
            if (self$options$showCorrelations) private$.populateCorrelTable()
            if (self$options$showInterpretation) private$.populateInterpTable()
            if (self$options$showSRDTable) private$.populateSRDTable()
            if (self$options$showSeriatedTable) private$.populateSeriatedTable()
            
            # Populate method information if requested
            if (self$options$showMethodInfo) private$.populateMethodInfo()
            
            # Warn when fewer dimensions exist than requested or plotted
            # (e.g. a 2 x J table has a single dimension)
            private$.addAxesNotice()
        
        # Attribute assessment (Krzanowski 1993)
        private$.attrInfluence <- NULL
        private$.attrElimination <- NULL
        private$.attrElbowStep <- NULL
        
        if (self$options$showAttrInfluence) {
            private$.populateAttrInfluenceTable()
        }
        
        if (self$options$showAttrElimination) {
            private$.populateAttrEliminationTable()
        }
        
        # Save what each plot draws into its image state, so that plots
        # also render when jamovi exports them from a fresh instance
        # (private fields set in .run() are NULL at export time)
        private$.setPlotStates()
    }, 
        
        # =====================================================================
        # BUILD CONTINGENCY TABLE
        # =====================================================================
        .buildContingencyTable = function() {
            
            row_var <- self$options$rows
            col_var <- self$options$cols
            count_var <- self$options$counts
            data <- self$data
            
            if (nrow(data) == 0) {
                return(NULL)
            }
            
            data[[row_var]] <- as.factor(data[[row_var]])
            data[[col_var]] <- as.factor(data[[col_var]])
            
            complete_cases <- complete.cases(data[[row_var]], data[[col_var]])
            if (!is.null(count_var)) {
                complete_cases <- complete_cases & complete.cases(data[[count_var]])
            }
            data <- data[complete_cases, , drop = FALSE]
            
            if (nrow(data) == 0) {
                return(NULL)
            }
            
            # Build contingency table safely (shared helper in utils.R);
            # degenerate tables now stop with a message instead of
            # silently returning NULL and leaving empty tables
            cont_table <- .ctBuildTable(data, c(row_var, col_var), count_var)
            .ctRejectTooFewLevels(cont_table, row_var, col_var)
            .ctRejectEmptyMargins(cont_table, row_var, col_var)
            
            return(as.matrix(cont_table))
        },
        
        # =====================================================================
        # PLOT STATE: SAVE (in .run) AND RESTORE (in each render function)
        # =====================================================================
        # Only the pieces the plots draw are stored: a pruned CA result
        # (coordinates, masses, eigenvalues, contributions, qualities),
        # the observed table, cluster memberships and attribute results.
        .pruneCA = function(ca) {
            if (is.null(ca)) return(NULL)
            keep <- c("N", "n_rows", "n_cols", "row_mass", "col_mass",
                      "row_std", "col_std", "row_princ", "col_princ",
                      "row_contrib_coord", "col_contrib_coord",
                      "eigenvalues", "inertia_total", "inertia_proportion",
                      "inertia_cumulative", "n_axes", "stat_name",
                      "distance_type", "resid_label",
                      "row_contrib", "col_contrib", "row_cos2", "col_cos2")
            ca[intersect(keep, names(ca))]
        },
        
        .setPlotStates = function() {
            ca  <- private$.pruneCA(private$.caResult)
            N   <- private$.contingencyTable
            res <- self$results
            
            res$screePlot$setState(list(ca = ca))
            res$symmetricPlot$setState(list(
                ca = ca, N = N,
                srdRows = private$.srdResultRows,
                srdCols = private$.srdResultCols))
            res$geometricPlot$setState(list(ca = ca))
            res$contribPlot$setState(list(
                ca = ca,
                srdRows = private$.srdResultRows,
                srdCols = private$.srdResultCols,
                clusterRows = private$.clusterResultRows,
                clusterCols = private$.clusterResultCols))
            res$overdispersionPlot$setState(list(N = N))
            
            attr <- list(
                N = N,
                attrInfluence = private$.attrInfluence,
                attrElimination = private$.attrElimination,
                attrElbowStep = private$.attrElbowStep)
            res$attrInfluencePlot$setState(attr)
            res$attrEliminationPlot$setState(attr)
            res$reducedCAPlot$setState(c(attr, list(ca = ca)))
        },
        
        # Called first in every render function: when the instance is fresh
        # (export), refill the private fields from the image state
        .restoreFromState = function(image) {
            st <- image$state
            if (is.null(st)) return(invisible(FALSE))
            if (is.null(private$.caResult) && !is.null(st$ca))
                private$.caResult <- st$ca
            if (is.null(private$.contingencyTable) && !is.null(st$N))
                private$.contingencyTable <- st$N
            if (is.null(private$.srdResultRows) && !is.null(st$srdRows))
                private$.srdResultRows <- st$srdRows
            if (is.null(private$.srdResultCols) && !is.null(st$srdCols))
                private$.srdResultCols <- st$srdCols
            if (is.null(private$.clusterResultRows) && !is.null(st$clusterRows))
                private$.clusterResultRows <- st$clusterRows
            if (is.null(private$.clusterResultCols) && !is.null(st$clusterCols))
                private$.clusterResultCols <- st$clusterCols
            if (is.null(private$.attrInfluence) && !is.null(st$attrInfluence))
                private$.attrInfluence <- st$attrInfluence
            if (is.null(private$.attrElimination) && !is.null(st$attrElimination))
                private$.attrElimination <- st$attrElimination
            if (is.null(private$.attrElbowStep) && !is.null(st$attrElbowStep))
                private$.attrElbowStep <- st$attrElbowStep
            invisible(TRUE)
        },
        
        # =====================================================================
        # NOTICE WHEN THE TABLE HAS FEWER DIMENSIONS THAN REQUESTED
        # =====================================================================
        .addAxesNotice = function() {
            ca <- private$.caResult
            if (is.null(ca)) return()
            nExtracted <- ca$n_axes
            nRequested <- self$options$nAxes
            maxPlotDim <- max(self$options$dim1, self$options$dim2)
            
            msgs <- character(0)
            if (nExtracted == 1) {
                # e.g. a 2 x J table: one dimension only
                if (nRequested > 1 || maxPlotDim > 1) {
                    msgs <- "This table has only one dimension (min(rows, columns) \u2212 1 = 1), so only Dimension 1 is reported and the two-dimensional plots cannot be drawn."
                }
            } else {
                if (nRequested > nExtracted) {
                    msgs <- c(msgs, sprintf(
                        "This table has %d dimensions (min(rows, columns) \u2212 1); %d were requested, so %d are reported.",
                        nExtracted, nRequested, nExtracted))
                }
                if (maxPlotDim > nExtracted) {
                    msgs <- c(msgs, sprintf(
                        "The plots cannot display Dimension %d, because the table has %d dimensions. Choose plot dimensions between 1 and %d.",
                        maxPlotDim, nExtracted, nExtracted))
                }
            }
            # Hide the two-dimensional plots that cannot be drawn: a visible
            # image whose render function returns FALSE keeps spinning
            if (maxPlotDim > nExtracted) {
                for (pl in c("symmetricPlot", "geometricPlot", "contribPlot", "reducedCAPlot"))
                    self$results[[pl]]$setVisible(FALSE)
            }
            
            if (length(msgs) == 0) return()
            
            notice <- jmvcore::Notice$new(
                self$options,
                name = 'axesNotice',
                type = jmvcore::NoticeType$WARNING
            )
            notice$setContent(paste(msgs, collapse = " "))
            self$results$insert(1, notice)
        },
        
        # =====================================================================
        # EFFECTIVE NUMBER OF AXES
        # min(requested axes, min(I, J) - 1), computed from the factor levels
        # so that .init() builds only the dimension columns .run() can fill.
        # (Empty levels are rejected in .buildContingencyTable(), so the
        # number of levels equals the table dimensions.)
        # =====================================================================
        .effectiveNAxes = function() {
            nAxes <- self$options$nAxes
            data <- self$data
            nI <- length(levels(as.factor(data[[self$options$rows]])))
            nJ <- length(levels(as.factor(data[[self$options$cols]])))
            if (nI < 2 || nJ < 2) return(nAxes)
            max(1, min(nAxes, min(nI, nJ) - 1))
        },
        
        # =====================================================================
        # CORRESPONDENCE ANALYSIS
        # =====================================================================
        .runCA = function(N) {
            
            n_axes <- self$options$nAxes
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            I <- nrow(N)
            J <- ncol(N)
            max_axes <- min(I, J) - 1
            if (n_axes > max_axes) n_axes <- max_axes
            
            grand_total <- sum(N)
            P <- N / grand_total
            r <- rowSums(P)
            c <- colSums(P)
            
            # Expected proportions under independence
            r_mat <- matrix(r, nrow = I, ncol = J, byrow = FALSE)
            c_mat <- matrix(c, nrow = I, ncol = J, byrow = TRUE)
            E <- r_mat * c_mat
            
            # Compute residuals based on selected metric and type
            if (distance_metric == "freemanTukey") {
                # Freeman-Tukey residuals: 2 * (sqrt(P) - sqrt(E))
                # These yield Hellinger distances (Beh, Lombardo & Alberti, 2018)
                S <- 2 * (sqrt(P) - sqrt(E))
                stat_name <- "T\u00b2"
                distance_type <- "hellinger"
                resid_label <- "Freeman-Tukey"
            } else if (residual_type == "adjusted") {
                # Adjusted standardised residuals
                S <- private$.computeAdjustedResiduals(P, E, r, c, I, J)
                stat_name <- "\u03c7\u00b2 (adj)"
                distance_type <- "chisq_adj"
                resid_label <- "Adjusted standardised"
            } else {
                # Pearson (standardised) residuals: (P - E) / sqrt(E)
                S <- (P - E) / sqrt(E + 1e-16)
                stat_name <- "\u03c7\u00b2"
                distance_type <- "chisq"
                resid_label <- "Standardised"
            }
            
            # SVD
            svd_result <- svd(S, nu = min(I, n_axes + 1), nv = min(J, n_axes + 1))
            d_all <- svd_result$d
            eigenvalues_all <- d_all^2
            
            # Starting index for axes: always the first singular value.
            # The residual matrices used here (Pearson, adjusted, Freeman-Tukey)
            # are already centred on independence, so there is no trivial
            # singular value of 1 to skip; a genuine first axis can have an
            # inertia close to 1 (e.g. a perfectly diagonal table).
            start_idx <- 1
            
            axes_idx <- start_idx:(start_idx + n_axes - 1)
            axes_idx <- axes_idx[axes_idx <= length(d_all)]
            
            d <- d_all[axes_idx]
            U <- svd_result$u[, axes_idx, drop = FALSE]
            V <- svd_result$v[, axes_idx, drop = FALSE]
            n_axes_actual <- length(d)
            eigenvalues <- d^2
            
            # Coordinates - recovery requires mass weighting for ALL methods
            # Beh et al. (2018), equations 10 and 13:
            #   f_im = (a_im / sqrt(p_i.)) * lambda_m
            #   g_jm = (b_jm / sqrt(p_.j)) * lambda_m
            
            Dr_inv_sqrt <- diag(1 / sqrt(r))
            Dc_inv_sqrt <- diag(1 / sqrt(c))
            
            # Standard coordinates: a_im / sqrt(p_i.) and b_jm / sqrt(p_.j)
            row_std <- Dr_inv_sqrt %*% U
            col_std <- Dc_inv_sqrt %*% V
            
            # Principal coordinates: standard coords × singular values
            row_princ <- row_std %*% diag(d, nrow = length(d))
            col_princ <- col_std %*% diag(d, nrow = length(d))
            
            rownames(row_std) <- rownames(row_princ) <- rownames(N)
            rownames(col_std) <- rownames(col_princ) <- colnames(N)
            colnames(row_std) <- colnames(row_princ) <- paste0("Dim", 1:n_axes_actual)
            colnames(col_std) <- colnames(col_princ) <- paste0("Dim", 1:n_axes_actual)
            
            # Total inertia
            # For all methods, use only min(I-1, J-1) eigenvalues (following Beh & Lombardo)
            max_axes_for_inertia <- min(I - 1, J - 1)
            
            if (start_idx == 2 && length(eigenvalues_all) > 1) {
                # Pearson: skip trivial axis, take next min(I-1,J-1) eigenvalues
                end_idx <- min(start_idx + max_axes_for_inertia - 1, length(eigenvalues_all))
                all_nontrivial <- eigenvalues_all[start_idx:end_idx]
            } else {
                # Freeman-Tukey: take first min(I-1,J-1) eigenvalues
                end_idx <- min(max_axes_for_inertia, length(eigenvalues_all))
                all_nontrivial <- eigenvalues_all[1:end_idx]
            }
            inertia_total <- sum(all_nontrivial)
            inertia_proportion <- eigenvalues / inertia_total
            inertia_cumulative <- cumsum(inertia_proportion)
            
            # Test statistic (both X² and T² are asymptotically chi-squared)
            test_statistic <- inertia_total * grand_total
            df <- (I - 1) * (J - 1)
            p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)
            
            # Contributions (%) - different calculation for Freeman-Tukey
            row_contrib <- matrix(0, nrow = I, ncol = n_axes_actual)
            col_contrib <- matrix(0, nrow = J, ncol = n_axes_actual)
            
            if (distance_metric == "freemanTukey") {
                # For Freeman-Tukey: contributions are based on squared SVD components
                # Following Beh & Lombardo's framework where U and V are orthonormal
                # Contribution = u_im^2 * 100 (sums to 100% per dimension)
                for (k in 1:n_axes_actual) {
                    row_contrib[, k] <- (U[, k]^2) * 100
                    col_contrib[, k] <- (V[, k]^2) * 100
                }
            } else {
                # Classical Pearson formula: contribution = (mass × coord²) / eigenvalue
                for (k in 1:n_axes_actual) {
                    row_contrib[, k] <- (r * row_princ[, k]^2) / eigenvalues[k] * 100
                    col_contrib[, k] <- (c * col_princ[, k]^2) / eigenvalues[k] * 100
                }
            }
            
            rownames(row_contrib) <- rownames(N)
            rownames(col_contrib) <- colnames(N)
            colnames(row_contrib) <- colnames(col_contrib) <- paste0("Dim", 1:n_axes_actual)
            
            # Quality (cos²) - computed using appropriate distance
            if (distance_type == "hellinger") {
                # Hellinger distance: d²_H = 4 * sum[(sqrt(p_i) - sqrt(p_j))²]
                row_dist_sq <- numeric(I)
                for (i in 1:I) {
                    row_profile <- P[i, ] / r[i]
                    row_dist_sq[i] <- 4 * sum((sqrt(row_profile) - sqrt(c))^2)
                }
                
                col_dist_sq <- numeric(J)
                for (j in 1:J) {
                    col_profile <- P[, j] / c[j]
                    col_dist_sq[j] <- 4 * sum((sqrt(col_profile) - sqrt(r))^2)
                }
            } else {
                # Chi-squared distance (standard or adjusted)
                row_dist_sq <- numeric(I)
                for (i in 1:I) {
                    row_profile <- P[i, ] / r[i]
                    row_dist_sq[i] <- sum((row_profile - c)^2 / c)
                }
                
                col_dist_sq <- numeric(J)
                for (j in 1:J) {
                    col_profile <- P[, j] / c[j]
                    col_dist_sq[j] <- sum((col_profile - r)^2 / r)
                }
            }
            
            # Cos² = (principal coordinate)² / (distance from centroid)²
            row_cos2 <- matrix(0, nrow = I, ncol = n_axes_actual)
            col_cos2 <- matrix(0, nrow = J, ncol = n_axes_actual)
            
            for (i in 1:I) {
                if (row_dist_sq[i] > 1e-10) {
                    for (k in 1:n_axes_actual) {
                        row_cos2[i, k] <- (row_princ[i, k]^2) / row_dist_sq[i]
                    }
                }
            }
            
            for (j in 1:J) {
                if (col_dist_sq[j] > 1e-10) {
                    for (k in 1:n_axes_actual) {
                        col_cos2[j, k] <- (col_princ[j, k]^2) / col_dist_sq[j]
                    }
                }
            }
            
            rownames(row_cos2) <- rownames(N)
            rownames(col_cos2) <- colnames(N)
            colnames(row_cos2) <- colnames(col_cos2) <- paste0("Dim", 1:n_axes_actual)
            
            # Contribution coordinates (for biplot) - different for Freeman-Tukey
            if (distance_metric == "freemanTukey") {
                # For Freeman-Tukey: use U and V directly (already orthonormal)
                # The "contribution coordinate" concept maps to the SVD vectors
                row_contrib_coord <- U
                col_contrib_coord <- V
            } else {
                # Classical: contribution coordinates = std_coord * sqrt(mass)
                row_contrib_coord <- row_std * sqrt(r)
                col_contrib_coord <- col_std * sqrt(c)
            }
            rownames(row_contrib_coord) <- rownames(N)
            rownames(col_contrib_coord) <- colnames(N)
            colnames(row_contrib_coord) <- colnames(col_contrib_coord) <- paste0("Dim", 1:n_axes_actual)
            
            # Malinvaud's test
            malinvaud <- private$.computeMalinvaud(eigenvalues_all, start_idx, I, J, grand_total, n_axes_actual)
            
            # Row and column profiles (for profiles table)
            row_profiles <- P / r  # Each row divided by its marginal
            col_profiles <- t(t(P) / c)  # Each column divided by its marginal
            
            list(
                N = N, n_rows = I, n_cols = J, grand_total = grand_total,
                row_mass = r, col_mass = c,
                row_profiles = row_profiles, col_profiles = col_profiles,
                row_std = row_std, col_std = col_std,
                row_princ = row_princ, col_princ = col_princ,
                row_contrib_coord = row_contrib_coord, col_contrib_coord = col_contrib_coord,
                eigenvalues = eigenvalues, singular_values = d,
                inertia_total = inertia_total,
                inertia_proportion = inertia_proportion, inertia_cumulative = inertia_cumulative,
                n_axes = n_axes_actual,
                test_statistic = test_statistic, stat_name = stat_name,
                distance_type = distance_type, resid_label = resid_label,
                df = df, p_value = p_value,
                row_contrib = row_contrib, col_contrib = col_contrib,
                row_cos2 = row_cos2, col_cos2 = col_cos2,
                malinvaud = malinvaud
            )
        },
        
        # =====================================================================
        # COMPUTE ADJUSTED RESIDUALS FOR CA (Beh, 2012; Beh & Lombardo)
        # =====================================================================
        .computeAdjustedResiduals = function(P, E, r, c, I, J) {
            # Adjusted residuals for CA following Beh & Lombardo's code:
            # adj.x <- dIh %*% solve(sqrt(I - dI)) %*% y %*% solve(sqrt(I - dJ)) %*% dJh
            #
            # This is equivalent to:
            # r_tilde_ij = (p_ij - p_i * p_j) / (sqrt(p_i * p_j) * sqrt((1-p_i)*(1-p_j)))
            #            = pearson_resid / sqrt((1-p_i)*(1-p_j))
            
            adj_resid <- matrix(0, nrow = I, ncol = J)
            
            for (i in 1:I) {
                for (j in 1:J) {
                    # Standard Pearson residual: (p_ij - p_i*p_j) / sqrt(p_i*p_j)
                    pearson_resid <- (P[i, j] - r[i] * c[j]) / sqrt(r[i] * c[j] + 1e-16)
                    
                    # Divide by sqrt((1-p_i)*(1-p_j)) to get adjusted residual
                    variance_factor <- sqrt((1 - r[i]) * (1 - c[j]))
                    
                    adj_resid[i, j] <- pearson_resid / (variance_factor + 1e-16)
                }
            }
            
            adj_resid
        },
        
        # =====================================================================
        # MALINVAUD'S TEST
        # =====================================================================
        .computeMalinvaud = function(eigenvalues_all, start_idx, I, J, grand_total, n_axes) {
            
            if (start_idx == 2) {
                eig <- eigenvalues_all[2:length(eigenvalues_all)]
            } else {
                eig <- eigenvalues_all
            }
            
            n_eig <- min(length(eig), n_axes)
            malin_results <- data.frame(
                dim = integer(n_eig), stat = numeric(n_eig),
                df = integer(n_eig), p_value = numeric(n_eig)
            )
            
            for (k in 1:n_eig) {
                remaining_inertia <- sum(eig[k:length(eig)])
                stat_k <- grand_total * remaining_inertia
                df_k <- (I - k) * (J - k)
                malin_results$dim[k] <- k
                malin_results$stat[k] <- stat_k
                malin_results$df[k] <- df_k
                malin_results$p_value[k] <- if (df_k > 0) {
                    stats::pchisq(stat_k, df_k, lower.tail = FALSE)
                } else {
                    NA
                }
            }
            return(malin_results)
        },
        
        # =====================================================================
        # SRD CLUSTERING (for contribution biplot - uses contribFocus)
        # =====================================================================
        .runSRD = function() {
            # This maintains backward compatibility for the contribution biplot
            # which clusters the "interpreted" variable (opposite of focus)
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                return(private$.srdResultRows)
            } else {
                return(private$.srdResultCols)
            }
        },
        
        # =====================================================================
        # CLUSTERING FOR SPECIFIC VARIABLE (dispatches to appropriate method)
        # =====================================================================
        .runClusteringForVariable = function(target) {
            
            ca <- private$.caResult
            method <- self$options$clusterMethod
            alpha <- self$options$clusterAlpha
            distance_type <- ca$distance_type
            
            # Get the target variable's data
            if (target == "rows") {
                mat <- ca$N
                names_target <- rownames(ca$N)
            } else {
                mat <- t(ca$N)
                names_target <- colnames(ca$N)
            }
            
            # Dispatch to appropriate clustering method
            if (method == "srd") {
                result <- private$.performSRD(mat, alpha = alpha, distance_type = distance_type)
                result$method <- "srd"
            } else {
                result <- private$.performHierarchicalClustering(mat, method = method, distance_type = distance_type)
            }
            
            result$names_target <- names_target
            return(result)
        },
        
        .performSRD = function(mat, alpha = 0.05, distance_type = "chisq") {
            
            n_items <- nrow(mat)
            original_names <- rownames(mat)
            if (is.null(original_names)) {
                original_names <- paste0("R", seq_len(n_items))
                rownames(mat) <- original_names
            }
            
            if (n_items < 2) {
                return(list(
                    groups = list(list(members = original_names, merge_p = NA)),
                    merge_history = NULL,
                    singletons = character(0)
                ))
            }
            
            current_labels <- original_names
            
            merge_history <- data.frame(
                step = integer(), item1 = character(), item2 = character(),
                pValue = numeric(), decision = character(),
                stringsAsFactors = FALSE
            )
            
            group_membership <- as.list(current_labels)
            names(group_membership) <- current_labels
            group_merge_p <- setNames(rep(NA_real_, length(current_labels)), current_labels)
            
            step <- 0
            
            while (nrow(mat) > 1) {
                
                n_current <- nrow(mat)
                best_pair <- NULL
                best_p_value <- 0
                
                for (i in 1:(n_current - 1)) {
                    for (j in (i + 1):n_current) {
                        
                        weight_i <- sum(mat[i, ])
                        weight_j <- sum(mat[j, ])
                        
                        if (weight_i == 0 || weight_j == 0) next
                        
                        profile_i <- mat[i, ] / weight_i
                        profile_j <- mat[j, ] / weight_j
                        col_masses <- colSums(mat) / sum(mat)
                        
                        valid_cols <- col_masses > 0
                        if (sum(valid_cols) < 2) next
                        
                        # Compute distance based on selected metric
                        if (distance_type == "hellinger") {
                            # Hellinger distance (squared)
                            d_squared <- 4 * sum((sqrt(profile_i[valid_cols]) - 
                                                  sqrt(profile_j[valid_cols]))^2)
                        } else {
                            # Chi-squared distance (squared) - applies to both standard and adjusted
                            d_squared <- sum(((profile_i[valid_cols] - profile_j[valid_cols])^2) / 
                                               col_masses[valid_cols])
                        }
                        
                        if (is.na(d_squared) || is.nan(d_squared)) next
                        
                        # Weighted distance for testing
                        weighted_dist <- d_squared * (weight_i * weight_j) / (weight_i + weight_j)
                        
                        if (is.na(weighted_dist) || is.nan(weighted_dist)) next
                        
                        # Both X² and T² are asymptotically chi-squared distributed
                        df_test <- sum(valid_cols) - 1
                        p_val <- 1 - stats::pchisq(weighted_dist, df = df_test)
                        
                        if (p_val > best_p_value) {
                            best_p_value <- p_val
                            best_pair <- c(i, j)
                        }
                    }
                }
                
                if (is.null(best_pair)) break
                
                step <- step + 1
                
                if (best_p_value > alpha) {
                    idx1 <- best_pair[1]
                    idx2 <- best_pair[2]
                    
                    label1 <- rownames(mat)[idx1]
                    label2 <- rownames(mat)[idx2]
                    
                    merged_row <- mat[idx1, , drop = FALSE] + mat[idx2, , drop = FALSE]
                    new_label <- paste0(label1, "+", label2)
                    rownames(merged_row) <- new_label
                    
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
                    group_merge_p[[new_label]] <- best_p_value
                    
                    mat <- mat[-c(idx1, idx2), , drop = FALSE]
                    mat <- rbind(mat, merged_row)
                    
                    merge_history <- rbind(merge_history, data.frame(
                        step = step, item1 = label1, item2 = label2,
                        pValue = best_p_value, decision = "Merged",
                        stringsAsFactors = FALSE
                    ))
                } else {
                    merge_history <- rbind(merge_history, data.frame(
                        step = step,
                        item1 = rownames(mat)[best_pair[1]],
                        item2 = rownames(mat)[best_pair[2]],
                        pValue = best_p_value, decision = "Stopped",
                        stringsAsFactors = FALSE
                    ))
                    break
                }
            }
            
            final_labels <- rownames(mat)
            groups <- list()
            singletons <- character(0)
            
            for (lab in final_labels) {
                members <- if (lab %in% names(group_membership)) {
                    group_membership[[lab]]
                } else {
                    lab
                }
                merge_p <- if (lab %in% names(group_merge_p)) {
                    group_merge_p[[lab]]
                } else {
                    NA
                }
                
                if (length(members) >= 2) {
                    groups[[length(groups) + 1]] <- list(members = members, merge_p = merge_p)
                } else {
                    singletons <- c(singletons, members)
                }
            }
            
            list(groups = groups, merge_history = merge_history, singletons = singletons)
        },
        
        # =====================================================================
        # HIERARCHICAL CLUSTERING (Greenacre / Husson methods)
        # =====================================================================
        .performHierarchicalClustering = function(mat, method = "greenacre", distance_type = "chisq") {
            # Performs hierarchical clustering using Ward's criterion
            # Returns only final cluster membership (no dendrogram)
            #
            # mat: contingency table (rows = items to cluster, cols = variables)
            # method: "greenacre" or "husson"
            # distance_type: "chisq", "chisq_adj", or "hellinger"
            
            # Ensure mat is a proper matrix
            if (!is.matrix(mat)) {
                mat <- as.matrix(mat)
            }
            
            n_items <- nrow(mat)
            original_names <- rownames(mat)
            if (is.null(original_names)) {
                original_names <- paste0("R", seq_len(n_items))
            }
            
            if (n_items < 2) {
                return(list(
                    groups = list(list(members = original_names, merge_stat = NA)),
                    singletons = character(0),
                    method = method
                ))
            }
            
            # Get table dimensions for critical value lookup (Greenacre method)
            table_dims <- dim(mat)
            critical_value <- NA
            
            if (method == "greenacre") {
                critical_value <- tryCatch({
                    private$.getCriticalChiSquare(table_dims[1], table_dims[2])
                }, error = function(e) {
                    NA
                })
                
                if (is.na(critical_value)) {
                    method <- "husson"
                }
            }
            
            # Helper function to compute the association statistic
            # Based on distance_type: chi-squared or Freeman-Tukey (T²)
            compute_statistic <- function(contingency_mat) {
                if (nrow(contingency_mat) < 2 || ncol(contingency_mat) < 2) {
                    return(0)
                }
                
                n <- sum(contingency_mat)
                if (n == 0) return(0)
                
                r <- rowSums(contingency_mat) / n
                c <- colSums(contingency_mat) / n
                P <- contingency_mat / n
                
                if (distance_type == "hellinger") {
                    # Freeman-Tukey statistic: T² = n * Σ(2*(√p_ij - √(r_i*c_j)))²
                    # This is the sum of squared Freeman-Tukey residuals times n
                    stat <- 0
                    for (i in seq_len(nrow(contingency_mat))) {
                        for (j in seq_len(ncol(contingency_mat))) {
                            ft_resid <- 2 * (sqrt(P[i, j]) - sqrt(r[i] * c[j]))
                            stat <- stat + ft_resid^2
                        }
                    }
                    stat <- n * stat
                } else {
                    # Pearson chi-squared statistic
                    expected <- outer(r, c) * n
                    stat <- sum((contingency_mat - expected)^2 / (expected + 1e-10))
                }
                
                return(stat)
            }
            
            # Calculate initial statistic
            stat_init <- compute_statistic(mat)
            
            # Track merge statistics for Husson criterion
            merge_stats <- data.frame(
                step = 0:(n_items - 1),
                statistic = numeric(n_items),
                reduction = numeric(n_items),
                stringsAsFactors = FALSE
            )
            merge_stats$statistic[1] <- stat_init
            merge_stats$reduction[1] <- 0
            
            # Track cluster membership
            cluster_membership <- as.list(original_names)
            names(cluster_membership) <- original_names
            cluster_merge_stat <- setNames(rep(NA_real_, length(original_names)), original_names)
            
            # Working copy of the matrix
            current_mat <- mat
            rownames(current_mat) <- original_names
            current_labels <- original_names
            prev_stat <- stat_init
            
            # For Greenacre: track the step where we should stop
            greenacre_stop_step <- n_items - 1
            
            # Ward-style agglomerative clustering
            for (i in 1:(n_items - 1)) {
                n_current <- nrow(current_mat)
                if (n_current < 2) break
                
                min_reduction <- Inf
                best_pair <- c(NA, NA)
                best_merged_mat <- NULL
                best_merged_labels <- NULL
                best_new_stat <- NULL
                
                # Try all possible pairs
                pairs <- utils::combn(1:n_current, 2)
                for (j in 1:ncol(pairs)) {
                    idx1 <- pairs[1, j]
                    idx2 <- pairs[2, j]
                    
                    # Merge the pair by summing rows
                    merged_row <- current_mat[idx1, , drop = FALSE] + current_mat[idx2, , drop = FALSE]
                    
                    # Build new matrix without the merged rows
                    keep_idx <- setdiff(1:n_current, c(idx1, idx2))
                    if (length(keep_idx) > 0) {
                        temp_mat <- rbind(current_mat[keep_idx, , drop = FALSE], merged_row)
                    } else {
                        temp_mat <- merged_row
                    }
                    
                    merged_label <- paste0("(", current_labels[idx1], "+", current_labels[idx2], ")")
                    temp_labels <- c(current_labels[keep_idx], merged_label)
                    rownames(temp_mat) <- temp_labels
                    
                    # Calculate statistic using the appropriate method
                    new_stat <- compute_statistic(temp_mat)
                    
                    reduction <- prev_stat - new_stat
                    
                    # Keep pair that minimises reduction (Ward criterion)
                    if (reduction < min_reduction) {
                        min_reduction <- reduction
                        best_pair <- c(idx1, idx2)
                        best_merged_mat <- temp_mat
                        best_merged_labels <- temp_labels
                        best_new_stat <- new_stat
                    }
                }
                
                if (is.na(best_pair[1])) break
                
                # Record merge statistics
                merge_stats$statistic[i + 1] <- best_new_stat
                merge_stats$reduction[i + 1] <- min_reduction
                
                # For Greenacre: check if this merge exceeds threshold
                if (method == "greenacre" && !is.na(critical_value)) {
                    if (min_reduction >= critical_value && greenacre_stop_step == n_items - 1) {
                        greenacre_stop_step <- i - 1
                    }
                }
                
                # Update cluster membership
                label1 <- current_labels[best_pair[1]]
                label2 <- current_labels[best_pair[2]]
                new_label <- paste0("(", label1, "+", label2, ")")
                
                members1 <- if (label1 %in% names(cluster_membership)) {
                    cluster_membership[[label1]]
                } else {
                    unlist(strsplit(gsub("[()]", "", label1), "\\+"))
                }
                members2 <- if (label2 %in% names(cluster_membership)) {
                    cluster_membership[[label2]]
                } else {
                    unlist(strsplit(gsub("[()]", "", label2), "\\+"))
                }
                
                cluster_membership[[new_label]] <- c(members1, members2)
                cluster_merge_stat[[new_label]] <- min_reduction
                
                # Update for next iteration
                current_mat <- best_merged_mat
                current_labels <- best_merged_labels
                prev_stat <- best_new_stat
            }
            
            # Determine optimal partition
            if (method == "greenacre") {
                optimal_step <- greenacre_stop_step
            } else {
                husson_result <- private$.findOptimalPartitionHusson(merge_stats)
                optimal_step <- husson_result$optimal_step
            }
            
            # Reconstruct clusters at optimal step
            final_clusters <- private$.reconstructClustersAtStep(
                original_names, mat, optimal_step, method, critical_value, distance_type
            )
            
            final_clusters$method <- method
            return(final_clusters)
        },
        
        # ---------------------------------------------------------------------
        # Find optimal partition using Husson's inertia gain ratio
        # ---------------------------------------------------------------------
        .findOptimalPartitionHusson = function(merge_stats, qmin = 2, qmax = NULL) {
            n_items <- nrow(merge_stats)
            n <- n_items - 1
            
            if (is.null(qmax)) {
                qmax <- min(10, n)
            }
            
            qmin <- max(2, qmin)
            qmax <- min(qmax, n)
            
            if (qmax <= qmin) {
                # step that leaves qmin clusters (n_items items, one merge per step)
                return(list(optimal_clusters = qmin, optimal_step = max(0, n_items - qmin)))
            }
            
            # Compute ratios for each candidate q
            best_ratio <- Inf
            best_q <- qmin
            
            for (q in qmax:(qmin + 1)) {
                step_q <- n - q + 1
                step_q_plus_1 <- n - q
                
                delta_q <- merge_stats$reduction[step_q + 1]
                delta_q_plus_1 <- merge_stats$reduction[step_q_plus_1 + 1]
                
                if (delta_q_plus_1 > 0) {
                    ratio <- delta_q / delta_q_plus_1
                    if (ratio < best_ratio) {
                        best_ratio <- ratio
                        best_q <- q
                    }
                }
            }
            
            list(optimal_clusters = best_q, optimal_step = n_items - best_q)
        },
        
        # ---------------------------------------------------------------------
        # Reconstruct cluster membership at a given step
        # ---------------------------------------------------------------------
        .reconstructClustersAtStep = function(original_names, mat, target_step, method, critical_value, distance_type = "chisq") {
            
            n_items <- length(original_names)
            
            if (target_step <= 0) {
                return(list(
                    groups = list(),
                    singletons = original_names,
                    method = method
                ))
            }
            
            # Ensure mat is a proper matrix
            if (!is.matrix(mat)) {
                mat <- as.matrix(mat)
            }
            
            # Helper function to compute the association statistic
            compute_statistic <- function(contingency_mat) {
                if (nrow(contingency_mat) < 2 || ncol(contingency_mat) < 2) {
                    return(0)
                }
                
                n <- sum(contingency_mat)
                if (n == 0) return(0)
                
                r <- rowSums(contingency_mat) / n
                c <- colSums(contingency_mat) / n
                P <- contingency_mat / n
                
                if (distance_type == "hellinger") {
                    stat <- 0
                    for (i in seq_len(nrow(contingency_mat))) {
                        for (j in seq_len(ncol(contingency_mat))) {
                            ft_resid <- 2 * (sqrt(P[i, j]) - sqrt(r[i] * c[j]))
                            stat <- stat + ft_resid^2
                        }
                    }
                    stat <- n * stat
                } else {
                    expected <- outer(r, c) * n
                    stat <- sum((contingency_mat - expected)^2 / (expected + 1e-10))
                }
                
                return(stat)
            }
            
            # Replay merging up to target_step
            cluster_membership <- as.list(original_names)
            names(cluster_membership) <- original_names
            cluster_merge_stat <- setNames(rep(NA_real_, length(original_names)), original_names)
            
            current_mat <- mat
            rownames(current_mat) <- original_names
            current_labels <- original_names
            prev_stat <- compute_statistic(mat)
            
            for (i in 1:target_step) {
                n_current <- nrow(current_mat)
                if (n_current < 2) break
                
                min_reduction <- Inf
                best_pair <- c(NA, NA)
                best_merged_mat <- NULL
                best_merged_labels <- NULL
                best_new_stat <- NULL
                
                pairs <- utils::combn(1:n_current, 2)
                for (j in 1:ncol(pairs)) {
                    idx1 <- pairs[1, j]
                    idx2 <- pairs[2, j]
                    
                    merged_row <- current_mat[idx1, , drop = FALSE] + current_mat[idx2, , drop = FALSE]
                    
                    keep_idx <- setdiff(1:n_current, c(idx1, idx2))
                    if (length(keep_idx) > 0) {
                        temp_mat <- rbind(current_mat[keep_idx, , drop = FALSE], merged_row)
                    } else {
                        temp_mat <- merged_row
                    }
                    
                    merged_label <- paste0("(", current_labels[idx1], "+", current_labels[idx2], ")")
                    temp_labels <- c(current_labels[keep_idx], merged_label)
                    rownames(temp_mat) <- temp_labels
                    
                    new_stat <- compute_statistic(temp_mat)
                    
                    reduction <- prev_stat - new_stat
                    
                    if (reduction < min_reduction) {
                        min_reduction <- reduction
                        best_pair <- c(idx1, idx2)
                        best_merged_mat <- temp_mat
                        best_merged_labels <- temp_labels
                        best_new_stat <- new_stat
                    }
                }
                
                if (is.na(best_pair[1])) break
                
                label1 <- current_labels[best_pair[1]]
                label2 <- current_labels[best_pair[2]]
                new_label <- paste0("(", label1, "+", label2, ")")
                
                members1 <- if (label1 %in% names(cluster_membership)) {
                    cluster_membership[[label1]]
                } else {
                    unlist(strsplit(gsub("[()]", "", label1), "\\+"))
                }
                members2 <- if (label2 %in% names(cluster_membership)) {
                    cluster_membership[[label2]]
                } else {
                    unlist(strsplit(gsub("[()]", "", label2), "\\+"))
                }
                
                cluster_membership[[new_label]] <- c(members1, members2)
                cluster_merge_stat[[new_label]] <- min_reduction
                
                current_mat <- best_merged_mat
                current_labels <- best_merged_labels
                prev_stat <- best_new_stat
            }
            
            # Extract final groups from current_labels
            groups <- list()
            singletons <- character(0)
            
            for (lab in current_labels) {
                members <- if (lab %in% names(cluster_membership)) {
                    cluster_membership[[lab]]
                } else {
                    lab
                }
                merge_stat <- if (lab %in% names(cluster_merge_stat)) {
                    cluster_merge_stat[[lab]]
                } else {
                    NA
                }
                
                if (length(members) >= 2) {
                    groups[[length(groups) + 1]] <- list(members = members, merge_stat = merge_stat)
                } else {
                    singletons <- c(singletons, members)
                }
            }
            
            list(groups = groups, singletons = singletons, method = method)
        },
        
        # =====================================================================
        # ATTRIBUTE ASSESSMENT (Krzanowski 1993)
        # =====================================================================
        # Implements Procrustes-based attribute selection to identify which
        # columns (attributes) contribute most to the row configuration.
        # Reference: Krzanowski, W.J. (1993). Attribute selection in correspondence
        #   analysis of incidence matrices. Applied Statistics, 42(3), 529-541.
        # =====================================================================
        
        # ---------------------------------------------------------------------
        # Lightweight CA for Procrustes comparison
        # ---------------------------------------------------------------------
        .computeCAForProcrustes = function(N, n_dims = NULL) {
            
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            I <- nrow(N)
            J <- ncol(N)
            
            if (I < 2 || J < 2) return(NULL)
            
            grand_total <- sum(N)
            if (grand_total == 0) return(NULL)
            
            P <- N / grand_total
            r <- rowSums(P)
            c <- colSums(P)
            
            if (any(r == 0) || any(c == 0)) return(NULL)
            
            r_mat <- matrix(r, nrow = I, ncol = J, byrow = FALSE)
            c_mat <- matrix(c, nrow = I, ncol = J, byrow = TRUE)
            E <- r_mat * c_mat
            
            # Compute residuals based on selected metric and type
            if (distance_metric == "freemanTukey") {
                S <- 2 * (sqrt(P) - sqrt(E))
                distance_type <- "hellinger"
            } else if (residual_type == "adjusted") {
                S <- private$.computeAdjustedResiduals(P, E, r, c, I, J)
                distance_type <- "chisq_adj"
            } else {
                S <- (P - E) / sqrt(E + 1e-16)
                distance_type <- "chisq"
            }
            
            if (any(!is.finite(S))) return(NULL)
            
            svd_result <- tryCatch(svd(S), error = function(e) NULL)
            if (is.null(svd_result)) return(NULL)
            
            d_all <- svd_result$d
            eigenvalues_all <- d_all^2
            
            # Starting index: residual matrix is centred, so no trivial axis
            start_idx <- 1
            
            # Determine dimensionality
            max_axes <- min(I, J) - 1
            if (is.null(n_dims)) {
                # Auto: capture >= 80% of inertia, minimum 2
                max_axes_for_inertia <- min(I - 1, J - 1)
                if (start_idx == 2 && length(eigenvalues_all) > 1) {
                    end_idx <- min(start_idx + max_axes_for_inertia - 1, length(eigenvalues_all))
                    all_nontrivial <- eigenvalues_all[start_idx:end_idx]
                } else {
                    end_idx <- min(max_axes_for_inertia, length(eigenvalues_all))
                    all_nontrivial <- eigenvalues_all[1:end_idx]
                }
                inertia_total <- sum(all_nontrivial)
                
                if (inertia_total > 0) {
                    cum_inertia <- cumsum(all_nontrivial) / inertia_total
                    n_dims <- which(cum_inertia >= 0.80)[1]
                    if (is.na(n_dims)) n_dims <- length(all_nontrivial)
                    n_dims <- max(n_dims, ifelse(max_axes >= 3, 3, 2))
                } else {
                    n_dims <- 2
                }
            }
            n_dims <- min(n_dims, max_axes)
            
            axes_idx <- start_idx:(start_idx + n_dims - 1)
            axes_idx <- axes_idx[axes_idx <= length(d_all)]
            
            if (length(axes_idx) == 0) return(NULL)
            
            d <- d_all[axes_idx]
            U <- svd_result$u[, axes_idx, drop = FALSE]
            n_axes_actual <- length(d)
            eigenvalues <- d^2
            
            # Row principal coordinates
            Dr_inv_sqrt <- diag(1 / sqrt(r))
            row_std <- Dr_inv_sqrt %*% U
            row_princ <- row_std %*% diag(d, nrow = length(d))
            rownames(row_princ) <- rownames(N)
            
            # Total inertia
            max_axes_for_inertia <- min(I - 1, J - 1)
            if (start_idx == 2 && length(eigenvalues_all) > 1) {
                end_idx <- min(start_idx + max_axes_for_inertia - 1, length(eigenvalues_all))
                all_nontrivial <- eigenvalues_all[start_idx:end_idx]
            } else {
                end_idx <- min(max_axes_for_inertia, length(eigenvalues_all))
                all_nontrivial <- eigenvalues_all[1:end_idx]
            }
            inertia_total <- sum(all_nontrivial)
            inertia_captured <- sum(eigenvalues) / inertia_total
            
            list(
                row_princ = row_princ,
                row_mass = r,
                eigenvalues = eigenvalues,
                inertia_total = inertia_total,
                inertia_captured = inertia_captured,
                n_dims = n_axes_actual,
                distance_type = distance_type
            )
        },
        
        # ---------------------------------------------------------------------
        # Weighted Procrustes M² (Krzanowski 1988, pp. 158-160)
        # ---------------------------------------------------------------------
        .computeProcrustesM2 = function(X, Y, weights = NULL, scale = TRUE) {
            
            r <- nrow(X)
            m <- ncol(X)
            
            if (nrow(Y) != r) return(Inf)
            
            # Match dimensions
            if (ncol(Y) < m) {
                Y <- cbind(Y, matrix(0, nrow = r, ncol = m - ncol(Y)))
            } else if (ncol(Y) > m) {
                Y <- Y[, 1:m, drop = FALSE]
            }
            
            # Set up weights
            if (is.null(weights)) {
                w <- rep(1 / r, r)
            } else {
                w <- weights / sum(weights)
            }
            
            # Safe diagonal construction
            D <- if (length(w) == 1) matrix(w, 1, 1) else diag(w)
            
            # Centre both configurations (weighted centroid at origin)
            centroid_X <- crossprod(w, X)
            centroid_Y <- crossprod(w, Y)
            
            X_c <- sweep(X, 2, centroid_X)
            Y_c <- sweep(Y, 2, centroid_Y)
            
            # Rescale so that weighted SS = 1 (if requested)
            if (scale) {
                ss_X <- sum(diag(t(X_c) %*% D %*% X_c))
                ss_Y <- sum(diag(t(Y_c) %*% D %*% Y_c))
                
                if (ss_X > 1e-12) X_c <- X_c / sqrt(ss_X)
                if (ss_Y > 1e-12) Y_c <- Y_c / sqrt(ss_Y)
            }
            
            # Optimal rotation via SVD of X'DY
            cross <- t(X_c) %*% D %*% Y_c
            svd_cross <- tryCatch(svd(cross), error = function(e) NULL)
            if (is.null(svd_cross)) return(Inf)
            
            Q <- svd_cross$v %*% t(svd_cross$u)
            
            # Rotate Y and compute M²
            Y_rotated <- Y_c %*% Q
            residuals <- X_c - Y_rotated
            M2 <- sum(diag(t(residuals) %*% D %*% residuals))
            
            M2
        },
        
        # ---------------------------------------------------------------------
        # Single-pass attribute influence ranking
        # ---------------------------------------------------------------------
        .assessAttributeInfluence = function(N) {
            
            c_total <- ncol(N)
            
            # Reference CA on full matrix
            ca_ref <- private$.computeCAForProcrustes(N)
            if (is.null(ca_ref)) return(NULL)
            
            X_ref <- ca_ref$row_princ
            p_i_full <- ca_ref$row_mass
            m <- ca_ref$n_dims
            
            # Compute M² for each column deletion
            M2_values <- numeric(c_total)
            names(M2_values) <- colnames(N)
            
            for (j in 1:c_total) {
                N_reduced <- N[, -j, drop = FALSE]
                
                # Check for zero-sum rows after deletion
                if (any(rowSums(N_reduced) == 0) || any(colSums(N_reduced) == 0)) {
                    M2_values[j] <- Inf
                    next
                }
                
                ca_reduced <- private$.computeCAForProcrustes(N_reduced, n_dims = m)
                
                if (is.null(ca_reduced)) {
                    M2_values[j] <- Inf
                    next
                }
                
                Y_j <- ca_reduced$row_princ
                
                # Pad with zeros if fewer dimensions
                if (ncol(Y_j) < m) {
                    Y_j <- cbind(Y_j, matrix(0, nrow = nrow(Y_j), ncol = m - ncol(Y_j)))
                }
                
                M2_values[j] <- private$.computeProcrustesM2(
                    X_ref, Y_j, 
                    weights = p_i_full, 
                    scale = TRUE
                )
            }
            
            # Rank: largest M² = most important (removing it distorts most)
            influence_order <- order(M2_values, decreasing = TRUE)
            
            data.frame(
                attribute = colnames(N)[influence_order],
                M2 = M2_values[influence_order],
                rank = 1:c_total,
                stringsAsFactors = FALSE
            )
        },
        
        # ---------------------------------------------------------------------
        # Backward elimination sequence (computationally expensive)
        # ---------------------------------------------------------------------
        .assessAttributeElimination = function(N) {
            
            c_total <- ncol(N)
            r_total <- nrow(N)
            
            # Reference CA for full data
            ca_full <- private$.computeCAForProcrustes(N)
            if (is.null(ca_full)) return(NULL)
            
            m <- ca_full$n_dims
            
            # Minimum columns to retain
            min_cols <- max(2, m)
            n_steps <- c_total - min_cols
            
            if (n_steps < 1) return(NULL)
            
            remaining_cols <- 1:c_total
            col_names <- colnames(N)
            
            elimination_sequence <- data.frame(
                step = integer(),
                attribute_removed = character(),
                M2_at_removal = numeric(),
                n_remaining = integer(),
                stringsAsFactors = FALSE
            )
            
            for (step in 1:n_steps) {
                current_N <- N[, remaining_cols, drop = FALSE]
                n_current <- ncol(current_N)
                
                # Reference CA for current reduced matrix
                ca_current <- private$.computeCAForProcrustes(current_N, n_dims = m)
                if (is.null(ca_current)) break
                
                X_current <- ca_current$row_princ
                p_i_current <- ca_current$row_mass
                m_current <- ncol(X_current)
                
                # Compute M² for each remaining column
                M2_step <- numeric(n_current)
                for (jj in 1:n_current) {
                    N_minus_j <- current_N[, -jj, drop = FALSE]
                    
                    if (any(rowSums(N_minus_j) == 0) || any(colSums(N_minus_j) == 0)) {
                        M2_step[jj] <- Inf
                        next
                    }
                    
                    ca_minus_j <- private$.computeCAForProcrustes(N_minus_j, n_dims = m_current)
                    
                    if (is.null(ca_minus_j)) {
                        M2_step[jj] <- Inf
                        next
                    }
                    
                    Y_j <- ca_minus_j$row_princ
                    if (ncol(Y_j) < m_current) {
                        Y_j <- cbind(Y_j, matrix(0, nrow = nrow(Y_j), ncol = m_current - ncol(Y_j)))
                    }
                    
                    M2_step[jj] <- private$.computeProcrustesM2(
                        X_current, Y_j,
                        weights = p_i_current,
                        scale = TRUE
                    )
                }
                
                # All indispensable?
                if (all(is.infinite(M2_step))) break
                
                # Remove the least important column
                least_important <- which.min(M2_step)
                removed_col_idx <- remaining_cols[least_important]
                
                elimination_sequence <- rbind(elimination_sequence, data.frame(
                    step = step,
                    attribute_removed = col_names[removed_col_idx],
                    M2_at_removal = M2_step[least_important],
                    n_remaining = n_current - 1,
                    stringsAsFactors = FALSE
                ))
                
                remaining_cols <- remaining_cols[-least_important]
            }
            
            elimination_sequence
        },
        
        # ---------------------------------------------------------------------
        # Elbow detection for elimination sequence
        # ---------------------------------------------------------------------
        .detectElbow = function(M2_values) {
            
            n <- length(M2_values)
            if (n < 3) return(max(1, n - 1))
            
            # Normalise to [0, 1] range
            x <- seq(0, 1, length.out = n)
            y_range <- range(M2_values)
            
            if (diff(y_range) < 1e-15) return(n %/% 2)
            
            y <- (M2_values - y_range[1]) / diff(y_range)
            
            # Distance of each point to line from first to last point
            x1 <- x[1]; y1 <- y[1]
            x2 <- x[n]; y2 <- y[n]
            
            line_len <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
            if (line_len < 1e-15) return(n %/% 2)
            
            distances <- abs((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1) / line_len
            
            which.max(distances)
        },
        
        # ---------------------------------------------------------------------
        # Get list of low-influence attributes (those removed before elbow)
        # ---------------------------------------------------------------------
        .getLowInfluenceAttributes = function() {
            
            if (is.null(private$.attrElimination) || 
                nrow(private$.attrElimination) == 0 ||
                is.null(private$.attrElbowStep)) {
                return(character(0))
            }
            
            elbow_step <- private$.attrElbowStep
            if (elbow_step < 1) return(character(0))
            
            # Attributes removed in steps 1 to elbow_step are "low influence"
            n_to_drop <- min(elbow_step, nrow(private$.attrElimination))
            private$.attrElimination$attribute_removed[1:n_to_drop]
        },
        
        # ---------------------------------------------------------------------
        # Get critical chi-square value (Greenacre method)
        # ---------------------------------------------------------------------
        .getCriticalChiSquare = function(nrow, ncol) {
          # Pearson & Hartley (1972, Table 51) critical values, alpha = 0.05;
          # single shared copy in utils.R (also used by the Clustering facility)
          .ctGreenacreCritical(nrow, ncol)
        },
        
        # =====================================================================
        # POPULATE SUMMARY TABLE
        # =====================================================================
        .populateSummaryTable = function() {
            ca <- private$.caResult
            table <- self$results$summaryTable
            
            table$setRow(rowNo = 1, values = list(
                statistic = ca$test_statistic, 
                df = ca$df, 
                pvalue = ca$p_value,
                totalInertia = ca$inertia_total, 
                nRows = ca$n_rows, 
                nCols = ca$n_cols
            ))
            
            # Set column title based on statistic type
            table$getColumn("statistic")$setTitle(ca$stat_name)
            
            # Add note about distance/residual type
            if (ca$distance_type == "hellinger") {
                table$setNote("distNote", 
                    "Freeman-Tukey residuals (Hellinger distance). T\u00b2 is asymptotically \u03c7\u00b2-distributed.")
            } else if (ca$distance_type == "chisq_adj") {
                table$setNote("distNote", 
                    "Adjusted standardised residuals (\u03c7\u00b2 distance with marginal correction).")
            } else {
                table$setNote("distNote", 
                    "Pearson residuals (\u03c7\u00b2 distance).")
            }
        },
        
        # =====================================================================
        # POPULATE OBSERVED CONTINGENCY TABLE (with marginals)
        # =====================================================================
        .populateObservedTable = function() {
            
            N <- private$.contingencyTable
            if (is.null(N)) return()
            
            table <- self$results$observedTable
            
            row_names <- rownames(N)
            col_names <- colnames(N)
            I <- nrow(N)
            J <- ncol(N)
            
            # Calculate marginals
            row_totals <- rowSums(N)
            col_totals <- colSums(N)
            grand_total <- sum(N)
            
            # Update data rows using setRow (structure already built in .init)
            for (i in seq_len(I)) {
                values <- list(rowname = row_names[i])
                for (j in seq_len(J)) {
                    values[[paste0("col", j)]] <- as.integer(N[i, j])
                }
                values$rowtotal <- as.integer(row_totals[i])
                
                table$setRow(rowKey = paste0("row", i), values = values)
            }
            
            # Update column totals row
            total_values <- list(rowname = "Total")
            for (j in seq_len(J)) {
                total_values[[paste0("col", j)]] <- as.integer(col_totals[j])
            }
            total_values$rowtotal <- as.integer(grand_total)
            
            table$setRow(rowKey = "total", values = total_values)
        },

        # =====================================================================
        # POPULATE SERIATED CONTINGENCY TABLE
        # =====================================================================
        .populateSeriatedTable = function() {
            
            ca <- private$.caResult
            N <- private$.contingencyTable
            if (is.null(ca) || is.null(N)) return()
            
            table <- self$results$seriatedTable
            
            # Get the dimensions from symmetric plot settings
            dim1 <- self$options$dim1  # X-axis (primary sort)
            dim2 <- self$options$dim2  # Y-axis (secondary sort)
            
            # Validate dimensions exist
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                maxDim <- max(dim1, dim2)
                table$setNote("dimError", 
                              paste0("Dimension ", maxDim, " not available. Only ", 
                                     ca$n_axes, " dimensions extracted."))
                return()
            }
            
            # Extract principal coordinates
            rowCoords1 <- ca$row_princ[, dim1]
            rowCoords2 <- ca$row_princ[, dim2]
            colCoords1 <- ca$col_princ[, dim1]
            colCoords2 <- ca$col_princ[, dim2]
            
            # Get row and column names
            rowNames <- rownames(ca$row_princ)
            colNames <- rownames(ca$col_princ)
            
            I <- length(rowNames)
            J <- length(colNames)
            
            # ---------------------------------------------------------------------
            # Sort by Dim1 (X-axis) coordinates only, ascending (negative to positive)
            # ---------------------------------------------------------------------
            rowOrder <- order(rowCoords1)
            colOrder <- order(colCoords1)
            
            # Reorder the contingency table
            seriatedData <- N[rowOrder, colOrder, drop = FALSE]
            
            # Sorted coordinate values and names
            sortedRowCoords1 <- rowCoords1[rowOrder]
            sortedRowCoords2 <- rowCoords2[rowOrder]
            sortedColCoords1 <- colCoords1[colOrder]
            sortedColCoords2 <- colCoords2[colOrder]
            sortedRowNames <- rowNames[rowOrder]
            sortedColNames <- colNames[colOrder]
            
            # Get variable names for labelling
            rowVar <- self$options$rows
            colVar <- self$options$cols
            
            # Build table columns dynamically
            # First column: Row category
            table$addColumn(
                name = "rowCategory",
                title = rowVar,
                type = "text"
            )
            
            # Add columns for each (sorted) column category - use NUMBER type for flexibility
            for (j in seq_along(sortedColNames)) {
                table$addColumn(
                    name = paste0("col_", j),
                    title = sortedColNames[j],
                    type = "number",
                    superTitle = colVar
                )
            }
            
            # Add row coordinate columns (marginals) - rightmost
            table$addColumn(
                name = "rowCoord1",
                title = paste0("Dim ", dim1),
                type = "number",
                format = "zto"
            )
            table$addColumn(
                name = "rowCoord2",
                title = paste0("Dim ", dim2),
                type = "number",
                format = "zto"
            )
            
            # Populate data rows
            for (i in seq_along(sortedRowNames)) {
                rowData <- list(
                    rowCategory = sortedRowNames[i]
                )
                
                # Add cell values (as integers)
                for (j in seq_along(sortedColNames)) {
                    rowData[[paste0("col_", j)]] <- as.integer(seriatedData[i, j])
                }
                
                # Add row coordinates (marginals)
                rowData$rowCoord1 <- sortedRowCoords1[i]
                rowData$rowCoord2 <- sortedRowCoords2[i]
                
                table$addRow(rowKey = i, values = rowData)
            }
            
            # Add column coordinates rows (marginals) - bottom rows
            # First: Dim 1 coordinates
            coordRow1Data <- list(
                rowCategory = paste0("Dim ", dim1)
            )
            for (j in seq_along(sortedColNames)) {
                coordRow1Data[[paste0("col_", j)]] <- sortedColCoords1[j]
            }
            coordRow1Data$rowCoord1 <- NA
            coordRow1Data$rowCoord2 <- NA
            
            table$addRow(rowKey = I + 1, values = coordRow1Data)
            
            # Format the first coordinates row to stand out
            table$addFormat(
                rowKey = I + 1,
                col = "rowCategory",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            # Second: Dim 2 coordinates
            coordRow2Data <- list(
                rowCategory = paste0("Dim ", dim2)
            )
            for (j in seq_along(sortedColNames)) {
                coordRow2Data[[paste0("col_", j)]] <- sortedColCoords2[j]
            }
            coordRow2Data$rowCoord1 <- NA
            coordRow2Data$rowCoord2 <- NA
            
            table$addRow(rowKey = I + 2, values = coordRow2Data)
            
            # Add footnote explaining the ordering
            table$setNote(
                "seriation",
                paste0("Rows and columns ordered by Dimension ", dim1, 
                       " (X-axis) coordinates, from most negative to most positive. ",
                       "Marginals show principal coordinates for Dimensions ", dim1, " and ", dim2, ".")
            )
            
            # Set table title
            table$setTitle(paste0("Seriated Contingency Table: ", rowVar, " \u00d7 ", colVar))
        },
        
        # =====================================================================
        # POPULATE INERTIA TABLE
        # =====================================================================
        .populateInertiaTable = function() {
            ca <- private$.caResult
            table <- self$results$inertiaTable
            table$deleteRows()
            for (k in 1:ca$n_axes) {
                table$addRow(rowKey = k, values = list(
                    dim = k, eigenvalue = ca$eigenvalues[k],
                    percent = ca$inertia_proportion[k] * 100,
                    cumPercent = ca$inertia_cumulative[k] * 100,
                    malinChi = ca$malinvaud$stat[k],
                    malinDf = ca$malinvaud$df[k],
                    malinP = ca$malinvaud$p_value[k]
                ))
            }
        },
        
        # =====================================================================
        # POPULATE SYMMETRIC COORDINATES TABLE
        # =====================================================================
        .populateSymmetricCoordsTable = function() {
            ca <- private$.caResult
            table <- self$results$symmetricCoordsTable
            nAxes <- ca$n_axes
            
            # Row categories - principal coordinates
            rowNames <- rownames(ca$row_princ)
            rowKey <- 1
            for (i in seq_along(rowNames)) {
                values <- list(
                    category = rowNames[i],
                    type = "Row"
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- ca$row_princ[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
            
            # Column categories - principal coordinates
            colNames <- rownames(ca$col_princ)
            for (i in seq_along(colNames)) {
                values <- list(
                    category = colNames[i],
                    type = "Column"
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- ca$col_princ[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
        },
        
        # =====================================================================
        # POPULATE CONTRIBUTION BIPLOT COORDINATES TABLE
        # =====================================================================
        .populateContribCoordsTable = function() {
            ca <- private$.caResult
            table <- self$results$contribCoordsTable
            nAxes <- ca$n_axes
            focusOn <- self$options$contribFocus
            
            # Determine which variable defines the plane vs is interpreted
            if (focusOn == "cols") {
                # Columns define plane (standard coords), rows interpreted (principal coords)
                focusCoords <- ca$col_std
                focusNames <- rownames(ca$col_std)
                focusType <- "Column"
                focusCoordType <- "Standard"
                
                interpCoords <- ca$row_princ
                interpNames <- rownames(ca$row_princ)
                interpType <- "Row"
                interpCoordType <- "Principal"
            } else {
                # Rows define plane (standard coords), columns interpreted (principal coords)
                focusCoords <- ca$row_std
                focusNames <- rownames(ca$row_std)
                focusType <- "Row"
                focusCoordType <- "Standard"
                
                interpCoords <- ca$col_princ
                interpNames <- rownames(ca$col_princ)
                interpType <- "Column"
                interpCoordType <- "Principal"
            }
            
            # Set table note explaining coordinate types
            table$setNote(
                "coordNote",
                paste0(
                    focusType, " categories (plane-defining): ", focusCoordType, " coordinates; ",
                    interpType, " categories (interpreted): ", interpCoordType, " coordinates"
                )
            )
            
            rowKey <- 1
            
            # Plane-defining categories (standard coordinates)
            for (i in seq_along(focusNames)) {
                values <- list(
                    category = focusNames[i],
                    type = focusType,
                    coordType = focusCoordType
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- focusCoords[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
            
            # Interpreted categories (principal coordinates)
            for (i in seq_along(interpNames)) {
                values <- list(
                    category = interpNames[i],
                    type = interpType,
                    coordType = interpCoordType
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- interpCoords[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
        },
        
        # =====================================================================
        # POPULATE QUALITY & CONTRIBUTIONS TABLE
        # =====================================================================
        .populateQualityContribTable = function() {
            ca <- private$.caResult
            table <- self$results$qualityContribTable
            
            nAxes <- ca$n_axes
            
            # Combine row and column data
            row_names <- rownames(ca$N)
            col_names <- colnames(ca$N)
            
            n_rows <- length(row_names)
            n_cols <- length(col_names)
            
            # Build data frame with all categories
            all_data <- data.frame(
                category = c(row_names, col_names),
                type = c(rep("Row", n_rows), rep("Column", n_cols)),
                stringsAsFactors = FALSE
            )
            
            # Add cos² and contributions for each dimension
            for (k in 1:nAxes) {
                all_data[[paste0("cos2_", k)]] <- c(ca$row_cos2[, k], ca$col_cos2[, k])
                all_data[[paste0("contrib_", k)]] <- c(ca$row_contrib[, k], ca$col_contrib[, k])
            }
            
            # Compute total quality (sum of cos² across displayed dimensions)
            all_data$totalQuality <- rowSums(all_data[, grep("^cos2_", names(all_data)), drop = FALSE])
            
            # Sort by Type (Row first, then Column), then by Total Quality descending within each type
            all_data <- all_data[order(all_data$type, -all_data$totalQuality), ]
            
            # Populate table
            for (i in seq_len(nrow(all_data))) {
                values <- list(
                    category = all_data$category[i],
                    type = all_data$type[i]
                )
                
                for (k in 1:nAxes) {
                    values[[paste0("cos2_", k)]] <- all_data[[paste0("cos2_", k)]][i]
                    values[[paste0("contrib_", k)]] <- all_data[[paste0("contrib_", k)]][i]
                }
                
                values$totalQuality <- all_data$totalQuality[i]
                
                table$addRow(rowKey = i, values = values)
            }
            
            table$setNote(
                "qualityNote",
                paste0(
                    "Quality (cos\u00b2) indicates how well a category is represented in each dimension. ",
                    "Contribution % indicates how much a category contributes to defining each dimension. ",
                    "Sorted by Total Quality (descending)."
                )
            )
        },
        
        # =====================================================================
        # POPULATE DIMENSION DEFINITION TABLE (all contributors, sorted)
        # =====================================================================
        .populateDimDefTable = function() {
            ca <- private$.caResult
            table <- self$results$dimDefTable
            table$deleteRows()
            
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                contrib <- ca$col_contrib
                princ <- ca$col_princ
                names_vec <- colnames(ca$N)
                n_cats <- ca$n_cols
            } else {
                contrib <- ca$row_contrib
                princ <- ca$row_princ
                names_vec <- rownames(ca$N)
                n_cats <- ca$n_rows
            }
            
            avg_contrib <- 100 / n_cats
            row_key <- 0
            
            for (dim_idx in 1:ca$n_axes) {
                
                dim_contrib <- contrib[, dim_idx]
                dim_coord <- princ[, dim_idx]
                
                # Sort ALL categories by contribution (decreasing)
                sorted_idx <- order(dim_contrib, decreasing = TRUE)
                
                cumul <- 0
                for (idx in sorted_idx) {
                    row_key <- row_key + 1
                    cumul <- cumul + dim_contrib[idx]
                    
                    direction <- if (dim_coord[idx] > 0) "+" else "\u2212"
                    
                    table$addRow(rowKey = row_key, values = list(
                        dimension = paste("Dim", dim_idx),
                        category = names_vec[idx],
                        contrib = dim_contrib[idx],
                        cumulContrib = cumul,
                        direction = direction
                    ))
                    
                    # Highlight above-average contributors (guard against NA)
                    if (!is.na(dim_contrib[idx]) && dim_contrib[idx] >= avg_contrib) {
                        table$addFormat(
                            rowKey = row_key,
                            col = "contrib",
                            format = jmvcore::Cell.NEGATIVE
                        )
                    }
                }
            }
            
            # Add note with average contribution threshold and variable clarification
            focus_label <- if (focus == "cols") "column" else "row"
            table$setNote('avgNote', sprintf(
                "Showing %s categories (plane-defining variable in Contribution Biplot). Contributions \u2265 average (%.2f%%) are highlighted.",
                focus_label, avg_contrib
            ))
        },
        
        # =====================================================================
        # POPULATE DIMENSION SUMMARY TABLE (pole-centric)
        # =====================================================================
        .populateDimSummaryTable = function() {
            ca <- private$.caResult
            table <- self$results$dimSummaryTable
            table$deleteRows()
            
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                contrib <- ca$col_contrib
                princ <- ca$col_princ
                names_vec <- colnames(ca$N)
                n_cats <- ca$n_cols
            } else {
                contrib <- ca$row_contrib
                princ <- ca$row_princ
                names_vec <- rownames(ca$N)
                n_cats <- ca$n_rows
            }
            
            avg_contrib <- 100 / n_cats
            
            for (dim_idx in 1:ca$n_axes) {
                
                dim_contrib <- contrib[, dim_idx]
                dim_coord <- princ[, dim_idx]
                
                # Identify above-average contributors
                above_avg <- !is.na(dim_contrib) & (dim_contrib >= avg_contrib)
                
                # Split by sign
                pos_idx <- which(above_avg & dim_coord > 0)
                neg_idx <- which(above_avg & dim_coord < 0)
                
                # Sort each group by contribution (descending)
                pos_idx <- pos_idx[order(dim_contrib[pos_idx], decreasing = TRUE)]
                neg_idx <- neg_idx[order(dim_contrib[neg_idx], decreasing = TRUE)]
                
                # Build strings with contributions
                pos_str <- if (length(pos_idx) > 0) {
                    paste(sapply(pos_idx, function(i) {
                        sprintf("%s (%.1f%%)", names_vec[i], dim_contrib[i])
                    }), collapse = ", ")
                } else {
                    "\u2014"
                }
                
                neg_str <- if (length(neg_idx) > 0) {
                    paste(sapply(neg_idx, function(i) {
                        sprintf("%s (%.1f%%)", names_vec[i], dim_contrib[i])
                    }), collapse = ", ")
                } else {
                    "\u2014"
                }
                
                table$addRow(rowKey = dim_idx, values = list(
                    dimension = paste("Dim", dim_idx),
                    variance = ca$inertia_proportion[dim_idx] * 100,
                    negativePole = neg_str,
                    positivePole = pos_str
                ))
            }
            
            table$setNote('avgNote', sprintf(
                "Only categories with contributions \u2265 average (%.2f%%) are shown. This table supports Contribution Biplot interpretation.",
                avg_contrib
            ))
        },
        
        # =====================================================================
        # POPULATE PROFILES TABLE (interpreted categories)
        # =====================================================================
        .populateProfilesTable = function() {
            ca <- private$.caResult
            table <- self$results$profilesTable
            
            focus <- self$options$contribFocus
            
            # Get the INTERPRETED variable's profiles (opposite of focus)
            if (focus == "cols") {
                profiles <- ca$row_profiles
                masses <- ca$row_mass
                names_vec <- rownames(ca$N)
                col_names <- colnames(ca$N)
                n_cats <- ca$n_rows
                table_title <- "Row Profiles (Interpreted Categories)"
            } else {
                profiles <- t(ca$col_profiles)
                masses <- ca$col_mass
                names_vec <- colnames(ca$N)
                col_names <- rownames(ca$N)
                n_cats <- ca$n_cols
                table_title <- "Column Profiles (Interpreted Categories)"
            }
            
            table$setTitle(table_title)
            
            for (j in seq_along(col_names)) {
                table$addColumn(
                    name = paste0("prof", j),
                    title = col_names[j],
                    type = "number",
                    format = "zto"
                )
            }
            
            for (i in seq_len(n_cats)) {
                values <- list(
                    category = names_vec[i],
                    mass = masses[i]
                )
                
                for (j in seq_along(col_names)) {
                    values[[paste0("prof", j)]] <- profiles[i, j]
                }
                
                table$addRow(rowKey = i, values = values)
            }
            
            if (focus == "cols") {
                marginal <- ca$col_mass
            } else {
                marginal <- ca$row_mass
            }
            
            marginal_values <- list(
                category = "Average (marginal)",
                mass = NA
            )
            for (j in seq_along(col_names)) {
                marginal_values[[paste0("prof", j)]] <- marginal[j]
            }
            
            table$addRow(rowKey = n_cats + 1, values = marginal_values)
            
            table$addFormat(
                rowKey = n_cats + 1,
                col = "category",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            table$setNote('profileNote', 
                "Profiles show conditional distributions. Average (marginal) profile is the centroid.")
        },
        
        # =====================================================================
        # POPULATE RESIDUAL MATRIX TABLE
        # =====================================================================
        .populateResidualMatrixTable = function() {
            
            ca <- private$.caResult
            table <- self$results$residualMatrixTable
            
            N <- ca$N
            n <- ca$grand_total
            I <- ca$n_rows
            J <- ca$n_cols
            r <- ca$row_mass
            c <- ca$col_mass
            
            row_names <- rownames(N)
            col_names <- colnames(N)
            row_var <- self$options$rows
            col_var <- self$options$cols
            
            threshold <- self$options$residualThreshold
            
            # Determine residual type and statistic label
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            if (distance_metric == "freemanTukey") {
                stat_label <- "% of T\u00b2"
                residual_label <- "Freeman-Tukey Residuals"
            } else if (residual_type == "adjusted") {
                stat_label <- "% of χ²ₐ"
                residual_label <- "Adjusted Standardised Residuals"
            } else {
                stat_label <- "% of χ²"
                residual_label <- "Standardised Residuals"
            }
            
            # Set table title
            table$setTitle(paste0(residual_label, ": ", row_var, " × ", col_var))
            
            # Build columns dynamically
            table$addColumn(name = "rowname", title = row_var, type = "text")
            
            for (j in seq_len(J)) {
                table$addColumn(
                    name = paste0("col", j),
                    title = col_names[j],
                    type = "number",
                    format = "zto",
                    superTitle = col_var
                )
            }
            
            # Add row contribution column
            table$addColumn(name = "rowPct", title = stat_label, type = "number", format = "zto")
            
            # Compute residuals based on selected method
            residual_matrix <- matrix(0, nrow = I, ncol = J)
            cell_contrib_matrix <- matrix(0, nrow = I, ncol = J)
            
            # Compute expected values
            expected <- outer(r, c) * n
            
            if (distance_metric == "freemanTukey") {
                # Standardised Freeman-Tukey residuals for CA (Beh, Lombardo & Alberti, 2018)
                # Formula: sqrt(n) * 2 * (sqrt(p_ij) - sqrt(p_i. * p_.j))
                # where p_ij = n_ij / n are proportions
                P <- N / n  # Convert to proportions
                for (i in 1:I) {
                    for (j in 1:J) {
                        residual_matrix[i, j] <- sqrt(n) * 2 * (sqrt(P[i, j]) - sqrt(r[i] * c[j]))
                        cell_contrib_matrix[i, j] <- residual_matrix[i, j]^2
                    }
                }
                total_stat <- sum(cell_contrib_matrix)
            } else if (residual_type == "adjusted") {
                # Adjusted standardised residuals
                for (i in 1:I) {
                    for (j in 1:J) {
                        obs <- N[i, j]
                        exp <- expected[i, j]
                        if (exp > 0) {
                            adjustment <- sqrt((1 - r[i]) * (1 - c[j]))
                            residual_matrix[i, j] <- (obs - exp) / (sqrt(exp) * adjustment)
                            cell_contrib_matrix[i, j] <- residual_matrix[i, j]^2
                        }
                    }
                }
                total_stat <- sum(cell_contrib_matrix)
            } else {
                # Standard Pearson residuals
                for (i in 1:I) {
                    for (j in 1:J) {
                        obs <- N[i, j]
                        exp <- expected[i, j]
                        if (exp > 0) {
                            residual_matrix[i, j] <- (obs - exp) / sqrt(exp)
                            cell_contrib_matrix[i, j] <- (obs - exp)^2 / exp
                        }
                    }
                }
                total_stat <- ca$test_statistic
            }
            
            # Compute row and column percentages
            row_pct <- rowSums(cell_contrib_matrix) / total_stat * 100
            col_pct <- colSums(cell_contrib_matrix) / total_stat * 100
            
            # Compute average contributions for highlighting
            avg_row_pct <- 100 / I
            avg_col_pct <- 100 / J
            
            # Add data rows
            for (i in seq_len(I)) {
                values <- list(rowname = row_names[i])
                
                for (j in seq_len(J)) {
                    values[[paste0("col", j)]] <- residual_matrix[i, j]
                }
                
                values$rowPct <- row_pct[i]
                
                table$addRow(rowKey = paste0("row", i), values = values)
                
                # Highlight residuals exceeding threshold
                if (threshold > 0) {
                    for (j in seq_len(J)) {
                        if (abs(residual_matrix[i, j]) > threshold) {
                            table$addFormat(
                                rowKey = paste0("row", i),
                                col = paste0("col", j),
                                format = jmvcore::Cell.NEGATIVE
                            )
                        }
                    }
                }
                
                # Highlight above-average row contributions
                if (row_pct[i] > avg_row_pct) {
                    table$addFormat(
                        rowKey = paste0("row", i),
                        col = "rowPct",
                        format = jmvcore::Cell.NEGATIVE
                    )
                }
            }
            
            # Add column percentages row
            col_pct_values <- list(rowname = stat_label)
            for (j in seq_len(J)) {
                col_pct_values[[paste0("col", j)]] <- col_pct[j]
            }
            col_pct_values$rowPct <- 100.0
            
            table$addRow(rowKey = "colPct", values = col_pct_values)
            
            # Format the percentage row to stand out
            table$addFormat(
                rowKey = "colPct",
                col = "rowname",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            # Highlight above-average column contributions
            for (j in seq_len(J)) {
                if (col_pct[j] > avg_col_pct) {
                    table$addFormat(
                        rowKey = "colPct",
                        col = paste0("col", j),
                        format = jmvcore::Cell.NEGATIVE
                    )
                }
            }
            
            # Build explanatory note
            note_parts <- c()
            
            if (threshold > 0) {
                note_parts <- c(note_parts, sprintf(
                    "Residuals with |z| > %.2f are highlighted.", threshold
                ))
            }
            
            note_parts <- c(note_parts, sprintf(
                "Margins show %% contribution to total statistic; values above average (rows: %.2f%%, cols: %.2f%%) are highlighted.",
                avg_row_pct, avg_col_pct
            ))
            
            if (distance_metric == "freemanTukey") {
                note_parts <- c(note_parts, 
                                "Freeman-Tukey residuals are robust to overdispersion (Beh, Lombardo & Alberti, 2018).")
            } else if (residual_type == "adjusted") {
                note_parts <- c(note_parts,
                                "Adjusted residuals account for marginal constraints.")
            }
            
            table$setNote('residualNote', paste(note_parts, collapse = " "))
        },
        
        # =====================================================================
        # POPULATE CORRELATIONS TABLE
        # =====================================================================
        .populateCorrelTable = function() {
            ca <- private$.caResult
            table <- self$results$correlTable
            
            focus <- self$options$contribFocus
            quality_threshold <- self$options$qualityThreshold
            
            if (focus == "cols") {
                princ <- ca$row_princ
                cos2 <- ca$row_cos2
                names_vec <- rownames(ca$N)
                n_cats <- ca$n_rows
            } else {
                princ <- ca$col_princ
                cos2 <- ca$col_cos2
                names_vec <- colnames(ca$N)
                n_cats <- ca$n_cols
            }
            
            n_shown <- 0
            
            for (i in seq_len(n_cats)) {
                total_quality <- sum(cos2[i, ])
                
                if (total_quality < quality_threshold) {
                    next
                }
                
                n_shown <- n_shown + 1
                
                values <- list(category = names_vec[i])
                
                max_abs_corr <- 0
                primary_dim <- 1
                primary_sign <- "+"
                
                for (k in 1:ca$n_axes) {
                    cos2_val <- cos2[i, k]
                    coord_sign <- sign(princ[i, k])
                    if (coord_sign == 0) coord_sign <- 1
                    signed_corr <- coord_sign * sqrt(cos2_val)
                    values[[paste0("corr", k)]] <- signed_corr
                    
                    if (abs(signed_corr) > max_abs_corr) {
                        max_abs_corr <- abs(signed_corr)
                        primary_dim <- k
                        primary_sign <- if (coord_sign >= 0) "(+)" else "(\u2212)"
                    }
                }
                
                values$primaryDim <- paste("Dim", primary_dim, primary_sign)
                values$quality <- total_quality
                
                table$addRow(rowKey = i, values = values)
                
                table$addFormat(
                    rowKey = i,
                    col = paste0("corr", primary_dim),
                    format = jmvcore::Cell.NEGATIVE
                )
            }
            
            correl_explanation <- "Correlation = \u00b1\u221acos\u00b2, where sign reflects coordinate direction. Quality = sum of cos\u00b2 across all extracted dimensions (overall representation quality)."
            
            if (quality_threshold > 0) {
                n_filtered <- n_cats - n_shown
                if (n_filtered > 0) {
                    table$setNote('filterNote', sprintf(
                        "%d categories with quality < %.2f are hidden. %s This table supports Contribution Biplot interpretation.", 
                        n_filtered, quality_threshold, correl_explanation
                    ))
                } else {
                    table$setNote('filterNote', paste(correl_explanation, "This table supports Contribution Biplot interpretation."))
                }
            } else {
                table$setNote('interpNote', paste(correl_explanation, "This table supports Contribution Biplot interpretation."))
            }
        },
        
        # =====================================================================
        # POPULATE INTERPRETATION SUMMARY TABLE
        # =====================================================================
        .populateInterpTable = function() {
            ca <- private$.caResult
            table <- self$results$interpTable
            table$deleteRows()
            
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                focus_contrib <- ca$col_contrib
                focus_princ <- ca$col_princ
                focus_names <- colnames(ca$N)
                n_focus <- ca$n_cols
                
                interp_princ <- ca$row_princ
                interp_cos2 <- ca$row_cos2
                interp_names <- rownames(ca$N)
            } else {
                focus_contrib <- ca$row_contrib
                focus_princ <- ca$row_princ
                focus_names <- rownames(ca$N)
                n_focus <- ca$n_rows
                
                interp_princ <- ca$col_princ
                interp_cos2 <- ca$col_cos2
                interp_names <- colnames(ca$N)
            }
            
            avg_contrib <- 100 / n_focus
            
            interp_primary <- data.frame(
                name = interp_names,
                primary_dim = integer(length(interp_names)),
                primary_sign = integer(length(interp_names)),
                stringsAsFactors = FALSE
            )
            
            for (i in seq_along(interp_names)) {
                max_corr <- 0
                primary_dim <- 1
                primary_sign <- 1
                
                for (k in 1:ca$n_axes) {
                    corr <- sqrt(interp_cos2[i, k]) * sign(interp_princ[i, k])
                    if (abs(corr) > abs(max_corr)) {
                        max_corr <- corr
                        primary_dim <- k
                        primary_sign <- sign(interp_princ[i, k])
                        if (primary_sign == 0) primary_sign <- 1
                    }
                }
                
                interp_primary$primary_dim[i] <- primary_dim
                interp_primary$primary_sign[i] <- primary_sign
            }
            
            definers <- data.frame(
                name = character(),
                dim = integer(),
                sign = integer(),
                contrib = numeric(),
                stringsAsFactors = FALSE
            )
            
            for (k in 1:ca$n_axes) {
                for (i in seq_along(focus_names)) {
                    contrib_val <- focus_contrib[i, k]
                    if (!is.na(contrib_val) && contrib_val >= avg_contrib) {
                        definers <- rbind(definers, data.frame(
                            name = focus_names[i],
                            dim = k,
                            sign = sign(focus_princ[i, k]),
                            contrib = contrib_val,
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }
            
            if (nrow(definers) > 0) {
                definers <- definers[order(definers$dim, -definers$contrib), ]
            }
            
            row_key <- 0
            
            for (d in seq_len(nrow(definers))) {
                def_name <- definers$name[d]
                def_dim <- definers$dim[d]
                def_sign <- definers$sign[d]
                
                associated <- interp_primary$name[
                    interp_primary$primary_dim == def_dim & 
                    interp_primary$primary_sign == def_sign
                ]
                
                if (length(associated) > 0) {
                    assoc_str <- paste(associated, collapse = ", ")
                } else {
                    assoc_str <- "\u2014"
                }
                
                sign_str <- if (def_sign > 0) "+" else "\u2212"
                direction_str <- paste0("Dim ", def_dim, " (", sign_str, ")")
                
                row_key <- row_key + 1
                table$addRow(rowKey = row_key, values = list(
                    category = def_name,
                    direction = direction_str,
                    association = assoc_str
                ))
            }
            
            orphans <- interp_primary$name[
                !sapply(seq_len(nrow(interp_primary)), function(i) {
                    any(definers$dim == interp_primary$primary_dim[i] & 
                        definers$sign == interp_primary$primary_sign[i])
                })
            ]
            
            if (length(orphans) > 0) {
                orphan_groups <- list()
                for (i in seq_along(orphans)) {
                    idx <- which(interp_primary$name == orphans[i])
                    key <- paste0(interp_primary$primary_dim[idx], "_", interp_primary$primary_sign[idx])
                    if (is.null(orphan_groups[[key]])) {
                        orphan_groups[[key]] <- list(
                            dim = interp_primary$primary_dim[idx],
                            sign = interp_primary$primary_sign[idx],
                            names = c()
                        )
                    }
                    orphan_groups[[key]]$names <- c(orphan_groups[[key]]$names, orphans[i])
                }
                
                for (grp in orphan_groups) {
                    sign_str <- if (grp$sign > 0) "+" else "\u2212"
                    direction_str <- paste0("Dim ", grp$dim, " (", sign_str, ")")
                    pole_label <- paste0("Dim ", grp$dim, " (", sign_str, ") pole")
                    
                    row_key <- row_key + 1
                    table$addRow(rowKey = row_key, values = list(
                        category = pole_label,
                        direction = direction_str,
                        association = paste(grp$names, collapse = ", ")
                    ))
                }
            }
            
            avg_pct <- sprintf("%.2f%%", avg_contrib)
            table$setNote(
                "contribNote",
                paste0(
                    "Only defining categories with contributions \u2265 average (",
                    avg_pct,
                    ") are shown. Low-contributing categories are omitted for clarity of interpretation. ",
                    "This table supports Contribution Biplot interpretation."
                )
            )
        },
        
        # =====================================================================
        # POPULATE CLUSTERS TABLE - Reports clusters for BOTH rows and columns
        # =====================================================================
        .populateSRDTable = function() {
            cluster_rows <- private$.clusterResultRows
            cluster_cols <- private$.clusterResultCols
            table <- self$results$srdTable
            
            row_var <- self$options$rows
            col_var <- self$options$cols
            
            # Use actual method from results (may differ from option if Greenacre fell back to Husson)
            method <- self$options$clusterMethod
            if (!is.null(cluster_rows) && !is.null(cluster_rows$method)) {
                method <- cluster_rows$method
            } else if (!is.null(cluster_cols) && !is.null(cluster_cols$method)) {
                method <- cluster_cols$method
            }
            
            # Ensure method is valid
            if (is.null(method) || !method %in% c("srd", "greenacre", "husson")) {
                method <- "srd"
            }
            
            # Determine column title and format based on method and distance metric
            ca <- private$.caResult
            is_hellinger <- !is.null(ca) && ca$distance_type == "hellinger"
            
            if (method == "srd") {
                stat_col_title <- "Merge p"
                stat_col_format <- "zto,pvalue"
            } else {
                # Use ΔT² for Freeman-Tukey, Δχ² for Pearson
                stat_col_title <- if (is_hellinger) "\u0394T\u00b2" else "\u0394\u03c7\u00b2"
                stat_col_format <- "zto"
            }
            
            # Add the mergeStat column dynamically with appropriate title
            table$addColumn(
                name = "mergeStat",
                title = stat_col_title,
                type = "number",
                format = stat_col_format
            )
            
            # Set table title based on method
            method_title <- if (method == "srd") {
                "SRD Clusters (Orton & Tyers)"
            } else if (method == "greenacre") {
                "Clusters (Greenacre Critical \u03c7\u00b2)"
            } else {
                "Clusters (Husson Inertia Gain Ratio)"
            }
            table$setTitle(method_title)
            
            row_key <- 0
            
            # -----------------------------------------------------------------
            # Row clusters section
            # -----------------------------------------------------------------
            if (!is.null(cluster_rows) && length(cluster_rows$groups) > 0) {
                row_key <- row_key + 1
                table$addRow(rowKey = row_key, values = list(
                    cluster = NA,
                    members = paste0("Row variable: ", row_var),
                    nMembers = NA,
                    mergeStat = NA
                ))
                table$addFormat(rowKey = row_key, col = "members", 
                                format = jmvcore::Cell.BEGIN_GROUP)
                
                for (i in seq_along(cluster_rows$groups)) {
                    group <- cluster_rows$groups[[i]]
                    row_key <- row_key + 1
                    
                    # Get the appropriate statistic
                    stat_value <- if (method == "srd") {
                        group$merge_p
                    } else {
                        group$merge_stat
                    }
                    
                    table$addRow(rowKey = row_key, values = list(
                        cluster = i,
                        members = paste(group$members, collapse = ", "),
                        nMembers = length(group$members),
                        mergeStat = stat_value
                    ))
                }
            }
            
            # -----------------------------------------------------------------
            # Column clusters section
            # -----------------------------------------------------------------
            if (!is.null(cluster_cols) && length(cluster_cols$groups) > 0) {
                row_key <- row_key + 1
                table$addRow(rowKey = row_key, values = list(
                    cluster = NA,
                    members = paste0("Column variable: ", col_var),
                    nMembers = NA,
                    mergeStat = NA
                ))
                table$addFormat(rowKey = row_key, col = "members", 
                                format = jmvcore::Cell.BEGIN_GROUP)
                
                for (i in seq_along(cluster_cols$groups)) {
                    group <- cluster_cols$groups[[i]]
                    row_key <- row_key + 1
                    
                    stat_value <- if (method == "srd") {
                        group$merge_p
                    } else {
                        group$merge_stat
                    }
                    
                    table$addRow(rowKey = row_key, values = list(
                        cluster = i,
                        members = paste(group$members, collapse = ", "),
                        nMembers = length(group$members),
                        mergeStat = stat_value
                    ))
                }
            }
            
            # -----------------------------------------------------------------
            # Handle case where neither variable has clusters
            # -----------------------------------------------------------------
            if (row_key == 0) {
                table$addRow(rowKey = 1, values = list(
                    cluster = NA,
                    members = "No clusters found (all categories are singletons)",
                    nMembers = NA,
                    mergeStat = NA
                ))
            }
            
            # -----------------------------------------------------------------
            # Build explanatory note
            # -----------------------------------------------------------------
            ca <- private$.caResult
            note_parts <- c()
            
            # Row singletons
            if (!is.null(cluster_rows) && length(cluster_rows$singletons) > 0) {
                note_parts <- c(note_parts, paste0(
                    "Row singletons: ", 
                    paste(cluster_rows$singletons, collapse = ", "), "."
                ))
            }
            
            # Column singletons
            if (!is.null(cluster_cols) && length(cluster_cols$singletons) > 0) {
                note_parts <- c(note_parts, paste0(
                    "Column singletons: ", 
                    paste(cluster_cols$singletons, collapse = ", "), "."
                ))
            }
            
            # No clusters with a test-based method: usually a large-sample effect
            if (row_key == 0 && method %in% c("srd", "greenacre")) {
                n_total <- if (!is.null(ca)) sum(ca$N) else NA
                note_parts <- c(note_parts, paste0(
                    "No two categories could be merged: every merge was statistically significant",
                    if (!is.na(n_total)) paste0(" (N = ", format(n_total, big.mark = ","), ")") else "",
                    ". With large samples even small differences between profiles are significant; ",
                    "the inertia gain ratio method (Husson) does not depend on sample size."))
            }
            
            # Method-specific explanation
            distance_label <- if (!is.null(ca) && ca$distance_type == "hellinger") {
                "Hellinger distance"
            } else {
                "\u03c7\u00b2 distance"
            }
            
            # Statistic label depends on distance metric
            stat_label <- if (is_hellinger) "T\u00b2" else "\u03c7\u00b2"
            delta_stat_label <- if (is_hellinger) "\u0394T\u00b2" else "\u0394\u03c7\u00b2"
            
            method_note <- if (method == "srd") {
                paste0(
                    "SRD method: Categories with statistically indistinguishable profiles (", 
                    distance_label, ", p > ", self$options$clusterAlpha, 
                    ") are grouped together."
                )
            } else if (method == "greenacre") {
                base_note <- paste0(
                    "Greenacre method: Categories merged whilst ", stat_label, 
                    " reduction < critical value (\u03b1 = 0.05). Statistic shows ", 
                    delta_stat_label, " at final merge."
                )
                # Add caveat for Freeman-Tukey
                if (is_hellinger) {
                    paste0(base_note, 
                           " Note: Critical values are derived for \u03c7\u00b2; application to T\u00b2 is approximate. ",
                           "Consider SRD or Husson et al. methods.")
                } else {
                    base_note
                }
            } else {
                paste0(
                    "Husson method: Optimal partition found by minimising the inertia gain ratio. ",
                    "Statistic shows ", delta_stat_label, " at final merge."
                )
            }
            note_parts <- c(note_parts, method_note)
            
            # Check for Greenacre fallback
            if (self$options$clusterMethod == "greenacre") {
                if (!is.null(cluster_rows) && !is.null(cluster_rows$method) && cluster_rows$method == "husson") {
                    note_parts <- c(note_parts, 
                                    "Greenacre critical values are tabulated only for tables with 3 to 11 categories on the smaller side and at most 201 on the larger side; the Husson method was used instead."
                    )
                }
            }
            
            table$setNote('clusterNote', paste(note_parts, collapse = " "))
        },
        
        # =====================================================================
        # POPULATE METHOD INFORMATION (HTML)
        # =====================================================================
        .populateMethodInfo = function() {
            
            if (!self$options$showMethodInfo) return()
            
            focus <- self$options$contribFocus
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
            
            # -----------------------------------------------------------------
            # Section 1: Introduction to Correspondence Analysis
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1em;'>Correspondence Analysis: Overview</h3>
              <p>Correspondence Analysis (CA) is a multivariate technique for visualising the association 
              structure between the row and column categories of a contingency table. It transforms the 
              distances between category profiles into a low-dimensional Euclidean space, 
              allowing the relationships to be displayed graphically. Categories that are associated 
              (i.e., occur together more often than expected under independence) appear close together 
              in the plot, whilst those that repel each other appear far apart.</p>
              
              <p><strong>Key output:</strong> The primary output is the <em>contribution biplot</em>, 
              which simultaneously displays both row and column categories. One set of categories 
              defines the dimensions of the space (shown with lines to the origin), whilst the other 
              set is projected onto this space for interpretation. The distances between points have 
              geometric meaning related to their statistical similarity.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017; Beh & Lombardo 2014.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 2: Distance Metrics and Residual Types
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Distance Metrics and Residual Types</h3>
              
              <p>This module supports two distance metrics for CA. The choice affects how deviations 
              from independence are measured and how distances between categories are calculated.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Pearson Residuals (\u03c7\u00b2 Distance)</h4>
              
              <p>The classical approach decomposes the matrix of <em>Pearson standardised residuals</em>:</p>
              
              <p style='text-align: center; font-style: italic;'>
                r<sub>ij</sub> = (p<sub>ij</sub> \u2212 p<sub>i\u00b7</sub>p<sub>\u00b7j</sub>) / \u221a(p<sub>i\u00b7</sub>p<sub>\u00b7j</sub>)
              </p>
              
              <p>The sum of squared Pearson residuals equals Pearson's chi-squared statistic divided by n. 
              The distances between row (or column) categories in the optimal CA space are <em>chi-squared distances</em>:</p>
              
              <p style='text-align: center; font-style: italic;'>
                d<sub>\u03c7\u00b2</sub>(i, i') = \u221a[\u03a3<sub>j</sub> (p<sub>ij</sub>/p<sub>i\u00b7</sub> \u2212 p<sub>i'j</sub>/p<sub>i'\u00b7</sub>)\u00b2 / p<sub>\u00b7j</sub>]
              </p>
              
              <p>This approach assumes that cell frequencies follow a Poisson distribution, implying that the 
              mean and variance of cell counts are equal. In practice, contingency tables often exhibit 
              <em>overdispersion</em> (variance exceeding the mean), which can affect the reliability of 
              inferences based on Pearson residuals.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017; Beh & Lombardo 2014.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Adjusted Standardised Residuals</h4>
              <p>Following Beh (2012), the adjusted residuals account for the variance of Pearson residuals:
              \u0072\u0303<sub>ij</sub> = r<sub>ij</sub> / \u221a((1\u2212p<sub>i\u00b7</sub>)(1\u2212p<sub>\u00b7j</sub>)).</p>
              <p>This adjustment has two key effects:</p>
              <ul style='margin-left: 1.5em;'>
                <li><strong>Stretching:</strong> Profile coordinates move further from the origin, 
                with the degree of stretching depending on marginal proportions. Categories with 
                small marginals experience greater stretching.</li>
                <li><strong>Enhanced detection:</strong> Cells that deviate significantly from 
                independence may be more readily identified, as the adjusted chi-squared statistic 
                \u0058\u0303\u00b2 is typically larger than X\u00b2.</li>
              </ul>
              <p>The relationship between standard and adjusted coordinates involves the stretching factor 
              \u03c9<sub>m</sub> for each dimension, such that \u0066\u0303<sub>im</sub> = f<sub>im</sub> \u00d7 
              \u221a(\u03c9<sub>m</sub> / (p<sub>i\u00b7</sub>(1\u2212p<sub>i\u00b7</sub>))).</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh 2012; Haberman 1973.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Freeman-Tukey Residuals (Hellinger Distance)</h4>
              
              <p>An alternative approach decomposes the matrix of <em>Freeman-Tukey residuals</em> 
              (Beh, Lombardo & Alberti, 2018):</p>
              
              <p style='text-align: center; font-style: italic;'>
                r\u0303<sub>ij</sub> = 2(\u221ap<sub>ij</sub> \u2212 \u221a(p<sub>i\u00b7</sub>p<sub>\u00b7j</sub>))
              </p>
              
              <p>The Freeman-Tukey transformation stabilises variance, addressing the overdispersion problem 
              common in sparse contingency tables. The sum of squared Freeman-Tukey residuals yields the 
              Freeman-Tukey statistic T\u00b2, which (like \u03c7\u00b2) is asymptotically chi-squared distributed 
              with (I\u22121)(J\u22121) degrees of freedom.</p>
              
              <p>The distances between categories in this framework are <em>Hellinger distances</em>:</p>
              
              <p style='text-align: center; font-style: italic;'>
                d<sub>H</sub>(i, i') = 2\u221a[\u03a3<sub>j</sub> (\u221a(p<sub>ij</sub>/p<sub>i\u00b7</sub>) \u2212 \u221a(p<sub>i'j</sub>/p<sub>i'\u00b7</sub>))\u00b2]
              </p>
              
              <p><strong>Advantages of Freeman-Tukey residuals:</strong></p>
              <ul style='margin-left: 2em;'>
                <li>More robust to overdispersion in sparse tables</li>
                <li>Reconstituted cell frequencies are always non-negative</li>
                <li>Interpretation of distances and contributions proceeds analogously to classical CA</li>
              </ul>
              
              <p><strong>Recommendation:</strong> Use Pearson residuals as the default for well-behaved tables. 
              If the overdispersion diagnostic plot shows non-random patterns, consider switching to 
              Freeman-Tukey residuals.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh, Lombardo & Alberti 2018; Bishop, Fienberg & Holland 2007.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 3: Visualisation Types
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Visualisation Types</h3>
              
              <h4 style='color: #5D6D7E;'>Symmetric CA Plot</h4>
              <p>The classic biplot showing both row and column categories in principal coordinates.
              Categories close together have similar profiles; those far apart differ substantially.
              The origin represents the average profile (complete independence).</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Geometric Interpretation Plot</h4>
              <p>This visualisation, following Borg & Groenen (2005) and Yelland (2010), provides an 
              intuitive method for reading relative frequencies directly from the plot:</p>
              <ul style='margin-left: 1.5em;'>
                <li>A line is drawn through the origin and a selected <em>reference category</em>.</li>
                <li>Perpendicular projections are dropped from each category to this reference line.</li>
                <li><strong>Same side as reference:</strong> Categories whose projections fall on the 
                same side of the origin as the reference category occur <em>more frequently</em> than 
                average in association with the reference. The further from the origin, the higher 
                the relative frequency.</li>
                <li><strong>Opposite side:</strong> Categories projecting on the opposite side occur 
                <em>less frequently</em> than average. The further from the origin, the lower the 
                relative frequency.</li>
              </ul>
              <p><strong>Limitations:</strong> This method complements but does not replace standard 
              CA interpretation. It is most effective for small-to-medium tables and is particularly 
              useful for communicating results to non-expert audiences.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Borg & Groenen 2005; Yelland 2010.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Contribution Biplot</h4>
              
              <p>The contribution biplot (Greenacre, 2017) is a refinement of the standard symmetric biplot 
              that facilitates interpretation. It distinguishes between:</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Plane-defining categories</strong> (shown in blue with lines to origin): These define 
                the dimensions of the space. Their positions show their contributions to each axis, with longer 
                lines indicating larger contributions.</li>
                <li><strong>Interpreted categories</strong> (shown in red as triangles): These are projected 
                onto the plane defined by the other variable. Their positions can be interpreted relative 
                to the dimension-defining categories.</li>
              </ul>
            ")
            
            if (focus == "cols") {
                html <- paste0(html, "
                  <p><strong>Current selection:</strong> Column categories define the plane; row categories are interpreted.</p>
                ")
            } else {
                html <- paste0(html, "
                  <p><strong>Current selection:</strong> Row categories define the plane; column categories are interpreted.</p>
                ")
            }
            
            html <- paste0(html, "
              <p><strong>Interpreting the biplot:</strong></p>
              <ul style='margin-left: 2em;'>
                <li>The <em>angle</em> between two plane-defining categories reflects their similarity 
                (small angle = similar profiles, 90\u00b0 = uncorrelated, 180\u00b0 = opposite profiles)</li>
                <li>The <em>length</em> of the line from origin to a plane-defining category reflects its 
                contribution to the displayed dimensions (longer = more important)</li>
                <li>Interpreted categories (triangles) that fall close to a plane-defining category are 
                strongly associated with it</li>
                <li>Above-average contributors are drawn with a darker, thicker outline; below-average contributors with a lighter, thinner outline</li>
              </ul>
              
              <p><strong>Dimension definition:</strong> The average contribution is 100%/k, where k is the 
              number of plane-defining categories. Categories contributing more than average are considered 
              <em>above-average contributors</em> and are the primary definers of that dimension.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 4: Understanding the Output Tables
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Understanding the Output Tables</h3>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Dimension Definition Table</h4>
              
              <p>This table shows which plane-defining categories contribute most to each dimension. 
              For each dimension, categories are listed in descending order of contribution. The columns show:</p>
              <ul style='margin-left: 2em;'>
                <li><strong>Contribution %:</strong> The percentage of the dimension's inertia explained by this category</li>
                <li><strong>Cumulative %:</strong> Running total of contributions</li>
                <li><strong>Direction:</strong> Whether the category falls on the positive (+) or negative (\u2212) side of the axis</li>
              </ul>
              <p>Categories contributing above the average (100%/k) are the primary definers of that dimension. 
              The table helps identify the substantive meaning of each axis.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Dimension Summary Table</h4>
              
              <p>This compact table summarises each dimension by showing only the above-average contributors, 
              grouped by their pole (positive or negative). It provides a quick interpretive label for each 
              dimension. For example, if Dimension 1 has 'Heavy, Chain' on the positive pole and 'None, Light' 
              on the negative pole, this dimension contrasts heavy/chain smokers against non/light smokers.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Correlations with Dimensions Table</h4>
              
              <p>This table shows how well each <em>interpreted</em> category is represented by each dimension. 
              The values are signed correlations (\u00b1\u221acos\u00b2), where:</p>
              <ul style='margin-left: 2em;'>
                <li>The <strong>magnitude</strong> (0 to 1) indicates how well the dimension captures the category's 
                deviation from the centroid</li>
                <li>The <strong>sign</strong> indicates whether the category falls on the positive or negative side 
                of the axis</li>
                <li><strong>Primary:</strong> The dimension with the largest absolute correlation for this category</li>
                <li><strong>Quality:</strong> The sum of cos\u00b2 across all retained dimensions; values near 1 indicate 
                excellent representation in the reduced space</li>
              </ul>
              <p>Categories with low quality should be interpreted cautiously as their positions may be misleading.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Interpretation Summary Table</h4>
              
              <p>This table directly links plane-defining categories (above-average contributors) to the 
              interpreted categories that are most strongly associated with them. For each defining category, 
              it lists the interpreted categories that fall on the same side of the relevant dimension, 
              making the association patterns explicit. This is the key table for substantive interpretation.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 4b: Residual matrix
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Residual Matrix Table</h4>
              
              <p>This table displays the residuals for each cell of the contingency table in their natural 
              matrix layout, preserving the row × column structure. The type of residual depends on the 
              selected distance metric and residual type:</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Standardised residuals:</strong> (O − E) / √E, where O is observed and E is expected count</li>
                <li><strong>Adjusted standardised residuals:</strong> Standardised residuals divided by 
                √[(1 − p<sub>i·</sub>)(1 − p<sub>·j</sub>)], accounting for marginal constraints</li>
                <li><strong>Freeman-Tukey residuals:</strong> √n × 2 × (√p<sub>ij</sub> − √(p<sub>i·</sub> × p<sub>·j</sub>)), 
                robust to overdispersion in sparse tables</li>
              </ul>
              
              <p><strong>Reading the table:</strong></p>
              <ul style='margin-left: 2em;'>
                <li><strong>Cell values:</strong> Residuals indicate departure from independence. Positive values 
                mean the cell occurs <em>more often</em> than expected; negative values mean <em>less often</em> 
                than expected.</li>
                <li><strong>Highlighted cells:</strong> Residuals with |z| > threshold (default 1.96) are highlighted, 
                indicating statistically noteworthy departures from independence at approximately p < 0.05.</li>
                <li><strong>Row margins (% of statistic):</strong> The rightmost column shows each row's percentage 
                contribution to the total test statistic (χ², χ²<sub>a</sub>, or T²). This identifies which row 
                categories contribute most to the overall association.</li>
                <li><strong>Column margins (% of statistic):</strong> The bottom row shows each column's percentage 
                contribution. Together with row margins, this reveals the structure of the association.</li>
                <li><strong>Highlighted margins:</strong> Marginal percentages exceeding the average contribution 
                (100%/I for rows, 100%/J for columns) are highlighted, identifying categories that contribute 
                disproportionately to the total association strength.</li>
              </ul>
              
              <p><strong>Interpretation strategy:</strong> First, identify highlighted marginal percentages to find 
              the most influential rows and columns. Then examine the highlighted cells within those rows/columns 
              to understand the specific associations driving the overall pattern. Positive residuals indicate 
              attraction (categories co-occur more than expected); negative residuals indicate repulsion.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh, Lombardo & Alberti 2018 (Table 2); Greenacre 2017.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 5: Quality of Representation
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Quality of Representation (cos\u00b2)</h3>
              
              <p>The <em>quality</em> (cos\u00b2) of a category's representation measures how well its position 
              in the reduced-dimension plot captures its full profile distance from the centroid. It is 
              calculated as:</p>
              
              <p style='text-align: center; font-style: italic;'>
                cos\u00b2<sub>k</sub> = (principal coordinate on axis k)\u00b2 / (total distance from centroid)\u00b2
              </p>
              
              <p>The sum of cos\u00b2 values across all retained axes gives the <em>total quality</em> for that 
              category. A quality of 0.8, for example, means 80% of the category's distance from the centroid 
              is captured in the displayed plot. Categories with low quality may be poorly represented and 
              should be interpreted cautiously.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017; Beh & Lombardo 2014.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 6: Clustering Methods
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Clustering Methods</h3>
              
              <p>This module offers three methods for identifying groups of categories with similar profiles. 
              All methods cluster row categories separately from column categories, producing independent 
              partitions for each. The methods differ in their stopping criteria and statistical philosophy.</p>
              
              <p><strong>Clustering in plots:</strong> The Symmetric CA Plot can display clusters for rows, 
              columns, or both (selectable via the 'Cluster target' option). The Contribution Biplot displays 
              clusters for the <em>interpreted</em> variable only (i.e., the variable opposite to the one 
              defining the plane). For example, if columns define the plane, row clusters are shown.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Method 1: Simultaneous Reduction of Dimension (Orton & Tyers, 1991)",
                           if (self$options$clusterMethod == "srd") " \u2014 <em>currently selected</em>" else "",
                           "</h4>
              
              <p>SRD is a technique for simplifying contingency tables by systematically merging rows 
              and/or columns that are statistically indistinguishable. Unlike conventional hierarchical 
              clustering which merges greedily and cuts the tree post-hoc, SRD performs a significance 
              test at each potential merge and only combines categories when their profiles cannot be 
              distinguished at the specified significance level.</p>
              
              <p><strong>The algorithm:</strong></p>
              <ol style='margin-left: 2em;'>
                <li>Calculate chi-squared distances between all pairs of row (or column) profiles.</li>
                <li>Identify the closest pair (highest p-value when testing for profile differences).</li>
                <li>If p > \u03b1, the pair cannot be distinguished\u2014merge them and return to step 1.</li>
                <li>If p \u2264 \u03b1, the closest pair differs significantly\u2014stop.</li>
              </ol>
              
              <p><strong>Distance metric:</strong> SRD uses the chi-squared distance between profiles. 
              For two rows <em>i</em> and <em>j</em>, the squared distance is:</p>
              
              <p style='text-align: center; font-style: italic;'>
                d\u00b2(i,j) = \u03a3<sub>k</sub> (p<sub>ik</sub> \u2212 p<sub>jk</sub>)\u00b2 / c<sub>k</sub>
              </p>
              
              <p>This is converted to a chi-squared statistic by weighting:</p>
              
              <p style='text-align: center; font-style: italic;'>
                \u03c7\u00b2 = d\u00b2 \u00d7 (n<sub>i</sub> \u00d7 n<sub>j</sub>) / (n<sub>i</sub> + n<sub>j</sub>)
              </p>
              
              <p>Under the null hypothesis that rows <em>i</em> and <em>j</em> are samples from the same 
              population, this statistic follows a chi-squared distribution with (k\u22121) degrees of freedom.</p>
              
              <p><strong>Interpretation:</strong> Categories in the same cluster have profiles that are 
              statistically indistinguishable at the chosen \u03b1 level. The clustering is conservative\u2014only 
              merging categories when there is insufficient evidence that they differ.</p>
              
              <p><strong>Freeman-Tukey variant:</strong> When Freeman-Tukey residuals are selected in 
              Analysis Settings, SRD uses Hellinger distance instead of chi-squared distance. The test 
              statistic remains asymptotically chi-squared distributed.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Orton & Tyers 1991; Greenacre 1984.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Method 2: Critical \u03c7\u00b2 (Greenacre, 2017)",
                           if (self$options$clusterMethod == "greenacre") " \u2014 <em>currently selected</em>" else "",
                           "</h4>
              
              <p>This method performs agglomerative hierarchical clustering using Ward's criterion 
              (minimising the loss of \u03c7\u00b2 at each merge). The partition is determined by comparing 
              each merge's \u03c7\u00b2 reduction against a critical threshold derived from the distribution 
              of the largest characteristic root (Pearson & Hartley, 1972, Table 51).</p>
              
              <p><strong>Interpretation:</strong> Merging continues as long as the \u03c7\u00b2 reduction at each 
              step remains below the critical value (\u03b1 = 0.05). When a merge would exceed this threshold, 
              the algorithm stops\u2014this merge would combine categories whose profiles differ significantly.</p>
              
              <p><strong>Table size constraints:</strong> Critical values are only available for tables where 
              3 \u2264 min(rows, columns) \u2264 11 and max(rows, columns) \u2264 201. For larger tables, the module 
              automatically uses the Husson method instead.</p>
              
              <p><strong>Note on Freeman-Tukey metric:</strong> When Freeman-Tukey residuals are selected, 
              the clustering uses T\u00b2 (Freeman-Tukey statistic) instead of \u03c7\u00b2. Since the 
              critical values in Pearson & Hartley (1972) are specifically derived for \u03c7\u00b2, 
              their application to T\u00b2 is approximate. Both statistics are asymptotically 
              \u03c7\u00b2-distributed, so the approximation is reasonable, but users requiring 
              strict statistical validity should prefer the SRD method (which uses Hellinger distance 
              directly in hypothesis tests) or the Husson method (which is purely data-driven and 
              makes no distributional assumptions) when using Freeman-Tukey residuals.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017, Chapter 10; Pearson & Hartley 1972, Table 51.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Method 3: Inertia Gain Ratio (Husson et al., 2017)",
                           if (self$options$clusterMethod == "husson") " \u2014 <em>currently selected</em>" else "",
                           "</h4>
              
              <p>This data-driven method builds a complete Ward hierarchy, then selects the optimal partition 
              by finding the point where the relative gain in between-cluster inertia drops most sharply. 
              The criterion minimises \u0394(q) / \u0394(q+1), where \u0394(q) is the inertia gain when going 
              from q clusters to q\u22121.</p>
              
              <p><strong>Interpretation:</strong> The optimal partition captures the 'elbow' in the inertia 
              curve\u2014the point where further merging yields diminishing returns. Unlike the other methods, 
              this approach makes no formal significance claims; it identifies structure rather than 
              testing hypotheses.</p>
              
              <p><strong>Table size:</strong> This method has no table size constraints and can be applied 
              to contingency tables of any dimension.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Husson et al. 2017, Chapter 3.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Comparing the Methods</h4>
              
              <table style='border-collapse: collapse; margin: 1em 0; font-size: 0.9em;'>
                <tr style='background-color: #f0f0f0;'>
                  <th style='border: 1px solid #ccc; padding: 6px;'>Aspect</th>
                  <th style='border: 1px solid #ccc; padding: 6px;'>SRD</th>
                  <th style='border: 1px solid #ccc; padding: 6px;'>Greenacre</th>
                  <th style='border: 1px solid #ccc; padding: 6px;'>Husson</th>
                </tr>
                <tr>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Stopping criterion</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>p \u2264 \u03b1</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>\u0394\u03c7\u00b2 \u2265 critical</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Min ratio</td>
                </tr>
                <tr>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Philosophy</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Hypothesis test</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Hypothesis test</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Data-driven</td>
                </tr>
                <tr>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Table limits</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>None</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>Yes</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>None</td>
                </tr>
                <tr>
                  <td style='border: 1px solid #ccc; padding: 6px;'>User parameter</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>\u03b1 level</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>None (fixed \u03b1=0.05)</td>
                  <td style='border: 1px solid #ccc; padding: 6px;'>None</td>
                </tr>
              </table>
              
              <p><strong>Practical guidance:</strong> SRD is useful when you want direct control over the 
              significance level and prefer a conservative approach. Greenacre's method provides a formal 
              test but is limited to smaller tables. Husson's method is versatile and often suggests 
              parsimonious solutions. When multiple methods are available, comparing their results can 
              provide useful perspective on the robustness of the identified structure.</p>
            ")
            
            # -----------------------------------------------------------------
            # Section 7: Overdispersion Diagnostic Plot
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Overdispersion Diagnostic Plot</h3>
              
              <p>The overdispersion diagnostic plot (Beh, Lombardo & Alberti, 2018, Figure 1) helps 
              assess whether the data exhibit overdispersion, which would suggest that Freeman-Tukey 
              residuals are more appropriate than Pearson residuals for the correspondence analysis.</p>
              
              <p><strong>Axes:</strong></p>
              <ul style='margin-left: 2em;'>
                <li><strong>X-axis:</strong> Standard deviation under independence, \u221a(n \u00d7 p<sub>i\u00b7</sub> \u00d7 p<sub>\u00b7j</sub>). 
                Cells with small expected counts appear on the left; cells with large expected counts appear on the right.</li>
                <li><strong>Y-axis:</strong> Pearson residual, (n<sub>ij</sub> \u2212 e<sub>ij</sub>) / \u221ae<sub>ij</sub>. 
                This measures how much each cell deviates from independence.</li>
              </ul>
              
              <p><strong>How to interpret the plot:</strong></p>
              <ul style='margin-left: 2em;'>
                <li><strong>Random scatter (no overdispersion):</strong> If points are randomly distributed 
                across the plot with no systematic pattern, the Poisson assumption is reasonable and Pearson 
                residuals are appropriate for the CA.</li>
                <li><strong>Problematic patterns (overdispersion present):</strong> The following patterns 
                suggest overdispersion:
                  <ul style='margin-left: 1.5em; margin-top: 0.3em;'>
                    <li><em>Clustering to the left:</em> Most points have small expected SD, indicating 
                    many sparse cells</li>
                    <li><em>Diagonal banding:</em> Points form parallel diagonal lines (common in sparse 
                    tables with many small counts)</li>
                    <li><em>Fan shape:</em> The spread of residuals increases or decreases systematically 
                    with expected SD</li>
                  </ul>
                If any of these patterns are present (as in Beh et al., 2018, Figure 1), Freeman-Tukey 
                residuals provide more reliable results.</li>
              </ul>
              
              <p><strong>Why overdispersion matters:</strong> Pearson residuals assume that cell frequencies 
              follow a Poisson distribution where the mean equals the variance. When this assumption is 
              violated (overdispersion), the variance exceeds the mean, and Pearson-based inferences become 
              unreliable. The Freeman-Tukey transformation stabilises the variance, making it constant 
              (equal to 1/4) regardless of the expected count. This is why Freeman-Tukey residuals are 
              recommended when overdispersion is detected.</p>
              
              <p><strong>Recommendation:</strong> If the diagnostic plot shows clustering to the left, 
              switch the Distance Metric option from 'Pearson (\u03c7\u00b2 distance)' to 'Freeman-Tukey 
              (Hellinger distance)' to obtain more robust results.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh, Lombardo & Alberti 2018.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 8: Inertia and Malinvaud's Test
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Inertia and Malinvaud's Test</h3>
              
              <p><em>Total inertia</em> is the sum of squared singular values (eigenvalues) from the SVD 
              decomposition. It equals the test statistic (\u03c7\u00b2 or T\u00b2) divided by the sample size n, and 
              measures the total association strength in the table. Each dimension captures a proportion 
              of this total inertia, shown in the inertia decomposition table.</p>
              
              <p><strong>Malinvaud's test</strong> (Saporta, 2006) assesses whether residual inertia remains 
              to be explained. In the row for dimension k, a non-significant result (p > 0.05) suggests 
              that the first k\u22121 dimensions adequately represent the data; a significant result suggests 
              that dimension k (and possibly further ones) carries structure worth interpreting.</p>
              
              <p>In the row for dimension k, the test statistic is n \u00d7 (inertia in dimensions k through M), 
              where M = min(I,J)\u22121, compared to a chi-squared distribution with (I\u2212k)(J\u2212k) degrees 
              of freedom. It therefore asks whether any structure remains after the first k\u22121 dimensions; 
              the row for dimension 1 is the overall test of independence.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Saporta 2006.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 9: Seriated Contingency Table
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Seriated Contingency Table</h3>
              
              <p>The seriated contingency table reorders rows and columns based on their principal 
              coordinates from the correspondence analysis, revealing gradient structures in the data. 
              This technique, also known as CA-based ordination or matrix seriation, is widely used in 
              ecology, archaeology, and other fields where detecting underlying gradients is important.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Sorting Algorithm</h4>
              
              <p>Both rows and columns are sorted by their principal coordinates on the X-axis dimension 
              (as selected in Analysis Settings), ordered from most negative to most positive. This 
              directly reflects the horizontal arrangement of categories in the symmetric CA plot.</p>
              
              <p>The X-axis typically displays the first principal dimension, which captures the largest 
              proportion of inertia (association) in the data. Sorting by this dimension therefore 
              reveals the primary gradient structure. To explore gradients along other dimensions, 
              change the 'Dimension for X-axis' setting in Analysis Settings.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Reading the Table</h4>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Cell values:</strong> The observed frequencies, reordered to reveal diagonal 
                or gradient patterns. If a strong gradient exists, high values should tend to cluster 
                along the diagonal.</li>
                <li><strong>Row marginals (rightmost columns):</strong> The principal coordinates for each 
                row category on the X-axis and Y-axis dimensions. These show the exact position of each 
                row in the CA space.</li>
                <li><strong>Column marginals (bottom rows):</strong> The principal coordinates for each 
                column category on the X-axis and Y-axis dimensions. These show the exact position of 
                each column in the CA space.</li>
              </ul>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Interpretation</h4>
              
              <p>A well-seriated table reveals the underlying structure of the data:</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Diagonal pattern:</strong> If high frequencies concentrate along the diagonal 
                from top-left to bottom-right (or vice versa), this indicates a strong gradient where 
                certain row categories consistently co-occur with certain column categories. In 
                archaeological contexts, this often reflects chronological change; in ecological data, 
                it may reveal environmental gradients.</li>
                <li><strong>Block structure:</strong> Clusters of high values in rectangular blocks suggest 
                groups of row and column categories that are mutually associated.</li>
                <li><strong>Scattered pattern:</strong> If no clear pattern emerges, the association 
                structure may be complex or weak, or the primary gradient may lie along a different 
                dimension.</li>
              </ul>
              
              <p><strong>Relationship to the plot:</strong> The table ordering directly corresponds to 
              the horizontal (X-axis) arrangement in the symmetric CA plot. Categories appearing on the 
              left side of the plot (negative coordinates) appear first in the table; those on the right 
              (positive coordinates) appear last.</p>
              
              <p><strong>Changing the sorting dimension:</strong> The seriated table sorts by whichever 
              dimension is selected as the X-axis in Analysis Settings. To seriate by a different 
              dimension, change the 'Dimension for X-axis' value. The Y-axis dimension coordinates are 
              still displayed in the marginals for reference, but do not affect the sort order.</p>
            ")
            
            # -----------------------------------------------------------------
            # Section: Attribute Assessment (Krzanowski 1993)
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Attribute Assessment (Krzanowski, 1993)</h3>
              
              <p>The attribute assessment facility identifies which column variables (attributes) contribute 
              most to the row configuration in a correspondence analysis. This is particularly useful when 
              analysing large tables where some columns may add little information to the ordination of rows.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Procrustes-Based Influence Measure</h4>
              
              <p>The method, developed by Krzanowski (1993), uses Procrustes analysis to measure how much 
              the row configuration changes when each attribute is removed:</p>
              
              <ol style='margin-left: 2em;'>
                <li>Perform CA on the full contingency table to obtain row principal coordinates <strong>X</strong>.</li>
                <li>For each column <em>j</em>, remove that column and perform CA on the reduced table to obtain 
                row coordinates <strong>Y</strong><sub>j</sub>.</li>
                <li>Use Procrustes rotation to optimally align <strong>Y</strong><sub>j</sub> with <strong>X</strong>, 
                allowing for translation, rotation, and uniform scaling.</li>
                <li>Compute the residual sum of squares M\u00b2<sub>j</sub> after alignment.</li>
              </ol>
              
              <p><strong>Interpretation:</strong> A <em>large</em> M\u00b2 indicates that removing attribute <em>j</em> 
              substantially distorts the row configuration\u2014the attribute is <em>important</em>. A <em>small</em> M\u00b2 
              indicates the attribute contributes little to distinguishing between rows and could potentially be 
              dropped without much loss of information.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Backward Elimination</h4>
              
              <p>The backward elimination procedure iteratively identifies an optimal subset of attributes:</p>
              
              <ol style='margin-left: 2em;'>
                <li>Compute M\u00b2 for all remaining attributes.</li>
                <li>Remove the attribute with the <em>smallest</em> M\u00b2 (least influential).</li>
                <li>Recompute all M\u00b2 values for the reduced table.</li>
                <li>Repeat until only a minimum number of attributes remain.</li>
              </ol>
              
              <p><strong>Elbow detection:</strong> The trajectory of M\u00b2 values typically remains low while 
              uninformative attributes are removed, then rises sharply when important attributes are eliminated. 
              The 'elbow' marks the transition point\u2014attributes removed before the elbow contribute minimally; 
              those after the elbow are essential.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Reduced-Data CA Plot</h4>
              
              <p>The reduced-data CA plot displays the correspondence analysis with low-influence attributes 
              either faded or hidden entirely. This allows visual comparison with the full-data symmetric plot 
              to assess whether the essential structure is preserved with fewer attributes.</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Fade:</strong> Low-influence attributes are shown with reduced opacity, allowing 
                comparison while de-emphasising their contribution.</li>
                <li><strong>Hide:</strong> Low-influence attributes are omitted entirely; CA is recomputed 
                on the reduced matrix.</li>
              </ul>
              
              <p><strong>Computational note:</strong> Backward elimination is computationally expensive for 
              large tables (O(c\u00b2) CA computations for c columns). The single-pass influence ranking is 
              faster (O(c) computations) and may suffice for initial screening.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Krzanowski 1993.</em></p>
            ")
            
            html <- paste0(html, "</div>")
            
            self$results$methodInfo$setContent(html)
        },
        
        # =====================================================================
        # POPULATE ATTRIBUTE INFLUENCE TABLE
        # =====================================================================
        .populateAttrInfluenceTable = function() {
            
            N <- private$.contingencyTable
            if (is.null(N)) return()
            
            table <- self$results$attrInfluenceTable
            
            # Compute influence ranking
            influence <- private$.assessAttributeInfluence(N)
            if (is.null(influence)) {
                table$setNote("error", 
                              "Could not compute attribute influence (matrix may be degenerate)",
                              init = FALSE)
                return()
            }
            
            # Store for use in plots
            private$.attrInfluence <- influence
            
            # Populate table
            for (i in 1:nrow(influence)) {
                table$addRow(rowKey = i, values = list(
                    rank = influence$rank[i],
                    attribute = influence$attribute[i],
                    M2 = if (is.infinite(influence$M2[i])) NA else influence$M2[i],
                    importance = if (is.infinite(influence$M2[i])) {
                        "Indispensable"
                    } else if (i <= 3) {
                        "High"
                    } else if (i <= nrow(influence) * 0.5) {
                        "Moderate"
                    } else {
                        "Low"
                    }
                ))
                
                # Format indispensable rows
                if (is.infinite(influence$M2[i])) {
                    table$addFormat(rowKey = i, col = "M2", jmvcore::Cell.NEGATIVE)
                }
            }
            
            # Add explanatory note
            ca <- private$.caResult
            resid_label <- if (!is.null(ca)) ca$resid_label else "Standardised"
            
            table$setNote("method", paste0(
                "Procrustes M\u00b2 based on ", resid_label, " residuals. ",
                "Higher M\u00b2 = more important (removing distorts row configuration more). ",
                "Krzanowski (1993)."),
                init = FALSE)
        },
        
    # =====================================================================
    # POPULATE BACKWARD ELIMINATION TABLE
    # =====================================================================
    .populateAttrEliminationTable = function() {
        
        N <- private$.contingencyTable
        if (is.null(N)) return()
        
        table <- self$results$attrEliminationTable
        
        # Compute elimination sequence
        elimination <- private$.assessAttributeElimination(N)
        if (is.null(elimination) || nrow(elimination) == 0) {
            table$setNote("error", "Could not perform backward elimination", init = FALSE)
            return()
        }
        
        # Detect elbow
        elbow_step <- private$.detectElbow(elimination$M2_at_removal)
        
        # Store for use in plots
        private$.attrElimination <- elimination
        private$.attrElbowStep <- elbow_step
        
        # Populate table
        for (i in 1:nrow(elimination)) {
            table$addRow(rowKey = i, values = list(
                step = elimination$step[i],
                attribute_removed = elimination$attribute_removed[i],
                M2_at_removal = elimination$M2_at_removal[i],
                n_remaining = elimination$n_remaining[i]
            ))
            
            # Highlight elbow row
            if (i == elbow_step) {
                table$addFormat(rowKey = i, col = "step", jmvcore::Cell.NEGATIVE)
            }
        }
        
        # Determine attributes to KEEP (those NOT removed before elbow)
        all_cols <- colnames(N)
        cols_dropped <- elimination$attribute_removed[1:min(elbow_step, nrow(elimination))]
        cols_retained <- setdiff(all_cols, cols_dropped)
        n_retained <- length(cols_retained)
        
        table$setNote("elbow", paste0(
            "Suggested elbow at step ", elbow_step, 
            " (retain ", n_retained, " of ", ncol(N), " attributes)."),
            init = FALSE)
        
        table$setNote("retained", paste0(
            "Suggested to keep: ", paste(cols_retained, collapse = ", ")),
            init = FALSE)
    },
        
        # =====================================================================
        # PLOT: SCREE
        # =====================================================================
        .plotScree = function(image, ggtheme, theme, ...) {
            # Refill private fields from image state (needed on export)
            private$.restoreFromState(image)
            
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showScreePlot) return(FALSE)
            
            ca <- private$.caResult
            
            scree_df <- data.frame(
                dim = factor(1:ca$n_axes),
                percent = ca$inertia_proportion * 100,
                cumulative = ca$inertia_cumulative * 100
            )
            
            p <- ggplot2::ggplot(scree_df, ggplot2::aes(x = dim)) + ggtheme  # jamovi results theme first; plot-specific tweaks follow
            p <- p + ggplot2::geom_bar(
                ggplot2::aes(y = percent),
                stat = "identity",
                fill = "#2171B5",
                colour = NA,
                width = 0.7
            )
            p <- p + ggplot2::geom_line(
                ggplot2::aes(y = cumulative, group = 1),
                colour = "#CB181D",
                linewidth = 1
            )
            p <- p + ggplot2::geom_point(
                ggplot2::aes(y = cumulative),
                colour = "#CB181D",
                size = 3
            )
            p <- p + ggplot2::geom_text(
                ggplot2::aes(y = cumulative, label = sprintf("%.1f%%", cumulative)),
                vjust = -0.5, size = 3, colour = "#CB181D"
            )
            
            p <- p + ggplot2::labs(
                title = paste0("Scree Plot (", ca$resid_label, " residuals)"),
                x = "Dimension",
                y = "% Inertia"
            )
            
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5)
            )
            
            print(p)
            return(TRUE)
        },
        
        # =====================================================================
        # PLOT: SYMMETRIC CA
        # =====================================================================
        .plotSymmetric = function(image, ggtheme, theme, ...) {
            # Refill private fields from image state (needed on export)
            private$.restoreFromState(image)
            
            # Early exit if data not ready
            if (is.null(self$options$rows) || is.null(self$options$cols)) return(FALSE)
            if (is.null(private$.contingencyTable)) return(FALSE)
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showSymmetricPlot) return(FALSE)
            
            ca <- private$.caResult
            
            # Additional validation - ensure we have valid coordinates
            if (is.null(ca$row_princ) || is.null(ca$col_princ)) return(FALSE)
            if (nrow(ca$row_princ) == 0 || nrow(ca$col_princ) == 0) return(FALSE)
            
            dim1 <- self$options$dim1
            dim2 <- self$options$dim2
            dotSize <- self$options$dotSize
            labelSize <- self$options$labelSize
            showClusters <- self$options$symmetricShowClusters
            clusterTarget <- self$options$symmetricClusterTarget
            showContrib <- self$options$symmetricShowContrib
            showQuality <- self$options$symmetricShowQuality
            showPoints <- self$options$symmetricShowPoints
            
            # Determine which point types to display
            showRowPoints <- showPoints %in% c("both", "rows")
            showColPoints <- showPoints %in% c("both", "cols")
            
            # Check dimension validity
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                return(FALSE)
            }
            
            # Contribution to the displayed plane (sum of contributions to dim1 and dim2)
            row_plane_contrib <- rowSums(ca$row_contrib[, c(dim1, dim2), drop = FALSE])
            col_plane_contrib <- rowSums(ca$col_contrib[, c(dim1, dim2), drop = FALSE])
            
            # Quality on the displayed plane (sum of cos² for dim1 and dim2)
            row_plane_quality <- rowSums(ca$row_cos2[, c(dim1, dim2), drop = FALSE])
            col_plane_quality <- rowSums(ca$col_cos2[, c(dim1, dim2), drop = FALSE])
            
            # Prepare data frames
            row_df <- data.frame(
                x = ca$row_princ[, dim1],
                y = ca$row_princ[, dim2],
                label = rownames(ca$N),
                type = "Row",
                contrib = row_plane_contrib,
                quality = row_plane_quality,
                stringsAsFactors = FALSE
            )
            
            col_df <- data.frame(
                x = ca$col_princ[, dim1],
                y = ca$col_princ[, dim2],
                label = colnames(ca$N),
                type = "Column",
                contrib = col_plane_contrib,
                quality = col_plane_quality,
                stringsAsFactors = FALSE
            )
            
            # Axis labels
            xlab <- sprintf("Dimension %d (%.1f%%)", dim1, ca$inertia_proportion[dim1] * 100)
            ylab <- sprintf("Dimension %d (%.1f%%)", dim2, ca$inertia_proportion[dim2] * 100)
            title <- paste0("Symmetric CA Plot (", ca$resid_label, " residuals)")
            
            # Build plot
            p <- ggplot2::ggplot() + ggtheme  # jamovi results theme first; plot-specific tweaks follow
            
            # Reference lines
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            
            # SRD clusters (if requested) - only draw for visible point types
            if (showClusters) {
                row_colours <- c("#E41A1C", "#FB6A4A", "#FC9272", "#FCBBA1")
                col_colours <- c("#2171B5", "#4292C6", "#6BAED6", "#9ECAE1")
                
                draw_clusters <- function(p, srd_result, point_df, colours, linetype = "dashed") {
                    if (is.null(srd_result) || length(srd_result$groups) == 0) return(p)
                    
                    for (g in seq_along(srd_result$groups)) {
                        members <- srd_result$groups[[g]]$members
                        member_idx <- which(point_df$label %in% members)
                        cluster_colour <- colours[(g - 1) %% length(colours) + 1]
                        
                        if (length(member_idx) >= 2) {
                            # Padded outline: visible even when members almost coincide
                            padX <- 0.03 * diff(range(point_df$x))
                            padY <- 0.03 * diff(range(point_df$y))
                            if (!is.finite(padX) || padX == 0) padX <- 0.02
                            if (!is.finite(padY) || padY == 0) padY <- 0.02
                            hull_df <- .ctClusterHull(point_df$x[member_idx], point_df$y[member_idx], padX, padY)
                            p <- p + ggplot2::geom_polygon(
                                data = hull_df,
                                ggplot2::aes(x = x, y = y),
                                fill = cluster_colour,
                                alpha = 0.12,
                                colour = cluster_colour,
                                linetype = linetype,
                                linewidth = 0.6
                            )
                        }
                    }
                    return(p)
                }
                
                # Only draw clusters for point types that are visible
                if (showRowPoints && (clusterTarget == "rows" || clusterTarget == "both")) {
                    p <- draw_clusters(p, private$.srdResultRows, row_df, row_colours, "dashed")
                }
                if (showColPoints && (clusterTarget == "cols" || clusterTarget == "both")) {
                    p <- draw_clusters(p, private$.srdResultCols, col_df, col_colours, "dotted")
                }
            }
            
            # Determine which aesthetics to map
            # Note: contribution has no "both" option
            # Only apply aesthetics if the corresponding points are visible
            useContribRows <- showContrib == "rows" && showRowPoints
            useContribCols <- showContrib == "cols" && showColPoints
            useQualityRows <- showQuality %in% c("rows", "both") && showRowPoints
            useQualityCols <- showQuality %in% c("cols", "both") && showColPoints
            
            # -----------------------------------------------------------------
            # ROW POINTS (only if visible)
            # -----------------------------------------------------------------
            if (showRowPoints) {
                if (useQualityRows && useContribRows) {
                    # Both quality (colour) and contribution (size)
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y, fill = quality, size = contrib),
                        colour = "#67000D", shape = 21, stroke = 0.6
                    )
                } else if (useQualityRows) {
                    # Quality only (colour)
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y, fill = quality),
                        colour = "#67000D", shape = 21, size = dotSize, stroke = 0.6
                    )
                } else if (useContribRows) {
                    # Contribution only (size)
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y, size = contrib),
                        colour = "#CB181D", fill = "white", shape = 21, stroke = 0.95
                    )
                } else {
                    # Neither - default appearance
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y),
                        colour = "#CB181D", fill = "white", shape = 21, size = dotSize, stroke = 0.95
                    )
                }
            }
            
            # -----------------------------------------------------------------
            # COLUMN POINTS (only if visible)
            # -----------------------------------------------------------------
            if (showColPoints) {
                if (useQualityCols && useContribCols) {
                    # Both quality (colour) and contribution (size)
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y, colour = quality, size = contrib),
                        shape = 17
                    )
                } else if (useQualityCols) {
                    # Quality only (colour)
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y, colour = quality),
                        shape = 17, size = dotSize
                    )
                } else if (useContribCols) {
                    # Contribution only (size)
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y, size = contrib),
                        colour = "#2171B5", shape = 17
                    )
                } else {
                    # Neither - default appearance
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y),
                        colour = "#2171B5", shape = 17, size = dotSize
                    )
                }
            }
            
            # -----------------------------------------------------------------
            # SCALES
            # -----------------------------------------------------------------
            # Size scale (contribution)
            if (useContribRows || useContribCols) {
                # Determine range based on which categories are shown
                all_contribs <- c()
                if (useContribRows) all_contribs <- c(all_contribs, row_df$contrib)
                if (useContribCols) all_contribs <- c(all_contribs, col_df$contrib)
                
                p <- p + ggplot2::scale_size_continuous(
                    name = "Contribution (%)",
                    range = c(dotSize * 0.5, dotSize * 2.5),
                    limits = c(0, max(all_contribs, na.rm = TRUE))
                )
            }
            
            # Colour/fill scales (quality)
            # We need separate handling for fill (rows) and colour (columns)
            if (useQualityRows && useQualityCols) {
                # Both - use fill for rows, colour for columns
                p <- p + ggplot2::scale_fill_gradient(
                    low = "#FCBBA1", high = "#67000D",
                    name = "Row quality (cos\u00b2)",
                    limits = c(0, 1)
                )
                p <- p + ggplot2::scale_colour_gradient(
                    low = "#C6DBEF", high = "#08519C",
                    name = "Col quality (cos\u00b2)",
                    limits = c(0, 1)
                )
            } else if (useQualityRows) {
                # Rows only - red gradient
                p <- p + ggplot2::scale_fill_gradient(
                    low = "#FCBBA1", high = "#67000D",
                    name = "Quality (cos\u00b2)",
                    limits = c(0, 1)
                )
            } else if (useQualityCols) {
                # Columns only - blue gradient
                p <- p + ggplot2::scale_colour_gradient(
                    low = "#C6DBEF", high = "#08519C",
                    name = "Quality (cos\u00b2)",
                    limits = c(0, 1)
                )
            }
            
            # -----------------------------------------------------------------
            # LABELS (only for visible point types)
            # -----------------------------------------------------------------
            row_df$label_colour <- "#CB181D"
            col_df$label_colour <- "#2171B5"
            
            # Build combined labels only from visible point types
            label_parts <- list()
            if (showRowPoints) {
                label_parts[[length(label_parts) + 1]] <- row_df[, c("x", "y", "label", "label_colour"), drop = FALSE]
            }
            if (showColPoints) {
                label_parts[[length(label_parts) + 1]] <- col_df[, c("x", "y", "label", "label_colour"), drop = FALSE]
            }
            
            if (length(label_parts) > 0) {
                combined_labels <- do.call(rbind, label_parts)
                
                p <- p + ggrepel::geom_text_repel(
                    data = combined_labels,
                    ggplot2::aes(x = x, y = y, label = label),
                    colour = combined_labels$label_colour,
                    size = labelSize,
                    max.overlaps = 30,
                    box.padding = 0.4,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.3,
                    min.segment.length = 0.5,
                    segment.colour = "grey50",
                    segment.size = 0.4,
                    seed = 42
                )
            }
            
            # -----------------------------------------------------------------
            # THEME
            # -----------------------------------------------------------------
            p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
            
            # Determine legend position based on what's shown
            hasLegend <- (useContribRows || useContribCols || useQualityRows || useQualityCols)
            
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = if (hasLegend) "right" else "none",
                legend.background = ggplot2::element_rect(fill = "white", colour = NA),
                legend.title = ggplot2::element_text(size = 9),
                legend.text = ggplot2::element_text(size = 8)
            )
            
            if (self$options$equalScale) {
                p <- p + ggplot2::coord_equal()
            }
            
            # -----------------------------------------------------------------
            # CAPTION (dynamically reflects which points are shown)
            # -----------------------------------------------------------------
            if (showRowPoints && showColPoints) {
                caption_text <- "Red (\u25cb) = Row categories; Blue (\u25b2) = Column categories"
            } else if (showRowPoints) {
                caption_text <- "Red (\u25cb) = Row categories"
            } else {
                caption_text <- "Blue (\u25b2) = Column categories"
            }
            
            if (showClusters) {
                # Only mention clusters for visible point types
                cluster_parts <- c()
                
                # Get method label for caption
                method <- self$options$clusterMethod
                method_label <- switch(method,
                                       srd = "SRD",
                                       greenacre = "Greenacre",
                                       husson = "Husson"
                )
                
                nRowGroups <- length(private$.srdResultRows$groups)
                nColGroups <- length(private$.srdResultCols$groups)
                if (showRowPoints && (clusterTarget == "rows" || clusterTarget == "both")) {
                    cluster_parts <- c(cluster_parts, if (nRowGroups > 0)
                        paste0("Dashed red = Row clusters (", method_label, ")") else
                        paste0("Row clusters (", method_label, "): none, all categories distinct"))
                }
                if (showColPoints && (clusterTarget == "cols" || clusterTarget == "both")) {
                    cluster_parts <- c(cluster_parts, if (nColGroups > 0)
                        paste0("Dotted blue = Column clusters (", method_label, ")") else
                        paste0("Column clusters (", method_label, "): none, all categories distinct"))
                }
                if (length(cluster_parts) > 0) {
                    caption_text <- paste0(caption_text, "\n", paste(cluster_parts, collapse = "; "))
                }
            }
            
            p <- p + ggplot2::labs(caption = caption_text)
            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            # Wrap print in tryCatch to handle ggrepel viewport errors
            tryCatch({
                print(p)
                return(TRUE)
            }, error = function(e) {
                # Silently fail if viewport error occurs during initial render
                return(FALSE)
            })
        },
        
        # =====================================================================
        # PLOT: GEOMETRIC INTERPRETATION
        # =====================================================================
        .plotGeometric = function(image, ggtheme, theme, ...) {
            # Refill private fields from image state (needed on export)
            private$.restoreFromState(image)
            
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showGeometricPlot) return(FALSE)
            
            ca <- private$.caResult
            
            dim1 <- self$options$dim1
            dim2 <- self$options$dim2
            dotSize <- self$options$dotSize
            labelSize <- self$options$labelSize
            refVariable <- self$options$geometricRefVariable
            
            # Get reference category based on which variable is selected
            if (refVariable == "rows") {
                refCategory <- self$options$geometricRefCategoryRow
            } else {
                refCategory <- self$options$geometricRefCategoryCol
            }
            
            # Check dimension validity
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                return(FALSE)
            }
            
            # Validate reference category
            if (is.null(refCategory) || refCategory == "") {
                # Default to first category of the selected variable
                if (refVariable == "rows") {
                    refCategory <- rownames(ca$N)[1]
                } else {
                    refCategory <- colnames(ca$N)[1]
                }
            }
            
            # Get coordinates based on reference variable
            if (refVariable == "rows") {
                ref_coord <- ca$row_princ
                other_coord <- ca$col_princ
                ref_names <- rownames(ca$N)
                other_names <- colnames(ca$N)
                ref_label <- "Row"
                other_label <- "Column"
                ref_colour <- "#CB181D"
                other_colour <- "#2171B5"
            } else {
                ref_coord <- ca$col_princ
                other_coord <- ca$row_princ
                ref_names <- colnames(ca$N)
                other_names <- rownames(ca$N)
                ref_label <- "Column"
                other_label <- "Row"
                ref_colour <- "#2171B5"
                other_colour <- "#CB181D"
            }
            
            # Check if reference category exists
            if (!(refCategory %in% ref_names)) {
                # Fall back to first category
                refCategory <- ref_names[1]
            }
            
            ref_idx <- which(ref_names == refCategory)
            
            # Reference category coordinates
            ref_x <- ref_coord[ref_idx, dim1]
            ref_y <- ref_coord[ref_idx, dim2]
            
            # Calculate angle theta for the reference line
            theta <- atan2(ref_y, ref_x)
            
            # Prepare data frames
            ref_df <- data.frame(
                x = ref_coord[, dim1],
                y = ref_coord[, dim2],
                label = ref_names,
                is_reference = ref_names == refCategory,
                stringsAsFactors = FALSE
            )
            
            other_df <- data.frame(
                x = other_coord[, dim1],
                y = other_coord[, dim2],
                label = other_names,
                stringsAsFactors = FALSE
            )
            
            # Rotate other coordinates to find projections
            other_df$rot_x <- cos(-theta) * other_df$x - sin(-theta) * other_df$y
            other_df$rot_y <- sin(-theta) * other_df$x + cos(-theta) * other_df$y
            
            # Project back to original coordinate system
            other_df$proj_x <- cos(theta) * other_df$rot_x
            other_df$proj_y <- sin(theta) * other_df$rot_x
            
            # Determine if projection is on same side as reference (positive rot_x)
            other_df$same_side <- other_df$rot_x > 0
            
            # Create segments data frame
            segments_df <- data.frame(
                x = other_df$x,
                y = other_df$y,
                xend = other_df$proj_x,
                yend = other_df$proj_y,
                same_side = other_df$same_side,
                stringsAsFactors = FALSE
            )
            
            # Axis labels
            xlab <- sprintf("Dimension %d (%.1f%%)", dim1, ca$inertia_proportion[dim1] * 100)
            ylab <- sprintf("Dimension %d (%.1f%%)", dim2, ca$inertia_proportion[dim2] * 100)
            title <- paste0("Geometric Interpretation CA Plot (ref: ", refCategory, ")")
            
            # Build plot
            p <- ggplot2::ggplot() + ggtheme  # jamovi results theme first; plot-specific tweaks follow
            
            # Reference lines (axes)
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70", linetype = "dashed")
            p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70", linetype = "dashed")
            
            # Line through origin and reference category
            p <- p + ggplot2::geom_abline(
                slope = tan(theta),
                intercept = 0,
                colour = "black",
                linewidth = 0.5
            )
            
            # Projection segments - solid for same side, dashed for opposite
            segments_same <- segments_df[segments_df$same_side, ]
            segments_opp <- segments_df[!segments_df$same_side, ]
            
            if (nrow(segments_same) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = segments_same,
                    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                    linetype = "solid",
                    linewidth = 0.4,
                    colour = "grey50"
                )
            }
            
            if (nrow(segments_opp) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = segments_opp,
                    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                    linetype = "dotted",
                    linewidth = 0.4,
                    colour = "grey50"
                )
            }
            
            # Reference variable categories - white-filled circles with outline
            # Reference category highlighted
            ref_other <- ref_df[!ref_df$is_reference, ]
            ref_main <- ref_df[ref_df$is_reference, ]
            
            # Other categories from reference variable (greyed out)
            if (nrow(ref_other) > 0) {
                p <- p + ggplot2::geom_point(
                    data = ref_other,
                    ggplot2::aes(x = x, y = y),
                    colour = "grey60", fill = "white", shape = 21, size = dotSize, stroke = 0.8
                )
            }
            
            # Reference category itself (highlighted)
            p <- p + ggplot2::geom_point(
                data = ref_main,
                ggplot2::aes(x = x, y = y),
                colour = ref_colour, fill = "white", shape = 21, size = dotSize * 1.3, stroke = 1.2
            )
            
            # Other variable categories (triangles)
            p <- p + ggplot2::geom_point(
                data = other_df,
                ggplot2::aes(x = x, y = y),
                colour = other_colour, shape = 17, size = dotSize
            )
            
            # Combine all labels for unified repulsion
            label_parts <- list()
            if (nrow(ref_other) > 0) {
                ref_other$colour <- "grey60"
                ref_other$fontface <- "plain"
                label_parts[[length(label_parts) + 1]] <- ref_other[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            }
            ref_main$colour <- ref_colour
            ref_main$fontface <- "bold"
            label_parts[[length(label_parts) + 1]] <- ref_main[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            other_df$colour <- other_colour
            other_df$fontface <- "plain"
            label_parts[[length(label_parts) + 1]] <- other_df[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            
            combined_labels <- do.call(rbind, label_parts)
            
            p <- p + ggrepel::geom_text_repel(
                data = combined_labels,
                ggplot2::aes(x = x, y = y, label = label, colour = colour, fontface = fontface),
                size = labelSize,
                max.overlaps = 30,
                box.padding = 0.4,
                point.padding = 0.3,
                force = 2,
                force_pull = 0.3,
                min.segment.length = 0.5,
                segment.colour = "grey50",
                segment.size = 0.4,
                seed = 42
            )
            p <- p + ggplot2::scale_colour_identity()
            
            # Theme
            p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = "none"
            )
            
            if (self$options$equalScale) {
                p <- p + ggplot2::coord_equal()
            }
            
            # Caption
            caption_text <- paste0(
                "Solid segments: above-average association with ", refCategory, "\n",
                "Dotted segments: below-average association with ", refCategory
            )
            p <- p + ggplot2::labs(caption = caption_text)
            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            print(p)
            return(TRUE)
        },
        
        # =====================================================================
        # PLOT: CONTRIBUTION BIPLOT
        # =====================================================================
        .plotContribBiplot = function(image, ggtheme, theme, ...) {
            # Refill private fields from image state (needed on export)
            private$.restoreFromState(image)
            
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showContribPlot) return(FALSE)
            
            ca <- private$.caResult
            srd <- private$.runSRD()
            
            dim1 <- self$options$dim1
            dim2 <- self$options$dim2
            dotSize <- self$options$dotSize
            labelSize <- self$options$labelSize
            focus <- self$options$contribFocus
            showClusters <- self$options$contribShowClusters
            
            # Check dimension validity
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                return(FALSE)
            }
            
            # Determine which variable defines the plane
            if (focus == "cols") {
                # Columns define the plane, rows are interpreted
                focus_contrib <- ca$col_contrib_coord
                focus_mass <- ca$col_mass
                focus_contrib_pct <- ca$col_contrib
                focus_names <- colnames(ca$N)
                n_focus <- ca$n_cols
                focus_label <- "Column"
                
                interp_princ <- ca$row_princ
                interp_cos2 <- ca$row_cos2
                interp_names <- rownames(ca$N)
                interp_label <- "Row"
            } else {
                # Rows define the plane, columns are interpreted
                focus_contrib <- ca$row_contrib_coord
                focus_mass <- ca$row_mass
                focus_contrib_pct <- ca$row_contrib
                focus_names <- rownames(ca$N)
                n_focus <- ca$n_rows
                focus_label <- "Row"
                
                interp_princ <- ca$col_princ
                interp_cos2 <- ca$col_cos2
                interp_names <- colnames(ca$N)
                interp_label <- "Column"
            }
            
            avg_contrib <- 100 / n_focus
            
            # Build data frames
            focus_df <- data.frame(
                x = focus_contrib[, dim1],
                y = focus_contrib[, dim2],
                label = focus_names,
                contrib_d1 = focus_contrib_pct[, dim1],
                contrib_d2 = focus_contrib_pct[, dim2],
                stringsAsFactors = FALSE
            )
            
            focus_df$above_avg <- (!is.na(focus_df$contrib_d1) & focus_df$contrib_d1 >= avg_contrib) | 
                (!is.na(focus_df$contrib_d2) & focus_df$contrib_d2 >= avg_contrib)
            # Handle potential NA values defensively
            focus_df$above_avg[is.na(focus_df$above_avg)] <- FALSE
            focus_df$alpha <- ifelse(focus_df$above_avg, 1.0, 0.4)
            focus_df$fontface <- ifelse(focus_df$above_avg, "bold", "plain")
            
            interp_df <- data.frame(
                x = interp_princ[, dim1],
                y = interp_princ[, dim2],
                label = interp_names,
                quality = rowSums(interp_cos2[, c(dim1, dim2), drop = FALSE]),
                stringsAsFactors = FALSE
            )
            interp_df$alpha <- 1.0
            
            # Axis labels
            xlab <- sprintf("Dimension %d (%.1f%%)", dim1, ca$inertia_proportion[dim1] * 100)
            ylab <- sprintf("Dimension %d (%.1f%%)", dim2, ca$inertia_proportion[dim2] * 100)
            title <- paste0("Contribution CA Biplot (", ca$resid_label, " residuals)")
            
            # Build plot
            p <- ggplot2::ggplot() + ggtheme  # jamovi results theme first; plot-specific tweaks follow
            
            # Reference lines
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            
            # SRD cluster hulls (if requested)
            if (showClusters && !is.null(srd) && length(srd$groups) > 0) {
                cluster_colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                                     "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
                
                for (g in seq_along(srd$groups)) {
                    members <- srd$groups[[g]]$members
                    member_idx <- which(interp_df$label %in% members)
                    cluster_colour <- cluster_colours[(g - 1) %% length(cluster_colours) + 1]
                    
                    if (length(member_idx) >= 2) {
                        # Padded outline: visible even when members almost coincide
                        padX <- 0.03 * diff(range(interp_df$x))
                        padY <- 0.03 * diff(range(interp_df$y))
                        if (!is.finite(padX) || padX == 0) padX <- 0.02
                        if (!is.finite(padY) || padY == 0) padY <- 0.02
                        hull_df <- .ctClusterHull(interp_df$x[member_idx], interp_df$y[member_idx], padX, padY)
                        p <- p + ggplot2::geom_polygon(
                            data = hull_df,
                            ggplot2::aes(x = x, y = y),
                            fill = cluster_colour,
                            alpha = 0.12,
                            colour = cluster_colour,
                            linetype = "dashed",
                            linewidth = 0.6
                        )
                    }
                }
            }
            
            # Lines from origin are drawn after splitting focus_df (below)
            
            # Plane-defining points - split into above-average and below-average
            # to ensure proper fill opacity (white fill must be fully opaque)
            focus_above <- focus_df[focus_df$above_avg, , drop = FALSE]
            focus_below <- focus_df[!focus_df$above_avg, , drop = FALSE]
            
            # Lines from origin to plane-defining categories
            # Below-average: dashed, lighter colour
            if (nrow(focus_below) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = focus_below,
                    ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
                    colour = "#9ECAE1",
                    linewidth = 0.4,
                    linetype = "dashed"
                )
            }
            
            # Above-average: solid, darker colour
            if (nrow(focus_above) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = focus_above,
                    ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
                    colour = "#2171B5",
                    linewidth = 0.5,
                    linetype = "solid"
                )
            }
            
            # Below-average contributors: lighter outline, solid white fill
            if (nrow(focus_below) > 0) {
                p <- p + ggplot2::geom_point(
                    data = focus_below,
                    ggplot2::aes(x = x, y = y),
                    colour = "#9ECAE1", fill = "white", shape = 21, size = dotSize, stroke = 0.8
                )
            }
            
            # Above-average contributors: solid outline, solid white fill
            if (nrow(focus_above) > 0) {
                p <- p + ggplot2::geom_point(
                    data = focus_above,
                    ggplot2::aes(x = x, y = y),
                    colour = "#2171B5", fill = "white", shape = 21, size = dotSize, stroke = 1.1
                )
            }
            
            # Interpreted variable - optionally coloured by quality
            if (self$options$showQuality) {
                p <- p + ggplot2::geom_point(
                    data = interp_df,
                    ggplot2::aes(x = x, y = y, colour = quality),
                    shape = 17, size = dotSize
                )
                p <- p + ggplot2::scale_colour_gradient(
                    low = "#FCBBA1", high = "#67000D",
                    name = "Quality (cos\u00b2)"
                )
            } else {
                p <- p + ggplot2::geom_point(
                    data = interp_df,
                    ggplot2::aes(x = x, y = y, alpha = alpha),
                    colour = "#CB181D", shape = 17, size = dotSize
                )
            }
            
            p <- p + ggplot2::scale_alpha_identity()
            
            # Labels
            if (self$options$hideWeakContrib) {
                focus_show <- focus_df[focus_df$above_avg, , drop = FALSE]
            } else {
                focus_show <- focus_df
            }
            
            # Combine both data frames for unified label repulsion
            label_parts <- list()
            if (nrow(focus_show) > 0) {
                focus_show$colour <- "#2171B5"
                label_parts[[1]] <- focus_show[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            }
            interp_df$colour <- "#CB181D"
            interp_df$fontface <- "plain"
            label_parts[[length(label_parts) + 1]] <- interp_df[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            
            combined_labels <- do.call(rbind, label_parts)
            
            if (nrow(combined_labels) > 0) {
                if (self$options$showQuality) {
                    # When showing quality gradient, use manual scale for labels
                    p <- p + ggrepel::geom_text_repel(
                        data = combined_labels,
                        ggplot2::aes(x = x, y = y, label = label, fontface = fontface),
                        colour = combined_labels$colour,
                        size = labelSize,
                        max.overlaps = 30,
                        box.padding = 0.4,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.3,
                        min.segment.length = 0.5,
                        segment.colour = "grey50",
                        segment.size = 0.4,
                        seed = 42
                    )
                } else {
                    p <- p + ggrepel::geom_text_repel(
                        data = combined_labels,
                        ggplot2::aes(x = x, y = y, label = label, colour = colour, fontface = fontface),
                        size = labelSize,
                        max.overlaps = 30,
                        box.padding = 0.4,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.3,
                        min.segment.length = 0.5,
                        segment.colour = "grey50",
                        segment.size = 0.4,
                        seed = 42
                    )
                    p <- p + ggplot2::scale_colour_identity()
                }
            }
            
            # Theme
            p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = if (self$options$showQuality) "right" else "none",
                legend.background = ggplot2::element_rect(fill = "white", colour = NA)
            )
            
            if (self$options$equalScale) {
                p <- p + ggplot2::coord_equal()
            }
            
            # Caption
            caption_text <- paste0(
                "Blue (\u25cf) = ", focus_label, " categories (darker outline = above-average contributor)\n",
                "Red (\u25b2) = ", interp_label, " categories"
            )
            if (showClusters) {
                has_row_clusters <- !is.null(private$.clusterResultRows) && 
                    length(private$.clusterResultRows$groups) > 0
                has_col_clusters <- !is.null(private$.clusterResultCols) && 
                    length(private$.clusterResultCols$groups) > 0
                
                if (has_row_clusters || has_col_clusters) {
                    method <- self$options$clusterMethod
                    method_label <- switch(method,
                                           srd = "SRD",
                                           greenacre = "Greenacre",
                                           husson = "Husson"
                    )
                    caption_text <- paste0(caption_text, "\nDashed regions = ", method_label, " clusters")
                } else {
                    method_label <- switch(self$options$clusterMethod,
                                           srd = "SRD", greenacre = "Greenacre", husson = "Husson")
                    caption_text <- paste0(caption_text, "\nClusters (", method_label,
                                           "): none, all categories distinct")
                }
            }
            
            p <- p + ggplot2::labs(caption = caption_text)
            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            print(p)
            return(TRUE)
        },
        
        # =====================================================================
        # PLOT: OVERDISPERSION DIAGNOSTIC
        # =====================================================================
        .plotOverdispersion = function(image, ggtheme, theme, ...) {
            # Refill private fields from image state (needed on export)
            private$.restoreFromState(image)
            
            if (is.null(private$.contingencyTable)) return(FALSE)
            if (!self$options$showOverdispersionPlot) return(FALSE)
            
            N <- private$.contingencyTable
            n <- sum(N)
            I <- nrow(N)
            J <- ncol(N)
            
            # Row and column marginal proportions
            r <- rowSums(N) / n
            c <- colSums(N) / n
            
            # Compute for each cell
            sd_indep <- numeric(I * J)
            pearson_resid <- numeric(I * J)
            
            idx <- 0
            for (i in 1:I) {
                for (j in 1:J) {
                    idx <- idx + 1
                    
                    e_ij <- n * r[i] * c[j]
                    sd_indep[idx] <- sqrt(n * r[i] * c[j])
                    
                    if (e_ij > 0) {
                        pearson_resid[idx] <- (N[i, j] - e_ij) / sqrt(e_ij)
                    } else {
                        pearson_resid[idx] <- 0
                    }
                }
            }
            
            plot_df <- data.frame(
                sd_indep = sd_indep,
                pearson_resid = pearson_resid,
                stringsAsFactors = FALSE
            )
            
            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = sd_indep, y = pearson_resid)) + ggtheme  # jamovi results theme first; plot-specific tweaks follow
            
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey60", linetype = "dashed")
            
            p <- p + ggplot2::geom_point(
                colour = "#2171B5",
                size = 2.5,
                alpha = 0.7
            )
            
            p <- p + ggplot2::labs(
                title = "Overdispersion Diagnostic Plot",
                x = expression(paste("SD under independence: ", sqrt(n %.% p[i.] %.% p[.j]))),
                y = expression(paste("Pearson residual: ", (n[ij] - e[ij]) / sqrt(e[ij])))
            )
            
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(colour = "grey90"),
                panel.grid.minor = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            p <- p + ggplot2::labs(
                caption = "Random scatter suggests no overdispersion. Problematic patterns include:\nclustering to the left, diagonal banding, or fan-shaped spread.\nIf present, consider using Freeman-Tukey residuals."
            )
            
            print(p)
            return(TRUE)
        }
        
        ,
        
    # =====================================================================
    # PLOT: ATTRIBUTE INFLUENCE BAR CHART
    # =====================================================================
    .plotAttrInfluence = function(image, ggtheme, theme, ...) {
        # Refill private fields from image state (needed on export)
        private$.restoreFromState(image)
        
        if (!self$options$showAttrInfluencePlot) return(FALSE)
        if (is.null(private$.attrInfluence)) return(FALSE)
        
        N <- private$.contingencyTable
        
        # Ensure elimination has been computed for consistent colouring
        if (is.null(private$.attrElimination) && !is.null(N)) {
            elimination <- private$.assessAttributeElimination(N)
            if (!is.null(elimination) && nrow(elimination) > 0) {
                private$.attrElimination <- elimination
                private$.attrElbowStep <- private$.detectElbow(elimination$M2_at_removal)
            }
        }
        
        influence <- private$.attrInfluence
        
        # Show ALL categories
        plot_data <- influence
        
        # Cap infinite values for display
        plot_data$M2_display <- plot_data$M2
        has_inf <- is.infinite(plot_data$M2_display)
        if (any(has_inf)) {
            finite_max <- max(plot_data$M2_display[!has_inf], na.rm = TRUE)
            plot_data$M2_display[has_inf] <- finite_max * 1.2
        }
        
        # Determine which are low-influence based on elimination elbow
        low_influence_attrs <- private$.getLowInfluenceAttributes()
        
        # Mark categories BEFORE converting to factor
        plot_data$category <- ifelse(plot_data$attribute %in% low_influence_attrs,
                                     "Low influence (drop)", "High influence (keep)")
        
        # Now convert to factor for correct plot ordering (most important at top)
        plot_data$attribute <- factor(plot_data$attribute, 
                                      levels = rev(plot_data$attribute))
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = attribute, y = M2_display, 
                                                     fill = category)) +
            ggtheme +
            ggplot2::geom_col(width = 0.7) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_manual(
                values = c("High influence (keep)" = "#2171B5", 
                           "Low influence (drop)" = "#BDBDBD"),
                name = NULL
            ) +
            ggplot2::labs(
                x = NULL,
                y = expression(M^2 ~ "(Procrustes residual)"),
                title = "Attribute Influence on Row Configuration"
            ) +
            ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_line(colour = "grey90"),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.y = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 8, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 10, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = "bottom",
                legend.text = ggplot2::element_text(size = 8)
            )
        
        print(p)
        TRUE
    },
        
    # =====================================================================
    # PLOT: BACKWARD ELIMINATION TRAJECTORY
    # =====================================================================
    .plotAttrElimination = function(image, ggtheme, theme, ...) {
        # Refill private fields from image state (needed on export)
        private$.restoreFromState(image)
        
        if (!self$options$showAttrEliminationPlot) return(FALSE)
        if (is.null(private$.attrElimination)) return(FALSE)
        
        elimination <- private$.attrElimination
        elbow_step <- private$.attrElbowStep
        if (is.null(elbow_step)) elbow_step <- nrow(elimination) %/% 2
        
        plot_data <- data.frame(
            step = elimination$step,
            M2 = elimination$M2_at_removal,
            label = elimination$attribute_removed
        )
        
        # Number of attributes retained at elbow
        n_original <- ncol(private$.contingencyTable)
        n_retained <- n_original - elbow_step
        
        # Calculate y position for annotation (middle of y-range)
        y_range <- range(plot_data$M2)
        y_annotation <- y_range[1] + (y_range[2] - y_range[1]) * 0.5
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = step, y = M2)) +
            ggtheme +
            ggplot2::geom_line(colour = "#2171B5", linewidth = 0.8) +
            ggplot2::geom_point(colour = "#2171B5", size = 2) +
            ggplot2::geom_vline(xintercept = elbow_step, linetype = "dashed", 
                                colour = "#E41A1C", linewidth = 0.8) +
            ggplot2::annotate("label", 
                              x = elbow_step, 
                              y = y_annotation,
                              label = paste0("Elbow (retain ", n_retained, ")"),
                              hjust = 1.1,
                              colour = "#E41A1C", 
                              size = 3,
                              fill = "white",
                              label.size = 0.3) +
            ggplot2::labs(
                x = "Elimination step",
                y = expression(M^2 ~ "at removal"),
                title = "Backward Elimination Sequence"
            ) +
            ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(colour = "grey90"),
                panel.grid.minor = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 10, face = "bold", colour = "grey20", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40", hjust = 0.5)
            )
        
        print(p)
        TRUE
    },
        
    # =====================================================================
    # PLOT: REDUCED-DATA CA PLOT
    # =====================================================================
    .plotReducedCA = function(image, ggtheme, theme, ...) {
        # Refill private fields from image state (needed on export)
        private$.restoreFromState(image)
        
        if (!self$options$showReducedCAPlot) return(FALSE)
        if (is.null(private$.contingencyTable)) return(FALSE)
        
        N <- private$.contingencyTable
        ca_full <- private$.caResult
        if (is.null(ca_full)) return(FALSE)
        
        # Ensure elimination has been computed (needed for low-influence detection)
        if (is.null(private$.attrElimination)) {
            elimination <- private$.assessAttributeElimination(N)
            if (!is.null(elimination) && nrow(elimination) > 0) {
                private$.attrElimination <- elimination
                private$.attrElbowStep <- private$.detectElbow(elimination$M2_at_removal)
            }
        }
        
        if (is.null(private$.attrElimination) || nrow(private$.attrElimination) == 0) return(FALSE)
        
        dim1 <- self$options$dim1
        dim2 <- self$options$dim2
        dotSize <- self$options$dotSize
        labelSize <- self$options$labelSize
        attrDisplay <- tolower(self$options$reducedPlotAttrDisplay)  # Handle case
        
        # Get low-influence attributes
        low_influence_attrs <- private$.getLowInfluenceAttributes()
        
        # For "hide" mode, exclude low-influence columns from the analysis
        if (attrDisplay == "hide" && length(low_influence_attrs) > 0) {
            cols_to_keep <- setdiff(colnames(N), low_influence_attrs)
            if (length(cols_to_keep) < 2) return(FALSE)
            N_plot <- N[, cols_to_keep, drop = FALSE]
            use_reduced_ca <- TRUE
        } else {
            N_plot <- N
            use_reduced_ca <- FALSE
        }
        
        # Run CA on the appropriate matrix
        distance_metric <- self$options$distanceMetric
        residual_type <- self$options$residualType
        
        I <- nrow(N_plot)
        J <- ncol(N_plot)
        
        grand_total <- sum(N_plot)
        P <- N_plot / grand_total
        r <- rowSums(P)
        c <- colSums(P)
        
        r_mat <- matrix(r, nrow = I, ncol = J, byrow = FALSE)
        c_mat <- matrix(c, nrow = I, ncol = J, byrow = TRUE)
        E <- r_mat * c_mat
        
        if (distance_metric == "freemanTukey") {
            S <- 2 * (sqrt(P) - sqrt(E))
            resid_label <- "Freeman-Tukey"
        } else if (residual_type == "adjusted") {
            S <- private$.computeAdjustedResiduals(P, E, r, c, I, J)
            resid_label <- "Adjusted standardised"
        } else {
            S <- (P - E) / sqrt(E + 1e-16)
            resid_label <- "Standardised"
        }
        
        svd_result <- tryCatch(svd(S), error = function(e) NULL)
        if (is.null(svd_result)) return(FALSE)
        
        d <- svd_result$d
        
        # Starting index: residual matrix is centred, so no trivial axis
        start_idx <- 1
        
        # Get requested dimensions: only min(I, J) - 1 are non-null
        max_dim <- min(length(d), min(I, J) - 1)
        if (dim1 > max_dim || dim2 > max_dim) return(FALSE)
        
        idx1 <- start_idx + dim1 - 1
        idx2 <- start_idx + dim2 - 1
        
        if (idx1 > length(d) || idx2 > length(d)) return(FALSE)
        
        d_use <- d[c(idx1, idx2)]
        U <- svd_result$u[, c(idx1, idx2), drop = FALSE]
        V <- svd_result$v[, c(idx1, idx2), drop = FALSE]
        
        Dr_inv_sqrt <- diag(1 / sqrt(r))
        Dc_inv_sqrt <- diag(1 / sqrt(c))
        
        row_princ <- Dr_inv_sqrt %*% U %*% diag(d_use, nrow = 2)
        col_princ <- Dc_inv_sqrt %*% V %*% diag(d_use, nrow = 2)
        
        # Prepare data frames
        row_df <- data.frame(
            x = row_princ[, 1],
            y = row_princ[, 2],
            label = rownames(N_plot),
            type = "Row",
            stringsAsFactors = FALSE
        )
        
        col_df <- data.frame(
            x = col_princ[, 1],
            y = col_princ[, 2],
            label = colnames(N_plot),
            type = "Column",
            stringsAsFactors = FALSE
        )
        
        # For "fade" mode, mark low-influence columns
        if (attrDisplay == "fade" && length(low_influence_attrs) > 0) {
            col_df$influence <- ifelse(col_df$label %in% low_influence_attrs,
                                       "Low", "High")
        } else {
            col_df$influence <- "High"
        }
        
        # Inertia for axis labels
        eigenvalues_all <- d^2
        if (start_idx == 2) {
            total_inertia <- sum(eigenvalues_all[2:length(eigenvalues_all)])
        } else {
            total_inertia <- sum(eigenvalues_all)
        }
        
        inertia_pct_1 <- d_use[1]^2 / total_inertia * 100
        inertia_pct_2 <- d_use[2]^2 / total_inertia * 100
        
        xlab <- sprintf("Dimension %d (%.1f%%)", dim1, inertia_pct_1)
        ylab <- sprintf("Dimension %d (%.1f%%)", dim2, inertia_pct_2)
        
        # Title and subtitle
        n_retained <- ncol(N_plot)
        n_original <- ncol(N)
        
        if (use_reduced_ca) {
            title <- paste0("Reduced-Data CA Plot (", resid_label, " residuals)")
            subtitle <- paste0(n_retained, " of ", n_original, " attributes; low-influence hidden")
        } else {
            title <- paste0("CA Plot with Attribute Assessment (", resid_label, " residuals)")
            subtitle <- "Low-influence attributes faded"
        }
        
        # Build plot - matching Symmetric CA Plot style exactly
        p <- ggplot2::ggplot() + ggtheme  # jamovi results theme first; plot-specific tweaks follow
        
        # Reference lines
        p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, 
                                     colour = "grey70", linetype = "dashed")
        p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, 
                                     colour = "grey70", linetype = "dashed")
        
        # Row points - white-filled circles with red outline (matching symmetric plot)
        p <- p + ggplot2::geom_point(
            data = row_df,
            ggplot2::aes(x = x, y = y),
            colour = "#CB181D", 
            fill = "white",
            size = dotSize, 
            shape = 21,
            stroke = 0.95
        )
        
        # Column points (with fading for low-influence if applicable)
        if (attrDisplay == "fade" && length(low_influence_attrs) > 0) {
            col_df_high <- col_df[col_df$influence == "High", ]
            col_df_low <- col_df[col_df$influence == "Low", ]
            
            if (nrow(col_df_high) > 0) {
                p <- p + ggplot2::geom_point(
                    data = col_df_high,
                    ggplot2::aes(x = x, y = y),
                    colour = "#2171B5", 
                    size = dotSize, 
                    shape = 17
                )
            }
            
            if (nrow(col_df_low) > 0) {
                p <- p + ggplot2::geom_point(
                    data = col_df_low,
                    ggplot2::aes(x = x, y = y),
                    colour = "#9ECAE1",  # lighter blue for faded
                    size = dotSize, 
                    shape = 17,
                    alpha = 0.25
                )
            }
        } else {
            p <- p + ggplot2::geom_point(
                data = col_df,
                ggplot2::aes(x = x, y = y),
                colour = "#2171B5", 
                size = dotSize, 
                shape = 17
            )
        }
        
        # Row labels
        p <- p + ggrepel::geom_text_repel(
            data = row_df,
            ggplot2::aes(x = x, y = y, label = label),
            colour = "#CB181D", 
            size = labelSize,
            max.overlaps = 30,
            box.padding = 0.4,
            point.padding = 0.3,
            force = 2,
            force_pull = 0.3,
            min.segment.length = 0.5,
            segment.colour = "grey50",
            segment.size = 0.4,
            seed = 42
        )
        
        # Column labels (with fading for low-influence if applicable)
        if (attrDisplay == "fade" && length(low_influence_attrs) > 0) {
            if (nrow(col_df_high) > 0) {
                p <- p + ggrepel::geom_text_repel(
                    data = col_df_high,
                    ggplot2::aes(x = x, y = y, label = label),
                    colour = "#2171B5", 
                    size = labelSize,
                    max.overlaps = 30,
                    box.padding = 0.4,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.3,
                    min.segment.length = 0.5,
                    segment.colour = "grey50",
                    segment.size = 0.4,
                    seed = 42
                )
            }
            
            if (nrow(col_df_low) > 0) {
                p <- p + ggrepel::geom_text_repel(
                    data = col_df_low,
                    ggplot2::aes(x = x, y = y, label = label),
                    colour = "#9ECAE1",  # lighter blue for faded labels
                    size = labelSize * 0.85,
                    max.overlaps = 20,
                    box.padding = 0.4,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.3,
                    min.segment.length = 0.5,
                    segment.colour = "grey70",
                    segment.size = 0.3,
                    seed = 42
                )
            }
        } else {
            p <- p + ggrepel::geom_text_repel(
                data = col_df,
                ggplot2::aes(x = x, y = y, label = label),
                colour = "#2171B5", 
                size = labelSize,
                max.overlaps = 30,
                box.padding = 0.4,
                point.padding = 0.3,
                force = 2,
                force_pull = 0.3,
                min.segment.length = 0.5,
                segment.colour = "grey50",
                segment.size = 0.4,
                seed = 42
            )
        }
        
        # Caption
        caption_text <- "Red (\u25cb) = Row categories; Blue (\u25b2) = Column categories"
        if (attrDisplay == "fade" && length(low_influence_attrs) > 0) {
            caption_text <- paste0(caption_text, "\nFaded = low-influence attributes (below elbow)")
        }
        
        p <- p + ggplot2::labs(title = title, subtitle = subtitle, 
                               x = xlab, y = ylab, caption = caption_text)
        
        # Theme - matching Symmetric CA Plot exactly
        p <- p + ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "white", colour = NA),
            panel.border = .ctFrameRect(ggtheme), axis.line = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
            axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
            plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
            plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40", hjust = 0.5),
            plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0),
            legend.position = "none"
        )
        
        if (self$options$equalScale) {
            p <- p + ggplot2::coord_fixed()
        }
        
        print(p)
        TRUE
    }
    )
)
