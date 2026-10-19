# ═══════════════════════════════════════════════════════════════════════════
# Shared internal helpers for all ChiSquaredTools analyses
# ═══════════════════════════════════════════════════════════════════════════

# ---------------------------------------------------------------------------
# .ctBuildTable(): build a two- or three-way contingency table safely
#
# - Variable names are never pasted into a formula, so names containing
#   spaces, backticks or operators can neither break parsing nor be
#   executed as code (replaces the former as.formula(paste0(...)) idiom).
# - Works for long data (one row per case) and for aggregated data
#   (one row per cell plus a counts variable).
# - Counts are stored as doubles, so products of large margins inside
#   downstream routines (e.g. stats::mantelhaen.test) cannot overflow
#   R's integer range and silently return NA.
# - All factor levels are kept, so the table matches the rows and columns
#   that .init() built from levels(); use .ctRejectEmptyMargins() to stop
#   with a clear message when a level has no observations.
#
# Arguments:
#   data   : data frame (typically self$data)
#   vars   : character vector of 2 or 3 variable names (rows, cols, strata)
#   counts : optional name of the counts variable (NULL or "" if none)
# Returns: an object of class "table" with double storage
# ---------------------------------------------------------------------------
.ctBuildTable <- function(data, vars, counts = NULL) {

    # Treat an empty string as "no counts variable"
    if (!is.null(counts) && identical(counts, ""))
        counts <- NULL

    # Coerce the classifying variables to factors (all levels retained)
    factors <- lapply(vars, function(v) {
        x <- data[[v]]
        if (!is.factor(x)) x <- as.factor(x)
        x
    })

    if (is.null(counts)) {
        # Long format: tabulate cases (table() drops NA cases itself)
        tab <- table(factors, useNA = "no")
        dimnames(tab) <- unname(dimnames(tab))
    } else {
        # Aggregated format: sum the counts within each cell
        w <- jmvcore::toNumeric(data[[counts]])
        if (any(w < 0, na.rm = TRUE))
            jmvcore::reject("The counts variable contains negative values.")

        # Keep rows where neither the factors nor the count are missing
        keep <- !is.na(w)
        for (f in factors) keep <- keep & !is.na(f)
        w <- w[keep]
        factors <- lapply(factors, function(f) f[keep])

        # tapply() groups by the factor levels without parsing any names
        tab <- tapply(w, factors, sum)
        tab[is.na(tab)] <- 0
        tab <- as.table(tab)
        dimnames(tab) <- unname(dimnames(tab))
    }

    # Double storage prevents integer overflow in later products
    storage.mode(tab) <- "double"
    tab
}

# ---------------------------------------------------------------------------
# .ctRejectEmptyMargins(): stop with a plain message when a row or column
# level has no observations (common after filtering, because factor
# levels survive the filter). Checks the first two dimensions only; empty
# strata are handled by the stratified analyses themselves.
#
# Arguments:
#   tab     : table returned by .ctBuildTable()
#   rowVar  : row variable name (for the message)
#   colVar  : column variable name (for the message)
# ---------------------------------------------------------------------------
.ctRejectEmptyMargins <- function(tab, rowVar, colVar) {

    # Overall total
    if (sum(tab) == 0)
        jmvcore::reject("The table contains no observations.")

    # Row and column margins (collapsed over strata when present)
    rs <- apply(tab, 1, sum)
    cs <- apply(tab, 2, sum)
    rn <- dimnames(tab)[[1]]
    cn <- dimnames(tab)[[2]]

    # Build a readable message listing every empty level
    msgs <- character(0)
    if (any(rs == 0))
        msgs <- c(msgs, paste0("'", rowVar, "': ",
                               paste(rn[rs == 0], collapse = ", ")))
    if (any(cs == 0))
        msgs <- c(msgs, paste0("'", colVar, "': ",
                               paste(cn[cs == 0], collapse = ", ")))

    if (length(msgs) > 0) {
        jmvcore::reject(paste0(
            "The table has a row or column with no observations (",
            paste(msgs, collapse = "; "),
            "). This often happens after filtering, because unused levels ",
            "are kept. Recode the variable to remove the empty level(s)."))
    }

    invisible(TRUE)
}

# ---------------------------------------------------------------------------
# .ctRejectTooFewLevels(): stop when a variable has fewer than 2 levels
# ---------------------------------------------------------------------------
.ctRejectTooFewLevels <- function(tab, rowVar, colVar) {
    if (dim(tab)[1] < 2)
        jmvcore::reject(paste0("The row variable '", rowVar,
                               "' needs at least 2 levels."))
    if (dim(tab)[2] < 2)
        jmvcore::reject(paste0("The column variable '", colVar,
                               "' needs at least 2 levels."))
    invisible(TRUE)
}

# ---------------------------------------------------------------------------
# .ctGreenacreCritical(): critical value for Greenacre's (2017) clustering
# test, i.e. the upper 5% point C(0.05) of the largest root, transcribed
# from Pearson & Hartley (1972), Biometrika Tables for Statisticians,
# Vol. 2, Table 51 (pp. 352-353), alpha = 0.05 columns.
#
# For an I x J table: p = min(I-1, J-1), nu = max(I-1, J-1).
# Tabulated for p = 2..10 and nu = 2..12, 15, 20, 25, 30, 40, ..., 100,
# 120, 140, ..., 200; other nu values are interpolated linearly.
# (The printed entry for p = 2, nu = 160 reads "2.037", an evident
# misprint for 203.7, which is the value used here.)
# Shared by the Clustering and Correspondence Analysis facilities.
# ---------------------------------------------------------------------------
.ctGreenacreTable <- local({
    nu <- c(2:12, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100,
            120, 140, 160, 180, 200)
    cv <- cbind(
        `2` = c(8.594, 10.74, 12.68, 14.49, 16.21, 17.88, 19.49, 21.06, 22.61, 24.12, 25.61,
                29.96, 36.94, 43.67, 50.24, 63.02, 75.46, 87.66, 99.70, 111.6, 123.4, 135.0,
                158.1, 181.0, 203.7, 226.2, 248.6),
        `3` = c(10.74, 13.11, 15.24, 17.21, 19.09, 20.88, 22.62, 24.31, 25.96, 27.58, 29.17,
                33.80, 41.18, 48.27, 55.15, 68.50, 81.44, 94.09, 106.5, 118.8, 130.9, 143.0,
                166.7, 190.1, 213.3, 236.3, 259.2),
        `4` = c(12.68, 15.24, 17.52, 19.63, 21.62, 23.53, 25.37, 27.15, 28.90, 30.60, 32.27,
                37.13, 44.84, 52.22, 59.37, 73.18, 86.53, 99.55, 112.3, 124.9, 137.4, 149.7,
                173.9, 197.8, 221.5, 244.9, 268.2),
        `5` = c(14.49, 17.21, 19.63, 21.85, 23.95, 25.96, 27.88, 29.75, 31.57, 33.35, 35.09,
                40.15, 48.14, 55.78, 63.16, 77.37, 91.08, 104.4, 117.5, 130.4, 143.1, 155.6,
                180.4, 204.7, 228.8, 252.6, 276.2),
        `6` = c(16.21, 19.09, 21.62, 23.95, 26.14, 28.23, 30.24, 32.18, 34.08, 35.93, 37.73,
                42.96, 51.21, 59.07, 66.66, 81.24, 95.27, 108.9, 122.3, 135.4, 148.3, 161.1,
                186.2, 211.0, 235.4, 259.5, 283.4),
        `7` = c(17.88, 20.88, 23.53, 25.96, 28.23, 30.40, 32.48, 34.50, 36.45, 38.36, 40.22,
                45.61, 54.10, 62.17, 69.95, 84.86, 99.18, 113.1, 126.7, 140.0, 153.2, 166.1,
                191.6, 216.7, 241.4, 265.8, 290.0),
        `8` = c(19.49, 22.62, 25.37, 27.88, 30.24, 32.48, 34.63, 36.70, 38.72, 40.69, 42.60,
                48.15, 56.86, 65.12, 73.07, 88.29, 102.9, 117.0, 130.9, 144.4, 157.8, 170.9,
                196.8, 222.2, 247.2, 271.9, 296.4),
        `9` = c(21.06, 24.31, 27.15, 29.75, 32.18, 34.50, 36.70, 38.84, 40.91, 42.93, 44.90,
                50.58, 59.50, 67.94, 76.05, 91.57, 106.4, 120.8, 134.8, 148.6, 162.1, 175.4,
                201.6, 227.3, 252.6, 277.6, 302.4),
        `10` = c(22.61, 25.96, 28.90, 31.57, 34.08, 36.45, 38.72, 40.91, 43.04, 45.10, 47.12,
                 52.94, 62.05, 70.67, 78.93, 94.72, 109.8, 124.4, 138.7, 152.6, 166.3, 179.8,
                 206.3, 232.3, 257.8, 283.0, 308.0)
    )
    rownames(cv) <- nu
    cv
})

.ctGreenacreCritical <- function(nrow, ncol) {
    p  <- min(nrow - 1, ncol - 1)
    nu <- max(nrow - 1, ncol - 1)
    tab <- .ctGreenacreTable
    nus <- as.numeric(rownames(tab))

    # Range of the published table
    if (p < 2)
        stop(paste0("Table too small (", nrow, "×", ncol,
                    "). Critical values require at least a 3×3 table."))
    if (p > 10)
        stop(paste0("Table dimensions (", nrow, "×", ncol,
                    ") exceed available critical values: the smaller dimension must be ≤ 11 ",
                    "(Pearson & Hartley, 1972, Table 51, tabulates p = min(I−1, J−1) ≤ 10)."))
    if (nu > 200)
        stop(paste0("Table dimensions (", nrow, "×", ncol,
                    ") exceed available critical values: the larger dimension must be ≤ 201 ",
                    "(Pearson & Hartley, 1972, Table 51, tabulates ν = max(I−1, J−1) ≤ 200)."))

    col <- tab[, as.character(p)]
    # Exact row, otherwise linear interpolation between tabulated rows
    if (nu %in% nus) return(unname(col[as.character(nu)]))
    stats::approx(nus, col, xout = nu)$y
}

# ---------------------------------------------------------------------------
# .ctFrameRect(): a panel border drawn in the same colour and width as the
# axis lines of the jamovi plot theme, so that all four sides of a plot
# look alike (used together with axis.line = element_blank()).
# ---------------------------------------------------------------------------
.ctFrameRect <- function(ggtheme) {
    th <- NULL
    if (inherits(ggtheme, "theme")) {
        th <- ggtheme
    } else if (is.list(ggtheme)) {
        for (el in ggtheme) if (inherits(el, "theme")) { th <- el; break }
    }
    al <- if (!is.null(th)) th$axis.line else NULL
    if (!inherits(al, "element_line")) al <- NULL
    col <- if (!is.null(al$colour)) al$colour else "#333333"
    lw  <- if (!is.null(al$linewidth)) al$linewidth else
           if (!is.null(al$size)) al$size else 0.5
    ggplot2::element_rect(fill = NA, colour = col, linewidth = lw)
}

# ---------------------------------------------------------------------------
# .ctClusterHull(): outline enclosing the points of one cluster, padded so
# that it stays visible even when the members almost coincide (a plain line
# or hull between near-identical points disappears under the symbols).
# Each point is replaced by a small ellipse (padX, padY in data units) and
# the convex hull of all ellipse points is returned, closed.
# ---------------------------------------------------------------------------
.ctClusterHull <- function(x, y, padX, padY, nSeg = 24) {
    th <- seq(0, 2 * pi, length.out = nSeg + 1)[-1]
    px <- as.vector(outer(cos(th) * padX, x, "+"))
    py <- as.vector(outer(sin(th) * padY, y, "+"))
    h  <- grDevices::chull(px, py)
    data.frame(x = px[c(h, h[1])], y = py[c(h, h[1])])
}

# ---------------------------------------------------------------------------
# Resampling distribution plot (permutation / Monte Carlo), ggplot2 + jamovi
# theme. The observed statistic is drawn as a dashed line with its p-value;
# when it lies far beyond the simulated values, an arrow at the right edge
# marks it as off scale (so the histogram is not squashed).
# ---------------------------------------------------------------------------
.ctDistPlot <- function(stats, observed, title, label, fill, ggtheme) {
    B_extreme <- sum(stats >= observed)
    p <- (B_extreme + 1) / (length(stats) + 1)
    p_txt <- if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)

    df <- data.frame(stat = stats)
    x_max_data <- max(stats)
    off_scale <- observed > 1.5 * x_max_data && observed > 0

    plt <- ggplot2::ggplot(df, ggplot2::aes(x = stat)) +
        ggplot2::geom_histogram(bins = 30, fill = fill, colour = "white") +
        ggplot2::labs(title = title, x = "χ² statistic", y = "Frequency") +
        ggtheme +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # y position for the annotations (from the drawn histogram)
    built <- ggplot2::ggplot_build(plt)
    y_top <- max(built$data[[1]]$count)

    if (!off_scale) {
        x_rng <- range(c(stats, observed))
        right_side <- observed <= x_rng[1] + 0.8 * diff(x_rng)
        plt <- plt +
            ggplot2::geom_vline(xintercept = observed, colour = "#C0392B",
                                linetype = "dashed", linewidth = 0.9) +
            ggplot2::annotate("text", x = observed, y = y_top * 0.85,
                              label = paste0("  observed ", label, "  \n  ", p_txt, "  "),
                              hjust = if (right_side) 0 else 1,
                              colour = "#C0392B", fontface = "bold", size = 3.5)
    } else {
        x_edge <- x_max_data * 1.05
        plt <- plt +
            ggplot2::annotate("segment", x = x_max_data * 0.88, xend = x_edge,
                              y = y_top * 0.5, yend = y_top * 0.5,
                              colour = "#C0392B", linewidth = 0.9,
                              arrow = grid::arrow(length = grid::unit(0.15, "inches"))) +
            ggplot2::annotate("text", x = x_max_data * 0.86, y = y_top * 0.5,
                              label = sprintf("observed %s = %.2f\n(off scale)\n%s", label, observed, p_txt),
                              hjust = 1, colour = "#C0392B", fontface = "bold", size = 3.5)
    }
    plt
}

# Column names used inside ggplot2::aes() in the helpers above (declared so
# that R CMD check does not report them as undefined global variables)
utils::globalVariables(c("stat"))
