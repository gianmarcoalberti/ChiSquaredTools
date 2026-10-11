# This file is a generated template, your changes will not be overwritten

#' @export
chisqpowerClass <- R6::R6Class(
  "chisqpowerClass",
  inherit = chisqpowerBase,
  private = list(
    
    # =========================================================================
    # INITIALISATION
    # =========================================================================
    .init = function() {
      
      # Pre-build results table structure
      resultsTable <- self$results$resultsTable
      
      analysisType <- self$options$analysisType
      
      # Add rows based on analysis type
      resultsTable$addRow(rowKey = "analysisType", values = list(parameter = "Analysis type"))
      resultsTable$addRow(rowKey = "tableDim", values = list(parameter = "Table dimensions"))
      resultsTable$addRow(rowKey = "df", values = list(parameter = "Degrees of freedom"))
      resultsTable$addRow(rowKey = "effectW", values = list(parameter = "Effect size (Cohen's w)"))
      resultsTable$addRow(rowKey = "effectV", values = list(parameter = "Effect size (Cramér's V)"))
      resultsTable$addRow(rowKey = "alpha", values = list(parameter = "Significance level (α)"))
      
      if (analysisType == "apriori") {
        resultsTable$addRow(rowKey = "targetPower", values = list(parameter = "Target power (1 − β)"))
        resultsTable$addRow(rowKey = "requiredN", values = list(parameter = "Required sample size (N)"))
      } else if (analysisType == "posthoc") {
        resultsTable$addRow(rowKey = "sampleN", values = list(parameter = "Sample size (N)"))
        resultsTable$addRow(rowKey = "achievedPower", values = list(parameter = "Achieved power (1 − β)"))
      } else if (analysisType == "sensitivity") {
        resultsTable$addRow(rowKey = "sampleN", values = list(parameter = "Sample size (N)"))
        resultsTable$addRow(rowKey = "targetPower", values = list(parameter = "Target power (1 − β)"))
        resultsTable$addRow(rowKey = "detectableW", values = list(parameter = "Detectable effect (Cohen's w)"))
        resultsTable$addRow(rowKey = "detectableV", values = list(parameter = "Detectable effect (Cramér's V)"))
      }
      
      resultsTable$addRow(rowKey = "critChi", values = list(parameter = "Critical χ²"))
      resultsTable$addRow(rowKey = "ncp", values = list(parameter = "Noncentrality parameter (λ)"))
      
      # Pre-build reference table structure
      if (self$options$showRefTable) {
        refTable <- self$results$refTable
        
        refTable$addRow(rowKey = "p70", values = list(power = "0.70"))
        refTable$addRow(rowKey = "p80", values = list(power = "0.80"))
        refTable$addRow(rowKey = "p90", values = list(power = "0.90"))
        refTable$addRow(rowKey = "p95", values = list(power = "0.95"))
      }
    },
    
    # =========================================================================
    # MAIN RUN FUNCTION
    # =========================================================================
    .run = function() {
      
      # ---------------------------------------------------------------------
      # 1. Extract parameters
      # ---------------------------------------------------------------------
      analysisType <- self$options$analysisType
      nRows <- self$options$nRows
      nCols <- self$options$nCols
      alpha <- self$options$alpha
      
      # Calculate degrees of freedom
      df <- (nRows - 1) * (nCols - 1)
      
      # Get effect size (Cohen's w)
      w <- private$.getEffectSizeW()
      
      # Convert to Cramér's V for display
      k <- min(nRows, nCols)
      v <- w / sqrt(k - 1)
      
      # Get other parameters based on analysis type
      if (analysisType == "apriori") {
        targetPower <- self$options$power
        result <- private$.computeSampleSize(w, alpha, targetPower, df)
        requiredN <- result$n
        ncp <- result$ncp
      } else if (analysisType == "posthoc") {
        sampleN <- self$options$n
        result <- private$.computePower(w, alpha, sampleN, df)
        achievedPower <- result$power
        ncp <- result$ncp
      } else if (analysisType == "sensitivity") {
        sampleN <- self$options$n
        targetPower <- self$options$power
        result <- private$.computeEffectSize(alpha, targetPower, sampleN, df)
        detectableW <- result$w
        detectableV <- detectableW / sqrt(k - 1)
        ncp <- result$ncp
      }
      
      # Critical chi-squared value
      critChi <- qchisq(1 - alpha, df)
      
      # ---------------------------------------------------------------------
      # 2. Populate results table
      # ---------------------------------------------------------------------
      resultsTable <- self$results$resultsTable
      
      # Analysis type label
      typeLabels <- c(
        apriori = "A priori (compute required N)",
        posthoc = "Post-hoc (compute achieved power)",
        sensitivity = "Sensitivity (compute detectable effect)"
      )
      
      resultsTable$setRow(rowKey = "analysisType", 
                          values = list(value = typeLabels[analysisType]))
      resultsTable$setRow(rowKey = "tableDim", 
                          values = list(value = sprintf("%d × %d", nRows, nCols)))
      resultsTable$setRow(rowKey = "df", 
                          values = list(value = as.character(df)))
      resultsTable$setRow(rowKey = "alpha", 
                          values = list(value = sprintf("%.3f", alpha)))
      resultsTable$setRow(rowKey = "critChi", 
                          values = list(value = sprintf("%.3f", critChi)))
      
      if (analysisType == "apriori") {
        resultsTable$setRow(rowKey = "effectW", 
                            values = list(value = sprintf("%.3f", w)))
        resultsTable$setRow(rowKey = "effectV", 
                            values = list(value = sprintf("%.3f", v)))
        resultsTable$setRow(rowKey = "targetPower", 
                            values = list(value = sprintf("%.2f", targetPower)))
        resultsTable$setRow(rowKey = "requiredN", 
                            values = list(value = as.character(requiredN)))
        resultsTable$setRow(rowKey = "ncp", 
                            values = list(value = sprintf("%.3f", ncp)))
        
      } else if (analysisType == "posthoc") {
        resultsTable$setRow(rowKey = "effectW", 
                            values = list(value = sprintf("%.3f", w)))
        resultsTable$setRow(rowKey = "effectV", 
                            values = list(value = sprintf("%.3f", v)))
        resultsTable$setRow(rowKey = "sampleN", 
                            values = list(value = as.character(sampleN)))
        resultsTable$setRow(rowKey = "achievedPower", 
                            values = list(value = sprintf("%.3f", achievedPower)))
        resultsTable$setRow(rowKey = "ncp", 
                            values = list(value = sprintf("%.3f", ncp)))
        
      } else if (analysisType == "sensitivity") {
        resultsTable$setRow(rowKey = "effectW", 
                            values = list(value = sprintf("(input: %.3f)", w)))
        resultsTable$setRow(rowKey = "effectV", 
                            values = list(value = sprintf("(input: %.3f)", v)))
        resultsTable$setRow(rowKey = "sampleN", 
                            values = list(value = as.character(sampleN)))
        resultsTable$setRow(rowKey = "targetPower", 
                            values = list(value = sprintf("%.2f", targetPower)))
        resultsTable$setRow(rowKey = "detectableW", 
                            values = list(value = sprintf("%.3f", detectableW)))
        resultsTable$setRow(rowKey = "detectableV", 
                            values = list(value = sprintf("%.3f", detectableV)))
        resultsTable$setRow(rowKey = "ncp", 
                            values = list(value = sprintf("%.3f", ncp)))
      }
      
      # ---------------------------------------------------------------------
      # 3. Post-hoc power notice
      # ---------------------------------------------------------------------
      if (analysisType == "posthoc") {
        private$.populatePostHocNotice()
      }
      
      # ---------------------------------------------------------------------
      # 4. Reference table
      # ---------------------------------------------------------------------
      if (self$options$showRefTable) {
        private$.populateRefTable(df, alpha)
      }
      
      # ---------------------------------------------------------------------
      # 5. Set up plot states
      # ---------------------------------------------------------------------
      if (self$options$showPowerCurve) {
        plotState <- list(
          analysisType = analysisType,
          df = df,
          alpha = alpha,
          w = w,
          targetPower = if (analysisType != "posthoc") self$options$power else NULL,
          sampleN = if (analysisType != "apriori") self$options$n else NULL,
          requiredN = if (analysisType == "apriori") requiredN else NULL,
          achievedPower = if (analysisType == "posthoc") achievedPower else NULL,
          detectableW = if (analysisType == "sensitivity") detectableW else NULL
        )
        self$results$powerCurvePlot$setState(plotState)
      }
      
      if (self$options$showESCurve) {
        esPlotState <- list(
          df = df,
          alpha = alpha,
          power = self$options$power,
          sampleN = self$options$n,
          k = k
        )
        self$results$esCurvePlot$setState(esPlotState)
      }
      
      # ---------------------------------------------------------------------
      # 6. Method information
      # ---------------------------------------------------------------------
      if (self$options$showMethodInfo) {
        private$.populateMethodInfo()
      }
    },
    
    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================
    
    # Get Cohen's w from user input (handling different input types)
    .getEffectSizeW = function() {
      esType <- self$options$esType
      
      if (esType == "conventional") {
        # Conventional thresholds
        conventional <- self$options$esConventional
        w <- switch(conventional,
                    small = 0.1,
                    medium = 0.3,
                    large = 0.5)
      } else if (esType == "v") {
        # Convert Cramér's V to Cohen's w
        v <- self$options$effectSize
        k <- min(self$options$nRows, self$options$nCols)
        w <- v * sqrt(k - 1)
      } else {
        # Cohen's w directly
        w <- self$options$effectSize
      }
      
      return(w)
    },
    
    # Compute power given w, alpha, N, df
    .computePower = function(w, alpha, n, df) {
      # Noncentrality parameter
      ncp <- n * w^2
      
      # Critical chi-squared
      critChi <- qchisq(1 - alpha, df)
      
      # Power = P(chi-squared > critical | ncp)
      power <- 1 - pchisq(critChi, df, ncp = ncp)
      
      return(list(power = power, ncp = ncp))
    },
    
    # Compute required sample size given w, alpha, power, df
    .computeSampleSize = function(w, alpha, targetPower, df) {
      # Critical chi-squared
      critChi <- qchisq(1 - alpha, df)
      
      # Function to find root: computed power - target power = 0
      powerDiff <- function(n) {
        ncp <- n * w^2
        computedPower <- 1 - pchisq(critChi, df, ncp = ncp)
        return(computedPower - targetPower)
      }
      
      # Find N using uniroot
      # Start with reasonable bounds
      lowerN <- 4
      upperN <- 1e7
      
      # Check if lower bound already exceeds target
      if (powerDiff(lowerN) > 0) {
        n <- lowerN
      } else if (powerDiff(upperN) < 0) {
        # Target not reachable within the search interval
        jmvcore::reject(paste0(
          "The target power cannot be reached with a sample of up to ",
          format(upperN, big.mark = ",", scientific = FALSE),
          " for this effect size. Increase the effect size or lower the target power."))
      } else {
        result <- uniroot(powerDiff, interval = c(lowerN, upperN), tol = 0.001)
        n <- ceiling(result$root)
      }
      
      # Compute final ncp
      ncp <- n * w^2
      
      return(list(n = n, ncp = ncp))
    },
    
    # Compute detectable effect size given alpha, power, N, df
    .computeEffectSize = function(alpha, targetPower, n, df) {
      # Critical chi-squared
      critChi <- qchisq(1 - alpha, df)
      
      # Function to find root: computed power - target power = 0
      powerDiff <- function(w) {
        ncp <- n * w^2
        computedPower <- 1 - pchisq(critChi, df, ncp = ncp)
        return(computedPower - targetPower)
      }
      
      # Power tends to alpha as w tends to 0, so a target power at or
      # below alpha needs no effect at all
      if (targetPower <= alpha) {
        jmvcore::reject(paste0(
          "The target power (", targetPower, ") must be greater than \u03b1 (",
          alpha, "); with power \u2264 \u03b1 any effect size, however small, is detectable."))
      }
      # Even a very large effect may be undetectable with a tiny sample
      if (powerDiff(5) < 0) {
        jmvcore::reject(paste0(
          "The target power cannot be reached with N = ", n,
          " even for w = 5. Increase the sample size or lower the target power."))
      }
      
      # Find w using uniroot
      result <- uniroot(powerDiff, interval = c(0.001, 5), tol = 0.0001)
      w <- result$root
      
      # Compute final ncp
      ncp <- n * w^2
      
      return(list(w = w, ncp = ncp))
    },
    
    # =========================================================================
    # POPULATE FUNCTIONS
    # =========================================================================
    
    .populatePostHocNotice = function() {
      # Create a Warning notice for post-hoc power analysis caveat
      notice <- jmvcore::Notice$new(
        self$options,
        name = 'postHocWarning',
        type = jmvcore::NoticeType$WARNING
      )
      
      notice$setContent(paste0(
        'Note on post-hoc power analysis: Observed (post-hoc) power is a direct ',
        'transformation of the p-value and does not provide independent information ',
        'about study adequacy. Consider sensitivity analysis as an alternative for ',
        'evaluating what effects your study could have detected. ',
        'See: Hoenig & Heisey (2001), The American Statistician, 55(1), 19-24.'
      ))
      
      # Insert after the results table (position 2)
      self$results$insert(2, notice)
    },
    
    .populateRefTable = function(df, alpha) {
      refTable <- self$results$refTable
      
      # Effect sizes to compute
      wValues <- c(0.1, 0.3, 0.5)
      powerLevels <- c(0.70, 0.80, 0.90, 0.95)
      rowKeys <- c("p70", "p80", "p90", "p95")
      
      for (i in seq_along(powerLevels)) {
        targetPower <- powerLevels[i]
        
        # Compute N for each effect size
        nSmall <- private$.computeSampleSize(0.1, alpha, targetPower, df)$n
        nMedium <- private$.computeSampleSize(0.3, alpha, targetPower, df)$n
        nLarge <- private$.computeSampleSize(0.5, alpha, targetPower, df)$n
        
        refTable$setRow(rowKey = rowKeys[i], values = list(
          small = nSmall,
          medium = nMedium,
          large = nLarge
        ))
      }
      
      # Add note about table parameters
      refTable$setNote(
        "params",
        sprintf("Table dimensions: %d × %d; df = %d; α = %.3f",
                self$options$nRows, self$options$nCols, df, alpha)
      )
    },
    
    .populateMethodInfo = function() {
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.95em;'>"
      html <- paste0(html, "<h3 style='color: #3E6DA6;'>Power Analysis for Chi-Squared Tests</h3>")
      
      # General explanation
      html <- paste0(html, 
        "<h4 style='color: #3E6DA6;'>Overview</h4>",
        "<p style='margin-bottom: 12px;'>Power analysis helps researchers plan studies ",
        "with adequate statistical power and interpret completed studies. For chi-squared ",
        "tests of independence, power depends on four parameters:</p>",
        "<ul style='margin-bottom: 12px;'>",
        "<li><strong>Effect size (w)</strong>: The magnitude of association in the population</li>",
        "<li><strong>Sample size (N)</strong>: Total number of observations</li>",
        "<li><strong>Significance level (α)</strong>: Probability of Type I error</li>",
        "<li><strong>Degrees of freedom</strong>: (rows − 1) × (columns − 1)</li>",
        "</ul>"
      )
      
      # Effect size
      html <- paste0(html,
        "<h4 style='color: #3E6DA6;'>Effect Size Measures</h4>",
        "<p style='margin-bottom: 12px;'><strong>Cohen's w</strong> is defined as:</p>",
        "<p style='text-align: center; margin-bottom: 12px;'>",
        "w = √[Σ(P<sub>1ij</sub> − P<sub>0ij</sub>)² / P<sub>0ij</sub>]</p>",
        "<p style='margin-bottom: 12px;'>where P<sub>1ij</sub> are the population proportions ",
        "under the alternative hypothesis and P<sub>0ij</sub> are the proportions under independence.</p>",
        "<p style='margin-bottom: 12px;'><strong>Cramér's V</strong> is related to w by:</p>",
        "<p style='text-align: center; margin-bottom: 12px;'>V = w / √(k − 1)</p>",
        "<p style='margin-bottom: 12px;'>where k = min(rows, columns). This means V is bounded ",
        "between 0 and 1, while w can exceed 1 for larger tables.</p>",
        "<p style='margin-bottom: 12px;'>Cohen's (1988) conventional thresholds for w:</p>",
        "<ul style='margin-bottom: 12px;'>",
        "<li>Small: w = 0.1</li>",
        "<li>Medium: w = 0.3</li>",
        "<li>Large: w = 0.5</li>",
        "</ul>"
      )
      
      # Noncentrality parameter
      html <- paste0(html,
        "<h4 style='color: #3E6DA6;'>The Noncentrality Parameter</h4>",
        "<p style='margin-bottom: 12px;'>Under the alternative hypothesis, the chi-squared ",
        "statistic follows a noncentral chi-squared distribution with noncentrality parameter:</p>",
        "<p style='text-align: center; margin-bottom: 12px;'>λ = N × w²</p>",
        "<p style='margin-bottom: 12px;'>Power is computed as the probability of exceeding the ",
        "critical chi-squared value under this noncentral distribution.</p>"
      )
      
      # Analysis types
      html <- paste0(html,
        "<h4 style='color: #3E6DA6;'>Types of Power Analysis</h4>",
        "<p style='margin-bottom: 12px;'><strong>A priori:</strong> Determines the sample size ",
        "needed to achieve a target power for a specified effect size. This is the most ",
        "useful form of power analysis, conducted during study planning.</p>",
        "<p style='margin-bottom: 12px;'><strong>Post-hoc:</strong> Calculates the power ",
        "achieved by a completed study. <em>Caution:</em> Post-hoc power is controversial ",
        "because it is a monotonic function of the p-value and adds no new information ",
        "(Hoenig & Heisey, 2001).</p>",
        "<p style='margin-bottom: 12px;'><strong>Sensitivity:</strong> Determines the minimum ",
        "effect size that could be detected with a given sample size and power. Useful for ",
        "grant applications and interpreting non-significant results.</p>"
      )
      
      # References
      html <- paste0(html,
        "<h4 style='color: #3E6DA6;'>References</h4>",
        "<p style='font-size: 0.85em; color: #666; margin-top: 8px;'>",
        "Cohen, J. (1988). <em>Statistical power analysis for the behavioral sciences</em> (2nd ed.). Lawrence Erlbaum Associates.<br>",
        "Cohen, J. (1992). A power primer. <em>Psychological Bulletin</em>, 112(1), 155–159.<br>",
        "Hoenig, J. M., & Heisey, D. M. (2001). The abuse of power: The pervasive fallacy of power ",
        "calculations for data analysis. <em>The American Statistician</em>, 55(1), 19–24.",
        "</p>"
      )
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # =========================================================================
    # PLOT FUNCTIONS
    # =========================================================================
    
    .plotPowerCurve = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      plotState <- image$state
      
      if (is.null(plotState)) {
        return(FALSE)
      }
      
      analysisType <- plotState$analysisType
      df <- plotState$df
      alpha <- plotState$alpha
      w <- plotState$w
      
      # Critical chi-squared
      critChi <- qchisq(1 - alpha, df)
      
      if (analysisType == "apriori" || analysisType == "posthoc") {
        # Power curve: Power vs Sample Size
        
        # Determine x-axis range
        if (analysisType == "apriori") {
          targetN <- plotState$requiredN
          maxN <- max(targetN * 1.5, 100)
          minN <- max(4, floor(targetN * 0.1))
        } else {
          sampleN <- plotState$sampleN
          maxN <- max(sampleN * 2, 200)
          minN <- 4
        }
        
        # Generate sequence of N values
        nSeq <- seq(minN, maxN, length.out = 200)
        
        # Compute power for each N
        powerSeq <- sapply(nSeq, function(n) {
          ncp <- n * w^2
          1 - pchisq(critChi, df, ncp = ncp)
        })
        
        plotData <- data.frame(n = nSeq, power = powerSeq)
        
        # Create plot
        plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = n, y = power)) +
          ggplot2::geom_line(colour = "#3E6DA6", linewidth = 1.2) +
          ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", colour = "#666666", linewidth = 0.5) +
          ggplot2::geom_hline(yintercept = 0.90, linetype = "dotted", colour = "#666666", linewidth = 0.5) +
          ggplot2::labs(
            title = sprintf("Power Curve (w = %.2f, α = %.3f, df = %d)", w, alpha, df),
            x = "Sample Size (N)",
            y = "Power (1 − β)"
          ) +
          ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
          ggplot2::annotate("text", x = minN + (maxN - minN) * 0.02, y = 0.82, 
                           label = "0.80", colour = "#666666", size = 3, hjust = 0) +
          ggplot2::annotate("text", x = minN + (maxN - minN) * 0.02, y = 0.92, 
                           label = "0.90", colour = "#666666", size = 3, hjust = 0)
        
        # Add solution point
        if (analysisType == "apriori") {
          targetPower <- plotState$targetPower
          requiredN <- plotState$requiredN
          plot <- plot +
            ggplot2::geom_point(data = data.frame(n = requiredN, power = targetPower),
                               ggplot2::aes(x = n, y = power),
                               colour = "#D32F2F", size = 4) +
            ggplot2::geom_segment(x = requiredN, xend = requiredN, 
                                 y = 0, yend = targetPower,
                                 linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
            ggplot2::geom_segment(x = minN, xend = requiredN, 
                                 y = targetPower, yend = targetPower,
                                 linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
            ggplot2::annotate("text", x = requiredN, y = targetPower + 0.05,
                             label = sprintf("N = %d", requiredN),
                             colour = "#D32F2F", size = 3.5, fontface = "bold")
        } else {
          sampleN <- plotState$sampleN
          achievedPower <- plotState$achievedPower
          plot <- plot +
            ggplot2::geom_point(data = data.frame(n = sampleN, power = achievedPower),
                               ggplot2::aes(x = n, y = power),
                               colour = "#D32F2F", size = 4) +
            ggplot2::geom_segment(x = sampleN, xend = sampleN, 
                                 y = 0, yend = achievedPower,
                                 linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
            ggplot2::geom_segment(x = minN, xend = sampleN, 
                                 y = achievedPower, yend = achievedPower,
                                 linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
            ggplot2::annotate("text", x = sampleN, y = achievedPower + 0.05,
                             label = sprintf("Power = %.3f", achievedPower),
                             colour = "#D32F2F", size = 3.5, fontface = "bold")
        }
        
      } else {
        # Sensitivity analysis: Power vs Effect Size
        sampleN <- plotState$sampleN
        targetPower <- plotState$targetPower
        detectableW <- plotState$detectableW
        
        # Generate sequence of w values
        wSeq <- seq(0.01, 1.0, length.out = 200)
        
        # Compute power for each w
        powerSeq <- sapply(wSeq, function(w_val) {
          ncp <- sampleN * w_val^2
          1 - pchisq(critChi, df, ncp = ncp)
        })
        
        plotData <- data.frame(w = wSeq, power = powerSeq)
        
        # Create plot
        plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = w, y = power)) +
          ggplot2::geom_line(colour = "#3E6DA6", linewidth = 1.2) +
          ggplot2::geom_hline(yintercept = targetPower, linetype = "dashed", colour = "#666666", linewidth = 0.5) +
          ggplot2::geom_vline(xintercept = 0.1, linetype = "dotted", colour = "#999999", linewidth = 0.3) +
          ggplot2::geom_vline(xintercept = 0.3, linetype = "dotted", colour = "#999999", linewidth = 0.3) +
          ggplot2::geom_vline(xintercept = 0.5, linetype = "dotted", colour = "#999999", linewidth = 0.3) +
          ggplot2::labs(
            title = sprintf("Sensitivity Curve (N = %d, α = %.3f, df = %d)", sampleN, alpha, df),
            x = "Effect Size (Cohen's w)",
            y = "Power (1 − β)"
          ) +
          ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
          ggplot2::scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
          ggplot2::annotate("text", x = 0.1, y = 0.02, label = "Small", 
                           colour = "#999999", size = 2.5, angle = 90, hjust = 0) +
          ggplot2::annotate("text", x = 0.3, y = 0.02, label = "Medium", 
                           colour = "#999999", size = 2.5, angle = 90, hjust = 0) +
          ggplot2::annotate("text", x = 0.5, y = 0.02, label = "Large", 
                           colour = "#999999", size = 2.5, angle = 90, hjust = 0) +
          # Solution point
          ggplot2::geom_point(data = data.frame(w = detectableW, power = targetPower),
                             ggplot2::aes(x = w, y = power),
                             colour = "#D32F2F", size = 4) +
          ggplot2::geom_segment(x = detectableW, xend = detectableW, 
                               y = 0, yend = targetPower,
                               linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
          ggplot2::geom_segment(x = 0, xend = detectableW, 
                               y = targetPower, yend = targetPower,
                               linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
          ggplot2::annotate("text", x = detectableW + 0.03, y = targetPower - 0.05,
                           label = sprintf("w = %.3f", detectableW),
                           colour = "#D32F2F", size = 3.5, fontface = "bold", hjust = 0)
      }
      
      # Apply jamovi theme
      plot <- plot + ggtheme
      
      print(plot)
      return(TRUE)
    },
    
    .plotESCurve = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      plotState <- image$state
      
      if (is.null(plotState)) {
        return(FALSE)
      }
      
      df <- plotState$df
      alpha <- plotState$alpha
      power <- plotState$power
      sampleN <- plotState$sampleN
      k <- plotState$k
      
      # Critical chi-squared
      critChi <- qchisq(1 - alpha, df)
      
      # Generate sequence of N values
      nSeq <- seq(10, max(sampleN * 3, 500), length.out = 200)
      
      # Compute detectable w for each N
      detectableW <- sapply(nSeq, function(n) {
        # Find w such that power = target power
        powerDiff <- function(w_val) {
          ncp <- n * w_val^2
          computedPower <- 1 - pchisq(critChi, df, ncp = ncp)
          return(computedPower - power)
        }
        
        # Handle edge cases
        tryCatch({
          result <- uniroot(powerDiff, interval = c(0.001, 5), tol = 0.0001)
          result$root
        }, error = function(e) NA)
      })
      
      plotData <- data.frame(n = nSeq, w = detectableW)
      plotData <- plotData[!is.na(plotData$w), ]
      
      # Current sample size detectable effect
      currentW <- tryCatch({
        powerDiff <- function(w_val) {
          ncp <- sampleN * w_val^2
          computedPower <- 1 - pchisq(critChi, df, ncp = ncp)
          return(computedPower - power)
        }
        result <- uniroot(powerDiff, interval = c(0.001, 5), tol = 0.0001)
        result$root
      }, error = function(e) NA)
      
      # Create plot
      plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = n, y = w)) +
        ggplot2::geom_line(colour = "#3E6DA6", linewidth = 1.2) +
        ggplot2::geom_hline(yintercept = 0.1, linetype = "dotted", colour = "#999999", linewidth = 0.3) +
        ggplot2::geom_hline(yintercept = 0.3, linetype = "dotted", colour = "#999999", linewidth = 0.3) +
        ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", colour = "#999999", linewidth = 0.3) +
        ggplot2::labs(
          title = sprintf("Detectable Effect Size by Sample Size (Power = %.2f, α = %.3f)", power, alpha),
          x = "Sample Size (N)",
          y = "Detectable Effect Size (Cohen's w)"
        ) +
        ggplot2::annotate("text", x = max(nSeq) * 0.98, y = 0.1, label = "Small (0.1)", 
                         colour = "#999999", size = 2.5, hjust = 1) +
        ggplot2::annotate("text", x = max(nSeq) * 0.98, y = 0.3, label = "Medium (0.3)", 
                         colour = "#999999", size = 2.5, hjust = 1) +
        ggplot2::annotate("text", x = max(nSeq) * 0.98, y = 0.5, label = "Large (0.5)", 
                         colour = "#999999", size = 2.5, hjust = 1)
      
      # Add current sample size point if valid
      if (!is.na(currentW)) {
        plot <- plot +
          ggplot2::geom_point(data = data.frame(n = sampleN, w = currentW),
                             ggplot2::aes(x = n, y = w),
                             colour = "#D32F2F", size = 4) +
          ggplot2::geom_segment(x = sampleN, xend = sampleN, 
                               y = 0, yend = currentW,
                               linetype = "dashed", colour = "#D32F2F", linewidth = 0.5) +
          ggplot2::annotate("text", x = sampleN, y = currentW + 0.03,
                           label = sprintf("N = %d\nw = %.3f", sampleN, currentW),
                           colour = "#D32F2F", size = 3, fontface = "bold")
      }
      
      # Apply jamovi theme
      plot <- plot + ggtheme
      
      print(plot)
      return(TRUE)
    }
  )
)
