# Small long-format tables (one row per cell, with a counts column)

# 3 x 3 table with one rare row (row total 5, N = 100)
rareRow <- data.frame(
    r = factor(rep(c("a", "b", "c"), each = 3)),
    c = factor(rep(c("x", "y", "z"), times = 3)),
    n = c(2, 2, 1,  30, 10, 5,  10, 15, 25))

# 2 x 4 table (one CA dimension)
twoByJ <- data.frame(
    r = factor(rep(c("a", "b"), each = 4)),
    c = factor(rep(c("w", "x", "y", "z"), times = 2)),
    n = c(20, 15, 10, 5,  5, 10, 15, 20))

# 2 x 2 table, n = 20
twoByTwo <- data.frame(
    r = factor(c("a", "a", "b", "b")),
    c = factor(c("x", "y", "x", "y")),
    n = c(8, 2, 3, 7))

# 10 x 10 table with weak association and many degrees of freedom (df = 81)
largeDf <- local({
    set.seed(2026)
    p <- outer(rep(1, 10), rep(1, 10)) + diag(0.35, 10)
    tab <- matrix(stats::rmultinom(1, 2000, as.vector(p / sum(p))), 10, 10)
    data.frame(r = factor(rep(sprintf("r%02d", 1:10), times = 10)),
               c = factor(rep(sprintf("c%02d", 1:10), each = 10)),
               n = as.vector(tab))
})

# Sparse 3 x 3 table: one case in the first row (N = 20)
sparseRow <- data.frame(
    r = factor(rep(c("a", "b", "c"), each = 3)),
    c = factor(rep(c("x", "y", "z"), times = 3)),
    n = c(1, 0, 0,  5, 2, 1,  1, 3, 7))

# Helper: one row of an association results table, by measure name
measureRow <- function(df, pattern) df[grepl(pattern, df$measure), , drop = FALSE][1, ]

# 2 x 2 x 3 table in which the same cell is 0 in every stratum
# (Mantel-Haenszel common odds ratio not finite)
zeroCellStrata <- data.frame(
    r = factor(rep(c("y", "n"), times = 6)),
    c = factor(rep(rep(c("e", "u"), each = 2), times = 3)),
    s = factor(rep(c("s1", "s2", "s3"), each = 4)),
    n = c(10, 5, 0, 8,  12, 4, 0, 9,  7, 6, 0, 10))

# 3 x 2 x 2 table with a rare row in the first stratum
rareRowStrata <- data.frame(
    a = factor(rep(c("x", "y", "z"), times = 4)),
    b = factor(rep(rep(c("p", "q"), each = 3), times = 2)),
    s = factor(rep(c("k1", "k2"), each = 6)),
    n = c(1, 20, 15,  2, 18, 25,  10, 12, 9,  11, 8, 14))
