# Phase 0.4 — leave-one-out validation of the interpolator.
# For each INTERIOR level of each axis, drop the cell and rebuild it by linear
# interpolation on the empirical-logit scale from its two immediate neighbours
# (all other design params held fixed). Compare to the actual simulated value.
# Settles: which axes may we interpolate, and what is the honest error band?

SIM <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(SIM) || !dir.exists(file.path(SIM, "R"))) SIM <- normalizePath(".")
p <- read.csv(file.path(SIM, "results-confirmatory", "primary.csv"), stringsAsFactors = FALSE)

emp_logit <- function(power, R) {           # analyze.R:28-31 — the manuscript's own scale
  k <- round(power * R)
  log((k + 0.5) / (R - k + 0.5))
}

# metric: which power column + its denominator
METRICS <- list(
  power_all = list(col = "power_all", Rcol = "R_total"),      # the planned headline
  power     = list(col = "power",     Rcol = "R_converged")   # convergence-conditional
)

# axes we intend to interpolate, and the scale we interpolate along
AXES <- list(
  N        = list(key = "N",          tx = identity),
  D        = list(key = "D",          tx = identity),
  logLmbda = list(key = "lambda_bar", tx = log)               # {1,2,4} geometric ⇒ log puts 2 midway
)
OTHERS <- c("N", "lambda_bar", "cv", "D")

loo_axis <- function(df, axis, metric) {
  key <- axis$key; tx <- axis$tx
  lv  <- sort(unique(df[[key]]))
  interior <- lv[-c(1, length(lv))]
  others <- setdiff(OTHERS, key)
  out <- list()
  for (v in interior) {
    lo <- max(lv[lv < v]); hi <- min(lv[lv > v])
    sub <- df[df[[key]] == v, ]
    for (i in seq_len(nrow(sub))) {
      row <- sub[i, ]
      sel <- function(val) {
        m <- df[[key]] == val
        for (o in others) m <- m & df[[o]] == row[[o]]
        df[m, ]
      }
      a <- sel(lo); b <- sel(hi)
      if (nrow(a) != 1 || nrow(b) != 1) next
      w  <- (tx(v) - tx(lo)) / (tx(hi) - tx(lo))          # interpolation weight
      la <- emp_logit(a[[metric$col]], a[[metric$Rcol]])
      lb <- emp_logit(b[[metric$col]], b[[metric$Rcol]])
      pred <- plogis(la + w * (lb - la))
      out[[length(out) + 1]] <- data.frame(
        level = v, actual = row[[metric$col]], pred = pred,
        err_pts = 100 * (pred - row[[metric$col]]),
        at_ceiling = row[[metric$col]] >= 0.95
      )
    }
  }
  do.call(rbind, out)
}

cat("LOO interpolation validation — primary.csv (144 cells, R=2000)\n")
cat("axis levels: N", paste(sort(unique(p$N)), collapse=","),
    "| D", paste(sort(unique(p$D)), collapse=","),
    "| lambda", paste(sort(unique(p$lambda_bar)), collapse=","),
    "| cv", paste(sort(unique(p$cv)), collapse=","), "\n\n")

for (mn in names(METRICS)) {
  cat(sprintf("=== metric: %s ===\n", mn))
  cat(sprintf("%-9s %5s  %7s %7s %7s   %7s\n", "axis", "n", "median", "p90", "max", "max(sub-ceil)"))
  for (an in names(AXES)) {
    r <- loo_axis(p, AXES[[an]], METRICS[[mn]])
    if (is.null(r)) next
    e <- abs(r$err_pts)
    sub <- abs(r$err_pts[!r$at_ceiling])
    cat(sprintf("%-9s %5d  %6.2f  %6.2f  %6.2f    %6.2f\n",
                an, nrow(r), median(e), quantile(e, .9), max(e),
                if (length(sub)) max(sub) else NA_real_))
  }
  cat("\n")
}

# Where does interpolation hurt most? (worst offenders on the headline metric)
r <- loo_axis(p, AXES$N, METRICS$power_all)
r <- r[order(-abs(r$err_pts)), ]
cat("worst N-interpolation errors (power_all, pts):\n")
print(head(data.frame(N = r$level, actual = round(r$actual, 3),
                      pred = round(r$pred, 3), err_pts = round(r$err_pts, 2)), 5), row.names = FALSE)

cat("\nNOTE: secondary.csv has D in {14,28} only — no interior level, so D-interpolation\n")
cat("      inside a secondary slab CANNOT be validated by LOO at all.\n")
