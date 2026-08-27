# Phase 0.4 — leave-one-out validation of the interpolator.
# For each INTERIOR level of each axis, drop the cell and rebuild it by linear
# interpolation on the empirical-logit scale from its two immediate neighbours
# (all other design params held fixed). Compare to the actual simulated value.
# Settles: which axes may we interpolate, and what is the honest error band?

#
# Runs over EVERY base slab, not just the paper's. Each (model, schedule)
# partition is interpolated along N by the same code, so each needs its own
# measured error band — a number taken from Model 3's grid is an assumption
# about the others, and the whole point of this file is to not assume.
SIM <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(SIM) || !dir.exists(file.path(SIM, "R"))) SIM <- normalizePath(".")

RC   <- file.path(SIM, "results-confirmatory")
TOOL <- file.path(SIM, "results-tool")
slab_files <- c(file.path(RC, "primary.csv"),
                if (dir.exists(TOOL)) setdiff(list.files(TOOL, "\\.csv$", full.names = TRUE),
                                              list.files(TOOL, "\\.part\\.csv$", full.names = TRUE)))
slab_files <- slab_files[file.exists(slab_files)]
SLABS <- lapply(slab_files, function(f) read.csv(f, stringsAsFactors = FALSE))
names(SLABS) <- vapply(SLABS, function(d) as.character(d$grid[1]), character(1))
p <- SLABS[["primary"]]

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

# =============================================================================
# Per-partition N-interpolation error.
#
# lookup.R interpolates N and reports interp_se_power scaled to the p90 of this
# error. That p90 was measured on the paper's grid alone; every other partition
# uses the SAME interpolator, so each needs its own measured number. What is
# printed below is what belongs in LOO_P90_N_BY_SLAB.
# =============================================================================
cat("\n\n=== N-interpolation error by slab (power_all, percentage points) ===\n")
cat("cells at the power ceiling cannot show interpolation error, so the\n")
cat("sub-ceiling column is the one that matters for a planning answer.\n\n")
cat(sprintf("%-14s %5s %7s %7s %7s %9s\n", "slab", "n", "median", "p90", "max", "p90(sub)"))
summ <- list()
for (nm in names(SLABS)) {
  d <- SLABS[[nm]]
  if (length(unique(d$N)) < 3) { cat(sprintf("%-14s  (N has no interior level)\n", nm)); next }
  r <- loo_axis(d, AXES$N, METRICS$power_all)
  if (is.null(r) || !nrow(r)) next
  e   <- abs(r$err_pts)
  sub <- abs(r$err_pts[!r$at_ceiling])
  p90 <- unname(quantile(e, .9))
  summ[[nm]] <- p90
  cat(sprintf("%-14s %5d %7.2f %7.2f %7.2f %9.2f\n", nm, nrow(r), median(e), p90, max(e),
              if (length(sub)) unname(quantile(sub, .9)) else NA_real_))
}
cat("\n--- as an R literal for lookup.R ---\n")
cat("LOO_P90_N_BY_SLAB <- list(\n")
cat(paste(sprintf('  `%s` = %.4f', names(summ), unlist(summ) / 100), collapse = ",\n"), "\n)\n")
cat(sprintf("\nworst slab: %s at %.2f pts | paper's grid (primary): %.2f pts\n",
            names(summ)[which.max(unlist(summ))], max(unlist(summ)), summ[["primary"]]))

# =============================================================================
# Per-slab D-interpolation error.
#
# Measured UNDER THE GUARD the lookup actually applies (D_INTERP_MIN_CONV): a
# bracket that did not converge is refused rather than interpolated, so counting
# its error here would describe a code path that does not exist. Slabs whose
# durations are too far apart to interpolate at all simply do not qualify — the
# printed list is what belongs in LOO_P90_D_BY_SLAB, and a slab's ABSENCE from
# it is what keeps lookup.R snapping instead of interpolating.
# =============================================================================
MIN_CONV <- 0.80
MIN_LEVELS <- 8L   # too few durations and "dense enough to interpolate" is a fiction

cat("\n\n=== D-interpolation error by slab (power_all, percentage points) ===\n")
cat(sprintf("guard: both brackets must have converged >= %.0f%%\n\n", 100*MIN_CONV))
cat(sprintf("%-14s %6s %7s %7s %7s %9s\n","slab","n","median","p90","max","guarded"))
dsumm <- list()
for (nm in names(SLABS)) {
  d <- SLABS[[nm]]; lv <- sort(unique(d$D))
  if (length(lv) < MIN_LEVELS) {
    cat(sprintf("%-14s  (only %d durations — not dense enough to interpolate)\n", nm, length(lv)))
    next
  }
  e <- c(); guarded <- 0L
  for (v in lv[-c(1,length(lv))]) {
    lo <- max(lv[lv<v]); hi <- min(lv[lv>v])
    for (i in which(d$D==v)) {
      r <- d[i,]
      a <- d[d$D==lo & d$N==r$N & d$lambda_bar==r$lambda_bar,]
      b <- d[d$D==hi & d$N==r$N & d$lambda_bar==r$lambda_bar,]
      if (nrow(a)!=1 || nrow(b)!=1) next
      if (min(a$conv_rate, b$conv_rate) < MIN_CONV) { guarded <- guarded + 1L; next }
      w <- (v-lo)/(hi-lo)
      pr <- plogis(emp_logit(a$power_all,a$R_total) +
                   w*(emp_logit(b$power_all,b$R_total)-emp_logit(a$power_all,a$R_total)))
      e <- c(e, abs(pr-r$power_all)*100)
    }
  }
  if (!length(e)) next
  p90 <- unname(quantile(e,.9)); dsumm[[nm]] <- p90
  cat(sprintf("%-14s %6d %7.2f %7.2f %7.2f %9d\n", nm, length(e), median(e), p90, max(e), guarded))
}
if (length(dsumm)) {
  cat("\n--- as an R literal for lookup.R ---\n")
  cat("LOO_P90_D_BY_SLAB <- list(\n")
  cat(paste(sprintf('  `%s` = %.4f', names(dsumm), unlist(dsumm)/100), collapse=",\n"), "\n)\n")
  cat(sprintf("\nworst: %s at %.2f pts | N band for comparison: 0.58-4.02 pts\n",
              names(dsumm)[which.max(unlist(dsumm))], max(unlist(dsumm))))
}
