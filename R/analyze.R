# =============================================================================
# analyze.R — Confirmatory analysis of a results table (prereg §5).
#
# Reads a primary results CSV (and optionally a secondary one) and runs the four
# preregistered hypothesis tests with their fixed decision criteria, writes the
# power/convergence surfaces + diagnostic figures, and emits an analysis summary.
#
#   H1  power monotone non-decreasing in N, rate, D (MCSE-sized slack allowed)
#   H2  matched-expected-total: higher CV lowers power (>=80% of pairs, median
#       gap >=5 pts) AND adjusted high-CV penalty < -0.02 with 95% CI excluding 0
#   H3  lower-tail P(n_i<10) predicts power better than total prompts or mean n
#   H4  non-convergence rises with CV, falls with D and rate (and, from secondary
#       grids, falls with compliance / rises with tighter caps)
#
# Everything is deterministic (LOOCV via hat values; no random folds), so the
# verdict is exactly reproducible. Prereg §5, §3-P.
#
# Usage:
#   Rscript R/analyze.R --primary=results/primary.csv \
#       [--secondary=results/secondary.csv] [--outdir=results/analysis]
# =============================================================================

getarg <- function(key, default) {
  a <- commandArgs(TRUE); hit <- grep(paste0("^--", key, "="), a, value = TRUE)
  if (length(hit) == 0) default else sub(paste0("^--", key, "="), "", hit[1])
}

emp_logit <- function(power, R) {                 # empirical logit, robust at 0/1
  k <- round(power * R)
  log((k + 0.5) / (R - k + 0.5))
}

# LOOCV R^2 for an lm, computed exactly from hat values (PRESS); no refitting.
loocv_r2 <- function(form, data) {
  fit <- lm(form, data = data)
  h   <- hatvalues(fit); r <- residuals(fit)
  press <- sum((r / (1 - h))^2)
  tss   <- sum((data[[all.vars(form)[1]]] - mean(data[[all.vars(form)[1]]]))^2)
  1 - press / tss
}

# ---------------------------------------------------------------------------
# H1 — monotonicity
# ---------------------------------------------------------------------------
test_H1 <- function(d) {
  check_factor <- function(factor, group_by) {
    viol <- 0L; seqs <- 0L
    g <- split(d, interaction(d[group_by], drop = TRUE))
    for (grp in g) {
      grp <- grp[order(grp[[factor]]), ]
      if (nrow(grp) < 2) next
      seqs <- seqs + 1L
      for (i in 2:nrow(grp)) {
        drop_ <- grp$power[i] - grp$power[i - 1]
        tol   <- 2 * sqrt(grp$mcse_power[i]^2 + grp$mcse_power[i - 1]^2)
        if (!is.na(drop_) && drop_ < -tol) viol <- viol + 1L
      }
    }
    c(violations = viol, sequences = seqs)
  }
  n_r   <- check_factor("N",          c("lambda_bar", "cv", "D"))
  rate_ <- check_factor("lambda_bar", c("N", "cv", "D"))
  d_r   <- check_factor("D",          c("N", "cv", "lambda_bar"))
  total_viol <- n_r["violations"] + rate_["violations"] + d_r["violations"]
  list(N = n_r, rate = rate_, D = d_r,
       supported = unname(total_viol == 0),
       note = sprintf("MCSE-sized violations: N=%d, rate=%d, D=%d (of %d/%d/%d sequences)",
                      n_r["violations"], rate_["violations"], d_r["violations"],
                      n_r["sequences"], rate_["sequences"], d_r["sequences"]))
}

# ---------------------------------------------------------------------------
# H2 — matched-expected-total CV effect (headline)
# ---------------------------------------------------------------------------
test_H2 <- function(d) {
  cvs <- sort(unique(d$cv))
  if (length(cvs) < 2) return(list(supported = NA, note = "only one CV level present"))
  lo <- cvs[1]; hi <- cvs[length(cvs)]
  key <- with(d, paste(N, lambda_bar, D, cap, compliance, phi, beta1, trigger_link))
  d$key <- key
  wide <- reshape(d[d$cv %in% c(lo, hi), c("key", "cv", "power")],
                  timevar = "cv", idvar = "key", direction = "wide")
  colnames(wide) <- sub("power\\.", "p", colnames(wide))
  lo_col <- paste0("p", lo); hi_col <- paste0("p", hi)
  wide <- wide[stats::complete.cases(wide[, c(lo_col, hi_col)]), ]
  gap  <- wide[[hi_col]] - wide[[lo_col]]               # high - low ; expect < 0
  n_pairs <- length(gap)
  frac_lower <- mean(gap < 0)
  med_gap    <- median(gap)

  # adjusted penalty: power ~ N + rate + D + highCV(0/1), coefficient on highCV
  d$highCV <- as.integer(d$cv == hi)
  m <- lm(power ~ scale(N) + scale(lambda_bar) + scale(D) + highCV, data = d)
  ci <- confint(m)["highCV", ]
  coef_hi <- coef(m)["highCV"]

  crit_pairs <- frac_lower >= 0.80 && med_gap <= -0.05
  crit_model <- coef_hi < 0 && ci[2] < -0.02        # 95% CI upper bound below -0.02
  list(supported = crit_pairs && crit_model,
       n_pairs = n_pairs, frac_lower = frac_lower, median_gap = med_gap,
       highCV_coef = unname(coef_hi), highCV_ci = unname(ci), gap = gap, wide = wide,
       note = sprintf("pairs=%d  P(high<low)=%.2f  median gap=%.3f  adj.penalty=%.3f [%.3f, %.3f]",
                      n_pairs, frac_lower, med_gap, coef_hi, ci[1], ci[2]))
}

# ---------------------------------------------------------------------------
# H3 — lower-tail predictor beats total-prompts and mean-n
# ---------------------------------------------------------------------------
test_H3 <- function(d) {
  d <- d[d$R_converged > 0 & is.finite(d$power), ]
  d$logit_power   <- emp_logit(d$power, d$R_converged)
  d$total_prompts <- d$N * d$lambda_bar * d$D          # the plannable proxy
  preds <- c(total_prompts = "total_prompts", mean_n = "mean_n", below_k = "below_k")
  uni <- sapply(preds, function(p) {
    d$z <- as.numeric(scale(d[[p]]))
    loocv_r2(logit_power ~ z, d)
  })
  # incremental over a base model with N (all three correlate with N)
  d$zN <- as.numeric(scale(d$N))
  base_r2 <- loocv_r2(logit_power ~ zN, d)
  incr <- sapply(preds, function(p) {
    d$z <- as.numeric(scale(d[[p]]))
    loocv_r2(logit_power ~ zN + z, d) - base_r2
  })
  winner_uni  <- names(which.max(uni))
  winner_incr <- names(which.max(incr))
  list(supported = winner_uni == "below_k" && winner_incr == "below_k",
       loocv_r2 = uni, incremental_r2 = incr, base_r2 = base_r2,
       note = sprintf("univariate LOOCV-R2: total=%.3f mean_n=%.3f below_k=%.3f (winner=%s); incr over N winner=%s",
                      uni["total_prompts"], uni["mean_n"], uni["below_k"], winner_uni, winner_incr))
}

# ---------------------------------------------------------------------------
# H4 — drivers of non-convergence
# ---------------------------------------------------------------------------
test_H4 <- function(d, sec = NULL) {
  d$conv_fail <- 1 - d$conv_rate
  d$highCV <- as.integer(d$cv == max(d$cv))
  m <- lm(conv_fail ~ scale(N) + scale(lambda_bar) + scale(D) + highCV, data = d)
  ci <- confint(m)
  sgn <- function(term, want) {
    lo <- ci[term, 1]; hi <- ci[term, 2]; est <- coef(m)[term]
    ok <- if (want > 0) lo > 0 else hi < 0
    list(est = unname(est), ci = unname(c(lo, hi)), ok = ok)
  }
  res <- list(
    highCV = sgn("highCV", +1),          # more non-convergence with high CV
    D      = sgn("scale(D)", -1),        # less with longer duration
    rate   = sgn("scale(lambda_bar)", -1)# less with higher rate
  )
  extra <- list()
  if (!is.null(sec)) {
    s2 <- sec[sec$grid == "S2_compliance", ]
    if (nrow(s2) > 5 && length(unique(s2$compliance)) > 1) {
      s2$conv_fail <- 1 - s2$conv_rate
      m2 <- lm(conv_fail ~ scale(N) + scale(lambda_bar) + scale(D) + scale(compliance), data = s2)
      ci2 <- confint(m2)["scale(compliance)", ]
      extra$compliance <- list(est = unname(coef(m2)["scale(compliance)"]),
                               ci = unname(ci2), ok = ci2[2] < 0)   # less with more compliance
    }
    s1 <- sec[sec$grid == "S1_cap" & is.finite(sec$cap), ]
    if (nrow(s1) > 5 && length(unique(s1$cap)) > 1) {
      s1$conv_fail <- 1 - s1$conv_rate
      m1 <- lm(conv_fail ~ scale(N) + scale(lambda_bar) + scale(D) + scale(cap), data = s1)
      ci1 <- confint(m1)["scale(cap)", ]
      extra$cap <- list(est = unname(coef(m1)["scale(cap)"]),
                        ci = unname(ci1), ok = ci1[1] > 0)          # tighter cap (smaller) -> more fail
    }
  }
  core_ok <- res$highCV$ok && res$D$ok && res$rate$ok
  list(supported = core_ok, core = res, extra = extra,
       note = sprintf("highCV=%.3f%s  D=%.3f%s  rate=%.3f%s",
                      res$highCV$est, ifelse(res$highCV$ok, "*", ""),
                      res$D$est, ifelse(res$D$ok, "*", ""),
                      res$rate$est, ifelse(res$rate$ok, "*", "")))
}

# ---------------------------------------------------------------------------
# Figures (base graphics -> PNG; no external plotting deps)
# ---------------------------------------------------------------------------
make_figures <- function(d, outdir, H2) {
  fig <- function(name, w = 1100, h = 850) png(file.path(outdir, name), width = w, height = h, res = 110)
  cvs <- sort(unique(d$cv)); cols <- c("#1b6ca8", "#c0392b")
  panels <- expand.grid(lambda_bar = sort(unique(d$lambda_bar)), D = sort(unique(d$D)))

  surface <- function(yvar, ylab, file, ref = NULL) {
    fig(file)
    op <- par(mfrow = c(length(unique(d$D)), length(unique(d$lambda_bar))),
              mar = c(3.2, 3.4, 2, 0.6), oma = c(0, 0, 2.4, 0), mgp = c(2, 0.6, 0))
    for (r in seq_len(nrow(panels))) {
      lam <- panels$lambda_bar[r]; Dd <- panels$D[r]
      sub <- d[d$lambda_bar == lam & d$D == Dd, ]
      plot(NA, xlim = range(d$N), ylim = c(0, 1), xlab = "N", ylab = ylab,
           main = sprintf("rate=%g / day, D=%g d", lam, Dd), cex.main = 0.95)
      if (!is.null(ref)) abline(h = ref, col = "grey70", lty = 2)
      for (j in seq_along(cvs)) {
        s <- sub[sub$cv == cvs[j], ]; s <- s[order(s$N), ]
        lines(s$N, s[[yvar]], col = cols[j], lwd = 2, type = "b", pch = 19, cex = 0.7)
      }
    }
    mtext(sprintf("%s — blue CV=%g, red CV=%g", ylab, cvs[1], cvs[length(cvs)]),
          outer = TRUE, cex = 1.05, font = 2)
    par(op); dev.off()
  }
  surface("power",     "power",            "power-surface.png",       ref = 0.8)
  surface("conv_rate", "convergence rate", "convergence-surface.png", ref = 0.95)
  surface("below_k",   "P(n_i < 10)",      "lower-tail-surface.png")

  # H2 paired-difference plot
  if (!is.null(H2$gap) && length(H2$gap)) {
    fig("h2-paired-diff.png", h = 620)
    op <- par(mar = c(4, 4, 2.5, 1))
    g <- sort(H2$gap)
    barplot(g, col = ifelse(g < 0, cols[1], cols[2]), border = NA,
            main = "H2: power(high CV) − power(low CV), matched expected total",
            ylab = "power difference", xlab = "matched (N, rate, D) pairs, sorted")
    abline(h = 0, col = "black"); abline(h = median(g), col = "darkgreen", lty = 2, lwd = 2)
    legend("topleft", bty = "n", lty = 2, col = "darkgreen",
           legend = sprintf("median gap = %.3f", median(g)))
    par(op); dev.off()
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------
write_summary <- function(path, H1, H2, H3, H4, meta) {
  yn <- function(x) if (is.na(x)) "—" else if (isTRUE(x)) "**supported**" else "**not supported**"
  lines <- c(
    "# Analysis summary — ESM power simulation",
    "",
    sprintf("> Auto-generated by `analyze.R` from `%s` (%d cells%s). Confirmatory tests per prereg §5.",
            meta$primary, meta$n_cells, if (nzchar(meta$secondary)) sprintf(" + secondary `%s`", meta$secondary) else ""),
    sprintf("> Master seed %s, R requested %s. Auto-generated; verify before citing.", meta$seed, meta$R),
    "",
    "| Hypothesis | Verdict | Key numbers |",
    "|---|---|---|",
    sprintf("| H1 monotonicity | %s | %s |", yn(H1$supported), H1$note),
    sprintf("| H2 CV lowers power (headline) | %s | %s |", yn(H2$supported), H2$note),
    sprintf("| H3 lower-tail best predictor | %s | %s |", yn(H3$supported), H3$note),
    sprintf("| H4 non-convergence drivers | %s | %s |", yn(H4$supported), H4$note),
    "",
    "## Figures",
    "- `power-surface.png` — power vs N, faceted by rate × duration, by CV (the planning lookup)",
    "- `convergence-surface.png` — convergence rate surface",
    "- `lower-tail-surface.png` — P(n_i < 10), the starvation mechanism",
    "- `h2-paired-diff.png` — matched-pair CV power differences",
    "",
    "## Notes",
    "- Verdicts apply the fixed criteria in prereg §5; exploratory reading of the surfaces is separate (§8).",
    "- H4 compliance/cap signs require the secondary grids (S1/S2); reported only if present.",
    if (!is.null(H4$extra$compliance)) sprintf("- H4 compliance coef = %.3f (CI %.3f, %.3f) %s",
        H4$extra$compliance$est, H4$extra$compliance$ci[1], H4$extra$compliance$ci[2],
        ifelse(H4$extra$compliance$ok, "✓", "review")) else NULL,
    if (!is.null(H4$extra$cap)) sprintf("- H4 cap coef = %.3f (CI %.3f, %.3f) %s",
        H4$extra$cap$est, H4$extra$cap$ci[1], H4$extra$cap$ci[2],
        ifelse(H4$extra$cap$ok, "✓", "review")) else NULL
  )
  writeLines(lines[!vapply(lines, is.null, logical(1))], path)
}

main <- function() {
  primary <- getarg("primary", "results/primary.csv")
  secondary <- getarg("secondary", "")
  outdir <- getarg("outdir", "results/analysis")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  here <- tryCatch(dirname(sub("^--file=", "",
            grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), error = function(e) "R")
  if (!is.na(here) && nzchar(here) && file.exists(file.path(here, "repro.R")))
    source(file.path(here, "repro.R"))

  d <- read.csv(primary, stringsAsFactors = FALSE)
  sec <- if (nzchar(secondary) && file.exists(secondary)) read.csv(secondary, stringsAsFactors = FALSE) else NULL

  H1 <- test_H1(d); H2 <- test_H2(d); H3 <- test_H3(d); H4 <- test_H4(d, sec)
  make_figures(d, outdir, H2)

  meta <- list(primary = primary, secondary = secondary, n_cells = nrow(d),
               seed = if ("master_seed" %in% names(d)) d$master_seed[1] else "?",
               R = if ("R_requested" %in% names(d)) d$R_requested[1] else "?")
  summ <- file.path(outdir, "analysis-summary.md")
  write_summary(summ, H1, H2, H3, H4, meta)

  cat("\n==================  CONFIRMATORY VERDICTS (prereg §5)  ==================\n")
  cat(sprintf("H1 monotonicity        : %s   | %s\n", ifelse(isTRUE(H1$supported), "SUPPORTED", "NOT"), H1$note))
  cat(sprintf("H2 CV lowers power     : %s   | %s\n", ifelse(isTRUE(H2$supported), "SUPPORTED", "NOT"), H2$note))
  cat(sprintf("H3 lower-tail predictor: %s   | %s\n", ifelse(isTRUE(H3$supported), "SUPPORTED", "NOT"), H3$note))
  cat(sprintf("H4 non-convergence     : %s   | %s\n", ifelse(isTRUE(H4$supported), "SUPPORTED", "NOT"), H4$note))
  cat("========================================================================\n")
  cat("wrote:", summ, "and figures in", outdir, "\n")
  if (exists("write_run_meta"))
    write_run_meta(file.path(outdir, "analysis.csv"),
                   list(primary = primary, secondary = secondary), tag = "analyze")
}

if (sys.nframe() == 0L) main()
