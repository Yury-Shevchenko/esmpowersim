# =============================================================================
# validate.R — Pre-run validation (prereg §7). Must pass before the confirmatory
# run. Three checks:
#   (1) Lafit special case: CV->0, no cap, full compliance, exogenous, fixed rate
#       reduces to a balanced design; report power so it can be matched against
#       against PowerAnalysisIL with the same N, occasions, and effect.
#   (2) Recovery: large N & n -> negligible bias, ~95% CI coverage.
#   (3) DGM sanity: realised effective-n matches the intended rate/cap/compliance.
#
# Usage: Rscript R/validate.R --R=300 --seed=20260709
# =============================================================================

suppressWarnings(suppressMessages({
  here <- tryCatch(dirname(sub("^--file=", "",
           grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), error = function(e) "R")
  if (is.na(here) || here == "") here <- "R"
  source(file.path(here, "seeds.R")); source(file.path(here, "models.R"))
  source(file.path(here, "dgm.R"));   source(file.path(here, "fit.R"))
  source(file.path(here, "performance.R"))
}))

getarg <- function(key, default) {
  a <- commandArgs(TRUE); hit <- grep(paste0("^--", key, "="), a, value = TRUE)
  if (length(hit) == 0) default else sub(paste0("^--", key, "="), "", hit[1])
}

# --- (1) Lafit special case --------------------------------------------------
# trigger_mode="fixed" + cap=Inf + compliance=1 + exogenous => EXACTLY
# lambda_bar*D occasions for every person: a genuinely balanced design, which is
# the design PowerAnalysisIL assumes. sd_n must come out at 0.00 — that is the
# check, and it is what entitles us to call this a fixed-count reduction rather
# than an approximation to one.
#
# This used to run in Poisson mode with cv=0, which only equalises the RATE: the
# counts kept Poisson spread (sd_n ~ 6.4 at these settings), so it approximated
# balance rather than reducing to it, and the comparison against a fixed-T tool
# was never like-for-like.
lafit_special_case <- function(R, seed) {
  cat("\n(1) Lafit special case (exact balanced design) — power vs N:\n")
  cat("    match these against PowerAnalysisIL: exogenous, fixed occasions,",
      "same beta1, phi, variance components.\n")
  cells <- expand.grid(N = c(30, 60, 100), lambda_bar = 3, cv = 0, D = 14,
                       cap = Inf, compliance = 1, decay = 0, phi = 0.3,
                       beta1 = 0.3, trigger_link = 0,
                       trigger_mode = "fixed", stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cells))) {
    r <- run_cell(cells[i, , drop = FALSE], seed, R, cell_id = 900 + i)
    cat(sprintf("    N=%3d  occ/person=%.1f (sd %.2f)  power=%.3f (MCSE %.3f)  coverage=%.3f  conv=%.2f\n",
                r$N, r$mean_n, r$sd_n, r$power, r$mcse_power, r$coverage, r$conv_rate))
  }
}

# --- (2) Recovery ------------------------------------------------------------
recovery_check <- function(R, seed) {
  cat("\n(2) Recovery (large N & n) — expect |rel_bias|<.05, coverage~.95:\n")
  cell <- data.frame(N = 150, lambda_bar = 6, cv = 0.3, D = 28, cap = Inf,
                     compliance = 0.9, decay = 0, phi = 0.3, beta1 = 0.3,
                     trigger_link = 0)
  r <- run_cell(cell, seed, R, cell_id = 950)
  cat(sprintf("    est bias=%.4f  rel_bias=%.3f  coverage=%.3f  emp_se=%.4f  power=%.3f  mean_n=%.1f\n",
              r$bias, r$rel_bias, r$coverage, r$emp_se, r$power, r$mean_n))
  ok <- is.finite(r$rel_bias) && abs(r$rel_bias) < 0.05 &&
        is.finite(r$coverage) && abs(r$coverage - 0.95) < 0.05
  cat("    -> ", if (ok) "PASS" else "REVIEW", "\n")
}

# --- (3) DGM sanity ----------------------------------------------------------
dgm_sanity <- function(seed) {
  cat("\n(3) DGM sanity — realised effective-n vs intended:\n")
  set.seed(rep_seed(seed, 800, 1))
  chk <- function(label, pars, N = 400, D = 14) {
    df <- generate_dataset(N, D, pars)
    n_i <- as.integer(table(factor(df$id, levels = seq_len(N))))
    cat(sprintf("    %-34s mean_n=%.2f  sd_n=%.2f  (expected mean ~ %.2f)\n",
                label, mean(n_i), sd(n_i), pars$lambda_bar * D * pars$compliance))
  }
  p <- default_pars()
  chk("rate=2, p=.75, cap=Inf, cv=.3",  modifyList(p, list(lambda_bar=2, compliance=.75, cv=.3)))
  chk("rate=2, p=.75, cap=Inf, cv=.9",  modifyList(p, list(lambda_bar=2, compliance=.75, cv=.9)))
  chk("rate=4, p=.60, cap=3,   cv=.3",  modifyList(p, list(lambda_bar=4, compliance=.60, cv=.3, cap=3)))
  cat("    (capped rows reduce mean_n below the uncapped expectation — expected.)\n")
}

main <- function() {
  R <- as.integer(getarg("R", "300")); seed <- as.integer(getarg("seed", "20260709"))
  cat(sprintf("[validate] R=%d seed=%d\n", R, seed))
  dgm_sanity(seed)
  recovery_check(R, seed)
  lafit_special_case(R, seed)
  cat("\n[validate] done. Check (1) is confirmed by tools/lafit-crosscheck.R;",
      "recorded in results-confirmatory/lafit-crosscheck.txt.\n")
}

if (sys.nframe() == 0L) main()
