# =============================================================================
# lafit-crosscheck.R — validation check (i): reproduce PowerAnalysisIL.
#
#   Rscript tools/lafit-crosscheck.R          (run from the repo root)
#
# Confirms that at the balanced reduction (CV -> 0, no cap, full compliance,
# exogenous triggering, fixed occasion count, no autoregression) this engine
# reproduces PowerAnalysisIL (Lafit et al., 2021) power to within Monte Carlo
# error — the check that entitles the paper to call the triggered framework a
# strict superset of the fixed-count tool rather than a re-implementation.
# Recorded result: results-confirmatory/lafit-crosscheck.txt (manuscript §4.1).
#
# NOT part of CI: it needs network access (to fetch Lafit's code), the nlme
# package, and ~15 min of compute. Lafit's two source files are DOWNLOADED at a
# pinned commit rather than vendored, so none of their code lives in this repo.
# =============================================================================
suppressWarnings(suppressMessages({
  library(nlme); library(MASS); library(lme4); library(compiler)
  if (requireNamespace("data.table", quietly = TRUE)) library(data.table)  # Sim.Data.IL sources it
}))

# --- fetch PowerAnalysisIL's DGM + fit at a pinned commit --------------------
PAIL_SHA <- "11ec9e12c1ef8f38f928fc9c1dc78cf4f18966a9"
base <- sprintf("https://raw.githubusercontent.com/ginettelafit/PowerAnalysisIL/%s/R", PAIL_SHA)
tmp  <- tempfile("pail-"); dir.create(tmp)
for (f in c("Sim.Data.IL.R", "Fit.model.IL.R")) {
  dest <- file.path(tmp, f)
  ok <- tryCatch({ utils::download.file(file.path(base, f), dest, quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!ok || !file.exists(dest))
    stop("Could not download ", f, " from PowerAnalysisIL. Network access is required.")
  source(dest)
}

# --- our engine --------------------------------------------------------------
for (f in c("seeds", "models", "dgm", "fit", "performance"))
  source(file.path("R", paste0(f, ".R")))

# --- matched true parameters (shared by both engines) ------------------------
sd_b0 <- 1; sd_b1 <- 0.3; rho_b <- 0; sd_e <- 1
R <- 800; SEED <- 20260709
cells <- list(list(N = 60, T = 42, eff = 0.08),
              list(N = 60, T = 42, eff = 0.11),
              list(N = 30, T = 42, eff = 0.11))

# PowerAnalysisIL: its own DGM + fit, Model 3, no AR (is.rho.zero = FALSE).
run_pail <- function(N, T, eff) {
  det <- 0L; conv <- 0L
  for (r in seq_len(R)) {
    fit <- try(Fit.model.IL(Model = 3, N = N, N.0 = 0, N.1 = 0, T = T,
      isX.center = FALSE, Ylag.center = FALSE, isW.center = FALSE,
      b00 = 0, b01.Z = 0, b01.W = 0, b10 = eff, b11.Z = 0, b11.W = 0,
      sigma = sd_e, rho = 0, sigma.v0 = sd_b0, sigma.v1 = sd_b1, rho.v = rho_b,
      mu.W = 0, sigma.W = 1, mu.X = 0, mu.X0 = 0, mu.X1 = 0,
      sigma.X = 1, sigma.X0 = 1, sigma.X1 = 1, Opt.Method = 2, is.rho.zero = FALSE),
      silent = TRUE)
    if (inherits(fit, "try-error") || is.character(fit) ||
        is.null(fit$tTable) || !("X" %in% rownames(fit$tTable))) next
    conv <- conv + 1L
    if (fit$tTable["X", "p-value"] < 0.05) det <- det + 1L
  }
  list(power = det / conv, mcse = sqrt((det/conv) * (1 - det/conv) / conv), conv = conv / R)
}

# Our DGM + a PowerAnalysisIL-equivalent fit (no y_lag), to isolate the effect
# of our lagged-outcome control.
run_ours_like <- function(N, T, eff) {
  pars <- modifyList(default_pars(), list(beta1 = eff, phi = 0, lambda_bar = 3,
            cv = 0, D = T/3, cap = Inf, compliance = 1, decay = 0, trigger_link = 0,
            trigger_mode = "fixed", model = 3L,
            sd_b0 = sd_b0, sd_b1 = sd_b1, rho_b = rho_b, sd_e = sd_e))
  det <- 0L; conv <- 0L
  for (r in seq_len(R)) {
    set.seed(rep_seed(SEED, 7000, r))
    df <- generate_dataset(N, T/3, pars)
    fit <- tryCatch(lmer(y ~ x + (1 + x | id), df, REML = TRUE,
                         control = lmerControl(calc.derivs = FALSE)),
                    error = function(e) NULL)
    if (is.null(fit) || isSingular(fit)) next
    conv <- conv + 1L
    fe <- fixef(fit); se <- sqrt(diag(as.matrix(vcov(fit))))
    if (2 * pnorm(-abs(unname(fe["x"] / se["x"]))) < 0.05) det <- det + 1L
  }
  list(power = det / conv, conv = conv / R)
}

cat(sprintf("PowerAnalysisIL cross-check | R=%d seed=%d | Lafit @ %s\n\n", R, SEED, substr(PAIL_SHA, 1, 10)))
cat(sprintf("%-24s %-16s %-16s %-16s\n", "cell", "PowerAnalysisIL", "ours(engine)", "ours(like)"))
for (c in cells) {
  set.seed(SEED)
  pail <- run_pail(c$N, c$T, c$eff)
  ours <- run_cell(data.frame(N = c$N, lambda_bar = 3, cv = 0, D = c$T/3, cap = Inf,
                     compliance = 1, decay = 0, phi = 0, beta1 = c$eff,
                     trigger_link = 0, trigger_mode = "fixed"),
                   master_seed = SEED, R = R, cell_id = 8000)
  like <- run_ours_like(c$N, c$T, c$eff)
  cat(sprintf("N=%d T=%d b1=%.2f%s %.3f (MCSE %.3f)  %.3f (d %+.3f)   %.3f\n",
      c$N, c$T, c$eff, strrep(" ", max(1, 6 - nchar(sprintf("%.2f", c$eff)))),
      pail$power, pail$mcse, ours$power, ours$power - pail$power, like$power))
}
cat("\nPASS if every |ours(engine) - PowerAnalysisIL| is within Monte Carlo error.\n")
