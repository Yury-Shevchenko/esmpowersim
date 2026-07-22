# =============================================================================
# fit.R — Fit the analysis model to one simulated dataset and extract the
#         target estimate, its test, CI, and a clean convergence flag.
#
# Analysis model (prereg §3-M): random intercept + random slope for x, with the
# AR term entered as the lagged outcome (lme4, REML) — the same lagged-outcome
# parameterisation PowerAnalysisIL uses, so the Lafit special case is comparable.
#
#   y ~ x + y_lag + (1 + x | id)
#
# The lag is within-person over consecutive realised occasions; each person's
# first occasion has no lag and is dropped as a predictor row (standard for the
# lagged-outcome parameterisation).
# =============================================================================

suppressWarnings(suppressMessages(library(lme4)))

# add within-person lag-1 outcome; drop the first occasion per person
add_lag <- function(df) {
  df <- df[order(df$id, df$occ), ]
  df$y_lag <- ave(df$y, df$id, FUN = function(v) c(NA, head(v, -1)))
  df[!is.na(df$y_lag), , drop = FALSE]
}

# Build the fit formula and the name of the target coefficient for a model spec.
# One definition drives all 11 models (see models.R). For the default Model 3
# this is exactly  y ~ x + y_lag + (1 + x | id)  with target "x".
model_formula <- function(spec) {
  P <- if (spec$l1 == "lag") "y_lag" else "x"    # the Level-1 predictor
  terms <- character(0)
  if (spec$l1 != "none") terms <- c(terms, P)
  if (spec$l1 != "lag")  terms <- c(terms, "y_lag")   # nuisance AR control (Models 1-8)
  if (spec$l2 != "none") terms <- c(terms, "W")
  if (spec$target == "b11") terms <- c(terms, paste0(P, ":W"))
  reff <- if (spec$slope && spec$l1 != "none") sprintf("(1 + %s | id)", P) else "(1 | id)"
  stats::as.formula(sprintf("y ~ %s + %s", paste(terms, collapse = " + "), reff))
}

# Fit and extract the TARGET effect for `model` (default 3). Returns a one-row
# data.frame: est, se, z, p, ci_lo, ci_hi, converged (logical), n_used, n_persons.
fit_one <- function(df, alpha = 0.05, model = NULL) {
  spec <- model_spec(model)
  na_row <- data.frame(est = NA_real_, se = NA_real_, z = NA_real_, p = NA_real_,
                       ci_lo = NA_real_, ci_hi = NA_real_, converged = FALSE,
                       n_used = 0L, n_persons = 0L)
  df <- add_lag(df)
  # need enough persons and rows to fit a random-slope model at all
  if (nrow(df) < 10L || length(unique(df$id)) < 5L) return(na_row)

  form   <- model_formula(spec)
  target <- target_coef(spec)

  msgs <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      lmer(form, data = df, REML = TRUE, control = lmerControl(calc.derivs = FALSE)),
      error = function(e) NULL
    ),
    warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  if (is.null(fit)) return(na_row)

  # clean convergence: no optimizer warning captured AND not a singular fit
  conv_msg <- tryCatch(fit@optinfo$conv$lme4$messages, error = function(e) NULL)
  converged <- length(msgs) == 0L && length(conv_msg) == 0L && !isSingular(fit)

  fe  <- fixef(fit)
  ses <- sqrt(diag(as.matrix(vcov(fit))))
  if (!(target %in% names(fe))) return(na_row)   # design too sparse to estimate it
  est <- unname(fe[target]); se <- unname(ses[target])
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))                       # Wald z (as in PowerAnalysisIL)
  crit <- qnorm(1 - alpha / 2)

  data.frame(est = est, se = se, z = z, p = p,
             ci_lo = est - crit * se, ci_hi = est + crit * se,
             converged = converged,
             n_used = nrow(df), n_persons = length(unique(df$id)))
}
