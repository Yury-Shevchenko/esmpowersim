# =============================================================================
# dgm.R — Data-generating mechanism for event/location-contingent ESM power sim
#
# Implements the preregistered data-generating mechanism (§3-D of the protocol):
#   (1) a TRIGGER PROCESS that yields each person's random, between-person-varying,
#       cap-truncated, compliance-thinned number of realised occasions, and
#   (2) a dynamic multilevel AR(1) OUTCOME MODEL evaluated at those occasions.
#
# No participant data is ever touched — every value here is synthetic.
# =============================================================================

# --- Trigger process ---------------------------------------------------------
# Two regimes, because they are genuinely different designs:
#
#   trigger_mode = "poisson"  event- / location-contingent. The prompt count is a
#       RANDOM variable: each day delivers rpois(1, lambda_i) candidate triggers.
#       lambda_i varies between people (see kappa_from_cv). This is the case no
#       existing power tool handles, and what the paper is about.
#
#   trigger_mode = "fixed"    time-contingent (fixed / interval / signal-contingent
#       schedules). The prompt count is PLANNED: exactly k = round(lambda_bar)
#       prompts every day, for everyone. There is no rate randomness and no
#       between-person rate variation, so `cv` does not apply here.
#
# Note "cv = 0" does NOT collapse the Poisson mode into the fixed mode: it only
# removes between-person variation in the RATE. Each day is still rpois(1, lambda),
# so the count keeps Poisson spread (sd = sqrt(lambda*D)). The two modes are
# distinct designs, not two ends of one dial.
#
# Person daily rate lambda_i ~ Gamma(shape = kappa, scale = lambda_bar / kappa)
#   => E[lambda_i] = lambda_bar ;  CV(lambda_i) = 1 / sqrt(kappa).
# So a target between-person CV maps to shape kappa = 1 / CV^2.
kappa_from_cv <- function(cv) {
  if (cv <= 0) return(Inf)          # CV -> 0: same rate for everyone (still Poisson counts)
  1 / (cv^2)
}

# Draw one person's realised occasions.
# Returns a data.frame with columns: day, x  (one row per realised, answered occasion),
# ordered in time; nrow() is the person's effective n_i.
#
#   D             : number of days
#   lambda_i      : this person's daily trigger rate
#   cap           : max ANSWERED occasions per day (Inf = no cap)
#   compliance    : base probability a delivered trigger is answered
#   decay         : per-day multiplicative fatigue on compliance (0 = none);
#                   compliance on day d = compliance * (1 - decay)^(d-1)
#   trigger_link  : 0 = exogenous (x independent of triggering);
#                   >0 = context-linked (triggers concentrate at high x -> range restriction)
#   mu_x          : mean of the latent context
#   trigger_mode  : "poisson" = random count (event/geofence);
#                   "fixed"   = planned count, exactly round(lambda_i) prompts per day
draw_person_occasions <- function(D, lambda_i, cap, compliance, decay,
                                   trigger_link, mu_x = 0,
                                   trigger_mode = "poisson") {
  fixed <- identical(trigger_mode, "fixed")
  k_planned <- if (fixed) as.integer(round(lambda_i)) else NA_integer_
  rows <- vector("list", D)
  for (d in seq_len(D)) {
    # The one substantive difference between the two designs: is the number of
    # prompts delivered today something the researcher SET, or something the
    # participant's context DID?
    n_delivered <- if (fixed) k_planned else rpois(1L, lambda_i)
    if (n_delivered == 0L) { rows[[d]] <- NULL; next }

    # latent context for each candidate trigger
    xc <- rnorm(n_delivered, mean = mu_x, sd = 1)

    # context-linked triggering: a candidate becomes a REAL trigger with prob
    # depending on x (logistic). Exogenous when trigger_link == 0 (all kept).
    if (trigger_link > 0) {
      keep_p <- plogis(trigger_link * xc)              # high x -> more likely triggered
      xc <- xc[runif(n_delivered) < keep_p]
    }
    if (length(xc) == 0L) { rows[[d]] <- NULL; next }

    # per-day cap on ANSWERED occasions -> cap the eligible pool first
    if (is.finite(cap) && length(xc) > cap) {
      xc <- xc[seq_len(cap)]
    }

    # compliance thinning (with optional fatigue decay by day)
    comp_d <- compliance * (1 - decay)^(d - 1)
    answered <- runif(length(xc)) < comp_d
    xc <- xc[answered]
    if (length(xc) == 0L) { rows[[d]] <- NULL; next }

    rows[[d]] <- data.frame(day = d, x = xc)
  }
  occ <- do.call(rbind, rows)
  if (is.null(occ)) occ <- data.frame(day = integer(0), x = numeric(0))
  occ
}

# --- Outcome model -----------------------------------------------------------
# Dynamic multilevel AR(1), lagged-outcome parameterisation (matches Lafit AR):
#   y_it = beta0 + b0_i + (beta1 + b1_i) * x_it + phi * y_i,(t-1) + e_it
# AR is over consecutive REALISED occasions within person (occasion-index lag,
# as in PowerAnalysisIL; irregular-spacing caveat noted in the prereg/limitations).
#
# Effect sizes are in outcome-SD units given the variance components in `pars`.
generate_dataset <- function(N, D, pars) {
  spec <- model_spec(pars$model)      # absent model => Model 3, the paper's model

  # person random effects: (b0_i, b1_i) ~ N(0, Sigma_b). The slope column is used
  # only when the model has a random slope, but is always drawn so the default
  # model's RNG stream is byte-identical to before this generalisation.
  Sigma_b <- matrix(c(pars$sd_b0^2,
                      pars$rho_b * pars$sd_b0 * pars$sd_b1,
                      pars$rho_b * pars$sd_b0 * pars$sd_b1,
                      pars$sd_b1^2), 2, 2)
  b <- MASS::mvrnorm(N, mu = c(0, 0), Sigma = Sigma_b)
  if (N == 1L) b <- matrix(b, nrow = 1)

  # Level-2 (person-level) predictor W. Only drawn when the model has one, so
  # models without it — including the default Model 3 — draw no extra randomness
  # here and keep an identical stream. Dummy = balanced groups (no RNG);
  # continuous = standardised, grand-mean 0.
  W <- rep(0, N)
  if (spec$l2 == "dummy")     W <- rep(c(0, 1), length.out = N)
  else if (spec$l2 == "cont") W <- rnorm(N)

  # Default to the Poisson (triggered) design, so a cell without a trigger_mode
  # column behaves exactly as it did before that option existed.
  mode    <- if (is.null(pars$trigger_mode)) "poisson" else as.character(pars$trigger_mode)
  fixed   <- identical(mode, "fixed")
  kappa   <- kappa_from_cv(pars$cv)
  lam_scl <- if (is.finite(kappa)) pars$lambda_bar / kappa else NA_real_

  b_l2    <- if (is.null(pars$beta_l2))    0 else pars$beta_l2     # L2 main effect (beta01)
  b_cross <- if (is.null(pars$beta_cross)) 0 else pars$beta_cross  # cross-level interaction (beta11)

  out <- vector("list", N)
  for (i in seq_len(N)) {
    # A planned schedule hands everyone the same number of prompts by
    # construction, so between-person rate variation (cv) does not apply in
    # fixed mode and is ignored there rather than quietly reinterpreted.
    lambda_i <- if (fixed || !is.finite(kappa)) pars$lambda_bar
                else rgamma(1L, shape = kappa, scale = lam_scl)
    occ <- draw_person_occasions(D, lambda_i, pars$cap, pars$compliance,
                                 pars$decay, pars$trigger_link, pars$mu_x,
                                 trigger_mode = mode)
    n_i <- nrow(occ)
    if (n_i == 0L) { out[[i]] <- NULL; next }

    intercept_i <- pars$beta0 + b_l2 * W[i] + b[i, 1]
    # coefficient on the Level-1 predictor: main effect + cross-level moderation
    # + (random slope, when the model has one).
    slope_i     <- pars$beta1 + b_cross * W[i] + (if (spec$slope) b[i, 2] else 0)

    y <- numeric(n_i)
    if (spec$l1 == "lag") {
      # Autoregressive models (9-11): the lagged outcome IS the predictor, and
      # slope_i is this person's AR coefficient. Clamp into the stationary region
      # (as Lafit draw bounded random AR slopes) so the series cannot explode.
      ar_i <- max(min(slope_i, 0.95), -0.95)
      y_prev <- intercept_i / (1 - ar_i) + rnorm(1, 0, pars$sd_e / sqrt(max(1e-6, 1 - ar_i^2)))
      for (t in seq_len(n_i)) {
        y[t]   <- intercept_i + ar_i * y_prev + rnorm(1, 0, pars$sd_e)
        y_prev <- y[t]
      }
    } else {
      # Models 1-8: temporal dependence enters as a lagged-outcome control (phi);
      # the analysis predictor is x (l1 = "x") or nothing (l1 = "none").
      y_prev <- intercept_i + rnorm(1, 0, pars$sd_e / sqrt(max(1e-6, 1 - pars$phi^2)))
      for (t in seq_len(n_i)) {
        mu_it <- intercept_i +
                 (if (spec$l1 == "x") slope_i * occ$x[t] else 0) +
                 pars$phi * y_prev
        y[t]  <- mu_it + rnorm(1, 0, pars$sd_e)
        y_prev <- y[t]
      }
    }
    out[[i]] <- data.frame(id = i, occ = seq_len(n_i), day = occ$day,
                           x = occ$x, y = y, W = W[i])
  }
  df <- do.call(rbind, out)
  if (is.null(df)) df <- data.frame(id = integer(0), occ = integer(0),
                                    day = integer(0), x = numeric(0),
                                    y = numeric(0), W = numeric(0))
  df
}

# Default variance components. These are documented STARTING VALUES, not
# authoritative estimates: set them from your own pilot data, or from published
# ESM studies in your domain, before relying on any power figure they produce.
default_pars <- function() {
  list(
    beta0 = 0, beta1 = 0.2,
    sd_b0 = 1.0, sd_b1 = 0.3, rho_b = 0,   # random intercept / slope SDs, correlation
    sd_e  = 1.0,                            # residual SD  (=> beta1 in ~SD units)
    phi   = 0.3,                            # AR(1) coefficient
    lambda_bar = 2, cv = 0.3,               # trigger rate mean & between-person CV
    cap = Inf, compliance = 0.75, decay = 0,
    trigger_link = 0, mu_x = 0,
    # "poisson" = event/location-contingent, the random-count case the paper is
    # about. "fixed" = time-contingent schedule: lambda_bar is then the PLANNED
    # prompts per day and cv / trigger_link do not apply.
    trigger_mode = "poisson",
    # multilevel model (see models.R). 3 = the paper's within-person effect.
    model = 3L,
    beta_l2    = 0.3,   # Level-2 main effect (beta01), used by Models 1,2,5-8,10,11
    beta_cross = 0.1    # cross-level interaction (beta11), the target of Models 5-8,10,11
  )
}
