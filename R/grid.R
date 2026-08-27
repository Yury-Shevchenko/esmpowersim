# =============================================================================
# grid.R — Preregistered design grids (prereg §4).
#
# reference values held constant in the PRIMARY grid:
#   cap = none, compliance = .75 (constant), decay = 0, phi = .3,
#   beta1 = 0.3, trigger_link = 0 (exogenous).
# Secondary grids each add ONE factor, crossed with the primary levers.
# A tiny `smoke` grid is provided for fast end-to-end checks.
# =============================================================================

# beta1 reference = 0.2: a smoke run showed beta1=0.3 saturates power (>.95 at
# N>=30) with the reference variance components, leaving no headroom to see the
# CV effect. 0.2 keeps the low corners of the grid sub-ceiling where CV bites.
REF <- list(cap = Inf, compliance = 0.75, decay = 0, phi = 0.3,
            beta1 = 0.2, trigger_link = 0)

.expand <- function(...) {
  g <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  g
}

# --- Primary (confirmatory): N x lambda_bar x cv x D = 6*3*2*4 = 144 cells ----
primary_grid <- function() {
  g <- .expand(N = c(20, 40, 60, 90, 120, 150),
               lambda_bar = c(1, 2, 4),
               cv = c(0.3, 0.9),
               D = c(7, 14, 21, 28))
  g$cap <- REF$cap; g$compliance <- REF$compliance; g$decay <- REF$decay
  g$phi <- REF$phi; g$beta1 <- REF$beta1; g$trigger_link <- REF$trigger_link
  g$grid <- "primary"
  g
}

# --- Secondary grids: extend the core one factor at a time -------------------
# Core lever set kept smaller here to bound compute; widen it before a re-run if
# you need finer resolution.
core <- function() .expand(N = c(40, 90, 150), lambda_bar = c(1, 2, 4),
                           cv = c(0.3, 0.9), D = c(14, 28))

secondary_grids <- function() {
  # S1 — per-day cap
  s1 <- do.call(rbind, lapply(c(5, 3), function(cp) { g <- core(); g$cap <- cp; g }))
  s1$compliance <- REF$compliance; s1$decay <- REF$decay; s1$phi <- REF$phi
  s1$beta1 <- REF$beta1; s1$trigger_link <- REF$trigger_link; s1$grid <- "S1_cap"

  # S2 — compliance level (+ a decay variant at compliance .75)
  s2a <- do.call(rbind, lapply(c(0.90, 0.60), function(pc){ g<-core(); g$compliance<-pc; g$decay<-0; g }))
  s2b <- { g <- core(); g$compliance <- 0.75; g$decay <- 0.03; g }   # fatigue decay
  s2 <- rbind(s2a, s2b)
  s2$cap <- REF$cap; s2$phi <- REF$phi; s2$beta1 <- REF$beta1
  s2$trigger_link <- REF$trigger_link; s2$grid <- "S2_compliance"

  # S3 — AR coefficient
  s3 <- do.call(rbind, lapply(c(0, 0.5), function(ph){ g<-core(); g$phi<-ph; g }))
  s3$cap <- REF$cap; s3$compliance <- REF$compliance; s3$decay <- REF$decay
  s3$beta1 <- REF$beta1; s3$trigger_link <- REF$trigger_link; s3$grid <- "S3_phi"

  # S4 — effect size
  s4 <- do.call(rbind, lapply(c(0.1, 0.5), function(b){ g<-core(); g$beta1<-b; g }))
  s4$cap <- REF$cap; s4$compliance <- REF$compliance; s4$decay <- REF$decay
  s4$phi <- REF$phi; s4$trigger_link <- REF$trigger_link; s4$grid <- "S4_effect"

  # S5 — context-linked (endogenous) triggering
  s5 <- do.call(rbind, lapply(c(0.75), function(tl){ g<-core(); g$trigger_link<-tl; g }))
  s5$cap <- REF$cap; s5$compliance <- REF$compliance; s5$decay <- REF$decay
  s5$phi <- REF$phi; s5$beta1 <- REF$beta1; s5$grid <- "S5_context_linked"

  cols <- c("N","lambda_bar","cv","D","cap","compliance","decay","phi",
            "beta1","trigger_link","grid")
  do.call(rbind, lapply(list(s1, s2, s3, s4, s5), function(g) g[, cols]))
}

# =============================================================================
# TOOL-SUPPORT GRIDS (not part of the paper's protocol)
#
# The grids above are the study's. The two below exist only so the planning tool
# can answer from a precomputed table instead of simulating in the researcher's
# browser at R = 100. They are NOT preregistered, are NOT analysed by analyze.R,
# and must never be reported as study results: they carry `grid` labels of their
# own ("model_<id>", "fixed_<id>") so a slab can never be mistaken for primary
# or S1-S5.
#
# Everything else about them is deliberately identical to the primary grid — the
# same lever set, the same levels, the same reference values — so the tool's
# answer policy (snap D/lambda/cv, interpolate N, refuse two-levers-off-REF)
# carries over unchanged.
# =============================================================================

# Reference effect per model, in outcome-SD units.
#
# Each model powers a DIFFERENT coefficient (see models.R): the within-person or
# AR slope (beta1), the Level-2 main effect (beta_l2), or the cross-level
# interaction (beta_cross). One shared number cannot serve all three.
#
# The value below is chosen by the same criterion that set beta1 = 0.2 for the
# primary grid — the grid must SPAN the 0.8 decision boundary, so that "how many
# participants do I need?" has an answer inside it. Measured at the weakest
# (N=20, rate=1, D=7) and strongest (N=150, rate=4, D=28) corners at R = 100:
#
#   model  coefficient          weak -> strong     note
#     1    beta_l2    = 0.6     0.15 -> 0.96       dummy; 0.3 gave 0.07 -> 0.38
#     2    beta_l2    = 0.3     0.12 -> 0.98
#     4    beta1      = 0.2     0.26 -> 1.00
#     5    beta_cross = 0.2     0.03 -> 0.95       dummy; 0.1 gave 0.03 -> 0.53
#     6    beta_cross = 0.1     0.04 -> 1.00
#     7    beta_cross = 0.1     0.05 -> 0.98
#     8    beta_cross = 0.1     0.07 -> 1.00
#     9    beta1      = 0.2     0.27 -> 1.00
#    10    beta_cross = 0.2     0.04 -> 0.99       dummy; 0.1 gave 0.02 -> 0.47
#    11    beta_cross = 0.1     0.08 -> 0.98
#
# The three that needed a larger value (1, 5, 10) are exactly the models whose
# Level-2 predictor is a balanced DUMMY carrying a random slope. A balanced dummy
# has SD(W) = 0.5, so the same coefficient is half the standardised effect of the
# N(0,1) continuous predictor its twin model uses — doubling it equates the two.
# (Model 6 is also a dummy but has a FIXED slope, whose precision gain already
# offsets the halving, so it spans the boundary at 0.1 and would ceiling at 0.2.)
MODEL_REF_EFFECT <- list(
  `1`  = list(beta_l2    = 0.6),
  `2`  = list(beta_l2    = 0.3),
  `3`  = list(beta1      = 0.2),   # the paper's model, for completeness
  `4`  = list(beta1      = 0.2),
  `5`  = list(beta_cross = 0.2),
  `6`  = list(beta_cross = 0.1),
  `7`  = list(beta_cross = 0.1),
  `8`  = list(beta_cross = 0.1),
  `9`  = list(beta1      = 0.2),
  `10` = list(beta_cross = 0.2),
  `11` = list(beta_cross = 0.1)
)

# Attach the effect columns for a model. beta1 always carries a value (it is a
# structural lever of the DGM, not only a target), so it stays at REF unless the
# model is powered on it; beta_l2 / beta_cross default to default_pars() and are
# set explicitly here so every shipped cell records the effect it was run at.
with_model_effects <- function(g, id) {
  eff <- MODEL_REF_EFFECT[[as.character(id)]]
  if (is.null(eff)) stop("no reference effect defined for model ", id)
  g$model      <- as.integer(id)
  g$beta1      <- if (!is.null(eff$beta1))      eff$beta1      else REF$beta1
  g$beta_l2    <- if (!is.null(eff$beta_l2))    eff$beta_l2    else 0.3
  g$beta_cross <- if (!is.null(eff$beta_cross)) eff$beta_cross else 0.1
  g
}

# --- Per-model grids: the primary lever set, for the ten models the paper's ---
# --- grid does not cover. Model 3 is excluded — `primary` already is it. ------
MODELS_TO_FILL <- c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)

models_grid <- function(ids = MODELS_TO_FILL) {
  do.call(rbind, lapply(ids, function(id) {
    g <- .expand(N = c(20, 40, 60, 90, 120, 150),
                 lambda_bar = c(1, 2, 4),
                 cv = c(0.3, 0.9),
                 D = c(7, 14, 21, 28))
    g$cap <- REF$cap; g$compliance <- REF$compliance; g$decay <- REF$decay
    g$phi <- REF$phi; g$trigger_link <- REF$trigger_link
    g$trigger_mode <- "poisson"
    g <- with_model_effects(g, id)
    g$grid <- paste0("model_", id)
    g
  }))
}

# --- Fixed (time-contingent) schedules, all eleven models --------------------
# A planned schedule is a different data-generating mechanism, not a corner of
# the triggered grid: the count is SET (k prompts/day for everyone) and thinned
# by compliance, so it is Binomial around a planned maximum rather than Poisson.
# Between-person rate variation therefore does not exist here — `cv` is inert and
# recorded as 0 rather than left at a triggered value it does not mean.
#
# The lever levels differ from the triggered grid on purpose. A fixed schedule
# delivers k*D prompts to everyone, so power saturates far earlier: measured at
# R = 100, N=150/D=28 reads 1.000 at every k from 1 to 8, while N=20/D=7 moves
# 0.07 -> 0.50 -> 0.70 across k = 1 -> 3 -> 5. The decision boundary therefore
# lives at SHORT durations, which is why D reaches down to 3 days (burst designs)
# rather than starting at 7.
#
# ROW ORDER IS FROZEN, and that is load-bearing rather than tidy. A cell's seed
# comes from (master_seed, cell_id, rep) where cell_id is its ROW INDEX in this
# grid (run.R, run_grid). Rebuilding the grid with the levels merged into one
# expand.grid would renumber every row, so the 1,320 cells already simulated
# would no longer be reproducible from their own grid definition — the CSVs and
# the code would silently disagree about which dataset any given number came
# from. So the original block is emitted first, byte-identical, and later levels
# are APPENDED. Rows 1-120 keep the indices they were run at; new levels take
# 121-240. Anything added in future must be appended for the same reason.
#
# The added levels exist so the planner can offer real SLIDERS instead of radio
# buttons: a slider needs evenly spaced stops, and the original levels
# (k = 1,2,3,5,8 and D = 3,7,14,28) have none. With k = 1..8 and D = 7,14,21,28
# the app can step exactly onto simulated cells, so every stop is an exact
# lookup — no snapping, which the measured gradients rule out here anyway
# (D 14 -> 28 moves power by 15.6 points at p90, far past what this tool is
# willing to guess across).
FIXED_N     <- c(20, 40, 60, 90, 120, 150)
FIXED_K_V1  <- c(1, 2, 3, 5, 8)      # as first run
FIXED_D_V1  <- c(3, 7, 14, 28)       # as first run
FIXED_K_NEW <- c(4, 6, 7)            # completes the integer rate slider 1-8
FIXED_D_NEW <- 21                    # completes the step-7 duration slider 7-28
# Third pass: durations dense where the power surface bends and sparse where it
# is flat, so D can be INTERPOLATED rather than snapped and the slider can read
# every day from 1 to 28. Measured on the pass-2 grid, interpolating D costs
# 27.8 points at p90 across an 11-day gap at short durations but only 2.3 across
# a 14-day gap at long ones — the curvature, and therefore the need for levels,
# is all at the short end. Cost runs the other way (a cell scales with D), so
# the levels that matter most are also the cheapest to buy.
FIXED_D_V3 <- c(1, 2, 4, 5, 6, 10, 17, 24)

fixed_grid <- function(ids = c(3, MODELS_TO_FILL)) {
  do.call(rbind, lapply(sort(ids), function(id) {
    g <- rbind(
      # rows 1-120 — the original block, order untouched
      .expand(N = FIXED_N, lambda_bar = FIXED_K_V1, D = FIXED_D_V1),
      # rows 121-150 — the new duration at the original rates
      .expand(N = FIXED_N, lambda_bar = FIXED_K_V1, D = FIXED_D_NEW),
      # rows 151-240 — the new rates at every duration
      .expand(N = FIXED_N, lambda_bar = FIXED_K_NEW, D = c(FIXED_D_V1, FIXED_D_NEW)),
      # rows 241-624 — the in-between durations, at every rate
      .expand(N = FIXED_N, lambda_bar = sort(c(FIXED_K_V1, FIXED_K_NEW)), D = FIXED_D_V3)
    )
    g$cv <- 0                                     # inert in fixed mode
    g$cap <- REF$cap; g$compliance <- REF$compliance; g$decay <- REF$decay
    g$phi <- REF$phi; g$trigger_link <- REF$trigger_link
    g$trigger_mode <- "fixed"
    g <- with_model_effects(g, id)
    g$grid <- paste0("fixed_", id)
    g
  }))
}

# Rows of fixed_grid() that were already simulated in the first run. The delta
# run starts after these.
FIXED_V1_ROWS <- length(FIXED_N) * length(FIXED_K_V1) * length(FIXED_D_V1)

# --- Smoke grid: 4 cells, for a fast end-to-end correctness check ------------
smoke_grid <- function() {
  g <- .expand(N = c(30, 120), lambda_bar = 2, cv = c(0.3, 0.9), D = 14)
  g$cap <- REF$cap; g$compliance <- REF$compliance; g$decay <- REF$decay
  g$phi <- REF$phi; g$beta1 <- REF$beta1; g$trigger_link <- REF$trigger_link
  g$grid <- "smoke"
  g
}

# --- Sub-ceiling confirmatory grid (PROSPECTIVE follow-up study) -------------
# Prospectively preregistered follow-up to the parent study (OSF osf.io/a5jdb).
# The parent's H2 omnibus test — "holding expected total observations constant,
# higher between-person CV in trigger rate lowers power" — was NOT supported,
# because it averaged over a full factorial that included many cells at the
# power ceiling, where a CV penalty is arithmetically impossible. Exploratory
# analysis located the penalty in the UNDER-powered ("sub-ceiling") regime
# (small effects, sparse sampling). This grid re-tests that effect there, on its
# own, with the decision criterion fixed in the registration BEFORE any run.
#
#   Factors (fully factorial, 4*2*2*2*3 = 96 cells):
#     N     {20, 40, 60, 90}          lambda_bar {1, 2}
#     D     {7, 14}                    beta1      {0.10, 0.15}  (small: sub-ceiling)
#     cv    {0.3, 0.6, 0.9}            (0.3 vs 0.9 = confirmatory contrast; 0.6 = dose-response)
#   Everything else at REF (cap=none, compliance=.75, decay=0, phi=.3, exogenous).
#   Intended invocation:  --grid=subceiling --R=2000 --seed=20260722
#
# NB: beta1 is a VARIED factor here, so it is set per cell (not taken from REF).
# DO NOT RUN before the follow-up registration is posted — the whole point is a
# verifiable prospective timestamp.
subceiling_grid <- function() {
  g <- .expand(N = c(20, 40, 60, 90),
               lambda_bar = c(1, 2),
               cv = c(0.3, 0.6, 0.9),
               D = c(7, 14),
               beta1 = c(0.10, 0.15))
  g$cap <- REF$cap; g$compliance <- REF$compliance; g$decay <- REF$decay
  g$phi <- REF$phi; g$trigger_link <- REF$trigger_link
  g$grid <- "subceiling"
  g
}
