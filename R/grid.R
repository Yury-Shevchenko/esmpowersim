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
