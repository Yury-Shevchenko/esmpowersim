# =============================================================================
# models.R — the model registry: the 11 multilevel models of Lafit et al. (2021).
#
# Each model asks a different research question, so the "effect of interest"
# differs. We describe every model as a combination of four structural choices,
# from which both the data-generating mechanism (dgm.R) and the fitted formula
# (fit.R) are derived — so there is one definition, not eleven parallel code
# paths.
#
#   l1     the momentary (Level-1) predictor:
#            "none" intercept-only (Models 1-2)
#            "x"    a momentary continuous predictor (Models 3-8)
#            "lag"  the lagged outcome itself is the predictor (Models 9-11)
#   l2     the person-level (Level-2) predictor:
#            "none" (Models 3,4,9) · "dummy" a group (1,5,6,10) · "cont" (2,7,8,11)
#   slope  is the Level-1 effect random across people? (only when l1 != none)
#   target which coefficient the power question is about:
#            "b01" L2 main effect · "b10" L1 main effect · "b11" cross-level interaction
#
# TEMPORAL DEPENDENCE — a documented divergence from Lafit worth understanding.
# Lafit's Models 1-8 capture temporal dependence with AR(1) *errors* (their rho),
# which lme4 cannot fit; their Models 9-11 use a lagged *outcome*. This tool uses
# a lagged outcome THROUGHOUT (as the paper does), so it matches Models 9-11
# exactly and, for Models 1-8, tests the identical effect but carries the AR as a
# lagged-outcome control rather than as correlated errors. Set the AR coefficient
# to 0 for the independent-errors case. This keeps one fit engine (lme4), keeps
# Model 3 identical to the paper's grid, and answers all eleven questions.
# =============================================================================

MODELS <- list(
  `1`  = list(name = "Group difference in mean level",              l1 = "none", l2 = "dummy", slope = FALSE, target = "b01"),
  `2`  = list(name = "Effect of a person-level continuous predictor", l1 = "none", l2 = "cont",  slope = FALSE, target = "b01"),
  `3`  = list(name = "Within-person effect (random slope)",         l1 = "x",    l2 = "none", slope = TRUE,  target = "b10"),
  `4`  = list(name = "Within-person effect (fixed slope)",          l1 = "x",    l2 = "none", slope = FALSE, target = "b10"),
  `5`  = list(name = "Group difference in the within-person effect", l1 = "x",   l2 = "dummy", slope = TRUE,  target = "b11"),
  `6`  = list(name = "Group difference in the within-person effect (fixed slope)", l1 = "x", l2 = "dummy", slope = FALSE, target = "b11"),
  `7`  = list(name = "Moderation of the within-person effect by a continuous predictor", l1 = "x", l2 = "cont", slope = TRUE, target = "b11"),
  `8`  = list(name = "Moderation of the within-person effect (fixed slope)", l1 = "x", l2 = "cont", slope = FALSE, target = "b11"),
  `9`  = list(name = "Autoregressive effect (random slope)",        l1 = "lag",  l2 = "none", slope = TRUE,  target = "b10"),
  `10` = list(name = "Group difference in the autoregressive effect", l1 = "lag", l2 = "dummy", slope = TRUE, target = "b11"),
  `11` = list(name = "Moderation of the autoregressive effect by a continuous predictor", l1 = "lag", l2 = "cont", slope = TRUE, target = "b11")
)

# Plain-language framing for the UI: the research QUESTION each model answers, a
# concrete ESM example, and the broad category it belongs to. Kept separate from
# the structural registry above so the user-facing prose lives in one place.
# `category`: within | l2 | moderation | ar  (drives the question-first chooser).
MODEL_INFO <- list(
  `1`  = list(category = "l2",
    question = "Do two groups differ in their average level of the outcome?",
    example  = "e.g. do people diagnosed with depression report higher average negative affect over a week than controls?"),
  `2`  = list(category = "l2",
    question = "Does a person-level characteristic relate to the average level of the outcome?",
    example  = "e.g. do people with higher baseline depression scores report higher average negative affect?"),
  `3`  = list(category = "within",
    question = "Within a person, does a momentary predictor track the momentary outcome — allowing the strength to differ between people?",
    example  = "e.g. within a person, does momentary stress predict momentary negative affect, and does that link vary across people?"),
  `4`  = list(category = "within",
    question = "Within a person, does a momentary predictor track the momentary outcome — assuming one common strength?",
    example  = "e.g. within a person, does momentary stress predict momentary negative affect (same strength for everyone)?"),
  `5`  = list(category = "moderation",
    question = "Does the within-person effect differ between two groups? (effect can vary between people)",
    example  = "e.g. is the momentary stress → affect link stronger in depression than in controls?"),
  `6`  = list(category = "moderation",
    question = "Does the within-person effect differ between two groups? (one common effect within each group)",
    example  = "e.g. is the momentary stress → affect link stronger in depression than in controls (fixed slope)?"),
  `7`  = list(category = "moderation",
    question = "Does a person-level characteristic strengthen or weaken the within-person effect? (effect can vary between people)",
    example  = "e.g. does baseline depression severity strengthen the momentary stress → affect link?"),
  `8`  = list(category = "moderation",
    question = "Does a person-level characteristic strengthen or weaken the within-person effect? (one common effect)",
    example  = "e.g. does baseline depression severity strengthen the momentary stress → affect link (fixed slope)?"),
  `9`  = list(category = "ar",
    question = "How much does the outcome carry over from one moment to the next, and does that carry-over differ between people?",
    example  = "e.g. how strong is emotional inertia — negative affect persisting moment to moment — and does it vary across people?"),
  `10` = list(category = "ar",
    question = "Does the moment-to-moment carry-over differ between two groups?",
    example  = "e.g. is emotional inertia stronger in depression than in controls?"),
  `11` = list(category = "ar",
    question = "Does a person-level characteristic predict stronger or weaker carry-over?",
    example  = "e.g. does baseline depression severity predict stronger emotional inertia?"))

model_info <- function(model = NULL) {
  id <- if (is.null(model) || is.na(model)) "3" else as.character(as.integer(model))
  MODEL_INFO[[id]]
}

# Model ids in each question category, in display order.
models_in_category <- function(cat)
  names(Filter(function(x) identical(x$category, cat), MODEL_INFO))

# Resolve the spec for a cell/pars. Absent model => 3 (the paper's model), so
# every existing grid, fixture, caller and the fixed-schedule mode keep working.
model_spec <- function(model = NULL) {
  id <- if (is.null(model) || is.na(model)) "3" else as.character(as.integer(model))
  if (is.null(MODELS[[id]])) stop("unknown model: ", id)
  MODELS[[id]]
}

# Human-readable label for the effect the power question is about.
target_label <- function(spec) switch(spec$target,
  b01 = if (spec$l2 == "dummy") "group difference in the mean" else "person-level predictor effect",
  b10 = if (spec$l1 == "lag") "autoregressive effect" else "within-person effect",
  b11 = "cross-level interaction")

# The name lme4 gives the target coefficient in fixef(), given the spec.
target_coef <- function(spec) {
  p <- if (spec$l1 == "lag") "y_lag" else "x"
  switch(spec$target, b01 = "W", b10 = p, b11 = paste0(p, ":W"))
}
