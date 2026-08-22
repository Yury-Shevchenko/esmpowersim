# =============================================================================
# lookup.R — instant answers from the precomputed confirmatory grid.
#
# The grid (results-confirmatory/{primary,secondary}.csv, 504 cells) is not a
# degraded stand-in for a live run: at R = 2000 an exact hit carries MCSE <= ~1.6
# points, where a live R = 100 run carries ~5. It is *more* precise AND instant.
# So the planner answers from the grid by default and only simulates off-grid.
#
# The grid's shape dictates what can be answered (verified against the CSVs).
# It is PARTITIONED by (model, trigger_mode) — see PARTITION_KEYS below — and
# within each partition:
#   * a base slab = full factorial N x lambda_bar x cv x D, every other lever at
#                   that partition's REF.
#   * optional secondary slabs, each moving exactly ONE lever off REF.
#   * No cell anywhere has TWO levers off REF simultaneously.
#
# Partitions present:
#   (3, poisson)   the STUDY grid: primary (144 cells, R = 2000) + S1-S5 (360
#                  cells, R = 1000), one lever off REF each.
#   (m, poisson)   tool-support, m in {1,2,4..11}: model_<m>, 144 cells, R = 1000.
#   (m, fixed)     tool-support, all eleven models: fixed_<m>, 120 cells,
#                  R = 1000, over N x k x D (cv is inert on a planned schedule).
# Only the study partition has secondary slabs, so off-REF levers (cap,
# compliance, decay, phi, effect size, context-linking) are answerable for the
# paper's model alone; everything else refuses and falls through to live
# simulation, exactly as before.
#
# Interpolation policy is measured, not assumed (leave-one-out over primary,
# absolute error on power_all, in percentage points):
#       N       median 0.20   p90 2.16   max  5.11   -> interpolate
#       D       median 0.52   p90 5.56   max 13.98   -> snap
#       log(l)  median 0.53   p90 3.99   max 11.77   -> snap
# So: interpolate N only. N's errors are systematically negative (linear-on-logit
# undershoots the concave sparse regime), i.e. the bias is conservative.
# =============================================================================

# design levers, in the order run_cell() and the CSVs use
DESIGN_KEYS <- c("N", "lambda_bar", "cv", "D",
                 "cap", "compliance", "decay", "phi", "beta1", "trigger_link",
                 "beta_l2", "beta_cross")
# the levers a base slab holds at a reference value
REF_KEYS <- c("cap", "compliance", "decay", "phi", "beta1", "trigger_link",
              "beta_l2", "beta_cross")

# PARTITION KEYS. These are not design levers to be interpolated or snapped —
# they select WHICH TABLE applies. A different model is a different fitted model
# powering a different coefficient; a fixed schedule is a different
# data-generating mechanism. Neither is a corner of the other's grid, so there is
# nothing between them to interpolate toward. Each (model, trigger_mode) pair is
# looked up in its own slab family, with its own reference values.
PARTITION_KEYS <- c("model", "trigger_mode")

# The base slab of a partition: the full factorial over N x lambda_bar x cv x D
# with every REF key at reference. For the paper's model on a triggered schedule
# that is the study's `primary` grid; the tool-support slabs are named by
# convention in grid.R.
partition_key <- function(model, trigger_mode)
  paste0(as.integer(model), "|", as.character(trigger_mode))

base_slab_name <- function(model, trigger_mode) {
  if (identical(as.character(trigger_mode), "fixed")) return(paste0("fixed_", model))
  if (as.integer(model) == 3L) return("primary")
  paste0("model_", model)
}

# Which REF keys are actually LIVE for a model. The DGM ignores the rest, so
# comparing them would refuse a design over a parameter that provably cannot
# change its answer — e.g. Model 3 has no Level-2 predictor at all (W is
# identically 0), so beta_l2 and beta_cross cannot move a single simulated value.
#
# Reading the DGM (dgm.R, generate_dataset):
#   l1 == "none"  (Models 1-2)   x is never used -> beta1, beta_cross inert
#   l2 == "none"  (Models 3,4,9) W == 0          -> beta_l2, beta_cross inert
#   l1 == "lag"   (Models 9-11)  the lagged outcome IS the predictor and its
#                                coefficient is beta1, so the separate AR control
#                                phi is never applied -> phi inert
active_ref_keys <- function(model) {
  spec  <- model_spec(model)
  inert <- character(0)
  if (spec$l1 == "none") inert <- c(inert, "beta1", "beta_cross")
  if (spec$l2 == "none") inert <- c(inert, "beta_l2", "beta_cross")
  if (spec$l1 == "lag")  inert <- c(inert, "phi")
  setdiff(REF_KEYS, unique(inert))
}

# p90 of |interpolation error| on power_all along N, from the LOO study above,
# measured PER SLAB by tools/loo.R. Regenerate it whenever a slab is re-run.
#
# One shared constant would have been wrong: the paper's grid interpolates along
# N better than any other partition (2.16 points), while the moderation models
# reach 4.01 — so Model 3's number applied globally would understate the
# interpolation uncertainty of a cross-level interaction by about half.
LOO_P90_N_BY_SLAB <- list(
  `primary` = 0.0216,
  `model_1` = 0.0303, `model_2` = 0.0352, `model_4` = 0.0057, `model_5` = 0.0378,
  `model_6` = 0.0327, `model_7` = 0.0401, `model_8` = 0.0298, `model_9` = 0.0144,
  `model_10` = 0.0333, `model_11` = 0.0377,
  `fixed_1` = 0.0338, `fixed_2` = 0.0394, `fixed_3` = 0.0163, `fixed_4` = 0.0058,
  `fixed_5` = 0.0353, `fixed_6` = 0.0270, `fixed_7` = 0.0402, `fixed_8` = 0.0192,
  `fixed_9` = 0.0154, `fixed_10` = 0.0395, `fixed_11` = 0.0344
)
# The paper's value, kept as a named constant because the manuscript quotes it.
LOO_P90_N <- LOO_P90_N_BY_SLAB[["primary"]]

# An unmeasured slab falls back to the WORST measured value, never the paper's:
# the error of a partition nobody has run LOO on is unknown, and the safe reading
# of unknown is "as bad as the worst we have seen", not "as good as our best".
loo_p90_n <- function(slab) {
  v <- LOO_P90_N_BY_SLAB[[slab]]
  if (is.null(v)) max(unlist(LOO_P90_N_BY_SLAB)) else v
}

# Identical to analyze.R:28-31. Duplicated deliberately: analyze.R is not part of
# the shipped app (see tools/build-app.R), and this must not drift from it.
emp_logit <- function(power, R) {
  k <- round(power * R)
  log((k + 0.5) / (R - k + 0.5))
}

# Float-safe equality that survives cap = Inf (Inf - Inf is NaN, so `abs(a-b)`
# alone would silently return NA and poison every comparison downstream).
near <- function(a, b, tol = 1e-8) {
  a <- as.numeric(a); b <- as.numeric(b)
  same_inf <- is.infinite(a) & is.infinite(b) & sign(a) == sign(b)
  fin      <- is.finite(a) & is.finite(b)
  same_inf | (fin & abs(a - b) <= tol)
}

# ---------------------------------------------------------------------------
# Load + validate the grid. Everything — the reference levels, the simulated
# levels, the slab structure — is derived from the data rather than hardcoded,
# so widening or re-running the grid is a drop-in CSV swap with no code change.
load_grid <- function(paths) {
  g <- do.call(rbind, lapply(paths, function(p) {
    d <- read.csv(p, stringsAsFactors = FALSE)
    # Back-fill the columns that post-date the study grids, so the archived
    # primary/secondary CSVs keep loading untouched. Their partition is the
    # paper's: Model 3 on a triggered schedule. beta_l2 / beta_cross are inert
    # for Model 3 (it has no Level-2 predictor) and are recorded only so the
    # column exists — active_ref_keys() keeps them out of every comparison.
    if (is.null(d$model))        d$model        <- 3L
    if (is.null(d$trigger_mode)) d$trigger_mode <- "poisson"
    if (is.null(d$beta_l2))      d$beta_l2      <- 0.3
    if (is.null(d$beta_cross))   d$beta_cross   <- 0.1
    d
  }))
  for (k in DESIGN_KEYS) g[[k]] <- as.numeric(g[[k]])   # the literal "Inf" cap parses here
  g$model        <- as.integer(g$model)
  g$trigger_mode <- as.character(g$trigger_mode)
  if (anyNA(g[DESIGN_KEYS])) stop("lookup: NA in a design column after parsing")
  if (anyNA(g[PARTITION_KEYS])) stop("lookup: NA in a partition column after parsing")

  # Each (model, trigger_mode) partition carries its own reference values, taken
  # from its base slab. Different models are simulated at different reference
  # effects — a cross-level interaction and a within-person slope are not
  # comparable quantities — so one global REF would be wrong for most of them.
  parts <- unique(g[PARTITION_KEYS])
  refs  <- list()
  n_off <- integer(nrow(g))
  for (i in seq_len(nrow(parts))) {
    mdl <- parts$model[i]; tm <- parts$trigger_mode[i]
    key <- partition_key(mdl, tm)
    sel <- g$model == mdl & g$trigger_mode == tm
    base_name <- base_slab_name(mdl, tm)
    b <- g[sel & g$grid == base_name, ]
    if (!nrow(b)) stop("lookup: partition ", key, " has no base slab (", base_name, ")")
    live <- active_ref_keys(mdl)
    r <- lapply(REF_KEYS, function(k) {
      u <- unique(b[[k]])
      # Only the LIVE keys must be constant. An inert one is free to hold
      # whatever the run recorded, since it cannot move a simulated value.
      if (k %in% live && length(u) != 1)
        stop("lookup: base slab ", base_name, " is not constant in ", k)
      u[1]
    })
    names(r) <- REF_KEYS
    refs[[key]] <- r

    # Invariant the answer policy depends on: never two levers off REF at once.
    idx <- which(sel)
    n_off[idx] <- vapply(idx, function(j)
      sum(!vapply(live, function(k) near(g[[k]][j], r[[k]]), logical(1))), integer(1))
  }
  if (any(n_off > 1)) stop("lookup: grid invariant broken — ", sum(n_off > 1),
                           " cell(s) have >1 lever off REF; the answer policy assumes none")

  # power is conditional on convergence (mcse_power is on R_converged);
  # power_all counts non-convergence as non-detection, so its MCSE is on R_total.
  # Precompute it so nothing downstream can reach for the wrong error bar.
  g$mcse_power_all <- sqrt(g$power_all * (1 - g$power_all) / g$R_total)
  if (any(is.na(g$power) & g$R_converged > 0)) stop("lookup: NA power with converged reps")

  g$.n_off <- n_off
  structure(list(cells = g, ref = refs[[partition_key(3L, "poisson")]], refs = refs),
            class = "esm_grid")
}

# which levers does this cell move off REF? Only the keys that are live for this
# model are considered — see active_ref_keys().
off_ref <- function(cell, ref, keys = REF_KEYS) {
  # A caller that omits an effect column is treated as asking for the reference
  # value of it, not as a mismatch — older fixtures predate beta_l2/beta_cross.
  same <- vapply(keys, function(k) {
    v <- cell[[k]]
    if (is.null(v) || length(v) != 1L || is.na(v)) return(TRUE)
    near(v, ref[[k]])
  }, logical(1))
  keys[!same]
}

# ---------------------------------------------------------------------------
# The columns run_cell() produces, so the UI can stay agnostic about provenance.
RUN_CELL_COLS <- c(DESIGN_KEYS, PARTITION_KEYS,
                   "power", "mcse_power", "power_all", "type_s", "type_m",
                   "bias", "rel_bias", "emp_se", "rmse", "coverage", "ci_width",
                   "conv_rate", "R_converged", "R_total", "mean_n", "sd_n", "below_k")

unsupported <- function(cell, reason) {
  # Tolerate a caller that omits the newer columns: a refusal must never itself
  # throw, or the UI loses the reason it was refused.
  keep <- intersect(c(DESIGN_KEYS, PARTITION_KEYS), names(cell))
  out  <- as.data.frame(cell[keep], stringsAsFactors = FALSE)
  for (k in setdiff(RUN_CELL_COLS, keep)) out[[k]] <- NA_real_
  out$mcse_power_all <- NA_real_
  out$source <- "unsupported"; out$source_grid <- NA_character_
  out$source_rows <- NA_character_; out$interp_dims <- ""
  out$n_corners <- 0L; out$interp_se_power <- NA_real_; out$reason <- reason
  out
}

#' Answer a design cell from the grid, or say honestly that we cannot.
#'
#' Returns one row with every run_cell() column, plus provenance:
#'   source      grid_exact | grid_interp | unsupported
#'   source_grid which slab answered
#'   source_rows the CSV row indices used (this is a reproducibility tool —
#'               every number must trace back to a row)
#'   interp_se_power  interpolation uncertainty. NOT MCSE: MCSE shrinks with R,
#'               this is a property of the grid's resolution and never shrinks.
lookup_cell <- function(cell, G) {
  g <- G$cells

  # --- 1. select the partition ---------------------------------------------
  # A model and a schedule type are not levers to interpolate along: each names
  # a different table. Pick it, or say plainly that it was never simulated.
  mdl <- if (is.null(cell$model)) 3L else as.integer(cell$model)
  tm  <- if (is.null(cell$trigger_mode)) "poisson" else as.character(cell$trigger_mode)

  refs <- if (!is.null(G$refs)) G$refs else stats::setNames(list(G$ref),
                                                partition_key(3L, "poisson"))
  ref  <- refs[[partition_key(mdl, tm)]]
  g    <- g[g$model == mdl & g$trigger_mode == tm, ]
  if (is.null(ref) || !nrow(g))
    return(unsupported(cell, if (identical(tm, "fixed")) paste0(
      "A fixed schedule is not in the precomputed table for this model. A planned ",
      "count is a different data-generating mechanism, not a corner of the ",
      "triggered grid, so there is nothing here to interpolate toward. ",
      "Run a live simulation.") else paste0(
      "Model ", mdl, " is not in the precomputed table. It is a different fitted ",
      "model powering a different coefficient, so it is answered by live simulation.")))

  live <- active_ref_keys(mdl)
  off  <- off_ref(cell, ref, live)

  # --- 2. answer policy ----------------------------------------------------
  if (length(off) > 1)
    return(unsupported(cell, paste0(
      "The grid has no cell with two levers off reference at once (here: ",
      paste(off, collapse = ", "), "). Answering would assume the levers act ",
      "separably — an assumption this study never tested. Run a live simulation.")))

  slab <- if (length(off) == 0) g[g$grid == base_slab_name(mdl, tm), ]
          else g[g$.n_off == 1 & !near(g[[off]], ref[[off]]), ]
  if (!nrow(slab)) return(unsupported(cell, paste0(
    off, " = ", cell[[off]], " was never simulated for this model (the only ",
    "off-reference slabs are for the paper's model on a triggered schedule). ",
    "Run a live simulation.")))

  # a single off-REF lever must sit exactly on a simulated level
  if (length(off) == 1) {
    lv <- unique(slab[[off]])
    if (!any(near(cell[[off]], lv)))
      return(unsupported(cell, paste0(
        off, " = ", cell[[off]], " was never simulated (levels: ",
        paste(sort(lv), collapse = ", "), "). Run a live simulation.")))
    slab <- slab[near(slab[[off]], cell[[off]]), ]
  }

  # D, lambda_bar and cv are snapped, never interpolated (LOO: up to 14 / 12
  # points of error, and cv has only two levels — see header).
  for (k in c("D", "lambda_bar", "cv")) {
    lv <- unique(slab[[k]])
    if (!any(near(cell[[k]], lv)))
      return(unsupported(cell, paste0(
        k, " = ", cell[[k]], " is not a simulated level (", paste(sort(lv), collapse = ", "),
        "). Interpolating it is not accurate enough; pick a level or simulate.")))
    slab <- slab[near(slab[[k]], cell[[k]]), ]
  }
  if (!nrow(slab)) return(unsupported(cell, "No matching cells in the grid."))

  # --- N: exact hit, or interpolate between its two bracketing levels -------
  Ns <- sort(unique(slab$N))
  if (cell$N < min(Ns) || cell$N > max(Ns))
    return(unsupported(cell, paste0(
      "N = ", cell$N, " is outside the simulated range (", min(Ns), "-", max(Ns),
      "). Extrapolating is unsafe: the surface is strongly nonlinear at the edges.")))

  finish <- function(row, source, dims, corners, se, rows) {
    out <- row[intersect(c(RUN_CELL_COLS, "mcse_power_all"), names(row))]
    out$source <- source; out$source_grid <- row$grid[1]
    out$source_rows <- rows; out$interp_dims <- dims
    out$n_corners <- corners; out$interp_se_power <- se; out$reason <- NA_character_
    out
  }

  hit <- slab[near(slab$N, cell$N), ]
  if (nrow(hit) == 1)
    return(finish(hit, "grid_exact", "", 1L, 0, as.character(rownames(hit))))

  lo <- slab[slab$N == max(Ns[Ns < cell$N]), ]
  hi <- slab[slab$N == min(Ns[Ns > cell$N]), ]
  if (nrow(lo) != 1 || nrow(hi) != 1)
    return(unsupported(cell, "Bracketing cells missing — the grid is not complete here."))

  w <- (cell$N - lo$N) / (hi$N - lo$N)
  out <- lo
  out$N <- cell$N

  # interpolate on the empirical logit (bounded, saturating), on the right
  # denominator for each quantity
  lin_logit <- function(col, Rcol) {
    a <- emp_logit(lo[[col]], lo[[Rcol]]); b <- emp_logit(hi[[col]], hi[[Rcol]])
    plogis(a + w * (b - a))
  }
  out$power     <- lin_logit("power", "R_converged")
  out$power_all <- lin_logit("power_all", "R_total")
  out$conv_rate <- lin_logit("conv_rate", "R_total")
  out$coverage  <- lin_logit("coverage", "R_converged")
  out$below_k   <- lin_logit("below_k", "R_total")
  for (k in c("mean_n", "sd_n", "type_m"))                 # positive, so log-linear
    out[[k]] <- exp(log(lo[[k]]) + w * (log(hi[[k]]) - log(lo[[k]])))
  # precision diagnostics are not planning quantities — don't invent them
  for (k in c("type_s", "bias", "rel_bias", "emp_se", "rmse", "ci_width"))
    out[[k]] <- NA_real_

  out$R_converged <- NA_integer_; out$R_total <- lo$R_total
  out$mcse_power     <- sqrt(out$power * (1 - out$power) / mean(c(lo$R_converged, hi$R_converged)))
  out$mcse_power_all <- sqrt(out$power_all * (1 - out$power_all) / out$R_total)

  # Interpolation error is 0 at the ends and worst mid-bracket; 4w(1-w) is that
  # shape, scaled to the LOO p90. Reported separately from MCSE — they are
  # different kinds of uncertainty and must never be merged into one number.
  se <- loo_p90_n(as.character(lo$grid[1])) * 4 * w * (1 - w)

  finish(out, "grid_interp", "N", 2L, se,
         paste(rownames(lo), rownames(hi), sep = "+"))
}
