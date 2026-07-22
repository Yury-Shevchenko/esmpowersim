# =============================================================================
# lookup.R — instant answers from the precomputed confirmatory grid.
#
# The grid (results-confirmatory/{primary,secondary}.csv, 504 cells) is not a
# degraded stand-in for a live run: at R = 2000 an exact hit carries MCSE <= ~1.6
# points, where a live R = 100 run carries ~5. It is *more* precise AND instant.
# So the planner answers from the grid by default and only simulates off-grid.
#
# The grid's shape dictates what can be answered (verified against the CSVs):
#   * primary   = full factorial N x lambda_bar x cv x D, all six other levers at
#                 REF. 144 cells, R = 2000.
#   * secondary = five slabs, each moving exactly ONE lever off REF, over a
#                 coarser core. 360 cells, R = 1000.
#   * No cell anywhere has TWO levers off REF simultaneously.
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
                 "cap", "compliance", "decay", "phi", "beta1", "trigger_link")
# the six the primary grid holds at a reference value
REF_KEYS <- c("cap", "compliance", "decay", "phi", "beta1", "trigger_link")

# p90 of |interpolation error| on power_all along N, from the LOO study above.
# Regenerate with tools/loo.R if the grid is ever re-run.
LOO_P90_N <- 0.0216

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
  g <- do.call(rbind, lapply(paths, function(p) read.csv(p, stringsAsFactors = FALSE)))
  for (k in DESIGN_KEYS) g[[k]] <- as.numeric(g[[k]])   # the literal "Inf" cap parses here
  if (anyNA(g[DESIGN_KEYS])) stop("lookup: NA in a design column after parsing")

  # REF = whatever the primary slab holds constant.
  p <- g[g$grid == "primary", ]
  if (!nrow(p)) stop("lookup: no primary rows")
  ref <- lapply(REF_KEYS, function(k) {
    u <- unique(p[[k]])
    if (length(u) != 1) stop("lookup: primary grid is not constant in ", k)
    u
  })
  names(ref) <- REF_KEYS

  # Invariant the answer policy depends on: never two levers off REF at once.
  n_off <- vapply(seq_len(nrow(g)), function(i)
    sum(!vapply(REF_KEYS, function(k) near(g[[k]][i], ref[[k]]), logical(1))), integer(1))
  if (any(n_off > 1)) stop("lookup: grid invariant broken — ", sum(n_off > 1),
                           " cell(s) have >1 lever off REF; the answer policy assumes none")

  # power is conditional on convergence (mcse_power is on R_converged);
  # power_all counts non-convergence as non-detection, so its MCSE is on R_total.
  # Precompute it so nothing downstream can reach for the wrong error bar.
  g$mcse_power_all <- sqrt(g$power_all * (1 - g$power_all) / g$R_total)
  if (any(is.na(g$power) & g$R_converged > 0)) stop("lookup: NA power with converged reps")

  g$.n_off <- n_off
  structure(list(cells = g, ref = ref), class = "esm_grid")
}

# which levers does this cell move off REF?
off_ref <- function(cell, ref) {
  REF_KEYS[!vapply(REF_KEYS, function(k) near(cell[[k]], ref[[k]]), logical(1))]
}

# ---------------------------------------------------------------------------
# The columns run_cell() produces, so the UI can stay agnostic about provenance.
RUN_CELL_COLS <- c(DESIGN_KEYS,
                   "power", "mcse_power", "power_all", "type_s", "type_m",
                   "bias", "rel_bias", "emp_se", "rmse", "coverage", "ci_width",
                   "conv_rate", "R_converged", "R_total", "mean_n", "sd_n", "below_k")

unsupported <- function(cell, reason) {
  out <- as.data.frame(cell[DESIGN_KEYS])
  for (k in setdiff(RUN_CELL_COLS, DESIGN_KEYS)) out[[k]] <- NA_real_
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
  g <- G$cells; ref <- G$ref

  # The grid covers ONE model: the paper's within-person effect (Model 3). Any
  # other of the eleven models is a different fitted model with a different
  # effect of interest, never simulated here, so it can only be answered live.
  mdl <- if (is.null(cell$model)) 3L else as.integer(cell$model)
  if (mdl != 3L)
    return(unsupported(cell, paste0(
      "The precomputed grid covers only the within-person effect (Model 3). ",
      "This model is answered by live simulation.")))

  # The grid simulates event-/location-triggered designs only: every cell draws
  # its prompt count from a Poisson process. A fixed schedule has a PLANNED
  # count, which is a different data-generating mechanism — not a corner of this
  # grid — so there is nothing here to look up or interpolate toward.
  tm <- if (is.null(cell$trigger_mode)) "poisson" else as.character(cell$trigger_mode)
  if (!identical(tm, "poisson"))
    return(unsupported(cell, paste0(
      "The simulated grid covers event- and location-triggered designs, where the ",
      "prompt count is random. A fixed schedule plans the count instead, which the ",
      "grid never simulated. Run a live simulation for this design.")))

  off <- off_ref(cell, ref)

  # --- answer policy -------------------------------------------------------
  if (length(off) > 1)
    return(unsupported(cell, paste0(
      "The grid has no cell with two levers off reference at once (here: ",
      paste(off, collapse = ", "), "). Answering would assume the levers act ",
      "separably — an assumption this study never tested. Run a live simulation.")))

  slab <- if (length(off) == 0) g[g$grid == "primary", ]
          else g[g$.n_off == 1 & !near(g[[off]], ref[[off]]), ]
  if (!nrow(slab)) return(unsupported(cell, paste0("No slab varies ", off, ".")))

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
  se <- LOO_P90_N * 4 * w * (1 - w)

  finish(out, "grid_interp", "N", 2L, se,
         paste(rownames(lo), rownames(hi), sep = "+"))
}
