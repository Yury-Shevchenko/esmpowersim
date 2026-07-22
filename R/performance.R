# =============================================================================
# performance.R — Run R replications of one design cell and compute the
#                 preregistered performance measures, each with Monte Carlo SE.
#                 Prereg §3-P.
# =============================================================================

# Run one full replication: generate -> fit -> also record the person-level
# effective-n distribution summaries (computed on the SAME generated data).
run_replication <- function(N, D, pars, master_seed, cell_id, rep) {
  set.seed(rep_seed(master_seed, cell_id, rep))
  df  <- generate_dataset(N, D, pars)
  # effective-n distribution BEFORE lag-dropping (realised answered occasions)
  n_i <- if (nrow(df) == 0L) integer(0) else as.integer(table(factor(df$id, levels = seq_len(N))))
  fitres <- fit_one(df, model = pars$model)
  cbind(fitres,
        data.frame(mean_n   = if (length(n_i)) mean(n_i) else 0,
                   sd_n     = if (length(n_i)) sd(n_i)   else 0,
                   below_k  = if (length(n_i)) mean(n_i < 10) else 1,  # lower-tail mass, k=10
                   total_n  = if (length(n_i)) sum(n_i)  else 0))
}

# Aggregate one cell's replications into the metric row.
# beta1 = the true effect (for bias / Type-S / Type-M / coverage).
summarise_cell <- function(reps_df, beta1) {
  conv <- reps_df[reps_df$converged, , drop = FALSE]
  Rc   <- nrow(conv)
  sig  <- conv[!is.na(conv$p) & conv$p < 0.05, , drop = FALSE]
  sig_correct <- sig[sign(sig$est) == sign(beta1), , drop = FALSE]

  power    <- if (Rc > 0) nrow(sig_correct) / Rc else NA_real_
  mcse_pow <- if (Rc > 0) sqrt(power * (1 - power) / Rc) else NA_real_

  type_s <- if (nrow(sig) > 0) mean(sign(sig$est) != sign(beta1)) else NA_real_
  type_m <- if (nrow(sig_correct) > 0) mean(abs(sig_correct$est) / abs(beta1)) else NA_real_

  bias   <- if (Rc > 0) mean(conv$est) - beta1 else NA_real_
  relbias<- if (Rc > 0 && beta1 != 0) bias / beta1 else NA_real_
  emp_se <- if (Rc > 1) sd(conv$est) else NA_real_
  rmse   <- if (Rc > 0) sqrt(mean((conv$est - beta1)^2)) else NA_real_
  cover  <- if (Rc > 0) mean(conv$ci_lo <= beta1 & beta1 <= conv$ci_hi) else NA_real_
  ci_w   <- if (Rc > 0) mean(conv$ci_hi - conv$ci_lo) else NA_real_

  # power counting non-convergence as non-detection (pre-specified sensitivity)
  R_all      <- nrow(reps_df)
  power_all  <- if (R_all > 0) nrow(sig_correct) / R_all else NA_real_

  data.frame(
    power = power, mcse_power = mcse_pow, power_all = power_all,
    type_s = type_s, type_m = type_m,
    bias = bias, rel_bias = relbias, emp_se = emp_se, rmse = rmse,
    coverage = cover, ci_width = ci_w,
    conv_rate = if (R_all > 0) Rc / R_all else NA_real_, R_converged = Rc, R_total = R_all,
    mean_n = mean(reps_df$mean_n), sd_n = mean(reps_df$sd_n),
    below_k = mean(reps_df$below_k)
  )
}

# Full cell: run R reps, then summarise.
run_cell <- function(cell, master_seed, R, cell_id) {
  # A cell without a trigger_mode column is a Poisson (triggered) design, so
  # older grids and callers keep working untouched. Resolved before modifyList
  # because modifyList(x, list(k = NULL)) DELETES k rather than defaulting it.
  tm  <- if (is.null(cell$trigger_mode)) "poisson" else as.character(cell$trigger_mode)
  mdl <- if (is.null(cell$model)) 3L else as.integer(cell$model)
  extra <- list(model = mdl, trigger_mode = tm)
  # carry the L2 / cross-level effect sizes when the cell supplies them
  if (!is.null(cell$beta_l2))    extra$beta_l2    <- cell$beta_l2
  if (!is.null(cell$beta_cross)) extra$beta_cross <- cell$beta_cross
  pars <- modifyList(default_pars(), c(list(
    beta1 = cell$beta1, phi = cell$phi,
    lambda_bar = cell$lambda_bar, cv = cell$cv,
    cap = cell$cap, compliance = cell$compliance, decay = cell$decay,
    trigger_link = cell$trigger_link
  ), extra))

  # The "true effect" for bias / coverage / Type-S / Type-M is the TARGET
  # coefficient's value, which differs by model: the within-person / AR effect
  # (beta1), the L2 main effect (beta_l2), or the cross-level interaction
  # (beta_cross).
  spec <- model_spec(mdl)
  target_true <- switch(spec$target,
    b10 = pars$beta1, b01 = pars$beta_l2, b11 = pars$beta_cross)

  reps <- lapply(seq_len(R), function(r)
    run_replication(cell$N, cell$D, pars, master_seed, cell_id, r))
  reps_df <- do.call(rbind, reps)
  cbind(cell, summarise_cell(reps_df, beta1 = target_true))
}
