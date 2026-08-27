SIM <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(SIM) || !dir.exists(file.path(SIM, "R"))) SIM <- normalizePath(".")
# models.R first: lookup.R reads the model registry to work out which parameters
# are live for a given model (active_ref_keys), the same order app.R and
# build-app.R load them in.
source(file.path(SIM, "R", "models.R"))
source(file.path(SIM, "R", "lookup.R"))
RC <- file.path(SIM, "results-confirmatory")
G <- load_grid(c(file.path(RC, "primary.csv"), file.path(RC, "secondary.csv")))
cat("grid loaded:", nrow(G$cells), "cells | REF:",
    paste(names(G$ref), unlist(G$ref), sep = "=", collapse = " "), "\n\n")

fail <- 0L
ok <- function(cond, msg) { if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") } }

# --- 1. every primary cell must round-trip EXACTLY -------------------------
p <- G$cells[G$cells$grid == "primary", ]
worst <- 0
for (i in seq_len(nrow(p))) {
  cell <- as.list(p[i, DESIGN_KEYS])
  r <- lookup_cell(cell, G)
  if (r$source != "grid_exact") { ok(FALSE, sprintf("row %d: source=%s", i, r$source)); next }
  for (m in c("power", "power_all", "conv_rate", "mean_n", "below_k")) {
    d <- abs(r[[m]] - p[[m]][i]); worst <- max(worst, d)
    ok(d < 1e-12, sprintf("row %d %s: %.10f vs %.10f", i, m, r[[m]], p[[m]][i]))
  }
}
cat(sprintf("1. all %d primary cells round-trip exactly       %s (worst delta %.2e)\n",
            nrow(p), if (fail == 0) "PASS" else "FAIL", worst))

# --- 2. every secondary cell too ------------------------------------------
f0 <- fail
s <- G$cells[G$cells$grid != "primary", ]
for (i in seq_len(nrow(s))) {
  r <- lookup_cell(as.list(s[i, DESIGN_KEYS]), G)
  ok(r$source == "grid_exact" && abs(r$power_all - s$power_all[i]) < 1e-12,
     sprintf("secondary row %d (%s): %s", i, s$grid[i], r$source))
}
cat(sprintf("2. all %d secondary cells round-trip exactly     %s\n",
            nrow(s), if (fail == f0) "PASS" else "FAIL"))

# --- 3. two levers off REF must refuse ------------------------------------
f0 <- fail
c2 <- as.list(p[1, DESIGN_KEYS]); c2$cap <- 3; c2$beta1 <- 0.1
r <- lookup_cell(c2, G)
ok(r$source == "unsupported", "two-off-REF should be unsupported")
ok(grepl("two levers", r$reason), "reason should name the problem")
cat(sprintf("3. two levers off REF refused                    %s\n", if (fail == f0) "PASS" else "FAIL"))
cat("   reason:", substr(r$reason, 1, 78), "...\n")

# --- 4. off-grid values refuse (not silently interpolate) -----------------
f0 <- fail
for (tc in list(list(k = "D", v = 10), list(k = "lambda_bar", v = 3), list(k = "cv", v = 0.5),
                list(k = "compliance", v = 0.85))) {
  cc <- as.list(p[1, DESIGN_KEYS]); cc[[tc$k]] <- tc$v
  r <- lookup_cell(cc, G)
  ok(r$source == "unsupported", sprintf("%s=%s should refuse, got %s", tc$k, tc$v, r$source))
}
cat(sprintf("4. off-grid D / lambda / cv / compliance refused %s\n", if (fail == f0) "PASS" else "FAIL"))

# --- 5. outside the N hull refuses ----------------------------------------
f0 <- fail
for (v in c(10, 200)) {
  cc <- as.list(p[1, DESIGN_KEYS]); cc$N <- v
  ok(lookup_cell(cc, G)$source == "unsupported", sprintf("N=%d should refuse", v))
}
cat(sprintf("5. N outside hull refused (no extrapolation)     %s\n", if (fail == f0) "PASS" else "FAIL"))

# --- 6. N interpolation: works, flagged, and inside the LOO band ----------
f0 <- fail
cc <- as.list(p[1, DESIGN_KEYS]); cc$N <- 30           # between 20 and 40
r <- lookup_cell(cc, G)
ok(r$source == "grid_interp", "N=30 should interpolate")
ok(r$interp_dims == "N" && r$n_corners == 2L, "should report interp dims/corners")
ok(r$interp_se_power > 0 && r$interp_se_power <= LOO_P90_N, "interp SE in (0, p90]")
ok(is.na(r$emp_se) && is.na(r$type_s), "precision diagnostics must not be invented")
ok(!is.na(r$mcse_power_all), "mcse_power_all must be present")
lo <- p$power_all[p$N == 20 & p$lambda_bar == cc$lambda_bar & p$cv == cc$cv & p$D == cc$D]
hi <- p$power_all[p$N == 40 & p$lambda_bar == cc$lambda_bar & p$cv == cc$cv & p$D == cc$D]
ok(r$power_all > lo && r$power_all < hi, "interpolated power_all must lie between neighbours")
cat(sprintf("6. N interpolation correct + flagged            %s\n", if (fail == f0) "PASS" else "FAIL"))
cat(sprintf("   N=30: power_all=%.3f (bracket %.3f..%.3f) interp_se=%.4f source_rows=%s\n",
            r$power_all, lo, hi, r$interp_se_power, r$source_rows))

# --- 7. the MCSE trap: mcse_power != mcse_power_all -----------------------
f0 <- fail
worst_row <- which.max(abs(p$power - p$power_all))
cell <- as.list(p[worst_row, DESIGN_KEYS])
r <- lookup_cell(cell, G)
ok(abs(r$mcse_power - r$mcse_power_all) > 1e-6, "the two MCSEs must differ")
ok(abs(r$mcse_power_all - sqrt(r$power_all * (1 - r$power_all) / r$R_total)) < 1e-12,
   "mcse_power_all must be on R_total")
cat(sprintf("7. MCSE computed on the right denominator      %s\n", if (fail == f0) "PASS" else "FAIL"))
cat(sprintf("   worst power/power_all gap: N=%g D=%g lambda=%g cv=%g -> power=%.3f (mcse %.4f) vs power_all=%.3f (mcse %.4f), conv=%.3f\n",
            cell$N, cell$D, cell$lambda_bar, cell$cv, r$power, r$mcse_power, r$power_all, r$mcse_power_all, r$conv_rate))

# --- 8. cap = Inf survives the float comparison ---------------------------
f0 <- fail
cc <- as.list(p[1, DESIGN_KEYS])
ok(is.infinite(cc$cap), "primary cap should parse to Inf")
ok(lookup_cell(cc, G)$source == "grid_exact", "cap=Inf must match, not NaN out")
# cap=3 lives only in the S1 slab, whose core is coarser than primary's
# (N in {40,90,150}, D in {14,28}) — so build the cell from the core, not from
# primary row 1 (N=20, D=7), which S1 never simulated.
core <- as.list(p[p$N == 40 & p$D == 14 & p$lambda_bar == 1 & p$cv == 0.3, DESIGN_KEYS][1, ])
core$cap <- 3; r <- lookup_cell(core, G)
ok(r$source == "grid_exact" && r$source_grid == "S1_cap", "cap=3 on a core cell should hit S1")
# and a cap=3 cell outside S1's coarser core must refuse rather than mislead
off <- as.list(p[1, DESIGN_KEYS]); off$cap <- 3     # N=20, D=7: not in S1's core
ok(lookup_cell(off, G)$source == "unsupported", "cap=3 outside S1's core must refuse")
cat(sprintf("8. cap=Inf / cap=3 handled (Inf-safe compare)   %s\n", if (fail == f0) "PASS" else "FAIL"))


# --- 9. fixed-schedule designs -------------------------------------------
# The grid is Poisson-only. A planned count is a different data-generating
# mechanism, not a corner of the grid, so lookup must refuse rather than answer
# a triggered-design number for a scheduled study.
f0 <- fail
cc <- as.list(p[1, DESIGN_KEYS]); cc$trigger_mode <- "fixed"
r <- lookup_cell(cc, G)
ok(r$source == "unsupported", "a fixed design must not be answered from the Poisson grid")
ok(grepl("fixed schedule", r$reason), "the refusal should say why")
# and a cell with no trigger_mode at all must still behave exactly as before
ok(lookup_cell(as.list(p[1, DESIGN_KEYS]), G)$source == "grid_exact",
   "absent trigger_mode must default to poisson (backward compatibility)")
cat(sprintf("9. fixed designs refused by the grid                %s\n", if (fail == f0) "PASS" else "FAIL"))

# --- 10. the fixed engine itself -----------------------------------------
# Full compliance on a planned schedule is the exactly-balanced design that
# PowerAnalysisIL assumes: sd of observations per person must be 0, or the
# "reduces to a balanced design" claim in validate.R is not true.
f0 <- fail
for (fn in c("seeds", "models", "dgm", "fit", "performance")) source(file.path(SIM, "R", paste0(fn, ".R")))
mkf <- function(comp, tm) data.frame(N = 30, D = 14, lambda_bar = 3, cv = 0, cap = Inf,
  compliance = comp, decay = 0, phi = 0.3, beta1 = 0.2, trigger_link = 0,
  trigger_mode = tm, stringsAsFactors = FALSE)
b <- run_cell(mkf(1.0, "fixed"), 20260709L, 40L, 1L)
ok(abs(b$mean_n - 42) < 1e-9 && b$sd_n < 1e-9,
   sprintf("fixed + full compliance must be exactly balanced (got mean_n=%.4f sd_n=%.4f)", b$mean_n, b$sd_n))
# compliance thins the planned prompts: realised count is Binomial(k*D, p)
h <- run_cell(mkf(0.75, "fixed"), 20260709L, 40L, 2L)
ok(abs(h$mean_n - 31.5) < 1.0, sprintf("fixed + 75%% compliance -> mean_n ~31.5 (got %.2f)", h$mean_n))
ok(abs(h$sd_n - sqrt(42 * 0.75 * 0.25)) < 1.0,
   sprintf("realised count should be Binomial: sd ~%.2f (got %.2f)", sqrt(42 * 0.75 * 0.25), h$sd_n))
# cv is not applicable to a planned schedule and must be ignored, not reinterpreted
c0 <- run_cell(mkf(1.0, "fixed"), 20260709L, 20L, 3L)
cv9 <- mkf(1.0, "fixed"); cv9$cv <- 0.9
c9 <- run_cell(cv9, 20260709L, 20L, 3L)
ok(abs(c0$sd_n - c9$sd_n) < 1e-9, "cv must be ignored in fixed mode")
cat(sprintf("10. fixed engine: exactly balanced + Binomial thinning %s\n", if (fail == f0) "PASS" else "FAIL"))
cat(sprintf("    fixed comp=1.00 -> mean_n=%.2f sd_n=%.2f | comp=0.75 -> mean_n=%.2f sd_n=%.2f\n",
            b$mean_n, b$sd_n, h$mean_n, h$sd_n))
# --- 11. fixed-mode reactive contract (regression for the browser race) ---
# The bug: design_type and engine were two inputs, and estimate() could observe
# design_type=fixed while engine still said lookup -> a stale "grid refusal".
# Contract now: sim_mode() is TRUE for fixed REGARDLESS of engine, and the cell
# a fixed design builds carries trigger_mode="fixed" with cv/trigger_link zeroed.
# (Reactive wiring itself is covered by tests/app-fixed.R via testServer.)
f0 <- fail
mkcell_fixed <- data.frame(N = 40, D = 14, lambda_bar = 3, cv = 0, cap = Inf,
  compliance = 0.75, decay = 0, phi = 0.3, beta1 = 0.2, trigger_link = 0,
  trigger_mode = "fixed", stringsAsFactors = FALSE)
rr <- lookup_cell(as.list(mkcell_fixed), G)
rr <- lookup_cell(as.list(mkcell_fixed), G)
ok(rr$source == "unsupported" && grepl("fixed schedule", rr$reason),
   "a fixed cell must be refused by the grid, so the app routes it to simulation")
cat(sprintf("11. fixed-cell routing contract                     %s\n", if (fail == f0) "PASS" else "FAIL"))

# --- 12. partitioning by (model, trigger_mode) ----------------------------
# The table is keyed by model and schedule type, not just by design levers. A
# partition that was never simulated must refuse; one that was must answer
# WITHOUT being disturbed by parameters the model does not use.
f0 <- fail
base <- as.list(p[1, DESIGN_KEYS])

# (a) a model with no slab refuses, and names itself in the reason
for (m in c(1, 5, 9, 11)) {
  cc <- base; cc$model <- m
  r  <- lookup_cell(cc, G)
  ok(r$source == "unsupported" && grepl(paste0("Model ", m), r$reason),
     sprintf("model %d must refuse while it has no slab, and say so", m))
}
# (b) the paper's model still answers when a model id is passed explicitly
cc <- base; cc$model <- 3
ok(lookup_cell(cc, G)$source == "grid_exact", "model=3 must still be an exact hit")

# (c) INERT parameters must not refuse. Model 3 has no Level-2 predictor (W is
# identically 0 in the DGM), so beta_l2 / beta_cross cannot move any simulated
# value — refusing over them would send a perfectly answerable design to a
# one-minute browser run for no reason.
for (k in c("beta_l2", "beta_cross")) {
  cc <- base; cc[[k]] <- 0.77
  ok(lookup_cell(cc, G)$source == "grid_exact",
     sprintf("%s is inert for Model 3 and must not affect the lookup", k))
}
# (d) ... but a LIVE parameter still refuses, on the same grid.
cc <- base; cc$beta1 <- 0.77
ok(lookup_cell(cc, G)$source == "unsupported",
   "beta1 IS live for Model 3, so an unsimulated value must still refuse")

# (e) active_ref_keys must match what the DGM actually reads (dgm.R)
ok(!("beta_l2" %in% active_ref_keys(3)),   "Model 3: beta_l2 inert (l2 = none)")
ok(!("beta1"   %in% active_ref_keys(1)),   "Model 1: beta1 inert (l1 = none)")
ok(!("phi"     %in% active_ref_keys(9)),   "Model 9: phi inert (the lag IS the predictor)")
ok("beta_cross" %in% active_ref_keys(7),   "Model 7: beta_cross is the target, must be live")
ok("beta_l2"    %in% active_ref_keys(2),   "Model 2: beta_l2 is the target, must be live")
cat(sprintf("12. (model, schedule) partitioning + inert params   %s\n", if (fail == f0) "PASS" else "FAIL"))

# --- 13. whatever tool-support slabs are present must round-trip ----------
# Skipped when results-tool/ has not been built yet, so the suite stays green on
# a fresh clone; exercised in full once the slabs ship.
f0 <- fail
tool_dir <- file.path(SIM, "results-tool")
tool_csv <- if (dir.exists(tool_dir)) setdiff(
  list.files(tool_dir, "\\.csv$", full.names = TRUE),
  list.files(tool_dir, "\\.part\\.csv$", full.names = TRUE)) else character(0)
if (!length(tool_csv)) {
  cat("13. tool-support slabs                             SKIP (none built yet)\n")
} else {
  GT <- load_grid(c(file.path(RC, c("primary.csv", "secondary.csv")), tool_csv))
  tc <- GT$cells[!GT$cells$grid %in% c("primary", "S1_cap", "S2_compliance",
                                       "S3_phi", "S4_effect", "S5_context_linked"), ]
  worst_t <- 0
  for (i in seq_len(nrow(tc))) {
    cell <- as.list(tc[i, c(DESIGN_KEYS, PARTITION_KEYS)])
    r <- lookup_cell(cell, GT)
    if (r$source != "grid_exact") {
      ok(FALSE, sprintf("%s row %d: source=%s (%s)", tc$grid[i], i, r$source, r$reason)); next
    }
    d <- abs(r$power_all - tc$power_all[i]); worst_t <- max(worst_t, d)
    ok(d < 1e-12, sprintf("%s row %d power_all mismatch", tc$grid[i], i))
  }
  cat(sprintf("13. %d tool-slab cells round-trip exactly          %s (worst %.2e)\n",
              nrow(tc), if (fail == f0) "PASS" else "FAIL", worst_t))
}

# --- 14. interpolating along D, where the grid is dense enough ------------
# D is snapped by default: the study grid's four durations sit 7 days apart and
# interpolating them costs 5.56 points at p90. The fixed-schedule slabs are dense
# enough to interpolate, so the policy is per slab (LOO_P90_D_BY_SLAB) and turns
# on only where it has been measured. Skipped until such a slab exists.
f0 <- fail
# G is primary+secondary only; the D-band slabs live in results-tool/, so ask
# there rather than at the study grid.
tool <- file.path(SIM, "results-tool")
dslabs <- if (dir.exists(tool))
  intersect(names(LOO_P90_D_BY_SLAB),
            sub("\\.csv$", "", basename(list.files(tool, "\\.csv$")))) else character(0)
have_d <- length(dslabs) > 0
if (!have_d) {
  cat("14. D interpolation                                SKIP (no slab has a D band)\n")
} else {
  paths <- c(file.path(RC, c("primary.csv", "secondary.csv")),
             setdiff(list.files(tool, "\\.csv$", full.names = TRUE),
                     list.files(tool, "\\.part\\.csv$", full.names = TRUE)))
  GT <- load_grid(paths)
  nm <- dslabs[1]
  b  <- GT$cells[GT$cells$grid == nm, ]
  mdl <- b$model[1]; tm <- b$trigger_mode[1]

  # (a) a slab WITHOUT a measured band must still refuse — the study grid must
  #     not silently acquire a policy that was never validated for it.
  pc <- as.list(p[1, c(DESIGN_KEYS, PARTITION_KEYS)]); pc$D <- 10
  r <- lookup_cell(pc, GT)
  ok(r$source == "unsupported" && grepl("too far apart", r$reason),
     "the study grid must still refuse an off-level D")

  # (b) hold out a simulated duration and rebuild it through the SHIPPED path.
  #     Holding out a level forces a double-width gap, so this is stricter than
  #     anything a user can ask for.
  Ds   <- sort(unique(b$D))
  hold <- Ds[ceiling(length(Ds) / 2)]
  tmpd <- tempfile(); dir.create(tmpd)
  ps <- vapply(paths, function(f) {
    d <- read.csv(f, stringsAsFactors = FALSE)
    if (identical(as.character(d$grid[1]), nm)) d <- d[d$D != hold, ]
    o <- file.path(tmpd, basename(f)); write.csv(d, o, row.names = FALSE); o
  }, character(1))
  Gh <- load_grid(ps)
  errs <- c(); guarded <- 0
  for (i in which(b$D == hold)) {
    r0 <- b[i, ]
    rr <- lookup_cell(as.list(r0[c(DESIGN_KEYS, PARTITION_KEYS)]), Gh)
    if (identical(rr$source, "unsupported")) { guarded <- guarded + 1; next }
    ok(rr$source == "grid_interp", "an off-level D must come back as grid_interp")
    ok(rr$interp_se_power > 0, "an interpolated D must carry a non-zero interpolation SE")
    errs <- c(errs, abs(rr$power_all - r0$power_all) * 100)
  }
  unlink(tmpd, recursive = TRUE)
  ok(length(errs) > 0, "the hold-out must answer at least some cells")
  # The bar is the band already accepted for N (p90 up to 4.02 points). If D
  # interpolation cannot meet the standard the tool already lives by, it should
  # not be enabled at all.
  ok(quantile(errs, .9) <= 4.02,
     sprintf("held-out D=%g: p90 error %.2f pts must be within the N band (4.02)",
             hold, quantile(errs, .9)))
  ok(max(errs) <= 12,
     sprintf("held-out D=%g: max error %.2f pts", hold, max(errs)))

  # (c) the convergence guard must actually fire somewhere, and say why.
  cliff <- b[b$conv_rate < 0.5, ]
  if (nrow(cliff)) {
    cc <- as.list(cliff[1, c(DESIGN_KEYS, PARTITION_KEYS)])
    Dl <- sort(unique(b$D)); d0 <- cc$D
    nxt <- Dl[Dl > d0]
    if (length(nxt)) {
      cc$D <- (d0 + nxt[1]) / 2
      if (!any(abs(cc$D - Dl) < 1e-9)) {
        rg <- lookup_cell(cc, GT)
        ok(rg$source == "unsupported" && grepl("estimab", rg$reason),
           "interpolating across a non-converged bracket must refuse, and explain")
      }
    }
  }
  cat(sprintf("14. D interpolation on %-14s          %s (p90 %.2f pts, %d guarded)\n",
              nm, if (fail == f0) "PASS" else "FAIL", quantile(errs, .9), guarded))
}

cat("\n", if (fail == 0) "ALL TESTS PASSED" else paste(fail, "FAILURE(S)"), "\n")
quit(status = if (fail == 0) 0 else 1)
