# =============================================================================
# app-fixed.R — reactive-wiring tests for the app's fixed-schedule mode.
#
#   Rscript tests/app-fixed.R      (needs shiny; run from the repo root)
#
# Guards the bug that shipped past unit tests and only showed in the browser:
# design_type and engine were separate inputs, so estimate() could observe
# design_type=fixed while engine still said "lookup" and freeze on a stale grid
# refusal. testServer exercises the real reactive graph, which plain function
# tests do not.
# =============================================================================
suppressWarnings(suppressMessages(library(shiny)))
# build the appdir engine copies if they are not present (CI runs build first)
if (!file.exists("app/R/lookup.R")) system("Rscript tools/build-app.R")

fail <- 0L
ok <- function(cond, msg) if (!isTRUE(cond)) { fail <<- fail + 1L; cat("  FAIL:", msg, "\n") }

testServer("app", {
  base <- list(mode = "single", power_def = "all", N = 40, D = 14, lambda_bar = 3,
               cap = 0, compliance = 0.75, decay = 0, phi = 0.3, beta1 = 0.2,
               seed = 20260709, R = 60)

  # 1. fixed is simulate-mode even with engine at its default (the race)
  do.call(session$setInputs, c(list(design_type = "fixed"), base))
  ok(isTRUE(sim_mode()), "fixed design must be sim_mode regardless of the engine radio")
  ok(estimate()$source == "awaiting", "before a run, fixed shows 'awaiting', not a grid refusal")
  ok(!supported(), "not 'supported' until the run (tiles blank)")

  # 2. the run produces a balanced-ish Binomial-thinned simulation
  session$setInputs(run = 1)
  r <- estimate()
  ok(r$source == "simulated", "after run, source is 'simulated'")
  ok(abs(r$mean_n - 31.5) < 1.5, sprintf("mean_n ~31.5 (got %.2f)", r$mean_n))
  ok(r$sd_n < 3.5, sprintf("planned schedule => tight sd_n (got %.2f)", r$sd_n))

  # 3. switching back to a triggered design restores instant grid lookup.
  #    Use an on-grid cell (lambda 2, not the fixed-mode 3, which is off-grid).
  session$setInputs(design_type = "poisson", engine = "lookup",
                    N = 60, D = 14, lambda_bar = 2, cv = 0.3)
  ok(estimate()$source == "grid_exact", "triggered design returns to instant grid lookup")

  # 4. the cell a fixed design builds zeroes the inapplicable levers
  session$setInputs(design_type = "fixed")
  cell <- build_cell()
  ok(identical(as.character(cell$trigger_mode), "fixed"), "cell carries trigger_mode=fixed")
  ok(cell$cv == 0 && cell$trigger_link == 0, "cv and trigger_link zeroed in fixed mode")
})

cat(if (fail == 0) "\nAPP-FIXED: ALL PASSED\n" else sprintf("\nAPP-FIXED: %d FAILURE(S)\n", fail))
quit(status = if (fail == 0) 0 else 1)
